open Core
open Hardcaml
module Unix = Core_unix

let signals_per_function = 1000

type t =
  { total_words : int
  ; offsets : [ `Global of int | `Local of int ] Signal.Uid_map.t
  ; circuit : Circuit.t
  ; functions : string list ref
  }

let signal_allocated_width signal =
  match signal with
  | Signal.Multiport_mem { size; _ } ->
    if Signal.width signal <= 8
    then (size + Codegen.word_bytes - 1) / Codegen.word_bytes
    else Codegen.width_to_word_count (Signal.width signal) * size
  | Signal.Const _ ->
    (* only optimize out small constants to simplify codegen *)
    if Signal.width signal <= Codegen.word_size
    then 0
    else Codegen.width_to_word_count (Signal.width signal)
  | Signal.Wire { driver; _ } ->
    (* empty wires are inputs, other wires can be always eliminated *)
    if Signal.is_empty !driver
    then Codegen.width_to_word_count (Signal.width signal)
    else 0
  | Signal.Reg _ ->
    (* registers need to keep a copy of the input signal from the previous cycle *)
    2 * Codegen.width_to_word_count (Signal.width signal)
  | _ -> Codegen.width_to_word_count (Signal.width signal)
;;

let c_scheduling_deps (s : Signal.t) =
  match s with
  | Mem _ -> failwith "Mem unsupported"
  | Mem_read_port { memory; read_address; _ } -> [ read_address; memory ]
  | Reg _ -> []
  | Multiport_mem _ -> []
  | Empty | Const _ | Op2 _ | Mux _ | Not _ | Cat _ | Wire _ | Select _ | Inst _ ->
    Signal.deps s
;;

let schedule_signals circuit =
  (* Topologically sort signals. This is similar to Signal_graph.topological_sort, but
     ensures that portions of the graph that are trees are scheduled in postorder DFS
     fashion (which probably improves cache behaviour). *)
  let queue = Queue.of_list (Circuit.outputs circuit) in
  let visited = Hash_set.create (module Signal.Uid) in
  let result = Queue.create () in
  let rec visit signal =
    if not (Hash_set.mem visited (Signal.uid signal))
    then (
      Hash_set.add visited (Signal.uid signal);
      List.iter (Signal.deps signal) ~f:(Queue.enqueue queue);
      List.iter (c_scheduling_deps signal) ~f:visit;
      Queue.enqueue result signal)
  in
  while not (Queue.is_empty queue) do
    visit (Queue.dequeue_exn queue)
  done;
  Queue.to_list result
;;

let rec unwrap_wire (s : Signal.t) =
  match s with
  | Wire { driver; _ } -> if Signal.is_empty !driver then s else unwrap_wire !driver
  | _ -> s
;;

let allocate_offsets interesting_signals circuit =
  let ordering = schedule_signals circuit in
  let section_numbers =
    List.mapi ordering ~f:(fun i signal ->
      let section =
        if Signal.is_reg signal || Signal.is_multiport_mem signal
        then -1 (* sequential elements are in separate functions *)
        else i / signals_per_function
      in
      Signal.uid signal, section)
    |> Map.of_alist_exn (module Signal.Uid)
  in
  let users =
    List.concat_map ordering ~f:(fun signal ->
      List.map (Signal.deps signal) ~f:(fun d -> Signal.uid (unwrap_wire d), signal))
    |> Map.of_alist_multi (module Signal.Uid)
  in
  (* can the signal be allocated as a local function variable? *)
  let is_local signal =
    let my_section = Map.find_exn section_numbers (Signal.uid signal) in
    let all_users_of_this_signal_are_in_same_section =
      Map.find_multi users (Signal.uid signal)
      |> List.for_all ~f:(fun user ->
        Map.find_exn section_numbers (Signal.uid user) = my_section)
    in
    all_users_of_this_signal_are_in_same_section
    && (not (Set.mem interesting_signals (Signal.uid signal)))
    && Signal.width signal <= Codegen.word_size
    && (not (Signal.is_reg signal))
    && not (Signal.is_multiport_mem signal)
  in
  let local_counter = ref 0 in
  List.fold
    ordering
    ~init:(0, Map.empty (module Signal.Uid))
    ~f:(fun (offset, acc) signal ->
      let word_count = signal_allocated_width signal in
      let is_local = is_local signal in
      if is_local then Int.incr local_counter;
      ( (if is_local then offset else offset + word_count)
      , Map.add_exn
          acc
          ~key:(Signal.uid signal)
          ~data:(if is_local then `Local !local_counter else `Global offset) ))
;;

let create ?(interesting_signals = []) circuit =
  let interesting_signals =
    Circuit.inputs circuit @ Circuit.outputs circuit @ interesting_signals
  in
  let interesting_signals =
    List.map ~f:(fun s -> Signal.uid (unwrap_wire s)) interesting_signals
    |> Set.of_list (module Signal.Uid)
  in
  let total_words, offsets = allocate_offsets interesting_signals circuit in
  { total_words = Int.max total_words 1; offsets; functions = ref []; circuit }
;;

let rec to_signal_info t signal =
  let index = Map.find_exn t.offsets (Signal.uid signal) in
  let normal = Codegen.Normal { Codegen.width = Signal.width signal; index } in
  match signal with
  | Signal.Const { constant; _ } ->
    if Bits.width constant <= 64 then Codegen.Const constant else normal
  | Signal.Wire { driver; _ } ->
    if Signal.is_empty !driver
    then normal
    else (
      match to_signal_info t !driver with
      | Codegen.Normal i -> Virtual i
      | info -> info)
  | _ -> normal
;;

let cached_to_signal_info t =
  (* speedup signal info lookup for input/output ports *)
  let cache = Hashtbl.create (module Signal.Uid) in
  fun signal ->
    Hashtbl.find_or_add cache (Signal.uid signal) ~default:(fun () ->
      to_signal_info t signal)
;;

let make_comb_code t =
  schedule_signals t.circuit
  |> List.map ~f:(fun signal ->
    Codegen.compile_comb_signal ~to_signal_info:(to_signal_info t) signal)
;;

let last_layer_of_nodes circuit =
  (*
     This is a slightly simpler version of Circuit_graph.last_layer_of_nodes.
     As in Hardcaml_c registers also generate some code in combinatorial
     section, they need to be included in last layer of nodes.
  *)
  let in_last_layer = Hash_set.create (module Signal.Uid) in
  let rec visit_signal signal =
    if not (Hash_set.mem in_last_layer (Signal.uid signal))
    then (
      Hash_set.add in_last_layer (Signal.uid signal);
      if
        not
          (Signal.is_empty signal
           || Signal.is_multiport_mem signal
           || Signal.is_reg signal)
      then List.iter (c_scheduling_deps signal) ~f:visit_signal)
  in
  List.iter (Circuit.outputs circuit) ~f:visit_signal;
  Hash_set.to_list in_last_layer |> Set.of_list (module Signal.Uid)
;;

let make_comb_last_layer_code t =
  let last_layer = last_layer_of_nodes t.circuit in
  schedule_signals t.circuit
  |> List.filter_map ~f:(fun signal ->
    if Set.mem last_layer (Signal.uid signal)
    then Some (Codegen.compile_comb_signal ~to_signal_info:(to_signal_info t) signal)
    else None)
;;

let make_reset_code t =
  schedule_signals t.circuit
  |> List.map ~f:(fun signal ->
    Codegen.compile_reset_signal ~to_signal_info:(to_signal_info t) signal)
  |> List.filter ~f:(fun l -> not (String.equal l ""))
;;

let make_seq_code t =
  schedule_signals t.circuit
  |> List.map ~f:(fun signal ->
    Codegen.compile_seq_signal ~to_signal_info:(to_signal_info t) signal)
  |> List.filter ~f:(fun l -> not (String.equal l ""))
;;

module Instance = struct
  type t =
    { to_signal_info : Signal.t -> Codegen.signal_info
    ; memory : char Ctypes.CArray.t
    ; memory_bigstring : Bigstring.t
    ; eval_library : Dl.library ref
    ; functions : (unit -> unit) array
    }

  external caml_bigstring_get64u : Bigstring.t -> int -> int64 = "%caml_bigstring_get64u"

  external caml_bigstring_set64u
    :  Bigstring.t
    -> int
    -> int64
    -> unit
    = "%caml_bigstring_set64u"

  external caml_bytes_get64u : Bytes.t -> int -> int64 = "%caml_bytes_get64u"
  external caml_bytes_set64u : Bytes.t -> int -> int64 -> unit = "%caml_bytes_set64u"

  let read t signal =
    let signal_info = t.to_signal_info signal in
    match signal_info with
    | Const c -> c
    | _ ->
      let pos = Codegen.word_offset signal_info in
      let size_words = Codegen.word_count signal_info in
      let dst = Bytes.create (size_words * 8) in
      for i = 0 to size_words - 1 do
        caml_bytes_set64u
          dst
          (i * 8)
          (caml_bigstring_get64u t.memory_bigstring ((pos + i) * 8))
      done;
      Constant.Raw.unsafe_of_bytes ~width:(Signal.width signal) dst |> Bits.of_constant
  ;;

  let write t signal bits =
    let signal_info = t.to_signal_info signal in
    (* Hardcaml_c assumes unused bits are set to zero, while in Hardcaml they can have
       arbitrary value. Mask them out. *)
    let bits_bytes = Bits.to_constant bits |> Constant.Raw.unsafe_to_bytes in
    let pos = Codegen.word_offset signal_info in
    let size_words = Codegen.word_count signal_info in
    let width = Codegen.width signal_info in
    for i = 0 to size_words - 1 do
      let width_left = width - (i * Codegen.word_size) in
      let value = caml_bytes_get64u bits_bytes (i * 8) in
      let value =
        if width_left < Codegen.word_size
        then Int64.(value land ((Int64.one lsl width_left) - Int64.one))
        else value
      in
      caml_bigstring_set64u t.memory_bigstring ((pos + i) * 8) value
    done
  ;;

  let run_function t id = t.functions.(id) ()
end

let format_single_function name lines =
  sprintf
    {|
static void %s(uint64_t* memory) {
  %s
}
    |}
    name
    (String.concat ~sep:"\n  " lines)
;;

let format_function name lines =
  let blocks = List.chunks_of ~length:signals_per_function lines in
  let bodies =
    List.mapi blocks ~f:(fun i block ->
      format_single_function (sprintf "%s_%d" name i) block)
    |> String.concat ~sep:"\n"
  in
  let footer =
    List.mapi blocks ~f:(fun i _ -> sprintf "%s_%d(memory);" name i)
    |> String.concat ~sep:"\n  "
  in
  sprintf {|
%s
void %s(uint64_t* memory) {
  %s
} |} bodies name footer
;;

let make_c_source t =
  let functions = !(t.functions) |> String.concat ~sep:"\n" in
  Simulate_c_header.header ^ functions
;;

let add_function t body =
  let function_id = List.length !(t.functions) in
  t.functions := format_function (sprintf "f%d" function_id) body :: !(t.functions);
  fun instance -> Instance.run_function instance function_id
;;

let start ?(compiler_command = "gcc -O0") t =
  let dir = Filename_unix.temp_dir "hardcaml-c" "" in
  (let c_file = Out_channel.create (dir ^ "/eval.c") in
   let source = make_c_source t in
   Core.fprintf c_file "%s" source;
   Out_channel.close c_file);
  (match
     Unix.system
       (sprintf
          !"%s -fno-strict-aliasing -g %s/eval.c -shared -o %s/eval.so -fPIC"
          compiler_command
          dir
          dir)
   with
   | Ok () -> ()
   | Error e -> raise_s [%message "could not compile" (e : Unix.Exit_or_signal.error)]);
  let eval_library =
    ref (Dl.dlopen ~filename:(sprintf "%s/eval.so" dir) ~flags:[ Dl.RTLD_NOW ])
  in
  Gc.Expert.add_finalizer_exn eval_library (fun lib -> Dl.dlclose ~handle:!lib);
  Core_unix.unlink (dir ^ "/eval.c");
  Core_unix.unlink (dir ^ "/eval.so");
  Core_unix.rmdir dir;
  let memory = Ctypes.CArray.make Ctypes.char (t.total_words * Codegen.word_bytes) in
  let open Ctypes in
  let functions =
    List.mapi !(t.functions) ~f:(fun id _ ->
      let f =
        Ctypes_foreign_threaded_flat.Foreign.foreign
          ~from:!eval_library
          (sprintf "f%d" id)
          (Ctypes.ptr Ctypes.char @-> returning void)
      in
      fun () -> f (Ctypes.CArray.start memory))
    |> Array.of_list
  in
  let memory_bigstring = Ctypes.bigarray_of_array Ctypes.array1 Bigarray.Char memory in
  let instance =
    { Instance.to_signal_info = cached_to_signal_info t
    ; memory
    ; memory_bigstring
    ; eval_library
    ; functions
    }
  in
  instance
;;
