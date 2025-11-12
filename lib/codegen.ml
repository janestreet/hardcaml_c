open Core
open Hardcaml

let word_size = 64
let word_bytes = word_size / 8

type normal_signal_info =
  { width : int
  ; index : [ `Local of int | `Global of int ]
  }

type signal_info =
  (* A constant *)
  | Const of Bits.t
  (* A signal other than constant. *)
  | Normal of normal_signal_info
  (* An eliminated wire - contains [normal_signal_info] of the target signal. *)
  | Virtual of normal_signal_info

let width = function
  | Const c -> Bits.width c
  | Normal { width; _ } -> width
  | Virtual { width; _ } -> width
;;

let word_offset = function
  | Const _ -> raise_s [%message "word_offset unsupported for constants"]
  | Normal { index; _ } | Virtual { index; _ } ->
    (match index with
     | `Global word_offset -> word_offset
     | _ -> raise_s [%message "word_offset unsupported on local signals"])
;;

let is_const = function
  | Const _ -> true
  | Normal _ -> false
  | Virtual _ -> false
;;

(* ID of a local variable the signal is stored in. *)
let local_index = function
  | Const _ -> None
  | Virtual _ -> None
  | Normal { index; _ } ->
    (match index with
     | `Local i -> Some i
     | _ -> None)
;;

let width_to_word_count width = (width + word_size - 1) / word_size
let word_count signal = width_to_word_count (width signal)

let is_virtual = function
  | Const _ -> true
  | Virtual _ -> true
  | Normal _ -> false
;;

let c_zero = [%rope "0ull"]
let newline = [%rope "\n"]
let negate x = [%rope "-(%{x})"]
let bracket d = Rope.concat [ [%rope "("]; d; [%rope ")"] ]
let assign tgt e = [%rope "%{tgt} = %{e};"]
let op2 a op b = [%rope "%{a} %{op#String} %{b}"]
let op2_int a op b = [%rope "%{a} %{op#String} %{b#Int}"]
let not_ x = [%rope "~(%{x})"]
let and_ x y = [%rope "(%{x}) & (%{y})"]
let or_ x y = [%rope "(%{x}) | (%{y})"]

let bits_to_c b =
  assert (Bits.width b <= 64);
  let hex =
    b |> Bits.to_constant |> Constant.to_hex_string ~signedness:Signedness.Unsigned
  in
  [%rope "0x%{hex#String}ull"]
;;

let mask_of_size n = if n = 0 then [%rope "0"] else Bits.ones n |> bits_to_c
let mask e width = [%rope "(%{e}) & %{mask_of_size width}"]

let get_bits_nth_word b offset =
  let hi = Int.min (Bits.width b - 1) (((offset + 1) * word_size) - 1) in
  let data = b.Bits.:[hi, offset * word_size] in
  bits_to_c data
;;

let memory_at_const_offset offset = [%rope "memory[%{offset#Int}]"]
let memory_at_ptr_offset ptr offset = [%rope "memory[%{ptr} + %{offset#Int}]"]

let ( .:() ) signal offset =
  let word_count = word_count signal in
  match Int.compare offset word_count with
  | 1 -> raise_s [%message "out of bounds read"]
  | 0 -> c_zero
  | -1 ->
    (match signal with
     | Const c -> get_bits_nth_word c offset
     | Virtual { index; _ } | Normal { index; _ } ->
       (match index with
        | `Local i -> [%rope "local_%{i#Int}"]
        | `Global word_offset -> memory_at_const_offset (word_offset + offset)))
  | _ -> assert false
;;

let get_nth_word_prev signal offset =
  assert (not (is_const signal));
  memory_at_const_offset (word_offset signal + word_count signal + offset)
;;

let get_word_at signal bit_offset =
  (* Return C code that evaluates to word of signal starting at bit [bit_offset]. *)
  if bit_offset < 0
  then (
    assert (bit_offset > -word_size);
    op2_int signal.:(0) "<<" (-bit_offset))
  else if bit_offset mod word_size = 0
  then (
    let word_offset = bit_offset / word_size in
    signal.:(word_offset))
  else (
    let word_offset = bit_offset / word_size in
    let ibit_offset = bit_offset mod word_size in
    bracket
      (or_
         (op2_int signal.:(word_offset) ">>" ibit_offset)
         (op2_int signal.:(word_offset + 1) "<<" (word_size - ibit_offset))))
;;

let call_long_op2 name tgt a b width =
  [%rope
    "long_%{name#String}((uint32_t*)&%{tgt}, (uint32_t*)&%{a}, (uint32_t*)&%{b}, \
     %{width#Int});"]
;;

let compile_add tgt a b =
  assert (width a = width b);
  if width a > word_size
  then call_long_op2 "add" tgt.:(0) a.:(0) b.:(0) (width a)
  else assign tgt.:(0) (mask (op2 a.:(0) "+" b.:(0)) (width a))
;;

let compile_sub tgt a b =
  assert (width a = width b);
  if width a > word_size
  then call_long_op2 "sub" tgt.:(0) a.:(0) b.:(0) (width a)
  else assign tgt.:(0) (mask (op2 a.:(0) "-" b.:(0)) (width a))
;;

let compile_lt tgt a b =
  assert (width a = width b);
  if width a > word_size
  then assign tgt.:(0) [%rope "long_lt(&%{a.:(0)}, &%{b.:(0)}, %{width a#Int})"]
  else assign tgt.:(0) (op2 a.:(0) "<" b.:(0))
;;

let compile_long_mul name tgt a b =
  let is_const_a = is_const a in
  let is_const_b = is_const b in
  let long_mul a b wa wb =
    [%rope "long_%{name#String}(&%{tgt.:(0)}, &%{a}, &%{b}, %{wa#Int}, %{wb#Int});"]
  in
  if is_const a && is_const b
  then (
    let long_mul = long_mul [%rope "a"] [%rope "b"] (width a) (width b) in
    [%rope "{ uint64_t a = %{a.:(0)}; uint64_t b = %{b.:(0)}; %{long_mul}; }"])
  else if is_const_a || is_const_b
  then (
    let a, b = if is_const_a then a, b else b, a in
    let long_mul = long_mul [%rope "v"] b.:(0) (width a) (width b) in
    [%rope "{ uint64_t v = %{a.:(0)}; %{long_mul}; }"])
  else long_mul a.:(0) b.:(0) (width a) (width b)
;;

let compile_mulu tgt a b =
  if width tgt > word_size
  then compile_long_mul "mulu" tgt a b
  else assign tgt.:(0) (op2 a.:(0) "*" b.:(0))
;;

let compile_muls tgt a b = compile_long_mul "muls" tgt a b
let multiline count f = List.init count ~f |> Rope.concat ~sep:newline
let multilines count f = List.init count ~f |> List.concat |> Rope.concat ~sep:newline

let compile_const ~tgt b =
  multiline (word_count tgt) (fun offset ->
    assign tgt.:(offset) (get_bits_nth_word b offset))
;;

let compile_bitop op tgt a b =
  assert (width a = width b);
  multiline (word_count a) (fun offset ->
    assign tgt.:(offset) (op2 a.:(offset) op b.:(offset)))
;;

let compile_not tgt a =
  multiline (word_count a) (fun offset ->
    let num_bits = Int.min (width a - (offset * word_size)) word_size in
    assign tgt.:(offset) (op2 a.:(offset) "^" (mask_of_size num_bits)))
;;

let eq_expr a b =
  bracket
    (Rope.concat
       ~sep:[%rope "&&"]
       (List.init (word_count a) ~f:(fun offset ->
          bracket (op2 a.:(offset) "==" b.:(offset)))))
;;

let compile_eq tgt a b = assign tgt.:(0) (eq_expr a b)

let memcpy dst src src_offset num_bytes =
  [%rope "memcpy(&%{dst}, ((char*)&%{src})+%{src_offset#Int}, %{num_bytes#Int});"]
;;

let compile_select tgt signal bit_offset length =
  assert (length = width tgt);
  if word_count tgt > 5 && width tgt mod 8 = 0 && bit_offset mod 8 = 0
  then memcpy tgt.:(0) signal.:(0) (bit_offset / 8) (width tgt / 8)
  else
    multiline (word_count tgt) (fun offset ->
      let num_bits = Int.min (length - (offset * word_size)) word_size in
      assign
        tgt.:(offset)
        (op2
           (get_word_at signal (bit_offset + (offset * word_size)))
           "&"
           (mask_of_size num_bits)))
;;

let compile_copy_to_address dst_address src =
  multiline (word_count src) (fun offset ->
    assign (memory_at_ptr_offset dst_address offset) src.:(offset))
;;

let compile_copy_from_address tgt source_address =
  multiline (word_count tgt) (fun offset ->
    assign tgt.:(offset) (memory_at_ptr_offset source_address offset))
;;

let compile_copy ~tgt a =
  multiline (word_count tgt) (fun offset -> assign tgt.:(offset) a.:(offset))
;;

let compile_copy_bits ~tgt a =
  multiline (word_count tgt) (fun offset ->
    assign tgt.:(offset) (get_bits_nth_word a offset))
;;

let compile_copy_from_prev ~tgt a =
  multiline (word_count tgt) (fun offset ->
    assign tgt.:(offset) (get_nth_word_prev a offset))
;;

let compile_copy_to_prev ~tgt a =
  multiline (word_count tgt) (fun offset ->
    assign (get_nth_word_prev tgt offset) a.:(offset))
;;

let compile_copy_bits_to_prev ~tgt a =
  multiline (word_count tgt) (fun offset ->
    assign (get_nth_word_prev tgt offset) (get_bits_nth_word a offset))
;;

let compile_mux_branchless ?(chunk_size = 1000) tgt selector signals =
  let or_ = [%rope " | "] in
  multilines (word_count tgt) (fun offset ->
    let target = tgt.:(offset) in
    let rec traverse idx l =
      match List.split_n l chunk_size with
      | [], _ -> []
      | start, next ->
        let selects =
          List.mapi start ~f:(fun i signal ->
            (* [%rope "(%{signal.:(offset)} & (-(%{selector.:(0)} == %{(idx+i)#Int})))"] *)
            bracket
              (and_ signal.:(offset) (negate (op2_int selector.:(0) "==" (idx + i)))))
          |> Rope.concat ~sep:or_
        in
        [%rope "%{target} |= %{selects};"] :: traverse (idx + chunk_size) next
    in
    assign target [%rope "0"] :: traverse 0 signals)
;;

let compile_mux_two tgt selector signals =
  (* optimization for common case where [word_count tgt = 1] and [length signals = 2] *)
  match word_count tgt, signals with
  | 1, [ signal_1; signal_2 ] ->
    let tgt = tgt.:(0) in
    let signal_1 = signal_1.:(0) in
    let sel = selector.:(0) in
    let signal_2 = signal_2.:(0) in
    Some
      (assign tgt (or_ (and_ signal_1 (not_ (negate sel))) (and_ signal_2 (negate sel))))
  | _ -> None
;;

let compile_mux tgt selector signals =
  match compile_mux_two tgt selector signals with
  | Some r -> r
  | None -> compile_mux_branchless tgt selector signals
;;

let mux sel on_true on_false = [%rope "%{sel} ? %{on_true} : %{on_false}"]

let compile_match_const_two tgt select match_with on_true on_false =
  multiline (word_count tgt) (fun offset ->
    assign
      tgt.:(offset)
      (mux (eq_expr select (Const match_with)) on_true.:(offset) on_false.:(offset)))
;;

let compile_cases tgt ~default select cases =
  let _, strings =
    List.fold_right
      ~init:(default, [])
      cases
      ~f:(fun (match_with, value) (default, strings) ->
        let string = compile_match_const_two tgt select match_with value default in
        tgt, string :: strings)
  in
  Rope.concat ~sep:newline (List.rev strings)
;;

let compile_cat tgt signals =
  let _, with_offset =
    List.fold (List.rev signals) ~init:(0, []) ~f:(fun (current_offset, acc) signal ->
      current_offset + width signal, (current_offset, signal) :: acc)
  in
  let s =
    List.concat_map with_offset ~f:(fun (bit_offset, signal) ->
      let first_word_bit_offset = bit_offset mod word_size in
      let first_word = bit_offset / word_size in
      List.range 0 (word_count signal + 1)
      |> List.concat_map ~f:(fun word_offset ->
        let v = get_word_at signal ((word_offset * word_size) - first_word_bit_offset) in
        if first_word + word_offset < word_count tgt
        then [ first_word + word_offset, v ]
        else []))
  in
  Int.Map.of_alist_multi s
  |> Map.to_alist
  |> List.map ~f:(fun (offset, values) ->
    assign
      tgt.:(offset)
      (match values with
       | [] -> c_zero
       | _ -> Rope.concat ~sep:[%rope " | "] values))
  |> Rope.concat ~sep:newline
;;

let compile_reg ~to_signal_info signal ~source reg =
  let { Signal.Type.Reg.Register.clock = _
      ; (* reset is supported by compile_reset_signal *)
        reset = _
      ; clear
      ; initialize_to = _
      ; enable
      }
    =
    reg
  in
  let tgt = to_signal_info signal in
  let c_clear =
    match clear with
    | None -> Rope.empty
    | Some { clear; clear_to } ->
      [%rope
        "if (%{((to_signal_info clear).:(0))} == 1) { %{(compile_copy_to_prev ~tgt \
         (to_signal_info clear_to))} } else"]
  in
  match enable with
  | None ->
    [%rope "%{c_clear} { %{(compile_copy_to_prev ~tgt (to_signal_info source))} }"]
  | Some enable ->
    [%rope
      "%{c_clear} if (%{(to_signal_info enable).:(0)} == 1) { %{(compile_copy_to_prev \
       ~tgt (to_signal_info source))} }"]
;;

let compile_write_port memory write_address write_enable write_data =
  let actual_copy =
    let width = width write_data in
    let offset = word_offset memory in
    if width <= 8
    then
      [%rope
        "((uint8_t*)(&memory[%{offset#Int}]))[%{write_address.:(0)}] = \
         (uint8_t)(%{write_data.:(0)});"]
    else (
      let word_count = word_count write_data in
      compile_copy_to_address
        [%rope "%{offset#Int} + (%{write_address.:(0)}) * %{word_count#Int}"]
        write_data)
  in
  [%rope " if (%{write_enable.:(0)} == 1) { %{actual_copy} }"]
;;

let compile_multiport_mem ~to_signal_info signal write_ports =
  Array.to_list write_ports
  |> List.map
       ~f:(fun { Write_port.write_clock = _; write_address; write_enable; write_data } ->
         compile_write_port
           (to_signal_info signal)
           (to_signal_info write_address)
           (to_signal_info write_enable)
           (to_signal_info write_data))
  |> Rope.concat ~sep:newline
;;

let compile_mem_read_port tgt memory address =
  let width = width tgt in
  let word_count = word_count tgt in
  let memory = word_offset memory in
  if width <= 8
  then assign tgt.:(0) [%rope "((uint8_t*)(&memory[%{memory#Int}]))[%{address.:(0)}]"]
  else
    compile_copy_from_address
      tgt
      [%rope "(%{memory#Int} + (%{address.:(0)}) * %{word_count#Int})"]
;;

let compile_comb_signal ~to_signal_info signal =
  let tgt = to_signal_info signal in
  let code =
    if is_virtual tgt
    then Rope.empty
    else (
      match (signal : Signal.t) with
      | Empty -> Rope.empty
      | Const { constant; _ } -> compile_const ~tgt constant
      | Not { arg; _ } ->
        let arg = to_signal_info arg in
        compile_not tgt arg
      | Cat { args; _ } -> compile_cat tgt (List.map ~f:to_signal_info args)
      | Mux { select; cases; _ } ->
        let select = to_signal_info select in
        let cases = List.map ~f:to_signal_info cases in
        compile_mux tgt select cases
      | Cases { select; cases; default; _ } ->
        let select = to_signal_info select in
        let default = to_signal_info default in
        let cases =
          List.map cases ~f:(fun (match_with, value) ->
            Signal.to_constant match_with |> Bits.of_constant, to_signal_info value)
        in
        compile_cases tgt ~default select cases
      | Op2 { op; arg_a; arg_b; _ } ->
        let op2 op a b =
          let a = to_signal_info a in
          let b = to_signal_info b in
          op tgt a b
        in
        (match op with
         | Add -> op2 compile_add
         | Sub -> op2 compile_sub
         | Mulu -> op2 compile_mulu
         | Muls -> op2 compile_muls
         | And -> op2 (compile_bitop "&")
         | Or -> op2 (compile_bitop "|")
         | Xor -> op2 (compile_bitop "^")
         | Eq -> op2 compile_eq
         | Lt -> op2 compile_lt)
          arg_a
          arg_b
      | Wire { driver = None; _ } -> [%rope "// %{(tgt.:(0))} = empty wire"]
      | Wire { driver = Some driver; _ } ->
        let src = to_signal_info driver in
        let tgt = tgt in
        compile_copy ~tgt src
      | Select { arg; high; low; _ } ->
        let d = to_signal_info arg in
        let offset = low in
        let length = high - low + 1 in
        compile_select tgt d offset length
      | Reg _ -> compile_copy_from_prev ~tgt tgt
      | Multiport_mem _ -> Rope.empty
      | Mem_read_port { memory; read_address; _ } ->
        compile_mem_read_port tgt (to_signal_info memory) (to_signal_info read_address)
      | Inst _ -> raise_s [%message "Inst signals are unsupported" (signal : Signal.t)])
  in
  let code =
    match local_index tgt with
    | Some index -> [%rope "uint64_t local_%{index#Int};\n%{code}"]
    | None -> code
  in
  [%rope "// Signal %{signal#Signal}\n%{code}"]
;;

let compile_seq_signal ~to_signal_info signal =
  match (signal : Signal.t) with
  | Reg { register = reg; d = source; _ } ->
    compile_reg ~to_signal_info signal ~source reg
  | Multiport_mem { write_ports; _ } ->
    compile_multiport_mem ~to_signal_info signal write_ports
  | _ -> Rope.empty
;;

let compile_reset_signal ~to_signal_info signal =
  match (signal : Signal.t) with
  | Reg { register = { reset; _ }; _ } ->
    Option.value_map
      ~default:Rope.empty
      reset
      ~f:(fun { reset = _; reset_edge = _; reset_to } ->
        Rope.concat
          [ compile_copy ~tgt:(to_signal_info signal) (to_signal_info reset_to)
          ; newline
          ; compile_copy_to_prev ~tgt:(to_signal_info signal) (to_signal_info reset_to)
          ; newline
          ])
  | _ -> Rope.empty
;;

let compile_register_initializer ~to_signal_info signal =
  match (signal : Signal.t) with
  | Reg { register = { initialize_to; _ }; _ } ->
    (match initialize_to with
     | None -> Rope.empty
     | Some initialize_to ->
       Rope.concat
         [ compile_copy_bits ~tgt:(to_signal_info signal) initialize_to
         ; newline
         ; compile_copy_bits_to_prev ~tgt:(to_signal_info signal) initialize_to
         ; newline
         ])
  | _ -> Rope.empty
;;

let compile_memory_initializer ~to_signal_info signal =
  match (signal : Signal.t) with
  | Multiport_mem { initialize_to; _ } ->
    Option.map initialize_to ~f:(fun initialize_to ->
      Array.mapi initialize_to ~f:(fun address init ->
        let memory = to_signal_info signal in
        if Signal.width signal <= 8
        then
          [%rope
            "((uint8_t*)(&memory[%{word_offset memory#Int}]))[%{address#Int}] = \
             (uint8_t)(%{get_bits_nth_word init 0});"]
        else (
          let word_count = word_count memory in
          multiline word_count (fun offset ->
            [%rope
              "memory[%{word_offset memory#Int} + (%{address#Int} * %{word_count#Int}) + \
               %{offset#Int}] = %{get_bits_nth_word init offset};"])))
      |> Array.to_list)
  | _ -> None
;;

module For_testing = struct
  let compile_add = compile_add
  let compile_sub = compile_sub
  let compile_lt = compile_lt
  let compile_eq = compile_eq
  let compile_mulu = compile_mulu
  let compile_muls = compile_muls
  let compile_bitop = compile_bitop
  let compile_not = compile_not
  let compile_const = compile_const
  let compile_select = compile_select
  let compile_mux_branchless = compile_mux_branchless
  let compile_cases = compile_cases
  let compile_mux = compile_mux
  let compile_cat = compile_cat
  let compile_reg = compile_reg
  let compile_write_port = compile_write_port
  let compile_mem_read_port = compile_mem_read_port
end
