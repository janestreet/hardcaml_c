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
  | Const _ -> failwith "word_offset unsupported for constants"
  | Normal { index; _ } | Virtual { index; _ } ->
    (match index with
     | `Global word_offset -> word_offset
     | _ -> failwith "word_offset unsupported on local signals")
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

let c_zero = "0ull"

let bits_to_c b =
  assert (Bits.width b <= 64);
  let hex =
    b |> Bits.to_constant |> Constant.to_hex_string ~signedness:Signedness.Unsigned
  in
  sprintf "0x%sull" hex
;;

let get_bits_nth_word b offset =
  let hi = Int.min (Bits.width b - 1) (((offset + 1) * word_size) - 1) in
  let data = b.Bits.:[hi, offset * word_size] in
  bits_to_c data
;;

let get_nth_word signal offset =
  let word_count = word_count signal in
  match Int.compare offset word_count with
  | 1 -> failwith "out of bounds read"
  | 0 -> c_zero
  | -1 ->
    (match signal with
     | Const c -> get_bits_nth_word c offset
     | Virtual { index; _ } | Normal { index; _ } ->
       (match index with
        | `Local i -> sprintf "local_%d" i
        | `Global word_offset -> sprintf "memory[%d]" (word_offset + offset)))
  | _ -> assert false
;;

let get_nth_word_prev signal offset =
  assert (not (is_const signal));
  sprintf "memory[%d]" (word_offset signal + word_count signal + offset)
;;

let get_word_at signal bit_offset =
  (* Return C code that evaluates to word of signal starting at bit [bit_offset]. *)
  if bit_offset < 0
  then (
    assert (bit_offset > -word_size);
    sprintf "%s << %d" (get_nth_word signal 0) (-bit_offset))
  else if bit_offset mod word_size = 0
  then (
    let word_offset = bit_offset / word_size in
    get_nth_word signal word_offset)
  else (
    let word_offset = bit_offset / word_size in
    let ibit_offset = bit_offset mod word_size in
    sprintf
      "((%s >> %d) | (%s << %d))"
      (get_nth_word signal word_offset)
      ibit_offset
      (get_nth_word signal (word_offset + 1))
      (word_size - ibit_offset))
;;

let mask_of_size n = if n = 0 then "0" else Bits.ones n |> bits_to_c

let compile_add tgt a b =
  assert (width a = width b);
  if width a > word_size
  then
    sprintf
      (* this is undefined behaviour, but we compile with -fno-strict-aliasing *)
      "long_add((uint32_t*)&%s, (uint32_t*)&%s, (uint32_t*)&%s, %d);"
      (get_nth_word tgt 0)
      (get_nth_word a 0)
      (get_nth_word b 0)
      (width a)
  else
    sprintf
      "%s = (%s + %s) & %s;"
      (get_nth_word tgt 0)
      (get_nth_word a 0)
      (get_nth_word b 0)
      (mask_of_size (width a))
;;

let compile_sub tgt a b =
  assert (width a = width b);
  if width a > word_size
  then
    sprintf
      "long_sub((uint32_t*)&%s, (uint32_t*)&%s, (uint32_t*)&%s, %d);"
      (get_nth_word tgt 0)
      (get_nth_word a 0)
      (get_nth_word b 0)
      (width a)
  else
    sprintf
      "%s = (%s - %s) & %s;"
      (get_nth_word tgt 0)
      (get_nth_word a 0)
      (get_nth_word b 0)
      (mask_of_size (width a))
;;

let compile_lt tgt a b =
  assert (width a = width b);
  if width a > word_size
  then
    sprintf
      "%s = long_lt(&%s, &%s, %d);"
      (get_nth_word tgt 0)
      (get_nth_word a 0)
      (get_nth_word b 0)
      (width a)
  else
    sprintf "%s = (%s < %s);" (get_nth_word tgt 0) (get_nth_word a 0) (get_nth_word b 0)
;;

let compile_long_mul name tgt a b =
  if is_const a && is_const b
  then
    sprintf
      "{ uint64_t a = %s; uint64_t b = %s; long_%s(&%s, &a, &b, %d, %d); }"
      (get_nth_word a 0)
      (get_nth_word b 0)
      name
      (get_nth_word tgt 0)
      (width a)
      (width b)
  else if is_const a || is_const b
  then (
    let a, b = if is_const a then a, b else b, a in
    sprintf
      "{ uint64_t v = %s; long_%s(&%s, &v, &%s, %d, %d); }"
      (get_nth_word a 0)
      name
      (get_nth_word tgt 0)
      (get_nth_word b 0)
      (width a)
      (width b))
  else
    sprintf
      "long_%s(&%s, &%s, &%s, %d, %d);"
      name
      (get_nth_word tgt 0)
      (get_nth_word a 0)
      (get_nth_word b 0)
      (width a)
      (width b)
;;

let compile_mulu tgt a b =
  if width tgt > word_size
  then compile_long_mul "mulu" tgt a b
  else
    sprintf "%s = (%s) * (%s);" (get_nth_word tgt 0) (get_nth_word a 0) (get_nth_word b 0)
;;

let compile_muls tgt a b = compile_long_mul "muls" tgt a b
let multiline count f = List.range 0 count |> List.map ~f |> String.concat ~sep:"\n"

let multiline' count f =
  List.range 0 count |> List.concat_map ~f |> String.concat ~sep:"\n"
;;

let compile_const ~tgt b =
  multiline (word_count tgt) (fun offset ->
    sprintf "%s = %s;" (get_nth_word tgt offset) (get_bits_nth_word b offset))
;;

let compile_bitop op tgt a b =
  assert (width a = width b);
  multiline (word_count a) (fun offset ->
    sprintf
      "%s = %s %s %s;"
      (get_nth_word tgt offset)
      (get_nth_word a offset)
      op
      (get_nth_word b offset))
;;

let compile_not tgt a =
  multiline (word_count a) (fun offset ->
    let num_bits = Int.min (width a - (offset * word_size)) word_size in
    sprintf
      "%s = %s ^ %s;"
      (get_nth_word tgt offset)
      (get_nth_word a offset)
      (mask_of_size num_bits))
;;

let compile_eq tgt a b =
  let value =
    List.range 0 (word_count a)
    |> List.map ~f:(fun offset ->
      sprintf "(%s == %s)" (get_nth_word a offset) (get_nth_word b offset))
    |> String.concat ~sep:"&&"
  in
  sprintf "%s = (%s);" (get_nth_word tgt 0) value
;;

let compile_select tgt signal bit_offset length =
  assert (length = width tgt);
  if word_count tgt > 5 && width tgt mod 8 = 0 && bit_offset mod 8 = 0
  then
    sprintf
      "memcpy(&%s, ((char*)&%s)+%d, %d);"
      (get_nth_word tgt 0)
      (get_nth_word signal 0)
      (bit_offset / 8)
      (width tgt / 8)
  else
    multiline (word_count tgt) (fun offset ->
      let num_bits = Int.min (length - (offset * word_size)) word_size in
      sprintf
        "%s = %s & %s;"
        (get_nth_word tgt offset)
        (get_word_at signal (bit_offset + (offset * word_size)))
        (mask_of_size num_bits))
;;

let%expect_test "select" =
  compile_select
    (Normal { index = `Global 2000; width = 120 })
    (Normal { index = `Global 1000; width = 210 })
    10
    120
  |> printf "%s\n";
  [%expect
    {|
    memory[2000] = ((memory[1000] >> 10) | (memory[1001] << 54)) & 0xffffffffffffffffull;
    memory[2001] = ((memory[1001] >> 10) | (memory[1002] << 54)) & 0xffffffffffffffull;
    |}]
;;

let compile_copy_to_address (dst_address : string) src =
  multiline (word_count src) (fun offset ->
    sprintf "memory[%s + %d] = %s;" dst_address offset (get_nth_word src offset))
;;

let compile_copy_from_address tgt (source_address : string) =
  multiline (word_count tgt) (fun offset ->
    sprintf "%s = memory[%s + %d];" (get_nth_word tgt offset) source_address offset)
;;

let compile_copy ~tgt a =
  multiline (word_count tgt) (fun offset ->
    sprintf "%s = %s;" (get_nth_word tgt offset) (get_nth_word a offset))
;;

let compile_copy_from_prev ~tgt a =
  multiline (word_count tgt) (fun offset ->
    sprintf "%s = %s;" (get_nth_word tgt offset) (get_nth_word_prev a offset))
;;

let compile_copy_to_prev ~tgt a =
  multiline (word_count tgt) (fun offset ->
    sprintf "%s = %s;" (get_nth_word_prev tgt offset) (get_nth_word a offset))
;;

let compile_mux_branchless ?(chunk_size = 1000) tgt selector signals =
  multiline' (word_count tgt) (fun offset ->
    let target = get_nth_word tgt offset in
    let rec traverse idx l =
      match List.split_n l chunk_size with
      | [], _ -> []
      | start, next ->
        let s =
          sprintf
            "%s |= %s;"
            target
            (List.mapi start ~f:(fun i signal ->
               let i = idx + i in
               sprintf
                 "(%s & (-(%s == %d)))"
                 (get_nth_word signal offset)
                 (get_nth_word selector 0)
                 i)
             |> String.concat ~sep:" | ")
        in
        s :: traverse (idx + chunk_size) next
    in
    sprintf "%s = 0;" target :: traverse 0 signals)
;;

let%expect_test "mux_branchless" =
  compile_mux_branchless
    (Normal { index = `Global 2000; width = 2 })
    (Normal { index = `Global 100; width = 4 })
    [ Const (Bits.of_string "00")
    ; Const (Bits.of_string "01")
    ; Const (Bits.of_string "10")
    ]
  |> printf "%s\n";
  [%expect
    {|
    memory[2000] = 0;
    memory[2000] |= (0x0ull & (-(memory[100] == 0))) | (0x1ull & (-(memory[100] == 1))) | (0x2ull & (-(memory[100] == 2)));
    |}];
  compile_mux_branchless
    ~chunk_size:2
    (Normal { index = `Global 2000; width = 2 })
    (Normal { index = `Global 100; width = 4 })
    [ Const (Bits.of_string "00")
    ; Const (Bits.of_string "01")
    ; Const (Bits.of_string "10")
    ]
  |> printf "%s\n";
  [%expect
    {|
    memory[2000] = 0;
    memory[2000] |= (0x0ull & (-(memory[100] == 0))) | (0x1ull & (-(memory[100] == 1)));
    memory[2000] |= (0x2ull & (-(memory[100] == 2)));
    |}]
;;

let compile_mux_two tgt selector signals =
  (* optimization for common case where [word_count tgt = 1] and [length signals = 2] *)
  match word_count tgt, signals with
  | 1, [ signal_1; signal_2 ] ->
    Some
      (sprintf
         "%s = (%s & (~(-(%s)))) | (%s & (-(%s)));"
         (get_nth_word tgt 0)
         (get_nth_word signal_1 0)
         (get_nth_word selector 0)
         (get_nth_word signal_2 0)
         (get_nth_word selector 0))
  | _ -> None
;;

let compile_mux tgt selector signals =
  match compile_mux_two tgt selector signals with
  | Some r -> r
  | None -> compile_mux_branchless tgt selector signals
;;

let compile_cat tgt signals =
  let _, with_offset =
    List.fold (List.rev signals) ~init:(0, []) ~f:(fun (current_offset, acc) signal ->
      current_offset + width signal, (current_offset, signal) :: acc)
  in
  List.concat_map with_offset ~f:(fun (bit_offset, signal) ->
    let first_word_bit_offset = bit_offset mod word_size in
    let first_word = bit_offset / word_size in
    List.range 0 (word_count signal + 1)
    |> List.concat_map ~f:(fun word_offset ->
      let v = get_word_at signal ((word_offset * word_size) - first_word_bit_offset) in
      if first_word + word_offset < word_count tgt
      then [ get_nth_word tgt (first_word + word_offset), v ]
      else []))
  |> String.Map.of_alist_multi
  |> Map.to_alist
  |> List.map ~f:(fun (target, values) ->
    sprintf
      "%s = %s;"
      target
      (match values with
       | [] -> c_zero
       | _ -> String.concat ~sep:" | " values))
  |> String.concat ~sep:"\n"
;;

let compile_reg ~to_signal_info signal ~source reg =
  let { Signal.Type.clock = _
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
    | None -> ""
    | Some { clear; clear_to } ->
      sprintf
        "if (%s == 1) { %s } else"
        (get_nth_word (to_signal_info clear) 0)
        (compile_copy_to_prev ~tgt (to_signal_info clear_to))
  in
  match enable with
  | None ->
    sprintf "%s { %s }" c_clear (compile_copy_to_prev ~tgt (to_signal_info source))
  | Some enable ->
    (sprintf "%s if (%s == 1) { %s }")
      c_clear
      (get_nth_word (to_signal_info enable) 0)
      (compile_copy_to_prev ~tgt (to_signal_info source))
;;

let compile_write_port memory _write_clock write_address write_enable write_data =
  let actual_copy =
    if width write_data <= 8
    then
      sprintf
        "((uint8_t*)(&memory[%d]))[%s] = (uint8_t)(%s);"
        (word_offset memory)
        (get_nth_word write_address 0)
        (get_nth_word write_data 0)
    else
      compile_copy_to_address
        (sprintf
           "%d + (%s) * %d"
           (word_offset memory)
           (get_nth_word write_address 0)
           (word_count write_data))
        write_data
  in
  sprintf " if (%s == 1) { %s }" (get_nth_word write_enable 0) actual_copy
;;

let compile_multiport_mem ~to_signal_info signal write_ports =
  Array.to_list write_ports
  |> List.map
       ~f:(fun { Write_port.write_clock; write_address; write_enable; write_data } ->
         compile_write_port
           (to_signal_info signal)
           (to_signal_info write_clock)
           (to_signal_info write_address)
           (to_signal_info write_enable)
           (to_signal_info write_data))
  |> String.concat ~sep:"\n"
;;

let compile_mem_read_port tgt memory address =
  if width tgt <= 8
  then
    sprintf
      "%s = ((uint8_t*)(&memory[%d]))[%s];"
      (get_nth_word tgt 0)
      (word_offset memory)
      (get_nth_word address 0)
  else
    compile_copy_from_address
      tgt
      (sprintf
         "(%d + (%s) * %d)"
         (word_offset memory)
         (get_nth_word address 0)
         (word_count tgt))
;;

let compile_comb_signal ~to_signal_info signal =
  let tgt = to_signal_info signal in
  let code =
    if is_virtual tgt
    then ""
    else (
      match (signal : Signal.t) with
      | Empty -> ""
      | Const { constant; _ } -> compile_const ~tgt constant
      | Not { arg; _ } ->
        let arg = to_signal_info arg in
        compile_not tgt arg
      | Cat { args; _ } -> compile_cat tgt (List.map ~f:to_signal_info args)
      | Mux { select; cases; _ } ->
        let select = to_signal_info select in
        let cases = List.map ~f:to_signal_info cases in
        compile_mux tgt select cases
      | Op2 { op; arg_a; arg_b; _ } ->
        let op2 op a b =
          let a = to_signal_info a in
          let b = to_signal_info b in
          op tgt a b
        in
        (match op with
         | Signal_add -> op2 compile_add
         | Signal_sub -> op2 compile_sub
         | Signal_mulu -> op2 compile_mulu
         | Signal_muls -> op2 compile_muls
         | Signal_and -> op2 (compile_bitop "&")
         | Signal_or -> op2 (compile_bitop "|")
         | Signal_xor -> op2 (compile_bitop "^")
         | Signal_eq -> op2 compile_eq
         | Signal_lt -> op2 compile_lt)
          arg_a
          arg_b
      | Wire { driver = None; _ } -> sprintf "// %s = empty wire" (get_nth_word tgt 0)
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
      | Multiport_mem _ -> ""
      | Mem_read_port { memory; read_address; _ } ->
        compile_mem_read_port tgt (to_signal_info memory) (to_signal_info read_address)
      | Inst _ -> raise_s [%message "Inst signals are unsupported" (signal : Signal.t)])
  in
  let code =
    match local_index tgt with
    | Some index -> sprintf "uint64_t local_%d;\n%s" index code
    | None -> code
  in
  sprintf "// Signal %s\n%s" (Signal.to_string signal) code
;;

let compile_seq_signal ~to_signal_info signal =
  match (signal : Signal.t) with
  | Reg { register = reg; d = source; _ } ->
    compile_reg ~to_signal_info signal ~source reg
  | Multiport_mem { write_ports; _ } ->
    compile_multiport_mem ~to_signal_info signal write_ports
  | _ -> ""
;;

let compile_reset_signal ~to_signal_info signal =
  match (signal : Signal.t) with
  | Reg { register = { reset; _ }; _ } ->
    Option.value_map ~default:"" reset ~f:(fun { reset = _; reset_edge = _; reset_to } ->
      compile_copy ~tgt:(to_signal_info signal) (to_signal_info reset_to)
      ^ "\n"
      ^ compile_copy_to_prev ~tgt:(to_signal_info signal) (to_signal_info reset_to)
      ^ "\n")
  | _ -> ""
;;

let compile_register_initializer ~to_signal_info signal =
  match (signal : Signal.t) with
  | Reg { register = { initialize_to; _ }; _ } ->
    (match initialize_to with
     | None -> ""
     | Some initialize_to ->
       compile_copy ~tgt:(to_signal_info signal) (to_signal_info initialize_to)
       ^ "\n"
       ^ compile_copy_to_prev ~tgt:(to_signal_info signal) (to_signal_info initialize_to)
       ^ "\n")
  | _ -> ""
;;

let compile_memory_initializer ~to_signal_info signal =
  match (signal : Signal.t) with
  | Multiport_mem { initialize_to; _ } ->
    Option.map initialize_to ~f:(fun initialize_to ->
      Array.mapi initialize_to ~f:(fun address init ->
        let memory = to_signal_info signal in
        if Signal.width signal <= 8
        then
          [%string
            "((uint8_t*)(&memory[%{word_offset memory#Int}]))[%{address#Int}] = \
             (uint8_t)(%{get_bits_nth_word init 0});"]
        else (
          let word_count = word_count memory in
          multiline word_count (fun offset ->
            [%string
              "memory[%{word_offset memory#Int} + (%{address#Int} * %{word_count#Int}) + \
               %{offset#Int}] = %{get_bits_nth_word init offset};"])))
      |> Array.to_list)
  | _ -> None
;;
