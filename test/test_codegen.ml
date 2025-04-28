open Core
open Hardcaml
open Hardcaml_c.For_testing.Codegen

let print_rope r = Rope.to_string r |> print_string

let%expect_test "add" =
  For_testing.compile_add
    (Normal { index = `Global 3000; width = 127 })
    (Normal { index = `Global 2000; width = 127 })
    (Normal { index = `Global 1000; width = 127 })
  |> print_rope;
  [%expect
    {| long_add((uint32_t*)&memory[3000], (uint32_t*)&memory[2000], (uint32_t*)&memory[1000], 127); |}];
  For_testing.compile_add
    (Normal { index = `Global 3000; width = 12 })
    (Normal { index = `Global 2000; width = 12 })
    (Normal { index = `Global 1000; width = 12 })
  |> print_rope;
  [%expect {| memory[3000] = (memory[2000] + memory[1000]) & 0xfffull; |}]
;;

let%expect_test "sub" =
  For_testing.compile_sub
    (Normal { index = `Global 3000; width = 127 })
    (Normal { index = `Global 2000; width = 127 })
    (Normal { index = `Global 1000; width = 127 })
  |> print_rope;
  [%expect
    {| long_sub((uint32_t*)&memory[3000], (uint32_t*)&memory[2000], (uint32_t*)&memory[1000], 127); |}];
  For_testing.compile_sub
    (Normal { index = `Global 3000; width = 12 })
    (Normal { index = `Global 2000; width = 12 })
    (Normal { index = `Global 1000; width = 12 })
  |> print_rope;
  [%expect {| memory[3000] = (memory[2000] - memory[1000]) & 0xfffull; |}]
;;

let%expect_test "lt" =
  For_testing.compile_lt
    (Normal { index = `Global 3000; width = 127 })
    (Normal { index = `Global 2000; width = 127 })
    (Normal { index = `Global 1000; width = 127 })
  |> print_rope;
  [%expect {| memory[3000] = long_lt(&memory[2000], &memory[1000], 127); |}];
  For_testing.compile_lt
    (Normal { index = `Global 3000; width = 12 })
    (Normal { index = `Global 2000; width = 12 })
    (Normal { index = `Global 1000; width = 12 })
  |> print_rope;
  [%expect {| memory[3000] = memory[2000] < memory[1000]; |}]
;;

let%expect_test "eq" =
  For_testing.compile_eq
    (Normal { index = `Global 3000; width = 127 })
    (Normal { index = `Global 2000; width = 127 })
    (Normal { index = `Global 1000; width = 127 })
  |> print_rope;
  [%expect
    {| memory[3000] = ((memory[2000] == memory[1000])&&(memory[2001] == memory[1001])); |}]
;;

let%expect_test "compile_mulu" =
  For_testing.compile_mulu
    (Normal { index = `Global 3000; width = 134 + 83 })
    (Normal { index = `Global 2000; width = 134 })
    (Normal { index = `Global 1000; width = 83 })
  |> print_rope;
  [%expect {| long_mulu(&memory[3000], &memory[2000], &memory[1000], 134, 83); |}];
  For_testing.compile_mulu
    (Normal { index = `Global 3000; width = 30 })
    (Normal { index = `Global 2000; width = 10 })
    (Normal { index = `Global 1000; width = 20 })
  |> print_rope;
  [%expect {| memory[3000] = memory[2000] * memory[1000]; |}]
;;

let%expect_test "compile_muls" =
  For_testing.compile_muls
    (Normal { index = `Global 3000; width = 134 + 83 })
    (Normal { index = `Global 2000; width = 134 })
    (Normal { index = `Global 1000; width = 83 })
  |> print_rope;
  [%expect {| long_muls(&memory[3000], &memory[2000], &memory[1000], 134, 83); |}];
  For_testing.compile_muls
    (Normal { index = `Global 3000; width = 30 })
    (Normal { index = `Global 2000; width = 10 })
    (Normal { index = `Global 1000; width = 20 })
  |> print_rope;
  [%expect {| long_muls(&memory[3000], &memory[2000], &memory[1000], 10, 20); |}]
;;

let%expect_test "compile_const" =
  For_testing.compile_const
    ~tgt:(Normal { index = `Global 3000; width = 110 })
    (Bits.ones 110)
  |> print_rope;
  [%expect
    {|
    memory[3000] = 0xffffffffffffffffull;
    memory[3001] = 0x3fffffffffffull;
    |}]
;;

let%expect_test "bitop" =
  For_testing.compile_bitop
    "£"
    (Normal { index = `Global 3000; width = 127 })
    (Normal { index = `Global 2000; width = 127 })
    (Normal { index = `Global 1000; width = 127 })
  |> print_rope;
  [%expect
    {|
    memory[3000] = memory[2000] £ memory[1000];
    memory[3001] = memory[2001] £ memory[1001];
    |}];
  For_testing.compile_bitop
    "£"
    (Normal { index = `Global 3000; width = 12 })
    (Normal { index = `Global 2000; width = 12 })
    (Normal { index = `Global 1000; width = 12 })
  |> print_rope;
  [%expect {| memory[3000] = memory[2000] £ memory[1000]; |}]
;;

let%expect_test "not" =
  For_testing.compile_not
    (Normal { index = `Global 2000; width = 127 })
    (Normal { index = `Global 1000; width = 127 })
  |> print_rope;
  [%expect
    {|
    memory[2000] = memory[1000] ^ 0xffffffffffffffffull;
    memory[2001] = memory[1001] ^ 0x7fffffffffffffffull;
    |}];
  For_testing.compile_not
    (Normal { index = `Global 2000; width = 12 })
    (Normal { index = `Global 1000; width = 12 })
  |> print_rope;
  [%expect {| memory[2000] = memory[1000] ^ 0xfffull; |}]
;;

let%expect_test "select" =
  For_testing.compile_select
    (Normal { index = `Global 2000; width = 120 })
    (Normal { index = `Global 1000; width = 210 })
    10
    120
  |> print_rope;
  [%expect
    {|
    memory[2000] = ((memory[1000] >> 10) | (memory[1001] << 54)) & 0xffffffffffffffffull;
    memory[2001] = ((memory[1001] >> 10) | (memory[1002] << 54)) & 0xffffffffffffffull;
    |}]
;;

let%expect_test "mux_branchless" =
  For_testing.compile_mux_branchless
    (Normal { index = `Global 2000; width = 2 })
    (Normal { index = `Global 100; width = 4 })
    [ Const (Bits.of_string "00")
    ; Const (Bits.of_string "01")
    ; Const (Bits.of_string "10")
    ]
  |> print_rope;
  [%expect
    {|
    memory[2000] = 0;
    memory[2000] |= ((0x0ull) & (-(memory[100] == 0))) | ((0x1ull) & (-(memory[100] == 1))) | ((0x2ull) & (-(memory[100] == 2)));
    |}];
  For_testing.compile_mux_branchless
    ~chunk_size:2
    (Normal { index = `Global 2000; width = 2 })
    (Normal { index = `Global 100; width = 4 })
    [ Const (Bits.of_string "00")
    ; Const (Bits.of_string "01")
    ; Const (Bits.of_string "10")
    ]
  |> print_rope;
  [%expect
    {|
    memory[2000] = 0;
    memory[2000] |= ((0x0ull) & (-(memory[100] == 0))) | ((0x1ull) & (-(memory[100] == 1)));
    memory[2000] |= ((0x2ull) & (-(memory[100] == 2)));
    |}]
;;

let%expect_test "cases" =
  let test select_width data_width =
    For_testing.compile_cases
      (Normal { index = `Global 100; width = select_width })
      ~default:(Normal { index = `Global 200; width = data_width })
      (Normal { index = `Global 300; width = select_width })
      (List.init 5 ~f:(fun i ->
         ( Bits.random ~width:select_width
         , Normal { index = `Global (100 * (i + 4)); width = data_width } )))
    |> print_rope
  in
  test 5 7;
  [%expect
    {|
    memory[100] = ((memory[300] == 0x03ull)) ? memory[800] : memory[200];
    memory[100] = ((memory[300] == 0x18ull)) ? memory[700] : memory[100];
    memory[100] = ((memory[300] == 0x05ull)) ? memory[600] : memory[100];
    memory[100] = ((memory[300] == 0x0eull)) ? memory[500] : memory[100];
    memory[100] = ((memory[300] == 0x13ull)) ? memory[400] : memory[100];
    |}];
  test 120 100;
  [%expect
    {|
    memory[100] = ((memory[300] == 0xe8cd618ced26fc0full)&&(memory[301] == 0x79a0498cda3605ull)) ? memory[800] : memory[200];
    memory[101] = ((memory[300] == 0xe8cd618ced26fc0full)&&(memory[301] == 0x79a0498cda3605ull)) ? memory[801] : memory[201];
    memory[100] = ((memory[300] == 0x3fa9867f3d252aacull)&&(memory[301] == 0x7da02517796223ull)) ? memory[700] : memory[100];
    memory[101] = ((memory[300] == 0x3fa9867f3d252aacull)&&(memory[301] == 0x7da02517796223ull)) ? memory[701] : memory[101];
    memory[100] = ((memory[300] == 0xa8e76824c87e266full)&&(memory[301] == 0x4094e8316bb2ffull)) ? memory[600] : memory[100];
    memory[101] = ((memory[300] == 0xa8e76824c87e266full)&&(memory[301] == 0x4094e8316bb2ffull)) ? memory[601] : memory[101];
    memory[100] = ((memory[300] == 0x8eeb020bd44b2d66ull)&&(memory[301] == 0xc935425cebe9aeull)) ? memory[500] : memory[100];
    memory[101] = ((memory[300] == 0x8eeb020bd44b2d66ull)&&(memory[301] == 0xc935425cebe9aeull)) ? memory[501] : memory[101];
    memory[100] = ((memory[300] == 0x053cff0edb4021c5ull)&&(memory[301] == 0x150f044f385ebcull)) ? memory[400] : memory[100];
    memory[101] = ((memory[300] == 0x053cff0edb4021c5ull)&&(memory[301] == 0x150f044f385ebcull)) ? memory[401] : memory[101];
    |}]
;;

let%expect_test "mux" =
  For_testing.compile_mux
    (Normal { index = `Global 2000; width = 2 })
    (Normal { index = `Global 100; width = 4 })
    [ Const (Bits.of_string "00"); Const (Bits.of_string "01") ]
  |> print_rope;
  [%expect
    {| memory[2000] = ((0x0ull) & (~(-(memory[100])))) | ((0x1ull) & (-(memory[100]))); |}];
  For_testing.compile_mux
    (Normal { index = `Global 2000; width = 3 })
    (Normal { index = `Global 100; width = 10 })
    (List.init 8 ~f:(fun i -> Const (Bits.of_unsigned_int ~width:10 i)))
  |> print_rope;
  [%expect
    {|
    memory[2000] = 0;
    memory[2000] |= ((0x000ull) & (-(memory[100] == 0))) | ((0x001ull) & (-(memory[100] == 1))) | ((0x002ull) & (-(memory[100] == 2))) | ((0x003ull) & (-(memory[100] == 3))) | ((0x004ull) & (-(memory[100] == 4))) | ((0x005ull) & (-(memory[100] == 5))) | ((0x006ull) & (-(memory[100] == 6))) | ((0x007ull) & (-(memory[100] == 7)));
    |}]
;;

let%expect_test "cat" =
  let cat = [ Bits.of_unsigned_int ~width:5 10; Bits.of_unsigned_int ~width:9 111 ] in
  For_testing.compile_cat
    (Normal
       { index = `Global 2000
       ; width = List.fold ~init:0 cat ~f:(fun a b -> a + Bits.width b)
       })
    (List.map cat ~f:(fun b -> Const b))
  |> print_rope;
  [%expect {| memory[2000] = 0x0aull << 9 | 0x06full; |}];
  let cat =
    [ Bits.of_unsigned_int ~width:5 10
    ; Bits.of_unsigned_int ~width:9 111
    ; Bits.of_unsigned_int ~width:3 4
    ; Bits.of_unsigned_int ~width:19 10234
    ; Bits.of_unsigned_int ~width:25 9862
    ; Bits.of_unsigned_int ~width:33 1124455
    ]
  in
  For_testing.compile_cat
    (Normal
       { index = `Global 2000
       ; width = List.fold ~init:0 cat ~f:(fun a b -> a + Bits.width b)
       })
    (List.map cat ~f:(fun b -> Const b))
  |> print_rope;
  [%expect
    {|
    memory[2000] = 0x027faull << 58 | 0x0002686ull << 33 | 0x000112867ull;
    memory[2001] = 0x0aull << 25 | 0x06full << 16 | 0x4ull << 13 | ((0x027faull >> 6) | (0ull << 58)) | ((0x0002686ull >> 31) | (0ull << 33)) | 0ull;
    |}]
;;

let%expect_test "reg" =
  let clock = Signal.input "clock" 1 in
  let reset = Signal.input "reset" 1 in
  let clear = Signal.input "clear" 1 in
  let enable = Signal.input "enable" 1 in
  let d = Signal.input "d" 8 in
  let to_signal_info d =
    let idx, d =
      List.find_exn
        [ 10, clock; 20, reset; 30, clear; 40, enable; 50, d ]
        ~f:(fun (_, a) -> Signal.Type.Uid.equal (Signal.uid a) (Signal.uid d))
    in
    Normal { index = `Global idx; width = Signal.width d }
  in
  let test (reg : Signal.t) =
    match reg with
    | Reg { register; d; _ } ->
      For_testing.compile_reg ~to_signal_info reg ~source:d register
    | _ -> failwith ""
  in
  print_rope (test (Signal.reg (Signal.Reg_spec.create ~clock ()) d));
  [%expect {| { memory[51] = memory[50]; } |}];
  print_rope (test (Signal.reg (Signal.Reg_spec.create ~clock ()) ~enable d));
  [%expect {| if (memory[40] == 1) { memory[51] = memory[50]; } |}];
  print_rope (test (Signal.reg (Signal.Reg_spec.create ~clock ~reset ()) d));
  [%expect {| { memory[51] = memory[50]; } |}];
  print_rope (test (Signal.reg (Signal.Reg_spec.create ~clock ~clear ()) d));
  [%expect
    {| if (memory[30] == 1) { memory[51] = memory[50]; } else { memory[51] = memory[50]; } |}];
  print_rope
    (test (Signal.reg (Signal.Reg_spec.create ~clock ~clear ~reset ()) ~enable d));
  [%expect
    {| if (memory[30] == 1) { memory[51] = memory[50]; } else if (memory[40] == 1) { memory[51] = memory[50]; } |}]
;;

let%expect_test "mem write port" =
  For_testing.compile_write_port
    (Normal { index = `Global 2000; width = 8 })
    (Normal { index = `Global 100; width = 4 })
    (Normal { index = `Global 200; width = 1 })
    (Normal { index = `Global 300; width = 8 })
  |> print_rope;
  [%expect
    {| if (memory[200] == 1) { ((uint8_t*)(&memory[2000]))[memory[100]] = (uint8_t)(memory[300]); } |}];
  For_testing.compile_write_port
    (Normal { index = `Global 2000; width = 32 })
    (Normal { index = `Global 100; width = 4 })
    (Normal { index = `Global 200; width = 1 })
    (Normal { index = `Global 300; width = 32 })
  |> print_rope;
  [%expect
    {| if (memory[200] == 1) { memory[2000 + (memory[100]) * 1 + 0] = memory[300]; } |}]
;;

let%expect_test "mem read port" =
  For_testing.compile_mem_read_port
    (Normal { index = `Global 3000; width = 8 })
    (Normal { index = `Global 2000; width = 8 })
    (Normal { index = `Global 100; width = 4 })
  |> print_rope;
  [%expect {| memory[3000] = ((uint8_t*)(&memory[2000]))[memory[100]]; |}];
  For_testing.compile_mem_read_port
    (Normal { index = `Global 3000; width = 32 })
    (Normal { index = `Global 2000; width = 32 })
    (Normal { index = `Global 100; width = 4 })
  |> print_rope;
  [%expect {| memory[3000] = memory[(2000 + (memory[100]) * 1) + 0]; |}]
;;
