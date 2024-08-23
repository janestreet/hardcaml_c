open! Core
open Hardcaml
open Hardcaml_c
module Cyclesim = Hardcaml.Cyclesim
module C_cyclesim = Hardcaml_c.Cyclesim

let%expect_test "no clock" =
  let open Hardcaml.Signal in
  let signal = of_string "1110" in
  let circuit = Circuit.create_exn ~name:"generated" [ output "out" signal ] in
  let sim = C_cyclesim.create circuit ~combine_with_cyclesim:true in
  Cyclesim.cycle sim;
  let out_port = Cyclesim.out_port sim "out" in
  printf !"%{Bits}\n" !out_port;
  [%expect {| 1110 |}]
;;

let test_circuit () =
  let open Hardcaml.Signal in
  let input = input "input" 63 in
  let a1 = input +:. 0x123456789 in
  let a2 = concat_msb [ a1; a1 ] in
  let v =
    a2
    |: of_constant
         (Constant.of_hex_string
            "123456789012345678"
            ~width:126
            ~signedness:Signedness.Unsigned)
  in
  Circuit.create_exn ~name:"test" [ output "output" v ]
;;

let%expect_test "simple" =
  let circuit = test_circuit () in
  let t = create circuit in
  make_comb_code t |> String.concat ~sep:"\n" |> print_endline;
  [%expect
    {|
    // Signal Empty
    uint64_t local_1;

    // Signal Wire[id:1 bits:63 names:input deps:0] -> 0
    // memory[0] = empty wire
    // Signal Const[id:3 bits:63 names: deps:] = 000000000000000000000000000000100100011010001010110011110001001

    // Signal Op[id:4 bits:63 names: deps:1,3] = add
    uint64_t local_3;
    local_3 = (memory[0] + 0x0000000123456789ull) & 0x7fffffffffffffffull;
    // Signal Op[id:5 bits:126 names: deps:4,4] = cat
    memory[1] = local_3 << 63 | local_3;
    memory[2] = ((local_3 >> 1) | (0ull << 63)) | 0ull;
    // Signal Const[id:6 bits:126 names: deps:] = 000000000000000000000000000000000000000000000000000000000100100011010001010110011110001001000000010010001101000101011001111000
    memory[3] = 0x3456789012345678ull;
    memory[4] = 0x0000000000000012ull;
    // Signal Op[id:7 bits:126 names: deps:5,6] = or
    memory[5] = memory[1] | memory[3];
    memory[6] = memory[2] | memory[4];
    // Signal Wire[id:2 bits:126 names:output deps:7] -> 7
    |}];
  make_seq_code t |> String.concat ~sep:"\n" |> print_endline;
  [%expect {| |}]
;;

let%expect_test "eval" =
  let circuit = test_circuit () in
  let t = create circuit in
  let out = List.nth_exn (Circuit.outputs circuit) 0 in
  let instance = start t in
  Instance.read instance out
  |> Bits.to_constant
  |> Constant.to_hex_string ~signedness:Signedness.Unsigned
  |> printf !"out=%s";
  [%expect {| out=00000000000000000000000000000000 |}]
;;

let test_register_circuit () =
  let open Hardcaml.Signal in
  let clk = input "clk" 1 in
  let enable = input "ena" 1 in
  let reg_spec = Reg_spec.create () ~clock:clk in
  let v = reg_fb ~enable ~width:5 reg_spec ~f:(fun s -> s +:. 3) in
  let circuit = Circuit.create_exn ~name:"test" [ output "output" v ] in
  circuit
;;

let%expect_test "register" =
  let circuit = test_register_circuit () in
  let t = create circuit in
  make_comb_code t |> String.concat ~sep:"\n" |> print_endline;
  [%expect
    {|
    // Signal Reg[id:7 bits:5 names: deps:3,2,0,5,0,6,1]
    memory[0] = memory[1];
    // Signal Wire[id:4 bits:5 names:output deps:7] -> 7

    // Signal Const[id:8 bits:5 names: deps:] = 00011

    // Signal Op[id:9 bits:5 names: deps:7,8] = add
    memory[2] = (memory[0] + 0x03ull) & 0x1full;
    // Signal Wire[id:3 bits:5 names: deps:9] -> 9

    // Signal Empty

    // Signal Wire[id:2 bits:1 names:clk deps:0] -> 0
    // memory[3] = empty wire
    // Signal Const[id:5 bits:5 names: deps:] = 00000

    // Signal Const[id:6 bits:5 names: deps:] = 00000

    // Signal Wire[id:1 bits:1 names:ena deps:0] -> 0
    // memory[4] = empty wire
    |}];
  make_comb_last_layer_code t |> String.concat ~sep:"\n" |> print_endline;
  [%expect
    {|
    // Signal Reg[id:7 bits:5 names: deps:3,2,0,5,0,6,1]
    memory[0] = memory[1];
    // Signal Wire[id:4 bits:5 names:output deps:7] -> 7
    |}];
  make_seq_code t |> String.concat ~sep:"\n" |> print_endline;
  [%expect {| if (memory[4] == 1) { memory[1] = memory[2]; } |}]
;;

let%expect_test "register eval" =
  let circuit = test_register_circuit () in
  let sim = C_cyclesim.create ~combine_with_cyclesim:true circuit in
  let ena = Cyclesim.in_port sim "ena" in
  let output_before = Cyclesim.out_port ~clock_edge:Before sim "output" in
  let output_after = Cyclesim.out_port ~clock_edge:After sim "output" in
  ena := Bits.of_string "1";
  Cyclesim.cycle sim;
  printf !"%{Bits} %{Bits}\n" !output_before !output_after;
  Cyclesim.cycle sim;
  printf !"%{Bits} %{Bits}\n" !output_before !output_after;
  Cyclesim.cycle sim;
  printf !"%{Bits} %{Bits}\n" !output_before !output_after;
  [%expect
    {|
    00000 00011
    00011 00110
    00110 01001
    |}]
;;

let test_multiport_memory_circuit () =
  let open Hardcaml.Signal in
  let clk = input "clk" 1 in
  let ports =
    multiport_memory
      12
      ~write_ports:
        [| { Write_port.write_clock = clk
           ; write_address = of_string "0011"
           ; write_data = of_string "1110"
           ; write_enable = of_string "1"
           }
        |]
      ~read_addresses:[| of_string "0001" |]
  in
  let v = ports.(0) in
  Circuit.create_exn ~name:"test" [ output "output" v ]
;;

let%expect_test "multiport memory" =
  let circuit = test_multiport_memory_circuit () in
  let t = create circuit in
  make_comb_code t |> String.concat ~sep:"\n" |> print_endline;
  [%expect
    {|
    // Signal Const[id:3 bits:4 names: deps:] = 0001

    // Signal Multiport_mem[id:7 bits:4 names: deps:1,4,5,6]

    // Signal Mem_read_port[id:8 bits:4 names: deps:3,7]
    memory[2] = ((uint8_t*)(&memory[0]))[0x1ull];
    // Signal Wire[id:2 bits:4 names:output deps:8] -> 8

    // Signal Empty
    uint64_t local_3;

    // Signal Wire[id:1 bits:1 names:clk deps:0] -> 0
    // memory[3] = empty wire
    // Signal Const[id:4 bits:4 names: deps:] = 0011

    // Signal Const[id:5 bits:4 names: deps:] = 1110

    // Signal Const[id:6 bits:1 names: deps:] = 1
    |}];
  make_seq_code t |> String.concat ~sep:"\n" |> print_endline;
  [%expect
    {| if (0x1ull == 1) { ((uint8_t*)(&memory[0]))[0x3ull] = (uint8_t)(0xeull); } |}]
;;

let%expect_test "Initial values, resets and clears of registers" =
  let open Hardcaml.Signal in
  let clock = input "clock" 1 in
  let reset = input "reset" 1 in
  let clear = input "clear" 1 in
  let q =
    reg_fb
      (Reg_spec.create ~clock ~reset ~clear ())
      ~initialize_to:(Signal.of_int ~width:8 16)
      ~reset_to:(Signal.of_int ~width:8 32)
      ~clear_to:(Signal.of_int ~width:8 48)
      ~width:8
      ~f:(fun d -> d +:. 1)
  in
  let circ = Circuit.create_exn ~name:"initial" [ output "q" q ] in
  let sim = C_cyclesim.create circ in
  let waves, sim = Hardcaml_waveterm.Waveform.create sim in
  for _ = 0 to 1 do
    Cyclesim.cycle sim
  done;
  Cyclesim.reset sim;
  for _ = 0 to 1 do
    Cyclesim.cycle sim
  done;
  Cyclesim.in_port sim "clear" := Bits.vdd;
  Cyclesim.cycle sim;
  Cyclesim.in_port sim "clear" := Bits.gnd;
  for _ = 0 to 1 do
    Cyclesim.cycle sim
  done;
  Hardcaml_waveterm.Waveform.print waves ~wave_width:2 ~display_height:12;
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clock          ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──│
    │               ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  │
    │reset          ││            ┌─────┐                                │
    │               ││────────────┘     └─────────────────────────────   │
    │clear          ││                              ┌─────┐              │
    │               ││──────────────────────────────┘     └───────────   │
    │               ││──────┬─────┬───────────┬─────┬─────┬─────┬─────   │
    │q              ││ 10   │11   │20         │21   │22   │30   │31      │
    │               ││──────┴─────┴───────────┴─────┴─────┴─────┴─────   │
    │               ││                                                   │
    └───────────────┘└───────────────────────────────────────────────────┘
    |}]
;;
