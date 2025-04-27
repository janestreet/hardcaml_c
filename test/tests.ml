open! Core
open Hardcaml
open Hardcaml_c
module Cyclesim = Hardcaml.Cyclesim
module C_cyclesim = Hardcaml_c.Cyclesim

let print_rope r = print_string (Rope.to_string r)
let print_ropes r = print_rope (Rope.concat ~sep:[%rope "\n"] r)

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
  make_comb_code t |> print_ropes;
  [%expect
    {|
    // Signal Wire[id:1 bits:63 names:input deps:] -> ()
    // memory[0] = empty wire
    // Signal Const[id:3 bits:63 names: deps:] = 000000000000000000000000000000100100011010001010110011110001001

    // Signal Op[id:4 bits:63 names: deps:1,3] = add
    uint64_t local_2;
    local_2 = (memory[0] + 0x0000000123456789ull) & 0x7fffffffffffffffull;
    // Signal Op[id:5 bits:126 names: deps:4,4] = cat
    memory[1] = local_2 << 63 | local_2;
    memory[2] = ((local_2 >> 1) | (0ull << 63)) | 0ull;
    // Signal Const[id:6 bits:126 names: deps:] = 000000000000000000000000000000000000000000000000000000000100100011010001010110011110001001000000010010001101000101011001111000
    memory[3] = 0x3456789012345678ull;
    memory[4] = 0x0000000000000012ull;
    // Signal Op[id:7 bits:126 names: deps:5,6] = or
    memory[5] = memory[1] | memory[3];
    memory[6] = memory[2] | memory[4];
    // Signal Wire[id:2 bits:126 names:output deps:7] -> 7
    |}];
  make_seq_code t |> print_ropes;
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
  let v =
    reg_fb
      ~enable
      ~initialize_to:(Signal.of_string "10101")
      ~width:5
      reg_spec
      ~f:(fun s -> s +:. 3)
  in
  let circuit = Circuit.create_exn ~name:"test" [ output "output" v ] in
  circuit
;;

let%expect_test "register" =
  let circuit = test_register_circuit () in
  let t = create circuit in
  make_comb_code t |> print_ropes;
  [%expect
    {|
    // Signal Reg[id:6 bits:5 names: deps:3,2,5,1]
    memory[0] = memory[1];
    // Signal Wire[id:4 bits:5 names:output deps:6] -> 6

    // Signal Const[id:7 bits:5 names: deps:] = 00011

    // Signal Op[id:8 bits:5 names: deps:6,7] = add
    memory[2] = (memory[0] + 0x03ull) & 0x1full;
    // Signal Wire[id:3 bits:5 names: deps:8] -> 8

    // Signal Wire[id:2 bits:1 names:clk deps:] -> ()
    // memory[3] = empty wire
    // Signal Const[id:5 bits:5 names: deps:] = 10101

    // Signal Wire[id:1 bits:1 names:ena deps:] -> ()
    // memory[4] = empty wire
    |}];
  make_comb_last_layer_code t |> print_ropes;
  [%expect
    {|
    // Signal Reg[id:6 bits:5 names: deps:3,2,5,1]
    memory[0] = memory[1];
    // Signal Wire[id:4 bits:5 names:output deps:6] -> 6
    |}];
  make_seq_code t |> print_ropes;
  [%expect {| if (memory[4] == 1) { memory[1] = memory[2]; } |}];
  make_register_initialization_code t |> print_ropes;
  [%expect
    {|
    memory[0] = 0x15ull;
    memory[1] = 0x15ull;
    |}]
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
    10101 11000
    11000 11011
    11011 11110
    |}]
;;

let test_multiport_memory_circuit data_width =
  let open Hardcaml.Signal in
  let clk = input "clk" 1 in
  let size = 12 in
  let ports =
    multiport_memory
      size
      ~write_ports:
        [| { Write_port.write_clock = clk
           ; write_address = of_string "0011"
           ; write_data = ones data_width
           ; write_enable = of_string "1"
           }
        |]
      ~read_addresses:[| of_string "0001" |]
      ~initialize_to:(Array.init size ~f:(fun _ -> Bits.random ~width:data_width))
  in
  let v = ports.(0) in
  Circuit.create_exn ~name:"test" [ output "output" v ]
;;

let%expect_test "multiport memory" =
  let circuit = test_multiport_memory_circuit 4 in
  let t = create circuit in
  make_comb_code t |> print_ropes;
  [%expect
    {|
    // Signal Const[id:3 bits:4 names: deps:] = 0001

    // Signal Multiport_mem[id:7 bits:4 names: deps:1,4,5,6]

    // Signal Mem_read_port[id:8 bits:4 names: deps:3,7]
    memory[2] = ((uint8_t*)(&memory[0]))[0x1ull];
    // Signal Wire[id:2 bits:4 names:output deps:8] -> 8

    // Signal Wire[id:1 bits:1 names:clk deps:] -> ()
    // memory[3] = empty wire
    // Signal Const[id:4 bits:4 names: deps:] = 0011

    // Signal Const[id:5 bits:4 names: deps:] = 1111

    // Signal Const[id:6 bits:1 names: deps:] = 1
    |}];
  make_seq_code t |> print_ropes;
  [%expect
    {| if (0x1ull == 1) { ((uint8_t*)(&memory[0]))[0x3ull] = (uint8_t)(0xfull); } |}];
  make_memory_initialization_code t |> print_ropes;
  [%expect
    {|
    ((uint8_t*)(&memory[0]))[0] = (uint8_t)(0x3ull);
    ((uint8_t*)(&memory[0]))[1] = (uint8_t)(0x8ull);
    ((uint8_t*)(&memory[0]))[2] = (uint8_t)(0x5ull);
    ((uint8_t*)(&memory[0]))[3] = (uint8_t)(0xeull);
    ((uint8_t*)(&memory[0]))[4] = (uint8_t)(0x3ull);
    ((uint8_t*)(&memory[0]))[5] = (uint8_t)(0xfull);
    ((uint8_t*)(&memory[0]))[6] = (uint8_t)(0xcull);
    ((uint8_t*)(&memory[0]))[7] = (uint8_t)(0xdull);
    ((uint8_t*)(&memory[0]))[8] = (uint8_t)(0x1ull);
    ((uint8_t*)(&memory[0]))[9] = (uint8_t)(0x8ull);
    ((uint8_t*)(&memory[0]))[10] = (uint8_t)(0x6ull);
    ((uint8_t*)(&memory[0]))[11] = (uint8_t)(0xcull);
    |}];
  test_multiport_memory_circuit 32
  |> create
  |> make_memory_initialization_code
  |> print_ropes;
  [%expect
    {|
    memory[0 + (0 * 1) + 0] = 0xefac79a0ull;
    memory[0 + (1 * 1) + 0] = 0x7f3d252aull;
    memory[0 + (2 * 1) + 0] = 0x233fa986ull;
    memory[0 + (3 * 1) + 0] = 0x25177962ull;
    memory[0 + (4 * 1) + 0] = 0x386f7da0ull;
    memory[0 + (5 * 1) + 0] = 0x24c87e26ull;
    memory[0 + (6 * 1) + 0] = 0xffa8e768ull;
    memory[0 + (7 * 1) + 0] = 0xe8316bb2ull;
    memory[0 + (8 * 1) + 0] = 0x3f664094ull;
    memory[0 + (9 * 1) + 0] = 0x0bd44b2dull;
    memory[0 + (10 * 1) + 0] = 0xae8eeb02ull;
    memory[0 + (11 * 1) + 0] = 0x425cebe9ull;
    |}];
  test_multiport_memory_circuit 64
  |> create
  |> make_memory_initialization_code
  |> print_ropes;
  [%expect
    {|
    memory[0 + (0 * 1) + 0] = 0x0edb402101c5c935ull;
    memory[0 + (1 * 1) + 0] = 0x044f385ebc053cffull;
    memory[0 + (2 * 1) + 0] = 0x1031c7ca25f1150full;
    memory[0 + (3 * 1) + 0] = 0xd375ac763c67e451ull;
    memory[0 + (4 * 1) + 0] = 0x8912b341bfa0df6cull;
    memory[0 + (5 * 1) + 0] = 0xfa06538b5df8f200ull;
    memory[0 + (6 * 1) + 0] = 0x96d15b3cd264e516ull;
    memory[0 + (7 * 1) + 0] = 0x1aa8afe5e28f4273ull;
    memory[0 + (8 * 1) + 0] = 0x19864b85d5dd310bull;
    memory[0 + (9 * 1) + 0] = 0xf22058ee53536c2eull;
    memory[0 + (10 * 1) + 0] = 0x1cabb8cdd4a140daull;
    memory[0 + (11 * 1) + 0] = 0xf87889536818c9efull;
    |}];
  test_multiport_memory_circuit 96
  |> create
  |> make_memory_initialization_code
  |> print_ropes;
  [%expect
    {|
    memory[0 + (0 * 2) + 0] = 0xeb5f03ea6ec9ec03ull;
    memory[0 + (0 * 2) + 1] = 0x522c8244ull;
    memory[0 + (1 * 2) + 0] = 0x6b46139ea911bdb4ull;
    memory[0 + (1 * 2) + 1] = 0x6e08ef0dull;
    memory[0 + (2 * 2) + 0] = 0xe633945390f701c7ull;
    memory[0 + (2 * 2) + 1] = 0x3771c681ull;
    memory[0 + (3 * 2) + 0] = 0xa23314c175a235f4ull;
    memory[0 + (3 * 2) + 1] = 0xf5e505c6ull;
    memory[0 + (4 * 2) + 0] = 0xbb0ce17d9683bfbaull;
    memory[0 + (4 * 2) + 1] = 0x5d2743f7ull;
    memory[0 + (5 * 2) + 0] = 0x1241c8c2f69ed5e4ull;
    memory[0 + (5 * 2) + 1] = 0xb236e4f4ull;
    memory[0 + (6 * 2) + 0] = 0xcbcc5ffebd34acffull;
    memory[0 + (6 * 2) + 1] = 0xa22ac4f7ull;
    memory[0 + (7 * 2) + 0] = 0xe54be2e2d080f85bull;
    memory[0 + (7 * 2) + 1] = 0x77217a99ull;
    memory[0 + (8 * 2) + 0] = 0xdfef1d71af2149d4ull;
    memory[0 + (8 * 2) + 1] = 0x114d92cdull;
    memory[0 + (9 * 2) + 0] = 0xf836656f6f52dd71ull;
    memory[0 + (9 * 2) + 1] = 0x0ce74aa6ull;
    memory[0 + (10 * 2) + 0] = 0x75b19be37c7eec4eull;
    memory[0 + (10 * 2) + 1] = 0x325f06aeull;
    memory[0 + (11 * 2) + 0] = 0x048e9c6ea221eb25ull;
    memory[0 + (11 * 2) + 1] = 0x72958780ull;
    |}]
;;

let%expect_test "Initial values, resets and clears of registers" =
  let open Hardcaml.Signal in
  let clock = input "clock" 1 in
  let reset = input "reset" 1 in
  let clear = input "clear" 1 in
  let q =
    reg_fb
      (Reg_spec.create ~clock ~reset ~clear ())
      ~initialize_to:(Signal.of_unsigned_int ~width:8 16)
      ~reset_to:(Signal.of_unsigned_int ~width:8 32)
      ~clear_to:(Signal.of_unsigned_int ~width:8 48)
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
  Hardcaml_waveterm.Waveform.print waves ~wave_width:2;
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
    └───────────────┘└───────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "Initial values of memory" =
  let open Signal in
  let test data_width =
    let clock = input "clock" 1 in
    let address_width = 3 in
    let size = 1 lsl address_width in
    let read_address =
      (* Start a counter at 2 *)
      reg_fb
        (Reg_spec.create ~clock ())
        ~initialize_to:(Signal.of_unsigned_int ~width:address_width 2)
        ~width:address_width
        ~f:(fun d -> d +:. 1)
    in
    let ports =
      multiport_memory
        size
        ~write_ports:
          [| { Write_port.write_clock = clock
             ; write_address = input "write_address" address_width
             ; write_data = input "write_data" data_width
             ; write_enable = input "write_enable" 1
             }
          |]
        ~read_addresses:[| read_address |]
        ~initialize_to:(Array.init size ~f:(Bits.of_unsigned_int ~width:data_width))
    in
    let circ = Circuit.create_exn ~name:"test" [ output "q" ports.(0) ] in
    let sim = C_cyclesim.create circ in
    let waves, sim = Hardcaml_waveterm.Waveform.create sim in
    for _ = 0 to 1 do
      Cyclesim.cycle sim
    done;
    Cyclesim.in_port sim "write_enable" := Bits.vdd;
    Cyclesim.in_port sim "write_data" := Bits.ones data_width;
    Cyclesim.in_port sim "write_address" := Bits.of_unsigned_int ~width:address_width 6;
    Cyclesim.cycle sim;
    Cyclesim.in_port sim "write_enable" := Bits.gnd;
    for _ = 0 to 5 do
      Cyclesim.cycle sim
    done;
    Hardcaml_waveterm.Waveform.print
      ~display_rules:
        [ Hardcaml_waveterm.Display_rule.port_name_matches
            Re.Posix.(compile (re ".*"))
            ~wave_format:(Bit_or Unsigned_int)
            ~alignment:Right
        ]
      waves
      ~wave_width:2
  in
  test 8;
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clock          ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──│
    │               ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  │
    │               ││────────────┬──────────────────────────────────────│
    │write_address  ││ 0          │6                                     │
    │               ││────────────┴──────────────────────────────────────│
    │               ││────────────┬──────────────────────────────────────│
    │write_data     ││ 0          │255                                   │
    │               ││────────────┴──────────────────────────────────────│
    │write_enable   ││            ┌─────┐                                │
    │               ││────────────┘     └────────────────────────────────│
    │               ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬──│
    │q              ││ 2    │3    │4    │5    │255  │7    │0    │1    │2 │
    │               ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴──│
    └───────────────┘└───────────────────────────────────────────────────┘
    |}];
  test 96;
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clock          ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──│
    │               ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  │
    │               ││────────────┬──────────────────────────────────────│
    │write_address  ││ 0          │6                                     │
    │               ││────────────┴──────────────────────────────────────│
    │               ││────────────┬──────────────────────────────────────│
    │write_data     ││ 0          │79228162514264337593543950335         │
    │               ││────────────┴──────────────────────────────────────│
    │write_enable   ││            ┌─────┐                                │
    │               ││────────────┘     └────────────────────────────────│
    │               ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬──│
    │q              ││ 2    │3    │4    │5    │.0335│7    │0    │1    │2 │
    │               ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴──│
    └───────────────┘└───────────────────────────────────────────────────┘
    |}]
;;
