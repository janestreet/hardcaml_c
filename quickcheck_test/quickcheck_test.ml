open Core
open Hardcaml

let cycle_count = 15

let cyclesim_eval inputs sim =
  Cyclesim.reset sim;
  let results = ref [] in
  let out_port = Cyclesim.out_port sim "out" in
  List.iter inputs ~f:(fun input_values ->
    List.iter input_values ~f:(fun (name, value) -> Cyclesim.in_port sim name := value);
    Cyclesim.cycle sim;
    results := !out_port :: !results);
  !results |> List.rev
;;

let gen_circuit_and_inputs =
  let open Quickcheck.Let_syntax in
  let%bind circuit = Hardcaml_test.Generator.gen_circuit ~allow_inputs:true ~depth:5 in
  let%map inputs =
    Quickcheck.Generator.list_with_length
      cycle_count
      (Hardcaml_test.Generator.gen_input_data circuit)
  in
  circuit, inputs
;;

let%test_unit "cyclesim vs hardcaml-c" =
  Quickcheck.test ~trials:300 gen_circuit_and_inputs ~f:(fun (circuit, inputs) ->
    let sim =
      Hardcaml_c.Cyclesim.create
        ?compiler_command:(Sys.getenv "HARDCAML_CC")
        ~combine_with_cyclesim:true
        circuit
    in
    let my_out = cyclesim_eval inputs sim in
    let sim = Cyclesim.create circuit in
    let expected_out = cyclesim_eval inputs sim in
    if not ([%equal: Bits.t list] my_out expected_out)
    then (
      print_s [%message "inputs" (inputs : (string * Bits.t) list list)];
      Rtl.print Verilog circuit;
      let c = Hardcaml_c.create circuit in
      let print m r =
        let r = Rope.concat ~sep:[%rope "\n"] r in
        [%rope "%{m#String}: \n%{r}\n"] |> Rope.to_string |> printf "%s%!"
      in
      Hardcaml_c.make_comb_code c |> print "comb";
      Hardcaml_c.make_seq_code c |> print "seq";
      Hardcaml_c.make_reset_code c |> print "reset";
      raise_s
        [%message "invalid result" (my_out : Bits.t list) (expected_out : Bits.t list)]))
;;
