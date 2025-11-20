open Core
open Hardcaml

let benchmarked_circuit () =
  Hardcaml_test.Generator.gen_circuit ~allow_inputs:false ~depth:9
  |> Quickcheck.Generator.generate ~size:1000 ~random:(Splittable_random.of_int 42)
;;

(*=
   ┌───────────────────────────────────────────┬─────────────┬─────────┬────────────┐
   │ Name                                      │    Time/Run │ mWd/Run │ Percentage │
   ├───────────────────────────────────────────┼─────────────┼─────────┼────────────┤
   │ [bench.ml] cyclesim circuit               │ 42_873.84ns │  12.00w │    100.00% │
   │ [bench.ml] hardcaml-c/tcc circuit         │  6_889.54ns │ 609.00w │     16.07% │
   │ [bench.ml] hardcaml-c/'clang -O3' circuit │  3_078.17ns │ 609.01w │      7.18% │
   │ [bench.ml] verilator circuit              │  3_721.79ns │ 159.00w │      8.68% │
   │ [bench.ml] verilator/opt circuit          │    657.98ns │ 159.00w │      1.53% │
   └───────────────────────────────────────────┴─────────────┴─────────┴────────────┘
*)

let%bench_fun "cyclesim circuit" =
  let sim = Cyclesim.create (benchmarked_circuit ()) in
  fun () -> Cyclesim.cycle sim
;;

let%bench_fun "hardcaml-c/tcc circuit" =
  let sim = Hardcaml_c.Cyclesim.create ~compiler_command:"tcc" (benchmarked_circuit ()) in
  fun () -> Cyclesim.cycle sim
;;

let%bench_fun "hardcaml-c/'clang -O3' circuit" =
  let sim =
    Hardcaml_c.Cyclesim.create ~compiler_command:"clang -O3" (benchmarked_circuit ())
  in
  fun () -> Cyclesim.cycle sim
;;

let%bench_fun "verilator circuit" =
  let sim =
    Hardcaml_verilator.create
      ~verilator_config:{ Hardcaml_verilator.Config.default with optimization_level = O0 }
      ~clock_names:[ "clock" ]
      (benchmarked_circuit ())
  in
  fun () -> Cyclesim.cycle sim
;;

let%bench_fun "verilator/opt circuit" =
  let sim = Hardcaml_verilator.create ~clock_names:[ "clock" ] (benchmarked_circuit ()) in
  fun () -> Cyclesim.cycle sim
;;

let many_inputs_and_outputs () =
  let n = 1000 in
  let values =
    List.init n ~f:(fun i ->
      Signal.output (sprintf "output_%d" i) (Signal.input (sprintf "input_%d" i) 1))
  in
  Circuit.create_exn ~name:"circuit" values
;;

(*=
   ┌────────────────────────────────────┬──────────┬─────────┬──────────┬──────────┬────────────┐
   │ Name                               │ Time/Run │ mWd/Run │ mjWd/Run │ Prom/Run │ Percentage │
   ├────────────────────────────────────┼──────────┼─────────┼──────────┼──────────┼────────────┤
   │ [bench.ml] many inputs and outputs │ 450.85us │ 60.16kw │  335.77w │  338.08w │    100.00% │
   └────────────────────────────────────┴──────────┴─────────┴──────────┴──────────┴────────────┘

   Around 200 ns per port / 30 words allocated.
*)

let%bench_fun "many inputs and outputs" =
  let sim = Hardcaml_c.Cyclesim.create (many_inputs_and_outputs ()) in
  fun () -> Cyclesim.cycle sim
;;
