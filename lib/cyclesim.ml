open Core
open Hardcaml

type t =
  { input_signals : (Bits.t ref * Signal.t) list
  ; output_signals_before : (Bits.t ref * Signal.t) list
  ; output_signals_after : (Bits.t ref * Signal.t) list
  ; internal_signals : (Bits.Mutable.t * Signal.t) list
  ; simulator : Simulator.Instance.t
  ; run_seq : Simulator.Instance.t -> unit
  ; run_comb : Simulator.Instance.t -> unit
  ; run_comb_last_layer : Simulator.Instance.t -> unit
  ; run_reset : Simulator.Instance.t -> unit
  }

let copy_to_outputs simulator output_signals =
  List.iter output_signals ~f:(fun (bits_ref, signal) ->
    bits_ref := Simulator.Instance.read simulator signal)
;;

let copy_to_internal_signals t =
  List.iter t.internal_signals ~f:(fun (bits_mutable, signal) ->
    Simulator.Instance.read_mutable t.simulator signal bits_mutable)
;;

let copy_to_inputs t =
  List.iter t.input_signals ~f:(fun (bits_ref, signal) ->
    Simulator.Instance.write t.simulator signal !bits_ref)
;;

let cycle_before_clock_edge t =
  copy_to_inputs t;
  t.run_comb t.simulator;
  copy_to_internal_signals t;
  copy_to_outputs t.simulator t.output_signals_before
;;

let cycle_at_clock_edge t = t.run_seq t.simulator

let cycle_after_clock_edge t =
  t.run_comb_last_layer t.simulator;
  copy_to_outputs t.simulator t.output_signals_after
;;

let reset t =
  copy_to_inputs t;
  (* recompute combinatorial logic to make sure reg_reset_value has a correct value *)
  t.run_comb t.simulator;
  t.run_reset t.simulator;
  copy_to_outputs t.simulator t.output_signals_before;
  copy_to_outputs t.simulator t.output_signals_after
;;

let create_internal_signal_map internal_signals =
  List.map internal_signals ~f:(fun signal ->
    let width = Signal.width signal in
    Signal.uid signal, (Bits.Mutable.create width, signal))
  |> Map.of_alist_exn (module Signal.Type.Uid)
;;

let create
  ?(config = Cyclesim.Config.default)
  ?(combine_with_cyclesim = false)
  ?compiler_command
  circuit
  =
  let circuit =
    if config.deduplicate_signals then Hardcaml.Dedup.deduplicate circuit else circuit
  in
  let traced =
    Cyclesim.Private.Traced_nodes.create ~is_internal_port:config.is_internal_port circuit
  in
  let internal_signals = List.map traced.internal_signals ~f:(fun t -> t.signal) in
  if List.length internal_signals > 1000
  then
    fprintf
      stderr
      !"%{Source_code_position}: You have %d internal signals in your design. This will \
        ruin simulation perfomance. Please don't pass [Fn.const true] as \
        [is_internal_port]."
      [%here]
      (List.length internal_signals);
  let simulator = Simulator.create ~interesting_signals:internal_signals circuit in
  let internal_signals_map = create_internal_signal_map internal_signals in
  let internal_signal_list =
    List.map internal_signals ~f:(fun signal ->
      Map.find_exn internal_signals_map (Signal.uid signal))
  in
  let run_seq = Simulator.add_function simulator (Simulator.make_seq_code simulator) in
  let run_comb = Simulator.add_function simulator (Simulator.make_comb_code simulator) in
  let run_comb_last_layer =
    Simulator.add_function simulator (Simulator.make_comb_last_layer_code simulator)
  in
  let run_reset =
    Simulator.add_function simulator (Simulator.make_reset_code simulator)
  in
  let run_initialization =
    let run_register_initialization =
      Simulator.add_function
        simulator
        (Simulator.make_register_initialization_code simulator)
    in
    let run_memory_initialization =
      Simulator.add_function
        simulator
        (Simulator.make_memory_initialization_code simulator)
    in
    fun instance ->
      run_register_initialization instance;
      run_memory_initialization instance
  in
  let instance = Simulator.start ?compiler_command simulator in
  run_initialization instance;
  let port_refs s = List.map s ~f:(fun s -> ref (Bits.zero (Signal.width s)), s) in
  let t =
    { input_signals = port_refs (Circuit.inputs circuit)
    ; output_signals_before = port_refs (Circuit.outputs circuit)
    ; output_signals_after = port_refs (Circuit.outputs circuit)
    ; internal_signals = internal_signal_list
    ; simulator = instance
    ; run_seq
    ; run_comb
    ; run_comb_last_layer
    ; run_reset
    }
  in
  let port_list ports =
    List.map ports ~f:(fun (bits, signal) ->
      let names = Signal.names signal in
      match names with
      | [ name ] -> name, bits
      | _ ->
        raise_s [%message "Circuit ports must have a single name" (names : string list)])
  in
  let lookup_node_by_id signal_id =
    Map.find internal_signals_map signal_id
    |> Option.map ~f:(fun (bits, _) -> Cyclesim.Node.create_from_bits_mutable bits)
  in
  let lookup_node (traced : Cyclesim.Traced.internal_signal) =
    lookup_node_by_id (Signal.uid traced.signal)
  in
  let lookup_unsupported _ = raise_s [%message "lookup unsupported in hardcaml C"] in
  let sim =
    Cyclesim.Private.create
      ~in_ports:(port_list t.input_signals)
      ~out_ports_before_clock_edge:(port_list t.output_signals_before)
      ~out_ports_after_clock_edge:(port_list t.output_signals_after)
      ~reset:(fun () -> reset t)
      ~clock_mode:`All_one_domain
      ~clocks_aligned:(Fn.const true)
      ~cycle_check:(fun () -> ())
      ~cycle_before_clock_edge:(fun () -> cycle_before_clock_edge t)
      ~cycle_at_clock_edge:(fun () -> cycle_at_clock_edge t)
      ~cycle_after_clock_edge:(fun () -> cycle_after_clock_edge t)
      ~traced
      ~lookup_node_by_id
      ~lookup_node
      ~lookup_reg_by_id:lookup_unsupported
      ~lookup_reg:lookup_unsupported
      ~lookup_mem:lookup_unsupported
      ()
  in
  if combine_with_cyclesim
  then (
    let on_error error =
      let open Cyclesim.Combine_error in
      match error.clock_edge with
      | Hardcaml.Side.Before ->
        raise_s
          [%message
            "Cyclesim/Hardcaml_c output port values differ before clock edge"
              (error.port_name : string)
              ~cyclesim_value:(error.value1 : Bits.t)
              ~hardcaml_c_value:(error.value0 : Bits.t)]
      | Hardcaml.Side.After ->
        raise_s
          [%message
            "Cyclesim/Hardcaml_c output port values differ after clock edge"
              (error.port_name : string)
              ~cyclesim_value:(error.value1 : Bits.t)
              ~hardcaml_c_value:(error.value0 : Bits.t)]
    in
    let cyclesim = Cyclesim.create circuit in
    (* it's important [sim] is first - otherwise internal signals will come from Cyclesim,
       not Hardcaml_c *)
    Cyclesim.combine ~on_error sim cyclesim)
  else sim
;;

module With_interface (I : Hardcaml.Interface.S) (O : Hardcaml.Interface.S) = struct
  type nonrec t = (Bits.t ref I.t, Bits.t ref O.t) Cyclesim.t

  module C = Circuit.With_interface (I) (O)

  let coerce sim =
    let find_port ports (name, width) =
      match List.Assoc.find ports name ~equal:String.equal with
      | Some x -> x
      | None -> ref (Bits.zero width)
    in
    let to_input ports = I.map I.port_names_and_widths ~f:(find_port ports) in
    let to_output ports = O.map O.port_names_and_widths ~f:(find_port ports) in
    Cyclesim.Private.coerce sim ~to_input ~to_output
  ;;

  let create
    ?config
    ?circuit_config
    ?(combine_with_cyclesim = false)
    ?compiler_command
    create_fn
    =
    let circuit = C.create_exn ?config:circuit_config ~name:"simulator" create_fn in
    let sim = create ?config ~combine_with_cyclesim ?compiler_command circuit in
    coerce sim
  ;;
end
