open Core
open Hardcaml

type t =
  { signals : (Bits.t ref * Signal.t) Signal.Type.Uid_map.t
  ; input_signals : (Bits.t ref * Signal.t) list
  ; output_signals : (Bits.t ref * Signal.t) list
  ; internal_signals : (Bits.Mutable.t * Signal.t) list
  ; simulator : Simulator.Instance.t
  ; run_seq : Simulator.Instance.t -> unit
  ; run_comb : Simulator.Instance.t -> unit
  ; run_comb_last_layer : Simulator.Instance.t -> unit
  ; run_reset : Simulator.Instance.t -> unit
  }

let signals_to_refs t =
  List.iter t.output_signals ~f:(fun (bits_ref, signal) ->
    bits_ref := Simulator.Instance.read t.simulator signal)
;;

let internal_signals_to_refs t =
  List.iter t.internal_signals ~f:(fun (bits_mutable, signal) ->
    Simulator.Instance.read_mutable t.simulator signal bits_mutable)
;;

let refs_to_signals t =
  List.iter t.input_signals ~f:(fun (bits_ref, signal) ->
    Simulator.Instance.write t.simulator signal !bits_ref)
;;

let cycle_before_clock_edge t =
  refs_to_signals t;
  t.run_comb t.simulator;
  internal_signals_to_refs t;
  signals_to_refs t
;;

let cycle_at_clock_edge t =
  refs_to_signals t;
  t.run_seq t.simulator;
  signals_to_refs t
;;

let cycle_after_clock_edge t =
  refs_to_signals t;
  t.run_comb_last_layer t.simulator;
  signals_to_refs t
;;

let reset t =
  refs_to_signals t;
  (* recompute combinatorial logic to make sure reg_reset_value has a correct value *)
  t.run_comb t.simulator;
  t.run_reset t.simulator;
  signals_to_refs t
;;

let make_port_list signal_map signals =
  List.concat_map signals ~f:(fun signal ->
    let bits_ref, _ = Map.find_exn signal_map (Signal.uid signal) in
    List.map (Signal.names signal) ~f:(fun name -> name, bits_ref))
  |> List.fold ~init:String.Map.empty ~f:(fun acc (name, bits_ref) ->
       Map.set acc ~key:name ~data:bits_ref (* waveterm doesn't like duplicates *))
  |> Map.to_alist
;;

let create_port_signal_map ports =
  List.map ports ~f:(fun signal ->
    let width = Signal.width signal in
    Signal.uid signal, (ref (Bits.zero width), signal))
  |> Map.of_alist_exn (module Signal.Uid)
;;

let create_internal_signal_map internal_signals =
  List.map internal_signals ~f:(fun signal ->
    let width = Signal.width signal in
    Signal.uid signal, (Bits.Mutable.create width, signal))
  |> Map.of_alist_exn (module Signal.Uid)
;;

let create
  ?(config = Cyclesim.Config.default)
  ?(combine_with_cyclesim = false)
  ?compiler_command
  circuit
  =
  let circuit = Hardcaml.Dedup.deduplicate circuit in
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
  let port_signals = Circuit.outputs circuit @ Circuit.inputs circuit in
  let simulator = Simulator.create ~interesting_signals:internal_signals circuit in
  let port_signals_map = create_port_signal_map port_signals in
  let internal_signals_map = create_internal_signal_map internal_signals in
  let signal_list signal_list =
    List.map signal_list ~f:(fun signal ->
      Map.find_exn port_signals_map (Signal.uid signal))
  in
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
  let instance = Simulator.start ?compiler_command simulator in
  let t =
    { signals = port_signals_map
    ; input_signals = signal_list (Circuit.inputs circuit)
    ; output_signals = signal_list (Circuit.outputs circuit)
    ; internal_signals = internal_signal_list
    ; simulator = instance
    ; run_seq
    ; run_comb
    ; run_comb_last_layer
    ; run_reset
    }
  in
  let lookup_node (traced : Cyclesim.Traced.internal_signal) =
    Map.find internal_signals_map (Signal.uid traced.signal)
    |> Option.map ~f:(fun (bits, _) -> Cyclesim.Node.create_from_bits_mutable bits)
  in
  let lookup_unsupported _ = raise_s [%message "lookup unsupported in hardcaml C"] in
  let sim =
    Cyclesim.Private.create
      ~in_ports:(make_port_list t.signals (Circuit.inputs circuit))
      ~out_ports_before_clock_edge:(make_port_list t.signals (Circuit.outputs circuit))
      ~out_ports_after_clock_edge:(make_port_list t.signals (Circuit.outputs circuit))
      ~reset:(fun () -> reset t)
      ~cycle_check:(fun () -> ())
      ~cycle_before_clock_edge:(fun () -> cycle_before_clock_edge t)
      ~cycle_at_clock_edge:(fun () -> cycle_at_clock_edge t)
      ~cycle_after_clock_edge:(fun () -> cycle_after_clock_edge t)
      ~traced
      ~lookup_node
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
        (* we don't support (yet?) recording values before clock edge *)
        ()
      | Hardcaml.Side.After ->
        raise_s
          [%message
            "Cyclesim/Event_driven_sim output port values differ"
              (error.port_name : string)
              ~cyclesim_value:(error.value1 : Bits.t)
              ~event_driven_sim_value:(error.value0 : Bits.t)]
    in
    let cyclesim = Cyclesim.create circuit in
    (* it's important [sim] is first - otherwise internal signals will come from Cyclesim,
       not Event_driven_sim *)
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
