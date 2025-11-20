open Hardcaml

val create
  :  ?config:Cyclesim.Config.t
  -> ?combine_with_cyclesim:bool
  -> ?compiler_command:string
  -> Circuit.t
  -> Cyclesim.t_port_list

module With_interface (I : Interface.S) (O : Interface.S) : sig
  type t = (Bits.t ref I.t, Bits.t ref O.t) Cyclesim.t

  (** Create a simulator using the provided [Create_fn]. The returned simulator ports are
      coerced to the input and output interface types. *)
  val create
    :  ?config:Cyclesim.Config.t
    -> ?circuit_config:Circuit.Config.t
    -> ?combine_with_cyclesim:bool
    -> ?compiler_command:
         string
         (* Runs regular Cyclesim simulation and compares results. Defaults to [false] *)
    -> Circuit.With_interface(I)(O).create
    -> t

  (** Coerce simulator port types to use the provided input and output interfaces. *)
  val coerce : Cyclesim.t_port_list -> t
end
