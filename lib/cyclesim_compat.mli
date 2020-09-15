open Hardcaml

val create
  :  ?is_internal_port:(Signal.t -> bool)
  -> ?combine_with_cyclesim:bool
  -> ?compiler_command:string
  -> Circuit.t
  -> Cyclesim.t_port_list

module With_interface (I : Interface.S) (O : Interface.S) : sig
  type t = (Bits.t ref I.t, Bits.t ref O.t) Cyclesim.t

  (** Create a simulator using the provided [Create_fn].  The returned simulator ports
      are coerced to the input and output interface types. *)
  val create
    : (?port_checks:Circuit.Port_checks.t
       -> ?add_phantom_inputs:bool
       -> ?modify_outputs:(Signal.t list -> Signal.t list)
       -> ?combine_with_cyclesim:bool
       -> ?compiler_command:
         string
       (* Runs regular Cyclesim simulation and compares
          results. Defaults to [false] *)
       -> Circuit.With_interface(I)(O).create
       -> t)
        Cyclesim.with_create_options
        Circuit.with_create_options

  (** Coerce simulator port types to use the provided input and output interfaces. *)
  val coerce : Cyclesim.t_port_list -> t
end
