open! Core
open Hardcaml

type t

module Instance : sig
  type t

  (** Write value into simulator instance memory. *)
  val write : t -> Signal.t -> Bits.t -> unit

  (** Read value from simulator instance memory. *)
  val read : t -> Signal.t -> Bits.t

  (** Read mutable value from simulator instance memory. *)
  val read_mutable : t -> Signal.t -> Bits.Mutable.t -> unit
end

(** Create a new simulator for a given circuit. It will be only possible to
    [Instance.read] and [Instance.write] signals that are in [interesting_signals] or are
    inputs/outputs of the circuit. *)
val create : ?interesting_signals:Signal.t list -> Circuit.t -> t

(** Start a new simulator. *)
val start : ?compiler_command:string -> t -> Instance.t

(* low level API *)

(** Add a C code to the simulator. Returns a function that, when called on an instance
    will execute this function. *)
val add_function : t -> Rope.t list -> Instance.t -> unit

val make_seq_code : t -> Rope.t list
val make_comb_code : t -> Rope.t list
val make_comb_last_layer_code : t -> Rope.t list
val make_reset_code : t -> Rope.t list
val make_register_initialization_code : t -> Rope.t list
val make_memory_initialization_code : t -> Rope.t list
