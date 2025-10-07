open! Core
open Hardcaml

type normal_signal_info =
  { width : int
  ; index : [ `Local of int | `Global of int ]
  }

type signal_info =
  | Const of Bits.t
  | Normal of normal_signal_info
  | Virtual of normal_signal_info

val word_offset : signal_info -> int
val word_size : int
val word_bytes : int
val width_to_word_count : int -> int
val word_count : signal_info -> int
val width : signal_info -> int
val compile_comb_signal : to_signal_info:(Signal.t -> signal_info) -> Signal.t -> Rope.t
val compile_seq_signal : to_signal_info:(Signal.t -> signal_info) -> Signal.t -> Rope.t
val compile_reset_signal : to_signal_info:(Signal.t -> signal_info) -> Signal.t -> Rope.t

val compile_register_initializer
  :  to_signal_info:(Signal.t -> signal_info)
  -> Signal.t
  -> Rope.t

val compile_memory_initializer
  :  to_signal_info:(Signal.t -> signal_info)
  -> Signal.t
  -> Rope.t list option

module For_testing : sig
  val compile_add : signal_info -> signal_info -> signal_info -> Rope.t
  val compile_sub : signal_info -> signal_info -> signal_info -> Rope.t
  val compile_lt : signal_info -> signal_info -> signal_info -> Rope.t
  val compile_eq : signal_info -> signal_info -> signal_info -> Rope.t
  val compile_mulu : signal_info -> signal_info -> signal_info -> Rope.t
  val compile_muls : signal_info -> signal_info -> signal_info -> Rope.t
  val compile_const : tgt:signal_info -> Bits.t -> Rope.t
  val compile_bitop : string -> signal_info -> signal_info -> signal_info -> Rope.t
  val compile_not : signal_info -> signal_info -> Rope.t
  val compile_select : signal_info -> signal_info -> int -> int -> Rope.t

  val compile_mux_branchless
    :  ?chunk_size:int
    -> signal_info
    -> signal_info
    -> signal_info list
    -> Rope.t

  val compile_cases
    :  signal_info
    -> default:signal_info
    -> signal_info
    -> (Bits.t * signal_info) list
    -> Rope.t

  val compile_mux : signal_info -> signal_info -> signal_info list -> Rope.t
  val compile_cat : signal_info -> signal_info list -> Rope.t

  val compile_reg
    :  to_signal_info:(Signal.t -> signal_info)
    -> Signal.t
    -> source:Signal.t
    -> Signal.t Hardcaml.Signal.Type.Reg.Register.t
    -> Rope.t

  val compile_write_port
    :  signal_info
    -> signal_info
    -> signal_info
    -> signal_info
    -> Rope.t

  val compile_mem_read_port : signal_info -> signal_info -> signal_info -> Rope.t
end
