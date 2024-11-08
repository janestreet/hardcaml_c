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
val compile_comb_signal : to_signal_info:(Signal.t -> signal_info) -> Signal.t -> string
val compile_seq_signal : to_signal_info:(Signal.t -> signal_info) -> Signal.t -> string
val compile_reset_signal : to_signal_info:(Signal.t -> signal_info) -> Signal.t -> string

val compile_register_initializer
  :  to_signal_info:(Signal.t -> signal_info)
  -> Signal.t
  -> string

val compile_memory_initializer
  :  to_signal_info:(Signal.t -> signal_info)
  -> Signal.t
  -> string list option
