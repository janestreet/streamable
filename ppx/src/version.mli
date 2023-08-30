open! Base
open! Import

(** The [Version.t] represents the version to use for the stable [Streamable] functors. *)
type t = private V1

val of_int_exn : location -> int -> t
val module_name : t -> label
