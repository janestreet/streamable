(** [Attribute.t] values that affect the behavior of ppx_streamable when they are attached
    to sub-expressions of a type definition. *)

open! Base
open! Import

(** [@streamable.atomic]: denotes whether a particular type should be treated as being
    (de)serialized atomically. *)
val atomic : (core_type, unit) Attribute.t
