open! Base
open! Import

(** Represents either a [type_declaration] or [core_type], both of which may appear as
    sub-expressions within the toplevel [type t]. *)
type t =
  | Type_declaration of type_declaration
  | Core_type of core_type
[@@deriving variants]

(** Grabs the code location of the [t]. *)
val loc : t -> location

(** Converts the [t] into some human-readable string. This is primarily used for
    error-message reporting. *)
val human_readable_name : t -> label

val match_core_type : t -> core_type option
