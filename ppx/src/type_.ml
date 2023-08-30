open! Base
open! Import

type t =
  | Type_declaration of type_declaration
  | Core_type of core_type
[@@deriving variants]

let loc = function
  | Type_declaration type_dec -> type_dec.ptype_loc
  | Core_type core_type -> core_type.ptyp_loc
;;

let human_readable_name = function
  | Type_declaration type_dec ->
    (match type_dec.ptype_manifest with
     | Some core_type -> string_of_core_type core_type
     | None -> type_dec.ptype_name.txt)
  | Core_type core_type -> string_of_core_type core_type
;;

let match_core_type = function
  | Type_declaration type_dec -> type_dec.ptype_manifest
  | Core_type core_type -> Some core_type
;;
