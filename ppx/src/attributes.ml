open! Base
open! Import

let atomic =
  Attribute.declare
    "streamable.atomic"
    Attribute.Context.core_type
    Ast_pattern.(pstr nil)
    ()
;;

let map_with_atomic_values =
  Attribute.declare
    "streamable.map_with_atomic_values"
    Attribute.Context.core_type
    Ast_pattern.(pstr nil)
    ()
;;
