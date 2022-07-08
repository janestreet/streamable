open! Base
open! Import

let atomic =
  Attribute.declare
    "streamable.atomic"
    Attribute.Context.core_type
    Ast_pattern.(pstr nil)
    ()
;;
