open! Base
open! Import

let maybe_match =
  Helpers.polymorphic_primitive_or_module_match
    ~num_type_parameters:1
    ~primitive_name:(Some "list")
    ~first_module_name:"List"
;;
