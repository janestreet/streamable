open! Base
open! Import

let maybe_match =
  Helpers.polymorphic_primitive_or_module_match
    ~num_type_parameters:2
    ~primitive_name:(Some "result")
    ~first_module_name:"Result"
;;
