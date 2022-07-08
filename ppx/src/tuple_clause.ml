open! Base
open! Import

let tuple_core_types ~core_type =
  match core_type.ptyp_desc with
  | Ptyp_tuple core_types -> Some core_types
  | _                     -> None
;;

let maybe_match type_ =
  let%bind core_type       = Type.match_core_type type_  in
  let%map tuple_core_types = tuple_core_types ~core_type in
  ({ children      = List.map tuple_core_types ~f:Type.core_type
   ; apply_functor =
       (fun ctx children_modules ->
          let children = List.zip_exn tuple_core_types children_modules in
          Nested_tuple.streamable_of_tuple ctx children)
   }
   : Clause.Match.t)
;;
