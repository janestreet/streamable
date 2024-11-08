open! Base
open! Import

let tuple_core_types ~core_type =
  match Ppxlib_jane.Shim.Core_type_desc.of_parsetree core_type.ptyp_desc with
  | Ptyp_tuple labeled_core_types -> Ppxlib_jane.as_unlabeled_tuple labeled_core_types
  | _ -> None
;;

let maybe_match type_ (_ : Ctx.t) =
  let%bind core_type = Type_.match_core_type type_ in
  let%map tuple_core_types = tuple_core_types ~core_type in
  ({ children = List.map tuple_core_types ~f:Type_.core_type
   ; apply_functor =
       (fun ctx children_modules ->
         let children = List.zip_exn tuple_core_types children_modules in
         Nested_tuple.streamable_of_tuple ctx children)
   }
   : Clause.Match.t)
;;
