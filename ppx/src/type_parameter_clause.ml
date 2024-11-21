open! Base
open! Import

let maybe_match type_ (_ : Ctx.t) =
  let%bind core_type = Type_.match_core_type type_ in
  let%map module_name =
    match Ppxlib_jane.Shim.Core_type_desc.of_parsetree core_type.ptyp_desc with
    | Ptyp_var (name, _) -> Some (Helpers.module_name_for_type_parameter (`Ptyp_var name))
    | _ -> None
  in
  ({ children = []
   ; apply_functor =
       (fun { loc; _ } children ->
         assert (List.is_empty children);
         pmod_ident ~loc (Loc.make ~loc (Longident.Lident module_name)))
   }
   : Clause.Match.t)
;;
