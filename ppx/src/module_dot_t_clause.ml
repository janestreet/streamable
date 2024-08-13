open! Base
open! Import

let maybe_match type_ (_ : Ctx.t) =
  let%bind core_type = Type_.match_core_type type_ in
  let%map module_name = Helpers.if_module_dot_t_then_module core_type
  and () =
    match core_type.ptyp_desc with
    | Ptyp_constr (_, []) -> Some ()
    | _ -> None
  in
  ({ children = []
   ; apply_functor =
       (fun { loc; _ } children ->
         assert (List.is_empty children);
         pmod_ident ~loc module_name)
   }
   : Clause.Match.t)
;;
