open! Base
open! Import

let maybe_match type_ (_ : Ctx.t) =
  let%bind core_type = Type_.match_core_type type_ in
  let module_name = Helpers.if_module_dot_t_then_module core_type in
  let%map type_parameters =
    match core_type.ptyp_desc with
    | Ptyp_constr (_, []) -> None
    | Ptyp_constr (longident_loc, type_parameters) ->
      (match Helpers.split_longident longident_loc, module_name with
       | Some (`prefix (Some module_name'), `last "t"), Some module_name
         when [%compare.equal: Longident.t] module_name.txt module_name' ->
         Some type_parameters
       | Some (`prefix None, `last "t"), None -> Some type_parameters
       | _ -> None)
    | _ -> None
  in
  ({ children = List.map type_parameters ~f:Type_.core_type
   ; apply_functor =
       (fun { loc; _ } children ->
         let functor_name =
           match module_name with
           | None -> Loc.make ~loc (Longident.Lident Helpers.make_streamable)
           | Some module_name ->
             Loc.map module_name ~f:(fun module_name ->
               Longident.Ldot (module_name, Helpers.make_streamable))
         in
         let functor_ = pmod_ident ~loc functor_name in
         List.fold children ~init:functor_ ~f:(pmod_apply ~loc))
   }
   : Clause.Match.t)
;;
