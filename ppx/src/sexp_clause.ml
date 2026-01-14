open! Base
open! Import

let maybe_match type_ (_ : Ctx.t) =
  let%bind core_type = Type_.match_core_type type_ in
  let%map module_ =
    match core_type.ptyp_desc with
    | Ptyp_constr (longident_loc, []) ->
      (match
         Helpers.longident_is_like_t
           longident_loc
           ~primitive_name:None
           ~first_module_name:"Sexp"
       with
       | false -> None
       | true -> Helpers.if_module_dot_t_then_module core_type)
    | _ -> None
  in
  ({ children = []
   ; apply_functor =
       (fun ctx (_ : module_expr list) ->
         Helpers.apply_streamable_dot
           (* NOTE: There is no [Of_sexpable_rpc] since, by definition, there must be both
              [of_sexp] and [sexp_of]. As a result, explicitly ask for the functor without
              the "_rpc" suffix. *)
           { ctx with rpc = false }
           ~functor_name:[%string "Of_sexpable"]
           ~arguments:[ pmod_ident ~loc:ctx.loc module_ ])
   }
   : Clause.Match.t)
;;
