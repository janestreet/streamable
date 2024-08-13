open! Base
open! Import

let maybe_match type_ (_ : Ctx.t) =
  let%bind core_type = Type_.match_core_type type_ in
  let%map () = Attribute.get Attributes.atomic core_type
  and longident_loc = Helpers.if_module_dot_t_then_module core_type in
  ({ children = []
   ; apply_functor =
       (fun ctx children ->
         assert (List.is_empty children);
         Helpers.apply_streamable_dot
           ctx
           ~functor_name:"Of_atomic"
           ~arguments:[ pmod_ident ~loc:ctx.loc longident_loc ])
   }
   : Clause.Match.t)
;;
