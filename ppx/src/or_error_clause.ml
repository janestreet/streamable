open! Base
open! Import

let type_parameter ~core_type =
  match core_type.ptyp_desc with
  | Ptyp_constr (longident_loc, [ type_parameter ]) ->
    (match
       Helpers.longident_is_like_t
         longident_loc
         ~primitive_name:None
         ~first_module_name:"Or_error"
     with
     | false -> None
     | true -> Some type_parameter)
  | _ -> None
;;

let maybe_match type_ { Ctx.version; _ } =
  let%bind core_type = Type_.match_core_type type_ in
  let%map type_parameter = type_parameter ~core_type in
  ({ children =
       [ Core_type type_parameter
       ; Core_type
           (Helpers.core_type_with_atomic_attribute
              ~loc:(Type_.loc type_)
              ~module_dot_t:
                (match version with
                 | V1 -> "Core.Error.Stable.V2.t"))
       ]
   ; apply_functor =
       (fun ctx children ->
         Helpers.apply_streamable_dot ctx ~functor_name:"Of_result" ~arguments:children)
   }
   : Clause.Match.t)
;;
