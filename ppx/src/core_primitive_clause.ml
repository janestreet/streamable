open! Base
open! Import

let known_zero_arity_core_primitives =
  Set.of_list
    (module String)
    [ "bool"
    ; "bytes"
    ; "char"
    ; "float"
    ; "int"
    ; "int32"
    ; "int64"
    ; "nativeint"
    ; "string"
    ; "unit"
    ]
;;

let primitive ~core_type =
  match core_type.ptyp_desc with
  | Ptyp_constr (longident_loc, []) ->
    (match longident_loc.txt with
     | Lident lident when Set.mem known_zero_arity_core_primitives lident -> Some lident
     | Lident _ | Ldot (_, _) | Lapply (_, _) -> None)
  | _ -> None
;;

let maybe_match type_ (_ : Ctx.t) =
  let%bind core_type = Type_.match_core_type type_ in
  let%map primitive = primitive ~core_type in
  ({ children = []
   ; apply_functor =
       (fun ctx children ->
         assert (List.is_empty children);
         Helpers.apply_streamable_dot
           ctx
           ~functor_name:"Of_atomic"
           ~arguments:
             [ pmod_ident
                 ~loc:ctx.loc
                 (Loc.make
                    ~loc:ctx.loc
                    (Longident.parse [%string "Core.%{String.capitalize primitive}"]))
             ])
   }
   : Clause.Match.t)
;;
