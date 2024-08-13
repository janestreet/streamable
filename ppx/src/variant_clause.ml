open! Base
open! Import

let pcstr_core_types ~constructor_declaration =
  match constructor_declaration.pcd_args with
  | Pcstr_tuple args -> List.map args ~f:Ppxlib_jane.Shim.Pcstr_tuple_arg.to_core_type
  | Pcstr_record labels -> List.map labels ~f:(fun label_decl -> label_decl.pld_type)
;;

let if_no_arg ~constructor_declaration ~then_ ~else_ =
  match pcstr_core_types ~constructor_declaration with
  | [] -> then_ ()
  | core_types -> else_ core_types
;;

let match_no_arg_tuple_or_record ~constructor_declaration ~no_arg ~tuple ~record =
  match constructor_declaration.pcd_args with
  | Pcstr_tuple [] -> no_arg ()
  | Pcstr_tuple core_types -> tuple core_types
  | Pcstr_record labels -> record labels
;;

let constructor_declarations type_dec =
  match type_dec.ptype_kind with
  | Ptype_variant constructor_declarations -> Some constructor_declarations
  | _ -> None
;;

let children ~loc ~constructor_declarations =
  List.map constructor_declarations ~f:(fun constructor_declaration ->
    if_no_arg
      ~constructor_declaration
      ~else_:(fun core_types -> ptyp_tuple ~loc core_types)
      ~then_:(fun () ->
        Helpers.core_type_with_atomic_attribute ~loc ~module_dot_t:"Core.Unit.t"))
;;

let streamable_module (ctx : Ctx.t) children =
  match List.map ~f:snd children with
  | [] ->
    Helpers.apply_streamable_dot
      ctx
      ~functor_name:"Of_atomic"
      ~arguments:
        [ pmod_ident ~loc:ctx.loc (Loc.make ~loc:ctx.loc (Longident.parse "Core.Nothing"))
        ]
  | _ -> Nested_variant.streamable_of_variant ctx children
;;

let to_streamable_fun ~loc ~constructor_declarations =
  match constructor_declarations with
  | [] ->
    [%expr
      function
      | (_ : t) -> .]
  | _ ->
    let cases =
      List.mapi
        constructor_declarations
        ~f:(fun constructor_index constructor_declaration ->
          let lhs_subpattern =
            match_no_arg_tuple_or_record
              ~constructor_declaration
              ~no_arg:(fun () -> None)
              ~tuple:(fun args ->
                Some
                  (ppat_tuple
                     ~loc
                     (List.mapi
                        args
                        ~f:(fun index (_ : Ppxlib_jane.Shim.Pcstr_tuple_arg.t) ->
                          let var = Loc.make ~loc (Helpers.lowercase_name_of_num index) in
                          ppat_var ~loc var))))
              ~record:(fun label_decalarations ->
                Some
                  (ppat_record
                     ~loc
                     (List.map label_decalarations ~f:(fun label_declaration ->
                        let field = Loc.map ~f:lident label_declaration.pld_name in
                        let pattern =
                          Loc.make ~loc (Loc.txt label_declaration.pld_name)
                        in
                        field, ppat_var ~loc pattern))
                     Closed))
          in
          let lhs_pattern =
            ppat_construct
              ~loc
              (Loc.map constructor_declaration.pcd_name ~f:lident)
              lhs_subpattern
          in
          let rhs_tuple =
            match_no_arg_tuple_or_record
              ~constructor_declaration
              ~no_arg:(fun () -> [%expr ()])
              ~tuple:(fun args ->
                pexp_tuple
                  ~loc
                  (List.mapi
                     args
                     ~f:(fun index (_ : Ppxlib_jane.Shim.Pcstr_tuple_arg.t) ->
                       let ident =
                         Loc.make ~loc (lident (Helpers.lowercase_name_of_num index))
                       in
                       pexp_ident ~loc ident)))
              ~record:(fun label_declarations ->
                pexp_tuple
                  ~loc
                  (List.map label_declarations ~f:(fun label_declaration ->
                     pexp_ident ~loc (Loc.map label_declaration.pld_name ~f:lident))))
          in
          let rhs_expression =
            match List.length constructor_declarations with
            (* Don't map to a variant if there is only one constructor. *)
            | 1 -> rhs_tuple
            (* Else, wrap the tuple in a variant. *)
            | _ ->
              pexp_variant
                ~loc
                (Helpers.uppercase_name_of_num constructor_index)
                (Some rhs_tuple)
          in
          case ~lhs:lhs_pattern ~guard:None ~rhs:rhs_expression)
    in
    pexp_function ~loc cases
;;

let of_streamable_fun ~loc ~constructor_declarations =
  match constructor_declarations with
  | [] -> [%expr Core.Nothing.unreachable_code]
  | _ ->
    let cases =
      List.mapi
        constructor_declarations
        ~f:(fun constructor_index constructor_declaration ->
          let lhs_tuple =
            match_no_arg_tuple_or_record
              ~constructor_declaration
              ~no_arg:(fun () -> [%pat? ()])
              ~tuple:(fun args ->
                ppat_tuple
                  ~loc
                  (List.mapi
                     args
                     ~f:(fun index (_ : Ppxlib_jane.Shim.Pcstr_tuple_arg.t) ->
                       ppat_var ~loc (Loc.make ~loc (Helpers.lowercase_name_of_num index)))))
              ~record:(fun label_declarations ->
                ppat_tuple
                  ~loc
                  (List.map label_declarations ~f:(fun label_declaration ->
                     ppat_var ~loc label_declaration.pld_name)))
          in
          let lhs_pattern =
            match List.length constructor_declarations with
            (* Don't map to a variant if there is only one constructor. *)
            | 1 -> lhs_tuple
            | _ ->
              (* Else, wrap the tuple in a variant. *)
              ppat_variant
                ~loc
                (Helpers.uppercase_name_of_num constructor_index)
                (Some lhs_tuple)
          in
          let rhs_subexpression =
            match_no_arg_tuple_or_record
              ~constructor_declaration
              ~no_arg:(fun () -> None)
              ~tuple:(fun args ->
                Some
                  (pexp_tuple
                     ~loc
                     (List.mapi
                        args
                        ~f:(fun index (_ : Ppxlib_jane.Shim.Pcstr_tuple_arg.t) ->
                          let ident =
                            Loc.make ~loc (lident (Helpers.lowercase_name_of_num index))
                          in
                          pexp_ident ~loc ident))))
              ~record:(fun label_declarations ->
                Some
                  (pexp_record
                     ~loc
                     (List.map label_declarations ~f:(fun label_declaration ->
                        let ident = Loc.map label_declaration.pld_name ~f:lident in
                        ident, pexp_ident ~loc ident))
                     None))
          in
          let rhs_expression =
            pexp_construct
              ~loc
              (Loc.map constructor_declaration.pcd_name ~f:lident)
              rhs_subexpression
          in
          case ~lhs:lhs_pattern ~guard:None ~rhs:rhs_expression)
    in
    pexp_function ~loc cases
;;

let maybe_match type_ (_ : Ctx.t) =
  Helpers.type_declaration_match
    type_
    ~payload:constructor_declarations
    ~children:(fun ~loc ~payload -> children ~loc ~constructor_declarations:payload)
    ~streamable_module
    ~to_streamable_fun:(fun ~loc ~payload ->
      to_streamable_fun ~loc ~constructor_declarations:payload)
    ~of_streamable_fun:(fun ~loc ~payload ->
      of_streamable_fun ~loc ~constructor_declarations:payload)
;;
