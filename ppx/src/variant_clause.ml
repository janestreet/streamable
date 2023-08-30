open! Base
open! Import

let pcstr_tuple_core_types ~loc ~constructor_declaration =
  match constructor_declaration.pcd_args with
  | Pcstr_tuple core_types -> core_types
  | Pcstr_record _ ->
    Helpers.unsupported_use
      ~loc
      ~why:
        [%string
          "variant constructor `%{constructor_declaration.pcd_name.txt}' contains an \
           anonymous record argument, which isn't supported"]
;;

let if_no_arg ~loc ~constructor_declaration ~then_ ~else_ =
  match pcstr_tuple_core_types ~loc ~constructor_declaration with
  | [] -> then_ ()
  | core_types -> else_ core_types
;;

let constructor_declarations type_dec =
  match type_dec.ptype_kind with
  | Ptype_variant constructor_declarations -> Some constructor_declarations
  | _ -> None
;;

let children ~loc ~constructor_declarations =
  List.map constructor_declarations ~f:(fun constructor_declaration ->
    if_no_arg
      ~loc
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
        let lhs_tuple =
          if_no_arg
            ~loc
            ~constructor_declaration
            ~then_:(fun () -> None)
            ~else_:(fun core_types ->
              Some
                (ppat_tuple
                   ~loc
                   (List.mapi core_types ~f:(fun core_type_index (_ : core_type) ->
                      let var =
                        Loc.make ~loc (Helpers.lowercase_name_of_num core_type_index)
                      in
                      ppat_var ~loc var))))
        in
        let lhs_pattern =
          ppat_construct
            ~loc
            (Loc.map constructor_declaration.pcd_name ~f:lident)
            lhs_tuple
        in
        let rhs_tuple =
          if_no_arg
            ~loc
            ~constructor_declaration
            ~then_:(fun () -> [%expr ()])
            ~else_:(fun core_types ->
              pexp_tuple
                ~loc
                (List.mapi core_types ~f:(fun core_type_index (_ : core_type) ->
                   let ident =
                     Loc.make
                       ~loc
                       (lident (Helpers.lowercase_name_of_num core_type_index))
                   in
                   pexp_ident ~loc ident)))
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
          if_no_arg
            ~loc
            ~constructor_declaration
            ~then_:(fun () -> [%pat? ()])
            ~else_:(fun core_types ->
              ppat_tuple
                ~loc
                (List.mapi core_types ~f:(fun core_type_index (_ : core_type) ->
                   ppat_var
                     ~loc
                     (Loc.make ~loc (Helpers.lowercase_name_of_num core_type_index)))))
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
        let rhs_tuple =
          if_no_arg
            ~loc
            ~constructor_declaration
            ~then_:(fun () -> None)
            ~else_:(fun core_types ->
              Some
                (pexp_tuple
                   ~loc
                   (List.mapi core_types ~f:(fun core_type_index (_ : core_type) ->
                      let ident =
                        Loc.make
                          ~loc
                          (lident (Helpers.lowercase_name_of_num core_type_index))
                      in
                      pexp_ident ~loc ident))))
        in
        let rhs_expression =
          pexp_construct
            ~loc
            (Loc.map constructor_declaration.pcd_name ~f:lident)
            rhs_tuple
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
