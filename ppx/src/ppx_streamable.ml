open! Base
open! Import

module For_testing = struct
  module Nested_variant = Nested_variant
  module Nested_tuple = Nested_tuple
end

let streamable_name = "streamable"
let atomic_arg_name = "atomic"
let rpc_arg_name = "rpc"
let version_arg_name = "version"

module Signature = struct
  let args = Deriving.Args.(empty +> flag rpc_arg_name)

  let generate_functor type_dec ~loc ~rpc =
    let streamable_module_type = Helpers.streamable_module_type ~loc ~rpc in
    let type_parameter_module_names =
      List.mapi
        type_dec.ptype_params
        ~f:(fun index (type_parameter, (variance, injectivity)) ->
          let module_name =
            Helpers.module_name_for_type_parameter
              (match
                 Ppxlib_jane.Shim.Core_type_desc.of_parsetree type_parameter.ptyp_desc
               with
               | Ptyp_var (name, _) -> `Ptyp_var name
               | Ptyp_any _ -> `Ptyp_any index
               | _ ->
                 raise_s
                   [%message
                     "Unexpected type for type parameter"
                       [%here]
                       (string_of_core_type type_parameter)])
          in
          (type_parameter, (variance, injectivity)), module_name)
    in
    let type_dec =
      { type_dec with
        ptype_params =
          (* Replace all type parameters with the appropriate concrete types. *)
          List.map
            type_parameter_module_names
            ~f:(fun ((type_parameter, (variance, injectivity)), module_name) ->
              let core_type =
                { type_parameter with
                  ptyp_desc =
                    Ptyp_constr
                      (Loc.make ~loc (Longident.Ldot (Lident module_name, "t")), [])
                }
              in
              core_type, (variance, injectivity))
      }
    in
    let functor_ =
      List.fold_right
        type_parameter_module_names
        ~init:
          (pmty_with
             ~loc
             streamable_module_type
             [ Pwith_type
                 ( Loc.make ~loc (Longident.Lident "t")
                 , Ast_builder.Default.type_declaration
                     ~name:(Loc.make ~loc "t")
                     ~params:[]
                     ~cstrs:[]
                     ~kind:Ptype_abstract
                     ~private_:Public
                     ~manifest:(Some (core_type_of_type_declaration type_dec))
                     ~loc )
             ])
        ~f:(fun (_, module_name) functor_ ->
          pmty_functor
            ~loc
            (Named (Loc.make ~loc (Some module_name), streamable_module_type, [])
             |> Ppxlib_jane.Shim.Functor_parameter.to_parsetree)
            functor_)
    in
    psig_module
      ~loc
      (module_declaration
         ~loc
         ~name:(Loc.make ~loc (Some Helpers.make_streamable))
         ~type_:functor_)
  ;;

  let generate ~loc ~path:(_ : label) ((_ : rec_flag), type_decs) rpc =
    (* Verify that the type declaration is valid. *)
    let type_dec = Helpers.get_the_one_and_only_type_t type_decs ~loc in
    match type_dec.ptype_params with
    | _ :: _ -> [ generate_functor type_dec ~loc ~rpc ]
    | [] ->
      let module_type = Helpers.streamable_module_type ~loc ~rpc in
      [ [%sigi: include [%m module_type] with type t := t] ]
  ;;
end

module Structure = struct
  let args =
    Deriving.Args.(
      empty
      +> flag atomic_arg_name
      +> flag rpc_arg_name
      +> arg version_arg_name (map1' ~f:Version.of_int_exn (eint __)))
  ;;

  let all_ordinary_clauses : Clause.t list =
    [ Core_primitive_clause.maybe_match
    ; Fqueue_clause.maybe_match
    ; Hashtbl_clause.maybe_match
    ; List_clause.maybe_match
    ; Map_clause.maybe_match
    ; Nonempty_list_clause.maybe_match
    ; Option_clause.maybe_match
    ; Or_error_clause.maybe_match
    ; Record_clause.maybe_match
    ; Result_clause.maybe_match
    ; Sequence_clause.maybe_match
    ; Set_clause.maybe_match
    ; Sexp_clause.maybe_match
    ; Total_map_clause.maybe_match
    ; Tuple_clause.maybe_match
    ; Type_parameter_clause.maybe_match
    ; Variant_clause.maybe_match
    ]
  ;;

  (* We attempt to match clauses in distinct groups. In each group, we expect at most one
     clause to match. If we fail to match any clauses from a given group, we move on to
     the next one. The groups are defined in the following order:

     (1) First, we just try applying [Atomic_clause]. If it matches, we should not recurse
         any further.

     (2) If that fails, we try applying all "ordinary" clauses.

     (3) If that fails, we try applying [Parameterized_type_clause]. We consider this
         separately since we'd otherwise have multiple clauses matching known
         parameterized types like [_ Option.t] within the same group.

     (4) Finally, we try applying [Module_dot_t_clause]. We consider this separately since
         we'd otherwise have multiple clauses matching known types like [Sexp.t] within
         the same group. *)
  let clause_groups_in_descending_order_of_precedence =
    [ [ Atomic_clause.maybe_match ]
    ; all_ordinary_clauses
    ; [ Parameterized_type_clause.maybe_match ]
    ; [ Module_dot_t_clause.maybe_match ]
    ]
  ;;

  let rec maybe_apply_clause maybe_match ~type_ ~loc ~rpc ~version =
    match maybe_match type_ { Ctx.loc; rpc; version } with
    | None -> None
    | Some { Clause.Match.apply_functor; children } ->
      (* We recognize the top-level structure of a type expression, now recurse on any
         arguments it may have. *)
      Some
        (apply_functor
           { loc; rpc; version }
           (List.map children ~f:(generate_streamable_module ~rpc ~version)))

  and find_at_most_one_matching_clause clause_group ~type_ ~loc ~rpc ~version =
    match
      List.filter_map clause_group ~f:(maybe_apply_clause ~type_ ~loc ~rpc ~version)
    with
    | [ module_expr ] -> Some module_expr
    | [] -> None
    | (_ : module_expr list) ->
      Location.raise_errorf
        ~loc
        "Multiple matchers satisfied type `%s' for a given clause group. This is likely \
         a bug with [ppx_streamable]."
        (Type_.human_readable_name type_)

  and generate_streamable_module type_ ~rpc ~version =
    let loc = Type_.loc type_ in
    match
      List.find_map
        clause_groups_in_descending_order_of_precedence
        ~f:(find_at_most_one_matching_clause ~type_ ~loc ~rpc ~version)
    with
    | Some module_expr -> module_expr
    | None ->
      Location.raise_errorf
        ~loc
        "Handling of type `%s' is unknown."
        (Type_.human_readable_name type_)
  ;;

  let extract_derivers type_dec =
    let extract_deriver_name_from_expr expr =
      match expr.pexp_desc with
      (* e.g. bin_io *)
      | Pexp_ident { txt = Lident name; _ } -> Some name
      (* e.g. streamable ~rpc *)
      | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident name; _ }; _ }, _) ->
        Some name
      | _ -> None
    in
    List.concat_map type_dec.ptype_attributes ~f:(fun attribute ->
      let attribute_name = attribute.attr_name.txt in
      match
        String.(attribute_name = "deriving" || attribute_name = "deriving_inline")
      with
      | false -> []
      | true ->
        (match attribute.attr_payload with
         (* e.g. [@@deriving foo, bar] *)
         | PStr
             [ { pstr_desc = Pstr_eval (({ pexp_desc; pexp_loc = loc; _ } as expr), _)
               ; _
               }
             ] ->
           (match Ppxlib_jane.Shim.Expression_desc.of_parsetree ~loc pexp_desc with
            | Pexp_tuple labeled_exprs ->
              (match Ppxlib_jane.as_unlabeled_tuple labeled_exprs with
               | Some exprs -> List.filter_map exprs ~f:extract_deriver_name_from_expr
               | None -> [])
              (* e.g. [@@deriving foo] *)
            | _ -> Option.to_list (extract_deriver_name_from_expr expr))
         | _ -> []))
  ;;

  let verify_required_derivers_appear_before
    ~when_passed_args:streamable_args
    ~loc
    ~required_derivers
    ~actual_derivers
    =
    List.fold_until
      actual_derivers
      ~init:required_derivers
      ~finish:(fun (_ : Set.M(String).t) -> ())
      ~f:(fun required_derivers deriver ->
        match String.(deriver = streamable_name) with
        | false -> Continue (Set.remove required_derivers deriver)
        | true ->
          if Set.is_empty required_derivers
          then Stop ()
          else (
            let required_derivers_names =
              String.concat (Set.to_list required_derivers) ~sep:", "
            in
            let streamable_args =
              List.map streamable_args ~f:(fun arg -> "~" ^ arg) |> String.concat ~sep:" "
            in
            Helpers.unsupported_use
              ~loc
              ~why:
                [%string
                  "The following derivers must appear before %{streamable_name} \
                   %{streamable_args}: %{required_derivers_names}"]))
  ;;

  let generate_atomic type_dec ~loc ~rpc ~version =
    let required_derivers =
      match rpc with
      | true -> Set.of_list (module String) [ "bin_io" ]
      | false -> Set.of_list (module String) [ "bin_io"; "sexp" ]
    in
    let actual_derivers = extract_derivers type_dec in
    verify_required_derivers_appear_before
      ~loc
      ~required_derivers
      ~actual_derivers
      ~when_passed_args:(atomic_arg_name :: (if rpc then [ rpc_arg_name ] else []));
    let deriving_expression =
      pexp_tuple
        ~loc
        (List.map (Set.to_list required_derivers) ~f:(fun name ->
           pexp_ident ~loc (Loc.make ~loc (lident name))))
    in
    let type_t_with_deriving =
      [%stri type nonrec t = t [@@deriving [%e deriving_expression]]]
    in
    Helpers.apply_streamable_dot
      { loc; rpc; version }
      ~functor_name:"Of_atomic"
      ~arguments:[ pmod_structure ~loc [ type_t_with_deriving ] ]
  ;;

  let rec generate_for_one_type type_dec ~loc ~atomic ~rpc ~version =
    match type_dec.ptype_params with
    | _ :: _ -> [ generate_functor type_dec ~atomic ~loc ~rpc ~version ]
    | [] ->
      let module_expr =
        match atomic with
        | true -> generate_atomic type_dec ~loc ~rpc ~version
        | false -> generate_streamable_module (Type_declaration type_dec) ~rpc ~version
      in
      [ [%stri
          include
            [%m
            Helpers.apply_streamable_dot
              { loc; rpc; version }
              ~functor_name:"Remove_t"
              ~arguments:[ module_expr ]]]
      ]

  and generate_functor type_dec ~loc ~atomic ~rpc ~version =
    let streamable_module_type = Helpers.streamable_module_type ~loc ~rpc in
    let type_parameter_module_names =
      List.mapi type_dec.ptype_params ~f:(fun index (type_parameter, _) ->
        Helpers.module_name_for_type_parameter
          (match
             Ppxlib_jane.Shim.Core_type_desc.of_parsetree type_parameter.ptyp_desc
           with
           | Ptyp_var (name, _) -> `Ptyp_var name
           | Ptyp_any _ -> `Ptyp_any index
           | _ ->
             raise_s
               [%message
                 "Unexpected type for type parameter"
                   [%here]
                   (string_of_core_type type_parameter)]))
    in
    let functor_body =
      let type_nonrec_t =
        let concrete_t =
          ptyp_constr
            ~loc
            (Loc.make ~loc (lident "t"))
            (List.map type_parameter_module_names ~f:(fun module_name ->
               ptyp_constr
                 ~loc
                 (Loc.make ~loc (Longident.Ldot (Lident module_name, "t")))
                 []))
        in
        [%stri type nonrec t = [%t concrete_t]]
      in
      let include_streamable =
        (* Generate the body of the functor by calling [generate_for_one_type] on this
           type with all type parameters erased. *)
        generate_for_one_type
          { type_dec with ptype_params = [] }
          ~loc
          ~atomic
          ~rpc
          ~version
      in
      pmod_structure ~loc (type_nonrec_t :: include_streamable)
    in
    let functor_ =
      List.fold_right
        type_parameter_module_names
        ~init:functor_body
        ~f:(fun module_name functor_ ->
          pmod_functor
            ~loc
            (Named (Loc.make ~loc (Some module_name), streamable_module_type, [])
             |> Ppxlib_jane.Shim.Functor_parameter.to_parsetree)
            functor_)
    in
    pstr_module
      ~loc
      (module_binding
         ~loc
         ~name:(Loc.make ~loc (Some Helpers.make_streamable))
         ~expr:functor_)
  ;;

  let generate ~loc ~path:(_ : label) ((_ : rec_flag), type_decs) atomic rpc version =
    let version =
      match version with
      | Some version -> version
      | None ->
        Helpers.unsupported_use
          ~loc
          ~why:
            [%string
              "the [~%{version_arg_name}] argument must be specified (as an integer \
               value)"]
    in
    let type_dec = Helpers.get_the_one_and_only_type_t type_decs ~loc in
    generate_for_one_type type_dec ~loc ~atomic ~rpc ~version
  ;;
end

let streamable =
  Deriving.add
    streamable_name
    ~sig_type_decl:(Deriving.Generator.make Signature.args Signature.generate)
    ~str_type_decl:
      (Deriving.Generator.make
         Structure.args
         Structure.generate
         ~attributes:[ T Attributes.atomic; T Attributes.map_with_atomic_values ])
;;
