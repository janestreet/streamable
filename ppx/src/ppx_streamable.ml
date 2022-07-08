open! Base
open! Import

module For_testing = struct
  module Nested_variant = Nested_variant
  module Nested_tuple   = Nested_tuple
end

let streamable_name  = "streamable"
let atomic_arg_name  = "atomic"
let rpc_arg_name     = "rpc"
let version_arg_name = "version"

module Signature = struct
  let args = Deriving.Args.(empty +> flag rpc_arg_name)

  let generate ~loc ~path:(_ : label) ((_ : rec_flag), type_decs) rpc =
    (* Verify that the type declaration is valid. *)
    let (_ : type_declaration) = Helpers.get_the_one_and_only_type_t type_decs ~loc in
    let module_type_name       = if rpc then "Streamable.S_rpc" else "Streamable.S" in
    let module_type =
      pmty_ident ~loc (Loc.make ~loc (Longident.parse module_type_name))
    in
    [ [%sigi: include [%m module_type] with type t := t] ]
  ;;
end

module Structure = struct
  let args =
    Deriving.Args.(
      empty +> flag atomic_arg_name +> flag rpc_arg_name +> arg version_arg_name (eint __))
  ;;

  let all_patterns : Clause.t list =
    [ Atomic_clause.maybe_match
    ; Core_primitive_clause.maybe_match
    ; Fqueue_clause.maybe_match
    ; Hashtbl_clause.maybe_match
    ; List_clause.maybe_match
    ; Map_clause.maybe_match
    ; Option_clause.maybe_match
    ; Or_error_clause.maybe_match
    ; Record_clause.maybe_match
    ; Result_clause.maybe_match
    ; Sequence_clause.maybe_match
    ; Set_clause.maybe_match
    ; Total_map_clause.maybe_match
    ; Tuple_clause.maybe_match
    ; Variant_clause.maybe_match
    ]
  ;;

  let rec generate_from_clauses type_ ~rpc ~version =
    let loc = Type.loc type_ in
    match
      List.filter_map all_patterns ~f:(fun maybe_match ->
        match maybe_match type_ with
        | None -> None
        | Some { apply_functor; children } ->
          (* We recognize the top-level structure of a type expression, now
             recurse on any arguments it may have. *)
          Some
            (apply_functor
               { loc; rpc; version }
               (List.map children ~f:(generate_from_clauses ~rpc ~version))))
    with
    | [ module_expr ] -> module_expr
    | []              ->
      (match Type.chop_t_suffix type_ with
       (* If no matchers are satisfied, default to trying to use the module name itself
          (without the [.t] suffix). *)
       | Some longident -> pmod_ident ~loc (Loc.make ~loc longident)
       | None           ->
         Location.raise_errorf
           ~loc
           "Handling of type `%s' is unknown."
           (Type.human_readable_name type_))
    | (_ : module_expr list) ->
      Location.raise_errorf
        ~loc
        "Multiple matchers satisfied type `%s'. This is likely a bug with \
         [ppx_streamable]."
        (Type.human_readable_name type_)
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
      | true  ->
        (match attribute.attr_payload with
         (* e.g. [@@deriving foo, bar] *)
         | PStr [ { pstr_desc = Pstr_eval ({ pexp_desc = Pexp_tuple exprs; _ }, _); _ } ]
           -> List.filter_map exprs ~f:extract_deriver_name_from_expr
         (* e.g. [@@deriving foo] *)
         | PStr [ { pstr_desc = Pstr_eval (expr, _); _ } ] ->
           Option.to_list (extract_deriver_name_from_expr expr)
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
        | true  ->
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
      | true  -> Set.of_list (module String) [ "bin_io"         ]
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

  let generate ~loc ~path:(_ : label) ((_ : rec_flag), type_decs) atomic rpc version =
    let version =
      match version with
      | Some version -> version
      | None         ->
        Helpers.unsupported_use
          ~loc
          ~why:
            [%string
              "the [~%{version_arg_name}] argument must be specified (as an integer \
               value)"]
    in
    let type_dec = Helpers.get_the_one_and_only_type_t type_decs ~loc in
    let module_expr =
      match atomic with
      | true  -> generate_atomic type_dec ~loc ~rpc ~version
      | false -> generate_from_clauses (Type_declaration type_dec) ~rpc ~version
    in
    [ [%stri
      include
        [%m
          Helpers.apply_streamable_dot
            { loc; rpc; version }
            ~functor_name:"Remove_t"
            ~arguments:[ module_expr ]]]
    ]
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
         ~attributes:[ T Attributes.atomic ])
;;
