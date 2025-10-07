open! Base
open! Import
include Keyed_container_clause_intf

module Matcher = struct
  let assert_arity_is_expected ~longident_loc ~actual_arity ~expected_arity =
    if actual_arity <> expected_arity
    then
      Location.raise_errorf
        ~loc:longident_loc.loc
        "The arity of `%s' was expected to be %d but was found to be %d"
        (Longident.name longident_loc.txt)
        expected_arity
        actual_arity
  ;;

  let match_keyed_container_type (module X : X) ~core_type =
    let match_on_submodule_form ~core_type =
      match core_type.ptyp_desc with
      | Ptyp_constr (longident_loc, type_parameters) ->
        (match Helpers.if_module_dot_t_then_module core_type with
         | None -> None
         | Some module_longident_loc ->
           (match%bind Helpers.split_longident module_longident_loc with
            | `prefix (Some prefix), `last last when String.(last = X.Submodule_form.name)
              ->
              assert_arity_is_expected
                ~longident_loc
                ~actual_arity:(List.length type_parameters)
                ~expected_arity:X.Submodule_form.arity;
              Some (prefix, X.Submodule_form.value_types ~type_parameters)
            | _ -> None))
      | _ -> None
    in
    let match_on_parameterized_form ~core_type =
      match core_type.ptyp_desc with
      | Ptyp_constr (longident_loc, type_parameters) ->
        (match
           Helpers.longident_is_like_t
             longident_loc
             ~primitive_name:None
             ~first_module_name:X.Parameterized_form.name
         with
         | false -> None
         | true ->
           assert_arity_is_expected
             ~longident_loc
             ~actual_arity:(List.length type_parameters)
             ~expected_arity:X.Parameterized_form.arity;
           let%bind atomic_type = X.Parameterized_form.key_type ~type_parameters in
           let children_types = X.Parameterized_form.value_types ~type_parameters in
           let atomic_longident =
             match Helpers.if_module_dot_t_then_module atomic_type with
             | Some longident_loc -> longident_loc.txt
             | None -> lident (String.capitalize (string_of_core_type atomic_type))
           in
           Some (atomic_longident, children_types))
      | _ -> None
    in
    let match_on_m_module_form ~core_type =
      match core_type.ptyp_desc with
      | Ptyp_constr (longident_loc, type_parameters) ->
        let%bind key_longident =
          Helpers.if_module_dot_m_then_arg
            longident_loc.txt
            ~module_name:X.M_module_form.name
        in
        let children_types = X.M_module_form.value_types ~type_parameters in
        Some (key_longident, children_types)
      | _ -> None
    in
    [ match_on_submodule_form ~core_type
    ; match_on_parameterized_form ~core_type
    ; match_on_m_module_form ~core_type
    ]
    |> List.find_map ~f:Fn.id
  ;;
end

module Make (X : X) = struct
  let maybe_match type_ (_ : Ctx.t) =
    let%bind core_type = Type_.match_core_type type_ in
    let%map atomic_longident, children_types =
      Matcher.match_keyed_container_type (module X) ~core_type
    in
    ({ children = List.map children_types ~f:Type_.core_type
     ; apply_functor =
         (fun ctx children ->
           let loc = ctx.loc in
           Helpers.apply_streamable_dot
             ctx
             ~functor_name:[%string "Of_%{String.lowercase X.Parameterized_form.name}"]
             ~arguments:(pmod_ident ~loc (Loc.make ~loc atomic_longident) :: children))
     }
     : Clause.Match.t)
  ;;
end
