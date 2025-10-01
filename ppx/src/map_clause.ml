open! Base
open! Import

module Map_config = struct
  module Submodule_form = struct
    let name = "Map"
    let arity = 1
    let value_types ~type_parameters = [ List.nth_exn type_parameters 0 ]
  end

  module Parameterized_form = struct
    let name = "Map"
    let arity = 3
    let key_type ~type_parameters = Some (List.nth_exn type_parameters 0)
    let value_types ~type_parameters = [ List.nth_exn type_parameters 1 ]
  end

  module M_module_form = struct
    let name = "Map"
    let arity = 1
    let value_types ~type_parameters = [ List.nth_exn type_parameters 0 ]
  end
end

include Keyed_container_clause.Make (Map_config)

let match_map_type ~core_type =
  match
    Keyed_container_clause.Matcher.match_keyed_container_type
      (module Map_config)
      ~core_type
  with
  | Some (key, [ value ]) -> Some (key, value)
  | Some (_, _) ->
    Location.raise_errorf !"BUG: [Map] types should always have exactly one value type."
  | None -> None
;;

let maybe_match_map_with_atomic_values type_ =
  let%bind core_type = Type_.match_core_type type_ in
  let%bind key_longident, value_type = match_map_type ~core_type in
  let%map () = Attribute.get Attributes.map_with_atomic_values core_type in
  ({ children = [] (* Don't recursively expand since values are atomic. *)
   ; apply_functor =
       (fun ctx _children ->
         let loc = ctx.loc in
         let key_module = pmod_ident ~loc (Loc.make ~loc key_longident) in
         let value_module =
           match Helpers.if_module_dot_t_then_module value_type, value_type.ptyp_desc with
           | Some module_longident_loc, Ptyp_constr (_, []) ->
             (* Value type is [Module.t] form with no type parameters *)
             pmod_ident ~loc module_longident_loc
           | _ ->
             let deriving_expression =
               if ctx.rpc then [%expr bin_io] else [%expr bin_io, sexp]
             in
             let type_t_with_deriving =
               [%stri type t = [%t value_type] [@@deriving [%e deriving_expression]]]
             in
             pmod_structure ~loc [ type_t_with_deriving ]
         in
         Helpers.apply_streamable_dot
           ctx
           ~functor_name:"Of_map_with_atomic_values"
           ~arguments:[ key_module; value_module ])
   }
   : Clause.Match.t)
;;

let maybe_match type_ ctx =
  Option.first_some (maybe_match_map_with_atomic_values type_) (maybe_match type_ ctx)
;;
