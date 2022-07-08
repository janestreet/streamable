open! Base
open! Import

(** Raises an error related to unsupported use of this ppx. *)
val unsupported_use : loc:location -> why:string -> 'a

(** pattern variable *)
val pat_var         : loc:location -> label      -> pattern

(** expression variable *)
val exp_var         : loc:location -> label      -> expression

(** Gets the single [type t] declaration, and checks that it is valid.
    If it is not valid, it raises [unsupported_use]. *)
val get_the_one_and_only_type_t
  :  type_declaration list
  -> loc:location
  -> type_declaration

(** Generates a [module_expr] of the form:
    {[
      Streamable.Stable.V%{version}.%{functor_name} %{arguments}
    ]}

    If [rpc] is true, then [_rpc] is appended to [functor_name].
*)
val apply_streamable_dot
  :  Ctx.t
  -> functor_name:label
  -> arguments:module_expr list
  -> module_expr

(** Returns %{module_name} if [core_type] is of the form %{module_name}.t. *)
val if_module_dot_t_then_module : core_type -> longident loc option

(** Determines whether the [longident] is either like %{primitive_name} (if specified), or
    %{module_name}.t. *)
val longident_is_like_t
  :  longident
  -> primitive_name:label option
  -> module_name:label
  -> bool

(** Generates a [core_type] representing [module_dot_t], with the atomic attribute
    attached. This is useful when we want to force a particular type to be passed in as a
    child to one of the [Streamable] functors. *)
val core_type_with_atomic_attribute : loc:location -> module_dot_t:label -> core_type

(** A call to [Streamable.Of_streamable]. *)
val streamable_of_streamable
  :  ?type_t:structure_item
  -> Ctx.t
  -> streamable_module:module_expr
  -> to_streamable_fun:expression
  -> of_streamable_fun:expression
  -> module_expr

(** Used in generic patterns.  0 -> a, 1 -> b, ..., 27 -> a1, ...  *)
val lowercase_name_of_num : int -> label

(** Used in generic patterns.  0 -> A, 1 -> B, ..., 27 -> A1, ...  *)
val uppercase_name_of_num : int -> label

(** Code common to handling record/variant clauses. *)
val type_declaration_match
  :  Type.t
  -> payload:(type_declaration -> 'a option)
  -> streamable_module:(Ctx.t -> (core_type * module_expr) list -> module_expr)
  -> to_streamable_fun:(loc:location -> payload:'a -> expression)
  -> of_streamable_fun:(loc:location -> payload:'a -> expression)
  -> children:(loc:location -> payload:'a -> core_type list)
  -> Clause.Match.t option

(** Handles clauses of the form:

    {[ (a1, ..., an) %{primitive_name} ]}
    {[ (a1, ..., an) %{module_name}.t ]}

    in order to generate:

    {v
        Streamable.Of_%{String.lowercase module_name}
          (<expansion of a1>)
          (...)
          (<expansion of an>)
    v}
*)
val polymorphic_primitive_or_module_match
  :  num_type_parameters:int
  -> primitive_name:string option
  -> module_name:string
  -> Clause.t
