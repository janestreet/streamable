open! Base
open! Import

module type X = sig
  module Submodule_form : sig
    (** The name of the submodule, if it were to appear in the "submodule" form. This
        should be capitalized.

        Examples: "Set" for [Foo.Set.t] and "Table" for ['data Foo.Table.t]. *)
    val name : string

    (** The number of type parameters expected, if it were to appear in the "submodule"
        form.

        Examples: 0 for [Foo.Set.t] and 1 for ['data Foo.Table.t]. *)
    val arity : int

    (** The value types that will be expanded recursively. You may assume that
        [type_parameters] has the expected [arity]. *)
    val value_types : type_parameters:core_type list -> core_type list
  end

  module Parameterized_form : sig
    (** The name of the module, if it were to appear in the "parameterized" form. This
        should be capitalized.

        Examples: "Set" for [('elt, 'cmp) Set.t] and "Hashtbl" for
        [('key, 'data) Hashtbl.t]. *)
    val name : string

    (** The number of type parameters expected, if it were to appear in the
        "parameterized" form.

        Examples: 2 for [('elt, 'cmp) Set.t] and 3 for [('key, 'data, 'cmp) Map.t]. *)
    val arity : int

    (** The type that is serialized atomically. You may assume that [type_parameters] has
        the expected [arity]. *)
    val key_type : type_parameters:core_type list -> core_type option

    (** The value types that will be expanded recursively. You may assume that
        [type_parameters] has the expected [arity]. *)
    val value_types : type_parameters:core_type list -> core_type list
  end

  module M_module_form : sig
    (** The name of the module, if it were to appear in the "M-module" form. This should
        be capitalized.

        Examples: "Set" for [Set.M(Foo).t] and "Hashtbl" for [Hashtbl.M(Foo).t]. *)
    val name : string

    (** The number of type parameters expected, if it were to appear in the "M-module"
        form.

        Examples: 0 for [Set.M(Foo).t] and 1 for [Hashtbl.M(Foo).t]. *)
    val arity : int

    (** The value types that will be expanded recursively. You may assume that
        [type_parameters] has the expected [arity]. *)
    val value_types : type_parameters:core_type list -> core_type list
  end
end

module type Keyed_container_clause = sig
  module Matcher : sig
    (** Generic matcher for keyed container types. Returns the key longident and value
        types. *)
    val match_keyed_container_type
      :  (module X)
      -> core_type:core_type
      -> (longident * core_type list) option
  end

  (** Generates a matcher that operates over types of the forms:

      {[
        (a1, ..., an)   K.%{Submodule_form.name}.t
      ]}
      (the "submodule" form)
      {[
        (k, a1, ..., an) %{Parameterized_form.name}.t
      ]}
      (the "parameterized" form)
      {[
        (a1, ..., an) %{M_module_form.name}.M(K).t
      ]}
      (the "M-module" form)

      in order to generate:

      {v
        Streamable.Of_%{String.lowercase Parameterized_form.name}
          (K)
          (<expansion of a1>)
          (...)
          (<expansion of an>)
      v}

      It is expected that values of type [K.t]/[k] can be serialized atomically. *)
  module Make (X : X) : sig
    val maybe_match : Clause.t
  end
end
