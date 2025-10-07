open! Base
open! Import

(** [Map_clause] matches types of the form:

    {[
      'a B.Map.t
    ]}
    {[
      (B.t, 'a, _) Map.t
    ]}

    It generates a module of the form:

    {[
      Streamable.Of_map (B) (<expansion of 'a>)
    ]}

    It also handles the [map_with_atomic_values] attribute, in which case it uses the
    [Streamable.Of_map_with_atomic_values] functor. *)
val maybe_match : Clause.t
