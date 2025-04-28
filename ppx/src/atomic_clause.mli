(** {v
 [Atomic_clause] matches types of the form:

    {[ (%{module_path}.t [@streamable.atomic]) ]}

    where %{module_path} contains one or more (capitalized) module names separated by
    dots.

    It generates a module of the form:

    {[
      Streamable.Of_atomic (%{module_path})
    ]}
    v} *)
val maybe_match : Clause.t
