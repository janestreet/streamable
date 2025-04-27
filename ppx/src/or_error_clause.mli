(** [Or_error_clause] matches types of the form:

    {[
      'a Or_error.t
    ]}

    It generates a module of the form:

    {[
      Streamable.Of_result
        (<expansion of 'a>)
        (Streamable.Of_atomic (Core.Error.Stable.Vn.t))
    ]} *)
val maybe_match : Clause.t
