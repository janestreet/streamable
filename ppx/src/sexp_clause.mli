(** [Sexpable_clause] matches types of the form:

    {[
      Sexp.t
    ]}

    It generates a module of the form:

    {[
      Streamable.Of_sexpable Sexp
    ]} *)
val maybe_match : Clause.t
