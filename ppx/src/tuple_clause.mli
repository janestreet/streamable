(** [Tuple_clause] matches types of the form:

    {[
      'a1 * ... * 'an
    ]}

    It generates a module of the form:

    {[
      Streamable.Of_tupleN
        (<expansion of 'a1>)
        (...)
        (<expansion of 'an>)
    ]} *)
val maybe_match : Clause.t
