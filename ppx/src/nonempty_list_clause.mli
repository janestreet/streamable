(** [Nonempty_list_clause] matches types of the form:

    {[
      'a Nonempty_list.t
    ]}

    It generates a module of the form:

    {[
      Streamable.Of_nonempty_list (<expansion of 'a>)
    ]} *)
val maybe_match : Clause.t
