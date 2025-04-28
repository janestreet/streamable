(** [List_clause] matches types of the forms:

    {[
      'a list
    ]}
    {[
      'a List.t
    ]}

    It generates a module of the form:

    {[
      Streamable.Of_list (<expansion of 'a>)
    ]} *)
val maybe_match : Clause.t
