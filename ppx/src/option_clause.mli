(** [Option_clause] matches types of the forms:

    {[
      'a option
    ]}
    {[
      'a Option.t
    ]}

    It generates a module of the form:

    {[
      Streamable.Of_option (<expansion of 'a>)
    ]} *)
val maybe_match : Clause.t
