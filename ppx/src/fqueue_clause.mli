(** [Fqueue_clause] matches types of the form:

    {[
      'a Fqueue.t
    ]}

    It generates a module of the form:

    {[
      Streamable.Of_fqueue (<expansion of 'a>)
    ]} *)
val maybe_match : Clause.t
