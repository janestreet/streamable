(** [Sequence_clause] matches types of the form:

    {[
      'a Sequence.t
    ]}

    It generates a module of the form:

    {[
      Streamable.Of_sequence (<expansion of 'a>)
    ]} *)
val maybe_match : Clause.t
