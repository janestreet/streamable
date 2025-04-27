(** [Set_clause] matches types of the form:

    {[
      A.Set.t
    ]}
    {[
      (A.t, _) Set.t
    ]}

    It generates a module of the form:

    {[
      Streamable.Of_set A
    ]} *)
val maybe_match : Clause.t
