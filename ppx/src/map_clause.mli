(** [Map_clause] matches types of the form:

    {[ 'a B.Map.t ]}
    {[ (B.t, 'a, _) Map.t ]}

    It generates a module of the form:

    {[
      Streamable.Of_map (B) (<expansion of 'a>)
    ]}
*)
val maybe_match : Clause.t
