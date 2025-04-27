(** [Total_map_clause] matches types of the form:

    {[
      'a B.Total_map.t
    ]}
    {[
      (B.t, 'a, _, _) Total_map.t
    ]}

    It generates a module of the form:

    {[
      Streamable.Of_total_map (B) (<expansion of 'a>)
    ]} *)
val maybe_match : Clause.t
