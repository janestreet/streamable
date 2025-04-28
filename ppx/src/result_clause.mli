(** [Result_clause] matches types of the forms:

    {[
      ('a, 'b) result
    ]}
    {[
      ('a, 'b) Result.t
    ]}

    It generates a module of the form:

    {[
      Streamable.Of_result
        (<expansion of 'a>)
        (<expansion of 'b>)
    ]} *)
val maybe_match : Clause.t
