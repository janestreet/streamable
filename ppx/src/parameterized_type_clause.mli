(** [Parameterized_type_clause] matches types of the form:

    {[
      (a1, ..., an) %{module_path}.t
    ]}

    where %[{module_path}] contains one or more (capitalized) module names separated by
    dots.

    It generates a module of the form:

    {[
      %{module_path}.Make_streamable
         (<expansion of a1>)
         (...)
         (<expansion of an>)
    ]} *)
val maybe_match : Clause.t
