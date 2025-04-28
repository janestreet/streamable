(** [Module_dot_t_clause] matches types of the form:

    {[
      %{module}.t
    ]}

    where %[{module}] consists of one or more dot-separated module names and there are no
    type parameters.

    It generates a module of the form:

    {[
      %{module}
    ]}

    for example: [Foo.Bar.t] becomes [Foo.Bar]. *)
val maybe_match : Clause.t
