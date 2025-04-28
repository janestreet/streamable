(** [Type_parameter_clause] matches types of the form:

    {[
      '%{type_parameter}
    ]}

    where %[{type_parameter}] is some valid, bound type parameter.

    It generates a module of the form:

    {[
      %{Helpers.module_name_for_type_parameter (`Ptyp_var type_parameter)}
    ]} *)
val maybe_match : Clause.t
