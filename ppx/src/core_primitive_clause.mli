(** [Core_primitive_clause] matches types of the form:

    {[
      %{primitive}
    ]}

    where %[{primitive}] is a zero-arity, single [Lident] type constructor (without any
    dots) that is exported by [Core].

    It generates a module of the form:

    {[
      Streamable.Of_atomic (Core).%{String.capitalize primitive}
    ]}

    for example: [int] becomes [Streamable.Of_atomic (Core.Int)] *)
val maybe_match : Clause.t
