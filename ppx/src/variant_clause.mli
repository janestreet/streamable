(** [Variant_clause] matches types of the form:

    {[
      | A1 of 'a1
      | (...)
      | An of 'an
    ]}

    It generates a module of the form:

    {[
      Streamable.Of_streamable
        (Streamable.Of_variantN
           (<expansion of 'a1>)
           (...)
           (<expansion of 'an>))
        (struct
          type nonrec t = t

          let to_streamable = (...)
          let of_streamable = (...)
        end)
    ]}

    As a special case, if the variant only contains one constructor, the
    [Streamable.Of_variantN] call is ommitted and the variant is directly converted to and
    from the singleton constructor's type.

    Further, if a constructor has no argument, then [Core.Unit] is used as the expansion
    of that constructor. *)
val maybe_match : Clause.t
