(** [Record_clause] matches types of the form:

    {[
      { a1 : 'a1
      ; (...)
      ; an : 'an
      }
    ]}

    It generates a module of the form:

    {[
      Streamable.Of_streamable
        (Streamable.Of_tupleN
           (<expansion of 'a1>)
           (...)
           (<expansion of 'an>))
        (struct
          type nonrec t = t

          let to_streamable = (...)
          let of_streamable = (...)
        end)
    ]}

    As a special case, if the record only contains one field, the [Streamable.Of_tupleN]
    call is ommitted and the record is directly converted to and from the singleton
    field's type. *)
val maybe_match : Clause.t
