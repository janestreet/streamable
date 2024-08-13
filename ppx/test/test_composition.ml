open! Core

module Foo = struct
  type t =
    { a : int
    ; b : string
    }
  [@@deriving streamable ~version:1]
end

(* Record nesting *)
include Test.Is_S (struct
    type t =
      { foo : Foo.t
      ; qux : bool
      }
    [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_streamable
           (Streamable.Stable.V1.Of_tuple2
              (Foo)
              (Streamable.Stable.V1.Of_atomic (Core.Bool)))
              (struct
                type nonrec t = t

                let to_streamable { foo; qux } = foo, qux
                let of_streamable (foo, qux) = { foo; qux }
              end))

    [@@@end]
  end)
