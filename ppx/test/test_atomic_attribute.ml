open! Core

module Foo = struct
  module T = struct
    type t =
      { a : int
      ; b : string
      }
    [@@deriving bin_io, compare, sexp]
  end

  include T
  include Comparable.Make_binable (T)
end

(* Basic *)
include Test.Is_S (struct
    type t = (Foo.t[@streamable.atomic]) [@@deriving_inline streamable ~version:1]

    include Streamable.Stable.V1.Remove_t (Streamable.Stable.V1.Of_atomic (Foo))

    [@@@end]
  end)

(* Basic (RPC) *)
include Test.Is_S_rpc (struct
    type t = (Foo.t[@streamable.atomic]) [@@deriving_inline streamable ~rpc ~version:1]

    include Streamable.Stable.V1.Remove_t_rpc (Streamable.Stable.V1.Of_atomic_rpc (Foo))

    [@@@end]
  end)

(* Compound *)
include Test.Is_S (struct
    type t = { a : (Foo.Set.t[@streamable.atomic]) }
    [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_streamable
           (Streamable.Stable.V1.Of_atomic
              (Foo.Set))
              (struct
                type nonrec t = t

                let to_streamable { a } = a
                let of_streamable a = { a }
              end))

    [@@@end]
  end)

(* Compound (RPC) *)
include Test.Is_S_rpc (struct
    type t = { a : (Foo.Set.t[@streamable.atomic]) }
    [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_streamable_rpc
           (Streamable.Stable.V1.Of_atomic_rpc
              (Foo.Set))
              (struct
                type nonrec t = t

                let to_streamable { a } = a
                let of_streamable a = { a }
              end))

    [@@@end]
  end)
