open! Core

module Foo = struct
  type t =
    { a : int
    ; b : string
    }
  [@@deriving bin_io, sexp]
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
