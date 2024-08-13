open! Core

(* Basic *)
include Test.Is_S (struct
    type t = Sexp.t [@@deriving_inline streamable ~version:1]

    include Streamable.Stable.V1.Remove_t (Streamable.Stable.V1.Of_sexpable (Sexp))

    [@@@end]
  end)

(* Basic (RPC) *)
include Test.Is_S_rpc (struct
    type t = Sexp.t [@@deriving_inline streamable ~version:1 ~rpc]

    include Streamable.Stable.V1.Remove_t_rpc (Streamable.Stable.V1.Of_sexpable (Sexp))

    [@@@end]
  end)

(* Stable *)
include Test.Is_S (struct
    type t = Sexp.Stable.V1.t [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t (Streamable.Stable.V1.Of_sexpable (Sexp.Stable.V1))

    [@@@end]
  end)

(* Stable (RPC) *)
include Test.Is_S_rpc (struct
    type t = Sexp.Stable.V1.t [@@deriving_inline streamable ~version:1 ~rpc]

    include
      Streamable.Stable.V1.Remove_t_rpc (Streamable.Stable.V1.Of_sexpable (Sexp.Stable.V1))

    [@@@end]
  end)
