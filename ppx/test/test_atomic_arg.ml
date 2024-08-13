open! Core

(* Basic *)
include Test.Is_S (struct
    type t = int * string
    [@@deriving bin_io, sexp] [@@deriving_inline streamable ~atomic ~version:1]

    include Streamable.Stable.V1.Remove_t (Streamable.Stable.V1.Of_atomic (struct
        type nonrec t = t [@@deriving bin_io, sexp]
      end))

    [@@@end]

    (* In the jbuild, we pass in [-pretty] so that we know if our ppx generates anything
       that isn't being used, but that also means we get warnings if other ppxes generate
       anything that isn't being used. This manually suppresses the unused [bin_t]. *)
    let _ = bin_t
  end)

(* Basic (RPC) *)
include Test.Is_S_rpc (struct
    type t = int * string
    [@@deriving bin_io] [@@deriving_inline streamable ~atomic ~rpc ~version:1]

    include Streamable.Stable.V1.Remove_t_rpc (Streamable.Stable.V1.Of_atomic_rpc (struct
        type nonrec t = t [@@deriving bin_io]
      end))

    [@@@end]

    let _ = bin_t
  end)
