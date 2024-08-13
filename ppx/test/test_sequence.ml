open! Core

(* Basic *)
include Test.Is_S (struct
    type t = int Sequence.t [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_sequence (Streamable.Stable.V1.Of_atomic (Core.Int)))

    [@@@end]
  end)

(* Basic (RPC) *)
include Test.Is_S_rpc (struct
    type t = int Sequence.t [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_sequence_rpc
           (Streamable.Stable.V1.Of_atomic_rpc (Core.Int)))

    [@@@end]
  end)

(* Nested *)
include Test.Is_S_rpc (struct
    type t = string Sequence.t Sequence.t [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_sequence
           (Streamable.Stable.V1.Of_sequence (Streamable.Stable.V1.Of_atomic (Core.String))))

    [@@@end]
  end)

(* Nested (RPC) *)
include Test.Is_S_rpc (struct
    type t = string Sequence.t Sequence.t [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_sequence_rpc
           (Streamable.Stable.V1.Of_sequence_rpc
              (Streamable.Stable.V1.Of_atomic_rpc (Core.String))))

    [@@@end]
  end)
