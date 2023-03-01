open! Core

(* Basic *)
include Test.Is_S (struct
    type t = int Fqueue.t [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_fqueue (Streamable.Stable.V1.Of_atomic (Core.Int)))

    [@@@end]
  end)

(* Basic (RPC) *)
include Test.Is_S_rpc (struct
    type t = int Fqueue.t [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_fqueue_rpc (Streamable.Stable.V1.Of_atomic_rpc (Core.Int)))

    [@@@end]
  end)

(* Stable *)
include Test.Is_S (struct
    type t = int Fqueue.Stable.V1.t [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_fqueue (Streamable.Stable.V1.Of_atomic (Core.Int)))

    [@@@end]
  end)

(* Stable (RPC) *)
include Test.Is_S (struct
    type t = int Fqueue.Stable.V1.t [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_fqueue (Streamable.Stable.V1.Of_atomic (Core.Int)))

    [@@@end]
  end)

(* Nested *)
include Test.Is_S (struct
    type t = string Fqueue.t Fqueue.t [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_fqueue
           (Streamable.Stable.V1.Of_fqueue (Streamable.Stable.V1.Of_atomic (Core.String))))

    [@@@end]
  end)

(* Nested (RPC) *)
include Test.Is_S_rpc (struct
    type t = string Fqueue.t Fqueue.t [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_fqueue_rpc
           (Streamable.Stable.V1.Of_fqueue_rpc
              (Streamable.Stable.V1.Of_atomic_rpc (Core.String))))

    [@@@end]
  end)
