open! Core

(* Basic *)
include Test.Is_S (struct
    type t = int Or_error.t [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_result
           (Streamable.Stable.V1.Of_atomic
              (Core.Int))
              (Streamable.Stable.V1.Of_atomic (Core.Error.Stable.V2)))

    [@@@end]
  end)

(* Basic (RPC) *)
include Test.Is_S_rpc (struct
    type t = int Or_error.t [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_result_rpc
           (Streamable.Stable.V1.Of_atomic_rpc
              (Core.Int))
              (Streamable.Stable.V1.Of_atomic_rpc (Core.Error.Stable.V2)))

    [@@@end]
  end)

(* Stable *)
include Test.Is_S (struct
    type t = int Or_error.Stable.V2.t [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_result
           (Streamable.Stable.V1.Of_atomic
              (Core.Int))
              (Streamable.Stable.V1.Of_atomic (Core.Error.Stable.V2)))

    [@@@end]
  end)

(* Stable (RPC) *)
include Test.Is_S (struct
    type t = int Or_error.Stable.V2.t [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_result
           (Streamable.Stable.V1.Of_atomic
              (Core.Int))
              (Streamable.Stable.V1.Of_atomic (Core.Error.Stable.V2)))

    [@@@end]
  end)

(* Nested *)
include Test.Is_S (struct
    type t = int Or_error.t Or_error.t [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_result
           (Streamable.Stable.V1.Of_result
              (Streamable.Stable.V1.Of_atomic
                 (Core.Int))
                 (Streamable.Stable.V1.Of_atomic (Core.Error.Stable.V2)))
              (Streamable.Stable.V1.Of_atomic (Core.Error.Stable.V2)))

    [@@@end]
  end)

(* Nested (RPC) *)
include Test.Is_S_rpc (struct
    type t = int Or_error.t Or_error.t [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_result_rpc
           (Streamable.Stable.V1.Of_result_rpc
              (Streamable.Stable.V1.Of_atomic_rpc
                 (Core.Int))
                 (Streamable.Stable.V1.Of_atomic_rpc (Core.Error.Stable.V2)))
              (Streamable.Stable.V1.Of_atomic_rpc (Core.Error.Stable.V2)))

    [@@@end]
  end)
