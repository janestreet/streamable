open! Core

(* Basic: primitive form *)
include Test.Is_S (struct
    type t = (int, unit) result [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_result
           (Streamable.Stable.V1.Of_atomic
              (Core.Int))
              (Streamable.Stable.V1.Of_atomic (Core.Unit)))

    [@@@end]
  end)

(* Basic: primitive form (RPC) *)
include Test.Is_S_rpc (struct
    type t = (int, unit) result [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_result_rpc
           (Streamable.Stable.V1.Of_atomic_rpc
              (Core.Int))
              (Streamable.Stable.V1.Of_atomic_rpc (Core.Unit)))

    [@@@end]
  end)

(* Nested: primitive form *)
include Test.Is_S (struct
    type t = ((int, bool) result, string) result [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_result
           (Streamable.Stable.V1.Of_result
              (Streamable.Stable.V1.Of_atomic
                 (Core.Int))
                 (Streamable.Stable.V1.Of_atomic (Core.Bool)))
              (Streamable.Stable.V1.Of_atomic (Core.String)))

    [@@@end]
  end)

(* Nested: primitive form (RPC) *)
include Test.Is_S_rpc (struct
    type t = ((int, bool) result, string) result
    [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_result_rpc
           (Streamable.Stable.V1.Of_result_rpc
              (Streamable.Stable.V1.Of_atomic_rpc
                 (Core.Int))
                 (Streamable.Stable.V1.Of_atomic_rpc (Core.Bool)))
              (Streamable.Stable.V1.Of_atomic_rpc (Core.String)))

    [@@@end]
  end)

(* Basic: module form *)
include Test.Is_S (struct
    type t = (int, unit) Result.t [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_result
           (Streamable.Stable.V1.Of_atomic
              (Core.Int))
              (Streamable.Stable.V1.Of_atomic (Core.Unit)))

    [@@@end]
  end)

(* Basic: module form (RPC) *)
include Test.Is_S_rpc (struct
    type t = (int, unit) Result.t [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_result_rpc
           (Streamable.Stable.V1.Of_atomic_rpc
              (Core.Int))
              (Streamable.Stable.V1.Of_atomic_rpc (Core.Unit)))

    [@@@end]
  end)

(* Stable: primitive form *)
include Test.Is_S (struct
    type t = (int, unit) Result.Stable.V1.t [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_result
           (Streamable.Stable.V1.Of_atomic
              (Core.Int))
              (Streamable.Stable.V1.Of_atomic (Core.Unit)))

    [@@@end]
  end)

(* Stable: primitive form (RPC) *)
include Test.Is_S_rpc (struct
    type t = (int, unit) Result.Stable.V1.t [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_result_rpc
           (Streamable.Stable.V1.Of_atomic_rpc
              (Core.Int))
              (Streamable.Stable.V1.Of_atomic_rpc (Core.Unit)))

    [@@@end]
  end)

(* Nested: module form *)
include Test.Is_S (struct
    type t = ((int, bool) Result.t, string) Result.t
    [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_result
           (Streamable.Stable.V1.Of_result
              (Streamable.Stable.V1.Of_atomic
                 (Core.Int))
                 (Streamable.Stable.V1.Of_atomic (Core.Bool)))
              (Streamable.Stable.V1.Of_atomic (Core.String)))

    [@@@end]
  end)

(* Nested: module form (RPC) *)
include Test.Is_S_rpc (struct
    type t = ((int, bool) Result.t, string) Result.t
    [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_result_rpc
           (Streamable.Stable.V1.Of_result_rpc
              (Streamable.Stable.V1.Of_atomic_rpc
                 (Core.Int))
                 (Streamable.Stable.V1.Of_atomic_rpc (Core.Bool)))
              (Streamable.Stable.V1.Of_atomic_rpc (Core.String)))

    [@@@end]
  end)
