open! Core

(* Basic: module form *)
include Test.Is_S (struct
    type t = int Nonempty_list.t [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_nonempty_list (Streamable.Stable.V1.Of_atomic (Core.Int)))

    [@@@end]
  end)

(* Basic: module form (RPC) *)
include Test.Is_S_rpc (struct
    type t = int Nonempty_list.t [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_nonempty_list_rpc
           (Streamable.Stable.V1.Of_atomic_rpc (Core.Int)))

    [@@@end]
  end)

(* Stable: module form *)
include Test.Is_S (struct
    type t = int Nonempty_list.Stable.V3.t [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_nonempty_list (Streamable.Stable.V1.Of_atomic (Core.Int)))

    [@@@end]
  end)

(* Stable: module form (RPC) *)
include Test.Is_S_rpc (struct
    type t = int Nonempty_list.Stable.V3.t [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_nonempty_list_rpc
           (Streamable.Stable.V1.Of_atomic_rpc (Core.Int)))

    [@@@end]
  end)

(* Nested: module form *)
include Test.Is_S (struct
    type t = string Nonempty_list.t Nonempty_list.t
    [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_nonempty_list
           (Streamable.Stable.V1.Of_nonempty_list
              (Streamable.Stable.V1.Of_atomic (Core.String))))

    [@@@end]
  end)

(* Nested: module form (RPC) *)
include Test.Is_S_rpc (struct
    type t = string Nonempty_list.t Nonempty_list.t
    [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_nonempty_list_rpc
           (Streamable.Stable.V1.Of_nonempty_list_rpc
              (Streamable.Stable.V1.Of_atomic_rpc (Core.String))))

    [@@@end]
  end)
