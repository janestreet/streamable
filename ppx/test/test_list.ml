open! Core

(* Basic: primitive form *)
include Test.Is_S (struct
    type t = int list [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_list (Streamable.Stable.V1.Of_atomic (Core.Int)))

    [@@@end]
  end)

(* Basic: primitive form (RPC) *)
include Test.Is_S_rpc (struct
    type t = int list [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_list_rpc (Streamable.Stable.V1.Of_atomic_rpc (Core.Int)))

    [@@@end]
  end)

(* Nested: primitive form *)
include Test.Is_S (struct
    type t = string list list [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_list
           (Streamable.Stable.V1.Of_list (Streamable.Stable.V1.Of_atomic (Core.String))))

    [@@@end]
  end)

(* Nested: primitive form (RPC) *)
include Test.Is_S_rpc (struct
    type t = string list list [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_list_rpc
           (Streamable.Stable.V1.Of_list_rpc
              (Streamable.Stable.V1.Of_atomic_rpc (Core.String))))

    [@@@end]
  end)

(* Basic: module form *)
include Test.Is_S (struct
    type t = int List.t [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_list (Streamable.Stable.V1.Of_atomic (Core.Int)))

    [@@@end]
  end)

(* Basic: module form (RPC) *)
include Test.Is_S_rpc (struct
    type t = int List.t [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_list_rpc (Streamable.Stable.V1.Of_atomic_rpc (Core.Int)))

    [@@@end]
  end)

(* Stable: module form *)
include Test.Is_S (struct
    type t = int List.Stable.V1.t [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_list (Streamable.Stable.V1.Of_atomic (Core.Int)))

    [@@@end]
  end)

(* Stable: module form (RPC) *)
include Test.Is_S_rpc (struct
    type t = int List.Stable.V1.t [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_list_rpc (Streamable.Stable.V1.Of_atomic_rpc (Core.Int)))

    [@@@end]
  end)

(* Nested: module form *)
include Test.Is_S (struct
    type t = string List.t List.t [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_list
           (Streamable.Stable.V1.Of_list (Streamable.Stable.V1.Of_atomic (Core.String))))

    [@@@end]
  end)

(* Nested: module form (RPC) *)
include Test.Is_S_rpc (struct
    type t = string List.t List.t [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_list_rpc
           (Streamable.Stable.V1.Of_list_rpc
              (Streamable.Stable.V1.Of_atomic_rpc (Core.String))))

    [@@@end]
  end)
