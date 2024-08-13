(* This file intentionally does not [open! Core], in order to verify that the [Core]
   prefix is being correctly added. *)

(* bool *)
include Test.Is_S (struct
    type t = bool [@@deriving_inline streamable ~version:1]

    include Streamable.Stable.V1.Remove_t (Streamable.Stable.V1.Of_atomic (Core.Bool))

    [@@@end]
  end)

(* bool (RPC) *)
include Test.Is_S_rpc (struct
    type t = bool [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc (Streamable.Stable.V1.Of_atomic_rpc (Core.Bool))

    [@@@end]
  end)

(* bytes *)
include Test.Is_S (struct
    type t = bytes [@@deriving_inline streamable ~version:1]

    include Streamable.Stable.V1.Remove_t (Streamable.Stable.V1.Of_atomic (Core.Bytes))

    [@@@end]
  end)

(* bytes (RPC) *)
include Test.Is_S_rpc (struct
    type t = bytes [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc (Streamable.Stable.V1.Of_atomic_rpc (Core.Bytes))

    [@@@end]
  end)

(* char *)
include Test.Is_S (struct
    type t = char [@@deriving_inline streamable ~version:1]

    include Streamable.Stable.V1.Remove_t (Streamable.Stable.V1.Of_atomic (Core.Char))

    [@@@end]
  end)

(* char (RPC) *)
include Test.Is_S_rpc (struct
    type t = char [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc (Streamable.Stable.V1.Of_atomic_rpc (Core.Char))

    [@@@end]
  end)

(* float *)
include Test.Is_S (struct
    type t = float [@@deriving_inline streamable ~version:1]

    include Streamable.Stable.V1.Remove_t (Streamable.Stable.V1.Of_atomic (Core.Float))

    [@@@end]
  end)

(* float (RPC) *)
include Test.Is_S_rpc (struct
    type t = float [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc (Streamable.Stable.V1.Of_atomic_rpc (Core.Float))

    [@@@end]
  end)

(* int *)
include Test.Is_S (struct
    type t = int [@@deriving_inline streamable ~version:1]

    include Streamable.Stable.V1.Remove_t (Streamable.Stable.V1.Of_atomic (Core.Int))

    [@@@end]
  end)

(* int (RPC) *)
include Test.Is_S_rpc (struct
    type t = int [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc (Streamable.Stable.V1.Of_atomic_rpc (Core.Int))

    [@@@end]
  end)

(* int32 *)
include Test.Is_S (struct
    type t = int32 [@@deriving_inline streamable ~version:1]

    include Streamable.Stable.V1.Remove_t (Streamable.Stable.V1.Of_atomic (Core.Int32))

    [@@@end]
  end)

(* int32 (RPC) *)
include Test.Is_S_rpc (struct
    type t = int32 [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc (Streamable.Stable.V1.Of_atomic_rpc (Core.Int32))

    [@@@end]
  end)

(* int64 *)
include Test.Is_S (struct
    type t = int64 [@@deriving_inline streamable ~version:1]

    include Streamable.Stable.V1.Remove_t (Streamable.Stable.V1.Of_atomic (Core.Int64))

    [@@@end]
  end)

(* int64 (RPC) *)
include Test.Is_S_rpc (struct
    type t = int64 [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc (Streamable.Stable.V1.Of_atomic_rpc (Core.Int64))

    [@@@end]
  end)

(* nativeint *)
include Test.Is_S (struct
    type t = nativeint [@@deriving_inline streamable ~version:1]

    include Streamable.Stable.V1.Remove_t (Streamable.Stable.V1.Of_atomic (Core.Nativeint))

    [@@@end]
  end)

(* nativeint (RPC) *)
include Test.Is_S_rpc (struct
    type t = nativeint [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_atomic_rpc (Core.Nativeint))

    [@@@end]
  end)

(* string *)
include Test.Is_S (struct
    type t = string [@@deriving_inline streamable ~version:1]

    include Streamable.Stable.V1.Remove_t (Streamable.Stable.V1.Of_atomic (Core.String))

    [@@@end]
  end)

(* string (RPC) *)
include Test.Is_S_rpc (struct
    type t = string [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc (Streamable.Stable.V1.Of_atomic_rpc (Core.String))

    [@@@end]
  end)

(* unit *)
include Test.Is_S (struct
    type t = unit [@@deriving_inline streamable ~version:1]

    include Streamable.Stable.V1.Remove_t (Streamable.Stable.V1.Of_atomic (Core.Unit))

    [@@@end]
  end)

(* unit (RPC) *)
include Test.Is_S_rpc (struct
    type t = unit [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc (Streamable.Stable.V1.Of_atomic_rpc (Core.Unit))

    [@@@end]
  end)
