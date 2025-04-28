open! Core

(* Submodule form *)
include Test.Is_S (struct
    type t = int String.Table.t [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_hashtbl
           (String)
           (Streamable.Stable.V1.Of_atomic (Core.Int)))

    [@@@end]
  end)

(* Submodule form (RPC) *)
include Test.Is_S_rpc (struct
    type t = int String.Table.t [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_hashtbl_rpc
           (String)
           (Streamable.Stable.V1.Of_atomic_rpc (Core.Int)))

    [@@@end]
  end)

(* Stable submodule form *)
include Test.Is_S (struct
    type t = int String.Stable.V1.Table.t [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_hashtbl
           (String.Stable.V1)
           (Streamable.Stable.V1.Of_atomic (Core.Int)))

    [@@@end]
  end)

(* Stable submodule form (RPC) *)
include Test.Is_S_rpc (struct
    type t = int String.Stable.V1.Table.t [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_hashtbl_rpc
           (String.Stable.V1)
           (Streamable.Stable.V1.Of_atomic_rpc (Core.Int)))

    [@@@end]
  end)

(* Parameterized form *)
include Test.Is_S (struct
    type t = (string, int) Hashtbl.t [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_hashtbl
           (String)
           (Streamable.Stable.V1.Of_atomic (Core.Int)))

    [@@@end]
  end)

(* Parameterized form (RPC) *)
include Test.Is_S_rpc (struct
    type t = (string, int) Hashtbl.t [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_hashtbl_rpc
           (String)
           (Streamable.Stable.V1.Of_atomic_rpc (Core.Int)))

    [@@@end]
  end)

(* M-module form *)
include Test.Is_S (struct
    type t = int Hashtbl.M(String).t [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_hashtbl
           (String)
           (Streamable.Stable.V1.Of_atomic (Core.Int)))

    [@@@end]
  end)

(* M-module form (RPC) *)
include Test.Is_S_rpc (struct
    type t = int Hashtbl.M(String).t [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_hashtbl_rpc
           (String)
           (Streamable.Stable.V1.Of_atomic_rpc (Core.Int)))

    [@@@end]
  end)
