open! Core

(* Submodule form *)
include Test.Is_S (struct
    type t = int String.Map.t [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_map (String) (Streamable.Stable.V1.Of_atomic (Core.Int)))

    [@@@end]
  end)

(* Submodule form (RPC) *)
include Test.Is_S_rpc (struct
    type t = int String.Map.t [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_map_rpc
           (String)
           (Streamable.Stable.V1.Of_atomic_rpc (Core.Int)))

    [@@@end]
  end)

(* Stable submodule form *)
include Test.Is_S (struct
    type t = int String.Stable.V1.Map.t [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_map
           (String.Stable.V1)
           (Streamable.Stable.V1.Of_atomic (Core.Int)))

    [@@@end]
  end)

(* Stable submodule form (RPC) *)
include Test.Is_S_rpc (struct
    type t = int String.Stable.V1.Map.t [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_map_rpc
           (String.Stable.V1)
           (Streamable.Stable.V1.Of_atomic_rpc (Core.Int)))

    [@@@end]
  end)

(* Parameterized form *)
include Test.Is_S (struct
    type t = (String.t, int, String.comparator_witness) Map.t
    [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_map (String) (Streamable.Stable.V1.Of_atomic (Core.Int)))

    [@@@end]
  end)

(* Parameterized form (RPC) *)
include Test.Is_S_rpc (struct
    type t = (string, int, String.comparator_witness) Map.t
    [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_map_rpc
           (String)
           (Streamable.Stable.V1.Of_atomic_rpc (Core.Int)))

    [@@@end]
  end)

(* Stable parameterized form *)
include Test.Is_S (struct
    type t = (String.Stable.V1.t, int, String.comparator_witness) Map.Stable.V1.t
    [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_map
           (String.Stable.V1)
           (Streamable.Stable.V1.Of_atomic (Core.Int)))

    [@@@end]
  end)

(* Stable parameterized form (RPC) *)
include Test.Is_S_rpc (struct
    type t = (string, int, String.comparator_witness) Map.Stable.V1.t
    [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_map_rpc
           (String)
           (Streamable.Stable.V1.Of_atomic_rpc (Core.Int)))

    [@@@end]
  end)

(* M-module form *)
include Test.Is_S (struct
    type t = int Map.M(Int).t [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_map (Int) (Streamable.Stable.V1.Of_atomic (Core.Int)))

    [@@@end]
  end)

(* M-module form (RPC) *)
include Test.Is_S_rpc (struct
    type t = int Map.M(Int).t [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_map_rpc
           (Int)
           (Streamable.Stable.V1.Of_atomic_rpc (Core.Int)))

    [@@@end]
  end)

(* Stable M-module form *)
include Test.Is_S (struct
    type t = int Map.M(Int.Stable.V1).t [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_map
           (Int.Stable.V1)
           (Streamable.Stable.V1.Of_atomic (Core.Int)))

    [@@@end]
  end)

(* Stable M-module form (RPC) *)
include Test.Is_S_rpc (struct
    type t = int Map.M(Int.Stable.V1).t [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_map_rpc
           (Int.Stable.V1)
           (Streamable.Stable.V1.Of_atomic_rpc (Core.Int)))

    [@@@end]
  end)

(* Nested *)
include Test.Is_S (struct
    type t = bool Int.Map.t String.Map.t [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_map
           (String)
           (Streamable.Stable.V1.Of_map (Int) (Streamable.Stable.V1.Of_atomic (Core.Bool))))

    [@@@end]
  end)

(* Nested (RPC) *)
include Test.Is_S_rpc (struct
    type t = bool Int.Map.t String.Map.t [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_map_rpc
           (String)
           (Streamable.Stable.V1.Of_map_rpc
              (Int)
              (Streamable.Stable.V1.Of_atomic_rpc (Core.Bool))))

    [@@@end]
  end)
