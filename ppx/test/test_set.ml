open! Core

(* Submodule form *)
include Test.Is_S (struct
    type t = String.Set.t [@@deriving_inline streamable ~version:1]

    include Streamable.Stable.V1.Remove_t (Streamable.Stable.V1.Of_set (String))

    [@@@end]
  end)

(* Submodule form (RPC) *)
include Test.Is_S_rpc (struct
    type t = String.Set.t [@@deriving_inline streamable ~rpc ~version:1]

    include Streamable.Stable.V1.Remove_t_rpc (Streamable.Stable.V1.Of_set_rpc (String))

    [@@@end]
  end)

(* Parameterized form *)
include Test.Is_S (struct
    type t = (string, String.comparator_witness) Set.t
    [@@deriving_inline streamable ~version:1]

    include Streamable.Stable.V1.Remove_t (Streamable.Stable.V1.Of_set (String))

    [@@@end]
  end)

(* Parameterized form (RPC) *)
include Test.Is_S_rpc (struct
    type t = (string, String.comparator_witness) Set.t
    [@@deriving_inline streamable ~rpc ~version:1]

    include Streamable.Stable.V1.Remove_t_rpc (Streamable.Stable.V1.Of_set_rpc (String))

    [@@@end]
  end)
