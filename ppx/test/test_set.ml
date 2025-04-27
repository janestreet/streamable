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

(* Stable submodule form *)
include Test.Is_S (struct
    type t = String.Stable.V1.Set.t [@@deriving_inline streamable ~version:1]

    include Streamable.Stable.V1.Remove_t (Streamable.Stable.V1.Of_set (String.Stable.V1))

    [@@@end]
  end)

(* Stable submodule form (RPC) *)
include Test.Is_S_rpc (struct
    type t = String.Stable.V1.Set.t [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc (Streamable.Stable.V1.Of_set_rpc (String.Stable.V1))

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

(* M-module form *)
include Test.Is_S (struct
    type t = Set.M(String).t [@@deriving_inline streamable ~version:1]

    include Streamable.Stable.V1.Remove_t (Streamable.Stable.V1.Of_set (String))

    [@@@end]
  end)

(* M-module form (RPC) *)
include Test.Is_S_rpc (struct
    type t = Set.M(String).t [@@deriving_inline streamable ~rpc ~version:1]

    include Streamable.Stable.V1.Remove_t_rpc (Streamable.Stable.V1.Of_set_rpc (String))

    [@@@end]
  end)
