open! Core

(* Basic: primitive form *)
include Test.Is_S (struct
    type t = int option [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_option (Streamable.Stable.V1.Of_atomic (Core.Int)))

    [@@@end]
  end)

(* Basic: primitive form (RPC) *)
include Test.Is_S_rpc (struct
    type t = int option [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_option_rpc (Streamable.Stable.V1.Of_atomic_rpc (Core.Int)))

    [@@@end]
  end)

(* Nested: primitive form *)
include Test.Is_S (struct
    type t = string option option [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_option
           (Streamable.Stable.V1.Of_option (Streamable.Stable.V1.Of_atomic (Core.String))))

    [@@@end]
  end)

(* Nested: primitive form (RPC) *)
include Test.Is_S_rpc (struct
    type t = string option option [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_option_rpc
           (Streamable.Stable.V1.Of_option_rpc
              (Streamable.Stable.V1.Of_atomic_rpc (Core.String))))

    [@@@end]
  end)

(* Basic: module form *)
include Test.Is_S (struct
    type t = int Option.t [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_option (Streamable.Stable.V1.Of_atomic (Core.Int)))

    [@@@end]
  end)

(* Basic: module form (RPC) *)
include Test.Is_S_rpc (struct
    type t = int Option.t [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_option_rpc (Streamable.Stable.V1.Of_atomic_rpc (Core.Int)))

    [@@@end]
  end)

(* Stable: module form *)
include Test.Is_S (struct
    type t = int Option.Stable.V1.t [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_option (Streamable.Stable.V1.Of_atomic (Core.Int)))

    [@@@end]
  end)

(* Stable: module form (RPC) *)
include Test.Is_S_rpc (struct
    type t = int Option.Stable.V1.t [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_option_rpc (Streamable.Stable.V1.Of_atomic_rpc (Core.Int)))

    [@@@end]
  end)

(* Nested: module form *)
include Test.Is_S (struct
    type t = string Option.t Option.t [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_option
           (Streamable.Stable.V1.Of_option (Streamable.Stable.V1.Of_atomic (Core.String))))

    [@@@end]
  end)

(* Nested: module form (RPC) *)
include Test.Is_S_rpc (struct
    type t = string Option.t Option.t [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_option_rpc
           (Streamable.Stable.V1.Of_option_rpc
              (Streamable.Stable.V1.Of_atomic_rpc (Core.String))))

    [@@@end]
  end)
