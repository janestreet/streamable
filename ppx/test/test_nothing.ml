open! Core

(* With type equality *)
include Test.Is_S (struct
    type t = Nothing.t = | [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_streamable
           (Streamable.Stable.V1.Of_atomic
              (Core.Nothing))
              (struct
                type nonrec t = t

                let to_streamable = function
                  | (_ : t) -> .
                ;;

                let of_streamable = Core.Nothing.unreachable_code
              end))

    [@@@end]
  end)

(* With type equality (RPC) *)
include Test.Is_S_rpc (struct
    type t = Nothing.t = | [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_streamable_rpc
           (Streamable.Stable.V1.Of_atomic_rpc
              (Core.Nothing))
              (struct
                type nonrec t = t

                let to_streamable = function
                  | (_ : t) -> .
                ;;

                let of_streamable = Core.Nothing.unreachable_code
              end))

    [@@@end]
  end)

(* Without type equality *)
include Test.Is_S (struct
    type t = | [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_streamable
           (Streamable.Stable.V1.Of_atomic
              (Core.Nothing))
              (struct
                type nonrec t = t

                let to_streamable = function
                  | (_ : t) -> .
                ;;

                let of_streamable = Core.Nothing.unreachable_code
              end))

    [@@@end]
  end)

(* Without type equality (RPC) *)
include Test.Is_S_rpc (struct
    type t = | [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_streamable_rpc
           (Streamable.Stable.V1.Of_atomic_rpc
              (Core.Nothing))
              (struct
                type nonrec t = t

                let to_streamable = function
                  | (_ : t) -> .
                ;;

                let of_streamable = Core.Nothing.unreachable_code
              end))

    [@@@end]
  end)
