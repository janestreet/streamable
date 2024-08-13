open! Core

(* 2 fields *)
include Test.Is_S (struct
    type t = int * string [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_tuple2
           (Streamable.Stable.V1.Of_atomic
              (Core.Int))
              (Streamable.Stable.V1.Of_atomic (Core.String)))

    [@@@end]
  end)

(* 2 fields (RPC) *)
include Test.Is_S_rpc (struct
    type t = int * string [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_tuple2_rpc
           (Streamable.Stable.V1.Of_atomic_rpc
              (Core.Int))
              (Streamable.Stable.V1.Of_atomic_rpc (Core.String)))

    [@@@end]
  end)

(* Nested *)
include Test.Is_S (struct
    type t = (int * string) * bool [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_tuple2
           (Streamable.Stable.V1.Of_tuple2
              (Streamable.Stable.V1.Of_atomic
                 (Core.Int))
                 (Streamable.Stable.V1.Of_atomic (Core.String)))
              (Streamable.Stable.V1.Of_atomic (Core.Bool)))

    [@@@end]
  end)

(* Nested (RPC) *)
include Test.Is_S_rpc (struct
    type t = (int * string) * bool [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_tuple2_rpc
           (Streamable.Stable.V1.Of_tuple2_rpc
              (Streamable.Stable.V1.Of_atomic_rpc
                 (Core.Int))
                 (Streamable.Stable.V1.Of_atomic_rpc (Core.String)))
              (Streamable.Stable.V1.Of_atomic_rpc (Core.Bool)))

    [@@@end]
  end)

(* Large tuple *)
include Test.Is_S (struct
    type t =
      int
      * char
      * float
      * string
      * int
      * char
      * float
      * string
      * int
      * char
      * float
      * string
      * int
      * char
      * float
      * string
      * int
      * char
      * float
    [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_streamable
           (Streamable.Stable.V1.Of_tuple7
              (Streamable.Stable.V1.Of_tuple3
                 (Streamable.Stable.V1.Of_atomic
                    (Core.Int))
                    (Streamable.Stable.V1.Of_atomic (Core.Char))
                 (Streamable.Stable.V1.Of_atomic (Core.Float)))
                 (Streamable.Stable.V1.Of_tuple3
                    (Streamable.Stable.V1.Of_atomic
                       (Core.String))
                       (Streamable.Stable.V1.Of_atomic (Core.Int))
                    (Streamable.Stable.V1.Of_atomic (Core.Char)))
                 (Streamable.Stable.V1.Of_tuple3
                    (Streamable.Stable.V1.Of_atomic
                       (Core.Float))
                       (Streamable.Stable.V1.Of_atomic (Core.String))
                    (Streamable.Stable.V1.Of_atomic (Core.Int)))
              (Streamable.Stable.V1.Of_tuple3
                 (Streamable.Stable.V1.Of_atomic
                    (Core.Char))
                    (Streamable.Stable.V1.Of_atomic (Core.Float))
                 (Streamable.Stable.V1.Of_atomic (Core.String)))
              (Streamable.Stable.V1.Of_tuple3
                 (Streamable.Stable.V1.Of_atomic
                    (Core.Int))
                    (Streamable.Stable.V1.Of_atomic (Core.Char))
                 (Streamable.Stable.V1.Of_atomic (Core.Float)))
              (Streamable.Stable.V1.Of_tuple3
                 (Streamable.Stable.V1.Of_atomic
                    (Core.String))
                    (Streamable.Stable.V1.Of_atomic (Core.Int))
                 (Streamable.Stable.V1.Of_atomic (Core.Char)))
              (Streamable.Stable.V1.Of_atomic (Core.Float)))
              (struct
                type t =
                  int
                  * char
                  * float
                  * string
                  * int
                  * char
                  * float
                  * string
                  * int
                  * char
                  * float
                  * string
                  * int
                  * char
                  * float
                  * string
                  * int
                  * char
                  * float

                let to_streamable (s, r, q, p, o, n, m, l, k, j, i, h, g, f, e, d, c, b, a)
                  =
                  (s, r, q), (p, o, n), (m, l, k), (j, i, h), (g, f, e), (d, c, b), a
                ;;

                let of_streamable
                  ((s, r, q), (p, o, n), (m, l, k), (j, i, h), (g, f, e), (d, c, b), a)
                  =
                  s, r, q, p, o, n, m, l, k, j, i, h, g, f, e, d, c, b, a
                ;;
              end))

    [@@@end]
  end)
