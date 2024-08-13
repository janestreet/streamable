open! Core

(* 1 field *)
include Test.Is_S (struct
    type t = { a : int } [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_streamable
           (Streamable.Stable.V1.Of_atomic
              (Core.Int))
              (struct
                type nonrec t = t

                let to_streamable { a } = a
                let of_streamable a = { a }
              end))

    [@@@end]
  end)

(* 1 field (RPC) *)
include Test.Is_S_rpc (struct
    type t = { a : int } [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_streamable_rpc
           (Streamable.Stable.V1.Of_atomic_rpc
              (Core.Int))
              (struct
                type nonrec t = t

                let to_streamable { a } = a
                let of_streamable a = { a }
              end))

    [@@@end]
  end)

(* 2 fields *)
include Test.Is_S (struct
    type t =
      { a : int
      ; b : string
      }
    [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_streamable
           (Streamable.Stable.V1.Of_tuple2
              (Streamable.Stable.V1.Of_atomic
                 (Core.Int))
                 (Streamable.Stable.V1.Of_atomic (Core.String)))
              (struct
                type nonrec t = t

                let to_streamable { a; b } = a, b
                let of_streamable (a, b) = { a; b }
              end))

    [@@@end]
  end)

(* 2 fields (RPC) *)
include Test.Is_S_rpc (struct
    type t =
      { a : int
      ; b : string
      }
    [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_streamable_rpc
           (Streamable.Stable.V1.Of_tuple2_rpc
              (Streamable.Stable.V1.Of_atomic_rpc
                 (Core.Int))
                 (Streamable.Stable.V1.Of_atomic_rpc (Core.String)))
              (struct
                type nonrec t = t

                let to_streamable { a; b } = a, b
                let of_streamable (a, b) = { a; b }
              end))

    [@@@end]
  end)

(* Large record *)
include Test.Is_S (struct
    type t =
      { a : int
      ; b : char
      ; c : float
      ; d : string
      ; e : int
      ; f : char
      ; g : float
      ; h : string
      ; i : int
      ; j : char
      ; k : float
      ; l : string
      ; m : int
      ; n : char
      ; o : float
      ; p : string
      ; q : int
      ; r : char
      ; s : float
      }
    [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_streamable
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

                   let to_streamable
                     (s, r, q, p, o, n, m, l, k, j, i, h, g, f, e, d, c, b, a)
                     =
                     (s, r, q), (p, o, n), (m, l, k), (j, i, h), (g, f, e), (d, c, b), a
                   ;;

                   let of_streamable
                     ((s, r, q), (p, o, n), (m, l, k), (j, i, h), (g, f, e), (d, c, b), a)
                     =
                     s, r, q, p, o, n, m, l, k, j, i, h, g, f, e, d, c, b, a
                   ;;
                 end))
                 (struct
                   type nonrec t = t

                   let to_streamable
                     { a; b; c; d; e; f; g; h; i; j; k; l; m; n; o; p; q; r; s }
                     =
                     a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s
                   ;;

                   let of_streamable
                     (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)
                     =
                     { a; b; c; d; e; f; g; h; i; j; k; l; m; n; o; p; q; r; s }
                   ;;
                 end))

    [@@@end]
  end)
