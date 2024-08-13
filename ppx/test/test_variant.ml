open! Core

(* 1 constructor *)
include Test.Is_S (struct
    type t = X of int [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_streamable
           (Streamable.Stable.V1.Of_atomic
              (Core.Int))
              (struct
                type nonrec t = t

                let to_streamable = function
                  | X a -> a
                ;;

                let of_streamable = function
                  | a -> X a
                ;;
              end))

    [@@@end]
  end)

(* 1 constructor (RPC) *)
include Test.Is_S_rpc (struct
    type t = X of int [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_streamable_rpc
           (Streamable.Stable.V1.Of_atomic_rpc
              (Core.Int))
              (struct
                type nonrec t = t

                let to_streamable = function
                  | X a -> a
                ;;

                let of_streamable = function
                  | a -> X a
                ;;
              end))

    [@@@end]
  end)

(* 1 no-arg constructor *)
include Test.Is_S (struct
    type t = X [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_streamable
           (Streamable.Stable.V1.Of_atomic
              (Core.Unit))
              (struct
                type nonrec t = t

                let to_streamable = function
                  | X -> ()
                ;;

                let of_streamable = function
                  | () -> X
                ;;
              end))

    [@@@end]
  end)

(* 1 no-arg constructor (RPC) *)
include Test.Is_S_rpc (struct
    type t = X [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_streamable_rpc
           (Streamable.Stable.V1.Of_atomic_rpc
              (Core.Unit))
              (struct
                type nonrec t = t

                let to_streamable = function
                  | X -> ()
                ;;

                let of_streamable = function
                  | () -> X
                ;;
              end))

    [@@@end]
  end)

(* 2 constructors *)
include Test.Is_S (struct
    type t =
      | X of int
      | Y of string * bool
    [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_streamable
           (Streamable.Stable.V1.Of_variant2
              (Streamable.Stable.V1.Of_atomic
                 (Core.Int))
                 (Streamable.Stable.V1.Of_tuple2
                    (Streamable.Stable.V1.Of_atomic
                       (Core.String))
                       (Streamable.Stable.V1.Of_atomic (Core.Bool))))
              (struct
                type nonrec t = t

                let to_streamable = function
                  | X a -> `A a
                  | Y (a, b) -> `B (a, b)
                ;;

                let of_streamable = function
                  | `A a -> X a
                  | `B (a, b) -> Y (a, b)
                ;;
              end))

    [@@@end]
  end)

(* 2 constructors (RPC) *)
include Test.Is_S_rpc (struct
    type t =
      | X of int
      | Y of string * bool
    [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_streamable_rpc
           (Streamable.Stable.V1.Of_variant2_rpc
              (Streamable.Stable.V1.Of_atomic_rpc
                 (Core.Int))
                 (Streamable.Stable.V1.Of_tuple2_rpc
                    (Streamable.Stable.V1.Of_atomic_rpc
                       (Core.String))
                       (Streamable.Stable.V1.Of_atomic_rpc (Core.Bool))))
              (struct
                type nonrec t = t

                let to_streamable = function
                  | X a -> `A a
                  | Y (a, b) -> `B (a, b)
                ;;

                let of_streamable = function
                  | `A a -> X a
                  | `B (a, b) -> Y (a, b)
                ;;
              end))

    [@@@end]
  end)

(* 2 constructors, 1 no-arg *)
include Test.Is_S (struct
    type t =
      | X
      | Y of string
    [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_streamable
           (Streamable.Stable.V1.Of_variant2
              (Streamable.Stable.V1.Of_atomic
                 (Core.Unit))
                 (Streamable.Stable.V1.Of_atomic (Core.String)))
              (struct
                type nonrec t = t

                let to_streamable = function
                  | X -> `A ()
                  | Y a -> `B a
                ;;

                let of_streamable = function
                  | `A () -> X
                  | `B a -> Y a
                ;;
              end))

    [@@@end]
  end)

(* 2 constructors, 1 no-arg (RPC) *)
include Test.Is_S_rpc (struct
    type t =
      | X
      | Y of string
    [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_streamable_rpc
           (Streamable.Stable.V1.Of_variant2_rpc
              (Streamable.Stable.V1.Of_atomic_rpc
                 (Core.Unit))
                 (Streamable.Stable.V1.Of_atomic_rpc (Core.String)))
              (struct
                type nonrec t = t

                let to_streamable = function
                  | X -> `A ()
                  | Y a -> `B a
                ;;

                let of_streamable = function
                  | `A () -> X
                  | `B a -> Y a
                ;;
              end))

    [@@@end]
  end)

(* 3 constructors, 1 no-arg, 1 tuple, 1 record *)
include Test.Is_S (struct
    type t =
      | X
      | Y of string * int
      | Z of
          { field1 : string
          ; field2 : bool
          }
    [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_streamable
           (Streamable.Stable.V1.Of_variant3
              (Streamable.Stable.V1.Of_atomic
                 (Core.Unit))
                 (Streamable.Stable.V1.Of_tuple2
                    (Streamable.Stable.V1.Of_atomic
                       (Core.String))
                       (Streamable.Stable.V1.Of_atomic (Core.Int)))
              (Streamable.Stable.V1.Of_tuple2
                 (Streamable.Stable.V1.Of_atomic
                    (Core.String))
                    (Streamable.Stable.V1.Of_atomic (Core.Bool))))
              (struct
                type nonrec t = t

                let to_streamable = function
                  | X -> `A ()
                  | Y (a, b) -> `B (a, b)
                  | Z { field1; field2 } -> `C (field1, field2)
                ;;

                let of_streamable = function
                  | `A () -> X
                  | `B (a, b) -> Y (a, b)
                  | `C (field1, field2) -> Z { field1; field2 }
                ;;
              end))

    [@@@end]
  end)

(* 3 constructors, 1 no-arg, 1 tuple, 1 record (rpc) *)
include Test.Is_S_rpc (struct
    type t =
      | X
      | Y of string * int
      | Z of
          { field1 : string
          ; field2 : bool
          }
    [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_streamable_rpc
           (Streamable.Stable.V1.Of_variant3_rpc
              (Streamable.Stable.V1.Of_atomic_rpc
                 (Core.Unit))
                 (Streamable.Stable.V1.Of_tuple2_rpc
                    (Streamable.Stable.V1.Of_atomic_rpc
                       (Core.String))
                       (Streamable.Stable.V1.Of_atomic_rpc (Core.Int)))
              (Streamable.Stable.V1.Of_tuple2_rpc
                 (Streamable.Stable.V1.Of_atomic_rpc
                    (Core.String))
                    (Streamable.Stable.V1.Of_atomic_rpc (Core.Bool))))
              (struct
                type nonrec t = t

                let to_streamable = function
                  | X -> `A ()
                  | Y (a, b) -> `B (a, b)
                  | Z { field1; field2 } -> `C (field1, field2)
                ;;

                let of_streamable = function
                  | `A () -> X
                  | `B (a, b) -> Y (a, b)
                  | `C (field1, field2) -> Z { field1; field2 }
                ;;
              end))

    [@@@end]
  end)

(* Large variant *)
include Test.Is_S (struct
    type t =
      | A
      | B of int
      | C of bool
      | D of float
      | E of int * string
      | F
      | G of int
      | H of bool
      | I of float
      | J of int * string
      | K
      | L of int
      | M of bool
      | N of float
      | O of int * string
      | P of
          { foo : int
          ; bar : bool
          }
    [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_streamable
           (Streamable.Stable.V1.Of_streamable
              (Streamable.Stable.V1.Of_variant4
                 (Streamable.Stable.V1.Of_variant4
                    (Streamable.Stable.V1.Of_atomic
                       (Core.Unit))
                       (Streamable.Stable.V1.Of_atomic (Core.Int))
                    (Streamable.Stable.V1.Of_atomic (Core.Bool))
                    (Streamable.Stable.V1.Of_atomic (Core.Float)))
                    (Streamable.Stable.V1.Of_variant4
                       (Streamable.Stable.V1.Of_tuple2
                          (Streamable.Stable.V1.Of_atomic
                             (Core.Int))
                             (Streamable.Stable.V1.Of_atomic (Core.String)))
                          (Streamable.Stable.V1.Of_atomic (Core.Unit))
                          (Streamable.Stable.V1.Of_atomic (Core.Int))
                       (Streamable.Stable.V1.Of_atomic (Core.Bool)))
                    (Streamable.Stable.V1.Of_variant4
                       (Streamable.Stable.V1.Of_atomic
                          (Core.Float))
                          (Streamable.Stable.V1.Of_tuple2
                             (Streamable.Stable.V1.Of_atomic
                                (Core.Int))
                                (Streamable.Stable.V1.Of_atomic (Core.String)))
                       (Streamable.Stable.V1.Of_atomic (Core.Unit))
                       (Streamable.Stable.V1.Of_atomic (Core.Int)))
                 (Streamable.Stable.V1.Of_variant4
                    (Streamable.Stable.V1.Of_atomic
                       (Core.Bool))
                       (Streamable.Stable.V1.Of_atomic (Core.Float))
                    (Streamable.Stable.V1.Of_tuple2
                       (Streamable.Stable.V1.Of_atomic
                          (Core.Int))
                          (Streamable.Stable.V1.Of_atomic (Core.String)))
                    (Streamable.Stable.V1.Of_tuple2
                       (Streamable.Stable.V1.Of_atomic
                          (Core.Int))
                          (Streamable.Stable.V1.Of_atomic (Core.Bool)))))
                 (struct
                   type t =
                     [ `A of Core.Unit.t
                     | `B of int
                     | `C of bool
                     | `D of float
                     | `E of int * string
                     | `F of Core.Unit.t
                     | `G of int
                     | `H of bool
                     | `I of float
                     | `J of int * string
                     | `K of Core.Unit.t
                     | `L of int
                     | `M of bool
                     | `N of float
                     | `O of int * string
                     | `P of int * bool
                     ]

                   let to_streamable = function
                     | `A a -> `A (`A a)
                     | `B b -> `A (`B b)
                     | `C c -> `A (`C c)
                     | `D d -> `A (`D d)
                     | `E e -> `B (`A e)
                     | `F f -> `B (`B f)
                     | `G g -> `B (`C g)
                     | `H h -> `B (`D h)
                     | `I i -> `C (`A i)
                     | `J j -> `C (`B j)
                     | `K k -> `C (`C k)
                     | `L l -> `C (`D l)
                     | `M m -> `D (`A m)
                     | `N n -> `D (`B n)
                     | `O o -> `D (`C o)
                     | `P p -> `D (`D p)
                   ;;

                   let of_streamable = function
                     | `A (`A a) -> `A a
                     | `A (`B b) -> `B b
                     | `A (`C c) -> `C c
                     | `A (`D d) -> `D d
                     | `B (`A e) -> `E e
                     | `B (`B f) -> `F f
                     | `B (`C g) -> `G g
                     | `B (`D h) -> `H h
                     | `C (`A i) -> `I i
                     | `C (`B j) -> `J j
                     | `C (`C k) -> `K k
                     | `C (`D l) -> `L l
                     | `D (`A m) -> `M m
                     | `D (`B n) -> `N n
                     | `D (`C o) -> `O o
                     | `D (`D p) -> `P p
                   ;;
                 end))
                 (struct
                   type nonrec t = t

                   let to_streamable = function
                     | A -> `A ()
                     | B a -> `B a
                     | C a -> `C a
                     | D a -> `D a
                     | E (a, b) -> `E (a, b)
                     | F -> `F ()
                     | G a -> `G a
                     | H a -> `H a
                     | I a -> `I a
                     | J (a, b) -> `J (a, b)
                     | K -> `K ()
                     | L a -> `L a
                     | M a -> `M a
                     | N a -> `N a
                     | O (a, b) -> `O (a, b)
                     | P { foo; bar } -> `P (foo, bar)
                   ;;

                   let of_streamable = function
                     | `A () -> A
                     | `B a -> B a
                     | `C a -> C a
                     | `D a -> D a
                     | `E (a, b) -> E (a, b)
                     | `F () -> F
                     | `G a -> G a
                     | `H a -> H a
                     | `I a -> I a
                     | `J (a, b) -> J (a, b)
                     | `K () -> K
                     | `L a -> L a
                     | `M a -> M a
                     | `N a -> N a
                     | `O (a, b) -> O (a, b)
                     | `P (foo, bar) -> P { foo; bar }
                   ;;
                 end))

    [@@@end]
  end)
