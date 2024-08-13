open! Core

(* Multiple type parameters with record type *)
module F_record = struct
  type ('a, 'b) t =
    { a : 'a
    ; b : 'b
    ; c : int
    }
  [@@deriving_inline streamable ~version:1]

  module Make_streamable (A : Streamable.S) (B : Streamable.S) = struct
    type nonrec t = (A.t, B.t) t

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_streamable
           (Streamable.Stable.V1.Of_tuple3 (A) (B)
              (Streamable.Stable.V1.Of_atomic (Core.Int)))
              (struct
                type nonrec t = t

                let to_streamable { a; b; c } = a, b, c
                let of_streamable (a, b, c) = { a; b; c }
              end))
  end

  [@@@end]
end

include Test.Is_S (struct
    type t = (int, string) F_record.t [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (F_record.Make_streamable
           (Streamable.Stable.V1.Of_atomic
              (Core.Int))
              (Streamable.Stable.V1.Of_atomic (Core.String)))

    [@@@end]
  end)

(* Same as above, except the RPC version. *)
module F_record_rpc = struct
  type ('a, 'b) t =
    { a : 'a
    ; b : 'b
    ; c : int
    }
  [@@deriving_inline streamable ~rpc ~version:1]

  module Make_streamable (A : Streamable.S_rpc) (B : Streamable.S_rpc) = struct
    type nonrec t = (A.t, B.t) t

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_streamable_rpc
           (Streamable.Stable.V1.Of_tuple3_rpc (A) (B)
              (Streamable.Stable.V1.Of_atomic_rpc (Core.Int)))
              (struct
                type nonrec t = t

                let to_streamable { a; b; c } = a, b, c
                let of_streamable (a, b, c) = { a; b; c }
              end))
  end

  [@@@end]
end

include Test.Is_S_rpc (struct
    type t = (int, string) F_record_rpc.t [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (F_record_rpc.Make_streamable
           (Streamable.Stable.V1.Of_atomic_rpc
              (Core.Int))
              (Streamable.Stable.V1.Of_atomic_rpc (Core.String)))

    [@@@end]
  end)

(* "Partially applied" type parameters. *)
module F_record_partial = struct
  type 'a t = (int, 'a) F_record.t [@@deriving_inline streamable ~version:1]

  module Make_streamable (A : Streamable.S) = struct
    type nonrec t = A.t t

    include
      Streamable.Stable.V1.Remove_t
        (F_record.Make_streamable (Streamable.Stable.V1.Of_atomic (Core.Int)) (A))
  end

  [@@@end]
end

include Test.Is_S (struct
    type t = string F_record_partial.t [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (F_record_partial.Make_streamable (Streamable.Stable.V1.Of_atomic (Core.String)))

    [@@@end]
  end)

(* Same as above, except the RPC version. *)
module F_record_partial_rpc = struct
  type 'a t = (int, 'a) F_record_rpc.t [@@deriving_inline streamable ~rpc ~version:1]

  module Make_streamable (A : Streamable.S_rpc) = struct
    type nonrec t = A.t t

    include
      Streamable.Stable.V1.Remove_t_rpc
        (F_record_rpc.Make_streamable (Streamable.Stable.V1.Of_atomic_rpc (Core.Int)) (A))
  end

  [@@@end]
end

include Test.Is_S_rpc (struct
    type t = string F_record_partial_rpc.t [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (F_record_partial_rpc.Make_streamable
           (Streamable.Stable.V1.Of_atomic_rpc (Core.String)))

    [@@@end]
  end)

(* Multiple type parameters with variant type *)
module F_variant = struct
  type ('a, 'b) t =
    | A of 'a
    | B of 'b
    | C of int
  [@@deriving_inline streamable ~version:1]

  module Make_streamable (A : Streamable.S) (B : Streamable.S) = struct
    type nonrec t = (A.t, B.t) t

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_streamable
           (Streamable.Stable.V1.Of_variant3 (A) (B)
              (Streamable.Stable.V1.Of_atomic (Core.Int)))
              (struct
                type nonrec t = t

                let to_streamable = function
                  | A a -> `A a
                  | B a -> `B a
                  | C a -> `C a
                ;;

                let of_streamable = function
                  | `A a -> A a
                  | `B a -> B a
                  | `C a -> C a
                ;;
              end))
  end

  [@@@end]
end

include Test.Is_S (struct
    type t = (int, string) F_variant.t [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (F_variant.Make_streamable
           (Streamable.Stable.V1.Of_atomic
              (Core.Int))
              (Streamable.Stable.V1.Of_atomic (Core.String)))

    [@@@end]
  end)

(* Same as above, except the RPC version. *)
module F_variant_rpc = struct
  type ('a, 'b) t =
    | A of 'a
    | B of 'b
    | C of int
  [@@deriving_inline streamable ~rpc ~version:1]

  module Make_streamable (A : Streamable.S_rpc) (B : Streamable.S_rpc) = struct
    type nonrec t = (A.t, B.t) t

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_streamable_rpc
           (Streamable.Stable.V1.Of_variant3_rpc (A) (B)
              (Streamable.Stable.V1.Of_atomic_rpc (Core.Int)))
              (struct
                type nonrec t = t

                let to_streamable = function
                  | A a -> `A a
                  | B a -> `B a
                  | C a -> `C a
                ;;

                let of_streamable = function
                  | `A a -> A a
                  | `B a -> B a
                  | `C a -> C a
                ;;
              end))
  end

  [@@@end]
end

include Test.Is_S_rpc (struct
    type t = (int, string) F_variant_rpc.t [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (F_variant_rpc.Make_streamable
           (Streamable.Stable.V1.Of_atomic_rpc
              (Core.Int))
              (Streamable.Stable.V1.Of_atomic_rpc (Core.String)))

    [@@@end]
  end)

(* Multiple type parameters with tuple type *)
module F_tuple = struct
  type ('a, 'b) t = 'a * 'b * int [@@deriving_inline streamable ~version:1]

  module Make_streamable (A : Streamable.S) (B : Streamable.S) = struct
    type nonrec t = (A.t, B.t) t

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_tuple3 (A) (B)
           (Streamable.Stable.V1.Of_atomic (Core.Int)))
  end

  [@@@end]
end

include Test.Is_S (struct
    type t = (int, string) F_tuple.t [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (F_tuple.Make_streamable
           (Streamable.Stable.V1.Of_atomic
              (Core.Int))
              (Streamable.Stable.V1.Of_atomic (Core.String)))

    [@@@end]
  end)

(* Same as above, except the RPC version. *)
module F_tuple_rpc = struct
  type ('a, 'b) t = 'a * 'b * int [@@deriving_inline streamable ~rpc ~version:1]

  module Make_streamable (A : Streamable.S_rpc) (B : Streamable.S_rpc) = struct
    type nonrec t = (A.t, B.t) t

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_tuple3_rpc (A) (B)
           (Streamable.Stable.V1.Of_atomic_rpc (Core.Int)))
  end

  [@@@end]
end

include Test.Is_S_rpc (struct
    type t = (int, string) F_tuple_rpc.t [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (F_tuple_rpc.Make_streamable
           (Streamable.Stable.V1.Of_atomic_rpc
              (Core.Int))
              (Streamable.Stable.V1.Of_atomic_rpc (Core.String)))

    [@@@end]
  end)

(* Nested type parameters *)
module F_nested = struct
  type 'a t = 'a [@@deriving_inline streamable ~version:1]

  module Make_streamable (A : Streamable.S) = struct
    type nonrec t = A.t t

    include Streamable.Stable.V1.Remove_t (A)
  end

  [@@@end]
end

include Test.Is_S (struct
    type t = int F_nested.t F_nested.t [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (F_nested.Make_streamable
           (F_nested.Make_streamable (Streamable.Stable.V1.Of_atomic (Core.Int))))

    [@@@end]
  end)

(* Same as above, except the RPC version. *)
module F_nested_rpc = struct
  type 'a t = 'a [@@deriving_inline streamable ~rpc ~version:1]

  module Make_streamable (A : Streamable.S_rpc) = struct
    type nonrec t = A.t t

    include Streamable.Stable.V1.Remove_t_rpc (A)
  end

  [@@@end]
end

include Test.Is_S_rpc (struct
    type t = int F_nested_rpc.t F_nested_rpc.t
    [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (F_nested_rpc.Make_streamable
           (F_nested_rpc.Make_streamable (Streamable.Stable.V1.Of_atomic_rpc (Core.Int))))

    [@@@end]
  end)

(* Unnamed/ignored type parameters. *)
module F_ignored = struct
  type ('a, _, _) t = 'a * int [@@deriving_inline streamable ~version:1]

  module Make_streamable
      (A : Streamable.S)
      (Unnamed_type_parameter1 : Streamable.S)
      (Unnamed_type_parameter2 : Streamable.S) =
  struct
    type nonrec t = (A.t, Unnamed_type_parameter1.t, Unnamed_type_parameter2.t) t

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_tuple2 (A) (Streamable.Stable.V1.Of_atomic (Core.Int)))
  end

  [@@@end]
end

include Test.Is_S (struct
    type t = (int, string, bool) F_ignored.t [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (F_ignored.Make_streamable
           (Streamable.Stable.V1.Of_atomic
              (Core.Int))
              (Streamable.Stable.V1.Of_atomic (Core.String))
           (Streamable.Stable.V1.Of_atomic (Core.Bool)))

    [@@@end]
  end)

(* Same as above, except the RPC version. *)
module F_ignored_rpc = struct
  type ('a, _, _) t = 'a * int [@@deriving_inline streamable ~rpc ~version:1]

  module Make_streamable
      (A : Streamable.S_rpc)
      (Unnamed_type_parameter1 : Streamable.S_rpc)
      (Unnamed_type_parameter2 : Streamable.S_rpc) =
  struct
    type nonrec t = (A.t, Unnamed_type_parameter1.t, Unnamed_type_parameter2.t) t

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_tuple2_rpc
           (A)
           (Streamable.Stable.V1.Of_atomic_rpc (Core.Int)))
  end

  [@@@end]
end

include Test.Is_S_rpc (struct
    type t = (int, string, bool) F_ignored_rpc.t
    [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (F_ignored_rpc.Make_streamable
           (Streamable.Stable.V1.Of_atomic_rpc
              (Core.Int))
              (Streamable.Stable.V1.Of_atomic_rpc (Core.String))
           (Streamable.Stable.V1.Of_atomic_rpc (Core.Bool)))

    [@@@end]
  end)

(* Parameterized types in nested modules. *)
module Foo = struct
  module Bar = struct
    type 'a t = 'a option [@@deriving_inline streamable ~version:1]

    module Make_streamable (A : Streamable.S) = struct
      type nonrec t = A.t t

      include Streamable.Stable.V1.Remove_t (Streamable.Stable.V1.Of_option (A))
    end

    [@@@end]
  end

  module Bar_rpc = struct
    type 'a t = 'a option [@@deriving_inline streamable ~rpc ~version:1]

    module Make_streamable (A : Streamable.S_rpc) = struct
      type nonrec t = A.t t

      include Streamable.Stable.V1.Remove_t_rpc (Streamable.Stable.V1.Of_option_rpc (A))
    end

    [@@@end]
  end
end

include Test.Is_S (struct
    type t = int Foo.Bar.t [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Foo.Bar.Make_streamable (Streamable.Stable.V1.Of_atomic (Core.Int)))

    [@@@end]
  end)

include Test.Is_S_rpc (struct
    type t = int Foo.Bar_rpc.t [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Foo.Bar_rpc.Make_streamable (Streamable.Stable.V1.Of_atomic_rpc (Core.Int)))

    [@@@end]
  end)

(* A parameterized type referring to another parameterized type. *)
module Baz = struct
  module F1 = struct
    type 'a t = 'a option [@@deriving_inline streamable ~version:1]

    module Make_streamable (A : Streamable.S) = struct
      type nonrec t = A.t t

      include Streamable.Stable.V1.Remove_t (Streamable.Stable.V1.Of_option (A))
    end

    [@@@end]
  end

  module F2 = struct
    type 'a t = 'a F1.t list [@@deriving_inline streamable ~version:1]

    module Make_streamable (A : Streamable.S) = struct
      type nonrec t = A.t t

      include
        Streamable.Stable.V1.Remove_t (Streamable.Stable.V1.Of_list (F1.Make_streamable (A)))
    end

    [@@@end]
  end
end

include Test.Is_S (struct
    type t = int Baz.F2.t [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Baz.F2.Make_streamable (Streamable.Stable.V1.Of_atomic (Core.Int)))

    [@@@end]
  end)

(* Same as the above, except the RPC version. *)
module Baz_rpc = struct
  module F1 = struct
    type 'a t = 'a option [@@deriving_inline streamable ~rpc ~version:1]

    module Make_streamable (A : Streamable.S_rpc) = struct
      type nonrec t = A.t t

      include Streamable.Stable.V1.Remove_t_rpc (Streamable.Stable.V1.Of_option_rpc (A))
    end

    [@@@end]
  end

  module F2 = struct
    type 'a t = 'a F1.t list [@@deriving_inline streamable ~rpc ~version:1]

    module Make_streamable (A : Streamable.S_rpc) = struct
      type nonrec t = A.t t

      include
        Streamable.Stable.V1.Remove_t_rpc
          (Streamable.Stable.V1.Of_list_rpc (F1.Make_streamable (A)))
    end

    [@@@end]
  end
end

include Test.Is_S_rpc (struct
    type t = int Baz_rpc.F2.t [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Baz_rpc.F2.Make_streamable (Streamable.Stable.V1.Of_atomic_rpc (Core.Int)))

    [@@@end]
  end)

(* Parameterized types in signatures. *)
module type F = sig
  type ('a, 'b, _) t [@@deriving_inline streamable]

  module Make_streamable : functor
      (A : Streamable.S)
      (B : Streamable.S)
      (Unnamed_type_parameter2 : Streamable.S)
      -> Streamable.S with type t = (A.t, B.t, Unnamed_type_parameter2.t) t

  [@@@end]
end

module F_impl : F = struct
  type ('a, 'b, _) t = 'a * 'b [@@deriving_inline streamable ~version:1]

  module Make_streamable
      (A : Streamable.S)
      (B : Streamable.S)
      (Unnamed_type_parameter2 : Streamable.S) =
  struct
    type nonrec t = (A.t, B.t, Unnamed_type_parameter2.t) t

    include Streamable.Stable.V1.Remove_t (Streamable.Stable.V1.Of_tuple2 (A) (B))
  end

  [@@@end]
end

include Test.Is_S (struct
    type t = (int, string, unit) F_impl.t [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (F_impl.Make_streamable
           (Streamable.Stable.V1.Of_atomic
              (Core.Int))
              (Streamable.Stable.V1.Of_atomic (Core.String))
           (Streamable.Stable.V1.Of_atomic (Core.Unit)))

    [@@@end]
  end)

(* Same as above, except the RPC version. *)
module type F_rpc = sig
  type ('a, 'b, _) t [@@deriving_inline streamable ~rpc]

  module Make_streamable : functor
      (A : Streamable.S_rpc)
      (B : Streamable.S_rpc)
      (Unnamed_type_parameter2 : Streamable.S_rpc)
      -> Streamable.S_rpc with type t = (A.t, B.t, Unnamed_type_parameter2.t) t

  [@@@end]
end

module F_impl_rpc : F_rpc = struct
  type ('a, 'b, _) t = 'a * 'b [@@deriving_inline streamable ~rpc ~version:1]

  module Make_streamable
      (A : Streamable.S_rpc)
      (B : Streamable.S_rpc)
      (Unnamed_type_parameter2 : Streamable.S_rpc) =
  struct
    type nonrec t = (A.t, B.t, Unnamed_type_parameter2.t) t

    include Streamable.Stable.V1.Remove_t_rpc (Streamable.Stable.V1.Of_tuple2_rpc (A) (B))
  end

  [@@@end]
end

include Test.Is_S_rpc (struct
    type t = (int, string, unit) F_impl_rpc.t
    [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (F_impl_rpc.Make_streamable
           (Streamable.Stable.V1.Of_atomic_rpc
              (Core.Int))
              (Streamable.Stable.V1.Of_atomic_rpc (Core.String))
           (Streamable.Stable.V1.Of_atomic_rpc (Core.Unit)))

    [@@@end]
  end)
