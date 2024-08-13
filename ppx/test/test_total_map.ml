open! Core

module Foo = struct
  module T = struct
    type t =
      | A
      | B of bool
      | C
    [@@deriving bin_io, compare, enumerate, sexp]

    include (val Comparator.make ~compare ~sexp_of_t)
    include (val Total_map.Enumeration.make ~all)
  end

  include T
  include Comparable.Make_using_comparator (T)
  module Total_map = Total_map.Make_with_witnesses (T)
end

(* Submodule form *)
include Test.Is_S (struct
    type t = int Foo.Total_map.t [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_total_map
           (Foo)
           (Streamable.Stable.V1.Of_atomic (Core.Int)))

    [@@@end]
  end)

(* Submodule form (RPC) *)
include Test.Is_S_rpc (struct
    type t = int Foo.Total_map.t [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_total_map_rpc
           (Foo)
           (Streamable.Stable.V1.Of_atomic_rpc (Core.Int)))

    [@@@end]
  end)

(* Parameterized form *)
include Test.Is_S (struct
    type t = (Foo.t, int, Foo.comparator_witness, Foo.enumeration_witness) Total_map.t
    [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_total_map
           (Foo)
           (Streamable.Stable.V1.Of_atomic (Core.Int)))

    [@@@end]
  end)

(* Parameterized form (RPC) *)
include Test.Is_S_rpc (struct
    type t = (Foo.t, int, Foo.comparator_witness, Foo.enumeration_witness) Total_map.t
    [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_total_map_rpc
           (Foo)
           (Streamable.Stable.V1.Of_atomic_rpc (Core.Int)))

    [@@@end]
  end)

(* Nested *)
include Test.Is_S (struct
    type t = int Foo.Total_map.t Foo.Total_map.t [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_total_map
           (Foo)
           (Streamable.Stable.V1.Of_total_map
              (Foo)
              (Streamable.Stable.V1.Of_atomic (Core.Int))))

    [@@@end]
  end)

(* Nested (RPC) *)
include Test.Is_S_rpc (struct
    type t = int Foo.Total_map.t Foo.Total_map.t
    [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_total_map_rpc
           (Foo)
           (Streamable.Stable.V1.Of_total_map_rpc
              (Foo)
              (Streamable.Stable.V1.Of_atomic_rpc (Core.Int))))

    [@@@end]
  end)
