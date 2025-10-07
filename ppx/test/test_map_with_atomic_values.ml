open! Core

(* Test the [@streamable.map_with_atomic_values] attribute *)

(* Submodule form with atomic values *)
include Test.Is_S (struct
    type t = (int String.Map.t[@streamable.map_with_atomic_values])
    [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_map_with_atomic_values
           (String)
           (struct
             type t = int [@@deriving bin_io, sexp]
           end))

    [@@@end]
  end)

(* Submodule form with atomic values (RPC), value type in Module.t form *)
include Test.Is_S_rpc (struct
    type t = (Int.t String.Map.t[@streamable.map_with_atomic_values])
    [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_map_with_atomic_values_rpc (String) (Int))

    [@@@end]
  end)

(* Parameterized form with atomic values, value type in Module.t form *)
include Test.Is_S (struct
    type t =
      ((String.t, Int.t, String.comparator_witness) Map.t
      [@streamable.map_with_atomic_values])
    [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_map_with_atomic_values (String) (Int))

    [@@@end]
  end)

(* Parameterized form with atomic values (RPC) *)
include Test.Is_S_rpc (struct
    type t =
      ((string, int, String.comparator_witness) Map.t[@streamable.map_with_atomic_values])
    [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_map_with_atomic_values_rpc
           (String)
           (struct
             type t = int [@@deriving bin_io]
           end))

    [@@@end]
  end)

(* M-module form with atomic values *)
include Test.Is_S (struct
    type t = (int Map.M(Int).t[@streamable.map_with_atomic_values])
    [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_map_with_atomic_values
           (Int)
           (struct
             type t = int [@@deriving bin_io, sexp]
           end))

    [@@@end]
  end)

(* M-module form with atomic values (RPC) *)
include Test.Is_S_rpc (struct
    type t = (int Map.M(Int).t[@streamable.map_with_atomic_values])
    [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_map_with_atomic_values_rpc
           (Int)
           (struct
             type t = int [@@deriving bin_io]
           end))

    [@@@end]
  end)

(* Test in a record field *)
include Test.Is_S (struct
    type t =
      { regular_map : int String.Map.t
      ; atomic_values_map : (int String.Map.t[@streamable.map_with_atomic_values])
      }
    [@@deriving_inline streamable ~version:1]

    include
      Streamable.Stable.V1.Remove_t
        (Streamable.Stable.V1.Of_streamable
           (Streamable.Stable.V1.Of_tuple2
              (Streamable.Stable.V1.Of_map
                 (String)
                 (Streamable.Stable.V1.Of_atomic (Core.Int)))
                 (Streamable.Stable.V1.Of_map_with_atomic_values
                    (String)
                    (struct
                      type t = int [@@deriving bin_io, sexp]
                    end)))
              (struct
                type nonrec t = t

                let to_streamable { regular_map; atomic_values_map } =
                  regular_map, atomic_values_map
                ;;

                let of_streamable (regular_map, atomic_values_map) =
                  { regular_map; atomic_values_map }
                ;;
              end))

    [@@@end]
  end)

(* Test in a record field (RPC) *)
include Test.Is_S_rpc (struct
    type t =
      { regular_map : int String.Map.t
      ; atomic_values_map : (int String.Map.t[@streamable.map_with_atomic_values])
      }
    [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_streamable_rpc
           (Streamable.Stable.V1.Of_tuple2_rpc
              (Streamable.Stable.V1.Of_map_rpc
                 (String)
                 (Streamable.Stable.V1.Of_atomic_rpc (Core.Int)))
                 (Streamable.Stable.V1.Of_map_with_atomic_values_rpc
                    (String)
                    (struct
                      type t = int [@@deriving bin_io]
                    end)))
              (struct
                type nonrec t = t

                let to_streamable { regular_map; atomic_values_map } =
                  regular_map, atomic_values_map
                ;;

                let of_streamable (regular_map, atomic_values_map) =
                  { regular_map; atomic_values_map }
                ;;
              end))

    [@@@end]
  end)

(* Polymorphic value type *)
include Test.Is_S_rpc (struct
    type t = (string option String.Map.t[@streamable.map_with_atomic_values])
    [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_map_with_atomic_values_rpc
           (String)
           (struct
             type t = string option [@@deriving bin_io]
           end))

    [@@@end]
  end)

(* Polymorphic value type in [Module.t] form *)
include Test.Is_S_rpc (struct
    type t = (string Option.t String.Map.t[@streamable.map_with_atomic_values])
    [@@deriving_inline streamable ~rpc ~version:1]

    include
      Streamable.Stable.V1.Remove_t_rpc
        (Streamable.Stable.V1.Of_map_with_atomic_values_rpc
           (String)
           (struct
             type t = string Option.t [@@deriving bin_io]
           end))

    [@@@end]
  end)
