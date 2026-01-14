open! Core
open! Import

(*_ see [../doc/streamable.mkd] for a high level introdution to this signature *)

(** Functors for serializing and deserializing potentially large values incrementally.

    Serializing large values can cause problems

    - bin_prot places size limits on what can be serialized.

    - long-running monolithic (de)serializations block out the rest of the program from
      running, even if we are using a concurrency library like Async.

    Instead, we want to serialize large values incrementally, which necessitates breaking
    them into reasonably sized parts that can individually serialized and then
    re-assembled after deserialization. *)

(** see comment in [Module_type] where [S] is defined *)
module type S = Module_type.S

module type S_rpc = Module_type.S_rpc
module type S_rpc_with_sexp_of_part = Module_type.S_rpc_with_sexp_of_part

(** [Stable_without_of_sexp] is used for keys in [S_rpc]-returning functors *)
module type%template [@mode m = (global, local)] Stable_without_of_sexp = sig
  type t [@@deriving bin_io, (compare [@mode m]), sexp_of]

  include Comparator.Stable.V1.S with type t := t
end

module type Of_atomic = functor
    (A : sig
       type t [@@deriving bin_io, sexp]
     end)
    -> S with type t = A.t

module type Of_atomic_rpc = functor
    (A : sig
       type t [@@deriving bin_io]
     end)
    -> S_rpc with type t = A.t

module type Of_map = functor (Key : Stable) (Data : S) ->
  S with type t = (Key.t, Data.t, Key.comparator_witness) Map.t

module type Of_map_rpc = functor (Key : Stable_without_of_sexp) (Data : S_rpc) ->
  S_rpc with type t = (Key.t, Data.t, Key.comparator_witness) Map.t

module type Of_map_with_atomic_values = functor
    (Key : Stable)
    (Data : sig
       type t [@@deriving bin_io, sexp]
     end)
    -> S with type t = (Key.t, Data.t, Key.comparator_witness) Map.t

module type Of_map_with_atomic_values_rpc = functor
    (Key : Stable_without_of_sexp)
    (Data : sig
       type t

       include Binable.S with type t := t
     end)
    -> S_rpc with type t = (Key.t, Data.t, Key.comparator_witness) Map.t

module type Of_total_map = functor (Key : Total_map.Key_with_witnesses) (Data : S) ->
  S
  with type t =
    (Key.t, Data.t, Key.comparator_witness, Key.enumeration_witness) Total_map.t

module type Of_total_map_rpc = functor
    (Key : Total_map.Key_with_witnesses)
    (Data : S_rpc)
    ->
  S_rpc
  with type t =
    (Key.t, Data.t, Key.comparator_witness, Key.enumeration_witness) Total_map.t

module type Of_hashtbl = functor
    (Key : sig
       include Stable
       include Hashtbl.Key with type t := t
     end)
    (Data : S)
    -> S with type t = (Key.t, Data.t) Hashtbl.t

module type Of_hashtbl_rpc = functor
    (Key : sig
       include Stable_without_of_sexp
       include Hashtbl.Key_plain with type t := t
     end)
    (Data : S_rpc)
    -> S_rpc with type t = (Key.t, Data.t) Hashtbl.t

module type Of_set = functor (Key : Stable) ->
  S with type t = (Key.t, Key.comparator_witness) Set.t

module type Of_set_rpc = functor (Key : Stable_without_of_sexp) ->
  S_rpc with type t = (Key.t, Key.comparator_witness) Set.t

module type Of_tuple2 = functor (A : S) (B : S) -> S with type t = A.t * B.t

module type Of_tuple2_rpc = functor (A : S_rpc) (B : S_rpc) ->
  S_rpc with type t = A.t * B.t

module type Of_tuple3 = functor (A : S) (B : S) (C : S) -> S with type t = A.t * B.t * C.t

module type Of_tuple3_rpc = functor (A : S_rpc) (B : S_rpc) (C : S_rpc) ->
  S_rpc with type t = A.t * B.t * C.t

module type Of_tuple4 = functor (A : S) (B : S) (C : S) (D : S) ->
  S with type t = A.t * B.t * C.t * D.t

module type Of_tuple4_rpc = functor (A : S_rpc) (B : S_rpc) (C : S_rpc) (D : S_rpc) ->
  S_rpc with type t = A.t * B.t * C.t * D.t

module type Of_tuple5 = functor (A : S) (B : S) (C : S) (D : S) (E : S) ->
  S with type t = A.t * B.t * C.t * D.t * E.t

module type Of_tuple5_rpc = functor
    (A : S_rpc)
    (B : S_rpc)
    (C : S_rpc)
    (D : S_rpc)
    (E : S_rpc)
    -> S_rpc with type t = A.t * B.t * C.t * D.t * E.t

module type Of_tuple6 = functor (A : S) (B : S) (C : S) (D : S) (E : S) (F : S) ->
  S with type t = A.t * B.t * C.t * D.t * E.t * F.t

module type Of_tuple6_rpc = functor
    (A : S_rpc)
    (B : S_rpc)
    (C : S_rpc)
    (D : S_rpc)
    (E : S_rpc)
    (F : S_rpc)
    -> S_rpc with type t = A.t * B.t * C.t * D.t * E.t * F.t

module type Of_tuple7 = functor (A : S) (B : S) (C : S) (D : S) (E : S) (F : S) (G : S) ->
  S with type t = A.t * B.t * C.t * D.t * E.t * F.t * G.t

module type Of_tuple7_rpc = functor
    (A : S_rpc)
    (B : S_rpc)
    (C : S_rpc)
    (D : S_rpc)
    (E : S_rpc)
    (F : S_rpc)
    (G : S_rpc)
    -> S_rpc with type t = A.t * B.t * C.t * D.t * E.t * F.t * G.t

module type Of_tuple8 = functor
    (A : S)
    (B : S)
    (C : S)
    (D : S)
    (E : S)
    (F : S)
    (G : S)
    (H : S)
    -> S with type t = A.t * B.t * C.t * D.t * E.t * F.t * G.t * H.t

module type Of_tuple8_rpc = functor
    (A : S_rpc)
    (B : S_rpc)
    (C : S_rpc)
    (D : S_rpc)
    (E : S_rpc)
    (F : S_rpc)
    (G : S_rpc)
    (H : S_rpc)
    -> S_rpc with type t = A.t * B.t * C.t * D.t * E.t * F.t * G.t * H.t

module type Of_tuple9 = functor
    (A : S)
    (B : S)
    (C : S)
    (D : S)
    (E : S)
    (F : S)
    (G : S)
    (H : S)
    (I : S)
    -> S with type t = A.t * B.t * C.t * D.t * E.t * F.t * G.t * H.t * I.t

module type Of_tuple9_rpc = functor
    (A : S_rpc)
    (B : S_rpc)
    (C : S_rpc)
    (D : S_rpc)
    (E : S_rpc)
    (F : S_rpc)
    (G : S_rpc)
    (H : S_rpc)
    (I : S_rpc)
    -> S_rpc with type t = A.t * B.t * C.t * D.t * E.t * F.t * G.t * H.t * I.t

(*$ Streamable_cinaps.of_variant_intf 2 *)
module type Of_variant2 = functor (A : S) (B : S) ->
  S
  with type t =
    [ `A of A.t
    | `B of B.t
    ]

(*$*)

(*$ Streamable_cinaps.of_variant_rpc_intf 2 *)
module type Of_variant2_rpc = functor (A : S_rpc) (B : S_rpc) ->
  S_rpc
  with type t =
    [ `A of A.t
    | `B of B.t
    ]

(*$*)

(*$ Streamable_cinaps.of_variant_intf 3 *)
module type Of_variant3 = functor (A : S) (B : S) (C : S) ->
  S
  with type t =
    [ `A of A.t
    | `B of B.t
    | `C of C.t
    ]

(*$*)

(*$ Streamable_cinaps.of_variant_rpc_intf 3 *)
module type Of_variant3_rpc = functor (A : S_rpc) (B : S_rpc) (C : S_rpc) ->
  S_rpc
  with type t =
    [ `A of A.t
    | `B of B.t
    | `C of C.t
    ]

(*$*)

(*$ Streamable_cinaps.of_variant_intf 4 *)
module type Of_variant4 = functor (A : S) (B : S) (C : S) (D : S) ->
  S
  with type t =
    [ `A of A.t
    | `B of B.t
    | `C of C.t
    | `D of D.t
    ]

(*$*)

(*$ Streamable_cinaps.of_variant_rpc_intf 4 *)
module type Of_variant4_rpc = functor (A : S_rpc) (B : S_rpc) (C : S_rpc) (D : S_rpc) ->
  S_rpc
  with type t =
    [ `A of A.t
    | `B of B.t
    | `C of C.t
    | `D of D.t
    ]

(*$*)

(*$ Streamable_cinaps.of_variant_intf 5 *)
module type Of_variant5 = functor (A : S) (B : S) (C : S) (D : S) (E : S) ->
  S
  with type t =
    [ `A of A.t
    | `B of B.t
    | `C of C.t
    | `D of D.t
    | `E of E.t
    ]

(*$*)

(*$ Streamable_cinaps.of_variant_rpc_intf 5 *)
module type Of_variant5_rpc = functor
    (A : S_rpc)
    (B : S_rpc)
    (C : S_rpc)
    (D : S_rpc)
    (E : S_rpc)
    ->
  S_rpc
  with type t =
    [ `A of A.t
    | `B of B.t
    | `C of C.t
    | `D of D.t
    | `E of E.t
    ]

(*$*)

module type Of_list = functor (A : S) -> S with type t = A.t list
module type Of_list_rpc = functor (A : S_rpc) -> S_rpc with type t = A.t list
module type Of_nonempty_list = functor (A : S) -> S with type t = A.t Nonempty_list.t

module type Of_nonempty_list_rpc = functor (A : S_rpc) ->
  S_rpc with type t = A.t Nonempty_list.t

module type Of_option = functor (A : S) -> S with type t = A.t option
module type Of_option_rpc = functor (A : S_rpc) -> S_rpc with type t = A.t option
module type Of_result = functor (A : S) (B : S) -> S with type t = (A.t, B.t) result

module type Of_result_rpc = functor (A : S_rpc) (B : S_rpc) ->
  S_rpc with type t = (A.t, B.t) result

module type Of_fqueue = functor (A : S) -> S with type t = A.t Fqueue.t
module type Of_fqueue_rpc = functor (A : S_rpc) -> S_rpc with type t = A.t Fqueue.t
module type Of_sequence = functor (A : S) -> S with type t = A.t Sequence.t
module type Of_sequence_rpc = functor (A : S_rpc) -> S_rpc with type t = A.t Sequence.t

module type Of_streamable = functor
    (Streamable : S)
    (X : sig
       type t

       val to_streamable : t -> Streamable.t
       val of_streamable : Streamable.t -> t
     end)
    -> S with type t = X.t

module type Of_streamable_rpc = functor
    (Streamable : S_rpc)
    (X : sig
       type t

       val to_streamable : t -> Streamable.t
       val of_streamable : Streamable.t -> t
     end)
    -> S_rpc with type t = X.t

module type Of_sexpable = functor
    (Sexpable : sig
       type t

       include Sexpable with type t := t
     end)
    -> S with type t = Sexpable.t

(** The [Fixpoint] functor can be used to make recursive types streamable *)
module type Fixpoint = functor
    (T : T)
    (F : functor (X : S with type t = T.t) -> S with type t = T.t)
    -> S with type t = T.t

module type Fixpoint_rpc = functor
    (T : T)
    (F : functor (X : S_rpc with type t = T.t) -> S_rpc with type t = T.t)
    -> S_rpc with type t = T.t

(** [Checked] is a wrapper functor for finding places that are producing binio values that
    are too large. The output behaves exactly like the input, except that [to_parts] will
    raise if it ever produces an intermediate part whose binio size exceeds
    [max_intermediate_part_bin_size]. *)
module type Checked = functor
    (Limit : sig
       val max_intermediate_part_bin_size : int
       val here : Source_code_position.t
     end)
    (X : S)
    -> S with type t = X.t

module type Packed = functor (X : S) -> S with type t = X.t
module type Packed_rpc = functor (X : S_rpc) -> S_rpc with type t = X.t

module type Main = sig
  module type S = S
  module type S_rpc = S_rpc
  module type S_rpc_with_sexp_of_part = S_rpc_with_sexp_of_part
  module type Of_atomic = Of_atomic
  module type Of_atomic_rpc = Of_atomic_rpc
  module type Of_map = Of_map
  module type Of_map_rpc = Of_map_rpc
  module type Of_map_with_atomic_values = Of_map_with_atomic_values
  module type Of_map_with_atomic_values_rpc = Of_map_with_atomic_values_rpc
  module type Of_total_map = Of_total_map
  module type Of_total_map_rpc = Of_total_map_rpc
  module type Of_hashtbl = Of_hashtbl
  module type Of_hashtbl_rpc = Of_hashtbl_rpc
  module type Of_set = Of_set
  module type Of_set_rpc = Of_set_rpc
  module type Of_tuple2 = Of_tuple2
  module type Of_tuple2_rpc = Of_tuple2_rpc
  module type Of_tuple3 = Of_tuple3
  module type Of_tuple3_rpc = Of_tuple3_rpc
  module type Of_tuple4 = Of_tuple4
  module type Of_tuple4_rpc = Of_tuple4_rpc
  module type Of_tuple5 = Of_tuple5
  module type Of_tuple5_rpc = Of_tuple5_rpc
  module type Of_tuple6 = Of_tuple6
  module type Of_tuple6_rpc = Of_tuple6_rpc
  module type Of_variant2 = Of_variant2
  module type Of_variant2_rpc = Of_variant2_rpc
  module type Of_variant3 = Of_variant3
  module type Of_variant3_rpc = Of_variant3_rpc
  module type Of_variant4 = Of_variant4
  module type Of_variant4_rpc = Of_variant4_rpc
  module type Of_variant5 = Of_variant5
  module type Of_variant5_rpc = Of_variant5_rpc
  module type Of_list = Of_list
  module type Of_list_rpc = Of_list_rpc
  module type Of_nonempty_list = Of_nonempty_list
  module type Of_nonempty_list_rpc = Of_nonempty_list_rpc
  module type Of_option = Of_option
  module type Of_option_rpc = Of_option_rpc
  module type Of_result = Of_result
  module type Of_result_rpc = Of_result_rpc
  module type Of_fqueue = Of_fqueue
  module type Of_fqueue_rpc = Of_fqueue_rpc
  module type Of_sequence = Of_sequence
  module type Of_sequence_rpc = Of_sequence_rpc
  module type Of_streamable = Of_streamable
  module type Of_streamable_rpc = Of_streamable_rpc
  module type Fixpoint = Fixpoint
  module type Fixpoint_rpc = Fixpoint_rpc
  module type Checked = Checked
  module type Packed = Packed
  module type Packed_rpc = Packed_rpc

  (** The latest versions of each functor. These functors are unstable *)

  module Of_atomic : Of_atomic
  module Of_atomic_rpc : Of_atomic_rpc
  module Of_map : Of_map
  module Of_map_rpc : Of_map_rpc
  module Of_map_with_atomic_values : Of_map_with_atomic_values
  module Of_map_with_atomic_values_rpc : Of_map_with_atomic_values_rpc
  module Of_total_map : Of_total_map
  module Of_total_map_rpc : Of_total_map_rpc
  module Of_hashtbl : Of_hashtbl
  module Of_hashtbl_rpc : Of_hashtbl_rpc
  module Of_set : Of_set
  module Of_set_rpc : Of_set_rpc
  module Of_tuple2 : Of_tuple2
  module Of_tuple2_rpc : Of_tuple2_rpc
  module Of_tuple3 : Of_tuple3
  module Of_tuple3_rpc : Of_tuple3_rpc
  module Of_tuple4 : Of_tuple4
  module Of_tuple4_rpc : Of_tuple4_rpc
  module Of_tuple5 : Of_tuple5
  module Of_tuple5_rpc : Of_tuple5_rpc
  module Of_tuple6 : Of_tuple6
  module Of_tuple6_rpc : Of_tuple6_rpc
  module Of_tuple7 : Of_tuple7
  module Of_tuple7_rpc : Of_tuple7_rpc
  module Of_tuple8 : Of_tuple8
  module Of_tuple8_rpc : Of_tuple8_rpc
  module Of_tuple9 : Of_tuple9
  module Of_tuple9_rpc : Of_tuple9_rpc
  module Of_variant2 : Of_variant2
  module Of_variant2_rpc : Of_variant2_rpc
  module Of_variant3 : Of_variant3
  module Of_variant3_rpc : Of_variant3_rpc
  module Of_variant4 : Of_variant4
  module Of_variant4_rpc : Of_variant4_rpc
  module Of_variant5 : Of_variant5
  module Of_variant5_rpc : Of_variant5_rpc
  module Of_list : Of_list
  module Of_list_rpc : Of_list_rpc
  module Of_nonempty_list : Of_nonempty_list
  module Of_nonempty_list_rpc : Of_nonempty_list_rpc
  module Of_option : Of_option
  module Of_option_rpc : Of_option_rpc
  module Of_result : Of_result
  module Of_result_rpc : Of_result_rpc
  module Of_fqueue : Of_fqueue
  module Of_fqueue_rpc : Of_fqueue_rpc
  module Of_sequence : Of_sequence
  module Of_sequence_rpc : Of_sequence_rpc
  module Of_streamable : Of_streamable
  module Of_streamable_rpc : Of_streamable_rpc
  module Of_sexpable : Of_sexpable
  module Fixpoint : Fixpoint
  module Fixpoint_rpc : Fixpoint_rpc
  module Checked : Checked
  module Packed : Packed
  module Packed_rpc : Packed_rpc
  module Remove_t_rpc = Remove_t.F_rpc
  module Remove_t = Remove_t.F

  module Stable : sig
    module type S = S
    module type S_rpc = S_rpc
    module type S_rpc_with_sexp_of_part = S_rpc_with_sexp_of_part

    module Remove_t = Remove_t
    module Remove_t_rpc = Remove_t_rpc

    (** Individually-accessible stable versions of each functor. *)

    module Checked : Checked

    module Packed : sig
      module V1 : Packed
    end

    module Packed_rpc : sig
      module V1 : Packed_rpc
    end

    module Fixpoint : sig
      module V1 : Fixpoint
    end

    module Fixpoint_rpc : sig
      module V1 : Fixpoint_rpc
    end

    module Of_atomic : sig
      module V1 : Of_atomic
    end

    module Of_atomic_rpc : sig
      module V1 : Of_atomic_rpc
    end

    module Of_fqueue : sig
      module V2 : Of_fqueue
      module V3 : Of_fqueue
    end

    module Of_fqueue_rpc : sig
      module V2 : Of_fqueue_rpc
      module V3 : Of_fqueue_rpc
    end

    module Of_hashtbl : sig
      module V1 : Of_hashtbl
    end

    module Of_hashtbl_rpc : sig
      module V1 : Of_hashtbl_rpc
    end

    module Of_list : sig
      module V2 : Of_list
      module V3 : Of_list
    end

    module Of_list_rpc : sig
      module V2 : Of_list_rpc
      module V3 : Of_list_rpc
    end

    module Of_nonempty_list : sig
      module V1 : Of_nonempty_list
    end

    module Of_nonempty_list_rpc : sig
      module V1 : Of_nonempty_list_rpc
    end

    module Of_map : sig
      module V1 : Of_map
      module V2 : Of_map
    end

    module Of_map_rpc : sig
      module V1 : Of_map_rpc
      module V2 : Of_map_rpc
    end

    module Of_map_with_atomic_values : sig
      module V1 : Of_map_with_atomic_values
    end

    module Of_map_with_atomic_values_rpc : sig
      module V1 : Of_map_with_atomic_values_rpc
    end

    module Of_total_map : sig
      module V1 : Of_total_map
    end

    module Of_total_map_rpc : sig
      module V1 : Of_total_map_rpc
    end

    module Of_option : sig
      module V1 : Of_option
      module V2 : Of_option
    end

    module Of_option_rpc : sig
      module V1 : Of_option_rpc
      module V2 : Of_option_rpc
    end

    module Of_result : sig
      module V1 : Of_result
    end

    module Of_result_rpc : sig
      module V1 : Of_result_rpc
    end

    module Of_set : sig
      module V2 : Of_set
      module V3 : Of_set
    end

    module Of_set_rpc : sig
      module V2 : Of_set_rpc
      module V3 : Of_set_rpc
    end

    module Of_sequence : sig
      module V1 : Of_sequence
    end

    module Of_sequence_rpc : sig
      module V1 : Of_sequence_rpc
    end

    module Of_streamable : sig
      module V1 : Of_streamable
    end

    module Of_streamable_rpc : sig
      module V1 : Of_streamable_rpc
    end

    module Of_tuple2 : sig
      module V1 : Of_tuple2
    end

    module Of_tuple2_rpc : sig
      module V1 : Of_tuple2_rpc
    end

    module Of_tuple3 : sig
      module V1 : Of_tuple3
    end

    module Of_tuple3_rpc : sig
      module V1 : Of_tuple3_rpc
    end

    module Of_tuple4 : sig
      module V1 : Of_tuple4
    end

    module Of_tuple4_rpc : sig
      module V1 : Of_tuple4_rpc
    end

    module Of_tuple5 : sig
      module V1 : Of_tuple5
    end

    module Of_tuple5_rpc : sig
      module V1 : Of_tuple5_rpc
    end

    module Of_tuple6 : sig
      module V1 : Of_tuple6
    end

    module Of_tuple6_rpc : sig
      module V1 : Of_tuple6_rpc
    end

    module Of_tuple7 : sig
      module V1 : Of_tuple7
    end

    module Of_tuple7_rpc : sig
      module V1 : Of_tuple7_rpc
    end

    module Of_tuple8 : sig
      module V1 : Of_tuple8
    end

    module Of_tuple8_rpc : sig
      module V1 : Of_tuple8_rpc
    end

    module Of_tuple9 : sig
      module V1 : Of_tuple9
    end

    module Of_tuple9_rpc : sig
      module V1 : Of_tuple9_rpc
    end

    module Of_variant2 : sig
      module V1 : Of_variant2
    end

    module Of_variant2_rpc : sig
      module V1 : Of_variant2_rpc
    end

    module Of_variant3 : sig
      module V1 : Of_variant3
    end

    module Of_variant3_rpc : sig
      module V1 : Of_variant3_rpc
    end

    module Of_variant4 : sig
      module V1 : Of_variant4
    end

    module Of_variant4_rpc : sig
      module V1 : Of_variant4_rpc
    end

    module Of_variant5 : sig
      module V1 : Of_variant5
    end

    module Of_variant5_rpc : sig
      module V1 : Of_variant5_rpc
    end

    module Of_sexpable : sig
      module V1 : Of_sexpable
    end

    (** Toplevel versions of the [Streamable] library, used by
        [@@deriving streamable ~version].

        These are meant to be "add only" -- new functor types can be added but not
        removed, and version cannot change after it's added. If we ever want add a new
        version of one of the functors bundled in a [Streamable.Stable.Vn] module, we'll
        add [Streamable.Stable.V(n+1)] and add it there. *)

    module V1 : sig
      module Fixpoint = Fixpoint.V1
      module Fixpoint_rpc = Fixpoint_rpc.V1
      module Of_atomic = Of_atomic.V1
      module Of_atomic_rpc = Of_atomic_rpc.V1
      module Of_fqueue = Of_fqueue.V3
      module Of_fqueue_rpc = Of_fqueue_rpc.V3
      module Of_hashtbl = Of_hashtbl.V1
      module Of_hashtbl_rpc = Of_hashtbl_rpc.V1
      module Of_list = Of_list.V3
      module Of_list_rpc = Of_list_rpc.V3
      module Of_map = Of_map.V2
      module Of_map_rpc = Of_map_rpc.V2
      module Of_map_with_atomic_values = Of_map_with_atomic_values.V1
      module Of_map_with_atomic_values_rpc = Of_map_with_atomic_values_rpc.V1
      module Of_nonempty_list = Of_nonempty_list.V1
      module Of_nonempty_list_rpc = Of_nonempty_list_rpc.V1
      module Of_option = Of_option.V2
      module Of_option_rpc = Of_option_rpc.V2
      module Of_result = Of_result.V1
      module Of_result_rpc = Of_result_rpc.V1
      module Of_sequence = Of_sequence.V1
      module Of_sequence_rpc = Of_sequence_rpc.V1
      module Of_set = Of_set.V3
      module Of_set_rpc = Of_set_rpc.V3
      module Of_sexpable = Of_sexpable.V1
      module Of_streamable = Of_streamable.V1
      module Of_streamable_rpc = Of_streamable_rpc.V1
      module Of_total_map = Of_total_map.V1
      module Of_total_map_rpc = Of_total_map_rpc.V1
      module Of_tuple2 = Of_tuple2.V1
      module Of_tuple2_rpc = Of_tuple2_rpc.V1
      module Of_tuple3 = Of_tuple3.V1
      module Of_tuple3_rpc = Of_tuple3_rpc.V1
      module Of_tuple4 = Of_tuple4.V1
      module Of_tuple4_rpc = Of_tuple4_rpc.V1
      module Of_tuple5 = Of_tuple5.V1
      module Of_tuple5_rpc = Of_tuple5_rpc.V1
      module Of_tuple6 = Of_tuple6.V1
      module Of_tuple6_rpc = Of_tuple6_rpc.V1
      module Of_tuple7 = Of_tuple7.V1
      module Of_tuple7_rpc = Of_tuple7_rpc.V1
      module Of_tuple8 = Of_tuple8.V1
      module Of_tuple8_rpc = Of_tuple8_rpc.V1
      module Of_tuple9 = Of_tuple9.V1
      module Of_tuple9_rpc = Of_tuple9_rpc.V1
      module Of_variant2 = Of_variant2.V1
      module Of_variant2_rpc = Of_variant2_rpc.V1
      module Of_variant3 = Of_variant3.V1
      module Of_variant3_rpc = Of_variant3_rpc.V1
      module Of_variant4 = Of_variant4.V1
      module Of_variant4_rpc = Of_variant4_rpc.V1
      module Of_variant5 = Of_variant5.V1
      module Of_variant5_rpc = Of_variant5_rpc.V1
      module Remove_t = Remove_t
      module Remove_t_rpc = Remove_t_rpc
    end
  end
end
