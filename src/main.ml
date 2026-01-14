open! Core
open! Import
include Main_intf

(* The [S_rpc.Intermediate] interface, used to abbreviate functor definitions below. *)
module type S_intermediate_rpc = sig
  type t

  module Part : sig
    type t [@@deriving bin_io]
  end

  val create : unit -> t
  val apply_part : t -> Part.t -> t
end

(** check compatibility of S_rpc and S_intermediate_rpc *)
module _ (M : S_rpc) : S_intermediate_rpc = M.Intermediate

module _ (M : S_intermediate_rpc) : S_rpc with module Intermediate = M = struct
  type t = unit

  module Intermediate = M

  let to_parts () = Sequence.empty
  let finalize (_ : M.t) = ()
end

(* the subset of S containing only functions, so that one can recursively define modules
   implementing S *)
module type S_only_functions = sig
  type t

  module Intermediate : sig
    type t

    module Part : sig
      type t [@@deriving sexp]

      include Binable.S_only_functions with type t := t
    end

    val create : unit -> t
    val apply_part : t -> Part.t -> t
  end

  val to_parts : t -> Intermediate.Part.t Sequence.t
  val finalize : Intermediate.t -> t
end

module type S_only_functions_rpc = sig
  type t

  module Intermediate : sig
    type t

    module Part : sig
      type t

      include Binable.S_only_functions with type t := t
    end

    val create : unit -> t
    val apply_part : t -> Part.t -> t
  end

  val to_parts : t -> Intermediate.Part.t Sequence.t
  val finalize : Intermediate.t -> t
end

module Stable = struct
  module type S = S
  module type S_rpc = S_rpc
  module type S_rpc_with_sexp_of_part = S_rpc_with_sexp_of_part
  module type S_only_functions = S_only_functions

  module Add_sexp = struct
    module V1 (S : S_rpc) (Part_sexp : Sexpable.S with type t := S.Intermediate.Part.t) :
      S
      with type t = S.t
       and type Intermediate.t = S.Intermediate.t
       and type Intermediate.Part.t = S.Intermediate.Part.t = struct
      include S

      module Intermediate = struct
        include S.Intermediate

        module Part = struct
          include Part
          include Part_sexp
        end
      end
    end
  end

  module Remove_t_rpc = Remove_t.F_rpc
  module Remove_t = Remove_t.F

  module Of_only_functions_rpc = struct
    module V1 (X : S_only_functions_rpc) :
      S_rpc
      with type t := X.t
      with type Intermediate.t = X.Intermediate.t
      with type Intermediate.Part.t = X.Intermediate.Part.t = struct
      module Intermediate = struct
        type t = X.Intermediate.t

        module Part = struct
          type t = X.Intermediate.Part.t [@@deriving bin_read, bin_write]

          let bin_shape_t =
            let open Bin_prot.Shape in
            basetype (Uuid.of_string "859ed728-490a-11e6-a4b5-576fb351e891") []
          ;;

          let bin_t =
            { Bin_prot.Type_class.shape = bin_shape_t
            ; writer = bin_writer_t
            ; reader = bin_reader_t
            }
          ;;
        end

        let create = X.Intermediate.create
        let apply_part = X.Intermediate.apply_part
      end

      let to_parts = X.to_parts
      let finalize = X.finalize
    end
  end

  module Of_only_functions = struct
    module V1 (X : S_only_functions) :
      S
      with type t := X.t
      with type Intermediate.t = X.Intermediate.t
      with type Intermediate.Part.t = X.Intermediate.Part.t =
      Add_sexp.V1
        (struct
          type t = X.t

          include Of_only_functions_rpc.V1 (X)
        end)
        (X.Intermediate.Part)
  end

  module Of_atomic_rpc = struct
    module V1 (A : sig
        type t [@@deriving bin_io]
      end) : S_rpc with type t = A.t and type Intermediate.Part.t = A.t = struct
      type t = A.t

      module Intermediate = struct
        type t = A.t option

        module Part = A

        let create () = None

        let apply_part t a =
          assert (Option.is_none t);
          Some a
        ;;
      end

      let to_parts = Sequence.singleton
      let finalize i = Option.value_exn i
    end
  end

  module Of_atomic = struct
    module V1 (A : sig
        type t [@@deriving bin_io, sexp]
      end) : S with type t = A.t =
      Add_sexp.V1 (Of_atomic_rpc.V1 (A)) (A)
  end

  module Of_streamable_rpc = struct
    module V1
        (Streamable : S_rpc)
        (X : sig
           type t

           val to_streamable : t -> Streamable.t
           val of_streamable : Streamable.t -> t
         end) :
      S_rpc
      with type t = X.t
       and type Intermediate.t = Streamable.Intermediate.t
       and type Intermediate.Part.t = Streamable.Intermediate.Part.t = struct
      type t = X.t

      module Intermediate = Streamable.Intermediate

      let to_parts t = Streamable.to_parts (X.to_streamable t)
      let finalize i = X.of_streamable (Streamable.finalize i)
    end
  end

  module Of_streamable = struct
    module V1
        (Streamable : S)
        (X : sig
           type t

           val to_streamable : t -> Streamable.t
           val of_streamable : Streamable.t -> t
         end) : S with type t = X.t =
      Add_sexp.V1 (Of_streamable_rpc.V1 (Streamable) (X)) (Streamable.Intermediate.Part)
  end

  module Checked
      (Limit : sig
         val max_intermediate_part_bin_size : int
         val here : Source_code_position.t
       end)
      (X : S) : S with type t = X.t = struct
    include X

    let here = Limit.here
    let max_part_bin_size = Limit.max_intermediate_part_bin_size

    let to_parts t =
      X.to_parts t
      |> Sequence.mapi ~f:(fun part_index part ->
        let the_part_bin_size = Intermediate.Part.bin_size_t part in
        if the_part_bin_size > max_part_bin_size
        then (
          let intermediate_part_bin_shape : Sexp.t =
            (* Bin_shape.t sexps contain a global counter on bin shapes, which is too
               flaky to display in test output *)
            if am_running_test
            then [%sexp "{omitted-in-test}"]
            else [%sexp (Intermediate.Part.bin_shape_t : Bin_shape.t)]
          in
          raise_s
            [%message
              "Streamable intermediate part exceeded size threshold.  Depending on the \
               max size, this might indicate that serialization or transmission will \
               fail."
                (here : Source_code_position.t)
                (part_index : int)
                (the_part_bin_size : int)
                (max_part_bin_size : int)
                (intermediate_part_bin_shape : Sexp.t)]);
        part)
    ;;
  end

  module Packed_rpc = struct
    module V1 (X : S_rpc) : sig
      type t = X.t

      module Part : sig
        type t =
          { parts : X.Intermediate.Part.t Fqueue.t
          ; bin_size : int option
          }
      end

      include
        S_rpc
        with type t := t
         and type Intermediate.t = X.Intermediate.t
         and type Intermediate.Part.t = Part.t
    end = struct
      type t = X.t

      module Part = struct
        module Format = struct
          type t = X.Intermediate.Part.t Fqueue.t [@@deriving bin_io]
        end

        module T = struct
          type t =
            { parts : Format.t
            ; bin_size : int option
            (** [bin_size] is a cached bin_size that we compute when we produce [t],
                otherwise we end up calling [bin_size] on the parts twice unnecessarily.
                It's optional because if [t] is produced by deserializing a sexp, we won't
                know the bin size, so we have to fall back to the regular size computation
                in that case. *)
            }

          let bin_shape_t = Format.bin_shape_t

          let bin_size_t t =
            match t.bin_size with
            | None -> Format.bin_size_t t.parts
            | Some size -> size
          ;;

          let bin_write_t buf ~pos t = Format.bin_write_t buf ~pos t.parts

          let bin_read_t buf ~pos_ref =
            let start_pos = !pos_ref in
            let parts = Format.bin_read_t buf ~pos_ref in
            let bin_size = !pos_ref - start_pos in
            { parts; bin_size = Some bin_size }
          ;;

          let __bin_read_t__ (_ : Bigstring.t) ~pos_ref:(_ : int ref) (_ : int) =
            failwith
              "vtag_read not implemented for Streamable.Packed_rpc.Intermediate.Part"
          ;;
        end

        include T
        include Bin_prot.Utils.Of_minimal (T)
      end

      module Intermediate = struct
        type t = X.Intermediate.t

        module Part = Part

        let create = X.Intermediate.create

        let apply_part inter (part : Part.t) =
          Fqueue.fold part.parts ~init:inter ~f:X.Intermediate.apply_part
        ;;
      end

      (* Practice shows that 2^18 is not too high, but (as of 2016-08-11) we shouldn't
         exceed 2^18 due to some limitations in writer.ml. *)
      let pack_threshold =
        if Ppx_inline_test_lib.am_running
        then 25 (* something small enough to test both sides of easily *)
        else 131_072
      ;;

      (* 2^17 gives us some room to grow in outer parts *)

      let to_parts t =
        let estimated_header_length = 1 + Bin_prot.Utils.size_header_length in
        (* serialized length of parts in the fqueue (must be exactly correct) *)
        let init = Fqueue.empty, 0 in
        let part_from_state parts total_bin_size : Intermediate.Part.t =
          let part_bin_size =
            (* The serialization format for [Fqueue] is the length encoded as a bin prot
               int, and then each of the serializations of the individual parts. *)
            total_bin_size + bin_size_int (Fqueue.length parts)
          in
          { parts; bin_size = Some part_bin_size }
        in
        Sequence.unfold_with_and_finish
          (X.to_parts t)
          ~init
          ~running_step:(fun (buffered_parts, buffered_len) xpart ->
            let new_parts = Fqueue.enqueue buffered_parts xpart in
            let new_len = buffered_len + X.Intermediate.Part.bin_size_t xpart in
            if new_len >= pack_threshold - estimated_header_length
            then Yield { value = part_from_state new_parts new_len; state = init }
            else Skip { state = new_parts, new_len })
          ~inner_finished:Fn.id
          ~finishing_step:(fun (buffered_parts, buffered_len) ->
            if Fqueue.is_empty buffered_parts
            then Done
            else
              Yield { value = part_from_state buffered_parts buffered_len; state = init })
      ;;

      let finalize = X.finalize
    end
  end

  module Packed = struct
    module V1 (X : S) : sig
      type t = X.t

      include S with type t := t
    end = struct
      module T = Packed_rpc.V1 (X)

      include
        Add_sexp.V1
          (T)
          (struct
            type t = T.Part.t =
              { parts : X.Intermediate.Part.t Fqueue.t
              ; bin_size : int option
              }

            let sexp_of_t t = [%sexp_of: X.Intermediate.Part.t Fqueue.t] t.parts

            let t_of_sexp sexp =
              { parts = [%of_sexp: X.Intermediate.Part.t Fqueue.t] sexp; bin_size = None }
            ;;
          end)
    end
  end

  module Of_key_value_intermediate_part = struct
    module Make (Key : Stable_without_of_sexp) (Data : S_rpc) = struct
      type t =
        | Add_key of Key.t
        | Add_part of Data.Intermediate.Part.t
      [@@deriving bin_io, variants]
    end
  end

  module Of_key_value_store_rpc = struct
    module V1
        (Key : Stable_without_of_sexp)
        (Data : S_rpc)
        (Store : sig
           type t

           val create : unit -> t
           val mem : t -> Key.t -> bool
           val set : t -> key:Key.t -> data:Data.t -> t
           val to_sequence : t -> (Key.t * Data.t) Sequence.t
         end) =
    struct
      type t = Store.t

      module Intermediate = struct
        type store = t

        type nonrec t =
          (* zipper like thing *)
          | Empty
          | At_key of store * Key.t * Data.Intermediate.t

        let to_store = function
          | Empty -> Store.create ()
          | At_key (store, key, int) -> Store.set store ~key ~data:(Data.finalize int)
        ;;

        module Part = Of_key_value_intermediate_part.Make (Key) (Data)

        let create () = Empty

        let apply_part t part =
          match (part : Part.t) with
          | Add_key key ->
            let store = to_store t in
            assert (not (Store.mem store key));
            At_key (store, key, Data.Intermediate.create ())
          | Add_part part ->
            (match t with
             | Empty -> assert false
             | At_key (store, key, int) ->
               At_key (store, key, Data.Intermediate.apply_part int part))
        ;;
      end

      let finalize = Intermediate.to_store

      let to_parts t =
        Store.to_sequence t
        |> Sequence.concat_map ~f:(fun (key, data) ->
          Sequence.cons
            (Intermediate.Part.add_key key)
            (Sequence.map ~f:Intermediate.Part.add_part (Data.to_parts data)))
      ;;
    end
  end

  module Of_key_value_store = struct
    module V1
        (Key : Stable)
        (Data : S)
        (Store : sig
           type t

           val create : unit -> t
           val mem : t -> Key.t -> bool
           val set : t -> key:Key.t -> data:Data.t -> t
           val to_sequence : t -> (Key.t * Data.t) Sequence.t
         end) =
    struct
      module Plain = Of_key_value_store_rpc.V1 (Key) (Data) (Store)

      include
        Add_sexp.V1
          (Plain)
          (struct
            type t = Plain.Intermediate.Part.t =
              | Add_key of Key.t
              | Add_part of Data.Intermediate.Part.t
            [@@deriving sexp]
          end)
    end
  end

  module Of_key_value_store_with_atomic_values_rpc = struct
    module V1
        (Key : Stable_without_of_sexp)
        (Data : sig
           type t

           include Binable.S with type t := t
         end)
        (Store : sig
           type t

           val create : unit -> t
           val set : t -> key:Key.t -> data:Data.t -> t
           val to_sequence : t -> (Key.t * Data.t) Sequence.t
         end) =
    struct
      type t = Store.t

      module Intermediate = struct
        type nonrec t = t

        module Part = struct
          type t = Key.t * Data.t [@@deriving bin_io]
        end

        let create () = Store.create ()
        let apply_part t (key, data) = Store.set t ~key ~data
      end

      let to_parts = Store.to_sequence
      let finalize = Fn.id
    end
  end

  module Of_key_value_store_with_atomic_values = struct
    module V1
        (Key : Stable)
        (Data : sig
           type t [@@deriving bin_io, sexp]
         end)
        (Store : sig
           type t

           val create : unit -> t
           val set : t -> key:Key.t -> data:Data.t -> t
           val to_sequence : t -> (Key.t * Data.t) Sequence.t
         end) =
    struct
      module Plain = Of_key_value_store_with_atomic_values_rpc.V1 (Key) (Data) (Store)

      include
        Add_sexp.V1
          (Plain)
          (struct
            type t = Key.t * Data.t [@@deriving sexp]
          end)
    end
  end

  module Map_store (Key : Stable_without_of_sexp) (Data : T) = struct
    type t = Data.t Map.M(Key).t

    let create () = Map.empty (module Key)
    let mem = Map.mem
    let set = Map.set
    let to_sequence t = Map.to_sequence t
  end

  module Of_map_rpc = struct
    module V1 (Key : Stable_without_of_sexp) (Data : S_rpc) : sig
      type t = Data.t Map.M(Key).t

      include S_rpc with type t := t
    end = struct
      include Of_key_value_store_rpc.V1 (Key) (Data) (Map_store (Key) (Data))
    end

    module V2 (Key : Stable_without_of_sexp) (Data : S_rpc) =
      Packed_rpc.V1 (V1 (Key) (Data))
  end

  module Of_map = struct
    module V1 (Key : Stable) (Data : S) : sig
      type t = (Key.t, Data.t, Key.comparator_witness) Map.t

      include S with type t := t
    end = struct
      include Of_key_value_store.V1 (Key) (Data) (Map_store (Key) (Data))
    end

    module V2 (Key : Stable) (Data : S) = Packed.V1 (V1 (Key) (Data))
  end

  module Of_map_with_atomic_values_rpc = struct
    module V1_unpacked
        (Key : Stable_without_of_sexp)
        (Data : sig
           type t

           include Binable.S with type t := t
         end) : sig
      type t = (Key.t, Data.t, Key.comparator_witness) Map.t

      include S_rpc with type t := t
    end =
      Of_key_value_store_with_atomic_values_rpc.V1 (Key) (Data) (Map_store (Key) (Data))

    module V1
        (Key : Stable_without_of_sexp)
        (Data : sig
           type t

           include Binable.S with type t := t
         end) =
      Packed_rpc.V1 (V1_unpacked (Key) (Data))
  end

  module Of_map_with_atomic_values = struct
    module V1_unpacked
        (Key : Stable)
        (Data : sig
           type t [@@deriving bin_io, sexp]
         end) : sig
      type t = (Key.t, Data.t, Key.comparator_witness) Map.t

      include S with type t := t
    end =
      Of_key_value_store_with_atomic_values.V1 (Key) (Data) (Map_store (Key) (Data))

    module V1
        (Key : Stable)
        (Data : sig
           type t [@@deriving bin_io, sexp]
         end) =
      Packed.V1 (V1_unpacked (Key) (Data))
  end

  module Of_total_map = struct
    module V1 (Key : Total_map.Key_with_witnesses) (Data : S) = struct
      module Key_total_map = Total_map.Make_with_witnesses (Key)

      include
        Of_streamable.V1
          (Of_map.V2 (Key) (Data))
             (struct
               type t = Data.t Key_total_map.t

               let to_streamable = Total_map.to_map

               let of_streamable map =
                 Key_total_map.create (fun key -> Map.find_exn map key)
               ;;
             end)
    end
  end

  module Of_total_map_rpc = struct
    module V1 (Key : Total_map.Key_with_witnesses) (Data : S_rpc) = struct
      module Key_total_map = Total_map.Make_with_witnesses (Key)

      include
        Of_streamable_rpc.V1
          (Of_map_rpc.V2 (Key) (Data))
             (struct
               type t = Data.t Key_total_map.t

               let to_streamable = Total_map.to_map

               let of_streamable map =
                 Key_total_map.create (fun key -> Map.find_exn map key)
               ;;
             end)
    end
  end

  module Hashtbl_store
      (Key : sig
         include Stable_without_of_sexp
         include Hashtbl.Key_plain with type t := t
       end)
      (Data : S_rpc) =
  struct
    type t = (Key.t, Data.t) Hashtbl.t

    let create () = Hashtbl.create (module Key)
    let mem = Hashtbl.mem

    let set t ~key ~data =
      Hashtbl.set t ~key ~data;
      t
    ;;

    let to_sequence t = Hashtbl.to_alist t |> Sequence.of_list
  end

  module Of_hashtbl_rpc = struct
    module V1_unpacked
        (Key : sig
           include Stable_without_of_sexp
           include Hashtbl.Key_plain with type t := t
         end)
        (Data : S_rpc) : sig
      type t = (Key.t, Data.t) Hashtbl.t

      include S_rpc with type t := t
    end = struct
      include Of_key_value_store_rpc.V1 (Key) (Data) (Hashtbl_store (Key) (Data))
    end

    module V1
        (Key : sig
           include Hashtbl.Key_plain
           include Stable_without_of_sexp with type t := t
         end)
        (Data : S_rpc) =
      Packed_rpc.V1 (V1_unpacked (Key) (Data))
  end

  module Of_hashtbl = struct
    module V1_unpacked
        (Key : sig
           include Stable
           include Hashtbl.Key with type t := t
         end)
        (Data : S) : sig
      type t = (Key.t, Data.t) Hashtbl.t

      include S with type t := t
    end = struct
      include Of_key_value_store.V1 (Key) (Data) (Hashtbl_store (Key) (Data))
    end

    module V1
        (Key : sig
           include Hashtbl.Key
           include Stable with type t := t
         end)
        (Data : S) =
      Packed.V1 (V1_unpacked (Key) (Data))
  end

  module Of_tuple2_rpc = struct
    module V1 (A : S_rpc) (B : S_rpc) : sig
      type t = A.t * B.t

      module Intermediate : sig
        module Part : sig
          type t =
            | A of A.Intermediate.Part.t
            | B of B.Intermediate.Part.t
          [@@deriving bin_io]
        end

        include S_intermediate_rpc with module Part := Part
      end

      include S_rpc with type t := t and module Intermediate := Intermediate
    end = struct
      type t = A.t * B.t

      module Intermediate = struct
        type t = A.Intermediate.t * B.Intermediate.t

        module Part = struct
          type t =
            | A of A.Intermediate.Part.t
            | B of B.Intermediate.Part.t
          [@@deriving bin_io, variants]
        end

        let create () = A.Intermediate.create (), B.Intermediate.create ()

        let apply_part (a, b) = function
          | Part.A x -> A.Intermediate.apply_part a x, b
          | Part.B x -> a, B.Intermediate.apply_part b x
        ;;
      end

      let to_parts (a, b) =
        Sequence.concat
        @@ Sequence.of_list
             [ Sequence.map ~f:Intermediate.Part.a (A.to_parts a)
             ; Sequence.map ~f:Intermediate.Part.b (B.to_parts b)
             ]
      ;;

      let finalize (a, b) = A.finalize a, B.finalize b
    end
  end

  module Of_tuple2 = struct
    module V1 (A : S) (B : S) : sig
      type t = A.t * B.t

      include S with type t := t
    end = struct
      module Plain = Of_tuple2_rpc.V1 (A) (B)

      include
        Add_sexp.V1
          (Plain)
          (struct
            type t = Plain.Intermediate.Part.t =
              | A of A.Intermediate.Part.t
              | B of B.Intermediate.Part.t
            [@@deriving sexp]
          end)
    end
  end

  module Of_tuple3_rpc = struct
    module V1 (A : S_rpc) (B : S_rpc) (C : S_rpc) : sig
      type t = A.t * B.t * C.t

      module Intermediate : sig
        module Part : sig
          type t =
            | A of A.Intermediate.Part.t
            | B of B.Intermediate.Part.t
            | C of C.Intermediate.Part.t
          [@@deriving bin_io]
        end

        include S_intermediate_rpc with module Part := Part
      end

      include S_rpc with type t := t and module Intermediate := Intermediate
    end = struct
      type t = A.t * B.t * C.t

      module Intermediate = struct
        type t = A.Intermediate.t * B.Intermediate.t * C.Intermediate.t

        module Part = struct
          type t =
            | A of A.Intermediate.Part.t
            | B of B.Intermediate.Part.t
            | C of C.Intermediate.Part.t
          [@@deriving bin_io, variants]
        end

        let create () =
          A.Intermediate.create (), B.Intermediate.create (), C.Intermediate.create ()
        ;;

        let apply_part (a, b, c) = function
          | Part.A x -> A.Intermediate.apply_part a x, b, c
          | Part.B x -> a, B.Intermediate.apply_part b x, c
          | Part.C x -> a, b, C.Intermediate.apply_part c x
        ;;
      end

      let to_parts (a, b, c) =
        Sequence.concat
        @@ Sequence.of_list
             [ Sequence.map ~f:Intermediate.Part.a (A.to_parts a)
             ; Sequence.map ~f:Intermediate.Part.b (B.to_parts b)
             ; Sequence.map ~f:Intermediate.Part.c (C.to_parts c)
             ]
      ;;

      let finalize (a, b, c) = A.finalize a, B.finalize b, C.finalize c
    end
  end

  module Of_tuple3 = struct
    module V1 (A : S) (B : S) (C : S) : sig
      type t = A.t * B.t * C.t

      include S with type t := t
    end = struct
      module Plain = Of_tuple3_rpc.V1 (A) (B) (C)

      include
        Add_sexp.V1
          (Plain)
          (struct
            type t = Plain.Intermediate.Part.t =
              | A of A.Intermediate.Part.t
              | B of B.Intermediate.Part.t
              | C of C.Intermediate.Part.t
            [@@deriving sexp]
          end)
    end
  end

  module Of_tuple4_rpc = struct
    module V1 (A : S_rpc) (B : S_rpc) (C : S_rpc) (D : S_rpc) : sig
      type t = A.t * B.t * C.t * D.t

      module Intermediate : sig
        module Part : sig
          type t =
            | A of A.Intermediate.Part.t
            | B of B.Intermediate.Part.t
            | C of C.Intermediate.Part.t
            | D of D.Intermediate.Part.t
          [@@deriving bin_io]
        end

        include S_intermediate_rpc with module Part := Part
      end

      include S_rpc with type t := t and module Intermediate := Intermediate
    end = struct
      type t = A.t * B.t * C.t * D.t

      module Intermediate = struct
        type t = A.Intermediate.t * B.Intermediate.t * C.Intermediate.t * D.Intermediate.t

        module Part = struct
          type t =
            | A of A.Intermediate.Part.t
            | B of B.Intermediate.Part.t
            | C of C.Intermediate.Part.t
            | D of D.Intermediate.Part.t
          [@@deriving bin_io, variants]
        end

        let create () =
          ( A.Intermediate.create ()
          , B.Intermediate.create ()
          , C.Intermediate.create ()
          , D.Intermediate.create () )
        ;;

        let apply_part (a, b, c, d) = function
          | Part.A x -> A.Intermediate.apply_part a x, b, c, d
          | Part.B x -> a, B.Intermediate.apply_part b x, c, d
          | Part.C x -> a, b, C.Intermediate.apply_part c x, d
          | Part.D x -> a, b, c, D.Intermediate.apply_part d x
        ;;
      end

      let to_parts (a, b, c, d) =
        Sequence.concat
        @@ Sequence.of_list
             [ Sequence.map ~f:Intermediate.Part.a (A.to_parts a)
             ; Sequence.map ~f:Intermediate.Part.b (B.to_parts b)
             ; Sequence.map ~f:Intermediate.Part.c (C.to_parts c)
             ; Sequence.map ~f:Intermediate.Part.d (D.to_parts d)
             ]
      ;;

      let finalize (a, b, c, d) = A.finalize a, B.finalize b, C.finalize c, D.finalize d
    end
  end

  module Of_tuple4 = struct
    module V1 (A : S) (B : S) (C : S) (D : S) : sig
      type t = A.t * B.t * C.t * D.t

      include S with type t := t
    end = struct
      module Plain = Of_tuple4_rpc.V1 (A) (B) (C) (D)

      include
        Add_sexp.V1
          (Plain)
          (struct
            type t = Plain.Intermediate.Part.t =
              | A of A.Intermediate.Part.t
              | B of B.Intermediate.Part.t
              | C of C.Intermediate.Part.t
              | D of D.Intermediate.Part.t
            [@@deriving sexp]
          end)
    end
  end

  module Of_tuple5_rpc = struct
    module V1 (A : S_rpc) (B : S_rpc) (C : S_rpc) (D : S_rpc) (E : S_rpc) : sig
      type t = A.t * B.t * C.t * D.t * E.t

      module Intermediate : sig
        module Part : sig
          type t =
            | A of A.Intermediate.Part.t
            | B of B.Intermediate.Part.t
            | C of C.Intermediate.Part.t
            | D of D.Intermediate.Part.t
            | E of E.Intermediate.Part.t
          [@@deriving bin_io]
        end

        include S_intermediate_rpc with module Part := Part
      end

      include S_rpc with type t := t and module Intermediate := Intermediate
    end = struct
      type t = A.t * B.t * C.t * D.t * E.t

      module Intermediate = struct
        type t =
          A.Intermediate.t
          * B.Intermediate.t
          * C.Intermediate.t
          * D.Intermediate.t
          * E.Intermediate.t

        module Part = struct
          type t =
            | A of A.Intermediate.Part.t
            | B of B.Intermediate.Part.t
            | C of C.Intermediate.Part.t
            | D of D.Intermediate.Part.t
            | E of E.Intermediate.Part.t
          [@@deriving bin_io, variants]
        end

        let create () =
          ( A.Intermediate.create ()
          , B.Intermediate.create ()
          , C.Intermediate.create ()
          , D.Intermediate.create ()
          , E.Intermediate.create () )
        ;;

        let apply_part (a, b, c, d, e) = function
          | Part.A x -> A.Intermediate.apply_part a x, b, c, d, e
          | Part.B x -> a, B.Intermediate.apply_part b x, c, d, e
          | Part.C x -> a, b, C.Intermediate.apply_part c x, d, e
          | Part.D x -> a, b, c, D.Intermediate.apply_part d x, e
          | Part.E x -> a, b, c, d, E.Intermediate.apply_part e x
        ;;
      end

      let to_parts (a, b, c, d, e) =
        Sequence.concat
        @@ Sequence.of_list
             [ Sequence.map ~f:Intermediate.Part.a (A.to_parts a)
             ; Sequence.map ~f:Intermediate.Part.b (B.to_parts b)
             ; Sequence.map ~f:Intermediate.Part.c (C.to_parts c)
             ; Sequence.map ~f:Intermediate.Part.d (D.to_parts d)
             ; Sequence.map ~f:Intermediate.Part.e (E.to_parts e)
             ]
      ;;

      let finalize (a, b, c, d, e) =
        A.finalize a, B.finalize b, C.finalize c, D.finalize d, E.finalize e
      ;;
    end
  end

  module Of_tuple5 = struct
    module V1 (A : S) (B : S) (C : S) (D : S) (E : S) : sig
      type t = A.t * B.t * C.t * D.t * E.t

      include S with type t := t
    end = struct
      module Plain = Of_tuple5_rpc.V1 (A) (B) (C) (D) (E)

      include
        Add_sexp.V1
          (Plain)
          (struct
            type t = Plain.Intermediate.Part.t =
              | A of A.Intermediate.Part.t
              | B of B.Intermediate.Part.t
              | C of C.Intermediate.Part.t
              | D of D.Intermediate.Part.t
              | E of E.Intermediate.Part.t
            [@@deriving sexp]
          end)
    end
  end

  module Of_tuple6_rpc = struct
    module V1
        (A : S_rpc)
        (B : S_rpc)
        (C : S_rpc)
        (D : S_rpc)
        (E : S_rpc)
        (F : S_rpc) : sig
      type t = A.t * B.t * C.t * D.t * E.t * F.t

      module Intermediate : sig
        module Part : sig
          type t =
            | A of A.Intermediate.Part.t
            | B of B.Intermediate.Part.t
            | C of C.Intermediate.Part.t
            | D of D.Intermediate.Part.t
            | E of E.Intermediate.Part.t
            | F of F.Intermediate.Part.t
          [@@deriving bin_io]
        end

        include S_intermediate_rpc with module Part := Part
      end

      include S_rpc with type t := t and module Intermediate := Intermediate
    end = struct
      type t = A.t * B.t * C.t * D.t * E.t * F.t

      module Intermediate = struct
        type t =
          A.Intermediate.t
          * B.Intermediate.t
          * C.Intermediate.t
          * D.Intermediate.t
          * E.Intermediate.t
          * F.Intermediate.t

        module Part = struct
          type t =
            | A of A.Intermediate.Part.t
            | B of B.Intermediate.Part.t
            | C of C.Intermediate.Part.t
            | D of D.Intermediate.Part.t
            | E of E.Intermediate.Part.t
            | F of F.Intermediate.Part.t
          [@@deriving bin_io, variants]
        end

        let create () =
          ( A.Intermediate.create ()
          , B.Intermediate.create ()
          , C.Intermediate.create ()
          , D.Intermediate.create ()
          , E.Intermediate.create ()
          , F.Intermediate.create () )
        ;;

        let apply_part (a, b, c, d, e, f) = function
          | Part.A x -> A.Intermediate.apply_part a x, b, c, d, e, f
          | Part.B x -> a, B.Intermediate.apply_part b x, c, d, e, f
          | Part.C x -> a, b, C.Intermediate.apply_part c x, d, e, f
          | Part.D x -> a, b, c, D.Intermediate.apply_part d x, e, f
          | Part.E x -> a, b, c, d, E.Intermediate.apply_part e x, f
          | Part.F x -> a, b, c, d, e, F.Intermediate.apply_part f x
        ;;
      end

      let to_parts (a, b, c, d, e, f) =
        Sequence.concat
        @@ Sequence.of_list
             [ Sequence.map ~f:Intermediate.Part.a (A.to_parts a)
             ; Sequence.map ~f:Intermediate.Part.b (B.to_parts b)
             ; Sequence.map ~f:Intermediate.Part.c (C.to_parts c)
             ; Sequence.map ~f:Intermediate.Part.d (D.to_parts d)
             ; Sequence.map ~f:Intermediate.Part.e (E.to_parts e)
             ; Sequence.map ~f:Intermediate.Part.f (F.to_parts f)
             ]
      ;;

      let finalize (a, b, c, d, e, f) =
        A.finalize a, B.finalize b, C.finalize c, D.finalize d, E.finalize e, F.finalize f
      ;;
    end
  end

  module Of_tuple6 = struct
    module V1 (A : S) (B : S) (C : S) (D : S) (E : S) (F : S) : sig
      type t = A.t * B.t * C.t * D.t * E.t * F.t

      include S with type t := t
    end = struct
      module Plain = Of_tuple6_rpc.V1 (A) (B) (C) (D) (E) (F)

      include
        Add_sexp.V1
          (Plain)
          (struct
            type t = Plain.Intermediate.Part.t =
              | A of A.Intermediate.Part.t
              | B of B.Intermediate.Part.t
              | C of C.Intermediate.Part.t
              | D of D.Intermediate.Part.t
              | E of E.Intermediate.Part.t
              | F of F.Intermediate.Part.t
            [@@deriving sexp]
          end)
    end
  end

  module Of_tuple7_rpc = struct
    module V1
        (A : S_rpc)
        (B : S_rpc)
        (C : S_rpc)
        (D : S_rpc)
        (E : S_rpc)
        (F : S_rpc)
        (G : S_rpc) : sig
      type t = A.t * B.t * C.t * D.t * E.t * F.t * G.t

      module Intermediate : sig
        module Part : sig
          type t =
            | A of A.Intermediate.Part.t
            | B of B.Intermediate.Part.t
            | C of C.Intermediate.Part.t
            | D of D.Intermediate.Part.t
            | E of E.Intermediate.Part.t
            | F of F.Intermediate.Part.t
            | G of G.Intermediate.Part.t
          [@@deriving bin_io]
        end

        include S_intermediate_rpc with module Part := Part
      end

      include S_rpc with type t := t and module Intermediate := Intermediate
    end = struct
      type t = A.t * B.t * C.t * D.t * E.t * F.t * G.t

      module Intermediate = struct
        type t =
          A.Intermediate.t
          * B.Intermediate.t
          * C.Intermediate.t
          * D.Intermediate.t
          * E.Intermediate.t
          * F.Intermediate.t
          * G.Intermediate.t

        module Part = struct
          type t =
            | A of A.Intermediate.Part.t
            | B of B.Intermediate.Part.t
            | C of C.Intermediate.Part.t
            | D of D.Intermediate.Part.t
            | E of E.Intermediate.Part.t
            | F of F.Intermediate.Part.t
            | G of G.Intermediate.Part.t
          [@@deriving bin_io, variants]
        end

        let create () =
          ( A.Intermediate.create ()
          , B.Intermediate.create ()
          , C.Intermediate.create ()
          , D.Intermediate.create ()
          , E.Intermediate.create ()
          , F.Intermediate.create ()
          , G.Intermediate.create () )
        ;;

        let apply_part (a, b, c, d, e, f, g) = function
          | Part.A x -> A.Intermediate.apply_part a x, b, c, d, e, f, g
          | Part.B x -> a, B.Intermediate.apply_part b x, c, d, e, f, g
          | Part.C x -> a, b, C.Intermediate.apply_part c x, d, e, f, g
          | Part.D x -> a, b, c, D.Intermediate.apply_part d x, e, f, g
          | Part.E x -> a, b, c, d, E.Intermediate.apply_part e x, f, g
          | Part.F x -> a, b, c, d, e, F.Intermediate.apply_part f x, g
          | Part.G x -> a, b, c, d, e, f, G.Intermediate.apply_part g x
        ;;
      end

      let to_parts (a, b, c, d, e, f, g) =
        Sequence.concat
        @@ Sequence.of_list
             [ Sequence.map ~f:Intermediate.Part.a (A.to_parts a)
             ; Sequence.map ~f:Intermediate.Part.b (B.to_parts b)
             ; Sequence.map ~f:Intermediate.Part.c (C.to_parts c)
             ; Sequence.map ~f:Intermediate.Part.d (D.to_parts d)
             ; Sequence.map ~f:Intermediate.Part.e (E.to_parts e)
             ; Sequence.map ~f:Intermediate.Part.f (F.to_parts f)
             ; Sequence.map ~f:Intermediate.Part.g (G.to_parts g)
             ]
      ;;

      let finalize (a, b, c, d, e, f, g) =
        ( A.finalize a
        , B.finalize b
        , C.finalize c
        , D.finalize d
        , E.finalize e
        , F.finalize f
        , G.finalize g )
      ;;
    end
  end

  module Of_tuple7 = struct
    module V1 (A : S) (B : S) (C : S) (D : S) (E : S) (F : S) (G : S) : sig
      type t = A.t * B.t * C.t * D.t * E.t * F.t * G.t

      include S with type t := t
    end = struct
      module Plain = Of_tuple7_rpc.V1 (A) (B) (C) (D) (E) (F) (G)

      include
        Add_sexp.V1
          (Plain)
          (struct
            type t = Plain.Intermediate.Part.t =
              | A of A.Intermediate.Part.t
              | B of B.Intermediate.Part.t
              | C of C.Intermediate.Part.t
              | D of D.Intermediate.Part.t
              | E of E.Intermediate.Part.t
              | F of F.Intermediate.Part.t
              | G of G.Intermediate.Part.t
            [@@deriving sexp]
          end)
    end
  end

  module Of_tuple8_rpc = struct
    module V1
        (A : S_rpc)
        (B : S_rpc)
        (C : S_rpc)
        (D : S_rpc)
        (E : S_rpc)
        (F : S_rpc)
        (G : S_rpc)
        (H : S_rpc) : sig
      type t = A.t * B.t * C.t * D.t * E.t * F.t * G.t * H.t

      module Intermediate : sig
        module Part : sig
          type t =
            | A of A.Intermediate.Part.t
            | B of B.Intermediate.Part.t
            | C of C.Intermediate.Part.t
            | D of D.Intermediate.Part.t
            | E of E.Intermediate.Part.t
            | F of F.Intermediate.Part.t
            | G of G.Intermediate.Part.t
            | H of H.Intermediate.Part.t
          [@@deriving bin_io]
        end

        include S_intermediate_rpc with module Part := Part
      end

      include S_rpc with type t := t and module Intermediate := Intermediate
    end = struct
      type t = A.t * B.t * C.t * D.t * E.t * F.t * G.t * H.t

      module Intermediate = struct
        type t =
          A.Intermediate.t
          * B.Intermediate.t
          * C.Intermediate.t
          * D.Intermediate.t
          * E.Intermediate.t
          * F.Intermediate.t
          * G.Intermediate.t
          * H.Intermediate.t

        module Part = struct
          type t =
            | A of A.Intermediate.Part.t
            | B of B.Intermediate.Part.t
            | C of C.Intermediate.Part.t
            | D of D.Intermediate.Part.t
            | E of E.Intermediate.Part.t
            | F of F.Intermediate.Part.t
            | G of G.Intermediate.Part.t
            | H of H.Intermediate.Part.t
          [@@deriving bin_io, variants]
        end

        let create () =
          ( A.Intermediate.create ()
          , B.Intermediate.create ()
          , C.Intermediate.create ()
          , D.Intermediate.create ()
          , E.Intermediate.create ()
          , F.Intermediate.create ()
          , G.Intermediate.create ()
          , H.Intermediate.create () )
        ;;

        let apply_part (a, b, c, d, e, f, g, h) = function
          | Part.A x -> A.Intermediate.apply_part a x, b, c, d, e, f, g, h
          | Part.B x -> a, B.Intermediate.apply_part b x, c, d, e, f, g, h
          | Part.C x -> a, b, C.Intermediate.apply_part c x, d, e, f, g, h
          | Part.D x -> a, b, c, D.Intermediate.apply_part d x, e, f, g, h
          | Part.E x -> a, b, c, d, E.Intermediate.apply_part e x, f, g, h
          | Part.F x -> a, b, c, d, e, F.Intermediate.apply_part f x, g, h
          | Part.G x -> a, b, c, d, e, f, G.Intermediate.apply_part g x, h
          | Part.H x -> a, b, c, d, e, f, g, H.Intermediate.apply_part h x
        ;;
      end

      let to_parts (a, b, c, d, e, f, g, h) =
        Sequence.concat
        @@ Sequence.of_list
             [ Sequence.map ~f:Intermediate.Part.a (A.to_parts a)
             ; Sequence.map ~f:Intermediate.Part.b (B.to_parts b)
             ; Sequence.map ~f:Intermediate.Part.c (C.to_parts c)
             ; Sequence.map ~f:Intermediate.Part.d (D.to_parts d)
             ; Sequence.map ~f:Intermediate.Part.e (E.to_parts e)
             ; Sequence.map ~f:Intermediate.Part.f (F.to_parts f)
             ; Sequence.map ~f:Intermediate.Part.g (G.to_parts g)
             ; Sequence.map ~f:Intermediate.Part.h (H.to_parts h)
             ]
      ;;

      let finalize (a, b, c, d, e, f, g, h) =
        ( A.finalize a
        , B.finalize b
        , C.finalize c
        , D.finalize d
        , E.finalize e
        , F.finalize f
        , G.finalize g
        , H.finalize h )
      ;;
    end
  end

  module Of_tuple8 = struct
    module V1 (A : S) (B : S) (C : S) (D : S) (E : S) (F : S) (G : S) (H : S) : sig
      type t = A.t * B.t * C.t * D.t * E.t * F.t * G.t * H.t

      include S with type t := t
    end = struct
      module Plain = Of_tuple8_rpc.V1 (A) (B) (C) (D) (E) (F) (G) (H)

      include
        Add_sexp.V1
          (Plain)
          (struct
            type t = Plain.Intermediate.Part.t =
              | A of A.Intermediate.Part.t
              | B of B.Intermediate.Part.t
              | C of C.Intermediate.Part.t
              | D of D.Intermediate.Part.t
              | E of E.Intermediate.Part.t
              | F of F.Intermediate.Part.t
              | G of G.Intermediate.Part.t
              | H of H.Intermediate.Part.t
            [@@deriving sexp]
          end)
    end
  end

  module Of_tuple9_rpc = struct
    module V1
        (A : S_rpc)
        (B : S_rpc)
        (C : S_rpc)
        (D : S_rpc)
        (E : S_rpc)
        (F : S_rpc)
        (G : S_rpc)
        (H : S_rpc)
        (I : S_rpc) : sig
      type t = A.t * B.t * C.t * D.t * E.t * F.t * G.t * H.t * I.t

      module Intermediate : sig
        module Part : sig
          type t =
            | A of A.Intermediate.Part.t
            | B of B.Intermediate.Part.t
            | C of C.Intermediate.Part.t
            | D of D.Intermediate.Part.t
            | E of E.Intermediate.Part.t
            | F of F.Intermediate.Part.t
            | G of G.Intermediate.Part.t
            | H of H.Intermediate.Part.t
            | I of I.Intermediate.Part.t
          [@@deriving bin_io]
        end

        include S_intermediate_rpc with module Part := Part
      end

      include S_rpc with type t := t and module Intermediate := Intermediate
    end = struct
      type t = A.t * B.t * C.t * D.t * E.t * F.t * G.t * H.t * I.t

      module Intermediate = struct
        type t =
          A.Intermediate.t
          * B.Intermediate.t
          * C.Intermediate.t
          * D.Intermediate.t
          * E.Intermediate.t
          * F.Intermediate.t
          * G.Intermediate.t
          * H.Intermediate.t
          * I.Intermediate.t

        module Part = struct
          type t =
            | A of A.Intermediate.Part.t
            | B of B.Intermediate.Part.t
            | C of C.Intermediate.Part.t
            | D of D.Intermediate.Part.t
            | E of E.Intermediate.Part.t
            | F of F.Intermediate.Part.t
            | G of G.Intermediate.Part.t
            | H of H.Intermediate.Part.t
            | I of I.Intermediate.Part.t
          [@@deriving bin_io, variants]
        end

        let create () =
          ( A.Intermediate.create ()
          , B.Intermediate.create ()
          , C.Intermediate.create ()
          , D.Intermediate.create ()
          , E.Intermediate.create ()
          , F.Intermediate.create ()
          , G.Intermediate.create ()
          , H.Intermediate.create ()
          , I.Intermediate.create () )
        ;;

        let apply_part (a, b, c, d, e, f, g, h, i) = function
          | Part.A x -> A.Intermediate.apply_part a x, b, c, d, e, f, g, h, i
          | Part.B x -> a, B.Intermediate.apply_part b x, c, d, e, f, g, h, i
          | Part.C x -> a, b, C.Intermediate.apply_part c x, d, e, f, g, h, i
          | Part.D x -> a, b, c, D.Intermediate.apply_part d x, e, f, g, h, i
          | Part.E x -> a, b, c, d, E.Intermediate.apply_part e x, f, g, h, i
          | Part.F x -> a, b, c, d, e, F.Intermediate.apply_part f x, g, h, i
          | Part.G x -> a, b, c, d, e, f, G.Intermediate.apply_part g x, h, i
          | Part.H x -> a, b, c, d, e, f, g, H.Intermediate.apply_part h x, i
          | Part.I x -> a, b, c, d, e, f, g, h, I.Intermediate.apply_part i x
        ;;
      end

      let to_parts (a, b, c, d, e, f, g, h, i) =
        Sequence.concat
        @@ Sequence.of_list
             [ Sequence.map ~f:Intermediate.Part.a (A.to_parts a)
             ; Sequence.map ~f:Intermediate.Part.b (B.to_parts b)
             ; Sequence.map ~f:Intermediate.Part.c (C.to_parts c)
             ; Sequence.map ~f:Intermediate.Part.d (D.to_parts d)
             ; Sequence.map ~f:Intermediate.Part.e (E.to_parts e)
             ; Sequence.map ~f:Intermediate.Part.f (F.to_parts f)
             ; Sequence.map ~f:Intermediate.Part.g (G.to_parts g)
             ; Sequence.map ~f:Intermediate.Part.h (H.to_parts h)
             ; Sequence.map ~f:Intermediate.Part.i (I.to_parts i)
             ]
      ;;

      let finalize (a, b, c, d, e, f, g, h, i) =
        ( A.finalize a
        , B.finalize b
        , C.finalize c
        , D.finalize d
        , E.finalize e
        , F.finalize f
        , G.finalize g
        , H.finalize h
        , I.finalize i )
      ;;
    end
  end

  module Of_tuple9 = struct
    module V1
        (A : S)
        (B : S)
        (C : S)
        (D : S)
        (E : S)
        (F : S)
        (G : S)
        (H : S)
        (I : S) : sig
      type t = A.t * B.t * C.t * D.t * E.t * F.t * G.t * H.t * I.t

      include S with type t := t
    end = struct
      module Plain = Of_tuple9_rpc.V1 (A) (B) (C) (D) (E) (F) (G) (H) (I)

      include
        Add_sexp.V1
          (Plain)
          (struct
            type t = Plain.Intermediate.Part.t =
              | A of A.Intermediate.Part.t
              | B of B.Intermediate.Part.t
              | C of C.Intermediate.Part.t
              | D of D.Intermediate.Part.t
              | E of E.Intermediate.Part.t
              | F of F.Intermediate.Part.t
              | G of G.Intermediate.Part.t
              | H of H.Intermediate.Part.t
              | I of I.Intermediate.Part.t
            [@@deriving sexp]
          end)
    end
  end

  (*$ Streamable_cinaps.of_variant_rpc_impl 2 *)
  module Of_variant2_rpc = struct
    module V1 (A : S_rpc) (B : S_rpc) = struct
      type t =
        [ `A of A.t
        | `B of B.t
        ]

      module Intermediate = struct
        type t =
          | Empty
          | A of A.Intermediate.t
          | B of B.Intermediate.t

        module Part = struct
          type t =
            | A_start
            | A_part of A.Intermediate.Part.t
            | B_start
            | B_part of B.Intermediate.Part.t
          [@@deriving bin_io, variants]
        end

        let create () = Empty

        let apply_part t (part : Part.t) =
          match t, part with
          | Empty, A_start -> A (A.Intermediate.create ())
          | Empty, B_start -> B (B.Intermediate.create ())
          | A a, A_part x -> A (A.Intermediate.apply_part a x)
          | B b, B_part x -> B (B.Intermediate.apply_part b x)
          | _ -> assert false
        ;;
      end

      let to_parts = function
        | `A a ->
          Sequence.cons
            Intermediate.Part.a_start
            (Sequence.map (A.to_parts a) ~f:Intermediate.Part.a_part)
        | `B b ->
          Sequence.cons
            Intermediate.Part.b_start
            (Sequence.map (B.to_parts b) ~f:Intermediate.Part.b_part)
      ;;

      let finalize = function
        | Intermediate.Empty -> assert false
        | Intermediate.A a -> `A (A.finalize a)
        | Intermediate.B b -> `B (B.finalize b)
      ;;
    end
  end

  (*$*)

  (*$ Streamable_cinaps.of_variant_impl 2 *)
  module Of_variant2 = struct
    module V1 (A : S) (B : S) = struct
      module Plain = Of_variant2_rpc.V1 (A) (B)

      include
        Add_sexp.V1
          (Plain)
          (struct
            type t = Plain.Intermediate.Part.t =
              | A_start
              | A_part of A.Intermediate.Part.t
              | B_start
              | B_part of B.Intermediate.Part.t
            [@@deriving sexp]
          end)
    end
  end

  (*$*)

  (*$ Streamable_cinaps.of_variant_rpc_impl 3 *)
  module Of_variant3_rpc = struct
    module V1 (A : S_rpc) (B : S_rpc) (C : S_rpc) = struct
      type t =
        [ `A of A.t
        | `B of B.t
        | `C of C.t
        ]

      module Intermediate = struct
        type t =
          | Empty
          | A of A.Intermediate.t
          | B of B.Intermediate.t
          | C of C.Intermediate.t

        module Part = struct
          type t =
            | A_start
            | A_part of A.Intermediate.Part.t
            | B_start
            | B_part of B.Intermediate.Part.t
            | C_start
            | C_part of C.Intermediate.Part.t
          [@@deriving bin_io, variants]
        end

        let create () = Empty

        let apply_part t (part : Part.t) =
          match t, part with
          | Empty, A_start -> A (A.Intermediate.create ())
          | Empty, B_start -> B (B.Intermediate.create ())
          | Empty, C_start -> C (C.Intermediate.create ())
          | A a, A_part x -> A (A.Intermediate.apply_part a x)
          | B b, B_part x -> B (B.Intermediate.apply_part b x)
          | C c, C_part x -> C (C.Intermediate.apply_part c x)
          | _ -> assert false
        ;;
      end

      let to_parts = function
        | `A a ->
          Sequence.cons
            Intermediate.Part.a_start
            (Sequence.map (A.to_parts a) ~f:Intermediate.Part.a_part)
        | `B b ->
          Sequence.cons
            Intermediate.Part.b_start
            (Sequence.map (B.to_parts b) ~f:Intermediate.Part.b_part)
        | `C c ->
          Sequence.cons
            Intermediate.Part.c_start
            (Sequence.map (C.to_parts c) ~f:Intermediate.Part.c_part)
      ;;

      let finalize = function
        | Intermediate.Empty -> assert false
        | Intermediate.A a -> `A (A.finalize a)
        | Intermediate.B b -> `B (B.finalize b)
        | Intermediate.C c -> `C (C.finalize c)
      ;;
    end
  end

  (*$*)

  (*$ Streamable_cinaps.of_variant_impl 3 *)
  module Of_variant3 = struct
    module V1 (A : S) (B : S) (C : S) = struct
      module Plain = Of_variant3_rpc.V1 (A) (B) (C)

      include
        Add_sexp.V1
          (Plain)
          (struct
            type t = Plain.Intermediate.Part.t =
              | A_start
              | A_part of A.Intermediate.Part.t
              | B_start
              | B_part of B.Intermediate.Part.t
              | C_start
              | C_part of C.Intermediate.Part.t
            [@@deriving sexp]
          end)
    end
  end

  (*$*)

  (*$ Streamable_cinaps.of_variant_rpc_impl 4 *)
  module Of_variant4_rpc = struct
    module V1 (A : S_rpc) (B : S_rpc) (C : S_rpc) (D : S_rpc) = struct
      type t =
        [ `A of A.t
        | `B of B.t
        | `C of C.t
        | `D of D.t
        ]

      module Intermediate = struct
        type t =
          | Empty
          | A of A.Intermediate.t
          | B of B.Intermediate.t
          | C of C.Intermediate.t
          | D of D.Intermediate.t

        module Part = struct
          type t =
            | A_start
            | A_part of A.Intermediate.Part.t
            | B_start
            | B_part of B.Intermediate.Part.t
            | C_start
            | C_part of C.Intermediate.Part.t
            | D_start
            | D_part of D.Intermediate.Part.t
          [@@deriving bin_io, variants]
        end

        let create () = Empty

        let apply_part t (part : Part.t) =
          match t, part with
          | Empty, A_start -> A (A.Intermediate.create ())
          | Empty, B_start -> B (B.Intermediate.create ())
          | Empty, C_start -> C (C.Intermediate.create ())
          | Empty, D_start -> D (D.Intermediate.create ())
          | A a, A_part x -> A (A.Intermediate.apply_part a x)
          | B b, B_part x -> B (B.Intermediate.apply_part b x)
          | C c, C_part x -> C (C.Intermediate.apply_part c x)
          | D d, D_part x -> D (D.Intermediate.apply_part d x)
          | _ -> assert false
        ;;
      end

      let to_parts = function
        | `A a ->
          Sequence.cons
            Intermediate.Part.a_start
            (Sequence.map (A.to_parts a) ~f:Intermediate.Part.a_part)
        | `B b ->
          Sequence.cons
            Intermediate.Part.b_start
            (Sequence.map (B.to_parts b) ~f:Intermediate.Part.b_part)
        | `C c ->
          Sequence.cons
            Intermediate.Part.c_start
            (Sequence.map (C.to_parts c) ~f:Intermediate.Part.c_part)
        | `D d ->
          Sequence.cons
            Intermediate.Part.d_start
            (Sequence.map (D.to_parts d) ~f:Intermediate.Part.d_part)
      ;;

      let finalize = function
        | Intermediate.Empty -> assert false
        | Intermediate.A a -> `A (A.finalize a)
        | Intermediate.B b -> `B (B.finalize b)
        | Intermediate.C c -> `C (C.finalize c)
        | Intermediate.D d -> `D (D.finalize d)
      ;;
    end
  end

  (*$*)

  (*$ Streamable_cinaps.of_variant_impl 4 *)
  module Of_variant4 = struct
    module V1 (A : S) (B : S) (C : S) (D : S) = struct
      module Plain = Of_variant4_rpc.V1 (A) (B) (C) (D)

      include
        Add_sexp.V1
          (Plain)
          (struct
            type t = Plain.Intermediate.Part.t =
              | A_start
              | A_part of A.Intermediate.Part.t
              | B_start
              | B_part of B.Intermediate.Part.t
              | C_start
              | C_part of C.Intermediate.Part.t
              | D_start
              | D_part of D.Intermediate.Part.t
            [@@deriving sexp]
          end)
    end
  end

  (*$*)

  (*$ Streamable_cinaps.of_variant_rpc_impl 5 *)
  module Of_variant5_rpc = struct
    module V1 (A : S_rpc) (B : S_rpc) (C : S_rpc) (D : S_rpc) (E : S_rpc) = struct
      type t =
        [ `A of A.t
        | `B of B.t
        | `C of C.t
        | `D of D.t
        | `E of E.t
        ]

      module Intermediate = struct
        type t =
          | Empty
          | A of A.Intermediate.t
          | B of B.Intermediate.t
          | C of C.Intermediate.t
          | D of D.Intermediate.t
          | E of E.Intermediate.t

        module Part = struct
          type t =
            | A_start
            | A_part of A.Intermediate.Part.t
            | B_start
            | B_part of B.Intermediate.Part.t
            | C_start
            | C_part of C.Intermediate.Part.t
            | D_start
            | D_part of D.Intermediate.Part.t
            | E_start
            | E_part of E.Intermediate.Part.t
          [@@deriving bin_io, variants]
        end

        let create () = Empty

        let apply_part t (part : Part.t) =
          match t, part with
          | Empty, A_start -> A (A.Intermediate.create ())
          | Empty, B_start -> B (B.Intermediate.create ())
          | Empty, C_start -> C (C.Intermediate.create ())
          | Empty, D_start -> D (D.Intermediate.create ())
          | Empty, E_start -> E (E.Intermediate.create ())
          | A a, A_part x -> A (A.Intermediate.apply_part a x)
          | B b, B_part x -> B (B.Intermediate.apply_part b x)
          | C c, C_part x -> C (C.Intermediate.apply_part c x)
          | D d, D_part x -> D (D.Intermediate.apply_part d x)
          | E e, E_part x -> E (E.Intermediate.apply_part e x)
          | _ -> assert false
        ;;
      end

      let to_parts = function
        | `A a ->
          Sequence.cons
            Intermediate.Part.a_start
            (Sequence.map (A.to_parts a) ~f:Intermediate.Part.a_part)
        | `B b ->
          Sequence.cons
            Intermediate.Part.b_start
            (Sequence.map (B.to_parts b) ~f:Intermediate.Part.b_part)
        | `C c ->
          Sequence.cons
            Intermediate.Part.c_start
            (Sequence.map (C.to_parts c) ~f:Intermediate.Part.c_part)
        | `D d ->
          Sequence.cons
            Intermediate.Part.d_start
            (Sequence.map (D.to_parts d) ~f:Intermediate.Part.d_part)
        | `E e ->
          Sequence.cons
            Intermediate.Part.e_start
            (Sequence.map (E.to_parts e) ~f:Intermediate.Part.e_part)
      ;;

      let finalize = function
        | Intermediate.Empty -> assert false
        | Intermediate.A a -> `A (A.finalize a)
        | Intermediate.B b -> `B (B.finalize b)
        | Intermediate.C c -> `C (C.finalize c)
        | Intermediate.D d -> `D (D.finalize d)
        | Intermediate.E e -> `E (E.finalize e)
      ;;
    end
  end

  (*$*)

  (*$ Streamable_cinaps.of_variant_impl 5 *)
  module Of_variant5 = struct
    module V1 (A : S) (B : S) (C : S) (D : S) (E : S) = struct
      module Plain = Of_variant5_rpc.V1 (A) (B) (C) (D) (E)

      include
        Add_sexp.V1
          (Plain)
          (struct
            type t = Plain.Intermediate.Part.t =
              | A_start
              | A_part of A.Intermediate.Part.t
              | B_start
              | B_part of B.Intermediate.Part.t
              | C_start
              | C_part of C.Intermediate.Part.t
              | D_start
              | D_part of D.Intermediate.Part.t
              | E_start
              | E_part of E.Intermediate.Part.t
            [@@deriving sexp]
          end)
    end
  end

  (*$*)

  module Of_list_or_sequence_not_packed_rpc = struct
    module V1
        (T : sig
           type 'a t

           val to_sequence : 'a t -> 'a Sequence.t
           val of_list : 'a list -> 'a t
         end)
        (X : S_rpc) =
    struct
      type t = X.t T.t

      module Intermediate = struct
        type t = (X.t list * X.Intermediate.t) option

        module Part = struct
          type t =
            | Elt_no_parts
            | Elt_first_part of X.Intermediate.Part.t
            | Elt_later_part of X.Intermediate.Part.t
          [@@deriving bin_io, variants]
        end

        let create () = None

        let force = function
          | None -> []
          | Some (xs, x_int) -> X.finalize x_int :: xs
        ;;

        let elt_start t = Some (force t, X.Intermediate.create ())

        let elt_part t ~part =
          match t with
          | Some (xs, int) -> Some (xs, X.Intermediate.apply_part int part)
          | None -> assert false
        ;;

        let apply_part t p =
          match (p : Part.t) with
          | Elt_no_parts -> elt_start t
          | Elt_first_part part -> elt_start t |> elt_part ~part
          | Elt_later_part part -> elt_part t ~part
        ;;
      end

      let finalize t = T.of_list (List.rev (Intermediate.force t))

      let to_parts xs =
        Sequence.concat_map (T.to_sequence xs) ~f:(fun x ->
          let xparts = X.to_parts x in
          match Sequence.next xparts with
          | None -> Sequence.singleton Intermediate.Part.elt_no_parts
          | Some (hd, tl) ->
            Sequence.cons
              (Intermediate.Part.elt_first_part hd)
              (Sequence.map tl ~f:Intermediate.Part.elt_later_part))
      ;;
    end
  end

  module Of_list_or_sequence_not_packed = struct
    module V1
        (T : sig
           type 'a t

           val to_sequence : 'a t -> 'a Sequence.t
           val of_list : 'a list -> 'a t
         end)
        (X : S) =
    struct
      module Plain = Of_list_or_sequence_not_packed_rpc.V1 (T) (X)

      include
        Add_sexp.V1
          (Plain)
          (struct
            type t = Plain.Intermediate.Part.t =
              | Elt_no_parts
              | Elt_first_part of X.Intermediate.Part.t
              | Elt_later_part of X.Intermediate.Part.t
            [@@deriving sexp]
          end)
    end
  end

  module Of_list_rpc = struct
    module V1 (X : S_rpc) = struct
      type t = X.t list

      module Intermediate = struct
        type t = (X.t list * X.Intermediate.t) option

        module Part = struct
          type t =
            | Elt_start
            | Elt_part of X.Intermediate.Part.t
          [@@deriving bin_io, variants]
        end

        let create () = None

        let force = function
          | None -> []
          | Some (xs, x_int) -> X.finalize x_int :: xs
        ;;

        let apply_part t p =
          match (p : Part.t) with
          | Elt_start -> Some (force t, X.Intermediate.create ())
          | Elt_part p ->
            (match t with
             | Some (xs, int) -> Some (xs, X.Intermediate.apply_part int p)
             | None -> assert false)
        ;;
      end

      let finalize t = List.rev (Intermediate.force t)

      let to_parts xs =
        Sequence.concat_map (Sequence.of_list xs) ~f:(fun x ->
          Sequence.cons
            Intermediate.Part.elt_start
            (Sequence.map ~f:Intermediate.Part.elt_part (X.to_parts x)))
      ;;
    end

    module V2 (X : S_rpc) = Packed_rpc.V1 (V1 (X))

    (* Same as V2, but mashes together [Elt_start] and first [Elt_part] for compactness.
       (Particularly noticeable when list elements are atomic, a common case.) *)
    module V3_not_packed = Of_list_or_sequence_not_packed_rpc.V1 (struct
        type 'a t = 'a list

        let to_sequence = Sequence.of_list
        let of_list x = x
      end)

    module V3 (X : S_rpc) = Packed_rpc.V1 (V3_not_packed (X))
  end

  module Of_list = struct
    module V1 (X : S) = struct
      module Plain = Of_list_rpc.V1 (X)

      include
        Add_sexp.V1
          (Plain)
          (struct
            type t = Plain.Intermediate.Part.t =
              | Elt_start
              | Elt_part of X.Intermediate.Part.t
            [@@deriving sexp]
          end)
    end

    module V2 (X : S) = Packed.V1 (V1 (X))

    (* Same as V2, but mashes together [Elt_start] and first [Elt_part] for compactness.
       (Particularly noticeable when list elements are atomic, a common case.) *)
    module V3_not_packed = Of_list_or_sequence_not_packed.V1 (struct
        type 'a t = 'a list

        let to_sequence = Sequence.of_list
        let of_list x = x
      end)

    module V3 (X : S) = Packed.V1 (V3_not_packed (X))
  end

  module Of_nonempty_list_rpc = struct
    module V1 (A : S_rpc) = struct
      type t = A.t Nonempty_list.t

      module M = struct
        type nonrec t = t

        let to_streamable = Nonempty_list.to_list
        let of_streamable = Nonempty_list.of_list_exn
      end

      include Remove_t_rpc (Of_streamable_rpc.V1 (Of_list_rpc.V3 (A)) (M))
    end
  end

  module Of_nonempty_list = struct
    module V1 (A : S) = struct
      type t = A.t Nonempty_list.t

      module M = struct
        type nonrec t = t

        let to_streamable = Nonempty_list.to_list
        let of_streamable = Nonempty_list.of_list_exn
      end

      include Remove_t (Of_streamable.V1 (Of_list.V3 (A)) (M))
    end
  end

  module Of_sexps = struct
    module V1_unpacked : S with type t = Sexp.t list = struct
      type t = Sexp.t list

      module Intermediate = struct
        type q = Sexp.t Queue.t

        type t =
          { stack : q Stack.t
          ; mutable queue : q
          }

        let create () = { stack = Stack.create (); queue = Queue.create () }

        module Part = struct
          type t =
            | Atom of string
            | Push
            | Pop
          [@@deriving bin_io, sexp]
        end

        let apply_part t part =
          (match (part : Part.t) with
           | Atom atom -> Queue.enqueue t.queue (Atom atom)
           | Push ->
             Stack.push t.stack t.queue;
             t.queue <- Queue.create ()
           | Pop ->
             let top = Sexp.List (Queue.to_list t.queue) in
             t.queue <- Stack.pop_exn t.stack;
             Queue.enqueue t.queue top);
          t
        ;;
      end

      let finalize { Intermediate.stack; queue } =
        assert (Stack.is_empty stack);
        Queue.to_list queue
      ;;

      module G = Sequence.Generator
      open G.Let_syntax

      let yield (part : Intermediate.Part.t) = G.yield part

      let rec parts_of_sexp : Sexp.t -> _ = function
        | Atom x -> yield (Atom x)
        | List xs ->
          let%bind () = yield Push in
          let%bind () = parts_of_sexps xs in
          let%bind () = yield Pop in
          return ()

      and parts_of_sexps xs = Sequence.Generator.all_unit (List.map xs ~f:parts_of_sexp)

      let to_parts t = G.run (parts_of_sexps t)
    end

    module V1 = Packed.V1 (V1_unpacked)
  end

  module Of_sexpable = struct
    module V1 (Sexpable : sig
        type t

        include Sexpable with type t := t
      end) =
      Of_streamable.V1
        (Of_sexps.V1)
        (struct
          type t = Sexpable.t

          let to_streamable x = [ [%sexp_of: Sexpable.t] x ]

          let of_streamable = function
            | [ x ] -> [%of_sexp: Sexpable.t] x
            | _ -> assert false
          ;;
        end)
  end

  module Of_option_rpc = struct
    module V1 (X : S_rpc) = struct
      type t = X.t option

      module Intermediate = struct
        type t = X.Intermediate.t option

        module Part = struct
          type t =
            | Elt_start
            | Elt_part of X.Intermediate.Part.t
          [@@deriving bin_io, variants]
        end

        let create () = None

        let apply_part t = function
          | Part.Elt_start ->
            (match t with
             | None -> Some (X.Intermediate.create ())
             | Some _ -> assert false)
          | Part.Elt_part p ->
            (match t with
             | Some x_int -> Some (X.Intermediate.apply_part x_int p)
             | None -> assert false)
        ;;
      end

      let finalize = function
        | None -> None
        | Some x_int -> Some (X.finalize x_int)
      ;;

      let to_parts = function
        | None -> Sequence.empty
        | Some x ->
          Sequence.cons
            Intermediate.Part.elt_start
            (Sequence.map ~f:Intermediate.Part.elt_part (X.to_parts x))
      ;;
    end

    (* Of_option.V2: use the same compactification as [Of_list.V3]. *)
    module V2 (X : S_rpc) = struct
      type t = X.t option

      include
        Remove_t_rpc
          (Of_streamable_rpc.V1
             (Of_list_rpc.V3_not_packed
                (X))
                (struct
                  type nonrec t = t

                  let to_streamable = function
                    | None -> []
                    | Some x -> [ x ]
                  ;;

                  let of_streamable = function
                    | [] -> None
                    | [ x ] -> Some x
                    | _ -> assert false
                  ;;
                end))
    end
  end

  module Of_option = struct
    module V1 (X : S) = struct
      module Plain = Of_option_rpc.V1 (X)

      include
        Add_sexp.V1
          (Plain)
          (struct
            type t = Plain.Intermediate.Part.t =
              | Elt_start
              | Elt_part of X.Intermediate.Part.t
            [@@deriving sexp]
          end)
    end

    (* Of_option.V2: use the same compactification as [Of_list.V3]. *)
    module V2 (X : S) = struct
      type t = X.t option

      include
        Remove_t
          (Of_streamable.V1
             (Of_list.V3_not_packed
                (X))
                (struct
                  type nonrec t = t

                  let to_streamable = function
                    | None -> []
                    | Some x -> [ x ]
                  ;;

                  let of_streamable = function
                    | [] -> None
                    | [ x ] -> Some x
                    | _ -> assert false
                  ;;
                end))
    end
  end

  module Of_result_rpc = struct
    module V1 (A : S_rpc) (B : S_rpc) = struct
      type t = (A.t, B.t) result

      include
        Remove_t_rpc
          (Of_streamable_rpc.V1
             (Of_variant2_rpc.V1 (A) (B))
                (struct
                  type nonrec t = t

                  let to_streamable = function
                    | Ok x -> `A x
                    | Error x -> `B x
                  ;;

                  let of_streamable = function
                    | `A x -> Ok x
                    | `B x -> Error x
                  ;;
                end))
    end
  end

  module Of_result = struct
    module V1 (A : S) (B : S) = struct
      type t = (A.t, B.t) result

      include
        Remove_t
          (Of_streamable.V1
             (Of_variant2.V1 (A) (B))
                (struct
                  type nonrec t = t

                  let to_streamable = function
                    | Ok x -> `A x
                    | Error x -> `B x
                  ;;

                  let of_streamable = function
                    | `A x -> Ok x
                    | `B x -> Error x
                  ;;
                end))
    end
  end

  module Of_fqueue_rpc = struct
    module Common (A : S_rpc) = struct
      type t = A.t Fqueue.t

      module M = struct
        type nonrec t = t

        let to_streamable = Fqueue.to_list
        let of_streamable = Fqueue.of_list
      end
    end

    module V2 (A : S_rpc) = struct
      include Common (A)
      include Remove_t_rpc (Of_streamable_rpc.V1 (Of_list_rpc.V2 (A)) (M))
    end

    module V3 (A : S_rpc) = struct
      include Common (A)
      include Remove_t_rpc (Of_streamable_rpc.V1 (Of_list_rpc.V3 (A)) (M))
    end
  end

  module Of_fqueue = struct
    module Common (A : S) = struct
      type t = A.t Fqueue.t

      module M = struct
        type nonrec t = t

        let to_streamable = Fqueue.to_list
        let of_streamable = Fqueue.of_list
      end
    end

    module V2 (A : S) = struct
      include Common (A)
      include Remove_t (Of_streamable.V1 (Of_list.V2 (A)) (M))
    end

    module V3 (A : S) = struct
      include Common (A)
      include Remove_t (Of_streamable.V1 (Of_list.V3 (A)) (M))
    end
  end

  module Of_set_rpc = struct
    module Common (Key : Stable_without_of_sexp) = struct
      type t = (Key.t, Key.comparator_witness) Set.t

      module M = struct
        type nonrec t = t

        let to_streamable = Set.to_list
        let of_streamable = Set.Using_comparator.of_list ~comparator:Key.comparator
      end

      module K = Of_atomic_rpc.V1 (Key)
    end

    module V2 (Key : Stable_without_of_sexp) = struct
      module C = Common (Key)
      include C
      include Remove_t_rpc (Of_streamable_rpc.V1 (Of_list_rpc.V2 (C.K)) (C.M))
    end

    module V3 (Key : Stable_without_of_sexp) = struct
      module C = Common (Key)
      include C
      include Remove_t_rpc (Of_streamable_rpc.V1 (Of_list_rpc.V3 (C.K)) (C.M))
    end
  end

  module Of_set = struct
    module Common (Key : Stable) = struct
      type t = (Key.t, Key.comparator_witness) Set.t

      module M = struct
        type nonrec t = t

        let to_streamable = Set.to_list
        let of_streamable = Set.Using_comparator.of_list ~comparator:Key.comparator
      end

      module K = Of_atomic.V1 (Key)
    end

    module V2 (Key : Stable) = struct
      module C = Common (Key)
      include C
      include Remove_t (Of_streamable.V1 (Of_list.V2 (C.K)) (C.M))
    end

    module V3 (Key : Stable) = struct
      module C = Common (Key)
      include C
      include Remove_t (Of_streamable.V1 (Of_list.V3 (C.K)) (C.M))
    end
  end

  module Of_sequence_rpc = struct
    module V1_not_packed = Of_list_or_sequence_not_packed_rpc.V1 (struct
        type 'a t = 'a Sequence.t

        let to_sequence x = x
        let of_list = Sequence.of_list
      end)

    module V1 (X : S_rpc) = Packed_rpc.V1 (V1_not_packed (X))
  end

  module Of_sequence = struct
    module V1_not_packed = Of_list_or_sequence_not_packed.V1 (struct
        type 'a t = 'a Sequence.t

        let to_sequence x = x
        let of_list = Sequence.of_list
      end)

    module V1 (X : S) = Packed.V1 (V1_not_packed (X))
  end

  module Fixpoint_rpc = struct
    module V1
        (T : sig
           type t
         end)
        (F : functor (_ : S_rpc with type t = T.t) -> S_rpc with type t = T.t) :
      S_rpc with type t = T.t = struct
      type t = T.t

      module rec M0 : (S_rpc with type t = t) = struct
        module Rec = F (M_t)

        type nonrec t = t

        module Intermediate = struct
          type t = Rec.Intermediate.t

          module Part = struct
            include Rec.Intermediate.Part

            let bin_shape_t =
              let open Bin_prot.Shape in
              basetype
                (Uuid.of_string "859f388a-490a-11e6-b296-cbd133aa1837")
                [ Rec.Intermediate.Part.bin_shape_t ]
            ;;
          end

          let create = Rec.Intermediate.create
          let apply_part = Rec.Intermediate.apply_part
        end

        let to_parts = Rec.to_parts
        let finalize = Rec.finalize
      end

      and M_only_functions : (S_only_functions_rpc with type t = t) = M0
      and M : (S_rpc with type t := t) = Of_only_functions_rpc.V1 (M_only_functions)

      and M_t : (S_rpc with type t = t) = struct
        type nonrec t = t

        include M
      end

      include Remove_t_rpc (M0)
    end
  end

  module Fixpoint = struct
    module V1
        (T : sig
           type t
         end)
        (F : functor (_ : S with type t = T.t) -> S with type t = T.t) :
      S with type t = T.t = struct
      type t = T.t

      module rec M0 : (S with type t = t) = struct
        module Rec = F (M_t)

        type nonrec t = t

        module Intermediate = struct
          type t = Rec.Intermediate.t

          module Part = struct
            include Rec.Intermediate.Part

            let bin_shape_t =
              let open Bin_prot.Shape in
              basetype
                (Uuid.of_string "859f388a-490a-11e6-b296-cbd133aa1837")
                [ Rec.Intermediate.Part.bin_shape_t ]
            ;;
          end

          let create = Rec.Intermediate.create
          let apply_part = Rec.Intermediate.apply_part
        end

        let to_parts = Rec.to_parts
        let finalize = Rec.finalize
      end

      and M_only_functions : (S_only_functions with type t = t) = M0
      and M : (S with type t := t) = Of_only_functions.V1 (M_only_functions)

      and M_t : (S with type t = t) = struct
        type nonrec t = t

        include M
      end

      include Remove_t (M0)
    end
  end

  module V1 = struct
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
    module Checked = Checked
    module Packed = Packed.V1
    module Packed_rpc = Packed_rpc.V1
    module Remove_t = Remove_t
    module Remove_t_rpc = Remove_t_rpc
  end

  module Latest = V1
end

include Stable.Latest
