open! Core
open! Import

(** Signatures for types supporting incremental serialization.

    If you're using lib/transaction_log, you'll need the full [S] interface: The sexp
    serialization is actually used for the [Intermediate.Part.t]s. If by contrast you're
    only using streamable RPCs via this library, then only the [S_rpc] interface is
    required, so you can avoid defining fake-stable sexps in a nominally stable scope.

    [Part.t] should be chosen such that any single part will be relatively small in
    binprot form.

    [Intermediate.t] represents the state until all the parts have been received.

    [finalize] is called when all the parts delivered by [to_parts] have been successfully
    transmitted.

    Every module implementing this interface ought to satisfy the "round trip" law that
    says that the following [round_trip] function is the identity:
    {[
      let round_trip t =
        let acc = Intermediate.create () in
        let acc = Sequence.fold ~init:acc ~f:Intermediate.apply_part (to_parts t) in
        finalize acc
      ;;
    ]}

    Note that the Intermediate interface feels indecisive, unable to decide whether
    [Intermediate.t] is mutable or immutable. This is by design, so that modules
    implementing this interface can decide to have either mutable or immutable
    [Intermediate.t] types. In each case, there is a small bit of annoying overhead:
    - for immutable types, one must add a "fun () -> " to the beginning of the definition
      of [create], which will always return the same constant.
    - for mutable types, one must add a "; t" to the end of the more natural
      [unit]-returning definition of [apply_part].

    Finally, note that one must take special care if [t] is mutable. In particular,
    calling [to_parts state] on a mutable [state] is risky, since future updates to
    [state] will change the sequence of parts one observes when one may be in the middle
    of traversing the sequence. One way to sidestep this issue is to keep aside a frozen
    copy of [state] by forking the entire process before calling and consuming
    [to_parts state]. Other less drastic solutions may also present themselves. *)

(** The minimal interface, for use with the RPC functionality in this library. *)
module type S_rpc = sig
  type t

  module Intermediate : sig
    type t

    module Part : sig
      type t [@@deriving bin_io]
    end

    val create : unit -> t
    val apply_part : t -> Part.t -> t
  end

  val to_parts : t -> Intermediate.Part.t Sequence.t
  val finalize : Intermediate.t -> t
end

(** The minimal interface plus [Intermediate.Part.sexp_of_t], for use with
    lib/kafka_transaction_log. *)
module type S_rpc_with_sexp_of_part = sig
  type t

  module Intermediate : sig
    type t

    module Part : sig
      type t [@@deriving bin_io, sexp_of]
    end

    val create : unit -> t
    val apply_part : t -> Part.t -> t
  end

  val to_parts : t -> Intermediate.Part.t Sequence.t
  val finalize : Intermediate.t -> t
end

(** The full interface, for use with lib/transaction_log. *)
module type S = sig
  type t

  module Intermediate : sig
    type t

    module Part : sig
      type t [@@deriving bin_io, sexp]
    end

    val create : unit -> t
    val apply_part : t -> Part.t -> t
  end

  val to_parts : t -> Intermediate.Part.t Sequence.t
  val finalize : Intermediate.t -> t
end

(** check compatibility of S and S_rpc_with_sexp_of_part *)
module Coerce1 (M : S) : S_rpc_with_sexp_of_part = M

(** check compatibility of S_rpc_with_sexp_of_part and S_rpc *)
module Coerce2 (M : S_rpc_with_sexp_of_part) : S_rpc = M
