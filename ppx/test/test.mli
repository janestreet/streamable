open! Core

(** No-op modules that simply ensure that the supplied module conforms to the expected
    interface. *)

module Is_S (X : Streamable.S) : sig end
module Is_S_rpc (X : Streamable.S_rpc) : sig end
