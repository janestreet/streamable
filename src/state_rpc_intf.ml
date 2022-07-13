(** A Streamable.State_rpc is just like a State_rpc except the state and updates are
    streamed out gradually rather than sent in one big [bin_io] blob. *)

open! Core
open! Async_kernel
open! Import

module type S = sig
  val name    : string
  val version : int

  type query [@@deriving bin_io]
  type state

  module State : Main.S_rpc with type t = state

  type update

  module Update : Main.S_rpc with type t = update

  val client_pushes_back : bool
end

module type State_rpc = sig
  module type S = S

  type ('q, 's, 'u) t

  val description : _ t -> Rpc.Description.t

  val dispatch
    :  ('q, 's, 'u) t
    -> Rpc.Connection.t
    -> 'q
    -> ('s * 'u Pipe.Reader.t) Deferred.Or_error.t

  val implement
    :  ('q, 's, 'u) t
    -> ('conn_state -> 'q -> ('s * 'u Pipe.Reader.t) Deferred.Or_error.t)
    -> 'conn_state Rpc.Implementation.t

  val bin_query_shape  : _ t -> Bin_prot.Shape.t
  val bin_state_shape  : _ t -> Bin_prot.Shape.t
  val bin_update_shape : _ t -> Bin_prot.Shape.t

  module Make (X : S) : sig
    val rpc : (X.query, X.state, X.update) t

    (** [implement'] is like [implement rpc] except that it allows the server
        to control the conversion from the [state] and [update]s to parts. *)
    val implement'
      :  ('conn_state
          -> X.query
          -> (X.State.Intermediate.Part.t Pipe.Reader.t
              * X.Update.Intermediate.Part.t Pipe.Reader.t Pipe.Reader.t)
               Deferred.Or_error.t)
      -> 'conn_state Rpc.Implementation.t
  end
end
