(** A Streamable.Plain_rpc is just like a Plain_rpc except the response is streamed out
    gradually rather than sent in one big [bin_io] blob. *)

open! Core
open! Async_kernel
open! Import

module type S = sig
  val name : string
  val version : int

  type query [@@deriving bin_io]
  type response

  module Response : Main.S_rpc with type t = response

  val client_pushes_back : bool
end

module type Plain_rpc = sig
  module type S = S

  type ('q, 'r) t

  val description : _ t -> Rpc.Description.t

  val dispatch
    :  ?metadata:Rpc_metadata.t
    -> ('q, 'r) t
    -> Rpc.Connection.t
    -> 'q
    -> 'r Deferred.Or_error.t

  val dispatch'
    :  ?metadata:Rpc_metadata.t
    -> ('q, 'r) t
    -> Rpc.Connection.t
    -> 'q
    -> 'r Or_error.t Deferred.Or_error.t

  val implement
    :  ?on_exception:Rpc.On_exception.t (** default: [On_exception.continue] **)
    -> ('q, 'r) t
    -> ('conn_state -> 'q -> 'r Deferred.Or_error.t)
    -> 'conn_state Rpc.Implementation.t

  val bin_query_shape : _ t -> Bin_prot.Shape.t
  val bin_response_shape : _ t -> Bin_prot.Shape.t

  module Make (X : S) : sig
    val rpc : (X.query, X.response) t

    (** [implement'] is like [implement rpc] except that it allows the server
        to control the conversion from the [response] to parts. *)
    val implement'
      :  ?on_exception:Rpc.On_exception.t (** default: [On_exception.continue] **)
      -> ('conn_state
          -> X.query
          -> X.Response.Intermediate.Part.t Pipe.Reader.t Deferred.Or_error.t)
      -> 'conn_state Rpc.Implementation.t
  end
end
