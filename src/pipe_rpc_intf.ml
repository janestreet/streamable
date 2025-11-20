(** A Streamable.Pipe_rpc is just like a Pipe_rpc except the updates are streamed out
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

module type Pipe_rpc = sig
  module type S = S

  type ('q, 'r) t

  val description : _ t -> Rpc.Description.t

  val dispatch
    :  ('q, 'r) t
    -> Rpc.Connection.t
    -> 'q
    -> 'r Pipe.Reader.t Deferred.Or_error.t

  val dispatch'
    :  ('q, 'r) t
    -> Rpc.Connection.t
    -> 'q
    -> 'r Pipe.Reader.t Or_error.t Deferred.Or_error.t

  val implement
    :  ?on_exception:Rpc.On_exception.t (** default: [On_exception.continue] *)
    -> ?leave_open_on_exception:bool (** default: See [Async_rpc.Pipe_rpc.implement] *)
    -> ('q, 'r) t
    -> ('conn_state -> 'q -> 'r Pipe.Reader.t Deferred.Or_error.t)
    -> 'conn_state Rpc.Implementation.t

  val bin_query_shape : _ t -> Bin_prot.Shape.t
  val bin_response_shape : _ t -> Bin_prot.Shape.t

  module Make (X : S) : sig
    val rpc : (X.query, X.response) t

    (** [implement'] is like [implement rpc] except that it allows the server to control
        the conversion from [response]s to parts. *)
    val implement'
      :  ?on_exception:Rpc.On_exception.t (** default: [On_exception.continue] *)
      -> ('conn_state
          -> X.query
          -> X.Response.Intermediate.Part.t Pipe.Reader.t Pipe.Reader.t
               Deferred.Or_error.t)
      -> 'conn_state Rpc.Implementation.t
  end
end
