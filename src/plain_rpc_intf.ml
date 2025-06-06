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
  val dispatch : ('q, 'r) t -> Rpc.Connection.t -> 'q -> 'r Deferred.Or_error.t

  val dispatch'
    :  ('q, 'r) t
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

  module Direct_writer : sig
    type 'response_part t

    (** Write a part of the response. Returns [`Closed] if [t] is closed. Will raise if
        the response has already been finalised. *)
    val write_response_without_pushback_exn
      :  'response_part t
      -> 'response_part
      -> [ `Ok | `Closed ]

    (** Finalise the response, indicating there are no more parts. Returns [`Closed] if
        [t] is closed.

        Will raise if the response has already been finalised. *)
    val finalise_response_without_pushback_exn : _ t -> [ `Ok | `Closed ]

    val is_response_finalised : _ t -> bool
    val response_finalised : _ t -> unit Deferred.t
    val close : _ t -> unit
    val closed : _ t -> unit Deferred.t
    val flushed : _ t -> unit Deferred.t
    val is_closed : _ t -> bool

    module Expert : sig
      (** Response part bigstrings need to be created by this function instead of directly
          written to the pipe. *)
      val create_response_part
        :  bin_writer:'response_part Bin_prot.Type_class.writer
        -> 'response_part
        -> Bigstring.t

      val finalise_response_message : Bigstring.t lazy_t

      val write_without_pushback
        :  ?pos:int
        -> ?len:int
        -> _ t
        -> Bigstring.t
        -> [ `Closed | `Ok ]
    end
  end

  module Make (X : S) : sig
    val rpc : (X.query, X.response) t

    (** [implement'] is like [implement rpc] except that it allows the server to control
        the conversion from the [response] to parts. *)
    val implement'
      :  ?on_exception:Rpc.On_exception.t (** default: [On_exception.continue] **)
      -> ('conn_state
          -> X.query
          -> X.Response.Intermediate.Part.t Pipe.Reader.t Deferred.Or_error.t)
      -> 'conn_state Rpc.Implementation.t

    val implement_direct
      :  ?on_exception:Rpc.On_exception.t (** default: [On_exception.continue] **)
      -> ('conn_state
          -> X.query
          -> X.Response.Intermediate.Part.t Direct_writer.t
          -> unit Deferred.Or_error.t)
      -> 'conn_state Rpc.Implementation.t
  end
end
