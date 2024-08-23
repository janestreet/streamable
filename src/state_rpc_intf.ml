(** A Streamable.State_rpc is just like a State_rpc except the state and updates are
    streamed out gradually rather than sent in one big [bin_io] blob. *)

open! Core
open! Async_kernel
open! Import

module type S = sig
  val name : string
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

  val dispatch'
    :  ('q, 's, 'u) t
    -> Rpc.Connection.t
    -> 'q
    -> ('s * 'u Pipe.Reader.t) Or_error.t Deferred.Or_error.t

  val implement
    :  ?on_exception:Rpc.On_exception.t (** default: [On_exception.continue] **)
    -> ('q, 's, 'u) t
    -> ('conn_state -> 'q -> ('s * 'u Pipe.Reader.t) Deferred.Or_error.t)
    -> 'conn_state Rpc.Implementation.t

  val bin_query_shape : _ t -> Bin_prot.Shape.t
  val bin_state_shape : _ t -> Bin_prot.Shape.t
  val bin_update_shape : _ t -> Bin_prot.Shape.t

  module Direct_writer : sig
    type ('state_part, 'update_part) t

    (** Write a part of the initial state. Returns [`Closed] if [t] is closed.

        Will raise if the initial state has already been finalised.
    *)
    val write_state_without_pushback_exn
      :  ('state_part, _) t
      -> 'state_part
      -> [ `Ok | `Closed ]

    (** Finalise the initial state, indicating there are no more parts.
        Returns [`Closed] if [t] is closed.

        Will raise if the initial state has already been finalised.
    *)
    val finalise_state_without_pushback_exn : _ t -> [ `Ok | `Closed ]

    val is_state_finalised : _ t -> bool
    val state_finalised : _ t -> unit Deferred.t

    (** Write a part of an update. Returns [`Closed] if [t] is closed.

        Will raise if the initial state has not yet been finalised.
    *)
    val write_update_without_pushback_exn
      :  (_, 'update_part) t
      -> 'update_part
      -> [ `Ok | `Closed ]

    (** Finalise the current update. Returns [`Closed] if [t] is closed.

        Will raise if the initial state has not yet been finalised.
    *)
    val finalise_update_without_pushback_exn : _ t -> [ `Ok | `Closed ]

    val close : _ t -> unit
    val closed : _ t -> unit Deferred.t
    val flushed : _ t -> unit Deferred.t
    val is_closed : _ t -> bool

    (** [Expert] allows the serialisation of parts to occur separately from the writing
        of parts. While you are able to write any bigstring to the pipe rpc with this
        interface, you should only write state parts until you finalise the state, and
        then after that you should only write update parts. *)
    module Expert : sig
      val create_state_part
        :  state_bin_writer:'state_part Bin_prot.Type_class.writer
        -> 'state_part
        -> Bigstring.t

      val finalise_state_message : Bigstring.t lazy_t

      val create_update_part
        :  update_bin_writer:'update_part Bin_prot.Type_class.writer
        -> 'update_part
        -> Bigstring.t

      val finalise_update_message : Bigstring.t lazy_t

      val write_without_pushback
        :  ?pos:int
        -> ?len:int
        -> _ t
        -> Bigstring.t
        -> [ `Closed | `Ok ]
    end

    module Group : sig
      type ('state_part, 'update_part) direct_writer := ('state_part, 'update_part) t
      type ('state_part, 'update_part) t

      val create : ?buffer:Rpc.Pipe_rpc.Direct_stream_writer.Group.Buffer.t -> unit -> _ t

      (** [flushed_or_closed t] is determined when the underlying writer for each member of [t] is
          flushed or closed.
      *)
      val flushed_or_closed : _ t -> unit Deferred.t

      (** Add a direct writer to the group. Raises if the writer has not finalised its initial state,
          if the writer is closed or already part of the group,
          or if its bin-prot writer is different than an existing group member's.
          When the writer is closed, it is automatically removed from the group.
      *)
      val add_exn
        :  ('state_part, 'update_part) t
        -> ('state_part, 'update_part) direct_writer
        -> unit

      (** Remove a writer from a group. Note that writers are automatically removed from
          all groups when they are closed, so you only need to call this if you want to
          remove a writer without closing it.
      *)
      val remove
        :  ('state_part, 'update_part) t
        -> ('state_part, 'update_part) direct_writer
        -> unit

      (** Write an update part on all direct writers in the group. Contrary to
          [Direct_writer.write_without_pushback],
          this cannot return [`Closed] as elements of the
          group are removed immediately when they are closed, and it cannot raise
          due to initial state not being finalised as this is checked when adding
          to the group.
      *)
      val write_update_without_pushback : (_, 'update_part) t -> 'update_part -> unit

      (** Finalise the current update on all direct writers in the group. *)
      val finalise_update_without_pushback : _ t -> unit

      (** The number of clients currently. *)
      val length : _ t -> int

      (** Close all of the added clients.

          You can use [flushed_or_closed] to then wait for the closing to complete.
      *)
      val close_all : ('state_part, 'update_part) t -> unit
    end
  end

  module Make (X : S) : sig
    val rpc : (X.query, X.state, X.update) t

    (** [implement'] is like [implement rpc] except that it allows the server
        to control the conversion from the [state] and [update]s to parts. *)
    val implement'
      :  ?on_exception:Rpc.On_exception.t (** default: [On_exception.continue] **)
      -> ('conn_state
          -> X.query
          -> (X.State.Intermediate.Part.t Pipe.Reader.t
             * X.Update.Intermediate.Part.t Pipe.Reader.t Pipe.Reader.t)
               Deferred.Or_error.t)
      -> 'conn_state Rpc.Implementation.t

    val implement_direct
      :  ?on_exception:Rpc.On_exception.t (** default: [On_exception.continue] **)
      -> ('conn_state
          -> X.query
          -> (X.State.Intermediate.Part.t, X.Update.Intermediate.Part.t) Direct_writer.t
          -> unit Deferred.Or_error.t)
      -> 'conn_state Rpc.Implementation.t
  end
end
