open! Core
open! Async_kernel
open! Import

module type Caller_converts = sig
  type query
  type state
  type update

  val dispatch_multi
    :  Versioned_rpc.Connection_with_menu.t
    -> query
    -> (state * update Or_error.t Pipe.Reader.t) Deferred.Or_error.t

  val dispatch_multi'
    :  Versioned_rpc.Connection_with_menu.t
    -> query
    -> (state * update Or_error.t Pipe.Reader.t) Or_error.t Deferred.Or_error.t

  val name : string
end

module type Callee_converts = sig
  type query
  type state
  type update

  val implement_multi
    :  ?on_exception:Rpc.On_exception.t (** default: [On_exception.continue] *)
    -> ('conn_state
        -> version:int
        -> query
        -> (state * update Pipe.Reader.t) Deferred.Or_error.t)
    -> 'conn_state Rpc.Implementation.t list

  val name : string
end

module type Both_convert = sig
  type caller_query
  type caller_state
  type caller_update
  type callee_query
  type callee_state
  type callee_update

  val dispatch_multi
    :  Versioned_rpc.Connection_with_menu.t
    -> caller_query
    -> (caller_state * caller_update Or_error.t Pipe.Reader.t) Deferred.Or_error.t

  val dispatch_multi'
    :  Versioned_rpc.Connection_with_menu.t
    -> caller_query
    -> (caller_state * caller_update Or_error.t Pipe.Reader.t) Or_error.t
         Deferred.Or_error.t

  val implement_multi
    :  ?on_exception:Rpc.On_exception.t (** default: [On_exception.continue] *)
    -> ('conn_state
        -> version:int
        -> callee_query
        -> (callee_state * callee_update Pipe.Reader.t) Deferred.Or_error.t)
    -> 'conn_state Rpc.Implementation.t list

  val name : string
end

module type Versioned_state_rpc = sig
  module type Caller_converts = Caller_converts
  module type Callee_converts = Callee_converts
  module type Both_convert = Both_convert

  module Caller_converts : sig
    module type S = Caller_converts

    module Make (Model : sig
        val name : string

        type query
        type state
        type update
      end) : sig
      module Register (Version : sig
          val version : int

          type query [@@deriving bin_io]
          type state

          module State : Main.S_rpc with type t = state

          type update

          module Update : Main.S_rpc with type t = update

          val query_of_model : Model.query -> query
          val model_of_state : state -> Model.state
          val model_of_update : update -> Model.update
          val client_pushes_back : bool
        end) : sig
        val rpc : (Version.query, Version.state, Version.update) State_rpc.t

        (** [implement'] is like [State_rpc.implement rpc] except that it allows the
            server to control the conversion from the [state] and [update]s to parts. *)
        val implement'
          :  ?on_exception:Rpc.On_exception.t (** default: [On_exception.continue] *)
          -> ('conn_state
              -> Version.query
              -> (Version.State.Intermediate.Part.t Pipe.Reader.t
                 * Version.Update.Intermediate.Part.t Pipe.Reader.t Pipe.Reader.t)
                   Deferred.Or_error.t)
          -> 'conn_state Rpc.Implementation.t
      end

      include
        S
        with type query := Model.query
        with type state := Model.state
        with type update := Model.update
    end
  end

  module Callee_converts : sig
    module type S = Callee_converts

    module Make (Model : sig
        val name : string

        type query
        type state
        type update
      end) : sig
      module Register (Version : sig
          val version : int

          type query [@@deriving bin_io]
          type state

          module State : Main.S_rpc with type t = state

          type update

          module Update : Main.S_rpc with type t = update

          val model_of_query : query -> Model.query
          val state_of_model : Model.state -> state
          val update_of_model : Model.update -> update
          val client_pushes_back : bool
        end) : sig
        val rpc : (Version.query, Version.state, Version.update) State_rpc.t

        (** [implement'] is like [State_rpc.implement rpc] except that it allows the
            server to control the conversion from the [state] and [update]s to parts. *)
        val implement'
          :  ?on_exception:Rpc.On_exception.t (** default: [On_exception.continue] *)
          -> ('conn_state
              -> Version.query
              -> (Version.State.Intermediate.Part.t Pipe.Reader.t
                 * Version.Update.Intermediate.Part.t Pipe.Reader.t Pipe.Reader.t)
                   Deferred.Or_error.t)
          -> 'conn_state Rpc.Implementation.t
      end

      include
        S
        with type query := Model.query
        with type state := Model.state
        with type update := Model.update
    end
  end

  module Both_convert : sig
    module type S = Both_convert

    module Make (Model : sig
        val name : string

        module Caller : sig
          type query
          type state
          type update
        end

        module Callee : sig
          type query
          type state
          type update
        end
      end) : sig
      open Model

      module Register (Version : sig
          val version : int

          type query [@@deriving bin_io]
          type state

          module State : Main.S_rpc with type t = state

          type update

          module Update : Main.S_rpc with type t = update

          val query_of_caller_model : Caller.query -> query
          val callee_model_of_query : query -> Callee.query
          val state_of_callee_model : Callee.state -> state
          val caller_model_of_state : state -> Caller.state
          val update_of_callee_model : Callee.update -> update
          val caller_model_of_update : update -> Caller.update
          val client_pushes_back : bool
        end) : sig
        val rpc : (Version.query, Version.state, Version.update) State_rpc.t

        (** [implement'] is like [State_rpc.implement rpc] except that it allows the
            server to control the conversion from the [state] and [update]s to parts. *)
        val implement'
          :  ?on_exception:Rpc.On_exception.t (** default: [On_exception.continue] *)
          -> ('conn_state
              -> Version.query
              -> (Version.State.Intermediate.Part.t Pipe.Reader.t
                 * Version.Update.Intermediate.Part.t Pipe.Reader.t Pipe.Reader.t)
                   Deferred.Or_error.t)
          -> 'conn_state Rpc.Implementation.t
      end

      include
        S
        with type caller_query := Caller.query
        with type caller_state := Caller.state
        with type caller_update := Caller.update
        with type callee_query := Callee.query
        with type callee_state := Callee.state
        with type callee_update := Callee.update
    end
  end
end
