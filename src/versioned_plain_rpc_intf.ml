open! Core
open! Async_kernel
open! Import

module type Caller_converts = sig
  type query
  type response

  val dispatch_multi
    :  Versioned_rpc.Connection_with_menu.t
    -> query
    -> response Deferred.Or_error.t

  val dispatch_multi'
    :  Versioned_rpc.Connection_with_menu.t
    -> query
    -> response Or_error.t Deferred.Or_error.t

  val name : string
end

module type Callee_converts = sig
  type query
  type response

  val implement_multi
    :  ?on_exception:Rpc.On_exception.t (** default: [On_exception.continue] *)
    -> ('conn_state -> version:int -> query -> response Deferred.Or_error.t)
    -> 'conn_state Rpc.Implementation.t list

  val name : string
end

module type Both_convert = sig
  type caller_query
  type caller_response
  type callee_query
  type callee_response

  val dispatch_multi
    :  Versioned_rpc.Connection_with_menu.t
    -> caller_query
    -> caller_response Deferred.Or_error.t

  val dispatch_multi'
    :  Versioned_rpc.Connection_with_menu.t
    -> caller_query
    -> caller_response Or_error.t Deferred.Or_error.t

  val implement_multi
    :  ?on_exception:Rpc.On_exception.t (** default: [On_exception.continue] *)
    -> ('conn_state -> version:int -> callee_query -> callee_response Deferred.Or_error.t)
    -> 'conn_state Rpc.Implementation.t list

  val name : string
end

module type Versioned_plain_rpc = sig
  module type Caller_converts = Caller_converts
  module type Callee_converts = Callee_converts
  module type Both_convert = Both_convert

  module Caller_converts : sig
    module type S = Caller_converts

    module Make (Model : sig
        val name : string

        type query
        type response
      end) : sig
      module Register (Version : sig
          val version : int

          type query [@@deriving bin_io]
          type response

          module Response : Main.S_rpc with type t = response

          val query_of_model : Model.query -> query
          val model_of_response : response -> Model.response
          val client_pushes_back : bool
        end) : sig
        val rpc : (Version.query, Version.response) Plain_rpc.t

        (** [implement'] is like [Plain_rpc.implement rpc] except that it allows the
            server to control the conversion from the [response] to parts. *)
        val implement'
          :  ?on_exception:Rpc.On_exception.t (** default: [On_exception.continue] *)
          -> ('conn_state
              -> Version.query
              -> Version.Response.Intermediate.Part.t Pipe.Reader.t Deferred.Or_error.t)
          -> 'conn_state Rpc.Implementation.t
      end

      include S with type query := Model.query with type response := Model.response
    end
  end

  module Callee_converts : sig
    module type S = Callee_converts

    module Make (Model : sig
        val name : string

        type query
        type response
      end) : sig
      module Register (Version : sig
          val version : int

          type query [@@deriving bin_io]
          type response

          module Response : Main.S_rpc with type t = response

          val model_of_query : query -> Model.query
          val response_of_model : Model.response -> response
          val client_pushes_back : bool
        end) : sig
        val rpc : (Version.query, Version.response) Plain_rpc.t

        (** [implement'] is like [Plain_rpc.implement rpc] except that it allows the
            server to control the conversion from the [response] to parts. *)
        val implement'
          :  ?on_exception:Rpc.On_exception.t (** default: [On_exception.continue] *)
          -> ('conn_state
              -> Version.query
              -> Version.Response.Intermediate.Part.t Pipe.Reader.t Deferred.Or_error.t)
          -> 'conn_state Rpc.Implementation.t
      end

      include S with type query := Model.query with type response := Model.response
    end
  end

  module Both_convert : sig
    module type S = Both_convert

    module Make (Model : sig
        val name : string

        module Caller : sig
          type query
          type response
        end

        module Callee : sig
          type query
          type response
        end
      end) : sig
      open Model

      module Register (Version : sig
          val version : int

          type query [@@deriving bin_io]
          type response

          module Response : Main.S_rpc with type t = response

          val query_of_caller_model : Caller.query -> query
          val callee_model_of_query : query -> Callee.query
          val response_of_callee_model : Callee.response -> response
          val caller_model_of_response : response -> Caller.response
          val client_pushes_back : bool
        end) : sig
        val rpc : (Version.query, Version.response) Plain_rpc.t

        (** [implement'] is like [Plain_rpc.implement rpc] except that it allows the
            server to control the conversion from the [response] to parts. *)
        val implement'
          :  ?on_exception:Rpc.On_exception.t (** default: [On_exception.continue] *)
          -> ('conn_state
              -> Version.query
              -> Version.Response.Intermediate.Part.t Pipe.Reader.t Deferred.Or_error.t)
          -> 'conn_state Rpc.Implementation.t
      end

      include
        S
        with type caller_query := Caller.query
        with type caller_response := Caller.response
        with type callee_query := Callee.query
        with type callee_response := Callee.response
    end
  end
end
