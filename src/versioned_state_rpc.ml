open! Core
open! Async_kernel
open! Import
include Versioned_state_rpc_intf

module Caller_converts = struct
  module type S = Caller_converts

  module Make (Model : sig
      val name : string

      type query
      type state
      type update
    end) =
  struct
    let name = Model.name

    type dispatch_fun =
      Rpc.Connection.t
      -> Model.query
      -> (Model.state * Model.update Or_error.t Pipe.Reader.t) Or_error.t
           Deferred.Or_error.t

    let registry : dispatch_fun Callers_rpc_version_table.t =
      Callers_rpc_version_table.create ~rpc_name:name
    ;;

    let dispatch_multi' conn_with_menu query =
      let conn = Versioned_rpc.Connection_with_menu.connection conn_with_menu in
      let menu = Versioned_rpc.Connection_with_menu.menu conn_with_menu in
      match Callers_rpc_version_table.lookup_most_recent registry ~callee_menu:menu with
      | Error e -> return (Error e)
      | Ok dispatch -> dispatch conn query
    ;;

    let dispatch_multi conn_with_menu query =
      dispatch_multi' conn_with_menu query |> Deferred.map ~f:Or_error.join
    ;;

    module Register (Version : sig
        type query [@@deriving bin_io]
        type state

        module State : Main.S_rpc with type t = state

        type update

        module Update : Main.S_rpc with type t = update

        val version : int
        val query_of_model : Model.query -> query
        val model_of_state : state -> Model.state
        val model_of_update : update -> Model.update
        val client_pushes_back : bool
      end) =
    struct
      (* introduce [rpc] *)
      include State_rpc.Make (struct
          let name = name

          include Version
        end)

      let version = Version.version

      let dispatch' conn query =
        let open Deferred.Or_error.Let_syntax in
        let query = Version.query_of_model query in
        let%bind server_response = State_rpc.dispatch' rpc conn query in
        Or_error.map server_response ~f:(fun (state, updates) ->
          let state = Version.model_of_state state in
          let updates =
            Pipe.map updates ~f:(fun update ->
              Or_error.try_with (fun () -> Version.model_of_update update))
          in
          state, updates)
        |> return
      ;;

      let () = Callers_rpc_version_table.add_exn registry ~version dispatch'
    end
  end
end

module Callee_converts = struct
  module type S = Callee_converts

  module Make (Model : sig
      val name : string

      type query
      type state
      type update
    end) =
  struct
    let name = Model.name

    type implementation =
      { implement :
          's.
          ?on_exception:Rpc.On_exception.t
          -> ('s
              -> version:int
              -> Model.query
              -> (Model.state * Model.update Pipe.Reader.t) Deferred.Or_error.t)
          -> 's Rpc.Implementation.t
      }

    let registry : implementation Callers_rpc_version_table.t =
      Callers_rpc_version_table.create ~rpc_name:name
    ;;

    let implement_multi ?on_exception f =
      List.map (Callers_rpc_version_table.data registry) ~f:(fun { implement } ->
        implement ?on_exception f)
    ;;

    module Register (Version : sig
        type query [@@deriving bin_io]
        type state

        module State : Main.S_rpc with type t = state

        type update

        module Update : Main.S_rpc with type t = update

        val version : int
        val model_of_query : query -> Model.query
        val state_of_model : Model.state -> state
        val update_of_model : Model.update -> update
        val client_pushes_back : bool
      end) =
    struct
      (* introduce [rpc] *)
      include State_rpc.Make (struct
          let name = name

          include Version
        end)

      let version = Version.version

      let implement
        (type s)
        ?on_exception
        (f :
          s
          -> version:int
          -> Model.query
          -> (Model.state * Model.update Pipe.Reader.t) Deferred.Or_error.t)
        =
        State_rpc.implement ?on_exception rpc (fun conn_state query ->
          let open Deferred.Or_error.Let_syntax in
          let query = Version.model_of_query query in
          let%bind state, updates = f ~version conn_state query in
          let state = Version.state_of_model state in
          let updates = Pipe.map updates ~f:Version.update_of_model in
          return (state, updates))
      ;;

      let () = Callers_rpc_version_table.add_exn registry ~version { implement }
    end
  end
end

module Both_convert = struct
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
    end) =
  struct
    let name = Model.name

    module Caller = Caller_converts.Make (struct
        let name = name

        include Model.Caller
      end)

    module Callee = Callee_converts.Make (struct
        let name = name

        include Model.Callee
      end)

    module Register (Version : sig
        val version : int

        type query [@@deriving bin_io]
        type state

        module State : Main.S_rpc with type t = state

        type update

        module Update : Main.S_rpc with type t = update

        val query_of_caller_model : Model.Caller.query -> query
        val callee_model_of_query : query -> Model.Callee.query
        val state_of_callee_model : Model.Callee.state -> state
        val caller_model_of_state : state -> Model.Caller.state
        val update_of_callee_model : Model.Callee.update -> update
        val caller_model_of_update : update -> Model.Caller.update
        val client_pushes_back : bool
      end) =
    struct
      include Callee.Register (struct
          include Version

          let model_of_query = callee_model_of_query
          let state_of_model = state_of_callee_model
          let update_of_model = update_of_callee_model
        end)

      include Caller.Register (struct
          include Version

          let query_of_model = query_of_caller_model
          let model_of_state = caller_model_of_state
          let model_of_update = caller_model_of_update
        end)
    end

    let dispatch_multi = Caller.dispatch_multi
    let dispatch_multi' = Caller.dispatch_multi'
    let implement_multi = Callee.implement_multi
  end
end
