open! Core
open! Async_kernel
open! Import
include Versioned_pipe_rpc_intf

module Caller_converts = struct
  module type S = Caller_converts

  module Make (Model : sig
      val name : string

      type query
      type response
    end) =
  struct
    let name = Model.name

    type dispatch_fun =
      Rpc.Connection.t
      -> Model.query
      -> Model.response Or_error.t Pipe.Reader.t Deferred.Or_error.t

    let registry : dispatch_fun Callers_rpc_version_table.t =
      Callers_rpc_version_table.create ~rpc_name:name
    ;;

    let dispatch_multi conn_with_menu query =
      let conn = Versioned_rpc.Connection_with_menu.connection conn_with_menu in
      let menu = Versioned_rpc.Connection_with_menu.menu conn_with_menu in
      match Callers_rpc_version_table.lookup_most_recent registry ~callee_menu:menu with
      | Error e -> return (Error e)
      | Ok dispatch -> dispatch conn query
    ;;

    module Register (Version : sig
        type query [@@deriving bin_io]
        type response

        module Response : Main.S_rpc with type t = response

        val version : int
        val query_of_model : Model.query -> query
        val model_of_response : response -> Model.response
        val client_pushes_back : bool
      end) =
    struct
      (* introduce [rpc] *)
      include Pipe_rpc.Make (struct
          let name = name

          include Version
        end)

      let version = Version.version

      let dispatch conn query =
        let open Deferred.Or_error.Let_syntax in
        let query = Version.query_of_model query in
        let%bind response = Pipe_rpc.dispatch rpc conn query in
        let response =
          Pipe.map response ~f:(fun response ->
            Or_error.try_with (fun () -> Version.model_of_response response))
        in
        return response
      ;;

      let () = Callers_rpc_version_table.add_exn registry ~version dispatch
    end
  end
end

module Callee_converts = struct
  module type S = Callee_converts

  module Make (Model : sig
      val name : string

      type query
      type response
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
              -> Model.response Pipe.Reader.t Deferred.Or_error.t)
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
        type response

        module Response : Main.S_rpc with type t = response

        val version : int
        val model_of_query : query -> Model.query
        val response_of_model : Model.response -> response
        val client_pushes_back : bool
      end) =
    struct
      (* introduce [rpc] *)
      include Pipe_rpc.Make (struct
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
          -> Model.response Pipe.Reader.t Deferred.Or_error.t)
        =
        Pipe_rpc.implement ?on_exception rpc (fun conn_state query ->
          let open Deferred.Or_error.Let_syntax in
          let query = Version.model_of_query query in
          let%bind response = f ~version conn_state query in
          let response = Pipe.map response ~f:Version.response_of_model in
          return response)
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
        type response
      end

      module Callee : sig
        type query
        type response
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
        type response

        module Response : Main.S_rpc with type t = response

        val query_of_caller_model : Model.Caller.query -> query
        val callee_model_of_query : query -> Model.Callee.query
        val response_of_callee_model : Model.Callee.response -> response
        val caller_model_of_response : response -> Model.Caller.response
        val client_pushes_back : bool
      end) =
    struct
      include Callee.Register (struct
          include Version

          let model_of_query = callee_model_of_query
          let response_of_model = response_of_callee_model
        end)

      include Caller.Register (struct
          include Version

          let query_of_model = query_of_caller_model
          let model_of_response = caller_model_of_response
        end)
    end

    let dispatch_multi = Caller.dispatch_multi
    let implement_multi = Callee.implement_multi
  end
end
