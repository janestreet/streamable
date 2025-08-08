open! Core
open! Async_kernel
open! Import
open Deferred.Or_error.Let_syntax
include Plain_rpc_intf

(* Implementation-wise, a [Streamable.Plain_rpc] is just a [Streamable.State_rpc] with the
   pipe unused. *)
let plain_impl_to_state f conn query =
  let%bind response = f conn query in
  let empty = Pipe.create_reader ~close_on_exception:false (fun _ -> Deferred.unit) in
  Deferred.Or_error.return (response, empty)
;;

module Update = Main.Of_atomic (Nothing)

type ('q, 'r) t = ('q, 'r, Nothing.t) State_rpc.t

module Direct_writer = struct
  type 'response_part t =
    ('response_part, Update.Intermediate.Part.t) State_rpc.Direct_writer.t

  let write_response_without_pushback_exn =
    State_rpc.Direct_writer.write_state_without_pushback_exn
  ;;

  let finalise_response_without_pushback_exn =
    State_rpc.Direct_writer.finalise_state_without_pushback_exn
  ;;

  let is_response_finalised = State_rpc.Direct_writer.is_state_finalised
  let response_finalised = State_rpc.Direct_writer.state_finalised
  let close = State_rpc.Direct_writer.close
  let closed = State_rpc.Direct_writer.closed
  let flushed = State_rpc.Direct_writer.flushed
  let is_closed = State_rpc.Direct_writer.is_closed

  module Expert = struct
    let create_response_part ~bin_writer response_part =
      State_rpc.Direct_writer.Expert.create_state_part
        ~state_bin_writer:bin_writer
        response_part
    ;;

    let finalise_response_message = State_rpc.Direct_writer.Expert.finalise_state_message
    let write_without_pushback = State_rpc.Direct_writer.Expert.write_without_pushback
  end
end

module Make (X : S) = struct
  module State_X = struct
    let name = X.name
    let version = X.version

    type query = X.query [@@deriving bin_io]
    type state = X.response

    module State = X.Response

    type update = Nothing.t

    module Update = Update

    let client_pushes_back = X.client_pushes_back
  end

  module M = State_rpc.Make (State_X)

  let rpc = M.rpc
  let implement' ?on_exception f = M.implement' ?on_exception (plain_impl_to_state f)
  let implement_direct ?on_exception f = M.implement_direct ?on_exception f
end

let description = State_rpc.description

let dispatch' rpc conn query =
  let%bind server_response = State_rpc.dispatch' rpc conn query in
  Or_error.map server_response ~f:(fun (response, pipe) ->
    Pipe.close_read pipe;
    response)
  |> return
;;

let dispatch rpc conn query = dispatch' rpc conn query |> Deferred.map ~f:Or_error.join

let dispatch_with_rpc_result_and_metadata rpc conn query ~metadata =
  let%bind.Deferred.Result server_response =
    State_rpc.Expert.dispatch_with_rpc_result_and_metadata rpc conn query ~metadata
  in
  Or_error.map server_response ~f:(fun (response, pipe) ->
    Pipe.close_read pipe;
    response)
  |> Deferred.Result.return
;;

let dispatch_with_rpc_result =
  dispatch_with_rpc_result_and_metadata ~metadata:Rpc_metadata.V2.empty
;;

module Expert = struct
  let dispatch_with_rpc_result_and_metadata = dispatch_with_rpc_result_and_metadata
end

let implement ?on_exception rpc f =
  let leave_open_on_exception =
    (* Exceptions would need to come from the streamable part unfolding so it makes sense
       to close the pipe. *)
    false
  in
  State_rpc.implement ?on_exception rpc (plain_impl_to_state f) ~leave_open_on_exception
;;

let implement_with_auth ?on_exception rpc f =
  State_rpc.implement_with_auth
    ?on_exception
    rpc
    (fun conn query ->
      let%map.Async_rpc_kernel.Or_not_authorized.Deferred response = f conn query in
      let empty = Pipe.create_reader ~close_on_exception:false (fun _ -> Deferred.unit) in
      response, empty)
    ~leave_open_on_exception:false
;;

let bin_query_shape = State_rpc.bin_query_shape
let bin_response_shape = State_rpc.bin_state_shape
let version = State_rpc.version
let name = State_rpc.name
