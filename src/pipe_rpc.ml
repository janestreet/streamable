open! Core
open! Async_kernel
open! Import
open Deferred.Or_error.Let_syntax
include Pipe_rpc_intf

(* Implementation-wise, a [Streamable.Pipe_rpc] is just a [Streamable.State_rpc] with a
   trivial state type. *)
type ('q, 'r) t = ('q, unit, 'r) State_rpc.t

module Make (X : S) = struct
  module State_X = struct
    let name = X.name
    let version = X.version

    type query = X.query [@@deriving bin_io]
    type state = unit

    module State = Main.Of_atomic (Unit)

    type update = X.response

    module Update = X.Response

    let client_pushes_back = X.client_pushes_back
  end

  module M = State_rpc.Make (State_X)

  let rpc = M.rpc

  let implement' ?on_exception f =
    let f conn query =
      let%bind response = f conn query in
      return (State_X.State.to_parts () |> Pipe.of_sequence, response)
    in
    M.implement' ?on_exception f
  ;;
end

let description = State_rpc.description

let dispatch' rpc conn query =
  let%bind server_response = State_rpc.dispatch' rpc conn query in
  Or_error.map server_response ~f:(fun ((), response) -> response) |> return
;;

let dispatch rpc conn query = dispatch' rpc conn query |> Deferred.map ~f:Or_error.join

let implement ?on_exception ?leave_open_on_exception rpc f =
  let f conn query =
    let%bind response = f conn query in
    return ((), response)
  in
  State_rpc.implement ?on_exception ?leave_open_on_exception rpc f
;;

let bin_query_shape = State_rpc.bin_query_shape
let bin_response_shape = State_rpc.bin_update_shape
