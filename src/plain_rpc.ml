open! Core
open! Async_kernel
open! Import
open  Deferred.Or_error.Let_syntax
include Plain_rpc_intf

(* Implementation-wise, a [Streamable.Plain_rpc] is just a [Streamable.State_rpc] with the
   pipe unused. *)
let plain_impl_to_state f conn query =
  let%bind response = f conn query in
  let empty = Pipe.create_reader ~close_on_exception:false (fun _ -> Deferred.unit) in
  Deferred.Or_error.return (response, empty)
;;

type ('q, 'r) t = ('q, 'r, Nothing.t) State_rpc.t

module Make (X : S) = struct
  module State_X = struct
    let name    = X.name
    let version = X.version

    type query = X.query [@@deriving bin_io]
    type state = X.response

    module State = X.Response

    type update = Nothing.t

    module Update = Main.Of_atomic (Nothing)

    let client_pushes_back = X.client_pushes_back
  end

  module M = State_rpc.Make (State_X)

  let rpc                        = M.rpc
  let implement' ?on_exception f = M.implement' ?on_exception (plain_impl_to_state f)
end

let description = State_rpc.description

let dispatch ?metadata rpc conn query =
  let%bind response, pipe = State_rpc.dispatch ?metadata rpc conn query in
  Pipe.close_read pipe;
  return response
;;

let implement ?on_exception rpc f =
  State_rpc.implement ?on_exception rpc (plain_impl_to_state f)
;;

let bin_query_shape    = State_rpc.bin_query_shape
let bin_response_shape = State_rpc.bin_state_shape
