open! Core
open! Async_kernel
open! Import
module Rpc_version = Int

type 'a t =
  { rpc_name : string
  ; table : 'a Rpc_version.Table.t
  }

let create ~rpc_name = { rpc_name; table = Rpc_version.Table.create () }
let data t = Hashtbl.data t.table

let add_exn (type a) ({ rpc_name; table } : a t) ~version (data : a) =
  match Hashtbl.add table ~key:version ~data with
  | `Ok -> ()
  | `Duplicate ->
    raise_s [%message "rpc registered multiple times" (rpc_name : string) (version : int)]
;;

let lookup_most_recent t ~callee_menu =
  let open Or_error.Let_syntax in
  let rpc_name = t.rpc_name in
  let caller_versions = Rpc_version.Set.of_list (Hashtbl.keys t.table) in
  let%bind version =
    Versioned_rpc.Menu.highest_shared_version ~callee_menu ~rpc_name ~caller_versions
  in
  match Hashtbl.find t.table version with
  | Some data -> Ok data
  | None -> Or_error.error_s [%message "unknown rpc" (rpc_name : string) (version : int)]
;;
