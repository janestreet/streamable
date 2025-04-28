open! Core
open! Async_kernel
open! Import

(** A table keyed by the versions of a particular rpc known to the currently running
    program (i.e. the caller), that supports looking up the most recent (i.e. highest
    version number) rpc known to a particular callee.

    This data structure is used to store per-version dispatch functions for
    caller_converts style versioning. *)
type 'a t

val create : rpc_name:string -> _ t
val add_exn : 'a t -> version:int -> 'a -> unit
val lookup_most_recent : 'a t -> callee_menu:Versioned_rpc.Menu.t -> 'a Or_error.t
val data : 'a t -> 'a list
