(** parameters passed around that affect code generation *)

open! Base
open! Import

type t =
  { loc : location (** location of the type we're currently traversing *)
  ; rpc : bool (** which of Streamable.[{S,S_rpc}] are we generating *)
  ; version : Version.t (** which Streamable.Stable.V[{NUM}] to use *)
  }
