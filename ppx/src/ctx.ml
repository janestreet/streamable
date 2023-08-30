open! Base
open! Import

type t =
  { loc : location
  ; rpc : bool
  ; version : Version.t
  }
