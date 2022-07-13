open! Core
open! Import

(** Useful for removing the [type t] in the output of one of the streamable functors.

    Note: these functors will never change.  It's fine to use them in stable contexts.
*)
module F (S : Module_type.S) = struct
  module Streamable_impl : Module_type.S with type t := S.t = S
  include Streamable_impl
end

module F_rpc (S : Module_type.S_rpc) = struct
  module Streamable_impl : Module_type.S_rpc with type t := S.t = S
  include Streamable_impl
end
