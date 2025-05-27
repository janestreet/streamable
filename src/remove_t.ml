open! Core
open! Import

(** Useful for removing the [type t] in the output of one of the streamable functors.

    Note: these functors will never change. It's fine to use them in stable contexts. *)
module%template.portable [@modality p] F (S : Module_type.S) = struct
  module Streamable_impl : sig @@ p
    include Module_type.S with type t := S.t
  end =
    S

  include Streamable_impl
end

module%template.portable [@modality p] F_rpc (S : Module_type.S_rpc) = struct
  module Streamable_impl : sig @@ p
    include Module_type.S_rpc with type t := S.t
  end =
    S

  include Streamable_impl
end
