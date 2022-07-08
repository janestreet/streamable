open! Core

module type S = sig
  type t [@@deriving_inline streamable]

  include Streamable.S with type t := t

  [@@@end]
end

module _ (M : S) = Test.Is_S (M)

module type S_rpc = sig
  type t [@@deriving_inline streamable ~rpc]

  include Streamable.S_rpc with type t := t

  [@@@end]
end

module _ (M : S_rpc) = Test.Is_S_rpc (M)
