open! Core
include Async_rpc_kernel

module Sequence = struct
  include Sequence

  let cons x xs = shift_right xs x

  (* shift_right?! *)
end
