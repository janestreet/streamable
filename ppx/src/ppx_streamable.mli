open! Base
open! Import

val streamable : Deriving.t

module For_testing : sig
  module Nested_variant = Nested_variant
  module Nested_tuple = Nested_tuple
end
