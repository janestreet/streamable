open! Base
open! Import

(** an variable-arity [Streamable.Of_variant] *)
val streamable_of_variant : Ctx.t -> (core_type * module_expr) list -> module_expr

module For_testing : sig
  module Path : sig
    type t [@@deriving sexp_of]
  end

  type 'a t [@@deriving sexp_of]

  val create : 'a list -> 'a t
  val paths : 'a t -> (Path.t * 'a) list
end
