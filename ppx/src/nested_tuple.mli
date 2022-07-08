open! Base
open! Import

(** an variable-arity [Streamable.Of_tuple] *)
val streamable_of_tuple : Ctx.t -> (core_type * module_expr) list -> module_expr

module For_testing : sig
  type 'a t [@@deriving sexp_of]

  val create : 'a list -> 'a t
end
