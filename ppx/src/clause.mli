(** At the highest level, Ppx_streamable traverses a type expression and transforms it
    into a nested functor application. This traversal proceeds recursively by pattern
    matching against the type expression to determine which functor to use.

    A [Clause.t] is a single clause in this overall pattern match. *)

open! Base
open! Import

module Match : sig
  (** The result of a successful pattern match consists of (1) any type sub-expressions
      that the traversal ought to visit next (i.e. [children]) and (2) how to build up a
      Streamable implementation from the Streamable implementations of these children
      types (i.e. [apply_functor]).

      The [children] are eventually passed into [apply_functor], after first being
      converted from [Type_.t]s to [module_expr]s.

      The reason for splitting these out is so that the recursive logic for converting
      from a [Type_.t] to a [module_expr] is centralized, and matchers do not need to
      re-implement nor worry about that logic. *)
  type t =
    { children : Type_.t list
    ; apply_functor : Ctx.t -> module_expr list -> module_expr
    }
end

(** the clause itself is a function that attempts the pattern match *)
type t = Type_.t -> Ctx.t -> Match.t option
