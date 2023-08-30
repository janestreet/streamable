open! Base
open! Import

module Match = struct
  type t =
    { children : Type_.t list
    ; apply_functor : Ctx.t -> module_expr list -> module_expr
    }
end

type t = Type_.t -> Ctx.t -> Match.t option
