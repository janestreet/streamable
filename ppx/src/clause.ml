open! Base
open! Import

module Match = struct
  type t =
    { children      : Type.t               list
    ; apply_functor : Ctx.t -> module_expr list -> module_expr
    }
end

type t = Type.t -> Match.t option
