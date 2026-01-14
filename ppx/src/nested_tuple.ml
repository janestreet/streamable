open! Base
open! Import

type 'a t =
  | Leaf of 'a
  | T2 of 'a t * 'a t
  | T3 of 'a t * 'a t * 'a t
  | T4 of 'a t * 'a t * 'a t * 'a t
  | T5 of 'a t * 'a t * 'a t * 'a t * 'a t
  | T6 of 'a t * 'a t * 'a t * 'a t * 'a t * 'a t
  | T7 of 'a t * 'a t * 'a t * 'a t * 'a t * 'a t * 'a t
  | T8 of 'a t * 'a t * 'a t * 'a t * 'a t * 'a t * 'a t * 'a t
  | T9 of 'a t * 'a t * 'a t * 'a t * 'a t * 'a t * 'a t * 'a t * 'a t

let create constructors =
  let rec loop = function
    | [] -> raise_s [%message [%here] "Error: [nested_tuple] of empty constructor list"]
    | [ a ] -> Leaf a
    | [ a; b ] -> T2 (Leaf a, Leaf b)
    | [ a; b; c ] -> T3 (Leaf a, Leaf b, Leaf c)
    | [ a; b; c; d ] -> T4 (Leaf a, Leaf b, Leaf c, Leaf d)
    | alist ->
      let n = List.length alist in
      let q = n / 9 in
      let r = n % 9 in
      let groups = List.chunks_of alist ~length:(if r = 0 then q else q + 1) in
      (match groups with
       | [ a; b; c; d; e ] ->
         let a = loop a in
         let b = loop b in
         let c = loop c in
         let d = loop d in
         let e = loop e in
         T5 (a, b, c, d, e)
       | [ a; b; c; d; e; f ] ->
         let a = loop a in
         let b = loop b in
         let c = loop c in
         let d = loop d in
         let e = loop e in
         let f = loop f in
         T6 (a, b, c, d, e, f)
       | [ a; b; c; d; e; f; g ] ->
         let a = loop a in
         let b = loop b in
         let c = loop c in
         let d = loop d in
         let e = loop e in
         let f = loop f in
         let g = loop g in
         T7 (a, b, c, d, e, f, g)
       | [ a; b; c; d; e; f; g; h ] ->
         let a = loop a in
         let b = loop b in
         let c = loop c in
         let d = loop d in
         let e = loop e in
         let f = loop f in
         let g = loop g in
         let h = loop h in
         T8 (a, b, c, d, e, f, g, h)
       | [ a; b; c; d; e; f; g; h; i ] ->
         let a = loop a in
         let b = loop b in
         let c = loop c in
         let d = loop d in
         let e = loop e in
         let f = loop f in
         let g = loop g in
         let h = loop h in
         let i = loop i in
         T9 (a, b, c, d, e, f, g, h, i)
       | _ ->
         (* It's not obvious that the above cases are exhaustive, but they are. See
            [../test/test_nested_tuple.ml] to convince yourself. *)
         assert false)
  in
  loop constructors
;;

let rec map t ~f =
  let m = map ~f in
  match t with
  | Leaf x0 -> Leaf (f x0)
  | T2 (x0, x1) -> T2 (m x0, m x1)
  | T3 (x0, x1, x2) -> T3 (m x0, m x1, m x2)
  | T4 (x0, x1, x2, x3) -> T4 (m x0, m x1, m x2, m x3)
  | T5 (x0, x1, x2, x3, x4) -> T5 (m x0, m x1, m x2, m x3, m x4)
  | T6 (x0, x1, x2, x3, x4, x5) -> T6 (m x0, m x1, m x2, m x3, m x4, m x5)
  | T7 (x0, x1, x2, x3, x4, x5, x6) -> T7 (m x0, m x1, m x2, m x3, m x4, m x5, m x6)
  | T8 (x0, x1, x2, x3, x4, x5, x6, x7) ->
    T8 (m x0, m x1, m x2, m x3, m x4, m x5, m x6, m x7)
  | T9 (x0, x1, x2, x3, x4, x5, x6, x7, x8) ->
    T9 (m x0, m x1, m x2, m x3, m x4, m x5, m x6, m x7, m x8)
;;

let number t =
  let n = ref 0 in
  map t ~f:(fun _ ->
    let i = !n in
    Int.incr n;
    i)
;;

let to_toplevel_list = function
  | Leaf _ -> assert false
  | T2 (x0, x1) -> [ x0; x1 ]
  | T3 (x0, x1, x2) -> [ x0; x1; x2 ]
  | T4 (x0, x1, x2, x3) -> [ x0; x1; x2; x3 ]
  | T5 (x0, x1, x2, x3, x4) -> [ x0; x1; x2; x3; x4 ]
  | T6 (x0, x1, x2, x3, x4, x5) -> [ x0; x1; x2; x3; x4; x5 ]
  | T7 (x0, x1, x2, x3, x4, x5, x6) -> [ x0; x1; x2; x3; x4; x5; x6 ]
  | T8 (x0, x1, x2, x3, x4, x5, x6, x7) -> [ x0; x1; x2; x3; x4; x5; x6; x7 ]
  | T9 (x0, x1, x2, x3, x4, x5, x6, x7, x8) -> [ x0; x1; x2; x3; x4; x5; x6; x7; x8 ]
;;

let rec sexp_of_t sexp_of_a t : Sexp.t =
  match t with
  | Leaf a -> [%sexp_of: a] a
  | _ -> [%sexp_of: a t list] (to_toplevel_list t)
;;

let rec to_list t =
  match t with
  | Leaf x -> [ x ]
  | _ -> List.concat_map ~f:to_list (to_toplevel_list t)
;;

let flat_tuple_type ~loc t : core_type =
  match t with
  | Leaf x -> x
  | _ -> ptyp_tuple ~loc (to_list t)
;;

let exp_var ~loc i = Helpers.exp_var ~loc (Helpers.lowercase_name_of_num i)
let pat_var ~loc i = Helpers.pat_var ~loc (Helpers.lowercase_name_of_num i)

let rec pattern ~loc t =
  match t with
  | Leaf i -> pat_var ~loc i
  | _ ->
    let ts = to_toplevel_list t in
    ppat_tuple ~loc (List.map ts ~f:(pattern ~loc))
;;

let rec expression ~loc t =
  match t with
  | Leaf i -> exp_var ~loc i
  | _ ->
    let ts = to_toplevel_list t in
    pexp_tuple ~loc (List.map ts ~f:(expression ~loc))
;;

let of_flat_tuple ~loc t =
  match t with
  | Leaf _ -> [%expr fun x -> x]
  | _ ->
    let t = number t in
    let lhs = pattern ~loc t in
    let rhs = pexp_tuple ~loc (List.map ~f:(exp_var ~loc) (to_list t)) in
    [%expr fun [%p lhs] -> [%e rhs]]
;;

let to_flat_tuple ~loc t =
  match t with
  | Leaf _ -> [%expr fun x -> x]
  | _ ->
    let t = number t in
    let lhs = ppat_tuple ~loc (List.map ~f:(pat_var ~loc) (to_list t)) in
    let rhs = expression ~loc t in
    [%expr fun [%p lhs] -> [%e rhs]]
;;

let rec streamable_module (ctx : Ctx.t) t =
  match t with
  | Leaf m -> m
  | _ ->
    let ts = to_toplevel_list t in
    let arity = List.length ts in
    Helpers.apply_streamable_dot
      ctx
      ~functor_name:[%string "Of_tuple%{arity#Int}"]
      ~arguments:(List.map ~f:(streamable_module ctx) ts)
;;

let is_flat = function
  | Leaf _ -> true
  | T2 (Leaf _, Leaf _) -> true
  | T3 (Leaf _, Leaf _, Leaf _) -> true
  | T4 (Leaf _, Leaf _, Leaf _, Leaf _) -> true
  | T5 (Leaf _, Leaf _, Leaf _, Leaf _, Leaf _) -> true
  | T6 (Leaf _, Leaf _, Leaf _, Leaf _, Leaf _, Leaf _) -> true
  | T7 (Leaf _, Leaf _, Leaf _, Leaf _, Leaf _, Leaf _, Leaf _) -> true
  | T8 (Leaf _, Leaf _, Leaf _, Leaf _, Leaf _, Leaf _, Leaf _, Leaf _) -> true
  | T9 (Leaf _, Leaf _, Leaf _, Leaf _, Leaf _, Leaf _, Leaf _, Leaf _, Leaf _) -> true
  | T2 _ | T3 _ | T4 _ | T5 _ | T6 _ | T7 _ | T8 _ | T9 _ -> false
;;

let streamable_of_tuple (ctx : Ctx.t) children =
  let loc = ctx.loc in
  let t = create children in
  if is_flat t
  then streamable_module ctx (map ~f:snd t)
  else
    Helpers.streamable_of_streamable
      ctx
      ~type_t:[%stri type t = [%t flat_tuple_type ~loc (map ~f:fst t)]]
      ~streamable_module:(streamable_module ctx (map ~f:snd t))
      ~to_streamable_fun:(to_flat_tuple ~loc t)
      ~of_streamable_fun:(of_flat_tuple ~loc t)
;;

module For_testing = struct
  type nonrec 'a t = 'a t [@@deriving sexp_of]

  let create = create
end
