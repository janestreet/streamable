open! Base
open! Import

module Tag = struct
  type t =
    | A
    | B
    | C
    | D
  [@@deriving sexp_of]

  let to_string = function
    | A -> "A"
    | B -> "B"
    | C -> "C"
    | D -> "D"
  ;;
end

module Path = struct
  type t = Tag.t list

  let to_string t = List.map ~f:Tag.to_string t |> String.concat
  let sexp_of_t t = [%sexp_of: string] (to_string t)
end

type 'a t =
  | Leaf of 'a
  | V2 of 'a t * 'a t
  | V3 of 'a t * 'a t * 'a t
  | V4 of 'a t * 'a t * 'a t * 'a t

let rec create = function
  | [] -> raise_s [%message [%here] "Error: [nested_variant] of empty constructor list"]
  | [ a ] -> Leaf a
  | [ a; b ] -> V2 (Leaf a, Leaf b)
  | alist ->
    let n = List.length alist in
    let q = n / 4 in
    let r = n % 4 in
    let groups = List.chunks_of alist ~length:(if r = 0 then q else q + 1) in
    (match groups with
     | [ a; b; c ] ->
       let a = create a in
       let b = create b in
       let c = create c in
       V3 (a, b, c)
     | [ a; b; c; d ] ->
       let a = create a in
       let b = create b in
       let c = create c in
       let d = create d in
       V4 (a, b, c, d)
     | _ ->
       (* It's not obvious that the above cases are exhaustive, but they are. See
          [../test/test_nested_variant.ml] to convince yourself. *)
       assert false)
;;

let iteri t ~f =
  let rec loop (path : Path.t) t =
    match t with
    | Leaf a -> f (List.rev path) a
    | V2 (a, b) ->
      loop (A :: path) a;
      loop (B :: path) b
    | V3 (a, b, c) ->
      loop (A :: path) a;
      loop (B :: path) b;
      loop (C :: path) c
    | V4 (a, b, c, d) ->
      loop (A :: path) a;
      loop (B :: path) b;
      loop (C :: path) c;
      loop (D :: path) d
  in
  loop [] t
;;

let paths t =
  let q = Queue.create () in
  iteri t ~f:(fun path value -> Queue.enqueue q (path, value));
  Queue.to_list q
;;

let rec map t ~f =
  let m = map ~f in
  match t with
  | Leaf x0 -> Leaf (f x0)
  | V2 (x0, x1) ->
    let x0 = m x0 in
    let x1 = m x1 in
    V2 (x0, x1)
  | V3 (x0, x1, x2) ->
    let x0 = m x0 in
    let x1 = m x1 in
    let x2 = m x2 in
    V3 (x0, x1, x2)
  | V4 (x0, x1, x2, x3) ->
    let x0 = m x0 in
    let x1 = m x1 in
    let x2 = m x2 in
    let x3 = m x3 in
    V4 (x0, x1, x2, x3)
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
  | V2 (a, b) -> [ Tag.A, a; B, b ]
  | V3 (a, b, c) -> [ Tag.A, a; B, b; C, c ]
  | V4 (a, b, c, d) -> [ Tag.A, a; B, b; C, c; D, d ]
;;

let rec sexp_of_t sexp_of_a t =
  match t with
  | Leaf x -> [%sexp_of: a] x
  | _ -> [%sexp_of: a t list] (List.map ~f:snd (to_toplevel_list t))
;;

let to_list t = paths t |> List.map ~f:snd

let flat_variant_type ~loc t : core_type =
  match t with
  | Leaf x -> x
  | _ ->
    let core_types = to_list t in
    ptyp_variant
      ~loc
      (List.mapi core_types ~f:(fun i core_type ->
         let core_type = { core_type with ptyp_attributes = [] } in
         rtag ~loc (Loc.make ~loc (Helpers.uppercase_name_of_num i)) false [ core_type ]))
      Closed
      None
;;

let pat_con_apply ~loc con arg = ppat_variant ~loc con (Some arg)
let exp_con_apply ~loc con arg = pexp_variant ~loc con (Some arg)
let pat_tag_apply ~loc tag arg = pat_con_apply ~loc (Tag.to_string tag) arg
let exp_tag_apply ~loc tag arg = exp_con_apply ~loc (Tag.to_string tag) arg

let of_flat_variant_case ~loc (path, i) : case =
  let var = Helpers.lowercase_name_of_num i in
  let con = String.capitalize var in
  case
    ~lhs:(List.fold_right path ~init:(Helpers.pat_var ~loc var) ~f:(pat_tag_apply ~loc))
    ~guard:None
    ~rhs:(exp_con_apply ~loc con (Helpers.exp_var ~loc var))
;;

let to_flat_variant_case ~loc (path, i) : case =
  let var = Helpers.lowercase_name_of_num i in
  let con = Helpers.uppercase_name_of_num i in
  case
    ~lhs:(pat_con_apply ~loc con (Helpers.pat_var ~loc var))
    ~guard:None
    ~rhs:(List.fold_right path ~init:(Helpers.exp_var ~loc var) ~f:(exp_tag_apply ~loc))
;;

let of_flat_variant ~loc t =
  let t = number t in
  let paths = paths t in
  let cases = List.map paths ~f:(of_flat_variant_case ~loc) in
  pexp_function ~loc cases
;;

let to_flat_variant ~loc t =
  let t = number t in
  let paths = paths t in
  let cases = List.map paths ~f:(to_flat_variant_case ~loc) in
  pexp_function ~loc cases
;;

let rec streamable_module ctx t =
  match t with
  | Leaf m -> m
  | _ ->
    let ts = to_toplevel_list t |> List.map ~f:snd in
    let arity = List.length ts in
    Helpers.apply_streamable_dot
      ctx
      ~functor_name:[%string "Of_variant%{arity#Int}"]
      ~arguments:(List.map ~f:(streamable_module ctx) ts)
;;

let is_flat = function
  | Leaf _ -> true
  | V2 (Leaf _, Leaf _) -> true
  | V3 (Leaf _, Leaf _, Leaf _) -> true
  | V4 (Leaf _, Leaf _, Leaf _, Leaf _) -> true
  | V2 _ | V3 _ | V4 _ -> false
;;

let streamable_of_variant (ctx : Ctx.t) children =
  let loc = ctx.loc in
  let t = create children in
  if is_flat t
  then streamable_module ctx (map ~f:snd t)
  else
    Helpers.streamable_of_streamable
      ctx
      ~type_t:[%stri type t = [%t flat_variant_type ~loc (map ~f:fst t)]]
      ~streamable_module:(streamable_module ctx (map ~f:snd t))
      ~to_streamable_fun:(to_flat_variant ~loc t)
      ~of_streamable_fun:(of_flat_variant ~loc t)
;;

module For_testing = struct
  module Path = Path

  type nonrec 'a t = 'a t [@@deriving sexp_of]

  let create = create
  let paths = paths
end
