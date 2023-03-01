open! Base
open! Import

let label_declarations type_dec =
  match type_dec.ptype_kind with
  | Ptype_record label_declarations -> Some label_declarations
  | _ -> None
;;

let children ~loc:_ ~label_declarations =
  List.map label_declarations ~f:(fun label_declaration -> label_declaration.pld_type)
;;

let streamable_module (ctx : Ctx.t) children =
  Nested_tuple.streamable_of_tuple ctx children
;;

let to_streamable_fun ~loc ~label_declarations =
  (* e.g. { a; b } *)
  let record_pattern =
    ppat_record
      ~loc
      (List.map label_declarations ~f:(fun label_declaration ->
         let lident = Loc.map label_declaration.pld_name ~f:lident in
         lident, ppat_var ~loc label_declaration.pld_name))
      Closed
  in
  (* e.g. (a, b) *)
  let tuple_expression =
    pexp_tuple
      ~loc
      (List.map label_declarations ~f:(fun label_declaration ->
         pexp_ident ~loc (Loc.map label_declaration.pld_name ~f:lident)))
  in
  [%expr fun [%p record_pattern] -> [%e tuple_expression]]
;;

let of_streamable_fun ~loc ~label_declarations =
  (* e.g. (a, b) *)
  let tuple_pattern =
    ppat_tuple
      ~loc
      (List.map label_declarations ~f:(fun label_declaration ->
         ppat_var ~loc label_declaration.pld_name))
  in
  (* e.g. { a; b } *)
  let record_expression =
    pexp_record
      ~loc
      (List.map label_declarations ~f:(fun label_declaration ->
         let lident = Loc.map label_declaration.pld_name ~f:lident in
         lident, pexp_ident ~loc lident))
      None
  in
  [%expr fun [%p tuple_pattern] -> [%e record_expression]]
;;

let maybe_match type_ (_ : Ctx.t) =
  Helpers.type_declaration_match
    type_
    ~payload:label_declarations
    ~children:(fun ~loc ~payload -> children ~loc ~label_declarations:payload)
    ~streamable_module
    ~to_streamable_fun:(fun ~loc ~payload ->
      to_streamable_fun ~loc ~label_declarations:payload)
    ~of_streamable_fun:(fun ~loc ~payload ->
      of_streamable_fun ~loc ~label_declarations:payload)
;;
