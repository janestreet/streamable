open! Base
open! Import

let pat_var ~loc name = ppat_var ~loc (Loc.make ~loc name)
let exp_var ~loc name = pexp_ident ~loc (Loc.make ~loc (lident name))
let unsupported_use ~loc ~why = Location.raise_errorf ~loc "ppx_streamable: %s." why

let get_the_one_and_only_type_t type_decs ~loc =
  match type_decs with
  | [ type_dec ] ->
    if String.(type_dec.ptype_name.txt <> "t")
    then unsupported_use ~loc ~why:"only types named [t] are supported"
    else type_dec
  | _ -> unsupported_use ~loc ~why:"mutually-recursive types are not supported"
;;

let apply_streamable_dot ({ loc; rpc; version } : Ctx.t) ~functor_name ~arguments =
  let functor_name =
    match rpc with
    | false -> functor_name
    | true -> [%string "%{functor_name}_rpc"]
  in
  let functor_ =
    pmod_ident
      ~loc
      (Loc.make
         ~loc
         (Longident.parse
            [%string "Streamable.Stable.%{Version.module_name version}.%{functor_name}"]))
  in
  List.fold arguments ~init:functor_ ~f:(fun accum argument ->
    pmod_apply ~loc accum argument)
;;

let split_longident longident =
  match List.rev (Longident.flatten_exn longident) with
  | [] -> invalid_arg "Ppxlib.Longident.flatten"
  | [ last ] -> `prefix None, `last last
  | last :: reversed_prefix ->
    let prefix =
      reversed_prefix |> List.rev |> String.concat ~sep:"." |> Longident.parse
    in
    `prefix (Some prefix), `last last
;;

let if_module_dot_t_then_module' longident =
  match split_longident longident with
  | `prefix (Some module_), `last last when String.(last = "t") -> Some module_
  | _ -> None
;;

let if_module_dot_t_then_module core_type =
  match core_type.ptyp_desc with
  | Ptyp_constr (longident_loc, _) ->
    (match if_module_dot_t_then_module' longident_loc.txt with
     | None -> None
     | Some longident -> Some { longident_loc with txt = longident })
  | _ -> None
;;

let longident_is_like_t longident ~primitive_name ~first_module_name =
  let is_like_primitive () =
    match primitive_name with
    | None -> false
    | Some primitive_name ->
      (match longident with
       | Lident lident -> String.(primitive_name = lident)
       | _ -> false)
  in
  let is_like_module () =
    match if_module_dot_t_then_module' longident with
    | None -> false
    | Some longident ->
      (match Longident.flatten_exn longident with
       | first :: _ -> String.(first_module_name = first)
       | _ -> false)
  in
  is_like_primitive () || is_like_module ()
;;

let core_type_with_atomic_attribute ~loc ~module_dot_t =
  let core_type = ptyp_constr ~loc (Loc.make ~loc (Longident.parse module_dot_t)) [] in
  { core_type with
    ptyp_attributes =
      [ { attr_name = Loc.make ~loc (Attribute.name Attributes.atomic)
        ; attr_payload = PStr []
        ; attr_loc = loc
        }
      ]
  }
;;

let to_streamable ~loc ~body = [%stri let to_streamable = [%e body]]
let of_streamable ~loc ~body = [%stri let of_streamable = [%e body]]

let streamable_of_streamable
  ?type_t
  ctx
  ~streamable_module
  ~to_streamable_fun
  ~of_streamable_fun
  =
  let loc = ctx.Ctx.loc in
  apply_streamable_dot
    ctx
    ~functor_name:"Of_streamable"
    ~arguments:
      [ streamable_module
      ; pmod_structure
          ~loc
          [ Option.value type_t ~default:[%stri type nonrec t = t]
          ; to_streamable ~loc ~body:to_streamable_fun
          ; of_streamable ~loc ~body:of_streamable_fun
          ]
      ]
;;

let name_of_num i ~starting_letter =
  assert (i >= 0);
  let q = i / 26 in
  let r = i % 26 in
  let c = Char.of_int_exn (r + Char.to_int starting_letter) in
  let s = String.of_char c in
  if q = 0 then s else s ^ Int.to_string q
;;

let lowercase_name_of_num = name_of_num ~starting_letter:'a'
let uppercase_name_of_num = name_of_num ~starting_letter:'A'

let type_declaration_match
  type_
  ~payload
  ~streamable_module
  ~to_streamable_fun
  ~of_streamable_fun
  ~children
  =
  match (type_ : Type_.t) with
  | Core_type (_ : core_type) -> None
  | Type_declaration type_dec ->
    let%map payload = payload type_dec in
    let children_types = children ~loc:(Type_.loc type_) ~payload in
    ({ children = List.map ~f:Type_.core_type children_types
     ; apply_functor =
         (fun ctx children_modules ->
           let loc = ctx.loc in
           let children = List.zip_exn children_types children_modules in
           streamable_of_streamable
             ctx
             ~streamable_module:(streamable_module ctx children)
             ~to_streamable_fun:(to_streamable_fun ~loc ~payload)
             ~of_streamable_fun:(of_streamable_fun ~loc ~payload))
     }
      : Clause.Match.t)
;;

let polymorphic_primitive_or_module_match
  ~num_type_parameters
  ~primitive_name
  ~first_module_name
  type_
  (_ : Ctx.t)
  =
  let%bind core_type = Type_.match_core_type type_ in
  let%map type_parameters =
    match core_type.ptyp_desc with
    | Ptyp_constr (longident_loc, type_parameters) ->
      (match longident_is_like_t longident_loc.txt ~primitive_name ~first_module_name with
       | false -> None
       | true ->
         assert (List.length type_parameters = num_type_parameters);
         Some type_parameters)
    | _ -> None
  in
  ({ children = List.map type_parameters ~f:Type_.core_type
   ; apply_functor =
       (fun ctx children ->
         apply_streamable_dot
           ctx
           ~functor_name:[%string "Of_%{String.lowercase first_module_name}"]
           ~arguments:children)
   }
    : Clause.Match.t)
;;

let module_name_for_type_parameter = function
  | `Ptyp_var name -> String.capitalize name
  | `Ptyp_any index -> [%string "Unnamed_type_parameter%{index#Int}"]
;;

let make_streamable = "Make_streamable"

let streamable_module_type ~loc ~rpc =
  pmty_ident
    ~loc
    (Loc.make ~loc (Longident.parse (if rpc then "Streamable.S_rpc" else "Streamable.S")))
;;
