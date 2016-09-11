open Asttypes
open! Location
open Parsetree
open Ast_helper
open Ast_convenience

let fatal loc s =
  Location.print_error Format.err_formatter loc;
  prerr_endline s;
  exit 2

let get_constr t =
  match t.ptyp_desc with
  | Ptyp_constr ({txt=Longident.Lident s; _}, tl) -> Some (s, tl)
  | _ -> None

let rec extract typ loc =
  match get_constr typ with
  | Some (f, []) -> evar f
  | Some (constr, [t]) -> app (evar constr) [extract t loc]
  | _ -> fatal loc "extract: unsupported type"

let make_name def attr =
  (* check for name override *)
  match find_attr_expr "name" attr with
  | Some x -> x
  | None -> str def

let make_field {pld_name; pld_loc; pld_type; pld_attributes; _}  =
  let name = make_name pld_name.txt pld_attributes in
  let func =
    match get_constr pld_type with
    | Some ("option", [t]) -> app (evar "assoc_map") [extract t pld_loc; name; evar "x"]
    | _ -> app (extract pld_type pld_loc) [app (evar "List.assoc") [name; evar "x"]]
  in
  (pld_name.txt, func)

let gen_builder tdecl =
  let make_func body =
    let body =
      match find_attr_expr "inject" tdecl.ptype_attributes with
      | Some x when get_lid x = Some "unnamed" -> body
      | _ -> (* this type is represented as name-value pair *)
        let name = make_name tdecl.ptype_name.txt tdecl.ptype_attributes in
        let unwrapped_x = app (evar "named") [name; evar "x"] in
        let_in [Vb.mk (pvar "x") unwrapped_x] body
    in
    Str.value Nonrecursive [Vb.mk (pvar tdecl.ptype_name.txt) (lam (pvar "x") body)]
  in
  match has_attr "inject" tdecl.ptype_attributes, tdecl.ptype_kind, tdecl.ptype_manifest with
  | true, Ptype_record fields, _ ->
      let fields = List.map make_field fields in
      [make_func @@ let_in [Vb.mk (pvar "x") (app (evar "tuple") [evar "x"])] (record fields)]
  | true, Ptype_abstract, Some ty ->
      [make_func @@ app (extract ty tdecl.ptype_loc) [evar "x"]]
  | true, _, _ -> fatal tdecl.ptype_loc "Unsupported usage"
  | false, _, _ -> []

let gen_builder tdecl =
  with_default_loc tdecl.ptype_loc (fun () -> gen_builder tdecl)

let builder _args =
  let open Ast_mapper in
  let super = default_mapper in
  {super
   with
    structure =
      (fun this l ->
         List.flatten
           (List.map
              (function
                | {pstr_desc = Pstr_type (_, tdecls); _} as i ->
                    i :: (List.flatten (List.map gen_builder tdecls))
                | i -> [this.structure_item this i]
              ) l
           )
      )
  }

let () = Ast_mapper.run_main builder
