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

let fix_assoc_name = function
| "thread_groups" -> "thread-groups"
| "typ" -> "type"
| s -> s

let assoc name typ loc var =
  app (extract typ loc) [app (evar "List.assoc") [str (fix_assoc_name name); var]]

let make_field name typ loc =
  let func =
    match get_constr typ with
    | Some ("option", [t]) -> app (evar "assoc_map") [extract t loc; str name; evar "x"]
    | _ -> assoc name typ loc (evar "x")
  in
  (name, func)

let gen_builder tdecl =
  let make_func body =
    let body =
      match find_attr_expr "inject" tdecl.ptype_attributes with
      | Some x when get_lid x = Some "unnamed" -> body
      | _ -> (* this type is represented as name-value pair *)
        let name =
          (* check for name override *)
          match find_attr_expr "inject_name" tdecl.ptype_attributes with
          | Some x -> x
          | None -> str tdecl.ptype_name.txt
        in
        let unwrapped_x = app (evar "named") [name; evar "x"] in
        let_in [Vb.mk (pvar "x") unwrapped_x] body
    in
    Str.value Nonrecursive [Vb.mk (pvar tdecl.ptype_name.txt) (lam (pvar "x") body)]
  in
  match has_attr "inject" tdecl.ptype_attributes, tdecl.ptype_kind, tdecl.ptype_manifest with
  | true, Ptype_record fields, _ ->
      let field pld = make_field pld.pld_name.txt pld.pld_type pld.pld_loc in
      let fields = List.map field fields in
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
                | {pstr_desc = Pstr_type tdecls; _} as i ->
                    i :: (List.flatten (List.map gen_builder tdecls))
                | i -> [this.structure_item this i]
              ) l
           )
      )
  }

let () = Ast_mapper.run_main builder
