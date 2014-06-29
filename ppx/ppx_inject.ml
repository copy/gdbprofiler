open Asttypes
open! Location
open Parsetree
open Ast_helper
open Ast_convenience

let fatal loc s =
  Location.print_error Format.err_formatter loc;
  prerr_endline s;
  exit 2

let param name typ loc =
  let get_constr t =
    match t.ptyp_desc with
    | Ptyp_constr ({txt=Longident.Lident s; _}, tl) -> Some (s, tl)
    | _ -> None
  in
  let get_func t =
    match get_constr t with
    | Some ("int",[]) -> Some "int"
    | Some ("string",[]) -> Some "string"
    | _ -> None
  in
  let func =
    let (mapper, f) = 
      match get_func typ with
      | Some f -> "assoc", f
      | None ->
        match get_constr typ with
        | Some ("option", [t]) -> (match get_func t with Some f -> "assoc_opt", f | None -> fatal loc "bad type")
        | _ -> fatal loc "bad type"
    in
    app (evar mapper) [evar f; str name; evar "raw"]
  in
  (name, func)

let gen_builder tdecl =
  match has_attr "inject" tdecl.ptype_attributes, tdecl.ptype_kind with
  | true, Ptype_record fields ->
      let field pld = param pld.pld_name.txt pld.pld_type pld.pld_loc in
      let fields = List.map field fields in
      let f = lam (pvar "raw") (record fields) in
      let s = Str.value Nonrecursive [Vb.mk (pvar tdecl.ptype_name.txt) f] in
      [s]
  | true, _ -> fatal tdecl.ptype_loc "Only records are supported"
  | false, _ -> []

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
