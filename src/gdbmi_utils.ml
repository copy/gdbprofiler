
open Gdbmi_types

type 'a values = 'a list

let failwithf fmt = Printf.ksprintf (fun s -> failwith s) fmt

let string = function String x -> x | v -> failwithf "expected String, got %s" @@ show_value v
let int = function String x -> int_of_string x | v -> failwith "expected int, got %s" @@ show_value v
let named name (name',x) = if name = name' then x else failwithf "expected name '%s' got '%s'" name name'
let tuple = function Tuple x -> x | v -> failwithf "expected tuple, got '%s'" @@ show_value v
let list f = function List x -> List.map f x | v -> failwithf "expected list, got '%s'" @@ show_value v
(* let list f = function List x -> List.map f x | Values x -> List.map f x | v -> failwithf "expected list, got '%s'" @@ show_value v *)
let values f = function Values x -> List.map f x | v -> failwithf "expected values, got '%s'" @@ show_value v
let assoc_map map k l = match try Some (List.assoc k l) with Not_found -> None with None -> None | Some x -> Some (map x)
