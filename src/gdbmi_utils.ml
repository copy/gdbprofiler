
open Gdbmi_types

type 'a values = 'a list

let string = function String x -> x | _ -> assert false
let int = function String x -> int_of_string x | _ -> assert false
let named name (name',x) = if name = name' then x else assert false
let tuple = function Tuple x -> x | _ -> assert false
let list f = function List x -> List.map f x | _ -> assert false
let values f = function Values x -> List.map f x | _ -> assert false
let assoc_map map k l = match try Some (List.assoc k l) with Not_found -> None with None -> None | Some x -> Some (map x)
