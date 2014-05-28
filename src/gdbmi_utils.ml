
open Gdbmi_types

let list = function List x -> x | _ -> assert false
let string = function String x -> x | _ -> assert false
let int = function String x -> int_of_string x | _ -> assert false
let assoc map k l = map @@ List.assoc k l
let assoc_opt map k l = try Some (assoc map k l) with Not_found -> None

