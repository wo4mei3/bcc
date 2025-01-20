open Syntax

type scope = {
  vars : (string * ty) list;
  structs : (string * (string * ty) list) list;
  unions : (string * (string * ty) list) list;
  enums : (string * string list) list;
}
[@@deriving show]

type stack = scope list

let curr : scope ref = ref { vars = []; structs = []; unions = []; enums = [] }
let stack : scope list ref = ref []

let push () =
  stack := !curr :: !stack;
  curr := { vars = []; structs = []; unions = []; enums = [] }

let pop () =
  curr := List.hd !stack;
  stack := List.tl !stack

let add_var pair = curr := { !curr with vars = pair :: !curr.vars }
let add_struct pair = curr := { !curr with structs = pair :: !curr.structs }
let add_union pair = curr := { !curr with unions = pair :: !curr.unions }
let add_enum pair = curr := { !curr with enums = pair :: !curr.enums }

let is_typedef name decl =
  let rec aux = function
    | TFun (ty, _) | TPtr ty | TArr (ty, _) -> aux ty
    | TBase base -> List.mem ScsTypedef base
  in
  fst decl = name && aux (snd decl)

let find_var name =
  let rec aux = function
    | [] -> None
    | x :: xs when List.mem_assoc name x.vars ->
        let ty = List.assoc name x.vars in
        if is_typedef name (name, ty) then aux xs else Some ty
    | _ :: xs -> aux xs
  in
  if List.mem_assoc name !curr.vars then Some (List.assoc name !curr.vars)
  else aux !stack

let find_typedef name =
  let rec aux = function
    | [] -> None
    | x :: xs -> (
        match List.find_opt (is_typedef name) x.vars with
        | Some x -> Some x
        | None -> aux xs)
  in
  match List.find_opt (is_typedef name) !curr.vars with
  | Some x -> Some x
  | None -> aux !stack

let find_struct name =
  let rec aux = function
    | [] -> None
    | x :: _ when List.mem_assoc name x.structs ->
        Some (List.assoc name x.structs)
    | _ :: xs -> aux xs
  in
  if List.mem_assoc name !curr.structs then Some (List.assoc name !curr.structs)
  else aux !stack

let find_union name =
  let rec aux = function
    | [] -> None
    | x :: _ when List.mem_assoc name x.unions ->
        Some (List.assoc name x.unions)
    | _ :: xs -> aux xs
  in
  if List.mem_assoc name !curr.unions then Some (List.assoc name !curr.unions)
  else aux !stack

let rec get_originty =
  let pred = function TsTypedef _ -> true | _ -> false in
  function
  | TBase base as ty -> (
      try
        match List.find pred base with
        | TsTypedef name -> (
            match find_typedef name with
            | Some (_, ty) -> get_originty ty
            | None -> failwith ("cannot find the typedef origin: " ^ name))
        | _ -> ty
      with _ -> ty)
  | ty -> ty

let get_retty ty =
  match get_originty ty with
  | TFun (ret, _) -> ret
  | _ -> failwith "cannot get the return type"

let get_basety ty =
  match get_originty ty with
  | TPtr ty | TArr (ty, _) -> ty
  | _ -> failwith "not a pointer type"

let get_members ty =
  let pred = function
    | TsStruct _ | TsUnion _ | TsStructDef _ | TsUnionDef _ -> true
    | _ -> false
  in
  match get_originty ty with
  | TBase base -> (
      match
        try List.find pred base with _ -> failwith "not a compound type"
      with
      | TsStructDef (n, _) | TsStruct n -> Option.get (find_struct n)
      | TsUnionDef (n, _) | TsUnion n -> Option.get (find_union n)
      | _ -> failwith "not a compound type")
  | _ -> failwith "not a compound type"
