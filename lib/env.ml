open Syntax

type scope = {
  vars : int list;
  structs : int list;
  unions : int list;
  enums : int list;
}
[@@deriving show]

let vars : (string * ty) list ref = ref []
let structs : (string * (string * ty) list) list ref = ref []
let unions : (string * (string * ty) list) list ref = ref []
let enums : (string * string list) list ref = ref []

type stack = scope list

let curr : scope ref = ref { vars = []; structs = []; unions = []; enums = [] }
let stack : scope list ref = ref []

let push () =
  stack := !curr :: !stack;
  curr := { vars = []; structs = []; unions = []; enums = [] }

let pop () =
  curr := List.hd !stack;
  stack := List.tl !stack

let insert id item env =
  let rec aux = function
    | 0, _ :: xs -> item :: xs
    | i, x :: xs -> x :: aux (i - 1, xs)
    | _, [] -> []
  in
  env := List.rev (aux (id - 1, List.rev !env))

let rec find_index name scope env =
  match scope with
  | id :: rest -> (
      match List.nth (List.rev env) (id - 1) with
      | n, item when n = name -> Some (id, item)
      | _ -> find_index name rest env)
  | [] -> None

let is_typedef ty =
  let rec aux = function
    | TFun (ty, _) | TPtr ty | TArr (ty, _) -> aux ty
    | TBase base -> List.mem ScsTypedef base
  in
  aux ty

let find_var name =
  let rec aux = function
    | [] -> None
    | x :: xs -> (
        match find_index name x.vars !vars with
        | Some (id, ty) -> if is_typedef ty then aux xs else Some (id, ty)
        | None -> aux xs)
  in
  aux (!curr :: !stack)

let find_typedef name =
  let rec aux = function
    | [] -> None
    | x :: xs -> (
        match find_index name x.vars !vars with
        | Some (id, ty) -> if is_typedef ty then Some id else aux xs
        | None -> aux xs)
  in
  aux (!curr :: !stack)

let find_struct name =
  let rec aux = function
    | [] -> None
    | x :: xs -> (
        match find_index name x.structs !structs with
        | Some (id, _) -> Some id
        | None -> aux xs)
  in
  aux (!curr :: !stack)

let find_current_struct name =
  let rec aux = function
    | [] -> None
    | x :: xs -> (
        match find_index name x.structs !structs with
        | Some (id, _) -> Some id
        | None -> aux xs)
  in
  aux (!curr :: [])

let find_union name =
  let rec aux = function
    | [] -> None
    | x :: xs -> (
        match find_index name x.unions !unions with
        | Some (id, _) -> Some id
        | None -> aux xs)
  in
  aux (!curr :: !stack)

let find_current_union name =
  let rec aux = function
    | [] -> None
    | x :: xs -> (
        match find_index name x.unions !unions with
        | Some (id, _) -> Some id
        | None -> aux xs)
  in
  aux (!curr :: [])

let add_var item =
  vars := item :: !vars;
  curr := { !curr with vars = List.length !vars :: !curr.vars }

let add_struct (name, item) =
  match find_struct name with
  | Some id -> insert id (name, item) structs
  | None ->
      structs := (name, item) :: !structs;
      curr := { !curr with structs = List.length !structs :: !curr.structs }

let add_union (name, item) =
  match find_current_union name with
  | Some id -> insert id (name, item) unions
  | None ->
      unions := (name, item) :: !unions;
      curr := { !curr with unions = List.length !unions :: !curr.unions }

let add_enum item =
  enums := item :: !enums;
  curr := { !curr with enums = List.length !enums :: !curr.enums }

let rec get_originty =
  let pred = function TsTypedef _ -> true | _ -> false in
  function
  | TBase base as ty -> (
      try
        match List.find pred base with
        | TsTypedef id -> (
            match List.nth (List.rev !vars) (id-1) with _, ty -> get_originty ty)
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
      | TsStruct id ->
          snd (List.nth (List.rev !structs) (id - 1))
      | TsUnion id -> snd (List.nth (List.rev !unions) (id - 1))
      | _ -> failwith "not a compound type")
  | _ -> failwith "not a compound type"
