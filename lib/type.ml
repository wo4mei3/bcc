open Env
open Syntax

let expr_ty = function
  | EConst (ty, _)
  | EVar (ty, _)
  | EBinary (ty, _, _, _)
  | EAssign (ty, _, _, _)
  | EUnary (ty, _, _) ->
      ty
  | ETySizeof _ -> TBase [ TsInt ]
  | EPostfix (ty, _, _)
  | ECond (ty, _, _, _)
  | ECast (ty, _)
  | ECompoundLit (ty, _) ->
      ty

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
      | TsStructDef (_, mem) | TsUnionDef (_, mem) -> mem
      | TsStruct n -> Option.get (find_struct n)
      | TsUnion n -> Option.get (find_union n)
      | _ -> failwith "not a compound type")
  | _ -> failwith "not a compound type"
