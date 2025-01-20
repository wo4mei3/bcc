type value =
  | VChar of string
  | VInt of string
  | VFloat of string
  | VStr of string
  | VNull
[@@deriving show]

type binary =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | LShift
  | RShift
  | BitAnd
  | BitXor
  | BitOr
  | LogAnd
  | LogOr
  | Lt
  | Le
  | Gt
  | Ge
  | Eq
  | Ne
  | Comma
[@@deriving show]

type unary = Plus | Minus | BitNot | LogNot | Ref | Deref | Sizeof | Inc | Dec
[@@deriving show]

and expr =
  | EConst of ty * value
  | EVar of ty * string
  | EBinary of ty * binary * expr * expr
  | EAssign of ty * binary option * expr * expr
  | EUnary of ty * unary * expr
  | ETySizeof of ty
  | EPostfix of ty * expr * postfix
  | ECond of ty * expr * expr * expr
  | ECast of ty * expr
  | ECompoundLit of ty * init
[@@deriving show]

and postfix =
  | PCall of expr list
  | PIdx of expr option
  | PDot of string
  | PArrow of string
  | PInc
  | PDec
[@@deriving show]

and init = IScal of expr | IVect of init list [@@deriving show]

and stmt =
  | SDecl of (ds list * (declarator * init option) list) list
  | SStmts of stmt list
  | SWhile of expr * stmt
  | SDoWhile of stmt * expr
  | SFor1 of
      (ds list * (declarator * init option) list) list
      * expr option
      * expr option
      * stmt
  | SFor2 of expr option * expr option * expr option * stmt
  | SIfElse of expr * stmt * stmt
  | SReturn of expr option
  | SLabel of string * stmt
  | SGoto of string
  | SContinue
  | SBreak
  | SSwitch of expr * stmt
  | SCase of expr
  | SDefault
  | SExpr of expr option
[@@deriving show]

and ty =
  | TFun of ty * decl list
  | TPtr of ty
  | TArr of ty * expr option
  | TBase of ds list
[@@deriving show]

and decl = string * ty [@@deriving show]

and ds =
  | TsVarlist
  | TsBool
  | TsInt
  | TsShort
  | TsLong
  | TsChar
  | TsSigned
  | TsUnsigned
  | TsFloat
  | TsDouble
  | TsVoid
  | TsStruct of string
  | TsUnion of string
  | TsEnum
  | TsStructDef of string * (ds list * (declarator * init option) list) list
  | TsUnionDef of string * (ds list * (declarator * init option) list) list
  | TsEnumDef of (string * int option) list
  | TsTypedef of string
  | ScsTypedef
  | ScsExtern
  | ScsStatic
  | ScsAuto
  | ScsRegister
  | TqConst
  | TqVolatile
  | FsInline
  | FsNoreturn
[@@deriving show]

and declarator =
  | DeclPtr of declarator
  | DeclIdent of string
  | DeclArr of declarator * expr option
  | DeclFun of declarator * (ds list * declarator list) list
[@@deriving show]

type item =
  | Decl of ds list * (declarator * init option) list
  | Function of (ds list * declarator) * stmt
  | End
[@@deriving show]

type unit = item list [@@deriving show]

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
