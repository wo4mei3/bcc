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
  | EConst of value
  | EVar of string
  | EBinary of binary * expr * expr
  | EAssign of binary option * expr * expr
  | EUnary of unary * expr
  | ETySizeof of ty
  | EPostfix of expr * postfix
  | ECond of expr * expr * expr
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
  | SDecl of decl * init
  | SStmts of stmt list
  | SWhile of expr * stmt
  | SDoWhile of stmt * expr
  | SFor of stmt * expr option * expr option * stmt
  | SIfElse of expr * stmt * stmt
  | SReturn of expr option
  | SLabel of string * stmt
  | SGoto of string
  | SSwitch of expr * stmt list
  | SCase of expr * stmt list
  | SDefault of stmt list
  | SExpr of expr option
[@@deriving show]

and ty =
  | TFun of ty * decl list
  | TPtr of ty
  | TArr of ty * expr
  | TDeclSpec of ds list
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
  | TsStruct
  | TsUnion
  | TsEnum
  | TsStructDef of (string * ty) list
  | TsUnionDef of (string * ty) list
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
