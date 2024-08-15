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
  | SDecl of (decl * init option) list
  | SStmts of stmt list
  | SWhile of expr * stmt
  | SDoWhile of stmt * expr
  | SFor1 of (decl * init option) list * expr option * expr option * stmt
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

type item = Var of decl * init option | Function of decl * stmt
[@@deriving show]
