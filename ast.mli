type bop = Plus | Minus | Mult | Div | Mod
type uop = Pos | Neg | Ptr

type vtype = Void | Int | Float | String | Char | StructName of string

type expr = Ident of string
  | Cst of vtype * string
  | Binop of expr * bop * expr
  | Unop of uop * expr
  | Null

type param = Param of vtype * string

type decl = VarDecl of vtype * (string * (expr option)) list
    | FunDecl of vtype * string * param list option * decl list option
    | StructDecl of string * (decl list option)
    | DeclError

type file = File of decl list
