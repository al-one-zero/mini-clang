type bop = Plus | Minus | Mult | Div | Mod
type uop = Pos | Neg | Ptr
type vtype = Void | Int | Float | String | Char | Struct of string

type expr = Ident of string
  | Cst of vtype * string
  | Binop of expr * bop * expr
  | Unop of uop * expr
  | None

type param = Param of vtype * string

type decl = 
    Var of vtype * (string * expr) list
    | Fun of string * vtype * param list * decl list
    | DeclError

type file = File of decl list
