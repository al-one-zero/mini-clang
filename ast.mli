type bop = Plus | Minus | Mult | Div | Mod
type uop = Pos | Neg | Ptr
type vtype = Void | Int | Float | String | Char | Struct of string

type expr = Var of string * vtype * string 
  | Cst of string * vtype * string 
  | Binop of expr * bop * expr
  | Unop of uop * expr
type decl = Expr of expr | ExprError
type file = File of decl list
