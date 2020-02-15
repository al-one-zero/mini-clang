type bop = Plus | Minus | Mult | Div | Mod | Equate | Equal | NEqual | LowTh | GrtTh | StrictLow| StrictGrt | And | Or
type uop = Pos | Neg | Ptr

type vtype = Void | Int | Float | String | Char | StructName of string | Ptr of vtype

type expr = Ident of string
  | Cst of vtype * string
  | Binop of expr * bop * expr
  | Unop of uop * expr
  | Null

type param = Param of vtype * string

type decl = VarDecl of vtype * (string * (expr option)) list
    | FunDecl of vtype * string * param list option * instr list option
    | StructDecl of string * (decl list option)
    | DeclError
and instr = Decl of decl | If | IfThEl | While | For | Return of expr option | Expr of expr

type file = File of decl list
