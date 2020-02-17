type bop = Plus
  | Minus
  | Mult
  | Div
  | Mod
  | Equate
  | Equal
  | NEqual
  | LowTh
  | GrtTh
  | StrictLow
  | StrictGrt
  | And
  | Or
  | Dot
  | Arrow

type uop = Pos | Neg | Ptr | Adress | Not | Incr | Decr

type vtype = Void | Int | Float | Char | StructName of string | Ptr of vtype

type expr = Ident of string
  | Cst of vtype * string
  | StructInst of expr list option
  | Subscript of expr * expr
  | Binop of expr * bop * expr
  | Unop of uop * expr
  | Null

type param = Param of vtype * string

type decl = VarDecl of vtype * (string * (expr option)) list
    | FunDecl of vtype * string * param list option * instr list option
    | StructDecl of string * (decl list option)
and instr = Decl of decl
    | If of expr * instr
    | IfThEl of expr * instr * instr
    | While of expr * instr
    | For of expr list option * expr option * expr list option * instr
    | Return of expr option
    | Expr of expr
    | Block of instr list option

type file = File of decl list

