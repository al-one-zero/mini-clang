open Printf
open List

type bop = Plus  | Minus | Mult | Div | Mod | Equate | Equal | NEqual | LowTh | GrtTh | StrictLow | StrictGrt | And | Or | Dot | Arrow

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

let print_bop = function
    | Plus -> "+"
    | Minus -> "-"
    | Mult -> "*"
    | Div -> "/"
    | Mod -> "%"
    | Equate -> "="
    | Equal -> "=="
    | NEqual -> "!="
    | LowTh -> "<="
    | GrtTh -> ">="
    | StrictLow -> "<"
    | StrictGrt -> ">"
    | And -> "&&"
    | Or -> "||"
    | Dot -> "."
    | Arrow -> "->"

let print_uop = function
    | Pos -> "+"
    | Neg -> "-"
    | Ptr -> "*"
    | Adress -> "&"
    | Not -> "!"
    | Incr -> "++"
    | Decr -> "--"

let print_type = function
    | Void -> "Void"
    | Int -> "Int"
    | _ -> "" 

let print_expr = function
    | Ident i -> i
    | Cst (t, v) -> sprintf "(%s, %s)" (print_type t) v
    | _ -> ""

let rec print_decl = function
    | VarDecl (t, vl) -> sprintf "%s[%s]"
                            (print_type t)
                            (String.concat ", " (map (fun (n, e) ->
                                                n ^ (match e with
                                                        Some v -> print_expr v
                                                        | None -> ""))
                                              vl))
    | StructDecl (n, d) -> sprintf "Struct(%s,\n  %s)" n (match d with
                            | Some dl -> String.concat ", \n" (map print_decl dl)
                            | None -> "")
    | _ -> ""
and print_instr = function
    | If (e, i) -> sprintf "IfThen(%s, %s)" (print_expr e) (print_instr i)
    | IfThEl (e, i, j) -> sprintf "ITE(%s, %s, %s)" (print_expr e) (print_instr i) (print_instr j)
    | While (e, i) -> sprintf "While(%s, %s)" (print_expr e) (print_instr i)
    | _ -> ""
(*
    | For ->
    | Return ->
    | Expr ->
    | Block ->
*)

let print_ast = function
    | File f -> printf "File(\n" ;
                iter (fun d -> printf "  Decl(%s)\n" (print_decl d)) f ;
                printf ")\n"

