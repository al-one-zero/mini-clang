open Format
open Parsing
open Lexing
open Printf
open Ast

let ifile = ref ""

let set_file f s = f :=s 

let options = []

let usage = "usage: clang [filename]"

let print_error_string (msg, start, finish) =
    eprintf "File \"%s\", line %d, characters %d-%d:\nSyntax error : %s\n"
        !ifile
        start.pos_lnum
        (start.pos_cnum - start.pos_bol)
        (finish.pos_cnum - finish.pos_bol)
        msg


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

let print_decl = function
    | VarDecl (t, lv) -> (print_type t) ^ (List.fold_left 
                                (fun a (t, v) -> 
                                    a ^ (match v with
                                            | Some(va) -> va 
                                            | None -> "None"))
                                "" lv)
    | _ -> ""
(*    | If ->
    | IfThEl ->
    | While ->
    | For ->
    | Return ->
    | Expr ->
    | Block ->
*)


let print_ast = Printf.print ""

let _ = 
  Arg.parse options (set_file ifile) usage;
  if !ifile="" then (eprintf "Aucun fichier a compiler\n@?"; exit 1); 
  if not (Filename.check_suffix !ifile ".c") then 
    (eprintf "Le fichier d'entree doit avoir l'extension .c\n@?";
     Arg.usage options usage;exit 1); 
  
  let f = open_in !ifile in
  let buf = from_channel f in
  let _ = 
      try
        Parser.file Lexer.nexttoken buf
      with 
        | Parser.Parsing_error (e, s, fi) ->  close_in f ;
                                    print_error_string (e, s, fi) ;
                                    exit 1 
        | Lexer.Lexing_error e -> close_in f ; eprintf "Lexical Error : %c\n" e ; exit 1
        | _ -> close_in f ; eprintf "Unknown Error : " ; exit 2
  in close_in f ; exit 0
