{
    open Lexing
    open Parser

    exception Lexing_error of char
    
    let kws = [ "else",ELSE;
                "for", FOR;
                "if", IF;
                "int", INT;
                "float", FLOAT;
                "string", STRING;
                "char", CHAR;
                "null", NULL;
                "return", RETURN;
                "struct", STRUCT;
                "void", VOID;
                "while", WHILE;]
    let kws_h = Hashtbl.create (List.length kws)
    let _ = List.iter (fun (l, c) -> Hashtbl.add kws_h l c) kws
    let id_or_kw s =  try Hashtbl.find kws_h s with _ -> IDENT(s)
      
}

let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let exp = ('e' | 'E') ('-' | '+')? (digit)+
let r_float = (digit+ '.' digit* exp?) | (digit* '.' digit+ exp?) | (digit+ exp)
let ident = (alpha | '_')(alpha | '_' | digit)*
let space = [' ' '\t' '\n']

rule nexttoken = parse
    space+          { nexttoken lexbuf }
  | ident as i      { id_or_kw i }
  | ';'             { SC }
  | '('             { LP }
  | ')'             { RP }
  | '{'             { LCB }
  | '}'             { RCB }
  | '['             { LSB }
  | ']'             { RSB }
  | '='             { EQ }
  | ','             { COMMA }
  | '*'             { STAR }
  | '+'             { PLUS }
  | '-'             { MINUS }
  | "=="            { EQUAL }
  | "!="            { NEQUAL }
  | "&&"            { AND }
  | "||"            { OR }
  | '<'             { SLT }
  | '>'             { SGT }
  | "<="            { LT }
  | ">="            { GT }
  | '%'             { MOD }
  | digit+ as i     { C_INT i }
  | r_float as f    { C_FLOAT f }
  | eof             { EOF }
  | _ as t         { raise (Lexing_error t) }
