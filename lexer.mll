{
    open Lexing
    open Parser

    exception Lexing_error of char
    
    let kws = [ "else",ELSE;
                "for", FOR;
                "if", IF;
                "int", INT;
                "float", FLOAT;
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
let eol = '\n'|"\r\n"

rule nexttoken = parse
    space+          { nexttoken lexbuf }
  | "/*"            { comment lexbuf }
  | "//"            { s_comment lexbuf }
  | "*/"            { failwith "Commentaire non ouvert" }
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
  | '/'             { DIV }
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
  | '.'             { DOT }
  | '&'             { ADRESS }
  | '!'             { NOT }
  | "->"            { ARROW }
  | "++"            { INCR }
  | "--"            { DECR }
  | digit+ as i     { C_INT i }
  | r_float as f    { C_FLOAT f }
  | ''' (_? as c) '''    { C_CHAR c }
  | '"' (_? as c) '"'    { C_CHAR c }
  | eof             { EOF }
  | _ as t         { raise (Lexing_error t) }

and comment = parse
    "*/"    { nexttoken lexbuf }
  | _       { comment lexbuf }
  | eof     { failwith "Commentaire non fermé" }

and s_comment = parse
    eol     { nexttoken lexbuf }
  | _       { s_comment lexbuf }
