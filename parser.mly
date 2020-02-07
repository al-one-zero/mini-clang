%{
    open Ast
    open Lexing
    open Parsing
    
    let error_string_format msg start finish =
        Printf.eprintf "(line %d: char %d..%d): %s\n" start.pos_lnum
            (start.pos_cnum -start.pos_bol)
            (finish.pos_cnum - finish.pos_bol)
            msg

    let parse_error msg =
        error_string_format msg (rhs_start_pos 1) (rhs_end_pos 1)

%}

%token ELSE FLOAT FOR IF NULL RETURN STRUCT VOID WHILE EOF
%token FLOAT
%token INT
%token STRING
%token CHAR
%token <string> CST
%token <string> IDENT
%token COMMA LP RP LCB RCB  LSB RSB
%token SC 

%token PTR
%token MULT DIV
%token PLUS MINUS

%start file
%type <Ast.file> file

%%

    file:
        | decls EOF { File($1) }
        ;

    decls:
        | decl decls    { $1::$2 }
        |               { [] }
        ;

    decl:
        | decl_var      { Expr($1) }
        | error         { parse_error "Unknown declaration" ; ExprError }
        ;

    decl_var:
        | var_type var SC { Var($2, $1, "") }
        ;

    var_type:
        | VOID      { Void }
        | INT       { Int }
        | FLOAT     { Float }
        | STRING    { String }
        | CHAR      { Char }
        ;

    var:
        | IDENT            { $1 }
        ;

%%
