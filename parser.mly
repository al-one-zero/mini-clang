%{
    open Ast
    open Lexing
    open Parsing
    
    let error_string_format msg start finish =
        Printf.eprintf "(line %d: char %d..%d): %s\n" start.pos_lnum
            (start.pos_cnum -start.pos_bol)
            (finish.pos_cnum - finish.pos_bol)
            msg

    let parse_error msg = error_string_format msg (rhs_start_pos 1) (rhs_end_pos 1)


%}

%token ELSE FOR IF NULL RETURN STRUCT WHILE EOF
%token FLOAT INT STRING CHAR VOID
%token <string> C_INT C_FLOAT
%token <string> IDENT
%token SC 
%token COMMA LP RP LCB RCB  LSB RSB

%token PTR
%token MULT DIV
%token PLUS MINUS
%token EQ

%right EQ

%start file
%type <Ast.file> file

%%

    file:
        | decls EOF { File($1) }
        ;

    decls:
        | decl decls    { $1 :: $2 }
        |               { [] }
        ;

    decl:
        | decl_var_init { $1 }
        | decl_struct   { $1 }
        | error         { DeclError }
        ;

    decl_var_init:
        | var_type var_init SC { VarDecl($1, $2) }
        ;

    var_init:
        | var_expr COMMA var_init   { $1 :: $3 } 
        | var_expr                  { $1 :: [] }
        ;
    
    var_expr:
        | ident expr_opt  { ($1, $2) }
        ;

    expr_opt:
        | EQ expression { Some($2) }
        |               { None }
        ;

    expression:
        | C_INT     { Cst(Int, $1) }
        | C_FLOAT   { Cst(Float, $1) }
        | NULL      { Null }
        | ident     { Ident($1) }
        ;

    var_type:
        | VOID          { Void }
        | INT           { Int }
        | FLOAT         { Float }
        | STRING        { String }
        | CHAR          { Char }
        ;

    ident:
        | IDENT            { $1 }
        ;

    decl_struct:
        | STRUCT ident LCB decl_vars_opt RCB SC { StructDecl($2, $4) }
        ;

    decl_vars_opt:
        | decl_vars { Some($1) }
        |           { None }
        ;

    decl_vars:
        | var_type vars SC decl_vars    { VarDecl( $1, $2 ) :: $4 }
        | var_type vars SC              { VarDecl( $1, $2 ) :: [] }
        ;

    vars:
        | ident COMMA vars  { ($1, None) :: $3 }
        | ident             { ($1, None) :: [] }
        ;

%%
