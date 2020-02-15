%{
    open Ast
    open Lexing
    open Parsing
    
    let error_string_format msg start finish =
        Printf.eprintf "(line %d: char %d..%d): %s\n"
            start.pos_lnum
            (start.pos_cnum - start.pos_bol)
            (finish.pos_cnum - finish.pos_bol)
            msg

    let parse_error msg = error_string_format msg (rhs_start_pos 1) (rhs_end_pos 1);;

    (*set_trace true*)

%}

%token ELSE FOR IF NULL RETURN STRUCT WHILE EOF
%token FLOAT INT STRING CHAR VOID
%token <string> C_INT C_FLOAT
%token <string> IDENT
%token SC 
%token COMMA LP RP LCB RCB  LSB RSB

%token STAR
%token PTR
%token EQ
%token OR
%token AND
%token EQUAL NEQUAL
%token LT SLT GT SGT
%token PLUS MINUS
%token MULT DIV MOD


%right EQ
%left OR
%left AND
%left EQUAL NEQUAL
%left LT SLT GT SGT
%left PLUS MINUS
%left MULT DIV MOD STAR
%left COMMA SC

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
        | decl_fun      { $1 }
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
        | ident assign_opt { ($1, $2) }
        ;

    assign_opt:
        | EQ expression { Some($2) }
        |               { None }
        ;

    expression:
        | expression PLUS   expression { Binop($1, Plus, $3) } %prec PLUS
        | expression MINUS  expression { Binop($1, Minus, $3) } %prec MINUS
        | expression DIV    expression { Binop($1, Div, $3) } %prec DIV
        | expression STAR   expression { Binop($1, Mult, $3) } %prec MULT
        | expression MOD    expression { Binop($1, Mod, $3) } %prec MOD
        | expression EQ     expression { Binop($1, Equate, $3) } %prec EQ
        | expression EQUAL  expression { Binop($1, Equal, $3) } %prec EQUAL
        | expression NEQUAL expression { Binop($1, NEqual, $3) } %prec NEQUAL
        | expression LT     expression { Binop($1, LowTh, $3) } %prec LT
        | expression GT     expression { Binop($1, GrtTh, $3) } %prec GT
        | expression SLT    expression { Binop($1, StrictLow, $3) } %prec SLT
        | expression SGT    expression { Binop($1, StrictGrt, $3) } %prec SGT
        | expression AND    expression { Binop($1, And, $3) } %prec AND
        | expression OR     expression { Binop($1, Or, $3) } %prec OR
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
        | var_type STAR { Ptr($1) }
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

    decl_fun:
        | var_type ident LP args_opt RP block { FunDecl($1, $2, $4, $6) }
        ;

    args_opt:
        | args  { Some($1) }
        |       { None }
        ;

    args:
        | var_type ident COMMA args { Param($1, $2) :: $4 }
        | var_type ident            { Param($1, $2) :: [] }
        ;

    block:
        | LCB block_lines RCB   { Some($2) }
        | LCB RCB               { None }
        ;

    block_lines:
        | decl_var_init block_lines { Decl($1) :: $2 }
        | decl_var_init             { Decl($1) :: [] }
        | instruction block_lines   { $1 :: $2 }
        | instruction               { $1 :: [] }
        ;
    
    instruction:
        | expression SC { Expr($1) }
        ;

%%
