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

%}

%token ELSE FOR IF NULL RETURN STRUCT WHILE EOF
%token FLOAT INT CHAR VOID
%token <string> C_INT C_FLOAT C_CHAR
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
%token NOT INCR DECR ADRESS PTR POS NEG
%token ARROW DOT


%right THEN
%right ELSE
%right EQ
%left OR
%left AND
%left EQUAL NEQUAL
%left LT SLT GT SGT
%left PLUS MINUS
%left MULT DIV MOD STAR
%right NOT ADRESS PTR POS NEG
%left LP RP LSB RSB ARROW DOT
%left COMMA SC
%nonassoc INCR DECR

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
        | var_type var_inits SC { VarDecl($1, $2) }
        ;

    var_inits:
        | var_expr COMMA var_inits   { $1 :: $3 } 
        | var_expr                   { $1 :: [] }
        ;
    
    var_expr:
        | ident assign_opt { ($1, $2) }
        ;

    assign_opt:
        | EQ expression         { Some($2) }
        | EQ LCB init RCB       { Some(StructInst(Some($3))) }
        | EQ LCB RCB            { Some(StructInst(None)) }
        |                       { None }
        ;

    init:
        | expression COMMA init     { $1 :: $3 }
        | expression COMMA init_b   { $1 :: $3 }
        | expression                { $1 :: [] }
        ;

    init_b:
        | LCB init RCB  { StructInst(Some($2)) :: [] }
        | LCB RCB       { StructInst(None) :: [] }
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
        | expression LSB expression RSB { Subscript($1, $3) }
        | LP expression RP              { $2 }
        | expression DOT expression     { Binop($1, Dot, $3) } %prec DOT
        | expression ARROW expression   { Binop($1, Arrow, $3) } %prec ARROW
        | expression INCR               { Unop(Incr, $1) } %prec INCR
        | expression DECR               { Unop(Decr, $1) } %prec DECR
        | INCR expression               { Unop(Incr, $2) } %prec INCR
        | DECR expression               { Unop(Decr, $2) } %prec DECR
        | ADRESS expression             { Unop(Adress, $2) } %prec ADRESS
        | NOT expression                { Unop(Not, $2) } %prec NOT
        | MINUS expression              { Unop(Neg, $2) } %prec NEG
        | PLUS expression               { Unop(Pos, $2) } %prec POS
        | C_INT     { Cst(Int, $1) }
        | C_FLOAT   { Cst(Float, $1) }
        | C_CHAR    { Cst(Char, $1) }
        | NULL      { Null }
        | ident     { Ident($1) }
        ;

    var_type:
        | VOID          { Void }
        | INT           { Int }
        | FLOAT         { Float }
        | CHAR          { Char }
        | var_type STAR { Ptr($1) }
        | STRUCT ident  { StructName($2) }
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
        | block         { Block($1) }
        | IF LP expression RP instruction                           { If($3, $5) } %prec THEN
        | IF LP expression RP instruction ELSE instruction          { IfThEl($3, $5, $7) }
        | WHILE LP expression RP instruction                        { While($3, $5) }
        | FOR LP exprs_opt SC expr_opt SC exprs_opt RP instruction  { For($3, $5, $7, $9) }
        | RETURN expression SC  { Return(Some($2)) }
        | RETURN SC  { Return(None) }
        ;

    exprs_opt:
        | exprs { Some($1) }
        |       { None }
        ;

    exprs:
        | expression COMMA exprs    { $1 :: $3 }
        | expression                { $1 :: [] }
        ;

    expr_opt:
        | expression    { Some($1) }
        |               { None }
        ;
%%
