
%{
  open SourceType

%}

%token LPAR RPAR

%token <int> NUM
%token <string> VAR

%token OFTYPE NAT ARR

%token IFZ THEN ELSE
%token FIXFUN
%token LET BE IN
%token FUN MAPS
%token PLUS MINUS MULT DIV
%token EQUAL

%token EOP

%start make_term             /* Entry point */

%type  <SourceType.term> make_term

%%

make_term:
  | term EOP                      {$1}

typ:
  | LPAR typ RPAR                       { $2 }
  | NAT                                 { Nat }
  | typ ARR typ                         { Arr($1,$3) } 

term:
  | VAR                                           {Var($1)}
  | NUM                                           {Const($1)}
  | term PLUS term                                {Sum($1,$3)}
  | term MINUS term                               {Minus($1,$3)}
  | term MULT term                                {Prod($1,$3)}
  | term DIV term                                 {Div($1,$3)}
  | FUN LPAR VAR OFTYPE typ RPAR MAPS term        {Fun($3,$5,$8)}
  | IFZ term THEN term ELSE term                  {Ifz($2,$4,$6)}
  | term LPAR term RPAR                           {Apply($1,$3)}
  | LET VAR BE term IN term                       {Let($2,$4,$6)}
  | FIXFUN LPAR VAR OFTYPE typ RPAR VAR MAPS term {FixFun($3,$5,$7,$9)}

%%
