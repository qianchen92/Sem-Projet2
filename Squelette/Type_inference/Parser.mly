
%{
  open SourceType

%}

%token LPAR RPAR

%token <int> NUM
%token <string> VAR

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

term:
  | VAR                                 {Var($1)}
  | NUM                                 {Const($1)}
  | term PLUS term                      {Sum($1,$3)}
  | term MINUS term                     {Minus($1,$3)}
  | term MULT term                      {Prod($1,$3)}
  | term DIV term                       {Div($1,$3)}
  | FUN VAR MAPS term                   {Fun($2,$4)}
  | IFZ term THEN term ELSE term        {Ifz($2,$4,$6)}
  | term LPAR term RPAR                 {Apply($1,$3)}
  | LET VAR BE term IN term             {Let($2,$4,$6)}
  | FIXFUN VAR VAR MAPS term            {FixFun($2,$3,$5)}

%%
