{
  open Parser
}

let BLANK = [' ' '\t' '\n']

let LINE = [^ '\n']* '\n'
let COMMENT = '#' LINE

let NUM = ['0'-'9']
let ALPHA =  ['a'-'z' 'A'-'Z' '_' ]
let WORD = ALPHA (ALPHA | NUM)*
let NUMBER =  ['0'-'9']+


rule token = parse	       
	| BLANK   {token lexbuf}     (* Skip blanks and comments*)
	| COMMENT {token lexbuf}
	
	| eof     { EOP }

	| "("      { LPAR }
	| ")"      { RPAR }
	| "ifz"    { IFZ }
	| "then"   { THEN }
	| "else"   { ELSE }
	| "fixfun" { FIXFUN }
	| "let"    { LET }
	| ":="     { BE }
	| "in"     { IN }
	| "fun"    { FUN }
	| "=>"     { MAPS }
	| "+"      { PLUS }
	| "-"      { MINUS }
	| "*"      { MULT }
	| "/"      { DIV }
	| "="      { EQUAL }

	| NUMBER
	    {
	      let s = (Lexing.lexeme lexbuf)
	      in NUM(int_of_string(s))
	    }

	| WORD
	    {
              let s = (Lexing.lexeme lexbuf)
              in VAR(s)
	    }
