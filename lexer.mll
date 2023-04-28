{
open Parser
}

let digit = ['0'-'9']
let letter = ['A'-'Z' 'a'-'z']
let num = ((digit | ['1'-'9'] digit*) ('.' digit*)?)

rule read = parse
  | num as n { FLOAT (float_of_string n) }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "/" { DIVIDE }
  | ":" { COLON }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "=" { EQUAL }
  | "," { COMMA }
  | letter digit+ as c { CELL c }
  | letter+ as f { FUNCTION f }
  | eof { EOF }