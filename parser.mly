%token <float> FLOAT
%token PLUS
%token MINUS
%token TIMES
%token DIVIDE
%token COLON
%token LPAREN
%token RPAREN
%token EQUAL
%token <string> CELL
%token <string> FUNCTION
%token EOF

%start <Expr.t> prog

%%

prog :
  | EQUAL; e = expr; EOF { e }
  | f = FLOAT  { Expr.Float f }
  | EOF { Expr.Null }

expr :
  | LPAREN; e = expr; RPAREN { e }
  | p1 = expr; PLUS; p2 = product { Expr.Add (p1,p2) }
  | p1 = expr; MINUS; p2 = product { Expr.Sub (p1,p2) }
  | p = product { p }
  
product :
  | p1 = product; TIMES; p2 = expr2 { Expr.Multiply (p1,p2) }
  | p1 = product; DIVIDE; p2 = expr2 { Expr.Divide (p1,p2) }
  | e = expr2 { e }

expr2 :
  | f = FLOAT  { Expr.Float f }
  | f = FUNCTION; LPAREN; e = expr; RPAREN  { Expr.Function (f, e) }
  | c1 = CELL; COLON; c2 = CELL { Expr.Range(Expr.cell_of_string c1,Expr.cell_of_string c2) }
  | c = CELL { Expr.cell_of_string c } 
