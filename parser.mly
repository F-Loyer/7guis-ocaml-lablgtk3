%token <float> FLOAT
%token PLUS
%token MINUS
%token TIMES
%token DIVIDE
%token COLON
%token COMMA
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
  | f = FLOAT; EOF  { Expr.Float f }
  | PLUS; f = FLOAT; EOF  { Expr.Float f }
  | MINUS; f = FLOAT; EOF  { Expr.Float (-. f) }
  | EOF { Expr.Null }

expr :
  | p1 = expr; PLUS; p2 = product { Expr.Add (p1,p2) }
  | p1 = expr; MINUS; p2 = product { Expr.Sub (p1,p2) }
  | PLUS; p = product { Expr.UnaryPlus p }
  | MINUS; p = product { Expr.UnaryMinus p }
  | p = product { p }
  
product :
  | p1 = product; TIMES; p2 = simple_expr { Expr.Multiply (p1,p2) }
  | p1 = product; DIVIDE; p2 = simple_expr { Expr.Divide (p1,p2) }
  | e = simple_expr { e }

simple_expr :
  | LPAREN; e = expr; RPAREN { e }
  | f = FLOAT  { Expr.Float f }
  | f = FUNCTION; LPAREN; e = separated_list(COMMA,expr); RPAREN  { Expr.Function (f, e) }
  | c1 = CELL; COLON; c2 = CELL { Expr.range_of_cells (Expr.cell_of_string c1) (Expr.cell_of_string c2) }
  | c = CELL { Expr.cell_of_string c } 
