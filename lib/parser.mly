%{
  open Ast
%}

%token <string> IDENT
%token <int> INT
%token TINT UNIT
%token STAR ARROW PLUS CO
%token LAMBDA LAMBDABAR
%token DOT COLON COMMA AT
%token FST SND INL INR CASE OF BAR
%token LPAREN RPAREN
%token END EOF

%left STAR PLUS AT
%right ARROW

%start <tm option> prog
%%

let prog :=
  | EOF; { None }
  | END; p = prog; { p }
  | t = expr; line_end; { Some t }

let line_end := END | EOF

let ty :=
  | TINT; { TInt }
  | UNIT; { TUnit }
  | t1=ty; STAR; t2=ty; { TProd (t1, t2) }
  | t1=ty; ARROW; t2=ty; { TArrow (t1, t2) }
  | t1=ty; PLUS; t2=ty; { TSum (t1, t2) }
  | CO; t=ty; { TDual t }
  | LPAREN; t=ty; RPAREN; { t }

let expr :=
  | i = INT; { Int i }
  | x = IDENT; { Var x }
  | LPAREN; e = expr; RPAREN; { e }
  | LAMBDA; x = IDENT; COLON; t = ty; DOT; e = expr; { Abs (x, t, e) }
  | e1 = expr; e2 = expr; { App (e1, e2) }
  | e1 = expr; PLUS; e2 = expr; { Plus (e1, e2) }
  | UNIT; { Unit }
  | LPAREN; e1 = expr; COMMA; e2 = expr; RPAREN; { Pair (e1, e2) }
  | FST; e = expr; { Fst e }
  | SND; e = expr; { Snd e }
  | INL; t = ty; e = expr; { Inl (t, e) }
  | INR; t = ty; e = expr; { Inr (t, e) }
  | CASE; e = expr; OF; x1 = IDENT; DOT; e1 = expr; BAR; x2 = IDENT; DOT; e2 = expr; { Case (e, x1, e1, x2, e2) }
  | LAMBDABAR; x = IDENT; COLON; t = ty; DOT; e = expr; { CoAbs (x, t, e) }
  | e1 = expr; AT; e2 = expr; { CoApp (e1, e2) }
