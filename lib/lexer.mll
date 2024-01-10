{
  open Lexing
  open Parser

  exception SyntaxError of string
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let digit = ['0'-'9']
let int = '-'? digit+
let char = ['a'-'z' 'A'-'Z']+
let string = char+

rule read =
  parse
    | white { read lexbuf }
    | "(" { LPAREN }
    | ")" { RPAREN }
    | "\\" { LAMBDA }
    | "λ" { LAMBDA }
    | "." { DOT }
    | ':' { COLON }
    | "->" { ARROW }
    | '*'  { STAR }
    | ','  { COMMA }
    | "int" { TINT }
    | "+" { PLUS }
    | "unit" { UNIT }
    | "tt" { UNIT }
    | "fst" { FST }
    | "snd" { SND }
    | "inl" { INL }
    | "inr" { INR }
    | "case" { CASE }
    | "of" { OF }
    | "|" { BAR }
    | "co" { CO }
    | "\\~" { LAMBDABAR }
    | "λ~" { LAMBDABAR }
    | "@" { AT }
    | "#" { skip_line lexbuf }
    | string { IDENT (lexeme lexbuf) }
    | int { INT (int_of_string (lexeme lexbuf)) }
    | newline { END }
    | _ { raise @@ SyntaxError ("unexpected char: " ^ lexeme lexbuf) }
    | eof { EOF }
and skip_line =
  parse
    | newline { new_line lexbuf; read lexbuf }
    | eof { EOF }
    | _ { skip_line lexbuf }
