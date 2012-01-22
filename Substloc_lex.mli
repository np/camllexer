type token = LOC of string * (int * int) * (int * int)
           | CHR of char
           | EOI

val loc : Lexing.lexbuf -> token

val line : Lexing.lexbuf -> string option
