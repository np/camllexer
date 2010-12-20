(****************************************************************************)
(*                                                                          *)
(*                              Objective Caml                              *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright 2006-2010 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the Objective   *)
(*  Caml source tree.                                                       *)
(*                                                                          *)
(****************************************************************************)

(* Authors:
 * - Nicolas Pouillard: initial version
 *)

(** A general convention about the constructors of this data type is to keep
    both the parsed value and the original string representation. For instance
    for int literals, `00000042' will represented by INT (42, "00000042"). *)
type caml_token = (*should we use private here?*)
  | KEYWORD of string (** Commonly used for words like `let', `open'... *)
  | LIDENT of string  (** Lower-case identifiers like `foo', `bar42'... *)
  | UIDENT of string  (** Upper-case identifiers like `Foo', `Bar42'... *)
  | SYMBOL of string  (** Symbol-based identifiers like `*', `++/--'... *)
  | INT of int * string
                      (** Caml int literal like `0', `0xAF', `4_2'...  *)
  | INT32 of int32 * string
                      (** Caml int32 literal like int but with a `l' suffix *)
  | INT64 of int64 * string
                      (** Caml int64 literal like int but with a `L' suffix *)
  | NATIVEINT of nativeint * string
                      (** Caml nativeint literal like int but with a `n' suffix *)
  | FLOAT of float * string
                      (** Caml float literal like `0.42', `12e+23', `0.1e-15'... *)
  | CHAR of char * string
                      (** Caml char literal like `'x'', `'\n'', `\xAF'... *)
  | STRING of string * string
                      (** Caml string literal like `"bla"', `"a\n\xAF"'... *)
  | LABEL of string
                      (** Caml label like `~foo:', `~bar:'... *)
  | OPTLABEL of string
                      (** Caml optional label like `?foo:', `?bar:'... *)
  | QUOTATION of quotation
                      (** Caml(p4) quotations like `<<bla>>', `<:foo<bar>>'... *)
  | ANTIQUOT of string * string
                      (** Caml(p4) anti-quotations like `$bla$', `$foo:bar$'... *)
  | COMMENT of comment
                      (** Caml comments like `(* foo *)'... *)
  | BLANKS of blanks
                      (** Caml blanks like spaces, tabulations... *)
  | NEWLINE of newline
                      (** Caml new lines: `\n', `\r', or `\r\n'. *)
  | LINE_DIRECTIVE of line_directive
                      (** Caml line directives `# 42', `# 2 "f.ml"' *)
  | WARNING of warning
                      (** This token warn about some other tokens of the input
                          but does not consume any input. *)
  | ERROR of string * error
                      (** The erroneous part of the input and an error value
                          explaining the issue. *)

(** The generic quotation type.
    To see how fields are used here is an example:
       <:q_name@q_loc<q_contents>> *)
and quotation = {
  q_name : string;
  q_loc : string;
  q_contents : string;
}

and line_directive = {
  l_blanks1  : blanks;
  (** The number of leading '0' *)
  l_zeros    : int;
  l_linenum  : int;
  l_blanks2  : blanks;
  l_filename : string option;
  l_comment  : comment;
  l_newline  : newline
}

and blanks = string

and comment = string

and newline = LF | CR | CRLF

and warning =
  | Comment_start
  | Comment_not_end
  | Illegal_escape_in_string of string * int
      (** The offending escaping sequence and its offset in the string literal *)

and error =
  | Illegal_character           of char
  | Illegal_escape_in_character of string
  | Unterminated                of (Lexing.position * unterminated) list
  | Literal_overflow            of string

and unterminated =
  | Ucomment
  | Ustring
  | Uquotation
  | Uantiquot

val mkCHAR : string -> caml_token
val mkSTRING : string -> caml_token * caml_token list
val mkINT : string -> caml_token
val mkINT32 : string -> caml_token
val mkINT64 : string -> caml_token
val mkNATIVEINT : string -> caml_token
val mkFLOAT : string -> caml_token
val mkKEYWORD : string -> caml_token
val mkLIDENT : string -> caml_token
val mkUIDENT : string -> caml_token
val mkSYMBOL : string -> caml_token
val mkLABEL : string -> caml_token
val mkOPTLABEL : string -> caml_token
val mkQUOTATION : quotation -> caml_token
val mkANTIQUOT : ?name:string -> string -> caml_token
val mkCOMMENT : comment -> caml_token
val mkBLANKS : blanks -> caml_token
val mkNEWLINE : newline -> caml_token
val mkLINE_DIRECTIVE : line_directive -> caml_token
(*
val mkLINE_DIRECTIVE : ?bl1:blanks -> ?bl2:blanks -> ?zeros:int -> ?s:string ->
                       ?com:string ?nl:string -> int -> caml_token
*)
val mkWARNING : warning -> caml_token
val mkERROR : string -> error -> caml_token

val string_of_quotation : quotation -> string

(** Display a caml token in caml lexical syntax.

    Examples:
      LIDENT "foo"              -> "foo"
      INT(42, "00000042")       -> "00000042"
      CHAR('\n', "\\n")         -> "'\n'"
      STRING("f\"o", "f\\\"o")  -> "\"f\\\"o\""
      ERROR("\"bla", Unterminated Ustring) -> "\"bla"
 *)
val string_of_token : caml_token -> string

(** Turns a caml token in a pair of a name and a list of
    arguments.

    Examples:
      LIDENT "foo"               -> ("LIDENT", ["foo"])
      INT(42, "00000042")        -> ("INT", ["00000042"])
      CHAR('\n', "\\n")          -> ("CHAR", ["'\n'"])
      STRING("f\"o", "f\\\"o")   -> ("STRING", ["\"f\\\"o\""])
      ANTIQUOT("foo","bar")      -> ("ANTIQUOT", ["foo"; "bar"])
      LINE_DIRECTIVE(" ",0,42," ",Some"f.ml","bla",LF)
         -> ("LINE_DIRECTIVE", [" "; "0", "42"; " "; "f.ml"; "bla"; "\n"])
      LINE_DIRECTIVE("",0,42,"",None,"",LF)
         -> ("LINE_DIRECTIVE", ["";"0";"42";"";"";"\n"])
*)
val strings_of_token : caml_token -> (string * string list)

(** Does the oppsite job of show_token *)
val token_of_strings : (string * string list) -> caml_token option

(** Returns the width of the given token. More precisely
    it is equal to String.length <.> string_of_token *)
val token_width : caml_token -> int

(** Show a caml token in an easily parsable format.
    The format is the token name and arguments strings
    separated by one space.

    Examples:
      LIDENT "foo"               -> LIDENT "foo"
      INT(42, "00000042")        -> INT "00000042"
      CHAR('\n', "\\n")          -> CHAR "'\n'"
      STRING("f\"o", "f\\\"o")   -> STRING "\"f\\\"o\""
      ANTIQUOT("foo","bar")      -> ANTIQUOT "foo" "bar"
      LINE_DIRECTIVE("",0,42,"",Some"f.ml","",LF)
        -> LINE_DIRECTIVE "" "0" "42" "" "f.ml" "" "\n"
      LINE_DIRECTIVE("",0,42,"",None,"",LF)
        -> LINE_DIRECTIVE "" "0" "42" "" "" "\n"
*)
val show_token : caml_token -> string

val string_of_error : error -> string

val newline_of_string : string -> newline

val message_of_warning : warning -> string

val eval_char : string -> char
  (** Given a char literal body, it interprets the escape sequences (backslashes)
      and returns the interpreted or raises [Failure] if an incorrect escape
      sequence is found.

      Note that [Camltoken.eval_char (Char.escaped c)] returns [c] *)

val eval_string : string -> string * (int * int) list
  (** [Camltoken.eval_string s]
      Given a literal string body, it interprets the escape sequences (backslashes)
      and return the interpreted string plus a list of incorrect escape
      sequences.
      For each incorrect escape sequence, the list holds the offset of the
      first character of the escape sequence (i.e. the character after the
      backslash), and the length of the offending sequence.

      Note that [fst (Camltoken.eval_string (String.escaped s))] returns [s] *)
