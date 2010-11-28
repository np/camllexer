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
type caml_token =
  | KEYWORD of string (** Commonly used for words like `let', `open'... *)
  | LIDENT of string  (** Lower-case identifiers like `foo', `bar42'... *)
  | UIDENT of string  (** Upper-case identifiers like `Foo', `Bar42'... *)
  | SYMBOL of string  (** Symbol-based identifiers like `*', `++/--'... *)
  | PSYMBOL of blanks * string * blanks
                      (** Parenthesized (or prefix) symbol-based
                          identifiers like `(+)', `( * )', `( */)'...   *)
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
  | LINE_DIRECTIVE of blanks * int * blanks * string option * comment
                      (** Caml line directives `# 42', `# 2 "bla"' *)
  | ERROR of error
                      (** Represent an error found in the input stream *)
  | EOI
                      (** End of input *)

(** The generic quotation type.
    To see how fields are used here is an example:
       <:q_name@q_loc<q_contents>>
    The last one, q_shift is equal to the length of "<:q_name@q_loc<". *)
and quotation = {
  q_name : string;
  q_loc : string;
  q_shift : int;
  q_contents : string;
}

and blanks = string
and comment = string

and newline = LF | CR | CRLF

and error =
  | Illegal_character of char
  | Illegal_escape    of string
  | Unterminated      of unterminated
  | Literal_overflow  of string

and unterminated =
  | Ucomment
  | Ustring
  | Ustring_in_comment
  | Uquotation
  | Uantiquot

exception Error of error

val mkCHAR : string -> caml_token
val mkSTRING : string -> caml_token
val mkINT : string -> caml_token
val mkINT32 : string -> caml_token
val mkINT64 : string -> caml_token
val mkNATIVEINT : string -> caml_token
val mkFLOAT : string -> caml_token

val string_of_quotation : quotation -> string

(** Display a caml token in caml lexical syntax.

    Note that this function raises the Error
    exception on the ERROR keyword.

    Examples:
      LIDENT "foo"              -> "foo"
      INT(42, "00000042")       -> "00000042"
      CHAR('\n', "\\n")         -> "'\n'"
      STRING("f\"o", "f\\\"o")  -> "\"f\\\"o\""
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
      LINE_DIRECTIVE(" ",42," ",Some"f","bla")
         -> ("LINE_DIRECTIVE", [" "; "42"; " "; "f"; "bla"])
      LINE_DIRECTIVE("",42,"",None,"")
         -> ("LINE_DIRECTIVE", ["";"42";"";""])
*)
val strings_of_token : caml_token -> (string * string list)

(** Does the oppsite job of show_token *)
val token_of_strings : (string * string list) -> caml_token option

(** Show a caml token in an easily parsable format.
    The format is the token name and arguments strings
    separated by one space.

    Examples:
      LIDENT "foo"               -> LIDENT "foo"
      INT(42, "00000042")        -> INT "00000042"
      CHAR('\n', "\\n")          -> CHAR "'\n'"
      STRING("f\"o", "f\\\"o")   -> STRING "\"f\\\"o\""
      ANTIQUOT("foo","bar")      -> ANTIQUOT "foo" "bar"
      LINE_DIRECTIVE("",42,"",Some"f","")
        -> LINE_DIRECTIVE "" "42" "" "f" ""
      LINE_DIRECTIVE("",42,"",None,"")
        -> LINE_DIRECTIVE "" "42" "" ""
*)
val show_token : caml_token -> string

val string_of_error : error -> string

module Eval : sig
  val char : string -> char
      (** Convert a char token, where the escape sequences (backslashes)
          remain to be interpreted; raise [Failure] if an
          incorrect backslash sequence is found; [Token.Eval.char (Char.escaped c)]
          returns [c] *)

  val string : ?strict:unit -> string -> string
      (** [Taken.Eval.string strict s]
          Convert a string token, where the escape sequences (backslashes)
          remain to be interpreted; raise [Failure] if [strict] and an
          incorrect backslash sequence is found;
          [Token.Eval.string strict (String.escaped s)] returns [s] *)
end
