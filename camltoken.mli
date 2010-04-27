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

(** The generic quotation type.
    To see how fields are used here is an example:
       <:q_name@q_loc<q_contents>>
    The last one, q_shift is equal to the length of "<:q_name@q_loc<". *)
type quotation = {
  q_name : string;
  q_loc : string;
  q_shift : int;
  q_contents : string;
}

type caml_token =
  | KEYWORD of string
  | SYMBOL of string
  | LIDENT of string
  | UIDENT of string
  | ESCAPED_IDENT of string
  | INT of int * string
  | INT32 of int32 * string
  | INT64 of int64 * string
  | NATIVEINT of nativeint * string
  | FLOAT of float * string
  | CHAR of char * string
  | STRING of string * string
  | LABEL of string
  | OPTLABEL of string
  | QUOTATION of quotation
  | ANTIQUOT of string * string
  | COMMENT of string
  | BLANKS of string
  | NEWLINE
  | LINE_DIRECTIVE of int * string option
  | EOI

val quotation_to_string : quotation -> string

(** Display a caml token in caml lexical syntax.

    Examples:
      LIDENT "foo"              -> "foo"
      INT(42, "00000042")       -> "00000042"
      CHAR('\n', "\\n")         -> "'\n'"
      STRING("f\"o", "f\\\"o")  -> "\"f\\\"o\""
 *)
val token_to_string : caml_token -> string

(** Show a caml token in a easily parsable format.
    The format is the token name and arguments strings
    separated by one space.

    Examples:
      LIDENT "foo"               -> LIDENT "foo"
      INT(42, "00000042")        -> INT "00000042"
      CHAR('\n', "\\n")          -> CHAR "'\n'"
      STRING("f\"o", "f\\\"o")   -> STRING "\"f\\\"o\""
      ANTIQUOT("foo","bar")      -> ANTIQUOT "foo" "bar"
      LINE_DIRECTIVE(42,Some"f") -> LINE_DIRECTIVE "42" "f"
      LINE_DIRECTIVE(42,None)    -> LINE_DIRECTIVE "42"
*)
val show_token : caml_token -> string

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
