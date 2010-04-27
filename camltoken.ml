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

type quotation =
  { q_name     : string ;
    q_loc      : string ;
    q_shift    : int    ;
    q_contents : string }

type caml_token =
  | KEYWORD       of string
  | SYMBOL        of string
  | LIDENT        of string
  | UIDENT        of string
  | ESCAPED_IDENT of string
  | INT           of int * string
  | INT32         of int32 * string
  | INT64         of int64 * string
  | NATIVEINT     of nativeint * string
  | FLOAT         of float * string
  | CHAR          of char * string
  | STRING        of string * string
  | LABEL         of string
  | OPTLABEL      of string
  | QUOTATION     of quotation
  | ANTIQUOT      of string * string
  | COMMENT       of string
  | BLANKS        of string
  | NEWLINE
  | LINE_DIRECTIVE of int * string option
  | EOI

let sf = Printf.sprintf

let quotation_to_string {q_name=n; q_loc=l; q_shift=_; q_contents=s} =
  let locname = if l = "" then "" else sf "@%s" l in
  if n = "" then sf "<%s<%s>>" locname s
  else sf "<:%s%s<%s>>" n locname s

let token_to_string = function
  | KEYWORD       s      |
    SYMBOL        s      |
    LIDENT        s      |
    UIDENT        s      |
    LABEL         s      |
    OPTLABEL      s      |
    COMMENT       s      |
    INT           (_, s) |
    FLOAT         (_, s) |
    ANTIQUOT      (_, s) |
    BLANKS        s -> s

  | ESCAPED_IDENT s      -> sf "( %s )" s
  | INT32         (_, s) -> sf "%sl" s
  | INT64         (_, s) -> sf "%sL" s
  | NATIVEINT     (_, s) -> sf "%sn" s

  (* 's' is already properly escaped *)
  | CHAR          (_, s) -> sf "'%s'" s

  (* 's' is already properly escaped *)
  | STRING        (_, s) -> sf "\"%s\"" s

  | QUOTATION     q -> quotation_to_string q
  | NEWLINE         -> "\n"
  | EOI             -> assert false
  | LINE_DIRECTIVE (i, Some s) -> sf "# %d \"%s\"\n" i s
  | LINE_DIRECTIVE (i, None) -> sf "# %d\n" i

let show_token = function
  | KEYWORD s       -> sf "KEYWORD %S" s
  | SYMBOL s        -> sf "SYMBOL %S" s
  | LIDENT s        -> sf "LIDENT %S" s
  | UIDENT s        -> sf "UIDENT %S" s
  | INT(_, s)       -> sf "INT %S" s
  | INT32(_, s)     -> sf "INT32 %S" s 
  | INT64(_, s)     -> sf "INT64 %S" s 
  | NATIVEINT(_,s)  -> sf "NATIVEINT %S" s
  | FLOAT(_, s)     -> sf "FLOAT %S" s
  | CHAR(_, s)      -> sf "CHAR %S" s
  | STRING(_, s)    -> sf "STRING \"%s\"" s
                    (* here it's not %S since the string is already escaped *)
  | LABEL s         -> sf "LABEL %S" s
  | OPTLABEL s      -> sf "OPTLABEL %S" s
  | ANTIQUOT(n, s)  -> sf "ANTIQUOT %S %S" n s
  | QUOTATION x     -> sf "QUOTATION %S %S %S" x.q_name x.q_loc x.q_contents
  | COMMENT s       -> sf "COMMENT %S" s
  | BLANKS s        -> sf "BLANKS %S" s
  | NEWLINE         -> sf "NEWLINE"
  | EOI             -> sf "EOI"
  | ESCAPED_IDENT s -> sf "ESCAPED_IDENT %S" s
  | LINE_DIRECTIVE(i, None)   -> sf "LINE_DIRECTIVE \"%d\"" i
  | LINE_DIRECTIVE(i, Some s) -> sf "LINE_DIRECTIVE \"%d\" %S" i s

module Eval = struct

  let valch x = Char.code x - Char.code '0'
  let valch_hex x =
    let d = Char.code x in
    if d >= 97 then d - 87
    else if d >= 65 then d - 55
    else d - 48

  let rec skip_indent = parser
    | [< ' (' ' | '\t'); s >] -> skip_indent s
    | [< >] -> ()

  let skip_opt_linefeed = parser
    | [< ''\010' >] -> ()
    | [< >] -> ()

  let chr c =
    if c < 0 || c > 255 then failwith "invalid char token" else Char.chr c

  let rec backslash = parser
    | [< ''\010' >] -> '\010'
    | [< ''\013' >] -> '\013'
    | [< ''n' >]  -> '\n'
    | [< ''r' >]  -> '\r'
    | [< ''t' >]  -> '\t'
    | [< ''b' >]  -> '\b'
    | [< ''\\' >] -> '\\'
    | [< ''"' >]  -> '"'
    | [< '  ''' >]  -> '''
    | [< '' ' >]  -> ' '
    | [< ' ('0'..'9' as c1); ' ('0'..'9' as c2); ' ('0'..'9' as c3) >] ->
        chr (100 * (valch c1) + 10 * (valch c2) + (valch c3))
    | [< ''x'; ' ('0'..'9' | 'a'..'f' | 'A'..'F' as c1) ;
              ' ('0'..'9' | 'a'..'f' | 'A'..'F' as c2) >] ->
        chr (16 * (valch_hex c1) + (valch_hex c2))

  let rec backslash_in_string strict store = parser
    | [< ''\010'; s >] -> skip_indent s
    | [< ''\013'; s >] -> (skip_opt_linefeed s; skip_indent s)
    | [< x = backslash >] -> store x
    | [< 'c when not strict >] -> (store '\\'; store c)
    | [< >] -> failwith "invalid string token"

  let char s =
    if String.length s = 1 then s.[0]
    else if String.length s = 0 then failwith "invalid char token"
    else match Stream.of_string s with parser
         | [< ''\\'; x = backslash >] -> x
         | [< >] -> failwith "invalid char token"

  let string ?strict s =
    let buf = Buffer.create 23 in
    let store = Buffer.add_char buf in
    let rec parse = parser
      | [< ''\\'; _ = backslash_in_string (strict <> None) store; s >] -> parse s
      | [< 'c; s >] -> (store c; parse s)
      | [< >] -> Buffer.contents buf
    in parse (Stream.of_string s);

end

