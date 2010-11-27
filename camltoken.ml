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

type blanks = string
type comment = string

type caml_token =
  | KEYWORD       of string
  | LIDENT        of string
  | UIDENT        of string
  | SYMBOL        of string
  | PSYMBOL       of blanks * string * blanks
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
  | COMMENT       of comment
  | BLANKS        of blanks
  | NEWLINE       of newline
  | LINE_DIRECTIVE of blanks * int * blanks * string option * comment
  | EOI

and newline = LF | CR | CRLF

let sf = Printf.sprintf

let quotation_to_string {q_name=n; q_loc=l; q_shift=_; q_contents=s} =
  let locname = if l = "" then "" else sf "@%s" l in
  if n = "" then sf "<%s<%s>>" locname s
  else sf "<:%s%s<%s>>" n locname s

let for_all p s =
  let len = String.length s in
  let rec loop i = i >= len || p s.[i] && loop (i+1)
  in loop 0

let blank c = List.mem c [' '; '\t'; '\012']
let newline c = List.mem c ['\r'; '\n']

let blanks = for_all blank

let no_newline = for_all (fun c -> not (newline c))

let newline_to_string = function
  | LF   -> "\n"
  | CR   -> "\r"
  | CRLF -> "\r\n"

let escaped_ident_to_string pre_blanks op post_blanks =
  assert (op <> "");
  assert (blanks pre_blanks);
  assert (blanks post_blanks);
  let pre_blanks =
    if pre_blanks = "" && op.[0] = '*' then " " else pre_blanks
  in
  let post_blanks =
    if post_blanks = "" && op.[String.length op - 1] = '*'
    then " " else post_blanks
  in
  sf "(%s%s%s)" pre_blanks op post_blanks

let token_to_string = function
  | KEYWORD       s      |
    SYMBOL        s      |
    LIDENT        s      |
    UIDENT        s      |
    COMMENT       s      |
    INT           (_, s) |
    FLOAT         (_, s) |
    BLANKS        s -> s

  | LABEL         s      -> sf "~%s:" s
  | OPTLABEL      s      -> sf "?%s:" s
  | PSYMBOL       (x,y,z)-> escaped_ident_to_string x y z
  | INT32         (_, s) -> sf "%sl" s
  | INT64         (_, s) -> sf "%sL" s
  | NATIVEINT     (_, s) -> sf "%sn" s

  (* 's' is already properly escaped *)
  | CHAR          (_, s) -> sf "'%s'" s

  (* 's' is already properly escaped *)
  | STRING        (_, s) -> sf "\"%s\"" s

  | ANTIQUOT     ("", c) -> sf "$%s$" c
  | ANTIQUOT      (n, c) -> sf "$%s:%s$" n c
  | QUOTATION     q -> quotation_to_string q
  | NEWLINE nl      -> newline_to_string nl
  | EOI             -> assert false
  | LINE_DIRECTIVE (bl1, i, bl2, sopt, com) ->
      assert (blanks bl1);
      assert (blanks bl2);
      assert (no_newline com);
      match sopt with
      | Some s ->
          sf "#%s%d%s\"%s\"%s\n" bl1 i bl2 s com
      | None ->
          sf "#%s%d%s%s\n" bl1 i bl2 com

let strings_of_token = function
  | KEYWORD s        -> ("KEYWORD", [s])
  | SYMBOL s         -> ("SYMBOL", [s])
  | LIDENT s         -> ("LIDENT", [s])
  | UIDENT s         -> ("UIDENT", [s])
  | INT(_, s)        -> ("INT", [s])
  | INT32(_, s)      -> ("INT32", [s]) 
  | INT64(_, s)      -> ("INT64", [s]) 
  | NATIVEINT(_, s)  -> ("NATIVEINT", [s])
  | FLOAT(_, s)      -> ("FLOAT", [s])
  | CHAR(_, s)       -> ("CHAR", [s])
  | STRING(_, s)     -> ("STRING", [s]) (* here we give the source string *)
  | LABEL s          -> ("LABEL", [s])
  | OPTLABEL s       -> ("OPTLABEL", [s])
  | ANTIQUOT(n, s)   -> ("ANTIQUOT", [n; s])
  | QUOTATION x      -> ("QUOTATION", [x.q_name; x.q_loc;
                                       string_of_int x.q_shift; x.q_contents])
  | COMMENT s        -> ("COMMENT", [s])
  | BLANKS s         -> ("BLANKS", [s])
  | NEWLINE nl       -> ("NEWLINE", [newline_to_string nl])
  | EOI              -> ("EOI", [])
  | PSYMBOL (x,y,z)  -> ("PSYMBOL", [x; y; z])
  | LINE_DIRECTIVE(bl1, i, bl2, sopt, com) ->
      match sopt with
      | None   -> ("LINE_DIRECTIVE", [bl1; string_of_int i; bl2; com])
      | Some s -> ("LINE_DIRECTIVE", [bl1; string_of_int i; bl2; s; com])

let show_token t =
  let (name, args) = strings_of_token t in
  if args = [] then name else
  sf "%s \"%s\"" name (String.concat "\" \"" (List.map String.escaped args))

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

let mkCHAR       s = CHAR(Eval.char s, s)
let mkSTRING     s = STRING(Eval.string s, s)
let mkINT        s = INT(int_of_string s, s)
let mkINT32      s = INT32(Int32.of_string s, s) 
let mkINT64      s = INT64(Int64.of_string s, s) 
let mkNATIVEINT  s = NATIVEINT(Nativeint.of_string s, s)
let mkFLOAT      s = FLOAT(float_of_string s, s)

(* not exported *)
let mkLINE_DIRECTIVE ?(bl1="") ?(bl2="") ?s ?(com="") i =
  assert (blanks bl1);
  assert (blanks bl2);
  assert (no_newline com);
  LINE_DIRECTIVE(bl1, int_of_string i, bl2, s, com)

let token_of_strings = function
  | "KEYWORD", [s]           -> Some (KEYWORD s)
  | "SYMBOL", [s]            -> Some (SYMBOL s)
  | "LIDENT", [s]            -> Some (LIDENT s)
  | "UIDENT", [s]            -> Some (UIDENT s)
  | "INT", [s]               -> Some (mkINT s)
  | "INT32", [s]             -> Some (mkINT32 s) 
  | "INT64", [s]             -> Some (mkINT64 s) 
  | "NATIVEINT", [s]         -> Some (mkNATIVEINT s)
  | "FLOAT", [s]             -> Some (mkFLOAT s)
  | "CHAR", [s]              -> Some (mkCHAR s)
  | "STRING", [s]            -> Some (mkSTRING s)
  | "LABEL", [s]             -> Some (LABEL s)
  | "OPTLABEL", [s]          -> Some (OPTLABEL s)
  | "ANTIQUOT", [n; s]       -> Some (ANTIQUOT(n, s))
  | "QUOTATION", [n;l;s;c]   -> Some (QUOTATION{q_name=n;q_loc=l
                                               ;q_shift=int_of_string s
                                               ;q_contents=c})
  | "COMMENT", [s]           -> Some (COMMENT s)
  | "BLANKS", [s]            -> Some (BLANKS s)
  | "NEWLINE", []            -> Some (NEWLINE LF) (* lax in the input *)
  | "NEWLINE", ["\n"]        -> Some (NEWLINE LF)
  | "NEWLINE", ["\r"]        -> Some (NEWLINE CR)
  | "NEWLINE", ["\r\n"]      -> Some (NEWLINE CRLF)
  | "EOI", []                -> Some EOI
  | "PSYMBOL", [x; y; z]     -> Some (PSYMBOL (x,y,z))
  | "LINE_DIRECTIVE", xs     ->
      begin match xs with
      (* One are a bit lax in the input *)
      | [i]                  -> Some (mkLINE_DIRECTIVE i)
      | [i; s]               -> Some (mkLINE_DIRECTIVE ~s i)

      | [bl1; i; bl2; com]   -> Some (mkLINE_DIRECTIVE ~bl1 ~bl2 ~com i)
      | [bl1; i; bl2; s; com]-> Some (mkLINE_DIRECTIVE ~bl1 ~bl2 ~s ~com i)
      | _                    -> None
      end
  | _                        -> None
