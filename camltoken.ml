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
  | LINE_DIRECTIVE of line_directive
  | ERROR         of string * error
  | EOI

and newline = LF | CR | CRLF

and error =
  | Illegal_character of char
  | Illegal_escape    of string
  | Unterminated      of unterminated
  | Literal_overflow  of string
(*
  | Comment_start
  | Comment_not_end
*)

and unterminated =
  | Ucomment
  | Ustring
  | Ustring_in_comment
  | Uquotation
  | Uantiquot

and line_directive = {
  l_blanks1  : blanks;
  l_zeros    : int;
  l_linenum  : int;
  l_blanks2  : blanks;
  l_filename : string option;
  l_comment  : comment;
  l_newline  : newline
}

let sf = Printf.sprintf

let string_of_unterminated =
  function
  | Ucomment           -> "Comment not terminated"
  | Ustring            -> "String literal not terminated"
  | Ustring_in_comment -> "This comment contains an unterminated string literal"
  | Uquotation         -> "Quotation not terminated"
  | Uantiquot          -> "Antiquotation not terminated"

let string_of_error =
  function
  | Illegal_character c ->
      sf "Illegal character (%s)" (Char.escaped c)
  | Illegal_escape s ->
      sf "Illegal backslash escape in string or character (%s)" s
  | Unterminated u ->
      string_of_unterminated u
  | Literal_overflow ty ->
      sf "Integer literal exceeds the range of representable integers of type %s" ty

let show_unterminated =
  function
  | Ucomment           -> "Ucomment"
  | Ustring            -> "Ustring"
  | Ustring_in_comment -> "Ustring_in_comment"
  | Uquotation         -> "Uquotation"
  | Uantiquot          -> "Uantiquot"

(* meant to be used by show_token/strings_of_token *)
let show_error =
  function
  | Illegal_character _ -> ("Illegal_character", [])
  | Illegal_escape s    -> ("Illegal_escape",    [s])
  | Unterminated u      -> ("Unterminated",      [show_unterminated u])
  | Literal_overflow ty -> ("Literal_overflow",  [ty])

let string_of_quotation {q_name=n; q_loc=l; q_shift=_; q_contents=s} =
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

let string_of_newline = function
  | LF   -> "\n"
  | CR   -> "\r"
  | CRLF -> "\r\n"

let string_of_psymbol pre_blanks op post_blanks =
  assert (op <> "");
  assert (blanks pre_blanks);
  assert (blanks post_blanks);
  let pre_blanks =
    if pre_blanks = "" && op.[0] = '*' then " " else pre_blanks
  in
  sf "(%s%s%s)" pre_blanks op post_blanks

let string_of_line_directive x =
  assert (blanks x.l_blanks1);
  assert (blanks x.l_blanks2);
  assert (x.l_zeros >= 0);
  assert (no_newline x.l_comment);
  let nl = string_of_newline x.l_newline in
  let zeros = String.make x.l_zeros '0' in
  match x.l_filename with
  | Some s ->
      sf "#%s%s%d%s\"%s\"%s%s" x.l_blanks1 zeros x.l_linenum x.l_blanks2 s x.l_comment nl
  | None ->
      sf "#%s%s%d%s%s%s" x.l_blanks1 zeros x.l_linenum x.l_blanks2 x.l_comment nl

let string_of_token = function
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
  | PSYMBOL       (x,y,z)-> string_of_psymbol x y z
  | INT32         (_, s) -> sf "%sl" s
  | INT64         (_, s) -> sf "%sL" s
  | NATIVEINT     (_, s) -> sf "%sn" s

  (* 's' is already properly escaped *)
  | CHAR          (_, s) -> sf "'%s'" s

  (* 's' is already properly escaped *)
  | STRING        (_, s) -> sf "\"%s\"" s

  | ANTIQUOT     ("", c) -> sf "$%s$" c
  | ANTIQUOT      (n, c) -> sf "$%s:%s$" n c
  | QUOTATION     q -> string_of_quotation q
  | NEWLINE nl      -> string_of_newline nl
  | EOI             -> assert false
  | ERROR (tok, _)  -> tok
  | LINE_DIRECTIVE ld -> string_of_line_directive ld

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
  | NEWLINE nl       -> ("NEWLINE", [string_of_newline nl])
  | EOI              -> ("EOI", [])
  | ERROR (tok, err) -> ("ERROR", tok :: let (x,xs) = show_error err in x :: xs)
  | PSYMBOL (x,y,z)  -> ("PSYMBOL", [x; y; z])
  | LINE_DIRECTIVE{l_blanks1=bl1;l_zeros=zeros;l_linenum=i;l_blanks2=bl2;
                   l_filename=sopt;l_comment=com;l_newline=nl} ->
      let nl = string_of_newline nl in
      match sopt with
      | None   -> ("LINE_DIRECTIVE", [bl1; string_of_int zeros; string_of_int i; bl2; com; nl])
      | Some s -> ("LINE_DIRECTIVE", [bl1; string_of_int zeros; string_of_int i; bl2; s; com; nl])

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

  exception Backtrack

  let peek s i = if i < String.length s then Some s.[i] else None

  let rec skip_indent s i =
    match peek s i with
    | Some (' ' | '\t') -> skip_indent s (i+1)
    | _ -> i

  let skip_opt_linefeed s i =
    match peek s i with
    | Some '\n' -> i + 1
    | _ -> i

  let chr c =
    if c < 0 || c > 255 then failwith "invalid char token" else Char.chr c

  let escaped_char s i = match peek s i with
    | Some '\n' -> '\n', i + 1
    | Some '\r' -> '\r', i + 1
    | Some 'n'  -> '\n', i + 1
    | Some 'r'  -> '\r', i + 1
    | Some 't'  -> '\t', i + 1
    | Some 'b'  -> '\b', i + 1
    | Some '\\' -> '\\', i + 1
    | Some '"'  -> '"',  i + 1
    | Some '\'' -> '\'', i + 1
    | Some ' '  -> ' ',  i + 1
    | Some ('0'..'9' as c1) ->
        begin match peek s (i + 1), peek s (i + 2) with
        | Some ('0'..'9' as c2), Some ('0'..'9' as c3) ->
            chr (100 * (valch c1) + 10 * (valch c2) + (valch c3)), i + 3
        | _ -> raise Backtrack
        end
    | Some 'x' ->
        begin match peek s (i + 1), peek s (i + 2) with
        | (Some ('0'..'9' | 'a'..'f' | 'A'..'F' as c1)),
          (Some ('0'..'9' | 'a'..'f' | 'A'..'F' as c2)) ->
            chr (16 * (valch_hex c1) + (valch_hex c2)), i + 3
        | _ -> raise Backtrack
        end
    | _ -> raise Backtrack

  let eof s i = if peek s i <> None then raise Backtrack

  let char s =
    match String.length s with
    | 1 -> s.[0]
    | 0 -> failwith "invalid char token"
    | _ when s = "\r\n" -> '\n'
    | _ when s.[0] <> '\\' -> failwith "invalid char token"
    | _ -> try let (c, i) = escaped_char s 1 in
               eof s i; c
           with Backtrack -> failwith "invalid char token"

  let backslash_in_string strict store s i =
    match peek s i with
    | Some '\n' -> skip_indent s (i+1)
    | Some '\r' -> skip_indent s (skip_opt_linefeed s (i+1))
    | Some c ->
        begin try let (x, i) = escaped_char s i in store x; i
        with Backtrack when not strict -> (store '\\'; store c; i + 1)
        end
    | None -> failwith "invalid string token"

  let string ?strict s =
    let buf = Buffer.create 23 in
    let store = Buffer.add_char buf in
    let rec parse i =
      match peek s i with
      | Some '\\' -> parse (backslash_in_string (strict <> None) store s (i+1))
      | Some c    -> store c; parse (i+1)
      | None      -> Buffer.contents buf
    in parse 0

end

let literal_overflow tok ty = ERROR (tok, Literal_overflow ty)
let illegal_escape tok s = ERROR (tok, Illegal_escape s)

(* To convert integer literals, allowing max_int + 1 (PR#4210) *)

let cvt_int_literal s =
  - int_of_string ("-" ^ s)
let cvt_int32_literal s =
  Int32.neg (Int32.of_string ("-" ^ String.sub s 0 (String.length s - 1)))
let cvt_int64_literal s =
  Int64.neg (Int64.of_string ("-" ^ String.sub s 0 (String.length s - 1)))
let cvt_nativeint_literal s =
  Nativeint.neg (Nativeint.of_string ("-" ^ String.sub s 0 (String.length s - 1)))

let mkCHAR s = try CHAR(Eval.char s, s)
               with Failure _ -> illegal_escape (sf "'%s'" s) s

let mkSTRING     s = try  STRING(Eval.string s, s)
                     with Failure _ -> illegal_escape (sf "\"%s\"" s) s
let mkINT        s = try  INT(cvt_int_literal s, s)
                     with Failure _ -> literal_overflow s "int"
let mkINT32      s = try  INT32(cvt_int32_literal s, s)
                     with Failure _ -> literal_overflow (s^"l") "int32"
let mkINT64      s = try  INT64(cvt_int64_literal s, s)
                     with Failure _ -> literal_overflow (s^"L")"int64"
let mkNATIVEINT  s = try  NATIVEINT(cvt_nativeint_literal s, s)
                     with Failure _ -> literal_overflow (s^"n") "nativeint"
let mkFLOAT      s = try  FLOAT(float_of_string s, s)
                     with Failure _ -> literal_overflow s "float"

let newline_of_string = function
  | "\n"   -> LF
  | "\r"   -> CR
  | "\r\n" -> CRLF
  | _      -> invalid_arg "newline_of_string"

(* not exported *)
let mkLINE_DIRECTIVE ?(bl1="") ?(bl2="") ?(zeros="0") ?s ?(com="") ?(nl="\n") i =
  assert (blanks bl1);
  assert (blanks bl2);
  assert (no_newline com);
  let zeros = int_of_string zeros in
  let nl = match nl with
    | "\n" -> LF
    | "\r" -> CR
    | "\r\n" -> CRLF
    | _ -> assert false
  in
  LINE_DIRECTIVE{l_blanks1=bl1;l_zeros=zeros;l_linenum=int_of_string i;
                 l_blanks2=bl2;l_filename=s;l_comment=com;l_newline=nl}

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
  | "ERROR", (tok :: x :: xs) ->
      (* Don't you see this code crying for the option monad? *)
      let mk err = Some (ERROR (tok, err)) in
      begin match x, xs with
      | "Illegal_character", []  -> assert (tok <> ""); mk (Illegal_character tok.[0])
      | "Illegal_escape",    [s] -> mk (Illegal_escape s)
      | "Literal_overflow",  [t] -> mk (Literal_overflow t)
      | "Unterminated",      [u] ->
          begin match u with
          | "Ucomment"           -> mk (Unterminated Ucomment)
          | "Ustring"            -> mk (Unterminated Ustring)
          | "Ustring_in_comment" -> mk (Unterminated Ustring_in_comment)
          | "Uquotation"         -> mk (Unterminated Uquotation)
          | "Uantiquot"          -> mk (Unterminated Uantiquot)
          | _                    -> None
          end
      | _ -> None
      end
  | "PSYMBOL", [x; y; z]     -> Some (PSYMBOL (x,y,z))
  | "LINE_DIRECTIVE", xs     ->
      begin match xs with
      (* One are a bit lax in the input *)
      | [i]                  -> Some (mkLINE_DIRECTIVE i)
      | [i; s]               -> Some (mkLINE_DIRECTIVE ~s i)

      | [bl1;zeros;i;bl2;com;nl]   -> Some (mkLINE_DIRECTIVE ~bl1 ~zeros ~bl2 ~com ~nl i)
      | [bl1;zeros;i;bl2;s;com;nl] -> Some (mkLINE_DIRECTIVE ~bl1 ~zeros ~bl2 ~s ~com ~nl i)
      | _                    -> None
      end
  | _                        -> None
