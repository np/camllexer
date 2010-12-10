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
    q_contents : string }

type blanks = string
type comment = string

type caml_token =
  | KEYWORD       of string
  | LIDENT        of string
  | UIDENT        of string
  | SYMBOL        of string
  | INT           of int * string
  | INT32         of int32 * string
  | INT64         of int64 * string
  | NATIVEINT     of nativeint * string
  | FLOAT         of float * string
  | CHAR          of char * string
  | STRING        of string * string
  | QUOTATION     of quotation
  | ANTIQUOT      of string * string
  | COMMENT       of comment
  | BLANKS        of blanks
  | NEWLINE       of newline
  | LINE_DIRECTIVE of line_directive
  | WARNING       of warning
  | ERROR         of string * error
  | EOI

and newline = LF | CR | CRLF

and warning =
  | Comment_start
  | Comment_not_end
  | Illegal_escape_in_string of string * int

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
  | Ucomment           -> "comment"
  | Ustring            -> "string literal"
  | Uquotation         -> "quotation"
  | Uantiquot          -> "antiquotation"

let string_of_unterminated_list us =
  String.concat " of " (List.map string_of_unterminated us)

let string_of_error =
  function
  | Illegal_character c ->
      sf "Illegal character (%s)" (Char.escaped c)
  | Illegal_escape_in_character s ->
      sf "Illegal backslash escape in character literal (%s)" s
  | Unterminated [(_,u)] ->
      sf "Unterminated %s" (string_of_unterminated u)
  | Unterminated ((_,u) :: us) ->
      let us = List.map snd us in
      sf "This %s contains an unterminated %s" (string_of_unterminated_list us) (string_of_unterminated u)
  | Unterminated [] ->
      assert false
  | Literal_overflow ty ->
      sf "Integer literal exceeds the range of representable integers of type %s" ty

let show_unterminated =
  function
  | Ucomment           -> ["Ucomment"]
  | Ustring            -> ["Ustring"]
  | Uquotation         -> ["Uquotation"]
  | Uantiquot          -> ["Uantiquot"]

let show_position { Lexing.pos_fname=fp; pos_lnum=ln; pos_bol=bol; pos_cnum=cn } =
  [fp; string_of_int ln; string_of_int bol; string_of_int cn]

let show2 s1 s2 (x, y) = s1 x @ s2 y

(* meant to be used by show_token/strings_of_token *)
let show_error =
  function
  | Illegal_character _ ->
      ("Illegal_character", [])
  | Illegal_escape_in_character s ->
      ("Illegal_escape_in_character", [s])
  | Unterminated us ->
      ("Unterminated", List.concat (List.map (show2 show_position show_unterminated) us))
  | Literal_overflow ty ->
      ("Literal_overflow", [ty])

let string_of_quotation {q_name=n; q_loc=l; q_contents=s} =
  let locname = if l = "" then "" else sf "@%s" l in
  if n = "" then sf "<%s<%s>>" locname s
  else sf "<:%s%s<%s>>" n locname s

(* spec quotation_width = String.length <.> string_of_quotation *)
let quotation_width {q_name=n; q_loc=l; q_contents=s} =
  let locname_width = if l = "" then 0 else String.length l + 1 in
  if n = "" then String.length s + locname_width + 4
  else String.length s + String.length n + locname_width + 5

let for_all p s =
  let len = String.length s in
  let rec loop i = i >= len || p s.[i] && loop (i+1)
  in loop 0

let (<$>) f = function
  | Some x -> Some (f x)
  | None -> None

let blank c = List.mem c [' '; '\t'; '\012']
let newline c = List.mem c ['\r'; '\n']

let blanks = for_all blank

let no_newline = for_all (fun c -> not (newline c))

let string_of_newline = function
  | LF   -> "\n"
  | CR   -> "\r"
  | CRLF -> "\r\n"

(* spec = String.length <.> string_of_newline *)
let newline_width = function
  | LF | CR -> 1
  | CRLF    -> 2

let strings_of_warning = function
  | Comment_start -> ["Comment_start"]
  | Comment_not_end -> ["Comment_not_end"]
  | Illegal_escape_in_string(s, i) ->
      ["Illegal_escape_in_string"; s; string_of_int i]

let message_of_warning = function
  | Comment_start   -> "this is the start of a comment"
  | Comment_not_end -> "this is not the end of a comment"
  | Illegal_escape_in_string(s,_) -> sf "Illegal backslash escape in string (\\%s)" s

(* spec String.length <.> string_of_int *)
let int_width =
  let rec loop acc x =
    let q = x / 10 in
    if q > 0 then loop (1 + acc) q else acc
  in loop 0

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

let line_directive_width x =
  let nl = newline_width x.l_newline in
  let fn_w =
    match x.l_filename with
    | Some s -> String.length s + 2
    | None -> 0
  in
  String.length x.l_blanks1 + x.l_zeros + int_width x.l_linenum + String.length x.l_blanks2 + fn_w + String.length x.l_comment + nl + 1

let string_of_token = function
  | KEYWORD       s      |
    SYMBOL        s      |
    LIDENT        s      |
    UIDENT        s      |
    COMMENT       s      |
    INT           (_, s) |
    FLOAT         (_, s) |
    ERROR         (s, _) |
    BLANKS        s -> s

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
  | WARNING _       -> ""
  | LINE_DIRECTIVE ld -> string_of_line_directive ld

(*
The spec is token_width = String.length <.> string_of_token, that is
the kind of code where the compiler could deduce the right code from
the spec.
*)
let token_width = function
  | KEYWORD       s      |
    SYMBOL        s      |
    LIDENT        s      |
    UIDENT        s      |
    COMMENT       s      |
    INT           (_, s) |
    FLOAT         (_, s) |
    ERROR         (s, _) |
    BLANKS        s      -> String.length s
  | CHAR          (_, s) |
    STRING        (_, s) |
    ANTIQUOT     ("", s) -> String.length s + 2
  | INT32         (_, s) |
    INT64         (_, s) |
    NATIVEINT     (_, s) -> String.length s + 1
  | ANTIQUOT      (n, c) -> String.length n + String.length c + 3
  | QUOTATION     q -> quotation_width q
  | NEWLINE nl      -> newline_width nl
  | EOI             -> 0
  | WARNING _       -> 0
  | LINE_DIRECTIVE ld -> line_directive_width ld

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
  | ANTIQUOT(n, s)   -> ("ANTIQUOT", [n; s])
  | QUOTATION x      -> ("QUOTATION", [x.q_name; x.q_loc; x.q_contents])
  | COMMENT s        -> ("COMMENT", [s])
  | BLANKS s         -> ("BLANKS", [s])
  | NEWLINE nl       -> ("NEWLINE", [string_of_newline nl])
  | EOI              -> ("EOI", [])
  | WARNING w        -> ("WARNING", strings_of_warning w)
  | ERROR (tok, err) -> ("ERROR", tok :: let (x,xs) = show_error err in x :: xs)
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

  exception Backtrack of int

  let peek s i = if i < String.length s then Some s.[i] else None
  let get s i =
    let len = String.length s in
    if i < len then s.[i] else raise (Backtrack (len - 1))

  let rec skip_indent s i =
    match peek s i with
    | Some (' ' | '\t') -> skip_indent s (i+1)
    | _ -> i

  let skip_opt_linefeed s i =
    match peek s i with
    | Some '\n' -> i + 1
    | _ -> i

  let get_dec_digit s i =
    match get s i with
    | '0'..'9' as c -> Char.code c - 48
    | _ -> raise (Backtrack i)

  let get_hex_digit s i =
    match get s i with
    | '0'..'9' | 'a'..'f' | 'A'..'F' as c ->
        let d = Char.code c in
        if d >= 97 then d - 87
        else if d >= 65 then d - 55
        else d - 48
    | _ -> raise (Backtrack i)

  let escaped_char s i = match get s i with
    | '\n' -> '\n', i + 1
    | '\r' -> '\r', i + 1
    | 'n'  -> '\n', i + 1
    | 'r'  -> '\r', i + 1
    | 't'  -> '\t', i + 1
    | 'b'  -> '\b', i + 1
    | '\\' -> '\\', i + 1
    | '"'  -> '"',  i + 1
    | '\'' -> '\'', i + 1
    | ' '  -> ' ',  i + 1
    | '0'..'9' as c ->
        let d1 = Char.code c - 48 in
        let d2 = get_dec_digit s (i + 1) in
        let d3 = get_dec_digit s (i + 2) in
        let c = 100 * d1 + 10 * d2 + d3 in
        if c < 0 || c > 255 then raise (Backtrack (i+2)) else (Char.chr c, i + 3)
    | 'x' ->
        let d1 = get_hex_digit s (i + 1) in
        let d2 = get_hex_digit s (i + 2) in
        Char.chr (16 * d1 + d2), i + 3
    | _ -> raise (Backtrack (i + 1))

  let eof s i = if peek s i <> None then raise (Backtrack (i + 1))

  let char s =
    match String.length s with
    | 1 -> s.[0]
    | 0 -> failwith "invalid char token"
    | _ when s = "\r\n" -> '\n'
    | _ when s.[0] <> '\\' -> failwith "invalid char token"
    | _ -> try let (c, i) = escaped_char s 1 in
               eof s i; c
           with Backtrack _ -> failwith "invalid char token"

  let backslash_in_string err store s i =
    match peek s i with
    | Some '\n' -> skip_indent s (i+1)
    | Some '\r' -> skip_indent s (skip_opt_linefeed s (i+1))
    | Some c ->
        begin try let (x, i) = escaped_char s i in store x; i
        with Backtrack j -> (store '\\'; store c; err j; i + 1)
        end
    | None -> err i; store '\\'; i

  let string s =
    let buf = Buffer.create 23 in
    let store = Buffer.add_char buf in
    let errors = ref [] in
    let err i j = errors := (i, j - i + 1) :: !errors in
    let rec parse i =
      match peek s i with
      | Some '\\' -> parse (backslash_in_string (err (i+1)) store s (i+1))
      | Some c    -> store c; parse (i+1)
      | None      -> Buffer.contents buf
    in
    let s = parse 0 in
    s, List.rev !errors

end

let eval_char = Eval.char
let eval_string = Eval.string

(* To convert integer literals, allowing max_int + 1 (PR#4210) *)

let cvt_int_literal s =
  - int_of_string ("-" ^ s)
let cvt_int32_literal s =
  Int32.neg (Int32.of_string ("-" ^ String.sub s 0 (String.length s - 1)))
let cvt_int64_literal s =
  Int64.neg (Int64.of_string ("-" ^ String.sub s 0 (String.length s - 1)))
let cvt_nativeint_literal s =
  Nativeint.neg (Nativeint.of_string ("-" ^ String.sub s 0 (String.length s - 1)))

let mkWARNING w = WARNING w
let mkERROR s e = ERROR (s, e)

let literal_overflow tok ty = mkERROR tok (Literal_overflow ty)
let illegal_escape_in_character tok s = mkERROR tok (Illegal_escape_in_character s)
let illegal_escape_in_string s (ofs, len) =
  let sub = try String.sub s ofs len with Invalid_argument _ -> assert false in
  mkWARNING (Illegal_escape_in_string(sub, ofs))

let mkCHAR s = try CHAR(eval_char s, s)
               with Failure _ -> illegal_escape_in_character (sf "'%s'" s) s

let mkSTRING     s = let s', errs = eval_string s in
                     STRING(s', s), List.map (illegal_escape_in_string s) errs
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

let mkKEYWORD s = KEYWORD s
(* TODO add some assertions? *)
let mkLIDENT s = LIDENT s
let mkUIDENT s = UIDENT s
let mkSYMBOL s = SYMBOL s
let mkQUOTATION q = QUOTATION q
let mkANTIQUOT ?(name="") s = ANTIQUOT (name, s)
let mkCOMMENT com = COMMENT com
let mkBLANKS s = BLANKS s
let mkNEWLINE nl = NEWLINE nl
let mkLINE_DIRECTIVE ld = LINE_DIRECTIVE ld
let eoi = EOI

let newline_of_string = function
  | "\n"   -> LF
  | "\r"   -> CR
  | "\r\n" -> CRLF
  | _      -> invalid_arg "newline_of_string"

(* not exported *)
let mkLINE_DIRECTIVE' ?(bl1="") ?(bl2="") ?(zeros="0") ?s ?(com="") ?(nl="\n") i =
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

module ParseLib = struct
  exception ParseFailure

  let pure x input = (x, input)
  let fail _input = raise ParseFailure

  let parse_elt = function
    | x :: xs -> (x, xs)
    | input -> fail input

  let pmap f p input =
    let (x, input) = p input in
    (f x, input)

  let pmapf f p input =
    let (x, input) = p input in
    match f x with
    | Some r -> (r, input)
    | None -> fail input

  let (<*>) pf px input =
    let (f, input) = pf input in
    let (x, input) = px input in
    (f x, input)

  let parse_int =
    let safe_int_of_string x =
      try Some (int_of_string x)
      with Failure _ -> None
    in
    pmapf safe_int_of_string parse_elt

  let try_parse p input =
    match try Some (p input) with ParseFailure -> None with
    | Some (x, input) -> (Some x, input)
    | None -> (None, input)

  let rec parse_list p input =
    let (xopt, input) = try_parse p input in
    match xopt with
    | Some x -> pmap (fun xs -> x :: xs) (parse_list p) input
    | None -> ([], input)

  let parse_full p input =
    let (x, input) = try_parse p input in
    if input = [] then x else None
end
open ParseLib

let parse_unterminated =
  let f = function
    | "Ucomment"           -> Some Ucomment
    | "Ustring"            -> Some Ustring
    | "Uquotation"         -> Some Uquotation
    | "Uantiquot"          -> Some Uantiquot
    | _                    -> None
  in
  pmapf f parse_elt

let parse_position =
  let mk_position fp ln bol cn =
    { Lexing.pos_fname=fp; pos_lnum=ln; pos_bol=bol; pos_cnum=cn }
  in
  pmap mk_position parse_elt <*> parse_int <*> parse_int <*> parse_int

let parse_position_unterminated =
  pmap (fun x y -> x,y) parse_position <*> parse_unterminated

let parse_position_unterminated_list =
  parse_list parse_position_unterminated

let warning_of_strings = function
  | ["Comment_start"] -> Some Comment_start
  | ["Comment_not_end"] -> Some Comment_not_end
  | ["Illegal_escape_in_string"; s; si] ->
      Some (Illegal_escape_in_string(s, int_of_string si))
  | _ -> None

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
  | "STRING", [s]            -> Some (fst (mkSTRING s))
  | "ANTIQUOT", [n; s]       -> Some (ANTIQUOT(n, s))
  | "QUOTATION", [n;l;c]     -> Some (QUOTATION{q_name=n;q_loc=l;q_contents=c})
  | "COMMENT", [s]           -> Some (COMMENT s)
  | "BLANKS", [s]            -> Some (BLANKS s)
  | "NEWLINE", []            -> Some (NEWLINE LF) (* lax in the input *)
  | "NEWLINE", ["\n"]        -> Some (NEWLINE LF)
  | "NEWLINE", ["\r"]        -> Some (NEWLINE CR)
  | "NEWLINE", ["\r\n"]      -> Some (NEWLINE CRLF)
  | "EOI", []                -> Some EOI
  | "WARNING", xs            -> mkWARNING <$> warning_of_strings xs
  | "ERROR", (tok :: x :: xs) ->
      (* Don't you see this code crying for the option monad? *)
      let mk err = Some (ERROR (tok, err)) in
      begin match x, xs with
      | "Illegal_character", []  -> assert (tok <> ""); mk (Illegal_character tok.[0])
      | "Illegal_escape_in_character", [s] -> mk (Illegal_escape_in_character s)
      | "Literal_overflow",  [t] -> mk (Literal_overflow t)
      | "Unterminated",      us  ->
          begin match parse_full parse_position_unterminated_list us with
          | Some x -> mk (Unterminated x)
          | None -> None
          end
      | _ -> None
      end
  | "LINE_DIRECTIVE", xs     ->
      begin match xs with
      (* One are a bit lax in the input *)
      | [i]                  -> Some (mkLINE_DIRECTIVE' i)
      | [i; s]               -> Some (mkLINE_DIRECTIVE' ~s i)

      | [bl1;zeros;i;bl2;com;nl]   -> Some (mkLINE_DIRECTIVE' ~bl1 ~zeros ~bl2 ~com ~nl i)
      | [bl1;zeros;i;bl2;s;com;nl] -> Some (mkLINE_DIRECTIVE' ~bl1 ~zeros ~bl2 ~s ~com ~nl i)
      | _                    -> None
      end
  | _                        -> None
