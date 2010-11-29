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
 * - Xavier Leroy: initial version for OCaml
 * - Daniel de Rauglaudre: some parts from Camlp4
 * - Nicolas Pouillard: this actual implementation
 *)

{

(** A lexical analyzer. *)

open Camltoken

(* See loc.mli for actual documentation *)
module type LOC = sig
  type t
  val ghost : t
  val of_lexbuf : Lexing.lexbuf -> t
  val merge : t -> t -> t
  val move_both : int -> t -> t
  val start_pos  : t -> Lexing.position
  val to_string : t -> string
  exception Exc_located of t * exn
  val raise : t -> exn -> 'a
end

let sf = Printf.sprintf

module Make (Loc : LOC)
= struct

  open Lexing

  (* To store some context information:
  *   loc       : position of the beginning of a string, quotation and comment
  *   stack     : what is our call stack?
  *   quotations: shall we lex quotation?
  *               If quotations is false it's a SYMBOL token.
  *   antiquots : shall we lex antiquotations.
  *)

  type context =
  { loc        : Loc.t
  ; stack      : unterminated list
  ; quotations : bool
  ; antiquots  : bool
  ; warnings   : bool
  ; lexbuf     : lexbuf
  ; buffer     : Buffer.t
  }

  let default_context lb =
  { loc        = Loc.ghost ;
    stack      = []        ;
    quotations = true      ;
    antiquots  = false     ;
    warnings   = true      ;
    lexbuf     = lb        ;
    buffer     = Buffer.create 256 }

  (* To buffer string literals, quotations and antiquotations *)

  let store c = Buffer.add_string c.buffer (Lexing.lexeme c.lexbuf)
  let istore_char c i = Buffer.add_char c.buffer (Lexing.lexeme_char c.lexbuf i)
  let buff_contents c =
    let contents = Buffer.contents c.buffer in
    Buffer.reset c.buffer; contents

  let loc c = Loc.merge c.loc (Loc.of_lexbuf c.lexbuf)
  let quotations c = c.quotations
  let antiquots c = c.antiquots
  let set_start_p c = c.lexbuf.lex_start_p <- Loc.start_pos c.loc
  let move_start_p shift c =
    let p = c.lexbuf.lex_start_p in
    c.lexbuf.lex_start_p <- { (p) with pos_cnum = p.pos_cnum + shift }

  let unterminated s u = mkERROR s (Unterminated u)

  let update_cxt_loc c = { (c) with loc = Loc.of_lexbuf c.lexbuf }
  let with_curr_loc f c = f (update_cxt_loc c) c.lexbuf
  let with_curr_loc_in frame f c = f { (c) with loc = Loc.of_lexbuf c.lexbuf
                                     ;          stack = frame :: c.stack } c.lexbuf
  let parse_comment comment c =
    let r = with_curr_loc_in Ucomment comment c in
    set_start_p c;
    let contents = buff_contents c in
    match r with
    | [] -> mkCOMMENT contents
    | us -> unterminated contents us
  let shift n c = { (c) with loc = Loc.move_both n c.loc }
  let store_parse f c = store c ; f c c.lexbuf
  let parse f c = f c c.lexbuf
  let parse' f c () = f c c.lexbuf
  let mk_quotation quotation c name loc shift =
    let mk contents =
      { q_name     = name     ;
        q_loc      = loc      ;
        q_shift    = shift    ;
        q_contents = contents }
    in
    let r = with_curr_loc_in Uquotation quotation c in
    set_start_p c;
    let contents = buff_contents c in
    match r with
    | [] -> let s = contents in
            mkQUOTATION (mk (String.sub s 0 (String.length s - 2)))
    | us -> unterminated (string_of_quotation (mk contents)) us

  let (&) x f = match x with
   | [] -> f ()
   | us -> us

  (* Update the current location with file name and line number. *)

  let update_location c file line absolute chars =
    let lexbuf = c.lexbuf in
    let pos = lexbuf.lex_curr_p in
    let new_file = match file with
                  | None -> pos.pos_fname
                  | Some s -> s
    in
    lexbuf.lex_curr_p <- { pos with
      pos_fname = new_file;
      pos_lnum = if absolute then line else pos.pos_lnum + line;
      pos_bol = pos.pos_cnum - chars;
    }

  let update_chars c chars = update_location c None 1 false chars

  (* Given the length of the input and a the get function of
     the input. count_newlines returns the number of newlines
     and the offset of the last one. *)
  let count_newlines len s =
    let rec go count nl_off i =
      if i >= len then (count, nl_off)
      else
        match s(i) with
        | '\n' -> go (count + 1) i (i + 1)
        | '\r' ->
          if i + 1 < len && s(i + 1) = '\n' then
            go (count + 1) (i + 1) (i + 2)
          else
            go (count + 1) i (i + 1)
        | _ -> go count nl_off (i + 1)
    in go 0 0 0

  let update_loc c =
    let lb = c.lexbuf in
    let len = lb.lex_curr_pos - lb.lex_start_pos in
    let newlines, last_newline_offset = count_newlines len (Lexing.lexeme_char lb) in
    let chars = len - 1 - last_newline_offset in
    update_location c None newlines false chars

  let illegal_character c = mkERROR (String.make 1 c) (Illegal_character c)

  let warn msg loc =
    Printf.eprintf "Warning: %s: %s\n%!" (Loc.to_string loc) msg

  let warn_comment_not_end c lexbuf op =
    if c.warnings && op <> "" && op.[String.length op - 1] = '*' then
      warn "this is not the end of a comment" (Loc.of_lexbuf lexbuf)

  let warn_comment_start c lexbuf =
    if c.warnings then
      warn "this is the start of a comment" (Loc.of_lexbuf lexbuf)


  }

  let newline = ('\n' | '\r' | "\r\n")
  let blank = [' ' '\t' '\012']
  let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
  let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']
  let identchar =
    ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']
  let ident = (lowercase|uppercase) identchar*
  let locname = ident
  let not_star_symbolchar =
    ['$' '!' '%' '&' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~' '\\']
  let symbolchar = '*' | not_star_symbolchar
  let quotchar =
    ['!' '%' '&' '+' '-' '.' '/' ':' '=' '?' '@' '^' '|' '~' '\\' '*']
  let hexa_char = ['0'-'9' 'A'-'F' 'a'-'f']
  let decimal_literal =
    ['0'-'9'] ['0'-'9' '_']*
  let hex_literal =
    '0' ['x' 'X'] hexa_char ['0'-'9' 'A'-'F' 'a'-'f' '_']*
  let oct_literal =
    '0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']*
  let bin_literal =
    '0' ['b' 'B'] ['0'-'1'] ['0'-'1' '_']*
  let int_literal =
    decimal_literal | hex_literal | oct_literal | bin_literal
  let float_literal =
    ['0'-'9'] ['0'-'9' '_']*
    ('.' ['0'-'9' '_']* )?
    (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*)?
  let char_literal_no_nl_quote =
    ( [^ '\\' '\n' '\r' '"']
    | '\\' ( ['\\' '"' 'n' 't' 'b' 'r' ' ' '\'']
           | ['0'-'9'] ['0'-'9'] ['0'-'9']
           | 'x' hexa_char hexa_char ))
  let char_litteral = char_literal_no_nl_quote | '"' | newline | ('\\' _)
  let char = "'" char_litteral "'"
  let string_char = char_literal_no_nl_quote | newline | ('\\' _)
  let string = '"' string_char* '"'
  let unterminated_string = '"' string_char* '\\'? eof

  (* Delimitors are extended (from 3.09) in a conservative way *)

  (* These chars that can't start an expression or a pattern: *)
  let safe_delimchars = ['%' '&' '/' '@' '^']

  (* These symbols are unsafe since "[<", "[|", etc. exsist. *)
  let delimchars = safe_delimchars | ['|' '<' '>' ':' '=' '.']

  let left_delims  = ['(' '[' '{']
  let right_delims = [')' ']' '}']

  let left_delimitor =
    (* At least a safe_delimchars *)
    left_delims delimchars* safe_delimchars (delimchars|left_delims)*

  (* A '(' or a new super '(' without "(<" *)
  | '(' (['|' ':'] delimchars*)?
  (* Old brackets, no new brackets starting with "[|" or "[:" *)
  | '[' ['|' ':']?
  (* Old "[<","{<" and new ones *)
  | ['[' '{'] delimchars* '<'
  (* Old brace and new ones *)
  | '{' (['|' ':'] delimchars*)?

  let right_delimitor =
    (* At least a safe_delimchars *)
    (delimchars|right_delims)* safe_delimchars (delimchars|right_delims)* right_delims
  (* A ')' or a new super ')' without ">)" *)
  | (delimchars* ['|' ':'])? ')'
  (* Old brackets, no new brackets ending with "|]" or ":]" *)
  | ['|' ':']? ']'
  (* Old ">]",">}" and new ones *)
  | '>' delimchars* [']' '}']
  (* Old brace and new ones *)
  | (delimchars* ['|' ':'])? '}'


  rule token c = parse
    | '\n'                                     { update_chars c 0; mkNEWLINE LF }
    | '\r'                                     { update_chars c 0; mkNEWLINE CR }
    | "\r\n"                                 { update_chars c 0; mkNEWLINE CRLF }
    | blank + as x                                                 { mkBLANKS x }
    | "~" (lowercase identchar * as x) ':'                          { mkLABEL x }
    | "?" (lowercase identchar * as x) ':'                       { mkOPTLABEL x }
    | lowercase identchar * as x                                   { mkLIDENT x }
    | uppercase identchar * as x                                   { mkUIDENT x }
    | int_literal as i                                                { mkINT i }
    | float_literal as f                                            { mkFLOAT f }
    | (int_literal as i) "l"                                        { mkINT32 i }
    | (int_literal as i) "L"                                        { mkINT64 i }
    | (int_literal as i) "n"                                    { mkNATIVEINT i }
    | '"' (string_char* as s) '"'                    { update_loc c; mkSTRING s }
    | unterminated_string as s                       {             update_loc c ;
                                                       unterminated s [Ustring] }
    | "'" (char_litteral as s) "'"                     { update_loc c; mkCHAR s }
    | "(*"                                   { store c; parse_comment comment c }
    | "(*)"                                  { warn_comment_start c lexbuf      ;
                                               store c; parse_comment comment c }
    | "<<" (quotchar* as beginning)
      { if quotations c
        then (move_start_p (-String.length beginning) c;
              mk_quotation quotation c "" "" 2)
        else parse (symbolchar_star ("<<" ^ beginning)) c                       }
    | "<<>>"
      { if quotations c
        then mkQUOTATION { q_name = ""; q_loc = ""; q_shift = 2; q_contents = "" }
        else parse (symbolchar_star "<<>>") c                                   }
    | "<@"
      { if quotations c then with_curr_loc maybe_quotation_at c
        else parse (symbolchar_star "<@") c                                     }
    | "<:"
      { if quotations c then with_curr_loc maybe_quotation_colon c
        else parse (symbolchar_star "<:") c                                     }
    | "#" ([' ' '\t']* as bl1) ('0'* as zeros) ('0' | ['1'-'9']['0'-'9']* as num)
          ([' ' '\t']* as bl2) ("\"" ([^ '\n' '\r' '"' ] * as name) "\"")?
          ([^ '\n' '\r']* as com) (newline as nl)
                                { let inum = int_of_string num in
                                  let nl = newline_of_string nl in
                                  update_location c name inum true 0;
                                  mkLINE_DIRECTIVE{l_blanks1=bl1;
                                                   l_zeros=String.length zeros;
                                                   l_linenum=inum;
                                                   l_blanks2=bl2;
                                                   l_filename=name;
                                                   l_comment=com;
                                                   l_newline=nl} }
    | '(' (not_star_symbolchar as op) ')'
                                                 { mkPSYMBOL (String.make 1 op) }
    | '(' (not_star_symbolchar symbolchar* as op) ')'
                                             { warn_comment_not_end c lexbuf op ;
                                                                   mkPSYMBOL op }
    | '(' (not_star_symbolchar symbolchar* as op) (blank+ as post_blanks) ')'
                                                    { mkPSYMBOL ~post_blanks op }
    | '(' (blank+ as pre_blanks) (symbolchar+ as op) ')'
                                             { warn_comment_not_end c lexbuf op ;
                                                       mkPSYMBOL ~pre_blanks op }
    | '(' (blank+ as pre_blanks) (symbolchar+ as op) (blank+ as post_blanks) ')'
                                        { mkPSYMBOL ~pre_blanks ~post_blanks op }
    | ( "#"  | "`"  | "'"  | ","  | "."  | ".." | ":"  | "::"
      | ":=" | ":>" | ";"  | ";;" | "_"
      | left_delimitor | right_delimitor ) as x                    { mkSYMBOL x }
    | '$' { if antiquots c
            then with_curr_loc dollar (shift 1 c)
            else parse (symbolchar_star "$") c }
    | ['~' '?' '!' '=' '<' '>' '|' '&' '@' '^' '+' '-' '*' '/' '%' '\\'] symbolchar *
                                                              as x { mkSYMBOL x }
    | eof
      { let pos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <- { pos with pos_bol  = pos.pos_bol  + 1 ;
                                        pos_cnum = pos.pos_cnum + 1 }; eoi      }
    | _ as c                                              { illegal_character c }

  and comment c = parse
      "(*"                               {                              store c ;
                                            with_curr_loc_in Ucomment comment c &
                                                               parse' comment c }
    | "*)"                                                        { store c; [] }
    | '<' (':' ident)? ('@' locname)? '<'
                { store c;
                  (if quotations c then
                    with_curr_loc_in Uquotation quotation c else []) &
                  parse' comment c }
    | ident                                             { store_parse comment c }
    | string                              { update_loc c; store_parse comment c }
    | unterminated_string           { update_loc c; store c; Ustring :: c.stack }
    | "''"                                              { store_parse comment c }
    | char                                { update_loc c; store_parse comment c }
    | eof                                                             { c.stack }
    | newline                         { update_chars c 0; store_parse comment c }
    | _                                                 { store_parse comment c }

  and symbolchar_star beginning c = parse
    | symbolchar* as tok            { move_start_p (-String.length beginning) c ;
                                                     mkSYMBOL (beginning ^ tok) }

  and maybe_quotation_at c = parse
    | (ident as loc) '<'
      { mk_quotation quotation c "" loc (1 + String.length loc)                 }
    | symbolchar* as tok                                 { mkSYMBOL("<@" ^ tok) }

  and maybe_quotation_colon c = parse
    | (ident as name) '<'
      { mk_quotation quotation c name "" (1 + String.length name)               }
    | (ident as name) '@' (locname as loc) '<'
      { mk_quotation quotation c name loc
                     (2 + String.length loc + String.length name)               }
    | symbolchar* as tok                                 { mkSYMBOL("<:" ^ tok) }

  and quotation c = parse
    | '<' (':' ident)? ('@' locname)? '<'           {                   store c ;
                                        with_curr_loc_in Uquotation quotation c &
                                                             parse' quotation c }
    | ">>"                                                        { store c; [] }
    | eof                                                             { c.stack }
    | newline                       { update_chars c 0; store_parse quotation c }
    | _                                               { store_parse quotation c }

  and dollar c = parse
    | '$'                                        { set_start_p c; mkANTIQUOT "" }
    | ('`'? (identchar*|'.'+) as name) ':'
      { with_curr_loc (antiquot name) (shift (1 + String.length name) c)        }
    | _                                           { store_parse (antiquot "") c }

  and antiquot name c = parse
    | '$'                   { set_start_p c; mkANTIQUOT ~name (buff_contents c) }
    | eof       { unterminated (sf "$%s:%s" name (buff_contents c)) [Uantiquot] }
    | newline                 { update_chars c 0; store_parse (antiquot name) c }
    | '<' (':' ident)? ('@' locname)? '<'
      { store c; match with_curr_loc_in Uquotation quotation c with
                 | [] -> parse (antiquot name) c
                 | stack -> unterminated (buff_contents c) stack  }
    | _                                         { store_parse (antiquot name) c }

  {

  let lexing_store s buff max =
    let rec self n s =
      if n >= max then n
      else
        match Stream.peek s with
        | Some x ->
            Stream.junk s;
            buff.[n] <- x;
            succ n
        | _ -> n
    in
    self 0 s

  let from_context c =
    let next _ =
      let tok = with_curr_loc token c in
      let loc = Loc.of_lexbuf c.lexbuf in
      Some ((tok, loc))
    in Stream.from next

  let from_lexbuf ~quotations ~antiquotations ~warnings lb =
    let c = { (default_context lb) with
              loc        = Loc.of_lexbuf lb;
              antiquots  = antiquotations;
              quotations = quotations;
              warnings   = warnings
            }
    in from_context c

  let setup_loc lb loc =
    let start_pos = Loc.start_pos loc in
    lb.lex_abs_pos <- start_pos.pos_cnum;
    lb.lex_curr_p  <- start_pos

  let from_string ~quotations ~antiquotations ~warnings loc str =
    let lb = Lexing.from_string str in
    setup_loc lb loc;
    from_lexbuf ~quotations ~antiquotations ~warnings lb

  let from_stream ~quotations ~antiquotations ~warnings loc strm =
    let lb = Lexing.from_function (lexing_store strm) in
    setup_loc lb loc;
    from_lexbuf ~quotations ~antiquotations ~warnings lb
end
}
