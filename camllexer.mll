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

type 'a iterator = unit -> 'a option

type 'a iterator_list = unit -> 'a list

let flatten_iterator_list next0 =
  let queue = ref [] in
  let rec next () =
    match !queue with
    | x :: xs -> queue := xs; Some x
    | [] ->
        match next0 () with
        | []      -> None
        | [x]     -> Some x
        | x :: xs ->
            queue := xs;
            Some x
  in next

(* See loc.mli for actual documentation *)
module type LOC = sig
  type t
  val ghost : t
  val of_lexbuf : Lexing.lexbuf -> t
  val of_positions : Lexing.position -> Lexing.position -> t
  val move_both : int -> t -> t
  val start_pos  : t -> Lexing.position
  val stop_pos  : t -> Lexing.position
  val to_string : t -> string
  exception Exc_located of t * exn
  val raise : t -> exn -> 'a
end

let (<.>) f g x = f (g x)

let sf = Printf.sprintf

type flags = { quotations      : bool  (** Enables the lexing of quotations *)
             ; antiquotations  : bool  (** Enables the lexing of anti-quotations *)
             ; line_directives : bool  (** Honor the # line directives *)
             }

let default_flags = { quotations = false
                    ; antiquotations = false
                    ; line_directives = true
                    }

module Make (Loc : LOC)
= struct

  open Lexing

  type token = (caml_token * Loc.t)

  type context =
  { stack      : (position * unterminated) list (** Stack of opened constructs *)
  ; flags      : flags (** Lexing flavors *)
  ; lexbuf     : lexbuf
  ; buffer     : Buffer.t
  }

  let default_context lb =
  { stack      = []
  ; flags      = default_flags
  ; lexbuf     = lb
  ; buffer     = Buffer.create 256
  }

  (* To buffer comments, quotations and antiquotations *)

  let store c = Buffer.add_string c.buffer (Lexing.lexeme c.lexbuf)
  let buff_contents c =
    let contents = Buffer.contents c.buffer in
    Buffer.reset c.buffer; contents

  (* Some projections *)

  let quotations c = c.flags.quotations
  let antiquots c = c.flags.antiquotations
  let line_directives c = c.flags.line_directives

  (* Various location/postion related functions *)

  let (>>>) p k = { p with pos_cnum = p.pos_cnum + k }
  let (>+>) p w = Loc.of_positions p (p >>> w)
  let (<-<) p w = Loc.of_positions (p >>> (-w)) p
  let set_sp c sp = c.lexbuf.lex_start_p <- sp
  let get_sp c = c.lexbuf.lex_start_p
  let move_sp shift c =
    c.lexbuf.lex_start_p <- c.lexbuf.lex_start_p >>> shift

  (* Update the current location with file name and line number. *)

  let update_absolute_position c file line =
    let lexbuf = c.lexbuf in
    let pos = lexbuf.lex_curr_p in
    let new_file = match file with
                  | None -> pos.pos_fname
                  | Some s -> s
    in
    lexbuf.lex_curr_p <- { pos with
      pos_fname = new_file;
      pos_lnum = line;
      pos_bol = pos.pos_cnum;
    }

  let update_relative_position c line chars =
    let lexbuf = c.lexbuf in
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
      pos_lnum = pos.pos_lnum + line;
      pos_bol = pos.pos_cnum - chars;
    }

  let update_chars c chars = update_relative_position c 1 chars

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
    if newlines <> 0 then update_relative_position c newlines chars

  let parse_with_sp f c =
    let sp = get_sp c in
    let r = f c c.lexbuf in
    set_sp c sp; r
  let parse_in frame f c = f { c with stack = (c.lexbuf.lex_start_p, frame) :: c.stack } c.lexbuf
  let store_parse f c = store c ; f c c.lexbuf
  let parse f c = f c c.lexbuf
  let parse' f c () = f c c.lexbuf

  let (&) x f = match x with
   | [] -> f ()
   | us -> us

  let unterminated s u = mkERROR s (Unterminated u)
  let unterminated1 s u c = unterminated s [(get_sp c, u)]

  let illegal_character c = mkERROR (String.make 1 c) (Illegal_character c)

  let mkANTIQUOT c sp ?name s = set_sp c sp; mkANTIQUOT ?name s

  let mkBLANKS_ s tail =
    match s with
    | "" -> tail
    | s  -> mkBLANKS s :: tail

  let mkPSYMBOL ?(pre_blanks="") ?(post_blanks="") op =
    assert (op <> "");
    let may_warn =
      if post_blanks = "" && op.[String.length op - 1] = '*'
      then [mkWARNING Comment_not_end]
      else []
    in
    mkSYMBOL "(" ::  mkBLANKS_ pre_blanks
                 (   mkSYMBOL op
                 ::  mkBLANKS_ post_blanks
                 (   mkSYMBOL ")"
                 ::  may_warn))

  let parse_comment comment c =
    let sp = c.lexbuf.lex_start_p in
    let r = parse_in Ucomment comment c in
    let contents = buff_contents c in
    c.lexbuf.lex_start_p <- sp;
    match r with
    | [] -> mkCOMMENT contents
    | us -> unterminated contents us

  let parse_quotation quotation c name loc =
    let mk contents =
      { q_name     = name     ;
        q_loc      = loc      ;
        q_contents = contents }
    in
    let sp = c.lexbuf.lex_start_p in
    let r = parse_in Uquotation quotation c in
    let contents = buff_contents c in
    c.lexbuf.lex_start_p <- sp;
    match r with
    | [] -> let s = contents in
            mkQUOTATION (mk (String.sub s 0 (String.length s - 2)))
    | us -> unterminated (string_of_quotation (mk contents)) us

  }

  let newline = ('\n' | '\r' | "\r\n")
  let blank = [' ' '\t' '\012']
  let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
  let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']
  let identchar =
    ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']
  let ident = (lowercase|uppercase) identchar*
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
    | '\n'                                   { update_chars c 0; [mkNEWLINE LF] }
    | '\r'                                   { update_chars c 0; [mkNEWLINE CR] }
    | "\r\n"                               { update_chars c 0; [mkNEWLINE CRLF] }
    | blank + as x                                               { [mkBLANKS x] }
    | "~" (lowercase identchar * as x) ':'                        { [mkLABEL x] }
    | "?" (lowercase identchar * as x) ':'                     { [mkOPTLABEL x] }
    | lowercase identchar * as x                                 { [mkLIDENT x] }
    | uppercase identchar * as x                                 { [mkUIDENT x] }
    | int_literal as i                                              { [mkINT i] }
    | float_literal as f                                          { [mkFLOAT f] }
    | (int_literal as i) "l"                                      { [mkINT32 i] }
    | (int_literal as i) "L"                                      { [mkINT64 i] }
    | (int_literal as i) "n"                                  { [mkNATIVEINT i] }
    | '"' (string_char* as s) '"'  { update_loc c; let x,y = mkSTRING s in x::y }
    | unterminated_string as s                     {               update_loc c ;
                                                    [unterminated1 s Ustring c] }
    | "'" (char_litteral as s) "'"                   { update_loc c; [mkCHAR s] }
    | "(*"                                 { store c; [parse_comment comment c] }
    | "(*)"       { store c; [mkWARNING Comment_start; parse_comment comment c] }
    | "<<" (quotchar* as beginning)
      { if quotations c
        then (move_sp (-String.length beginning) c;
              [parse_quotation quotation c "" ""])
        else parse (symbolchar_star ("<<" ^ beginning)) c                       }
    | "<<>>"
      { if quotations c
        then [mkQUOTATION { q_name = ""; q_loc = ""; q_contents = "" }]
        else parse (symbolchar_star "<<>>") c                                   }
    | "<@"
      { if quotations c then parse_with_sp left_angle_at c
        else parse (symbolchar_star "<@") c                                     }
    | "<:"
      { if quotations c then parse_with_sp left_angle_colon c
        else parse (symbolchar_star "<:") c                                     }
    | "#" ([' ' '\t']* as bl1) ('0'* as zeros) ('0' | ['1'-'9']['0'-'9']* as num)
          ([' ' '\t']* as bl2) ("\"" ([^ '\n' '\r' '"' ] * as name) "\"")?
          ([^ '\n' '\r']* as com) (newline as nl)
                                { let inum = int_of_string num in
                                  let nl = newline_of_string nl in
                                  if line_directives c then
                                    update_absolute_position c name inum
                                  else
                                    update_chars c 0;
                                  [mkLINE_DIRECTIVE{l_blanks1=bl1;
                                                   l_zeros=String.length zeros;
                                                   l_linenum=inum;
                                                   l_blanks2=bl2;
                                                   l_filename=name;
                                                   l_comment=com;
                                                   l_newline=nl}] }
    | '(' (not_star_symbolchar symbolchar* as op) (blank* as post_blanks) ')'
                                                    { mkPSYMBOL ~post_blanks op }
    | '(' (blank+ as pre_blanks) (symbolchar+ as op) (blank* as post_blanks) ')'
                                        { mkPSYMBOL ~pre_blanks ~post_blanks op }
    | ( "#"  | "`"  | "'"  | ","  | "."  | ".." | ":"  | "::"
      | ":=" | ":>" | ";"  | ";;" | "_"
      | left_delimitor | right_delimitor ) as x                  { [mkSYMBOL x] }
    | '$' { if antiquots c
            then parse (dollar (get_sp c)) c
            else parse (symbolchar_star "$") c }
    | ['~' '?' '!' '=' '<' '>' '|' '&' '@' '^' '+' '-' '*' '/' '%' '\\'] symbolchar *
                                                            as x { [mkSYMBOL x] }
    | eof
            { let pos = lexbuf.lex_curr_p in
              lexbuf.lex_curr_p <- { pos with pos_bol  = pos.pos_bol  + 1 ;
                                              pos_cnum = pos.pos_cnum + 1 }; [] }
    | _ as c                                            { [illegal_character c] }

  and comment c = parse
      "(*"                               {                              store c ;
                                                    parse_in Ucomment comment c &
                                                               parse' comment c }
    | "*)"                                                        { store c; [] }
    | '<' (':' ident)? ('@' ident)? '<'
                { store c;
                  (if quotations c then parse_in Uquotation quotation c else []) &
                  parse' comment c }
    | ident                                             { store_parse comment c }
    | string                              { update_loc c; store_parse comment c }
    | unterminated_string           { update_loc c; store c; (c.lexbuf.lex_start_p, Ustring) :: c.stack }
    | "''"                                              { store_parse comment c }
    | char                                { update_loc c; store_parse comment c }
    | eof                                                             { c.stack }
    | newline                         { update_chars c 0; store_parse comment c }
    | _                                                 { store_parse comment c }

  and symbolchar_star beginning c = parse
    | symbolchar* as tok                 { move_sp (-String.length beginning) c ;
                                                   [mkSYMBOL (beginning ^ tok)] }

  (* <@ *)
  and left_angle_at c = parse
    | (ident as loc) '<'                 { [parse_quotation quotation c "" loc] }
    | symbolchar* as tok                               { [mkSYMBOL("<@" ^ tok)] }

  (* <: *)
  and left_angle_colon c = parse
    | (ident as name) '<'               { [parse_quotation quotation c name ""] }
    | (ident as name) '@' (ident as loc) '<'
                                       { [parse_quotation quotation c name loc] }
    | symbolchar* as tok                               { [mkSYMBOL("<:" ^ tok)] }

  and quotation c = parse
    | '<' (':' ident)? ('@' ident)? '<'      {                          store c ;
                                                parse_in Uquotation quotation c &
                                                             parse' quotation c }
    | ">>"                                                        { store c; [] }
    | eof                                                             { c.stack }
    | newline                       { update_chars c 0; store_parse quotation c }
    | _                                               { store_parse quotation c }

  and dollar sp c = parse
    | '$'                                                { [mkANTIQUOT c sp ""] }
    | ('`'? (identchar*|'.'+) as name) ':'         { parse (antiquot sp name) c }
    | _                                        { store_parse (antiquot sp "") c }

  and antiquot sp name c = parse
    | '$'                           { [mkANTIQUOT c sp ~name (buff_contents c)] }
    | eof    {                                                      set_sp c sp ;
               [unterminated1 (sf "$%s:%s" name (buff_contents c)) Uantiquot c] }
    | newline              { update_chars c 0; store_parse (antiquot sp name) c }
    | '<' (':' ident)? ('@' ident)? '<'
      { store c; match parse_in Uquotation quotation c with
                 | [] -> parse (antiquot sp name) c
                 | stack -> [unterminated (buff_contents c) stack] }
    | _                                      { store_parse (antiquot sp name) c }

  {


  let iterator_of_stream s () =
    match Stream.peek s with
    | Some x -> Stream.junk s; Some x
    | None   -> None

  (* If we doesn't want to block on waiting input,
     we can't return more than one element at a time. *)
  let lexing_store next buff max =
    assert (max > 0);
    match next () with
    | Some x -> buff.[0] <- x; 1
    | _      -> 0

  let distribute_location loc = function
    | [] -> []
    | [tok] -> [(tok, loc)]
    | toks ->
      let rec loop pp p = function
        | [] -> assert (p = Loc.stop_pos loc); []
        | WARNING Comment_start as tok :: toks ->
            (tok, p >+> 2) :: loop pp p toks
        | WARNING Comment_not_end as tok :: toks ->
            (tok, p <-< 2) :: loop pp p toks
        | WARNING (Illegal_escape_in_string(s, i)) as tok :: toks ->
            (tok, (pp >>> i) >+> 1 + String.length s) :: loop pp p toks
        | tok :: toks ->
            let p' = p >>> token_width tok in
            (tok, Loc.of_positions p p') :: loop p p' toks
      in
      let p = Loc.start_pos loc
      in loop p p toks

  (* I do not really know what to do about the ``end of input''.
     I see various options:
       1/ The output stream is infinite and repeats EOI indefinitely
          because each time we give eof to the token rule it gives
          us EOI.
       2/ The output stream terminates with a single EOI token.
       3/ The output stream terminates without outputing any EOI token.

    Previously it was 1/, and know it is 3/. Implenting 2/ would require
    some state.

    In this situation EOI could just be removed.
   *)
  let from_context c =
    let next_list () =
      let toks = parse token c in
      let loc = Loc.of_lexbuf c.lexbuf in
      distribute_location loc toks
    in flatten_iterator_list next_list

  let from_lexbuf flags pos lb =
    lb.lex_abs_pos <- pos.pos_cnum;
    lb.lex_curr_p  <- pos;
    from_context { (default_context lb) with flags = flags }

  let from_string flags pos str =
    let lb = Lexing.from_string str in
    from_lexbuf flags pos lb

  let from_channel flags pos ic =
    let lb = Lexing.from_channel ic in
    from_lexbuf flags pos lb

  let from_iterator flags pos next =
    let lb = Lexing.from_function (lexing_store next) in
    from_lexbuf flags pos lb

  let from_stream flags pos strm =
    from_iterator flags pos (iterator_of_stream strm)
end
}
