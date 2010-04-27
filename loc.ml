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
 * - Daniel de Rauglaudre: initial version
 * - Nicolas Pouillard: refactoring
 *)

type pos = {
  line : int;
  bol  : int;
  off  : int
}

type t = {
  file_name : string;
  start     : pos;
  stop      : pos;
  ghost     : bool
}

let init_pos = { line = 1 ; bol = 0 ; off = 0 }

let mk file_name =
  { file_name = file_name;
    start     = init_pos ;
    stop      = init_pos ;
    ghost     = false    }

let ghost =
  { file_name = "ghost-location";
    start     = init_pos;
    stop      = init_pos;
    ghost     = true     }

let pos_of_lexing_position p =
  let pos =
  { line = p.Lexing.pos_lnum ;
    bol  = p.Lexing.pos_bol  ;
    off  = p.Lexing.pos_cnum } in
  pos

let pos_to_lexing_position p file_name =
  { Lexing.
    pos_fname = file_name;
    pos_lnum  = p.line   ;
    pos_bol   = p.bol    ;
    pos_cnum  = p.off    }

let better_file_name a b =
  match (a, b) with
  | ("", "") -> a
  | ("", x)  -> x
  | (x, "")  -> x
  | ("-", x) -> x
  | (x, "-") -> x
  | (x, _)   -> x

let of_lexbuf lb =
  let start = Lexing.lexeme_start_p lb
  and stop  = Lexing.lexeme_end_p lb in
  let loc =
  { file_name = better_file_name start.Lexing.pos_fname stop.Lexing.pos_fname;
    start     = pos_of_lexing_position start;
    stop      = pos_of_lexing_position stop;
    ghost     = false } in
  loc

let start_pos x = pos_to_lexing_position x.start x.file_name

let merge a b =
  if a == b then
    a
  else
    let r =
      match (a.ghost, b.ghost) with
      | (false, false) -> { (a) with stop = b.stop }
      | (true, true) -> { (a) with stop = b.stop }
      | (true, _) -> { (a) with stop = b.stop }
      | (_, true) -> { (b) with start = a.start }
    in r

let map_both f x = { (x) with start = f x.start; stop  = f x.stop }

let move_pos chars x = { (x) with off = x.off + chars }

let move_both chars x = map_both (move_pos chars) x

let to_string x =
  let (a, b) = (x.start, x.stop) in
  let res = Printf.sprintf "File \"%s\", line %d, characters %d-%d"
                    x.file_name a.line (a.off - a.bol) (b.off - a.bol) in
  if x.start.line <> x.stop.line then
    Printf.sprintf "%s (end at line %d, character %d)"
            res x.stop.line (b.off - b.bol)
  else res

exception Exc_located of t * exn

let raise loc exc =
  match exc with
  | Exc_located(_, _) -> raise exc
  | _ -> raise (Exc_located (loc, exc))
