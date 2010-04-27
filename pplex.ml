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

open Camltoken
module Lex = Camllexer.Make(Loc)

let rec drop_locs : (caml_token * Loc.t) Stream.t -> caml_token Stream.t =
  parser
  | [< '(x,_) ; xs >] -> [< 'x; drop_locs xs >]
  | [<>] -> [<>]

let rec iter_tokens (f : caml_token -> unit) : caml_token Stream.t -> unit =
  parser
  | [< 'x ; xs >] -> if x <> EOI then (f x; iter_tokens f xs)
  | [<>] -> ()

let rec strings =
  parser
  | [< '(BLANKS " ",_) ; '(STRING(x,_),_) ; xs = strings >] -> x :: xs
  | [<>] -> []

exception PPLexParseError

let rec unparse_tokens =
  parser
  | [< '(UIDENT name, loc); args = strings; '(NEWLINE,_); strm >] ->
      begin match token_of_strings (name, args) with
      | Some x -> [< '(x,loc); unparse_tokens strm >]
      | None   -> Loc.raise loc PPLexParseError
      end
  | [< '(EOI,loc) >] -> [< '(EOI,loc) >]
  | [< '(_,loc) >] -> Loc.raise loc PPLexParseError
  | [<>] -> raise PPLexParseError

let main () =
  let usage () = 
    Printf.eprintf "Usage: pplex [-s|-r] ( - | <file.ml> )\n%!";
    Printf.eprintf " -s Shows the token in a easily parsable format\n%!";
    Printf.eprintf " -r Reverse the preprocessor by reading the -s output format\n%!";
    exit 1
  in
  let argv = Array.to_list Sys.argv in
  let rm x xs = List.mem x xs, List.filter ((<>) x) xs in
  let filename, show, reverse =
    let argv = List.tl argv in
    let show, argv = rm "-s" argv in
    let reverse, argv = rm "-r" argv in
    match argv with
    | [filename] -> filename, show, reverse
    | _::_ | []  -> usage ()
  in
  let loc = Loc.mk filename in
  let ic = if filename = "-" then stdin else open_in filename in
  let lexbuf = Lexing.from_channel ic in
  let () = Lex.setup_loc lexbuf loc in
  let strm = Lex.from_lexbuf ~quotations:true ~antiquotations:true lexbuf in
  let show_token_nl x = print_string (show_token x); print_char '\n' in
  let print_token x = print_string (token_to_string x) in
  let exn_to_string = function
    | Lex.Error err   -> Lex.string_of_error err
    | PPLexParseError -> "Unexpected token"
    | exn             -> Printexc.to_string exn
  in
  try
    iter_tokens (if show then show_token_nl else print_token)
                (drop_locs (if reverse then unparse_tokens strm else strm))
  with
    Loc.Exc_located(loc, exn) ->
      Printf.eprintf "Error: %s: %s\n%!" (Loc.to_string loc) (exn_to_string exn)
;;

main ()
