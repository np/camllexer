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
open Lexing
module Lex = Camllexer.Make(Loc)

let sf = Printf.sprintf

let rec iter_tokens (f : (caml_token * 'a) -> unit) : (caml_token * 'a) Stream.t -> unit =
  parser
  | [< '(EOI, _) >] -> ()
  | [< 'x ; xs >] -> f x; iter_tokens f xs
  | [<>] -> ()

let rec strings =
  parser
  | [< '(BLANKS " ",_) ; '(STRING(x,_),_) ; xs = strings >] -> x :: xs
  | [<>] -> []

exception LexError of error
exception Unexpected_token of caml_token
exception Token_of_strings_error of string * string list

let rec unparse_tokens =
  parser
  | [< '(UIDENT name, loc); args = strings; '(NEWLINE _,_); strm >] ->
      begin match token_of_strings (name, args) with
      | Some x -> [< '(x,loc); unparse_tokens strm >]
      | None   -> Loc.raise loc (Token_of_strings_error (name, args))
      end
  | [< '(EOI,loc) >] -> [< '(eoi,loc) >]
  | [< '(tok,loc) >] -> Loc.raise loc (Unexpected_token tok)
  | [<>] -> [<>]

let main () =
  let usage () = 
    Printf.eprintf "Usage: pplex [-s|-r|-f|-Q|-A|-h] [-|<file.ml>]\n";
    Printf.eprintf " -s Show the token in a easily parsable format\n";
    Printf.eprintf " -p Show positions between tokens (implies -s)\n";
    Printf.eprintf " -r Reverse the preprocessor by reading the -s output format\n";
    Printf.eprintf " -f Enable fault tolerance\n";
    Printf.eprintf " -Q Enable the lexing of quotations\n";
    Printf.eprintf " -A Enable the lexing of anti-quotations\n";
    Printf.eprintf " -w Disable warnings\n";
    Printf.eprintf " -h Display this help and exit\n";
    exit 1
  in
  let argv = Array.to_list Sys.argv in
  let rm x xs = List.mem x xs, List.filter ((<>) x) xs in
  let argv = List.tl argv in
  let positions, argv = rm "-p" argv in
  let show, argv = rm "-s" argv in
  let reverse, argv = rm "-r" argv in
  let fault_tolerant, argv = rm "-f" argv in
  let quotations, argv = rm "-Q" argv in
  let antiquotations, argv = rm "-A" argv in
  let not_warnings, argv = rm "-w" argv in
  let show = show || positions in
  let warnings = not not_warnings in
  let help, argv = rm "-h" argv in
  let () = if help then usage () in
  let filename =
    match argv with
    | [filename]  -> filename
    | []          -> "-"
    | _           -> usage ()
  in
  let loc = Loc.mk filename in
  let ic = if filename = "-" then stdin else open_in filename in
  let next = Lex.from_channel ~quotations ~antiquotations ~warnings loc ic in
  let strm = Stream.from (fun _ -> next ()) in
  let loc_of_unterminated loc = function
    | (pos, _) :: _ -> Loc.of_postions pos (Loc.stop_pos loc)
    | [] -> loc
  in
  let dont_raise_errors f (x, loc) = f loc x in
  let raise_errors f (x, loc) =
    match x with
    | ERROR(_, (Unterminated us as err)) -> Loc.raise (loc_of_unterminated loc us) (LexError err)
    | ERROR(_, err) -> Loc.raise loc (LexError err)
    | _ -> f loc x
  in
  let print_pos p = Printf.printf "File \"%s\", line %d, character %d\n"
                                  p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol) in
  let show_token_nl x = print_string (show_token x); print_char '\n' in
  let show_token_nl =
    if positions then fun loc x -> print_pos (Loc.start_pos loc);
                                   show_token_nl x
    else fun _ -> show_token_nl
  in
  let print_token _ x = print_string (string_of_token x) in
  let rec string_of_exn = function
    | LexError err    -> string_of_error err
    | Token_of_strings_error (name, args) ->
        sf "Parse Error: %s %s" name (String.concat " " (List.map String.escaped args))
    | Unexpected_token tok -> sf "Unpexcted token: %s" (string_of_token tok)
    | Stream.Error "" -> "Unknown stream error"
    | Stream.Error msg -> sf "Stream Error: %s" msg
    | Loc.Exc_located(loc, exn) ->
        sf "%s: %s" (Loc.to_string loc) (string_of_exn exn)
    | exn             -> Printexc.to_string exn
  in
  try
    iter_tokens ((if fault_tolerant then dont_raise_errors else raise_errors)
                 (if show then show_token_nl else print_token))
                (if reverse then unparse_tokens strm else strm)
  with exn ->
    (Printf.eprintf "Error: %s\n%!" (string_of_exn exn); exit 1)
;;

main ()
