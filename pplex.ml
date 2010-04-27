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

let rec iter_tokens (f : caml_token -> unit) : (caml_token * Loc.t) Stream.t -> unit =
  parser
  | [< '(x,_) ; xs >] ->
      if x <> EOI then (f x; iter_tokens f xs)
  | [<>] -> ()

let main () =
  let usage () = 
    Printf.eprintf "Usage: pplex [-s] ( - | <file.ml> )\n%!"; exit 1
  in
  let argv = Array.to_list Sys.argv in
  let filename, show =
    match List.tl argv with
    | ["-s";filename] -> filename, true
    | ["-s"]          -> usage ()
    | [filename]      -> filename, false
    | _::_ | []       -> usage ()
  in
  let loc = Loc.mk filename in
  let ic = if filename = "-" then stdin else open_in filename in
  let lexbuf = Lexing.from_channel ic in
  let () = Lex.setup_loc lexbuf loc in
  let strm = Lex.from_lexbuf ~quotations:true ~antiquotations:true lexbuf in
  let show_token_nl x = print_string (show_token x); print_char '\n' in
  let print_token x = print_string (token_to_string x) in
  try
    iter_tokens (if show then show_token_nl else print_token) strm
  with
    Loc.Exc_located(loc, Lex.Error err) -> 
      Printf.eprintf "Error: %s: %s\n%!"
        (Loc.to_string loc) (Lex.string_of_error err)
;;

main ()
