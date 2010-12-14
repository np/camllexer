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
open Camllexer
open Lexing
open Located

let (|>) x f = f x
let (<.>) f g x = f (g x)
let id x = x
let sf = Printf.sprintf
let (>>) f g x = f x; g x

let opt_map f = function
  | Some x -> Some (f x)
  | None   -> None

let isWARNING = function
  | WARNING _ -> true
  | _         -> false

let rec iter f next =
  match next () with
  | Some x -> f x; iter f next
  | None   -> ()

let rec filter p next () =
  match next () with
  | Some x -> if p x then Some x else filter p next ()
  | None -> None

let map f next = opt_map f <.> next

let drop_loc = opt_map located

let rec strings next =
  let x = next () in
  match drop_loc x with
  | Some (BLANKS " ") ->
      let y = next () in
      begin match drop_loc y with
      | Some (STRING(z,_)) ->
          let (zs, t) = strings next in
          (z :: zs, t)
      | _ -> ([], y)
      end
  | _ -> ([], x)

exception LexError of error
exception Unexpected_token of caml_token
exception Unexpected_EOI
exception Token_of_strings_error of string * string list
exception Exc_located of exn located

let raise located_exn = raise (Exc_located located_exn)

let unparse_tokens next () =
  match next () with
  | None -> None
  | Some tok ->
      match tok.located with
      | UIDENT name ->
          let (args, lh) = strings next in
          begin match lh with
          | None -> raise{tok with located = Unexpected_EOI}
          | Some t2 ->
              match t2.located with
              | NEWLINE _ ->
                begin match token_of_strings (name, args) with
                | Some x -> Some{tok with located = x}
                | None   -> raise{tok with located = Token_of_strings_error (name, args)}
                end
              | _ -> raise{t2 with located = Unexpected_token t2.located}
          end
      | _ -> raise{tok with located = Unexpected_token tok.located}

let rec rm x = function
  | [] -> (false, [])
  | y :: ys -> if x = y then (true, ys)
               else let (b,zs) = rm x ys in (b, y :: zs)

let mk_position fp = { pos_fname = fp; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }

let string_of_loc (x, y) =
  assert (x.pos_fname = y.pos_fname);
  let res = sf "File \"%s\", line %d, characters %d-%d"
               x.pos_fname x.pos_lnum
               (x.pos_cnum - x.pos_bol) (y.pos_cnum - x.pos_bol) in
  if x.pos_lnum <> y.pos_lnum then
    sf "%s (end at line %d, character %d)"
       res y.pos_lnum (y.pos_cnum - y.pos_bol)
  else res

let location_of_located x = string_of_loc (x.before_pos, x.after_pos)

let main () : unit =
  let usage () =
    Printf.eprintf "Usage: pplex [<option>] [-|<file.ml>]\n";
    Printf.eprintf "Options:\n";
    Printf.eprintf " -s Show the token in a easily parsable format\n";
    Printf.eprintf " -p Show positions\n";
    Printf.eprintf " -l Show locations of tokens\n";
    Printf.eprintf " -r Reverse the preprocessor by reading the -s output format\n";
    Printf.eprintf " -f Enable fault tolerance\n";
    Printf.eprintf " -Q Enable the lexing of quotations\n";
    Printf.eprintf " -A Enable the lexing of anti-quotations\n";
    Printf.eprintf " -d Disable the effect of # line directives\n";
    Printf.eprintf " -w Disable warnings (twice to hide the token as well)\n";
    Printf.eprintf " -h Display this help and exit\n";
    exit 1
  in
  let argv = Array.to_list Sys.argv in
  let argv = List.tl argv in
  let positions, argv = rm "-p" argv in
  let locations, argv = rm "-l" argv in
  let show_tokens, argv = rm "-s" argv in
  let reverse, argv = rm "-r" argv in
  let fault_tolerant, argv = rm "-f" argv in
  let quotations, argv = rm "-Q" argv in
  let antiquotations, argv = rm "-A" argv in
  let no_line_directives, argv = rm "-d" argv in
  let line_directives = not no_line_directives in
  let no_warnings, argv = rm "-w" argv in
  let nor_WARNING, argv = rm "-w" argv in
  let help, argv = rm "-h" argv in
  let () = if help then usage () in
  let filename =
    match argv with
    | [filename]  -> filename
    | []          -> "-"
    | _           -> usage ()
  in
  let ic = if filename = "-" then stdin else open_in filename in
  let flags = { quotations = quotations
              ; antiquotations = antiquotations
              ; line_directives = line_directives } in
  let next = Camllexer.from_channel flags (mk_position filename) ic in
  let show_warnings x =
    match x.located with
    | WARNING w -> Printf.eprintf "Warning: %s: %s\n%!" (location_of_located x) (message_of_warning w)
    | _ -> ()
  in
  let raise_errors x =
    match x.located with
    | ERROR(_, (Unterminated [] as err)) ->
        raise{x with located = LexError err}
    | ERROR(_, (Unterminated ((bpos, _) :: _) as err)) ->
        raise{x with before_pos = bpos; located = LexError err}
    | ERROR(_, err) -> raise{x with located = LexError err}
    | _ -> ()
  in
  let print_pos p = Printf.printf "File \"%s\", line %d, character %d\n"
                                  p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol) in
  let show_pos = print_pos <.> before_pos in
  let show_loc = Printf.printf "%s\n" <.> location_of_located in
  let show_token_nl = print_endline <.> show_token <.> located in
  let print_token = print_string <.> string_of_token <.> located in
  let rec string_of_exn = function
    | LexError err    -> string_of_error err
    | Token_of_strings_error (name, args) ->
        sf "Parse Error: %s %s" name (String.concat " " (List.map String.escaped args))
    | Unexpected_token tok -> sf "Unexpected token: %s" (string_of_token tok)
    | Unexpected_EOI -> "Unexpected end of input"
    | Exc_located lexn ->
        sf "%s: %s" (location_of_located lexn) (string_of_exn lexn.located)
    | exn             -> Printexc.to_string exn
  in
  let show = show_tokens || positions || locations in
  try
    (if reverse then unparse_tokens next else next) |>
    (if nor_WARNING then filter (fun x -> not (isWARNING x.located)) else id) |>
    iter        ((if no_warnings    then ignore        else show_warnings) >>
                 (if fault_tolerant then ignore        else raise_errors)  >>
                 (if positions      then show_pos      else ignore)        >>
                 (if locations      then show_loc      else ignore)        >>
                 (if show_tokens    then show_token_nl else ignore)        >>
                 (if show           then ignore        else print_token))
  with exn ->
    begin
      if Printexc.backtrace_status () then
        Printexc.print_backtrace stderr;
      Printf.eprintf "Error: %s\n%!" (string_of_exn exn);
      exit 1
    end
;;

main ()
