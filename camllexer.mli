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

module type LOC = sig
  type t
  val ghost : t
  val of_lexbuf : Lexing.lexbuf -> t
  val merge : t -> t -> t
  val move_both : int -> t -> t
  val start_pos : t -> Lexing.position
  val to_string : t -> string
  exception Exc_located of t * exn
  val raise : t -> exn -> 'a
end

type 'a iterator = unit -> 'a option

module Make : functor (Loc : LOC) -> sig
  type token = Camltoken.caml_token * Loc.t

  val setup_loc : Lexing.lexbuf -> Loc.t -> unit

  val from_lexbuf :
    quotations:bool ->
    antiquotations:bool ->
    warnings:bool ->
    Lexing.lexbuf -> token iterator

  val from_string :
    quotations:bool ->
    antiquotations:bool ->
    warnings:bool ->
    Loc.t -> string -> token iterator

  val from_stream :
    quotations:bool ->
    antiquotations:bool ->
    warnings:bool ->
    Loc.t -> char Stream.t -> token iterator
end
