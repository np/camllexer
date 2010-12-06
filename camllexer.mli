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
  val move_both : int -> t -> t
  val start_pos : t -> Lexing.position
  val to_string : t -> string
  exception Exc_located of t * exn
  val raise : t -> exn -> 'a
end

type 'a iterator = unit -> 'a option

type flags = { quotations      : bool  (** Enables the lexing of quotations *)
             ; antiquotations  : bool  (** Enables the lexing of anti-quotations *)
             ; warnings        : bool  (** Enables the production of warnings *)
             }

val default_flags : flags
(** By default, quotations and anti-quotations are NOT recognized,
    warnings are produced. *)

module Make : functor (Loc : LOC) -> sig
  type token = Camltoken.caml_token * Loc.t

  val setup_loc : Lexing.lexbuf -> Loc.t -> unit

  val from_lexbuf : flags -> Lexing.lexbuf -> token iterator

  val from_string : flags -> Loc.t -> string -> token iterator

  val from_channel : flags -> Loc.t -> in_channel -> token iterator

  val from_stream : flags -> Loc.t -> char Stream.t -> token iterator
end
