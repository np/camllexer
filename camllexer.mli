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

type 'a iterator = unit -> 'a option

type flags = { quotations      : bool  (** Enables the lexing of quotations *)
             ; antiquotations  : bool  (** Enables the lexing of anti-quotations *)
             ; line_directives : bool  (** Honor the # line directives *)
             }

val default_flags : flags
(** By default, quotations and anti-quotations are NOT recognized,
    and line_directives are honored. *)

type position = Lexing.position

type 'a located = { before_pos : position
                  ; located    : 'a
                  ; after_pos  : position }

val located : position -> 'a -> position -> 'a located

type token = Camltoken.caml_token located

val from_lexbuf : flags -> position -> Lexing.lexbuf -> token iterator

val from_string : flags -> position -> string -> token iterator

val from_channel : flags -> position -> in_channel -> token iterator

val from_stream : flags -> position -> char Stream.t -> token iterator

val from_iterator : flags -> position -> char iterator -> token iterator
