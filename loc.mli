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

type t

(** Return a start location for the given file name.
    This location starts at the begining of the file. *)
val mk : string -> t

(** The [ghost] location can be used when no location
    information is available. *)
val ghost : t

(** {6 Conversion functions} *)

(** Return a location from a couple of postitions. *)
val of_positions : Lexing.position -> Lexing.position -> t

(** Return a location from ocamllex buffer. *)
val of_lexbuf : Lexing.lexbuf -> t

(** [merge loc1 loc2] Return a location that starts at [loc1] and end at [loc2]. *)
val merge : t -> t -> t

(** [move_both n loc]
    Return the location where positions are moved.
    Both positions are affected.
    Returned positions have their character offset plus [n]. *)
val move_both : int -> t -> t

(** {6 Accessors} *)

(** Return the start position as a Lexing.position. *)
val start_pos  : t -> Lexing.position

(** Return the stop position as a Lexing.position. *)
val stop_pos  : t -> Lexing.position

(** {6 Printing} *)

(** Return a string from the location in a format suitable for error
    reporting. *)
val to_string : t -> string

(** {6 Exceptions} *)

(** [Exc_located loc e] is an encapsulation of the exception [e] with
    the input location [loc]. To be used in quotation expanders
    and in grammars to specify some input location for an error.
    Do not raise this exception directly: rather use the following
    function [Loc.raise]. *)
exception Exc_located of t * exn

(** [raise loc e], if [e] is already an [Exc_located] exception,
    re-raise it, else raise the exception [Exc_located loc e]. *)
val raise : t -> exn -> 'a
