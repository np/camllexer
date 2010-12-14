open Lexing

type 'a located = { before_pos : position
                  ; located    : 'a
                  ; after_pos  : position }

val locate     : position -> 'a -> position -> 'a located
val located    : 'a located -> 'a
val before_pos : 'a located -> position
val after_pos  : 'a located -> position
