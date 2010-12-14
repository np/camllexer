open Lexing

type 'a located = { before_pos : position
                  ; located    : 'a
                  ; after_pos  : position }

let locate bpos x apos = { before_pos = bpos; located = x; after_pos = apos }

let located x = x.located
let before_pos x = x.before_pos
let after_pos x = x.after_pos

