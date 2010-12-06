{

type token = LOC of string * (int * int) * (int * int)
           | CHR of char
           | EOI

let mkLOC fn (bline, bchar) (eline, echar) =
  let bline = int_of_string bline in
  let bchar = int_of_string bchar in
  let eline = int_of_string eline in
  let echar = int_of_string echar in
  LOC(fn, (bline, bchar), (eline, echar))
}

let int = ['0'-'9']+

rule loc = parse
  | "File \"" ([^'"']* as file_name) "\", line " (int as line) ", characters "
    (int as start) '-' int " (end at line " (int as stop_line)
    ", character " (int as stop) ')' [^'\n']* '\n'

    { mkLOC file_name (line, start) (stop_line, stop) }

  | "File \"" ([^'"']* as file_name) "\", line " (int as line) ", characters "
    (int as start) '-' (int as stop) ([^'0'-'9' '\n'] [^'\n']*)? '\n'

    { mkLOC file_name (line, start) (line, stop) }

  | eof { EOI }

  | _ as c { CHR c }

and line = parse
  | [^ '\n']* '\n'? as ln { Some ln }
  | eof { None }
