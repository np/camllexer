open Substloc_lex
let lex = Substloc_lex.loc
let line = Substloc_lex.line

let print_substring = output stdout

let grab oc fn (bline, bchr) (eline, echr) =
  let ic = open_in fn in
  let lb = Lexing.from_channel ic in
  let rec start lnum =
    match line lb with
    | None -> close ()
    | Some ln ->
        if lnum = bline then
          go ln lnum bchr
        else
          start (lnum + 1)
  and go ln lnum cnum =
    if lnum = eline then begin
      output oc ln cnum (echr - cnum);
      close ()
    end else begin
      output oc ln cnum (String.length ln - cnum);
      match line lb with
      | None -> close ()
      | Some ln -> go ln (lnum + 1) 0
    end
  and close () = close_in ic
  in start 1

let main ic oc =
  let lb = Lexing.from_channel ic in
  let rec go () =
    match lex lb with
    | LOC(fn, start, stop) ->
        begin try
          grab oc fn start stop
        with _ ->
          output_string oc (Lexing.lexeme lb)
        end; go ()
    | CHR c ->
        output_char oc c; go ()
    | EOI ->
        ()
  in go ()

let () = main stdin stdout
