(* COMMENT, SYMBOL, LIDENT, INT, BLANKS, NEWLINE *)
let x  =  42
(* UIDENT, INT32, INT64, NATIVEINT, CHAR, STRING *)
let (Some ((+), ( * ))) = Some (42l, 42L, 42n, 4.2, '?', '\'', '\n',
                                "string", "\t\n\"'", "
",'
')
(* LINE_DIRECTIVE *)
# 42
# 42 "somefilename.ml"
(* LABEL, OPTLABEL *)
let f g x y = g ~x:x ?y:y
(* QUOTATION *)
<<foo>> <:bar<baz>> <<foo$bar$baz>> <:bar<foo$bar$baz>>
<@loca<foo>> <:expr@loc<foo>> <<abc<:def<ghi>>klm>>
(* ANTIQUOT *)
$foo:bar$ $baz$ $`foo:bar$ $..:bar$
caml $antiquo << quot $antiquo$ quot >> antiquo$ caml

(* This is not a quotation *)
caml $<< antiquo $caml$ antiquo >>$ caml
