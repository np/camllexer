build:
	ocamlbuild pplex.byte
test:
	@PPLEX=$(PWD)/_build/pplex.byte cmdcheck tests/*.t
	find tests -path '*.t/*' \( -name stdin -o -name '*.ml' \) -exec ./tests/check-roundtrip -f -w {} \;
