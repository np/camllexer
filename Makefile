build:
	ocamlbuild pplex.byte
test:
	find tests -path '*.t/*' \( -name stdin -o -name '*.ml' \) -print0 | xargs -0 ./tests/check-roundtrip -f -w && \
	find tests -path '*.t/*' \( -name stdin -o -name '*.ml' \) -print0 | xargs -0 ./tests/check-roundtrip -A -Q -f -w && \
	PPLEX=$(PWD)/_build/pplex.byte cmdcheck tests/*.t
