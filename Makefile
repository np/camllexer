build:
	ocamlbuild pplex.byte
test:
	INPUTS=`find tests -path '*.t/*' \( -name stdin -o -name '*.ml' \)` ; \
	./tests/check-roundtrip -f -w $$INPUTS && \
	./tests/check-roundtrip -A -Q -f -w $$INPUTS && \
	PPLEX=$(PWD)/_build/pplex.byte cmdcheck tests/*.t
