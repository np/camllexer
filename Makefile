build:
	ocamlbuild pplex.byte substloc.byte
test:
	INPUTS=`find tests -path '*.t/*' \( -name stdin -o -name '*.ml' \)` ; \
	./tests/check-roundtrip -f -w $$INPUTS && \
	./tests/check-roundtrip -A -Q -f -w $$INPUTS && \
	./tests/check-locs -f -w -w $$INPUTS && \
	./tests/check-locs -A -Q -f -w -w $$INPUTS && \
	PPLEX=$(PWD)/_build/pplex.byte cmdcheck tests/*.t
