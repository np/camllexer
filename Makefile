build:
	ocamlbuild pplex.byte substloc.byte
debug:
	ocamlbuild -tags debug pplex.byte substloc.byte
checkidentities:
	./tests/check-identities `find tests -path '*.t/*' \( -name stdin -o -name '*.ml' \)`
regtest:
	PPLEX=$(PWD)/_build/pplex.byte cmdcheck tests/*.t
test: checkidentities regtest
bigtest:
	./tests/check-identities local/1m.ml local/huge.ml
