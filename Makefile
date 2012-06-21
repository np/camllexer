build:
	ocaml setup.ml -build
checkidentities:
	./tests/check-identities `find tests -path '*.t/*' \( -name stdin -o -name '*.ml' \)`
regtest:
	PPLEX=$(PWD)/_build/Pplex.byte cmdcheck tests/*.t
test: checkidentities regtest
bigtest:
	./tests/check-identities local/1m.ml local/huge.ml
