default all build:
	@echo The build system is using oasis, here is a quick walkthrough:
	@echo 1. If you got the sources from the repository you need to setup oasis:
	@echo $$ oasis setup
	@echo 2. Configure
	@echo $$ ocaml setup.ml -configure
	@echo 3. Build
	@echo $$ ocaml setup.ml -build
	@echo For more commands and options:
	@echo $$ ocaml setup.ml -help
checkidentities:
	./tests/check-identities `find tests -path '*.t/*' \( -name stdin -o -name '*.ml' \)`
regtest:
	PPLEX=$(PWD)/_build/Pplex.byte cmdcheck tests/*.t
test: checkidentities regtest
bigtest:
	./tests/check-identities local/1m.ml local/huge.ml
