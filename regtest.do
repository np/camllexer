export PPLEX=$(pwd)/pplex.byte
redo-ifchange "$PPLEX"
cmdcheck tests/*.t
