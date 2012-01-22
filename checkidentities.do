# vim: ft=sh
find tests -path '*.t/*' \( -name stdin -o -name '*.ml' \) |
  xargs ./tests/check-identities >&2
