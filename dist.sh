#!/bin/bash

dist_files=(META
            myocamlbuild.ml
            camllexer.mllib
            setup.ml)
exclude_pattern='.gitignore\|.gitattributes\|local'

base_files=( $(git ls-files | grep -v "$exclude_pattern") )
name="$(sed -n "s/Name:[ \t]*//p" _oasis)"
version="$(sed -n "s/Version:[ \t]*//p" _oasis)"
canonical_name="$name-$version"
dir=_dist/"$canonical_name"

rm -rf "$dir" "$dir".tar.gz
mkdir -p "$dir"

for f in "${dist_files[@]}" "${base_files[@]}"; do
    mkdir -p "$dir/$(dirname "$f")"
    cp -R "$f" "$dir/$f"
done

echo Producing "$dir".tar.gz...
tar -C "$(dirname "$dir")" -czf "$dir".tar.gz "$canonical_name"
rm -rf "$dir"
