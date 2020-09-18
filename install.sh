#! /usr/bin/env bash

# The dotfiles directory.
directory="$PWD"

# The symlinks file.
bindings=bindings

while read -r file link; do
    filepath="$directory/$file"
    echo "Binding $link to file $filepath"
    ln -sfn "$filepath" $(eval echo "$link")
done < $bindings
