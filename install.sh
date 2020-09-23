#! /usr/bin/env bash

# The user that called the script.
user=$(logname)

# The dotfiles directory.
directory="$PWD"

# The symlinks file.
bindings=bindings

while read -r file destination; do
    filepath="$directory/$file"
    link="${destination//\~/\~$user}"
    echo "Binding $link to file $filepath"
    ln -sfn "$filepath" $(eval echo "$link")
done < $bindings
