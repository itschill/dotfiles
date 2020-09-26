#! /usr/bin/env bash

# The user that called the script.
user=$(logname)

# The dotfiles directory.
directory="$PWD"

# The symlinks file.
bindings=bindings

while read -r file destination; do
    [[ $file =~ ^#.* ]] && continue # ignore commented lines

    filepath="$directory/$file"
    link="${destination//\~/\~$user}"

    if [ -e $filepath ]; then
        echo "Binding $link to file $filepath"
        ln -sfn "$filepath" $(eval echo "$link")
    else
        echo "File $filepath does not exist, ignoring"
    fi
done < $bindings
