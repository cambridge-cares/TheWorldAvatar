#!/bin/bash

#
# Fixes stranges characters in the raw NHS data files used within this example.
#

# Loop through arguments
while test $# -gt 0
do
    FILENAME="$(basename $1)"
    mkdir -p ./fixed
    tr $'\xAC\x92' $'\x09\x27' < "$1" | iconv -f utf-8 -t utf-8 -c > ./fixed/${FILENAME}

    echo "Written fixed file to ./fixed/${FILENAME}"
    shift
done