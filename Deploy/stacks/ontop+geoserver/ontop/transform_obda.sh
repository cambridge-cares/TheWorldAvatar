#!/bin/bash
pushd $(dirname "$0")
pwd="$(pwd)"
input_dir=${1:-.}
output_dir=${2:-inputs}
mkdir -p "$output_dir"
echo "input_dir: "$input_dir
echo "output_dir: "$output_dir
sed -e '/# /d' -e '/^ *#/d' "${input_dir}/ontop_with_comments.obda" | sed -e ':a' -e 'N' -e '$!ba' -e 's/; *\r*\n  */; /g' -e 's/, *\r*\n  */, /g' -e 's/\. *\r*\n  */. /g' > ${output_dir}/ontop.obda
popd

