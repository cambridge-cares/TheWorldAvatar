#!/bin/sh

INPUT_PATH=$1
OUTPUT_PATH=$2

ct2-transformers-converter --force --model $INPUT_PATH --output_dir $OUTPUT_PATH
cp "$INPUT_PATH/spiece.model" "$OUTPUT_PATH/spiece.model"