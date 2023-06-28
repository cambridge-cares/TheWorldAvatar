#!/bin/bash

COMPILE_FLAGS="-fcheck=all -Wall -Wextra -O2 -static -Wno-compare-reals -Wno-character-truncation -std=f2008 -ffixed-form"
LINK_FLAGS="-static -O2 -g"

gfortran ${COMPILE_FLAGS} Bpipprm.for -o bpipprm

