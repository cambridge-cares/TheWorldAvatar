#!/bin/bash

COMPILE_FLAGS="-fcheck=all -Wall -Wextra -O2 -static -Wno-compare-reals -Wno-character-truncation -std=f2008 -ffree-form"
LINK_FLAGS="-static -O2 -g"

gfortran -c ${COMPILE_FLAGS} mod_file_units.f90    
gfortran -c ${COMPILE_FLAGS} mod_main1.f90
gfortran -c ${COMPILE_FLAGS} mod_upperair.f90
gfortran -c ${COMPILE_FLAGS} mod_surface.f90
gfortran -c ${COMPILE_FLAGS} mod_onsite.f90
gfortran -c ${COMPILE_FLAGS} mod_pbl.f90
gfortran -c ${COMPILE_FLAGS} mod_read_input.f90    
gfortran -c ${COMPILE_FLAGS} mod_reports.f90
gfortran -c ${COMPILE_FLAGS} mod_misc.f90
gfortran -c ${COMPILE_FLAGS} aermet.f90            

gfortran -o aermet ${LINK_FLAGS} file_units.mod main1.mod upperair.mod surface.mod onsite.mod pbl.mod read_input.mod reports.mod misc.mod aermet.o 

rm *.o
rm *.mod