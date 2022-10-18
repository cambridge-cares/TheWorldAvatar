rem @echo off
setlocal
set COMPILE_FLAGS=-fcheck=all -Wall -Wextra -O2 -static -Wno-compare-reals -Wno-character-truncation -std=f2008 -ffree-form
set LINK_FLAGS= -static -O2

gfortran -c %COMPILE_FLAGS% mod_file_units.f90    
gfortran -c %COMPILE_FLAGS% mod_main1.f90
gfortran -c %COMPILE_FLAGS% mod_upperair.f90
gfortran -c %COMPILE_FLAGS% mod_surface.f90
gfortran -c %COMPILE_FLAGS% mod_onsite.f90
gfortran -c %COMPILE_FLAGS% mod_pbl.f90
gfortran -c %COMPILE_FLAGS% mod_read_input.f90    
gfortran -c %COMPILE_FLAGS% mod_reports.f90
gfortran -c %COMPILE_FLAGS% mod_misc.f90
gfortran -c %COMPILE_FLAGS% aermet.f90            

gfortran -o aermet.exe %LINK_FLAGS% mod_file_units.o mod_main1.o mod_upperair.o mod_surface.o mod_onsite.o mod_pbl.o mod_read_input.o mod_reports.o mod_misc.o aermet.o 

del *.o
del *.mod