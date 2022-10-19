rem @echo off
setlocal
set COMPILE_FLAGS= -fbounds-check -Wuninitialized -O2 -static
set LINK_FLAGS= -static -O2

gfortran -c %COMPILE_FLAGS% modules.f    
gfortran -c %COMPILE_FLAGS% grsm.f 
gfortran -c %COMPILE_FLAGS% aermod.f   
gfortran -c %COMPILE_FLAGS% setup.f    
gfortran -c %COMPILE_FLAGS% coset.f    
gfortran -c %COMPILE_FLAGS% soset.f    
gfortran -c %COMPILE_FLAGS% reset.f    
gfortran -c %COMPILE_FLAGS% meset.f    
gfortran -c %COMPILE_FLAGS% ouset.f    
gfortran -c %COMPILE_FLAGS% inpsum.f   
gfortran -c %COMPILE_FLAGS% metext.f   
gfortran -c %COMPILE_FLAGS% iblval.f   
gfortran -c %COMPILE_FLAGS% siggrid.f  
gfortran -c %COMPILE_FLAGS% tempgrid.f 
gfortran -c %COMPILE_FLAGS% windgrid.f 
gfortran -c %COMPILE_FLAGS% calc1.f    
gfortran -c %COMPILE_FLAGS% calc2.f    
gfortran -c %COMPILE_FLAGS% prise.f    
gfortran -c %COMPILE_FLAGS% prime.f    
gfortran -c %COMPILE_FLAGS% sigmas.f   
gfortran -c %COMPILE_FLAGS% pitarea.f  
gfortran -c %COMPILE_FLAGS% uninam.f 
gfortran -c %COMPILE_FLAGS% output.f   
gfortran -c %COMPILE_FLAGS% evset.f    
gfortran -c %COMPILE_FLAGS% evcalc.f   
gfortran -c %COMPILE_FLAGS% evoutput.f
gfortran -c %COMPILE_FLAGS% rline.f 
gfortran -c %COMPILE_FLAGS% bline.f 

gfortran -o aermod.exe %LINK_FLAGS% MODULES.o GRSM.o AERMOD.o SETUP.o COSET.o SOSET.o RESET.o MESET.o OUSET.o INPSUM.o METEXT.o IBLVAL.o SIGGRID.o TEMPGRID.o WINDGRID.o CALC1.o CALC2.o PRISE.o PRIME.o SIGMAS.o PITAREA.o UNINAM.o OUTPUT.o EVSET.o EVCALC.o EVOUTPUT.o RLINE.o BLINE.o

del *.o
del *.mod