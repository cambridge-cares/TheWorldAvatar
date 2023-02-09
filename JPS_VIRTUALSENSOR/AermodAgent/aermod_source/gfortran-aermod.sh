#!/bin/bash
COMPILE_FLAGS="-fbounds-check -Wuninitialized -O2 -static"
LINK_FLAGS="-static -O2"

gfortran -c ${COMPILE_FLAGS} modules.f    
gfortran -c ${COMPILE_FLAGS} grsm.f 
gfortran -c ${COMPILE_FLAGS} aermod.f   
gfortran -c ${COMPILE_FLAGS} setup.f    
gfortran -c ${COMPILE_FLAGS} coset.f    
gfortran -c ${COMPILE_FLAGS} soset.f    
gfortran -c ${COMPILE_FLAGS} reset.f    
gfortran -c ${COMPILE_FLAGS} meset.f    
gfortran -c ${COMPILE_FLAGS} ouset.f    
gfortran -c ${COMPILE_FLAGS} inpsum.f   
gfortran -c ${COMPILE_FLAGS} metext.f   
gfortran -c ${COMPILE_FLAGS} iblval.f   
gfortran -c ${COMPILE_FLAGS} siggrid.f  
gfortran -c ${COMPILE_FLAGS} tempgrid.f 
gfortran -c ${COMPILE_FLAGS} windgrid.f 
gfortran -c ${COMPILE_FLAGS} calc1.f    
gfortran -c ${COMPILE_FLAGS} calc2.f    
gfortran -c ${COMPILE_FLAGS} prise.f    
gfortran -c ${COMPILE_FLAGS} prime.f    
gfortran -c ${COMPILE_FLAGS} sigmas.f   
gfortran -c ${COMPILE_FLAGS} pitarea.f  
gfortran -c ${COMPILE_FLAGS} uninam.f 
gfortran -c ${COMPILE_FLAGS} output.f   
gfortran -c ${COMPILE_FLAGS} evset.f    
gfortran -c ${COMPILE_FLAGS} evcalc.f   
gfortran -c ${COMPILE_FLAGS} evoutput.f
gfortran -c ${COMPILE_FLAGS} rline.f 
gfortran -c ${COMPILE_FLAGS} bline.f 

gfortran -o aermod ${LINK_FLAGS} modules.o grsm.o aermod.o setup.o coset.o soset.o reset.o meset.o ouset.o inpsum.o metext.o iblval.o siggrid.o tempgrid.o windgrid.o calc1.o calc2.o prise.o prime.o sigmas.o pitarea.o uninam.o output.o evset.o evcalc.o evoutput.o rline.o bline.o

rm *.o
rm *.mod