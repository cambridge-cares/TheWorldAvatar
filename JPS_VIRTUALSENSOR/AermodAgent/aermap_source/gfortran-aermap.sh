#!/bin/bash

COMPILE_FLAGS="-fbounds-check -Wuninitialized -O2 -static -ffixed-form"
LINK_FLAGS="-static -O2"
 
gfortran -c ${COMPILE_FLAGS} mod_main1.f 
gfortran -c ${COMPILE_FLAGS} aermap.f 
gfortran -c ${COMPILE_FLAGS} mod_tifftags.f   
gfortran -c ${COMPILE_FLAGS} sub_calchc.f    
gfortran -c ${COMPILE_FLAGS} sub_chkadj.f    
gfortran -c ${COMPILE_FLAGS} sub_chkext.f    
gfortran -c ${COMPILE_FLAGS} sub_cnrcnv.f    
gfortran -c ${COMPILE_FLAGS} sub_demchk.f    
gfortran -c ${COMPILE_FLAGS} sub_demrec.f    
gfortran -c ${COMPILE_FLAGS} sub_demsrc.f   
gfortran -c ${COMPILE_FLAGS} sub_domcnv.f   
gfortran -c ${COMPILE_FLAGS} sub_initer_dem.f   
gfortran -c ${COMPILE_FLAGS} sub_initer_ned.f  
gfortran -c ${COMPILE_FLAGS} sub_nadcon.f 
gfortran -c ${COMPILE_FLAGS} sub_nedchk.f 
gfortran -c ${COMPILE_FLAGS} sub_read_tifftags.f    
gfortran -c ${COMPILE_FLAGS} sub_reccnv.f    
gfortran -c ${COMPILE_FLAGS} sub_recelv.f    
gfortran -c ${COMPILE_FLAGS} sub_srccnv.f    
gfortran -c ${COMPILE_FLAGS} sub_srcelv.f   
gfortran -c ${COMPILE_FLAGS} sub_utmgeo.f  


gfortran -o aermap ${LINK_FLAGS} mod_main1.o mod_tifftags.o aermap.o sub_calchc.o sub_chkadj.o sub_chkext.o sub_cnrcnv.o sub_demchk.o sub_demrec.o sub_demsrc.o sub_domcnv.o sub_initer_dem.o sub_initer_ned.o sub_nadcon.o sub_nedchk.o sub_read_tifftags.o sub_reccnv.o sub_recelv.o sub_srccnv.o sub_srcelv.o sub_utmgeo.o

chmod 777 aermap
rm *.o
rm *.mod
