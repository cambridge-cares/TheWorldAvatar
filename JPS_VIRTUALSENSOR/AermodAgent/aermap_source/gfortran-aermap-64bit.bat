rem @echo off
setlocal

set COMPILE_FLAGS=-fbounds-check -Wuninitialized -static -O2
set LINK_FLAGS=-static -O2

gfortran -m64 -c %COMPILE_FLAGS%  mod_main1.f        
gfortran -m64 -c %COMPILE_FLAGS%  mod_tifftags.f     
gfortran -m64 -c %COMPILE_FLAGS%  aermap.f           
gfortran -m64 -c %COMPILE_FLAGS%  sub_calchc.f       
gfortran -m64 -c %COMPILE_FLAGS%  sub_chkadj.f       
gfortran -m64 -c %COMPILE_FLAGS%  sub_chkext.f       
gfortran -m64 -c %COMPILE_FLAGS%  sub_demchk.f       
gfortran -m64 -c %COMPILE_FLAGS%  sub_nedchk.f       
gfortran -m64 -c %COMPILE_FLAGS%  sub_cnrcnv.f       
gfortran -m64 -c %COMPILE_FLAGS%  sub_demrec.f       
gfortran -m64 -c %COMPILE_FLAGS%  sub_demsrc.f       
gfortran -m64 -c %COMPILE_FLAGS%  sub_domcnv.f       
gfortran -m64 -c %COMPILE_FLAGS%  sub_initer_dem.f   
gfortran -m64 -c %COMPILE_FLAGS%  sub_initer_ned.f   
gfortran -m64 -c %COMPILE_FLAGS%  sub_nadcon.f       
gfortran -m64 -c %COMPILE_FLAGS%  sub_reccnv.f       
gfortran -m64 -c %COMPILE_FLAGS%  sub_recelv.f       
gfortran -m64 -c %COMPILE_FLAGS%  sub_srccnv.f       
gfortran -m64 -c %COMPILE_FLAGS%  sub_srcelv.f       
gfortran -m64 -c %COMPILE_FLAGS%  sub_utmgeo.f     
gfortran -m64 -c %COMPILE_FLAGS%  sub_read_tifftags.f
           
gfortran -m64 -o aermap.exe %LINK_FLAGS% mod_main1.o mod_tifftags.o aermap.o sub_calchc.o sub_chkadj.o sub_chkext.o sub_demchk.o sub_nedchk.o sub_cnrcnv.o sub_demrec.o sub_demsrc.o sub_domcnv.o sub_initer_dem.o sub_initer_ned.o sub_nadcon.o sub_reccnv.o sub_recelv.o sub_srccnv.o sub_srcelv.o sub_utmgeo.o sub_read_tifftags.o

del *.o *.mod