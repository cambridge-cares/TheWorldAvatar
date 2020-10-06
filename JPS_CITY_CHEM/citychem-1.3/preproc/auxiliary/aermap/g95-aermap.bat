g95 -c -O3 mod_main1.f        
g95 -c -O3 mod_tifftags.f     
g95 -c -O3 aermap.f           
g95 -c -O3 sub_calchc.f       
g95 -c -O3 sub_chkadj.f       
g95 -c -O3 sub_chkext.f       
g95 -c -O3 sub_demchk.f       
g95 -c -O3 sub_nedchk.f       
g95 -c -O3 sub_cnrcnv.f       
g95 -c -O3 sub_demrec.f       
g95 -c -O3 sub_demsrc.f       
g95 -c -O3 sub_domcnv.f       
g95 -c -O3 sub_initer_dem.f   
g95 -c -O3 sub_initer_ned.f   
g95 -c -O3 sub_nadcon.f       
g95 -c -O3 sub_reccnv.f       
g95 -c -O3 sub_recelv.f       
g95 -c -O3 sub_srccnv.f       
g95 -c -O3 sub_srcelv.f       
g95 -c -O3 sub_utmgeo.f       
g95 -c -O3 sub_read_tifftags.f
           
g95 -o aermap.exe mod_main1.o mod_tifftags.o aermap.o sub_calchc.o sub_chkadj.o sub_chkext.o sub_demchk.o sub_nedchk.o sub_cnrcnv.o sub_demrec.o sub_demsrc.o sub_domcnv.o sub_initer_dem.o sub_initer_ned.o sub_nadcon.o sub_reccnv.o sub_recelv.o sub_srccnv.o sub_srcelv.o sub_utmgeo.o sub_read_tifftags.o

del *.o *.mod