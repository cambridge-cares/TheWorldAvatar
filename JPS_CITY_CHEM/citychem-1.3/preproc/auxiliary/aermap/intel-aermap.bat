ifort mod_main1.f         /compile_only /O3 /Qipo /Qprec-div- /QaxPT /traceback                   
ifort mod_tifftags.f      /compile_only /O3 /Qipo /Qprec-div- /QaxPT /traceback                   
ifort aermap.f            /compile_only /O3 /Qipo /Qprec-div- /QaxPT /traceback                   
ifort sub_calchc.f        /compile_only /O3 /Qipo /Qprec-div- /QaxPT /traceback  /assume:byterecl 
ifort sub_chkadj.f        /compile_only /O3 /Qipo /Qprec-div- /QaxPT /traceback                   
ifort sub_chkext.f        /compile_only /O3 /Qipo /Qprec-div- /QaxPT /traceback                   
ifort sub_demchk.f        /compile_only /O3 /Qipo /Qprec-div- /QaxPT /traceback  /assume:byterecl 
ifort sub_nedchk.f        /compile_only /O3 /Qipo /Qprec-div- /QaxPT /traceback  /assume:byterecl 
ifort sub_cnrcnv.f        /compile_only /O3 /Qipo /Qprec-div- /QaxPT /traceback                   
ifort sub_demrec.f        /compile_only /O3 /Qipo /Qprec-div- /QaxPT /traceback                   
ifort sub_demsrc.f        /compile_only /O3 /Qipo /Qprec-div- /QaxPT /traceback                   
ifort sub_domcnv.f        /compile_only /O3 /Qipo /Qprec-div- /QaxPT /traceback                   
ifort sub_initer_dem.f    /compile_only /O3 /Qipo /Qprec-div- /QaxPT /traceback  /assume:byterecl 
ifort sub_initer_ned.f    /compile_only /O3 /Qipo /Qprec-div- /QaxPT /traceback  /assume:byterecl 
ifort sub_nadcon.f        /compile_only /O3 /Qipo /Qprec-div- /QaxPT /traceback  /assume:byterecl 
ifort sub_reccnv.f        /compile_only /O3 /Qipo /Qprec-div- /QaxPT /traceback                   
ifort sub_recelv.f        /compile_only /O3 /Qipo /Qprec-div- /QaxPT /traceback  /assume:byterecl 
ifort sub_srccnv.f        /compile_only /O3 /Qipo /Qprec-div- /QaxPT /traceback                   
ifort sub_srcelv.f        /compile_only /O3 /Qipo /Qprec-div- /QaxPT /traceback  /assume:byterecl 
ifort sub_utmgeo.f        /compile_only /O3 /Qipo /Qprec-div- /QaxPT /traceback                   
ifort sub_read_tifftags.f /compile_only /O3 /Qipo /Qprec-div- /QaxPT /traceback  /assume:byterecl 

ifort /exe:aermap.exe mod_main1.obj mod_tifftags.obj aermap.obj sub_calchc.obj sub_chkadj.obj sub_chkext.obj sub_demchk.obj sub_nedchk.obj sub_cnrcnv.obj sub_demrec.obj sub_demsrc.obj sub_domcnv.obj sub_initer_dem.obj sub_initer_ned.obj sub_nadcon.obj sub_reccnv.obj sub_recelv.obj sub_srccnv.obj sub_srcelv.obj sub_utmgeo.obj sub_read_tifftags.obj

del *.obj
del *.mod

