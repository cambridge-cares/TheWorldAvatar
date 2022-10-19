@REM                                                                    + + +
@echo off

setlocal

set COMPILE_FLAGS=/O2 /check:format /Qipo /Qprec-div- /QaxSSE2 /trace  /Qdiag-disable:8291
set LINK_FLAGS=/O2 /Qipo /check:format /Qprec-div- /QaxSSE2 


ifort /compile_only %COMPILE_FLAGS% modules.f  
ifort /compile_only %COMPILE_FLAGS% grsm.f  
ifort /compile_only %COMPILE_FLAGS% aermod.f   
ifort /compile_only %COMPILE_FLAGS% setup.f    
ifort /compile_only %COMPILE_FLAGS% coset.f    
ifort /compile_only %COMPILE_FLAGS% soset.f    
ifort /compile_only %COMPILE_FLAGS% reset.f    
ifort /compile_only %COMPILE_FLAGS% meset.f    
ifort /compile_only %COMPILE_FLAGS% ouset.f    
ifort /compile_only %COMPILE_FLAGS% inpsum.f   
ifort /compile_only %COMPILE_FLAGS% metext.f   
ifort /compile_only %COMPILE_FLAGS% iblval.f   
ifort /compile_only %COMPILE_FLAGS% siggrid.f  
ifort /compile_only %COMPILE_FLAGS% tempgrid.f 
ifort /compile_only %COMPILE_FLAGS% windgrid.f 
ifort /compile_only %COMPILE_FLAGS% calc1.f    
ifort /compile_only %COMPILE_FLAGS% calc2.f    
ifort /compile_only %COMPILE_FLAGS% prise.f    
ifort /compile_only %COMPILE_FLAGS% prime.f    
ifort /compile_only %COMPILE_FLAGS% sigmas.f   
ifort /compile_only %COMPILE_FLAGS% pitarea.f
ifort /compile_only %COMPILE_FLAGS% uninam.f 
ifort /compile_only %COMPILE_FLAGS% output.f   
ifort /compile_only %COMPILE_FLAGS% evset.f    
ifort /compile_only %COMPILE_FLAGS% evcalc.f   
ifort /compile_only %COMPILE_FLAGS% evoutput.f 
ifort /compile_only %COMPILE_FLAGS% rline.f 
ifort /compile_only %COMPILE_FLAGS% bline.f

ifort /exe:aermod.exe %LINK_FLAGS% MODULES.obj GRSM.obj AERMOD.obj SETUP.obj COSET.obj SOSET.obj RESET.obj MESET.obj OUSET.obj INPSUM.obj METEXT.obj IBLVAL.obj SIGGRID.obj TEMPGRID.obj WINDGRID.obj CALC1.obj CALC2.obj PRISE.obj PRIME.obj SIGMAS.obj PITAREA.obj UNINAM.obj OUTPUT.obj EVSET.obj EVCALC.obj EVOUTPUT.obj RLINE.obj bline.obj

del *.obj
del *.mod
