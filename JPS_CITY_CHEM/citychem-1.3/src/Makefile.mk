# Makefile for CityChem-EPISODE
#
# Version Episode 5.5 (May29, 2012) prepared for BB-Stand-Alone
# modified by Matthias S. Karl
# The following source files are taken from BDE_Episode:
# ./mete/cdzdt_new.for
# ./mete/mprof_new.for
# ./mete/turb_um.for
# ./mete/turb_profile.for
# ./mete/turb_ebudget.for
# ./mete/turb_qnet.for
# ./mete/sinsun.for
# ./mete/radiat.for
# ./mete/flux_2_new.for
#
# debugging: gdb episode.exe, then 'run' and 'bt'


#Compilation with gfortran
#F90 = gfortran
#Compilation with intel fortran
#F90 = ifort

F90 =  gfortran

#Compilation flags with gfortran
#flags for debugging (use for the test examples). Optimization (-O) flags cannot be used together with -ggdb.
#F90FLAGS =  -ggdb -u -C -cpp -pedantic -ffpe-trap=invalid,underflow,zero -ftrapv -fbounds-check -finit-real=nan -fbacktrace -fimplicit-none -finit-integer=n -ffree-form
#Optimized run -O2
#F90FLAGS = -O2 -cpp -fbacktrace -ffree-form -fimplicit-none -finit-integer=n -ffast-math -funroll-loops -m64 -dynamic
## How to use the debugger:
## > gdb ./citychem.exe
## gdb> run
## gdb> main_episode_2013_tapm.txt
## gdb> bt    !(backtrace after crash)
## ---
## linux 32-bit: F90FLAGS =  -O1 -g -u -C -cpp -Wall -m32

#Compilation flags with intel fortran
#F90FLAGS =  -g -cpp -FR -traceback

F90FLAGS = -O2 -cpp -fbacktrace -ffree-form -fimplicit-none -finit-integer=n -ffast-math -funroll-loops -m64 -dynamic

#NETCDF libraries and includes
#INCLUDES =  -I/usr/local/netcdf4/include
#LIBS     =  -L/usr/local/netcdf4/lib -lnetcdf -lnetcdff

INCLUDES =  -I/usr/local/netcdf4/include
LIBS     =  -L/usr/local/netcdf4/lib -lnetcdf -lnetcdff

#subdirs
ASRC   = ./area/
CONC   = ./conc/
DEPO   = ./depo/
EMEP3  = ./emep03/
EMEP10 = ./ep10/
#EMEP45 = ./emep45/
EMEP45 = ./emchem03mod/
#EMEP70 = ./emchem09mod/
EMEP70 = ./emchem09het/
GRID   = ./grid/
LSRC   = ./line/
MAIN   = ./main/
METE   = ./mete/
PHOT   = ./photo/
PSRC   = ./point/
SITE   = ./site/
STAT   = ./stat/
TIME   = ./time/
UTIL   = ./util/

RUNDIR = ../bin/
TARGET = $(RUNDIR)citychem.exe


MODS_OBJ =		$(ASRC)mod_asrc.o      $(SITE)mod_site.o    \
			$(CONC)mod_conc.o      $(UTIL)mod_util.o    \
			$(DEPO)mod_depo.o      $(EMEP3)mod_emep03.o \
			$(GRID)mod_grid.o      $(LSRC)mod_lsrc.o    \
			$(METE)mod_mete.o      $(PHOT)mod_phot.o    \
			$(PSRC)mod_psrc.o      $(STAT)mod_stat.o    \
			$(TIME)mod_time.o      $(MAIN)mod_writenc.o \
			$(MAIN)mod_readnc.o    $(MAIN)mod_main.o

UTIL_OBJ =		$(UTIL)nextun.o        $(UTIL)a4dfld.o     \
			$(UTIL)clifil.o        $(UTIL)clofil.o     \
			$(UTIL)nxtdat.o        $(UTIL)opifil.o     \
			$(UTIL)ngetarg.o       $(UTIL)getdat.o     \
			$(UTIL)getfnv.o        $(UTIL)getmgi.o     \
			$(UTIL)getrwf.o        $(UTIL)h2dfld.o     \
			$(UTIL)h3dfld.o        $(UTIL)h4dfld.o     \
			$(UTIL)indxl.o         $(UTIL)opofil.o     \
			$(UTIL)r2dfld.o        $(UTIL)r3dfld.o     \
			$(UTIL)r4dbcfld.o      $(UTIL)r4dfld.o     \
			$(UTIL)stopit.o

SITE_OBJ =		$(SITE)rsite.o      \
			$(SITE)rtopm.o         $(SITE)rsurf.o      \
			$(SITE)rrecp.o         $(SITE)ctopo.o

TIME_OBJ =		$(TIME)rtime.o      \
			$(TIME)sysdat.o        $(TIME)cdayw.o      \
			$(TIME)cdayy.o         $(TIME)incrtm.o     \
			$(TIME)calcdt.o        $(TIME)tstime.o     \
			$(TIME)incrts.o

STAT_OBJ =		$(STAT)rstat.o     $(STAT)tsstat.o      \
			$(STAT)opstat.o        $(STAT)cstatm.o      \
			$(STAT)cstatr.o        $(STAT)cstatl.o      \
			$(STAT)wstatr.o        $(STAT)wstatm.o      \
			$(STAT)wstatl.o        $(STAT)wcmavea.o     \
			$(STAT)wcravea.o       $(STAT)clstat.o

METE_OBJ =		$(METE)rmete.o     $(METE)tsmete.o      \
			$(METE)opmete.o        $(METE)rgpot.o       \
			$(METE)clmete.o        $(METE)rsdvw.o       \
			$(METE)rtemp.o         $(METE)rwind.o       \
			$(METE)raero.o         $(METE)rhmix.o       \
			$(METE)rustr.o         $(METE)rshfl.o       \
			$(METE)rlhfl.o         $(METE)rlanu.o       \
			$(METE)rmflx.o         $(METE)rtaus.o       \
			$(METE)rins_t.o        $(METE)rpot_t.o      \
			$(METE)rptstr.o        $(METE)rpvstr.o      \
			$(METE)rgtmp.o         $(METE)rwstr.o       \
                        $(METE)rtsrad.o                             \
			$(METE)rprec.o         $(METE)rrhum.o       \
			$(METE)rclou.o         $(METE)cstab.o       \
			$(METE)mprof_new.o     $(METE)diffpar.o     \
			$(METE)wsprof.o        $(METE)flux_2_new.o  \
			$(METE)mixht_new.o     $(METE)pot_tmpgr_new.o \
			$(METE)flux_1_new.o    $(METE)turb_qnet.o   \
			$(METE)sinsun.o        $(METE)radiat.o      \
			$(METE)turb_ebudget.o  $(METE)turb_profile.o \
			$(METE)turb_um.o       $(METE)cdive.o       \
			$(METE)calcw.o         $(METE)cairpar.o     \
			$(METE)caero.o         $(METE)ccunc.o       \
			$(METE)part_diff.o     $(METE)part_z0.o     \
			$(METE)pfallsp.o       $(METE)c_rb.o        \
			$(METE)cdrydepv.o      $(METE)cddepvel.o    \
			$(METE)cwdeprate.o     $(METE)ctlgr.o       \
			$(METE)cdzdt_new.o

CONC_OBJ =		$(CONC)rconc.o        $(CONC)tsconc.o       \
			$(CONC)opconc.o       $(CONC)clconc.o       \
			$(CONC)wconcm.o       $(CONC)wconcr.o       \
			$(CONC)wconcl.o

DEPO_OBJ =		$(DEPO)rdepo.o    $(DEPO)tsdepo.o

GRID_OBJ =		$(GRID)rgrid.o    $(GRID)tsgrid.o      \
			$(GRID)rbcon.o        $(GRID)ricon.o       \
			$(GRID)csubg.o                             \
			$(GRID)csubgm.o       $(GRID)csubgr.o      \
			$(GRID)csubgl.o       $(GRID)adiv.o        \
			$(GRID)v_dif.o        $(GRID)v_adv.o       \
			$(GRID)difv_cn.o      $(GRID)advh.o        \
			$(GRID)advb4m.o       $(GRID)advb4p.o      \
			$(GRID)b4m.o          $(GRID)b4p.o         \
			$(GRID)difh.o         $(GRID)dxyf.o        \
			$(GRID)gmdepo.o       $(GRID)gmradi.o      \
			$(GRID)gmphot.o

LSRC_OBJ =		$(LSRC)rlsrcfn.o  $(LSRC)tslsrc.o      \
			$(LSRC)oplsrc.o       $(LSRC)rlsrcs.o      \
			$(LSRC)clsrcs.o       $(LSRC)rlsrcv.o      \
			$(LSRC)lsgrid.o       $(LSRC)distrl.o      \
			$(LSRC)csubl.o        $(LSRC)csublr.o      \
			$(LSRC)csubll.o       $(LSRC)cllsrc.o      \
			$(LSRC)hwylne.o       $(LSRC)hwyrcx.o      \
			$(LSRC)hwysig.o       $(LSRC)dbtsig.o

PHOT_OBJ =		$(PHOT)rphot.o    $(PHOT)tsphot.o      \
			$(PHOT)photo.o        $(PHOT)cphotm.o      \
			$(PHOT)cphotr.o       $(PHOT)cphotl.o      \
			$(PHOT)cphotr03.o     $(PHOT)cphotr10.o    \
			$(EMEP45)cphotrate.o  $(EMEP70)cphotratej25.o \
			$(EMEP3)emep03.o      $(EMEP10)emep10.o    \
			$(EMEP45)emep45_new.o $(EMEP70)emep70bio.o

PSRC_OBJ = 		$(PSRC)rpsrcfn.o   $(PSRC)heff.o      \
			$(PSRC)build.o         $(PSRC)penetr.o    \
			$(PSRC)tspsrc.o        $(PSRC)oppsrc.o    \
			$(PSRC)roldp.o         $(PSRC)rpsrc.o     \
			$(PSRC)ppsrc.o         $(PSRC)cpsrc.o     \
			$(PSRC)psadif.o        $(PSRC)psgene.o    \
			$(PSRC)psgrid.o        $(PSRC)psdele.o    \
			$(PSRC)psdepo.o        $(PSRC)psradi.o    \
			$(PSRC)wnewp.o         $(PSRC)clpsrc.o    \
			$(PSRC)csubpx.o        $(PSRC)csubpr.o    \
			$(PSRC)csubp.o         $(PSRC)csubpl.o

ASRC_OBJ =		$(ASRC)rasrcfn.o   $(ASRC)tsasrc.o    \
			$(ASRC)opasrc.o        $(ASRC)rasrc.o     \
			$(ASRC)clasrc.o        $(ASRC)psrctoasrc.o\
			$(ASRC)casrc.o         $(ASRC)csubaq.o    \
                        $(ASRC)wasrc.o

MAIN_OBJ =		$(MAIN)iniepi.o    $(MAIN)init.o      \
			$(MAIN)rmain.o         $(MAIN)exiepi.o    \
			$(MAIN)runepi.o

EPI_FOR =		citychem.f90

EPI_OBJ =		citychem.o


$(TARGET) : $(MODS_OBJ) $(UTIL_OBJ) $(SITE_OBJ) $(TIME_OBJ) $(STAT_OBJ) $(METE_OBJ) $(CONC_OBJ) $(DEPO_OBJ) $(GRID_OBJ) $(LSRC_OBJ) $(PHOT_OBJ) $(PSRC_OBJ) $(ASRC_OBJ) $(MAIN_OBJ) $(EPI_OBJ)
	$(F90) $(FFLAGS) $(MODS_OBJ) $(UTIL_OBJ) $(SITE_OBJ) $(TIME_OBJ) $(STAT_OBJ) $(METE_OBJ) $(CONC_OBJ) $(DEPO_OBJ) $(GRID_OBJ) $(LSRC_OBJ) $(PHOT_OBJ) $(PSRC_OBJ) $(ASRC_OBJ) $(MAIN_OBJ) $(EPI_OBJ) $(LIBS) -o $@

#IMPLICIT ROULE FOR FORTRAN FILES

.SUFFIXES: .f90 .o

.f90.o:
	$(F90) $(F90FLAGS) $(INCLUDES) -c $< -o $@

.SUFFIXES: .F90 .o

.F90.o:
	$(F90) -c $(F90FLAGS) $(INCLUDES) $*.F90

# Explicit rules and targets

clean:
	rm -f $(TARGET) core a.out

cleanall:
	rm -f $(TARGET) core a.out
	rm -f *.o *.mod *.f90~ *.f90.1* *.f90.2*
	rm -f $(ASRC)*.o   $(ASRC)*.f90~   $(ASRC)*.f90.1*   $(ASRC)*.f90.2*
	rm -f $(CONC)*.o   $(CONC)*.f90~   $(CONC)*.f90.1*   $(CONC)*.f90.2*
	rm -f $(DEPO)*.o   $(DEPO)*.f90~   $(DEPO)*.f90.1*   $(DEPO)*.f90.2*
	rm -f $(EMEP3)*.o  $(EMEP3)*.f90~  $(EMEP3)*.f90.1*  $(EMEP3)*.f90.2*
	rm -f $(EMEP10)*.o $(EMEP10)*.f90~ $(EMEP10)*.f90.1* $(EMEP10)*.f90.2*
	rm -f $(EMEP45)*.o $(EMEP45)*.f90~ $(EMEP45)*.f90.1* $(EMEP45)*.f90.2*
	rm -f $(EMEP70)*.o $(EMEP70)*.f90~ $(EMEP70)*.f90.1* $(EMEP70)*.f90.2*
	rm -f $(GRID)*.o   $(GRID)*.f90~   $(GRID)*.f90.1*   $(GRID)*.f90.2*
	rm -f $(LSRC)*.o   $(LSRC)*.f90~   $(LSRC)*.f90.1*   $(LSRC)*.f90.2*
	rm -f $(MAIN)*.o   $(MAIN)*.f90~   $(MAIN)*.f90.1*   $(MAIN)*.f90.2*
	rm -f $(METE)*.o   $(METE)*.f90~   $(METE)*.f90.1*   $(METE)*.f90.2*
	rm -f $(PHOT)*.o   $(PHOT)*.f90~   $(PHOT)*.f90.1*   $(PHOT)*.f90.2*
	rm -f $(PSRC)*.o   $(PSRC)*.f90~   $(PSRC)*.f90.1*   $(PSRC)*.f90.2*
	rm -f $(SITE)*.o   $(SITE)*.f90~   $(SITE)*.f90.1*   $(SITE)*.f90.2*
	rm -f $(STAT)*.o   $(STAT)*.f90~   $(STAT)*.f90.1*   $(STAT)*.f90.2*
	rm -f $(TIME)*.o   $(TIME)*.f90~   $(TIME)*.f90.1*   $(TIME)*.f90.2*
	rm -f $(UTIL)*.o   $(UTIL)*.f90~   $(UTIL)*.f90.1*   $(UTIL)*.f90.2*
