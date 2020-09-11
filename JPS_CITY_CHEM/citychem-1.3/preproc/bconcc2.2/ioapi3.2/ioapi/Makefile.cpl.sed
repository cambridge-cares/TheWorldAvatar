#.........................................................................
# VERSION "$Id: Makefile.cpl.sed 1 2017-06-10 18:05:20Z coats $"
#    EDSS/Models-3 I/O API Version 3.
#.........................................................................
# COPYRIGHT
#    (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
#    (C) 2003-2004 by Baron Advanced Meteorological Systems,
#    (C) 2005-2014 Carlie J. Coats, Jr., and
#    (C) 2014-2015 UNC Institute for the Environment
#    Distributed under the GNU Lesser PUBLIC LICENSE version 2.1
#    See file "LGPL.txt" for conditions of use.
#.........................................................................
#  Environment Variables:
#       BIN     machine/OS/compiler/mode type. Shows up as suffix
#               for "Makeinclude.$(BIN)" to determine compilation
#               flags, and in $(OBJDIR) and $(INSTALL) to determine
#               binary directories
#       INSTALL installation-directory root, used for "make install":
#               "libioapi.a" and the tool executables will be installed
#               in $(INSTDIR) = $(INSTALL)/$(BIN)
#.........................................................................
#  Directories:
#       BASEDIR serves as a root directory for the I/O API library
#               source, M3Tools source, HTML documentation, and
#               (machine-specific) object/library/executable
#               directories.
#       IODIR  is where the I/O API source "lives"
#       OBJDIR  is where the ".o" and "libioapi.a" files will be built.
#               Note that its default depends upon the machine/compiler
#               architecture type, specified by environment variable BIN
#       INSTDIR = $(INSTALL)/$(BIN) is where the "libioapi.a" files will be
#               copied--must be a user-supplied environment variable
#       FIXDIR  is the directory in which to build extended-source-line
#               fixed-source-form INCLUDE files (these files are so
#               coded as to work correctly with both f90 free-form and
#               standard (f77 and f90) fixed source forms.)
#.........................................................................
#  Special Make-targets
#       all:        OBJDIR and libioapi.a
#       clean:      remove .o's and libioapi.a from OBJDIR
#       install:    copy "libioapi.a" (and "m3tools" executables) to $(INSTDIR)
#       gtar:       GZipped tar-file of the source
#       fixed_src:  FIXDIR and extended-fixed-source INCLUDE-files
#.........................................................................
# Library Versions:
#     Environment variable "BIN" specifies library version up to
#     link- and compile-flag compatibility.  Dependecies upon machine,
#     OS, and compiler are found in file "Makeinclude.$(BIN).
#     In particular, pay attention to the notes for various versions
#     that may be built for Linux x86 with the Portland Group
#     compilers:  see comments in include MAKEINCLUDE.Linux2_x86pg
#
#     The following DEFINEFLAGS options are NOT library- nor object-compatible;
#     versions with distinct combinations of these options should be
#     built in *distinct*  $(OBJDIR)s:
#
#     Defining IOAPICPL turns on "coupling mode."
#
#     Defining IOAPI_PNCF turns on "PnetCDF/MPI distributed-file mode."
#
#     Defining IOAPI_NCF4 turns on netCDF-4 INTEGER*8 operations, and
#     requires HDF-enabled netCDF-4 libraries, instead of netCDF-3.
#
#     Defining IOAPI_NO_STDOUT suppresses WRITEs to the screen in
#     routines INIT3(), M3MSG2(), M3MESG(), and M3ABORT().
#
#     Defining IOAPI_SNOOP turns on "snoop mode" for read-operations:
#     if timestep-flag not available, sleep for SNOOPSECS3 seconds,
#     then re-try, for up to SNOOPTRY3 attempts
#
#     Defining IO_360 or IO_365 creates the 360-day or 365-day "global climate"
#     versions of the library.
#
# DEFINEFLAGS = $(ARCHFLAGS) $(PARFLAGS) \
#               -DIOAPICPL=1 -DIOAPI_NO_STDOUT=1 -DIO_360=1
#
######################################################################

.SUFFIXES: .m4 .c .F .f .f90 .F90 .mod

BASEDIR = IOAPI_BASE
INSTDIR = LIBINSTALL

IODIR  = $(BASEDIR)/ioapi

# OBJDIR = $(IODIR)/../lib
# OBJDIR = $(IODIR)/../$(BIN)
OBJDIR  = $(BASEDIR)/$(BIN)

FIXDIR  = $(IODIR)/fixed_src

# Architecture dependent stuff

MAKEINCLUDE

PVMINCLUDE

 
 DEFINEFLAGS = -DIOAPICPL=1   $(ARCHFLAGS) $(PARFLAGS)
#DEFINEFLAGS  =               $(ARCHFLAGS) $(PARFLAGS)
#DEFINEFLAGS = -DIOAPICPL=1  DIOAPI_PNCF=1 $(ARCHFLAGS) $(PARFLAGS)
#DEFINEFLAGS = -DIOAPICPL=1 -DIOAPI_NCF4=1 $(ARCHFLAGS) $(PARFLAGS)
#DEFINEFLAGS = -DIOAPICPL=1 -DIOAPI_PNCF=1 -DIOAPI_NCF4=1 $(ARCHFLAGS) $(PARFLAGS)

#VFLAG  = -DVERSION='3.2-cpl'
#VFLAG  = -DVERSION='3.2-cpl-ncf4'
#VFLAG  = -DVERSION='3.2-cpl-mpi'
#VFLAG  = -DVERSION='3.2-cpl-ncf4-mpi'
#VFLAG  = -DVERSION='3.2-nocpl'
#VFLAG  = -DVERSION='3.2-nocpl-mpi'
#VFLAG  = -DVERSION='3.2-nocpl-ncf4'
#VFLAG  = -DVERSION='3.2-nocpl-ncf4-mpi'
VFLAG   = -DVERSION='3.2-cpl'

CFLAGS = $(DEFINEFLAGS) $(COPTFLAGS) $(VFLAG)
FFLAGS = $(DEFINEFLAGS) $(FOPTFLAGS) $(OMPFLAGS) $(ARCHFLAGS) -I${IODIR}
ARFLAGS = rsv

VPATH = ${OBJDIR}

CSRC = \
bufint3.c    check3c.c    close3c.c    currstepc.c  daymonc.c    ddtvar3c.c   \
desc3c.c     dscgridc.c   dt2strc.c    envgets.c    filchk3c.c   findsc.c     \
get_endian.c getdfilec.c  getdttime.c  getefilec.c  hhmmssc.c    init3c.c     \
inqatt3c.c   interp3c.c   iobin3.c     julianc.c    locatsc.c    m3errc.c     \
m3exitc.c    m3mesgc.c    m3warnc.c    mmddyyc.c    nameval.c    nextimec.c   \
open3c.c     rdatt3c.c    read3c.c     read4dc.c    rmfile.c     sec2timec.c  \
secsdiffc.c  shut3c.c     sleep3.c     sortic.c     sortir.c     sortis.c     \
sync3c.c     systemf.c    time2secc.c  wkdayc.c     wratt3c.c    write3c.c    \
write4dc.c   xtract3c.c   iocpl.c      iocplf2c.c

fSRC = \
bilin.f       bmatvec.f     chkbuf3.f     ckdesc3.f     ckfile3.f     ckgeom.f      \
ckname.f      crtbuf3.f     currstep.f    dble2real.f   dbllist.f     dmatvec.f     \
dscgrid.f     dt2str.f      filchk3.f     find1.f       find2.f       \
find3.f       find4.f	    findc.f       findr1.f      findr2.f      \
findr3.f      findr4.f      flush3.f      gcd.f         gctp.f        \
getdble.f     getmenu.f     getnum.f      getreal.f     getstr.f      \
getyn.f	      grdchk3.f     gridops.f     hhmmss.f      index1.f      \
initblk3.f    intg2real.f   intlist.f     ioparms3.f    lambert.f     \
lblank.f      len2.f        ll2utm.f      locat1.f      locat2.f      \
locat3.f      locat4.f      locatc.f      locatr1.f     locatr2.f     \
locatr3.f     locatr4.f     lustr.f	      m3warn.f      name2fid.f    \
pcoef.f	      pgrdsum.f     pmatvec.f     poly.f        promptdfile.f \
promptffile.f promptgrid.f  promptmfile.f               rdbndary.f    \
rdbuf3.f      rdcustom.f    rdgrdded.f    readsmet.f    realist.f     \
scanint.f     setsphere.f   sec2time.f    str2dble.f    skipl.f       \
smatvec.f     splitline.f   str2int.f     str2real.f    strlist.f     \
synchtao.f    time2sec.f    trimlen.f     ungridb.f     ungridi.f     \
upcase.f      utm2ll.f      wrbndary.f    wrbuf3.f      wrcustom.f    \
wrgrdded.f    xtbuf3.f      year4.f

FSRC = \
cbarnes1.F    cbarnesN.F    check3.F      crlf.F        currec.F      \
daymon.F      ddtvar3.F     ddtvar3v.F    getdate.F     getdfile.F    \
getefile.F    getffile.F    initlog3.F    interp3.F     isdstime.F    \
jstep3.F      julian.F      junit.F       m3err.F       m3exit.F      \
m3msg2.F      mmddyy.F      nextime.F     read3.F       read4d.F      \
secsdiff.F    wkday.F       write3.F      write4d.F     yr2day.F      \
interp3v.F    intpqv.F      updtvir3.F    single_thread.F

f90SRC = \
chkfil3.f90     cktflag3.f90    crdict3.f90     crtkf.f90       \
kfindx.f90      kfopen.f90      m3abort.f90     modgctp.f90     opnlist3.f90    \
rddict3.f90     rdiddata.f90    rdsmatrx.f90    runspec.f90     sync3.f90       \
wrdict3.f90     wriddata.f90    wrsmatrx.f90    xtract3.f90

F90SRC = \
close3.F90      \
crtfil3.F90     desc3.F90       init3.F90       inqatt3.F90     kfread.F90      \
kfwrite.F90     open3.F90       opnfil3.F90     opnkf.F90       opnlog3.F90     \
pn_crtfil3.F90  pn_opnfil3.F90  pn_wrvars.F90   rdatt3.F90      rdgrnest.F90    \
rdprofil.F90    rdtflag.F90     rdvars.F90      shut3.F90       syncfid.F90     \
wratt3.F90      wrgrnest.F90    wrmpigrd.F90    wrpatch.F90     wrprofil.F90    \
wrtflag.F90     wrvars.F90

##  Module sources:

mSRC = m3utilio.f

m90SRC = modgctp.f90    modwrfio.f90    modmpasfio.f90

M90SRC = modatts3.F90   modncfio.F90    modpdata.F90

EXTS =\
ATDSC3.EXT   CONST3.EXT   FDESC3.EXT   IODECL3.EXT  NETCDF.EXT  \
NOTICE.EXT   PARMS3.EXT   STATE3.EXT

hSRC=\
attdsc3.h   fdesc3.h    iodecl3.h   parms3.h  state3.h

fix_EXT = \
${FIXDIR}/ATDSC3.EXT   ${FIXDIR}/CONST3.EXT  \
${FIXDIR}/FDESC3.EXT   ${FIXDIR}/IODECL3.EXT \
${FIXDIR}/NETCDF.EXT   ${FIXDIR}/NOTICE.EXT  \
${FIXDIR}/PARMS3.EXT   ${FIXDIR}/STATE3.EXT

LIB = libioapi.a

MOBJ = $(mSRC:.f=.o)   $(m90SRC:.f90=.o)   $(M90SRC:.F90=.o)
MODS = $(mSRC:.f=.mod) $(m90SRC:.f90=.mod) $(M90SRC:.F90=.mod)
fOBJ = $(fSRC:.f=.o) $(f90SRC:.f90=.o)
FOBJ = $(FSRC:.F=.o) $(F90SRC:.F90=.o)
COBJ = $(CSRC:.c=.o)
OBJ  = ${fOBJ} ${FOBJ} ${COBJ} ${MOBJ}


######################################################################

all: ${MODS} ${LIB} fixed_src

mod:  ${MODS}

clean:  ${OBJDIR}
	cd ${OBJDIR}; rm $(fOBJ); rm $(FOBJ); rm $(MOBJ); rm $(COBJ); rm ${LIB} ${MODS}
	cd ${SRCDIR}; rm *.o core* *.mod *.MOD

install: ${INSTDIR}
	echo "Installing in ${INSTDIR}" ; cd ${OBJDIR}; cp ${LIB} ${MODS} ${INSTDIR}

dir:
	mkdir -p ${OBJDIR}

gtar:
	cd ${BASEDIR}; make gtar

bins:
	make BIN=Linux2_x86_64
	make BIN=Linux2_x86_64sun
	make BIN=Linux2_x86_64ifort
	make BIN=Linux2_x86_64dbg
	make BIN=Linux2_x86_64sundbg
	make BIN=Linux2_x86_64ifortdbg

binclean:
	make BIN=Linux2_x86_64          clean
	make BIN=Linux2_x86_64sun       clean
	make BIN=Linux2_x86_64ifort     clean
	make BIN=Linux2_x86_64dbg       clean
	make BIN=Linux2_x86_64sundbg    clean
	make BIN=Linux2_x86_64ifortdbg  clean

bindirs:
	make BIN=Linux2_x86_64          dir
	make BIN=Linux2_x86_64sun       dir
	make BIN=Linux2_x86_64ifort     dir
	make BIN=Linux2_x86_64dbg       dir
	make BIN=Linux2_x86_64sundbg    dir
	make BIN=Linux2_x86_64ifortdbg  dir

fixed_src:  ${FIXDIR} $(fix_EXT)

nametest: ${LIB} ${OBJDIR}/libnetcdff.a
	${SRCDIR}/nm_test.csh ${OBJDIR}/${LIB} ${OBJDIR}/libnetcdff.a nf_open


#  ---------------------------  RULES:  --------------------------

%.o : %.mod        #  Disable "gmake"s obnoxious implicit Modula-2 rule !!
%.f : %.F          #  Hack for some versions of  "gmake" + "gfortran"

.c.o:  $(hSRC) ${IODIR}/Makeinclude.${BIN}
	cd ${OBJDIR}; $(CC) -c $(CFLAGS) ${IODIR}/$<

.m4.c:  $(hSRC) ${IODIR}/Makeinclude.${BIN}
	$(M4) $(M4DEFFILE) $< > $(<:.m4=.c)

.m4.o:  $(hSRC) ${IODIR}/Makeinclude.${BIN}
	$(M4) $(M4DEFFILE) $< > $(<:.m4=.c)
	cd ${OBJDIR}; $(CC) $(CFLAGS) -c ${IODIR}/$(<:.m4=.c) -o $(<:.m4=.o)
	rm -f $(<:.m4=.c)

.F.o .F90.o .F90.mod:  ${EXTS} ${IODIR}/Makeinclude.${BIN}
	cd ${OBJDIR}; $(FC) -c $(FPPFLAGS) $(FFLAGS) ${IODIR}/$<

.f.o .f.mod .f90.o .f90.mod:  ${EXTS} ${IODIR}/Makeinclude.${BIN}
	cd ${OBJDIR}; $(FC) -c $(FFLAGS) ${IODIR}/$<


#  ---------------------------  Dependencies:  --------------------------
#  multiple lines to avoid "command line too long":

${LIB}: mlib flib Flib clib

mlib: ${MOBJ}
	cd ${OBJDIR}; $(AR) $(ARFLAGS) ${LIB} ${MOBJ}

flib:  ${fOBJ}
	cd ${OBJDIR}; $(AR) $(ARFLAGS) ${LIB} ${fOBJ}

Flib:  ${FOBJ}
	cd ${OBJDIR}; $(AR) $(ARFLAGS) ${LIB} ${FOBJ}

clib: ${COBJ}
	cd ${OBJDIR}; $(AR) $(ARFLAGS) ${LIB} ${COBJ}

${OBJDIR}:
	mkdir -p ${OBJDIR}

# init3() needs the library-version:
#  gctp requires "SAVE all variables" flag;
#  crtfil3, modatts3 and modgctp USE M3UTILIO

init3.o:  ${EXTS}
	echo $(VFLAG)
	cd ${OBJDIR}; $(FC) -c $(FPPFLAGS) $(FFLAGS) $(VFLAG) ${IODIR}/init3.F90 -o $@

gctp.o: ${IODIR}/gctp.f
	cd ${OBJDIR}; $(FC) -c $(FSFLAGS) $(FFLAGS) ${IODIR}/gctp.f

m3utilio.o   m3utilio.mod  :  ${EXTS}
modatts3.o   modatts3.mod  :  m3utilio.mod modncfio.mod modpdata.mod
modgctp.o    modgctp.mod   :  m3utilio.mod
modmpasfio.o modmpasfio.mod:  m3utilio.mod modncfio.mod
modpdata.o   modpdata.mod  :  m3utilio.mod modncfio.mod
modwrfio.o   modwrfio.mod  :  m3utilio.mod modncfio.mod
modncfio.o   modncfio.mod  :  m3utilio.mod
modwrfio.o   modwrfio.mod  :  m3utilio.mod modncfio.mod

chkfil3.o     :  m3utilio.mod
ckdesc.o      :  m3utilio.mod
ckfile3.o     :  m3utilio.mod
ckgeom.o      :  m3utilio.mod
cktflag3.o    :  m3utilio.mod modncfio.mod
close3.o      :  modncfio.mod modpdata.mod
crtbuf3.o     :  m3utilio.mod
crdict3.o     :  modncfio.mod
crtfil3.o     :  m3utilio.mod modncfio.mod modatts3.mod
crtkf.o       :  m3utilio.mod modncfio.mod
ddtvar3v.o    :  m3utilio.mod
desc3.o       :  modncfio.mod modpdata.mod
getstr.o      :  m3utilio.mod
getyn.o       :  m3utilio.mod
getdate.o     :  m3utilio.mod
gridops.o     :  m3utilio.mod
init3.o       :  modncfio.mod modpdata.mod
inqatt3.o     :  modncfio.mod modpdata.mod
intppqv.o     :  m3utilio.mod
kfindx.o      :  m3utilio.mod modncfio.mod
kfopen.o      :  m3utilio.mod modncfio.mod
kfread.o      :  m3utilio.mod modncfio.mod
kfwrite.o     :  m3utilio.mod modncfio.mod
m3abort.o     :  modncfio.mod
open3.o       :  modncfio.mod modpdata.mod
opnfil3.o     :  modncfio.mod
opnkf.o       :  modncfio.mod
opnlist3.o    :  m3utilio.mod
opnlog3.o     :  m3utilio.mod modncfio.mod modpdata.mod
pn_crtfil3.o  :  m3utilio.mod modncfio.mod modpdata.mod modatts3.mod 
pn_opnfil3.o  :  m3utilio.mod modncfio.mod modpdata.mod
pn_wrvars.o   :  m3utilio.mod modncfio.mod modpdata.mod
rdatt3.o      :  modncfio.mod
rdbndary.o    :  modncfio.mod
rdcustom.o    :  modncfio.mod
rddict3.o     :  m3utilio.mod modncfio.mod
rdgrdded.o    :  modncfio.mod
rdgrnest.o    :  modncfio.mod
rdiddata.o    :  modncfio.mod
rdprofil.o    :  modncfio.mod
rdsmatrx.o    :  modncfio.mod
rdtflag.o     :  m3utilio.mod modncfio.mod
rdvars.o      :  m3utilio.mod modncfio.mod
read3.o       :  modncfio.mod
read4d.o      :  m3utilio.mod modncfio.mod
readsmet.o    :  m3utilio.mod
shut3.o       :  modncfio.mod modpdata.mod
syncfid.o     :  modncfio.mod modpdata.mod
synchtao.o    :  m3utilio.mod
updtvir3.o    :  m3utilio.mod
wratt3.o      :  modncfio.mod modpdata.mod
wrbndary.o    :  modncfio.mod
wrcustom.o    :  modncfio.mod
wrdict3.o     :  m3utilio.mod
wrgrnest.o    :  modncfio.mod
wriddata.o    :  modncfio.mod
write3.o      :  modncfio.mod
write4d.o     :  m3utilio.mod modncfio.mod
wrmpigrd.o    :  modncfio.mod modpdata.mod
wrpatch.o     :  m3utilio.mod modncfio.mod
wrprofil.o    :  modncfio.mod
wrsmatrx.o    :  modncfio.mod
wrtflag.o     :  modncfio.mod modpdata.mod
wrvars.o      :  m3utilio.mod modncfio.mod
xtbuf3.o      :  m3utilio.mod



#  "fixed-source" stuff for use with CMAQ/SMOKE "F90 132-column fixed-source"
#  non-standard source code formatting:

${FIXDIR}:
	mkdir -p ${FIXDIR}

${FIXDIR}/ATDSC3.EXT: ATDSC3.EXT
	${IODIR}/fix_src.csh ATDSC3.EXT ${FIXDIR}/ATDSC3.EXT

${FIXDIR}/CONST3.EXT: CONST3.EXT
	${IODIR}/fix_src.csh CONST3.EXT ${FIXDIR}/CONST3.EXT

${FIXDIR}/FDESC3.EXT: FDESC3.EXT
	${IODIR}/fix_src.csh FDESC3.EXT ${FIXDIR}/FDESC3.EXT

${FIXDIR}/IODECL3.EXT: IODECL3.EXT
	${IODIR}/fix_src.csh IODECL3.EXT ${FIXDIR}/IODECL3.EXT

${FIXDIR}/NETCDF.EXT: NETCDF.EXT
	${IODIR}/fix_src.csh NETCDF.EXT ${FIXDIR}/NETCDF.EXT

${FIXDIR}/NOTICE.EXT: NOTICE.EXT
	${IODIR}/fix_src.csh NOTICE.EXT ${FIXDIR}/NOTICE.EXT

${FIXDIR}/PARMS3.EXT: PARMS3.EXT
	${IODIR}/fix_src.csh PARMS3.EXT ${FIXDIR}/PARMS3.EXT

${FIXDIR}/STATE3.EXT: STATE3.EXT
	${IODIR}/fix_src.csh STATE3.EXT ${FIXDIR}/STATE3.EXT

