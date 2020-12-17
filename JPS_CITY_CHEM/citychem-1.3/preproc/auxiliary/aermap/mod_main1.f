       MODULE MAIN1
C***********************************************************************
C     AERMAP.INC
C     AERMAP Data - Array Names, Array Limits, Named Common Blocks
C                   Global Data for All Modules
C
C     MODIFIED - 4/24/2003 (03107)
C***********************************************************************

      IMPLICIT NONE

C***********************************************************************

C***********************************************************************
C     User Specified Model Parameters for Array Dimensions
C***********************************************************************

C**   Set record length for direct access elevation files, LREC_DIR
C**   LREC_DIR = 8 for 8-byte (double precision) elevation data
      INTEGER, PARAMETER :: LREC_DIR = 8

C**   Nudge factor, in arc-seconds, for nudging DEM corner coordinates 
C**   inward toward the file so that converted coordinates which may 
C**   be slightly over a UTM zone boundary due to precision or rounding
C**   will be assigned to the expected zone.  
C**   Nudge distance of 0.03 arc-seconds is approximately 1 meter.
      DOUBLE PRECISION, PARAMETER  :: nudge = 0.03D0

C**   The following array limits are allocated dynamically at runtime, based
C**   on the input data:
      INTEGER :: MAXPRF
      INTEGER :: NREC, IXM, IYM, NNET, NDEM, NSRC, NARC
    
C**   MAXPRF = Max Number of Profiles per data file
C**   NREC   = Max Number of Receptors
C**   IXM    = Max Number of X-coord (Distance) Values Per Receptor Network
C**   IYM    = Max Number of Y-coord (Direction) Values Per Receptor Network
C**   NNET   = Max Number of Cartesian and/or Polar Receptor Networks
C**   NDEM   = Max Number of Digital Elevation MODEL(DEM) Terrain Data Files
C**   NSRC   = Max Number of Sources
C**   NARC   = Max Number of Receptor Groupings ('ARCs') for EVALCART Keyword


C***********************************************************************
C     Model Constants Initialized in sub.VARINI
C***********************************************************************

      DOUBLE PRECISION :: PI, DTORAD, RTODEG

C**   PI     = Number of radians in 180 degrees; assigned in sub.VARINI
C**            as 4.0D0*DATAN(1.0D0)
C**   DTORAD = Degrees to Radians Conversion Factor; assigned in sub.VARINI
C**            as PI/180.0D0
C**   RTODEG = Radians to Degrees Conversion Factor; assigned in sub.VARINI
C**            as 180.0D0/PI

C***********************************************************************
C     Programmer Specified Model Parameters
C***********************************************************************

      INTEGER, PARAMETER :: IFMAX=40, IKN=28, ISTRG=512, ILEN_FLD=200,
     &                      IERRN=121


C**   IFMAX    = Max Number of Fields Per Runstream Record
C**   IKN      = Number of Keywords
C**   ISTRG    = Length of Runstream Image Character String
C**   ILEN_FLD = Length of Runstream Input Fields.  Also used to
C**              specify length of input filenames and formats.
C**   IERRN    = Number of Error/Warning/Informational Messages


C***********************************************************************
C     Common Block for Input/Output File Units (Initialized in BLOCK DATA)
C***********************************************************************
      INTEGER INUNIT, IOUNIT, IRUNIT, ISUNIT, IERUNT, IERWRT, IDBGUNT,
     &                RELVK, RECK, RECD, SELVK, SRCK, SRCD, MAPK, DEMK,
     &                DOMK, HDBG, INCUNT

      INTEGER, ALLOCATABLE :: IDMUNT(:), IDRUNT(:), IDXUNT(:)
      INTEGER, ALLOCATABLE :: ITiffDbg_Unt(:)


crfl 9/5/02 Add type & common for input/output used in NADCON21 as part of a 
crfl        change to be sure unit numbers are assigned before the corresponding
crfl        write statements are executed.
      INTEGER, PARAMETER :: MXAREA = 8
      INTEGER LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA(2*MXAREA)
      INTEGER ILEN_PATH

crfl  Change by Russell F. Lee, under contract to Lakes Environmental Software
crfl  End of change

C**   INUNIT = Input Runstream File Unit (Initialized to 5)
C**   IOUNIT = Main Printed Output File Unit (Initialized to 6)
C**   IRUNIT = Receptor Data Output File Unit (Initialized to 8)
C**   ISUNIT = Source Data Output File Unit (Initialized to 9)
C**   IERUNT = Temporary Error/Message File Unit (Initialized to 10)
C**   IERWRT = Permanent Detailed Error/Message File Unit (Init. to 11)
C**   IDBGUNT = Read debug switches Unit (Init. to 12)
C**   INCUNT = INCLUDED File Unit (Initialized to 14)
C**   IDMUNT = DEM DATA Input File Unit (Based from unit 100)
C**   IDRUNT = DEM Direct Access File Unit to Elevation DATA (Based from unit 300)
C**   IDXUNT = DEM Index File Unit (Based from unit 500)

C***********************************************************************
C     This is The Global Variable Definition Block for Runstream Data
C***********************************************************************

      LOGICAL BLINE, INFLD, MARK, ECHO

      CHARACTER PATH*2, PPATH*2, KEYWRD*8, PKEYWD*8, KEYWD*8, KTYPE*5,
     &          RUNST*1

      CHARACTER DUMMY*8
      
      CHARACTER (LEN=ILEN_FLD) :: FIELD, INPFIL, OUTFIL, DEMFIL, INCFIL,
     &                            DIRFIL, IDXFIL, MAPPARAMS_FILE, 
     &                            RECFIL, SRCFIL,
     &                            MAPDET_FILE, DOMDET_FILE, CALCHC_FILE,
     &                           RECNDEM_FILE, RECELV_FILE, RECDET_FILE,
     &                           SRCNDEM_FILE, SRCELV_FILE, SRCDET_FILE,
     &                           TiffDbgFil, NADGRID_PATH
     
      CHARACTER (LEN=ISTRG)    :: RUNST1
      
C*    Declare character variable for Index File Format, IDXFRM      
      CHARACTER (LEN=ILEN_FLD) :: IDXFRM

      INTEGER ::     LOCB(IFMAX), LOCE(IFMAX), IFC, IDC1, IPNUM, IPPNUM
      DIMENSION      FIELD(IFMAX), KEYWD(IKN), RUNST(ISTRG)
      
      ALLOCATABLE :: DEMFIL(:), DIRFIL(:), IDXFIL(:), TiffDbgFil(:)

      REAL FNUM
      DOUBLE PRECISION DNUM


C***********************************************************************
C     This is The Global Variable Definition Block for Error Handling
C***********************************************************************

      INTEGER MITL, IFSTAT, INDCHK, MAXNUM
      INTEGER IDEM
      INTEGER NRGAP, NSGAP, NRGAP2, NSGAP2, NRGAP3, NSGAP3
      INTEGER NRGAP_IN, NSGAP_IN, NRMISS, NSMISS, NRSUBS, NSSUBS
      INTEGER NRFILLED, NSFILLED
      INTEGER ISC
      LOGICAL HILLDBG, RECDBG, SRCDBG

      LOGICAL EOF
      LOGICAL FATAL, ISTART, IFINIS, RECERR, ERRLST, EOF1, RUNERR
      LOGICAL DEMERR, NEDERR
      LOGICAL GAPSFOUND, GAPSFOUND_IN, MISSFOUND, FILLGAPS
      LOGICAL NADPATH
      LOGICAL L_NeedNADCON
      
      LOGICAL, ALLOCATABLE :: LRec_FILLED(:,:), LSrc_FILLED(:,:)

      CHARACTER ERRMSG*50, ERRCOD*3, VERSN*5
      CHARACTER RUNDAT*8, RUNTIM*8
      
      CHARACTER (LEN=ILEN_FLD) :: MSGFIL

      DIMENSION ERRMSG(IERRN), ERRCOD(IERRN)

      INTEGER ILINE, IERROR, IFTL, IWRN, INFO, ICLM, IMSG, NFATAL, NWARN
      INTEGER ICSTAT(20), ISSTAT(20), IRSTAT(20), IOSTAT(20)
      INTEGER EXPLIM, INCSET, IXYSET, IEVSET, IHLSET, IFGSET
      INTEGER ILSAVE


C***********************************************************************
C     This is The Global Variable Definition Block for COntrol Pathway
C***********************************************************************

C*****  NOTE: RURAL and URBAN have been returned to COMMON /CNTLOG/

      LOGICAL RUN, EXTRACT, FLGPOL, DOMFIX
      
      CHARACTER (LEN=ILEN_FLD) :: TITLE1, TITLE2,
     &          DOMCARD, ANCHCRD, HGTCARD,
     &          DOMADJ     
      
      CHARACTER REELEV*6,
     &          DOMTYP*3


C***********************************************************************
C     This is The Global Variable Definition Block for REceptor Pathway
C***********************************************************************

      LOGICAL ISTA, IEND, NEWID, NODATA
      
      LOGICAL, ALLOCATABLE :: L_DEMCHECK(:), L_UserElevUnits(:),
     &                        L_TiffDebug(:), 
     &                        L_NEDSkip(:)

      CHARACTER NETID*8, NETIDT*8, PNETID*8, NTID*8, NTTYP*8,
     &          RECTYP*2, PXSOID*12, PESOID*12, TYPDEM*4, ARCID*8,
     &          RECNAM*8, TYPDAT*4

      ALLOCATABLE :: NETID(:), RECTYP(:), NTID(:),
     &               NTTYP(:), ARCID(:), RECNAM(:)
     
     
      DOUBLE PRECISION XINT, YINT, XDELTA, YDELTA
      DOUBLE PRECISION XNODE, YNODE, XDMIN, XDMAX, YDMIN, YDMAX
      DOUBLE PRECISION XATERR, YATERR, XAUSER, YAUSER
      
      DOUBLE PRECISION, ALLOCATABLE :: AXR(:), AYR(:)
      DOUBLE PRECISION, ALLOCATABLE :: AXS(:), AYS(:)
      DOUBLE PRECISION, ALLOCATABLE :: RLAT(:), RLON(:)
      DOUBLE PRECISION, ALLOCATABLE :: SLAT(:), SLON(:)
      DOUBLE PRECISION, ALLOCATABLE :: XRECU(:), YRECU(:)
      DOUBLE PRECISION, ALLOCATABLE :: XCOORD(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: YCOORD(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: XSRCU(:), YSRCU(:)
      DOUBLE PRECISION, ALLOCATABLE :: XORIG(:), YORIG(:)
      DOUBLE PRECISION, ALLOCATABLE :: XRDIFS(:), YRDIFS(:)
      DOUBLE PRECISION, ALLOCATABLE :: XRDIFM(:), YRDIFM(:)
      DOUBLE PRECISION, ALLOCATABLE :: XSDIFS(:), YSDIFS(:)
      DOUBLE PRECISION, ALLOCATABLE :: XSDIFM(:), YSDIFM(:)
      DOUBLE PRECISION, ALLOCATABLE :: DATUMSHFT(:), DATUMSHFTS(:)

      DOUBLE PRECISION, ALLOCATABLE :: XBASE(:), YBASE(:)
      
      DOUBLE PRECISION, ALLOCATABLE :: DXM(:), DYM(:)
     
      DOUBLE PRECISION XPT27, YPT27, XPT83, YPT83, AMAG
      DOUBLE PRECISION DLOS, DLOM, DLAS, DLAM, EPS
      DOUBLE PRECISION XDMNDIFS, YDMNDIFS, XDMNDIFM, YDMNDIFM
      DOUBLE PRECISION XDMXDIFS, YDMXDIFS, XDMXDIFM, YDMXDIFM
      DOUBLE PRECISION XDMNSHFT, YDMNSHFT, XDMXSHFT, YDMXSHFT
      DOUBLE PRECISION RDIST, DOMLL(2,4)
      DOUBLE PRECISION X(4), Y(4)
      
      DOUBLE PRECISION, ALLOCATABLE :: HC(:), AZS(:), AZELEV(:)
      DOUBLE PRECISION, ALLOCATABLE :: AZFLAG(:), AZHILL(:)
      DOUBLE PRECISION, ALLOCATABLE :: ZETMP1(:), ZETMP2(:)
      DOUBLE PRECISION, ALLOCATABLE :: ZFTMP1(:), ZFTMP2(:)

      DOUBLE PRECISION, ALLOCATABLE :: DCI(:), UserDCI(:)

      INTEGER, ALLOCATABLE :: NUMPRF(:), IZOND(:)
      
      INTEGER, ALLOCATABLE :: IZONR(:), IZONS(:), IREF(:)
      INTEGER, ALLOCATABLE :: IRIN(:,:), ISIN(:,:)
      INTEGER, ALLOCATABLE :: JDM(:), JDMS(:)
      INTEGER, ALLOCATABLE :: NDXARC(:)
      INTEGER, ALLOCATABLE :: NUMXPT(:), NUMYPT(:)
      INTEGER, ALLOCATABLE :: NETSTA(:), NETEND(:)
      
      INTEGER, ALLOCATABLE :: NODES(:), IZONP(:)
      
      INTEGER NX(2), NY1(2), NY2(2)
      INTEGER ZONMIN, ZONMAX, ZATERR, NPROFS
      INTEGER ZONMIN_SHFT, ZONMAX_SHFT
      INTEGER IZDUM, IFRST
      INTEGER ICOUNT, JCOUNT, IZE, IZH, IZF, IRZE, IRZH, IRZF,
     &        IRXR, IRYR, IRHZ, INNET,  
     &        IRLAS, NADA, JREC(4), IRXT, IRYT

      INTEGER NUMDEM, NUMREC, NUMARC, NUMSRC


C***********************************************************************
C     This is The Global Variable Definition Block for Receptor Network Origin
C***********************************************************************

      CHARACTER SRCID*12, SRCTYP*8, SOELEV*6, PSOID*12

      ALLOCATABLE :: SRCID(:), SRCTYP(:)

C***********************************************************************
C     This pertains to the DEM maps
C***********************************************************************
      CHARACTER*1  FILR2, PROCODE, INSPF
      CHARACTER*3  SECTNL
      CHARACTER*4  MCTR
      CHARACTER*11 Chr_UserElevUnits
      CHARACTER*6  LVLN
      CHARACTER*5  CDLVL
      CHARACTER*55 FILR1
      CHARACTER    DEMTYP*4
      CHARACTER*11 CUNITN
      
      CHARACTER (LEN=40) :: FLN, MAPNAME, MAPN, FREEF, NADN, PLAN
      
      DIMENSION    NADN(0:7), PLAN(0:3), LVLN(0:2), CDLVL(5),
     &             CUNITN(0:3)

      ALLOCATABLE :: FLN(:)
            
      ALLOCATABLE :: DEMTYP(:), MAPNAME(:), MAPN(:)
      ALLOCATABLE :: MCTR(:), INSPF(:)
      ALLOCATABLE :: FILR1(:), FILR2(:), PROCODE(:)
      ALLOCATABLE :: SECTNL(:), FREEF(:)
      ALLOCATABLE :: Chr_UserElevUnits(:)
      
      INTEGER, ALLOCATABLE :: FT(:), ADJMAP(:,:)
      INTEGER, ALLOCATABLE :: MAPARRAY(:,:)
      INTEGER, ALLOCATABLE :: DEMLVL(:), ELEVPAT(:), IPLAN(:)
      INTEGER, ALLOCATABLE :: IZO(:), ACCUC(:), LPINT(:)
      INTEGER, ALLOCATABLE :: CUNIT(:), ELUNIT(:), SIDZ(:)
      INTEGER, ALLOCATABLE :: UserElevUnits(:)
      INTEGER, ALLOCATABLE :: NROW(:), NPROF(:), LPRIM(:)
      INTEGER, ALLOCATABLE :: SPRIM(:), SPINT(:), DDATE(:)
      INTEGER, ALLOCATABLE :: DVALD(:), SUSF(:), VDAT(:)
      INTEGER, ALLOCATABLE :: DINSP(:)
      INTEGER, ALLOCATABLE :: EDITN(:), PVOID(:), NADD(:)
      
      INTEGER, ALLOCATABLE :: ZDEM(:)
      
      DOUBLE PRECISION, ALLOCATABLE :: SWE_MTRS(:), SWN_MTRS(:)
      DOUBLE PRECISION, ALLOCATABLE :: NWE_MTRS(:), NWN_MTRS(:)
      DOUBLE PRECISION, ALLOCATABLE :: NEE_MTRS(:), NEN_MTRS(:)
      DOUBLE PRECISION, ALLOCATABLE :: SEE_MTRS(:), SEN_MTRS(:)
      DOUBLE PRECISION, ALLOCATABLE :: SWLAT_ARCS(:),SWLON_ARCS(:)
      DOUBLE PRECISION, ALLOCATABLE :: NWLAT_ARCS(:),NWLON_ARCS(:)
      DOUBLE PRECISION, ALLOCATABLE :: NELAT_ARCS(:),NELON_ARCS(:)
      DOUBLE PRECISION, ALLOCATABLE :: SELAT_ARCS(:),SELON_ARCS(:)
      DOUBLE PRECISION, ALLOCATABLE :: SWLAT_DEGS(:),SWLON_DEGS(:)
      DOUBLE PRECISION, ALLOCATABLE :: NWLAT_DEGS(:),NWLON_DEGS(:)
      DOUBLE PRECISION, ALLOCATABLE :: NELAT_DEGS(:),NELON_DEGS(:)
      DOUBLE PRECISION, ALLOCATABLE :: SELAT_DEGS(:),SELON_DEGS(:)
      
      INTEGER, ALLOCATABLE          :: SW_UTMZ(:), SE_UTMZ(:)
      INTEGER, ALLOCATABLE          :: NW_UTMZ(:), NE_UTMZ(:)
      
      DOUBLE PRECISION, ALLOCATABLE :: ELEVMAX(:)
      DOUBLE PRECISION, ALLOCATABLE :: DLTN(:), DLGE(:)
      DOUBLE PRECISION, ALLOCATABLE :: MPROJ(:,:), DMCNR(:,:,:)
      DOUBLE PRECISION, ALLOCATABLE :: ELEVMN(:), ELEVMX(:)
      DOUBLE PRECISION, ALLOCATABLE :: CNTRC(:)
      
      DOUBLE PRECISION, ALLOCATABLE :: LOCEL(:)
      DOUBLE PRECISION, ALLOCATABLE :: MINEL(:), MAXEL(:)


      LOGICAL GOTDOMFLG ! Flag to indicate if user specified domain


C***********************************************************************
C     Initialize Model Version Number, VERSN (Year, Julian Day), as a
C     Character Variable
C***********************************************************************

      DATA VERSN /'11103'/           ! Version date: April 13, 2011


C***********************************************************************
C*    Input/Output File Units and Input/Output File Names
C***********************************************************************

      DATA INUNIT /5/, IOUNIT /7/, IRUNIT /8/, ISUNIT /9/, IERUNT /10/,
     &     IERWRT /4/, IDBGUNT /40/, INCUNT /14/
     
C*    Debug file units
      DATA RELVK /51/, RECK /52/, RECD /53/, SELVK /61/, SRCK /62/,
     &     SRCD  /63/, MAPK /71/, DEMK /72/, DOMK  /73/, HDBG /81/

crfl 9/5/02 Add I/O file units used in NADCON21 so that they can be 
crfl          opened before NADCON is called
C                     LUOOUT ! Information is written to AERMAP.OUT
      DATA LUIN /35/, LUOUT / 7/, NOUT /31/, NIN /32/, NAPAR /33/
crfl  Note: LUOUT is intentionally given the value of IOUNIT (i.e., 7) so that
crfl  the grid file information can all be output to AERMAP.OUT.
crfl  Change by Russell F. Lee, under contract to Lakes Environmental Software
crfl  End of change

C**** Define Index File Format, IDXFRM
C     Read parameters are: XBASE, YBASE, NUMNODES, UTMZONE, MAXELEV
      DATA IDXFRM /'(2(D24.15,1X),I6,1X,I3,F15.5)'/
      
C***********************************************************************
C*    Initialize Keyword Array
C***********************************************************************
      INTEGER II
      DATA (KEYWD(II),II=1,IKN) 
     &        /'STARTING','FINISHED','TITLEONE',
     &         'TITLETWO','ELEVUNIT','RUNORNOT',
     &         'DATATYPE','DATAFILE','DEBUGOPT',
     &         'DOMAINXY','DOMAINLL','ANCHORXY',
     &         'LOCATION','TERRHGTS','FLAGPOLE',
     &         'GRIDCART','GRIDPOLR','DISCCART',
     &         'DISCPOLR','EVALCART','RECEPTOR',
     &         'SOURCLOC','DEBUGHIL','DEBUGREC',
     &         'DEBUGSRC','INCLUDED','SRCPARAM',
     &         'NADGRIDS'/


C***********************************************************************
C*    Initialize Error Code and Message Arrays
C***********************************************************************


C*    Now assign individual error codes and messages

      DATA ERRCOD(  1) /'100'/,
     &ERRMSG(  1) /'Invalid Pathway Specified. The Troubled Pathway is'/
      DATA ERRCOD(  2) /'105'/,
     &ERRMSG(  2) /'Invalid Keyword Specified. The Troubled Keyword is'/
      DATA ERRCOD(  3) /'110'/,
     &ERRMSG(  3) /'Keyword is Not Valid for This Pathway.  Keyword is'/
      DATA ERRCOD(  4) /'112'/,
     &ERRMSG(  4) /'SRCPARAM keyword is not valid for AERMAP.  Keyword'/
      DATA ERRCOD(  5) /'115'/,
     &ERRMSG(  5) /'STARTING or FINISHED Out of Sequence:  Pathway =  '/
      DATA ERRCOD(  6) /'120'/,
     &ERRMSG(  6) /'Pathway is Out of Sequence:  Pathway =            '/
      DATA ERRCOD(  7) /'125'/,
     &ERRMSG(  7) /'Missing FINISHED-Runstream File Incomplete: ISTAT='/
      DATA ERRCOD(  8) /'130'/,
     &ERRMSG(  8) /'Missing Mandatory Keyword.  The Missing Keyword is'/
      DATA ERRCOD(  9) /'135'/,
     &ERRMSG(  9) /'Duplicate Nonrepeatable Keyword Specified:Keyword='/
      DATA ERRCOD( 10) /'140'/,
     &ERRMSG( 10) /'Invalid Order of Keyword.  The Troubled Keyword is'/
      DATA ERRCOD( 11) /'150'/,
     &ERRMSG( 11) /'Keyword Not Valid Without CO DEBUGOPT Keyword     '/
      DATA ERRCOD( 12) /'152'/,
     &ERRMSG( 12) /'ELEVUNIT card must be first for this Pathway:     '/
      DATA ERRCOD( 13) /'160'/,
     &ERRMSG( 13) /'Duplicate ORIG Secondary Keyword for GRIDPOLR:    '/
      DATA ERRCOD( 14) /'170'/,
     &ERRMSG( 14) /'Invalid Secondary Keyword for Receptor Grid:      '/
      DATA ERRCOD( 15) /'175'/,
     &ERRMSG( 15) /'Missing Secondary Keyword END for Receptor Grid:  '/
      DATA ERRCOD( 16) /'180'/,
     &ERRMSG( 16) /'Conflicting Secondary Keyword for Receptor Grid:  '/
      DATA ERRCOD( 17) /'185'/,
     &ERRMSG( 17) /'Missing Receptor Keywords. No Receptors Specified.'/
      DATA ERRCOD( 18) /'190'/,
     &ERRMSG( 18) /'SOURLOC keyword specified but NO sources defined  '/
      DATA ERRCOD( 19) /'191'/,
     &ERRMSG( 19) /'No SOURLOC keyword spec with EXTRACT option, NSRC='/
     
      DATA ERRCOD( 20) /'200'/,
     &ERRMSG( 20) /'Missing Parameter(s). No Options Specified For    '/
      DATA ERRCOD( 21) /'201'/,
     &ERRMSG( 21) /'Not Enough Parameters Specified For the Keyword of'/
      DATA ERRCOD( 22) /'202'/,
     &ERRMSG( 22) /'Too Many Parameters Specified For the Keyword of  '/
      DATA ERRCOD( 23) /'203'/,
     &ERRMSG( 23) /'Invalid Parameter Specified.  Troubled Parameter: '/
      DATA ERRCOD( 24) /'205'/,
     &ERRMSG( 24) /'No Option Parameter Setting.  Forced by Default to'/
      DATA ERRCOD( 25) /'206'/,
     &ERRMSG( 25) /'SRCID string is too long, must be <= 12 characters'/
      DATA ERRCOD( 26) /'207'/,
     &ERRMSG( 26) /'No Parameters Specified. Default Values Will Used.'/
      DATA ERRCOD( 27) /'208'/,
     &ERRMSG( 27) /'Illegal Numerical Field Encountered in            '/
      DATA ERRCOD( 28) /'209'/,
     &ERRMSG( 28) /'Negative Value Appears For Non-negative Variable. '/
      DATA ERRCOD( 29) /'210'/,
     &ERRMSG( 29) /'Filename specified is too long. Maximum length =  '/
      DATA ERRCOD( 30) /'212'/,
     &ERRMSG( 30) /'END Encountered Without (X,Y) Points Properly Set '/
      DATA ERRCOD( 31) /'   '/,
     &ERRMSG( 31) /'                                                  '/
      DATA ERRCOD( 32) /'214'/,
     &ERRMSG( 32) /'ELEV Input Inconsistent With Option: Defaults Used'/
      DATA ERRCOD( 33) /'215'/,
     &ERRMSG( 33) /'FLAG Input Inconsistent With Option: Input Ignored'/
      DATA ERRCOD( 34) /'216'/,
     &ERRMSG( 34) /'FLAG Input Inconsistent With Option: Defaults Used'/
      DATA ERRCOD( 35) /'217'/,
     &ERRMSG( 35) /'Number of Sources Exceeds NSRC Parameter          '/
      DATA ERRCOD( 36) /'218'/,
     &ERRMSG( 36) /'Number of (X,Y) Points Not Match With Number Of   '/
      DATA ERRCOD( 37) /'219'/,
     &ERRMSG( 37) /'Number Of Receptors Specified Exceeds Max:  NREC ='/
      DATA ERRCOD( 38) /'220'/,
     &ERRMSG( 38) /'Missing Origin (Use Default = 0,0) In GRIDPOLR    '/
      DATA ERRCOD( 39) /'221'/,
     &ERRMSG( 39) /'Missing Distance Setting in Polar Network         '/
      DATA ERRCOD( 40) /'222'/,
     &ERRMSG( 40) /'Missing Direction Setting in Polar Network        '/
      DATA ERRCOD( 41) /'223'/,
     &ERRMSG( 41) /'Missing Elevation or Flagpole Field in            '/
      DATA ERRCOD( 42) /'224'/,
     &ERRMSG( 42) /'Number of Receptor Networks Exceeds Max:  NNET=   '/
      DATA ERRCOD( 43) /'225'/,
     &ERRMSG( 43) /'Number of X-Coords Specified Exceeds Max:  IXM=   '/
      DATA ERRCOD( 44) /'226'/,
     &ERRMSG( 44) /'Number of Y-Coords Specified Exceeds Max:  IYM=   '/
      DATA ERRCOD( 45) /'227'/,
     &ERRMSG( 45) /'No Receptors Were Defined on the RE Pathway.      '/
      DATA ERRCOD( 46) /'228'/,
     &ERRMSG( 46) /'Default(s) Used for Missing Parameters on Keyword '/
      DATA ERRCOD( 47) /'229'/,
     &ERRMSG( 47) /'Too Many Parameters - Inputs Ignored on Keyword   '/
      DATA ERRCOD( 48) /'232'/,
     &ERRMSG( 48) /'Domain/Data spans more than two UTM zones;  Range='/
      DATA ERRCOD( 49) /'233'/,
     &ERRMSG( 49) /'DOMAINLL/XY required for data that spans > two UTM'/
      DATA ERRCOD( 50) /'234'/,
     &ERRMSG( 50) /'DEM/NED files span more than two UTM zones; Range='/
      DATA ERRCOD( 51) /'242'/,
     &ERRMSG( 51) /'Anchor UTM zone is out of range.  Zone =          '/
      DATA ERRCOD( 52) /'244'/,
     &ERRMSG( 52) /'Anchor Zone-DOMAINLL Mismatch. Longitude adjusted:'/
      DATA ERRCOD( 53) /'246'/,
     &ERRMSG( 53) /'Domain spans 180E-180W, DOMAINLL/XY not supported '/
      DATA ERRCOD( 54) /'250'/,
     &ERRMSG( 54) /'Duplicate XPNT-DIST or YPNT-DIR Specified for GRID'/
      DATA ERRCOD( 55) /'252'/,
     &ERRMSG( 55) /'Duplicate Receptor Network ID Specified.  NETID = '/
      DATA ERRCOD( 56) /'254'/,
     &ERRMSG( 56) /'Number of Receptor ARCs Exceeds Max:     NARC =   '/
     
      DATA ERRCOD( 57) /'300'/,
     &ERRMSG( 57) /'Receptor Location is NOT Inside the Domain. IREC= '/
      DATA ERRCOD( 58) /'305'/,
     &ERRMSG( 58) /'Source Location is NOT Inside the Domain.   SRCID '/
      DATA ERRCOD( 59) /'310'/,
     &ERRMSG( 59) /'Domain Coordinate is NOT Inside a DEM File. Pt.=  '/
      DATA ERRCOD( 60) /'325'/,
     &ERRMSG( 60) /'DEM file order; Enter higher res file 1st; File:  '/
      DATA ERRCOD( 61) /'326'/,
     &ERRMSG( 61) /'REC assigned to lower resolution file first, REC# '/
      DATA ERRCOD( 62) /'327'/,
     &ERRMSG( 62) /'SRC assigned to lower resolution file first, SRCID'/
      DATA ERRCOD( 63) /'330'/,
     &ERRMSG( 63) /'Receptor NOT Inside DEM File - First Pass. IREC=  '/
      DATA ERRCOD( 64) /'331'/,
     &ERRMSG( 64) /'Gap Receptor NOT Inside DEM File 2nd Pass. IREC=  '/
      DATA ERRCOD( 65) /'332'/,
     &ERRMSG( 65) /'Receptor Located in Multiple Overlapping DEMs:    '/
      DATA ERRCOD( 66) /'333'/,
     &ERRMSG( 66) /'Source Located in Multiple Overlapping DEMs: SRCID'/
      DATA ERRCOD( 67) /'335'/,
     &ERRMSG( 67) /'Source NOT Inside a DEM File - First Pass. SRCID  '/
      DATA ERRCOD( 68) /'336'/,
     &ERRMSG( 68) /'Gap Source NOT Inside DEM File 2nd Pass.  SRCID   '/
      DATA ERRCOD( 69) /'340'/,
     &ERRMSG( 69) /'No Terrain File Adjacent to this one. IDEM=       '/
      DATA ERRCOD( 70) /'350'/,
     &ERRMSG( 70) /'Max Number of Profiles or Nodes Exceeded: IDEM=   '/
      DATA ERRCOD( 71) /'360'/,
     &ERRMSG( 71) /'Latitude Specified on DOMAINLL Exceeds 66 Degrees '/
      DATA ERRCOD( 72) /'362'/,
     &ERRMSG( 72) /'Invalid DEM File Type: May Be Binary File         '/
      DATA ERRCOD( 73) /'365'/,
     &ERRMSG( 73) /'NAD Conversion Grid Files (*.las; *.los) Not Found'/
      DATA ERRCOD( 74) /'375'/,
     &ERRMSG( 74) /'Specified SRCID Has Not Been Defined Yet: KEYWORD='/
      DATA ERRCOD( 75) /'377'/,
     &ERRMSG( 75) /'Duplicate LOCATION Card Specified for Source      '/
      DATA ERRCOD( 76) /'380'/,
     &ERRMSG( 76) /'NED file is outside domain and skipped; NED file: '/
     
      DATA ERRCOD( 77) /'400'/,
     &ERRMSG( 77) /'Receptor Location Outside Range of Profiles, IREC='/
      DATA ERRCOD( 78) /'402'/,
     &ERRMSG( 78) /'Receptor Elevation from NAD Gap Location,    IREC='/
      DATA ERRCOD( 79) /'403'/,
     &ERRMSG( 79) /'Receptor Elevation from Inside Gap Location, IREC='/
      DATA ERRCOD( 80) /'405'/,
     &ERRMSG( 80) /'Receptor Elevation Based on Subsequent File, IREC='/
      DATA ERRCOD( 81) /'410'/,
     &ERRMSG( 81) /'Receptor Elevation is Missing (-9999.0),     IREC='/
      DATA ERRCOD( 82) /'415'/,
     &ERRMSG( 82) /'Node Elev Miss (-9999) Used Ave of Adj Nodes:IREC='/
      DATA ERRCOD( 83) /'416'/,
     &ERRMSG( 83) /'Node Elev Miss (-9999);  No Adj Nodes Avail: IREC='/
      DATA ERRCOD( 84) /'420'/,
     &ERRMSG( 84) /'Source Location Outside Range of Profiles,   SRCID'/
      DATA ERRCOD( 85) /'422'/,
     &ERRMSG( 85) /'Source Elevation from NAD Gap Location,      SRCID'/
      DATA ERRCOD( 86) /'423'/,
     &ERRMSG( 86) /'Source Elevation from Inside Gap Location,   SRCID'/
      DATA ERRCOD( 87) /'425'/,
     &ERRMSG( 87) /'Source Elevation Based on Subsequent File,   SRCID'/
      DATA ERRCOD( 88) /'430'/,
     &ERRMSG( 88) /'Source Elevation is Missing (-9999.0),       SRCID'/
      DATA ERRCOD( 89) /'435'/,
     &ERRMSG( 89) /'Node Elev Miss (-9999) Used Ave of Adj Nodes:SRCID'/
      DATA ERRCOD( 90) /'436'/,
     &ERRMSG( 90) /'Node Elev Miss (-9999);  No Adj Nodes Avail: SRCID'/
      DATA ERRCOD( 91) /'445'/,
     &ERRMSG( 91) /'Inconsistent coord system and units, DEM file:    '/
      DATA ERRCOD( 92) /'446'/,
     &ERRMSG( 92) /'Invalid UTM file, no reference UTM zone.  File:   '/
      DATA ERRCOD( 93) /'447'/,
     &ERRMSG( 93) /'Non-zero UTM zone for geographic DEM. DEM file:   '/
      DATA ERRCOD( 94) /'448'/,
     &ERRMSG( 94) /'Unexpected coordinate system.    DEM file:        '/
      DATA ERRCOD( 95) /'450'/,
     &ERRMSG( 95) /'Parameter not found in TIFF file.  TiffTag:       '/
      DATA ERRCOD( 96) /'455'/,
     &ERRMSG( 96) /'TIFF parameter has unexpected value.  TiffTag:    '/
      DATA ERRCOD( 97) /'460'/,
     &ERRMSG( 97) /'Parameter not found in TIFF file.   GeoKey:       '/
      DATA ERRCOD( 98) /'465'/,
     &ERRMSG( 98) /'TIFF parameter has unexpected value.   GeoKey:    '/
      DATA ERRCOD( 99) /'466'/,
     &ERRMSG( 99) /'TIFF File Type Mismatch:  IPLAN and CUNIT values: '/
      DATA ERRCOD(100) /'470'/,
     &ERRMSG(100) /'Unexpected datum for NED file encountered:        '/
      DATA ERRCOD(101) /'471'/,
     &ERRMSG(101) /'Datum not found, use NAD83 as default. NED file:  '/
      DATA ERRCOD(102) /'472'/,
     &ERRMSG(102) /'Datum not found and NADA does not = 0; NED file:  '/
      DATA ERRCOD(103) /'473'/,
     &ERRMSG(103) /'Default elevation units of METERS used; NED file: '/
      DATA ERRCOD(104) /'474'/,
     &ERRMSG(104) /'User-specified elevation units used for NED file: '/
      DATA ERRCOD(105) /'475'/,
     &ERRMSG(105) /'NED file has an unexpected data structure         '/
      DATA ERRCOD(106) /'476'/,
     &ERRMSG(106) /'User-specified elev units conflict with NED file: '/
      DATA ERRCOD(107) /'477'/,
     &ERRMSG(107) /'User-spec vertical scale conflicts with NED file: '/
      DATA ERRCOD(108) /'478'/,
     &ERRMSG(108) /'Default horizontal units of METERS used; NED file:'/
      DATA ERRCOD(109) /'479'/,
     &ERRMSG(109) /'Default angular units of DEGREES used;   NED file:'/
      DATA ERRCOD(110) /'480'/,
     &ERRMSG(110) /'Warning encountered reading TIFF file. NED file:  '/
      DATA ERRCOD(111) /'481'/,
     &ERRMSG(111) /'Error encountered reading TIFF file. NED file:    '/
      DATA ERRCOD(112) /'482'/,
     &ERRMSG(112) /'Allocation error encountered reading TIFF file:   '/
      DATA ERRCOD(113) /'483'/,
     &ERRMSG(113) /'Non-0 TIFF Anchor-Z with Pxlscale-Z=0 in NED file:'/
      DATA ERRCOD(114) /'484'/,
     &ERRMSG(114) /'Data horizontal resolution is too large;   File:  '/
      DATA ERRCOD(115) /'485'/,
     &ERRMSG(115) /'Data horizontal resolution out-of-range;   File:  '/
      DATA ERRCOD(116) /'486'/,
     &ERRMSG(116) /'Invalid UTM Zone based on SW corner coord. File:  '/
     
      DATA ERRCOD(117) /'500'/,
     &ERRMSG(117) /'Fatal Error Occurs Opening the Data File          '/
      DATA ERRCOD(118) /'505'/,
     &ERRMSG(118) /'File Already in Use, Cannot be Opened:            '/
      DATA ERRCOD(119) /'510'/,
     &ERRMSG(119) /'Fatal Error Occurs During Reading of the File     '/
      DATA ERRCOD(120) /'520'/,
     &ERRMSG(120) /'Fatal Error Occurs During Writing to the File of  '/
      DATA ERRCOD(121) /'550'/,
     &ERRMSG(121) /'DEM File or Unit Conflict for Specified DEM File: '/

      DATA (PLAN(II), II=0,3)
     &        /'Geographic Coordinates  ', 
     &         'UTM Coordinates         ',
     &         'State Plane Coordinates ', 
     &         'XYZ Coordinates         '/

      DATA (NADN(II), II=0,7) 
     &        /'Undef: Assume NAD27-7.5min; WGS72-1deg',
     &         'North American Datum of 1927',
     &         'World Geodetic System of 1972',
     &         'World Geodetic System of 1984',
     &         'North American Datum of 1983',
     &         'Old Hawaiian Datum', 'Puerto Rico Datum',
     &         'North American Datum 1983 Provisional'/

      DATA (LVLN(II), II=0,2) / 'N.A. ', 'feet ', 'meters'/
      DATA (CDLVL(II), II=1,5) /'DEM-1', 'DEM-2', 'DEM-3', 'DEM-4',
     &                                                     ' NED '/
      DATA (CUNITN(II), II=0,3) / 'radians    ', 'feet       ', 
     &                            'meters     ', 'arc-seconds'/
      
     
 
C --- The following vars are specific for processing GeoTIFF files

      character(len=5), allocatable  :: tiffType(:) ! data organization ('strip' || 'tile')

      logical, allocatable :: byteSwap(:) ! flag to indicate byteswap is needed (endian mismatch)
 
      integer (kind=8), allocatable :: nCols(:), nRows(:)  ! number of rows and cols in NED GeoTIFF 
      
      integer (kind=8), allocatable :: rowsPerStrip(:)     ! number of data rows per strip
      integer (kind=8), allocatable :: tileLen(:)          ! length (height) of tile in pixels
      integer (kind=8), allocatable :: tileWid(:)          ! width of tile in pixels
      integer (kind=8), allocatable :: bytesPerSample(:)   ! number of bytes per data sample
            
      double precision, allocatable :: tiePtx(:)    ! upper left X coordinate NED GeoTIFF
      double precision, allocatable :: tiePty(:)    ! upper left Y coordinate NED GeoTIFF 
      double precision, allocatable :: tiePtz(:)    ! upper left Z coordinate NED GeoTIFF 
      double precision, allocatable :: pxlScalex(:) ! model pixel scale X-direction
      double precision, allocatable :: pxlScaley(:) ! model pixel scale Y-direction
      double precision, allocatable :: pxlScalez(:) ! model pixel scale Z-direction
      
      type arrOffsets
         integer, dimension(:), allocatable :: numbytes

      end type
      
      type(arrOffsets), allocatable   :: dataOS(:)  ! offsets (in bytes) to data strips or tiles


      END MODULE MAIN1
