
LOGICAL FUNCTION CRTFIL3( EQNAME, FID, PGNAME )  RESULT( CFLAG3 )

    !!***********************************************************************
    !! Version "$Id: crtfil3.F90 1 2017-06-10 18:05:20Z coats $"
    !! EDSS/Models-3 I/O API.
    !! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    !! (C) 2003-2011 Baron Advanced Meteorological Systems,
    !! (C) 2011 David Wong, (C) 2007-2013 Carlie J. Coats, Jr., and 
    !! (C) 2014-2015 UNC Institute for the Environment.
    !! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    !! See file "LGPL.txt" for conditions of use.
    !!.........................................................................
    !!  subroutine body starts at line 136
    !!
    !!  FUNCTION:
    !!      Create netCDF file FNAME using info stored in the FDESC3
    !!      common.  Record history,   Leave file open for update.
    !!
    !!  PRECONDITIONS REQUIRED:
    !!      File definition already put into FDESC3 common.
    !!      For file type TSERIES3:  extra attributes in ATDSC3 COMMON
    !!
    !!  SUBROUTINES AND FUNCTIONS CALLED:
    !!      CRDICT3
    !!
    !!  REVISION  HISTORY:
    !!      prototype 9/1994 by CJC for new version of OPEN3()
    !!      modified 10/1994 by CJC to support write granularity at the
    !!      varible-level for gridded, boundary, and custom files:  TFLAG
    !!      now dimensioned by <variable-number>, <time-step-number>
    !!
    !!      Modified 12/1994 by CJC:  TFLAG ~~> TFLAG to contain the
    !!      date-and-time, dimensioned by 2, <variable-number>, <time-step-number>
    !!
    !!      Modified  2/1995 by CJC to support file type SMATRX3.
    !!
    !!      Modified 10/1996 by CJC to support file type TSERIES3.
    !!      Modified 12/1996 by CJC to support file type PTRFLY3.
    !!      revised   9/1999 by CJC:  Unification of OPEN3(), KFOPEN()
    !!      revised   2/2002 by CJC:  for volatile, call nf_sync() before return
    !!      Revised   3/2002 by CJC:  OpenMP thread-safety;
    !!                                turn off NF_SHARED for Crays
    !!      Revised   4/2002 by CJC:  treatment of PROFIL3 files
    !!      Revised   7/2002 by CJC:  fixed KF initialization bug
    !!      Revised   5/2003 by CJC:  Set NF_SHARE for volatile files
    !!      Modified 10/2003 by CJC for I/O API version 3:  support for
    !!      native-binary BINFIL3 file type
    !!      Modified 12/2004 by CJC:  NVARS3D range check
    !!      Modified 11/2008 by CJC:  Environment variable IOAPI_OFFSET_64
    !!      controls whether netCDF opened with NF_64BIT_OFFSET, to support
    !!      timestep-records > 2GB
    !!      Modified 10/2009 by CJC:  flags-change for netCDF4  from
    !!      Martin Otte, US EPA -- remove NF_WRITE.
    !!      Modified 03/20010 by CJC: F9x changes for I/O API v3.1
    !!      Revised 4/2011 by David Wong, US EPA, and by CJC, to add state for
    !!      full buffered-file file descriptions.
    !!      Revised 12/2014 by CJC:  USE M3UTILIO; USE MODATTS3 extra-attributes
    !!      Modified 02/2015 by CJC for I/O API 3.2: Support for M3INT8
    !!      Modified 10/2015 by CJC:  USE MODNCFIO, NF_*() interfaces;
    !!      F90 "free" source format.  interface-structure for CMAQ and SMOKE
    !!      metadata.
    !!      Modified 12/2017 by CJC:  default TRUE for IOAPI_OFFSET_64
    !!***********************************************************************

    USE M3UTILIO
    USE MODATTS3
    USE MODNCFIO

    IMPLICIT NONE

#ifdef   _AIX
#define  FLUSH flush_
#endif

    !!...........   INCLUDES:

    INCLUDE 'STATE3.EXT'
    INCLUDE 'ATDSC3.EXT'


    !!...........   ARGUMENTS and their descriptions:

    CHARACTER*(*), INTENT(IN   ) ::   EQNAME  !  physical name of file to be created
    INTEGER      , INTENT(IN   ) ::   FID     !  index into STATE3 arrays
    CHARACTER*(*), INTENT(IN   ) ::   PGNAME  !  name of calling program


    !!...........   EXTERNAL FUNCTIONS and their descriptions:

    LOGICAL, EXTERNAL :: CRDICT3 !  create variables for dictionary files
    INTEGER, EXTERNAL :: CRTBIN3 !  create native-binary-mode files


    !!...........   PARAMETERs and their descriptions:

    INTEGER, PARAMETER :: MXCOL = 8192


    !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         ISCR
    INTEGER         FNUM            !  netCDF file ID from NF_CREATE()
    INTEGER         IERR            !  netCDF error status return
    INTEGER         ATT             !  loop counter for var. attributes
    INTEGER         VAR             !  loop counter for file variables
    INTEGER         TSIZE           !  time-step mode
    INTEGER         DSIZE           !  D/T width
    INTEGER         CDIM            !  col/level     dim #
    INTEGER         RDIM            !  row/site      dim #
    INTEGER         LDIM            !  layer         dim #
    INTEGER         DDIM            !  timestamp date-and-time dimension #
    INTEGER         EDIM            !  cell-event    dim # (KF only)
    INTEGER         TDIM            !  timestep      dim #
    INTEGER         VDIM            !  variable-flag dim #
    INTEGER         NDIMS           !  number of dims for nf_def_var()
    INTEGER         DIMS( 5 )       !  array  of dims for nf_def_var()
    INTEGER         DELS( 5 )       !  array  of dims for nf_def_var()
    INTEGER         C, R, L
    LOGICAL         EFLAG, AFLAG
    CHARACTER*80    DSCBUF          !  scratch buffer for descriptions
    CHARACTER*80    DSCBU2          !  scratch buffer for descriptions
    CHARACTER*512   NAMBUF          !  scratch buffer to upcase EQNAME in.
    CHARACTER*256   MESG
    INTEGER         KFBUF( MXCOL )  !  scratch buffer for KFCOUNT

    LOGICAL, SAVE :: FIRSTIME = .TRUE.
    LOGICAL, SAVE :: OFFSET64 = .FALSE.
    INTEGER, SAVE :: FMODE           !  netCDF file-opening mode


    !!.............................................................................
    !!   begin body of subroutine  CRTFIL3
    !!...........  Create the netCDF file, and put it into definition mode.

!$OMP   CRITICAL( WRITE3_INIT )
    IF ( FIRSTIME ) THEN

        OFFSET64 = ENVYN( 'IOAPI_OFFSET_64', 'Use NF_64BIT_OFFSET or not', .TRUE., IERR )
        IF ( IERR .GT. 0 ) THEN
            EFLAG = .TRUE.
            CALL M3MSG2( 'Bad environment vble "IOAPI_LOG_WRITE"' )
        END IF

        FMODE = IOR( NF_NOCLOBBER, NF_SHARE )
        IF ( OFFSET64 ) THEN
            FMODE = IOR( FMODE, NF_64BIT_OFFSET )
        END IF

        AFLAG = ENVYN( 'IOAPI_CFMETA', 'Generate CF-convention metadata or not?', .FALSE., IERR )
        IF ( IERR .GT. 0 ) THEN
            CALL M3MSG2( 'Bad environment vble "IOAPI_CFMETA"' )
        ELSE IF ( AFLAG ) THEN
            CALL INITCF()
        END IF

        CALL ENVSTR( 'IOAPI_CMAQMETA', 'File for CMAQ-convention metadata, "ENV", or "NONE"?', 'NONE', NAMBUF, IERR )
        CALL UPCASE( NAMBUF )
        IF ( IERR .GT. 0 ) THEN
            CALL M3MSG2( 'Bad environment vble "IOAPI_CMAQMETA"' )
        ELSE IF ( NAMBUF .EQ. 'NONE' ) THEN
            CONTINUE
        ELSE
            EFLAG = INITCMAQ()
        END IF

        CALL ENVSTR( 'IOAPI_SMOKEMETA', 'File for SMOKE-convention metadata or "NONE"?', 'NONE', NAMBUF, IERR )
        CALL UPCASE( NAMBUF )
        IF ( IERR .GT. 0 ) THEN
            CALL M3MSG2( 'Bad environment vble "IOAPI_SMOKEMETA"' )
        ELSE IF ( NAMBUF .EQ. 'NONE' ) THEN
            CONTINUE
        ELSE
            EFLAG = INITSMOKE()
        END IF

        CALL ENVSTR( 'IOAPI_TEXTMETA', 'File for unstructured-text metadata or "NONE"?', 'NONE', NAMBUF, IERR )
        CALL UPCASE( NAMBUF )
        IF ( IERR .GT. 0 ) THEN
            CALL M3MSG2( 'Bad environment vble "IOAPI_TEXTMETA"' )
        ELSE IF ( NAMBUF .EQ. 'NONE' ) THEN
            CONTINUE
        ELSE
            EFLAG = INITMTEXT()
        END IF

        FIRSTIME = .FALSE.

    END IF          !  firstime
!$OMP   END CRITICAL( WRITE3_INIT )

    IF ( NVARS3D .GT. MXVARS3 ) THEN
        NAMBUF = 'Max NVARS for this build exceeded:  file' // FLIST3( FID )
        CALL M3WARN( 'OPEN3/CRTFIL3', 0,0, NAMBUF )
        CFLAG3 = .FALSE.
        RETURN
    END IF

    EFLAG = .FALSE.

!$OMP CRITICAL( S_NC )

    !!...........  If BINIO3-mode file, do CRTBIN3 and return

    IF ( CDFID3( FID ) .EQ. BINFIL3 ) THEN
        EFLAG = ( 0 .EQ. CRTBIN3( EQNAME, FID, PGNAME ) )
        GO TO 999
    END IF

    !!...........  Else create netCDF file and proceed...

    IERR = NF_CREATE( EQNAME, FMODE, FNUM )
    IF ( IERR .NE. 0 ) THEN
        NAMBUF = EQNAME
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR,  'Error creating netCDF file' )
        CALL M3MSG2( EQNAME )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed

    CDFID3( FID ) = FNUM


    !!...........   Set attributes valid for all file types:
    !!.......   IOAPI_VERSION:  I/O API version and date

    IERR = NF_PUT_ATT_TEXT( FNUM, NF_GLOBAL, 'IOAPI_VERSION',  LEN_TRIM( DSCBUF ), VERSN3 )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF file attribute IOAPI_VERSION' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed

    !!.......   EXEC_ID:  execution ID

    IERR = NF_PUT_ATT_TEXT( FNUM, NF_GLOBAL, 'EXEC_ID', MXDLEN3, EXECN3 )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF file attribute EXEC_ID ' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed

    !!.......   FTYPE:  file type ID

    FTYPE3( FID ) = FTYPE3D
    IERR = NF_PUT_ATT_INT( FNUM, NF_GLOBAL, 'FTYPE', NF_INT, 1, FTYPE3D )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF file attribute FTYPE' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed

    !!.......   CDATE:  creation date

    IERR = NF_PUT_ATT_INT( FNUM, NF_GLOBAL, 'CDATE', NF_INT, 1, CURDATE )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF file attribute CDATE' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed

    !!.......   CTIME:  creation time

    IERR = NF_PUT_ATT_INT( FNUM, NF_GLOBAL, 'CTIME', NF_INT, 1, CURTIME )

    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF file attribute CTIME' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed

    !!.......   WDATE:  date of last update

    IERR = NF_PUT_ATT_INT( FNUM, NF_GLOBAL, 'WDATE', NF_INT, 1, CURDATE )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF file attribute WDATE' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed

    !!.......   WTIME:  time of last update

    IERR = NF_PUT_ATT_INT( FNUM, NF_GLOBAL, 'WTIME', NF_INT, 1, CURTIME )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF file attribute WTIME' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed


    !!...........   Set up attributes and variables valid for dictionary
    !!...........   files only:

    IF ( FTYPE3D .EQ. DCTNRY3 ) THEN

        CFLAG3 = CRDICT3( FID )
        GO TO 999

    END IF          !  ftype is dictionary


    !!...............   Set up remaining data file attributes:
    !!.......   SDATE:STIME  starting date&time (normalized by NEXTIME(:,:,0) )

    SDATE3( FID ) = SDATE3D
    STIME3( FID ) = STIME3D
    CALL NEXTIME( SDATE3( FID ), STIME3( FID ), 0 )

    IERR = NF_PUT_ATT_INT( FNUM, NF_GLOBAL, 'SDATE', NF_INT, 1, SDATE3( FID ) )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF file attribute SDATE' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed

    IERR = NF_PUT_ATT_INT( FNUM, NF_GLOBAL, 'STIME', NF_INT, 1, STIME3( FID ) )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF file attribute STIME' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed

    !!.......   TSTEP:  time step

    TSTEP3( FID ) = TSTEP3D
    IERR = NF_PUT_ATT_INT( FNUM, NF_GLOBAL, 'TSTEP', NF_INT, 1, TSTEP3D )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF file attribute TSTEP' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed

    !!.......   MXREC:  maximum existing time step number

    MXREC3( FID ) = 0

    !!.......   NTHIK:  thickness of perimeter (cells) for BOUNDARY files

    NTHIK3( FID ) = NTHIK3D
    IERR = NF_PUT_ATT_INT( FNUM, NF_GLOBAL, 'NTHIK', NF_INT, 1, NTHIK3D )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF file attribute NTHIK' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed

    !!.......   NCOLS:  number of grid columns/profile levels

    NCOLS3( FID ) = NCOLS3D
    IERR = NF_PUT_ATT_INT( FNUM, NF_GLOBAL, 'NCOLS', NF_INT, 1, NCOLS3D )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF file attribute NCOLS' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed

    !!.......   NROWS:  number of grid rows/data sites

    NROWS3( FID ) = NROWS3D
    IERR = NF_PUT_ATT_INT( FNUM, NF_GLOBAL, 'NROWS', NF_INT, 1, NROWS3D )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF file attribute NROWS' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed

    !!.......   NLAYS:  number of layers

    NLAYS3( FID ) = NLAYS3D
    IERR = NF_PUT_ATT_INT( FNUM, NF_GLOBAL, 'NLAYS', NF_INT, 1, NLAYS3D )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF file attribute NLAYS' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed

    !!.......   NVARS:  number of variables

    NVARS3( FID ) = NVARS3D
    IERR = NF_PUT_ATT_INT( FNUM, NF_GLOBAL, 'NVARS', NF_INT, 1, NVARS3D )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF file attribute NVARS' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed

    !!.......   GDTYP:  grid type ID (lat-lon, UTM, RADM, etc...)

    GDTYP3( FID ) = GDTYP3D
    IERR = NF_PUT_ATT_INT( FNUM, NF_GLOBAL, 'GDTYP', NF_INT, 1, GDTYP3D )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF file attribute GDTYP' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed

    !!.......   P_ALP:  first map-projection descriptive angle

    P_ALP3( FID ) = P_ALP3D
    IERR = NF_PUT_ATT_DOUBLE( FNUM, NF_GLOBAL, 'P_ALP', NF_DOUBLE, 1, P_ALP3D )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF file attribute P_ALP ' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed

    !!.......   P_BET:  second map-projection descriptive angle

    P_BET3( FID ) = P_BET3D
    IERR = NF_PUT_ATT_DOUBLE( FNUM, NF_GLOBAL, 'P_BET', NF_DOUBLE, 1, P_BET3D )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF file attribute P_BET ' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed

    !!.......   P_GAM:  third map-projection descriptive angle

    P_GAM3( FID ) = P_GAM3D
    IERR = NF_PUT_ATT_DOUBLE( FNUM, NF_GLOBAL, 'P_GAM', NF_DOUBLE, 1, P_GAM3D )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF file attribute P_GAM ' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed

    !!.......   XCENT:  lon of coordinate-system origin

    XCENT3( FID ) = XCENT3D
    IERR = NF_PUT_ATT_DOUBLE( FNUM, NF_GLOBAL, 'XCENT', NF_DOUBLE, 1, XCENT3D )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF file attribute XCENT ' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed

    !!.......   YCENT:  lat of coordinate-system origin

    YCENT3( FID ) = YCENT3D
    IERR = NF_PUT_ATT_DOUBLE( FNUM, NF_GLOBAL, 'YCENT', NF_DOUBLE, 1, YCENT3D )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF file attribute YCENT ' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed

    !!.......   XORIG:  X-coord of grid origin

    XORIG3( FID ) = XORIG3D
    IERR = NF_PUT_ATT_DOUBLE( FNUM, NF_GLOBAL, 'XORIG', NF_DOUBLE, 1, XORIG3D )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF file attribute XORIG ' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed

    !!.......   YORIG:  Y-coord of grid origin

    YORIG3( FID ) = YORIG3D
    IERR = NF_PUT_ATT_DOUBLE( FNUM, NF_GLOBAL, 'YORIG', NF_DOUBLE, 1, YORIG3D  )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF file attribute YORIG ' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed

    !!.......   XCELL:  cell width (X direction)

    XCELL3( FID ) = XCELL3D
    IERR = NF_PUT_ATT_DOUBLE( FNUM, NF_GLOBAL, 'XCELL', NF_DOUBLE, 1, XCELL3D )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF file attribute XCELL ' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed

    !!.......   YCELL:  cell width (Y direction)

    YCELL3( FID ) = YCELL3D
    IERR = NF_PUT_ATT_DOUBLE( FNUM, NF_GLOBAL, 'YCELL', NF_DOUBLE, 1, YCELL3D  )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF file attribute YCELL ' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed

    !!.......   VGTYP:  vertical coordinate type (VGSIGP3, ... ) or IMISS3

    IERR = NF_PUT_ATT_INT( FNUM, NF_GLOBAL, 'VGTYP', NF_INT, 1, VGTYP3D )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF file attribute VGTYP ' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed
    VGTYP3( FID ) = VGTYP3D

    !!.......   VGTOP:

    IERR = NF_PUT_ATT_REAL( FNUM, NF_GLOBAL, 'VGTOP', NF_FLOAT, 1, VGTOP3D )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF file attribute VGTOP ' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed
    VGTOP3( FID ) = VGTOP3D

    !!.......   VGLVS( 1, ..., NLAYS3D+1 ):  vertical coordinate values
    !!.......   (trapped against MXLAYS3 to prevent possible array overflow)

    IERR = NF_PUT_ATT_REAL( FNUM, NF_GLOBAL, 'VGLVLS', NF_FLOAT, MIN( NLAYS3D+1, MXLAYS3 ), VGLVS3D )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF file attribute VGLVLS ' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed
    DO L = 1, MIN( NLAYS3D+1, MXLAYS3)
       VGLVS3( L,FID ) = VGLVS3D( L )
    END DO

    !!.......   GDNAM:  grid name

    GDNAM3( FID ) = GDNAM3D
    IERR = NF_PUT_ATT_TEXT( FNUM, NF_GLOBAL, 'GDNAM', NAMLEN3, GDNAM3D )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF file attribute GDNAM ' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed

    !!.......   UPNAM:  name of last program to update the file

    IERR = NF_PUT_ATT_TEXT( FNUM, NF_GLOBAL, 'UPNAM', NAMLEN3, PGNAME )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF file attribute UPNAM ' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed

    !!.......   Variables-list for the file:

    IF( NVARS3( FID ) .GT. 0 ) THEN
        IERR = NF_PUT_ATT_TEXT( FNUM, NF_GLOBAL, 'VAR-LIST',  NAMLEN3 * NVARS3( FID ), VNAME3D )
        IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating attribute VAR-LIST ' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed
    END IF              !  if nvars > 0

    !!.......   FILEDESC:  file description

    IERR = NF_PUT_ATT_TEXT( FNUM, NF_GLOBAL, 'FILEDESC', MXDLEN3 * MXDESC3, FDESC3D )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF file attribute FILEDESC ' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed

    !!.......   HISTORY:  update description

    IERR = NF_PUT_ATT_TEXT( FNUM, NF_GLOBAL, 'HISTORY', MXDLEN3 * MXDESC3, SCNDSC )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF file attribute HISTORY ' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed



    !!...............   Now define dimensions for the file:
    !!...........   First:  time-step and layer-number dimensions
    !!...........   (valid for all file types)

    IF ( FTYPE3D .EQ. KFEVNT3 ) THEN

        TSIZE = NF_UNLIMITED
        DSIZE = 5               !  for ddim

    ELSE
        IF ( TSTEP3D .GT. 0 ) THEN
            TSIZE = NF_UNLIMITED
        ELSE IF ( TSTEP3D .LT. 0 ) THEN    !  restart (circular-buffer) file
            TSIZE = 2
        ELSE                               !  time-independent-data file
            TSIZE = 1
        END IF          !  kfevent, or not

        DSIZE = 2               !  for ddim

    END IF          !  kfevent, or tstep > 0, or < 0, or = 0

    IERR = NF_DEF_DIM( FNUM, 'TSTEP', TSIZE, TDIM )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF dimension TSTEP ' )
        EFLAG = .TRUE.
        GO TO 999
    END IF              !  ierr nonzero:  operation failed

    IERR = NF_DEF_DIM( FNUM, 'DATE-TIME', DSIZE, DDIM )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF dimension DATE-TIME' )
        EFLAG = .TRUE.
        GO TO 999
    END IF              !  ierr nonzero:  operation failed

    IERR = NF_DEF_DIM( FNUM, 'LAY', NLAYS3D, LDIM )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF dimension LAY' )
        EFLAG = .TRUE.
        GO TO 999
    END IF              !  ierr nonzero:  operation failed

    IERR = NF_DEF_DIM( FNUM, 'VAR', MAX( NVARS3D, 1 ), VDIM )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF dimension VAR ' )
        EFLAG = .TRUE.
        GO TO 999
    END IF              !  ierr nonzero:  operation failed

    IF ( FTYPE3D .EQ. KFEVNT3 ) THEN

        NDIMS = 2               !  for tindx(fid)
        DIMS( 1 ) = DDIM        !  date-time-length-col-row field
        DIMS( 2 ) = TDIM        !  time step dimension
        DSCBUF = '<YYYYDDD,HHMMSS,C,R,H*MMSS>'
        DSCBU2 = 'Event-valid flags:  start YYYYDDD,HHMMSS; COL,ROW; duration H*MMSS>'

    ELSE

        NDIMS = 3               !  for tindx(fid)
        DIMS( 1 ) = DDIM        !  date field or time field
        DIMS( 2 ) = VDIM        !  variable number
        DIMS( 3 ) = TDIM        !  time step dimension
        DSCBUF = '<YYYYDDD,HHMMSS>'
        DSCBU2 = 'Timestep-valid flags:  (1) YYYYDDD or (2) HHMMSS'

    END IF          !  kfevent, or tstep > 0, or < 0, or = 0


    !!...........   Create timestep-available-flag variable:

    IERR = NF_DEF_VAR( FNUM, 'TFLAG', NF_INT, NDIMS, DIMS, TINDX3( FID ) )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF variable TFLAG ' )
        EFLAG = .TRUE.
        GO TO 999
    END IF              !  ierr nonzero:  operation failed

    IERR = NF_PUT_ATT_TEXT( FNUM, TINDX3( FID ), 'units', NAMLEN3, DSCBUF( 1:NAMLEN3 ) )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating FLAG attribute "units"' )
        EFLAG = .TRUE.
        GO TO 999
    END IF              !  ierr nonzero:  operation failed

    DSCBUF = 'TFLAG'
    IERR = NF_PUT_ATT_TEXT( FNUM, TINDX3( FID ), 'long_name', NAMLEN3, DSCBUF( 1:NAMLEN3 ) )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating FLAG attribute "long_name" ' )
        EFLAG = .TRUE.
        GO TO 999
    END IF              !  ierr nonzero:  operation failed

    IERR = NF_PUT_ATT_TEXT( FNUM, TINDX3( FID ), 'var_desc', MXDLEN3, DSCBU2 )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating FLAG attribute VAR_DESC ')
        EFLAG = .TRUE.
        GO TO 999
    END IF              !  ierr nonzero:  operation failed


    !!...........   Now, other dimensions (depend upon file type):

    IF ( FTYPE3D .EQ. CUSTOM3 ) THEN        !  other dimensions not known

        IERR = NF_DEF_DIM( FNUM, 'COL', NCOLS3D, CDIM )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF dimension COL' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

        BSIZE3( FID ) = NCOLS3( FID )
        DIMS( 1 ) = CDIM    !  blob-size
        DIMS( 2 ) = LDIM
        DIMS( 3 ) = TDIM

        NDIMS = 3

    ELSE IF ( FTYPE3D .EQ. GRDDED3 ) THEN

        BSIZE3( FID ) = NCOLS3( FID ) * NROWS3( FID )
        IERR = NF_DEF_DIM( FNUM, 'ROW', NROWS3D, RDIM )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF dimension ROW ' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

        IERR = NF_DEF_DIM( FNUM, 'COL', NCOLS3D, CDIM )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF dimension COL' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

        DIMS( 1 ) = CDIM
        DIMS( 2 ) = RDIM
        DIMS( 3 ) = LDIM
        DIMS( 4 ) = TDIM

        NDIMS = 4

    ELSE IF ( FTYPE3D .EQ. BNDARY3 ) THEN

        BSIZE3( FID ) = 2 * ABS( NTHIK3( FID ) ) * ( NCOLS3( FID ) + NROWS3( FID ) + 2 * NTHIK3( FID ) )
        ISCR = 2 * NTHIK3D
        ISCR = ISCR * ( NCOLS3D + NROWS3D + ISCR )
        IERR = NF_DEF_DIM( FNUM, 'PERIM', ISCR, RDIM )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF dimension PERIM' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

        DIMS( 1 ) = RDIM
        DIMS( 2 ) = LDIM
        DIMS( 3 ) = TDIM

        NDIMS = 3

    ELSE IF ( FTYPE3D .EQ. IDDATA3 ) THEN

        BSIZE3( FID ) = NROWS3( FID )
        IERR = NF_DEF_DIM( FNUM, 'SITE-NO', NROWS3D, RDIM )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF dimension SITE-NO' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

        DIMS( 1 ) = TDIM

        IERR = NF_DEF_VAR( FNUM, 'ID-COUNT', NF_INT, 1, DIMS, NINDX3( FID ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF variable ID-COUNT' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

        DIMS( 1 ) = RDIM
        DIMS( 2 ) = TDIM

        IERR = NF_DEF_VAR( FNUM, 'ID', NF_INT, 2, DIMS, SINDX3( FID ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF variable ID' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

        DIMS( 1 ) = RDIM
        DIMS( 2 ) = LDIM
        DIMS( 3 ) = TDIM

        NDIMS = 3

    ELSE IF ( FTYPE3D .EQ. PROFIL3 ) THEN

        BSIZE3( FID ) = NCOLS3( FID ) * NROWS3( FID )
        IERR = NF_DEF_DIM( FNUM, 'SITE-NO', NROWS3D, RDIM )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF dimension SITE-NO' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

        IERR = NF_DEF_DIM( FNUM, 'LEVEL', NCOLS3D, CDIM )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF dimension LEVEL' )
            EFLAG = .TRUE.
             GO TO 999
        END IF              !  ierr nonzero:  operation failed

        DIMS( 1 ) = TDIM

        IERR = NF_DEF_VAR( FNUM, 'ID-COUNT', NF_INT, 1, DIMS, NINDX3( FID ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF variable ID-COUNT' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

        DIMS( 1 ) = RDIM
        DIMS( 2 ) = TDIM

        IERR = NF_DEF_VAR( FNUM, 'ID', NF_INT, 2, DIMS, SINDX3( FID ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF variable ID' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

        IERR = NF_DEF_VAR( FNUM, 'LEVEL-COUNT', NF_INT, 2, DIMS, LINDX3( FID ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF variable LEVEL-COUNT' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

        IERR = NF_DEF_VAR( FNUM, 'X', NF_FLOAT, 2, DIMS, XINDX3( FID ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF variable X' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

        IERR = NF_DEF_VAR( FNUM, 'Y', NF_FLOAT, 2, DIMS, YINDX3( FID ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF variable Y' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

        IERR = NF_DEF_VAR( FNUM, 'Z', NF_FLOAT, 2, DIMS, ZINDX3( FID ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF variable Z' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

        DIMS( 1 ) = CDIM
        DIMS( 2 ) = RDIM
        DIMS( 3 ) = LDIM
        DIMS( 4 ) = TDIM

        NDIMS = 4

    ELSE IF ( FTYPE3D .EQ. GRNEST3 ) THEN

        BSIZE3( FID ) = NCOLS3( FID ) * NROWS3( FID )
        IERR = NF_DEF_DIM( FNUM, 'SITE-NO', NROWS3D, RDIM )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF dimension SITE-NO' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

        IERR = NF_DEF_DIM( FNUM, 'LEVEL', NCOLS3D, CDIM )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF dimension LEVEL' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

        DIMS( 1 ) = TDIM

        IERR = NF_DEF_VAR( FNUM, 'ID-COUNT', NF_INT, 1, DIMS, NINDX3( FID ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF variable ID-COUNT' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

        DIMS( 1 ) = RDIM
        DIMS( 2 ) = TDIM

        IERR = NF_DEF_VAR( FNUM, 'ID', NF_INT, 2, DIMS, SINDX3( FID ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF variable ID' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

        IERR = NF_DEF_VAR( FNUM, 'WIN-COLS', NF_INT, 2, DIMS, WCNDX3( FID ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF variable WIN-ROWS' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

        IERR = NF_DEF_VAR( FNUM, 'WIN-ROWS', NF_INT, 2, DIMS, WRNDX3( FID ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF variable WIN-ROWS' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

        IERR = NF_DEF_VAR( FNUM, 'WIN-LEVS', NF_INT, 2, DIMS, LINDX3( FID ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF variable WIN-LEVS' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

        IERR = NF_DEF_VAR( FNUM, 'X', NF_DOUBLE, 2, DIMS, XINDX3( FID ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF variable X' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

        IERR = NF_DEF_VAR( FNUM, 'Y', NF_DOUBLE, 2, DIMS, YINDX3( FID ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF variable Y' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

        IERR = NF_DEF_VAR( FNUM, 'DX', NF_DOUBLE, 2, DIMS, DXNDX3( FID ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF variable DX' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

        IERR = NF_DEF_VAR( FNUM, 'DY', NF_DOUBLE, 2, DIMS, DYNDX3( FID ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF variable DY' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

        DIMS( 1 ) = CDIM
        DIMS( 2 ) = RDIM
        DIMS( 3 ) = TDIM

        NDIMS = 3

    ELSE IF ( FTYPE3D .EQ. SMATRX3 ) THEN

        BSIZE3( FID ) = NCOLS3( FID )
        IERR = NF_DEF_DIM( FNUM, 'ROW-NO', NROWS3D, RDIM )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF dimension ROW-NO' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

        IERR = NF_DEF_DIM( FNUM, 'SP-COEF-NO', NCOLS3D, CDIM )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF dimension SP-COEF-NO' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

        DIMS( 1 ) = RDIM
        DIMS( 2 ) = TDIM

        NDIMS = 2

        IERR = NF_DEF_VAR( FNUM, 'COL-COUNT', NF_INT, NDIMS, DIMS, SINDX3( FID ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF variable COL-COUNT' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

        DIMS( 1 ) = CDIM
        DIMS( 2 ) = TDIM

        NDIMS = 2

        IERR = NF_DEF_VAR( FNUM, 'COEFF-INDEX', NF_INT, NDIMS, DIMS, LINDX3( FID ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF variable COEFF-INDEX' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

    ELSE IF ( FTYPE3D .EQ. TSRIES3 ) THEN

        BSIZE3( FID ) = NCOLS3( FID ) * NROWS3( FID )
        IERR = NF_DEF_DIM( FNUM, 'ROW', NROWS3D, RDIM )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF dimension ROW' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

        IERR = NF_DEF_DIM( FNUM, 'COL', NCOLS3D, CDIM )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF dimension COL' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

        DIMS( 1 ) = CDIM
        DIMS( 2 ) = RDIM
        DIMS( 3 ) = LDIM
        DIMS( 4 ) = TDIM

        NDIMS = 4

    ELSE IF ( FTYPE3D .EQ. PTRFLY3 ) THEN   !  "exotic" gridded type

        BSIZE3( FID ) = NCOLS3( FID ) * NROWS3( FID )
        IERR = NF_DEF_DIM( FNUM, 'ROW', NROWS3D, RDIM )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF dimension ROW' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

        IERR = NF_DEF_DIM( FNUM, 'COL', NCOLS3D, CDIM )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF dimension COL' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

        DIMS( 1 ) = CDIM
        DIMS( 2 ) = RDIM
        DIMS( 3 ) = LDIM
        DIMS( 4 ) = TDIM

        NDIMS = 4

    ELSE IF ( FTYPE3D .EQ. KFEVNT3 ) THEN

        IERR = NF_DEF_DIM( FNUM, 'CELL-EVENT', NTHIK3D, EDIM )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF dimension CELL-EVENT' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

        IERR = NF_DEF_DIM( FNUM, 'ROW', NROWS3D, RDIM )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF dimension ROW' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

        IERR = NF_DEF_DIM( FNUM, 'COL', NCOLS3D, CDIM )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF dimension COL' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

        DIMS( 1 ) = CDIM
        DIMS( 2 ) = RDIM
        NDIMS = 2

        IERR = NF_DEF_VAR( FNUM, 'KFCOUNT', NF_INT, 2, DIMS, NINDX3( FID ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF variable KFCOUNT' )
             EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

        DIMS( 1 ) = EDIM
        DIMS( 2 ) = CDIM
        DIMS( 3 ) = RDIM
        NDIMS = 3

        IERR = NF_DEF_VAR( FNUM, 'KFEVENT', NF_INT, 3, DIMS, SINDX3( FID ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netcdf variable KFEVENT' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

        IERR = NF_DEF_VAR( FNUM, 'KFSDATE', NF_INT, 3, DIMS, LINDX3( FID ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF variable KFSDATE' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

        IERR = NF_DEF_VAR( FNUM, 'KFSTIME', NF_INT, 3, DIMS, XINDX3( FID ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF variable KFSTIME' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

        IERR = NF_DEF_VAR( FNUM, 'KFLNGTH', NF_INT, 3, DIMS, YINDX3( FID ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF variable KFLNGTH' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

        NDIMS = 2           !  for kfevnt variables
        DIMS( 1 ) = LDIM
        DIMS( 2 ) = TDIM

    ELSE    !  illegal file type

        CALL M3ABORT( FLIST3( FID ), FNUM, NF_EINVAL, 'ILLEGAL FILE TYPE.' )
        EFLAG = .TRUE.
        GO TO 999

    END IF  !  if file type CUSTOM3, GRDDED3, BNDARY3, IDDATA3,
            !  PROFIL3, GRNEST3, SMATRIX3, or else none of these.


    !!.......   Define all the Models-3 variables for this file:

    DO 111  VAR = 1 , NVARS3D

        ISCR = VTYPE3D( VAR )

        IF ( INDEXINT1( ISCR, NM3TYPES, M3TYPES ) .LE. 0 ) THEN
            WRITE( MESG, '( A, I5, 2X, A )' ) 'Illegal data type', ISCR, 'for variable ' // VNAME3D( VAR )
            CALL M3ABORT( FLIST3( FID ), FNUM, NF_EINVAL, MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF

        VLIST3( VAR,FID ) = VNAME3D( VAR )
        UNITS3( VAR,FID ) = UNITS3D( var )
        VTYPE3( VAR,FID ) = ISCR

        IERR = NF_DEF_VAR( FNUM, VNAME3D( VAR ), VTYPE3D( VAR ), NDIMS, DIMS, VINDX3( VAR,FID ) )
        IF ( IERR .NE. 0 ) THEN
            DSCBUF = 'Error creating netCDF variable ' // VNAME3D( VAR )
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBUF )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

        !!...............   Now define attributes for the variables:
        !!...........   Note:  "long_name" and "units" are standard netCDF attributes.

        IERR = NF_PUT_ATT_TEXT( FNUM, VINDX3( VAR,FID ), 'long_name', NAMLEN3, VNAME3D( VAR ) )
        IF ( IERR .NE. 0 ) THEN
            DSCBUF = 'Error creating  attribute LONG_NAME for variable ' // VNAME3D( VAR )
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBUF )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

        IERR = NF_PUT_ATT_TEXT( FNUM, VINDX3( VAR,FID ), 'units', NAMLEN3, UNITS3D( VAR ) )
        IF ( IERR .NE. 0 ) THEN
            DSCBUF = 'Error creating ' // VNAME3D( VAR ) // ' attribute UNITS for file ' // FLIST3( FID )
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBUF )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

        IERR = NF_PUT_ATT_TEXT( FNUM, VINDX3( VAR,FID ), 'var_desc', MXDLEN3, VDESC3D( VAR ) )
        IF ( IERR .NE. 0 ) THEN
            DSCBUF = 'Error creating  attribute VAR_DESC for variable ' // VNAME3D( VAR )
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBUF )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

111     CONTINUE        ! end loop on VAR, defining variables & attributes


    !!...........   Additional variable attributes for time series

    IF ( FTYPE3D .EQ. TSRIES3 ) THEN

        DO  VAR = 1 , NVARS3D

            IERR = NF_PUT_ATT_INT( FNUM, VINDX3( VAR,FID ), 'natts', NF_INT, 1, NATTS3D( VAR ) )
            IF ( IERR .NE. 0 ) THEN
                DSCBUF = 'Error creating  attribute "natts" for ' // 'variable ' // VNAME3D( VAR )
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBUF )
                EFLAG = .TRUE.
                GO TO 999
            END IF

            DO ATT = 1, NATTS3D( VAR )   ! loop for each addtnl var. att.

                IERR = NF_PUT_ATT_REAL( FNUM, VINDX3( VAR,FID ), ATNAM3D( ATT,VAR ), NF_FLOAT, 1, FATTS3D( ATT,VAR ) )

                IF ( IERR .NE. 0 ) THEN
                    DSCBUF = 'Error creating  attribute ' // ATNAM3D( ATT,VAR ) // ' for variable ' // VNAME3D( VAR )
                    CALL M3ABORT( FLIST3(FID), FNUM, IERR, DSCBUF )
                    EFLAG = .TRUE.
                    GO TO 999
                END IF

            END DO          !!  loop on ATT

        END DO              !!  loop on VAR

    END IF                  !!  ftype tseries3


    !!...........   Put FNUM back into data mode:  attributes and variables
    !!...........   now defined.

    IERR = NF_ENDDEF( FNUM )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error putting netCDF file into data mode.' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed

    IF ( FTYPE3D .EQ. KFEVNT3 ) THEN        !  initialize KFCOUNT

        DO  C = 1, MIN( NCOLS3( FID ), MXCOL )
            KFBUF( C ) = 0
        END DO

        DO R = 1, NROWS3( FID )
        DO C = 1, NCOLS3( FID ), MXCOL

            DIMS( 1 ) = C
            DIMS( 2 ) = R
            DELS( 1 ) = MIN( NCOLS3D, C+MXCOL-1 ) - C + 1
            DELS( 2 ) = 1

            IERR = NF_PUT_VARA_INT( FNUM, NINDX3( FID ), DIMS, DELS, KFBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error initializing KFCOUNT' )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !  ierr nonzero:  operation failed

        END DO
        END DO

    END IF          !  if KF-Event file:         !  if file is volatile


    IF ( CFMETA ) THEN
        IF ( .NOT.SETCF( FID ) ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error with CF metadata' )
            EFLAG = .TRUE.
            GO TO 999
        END IF
    END IF

    IF ( CMAQMETA ) THEN
        IF ( .NOT.SETCMAQ( FID ) ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error with CMAQ metadata' )
            EFLAG = .TRUE.
            GO TO 999
        END IF
    END IF

    IF ( SMOKEMETA ) THEN
        IF ( .NOT.SETSMOKE( FID ) ) THEN
!!                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,          !!  SMOKEmeta not yet implemented
!!     &          'Error with SMOKE metadata' )
!!                EFLAG = .TRUE.
!!                GO TO 999
        END IF
    END IF

    IF ( TEXTMETA ) THEN
        IF ( .NOT.SETMTEXT( FID ) ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error with TEXT metadata' )
            EFLAG = .TRUE.
            GO TO 999
        END IF
    END IF


    IF ( VOLAT3( FID ) ) THEN     !  volatile file:  synch with disk
        IERR = NF_SYNC( FNUM )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error with disk synchronization' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  if synch failed
    END IF 


    !!.......   File creation now complete, or else we have a failure-exit
    !!.......   from this critical section.

999     CONTINUE

!$OMP END CRITICAL( S_NC )

    IF ( EFLAG ) THEN
!$OMP       CRITICAL( S_NC )
        IERR =  NF_CLOSE( CDFID3( FID ) )
!$OMP       END CRITICAL( S_NC )
        CFLAG3        = .FALSE.
        CDFID3( FID ) = IMISS3
        FLIST3( FID ) = CMISS3
    ELSE
        CFLAG3 = .TRUE.
    END IF

    RETURN

    !!******************  FORMAT  STATEMENTS   ******************************

    !!...........   Error and warning message formats..... 91xxx

91010   FORMAT ( //5X , '>>> WARNING in subroutine CRTFIL3 <<<',    &
              2 ( /5X , A , : ) , I5, // )


END FUNCTION CRTFIL3

