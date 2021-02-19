
LOGICAL FUNCTION PN_CRTFIL3( EQNAME, FID, PGNAME ) RESULT( CFLAG3 )

    !!***********************************************************************
    !! Version "$Id: pn_crtfil3.F90 1 2017-06-10 18:05:20Z coats $"
    !! EDSS/Models-3 I/O API.
    !! Copyright (C) 2014-2015 UNC Institute for the Environment.
    !! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    !! See file "LGPL.txt" for conditions of use.
    !!.........................................................................
    !!  subroutine body starts at line 112
    !!
    !!  FUNCTION:
    !!      Create PnetCDF file FNAME using info stored in the FDESC3
    !!      commons.  Record history,   Leave file open for update.
    !!
    !!  PRECONDITIONS REQUIRED:
    !!       File definition already put into FDESC3 common.
    !!
    !!  SUBROUTINES AND FUNCTIONS CALLED:
    !!       CRDICT3
    !!
    !!  REVISION  HISTORY:
    !!      Adapted  07/2015 by David Wong from  I/O API-3.1 "crtfil3.F"
    !!      to support PnetCDF distributed-file operations
    !!
    !!      Version  08/2015 by CJC: Major cleanup and bug-fixes for I/O API-3.2
    !!      Factor non-I/O-PE case through PN_OPNFIL3()
    !!
    !!      Version  11/2015 by CJC: replace MPI_OFFSET_KIND by hard-coded INTEGER(8)
    !!      because OpenMPI-1.4.x does not follow the MPOI "standard" competently.
    !!***********************************************************************

#ifdef IOAPI_PNCF

    USE M3UTILIO
    USE MODATTS3
    USE MODPDATA
    USE MODNCFIO

    IMPLICIT NONE

#ifdef   _AIX
#define  FLUSH flush_
#endif

    !!...........   INCLUDES:

    INCLUDE 'STATE3.EXT'
    INCLUDE 'mpif.h'


    !!...........   ARGUMENTS and their descriptions:

    CHARACTER*(*), INTENT(IN   ) ::   EQNAME  !  physical name of file to be created
    INTEGER      , INTENT(IN   ) ::   FID     !  index into STATE3 arrays
    CHARACTER*(*), INTENT(IN   ) ::   PGNAME  !  name of calling program


    !!...........   EXTERNAL FUNCTIONS and their descriptions:

    LOGICAL, EXTERNAL :: PN_OPNFIL3
    LOGICAL, EXTERNAL :: SYNCFID


    !!...........   PARAMETERs and their descriptions:

    CHARACTER*16,  PARAMETER :: PNAME  = 'PN_CRTFIL3'


    !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         ISCR
    INTEGER         FNUM            !  PnetCDF file ID from NCCRE()
    INTEGER         IERR            !  PnetCDF error status return
    INTEGER         ATT             !  loop counter for var. attributes
    INTEGER         VAR             !  loop counter for file variables
    INTEGER         CDIM            !  col/level     dim #
    INTEGER         RDIM            !  row/site      dim #
    INTEGER         LDIM            !  layer         dim #
    INTEGER         DDIM            !  timestamp date-and-time dimension #
    INTEGER         EDIM            !  cell-event    dim # (KF only)
    INTEGER         TDIM            !  timestep      dim #
    INTEGER         VDIM            !  variable-flag dim #
    INTEGER         NDIMS           !  number of dims  for NFMPI_DEF_VAR()
    INTEGER         DIMS( 5 )       !  array  of dims  for NFMPI_DEF_VAR()
    INTEGER         DELS( 5 )       !  array  of edges for NFMPI_PUT_VARA*()
    INTEGER         C, R, L
    LOGICAL         EFLAG, AFLAG
    CHARACTER*80    DSCBUF          !  scratch buffer for descriptions
    CHARACTER*80    DSCBU2          !  scratch buffer for descriptions
    CHARACTER*512   NAMBUF          !  scratch buffer to upcase EQNAME in.
    CHARACTER*256   MESG            !  scratch buffer to upcase EQNAME in.

    LOGICAL, SAVE :: FIRSTIME = .TRUE.
    LOGICAL, SAVE :: OFFSET64 = .FALSE.
    INTEGER, SAVE :: FMODE           !  PnetCDF file-opening mode

!!  INTEGER( MPI_OFFSET_KIND ) :: TSIZE           !  time-step mode
!!  INTEGER( MPI_OFFSET_KIND ) :: DSIZE           !  D/T width
!!  INTEGER( MPI_OFFSET_KIND ) :: TV1, TV2, TV3

    INTEGER( 8 ) :: TSIZE           !  time-step mode
    INTEGER( 8 ) :: DSIZE           !  D/T width
    INTEGER( 8 ) :: TV1, TV2, TV3

    !!.............................................................................
    !!   begin body of subroutine  PN_CRTFIL3
    !!...........  Create the PnetCDF file, and put it into definition mode.


!$OMP   CRITICAL( WRITE3_INIT )
    IF ( FIRSTIME ) THEN

        OFFSET64 = ENVYN( 'IOAPI_OFFSET_64',                &
                          'Use NF_64BIT_OFFSET or not',     &
                          .FALSE.,  IERR )
        IF ( IERR .GT. 0 ) THEN
            EFLAG = .TRUE.
            CALL M3MSG2( 'Bad environment vble "IOAPI_OFFSET_64"' )
        END IF

        FMODE = IOR( NF_NOCLOBBER, NF_SHARE )
        IF ( OFFSET64 ) THEN
            FMODE = IOR( FMODE, NF_64BIT_OFFSET )
        END IF

        AFLAG = ENVYN( 'IOAPI_CFMETA',                                  &
                       'Generate CF-convention metadata or not?',       &
                       .FALSE., IERR )
        IF ( IERR .GT. 0 ) THEN
            CALL M3MSG2( 'Bad environment vble "IOAPI_CFMETA"' )
        ELSE IF ( AFLAG ) THEN
            CALL INITCF()
        END IF

        CALL ENVSTR( 'IOAPI_CMAQMETA', 'File for CMAQ-convention metadata, "ENV", or "NONE"?', 'NONE', NAMBUF, IERR )
        IF ( IERR .GT. 0 ) THEN
            CALL M3MSG2( 'Bad environment vble "IOAPI_CMAQMETA"' )
        ELSE IF ( NAMBUF .EQ. 'NONE' ) THEN
            CONTINUE
        ELSE
            EFLAG = INITCMAQ()
        END IF

        CALL ENVSTR( 'IOAPI_SMOKEMETA', 'File for SMOKE-convention metadata or "NONE"?', 'NONE', NAMBUF, IERR )
        IF ( IERR .GT. 0 ) THEN
            CALL M3MSG2( 'Bad environment vble "IOAPI_SMOKEMETA"' )
        ELSE IF ( NAMBUF .EQ. 'NONE' ) THEN
            CONTINUE
        ELSE
            EFLAG = INITSMOKE()
        END IF

        FIRSTIME = .FALSE.

    END IF		!  firstime
!$OMP   END CRITICAL( WRITE3_INIT )

    IF ( NVARS3D .GT. MXVARS3 ) THEN
        MESG = 'Max NVARS for this build exceeded:  file' // FLIST3( FID )
        CALL M3WARN( 'OPEN3/PN_CRTFIL3', 0,0, NAMBUF )
        CFLAG3 = .FALSE.
        RETURN
    ELSE IF ( FTYPE3D .NE. GRDDED3 ) THEN
        MESG = 'PN_CRTFIL3:  Non-GRIDDED files not supported for PnetCDF mode'
        CALL M3WARN( 'OPEN3/PN_CRTFIL3', 0,0, NAMBUF )
        CFLAG3 = .FALSE.
        RETURN
    ELSE IF ( NCOLS3D .NE. GRID_COLS  .OR.  NROWS3D .NE. GRID_ROWS ) THEN
        WRITE( MESG, '( 2( A, I6, 2X ))' ) 'Master-grid NCOLS=', GRID_COLS, 'file-grid NCOLS=', NCOLS3D
        CALL M3MESG( MESG )
        WRITE( MESG, '( 2( A, I6, 2X ))' ) 'Master-grid NROWS=', GRID_ROWS, 'file-grid NROWS=', NROWS3D
        CALL M3MESG( MESG )
        MESG = 'PN_CRTFIL3:  File dimensions must match master-grid dimensions'
        CALL M3WARN( 'OPEN3/PN_CRTFIL3', 0,0, NAMBUF )
        CFLAG3 = .FALSE.
        RETURN
    END IF

    EFLAG = .FALSE.

!$OMP CRITICAL( S_NC )

    !!...........  Else if an I/O PE, create PnetCDF file and proceed...

    IF ( PN_IO_PE ) THEN

        IERR = NFMPI_CREATE( PN_COL_IO_COMM, EQNAME, FMODE, MPI_INFO_NULL, FNUM )

        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_CRTFIL3:  Error creating PnetCDF file' )
            CALL M3MSG2( EQNAME )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

        CDFID3( FID ) = FNUM


        !!...........   Set attributes valid for all file types:
        !!.......   IOAPI_VERSION:  I/O API version and date

        IERR = NFMPI_PUT_ATT_TEXT( FNUM, NF_GLOBAL, 'IOAPI_VERSION', PN_MXDLEN, VERSN3 )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_CRTFIL3:  Error creating PnetCDF file att IOAPI_VERSION' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

        !!.......   EXEC_ID:  execution ID

        IERR = NFMPI_PUT_ATT_TEXT( FNUM, NF_GLOBAL, 'EXEC_ID', PN_MXDLEN, EXECN3 )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_CRTFIL3:  Error creating PnetCDF file attribute EXEC_ID ' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

        !!.......   FTYPE:  file type ID:  must be GRIDDED on-file, MPIGRD3 in STATE3 arrays

        FTYPE3( FID ) = MPIGRD3
        IERR = NFMPI_PUT_ATT_INT1( FNUM, NF_GLOBAL, 'FTYPE', NF_INT, FTYPE3D )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_CRTFIL3:  Error creating PnetCDF file attribute FTYPE' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

        !!.......   CDATE:  creation date

        IERR = NFMPI_PUT_ATT_INT1( FNUM, NF_GLOBAL, 'CDATE', NF_INT, CURDATE )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_CRTFIL3:  Error creating PnetCDF file attribute CDATE' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

        !!.......   CTIME:  creation time

        IERR = NFMPI_PUT_ATT_INT1( FNUM, NF_GLOBAL, 'CTIME', NF_INT, CURTIME )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_CRTFIL3:  Error creating PnetCDF file attribute CTIME' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

        !!.......   WDATE:  date of last update

        IERR = NFMPI_PUT_ATT_INT1( FNUM, NF_GLOBAL, 'WDATE',  NF_INT, CURDATE)
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_CRTFIL3:  Error creating PnetCDF file attribute WDATE' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

        !!.......   WTIME:  time of last update

        IERR = NFMPI_PUT_ATT_INT1( FNUM, NF_GLOBAL, 'WTIME', NF_INT, CURTIME)
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_CRTFIL3:  Error creating PnetCDF file attribute WTIME' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed


        !!...............   Set up remaining data file attributes:
        !!.......   SDATE:STIME  starting date&time (normalized by NEXTIME(:,:,0) )

        SDATE3( FID ) = SDATE3D
        STIME3( FID ) = STIME3D
        CALL NEXTIME( SDATE3( FID ), STIME3( FID ), 0 )

        IERR = NFMPI_PUT_ATT_INT1( FNUM, NF_GLOBAL, 'SDATE', NF_INT, SDATE3( FID ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_CRTFIL3:  Error creating PnetCDF file attribute SDATE' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_PUT_ATT_INT1( FNUM, NF_GLOBAL, 'STIME', NF_INT, STIME3( FID ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_CRTFIL3:  Error creating PnetCDF file attribute STIME' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

        !!.......   TSTEP:  time step

        TSTEP3( FID ) = TSTEP3D
        IERR = NFMPI_PUT_ATT_INT1( FNUM, NF_GLOBAL, 'TSTEP', NF_INT, TSTEP3D )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_CRTFIL3:  Error creating PnetCDF file attribute TSTEP' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

        !!.......   MXREC:  maximum existing time step number

        MXREC3( FID ) = 0

        !!.......   NTHIK:  thickness of perimeter (cells) for BOUNDARY files

        NTHIK3( FID ) = NTHIK3D
        IERR = NFMPI_PUT_ATT_INT1( FNUM, NF_GLOBAL, 'NTHIK', NF_INT, NTHIK3D )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_CRTFIL3:  Error creating PnetCDF file attribute NTHIK' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

        !!.......   NCOLS:  number of grid columns/profile levels

           NCOLS3( FID ) = NCOLS3D
        IERR = NFMPI_PUT_ATT_INT1( FNUM, NF_GLOBAL, 'NCOLS', NF_INT, NCOLS3D )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_CRTFIL3:  Error creating PnetCDF file attribute NCOLS' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

        !!.......   NROWS:  number of grid rows/data sites

        NROWS3( FID ) = NROWS3D
        IERR = NFMPI_PUT_ATT_INT1( FNUM, NF_GLOBAL, 'NROWS', NF_INT, NROWS3D )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_CRTFIL3:  Error creating PnetCDF file attribute NROWS' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

        !!.......   NLAYS:  number of layers

        NLAYS3( FID ) = NLAYS3D
        IERR = NFMPI_PUT_ATT_INT1( FNUM, NF_GLOBAL, 'NLAYS', NF_INT, NLAYS3D )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_CRTFIL3:  Error creating PnetCDF file attribute NLAYS' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

        !!.......   NVARS:  number of variables

        NVARS3( FID ) = NVARS3D
        IERR = NFMPI_PUT_ATT_INT( FNUM, NF_GLOBAL, 'NVARS', NF_INT, PN_ONE, NVARS3D )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_CRTFIL3:  Error creating PnetCDF file attribute NVARS' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

        !!.......   GDTYP:  grid type ID (lat-lon, UTM, RADM, etc...)

        GDTYP3( FID ) = GDTYP3D
        IERR = NFMPI_PUT_ATT_INT1( FNUM, NF_GLOBAL, 'GDTYP', NF_INT, GDTYP3D )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_CRTFIL3:  Error creating PnetCDF file attribute GDTYP' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

        !!.......   P_ALP:  first map-projection descriptive angle

        P_ALP3( FID ) = P_ALP3D
        IERR = NFMPI_PUT_ATT_DOUBLE( FNUM, NF_GLOBAL, 'P_ALP', NF_DOUBLE, PN_ONE, P_ALP3D )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_CRTFIL3:  Error creating PnetCDF file attribute P_ALP ' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

        !!.......   P_BET:  second map-projection descriptive angle

        P_BET3( FID ) = P_BET3D
        IERR = NFMPI_PUT_ATT_DOUBLE( FNUM, NF_GLOBAL, 'P_BET', NF_DOUBLE, PN_ONE, P_BET3D )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_CRTFIL3:  Error creating PnetCDF file attribute P_BET ' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

        !!.......   P_GAM:  third map-projection descriptive angle

        P_GAM3( FID ) = P_GAM3D
        IERR = NFMPI_PUT_ATT_DOUBLE( FNUM, NF_GLOBAL, 'P_GAM', NF_DOUBLE, PN_ONE, P_GAM3D )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_CRTFIL3:  Error creating PnetCDF file attribute P_GAM ' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

        !!.......   XCENT:  lon of coordinate-system origin

        XCENT3( FID ) = XCENT3D
        IERR = NFMPI_PUT_ATT_DOUBLE( FNUM, NF_GLOBAL, 'XCENT', NF_DOUBLE, PN_ONE, XCENT3D )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_CRTFIL3:  Error creating PnetCDF file attribute XCENT ' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

        !!.......   YCENT:  lat of coordinate-system origin

        YCENT3( FID ) = YCENT3D
        IERR = NFMPI_PUT_ATT_DOUBLE( FNUM, NF_GLOBAL, 'YCENT', NF_DOUBLE, PN_ONE, YCENT3D )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_CRTFIL3:  Error creating PnetCDF file attribute YCENT ' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

        !!.......   XORIG:  X-coord of grid origin

        XORIG3( FID ) = XORIG3D
        IERR = NFMPI_PUT_ATT_DOUBLE( FNUM, NF_GLOBAL, 'XORIG', NF_DOUBLE, PN_ONE, XORIG3D )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_CRTFIL3:  Error creating PnetCDF file attribute XORIG ' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

        !!.......   YORIG:  Y-coord of grid origin

        YORIG3( FID ) = YORIG3D
        IERR = NFMPI_PUT_ATT_DOUBLE( FNUM, NF_GLOBAL, 'YORIG', NF_DOUBLE, PN_ONE, YORIG3D )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_CRTFIL3:  Error creating PnetCDF file attribute YORIG ' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

        !!.......   XCELL:  cell width (X direction)

        XCELL3( FID ) = XCELL3D
        IERR = NFMPI_PUT_ATT_DOUBLE( FNUM, NF_GLOBAL, 'XCELL', NF_DOUBLE, PN_ONE, XCELL3D )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_CRTFIL3:  Error creating PnetCDF file attribute XCELL ' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

        !!.......   YCELL:  cell width (Y direction)

        YCELL3( FID ) = YCELL3D
        IERR = NFMPI_PUT_ATT_DOUBLE( FNUM, NF_GLOBAL, 'YCELL', NF_DOUBLE, PN_ONE, YCELL3D )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_CRTFIL3:  Error creating PnetCDF file attribute YCELL ' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

        !!.......   VGTYP:  vertical coordinate type (VGSIGP3, ... ) or IMISS3

        IERR = NFMPI_PUT_ATT_INT1( FNUM, NF_GLOBAL, 'VGTYP', NF_INT, VGTYP3D )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_CRTFIL3:  Error creating PnetCDF file attribute VGTYP ' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed
        VGTYP3( FID ) = VGTYP3D

        !!.......   VGTOP:

        IERR = NFMPI_PUT_ATT_REAL( FNUM, NF_GLOBAL, 'VGTOP', NF_REAL, PN_ONE, VGTOP3D )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_CRTFIL3:  Error creating PnetCDF file attribute VGTOP ' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed
        VGTOP3( FID ) = VGTOP3D

        !!.......   VGLVS( 1, ..., NLAYS3D+1 ):  vertical coordinate values
        !!.......   (trapped against MXLAYS3 to prevent possible array overflow)

        TV1 = MIN( NLAYS3D+1, MXLAYS3 )      !! as integer( mpi_offset_kind )
        IERR = NFMPI_PUT_ATT_REAL( FNUM, NF_GLOBAL, 'VGLVLS', NF_REAL, TV1, VGLVS3D )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_CRTFIL3:  Error creating PnetCDF file attribute VGLVLS ' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed
        DO L = 1, MIN( NLAYS3D+1, MXLAYS3)
            VGLVS3( L,FID ) = VGLVS3D( L )
        END DO

        !!.......   GDNAM:  grid name

        GDNAM3( FID ) = GDNAM3D
        IERR = NFMPI_PUT_ATT_TEXT( FNUM, NF_GLOBAL, 'GDNAM', PN_NAMLEN, GDNAM3D )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_CRTFIL3:  Error creating PnetCDF file attribute GDNAM ' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCAPTC) failed

        !!.......   UPNAM:  name of last program to update the file

        IERR = NFMPI_PUT_ATT_TEXT( FNUM, NF_GLOBAL, 'UPNAM', PN_NAMLEN, PGNAME )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_CRTFIL3:  Error creating PnetCDF file attribute UPNAM ' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

        !!.......   Variables-list for the file:

        IF( NVARS3( FID ) .GT. 0 ) THEN
            TV2 = NAMLEN3 * NVARS3( FID )      !! as integer( mpi_offset_kind )
            IERR = NFMPI_PUT_ATT_TEXT( FNUM, NF_GLOBAL, 'VAR-LIST', TV2, VNAME3D )
            IF ( IERR .NE. 0 ) THEN
               CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_CRTFIL3:  Error creating attribute VAR-LIST ' )
               EFLAG = .TRUE.
               GO TO 999
            END IF              !  ierr nonzero:  operation failed
        END IF              !  if nvars > 0

        !!.......   FILEDESC:  file description

        IERR = NFMPI_PUT_ATT_TEXT( FNUM, NF_GLOBAL, 'FILEDESC', PN_DSCLEN, FDESC3D )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_CRTFIL3:  Error creating PnetCDF file attribute FILEDESC ' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

        !!.......   HISTORY:  update description

        IERR = NFMPI_PUT_ATT_TEXT( FNUM, NF_GLOBAL, 'HISTORY', PN_DSCLEN, SCNDSC )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_CRTFIL3:  Error creating PnetCDF file attribute HISTORY ' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed



        !!...............   Now define dimensions for the file:
        !!...........   First:  time-step and layer-number dimensions
        !!...........   (valid for all file types)

        IF ( TSTEP3D .GT. 0 ) THEN
            TSIZE = NF_UNLIMITED
        ELSE IF ( TSTEP3D .LT. 0 ) THEN    !  restart (circular-buffer) file
            TSIZE = 2
        ELSE                               !  time-independent-data file
            TSIZE = 1
        END IF          !  kfevent, or not

        DSIZE = 2               !  for ddim

        IERR = NFMPI_DEF_DIM( FNUM, 'TSTEP', TSIZE, TDIM )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_CRTFIL3:  Error creating PnetCDF dimension TSTEP ' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

        IERR = NFMPI_DEF_DIM( FNUM, 'DATE-TIME', DSIZE, DDIM )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_CRTFIL3:  Error creating PnetCDF dimension DATE-TIME' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

        TV3 = NLAYS3D      !! as integer( mpi_offset_kind )
        IERR = NFMPI_DEF_DIM( FNUM, 'LAY', TV3, LDIM )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_CRTFIL3:  Error creating PnetCDF dimension LAY' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

        TV3  = MAX( NVARS3D, 1 )      !! as integer( mpi_offset_kind )
        IERR = NFMPI_DEF_DIM( FNUM, 'VAR', TV3, VDIM )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_CRTFIL3:  Error creating PnetCDF dimension VAR ' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

        NDIMS = 3               !  for tindx(fid)
        DIMS( 1 ) = DDIM        !  date field or time field
        DIMS( 2 ) = VDIM        !  variable number
        DIMS( 3 ) = TDIM        !  time step dimension
        DSCBUF = '<YYYYDDD,HHMMSS>'
        DSCBU2 ='Timestep-valid flags: (1) YYYYDDD or (2) HHMMSS'


        !!...........   Create timestep-available-flag variable:

        IERR = NFMPI_DEF_VAR( FNUM, 'TFLAG', NF_INT, NDIMS, DIMS, TINDX3( FID ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_CRTFIL3:  Error creating PnetCDF variable TFLAG ' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

        IERR = NFMPI_PUT_ATT_TEXT( FNUM, TINDX3( FID ), 'units', PN_NAMLEN, DSCBUF( 1:NAMLEN3 ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_CRTFIL3:  Error creating FLAG attribute "units"' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

        DSCBUF = 'TFLAG'
        IERR = NFMPI_PUT_ATT_TEXT( FNUM, TINDX3( FID ), 'long_name', PN_NAMLEN, DSCBUF( 1:NAMLEN3 ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,  'PN_CRTFIL3:  Error creating FLAG attribute "long_name" ' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

        IERR = NFMPI_PUT_ATT_TEXT( FNUM, TINDX3( FID ), 'var_desc', PN_MXDLEN, DSCBU2 )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_CRTFIL3:  Error creating FLAG attribute VAR_DESC ')
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed


        !!...........   Now, other dimensions (file type must be MPIGRD3):

        BSIZE3( FID ) = NCOLS3( FID ) * NROWS3( FID )
        TV3 = NROWS3D
        IERR = NFMPI_DEF_DIM(FNUM, 'ROW', TV3, RDIM )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_CRTFIL3:  Error creating PnetCDF dimension ROW ' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

        TV3 = NCOLS3D
        IERR = NFMPI_DEF_DIM(FNUM, 'COL', TV3, CDIM )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_CRTFIL3:  Error creating PnetCDF dimension COL' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

        DIMS( 1 ) = CDIM
        DIMS( 2 ) = RDIM
        DIMS( 3 ) = LDIM
        DIMS( 4 ) = TDIM

        NDIMS = 4


        !!.......   Define all the Models-3 variables for this file:

        DO 111  VAR = 1 , NVARS3D

            ISCR = VTYPE3D( VAR )

            IF ( ISCR .NE. M3INT   .AND.     &
                 ISCR .NE. M3INT8  .AND.     &
                 ISCR .NE. M3REAL  .AND.     &
                 ISCR .NE. M3DBLE ) THEN
                WRITE( MESG,'(A,I10)')     'Illegal data type', ISCR
                CALL M3MESG( MESG )
                MESG =  'PN_CRTFIL3:  Error creating variable ' // VNAME3D( VAR ) // ' for file ' // FLIST3( FID )
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, MESG )
                EFLAG = .TRUE.
                IERR = NFMPI_ABORT( FNUM )
                GO TO 999
            END IF

            VLIST3( VAR,FID ) = VNAME3D( VAR )
            UNITS3( VAR,FID ) = UNITS3D( VAR )
            VTYPE3( VAR,FID ) = ISCR

            IERR = NFMPI_DEF_VAR( FNUM, VNAME3D( VAR ), VTYPE3D( VAR ), NDIMS, DIMS, VINDX3( VAR,FID ) )

            IF ( IERR .NE. 0 ) THEN
                DSCBUF =  'PN_CRTFIL3:  Error creating PnetCDF variable ' // VNAME3D( VAR )
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBUF )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  operation failed

            !!...............   Now define attributes for the variables:
            !!...........   Note:  "long_name" and "units" are standard PnetCDF attributes.

            IERR = NFMPI_PUT_ATT_TEXT( FNUM, VINDX3( VAR,FID ), 'long_name', PN_NAMLEN, VNAME3D( VAR ) )
            IF ( IERR .NE. 0 ) THEN
                MESG = 'PN_CRTFIL3:  Error creating  attribute LONG_NAME for variable ' // VNAME3D( VAR )
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, MESG )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  operation failed

            IERR = NFMPI_PUT_ATT_TEXT( FNUM, VINDX3( VAR,FID ), 'units', PN_NAMLEN, UNITS3D( VAR ) )
            IF ( IERR .NE. 0 ) THEN
                MESG = 'PN_CRTFIL3:  Error creating ' // VNAME3D( VAR ) //   &
                    ' attribute UNITS for file ' // FLIST3( FID )
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, MESG )
                EFLAG = .TRUE.
                IERR = NFMPI_ABORT(FNUM)
                GO TO 999
            END IF              !  ierr nonzero:  operation failed

            IERR = NFMPI_PUT_ATT_TEXT( FNUM, VINDX3( VAR,FID ), 'var_desc', PN_MXDLEN, VDESC3D( VAR ) )
            IF ( IERR .NE. 0 ) THEN
                DSCBUF = 'PN_CRTFIL3:  Error creating  attribute UNITS for variable ' // VNAME3D( VAR )
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBUF )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  operation failed

111     CONTINUE        ! end loop on VAR, defining variables & attributes


        !!...........   Put FNUM back into data mode:  attributes and variables
        !!...........   now defined.

        IERR = NFMPI_ENDDEF( FNUM )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_CRTFIL3:  Error putting PnetCDF file into data mode.' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NCENDF() failed


        IF ( CFMETA ) THEN
            IF ( .NOT.SETCF( FID ) ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error with CF metadata' )
                EFLAG = .TRUE.
                GO TO 999
            END IF
        END IF

        IF ( CMAQMETA ) THEN
            IF ( .NOT.SETCMAQ( FID ) ) THEN
!!                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,      !!  cmaqmeta not yet implemented
!!     &          'Error with CF metadata' )
!!                EFLAG = .TRUE.
!!                GO TO 999
            END IF
        END IF

        IF ( SMOKEMETA ) THEN
            IF ( .NOT.SETSMOKE( FID ) ) THEN
!!                CALL M3ABORT( FLIST3( FID ), FNUM, IERR,      !!  SMOKEmeta not yet implemented
!!     &          'Error with CF metadata' )
!!                EFLAG = .TRUE.
!!                GO TO 999
            END IF
        END IF

        IF ( VOLAT3( FID ) ) THEN     !  volatile file:  synch with disk
            IERR = NFMPI_SYNC( FNUM )

            IF ( IERR .NE. 0 ) THEN
                MESG = TRIM( PNAME ) // '  Error with disk synchronization'
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, MESG )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  if synch failed
        END IF          !  if file is volatile

    END IF      !!  if this is an I/O PE


    !!.......   File creation now complete, or else we have a failure-exit
    !!.......   from this critical section.

999 CONTINUE

!$OMP END CRITICAL( S_NC )


    !!.......   Must synch file outside the critical section,
    !!.......   then for non-I/O PE's use ONPFIL3() after a
    !!.......   barrier ensures all PE's are up-to-date.


    IF ( PN_IO_PE ) THEN
        IF ( EFLAG ) THEN
!$OMP       CRITICAL( S_NC )
            EFLAG =  ( NFMPI_CLOSE( FNUM ) .NE. NF_NOERR )
!$OMP       END CRITICAL( S_NC )
        ELSE IF ( .NOT.SYNCFID( FID ) ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'PN_CRTFIL3:  synch failure' )
        END IF
    END IF

    CALL MPI_BARRIER( PN_COL_IO_COMM, IERR )
    IF ( IERR .NE. 0 ) THEN
        EFLAG = .TRUE.
        WRITE( MESG, '(A,I9)' ) 'PN_CRTFIL3:  MPI_BARRIER error.  IERR=', IERR
    END IF

    IF ( .NOT.PN_IO_PE ) THEN
        IF ( .NOT.EFLAG ) THEN
            EFLAG = ( .NOT. PN_OPNFIL3( EQNAME, FID, NF_NOWRITE, PGNAME ) )
        END IF
    END IF

    IF ( EFLAG ) THEN
        CFLAG3 = .FALSE.
        CDFID3( FID ) = IMISS3
        FLIST3( FID ) = CMISS3
    ELSE
        CFLAG3 = .TRUE.
    END IF

#endif

#ifndef IOAPI_PNCF

    CALL M3WARN( PNAME, 0,0, 'PN_CRTFIL3:  PnetCDF Mode not enabled' )
    CFLAG3 = .FALSE.

#endif

    RETURN

END FUNCTION PN_CRTFIL3

