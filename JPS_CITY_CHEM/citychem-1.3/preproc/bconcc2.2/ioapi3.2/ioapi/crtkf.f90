
LOGICAL FUNCTION CRTKF( EQNAME, FID, PGNAME, BUFFER )  RESULT( KFFLAG )

    !!***********************************************************************
    !! Version "$Id: crtkf.f90 1 2017-06-10 18:05:20Z coats $"
    !! EDSS/Models-3 I/O API.
    !! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    !! (C) 2003-2011 Baron Advanced Meteorological Systems,
    !! (C) 2011 David Wong, (C) 2007-2013 Carlie J. Coats, Jr.,, and
    !! (C) 2015 UNC Institute for the Environment.
    !! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    !! See file "LGPL.txt" for conditions of use.
    !!.........................................................................
    !!  subroutine body starts at line 86
    !!
    !!  FUNCTION:  Create netCDF KF-Cloud Event file FNAME using info stored
    !!      in the FDESC3 common.  Record history, Leave file open for update.
    !!
    !!  PRECONDITIONS REQUIRED:  File definition already put into FDESC3 common
    !!
    !!  SUBROUTINES AND FUNCTIONS CALLED:  M3ABORT()
    !!
    !!  REVISION  HISTORY:
    !!       adapted   4/1996 from CRTFIL3() by CJC for KF-Cloud Event files
    !!       revised   9/1999 by CJC:  Unification of KFOPEN() with OPEN3()
    !!       revised   2/2002 by CJC:  for volatile, call nf_sync() before return
    !!       Revised   3/2002 by CJC:  OpenMP thread-safety;
    !!                                 turn off NF_SHARED for Crays
    !!       Modified 10/2009 by CJC:  flags-change for netCDF4  from
    !!       Martin Otte, US EPA -- remove NF_WRITE.
    !!       Modified 03/20010 by CJC: F9x changes for I/O API v3.1
    !!       Revised 4/2011 by David Wong, US EPA, and by CJC, to add state for
    !!       full buffered-file file descriptions.
    !!       Modified 02-08/2015 by CJC for I/O API 3.2: USE M3UTILIO, MODNCFIO,
    !!       NF_*() interfaces; F90 "free" source format.
    !!      Modified 12/2017 by CJC:  default TRUE for IOAPI_OFFSET_64
    !!***********************************************************************

    USE M3UTILIO
    USE MODNCFIO

    IMPLICIT NONE

    !!...........   INCLUDES:

    INCLUDE 'STATE3.EXT'


    !!...........   ARGUMENTS and their descriptions:

    CHARACTER*(*), INTENT(IN   ) :: EQNAME  !  physical name of file to be created
    INTEGER      , INTENT(IN   ) :: FID     !  index into STATE3 arrays
    CHARACTER*(*), INTENT(IN   ) :: PGNAME  !  name of calling program
    REAL         , INTENT(  OUT) :: BUFFER( * )


    !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         ISCR
    INTEGER         FNUM            !  netCDF file ID from NF_CREATE()
    INTEGER         IERR            !  netCDF error status return
    INTEGER         VAR, LVL        !  loop counters for file variables
    INTEGER         TSIZE           !  time-step mode
    INTEGER         CDIM            !  col/level     dim #
    INTEGER         RDIM            !  row/site      dim #
    INTEGER         LDIM            !  layer         dim #
    INTEGER         EDIM            !  cell-event    dim #
    INTEGER         DDIM            !  date-time-duration-cell dim #
    INTEGER         TDIM            !  timestep      dim #
    INTEGER         VDIM            !  variable-flag dim #
    INTEGER         NDIMS           !  number of dims for NF_DEF_VAR()
    INTEGER         DIMS( 5 )       !  array  of dims for NF_DEF_VAR()
    INTEGER         DELS( 5 )       !  array  of dims for NF_DEF_VAR()
    LOGICAL         EFLAG
    CHARACTER*8     BNAME           !  for "BUFFERED", etc.
    CHARACTER*80    DSCBUF          !  scratch buffer for descriptions

    INTEGER, SAVE :: FMODE           !  netCDF file-opening mode
    LOGICAL, SAVE :: FIRSTIME = .TRUE.
    LOGICAL, SAVE :: OFFSET64 = .FALSE.

    !!.............................................................................
    !!   begin body of subroutine  CRTKF
    !!...........  Create the netCDF file, and put it into definition mode.

    BNAME = EQNAME( 1:8 )   ! normalize case for "BUFFERED" files
    CALL UPCASE( BNAME )
    IF ( BNAME .EQ. 'BUFFERED' ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Path not a real file:' )
        CALL M3MSG2( EQNAME )
        KFFLAG = .FALSE.
        RETURN
    END IF

    EFLAG = .FALSE.

!$OMP   CRITICAL( WRITE3_INIT )
    IF ( FIRSTIME ) THEN

        OFFSET64 = ENVYN( 'IOAPI_OFFSET_64', 'Use NF_64BIT_OFFSET or not', .TRUE., IERR )
        IF ( IERR .GT. 0 ) THEN
            EFLAG = .TRUE.
            CALL M3MSG2( 'Bad environment vble "IOAPI_OFFSET_64"' )
        END IF

        FMODE = IOR( NF_NOCLOBBER, NF_SHARE )
        IF ( OFFSET64 ) THEN
            FMODE = IOR( FMODE, NF_64BIT_OFFSET )
        END IF

        FIRSTIME = .FALSE.

    END IF          !  firstime
!$OMP   END CRITICAL( WRITE3_INIT )


!$OMP CRITICAL( S_NC )

    IERR = NF_CREATE( EQNAME , FMODE, FNUM )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF file at path:' )
        CALL M3MSG2( EQNAME )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed

    CDFID3( FID ) = FNUM


    !!...........   Set attributes valid for all file types:
    !!.......   IOAPI_VERSION:  I/O API version and date

    IERR = NF_PUT_ATT_TEXT( FNUM, NF_GLOBAL, 'IOAPI_VERSION', LEN_TRIM( DSCBUF ), VERSN3 )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF file attribute IOAPI_VERSION' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed

    !!.......   EXEC_ID:  execution ID

    IERR = NF_PUT_ATT_TEXT( FNUM, NF_GLOBAL, 'EXEC_ID', MXDLEN3, EXECN3 )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF file attribute EXEC_ID' )
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

    !!.......   SDATE:  starting date

    SDATE3( FID ) = SDATE3D
    IERR = NF_PUT_ATT_INT( FNUM, NF_GLOBAL, 'SDATE', NF_INT, 1, SDATE3D )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF file attribute SDATE' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed

    !!.......   STIME:  starting time:

    STIME3( FID ) = STIME3D
    IERR = NF_PUT_ATT_INT( FNUM, NF_GLOBAL, 'STIME', NF_INT, 1, STIME3D )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF file attribute STIME' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed

    !!.......   MXREC:  maximum existing time step number

    MXREC3( FID ) = 0

    !!.......   NTHIK:  mapped onto cellwise max number of events

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
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF file attribute P_ALP' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed

    !!.......   P_BET:  second map-projection descriptive angle

    P_BET3( FID ) = P_BET3D
    IERR = NF_PUT_ATT_DOUBLE( FNUM, NF_GLOBAL, 'P_BET', NF_DOUBLE, 1, P_BET3D )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF file attribute P_BET' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed

    !!.......   P_GAM:  third map-projection descriptive angle

    P_GAM3( FID ) = P_GAM3D
    IERR = NF_PUT_ATT_DOUBLE( FNUM, NF_GLOBAL, 'P_GAM', NF_DOUBLE, 1, P_GAM3D )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF file attribute P_GAM' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed

    !!.......   XCENT:  lon of coordinate-system origin

    XCENT3( FID ) = XCENT3D
    IERR = NF_PUT_ATT_DOUBLE( FNUM, NF_GLOBAL, 'XCENT', NF_DOUBLE, 1, XCENT3D )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF file attribute XCENT' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed

    !!.......   YCENT:  lat of coordinate-system origin

    YCENT3( FID ) = YCENT3D
    IERR = NF_PUT_ATT_DOUBLE( FNUM, NF_GLOBAL, 'YCENT', NF_DOUBLE, 1, YCENT3D )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF file attribute YCENT' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed

    !!.......   XORIG:  X-coord of grid origin

    XORIG3( FID ) = XORIG3D
    IERR = NF_PUT_ATT_DOUBLE( FNUM, NF_GLOBAL, 'XORIG', NF_DOUBLE, 1, XORIG3D )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF file attribute XORIG' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed

    !!.......   YORIG:  Y-coord of grid origin

    YORIG3( FID ) = YORIG3D
    IERR = NF_PUT_ATT_DOUBLE( FNUM, NF_GLOBAL, 'YORIG', NF_DOUBLE, 1, YORIG3D )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF file attribute YORIG' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed

    !!.......   XCELL:  cell width (X direction)

    XCELL3( FID ) = XCELL3D
    IERR = NF_PUT_ATT_DOUBLE( FNUM, NF_GLOBAL, 'XCELL', NF_DOUBLE, 1, XCELL3D )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF file attribute XCELL' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed

    !!.......   YCELL:  cell width (Y direction)

    YCELL3( FID ) = YCELL3D
    IERR = NF_PUT_ATT_DOUBLE( FNUM, NF_GLOBAL, 'YCELL', NF_DOUBLE, 1, YCELL3D )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF file attribute YCELL' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed

    !!.......   VGTYP:  vertical coordinate type (VGSIGP3, ... ) or IMISS3

    IERR = NF_PUT_ATT_INT( FNUM, NF_GLOBAL, 'VGTYP', NF_INT, 1, VGTYP3D )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF file attribute VGTYP' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed
    VGTYP3( FID ) = VGTYP3D

    !!.......   VGTOP:

    IERR = NF_PUT_ATT_REAL( FNUM, NF_GLOBAL, 'VGTOP', NF_FLOAT, 1, VGTOP3D )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF file attribute VGTOP' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed
    VGTOP3( FID ) = VGTOP3D

    !!.......   VGLVS( 1, ..., NLAYS3D+1 ):  vertical coordinate values
    !!.......   (trapped against MXLAYS3 to prevent possible array overflow)

    IERR = NF_PUT_ATT_REAL( FNUM, NF_GLOBAL, 'VGLVLS', NF_FLOAT, MIN( NLAYS3D+1, MXLAYS3), VGLVS3D )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF file attribute VGLVLS' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed         !  ierr nonzero:  operation failed
    DO LVL = 1, MIN( NLAYS3D+1, MXLAYS3 )
       VGLVS3( LVL,FID ) = VGLVS3D( LVL )
    END DO

    !!.......   GDNAM:  grid name

    GDNAM3( FID ) = GDNAM3D
    IERR = NF_PUT_ATT_TEXT( FNUM, NF_GLOBAL, 'GDNAM', NAMLEN3, GDNAM3D )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF file attribute GDNAM' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed

    !!.......   UPNAM:  name of last program to update the file

    IERR = NF_PUT_ATT_TEXT( FNUM, NF_GLOBAL, 'UPNAM', NAMLEN3, PGNAME )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF file attribute UPNAM' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed

    !!.......   Variables-list for the file:

    IF( NVARS3( FID ) .GT. 0 ) THEN
        IERR = NF_PUT_ATT_TEXT( FNUM, NF_GLOBAL, 'VAR-LIST', NAMLEN3 * NVARS3( FID ), VNAME3D  )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating attribute VAR-LIST for file' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed
    END IF              !  if nvars > 0

    !!.......   FILEDESC:  file description

    IERR = NF_PUT_ATT_TEXT( FNUM, NF_GLOBAL, 'FILEDESC', MXDLEN3 * MXDESC3, FDESC3D )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF file attribute FILEDESC' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed

    !!.......   HISTORY:  update description

    IERR = NF_PUT_ATT_TEXT( FNUM, NF_GLOBAL, 'HISTORY', MXDLEN3 * MXDESC3, SCNDSC )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF file attribute HISTORY' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed



    !!...............   Now define dimensions for the file:
    !!...........   First:  time-step (event) and layer-number dimensions
    !!...........   (valid for all file types)

    TSIZE = NF_UNLIMITED

    IERR = NF_DEF_DIM( FNUM, 'TSTEP', TSIZE, TDIM )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF dimension TSTEP' )
        EFLAG = .TRUE.
        GO TO 999
    END IF              !  ierr nonzero:  operation failed

    IERR = NF_DEF_DIM( FNUM, 'EVENT-LOC', 5, DDIM )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF dimension EVENT-LOC' )
        EFLAG = .TRUE.
        GO TO 999
    END IF              !  ierr nonzero:  operation failed

    IERR = NF_DEF_DIM( FNUM, 'CELL-EVENT', NTHIK3D, EDIM )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF dimension CELL-EVENT' )
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
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF dimension VAR' )
        EFLAG = .TRUE.
        GO TO 999
    END IF              !  ierr nonzero:  operation failed


    !!...........   Now, other dimensions

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

    !!...........   Create KFCOUNT event-count variable

    DIMS( 1 ) = CDIM
    DIMS( 2 ) = RDIM
    NDIMS = 2

    IERR = NF_DEF_VAR( FNUM, 'KFCOUNT', NF_INT, 2, DIMS, NINDX3( FID ) )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF variable KFCOUNT' )
        EFLAG = .TRUE.
        GO TO 999
    END IF              !  ierr nonzero:  operation failed

    !!...........   Create event-description variable:

    DIMS( 1 ) = DDIM        !  date-time-length-col-row field
    DIMS( 2 ) = TDIM        !  time step dimension
    NDIMS = 2

    IERR = NF_DEF_VAR( FNUM, 'TFLAG', NF_INT, NDIMS, DIMS, TINDX3( FID ) )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF variable TFLAG' )
        EFLAG = .TRUE.
        GO TO 999
    END IF              !  ierr nonzero:  operation failed

    DSCBUF = '<YYYYDDD,HHMMSS,C,R,H*MMSS>'
    IERR = NF_PUT_ATT_TEXT( FNUM, TINDX3( FID ), 'units', NAMLEN3, DSCBUF( 1:NAMLEN3 )  )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating FLAG attribute "units"' )
        EFLAG = .TRUE.
        GO TO 999
    END IF              !  ierr nonzero:  operation failed

    DSCBUF = 'TFLAG'
    IERR = NF_PUT_ATT_TEXT( FNUM, TINDX3( FID ), 'long_name', NAMLEN3, DSCBUF( 1:NAMLEN3 ) )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating FLAG attribute "long_name"' )
        EFLAG = .TRUE.
        GO TO 999
    END IF              !  ierr nonzero:  operation failed

    DSCBUF = 'Event-valid flags:  start YYYYDDD,HHMMSS; COL,ROW; duration H*MMSS>'
    IERR = NF_PUT_ATT_TEXT( FNUM, TINDX3( FID ), 'var_desc', MXDLEN3, DSCBUF )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating FLAG attribute VAR_DESC' )
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


    !!.......   Define rest of the variables for this file:

    DIMS( 1 ) = LDIM
    DIMS( 2 ) = TDIM
    NDIMS = 2

    DO  VAR = 1 , NVARS3D

        ISCR = VTYPE3D( VAR )

        IF ( ISCR .NE. M3INT   .AND. ISCR .NE. M3REAL  .AND. ISCR .NE. M3DBLE  .AND. ISCR .NE. M3INT8 ) THEN
            WRITE( DSCBUF, '( A, I5, 2X, A )' ) 'Illegal data type', ISCR, 'for variable ' // VNAME3D( VAR )
            CALL M3ABORT( FLIST3( FID ), FNUM, NF_EINVAL, DSCBUF )
            EFLAG = .TRUE.
            GO TO 999
        END IF

        VLIST3( VAR,FID ) = VNAME3D( VAR )
        UNITS3( VAR,FID ) = UNITS3D( VAR )
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
            DSCBUF = 'Error creating attribute LONG_NAME for ' // VNAME3D( VAR )
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBUF )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

        IERR = NF_PUT_ATT_TEXT( FNUM, VINDX3( VAR,FID ), 'units', NAMLEN3, UNITS3D( VAR ) )
        IF ( IERR .NE. 0 ) THEN
            DSCBUF = 'Error creating ttribute UNITS for ' // VNAME3D( VAR )
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBUF )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

        IERR = NF_PUT_ATT_TEXT( FNUM, VINDX3( VAR,FID ), 'var_desc', MXDLEN3, VDESC3D( VAR ) )
        IF ( IERR .NE. 0 ) THEN
            DSCBUF = 'Error creating  attribute VAR_DESC for ' // VNAME3D( VAR )
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBUF )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  ierr nonzero:  operation failed

    END DO        ! end loop on VAR, defining variables & attributes


    !!...........   Put FNUM back into data mode:  attributes and variables
    !!...........   now defined.

    IERR = NF_ENDDEF( FNUM )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error putting netCDF file into data mode.' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed

    !!.......   File creation now complete (control only falls through
    !!.......   to here in case of success).  Need to initialize KFCOUNT:

    DO  VAR = 1, NCOLS3D * NROWS3D
        BUFFER( VAR ) = 0
    END DO


    DIMS( 1 ) = 1
    DIMS( 2 ) = 1
    DELS( 1 ) = NCOLS3D
    DELS( 2 ) = NROWS3D
    IERR = NF_PUT_VARA_INT( FNUM, NINDX3( FID ), DIMS, DELS, BUFFER )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error initializing KFCOUNT' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed

    IF ( VOLAT3( FID ) ) THEN     !  volatile file:  synch with disk
        IERR = NF_SYNC( FNUM )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error with disk synchronization' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  if synch failed
    END IF          !  if file is volatile


    !!.......   File creation now complete, or else we have a failure-exit
    !!.......   from this critical section.

999     CONTINUE

!$OMP END CRITICAL( S_NC )

    KFFLAG = ( .NOT. EFLAG )

    RETURN


END FUNCTION CRTKF



