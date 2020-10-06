
LOGICAL FUNCTION  OPNKF( EQNAME, FID, FSTATUS, PGNAME, BUFFER )

    !!***********************************************************************
    !! Version "$Id: opnkf.F90 1 2017-06-10 18:05:20Z coats $"
    !! EDSS/Models-3 I/O API.
    !! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    !! (C) 2003-2010 Baron Advanced Meteorological Systems
    !! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    !! See file "LGPL.txt" for conditions of use.
    !!.........................................................................
    !!  function body starts at line  87
    !!
    !!  FUNCTION:
    !!       open pre-existing KF-Cloud file with logical name FLIST3( FID ),
    !!       with readonly/readwrite status FSTATUS.  If opened for
    !!       write, copy scenario description from I/O STATE3.EXT to
    !!       file's history, and name PGNAME of caller to file's
    !!       updater-name.  Returns TRUE if the file is already open.
    !!       Return gridded event-counts array in BUFFER
    !!
    !!  RETURN VALUE:
    !!       TRUE iff it succeeds in opening the file, reading its
    !!       attributes, and storing the relevant ones in STATE3.EXT
    !!
    !!  PRECONDITIONS REQUIRED:  File FLIST3( FID ) already exists.
    !!
    !!  SUBROUTINES AND FUNCTIONS CALLED:
    !!
    !!  REVISION  HISTORY:
    !!      prototype 9/1994 by CJC to go with new version of OPEN3()
    !!
    !!      Modified  2/1995 by CJC to handle file type SMATRX3.
    !!
    !!      revised  6/1999 by CJC:  OpenMP thread-safety
    !!
    !!      revised   3/2002 by CJC:  error message for bad ncclos() status,
    !!      for volatile, call ncsync() before return; no NF_SHARED  for Crays
    !!
    !!      Modified 5/2003 by CJC: bugfix by by David Wong, US EPA;
    !!      set NF_SHARE for volatile files.
    !!
    !!      revised   6/2002 by CJC:  Don't set NF_SHARE for Cray
    !!
    !!      Modified 10/2009 by CJC:  flags-change for netCDF4  from
    !!      Martin Otte, US EPA -- remove NF_NOCLOBBER
    !!
    !!      Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !!
    !!      Modified 10/2015 by CJC for I/O API 3.2: USE MODNCFIO; use NF_*()
    !!      instead of NC*(), for netCDF-Fortran 4.x compatibility;
    !!      free F90 source format
    !!***********************************************************************

    USE MODNCFIO

    IMPLICIT NONE

    !!...........   INCLUDES:

    INCLUDE 'PARMS3.EXT'
    INCLUDE 'STATE3.EXT'


    !!...........   ARGUMENTS and their descriptions:

    CHARACTER*(*), INTENT(IN   ) :: EQNAME  !  physical name of file to be opened
    INTEGER      , INTENT(IN   ) :: FID     !  index into STATE3 tables
    INTEGER      , INTENT(IN   ) :: FSTATUS !  read-only/read-write status for NF_OPEN()
    CHARACTER*(*), INTENT(IN   ) :: PGNAME  !  name of calling program
    REAL         , INTENT(  OUT) :: BUFFER( * )


    !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER              FMODE      !  netCDF file-opening mode
    INTEGER              FNUM       !  netCDF file ID from NF_CREATE()
    INTEGER              TDIM       !  netCDF dimension ID from NF_DIMID
    INTEGER              IERR       !  netCDF error status return
    INTEGER              VAR        !  loop counter for file variables
    INTEGER              ND, NA
    INTEGER              D( MAXVDIMS )
    LOGICAL              EFLAG
    INTEGER         DIMS ( 5 )      !  corner arg array for NF_GET_VARA_*()
    INTEGER         DELTS( 5 )      !  corner arg array for NF_GET_VARA_*()
    CHARACTER*(MAXNCNAM) TNAME      !  dummy arg for NF_INQ_DIM
    CHARACTER*256        MESG       !  fixed-length buffer

    !!.............................................................................
    !!   begin body of subroutine  OPNKF

    IF ( FSTATUS .NE. NF_WRITE ) THEN
        FMODE = FSTATUS
    ELSE
#if _CRAY
        FMODE = NF_WRITE
#endif
#if ! ( _CRAY )
       FMODE = IOR( NF_SHARE, NF_WRITE )
#endif
    END IF
#if ! ( _CRAY )
    IF ( VOLAT3( FID ) ) THEN
        FMODE = IOR( FMODE , NF_SHARE )
    END IF
#endif

    EFLAG = .FALSE.

!$OMP CRITICAL( S_NC )

    !!...........  Open the netCDF file with that EQNAME.

    MESG = EQNAME
    IERR  = NF_OPEN( EQNAME, FMODE, FNUM )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR,  'Error opening file at path-name:' )
        CALL M3MSG2( EQNAME )
        EFLAG = .TRUE.
        GO TO  999        !  return
    END IF          !  ierr nonzero:  NF_CREATE() failed
    CDFID3( FID ) = FNUM


    !!...........   Get attributes
    !!...........   FTYPE:  file type ID

    IERR = NF_GET_ATT_INT( FNUM, NF_GLOBAL, 'FTYPE', FTYPE3( FID ) )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR,  'Error reading netCDF file attribute FTYPE.' )
        EFLAG = .TRUE.
        GO TO  999        !  return
    ELSE IF ( FTYPE3( FID ) .NE. KFEVNT3 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR,  'OPNKF:  file not a KF-Cloud Event file' )
        EFLAG = .TRUE.
        GO TO  999        !  return
    END IF          !  ierr nonzero:  operation failed

    !!.......   NVARS:  number of variables

    IERR = NF_GET_ATT_INT( FNUM, NF_GLOBAL, 'NVARS', NVARS3( FID ) )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading netCDF file attribute NVARS' )
        EFLAG = .TRUE.
        GO TO  999        !  return
    END IF          !  ierr nonzero:  operation failed


    !!.......   SDATE:  starting date (Julian date YYYYDDD)

    IERR = NF_GET_ATT_INT( FNUM, NF_GLOBAL, 'SDATE', SDATE3( FID )  )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading netCDF file attribute SDATE' )
        EFLAG = .TRUE.
        GO TO  999        !  return
    END IF          !  ierr nonzero:  operation failed

    !!.......   STIME:  starting time (HHMMSS)

    IERR = NF_GET_ATT_INT( FNUM, NF_GLOBAL, 'STIME', STIME3( FID )  )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error netCDF file attribute STIME' )
        EFLAG = .TRUE.
        GO TO  999        !  return
    END IF          !  ierr nonzero:  operation failed

    !!.......   NTHIK:  mapped onto cellwise max number of events

    IERR = NF_GET_ATT_INT( FNUM, NF_GLOBAL, 'NTHIK', NTHIK3( FID ) )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading netCDF file attribute NTHIK' )
        EFLAG = .TRUE.
        GO TO  999        !  return
    END IF          !  ierr nonzero:  operation failed

    !!.......   NCOLS:  number of grid columns/profile levels

    IERR = NF_GET_ATT_INT( FNUM, NF_GLOBAL, 'NCOLS', NCOLS3( FID ) )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading netCDF file attribute NCOLS' )
        EFLAG = .TRUE.
        GO TO  999        !  return
    END IF          !  ierr nonzero:  operation failed

    !!.......   NROWS:  number of grid rows/data sites

    IERR = NF_GET_ATT_INT( FNUM, NF_GLOBAL, 'NROWS', NROWS3( FID ) )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading netCDF file attribute NROWS' )
        EFLAG = .TRUE.
        GO TO  999        !  return
    END IF          !  ierr nonzero:  operation failed

    !!.......   NLAYS:  number of layers

    IERR = NF_GET_ATT_INT( FNUM, NF_GLOBAL, 'NLAYS', NLAYS3( FID ) )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading netCDF file attribute NLAYS' )
        EFLAG = .TRUE.
        GO TO  999        !  return
    END IF          !  ierr nonzero:  operation failed

    !!.......   GDTYP:  grid type ID (lat-lon, UTM, RADM, etc...)

    IERR = NF_GET_ATT_INT( FNUM, NF_GLOBAL, 'GDTYP', GDTYP3( FID ) )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading netCDF file attribute GDTYP' )
        EFLAG = .TRUE.
        GO TO  999        !  return
    END IF          !  ierr nonzero:  operation failed


    !!.......   P_ALP:  first map-projection-description angle

    IERR = NF_GET_ATT_DOUBLE( FNUM, NF_GLOBAL, 'P_ALP', P_ALP3( FID ) )
    IF ( IERR .NE. 0 ) THEN
        P_ALP3( FID ) = BADVAL3
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'WARNING:  Error in netCDF file attribute P_ALP' )
        EFLAG = .TRUE.
        GO TO  999        !  return
    END IF          !  ierr nonzero:  operation failed

    !!.......   P_BET:  second map-projection-description angle

    IERR = NF_GET_ATT_DOUBLE( FNUM, NF_GLOBAL, 'P_BET', P_BET3( FID ) )
    IF ( IERR .NE. 0 ) THEN
        P_BET3( FID ) = BADVAL3
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'WARNING:  Error in netCDF file attribute P_BET' )
        EFLAG = .TRUE.
        GO TO  999        !  return
    END IF          !  ierr nonzero:  operation failed

    !!.......   P_GAM:  third map-projection-description angle

    IERR = NF_GET_ATT_DOUBLE( FNUM, NF_GLOBAL, 'P_GAM', P_GAM3( FID ) )
    IF ( IERR .NE. 0 ) THEN
        P_GAM3( FID ) = BADVAL3
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'WARNING:  Error in netCDF file attribute P_GAM' )
        EFLAG = .TRUE.
        GO TO  999        !  return
    END IF          !  ierr nonzero:  operation failed

    !!.......   XCENT:  lon of coordinate-system (0,0) origin

    IERR = NF_GET_ATT_DOUBLE( FNUM, NF_GLOBAL, 'XCENT', XCENT3( FID ) )
    IF ( IERR .NE. 0 ) THEN
        XCENT3( FID ) = BADVAL3
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'WARNING:  Error in netCDF file attribute XCENT' )
        EFLAG = .TRUE.
        GO TO  999        !  return
    END IF          !  ierr nonzero:  operation failed

    !!.......   YCENT:  lat of coordinate-system (0,0) origin

    IERR = NF_GET_ATT_DOUBLE( FNUM, NF_GLOBAL, 'YCENT', YCENT3( FID ) )
    IF ( IERR .NE. 0 ) THEN
        YCENT3( FID ) = BADVAL3
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'WARNING: Error in netCDF file attribute YCENT' )
       EFLAG = .TRUE.
        GO TO  999        !  return
    END IF          !  ierr nonzero:  operation failed

    !!.......   XORIG:  X-coord of grid origin
    !!.......   (in map units; see FDESC3.EXT for description)

    IERR = NF_GET_ATT_DOUBLE( FNUM, NF_GLOBAL, 'XORIG', XORIG3( FID ) )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading netCDF file attribute XORIG' )
        EFLAG = .TRUE.
        GO TO  999        !  return
    END IF          !  ierr nonzero:  operation failed

    !!.......   YORIG:  Y-coord of grid origin
    !!.......   (in map units; see FDESC3.EXT for description)

    IERR = NF_GET_ATT_DOUBLE( FNUM, NF_GLOBAL, 'YORIG', YORIG3( FID ) )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading netCDF file attribute YORIG' )
        EFLAG = .TRUE.
        GO TO  999        !  return
    END IF          !  ierr nonzero:  operation failed

    !!.......   XCELL:  cell width (X direction)
    !!.......   (in map units; see FDESC3.EXT for description)

    IERR = NF_GET_ATT_DOUBLE( FNUM, NF_GLOBAL, 'XCELL', XCELL3( FID ) )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading netCDF file attribute XCELL' )
        EFLAG = .TRUE.
        GO TO  999        !  return
    END IF          !  ierr nonzero:  operation failed

    !!.......   YCELL:  cell width (Y direction)
    !!.......   (in map units; see FDESC3.EXT for description)

    IERR = NF_GET_ATT_DOUBLE( FNUM, NF_GLOBAL, 'YCELL', YCELL3( FID ) )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading netCDF file attribute YCELL' )
        EFLAG = .TRUE.
        GO TO  999        !  return
    END IF          !  ierr nonzero:  operation failed

    !!.......   VGTYP:  vertical coordinate type

    IERR = NF_GET_ATT_INT( FNUM, NF_GLOBAL, 'VGTYP', VGTYP3( FID ) )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading file attribute VGTYP.' )
        CALL M3MSG2( MESG )
        GO TO 999
    END IF          !  ierr nonzero:  operation failed

    !!.......   VGTOP:  vertical  coordinate sigma-top

    IERR = NF_GET_ATT_REAL( FNUM, NF_GLOBAL, 'VGTOP', VGTOP3( FID ) )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading file attribute VGTOP.' )
        CALL M3MSG2( MESG )
        GO TO 999
    END IF          !  ierr nonzero:  operation failed

    !!.......   VGLVS:  vertical coordinate values

    IERR = NF_GET_ATT_REAL( FNUM, NF_GLOBAL, 'VGLVLS', VGLVS3( 1,FID ) )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading file attribute VGLVS.' )
        CALL M3MSG2( MESG )
        GO TO 999
    END IF          !  ierr nonzero:  operation failed

    !!.......   GDNAM:  grid name

    IERR = NF_GET_ATT_TEXT( FNUM, NF_GLOBAL, 'GDNAM', GDNAM3( FID ) )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading netCDF file attribute GDNAM' )
        EFLAG = .TRUE.
        GO TO  999        !  return
    END IF          !  ierr nonzero:  NCAGTC) failed


    !!...........   Get ID for timestep-available-flag variable:

    IERR = NF_INQ_VARID( FNUM, 'TFLAG', TINDX3( FID ) )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading netCDF ID for variable TFLAG' )
        EFLAG = .TRUE.
        GO TO  999        !  return
    END IF              !  ierr nonzero:  operation failed

    !!...........   Get extent of the time dimension:

    IERR = NF_INQ_DIMID( FNUM, 'TSTEP', TDIM )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading netCDF time-dimension ID' )
        EFLAG = .TRUE.
        GO TO  999        !  return
    END IF              !  ierr nonzero:  operation failed

    IERR = NF_INQ_DIMLEN( FNUM, TDIM, MXREC3( FID ) )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading maximum timestep record number' )
        EFLAG = .TRUE.
        GO TO  999        !  return
    END IF              !  ierr nonzero:  operation failed


    !!...........   Now, get netCDF variable ID's

    BSIZE3( FID ) = NROWS3( FID )

    IERR = NF_INQ_VARID( FNUM, 'KFCOUNT', NINDX3( FID ) )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading netCDF ID for variable ID-COUNT' )
        EFLAG = .TRUE.
        GO TO  999        !  return
    END IF              !  ierr nonzero:  NF_INQ_VARID() failed

    IERR = NF_INQ_VARID( FNUM, 'KFEVENT', SINDX3( FID ) )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading netCDF variable ID' )
        EFLAG = .TRUE.
        GO TO  999        !  return
    END IF              !  ierr nonzero:  NF_INQ_VARID() failed

    IERR = NF_INQ_VARID( FNUM, 'KFSDATE', LINDX3( FID ) )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading netCDF ID for variable LEVEL-COUNT' )
        EFLAG = .TRUE.
        GO TO  999        !  return
    END IF              !  ierr nonzero:  NF_INQ_VARID() failed

    IERR = NF_INQ_VARID( FNUM, 'KFSTIME', XINDX3( FID ) )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading netCDF ID for variable KFSDATE' )
        EFLAG = .TRUE.
        GO TO  999        !  return
    END IF              !  ierr nonzero:  NF_INQ_VARID() failed

    IERR = NF_INQ_VARID( FNUM, 'KFLNGTH', YINDX3( FID ) )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading netCDF ID for variable KFLNGTH' )
        EFLAG = .TRUE.
        GO TO  999        !  return
    END IF              !  ierr nonzero:  NF_INQ_VARID() failed


    !!.......   Variables-list for the file:

    IF ( NVARS3( FID ) .GT. 0 ) THEN
        IERR = NF_GET_ATT_TEXT( FNUM, NF_GLOBAL, 'VAR-LIST', VLIST3( 1,FID ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading attribute VAR-LIST' )
            EFLAG = .TRUE.
            GO TO  999        !  return
        END IF              !  ierr nonzero:  operation failed
    END IF              !  if nvars > 0

    DO  VAR = 1 , NVARS3( FID )

        IERR = NF_INQ_VARID( FNUM, VLIST3( VAR,FID ), VINDX3( VAR,FID ) )
        IF ( IERR .NE. 0 ) THEN
            MESG = 'Error reading ID for netCDF variable ' // VLIST3( VAR,FID )
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, MESG )
            EFLAG = .TRUE.
            GO TO  999        !  return
        END IF              !  ierr nonzero:  NF_INQ_VARID() failed

        IERR = NF_INQ_VAR( FNUM, VINDX3( VAR,FID ), TNAME, VTYPE3( VAR,FID ), ND, D, NA )
        IF ( IERR .NE. 0 ) THEN
            MESG = 'Error reading type for netCDF variable '
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, MESG )
            EFLAG = .TRUE.
            GO TO  999        !  return
        END IF              !  ierr nonzero:  NF_INQ_VARID() failed

        IERR = NF_GET_ATT_TEXT( FNUM, VINDX3( VAR,FID ), 'units', UNITS3( VAR,FID ) )
        IF ( IERR .NE. 0 ) THEN
            MESG = 'Error reading units for netCDF variable '
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, MESG )
            EFLAG = .TRUE.
            GO TO  999        !  return
        END IF              !  ierr nonzero:  NF_INQ_VARID() failed

    END DO          !  end loop on variables


    !!.......   If opened for WRITE:  put attribute HISTORY:  update description

    IF ( IAND( FSTATUS, NF_WRITE ) .NE. 0 ) THEN

        IERR = NF_REDEF( FNUM )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error opening history for update' )
            EFLAG = .TRUE.
            GO TO  999        !  return
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_PUT_ATT_TEXT( FNUM, NF_GLOBAL, 'UPNAM', NAMLEN3, PGNAME )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error writing netCDF file attribute UPNAM' )
            EFLAG = .TRUE.
            GO TO  999        !  return
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_PUT_ATT_TEXT( FNUM, NF_GLOBAL, 'HISTORY', MXDLEN3 * MXDESC3, SCNDSC )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error writing netCDF file attribute HISTORY' )
            EFLAG = .TRUE.
            GO TO  999        !  return
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_ENDDEF( FNUM )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error flushing history to disk' )
            EFLAG = .TRUE.
            GO TO  999        !  return
        END IF          !  ierr nonzero:  operation failed

    END IF

    DIMS ( 1 ) = 1
    DELTS( 1 ) = NCOLS3( FID )

    DIMS ( 2 ) = 1
    DELTS( 2 ) = NROWS3( FID )

    IERR = NF_GET_VARA_INT( FNUM, NINDX3( FID ), DIMS, DELTS, BUFFER )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading variable KFCOUNT' )
        EFLAG = .TRUE.
        GO TO  999        !  return
    END IF          !  ierr nonzero:  NF_GET_VARA_*() failed, or succeeded

    OPNKF = .TRUE.        !  (if you get to here)

999     CONTINUE

!$OMP END CRITICAL( S_NC )

    OPNKF = ( .NOT. EFLAG )

    RETURN

END FUNCTION  OPNKF

