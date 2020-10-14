
LOGICAL FUNCTION KFINDX( FNAME, COL, ROW, ECOUNT, SDATES, STIMES, KFLENS, EVENTS )  &
        RESULT( KFFLAG )

    !!***********************************************************************
    !! Version "$Id: kfindx.f90 1 2017-06-10 18:05:20Z coats $"
    !! EDSS/Models-3 I/O API.
    !! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    !! (C) 2003-2013 Baron Advanced Meteorological Systems,
    !! (C) 2007-2013 Carlie J. Coats, Jr., and
    !! (C) 2014-2015 UNC Institute for the Environment.
    !! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    !! See file "LGPL.txt" for conditions of use.
    !!.........................................................................
    !!  function body starts at line  82
    !!
    !!  FUNCTION:  reads the event descriptions for the indicated cell
    !! 	from the KF-Cloud-Event file with logical name FNAME.
    !!
    !!  RETURN VALUE:  TRUE iff the operation succeeds (and the data is available)
    !!
    !!  PRECONDITIONS REQUIRED:  FNAME is a  KF-Cloud-Event file already
    !!       opened by kfindx()
    !!
    !!  REVISION  HISTORY:
    !!      Adapted  4/1996 by CJC from READ3().
    !!
    !!      Modified  5/1998 by CJC for OpenMP thread-safety
    !!
    !!      Modified  1/2002 by CJC:  check TRIMLEN() of FNAME
    !!
    !!      Modified 7/2003 by CJC:  bugfix -- clean up critical sections
    !!      associated with INIT3()
    !!
    !!      Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !!
    !!      Modified 02/2015 by CJC for I/O API 3.2: USE M3UTILIO
    !!
    !!      Modified 10/2015 by CJC for I/O API 3.2: USE MODNCFIO,
    !!      use NF_*() interfaces.  F90 "free" source-format
    !!***********************************************************************

    USE M3UTILIO
    USE MODNCFIO

    IMPLICIT NONE

    !!...........   INCLUDES:

    INCLUDE 'STATE3.EXT'


    !!...........   ARGUMENTS and their descriptions:

    CHARACTER*(*), INTENT(IN   ) :: FNAME      !  logical file name
    INTEGER      , INTENT(IN   ) :: COL        !  column number for this event
    INTEGER      , INTENT(IN   ) :: ROW        !  row    number for this event
    INTEGER      , INTENT(  OUT) :: ECOUNT     !  # of events for this col-row
    INTEGER      , INTENT(  OUT) :: SDATES(*)  !  starting date,  formatted YYYYDDD
    INTEGER      , INTENT(  OUT) :: STIMES(*)  !  starting time,  formatted HHMMSS
    INTEGER      , INTENT(  OUT) :: KFLENS(*)  !  event duration, formatted HHMMSS
    INTEGER      , INTENT(  OUT) :: EVENTS(*)  !  event numbers


    !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         FID             !  subscript  for STATE3 arrays
    INTEGER         FNUM            !  CDFID3( FID )
    INTEGER         IERR            !  netCDF error status return
    INTEGER         DIMT( 5 )       !  corner   for NF_GET_VARA_*()
    INTEGER         DELT( 5 )       !  diagonal for NF_GET_VARA_*()
    CHARACTER*16    FIL16           !  scratch file-name buffer
    CHARACTER*256   MESG            !  scratch message buffer
    LOGICAL         EFLAG


    !!***********************************************************************
    !!   begin body of function  KFINDX

    !!.......   Check that Models-3 I/O has been initialized:

    EFLAG = .FALSE.
!$OMP   CRITICAL( S_INIT )
    IF ( .NOT. FINIT3 ) THEN
        LOGDEV = INIT3()
        EFLAG   = .TRUE.
    END IF          !  if not FINIT3
!$OMP   END CRITICAL( S_INIT )
    IF ( EFLAG ) THEN
        CALL M3MSG2(  'KFINDX:  I/O API not yet initialized.' )
        KFFLAG = .FALSE.
        RETURN
    END IF

    IF ( LEN_TRIM( FNAME ) .GT. 16 ) THEN
        MESG = 'File "'// FNAME //'"'
        CALL M3MSG2( MESG )
        WRITE( MESG, '( A, I10 )' ) 'Max file name length 16; actual:', LEN_TRIM( FNAME )
        CALL M3WARN( 'KFINDX', 0,0, MESG )
        KFFLAG = .FALSE.
        RETURN
    END IF          !  if len( fname ) > 16, or if len( vname ) > 16


    !!.......   Find netCDF index for the file, and check time step availability:

    FIL16 = FNAME   !  fixed-length-16 scratch copy of name
    FID   = INDEX1( FIL16, COUNT3, FLIST3 )

    IF ( FID .EQ. 0 ) THEN  !  file not available
        MESG = 'File:  '//FIL16// ' not yet opened.'
        CALL M3WARN( 'KFINDX', 0,0, MESG )
        KFFLAG = .FALSE.
        RETURN
    END IF          !  if file not available

    !!.......   Check col/row for this cell:

    IF      ( COL .LT. 1 .OR. COL .GT. NCOLS3( FID ) ) THEN
        WRITE( MESG,91010 )                             &
                'Column requested:    ', COL,           &
                'out of range; max is ', NCOLS3( FID )
        CALL M3WARN( 'KFINDX', 0,0, MESG )
        KFFLAG = .FALSE.
        RETURN
    ELSE IF ( ROW .LT. 1 .OR. ROW .GT. NROWS3( FID ) ) THEN
        WRITE( MESG,91010 )                             &
                'Row requested:       ', ROW,           &
                'out of range; max is ', NROWS3( FID )
        CALL M3WARN( 'KFINDX', 0,0, MESG )
        KFFLAG = .FALSE.
        RETURN
    END IF

    FNUM = CDFID3( FID )

!$OMP   CRITICAL( S_NC )

    IF ( VOLAT3( FID ) ) THEN     !  volatile file:  synch with disk

        IERR = NF_SYNC( CDFID3( FID ) )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG,91010 ) 'netCDF error number', IERR,    &
                'Error with disk synchronization for file:  ' // FIL16
            CALL M3WARN( 'KFINDX', 0,0, MESG )
            KFFLAG = .FALSE.
            GO TO  999        !  return from kfindx()
        END IF      !  if ncsnc() failed

    END IF          !  if file not available, or if file is volatile


    !!.......   Read number of events for this cell:

    DIMT( 1 ) = COL
    DIMT( 2 ) = ROW
    IERR = NF_GET_VAR1_INT( FNUM,  NINDX3( FID ), DIMT, ECOUNT )
    IF ( IERR .NE. 0 ) THEN

        WRITE( MESG,91010 ) 'Requested col:  ', COL, 'Requested row:  ', ROW
        CALL M3MESG( MESG )
        WRITE( MESG,91010 ) 'netCDF error number', IERR, 'netCDF ID:  ', FNUM
        CALL M3MESG( MESG )
        MESG = 'Error reading KFCOUNT from ' // FIL16
        CALL M3WARN( 'KFINDX', 0,0, MESG )
        KFFLAG = .FALSE.
        GO TO  999        !  return from kfindx()

    END IF


    !!.......   Read the event-specification parameters for ths cell:

    DIMT( 1 ) = 1
    DELT( 1 ) = ECOUNT
    DIMT( 2 ) = COL
    DELT( 2 ) = 1
    DIMT( 3 ) = ROW
    DELT( 3 ) = 1

    IERR = NF_GET_VARA_INT( FNUM, SINDX3( FID ), DIMT, DELT, EVENTS )
    IF ( IERR .NE. 0 ) THEN     !  timestep flag not yet written

        WRITE( MESG,91010 ) 'Requested col:  ', COL, 'Requested row:  ', ROW
        CALL M3MESG( MESG )
        WRITE( MESG,91010 ) 'netCDF error number', IERR, 'netCDF ID:  ', FNUM
        CALL M3MESG( MESG )
        MESG = 'Error reading KFEVENT from ' // FIL16
        CALL M3WARN( 'KFINDX', 0,0, MESG )
        KFFLAG = .FALSE.
        GO TO  999        !  return from kfindx()

    END IF          !  if ierr bad or if timestep flags bad

    IERR = NF_GET_VARA_INT( FNUM, LINDX3( FID ), DIMT, DELT, SDATES )
    IF ( IERR .NE. 0 ) THEN     !  timestep flag not yet written

        WRITE( MESG,91010 ) 'Requested col:  ', COL, 'Requested row:  ', ROW
        CALL M3MESG( MESG )
        WRITE( MESG,91010 ) 'netCDF error number', IERR, 'netCDF ID:  ', FNUM
        CALL M3MESG( MESG )
        MESG = 'Error reading KFSDATE from ' // FIL16
        CALL M3WARN( 'KFINDX', 0,0, MESG )
        KFFLAG = .FALSE.
        GO TO  999        !  return from kfindx()

    END IF          !  if ierr bad or if timestep flags bad

    IERR = NF_GET_VARA_INT( FNUM, XINDX3( FID ), DIMT, DELT, STIMES )
    IF ( IERR .NE. 0 ) THEN     !  timestep flag not yet written

        WRITE( MESG,91010 ) 'Requested col:  ', COL, 'Requested row:  ', ROW
        CALL M3MESG( MESG )
        WRITE( MESG,91010 ) 'netCDF error number', IERR, 'netCDF ID:  ', FNUM
        CALL M3MESG( MESG )
        MESG = 'Error reading KFSTIME from ' // FIL16
        CALL M3WARN( 'KFINDX', 0,0, MESG )
        KFFLAG = .FALSE.
        GO TO  999        !  return from kfindx()

    END IF          !  if ierr bad or if timestep flags bad

    IERR = NF_GET_VARA_INT( FNUM, YINDX3( FID ), DIMT, DELT, KFLENS )
    IF ( IERR .NE. 0 ) THEN     !  timestep flag not yet written

        WRITE( MESG,91010 ) 'Requested col:  ', COL, 'Requested row:  ', ROW
        CALL M3MESG( MESG )
        WRITE( MESG,91010 ) 'netCDF error number', IERR, 'netCDF ID:  ', FNUM
        CALL M3MESG( MESG )
        MESG = 'Error reading KFLNGTH from ' // FIL16
        CALL M3WARN( 'KFINDX', 0,0, MESG )
        KFFLAG = .FALSE.
        GO TO  999        !  return from kfindx()

    END IF          !  if ierr bad or if timestep flags bad

    KFFLAG = .TRUE.

999     CONTINUE        !  target of "exit from routine"

!$OMP   END CRITICAL( S_NC )

    RETURN

    !!******************  FORMAT  STATEMENTS   ******************************

    !!...........   Error and warning message formats..... 91xxx

91010   FORMAT ( 5 ( A , :, I5, :, 2X ) )

END FUNCTION KFINDX

