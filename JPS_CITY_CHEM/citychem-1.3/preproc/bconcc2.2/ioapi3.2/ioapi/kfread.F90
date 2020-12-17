
LOGICAL FUNCTION KFREAD( FNAME, VNAME, EVENT,                       &
                         COL, ROW, JDATE, JTIME, KFLEN, VBLE )      &
     &  RESULT( KFFLAG )

    !!***********************************************************************
    !! Version "$Id: kfread.F90 1 2017-06-10 18:05:20Z coats $"
    !! EDSS/MoDELT-3 I/O API.
    !! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    !! (C) 2003-2010 by Baron Advanced Meteorological Systems.
    !! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    !! See file "LGPL.txt" for conditions of use.
    !!.........................................................................
    !!  function body starts at line  100
    !!
    !!  FUNCTION:  reads the indicated event from the KF-Cloud-Event file
    !!       with logical name FNAME for variable with name VNAME.
    !!       If VNAME = ALLVAR3 = 'ALL', reads all variables.
    !!
    !!  RETURN VALUE:  TRUE iff the operation succeeds (and the data is available)
    !!
    !!  PRECONDITIONS REQUIRED:  FNAME is a  KF-Cloud-Event file already
    !!       opened by KFOPEN()
    !!
    !!  REVISION  HISTORY:
    !!      Adapted   4/1996 by CJC from READ3().
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
    !!      Modified 10/2015 by CJC for I/O API 3.2: USE MODNCFIO;
    !!      support for M3INT8 variables; use NF_*() interfaces.
    !!      F90 "free" source-format
    !!***********************************************************************

    USE M3UTILIO
    USE MODNCFIO

    IMPLICIT NONE

    !!...........   INCLUDES:

    INCLUDE 'STATE3.EXT'


    !!...........   ARGUMENTS and their descriptions:

    CHARACTER*(*), INTENT(IN   ) :: FNAME      !  logical file name
    CHARACTER*(*), INTENT(IN   ) :: VNAME      !  variable name, or 'ALL'
    INTEGER      , INTENT(IN   ) :: EVENT      !  KF-cloud event number
    INTEGER      , INTENT(  OUT) :: COL        !  column number for this event
    INTEGER      , INTENT(  OUT) :: ROW        !  row    number for this event
    INTEGER      , INTENT(  OUT) :: JDATE      !  starting date, formatted YYYYDDD
    INTEGER      , INTENT(  OUT) :: JTIME      !  starting time, formatted HHMMSS
    INTEGER      , INTENT(  OUT) :: KFLEN      !  event duration, formatted HHMMSS
    REAL         , INTENT(  OUT) :: VBLE(*)    !  array of returned values for VNAME


    !!...........   MACHINE DEPENDENCY !!

#if _CRAY || REAL8
    INTEGER, PARAMETER :: TYPSIZE( 10 ) = (/ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 /)
#endif
#if ! ( _CRAY || REAL8 )
    INTEGER, PARAMETER :: TYPSIZE( 10 ) = (/ 1, 1, 1, 1, 1, 2, 1, 1, 1, 2 /)
#endif

    !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         FID             !  subscript  for STATE3 arrays
    INTEGER         TDIM            !  netCDF ID for record dimension
    INTEGER         VID, VAR        !  subscripts for STATE3 arrays
    INTEGER         VTYPE, VINDX
    INTEGER         FNUM            !  CDFID3( FID )
    INTEGER         INDX            !  subscript into VBLE( * )
    INTEGER         IERR            !  netCDF error status return
    INTEGER         DIMT( 5 )       !  corner   for NF_GET_VARA_*()
    INTEGER         DELT( 5 )       !  diagonal for NF_GET_VARA_*()
    INTEGER         FLAGS( 5 )      !  event COL-ROW-DATE-TIME-DURATION
    CHARACTER*16    FIL16           !  scratch file-name buffer
    CHARACTER*16    VAR16           !  scratch vble-name buffer
    CHARACTER*256   MESG
    CHARACTER*(MAXNCNAM) TNAME      !  dummy arg for NF_INQ_DIOM()
    LOGICAL EFLAG


    !!***********************************************************************
    !!   begin body of function  KFREAD

    !!.......   Check that MoDELT-3 I/O has been initialized:

    EFLAG = .FALSE.
!$OMP   CRITICAL( S_INIT )
    IF ( .NOT. FINIT3 ) THEN
        LOGDEV = INIT3()
        EFLAG  = .TRUE.
    END IF          !  if not FINIT3
!$OMP   END CRITICAL( S_INIT )
    IF ( EFLAG ) THEN
        CALL M3MSG2(  'KFREAD:  I/O API not yet initialized.' )
        KFFLAG = .FALSE.
        RETURN
    END IF

    IF ( LEN_TRIM( FNAME ) .GT. NAMLEN3 ) THEN
        EFLAG = .TRUE.
        MESG  = 'File "'// FNAME// '" Variable "'// VNAME//'"'
        CALL M3MSG2( MESG )
        WRITE( MESG, '( A , I10 )' ) 'Max file name length 16; actual:', LEN_TRIM( FNAME )
        CALL M3MSG2( MESG )
    END IF          !  if len( fname ) > 16

    IF ( LEN_TRIM( VNAME ) .GT. NAMLEN3 ) THEN
        EFLAG = .TRUE.
        MESG  = 'File "'// FNAME// '" Variable "'// VNAME//'"'
        CALL M3MSG2( MESG )
        WRITE( MESG, '( A, I10 )'  ) 'Max vble name length 16; actual:', LEN_TRIM( VNAME )
        CALL M3MSG2( MESG )
    END IF          !  if len( vname ) > 16

    IF ( EFLAG ) THEN
        MESG = 'Invalid variable or file name arguments'
        CALL M3WARN( 'KFREAD', 0, 0, MESG )
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
    ELSE IF ( FTYPE3( FID ) .NE. KFEVNT3 ) THEN
        WRITE( MESG,'( A, I5, 2X, A )' ) 'File type', FTYPE3( FID ), 'is not KFEVNT3=-3'
        CALL M3WARN( 'KFREAD', 0, 0, MESG )
        KFFLAG = .FALSE.
        RETURN
    END IF          !  if file not available

    FNUM = CDFID3( FID )

    !!.......   Check availability of requested  layer, variable:

    VAR16 = VNAME   !  fixed-length-16 scratch copy of name

    IF ( VAR16 .EQ. ALLVAR3 ) THEN
        VID = ALLAYS3
    ELSE
        VID = INDEX1 ( VAR16, NVARS3( FID ), VLIST3( 1,FID ) )

        IF ( VID .EQ. 0 ) THEN
            WRITE( MESG,91010 ) 'Requested variable ' //  VAR16 // 'not available in file ' // FLIST3( FID )
            CALL M3WARN( 'KFREAD', 0, 0, MESG )
            KFFLAG = .FALSE.
            RETURN
        END IF

        VTYPE = VTYPE3( FID,VID )
        VINDX = VINDX3( VAR,FID )

    END IF          !  end check on VNAME

!$OMP  CRITICAL( S_NC )

    IF ( VOLAT3( FID ) ) THEN

        !!  volatile file:  synch with disk,
        !!  Since such files may also be used as active channels
        !!  of communication between coupled moDELT, we must also
        !!  re-evaluate the maximum record-number in the file.

        IERR = NF_SYNC( FNUM )
        IF ( IERR .NE. 0 ) THEN

            WRITE( MESG,91010 )                         &
                'netCDF error number', IERR,            &
                'Error with disk synchronization for file:  ' // FIL16
            CALL M3WARN( 'KFREAD', 0, 0, MESG )
            KFFLAG = .FALSE.
            GO TO  999        !  return from kfread()

        END IF      !  if ncsnc() failed

        IERR = NF_INQ_DIMID( FNUM, 'TSTEP', TDIM )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( A, I5 )' ) 'netCDF error number', IERR
            CALL M3MSG2( MESG )
            MESG = 'Error reading netCDF time-dimension ID for file ' // FLIST3( FID )
            CALL M3WARN( 'KFREAD', 0, 0, MESG )
            KFFLAG = .FALSE.
            GO TO  999        !  return
        END IF              !  ierr nonzero:  operation failed

        IERR = NF_INQ_DIMLEN( FNUM, TDIM, MXREC3( FID ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MSG2( 'Error reading maximum timestep record number for ' // FLIST3( FID ) )
            WRITE( MESG, '( A, I5 )' ) 'netCDF error number', IERR
            CALL M3MSG2( MESG )
            MESG = 'Error reading maximum timestep record number ' // 'for file ' // FLIST3( FID )
            CALL M3WARN( 'KFREAD', 0, 0, MESG )
            KFFLAG = .FALSE.
            GO TO  999        !  return
        END IF              !  ierr nonzero:  operation failed

    END IF          !  if file not available, or if file is volatile


    !!.......   Check availability of requested  layer, variable:

    IF ( EVENT .GT. MXREC3( FID ) .OR. EVENT .LE. 0 ) THEN
        WRITE( MESG,91010 ) 'Event number', EVENT, 'not available in file ' // FIL16
        CALL M3WARN( 'KFREAD', 0, 0, MESG )
        KFFLAG = .FALSE.
        GO TO  999        !  return from kfread()
    END IF          !  end check on VNAME


    !!.......   Read contents of the variable(s):

    DIMT( 1 ) = 1
    DELT( 1 ) = NLAYS3( FID )

    DIMT( 2 ) = EVENT
    DELT( 2 ) = 1

    IF ( VID .GT. 0 ) THEN  !  reading just one variable

        IF (      VTYPE .EQ. M3INT  ) THEN
            IERR = NF_GET_VARA_INT(    FNUM, VINDX, DIMT, DELT, VBLE )
        ELSE IF ( VTYPE .EQ. M3REAL ) THEN
            IERR = NF_GET_VARA_REAL(   FNUM, VINDX, DIMT, DELT, VBLE )
        ELSE IF ( VTYPE .EQ. M3DBLE ) THEN
            IERR = NF_GET_VARA_DOUBLE( FNUM, VINDX, DIMT, DELT, VBLE )
        ELSE IF ( VTYPE .EQ. M3INT8 ) THEN
            IERR = NF_GET_VARA_INT64(  FNUM, VINDX, DIMT, DELT, VBLE )
        ELSE
            IERR = NF_EBADTYPE
        END IF
        IF ( IERR .NE. 0 ) THEN     !  variable not yet written

            WRITE( MESG,91010 ) 'netCDF error number', IERR,            &
                ' reading ' // VLIST3( VID,FID ) // ' from ' // FIL16
            CALL M3WARN( 'KFREAD', 0, 0, MESG )
            KFFLAG = .FALSE.
            GO TO  999        !  return from kfread()

        END IF          !  if ierr bad or if variable bad

    ELSE            !  reading all variables

        INDX = 1

        DO  VAR = 1, NVARS3( FID )

            VTYPE = VTYPE3( VAR,FID )
            VINDX = VINDX3( VAR,FID )
            IF (      VTYPE .EQ. M3INT  ) THEN
                IERR = NF_GET_VARA_INT(    FNUM, VINDX, DIMT, DELT, VBLE( INDX ) )
            ELSE IF ( VTYPE .EQ. M3REAL ) THEN
                IERR = NF_GET_VARA_REAL(   FNUM, VINDX, DIMT, DELT, VBLE( INDX ) )
            ELSE IF ( VTYPE .EQ. M3DBLE ) THEN
                IERR = NF_GET_VARA_DOUBLE( FNUM, VINDX, DIMT, DELT, VBLE( INDX ) )
            ELSE IF ( VTYPE .EQ. M3INT8 ) THEN
                IERR = NF_GET_VARA_INT64(  FNUM, VINDX, DIMT, DELT, VBLE( INDX ) )
            ELSE
                IERR = NF_EBADTYPE
            END IF
            IF ( IERR .NE. 0 ) THEN     !  variable not yet written

                WRITE( MESG,91010 )  'netCDF error number', IERR,       &
                    ' reading ' // VLIST3( VAR,FID ) // ' from ' // FIL16
                CALL M3WARN( 'KFREAD', 0, 0, MESG )
                KFFLAG = .FALSE.
                GO TO  999        !  return from kfread()

            END IF          !  if ierr bad or if variable bad

            INDX = INDX + NLAYS3( FID )*TYPSIZE( VTYPE )

        END DO

    END IF


    !!.......   Read record header:

    DIMT( 1 ) = 1
    DELT( 1 ) = 5

    DIMT( 2 ) = EVENT
    DELT( 2 ) = 1

    IERR = NF_GET_VARA_INT( FNUM, TINDX3( FID ), DIMT, DELT, FLAGS )
    IF ( IERR .NE. 0 ) THEN     !  record header not yet written

        WRITE( MESG,91010 ) 'netCDF error number', IERR, 'reading TFLAG from ' // FIL16
        CALL M3WARN( 'KFREAD', 0, 0, MESG )
        KFFLAG = .FALSE.
        GO TO  999        !  return from kfread()

    END IF          !  if ierr bad or if record header bad

    COL   = FLAGS( 1 )
    ROW   = FLAGS( 2 )
    JDATE = FLAGS( 3 )
    JTIME = FLAGS( 4 )
    KFLEN = FLAGS( 5 )

    KFFLAG = .TRUE.

999     CONTINUE        !  target of "exit from routine"

!$OMP END CRITICAL( S_NC )

    RETURN

        !!******************  FORMAT  STATEMENTS   ******************************

        !!...........   Error and warning message formats..... 91xxx

91010   FORMAT ( 3 ( A , :, I5, :, 2X ) )

END FUNCTION KFREAD

