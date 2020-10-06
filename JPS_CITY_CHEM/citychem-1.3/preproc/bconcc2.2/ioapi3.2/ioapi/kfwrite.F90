
INTEGER FUNCTION KFWRITE( FNAME, COL, ROW, JDATE, JTIME, KFLEN, BUFFER )    &
        RESULT( KFFLAG )

    !!***********************************************************************
    !! Version "$Id: kfwrite.F90 1 2017-06-10 18:05:20Z coats $"
    !! EDSS/Models-3 I/O API.
    !! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    !! (C) 2003-2013 Baron Advanced Meteorological Systems,
    !! (C) 2007-2013 Carlie J. Coats, Jr., and
    !! (C) 2014-2015 UNC Institute for the Environment.
    !! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    !! See file "LGPL.txt" for conditions of use.
    !!.........................................................................
    !!  function body starts at line  114
    !!
    !!  FUNCTION:  writes all the data from BUFFER() for for the event at
    !!       COL:ROW:JDATE:JTIME and having duration KFLEN (formatted YYYYDDD
    !!       and HHMMSS) to the KF-Cloud Event file with logical name FNAME.
    !!
    !!  RETURN VALUE:  record number at which the event written, or -1 for failure
    !!
    !!  PRECONDITIONS REQUIRED:  FNAME is a KF-Cloud Event file already opened
    !!                 for write access by KFOPEN()
    !!
    !!  POSTCONDITIONS REQUIRED:  subsequent call to SHUT3() to flush the
    !!                 file to disk.
    !!
    !!  SUBROUTINES AND FUNCTIONS CALLED:
    !!                 INDEX1, INIT3
    !!
    !!  REVISION  HISTORY:
    !!      Adapted   4/1996 by CJC from WRITE3()
    !!
    !!      Modified  5/1998 by CJC for OpenMP thread-safety
    !!
    !!      Modified  5/1999 by ALT for coupling-mode operation
    !!
    !!      Modified  9/1999 by CJC unification with KFOPEN()
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

    CHARACTER*(*), INTENT(IN   ) ::  FNAME      !  logical file name
    INTEGER      , INTENT(IN   ) ::  COL        !  column number for this event
    INTEGER      , INTENT(IN   ) ::  ROW        !  row    number for this event
    INTEGER      , INTENT(IN   ) ::  JDATE      !  date, formatted YYYYDDD
    INTEGER      , INTENT(IN   ) ::  JTIME      !  date, formatted HHMMSS
    INTEGER      , INTENT(IN   ) ::  KFLEN      !  time, formatted HHMMSS
    REAL         , INTENT(IN   ) ::  BUFFER(*)  !  output buffer array


    !!...........   EXTERNAL FUNCTIONS and their descriptions:

    EXTERNAL          :: INITBLK3        !!  BLOCK DATA to initialize STATE3 commons

    !!...........   MACHINE DEPENDENCY !!

#if _CRAY || REAL8
    INTEGER, PARAMETER :: TYPSIZE( 10 ) = (/ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 /)
#endif
#if ! ( _CRAY || REAL8 )
    INTEGER, PARAMETER :: TYPSIZE( 10 ) = (/ 1, 1, 1, 1, 1, 2, 1, 1, 1, 2 /)
#endif


    !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER       IDUM            !  holds return value for INIT3()
    INTEGER       CREC            !  record number for this cell
    INTEGER       IREC            !  record number for file
    INTEGER       FID             !  file-subscript for STATE3 arrays
    INTEGER       FNUM            !  CDFID3( FID )
    INTEGER       INDX            !  subscript into BUFFER( * )
    INTEGER       V               !  loop counter:  variables
    INTEGER       VTYPE, VINDX
    INTEGER       DIMS ( 5 )      !  corner   for NF_GET_VARA_*()
    INTEGER       DELS ( 5 )      !  diagonal for NF_GET_VARA_*()
    INTEGER       TFLAG( 5 )      !  tuple:  COL-ROW-DATE-TIME-DURATION
    INTEGER       IERR            !  netCDF error status return
    CHARACTER*16  FIL16           !  scratch file-name     buffer
    CHARACTER*256 MESG
    LOGICAL       EFLAG


    !!***********************************************************************
    !!   begin body of function  KFWRITE

    !!.......   Check that Models-3 I/O has been initialized:

    EFLAG = .FALSE.
!$OMP   CRITICAL( S_INIT )
    IF ( .NOT. FINIT3 ) THEN
        IDUM = INIT3()
        EFLAG   = .TRUE.
    END IF          !  if not FINIT3
!$OMP   END CRITICAL( S_INIT )
    IF ( EFLAG ) THEN
        CALL M3MSG2( 'KFWRITE: I/O API not yet initialized.' )
        KFFLAG = -1
        RETURN
    END IF


    !!.......   Find netCDF index for the file, and check time step availability:

    IF ( LEN_TRIM( FNAME ) .GT. NAMLEN3 ) THEN
        MESG = 'File "'// FNAME// '"'
        CALL M3MSG2( MESG )
        WRITE( MESG, '( 3( A , :, I5, :, 2X ) )' ) 'Max file name length 16; actual:', LEN_TRIM( FNAME )
        CALL M3WARN( 'KFWRITE', 0, 0, MESG )
        KFFLAG = -1
        RETURN
    END IF

    FIL16 = FNAME   !  fixed-length-16 scratch copy of name
    FID   = INDEX1( FIL16, COUNT3, FLIST3 )

    IF ( FID .EQ. 0 ) THEN  !  file not available
        MESG = 'File:  '//FIL16// ' not yet opened.'
        CALL M3WARN( 'KFWRITE', 0, 0, MESG )
        KFFLAG = -1
        RETURN
    ELSE IF ( FTYPE3( FID ) .NE. KFEVNT3 ) THEN
        WRITE( MESG,'(A,I5)' ) 'File:  ' // FIL16 // ' has non-KF type ', FTYPE3( FID )
        CALL M3WARN( 'KFWRITE', 0, 0, MESG )
        RETURN
    ELSE IF ( RONLY3( FID ) ) THEN
        MESG = 'File:  '//FIL16// ' IS READ-ONLY.'
        CALL M3WARN( 'KFWRITE', 0, 0, MESG )
        KFFLAG = -1
        RETURN
    ELSE IF ( COL .GT. NCOLS3( FID ) .OR.   &
              ROW .GT. NROWS3( FID ) .OR.   &
              COL .LT. 1             .OR.   &
              ROW .LT. 1 ) THEN
        WRITE( MESG, '(A, A )' ) 'KFWRITE:  file ', FNAME
        CALL M3MSG2( MESG )
        WRITE( MESG, '( A, I9, A, I9, A )' ) 'Requested (col,row)=(', COL, ',',ROW, ')'
        CALL M3MSG2( MESG )
        WRITE( MESG, '( A, I9, A, I9, A )' ) 'File dimensions (ncols,nrows)=(', NCOLS3(FID), ',',NROWS3(FID), ')'
        CALL M3MSG2( MESG )
        CALL M3WARN( 'KFWRITE', 0, 0, 'Out-of-bounds request' )
        KFFLAG = -1
        RETURN
    END IF
    FNUM = CDFID3( FID )

!$OMP  CRITICAL( S_NC )

    IF ( VOLAT3( FID ) ) THEN     !  volatile file:  synch with disk

       IERR = NF_SYNC( FNUM )
       IF ( IERR .NE. 0 ) THEN
           WRITE( MESG, '( A, I10, 2X, A )' ) 'netCDF error number', IERR,  &
                'Error with disk synchronization for file:  ' // FIL16
           CALL M3WARN( 'KFWRITE', 0, 0, MESG )
           KFFLAG = -1
           GO TO  999        !  return from kfwrite()
        END IF      !  if NF_SYNC() error

    END IF          !  if file not opened, or if readonly, or if volatile

    DIMS( 1 ) = COL
    DIMS( 2 ) = ROW        

    IERR = NF_GET_VAR1_INT( FNUM,  NINDX3( FID ), DIMS, CREC )
    IF ( IERR .NE. 0 ) THEN     !  timestep flag not yet written
        CALL  KFRERR( FNAME, 'KFCOUNT', COL, ROW, IERR )
        KFFLAG = -1
        GO TO  999        !  return from kfwrite()
    END IF

    CREC = CREC + 1
    IREC = MXREC3( FID ) + 1

    IF ( CREC .GT. NTHIK3( FID ) ) THEN
        WRITE( MESG, '( 3 ( A, I10, :, 2X ), A, I9.7, ":", I6.6 )' )    &
                'Maximum event count ', NTHIK3( FID ),                  &
                'exceeded at column', COL, 'row', ROW,                  &
                'date and time', JDATE, JTIME

        CALL M3WARN( 'KFWRITE', 0, 0, MESG )
        KFFLAG = -1
        GO TO  999        !  return from kfwrite()
    END IF


    !!.......   Write the variables in the event-record:

    DIMS( 1 ) = 1
    DELS( 1 ) = NLAYS3( FID )

    DIMS( 2 ) = IREC
    DELS( 2 ) = 1

    INDX = 1
    DO   V = 1, NVARS3( FID )

        VTYPE = VTYPE3( V,FID )
        VINDX = VINDX3( V,FID )

        IF ( VTYPE .EQ. M3INT ) THEN
            IERR  = NF_PUT_VARA_INT(    FNUM, VINDX, DIMS, DELS, BUFFER( INDX ) )
        ELSE IF ( VTYPE .EQ. M3REAL ) THEN
            IERR  = NF_PUT_VARA_REAL(   FNUM, VINDX, DIMS, DELS, BUFFER( INDX ) )
        ELSE IF ( VTYPE .EQ. M3DBLE ) THEN
            IERR  = NF_PUT_VARA_DOUBLE( FNUM, VINDX, DIMS, DELS, BUFFER( INDX ) )
        ELSE IF ( VTYPE .EQ. M3INT8 ) THEN
            IERR  = NF_PUT_VARA_INT64(  FNUM, VINDX, DIMS, DELS, BUFFER( INDX ) )
        ELSE
            IERR = NF_EBADTYPE
        END IF
        IF ( IERR .NE. 0 ) THEN

            CALL  KFWERR( FNAME, VLIST3( V,FID ), COL, ROW, IERR )
!$OMP       CRITICAL( S_LOGOUT )
            WRITE( LOGDEV,* ) 'IOAPI ID    ', FID
            WRITE( LOGDEV,* ) 'netCDF ID   ', FNUM
            WRITE( LOGDEV,* ) 'vble        ', VINDX
            WRITE( LOGDEV,* ) 'dims array  ', DIMS
            WRITE( LOGDEV,* ) 'DELSs array ', DELS
            WRITE( LOGDEV,* ) 'offset      ', INDX
            WRITE( LOGDEV,* )
!$OMP       END CRITICAL( S_LOGOUT )

            KFFLAG = -1
            GO TO  999        !  return from kfwrite()

        END IF                  !  ierr nonzero:  operation failed

        INDX = INDX  +  NLAYS3( FID ) * TYPSIZE( VTYPE3( V,FID ) )

    END DO


    !!.......   Write TFLAG for the event-record:

    TFLAG( 1 ) = COL        !  grid column for this even
    TFLAG( 2 ) = ROW        !  grid row
    TFLAG( 3 ) = JDATE      !  starting date YYYYDDD
    TFLAG( 4 ) = JTIME      !  starting time HHMMSS
    TFLAG( 5 ) = KFLEN      !  duration      HHMMSS

    DIMS( 1 ) = 1
    DELS( 1 ) = 5

    DIMS( 2 ) = IREC
    DELS( 2 ) = 1

    IERR  = NF_PUT_VARA_INT( FNUM, TINDX3( FID ), DIMS, DELS, TFLAG  )

    IF ( IERR .NE. 0 ) THEN
        CALL  KFWERR( FNAME, 'TFLAG', COL, ROW, IERR )
        KFFLAG = -1
        GO TO  999        !  return from kfwrite()
   END IF


    !!.......   update KFCOUNT, KFEVENT, KFSDATE, KFSTIME, KFLNGTH:

    DIMS( 1 ) = CREC
    DIMS( 2 ) = COL
    DIMS( 3 ) = ROW

    IERR  = NF_PUT_VAR1_INT( FNUM, SINDX3( FID ), DIMS, IREC )
    IF ( IERR .NE. 0 ) THEN
        CALL  KFWERR( FNAME, 'KFEVENT', COL, ROW, IERR )
        KFFLAG = -1
        GO TO  999        !  return from kfwrite()
   END IF

    IERR  = NF_PUT_VAR1_INT( FNUM, LINDX3( FID ), DIMS, JDATE )
    IF ( IERR .NE. 0 ) THEN
        CALL  KFWERR( FNAME, 'KFSDATE', COL, ROW, IERR )
        KFFLAG = -1
        GO TO  999        !  return from kfwrite()
   END IF

    IERR  = NF_PUT_VAR1_INT( FNUM, XINDX3( FID ), DIMS, JTIME )
    IF ( IERR .NE. 0 ) THEN
        CALL  KFWERR( FNAME, 'KFSTIME', COL, ROW, IERR )
        KFFLAG = -1
        GO TO  999        !  return from kfwrite()
   END IF

    IERR  = NF_PUT_VAR1_INT( FNUM, YINDX3( FID ), DIMS, KFLEN )
    IF ( IERR .NE. 0 ) THEN
        CALL  KFWERR( FNAME, 'KFLNGTH', COL, ROW, IERR )
        KFFLAG = -1
        GO TO  999        !  return from kfwrite()
   END IF


    !!.......   Update KFCOUNT:

    DIMS( 1 ) = COL
    DIMS( 2 ) = ROW

    IERR  = NF_PUT_VAR1_INT( FNUM, NINDX3( FID ), DIMS, CREC )
    IF ( IERR .NE. 0 ) THEN
        CALL  KFWERR( FNAME, 'KFCOUNT', COL, ROW, IERR )
        KFFLAG = -1
        GO TO  999        !  return from kfwrite()
    END IF

    MXREC3( FID ) = IREC
    KFFLAG       = IREC

999 CONTINUE        !  target of "exit from routine"

    IF ( VOLAT3( FID ) ) THEN     !  volatile file:  synch with disk

       IERR = NF_SYNC( FNUM )
       IF ( IERR .NE. 0 ) THEN
           WRITE( MESG, '( A, I10, 2X, A )' ) 'netCDF error number', IERR, 'with disk synchronization for file:  ' // FIL16
           CALL M3WARN( 'KFWRITE', 0, 0, MESG )
           KFFLAG = -1
           GO TO  999        !  return from kfwrite()
        END IF      !  if NF_SYNC() error

    END IF          !  if file is volatile

!$OMP END CRITICAL( S_NC )

    RETURN


CONTAINS


    SUBROUTINE KFRERR( FNAME, VNAME, COL, ROW, IERR )
        CHARACTER(*), INTENT( IN ) :: FNAME, VNAME
        INTEGER     , INTENT( IN ) :: COL, ROW, IERR

        CHARACTER*256   MESG

        WRITE( MESG, '( A, I5, 2X, 5 A, I7, 2X, A, I7 )' )          &
           'Error ', IERR, 'writing ', VNAME, ' to file ', FNAME,   &
           'at col', COL, 'row', ROW
        CALL M3WARN( 'KFWRITE', 0, 0, MESG )

        RETURN
    END SUBROUTINE KFRERR


    SUBROUTINE KFWERR( FNAME, VNAME, COL, ROW, IERR )
        CHARACTER(*), INTENT( IN ) :: FNAME, VNAME
        INTEGER     , INTENT( IN ) :: COL, ROW, IERR

        CHARACTER*256   MESG

        WRITE( MESG, '( A, I5, 2X, 5 A, I7, 2X, A, I7 )' )          &
           'Error ', IERR, 'writing ', VNAME, ' to file ', FNAME,   &
           'at col', COL, 'row', ROW
        CALL M3WARN( 'KFWRITE', 0, 0, MESG )

        RETURN
    END SUBROUTINE KFWERR


END FUNCTION  KFWRITE

