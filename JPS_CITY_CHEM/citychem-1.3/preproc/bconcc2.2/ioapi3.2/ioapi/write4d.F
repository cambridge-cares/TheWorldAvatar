
        LOGICAL FUNCTION WRITE4D( FNAME, VNAME,
     &                            JDATE, JTIME, TSTEP, NRECS,
     &                            BUFFER )
     &                    RESULT( WFLAG )

C***********************************************************************
C Version "$Id: write4d.F 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2013 Baron Advanced Meteorological Systems,
C (C) 2007-2013 Carlie J. Coats, Jr., and
C (C) 2014-2015 UNC Institute for the Environment.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  133
C
C  FUNCTION:
C       writes all the data from BUFFER() for timestep sequence
C       JDATE:JTIME:TSTEP:NRECS (formatted YYYYDDD and HHMMSS)
C       to the time-stepped Models-3 data file with logical name FNAME.
C
C  RETURN VALUE:  TRUE iff the operation succeeds
C
C  PRECONDITIONS REQUIRED:
C        FNAME is a disk-based time-stepped Models-3 data file
C        of type GRDDED3, BNDARY3, CUSTOM3, or TSERIES3 that is
C        already opened for write access by OPEN3().
C        Does NOT support ALLVARS3.
C        Storage order for BUFFER is according to Fortran storage
C        order for subscript tuple  ( ..., TIMESTEPS )
C
C  POSTCONDITIONS REQUIRED:
C        subsequent call to SHUT3() to flush the file to disk.
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C        ENVYN, INDEX1, INIT3, JSTEP3, TRIMLEN, WRVARS
C
C  REVISION  HISTORY:
C       Adapted   9/1999 by CJC from WRITE3()
C
C       Modified  1/2002 by CJC:  check TRIMLEN() of FNAME
C
C       Modified 7/2003 by CJC:  bugfix -- clean up critical sections
C       associated with INIT3()
C
C       Bug-fix 2/2005 due to Alex Zubrow (U. Chicago):  need to return
C       WRITE4D = .TRUE. for success.
C
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C
C       Modified 02/2015 by CJC for I/O API 3.2: USE M3UTILIO, MODNCFIO;
C       support for M3INT8 variables.  USE NF_*() interfaces
C***********************************************************************

        USE M3UTILIO
        USE MODNCFIO

        IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'STATE3.EXT'


C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*), INTENT(IN   ) :: FNAME      !  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME      !  logical file name
        INTEGER      , INTENT(IN   ) :: JDATE      !  starting date, formatted YYYYDDD
        INTEGER      , INTENT(IN   ) :: JTIME      !  startng time, formatted HHMMSS
        INTEGER      , INTENT(IN   ) :: TSTEP      !  time-step, formatted HHMMSS
        INTEGER      , INTENT(IN   ) :: NRECS      !  Number of steps to write
        REAL         , INTENT(IN   ) :: BUFFER(*)  !  output buffer array


C...........   EXTERNAL FUNCTIONS and their descriptions:

        LOGICAL, EXTERNAL :: WRVARS     !  write time step records


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER       IREC            !  record number:  loop counter
        INTEGER       IT              !  subscript for STEP
        INTEGER       IS              !  subscript for BUFFER
        INTEGER       IDUM            !  holds return value for INIT3()
        INTEGER       ITYP            !  holds file type
        INTEGER       FID             !  file-subscript for STATE3 arrays
        INTEGER       VID             !  vble-subscript for STATE3 arrays
        INTEGER       STEP            !  time step record number
        INTEGER       TFLAG( 2 )      !  ( JDATE=yyyyddd, JTIME=hhmmss )
        INTEGER       DELTA           !  d(INDX) / d(NCVGTcall)
        INTEGER       DIMS ( 5 )      !  corner arg array for NCVGT()
        INTEGER       DELTS( 5 )      !  corner arg array for NCVGT()
        INTEGER       IERR            !  netCDF error status return
        LOGICAL       EFLAG
        CHARACTER*16  FIL16           !  scratch file-name     buffer
        CHARACTER*16  VAR16           !  scratch variable-name buffer
        CHARACTER*256 MESG            !  for m3msg2


C...........   PARAMETERs Variables:

        CHARACTER*7, PARAMETER :: TYPNAMES( -3 : 7 ) =
     &     (/ 'UNKNOWN',        !  -3:  file type error
     &        'DGRAPH3',        !  -2:  known file types
     &        'CUSTOM3',        !  -1:    "
     &        'DCTNRY3',        !   0:    "
     &        'GRDDED3',        !   1:    "
     &        'BNDARY3',        !   2:    "
     &        'IDDATA3',        !   3:    "
     &        'PROFIL3',        !   4:    "
     &        'GRNEST3',        !   5:    "
     &        'SMATRX3',        !   6:    "
     &        'UNKNOWN' /)      !   7:    "

#if _CRAY || REAL8
        INTEGER, PARAMETER :: TYPSIZE( 10 ) =
     &        (/ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 /)
#endif
#if ! ( _CRAY || REAL8 )
        INTEGER, PARAMETER :: TYPSIZE( 10 ) =
     &        (/ 1, 1, 1, 1, 1, 2, 1, 1, 1, 2 /)
#endif


C***********************************************************************
C   begin body of function  WRITE4D

C.......   Check that Models-3 I/O has been initialized:

        EFLAG = .FALSE.
!$OMP   CRITICAL( S_INIT )
        IF ( .NOT. FINIT3 ) THEN
            LOGDEV = INIT3()
            WRITE( LOGDEV, '( 5X, A )' )
     &       'WARNING in WRITE4D: I/O API not yet initialized.'
            WFLAG = .FALSE.
            EFLAG  = .TRUE.
        END IF          !  if not FINIT3
!$OMP   END CRITICAL( S_INIT )
        IF ( EFLAG ) RETURN


C.......   Find netCDF index for the file, and check time step availability:

        EFLAG = .FALSE.
        IF ( LEN_TRIM( FNAME ) .GT. NAMLEN3 ) THEN
            EFLAG = .TRUE.
            MESG  = 'File "'// FNAME// '" Variable "'// VNAME//'"'
            CALL M3MSG2( MESG )
            WRITE( MESG, '( A , I10 )' )
     &          'Max file name length 16; actual:', LEN_TRIM( FNAME )
            CALL M3MSG2( MESG )
        END IF          !  if len( fname ) > 16

        IF ( LEN_TRIM( VNAME ) .GT. NAMLEN3 ) THEN
            EFLAG = .TRUE.
            MESG  = 'File "'// FNAME// '" Variable "'// VNAME//'"'
            CALL M3MSG2( MESG )
            WRITE( MESG, '( A, I10 )'  )
     &          'Max vble name length 16; actual:', LEN_TRIM( VNAME )
            CALL M3MSG2( MESG )
        END IF          !  if len( vname ) > 16

        IF ( EFLAG ) THEN
            MESG = 'Invalid variable or file name arguments'
            CALL M3WARN( 'WRITE4D', JDATE, JTIME, MESG )
            WFLAG = .FALSE.
            RETURN
        END IF

        VAR16 = VNAME   !  fixed-length-16 scratch copy of name
        FIL16 = FNAME   !  fixed-length-16 scratch copy of name
        FID   = INDEX1( FIL16, COUNT3, FLIST3 )

        IF ( FID .EQ. 0 ) THEN  !  file not available

            MESG = 'File:  '//FIL16// ' not yet opened.'
            CALL M3WARN( 'WRITE4D', JDATE, JTIME, MESG )
            WFLAG = .FALSE.
            RETURN

        ELSE IF ( RONLY3( FID ) ) THEN

            MESG = 'File:  '//FIL16// ' IS READ-ONLY.'
            CALL M3WARN( 'WRITE4D', JDATE, JTIME, MESG )
            WFLAG = .FALSE.
            RETURN

        ELSE IF ( CDFID3( FID ) .EQ. BUFFIL3 ) THEN     !  BUFFERED "file"

            MESG = 'File:  '//FIL16// ' is BUFFERED.'
            CALL M3WARN( 'WRITE4D', JDATE, JTIME, MESG )
            WFLAG = .FALSE.
            RETURN

        ELSE IF ( CDFID3( FID ) .EQ. VIRFIL3 ) THEN

            MESG = 'File:  '//FIL16// ' is VIRTUAL.'
            CALL M3WARN( 'WRITE4D', JDATE, JTIME, MESG )
            WFLAG = .FALSE.
            RETURN

        ELSE IF ( TSTEP3( FID ) .EQ. 0 ) THEN

            MESG = 'File:  '//FIL16// ' is time-independent.'
            CALL M3WARN( 'WRITE4D', JDATE, JTIME, MESG )
            WFLAG = .FALSE.
            RETURN

        ELSE IF ( TSTEP3( FID ) .LT. 0 ) THEN

            MESG = 'File:  '//FIL16// ' is RESTART, not time-stepped.'
            CALL M3WARN( 'WRITE4D', JDATE, JTIME, MESG )
            WFLAG = .FALSE.
            RETURN

        ELSE IF ( VOLAT3( FID ) ) THEN     !  volatile file:  synch with disk

!$OMP      CRITICAL( S_NC )
           IERR = nf_sync( CDFID3( FID ) )
!$OMP      END CRITICAL( S_NC )
           IF ( IERR .NE. 0 ) THEN

                WRITE( MESG,93010 )
     &              'netCDF error number', IERR,
     &              'with disk synchronization for file:  '// FIL16
     &
                CALL M3WARN( 'WRITE4D', JDATE, JTIME, MESG )
                WFLAG = .FALSE.
                RETURN

            END IF      !  if NCSNC() error

        END IF          !  if file not opened, or if readonly, or if volatile


        ITYP = FTYPE3( FID )
        IF ( ITYP .EQ. CUSTOM3 ) THEN

            DIMS ( 1 ) = 1
            DELTS( 1 ) = NCOLS3( FID )

            DIMS ( 2 ) = 1
            DELTS( 2 ) = NLAYS3( FID )

            DELTS( 3 ) = 1
            IT = 3

            DELTA = NCOLS3( FID ) * NLAYS3( FID )

        ELSE IF ( ITYP .EQ. GRDDED3 ) THEN

            DIMS ( 1 ) = 1
            DELTS( 1 ) = NCOLS3( FID )

            DIMS ( 2 ) = 1
            DELTS( 2 ) = NROWS3( FID )

            DIMS ( 3 ) = 1
            DELTS( 3 ) = NLAYS3( FID )

            DELTS( 4 ) = 1
            IT = 4

            DELTA = NCOLS3( FID ) * NROWS3( FID ) * NLAYS3( FID )

        ELSE IF ( ITYP .EQ. BNDARY3 ) THEN

            DELTA = 2 * NTHIK3( FID )
            DELTA = DELTA * ( NCOLS3( FID ) + NROWS3( FID ) + DELTA )

            DIMS ( 1 ) = 1
            DELTS( 1 ) = DELTA

            DIMS ( 2 ) = 1
            DELTS( 2 ) = NLAYS3( FID )

            DELTS( 3 ) = 1

            DELTA = DELTA * NLAYS3( FID )

        ELSE IF ( ITYP .EQ. TSRIES3 ) THEN

            DIMS ( 1 ) = 1
            DELTS( 1 ) = NCOLS3( FID )

            DIMS ( 2 ) = 1
            DELTS( 2 ) = NROWS3( FID )

            DIMS ( 3 ) = 1
            DELTS( 3 ) = NLAYS3( FID )

            DELTS( 4 ) = 1
            IT = 4

            DELTA = NCOLS3( FID ) * NROWS3( FID ) * NLAYS3( FID )

        ELSE

            IDUM = MIN( MAX( ITYP, -3 ), 7 )
            WRITE( MESG, '( A, I7, 1X, 3A )' )
     &      'File type=', ITYP, '(', TYPNAMES( IDUM ), ')'
            CALL M3MSG2( MESG )
            MESG = 'Filetype not CUSTOM3, GRDDED3, BNDARY3, or TSRIES3'
            CALL M3WARN( 'WRITE4D', JDATE, JTIME, MESG )

            WFLAG = .FALSE.
            RETURN

        END IF          !  if time step bad, or if file of type
                        !  custom, gridded, ..., or unknown.

C...........   Get ID for variable to be written.

        VID = INDEX1( VAR16, NVARS3( FID ) , VLIST3( 1,FID ) )

        IF ( VID .EQ. 0 ) THEN

            MESG = 'Variable ' // VAR16 //' not in file:  '//FIL16
            CALL M3WARN( 'WRITE4D', JDATE, JTIME, MESG )
            WFLAG = .FALSE.
            RETURN

        END IF


C...........   Compute record number for this time step.  Update
C...........   max step number if necessary.  Then read data
C...........   from file into BUFFER(), selecting by file type:

        TFLAG( 1 ) = JDATE
        TFLAG( 2 ) = JTIME
        STEP = JSTEP3( JDATE, JTIME,
     &                 SDATE3( FID ),
     &                 STIME3( FID ),
     &                 TSTEP3( FID ) )

        IF ( STEP .LT. 0 ) THEN

            CALL M3MSG2( MESG )
            WRITE( MESG, 93020 )
     &      'Requested date & time:', JDATE, JTIME
            CALL M3MSG2( MESG )
            WRITE( MESG, 93020 )
     &      'File starting date & time:', SDATE3( FID ), STIME3( FID ),
     &      'time step:', TSTEP3( FID )
            CALL M3MSG2( MESG )
            WFLAG = .FALSE.
            MESG = 'Time step error writing file:  ' // FIL16
            CALL M3WARN( 'WRITE4D', JDATE, JTIME, MESG )
            RETURN

        END IF

        IS = 1
        DO  IREC = 1, NRECS

            STEP = JSTEP3( TFLAG( 1 ),    TFLAG( 2 ),
     &                     SDATE3( FID ), STIME3( FID ),
     &                     TSTEP3( FID ) )

            DIMS( IT ) = STEP

            IF ( .NOT. WRVARS( FID,  VID,   TFLAG, STEP,
     &                         DIMS, DELTS, DELTA, BUFFER( IS ) ) ) THEN

                WRITE( MESG, '( 5A, I7.7, A, I6.6 )' )
     &              'Error writing "', VNAME, '" to "', FNAME,
     &              '" for date&time ', TFLAG(1), ':', TFLAG(2)
                CALL M3WARN( 'WRITE4D', TFLAG(1), TFLAG(2), MESG )
                WFLAG = .FALSE.
                RETURN

            END IF              !  if WRVARS failed

            !!  update state variable MXREC3D, and
            !!  increment time-flag and buffer-pointer:

            MXREC3( FID ) = MAX( MXREC3( FID ), ABS( STEP ) )
            IS = IS + DELTA * TYPSIZE( VTYPE3( VID,FID ) )
            CALL NEXTIME( TFLAG( 1 ), TFLAG( 2 ), TSTEP )

        END DO          !  end loop on output records.

        WRITE( MESG, '( A, I9, A, I6.6, A, I6.6, A, I10)' )
     &          TRIM( VAR16 ) // ' written to ' //
     &          TRIM( FIL16 ) // ' for sequence',
     &          JDATE, ':', JTIME, ':', TSTEP, ':', NRECS
        CALL M3MSG2( MESG )

        WFLAG = .TRUE.

        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Error and warning message formats..... 91xxx
C...........   Log message formats..... 92xxx
C...........   Internal buffering formats..... 93xxx

93010   FORMAT ( 5 ( A, :, I9, :, 2X ) )

93020   FORMAT ( A, :, I9, ':', I6.6, :, A, :, 2X, I6.6 )

        END FUNCTION WRITE4D

