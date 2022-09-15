
LOGICAL FUNCTION RDTFLAG( FID,VID, JDATE,JTIME, STEP, VERBOSE ) RESULT( RDFLAG )

    !!***********************************************************************
    !!EDSS/Models-3 I/O API.
    !!Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    !!(C) 2003-2013 Baron Advanced Meteorological Systems,
    !!(C) 2007-2013 Carlie J. Coats, Jr., and
    !!(C) 2014-2015 UNC Institute for the Environment.
    !!Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    !!See file "LGPL.txt" for conditions of use.
    !!.........................................................................
    !! function body   starts at line  141
    !! Entry INITSNOOP starts at line  437
    !!
    !! FUNCTION:
    !!      returns TRUE with STEP = record number for this time step
    !!      iff time step for (JDATE,JTIME) and variable with ID=VID is
    !!      available in file with ID=FID.
    !!
    !!      If FID is time independent, only VID is significant (but
    !!      not JDATE:JTIME).
    !!
    !!      If FID is a "list file-set" upon entry, returns the FID of the
    !!      appropriate "list" entry.
    !!
    !!      If VERBOSE, writes warning message when data not available.
    !!
    !!       If SNOOP is enabled:
    !!         - Initializes SNOOP-delay (secs) and max attempts by
    !!           calling entry INITSNOOP() or from environment
    !!           variables  SNOOPSECS3 and SNOOPTRY3
    !!         - If SNOOPSECS3 > 0, repeatedly tries to read the TFLAG if
    !!           end-of-file, for SNOOPTRY3 attempts at interval SNOOPSECS3 seconds.
    !!
    !! PRECONDITIONS REQUIRED:
    !!      FID is the file ID of either a "real" netCDF file or of a
    !!      "list" file; in either case, for a file already opened by
    !!      OPEN3().
    !!
    !!      VID is the ID for a valid variable in FID, or else
    !!      is -1 for "all variables".
    !!
    !!      For list-files, also returns the FID which contains the actual
    !!      time step.
    !!
    !! SUBROUTINES AND FUNCTIONS CALLED:
    !!      JSTEP3
    !!
    !! REVISION  HISTORY:
    !!      Adapted  3/2002 by CJC from READ3, READ4D, INTERP3, and CHECK3
    !!
    !!      Modified 8/2002 by CJC:  fixed JSTEP3 RESTART-file bug
    !!
    !!      Modified 7/2003 by CJC:  improved error-handling
    !!
    !!      Modified 10/2003 by CJC for I/O API version 3:  support for
    !!      native-binary BINFIL3 file type
    !!
    !!      Bug-Fix 11/2004 by CJC:  correct "timestep not available"
    !!      test & message for case that VID > 0.
    !!
    !!      Modified 11/2004 by CJC:  new "verbose-flag" argument
    !!
    !!      Modified 1/2007 by CJC:  improved error-messages; logic simplification
    !!
    !!      Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !!
    !!      Modified 02/2015 by CJC for I/O API 3.2: USE M3UTILIO, MODNCFIO
    !!      support for MPI/PnetCDF MPIGRD3 files; F90 free format
    !!
    !!      Version  11/2015 by CJC: replace MPI_OFFSET_KIND by hard-coded INTEGER(8)
    !!      because OpenMPI-1.4.x does not follow the MPOI "standard" competently.
    !!
    !!      Version  10/2016 by CJC: "snoop" functionality
    !!
    !!      Version  3/2019 by CJC: Normalize dates&times before testing
    !!      for equality, for compatibility with MEGAN usage of non-standard
    !!      dates&times
    !!***********************************************************************

    USE M3UTILIO
    USE MODNCFIO

    IMPLICIT NONE
    LOGICAL  INITSNOOP

    !!...........   INCLUDES:

    INCLUDE 'STATE3.EXT'


    !!...........   ARGUMENTS and their descriptions:

    INTEGER, INTENT(INOUT) :: FID             !  subscript for file in STATE3 arrays
    INTEGER, INTENT(IN   ) :: VID             !  subscript for vble in STATE3 arrays
    INTEGER, INTENT(IN   ) :: JDATE           !  date (YYYYDDD) for query
    INTEGER, INTENT(IN   ) :: JTIME           !  time (HHMMSS) for query
    INTEGER, INTENT(  OUT) :: STEP            !  time step record number
    LOGICAL, INTENT(IN   ) :: VERBOSE

    INTEGER, INTENT(IN   ) :: DELAY           !  delay (secs) for SNOOP-mode retries
    INTEGER, INTENT(IN   ) :: TRIES           !  max # of retry-attempts

    !!...........   EXTERNAL FUNCTIONS and their descriptions:

    INTEGER, EXTERNAL :: RDBFLAG         !  for BINFIL3 files
    INTEGER, EXTERNAL :: SLEEP3          !  sleep for N secs
    LOGICAL, EXTERNAL :: SYNCFID


    !!...........   SAVED LOCAL VARIABLES and their descriptions:

    INTEGER, SAVE :: SLEEPSECS = IMISS3   !  "snoop" sleep-delay
    INTEGER, SAVE :: SLEEPTRY  = -1       !  "snoop" max # of tries


    !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         F, I, V         !  loop counters over files, variables
    INTEGER         FLAG1, FLAG2    !  date:time scratch vbles
    INTEGER         IERR            !  netCDF error status return
    INTEGER         SCNT            !  current number of "snoop" tries
    INTEGER         DIMT( 5 )       !  corner   for NF_GET_VARA_*()
    INTEGER         DELT( 5 )       !  diagonal for NF_GET_VARA_*()
    INTEGER         FLAGS( 2,MXVARS3 )!  flags from NF_GET_VARA_*()
    LOGICAL         EFLAG
    LOGICAL         SFLAG           !  is this a "snoop"?
    CHARACTER*16    FNAME, VNAME
    CHARACTER*256   MESG            !  buffer for building error messages         !  netCDF error status return

#ifdef IOAPI_PNCF
    INCLUDE 'mpif.h'

    INTEGER( 8 ) :: DIMP( 5 )       !  corner:    FLAGS
    INTEGER( 8 ) :: DELP( 5 )       !  diagonal:  FLAGS

!!  INTEGER( MPI_OFFSET_KIND ) :: DIMP( 5 )       !  corner:    FLAGS
!!  INTEGER( MPI_OFFSET_KIND ) :: DELP( 5 )       !  diagonal:  FLAGS
#endif

    !!***********************************************************************
    !!  begin body of function  RDTFLAG

    EFLAG = .FALSE.         !  no errors yet...

    !!.......   If first call, initialize SNOOP structures

#ifdef  IOAPI_SNOOP

    IF ( SLEEPSECS .EQ. IMISS3 ) THEN

        SLEEPSECS = ENVINT( 'SNOOPSECS3', 'Snoop delay (secs>0 to enable)', -1, IERR )
        IF ( IERR .GT. 0 ) THEN
            MESG = 'Bad environment variable "SNOOPSECS3"'
            CALL M3WARN('SNOOPTFLAG', JDATE, JTIME, MESG )
            EFLAG = .TRUE.
        ELSE IF ( SLEEPSECS > 0 ) THEN
            SLEEPTRY = ENVINT( 'SNOOPTRY3', 'Maximum number of snoop attempts', 10, IERR )
            IF ( IERR .GT. 0 ) THEN
                MESG = 'Bad environment variable "SNOOPTRY3"'
                CALL M3WARN('SNOOPTFLAG', JDATE, JTIME, MESG )
                EFLAG = .TRUE.
            ELSE IF ( SLEEPTRY .EQ. 0 ) THEN
                SLEEPTRY = 1999999999       !!  might as well be INTEGER*4 "infinity"
            END IF
        ELSE
            SLEEPTRY = -2
            CALL M3MESG( '"SNOOP" turned off for READ3/INTERP3' )
        END IF

        IF ( EFLAG ) THEN
            SLEEPSECS = -2
            SLEEPTRY  = -2
            RDFLAG    = .FALSE.
            RETURN
        END IF

    END IF          !  if sleepsecs < 0:  first call

#endif

    !!.......   If list file-set, find which actual file contains this time step:

    FNAME = FLIST3( FID )

    IF ( CDFID3( FID ) .EQ. VIRFIL3 ) THEN     !  virtual "file"

        CALL M3WARN( 'RDTFLAG', JDATE, JTIME, 'Bad call to RDTFLAG' )
        RDFLAG = .FALSE.
        RETURN

    END IF                  !  if cdfid3(fid) = lstfil3

    !!...........   Compute record number, and check availability:


    IF ( FTYPE3( FID ) .EQ. DCTNRY3 ) THEN

        STEP = VID
        IF ( STEP .GT. NVARS3( FID ) ) THEN
            WRITE( MESG, '(3A,I9)' ) 'Dictionary-file ', FNAME, ' does not contain variable-index', VID
            CALL M3WARN( 'RDTFLAG', JDATE, JTIME, MESG )
            RDFLAG = .FALSE.
            RETURN
        END IF

    ELSE

        STEP = JSTEP3( JDATE, JTIME, SDATE3( FID ), STIME3( FID ), ABS( TSTEP3( FID ) ) )

        IF ( STEP .LT. 0 ) THEN

            IF ( VERBOSE ) THEN
                WRITE( MESG,91020 ) 'Requested date & time:    ', JDATE, JTIME
                CALL M3MSG2( MESG )
                WRITE( MESG,91020 ) 'File starting date & time:', SDATE3( FID ), STIME3( FID )
                CALL M3MSG2( MESG )
                WRITE( MESG,91030 ) 'File time step:           ', TSTEP3( FID )
                CALL M3MSG2( MESG )
                MESG = 'Time step error for file:  '//FLIST3(FID)
                CALL M3WARN( 'RDTFLAG', JDATE, JTIME, MESG )
            END IF          !  if verbose
            RDFLAG = .FALSE.
            RETURN

        ELSE  IF ( CDFID3( FID ) .EQ. LSTFIL3 ) THEN     !  list "file set"

            DO  I = IFRST3(FID), IFRST3(FID) + NLIST3(FID) - 1
                F = ILIST3( I )
                IF ( BEGRC3( F ) .LE. STEP .AND. ENDRC3( F ) .GE. STEP ) THEN
                    FID  = F
                    STEP = STEP - BEGRC3( F ) + 1
                    GO TO  11
                END IF
            END DO

            !!  if you get to here:  data not available in this file-set

            WRITE( MESG,91020 ) 'Requested date & time:', JDATE, JTIME
            CALL M3MSG2( MESG )
            MESG = 'Time step not available in file-set ' // FNAME
            CALL M3WARN( 'RDTFLAG', JDATE, JTIME, MESG )
            RDFLAG = .FALSE.
            RETURN

11          CONTINUE

        END IF          !  check on step number

        IF ( TSTEP3( FID ) .LT. 0 ) THEN
            STEP  = 1 + MOD( STEP - 1, 2 )
            FLAG1 = JDATE
            FLAG2 = JTIME
        ELSE IF ( TSTEP3( FID ) .GT. 0 ) THEN
            STEP  = STEP
            FLAG1 = JDATE
            FLAG2 = JTIME
        ELSE    ! tstep3( fid ) = 0
            FLAG1 = 0
            FLAG2 = 0
        END IF
        CALL NEXTIME( FLAG1, FLAG2, 0 )

    END IF          ! if dictionary-file, or not


    SCNT  = 1

33  CONTINUE        !  head of "snoop" loop


    IF ( VOLAT3( FID ) ) THEN      !  volatile file:  synch with disk

        IF ( .NOT. SYNCFID( FID ) ) THEN

            MESG = 'Error with disk synchronization for file:  ' // FNAME
            CALL M3WARN( 'RDTFLAG', JDATE, JTIME, MESG )
            RDFLAG = .FALSE.
            RETURN

        END IF              !  if synch failed

    END IF                  !  if file is volatile

    !!.......   Deal with netCDF, native-binary-layer BINFIL3 files:

    IF ( VID .GT. 0 ) THEN  !  reading just one variable

        DIMT( 2 ) = VID     !  variable-number
        DELT( 2 ) = 1       !  extent:  one variable (also loop count)

    ELSE            !  reading all variables

        DIMT( 2 ) = 1               !  initial variable-number
        DELT( 2 ) = MAX( 1, NVARS3( FID ) )   !  extent:  all variables

    END IF


    IF ( CDFID3( FID ) .EQ. BINFIL3 ) THEN       ! BINFIL3 file:

!$OMP CRITICAL( S_NC )
        IERR = RDBFLAG( FID, VID, STEP, FLAGS )
!$OMP END CRITICAL( S_NC )

        IF ( IERR .EQ. 0 ) THEN

            IF ( TSTEP3(FID) .NE. 0 .AND. SLEEPSECS .GT. 0  .AND.  SCNT .LT. SCNT ) THEN
                I    = SLEEP3( SLEEPSECS )
                SCNT = SCNT + 1
                GO TO  33
            END IF

            MESG = 'Error reading time-flags for BINIO3 file ' // FNAME
            EFLAG = .TRUE.

        END IF          !  if rdbflag() failed

    ELSE IF ( FTYPE3( FID ) .EQ. MPIGRD3 ) THEN     !  PnetCDF file:

#ifdef  IOAPI_PNCF
        DIMP( 1 ) = 1           !  field:  date or time
        DELP( 1 ) = 2           !  extent:  entire field
        DIMP( 3 ) = STEP        !  timestep dimension
        DELP( 3 ) = 1           !  extent in timestep dimension

!$OMP CRITICAL( S_NC )
        IERR = NFMPI_GET_VARA_INT( CDFID3( FID ), TINDX3( FID ), DIMT, DELT, FLAGS )
!$OMP END CRITICAL( S_NC )

        IF ( IERR .EQ. 8 ) THEN     !  timestep flag not yet written

            IF ( TSTEP3(FID) .NE. 0 .AND. SLEEPSECS .GT. 0  .AND.  SCNT .LT. SCNT ) THEN
                I    = SLEEP3( SLEEPSECS )
                SCNT = SCNT + 1
                GO TO  33
            END IF

            EFLAG = .TRUE.
            MESG  = 'Time step not yet written in pnetCDF file ' // FNAME

        ELSE IF ( IERR .NE. 0 ) THEN

            WRITE( MESG,91010 ) 'pnetCDF error number', IERR
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
            MESG  = 'Error reading pnetCDF time step flag for '// FNAME

        END IF
#endif

#ifndef  IOAPI_PNCF
        EFLAG = .TRUE.
        MESG  = 'MPI/PnetCDF I/O not enabled in this build'
#endif

    ELSE IF ( CDFID3( FID ) .GE. 0 ) THEN           !  netCDF file:

        DIMT( 1 ) = 1           !  field:  date or time
        DELT( 1 ) = 2           !  extent:  entire field
        DIMT( 3 ) = STEP        !  timestep dimension
        DELT( 3 ) = 1           !  extent in timestep dimension

!$OMP CRITICAL( S_NC )
        IERR = NF_GET_VARA_INT( CDFID3( FID ), TINDX3( FID ), DIMT, DELT, FLAGS )
!$OMP END CRITICAL( S_NC )

        IF ( IERR .EQ. 8 ) THEN     !  timestep flag not yet written

            IF ( TSTEP3(FID) .NE. 0 .AND. SLEEPSECS .GT. 0  .AND.  SCNT .LT. SCNT ) THEN
                I    = SLEEP3( SLEEPSECS )
                SCNT = SCNT + 1
                GO TO  33
            END IF

            EFLAG = .TRUE.
            MESG  = 'Time step not yet written in netCDF file ' // FNAME

        ELSE IF ( IERR .NE. 0 ) THEN

            WRITE( MESG,91010 ) 'netCDF error number', IERR
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
            MESG  = 'Error reading netCDF time step flag for '// FNAME

        END IF

    END IF          !  if netCDF file; else if BINIO3 file

    IF ( EFLAG ) THEN       !  errors

        IF ( VERBOSE ) THEN
            CALL M3WARN( 'RDTFLAG', JDATE, JTIME, MESG )
        END IF              !  if verbose

        RDFLAG = .FALSE.
        RETURN

    END IF                  !  if eflag


!!...........   Check time step flags for all variables:

    IF ( VID .GT. 0 ) THEN

        CALL NEXTIME( FLAGS( 1,1 ), FLAGS( 2,1 ), 0 )
        IF ( FLAGS( 1,1 ) .NE. FLAG1  .OR.          &
             FLAGS( 2,1 ) .NE. FLAG2  ) THEN


            VNAME = VLIST3( VID,FID )
            MESG  = 'Time step not available in file ' // FNAME // ' for variable ' // VNAME
            EFLAG = .TRUE.
            CALL M3WARN( 'RDTFLAG', JDATE, JTIME, MESG )

        END IF          !  if bad flag value

    ELSE

        DO  V = 1, DELT( 2 )

            CALL NEXTIME( FLAGS( 1,V ), FLAGS( 2,V ), 0 )
            IF ( FLAGS( 1,V ) .NE. FLAG1  .OR.      &
                 FLAGS( 2,V ) .NE. FLAG2  ) THEN

                VNAME = VLIST3( V,FID )
                MESG  = 'Time step not available in file ' // FNAME // ' for variable ' // VNAME
                EFLAG = .TRUE.
                CALL M3WARN( 'RDTFLAG', JDATE, JTIME, MESG )

            END IF          !  if bad flag value

        END DO

    END IF          !  if vid > 0, or not

    RDFLAG = ( .NOT.EFLAG )

    RETURN


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

    ENTRY INITSNOOP( DELAY, TRIES )

#ifdef  IOAPI_SNOOP

        IF ( DELAY .LE. 0 .OR. TRIES .LT. 0 ) THEN
            SLEEPSECS = -2
            SLEEPTRY  = -2
            MESG = 'RDTFLAG():  SNOOP turned off'
        ELSE IF IF ( TRIES .EQ. 0 ) THEN
            SLEEPSECS = DELAY
            SLEEPTRY  = 1999999999       !!  might as well be INTEGER*4 "infinity"
            WRITE( MESG, '( A, I9, 1X, A )' )       &
                'RDTFLAG():  Initializing SNOOP with DELAY=', DELAY
        ELSE
            SLEEPSECS = DELAY
            SLEEPTRY  = TRIES
            WRITE( MESG, '( A, I9, 1X, A, I9 )' )   &
                'RDTFLAG():  Initializing SNOOP with DELAY=', DELAY,    &
                '(secs) and TRIES=', TRIES
        END IF

        CALL M3MESG( '-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-' )
        CALL M3MESG( MESG )
        CALL M3MESG( '-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-' )

        INITSNOOP = .TRUE.

#endif

#ifndef  IOAPI_SNOOP
        CALL M3MESG( '"SNOOP" not enabled in this I/O API buiod.' )
        INITSNOOP = .FALSE.
#endif

    RETURN

    !!******************  FORMAT  STATEMENTS   ******************************

    !!...........   Error and warning message formats..... 91xxx

91010   FORMAT ( 3 ( A , :, I5, :, 2X ) )

91020   FORMAT ( A , I9, ':' , I6.6, :, A )

91030   FORMAT ( A , I6.6 )

END FUNCTION RDTFLAG

