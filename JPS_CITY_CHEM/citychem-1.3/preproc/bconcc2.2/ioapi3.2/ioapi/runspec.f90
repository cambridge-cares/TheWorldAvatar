
SUBROUTINE RUNSPEC( FNAME, USEENV, SDATE, STIME, TSTEP, NRECS )

    !!***********************************************************************
    !! Version "$Id: runspec.f90 1 2017-06-10 18:05:20Z coats $"
    !! EDSS/Models-3 I/O API.
    !! (C) 2015-2016 UNC Institute for the Environment.
    !! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    !! See file "LGPL.txt" for conditions of use.
    !!.........................................................................
    !!  subroutine body starts at line  82
    !!
    !!  FUNCTION:
    !!      Return time step sequence SDATE:STIME:TSTEP:NRECS
    !!      compatible with FNAME as an input file.
    !!      If FNAME is blank, get default SDATE:STIME from wall-clock
    !!      and use 
    !!      If USEENV, get SDATE, STIME, TSTEP, EDATE, ETIME from environment
    !!      Otherwise prompt the user for them.
    !!      NOTE:  May *NOT* assume that FNAME is already completed.
    !!      M3EXIT on failure.
    !!
    !!  PRECONDITIONS REQUIRED:
    !!      FNAME already opened.
    !!      If  USEENV:
    !!          setenv SDATE <starting date (YYYYDDD)>
    !!          setenv STIME <starting time  (HHMMSS)>
    !!          setenv TSTEP <time step      (H*MMSS)>
    !!          setenv EDATE <ending date   (YYYYDDD)>
    !!          setenv ETIME <ending date    (HHMMSS)>
    !!
    !!  REVISION  HISTORY:
    !!      Adapted 03/2015 by Carlie J. Coats, Jr., UNC IE, from algorithm
    !!      used in various "m3tools" programs
    !!      Bug-fix 05/2016 by CJC
    !!***********************************************************************

    IMPLICIT NONE

    INCLUDE 'PARMS3.EXT'
    INCLUDE 'FDESC3.EXT'
    INCLUDE 'IODECL3.EXT'

    !!...........   ARGUMENTS and their descriptions:

    CHARACTER(LEN=*), INTENT(IN   ) :: FNAME        !!  input file
    LOGICAL,          INTENT(IN   ) :: USEENV       !!  input file
    INTEGER,          INTENT(  OUT) :: SDATE        !!  starting date YYYYDDD
    INTEGER,          INTENT(  OUT) :: STIME        !!  starting time  H*MMSS
    INTEGER,          INTENT(  OUT) :: TSTEP        !!  time step      H*MMSS
    INTEGER,          INTENT(  OUT) :: NRECS        !!  Number of records

    !!...........   PARAMETERS and their descriptions:

    CHARACTER*1 , PARAMETER :: BLANK = ' '
    CHARACTER*16, PARAMETER :: PNAME = 'RUNSPEC'

    !!...........   EXTERNAL FUNCTIONS and their descriptions:

    LOGICAL, EXTERNAL :: ENVYN
    INTEGER, EXTERNAL :: ENVINT, GETNUM, JSTEP3, CURREC

    !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER     SDATE1, JDATE1
    INTEGER     STIME1
    INTEGER     EDATE1
    INTEGER     ETIME1
    INTEGER     TSTEP1
    INTEGER     NRECS1
    INTEGER     JDATE, JTIME
    INTEGER     EDATE, ETIME
    INTEGER     ISTAT

    LOGICAL     EFLAG

    CHARACTER*256   MESG

    !!--------------------------------------------------------------
    !!   begin body of program RUNSPEC

    IF ( FNAME .EQ. BLANK ) THEN

        CALL GETDTTIME( JDATE1, STIME1 )
        SDATE1 = -1
        STIME1 = 10000 * ( STIME1 / 10000 ) !!  truncate to hour
        TSTEP1 = 10000
        EDATE1 = 9999999
        ETIME1 = 9999999

    ELSE IF ( .NOT.DESC3( FNAME ) ) THEN

        MESG = 'Could not get description for "'//TRIM( FNAME )//'"'
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )

    ELSE IF ( TSTEP3D .EQ. 0 ) THEN   !!  time independent case

        SDATE = SDATE3D
        STIME = STIME3D
        TSTEP = 0
        NRECS = 1
        RETURN

    ELSE

        SDATE1 = SDATE3D
        STIME1 = STIME3D
        TSTEP1 = TSTEP3D
        NRECS1 = MXREC3D
        CALL LASTTIME( SDATE1,STIME1,TSTEP1,NRECS1, EDATE1,ETIME1 )

    END IF

    EFLAG  = .FALSE.

    IF ( USEENV ) THEN

        SDATE = ENVINT( 'SDATE', 'Starting date (YYYYDDD)', SDATE1, ISTAT )
        IF ( ISTAT .GT. 0 ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'Bad environment variable "SDATE"' )
        END IF

        STIME = ENVINT( 'STIME', 'Starting time (HHMMSS)', STIME1, ISTAT )
        IF ( ISTAT .GT. 0 ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'Bad environment variable "STIME"' )
        END IF

        EDATE = ENVINT( 'EDATE', 'Ending date (YYYYDDD)', EDATE1, ISTAT )
        IF ( ISTAT .GT. 0 ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'Bad environment variable "EDATE"' )
        END IF

        ETIME = ENVINT( 'ETIME', 'Ending time (HHMMSS)', ETIME1, ISTAT )
        IF ( ISTAT .GT. 0 ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'Bad environment variable "ETIME"' )
        END IF

        TSTEP = ENVINT( 'TSTEP', 'Time step (H*MMSS)', TSTEP1, ISTAT )
        IF ( ISTAT .GT. 0 ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'Bad environment variable "TSTEP"' )
        END IF

        IF ( EFLAG ) THEN
            CALL M3EXIT( PNAME,0,0, 'Bad environment', 2 )
        END IF

    ELSE

        CALL M3MESG( 'Now enter time step sequence parameters' )

        SDATE = GETNUM( SDATE1, EDATE1, SDATE1, 'Enter STARTING DATE (YYYYDDD)' )

        STIME = GETNUM( 0, 999999999,   STIME1, 'Enter STARTING TIME  (H*MMSS)' )

        TSTEP = GETNUM( TSTEP1, 999999999, TSTEP1, 'Enter     TIME STEP  (H*MMSS)' )

        EDATE = GETNUM( SDATE, EDATE1, EDATE1, 'Enter  ENDING DATE (YYYYDDD)' )

        ETIME = GETNUM( 0, 999999999,  ETIME1, 'Enter   ENDING TIME  (H*MMSS)' )

    END IF      !!  if useenv, or not

    IF ( FNAME .EQ. BLANK ) THEN
        CONTINUE
    ELSE IF ( JSTEP3(SDATE,STIME,SDATE1,STIME1,TSTEP1) .LT. 0 ) THEN
        EFLAG = .TRUE.
        WRITE( MESG, '(A, I9.7, A, I6.6, 2X, 3 A )' )       &
           'Starting date&time', SDATE, ':', STIME,  'not compatible with "', TRIM( FNAME ) , '"'
        CALL M3MESG( MESG )
    END IF

    JDATE = SDATE
    JTIME = STIME
    CALL NEXTIME( JDATE, JTIME, TSTEP )
    IF ( JSTEP3( JDATE,JTIME, SDATE,STIME,TSTEP ) .LT. 0 ) THEN
        EFLAG = .TRUE.
        WRITE( MESG, '(A, I10.6, 2X, 3 A )' ) 'Time step', TSTEP, 'not compatible with "', TRIM( FNAME ) , '"'
        CALL M3MESG( MESG )
    END IF
    
    NRECS = CURREC( EDATE, ETIME, SDATE, STIME, TSTEP, JDATE, JTIME )

    IF ( EFLAG ) THEN
        CALL M3EXIT( PNAME,0,0, 'Bad input time step sequence', 2 )
    END IF

    RETURN

END SUBROUTINE  RUNSPEC
