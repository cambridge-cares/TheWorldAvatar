
        LOGICAL FUNCTION WRBUF3( FID, VID, JDATE, JTIME, STEP, BUFFER )

C***********************************************************************
C Version "$Id: wrbuf3.f 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2003-2010 Baron Advanced Meteorological Systems,
C (C) 2007-2013 Carlie J. Coats, Jr., and
C (C) 2015 UNC Institute for the Environment.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  85
C
C  FUNCTION:  writes all the data from BUFFER() for timestep JDATE:JTIME
C             (formatted YYYYDDD and HHMMSS) to the Models-3 BUFFERED
C             data "file" with file index FID.
C             If FNAME is time-independent, JDATE and JTIME are ignored.
C
C  RETURN VALUE:  TRUE iff the operation succeeds
C
C  PRECONDITIONS REQUIRED:
C       FNAME is a BUFFERED Models-3 data file already opened for
C       write access by OPEN3()
C       For ALLLVARS3 reads, all variables must be of type M3REAL
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C                 JSTEP3, BUFPUT3 (from bufint3.c)
C
C  REVISION  HISTORY:
C       prototype 7/1994 by CJC
C
C       modified 10/1994 by CJC to permit WRITE3-granularity at the level
C       of individual variables.
C
C       Modified 5/2002 to support types other than REAL
C
C       Modified 9/2004 by CJC for I/O API v3 TSTEP/buffer management
C       unification
C
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C
C       Bug-fix 04/2011 vy CJC:  argument-list fix for BUFPUT3()
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'STATE3.EXT'


C...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: FID             !  file-subscript for STATE3 arrays
        INTEGER, INTENT(IN   ) :: VID             !  vble-subscript for STATE3 arrays
        INTEGER, INTENT(IN   ) :: JDATE           !  date, formatted YYYYDDD
        INTEGER, INTENT(IN   ) :: JTIME           !  time, formatted HHMMSS
        INTEGER, INTENT(IN   ) :: STEP            !  time step record number
        REAL   , INTENT(IN   ) :: BUFFER(*)       !  output buffer array


C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER, EXTERNAL :: BUFPUT3, BUFPUT3D, BUFPUT3I


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         V       !  loop counter (over variables)
        INTEGER         IDUM    !  scratch variable
        INTEGER         SIZE, I, TSTEP
        INTEGER         JSTEP
        INTEGER         ADATE, ATIME
        INTEGER         ZDATE, ZTIME
        LOGICAL         WFLAG
        CHARACTER*256   MESG            !  for m3msg2, m3warn


C***********************************************************************
C   begin body of function  WRBUF3

        SIZE  = BSIZE3( FID ) * NLAYS3( FID )
        TSTEP = TSTEP3( FID )

        JSTEP = ABS( TSTEP3( FID ) )
        IF ( JSTEP .GT. 0 ) THEN
            ADATE = JDATE
            ATIME = JTIME
            CALL NEXTIME( ADATE, ATIME, -JSTEP )    !  one time step before J
            ZDATE = JDATE
            ZTIME = JTIME
            CALL NEXTIME( ZDATE, ZTIME, JSTEP )     !  one time step after J
            IDUM = MOD( 1 + STEP, 2 )
        ELSE
            ADATE = 0
            ATIME = 0
            ZDATE = 0
            ZTIME = 0
            IDUM  = 0
        END IF

        IF ( VID .GT. 0 ) THEN          !  single-variable write request

            IF ( LDATE3( VID,FID ) .EQ. IMISS3 ) THEN !  first call

                IF ( JSTEP .NE. 0 ) THEN

                    ILAST3( VID,FID ) = IDUM
                    LDATE3( VID,FID ) = JDATE
                    LTIME3( VID,FID ) = JTIME

                ELSE

                    ILAST3( VID,FID ) = 0
                    LDATE3( VID,FID ) = 0
                    LTIME3( VID,FID ) = 0
                    NDATE3( VID,FID ) = 0
                    NTIME3( VID,FID ) = 0

                END IF

            ELSE IF ( NDATE3( VID,FID ) .EQ. ADATE  .AND.
     &                NTIME3( VID,FID ) .EQ. ATIME ) THEN ! advance 1 time step

                IF ( JSTEP .NE. 0 ) THEN

                    ILAST3( VID,FID ) = 1 - IDUM
                    LDATE3( VID,FID ) = NDATE3( VID,FID )
                    LTIME3( VID,FID ) = NTIME3( VID,FID )
                    NDATE3( VID,FID ) = JDATE
                    NTIME3( VID,FID ) = JTIME

                ELSE

                    ILAST3( VID,FID ) = 0
                    LDATE3( VID,FID ) = 0
                    LTIME3( VID,FID ) = 0
                    NDATE3( VID,FID ) = 0
                    NTIME3( VID,FID ) = 0

                END IF

            ELSE IF ( LDATE3( VID,FID ) .EQ. ZDATE  .AND.
     &                LTIME3( VID,FID ) .EQ. ZTIME ) THEN  ! retreat 1 time step

                ILAST3( VID,FID ) = 1 - IDUM            !  case _must_ be
                NDATE3( VID,FID ) = LDATE3( VID,FID )
                NTIME3( VID,FID ) = LTIME3( VID,FID )
                LDATE3( VID,FID ) = JDATE               !  time-dependent
                LTIME3( VID,FID ) = JTIME

            ELSE IF ( NDATE3( VID,FID ) .EQ. IMISS3 ) THEN  ! second call,
                                                            ! time dependent case

                IF  ( LDATE3( VID,FID ) .EQ. ADATE  .AND.
     &                LTIME3( VID,FID ) .EQ. ATIME ) THEN  ! step forward

                    NDATE3( VID,FID ) = JDATE
                    NTIME3( VID,FID ) = JTIME

                ELSE IF  ( LDATE3( VID,FID ) .EQ. ZDATE  .AND.
     &                     LTIME3( VID,FID ) .EQ. ZTIME ) THEN  ! step forward

                    ILAST3( VID,FID ) = 1 - IDUM
                    LDATE3( VID,FID ) = NDATE3( VID,FID )
                    LTIME3( VID,FID ) = NTIME3( VID,FID )
                    NDATE3( VID,FID ) = JDATE
                    NTIME3( VID,FID ) = JTIME

                END IF

            ELSE IF ( ( JDATE .NE. LDATE3( VID,FID ) .OR.
     &                  JTIME .NE. LTIME3( VID,FID ) )
     &                .AND.
     &                ( JDATE .NE. NDATE3( VID,FID ) .OR.
     &                  JTIME .NE. NTIME3( VID,FID ) ) ) THEN

                WRITE( MESG, 93020 )
     &              'Date/time being written to BUFFERED file ' //
     &              FLIST3( FID ) // ':', JDATE, JTIME
                CALL M3MSG2( MESG )
                WRITE( MESG, 93020 )
     &              'Last date/time  written:',
     &              LDATE3( VID,FID ), LTIME3( VID,FID )
                CALL M3MSG2( MESG )
                MESG = 'Out-of-order write to BUFFERED file '
                CALL M3WARN( 'WRITE3/WRBUF3', JDATE, JTIME, MESG )

                WRBUF3 = .FALSE.
                RETURN

            END IF

            IF ( VTYPE3( VID,FID ) .EQ. M3REAL ) THEN
                WRBUF3 = ( 0 .NE. BUFPUT3 ( FID, VID,
     &                                      SIZE, IDUM, TSTEP,
     &                                      BUFFER ) )
            ELSE IF ( VTYPE3( VID,FID ) .EQ. M3INT ) THEN
                WRBUF3 = ( 0 .NE. BUFPUT3I( FID, VID,
     &                                      SIZE, IDUM, TSTEP,
     &                                      BUFFER ) )
            ELSE IF ( VTYPE3( VID,FID ) .EQ. M3DBLE ) THEN
                WRBUF3 = ( 0 .NE. BUFPUT3D( FID, VID,
     &                                      SIZE, IDUM, TSTEP,
     &                                      BUFFER ) )
            END IF

        ELSE    !  "all-variables" write request

            I = 1       !  starting subscript for buffer-slice being written

            DO  99  V = 1, NVARS3( FID )

                IF ( VTYPE3( V,FID ) .NE. M3REAL ) THEN

                    MESG = 'ALLVAR3 non-REAL types not supported'
                    CALL M3WARN( 'WRITE3/WRBUF3', JDATE, JTIME, MESG )
                    WRBUF3 = .FALSE.
                    RETURN

                ELSE IF ( LDATE3( V,FID ) .EQ. IMISS3 ) THEN !  first call

                    IF ( JSTEP .NE. 0 ) THEN

                        ILAST3( V,FID ) = IDUM
                        LDATE3( V,FID ) = JDATE
                        LTIME3( V,FID ) = JTIME

                    ELSE

                        ILAST3( V,FID ) = IDUM
                        LDATE3( V,FID ) = 0
                        LTIME3( V,FID ) = 0
                        NDATE3( V,FID ) = 0
                        NTIME3( V,FID ) = 0

                    END IF

                ELSE IF ( NDATE3( V,FID ) .EQ. ADATE  .AND.
     &                    NTIME3( V,FID ) .EQ. ATIME ) THEN ! advance 1 timestep

                    WFLAG = ( 0 .NE. BUFPUT3( FID, V, SIZE,
     &                                        1-IDUM , BUFFER( I ) ) )

                    IF ( JSTEP .NE. 0 ) THEN

                        ILAST3( V,FID ) = IDUM
                        LDATE3( V,FID ) = NDATE3( V,FID )
                        LTIME3( V,FID ) = NTIME3( V,FID )
                        NDATE3( V,FID ) = JDATE
                        NTIME3( V,FID ) = JTIME

                    ELSE

                        ILAST3( V,FID ) = 1 - IDUM
                        LDATE3( V,FID ) = 0
                        LTIME3( V,FID ) = 0
                        NDATE3( V,FID ) = 0
                        NTIME3( V,FID ) = 0

                    END IF

                ELSE IF ( LDATE3( V,FID ) .EQ. ZDATE  .AND.
     &                    LTIME3( V,FID ) .EQ. ZTIME ) THEN  ! retreat 1 time step

                    WFLAG = ( 0 .NE. BUFPUT3( FID, V, SIZE,
     &                                        IDUM , BUFFER( I ) ) )

                    ILAST3( V,FID ) = IDUM      !  case must be time-dependent
                    NDATE3( V,FID ) = LDATE3( V,FID )
                    NTIME3( V,FID ) = LTIME3( V,FID )
                    LDATE3( V,FID ) = JDATE
                    LTIME3( V,FID ) = JTIME

                ELSE IF ( NDATE3( V,FID ) .EQ. IMISS3 ) THEN  ! second call,
                                                              ! time dependent

                    IF (  LDATE3( V,FID ) .EQ. ADATE  .AND.
     &                    LTIME3( V,FID ) .EQ. ATIME ) THEN     !  step forward

                        ILAST3( V,FID ) = 1 - IDUM
                        NDATE3( V,FID ) = JDATE
                        NTIME3( V,FID ) = JTIME

                    ELSE IF (  LDATE3( V,FID ) .EQ. ZDATE  .AND.
     &                         LTIME3( V,FID ) .EQ. ZTIME ) THEN ! step backward

                        ILAST3( V,FID ) = IDUM
                        LDATE3( V,FID ) = NDATE3( V,FID )
                        LTIME3( V,FID ) = NTIME3( V,FID )
                        NDATE3( V,FID ) = JDATE
                        NTIME3( V,FID ) = JTIME

                    END IF

                ELSE IF ( ( JDATE .NE. LDATE3( V,FID ) .OR.
     &                      JTIME .NE. LTIME3( V,FID ) )
     &                    .AND.
     &                    ( JDATE .NE. NDATE3( V,FID ) .OR.
     &                      JTIME .NE. NTIME3( V,FID ) ) ) THEN

                    WRITE( MESG, 93020 )
     &              'Date/time being written to BUFFERED file ' //
     &              FLIST3( FID ) // ':', JDATE, JTIME
                    CALL M3MSG2( MESG )
                    WRITE( MESG, 93020 )
     &              'Last date/time  written:',
     &              LDATE3( VID,FID ), LTIME3( VID,FID )
                    CALL M3MSG2( MESG )
                    MESG = 'Out-of-order write to BUFFERED file '
                    CALL M3WARN( 'WRITE3/WRBUF3', JDATE, JTIME, MESG )

                    WRBUF3 = .FALSE.
                    RETURN

                END IF!  if "advance 1", "retreat 1", or not "last" or "next"

                WFLAG = ( 0 .NE. BUFPUT3( FID, V, SIZE, IDUM,
     &                                    TSTEP3( FID ), BUFFER( I ) ) )

                I = I + SIZE    !  set up for next variable's slice of buffer()

                IF( .NOT. WFLAG ) THEN

                    MESG = 'Failure writing ' // VLIST3( V,FID ) //
     &                     ' to ' // FLIST3( FID )
                    CALL M3WARN( 'WRITE3/WRBUF3', JDATE, JTIME, MESG )
                    WRBUF3 = .FALSE.
                    RETURN

                END IF  !  if wflag:  bufput failed for this variable

99          CONTINUE            !  end loop on variables in this file

            MXREC3( FID ) = MAX( MXREC3( FID ), STEP )

            WRBUF3 = .TRUE.     !  (if you get to here)

        END IF  !  if vid>0 (one-vble request), or not (all-vbles request)

        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Internal buffering formats..... 93xxx

93020   FORMAT ( A, :, I9, ':', I6.6, :, A, :, 2X, I6.6 )

        END FUNCTION WRBUF3

