C
#ifdef IOAPICPL

      LOGICAL FUNCTION INTERP3V( FID, VID, JDATE, JTIME, BUFFER )

C***********************************************************************
C Version "$Id: interp3v.F 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body          starts at line   84
C  Worker routine VIR3BUF starts at line  191
C
C  FUNCTION:
C       Set up INTERP3 for virtual files
C
C  RETURN VALUE:
C       TRUE iff the operation succeeds
C
C  PRECONDITION:
C       FID is a virtual file containing the requested data
C
C  REVISION  HISTORY:
C       Prototype  5/1999 by Atanas L Trayanov, MCNC
C
C       Modified   9/2004 by Carlie J. Coats, Jr., BAMS, for
C       I/O API Version 3:  re-structured interface to bufint3.c
C       routines and data structures; new READ3V interface
C
C       Modified   3/2016:  bug-fixes from Edward Anderson, US EPA
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

      INCLUDE 'PARMS3.EXT'
      INCLUDE 'STATE3.EXT'
      INCLUDE 'STATE3V.EXT'


C...........   ARGUMENTS and their descriptions:

      INTEGER       FID         !  file subscript for STATE3 arrays
      INTEGER       VID         !  variable subscript for STATE3 arrays
      INTEGER       JDATE       !  date, formatted YYYYDDD
      INTEGER       JTIME       !  time, formatted HHMMSS
      REAL          BUFFER(*)   !  interpolation-output buffer array


C...........   EXTERNAL FUNCTIONS and their descriptions:

      INTEGER, EXTERNAL :: BUFINT3
      LOGICAL, EXTERNAL :: CURRSTEP    !  finds start of timestep
      INTEGER, EXTERNAL :: SECSDIFF    !  computes date&time differences
      INTEGER, EXTERNAL :: TIME2SEC    !  converts time to seconds
      LOGICAL, EXTERNAL :: VIR3BUF



C...........   SCRATCH LOCAL VARIABLES and their descriptions:

      REAL          P, Q        !  fractions used for interpolation.
      LOGICAL       RFLAG       !  BUFINT3() read-data flag
      INTEGER       DELTA       !  dummy variable
      INTEGER       DIMS( 5 )   !  dummy variable
      INTEGER       DELS( 5 )   !  dummy variable
      INTEGER       IP
      INTEGER       DT          !  seconds in TSTEP3
      INTEGER       DTJP        !  difference LDATE:LTIME to JDATE:JTIME
      INTEGER       TSTEP
      INTEGER       PDATE, PTIME, FDATE, FTIME

      INTEGER       COUNT
      LOGICAL       PL, PN, FL, FN

C....................................................................
C       begin body of INTERP3V

      TSTEP = TSTEP3( FID )
      COUNT = BSIZE3(FID)*NLAYS3(FID)

      IF ( .NOT. CURRSTEP( JDATE, JTIME,
     &                     SDATE3( FID ), STIME3( FID ), TSTEP,
     &                     PDATE, PTIME ) ) THEN
          CALL M3WARN('INTERP3V', JDATE, JTIME, 'CURRSTEP ERROR')
          INTERP3V = .FALSE.
          RETURN
      END IF

      RFLAG = .FALSE.
      IF ( TSTEP .EQ. 0 ) THEN
          P=1.0
          Q=0.0
          IP=0
          IF ( LDATE3(VID,FID) .EQ. IMISS3 .OR.
     $         LTIME3(VID,FID) .EQ. IMISS3 ) THEN

              INTERP3V = VIR3BUF( FID, VID, 0, 0, 0, BUFFER)

          END IF
      ELSE

         DT   = TIME2SEC( TSTEP )
         DTJP = SECSDIFF( PDATE, PTIME, JDATE, JTIME )

         Q = FLOAT( DTJP ) / FLOAT( DT ) !  INTERPOLATION COEFFICIENTS
         P = 1.0 - Q

         FDATE = PDATE
         FTIME = PTIME
         CALL NEXTIME( FDATE, FTIME, ABS(TSTEP) )

         PL = (PDATE.EQ.LDATE3(VID,FID)).AND.(PTIME.EQ.LTIME3(VID,FID))
         PN = (PDATE.EQ.NDATE3(VID,FID)).AND.(PTIME.EQ.NTIME3(VID,FID))
         FL = (FDATE.EQ.LDATE3(VID,FID)).AND.(FTIME.EQ.LTIME3(VID,FID))
         FN = (FDATE.EQ.NDATE3(VID,FID)).AND.(FTIME.EQ.NTIME3(VID,FID))

         IF ((PL .AND. PN) .OR. (FL .AND. FN) .OR.
     $       (PL .AND. FL) .OR. (PN .AND. FN)) THEN

            CALL M3WARN('INTERP3V', JDATE, JTIME,
     $           'Inconsistent LDATE3/LTIME3 or NDATE3/NTIME3 ERROR')
            INTERP3V = .FALSE.
            RETURN

         ENDIF

         INTERP3V = .TRUE.
         IF(PL) THEN
            IP=0
            IF (.NOT. FN) THEN
               IF (Q .NE. 0.0) THEN
                  INTERP3V = VIR3BUF( FID, VID, 1, FDATE, FTIME, BUFFER)
               ENDIF
            ENDIF
         ELSE IF(PN) THEN
            IP=1
            IF (.NOT. FL) THEN
               IF (Q .NE. 0.0) THEN
                  INTERP3V = VIR3BUF( FID, VID, 0, FDATE, FTIME, BUFFER)
               ENDIF
            ENDIF
         ELSE IF(FL) THEN
            IP=1
            IF (P .NE. 0.0) THEN
               INTERP3V = VIR3BUF( FID, VID, 1, PDATE, PTIME, BUFFER)
            ENDIF
         ELSE IF(FN) THEN
            IP=0
            IF (P .NE. 0.0) THEN
               INTERP3V = VIR3BUF( FID, VID, 0, PDATE, PTIME, BUFFER)
            ENDIF
         ELSE
            IP=0
            IF (P .NE. 0.0) THEN
               INTERP3V = VIR3BUF( FID, VID, 0, PDATE, PTIME, BUFFER)
            ENDIF
            IF (Q .NE. 0.0) THEN
               INTERP3V = INTERP3V .AND .
     $              VIR3BUF( FID, VID, 1, FDATE, FTIME, BUFFER)
            END IF
         END IF
      END IF

      IF (.NOT. INTERP3V) THEN
         CALL M3WARN('INTERP3V', JDATE, JTIME, 'VIR3BUF ERROR')
         RETURN
      ENDIF

      ILAST3(VID,FID) = IP

      IF ( 0 .NE. BUFINT3( FID, VID,NLAYS3( FID ) * BSIZE3( FID ),
     $                     IP, TSTEP, P, Q, BUFFER ) ) THEN
          INTERP3V = .TRUE.
      ELSE
          CALL M3WARN('INTERP3V', JDATE, JTIME, 'BUFINT3 ERROR')
          INTERP3V=.FALSE.
      END IF

      RETURN
      END FUNCTION INTERP3V


C*********************************************************************

      LOGICAL FUNCTION VIR3BUF( FID, VID, IP, KDATE, KTIME, BUFFER )

      IMPLICIT NONE

C...........   INCLUDES:

      INCLUDE 'PARMS3.EXT'
      INCLUDE 'STATE3.EXT'
      INCLUDE 'STATE3V.EXT'


C...........   ARGUMENTS and their descriptions:

      INTEGER           VID, FID, IP, KDATE, KTIME
      REAL              BUFFER(*)

C...........   EXTERNAL FUNCTIONS and their descriptions:

      LOGICAL, EXTERNAL :: READ3V
      INTEGER, EXTERNAL :: BUFPUT3

C...........   SCRATCH LOCAL VARIABLES and their descriptions:

      INTEGER           COUNT, SKIP, TYPE
      INTEGER           RET

C....................................................................
C       begin body of VIR3BUF

      COUNT = BSIZE3(FID)*NLAYS3(FID)
      SKIP = 0

      VIR3BUF = READ3V( FID, VID, SKIP, COUNT,
     &                  KDATE, KTIME, BUFFER )
      RET = BUFPUT3( FID, VID, COUNT, IP, BUFFER)
      IF (VIR3BUF .AND. RET .NE. 0) THEN
         IF (IP .EQ. 0) THEN
            LDATE3(VID,FID) = KDATE
            LTIME3(VID,FID) = KTIME
         ELSE
            NDATE3(VID,FID) = KDATE
            NTIME3(VID,FID) = KTIME
         ENDIF
         VIR3BUF = .TRUE.
      ELSE
         IF (IP .EQ. 0) THEN
            LDATE3(VID,FID) = IMISS3
            LTIME3(VID,FID) = IMISS3
         ELSE
            NDATE3(VID,FID) = IMISS3
            NTIME3(VID,FID) = IMISS3
         ENDIF
         VIR3BUF = .FALSE.
      ENDIF
      RETURN
      END FUNCTION VIR3BUF

#endif
