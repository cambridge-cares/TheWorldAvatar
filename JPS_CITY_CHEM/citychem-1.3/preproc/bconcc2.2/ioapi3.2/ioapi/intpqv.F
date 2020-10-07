C.........................................................................
C Version "$Id: intpqv.F 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2013 Baron Advanced Meteorological Systems,
C (C) 2007-2013 Carlie J. Coats, Jr., and
C (C) 2015 UNC Institute for the Environment.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  INTPQV  function body starts at line   79
C  VIR2BUF function body starts at line  180
C
C  FUNCTION:
C       For virtual files, initialize buffers maintained by "bufint3.c"
C       and compute interpolation coefficients P, Q
C
C  RETURN VALUE:
C       TRUE iff the operation succeeds (and the data is available)
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       CURRSTEP, SECSDIFF, TIME2SEC, VIR2BUF
C
C  REVISION  HISTORY:
C       Created  5/1999 by CJC and ALT for coupling-mode operation
C
C       Modified  8/17/2004 by CJC for I/O API v3:  new routine
C       INTPQV() replaces INTERP3V; works with main INTERP3/INTERPX
C       driver to initialize "bufint3.c" buffers, and then do calls
C       to BUFINT*() from there instead of redundantly from here.
C
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C
C       Modified 02/2015 by CJC for I/O API 3.2: USE M3UTILIO; INTENT
C
C       Modified   3/2016:  bug-fix from Edward Anderson, US EPA 
C.........................................................................

#ifdef IOAPICPL

      LOGICAL FUNCTION INTPQV( FID, VID, JDATE, JTIME, P, Q )
     &                 RESULT( INTFLAG )

        USE M3UTILIO

        IMPLICIT NONE

C...........   INCLUDES:

      INCLUDE 'STATE3.EXT'
      INCLUDE 'STATE3V.EXT'


C...........   ARGUMENTS and their descriptions:

      INTEGER, INTENT(IN   ) :: FID         !  file subscript for STATE3 arrays
      INTEGER, INTENT(IN   ) :: VID         !  variable subscript for STATE3 arrays
      INTEGER, INTENT(IN   ) :: JDATE       !  date, formatted YYYYDDD
      INTEGER, INTENT(IN   ) :: JTIME       !  time, formatted HHMMSS
      REAL   , INTENT(INOUT) :: P, Q        !  fractions used for interpolation.


C...........   EXTERNAL FUNCTIONS and their descriptions:

      INTEGER, EXTERNAL :: VIR2BUF


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

      INTEGER       IP
      INTEGER       TSTEP
      INTEGER       PDATE, PTIME, FDATE, FTIME, IRET
      INTEGER       DT, DTJP
      LOGICAL       PL, PN, FL, FN

C***********************************************************************
C   begin body of function  INTERP3

      TSTEP = TSTEP3( FID )

      IF ( .NOT. CURRSTEP( JDATE, JTIME,
     &                     SDATE3( FID ), STIME3( FID ), TSTEP,
     &                     PDATE, PTIME ) ) THEN
          CALL M3WARN('INTPQV', JDATE, JTIME, 'CURRSTEP ERROR')
          INTFLAG = .FALSE.
          RETURN
      END IF

      IRET = 1

      IF ( TSTEP .EQ. 0 ) THEN

          P = 1.0
          Q = 0.0
          IP= 0
          IF ( LDATE3(VID,FID) .EQ. IMISS3 .OR.
     &         LTIME3(VID,FID) .EQ. IMISS3 ) THEN
              IRET = VIR2BUF( FID, VID, 0, 0, 0 )
          END IF

      ELSE

         DT   = TIME2SEC( TSTEP )
         DTJP = SECSDIFF( PDATE, PTIME, JDATE, JTIME )

         Q = FLOAT( DTJP ) / FLOAT( DT ) !  Interpolation coefficients
         P = 1.0 - Q

         FDATE = PDATE
         FTIME = PTIME
         CALL NEXTIME( FDATE, FTIME, ABS(TSTEP) )

         PL = (PDATE.EQ.LDATE3(VID,FID)).AND.(PTIME.EQ.LTIME3(VID,FID))
         PN = (PDATE.EQ.NDATE3(VID,FID)).AND.(PTIME.EQ.NTIME3(VID,FID))
         FL = (FDATE.EQ.LDATE3(VID,FID)).AND.(FTIME.EQ.LTIME3(VID,FID))
         FN = (FDATE.EQ.NDATE3(VID,FID)).AND.(FTIME.EQ.NTIME3(VID,FID))

         IF ( (PL .AND. PN) .OR. (FL .AND. FN) .OR.
     $        (PL .AND. FL) .OR. (PN .AND. FN) ) THEN
            CALL M3WARN('INTPQV', JDATE, JTIME,
     $           'Inconsistent LDATE3/LTIME3 or NDATE3/NTIME3 ERROR')
            INTFLAG = .FALSE.
            RETURN
         ENDIF

         IF ( PL ) THEN
            IP=0
            IF ( .NOT. FN ) THEN
               IF (Q .NE. 0.0) THEN
                  IRET = VIR2BUF( FID, VID, 1, FDATE, FTIME)
               END IF
            ENDIF
         ELSE IF ( PN ) THEN
            IP=1
            IF ( .NOT. FL) THEN
               IF (Q .NE. 0.0) THEN
                  IRET = VIR2BUF( FID, VID, 0, FDATE, FTIME )
               END IF
            ENDIF
         ELSE IF ( FL ) THEN
            IP=1
            IF (P .NE. 0.0 ) THEN
               IRET = VIR2BUF( FID, VID, 1, PDATE, PTIME )
            ENDIF
         ELSE IF ( FN ) THEN
            IP=0
            IF (P .NE. 0.0) THEN
               IRET = VIR2BUF( FID, VID, 0, PDATE, PTIME )
            END IF
         ELSE
            IP=0
            IF ( P .NE. 0.0 ) THEN
               IRET = VIR2BUF( FID, VID, 0, PDATE, PTIME )
            ENDIF
            IF (Q .NE. 0.0) THEN
               IRET = IRET * VIR2BUF( FID, VID, 1, FDATE, FTIME )
            ENDIF
         ENDIF
      ENDIF

      IF ( 0 .EQ.IRET ) THEN

          CALL M3WARN('INTERP3/INTPQV', JDATE, JTIME, 'vir2buf ERROR')
          INTFLAG = .FALSE.

      ELSE

          ILAST3(VID,FID) = IP
          INTFLAG = .TRUE.

      END IF            !  if 0 = iret

      RETURN

      END FUNCTION INTPQV


C*********************************************************************

      INTEGER FUNCTION VIR2BUF( FID, VID, IP, KDATE, KTIME )

        !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        !!    Set up buffered data in "bufint3.c" data structures;
        !!    update LDATE:LTIME and NDATE:NTIME
        !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

        USE M3UTILIO

        IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'STATE3.EXT'
        INCLUDE 'STATE3V.EXT'


C...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: FID, VID, IP, KDATE, KTIME

C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER, EXTERNAL :: BUFVRD3, BUFVRD3D

C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER   SIZE, TYPE
        INTEGER   RET

        !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

        SIZE = BSIZE3( FID )*NLAYS3( FID )
        TYPE = VTYPE3( VID, FID )

        IF ( TYPE .EQ. M3REAL ) THEN
             RET = BUFVRD3 ( FID, VID, IP, SIZE, KDATE, KTIME )
        ELSE IF ( TYPE .EQ. M3DBLE ) THEN
             RET = BUFVRD3D( FID, VID, IP, SIZE, KDATE, KTIME )
        ELSE
             RET = 0
             CALL M3WARN( 'VIR2BUF', KDATE, KTIME,
     &                    'Invalid vble type (not REAL nor DBLE)' )
        END IF

        IF ( RET .NE. 0 ) THEN
            IF (IP .EQ. 0) THEN
                LDATE3(VID,FID) = KDATE
                LTIME3(VID,FID) = KTIME
            ELSE
                NDATE3(VID,FID) = KDATE
                NTIME3(VID,FID) = KTIME
            ENDIF
        ELSE
            IF ( IP .EQ. 0 ) THEN
                LDATE3(VID,FID) = IMISS3
                LTIME3(VID,FID) = IMISS3
            ELSE
                NDATE3(VID,FID) = IMISS3
                NTIME3(VID,FID) = IMISS3
            ENDIF
        END IF
        VIR2BUF = RET

        RETURN
        END FUNCTION VIR2BUF

#endif
