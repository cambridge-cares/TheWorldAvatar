
#ifdef IOAPICPL
      LOGICAL FUNCTION DDTVAR3V( FID, VID, JDATE, JTIME, BUFFER )

C***********************************************************************
C Version "$Id: ddtvar3v.F 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2003-2010 Baron Advanced Meteorological Systems,
C (C) 2007-2013 Carlie J. Coats, Jr., and 
C (C) 2015 UNC Institute for the Environment.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  subroutine body starts at line  79
C
C  FUNCTION:
C       Wrapper for coupling-mode DDTVAR3()
C
C  RETURN VALUE:
C       TRUE iff operation succeeds
C
C  PRECONDITIONS REQUIRED:
C
C  REVISION  HISTORY:
C       Prototype  5/1999 by Atanas L Trayanov, MCNC
C
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C
C       Modified 02/2015 by CJC for I/O API 3.2: USE M3UTILIO
C***********************************************************************

        USE M3UTILIO

        IMPLICIT NONE

C...........   INCLUDES:

      INCLUDE 'STATE3.EXT'


C...........   ARGUMENTS and their descriptions:

      INTEGER, INTENT(IN   ) :: FID         !  file subscript for STATE3 arrays
      INTEGER, INTENT(IN   ) :: VID         !  variable subscript for STATE3 arrays
      INTEGER, INTENT(IN   ) :: JDATE       !  date, formatted YYYYDDD
      INTEGER, INTENT(IN   ) :: JTIME       !  time, formatted HHMMSS
      REAL   , INTENT(  OUT) :: BUFFER(*)   !  interpolation-output buffer array


C...........   EXTERNAL FUNCTIONS and their descriptions:

      INTEGER, EXTERNAL :: BUFVGT3     !  alloc/interp from circ. bufs. (C)
      INTEGER, EXTERNAL :: BUFDDT3     !  alloc/ddt from circ. bufs. (C)
      LOGICAL, EXTERNAL :: VIR2BUF


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

      LOGICAL       RFLAG       !  BUFINT3() read-data flag
      INTEGER       DELTA       !  dummy variable
      INTEGER       DIMS( 5 )   !  dummy variable
      INTEGER       DELS( 5 )   !  dummy variable
      INTEGER       IP
      INTEGER       DT          !  seconds in TSTEP3
      INTEGER       DTJP        !  difference LDATE:LTIME to JDATE:JTIME
      INTEGER       TSTEP

      INTEGER PDATE, PTIME, FDATE, FTIME

      INTEGER       COUNT
      INTEGER       I
      LOGICAL       PL, PN, FL, FN


C***********************************************************************
C   begin body of subroutine  DDTVAR3V()

      TSTEP = TSTEP3( FID )
      COUNT = BSIZE3(FID)*NLAYS3(FID)

C.....we use the next call only to allocate memory for circular buffer
      RFLAG = .FALSE.
      IF (0 .EQ. BUFVGT3( FID, VID, RFLAG,
     $                    DIMS, DELS,
     $                    COUNT, DELTA, TSTEP ) ) THEN
         CALL M3WARN('DDTVAR3V', JDATE, JTIME, 'BUFVGT3 ERROR')
         DDTVAR3V = .FALSE.
         RETURN
      ENDIF
         
      IF ( .NOT. CURRSTEP( JDATE, JTIME, SDATE3( FID ), STIME3( FID ),
     &     TSTEP, PDATE, PTIME ) ) THEN
         CALL M3WARN('DDTVAR3V', JDATE, JTIME, 'CURRSTEP ERROR')
         DDTVAR3V = .FALSE.
         RETURN
      ENDIF
      
      IF (TSTEP .EQ. 0) THEN
         DO I = 1, COUNT
            BUFFER(I) = 0.0
         END DO
         DDTVAR3V = .TRUE.
         RETURN
      ELSE

         DT   = TIME2SEC( TSTEP )
         DTJP = SECSDIFF( PDATE, PTIME, JDATE, JTIME )

         FDATE = PDATE
         FTIME = PTIME
         CALL NEXTIME( FDATE, FTIME, ABS(TSTEP) )

         PL = (PDATE.EQ.LDATE3(VID,FID)).AND.(PTIME.EQ.LTIME3(VID,FID))
         PN = (PDATE.EQ.NDATE3(VID,FID)).AND.(PTIME.EQ.NTIME3(VID,FID))
         FL = (FDATE.EQ.LDATE3(VID,FID)).AND.(FTIME.EQ.LTIME3(VID,FID))
         FN = (FDATE.EQ.NDATE3(VID,FID)).AND.(FTIME.EQ.NTIME3(VID,FID))

         IF ((PL .AND. PN) .OR. (FL .AND. FN) .OR.
     $       (PL .AND. FL) .OR. (PN .AND. FN)) THEN
            CALL M3WARN('DDTVAR3V', JDATE, JTIME,
     $           'Inconsistent LDATE3/LTIME3 or NDATE3/NTIME3 ERROR')
            DDTVAR3V = .FALSE.
            RETURN
         ENDIF

         DDTVAR3V = .TRUE.
         IF(PL) THEN
            IP=0
            IF (.NOT. FN) THEN
               DDTVAR3V = VIR2BUF( FID, VID, 1, FDATE, FTIME )
            ENDIF
         ELSE IF(PN) THEN
            IP=1
            IF (.NOT. FL) THEN
               DDTVAR3V = VIR2BUF( FID, VID, 0, FDATE, FTIME )
            ENDIF
         ELSE IF(FL) THEN
            IP=1
            DDTVAR3V = VIR2BUF( FID, VID, 1, PDATE, PTIME )
         ELSE IF(FN) THEN
            IP=0
            DDTVAR3V = VIR2BUF( FID, VID, 0, PDATE, PTIME )
         ELSE
            IP=0
            DDTVAR3V = VIR2BUF( FID, VID, 0, PDATE, PTIME )
            DDTVAR3V = DDTVAR3V .AND.
     $                 VIR2BUF( FID, VID, 1, FDATE, FTIME )
         ENDIF
      ENDIF

      IF (.NOT. DDTVAR3V) THEN
         CALL M3WARN('DDTVAR3V', JDATE, JTIME, 'vir2buf ERROR')
         RETURN
      ENDIF

      ILAST3(VID,FID) = IP

      IF ( 0 .NE. BUFDDT3( FID, VID, COUNT,
     &                     IP, TSTEP,
     &                     1.0 / FLOAT( TIME2SEC( TSTEP ) ),
     &                     BUFFER ) ) THEN

         DDTVAR3V = .TRUE.
      ELSE
         CALL M3WARN('DDTVAR3V', JDATE, JTIME, 'BUFINT3 ERROR')
         DDTVAR3V=.FALSE.
      ENDIF

      RETURN
      END FUNCTION DDTVAR3V

#endif
