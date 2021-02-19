
        CHARACTER*10 FUNCTION HHMMSS ( JTIME )

C***********************************************************************
C Version "$Id: hhmmss.f 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  57
C
C  FUNCTION:  format and return the time as a character string 
C             "HH:MM:SS"
C
C
C  PRECONDITIONS REQUIRED:  valid time HHMMSS with hours component
C             at most 9999
C
C  RETURN VALUE:  time, as "HH:MM:SS"
C
C
C  SUBROUTINES AND FUNCTIONS CALLED:  none
C
C
C  REVISION  HISTORY:
C	prototype 10/90 by CJC
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C***********************************************************************

      IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: JTIME   !  Julian time, coded YYYYDDD


C...........   PARAMETERs

        CHARACTER*1, PARAMETER :: DIGITS( 0:9 ) =
     &  (/ '0','1','2','3','4','5','6','7','8','9' /)

C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER       HOUR
        INTEGER       MINS
        INTEGER       SECS
        INTEGER       J , K

        CHARACTER*10    CHRBUF


C***********************************************************************
C   begin body of function  HHMMSS

        CHRBUF = '          '

        HOUR = JTIME
        SECS = MOD ( HOUR , 100 )
        HOUR = HOUR / 100
        MINS = MOD ( HOUR , 100 )
        HOUR = HOUR / 100

        J = 1
        K = HOUR / 1000
        IF ( K .GT. 9 ) THEN
            HHMMSS = '<TIMERROR>'
            RETURN
        ELSE IF ( K .NE. 0 ) THEN
            CHRBUF( J:J ) = DIGITS( K )
            J = J + 1
        END IF

        K = MOD( HOUR / 100 , 10 )
        IF ( K .NE. 0 ) THEN
            CHRBUF( J:J ) = DIGITS( K )
            J = J + 1
        END IF

        K = MOD( HOUR / 10 , 10 )
        IF ( K .NE. 0 ) THEN
            CHRBUF( J:J ) = DIGITS( K )
            J = J + 1
        END IF

        CHRBUF( J:J ) = DIGITS( MOD( HOUR, 10 ) )
        J = J + 1
        CHRBUF( J:J ) = ':'
        J = J + 1

        CHRBUF( J:J ) = DIGITS( MINS / 10 )
        J = J + 1
        CHRBUF( J:J ) = DIGITS( MOD( MINS, 10 ) )
        J = J + 1
        CHRBUF( J:J ) = ':'
        J = J + 1

        CHRBUF( J:J ) = DIGITS( SECS / 10 )
        J = J + 1
        CHRBUF( J:J ) = DIGITS( MOD( SECS, 10 ) )

        HHMMSS = CHRBUF

        RETURN

        END FUNCTION HHMMSS

