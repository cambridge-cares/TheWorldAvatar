
        INTEGER  FUNCTION SECSDIFF ( ADATE, ATIME, ZDATE, ZTIME )

C***********************************************************************
C Version "$Id: secsdiff.F 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2003-2010 Baron Advanced Meteorological Systems
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  67
C
C  FUNCTION:  returns the time interval (seconds) between ADATE:ATIME
C             and ZDATE:ZTIME
C
C  PRECONDITION:  normalized dates and times (0 <= SS <= 59, etc.)
C                 stored in format YYYYDDD:HHMMSS.
C
C  REVISION  HISTORY:
C       Prototype   5/1992 by Carlie J. Coats, Jr.,
C       MCNC Environmental Programs
C
C       Version     2/1993 by CJC for CRAY, etc.
C
C       Unification 2/2002 by CJC with global-climate SECSDIFF, which
C       uses a 360-day "year"
C
C       Version 1/2007 by CJC:  simplification; handle negative
C       *DATE arguments correctly
C
C       Modified 03/2010 by CJC: F90 changes for I/O API v3.1
C
C       Modified 4/2016 by CJC: IO_365 changes, from Chris Nolte, US EPA
C***********************************************************************

      IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: ADATE, ATIME
        INTEGER, INTENT(IN   ) :: ZDATE, ZTIME


C...........   PARAMETERS and their descriptions:

        !  "normal"-year, leap-year # of days

#ifdef IO_360
#define     YDAYS   (360)
#define     LDAYS   (360)
#endif
#ifdef IO_365
#define     YDAYS   (365)
#define     LDAYS   (365)
#endif
#ifndef YDAYS
#define     YDAYS   (365)
#define     LDAYS   (366)
#endif


C...........   Local Variables

        INTEGER         AYEAR,  ZYEAR, YEAR, DAYS, HOURS, MINS, SECS
        INTEGER         TOTAL
        INTEGER         BDATE, YDATE


C***********************************************************************
C   begin body of function  SECSDIFF

        IF ( ADATE .GT. 1000 .AND. ZDATE .GT. 1000 ) THEN
            BDATE = ADATE
            YDATE = ZDATE
        ELSE    ! adjust both by multiple of 400-year leap-year cycle
            YEAR  = MAX( -ADATE, -ZDATE ) / 1000 + 1
            YEAR  = 400 * ( YEAR / 400 + 1 )
            BDATE = ADATE + YEAR * 1000
            YDATE = ZDATE + YEAR * 1000
        END IF


C.......   Start with day, hour, min, sec differences:

        DAYS  = MOD( YDATE,     1000 ) -  MOD( BDATE,   1000 )
        HOURS =      ZTIME /   10000   -       ATIME / 10000
        MINS  = MOD( ZTIME/100, 100 )  -  MOD( ATIME/100, 100 )
        SECS  = MOD( ZTIME,     100 )  -  MOD( ATIME,     100 )

        TOTAL = 60 * ( 60 * ( 24 * DAYS + HOURS ) + MINS ) + SECS


C.......   Now add corrections for differences in years:

        AYEAR = BDATE / 1000
        ZYEAR = YDATE / 1000

11      CONTINUE        !  loop accumulating seconds if AYEAR < ZYEAR

            IF ( AYEAR .GE. ZYEAR )  THEN
                GO TO 22
            ELSE IF ( MOD( AYEAR, 4   ) .NE. 0 ) THEN   !  nonleap
                TOTAL = TOTAL + YDAYS * 86400
            ELSE IF ( MOD( AYEAR, 100 ) .NE. 0 ) THEN   !  leap noncentury
                TOTAL = TOTAL + LDAYS * 86400
            ELSE IF ( MOD( AYEAR, 400 ) .NE. 0 ) THEN   !  nonleap century
                TOTAL = TOTAL + YDAYS * 86400
            ELSE                                        !  leap century
                TOTAL = TOTAL + LDAYS * 86400
            END IF
            AYEAR = AYEAR + 1

            GO TO  11

22      CONTINUE        !  loop accumulating seconds if AYEAR > ZYEAR

            IF ( ZYEAR .GE. AYEAR )  THEN
                GO TO 33
            ELSE IF ( MOD( ZYEAR, 4   ) .NE. 0 ) THEN   !  nonleap
                TOTAL = TOTAL - YDAYS * 86400
            ELSE IF ( MOD( ZYEAR, 100 ) .NE. 0 ) THEN   !  leap noncentury
                TOTAL = TOTAL - LDAYS * 86400
            ELSE IF ( MOD( ZYEAR, 400 ) .NE. 0 ) THEN   !  nonleap century
                TOTAL = TOTAL - YDAYS * 86400
            ELSE                                        !  leap century
                TOTAL = TOTAL - LDAYS * 86400
            END IF
            ZYEAR = ZYEAR + 1

            GO TO  22

33      CONTINUE        !  end loops dealing with year adjustments

        SECSDIFF = TOTAL

        RETURN

        END FUNCTION SECSDIFF

