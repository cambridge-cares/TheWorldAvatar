
      SUBROUTINE DAYMON( JDATE, MNTH, MDAY )

C***********************************************************************
C Version "$Id: daymon.F 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2003-2013 Baron Advanced Meteorological Systems
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  49
C
C  FUNCTION:
C
C    This routine determines the month and day of the month
C    for the Julian date YYYYDDD that is input
C
C  REVISION HISTORY:
C
C       3/1995   Adapted for Models-3/EDSS from ROM GREG.FOR by CJC
C
C       2/2002 Unification by CJC with global-climate DAYMON, which
C       uses a 360-day "year"
C
C       Version 1/2007 by CJC:  handle negative JDATEs correctly
C
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C
C       Version 5/2013 by CJC:  handle standard-year cases
C***********************************************************************


      IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: JDATE	!  Julian date, format YYYYDDD = 1000*Year + Day
        INTEGER, INTENT(  OUT) :: MNTH  !  month (1...12)
        INTEGER, INTENT(  OUT) :: MDAY  !  day-of-month (1...28,29,30,31)


C...........   SCRATCH LOCAL VARIABLES:

        INTEGER IBIAS, IDATE, YEAR, DAY, L, J


C***********************************************************************
C   begin body of subroutine  DAYMON

        IF ( JDATE .GT. -1000 ) THEN
            IDATE = JDATE
            IBIAS = 0
        ELSE
            YEAR  = -JDATE
            YEAR  = YEAR / 1000 + 1
            IBIAS = 2800000 * YEAR
            IDATE = JDATE + IBIAS
        END IF

#ifdef IO_360
        DAY  = MOD( IDATE, 1000 ) - 1
        MNTH =      DAY / 30 + 1
        MDAY = MOD( DAY , 30 ) + 1
        RETURN
#endif

#ifdef IO_365
        YEAR = JDATE / 1000
        DAY  = MOD( IDATE, 1000 )
        J = MOD( DAY + 305, 365 )
        J = MOD( J, 153 ) / 61 + ( J / 153 ) * 2 + J

        MNTH = MOD( J / 31 + 2, 12 ) + 1
        MDAY = MOD( J, 31 ) + 1
        RETURN
#endif

        YEAR = JDATE / 1000
        DAY  = MOD( IDATE, 1000 )
        IF      ( YEAR .LE. 2 ) THEN      !!  "standard-year data
            L = 365
        ELSE IF      ( MOD( YEAR, 400 ) .EQ. 0 ) THEN
            L = 366
        ELSE IF ( MOD( YEAR, 100 ) .EQ. 0 ) THEN
            L = 365
        ELSE IF ( MOD( YEAR, 4 )   .EQ. 0 ) THEN
            L = 366
        ELSE
            L = 365
        END IF

        J = MOD( DAY + 305, L )
        J = MOD( J, 153 ) / 61 + ( J / 153 ) * 2 + J

        MNTH = MOD( J / 31 + 2, 12 ) + 1
        MDAY = MOD( J, 31 ) + 1

        RETURN

      END SUBROUTINE DAYMON

