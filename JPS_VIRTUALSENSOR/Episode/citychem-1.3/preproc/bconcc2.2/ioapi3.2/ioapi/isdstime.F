
        LOGICAL FUNCTION ISDSTIME( JDATE )

C***********************************************************************
C Version "$Id: isdstime.F 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2003-2010 Baron Advanced Meteorological Systems
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  70
C
C  RETURNS:
C       TRUE iff Daylight Savings Time is in effect for JDATE,
C       calculated using Zeller's Congruence to find the
C       starting and ending dates for Daylight Savings Time:
C       pre-2007:  first Sunday in April and the last Sunday in October
C       post-2007: secnd Sunday in March and the first Sunday in November
C
C  PRECONDITIONS REQUIRED:
C       JDATE represents a date YYYYDDD according to Models-3 conventions
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       None
C
C  REVISION  HISTORY:
C       Prototype  8/1995 by Carlie J. Coats, Jr., MCNC Environmental Programs
C
C       Unification 2/2002 by CJC with global-climate ISDST, which
C       uses a 360-day "year"
C
C       Bugfix     7/2002 by CJC
C
C       Version 1/2007 by CJC:  update for 2007 Daylight Saving Time changes;
C       handle negative JDATE arguments correctly
C
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C
C       Modified 11/2015 by CJC: IO_365 changes
C***********************************************************************

      IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT( IN ) :: JDATE   !  Julian date, coded YYYYDDD


C...........   PARAMETERS and their descriptions:

#ifdef IO_360
        INTEGER, PARAMETER :: APR1  =  91
        INTEGER, PARAMETER :: OCT31 = 300
#define CLIMO
#endif

#ifdef IO_365
#define CLIMO
#endif

#ifndef IO_360
        INTEGER, PARAMETER :: APR1  =  91
        INTEGER, PARAMETER :: OCT31 = 304
#endif


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         YEAR, IDAY
        INTEGER         DAY0, DAY1
        INTEGER         K, IBIAS


C***********************************************************************
C   begin body of function  ISDSTIME

        IF ( JDATE .GE. 1000 ) THEN
            YEAR = JDATE / 1000
            IDAY = MOD ( JDATE , 1000 )
        ELSE
            YEAR  = -JDATE
            YEAR  = YEAR / 1000 + 2800  !  leap-year/day-of-week cycle 2800
            IBIAS = 2800000 * ( YEAR / 2800 )
            YEAR =     ( JDATE + IBIAS ) / 1000
            IDAY = MOD ( JDATE + IBIAS   , 1000 )
        END IF


C.......   Uses Zeller's Congruence calculation of day of wk for APR1, OCT31:
C.......   wkday = 1 + mod( k + day, 7 )

        DAY0  = APR1        !  get first Sunday in April,
        DAY1  = OCT31       !  last Sunday in October

#ifndef CLIMO
        IF (          ( MOD (YEAR,4)   .EQ. 0 )        !  leap year adjustment
     &     .AND. (    ( MOD (YEAR,100) .NE. 0 )        !  month >= FEB
     &           .OR. ( MOD (YEAR,400) .EQ. 0 ) ) ) THEN

            DAY0 = DAY0 + 1
            DAY1 = DAY1 + 1

        END IF
#endif

        K     = YEAR - 1
        K     = K * 365  +  K / 4  -  K / 100  +  K / 400  -  1
        IF ( YEAR .LT. 2007 ) THEN
            DAY0  = DAY0 + 6 - MOD( K + DAY0, 7 ) !  first Sun. in Apr.
            K     = 1 + MOD( K + DAY1, 7 )        !  day-number 1...7 for OCT31
            DAY1  = DAY1     - MOD( K, 7 )        !  last  Sun. in Oct.
        ELSE
            DAY0  = DAY0 - 31                      !  March 1
            DAY0  = DAY0 + 13 - MOD( K + DAY0, 7 ) !  second Sunday in March,
            K     = 1 + MOD( K + DAY1, 7 )         !  day-number 1...7 for OCT31
            DAY1  = DAY1 +  6 - MOD( K, 7 )        !  first Sunday in November
        END IF

        ISDSTIME = ( ( IDAY .GE. DAY0 ) .AND. ( IDAY .LT. DAY1 ) )

        RETURN

        END FUNCTION ISDSTIME

