
      INTEGER FUNCTION JULIAN (YEAR, MNTH, MDAY)

C***********************************************************************
C Version "$Id: julian.F 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2003-2010 Baron Advanced Meteorological Systems,
C (C) 2007-2013 Carlie J. Coats, Jr., and
C (C) 2015 UNC Institute for the Environment.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  68
C
C  FUNCTION:  returns the Julian day (1...365,366) corresponding to
C      the date MNTH-MDAY-YEAR.
C      NOTE:  This is NOT the Julian DATE -- only the
C      day-number.  To get the Julian date:
C
C      JDATE = 1000 * YEAR  +  JULIAN ( YEAR , MNTH , MDAY )
C
C  ARGUMENT LIST DESCRIPTION:
C
C    Input arguments:
C
C      YEAR     Calendar year
C      MNTH     Month of year  1, 12
C      MDAY     Day of month   1, 31
C
C     Output arguments:  none
C
C  RETURN VALUE:
C
C      JULIAN   The Julian DAY of the input arguments combined
C
C  REVISION HISTORY:
C
C    5/1988   Modified for ROMNET
C
C    8/1990   Modified for ROM 2.2 by Carlie J. Coats, Jr., CSC
C       improved comments; improved Zeller's Congruence algorithm
C       and using IF-THEN ... ELSE IF ... construction.
C
C    8/1999   Version for global-climate IO_360, which uses 360-day "year"
C
C       2/2002 Unification by CJC with global-climate JULIAN
C
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C
C       Modified 11/2015 by CJC: IO_365 changes
C***********************************************************************

      IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: YEAR            ! year YYYY
        INTEGER, INTENT(IN   ) :: MNTH            ! month 1...12
        INTEGER, INTENT(IN   ) :: MDAY            ! day-of-month 1...28,29,30,31

C...........   SCRATCH LOCAL VARIABLES:

        INTEGER   M, N, L


C***********************************************************************
C   begin body of function  JULIAN

C...........   Climatology-year calculations:

#ifdef IO_360

        JULIAN = MDAY + 30 * ( MNTH - 1 )
        RETURN

#endif

        M = MOD ((MNTH + 9), 12)
        N = (M * 153 + 2) / 5 + MDAY + 58

#ifdef IO_365

        JULIAN = 1 + MOD (N, 365)
        RETURN

#endif


C...........   Otherwise

        M = MOD ((MNTH + 9), 12)
        N = (M * 153 + 2) / 5 + MDAY + 58

        IF      ( MOD (YEAR,   4) .NE. 0 ) THEN
            L = 365
        ELSE IF ( MOD (YEAR, 100) .NE. 0 ) THEN
            L = 366
            N = 1 + N
        ELSE IF ( MOD (YEAR, 400) .NE. 0 )  THEN
            L = 365
        ELSE
            L = 366
            N = 1 + N
        END IF
        JULIAN = 1 + MOD (N, L)

      RETURN
      END FUNCTION JULIAN

