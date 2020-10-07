
      INTEGER FUNCTION WKDAY( JDATE )

C***********************************************************************
C Version "$Id: wkday.F 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2003-2010 Baron Advanced Meteorological Systems,
C (C) 2007-2013 Carlie J. Coats, Jr., and
C (C) 2015 UNC Institute for the Environment.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  58
C
C  RETURNS:
C	day (1=Monday, ..., 7=Sunday) of the week for the given Julian date
C
C  PRECONDITIONS REQUIRED:
C	JDATE integer of the form  1000 * YEAR +  DAY
C
C  SUBROUTINES AND FUNCTIONS CALLED:  none
C
C  REVISION  HISTORY:
C	adapted  2/27/1995  by CJC from ROM 2.2 UTILIO function KDAY.FOR
C
C       Unification 2/2002 by CJC with global-climate DAYMON, which
C       uses a 360-day "year"
C
C       Modified 03/2010 by CJC: F90 changes for I/O API v3.1
C
C       Modified 11/2015 by CJC: IO_365 changes.  For IO_360 an IO_365,
C       assume Year 1900 Day 1 is a Monday.  Equivalently, Year 3 Day 1
C
C       Bug-fix 4/2016 by CJC for IO_365
C***********************************************************************

      IMPLICIT NONE


C...........   ARGUMENTS and their descriptions:

      INTEGER, INTENT(IN   ) :: JDATE	!  Julian date, YYYYDDD = 1000 * YEAR + DAY


C...........   PARAMETERS and their descriptions:

        INTEGER, PARAMETER :: YDAYS = 365

C...........   SCRATCH LOCAL VARIABLES and their descriptions:

      INTEGER  YEAR	!  year
      INTEGER  JDAY	!  Julian day-number 1 ... 365,366
      INTEGER  K	!  subexpression

#ifdef IO_360
#define     CLIMODAYS   (360)
#endif

#ifdef IO_365
#define     CLIMODAYS   (365)
#endif


C***********************************************************************
C   begin body of function  WKDAY

      YEAR  = JDATE / 1000
      JDAY  = MOD( JDATE, 1000 )

#ifdef CLIMODAYS
        K     = CLIMODAYS*( YEAR - 3 ) + MOD( JDAY + CLIMODAYS - 1, 7 )
        WKDAY = 1 + MOD (K, 7)
        RETURN
#endif

      K     = MAX( YEAR - 1, 0 )
      K     = K * YDAYS  +  K / 4  -  K / 100  +  K / 400  +  JDAY  -  1
      WKDAY = 1 + MOD (K, 7)

      RETURN
      END FUNCTION WKDAY

