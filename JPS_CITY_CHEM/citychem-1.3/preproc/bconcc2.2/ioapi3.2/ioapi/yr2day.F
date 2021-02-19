
        REAL FUNCTION YR2DAY( YEAR )

C********************************************************************
C Version "$Id: yr2day.F 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2003-2010 Baron Advanced Meteorological Systems,
C (C) 2007-2013 Carlie J. Coats, Jr., and
C (C) 2015 UNC Institute for the Environment.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C       function body starts at line  48
C
C  FUNCTION:
C
C      Returns the year to day conversion factor for a given year
C
C  ARGUMENT LIST DESCRIPTION:
C
C    Input arguments:
C
C        YEAR   - 4 digit year
C
C    Output arguments:  none
C
C  RETURNS   user response after checking its range; or default.
C
C  REVISION HISTORY:
C
C       Created 4/1997 by M Houyoux, MCNC Environmental Programs
C
C       Unification 2/2002 by CJC with global-climate DAYMON, which
C       uses a 360-day "year"
C
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C
C       Modified 11/2015 by CJC: IO_365 changes
C****************************************************************************

        IMPLICIT NONE

C.......   ARGUMENTS:

        INTEGER, INTENT(IN   ) :: YEAR  ! 4 digit year YYYY

C......................................................................
C       begin YR2DAY

#ifdef IO_360
        YR2DAY = 1.0 / 360.0
        RETURN
#endif

#ifdef IO_365
        YR2DAY = 1.0 / 365.0
        RETURN
#endif

        IF ( MOD( YEAR,4 ) .NE. 0 ) THEN           !  nonleap years
            YR2DAY = 1.0 / 365.0
        ELSE IF ( MOD( YEAR,100 ) .NE. 0 ) THEN    !  noncentury leap years
            YR2DAY = 1.0 / 366.0
        ELSE IF ( MOD( YEAR,400 ) .NE. 0 ) THEN    !  century nonleap years
            YR2DAY = 1.0 / 365.0
        ELSE                                       !  leap centuries
            YR2DAY = 1.0 / 366.0
        END IF

        RETURN

        END FUNCTION YR2DAY

