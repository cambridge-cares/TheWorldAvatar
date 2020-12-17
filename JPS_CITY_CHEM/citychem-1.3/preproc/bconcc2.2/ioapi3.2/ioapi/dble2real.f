
        SUBROUTINE DBLE2REAL( SIZE, DBLG, GRID )

C***********************************************************************
C Version "$Id: dble2real.f 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  subroutine body starts at line  44
C
C  FUNCTION:
C	convert INTEGER input array DBLG( SIZE ) to REAL
C
C  PRECONDITIONS REQUIRED:
C	none
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C	none
C
C  REVISION  HISTORY:
C	prototype 6/1995 by CJC
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C***********************************************************************

      IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:
        
        INTEGER		, INTENT(IN   ) :: SIZE		!  array dimension
        DOUBLE PRECISION, INTENT(IN   ) :: DBLG( SIZE )	!  input double array
        REAL		, INTENT(  OUT) :: GRID( SIZE )	! output real   array

C...........   SCRATCH LOCAL VARIABLES and their descriptions:
        
        INTEGER		I


C***********************************************************************
C   begin body of subroutine  INT2REAL

        DO  11  I = 1, SIZE
            GRID( I ) = REAL( DBLG( I ) )
11      CONTINUE

        RETURN
        END SUBROUTINE DBLE2REAL

