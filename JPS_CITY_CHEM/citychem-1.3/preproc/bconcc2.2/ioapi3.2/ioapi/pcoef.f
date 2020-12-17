
        SUBROUTINE  PCOEF( N, X, Y, C )

C***********************************************************************
C Version "$Id: pcoef.f 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  subroutine body starts at line  54
C
C  FUNCTION:  returns array C of polynomial coefficients
C	for the polynomial fitting the data 
C       Y( K ) = P( X( K ) ) = sum_I
C
C  PRECONDITIONS REQUIRED:  none
C
C  SUBROUTINES AND FUNCTIONS CALLED:  none
C
C  REVISION  HISTORY:
C	Prototype 11/95 by CJC adapted from _Numerical_Recipes_,
C	section 3.5 (pp. 92-94), routine POLCOE
C       Modified 03/2010 by CJC: F90 changes for I/O API v3.1
C***********************************************************************

      IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: N
        REAL   , INTENT(IN   ) :: X( N )
        REAL   , INTENT(IN   ) :: Y( N )
        REAL   , INTENT(  OUT) :: C( N )


C...........   PARAMETERS and their descriptions:

        INTEGER, PARAMETER :: NMAX = 15


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER		I, J
        REAL		P, F, B
        REAL		S( NMAX )
        REAL		T( NMAX )
        CHARACTER*80    MESG

C***********************************************************************
C   begin body of subroutine  PCOEF

        IF ( N .GT. NMAX ) THEN
            WRITE( MESG, '(A, I5, 2X, A )' )
     &          'Requested degree', N, 'exceeds max=15'
            CALL M3EXIT( 'PCOEF', 0, 0, MESG, 2 )
        END IF

        DO  11  I = 1, N

            S( I ) = 0.0
            C( I ) = 0.0
            T( I ) = FLOAT( I )

11      CONTINUE
        
        S( N ) = -X( 1 )
        
        DO  22  I = 2, N
        DO  21  J = N - I + 1, N - 1

            S( J ) = S( J ) - X( I ) * S( J+1 )

21      CONTINUE
22      CONTINUE

        DO  33  I = 1, N

            P = T( N )

            DO  31  J = N - 1, 1, -1

                P = T( J ) * S( J+1 ) + P * X( I )

31          CONTINUE		!  end loop on J

            F = Y( I ) / P
            B = 1.0

            DO  32  J = N, 1, -1

                C( J ) = C( J ) + B * F
                B      = S( J ) + B * X( I )

32          CONTINUE	!  end loop on J

33      CONTINUE	!  end loop on I

        RETURN

        END SUBROUTINE PCOEF

