
        SUBROUTINE  PGRDSUM( NCOLS, NROWS, NCOFF, N, I, PTR, U, V )

C***********************************************************************
C Version "$Id: pgrdsum.f 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  subroutine body starts at line  50
C
C  FUNCTION:  multiply a sparse matrix <N,I> by a vector U, sum and 
C             then return the result V
C
C  PRECONDITIONS REQUIRED:  none
C
C  SUBROUTINES AND FUNCTIONS CALLED:  none
C
C  REVISION  HISTORY:
C       prototype 4/97 by JMV
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C****************************************************************************

      IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:
        
        INTEGER, INTENT(IN   ) :: NCOLS           ! length of input vector
        INTEGER, INTENT(IN   ) :: NROWS           ! length of output vector
        INTEGER, INTENT(IN   ) :: NCOFF           ! max number of coefficients
        
        INTEGER, INTENT(IN   ) :: N( NROWS )      ! # of entries per row
        INTEGER, INTENT(IN   ) :: I( NCOFF )      ! columns list

        INTEGER, INTENT(IN   ) :: PTR ( NCOLS )   !  summation pointer
        REAL   , INTENT(IN   ) :: U( NCOLS )      !  input vector
        REAL   , INTENT(  OUT) :: V( NROWS )      ! output vector


C...........   SCRATCH LOCAL VARIABLES and their descriptions:
        
        INTEGER         R, C, K, L, P   


C***********************************************************************
C   begin body of subroutine  PSUMGRD

        K = 0

        DO  22  R = 1, NROWS
            
            DO  11  C = 1, N( R )
                K = K + 1
                L = I( K ) 
                P = PTR( L )
                V( P ) = V( P ) + U( L )
11          CONTINUE
            
22      CONTINUE

        RETURN

        END SUBROUTINE PGRDSUM

