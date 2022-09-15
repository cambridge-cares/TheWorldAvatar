
        INTEGER FUNCTION FINDR1( K, N, LIST )

C***********************************************************************
C Version "$Id: findr1.f 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line 49
C
C  RETURNS:
C       subscript at which the targeted key appears, or 
C       -1 in case of failure.
C       EXAMPLE:  search for FIP in table of FIP values
C
C  PRECONDITIONS REQUIRED:
C       Sorted table <N,LIST> for searching
C
C  SUBROUTINES AND FUNCTIONS CALLED:  none
C
C  REVISION  HISTORY:
C       prototype 3/1/95 by CJC
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C***********************************************************************

      IMPLICIT NONE


C...........   ARGUMENTS and their descriptions:
        
        REAL   , INTENT(IN   ) :: K             !  first  key
        INTEGER, INTENT(IN   ) :: N             !  table size
        REAL   , INTENT(IN   ) :: LIST( N )     !  table to search for K


C...........   SCRATCH LOCAL VARIABLES and their descriptions:
        
        INTEGER  LO
        INTEGER  HI
        INTEGER  M


C***********************************************************************
C   begin body of function  FINDR1

        LO = 1
        HI = N
        
11      CONTINUE
            
            IF ( LO .GT. HI ) THEN
            
                FINDR1 = -1
                RETURN
                
            END IF
           
            M = ( LO + HI ) / 2
            IF ( K .GT. LIST( M ) ) THEN
                LO = M + 1
                GO TO  11
            ELSE IF ( K .LT. LIST( M ) ) THEN
                HI = M - 1
                GO TO  11
            END IF          !  end of bin search loop for this K
        
        
        FINDR1 = M
        RETURN
        END FUNCTION FINDR1

