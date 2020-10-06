
        INTEGER FUNCTION FIND1( K, N, LIST )

C***********************************************************************
C Version "$Id: find1.f 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (c) 2004-2007 Baron Advanced Meteorological Systems,
C (c) 2007-2013 Carlie J. Coats, Jr., and (C) 2014 UNC Institute
C for the Environment.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  FIND1()  function body starts at line 54
C  FINDL1() function body starts at line 83
C
C  RETURNS:
C       subscript at which the targeted key appears, or 
C       -1 in case of failure.
C       EXAMPLE:  search for FIP in table of FIP values
C
C  PRECONDITIONS REQUIRED:
C       FIND1() for INTEGER  K, LIST; FINDL1() for INTEGER*8 K, LIST
C       Sorted table <N,LIST> for searching
C
C  SUBROUTINES AND FUNCTIONS CALLED:  none
C
C  REVISION  HISTORY:
C       prototype 3/1/95 by CJC
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C       Version  08/2014 by CJC: Add FINDL1()
C***********************************************************************

      IMPLICIT NONE


C...........   ARGUMENTS and their descriptions:
        
        INTEGER, INTENT(IN   ) :: K             !  first  key
        INTEGER, INTENT(IN   ) :: N             !  table size
        INTEGER, INTENT(IN   ) :: LIST( N )     !  table to search for K


C...........   SCRATCH LOCAL VARIABLES and their descriptions:
        
        INTEGER  LO
        INTEGER  HI
        INTEGER  M


C.......................................................................
C   begin body of function  FIND1

        LO = 1
        HI = N
        
11      CONTINUE
            
            IF ( LO .GT. HI ) THEN
            
                FIND1 = -1
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
        
        
        FIND1 = M
        RETURN
        END FUNCTION FIND1


C***********************************************************************

        INTEGER FUNCTION FINDL1( K, N, LIST )


      IMPLICIT NONE


C...........   ARGUMENTS and their descriptions:
        
        INTEGER(8), INTENT(IN   ) :: K             !  first  key
        INTEGER,    INTENT(IN   ) :: N             !  table size
        INTEGER(8), INTENT(IN   ) :: LIST( N )     !  table to search for K


C...........   SCRATCH LOCAL VARIABLES and their descriptions:
        
        INTEGER  LO, HI, M


C.......................................................................
C   begin body of function  FINDL1

        LO = 1
        HI = N
        
11      CONTINUE
            
            IF ( LO .GT. HI ) THEN
            
                FINDL1 = -1
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
        
        
        FINDL1 = M
        RETURN
        END FUNCTION FINDL1
