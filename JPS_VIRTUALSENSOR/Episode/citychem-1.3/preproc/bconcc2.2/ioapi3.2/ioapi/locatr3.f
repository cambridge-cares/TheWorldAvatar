
        INTEGER FUNCTION LOCATR3( K1, K2, K3, N, LIST1, LIST2, LIST3 )

C***********************************************************************
C Version "$Id: locatr3.f 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line 55
C
C  RETURNS:
C       subscript at which the targeted REAL key-triple should be inserted
C       or -1 if the key-triple is found.
C       EXAMPLE:  search for <FIP,ID7,ID3> in table of FIP and ASC values
C       (where ID7 is leading-7 digits and ID3 is trailing 3 digits
C       in a 10-digit Area Source Code).
C
C  PRECONDITIONS REQUIRED:
C       Sorted table <N,LIST1,LIST3,LIST3> for searching
C
C  SUBROUTINES AND FUNCTIONS CALLED:  none
C
C  REVISION  HISTORY:
C       prototype 12/95 by MRH copied from FIND3 of CJC
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C***********************************************************************

      IMPLICIT NONE


C...........   ARGUMENTS and their descriptions:
        
        REAL   , INTENT(IN   ) :: K1             !  first  key
        REAL   , INTENT(IN   ) :: K2             !  second key
        REAL   , INTENT(IN   ) :: K3             !  third  key
        INTEGER, INTENT(IN   ) :: N              !  table size
        REAL   , INTENT(IN   ) :: LIST1( N )     !  table to search for K1
        REAL   , INTENT(IN   ) :: LIST2( N )     !  table to search for K2
        REAL   , INTENT(IN   ) :: LIST3( N )     !  table to search for K3


C...........   SCRATCH LOCAL VARIABLES and their descriptions:
        
        INTEGER  LO
        INTEGER  HI
        INTEGER  M


C***********************************************************************
C   begin body of function  LOCATR3

        LO = 1
        HI = N

        IF( N .EQ. 0 ) THEN

            LOCATR3 = -1
            RETURN

        ENDIF 

11      CONTINUE
           
            IF ( LO .GT. HI ) THEN
            
                LOCATR3 = LO
                RETURN
                
            END IF
           
            M = ( LO + HI ) / 2
            IF ( K1 .GT. LIST1( M ) ) THEN
                LO = M + 1
                GO TO  11
            ELSE IF ( K1 .LT. LIST1( M ) ) THEN
                HI = M - 1
                GO TO  11
            ELSE IF ( K2 .GT. LIST2( M ) ) THEN
                LO = M + 1
                GO TO  11
            ELSE IF ( K2 .LT. LIST2( M ) ) THEN
                HI = M - 1
                GO TO  11
            ELSE IF ( K3 .GT. LIST3( M ) ) THEN
                LO = M + 1
                GO TO  11
            ELSE IF ( K3 .LT. LIST3( M ) ) THEN
                HI = M - 1
                GO TO  11
            END IF          !  end of bin search loop for this <K1,K2,K3>
        
        
        LOCATR3 = -1         ! key-tuple found
        RETURN
        END FUNCTION LOCATR3

