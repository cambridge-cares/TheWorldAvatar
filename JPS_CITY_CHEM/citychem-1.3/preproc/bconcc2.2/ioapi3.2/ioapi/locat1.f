
        INTEGER FUNCTION LOCAT1( K1, N, LIST1 )

C***********************************************************************
C Version "$Id: locat1.f 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (c) 2004-2007 Baron Advanced Meteorological Systems,
C (c) 2007-2013 Carlie J. Coats, Jr., and
C (C) 2014 UNC Institute for the Environment.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  LOCAT1()   function body starts at line  54
C  LOCATL1()  function body starts at line 113
C
C  RETURNS:
C       subscript at which the targeted key should be inserted
C       or -1 if the key is found.
C       EXAMPLE:  search for <FIP> in table of FIP values.
C
C  PRECONDITIONS REQUIRED:
C       LOCAT1()  for INTEGER   key-tuples and lists
C       LOCATL1() for INTEGER*8 key-tuples and lists
C       Sorted table <N,LIST1> for searching
C
C  SUBROUTINES AND FUNCTIONS CALLED:  none
C
C  REVISION  HISTORY:
C       prototype 12/95 by MRH copied from FIND1 of CJC
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C***********************************************************************

      IMPLICIT NONE


C...........   ARGUMENTS and their descriptions:
        
        INTEGER, INTENT(IN   ) :: K1             !  first  key
        INTEGER, INTENT(IN   ) :: N              !  table size
        INTEGER, INTENT(IN   ) :: LIST1( N )     !  table to search for K1


C...........   SCRATCH LOCAL VARIABLES and their descriptions:
        
        INTEGER  LO
        INTEGER  HI
        INTEGER  M

C.......................................................................
C   begin body of function  LOCAT1

        LO = 1
        HI = N

        IF( N .EQ. 0 ) THEN

            LOCAT1 = -1
            RETURN

        ENDIF 

11      CONTINUE
           
            IF ( LO .GT. HI ) THEN
            
                LOCAT1 = LO
                RETURN
                
            END IF
           
            M = ( LO + HI ) / 2
            IF ( K1 .GT. LIST1( M ) ) THEN
                LO = M + 1
                GO TO  11
            ELSE IF ( K1 .LT. LIST1( M ) ) THEN
                HI = M - 1
                GO TO  11
            END IF          !  end of bin search loop for this <K1>
        
        
        LOCAT1 = -1         ! key found
        RETURN
        END FUNCTION LOCAT1


C***********************************************************************


        INTEGER FUNCTION LOCATL1( K1, N, LIST1 )

        IMPLICIT NONE


C...........   ARGUMENTS and their descriptions:
        
        INTEGER(8), INTENT(IN   ) :: K1             !  first  key
        INTEGER   , INTENT(IN   ) :: N              !  table size
        INTEGER(8), INTENT(IN   ) :: LIST1( N )     !  table to search for K1


C...........   SCRATCH LOCAL VARIABLES and their descriptions:
        
        INTEGER  LO
        INTEGER  HI
        INTEGER  M


C.......................................................................
C   begin body of function  LOCATL1

        LO = 1
        HI = N

        IF( N .EQ. 0 ) THEN

            LOCATL1 = -1
            RETURN

        ENDIF 

11      CONTINUE
           
            IF ( LO .GT. HI ) THEN
            
                LOCATL1 = LO
                RETURN
                
            END IF
           
            M = ( LO + HI ) / 2
            IF ( K1 .GT. LIST1( M ) ) THEN
                LO = M + 1
                GO TO  11
            ELSE IF ( K1 .LT. LIST1( M ) ) THEN
                HI = M - 1
                GO TO  11
            END IF          !  end of bin search loop for this <K1>
        
        
        LOCATL1 = -1         ! key found
        RETURN
        END FUNCTION LOCATL1


