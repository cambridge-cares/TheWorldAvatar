
      INTEGER FUNCTION LBLANK( STRING )

C***********************************************************************
C Version "$Id: lblank.f 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C    function body starts at line 42
C
C  FUNCTION:
C
C    Returns the number of leading blanks in STRING
C
C  REVISION HISTORY:
C
C       Adapted  09/1995 from ROM utility routine LEN2() by CJC
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C***********************************************************************

      IMPLICIT NONE
            
C.......   Arguments and their descriptions:

      CHARACTER*(*), INTENT(IN   ) :: STRING	!  Character string to search

C.......   PARAMETERs:

      CHARACTER*1, PARAMETER :: BLANK = ' '
      

C.......   Local variable:  loop counter

      INTEGER       I, L

C........................................................................
C.......   begin body:  Scan from left to right until non blank character

      L = LEN( STRING )
      DO  100  I = 1 , L

          IF ( ( STRING( I:I ) .NE. BLANK ) .AND.
     &         ( STRING( I:I ) .NE. CHAR( 9 ) ) ) THEN
              LBLANK = I - 1
              RETURN
          END IF

100   CONTINUE

      LBLANK = L
      RETURN

      END FUNCTION LBLANK

