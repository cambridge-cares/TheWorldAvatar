
      INTEGER FUNCTION LEN2 (J1, J2, STRING)

C***********************************************************************
C Version "$Id: len2.f 1 2017-06-10 18:05:20Z coats $"
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
C    Returns the number of leading blanks in STRING( J1:J2 )
C
C  REVISION HISTORY:
C
C       5/88   Modified for ROMNET
C       8/90   Modified for ROM 2.2:  simpler algorithm uses DO loop.
C       2/93   Modified for CRAY by CJC.
C       9/94   Simpler algorithm for Models-3 by CJC
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C***********************************************************************

      IMPLICIT NONE
            
C.......   Arguments and their descriptions:

      INTEGER      , INTENT(IN   ) :: J1        !  First position in string to be searched
      INTEGER      , INTENT(IN   ) :: J2        !  Last     "
      CHARACTER*(*), INTENT(IN   ) :: STRING    !  Character string to search

C.......   Local variable:  loop counter

      INTEGER       I

C........................................................................
C.......   begin body:  Scan from left to right until non blank character

      DO  100  I = J1 , J2

          IF ( STRING ( I:I ) .NE. ' ' )  THEN
              LEN2 = I - J1
              RETURN
          END IF

100   CONTINUE

      LEN2 = J2 - J1 + 1
      RETURN

      END FUNCTION LEN2

