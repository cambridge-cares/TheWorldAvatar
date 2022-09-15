
        INTEGER FUNCTION STR2INT( STRING )

C***********************************************************************
C Version "$Id: str2int.f 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line 57
C
C  RETURNS:
C       INTEGER value decoded from STRING, or IMISS3 for "missing",
C       after skipping leading blanks.
C
C  PRECONDITIONS REQUIRED:
C       Properly formatted integer in STRING
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       M3WARN()
C
C  REVISION  HISTORY:
C       Prototype 6/95 by CJC for point source prototype
C
C       Modified 03/2010 by CJC: F90 changes for I/O API v3.1
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'


C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*), INTENT(IN   ) :: STRING


C...........   PARAMETERS
            
        CHARACTER*1, PARAMETER :: BLANK = ' '
        
C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         SUM, SIGN
        INTEGER         I, J, K, L
        INTEGER         IC, I0
        CHARACTER*80    MESG


C***********************************************************************
C   begin body of function  STR2INT

        L = LEN( STRING )
            
        DO  11  I = 1, L        !  skip leading whitespace
            IF ( STRING( I:I ) .GT. BLANK ) GO TO 12
11      CONTINUE

C.......   If you get to here:  no number there

        STR2INT = IMISS3
        RETURN

12      CONTINUE

        IF( STRING( I:I ) .EQ. '-' ) THEN       !  adjust for sign
            SIGN = -1
            I    = I + 1
        ELSE IF( STRING( I:I ) .EQ. '+' ) THEN
            SIGN = 1
            I    = I + 1
        ELSE
            SIGN = 1
        END IF
        
        SUM = 0         !  accumulate as long as there are digits.
        K   = 0
        I0  = ICHAR( '0' )
        DO  22  J = I, L
            IC = ICHAR( STRING( J:J ) ) - I0
            IF ( IC .LT. 0  .OR.  IC .GT. 9 )  GO TO  23
            SUM = 10 * SUM  +  IC
            K   = K   +  1
22      CONTINUE
23      CONTINUE

        IF ( K .GT. 0 ) THEN
            STR2INT = SIGN * SUM
        ELSE
            MESG = 'No digits in  "' // STRING // '"'
            CALL M3WARN( 'STR2INT', 0, 0, MESG )
            STR2INT = IMISS3
        END IF
        
        RETURN
        END FUNCTION STR2INT

