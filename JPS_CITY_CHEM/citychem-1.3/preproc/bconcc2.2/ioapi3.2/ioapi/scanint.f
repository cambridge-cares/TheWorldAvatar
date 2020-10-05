
        SUBROUTINE SCANINT( STRING, VALUE, NCHARS, NDIGITS )

C***********************************************************************
C Version "$Id: scanint.f 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  subroutine body starts at line 68
C
C  RETURNS:
C       INTEGER
C               VALUE decoded from STRING, or IMISS3 for "missing",
C               after skipping leading blanks.
C               NCHARS the number of characters consumed (including
C               leading whitespace
C               NDIGITS the number of digits (counting leading
C               minus-sign, if any)
C
C  PRECONDITIONS REQUIRED:
C       ASCII.
C       Properly formatted integer in STRING.  In particular:
C         * no whitespace between sign and digits composing the rest
C           of the value; and
C         * leading whitespace is OK (and is skipped over and counted);
C           whitespace is defined to be characters <= BLANK
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       M3WARN()
C
C  REVISION  HISTORY:
C       Adapted 7/2001 by CJC from STR2INT()
C
C       Modified 03/2010 by CJC: F90 changes for I/O API v3.1
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'


C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*), INTENT(IN   ) :: STRING
        INTEGER      , INTENT(  OUT) :: VALUE, NCHARS, NDIGITS


C...........   PARAMETERS
            
        CHARACTER*1, PARAMETER :: BLANK = ' '
        
C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         SUM, SIGN
        INTEGER         I, J, K, L
        INTEGER         IC, I0
        CHARACTER*256   MESG


C***********************************************************************
C   begin body of function  SCANINT

        L = LEN( STRING )
            
        DO  11  I = 1, L        !  skip leading whitespace
            IF ( STRING( I:I ) .GT. BLANK ) GO TO 12
11      CONTINUE

C.......   If you get to here:  no number there

        VALUE   = IMISS3
        NCHARS  = L
        NDIGITS = 0
        RETURN

12      CONTINUE

        IF( STRING( I:I ) .EQ. '-' ) THEN       !  adjust for sign
            SIGN    = -1
            I       = I + 1
            NDIGITS = 1
        ELSE IF( STRING( I:I ) .EQ. '+' ) THEN
            SIGN    = 1
            I       = I + 1
            NDIGITS = 0
        ELSE
            SIGN = 1
            NDIGITS = 0
        END IF
        NCHARS  = I
        
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
        NCHARS  = NCHARS  + K
        NDIGITS = NDIGITS + K

        IF ( K .GT. 0 ) THEN
            VALUE = SIGN * SUM
        ELSE
            MESG = 'No digits in  "' // STRING // '"'
            CALL M3WARN( 'SCANINT', 0, 0, MESG )
            VALUE = IMISS3
        END IF
        
        RETURN
        END SUBROUTINE SCANINT

