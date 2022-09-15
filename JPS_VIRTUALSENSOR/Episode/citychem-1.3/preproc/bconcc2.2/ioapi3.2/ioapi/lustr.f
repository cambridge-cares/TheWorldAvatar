
        SUBROUTINE LUSTR( STRING )

C***********************************************************************
C Version "$Id: lustr.f 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  subroutine body starts at line  50
C
C  FUNCTION:
C       left-justify and upcase contents of STRING
C
C  PRECONDITIONS REQUIRED: none
C
C  SUBROUTINES AND FUNCTIONS CALLED: none
C
C  REVISION  HISTORY:
C       Prototype 6/1995 by CJC
C
C       Modified 03/2010 by CJC: F90 changes for I/O API v3.1
C***********************************************************************

      IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*), INTENT(INOUT) :: STRING


C...........   PARAMETERS and their descriptions:

        CHARACTER*1, PARAMETER :: BLANK = ' '
        INTEGER    , PARAMETER :: IDIFF = 65 - 97       ! = ichar( 'A' ) -  ichar( 'a' )


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         ILEN
        INTEGER         I, J, IBEG
        CHARACTER*1     CH


C***********************************************************************
C   begin body of subroutine  LUSTR

        ILEN = LEN( STRING )
        DO I = 1, ILEN
            IF( STRING(I:I) .NE. BLANK ) THEN
                IBEG = I
                GO TO  11
            END IF
        END DO

C.......   If you get to here:  string all blanks.  No action necessary.

        RETURN

11      CONTINUE

C.......   Go thru rest of string, replacing lower-case by corresponding upper

        J = 1
        DO  I = IBEG, ILEN
            CH = STRING( I:I )
            IF ( CH .GE. 'a'  .AND.  CH .LE. 'z' ) THEN
                STRING(J:J) = CHAR( ICHAR( CH ) + IDIFF )
            ELSE
                STRING(J:J) = CH
            END IF
            J = J + 1
        END DO

C.......    pad trailing section of string with blank

        STRING(J:) = BLANK

        RETURN
        END SUBROUTINE LUSTR

