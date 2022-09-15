
        CHARACTER*2 FUNCTION CRLF()

C***********************************************************************
C Version "$Id: crlf.F 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2003-2010 Baron Advanced Meteorological Systems
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  subroutine body starts at line  36
C
C  FUNCTION:  Return character string of character return and line feed
C
C  PRECONDITIONS REQUIRED:
C
C  SUBROUTINES AND FUNCTIONS CALLED:  none
C
C  REVISION  HISTORY:
C       prototype 1/97 by M Houyoux
C       Modified 5/2003 by Carlie J. Coats, Jr., BAMS,  so that it
C       works correctly for "normal" UNIX systems.
C****************************************************************************

        IMPLICIT NONE

        CHARACTER*2 VALUE
        SAVE        VALUE

        LOGICAL, SAVE :: FIRSTIME = .TRUE.

C***********************************************************************
C   begin body of subroutine CRLF

        IF( FIRSTIME ) THEN
            FIRSTIME = .FALSE.
#ifdef _WIN32
            VALUE( 1:1 ) = CHAR( 13 )
            VALUE( 2:2 ) = CHAR( 10 )
#endif
#ifndef _WIN32
            VALUE( 1:1 ) = ' '
            VALUE( 2:2 ) = CHAR( 10 )
#endif
        ENDIF

        CRLF = VALUE

        RETURN

        END FUNCTION CRLF
