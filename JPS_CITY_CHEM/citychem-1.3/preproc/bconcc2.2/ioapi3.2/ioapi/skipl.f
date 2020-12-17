
       SUBROUTINE SKIPL( UNIT, NLINES )

C***********************************************************************
C Version "$Id: skipl.f 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (c) 2004-2007 Baron Advanced Meteorological Systems,
C (c) 2007-2013 Carlie J. Coats, Jr., and (C) 2014 UNC Institute
C for the Environment.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  subroutine body starts at line  44
C
C  SUBROUTINE:  Skips NLINES number of lines in file UNIT
C
C  PRECONDITIONS REQUIRED:
C
C  SUBROUTINES AND FUNCTIONS CALLED:  none
C
C  REVISION  HISTORY:
C       prototype 3/97 by M Houyoux for SMOKE
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C       Modified 02/2014 by CJC: Fix MH violation of coding-standards:
C       check status ISTAT from  READ()!!
C****************************************************************************

        IMPLICIT NONE

C.........  Subroutine arguments

        INTEGER, INTENT(IN   ) :: UNIT
        INTEGER, INTENT(IN   ) :: NLINES

C.........  Local variables

        INTEGER    I, ISTAT
        CHARACTER*256   MESG

C***********************************************************************
C   begin body of subroutine SKIPL

        DO I = 1, NLINES
            READ( UNIT, *, IOSTAT=ISTAT )
            IF ( ISTAT .NE. 0 ) THEN
                WRITE( MESG, '( A, I4, 2X, A, I9 )' )
     &                'Error reading unit', UNIT,
     &                'IOSTAT=', ISTAT
                CALL M3EXIT( 'SKIPL',0,0, MESG, 2)
            END IF
        END DO

        RETURN

        END SUBROUTINE SKIPL
