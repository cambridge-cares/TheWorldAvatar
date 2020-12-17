
        LOGICAL FUNCTION CHKBUF3( FDUM )

C***********************************************************************
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2011 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  95
C
C  FUNCTION:  Check consistency pf BUFFERED file treatment between
C             libioapi.a and model-code
C
C  RETURN VALUE:  TRUE iff consistent
C
C  PRECONDITIONS REQUIRED:  call after INIT3()
C
C  REVISION  HISTORY:
C       prototype 04/2011 by Carlie J. Coats, Jr.
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'STATE3.EXT'

C...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(  OUT) :: FDUM            !  prevents excessive optimization

C.............................................................................
C   begin body of subroutine  CHKBUF3

        FDUM    = VGTYP3( 1 )
        CHKBUF3 = .TRUE.
        RETURN
        
        END FUNCTION CHKBUF3
