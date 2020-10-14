
        SUBROUTINE  M3ERR ( CALLER, JDATE, JTIME, MSGTXT, FATAL )

C***********************************************************************
C Version "$Id: m3err.F 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C
C       DEPRECATED!!
C       Use M3EXIT(), instead.
C
C  subroutine body starts at line  51
C                                    
C  FUNCTION:  Generate simple error messages for Models-3 core;
C             terminate program execution via exit( 2 ) iff FATAL
C
C  PRECONDITIONS REQUIRED:  
C       JDATE:JTIME represented as YYYYDDD:HHMMSS
C
C  SUBROUTINES AND FUNCTIONS CALLED:  DT2STR, INIT3, SHUT3
C
C  REVISION  HISTORY:	
C	prototype 5/92 by CJC
C	Revised   8/96 to close currently-open POSIX-OK Fortran units.
C       Modified  1/97 by CJC to trim trailing blanks from MSGTXT
C       Modified 10/98 by CJC:  rewritten in terms of m3exit(), m3warn()  
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'IODECL3.EXT'


C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*), INTENT(IN   ) :: CALLER          !  name of the caller
        INTEGER      , INTENT(IN   ) :: JDATE, JTIME    !  model date&time for the error
        CHARACTER*(*), INTENT(IN   ) :: MSGTXT          !  error message
        LOGICAL      , INTENT(IN   ) :: FATAL           !  terminate program iff TRUE

                 
C***********************************************************************
C   begin body of subroutine  M3ERR

        IF ( FATAL ) THEN
            CALL M3EXIT( CALLER, JDATE, JTIME, MSGTXT, 2 )
        ELSE    !  not endflag
            CALL M3WARN( CALLER, JDATE, JTIME, MSGTXT )
        END IF
        RETURN

        END  SUBROUTINE  M3ERR

