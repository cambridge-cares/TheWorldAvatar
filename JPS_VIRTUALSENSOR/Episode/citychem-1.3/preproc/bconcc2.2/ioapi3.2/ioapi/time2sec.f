
        INTEGER  FUNCTION  TIME2SEC ( TIME )

C***********************************************************************
C Version "$Id: time2sec.f 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  40
C
C  FUNCTION:  convert time difference format HHMMSS to integer seconds 
C
C  RETURN VALUE:  seconds
C
C  PRECONDITION:  integer TIME formatted HHMMSS
C
C  REVISION  HISTORY:  
C      Prototype  5/1992 by CJC
C      Version    2/1993 by CJC for CRAY, etc.
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C***********************************************************************

      IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: TIME

C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER      	ABST


C***********************************************************************
C   begin body of function  TIME2SEC

        IF ( TIME .GE. 0 ) THEN		!  div-mod arithmetic OK
            TIME2SEC = MOD( TIME, 100 )  +  
     &                      60 * ( MOD( TIME / 100, 100 )  +  
     &                             60 * ( TIME / 10000 ) )
        ELSE				!  work with absolute values:
            ABST     = - TIME
            TIME2SEC = - ( MOD( ABST, 100 )  +  
     &                          60 * ( MOD( ABST / 100, 100 )  +  
     &                                 60 * ( ABST / 10000 ) ) )
        END IF

        RETURN

        END FUNCTION TIME2SEC

