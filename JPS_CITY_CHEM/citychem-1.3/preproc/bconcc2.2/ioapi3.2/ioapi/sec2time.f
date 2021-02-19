
        INTEGER  FUNCTION SEC2TIME( SECS )

C***********************************************************************
C Version "$Id: sec2time.f 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  39
C
C  FUNCTION:  convert integer seconds to time difference format HHMMSS
C
C  RETURN VALUE:  integer HHMMSS formatted secs
C
C  REVISION  HISTORY:  
C       Prototype  5/92 by CJC
C       Version    3/93 by CJC for CRAY, etc.
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C***********************************************************************

      IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: SECS


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER        	ABSS


C***********************************************************************
C   begin body of function  SEC2TIME

        IF ( SECS .GE. 0 ) THEN		!  div-mod arithmetic OK
            SEC2TIME = MOD( SECS, 60 )  +  
     &                   100 * ( MOD( SECS / 60, 60 )  +  
     &                   100 * ( SECS / 3600 ) )
        ELSE				!  work with absolute values:
            ABSS     = - SECS
            SEC2TIME = - ( MOD( ABSS, 60 )  +  
     &                       100 * ( MOD( ABSS / 60, 60 )  +  
     &                       100 * ( ABSS / 3600 ) ) )
        END IF

        RETURN

        END FUNCTION SEC2TIME

