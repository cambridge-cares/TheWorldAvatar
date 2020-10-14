
        REAL FUNCTION STR2REAL( STRING )

C***********************************************************************
C Version "$Id: str2real.f 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems, and
C (C) 2016 UNC Institute for the Environment
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  59
C
C  RETURNS:
C       REAL value decoded from STRING, or BADVAL3 for "missing",
C       after skipping leading blanks.
C
C  PRECONDITIONS REQUIRED:
C       Properly formatted REAL in STRING
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       M3ERR()
C
C  REVISION  HISTORY:
C       Prototype 6/95 by CJC for point source prototype
C
C       Modified 03/20010 by CJC: F90 changes for I/O API v3.1
C
C       Modified 03/20016 by CJC: F90 change:  READ( STRING, *,...)
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'


C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*), INTENT(IN   ) :: STRING


C...........   PARAMETERS
            
        CHARACTER*1, PARAMETER :: BLANK = ' '
        
C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        REAL		    VAL
        INTEGER         IOS
        CHARACTER*80    MSG


C***********************************************************************
C   begin body of function  STR2REAL

        IF ( STRING .EQ. BLANK ) THEN
            STR2REAL = BADVAL3
            RETURN
        END IF                

        READ( STRING, *, IOSTAT = IOS ) VAL

        IF( IOS .NE. 0 ) THEN
            WRITE( MSG, '( 3A, I10 )' ) 
     &          'Error reading REAL from "', TRIM( STRING ), 
     &          '"; IOSTAT=', IOS
            CALL M3WARN( 'STR2REAL', 0, 0, MSG )
            STR2REAL = BADVAL3
        ELSE
            STR2REAL = VAL
        END IF
        
        RETURN

        END FUNCTION STR2REAL

