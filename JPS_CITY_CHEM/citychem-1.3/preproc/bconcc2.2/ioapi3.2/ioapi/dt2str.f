
        CHARACTER*24 FUNCTION  DT2STR( JDATE, JTIME )

C***********************************************************************
C Version "$Id: dt2str.f 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2003-2010 Baron Advanced Meteorological Systems,
C (C) 2007-2013 Carlie J. Coats, Jr., and 
C (C) 2015 UNC Institute for the Environment.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  60
C
C  FUNCTION:  format and return the date and time as a character string
C             "HH:MM:SS  M+ D+, YYYY"
C
C
C  PRECONDITIONS REQUIRED:  valid Julian date YYYYDDD, time HHMMSS
C
C
C  RETURN VALUE:  date & time, as "HH:MM:SS  MMM DD, YYYY"
C
C
C  SUBROUTINES AND FUNCTIONS CALLED:  none
C
C
C  REVISION  HISTORY:  
C       prototype 10/90 by CJC
C
C       Version    2/93 by CJC for CRAY, etc.
C
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C***********************************************************************

      IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: JDATE   !  Julian date, coded YYYYDDD
        INTEGER, INTENT(IN   ) :: JTIME   !  time, coded HHMMSS


C...........  EXTERNAL FUNCTIONS:

        CHARACTER*10, EXTERNAL :: HHMMSS
        CHARACTER*14, EXTERNAL :: MMDDYY


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER       J, T

        CHARACTER*10    TIMBUF
        CHARACTER*24    DATBUF


C***********************************************************************
C   begin body of function  DT2STR

        J = JDATE
        T = JTIME
        CALL NEXTIME( J, T, 0 )
        TIMBUF = HHMMSS( T )
        DATBUF = MMDDYY( J )
        DT2STR = TIMBUF // DATBUF
        RETURN

        END FUNCTION  DT2STR

