
        LOGICAL   FUNCTION CURRSTEP ( JDATE, JTIME, 
     &                                SDATE, STIME, TSTEP, 
     &                                CDATE, CTIME )

C***********************************************************************
C Version "$Id: currstep.f 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2003-2010 Baron Advanced Meteorological Systems,
C (C) 2007-2013 Carlie J. Coats, Jr., and 
C (C) 2014 UNC Institute for the Environment.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  subroutine body starts at line  61
C
C  FUNCTION:  Compute the date&time CDATE:CTIME for the time step in 
C             the time step sequence starting at SDATE:STIME and having
C             time step TSTEP.  In particular, it is the largest time
C             step in the sequence having the property:
C
C                 CDATE:CTIME <= JDATE:JTIME
C
C  PRECONDITIONS REQUIRED:  Dates represented YYYYDDD, 
C                           times represented HHMMSS.
C
C  SUBROUTINES AND FUNCTIONS CALLED:  NEXTIME, SEC2TIME, SECSDIFF, TIME2SEC
C
C  REVISION  HISTORY:
C       prototype 5/92 by CJC
C
C       Version 1/2007 by CJC:  simplification; handle negative 
C       *DATE arguments correctly
C       prototype 5/92 by CJC
C
C       Gross simplification 1/2008 by CJC:  use result from CURREC(),
C       new version of which is now relatively safe from integer overflow.
C***********************************************************************

        IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: SDATE, STIME    !  starting d&t for the sequence
        INTEGER, INTENT(IN   ) :: TSTEP           !  time step for the sequence
        INTEGER, INTENT(IN   ) :: JDATE, JTIME    !  d&t requested
        INTEGER, INTENT(  OUT) :: CDATE, CTIME    !  d&t for timestep of JDATE:JTIME

C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER, EXTERNAL :: CURREC

C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER     IREC

C***********************************************************************
C   begin body of subroutine  CURRSTEP

        IREC = CURREC( JDATE, JTIME, 
     &                 SDATE, STIME, TSTEP, 
     &                 CDATE, CTIME )

        CURRSTEP = ( IREC .GT. 0 )
        RETURN

        END FUNCTION CURRSTEP

