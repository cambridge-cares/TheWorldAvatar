
        INTEGER FUNCTION JSTEP3( JDATE, JTIME, SDATE, STIME, TSTEP )

C***********************************************************************
C Version "$Id: jstep3.F 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2013 Baron Advanced Meteorological Systems,
C (C) 2007-2013 Carlie J. Coats, Jr., and
C (C) 2014 UNC Institute for the Environment.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  73
C
C  FUNCTION:
C       returns the timestep-record number corresponding to JDATE:JTIME
C       for the timestep sequence starting at SDATE:STIME, with time
C       step increment TSTEP
C
C  RETURN VALUE:
C       time step record number, if successful, or  -1  if the
C       indicated  JDATE:JTIME  is not a positive multiple of
C       TSTEP from SDATE:STIME
C
C  PRECONDITIONS REQUIRED:
C       avoid integer overflow
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       none
C
C  REVISION  HISTORY:
C       prototype 3/1992 by Carlie J. Coats, Jr., MCNC Environmental Programs
C       
C       Unification 2/2002 by CJC with global-climate DAYMON, which
C       uses a 360-day "year"
C
C       Version 1/2007 by CJC:  simplification after the fashion of
C       currec(); handles negative *DATE arguments correctly
C
C       Version 1/2008 by CJC:  Problem reported by Christian Hogrefe, 
C       NY Division of Environmental Conservation:  be careful to avoid
C       integer overflow, for climate modeling applications, etc.
C       Gross simplification:  use result from (new no-overflow version
C       of) CURREC()
C
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C***********************************************************************

        IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT( IN ) :: JDATE   !  requested date YYYYDDD
        INTEGER, INTENT( IN ) :: JTIME   !  requested time HHMMSS
        INTEGER, INTENT( IN ) :: SDATE   !  starting date  YYYYDDD
        INTEGER, INTENT( IN ) :: STIME   !  starting time  HHMMSS
        INTEGER, INTENT( IN ) :: TSTEP   !  time step      H*MMSS

C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER, EXTERNAL :: CURREC

C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         IREC, SECS
        INTEGER         KDATE, KTIME, CDATE, CTIME


C***********************************************************************
C   begin body of function  JSTEP3

        IF ( TSTEP .EQ. 0 ) THEN
            JSTEP3 = 1
            RETURN
        END IF

        IREC = CURREC( JDATE, JTIME, 
     &                 SDATE, STIME, TSTEP, 
     &                 CDATE, CTIME )


        IF ( IREC .LT. 0 ) THEN
            JSTEP3 = -1
            RETURN
        END IF

        !!  Normalize the JDATE:JTIME argument
        !!  (CDATE:CTIME already normalized by CURREC):
        
        KDATE = JDATE
        KTIME = JTIME
        CALL NEXTIME( KDATE, KTIME, 0 )
        
        !!  compare with CURREC() result:
        
        IF ( KDATE .EQ. CDATE .AND. KTIME .EQ. CTIME ) THEN
            JSTEP3 = IREC
        ELSE
            JSTEP3 = -1
        END IF

        RETURN

        END FUNCTION JSTEP3

