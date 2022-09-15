
        SUBROUTINE M3EXIT( CALLER, JDATE, JTIME, MSGTXT, EXITSTAT )

C***********************************************************************
C Version "$Id: m3exit.F 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  subroutine body starts at line  76
C
C  FUNCTION:  Generate simple error messages for Models-3 core;
C             terminate program execution via CALL EXIT( EXITSTAT )
C
C  PRECONDITIONS REQUIRED:  JDATE:JTIME represented as YYYYDDD:HHMMSS
C
C  SUBROUTINES AND FUNCTIONS CALLED:  DT2STR, INIT3, SHUT3
C
C  REVISION  HISTORY:	
C	prototype 5/1992 by CJC
C       Revised   8/1996 to close currently-open POSIX-OK Fortran units.
C       Modified  1/1997 by CJC to trim trailing blanks from MSGTXT
C       Modified  2/1997 by CJC:  conditional definition of EXIT under AIX 
C       Modified 10/1998 by CJC:  Factor all output through m3msg2(), to
C       get around SGI OpenMP bug.
C       Caution:  messages may not be sequenced correctly when called by
C       multiple threads simultaneously
C       Modified  5/1999 by ALT for coupling-mode operation
C       Modified  4/2004 by CJC:  factor through M3PARAG() -- fixes
C       multi-thread sequencing problem.
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C***********************************************************************

#ifdef   _AIX
#define  EXIT exit_
#endif

      IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'IODECL3.EXT'


C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*), INTENT(IN   ) :: CALLER          !  name of the caller
        INTEGER      , INTENT(IN   ) :: JDATE, JTIME    !  model date&time for the error
        CHARACTER*(*), INTENT(IN   ) :: MSGTXT          !  error message
        INTEGER      , INTENT(IN   ) :: EXITSTAT        !  exit status for program

                 
C...........   EXTERNAL FUNCTIONS and their descriptions:

        CHARACTER*24, EXTERNAL :: DT2STR


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        CHARACTER*256   MESG( 6 )
        INTEGER         LINE, IDEV, ISTAT
        LOGICAL         OFLAG


C***********************************************************************
C   begin body of subroutine  M3EXIT

#ifdef IOAPICPL
        IF ( EXITSTAT .NE. 0 ) THEN
            CALL SETSYNCHRO3V(1)
        END IF
#endif

        LINE = 1
        MESG( LINE ) = ' '

        IF ( .NOT. SHUT3() ) THEN
            LINE = LINE + 1
            MESG( LINE ) ='Could not shut down I/O API files correctly'
        END IF

        LINE = LINE + 1
        IF ( EXITSTAT .NE. 0 ) THEN
            MESG( LINE ) = '*** ERROR ABORT in subroutine ' // CALLER
        ELSE    !  errstat = 0:  successful completion
            MESG( LINE ) = '--->> Normal Completion of program '//CALLER
        END IF

        LINE = LINE + 1
        MESG( LINE ) = MSGTXT

        IF ( JDATE .GT. 0  .OR.  JTIME .GT. 0 ) THEN
        LINE = LINE + 1
            WRITE( MESG( LINE ), '( A, 2X, A, 2X, A, I7, A, I6.6, A )' ) 
     &      'Date and time', DT2STR( JDATE, JTIME ),  
     &      '(', JDATE, ':', JTIME, ')'
        END IF

        LINE = LINE + 1
        MESG( LINE ) = ' '
        CALL M3PARAG( LINE, MESG )

        DO  IDEV = 10, 99
            INQUIRE( UNIT = IDEV, OPENED = OFLAG, IOSTAT = ISTAT )
            !!  paranoia to deal with certain rare situations on SGI
            IF ( ISTAT .EQ. 0 ) THEN
                IF ( OFLAG ) CLOSE( UNIT = IDEV, IOSTAT = ISTAT )
                IF ( ISTAT .NE. 0 ) THEN
                    WRITE( MESG, '( A, 2X, I4, A, I10 )' )
     &                  'Error closing unit', IDEV,
     &                  ' status =', ISTAT
                    CALL M3MSG2( MESG )
                END IF
            END IF
        END DO

        CALL EXIT( EXITSTAT )

        END SUBROUTINE M3EXIT

