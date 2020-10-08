
      INTEGER FUNCTION  JUNIT()

C***********************************************************************
C Version "$Id: junit.F 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line 65
C
C  FUNCTION:
C
C    Routine returns next available FORTRAN unit number
C
C  ARGUMENT LIST DESCRIPTION:  empty argument list
C
C  RETURN VALUE:  JUNIT        Unit number selected
C
C  LOCAL VARIABLE DESCRIPTION:
C
C    IUNIT    state variable:  counts through available units
C    BOT      parameter:  first POSIX-approved unit number for FORTRAN I/O
C    TOP      parameter:  last   ...
C
C  REVISION HISTORY:
C
C    3/88  Maximum number of I/O unit numbers was increased from 50
C          to 75 due to increased file I/O requirements.
C    5/88  Modified for ROMNET
C    7/90  Modified for ROM 2.2 -- uses EXWST for error abort.
C    8/90  Algorithm simplification:  replaced IF-GOTO loop by DO loop.
C    8/90  Algorithm simplification:  counting algorithm instead of table
C          of flags; uses POSIX standards-approved unit numbers 11-99
C    3/92  Models-3 Prototype version (eliminate EXWST)
C    8/96  Modified by CJC -- On counting-algorithm failure, performs
C          INQUIREs to find available unit.
C    2/97  conditional definition of EXIT under AIX
C    9/99  by CJC:  modifications for portability
C       Modified 03/2010 by CJC: F90 changes for I/O API v3.1
C***********************************************************************

#ifdef   _AIX
#define  EXIT exit_
#endif

      IMPLICIT NONE

C...........   PARAMETERS and their descriptions:

      INTEGER, PARAMETER :: BOT = 10          !  1 less than initial unit number
      INTEGER, PARAMETER :: TOP = 99          !  final unit number


C...........   LOCAL VARIABLES and their descriptions:

      INTEGER      J
      LOGICAL      FLAG

C............................................................................
C.......   begin body of JUNIT:

      DO J = TOP, BOT, -1
          INQUIRE( UNIT=J, OPENED=FLAG )
          IF ( .NOT. FLAG ) THEN
              JUNIT = J
              RETURN
          END IF 
      END DO
  
C.........   If you get to here: failure

      WRITE (*,91001) BOT, TOP
      CALL EXIT( 2 )
      RETURN


C*************************  FORMAT  STATEMENTS  **************************

C  Error and warning message formats     91xxx


91001 FORMAT (///, 1X, '*** ERROR ABORT IN ROUTINE JUNIT ***',
     &          /, 5X, 'NO MORE UNIT NUMBERS AVAILABLE FOR I/O',
     &          /, 5X, 'First POSIX-approved unit:', I4 ,
     &          /, 5X, 'Last  POSIX-approved unit:', I4 ,
     &          //)

      END FUNCTION  JUNIT
