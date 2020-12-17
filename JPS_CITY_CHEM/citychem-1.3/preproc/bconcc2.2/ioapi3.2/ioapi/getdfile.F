
      INTEGER FUNCTION GETDFILE( LNAME, RDONLY, FMTFLAG, RECLEN,
     &                           CALLER )

C***********************************************************************
C Version "$Id: getdfile.F 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2013 Baron Advanced Meteorological Systems,
C (C) 2007-2013 Carlie J. Coats, Jr., and
C (C) 2014-2016 UNC Institute for the Environment.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line 77
C
C  DESCRIPTION:
C    Gets value of logical name LNAME from the environment, checks for
C    existence of a file whose file name is that value, then opens the
C    file as a direct access file on unit IUNIT according to the flags
C    RDONLY (open for read-only iff TRUE, read/write if FALSE),
C    FMTFLAG (formatted iff TRUE, else unformatted), and with the
C    indicated record length RECLEN.
C    Logs the file-opening, together with the CALLER version, and
C    returns the unit number (or -1 for failure)
C
C    RETURNS:   unit number, (or -1 for failure)
C
C  PRECONDITIONS REQUIRED:
C
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C
C  REVISION  HISTORY:
C       Prototype  2/95 by CJC.
C
C       Modified  9/99 by CJC:  portability/standardization changes
C
C       Modified 7/2003 by CJC:  OMP thread safety -- critical sections
C       associated with INIT3()
C
C       Modified 03/2010, 09/2014 by CJC: F9x changes for I/O API v3.1
C
C       Modified 02/2016 by CJC: eliminate non-F90 cases.
C***********************************************************************

        IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:


       CHARACTER*(*), INTENT(IN   ) :: LNAME          !  logical file name
       LOGICAL      , INTENT(IN   ) :: RDONLY         !  TRUE iff file is input-only
       LOGICAL      , INTENT(IN   ) :: FMTFLAG        !  TRUE iff file should be formatted
       INTEGER      , INTENT(IN   ) :: RECLEN         !  record length for direct access
       CHARACTER*(*), INTENT(IN   ) :: CALLER         !  caller-name for logging

C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER, EXTERNAL :: INIT3, JUNIT

C...........   LOCAL VARIABLES and their descriptions:

        INTEGER     IUNIT
        INTEGER		ISTAT, JSTAT
        LOGICAL     LCHECK

        CHARACTER*512   PNAME
        CHARACTER*512   MESG
        CHARACTER*16    FMTSTRING

        INTEGER, SAVE :: LOGDEV = -1

C***********************************************************************
C   begin body of GETDFILE()

!$OMP   CRITICAL( S_INIT )
        IF ( LOGDEV .LT. 0 ) THEN
            LOGDEV = INIT3()
        END IF
!$OMP   END CRITICAL( S_INIT )

C...........   Read filename from environment ---

      CALL NAMEVAL(  LNAME, PNAME )

      IF ( FMTFLAG ) THEN
          FMTSTRING = 'FORMATTED'
      ELSE
          FMTSTRING = 'UNFORMATTED'
      END IF

C.......   Check for existence of files ---

      INQUIRE( FILE=PNAME, EXIST=LCHECK , IOSTAT=JSTAT )

      IF ( JSTAT .NE. 0 ) THEN
          WRITE( MESG, '( A, I11, 2X, A, 1X, A)' )
     &       'GETDFILE:  Error', JSTAT, 'inquiring about', LNAME
          CALL M3MESG( MESG )
          MESG = 'Path-name: ' // PNAME
          CALL M3MESG( MESG )
          GETDFILE = -1
          RETURN
      END IF

      IF ( RDONLY ) THEN

          IF ( .NOT. LCHECK ) THEN
              WRITE( LOGDEV, 9000, IOSTAT=JSTAT, ERR=9999 )
     &            'ERROR: input file not found: ',
     &            TRIM( LNAME ),
     &            TRIM( PNAME )
              GETDFILE = -1
              RETURN
          END IF

          IUNIT = JUNIT()

          OPEN( UNIT   = IUNIT,
     &          FILE   = PNAME,
     &          FORM   = FMTSTRING,
     &          ACCESS = 'DIRECT',
     &          RECL   = RECLEN,
     &          STATUS = 'OLD',
     &          ACTION = 'READ',
     &          IOSTAT = ISTAT )
          IF ( ISTAT .NE. 0 ) THEN
              CALL PERROR( 'ERROR: file not opened successfully')
              WRITE( LOGDEV, 9100, IOSTAT=JSTAT, ERR=9999 )
     &        TRIM( LNAME ),
     &        TRIM( PNAME ),
     &        'I/O status:  ', ISTAT
              GETDFILE = -1
              RETURN
          END IF

          WRITE( LOGDEV, 9001, IOSTAT=JSTAT, ERR=9999 )
     &        'File ', TRIM( LNAME ),
     &        ' opened for input on unit', IUNIT,
     &        ' with record length', RECLEN,
     &        TRIM( PNAME )

      ELSE      !  not read-only:  open for write

          IF ( LCHECK ) THEN
              WRITE( LOGDEV, 9000, IOSTAT=JSTAT, ERR=9999 )
     &            'WARNING: output file already exists: ',
     &            TRIM( LNAME ),
     &            TRIM( PNAME )
          END IF

          IUNIT = JUNIT()

#ifdef __alpha
          OPEN( UNIT   = IUNIT,
     &          FILE   = PNAME,
     &          FORM   = FMTSTRING,
     &          ACCESS = 'DIRECT',
     &          RECL   = RECLEN,
     &          STATUS = 'UNKNOWN',
     &          IOSTAT = ISTAT )
#endif    /*  ifdef __alpha */
#ifndef __alpha
          OPEN( UNIT   = IUNIT,
     &          FILE   = PNAME,
     &          FORM   = FMTSTRING,
     &          ACCESS = 'DIRECT',
     &          RECL   = RECLEN,
     &          IOSTAT = ISTAT )
#endif    /*  ifndef __alpha */

          IF ( ISTAT .NE. 0 ) THEN
              CALL PERROR( 'ERROR: file not opened successfully')
              WRITE( LOGDEV, 9100, IOSTAT=JSTAT, ERR=9999 )
     &        TRIM( LNAME ),
     &        TRIM( PNAME ),
     &        'I/O status:  ', ISTAT
              GETDFILE = -1
              RETURN
          END IF

          WRITE( LOGDEV, 9001, IOSTAT=JSTAT, ERR=9999 )
     &        'File "', TRIM( LNAME ),
     &        '" opened for output on unit:', IUNIT,
     &        ' with record length', RECLEN,
     &        TRIM( PNAME )

      END IF

      GETDFILE = IUNIT
      RETURN

C----------------------------------------------------------------------
C     handle messaging errors:

9999  CONTINUE

      IF ( JSTAT .NE. 0 ) THEN
           CALL PERROR( 'ERROR writing log message.')
           WRITE( LOGDEV, 9200, IOSTAT=ISTAT )
     &         'I/O status:', JSTAT
           GETDFILE = -1
           RETURN
      END IF

C-----------------------------------------------------------------------
C    Format statements:

8000   FORMAT( A )
9000   FORMAT( /, 1X, 2A, /, :, 1X, A, / )
9001   FORMAT( /, 5X, 3A, I4, A, I8,
     &         /, 5X,  A, /)
9100   FORMAT( /, 1X,  A, ':', A,
     &         /, 1X,  A, I7 , / )
9200   FORMAT( /, 1X,  A, I7 , / )

       END FUNCTION GETDFILE
