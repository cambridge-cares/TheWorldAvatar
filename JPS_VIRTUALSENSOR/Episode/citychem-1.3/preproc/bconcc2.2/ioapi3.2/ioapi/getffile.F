
      INTEGER FUNCTION GETFFILE( LNAME, RDONLY, FMTFLAG, RECLEN,
     &                           CALLER )

C***********************************************************************
C Version "$Id: getffile.F 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems, and
C (C) 2016 UNC Institute for the Environment.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  77
C
C  DESCRIPTION:
C    Gets value of logical name LNAME from the environment, checks for
C    existence of a file whose file name is that value, then opens the
C    file as a sequential file on unit IUNIT according to the flags RDONLY
C    (open for read-only iff TRUE, read/write if FALSE) and FMTFLAG
C    (formatted iff TRUE, else unformatted), and with the
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
C       TRIMLEN
C
C  REVISION  HISTORY:
C       Adapted  10/2000 by CJC from GETFFILE.F
C
C       Modified 7/2003 by CJC:  thread safety -- critical section
C       associated with INIT3()
C
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C
C       Modified 02/2016 by CJC: eliminate non-F90 cases.
C***********************************************************************

       IMPLICIT NONE


C...........   ARGUMENTS and their descriptions:


       CHARACTER*(*), INTENT(IN   ) :: LNAME          !  logical file name
       LOGICAL      , INTENT(IN   ) :: RDONLY         !  TRUE iff file is input-only
       LOGICAL      , INTENT(IN   ) :: FMTFLAG        !  TRUE iff file should be formatted
       INTEGER      , INTENT(IN   ) :: RECLEN         !  record length
       CHARACTER*(*), INTENT(IN   ) :: CALLER         !  caller-name for logging

C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER, EXTERNAL :: INIT3
        INTEGER, EXTERNAL :: JUNIT


C...........   LOCAL VARIABLES and their descriptions:

        INTEGER         IUNIT
        INTEGER         LLEN, PLEN
        INTEGER		    ISTAT, JSTAT

        CHARACTER*256   PNAME
        CHARACTER*16    FMTSTRING
        LOGICAL         LCHECK, EFLAG

        INTEGER, SAVE :: LOGDEV = -1

C***********************************************************************
C   begin body of GETFFILE()

        EFLAG = .FALSE.
!$OMP   CRITICAL( S_INIT )
        IF ( LOGDEV .LT. 0 ) THEN
            LOGDEV = INIT3()
        END IF
!$OMP   END CRITICAL( S_INIT )

C...........   Read filename from environment ---

      CALL NAMEVAL(  LNAME, PNAME )

      LLEN = LEN_TRIM( LNAME )
      PLEN = LEN_TRIM( PNAME )

      IF ( FMTFLAG ) THEN
          FMTSTRING = 'FORMATTED'
      ELSE
          FMTSTRING = 'UNFORMATTED'
      END IF

C.......   Check for existence of files ---

      INQUIRE( FILE=PNAME, EXIST=LCHECK )

      IF ( RDONLY ) THEN

          IF ( .NOT. LCHECK ) THEN
              WRITE( LOGDEV, 9000, IOSTAT=JSTAT, ERR=9999 )
     &            'ERROR: input file not found: ',
     &            LNAME( 1:LLEN ),
     &            PNAME( 1:PLEN )
              GETFFILE = -1
              RETURN
          END IF

          IUNIT = JUNIT()

          OPEN( UNIT   = IUNIT,
     &          FILE   = PNAME,
     &          FORM   = FMTSTRING,
     &          ACTION = 'READ',
     &          RECL   = RECLEN,
     &          IOSTAT = ISTAT )
          IF ( ISTAT .NE. 0 ) THEN
              CALL PERROR( 'ERROR: file not opened successfully')
              WRITE( LOGDEV, 9100, IOSTAT=JSTAT, ERR=9999 )
     &        LNAME( 1:LLEN ),
     &        PNAME( 1:PLEN ),
     &        'I/O status:  ', ISTAT
              GETFFILE = -1
              RETURN
          END IF

          WRITE( LOGDEV, 9001, IOSTAT=JSTAT, ERR=9999 )
     &        'File ', LNAME( 1:LLEN ),
     &        ' opened for input on unit:', IUNIT,
     &        PNAME(  1:PLEN  )

      ELSE      !  not read-only:  open for write

          IF ( LCHECK ) THEN
              WRITE( LOGDEV, 9000, IOSTAT=JSTAT, ERR=9999 )
     &            'WARNING: output file already exists: ',
     &            LNAME( 1:LLEN ),
     &            PNAME( 1:PLEN )
          END IF

          IUNIT = JUNIT()

#ifdef __alpha
          OPEN( UNIT   = IUNIT,
     &          FILE   = PNAME,
     &          FORM   = FMTSTRING,
     &          STATUS = 'UNKNOWN',
     &          RECL   = RECLEN,
     &          IOSTAT = ISTAT )
#endif    /*  ifdef __alpha */
#ifdef _WIN32
          OPEN( UNIT   = IUNIT,
     &          FILE   = PNAME,
     &          FORM   = FMTSTRING,
     &          CONVERT= 'BIG_ENDIAN',
     &          RECL   = RECLEN,
     &          IOSTAT = ISTAT )
#endif    /*  ifdef _WIN32 */
#ifndef __alpha
#ifndef _WIN32
          OPEN( UNIT   = IUNIT,
     &          FILE   = PNAME,
     &          FORM   = FMTSTRING,
     &          RECL   = RECLEN,
     &          IOSTAT = ISTAT )
#endif    /*  ifndef _WIN32  */
#endif    /*  ifndef __alpha */

          IF ( ISTAT .NE. 0 ) THEN
              CALL PERROR( 'ERROR: file not opened successfully')
              WRITE( LOGDEV, 9100, IOSTAT=JSTAT, ERR=9999 )
     &        LNAME( 1:LLEN ),
     &        PNAME( 1:PLEN ),
     &        'I/O status:  ', ISTAT
              GETFFILE = -1
              RETURN
          END IF

          WRITE( LOGDEV, 9001, IOSTAT=JSTAT, ERR=9999 )
     &        'File "', LNAME( 1:LLEN ),
     &        '" opened for output on unit:', IUNIT,
     &        PNAME(  1:PLEN  )

      END IF

      GETFFILE = IUNIT
      RETURN

C----------------------------------------------------------------------
C     handle messaging errors:

9999  CONTINUE

      IF ( JSTAT .NE. 0 ) THEN
           CALL PERROR( 'ERROR writing log message.')
           WRITE( LOGDEV, 9200, IOSTAT=ISTAT )
     &         'I/O status:', JSTAT
           GETFFILE = -1
           RETURN
      END IF

C-----------------------------------------------------------------------
C    Format statements:

8000   FORMAT( A )
9000   FORMAT( /, 1X, 2A, /, :, 1X, A, / )
9001   FORMAT( /, 5X, 3A, I4,
     &         /, 5X,  A, /)
9100   FORMAT( /, 1X,  A, ':', A,
     &         /, 1X,  A, I7 , / )
9200   FORMAT( /, 1X,  A, I7 , / )

       END FUNCTION GETFFILE
