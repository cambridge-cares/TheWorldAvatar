
        LOGICAL FUNCTION WRITE3( FNAME, VNAME, JDATE, JTIME, BUFFER )

C***********************************************************************
C Version "$Id: write3.F 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2013 Baron Advanced Meteorological Systems,
C (C) 2007-2013 Carlie J. Coats, Jr., and
C (C) 2014-2015 UNC Institute for the Environment.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  159
C
C  FUNCTION:  writes all the data from BUFFER() for timestep JDATE:JTIME
C             (formatted YYYYDDD and HHMMSS) to the Models-3 data file
C             with logical name FNAME.
C             If FNAME is time-independent, JDATE and JTIME are ignored.
C             If FNAME is a dictionary file, returns WRDICT3( FNAME, JDATE )
C
C  RETURN VALUE:
C       TRUE iff the operation succeeds
C
C  PRECONDITIONS REQUIRED:
C       FNAME is a Models-3 data file already opened
C       for write access by OPEN3()
C
C  POSTCONDITIONS REQUIRED:
C       subsequent call to SHUT3() to flush the file to disk.
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       INDEX1, INIT3, JSTEP3, WRDICT3, WRCUSTOM, WRGRDDED,
C       WRBNDARY, WRIDDATA, WRPROFIL, WRGRNEST
C
C  REVISION  HISTORY:
C       prototype 3/1992 by CJC
C
C       modified  7/1994 by CJC to handle restart files (tstep < 0)
C
C       Modified 10/1994 by CJC to handle new files with write granularity
C       at the level of individual variables.
C
C       Modified  2/1995 by CJC to handle files of type SMATRX3
C
C       Modified 10/1995 by CJC to handle files of type TSRIES3
C
C       Modified  8/1996 by CJC to determine whether to log successful
C       WRITEs according to the value of environment variable
C       'IOAPI_LOG_WRITE'
C
C       Modified 2/1997  by CJC for OpenMP:  make it thread-safe to
C       call WRITE3 on distinct files in task-parallel.
C
C       Modified  5/1999 by ALT for coupling-mode operation
C
C       Bug-fix 1/2002 by CJC on time step error
C
C       Modified  1/2002 by CJC:  check TRIMLEN() of FNAME
C
C       Modified  3/2002 by CJC:  STATE3V changes
C
C       Modified  8/2002 by CJC:  fixed JSTEP3 RESTART-file bug
C
C       Modified 7/2003 by CJC:  bugfix -- clean up critical sections
C       associated with INIT3()
C
C       Modified 10/2003 by CJC for I/O API version 3:  support for
C       native-binary BINFIL3 file type; uses new INTEGER NAME2FID,
C       LOGICAL SYNCFID
C
C       Modified 9/2004 by CJC for virtual INTERP bug-fix: change to
C       READ3V / WRITE3V interfaces
C
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C       Modified 05/2011 by CJC:  better error-messages
C
C       Modified 08/2015 by CJC for I/O API 3.2:  USE MODNCFIO.
C       Support for MPI/PnetCDF files via WRMPIGRD()
C***********************************************************************

        USE MODNCFIO

        IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'STATE3.EXT'
#ifdef IOAPICPL
        INCLUDE 'STATE3V.EXT'
#endif


C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*), INTENT(IN   ) :: FNAME      !  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME      !  logical file name
        INTEGER      , INTENT(IN   ) :: JDATE      !  date, formatted YYYYDDD
        INTEGER      , INTENT(IN   ) :: JTIME      !  time, formatted HHMMSS
        REAL         , INTENT(IN   ) :: BUFFER(*)  !  output buffer array


C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER, EXTERNAL :: INDEX1, JSTEP3
        LOGICAL, EXTERNAL :: ENVYN
        INTEGER, EXTERNAL :: NAME2FID   !  fname~~> fid lookup
        LOGICAL, EXTERNAL :: SYNCFID    !  disk-synch
        LOGICAL, EXTERNAL :: WRDICT3    !  read time steps for  DCTNRY3 files
        LOGICAL, EXTERNAL :: WRBUF3     !  read time steps for BUFFERED files
        LOGICAL, EXTERNAL :: WRCUSTOM   !  read time steps for CUSTOM3 files
        LOGICAL, EXTERNAL :: WRGRDDED   !  read time steps for GRDDED3 files
        LOGICAL, EXTERNAL :: WRMPIGRD   !  read time steps for MPIGRD3 files
        LOGICAL, EXTERNAL :: WRBNDARY   !  read time steps for BNDARY3 files
        LOGICAL, EXTERNAL :: WRIDDATA   !  read time steps for IDDATA3 files
        LOGICAL, EXTERNAL :: WRPROFIL   !  read time steps for PROFIL3 files
        LOGICAL, EXTERNAL :: WRGRNEST   !  read time steps for GRNEST3 files
        LOGICAL, EXTERNAL :: WRSMATRX   !  read time steps for SMATRX3 files
        EXTERNAL          :: INITBLK3        !!  BLOCK DATA to initialize STATE3 commons


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER       ITYP            !  holds file type
        INTEGER       FID             !  file-subscript for STATE3 arrays
        INTEGER       VID             !  vble-subscript for STATE3 arrays
        INTEGER       FLEN, VLEN      !  name lengths for file, vble
        INTEGER       STEP            !  time step record number
        INTEGER       STEP2           !  1 or 2, according to step mod 2
        INTEGER       TFLAG( 2 )      !  ( JDATE=yyyyddd, JTIME=hhmmss )
        INTEGER       IERR            !  netCDF error status return
        LOGICAL       EFLAG, WFLAG
        CHARACTER*256 MESG            !  for m3msg2

C...........   STATIC Variables:

        CHARACTER*7, PARAMETER :: TYPNAMES( -3 : 7 ) =
     &     (/ 'UNKNOWN',        !  -3:  file type error
     &        'DGRAPH3',        !  -2:  known file types
     &        'CUSTOM3',        !  -1:    "
     &        'DCTNRY3',        !   0:    "
     &        'GRDDED3',        !   1:    "
     &        'BNDARY3',        !   2:    "
     &        'IDDATA3',        !   3:    "
     &        'PROFIL3',        !   4:    "
     &        'GRNEST3',        !   5:    "
     &        'SMATRX3',        !   6:    "
     &        'UNKNOWN' /)      !   7:    "

        LOGICAL, SAVE :: FIRSTIME = .TRUE.
        LOGICAL, SAVE :: LOGFLAG  = .TRUE.

#ifdef IOAPICPL
        INTEGER       COUNT, TYPE
        LOGICAL, EXTERNAL :: WRITE3V
#endif

C***********************************************************************
C   begin body of function  WRITE3
C......,....   Check that Models-3 I/O has been initialized; get STATE3
C......,....   subscript for FNAME; check length of vname argument

        FLEN  = LEN_TRIM( FNAME )
        VLEN  = LEN_TRIM( VNAME )
        FID   = NAME2FID( FNAME )

        EFLAG = ( FID .LE. 0 )

        IF ( EFLAG ) THEN
            MESG = 'Invalid file.  FNAME="' // FNAME
            CALL M3WARN( 'WRITE3', JDATE, JTIME, MESG )
            WRITE3 = .FALSE.
            RETURN
        END IF

!$OMP   CRITICAL( WRITE3_INIT )
        IF ( FIRSTIME ) THEN
            LOGFLAG = ENVYN( 'IOAPI_LOG_WRITE',
     &                       'Log write operations or not',
     &                       .TRUE.,
     &                       IERR )
            FIRSTIME = .FALSE.
            IF ( IERR .GT. 0 ) THEN
                EFLAG = .TRUE.
                CALL M3MSG2( 'Bad environment vble "IOAPI_LOG_WRITE"' )
            END IF
        END IF          !  firstime
!$OMP   END CRITICAL( WRITE3_INIT )

        IF ( VLEN .GT. NAMLEN3 ) THEN
            EFLAG = .TRUE.
            MESG  = 'File "'// FNAME// '" Variable "'// VNAME//'"'
            CALL M3WARN( 'WRITE3', JDATE, JTIME, MESG )
            WRITE( MESG, '( A, I10 )'  )
     &          'Max vble name length 16; actual:', VLEN
            CALL M3MSG2( MESG )
        END IF          !  if len( vname ) > 16

        IF ( RONLY3( FID ) ) THEN
            EFLAG = .TRUE.
            MESG = 'File:  ' // FNAME // ' IS READ-ONLY.'
            CALL M3MSG2( MESG )
        END IF          !  if len( vname ) > 16


        IF ( EFLAG ) THEN
            MESG = 'Invalid arguments: file="' // FNAME //
     &                       '" or variable="' // VNAME // '"'
            CALL M3WARN( 'WRITE3', JDATE, JTIME, MESG )
            WRITE3 = .FALSE.
            RETURN
        END IF

        IF ( VOLAT3( FID ) ) THEN     !  volatile file:  synch with disk

           IF ( .NOT. SYNCFID( FID ) ) THEN

                MESG = 'Error with disk synch for file:  '// FNAME
                CALL M3WARN( 'WRITE3', JDATE, JTIME, MESG )
                WRITE3 = .FALSE.
                RETURN

            END IF      !  if NCSNC() error

        END IF          !  if file not opened, or if readonly, or if volatile


C...........   Get ID for variable(s) to be written.  Note subcases
C...........   that 'ALL' must be the VNAME for non-(gridded | boundary
C...........   | custom) files.

        ITYP = FTYPE3( FID )

        IF ( VNAME .EQ. ALLVAR3 ) THEN

            VID = ALLAYS3

        ELSE IF ( ITYP .EQ. CUSTOM3  .OR.
     &            ITYP .EQ. GRDDED3  .OR.
     &            ITYP .EQ. MPIGRD3  .OR.
     &            ITYP .EQ. BNDARY3  .OR.
     &            ITYP .EQ. SMATRX3  .OR.
     &            ITYP .EQ. TSRIES3  .OR.
     &            ITYP .EQ. PTRFLY3 ) THEN

            VID = INDEX1( VNAME, NVARS3( FID ) , VLIST3( 1,FID ) )

            IF ( VID .EQ. 0 ) THEN

                MESG = 'Variable ' // VNAME //' not in file:  '//FNAME
                CALL M3WARN( 'WRITE3', JDATE, JTIME, MESG )
                WRITE3 = .FALSE.
                RETURN

            END IF

        ELSE IF ( ITYP .EQ. DCTNRY3 ) THEN

            WRITE3 = WRDICT3( FID, VNAME )
            RETURN

        ELSE

            IF ( ITYP .LT. -3  .OR.  ITYP .GT. SMATRX3 ) ITYP = -3
            MESG =
     &          'File '//FNAME//' of type ' //TYPNAMES( ITYP ) //
     &          ' WRITE BY VARIABLE not supported.'
                WRITE3 = .FALSE.
                CALL M3WARN( 'WRITE3', JDATE, JTIME, MESG )
                RETURN

        END IF          !  if VNAME is 'ALL', or if var-granularity, or not.

C...........   Compute record number for this time step.  Update
C...........   max step number if necessary.  Then read data
C...........   from file into BUFFER(), selecting by file type:

        STEP = JSTEP3( JDATE, JTIME,
     &                 SDATE3( FID ),
     &                 STIME3( FID ),
     &                 ABS( TSTEP3( FID ) ) )

        IF ( STEP .LT. 0 ) THEN

            WRITE( MESG, 93020 )
     &      'Requested date & time:', JDATE, JTIME
            CALL M3MSG2( MESG )
            WRITE( MESG, 93020 )
     &      'File starting date & time:', SDATE3( FID ), STIME3( FID ),
     &      'time step:', TSTEP3( FID )
            CALL M3MSG2( MESG )
            WRITE3 = .FALSE.
            MESG = 'Time step error writing file:  ' // FNAME
            CALL M3WARN( 'WRITE3', JDATE, JTIME, MESG )
            RETURN

        END IF

        IF ( TSTEP3( FID ) .LT. 0 ) THEN
            STEP2 = 1 + MOD( STEP-1, 2 )
            TFLAG( 1 ) = JDATE
            TFLAG( 2 ) = JTIME
        ELSE IF ( TSTEP3( FID ) .EQ. 0 ) THEN
            STEP2 = STEP
            TFLAG( 1 ) = 0
            TFLAG( 2 ) = 0
        ELSE
            STEP2 = STEP
            TFLAG( 1 ) = JDATE
            TFLAG( 2 ) = JTIME
        END IF

        IF ( CDFID3( FID ) .EQ. BUFFIL3 ) THEN     !  BUFFERED "file"

            WFLAG = WRBUF3( FID, VID, JDATE, JTIME, STEP2, BUFFER )

#ifdef IOAPICPL
        ELSE IF ( CDFID3( FID ) .EQ. VIRFIL3 ) THEN

           COUNT = BSIZE3(FID)*NLAYS3(FID)
           IF ( VNAME .EQ. ALLVAR3 ) THEN
              WFLAG = .TRUE.
              DO VID = 1, NVARS3(FID)
                 WFLAG = WFLAG .AND.
     $                WRITE3V( FID, VID,
     $                         TFLAG(1), TFLAG(2),
     $                         BUFFER(1+(VID-1)*COUNT) )
              ENDDO
           ELSE
              WFLAG = WRITE3V( FID, VID,
     $                         TFLAG(1), TFLAG(2),
     $                         BUFFER )
           END IF
#endif

        ELSE IF ( ITYP .EQ. CUSTOM3 ) THEN

            WFLAG = WRCUSTOM( FID, VID, TFLAG, STEP2, BUFFER )

        ELSE IF ( ITYP .EQ. GRDDED3 ) THEN

            WFLAG = WRGRDDED( FID, VID, TFLAG, STEP2, BUFFER )

        ELSE IF ( ITYP .EQ. MPIGRD3 ) THEN

            WFLAG = WRMPIGRD( FID, VID, TFLAG, STEP2, BUFFER )

        ELSE IF ( ITYP .EQ. BNDARY3 ) THEN

            WFLAG = WRBNDARY( FID, VID, TFLAG, STEP2, BUFFER )

        ELSE IF ( ITYP .EQ. IDDATA3 ) THEN

            WFLAG = WRIDDATA( FID, TFLAG, STEP2, BUFFER )

        ELSE IF ( ITYP .EQ. PROFIL3 ) THEN

            WFLAG = WRPROFIL( FID, TFLAG, STEP2, BUFFER )

        ELSE IF ( ITYP .EQ. GRNEST3 ) THEN

            WFLAG = WRGRNEST( FID, TFLAG, STEP2, BUFFER )

        ELSE IF ( ITYP .EQ. SMATRX3 ) THEN

            WFLAG = WRSMATRX( FID, TFLAG, STEP2, BUFFER )

        ELSE IF ( ITYP .EQ. TSRIES3 ) THEN

            WFLAG = WRGRDDED( FID, VID, TFLAG, STEP2, BUFFER )

        ELSE IF ( ITYP .EQ. PTRFLY3 ) THEN

            WFLAG = WRGRDDED( FID, VID, TFLAG, STEP2, BUFFER )

        ELSE    !  illegal file type

            WRITE( MESG, '( 3 A, I5 )' )
     &      'File:  ', FNAME, ' of unknown type:', FTYPE3( FID )
            CALL M3WARN( 'WRITE3', JDATE, JTIME, MESG )
            WRITE3 = .FALSE.

            RETURN

        END IF          !  if time step bad, or if file of type
                        !  custom, gridded, ..., or unknown.
        WRITE3 = WFLAG

        IF ( .NOT. WFLAG ) THEN         !  failure

            IF( TSTEP3( FID ) .EQ. 0 ) THEN
                MESG =
     &          'Error writing "' // VNAME( 1:VLEN ) //
     &         '" to "'           // FNAME( 1:FLEN ) // '"'
            ELSE
                WRITE( MESG, "( 5 A, I9.7, A, I6.6 )" )
     &          'Error writing "', VNAME( 1:VLEN ),
     &          '" to "',          FNAME( 1:FLEN ),
     &          '" for', JDATE, ':', JTIME
            END IF
            CALL M3WARN( 'WRITE3', JDATE, JTIME, MESG )

            RETURN

        END IF              !  if failure

        MXREC3( FID ) = MAX( MXREC3( FID ), ABS( STEP ) )

        IF ( LOGFLAG ) THEN  !  message to log on successful writes:
            IF ( VID .GT. 0 ) THEN  !  single variable written:
                IF( TSTEP3( FID ) .EQ. 0 ) THEN
                    MESG = VNAME( 1:VLEN ) //
     &              ' written to ' // FNAME( 1:FLEN )
                ELSE
                    WRITE( MESG, '( 4 A, I9.7, A, I6.6 )' )
     &                  VNAME( 1:VLEN ), ' written to ',
     &                  FNAME( 1:FLEN ), ' for ', JDATE, ':', JTIME
                END IF
            ELSE            !  vid <= 0:  entire timestep written
                IF( TSTEP3( FID ) .EQ. 0 ) THEN
                    MESG = 'Record written to ' // FNAME( 1:FLEN )
                ELSE
                    WRITE( MESG, '( 3 A, I9.7, A, I6.6 )' )
     &                  'Timestep written to ', FNAME( 1:FLEN ),
     &                  ' for date and time ', JDATE, ':', JTIME
                END IF
            END IF          !  if single-variable-write, or timestep-write.
            CALL M3MSG2( MESG )
        END IF          !  if logflag

        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Error and warning message formats..... 91xxx
C...........   Log message formats..... 92xxx
C...........   Internal buffering formats..... 93xxx

93010   FORMAT ( 5 ( A, :, I9, :, 2X ) )

93020   FORMAT ( A, :, I9, ':', I6.6, :, 2X, A, :, I10.6 )

        END FUNCTION WRITE3

