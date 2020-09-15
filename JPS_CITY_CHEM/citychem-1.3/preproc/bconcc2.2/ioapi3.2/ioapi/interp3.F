
        LOGICAL FUNCTION INTERP3( FNAME, VNAME, CALLER,
     &                            JDATE, JTIME, RSIZE, BUFFER )
     &                    RESULT( IFLAG3 )

        IMPLICIT NONE

        LOGICAL INTERPX

C***********************************************************************
C Version "$Id: interp3.F 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems,
C (C) 2007-2013 Carlie J. Coats, Jr., and 
C (C) 2014-2015 UNC Institute for the Environment.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  INTERP3 function body starts at line  199
C  Entry INTERPX         starts at line  205
C
C  NOTE:  MACHINE-DEPENDENT CODE !!!
C           depends upon FORTRAN :: C coupling between INTERP3() and BUFINT3()
C
C  FUNCTION:
C       INTERP3() interpolates data from Models-3 data file with logical
C       name FNAME, variable VNAME for all layers to date and time
C       JDATE (coded YYYYDDD) and time JTIME (HHMMSS), putting the
C       result in BUFFER.
C
C       INTERPX() interpolates with windowing for the specified
C       column-row-layer range.  For BOUNDARY files, only the
C       layer-range is relevant; the "window" returned is always
C       a full horizontal slab. For CUSTOM files, only the column
C       and layer-ranges are relevant.
C
C       File type must be CUSTOM, GRIDDED, or BOUNDARY.
C       For time-independent files, JDATE:JTIME are ignored.
C
C       Encapsulates file opens and reads, and all the book-keeping
C       and data structures necessary for that task.
C
C  USES
C       C routine BUFINT3():  sets up all the data structures for the
C       call so that BUFINT3 can allocate and manage read-buffers
C       internally, call RDVARS() and perform the interpolation.
C       (Actually, INTERP3() + BUFINT3() should be considered as
C       two parts of a single routine)
C
C       C routine BUFINTX:  like BUFINT3(), except that the interpolation
C       occurs only for the specified output window.
C
C  RETURN VALUE:
C       TRUE iff the operation succeeds (and the data is available)
C
C  PRECONDITIONS REQUIRED:
C       FNAME is a Models-3 data file containing variable VNAME for
C       a set of time steps enclosing JDATE:JTIME
C       File type must be CUSTOM3, GRDDED3, or BNDARY3.
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       BUFINT3, CURRSTEP, INDEX1, JSTEP3, M3WARN, NAME2FID, NEXTIME,
C       OPEN3, SECSDIFF, TIME2SEC
C
C  REVISION  HISTORY:
C       prototype 6/1993 by Carlie J. Coats, Jr, MCNC-EMC
C       modified  7/1994 by CJC for restart (circular-buffer) files
C       modified  9/1994 by CJC for new version of OPEN3()
C       modified 10/1994 by CJC for new WRITE3 -- granularity at the
C       level of individual variables.  FNAME, VNAME are now CHAR*(*).
C       modified  6/1995 by CJC to check VTYPE3( VID,FID )
C       modified 11/1995 by CJC to check dates for BUFFERED "files"
C       Modified  5/1998 by CJC for OpenMP thread-safety
C       Modified  5/1999 by CJC and ALT for coupling-mode operation
C       Modified  1/2002 by CJC:  check TRIMLEN() of FNAME
C       Modified  2/2002 by CJC:  integration with per-patch/windowing
C       ENTRY INTERPX() adapted from work of Jeff Young, US EPA/ORD.
C       Modified  3/2002 by CJC:  uses RDTFLAG()
C       Modified  5/2002 by CJC:  support for DOUBLE variables;
C       strict-f77 string treatment in messages
C       Bug-fix   2/2003 from David Wong, US EPA:
C       corrections in error message texts
C       Bug-fix   5/2003 by CJC:  new critical sections (deadlock avoidance)
C       Modified 7/2003 by CJC:  bugfix -- clean up critical sections
C       associated with INIT3()
C       Modified 10/2003 by CJC for I/O API version 3:  support for
C       native-binary BINFIL3 file type; uses INTEGER NAME2FID
C       Modified  3/14/2004 by CJC:  FORMAT changes.
C       Modified  8/17/2004 by CJC:  deal with virtual-file bug.
C       Modified 11/2004 by CJC:  new "verbose-flag" argument to RDTFLAG
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C       Modified 08/2015 by CJC for I/O API 3.2:  support for type MPIGRD3
C       for MPI/PnetCDF distributed I/O
C***********************************************************************

#define REAL8 DOUBLE PRECISION

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'STATE3.EXT'


C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !  variable name, or 'ALL'
        CHARACTER*(*), INTENT(IN   ) :: CALLER          !  name of caller
        INTEGER      , INTENT(IN   ) :: COL0            !  lower col   bound for INTERPX
        INTEGER      , INTENT(IN   ) :: COL1            !  upper col   bound for INTERPX
        INTEGER      , INTENT(IN   ) :: ROW0            !  lower row   bound for INTERPX
        INTEGER      , INTENT(IN   ) :: ROW1            !  upper row   bound for INTERPX
        INTEGER      , INTENT(IN   ) :: LAY0            !  lower layer bound for INTERPX
        INTEGER      , INTENT(IN   ) :: LAY1            !  upper layer bound for INTERPX
        INTEGER      , INTENT(IN   ) :: JDATE           !  date, formatted YYYYDDD
        INTEGER      , INTENT(IN   ) :: JTIME           !  time, formatted HHMMSS
        INTEGER      , INTENT(IN   ) :: RSIZE           !  record dim, words, for error-check
        REAL         , INTENT(  OUT) :: BUFFER( * )     !  interpolation-output buffer array

C...........   PARAMETER :

        CHARACTER*16, PARAMETER :: TYPNAMES( -4 : 5 ) =
     &     (/ 'UNKNOWN',        !  -4:  file type error
     &        'KFEVNT3',        !  -3:  file type error
     &        'DGRAPH3',        !  -2:  known file types
     &        'CUSTOM3',        !  -1:    "
     &        'DCTNRY3',        !   0:    "
     &        'GRDDED3',        !   1:    "
     &        'BNDARY3',        !   2:    "
     &        'IDDATA3',        !   3:    "
     &        'PROFIL3',        !   4:    "
     &        'GRNEST3' /)      !   5:    "


C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER, EXTERNAL :: BUFVGT3         !  alloc/read for   circ. bufs. (C)
        INTEGER, EXTERNAL :: BUFINT3         !  interp from      circ. bufs. (C)
        INTEGER, EXTERNAL :: BUFINTX         !  interp/wndw from circ. bufs. (C)
        INTEGER, EXTERNAL :: BUFVGT3D        !  alloc/read for   circ. bufs. (C)
        INTEGER, EXTERNAL :: BUFINT3D        !  interp from      circ. bufs. (C)
        INTEGER, EXTERNAL :: BUFINTXD        !  interp/wndw from circ. bufs. (C)
        LOGICAL, EXTERNAL :: RDTFLAG         !  get date&time availability; rec #

        LOGICAL, EXTERNAL :: CURRSTEP        !  finds start of timestep
        LOGICAL, EXTERNAL :: OPEN3           !  opens M3 files
        INTEGER, EXTERNAL :: INDEX1          !  name-table lookup
        INTEGER, EXTERNAL :: INIT3           !  start up M3 I/O API
        INTEGER, EXTERNAL :: JSTEP3          !  compute time step record numbers
        INTEGER, EXTERNAL :: NAME2FID        !  fname~~> fid lookup
        INTEGER, EXTERNAL :: SECSDIFF        !  computes date&time differences
        INTEGER, EXTERNAL :: TIME2SEC        !  converts time to seconds

#ifdef IOAPICPL
        LOGICAL, EXTERNAL :: INTPQV
#endif


C...........   STATE VARIABLES:  circular buffer structures used for
C...........   the interpolation.  Such variables must be SAVEd in order
C...........   to retain their values from invocation to invocation.

        LOGICAL, SAVE :: FIRSTIME = .TRUE.


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        LOGICAL       XFLAG           !  TRUE iff call to INTERPX

        INTEGER       IDUM            !  scratch variable
        INTEGER       FID             !  file subscript for STATE3 arrays
        INTEGER       VID             !  variable subscript for STATE3 arrays
        INTEGER       FLEN, VLEN      !  name lengths for file, vble
        INTEGER       TDIM            !  subscript for time step
        INTEGER       STEP, STEP2     !  time step record numbers
        INTEGER       DT              !  seconds in TSTEP3
        INTEGER       MDATE, MTIME    !  date:time args for CURRSTEP
        INTEGER       DTJL            !  difference LDATE:LTIME to JDATE:JTIME
        INTEGER       DTNJ            !  difference JDATE:JTIME to NDATE:NTIME
        REAL          P, Q            !  fractions used for interpolation.
        REAL8         PP, QQ          !  fractions used for interpolation.
        INTEGER       DELTA           !  d(INDX) / d(NCVGTcall)
        INTEGER       DIMS( 5 )       !  hyperslab corner   for NCVGT()
        INTEGER       DELS( 5 )       !  hyperslab diagonal for NCVGT()
        INTEGER       DELT( 5 )       !  hyperslab diagonal for NCVGT()
        INTEGER       DATE1, DATE2    !  for checking timestep-valid flags
        INTEGER       TIME1, TIME2    !  "
        LOGICAL       RFLAG           !  BUFINT3() read-data flag
        LOGICAL       EFLAG           !  error flag
        CHARACTER*16  FIL16, VAR16
        CHARACTER*24  NAMBUF          !  buffer for names (CRAY-PORTING HACK)
        CHARACTER*256 MESG            !  buffer for building error messages


C***********************************************************************
C   begin body of function  INTERP3

        XFLAG = .FALSE.
        NAMBUF = TRIM( CALLER ) // ':INTERP3'
        GO TO  111

        !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

        ENTRY INTERPX( FNAME, VNAME, CALLER,
     &                 COL0, COL1, ROW0, ROW1, LAY0, LAY1,
     &                 JDATE, JTIME, BUFFER )

        XFLAG  = .TRUE.
        NAMBUF = TRIM( CALLER ) // ':INTERPX'
        GO TO  111

        !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

111     CONTINUE
!$OMP CRITICAL( S_INIT )
        IF ( .NOT. FINIT3 ) THEN
            IDUM = INIT3()
        END IF
!$OMP END CRITICAL( S_INIT )


C...........   Check length of name arguments; copy into length=16 buffers

        EFLAG = .FALSE.
        FLEN  = LEN_TRIM( FNAME )
        VLEN  = LEN_TRIM( VNAME )
        FIL16 = FNAME
        VAR16 = VNAME

        IF ( FLEN .GT. NAMLEN3 ) THEN
            EFLAG = .TRUE.
            WRITE( MESG, '( A , I10 )' )
     &          'Max file name length 16; actual:', FLEN
            CALL M3MSG2( MESG )
        END IF          !  if len( fname ) > 16

        IF ( VLEN .GT. NAMLEN3 ) THEN
            EFLAG = .TRUE.
            WRITE( MESG, '( A, I10 )'  )
     &          'Max vble name length 16; actual:', VLEN
            CALL M3MSG2( MESG )
        END IF          !  if len( vname ) > 16

        IF ( EFLAG ) THEN
            MESG  = 'File "' //FIL16// '" Variable "' //VAR16//'"'
            CALL M3MSG2( MESG )
            MESG = 'Invalid variable or file name arguments'
            CALL M3WARN( NAMBUF, JDATE, JTIME, MESG )
            IF ( XFLAG ) THEN
                INTERPX = .FALSE.
            ELSE
                IFLAG3 = .FALSE.
            END IF
            RETURN
        END IF          !  if len( fname ) > 16, or if len( vname ) > 16


C.......   Find netCDF index for the file, and open it if necessary

        EFLAG = .FALSE.

!$OMP CRITICAL( S_INTERP )

        FID = NAME2FID( FNAME )

        IF ( FID .EQ. 0 ) THEN  !  file needs opening

            IF ( .NOT.OPEN3( FIL16, FSREAD3, NAMBUF ) ) THEN

                MESG = 'Could not open ' // FNAME
                CALL M3WARN( NAMBUF, JDATE, JTIME, MESG )

                EFLAG = .TRUE.
                GO TO  1

            END IF      !  if open3() failed

            FID = INDEX1( FIL16, COUNT3, FLIST3 )

            IF ( FID .EQ. 0 ) THEN      !  lookup failed

                MESG = 'Error accessing ' // FNAME
                CALL M3WARN( NAMBUF, JDATE, JTIME, MESG )

                EFLAG = .TRUE.
                GO TO  1

            END IF      !  if file error

        END IF          !  if file needs opening

1       CONTINUE

!$OMP END CRITICAL( S_INTERP )

        IF ( EFLAG ) THEN
            IF ( XFLAG ) THEN
                INTERPX = .FALSE.
            ELSE
                IFLAG3 = .FALSE.
            END IF
            RETURN
        END IF


        IDUM = FTYPE3( FID )
        IF ( IDUM .EQ. CUSTOM3 ) THEN

            DIMS( 1 ) = 1
            DELS( 1 ) = NCOLS3( FID )

            DIMS( 2 ) = 1
            DELS( 2 ) = NLAYS3( FID )

            TDIM  = 3

        ELSE IF ( IDUM .EQ. GRDDED3 .OR. IDUM .EQ. MPIGRD3 ) THEN

            DIMS( 1 ) = 1
            DELS( 1 ) = NCOLS3( FID )

            DIMS( 2 ) = 1
            DELS( 2 ) = NROWS3( FID )

            DIMS( 3 ) = 1
            DELS( 3 ) = NLAYS3( FID )

            TDIM  = 4

        ELSE IF ( IDUM .EQ. BNDARY3 ) THEN

            DELTA = 2 * NTHIK3( FID )
            DELTA = ABS( DELTA ) * ( NCOLS3( FID )
     &                             + NROWS3( FID ) + DELTA )

            DIMS( 1 ) = 1
            DELS( 1 ) = DELTA

            DIMS( 2 ) = 1
            DELS( 2 ) = NLAYS3( FID )

            TDIM  = 3

        ELSE        !  bad file type for interp3()

            IF ( IDUM .LT. -2   .OR.  IDUM .GT. 5 ) IDUM = -3
            MESG = 'Can not interpolate:  file "' //
     &              FIL16( 1:FLEN ) //
     &              '" of type ' // TYPNAMES( IDUM )
            CALL M3WARN( NAMBUF, JDATE, JTIME, MESG )

            IF ( XFLAG ) THEN
                INTERPX = .FALSE.
            ELSE
                IFLAG3 = .FALSE.
            END IF
            RETURN

        END IF              !  check file types; set DIMS, DELS, TDIM

        IF ( XFLAG ) THEN    !  call to interpx:

            EFLAG = .FALSE.
            IF ( LAY0 .LT. 1 .OR. LAY0 .GT. LAY1 ) THEN
                EFLAG = .TRUE.
                WRITE( MESG, '( A, I10 )' )
     &                  'Bad argument LAY0=', LAY0
                CALL M3MSG2( MESG )
            END IF
            IF ( LAY1 .LT. LAY0 .OR. LAY1 .GT.  NLAYS3( FID ) ) THEN
                EFLAG = .TRUE.
                WRITE( MESG, '( A, I10 )' )
     &                  'Bad argument LAY1=', LAY1
                CALL M3MSG2( MESG )
            END IF
            IF ( FTYPE3( FID ) .EQ. CUSTOM3 ) THEN
                IF ( COL0 .LT. 1 .OR. COL0 .GT.  COL1 ) THEN
                    EFLAG = .TRUE.
                    WRITE( MESG, '( A, I10 )' )
     &                  'Bad argument COL0=', COL0
                    CALL M3MSG2( MESG )
                END IF
                IF ( COL1 .LT. COL0 .OR. COL1 .GT.  NCOLS3( FID ) ) THEN
                    EFLAG = .TRUE.
                    WRITE( MESG, '( A, I10 )' )
     &                  'Bad argument COL1=', COL1
                    CALL M3MSG2( MESG )
                END IF
            ELSE IF ( FTYPE3( FID ) .EQ. GRDDED3 ) THEN
                IF ( COL0 .LT. 1 .OR. COL0 .GT.  COL1 ) THEN
                    EFLAG = .TRUE.
                    WRITE( MESG, '( A, I10 )' )
     &                  'Bad argument COL0=', COL0
                    CALL M3MSG2( MESG )
                END IF
                IF ( COL1 .LT. COL0 .OR. COL1 .GT.  NCOLS3( FID ) ) THEN
                    EFLAG = .TRUE.
                    WRITE( MESG, '( A, I10 )' )
     &                  'Bad argument COL1=', COL1
                    CALL M3MSG2( MESG )
                END IF
                IF ( ROW0 .LT. 1 .OR. ROW0 .GT.  ROW1 ) THEN
                    EFLAG = .TRUE.
                    WRITE( MESG, '( A, I10 )' )
     &                  'Bad argument ROW0=', ROW0
                    CALL M3MSG2( MESG )
                END IF
                IF ( ROW1 .LT. ROW0 .OR. ROW1 .GT.  NROWS3( FID ) ) THEN
                    EFLAG = .TRUE.
                    WRITE( MESG, '( A, I10 )' )
     &                  'Bad argument ROW1=', ROW1
                    CALL M3MSG2( MESG )
                END IF
            END IF

            IF ( EFLAG ) THEN

                MESG   = 'Bad window dimension(s)'
                CALL M3WARN( NAMBUF, JDATE, JTIME, MESG )
                INTERPX = .FALSE.
                RETURN

            END IF              !  if eflag

        ELSE    !  call to interp3:

            IF ( RSIZE .NE. NLAYS3( FID ) * BSIZE3( FID ) ) THEN

                WRITE( MESG,94010 )
     &          'Size error for ' //VAR16( 1:VLEN )//
     &          ' from ' //         FIL16( 1:FLEN ) //
     &          '--REQ:', RSIZE, ' ACT:', NLAYS3( FID )*BSIZE3( FID )

                CALL M3WARN( NAMBUF, JDATE, JTIME, MESG )

                IFLAG3 = .FALSE.
                RETURN

            END IF          !  requested record-size in error

        END IF          !  if XFLAG, or not


C.......   Find index for variable, and if necessary allocate circular
C.......   buffers for interpolation, and initialize related data structures.

        VID = INDEX1( VNAME, NVARS3( FID ), VLIST3( 1,FID ) )

        IF ( VID .EQ. 0 ) THEN  !  variable not available in that file

            MESG = 'Variable "' // VAR16( 1:VLEN ) //
     &             '" not in file ' // FNAME
            CALL M3WARN( NAMBUF, JDATE, JTIME,MESG )
            IF ( XFLAG ) THEN
                INTERPX = .FALSE.
            ELSE
                IFLAG3 = .FALSE.
            END IF
            RETURN

        ELSE IF ( VTYPE3( VID,FID ) .NE. M3REAL  .AND.
     &            VTYPE3( VID,FID ) .NE. M3DBLE ) THEN  ! variable of wrong type

            MESG = 'Variable "' // VAR16( 1:VLEN ) //
     &             '" not of type REAL/DOUBLE in file ' // FNAME
            CALL M3WARN( NAMBUF, JDATE, JTIME,MESG )
            IF ( XFLAG ) THEN
                INTERPX = .FALSE.
            ELSE
                IFLAG3 = .FALSE.
            END IF
            RETURN

        END IF    !  if variable not available; else if not already set up

#ifdef IOAPICPL
        IF ( CDFID3( FID ) .EQ. VIRFIL3 ) THEN !  virtual file
            EFLAG = (.NOT. INTPQV( FID, VID, JDATE, JTIME, P, Q ) )
            PP    = DBLE( P )
            QQ    = 1.0D0 - PP
            GO TO   1001
        END IF
#endif

C.......   Interpolate/copy to JDATE:JTIME

!$OMP       CRITICAL( S_INTERP )        !!  avoid race conditions for
                                        !!  concurrent same-variable calls
                                        !!  to INTERPX(), etc.

        IF ( TSTEP3( FID ) .EQ. 0 ) THEN        !  time-independent file

            RFLAG =  ( LDATE3( VID,FID ) .NE. 0 )

            IF ( RFLAG  ) THEN

                IF ( CDFID3( FID ) .EQ. BUFFIL3 ) THEN

                    MESG = 'Variable "'//VAR16( 1:VLEN ) //
     &                     '" not yet written to BUFFERED file "' //
     &                     FIL16( 1:FLEN )  // '".'
                    CALL M3WARN( NAMBUF, JDATE, JTIME, MESG )
                    EFLAG   = .TRUE.

                ELSE IF (.NOT.RDTFLAG( FID,VID,0,0,STEP,.TRUE. ) ) THEN

                    MESG = 'Record not available for "' //
     &                           VAR16( 1:VLEN ) //
     &                           '" from file '// FNAME
                    CALL M3WARN( NAMBUF, 0, 0, MESG )
                    EFLAG   = .TRUE.
                    GO TO 999

                END IF      !  if bad cdfid3() or bad rdtflag()

            END IF      !  if RFLAG

            P     =  1.0
            Q     =  0.0
            PP    =  1.0D0
            QQ    =  0.0D0
            DELTA =  0
            DATE1 =  0
            TIME1 =  0
            DIMS( TDIM ) = 1
            DELS( TDIM ) = 1
            ILAST3( VID,FID ) = 0
            LDATE3( VID,FID ) = 0
            LTIME3( VID,FID ) = 0
            NDATE3( VID,FID ) = 0
            NTIME3( VID,FID ) = 0

        ELSE IF ( CDFID3( FID ) .EQ. BUFFIL3 ) THEN     !  "buffered" file

            RFLAG = .FALSE.

            IF( LDATE3( VID,FID ) .LT. 0 ) THEN

                IF ( ( NDATE3( VID,FID ) .EQ. JDATE ) .AND.
     &               ( NTIME3( VID,FID ) .EQ. JTIME )    ) THEN

                     Q  = 1.0
                     P  = 0.0
                     QQ = 1.0D0
                     PP = 0.0D0

                ELSE

                    MESG = 'Variable "' //
     &                       VAR16( 1:VLEN ) //
     &                       '" not yet written to BUFFERED file "' //
     &                       FIL16( 1:FLEN ) // '".'
                    CALL M3WARN( NAMBUF, JDATE, JTIME, MESG )
                    EFLAG   = .TRUE.

                END IF

            ELSE

                DT   = TIME2SEC( TSTEP3( FID ) )
                DTJL = SECSDIFF( LDATE3( VID,FID ), LTIME3( VID,FID ),
     &                           JDATE,             JTIME )

                IF ( DTJL .LT. 0 ) THEN     !  ldate:ltime not OK for interp

                    WRITE( MESG, 94030 )
     &              JDATE, JTIME,
     &              ' n/a for ' // VAR16( 1:VLEN ) //
     &              ' from '    // FIL16( 1:FLEN ) //
     &              ' (',
     &              SDATE3( FID ), STIME3( FID ),
     &              ' by', TSTEP3( FID ), ')'
                    CALL M3WARN( NAMBUF, JDATE, JTIME, MESG )
                    EFLAG   = .TRUE.

                ELSE IF ( DTJL .GT. 0 ) THEN !  interp between ld:lt and nd:nt

                    DTNJ = SECSDIFF( JDATE,             JTIME,
     &                        NDATE3( VID,FID ), NTIME3( VID,FID ) )
                    IF ( ( DTNJ .LT. 0 ) .OR. DTNJ .GT. DT ) THEN
                        WRITE( MESG, 94030 )
     &                  JDATE, JTIME,
     &                  ' n/a for ' // VAR16( 1:VLEN ) //
     &                  ' from '    // FIL16( 1:FLEN ) //
     &                  ' (',
     &                  SDATE3( FID ), STIME3( FID ),
     &                  ' by', TSTEP3( FID ), ')'
                        CALL M3WARN( NAMBUF, JDATE, JTIME, MESG )
                        EFLAG   = .TRUE.

                    END IF      !  if ndate:ntime not OK for interp

                END IF      !  if dtjl < 0; else if dtjl > 0 (just copy if dtjl=0)

                QQ   = DBLE( DTJL ) / DBLE( DT )  !  Interpolation coefficients
                PP   = 1.0 - QQ                   ! = dble(dt - dtjl)/dble( dt )
                Q    = QQ
                P    = PP

            END IF

        ELSE            !  time-stepped file.

C...........   Check to see relationship between currently circular buffer
C...........   start and ending times, and requested time.  Two cases
C...........   require updates:  (1) need new data at both ends; and
C...........   (2) need new data at the futureward end only. (Needing
C...........   data at the pastward end is still treated under (1).)

            DT = TIME2SEC( TSTEP3( FID ) )

            IF ( LDATE3( VID,FID ) .GT. 0 ) THEN
                DTJL = SECSDIFF( LDATE3( VID,FID ), LTIME3( VID,FID ),
     &                           JDATE,             JTIME )
            ELSE                !  ldate set to "invalid"
                DTJL = -1       !  dtjl "invalid"
            END IF

            IF ( ( DTJL .LT. 0 ) .OR. ( DTJL .GT. 2*DT ) ) THEN ! read both

                IF ( CURRSTEP( JDATE, JTIME,
     &                         SDATE3( FID ), STIME3( FID ),
     &                         TSTEP3( FID ),
     &                         MDATE, MTIME ) ) THEN

                    RFLAG = .TRUE.

                    DATE1 = MDATE
                    DATE2 = MDATE
                    TIME1 = MTIME
                    TIME2 = MTIME
                    CALL NEXTIME( DATE2, TIME2, TSTEP3( FID ) )

                    !  **NOTE** --  use DATE1:TIME1 "step" in DIMS(:) below.
                    !  If DATE1:TIME1 and DATE2:TIME2 both OK, then do
                    !  normal double-buffered INTERP;
                    !  else if only DATE1:TIME1 OK then set up single-record
                    !  read and copy
                    !  else fail

                    IF ( .NOT. RDTFLAG( FID, VID,
     &                                  DATE1, TIME1,
     &                                  STEP, .TRUE. ) ) THEN

                        MESG = 'Time step not available for "' //
     &                       VAR16( 1:VLEN ) //
     &                       '" from ' // FNAME
                        CALL M3WARN( NAMBUF, DATE2, TIME2, MESG )
                        EFLAG   = .TRUE.
                        GO TO 999

                    ELSE IF ( .NOT. RDTFLAG( FID, VID,
     &                                       DATE2, TIME2,
     &                                       STEP2, .TRUE. ) ) THEN

                        DTJL = SECSDIFF( MDATE, MTIME,
     &                                   JDATE, JTIME )

                        IF ( DTJL .EQ. 0 ) THEN         !  do copy-operation

                            DELTA =  0                  !  read starts at 2
                            ILAST3( VID, FID ) = 0      !  buffer starts at 0
                            LDATE3( VID, FID ) = MDATE
                            LTIME3( VID, FID ) = MTIME
                            DELS( TDIM ) = 1            !  read one time step
                            NDATE3( VID,FID ) = IMISS3
                            NTIME3( VID,FID ) = IMISS3

                        ELSE

                            MESG = 'Time step not available for "' //
     &                           VAR16( 1:VLEN ) //
     &                           '" from ' // FNAME
                            CALL M3WARN( NAMBUF, DATE1, TIME1, MESG )
                            EFLAG   = .TRUE.
                            GO TO 999

                        END IF          !  if dtjl=0, or not

                    ELSE

                        DELTA =  0                      !  read starts at 2
                        ILAST3( VID, FID ) = 0          !  buffer starts at 0
                        LDATE3( VID, FID ) = MDATE
                        LTIME3( VID, FID ) = MTIME
                        DELS( TDIM ) = 2                !  read BOTH time steps
                        NDATE3( VID,FID ) = DATE2
                        NTIME3( VID,FID ) = TIME2

                        DTJL = SECSDIFF( MDATE, MTIME,
     &                                   JDATE, JTIME )

                        IF ( TSTEP3( FID ) .LT. 0 ) THEN
                            DIMS( TDIM ) = 1 + MOD( STEP-1, 2 )
                        ELSE
                            DIMS( TDIM ) = STEP
                        END IF

                    END IF      !  end checking time step flags.

                ELSE

                    WRITE( MESG, 94030 )
     &                  JDATE, JTIME,
     &                  ' n/a for ' // VAR16( 1:VLEN ) //
     &                  ' from '    // FIL16( 1:FLEN ) //
     &                  ' (',
     &                  SDATE3( FID ), STIME3( FID ),
     &                  ' by', TSTEP3( FID ), ')'
                    CALL M3WARN( NAMBUF, JDATE, JTIME, MESG )
                    EFLAG = .TRUE.

                END IF  !  if requested date&time after start of file, or not

            ELSE IF ( DTJL .GT. DT ) THEN      !  advance by one time step

                DATE1 = NDATE3( VID, FID )
                TIME1 = NTIME3( VID, FID )
                CALL NEXTIME( DATE1, TIME1, TSTEP3( FID ) )

                IF ( .NOT. RDTFLAG( FID, VID, DATE1, TIME1,
     &                              STEP, .TRUE. ) ) THEN

                    MESG = 'Time step not available for "' //
     &                      VAR16( 1:VLEN ) //
     &                     '" from ' // FNAME
                    CALL M3WARN( NAMBUF, DATE1, TIME1, MESG )
                    EFLAG   = .TRUE.
                    GO TO 999

                END IF

                DELTA = ILAST3( VID,FID )        !  overwrite ILAST entry
                ILAST3( VID,FID ) = 1 - DELTA    !  new ILAST after read:
                                                !  formula swaps 0,1
                DTJL  = DTJL - DT
                RFLAG = .TRUE.
                DELS( TDIM ) = 1                      !  read one time

                LDATE3( VID,FID ) = NDATE3( VID, FID )
                LTIME3( VID,FID ) = NTIME3( VID, FID )
                NDATE3( VID,FID ) = DATE1
                NTIME3( VID,FID ) = TIME1

                IF ( TSTEP3( FID ) .LT. 0 ) THEN
                    DIMS( TDIM ) = 1 + MOD( STEP-1, 2 )
                ELSE
                    DIMS( TDIM ) = STEP
                END IF

            ELSE        !  do not need to read this variable from this file:

                RFLAG = .FALSE.

            END IF      !  if circular-buffer structure needed updating

            QQ = DBLE( DTJL ) / DBLE( DT )  !  Interpolation coefficients
            PP = 1.0 - QQ                   ! = dble(dt - dtjl)/dble( dt )
            Q  = QQ
            P  = PP

        END IF          ! time stepped file or not

999     CONTINUE

C..........   If read will be necessary, check timestep flags first:

        IF ( RFLAG .AND. .NOT. EFLAG ) THEN   !  check record-available flag

            IF ( VTYPE3( VID,FID ) .EQ. M3REAL ) THEN
                IF ( 0 .EQ. BUFVGT3( FID, VID, RFLAG, DIMS, DELS,
     &                           NLAYS3( FID ) * BSIZE3( FID ),
     &                           DELTA, TSTEP3( FID ) ) ) THEN

                    WRITE( MESG,94040 ) 'Timesteps:  LAST',
     &                         LDATE3( VID,FID ), LTIME3( VID,FID ),
     &              '; NEXT',  NDATE3( VID,FID ), NTIME3( VID,FID )
                    CALL M3MSG2( MESG )
                    MESG = 'Error reading ' //
     &                      VAR16( 1:VLEN ) // ' from ' //
     &                      FIL16( 1:FLEN )
                    CALL M3WARN( NAMBUF, JDATE, JTIME, MESG )
                    EFLAG   = .TRUE.
                END IF              !  if bufvgt() fails
            ELSE IF ( VTYPE3( VID,FID ) .EQ. M3DBLE ) THEN
                IF ( 0 .EQ. BUFVGT3D( FID, VID, RFLAG, DIMS, DELS,
     &                           NLAYS3( FID ) * BSIZE3( FID ),
     &                           DELTA, TSTEP3( FID ) ) ) THEN

                    WRITE( MESG,94040 ) 'Timesteps:  LAST',
     &                         LDATE3( VID,FID ), LTIME3( VID,FID ),
     &              '; NEXT',  NDATE3( VID,FID ), NTIME3( VID,FID )
                    CALL M3MSG2( MESG )
                    MESG = 'Error reading ' //
     &                      VAR16( 1:VLEN ) // ' from ' //
     &                      FIL16( 1:FLEN )
                    CALL M3WARN( NAMBUF, JDATE, JTIME, MESG )
                    EFLAG   = .TRUE.
                END IF              !  if bufvgt() fails
            ELSE
                EFLAG   = .TRUE.
            END IF

        END IF

!$OMP   END CRITICAL( S_INTERP )

1001    CONTINUE
        IF ( EFLAG ) THEN
            IF ( XFLAG ) THEN
                INTERPX = .FALSE.
            ELSE
                IFLAG3 = .FALSE.
            END IF
            RETURN
        END IF

C...........   Call BUFINT3 to interpolate; check its success/failure
C...........   status and return accordingly:

        IF ( XFLAG ) THEN       !  finish interpx:

            IF ( FTYPE3( FID ) .EQ. CUSTOM3 ) THEN
                DIMS(1) = NCOLS3( FID )
                DELS(1) = COL0
                DELT(1) = COL1
                DIMS(2) = 1
                DELS(2) = 1
                DELT(2) = 1
            ELSE IF ( FTYPE3( FID ) .EQ. BUFFIL3 ) THEN
                DIMS(1) = BSIZE3( FID )
                DELS(1) = 1
                DELT(1) = BSIZE3( FID )
                DELS(2) = 1
                DELT(2) = 1
            ELSE IF ( FTYPE3( FID ) .EQ. GRDDED3 ) THEN
                DIMS(1) = NCOLS3( FID )
                DELS(1) = COL0
                DELT(1) = COL1
                DIMS(2) = NROWS3( FID )
                DELS(2) = ROW0
                DELT(2) = ROW1
            END IF
            DIMS(3) = NLAYS3( FID )
            DELS(3) = LAY0
            DELT(3) = LAY1

            IF ( VTYPE3( VID,FID ) .EQ. M3REAL ) THEN
                INTERPX = ( BUFINTX( FID, VID,
     &                               NLAYS3( FID )*BSIZE3( FID ),
     &                               ILAST3( VID,FID ), TSTEP3( FID ),
     &                               DIMS, DELS, DELT,
     &                               P, Q, BUFFER ) .NE. 0 )
            ELSE IF ( VTYPE3( VID,FID ) .EQ. M3DBLE ) THEN
                INTERPX = ( BUFINTXD( FID, VID,
     &                                NLAYS3( FID )*BSIZE3( FID ),
     &                                ILAST3( VID,FID ), TSTEP3( FID ),
     &                                DIMS, DELS, DELT,
     &                                PP, QQ, BUFFER ) .NE. 0 )
            END IF

            EFLAG   = ( .NOT. INTERPX )

        ELSE IF ( VTYPE3( VID,FID ) .EQ. M3REAL ) THEN

            IFLAG3 = ( BUFINT3( FID, VID, NLAYS3( FID )*BSIZE3( FID ),
     &                           ILAST3( VID,FID ), TSTEP3( FID ),
     &                           P, Q, BUFFER ) .NE. 0 )
            EFLAG   = ( .NOT. IFLAG3 )

        ELSE IF ( VTYPE3( VID,FID ) .EQ. M3DBLE ) THEN

            IFLAG3 = ( BUFINT3D( FID, VID, NLAYS3( FID )*BSIZE3( FID ),
     &                            ILAST3( VID,FID ), TSTEP3( FID ),
     &                            PP, QQ, BUFFER ) .NE. 0 )
            EFLAG   = ( .NOT. IFLAG3 )

        END IF          !  if XFLAG or not

        IF ( EFLAG ) THEN
            MESG   =  'Could not interpolate "'
     &                 // VAR16( 1:VLEN )
     &                 // '" from file "'
     &                 // FIL16( 1:FLEN ) // '"'
            CALL M3WARN( NAMBUF, JDATE, JTIME, MESG )
            IF( CDFID3( FID ) .GE. 0 ) THEN	!  not a buffered virtual file
                LDATE3( VID, FID ) = -1         !  "invalid"
            END IF
        END IF

        RETURN          !  from INTERP3/INTERPX

C******************  FORMAT  STATEMENTS   ******************************

C...........   Internal buffering formats............ 94xxx

94010   FORMAT( A, I7, A, I7 )

94030   FORMAT( I7.7, ':', I6.6, A, I7.7, ':', I6.6, A, I8 , A )

94040   FORMAT( 2( A, :, I9.7, ':', I6.6, : ) )


        END FUNCTION INTERP3

