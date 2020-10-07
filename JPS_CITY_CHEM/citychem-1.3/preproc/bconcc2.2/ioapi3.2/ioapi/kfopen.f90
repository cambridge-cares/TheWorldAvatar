
LOGICAL FUNCTION  KFOPEN( FNAME, FSTATUS, PGNAME, KFCOUNT )  RESULT( KFFLAG )

    !!***********************************************************************
    !! Version "$Id: kfopen.f90 1 2017-06-10 18:05:20Z coats $"
    !! EDSS/Models-3 I/O API.
    !! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    !! (C) 2003-2013 Baron Advanced Meteorological Systems,
    !! (C) 2007-2013 Carlie J. Coats, Jr., and
    !! (C) 2014-2015 UNC Institute for the Environment.
    !! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    !! See file "LGPL.txt" for conditions of use.
    !!.........................................................................
    !!  function body starts at line  86
    !!
    !!  FUNCTION:  open KF-cloud file with logical name FNAME, with
    !!             file status FSTATUS = FSREAD3==1 for read-only,
    !!             FSRDWR3==2 for read/write/update of existing files,
    !!             FSNEW3==3 for read/write of new files, or FSUNKN3==4
    !!             for read/write/update of unknown (new vs. old) files.
    !!             If opened for write, copies scenario description from
    !!             I/O STATE3.EXT to file's history, and name PGNAME of
    !!             caller to file's updater-name.  Sets KFCOUNT
    !!             Returns TRUE if the file is already open.
    !!
    !!  RETURN VALUE:  TRUE iff it succeeds in opening the file, reading its
    !!                 attributes, and storing the relevant ones in STATE3.EXT
    !!
    !!  PRECONDITIONS REQUIRED:
    !!       FSREAD3 or FSRDWR3:  File FNAME already exists.
    !!       FSNEW3:  file must _not_ already exist.
    !!       FSNEW3, FSUNKN3:  user must supply file description in FDESC3.EXT
    !!       COMMONS
    !!
    !!  SUBROUTINES AND FUNCTIONS CALLED:  INDEX1, INIT3
    !!
    !!  REVISION  HISTORY:
    !!       Adapted   4/1996 by CJC from OPEN3()
    !!
    !!       Modified  5/1996 by CJC to support new mode FSCREA3 for opening files.
    !!
    !!       Modified  8/1999 by CJC:  OpenMP thread-safe; unified with OPEN3()
    !!
    !!       Modified  5/2003 by CJC:  critical-section change (deadlock-removal)
    !!
    !!       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !!
    !!      Modified 02/2015 by CJC for I/O API 3.2: USE M3UTILIO
    !!
    !!      Modified 10/2015 by CJC for I/O API 3.2: USE MODNCFIO,
    !!      use NF_*() interfaces.  F90 "free" source-format
    !!***********************************************************************

    USE M3UTILIO
    USE MODNCFIO

    IMPLICIT NONE

    !!...........   INCLUDES:

    INCLUDE 'STATE3.EXT'


    !!...........   ARGUMENTS and their descriptions:

    CHARACTER*(*), INTENT(IN   ) :: FNAME   !  logical name of file to be opened
    INTEGER      , INTENT(IN   ) :: FSTATUS !  read-only, read-write, new, or unknown
    CHARACTER*(*), INTENT(IN   ) :: PGNAME  !  name of calling program
    INTEGER      , INTENT(  OUT) :: KFCOUNT( * )  !  gridded event counts

    !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         I
    INTEGER         FID             !  subscript for STATE3 arrays
    INTEGER         FNUM            !  netCDF file ID
    INTEGER         IERR            !  netCDF error status return
    LOGICAL         AFLAG           !  return value from INQUIRE
    CHARACTER*256   EQNAME          !  environment value of FNAME
    CHARACTER*256   MESG
    INTEGER         DIMS( 5 )      !  corner arg array for NF_GET_VARA_*()
    INTEGER         DELS( 5 )      !  corner arg array for NF_GET_VARA_*()

    !!.............................................................................
    !!   begin body of subroutine  KFOPEN

    IF ( LEN( FNAME ) .GT. 16 ) THEN
        WRITE( MESG, '( 3A, 2X, A , I5 )' )                     &
            'File name length bad for "', FNAME, '"',           &
            'Max file name length 16; actual:', LEN( FNAME )
        CALL M3WARN( 'KFOPEN', 0, 0, MESG )
        KFFLAG = .FALSE.
        RETURN
    END IF

    CALL NAMEVAL( FNAME, EQNAME )

    I = MAX ( INDEX( EQNAME, '-v' ) , INDEX( EQNAME, '-V' ) )
    IF ( I .GT. 0 ) THEN
        EQNAME( I:I+1 ) = '  '        !  fix the '-v' (etc.)
    END IF


    !!.......   Find netCDF index for the file, and check time step availability:
    !!.......   AFLAG indicates whether the file exists before the call to OPEN3,
    !!.......   or not, hence whether we read KFCOUNT from the file or write it
    !!.......   to the file.

!$OMP   CRITICAL( S_KFO )

    INQUIRE ( FILE = EQNAME, EXIST = AFLAG )

    IF ( OPEN3( FNAME, FSTATUS, PGNAME ) ) THEN

        FID  = INDEX1( FNAME, COUNT3, FLIST3 )
        FNUM = CDFID3( FID )
        DIMS( 1 ) = 1
        DELS( 1 ) = NCOLS3( FID )
        DIMS( 2 ) = 1
        DELS( 2 ) = NROWS3( FID )
        WRITE( MESG, '(A, I7, 2( 2X, A ) )' ) 'NetCDF ID=', FNUM, 'for file', FNAME

        IF ( AFLAG ) THEN

            !!.......   Read KFCOUNT from FNAME:

            IF ( VOLAT3( FID ) ) THEN     !  volatile file:  synch with disk
                IERR = NF_SYNC( FNUM )
                IF ( IERR .NE. 0 ) THEN
                    CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error with input disk synchronization' )
                    KFFLAG = .FALSE.
                    GO TO 999       !  to return
                END IF              !  if synch failed
            END IF          !  if file is volatile

            IERR = NF_GET_VARA_INT( FNUM, NINDX3( FID ), DIMS, DELS, KFCOUNT )
            IF ( IERR .EQ. 0 ) THEN
                KFFLAG = .TRUE.
            ELSE
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading variable KFCOUNT' )
                KFFLAG = .FALSE.
            END IF          !  ierr nonzero:  NCVGTC() failed, or succeeded

        ELSE

            !!.......   Initialize KFCOUNT and write it to FNAME:

            DO  I = 1, NCOLS3( FID ) * NROWS3( FID )
                KFCOUNT( I ) = 0
            END DO

            IERR = NF_PUT_VARA_INT( FNUM, NINDX3( FID ), DIMS, DELS, KFCOUNT )
            IF ( IERR .EQ. 0 ) THEN
                KFFLAG = .TRUE.
            ELSE
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error initializing KFCOUNT' )
                KFFLAG = .FALSE.
                GO TO 999   !  to return
            END IF          !  ierr nonzero:  NCENDF() failed

            IF ( VOLAT3( FID ) ) THEN     !  volatile file:  synch with disk
                IERR = NF_SYNC( FNUM )
                IF ( IERR .EQ. 0 ) THEN
                    KFFLAG = .TRUE.
                ELSE
                    CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error with output disk synchronization' )
                    KFFLAG = .FALSE.
                END IF              !  if synch failed
            END IF          !  if file is volatile

        END IF

    ELSE

        KFFLAG = .FALSE.    !  open3() failed

    END IF          !  if open3() succeeded or not

999     CONTINUE

!$OMP   END CRITICAL( S_KFO )

    RETURN


END FUNCTION  KFOPEN

