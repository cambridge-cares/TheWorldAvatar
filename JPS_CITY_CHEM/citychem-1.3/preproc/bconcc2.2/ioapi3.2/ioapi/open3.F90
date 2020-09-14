
LOGICAL FUNCTION  OPEN3( FNAME, FSTATUS, PGNAME )

    !!***********************************************************************
    !! Version "$Id: open3.F90 1 2017-06-10 18:05:20Z coats $"
    !! EDSS/Models-3 I/O API.
    !! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    !! (C) 2003-2013 Baron Advanced Meteorological Systems,
    !! (C) 2007-2013 Carlie J. Coats, Jr., and
    !! (C) 2015 UNC Institute for the Environment.
    !! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    !! See file "LGPL.txt" for conditions of use.
    !!.........................................................................
    !!  function body starts at line  135
    !!
    !!  FUNCTION:
    !!      open Models-3 file with logical name FNAME, with file status
    !!      FSTATUS = FSREAD3==1 for read-only,
    !!      FSRDWR3==2 for read/write/update of existing files,
    !!      FSNEW3 ==3 for read/write of new files,
    !!      FSUNKN3==4 for read/write/update of unknown (new vs. old) files, or
    !!      FSCREA3==5 for delete-and-reopen for write.
    !!      If opened for write, copies scenario description from 
    !!      I/O STATE3.EXT to file's history, and name PGNAME of 
    !!      caller to file's updater-name.  
    !!      Returns TRUE if the file is already open.
    !!
    !!  RETURN VALUE:
    !!      TRUE iff it succeeds in opening the file, reading its attributes,
    !!      and storing the relevant ones in STATE3.EXT
    !!
    !!  PRECONDITIONS REQUIRED:  
    !!      FSREAD3 or FSRDWR3:  File FNAME already exists.
    !!      FSNEW3:  file must _not_ already exist.
    !!      FSCREA3: file deleted and new file created if it already exist.
    !!      FSCREA3, FSNEW3, FSUNKN3:  user must supply file description in 
    !!	FDESC3.EXT COMMONs
    !!
    !!  SUBROUTINES AND FUNCTIONS CALLED:
    !!      CKDESC3, CHKFIL3, CLOSE3, CRTBUF3, CRTFIL3, INDEX1, INIT3,
    !!      OPNFIL3, OPNLIST3, RMFILE
    !!
    !!  REVISION  HISTORY:
    !!      prototype 3/1992 by CJC
    !!      modified  7/1992 by CJC:  handles coordinate system, grid 
    !!                              description attributes in file headers,
    !!                              in an upward-compatible fashion.
    !!      Modified  9/1994 by CJC:  4-way mode of file opening (read-only, 
    !!                              read-write, new, unknown, using parameter 
    !!                              tokens from PARMS3.EXT); 
    !!                              also BUFFERED virtual files.
    !!      Modified  8/1995 by CJC to support CLOSE3()
    !!      Modified  5/1996 by CJC to support new mode FSCREA3 for opening files.
    !!      Modified  5/1998 by CJC for OpenMP thread-safety
    !!      Modified  4/1997 by CJC to fix bug in "already-open" warning formats
    !!      Modified  5/1998 by CJC for OpenMP thread-safety
    !!      Modified  5/1999 by ALT for coupling-mode operation
    !!      Modified  9/1999 by CJC unification with KFOPEN()
    !!      Modified  2/2002 by CJC check TRIMLEN() of FNAME; File-list
    !!      multi-file input data sets;
    !!      Modified  3/2002 by CJC:  STATE3V changes
    !!      Bugfix    5/2003 by CJC:  crtbuf3() is LOGICAL, not INTEGER
    !!      (correction by David Wong, US EPA)
    !!      Modified 7/2003 by CJC:  bugfix -- clean up critical sections
    !!      associated with INIT3()
    !!      Modified 10/2003 by CJC for I/O API version 3:  support for
    !!      native-binary BINFIL3 file type; uses INTEGER NAME2FID
    !!      Modified 12/2004 by CJC: implement NVARS range-checks
    !!      Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !!      Modified 08/2015 by CJC for PnetCDF distributed I/O.  USE MODNCFIO,
    !!      USE MODPDATA; free F90 source format; use NF_ interfaces;
    !!      free F90 source format
    !!      Modified 12/2015 by CJC:  Move PN_SETUP() here from INIT3()
    !!      because of infinite recursion-loop problem
    !!***********************************************************************

    USE MODNCFIO
    USE MODPDATA

    IMPLICIT NONE

    !!...........   INCLUDES:

    INCLUDE 'PARMS3.EXT'
    INCLUDE 'STATE3.EXT'
#ifdef IOAPICPL
    INCLUDE 'STATE3V.EXT'
    LOGICAL, EXTERNAL :: OPEN3V, UPDTVIR3
    CHARACTER *256 PGMNAME
#endif


    !!...........   ARGUMENTS and their descriptions:

    CHARACTER*(*), INTENT(IN   ) :: FNAME   !  logical name of file to be opened
    INTEGER      , INTENT(IN   ) :: FSTATUS !  read-only, read-write, new, or unknown
    CHARACTER*(*), INTENT(IN   ) :: PGNAME  !  name of calling program


    !!...........   EXTERNAL FUNCTIONS and their descriptions:

    INTEGER, EXTERNAL :: INDEX1
    LOGICAL, EXTERNAL :: CLOSE3
    LOGICAL, EXTERNAL :: CKDESC3  !  does header validity checks
    LOGICAL, EXTERNAL :: CHKFIL3  !  does work of checking file def consistency
    LOGICAL, EXTERNAL :: CRTBUF3  !  create "buffered" virtual files
    LOGICAL, EXTERNAL :: CRTFIL3  !  does work of creating new files
    INTEGER, EXTERNAL :: NAME2FID !  fname~~> fid lookup
    LOGICAL, EXTERNAL :: OPNFIL3  !  does work of opening "old" files
    LOGICAL, EXTERNAL :: PN_CRTFIL3, PN_OPNFIL3
    LOGICAL, EXTERNAL :: OPNLIST3 !   " multi-file FILE-LIST data sets
    INTEGER, EXTERNAL :: RMFILE   !  shell around UNIX "unlink()" system call
    EXTERNAL          :: INITBLK3 !  BLOCK DATA to initialize STATE3 commons


!!...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         IDUM    !  dummy for INIT3 return value
    INTEGER         FID     !  subscript for STATE3 arrays
    INTEGER         RWMODE  !  netCDF mode corresponding to RDONLY
    INTEGER         IERR    !  netCDF error status return
    LOGICAL         AFLAG   !  return value from INQUIRE
    LOGICAL         MPIFLAG !  "MPI:<path>" ??
    LOGICAL         BINFLAG !  native-binary data set
    LOGICAL         LISTFLAG!  multi-file LIST-FILE data set
    CHARACTER*512   EQNAME  !  environment value of FNAME
    CHARACTER*8     BNAME   !  for "BUFFERED", etc.
    CHARACTER*16    PRG16   !  scratch  pgm-name buffer
    LOGICAL, SAVE :: FIRSTIME = .TRUE.
    CHARACTER*512   MESG    !  for m3msg2, m3warn

    !!.............................................................................
    !!   begin body of subroutine  OPEN3

!$OMP   CRITICAL( S_NC )
    IF ( FIRSTIME ) THEN
#ifdef IOAPI_PNCF
        CALL PN_SETUP( )
        PN_MODE = .TRUE.
#else
        PN_MODE = .FALSE.
#endif
        FIRSTIME = .FALSE.
    END IF
!$OMP   END CRITICAL( S_NC )

    !!.......   Find STATE3 index for the file:

    MPIFLAG  = .FALSE.
    BINFLAG  = .FALSE.
    LISTFLAG = .FALSE.

    FID = NAME2FID( FNAME )

    IF ( FID .NE. 0 ) THEN  !  file already open.  Consistency checks:

        IF ( FSTATUS .EQ. FSNEW3 ) THEN

            MESG = 'File ' // FNAME // ' already opened;  cannot subsequently create it "NEW".'
            CALL M3WARN( 'OPEN3', 0, 0, MESG )
            OPEN3 = .FALSE.
            RETURN

        ELSE IF ( FSTATUS .EQ. FSUNKN3 ) THEN

            IF( RONLY3( FID ) ) THEN

                MESG = 'File ' // FNAME // ' already opened READONLY;  cannot subsequently open it "UNKNOWN'
                CALL M3WARN( 'OPEN3', 0, 0, MESG )
                OPEN3 = .FALSE.
                RETURN

            ELSE 

                OPEN3 = CHKFIL3( FID ) 
                RETURN

            END IF

        ELSE IF ( RONLY3( FID ) .AND. FSTATUS .EQ. FSRDWR3 ) THEN

                MESG = 'File ' // FNAME // ' already opened READONLY;  cannot subsequently open it "READ/WRITE'
                CALL M3WARN( 'OPEN3', 0, 0, MESG )
            OPEN3 = .FALSE.
            RETURN

        ELSE IF ( FSTATUS .EQ. FSCREA3 ) THEN

            IF ( .NOT. CKDESC3( FNAME ) ) THEN

                MESG = 'Error:  bad file description in FDESC'
                CALL M3WARN( 'OPEN3', 0, 0, MESG )
                FLIST3( FID ) = CMISS3
                OPEN3 = .FALSE.
                RETURN

            END IF  !  if CKDESC3() failed

            IF ( CLOSE3( FNAME ) ) THEN

                MESG = 'File ' // FNAME // ' already opened; Closing, deleting, and re-opening it'
                CALL M3WARN( 'OPEN3', 0, 0, MESG )

            ELSE

                MESG = 'File ' // FNAME // ' already opened; Could not close to reopen with status FSCREA3'
                CALL M3WARN( 'OPEN3', 0, 0, MESG )
                OPEN3 = .FALSE.
                RETURN

            END IF

        ELSE

            OPEN3 = .TRUE.
            RETURN

        END IF

    ELSE	!  else file not yet open:  check file desc. for consistency

        IF ( FSTATUS .EQ. FSNEW3   .OR.     &
             FSTATUS .EQ. FSUNKN3  .OR.     &
             FSTATUS .EQ. FSCREA3 ) THEN

            IF ( .NOT. CKDESC3( FNAME ) ) THEN

                MESG = 'Error:   bad file description in FDESC'
                CALL M3WARN( 'OPEN3', 0, 0, MESG )
                OPEN3 = .FALSE.
                RETURN

            END IF

        END IF	!  if status new, unknown, or create/truncate

    END IF  !  if file already open, or not

    FID = INDEX1( CMISS3, MXFILE3, FLIST3 )
    IF ( FID .EQ. 0 ) THEN
        MESG = 'Could not open ' // FNAME // ':  Maximum number of files already opened.'
        CALL M3WARN( 'OPEN3', 0, 0, MESG )
        OPEN3 = .FALSE.
        RETURN
    END IF


    !!.......  Find the value EQNAME for logical name FNAME.
    !!.......  If EQNAME contains "-v" or "-V" mark this file as volatile:

    CALL NAMEVAL( FNAME, EQNAME )

    IDUM = MAX( INDEX( EQNAME, ' -v' ) , INDEX( EQNAME, ' -V' ) )

    IF ( IDUM .GT. 0 ) THEN
        EQNAME( IDUM: ) = '  '              !  fix the '-v' (etc.)
        VOLAT3( FID )   = .TRUE.            !  volatile file
    ELSE
        VOLAT3( FID ) = .FALSE.
    END IF

    FLIST3( FID ) = FNAME
    CDFID3( FID ) = 0
    RONLY3( FID ) = ( FSTATUS .EQ. FSREAD3 )


    !!.......   Open or create new file, according to FSTATUS and existence 
    !!.......   of the file:

    MPIFLAG = .FALSE.         ! "not a PnetCDF/MPI distributed file"
    BNAME   = EQNAME( 1:8 )   ! normalize case for "BUFFERED" files
    CALL UPCASE( BNAME )

    IF ( BNAME .EQ. 'BUFFERED' ) THEN

        IF ( CRTBUF3( FID ) ) THEN  !  sets cdfid3( fid ) = buffil3
            OPEN3  = .TRUE.
            CALL OPNLOG3 ( FID , EQNAME, FSTATUS )
            COUNT3 = MAX( COUNT3, FID )
        ELSE
            FLIST3( FID ) = CMISS3
            OPEN3 = .FALSE.
        END IF

        RETURN

    ELSE IF ( BNAME .EQ. 'VIRTUAL ' .OR. BNAME(1:4) .EQ. 'PVM:' ) THEN

#ifdef  IOAPICPL
       PLIST3(FID) = EQNAME(9:256)
       CDFID3( FID ) = VIRFIL3
       CALL GETARG( 0, PGMNAME )
       OPEN3 = OPEN3V( PLIST3(FID), FSTATUS, PGMNAME )
       IF ( OPEN3 ) THEN
           IF ( UPDTVIR3( FID ) ) THEN
               CALL OPNLOG3 ( FID , EQNAME, FSTATUS )
               COUNT3 = MAX( COUNT3, FID )
           ELSE
               FLIST3( FID ) = CMISS3
               OPEN3 = .FALSE.
           END IF
       ELSE
           FLIST3( FID ) = CMISS3
       END IF
       RETURN
#endif

#ifndef IOAPICPL
        CALL M3WARN( 'OPEN3', 0, 0, 'VIRTUAL files not implemented in this I/O API version' )
        FLIST3( FID ) = CMISS3
        OPEN3 = .FALSE.
        RETURN
#endif

    ELSE IF ( BNAME(1:4) .EQ. 'BIN:' ) THEN

        CDFID3( FID ) = BINFIL3
        BINFLAG = .TRUE.
        MESG    = EQNAME( 5:512 )     !  strip off the "BIN:"
        EQNAME  = MESG
        INQUIRE ( FILE = EQNAME, EXIST = AFLAG )

    ELSE IF ( BNAME(1:4) .EQ. 'MPI:' ) THEN

#ifdef  IOAPI_PNCF
        MPIFLAG  = .TRUE.
        LISTFLAG = .FALSE.
        MESG     = EQNAME( 5:512 )     !  strip off the "BIN:"
        EQNAME   = MESG
        INQUIRE ( FILE = EQNAME, EXIST = AFLAG )
#endif
#ifndef IOAPI_PNCF
        CALL M3WARN( 'OPEN3', 0, 0, 'PMPI files not implemented in this I/O API version' )
        FLIST3( FID ) = CMISS3
        OPEN3 = .FALSE.
        RETURN
#endif

    ELSE IF ( BNAME(1:5) .EQ. 'LIST:' ) THEN

        IF ( FSTATUS .EQ. FSREAD3 ) THEN
            CDFID3( FID ) = LSTFIL3
            LISTFLAG = .TRUE.
            AFLAG    = .FALSE.
        ELSE
            MESG = FNAME // ':' // TRIM( EQNAME )
            CALL M3MSG2( MESG )
            CALL M3WARN( 'OPEN3', 0, 0, 'LIST-File not readonly.' )
            FLIST3( FID ) = CMISS3
            OPEN3 = .FALSE.
            RETURN
        END IF

    ELSE

        INQUIRE ( FILE = EQNAME, EXIST = AFLAG )
        LISTFLAG = .FALSE.

    END IF        !  if buffered, else if virtual, or not

    PRG16  = PGNAME

    IF ( FSTATUS .EQ. FSREAD3 ) THEN        !  read-only

        RWMODE = NF_NOWRITE

        IF ( AFLAG ) THEN           !  file exists:

            IF ( MPIFLAG ) THEN
#ifdef  IOAPI_PNCF
                IF ( .NOT.PN_OPNFIL3( EQNAME, FID, RWMODE, PRG16 ) ) THEN
                    FLIST3( FID ) = CMISS3
                    OPEN3 = .FALSE.
                    RETURN
                END IF
#endif
            ELSE 
                IF ( .NOT.OPNFIL3( EQNAME, FID, RWMODE, PRG16 ) ) THEN
                    FLIST3( FID ) = CMISS3
                    OPEN3 = .FALSE.
                    RETURN
                END IF
            END IF

        ELSE IF ( LISTFLAG ) THEN

            IF ( OPNLIST3( FID, PRG16 ) ) THEN
                OPEN3 = .TRUE.
            ELSE
                FLIST3( FID ) = CMISS3
                OPEN3 = .FALSE.
                RETURN
            END IF

        ELSE                        !  file does not exist

            MESG = FNAME // ':' // TRIM( EQNAME )
            CALL M3MSG2( MESG )
            CALL M3WARN( 'OPEN3', 0, 0, 'File not available.' )
            FLIST3( FID ) = CMISS3
            OPEN3 = .FALSE.
            RETURN

        END IF      !  if file exist or not (file status read-only)

    ELSE IF ( FSTATUS .EQ. FSRDWR3 ) THEN   !  read-write

        IF ( AFLAG ) THEN           !  if file exists:  open it

            RWMODE = NF_WRITE

            IF ( MPIFLAG ) THEN
                IF ( .NOT.PN_OPNFIL3( EQNAME, FID, RWMODE, PRG16 ) ) THEN
                    FLIST3( FID ) = CMISS3
                    OPEN3 = .FALSE.
                    RETURN
                END IF
            ELSE 
                IF ( .NOT.OPNFIL3( EQNAME, FID, RWMODE, PRG16 ) ) THEN
                    FLIST3( FID ) = CMISS3
                    OPEN3 = .FALSE.
                    RETURN
                END IF
            END IF

        ELSE                        !  file does not exist

            MESG = FNAME // ':' // TRIM( EQNAME )
            CALL M3MSG2( MESG )
            CALL M3WARN( 'OPEN3', 0, 0, 'File not available.' )
            FLIST3( FID ) = CMISS3
            OPEN3 = .FALSE.
            RETURN

        END IF      !  if file exists or not (file status read-write)

    ELSE IF ( FSTATUS .EQ. FSNEW3  ) THEN   !  new  (create)

        IF ( AFLAG ) THEN           !  file exists (error)

            CALL M3WARN( 'OPEN3', 0, 0, 'File already exists.' )
            FLIST3( FID ) = CMISS3
            OPEN3 = .FALSE.
            RETURN

        ELSE

            IF ( MPIFLAG ) THEN
                IF ( .NOT.PN_CRTFIL3( EQNAME, FID, PRG16 ) ) THEN
                    FLIST3( FID ) = CMISS3
                    OPEN3 = .FALSE.
                    RETURN
                END IF
            ELSE
                IF ( .NOT.CRTFIL3( EQNAME, FID, PRG16 ) ) THEN
                    FLIST3( FID ) = CMISS3
                    OPEN3 = .FALSE.
                    RETURN
                END IF
            END IF

        END IF     !  if file exists or not (file status create)

    ELSE IF ( FSTATUS .EQ. FSUNKN3 ) THEN   !  unknown

        IF ( AFLAG ) THEN   !  file exists:  open and check consistency

            RWMODE = NF_WRITE

            IF ( MPIFLAG ) THEN
                IF ( .NOT.PN_OPNFIL3( EQNAME, FID, RWMODE, PRG16 ) ) THEN
                    OPEN3 = .FALSE.
                    RETURN
                END IF
            ELSE 
                IF ( .NOT.OPNFIL3( EQNAME, FID, RWMODE, PRG16 ) ) THEN
                    OPEN3 = .FALSE.
                    RETURN
                END IF
            END IF

            IF ( .NOT. CHKFIL3( FID ) ) THEN  !  consistency check

                FLIST3( FID ) = CMISS3
                OPEN3 = .FALSE.
                IF ( MPIFLAG ) THEN
#ifdef  IOAPI_PNCF
                    IERR = NFMPI_CLOSE( CDFID3( FID ) )
#endif
                ELSE
                    IERR = NF_CLOSE( CDFID3( FID ) )
                END IF
                RETURN 

            END IF          !  if OPNFIL3() succeeded or not

        ELSE

            IF ( MPIFLAG ) THEN
                IF ( .NOT.PN_CRTFIL3( EQNAME, FID, PRG16 ) ) THEN
                    OPEN3 = .FALSE.
                    RETURN
                END IF
            ELSE
                IF ( .NOT.CRTFIL3( EQNAME, FID, PRG16 ) ) THEN
                    OPEN3 = .FALSE.
                    RETURN
                END IF
            END IF

        END IF      !  if file exists or can be created or not

    ELSE IF ( FSTATUS .EQ. FSCREA3 ) THEN   !  truncate and create new

        IF ( AFLAG ) THEN   !  file exists:  remove it first.

            IERR = RMFILE( EQNAME )
            IF ( IERR .NE. 0 ) THEN
                WRITE ( MESG, '( A, I9, 2X, 3A  )' ) 'Error number', IERR, 'removing ', FNAME, ' from pathname:'
                 CALL M3WARN( 'OPEN3', 0, 0, MESG )
                 CALL M3MSG2( EQNAME )
                OPEN3 = .FALSE.
                RETURN
            END IF	!  if RMFILE failed

        END IF  !  if file exists

        IF ( MPIFLAG ) THEN
            IF ( .NOT.PN_CRTFIL3( EQNAME, FID, PRG16 ) ) THEN
                OPEN3 = .FALSE.
                RETURN
            END IF
        ELSE
            IF ( .NOT.CRTFIL3( EQNAME, FID, PRG16 ) ) THEN
                OPEN3 = .FALSE.
                RETURN
            END IF
        END IF

    ELSE            !  illegal FSTATUS value:

        CALL M3WARN( 'OPEN3', 0, 0, 'File opening error:  illegal FSTATUS argument.' )
        CALL M3MSG2( 'Legal: 1-READONLY, 2-READ/WRITE, 3-NEW, 4-UNKNOWN')
         WRITE ( MESG, '( 5 ( A, :, I9, :, 2X ) )' ) 'Value supplied by caller:', FSTATUS
        CALL M3MSG2( MESG )

        FLIST3( FID ) = CMISS3
        OPEN3 = .FALSE.
        RETURN

    END IF  !  read-only, read-write, new, unknown, create/trunc, or illegal


    !!.......   Update COUNT3 to account for new file, and log it:

    COUNT3 = MAX( COUNT3, FID )
    CALL OPNLOG3( FID , EQNAME, FSTATUS )

    OPEN3 = .TRUE.

    RETURN

END FUNCTION  OPEN3

