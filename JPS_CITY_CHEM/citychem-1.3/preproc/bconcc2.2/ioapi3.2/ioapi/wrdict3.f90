
LOGICAL FUNCTION WRDICT3 ( FID, FNAME ) RESULT( WFLAG )

    !!***********************************************************************
    !! Version "$Id: wrdict3.f90 1 2017-06-10 18:05:20Z coats $"
    !! EDSS/Models-3 I/O API.
    !! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    !! (C) 2003-2013 Baron Advanced Meteorological Systems,
    !! (C) 2007-2013 Carlie J. Coats, Jr., and
    !! (C) 2014-2015 UNC Institute for the Environment.
    !! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    !! See file "LGPL.txt" for conditions of use.
    !!.........................................................................
    !!  function body starts at line 90
    !!
    !!  FUNCTION:  write the file definition stored in the commons of the
    !!             include-file FDESC3.EXT to logical record indexed by
    !!             name FNAME, of the dictionary file with logical name DNAME.
    !!             Logs warning message iff record for FNAME already exists.
    !!
    !!  RETURN VALUE:  TRUE iff the operation succeeds.
    !!
    !!
    !!  PRECONDITIONS REQUIRED:
    !!       FID is STATE3 subscript for a dictionary file already opened
    !!       for write by OPEN3();  >= 1.
    !!
    !!  SUBROUTINES AND FUNCTIONS CALLED:
    !!
    !!  REVISION  HISTORY:
    !!      prototype 3/1992 by CJC
    !!
    !!      Modified 11/1994 by CJC to use numeric FID argument for dictionary,
    !!      logical name argument FNAME for dictionary-entry identifier.
    !!
    !!      revised  6/1999 by CJC:  OpenMP thread-safety for log-file
    !!
    !!      revised  2/2002 by CJC:  OpenMP thread-safety for netCDF
    !!
    !!      Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !!
    !!      Modified 02-08/2015 by CJC for I/O API 3.2: USE M3UTILIO, MODNCFIO
    !!
    !!      Modified 10/2015 by CJC for I/O API 3.2: use NF_*() instead of NC*()
    !!      for netCDF-Fortran 4.x compatibility; F90 free source format. More F9x.
    !!***********************************************************************

    USE M3UTILIO
    USE MODNCFIO

    IMPLICIT NONE

    !!...........   INCLUDES:

    INCLUDE 'STATE3.EXT'


    !!...........   ARGUMENTS and their descriptions:
    
    INTEGER      , INTENT(IN   ) :: FID     !  output-file subscript for STATE3 arrays
    CHARACTER*(*), INTENT(IN   ) :: FNAME   !  name of the data schema to be written


    !!...........   EXTERNAL FUNCTIONS, PARAMETERs, and their descriptions:

    EXTERNAL :: INITBLK3        !!  BLOCK DATA to initialize STATE3 commons
    
    CHARACTER*72, PARAMETER :: WARN = '>>> WARNING in subroutine WRDICT3 <<<'


    !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         VID             !  record number to be written.
    INTEGER         IDUM            !  holds return value for INIT3()
    INTEGER         FLAG            !
    INTEGER         FNUM            !  netCDF file ID from NF_CREATE()
    INTEGER         IERR            !  netCDF error status return
    INTEGER         DIMS( 5 )       !  array of dims for NF_PUT_VARA*()
    INTEGER         DELS( 5 )       !  array of edge deltas for NF_PUT_VARA*()
    LOGICAL         EFLAG
    CHARACTER*16    ENAME

    !!...........   STATE VARIABLE:  names table, file ID for last call

    CHARACTER*16, SAVE :: ENAMES( MXVARS3 )
    INTEGER,      SAVE :: LID = -1

    !!.............................................................................
    !!   begin body of subroutine  WRDICT3

    !!.......   Check whether file description FNAME already exists
    !!.......   (if so, write warning message)
    !!.......   Look up the "variable" requested from the ENAMES table:

    EFLAG = .FALSE.
    ENAME =  FNAME

!$OMP CRITICAL( S_LOGOUT )
!$OMP CRITICAL( S_NC )
    IDUM = MIN( MXVARS3, MXREC3( FID ) )
    FNUM = CDFID3( FID )

    IF( FID .NE. LID ) THEN

        DIMS( 1 ) = 1
        DELS( 1 ) = NAMLEN3
        DIMS( 2 ) = 1
        DELS( 2 ) = IDUM
        IERR = NF_PUT_VARA_TEXT( FNUM, VINDX3( 1,FID ), DIMS, DELS, ENAMES )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV, '( //5X, A, 3( /, 5X, A, I6, : ) )' )                    &
                WARN, 'netCDF error number', IERR,                                  &
                'Dictionary file:  ' // TRIM( FNAME ) // '; entry requested:', FID, &
                'Error reading netCDF dictionary variable VDESC.'
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

        LID = FID

    END IF          !  fid, lid differ

    VID = INDEX1( ENAME, IDUM, ENAMES )

    IF ( VID .GT. 0 ) THEN
            CALL M3WARN( ' ', 0, 0, 'File description ' // ENAME // ' being overwritten' )
    ELSE
        VID = IDUM + 1
        IF ( VID .GT. MXVARS3 ) THEN
            WRITE( LOGDEV, '( /, 4( /, 5X, A ) )' ) WARN,                               &
                'Dictionary file:  ' // TRIM( FNAME ) // '; entry requested:' // ENAME, &
                'Maximum record number exceeded'
            EFLAG = .TRUE.
            GO TO 999
        END IF
        ENAMES( VID ) = FLIST3( FID )
    END IF

    !!.......   Write characteristics to this record:

    DIMS( 1 ) = 1
    DIMS( 2 ) = VID
    DELS( 1 ) = NAMLEN3
    DELS( 2 ) = 1
    IERR = NF_PUT_VARA_TEXT( FNUM, VINDX3( 1,FID ), DIMS, DELS, FNAME )
    IF ( IERR .NE. 0 ) THEN
        CALL LOGERR( FNAME, ENAME, 'FNAME', IERR )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed


    DIMS( 1 ) = VID
    DIMS( 2 ) = 1
    DIMS( 3 ) = 1
    DIMS( 4 ) = 1
    DIMS( 5 ) = 1
    DELS( 1 ) = 1
    DELS( 2 ) = 1
    DELS( 3 ) = 1
    DELS( 4 ) = 1
    DELS( 5 ) = 1

    IERR = NF_PUT_VARA_INT( FNUM, VINDX3( 2,FID ), DIMS, DELS, FTYPE3D )
    IF ( IERR .NE. 0 ) THEN
        CALL LOGERR( FNAME, ENAME, 'FTYPE', IERR )
        EFLAG = .TRUE.
        GO TO 999
    END IF

    IERR = NF_PUT_VARA_INT( FNUM, VINDX3( 3,FID ), DIMS, DELS, TSTEP3D )
    IF ( IERR .NE. 0 ) THEN
        CALL LOGERR( FNAME, ENAME, 'TSTEP', IERR )
        EFLAG = .TRUE.
        GO TO 999
    END IF

    IERR = NF_PUT_VARA_INT( FNUM, VINDX3( 4,FID ), DIMS, DELS, DIMS, NVARS3D )
    IF ( IERR .NE. 0 ) THEN
        CALL LOGERR( FNAME, ENAME, 'NVARS', IERR )
        EFLAG = .TRUE.
        GO TO 999
    END IF

    IERR = NF_PUT_VARA_INT( FNUM, VINDX3( 5,FID ), DIMS, DELS, NLAYS3D )
    IF ( IERR .NE. 0 ) THEN
        CALL LOGERR( FNAME, ENAME, 'NLAYS', IERR )
        EFLAG = .TRUE.
        GO TO 999
    END IF

    IERR = NF_PUT_VARA_INT( FNUM, VINDX3( 6,FID ), DIMS, DELS, NROWS3D )
    IF ( IERR .NE. 0 ) THEN
        CALL LOGERR( FNAME, ENAME, 'NROWS', IERR )
        EFLAG = .TRUE.
        GO TO 999
    END IF

    IERR = NF_PUT_VARA_INT( FNUM, VINDX3( 7,FID ), DIMS, DELS, DIMS, NCOLS3D )
    IF ( IERR .NE. 0 ) THEN
        CALL LOGERR( FNAME, ENAME, 'NCOLS', IERR )
        EFLAG = .TRUE.
        GO TO 999
    END IF

    IERR = NF_PUT_VARA_INT( FNUM, VINDX3( 8,FID ), DIMS, DELS, NTHIK3D )
    IF ( IERR .NE. 0 ) THEN
        CALL LOGERR( FNAME, ENAME, 'NTHIK', IERR )
        EFLAG = .TRUE.
        GO TO 999
    END IF

    IERR = NF_PUT_VARA_INT( FNUM, VINDX3( 9,FID ), DIMS, DELS, GDTYP3D )
    IF ( IERR .NE. 0 ) THEN
        CALL LOGERR( FNAME, ENAME, 'GDTYP', IERR )
        EFLAG = .TRUE.
        GO TO 999
    END IF

    IERR = NF_PUT_VARA_INT( FNUM, VINDX3( 10,FID ), DIMS, DELS, VGTYP3D )
    IF ( IERR .NE. 0 ) THEN
        CALL LOGERR( FNAME, ENAME, 'VGTYP', IERR )
        EFLAG = .TRUE.
        GO TO 999
    END IF

    DIMS( 1 ) = 1
    DIMS( 2 ) = VID
    DELS( 1 ) = NLAYS3D + 1
    DELS( 2 ) = 1

    IERR = NF_PUT_VARA_REAL( FNUM, VINDX3( 11,FID ), DIMS, DELS, VGLVS3D )
    IF ( IERR .NE. 0 ) THEN
        CALL LOGERR( FNAME, ENAME, 'VGLVS', IERR )
        EFLAG = .TRUE.
        GO TO 999
    END IF

    DIMS( 1 ) = VID
    DIMS( 2 ) = 1
    DIMS( 3 ) = 1
    DIMS( 4 ) = 1
    DIMS( 5 ) = 1
    DELS( 1 ) = 1
    DELS( 2 ) = 1
    DELS( 3 ) = 1
    DELS( 4 ) = 1
    DELS( 5 ) = 1

    IERR = NF_PUT_VARA_DOUBLE( FNUM, VINDX3( 12,FID ), DIMS, DELS, P_ALP3D )
    IF ( IERR .NE. 0 ) THEN
        CALL LOGERR( FNAME, ENAME, 'P_ALP', IERR )
        EFLAG = .TRUE.
        GO TO 999
    END IF

    IERR = NF_PUT_VARA_DOUBLE( FNUM, VINDX3( 13,FID ), DIMS, DELS, P_BET3D )
    IF ( IERR .NE. 0 ) THEN
        CALL LOGERR( FNAME, ENAME, 'P_BET', IERR )
        EFLAG = .TRUE.
        GO TO 999
    END IF

    IERR = NF_PUT_VARA_DOUBLE( FNUM, VINDX3( 14,FID ), DIMS, DELS, P_GAM3D )
    IF ( IERR .NE. 0 ) THEN
        CALL LOGERR( FNAME, ENAME, 'P_GAM', IERR )
        EFLAG = .TRUE.
        GO TO 999
    END IF

    IERR = NF_PUT_VARA_DOUBLE( FNUM, VINDX3( 15,FID ), DIMS, DELS, XCENT3D )
    IF ( IERR .NE. 0 ) THEN
        CALL LOGERR( FNAME, ENAME, 'XCENT', IERR )
        EFLAG = .TRUE.
        GO TO 999
    END IF

    IERR = NF_PUT_VARA_DOUBLE( FNUM, VINDX3( 16,FID ), DIMS, DELS, YCENT3D )
    IF ( IERR .NE. 0 ) THEN
        CALL LOGERR( FNAME, ENAME, 'YCENT', IERR )
        EFLAG = .TRUE.
        GO TO 999
    END IF

    IERR = NF_PUT_VARA_DOUBLE( FNUM, VINDX3( 17,FID ), DIMS, DELS, XORIG3D )
    IF ( IERR .NE. 0 ) THEN
        CALL LOGERR( FNAME, ENAME, 'XORIG', IERR )
        EFLAG = .TRUE.
        GO TO 999
    END IF

    IERR = NF_PUT_VARA_DOUBLE( FNUM, VINDX3( 18,FID ), DIMS, DELS, YORIG3D )
    IF ( IERR .NE. 0 ) THEN
        CALL LOGERR( FNAME, ENAME, 'YORIG', IERR )
        EFLAG = .TRUE.
        GO TO 999
    END IF

    IERR = NF_PUT_VARA_DOUBLE( FNUM, VINDX3( 19,FID ), DIMS, DELS, XCELL3D )
    IF ( IERR .NE. 0 ) THEN
        CALL LOGERR( FNAME, ENAME, 'XCELL', IERR )
        EFLAG = .TRUE.
        GO TO 999
    END IF

    IERR = NF_PUT_VARA_DOUBLE( FNUM, VINDX3( 20,FID ), DIMS, DELS, YCELL3D )
    IF ( IERR .NE. 0 ) THEN
        CALL LOGERR( FNAME, ENAME, 'YCELL', IERR )
        EFLAG = .TRUE.
        GO TO 999
    END IF



    DIMS( 1 ) = 1
    DIMS( 2 ) = VID
    DELS( 1 ) = NAMLEN3
    DELS( 2 ) = 1

    IERR = NF_PUT_VARA_TEXT( FNUM, VINDX3( 21,FID ), DIMS, DELS, GDNAM3D )
    IF ( IERR .NE. 0 ) THEN
        CALL LOGERR( FNAME, ENAME, 'GDNAM', IERR )
        EFLAG = .TRUE.
        GO TO 999
    END IF

    DIMS( 1 ) = 1
    DIMS( 2 ) = 1
    DIMS( 3 ) = VID
    DELS( 1 ) = MXDLEN3
    DELS( 2 ) = MXDESC3
    DELS( 3 ) = 1

    IERR = NF_PUT_VARA_TEXT( FNUM, VINDX3( 22,FID ), DIMS, DELS, FDESC3D )
    IF ( IERR .NE. 0 ) THEN
        CALL LOGERR( FNAME, ENAME, 'FILEDESC', IERR )
        EFLAG = .TRUE.
        GO TO 999
    END IF

    DIMS( 1 ) = 1
    DIMS( 2 ) = 1
    DIMS( 3 ) = VID
    DELS( 1 ) = NAMLEN3
    DELS( 2 ) = NVARS3D
    DELS( 3 ) = 1

    IERR = NF_PUT_VARA_TEXT( FNUM, VINDX3( 23,FID ), DIMS, DELS, VNAME3D )
    IF ( IERR .NE. 0 ) THEN
        CALL LOGERR( FNAME, ENAME, 'FILEDESC', IERR )
        EFLAG = .TRUE.
        GO TO 999
    END IF

    DIMS( 1 ) = 1
    DIMS( 2 ) = 1
    DIMS( 3 ) = VID
    DELS( 1 ) = NAMLEN3
    DELS( 2 ) = NVARS3D
    DELS( 3 ) = 1

    IERR = NF_PUT_VARA_TEXT( FNUM, VINDX3( 24,FID ), DIMS, DELS, UNITS3D )
    IF ( IERR .NE. 0 ) THEN
        CALL LOGERR( FNAME, ENAME, 'UNITS', IERR )
        EFLAG = .TRUE.
        GO TO 999
    END IF


    DIMS( 1 ) = 1
    DIMS( 2 ) = 1
    DIMS( 3 ) = VID
    DELS( 1 ) = MXDLEN3
    DELS( 2 ) = NVARS3D
    DELS( 3 ) = 1

    IERR = NF_PUT_VARA_TEXT( FNUM, VINDX3( 25,FID ), DIMS, DELS, VDESC3D )
    IF ( IERR .NE. 0 ) THEN
        CALL LOGERR( FNAME, ENAME, 'VDESC', IERR )
        EFLAG = .TRUE.
        GO TO 999
    END IF

    DIMS( 1 ) = 1
    DIMS( 2 ) = VID
    DELS( 1 ) = NVARS3D
    DELS( 2 ) = 1

    IERR = NF_PUT_VARA_INT( FNUM, VINDX3( 26,FID ), DIMS, DELS, VTYPE3D )
    IF ( IERR .NE. 0 ) THEN
        CALL LOGERR( FNAME, ENAME, 'VTYPE', IERR )
        EFLAG = .TRUE.
        GO TO 999
    END IF


    !!.......   Write record-flag value:

    FLAG = VID

    IERR = NF_PUT_VAR1_INT( FNUM, TINDX3( FID ), VID, FLAG )
    IF ( IERR .NE. 0 ) THEN
        CALL LOGERR( FNAME, ENAME, 'RECORD-FLAG', IERR )
        EFLAG = .TRUE.
        GO TO 999
    END IF

999 CONTINUE

!$OMP END CRITICAL( S_NC )
!$OMP END CRITICAL( S_LOGOUT )

    IF  ( EFLAG ) THEN
        WFLAG = .FALSE.
        RETURN
    END IF

    MXREC3( FID ) = MAX( MXREC3( FID ), VID )

    WFLAG = .TRUE.
    RETURN

CONTAINS    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE LOGERR( FNAME, DNAME, VNAME, IERR )

        CHARACTER(LEN=*), INTENT( IN ) :: FNAME, DNAME, VNAME
        INTEGER         , INTENT( IN ) :: IERR
        
        CHARACTER*256   MESG
        
        MESG =  'Error writing netCDF dictionary variable ' // TRIM( VNAME ) // 'in file:  ' // TRIM( FNAME )

        WRITE( LOGDEV , '( /, 4( /, 5X , A ) , I5, // )' )  &
            WARN,                                           &
            MESG,                                           &
           'Dictionary entry ' // TRIM( DNAME ),            &
           'netCDF error number', IERR

        RETURN

    END SUBROUTINE LOGERR

END FUNCTION WRDICT3

