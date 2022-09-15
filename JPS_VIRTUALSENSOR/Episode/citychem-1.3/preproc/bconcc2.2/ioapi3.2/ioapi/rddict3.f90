
LOGICAL FUNCTION RDDICT3( FID, FNAME ) RESULT( RDFLAG )

    !!***********************************************************************
    !! Version "$Id: rddict3.f90 1 2017-06-10 18:05:20Z coats $"
    !! EDSS/Models-3 I/O API.
    !! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    !! (C) 2003-2013 Baron Advanced Meteorological Systems,
    !! (C) 2007-2013 Carlie J. Coats, Jr., and
    !! (C) 2014-2015 UNC Institute for the Environment.
    !! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    !! See file "LGPL.txt" for conditions of use.
    !!.........................................................................
    !!  function body starts at line  78
    !!
    !!  FUNCTION:  
    !!       read the file definition for specified description-name FMAME
    !!       from the dictionary file with logical name FLIST3( FID ) into
    !!       the commons in include file FDESC3.EXT
    !!
    !!  RETURN 
    !!       VALUE:  TRUE iff the operation succeeds (and the data is available)
    !!
    !!  PRECONDITIONS REQUIRED:  
    !!       Called from READ3() on a file already open.
    !!
    !!  SUBROUTINES AND FUNCTIONS CALLED:  netCDF, INDEX1
    !!
    !!  REVISION  HISTORY:  prototype 3/92 by CJC
    !!      Modified 02/2015 by CJC for I/O API 3.2: USE M3UTILIO
    !!
    !!      Modified 08/2015 by CJC: USE MODNCFIO
    !!
    !!      Modified 10/2015 by CJC for I/O API 3.2: use NF_*() instead of NC*()
    !!      for netCDF-Fortran 4.x compatibility; F90 "free" source format.
    !!***********************************************************************

    USE MODNCFIO
    USE M3UTILIO

    IMPLICIT NONE

    !!...........   INCLUDES:

    INCLUDE 'STATE3.EXT'


    !!...........   ARGUMENTS, PARAMETERs, and their descriptions:

    INTEGER      , INTENT(IN   ) :: FID   !  index for file
    CHARACTER*(*), INTENT(IN   ) :: FNAME !  name of requested file description

    CHARACTER*80, PARAMETER :: WARN = '>>--->> WARNING in subroutine RDDICT3'

    !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         VID             !  index (record) number within FNAMES
    INTEGER         IDUM            !  Scratch variable
    INTEGER         FNUM            !  netCDF file ID from NF_CREATE()
    INTEGER         IERR            !  netCDF error status return
    INTEGER         FLAG            !  index-flag from dictionary
    INTEGER         DIMS( 6 )       !  array of dims for NF_GET_VARA_*()
    INTEGER         DELS( 6 )       !  array of edge deltas for NF_GET_VARA_*()
    LOGICAL         EFLAG
    CHARACTER*16    ENAME

        
    !!.......   State variable:  names table, file ID for last call

    CHARACTER*16    FNAMES( MXVARS3 )
    INTEGER, SAVE :: LID = -1

    !!.............................................................................
    !!   begin body of subroutine  RDDICT3

    !!.......   Get dictionary ID

    FNUM = CDFID3( FID )

    IF ( FTYPE3( FID ) .NE. DCTNRY3 ) THEN
        RDFLAG = .FALSE.
        RETURN
    END IF

    EFLAG = .FALSE.
    ENAME = FNAME
    IDUM  = MIN( MXVARS3, MXREC3( FID ) )
    FNUM  = CDFID3( FID )

!$OMP CRITICAL( S_LOGOUT )
!$OMP CRITICAL( S_NC )

    !!.......   Look up the "variable" requested from the FNAMES table:
        
    IF ( FID .NE. LID ) THEN

        DIMS( 1 ) = 1
        DELS( 1 ) = NAMLEN3
        DIMS( 2 ) = 1
        DELS( 2 ) = IDUM
        IERR = NF_GET_VARA_TEXT( FNUM, VINDX3( 1,FID ), DIMS, DELS, FNAMES )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV, '( //5X, A, 3( /, 5X, A, :, I6 ) )' )                    &
                WARN, 'netCDF error number', IERR,                                  &
                'Dictionary file:  ' // TRIM( FNAME ) // '; entry requested:', FID, &
                'Error reading netCDF dictionary variable VDESC.'
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

        LID = FID
        
    END IF          !  if new FID

    VID = INDEX1( FNAME, IDUM, FNAMES )

    IERR = NF_GET_VAR1_INT( FNUM, TINDX3( FID ), VID, FLAG )
    IF ( IERR .EQ. 8 ) THEN
        EFLAG = .TRUE.
        GO TO 999
    ELSE IF ( IERR .NE. 0 ) THEN
        CALL LOGERR( FNAME, ENAME, 'Record-FLAG', IERR )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed

    IF ( FLAG .NE. OKFLAG3 .AND. FLAG .NE. VID ) THEN
        WRITE( LOGDEV, '( //5X, 2 A, /5X, 2 A )' )          &
                'Dictionary entry requested:', FNAME,       &
                'Record not available in dictionary file ', FLIST3( FID )
        EFLAG = .TRUE.
        GO TO 999
    END IF  


    !!.......   Read characteristics from this record:


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

    IERR = NF_GET_VARA_INT( FNUM, VINDX3( 2,FID ), DIMS, DELS, FTYPE3D )
    IF ( IERR .NE. 0 ) THEN
        CALL LOGERR( FNAME, ENAME, 'FTYPE', IERR )
        EFLAG = .TRUE.
        GO TO 999
    END IF

    IERR = NF_GET_VARA_INT( FNUM, VINDX3( 3,FID ), DIMS, DELS, TSTEP3D )
    IF ( IERR .NE. 0 ) THEN
        CALL LOGERR( FNAME, ENAME, 'TSTEP', IERR )
        EFLAG = .TRUE.
        GO TO 999
    END IF

    IERR = NF_GET_VARA_INT( FNUM, VINDX3( 4,FID ), DIMS, DELS, DIMS, NVARS3D )
    IF ( IERR .NE. 0 ) THEN
        CALL LOGERR( FNAME, ENAME, 'NVARS', IERR )
        EFLAG = .TRUE.
        GO TO 999
    END IF

    IERR = NF_GET_VARA_INT( FNUM, VINDX3( 5,FID ), DIMS, DELS, NLAYS3D )
    IF ( IERR .NE. 0 ) THEN
        CALL LOGERR( FNAME, ENAME, 'NLAYS', IERR )
        EFLAG = .TRUE.
        GO TO 999
    END IF

    IERR = NF_GET_VARA_INT( FNUM, VINDX3( 6,FID ), DIMS, DELS, NROWS3D )
    IF ( IERR .NE. 0 ) THEN
        CALL LOGERR( FNAME, ENAME, 'NROWS', IERR )
        EFLAG = .TRUE.
        GO TO 999
    END IF

    IERR = NF_GET_VARA_INT( FNUM, VINDX3( 7,FID ), DIMS, DELS, DIMS, NCOLS3D )
    IF ( IERR .NE. 0 ) THEN
        CALL LOGERR( FNAME, ENAME, 'NCOLS', IERR )
        EFLAG = .TRUE.
        GO TO 999
    END IF

    IERR = NF_GET_VARA_INT( FNUM, VINDX3( 8,FID ), DIMS, DELS, NTHIK3D )
    IF ( IERR .NE. 0 ) THEN
        CALL LOGERR( FNAME, ENAME, 'NTHIK', IERR )
        EFLAG = .TRUE.
        GO TO 999
    END IF

    IERR = NF_GET_VARA_INT( FNUM, VINDX3( 9,FID ), DIMS, DELS, GDTYP3D )
    IF ( IERR .NE. 0 ) THEN
        CALL LOGERR( FNAME, ENAME, 'GDTYP', IERR )
        EFLAG = .TRUE.
        GO TO 999
    END IF

    IERR = NF_GET_VARA_INT( FNUM, VINDX3( 10,FID ), DIMS, DELS, VGTYP3D )
    IF ( IERR .NE. 0 ) THEN
        CALL LOGERR( FNAME, ENAME, 'VGTYP', IERR )
        EFLAG = .TRUE.
        GO TO 999
    END IF

    DIMS( 1 ) = 1
    DIMS( 2 ) = VID
    DELS( 1 ) = NLAYS3D + 1
    DELS( 2 ) = 1

    IERR = NF_GET_VARA_REAL( FNUM, VINDX3( 11,FID ), DIMS, DELS, VGLVS3D )
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

    IERR = NF_GET_VARA_DOUBLE( FNUM, VINDX3( 12,FID ), DIMS, DELS, P_ALP3D )
    IF ( IERR .NE. 0 ) THEN
        CALL LOGERR( FNAME, ENAME, 'P_ALP', IERR )
        EFLAG = .TRUE.
        GO TO 999
    END IF

    IERR = NF_GET_VARA_DOUBLE( FNUM, VINDX3( 13,FID ), DIMS, DELS, P_BET3D )
    IF ( IERR .NE. 0 ) THEN
        CALL LOGERR( FNAME, ENAME, 'P_BET', IERR )
        EFLAG = .TRUE.
        GO TO 999
    END IF

    IERR = NF_GET_VARA_DOUBLE( FNUM, VINDX3( 14,FID ), DIMS, DELS, P_GAM3D )
    IF ( IERR .NE. 0 ) THEN
        CALL LOGERR( FNAME, ENAME, 'P_GAM', IERR )
        EFLAG = .TRUE.
        GO TO 999
    END IF

    IERR = NF_GET_VARA_DOUBLE( FNUM, VINDX3( 15,FID ), DIMS, DELS, XCENT3D )
    IF ( IERR .NE. 0 ) THEN
        CALL LOGERR( FNAME, ENAME, 'XCENT', IERR )
        EFLAG = .TRUE.
        GO TO 999
    END IF

    IERR = NF_GET_VARA_DOUBLE( FNUM, VINDX3( 16,FID ), DIMS, DELS, YCENT3D )
    IF ( IERR .NE. 0 ) THEN
        CALL LOGERR( FNAME, ENAME, 'YCENT', IERR )
        EFLAG = .TRUE.
        GO TO 999
    END IF

    IERR = NF_GET_VARA_DOUBLE( FNUM, VINDX3( 17,FID ), DIMS, DELS, XORIG3D )
    IF ( IERR .NE. 0 ) THEN
        CALL LOGERR( FNAME, ENAME, 'XORIG', IERR )
        EFLAG = .TRUE.
        GO TO 999
    END IF

    IERR = NF_GET_VARA_DOUBLE( FNUM, VINDX3( 18,FID ), DIMS, DELS, YORIG3D )
    IF ( IERR .NE. 0 ) THEN
        CALL LOGERR( FNAME, ENAME, 'YORIG', IERR )
        EFLAG = .TRUE.
        GO TO 999
    END IF

    IERR = NF_GET_VARA_DOUBLE( FNUM, VINDX3( 19,FID ), DIMS, DELS, XCELL3D )
    IF ( IERR .NE. 0 ) THEN
        CALL LOGERR( FNAME, ENAME, 'XCELL', IERR )
        EFLAG = .TRUE.
        GO TO 999
    END IF

    IERR = NF_GET_VARA_DOUBLE( FNUM, VINDX3( 20,FID ), DIMS, DELS, YCELL3D )
    IF ( IERR .NE. 0 ) THEN
        CALL LOGERR( FNAME, ENAME, 'YCELL', IERR )
        EFLAG = .TRUE.
        GO TO 999
    END IF



    DIMS( 1 ) = 1
    DIMS( 2 ) = VID
    DELS( 1 ) = NAMLEN3
    DELS( 2 ) = 1

    IERR = NF_GET_VARA_TEXT( FNUM, VINDX3( 21,FID ), DIMS, DELS, GDNAM3D )
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

    IERR = NF_GET_VARA_TEXT( FNUM, VINDX3( 22,FID ), DIMS, DELS, FDESC3D )
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

    IERR = NF_GET_VARA_TEXT( FNUM, VINDX3( 23,FID ), DIMS, DELS, VNAME3D )
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

    IERR = NF_GET_VARA_TEXT( FNUM, VINDX3( 24,FID ), DIMS, DELS, UNITS3D )
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

    IERR = NF_GET_VARA_TEXT( FNUM, VINDX3( 25,FID ), DIMS, DELS, VDESC3D )
    IF ( IERR .NE. 0 ) THEN
        CALL LOGERR( FNAME, ENAME, 'VDESC', IERR )
        EFLAG = .TRUE.
        GO TO 999
    END IF

    DIMS( 1 ) = 1
    DIMS( 2 ) = VID
    DELS( 1 ) = NVARS3D
    DELS( 2 ) = 1

    IERR = NF_GET_VARA_INT( FNUM, VINDX3( 26,FID ), DIMS, DELS, VTYPE3D )
    IF ( IERR .NE. 0 ) THEN
        CALL LOGERR( FNAME, ENAME, 'VTYPE', IERR )
        EFLAG = .TRUE.
        GO TO 999
    END IF

999 CONTINUE

!$OMP END CRITICAL( S_NC )
!$OMP END CRITICAL( S_LOGOUT )


    RDFLAG = .TRUE.
    RETURN


CONTAINS    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE LOGERR( FNAME, DNAME, VNAME, IERR )

        CHARACTER(LEN=*), INTENT( IN ) :: FNAME, DNAME, VNAME
        INTEGER         , INTENT( IN ) :: IERR

        WRITE( LOGDEV , '( /, 4( /5X , A ) , I5, // )' )                    &
           WARN,                                                            &
           'Error reading netCDF dictionary variable ' // TRIM( VNAME ) //  &
           'in file:  ' // TRIM( FNAME ),                                   &
           'Dictionary entry ' // TRIM( DNAME ),                            &
           'netCDF error number', IERR

        RETURN

    END SUBROUTINE LOGERR

END FUNCTION RDDICT3

