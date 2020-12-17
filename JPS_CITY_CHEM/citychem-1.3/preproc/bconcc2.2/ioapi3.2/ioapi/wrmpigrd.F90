
LOGICAL FUNCTION WRMPIGRD( FID, VID, TSTAMP, STEP2, BUFFER )

    !!***********************************************************************
    !! Version "$Id: wrmpigrd.F90 1 2017-06-10 18:05:20Z coats $"
    !! EDSS/Models-3 I/O API.
    !! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    !! (C) 2003-2010 Baron Advanced Meteorological Systems
    !! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    !! See file "LGPL.txt" for conditions of use.
    !!.........................................................................
    !!  function body starts at line  79
    !!
    !!  FUNCTION:  writes data from Models-3 GRIDDED data file with STATE3
    !!             index FID, for all variables and layers, for the time step
    !!             record STEP.
    !!
    !!  RETURN VALUE:  TRUE iff the operation succeeds
    !!
    !!  PRECONDITIONS REQUIRED:  Should only be called by WRITE3(), after it
    !!             has checked that file and time step are available, and that
    !!             file type is GRDDED3.
    !!
    !!  SUBROUTINES AND FUNCTIONS CALLED:  WRVARS
    !!
    !!  REVISION  HISTORY:
    !!      Adapted  08/2010 by CJC I/O from "ioapi/wrvars.F" for MPIGRD3 files
    !!      using MPI/PnetCDF distributed I/O
    !!
    !!      Version 12/2015 by CJC, responding to some issues from D. Wong.
    !!***********************************************************************

#ifdef IOAPI_PNCF

    USE MODNCFIO
    USE MODPDATA

    IMPLICIT NONE

    !!...........   INCLUDES:

    INCLUDE 'PARMS3.EXT'
    INCLUDE 'STATE3.EXT'
    INCLUDE 'mpif.h'

    !!...........   ARGUMENTS and their descriptions:

    INTEGER, INTENT(IN   ) :: FID             !  file index within the STATE3 commons
    INTEGER, INTENT(IN   ) :: VID             !  vble index within the STATE3 commons
    INTEGER, INTENT(IN   ) :: TSTAMP( 2 )     !  ( jdate yyyyddd, jtime hhmmss )
    INTEGER, INTENT(IN   ) :: STEP2           !  file record number (maybe mod 2)
    REAL   , INTENT(IN   ) :: BUFFER(*)       !  buffer array for input

    !!...........   External Functions:  type-specific MPI/PnetCDF variants on WRVARS()

    LOGICAL, EXTERNAL :: PN_WRINT, PN_WRREAL, PN_WRDBLE, PN_WRINT8

    !!...........   PARAMETER:

    INTEGER, PARAMETER :: TYPSIZE( 10 ) = (/ 1, 1, 1, 1, 1, 2, 1, 1, 1, 2 /)


    !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         DELTA           !  d(INDX) / d(NCVGTcall)
    INTEGER         NLAYS, V,  PE,  IERR
    INTEGER         VTYPE, VINDX, INDX, SIZE
    INTEGER         STATUS( MPI_STATUS_SIZE )
    LOGICAL         SFLAG, EFLAG
    CHARACTER*16    FNAME, VNAME
    CHARACTER*256   MESG

    INTEGER :: DIMS( 5 )      !  corner arg array for NF_PUT_VARA()
    INTEGER :: DELS( 5 )      !  corner arg array for NF_PUT_VARA()

    !!***********************************************************************
    !!   begin body of function  PN_WRGRDDED

    NLAYS = NLAYS3( FID )
    CALL MPI_BCAST( NLAYS, 1, MPI_INTEGER, 0, PN_WORLD_COMM, IERR )
    IF ( IERR .NE. 0 ) THEN
       WRITE( MESG, '(A,I9)' ) 'WRMPIGRD:  MPI_BCAST error=', IERR
       CALL M3MESG( MESG )
       WRMPIGRD = .FALSE.
       RETURN
    END IF

    DIMS( 1 ) = PN_AGG_COLSX_PE( 1, PN_AGG_MYPE_P1 )
    DELS( 1 ) = PN_AGG_NCOLS_PE(    PN_AGG_MYPE_P1 )

    DIMS( 2 ) = PN_AGG_ROWSX_PE( 1, PN_AGG_MYPE_P1 )
    DELS( 2 ) = PN_AGG_NROWS_PE(    PN_AGG_MYPE_P1 )

    DIMS( 3 ) = 1
    DELS( 3 ) = NLAYS

    DIMS( 4 ) = STEP2
    DELS( 4 ) = 1

    DELTA = DELS( 1 ) * DELS( 2 ) * DELS( 3 )

    !!...........   Perform the writes, according to VID

    IF ( VID .GT. 0 ) VTYPE = VTYPE3( VID,FID )

    IF ( VID .EQ. ALLAYS3 ) THEN        !!  "all variables"
        EFLAG = .FALSE.
        INDX  = 1
        SIZE  = PN_NCOLS_PE( PN_MYPE_P1 ) * PN_NROWS_PE( PN_MYPE_P1 ) * DELS(3)
        DO V = 1, NVARS3( FID )
            VTYPE = VTYPE3( V,FID )
            IF ( VTYPE .EQ. M3INT ) THEN
                EFLAG = ( EFLAG .OR. PN_WRINT(  FID, V, TSTAMP, STEP2, DIMS, DELS, DELTA, BUFFER( INDX ) ) )
            ELSE IF ( VTYPE .EQ. M3REAL ) THEN
                EFLAG = ( EFLAG .OR. PN_WRREAL( FID, V, TSTAMP, STEP2, DIMS, DELS, DELTA, BUFFER( INDX ) ) )
            ELSE IF ( VTYPE .EQ. M3DBLE ) THEN
                EFLAG = ( EFLAG .OR. PN_WRDBLE( FID, V, TSTAMP, STEP2, DIMS, DELS, DELTA, BUFFER( INDX ) ) )
            ELSE IF ( VTYPE .EQ. M3INT8 ) THEN
                EFLAG = ( EFLAG .OR. PN_WRINT8( FID, V, TSTAMP, STEP2, DIMS, DELS, DELTA, BUFFER( INDX ) ) )
            ELSE
                FNAME = FLIST3(FID)
                VNAME = VLIST3(V,FID)
                WRITE( MESG, '( 5A, I9 )' ) 'WRMPIGRD:  file/vble ', FNAME, '/', VNAME, ' illegal vble-type', VTYPE
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF
            INDX = INDX + SIZE * TYPSIZE( VTYPE )
        END DO
        SFLAG = ( .NOT.EFLAG )
    ELSE IF ( VTYPE .EQ. M3INT ) THEN
        SFLAG = PN_WRINT(  FID, VID, TSTAMP, STEP2, DIMS, DELS, DELTA, BUFFER )
    ELSE IF ( VTYPE .EQ. M3REAL ) THEN
        SFLAG = PN_WRREAL( FID, VID, TSTAMP, STEP2, DIMS, DELS, DELTA, BUFFER )
    ELSE IF ( VTYPE .EQ. M3DBLE ) THEN
        SFLAG = PN_WRDBLE( FID, VID, TSTAMP, STEP2, DIMS, DELS, DELTA, BUFFER )
    ELSE IF ( VTYPE .EQ. M3INT8 ) THEN
        SFLAG = PN_WRINT8( FID, VID, TSTAMP, STEP2, DIMS, DELS, DELTA, BUFFER )
    ELSE
        FNAME = FLIST3(FID)
        VNAME = VLIST3(VID,FID)
        WRITE( MESG, '( 3A, I9 )' ) 'WRMPIGRD:  file/vble ', FNAME, '/', VNAME, ' illegal vble-type', VTYPE
        CALL M3MESG( MESG )
        SFLAG = .FALSE.
    END IF

    IF ( .NOT. PN_FLAG( SFLAG ) ) THEN
        CALL M3MSG2( 'WRMPIGRD:  MPI_SEND(SFLAG) error' )
        SFLAG = .TRUE.
    END IF

    WRMPIGRD = SFLAG

#endif

#ifndef IOAPI_PNCF
    CALL M3WARN( 'WRMPIGRD',0,0, 'MPI/PnetCDF I/O not enabled in this build' )
    WRMPIGRD = .FALSE.
#endif

    RETURN

END FUNCTION WRMPIGRD

