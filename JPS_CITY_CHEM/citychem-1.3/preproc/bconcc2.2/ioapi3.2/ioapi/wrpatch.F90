
LOGICAL FUNCTION WRPATCH( FID, VID, TSTAMP, STEP2, SIZE_MAP, GRID_MAP, VBUF )     &
        RESULT( WRFLAG )

    !!***********************************************************************
    !! Version "$Id: wrpatch.F90 1 2017-06-10 18:05:20Z coats $"
    !! EDSS/Models-3 I/O API.
    !! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    !! (C) 2003-2013 Baron Advanced Meteorological Systems,
    !! (C) 2007-2013 Carlie J. Coats, Jr., and
    !! (C) 2014-2015 UNC Institute for the Environment.
    !! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    !! See file "LGPL.txt" for conditions of use.
    !!.........................................................................
    !!  function body starts at line  97
    !!
    !!  FUNCTION:
    !!       writes tile of data defined by data  SIZE_MAP, GRID_MAP
    !!       from netCDFF-mode Models-3 GRIDDED data file with STATE3
    !!       index FID, for all variables and layers, for the time step
    !!       record STEP2.
    !!
    !!  RETURN VALUE:
    !!       TRUE iff the operation succeeds
    !!
    !!  PRECONDITIONS REQUIRED:
    !!       Should only be called by WRITE3(), after it has checked that
    !!       file and time step are available, and that file type is GRDDED3.
    !!
    !!  SUBROUTINES AND FUNCTIONS CALLED:
    !!       netCDF
    !!
    !!  REVISION  HISTORY:
    !!       Version 8/2001 WRGRDDED_P adapted from WRGRDDED by David Wong,
    !!       Lockheed Martin
    !!
    !!       Version 2/2002 by Carlie J. Coats, Jr, MCNC-EMC:
    !!       Adapted from WRGRDDED_P and WRPATCH and modified to ensure
    !!       correctness of time-stamp handling (which must be performed
    !!       by a separate WRTFLAG call subsequent to completion of all
    !!       the WRPATCH calls).
    !!
    !!       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !!
    !!       Modified 02-08/2015 by CJC for I/O API 3.2: USE M3UTILIO, MODNCFIO;
    !!       support for M3INT8 variables, factor out CONTAINed subroutine LOGERR()
    !!
    !!      Modified 10/2015 by CJC for I/O API 3.2: use NF_*() instead of NC*()
    !!      for netCDF-Fortran 4.x compatibility; F90 free format.
    !!***********************************************************************

    USE M3UTILIO
    USE MODNCFIO

    IMPLICIT NONE

    !!...........   INCLUDES:

    INCLUDE 'STATE3.EXT'


    !!...........   ARGUMENTS and their descriptions:

    INTEGER, INTENT(IN   ) :: FID             !  file index within the STATE3 commons
    INTEGER, INTENT(IN   ) :: VID             !  vble index within the STATE3 commons
    INTEGER, INTENT(IN   ) :: TSTAMP( 2 )     !  ( jdate yyyyddd, jtime hhmmss )
    INTEGER, INTENT(IN   ) :: STEP2           !  file record number (maybe mod 2)
    INTEGER, INTENT(IN   ) :: GRID_MAP(2)     !  starting corners for grid-patch
    INTEGER, INTENT(IN   ) :: SIZE_MAP(3)     !  diagonals for grid-patch
    REAL   , INTENT(IN   ) :: VBUF(*)         !  buffer array for input


    !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         DELTA           !  d(INDX) / d(NCVGTcall)
    INTEGER         DIMS( 5 )       !  corner arg array for NF_PUT_VARA*()
    INTEGER         DELS( 5 )       !  corner arg array for NF_PUT_VARA*()

    INTEGER         IERR            !  netCDF error status return
    INTEGER         VAR             !  loop counter for file variables
    INTEGER         INDX            !  subscript location in VBUF(*)
    INTEGER         VTYPE, VINDX, FNUM
    LOGICAL         AFLAG
    CHARACTER*16    FNAME, VNAME

#if _CRAY || REAL8
    INTEGER, PARAMETER :: TYPSIZE( 10 ) = (/ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 /)
#endif
#if ! ( _CRAY || REAL8 )
    INTEGER, PARAMETER :: TYPSIZE( 10 ) = (/ 1, 1, 1, 1, 1, 2, 1, 1, 1, 2 /)
#endif


    !!***********************************************************************
    !!   begin body of function  WRPATCH

    DIMS( 1 ) = GRID_MAP(1)
    DELS( 1 ) = SIZE_MAP(1)

    DIMS( 2 ) = GRID_MAP(2)
    DELS( 2 ) = SIZE_MAP(2)

    DIMS( 3 ) = 1
    DELS( 3 ) = SIZE_MAP(3)

    DIMS( 4 ) = STEP2
    DELS( 4 ) = 1
    
    FNUM = CDFID3( FID )

    DELTA = SIZE_MAP(1) * SIZE_MAP(2) * SIZE_MAP(3)

    !!...........   Perform the writes, according to VID

    IF ( VID .GT. 0 ) THEN          !  write just one variable

        VTYPE = VTYPE3( VID,FID )
        VINDX = VINDX3( VID,FID )

!$OMP CRITICAL( S_NC )
        IF ( VTYPE .EQ. M3INT ) THEN
            IERR  = NF_PUT_VARA_INT(    FNUM, VINDX, DIMS, DELS, VBUF )
        ELSE IF ( VTYPE .EQ. M3REAL ) THEN
            IERR  = NF_PUT_VARA_REAL(   FNUM, VINDX, DIMS, DELS, VBUF )
        ELSE IF ( VTYPE .EQ. M3DBLE ) THEN
            IERR  = NF_PUT_VARA_DOUBLE( FNUM, VINDX, DIMS, DELS, VBUF )
        ELSE IF ( VTYPE .EQ. M3INT8 ) THEN
            IERR  = NF_PUT_VARA_INT64( FNUM, VINDX, DIMS, DELS, VBUF )
        ELSE
            IERR = NF_EBADTYPE
        END IF
!$OMP END CRITICAL( S_NC )

        IF ( IERR .NE. 0 ) THEN
            CALL LOGERR( FID, VAR, IERR )
            WRFLAG = .FALSE.
            RETURN
        END IF              !  if ierr nonzero:  operation failed

    ELSE                    !  else write all variables:

        INDX  = 1           !  starting subscript for VBUF(*)
        AFLAG = .TRUE.

        DO  VAR = 1 , NVARS3( FID )

            VTYPE = VTYPE3( VAR,FID )
            VINDX = VINDX3( VAR,FID )

!$OMP CRITICAL( S_NC )
            IF ( VTYPE .EQ. M3INT ) THEN
                IERR  = NF_PUT_VARA_INT(     FNUM, VINDX, DIMS, DELS, VBUF( INDX ) )
            ELSE IF ( VTYPE .EQ. M3REAL ) THEN
                IERR  = NF_PUT_VARA_REAL(   FNUM, VINDX, DIMS, DELS, VBUF( INDX ) )
            ELSE IF ( VTYPE .EQ. M3DBLE ) THEN
                IERR  = NF_PUT_VARA_DOUBLE( FNUM, VINDX, DIMS, DELS, VBUF( INDX ) )
            ELSE IF ( VTYPE .EQ. M3INT8 ) THEN
                IERR  = NF_PUT_VARA_INT64(  FNUM, VINDX, DIMS, DELS, VBUF( INDX ) )
            ELSE
                IERR = NF_EBADTYPE
            END IF
!$OMP END CRITICAL( S_NC )
            IF ( IERR .NE. 0 ) THEN
                CALL LOGERR( FID, VAR, IERR )
                WRFLAG = .FALSE.
                RETURN
            END IF                  !  ierr nonzero:  operation failed

            INDX = INDX  +  DELTA * TYPSIZE( VTYPE )

        END DO              ! end loop on variables VAR

    END IF                  !  if writing just one vble, or all vbles

    WRFLAG = .TRUE.        ! (if you get to here)
    RETURN


CONTAINS    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


        SUBROUTINE LOGERR( FID, VAR, ERR )

            INTEGER, INTENT(IN   ) :: FID, VAR, ERR

            INTEGER         L, CDFID, VINDX
            CHARACTER*16    VNAME, FNAME

            L     = LOGDEV
            CDFID = CDFID3( FID )
            VINDX = VINDX3( VAR,FID )
            FNAME = FLIST3( FID )
            VNAME = VLIST3( VAR,FID )

            WRITE( L,'(5X, 4A )'  ) 'WRPATCH:  Error writing patch of variable ', VNAME, ' to file ' // FNAME
            WRITE( L,'(5X,A, I4)' ) 'Error number', IERR
            WRITE( L,'(5X,A, I4)' ) 'IOAPI ID    ', FID
            WRITE( L,'(5X,A, I4)' ) 'netCDF ID   ', CDFID
            WRITE( L,'(5X,A, I4)' ) 'vble        ', VINDX
            WRITE( L,'(5X,A,5I4)' ) 'dims array  ', DIMS
            WRITE( L,'(5X,A,5I4)' ) 'delts array ', DELS
            WRITE( L,'(5X,A, I4)' ) 'offset      ', INDX

        END SUBROUTINE LOGERR


END FUNCTION WRPATCH

