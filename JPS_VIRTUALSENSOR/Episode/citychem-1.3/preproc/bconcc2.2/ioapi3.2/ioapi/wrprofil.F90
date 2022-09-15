
LOGICAL FUNCTION WRPROFIL( FID, TSTAMP, STEP2, BUFFER )

    !!***********************************************************************
    !! Version "$Id: wrprofil.F90 1 2017-06-10 18:05:20Z coats $"
    !! EDSS/Models-3 I/O API.
    !! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    !! (C) 2003-2010 by Baron Advanced Meteorological Systems, and 
    !! (C) 2015 UNC Institute for the Environment
    !! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    !! See file "LGPL.txt" for conditions of use.
    !!.........................................................................
    !!  function body starts at line  99
    !!
    !!  MACHINE DEPENDENCY:  Depends upon how much space location variables
    !!             take up within BUFFER(*):  managed via parameter DBLSIZE
    !!             which should be  DBLSIZE = 1  for 64-bit machines (e.g., CRAY),
    !!             and  DBLSIZE = 2  for other machines
    !!
    !!  FUNCTION:  writes data from Models-3 PROFIL data file with STATE3
    !!             index FID, for alll variables and layers, for time step
    !!             record STEP.
    !!
    !!  RETURN VALUE:  TRUE iff the operation succeeds
    !!
    !!  PRECONDITIONS REQUIRED:  Should only be called by WRITE3(), after it
    !!             has checked that file and time step are available, and that
    !!             file type is PROFIL3.
    !!
    !!  SUBROUTINES AND FUNCTIONS CALLED:  WRVARS
    !!
    !!  REVISION  HISTORY:
    !!       prototype 3/1992 by CJC
    !!
    !!       revised  10/1994 by CJC:  allow write-by-variable; record
    !!               time-step number as time step flag; restart files.
    !!
    !!       Modified  5/1999 by CJC for OpenMP thread-safety
    !!
    !!       Modified 03/2010 by CJC: F90 changes for I/O API v3.1
    !!
    !!       Modified 08/2015 by CJC for I/O API 3.2:  USE MODNCFIO
    !!
    !!      Modified 10/2015 by CJC for I/O API 3.2: use NF_*() instead of NC*()
    !!      for netCDF-Fortran 4.x compatibility; F90 free format.
    !!***********************************************************************

    USE MODNCFIO

    IMPLICIT NONE

    !!...........   INCLUDES:

    INCLUDE 'PARMS3.EXT'
    INCLUDE 'STATE3.EXT'


    !!...........   ARGUMENTS and their descriptions:

    INTEGER, INTENT(IN   ) :: FID             !  file index within the STATE3 commons
    INTEGER, INTENT(IN   ) :: TSTAMP( 2 )     !  ( jdate yyyyddd, jtime hhmmss )
    INTEGER, INTENT(IN   ) :: STEP2           !  file record number (maybe mod 2)
    REAL   , INTENT(IN   ) :: BUFFER(*)       !  buffer array for input


    !!...........   PARAMETER and its description:
    !!...........   MACHINE DEPENDENCY:

#if  _CRAY || REAL8
    INTEGER, PARAMETER :: DBLSIZE = 1
#endif
#if  ! ( _CRAY || REAL8 )
    INTEGER, PARAMETER :: DBLSIZE = 2
#endif


    !!...........   EXTERNAL FUNCTIONS and their descriptions:

    LOGICAL, EXTERNAL :: WRVARS
    EXTERNAL          :: INITBLK3   !  block data: initialize I/O state


    !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER       ICDF            !  netCDF ID
    INTEGER       IERR            !  netCDF error status return
    INTEGER       INDX            !  subscript location in BUFFER(*)
    INTEGER       DELTA           !  d(INDX) / d(NCVPTcall)
    INTEGER       DIMS( 5 )       !  corner arg array for NF_PUT_VARA*()
    INTEGER       DELS( 5 )       !  corner arg array for NF_PUT_VARA*()
    LOGICAL       EFLAG


    !!***********************************************************************
    !!   begin body of function  WRPROFIL

    !!...........   Write the site count for this time step

    EFLAG = .FALSE.
    ICDF  = CDFID3( FID )

!$OMP CRITICAL( S_LOGOUT )
!$OMP CRITICAL( S_NC )

    DIMS ( 1 ) = STEP2

    IERR  = NF_PUT_VAR1_INT( ICDF, NINDX3( FID ), DIMS, BUFFER )
    IF ( IERR .NE. 0 ) THEN
        WRITE( LOGDEV, '(5X, A, I6, /, 5X, A )' )   &
            'netCDF error number', IERR,            &
            'Error writing site count to file ' // FLIST3( FID )
        EFLAG = .TRUE.
        GO TO  999
    END IF          !  ierr nonzero:  operation failed


    !!.......   Write the site ID list for this time step

    INDX  = 2
    DELTA = NROWS3( FID )
    DIMS( 1 ) = 1
    DELS( 1 ) = NROWS3( FID )

    DIMS ( 2 ) = STEP2
    DELS( 2 ) = 1
    
    IERR  = NF_PUT_VARA_INT( ICDF, SINDX3( FID ), DIMS, DELS, BUFFER( INDX ) )
    IF ( IERR .NE. 0 ) THEN
        WRITE( LOGDEV, '(5X, A, I6, /, 5X, A )' )   &
            'netCDF error number', IERR,            &
            'Error writing ID list to file ' // FLIST3( FID )
            EFLAG = .TRUE.
            GO TO  999
    END IF          !  ierr nonzero:  operation failed

    INDX = INDX  +  DELTA


    !!.......   Write the site profile-count list for this time step
    
    IERR  = NF_PUT_VARA_INT( ICDF, LINDX3( FID ), DIMS, DELS, BUFFER( INDX ) )
    IF ( IERR .NE. 0 ) THEN
        WRITE( LOGDEV, '(5X, A, I6, /, 5X, A )' )   &
            'netCDF error number', IERR,            &
            'Error writing profile-counts to file ' // FLIST3( FID )
            EFLAG = .TRUE.
            GO TO  999
    END IF          !  ierr nonzero:  operation failed

    INDX = INDX  +  DELTA


    !!.......   Write the site X,Y,Z location lists for this time step

    IERR  = NF_PUT_VARA_DOUBLE( ICDF, XINDX3( FID ), DIMS, DELS, BUFFER( INDX ) )
    IF ( IERR .NE. 0 ) THEN
        WRITE( LOGDEV, '(5X, A, I6, /, 5X, A )' )   &
            'netCDF error number', IERR,            &
            'Error writing X-coordinate list to file ' // FLIST3( FID )
            EFLAG = .TRUE.
            GO TO  999
    END IF          !  ierr nonzero:  operation failed

    INDX = INDX  +  DELTA * DBLSIZE

    IERR  = NF_PUT_VARA_DOUBLE( ICDF, YINDX3( FID ), DIMS, DELS, BUFFER( INDX ) )
    IF ( IERR .NE. 0 ) THEN
        WRITE( LOGDEV, '(5X, A, I6, /, 5X, A )' )   &
            'netCDF error number', IERR,            &
            'Error writing Y-coordinate list to file ' // FLIST3( FID )
            EFLAG = .TRUE.
            GO TO  999
    END IF          !  ierr nonzero:  operation failed

    INDX = INDX  +  DELTA * DBLSIZE

    IERR  = NF_PUT_VARA_DOUBLE( ICDF, ZINDX3( FID ), DIMS, DELS, BUFFER( INDX ) )
    IF ( IERR .NE. 0 ) THEN
        WRITE( LOGDEV, '(5X, A, I6, /, 5X, A )' )   &
            'netCDF error number', IERR,            &
            'Error writing Z-coordinate list to file ' // FLIST3( FID )
            EFLAG = .TRUE.
            GO TO  999
    END IF          !  ierr nonzero:  operation failed

    INDX = INDX  +  DELTA * DBLSIZE

999 CONTINUE

!$OMP END CRITICAL( S_NC )
!$OMP END CRITICAL( S_LOGOUT )

    IF ( EFLAG ) THEN
        WRPROFIL = .FALSE.
        RETURN
    END IF


    !!...........   Perform the writes, according to VNAME

    DIMS( 1 ) = 1
    DELS( 1 ) = NCOLS3( FID )

    DIMS( 2 ) = 1
    DELS( 2 ) = NROWS3( FID )

    DIMS( 3 ) = 1
    DELS( 3 ) = NLAYS3( FID )

    DIMS( 4 ) = STEP2
    DELS( 4 ) = 1

    DELTA = NCOLS3( FID ) * NROWS3( FID ) * NLAYS3( FID )

    WRPROFIL = WRVARS( FID, ALLAYS3, TSTAMP, STEP2, DIMS, DELS, DELTA, BUFFER( INDX ) )

    RETURN


END FUNCTION WRPROFIL

