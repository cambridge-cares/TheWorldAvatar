
LOGICAL FUNCTION WRGRNEST( FID, TSTAMP, STEP2, BUFFER )

    !!***********************************************************************
    !! Version "$Id: wrgrnest.F90 1 2017-06-10 18:05:20Z coats $"
    !! EDSS/Models-3 I/O API.
    !! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    !! (C) 2003-2010 Baron Advanced Meteorological Systems,
    !! (C) 2007-2013 Carlie J. Coats, Jr., and
    !! (C) 2015 UNC Institute for the Environment.
    !! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    !! See file "LGPL.txt" for conditions of use.
    !!.........................................................................
    !!  function body starts at line  100
    !!
    !!  MACHINE DEPENDENCY:  Depends upon how much space location variables
    !!             take up within BUFFER(*):  managed via parameter DBLSIZE
    !!             which should be  DBLSIZE = 1  for 64-bit machines (e.g., CRAY),
    !!             and  DBLSIZE = 2  for other machines
    !!
    !!  FUNCTION:  writes data from Models-3 GRNEST data file with STATE3
    !!             index FID, for alll variables and layers, for time step
    !!             record STEP.
    !!
    !!  RETURN VALUE:  TRUE iff the operation succeeds
    !!
    !!  PRECONDITIONS REQUIRED:  Should only be called by WRITE3(), after it
    !!             has checked that file and time step are available, and that
    !!             file type is GRNEST3.
    !!
    !!  SUBROUTINES AND FUNCTIONS CALLED:  WRVARS
    !!
    !!  REVISION  HISTORY:
    !!      prototype 3/1992 by CJC
    !!
    !!      revised  10/1994 by CJC:  allow write-by-variable; record
    !!      time-step number as time step flag; support restart files.
    !!
    !!      Modified  5/1999 by CJC for OpenMP log-file thread-safety
    !!
    !!      Modified  2/2002 by CJC for OpenMP netCDF thread-safety
    !!
    !!      Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !!
    !!      Modified 08/2015 by CJC for I/O API 3.2:  USE MODNCFIO
    !!
    !!      Modified 10/2015 by CJC for I/O API 3.2: use NF_*() instead of NC*()
    !!      for netCDF-Fortran 4.x compatibility; F90 free source format.
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


    !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER       IERR            !  netCDF error status return
    INTEGER       INDX            !  subscript location in BUFFER(*)
    INTEGER       DELTA           !  d(INDX) / d(NCVGTcall)
    INTEGER       DIMS ( 5 )      !  corner arg array for NF_PUT_VARA()
    INTEGER       DELTS( 5 )      !  corner arg array for NF_PUT_VARA()
    LOGICAL       EFLAG           !  error flag


    !!***********************************************************************
    !!   begin body of function  WRGRNEST

    !!...........   Write the site count for this time step

    EFLAG = .FALSE.

!$OMP CRITICAL( S_LOGOUT )
!$OMP CRITICAL( S_NC )

    DIMS ( 1 ) = STEP2

    IERR  = NF_PUT_VAR1_INT( CDFID3( FID ), NINDX3( FID ), DIMS, BUFFER )
    IF ( IERR .NE. 0 ) THEN
        WRITE( LOGDEV, '( 5X, A, I6, /, 5X, A )' )  &
            'netCDF error number', IERR,            &
            'Error writing site count to file ' // FLIST3( FID )
        EFLAG = .TRUE.
        GO TO  999
    END IF          !  ierr nonzero:  operation failed

    INDX  = 2
    DELTA = NROWS3( FID )


    !!.......   Write the site ID list for this time step

    DIMS ( 1 ) = 1
    DELTS( 1 ) = NROWS3( FID )

    DIMS ( 2 ) = STEP2
    DELTS( 2 ) = 1

    INDX = 2

    IERR  = NF_PUT_VARA_INT( CDFID3( FID ), SINDX3( FID ), DIMS, DELTS, BUFFER( INDX ) )
    IF ( IERR .NE. 0 ) THEN
        WRITE( LOGDEV, '( 5X, A, I6, /, 5X, A )' )  &
            'netCDF error number', IERR,            &
            'Error writing site ID list to file ' // FLIST3( FID )
        EFLAG = .TRUE.
        GO TO  999
    END IF          !  ierr nonzero:  operation failed

    INDX = INDX  +  DELTA


    !!.......   Write the site  column, row, and level count lists for this time step

    IERR  = NF_PUT_VARA_INT( CDFID3( FID ), WCNDX3( FID ), DIMS, DELTS, BUFFER( INDX ) )
    IF ( IERR .NE. 0 ) THEN
        WRITE( LOGDEV, '( 5X, A, I6, /, 5X, A )' )  &
        'netCDF error number', IERR,                &
        'Error writing site column count to file ' // FLIST3( FID )
    EFLAG = .TRUE.
        GO TO  999
    END IF          !  ierr nonzero:  operation failed

    INDX = INDX  +  DELTA

    IERR  = NF_PUT_VARA_INT( CDFID3( FID ), WRNDX3( FID ), DIMS, DELTS, BUFFER( INDX ) )
    IF ( IERR .NE. 0 ) THEN
        WRITE( LOGDEV, '( 5X, A, I6, /, 5X, A )' )      &
            'netCDF error number', IERR,                &
            'Error writing site row count to file ' // FLIST3( FID )
        EFLAG = .TRUE.
        GO TO  999
    END IF          !  ierr nonzero:  operation failed

    INDX = INDX  +  DELTA

    IERR  = NF_PUT_VARA_INT( CDFID3( FID ), LINDX3( FID ), DIMS, DELTS, BUFFER( INDX ) )
    IF ( IERR .NE. 0 ) THEN
        WRITE( LOGDEV, '( 5X, A, I6, /, 5X, A )' )      &
            'netCDF error number', IERR,                &
            'Error writing site level count to file ' // FLIST3( FID )
        EFLAG = .TRUE.
        GO TO  999
    END IF          !  ierr nonzero:  operation failed

    INDX = INDX  +  DELTA


    !!.......   Write the site X,Y,Z location lists for this time step

    IERR  = NF_PUT_VARA_DOUBLE( CDFID3( FID ), XINDX3( FID ), DIMS, DELTS, BUFFER( INDX ) )
    IF ( IERR .NE. 0 ) THEN
    WRITE( LOGDEV, '( 5X, A, I6, /, 5X, A )' )      &
        'netCDF error number', IERR,                &
        'Error writing X-coordinate list to file ' // FLIST3( FID )
        EFLAG = .TRUE.
        GO TO  999
    END IF          !  ierr nonzero:  operation failed

    INDX = INDX  +  DELTA * DBLSIZE

    IERR  = NF_PUT_VARA_DOUBLE( CDFID3( FID ), YINDX3( FID ), DIMS, DELTS, BUFFER( INDX ) )
    IF ( IERR .NE. 0 ) THEN
    WRITE( LOGDEV, '( 5X, A, I6, /, 5X, A )' )      &
        'netCDF error number', IERR,                &
        'Error writing Y-coordinate list to file ' // FLIST3( FID )
        EFLAG = .TRUE.
        GO TO  999
    END IF          !  ierr nonzero:  operation failed

    INDX = INDX  +  DELTA * DBLSIZE

    IERR  = NF_PUT_VARA_DOUBLE( CDFID3( FID ), DXNDX3( FID ), DIMS, DELTS, BUFFER( INDX ) )
    IF ( IERR .NE. 0 ) THEN
    WRITE( LOGDEV, '( 5X, A, I6, /, 5X, A )' )      &
        'netCDF error number', IERR,                &
        'Error writing cellsize DX to file ' // FLIST3( FID )
        EFLAG = .TRUE.
        GO TO  999
    END IF          !  ierr nonzero:  operation failed

    INDX = INDX  +  DELTA * DBLSIZE

    IERR  = NF_PUT_VARA_DOUBLE( CDFID3( FID ), DYNDX3( FID ), DIMS, DELTS, BUFFER( INDX ) )
    IF ( IERR .NE. 0 ) THEN
    WRITE( LOGDEV, '( 5X, A, I6, /, 5X, A )' )      &
        'netCDF error number', IERR,                &
        'Error writing cellsize DY to file ' // FLIST3( FID )
        EFLAG = .TRUE.
        GO TO  999
    END IF          !  ierr nonzero:  operation failed

    INDX = INDX  +  DELTA * DBLSIZE

999     CONTINUE

!$OMP END CRITICAL( S_NC )
!$OMP END CRITICAL( S_LOGOUT )

    IF ( EFLAG ) THEN
        WRGRNEST = .FALSE.
        RETURN
    END IF

    !!...........   Perform the writes, according to VNAME

    DIMS ( 1 ) = 1
    DELTS( 1 ) = NCOLS3( FID )

    DIMS ( 2 ) = 1
    DELTS( 2 ) = NROWS3( FID )

    DIMS ( 3 ) = 1
    DELTS( 3 ) = NLAYS3( FID )

    DIMS ( 4 ) = STEP2
    DELTS( 4 ) = 1

    DELTA = NCOLS3( FID ) * NROWS3( FID ) * NLAYS3( FID )

    WRGRNEST = WRVARS( FID, ALLAYS3, TSTAMP, STEP2, DIMS, DELTS, DELTA, BUFFER( INDX ) )

    RETURN


END FUNCTION WRGRNEST

