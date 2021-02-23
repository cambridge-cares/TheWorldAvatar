
LOGICAL FUNCTION WRIDDATA( FID, TSTAMP, STEP2, BUFFER )

    !!***********************************************************************
    !! Version "$Id: wriddata.f90 1 2017-06-10 18:05:20Z coats $"
    !! EDSS/Models-3 I/O API.
    !! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    !! (C) 2003-2010 Baron Advanced Meteorological Systems,
    !! (C) 2007-2013 Carlie J. Coats, Jr., and
    !! (C) 2015 UNC Institute for the Environment.
    !! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    !! See file "LGPL.txt" for conditions of use.
    !!.........................................................................
    !!  function body starts at line  83
    !!
    !!  FUNCTION:  writes data from Models-3 IDDATA data file with STATE3
    !!             index FID, for alll variables and layers, for time step
    !!             record TSTAMP.
    !!
    !!  RETURN VALUE:  TRUE iff the operation succeeds
    !!
    !!  PRECONDITIONS REQUIRED:  Should only be called by WRITE3(), after it
    !!             has checked that file and time step are available, and that
    !!             file type is IDDATA3.
    !!
    !!  SUBROUTINES AND FUNCTIONS CALLED:  WRVARS
    !!
    !!  REVISION  HISTORY:
    !!       prototype 3/1992 by CJC
    !!
    !!       revised  10/1994 by CJC:  allow write-by-variable; record
    !!       time-step number as time step flag; restart files.
    !!
    !!       revised  2/2002 by CJC:  OpenMP thread-safety
    !!
    !!       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !!
    !!       Modified 08/2015 by CJC for I/O API 3.2:  USE MODNCFIO
    !!
    !!      Modified 10/2015 by CJC for I/O API 3.2: use NF_*() instead of NC*()
    !!      for netCDF-Fortran 4.x compatibility; F90 "free" source format.
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


    !!...........   EXTERNAL FUNCTIONS and their descriptions:

    LOGICAL, EXTERNAL :: WRVARS     !  write "variables" part of timestep record


    !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER       IERR            !  netCDF error status return
    INTEGER       INDX            !  subscript location in BUFFER(*)
    INTEGER       DELTA           !  d(INDX) / d(NCVPTcall)
    INTEGER       DIMS ( 5 )      !  corner arg array for NF_PUT_VARA()
    INTEGER       DELTS( 5 )      !  corner arg array for NF_PUT_VARA()
    INTEGER       ICDF
    LOGICAL       EFLAG


    !!***********************************************************************
    !!   begin body of function  WRIDDATA

    !!.......   Write the site count for this

    EFLAG = .FALSE.
    ICDF  = CDFID3( FID )

!$OMP CRITICAL( S_LOGOUT )

!$OMP   CRITICAL( S_NC )

    DIMS ( 1 ) = STEP2
    DELTS( 1 ) = 1

    IERR  = NF_PUT_VAR1_INT( ICDF, NINDX3( FID ), DIMS, BUFFER )
    IF ( IERR .NE. 0 ) THEN
        WRITE( LOGDEV, '( 5X, A, I6, /, 5X, A )' )  &
            'netCDF error number', IERR,            &
            'Error writing site count to file ' // FLIST3( FID )
        EFLAG = .TRUE.
        GO TO  999
    END IF          !  ierr nonzero:  operation failed


    !!.......   Write the site ID list for this time step

    DIMS ( 1 ) = 1
    DELTS( 1 ) = NROWS3( FID )

    DIMS ( 2 ) = STEP2
    DELTS( 2 ) = 1

    INDX = 2

    IERR  = NF_PUT_VARA_INT( ICDF, SINDX3( FID ), DIMS, DELTS, BUFFER( INDX ) )

    IF ( IERR .NE. 0 ) THEN
        WRITE( LOGDEV, '( 5X, A, I6, /, 5X, A )' )  &
            'netCDF error number', IERR,            &
            'Error writing site ID list to file ' // FLIST3( FID )
        EFLAG = .TRUE.
        GO TO  999
    END IF          !  ierr nonzero:  operation failed

    INDX = INDX  +  NROWS3( FID )

999 CONTINUE

!$OMP   END CRITICAL( S_NC )

!$OMP END CRITICAL( S_LOGOUT )

    IF ( EFLAG ) THEN
        WRIDDATA = .FALSE.
        RETURN
    END IF

    !!...........   Perform the writes of the "variables" part of the data:

    DIMS ( 1 ) = 1
    DELTS( 1 ) = NROWS3( FID )

    DIMS ( 2 ) = 1
    DELTS( 2 ) = NLAYS3( FID )

    DIMS ( 3 ) = STEP2
    DELTS( 3 ) = 1

    DELTA = NROWS3( FID ) * NLAYS3( FID )

    WRIDDATA = WRVARS( FID, ALLAYS3, TSTAMP, STEP2, DIMS, DELTS, DELTA, BUFFER ( INDX ) )

    RETURN


END FUNCTION WRIDDATA

