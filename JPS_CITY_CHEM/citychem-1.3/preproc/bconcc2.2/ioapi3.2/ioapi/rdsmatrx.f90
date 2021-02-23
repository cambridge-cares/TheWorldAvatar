
LOGICAL FUNCTION RDSMATRX( FID, VID, STEP, BUFFER )

    !!***********************************************************************
    !! Version "$Id: rdsmatrx.f90 1 2017-06-10 18:05:20Z coats $"
    !! EDSS/Models-3 I/O API.
    !! Copyright (C) 1992-2002 MCNC, (C) 1992-2012 Carlie J. Coats, Jr.,
    !! (C) 2003-2011 Baron Advanced Meteorological Systems, and
    !! (C) 2015 UNC Institute for the Environment
    !! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    !! See file "LGPL.txt" for conditions of use.
    !!.........................................................................
    !!  function body starts at line  72
    !!
    !!  FUNCTION:  reads data from Models-3 SMATRX3 data file with state-variable
    !!             index FID, for variable VID for the time step record STEP.
    !!             If VID is -1=ALLAYS3, reads all variables.
    !!
    !!  RETURN VALUE:  TRUE iff the operation succeeds (and the data is available)
    !!
    !!  PRECONDITIONS REQUIRED:  Should only be called by READ3(), after it
    !!             has checked for file, time step, and variable availability,
    !!             and that the file type is SMATRX3.
    !!
    !!  SUBROUTINES AND FUNCTIONS CALLED:  RDVARS
    !!
    !!  REVISION  HISTORY:
    !!      prototype 2/1995 by CJC
    !!
    !!      Modified 10/2003 by CJC for I/O API version 3:  support for
    !!      native-binary BINFIL3 file type; uses INTEGER NAME2FID
    !!
    !!      Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !!
    !!      Modified 08/2015 by CJC: USE MODNCFIO for I/O API v3.2
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
    INTEGER, INTENT(IN   ) :: VID             !  variable index or -1 == ALL
    INTEGER, INTENT(IN   ) :: STEP            !  time step record number
    REAL   , INTENT(  OUT) :: BUFFER(*)       !  buffer array for input


    !!...........   EXTERNAL FUNCTIONS and their descriptions:

    LOGICAL, EXTERNAL :: RDVARS


    !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         IERR            !  netCDF error status return
    INTEGER         INDX            !  subscript location in BUFFER(*)
    INTEGER         DELTA           !  d(INDX) / d(NF_GET_VARA_* call)
    INTEGER         DIMS ( 5 )      !  corner arg array for NF_GET_VARA_*()
    INTEGER         DELTS( 5 )      !  corner arg array for NF_GET_VARA_*()
    CHARACTER*80    MESG


    !!***********************************************************************
    !!   begin body of function  RDSMATRX

    !!.......   Read the max-col-count array for this time step

    DIMS ( 1 ) = 1
    DELTS( 1 ) = NROWS3( FID )

    DIMS ( 2 ) = STEP
    DELTS( 2 ) = 1

    DIMS ( 3 ) = 0
    DELTS( 3 ) = 0

    DIMS ( 4 ) = 0
    DELTS( 4 ) = 0

    DIMS ( 5 ) = 0
    DELTS( 5 ) = 0

    IF ( CDFID3( FID ) .GE. 0 ) THEN                !   netcdf file:

!$OMP CRITICAL( S_NC )
        IERR = NF_GET_VARA_INT( CDFID3( FID ), SINDX3( FID ), DIMS, DELTS, BUFFER )
!$OMP END CRITICAL( S_NC )

        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG,'( A, I5 )' ) 'netCDF error number', IERR
            CALL M3MSG2( 'Error reading MAXROW for file ' // FLIST3( FID ) )
            CALL M3WARN( 'READ3/RDSMATRX', 0, 0, MESG )
            RDSMATRX = .FALSE.
            RETURN
        END IF          !  ierr nonzero:  operation failed


        !!.......   Read the column-index array for this time step

        DIMS ( 1 ) = 1
        DELTS( 1 ) = NCOLS3( FID )

        INDX = NROWS3( FID ) + 1

!$OMP CRITICAL( S_NC )
        IERR = NF_GET_VARA_INT( CDFID3( FID ), LINDX3( FID ), DIMS, DELTS, BUFFER( INDX ) )
!$OMP END CRITICAL( S_NC )

        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG,'( A, I5 )' ) 'netCDF error number', IERR
            CALL M3MSG2( 'Error reading ROW INDEX for file ' // FLIST3( FID ) )
            CALL M3WARN( 'READ3/RDSMATRX', 0, 0, MESG )
            RDSMATRX = .FALSE.
            RETURN
        END IF          !  ierr nonzero:  operation failed

        !!.......   Set up DIMS and DELTS arguments for NF_GET_VARA_*() according to
        !!.......   whether or not request is for is all layers:

        DELTA = NCOLS3( FID )
        INDX  = INDX + DELTA

    ELSE            !  fixup buffer-offset

        INDX = 1

    END IF          !  if netcdf file



    !!...........   Perform the reads, according to VID

111 CONTINUE
    RDSMATRX = RDVARS( FID, VID, DIMS, DELTS, DELTA, BUFFER( INDX ) )

    RETURN

END FUNCTION RDSMATRX

