
        LOGICAL FUNCTION RDIDDATA( FID, VID, LAYER, STEP, BUFFER )

    !!***********************************************************************
    !! Version "$Id: rdiddata.f90 1 2017-06-10 18:05:20Z coats $"
    !! EDSS/Models-3 I/O API.
    !! Copyright (C) 1992-2002 MCNC, (C) 1992-2012 Carlie J. Coats, Jr.,
    !! (C) 2003-2011 Baron Advanced Meteorological Systems, and
    !! (C) 2015 UNC Institute for the Environment
    !! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    !! See file "LGPL.txt" for conditions of use.
    !!.........................................................................
    !!  function body starts at line  85
    !!
    !!  FUNCTION:  reads data from Models-3 IDDATA data file with state-variable
    !!             index FID, for variable VID and layer LAYER, for the
    !!             time step record STEP.
    !!             If VID is -1=ALLAYS3, reads all variables; if LAYER is -1,
    !!             reads all layers.
    !!
    !!  RETURN VALUE:  TRUE iff the operation succeeds (and the data is available)
    !!
    !!  PRECONDITIONS REQUIRED:  Should only be called by READ3(), after it
    !!             has checked for file, time step, and layer availability,
    !!             and that the file type is IDDATA3.
    !!
    !!  SUBROUTINES AND FUNCTIONS CALLED:  RDVARS
    !!
    !!  REVISION  HISTORY:
    !!       prototype 3/1992 by CJC
    !!
    !!       Modified  9/1994 by CJC:  argument VID instead of VNAME
    !!
    !!       Modified 10/2003 by CJC for I/O API version 3:  support for
    !!       native-binary BINFIL3 file type; uses INTEGER NAME2FID
    !!
    !!       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !!
    !!       Modified 08/2015 by CJC: USE MODNCFIO for I/O API v3.2
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
    INTEGER, INTENT(IN   ) :: LAYER           !  layer number,  or -1 == ALL
    INTEGER, INTENT(IN   ) :: STEP            !  time step record number
    REAL   , INTENT(  OUT) :: BUFFER(*)       !  buffer array for input


    !!...........   EXTERNAL FUNCTIONS and their descriptions:

    LOGICAL, EXTERNAL :: RDVARS
    EXTERNAL          :: INITBLK3        !!  BLOCK DATA to initialize STATE3 commons


    !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         IERR            !  netCDF error status return
    INTEGER         INDX            !  subscript location in BUFFER(*)
    INTEGER         DELTA           !  d(INDX) / d(NF_GET_VARA_* call)
    INTEGER         DIMS ( 5 )      !  corner arg array for NF_GET_VARA operation
    INTEGER         DELTS( 5 )      !  corner arg array for NF_GET_VARA operation
    CHARACTER*80    MESG


    !!***********************************************************************
    !!   begin body of function  RDIDDATA

    !!.......   Read the site count for this time step

    IF ( CDFID3( FID ) .GE. 0 ) THEN        !  netCDF file

        DIMS ( 1 ) = STEP

!$OMP CRITICAL( S_NC )
        IERR = NF_GET_VAR1_INT( CDFID3( FID ), NINDX3( FID ), DIMS, BUFFER )
!$OMP END CRITICAL( S_NC )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG,'( A, I5 )' ) 'netCDF error number', IERR
            CALL M3MSG2( MESG )
            MESG = 'Error reading count for file '//FLIST3( FID )
            CALL M3WARN( 'READ3/RDIDDATA', 0, 0, MESG )
            RDIDDATA = .FALSE.
            RETURN
        END IF          !  ierr nonzero:  operation failed


        !!.......   Read the site ID list for this time step

        DIMS ( 1 ) = 1
        DELTS( 1 ) = NROWS3( FID )

        DIMS ( 2 ) = STEP
        DELTS( 2 ) = 1

!$OMP CRITICAL( S_NC )
       IERR = NF_GET_VARA_INT( CDFID3( FID ), SINDX3( FID ), DIMS, DELTS, BUFFER( 2 ) )
!$OMP END CRITICAL( S_NC )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG,'( A, I5 )' ) 'netCDF error number', IERR
            CALL M3MSG2( MESG )
            MESG = 'Error reading ID list for file '//FLIST3( FID )
            CALL M3WARN( 'READ3/RDIDDATA', 0, 0, MESG )
            RDIDDATA = .FALSE.
            RETURN
        END IF          !  ierr nonzero:  operation failed

        INDX = 2 + NROWS3( FID )

    ELSE

        INDX = 1

    END IF          !  if netCDF file


    !!.......   Set up DIMS and DELTS arguments for operation according to
    !!.......   whether or not request is for is all layers:

    DIMS ( 1 ) = 1
    DELTS( 1 ) = NROWS3( FID )

    DIMS ( 3 ) = STEP
    DELTS( 3 ) = 1

    DIMS ( 4 ) = 0
    DELTS( 4 ) = 0

    DIMS ( 5 ) = 0
    DELTS( 5 ) = 0

    IF ( LAYER .EQ. ALLAYS3 ) THEN

        DIMS ( 2 ) = 1
        DELTS( 2 ) = NLAYS3( FID )

        DELTA = NROWS3( FID ) * NLAYS3( FID )

    ELSE         !  read a specific layer

        DIMS ( 2 ) = LAYER
        DELTS( 2 ) = 1

        DELTA = NROWS3( FID )

    END IF          !  if layer == alllays or not


!!...........   Perform the reads, according to VID

    RDIDDATA = RDVARS( FID, VID, DIMS, DELTS, DELTA, BUFFER( INDX ) )

    RETURN


END FUNCTION RDIDDATA

