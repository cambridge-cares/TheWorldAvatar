
LOGICAL FUNCTION RDGRNEST( FID, VID, LAYER, STEP, BUFFER )

    !!***********************************************************************
    !! Version "$Id: rdgrnest.F90 1 2017-06-10 18:05:20Z coats $"
    !! EDSS/Models-3 I/O API.
    !! Copyright (C) 1992-2002 MCNC, (C) 1992-2012 Carlie J. Coats, Jr.,
    !! (C) 2003-2011 Baron Advanced Meteorological Systems, and
    !! (C) 2015 UNC Institute for the Environment
    !! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    !! See file "LGPL.txt" for conditions of use.
    !!.........................................................................
    !!  function body starts at line  96
    !!
    !!  MACHINE DEPENDENCY:  Depends upon how much space location variables
    !!             take up within BUFFER(*):  managed via parameter DBLSIZE
    !!             which should be  DBLSIZE = 1  for 64-bit machines (e.g., CRAY),
    !!             and  DBLSIZE = 2  for other machines
    !!
    !!  FUNCTION:  reads data from Models-3 GRDDED data file with state-variable
    !!             index FID, for variable VID and layer LAYER, for the
    !!             time step record STEP.
    !!             If VID is -1, reads all variables; if LAYER is -1,
    !!             reads all layers.
    !!
    !!  RETURN VALUE:  TRUE iff the operation succeeds (and the data is available)
    !!
    !!  PRECONDITIONS REQUIRED:  Should only be called by READ3(), after it
    !!             has checked for file, time step, and layer availability,
    !!             and that file type is GRNEST3.
    !!
    !!  SUBROUTINES AND FUNCTIONS CALLED:  RDVARS
    !!
    !!  REVISION  HISTORY:
    !!      prototype 3/92 by CJC
    !!
    !!      Modified  9/94 by CJC:  argument now VID, not VNAME
    !!
    !!      Modified  9/99 by CJC:  portability issues
    !!
    !!      Modified 10/2003 by CJC for I/O API version 3:  support for
    !!      native-binary BINFIL3 file type; uses INTEGER NAME2FID
    !!
    !!      Modified 03/20010 by CJC: F90 changes for I/O API v3.1
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
    INTEGER, INTENT(IN   ) :: LAYER           !  layer number,  or -1 == ALL
    INTEGER, INTENT(IN   ) :: STEP            !  time step record number
    REAL   , INTENT(  OUT) :: BUFFER(*)       !  buffer array for input


    !!...........   PARAMETER and its description:
    !!...........   MACHINE DEPENDENCY:

#if  _CRAY || REAL8
    INTEGER, PARAMETER :: DBLSIZE = 1
#endif
#if  ! ( _CRAY || REAL8 )
    INTEGER, PARAMETER :: DBLSIZE = 2
#endif


    !!...........   EXTERNAL FUNCTIONS and their descriptions:

    LOGICAL, EXTERNAL :: RDVARS     !  read "variables" part of timestep records
    EXTERNAL          :: INITBLK3   !  block data: initialize I/O state


    !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         ICDF
    INTEGER         IERR            !  netCDF error status return
    INTEGER         INDX            !  subscript location in BUFFER(*)
    INTEGER         DELTA           !  d(INDX) / d(NF_GET_VARA_* call)
    INTEGER         DIMS ( 5 )      !  corner arg array for NF_GET_VARA_*()
    INTEGER         DELTS( 5 )      !  corner arg array for NF_GET_VARA_*()
    CHARACTER*512   MESG


    !!***********************************************************************
    !!   begin body of function  RDGRNEST

    !!...........   Read the site count for this time step

    ICDF = CDFID3( FID )
    
    IF ( ICDF .GE. 0 ) THEN          !  netcdf file:

        DIMS ( 1 ) = STEP

!$OMP CRITICAL( S_NC )
        IERR = NF_GET_VAR1_INT( ICDF, NINDX3( FID ), DIMS, BUFFER )
!$OMP END CRITICAL( S_NC )
      IF ( IERR .NE. 0 ) THEN
            WRITE( MESG,'( A, I5 )' ) 'netCDF error number', IERR
            CALL M3MSG2( MESG )
            MESG = 'Error reading count for file ' //FLIST3( FID )
            CALL M3WARN( 'READ3/RDGRNEST', 0, 0, MESG )
            RDGRNEST = .FALSE.
            RETURN
        END IF          !  ierr nonzero:  operation failed

        INDX  = 2
        DELTA = NROWS3( FID )


        !!.......   Read the site ID list for this time step

        DIMS ( 1 ) = 1
        DELTS( 1 ) = NROWS3( FID )

        DIMS ( 2 ) = STEP
        DELTS( 2 ) = 1

!$OMP CRITICAL( S_NC )
        IERR = NF_GET_VARA_INT( ICDF, SINDX3( FID ), DIMS, DELTS, BUFFER( INDX ) )
!$OMP END CRITICAL( S_NC )
      IF ( IERR .NE. 0 ) THEN
            WRITE( MESG,'( A, I5 )' ) 'netCDF error number', IERR
            CALL M3MSG2( MESG )
            MESG = 'Error reading ID list for file '//FLIST3( FID )
            CALL M3WARN( 'READ3/RDGRNEST', 0, 0, MESG )
            RDGRNEST = .FALSE.
            RETURN
        END IF          !  ierr nonzero:  operation failed

        INDX = INDX + DELTA


       !!.......   Read the site profile-count list for this time step

!$OMP CRITICAL( S_NC )
        IERR = NF_GET_VARA_INT( ICDF, LINDX3( FID ), DIMS, DELTS, BUFFER( INDX ) )
!$OMP END CRITICAL( S_NC )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG,'( A, I5 )' ) 'netCDF error number', IERR
            CALL M3MSG2( MESG )
            MESG = 'Error reading profile count for file '// FLIST3( FID )
            CALL M3WARN( 'READ3/RDGRNEST', 0, 0, MESG )
            RDGRNEST = .FALSE.
            RETURN
        END IF          !  ierr nonzero:  operation failed

        INDX = INDX + DELTA


        !!.......   Read the site X,Y location lists for this time step

!$OMP CRITICAL( S_NC )
        IERR = NF_GET_VARA_DOUBLE( ICDF, XINDX3( FID ), DIMS, DELTS, BUFFER( INDX ) )
!$OMP END CRITICAL( S_NC )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG,'( A, I5 )' ) 'netCDF error number', IERR
            CALL M3MSG2( MESG )
            MESG = 'Error reading X coord for file '//FLIST3( FID )
            CALL M3WARN( 'READ3/RDGRNEST', 0, 0, MESG )
            RDGRNEST = .FALSE.
            RETURN
        END IF          !  ierr nonzero:  operation failed

        INDX = INDX + DELTA * DBLSIZE

!$OMP CRITICAL( S_NC )
        IERR = NF_GET_VARA_DOUBLE( ICDF, YINDX3( FID ), DIMS, DELTS, BUFFER( INDX ) )
!$OMP END CRITICAL( S_NC )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG,'( A, I5 )' ) 'netCDF error number', IERR
            CALL M3MSG2( MESG )
            MESG = 'Error reading Y coord for file '//FLIST3( FID )
            CALL M3WARN( 'READ3/RDGRNEST', 0, 0, MESG )
            RDGRNEST = .FALSE.
            RETURN
        END IF          !  ierr nonzero:  operation failed

        INDX = INDX + DELTA * DBLSIZE


    !!.......   Read the site cellsize DX,DY lists for this time step

!$OMP CRITICAL( S_NC )
        IERR = NF_GET_VARA_DOUBLE( ICDF, DXNDX3( FID ), DIMS, DELTS, BUFFER( INDX ) )
!$OMP END CRITICAL( S_NC )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG,'( A, I5 )' ) 'netCDF error number', IERR
            CALL M3MSG2( MESG )
            MESG = 'Error reading cell DX for file '//FLIST3( FID )
            CALL M3WARN( 'READ3/RDGRNEST', 0, 0, MESG )
            RDGRNEST = .FALSE.
            RETURN
        END IF          !  ierr nonzero:  operation failed

        INDX = INDX + DELTA * DBLSIZE

!$OMP CRITICAL( S_NC )
        IERR = NF_GET_VARA_DOUBLE( ICDF, DYNDX3( FID ), DIMS, DELTS, BUFFER( INDX ) )
!$OMP END CRITICAL( S_NC )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG,'( A, I5 )' ) 'netCDF error number', IERR
            CALL M3MSG2( MESG )
            MESG = 'Error reading cell DY for file '//FLIST3( FID )
            CALL M3WARN( 'READ3/RDGRNEST', 0, 0, MESG )
            RDGRNEST = .FALSE.
            RETURN
        END IF          !  ierr nonzero:  operation failed

        INDX = INDX + DELTA * DBLSIZE

    ELSE            !  fixup buffer-offset

        INDX = 1

    END IF          !  if netcdf file


    !!.......   Set up DIMS and DELTS arguments for NF_GET_VARA_*() according to
    !!.......   whether all layers are requested:

    DIMS ( 1 ) = 1
    DELTS( 1 ) = NCOLS3( FID )

    DIMS ( 2 ) = 1
    DELTS( 2 ) = NROWS3( FID )

    DIMS ( 4 ) = STEP
    DELTS( 4 ) = 1

    DIMS ( 5 ) = 0
    DELTS( 5 ) = 0

   IF ( LAYER .EQ. ALLAYS3 ) THEN

        DIMS ( 1 ) = 1
        DELTS( 1 ) = NCOLS3( FID )

        DIMS ( 2 ) = 1
        DELTS( 2 ) = NROWS3( FID )

        DIMS ( 3 ) = 1
        DELTS( 3 ) = NLAYS3( FID )

        DELTA = NCOLS3( FID ) * NROWS3( FID ) * NLAYS3( FID )

    ELSE    !  read a specific layer

        DIMS ( 3 ) = LAYER
        DELTS( 3 ) = 1

        DELTA = NCOLS3( FID ) * NROWS3( FID )

    END IF


    !!...........   Perform the reads, according to vid

    RDGRNEST = RDVARS( FID, VID, DIMS, DELTS, DELTA, BUFFER( INDX ) )

    RETURN


END FUNCTION RDGRNEST

