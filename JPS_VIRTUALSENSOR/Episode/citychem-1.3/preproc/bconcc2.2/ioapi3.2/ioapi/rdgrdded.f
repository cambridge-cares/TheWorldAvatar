
        LOGICAL FUNCTION RDGRDDED( FID, VID, LAYER, STEP, BUFFER )

C***********************************************************************
C Version "$Id: rdgrdded.f 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC, (C) 1992-2012 Carlie J. Coats, Jr.,
C (C) 2003-2011 Baron Advanced Meteorological Systems, and 
C (C) 2015 UNC Institute for the Environment
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  76
C
C  FUNCTION:  reads data from Models-3 GRDDED data file with state-variable
C             index FID, for variable with name VID and layer LAYER, for the
C             time step record STEP.
C             If VID is -1 reads all variables; if LAYER is -1,
C             reads all layers.
C
C  RETURN VALUE:  TRUE iff the operation succeeds (and the data is available)
C
C  PRECONDITIONS REQUIRED:  Should only be called by READ3(), after it
C             has checked for file and time step availability, and that
C             file type is GRDDED3.
C
C  SUBROUTINES AND FUNCTIONS CALLED:  RDVARS
C
C  REVISION  HISTORY:  
C	prototype 3/1992 by CJC
C
C	Modified  9/1994 by CJC:  argument VID not VNAME 
C
C       Modified 10/2003 by CJC for I/O API version 3:  support for
C       native-binary BINFIL3 file type; uses INTEGER NAME2FID
C
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'STATE3.EXT'


C...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: FID             !  file index within the STATE3 commons
        INTEGER, INTENT(IN   ) :: VID             !  variable index  or ALLAYS3
        INTEGER, INTENT(IN   ) :: LAYER           !  layer number, or -1 = ALLAYS3
        INTEGER, INTENT(IN   ) :: STEP            !  time step record number
        REAL   , INTENT(  OUT) :: BUFFER(*)       !  buffer array for input


C...........   EXTERNAL FUNCTIONS and their descriptions:

        LOGICAL, EXTERNAL :: RDVARS     !  read "variables" part of timestep records
        EXTERNAL          :: INITBLK3   !  block data: initialize I/O state


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         DELTA           !  d(INDX) / d(NF_GET_VARA call)
        INTEGER         DIMS ( 5 )      !  corner arg array for NF_GET_VARA()
        INTEGER         DELTS( 5 )      !  corner arg array for NF_GET_VARA()


C***********************************************************************
C   begin body of function  RDGRDDED

C.......   Set up DIMS and DELTS arguments for NCVGT() according to
C.......   whether request is for all layers:

        DIMS ( 1 ) = 1
        DELTS( 1 ) = NCOLS3( FID )

        DIMS ( 2 ) = 1
        DELTS( 2 ) = NROWS3( FID )

        DIMS ( 4 ) = STEP
        DELTS( 4 ) = 1

        DIMS ( 5 ) = 0
        DELTS( 5 ) = 0

        IF ( LAYER .EQ. ALLAYS3 ) THEN

            DIMS ( 3 ) = 1
            DELTS( 3 ) = NLAYS3( FID )

            DELTA = NCOLS3( FID ) * NROWS3( FID ) * NLAYS3( FID )

        ELSE    !  read a specific layer:

            DIMS ( 3 ) = LAYER
            DELTS( 3 ) = 1

            DELTA = NCOLS3( FID ) * NROWS3( FID )

        END IF          !  if layer == alllays or not


C...........   Perform the reads, according to VID

        RDGRDDED = RDVARS( FID, VID, DIMS, DELTS, DELTA, BUFFER )

        RETURN


        END FUNCTION RDGRDDED

