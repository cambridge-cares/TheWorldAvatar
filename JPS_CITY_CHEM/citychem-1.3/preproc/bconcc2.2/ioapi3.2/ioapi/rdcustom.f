
        LOGICAL FUNCTION RDCUSTOM( FID, VID, LAYER, STEP, BUFFER )

C***********************************************************************
C Version "$Id: rdcustom.f 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC, (C) 1992-2012 Carlie J. Coats, Jr.,
C (C) 2003-2011 Baron Advanced Meteorological Systems, and 
C (C) 2015 UNC Institute for the Environment
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  80
C
C  FUNCTION:  reads data from Models-3 CUSTOM data file with state-variable
C             file index FID, for variable VID and layer LAYER, for the
C             time step record STEP.
C             If VID is -1, reads all variables; if LAYER is -1,
C             reads all layers.
C
C  RETURN VALUE:  TRUE iff the operation succeeds (and the data is available)
C
C  PRECONDITIONS REQUIRED:  Should only be called by READ3(), after it
C             has checked for file, time step, and layer availability,
C             and that file type is CUSTOM3.
C
C  SUBROUTINES AND FUNCTIONS CALLED:  RDVARS
C
C  REVISION  HISTORY:  
C	prototype 3/1992 by CJC
C
C       modified  9/1994 by CJC:  argument is now VID, not VNAME
C
C       Modified 10/2003 by CJC for I/O API version 3:  support for
C       native-binary BINFIL3 file type; uses INTEGER NAME2FID
C
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C
C       Modified 08/2015 by CJC: USE MODNCFIO for I/O API v3.2
C***********************************************************************

      USE MODNCFIO

      IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'STATE3.EXT'


C...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: FID             !  file index within the STATE3 commons
        INTEGER, INTENT(IN   ) :: VID             !  variable index, or 0
        INTEGER, INTENT(IN   ) :: LAYER           !  layer number,   or 0
        INTEGER, INTENT(IN   ) :: STEP            !  time step record number
        REAL   , INTENT(  OUT) :: BUFFER(*)       !  buffer array for input


C...........   EXTERNAL FUNCTIONS and their descriptions:

        LOGICAL, EXTERNAL :: RDVARS     !  read "variables" part of timestep records
        EXTERNAL          :: INITBLK3   !  block data: initialize I/O state


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         DELTA           !  d(INDX) / d(NF_GET_VARA_* call)
        INTEGER         DIMS ( 5 )      !  corner arg array for NF_GET_VARA_*()
        INTEGER         DELTS( 5 )      !  corner arg array for NF_GET_VARA_*()


C***********************************************************************
C   begin body of function  RDCUSTOM

C.......   Set up DIMS and DELTS arguments for NCVGT() according
C.......   to whether request is for all layers or not:

        DIMS ( 1 ) = 1
        DELTS( 1 ) = NCOLS3( FID )

        DIMS ( 3 ) = STEP
        DELTS( 3 ) = 1

        IF ( LAYER .EQ. ALLAYS3 ) THEN

            DIMS ( 2 ) = 1
            DELTS( 2 ) = NLAYS3( FID )

            DELTA = NCOLS3( FID ) * NLAYS3( FID )

        ELSE

            DIMS ( 2 ) = LAYER
            DELTS( 2 ) = 1

            DELTA = NCOLS3( FID )

        END IF

        DIMS ( 4 ) = 0
        DELTS( 4 ) = 0

        DIMS ( 5 ) = 0
        DELTS( 5 ) = 0


C...........   Perform the reads, according to VNAME

        RDCUSTOM = RDVARS( FID, VID, DIMS, DELTS, DELTA, BUFFER )

        RETURN

        END FUNCTION RDCUSTOM

