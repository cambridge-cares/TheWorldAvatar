
LOGICAL FUNCTION WRTFLAG( FID, VID, FLAGS, STEP2 )

    !!***********************************************************************
    !! Version "$Id: wrtflag.F90 1 2017-06-10 18:05:20Z coats $"
    !! EDSS/Models-3 I/O API.
    !! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    !! (C) 2003-2010 by Baron Advanced Meteorological Systems,
    !! (C) 2007-2013 Carlie J. Coats, Jr., and
    !! (C) 2015 UNC Institute for the Environment.
    !! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    !! See file "LGPL.txt" for conditions of use.
    !!.........................................................................
    !!  function body starts at line  89
    !!
    !!  FUNCTION:
    !!      writes "time-step flag" part of time step records from Models-3
    !!      files with index FID, for all layers and variables for routine
    !!      WRVARS and PWRITE3
    !!
    !!  RETURN VALUE:  TRUE iff the operation succeeds
    !!
    !!  PRECONDITIONS REQUIRED:
    !!      Should only be called by the above routines after OPEN3() has
    !!      checked for file and time step availability.
    !!
    !!  SUBROUTINES AND FUNCTIONS CALLED:
    !!      (netCDF)
    !!
    !!  REVISION  HISTORY:
    !!      Adapted 3/2002 by Carlie J. Coats, Jr., MCNC-EMC from
    !!      timestep-flag parts of WRVARS()
    !!
    !!      Modified 5/2003 by CJC:  additional error-logging messages
    !!
    !!      Modified 10/2003 by CJC for I/O API version 3:  support for
    !!      native-binary BINFILE3 file type
    !!
    !!      Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !!
    !!      Modified 08/2010 by CJC: F90 "free" source format;
    !!      support for MPI/PnetCDF MPIGRD3 files; USE MODNCFIO, MODPDATA
    !!***********************************************************************

    USE MODNCFIO
    USE MODPDATA

    IMPLICIT NONE

    !!...........   INCLUDES:

    INCLUDE 'PARMS3.EXT'
    INCLUDE 'STATE3.EXT'

    !!...........   ARGUMENTS and their descriptions:

    INTEGER, INTENT(IN   ) :: FID             !  file index within the STATE3 commons
    INTEGER, INTENT(IN   ) :: VID             !  vble index within the STATE3 commons
    INTEGER, INTENT(IN   ) :: FLAGS( 2 )      !  ( jdate yyyyddd, jtime hhmmss )
    INTEGER, INTENT(IN   ) :: STEP2           !  physical time step number (maybe mod 2)


    !!...........   EXTERNAL FUNCTIONS and their descriptions:

    INTEGER, EXTERNAL :: WRBFLAG         !  for BINFIL3 files


    !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         IERR            !  netCDF error status return
    LOGICAL         EFLAG, SFLAG

#ifdef IOAPI_PNCF
    INCLUDE 'mpif.h'

    INTEGER( 8 ) :: DIMP( 5 )       !  corner:    FLAGS
    INTEGER( 8 ) :: DELP( 5 )       !  diagonal:  FLAGS

!!  INTEGER( MPI_OFFSET_KIND ) :: DIMP( 5 )       !  corner:    FLAGS
!!  INTEGER( MPI_OFFSET_KIND ) :: DELP( 5 )       !  diagonal:  FLAGS
#endif
    INTEGER         DIMT( 5 )       !  corner:    FLAGS
    INTEGER         DELT( 5 )       !  diagonal:  FLAGS

    CHARACTER*256   MESG
    CHARACTER*80    PARAG( 9 )


    !!***********************************************************************
    !!   begin body of function  WRTFLAG

    IF ( VID .GT. 0 ) THEN          !  write time flag for just one variable

        DIMT( 2 ) = VID    !  starting variable

    ELSE IF ( NVARS3( FID ) .EQ. 0 ) THEN	!  write all-vars timestep flag for this case

        DIMT( 2 ) = 1           !  variable-number

    ELSE

        MESG = 'Invalid call to WRTFLAG() for file ' // FLIST3(FID)
        CALL M3MSG2( MESG )
        WRTFLAG = .FALSE.
        RETURN

    END IF                  !  if writing just one vble, or all vbles

    !!...........   Deal with case of BINIO3 file:

    IF ( CDFID3( FID ) .EQ. BINFIL3 ) THEN

!$OMP CRITICAL( S_NC )

        IERR = WRBFLAG( FID, VID, STEP2, FLAGS )

!$OMP END CRITICAL( S_NC )

        IF ( IERR .EQ. 0 ) THEN
            WRTFLAG = .FALSE.
            MESG    = 'WRTFLAG:  Error writing time-flag for vble ' &
                // VLIST3( VID,FID ) // ' to file ' // FLIST3( FID )
            CALL M3MSG2( MESG )
        ELSE
            WRTFLAG = .TRUE.
        END IF

        RETURN

   END IF   !  if BINFIL3

    !!...........   Deal with netCDF file:  set up hyperslab for TFLAG

    DIMT( 1 ) = 1           !  field:  date or time
    DELT( 1 ) = 2           !  extent:  both date and time
    DIMT( 3 ) = STEP2       !  time start
    DELT( 3 ) = 1           !  time extent:  1

    DELT( 2 ) = 1           !  extent:  1
    
    EFLAG = .FALSE.
    IERR  = 0

    IF ( FTYPE3( FID ) .EQ. MPIGRD3 ) THEN

#ifdef  IOAPI_PNCF
!$OMP   CRITICAL( S_NC )

        IF ( PN_IO_PE ) THEN
            DIMP = DIMT
            DELP = DELT
            IERR = NFMPI_PUT_VARA_INT( CDFID3( FID ), TINDX3( FID ), DIMP, DELP, FLAGS )
        END IF

!$OMP   END CRITICAL( S_NC )
#endif

    ELSE IF ( CDFID3( FID ) .GE. 0 ) THEN

!$OMP   CRITICAL( S_NC )

        IERR = NF_PUT_VARA_INT( CDFID3( FID ), TINDX3( FID ), DIMT, DELT, FLAGS )

!$OMP   END CRITICAL( S_NC )

    ELSE

        EFLAG = .TRUE.
        MESG  = 'WRTFLAG:  invalid file type for '// FLIST3( FID )
        CALL M3MSG2( MESG )

    END IF

    SFLAG = ( .NOT.EFLAG )
    IF ( FTYPE3( FID ) .EQ. MPIGRD3 ) THEN
        IF ( .NOT.PN_FLAG( SFLAG ) ) THEN
            CALL M3MSG2( 'WRTFLAG:  MPI_SEND(EFLAG) error' )
            SFLAG = .FALSE.          
        END IF
    END IF

    IF ( IERR .NE. 0 ) THEN
        PARAG( 1 ) = 'WRTFLAG:  Error writing time-flag for file/variable'
        PARAG( 2 ) = 'File "' // FLIST3( FID ) // '", variable "' // VLIST3( VID,FID ) // '"'
        WRITE( PARAG( 3 ), * ) 'netCDF error number', IERR
        WRITE( PARAG( 4 ), * ) 'IOAPI file ID ', FID
        WRITE( PARAG( 5 ), * ) 'netCDF ID     ', CDFID3( FID )
        WRITE( PARAG( 6 ), * ) 'variable      ', VINDX3( VID,FID )
        WRITE( PARAG( 7 ), * ) 'dims array    ', DIMT
        WRITE( PARAG( 8 ), * ) 'delts array   ', DELT
        PARAG( 9 ) = ' '
        CALL M3PARAG( 9, PARAG )
    END IF

    WRTFLAG = SFLAG

    RETURN

END FUNCTION WRTFLAG

