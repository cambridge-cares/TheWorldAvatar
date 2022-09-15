
LOGICAL FUNCTION CLOSE3( FNAME )

    !!***********************************************************************
    !!Version "$Id: close3.F90 1 2017-06-10 18:05:20Z coats $"
    !!EDSS/Models-3 I/O API.
    !! Copyright (C) 1992-2002 MCNC, (C) 1992-2012 Carlie J. Coats, Jr.,
    !! (C) 2003-2011 Baron Advanced Meteorological Systems, and 
    !! (C) 2015 UNC Institute for the Environment
    !!Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    !!See file "LGPL.txt" for conditions of use.
    !!.........................................................................
    !! function body starts at line  80
    !!
    !! FUNCTION:
    !!      Flushes and closes down file with logical name FNAME
    !!
    !! RETURN VALUE:
    !!      TRUE iff it succeeds.
    !!
    !! PRECONDITIONS REQUIRED:
    !!
    !!  **  NOT TO BE USED IN MODELING CODES  **
    !!  **  by order of Joan Novak, EPA ORD, and Ed Bilicki, MCNC EMC **
    !!
    !!      FNAME is not virtual, does exist, and has been opened
    !!
    !! SUBROUTINES AND FUNCTIONS CALLED:
    !!      CLOSEBIN3, M3MSG2, NAME2FID
    !!
    !! REVISION  HISTORY:  
    !!      prototype 8/1995 by CJC
    !!
    !!      Modified  1/2000 by CJC for OpenMP thread-safety
    !!
    !!      Modified 10/2003 by CJC for I/O API version 3:  support for
    !!      native-binary BINFIL3 file type; use NAME2FID
    !!
    !!      Modified 03/20010 by CJC: F9x changes for I/O API v3.1
    !!
    !!      Modified 08/2015 by CJC for I/O API 3.2: MPI/PnetCDF support;
    !!      USE MODNCFIO, MODPDATA, NF_CLOSE() instead of NCCLOS()
    !!***********************************************************************

    USE MODNCFIO
    USE MODPDATA

    IMPLICIT NONE

    !!...........   INCLUDES:

    INCLUDE 'PARMS3.EXT'
    INCLUDE 'STATE3.EXT'


    !!...........   ARGUMENTS and their descriptions:

    CHARACTER*(*), INTENT(IN   ) :: FNAME   !  logical name of file to be closed


    !!...........   EXTERNAL FUNCTIONS and their descriptions:

    INTEGER, EXTERNAL :: CLOSEBIN3       !  for BINFIL3 files
    INTEGER, EXTERNAL :: NAME2FID        !  fname ~~> fid lookup


    !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         FILE            !  file index
    INTEGER         V               !  vble index
    INTEGER         IERR            !  netCDF error status return
    CHARACTER*256   MESG
    LOGICAL         EFLAG


    !!***********************************************************************
    !!  begin body of function  CLOSE3
    !!.......   Find STATE3 index for the file:

    FILE = NAME2FID( FNAME )
    IF ( FILE .EQ. 0 ) THEN !  file not open.
        MESG = 'CLOSE3:  File "' // FNAME // '" not currently open'
        CALL M3MSG2( MESG )
        CLOSE3 = .FALSE.
        RETURN
    END IF

    EFLAG = .FALSE.

!$OMP CRITICAL( S_NC )

    IF( FTYPE3( FILE ) .EQ. MPIGRD3 .AND. PN_IO_PE ) THEN

#ifdef IOAPI_PNCF
        IERR =  NFMPI_CLOSE( CDFID3( FILE ) )
        IF ( IERR .NE. 0 ) THEN
            MESG =  'Error closing PnetCDF file "' // FNAME // '"'
            CALL M3MSG2( MESG )
            WRITE( LOGDEV,91010 )  'netCDF error number', IERR, MESG
            EFLAG = .TRUE.
        END IF      !  if ierr nonzero
#endif
#ifndef IOAPI_PNCF
        CALL M3MESG( 'PnetCDF not supported in this build' )
        EFLAG = .TRUE.
#endif

    ELSE IF( CDFID3( FILE ) .GE. 0 ) THEN

        IERR = NF_CLOSE( CDFID3( FILE ) )
        IF ( IERR .NE. 0 ) THEN
            MESG =  'Error closing netCDF file "' // FNAME // '"'
            CALL M3MSG2( MESG )
            WRITE( LOGDEV,91010 )  'netCDF error number', IERR, MESG
            EFLAG = .TRUE.
        END IF      !  if ierr nonzero

    ELSE IF ( CDFID3( FILE ) .EQ. BINFIL3 ) THEN

        IF ( 0 .EQ. CLOSEBIN3( FILE ) ) THEN
            MESG =  'Error closing BINFIL3 file "' // FNAME // '"'
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF

    END IF          !  if cdfid(file) positive or not

!$OMP END CRITICAL( S_NC )

    IF ( EFLAG ) THEN
        CLOSE3 = .FALSE.
        RETURN
    END IF

    CALL BUFDEL3( FILE )
    FLIST3( FILE ) = CMISS3
    FTYPE3( FILE ) = IMISS3
    DO  V = 1, NVARS3( FILE )
        VLIST3( V,FILE ) = CMISS3
        ILAST3( V,FILE ) = IMISS3
        LDATE3( V,FILE ) = IMISS3
        LTIME3( V,FILE ) = IMISS3
        NDATE3( V,FILE ) = IMISS3
        NTIME3( V,FILE ) = IMISS3
    END DO

    MESG = 'Closing file ' // FNAME
    CALL M3MSG2( MESG )
    CLOSE3 = .TRUE.

    RETURN

    !!******************  FORMAT  STATEMENTS   ******************************

    !!...........   Error and warning message formats..... 91xxx

91010   FORMAT ( //5X , '>>> WARNING in subroutine CLOSE3 <<<',     &
              3 ( /5X , A , : ) , I5, // )


END FUNCTION CLOSE3

