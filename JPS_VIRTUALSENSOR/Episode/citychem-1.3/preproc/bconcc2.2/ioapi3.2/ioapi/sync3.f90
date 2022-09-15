
LOGICAL FUNCTION SYNC3( FNAME )

    !!***********************************************************************
    !! Version "$Id: sync3.f90 1 2017-06-10 18:05:20Z coats $"
    !! EDSS/Models-3 I/O API.
    !! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    !! (C) 2003-2013 Baron Advanced Meteorological Systems,
    !! (C) 2007-2013 Carlie J. Coats, Jr., and
    !! (C) 2014-2015 UNC Institute for the Environment.
    !! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    !! See file "LGPL.txt" for conditions of use.
    !!.........................................................................
    !!  function body starts at line  68
    !!
    !!  FUNCTION:
    !!      Performs disk synchronization for file FNAME
    !!
    !!  RETURN VALUE:
    !!      TRUE iff the operation succeeds (and the data is available)
    !!
    !!  PRECONDITIONS REQUIRED:
    !!      FNAME is a netCDF-based Models-3 data file already opened by OPEN3()
    !!
    !!  REVISION  HISTORY:  
    !!      prototype 3/2002 by Carlie J. Coats, Jr., MCNC-EMC
    !!
    !!      Modified 7/2003 by CJC:  bugfix -- clean up critical sections
    !!      associated with INIT3()
    !!
    !!      Modified 7/2007 by CJC:  bugfix -- format at line 120
    !!
    !!      Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !!
    !!      Modified 08/2015 by CJC for I/O API 3.2:  factor through SYNCFID():
    !!      support for MPI/PnetCDF
    !!***********************************************************************

    IMPLICIT NONE

    !!...........   INCLUDES:

    INCLUDE 'PARMS3.EXT'      ! I/O API constants
    INCLUDE 'STATE3.EXT'      ! I/O API internal state

    !!...........   ARGUMENTS and their descriptions:

    CHARACTER*(*), INTENT(IN   ) :: FNAME           !  logical file name

    !!...........   EXTERNAL FUNCTIONS and their descriptions:

    INTEGER, EXTERNAL :: INIT3      !  initialize I/O API
    INTEGER, EXTERNAL :: NAME2FID   !  fname~~> fid lookup
    LOGICAL, EXTERNAL :: SYNCFID    !  performs sync on this file

    !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         FID             !  subscript  for STATE3 arrays
    INTEGER         IERR            !  netCDF status return
    LOGICAL         EFLAG
    CHARACTER*16    FIL16           !  scratch file-name buffer
    CHARACTER*256   MESG

    !!***********************************************************************
    !!   begin body of function  SYNC3
    !!.......   Check that Models-3 I/O has been initialized:

    EFLAG = .FALSE.
!$OMP   CRITICAL( S_INIT )
    IF ( .NOT. FINIT3 ) THEN
        LOGDEV = INIT3()
        EFLAG = .TRUE.
    END IF          !  if not FINIT3
!$OMP   END CRITICAL( S_INIT )
    IF ( EFLAG ) THEN
        SYNC3 = .FALSE.
        CALL M3MSG2(  'SYNC3:  I/O API not yet initialized:  no files yet open.' )
        RETURN
    END IF

    !!...........   Check length of name arguments; copy into length=16 buffers

    FID = NAME2FID( FNAME )

    IF ( FID .EQ. 0 ) THEN  !  file not available

        MESG = 'File:  '//FIL16// ' not yet opened.'
        CALL M3WARN( 'SYNC3', 0,0, MESG )
        SYNC3 = .FALSE.

    ELSE            !  not a netCDF file...default TRUE

        SYNC3 = SYNCFID( FID )

    END IF          !  if file not available, or if file is volatile

    RETURN

END FUNCTION SYNC3


        
