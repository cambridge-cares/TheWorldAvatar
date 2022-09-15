
LOGICAL FUNCTION SHUT3 ( )

    !!***********************************************************************
    !! Version "$Id: shut3.F90 1 2017-06-10 18:05:20Z coats $"
    !! EDSS/Models-3 I/O API.
    !! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    !! (C) 2003-2010 by Baron Advanced Meteorological Systems.
    !! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    !! See file "LGPL.txt" for conditions of use.
    !!.........................................................................
    !!  function body starts at line  70
    !!
    !!  FUNCTION:  Flushes and closes down all Models-3 files currently open.
    !!
    !!  RETURN VALUE:  TRUE iff it succeeds.
    !!
    !!  PRECONDITIONS REQUIRED:  none
    !!
    !!  SUBROUTINES AND FUNCTIONS CALLED:
    !!       netCDF2 interface to netCDF
    !!
    !!  REVISION  HISTORY:  
    !!       prototype 3/1992 by CJC
    !!
    !!       Modified  8/1995 by CJC to support CLOSE3()
    !!
    !!       Modified  5/1998 by CJC for OpenMP thread-safety
    !!
    !!       Modified  5/1999 by ALT for coupling-mode operation
    !!
    !!       Modified 10/2003 by CJC for I/O API version 3:  support for
    !!       native-binary BINFIL3 file type.
    !!
    !!       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !!
    !!       Modified 08/2015 by CJC:  free F90 format; support for 
    !!       MPI/PnetCDF distributed I/O; USE MODNCFIO, MPDPMPI
    !!***********************************************************************

    USE MODNCFIO
    USE MODPDATA

    IMPLICIT NONE

    !!...........   INCLUDES:

    INCLUDE 'PARMS3.EXT'
    INCLUDE 'STATE3.EXT'
#ifdef IOAPICPL
    INCLUDE 'STATE3V.EXT'
    LOGICAL, EXTERNAL :: SHUT3V
#endif


    !!...........   EXTERNAL FUNCTIONS and their descriptions:

    INTEGER, EXTERNAL :: CLOSEBIN3


    !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         FILE            !  loop counter over files
    INTEGER         V, FID          !  loop counter over vbles
    INTEGER         IERR            !  netCDF error status return

!!***********************************************************************
!!   begin body of function  SHUT3

    SHUT3 = .TRUE.
    IF ( .NOT. FINIT3 ) THEN
        RETURN
    END IF

#ifdef  IOAPICPL
    SHUT3 = SHUT3V()
#endif

!$OMP CRITICAL( S_NC )
    DO  FILE = 1 , COUNT3

        IF( FLIST3( FILE ) .NE. CMISS3 ) THEN

            FID = CDFID3( FILE )
            
            IF( FTYPE3( FILE ) .EQ. MPIGRD3 ) THEN

#ifdef IOAPI_PNCF
                IERR =  NFMPI_CLOSE( FID )
                IF ( IERR .NE. 0 ) THEN
                    WRITE( LOGDEV,91010 )               &
                    'Error closing PnetCDF file ',      &
                    'File name:  ' // FLIST3( FILE ) ,  &
                    'PnetCDF error number', IERR
                    SHUT3 = .FALSE.
                END IF
#endif

            ELSE IF( FID .GE. 0 ) THEN

                IERR =  NF_CLOSE( FID )
                IF ( IERR .NE. 0 ) THEN
                    WRITE( LOGDEV,91010 )               &
                    'Error closing netCDF file ',       &
                    'File name:  ' // FLIST3( FILE ) ,  &
                    'netCDF error number', IERR
                    SHUT3 = .FALSE.
                END IF

            ELSE IF( FID .EQ. BINFIL3 ) THEN

                IERR = CLOSEBIN3( FILE )
                IF ( IERR .EQ. 0 ) THEN
                    WRITE( LOGDEV,91010 )               &
                    'Error closing BINFIL3 file ',      &
                    'File name:  ' // FLIST3( FILE )
                    SHUT3 = .FALSE.
                END IF
            END IF

        END IF              !  if flist not "missing"

        CALL BUFDEL3( FILE )
        FLIST3( FILE ) = CMISS3
        NLIST3( FILE ) = 0
        IFRST3( FILE ) = IMISS3
        DO  V = 1, NVARS3( FILE )
            VLIST3( V,FILE ) = CMISS3
            ILAST3( V,FILE ) = IMISS3
            LDATE3( V,FILE ) = IMISS3
            LTIME3( V,FILE ) = IMISS3
            NDATE3( V,FILE ) = IMISS3
            NTIME3( V,FILE ) = IMISS3
        END DO

    END DO        !  end loop on files
!$OMP END CRITICAL( S_NC )

    COUNT3 = IMISS3
    ILCNT3 = 0
    FINIT3 = .FALSE.

    RETURN

    !!******************  FORMAT  STATEMENTS   ******************************

    !!...........   Error and warning message formats..... 91xxx

91010   FORMAT ( //5X , '>>> WARNING in subroutine SHUT3 <<<',  &
              3 ( /5X , A , : ) , I5, // )

END FUNCTION SHUT3

