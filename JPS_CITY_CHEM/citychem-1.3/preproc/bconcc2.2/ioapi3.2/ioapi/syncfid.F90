
RECURSIVE LOGICAL FUNCTION SYNCFID( FID ) RESULT( SFLAG )

    !!***********************************************************************
    !! Version "$Id: syncfid.F90 1 2017-06-10 18:05:20Z coats $"
    !! EDSS/Models-3 I/O API.
    !! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    !! (C) 2003-2010 by Baron Advanced Meteorological Systems, and
    !! Copyright (C) 2014-2015 UNC Institute for the Environment.
    !! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    !! See file "LGPL.txt" for conditions of use.
    !!.........................................................................
    !!  function  SYNCFID  starts at line  67
    !!
    !!  FUNCTION:
    !!      Flushes/syncs I/O API file with STATE3-index FID
    !!
    !!  RETURN VALUE:
    !!      TRUE iff it succeeds.
    !!
    !!  PRECONDITIONS REQUIRED:
    !!      FNAME exists and has been opened:  FID is a STATE3-arrays subscript
    !!
    !!  SUBROUTINES AND FUNCTIONS CALLED:
    !!      netCDF; FLUSHBIN3
    !!
    !!  REVISION  HISTORY:  
    !!      Prototype 10/2003 by CJC for I/O API version 3
    !!
    !!      Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !!
    !!      Modified 08/2015 by CJC for I/O API 3.2:  F90 free format;
    !!      support for MPI/PnetCDF; recursion for LIST-files
    !!***********************************************************************

    USE MODNCFIO
    USE MODPDATA

    IMPLICIT NONE

     !!...........   INCLUDES:

    INCLUDE 'PARMS3.EXT'
    INCLUDE 'STATE3.EXT'

    !!...........   ARGUMENT, RESULT, and their descriptions:

    INTEGER, INTENT(IN   ) :: FID             !  STATE3-index for the file


    !!...........   EXTERNAL FUNCTIONS and their descriptions:

    INTEGER, EXTERNAL :: FLUSHBIN3  !  sync for BINIO3 files


    !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         IERR, N, F
    LOGICAL         EFLAG
    CHARACTER*16    FNAME
    CHARACTER*256   MESG


    !!***********************************************************************
    !!   begin body of function  SYNCFID

    IF ( CDFID3( FID ) .EQ. BINFIL3 ) THEN

!$OMP       CRITICAL( S_NC )
            IF ( 0 .EQ. FLUSHBIN3( FID ) ) THEN
                FNAME = FLIST3( FID )             
                MESG  = 'Error flushing BINIO3 file "' // TRIM( FNAME ) // '"'
                CALL M3MSG2( MESG )
                SFLAG = .FALSE.
            ELSE
                SFLAG = .TRUE.
            END IF
!$OMP       END CRITICAL( S_NC )

        ELSE IF( FTYPE3( FID ) .EQ. MPIGRD3 ) THEN

#ifndef IOAPI_PNCF
            CALL M3MESG( 'SYNCFID:  MP:I/PnetCDF not supported for this build' )
            SFLAG = .FALSE.
            RETURN
#endif

#ifdef  IOAPI_PNCF

            IF ( PN_IO_PE ) THEN
!$OMP           CRITICAL( S_NC )
                IERR = NFMPI_SYNC( CDFID3( FID ) )
!$OMP           END CRITICAL( S_NC )
                IF ( IERR .NE. 0 ) THEN
                    FNAME = FLIST3( FID )
                    MESG  = 'Error flushing PnetCDF file "' // TRIM( FNAME ) // '"'
                    CALL M3MSG2( MESG )
                    WRITE( MESG, '( A, I9 )' )  'PnetCDF error number', IERR
                    CALL M3MSG2( MESG )
                    SFLAG = .FALSE.
                ELSE
                    SFLAG = .TRUE.
                END IF      !  if ierr nonzero
            ELSE
                SFLAG = .TRUE.
            END IF      !  if ierr nonzero

            IF ( .NOT.PN_FLAG( SFLAG ) ) THEN
                CALL M3MSG2( 'WRTFLAG:  MPI_SEND(SFLAG) error' )
                SFLAG = .FALSE.          
            END IF

#endif

        ELSE IF( CDFID3( FID ) .GE. 0 ) THEN

!$OMP       CRITICAL( S_NC )
            IERR = NF_SYNC( CDFID3( FID ) )
!$OMP       END CRITICAL( S_NC )
            IF ( IERR .NE. 0 ) THEN
                FNAME = FLIST3( FID )
                MESG  = 'Error flushing netCDF file "' // TRIM( FNAME ) // '"'
                CALL M3MSG2( MESG )
                WRITE( MESG, '( A, I9 )' )  'netCDF error number', IERR
                CALL M3MSG2( MESG )
                SFLAG = .FALSE.
            ELSE
                SFLAG = .TRUE.
            END IF      !  if ierr nonzero

        ELSE IF ( CDFID3( FID ) .EQ. LSTFIL3 ) THEN     !! iterate syncfid() over list-entries

            EFLAG = .FALSE.
            DO  N = 1, NLIST3( FID )
                F = ILIST3( N )
                IF ( .NOT. SYNCFID( CDFID3( F ) ) ) SFLAG = .FALSE.
            END DO

            IF ( EFLAG ) THEN
                SFLAG = .FALSE.
                FNAME = FLIST3( FID )
                MESG  = 'Error flushing list file "' // TRIM( FNAME ) // '"'
                CALL M3MSG2( MESG )
            ELSE
                SFLAG = .TRUE.
            END IF

        ELSE

            SFLAG = .TRUE.

        END IF  ! if cdfid(FID) positive, or =buffil3, or listfil3, or not

        RETURN

END FUNCTION SYNCFID
        
