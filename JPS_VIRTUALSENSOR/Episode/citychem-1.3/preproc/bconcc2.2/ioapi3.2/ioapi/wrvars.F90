
LOGICAL FUNCTION WRVARS( FID, VID, FLAGS, STEP2, DIMS, DELS, DELTA, BUFFER )    &
                 RESULT( WRFLAG )

    !!***********************************************************************
    !! Version "$Id: wrvars.F90 1 2017-06-10 18:05:20Z coats $"
    !! EDSS/Models-3 I/O API.
    !! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    !! (C) 2003-2010 Baron Advanced Meteorological Systems,
    !! (C) 2007-2013 Carlie J. Coats, Jr., and
    !! (C) 2015 UNC Institute for the Environment.
    !! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    !! See file "LGPL.txt" for conditions of use.
    !!.........................................................................
    !!  function body starts at line  110
    !!
    !!  FUNCTION:  writes "variables" part of time step records from Models-3
    !!             files with index FID, for all layers and variables for
    !!             routines WRCUSTOM, WRGWRDED, WRBNDARY, WRIDDATA, WRPROFIL,
    !!             and WRGRNEST.
    !!
    !!  MACHINE DEPENDENCY:
    !!       TYPSIZE( 6 ) must be sizeof( double )/sizeof( real )
    !!
    !!  RETURN VALUE:  TRUE iff the operation succeeds
    !!
    !!  PRECONDITIONS REQUIRED:  Should only be called by the above routines,
    !!             after OPEN3() has checked for file and time step availability,
    !!             and after the above routines have set up DIMS, , and DELTA.
    !!
    !!  SUBROUTINES AND FUNCTIONS CALLED:
    !!       WRTFLAG, (netCDF)
    !!
    !!  REVISION  HISTORY:
    !!      prototype 3/1992 by CJC
    !!
    !!      modified 10/1994 by CJC:  writes STEP number instead of OKFLAG3;
    !!      supports WRITE3() operations with single-variable granularity
    !!
    !!      Modified 2/1997  by CJC for OpenMP:  make it thread-safe to
    !!      call WRITE3 on distinct files in task-parallel.
    !!
    !!      Modified  9/1999 by CJC for new portability conventions
    !!      Modified  3/2002 by CJC for new CMAQ functionality:  uses WRTFLAG()
    !!
    !!      Modified 10/2003 by CJC for I/O API version 3:
    !!      Structure in terms of LOGICAL SYNCFID
    !!      support for native-binary BINFILE3 and LISTFIL3 file types
    !!
    !!      Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !!
    !!      Modified 02-08/2015 by CJC for I/O API 3.2: Support for M3INT8,
    !!      USE M3UTILIO, MODNCFIO.  Factor error messages through CONTAINed
    !!      subroutine LOGERR()
    !!
    !!      Modified 10/2015 by CJC for I/O API 3.2: use NF_*() instead of NC*()
    !!      for netCDF-Fortran 4.x compatibility; F90 free source format.
    !!***********************************************************************

    USE M3UTILIO
    USE MODNCFIO

    IMPLICIT NONE

    !!...........   INCLUDES:

    INCLUDE 'STATE3.EXT'


    !!...........   ARGUMENTS and their descriptions:

    INTEGER, INTENT(IN   ) :: FID             !  file index within the STATE3 commons
    INTEGER, INTENT(IN   ) :: VID             !  vble index within the STATE3 commons
    INTEGER, INTENT(IN   ) :: FLAGS( 2 )      !  ( jdate yyyyddd, jtime hhmmss )
    INTEGER, INTENT(IN   ) :: STEP2           !  physical time step number (maybe mod 2)
    INTEGER, INTENT(IN   ) :: DIMS( 5 )       !  corner arg array for NF_PUT_VARA*()
    INTEGER, INTENT(IN   ) :: DELS( 5 )       !  diag   arg array for NF_PUT_VARA*()
    INTEGER, INTENT(IN   ) :: DELTA           !  d(INDX) / d(NCVGTcall)
    REAL   , INTENT(IN   ) :: BUFFER(*)       !  buffer array for input


    !!...........   EXTERNAL FUNCTIONs:

    INTEGER, EXTERNAL :: WRBFLAG, WRBVARS
    LOGICAL, EXTERNAL :: WRTFLAG, SYNCFID


    !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER       ICDF            !  netCDF ID
    INTEGER       IERR            !  netCDF error status return
    INTEGER       VAR             !  loop counter for file variables
    INTEGER       INDX            !  subscript location in BUFFER(*)
    INTEGER       VTYPE, VINDX    !  variable-type, netCDF vble-ID
    LOGICAL       AFLAG
    CHARACTER*256 MESG

#if _CRAY || REAL8
    INTEGER, PARAMETER :: TYPSIZE( 10 ) = (/ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 /)
#endif
#if ! ( _CRAY || REAL8 )
    INTEGER, PARAMETER :: TYPSIZE( 10 ) = (/ 1, 1, 1, 1, 1, 2, 1, 1, 1, 2 /)
#endif


    !!***********************************************************************
    !!   begin body of function  WRVARS

    ICDF  = CDFID3( FID )

    !!...............   Process native-binary-layer files:

    IF ( ICDF  .EQ. BINFIL3 ) THEN    ! native-binary file

!$OMP CRITICAL( S_NC )
        IERR = WRBVARS( FID, VID, STEP2, BUFFER )
        IF ( 0 .EQ. IERR ) THEN     !  error
            AFLAG = .TRUE.
        ELSE IF ( 0 .EQ. WRBFLAG( FID, VID, STEP2, FLAGS ) ) THEN
            AFLAG = .TRUE.
        ELSE
            AFLAG = .FALSE.
        END IF
!$OMP END CRITICAL( S_NC )

        IF ( AFLAG ) THEN
!$OMP CRITICAL( S_LOGOUT )
            WRITE( LOGDEV,'( 5X, A )' ) '>>> WARNING in subroutine WRVARS <<<'
            IF ( VID .GT. 0 ) THEN
                WRITE( LOGDEV, '( 5X, 5 A, /, 5X, A, I6 )' )        &
                    'Error writing variable ', VLIST3( VID,FID ),   &
                    'to BINIO file ', FLIST3( FID ),                &
                    'variable  ID  ', VINDX3( VID,FID )
            ELSE
                WRITE( LOGDEV,'( 5X, 2 A )' )                       &
                    'Error writing ALL-VARIABLES to BINIO file ', FLIST3( FID )
            END IF
            WRITE( LOGDEV,'( 5X, A, I6 )' ) 'IOAPI file ID ', FID
            WRITE( LOGDEV,'( 5X, A, 5 I6 )' ) 'dims array  ', DIMS
            WRITE( LOGDEV,'( 5X, A, 5 I6 )' ) 'delts array ', DELS
            WRITE( LOGDEV,* )
!$OMP END CRITICAL( S_LOGOUT )
            WRFLAG = .FALSE.
        ELSE
            WRFLAG = .TRUE.
        END IF

        RETURN

    END IF                          !  if native-binary file

    IF ( VID .GT. 0 ) THEN          !  write just one variable

        VTYPE = VTYPE3( VID,FID )
        VINDX = VINDX3( VID,FID )

        !!...............   Write contents of the buffer for this variable

!$OMP CRITICAL( S_NC )
        IF ( VTYPE .EQ. M3INT ) THEN
            IERR  = NF_PUT_VARA_INT(    ICDF, VINDX, DIMS, DELS, BUFFER )
        ELSE IF ( VTYPE .EQ. M3REAL ) THEN
            IERR  = NF_PUT_VARA_REAL(   ICDF, VINDX, DIMS, DELS, BUFFER )
        ELSE IF ( VTYPE .EQ. M3DBLE ) THEN
            IERR  = NF_PUT_VARA_DOUBLE( ICDF, VINDX, DIMS, DELS, BUFFER )
        ELSE IF ( VTYPE .EQ. M3INT8 ) THEN
            IERR  = NF_PUT_VARA_INT64(  ICDF, VINDX, DIMS, DELS, BUFFER )
        ELSE
            IERR = NF_EBADTYPE
        END IF
!$OMP END CRITICAL( S_NC )

        IF ( IERR .NE. 0 ) THEN
            CALL LOGERR( FID, VID, VID, IERR )
            WRFLAG = .FALSE.
            RETURN

        END IF          !  ierr nonzero:  operation failed

        !!...............   Write time step flag:

        WRFLAG = WRTFLAG( FID, VID, FLAGS, STEP2 )
        RETURN

    ELSE IF ( NVARS3( FID ) .EQ. 0 ) THEN   !  write all-vars timestep flag

        WRFLAG = WRTFLAG( FID, VID, FLAGS, STEP2 )
        RETURN

    ELSE                            !  vid = allvars3:  write all variables:

    !!...............   Write contents of the buffer for each variable

        INDX  = 1                   !  starting subscript for BUFFER(*)
        AFLAG = .TRUE.

        DO  VAR = 1 , NVARS3( FID )

            VTYPE = VTYPE3( VAR,FID )
            VINDX = VINDX3( VAR,FID )

!$OMP CRITICAL( S_NC )
            IF ( VTYPE .EQ. M3INT ) THEN
                IERR  = NF_PUT_VARA_INT(    ICDF, VINDX, DIMS, DELS, BUFFER( INDX ) )
            ELSE IF ( VTYPE .EQ. M3REAL ) THEN
                IERR  = NF_PUT_VARA_REAL(   ICDF, VINDX, DIMS, DELS, BUFFER( INDX ) )
            ELSE IF ( VTYPE .EQ. M3DBLE ) THEN
                IERR  = NF_PUT_VARA_DOUBLE( ICDF, VINDX, DIMS, DELS, BUFFER( INDX ) )
            ELSE IF ( VTYPE .EQ. M3INT8 ) THEN
                IERR  = NF_PUT_VARA_INT64(  ICDF, VINDX, DIMS, DELS, BUFFER( INDX ) )
            ELSE
                IERR = NF_EBADTYPE
            END IF
!$OMP END CRITICAL( S_NC )

            IF ( IERR .NE. 0 ) THEN
                CALL LOGERR( FID, VAR, VID, IERR )
                WRFLAG = .FALSE.
                RETURN
            END IF                  !  ierr nonzero:  operation failed

            INDX = INDX  +  DELTA * TYPSIZE( VTYPE3( VAR,FID ) )

            !!...............   Write time step flags:

            IF ( .NOT.WRTFLAG( FID, VAR, FLAGS, STEP2 ) ) THEN
                MESG = 'Error writing time-flags for file ' // FLIST3( FID )
                CALL M3MSG2( MESG )
                WRFLAG = .FALSE.
                RETURN
            END IF

        END DO                    !  end loop on variables VAR

    END IF                  !  if writing just one vble, or all vbles

    IF ( VOLAT3( FID ) ) THEN     !  volatile file:  synch with disk

        IF ( .NOT. SYNCFID( FID ) ) THEN

            MESG = 'Error with disk synchronization for file:  ' // FLIST3( FID )
            CALL M3MSG2( MESG )
            WRFLAG = .FALSE.
            RETURN

        END IF              !  if synch failed

    END IF                  !  if file volatile

    WRFLAG = .TRUE.         ! (if you get to here)
    RETURN

CONTAINS    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


        SUBROUTINE LOGERR( FID, VAR, VID, ERR )

            INTEGER, INTENT(IN   ) :: FID, VAR, VID, ERR

            INTEGER         L, CDFID, VINDX
            CHARACTER*16    VNAME, FNAME

            IF ( VID .LE. 0 ) THEN
                WRITE( L,'(5X,A)') 'WRITE3 request: ALL VARIABLES'
            ELSE
                WRITE( L,'(/)')
            END IF
            L     = LOGDEV
            CDFID = CDFID3( FID )
            VINDX = VINDX3( VAR,FID )
            FNAME = FLIST3( FID )
            VNAME = VLIST3( VAR,FID )

            WRITE( L, '(5X, 4A )' ) 'WRVARS:  Error writing variable ', VNAME, ' to file ' // FNAME
            WRITE( L,'(5X,A, I7)' ) 'Error number', IERR
            WRITE( L,'(5X,A, I7)' ) 'IOAPI ID    ', FID
            WRITE( L,'(5X,A, I7)' ) 'netCDF ID   ', CDFID
            WRITE( L,'(5X,A, I7)' ) 'vble        ', VINDX
            WRITE( L,'(5X,A,5I7)' ) 'dims array  ', DIMS
            WRITE( L,'(5X,A,5I7)' ) 'delts array ', DELS
            WRITE( L,'(5X,A, I7)' ) 'offset      ', INDX

        END SUBROUTINE LOGERR

END FUNCTION WRVARS

