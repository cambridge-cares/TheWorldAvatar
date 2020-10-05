

!!***********************************************************************
!! Version "$Id: pn_wrvars.F90 1 2017-06-10 18:05:20Z coats $"
!! EDSS/Models-3 I/O API.
!! Copyright (C) 2014-2015 UNC Institute for the Environment.
!! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
!! See file "LGPL.txt" for conditions of use.
!!.........................................................................
!!
!!  FUNCTION:
!!      Routines for type-specific PnetCDF/MPI distributed output,
!!      for use in WRMPIGRD3().
!!
!!      Must be separately compiled, because of essentially="void-pointer"
!!      BUFFER argument for WRMPIGRD3().
!!
!!  PRECONDITIONS REQUIRED:
!!       Should only be called by WRMPIGRD3().
!!
!!  SUBROUTINES AND FUNCTIONS CALLED:
!!       MPI*, NFMPI*, WRTFLAG, SYNCFID, PN_FLAG
!!
!!  REVISION  HISTORY:
!!      Prototype  07/2015 by David Wong from  I/O API-3.1 "wrvars.F"
!!      to support PnetCDF distributed-file operations
!!
!!      Version  08/2015 by CJC: Major cleanup and bug-fixes for I/O API-3.2
!!      Support for all M3IO types, with one routine for each.
!!      NFMPI_PUT_*() error messages via PN_WRVERR().
!!
!!      Version  11/2015 by CJC: replace MPI_OFFSET_KIND by hard-coded INTEGER(8)
!!      because OpenMPI-1.4.x does not follow the MPOI "standard" competently.
!!***********************************************************************


SUBROUTINE  PN_WRVERR( PNAME, FID, VID, INDX, DIMS, DELS, IERR )

    USE MODNCFIO

    IMPLICIT NONE

    !----------------------------------------------------------------------
    !  For use in routines  PN_WR*() below
    !----------------------------------------------------------------------

    !!...........   Arguments

    CHARACTER(*), INTENT(IN   ) :: PNAME
    INTEGER     , INTENT(IN   ) :: FID, VID, INDX, IERR
    INTEGER     , INTENT(IN   ) :: DIMS( 5 ), DELS( 5 )

    !!...........   INCLUDES:

    INCLUDE 'PARMS3.EXT'
    INCLUDE 'STATE3.EXT'

    CHARACTER*16    FNAME, VNAME
    CHARACTER*80    ERRSTR

#ifdef IOAPI_PNCF
    FNAME  = FLIST3( FID )
    VNAME  = VLIST3( VID,FID )
    ERRSTR = NFMPI_STRERROR( IERR )

    WRITE( LOGDEV, '(5X,A     )' ) TRIM( PNAME ),': error writing variable ' // VNAME // 'to file ' // FNAME
    WRITE( LOGDEV, '(5X,A, I9 )' ) 'PnetCDF error number', IERR
    WRITE( LOGDEV, '(5X,A     )' ) ERRSTR
    WRITE( LOGDEV, '(5X,A,5I9 )' ) 'dims array  ', DIMS
    WRITE( LOGDEV, '(5X,A,5I9 )' ) 'delts array ', DELS
    WRITE( LOGDEV, '(5X,A, I9 )' ) 'offset      ', INDX
    WRITE( LOGDEV,* )
#endif

    RETURN

END SUBROUTINE PN_WRVERR


!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


LOGICAL FUNCTION PN_WRINT( FID, VID, FLAGS, STEP2, DIMS, DELS, DELTA, BUFFER )

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
    INTEGER, INTENT(IN   ) :: DIMS( 5 )       !  corner arg array for NF_GET_VARA_*()
    INTEGER, INTENT(IN   ) :: DELS( 5 )       !  diag   arg array for NF_GET_VARA_*()
    INTEGER, INTENT(IN   ) :: DELTA           !  d(INDX) / d(NF_GET_VARA_*call)
    INTEGER, INTENT(IN   ) :: BUFFER(*)       !  buffer array for input

#ifdef IOAPI_PNCF

        INCLUDE "mpif.h"


    !!...........   EXTERNAL FUNCTIONs:

    LOGICAL, EXTERNAL :: WRTFLAG, SYNCFID


    !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER     IERR            !  netCDF error status return
    INTEGER     VAR             !  loop counter for file variables
    INTEGER     VLO, VHI        !  starting, ending  vble-loop indices
    INTEGER     INDX            !  subscript location in BUFFER(*)
    INTEGER     K, L, R, C, N
    INTEGER     DSIZE, COL_SIZE, ROW_SIZE
    INTEGER     STATUS( MPI_STATUS_SIZE )

    LOGICAL     SUCCESS

    CHARACTER*256 MESG

    INTEGER, ALLOCATABLE :: IOBUF(:,:,:), RBUF(:,:,:)

    !!***********************************************************************
    !!   begin body of function  PN_WRINT

    INTEGER( 8 ) :: DIMP( 5 )
    INTEGER( 8 ) :: DELP( 5 )

!!  INTEGER( MPI_OFFSET_KIND ) :: DIMP( 5 )
!!  INTEGER( MPI_OFFSET_KIND ) :: DELP( 5 )

    DIMP( 1:5 ) = DIMS( 1:5 )
    DELP( 1:5 ) = DELS( 1:5 )

!$OMP SINGLE

    IF ( VID .GT. 0 ) THEN          !  write just one variable
        VLO = VID
        VHI = VID
    ELSE
        VLO = 1
        VHI = NVARS3( FID )
    END IF

    IF ( PN_IO_PE ) THEN
        ALLOCATE( IOBUF( PN_AGG_NCOLS_PE(PN_AGG_MYPE_P1), PN_AGG_NROWS_PE(PN_AGG_MYPE_P1), DELS(3) ), STAT=IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '(A,I9)' ) 'PN_WRINT:  memory allocation error:', IERR
            CALL M3MSG2( MESG )
            SUCCESS = .FALSE.
            GO TO  999
        END IF
    END IF

    !!...............   Write contents of the buffer for this variable

    INDX = 1

    DO  VAR = VLO, VHI

        IF ( PN_IO_PE ) THEN

            N = INDX
            DO L = 1, DELS(3)
            DO R = 1, PN_NROWS_PE( PN_MYPE_P1 )
            DO C = 1, PN_NCOLS_PE( PN_MYPE_P1 )
                  IOBUF(C,R,L) = BUFFER(N)
                  N = N + 1
            END DO
            END DO
            END DO

             DO K = PN_MYPE+1, PN_MYPE+PN_NPCOL-1
                ROW_SIZE = PN_NROWS_PE(K+1)
                COL_SIZE = PN_NCOLS_PE(K+1)
                ALLOCATE( RBUF( COL_SIZE, ROW_SIZE, DELS(3) ), STAT=IERR )
                IF ( IERR .NE. 0 ) THEN
                    WRITE( MESG, '(A,I9)' ) 'PN_WRINT:  memory allocation error:', IERR
                    CALL M3MSG2( MESG )
                    SUCCESS = .FALSE.
                    GO TO  999
                END IF
                DSIZE = COL_SIZE * ROW_SIZE * DELS(3)

                CALL MPI_RECV( RBUF, DSIZE, MPI_INTEGER, K, VAR, MPI_COMM_WORLD, STATUS, IERR )
                IF ( IERR .NE. 0 ) THEN
                    WRITE( MESG, '(A,I9)' ) 'PN_WRINT:  MPI_RECV error:  IERR=', IERR
                    CALL M3MSG2( MESG )
                    SUCCESS = .FALSE.
                    GO TO  999
                END IF

                IOBUF( PN_COLSX_PE(1, K+1):PN_COLSX_PE(2, K+1),:,: ) = RBUF
                DEALLOCATE( RBUF )
            END DO

            IERR = NFMPI_PUT_VARA_REAL_ALL( CDFID3( FID ), VINDX3( VAR,FID ), DIMP, DELP, IOBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL PN_WRVERR( 'PN_WRINT', FID, VAR, INDX, DIMS, DELS, IERR )
                SUCCESS = .FALSE.
                GO TO  999
            END IF                  !  ierr nonzero:  operation failed

            IF ( .NOT.WRTFLAG( FID, VAR, FLAGS, STEP2 ) ) THEN
                MESG = 'Error writing time-flags for file ' // FLIST3( FID )
                CALL M3MSG2( MESG )
                SUCCESS = .FALSE.
                GO TO  999
            END IF                  !  ierr nonzero:  WRTFLAG() failed

        ELSE     !!  not I/O PE

            ROW_SIZE = PN_NROWS_PE(PN_MYPE_P1)
            COL_SIZE = PN_NCOLS_PE(PN_MYPE_P1)
            ALLOCATE( RBUF(COL_SIZE, ROW_SIZE, DELS(3) ), STAT=IERR )
            IF ( IERR .NE. 0 ) THEN
                WRITE( MESG, '(A,I9)' ) 'PN_WRINT:  memory allocation error:', IERR
                CALL M3MSG2( MESG )
                SUCCESS = .FALSE.
                GO TO  999
            END IF

            N = INDX
            DO L = 1, DELS(3)
            DO R = 1, PN_NROWS_PE(PN_MYPE_P1)
            DO C = 1, PN_NCOLS_PE(PN_MYPE_P1)
                RBUF(C,R,L) = BUFFER(N)
                N = N + 1
            END DO
            END DO
            END DO

            DSIZE = PN_NCOLS_PE(PN_MYPE_P1) * PN_NROWS_PE(PN_MYPE_P1) * DELS(3)

            CALL MPI_SEND( RBUF, DSIZE, MPI_INTEGER, PN_AGG_MYPE_G, VAR, MPI_COMM_WORLD, IERR )
            IF ( IERR .NE. 0 ) THEN
                WRITE( MESG, '(A,I9)' ) 'PN_WRINT:  MPI_SEND error:  IERR=', IERR
                CALL M3MSG2( MESG )
                SUCCESS = .FALSE.
                GO TO  999
            END IF

        END IF            !!  if I/O  PE

        INDX = INDX + DELTA

    END DO      !!  end loop on variables

    IF ( VOLAT3( FID ) .AND. PN_IO_PE ) THEN     !  volatile file:  synch with disk

        IF ( .NOT. SYNCFID( FID ) ) THEN
            MESG = 'Error with disk synchronization for file:  ' // FLIST3( FID )
            CALL M3MSG2( MESG )
            SUCCESS = .FALSE.
            GO TO  999
        END IF              !  if synch failed

    END IF                  !  if file volatile

    SUCCESS = .TRUE.      ! (if you get to here)

999 CONTINUE

    IF ( ALLOCATED( IOBUF ) )  DEALLOCATE( IOBUF )

    IF ( .NOT. PN_FLAG( SUCCESS ) ) THEN
        CALL M3MSG2( 'PN_WRINT:  MPI_SEND(SUCCESS) error' )
        SUCCESS = .FALSE.
    END IF

    PN_WRINT = SUCCESS

!$OMP  END SINGLE

#endif

#ifndef IOAPI_PNCF
    CALL M3WARN( 'PN_WRINT',0,0, 'MPI/PnetCDF I/O not enabled in this build' )
    PN_WRINT = .FALSE.
#endif

    RETURN

END FUNCTION PN_WRINT


!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


LOGICAL FUNCTION PN_WRREAL( FID, VID, FLAGS, STEP2, DIMS, DELS, DELTA, BUFFER )

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
    INTEGER, INTENT(IN   ) :: DIMS( 5 )       !  corner arg array for NF_GET_VARA_*()
    INTEGER, INTENT(IN   ) :: DELS( 5 )       !  diag   arg array for NF_GET_VARA_*()
    INTEGER, INTENT(IN   ) :: DELTA           !  d(INDX) / d(NF_GET_VARA_*call)
    REAL   , INTENT(IN   ) :: BUFFER(*)       !  buffer array for input

#ifdef IOAPI_PNCF

        INCLUDE "mpif.h"


    !!...........   EXTERNAL FUNCTIONs:

    LOGICAL, EXTERNAL :: WRTFLAG, SYNCFID


    !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER     IERR            !  netCDF error status return
    INTEGER     VAR             !  loop counter for file variables
    INTEGER     VLO, VHI        !  starting, ending  vble-loop indices
    INTEGER     INDX            !  subscript location in BUFFER(*)
    INTEGER     K, L, R, C, N
    INTEGER     DSIZE, COL_SIZE, ROW_SIZE
    INTEGER     STATUS( MPI_STATUS_SIZE )

    LOGICAL     SUCCESS

    CHARACTER*256 MESG

    REAL, ALLOCATABLE :: IOBUF(:,:,:), RBUF(:,:,:)


    !!***********************************************************************

    INTEGER( 8 ) :: DIMP( 5 )
    INTEGER( 8 ) :: DELP( 5 )

!!  INTEGER( MPI_OFFSET_KIND ) :: DIMP( 5 )
!!  INTEGER( MPI_OFFSET_KIND ) :: DELP( 5 )

    DIMP( 1:5 ) = DIMS( 1:5 )
    DELP( 1:5 ) = DELS( 1:5 )

!$OMP SINGLE

    IF ( VID .GT. 0 ) THEN          !  write just one variable
        VLO = VID
        VHI = VID
    ELSE
        VLO = 1
        VHI = NVARS3( FID )
    END IF

    IF ( PN_IO_PE ) THEN
        ALLOCATE( IOBUF( PN_AGG_NCOLS_PE(PN_AGG_MYPE_P1), PN_AGG_NROWS_PE(PN_AGG_MYPE_P1), DELS(3) ), STAT=IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '(A,I9)' ) 'PN_WRREAL:  memory allocation error:', IERR
            CALL M3MSG2( MESG )
            SUCCESS = .FALSE.
            GO TO  999
        END IF
    END IF

    !!...............   Write contents of the buffer for this variable

    INDX = 1

    DO  VAR = VLO, VHI

        IF ( PN_IO_PE ) THEN

            N = INDX
            DO L = 1, DELS(3)
            DO R = 1, PN_NROWS_PE( PN_MYPE_P1 )
            DO C = 1, PN_NCOLS_PE( PN_MYPE_P1 )
                  IOBUF(C,R,L) = BUFFER(N)
                  N = N + 1
            END DO
            END DO
            END DO

             DO K = PN_MYPE+1, PN_MYPE+PN_NPCOL-1
                ROW_SIZE = PN_NROWS_PE(K+1)
                COL_SIZE = PN_NCOLS_PE(K+1)
                ALLOCATE( RBUF( COL_SIZE, ROW_SIZE, DELS(3) ), STAT=IERR )
                IF ( IERR .NE. 0 ) THEN
                    WRITE( MESG, '(A,I9)' ) 'PN_WRREAL:  memory allocation error:', IERR
                    CALL M3MSG2( MESG )
                    SUCCESS = .FALSE.
                    GO TO  999
                END IF
                DSIZE = COL_SIZE * ROW_SIZE * DELS(3)

                CALL MPI_RECV( RBUF, DSIZE, MPI_REAL, K, VAR, MPI_COMM_WORLD, STATUS, IERR )
                IF ( IERR .NE. 0 ) THEN
                    WRITE( MESG, '(A,I9)' ) 'PN_WRREAL:  MPI_RECV error:  IERR=', IERR
                    CALL M3MSG2( MESG )
                    SUCCESS = .FALSE.
                    GO TO  999
                END IF

                IOBUF( PN_COLSX_PE(1, K+1):PN_COLSX_PE(2, K+1),:,: ) = RBUF
                DEALLOCATE( RBUF )
            END DO

            IERR = NFMPI_PUT_VARA_REAL_ALL( CDFID3( FID ), VINDX3( VAR,FID ), DIMP, DELP, IOBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL PN_WRVERR( 'PN_WRREAL', FID, VAR, INDX, DIMS, DELS, IERR )
                SUCCESS = .FALSE.
                GO TO  999
            END IF                  !  ierr nonzero:  operation failed

            IF ( .NOT.WRTFLAG( FID, VAR, FLAGS, STEP2 ) ) THEN
                MESG = 'Error writing time-flags for file ' // FLIST3( FID )
                CALL M3MSG2( MESG )
                SUCCESS = .FALSE.
                GO TO  999
            END IF                  !  ierr nonzero:  WRTFLAG() failed

        ELSE     !!  not I/O PE

            ROW_SIZE = PN_NROWS_PE(PN_MYPE_P1)
            COL_SIZE = PN_NCOLS_PE(PN_MYPE_P1)
            ALLOCATE( RBUF(COL_SIZE, ROW_SIZE, DELS(3) ), STAT=IERR )
            IF ( IERR .NE. 0 ) THEN
                WRITE( MESG, '(A,I9)' ) 'PN_WRREAL:  memory allocation error:', IERR
                CALL M3MSG2( MESG )
                SUCCESS = .FALSE.
                GO TO  999
            END IF

            N = INDX
            DO L = 1, DELS(3)
            DO R = 1, PN_NROWS_PE(PN_MYPE_P1)
            DO C = 1, PN_NCOLS_PE(PN_MYPE_P1)
                RBUF(C,R,L) = BUFFER(N)
                N = N + 1
            END DO
            END DO
            END DO

            DSIZE = PN_NCOLS_PE(PN_MYPE_P1) * PN_NROWS_PE(PN_MYPE_P1) * DELS(3)

            CALL MPI_SEND( RBUF, DSIZE, MPI_REAL, PN_AGG_MYPE_G, VAR, MPI_COMM_WORLD, IERR )
            IF ( IERR .NE. 0 ) THEN
                WRITE( MESG, '(A,I9)' ) 'PN_WRREAL:  MPI_SEND error:  IERR=', IERR
                CALL M3MSG2( MESG )
                SUCCESS = .FALSE.
                GO TO  999
            END IF

        END IF            !!  if I/O  PE

        INDX = INDX + DELTA

    END DO      !!  end loop on variables

    IF ( VOLAT3( FID ) .AND. PN_IO_PE ) THEN     !  volatile file:  synch with disk

        IF ( .NOT. SYNCFID( FID ) ) THEN
            MESG = 'Error with disk synchronization for file:  ' // FLIST3( FID )
            CALL M3MSG2( MESG )
            SUCCESS = .FALSE.
            GO TO  999
        END IF              !  if synch failed

    END IF                  !  if file volatile

    SUCCESS = .TRUE.      ! (if you get to here)

999 CONTINUE

    IF ( ALLOCATED( IOBUF ) )  DEALLOCATE( IOBUF )

    IF ( .NOT. PN_FLAG( SUCCESS ) ) THEN
        CALL M3MSG2( 'PN_WRREAL:  MPI_SEND(SUCCESS) error' )
        SUCCESS = .FALSE.
    END IF

    PN_WRREAL = SUCCESS

!$OMP  END SINGLE

#endif

#ifndef IOAPI_PNCF
    CALL M3WARN( 'PN_WRREAL',0,0, 'MPI/PnetCDF I/O not enabled in this build' )
    PN_WRREAL = .FALSE.
#endif

    RETURN

END FUNCTION PN_WRREAL


!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


LOGICAL FUNCTION PN_WRDBLE( FID, VID, FLAGS, STEP2, DIMS, DELS, DELTA, BUFFER )

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
    INTEGER, INTENT(IN   ) :: DIMS( 5 )       !  corner arg array for NF_GET_VARA_*()
    INTEGER, INTENT(IN   ) :: DELS( 5 )       !  diag   arg array for NF_GET_VARA_*()
    INTEGER, INTENT(IN   ) :: DELTA           !  d(INDX) / d(NF_GET_VARA_*call)
    REAL*8 , INTENT(IN   ) :: BUFFER(*)       !  buffer array for input

#ifdef IOAPI_PNCF

        INCLUDE "mpif.h"


    !!...........   EXTERNAL FUNCTIONs:

    LOGICAL, EXTERNAL :: WRTFLAG, SYNCFID


    !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER     IERR            !  netCDF error status return
    INTEGER     VAR             !  loop counter for file variables
    INTEGER     VLO, VHI        !  starting, ending  vble-loop indices
    INTEGER     INDX            !  subscript location in BUFFER(*)
    INTEGER     K, L, R, C, N
    INTEGER     DSIZE, COL_SIZE, ROW_SIZE
    INTEGER     STATUS( MPI_STATUS_SIZE )

    LOGICAL     SUCCESS

    CHARACTER*256 MESG

    REAL*8, ALLOCATABLE :: IOBUF(:,:,:), RBUF(:,:,:)

    !!***********************************************************************

    INTEGER( 8 ) :: DIMP( 5 )
    INTEGER( 8 ) :: DELP( 5 )

!!  INTEGER( MPI_OFFSET_KIND ) :: DIMP( 5 )
!!  INTEGER( MPI_OFFSET_KIND ) :: DELP( 5 )

    DIMP( 1:5 ) = DIMS( 1:5 )
    DELP( 1:5 ) = DELS( 1:5 )

!$OMP SINGLE

    IF ( VID .GT. 0 ) THEN          !  write just one variable
        VLO = VID
        VHI = VID
    ELSE
        VLO = 1
        VHI = NVARS3( FID )
    END IF

    IF ( PN_IO_PE ) THEN
        ALLOCATE( IOBUF( PN_AGG_NCOLS_PE(PN_AGG_MYPE_P1), PN_AGG_NROWS_PE(PN_AGG_MYPE_P1), DELS(3) ), STAT=IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '(A,I9)' ) 'PN_WRDBLE:  memory allocation error:', IERR
            CALL M3MSG2( MESG )
            SUCCESS = .FALSE.
            GO TO  999
        END IF
    END IF

    !!...............   Write contents of the buffer for this variable

    INDX = 1

    DO  VAR = VLO, VHI

        IF ( PN_IO_PE ) THEN

            N = INDX
            DO L = 1, DELS(3)
            DO R = 1, PN_NROWS_PE( PN_MYPE_P1 )
            DO C = 1, PN_NCOLS_PE( PN_MYPE_P1 )
                  IOBUF(C,R,L) = BUFFER(N)
                  N = N + 1
            END DO
            END DO
            END DO

             DO K = PN_MYPE+1, PN_MYPE+PN_NPCOL-1
                ROW_SIZE = PN_NROWS_PE(K+1)
                COL_SIZE = PN_NCOLS_PE(K+1)
                ALLOCATE( RBUF( COL_SIZE, ROW_SIZE, DELS(3) ), STAT=IERR )
                IF ( IERR .NE. 0 ) THEN
                    WRITE( MESG, '(A,I9)' ) 'PN_WRDBLE:  memory allocation error:', IERR
                    CALL M3MSG2( MESG )
                    SUCCESS = .FALSE.
                    GO TO  999
                END IF
                DSIZE = COL_SIZE * ROW_SIZE * DELS(3)

                CALL MPI_RECV( RBUF, DSIZE, MPI_DOUBLE_PRECISION, K, VAR, MPI_COMM_WORLD, STATUS, IERR )
                IF ( IERR .NE. 0 ) THEN
                    WRITE( MESG, '(A,I9)' ) 'PN_WRDBLE:  MPI_RECV error:  IERR=', IERR
                    CALL M3MSG2( MESG )
                    SUCCESS = .FALSE.
                    GO TO  999
                END IF

                IOBUF( PN_COLSX_PE(1, K+1):PN_COLSX_PE(2, K+1),:,: ) = RBUF
                DEALLOCATE( RBUF )
            END DO

            IERR = NFMPI_PUT_VARA_REAL_ALL( CDFID3( FID ), VINDX3( VAR,FID ), DIMP, DELP, IOBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL PN_WRVERR( 'PN_WRDBLE', FID, VAR, INDX, DIMS, DELS, IERR )
                SUCCESS = .FALSE.
                GO TO  999
            END IF                  !  ierr nonzero:  operation failed

            IF ( .NOT.WRTFLAG( FID, VAR, FLAGS, STEP2 ) ) THEN
                MESG = 'Error writing time-flags for file ' // FLIST3( FID )
                CALL M3MSG2( MESG )
                SUCCESS = .FALSE.
                GO TO  999
            END IF                  !  ierr nonzero:  WRTFLAG() failed

        ELSE     !!  not I/O PE

            ROW_SIZE = PN_NROWS_PE(PN_MYPE_P1)
            COL_SIZE = PN_NCOLS_PE(PN_MYPE_P1)
            ALLOCATE( RBUF(COL_SIZE, ROW_SIZE, DELS(3) ), STAT=IERR )
            IF ( IERR .NE. 0 ) THEN
                WRITE( MESG, '(A,I9)' ) 'PN_WRDBLE:  memory allocation error:', IERR
                CALL M3MSG2( MESG )
                SUCCESS = .FALSE.
                GO TO  999
            END IF

            N = INDX
            DO L = 1, DELS(3)
            DO R = 1, PN_NROWS_PE(PN_MYPE_P1)
            DO C = 1, PN_NCOLS_PE(PN_MYPE_P1)
                RBUF(C,R,L) = BUFFER(N)
                N = N + 1
            END DO
            END DO
            END DO

            DSIZE = PN_NCOLS_PE(PN_MYPE_P1) * PN_NROWS_PE(PN_MYPE_P1) * DELS(3)

            CALL MPI_SEND( RBUF, DSIZE, MPI_DOUBLE_PRECISION, PN_AGG_MYPE_G, VAR, MPI_COMM_WORLD, IERR )
            IF ( IERR .NE. 0 ) THEN
                WRITE( MESG, '(A,I9)' ) 'PN_WRDBLE:  MPI_SEND error:  IERR=', IERR
                CALL M3MSG2( MESG )
                SUCCESS = .FALSE.
                GO TO  999
            END IF

        END IF            !!  if I/O  PE

        INDX = INDX + DELTA

    END DO      !!  end loop on variables

    IF ( VOLAT3( FID ) .AND. PN_IO_PE ) THEN     !  volatile file:  synch with disk

        IF ( .NOT. SYNCFID( FID ) ) THEN
            MESG = 'Error with disk synchronization for file:  ' // FLIST3( FID )
            CALL M3MSG2( MESG )
            SUCCESS = .FALSE.
            GO TO  999
        END IF              !  if synch failed

    END IF                  !  if file volatile

    SUCCESS = .TRUE.        ! (if you get to here)

999 CONTINUE

    IF ( ALLOCATED( IOBUF ) )  DEALLOCATE( IOBUF )

    IF ( .NOT. PN_FLAG( SUCCESS ) ) THEN
        CALL M3MSG2( 'PN_WRDBLE:  MPI_SEND(SUCCESS) error' )
        SUCCESS = .FALSE.
    END IF

    PN_WRDBLE = SUCCESS

!$OMP  END SINGLE

#endif

#ifndef IOAPI_PNCF
    CALL M3WARN( 'PN_WRDBLE',0,0, 'MPI/PnetCDF I/O not enabled in this build' )
    PN_WRDBLE = .FALSE.
#endif

    RETURN

END FUNCTION PN_WRDBLE




!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


LOGICAL FUNCTION PN_WRINT8( FID, VID, FLAGS, STEP2, DIMS, DELS, DELTA, BUFFER )

    USE MODNCFIO
    USE MODPDATA

    IMPLICIT NONE

    !!...........   INCLUDES:

    INCLUDE 'PARMS3.EXT'
    INCLUDE 'STATE3.EXT'


    !!...........   ARGUMENTS and their descriptions:

    INTEGER,   INTENT(IN   ) :: FID             !  file index within the STATE3 commons
    INTEGER,   INTENT(IN   ) :: VID             !  vble index within the STATE3 commons
    INTEGER,   INTENT(IN   ) :: FLAGS( 2 )      !  ( jdate yyyyddd, jtime hhmmss )
    INTEGER,   INTENT(IN   ) :: STEP2           !  physical time step number (maybe mod 2)
    INTEGER,   INTENT(IN   ) :: DIMS( 5 )       !  corner arg array for NF_GET_VARA_*()
    INTEGER,   INTENT(IN   ) :: DELS( 5 )       !  diag   arg array for NF_GET_VARA_*()
    INTEGER,   INTENT(IN   ) :: DELTA           !  d(INDX) / d(NF_GET_VARA_*call)
    INTEGER*8, INTENT(IN   ) :: BUFFER(*)       !  buffer array for input

#ifdef IOAPI_PNCF

        INCLUDE "mpif.h"


    !!...........   EXTERNAL FUNCTIONs:

    LOGICAL, EXTERNAL :: WRTFLAG, SYNCFID


    !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER     IERR            !  netCDF error status return
    INTEGER     VAR             !  loop counter for file variables
    INTEGER     VLO, VHI        !  starting, ending  vble-loop indices
    INTEGER     INDX            !  subscript location in BUFFER(*)
    INTEGER     K, L, R, C, N
    INTEGER     DSIZE, COL_SIZE, ROW_SIZE
    INTEGER     STATUS( MPI_STATUS_SIZE )

    LOGICAL     SUCCESS

    CHARACTER*256 MESG

    INTEGER*8, ALLOCATABLE :: IOBUF(:,:,:), RBUF(:,:,:)

!!  INTEGER( MPI_OFFSET_KIND ) :: DIMP( 5 )
!!  INTEGER( MPI_OFFSET_KIND ) :: DELP( 5 )
    INTEGER( 8 ) :: DIMP( 5 )
    INTEGER( 8 ) :: DELP( 5 )

    !!***********************************************************************

    DIMP( 1:5 ) = DIMS( 1:5 )
    DELP( 1:5 ) = DELS( 1:5 )

!$OMP SINGLE

    IF ( VID .GT. 0 ) THEN          !  write just one variable
        VLO = VID
        VHI = VID
    ELSE
        VLO = 1
        VHI = NVARS3( FID )
    END IF

    IF ( PN_IO_PE ) THEN
        ALLOCATE( IOBUF( PN_AGG_NCOLS_PE(PN_AGG_MYPE_P1), PN_AGG_NROWS_PE(PN_AGG_MYPE_P1), DELS(3) ), STAT=IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '(A,I9)' ) 'PN_WRINT8:  memory allocation error:', IERR
            CALL M3MSG2( MESG )
            SUCCESS = .FALSE.
            GO TO  999
        END IF
    END IF

    !!...............   Write contents of the buffer for this variable

    INDX = 1

    DO  VAR = VLO, VHI

        IF ( PN_IO_PE ) THEN

            N = INDX
            DO L = 1, DELS(3)
            DO R = 1, PN_NROWS_PE( PN_MYPE_P1 )
            DO C = 1, PN_NCOLS_PE( PN_MYPE_P1 )
                  IOBUF(C,R,L) = BUFFER(N)
                  N = N + 1
            END DO
            END DO
            END DO

             DO K = PN_MYPE+1, PN_MYPE+PN_NPCOL-1
                ROW_SIZE = PN_NROWS_PE(K+1)
                COL_SIZE = PN_NCOLS_PE(K+1)
                ALLOCATE( RBUF( COL_SIZE, ROW_SIZE, DELS(3) ), STAT=IERR )
                IF ( IERR .NE. 0 ) THEN
                    WRITE( MESG, '(A,I9)' ) 'PN_WRINT8:  memory allocation error:', IERR
                    CALL M3MSG2( MESG )
                    SUCCESS = .FALSE.
                    GO TO  999
                END IF
                DSIZE = COL_SIZE * ROW_SIZE * DELS(3)

                CALL MPI_RECV( RBUF, DSIZE, MPI_INTEGER8, K, VAR, MPI_COMM_WORLD, STATUS, IERR )
                IF ( IERR .NE. 0 ) THEN
                    WRITE( MESG, '(A,I9)' ) 'PN_WRINT8:  MPI_RECV error:  IERR=', IERR
                    CALL M3MSG2( MESG )
                    SUCCESS = .FALSE.
                    GO TO  999
                END IF

                IOBUF( PN_COLSX_PE(1, K+1):PN_COLSX_PE(2, K+1),:,: ) = RBUF
                DEALLOCATE( RBUF )
            END DO

            IERR = NFMPI_PUT_VARA_INT8_ALL( CDFID3( FID ), VINDX3( VAR,FID ), DIMP, DELP, IOBUF )
            IF ( IERR .NE. 0 ) THEN
                CALL PN_WRVERR( 'PN_WRINT8', FID, VAR, INDX, DIMS, DELS, IERR )
                SUCCESS = .FALSE.
                GO TO  999
            END IF                  !  ierr nonzero:  operation failed

            IF ( .NOT.WRTFLAG( FID, VAR, FLAGS, STEP2 ) ) THEN
                MESG = 'Error writing time-flags for file ' // FLIST3( FID )
                CALL M3MSG2( MESG )
                SUCCESS = .FALSE.
                GO TO  999
            END IF                  !  ierr nonzero:  WRTFLAG() failed

        ELSE     !!  not I/O PE

            ROW_SIZE = PN_NROWS_PE( PN_MYPE_P1 )
            COL_SIZE = PN_NCOLS_PE( PN_MYPE_P1 )
            ALLOCATE( RBUF(COL_SIZE, ROW_SIZE, DELS(3) ), STAT=IERR )
            IF ( IERR .NE. 0 ) THEN
                WRITE( MESG, '(A,I9)' ) 'PN_WRINT8:  memory allocation error:', IERR
                CALL M3MSG2( MESG )
                SUCCESS = .FALSE.
                GO TO  999
            END IF

            N = INDX
            DO L = 1, DELS(3)
            DO R = 1, PN_NROWS_PE(PN_MYPE_P1)
            DO C = 1, PN_NCOLS_PE(PN_MYPE_P1)
                RBUF(C,R,L) = BUFFER(N)
                N = N + 1
            END DO
            END DO
            END DO

            DSIZE = PN_NCOLS_PE(PN_MYPE_P1) * PN_NROWS_PE(PN_MYPE_P1) * DELS(3)

            CALL MPI_SEND( RBUF, DSIZE, MPI_INTEGER8, PN_AGG_MYPE_G, VAR, MPI_COMM_WORLD, IERR )
            IF ( IERR .NE. 0 ) THEN
                WRITE( MESG, '(A,I9)' ) 'PN_WRINT8:  MPI_SEND error:  IERR=', IERR
                CALL M3MSG2( MESG )
                SUCCESS = .FALSE.
                GO TO  999
            END IF

        END IF            !!  if I/O  PE

        INDX = INDX + DELTA

    END DO      !!  end loop on variables

    IF ( VOLAT3( FID ) .AND. PN_IO_PE ) THEN     !  volatile file:  synch with disk

        IF ( .NOT. SYNCFID( FID ) ) THEN
            MESG = 'Error with disk synchronization for file:  ' // FLIST3( FID )
            CALL M3MSG2( MESG )
            SUCCESS = .FALSE.
            GO TO  999
        END IF              !  if synch failed

    END IF                  !  if file volatile

    SUCCESS = .TRUE.      ! (if you get to here)

999 CONTINUE

    IF ( ALLOCATED( IOBUF ) )  DEALLOCATE( IOBUF )

    IF ( .NOT. PN_FLAG( SUCCESS ) ) THEN
        CALL M3MSG2( 'PN_WRINT8:  MPI_SEND(SUCCESS) error' )
        SUCCESS = .FALSE.
    END IF

    PN_WRINT8 = SUCCESS

!$OMP  END SINGLE

#endif

#ifndef IOAPI_PNCF
    CALL M3WARN( 'PN_WRINT8',0,0, 'MPI/PnetCDF I/O not enabled in this build' )
    PN_WRINT8 = .FALSE.
#endif

    RETURN

END FUNCTION PN_WRINT8

