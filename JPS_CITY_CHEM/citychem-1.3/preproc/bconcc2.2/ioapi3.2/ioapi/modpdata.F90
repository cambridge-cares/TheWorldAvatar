MODULE MODPDATA

    !!***********************************************************************
    !! Version "$Id: modpdata.F90 1 2017-06-10 18:05:20Z coats $"
    !! EDSS/Models-3 I/O API.
    !! Copyright (C) 2015 UNC Institute for the Environment.
    !! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    !! See file "LGPL.txt" for conditions of use.
    !!.........................................................................
    !!
    !!  FUNCTION:
    !!      State and initialization for parallel-netCDF routines
    !!
    !!  REVISION  HISTORY:
    !!      Version  07/2015 by David WONG, US EPA:  prototype
    !!
    !!      Version  08/2015 by Carlie J. Coats, Jr., UNC IE:
    !!      Bug-fixes, USE M3UTILIO, standards-conformance, encapsulation
    !!      and modularity, general clean-up. 
    !!
    !!      Version  12/2015 by Carlie J. Coats, Jr., UNC IE: Do list-based
    !!      read for NPCOL_NPROW-values.  Bug-fixes and changes from D.Wong.
    !!***********************************************************************

    USE M3UTILIO
    USE MODNCFIO

    IMPLICIT NONE

#ifdef IOAPI_PNCF
        INCLUDE "mpif.h"
#endif

    PUBLIC  PN_SETUP, PN_FLAG

    !!........   object-sizes, as MPI-offset type:

    INTEGER                   , PUBLIC, PARAMETER :: PN_RETVAL = MXVARS3+1          !!  tag for "success/failure" communications

#ifdef IOAPI_PNCF
    INTEGER( MPI_OFFSET_KIND ), PUBLIC, PARAMETER :: PN_ONE    = 1                  !!  constant, as INTEGER( MPI_OFFSET_KIND )
    INTEGER( MPI_OFFSET_KIND ), PUBLIC, PARAMETER :: PN_TWO    = 2                  !!  "
    INTEGER( MPI_OFFSET_KIND ), PUBLIC, PARAMETER :: PN_NAMLEN = NAMLEN3            !!  ", length of name-object
    INTEGER( MPI_OFFSET_KIND ), PUBLIC, PARAMETER :: PN_MXDLEN = MXDLEN3            !!  " description-line object
    INTEGER( MPI_OFFSET_KIND ), PUBLIC, PARAMETER :: PN_MXDESC = MXDESC3            !!  " number of history/desc lines
    INTEGER( MPI_OFFSET_KIND ), PUBLIC, PARAMETER :: PN_DSCLEN = MXDLEN3*MXDESC3    !!  " history and description objects

#endif

    !!........   Master-grid dimensions

    INTEGER, PUBLIC, PROTECTED, SAVE :: GRID_COLS         !! number of master-grid columns
    INTEGER, PUBLIC, PROTECTED, SAVE :: GRID_ROWS         !! number of master-grid rows

    !!........   Processor-decomposition

    INTEGER, PUBLIC, PROTECTED, SAVE :: PN_NPROCS         !! number of processors allocated
    INTEGER, PUBLIC, PROTECTED, SAVE :: PN_NPCOL          !! number of processors assigned to column dimension
    INTEGER, PUBLIC, PROTECTED, SAVE :: PN_NPROW          !! number of processors assigned to row    dimension
    INTEGER, PUBLIC, PROTECTED, SAVE :: PN_WORLD_COMM     !! mpi global     domain communicator
    INTEGER, PUBLIC, PROTECTED, SAVE :: PN_COL_IO_COMM    !! mpi aggregated domain communicator
    INTEGER, PUBLIC, PROTECTED, SAVE :: PN_NON_IO_COMM    !! non IO communicator
    INTEGER, PUBLIC, PROTECTED, SAVE :: PN_MYPE           !! processor ID in global     domain
    INTEGER, PUBLIC, PROTECTED, SAVE :: PN_MYPE_P1        !! PN_MYPE + 1
    INTEGER, PUBLIC, PROTECTED, SAVE :: PN_AGG_MYPE       !! processor ID in aggregated domain
    INTEGER, PUBLIC, PROTECTED, SAVE :: PN_AGG_MYPE_P1    !! PN_AGG_MYPE + 1
    INTEGER, PUBLIC, PROTECTED, SAVE :: PN_AGG_MYPE_G     !! PN_AGG_MYPE * PN_NPCOL

    INTEGER, ALLOCATABLE, PUBLIC, PROTECTED, SAVE :: PN_NCOLS_PE(:)       !! number of columns  in each PE in decomposed global domain
    INTEGER, ALLOCATABLE, PUBLIC, PROTECTED, SAVE :: PN_NROWS_PE(:)       !! number of rows     in each PE in decomposed global domain
    INTEGER, ALLOCATABLE, PUBLIC, PROTECTED, SAVE :: PN_COLSX_PE(:,:)     !! column index range in each PE in decomposed global domain
    INTEGER, ALLOCATABLE, PUBLIC, PROTECTED, SAVE :: PN_ROWSX_PE(:,:)     !! row    index range in each PE in decomposed global domain
    INTEGER, ALLOCATABLE, PUBLIC, PROTECTED, SAVE :: PN_AGG_NCOLS_PE(:)   !! number of columns  in each PE in aggregated        domain
    INTEGER, ALLOCATABLE, PUBLIC, PROTECTED, SAVE :: PN_AGG_NROWS_PE(:)   !! number of rows     in each PE in aggregated        domain
    INTEGER, ALLOCATABLE, PUBLIC, PROTECTED, SAVE :: PN_AGG_COLSX_PE(:,:) !! column index range in each PE in aggregated        domain
    INTEGER, ALLOCATABLE, PUBLIC, PROTECTED, SAVE :: PN_AGG_ROWSX_PE(:,:) !! row    index range in each PE in aggregated        domain

    LOGICAL, PUBLIC, PROTECTED, SAVE :: PN_IO_PE = .FALSE.    !! indicator of a processor involved in collective pnetCDF output process

    PRIVATE         !!  everything else

    LOGICAL, SAVE :: PN_INIT  = .FALSE.


CONTAINS  !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE SETUP_DECOMP( NPROCS, NPCOL, NPROW, NCOLS, NROWS,        &
                             NCOLS_PE, NROWS_PE, COLSX_PE, ROWSX_PE )

        !----------------------------------------------------------------------
        !  Purpose: to construct a horizontal processor-to-subdomain
        !           decomposition map.
        !----------------------------------------------------------------------

        IMPLICIT  NONE

        INTEGER, INTENT(IN) :: NPROCS    ! Number of processors
        INTEGER, INTENT(IN) :: NCOLS     ! Total number of columns in grid
        INTEGER, INTENT(IN) :: NROWS     ! Total number of rows in grid
        INTEGER, INTENT(IN) :: NPCOL     ! Number of PEs across grid cols
        INTEGER, INTENT(IN) :: NPROW     ! Number of PEs across grid rows

        INTEGER, INTENT(OUT) :: NCOLS_PE( : )   ! Number of columns in each PE
        INTEGER, INTENT(OUT) :: NROWS_PE( : )   ! Number of rows in each PE
        INTEGER, INTENT(OUT) :: COLSX_PE( :,: ) ! Column index range in each PE
        INTEGER, INTENT(OUT) :: ROWSX_PE( :,: ) ! Row index range in each PE

#ifdef IOAPI_PNCF

          !!......   Local Variables:

        INTEGER :: C, R       ! loop counters; single-index
        INTEGER :: J, K       ! temporary variables
        INTEGER :: QC, QR     ! quotient
        INTEGER :: RC, RR     ! remainder

        !!.........................   body  ................................
        !!......   for the column, row dimensions

        QC =      NCOLS / NPCOL
        RC = MOD( NCOLS , NPCOL )
        QR =      NROWS / NPROW
        RR = MOD( NROWS , NPROW )

        DO R = 1, NPROW
        DO C = 1, NPCOL
            J = (R - 1) * NPCOL + C
            NCOLS_PE( J ) = QC + MIN( RC / J, 1)
            IF ( C == 1) THEN
                COLSX_PE (1, J) = 1
            ELSE
                COLSX_PE (1, J) = COLSX_PE (2, J-1) + 1
            END IF
            COLSX_PE (2, J) = COLSX_PE (1, J) + NCOLS_PE (J) -1

            NROWS_PE( J ) = QR + MIN( RR / J, 1)
            IF ( R == 1 ) THEN
                ROWSX_PE (1, J) = 1
            ELSE
                K = J - NPCOL
                ROWSX_PE (1, J) = ROWSX_PE (2, K) + 1
            END IF
            ROWSX_PE (2, J) = ROWSX_PE (1, J) + NROWS_PE( J ) - 1
        END DO
        END DO

#endif                  /*  ifdef IOAPI_PNCF   */

#ifndef IOAPI_PNCF     /*  IOAPI_PNCF not defined:   */

        CALL M3EXIT( 'MODPDATA:SETUP_DECOMP',0,0, 'MPI/PnetCDF "IOAPI_PNCF" not defined for this build', 2 )

#endif                  /*  ifndef IOAPI_PNCF   */

    END SUBROUTINE SETUP_DECOMP


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE PN_SETUP()

        CHARACTER*24, PARAMETER :: PNAME = 'MODPDATA:PN_SETUP'
        CHARACTER*16, PARAMETER :: BLANK = ' '

#ifdef IOAPI_PNCF

        INTEGER         IERR, K, N
        INTEGER         COL_WORLD_GROUP, COL_IO_GROUP, NON_IO_GROUP

        !! Grid description:

        CHARACTER*16    GDNAM       !! grid name
        CHARACTER*16    PRNAM       !! map-projection name
        INTEGER         NCOLS       !! number of input-grid columns
        INTEGER         NROWS       !! number of input-grid rows
        INTEGER         NTHIK       !! number of layers
        INTEGER         NLAYS       !! boundary thickness (cells)
        INTEGER         NSIZE       !! number of input-grid cells
        INTEGER         GDTYP       !! grid type:  1=LAT-LON, 2=UTM, ...
        REAL*8          P_ALP       !! first, second, third map
        REAL*8          P_BET       !! projection descriptive
        REAL*8          P_GAM       !! parameters.
        REAL*8          XCENT       !! lon for coord-system X=0
        REAL*8          YCENT       !! lat for coord-system Y=0
        REAL*8          XORIG       !! X-coordinate origin of grid (map units)
        REAL*8          YORIG       !! Y-coordinate origin of grid
        REAL*8          XCELL       !! X-coordinate cell dimension
        REAL*8          YCELL       !! Y-coordinate cell dimension

        CHARACTER*256   MESG, EBUF

        INTEGER, ALLOCATABLE :: RANKS(:)

        LOGICAL, SAVE :: FIRSTIME = .TRUE.

        !!.........................   body  ................................

!$OMP   SINGLE

        IF ( .NOT.FIRSTIME ) GO TO  99

        FIRSTIME = .FALSE.
        MESG = '"' // TRIM( PNAME ) // '":  Version'
        CALL M3MESG( MESG )
        CALL M3MESG( &
'$Id: modpdata.F90 1 2017-06-10 18:05:20Z coats $' )

        CALL ENVSTR( 'NPCOL_NPROW', 'Processor decomposition: npcol x nprow', BLANK, EBUF, IERR )
        IF ( IERR .NE. 0 ) THEN
            MESG = 'Bad environment-list "NPCOL_NPROW"'
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF

        READ( EBUF, *, IOSTAT=IERR ) PN_NPCOL, PN_NPROW
        IF ( IERR .NE. 0 ) THEN
            MESG = 'Bad entries for environment-list "NPCOL_NPROW"'
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        ELSE IF ( PN_NPCOL*PN_NPROW .LE. 0 ) THEN
            MESG = 'Non-positive entries in environment-list "NPCOL_NPROW"'
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF

        PN_NPROCS = PN_NPCOL * PN_NPROW
        WRITE( MESG, '( A, I7, 2X, A, I7 )' )  &
            'Processor-grid dimensions:  COLS=', PN_NPCOL, 'ROWS=', PN_NPROW
        CALL M3MESG( MESG )

        CALL ENVSTR( 'GRID_NAME', PNAME, BLANK, GDNAM, IERR )

        IF ( IERR .NE. 0 ) THEN
            MESG = 'Bad environment variable "GRID_NAME"'
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        ELSE IF ( .NOT.DSCGRID( GDNAM, PRNAM, GDTYP,                &
                                P_ALP, P_BET,P_GAM, XCENT, YCENT,   &
                                XORIG, YORIG, XCELL, YCELL,         &
                                NCOLS, NROWS, NTHIK ) ) THEN
            MESG = 'Grid "' // TRIM( GDNAM ) // '" not found in GRIDDESC file'
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        ELSE
            GRID_COLS = NCOLS
            GRID_ROWS = NROWS
            WRITE( MESG, '( A, 2( 2X, A, I7 ) )' )     &
                TRIM( PNAME ), 'Model-grid dimensions:  NCOLS=', NCOLS, 'NROWS=', NROWS
            CALL M3MESG( MESG )
        END IF

        ALLOCATE( PN_NCOLS_PE( PN_NPROCS ),     &
                  PN_NROWS_PE( PN_NPROCS ),     &
                PN_COLSX_PE( 2,PN_NPROCS ),     &
                PN_ROWSX_PE( 2,PN_NPROCS ),     &
                         RANKS( PN_NPROW ),   &
               PN_AGG_NROWS_PE( PN_NPROW ),     &
               PN_AGG_NCOLS_PE( PN_NPROW ),     &
             PN_AGG_COLSX_PE( 2,PN_NPROW ),     &
             PN_AGG_ROWSX_PE( 2,PN_NPROW ), STAT=IERR )

        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( A, I10 )' ) 'Allocation failure.  IERR=', IERR
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF


        IF ( PN_NPROCS .GT. 1 ) THEN

            DO K = 1, PN_NPROCS, PN_NPCOL
                RANKS(K/PN_NPCOL+1) = K - 1
            END DO

            CALL MPI_COMM_RANK( MPI_COMM_WORLD, PN_MYPE, IERR )
            IF ( IERR .NE. 0 ) THEN
                WRITE( MESG, '( A, I10 )' ) 'MPI_COMM_RANK() failure.  IERR=', IERR
                CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
            END IF

            PN_AGG_MYPE    = PN_MYPE / PN_NPCOL
            PN_AGG_MYPE_P1 = PN_AGG_MYPE + 1
            PN_AGG_MYPE_G  = PN_AGG_MYPE * PN_NPCOL
            PN_MYPE_P1     = PN_MYPE + 1

            CALL MPI_COMM_GROUP( MPI_COMM_WORLD, COL_WORLD_GROUP, IERR )
            IF ( IERR .NE. 0 ) THEN
                WRITE( MESG, '( A, I10 )' ) 'MPI_COMM_RANK() failure.  IERR=', IERR
                CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
            END IF

            CALL MPI_GROUP_INCL( COL_WORLD_GROUP, PN_NPROW, RANKS, COL_IO_GROUP, IERR )
            IF ( IERR .NE. 0 ) THEN
                WRITE( MESG, '( A, I10 )' ) 'MPI_COMM_GROUP() failure.  IERR=', IERR
                CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
            END IF

            CALL MPI_COMM_CREATE( MPI_COMM_WORLD, COL_IO_GROUP, PN_COL_IO_COMM, IERR )
            IF ( IERR .NE. 0 ) THEN
                WRITE( MESG, '( A, I10 )' ) 'MPI_COMM_CREATE(...COL_IO_GROUP...) failure.  IERR=', IERR
                CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
            END IF

            CALL MPI_GROUP_EXCL( COL_WORLD_GROUP, PN_NPROW, RANKS, NON_IO_GROUP, IERR )
            IF ( IERR .NE. 0 ) THEN
                WRITE( MESG, '( A, I10 )' ) 'MPI_GROUP_EXCL() failure.  IERR=', IERR
                CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
            END IF

            CALL MPI_COMM_CREATE( MPI_COMM_WORLD, NON_IO_GROUP, PN_NON_IO_COMM, IERR )
            IF ( IERR .NE. 0 ) THEN
                WRITE( MESG, '( A, I10 )' ) 'MPI_COMM_CREATE(...NON_IO_GROUP...) failure.  IERR=', IERR
                CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
            END IF

        ELSE
             RANKS(1) = 0
            CALL MPI_COMM_DUP( MPI_COMM_WORLD, PN_COL_IO_COMM, IERR )
            IF ( IERR .NE. 0 ) THEN
                WRITE( MESG, '( A, I10 )' ) 'MPI_COMM_DUP(...PN_COL_IO_COMM...) failure.  IERR=', IERR
                CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
            END IF
            PN_MYPE = 1
        END IF

        CALL MPI_COMM_DUP( MPI_COMM_WORLD, PN_WORLD_COMM, IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( A, I10 )' ) 'MPI_COMM_DUP(...PN_WORLD_COMM...) failure.  IERR=', IERR
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF

        PN_IO_PE = ( MOD( PN_MYPE, PN_NPCOL ) == 0 )

        DEALLOCATE( RANKS )

        CALL SETUP_DECOMP( PN_NPROCS, PN_NPCOL, PN_NPROW,  NCOLS, NROWS,        &
                           PN_NCOLS_PE, PN_NROWS_PE, PN_COLSX_PE, PN_ROWSX_PE )

        CALL SETUP_DECOMP( PN_NPROW, 1, PN_NPROW, NCOLS, NROWS,                 &
                           PN_AGG_NCOLS_PE, PN_AGG_NROWS_PE, PN_AGG_COLSX_PE, PN_AGG_ROWSX_PE )
        CALL M3MESG( BLANK )

99      CONTINUE

!$OMP   END SINGLE

        PN_INIT = .TRUE.

#endif                  /*  ifdef IOAPI_PNCF   */

#ifndef IOAPI_PNCF     /*  IOAPI_PNCF not defined:   */

        CALL M3EXIT( PNAME,0,0, 'MPI/PnetCDF "IOAPI_PNCF" not defined for this build', 2 )

#endif                  /*  ifndef IOAPI_PNCF   */

        RETURN

    END SUBROUTINE PN_SETUP


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION PN_FLAG( FLAG )
    
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !!   True iff FLAG true on all related threads
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        LOGICAL, INTENT( INOUT ) :: FLAG

#ifdef IOAPI_PNCF
        
        INTEGER         IERR
        LOGICAL         EFLAG
        CHARACTER*256   MESG

        EFLAG = .FALSE.

        IF ( .NOT.PN_INIT ) THEN
            EFLAG = .FALSE. 
        ELSE
            CALL MPI_ALLREDUCE( FLAG, EFLAG, 1, MPI_LOGICAL, MPI_LAND, PN_COL_IO_COMM, IERR )
            IF ( IERR .NE. 0 ) THEN
                WRITE( MESG, '(A,I9)' ) 'WRTFLAG:  MPI_SEND(EFLAG) error: IERR=', IERR
                CALL M3MSG2( MESG )
            END IF
        END IF

        PN_FLAG = EFLAG

#endif                  /*  ifdef IOAPI_PNCF   */

#ifndef IOAPI_PNCF      /*  IOAPI_PNCF not defined:   */
        PN_FLAG = .FALSE.
        CALL M3EXIT(  'MODPDATA:PN_FLAG',0,0, 'MPI/PnetCDF "IOAPI_PNCF" not defined for this build', 2 )
#endif                  /*  ifndef IOAPI_PNCF   */
        
        RETURN

    END FUNCTION PN_FLAG
    

    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


END MODULE MODPDATA
