
LOGICAL FUNCTION RDVARS( FID, VID, DIMS, DELS, DELTA, BUFFER )  RESULT( RDFLAG )

    !!***********************************************************************
    !!Version "$Id: rdvars.F90 1 2017-06-10 18:05:20Z coats $"
    !!EDSS/Models-3 I/O API.
    !!Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    !!(C) 2003-2010 Baron Advanced Meteorological Systems,
    !!(C) 2007-2013 Carlie J. Coats, Jr., and
    !!(C) 2015 UNC Institute for the Environment.
    !!Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    !!See file "LGPL.txt" for conditions of use.
    !!.........................................................................
    !! function body starts at line  138
    !!
    !! FUNCTION:
    !!      reads "variables" part of time step records from Models-3 file
    !!      with index FID, for variable with index VID, for routines
    !!      RDCUSTOM, RDGRDDED, RDBNDARY, RDIDDATA, RDPROFIL, and RDGRNEST.
    !!
    !!      If VID is -1 = ALLAYS3 reads all variables; if LAYER is -1,
    !!      reads all layers.
    !!
    !! MACHINE DEPENDENCY:
    !!	TYPSIZE( 6 ) must be sizeof( double )/sizeof( real )
    !!
    !! RETURN VALUE:  TRUE iff the operation succeeds (and the data is available)
    !!
    !! PRECONDITIONS REQUIRED:
    !!      DIMS and DELS must have the time dimension entries as the last
    !!      nonzero entries, and must be padded with zeros beyond that point.
    !!
    !!      Should only be called by the above routines, after OPEN3() has
    !!      checked for file and time step availability, and after the above
    !!      routines have set up DIMS, DELS, and DELTA.
    !!
    !! SUBROUTINES AND FUNCTIONS CALLED:  INDEX1
    !!
    !! REVISION  HISTORY:
    !!	    prototype 3/1992 by CJC
    !!
    !!	    Modified  9/1994 by CJC:  argument VID instead of VNAME
    !!
    !!      Modified  5/1998 by CJC for OpenMP thread-safety
    !!
    !!      Modified 10/2003 by CJC for I/O API version 3:  support for
    !!      native-binary BINFIL3 file type; uses INTEGER NAME2FID
    !!
    !!      Modified  4/2004 by CJC for I/O API version 3:  fix bug in
    !!      ALLAYS-ALVARS case; correct ALLVARS warning messages.
    !!
    !!      Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !!
    !!      Modified 08/2015 by CJC for I/O API 3.2:  USE M3UTILIO, MODNCFIO,
    !!      MPI/PnetCDF distributed I/O; free F90 source-format; M3INT8 support
    !!
    !!      Version  11/2015 by CJC: replace MPI_OFFSET_KIND by hard-coded INTEGER(8)
    !!      because OpenMPI-1.4.x does not follow the MPOI "standard" competently.
    !!
    !!      Version  12/2015 by CJC: Fixed bug found by D Wong for single-vble cases
    !!***********************************************************************

    USE M3UTILIO
    USE MODNCFIO

    IMPLICIT NONE

    !!...........   INCLUDES:

    INCLUDE 'STATE3.EXT'


    !!...........   ARGUMENTS and their descriptions:

    INTEGER, INTENT(IN   ) :: FID             !  file index within the STATE3 commons
    INTEGER, INTENT(IN   ) :: VID             !  variable index or -1 == ALL
    INTEGER, INTENT(IN   ) :: DIMS( 5 )       !  corner arg array for NF_GET_VARA_*()
    INTEGER, INTENT(IN   ) :: DELS( 5 )       !  corner arg array for NF_GET_VARA_*()
    INTEGER, INTENT(IN   ) :: DELTA           !  d(INDX) / d(NF_GET_VARA_* call)
    REAL   , INTENT(  OUT) :: BUFFER(*)       !  buffer array for input


    !!...........   EXTERNAL FUNCTIONs:

    INTEGER, EXTERNAL :: RDBVARS


    !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         IERR            !  netCDF error status return
        INTEGER         VAR             !  loop counter for file variables
        INTEGER         CDFID           !  CDFID3( FID )
        INTEGER         VTYPE           !  VTYPE3( FID )
        INTEGER         VINDX           !  VINDX3( FID )
        INTEGER         INDX            !  subscript location in BUFFER(*)
        INTEGER         I               !  counter
        INTEGER         STEP, NREC      !  time subscript location in BUFFER(*)
        INTEGER         LAYR            !  layer subscript location in BUFFER(*)
        INTEGER         RSIZE
        LOGICAL         EFLAG
        CHARACTER*16    FNAME, VNAME
        CHARACTER*256   MESG

        INTEGER, PARAMETER :: STEPDEX( -2:9 ) =     &
           (/ IMISS3,   &        !!  out-of-range
              3,        &        !!  CUSTOM3
              IMISS3,   &        !!  DCTNRY3:  should not go through RDVARS()
              4,        &        !!  GRDDED3
              3,        &        !!  BNDARY3
              3,        &        !!  IDDATA3
              4,        &        !!  PROFIL3
              4,        &        !!  GRNEST3
              2,        &        !!  SMATRX3
              4,        &        !!  TSRIES3
              4,        &        !!  PTRFLY3
              IMISS3 /)          !!  out-of-range

#if _CRAY || REAL8
    INTEGER, PARAMETER :: TYPSIZE( 10 ) = (/ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 /)
#endif
#if ! ( _CRAY || REAL8 )
    INTEGER, PARAMETER :: TYPSIZE( 10 ) = (/ 1, 1, 1, 1, 1, 2, 1, 1, 1, 2 /)
#endif

#ifdef  IOAPI_PNCF
    INCLUDE "mpif.h"

    INTEGER( 8 ) :: DIMP( 5 )      !  corner arg array for NFMPI_GET_VARA_*()
    INTEGER( 8 ) :: DELP( 5 )      !  corner arg array for NFMPI_GET_VARA_*()

!!  INTEGER( MPI_OFFSET_KIND ) :: DIMP( 5 )      !  corner arg array for NFMPI_GET_VARA_*()
!!  INTEGER( MPI_OFFSET_KIND ) :: DELP( 5 )      !  corner arg array for NFMPI_GET_VARA_*()
#endif

    !!***********************************************************************
    !!  begin body of function  RDVARS

    CDFID = CDFID3( FID )
    FNAME = FLIST3( FID )
    IF ( VID .GT. 0 )  THEN
        VNAME = VLIST3( VID,FID )
        VTYPE = VTYPE3( VID,FID )
        VINDX = VINDX3( VID,FID )
    ELSE
        VNAME = ALLVAR3
    END IF

    !!...........   Perform the reads, according to VID

    IF ( CDFID .EQ. BINFIL3 ) THEN    ! native-binary file

        !!  the highest-subscripted nonzero entry of DELS()
        !!  determines STEP:  this may be 1 or 2 (the latter
        !!  for calls from INTERP3() or DDTVAR3())

        I = STEPDEX( MIN( 9, MAX( -2, FTYPE3( FID ) ) ) )

        IF ( I .LT. 0 ) THEN
            IF ( VID .GT. 0 ) THEN
                MESG = 'RDVARS:  bad file type for variable ' // VNAME// 'from file ' // FNAME
            ELSE
                MESG = 'RDVARS:  bad file type for file ' // FNAME
            END IF
            CALL M3MSG2( MESG )
            RDFLAG = .FALSE.
            RETURN
        ELSE
            STEP = DIMS( I )
            NREC = DELS( I )
            LAYR = DIMS( I-1 )
        END IF

        IF ( LAYR .EQ. 1 .AND. DELS(I-1) .EQ. NLAYS3( FID ) ) THEN
            LAYR = ALLAYS3
        END IF

        IF ( VID .GT. 0 ) THEN
            RSIZE = DELTA*TYPSIZE( VTYPE3( VID,FID ) )
        ELSE
            RSIZE = 0
            DO VAR = 1, MAX( 1, NVARS3( FID ) )
                RSIZE = RSIZE + DELTA*TYPSIZE( VTYPE3( VAR,FID ) )
            END DO
        END IF

        EFLAG = .FALSE.
        INDX  = 1

!$OMP CRITICAL( S_NC )
        DO  I = STEP, STEP+NREC-1
            IF ( 0 .EQ. RDBVARS( FID, VID, LAYR, I, BUFFER( INDX ) ) ) THEN
                EFLAG = .TRUE.
            END IF
            INDX = INDX + RSIZE
        END DO
!$OMP END CRITICAL( S_NC )

        IF ( EFLAG  ) THEN     !  error
            IF ( VID .GT. 0 ) THEN
                MESG = 'Error reading variable ' // VNAME // 'from file ' // FNAME
            ELSE
                MESG = 'Error reading ALL VARIABLES from file ' // FNAME
            END IF
            CALL M3MSG2( MESG )
            RDFLAG = .FALSE.
        ELSE
            RDFLAG = .TRUE.
        END IF

        RETURN

    ELSE IF ( FTYPE3( FID ) .EQ. MPIGRD3 ) THEN

#ifndef IOAPI_PNCF
        CALL M3WARN( 'RDVARS',0,0, 'MPI/PnetCDF I/O not enabled in this build' )
        RDFLAG = .FALSE.
        RETURN
#endif

#ifdef  IOAPI_PNCF

        DIMP(1:5) = DIMS(1:5)       !!  convert to type INTEGER( MPI_OFFSET_KIND )
        DELP(1:5) = DELS(1:5)

        IF ( VID .EQ. ALLAYS3 ) THEN

            IERR = NF_NOERR
            INDX = 1    !  starting subscript for BUFFER(*)

            DO  VAR = 1 , NVARS3( FID )

                VTYPE = VTYPE3( VAR,FID )
                VINDX = VINDX3( VAR,FID )

!$OMP           CRITICAL( S_NC )
                IF (      VTYPE .EQ. M3INT  ) THEN
                    IERR = NFMPI_GET_VARA_INT(    CDFID, VINDX, DIMP, DELP, BUFFER( INDX ) )
                ELSE IF ( VTYPE .EQ. M3REAL ) THEN
                    IERR = NFMPI_GET_VARA_REAL(   CDFID, VINDX, DIMP, DELP, BUFFER( INDX ) )
                ELSE IF ( VTYPE .EQ. M3DBLE ) THEN
                    IERR = NFMPI_GET_VARA_DOUBLE( CDFID, VINDX, DIMP, DELP, BUFFER( INDX ) )
                ELSE IF ( VTYPE .EQ. M3INT8 ) THEN
                    IERR = NFMPI_GET_VARA_INT8(   CDFID, VINDX, DIMP, DELP, BUFFER( INDX ) )
                ELSE
                    IERR = NF_EBADTYPE
                END IF
!$OMP           END CRITICAL( S_NC )

                IF ( IERR .NE. 0 ) THEN
                    VNAME = VLIST3( VAR,FID )
                    WRITE( MESG,'( A,I6 )' ) 'RDVARS:  PnetCDF error number', IERR
                    CALL M3MESG( MESG )
                    MESG =  'Error reading variable "'//VNAME//'" from file "'//FNAME//'"'
                    CALL M3MSG2( MESG )
                    RDFLAG = .FALSE.
                    RETURN
                END IF          !  ierr nonzero:  operation failed

                INDX = INDX  +  DELTA * TYPSIZE( VTYPE )

            END DO    !  end loop on variables VAR

        ELSE    !  read a specific variable

            VTYPE = VTYPE3( VID,FID )
            VINDX = VINDX3( VID,FID )

!$OMP       CRITICAL( S_NC )
            IF (      VTYPE .EQ. M3INT  ) THEN
                IERR = NFMPI_GET_VARA_INT(    CDFID, VINDX, DIMP, DELP, BUFFER )
            ELSE IF ( VTYPE .EQ. M3REAL ) THEN
                IERR = NFMPI_GET_VARA_REAL(   CDFID, VINDX, DIMP, DELP, BUFFER )
            ELSE IF ( VTYPE .EQ. M3DBLE ) THEN
                IERR = NFMPI_GET_VARA_DOUBLE( CDFID, VINDX, DIMP, DELP, BUFFER )
            ELSE IF ( VTYPE .EQ. M3INT8 ) THEN
                IERR = NFMPI_GET_VARA_INT8(   CDFID, VINDX, DIMP, DELP, BUFFER )
            ELSE
                IERR = NF_EBADTYPE
            END IF
!$OMP       END CRITICAL( S_NC )

            IF ( IERR .NE. 0 ) THEN
                VNAME = VLIST3( VID,FID )
                WRITE( MESG, '( A,I6 )' ) 'RDVARS:  PnetCDF error number', IERR
                CALL M3MESG( MESG )
                MESG =  'Error reading variable "'//VNAME//'" from file "'//FNAME//'"'
                CALL M3MSG2( MESG )
                RDFLAG = .FALSE.
                RETURN
            END IF          !  ierr nonzero:  operation failed, or succeeded

        END IF  !  read all variables, or read a specific variable

#endif          /*  ifdef IOAPI_PNCF   */

    ELSE IF ( CDFID .GT. 0 ) THEN

        IF ( VID .EQ. ALLAYS3 ) THEN

            IERR = NF_NOERR
            INDX = 1    !  starting subscript for BUFFER(*)

            DO  VAR = 1 , NVARS3( FID )

                VTYPE = VTYPE3( VAR,FID )
                VINDX = VINDX3( VAR,FID )

!$OMP           CRITICAL( S_NC )
                IF (      VTYPE .EQ. M3INT  ) THEN
                    IERR = NF_GET_VARA_INT(    CDFID, VINDX, DIMS, DELS, BUFFER( INDX ) )
                ELSE IF ( VTYPE .EQ. M3REAL ) THEN
                    IERR = NF_GET_VARA_REAL(   CDFID, VINDX, DIMS, DELS, BUFFER( INDX ) )
                ELSE IF ( VTYPE .EQ. M3DBLE ) THEN
                    IERR = NF_GET_VARA_DOUBLE( CDFID, VINDX, DIMS, DELS, BUFFER( INDX ) )
                ELSE IF ( VTYPE .EQ. M3INT8 ) THEN
                    IERR = NF_GET_VARA_INT64( CDFID, VINDX, DIMS, DELS, BUFFER( INDX ) )
                ELSE
                    IERR = NF_EBADTYPE
                END IF
!$OMP           END CRITICAL( S_NC )

                IF ( IERR .NE. 0 ) THEN
                    VNAME = VLIST3( VAR,FID )
                    WRITE( MESG,'( A,I6 )' ) 'RDVARS:  netCDF error number', IERR
                    CALL M3MESG( MESG )
                    MESG =  'Error reading variable "'//VNAME//'" from file "'//FNAME//'"'
                    CALL M3MSG2( MESG )
                    RDFLAG = .FALSE.
                    RETURN
                END IF          !  ierr nonzero:  operation failed

                INDX = INDX  +  DELTA * TYPSIZE( VTYPE3( VAR,FID ) )

            END DO    !  end loop on variables VAR

        ELSE    !  read a specific variable

            VTYPE = VTYPE3( VID,FID )
            VINDX = VINDX3( VID,FID )

!$OMP       CRITICAL( S_NC )
            IERR = NF_NOERR
            IF (      VTYPE .EQ. M3INT  ) THEN
                IERR = NF_GET_VARA_INT(    CDFID, VINDX, DIMS, DELS, BUFFER )
            ELSE IF ( VTYPE .EQ. M3REAL ) THEN
                IERR = NF_GET_VARA_REAL(   CDFID, VINDX, DIMS, DELS, BUFFER )
            ELSE IF ( VTYPE .EQ. M3DBLE ) THEN
                IERR = NF_GET_VARA_DOUBLE( CDFID, VINDX, DIMS, DELS, BUFFER )
            ELSE IF ( VTYPE .EQ. M3INT8 ) THEN
                IERR = NF_GET_VARA_INT64(  CDFID, VINDX, DIMS, DELS, BUFFER )
            ELSE
                IERR = NF_EBADTYPE
            END IF
!$OMP       END CRITICAL( S_NC )

            IF ( IERR .NE. 0 ) THEN
                VNAME = VLIST3( VID,FID )
                WRITE( MESG, '( A,I6 )' ) 'RDVARS:  netCDF error number', IERR
                CALL M3MESG( MESG )
                MESG =  'Error reading variable "'//VNAME//'" from file "'//FNAME//'"'
                CALL M3MSG2( MESG )
                RDFLAG = .FALSE.
                RETURN
            END IF          !  ierr nonzero:  operation failed, or succeeded

        END IF  !  read all variables, or read a specific variable

    ELSE

        WRITE( MESG, '(A,I9,2A)' ) 'RDVARS:  unsupported file type', FTYPE3(FID), ' for ', FNAME
        RDFLAG = .FALSE.
        RETURN

    END IF          !  if native-binary file; else MPIGRD3; else CDF > 0; else...

    RDFLAG = .TRUE.
    RETURN


END FUNCTION RDVARS

