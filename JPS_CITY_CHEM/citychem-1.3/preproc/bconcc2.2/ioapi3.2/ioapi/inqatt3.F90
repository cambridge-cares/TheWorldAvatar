
LOGICAL FUNCTION INQATT3( FNAME, VNAME, MXATTS,     &
                          NATTS, ANAMES, ATYPES, ASIZES )

    !!***********************************************************************
    !! Version "$Id: inqatt3.F90 99 2018-04-05 21:34:05Z coats $"
    !! BAMS/MCNC/EDSS/Models-3 I/O API.
    !! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    !! (C) 2004-2007 Baron Advanced Meteorological Systems,
    !! (C) 2007-2013 Carlie J. Coats, Jr., and
    !! (C) 2014 UNC Institute for the Environment.
    !! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    !! See file "LGPL.txt" for conditions of use.
    !!.........................................................................
    !!  subroutine body starts at line  123
    !!
    !!  FUNCTION:
    !!       returns list of attributes, their types, and sizes for the
    !!       file FNAME and variable VNAME (or ALLVAR3 for "global" file
    !!       attributes).
    !!
    !!  PRECONDITIONS REQUIRED:
    !!       Number of attributes at most MXVARS3( = 120)
    !!
    !!  SUBROUTINES AND FUNCTIONS CALLED:
    !!       netCDF
    !!
    !!  REVISION  HISTORY:
    !!      prototype 1/2002 by Carlie J. Coats, Jr., MCNC-EMC for I/O API v2.2
    !!
    !!      Modified 7/2003 by CJC:  bugfix -- clean up critical sections
    !!      associated with INIT3()
    !!
    !!      Modified 7/2012 by CJC:  bugfix associated with attribute-table
    !!      overflow.  Exclude standard attributes
    !!
    !!      Modified 08/2015 by CJC for I/O API 3.2:  support for M3INT8,
    !!      MPI/PnetCDF, USE MODNCFIO, MODPDATA, NF_*() interfaces.
    !!      F90 "free" source form.
    !!***********************************************************************

    USE MODNCFIO
    USE MODPDATA

    IMPLICIT NONE

    !!...........   INCLUDES:

    INCLUDE 'PARMS3.EXT'      ! I/O API constants
    INCLUDE 'STATE3.EXT'      ! I/O API internal state


    !!...........   ARGUMENTS and their descriptions:

    CHARACTER*(*), INTENT(IN   ) :: FNAME             !  logical file name
    CHARACTER*(*), INTENT(IN   ) :: VNAME             !  vble name, or ALLVARS3
    INTEGER      , INTENT(IN   ) :: MXATTS            !  max number of attributes
    INTEGER      , INTENT(  OUT) :: NATTS             !  number of actual attributes
    CHARACTER*(*), INTENT(  OUT) :: ANAMES( MXATTS )  !  attribute names
    INTEGER      , INTENT(  OUT) :: ATYPES( MXATTS )  !  " types (M3CHAR, M3REAL, M3INT, M3DBLE, M3INT8)
    INTEGER      , INTENT(  OUT) :: ASIZES( MXATTS )  !  " size/length

    !!...........   EXTERNAL FUNCTIONS and their descriptions:

    INTEGER, EXTERNAL :: NAME2FID, INIT3, INDEX1


    !!...........   PARAMETERs:  "standard-M3IO-attributes" table

    INTEGER     , PARAMETER :: NSTDATTS = 37
    CHARACTER*32, PARAMETER ::  STDATTS( NSTDATTS ) =   &    
    (/  'CDATE                            ',            &   
        'CTIME                            ',            &   
        'EXEC_ID                          ',            &   
        'FILEDESC                         ',            &   
        'FTYPE                            ',            &   
        'FTYPE                            ',            &   
        'GDNAM                            ',            &   
        'GDTYP                            ',            &   
        'HISTORY                          ',            &   
        'IOAPI_VERSION                    ',            &   
        'NCOLS                            ',            &   
        'NLAYS                            ',            &   
        'NROWS                            ',            &   
        'NTHIK                            ',            &   
        'NVARS                            ',            &   
        'P_ALP                            ',            &   
        'P_BET                            ',            &   
        'P_GAM                            ',            &   
        'SDATE                            ',            &   
        'STIME                            ',            &   
        'TSTEP                            ',            &   
        'UPNAM                            ',            &   
        'VAR-LIST                         ',            &   
        'VGLVLS                           ',            &   
        'VGTOP                            ',            &   
        'VGTYP                            ',            &   
        'WDATE                            ',            &   
        'WTIME                            ',            &   
        'XCELL                            ',            &   
        'XCENT                            ',            &   
        'XORIG                            ',            &   
        'YCELL                            ',            &   
        'YCENT                            ',            &   
        'YORIG                            ',            &   
        'long_name                        ',            &   
        'units                            ',            &   
        'var_desc                         '  /)

    !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         I, N, F, V      !  subscripts for STATE3 arrays
    INTEGER         FID, VID        !  netCDF ID's
    INTEGER         IERR            !  netCDF error status return
    LOGICAL         EFLAG
    CHARACTER*16    FIL16           !  scratch file-name buffer
    CHARACTER*16    VAR16           !  scratch vble-name buffer
    CHARACTER*32    ANAME           !  scratch vble-name buffer
    CHARACTER*256   MESG            !  message-buffer

#ifdef IOAPI_PNCF
    INCLUDE     'mpif.h'
    INTEGER( MPI_OFFSET_KIND ) :: SIZE
    INTEGER( MPI_OFFSET_KIND ) :: SIZES( MXATTS )
#endif


    !!***********************************************************************
    !!   begin body of subroutine  INQATT3

    !!.......   Check that Models-3 I/O has been initialized:

    EFLAG = .FALSE.
!$OMP   CRITICAL( S_INIT )
    IF ( .NOT. FINIT3 ) THEN
        LOGDEV = INIT3()
        EFLAG  = .TRUE.
    END IF          !  if not FINIT3
!$OMP   END CRITICAL( S_INIT )
    IF ( EFLAG ) THEN
        CALL M3MSG2(  'INQATT3:  I/O API not yet initialized.' )
        INQATT3 = .FALSE.
        RETURN
    END IF

    F   = NAME2FID( FNAME )
    EFLAG = ( F .LE. 0 )

    IF ( LEN_TRIM( VNAME ) .GT. NAMLEN3 ) THEN
        EFLAG = .TRUE.
        MESG  = 'File "'// FNAME// '" Variable "'// VNAME // '"'
        CALL M3MSG2( MESG )
        WRITE( MESG, '( A, I10 )'  ) 'Max vble name length 16; actual:', LEN_TRIM( VNAME )
        CALL M3MSG2( MESG )
    END IF          !  if len( vname ) > 16

    IF ( EFLAG ) THEN
        MESG = 'Invalid variable or file name arguments'
        CALL M3WARN( 'INQATT3', 0, 0, MESG )
        INQATT3 = .FALSE.
        RETURN
    END IF

    VAR16 = VNAME   !  fixed-length-16 scratch copy of name
    FIL16 = FNAME   !  fixed-length-16 scratch copy of name

    IF ( F .EQ. 0 ) THEN  !  file not available

        MESG = 'File "'// FIL16 // '" not yet opened.'
        CALL M3WARN( 'INQATT3', 0, 0, MESG )
        INQATT3 = .FALSE.
        RETURN

    ELSE IF ( CDFID3( F ) .LT. 0 ) THEN

        MESG = 'File:  "' // FIL16 // '" is NOT A NetCDF file.'
        CALL M3WARN( 'INQATT3', 0, 0, MESG )
        INQATT3 = .FALSE.
        RETURN

    ELSE

        FID = CDFID3( F )

    END IF          !  if file not opened, or if readonly, or if volatile

    !!...........   Get ID for variable(s) to be inquired.

    IF ( VAR16 .EQ. ALLVAR3 ) THEN

        VID = NF_GLOBAL

    ELSE

        V = INDEX1( VAR16, NVARS3( F ) , VLIST3( 1,F ) )
        IF ( V .EQ. 0 ) THEN
            MESG = 'Variable "'      // VAR16 // '" not in file "' // FIL16 // '"'
            CALL M3WARN( 'INQATT3', 0, 0, MESG )
            INQATT3 = .FALSE.
            RETURN
        ELSE
            VID = VINDX3( V, F )
        END IF

    END IF          !  if VAR16 is 'ALL', or not.

    !!...........   Inquire attributes for this file and variable:
    !!...........   how many; names; sizes and types:
    !!...........   Somewhat tortured logic-structure due to the fact that
    !!...........   one can't execute a RETURN within a critical section.
           
!$OMP   CRITICAL( S_NC )

    IF ( FTYPE3( F ) .EQ. MPIGRD3 ) THEN
#ifdef IOAPI_PNCF
        IERR = NFMPI_INQ_VARNATTS( FID, VID, NATTS )
#endif
#ifndef IOAPI_PNCF
        CALL M3WARN( 'RDATT3', 0,0, 'PnetCDF/MPI not supported in this build.' )
        EFLAG = .TRUE.
        GO TO 999
#endif
    ELSE
        IERR = NF_INQ_VARNATTS( FID, VID, NATTS )
    END IF

    IF ( IERR .NE. NF_NOERR ) THEN

        MESG = 'Error inquiring attribute count for file "' // FNAME // '" and vble "' // VNAME // '"'
        CALL M3WARN( 'INQATT3', 0, 0, MESG )
        EFLAG = .TRUE.

    ELSE IF ( NATTS .GT. MXATTS+51 ) THEN

        MESG = 'Too many attributes for file "' // FNAME // '" and vble "' // VNAME // '"'
        CALL M3WARN( 'INQATT3', 0, 0, MESG )
        EFLAG = .TRUE.

    ELSE

        N = 0

        DO  I = 1, NATTS

            IF ( FTYPE3( F ) .EQ. MPIGRD3 ) THEN
#ifdef IOAPI_PNCF
                IERR = NFMPI_INQ_ATTNAME( FID, VID, I, ANAME )
#endif
            ELSE
                IERR = NF_INQ_ATTNAME( FID, VID, I, ANAME )
            END IF

            IF ( IERR .NE. NF_NOERR ) THEN

                EFLAG = .TRUE.
                MESG = 'Error inquiring att-name for file "' // FNAME // '" and vble "' // VNAME // '"'
                CALL M3MSG2( MESG )

            ELSE IF ( INDEX1( ANAME,NSTDATTS,STDATTS ) .GT. 0 ) THEN

                CONTINUE        !!  skip over "standard" M3IO attributes

            ELSE IF ( N .LT. MXATTS ) THEN

                N = N + 1
                ANAMES( N ) = ANAME

                IF ( FTYPE3( FID ) .EQ. MPIGRD3 ) THEN
#ifdef IOAPI_PNCF
                    IERR = NFMPI_INQ_ATT( FID, VID, ANAME, ATYPES( N ), SIZES( N ) )
                    ASIZES( N ) = SIZES( N )
#endif
                ELSE
                    IERR = NF_INQ_ATT( FID, VID, ANAME, ATYPES( N ), ASIZES( N ) )
                END IF

                IF ( IERR .NE. NF_NOERR ) THEN
                    EFLAG = .TRUE.
                    MESG = 'Error inquiring type&size: att "' // ANAMES( I ) //         &
                            '" for file "' // FNAME // '" and vble "' // VNAME // '"'
                    CALL M3MSG2( MESG )
                ELSE IF ( ATYPES( N ) .NE. NF_INT     .AND.                     &
                          ATYPES( N ) .NE. NF_FLOAT   .AND.                     &
                          ATYPES( N ) .NE. NF_INT64   .AND.                     &
                          ATYPES( N ) .NE. NF_DOUBLE  ) THEN
                    WRITE( MESG, '( 7A, I9 )' )                                 &
                        'INQATT3 Warning:  File "', FNAME, '" vble "', VNAME,   &
                        '":  att "', ANAME, '" has unsupported TYPE=', ATYPES( N )
                    CALL M3MSG2( MESG )     !!  not necessarily an error...
                END IF

            ELSE

                EFLAG = .TRUE.
                MESG = 'Array overflow inquiring att-names for file "' // FNAME // '" and vble "' // VNAME // '"'
                CALL M3MSG2( MESG )

            END IF

        END DO      !  end loop on attributes for this vble

    END IF

999 CONTINUE

!$OMP   END CRITICAL( S_NC )

    IF ( EFLAG ) THEN
        INQATT3 = .FALSE.
        MESG    = 'Error inquiring atts for file "' // FNAME // '" and vble "' // VNAME // '"'
        CALL M3WARN( 'INQATT3', 0, 0, MESG )
    ELSE
        INQATT3 = .TRUE.
    END IF          !  if errors, or not

    RETURN

END FUNCTION INQATT3

