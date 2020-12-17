
MODULE MODWRFIO

    !!********************************************************************
    !!  Version "$Id: modwrfio.f90 4 2017-06-20 17:43:15Z coats $"
    !!  Copyright (C) 2010-2013 Baron Advanced Meteorological Systems, and
    !!            (C) 2015 UNC Institute for the Environment.
    !!  Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    !!  See file "LGPL.txt" for conditions of use.
    !!....................................................................
    !!  DESCRIPTION:
    !!      Routines and INTERFACEs for reading WRF netCDF files
    !!
    !!  PRECONDITIONS:
    !!      Only manages one WRF-format file at a time:  call CLOSEWRF()
    !!      between processing distinct files.
    !!
    !!      If GRIDNAME argument to OPENWRF() is not BLANK:
    !!          setenv  GRIDDESC  <path name>
    !!
    !!  PUBLIC LOGICAL FUNCTIONS:
    !!      OPENWRF:    open a new "current" WRFIO file for READ
    !!
    !!      CRTWRF:     Open/create a WRFIO file for WRITE
    !!      (not yet implemented)
    !!
    !!      READWRF:    Read a time step of a variable from the
    !!      current WRFIO file.  Generic interface for GRIDDED
    !!      1-D, 2-D, 3-D DOUBLE, REAL and INTEGER variables
    !!
    !!      WRITEWRF:   Write a time step of a variable to the
    !!      current WRFIO file.  Generic interface for GRIDDED
    !!      1-D, 2-D, 3-D DOUBLE, REAL and INTEGER variables
    !!      (not yet implemented)
    !!
    !!      CLOSEWRF:   Close the current WRFIO file
    !!
    !!  REVISION  HISTORY:
    !!      Prototype  9/2010 by Carlie J. Coats, Jr., BAMS --
    !!      builds on "mcip" WRF codes
    !!
    !!      Version  11/2013 by CJC:  Support for additional types of
    !!      WRFIO files (e.g, time independent "geogrid" output)
    !!
    !!      Version  11/2015 by CJC:  support for M3DBLE.  USE MODNCFIO.
    !!********************************************************************

    USE M3UTILIO
    USE MODNCFIO

    IMPLICIT NONE

    PRIVATE


    !!--------  Public Routines in this module:  -----------------------

    PUBLIC   OPENWRF, CRTWRF, READWRF, WRITEWRF, CLOSEWRF


    !!--------  Generic Interfaces:  -----------------------------------

    INTERFACE READWRF
        MODULE PROCEDURE RDWRF1DDBLE, RDWRF2DDBLE, RDWRF3DDBLE,     &
                         RDWRF1DREAL, RDWRF2DREAL, RDWRF3DREAL,     &
                         RDWRF1DINT,  RDWRF2DINT,  RDWRF3DINT
    END INTERFACE READWRF

    INTERFACE WRITEWRF
        MODULE PROCEDURE WRWRF1DDBLE, WRWRF2DDBLE, WRWRF3DDBLE,     &
                         WRWRF1DREAL, WRWRF2DREAL, WRWRF3DREAL,     &
                         WRWRF1DINT,  WRWRF2DINT,  WRWRF3DINT
    END INTERFACE WRITEWRF


    !!--------  Parameters:          -----------------------------------

    INTEGER, PUBLIC, PARAMETER :: MXWRFDIMS =  12
    INTEGER, PUBLIC, PARAMETER :: MXWRFFILE =   1
    INTEGER, PUBLIC, PARAMETER :: MXWRFVARS = 256

    REAL*8 , PUBLIC, PARAMETER :: WRFEARTH = 6370.0D3   ! WRF-ARW Earth Radius [m]

    CHARACTER*16, PARAMETER :: BLANK = ' '
    CHARACTER*64, PARAMETER :: BAR = '-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'

    !!--------  Internal Variables:  -----------------------------------

    INTEGER, SAVE :: LDEV = -1

    !!--------  File-state tables:


    INTEGER, PROTECTED, SAVE :: CDFID  = IMISS3               !  netCDF ID
    INTEGER, PROTECTED, SAVE :: FMODE  = IMISS3               !  file mode:  FSREAD3 or FSRDWR3

    CHARACTER*16,  PUBLIC, PROTECTED, SAVE :: LNAME  = CMISS3        !  logical name for current file
    CHARACTER*512, PUBLIC, PROTECTED, SAVE :: EQNAME = CMISS3        !  path name for current file

    CHARACTER*32, PUBLIC, PROTECTED, SAVE :: VNAMEW( MXWRFVARS )       !  variable names
    CHARACTER*32, PUBLIC, PROTECTED, SAVE :: UNITSW( MXWRFVARS )       !  variable units
    CHARACTER*80, PUBLIC, PROTECTED, SAVE :: VDESCW( MXWRFVARS )       !  variable descriptions
    CHARACTER*4 , PUBLIC, PROTECTED, SAVE :: VSTAGR( MXWRFVARS )       !  variable stagger ("", "X", "Y", "Z", "XY"
    INTEGER     , PUBLIC, PROTECTED, SAVE :: VARIDW( MXWRFVARS )       !  types (NCREAL, etc.)
    INTEGER     , PUBLIC, PROTECTED, SAVE :: VTYPEW( MXWRFVARS )       !  types (NCREAL, etc.)
    INTEGER     , PUBLIC, PROTECTED, SAVE :: DIMCNT( MXWRFVARS )       !  number of dimensions
    INTEGER     , PUBLIC, PROTECTED, SAVE :: VARDIM( MXWRFDIMS, MXWRFVARS ) = 1  !  dimension-extents

    !!--------  Map projection parameters:

    INTEGER, PUBLIC, PROTECTED, SAVE :: GDTYP1 = IMISS3    !  for output window, from GRIDDESC file
    INTEGER, PUBLIC, PROTECTED, SAVE :: NCOLS1
    INTEGER, PUBLIC, PROTECTED, SAVE :: NROWS1
    INTEGER, PUBLIC, PROTECTED, SAVE :: NTHIK1
    REAL*8 , PUBLIC, PROTECTED, SAVE :: P_ALP1
    REAL*8 , PUBLIC, PROTECTED, SAVE :: P_BET1
    REAL*8 , PUBLIC, PROTECTED, SAVE :: P_GAM1
    REAL*8 , PUBLIC, PROTECTED, SAVE :: XCENT1
    REAL*8 , PUBLIC, PROTECTED, SAVE :: YCENT1
    REAL*8 , PUBLIC, PROTECTED, SAVE :: XORIG1
    REAL*8 , PUBLIC, PROTECTED, SAVE :: YORIG1
    REAL*8 , PUBLIC, PROTECTED, SAVE :: XCELL1
    REAL*8 , PUBLIC, PROTECTED, SAVE :: YCELL1

    INTEGER, PUBLIC, PROTECTED, SAVE :: XOFFS1      !!  grid column C is wrf column C+XOFFS1
    INTEGER, PUBLIC, PROTECTED, SAVE :: YOFFS1      !!       row    R               R+YOFFS1
    LOGICAL, PUBLIC, PROTECTED, SAVE :: XSTAGR      !!  this
    LOGICAL, PUBLIC, PROTECTED, SAVE :: YSTAGR
    LOGICAL, PUBLIC, PROTECTED, SAVE :: ZSTAGR

    REAL   , PUBLIC, PROTECTED, SAVE :: LON11W      !!  lon-lat of (1,1) cross point
    REAL   , PUBLIC, PROTECTED, SAVE :: LAT11W

    REAL*8 , PUBLIC, PROTECTED, SAVE :: XORIGW      !!  for input-grid cross points
    REAL*8 , PUBLIC, PROTECTED, SAVE :: YORIGW

    REAL*8 , PUBLIC, PROTECTED, SAVE :: XCENTW
    REAL*8 , PUBLIC, PROTECTED, SAVE :: YCENTW

    INTEGER, PUBLIC, PROTECTED, SAVE :: NCOLSW = IMISS3
    INTEGER, PUBLIC, PROTECTED, SAVE :: NROWSW = IMISS3
    INTEGER, PUBLIC, PROTECTED, SAVE :: NLAYSW = IMISS3
    INTEGER, PUBLIC, PROTECTED, SAVE :: NSOILW = IMISS3
    INTEGER, PUBLIC, PROTECTED, SAVE :: NTIMEW = IMISS3
    INTEGER, PUBLIC, PROTECTED, SAVE :: NVARSW = IMISS3

    INTEGER, PUBLIC, PROTECTED, SAVE :: SDATEW = IMISS3
    INTEGER, PUBLIC, PROTECTED, SAVE :: STIMEW = IMISS3
    INTEGER, PUBLIC, PROTECTED, SAVE :: TSTEPW = IMISS3

    REAL   , PUBLIC, PROTECTED, SAVE :: VGTOPW = BADVAL3

    REAL   , PUBLIC, PROTECTED, SAVE :: TSECSW = BADVAL3

    REAL   , PUBLIC, PROTECTED, ALLOCATABLE, SAVE :: ZSOILW( : )
    REAL   , PUBLIC, PROTECTED, ALLOCATABLE, SAVE :: VGLVSW( : )
    INTEGER, PUBLIC, PROTECTED, ALLOCATABLE, SAVE :: DATESW( : )
    INTEGER, PUBLIC, PROTECTED, ALLOCATABLE, SAVE :: TIMESW( : )


CONTAINS    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-



    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !!  Open WRF-output file with logical name FNAME for read or read/write
    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

    LOGICAL FUNCTION OPENWRF( FNAME, GNAME, FSTATUS )

        !!-----------   Arguments:

        CHARACTER(LEN=*), INTENT(IN   ) :: FNAME        !!  logical file name
        CHARACTER(LEN=*), INTENT(IN   ) :: GNAME        !!  GRIDDESC name for output grid
        INTEGER         , INTENT(IN   ) :: FSTATUS      !!  FSREAD3, FSRDWR3

        CHARACTER*24, PARAMETER :: PNAME = 'MODWRFIO/OPENWRF'

        !!-----------   Local Variables:

        INTEGER         I, J, L, M, N, V
        INTEGER         IERR, FID, FSTAT
        INTEGER         JDATE, JTIME, TSECS
        INTEGER         DIMS( MXWRFDIMS ), DELS( MXWRFDIMS )
        INTEGER         NDIMS, DIMIDS( MXWRFDIMS ), IDIMS( MXWRFDIMS ), VDIMS( MXWRFDIMS )
        INTEGER         NATTS, UDIM, VTYPE
        INTEGER         WRFPROJ
        REAL            X, Y, DX, DY, STANDLON, CENLAT0, CENLAT, CENLON, TRUELAT1, TRUELAT2
        REAL            POLEFAC, CONEFAC

        INTEGER :: GDTYPW
        INTEGER :: NCOLSW
        INTEGER :: NROWSW
        INTEGER :: NTHIKW
        REAL*8  :: P_ALPW
        REAL*8  :: P_BETW
        REAL*8  :: P_GAMW
        REAL*8  :: XCELLW
        REAL*8  :: YCELLW

        REAL*8  XX, YY

        LOGICAL         EFLAG

        CHARACTER*16    CNAME
        CHARACTER*32    VNAME( MXWRFVARS )
        CHARACTER*528   MESG

        REAL,         ALLOCATABLE ::       DZS( : )
        CHARACTER*1,  ALLOCATABLE :: DTCHARS( :,: )           !!  hack necessitated by idiocy in WRF
        CHARACTER*19             ACTUALSTRING


        !!-----------   Body:

        IF ( LDEV .GE. 0 ) THEN
            MESG = 'Redundant call to ' // PNAME
            CALL M3MESG( MESG )
            OPENWRF = .TRUE.
            RETURN
        END IF

        LDEV  = INIT3()

        WRITE( LDEV, '( 5X, A )' ) BAR,                                 &
'Module MODWRFIO to read and write WRFIO netCD output data.',           &
'Subroutine  OPENWRF()',                                                &
'',                                                                     &
'NOTE:  Current version supports at most one input file.',              &
'',                                                                     &
'Module version:',                                                      &
'$Id: modwrfio.f90 4 2017-06-20 17:43:15Z coats $',            &
'',                                                                     &
'Copyright (C) 2013 Baron Advanced Meteorological Systems, LLC.,',      &
'(C) 2015 UNC Institute for the Environment.',                          &
'Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1',  &
'See file "LGPL.txt" for conditions of use.',                           &
BAR, ''

        EFLAG = .FALSE.
        IF ( LNAME .NE. CMISS3 ) THEN
            MESG = 'File "' // TRIM( LNAME ) // '" already opened.  CLOSEWRF() it before opening a new file.'
            CALL M3WARN( PNAME, 0,0, MESG )
            OPENWRF = .FALSE.
            RETURN
        ELSE IF ( FSTATUS .EQ. FSREAD3 ) THEN
            FSTAT = NF_NOWRITE
        ELSE IF ( FSTATUS .EQ. FSRDWR3 ) THEN
            FSTAT = NF_WRITE
        ELSE
            WRITE( MESG, '( A, I10 )' ) 'File status not supported:', FSTATUS
            CALL M3WARN( PNAME, 0,0, MESG )
            OPENWRF = .FALSE.
            RETURN
        END IF

        CALL NAMEVAL( FNAME, EQNAME )
        IERR = NF_OPEN( EQNAME, FSTAT, FID )
        IF ( IERR .NE. 0 ) THEN
            MESG  = NF_STRERROR( IERR )
            CALL M3MESG( MESG )
            MESG = 'Path name "' // TRIM( EQNAME ) // '"'
            CALL M3MESG( MESG )
            WRITE( MESG, '( 3 A, I10 )' ) 'NF_OPEN(', TRIM( FNAME ), ') failure:  IERR=', IERR
            CALL M3WARN( PNAME, 0,0, MESG )
            OPENWRF = .FALSE.
            RETURN
        END IF

        IERR = NF_INQ( FID, NDIMS, NVARSW, NATTS, UDIM )
        IF ( IERR .NE. 0 ) THEN
            MESG  = NF_STRERROR( IERR )
            CALL M3MESG( MESG )
            WRITE( MESG, '( 3 A, I10 )' ) 'NF_INQ_NDIMS(', TRIM( FNAME ), ') failure:  IERR=', IERR
            CALL M3WARN( PNAME, 0,0, MESG )
            OPENWRF = .FALSE.
            RETURN
        ELSE IF ( NDIMS .GT. MXWRFDIMS ) THEN
            MESG = 'File "' // TRIM( FNAME ) // '":  NDIMS exceeds MXWRFDIMS'
            CALL M3WARN( PNAME, 0,0, MESG )
            OPENWRF = .FALSE.
            RETURN
        ELSE IF ( NVARSW .GT. MXWRFVARS ) THEN
            MESG = 'File "' // TRIM( FNAME ) // '":  NVARS exceeds MXWRFVARS'
            CALL M3WARN( PNAME, 0,0, MESG )
            OPENWRF = .FALSE.
            RETURN
        END IF

        DO N = 1, NDIMS

            IERR = NF_INQ_DIMLEN( FID, N, IDIMS(N) )
            IF ( IERR .NE. 0 ) THEN
                MESG  = NF_STRERROR( IERR )
                CALL M3MESG( MESG )
                WRITE( MESG, '( 3A, I10 )' ) 'NF_INQ_DIMLEN(', TRIM( FNAME ), ') failure:  IERR=', IERR
                CALL M3WARN( PNAME, 0,0, MESG )
                OPENWRF = .FALSE.
                RETURN
            END IF

        END DO

        NTIMEW = IDIMS( UDIM )

        IERR = NF_INQ_DIMID( FID, 'west_east', M )
        IF ( IERR .NE. NF_NOERR ) THEN
            MESG  = NF_STRERROR( IERR )
            CALL M3MESG( MESG )
            WRITE( MESG, '( A, I10 )' ) 'Error finding dimension "west_east".  IERR=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        ELSE
            NCOLSW = IDIMS( M )
        END IF

        IERR = NF_INQ_DIMID( FID, 'south_north', M )
        IF ( IERR .NE. NF_NOERR ) THEN
            MESG  = NF_STRERROR( IERR )
            CALL M3MESG( MESG )
            WRITE( MESG, '( A, I10 )' ) 'Error finding dimension "south_north".  IERR=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        ELSE
            NROWSW = IDIMS( M )
        END IF

        IERR = NF_INQ_DIMID( FID, 'bottom_top_stag', M )
        IF ( IERR .EQ. NF_EBADDIM ) THEN
            WRITE( MESG, '( A )' ) 'Dimension "bottom_top_stag" not found.  Proceeding..'
            CALL M3MESG( MESG )
            CALL M3MESG( 'NLAYSW assumed to be 1' )
            NLAYSW = 1
            ZSTAGR = .FALSE.
        ELSE IF ( IERR .NE. NF_NOERR ) THEN
            MESG  = NF_STRERROR( IERR )
            CALL M3MESG( MESG )
            WRITE( MESG, '( A, I10 )' ) 'Error reading dimension "bottom_top_stag".  IERR=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        ELSE
            NLAYSW = IDIMS( M ) - 1
            ZSTAGR = .TRUE.
        END IF

        IERR = NF_INQ_DIMID( FID, 'soil_layers_stag', M )
        IF ( IERR .EQ. NF_EBADDIM ) THEN
            WRITE( MESG, '( A )' ) 'Dimension "soil_layers_stag" not found; proceeding'
            CALL M3MESG( MESG )
            CALL M3MESG( 'NSOILW assumed to be 1' )
            NSOILW = 1
        ELSE IF ( IERR .NE. NF_NOERR ) THEN
            MESG  = NF_STRERROR( IERR )
            WRITE( MESG, '( A, I10 )' ) 'Error reading dimension "soil_layers_stag".  IERR=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        ELSE
            NSOILW = IDIMS( M )
        END IF

        DO V = 1, NVARSW

            VARIDW(V) = V
            IERR = NF_INQ_VAR( FID, V, VNAMEW(V), VTYPEW(V), DIMCNT(V), VDIMS, NATTS )
            IF ( IERR .NE. NF_NOERR ) THEN
                MESG  = NF_STRERROR( IERR )
                CALL M3MESG( MESG )
                WRITE( MESG, '(3A, I10 )' ) 'NF_INQ_VAR(', TRIM( FNAME ), ') failure:  IERR=', IERR
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
                CYCLE
            END IF

            DO N = 1, DIMCNT(V)
                VARDIM( N,V ) = IDIMS( VDIMS(N) )
            END DO

            IF ( VNAMEW(V) .EQ. 'Times' )  THEN
                UNITSW( V ) = 'n/a'
                VDESCW( V ) = 'YYYY-MM-DD-HH:MM:SS'
                VSTAGR( V ) = ''
                CYCLE
            END IF

            IERR = NF_GET_ATT_TEXT( FID, V, 'units', UNITSW( V ) )
            IF ( IERR .NE. NF_NOERR ) THEN
                MESG  = NF_STRERROR( IERR )
                CALL M3MESG( MESG )
                WRITE( MESG, '(3A, I10 )' ) 'Failure getting attribute "units" for "', TRIM( VNAMEW(V) ), '" :  IERR=', IERR
                CALL M3MESG( MESG )
                UNITSW( V ) = CMISS3
            ELSE
                CALL NULLFIX( UNITSW( V ) )
            END IF

            IERR = NF_GET_ATT_TEXT( FID, V, 'description', VDESCW( V ) )
            IF ( IERR .NE. NF_NOERR ) THEN
                MESG  = NF_STRERROR( IERR )
                CALL M3MESG( MESG )
                WRITE( MESG, '(3A, I10 )' ) 'Failure getting attribute "description" for "', TRIM( VNAMEW(V) ), '" :  IERR=', IERR
                CALL M3MESG( MESG )
                VDESCW( V ) = CMISS3
            ELSE
                CALL NULLFIX( VDESCW( V ) )
            END IF

            IERR = NF_GET_ATT_TEXT( FID, V, 'stagger', VSTAGR( V ) )
            IF ( IERR .NE. NF_NOERR ) THEN
                MESG  = NF_STRERROR( IERR )
                CALL M3MESG( MESG )
                WRITE( MESG, '(3A, I10 )' ) 'Failure getting attribute "stagger" for "', TRIM( VNAMEW(V) ), '" :  IERR=', IERR
                CALL M3MESG( MESG )
                VSTAGR( V ) = BLANK
            ELSE
                CALL NULLFIX( VSTAGR( V ) )
            END IF

        END DO


        IF ( EFLAG ) THEN
            MESG = 'Error(s) reading file header for "' // TRIM( FNAME ) // '"'
            CALL M3WARN( PNAME, 0, 0, MESG )
            OPENWRF = .FALSE.
            RETURN
        END IF


        ALLOCATE( DTCHARS( 19, NTIMEW ),        &
                       DATESW( NTIMEW ),        &
                       TIMESW( NTIMEW ),        &
                          DZS( NSOILW ),        &
                       ZSOILW( NSOILW+1 ),      &
                       VGLVSW( NLAYSW+1 ), STAT = IERR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( A, I10 )' ) 'Error allocating coordinate buffers.  STAT=', IERR
            CALL M3WARN( PNAME, 0, 0, MESG )
            OPENWRF = .FALSE.
            RETURN
        END IF


        !!-----------   Get timestep sequence

        IERR = NF_GET_ATT_REAL( FID, NF_GLOBAL, 'DT', X )
        IF ( IERR .NE. NF_NOERR ) THEN
            CALL M3MESG( 'No attribute "DT" for file:  assuming time independent' )
            STIMEW = 0
            SDATEW = 0
            TSECSW = 0.0
            TSTEPW = 0
        ELSE

            TSECSW = DBLFIX( X )

            V = INDEX1( 'Times', NVARSW, VNAMEW )
            IF ( V .LE. 0 ) THEN
                WRITE( MESG, '( A, I10 )' ) 'Error finding time-vble "Times" for file.'
                CALL M3WARN( PNAME, 0,0, MESG )
                OPENWRF = .FALSE.
                RETURN
            END IF

            DIMS( 1 ) = 1
            DELS( 1 ) = 19
            DIMS( 2 ) = 1
            DELS( 2 ) = NTIMEW
            IERR = NF_GET_VARA_TEXT( FID, V, DIMS, DELS, DTCHARS )
            IF ( IERR .NE. NF_NOERR ) THEN
                MESG  = NF_STRERROR( IERR )
                CALL M3MESG( MESG )
                WRITE( MESG, '( A, I10 )' ) 'Error reading time-vble "Times" for file.  IERR=', IERR
                CALL M3MESG( MESG )
                MESG  = NF_STRERROR( IERR )
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

            DO I = 1, NTIMEW

                DO J = 1, 19
                   ACTUALSTRING( J:J ) = DTCHARS( J,I )
                END DO

                CALL DATETIME( ACTUALSTRING, DATESW( I ), TIMESW( I ) )

            END DO

            DEALLOCATE( DTCHARS )

            STIMEW = TIMESW( 1 )
            SDATEW = DATESW( 1 )
            IF ( NTIMEW .GT. 1 ) THEN
                TSTEPW = SEC2TIME( SECSDIFF( SDATEW, STIMEW, DATESW( 2 ), TIMESW( 2 ) ) )
            ELSE
                TSTEPW = 0
            END IF

        END IF          !!  if attribute "DT" fails; or if it exists


        !!-----------   Get vertical atmospheric, soil coordinates

        V = INDEX1( 'ZNW', NVARSW, VNAMEW )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'Vertical coord-vble ZNW for file not found.  Proceeding...' )
        ELSE IF ( .NOT. ZSTAGR ) THEN
            CALL M3WARN( PNAME, 0,0, 'Inconsistent dimensions for  coord-vble ZNW for file.' )
            EFLAG = .TRUE.
        ELSE

            DIMS( 1 ) = 1
            DELS( 1 ) = NLAYSW+1
            DIMS( 2 ) = 1
            DELS( 2 ) = 1

            IERR = NF_GET_VARA_REAL( FID, V, DIMS, DELS,  VGLVSW )
            IF ( IERR .NE. NF_NOERR ) THEN
                MESG  = NF_STRERROR( IERR )
                CALL M3MESG( MESG )
                WRITE( MESG, '( A, I10 )' ) 'Error reading vertical coord-vble "ZNW".  IERR=', IERR
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

        END IF


        V = INDEX1( 'DZS', NVARSW, VNAMEW )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG( 'Soil-thickness coordinate DZS for file not found.  Proceeding...' )
        ELSE

            DIMS( 1 ) = 1
            DELS( 1 ) = NSOILW
            DIMS( 2 ) = 1
            DELS( 2 ) = 1

            IERR = NF_GET_VARA_REAL( FID, V, DIMS, DELS, DZS )
            IF ( IERR .NE. NF_NOERR ) THEN
                MESG  = NF_STRERROR( IERR )
                CALL M3MESG( MESG )
                    WRITE( MESG, '( A, I10 )' ) 'Error reading vertical coord-vble "DZS".  IERR=', IERR
                CALL M3MESG( MESG )
                !! EFLAG = .TRUE.
            END IF

        END IF


        !!-----------   Get lat-lon of (1,1) cross-point

        V = INDEX1( 'XLONG', NVARSW, VNAMEW )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG(  'Longitude coord-vble XLONG not found in file.  Proceeding...' )
        ELSE

            DIMS( 1 ) = 1
            DIMS( 2 ) = 1
            DIMS( 3 ) = 1

            IERR = NF_GET_VAR1_REAL( FID, V, DIMS, LON11W )
            IF ( IERR .NE. NF_NOERR ) THEN
                MESG  = NF_STRERROR( IERR )
                CALL M3MESG( MESG )
                WRITE( MESG, '( A, I10 )' ) 'Error reading longitude coord-vble XLONG.  IERR=', IERR
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

        END IF

        V = INDEX1( 'XLAT', NVARSW, VNAMEW )
        IF ( V .LE. 0 ) THEN
            CALL M3MESG(  'Latitude coord-vble XLAT not found for file.  Proceeding...' )
        ELSE

            DIMS( 1 ) = 1
            DIMS( 2 ) = 1
            DIMS( 3 ) = 1

            IERR = NF_GET_VAR1_REAL( FID, V, DIMS, LAT11W )
            IF ( IERR .NE. NF_NOERR ) THEN
                MESG  = NF_STRERROR( IERR )
                CALL M3MESG( MESG )
                WRITE( MESG, '( A, I10 )' ) 'Error reading reading latitude coord-vble XLAT.  IERR=', IERR
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            END IF

        END IF


        !!-----------   Get/convert map projection metadata:

        IERR = NF_GET_ATT_INT( FID, NF_GLOBAL, 'MAP_PROJ', WRFPROJ )
        IF ( IERR .NE. NF_NOERR ) THEN
            MESG  = NF_STRERROR( IERR )
            CALL M3MESG( MESG )
            WRITE( MESG, '( A, I10 )' ) 'Error finding MAP_PROJ for file.  IERR=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF

        IERR = NF_GET_ATT_REAL( FID, NF_GLOBAL, 'DX', DX )
        IF ( IERR .NE. NF_NOERR ) THEN
            MESG  = NF_STRERROR( IERR )
            CALL M3MESG( MESG )
            WRITE( MESG, '( A, I10 )' ) 'Error finding DX for file.  IERR=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF

        IERR = NF_GET_ATT_REAL( FID, NF_GLOBAL, 'DY', DY )
        IF ( IERR .NE. NF_NOERR ) THEN
            MESG  = NF_STRERROR( IERR )
            CALL M3MESG( MESG )
            WRITE( MESG, '( A, I10 )' ) 'Error finding DY for file.  IERR=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF

        IERR = NF_GET_ATT_REAL( FID, NF_GLOBAL, 'STAND_LON', STANDLON )
        IF ( IERR .NE. NF_NOERR ) THEN
            MESG  = NF_STRERROR( IERR )
            CALL M3MESG( MESG )
            WRITE( MESG, '( A, I10 )' ) 'Error finding STAND_LON for file.  IERR=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF

        IERR = NF_GET_ATT_REAL( FID, NF_GLOBAL, 'MOAD_CEN_LAT', CENLAT0 )
        IF ( IERR .NE. NF_NOERR ) THEN
            MESG  = NF_STRERROR( IERR )
            CALL M3MESG( MESG )
            WRITE( MESG, '( A, I10 )' ) 'Error finding MOAD_CEN_LAT for file.  IERR=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF

        IERR = NF_GET_ATT_REAL( FID, NF_GLOBAL, 'CEN_LON', CENLON )
        IF ( IERR .NE. NF_NOERR ) THEN
            MESG  = NF_STRERROR( IERR )
            CALL M3MESG( MESG )
            WRITE( MESG, '( A, I10 )' ) 'Error finding CEN_LON for file.  IERR=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF

        IERR = NF_GET_ATT_REAL( FID, NF_GLOBAL, 'CEN_LAT', CENLAT )
        IF ( IERR .NE. NF_NOERR ) THEN
            MESG  = NF_STRERROR( IERR )
            CALL M3MESG( MESG )
            WRITE( MESG, '( A, I10 )' ) 'Error finding CEN_LAT for file.  IERR=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF

        IERR = NF_GET_ATT_REAL( FID, NF_GLOBAL, 'TRUELAT1', TRUELAT1 )
        IF ( IERR .NE. NF_NOERR ) THEN
            MESG  = NF_STRERROR( IERR )
            CALL M3MESG( MESG )
            WRITE( MESG, '( A, I10 )' ) 'Error finding TRUELAT1 for file.  IERR=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF

        IERR = NF_GET_ATT_REAL( FID, NF_GLOBAL, 'TRUELAT2', TRUELAT2 )
        IF ( IERR .NE. NF_NOERR ) THEN
            MESG  = NF_STRERROR( IERR )
            CALL M3MESG( MESG )
            WRITE( MESG, '( A, I10 )' ) 'Error finding TRUELAT2 for file.  IERR=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF


        IF ( EFLAG ) THEN
            CALL M3WARN( PNAME, 0, 0, 'Error(s) reading grid parameters' )
            OPENWRF = .FALSE.
            RETURN
        END IF

        !!-----------   Get/convert map projection metadata:

        XCELLW = DBLFIX( DX )
        YCELLW = DBLFIX( DY )

        IF ( WRFPROJ .EQ. 1 ) THEN
            GDTYPW = LAMGRD3
            P_ALPW = DBLFIX( MIN( TRUELAT1, TRUELAT2 ) )
            P_BETW = DBLFIX( MAX( TRUELAT1, TRUELAT2 ) )
            P_GAMW = DBLFIX( STANDLON )
            XCENTW = DBLFIX( STANDLON )
            YCENTW = DBLFIX( CENLAT0 )
        ELSE IF ( WRFPROJ .EQ. 2 ) THEN
            GDTYPW = POLGRD3
            P_ALPW = SIGN( 1.0D0, DBLE( CENLAT ) )
            P_BETW = DBLFIX( TRUELAT1 )
            P_GAMW = DBLFIX( STANDLON )
            XCENTW = DBLFIX( STANDLON )
            YCENTW = DBLFIX( CENLAT0 )
        ELSE IF ( WRFPROJ .EQ. 3 ) THEN
            GDTYPW = EQMGRD3
            P_ALPW = DBLFIX( MAX( TRUELAT1, TRUELAT2 ) )
            P_BETW = 0.0D0
            P_GAMW = DBLFIX( STANDLON )
            XCENTW = DBLFIX( STANDLON )
            YCENTW = DBLFIX( CENLAT0 )
        ELSE
            WRITE( MESG, '( A, I10 )' ) 'Unrecognized/unsupported map projection', WRFPROJ
            CALL M3WARN( PNAME, 0, 0, MESG )
            OPENWRF = .FALSE.
            RETURN
        END IF

        IF ( GNAME .EQ. BLANK ) THEN
            CALL M3MESG( BLANK )
            CALL M3MESG( 'Using grid description from file' )
            CALL M3MESG( BLANK )

            GDTYP1 = GDTYPW
            NCOLS1 = NCOLSW
            NROWS1 = NROWSW
            NTHIK1 =      1
            P_ALP1 = P_ALPW
            P_BET1 = P_BETW
            P_GAM1 = P_GAMW
            XCENT1 = P_GAM1
            YCENT1 = YCENTW
            XCELL1 = XCELLW
            YCELL1 = YCELLW
            XOFFS1 = 0.0D0
            YOFFS1 = 0.0D0

            CALL GETORIG( LON11W, LAT11W )

            XORIG1 = XORIGW
            YORIG1 = YORIGW

        ELSE

            IF ( .NOT.DSCGRID( GNAME, CNAME, GDTYP1,                   &
                               P_ALP1, P_BET1,P_GAM1, XCENT1, YCENT1,  &
                               XORIG1, YORIG1, XCELL1, YCELL1,         &
                               NCOLS1, NROWS1, NTHIK1 ) ) THEN
                EFLAG = .TRUE.
                MESG  = '"' // TRIM( GNAME ) // '" not found in GRIDDESC file'
                CALL M3MESG( '"OUTGRID" not found in GRIDDESC file' )
            ELSE
                MESG = 'GRIDNAME FOUND: ' // TRIM( GNAME )
                CALL M3MESG( BLANK )
                CALL M3MESG( MESG )
                CALL M3MESG( BLANK )
            END IF

            IF ( GDTYP1 .NE. GDTYPW ) THEN
                CALL M3MESG( 'Map projection-type mismatch' )
                EFLAG = .TRUE.
            END IF

            IF ( DBLERR( P_ALPW , P_ALP1 ) ) THEN
                CALL M3MESG( 'Map projection parameter P_ALP mismatch' )
                EFLAG = .TRUE.
            END IF

            IF ( DBLERR( P_BETW , P_BET1 ) ) THEN
                CALL M3MESG( 'Map projection parameter P_BET mismatch' )
                EFLAG = .TRUE.
            END IF

            IF ( DBLERR( P_GAMW , P_GAM1 ) ) THEN
                CALL M3MESG( 'Map projection parameter P_GAM mismatch' )
                EFLAG = .TRUE.
            END IF

            IF ( DBLERR( XCELLW , XCELL1 ) ) THEN
                CALL M3MESG( 'Map projection parameter XCELL mismatch' )
                EFLAG = .TRUE.
            END IF

            IF ( DBLERR( YCELLW , YCELL1 ) ) THEN
                CALL M3MESG( 'Map projection parameter YCELL mismatch' )
                EFLAG = .TRUE.
            END IF

            !! CALL GETORIG( LON11W, LAT11W )

            XORIGW = XORIG1
            YORIGW = YORIG1

            XX = ( XORIG1 - XORIGW ) / XCELL1
            YY = ( YORIG1 - YORIGW ) / YCELL1

            !!  <hack>:  need to use DBLERR() away from zero:

            IF ( .NOT.DBLERR( DBLE( NCOLS1+1 ) + XX, DBLE( NCOLS1+1 + NINT( XX ) ) ) ) THEN
                 CALL M3MESG( 'Output grid is non-staggered in X' )
                 XOFFS1 = NINT( XX )
                 XSTAGR = .FALSE.
            ELSE IF ( .NOT.DBLERR( XX+0.5D0, DBLE( NINT( XX+0.5D0 ) ) ) ) THEN
                 CALL M3MESG( 'Output grid is staggered in X' )
                 XOFFS1 = NINT( XX+0.5D0 )
                 XSTAGR = .TRUE.
            ELSE
                CALL M3WARN( PNAME, 0, 0, 'Error(s) in X grid alignment' )
                EFLAG = .TRUE.
            END IF

            IF ( .NOT.DBLERR( DBLE( NROWS1+1 ) + YY, DBLE( NROWS1+1 + NINT( YY ) ) ) ) THEN
                 CALL M3MESG( 'Output grid is non-staggered in Y' )
                 YOFFS1 = NINT( YY )
                 YSTAGR = .FALSE.
            ELSE IF ( .NOT.DBLERR( YY+0.5D0, DBLE( NINT( YY+0.5D0 ) ) ) ) THEN
                 CALL M3MESG( 'Output grid is staggered in Y' )
                 YOFFS1 = NINT( YY+0.5D0 )
                 YSTAGR = .TRUE.
            ELSE
                CALL M3WARN( PNAME, 0, 0, 'Error(s) in Y grid alignment' )
                EFLAG = .TRUE.
            END IF

        END IF          !!  if GNAME is blank, or not

        IF ( EFLAG ) THEN
            CALL M3WARN( PNAME, 0, 0, 'Error(s) in grid/projection metadata' )
            OPENWRF = .FALSE.
            RETURN
        END IF

        LNAME = FNAME
        CDFID = FID
        FMODE = FSTATUS

        CALL M3MESG( BAR )
        MESG = '"' // TRIM( FNAME ) // '" opened for read'
        CALL M3MESG( MESG )
        MESG = 'File name "' // TRIM( EQNAME ) // '"'
        CALL M3MESG( MESG )
        WRITE( MESG, '(A, I9.7, A, I6.6, 2X, A, I8.6, 2X, A, I10 )' )   &
                'Starting date&time', SDATEW, ':', STIMEW, 'timestep', TSTEPW, 'nrecs', NTIMEW
        CALL M3MESG( MESG )
        CALL M3MESG( 'Mass-Point grid parameters:' )
        WRITE( MESG, '( 5( A, :, I6      ) )' ) '"OUTGRID proj type  = ', GDTYPW
        CALL M3MESG( MESG )
        WRITE( MESG, '( 5( A, :, 1PD24.17 ) )' ) '<P_ALP:P_BET:P_GAM>=<', P_ALPW, ':', P_BETW, ':', P_GAMW, '>'
        CALL M3MESG( MESG )
        WRITE( MESG, '( 5( A, :, 1PD24.17 ) )' ) '<XCENT:YCENT>      =<', XCENTW, ':', YCENTW, '>'
        CALL M3MESG( MESG )
        WRITE( MESG, '( 5( A, :, 1PD24.17 ) )' ) '<XORIG:YORIG>      =<', XORIGW, ':', YORIGW, '>'
        CALL M3MESG( MESG )
        WRITE( MESG, '( 5( A, :, 1PD24.17 ) )' ) '<XCELL:YCELL>      =<', XCELLW, ':', YCELLW, '>'
        CALL M3MESG( MESG )
        WRITE( MESG, '( 5( A, :, I6       ) )' ) '<NC   :   NR>      =<', NCOLSW, ':', NROWSW, '>'
        CALL M3MESG( MESG )
        WRITE( MESG, '( 5( A, :, I6       ) )' ) '<NLAYS:NSOIL>      =<', NLAYSW, ':', NSOILW, '>'
        CALL M3MESG( MESG )
        CALL M3MESG( BAR )

        OPENWRF = .TRUE.
        RETURN

    END FUNCTION OPENWRF



    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !!  Open new or unknown WRF-output file with logical name FNAME for read/write
    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

    LOGICAL FUNCTION CRTWRF( FNAME, GNAME, FSTATUS )

        !!-----------   Arguments:

        CHARACTER(LEN=*), INTENT(IN   ) :: FNAME        !!  logical file name
        CHARACTER(LEN=*), INTENT(IN   ) :: GNAME        !!  GRIDDESC name for output grid
        INTEGER         , INTENT(IN   ) :: FSTATUS      !!  FSUNKN3, FSCREA3, FSNEW3

        CHARACTER*24, PARAMETER :: PNAME = 'MODWRFIO/CRTWRF'

        !!-----------   Local Variables:

        INTEGER         I, J, L, M, N, V
        INTEGER         IERR, FID, FSTAT
        CHARACTER*256   MESG

        !!-----------   Body:

        IF ( FSTATUS .EQ. FSCREA3 ) THEN
            FSTAT = NF_WRITE + NF_NOCLOBBER
        ELSE IF ( FSTATUS .EQ. FSUNKN3 ) THEN
            FSTAT = NF_WRITE
        ELSE IF ( FSTATUS .EQ. FSNEW3 ) THEN
            FSTAT = NF_WRITE + NF_NOCLOBBER
        ELSE
            WRITE( MESG, '( A, I10 )' ) 'File status', FSTATUS,  'not supported for creating "' // TRIM( FNAME ) // '"'
            CALL M3WARN( PNAME, 0,0, MESG )
            CRTWRF = .FALSE.
            RETURN
        END IF

        CALL NAMEVAL( FNAME, EQNAME )
!       IERR = NF_OPEN( EQNAME, FSTAT, FID )
!       IF ( IERR .NE. 0 ) THEN
!           MESG  = NF_STRERROR( IERR )
!           CALL M3MESG( MESG )
!           MESG = 'Path name "' // TRIM( EQNAME ) // '"'
!           CALL M3MESG( MESG )
!           WRITE( MESG, '( 3 A, I10 )' ) 'NF_OPEN(', TRIM( FNAME ), ') failure:  IERR=', IERR
!           CALL M3WARN( PNAME, 0,0, MESG )
!           OPENWRF = .FALSE.
!           RETURN
!       END IF

        LNAME = FNAME
        FMODE = FSRDWR3
        CALL M3WARN( PNAME, 0,0, 'MODWRFIO/CRTWRF() not yet implemented' )
        CRTWRF = .FALSE.
        RETURN

    END FUNCTION CRTWRF



    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !!  Close WRF-output file with logical name FNAME for read or read/write
    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

    LOGICAL FUNCTION CLOSEWRF( FNAME )

        !!-----------   Arguments:

        CHARACTER(LEN=*), INTENT(IN   ) :: FNAME        !!  logical file name

        CHARACTER*24, PARAMETER :: PNAME = 'MODWRFIO/CLOSEWRF'

        !!-----------   Local Variables

        INTEGER         FILE
        INTEGER         IERR            !  netCDF error status return
        CHARACTER*256   MESG

        !!-----------   Body:

        IF ( CDFID .EQ. IMISS3 ) THEN
            CALL M3MESG( 'MODWRFIO/CLOSEWRF:  no files open; returning' )
            CLOSEWRF = .TRUE.
            RETURN
        END IF

        IF ( FNAME .NE. LNAME ) THEN
            MESG = 'MODWRFIO/CLOSEWRF:  file "' // TRIM( FNAME ) // '" does not match "' // TRIM( LNAME ) // '"'
            CALL M3MESG( MESG )
            CLOSEWRF = .FALSE.
            RETURN
        END IF

        IERR = NF_CLOSE( CDFID )
        IF ( IERR .NE. NF_NOERR ) THEN
            MESG  = NF_STRERROR( IERR )
            CALL M3MESG( MESG )
            WRITE( MESG, '( 3 A, I10 )' ) 'Error closing file "', TRIM( FNAME ), '"  IERR=', IERR
            CALL M3MESG( MESG )
            CLOSEWRF = .FALSE.
            RETURN
        END IF

        CDFID  = IMISS3
        FMODE  = IMISS3
        LNAME  = CMISS3
        EQNAME = CMISS3

        MESG = 'MODWRFIO/CLOSEWRF:  file "' // TRIM( FNAME ) // '" closed.'
        CALL M3MESG( MESG )
        CLOSEWRF = .TRUE.

        RETURN

    END FUNCTION CLOSEWRF


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !!  MODULE PROCEDURES for generic READWRF():
    !!  Read variable VNAME for date&time JDATE:JTIME
    !!  1-D/2-D/3-D ; DBLE/REAL/INTEGER cases
    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

    LOGICAL FUNCTION RDWRF1DDBLE( VNAME, JDATE, JTIME, NSIZE, BUF )

        !!-----------   Arguments:

        CHARACTER(LEN=*), INTENT(IN   ) :: VNAME
        INTEGER         , INTENT(IN   ) :: JDATE, JTIME
        INTEGER         , INTENT(IN   ) :: NSIZE
        REAL*8          , INTENT(  OUT) :: BUF( NSIZE )

        CHARACTER*24, PARAMETER :: PNAME = 'MODWRFIO/RDWRF1DDBLE'

        !!-----------   Local Variables:

        INTEGER         IERR, V, FID, VID, L, T
        INTEGER         DIMS( MXWRFDIMS ), DELS( MXWRFDIMS )
        CHARACTER*256   MESG

        !!-----------   Body:

        IF ( CDFID .EQ. IMISS3 ) THEN
            MESG  = '*** File not yet open for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            RDWRF1DDBLE = .FALSE.
            RETURN
        ELSE IF ( .NOT.CHECKNAME( VNAME, V ) ) THEN
            MESG  = '*** Bad file setup for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            RDWRF1DDBLE = .FALSE.
            RETURN
        ELSE IF ( DIMCNT( V ) .GT. 2 ) THEN
            MESG  = '*** Bad DIMENSION-COUNT for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            RDWRF1DDBLE = .FALSE.
            RETURN
        ELSE IF ( VARDIM( 1,V ) .NE. NSIZE ) THEN
            MESG  = '*** Bad DIMENSION for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            RDWRF1DDBLE = .FALSE.
            RETURN
        ELSE IF ( VTYPEW( V ) .NE. M3DBLE ) THEN
            MESG  = '*** Bad TYPE for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            RDWRF1DDBLE = .FALSE.
            RETURN
        END IF

        T = TINDEX( JDATE, JTIME )
        IF ( T .EQ. 0 ) THEN
            WRITE( MESG, '(3 A, I9.7, A, I6.6 )' ) '*** Timestep not available for "', TRIM( VNAME ),  '" at', JDATE, ':', JTIME
            CALL M3MESG( MESG )
            RDWRF1DDBLE = .FALSE.
            RETURN
        END IF

        FID = CDFID
        VID = VARIDW( V )
        DIMS( 1 ) = 1
        DELS( 1 ) = VARDIM( 1,V )
        DIMS( 2 ) = T
        DELS( 2 ) = 1

        IERR = NF_GET_VARA_DOUBLE( FID, VID, DIMS, DELS, BUF )
        IF ( IERR .NE. NF_NOERR ) THEN
            MESG  = NF_STRERROR( IERR )
            CALL M3MESG( MESG )
            WRITE( MESG, '( 3 A, I10 )' ) 'NF_GET_VARA_DOUBLE(', TRIM( VNAME ), ') failure:  IERR=', IERR
            CALL M3WARN( PNAME, 0,0, MESG )
            RDWRF1DDBLE = .FALSE.
            RETURN
        END IF

        RDWRF1DDBLE = .TRUE.
        RETURN

    END FUNCTION RDWRF1DDBLE



    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



    LOGICAL FUNCTION RDWRF2DDBLE( VNAME, JDATE, JTIME, NCOLS, NROWS, BUF )

        !!-----------   Arguments:

        CHARACTER(LEN=*), INTENT(IN   ) :: VNAME
        INTEGER         , INTENT(IN   ) :: JDATE, JTIME
        INTEGER         , INTENT(IN   ) :: NCOLS, NROWS
        REAL*8          , INTENT(  OUT) :: BUF( NCOLS, NROWS )

        CHARACTER*24, PARAMETER :: PNAME = 'MODWRFIO/RDWRF2DDBLE'

        !!-----------   Local Variables:

        INTEGER         IERR, V, FID, VID, L, T
        INTEGER         DIMS( MXWRFDIMS ), DELS( MXWRFDIMS )
        CHARACTER*256   MESG

        !!-----------   Body:

        IF ( CDFID .EQ. IMISS3 ) THEN
            MESG  = '*** File not yet open for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            RDWRF2DDBLE = .FALSE.
            RETURN
        ELSE IF ( .NOT.CHECKNAME( VNAME, V ) ) THEN
            MESG  = '*** Bad file setup for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            RDWRF2DDBLE = .FALSE.
            RETURN
        ELSE IF ( DIMCNT( V ) .GT. 3 ) THEN
            MESG  = '*** Bad DIMENSION-COUNT for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            RDWRF2DDBLE = .FALSE.
            RETURN
        ELSE IF ( VARDIM( 1,V ) .NE. NCOLS .OR. VARDIM( 2,V ) .NE. NROWS ) THEN
            MESG  = '*** Bad DIMENSION for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            RDWRF2DDBLE = .FALSE.
            RETURN
        ELSE IF ( VTYPEW( V ) .NE. M3DBLE ) THEN
            MESG  = '*** Bad TYPE for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            RDWRF2DDBLE = .FALSE.
            RETURN
        END IF

        T = TINDEX( JDATE, JTIME )
        IF ( T .EQ. 0 ) THEN
            WRITE( MESG, '(3 A, I9.7, A, I6.6 )' ) '*** Timestep not available for "', TRIM( VNAME ),  '" at', JDATE, ':', JTIME
            CALL M3MESG( MESG )
            RDWRF2DDBLE = .FALSE.
            RETURN
        END IF

        FID = CDFID
        VID = VARIDW( V )
        DIMS( 1 ) = 1
        DELS( 1 ) = VARDIM( 1,V )
        DIMS( 2 ) = 1
        DELS( 2 ) = VARDIM( 2,V )
        DIMS( 3 ) = T
        DELS( 3 ) = 1

        IERR = NF_GET_VARA_DOUBLE( VID, DIMS, DELS, BUF )
        IF ( IERR .NE. NF_NOERR ) THEN
            MESG  = NF_STRERROR( IERR )
            CALL M3MESG( MESG )
            WRITE( MESG, '( 3 A, I10 )' ) 'NF_GET_VARA_DOUBLE(', TRIM( VNAME ), ') failure:  IERR=', IERR
            CALL M3WARN( PNAME, 0,0, MESG )
            RDWRF2DDBLE = .FALSE.
            RETURN
        END IF

        RDWRF2DDBLE = .TRUE.
        RETURN

    END FUNCTION RDWRF2DDBLE



    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



    LOGICAL FUNCTION RDWRF3DDBLE( VNAME, JDATE, JTIME, NCOLS, NROWS, NLAYS, BUF )

        !!-----------   Arguments:

        CHARACTER(LEN=*), INTENT(IN   ) :: VNAME
        INTEGER         , INTENT(IN   ) :: JDATE, JTIME
        INTEGER         , INTENT(IN   ) :: NCOLS, NROWS, NLAYS
        REAL*8          , INTENT(  OUT) :: BUF( NCOLS, NROWS, NLAYS )

        CHARACTER*24, PARAMETER :: PNAME = 'MODWRFIO/RDWRF3DDBLE'

        !!-----------   Local Variables:

        INTEGER         IERR, V, FID, VID, L, T
        INTEGER         DIMS( MXWRFDIMS ), DELS( MXWRFDIMS )
        CHARACTER*256   MESG

        !!-----------   Body:

        IF ( CDFID .EQ. IMISS3 ) THEN
            MESG  = '*** File not yet open for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            RDWRF3DDBLE = .FALSE.
            RETURN
        ELSE IF ( .NOT.CHECKNAME( VNAME, V ) ) THEN
            MESG  = '*** Bad file setup for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            RDWRF3DDBLE = .FALSE.
            RETURN
        ELSE IF ( DIMCNT( V ) .GT. 4 ) THEN
            MESG  = '*** Bad DIMENSION-COUNT for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            RDWRF3DDBLE = .FALSE.
            RETURN
        ELSE IF ( VARDIM( 1,V ) .NE. NCOLS .OR. VARDIM( 2,V ) .NE. NROWS ) THEN
            MESG  = '*** Bad DIMENSION for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            RDWRF3DDBLE = .FALSE.
            RETURN
        ELSE IF ( VTYPEW( V ) .NE. M3DBLE ) THEN
            MESG  = '*** Bad TYPE for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            RDWRF3DDBLE = .FALSE.
            RETURN
        END IF

        T = TINDEX( JDATE, JTIME )
        IF ( T .EQ. 0 ) THEN
            WRITE( MESG, '(3 A, I9.7, A, I6.6 )' ) '*** Timestep not available for "', TRIM( VNAME ),  '" at', JDATE, ':', JTIME
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            RDWRF3DDBLE = .FALSE.
            RETURN
        END IF

        FID = CDFID
        VID = VARIDW( V )
        DIMS( 1 ) = 1
        DELS( 1 ) = VARDIM( 1,V )
        DIMS( 2 ) = 1
        DELS( 2 ) = VARDIM( 2,V )
        DIMS( 3 ) = 1
        DELS( 3 ) = VARDIM( 3,V )
        DIMS( 4 ) = T
        DELS( 4 ) = 1

        IERR = NF_GET_VARA_DOUBLE( FID, VID, DIMS, DELS, BUF )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3 A, I10 )' ) 'NF_GET_VARA_DOUBLE(', TRIM( VNAME ), ') failure:  IERR=', IERR
            CALL M3WARN( PNAME, 0,0, MESG )
            RDWRF3DDBLE = .FALSE.
            RETURN
        END IF

        RDWRF3DDBLE = .TRUE.
        RETURN

    END FUNCTION RDWRF3DDBLE


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !!  MODULE PROCEDURES for generic READWRF():
    !!  Read variable VNAME for date&time JDATE:JTIME
    !!  1-D/2-D/3-D ; REAL/INTEGER cases
    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

    LOGICAL FUNCTION RDWRF1DREAL( VNAME, JDATE, JTIME, NSIZE, BUF )

        !!-----------   Arguments:

        CHARACTER(LEN=*), INTENT(IN   ) :: VNAME
        INTEGER         , INTENT(IN   ) :: JDATE, JTIME
        INTEGER         , INTENT(IN   ) :: NSIZE
        REAL            , INTENT(  OUT) :: BUF( NSIZE )

        CHARACTER*24, PARAMETER :: PNAME = 'MODWRFIO/RDWRF1DREAL'

        !!-----------   Local Variables:

        INTEGER         IERR, V, FID, VID, L, T
        INTEGER         DIMS( MXWRFDIMS ), DELS( MXWRFDIMS )
        CHARACTER*256   MESG

        !!-----------   Body:

        IF ( CDFID .EQ. IMISS3 ) THEN
            MESG  = '*** File not yet open for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            RDWRF1DREAL = .FALSE.
            RETURN
        ELSE IF ( .NOT.CHECKNAME( VNAME, V ) ) THEN
            MESG  = '*** Bad file setup for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            RDWRF1DREAL = .FALSE.
            RETURN
        ELSE IF ( DIMCNT( V ) .GT. 2 ) THEN
            MESG  = '*** Bad DIMENSION-COUNT for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            RDWRF1DREAL = .FALSE.
            RETURN
        ELSE IF ( VARDIM( 1,V ) .NE. NSIZE ) THEN
            MESG  = '*** Bad DIMENSION for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            RDWRF1DREAL = .FALSE.
            RETURN
        ELSE IF ( VTYPEW( V ) .NE. M3REAL ) THEN
            MESG  = '*** Bad TYPE for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            RDWRF1DREAL = .FALSE.
            RETURN
        END IF

        T = TINDEX( JDATE, JTIME )
        IF ( T .EQ. 0 ) THEN
            WRITE( MESG, '(3 A, I9.7, A, I6.6 )' ) '*** Timestep not available for "', TRIM( VNAME ),  '" at', JDATE, ':', JTIME
            CALL M3MESG( MESG )
            RDWRF1DREAL = .FALSE.
            RETURN
        END IF

        FID = CDFID
        VID = VARIDW( V )
        DIMS( 1 ) = 1
        DELS( 1 ) = VARDIM( 1,V )
        DIMS( 2 ) = T
        DELS( 2 ) = 1

        IERR = NF_GET_VARA_REAL( FID, VID, DIMS, DELS, BUF )
        IF ( IERR .NE. NF_NOERR ) THEN
            MESG  = NF_STRERROR( IERR )
            CALL M3MESG( MESG )
            WRITE( MESG, '( 3 A, I10 )' ) 'NF_GET_VARA_REAL(', TRIM( VNAME ), ') failure:  IERR=', IERR
            CALL M3WARN( PNAME, 0,0, MESG )
            RDWRF1DREAL = .FALSE.
            RETURN
        END IF

        RDWRF1DREAL = .TRUE.
        RETURN

    END FUNCTION RDWRF1DREAL



    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



    LOGICAL FUNCTION RDWRF2DREAL( VNAME, JDATE, JTIME, NCOLS, NROWS, BUF )

        !!-----------   Arguments:

        CHARACTER(LEN=*), INTENT(IN   ) :: VNAME
        INTEGER         , INTENT(IN   ) :: JDATE, JTIME
        INTEGER         , INTENT(IN   ) :: NCOLS, NROWS
        REAL            , INTENT(  OUT) :: BUF( NCOLS, NROWS )

        CHARACTER*24, PARAMETER :: PNAME = 'MODWRFIO/RDWRF2DREAL'

        !!-----------   Local Variables:

        INTEGER         IERR, V, FID, VID, L, T
        INTEGER         DIMS( MXWRFDIMS ), DELS( MXWRFDIMS )
        CHARACTER*256   MESG

        !!-----------   Body:

        IF ( CDFID .EQ. IMISS3 ) THEN
            MESG  = '*** File not yet open for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            RDWRF2DREAL = .FALSE.
            RETURN
        ELSE IF ( .NOT.CHECKNAME( VNAME, V ) ) THEN
            MESG  = '*** Bad file setup for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            RDWRF2DREAL = .FALSE.
            RETURN
        ELSE IF ( DIMCNT( V ) .GT. 3 ) THEN
            MESG  = '*** Bad DIMENSION-COUNT for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            RDWRF2DREAL = .FALSE.
            RETURN
        ELSE IF ( VARDIM( 1,V ) .NE. NCOLS .OR. VARDIM( 2,V ) .NE. NROWS ) THEN
            MESG  = '*** Bad DIMENSION for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            RDWRF2DREAL = .FALSE.
            RETURN
        ELSE IF ( VTYPEW( V ) .NE. M3REAL ) THEN
            MESG  = '*** Bad TYPE for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            RDWRF2DREAL = .FALSE.
            RETURN
        END IF

        T = TINDEX( JDATE, JTIME )
        IF ( T .EQ. 0 ) THEN
            WRITE( MESG, '(3 A, I9.7, A, I6.6 )' ) '*** Timestep not available for "', TRIM( VNAME ),  '" at', JDATE, ':', JTIME
            CALL M3MESG( MESG )
            RDWRF2DREAL = .FALSE.
            RETURN
        END IF

        FID = CDFID
        VID = VARIDW( V )
        DIMS( 1 ) = 1
        DELS( 1 ) = VARDIM( 1,V )
        DIMS( 2 ) = 1
        DELS( 2 ) = VARDIM( 2,V )
        DIMS( 3 ) = T
        DELS( 3 ) = 1

        IERR = NF_GET_VARA_REAL( FID, VID, DIMS, DELS, BUF )
        IF ( IERR .NE. NF_NOERR ) THEN
            MESG  = NF_STRERROR( IERR )
            CALL M3MESG( MESG )
            WRITE( MESG, '( 3 A, I10 )' ) 'NF_GET_VARA_REAL(', TRIM( VNAME ), ') failure:  IERR=', IERR
            CALL M3WARN( PNAME, 0,0, MESG )
            RDWRF2DREAL = .FALSE.
            RETURN
        END IF

        RDWRF2DREAL = .TRUE.
        RETURN

    END FUNCTION RDWRF2DREAL



    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



    LOGICAL FUNCTION RDWRF3DREAL( VNAME, JDATE, JTIME, NCOLS, NROWS, NLAYS, BUF )

        !!-----------   Arguments:

        CHARACTER(LEN=*), INTENT(IN   ) :: VNAME
        INTEGER         , INTENT(IN   ) :: JDATE, JTIME
        INTEGER         , INTENT(IN   ) :: NCOLS, NROWS, NLAYS
        REAL            , INTENT(  OUT) :: BUF( NCOLS, NROWS, NLAYS )

        CHARACTER*24, PARAMETER :: PNAME = 'MODWRFIO/RDWRF3DREAL'

        !!-----------   Local Variables:

        INTEGER         IERR, V, FID, VID, L, T
        INTEGER         DIMS( MXWRFDIMS ), DELS( MXWRFDIMS )
        CHARACTER*256   MESG

        !!-----------   Body:

        IF ( CDFID .EQ. IMISS3 ) THEN
            MESG  = '*** File not yet open for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            RDWRF3DREAL = .FALSE.
            RETURN
        ELSE IF ( .NOT.CHECKNAME( VNAME, V ) ) THEN
            MESG  = '*** Bad file setup for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            RDWRF3DREAL = .FALSE.
            RETURN
        ELSE IF ( DIMCNT( V ) .GT. 4 ) THEN
            MESG  = '*** Bad DIMENSION-COUNT for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            RDWRF3DREAL = .FALSE.
            RETURN
        ELSE IF ( VARDIM( 1,V ) .NE. NCOLS .OR. VARDIM( 2,V ) .NE. NROWS ) THEN
            MESG  = '*** Bad DIMENSION for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            RDWRF3DREAL = .FALSE.
            RETURN
        ELSE IF ( VTYPEW( V ) .NE. M3REAL ) THEN
            MESG  = '*** Bad TYPE for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            RDWRF3DREAL = .FALSE.
            RETURN
        END IF

        T = TINDEX( JDATE, JTIME )
        IF ( T .EQ. 0 ) THEN
            WRITE( MESG, '(3 A, I9.7, A, I6.6 )' ) '*** Timestep not available for "', TRIM( VNAME ),  '" at', JDATE, ':', JTIME
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            RDWRF3DREAL = .FALSE.
            RETURN
        END IF

        FID = CDFID
        VID = VARIDW( V )
        DIMS( 1 ) = 1
        DELS( 1 ) = VARDIM( 1,V )
        DIMS( 2 ) = 1
        DELS( 2 ) = VARDIM( 2,V )
        DIMS( 3 ) = 1
        DELS( 3 ) = VARDIM( 3,V )
        DIMS( 4 ) = T
        DELS( 4 ) = 1

        IERR = NF_GET_VARA_REAL( FID, VID, DIMS, DELS, BUF )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3 A, I10 )' ) 'NF_GET_VARA_REAL(', TRIM( VNAME ), ') failure:  IERR=', IERR
            CALL M3WARN( PNAME, 0,0, MESG )
            RDWRF3DREAL = .FALSE.
            RETURN
        END IF

        RDWRF3DREAL = .TRUE.
        RETURN

    END FUNCTION RDWRF3DREAL



    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



    LOGICAL FUNCTION RDWRF1DINT( VNAME, JDATE, JTIME, NSIZE, BUF )

        !!-----------   Arguments:

        CHARACTER(LEN=*), INTENT(IN   ) :: VNAME
        INTEGER         , INTENT(IN   ) :: JDATE, JTIME
        INTEGER         , INTENT(IN   ) :: NSIZE
        INTEGER         , INTENT(  OUT) :: BUF( NSIZE )

        CHARACTER*24, PARAMETER :: PNAME = 'MODWRFIO/RDWRF1DINT'

        !!-----------   Local Variables:

        INTEGER         IERR, V, FID, VID, L, T
        INTEGER         DIMS( MXWRFDIMS ), DELS( MXWRFDIMS )
        CHARACTER*256   MESG

        !!-----------   Body:

        IF ( CDFID .EQ. IMISS3 ) THEN
            MESG  = '*** File not yet open for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            RDWRF1DINT = .FALSE.
            RETURN
        ELSE IF ( .NOT.CHECKNAME( VNAME, V ) ) THEN
            MESG  = '*** Bad file setup for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            RDWRF1DINT = .FALSE.
            RETURN
        ELSE IF ( DIMCNT( V ) .GT. 2 ) THEN
            MESG  = '*** Bad DIMENSION-COUNT for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            RDWRF1DINT = .FALSE.
            RETURN
        ELSE IF ( VARDIM( 1,V ) .NE. NSIZE ) THEN
            MESG  = '*** Bad DIMENSION for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            RDWRF1DINT = .FALSE.
            RETURN
        ELSE IF ( VTYPEW( V ) .NE. M3INT ) THEN
            MESG  = '*** Bad TYPE for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            RDWRF1DINT = .FALSE.
            RETURN
        END IF

        T = TINDEX( JDATE, JTIME )
        IF ( T .EQ. 0 ) THEN
            WRITE( MESG, '(3 A, I9.7, A, I6.6 )' ) '*** Timestep not available for "', TRIM( VNAME ),  '" at', JDATE, ':', JTIME
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            RDWRF1DINT = .FALSE.
            RETURN
        END IF

        FID = CDFID
        VID = VARIDW( V )
        DIMS( 1 ) = 1
        DELS( 1 ) = VARDIM( 1,V )
        DIMS( 2 ) = T
        DELS( 2 ) = 1

        IERR = NF_GET_VARA_INT( FID, VID, DIMS, DELS, BUF )
        IF ( IERR .NE. NF_NOERR ) THEN
            MESG  = NF_STRERROR( IERR )
            CALL M3MESG( MESG )
            WRITE( MESG, '( 3 A, I10 )' ) 'NF_GET_VARA_INT(', TRIM( VNAME ), ') failure:  IERR=', IERR
            CALL M3WARN( PNAME, 0,0, MESG )
            RDWRF1DINT = .FALSE.
            RETURN
        END IF


        RDWRF1DINT = .TRUE.
        RETURN

    END FUNCTION RDWRF1DINT



    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



    LOGICAL FUNCTION RDWRF2DINT( VNAME, JDATE, JTIME, NCOLS, NROWS, BUF )

        !!-----------   Arguments:

        CHARACTER(LEN=*), INTENT(IN   ) :: VNAME
        INTEGER         , INTENT(IN   ) :: JDATE, JTIME
        INTEGER         , INTENT(IN   ) :: NCOLS, NROWS
        INTEGER         , INTENT(  OUT) :: BUF( NCOLS, NROWS )

        CHARACTER*24, PARAMETER :: PNAME = 'MODWRFIO/RDWRF2DINT'

        !!-----------   Local Variables:

        INTEGER         IERR, V, FID, VID, L, T
        INTEGER         DIMS( MXWRFDIMS ), DELS( MXWRFDIMS )
        CHARACTER*256   MESG

        !!-----------   Body:

        IF ( CDFID .EQ. IMISS3 ) THEN
            MESG  = '*** File not yet open for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            RDWRF2DINT = .FALSE.
            RETURN
        ELSE IF ( .NOT.CHECKNAME( VNAME, V ) ) THEN
            MESG  = '*** Bad file setup for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            RDWRF2DINT = .FALSE.
            RETURN
        ELSE IF ( DIMCNT( V ) .GT. 3 ) THEN
            MESG  = '*** Bad DIMENSION-COUNT for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            RDWRF2DINT = .FALSE.
            RETURN
        ELSE IF ( VARDIM( 1,V ) .NE. NCOLS .OR. VARDIM( 2,V ) .NE. NROWS ) THEN
            MESG  = '*** Bad DIMENSION for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            RDWRF2DINT = .FALSE.
            RETURN
        ELSE IF ( VTYPEW( V ) .NE. M3INT ) THEN
            MESG  = '*** Bad TYPE for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            RDWRF2DINT = .FALSE.
            RETURN
        END IF

        T = TINDEX( JDATE, JTIME )
        IF ( T .EQ. 0 ) THEN
            WRITE( MESG, '(3 A, I9.7, A, I6.6 )' ) '*** Timestep not available for "', TRIM( VNAME ),  '" at', JDATE, ':', JTIME
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            RDWRF2DINT = .FALSE.
            RETURN
        END IF

        FID = CDFID
        VID = VARIDW( V )
        DIMS( 1 ) = 1
        DELS( 1 ) = VARDIM( 1,V )
        DIMS( 2 ) = 1
        DELS( 2 ) = VARDIM( 2,V )
        DIMS( 3 ) = T
        DELS( 3 ) = 1

        IERR = NF_GET_VARA_INT( FID, VID, DIMS, DELS, BUF )
        IF ( IERR .NE. NF_NOERR ) THEN
            MESG  = NF_STRERROR( IERR )
            CALL M3MESG( MESG )
            WRITE( MESG, '( 3 A, I10 )' ) 'NF_GET_VARA_INT(', TRIM( VNAME ), ') failure:  IERR=', IERR
            CALL M3WARN( PNAME, 0,0, MESG )
            RDWRF2DINT = .FALSE.
            RETURN
        END IF

        RDWRF2DINT = .TRUE.
        RETURN

    END FUNCTION RDWRF2DINT



    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



    LOGICAL FUNCTION RDWRF3DINT( VNAME, JDATE, JTIME, NCOLS, NROWS, NLAYS, BUF )

        !!-----------   Arguments:

        CHARACTER(LEN=*), INTENT(IN   ) :: VNAME
        INTEGER         , INTENT(IN   ) :: JDATE, JTIME
        INTEGER         , INTENT(IN   ) :: NCOLS, NROWS, NLAYS
        INTEGER         , INTENT(  OUT) :: BUF( NCOLS, NROWS, NLAYS )

        CHARACTER*24, PARAMETER :: PNAME = 'MODWRFIO/RDWRF3DINT'

        !!-----------   Local Variables:

        INTEGER         IERR, V, FID, VID, L, T
        INTEGER         DIMS( MXWRFDIMS ), DELS( MXWRFDIMS )
        CHARACTER*256   MESG

        !!-----------   Body:

        IF ( CDFID .EQ. IMISS3 ) THEN
            MESG  = '*** File not yet open for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            RDWRF3DINT = .FALSE.
            RETURN
        ELSE IF ( .NOT.CHECKNAME( VNAME, V ) ) THEN
            MESG  = '*** Bad file setup for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            RDWRF3DINT = .FALSE.
            RETURN
        ELSE IF ( DIMCNT( V ) .GT. 3 ) THEN
            MESG  = '*** Bad DIMENSION-COUNT for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            RDWRF3DINT = .FALSE.
            RETURN
        ELSE IF ( VARDIM( 1,V ) .NE. NCOLS .OR. VARDIM( 2,V ) .NE. NROWS ) THEN
            MESG  = '*** Bad DIMENSION for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            RDWRF3DINT = .FALSE.
            RETURN
        ELSE IF ( VTYPEW( V ) .NE. M3INT ) THEN
            MESG  = '*** Bad TYPE for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            RDWRF3DINT = .FALSE.
            RETURN
        END IF

        T = TINDEX( JDATE, JTIME )
        IF ( T .EQ. 0 ) THEN
            WRITE( MESG, '(3 A, I9.7, A, I6.6 )' ) '*** Timestep not available for "', TRIM( VNAME ),  '" at', JDATE, ':', JTIME
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            RDWRF3DINT = .FALSE.
            RETURN
        END IF

        FID = CDFID
        VID = VARIDW( V )
        DIMS( 1 ) = 1
        DELS( 1 ) = VARDIM( 1,V )
        DIMS( 2 ) = 1
        DELS( 2 ) = VARDIM( 2,V )
        DIMS( 3 ) = 1
        DELS( 3 ) = VARDIM( 3,V )
        DIMS( 4 ) = T
        DELS( 4 ) = 1

        IERR = NF_GET_VARA_INT( FID, VID, DIMS, DELS, BUF )
        IF ( IERR .NE. NF_NOERR ) THEN
            MESG  = NF_STRERROR( IERR )
            CALL M3MESG( MESG )
            WRITE( MESG, '( 3 A, I10 )' ) 'NF_GET_VARA_INT(', TRIM( VNAME ), ') failure:  IERR=', IERR
            CALL M3WARN( PNAME, 0,0, MESG )
            RDWRF3DINT = .FALSE.
            RETURN
        END IF

        RDWRF3DINT = .TRUE.
        RETURN

    END FUNCTION RDWRF3DINT




    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !!  MODULE PROCEDURES for generic WRITEWRF():
    !!  Write variable VNAME for date&time JDATE:JTIME
    !!  1-D/2-D/3-D ; DBLE/REAL/INTEGER cases
    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



    LOGICAL FUNCTION WRWRF1DDBLE( VNAME, JDATE, JTIME, NSIZE, BUF )

        !!-----------   Arguments:

        CHARACTER(LEN=*), INTENT(IN   ) :: VNAME
        INTEGER         , INTENT(IN   ) :: JDATE, JTIME
        INTEGER         , INTENT(IN   ) :: NSIZE
        REAL*8          , INTENT(IN   ) :: BUF( NSIZE )

        CHARACTER*24, PARAMETER :: PNAME = 'MODWRFIO/WRWRF1DDBLE'

        !!-----------   Local Variables:

        INTEGER         V
        CHARACTER*256   MESG

        !!-----------   Body:

        IF ( CDFID .EQ. IMISS3 ) THEN
            MESG  = '*** File not yet open for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            WRWRF1DDBLE = .FALSE.
            RETURN
        ELSE IF ( FMODE .NE. FSRDWR3 ) THEN
            MESG  = '*** File  "' // TRIM( LNAME ) // '" not opened for "WRITE(' // TRIM( VNAME ) // '...)"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            WRWRF1DDBLE = .FALSE.
            RETURN
        ELSE IF ( .NOT.CHECKNAME( VNAME, V ) ) THEN
            MESG  = '*** Bad file setup for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            WRWRF1DDBLE = .FALSE.
            RETURN
        ELSE IF ( DIMCNT( V ) .GT. 3 ) THEN
            MESG  = '*** Bad DIMENSION-COUNT for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            WRWRF1DDBLE = .FALSE.
            RETURN
        ELSE IF ( VARDIM( 1,V ) .NE. NSIZE ) THEN
            MESG  = '*** Bad DIMENSION for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            WRWRF1DDBLE = .FALSE.
            RETURN
        ELSE IF ( VTYPEW( V ) .NE. M3DBLE ) THEN
            MESG  = '*** Bad TYPE for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            WRWRF1DDBLE = .FALSE.
            RETURN
        END IF

        CALL M3MESG( 'WRITEWRF() not yet implemented' )
        WRWRF1DDBLE = .FALSE.
        RETURN

    END FUNCTION WRWRF1DDBLE



    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



    LOGICAL FUNCTION WRWRF2DDBLE( VNAME, JDATE, JTIME, NCOLS, NROWS, BUF )

        !!-----------   Arguments:

        CHARACTER(LEN=*), INTENT(IN   ) :: VNAME
        INTEGER         , INTENT(IN   ) :: JDATE, JTIME
        INTEGER         , INTENT(IN   ) :: NCOLS, NROWS
        REAL*8          , INTENT(IN   ) :: BUF( NCOLS, NROWS )

        CHARACTER*24, PARAMETER :: PNAME = 'MODWRFIO/WRWRF2DDBLE'

        !!-----------   Local Variables:

        INTEGER         V
        CHARACTER*256   MESG

        !!-----------   Body:

        IF ( CDFID .EQ. IMISS3 ) THEN
            MESG  = '*** File not yet open for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            WRWRF2DDBLE = .FALSE.
            RETURN
        ELSE IF ( FMODE .NE. FSRDWR3 ) THEN
            MESG  = '*** File  "' // TRIM( LNAME ) // '" not opened for "WRITE(' // TRIM( VNAME ) // '...)"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            WRWRF2DDBLE = .FALSE.
        ELSE IF ( .NOT.CHECKNAME( VNAME, V ) ) THEN
            MESG  = '*** Bad file setup for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            WRWRF2DDBLE = .FALSE.
            RETURN
        ELSE IF ( DIMCNT( V ) .GT. 3 ) THEN
            MESG  = '*** Bad DIMENSION-COUNT for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            WRWRF2DDBLE = .FALSE.
            RETURN
        ELSE IF ( VARDIM( 1,V ) .NE. NCOLS .OR. VARDIM( 2,V ) .NE. NROWS ) THEN
            MESG  = '*** Bad DIMENSION for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            WRWRF2DDBLE = .FALSE.
            RETURN
        ELSE IF ( VTYPEW( V ) .NE. M3DBLE ) THEN
            MESG  = '*** Bad TYPE for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            WRWRF2DDBLE = .FALSE.
            RETURN
        END IF

        CALL M3MESG( 'WRITEWRF() not yet implemented' )
        WRWRF2DDBLE = .FALSE.
        RETURN

    END FUNCTION WRWRF2DDBLE



    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



    LOGICAL FUNCTION WRWRF3DDBLE( VNAME, JDATE, JTIME, NCOLS, NROWS, NLAYS, BUF )

        !!-----------   Arguments:

        CHARACTER(LEN=*), INTENT(IN   ) :: VNAME
        INTEGER         , INTENT(IN   ) :: JDATE, JTIME
        INTEGER         , INTENT(IN   ) :: NCOLS, NROWS, NLAYS
        REAL*8          , INTENT(IN   ) :: BUF( NCOLS, NROWS, NLAYS )

        CHARACTER*24, PARAMETER :: PNAME = 'MODWRFIO/WRWRF3DDBLE'

        !!-----------   Local Variables:

        INTEGER         V
        CHARACTER*256   MESG

        !!-----------   Body:

        IF ( CDFID .EQ. IMISS3 ) THEN
            MESG  = '*** File not yet open for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            WRWRF3DDBLE = .FALSE.
            RETURN
        ELSE IF ( FMODE .NE. FSRDWR3 ) THEN
            MESG  = '*** File  "' // TRIM( LNAME ) // '" not opened for "WRITE(' // TRIM( VNAME ) // '...)"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            WRWRF3DDBLE = .FALSE.
        ELSE IF ( .NOT.CHECKNAME( VNAME, V ) ) THEN
            MESG  = '*** Bad file setup for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            WRWRF3DDBLE = .FALSE.
            RETURN
        ELSE IF ( DIMCNT( V ) .GT. 3 ) THEN
            MESG  = '*** Bad DIMENSION-COUNT for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            WRWRF3DDBLE = .FALSE.
            RETURN
        ELSE IF ( VARDIM( 1,V ) .NE. NCOLS .OR. VARDIM( 2,V ) .NE. NROWS ) THEN
            MESG  = '*** Bad DIMENSION for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            WRWRF3DDBLE = .FALSE.
            RETURN
        ELSE IF ( VTYPEW( V ) .NE. M3DBLE ) THEN
            MESG  = '*** Bad TYPE for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            WRWRF3DDBLE = .FALSE.
            RETURN
        END IF

        CALL M3MESG( 'WRITEWRF() not yet implemented' )
        WRWRF3DDBLE = .FALSE.
        RETURN

    END FUNCTION WRWRF3DDBLE



    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



    LOGICAL FUNCTION WRWRF1DREAL( VNAME, JDATE, JTIME, NSIZE, BUF )

        !!-----------   Arguments:

        CHARACTER(LEN=*), INTENT(IN   ) :: VNAME
        INTEGER         , INTENT(IN   ) :: JDATE, JTIME
        INTEGER         , INTENT(IN   ) :: NSIZE
        REAL            , INTENT(IN   ) :: BUF( NSIZE )

        CHARACTER*24, PARAMETER :: PNAME = 'MODWRFIO/WRWRF1DREAL'

        !!-----------   Local Variables:

        INTEGER         V
        CHARACTER*256   MESG

        !!-----------   Body:

        IF ( CDFID .EQ. IMISS3 ) THEN
            MESG  = '*** File not yet open for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            WRWRF1DREAL = .FALSE.
            RETURN
        ELSE IF ( FMODE .NE. FSRDWR3 ) THEN
            MESG  = '*** File  "' // TRIM( LNAME ) // '" not opened for "WRITE(' // TRIM( VNAME ) // '...)"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            WRWRF1DREAL = .FALSE.
            RETURN
        ELSE IF ( .NOT.CHECKNAME( VNAME, V ) ) THEN
            MESG  = '*** Bad file setup for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            WRWRF1DREAL = .FALSE.
            RETURN
        ELSE IF ( DIMCNT( V ) .GT. 3 ) THEN
            MESG  = '*** Bad DIMENSION-COUNT for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            WRWRF1DREAL = .FALSE.
            RETURN
        ELSE IF ( VARDIM( 1,V ) .NE. NSIZE ) THEN
            MESG  = '*** Bad DIMENSION for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            WRWRF1DREAL = .FALSE.
            RETURN
        ELSE IF ( VTYPEW( V ) .NE. M3REAL ) THEN
            MESG  = '*** Bad TYPE for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            WRWRF1DREAL = .FALSE.
            RETURN
        END IF

        CALL M3MESG( 'WRITEWRF() not yet implemented' )
        WRWRF1DREAL = .FALSE.
        RETURN

    END FUNCTION WRWRF1DREAL



    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



    LOGICAL FUNCTION WRWRF2DREAL( VNAME, JDATE, JTIME, NCOLS, NROWS, BUF )

        !!-----------   Arguments:

        CHARACTER(LEN=*), INTENT(IN   ) :: VNAME
        INTEGER         , INTENT(IN   ) :: JDATE, JTIME
        INTEGER         , INTENT(IN   ) :: NCOLS, NROWS
        REAL            , INTENT(IN   ) :: BUF( NCOLS, NROWS )

        CHARACTER*24, PARAMETER :: PNAME = 'MODWRFIO/WRWRF2DREAL'

        !!-----------   Local Variables:

        INTEGER         V
        CHARACTER*256   MESG

        !!-----------   Body:

        IF ( CDFID .EQ. IMISS3 ) THEN
            MESG  = '*** File not yet open for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            WRWRF2DREAL = .FALSE.
            RETURN
        ELSE IF ( FMODE .NE. FSRDWR3 ) THEN
            MESG  = '*** File  "' // TRIM( LNAME ) // '" not opened for "WRITE(' // TRIM( VNAME ) // '...)"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            WRWRF2DREAL = .FALSE.
        ELSE IF ( .NOT.CHECKNAME( VNAME, V ) ) THEN
            MESG  = '*** Bad file setup for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            WRWRF2DREAL = .FALSE.
            RETURN
        ELSE IF ( DIMCNT( V ) .GT. 3 ) THEN
            MESG  = '*** Bad DIMENSION-COUNT for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            WRWRF2DREAL = .FALSE.
            RETURN
        ELSE IF ( VARDIM( 1,V ) .NE. NCOLS .OR. VARDIM( 2,V ) .NE. NROWS ) THEN
            MESG  = '*** Bad DIMENSION for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            WRWRF2DREAL = .FALSE.
            RETURN
        ELSE IF ( VTYPEW( V ) .NE. M3REAL ) THEN
            MESG  = '*** Bad TYPE for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            WRWRF2DREAL = .FALSE.
            RETURN
        END IF

        CALL M3MESG( 'WRITEWRF() not yet implemented' )
        WRWRF2DREAL = .FALSE.
        RETURN

    END FUNCTION WRWRF2DREAL



    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



    LOGICAL FUNCTION WRWRF3DREAL( VNAME, JDATE, JTIME, NCOLS, NROWS, NLAYS, BUF )

        !!-----------   Arguments:

        CHARACTER(LEN=*), INTENT(IN   ) :: VNAME
        INTEGER         , INTENT(IN   ) :: JDATE, JTIME
        INTEGER         , INTENT(IN   ) :: NCOLS, NROWS, NLAYS
        REAL            , INTENT(IN   ) :: BUF( NCOLS, NROWS, NLAYS )

        CHARACTER*24, PARAMETER :: PNAME = 'MODWRFIO/WRWRF3DREAL'

        !!-----------   Local Variables:

        INTEGER         V
        CHARACTER*256   MESG

        !!-----------   Body:

        IF ( CDFID .EQ. IMISS3 ) THEN
            MESG  = '*** File not yet open for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            WRWRF3DREAL = .FALSE.
            RETURN
        ELSE IF ( FMODE .NE. FSRDWR3 ) THEN
            MESG  = '*** File  "' // TRIM( LNAME ) // '" not opened for "WRITE(' // TRIM( VNAME ) // '...)"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            WRWRF3DREAL = .FALSE.
        ELSE IF ( .NOT.CHECKNAME( VNAME, V ) ) THEN
            MESG  = '*** Bad file setup for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            WRWRF3DREAL = .FALSE.
            RETURN
        ELSE IF ( DIMCNT( V ) .GT. 3 ) THEN
            MESG  = '*** Bad DIMENSION-COUNT for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            WRWRF3DREAL = .FALSE.
            RETURN
        ELSE IF ( VARDIM( 1,V ) .NE. NCOLS .OR. VARDIM( 2,V ) .NE. NROWS ) THEN
            MESG  = '*** Bad DIMENSION for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            WRWRF3DREAL = .FALSE.
            RETURN
        ELSE IF ( VTYPEW( V ) .NE. M3REAL ) THEN
            MESG  = '*** Bad TYPE for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            WRWRF3DREAL = .FALSE.
            RETURN
        END IF

        CALL M3MESG( 'WRITEWRF() not yet implemented' )
        WRWRF3DREAL = .FALSE.
        RETURN

    END FUNCTION WRWRF3DREAL



    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



    LOGICAL FUNCTION WRWRF1DINT( VNAME, JDATE, JTIME, NSIZE, BUF )

        !!-----------   Arguments:

        CHARACTER(LEN=*), INTENT(IN   ) :: VNAME
        INTEGER         , INTENT(IN   ) :: JDATE, JTIME
        INTEGER         , INTENT(IN   ) :: NSIZE
        INTEGER         , INTENT(IN   ) :: BUF( NSIZE )

        CHARACTER*24, PARAMETER :: PNAME = 'MODWRFIO/WRWRF1DINT'

        !!-----------   Local Variables:

        INTEGER         V
        CHARACTER*256   MESG

        !!-----------   Body:

        IF ( CDFID .EQ. IMISS3 ) THEN
            MESG  = '*** File not yet open for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            WRWRF1DINT = .FALSE.
            RETURN
        ELSE IF ( FMODE .NE. FSRDWR3 ) THEN
            MESG  = '*** File  "' // TRIM( LNAME ) // '" not opened for "WRITE(' // TRIM( VNAME ) // '...)"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            WRWRF1DINT = .FALSE.
        ELSE IF ( .NOT.CHECKNAME( VNAME, V ) ) THEN
            MESG  = '*** Bad file setup for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            WRWRF1DINT = .FALSE.
            RETURN
        ELSE IF ( DIMCNT( V ) .GT. 3 ) THEN
            MESG  = '*** Bad DIMENSION-COUNT for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            WRWRF1DINT = .FALSE.
            RETURN
        ELSE IF ( VARDIM( 1,V ) .NE. NSIZE ) THEN
            MESG  = '*** Bad DIMENSION for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            WRWRF1DINT = .FALSE.
            RETURN
        ELSE IF ( VTYPEW( V ) .NE. M3REAL ) THEN
            MESG  = '*** Bad TYPE for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            WRWRF1DINT = .FALSE.
            RETURN
        END IF

        CALL M3MESG( 'WRITEWRF() not yet implemented' )
        WRWRF1DINT = .FALSE.
        RETURN

    END FUNCTION WRWRF1DINT



    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



    LOGICAL FUNCTION WRWRF2DINT( VNAME, JDATE, JTIME, NCOLS, NROWS, BUF )

        !!-----------   Arguments:

        CHARACTER(LEN=*), INTENT(IN   ) :: VNAME
        INTEGER         , INTENT(IN   ) :: JDATE, JTIME
        INTEGER         , INTENT(IN   ) :: NCOLS, NROWS
        INTEGER         , INTENT(IN   ) :: BUF( NCOLS, NROWS )

        CHARACTER*24, PARAMETER :: PNAME = 'MODWRFIO/WRWRF2DINT'

        !!-----------   Local Variables:

        INTEGER         V
        CHARACTER*256   MESG

        IF ( CDFID .EQ. IMISS3 ) THEN
            MESG  = '*** File not yet open for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            WRWRF2DINT = .FALSE.
            RETURN
        ELSE IF ( FMODE .NE. FSRDWR3 ) THEN
            MESG  = '*** File  "' // TRIM( LNAME ) // '" not opened for "WRITE(' // TRIM( VNAME ) // '...)"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            WRWRF2DINT = .FALSE.
        ELSE IF ( .NOT.CHECKNAME( VNAME, V ) ) THEN
            MESG  = '*** Bad file setup for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            WRWRF2DINT = .FALSE.
            RETURN
        ELSE IF ( DIMCNT( V ) .GT. 3 ) THEN
            MESG  = '*** Bad DIMENSION-COUNT for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            WRWRF2DINT = .FALSE.
            RETURN
        ELSE IF ( VARDIM( 1,V ) .NE. NCOLS .OR. VARDIM( 2,V ) .NE. NROWS ) THEN
            MESG  = '*** Bad DIMENSION for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            WRWRF2DINT = .FALSE.
            RETURN
        ELSE IF ( VTYPEW( V ) .NE. M3REAL ) THEN
            MESG  = '*** Bad TYPE for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            WRWRF2DINT = .FALSE.
            RETURN
        END IF

        !!-----------   Body:

        CALL M3MESG( 'WRITEWRF() not yet implemented' )
        WRWRF2DINT = .FALSE.
        RETURN

    END FUNCTION WRWRF2DINT



    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



    LOGICAL FUNCTION WRWRF3DINT( VNAME, JDATE, JTIME, NCOLS, NROWS, NLAYS, BUF )

        !!-----------   Arguments:

        CHARACTER(LEN=*), INTENT(IN   ) :: VNAME
        INTEGER         , INTENT(IN   ) :: JDATE, JTIME
        INTEGER         , INTENT(IN   ) :: NCOLS, NROWS, NLAYS
        INTEGER         , INTENT(IN   ) :: BUF( NCOLS, NROWS, NLAYS )

        CHARACTER*24, PARAMETER :: PNAME = 'MODWRFIO/WRWRF3DINT'

        !!-----------   Local Variables:

        INTEGER         V
        CHARACTER*256   MESG

        !!-----------   Body:

        IF ( CDFID .EQ. IMISS3 ) THEN
            MESG  = '*** File not yet open for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            WRWRF3DINT = .FALSE.
            RETURN
        ELSE IF ( FMODE .NE. FSRDWR3 ) THEN
            MESG  = '*** File  "' // TRIM( LNAME ) // '" not opened for "WRITE(' // TRIM( VNAME ) // '...)"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            WRWRF3DINT = .FALSE.
        ELSE IF ( .NOT.CHECKNAME( VNAME, V ) ) THEN
            MESG  = '*** Bad file setup for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            WRWRF3DINT = .FALSE.
            RETURN
        ELSE IF ( DIMCNT( V ) .GT. 3 ) THEN
            MESG  = '*** Bad DIMENSION-COUNT for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            WRWRF3DINT = .FALSE.
            RETURN
        ELSE IF ( VARDIM( 1,V ) .NE. NCOLS .OR. VARDIM( 2,V ) .NE. NROWS ) THEN
            MESG  = '*** Bad DIMENSION for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            WRWRF3DINT = .FALSE.
            RETURN
        ELSE IF ( VTYPEW( V ) .NE. M3REAL ) THEN
            MESG  = '*** Bad TYPE for "' // TRIM( VNAME ) // '"'
            CALL M3WARN( PNAME, JDATE, JTIME, MESG )
            WRWRF3DINT = .FALSE.
            RETURN
        END IF

        CALL M3MESG( 'WRITEWRF() not yet implemented' )
        WRWRF3DINT = .FALSE.
        RETURN

    END FUNCTION WRWRF3DINT



    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



    LOGICAL FUNCTION CHECKNAME( VNAME, VBLE )

        !!-----------   Arguments:

        CHARACTER(LEN=*), INTENT(IN   ) :: VNAME
        INTEGER         , INTENT(  OUT) :: VBLE

        !!-----------   Local Variables:

        CHARACTER*256    MESG

        !!-----------   Body:

        IF ( CDFID .LT. 0 ) THEN
            MESG  = '*** No files open: Must call OPENWRF() or CRTWRF() before any I/O call'
            CALL M3MESG( MESG )
            CHECKNAME = .FALSE.
            RETURN
        END IF

        IF ( LEN_TRIM( VNAME ) .GT. 32 ) THEN
            MESG  = '*** Vble name length exceeds 32 for "' // TRIM( VNAME ) // '"'
            CALL M3MESG( MESG )
            CHECKNAME = .FALSE.
            RETURN
        END IF

        VBLE = INDEX1( VNAME, NVARSW, VNAMEW )
        IF ( VBLE .LE. 0 ) THEN
            MESG  = '*** Vble "' // TRIM( VNAME ) // '" not found in file "' // TRIM( LNAME ) // '"'
            CALL M3MESG( MESG )
            CHECKNAME = .FALSE.
            RETURN
        ELSE
            CHECKNAME = .TRUE.
        END IF

        RETURN

    END FUNCTION CHECKNAME


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE DATETIME( DTSTR, JDATE, JTIME )

        CHARACTER(LEN=*), INTENT(IN   ) :: DTSTR        !! yyyy-mm-dd:hh-mm-ss[.ssss]
        INTEGER         , INTENT(  OUT) :: JDATE, JTIME

        INTEGER         YY, MM, DD, JJ, HH, SS, TT

        YY = STR2INT( DTSTR(  1: 4 ) )
        MM = STR2INT( DTSTR(  6: 7 ) )
        DD = STR2INT( DTSTR(  9:10 ) )
        JDATE = 1000 * YY + JULIAN( YY, MM, DD )
        HH = STR2INT( DTSTR( 12:13 ) )
        MM = STR2INT( DTSTR( 15:16 ) )
        SS = STR2INT( DTSTR( 18:19 ) )
        JTIME = SS + 100 * MM + 10000 * HH

        RETURN

    END SUBROUTINE DATETIME


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    INTEGER  FUNCTION     TINDEX( JDATE, JTIME  )

        INTEGER, INTENT(IN   ) :: JDATE, JTIME

        INTEGER, SAVE :: IREC = 1
        INTEGER       :: I

        !!........  If <jdate:jtime> at IREC: return irec

        IF ( TSTEPW .EQ. 0 ) THEN
            TINDEX = 1
            RETURN
        ELSE IF ( JDATE .EQ. DATESW( IREC ) .AND. JTIME .EQ. TIMESW( IREC ) ) THEN
            TINDEX = IREC
            RETURN
        END IF

        !!........  else must search for <jdate:jtime>

        IF (      ( JDATE .EQ. DATESW( IREC ) .AND. JTIME .GT. TIMESW( IREC ) )       &
             .OR. ( JDATE .GT. DATESW( IREC ) ) ) THEN

            DO I = IREC+1, NTIMEW
                IF ( JDATE .GT. DATESW( I ) ) THEN
                    CYCLE
                ELSE IF ( JTIME .EQ. TIMESW( I ) ) THEN
                    IREC   = I
                    TINDEX = IREC
                    RETURN
                END IF
            END DO

        ELSE            !!  <jdate:jtime> before irec:

            DO I = IREC-1, 1, -1
                IF ( JDATE .LT. DATESW( I ) ) THEN
                    CYCLE
                ELSE IF ( JTIME .EQ. TIMESW( I ) ) THEN
                    IREC   = I
                    TINDEX = IREC
                    RETURN
                END IF
            END DO

        END IF

        !!........  else <jdate:jtime> not found

        TINDEX = -1
        RETURN

    END FUNCTION TINDEX


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE  GETORIG( X, Y )

        REAL, INTENT( IN ) :: X, Y

        CHARACTER*24, PARAMETER :: PNAME = 'MODWRFIO/GETORIG'

        !!   Arguments for GTPZ0:

        REAL*8          DSCR            !  scratch variables
        INTEGER         DEG, MNT        !  scratch variables
        REAL*8          CRDIN( 2 )      !  input coordinates x,y
        INTEGER*4       INSYS           !  input projection code
        INTEGER*4       INZONE          !  input utm zone, etc.
        REAL*8          TPAIN( 15 )     !  input projection parameters
        INTEGER*4       INUNIT          !  input units code
        INTEGER*4       INSPH           !  spheroid code
        INTEGER*4       IPR             !  error print flag
        INTEGER*4       JPR             !  projection parameter print flag
        INTEGER*4       LEMSG           !  error message unit number
        INTEGER*4       LPARM           !  projection parameter unit number
        REAL*8          CRDIO( 2 )      !  output coordinates x,y
        INTEGER*4       IOSYS           !  output projection code
        INTEGER*4       IOZONE          !  output utm zone, etc.
        REAL*8          TPOUT( 15 )     !  output projection parameters
        INTEGER*4       IOUNIT          !  output units code
        INTEGER*4       LN27            !  NAD1927 file unit number
        INTEGER*4       LN83            !  NAD1983 file unit number
        CHARACTER*128   FN27            !  NAD1927 file name
        CHARACTER*128   FN83            !  NAD1983 file name
        INTEGER*4       LENGTH          !  NAD* record-length
        INTEGER*4       IFLG            !  error flag

        CHARACTER*256   MESG

        !!.......   Set up arguments for GTP0:

        TPOUT = 0.0D0   !  array assignment

        IF ( GDTYP1 .EQ. LAMGRD3 ) THEN

            DSCR = P_ALP1
            DEG  = INT( DSCR )                              !  int degrees
            DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
            MNT  = INT( DSCR )                              !  int minutes
            DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
            TPOUT( 3 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

            DSCR = P_BET1
            DEG  = INT( DSCR )                              !  int degrees
            DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
            MNT  = INT( DSCR )                              !  int minutes
            DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
            TPOUT( 4 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

            DSCR = P_GAM1
            DEG  = INT( DSCR )                              !  int degrees
            DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
            MNT  = INT( DSCR )                              !  int minutes
            DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
            TPOUT( 5 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

            DSCR = YCENT1
            DEG  = INT( DSCR )                              !  int degrees
            DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
            MNT  = INT( DSCR )                              !  int minutes
            DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
            TPOUT( 6 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

            IOSYS  = 4       !  Lambert conformal conic
            IOZONE = 81
            IOUNIT = 2       !  output units:  meters

        ELSE IF ( GDTYP1 .EQ. POLGRD3 ) THEN

            DSCR = P_GAM1
            DEG  = INT( DSCR )                              !  int degrees
            DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
            MNT  = INT( DSCR )                              !  int minutes
            DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
            TPOUT( 5 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

            DSCR = P_BET1
            DEG  = INT( DSCR )                              !  int degrees
            DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
            MNT  = INT( DSCR )                              !  int minutes
            DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
            TPOUT( 6 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

            IOSYS  = 6       !  Polar stereographic
            IOZONE = 83
            IOUNIT = 2       !  output units:  meters

        ELSE IF ( GDTYP1 .EQ. EQMGRD3 ) THEN

            DSCR = P_GAM1
            DEG  = INT( DSCR )                              !  int degrees
            DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
            MNT  = INT( DSCR )                              !  int minutes
            DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
            TPOUT( 5 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

            DSCR = P_ALP1
            DEG  = INT( DSCR )                              !  int degrees
            DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
            MNT  = INT( DSCR )                              !  int minutes
            DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
            TPOUT( 6 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

            IOSYS  = 5       !  Equatorial Mercator
            IOZONE = 85
            IOUNIT = 2       !  output units:  meters

        ELSE

            MESG = 'Suppported grid types are:'
            CALL M3MSG2( MESG )
            MESG = 'LAM (2), POL (6), EQM (7)'
            CALL M3MSG2( MESG )
            WRITE( MESG, '( A, I10, 2X, A )' ) 'Requested grid type', GDTYP1, 'not supported'
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )

        END IF                  ! if dscgrid() failed, or if non-Lambert grid

        IPR    = 1
        JPR    = 1
        LEMSG  = LDEV
        INSYS  = 0       !  geographic coords (=Lat-Lon)
        INZONE = 0
        INUNIT = 4       !  input units:  degrees
        INSPH  = 19
        TPAIN( 1 ) = WRFEARTH
        CRDIN( 1 ) = DBLE( X )
        CRDIN( 2 ) = DBLE( Y )

        CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT,    &
                    INSPH, IPR, JPR, LEMSG, LPARM,          &
                    CRDIO, IOSYS, IOZONE, TPOUT, IOUNIT,    &
                    LN27, LN83, FN27, FN83, LENGTH, IFLG )

        IF ( IFLG .NE. 0 ) THEN
            WRITE( MESG, '( A, I10 )' ) 'Failure in GTPZ0:  Exit status=', IFLG
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF

        XORIGW = CRDIO( 1 ) - 0.5D0 * XCELL1
        YORIGW = CRDIO( 2 ) - 0.5D0 * YCELL1

        RETURN

    END SUBROUTINE GETORIG


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE  NULLFIX( X )

        CHARACTER(LEN=*), INTENT( INOUT ) :: X

        CHARACTER*1, PARAMETER :: CNULL = CHAR( 0 )
        CHARACTER*1, PARAMETER :: SPACE = ' '

        INTEGER L, I

        DO I = LEN( X ), 1, -1
            IF ( X(I:I) .EQ. CNULL ) THEN
                X(I:I) = SPACE
            ELSE
                EXIT
            END IF
        END DO

        RETURN

    END SUBROUTINE  NULLFIX


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    REAL*8  FUNCTION  DBLFIX( X )

        REAL, INTENT( IN ) :: X

        REAL*8, PARAMETER :: DFAC = DBLE( 16 * 81 * 25 )  !  = 2^4 * 3^4 * 5^2
        REAL*8, PARAMETER :: DINV = 1.0D0 / DFAC

        DBLFIX = DINV * DBLE( NINT( DFAC * DBLE( X ) ) )

        RETURN

    END FUNCTION DBLFIX


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION DBLERR( P, Q )
        REAL*8, INTENT( IN ) :: P, Q
        DBLERR = ( (P - Q)**2  .GT.  1.0D-10*( P*P + Q*Q + 1.0D-5 ) )
        RETURN
    END FUNCTION DBLERR



    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION FLTSAME( PP, QQ )
        REAL, INTENT( IN ) :: PP, QQ
        REAL*8 P, Q
        P = DBLE( PP )          !  protect against overflow for BADVAL3...
        Q = DBLE( QQ )
        FLTSAME = ( (P - Q)**2  .LT.  1.0D-9*( P*P + Q*Q + 1.0D-5 ) )
    END FUNCTION FLTSAME



END MODULE MODWRFIO
