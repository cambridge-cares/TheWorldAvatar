
LOGICAL FUNCTION CRDICT3( FID )

    !!***********************************************************************
    !! Version "$Id: crdict3.f90 1 2017-06-10 18:05:20Z coats $"
    !! BAMS/MCNC/EDSS/Models-3 I/O API
    !! Copyright (C) 1992-2002 MCNC, (C) 1992-2012 Carlie J. Coats, Jr.,
    !! (C) 2003-2011 Baron Advanced Meteorological Systems, and
    !! (C) 2015 UNC Institute for the Environment
    !! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    !! See file "LGPL.txt" for conditions of use.
    !!.........................................................................
    !!  function body starts at line  73
    !!
    !!  FUNCTION:  Perform "Models-3 variables" part of netCDF file creation
    !!             for CREATE3 for the dictionary file with index FID.
    !!
    !!  RETURN VALUE:  TRUE iff the operation succeeds
    !!
    !!  PRECONDITIONS REQUIRED:  Should only be called by CREATE3, after it has
    !!             performed the general attribute initializations appropriate
    !!             for all file types.
    !!
    !!  SUBROUTINES AND FUNCTIONS CALLED:  INDEX1
    !!
    !!  REVISION  HISTORY:
    !!       prototype 3/92 by CJC
    !!
    !!       Modified 03/20010 by CJC: F9x changes for I/O API v3.1
    !!
    !!       Modified 02/2015 by CJC for I/O API 3.2: Support for M3INT8
    !!
    !!       Modified 08/2015 by CJC:  USE MODNCFIO, "NF_" routines.
    !!       F90 "free" source format.
    !!***********************************************************************

    USE MODNCFIO

    IMPLICIT NONE

    !!...........   INCLUDES:

    INCLUDE 'PARMS3.EXT'
    INCLUDE 'STATE3.EXT'


    !!...........   ARGUMENTS and their descriptions:

    INTEGER, INTENT(IN   ) :: FID       !  file index within the STATE3 commons


    !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         IERR            !  netCDF error status return
    INTEGER         FNUM            !  local for CDFID( FID )
    INTEGER         TDIM            !  timestep  dim #
    INTEGER         VDIM            !  file-variable-name dim #
    INTEGER         ZDIM            !  vertical-layer dim #
    INTEGER         NDIM            !  name-char-position dim #
    INTEGER         DDIM            !  var-desc-char-position dim #
    INTEGER         LDIM            !  file-desc-line-# dim #
    INTEGER         DIMS( 5 )       !  array of dims for NF_DEF_VAR()
    INTEGER         VAR             !  loop counter
    CHARACTER*16    VUNIT( MXVARS3 )! variable-unit table
    CHARACTER*16    VDESC( MXVARS3 )! variable-description table


    !!***********************************************************************
    !!   begin body of function  CRDICT3

    !!.......   Local copy of netCDF file ID:

    FNUM = CDFID3( FID )

    !!...........   Dimensions TDIM for dictionary index number,
    !!...........   VDIM for variable number

    IERR = NF_DEF_DIM( FNUM, 'FINDX3', NF_UNLIMITED, TDIM )
    IF ( IERR .NE. 0 ) THEN
        WRITE( LOGDEV,91010 )                                   &
            'Error creating netCDF file dimension FINDX3.',     &
            'File name ' // FLIST3( FID ) ,                     &
            'netCDF error number', IERR
        CRDICT3 = .FALSE.
        IERR    = NF_ABORT( FNUM )
        RETURN
    END IF              !  ierr nonzero:  NF_DEF_DIM() failed

    IERR = NF_DEF_DIM( FNUM, 'VINDX', MXVARS3, VDIM )
    IF ( IERR .NE. 0 ) THEN
        WRITE( LOGDEV,91010 )                                   &
            'Error creating netCDF dimension VINDX',            &
            'File name ' // FLIST3( FID ) ,                     &
            'netCDF error number', IERR
        CRDICT3 = .FALSE.
        IERR    = NF_ABORT( FNUM )
        RETURN
    END IF              !  ierr nonzero:  NF_DEF_DIM() failed

    IERR = NF_DEF_DIM( FNUM, 'ZINDX', MXLAYS3+1, ZDIM )
    IF ( IERR .NE. 0 ) THEN
        WRITE( LOGDEV,91010 )                                   &
            'Error creating netCDF dimension ZINDX',            &
            'File name ' // FLIST3( FID ) ,                     &
            'netCDF error number', IERR
        CRDICT3 = .FALSE.
        IERR    = NF_ABORT( FNUM )
        RETURN
    END IF              !  ierr nonzero:  NF_DEF_DIM() failed

    !!...........   Dimensions NDIM for character-position in name variables,
    !!...........   DDIM for character-position in description variables,
    !!...........   LDIM for line-number in file-description variable:

    IERR = NF_DEF_DIM( FNUM, 'NAME-POS', NAMLEN3, NDIM )
    IF ( IERR .NE. 0 ) THEN
        WRITE( LOGDEV,91010 )                                   &
            'Error creating netCDF dimension NAME-POS.',        &
            'File name ' // FLIST3( FID ) ,                     &
            'netCDF error number', IERR
        CRDICT3 = .FALSE.
        IERR    = NF_ABORT( FNUM )
        RETURN
    END IF              !  ierr nonzero:  NF_DEF_DIM() failed

    IERR = NF_DEF_DIM( FNUM, 'DESC-POS', MXDLEN3, DDIM )
    IF ( IERR .NE. 0 ) THEN
        WRITE( LOGDEV,91010 )                                   &
            'Error creating netCDF dimension DESC-POS.',        &
            'File name ' // FLIST3( FID ) ,                     &
            'netCDF error number', IERR
        CRDICT3 = .FALSE.
        IERR    = NF_ABORT( FNUM )
        RETURN
    END IF              !  ierr nonzero:  NF_DEF_DIM() failed

    IERR = NF_DEF_DIM( FNUM, 'DLINE-NUM', MXDESC3,LDIM )
    IF ( IERR .NE. 0 ) THEN
        WRITE( LOGDEV,91010 )                                   &
            'Error creating netCDF dimension DLINE-NUM.',       &
            'File name ' // FLIST3( FID ) ,                     &
            'netCDF error number', IERR
        CRDICT3 = .FALSE.
        IERR    = NF_ABORT( FNUM )
        RETURN
    END IF              !  ierr nonzero:  NF_DEF_DIM() failed


    !!...........   File type:

    DIMS( 1 ) = TDIM        !  file indexes:  unlimited file index dimension

    IERR = NF_DEF_VAR( FNUM, 'FLAG', NF_INT, 1, DIMS, TINDX3( FID ) )
    IF ( IERR .NE. 0 ) THEN
        WRITE( LOGDEV,91010 )                           &
            'Error creating netCDF variable FLAG',      &
            'File name ' // FLIST3( FID ) ,             &
            'netCDF error number', IERR
        CRDICT3 = .FALSE.
        IERR    = NF_ABORT( FNUM )
        RETURN
    END IF              !  ierr nonzero:  NF_DEF_VAR() failed

    IERR = NF_PUT_ATT_TEXT( FNUM, TINDX3( FID ), 'units', NAMLEN3, '5461 or not' )
    IF ( IERR .NE. 0 ) THEN
        WRITE( LOGDEV,91010 )                           &
            'Error creating FLAG attribute UNITS',      &
            'File name ' // FLIST3( FID ) ,             &
            'netCDF error number', IERR
        CRDICT3 = .FALSE.
        IERR    = NF_ABORT( FNUM )
        RETURN
    END IF              !  ierr nonzero:  operation failed

    IERR = NF_PUT_ATT_TEXT( FNUM, TINDX3( FID ), 'var_desc', MXDLEN3, 'Record-valid flags:  OKFLAG3=5461, or not' )
    IF ( IERR .NE. 0 ) THEN
        WRITE( LOGDEV,91010 )                           &
            'Error creating FLAG attribute VAR_DESC',   &
            'File name ' // FLIST3( FID ) ,             &
            'netCDF error number', IERR
        CRDICT3 = .FALSE.
        IERR    = NF_ABORT( FNUM )
        RETURN
    END IF              !  ierr nonzero:  operation failed


    !!...............   Variable list:

    VLIST3(  1,FID ) = 'FNAME'      !  type:  char*16
    VUNIT (  1 )     = 'name text'
    VDESC (  1 )     = 'File-schema name variable'

    VLIST3(  2,FID ) = 'FTYPE'      !  type:  integer
    VUNIT (  2 )     = '-1...5'
    VDESC (  2 )     = 'file type variable'

    VLIST3(  3,FID ) = 'TSTEP'      !  type:  integer
    VUNIT (  3 )     = 'hhmmss'
    VDESC (  3 )     = 'time step variable'

    VLIST3(  4,FID ) = 'NVARS'      !  type:  integer
    VUNIT (  4 )     = 'count'
    VDESC (  4 )     = 'variables-count variable'

    VLIST3(  5,FID ) = 'NLAYS'      !  type:  integer
    VUNIT (  5 )     = 'count'
    VDESC (  5 )     = 'layers-count variable'

    VLIST3(  6,FID ) = 'NROWS'      !  type:  integer
    VUNIT (  6 )     = 'count'
    VDESC (  6 )     = 'rows-count variable'

    VLIST3(  7,FID ) = 'NCOLS'      !  type:  integer
    VUNIT (  7 )     = 'count'
    VDESC (  7 )     = 'columns-count variable'

    VLIST3(  8,FID ) = 'NTHIK'      !  type:  integer
    VUNIT (  8 )     = 'count'
    VDESC (  8 )     = 'bdy perimeter thickness variable'

    VLIST3(  9,FID ) = 'GDTYP'      !  type:  integer
    VUNIT (  9 )     = 'token'
    VDESC (  9 )     = 'horizontal grid type (see FDESC3.EXT)'

    VLIST3( 10,FID ) = 'VGTYP'      !  type:  integer
    VUNIT ( 10 )     = 'token'
    VDESC ( 10 )     = 'vertical grid type (see FDESC3.EXT)'

    VLIST3( 11,FID ) = 'VGLVS'      !  type:  float
    VUNIT ( 11 )     = 'vert.proj'
    VDESC ( 11 )     = 'level surfaces'

    VLIST3( 12,FID ) = 'P_ALP'      !  type:  double
    VUNIT ( 12 )     = 'deg'
    VDESC ( 12 )     = 'First map projection parameter angle'

    VLIST3( 13,FID ) = 'P_BET'      !  type:  double
    VUNIT ( 13 )     = 'deg'
    VDESC ( 13 )     = 'Second map projection parameter angle'

    VLIST3( 14,FID ) = 'P_GAM'      !  type:  double
    VUNIT ( 14 )     = 'deg'
    VDESC ( 14 )     = 'Third map projection parameter angle'

    VLIST3( 15,FID ) = 'XCENT'      !  type:  double
    VUNIT ( 15 )     = 'deg.lon'
    VDESC ( 15 )     = 'X-coordinate map coord origin variable'

    VLIST3( 16,FID ) = 'YCENT'      !  type:  double
    VUNIT ( 16 )     = 'deg.lat'
    VDESC ( 16 )     = 'Y-coordinate map coord origin variable'

    VLIST3( 17,FID ) = 'XORIG'      !  type:  double
    VUNIT ( 17 )     = 'position'
    VDESC ( 17 )     = 'X-coordinate grid origin variable'

    VLIST3( 18,FID ) = 'YORIG'      !  type:  double
    VUNIT ( 18 )     = 'position'
    VDESC ( 18 )     = 'Y-coordinate grid origin variable'

    VLIST3( 19,FID ) = 'XCELL'      !  type:  double
    VUNIT ( 19 )     = 'size'
    VDESC ( 19 )     = 'X-coordinate cell size variable'

    VLIST3( 20,FID ) = 'YCELL'      !  type:  double
    VUNIT ( 20 )     = 'size'
    VDESC ( 20 )     = 'Y-coordinate cell size  variable'

    VLIST3( 21,FID ) = 'GDNAM'      !  type:  char*16
    VUNIT ( 21 )     = 'name text'
    VDESC ( 21 )     = 'grid domain name variable'

    VLIST3( 22,FID ) = 'FDESC'      !  type:  char*80( MXDESC3=4 )
    VUNIT ( 22 )     = 'desc text'
    VDESC ( 22 )     = 'file description variable'

    VLIST3( 23,FID ) = 'VNAME'      !  type:  char*16( NVARS )
    VUNIT ( 23 )     = 'name text'
    VDESC ( 23 )     = 'variable-names-list variable'

    VLIST3( 24,FID ) = 'VUNIT'      !  type:  char*16( NVARS )
    VUNIT ( 24 )     = 'name text'
    VDESC ( 24 )     = 'variable-units-list  variable'

    VLIST3( 25,FID ) = 'VDESC'      !  type:  char*80( NVARS )
    VUNIT ( 25 )     = 'desc text'
    VDESC ( 25 )     = 'variable-descriptions-list  variable'

    VLIST3( 26,FID ) = 'VTYPE'      !  type:  integer
    VUNIT ( 26 )     = 'basic datatype'
    VDESC ( 26 )     = 'M3INT, M3REAL, M3DBLE, or M3INT8 for INTEGER, REAL, DOUBLE PRECISION, or INTEGER*8 types'


    !!.......   Put header attributes:  number of variables and
    !!.......   variables-list for the file:

    NVARS3( FID ) = 26
    IERR = NF_PUT_ATT_INT( FNUM, NF_GLOBAL, 'NVARS', NF_INT, 1, NVARS3( FID ) )
    IF ( IERR .NE. 0 ) THEN
        WRITE( LOGDEV,91010 )                           &
            'Error creating netCDF attribute NVARS',    &
            'File name ' // FLIST3( FID ) ,             &
            'netCDF error number', IERR
        CRDICT3 = .FALSE.
        IERR = NF_ABORT( FNUM )
        RETURN
    END IF          !  ierr nonzero:  operation failed

    IERR = NF_PUT_ATT_TEXT( FNUM, NF_GLOBAL, 'VAR-LIST',  NAMLEN3 * NVARS3( FID ), VLIST3( 1,FID ) )
    IF ( IERR .NE. 0 ) THEN
        WRITE( LOGDEV,91010 )                           &
            'Error creating attribute VAR-LIST ',       &
            'File name ' // FLIST3( FID ) ,             &
            'netCDF error number', IERR
        CRDICT3 = .FALSE.
        IERR = NF_ABORT( FNUM )
        RETURN
    END IF              !  ierr nonzero:  operation failed


    !!.......   Make netCDF definitions for all these variables:

    !!...........   Schema name:

    DIMS( 1 )  = NDIM   !  namlen3 (=16) characters per name
    DIMS( 2 )  = TDIM   !  unlimited file index dimension

    IERR = NF_DEF_VAR( FNUM, VLIST3( 1,FID ), NF_CHAR, 2, DIMS, VINDX3( 1,FID ) )
    IF ( IERR .NE. 0 ) THEN
        WRITE( LOGDEV,91010 )                           &
            'Error creating netCDF variable FNAME' ,    &
            'File name ' // FLIST3( FID ) ,             &
            'netCDF error number', IERR
        CRDICT3 = .FALSE.
        IERR = NF_ABORT( FNUM )
        RETURN
    END IF                  !  ierr nonzero:  NF_DEF_VAR() failed

    DIMS( 1 ) = TDIM        !  unlimited file index dimension

    DO  111  VAR = 2 , 10   !  define the integer variables

        IERR = NF_DEF_VAR( FNUM, VLIST3( VAR,FID ), NF_INT, 1, DIMS, VINDX3( VAR,FID ) )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )                                           &
                'Error creating netCDF variable ' // VLIST3( VAR,FID ),     &
                'File name ' // FLIST3( FID ) ,                             &
                'netCDF error number', IERR
            CRDICT3 = .FALSE.
            IERR = NF_ABORT( FNUM )
            RETURN
        END IF              !  ierr nonzero:  NF_DEF_VAR() failed

111 CONTINUE        !  end loop on integer variables

    DIMS( 1 )  = ZDIM   !  number of layer surfaces
    DIMS( 2 )  = TDIM   !  unlimited file index dimension

    IERR = NF_DEF_VAR( FNUM, VLIST3( 11,FID ), NF_DOUBLE, 1, DIMS, VINDX3( 11,FID ) )
    IF ( IERR .NE. 0 ) THEN
        WRITE( LOGDEV,91010 )                                           &
            'Error creating netCDF variable ' // VLIST3( 11,FID ),      &
            'File name ' // FLIST3( FID ) ,                             &
            'netCDF error number', IERR
        CRDICT3 = .FALSE.
        IERR = NF_ABORT( FNUM )
        RETURN
    END IF                  !  ierr nonzero:  NF_DEF_VAR() failed

    DIMS( 1 ) = TDIM        !  unlimited file index dimension

    DO  122  VAR = 12 , 20  !  define the double variables

        IERR = NF_DEF_VAR( FNUM, VLIST3( VAR,FID ), NF_DOUBLE, 1, DIMS, VINDX3( VAR,FID ) )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )                                       &
                'Error creating netCDF variable ' // VLIST3( VAR,FID ), &
                'File name ' // FLIST3( FID ) ,                         &
                'netCDF error number', IERR
            CRDICT3 = .FALSE.
            IERR = NF_ABORT( FNUM )
            RETURN
        END IF              !  ierr nonzero:  NF_DEF_VAR() failed

122 CONTINUE        !  end loop on double variables


    !!...........   Grid name:

    DIMS( 1 )  = NDIM   !  namlen3 (=16) characters per name
    DIMS( 2 )  = TDIM   !  unlimited file index dimension

    IERR = NF_DEF_VAR( FNUM, VLIST3( 21,FID ), NF_CHAR, 2, DIMS, VINDX3( 21,FID ) )
    IF ( IERR .NE. 0 ) THEN
        WRITE( LOGDEV,91010 )                           &
            'Error creating netCDF variable GDNAM' ,    &
            'File name ' // FLIST3( FID ) ,             &
            'netCDF error number', IERR
        CRDICT3 = .FALSE.
        IERR = NF_ABORT( FNUM )
        RETURN
    END IF              !  ierr nonzero:  NF_DEF_VAR() failed


    !!...........   File description:  MXDESC3 lines of MXDLEN3 characters per file

    DIMS( 1 )  = DDIM       !  mxdlen3 (=80) characters per line
    DIMS( 2 )  = LDIM       !  mxdesc3 (= 4) lines
    DIMS( 3 )  = TDIM       !  unlimited file index dimension

    IERR = NF_DEF_VAR( FNUM, VLIST3( 22,FID ), NF_CHAR, 3, DIMS, VINDX3( 22,FID ) )
    IF ( IERR .NE. 0 ) THEN
        WRITE( LOGDEV,91010 )                                           &
            'Error creating netCDF variable ' // VLIST3( 22,FID ),      &
            'File name ' // FLIST3( FID ) ,                             &
            'netCDF error number', IERR
        CRDICT3 = .FALSE.
        IERR = NF_ABORT( FNUM )
        RETURN
    END IF              !  ierr nonzero:  NF_DEF_VAR() failed


    !!...........   Variable names, units, and descriptions:

    DIMS( 1 ) = NDIM    !  namlen3 (=16) columns
    DIMS( 2 ) = VDIM    !  mxvars3 (=60) variables
    DIMS( 3 ) = TDIM    !  unlimited file index dimension

    IERR = NF_DEF_VAR( FNUM, VLIST3( 23,FID ), NF_CHAR, 3, DIMS, VINDX3( 23,FID ) )
    IF ( IERR .NE. 0 ) THEN
        WRITE( LOGDEV,91010 )                                           &
            'Error creating netCDF variable ' // VLIST3( 23,FID ),      &
            'File name ' // FLIST3( FID ) ,                             &
            'netCDF error number', IERR
        CRDICT3 = .FALSE.
        IERR = NF_ABORT( FNUM )
        RETURN
    END IF              !  ierr nonzero:  NF_DEF_VAR() failed

    IERR = NF_DEF_VAR( FNUM, VLIST3( 24,FID ), NF_CHAR, 3, DIMS, VINDX3( 24,FID ) )
    IF ( IERR .NE. 0 ) THEN
        WRITE( LOGDEV,91010 )                                           &
            'Error creating netCDF variable ' // VLIST3( 24,FID ),      &
            'File name ' // FLIST3( FID ) ,                             &
            'netCDF error number', IERR
        CRDICT3 = .FALSE.
        IERR = NF_ABORT( FNUM )
        RETURN
    END IF              !  ierr nonzero:  NF_DEF_VAR() failed

    DIMS( 1 ) = DDIM    !  mxdlen3 (=80) chars per description
    DIMS( 2 ) = VDIM    !  mxvars3 (=60) variables per file
    DIMS( 3 ) = TDIM    !  unlimited file index dimension

    IERR = NF_DEF_VAR( FNUM, VLIST3( 25,FID ), NF_CHAR, 3, DIMS, VINDX3( 25,FID ) )
    IF ( IERR .NE. 0 ) THEN
        WRITE( LOGDEV,91010 )                                           &
            'Error creating netCDF variable ' // VLIST3( 25,FID ),      &
            'File name ' // FLIST3( FID ) ,                             &
            'netCDF error number', IERR
        CRDICT3 = .FALSE.
        IERR = NF_ABORT( FNUM )
        RETURN
    END IF              !  ierr nonzero:  NF_DEF_VAR() failed

    DIMS( 1 ) = VDIM    !  mxvars3 (=60) variables per file
    DIMS( 2 ) = TDIM    !  unlimited file index dimension

    IERR = NF_DEF_VAR( FNUM, VLIST3( 26,FID ), NF_INT, 2, DIMS, VINDX3( 26,FID ) )
    IF ( IERR .NE. 0 ) THEN
        WRITE( LOGDEV,91010 )                                           &
            'Error creating netCDF variable ' // VLIST3( 26,FID ),      &
            'File name ' // FLIST3( FID ) ,                             &
            'netCDF error number', IERR
        CRDICT3 = .FALSE.
        IERR = NF_ABORT( FNUM )
        RETURN
    END IF              !  ierr nonzero:  NF_DEF_VAR() failed


    DO  133  VAR = 1 , NVARS3( FID )    !  put units and descriptions:

        IERR = NF_PUT_ATT_TEXT( FNUM, VINDX3( VAR,FID ), 'units',  NAMLEN3, VUNIT( VAR ) )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )                                               &
 &              'Error creating ' // VLIST3( VAR,FID ) // ' attribute UNITS',   &
 &              'File name ' // FLIST3( FID ) ,                                 &
 &              'netCDF error number', IERR
            CRDICT3 = .FALSE.
            IERR = NF_ABORT( FNUM )
            RETURN
        END IF              !  ierr nonzero:  operation failed

        IERR = NF_PUT_ATT_TEXT( FNUM, VINDX3( VAR,FID ), 'var_desc',  MXDLEN3, VDESC( VAR ) )
        IF ( IERR .NE. 0 ) THEN
            WRITE( LOGDEV,91010 )                                                   &
 &              'Error creating ' // VLIST3( VAR,FID ) // ' attribute VAR_DESC',    &
 &              'File name:  ' // FLIST3( FID ),                                    &
 &              'netCDF error number', IERR
            CRDICT3 = .FALSE.
            IERR = NF_ABORT( FNUM )
            RETURN
        END IF              !  ierr nonzero:  operation failed

133 CONTINUE    !  end loop putting units and descriptions.


    !!.......   Put FNUM back into data mode:

    IERR =  NF_ENDDEF( FNUM )
    IF ( IERR .NE. 0 ) THEN
        WRITE( LOGDEV,91010 )                               &
            'Error returning netCDF file into data mode.',  &
            'File name ' // FLIST3( FID ) ,                 &
            'netCDF error number', IERR
        IERR = NF_ABORT( FNUM )
        CRDICT3 = .FALSE.
        RETURN
    END IF          !  ierr nonzero:  operation failed

    COUNT3  =  COUNT3  +  1
    CRDICT3 = .TRUE.
    RETURN

        !!******************  FORMAT  STATEMENTS   ******************************

        !!...........   Error and warning message formats..... 91xxx

91010   FORMAT ( //5X , '>>> WARNING in subroutine CRDICT3 <<<',    &
              3 ( /5X , A , : ) , I5, // )


END FUNCTION CRDICT3

