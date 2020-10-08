
MODULE MODGCTP

    !!***************************************************************
    !!  Version "$Id: modgctp.f90 108 2018-09-07 18:59:37Z coats $"
    !!  Copyright (c) 2014-2015 UNC Institute for the Environment and
    !!  (C) 2015-2018 Carlie J. Coats, Jr.
    !!  Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    !!  See file "LGPL.txt" for conditions of use.
    !!..............................................................
    !!  DESCRIPTION:
    !!      GRID2XY:  Compute cell-centers  <XLOC2,YLOC2> for GRID2 relative
    !!      to the coordinate system for GRID1 using USGS GCTP-package
    !!      routine GTPZ0()
    !!
    !!      XY2XY:  Transform array <XLOC2,YLOC2> of GRID2-coordinates
    !!      into GRID1-coordinates <XLOC1,YLOC1> using USGS GCTP-package
    !!      routine  GTPZ0()
    !!
    !!      GRID2INDX, PNTS2INDX:  Compute single-indexed array subscripts
    !!      IX and bilinear interpolation fractions PX, PY for input-points
    !!      or grid relative to output-grid GRID1 using USGS GCTP-package
    !!      routine GTPZ0(), for use in INDXMULT() below
    !!
    !!      INDXMULT:  Use <IX,PX,PY> to do bilinear interpolation
    !!      from an input-grid 2D or 3D array to an output 2D or 3D array
    !!      (which may be single-indexed in the horizontal, or not)
    !!
    !!      M3TOGTPZ:  Set up input or output arguments for GTPZ0()
    !!
    !!      STDSPHERES, SPHERENAMES:  sphere parameters
    !!
    !!      SETSPHERE, INITSPHERES, SPHEREDAT:  spheroid-manipulation routines
    !!
    !!      LL2LAM...ALB2EQM:  single-precision/single-point coordinate
    !!      transform routines (mostly for SMOKE and for backwards compatibility)
    !!
    !!  REVISION  HISTORY:
    !!      Prototype  8/2013 by Carlie J. Coats, Jr., BAMS
    !!      Bug-fix    1/2014 by CJC:  uninitialized EFLAG bugs
    !!      Version   11/2014 by CJC:  Thread-safety:  OpenMP parallel
    !!      and critical sections; support for additional spheres.
    !!      Version   12/2014 by CJC for I/O API-3.2:  move to "ioapi" and
    !!      unify with "lambert.f", "ll2utm.f", "utm2ll.f", and "setsphere.f"
    !!      Version  11/2015:  re-add LAMBERT etc. INTERFACEs from 3.1
    !!      to MODULE M3UTILIO, together with re-naming clauses here
    !!      to avoid double-declaration problems.
    !!      Version  1/2018 by CJC:  Handle "missing" in XY2XY()
    !!      Version  8/2018 by CJC:  bug fixes in GRD2INDX, GRID2XY
    !!..............................................................

    USE M3UTILIO, M3U_GTPZ0       => GTPZ0      ,   &
                  M3U_SETSPHERE   => SETSPHERE  ,   &
                  M3U_INITSPHERES => INITSPHERES,   &
                  M3U_SPHEREDAT   => SPHEREDAT  ,   &
                  M3U_LAMBERT     => LAMBERT    ,   &
                  M3U_EQMERC      => EQMERC     ,   &
                  M3U_TRMERC      => TRMERC     ,   &
                  M3U_ALBERS      => ALBERS     ,   &
                  M3U_SETLAM      => SETLAM     ,   &
                  M3U_SETPOL      => SETPOL     ,   &
                  M3U_SETEQM      => SETEQM     ,   &
                  M3U_SETTRM      => SETTRM     ,   &
                  M3U_SETALB      => SETALB     ,   &
                  M3U_LAM2LL      => LAM2LL     ,   &
                  M3U_LL2LAM      => LL2LAM     ,   &
                  M3U_UTM2LL      => UTM2LL     ,   &
                  M3U_LL2UTM      => LL2UTM     ,   &
                  M3U_LAM2UTM     => LAM2UTM    ,   &
                  M3U_UTM2LAM     => UTM2LAM    ,   &
                  M3U_LAM2POL     => LAM2POL    ,   &
                  M3U_POL2LAM     => POL2LAM    ,   &
                  M3U_POL2LL      => POL2LL     ,   &
                  M3U_LL2POL      => LL2POL     ,   &
                  M3U_POL2UTM     => POL2UTM    ,   &
                  M3U_UTM2POL     => UTM2POL    ,   &
                  M3U_TRM2LL      => TRM2LL     ,   &
                  M3U_LL2TRM      => LL2TRM     ,   &
                  M3U_LAM2TRM     => LAM2TRM    ,   &
                  M3U_TRM2LAM     => TRM2LAM    ,   &
                  M3U_TRM2UTM     => TRM2UTM    ,   &
                  M3U_UTM2TRM     => UTM2TRM    ,   &
                  M3U_TRM2POL     => TRM2POL    ,   &
                  M3U_POL2TRM     => POL2TRM    ,   &
                  M3U_EQM2LL      => EQM2LL     ,   &
                  M3U_LL2EQM      => LL2EQM     ,   &
                  M3U_LAM2EQM     => LAM2EQM    ,   &
                  M3U_EQM2LAM     => EQM2LAM    ,   &
                  M3U_EQM2UTM     => EQM2UTM    ,   &
                  M3U_UTM2EQM     => UTM2EQM    ,   &
                  M3U_EQM2POL     => EQM2POL    ,   &
                  M3U_POLSTE      => POLSTE     ,   &
                  M3U_POL2EQM     => POL2EQM    ,   &
                  M3U_EQM2TRM     => EQM2TRM    ,   &
                  M3U_TRM2EQM     => TRM2EQM    ,   &
                  M3U_ALB2LL      => ALB2LL     ,   &
                  M3U_LL2ALB      => LL2ALB

    IMPLICIT NONE

    !!--------  Routines exported by this module:  ----------------------

    PUBLIC  STDSPHERES, SPHERENAMES
    PUBLIC  SETSPHERE, SPHEREDAT, INITSPHERES
    PUBLIC  M3TOGTPZ,  GRID2XY, GRID2INDX, PNTS2INDX, XY2XY, INDXMULT
    PUBLIC  INITPROJ, LAMBERT, POLSTE,  TRMERC,  EQMERC,  ALBERS
    PUBLIC  SETPROJ,  SETLAM,  SETPOL,  SETTRM,  SETEQM,  SETALB
    PUBLIC  LL2LAM,   LL2UTM,  LL2POL,  LL2TRM,  LL2EQM,  LL2ALB
    PUBLIC  LAM2LL,   LAM2UTM, LAM2POL, LAM2TRM, LAM2EQM, LAM2ALB
    PUBLIC  POL2LL,   POL2LAM, POL2UTM, POL2TRM, POL2EQM, POL2ALB
    PUBLIC  TRM2LL,   TRM2LAM, TRM2UTM, TRM2POL, TRM2EQM, TRM2ALB
    PUBLIC  EQM2LL,   EQM2LAM, EQM2UTM, EQM2POL, EQM2TRM, EQM2ALB
    PUBLIC  ALB2LL,   ALB2LAM, ALB2UTM, ALB2POL, ALB2TRM, ALB2EQM

    PRIVATE     !!  everything else  !!

    !!...........   PARAMETERs and their descriptions:

    REAL*8, PARAMETER :: AMISSD  = DBLE(  AMISS3 )
    REAL*8, PARAMETER :: BADVALD = DBLE( BADVAL3 )

    REAL*8, PARAMETER :: D60 = 1.0D0 / 60.0d0
    REAL*8, PARAMETER :: PI  = 3.141592653589793238462643383279d0
    REAL*8, PARAMETER :: PI180  = PI / 180.0d0
    REAL*8, PARAMETER :: RPI180 = 180.0d0 / PI
    REAL*8, PARAMETER :: RADE19   = 6370997.0d0
    REAL*8, PARAMETER :: RADE20   = 6370000.0d0
    REAL*8, PARAMETER :: RADE21   = 6371200.0d0

    CHARACTER*1,  PARAMETER ::  BLANK = ' '
    CHARACTER*16, PARAMETER ::  MNAME = 'MODGCTP'

    INTEGER, PARAMETER :: STDSPHERES( 0:21 ) =          &
          (/ 0,  1,  2,  3,  4,  5,  6,  7,  8,  9,     &
            10, 11, 12, 13, 14, 15, 16, 17, 18, 19,     &
            20, 21 /)

    CHARACTER*48, PARAMETER :: SPHERENAMES( 0:21 ) =            &
          (/ 'Clarke 1866                               ' ,     &   !!   0
             'Clarke 1880                               ' ,     &   !!   1
             'Bessel                                    ' ,     &   !!   2
             'New International 1967                    ' ,     &   !!   3
             'International 1909                        ' ,     &   !!   4
             'WGS 72                                    ' ,     &   !!   5
             'Everest                                   ' ,     &   !!   6
             'WGS 66                                    ' ,     &   !!   7
             'GRS 1980                                  ' ,     &   !!   8
             'Airy                                      ' ,     &   !!   9
             'Modified Everest                          ' ,     &   !!  10
             'Modified Airy                             ' ,     &   !!  11
             'WGS 84                                    ' ,     &   !!  12
             'Southeast Asia                            ' ,     &   !!  13
             'Australian National                       ' ,     &   !!  14
             'Krassovsky                                ' ,     &   !!  15
             'Hough                                     ' ,     &   !!  16
             'Mercury 1960                              ' ,     &   !!  17
             'Modified Mercury 1968                     ' ,     &   !!  18
             'Normal SPHERE, R_Earth=6370997            ' ,     &   !!  19
             'Normal Sphere (MM5 / WRF-ARW) R=6370000   ' ,     &   !!  20
             'Normal Sphere (WRF-NMM) R=6371200         '      /)   !!  21

    !!.......   Error codes for GTPZ0:

    CHARACTER*64, PARAMETER :: GCTPMESG( 9 ) = (/               &
        'Illegal  input system code INSYS               ',      &
        'Illegal output system code IOSYS               ',      &
        'Illegal  input unit code INUNIT                ',      &
        'Illegal output unit code IOUNIT                ',      &
        'Inconsistent unit and system codes for  input  ',      &
        'Inconsistent unit and system codes for output  ',      &
        'Illegal  input zone code INZONE                ',      &
        'Illegal output zone code IOZONE                ',      &
        'Projection-specific error                      ' /)


    !!--------  Generic interfaces:  -----------------------------------

    INTERFACE
        SUBROUTINE GTPZ0( CRDIN, INSYS, INZONE, TPARIN, INUNIT,     &
                          INSPH, IPR, JPR, LEMSG, LPARM,            &
                          CRDIO, IOSYS, IOZONE, TPARIO, IOUNIT,     &
                          LN27, LN83, FN27, FN83, LENGTH, IFLG )
        REAL*8 , INTENT( IN ) :: CRDIN(2), TPARIN(15), TPARIO(15)
        INTEGER, INTENT( IN ) :: INSYS, INZONE, INUNIT, INSPH
        INTEGER, INTENT( IN ) :: IPR, JPR, LEMSG, LPARM, IOUNIT
        INTEGER, INTENT( IN ) :: LN27, LN83, LENGTH
        CHARACTER(LEN=128), INTENT( IN ) :: FN27, FN83
        REAL*8 , INTENT( OUT ) :: CRDIO(2)
        INTEGER, INTENT( OUT ) :: IFLG
        END SUBROUTINE GTPZ0
    END INTERFACE

    INTERFACE  GRID2XY
        MODULE PROCEDURE  GRID2XY1, GRID2XY2
    END INTERFACE

    INTERFACE  GRID2INDX
        MODULE PROCEDURE  GRID2INDX1, GRID2INDX2
    END INTERFACE

    INTERFACE  PNTS2INDX
        MODULE PROCEDURE  PNTS2INDX1, PNTS2INDX2
    END INTERFACE

    INTERFACE  XY2XY
        MODULE PROCEDURE  XY2XY0D1, XY2XY0D2,   &
                          XY2XY1D1, XY2XY1D2,   &
                          XY2XY2D1, XY2XY2D2
    END INTERFACE

    INTERFACE  INDXMULT
        MODULE PROCEDURE  INDXMULT1, INDXMULT2, INDXMULT4
    END INTERFACE

    INTERFACE  M3TOGTPZ
        MODULE PROCEDURE  M3TOGTPZ1, M3TOGTPZ2
    END INTERFACE

    INTERFACE  INITPROJ
        MODULE PROCEDURE  INITPROJS, INITPROJD, INITPROJ1
    END INTERFACE

    INTERFACE  LL2UTM
        MODULE PROCEDURE  LL2UTMI, LL2UTMR
    END INTERFACE

    INTERFACE  UTM2LL
        MODULE PROCEDURE  UTM2LLI, UTM2LLR
    END INTERFACE

    INTERFACE  SETPROJ
        MODULE PROCEDURE  SETPROJS, SETPROJD
    END INTERFACE


    CHARACTER*132, SAVE :: SVN_ID = &
'$Id:: modgctp.f90 108 2018-09-07 18:59:37Z coats                     $'


    !!  internal state-variables for SETSPHERE, INITSPHERES, SPHEREDAT:

    INTEGER, SAVE :: KSPH   = 8      !  default GRS80
    INTEGER, SAVE :: NCALLS = 0

    REAL*8, SAVE :: AXISMAJ = 0.0D0
    REAL*8, SAVE :: AXISMIN = 0.0D0


    !!  internal state-variables for LAMBERT() etc.

    REAL*8, SAVE :: P_ALPL     !  first, second, third map
    REAL*8, SAVE :: P_BETL     !  projection descriptive
    REAL*8, SAVE :: P_GAML     !  parameters
    REAL*8, SAVE :: XCENTL     !  lon for coord-system X=0
    REAL*8, SAVE :: YCENTL     !  lat for coord-system Y=0

    REAL*8, SAVE :: P_ALPP     !  first, second, third map
    REAL*8, SAVE :: P_BETP     !  projection descriptive
    REAL*8, SAVE :: P_GAMP     !  parameters
    REAL*8, SAVE :: XCENTP     !  lon for coord-system X=0
    REAL*8, SAVE :: YCENTP     !  lat for coord-system Y=0


    REAL*8, SAVE :: P_ALPT     !  first, second, third map
    REAL*8, SAVE :: P_BETT     !  projection descriptive
    REAL*8, SAVE :: P_GAMT     !  parameters
    REAL*8, SAVE :: XCENTT     !  lon for coord-system X=0
    REAL*8, SAVE :: YCENTT     !  lat for coord-system Y=0

    REAL*8, SAVE :: P_ALPE     !  first, second, third map
    REAL*8, SAVE :: P_BETE     !  projection descriptive
    REAL*8, SAVE :: P_GAME     !  parameters
    REAL*8, SAVE :: XCENTE     !  lon for coord-system X=0
    REAL*8, SAVE :: YCENTE     !  lat for coord-system Y=0

    REAL*8, SAVE :: P_ALPA     !  first, second, third map
    REAL*8, SAVE :: P_BETA     !  projection descriptive
    REAL*8, SAVE :: P_GAMA     !  parameters
    REAL*8, SAVE :: XCENTA     !  lon for coord-system X=0
    REAL*8, SAVE :: YCENTA     !  lat for coord-system Y=0

    !!  Coordinate system ID's. Note that each call to set a new
    !!  coordinate system of the indicated type increments the
    !!  corresponding ID by 4 = Number of types implemented

    INTEGER, SAVE :: LZONE = 61   !  Lambert
    INTEGER, SAVE :: PZONE = 71   !  Polar Stereographic
    INTEGER, SAVE :: TZONE = 81   !  Transverse Mercator
    INTEGER, SAVE :: EZONE = 91   !  Equatorial Mercator
    INTEGER, SAVE :: AZONE = 101  !  Equatorial Mercator

    !!.......   Arguments for GTPZ0 (in POLSTE(), etc...)

    REAL*8            CRDIN( 2 )      !  input coordinates x,y
    INTEGER           INSYS           !  input projection code
    INTEGER           INZONE          !  input utm ZONE, etc.
    REAL*8            TPAIN( 15 )     !  input projection parameters
    INTEGER           INUNIT          !  input units code
    INTEGER           INSPH           !  spheroid code
    INTEGER           IPR             !  error print flag
    INTEGER           JPR             !  projection parameter print flag
    INTEGER           LEMSG           !  error message unit number
    INTEGER           LPARM           !  projection parameter unit number
    REAL*8            CRDIO( 2 )      !  output coordinates x,y
    INTEGER           IOSYS           !  output projection code
    INTEGER           IOZONE          !  output utm ZONE, etc.
    REAL*8            TPARO( 15 )     !  output projection parameters
    INTEGER           IOUNIT          !  output units code
    INTEGER           LN27            !  NAD1927 file unit number
    INTEGER           LN83            !  NAD1983 file unit number
    CHARACTER*128     FN27            !  NAD1927 file name
    CHARACTER*128     FN83            !  NAD1983 file name
    INTEGER           LENGTH          !  NAD* record-length
    INTEGER           IFLG            !  error flag                                                                  $'


CONTAINS    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION SETSPHERE( PARM1, PARM2 )

        !!........  Arguments and their descriptions:

        REAL*8 , INTENT(IN   ) :: PARM1, PARM2


        !!........  Local Variables and their descriptions:

        INTEGER         STATUS
        LOGICAL         EFLAG
        INTEGER         I1
        REAL*8          P1, P2, PP
        CHARACTER*256   MESG
        CHARACTER*256   EVALUE

        !!........  Body  ......................................................

        EFLAG = .FALSE.

        P1 = PARM1
        P2 = PARM2

        IF ( P1 .GT. -0.5D0 .AND. P1 .LT. 21.5D0 ) THEN
            I1 = NINT( P1 )
            PP = DBLE( I1 )
            IF ( DBLERR( P1, PP ) ) THEN
                EFLAG = .TRUE.
                MESG = 'Bad standard input sphere-number'
                CALL M3MSG2( MESG )
            ELSE
                MESG = 'SETSPHERE:  sphere ' // SPHERENAMES( I1 )
                CALL M3MSG2( MESG )
            END IF
        ELSE
            I1     = -NCALLS - 1
            NCALLS =  NCALLS + 1
            WRITE( MESG, '( A, 1X, 1PD25.16 )' ) 'SETSPHERE:  major axis', P1
            CALL M3MSG2( MESG )
            WRITE( MESG, '( A, 1X, 1PD25.16 )' ) 'SETSPHERE:  minor axis/eccentricity^2', P2
            CALL M3MSG2( MESG )
        END IF

        IF ( EFLAG ) THEN
            SETSPHERE = .FALSE.
        ELSE
            NCALLS    = NCALLS + 1
            KSPH      = I1
            AXISMAJ   = P1
            AXISMIN   = P2
            SETSPHERE = .TRUE.
        END IF

        RETURN

    END FUNCTION SETSPHERE

    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION INITSPHERES()

        !!........  Local Variables and their descriptions:

        INTEGER         STATUS, L, M
        LOGICAL         EFLAG
        INTEGER         I1
        REAL*8          P1, P2, PP
        CHARACTER*256   MESG
        CHARACTER*256   EVALUE

        !!........  Body  ......................................................


            IF ( NCALLS .GT. 0 ) THEN
                INITSPHERES = .TRUE.
                RETURN
            END IF

            EFLAG  = .FALSE.
            NCALLS = NCALLS + 1

            CALL ENVSTR( 'IOAPI_ISPH', 'Input sphere for GCTP', '8', EVALUE, STATUS )

            IF ( STATUS .GT. 0 ) THEN
                EFLAG = .TRUE.
                MESG = 'Bad environment value for "IOAPI_ISPH"'
                CALL M3MSG2( MESG )
            ELSE IF ( STATUS .EQ. 0 ) THEN
                L = LBLANK( EVALUE )
                M = L + INDEX( EVALUE( L+1:256 ), BLANK )
                P1 = STR2DBLE( EVALUE( L+1:M ) )
                IF ( EVALUE( M:256 ) .NE. ' ' ) THEN
                    P2 = STR2DBLE( EVALUE( M+1:256 ) )
                ELSE
                    P2 = 0.0D0
                END IF
            ELSE
                P1 = 8.0D0
                P2 = 0.0D0
            END IF

            IF ( P1 .GT. -0.5D0 .AND. P1 .LT. 21.5D0 ) THEN
                I1 = NINT( P1 )
                PP = DBLE( I1 )
                IF ( DBLERR( P1, PP ) ) THEN
                    EFLAG = .TRUE.
                    MESG = 'Bad standard input sphere-number'
                    CALL M3MSG2( MESG )
                ELSE
                    MESG = 'INITSPHERES:  input sphere ' // SPHERENAMES( I1 )
                    CALL M3MSG2( MESG )
                END IF
            ELSE
                I1     = -NCALLS - 1
                NCALLS =  NCALLS + 1
                WRITE( MESG, '( A, 1X, 1PD25.16 )' )  'INITSPHERES:  major axis', P1
                CALL M3MSG2( MESG )
                WRITE( MESG, '( A, 1X, 1PD25.16 )' )  'INITSPHERES:  minor axis/eccentricity^2', P2
                CALL M3MSG2( MESG )
                CALL M3MSG2( MESG )
            END IF

            IF ( EFLAG ) THEN
                INITSPHERES = .FALSE.
            ELSE
                KSPH    = I1
                AXISMAJ = P1
                AXISMIN = P2
                INITSPHERES = .TRUE.
            END IF

        RETURN

    END FUNCTION INITSPHERES


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION SPHEREDAT( INSPHERE, INPARAM, IOPARAM )

        !!........  Arguments and their descriptions:

        INTEGER, INTENT(  OUT) :: INSPHERE
        REAL*8 , INTENT(INOUT) :: INPARAM( 15 ), IOPARAM( 15 )

        !!........  Body  ......................................................

            INSPHERE     = KSPH
            INPARAM( 1 ) = AXISMAJ
            INPARAM( 2 ) = AXISMIN
            IOPARAM( 1 ) = AXISMAJ
            IOPARAM( 2 ) = AXISMIN

            SPHEREDAT    = .TRUE.

        RETURN

    END FUNCTION SPHEREDAT


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !  Compute cell-centers  <XLOC2,YLOC2> for GRID2 relative to
    !  the coordinate system for GRID1

    SUBROUTINE GRID2XY1( GDTYP1, P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1,    &
                         GDTYP2, P_ALP2, P_BET2, P_GAM2, XCENT2, YCENT2,    &
                         NCOLS2, NROWS2, XORIG2, YORIG2, XCELL2, YCELL2,    &
                         XLOC2, YLOC2 )

        !!........  Arguments and their descriptions:

        INTEGER, INTENT(IN   ) :: GDTYP1, GDTYP2
        INTEGER, INTENT(IN   ) :: NCOLS2, NROWS2
        REAL*8 , INTENT(IN   ) :: P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1
        REAL*8 , INTENT(IN   ) :: P_ALP2, P_BET2, P_GAM2, XCENT2, YCENT2
        REAL*8 , INTENT(IN   ) :: XORIG2, YORIG2, XCELL2, YCELL2

        REAL*8 , INTENT(  OUT) :: XLOC2( NCOLS2,NROWS2 )
        REAL*8 , INTENT(  OUT) :: YLOC2( NCOLS2,NROWS2 )

        CALL GRID2XY2( GDTYP1, P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1, DBLE(KSPH),  &
                       GDTYP2, P_ALP2, P_BET2, P_GAM2, XCENT2, YCENT2, DBLE(KSPH),  &
                       NCOLS2, NROWS2, XORIG2, YORIG2, XCELL2, YCELL2,              &
                       XLOC2, YLOC2 )

        RETURN


    END SUBROUTINE GRID2XY1


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE GRID2XY2( GDTYP1, P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1, SPHER1,   &
                         GDTYP2, P_ALP2, P_BET2, P_GAM2, XCENT2, YCENT2, SPHER2,   &
                         NCOLS2, NROWS2, XORIG2, YORIG2, XCELL2, YCELL2,    &
                         XLOC2, YLOC2 )

        !!........  Arguments and their descriptions:
        !!  SPHERE[1,2] should be 0-21 for "standard" spheres
        !!  (as in SPHERENAMES above); or else Earth-radius for
        !!  non-standard perfect spheres.

        INTEGER, INTENT(IN   ) :: GDTYP1, GDTYP2
        INTEGER, INTENT(IN   ) :: NCOLS2, NROWS2
        REAL*8 , INTENT(IN   ) :: SPHER1, SPHER2        !!  input, output spheres
        REAL*8 , INTENT(IN   ) :: P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1
        REAL*8 , INTENT(IN   ) :: P_ALP2, P_BET2, P_GAM2, XCENT2, YCENT2
        REAL*8 , INTENT(IN   ) :: XORIG2, YORIG2, XCELL2, YCELL2

        REAL*8 , INTENT(  OUT) :: XLOC2( NCOLS2,NROWS2 )
        REAL*8 , INTENT(  OUT) :: YLOC2( NCOLS2,NROWS2 )

        !!........  PARAMETERs and their descriptions:

        CHARACTER*24, PARAMETER ::  PNAME = 'MODGCTP/GRID2XY'
        CHARACTER*16, PARAMETER ::  BLANK = ' '
        CHARACTER*80, PARAMETER ::  BAR   = &
      '-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'


        !!........  Local Variables and their descriptions:

        INTEGER     ISTAT
        INTEGER     C, R
        LOGICAL     EFLAG
        REAL*8      X0, Y0

        CHARACTER*512   MESG

        !!   Arguments for GTPZ0:

        REAL*8          CRDIN( 2 )      !  input coordinates x,y
        INTEGER*4       INSYS           !  input projection code
        INTEGER*4       INZONE          !  input utm ZONE, etc.
        REAL*8          TPAIN( 15 )     !  input projection parameters
        INTEGER*4       INUNIT          !  input units code
        INTEGER*4       INSPH           !  spheroid code
        INTEGER*4       IPR             !  error print flag
        INTEGER*4       JPR             !  projection parameter print flag
        INTEGER*4       LEMSG           !  error message unit number
        INTEGER*4       LPARM           !  projection parameter unit number
        REAL*8          CRDIO( 2 )      !  output coordinates x,y
        INTEGER*4       IOSYS           !  output projection code
        INTEGER*4       IOZONE          !  output utm ZONE, etc.
        REAL*8          TPOUT( 15 )     !  output projection parameters
        INTEGER*4       IOUNIT          !  output units code
        INTEGER*4       LN27            !  NAD1927 file unit number
        INTEGER*4       LN83            !  NAD1983 file unit number
        CHARACTER*128   FN27            !  NAD1927 file name
        CHARACTER*128   FN83            !  NAD1983 file name
        INTEGER*4       LENGTH          !  NAD* record-length
        INTEGER*4       IFLG            !  error flag


        !!........  Body  ......................................................

        EFLAG = .FALSE.

        !!...............  Compute intermediate REAL*8 Lat-Lon for grid2 centers:

        X0 = XORIG2 - 0.5 * XCELL2
        Y0 = YORIG2 - 0.5 * YCELL2

        IF ( GDTYP2 .EQ. LATGRD3 ) THEN

!$OMP        PARALLEL DO                                                &
!$OMP&           DEFAULT( NONE ),                                       &
!$OMP&            SHARED( NROWS2, NCOLS2, X0, Y0, XCELL2, YCELL2,       &
!$OMP&                    XLOC2, YLOC2 ),                               &
!$OMP&           PRIVATE( C, R )

            DO  R = 1, NROWS2
            DO  C = 1, NCOLS2

                XLOC2( C,R ) = X0  +  XCELL2 * DBLE( C )
                YLOC2( C,R ) = Y0  +  YCELL2 * DBLE( R )

            END DO
            END DO          !  end traversal of input grid

        ELSE

            !!...............  Set up arguments for call to GTP0:

            IF ( .NOT.M3TOGTPZ( GDTYP2, 100, P_ALP2, P_BET2, P_GAM2, YCENT2, SPHER2,    &
                                TPAIN, INSYS, INZONE, INUNIT, INSPH ) ) THEN
                EFLAG = .TRUE.
                WRITE( MESG, '( A, I6, 2X, A )' ) '>>> Grid type', GDTYP2, 'not supported'
                CALL M3MESG( MESG )
                CALL M3EXIT( PNAME, 0, 0, 'Map-projection setup error(s)', 2 )
            END IF

            TPOUT  = 0.0D0
            IPR    = 0              !!  print error messages, if any
            JPR    = 1              !!  do NOT print projection parameters
            LEMSG  = INIT3()        !!  unit number for log file
            LPARM  = LEMSG          !!  projection parameters file

            IOSYS  = 0              !!  geographic coords (=Lat-Lon)
            IOZONE = 0
            IOUNIT = 4              !!  output units:  degrees

!$OMP       PARALLEL DO                                                 &
!$OMP&           DEFAULT( NONE ),                                       &
!$OMP&            SHARED( NROWS2, NCOLS2, X0, Y0, XCELL2, YCELL2,       &
!$OMP&                    XLOC2, YLOC2,                                 &
!$OMP&                    INSYS, INZONE, TPAIN, INUNIT, INSPH,          &
!$OMP&                    IOSYS, IOZONE, TPOUT, IOUNIT, LPARM,          &
!$OMP&                    IPR, JPR, LEMSG, LN27, LN83, FN27, FN83 ),    &
!$OMP&           PRIVATE( C, R, CRDIN, CRDIO, LENGTH, IFLG, MESG ),     &
!$OMP&         REDUCTION( .OR.:  EFLAG )

            DO  R = 1, NROWS2
            DO  C = 1, NCOLS2

                CRDIN( 1 ) = X0  +  XCELL2 * DBLE( C )
                CRDIN( 2 ) = Y0  +  YCELL2 * DBLE( R )

!$OMP           CRITICAL( S_GTPZ0 )
                CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,     &
                            IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,   &
                            TPOUT, IOUNIT, LN27, LN83, FN27, FN83,          &
                            LENGTH, IFLG )
!$OMP           END CRITICAL( S_GTPZ0 )

                IF ( IFLG .NE. 0 ) THEN
                    IFLG  = MAX( MIN( 9, IFLG ), 1 )   !  trap between 1 and 9
                    WRITE( MESG, '( A, I3, 2X, A, I5, A, I5, A )' ) &
                       'Failure:  status ', IFLG,                   &
                       'in GTPZ0 at (c,r)=(', C, ',', R, ')'
                    EFLAG = .TRUE.
                    CALL M3MESG( MESG )
                    CYCLE
                END IF

                XLOC2( C,R ) = CRDIO( 1 )
                YLOC2( C,R ) = CRDIO( 2 )

            END DO
            END DO          !  end traversal of input grid

            IF ( EFLAG ) THEN
                CALL M3EXIT( PNAME, 0, 0, 'Output-grid coord-transform error(s)', 2 )
            END IF

        END IF          !  if latgrd3, or not


        !!...............  Then compute GRID1 coords for GRID2 cell-centers

        IF ( GDTYP1 .EQ. LATGRD3 ) THEN

            CONTINUE

        ELSE

            IF ( .NOT.M3TOGTPZ( GDTYP1, 200, P_ALP1, P_BET1, P_GAM1, YCENT1, SPHER1,     &
                                TPOUT, IOSYS, IOZONE, IOUNIT, INSPH ) ) THEN

                MESG = 'Lat-Lon, LAM, UTM, TRM, POL, EQM, and ALB supported'
                CALL M3MSG2( MESG )
                WRITE( MESG, '( A, I6, 2X, A, A )' ) 'Grid type', GDTYP1,'not supported'
                CALL M3MESG( MESG )

                CALL M3EXIT( PNAME, 0, 0, 'Map-projection setup error(s)', 2 )

            END IF

            TPAIN   = 0.0D0     !!  array assignment
            TPAIN(1)= RADE19
            IPR     = 0         !!  print error messages, if any
            JPR     = 1         !!  do NOT print projection parameters
            LEMSG   = INIT3()   !!  unit number for log file
            LPARM   = LEMSG     !!  projection parameters file

            INSYS  = 0          !!  geographic (=Lat-Lon)
            INZONE = 0
            INUNIT = 4          !!  input units:  degrees

!$OMP       PARALLEL DO                                                 &
!$OMP&           DEFAULT( NONE ),                                       &
!$OMP&            SHARED( NROWS2, NCOLS2, XLOC2, YLOC2,                 &
!$OMP&                    INSYS, INZONE, TPAIN, INUNIT, INSPH,          &
!$OMP&                    IOSYS, IOZONE, TPOUT, IOUNIT, LPARM,          &
!$OMP&                    IPR, JPR, LEMSG, LN27, LN83, FN27, FN83 ),    &
!$OMP&           PRIVATE( C, R, CRDIN, CRDIO, LENGTH, IFLG, MESG ),     &
!$OMP&         REDUCTION( .OR.:  EFLAG )

            DO  R = 1, NROWS2
            DO  C = 1, NCOLS2

                CRDIN( 1 ) = XLOC2( C,R )
                CRDIN( 2 ) = YLOC2( C,R )

!$OMP           CRITICAL( S_GTPZ0 )
                CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,     &
                            IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,   &
                            TPOUT, IOUNIT, LN27, LN83, FN27, FN83,          &
                            LENGTH, IFLG )
!$OMP           END CRITICAL( S_GTPZ0 )

                IF ( IFLG .NE. 0 ) THEN
                    IFLG  = MAX( MIN( 9, IFLG ), 1 )   !  trap between 1 and 9
                    WRITE( MESG, '( A, I3, 2X, A, I5, A, I5, A )' ) &
                       'Failure:  status ', IFLG,                   &
                       'in GTPZ0 at (c,r)=(', C, ',', R, ')'
                    EFLAG = .TRUE.
                    CALL M3MESG( MESG )
                END IF

                XLOC2( C,R ) = CRDIO( 1 )
                YLOC2( C,R ) = CRDIO( 2 )

            END DO
            END DO

            IF ( EFLAG ) THEN
                CALL M3EXIT( PNAME, 0, 0, 'Input-grid coord-transform error(s)', 2 )
            END IF

        END IF          !  if latgrd3, or not

        RETURN


    END SUBROUTINE GRID2XY2


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE GRID2INDX1( GDTYP1, P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1,      &
                           GDTYP2, P_ALP2, P_BET2, P_GAM2, XCENT2, YCENT2,      &
                           NCOLS1, NROWS1, XORIG1, YORIG1, XCELL1, YCELL1,      &
                           NCOLS2, NROWS2, XORIG2, YORIG2, XCELL2, YCELL2,      &
                           IX2, PX2, PY2 )

        !!........  Arguments and their descriptions:

        INTEGER, INTENT(IN   ) :: GDTYP1, GDTYP2
        INTEGER, INTENT(IN   ) :: NCOLS1, NROWS1, NCOLS2, NROWS2
        REAL*8 , INTENT(IN   ) :: P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1
        REAL*8 , INTENT(IN   ) :: P_ALP2, P_BET2, P_GAM2, XCENT2, YCENT2
        REAL*8 , INTENT(IN   ) :: XORIG1, YORIG1, XCELL1, YCELL1
        REAL*8 , INTENT(IN   ) :: XORIG2, YORIG2, XCELL2, YCELL2

        INTEGER, INTENT(  OUT) :: IX2( NCOLS2*NROWS2 )
        REAL   , INTENT(  OUT) :: PX2( NCOLS2*NROWS2 )
        REAL   , INTENT(  OUT) :: PY2( NCOLS2*NROWS2 )

        CALL GRID2INDX2( GDTYP1, P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1, DBLE(KSPH),    &
                         GDTYP2, P_ALP2, P_BET2, P_GAM2, XCENT2, YCENT2, DBLE(KSPH),    &
                         NCOLS1, NROWS1, XORIG1, YORIG1, XCELL1, YCELL1,                &
                         NCOLS2, NROWS2, XORIG2, YORIG2, XCELL2, YCELL2,                &
                         IX2, PX2, PY2 )

        RETURN


    END SUBROUTINE GRID2INDX1

    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE GRID2INDX2( GDTYP1, P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1, SPHER1,  &
                           GDTYP2, P_ALP2, P_BET2, P_GAM2, XCENT2, YCENT2, SPHER2,  &
                           NCOLS1, NROWS1, XORIG1, YORIG1, XCELL1, YCELL1,          &
                           NCOLS2, NROWS2, XORIG2, YORIG2, XCELL2, YCELL2,          &
                           IX2, PX2, PY2 )

        !!........  Arguments and their descriptions:
        !!  SPHERE[1,2] should be 0-19 for "standard" spheres
        !!  (as in SPHERENAMES above); or else Earth-radius for
        !!  non-standard perfect spheres.

        INTEGER, INTENT(IN   ) :: GDTYP1, GDTYP2
        INTEGER, INTENT(IN   ) :: NCOLS1, NROWS1, NCOLS2, NROWS2
        REAL*8 , INTENT(IN   ) :: SPHER1, SPHER2        !!  input, output spheres
        REAL*8 , INTENT(IN   ) :: P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1
        REAL*8 , INTENT(IN   ) :: P_ALP2, P_BET2, P_GAM2, XCENT2, YCENT2
        REAL*8 , INTENT(IN   ) :: XORIG1, YORIG1, XCELL1, YCELL1
        REAL*8 , INTENT(IN   ) :: XORIG2, YORIG2, XCELL2, YCELL2

        INTEGER, INTENT(  OUT) :: IX2( NCOLS2*NROWS2 )
        REAL   , INTENT(  OUT) :: PX2( NCOLS2*NROWS2 )
        REAL   , INTENT(  OUT) :: PY2( NCOLS2*NROWS2 )

        !!........  PARAMETERs and their descriptions:

        CHARACTER*24, PARAMETER ::  PNAME = 'MODGCTP/GRID2INDX'
        CHARACTER*16, PARAMETER ::  BLANK = ' '
        CHARACTER*80, PARAMETER ::  BAR   = &
      '-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'


        !!........  Local Variables and their descriptions:

        INTEGER     C, R, CC, RR, J
        LOGICAL     EFLAG
        REAL*8      X0, Y0, X1, Y1, X2, Y2, DDX1, DDY1, XX, YY

        REAL*8      XLOC2( NCOLS2,NROWS2 )
        REAL*8      YLOC2( NCOLS2,NROWS2 )

        CHARACTER*512   MESG

        !!   Arguments for GTPZ0:

        REAL*8          CRDIN( 2 )      !  input coordinates x,y
        INTEGER*4       INSYS           !  input projection code
        INTEGER*4       INZONE          !  input utm ZONE, etc.
        REAL*8          TPAIN( 15 )     !  input projection parameters
        INTEGER*4       INUNIT          !  input units code
        INTEGER*4       INSPH           !  spheroid code
        INTEGER*4       IPR             !  error print flag
        INTEGER*4       JPR             !  projection parameter print flag
        INTEGER*4       LEMSG           !  error message unit number
        INTEGER*4       LPARM           !  projection parameter unit number
        REAL*8          CRDIO( 2 )      !  output coordinates x,y
        INTEGER*4       IOSYS           !  output projection code
        INTEGER*4       IOZONE          !  output utm ZONE, etc.
        REAL*8          TPOUT( 15 )     !  output projection parameters
        INTEGER*4       IOUNIT          !  output units code
        INTEGER*4       LN27            !  NAD1927 file unit number
        INTEGER*4       LN83            !  NAD1983 file unit number
        CHARACTER*128   FN27            !  NAD1927 file name
        CHARACTER*128   FN83            !  NAD1983 file name
        INTEGER*4       LENGTH          !  NAD* record-length
        INTEGER*4       IFLG            !  error flag


        !!........  Body  ......................................................

        DDX1 = 1.0d0 / XCELL1
        DDY1 = 1.0d0 / YCELL1
        X0 = XORIG2 - 0.5D0 * XCELL2
        Y0 = YORIG2 - 0.5D0 * YCELL2
        X1 = XORIG1 - 0.5D0 * XCELL1
        Y1 = YORIG1 - 0.5D0 * YCELL1
        X2 = DBLE( NCOLS1 )
        Y2 = DBLE( NROWS1 )

        IF ( SAMEPROJ2( GDTYP1, P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1, SPHER1,      &
                        GDTYP2, P_ALP2, P_BET2, P_GAM2, XCENT2, YCENT2, SPHER2 ) ) THEN

!$OMP        PARALLEL DO                                                        &
!$OMP&           DEFAULT( NONE ),                                               &
!$OMP&            SHARED( NROWS2, NCOLS2, X0, Y0, X1, Y1, X2, Y2, DDX1, DDY1,   &
!$OMP&                    NCOLS1, NROWS1, XCELL2, YCELL2, IX2, PX2, PY2 ),      &
!$OMP&           PRIVATE( C, R, XX, YY, J, CC, RR )

            DO  R = 1, NROWS2
            DO  C = 1, NCOLS2

                XX = DDX1 * ( X0  +  XCELL2 * DBLE( C ) - X1 )
                YY = DDY1 * ( Y0  +  YCELL2 * DBLE( R ) - Y1 )
                XX = MAX( XX, 1.0d0 )
                YY = MAX( YY, 1.0d0 )
                CC = MIN( INT( XX ), NCOLS1-1 )
                RR = MIN( INT( YY ), NROWS1-1 )
                J  = C + NCOLS2 * ( R - 1 )
                IX2( J ) = CC + NCOLS1 * ( RR - 1 )
                PX2( J ) = SNGL( 1.0D0 - MOD( XX, 1.0D0 ) )
                PY2( J ) = SNGL( 1.0D0 - MOD( XX, 1.0D0 ) )

            END DO
            END DO          !  end traversal of input grid

            RETURN

        END IF

        EFLAG = .FALSE.

        !!...............  Next, calculate Lat-Lon for GRID2 cell-centers
        !!...............  Set up arguments for call to GTP0:

        TPOUT   = 0.0D0
        TPOUT(1)= RADE19
        IPR     = 0              !!  print error messages, if any
        JPR     = 1              !!  do NOT print projection parameters
        LEMSG   = INIT3()        !!  unit number for log file
        LPARM   = LEMSG          !!  projection parameters file

        IOSYS  = 0              !!  geographic coords (=Lat-Lon)
        IOZONE = 0
        IOUNIT = 4              !!  output units:  degrees

        IF ( .NOT.M3TOGTPZ( GDTYP2, 100, P_ALP2, P_BET2, P_GAM2, YCENT2, SPHER2,    &
                            TPAIN, INSYS, INZONE, INUNIT, INSPH ) ) THEN
            EFLAG = .TRUE.
            WRITE( MESG, '( A, I6, 2X, A )' ) '>>> Output grid type', GDTYP2, 'not supported'
            CALL M3MESG( MESG )
            CALL M3EXIT( PNAME, 0, 0, 'Map-projection setup error(s)', 2 )
        END IF


        !!...............  Compute intermediate REAL*8 Lat-Lon:

        IF ( GDTYP2 .EQ. LATGRD3 ) THEN

!$OMP        PARALLEL DO                                                            &
!$OMP&           DEFAULT( NONE ),                                                   &
!$OMP&            SHARED( NROWS2, NCOLS2, X0, Y0,  XCELL2, YCELL2, XLOC2, YLOC2 ),  &
!$OMP&           PRIVATE( C, R )

            DO  R = 1, NROWS2
            DO  C = 1, NCOLS2

                XLOC2( C,R ) = X0  +  XCELL2 * DBLE( C )
                YLOC2( C,R ) = Y0  +  YCELL2 * DBLE( R )

            END DO
            END DO          !  end traversal of input grid

        ELSE

!$OMP        PARALLEL DO                                                &
!$OMP&           DEFAULT( NONE ),                                       &
!$OMP&            SHARED( NROWS2, NCOLS2, X0, Y0,  XCELL2, YCELL2,      &
!$OMP&                    XLOC2, YLOC2,                                 &
!$OMP&                    INSYS, INZONE, TPAIN, INUNIT, INSPH,          &
!$OMP&                    IOSYS, IOZONE, TPOUT, IOUNIT, LPARM,          &
!$OMP&                    IPR, JPR, LEMSG, LN27, LN83, FN27, FN83 ),    &
!$OMP&           PRIVATE( C, R, CRDIN, CRDIO, LENGTH, IFLG, MESG ),     &
!$OMP&         REDUCTION( .OR.:  EFLAG )

            DO  R = 1, NROWS2
            DO  C = 1, NCOLS2

                CRDIN( 1 ) = X0  +  XCELL2 * DBLE( C )
                CRDIN( 2 ) = Y0  +  YCELL2 * DBLE( R )

!$OMP           CRITICAL( S_GTPZ0 )
                CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,     &
                            IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,   &
                            TPOUT, IOUNIT, LN27, LN83, FN27, FN83,          &
                            LENGTH, IFLG )
!$OMP           END CRITICAL( S_GTPZ0 )

                IF ( IFLG .NE. 0 ) THEN
                    IFLG  = MAX( MIN( 9, IFLG ), 1 )   !  trap between 1 and 9
                    WRITE( MESG, '( A, I3, 2X, A, I5, A, I5, A )' ) &
                       'Failure:  status ', IFLG,                   &
                       'in GTPZ0 at (c,r)=(', C, ',', R, ')'
                    EFLAG = .TRUE.
                    CALL M3MESG( MESG )
                    CYCLE
                END IF

                XLOC2( C,R ) = CRDIO( 1 )
                YLOC2( C,R ) = CRDIO( 2 )

            END DO
            END DO          !  end traversal of input grid

            IF ( EFLAG ) THEN
                CALL M3EXIT( PNAME, 0, 0, 'Output-grid coord-transform error(s)', 2 )
            END IF

        END IF          !  if latgrd3, or not


        !!...............  Then compute GRID1 coords for GRID2 cell-centers

        TPAIN   = 0.0D0       !!  array assignment
        TPAIN(1)= RADE19
        IPR     = 0           !!  print error messages, if any
        JPR     = 1           !!  do NOT print projection parameters
        LEMSG   = INIT3()     !!  unit number for log file
        LPARM   = LEMSG       !!  projection parameters file

        INSYS  = 0          !!  geographic (=Lat-Lon)
        INZONE = 0
        INUNIT = 4          !!  input units:  degrees

        IF ( .NOT.M3TOGTPZ( GDTYP1, 200, P_ALP1, P_BET1, P_GAM1, YCENT1, SPHER1,     &
                            TPOUT, IOSYS, IOZONE, IOUNIT, INSPH ) ) THEN

            MESG = 'Lat-Lon, LAM, UTM, TRM, POL, EQM, and ALB supported'
            CALL M3MSG2( MESG )
            WRITE( MESG, '( A, I6, 2X, A, A )' ) 'Grid type', GDTYP1,'not supported'
            CALL M3MESG( MESG )
            CALL M3EXIT( PNAME, 0, 0, 'Map-projection setup error(s)', 2 )

        END IF


        !!...............  Compute transform; indices and coefficients

        DDX1 = 1.0d0 / XCELL1
        DDY1 = 1.0d0 / YCELL1
        X0 = XORIG2 - 0.5D0 * XCELL2
        Y0 = YORIG2 - 0.5D0 * YCELL2
        X1 = XORIG1 - 0.5D0 * XCELL1
        Y1 = YORIG1 - 0.5D0 * YCELL1
        X2 = DBLE( NCOLS1 )
        Y2 = DBLE( NROWS1 )

        IF ( GDTYP1 .EQ. LATGRD3 ) THEN

!$OMP        PARALLEL DO                                                        &
!$OMP&           DEFAULT( NONE ),                                               &
!$OMP&            SHARED( NROWS2, NCOLS2, X0, Y0, X1, Y1, X2, Y2, DDX1, DDY1,   &
!$OMP&                    XLOC2, YLOC2, NCOLS1, NROWS1, IX2, PX2, PY2 ),        &
!$OMP&           PRIVATE( C, R, XX, YY, J, CC, RR )

            DO  R = 1, NROWS2
            DO  C = 1, NCOLS2

                XX = DDX1 * ( XLOC2( C,R ) - X1 )
                YY = DDY1 * ( YLOC2( C,R ) - Y1 )
                XX = MAX( XX, 1.0d0 )
                YY = MAX( YY, 1.0d0 )
                CC = MIN( INT( XX ), NCOLS1-1 )
                RR = MIN( INT( YY ), NROWS1-1 )
                J  = C + NCOLS2 * ( R - 1 )
                IX2( J ) = CC + NCOLS1 * ( RR - 1 )
                PX2( J ) = SNGL( 1.0D0 - MOD( XX, 1.0D0 ) )
                PY2( J ) = SNGL( 1.0D0 - MOD( XX, 1.0D0 ) )

            END DO
            END DO          !  end traversal of input grid

        ELSE

!$OMP        PARALLEL DO                                                &
!$OMP&           DEFAULT( NONE ),                                       &
!$OMP&            SHARED( NROWS2, NCOLS2, X0, Y0, X1, Y1, X2, Y2,       &
!$OMP&                    DDX1, DDY1, XLOC2, YLOC2, NCOLS1, NROWS1,     &
!$OMP&                    IX2, PX2, PY2,                                &
!$OMP&                    INSYS, INZONE, TPAIN, INUNIT, INSPH,          &
!$OMP&                    IOSYS, IOZONE, TPOUT, IOUNIT, LPARM,          &
!$OMP&                    IPR, JPR, LEMSG, LN27, LN83, FN27, FN83 ),    &
!$OMP&           PRIVATE( C, R, XX, YY, J, CC, RR, CRDIN, CRDIO,        &
!$OMP&                    LENGTH, IFLG, MESG ),                         &
!$OMP&         REDUCTION( .OR.:  EFLAG )

            DO  R = 1, NROWS2
            DO  C = 1, NCOLS2

                CRDIN( 1 ) = XLOC2( C,R )
                CRDIN( 2 ) = YLOC2( C,R )

!$OMP           CRITICAL( S_GTPZ0 )
                CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,     &
                            IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,   &
                            TPOUT, IOUNIT, LN27, LN83, FN27, FN83,          &
                            LENGTH, IFLG )
!$OMP           END CRITICAL( S_GTPZ0 )

                IF ( IFLG .NE. 0 ) THEN
                    IFLG  = MAX( MIN( 9, IFLG ), 1 )   !  trap between 1 and 9
                    WRITE( MESG, '( A, I3, 2X, A, I5, A, I5, A )' ) &
                       'Failure:  status ', IFLG,                   &
                       'in GTPZ0 at (c,r)=(', C, ',', R, ')'
                    EFLAG = .TRUE.
                    CALL M3MESG( MESG )
                    CYCLE
                END IF

                XX = DDX1 * ( CRDIO( 1 ) - X1 )
                YY = DDY1 * ( CRDIO( 2 ) - Y1 )
                XX = MAX( XX, 1.0d0 )
                YY = MAX( YY, 1.0d0 )
                CC = MIN( INT( XX ), NCOLS1-1 )
                RR = MIN( INT( YY ), NROWS1-1 )
                J  = C + NCOLS2 * ( R - 1 )
                IX2( J ) = CC + NCOLS1 * ( RR - 1 )
                PX2( J ) = SNGL( 1.0D0 - MOD( XX, 1.0D0 ) )
                PY2( J ) = SNGL( 1.0D0 - MOD( XX, 1.0D0 ) )

            END DO
            END DO

            IF ( EFLAG ) THEN
                CALL M3EXIT( PNAME, 0, 0, 'Input-grid coord-transform error(s)', 2 )
            END IF

        END IF          !  if latgrd3, or not

        RETURN


    END SUBROUTINE GRID2INDX2


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE PNTS2INDX1( GDTYP1, P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1,      &
                           GDTYP2, P_ALP2, P_BET2, P_GAM2, XCENT2, YCENT2,      &
                           NCOLS1, NROWS1, XORIG1, YORIG1, XCELL1, YCELL1,      &
                           NPNTS2, XPNTS2, YPNTS2,                              &
                           IX2, PX2, PY2 )

        !!........  Arguments and their descriptions:

        INTEGER, INTENT(IN   ) :: GDTYP1, GDTYP2
        INTEGER, INTENT(IN   ) :: NCOLS1, NROWS1, NPNTS2
        REAL*8 , INTENT(IN   ) :: P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1
        REAL*8 , INTENT(IN   ) :: P_ALP2, P_BET2, P_GAM2, XCENT2, YCENT2
        REAL*8 , INTENT(IN   ) :: XORIG1, YORIG1, XCELL1, YCELL1
        REAL*8 , INTENT(IN   ) :: XPNTS2( NPNTS2 ),  YPNTS2( NPNTS2 )

        INTEGER, INTENT(  OUT) :: IX2( NPNTS2 )
        REAL   , INTENT(  OUT) :: PX2( NPNTS2 )
        REAL   , INTENT(  OUT) :: PY2( NPNTS2 )

        CALL PNTS2INDX2( GDTYP1, P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1, DBLE(KSPH),    &
                         GDTYP2, P_ALP2, P_BET2, P_GAM2, XCENT2, YCENT2, DBLE(KSPH),    &
                         NCOLS1, NROWS1, XORIG1, YORIG1, XCELL1, YCELL1,                &
                         NPNTS2, XPNTS2, YPNTS2,                                        &
                         IX2, PX2, PY2 )
 
        RETURN


    END SUBROUTINE PNTS2INDX1


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE PNTS2INDX2( GDTYP1, P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1, SPHER1,  &
                           GDTYP2, P_ALP2, P_BET2, P_GAM2, XCENT2, YCENT2, SPHER2,  &
                           NCOLS1, NROWS1, XORIG1, YORIG1, XCELL1, YCELL1,          &
                           NPNTS2, XPNTS2, YPNTS2,                                  &
                           IX2, PX2, PY2 )

        !!........  Arguments and their descriptions:
        !!  SPHERE[1,2] should be 0-19 for "standard" spheres
        !!  (as in SPHERENAMES above); or else Earth-radius for
        !!  non-standard perfect spheres.

        INTEGER, INTENT(IN   ) :: GDTYP1, GDTYP2
        INTEGER, INTENT(IN   ) :: NCOLS1, NROWS1, NPNTS2
        REAL*8 , INTENT(IN   ) :: SPHER1, SPHER2        !!  input, output spheres
        REAL*8 , INTENT(IN   ) :: P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1
        REAL*8 , INTENT(IN   ) :: P_ALP2, P_BET2, P_GAM2, XCENT2, YCENT2
        REAL*8 , INTENT(IN   ) :: XORIG1, YORIG1, XCELL1, YCELL1
        REAL*8 , INTENT(IN   ) :: XPNTS2( NPNTS2 ),  YPNTS2( NPNTS2 )

        INTEGER, INTENT(  OUT) :: IX2( NPNTS2 )
        REAL   , INTENT(  OUT) :: PX2( NPNTS2 )
        REAL   , INTENT(  OUT) :: PY2( NPNTS2 )

        !!........  PARAMETERs and their descriptions:

        CHARACTER*24, PARAMETER ::  PNAME = 'MODGCTP/PNTS2INDX'
        CHARACTER*16, PARAMETER ::  BLANK = ' '
        CHARACTER*80, PARAMETER ::  BAR   = &
      '-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'


        !!........  Local Variables and their descriptions:

        INTEGER     C, R, CC, RR, J, K
        LOGICAL     EFLAG
        REAL*8      X0, Y0, X1, Y1, X2, Y2, DDX1, DDY1, XX, YY

        REAL*8      XLOC2( NPNTS2 )
        REAL*8      YLOC2( NPNTS2 )

        CHARACTER*512   MESG

        !!   Arguments for GTPZ0:

        REAL*8          CRDIN( 2 )      !  input coordinates x,y
        INTEGER*4       INSYS           !  input projection code
        INTEGER*4       INZONE          !  input utm ZONE, etc.
        REAL*8          TPAIN( 15 )     !  input projection parameters
        INTEGER*4       INUNIT          !  input units code
        INTEGER*4       INSPH           !  spheroid code
        INTEGER*4       IPR             !  error print flag
        INTEGER*4       JPR             !  projection parameter print flag
        INTEGER*4       LEMSG           !  error message unit number
        INTEGER*4       LPARM           !  projection parameter unit number
        REAL*8          CRDIO( 2 )      !  output coordinates x,y
        INTEGER*4       IOSYS           !  output projection code
        INTEGER*4       IOZONE          !  output utm ZONE, etc.
        REAL*8          TPOUT( 15 )     !  output projection parameters
        INTEGER*4       IOUNIT          !  output units code
        INTEGER*4       LN27            !  NAD1927 file unit number
        INTEGER*4       LN83            !  NAD1983 file unit number
        CHARACTER*128   FN27            !  NAD1927 file name
        CHARACTER*128   FN83            !  NAD1983 file name
        INTEGER*4       LENGTH          !  NAD* record-length
        INTEGER*4       IFLG            !  error flag


        !!........  Body  ......................................................

        DDX1 = 1.0d0 / XCELL1
        DDY1 = 1.0d0 / YCELL1
        X1 = XORIG1 - 0.5D0 * XCELL1
        Y1 = YORIG1 - 0.5D0 * YCELL1
        X2 = DBLE( NCOLS1 )
        Y2 = DBLE( NROWS1 )

        IF ( SAMEPROJ2( GDTYP1, P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1, SPHER1,      &
                        GDTYP2, P_ALP2, P_BET2, P_GAM2, XCENT2, YCENT2, SPHER2 ) ) THEN

!$OMP        PARALLEL DO                                                    &
!$OMP&           DEFAULT( NONE ),                                           &
!$OMP&            SHARED( NPNTS2, X1, Y1, X2, Y2, DDX1, DDY1,               &
!$OMP&                    NCOLS1, NROWS1, XPNTS2, YPNTS2, IX2, PX2, PY2 ),  &
!$OMP&           PRIVATE( K, XX, YY, CC, RR )

            DO  K = 1, NPNTS2

                XX = DDX1 * ( XPNTS2( K ) - X1 )
                YY = DDY1 * ( YPNTS2( K ) - Y1 )
                XX = MAX( XX, 1.0d0 )
                YY = MAX( YY, 1.0d0 )
                CC = MIN( INT( XX ), NCOLS1-1 )
                RR = MIN( INT( YY ), NROWS1-1 )
                IX2( K ) = CC + NCOLS1 * ( RR - 1 )
                PX2( K ) = SNGL( 1.0D0 - MOD( XX, 1.0D0 ) )
                PY2( K ) = SNGL( 1.0D0 - MOD( XX, 1.0D0 ) )

            END DO          !  end traversal of input PNTS

            RETURN

        END IF

        EFLAG = .FALSE.

        !!...............  Next, calculate Lat-Lon for PNTS2 cell-centers
        !!...............  Set up arguments for call to GTP0:

        TPOUT   = 0.0D0
        TPOUT(1)= RADE19
        IPR     = 0              !!  print error messages, if any
        JPR     = 1              !!  do NOT print projection parameters
        LEMSG   = INIT3()        !!  unit number for log file
        LPARM   = LEMSG          !!  projection parameters file

        IOSYS  = 0              !!  geographic coords (=Lat-Lon)
        IOZONE = 0
        IOUNIT = 4              !!  output units:  degrees

        IF ( .NOT.M3TOGTPZ( GDTYP2, 100, P_ALP2, P_BET2, P_GAM2, YCENT2, SPHER2,    &
                            TPAIN, INSYS, INZONE, INUNIT, INSPH ) ) THEN
            EFLAG = .TRUE.
            WRITE( MESG, '( A, I6, 2X, A )' ) '>>> Output PNTS type', GDTYP2, 'not supported'
            CALL M3MESG( MESG )
             CALL M3EXIT( PNAME, 0, 0, 'Map-projection setup error(s)', 2 )
        END IF


        !!...............  Compute intermediate REAL*8 Lat-Lon:

        IF ( GDTYP2 .EQ. LATGRD3 ) THEN

!$OMP        PARALLEL DO  DEFAULT( NONE ),                                  &
!$OMP&                     SHARED( NPNTS2, XPNTS2, YPNTS2, XLOC2, YLOC2 ),  &
!$OMP&                    PRIVATE( K )

            DO  K = 1, NPNTS2

                XLOC2( K ) = XPNTS2( K )
                YLOC2( K ) = YPNTS2( K )

            END DO          !  end traversal of input PNTS

        ELSE

!$OMP        PARALLEL DO                                                &
!$OMP&           DEFAULT( NONE ),                                       &
!$OMP&            SHARED( NPNTS2, XPNTS2, YPNTS2, XLOC2, YLOC2,         &
!$OMP&                    INSYS, INZONE, TPAIN, INUNIT, INSPH,          &
!$OMP&                    IOSYS, IOZONE, TPOUT, IOUNIT, LPARM,          &
!$OMP&                    IPR, JPR, LEMSG, LN27, LN83, FN27, FN83 ),    &
!$OMP&           PRIVATE( C, R, XX, YY, J, CC, RR, CRDIN, CRDIO,        &
!$OMP&                    LENGTH, IFLG, MESG ),                         &
!$OMP&         REDUCTION( .OR.:  EFLAG )

            DO  K = 1, NPNTS2

                CRDIN( 1 ) = XPNTS2( K )
                CRDIN( 2 ) = YPNTS2( K )

!$OMP           CRITICAL( S_GTPZ0 )
                CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,     &
                            IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,   &
                            TPOUT, IOUNIT, LN27, LN83, FN27, FN83,          &
                            LENGTH, IFLG )
!$OMP           END CRITICAL( S_GTPZ0 )

                IF ( IFLG .NE. 0 ) THEN
                    IFLG  = MAX( MIN( 9, IFLG ), 1 )   !  trap between 1 and 9
                    WRITE( MESG, '( A, I3, 2X, A, I5, A, I5, A )' ) &
                       'Failure:  status ', IFLG,                   &
                       'in GTPZ0 at (c,r)=(', C, ',', R, ')'
                    EFLAG = .TRUE.
                    CALL M3MESG( MESG )
                    CYCLE
                END IF

                XLOC2( K ) = CRDIO( 1 )
                YLOC2( K ) = CRDIO( 2 )

            END DO          !  end traversal of input PNTS

            IF ( EFLAG ) THEN
                CALL M3EXIT( PNAME, 0, 0, 'Output-PNTS coord-transform error(s)', 2 )
            END IF

        END IF          !  if latgrd3, or not


        !!...............  Then compute PNTS1 coords for PNTS2 cell-centers

        TPAIN   = 0.0D0     !!  array assignment
        TPAIN(1)= RADE19
        IPR     = 0         !!  print error messages, if any
        JPR     = 1         !!  do NOT print projection parameters
        LEMSG   = INIT3()   !!  unit number for log file
        LPARM   = LEMSG     !!  projection parameters file

        INSYS  = 0          !!  geographic (=Lat-Lon)
        INZONE = 0
        INUNIT = 4          !!  input units:  degrees

        IF ( .NOT.M3TOGTPZ( GDTYP1, 200, P_ALP1, P_BET1, P_GAM1, YCENT1, SPHER1,     &
                            TPOUT, IOSYS, IOZONE, IOUNIT, INSPH ) ) THEN
            MESG = 'Lat-Lon, LAM, UTM, TRM, POL, EQM, and ALB supported'
            CALL M3MSG2( MESG )
            WRITE( MESG, '( A, I6, 2X, A, A )' ) 'PNTS type', GDTYP1,'not supported'
            CALL M3MESG( MESG )
            CALL M3EXIT( PNAME, 0, 0, 'Map-projection setup error(s)', 2 )
        END IF


        !!...............  Compute transform; indices and coefficients

        IF ( GDTYP1 .EQ. LATGRD3 ) THEN

!$OMP        PARALLEL DO                                                    &
!$OMP&           DEFAULT( NONE ),                                           &
!$OMP&            SHARED( NPNTS2, X1, Y1, X2, Y2, DDX1, DDY1, XLOC2, YLOC2, &
!$OMP&                    NCOLS1, NROWS1, IX2, PX2, PY2 ),                  &
!$OMP&           PRIVATE( XX, YY, K, CC, RR )

            DO  K = 1, NPNTS2

                XX = DDX1 * ( XLOC2( K ) - X1 )
                YY = DDY1 * ( YLOC2( K ) - Y1 )
                XX = MAX( XX, 1.0d0 )
                YY = MAX( YY, 1.0d0 )
                CC = MIN( INT( XX ), NCOLS1-1 )
                RR = MIN( INT( YY ), NROWS1-1 )
                IX2( K ) = CC + NCOLS1 * ( RR - 1 )
                PX2( K ) = SNGL( 1.0D0 - MOD( XX, 1.0D0 ) )
                PY2( K ) = SNGL( 1.0D0 - MOD( XX, 1.0D0 ) )

            END DO          !  end traversal of input PNTS

        ELSE

!$OMP        PARALLEL DO                                                &
!$OMP&           DEFAULT( NONE ),                                       &
!$OMP&            SHARED( NPNTS2, X1, Y1, X2, Y2, DDX1, DDY1,           &
!$OMP&                    XLOC2, YLOC2, NCOLS1, NROWS1, IX2, PX2, PY2,  &
!$OMP&                    INSYS, INZONE, TPAIN, INUNIT, INSPH,          &
!$OMP&                    IOSYS, IOZONE, TPOUT, IOUNIT, LPARM,          &
!$OMP&                    IPR, JPR, LEMSG, LN27, LN83, FN27, FN83 ),    &
!$OMP&           PRIVATE( C, R, XX, YY, J, CC, RR, CRDIN, CRDIO,        &
!$OMP&                    LENGTH, IFLG, MESG ),                         &
!$OMP&         REDUCTION( .OR.:  EFLAG )

            DO  K = 1, NPNTS2

                CRDIN( 1 ) = XLOC2( K )
                CRDIN( 2 ) = YLOC2( K )

!$OMP           CRITICAL( S_GTPZ0 )
                CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,     &
                            IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,   &
                            TPOUT, IOUNIT, LN27, LN83, FN27, FN83,          &
                            LENGTH, IFLG )
!$OMP           END CRITICAL( S_GTPZ0 )

                IF ( IFLG .NE. 0 ) THEN
                    IFLG  = MAX( MIN( 9, IFLG ), 1 )   !  trap between 1 and 9
                    WRITE( MESG, '( A, I3, 2X, A, I5, A, I5, A )' ) &
                       'Failure:  status ', IFLG,                   &
                       'in GTPZ0 at (c,r)=(', C, ',', R, ')'
                    EFLAG = .TRUE.
                    CALL M3MESG( MESG )
                    CYCLE
                END IF

                XX = DDX1 * ( CRDIO( 1 ) - X1 )
                YY = DDY1 * ( CRDIO( 2 ) - Y1 )
                XX = MAX( XX, 1.0d0 )
                YY = MAX( YY, 1.0d0 )
                CC = MIN( INT( XX ), NCOLS1-1 )
                RR = MIN( INT( YY ), NROWS1-1 )
                IX2( K ) = CC + NCOLS1 * ( RR - 1 )
                PX2( K ) = SNGL( 1.0D0 - MOD( XX, 1.0D0 ) )
                PY2( K ) = SNGL( 1.0D0 - MOD( XX, 1.0D0 ) )

            END DO

            IF ( EFLAG ) THEN
                CALL M3EXIT( PNAME, 0, 0, 'Input-PNTS coord-transform error(s)', 2 )
            END IF

        END IF          !  if latgrd3, or not

        RETURN


    END SUBROUTINE PNTS2INDX2


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE INDXMULT1( NSIZE1, NCOLS2, NROWS2,    &
                          IX1, PX1, PY1, GR1, GR2 )

        !!  Use <IX2,PX2,PY2> from GRID2INDX() or PNTS2INDX()  to map GR2 into GR1

        INTEGER, INTENT(IN   ) :: NSIZE1, NCOLS2, NROWS2
        INTEGER, INTENT(IN   ) :: IX1( NSIZE1 )
        REAL   , INTENT(IN   ) :: PX1( NSIZE1 )
        REAL   , INTENT(IN   ) :: PY1( NSIZE1 )
        REAL   , INTENT(  OUT) :: GR1( NSIZE1 )
        REAL   , INTENT(IN   ) :: GR2( NCOLS2*NROWS2 )

        INTEGER     I, ILL, ILR, IUL, IUR
        REAL        ALL, ALR, AUL, AUR, PX, PY, QX, QY

        !!........  Body  ......................................................

!$OMP    PARALLEL DO                                                        &
!$OMP&       DEFAULT( NONE ),                                               &
!$OMP&        SHARED( NSIZE1, IX1, PX1, PY1, NCOLS2, NROWS2, GR1, GR2 ),    &
!$OMP&       PRIVATE( I, ILL, ILR, IUL, IUR, PX, QX, PY, QY,                &
!$OMP&                ALL, ALR, AUL, AUR )

        DO I = 1, NSIZE1
            ILL = IX1( I )
            IF ( ILL .LT. 0 ) THEN
                GR1( I ) = BADVAL3
                CYCLE
            END IF
            ILR = ILL + 1
            IUL = ILL + NCOLS2
            IUR = ILL + NCOLS2 + 1

            PX = PX1( I )
            QX = 1.0 - PX
            PY = PY1( I )
            QY = 1.0 - PY

            ALL = PX * PY
            ALR = QX * PY
            AUL = PX * QY
            AUR = QX * QY

            GR1( I ) = ALL * GR2(ILL) + ALR * GR2(ILR) + AUL * GR2(IUL) + AUR * GR2(IUR)
        END DO

        RETURN

    END SUBROUTINE INDXMULT1


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE INDXMULT2( NSIZE1, NLAYS, NCOLS2, NROWS2,    &
                          IX1, PX1, PY1, GR1, GR2 )

        !!  Use <IX,PX,PY> from GRID2INDX() or PNTS2INDX()  to map GR2 into GR1

        INTEGER, INTENT(IN   ) :: NSIZE1, NLAYS, NCOLS2, NROWS2
        INTEGER, INTENT(IN   ) :: IX1( NSIZE1 )
        REAL   , INTENT(IN   ) :: PX1( NSIZE1 )
        REAL   , INTENT(IN   ) :: PY1( NSIZE1 )
        REAL   , INTENT(  OUT) :: GR1( NSIZE1,NLAYS )
        REAL   , INTENT(IN   ) :: GR2( NCOLS2*NROWS2,NLAYS )

        INTEGER     I, L, ILL, ILR, IUL, IUR
        REAL        ALL, ALR, AUL, AUR, PX, PY, QX, QY

        !!........  Body  ......................................................

        IF ( NLAYS .EQ. 1 ) THEN

!$OMP        PARALLEL DO                                                &
!$OMP&           DEFAULT( NONE ),                                       &
!$OMP&            SHARED( NLAYS, NSIZE1, IX1, PX1, PY1,                 &
!$OMP&                    NCOLS2, NROWS2, GR1, GR2 ),                   &
!$OMP&           PRIVATE( I, ILL, ILR, IUL, IUR, PX, QX, PY, QY,        &
!$OMP&                    ALL, ALR, AUL, AUR )

            DO I = 1, NSIZE1
                ILL = IX1( I )
                IF ( ILL .LT. 0 ) THEN
                    GR1( I,1 ) = BADVAL3
                    CYCLE
                END IF
                ILR = ILL + 1
                IUL = ILL + NCOLS2
                IUR = ILL + NCOLS2 + 1

                PX = PX1( I )
                QX = 1.0 - PX
                PY = PY1( I )
                QY = 1.0 - PY

                ALL = PX * PY
                ALR = QX * PY
                AUL = PX * QY
                AUR = QX * QY

                GR1( I,1 ) = ALL * GR2(ILL,1) + ALR * GR2(ILR,1) + AUL * GR2(IUL,1) + AUR * GR2(IUR,1)
            END DO

        ELSE

!$OMP        PARALLEL DO                                                &
!$OMP&           DEFAULT( NONE ),                                       &
!$OMP&            SHARED( NLAYS, NSIZE1, IX1, PX1, PY1,                 &
!$OMP&                    NCOLS2, NROWS2, GR1, GR2 ),                   &
!$OMP&           PRIVATE( L, I, ILL, ILR, IUL, IUR, PX, QX, PY, QY,     &
!$OMP&                    ALL, ALR, AUL, AUR )

            DO L = 1, NLAYS
            DO I = 1, NSIZE1
                ILL = IX1( I )
                IF ( ILL .LT. 0 ) THEN
                    GR1( I,L ) = BADVAL3
                    CYCLE
                END IF
                ILR = ILL + 1
                IUL = ILL + NCOLS2
                IUR = ILL + NCOLS2 + 1

                PX = PX1( I )
                QX = 1.0 - PX
                PY = PY1( I )
                QY = 1.0 - PY

                ALL = PX * PY
                ALR = QX * PY
                AUL = PX * QY
                AUR = QX * QY

                GR1( I,L ) = ALL * GR2(ILL,L) + ALR * GR2(ILR,L) + AUL * GR2(IUL,L) + AUR * GR2(IUR,L)
            END DO
            END DO

        END IF

        RETURN

    END SUBROUTINE INDXMULT2


!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
!!  Sadly, this doesn't work:  F90 can't distinguish between
!!  horizontally-single-indexed ( NCOLS2*NROWS2,NLAYS ) 3D arrays and
!!  doubly-indexed ( NCOLS2,NROWS2 ) 2D arrays.
!!
!!     SUBROUTINE INDXMULT3( NCOLS1, NROWS1, NCOLS2, NROWS2,    &
!!                           IX1, PX1, PY1, GR1, GR2 )
!!
!!         !!  Use <IX,PX,PY> from GRID2INDX() or PNTS2INDX()  to map GR2 into GR1
!!
!!         INTEGER, INTENT(IN   ) :: NCOLS1, NROWS1, NCOLS2, NROWS2
!!         INTEGER, INTENT(IN   ) :: IX1( NCOLS1*NROWS1 )
!!         REAL   , INTENT(IN   ) :: PX1( NCOLS1*NROWS1 )
!!         REAL   , INTENT(IN   ) :: PY1( NCOLS1*NROWS1 )
!!         REAL   , INTENT(  OUT) :: GR1( NCOLS1,NROWS1 )
!!         REAL   , INTENT(IN   ) :: GR2( NCOLS2,NROWS2 )
!!
!!         INTEGER     C, R, I, CC, RR
!!         REAL        ALL, ALR, AUL, AUR, PX, PY, QX, QY
!!
!! !$OMP        PARALLEL DO                                                &
!! !$OMP&           DEFAULT( NONE ),                                       &
!! !$OMP&            SHARED( NLAYS, NCOLS1, NROWS1, NCOLS2, NROWS2,        &
!! !$OMP&                    IX1, PX1, PY1, GR1, GR2 ),                    &
!! !$OMP&           PRIVATE( L, R, C, I, RR, CC, PX, QX, PY, QY,           &
!! !$OMP&                    ALL, ALR, AUL, AUR )
!!
!!         DO R = 1, NROWS1
!!         DO C = 1, NCOLS1
!!             I  = IX1( C + ( R - 1) * NCOLS1 ) - 1
!!             CC = 1 + MOD( I , NCOLS2 )
!!             RR = 1 +      I / NCOLS2
!!
!!             PX = PX1( I )
!!             QX = 1.0 - PX
!!             PY = PY1( I )
!!             QY = 1.0 - PY
!!
!!             ALL = PX * PY
!!             ALR = QX * PY
!!             AUL = PX * QY
!!             AUR = QX * QY
!!
!!             GR1( C,R ) = ALL * GR2(CC,RR  ) + ALR * GR2(CC+1,RR  )  +   &
!!                          AUL * GR2(CC,RR+1) + AUR * GR2(CC+1,RR+1)
!!         END DO
!!         END DO
!!
!!         RETURN
!!
!!     END SUBROUTINE INDXMULT3


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE INDXMULT4( NCOLS1, NROWS1, NLAYS, NCOLS2, NROWS2,    &
                          IX1, PX1, PY1, GR1, GR2 )

        !!  Use <IX,PX,PY> from GRID2INDX() or PNTS2INDX()  to map GR2 into GR1

        INTEGER, INTENT(IN   ) :: NCOLS1, NROWS1, NLAYS, NCOLS2, NROWS2
        INTEGER, INTENT(IN   ) :: IX1( NCOLS1*NROWS1 )
        REAL   , INTENT(IN   ) :: PX1( NCOLS1*NROWS1 )
        REAL   , INTENT(IN   ) :: PY1( NCOLS1*NROWS1 )
        REAL   , INTENT(  OUT) :: GR1( NCOLS1,NROWS1,NLAYS )
        REAL   , INTENT(IN   ) :: GR2( NCOLS2,NROWS2,NLAYS )

        INTEGER     C, R, I, K, L, CC, RR
        REAL        ALL, ALR, AUL, AUR, PX, PY, QX, QY

    IF ( NLAYS .EQ. 1 ) THEN

!$OMP        PARALLEL DO                                                &
!$OMP&           DEFAULT( NONE ),                                       &
!$OMP&            SHARED( NLAYS, NCOLS1, NROWS1, NCOLS2, NROWS2,        &
!$OMP&                    IX1, PX1, PY1, GR1, GR2 ),                    &
!$OMP&           PRIVATE( R, C, I, K, RR, CC, PX, QX, PY, QY,           &
!$OMP&                    ALL, ALR, AUL, AUR )

            DO R = 1, NROWS1
            DO C = 1, NCOLS1

                K  = C + ( R - 1) * NCOLS1
                I  = IX1( K ) - 1
                IF ( I .LT. 0 ) THEN
                    GR1( C,R,1 ) = BADVAL3
                    CYCLE
                END IF

                CC = 1 + MOD( I , NCOLS2 )
                RR = 1 +      I / NCOLS2

                PX = PX1( I )
                QX = 1.0 - PX
                PY = PY1( I )
                QY = 1.0 - PY

                ALL = PX * PY
                ALR = QX * PY
                AUL = PX * QY
                AUR = QX * QY

                GR1( C,R,1 ) = ALL * GR2(CC,RR  ,1) + ALR * GR2(CC+1,RR  ,1)  +   &
                               AUL * GR2(CC,RR+1,1) + AUR * GR2(CC+1,RR+1,1)
            END DO
            END DO

    ELSE

!$OMP        PARALLEL DO                                                &
!$OMP&           DEFAULT( NONE ),                                       &
!$OMP&            SHARED( NLAYS, NCOLS1, NROWS1, NCOLS2, NROWS2,        &
!$OMP&                    IX1, PX1, PY1, GR1, GR2 ),                    &
!$OMP&           PRIVATE( L, R, C, I, K, RR, CC, PX, QX, PY, QY,        &
!$OMP&                    ALL, ALR, AUL, AUR )

            DO L = 1, NLAYS
            DO R = 1, NROWS1
            DO C = 1, NCOLS1

                K  = C + ( R - 1) * NCOLS1
                IF ( IX1( K ) .LT. 0 ) THEN
                    GR1( C,R,L ) = BADVAL3
                    CYCLE
                END IF

                I  = IX1( K ) - 1
                CC = 1 + MOD( I , NCOLS2 )
                RR = 1 +      I / NCOLS2

                PX = PX1( I )
                QX = 1.0 - PX
                PY = PY1( I )
                QY = 1.0 - PY

                ALL = PX * PY
                ALR = QX * PY
                AUL = PX * QY
                AUR = QX * QY

                GR1( C,R,L ) = ALL * GR2(CC,RR  ,L) + ALR * GR2(CC+1,RR  ,L)  +   &
                               AUL * GR2(CC,RR+1,L) + AUR * GR2(CC+1,RR+1,L)
            END DO
            END DO
            END DO

        END IF      !! if nlays==1, or nolt

        RETURN

    END SUBROUTINE INDXMULT4


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE XY2XY0D1( GDTYP1, P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1,     &
                         GDTYP2, P_ALP2, P_BET2, P_GAM2, XCENT2, YCENT2,     &
                         XLOC2, YLOC2, XLOC1, YLOC1 )

        !!........  Arguments and their descriptions:

        INTEGER, INTENT(IN   ) :: GDTYP1, GDTYP2
        REAL*8 , INTENT(IN   ) :: P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1
        REAL*8 , INTENT(IN   ) :: P_ALP2, P_BET2, P_GAM2, XCENT2, YCENT2
        REAL*8 , INTENT(IN   ) :: XLOC2
        REAL*8 , INTENT(IN   ) :: YLOC2
        REAL*8 , INTENT(  OUT) :: XLOC1
        REAL*8 , INTENT(  OUT) :: YLOC1

        CALL XY2XY0D2( GDTYP1, P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1, DBLE(KSPH),  &
                       GDTYP2, P_ALP2, P_BET2, P_GAM2, XCENT2, YCENT2, DBLE(KSPH),  &
                       XLOC2, YLOC2, XLOC1, YLOC1 )
 
        RETURN


    END SUBROUTINE XY2XY0D1


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE XY2XY0D2( GDTYP1, P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1, SPHER1,     &
                         GDTYP2, P_ALP2, P_BET2, P_GAM2, XCENT2, YCENT2, SPHER2,     &
                         XLOC2, YLOC2, XLOC1, YLOC1 )

        !!........  Arguments and their descriptions:

        INTEGER, INTENT(IN   ) :: GDTYP1, GDTYP2
        REAL*8 , INTENT(IN   ) :: P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1, SPHER1
        REAL*8 , INTENT(IN   ) :: P_ALP2, P_BET2, P_GAM2, XCENT2, YCENT2, SPHER2
        REAL*8 , INTENT(IN   ) :: XLOC2
        REAL*8 , INTENT(IN   ) :: YLOC2
        REAL*8 , INTENT(  OUT) :: XLOC1
        REAL*8 , INTENT(  OUT) :: YLOC1

        !!........  PARAMETERs and their descriptions:

        CHARACTER*24, PARAMETER ::  PNAME = 'MODGCTP/XY2XY0D2'
        CHARACTER*16, PARAMETER ::  BLANK = ' '
        CHARACTER*80, PARAMETER ::  BAR   = &
      '-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'


        !!........  Local Variables and their descriptions:

        LOGICAL     EFLAG

        CHARACTER*512   MESG

        !!   Arguments for GTPZ0:

        REAL*8          CRDIN( 2 )      !  input coordinates x,y
        INTEGER*4       INSYS           !  input projection code
        INTEGER*4       INZONE          !  input utm ZONE, etc.
        REAL*8          TPAIN( 15 )     !  input projection parameters
        INTEGER*4       INUNIT          !  input units code
        INTEGER*4       INSPH           !  spheroid code
        INTEGER*4       IPR             !  error print flag
        INTEGER*4       JPR             !  projection parameter print flag
        INTEGER*4       LEMSG           !  error message unit number
        INTEGER*4       LPARM           !  projection parameter unit number
        REAL*8          CRDIO( 2 )      !  output coordinates x,y
        INTEGER*4       IOSYS           !  output projection code
        INTEGER*4       IOZONE          !  output utm ZONE, etc.
        REAL*8          TPOUT( 15 )     !  output projection parameters
        INTEGER*4       IOUNIT          !  output units code
        INTEGER*4       LN27            !  NAD1927 file unit number
        INTEGER*4       LN83            !  NAD1983 file unit number
        CHARACTER*128   FN27            !  NAD1927 file name
        CHARACTER*128   FN83            !  NAD1983 file name
        INTEGER*4       LENGTH          !  NAD* record-length
        INTEGER*4       IFLG            !  error flag


        !!........  Body  ......................................................

        EFLAG = .FALSE.

        !!...............  Calculate Lat-Lon:
        !!...............  Set up arguments for call to GTP0:

        IF ( XLOC2 .LT. AMISSD .OR. YLOC2 .LT. AMISSD ) THEN

            XLOC1 = BADVALD
            YLOC1 = BADVALD
            RETURN

        ELSE IF ( SAMEPROJ2( GDTYP1, P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1, SPHER1,     &
                             GDTYP2, P_ALP2, P_BET2, P_GAM2, XCENT2, YCENT2, SPHER2 ) ) THEN

            XLOC1 = XLOC2
            YLOC1 = YLOC2
            RETURN

        END IF

        TPOUT = 0.0D0
        IPR    = 0              !!  print error messages, if any
        JPR    = 1              !!  do NOT print projection parameters
        LEMSG  = INIT3()        !!  unit number for log file
        LPARM  = LEMSG          !!  projection parameters file

        IOSYS  = 0              !!  geographic coords (=Lat-Lon)
        IOZONE = 0
        IOUNIT = 4              !!  output units:  degrees

        IF ( .NOT.M3TOGTPZ( GDTYP2, 100, P_ALP2, P_BET2, P_GAM2, YCENT2,    &
                            TPAIN, INSYS, INZONE, INUNIT, INSPH ) ) THEN
            EFLAG = .TRUE.
            MESG = 'Lat-Lon, LAM, UTM, TRM, POL, EQM, and ALB supported'
            CALL M3MSG2( MESG )
            WRITE( MESG, '( A, I6, 2X, A )' ) '>>> Output grid type', GDTYP2, 'not supported'
            CALL M3MESG( MESG )
        END IF

        !!...............  Sphere adjustment:

        INSPH = NINT( SPHER2 )

        IF ( SPHER2 .GT. 21.5d0 ) THEN
            INSPH    = -1
            TPAIN(1) = SPHER2
        ELSE IF ( SPHER2 .LT. -0.05d0 ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'Illegal sphere:  SPHER2 < 0' )
        ELSE IF ( DBLERR( SPHER2, DBLE( INSPH ) ) ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'Illegal sphere:  non-integer SPHER2 ' )
        END IF


        IF ( EFLAG ) THEN
            CALL M3EXIT( PNAME, 0, 0, 'Map-projection setup error(s)', 2 )
        END IF


        !!...............  Compute transform:

        IF ( GDTYP2 .EQ. LATGRD3 ) THEN

            XLOC1 = XLOC2
            YLOC1 = YLOC2

        ELSE

            CRDIN( 1 ) = XLOC2
            CRDIN( 2 ) = YLOC2

            CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,     &
                        IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,   &
                        TPOUT, IOUNIT, LN27, LN83, FN27, FN83,          &
                        LENGTH, IFLG )

            IF ( IFLG .NE. 0 ) THEN
                IFLG  = MAX( MIN( 9, IFLG ), 1 )   !  trap between 1 and 9
                WRITE( MESG, '( A, I3, 2X, A )' )   &
                   'Failure:  status ', IFLG, 'in GTPZ0'
                EFLAG = .TRUE.
                CALL M3MESG( MESG )
            END IF

            XLOC1 = CRDIO( 1 )
            YLOC1 = CRDIO( 2 )

            IF ( EFLAG ) THEN
                CALL M3EXIT( PNAME, 0, 0, 'GRID2::LATLON coord-transform error(s)', 2 )
            END IF

        END IF          !  if sameproj, or not


        !!...............  Then compute GRID1 coords

        TPAIN = 0.0D0           !  array assignment
        IPR   = 0       !  print error messages, if any
        JPR   = 1       !  do NOT print projection parameters
        LEMSG = INIT3() !  unit number for log file
        LPARM = LEMSG   !  projection parameters file

        INSYS  = 0           !!  geographic (=Lat-Lon)
        INZONE = 0
        INUNIT = 4           !!  input units:  degrees

        IF ( .NOT.M3TOGTPZ( GDTYP1, 200, P_ALP1, P_BET1, P_GAM1, YCENT1,    &
                            TPOUT, IOSYS, IOZONE, IOUNIT, INSPH ) ) THEN
            EFLAG = .TRUE.
            MESG  = 'Lat-Lon, LAM, UTM, TRM, POL, EQM, and ALB supported'
            CALL M3MSG2( MESG )
            WRITE( MESG, '( A, I6, 2X, A )' ) '>>> Output grid type', GDTYP1, 'not supported'
            CALL M3MESG( MESG )
        END IF

        INSPH = NINT( SPHER1 )

        IF ( SPHER1 .GT. 21.5d0 ) THEN
            INSPH    = -1
            TPAIN(1) = SPHER2
        ELSE IF ( SPHER1 .LT. -0.05d0 ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'Illegal sphere:  SPHER2 < 0' )
        ELSE IF ( DBLERR( SPHER1, DBLE( INSPH ) ) ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'Illegal sphere:  non-integer SPHER1 ' )
        END IF

        IF ( GDTYP1 .EQ. LATGRD3 ) THEN

            CONTINUE

        ELSE

            CRDIN( 1 ) = XLOC1
            CRDIN( 2 ) = YLOC1

            CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,     &
                        IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,   &
                        TPOUT, IOUNIT, LN27, LN83, FN27, FN83,          &
                        LENGTH, IFLG )

            IF ( IFLG .NE. 0 ) THEN
                IFLG  = MAX( MIN( 9, IFLG ), 1 )   !  trap between 1 and 9
                WRITE( MESG, '( A, I3, 2X, A )' )   &
                   'Failure:  status ', IFLG, 'in GTPZ0'
                EFLAG = .TRUE.
                CALL M3MESG( MESG )
            END IF

            XLOC1 = CRDIO( 1 )
            YLOC1 = CRDIO( 2 )

            IF ( EFLAG ) THEN
                CALL M3EXIT( PNAME, 0, 0, 'LATLON::GRID1 coord-transform error(s)', 2 )
            END IF

        END IF          !  if latgrd3, or not

        RETURN


    END SUBROUTINE XY2XY0D2


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE XY2XY1D1( GDTYP1, P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1,     &
                         GDTYP2, P_ALP2, P_BET2, P_GAM2, XCENT2, YCENT2,     &
                         NPTS, XLOC2, YLOC2, XLOC1, YLOC1 )

        !!........  Arguments and their descriptions:

        INTEGER, INTENT(IN   ) :: GDTYP1, GDTYP2, NPTS
        REAL*8 , INTENT(IN   ) :: P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1
        REAL*8 , INTENT(IN   ) :: P_ALP2, P_BET2, P_GAM2, XCENT2, YCENT2
        REAL*8 , INTENT(IN   ) :: XLOC2( NPTS )
        REAL*8 , INTENT(IN   ) :: YLOC2( NPTS )
        REAL*8 , INTENT(  OUT) :: XLOC1( NPTS )
        REAL*8 , INTENT(  OUT) :: YLOC1( NPTS )

        CALL XY2XY1D2( GDTYP1, P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1, DBLE(KSPH),  &
                       GDTYP2, P_ALP2, P_BET2, P_GAM2, XCENT2, YCENT2, DBLE(KSPH),  &
                       NPTS, XLOC2, YLOC2, XLOC1, YLOC1 )

        RETURN


    END SUBROUTINE XY2XY1D1


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE XY2XY1D2( GDTYP1, P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1, SPHER1,     &
                         GDTYP2, P_ALP2, P_BET2, P_GAM2, XCENT2, YCENT2, SPHER2,     &
                         NPTS, XLOC2, YLOC2, XLOC1, YLOC1 )

        !!........  Arguments and their descriptions:

        INTEGER, INTENT(IN   ) :: GDTYP1, GDTYP2, NPTS
        REAL*8 , INTENT(IN   ) :: P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1, SPHER1
        REAL*8 , INTENT(IN   ) :: P_ALP2, P_BET2, P_GAM2, XCENT2, YCENT2, SPHER2
        REAL*8 , INTENT(IN   ) :: XLOC2( NPTS )
        REAL*8 , INTENT(IN   ) :: YLOC2( NPTS )
        REAL*8 , INTENT(  OUT) :: XLOC1( NPTS )
        REAL*8 , INTENT(  OUT) :: YLOC1( NPTS )

        !!........  PARAMETERs and their descriptions:

        CHARACTER*24, PARAMETER ::  PNAME = 'MODGCTP/XY2XY1D2'
        CHARACTER*16, PARAMETER ::  BLANK = ' '
        CHARACTER*80, PARAMETER ::  BAR   = &
      '-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'


        !!........  Local Variables and their descriptions:

        INTEGER     K
        LOGICAL     EFLAG

        CHARACTER*512   MESG

        !!   Arguments for GTPZ0:

        REAL*8          CRDIN( 2 )      !  input coordinates x,y
        INTEGER*4       INSYS           !  input projection code
        INTEGER*4       INZONE          !  input utm ZONE, etc.
        REAL*8          TPAIN( 15 )     !  input projection parameters
        INTEGER*4       INUNIT          !  input units code
        INTEGER*4       INSPH           !  spheroid code
        INTEGER*4       IPR             !  error print flag
        INTEGER*4       JPR             !  projection parameter print flag
        INTEGER*4       LEMSG           !  error message unit number
        INTEGER*4       LPARM           !  projection parameter unit number
        REAL*8          CRDIO( 2 )      !  output coordinates x,y
        INTEGER*4       IOSYS           !  output projection code
        INTEGER*4       IOZONE          !  output utm ZONE, etc.
        REAL*8          TPOUT( 15 )     !  output projection parameters
        INTEGER*4       IOUNIT          !  output units code
        INTEGER*4       LN27            !  NAD1927 file unit number
        INTEGER*4       LN83            !  NAD1983 file unit number
        CHARACTER*128   FN27            !  NAD1927 file name
        CHARACTER*128   FN83            !  NAD1983 file name
        INTEGER*4       LENGTH          !  NAD* record-length
        INTEGER*4       IFLG            !  error flag


        !!........  Body  ......................................................

        EFLAG = .FALSE.

        !!...............  Calculate Lat-Lon:
        !!...............  Set up arguments for call to GTP0:

        IF ( SAMEPROJ2( GDTYP1, P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1, SPHER1,      &
                        GDTYP2, P_ALP2, P_BET2, P_GAM2, XCENT2, YCENT2, SPHER2 ) ) THEN

!$OMP        PARALLEL DO                                            &
!$OMP&           DEFAULT( NONE ),                                   &
!$OMP&            SHARED( NPTS, XLOC1, YLOC1, XLOC2, YLOC2 ),       &
!$OMP&           PRIVATE( K )

            DO  K = 1, NPTS

                XLOC1( K ) = XLOC2( K )
                YLOC1( K ) = YLOC2( K )

            END DO

            RETURN

        END IF

        TPOUT = 0.0D0
        IPR    = 0              !!  print error messages, if any
        JPR    = 1              !!  do NOT print projection parameters
        LEMSG  = INIT3()        !!  unit number for log file
        LPARM  = LEMSG          !!  projection parameters file

        IOSYS  = 0              !!  geographic coords (=Lat-Lon)
        IOZONE = 0
        IOUNIT = 4              !!  output units:  degrees

        IF ( .NOT.M3TOGTPZ( GDTYP2, 100, P_ALP2, P_BET2, P_GAM2, YCENT2,    &
                            TPAIN, INSYS, INZONE, INUNIT, INSPH ) ) THEN
            EFLAG = .TRUE.
            MESG = 'Lat-Lon, LAM, UTM, TRM, POL, EQM, and ALB supported'
            CALL M3MSG2( MESG )
            WRITE( MESG, '( A, I6, 2X, A )' ) '>>> Output grid type', GDTYP2, 'not supported'
            CALL M3MESG( MESG )
        END IF

        !!...............  Sphere adjustment:

        INSPH = NINT( SPHER2 )

        IF ( SPHER2 .GT. 21.5d0 ) THEN
            INSPH    = -1
            TPAIN(1) = SPHER2
        ELSE IF ( SPHER2 .LT. -0.05d0 ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'Illegal sphere:  SPHER2 < 0' )
        ELSE IF ( DBLERR( SPHER2, DBLE( INSPH ) ) ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'Illegal sphere:  non-integer SPHER2 ' )
        END IF


        IF ( EFLAG ) THEN
            CALL M3EXIT( PNAME, 0, 0, 'Map-projection setup error(s)', 2 )
        END IF


        !!...............  Compute transform:

        IF ( GDTYP2 .EQ. LATGRD3 ) THEN

!$OMP        PARALLEL DO                                            &
!$OMP&           DEFAULT( NONE ),                                   &
!$OMP&            SHARED( NPTS, XLOC1, YLOC1, XLOC2, YLOC2 ),       &
!$OMP&           PRIVATE( K )

            DO  K = 1, NPTS

                XLOC1( K ) = XLOC2( K )
                YLOC1( K ) = YLOC2( K )

            END DO

        ELSE

!$OMP        PARALLEL DO                                                &
!$OMP&           DEFAULT( NONE ),                                       &
!$OMP&            SHARED( NPTS, XLOC1, YLOC1, XLOC2, YLOC2,             &
!$OMP&                    INSYS, INZONE, TPAIN, INUNIT, INSPH,          &
!$OMP&                    IOSYS, IOZONE, TPOUT, IOUNIT, LPARM,          &
!$OMP&                    IPR, JPR, LEMSG, LN27, LN83, FN27, FN83 ),    &
!$OMP&           PRIVATE( K, CRDIN, CRDIO, LENGTH, IFLG, MESG ),        &
!$OMP&         REDUCTION( .OR.:  EFLAG )

            DO K = 1, NPTS

                CRDIN( 1 ) = XLOC2( K )
                CRDIN( 2 ) = YLOC2( K )

                IF ( CRDIN( 1 ) .LT. AMISSD .OR. CRDIN( 2 ) .LT. AMISSD ) THEN
                    XLOC1( K ) = BADVALD
                    YLOC1( K ) = BADVALD
                    CYCLE
                END IF

!$OMP           CRITICAL( S_GTPZ0 )
                CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,     &
                            IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,   &
                            TPOUT, IOUNIT, LN27, LN83, FN27, FN83,          &
                            LENGTH, IFLG )
!$OMP           END CRITICAL( S_GTPZ0 )

                IF ( IFLG .NE. 0 ) THEN
                    IFLG  = MAX( MIN( 9, IFLG ), 1 )   !  trap between 1 and 9
                    WRITE( MESG, '( A, I3, 2X, A, I4 )' )   &
                       'Failure:  status ', IFLG, 'in GTPZ0 at K=', K
                    EFLAG = .TRUE.
                    CALL M3MESG( MESG )
                END IF

                XLOC1( K ) = CRDIO( 1 )
                YLOC1( K ) = CRDIO( 2 )

            END DO

            IF ( EFLAG ) THEN
                CALL M3EXIT( PNAME, 0, 0, 'GRID2::LATLON coord-transform error(s)', 2 )
            END IF

        END IF          !  if sameproj, or not


        !!...............  Then compute GRID1 coords

        TPAIN = 0.0D0           !  array assignment
        IPR   = 0       !  print error messages, if any
        JPR   = 1       !  do NOT print projection parameters
        LEMSG = INIT3() !  unit number for log file
        LPARM = LEMSG   !  projection parameters file

        INSYS  = 0           !!  geographic (=Lat-Lon)
        INZONE = 0
        INUNIT = 4           !!  input units:  degrees

        IF ( .NOT.M3TOGTPZ( GDTYP1, 200, P_ALP1, P_BET1, P_GAM1, YCENT1,    &
                            TPOUT, IOSYS, IOZONE, IOUNIT, INSPH ) ) THEN
            EFLAG = .TRUE.
            MESG  = 'Lat-Lon, LAM, UTM, TRM, POL, EQM, and ALB supported'
            CALL M3MSG2( MESG )
            WRITE( MESG, '( A, I6, 2X, A )' ) '>>> Output grid type', GDTYP1, 'not supported'
            CALL M3MESG( MESG )
        END IF

        INSPH = NINT( SPHER1 )

        IF ( SPHER1 .GT. 21.5d0 ) THEN
            INSPH    = -1
            TPAIN(1) = SPHER2
        ELSE IF ( SPHER1 .LT. -0.05d0 ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'Illegal sphere:  SPHER2 < 0' )
        ELSE IF ( DBLERR( SPHER1, DBLE( INSPH ) ) ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'Illegal sphere:  non-integer SPHER1 ' )
        END IF

        IF ( GDTYP1 .EQ. LATGRD3 ) THEN

            CONTINUE

        ELSE

!$OMP        PARALLEL DO                                                &
!$OMP&           DEFAULT( NONE ),                                       &
!$OMP&            SHARED( NPTS, XLOC1, YLOC1, XLOC2, YLOC2,             &
!$OMP&                    INSYS, INZONE, TPAIN, INUNIT, INSPH,          &
!$OMP&                    IOSYS, IOZONE, TPOUT, IOUNIT, LPARM,          &
!$OMP&                    IPR, JPR, LEMSG, LN27, LN83, FN27, FN83 ),    &
!$OMP&           PRIVATE( K, CRDIN, CRDIO, LENGTH, IFLG, MESG  ),       &
!$OMP&         REDUCTION( .OR.:  EFLAG )

            DO  K = 1, NPTS

                CRDIN( 1 ) = XLOC1( K )
                CRDIN( 2 ) = YLOC1( K )

                 IF ( CRDIN( 1 ) .LT. AMISSD .OR. CRDIN( 2 ) .LT. AMISSD ) THEN
                     XLOC1( K ) = BADVALD
                     YLOC1( K ) = BADVALD
                     CYCLE
                 END IF

!$OMP           CRITICAL( S_GTPZ0 )
                CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,     &
                            IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,   &
                            TPOUT, IOUNIT, LN27, LN83, FN27, FN83,          &
                            LENGTH, IFLG )
!$OMP           END CRITICAL( S_GTPZ0 )

                IF ( IFLG .NE. 0 ) THEN
                    IFLG  = MAX( MIN( 9, IFLG ), 1 )   !  trap between 1 and 9
                    WRITE( MESG, '( A, I3, 2X, A, I4 )' )   &
                       'Failure:  status ', IFLG, 'in GTPZ0 at K=', K
                    EFLAG = .TRUE.
                    CALL M3MESG( MESG )
                END IF

                XLOC1( K ) = CRDIO( 1 )
                YLOC1( K ) = CRDIO( 2 )

            END DO

            IF ( EFLAG ) THEN
                CALL M3EXIT( PNAME, 0, 0, 'LATLON::GRID1 coord-transform error(s)', 2 )
            END IF

        END IF          !  if latgrd3, or not

        RETURN


    END SUBROUTINE XY2XY1D2


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE XY2XY2D1( GDTYP1, P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1,     &
                         GDTYP2, P_ALP2, P_BET2, P_GAM2, XCENT2, YCENT2,     &
                         NCOLS, NROWS, XLOC2, YLOC2, XLOC1, YLOC1 )

        !!........  Arguments and their descriptions:

        INTEGER, INTENT(IN   ) :: GDTYP1, GDTYP2, NCOLS, NROWS
        REAL*8 , INTENT(IN   ) :: P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1
        REAL*8 , INTENT(IN   ) :: P_ALP2, P_BET2, P_GAM2, XCENT2, YCENT2
        REAL*8 , INTENT(IN   ) :: XLOC2( NCOLS,NROWS )
        REAL*8 , INTENT(IN   ) :: YLOC2( NCOLS,NROWS )
        REAL*8 , INTENT(  OUT) :: XLOC1( NCOLS,NROWS )
        REAL*8 , INTENT(  OUT) :: YLOC1( NCOLS,NROWS )

        CALL XY2XY2D2( GDTYP1, P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1, DBLE(KSPH),  &
                       GDTYP2, P_ALP2, P_BET2, P_GAM2, XCENT2, YCENT2, DBLE(KSPH),  &
                       NCOLS, NROWS, XLOC2, YLOC2, XLOC1, YLOC1 )

        RETURN


    END SUBROUTINE XY2XY2D1


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE XY2XY2D2( GDTYP1, P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1, SPHER1,     &
                         GDTYP2, P_ALP2, P_BET2, P_GAM2, XCENT2, YCENT2, SPHER2,     &
                         NCOLS, NROWS, XLOC2, YLOC2, XLOC1, YLOC1 )

        !!........  Arguments and their descriptions:

        INTEGER, INTENT(IN   ) :: GDTYP1, GDTYP2, NCOLS, NROWS
        REAL*8 , INTENT(IN   ) :: P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1, SPHER1
        REAL*8 , INTENT(IN   ) :: P_ALP2, P_BET2, P_GAM2, XCENT2, YCENT2, SPHER2
        REAL*8 , INTENT(IN   ) :: XLOC2( NCOLS,NROWS )
        REAL*8 , INTENT(IN   ) :: YLOC2( NCOLS,NROWS )
        REAL*8 , INTENT(  OUT) :: XLOC1( NCOLS,NROWS )
        REAL*8 , INTENT(  OUT) :: YLOC1( NCOLS,NROWS )

        !!........  PARAMETERs and their descriptions:

        CHARACTER*24, PARAMETER ::  PNAME = 'MODGCTP/XY2XY2D2'
        CHARACTER*16, PARAMETER ::  BLANK = ' '
        CHARACTER*80, PARAMETER ::  BAR   = &
      '-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'


        !!........  Local Variables and their descriptions:

        INTEGER     C, R
        LOGICAL     EFLAG

        CHARACTER*512   MESG

        !!   Arguments for GTPZ0:

        REAL*8          CRDIN( 2 )      !  input coordinates x,y
        INTEGER*4       INSYS           !  input projection code
        INTEGER*4       INZONE          !  input utm ZONE, etc.
        REAL*8          TPAIN( 15 )     !  input projection parameters
        INTEGER*4       INUNIT          !  input units code
        INTEGER*4       INSPH           !  spheroid code
        INTEGER*4       IPR             !  error print flag
        INTEGER*4       JPR             !  projection parameter print flag
        INTEGER*4       LEMSG           !  error message unit number
        INTEGER*4       LPARM           !  projection parameter unit number
        REAL*8          CRDIO( 2 )      !  output coordinates x,y
        INTEGER*4       IOSYS           !  output projection code
        INTEGER*4       IOZONE          !  output utm ZONE, etc.
        REAL*8          TPOUT( 15 )     !  output projection parameters
        INTEGER*4       IOUNIT          !  output units code
        INTEGER*4       LN27            !  NAD1927 file unit number
        INTEGER*4       LN83            !  NAD1983 file unit number
        CHARACTER*128   FN27            !  NAD1927 file name
        CHARACTER*128   FN83            !  NAD1983 file name
        INTEGER*4       LENGTH          !  NAD* record-length
        INTEGER*4       IFLG            !  error flag


        !!........  Body  ......................................................

        EFLAG = .FALSE.

        !!...............  Calculate Lat-Lon:
        !!...............  Set up arguments for call to GTP0:

        TPOUT = 0.0D0
        IPR    = 0              !!  print error messages, if any
        JPR    = 1              !!  do NOT print projection parameters
        LEMSG  = INIT3()        !!  unit number for log file
        LPARM  = LEMSG          !!  projection parameters file

        IOSYS  = 0              !!  geographic coords (=Lat-Lon)
        IOZONE = 0
        IOUNIT = 4              !!  output units:  degrees

        IF ( .NOT.M3TOGTPZ( GDTYP2, 100, P_ALP2, P_BET2, P_GAM2, YCENT2,    &
                            TPAIN, INSYS, INZONE, INUNIT, INSPH ) ) THEN
            EFLAG = .TRUE.
            MESG = 'Lat-Lon, LAM, UTM, TRM, POL, EQM, and ALB supported'
            CALL M3MSG2( MESG )
            WRITE( MESG, '( A, I6, 2X, A )' ) '>>> Output grid type', GDTYP2, 'not supported'
            CALL M3MESG( MESG )
        END IF

        !!...............  Sphere adjustment:

        INSPH = NINT( SPHER2 )

        IF ( SPHER2 .GT. 21.5d0 ) THEN
            INSPH    = -1
            TPAIN(1) = SPHER2
        ELSE IF ( SPHER2 .LT. -0.05d0 ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'Illegal sphere:  SPHER2 < 0' )
        ELSE IF ( DBLERR( SPHER2, DBLE( INSPH ) ) ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'Illegal sphere:  non-integer SPHER2 ' )
        END IF


        IF ( EFLAG ) THEN
            CALL M3EXIT( PNAME, 0, 0, 'Map-projection setup error(s)', 2 )
        END IF


        !!...............  Compute transform:

        IF ( GDTYP2 .EQ. LATGRD3 ) THEN

!$OMP        PARALLEL DO                                                &
!$OMP&           DEFAULT( NONE ),                                       &
!$OMP&            SHARED( NCOLS, NROWS, XLOC1, YLOC1, XLOC2, YLOC2 ),   &
!$OMP&           PRIVATE( C, R )

            DO  R = 1, NROWS
            DO  C = 1, NCOLS

                XLOC1( C,R ) = XLOC2( C,R )
                YLOC1( C,R ) = YLOC2( C,R )

            END DO
            END DO

        ELSE

!$OMP        PARALLEL DO                                                &
!$OMP&           DEFAULT( NONE ),                                       &
!$OMP&            SHARED( NCOLS, NROWS, XLOC1, YLOC1, XLOC2, YLOC2,     &
!$OMP&                    INSYS, INZONE, TPAIN, INUNIT, INSPH,          &
!$OMP&                    IOSYS, IOZONE, TPOUT, IOUNIT, LPARM,          &
!$OMP&                    IPR, JPR, LEMSG, LN27, LN83, FN27, FN83 ),    &
!$OMP&           PRIVATE( C, R, CRDIN, CRDIO, LENGTH, IFLG, MESG ),     &
!$OMP&         REDUCTION( .OR.:  EFLAG )

            DO  R = 1, NROWS
            DO  C = 1, NCOLS

                CRDIN( 1 ) = XLOC2( C,R )
                CRDIN( 2 ) = YLOC2( C,R )

                IF ( CRDIN( 1 ) .LT. AMISSD .OR. CRDIN( 2 ) .LT. AMISSD ) THEN
                    XLOC1( C,R ) = BADVALD
                    YLOC1( C,R ) = BADVALD
                    CYCLE
                END IF

!$OMP           CRITICAL( S_GTPZ0 )
                CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,     &
                            IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,   &
                            TPOUT, IOUNIT, LN27, LN83, FN27, FN83,          &
                            LENGTH, IFLG )
!$OMP           END CRITICAL( S_GTPZ0 )

                IF ( IFLG .NE. 0 ) THEN
                    IFLG  = MAX( MIN( 9, IFLG ), 1 )   !  trap between 1 and 9
                    WRITE( MESG, '( A, I3, 2X, A, I5, A, I5, A )' )   &
                       'Failure:  status ', IFLG, 'in GTPZ0 at (C,R)=(', C, ',', R, ')'
                    EFLAG = .TRUE.
                    CALL M3MESG( MESG )
                END IF

                XLOC1( C,R ) = CRDIO( 1 )
                YLOC1( C,R ) = CRDIO( 2 )

            END DO
            END DO

            IF ( EFLAG ) THEN
                CALL M3EXIT( PNAME, 0, 0, 'GRID2::LATLON coord-transform error(s)', 2 )
            END IF

        END IF          !  if sameproj, or not


        !!...............  Then compute GRID1 coords

        TPAIN = 0.0D0           !  array assignment
        IPR   = 0       !  print error messages, if any
        JPR   = 1       !  do NOT print projection parameters
        LEMSG = INIT3() !  unit number for log file
        LPARM = LEMSG   !  projection parameters file

        INSYS  = 0           !!  geographic (=Lat-Lon)
        INZONE = 0
        INUNIT = 4           !!  input units:  degrees

        IF ( .NOT.M3TOGTPZ( GDTYP1, 200, P_ALP1, P_BET1, P_GAM1, YCENT1,    &
                            TPOUT, IOSYS, IOZONE, IOUNIT, INSPH ) ) THEN
            EFLAG = .TRUE.
            MESG  = 'Lat-Lon, LAM, UTM, TRM, POL, EQM, and ALB supported'
            CALL M3MSG2( MESG )
            WRITE( MESG, '( A, I6, 2X, A )' ) '>>> Output grid type', GDTYP1, 'not supported'
            CALL M3MESG( MESG )
        END IF

        INSPH = NINT( SPHER1 )

        IF ( SPHER1 .GT. 21.5d0 ) THEN
            INSPH    = -1
            TPAIN(1) = SPHER2
        ELSE IF ( SPHER1 .LT. -0.05d0 ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'Illegal sphere:  SPHER2 < 0' )
        ELSE IF ( DBLERR( SPHER1, DBLE( INSPH ) ) ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'Illegal sphere:  non-integer SPHER1 ' )
        END IF

        IF ( GDTYP1 .EQ. LATGRD3 ) THEN

            CONTINUE

        ELSE

!$OMP        PARALLEL DO                                                &
!$OMP&           DEFAULT( NONE ),                                       &
!$OMP&            SHARED( NCOLS, NROWS, XLOC1, YLOC1, XLOC2, YLOC2,     &
!$OMP&                    INSYS, INZONE, TPAIN, INUNIT, INSPH,          &
!$OMP&                    IOSYS, IOZONE, TPOUT, IOUNIT, LPARM,          &
!$OMP&                    IPR, JPR, LEMSG, LN27, LN83, FN27, FN83 ),    &
!$OMP&           PRIVATE( C, R, CRDIN, CRDIO, LENGTH, IFLG, MESG ),     &
!$OMP&         REDUCTION( .OR.:  EFLAG )

            DO  R = 1, NROWS
            DO  C = 1, NCOLS

                CRDIN( 1 ) = XLOC1( C,R )
                CRDIN( 2 ) = YLOC1( C,R )

                IF ( CRDIN( 1 ) .LT. AMISSD .OR. CRDIN( 2 ) .LT. AMISSD ) THEN
                    XLOC1( C,R ) = BADVALD
                    YLOC1( C,R ) = BADVALD
                    CYCLE
                END IF

!$OMP           CRITICAL( S_GTPZ0 )
                CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,     &
                            IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,   &
                            TPOUT, IOUNIT, LN27, LN83, FN27, FN83,          &
                            LENGTH, IFLG )
!$OMP           END CRITICAL( S_GTPZ0 )

                IF ( IFLG .NE. 0 ) THEN
                    IFLG  = MAX( MIN( 9, IFLG ), 1 )   !  trap between 1 and 9
                    WRITE( MESG, '( A, I3, 2X, A, I5, A, I5, A )' )   &
                       'Failure:  status ', IFLG, 'in GTPZ0 at (C,R)=(', C, ',', R, ')'
                    EFLAG = .TRUE.
                    CALL M3MESG( MESG )
                END IF

                XLOC1( C,R ) = CRDIO( 1 )
                YLOC1( C,R ) = CRDIO( 2 )

            END DO
            END DO

            IF ( EFLAG ) THEN
                CALL M3EXIT( PNAME, 0, 0, 'LATLON::GRID1 coord-transform error(s)', 2 )
            END IF

        END IF          !  if latgrd3, or not

        RETURN


    END SUBROUTINE XY2XY2D2


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION  M3TOGTPZ1( GDTYP, IZONE1, P_ALP, P_BET, P_GAM, YCENT, &
                                 TPA, ISYS, IZONE, IUNIT, ISPH )

        INTEGER, INTENT(IN   ) :: GDTYP, IZONE1
        REAL*8 , INTENT(IN   ) :: P_ALP, P_BET, P_GAM, YCENT
        REAL*8 , INTENT(  OUT) :: TPA( 15 )
        INTEGER, INTENT(  OUT) :: ISYS, IZONE, IUNIT
        INTEGER, INTENT(INOUT) :: ISPH

        M3TOGTPZ1 =  M3TOGTPZ2( GDTYP, IZONE1, P_ALP, P_BET, P_GAM, YCENT,  &
                                DBLE(KSPH),                                 &
                                TPA, ISYS, IZONE, IUNIT, ISPH )
        

        RETURN


    END FUNCTION M3TOGTPZ1


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION  M3TOGTPZ2( GDTYP, IZONE1, P_ALP, P_BET, P_GAM, YCENT, SPHER,  &
                                 TPA, ISYS, IZONE, IUNIT, ISPH )

        INTEGER, INTENT(IN   ) :: GDTYP, IZONE1
        REAL*8 , INTENT(IN   ) :: P_ALP, P_BET, P_GAM, YCENT, SPHER
        REAL*8 , INTENT(  OUT) :: TPA( 15 )
        INTEGER, INTENT(  OUT) :: ISYS, IZONE, IUNIT
        INTEGER, INTENT(INOUT) :: ISPH

        REAL*8      DSCR            !  scratch variables
        INTEGER     DEG, MNT        !  scratch variables
        LOGICAL     EFLAG

        CHARACTER*512   MESG

        !!..........   body   .........................................

        EFLAG = .FALSE.
        TPA   = 0.0D0           !  array assignment

        ISPH = NINT( SPHER )

        IF ( SPHER .GT. 21.5d0 ) THEN
            ISPH   = -1
            TPA(1) = SPHER
            TPA(2) = 0.0d0
        ELSE IF ( SPHER .LT. -0.05d0 ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'Illegal sphere:  SPHER < 0' )
        ELSE IF ( DBLERR( SPHER, DBLE( ISPH ) ) ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'Illegal sphere:  non-integer SPHER2 ' )
        ELSE IF ( ISPH .EQ. 21 ) THEN
            ISPH   = -1
            TPA(1) = RADE21
            TPA(2) = 0.0d0
        ELSE IF ( ISPH .EQ. 20 ) THEN
            ISPH   = -1
            TPA(1) = RADE20
            TPA(2) = 0.0d0
        ELSE IF ( ISPH .EQ. 19 ) THEN
            ISPH   = -1
            TPA(1) = RADE19
            TPA(2) = 0.0d0
        END IF

        IF ( GDTYP .EQ. LATGRD3 ) THEN

            ISYS  = 0       !  geographic (=Lat-Lon)
            IZONE = 0
            IUNIT = 4       !  output units:  REAL degrees

        ELSE IF ( GDTYP .EQ. LAMGRD3 ) THEN

            TPA( 3 ) = DDDMMMSSS( P_ALP ) !  dddmmmsss.sssD0
            TPA( 4 ) = DDDMMMSSS( P_BET ) !  dddmmmsss.sssD0
            TPA( 5 ) = DDDMMMSSS( P_GAM ) !  dddmmmsss.sssD0
            TPA( 6 ) = DDDMMMSSS( YCENT ) !  dddmmmsss.sssD0

            ISYS  = 4       !  Lambert conformal conic
            IZONE = IZONE1 + 1
            IUNIT = 2       !  input units:  meters

        ELSE IF ( GDTYP .EQ. UTMGRD3 ) THEN

            ISYS  = 1       !  Universal Transverse Mercator
            IZONE = NINT( P_ALP )
            IUNIT = 2       !  input units:  meters
            ISPH  = 8       !  GRS 1980 spheroid

        ELSE IF ( GDTYP .EQ. POLGRD3 ) THEN

            TPA( 5 ) = DDDMMMSSS( P_GAM ) !  dddmmmsss.sssD0
            TPA( 6 ) = DDDMMMSSS( P_BET ) !  dddmmmsss.sssD0

            ISYS  = 6       !  Polar stereographic
            IZONE = IZONE1 + 2
            IUNIT = 2       !  input units:  meters

        ELSE IF ( GDTYP .EQ. TRMGRD3 ) THEN

            TPA( 5 ) = DDDMMMSSS( P_GAM ) !  dddmmmsss.sssD0
            TPA( 6 ) = DDDMMMSSS( P_ALP ) !  dddmmmsss.sssD0

            ISYS  = 9       !  Transverse Mercator
            IZONE = IZONE1 + 3
            IUNIT = 2       !  input units:  meters

        ELSE IF ( GDTYP .EQ. EQMGRD3 ) THEN

            TPA( 5 ) = DDDMMMSSS( P_GAM ) !  dddmmmsss.sssD0
            TPA( 6 ) = DDDMMMSSS( P_ALP ) !  dddmmmsss.sssD0

            ISYS  = 5       !  Equatorial Mercator
            IZONE = IZONE1 + 4
            IUNIT = 2       !  input units:  meters

        ELSE IF ( GDTYP .EQ. ALBGRD3 ) THEN


            TPA( 3 ) = DDDMMMSSS( P_ALP ) !  dddmmmsss.sssD0
            TPA( 4 ) = DDDMMMSSS( P_BET ) !  dddmmmsss.sssD0
            TPA( 5 ) = DDDMMMSSS( P_GAM ) !  dddmmmsss.sssD0
            TPA( 6 ) = DDDMMMSSS( YCENT ) !  dddmmmsss.sssD0

            ISYS  = 3       !  Albers Conic Equal Area
            IZONE = IZONE1 + 5
            IUNIT = 2       !  input units:  meters

        ELSE IF ( GDTYP .EQ. SINUGRD3 ) THEN

            TPA( 6 ) = DDDMMMSSS( P_GAM ) !  dddmmmsss.sssD0

            ISYS  = 16      !  Sinusoidal Equal Area
            IZONE = IZONE1 + 5
            IUNIT = 2       !  input units:  meters

        ELSE

            EFLAG = .TRUE.
            MESG  = 'Lat-Lon, LAM, UTM, TRM, POL, EQM, and ALB supported'
            CALL M3MSG2( MESG )
            WRITE( MESG, '( A, I6, 2X, A, A )' ) 'Grid type', GDTYP,'not supported'
            CALL M3MESG( MESG )

        END IF  ! if non-Lam grid (etc...) for GTP0 input

        M3TOGTPZ2 = ( .NOT. EFLAG )

        RETURN


    END FUNCTION M3TOGTPZ2


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !!  Set up a particular named projection or grid  CNAME:
    !!  INITPROJD() returns  REAL*8 projection parameters
    !!  INITPROJS() returns  REAL   projection parameters
    !!  INITPROJ1() has no return values
    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

    LOGICAL FUNCTION INITPROJD( CNAME, CTYPE, P_ALP, P_BET, P_GAM, XCENT, YCENT )

        CHARACTER*(*), INTENT(IN   ) :: CNAME
        INTEGER      , INTENT(  OUT) :: CTYPE
        REAL*8       , INTENT(  OUT) :: P_ALP, P_BET, P_GAM, XCENT, YCENT

        CHARACTER*16    ANAME
        CHARACTER*256   MESG
        INTEGER         NCOLS, NROWS, NTHIK
        REAL*8          XORIG, YORIG, XCELL, YCELL

        !!------------  begin body   ---------------------------------

        IF ( .NOT. INITSPHERES() ) THEN
            CALL M3WARN( 'MODGCTP/INITPROJ',0,0,'Bad geodetic sphere info' )
        END IF

        IF ( DSCOORD( CNAME, CTYPE, P_ALP, P_BET, P_GAM, XCENT, YCENT ) ) THEN
            CONTINUE
        ELSE IF ( DSCGRID( CNAME, ANAME, CTYPE,                     &
                           P_ALP, P_BET, P_GAM, XCENT, YCENT,       &
                           XORIG, YORIG, XCELL, YCELL, NCOLS, NROWS, NTHIK ) ) THEN
            CONTINUE
        ELSE        !  dscgrid and dscoord both failed
            CALL M3WARN( 'MODGCTP/INITPROJ', 0, 0, 'Projection not found in GRIDDESC' )
            INITPROJD = .FALSE.
            RETURN
        END IF          !  if dscoord failed

        !!.......   Return the projection parameters as REAL   A,B,C,X,Y:

        IF ( CTYPE .EQ. LATGRD3 ) THEN

            CONTINUE

        ELSE IF ( CTYPE .EQ. LAMGRD3 ) THEN

            LZONE  = LZONE + 5
            P_ALPL = P_ALP
            P_BETL = P_BET
            P_GAML = P_GAM
            XCENTL = XCENT
            YCENTL = YCENT

        ELSE IF ( CTYPE .EQ. UTMGRD3 ) THEN

            CONTINUE

        ELSE IF ( CTYPE .EQ. POLGRD3 ) THEN

            PZONE  = PZONE + 5
            P_ALPP = P_ALP
            P_BETP = P_BET
            P_GAMP = P_GAM
            XCENTP = XCENT
            YCENTP = YCENT
            TPAIN  = 0.0D0
            TPARO  = 0.0D0
            CRDIN( 1 ) = XCENT
            CRDIN( 2 ) = YCENT
            INSYS  = 0       !  projection default (lat-lon)
            INUNIT = 4       !  input units:  degrees
            INSPH  = 8       !  GRS 1980 spheroid
            IPR    = 0       !  print error messages, if any
            JPR    = 1       !  do NOT print projection parameters
            LEMSG  = INIT3() !  unit number for log file
            LPARM  = LEMSG   !  projection parameters file
            IOSYS  = 6       !  Polar stereographic
            IOZONE = PZONE   !  POL zone
            IOUNIT = 2       !  output units: meters
            TPARO( 1 ) = 0.0D0
            TPARO( 5 ) = DDDMMMSSS( P_GAM )
            TPARO( 6 ) = DDDMMMSSS( P_BET )
            IF ( .NOT.SPHEREDAT( INSPH, TPAIN, TPARO ) ) THEN
                CALL M3WARN( 'MODGCTP/INITPROJ',0,0,'Bad geodetic sphere info' )
            END IF
            CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,     &
                        IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,   &
                        TPARO, IOUNIT, LN27, LN83, FN27, FN83, LENGTH, IFLG )
            IF ( IFLG .NE. 0 ) THEN
                IFLG = MAX( MIN( 9, IFLG ), 1 )     !  between 1 and 9
                CALL M3WARN( 'MODGCTP/INITPROJ', 0,0, GCTPMESG( IFLG ) )
                INITPROJD = .FALSE.
                RETURN
            END IF
            XCENTP = -CRDIO( 1 )
            YCENTP = -CRDIO( 2 )
            PZONE  =  PZONE + 5

        ELSE IF ( CTYPE .EQ. TRMGRD3 ) THEN

            TZONE  = TZONE + 5
            P_ALPT = P_ALP
            P_BETT = P_BET
            P_GAMT = P_GAM
            XCENTT = XCENT
            YCENTT = YCENT
            TPAIN  = 0.0D0
            TPARO  = 0.0D0
            CRDIN( 1 ) = XCENT
            CRDIN( 2 ) = YCENT
            INSYS  = 0       !  projection default (lat-lon)
            INUNIT = 4       !  input units:  degrees
            INSPH  = 8       !  GRS 1980 spheroid
            IPR    = 0       !  print error messages, if any
            JPR    = 1       !  do NOT print projection parameters
            LEMSG  = INIT3() !  unit number for log file
            LPARM  = LEMSG   !  projection parameters file
            IOSYS  = 6       !  Polar stereographic
            IOZONE = PZONE   !  POL zone
            IOUNIT = 2       !  output units: meters
            TPARO( 1 ) = 0.0D0
            TPARO( 5 ) = DDDMMMSSS( P_GAM )
            TPARO( 6 ) = DDDMMMSSS( P_BET )
            IF ( .NOT.SPHEREDAT( INSPH, TPAIN, TPARO ) ) THEN
                CALL M3WARN( 'MODGCTP/INITPROJ',0,0,'Bad geodetic sphere info' )
            END IF
            CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,     &
                        IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,   &
                        TPARO, IOUNIT, LN27, LN83, FN27, FN83, LENGTH, IFLG )
            IF ( IFLG .NE. 0 ) THEN
                IFLG = MAX( MIN( 9, IFLG ), 1 )     !  between 1 and 9
                CALL M3WARN( 'MODGCTP/INITPROJ', 0,0, GCTPMESG( IFLG ) )
                INITPROJD = .FALSE.
                RETURN
            END IF
            XCENTT = CRDIO( 1 )
            YCENTT = CRDIO( 2 )
            TZONE  = TZONE + 5

        ELSE IF ( CTYPE .EQ. EQMGRD3 ) THEN

            EZONE  = EZONE + 5
            P_ALPE = P_ALP
            P_BETE = P_BET
            P_GAME = P_GAM
            XCENTE = XCENT
            YCENTE = YCENT
            TPAIN  = 0.0D0
            TPARO  = 0.0D0
            CRDIN( 1 ) = XCENT
            CRDIN( 2 ) = YCENT
            INSYS  = 0       !  projection default (lat-lon)
            INUNIT = 4       !  input units:  degrees
            INSPH  = 8       !  GRS 1980 spheroid
            IPR    = 0       !  print error messages, if any
            JPR    = 1       !  do NOT print projection parameters
            LEMSG  = INIT3() !  unit number for log file
            LPARM  = LEMSG   !  projection parameters file
            IOSYS  = 6       !  Polar stereographic
            IOZONE = PZONE   !  POL zone
            IOUNIT = 2       !  output units: meters
            TPARO( 1 ) = 0.0D0
            TPARO( 5 ) = DDDMMMSSS( P_GAM )
            TPARO( 6 ) = DDDMMMSSS( P_BET )
            IF ( .NOT.SPHEREDAT( INSPH, TPAIN, TPARO ) ) THEN
                CALL M3WARN( 'MODGCTP/INITPROJ',0,0,'Bad geodetic sphere info' )
            END IF
            CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,     &
                        IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,   &
                        TPARO, IOUNIT, LN27, LN83, FN27, FN83, LENGTH, IFLG )
            IF ( IFLG .NE. 0 ) THEN
                IFLG = MAX( MIN( 9, IFLG ), 1 )     !  between 1 and 9
                CALL M3WARN( 'MODGCTP/INITPROJ', 0,0, GCTPMESG( IFLG ) )
                INITPROJD = .FALSE.
                RETURN
            END IF
            XCENTT = CRDIO( 1 )
            YCENTT = CRDIO( 2 )
            EZONE  = EZONE + 5

        ELSE IF ( CTYPE .EQ. ALBGRD3 ) THEN

            AZONE  = AZONE + 5
            P_ALPA = P_ALP
            P_BETA = P_BET
            P_GAMA = P_GAM
            XCENTA = XCENT
            YCENTA = YCENT

        ELSE

            WRITE( MESG, '(A, I6)' ) 'Unrecognized projection type=', CTYPE
            CALL M3WARN( 'MODGCTP/INITPROJ', 0,0, MESG )
            INITPROJD = .FALSE.
            RETURN

        END IF        !!  if latlon; else if lambert, polste, trm, eqm, albers; else...

        INITPROJD = .TRUE.
        RETURN

    END FUNCTION INITPROJD

    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

    LOGICAL FUNCTION INITPROJS( CNAME, CTYPE, A, B, C, X, Y )

        CHARACTER*(*), INTENT(IN   ) :: CNAME
        INTEGER      , INTENT(  OUT) :: CTYPE
        REAL         , INTENT(  OUT) :: A, B, C, X, Y

        REAL*8          P_ALP, P_BET, P_GAM, XCENT, YCENT


        IF ( INITPROJD( CNAME, CTYPE, P_ALP, P_BET, P_GAM, XCENT, YCENT ) ) THEN
            A = SNGL( P_ALP )
            B = SNGL( P_BET )
            C = SNGL( P_GAM )
            X = SNGL( XCENT )
            Y = SNGL( YCENT )
            INITPROJS = .TRUE.
        ELSE
            INITPROJS = .FALSE.
        END IF

        RETURN

    END FUNCTION INITPROJS

    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

    LOGICAL FUNCTION INITPROJ1( CNAME )

        CHARACTER*(*), INTENT(IN   ) :: CNAME

        INTEGER     CTYPE
        REAL*8      P_ALP, P_BET, P_GAM, XCENT, YCENT

        INITPROJ1 =  INITPROJD( CNAME, CTYPE, P_ALP, P_BET, P_GAM, XCENT, YCENT )

        RETURN

    END FUNCTION INITPROJ1


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !!  Set up a particular anonymous projection:
    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

    LOGICAL FUNCTION SETPROJD( CTYPE, P_ALP, P_BET, P_GAM, XCENT, YCENT )

        INTEGER, INTENT(IN   ) :: CTYPE
        REAL*8 , INTENT(IN   ) :: P_ALP, P_BET, P_GAM, XCENT, YCENT

        CHARACTER*256   MESG

        !!------------  begin body   ---------------------------------

        IF ( CTYPE .EQ. LATGRD3 ) THEN

            CONTINUE

        ELSE IF ( CTYPE .EQ. LAMGRD3 ) THEN

            LZONE  = LZONE + 5
            P_ALPL = P_ALP
            P_BETL = P_BET
            P_GAML = P_GAM
            XCENTL = XCENT
            YCENTL = YCENT

        ELSE IF ( CTYPE .EQ. UTMGRD3 ) THEN

            CONTINUE

        ELSE IF ( CTYPE .EQ. POLGRD3 ) THEN

            PZONE  = PZONE + 5
            P_ALPP = P_ALP
            P_BETP = P_BET
            P_GAMP = P_GAM
            XCENTP = XCENT
            YCENTP = YCENT
            TPAIN  = 0.0D0
            TPARO  = 0.0D0
            CRDIN( 1 ) = XCENT
            CRDIN( 2 ) = YCENT
            INSYS  = 0       !  projection default (lat-lon)
            INUNIT = 4       !  input units:  degrees
            INSPH  = 8       !  GRS 1980 spheroid
            IPR    = 0       !  print error messages, if any
            JPR    = 1       !  do NOT print projection parameters
            LEMSG  = INIT3() !  unit number for log file
            LPARM  = LEMSG   !  projection parameters file
            IOSYS  = 6       !  Polar stereographic
            IOZONE = PZONE   !  POL zone
            IOUNIT = 2       !  output units: meters
            TPARO( 1 ) = 0.0D0
            TPARO( 5 ) = DDDMMMSSS( P_GAM )
            TPARO( 6 ) = DDDMMMSSS( P_BET )
            IF ( .NOT.SPHEREDAT( INSPH, TPAIN, TPARO ) ) THEN
                CALL M3WARN( 'MODGCTP/SETPROJ',0,0,'Bad geodetic sphere info' )
            END IF
            CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,     &
                        IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,   &
                        TPARO, IOUNIT, LN27, LN83, FN27, FN83, LENGTH, IFLG )
            IF ( IFLG .NE. 0 ) THEN
                IFLG = MAX( MIN( 9, IFLG ), 1 )     !  between 1 and 9
                CALL M3WARN( 'MODGCTP/SETPROJ', 0,0, GCTPMESG( IFLG ) )
                SETPROJD = .FALSE.
                RETURN
            END IF
            XCENTP = -CRDIO( 1 )
            YCENTP = -CRDIO( 2 )
            PZONE  =  PZONE + 5

        ELSE IF ( CTYPE .EQ. TRMGRD3 ) THEN

            TZONE  = TZONE + 5
            P_ALPT = P_ALP
            P_BETT = P_BET
            P_GAMT = P_GAM
            XCENTT = XCENT
            YCENTT = YCENT
            TPAIN  = 0.0D0
            TPARO  = 0.0D0
            CRDIN( 1 ) = XCENT
            CRDIN( 2 ) = YCENT
            INSYS  = 0       !  projection default (lat-lon)
            INUNIT = 4       !  input units:  degrees
            INSPH  = 8       !  GRS 1980 spheroid
            IPR    = 0       !  print error messages, if any
            JPR    = 1       !  do NOT print projection parameters
            LEMSG  = INIT3() !  unit number for log file
            LPARM  = LEMSG   !  projection parameters file
            IOSYS  = 6       !  Polar stereographic
            IOZONE = PZONE   !  POL zone
            IOUNIT = 2       !  output units: meters
            TPARO( 1 ) = 0.0D0
            TPARO( 5 ) = DDDMMMSSS( P_GAM )
            TPARO( 6 ) = DDDMMMSSS( P_BET )
            IF ( .NOT.SPHEREDAT( INSPH, TPAIN, TPARO ) ) THEN
                CALL M3WARN( 'MODGCTP/SETPROJ',0,0,'Bad geodetic sphere info' )
            END IF
            CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,     &
                        IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,   &
                        TPARO, IOUNIT, LN27, LN83, FN27, FN83, LENGTH, IFLG )
            IF ( IFLG .NE. 0 ) THEN
                IFLG = MAX( MIN( 9, IFLG ), 1 )     !  between 1 and 9
                CALL M3WARN( 'MODGCTP/SETPROJ', 0,0, GCTPMESG( IFLG ) )
                SETPROJD = .FALSE.
                RETURN
            END IF
            XCENTT = CRDIO( 1 )
            YCENTT = CRDIO( 2 )
            TZONE  = TZONE + 5

        ELSE IF ( CTYPE .EQ. EQMGRD3 ) THEN

            EZONE  = EZONE + 5
            P_ALPE = P_ALP
            P_BETE = P_BET
            P_GAME = P_GAM
            XCENTE = XCENT
            YCENTE = YCENT
            TPAIN  = 0.0D0
            TPARO  = 0.0D0
            CRDIN( 1 ) = XCENT
            CRDIN( 2 ) = YCENT
            INSYS  = 0       !  projection default (lat-lon)
            INUNIT = 4       !  input units:  degrees
            INSPH  = 8       !  GRS 1980 spheroid
            IPR    = 0       !  print error messages, if any
            JPR    = 1       !  do NOT print projection parameters
            LEMSG  = INIT3() !  unit number for log file
            LPARM  = LEMSG   !  projection parameters file
            IOSYS  = 6       !  Polar stereographic
            IOZONE = PZONE   !  POL zone
            IOUNIT = 2       !  output units: meters
            TPARO( 1 ) = 0.0D0
            TPARO( 5 ) = DDDMMMSSS( P_GAM )
            TPARO( 6 ) = DDDMMMSSS( P_BET )
            IF ( .NOT.SPHEREDAT( INSPH, TPAIN, TPARO ) ) THEN
                CALL M3WARN( 'MODGCTP/SETPROJ',0,0,'Bad geodetic sphere info' )
            END IF
            CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,     &
                        IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,   &
                        TPARO, IOUNIT, LN27, LN83, FN27, FN83, LENGTH, IFLG )
            IF ( IFLG .NE. 0 ) THEN
                IFLG = MAX( MIN( 9, IFLG ), 1 )     !  between 1 and 9
                CALL M3WARN( 'MODGCTP/SETPROJ', 0,0, GCTPMESG( IFLG ) )
                SETPROJD = .FALSE.
                RETURN
            END IF
            XCENTT = CRDIO( 1 )
            YCENTT = CRDIO( 2 )
            EZONE  = EZONE + 5

        ELSE IF ( CTYPE .EQ. ALBGRD3 ) THEN

            AZONE  = AZONE + 5
            P_ALPA = P_ALP
            P_BETA = P_BET
            P_GAMA = P_GAM
            XCENTA = XCENT
            YCENTA = YCENT

        ELSE

            WRITE( MESG, '(A, I6)' ) 'Unrecognized projection type=', CTYPE
            CALL M3WARN( 'MODGCTP/SETPROJ', 0,0, MESG )
            SETPROJD = .FALSE.
            RETURN

        END IF        !!  if latlon; else if lambert, polste, trm, eqm, albers; else...

        SETPROJD = .TRUE.
        RETURN

    END FUNCTION SETPROJD

    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

    LOGICAL FUNCTION SETPROJS( CTYPE, A, B, C, X, Y )

        INTEGER      , INTENT(IN   ) :: CTYPE
        REAL         , INTENT(IN   ) :: A, B, C, X, Y

        SETPROJS = SETPROJD( CTYPE, DBLEFIX( A ), DBLEFIX( B ), DBLEFIX( C ), DBLEFIX( X ), DBLEFIX( Y ) )

        RETURN

    END FUNCTION SETPROJS


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !!  Set up a particular named Lambert projection:
    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

    LOGICAL FUNCTION LAMBERT( CNAME, A, B, C, X, Y )

        CHARACTER*(*), INTENT(IN   ) :: CNAME
        REAL         , INTENT(  OUT) :: A, B, C, X, Y

        CHARACTER*16    ANAME
        CHARACTER*256   MESG
        INTEGER         CTYPE, NCOLS, NROWS, NTHIK
        REAL*8          XORIG, YORIG, XCELL, YCELL

        !!------------  begin body   ---------------------------------

        IF ( .NOT. INITSPHERES() ) THEN
            CALL M3WARN( 'MODGCTP/LAMBERT',0,0,'Bad geodetic sphere info' )
        END IF

        IF ( DSCOORD( CNAME, CTYPE, P_ALPL, P_BETL, P_GAML, XCENTL, YCENTL ) ) THEN
            CONTINUE
        ELSE IF ( DSCGRID( CNAME, ANAME, CTYPE,                     &
                           P_ALPL, P_BETL, P_GAML, XCENTL, YCENTL,  &
                           XORIG, YORIG, XCELL, YCELL, NCOLS, NROWS, NTHIK ) ) THEN
            CONTINUE
        ELSE        !  dscgrid and dscoord both failed
            CALL M3WARN( 'MODGCTP/LAMBERT', 0, 0, 'Projection not found in GRIDDESC' )
            LAMBERT = .FALSE.
            RETURN
        END IF          !  if dscoord failed

        IF ( CTYPE .NE. LAMGRD3 ) THEN
            WRITE( MESG,'( A, I10, :, 2X )' ) 'Projection not Lambert:  type ', CTYPE
            CALL M3WARN( 'MODGCTP/LAMBERT', 0, 0, MESG )
            LAMBERT = .FALSE.
            RETURN
        END IF

        !!.......   Return the projection parameters as REAL   A,B,C,X,Y:

        A = SNGL( P_ALPL )
        B = SNGL( P_BETL )
        C = SNGL( P_GAML )
        X = SNGL( XCENTL )
        Y = SNGL( YCENTL )

        LZONE  = LZONE + 5

        LAMBERT = .TRUE.
        RETURN

    END FUNCTION LAMBERT


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !!  Set up a particular named Polar Stereographic projection:
    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

    LOGICAL FUNCTION POLSTE( CNAME, A, B, C, X, Y )

        CHARACTER*(*), INTENT(IN   ) :: CNAME
        REAL         , INTENT(  OUT) :: A, B, C, X, Y

        CHARACTER*16    ANAME
        CHARACTER*256   MESG
        INTEGER         CTYPE, NCOLS, NROWS, NTHIK
        REAL*8          XORIG, YORIG, XCELL, YCELL

        !!------------  begin body   ---------------------------------

        IF ( .NOT. INITSPHERES() ) THEN
            CALL M3WARN( 'MODGCTP/POLSTE',0,0,'Bad geodetic sphere info' )
        END IF

        IF ( DSCOORD( CNAME, CTYPE, P_ALPP, P_BETP, P_GAMP, XCENTP, YCENTP ) ) THEN
            CONTINUE
        ELSE IF ( DSCGRID( CNAME, ANAME, CTYPE,                     &
                           P_ALPP, P_BETP, P_GAMP, XCENTP, YCENTP,  &
                           XORIG, YORIG, XCELL, YCELL, NCOLS, NROWS, NTHIK ) ) THEN
            CONTINUE
        ELSE        !  dscgrid and dscoord both failed
            CALL M3WARN( 'MODGCTP/POLSTE', 0, 0, 'Projection not found in GRIDDESC' )
            POLSTE = .FALSE.
            RETURN
        END IF          !  if dscoord failed

        IF ( CTYPE .NE. POLGRD3 ) THEN
            WRITE( MESG,'( A, I10, :, 2X )' ) 'Projection not POLGRD3:  type ', CTYPE
            CALL M3WARN( 'MODGCTP/POLSTE', 0, 0, MESG )
            POLSTE = .FALSE.
            RETURN
        END IF

        !!.......   Return the projection parameters as REAL   A,B,C,X,Y:

        A = SNGL( P_ALPP )
        B = SNGL( P_BETP )
        C = SNGL( P_GAMP )
        X = SNGL( XCENTP )
        Y = SNGL( YCENTP )

        PZONE = PZONE + 5

        !!.......    Convert <XCENT,YCENT> to POL-Cartesian offsets
        !!.......   Set up input arguments for GTPZ0():

        TPAIN = 0.0D0
        TPARO = 0.0D0

        CRDIN( 1 ) = XCENTP
        CRDIN( 2 ) = YCENTP
        INSYS  = 0       !  projection default (lat-lon)
        INUNIT = 4       !  input units:  degrees
        INSPH  = 8       !  GRS 1980 spheroid
        IPR    = 0       !  print error messages, if any
        JPR    = 1       !  do NOT print projection parameters
        LEMSG  = INIT3() !  unit number for log file
        LPARM  = LEMSG   !  projection parameters file
        IOSYS  = 6       !  Polar stereographic
        IOZONE = PZONE   !  POL zone
        IOUNIT = 2       !  output units: meters
        TPARO( 1 ) = 0.0D0
        TPARO( 5 ) = DDDMMMSSS( P_GAMP )
        TPARO( 6 ) = DDDMMMSSS( P_BETP )

        IF ( .NOT.SPHEREDAT( INSPH, TPAIN, TPARO ) ) THEN
            CALL M3WARN( 'MODGCTP/POLSTE',0,0,'Bad geodetic sphere info' )
        END IF

        CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,     &
                    IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,   &
                    TPARO, IOUNIT, LN27, LN83, FN27, FN83, LENGTH, IFLG )

        IF ( IFLG .NE. 0 ) THEN
            IFLG = MAX( MIN( 9, IFLG ), 1 )     !  between 1 and 9
            CALL M3WARN( 'MODGCTP/POLSTE', 0,0, GCTPMESG( IFLG ) )
            POLSTE = .FALSE.
            RETURN
        END IF

        !!.......   Decode output arguments for GTPZ0();
        !!.......   update PZONE for the new false-easting/false-northing offsets

        XCENTP = -CRDIO( 1 )
        YCENTP = -CRDIO( 2 )
        PZONE  =  PZONE + 5
        POLSTE = .TRUE.
        RETURN

    END FUNCTION POLSTE


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !!  Set up a particular named Transverse Mercator projection:
    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION TRMERC( CNAME, A, B, C, X, Y )

        CHARACTER*(*), INTENT(IN   ) :: CNAME
        REAL         , INTENT(  OUT) :: A, B, C, X, Y

        CHARACTER*16    ANAME
        CHARACTER*256   MESG
        INTEGER         CTYPE, NCOLS, NROWS, NTHIK
        REAL*8          XORIG, YORIG, XCELL, YCELL

        !!------------  begin body   ---------------------------------

        IF ( .NOT. INITSPHERES() ) THEN
            CALL M3WARN( 'MODGCTP/TRMERC',0,0,'Bad geodetic sphere info' )
        END IF

        IF ( DSCOORD( CNAME, CTYPE, P_ALPT, P_BETT, P_GAMT, XCENTT, YCENTT ) ) THEN
            CONTINUE
        ELSE IF ( DSCGRID( CNAME, ANAME, CTYPE,                     &
                           P_ALPT, P_BETT, P_GAMT, XCENTT, YCENTT,  &
                           XORIG, YORIG, XCELL, YCELL, NCOLS, NROWS, NTHIK ) ) THEN
            CONTINUE
        ELSE        !  dscgrid and dscoord both failed
            CALL M3WARN( 'MODGCTP/TRMERC', 0, 0, 'Projection not found in GRIDDESC' )
            TRMERC = .FALSE.
            RETURN
        END IF          !  if dscoord failed

        IF ( CTYPE .NE. TRMGRD3 ) THEN
            WRITE( MESG,'( A, I10, :, 2X )' ) 'Projection not TRMGRD3:  type ', CTYPE
            CALL M3WARN( 'MODGCTP/TRMERC', 0, 0, MESG )
            TRMERC = .FALSE.
            RETURN
        END IF

        !!.......   Return the projection parameters as REAL   A,B,C,X,Y:

        A = SNGL( P_ALPT )
        B = SNGL( P_BETT )
        C = SNGL( P_GAMT )
        X = SNGL( XCENTT )
        Y = SNGL( YCENTT )

        TZONE   = TZONE + 5

        !!.......    Convert <XCENT,YCENT> to POL-Cartesian offsets
        !!.......   Set up input arguments for GTPZ0():

        TPAIN = 0.0D0
        TPARO = 0.0D0

        CRDIN( 1 ) = XCENTT
        CRDIN( 2 ) = YCENTT
        INSYS  = 0       !  projection default (lat-lon)
        INUNIT = 4       !  input units:  degrees
        INSPH  = 8       !  GRS 1980 spheroid
        IPR    = 0       !  print error messages, if any
        JPR    = 1       !  do NOT print projection parameters
        LEMSG  = INIT3() !  unit number for log file
        LPARM  = LEMSG   !  projection parameters file
        IOSYS  = 6       !  Polar stereographic
        IOZONE = PZONE   !  POL zone
        IOUNIT = 2       !  output units: meters
        TPARO( 1 ) = 0.0D0
        TPARO( 5 ) = DDDMMMSSS( P_GAMT )
        TPARO( 6 ) = DDDMMMSSS( P_BETT )

        IF ( .NOT.SPHEREDAT( INSPH, TPAIN, TPARO ) ) THEN
            CALL M3WARN( 'MODGCTP/TRMERC',0,0,'Bad geodetic sphere info' )
        END IF

        CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,     &
                    IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,   &
                    TPARO, IOUNIT, LN27, LN83, FN27, FN83, LENGTH, IFLG )

        IF ( IFLG .NE. 0 ) THEN
            IFLG = MAX( MIN( 9, IFLG ), 1 )     !  between 1 and 9
            CALL M3WARN( 'MODGCTP/TRMERC', 0,0, GCTPMESG( IFLG ) )
            TRMERC = .FALSE.
            RETURN
        END IF

        !!.......   Decode output arguments for GTPZ0();
        !!.......   update PZONE for the new false-easting/false-northing offsets

        XCENTT =  CRDIO( 1 )
        YCENTT =  CRDIO( 2 )
        TZONE  =  TZONE + 5
        TRMERC = .TRUE.
        RETURN

    END FUNCTION TRMERC


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !!  Set up a particular named Lambert projection:
    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION EQMERC( CNAME, A, B, C, X, Y )

        CHARACTER*(*), INTENT(IN   ) :: CNAME
        REAL         , INTENT(  OUT) :: A, B, C, X, Y

        CHARACTER*16    ANAME
        CHARACTER*256   MESG
        INTEGER         CTYPE, NCOLS, NROWS, NTHIK
        REAL*8          XORIG, YORIG, XCELL, YCELL

        !!------------  begin body   ---------------------------------

        IF ( .NOT. INITSPHERES() ) THEN
            CALL M3WARN( 'MODGCTP/EQMERC',0,0,'Bad geodetic sphere info' )
        END IF

        IF ( DSCOORD( CNAME, CTYPE, P_ALPA, P_BETA, P_GAMA, XCENTA, YCENTE ) ) THEN
            CONTINUE
        ELSE IF ( DSCGRID( CNAME, ANAME, CTYPE,                     &
                           P_ALPA, P_BETA, P_GAMA, XCENTA, YCENTA,  &
                           XORIG, YORIG, XCELL, YCELL, NCOLS, NROWS, NTHIK ) ) THEN
            CONTINUE
        ELSE        !  dscgrid and dscoord both failed
            CALL M3WARN( 'MODGCTP/EQMERC', 0, 0, 'Projection not found in GRIDDESC' )
            EQMERC = .FALSE.
            RETURN
        END IF          !  if dscoord failed

        IF ( CTYPE .NE. EQMGRD3 ) THEN
            WRITE( MESG,'( A, I10, :, 2X )' ) 'Projection not TRMGRD3:  type ', CTYPE
            CALL M3WARN( 'MODGCTP/EQMERC', 0, 0, MESG )
            EQMERC = .FALSE.
            RETURN
        END IF

        !!.......   Return the projection parameters as REAL   A,B,C,X,Y:

        A = SNGL( P_ALPE )
        B = SNGL( P_BETE )
        C = SNGL( P_GAME )
        X = SNGL( XCENTE )
        Y = SNGL( YCENTE )

        EZONE   = EZONE + 5

        !!.......    Convert <XCENT,YCENT> to POL-Cartesian offsets
        !!.......   Set up input arguments for GTPZ0():

        TPAIN = 0.0D0
        TPARO = 0.0D0

        CRDIN( 1 ) = XCENTT
        CRDIN( 2 ) = YCENTT
        INSYS  = 0       !  projection default (lat-lon)
        INUNIT = 4       !  input units:  degrees
        INSPH  = 8       !  GRS 1980 spheroid
        IPR    = 0       !  print error messages, if any
        JPR    = 1       !  do NOT print projection parameters
        LEMSG  = INIT3() !  unit number for log file
        LPARM  = LEMSG   !  projection parameters file
        IOSYS  = 6       !  Polar stereographic
        IOZONE = PZONE   !  POL zone
        IOUNIT = 2       !  output units: meters
        TPARO( 1 ) = 0.0D0
        TPARO( 5 ) = DDDMMMSSS( P_GAME )
        TPARO( 6 ) = DDDMMMSSS( P_BETE )

        IF ( .NOT.SPHEREDAT( INSPH, TPAIN, TPARO ) ) THEN
            CALL M3WARN( 'MODGCTP/TRMERC',0,0,'Bad geodetic sphere info' )
        END IF

        CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,     &
                    IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,   &
                    TPARO, IOUNIT, LN27, LN83, FN27, FN83, LENGTH, IFLG )

        IF ( IFLG .NE. 0 ) THEN
            IFLG = MAX( MIN( 9, IFLG ), 1 )     !  between 1 and 9
            CALL M3WARN( 'MODGCTP/EQMERC', 0,0, GCTPMESG( IFLG ) )
            EQMERC = .FALSE.
            RETURN
        END IF

        !!.......   Decode output arguments for GTPZ0();
        !!.......   update PZONE for the new false-easting/false-northing offsets

        XCENTE =  CRDIO( 1 )
        YCENTE =  CRDIO( 2 )
        EZONE  =  EZONE + 5
        EQMERC = .TRUE.

        RETURN

    END FUNCTION EQMERC


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !!  Set up a particular named Lambert projection:
    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION ALBERS( CNAME, A, B, C, X, Y )

        CHARACTER*(*), INTENT(IN   ) :: CNAME
        REAL         , INTENT(  OUT) :: A, B, C, X, Y

        CHARACTER*16    ANAME
        CHARACTER*256   MESG
        INTEGER         CTYPE, NCOLS, NROWS, NTHIK
        REAL*8          XORIG, YORIG, XCELL, YCELL

        !!------------  begin body   ---------------------------------

        IF ( .NOT. INITSPHERES() ) THEN
            CALL M3WARN( 'MODGCTP/EQMERC',0,0,'Bad geodetic sphere info' )
        END IF

        IF ( DSCOORD( CNAME, CTYPE, P_ALPA, P_BETA, P_GAMA, XCENTA, YCENTA ) ) THEN
            CONTINUE
        ELSE IF ( DSCGRID( CNAME, ANAME, CTYPE,                     &
                           P_ALPA, P_BETA, P_GAMA, XCENTA, YCENTA,  &
                           XORIG, YORIG, XCELL, YCELL, NCOLS, NROWS, NTHIK ) ) THEN
            CONTINUE
        ELSE        !  dscgrid and dscoord both failed
            CALL M3WARN( 'MODGCTP/ALBERS', 0, 0, 'Projection not found in GRIDDESC' )
            ALBERS = .FALSE.
            RETURN
        END IF          !  if dscoord failed

        IF ( CTYPE .NE. ALBGRD3 ) THEN
            WRITE( MESG,'( A, I10, :, 2X )' ) 'Projection not TRMGRD3:  type ', CTYPE
            CALL M3WARN( 'MODGCTP/ALBERS', 0, 0, MESG )
            ALBERS = .FALSE.
            RETURN
        END IF

        !!.......   Return the projection parameters as REAL   A,B,C,X,Y:

        A = SNGL( P_ALPA )
        B = SNGL( P_BETA )
        C = SNGL( P_GAMA )
        X = SNGL( XCENTA )
        Y = SNGL( YCENTA )

        AZONE   = AZONE + 5

        ALBERS = .TRUE.
        RETURN

    END FUNCTION ALBERS


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !!  Set up a Lambert projection by angles:
    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

    LOGICAL FUNCTION SETLAM( A, B, C, X, Y )

        REAL, INTENT(IN   ) :: A, B, C, X, Y

        CHARACTER*256   MESG

        IF ( .NOT. INITSPHERES() ) THEN
            CALL M3WARN( 'MODGCTP/SETLAM',0,0,'Bad geodetic sphere' )
        END IF

        !!.......   Check validity of input parameters:

        IF ( A .LT. -90.0 ) THEN
            WRITE( MESG, '(A, 1PG14.5, :, 2X )' ) 'Bad first latitude A =', A
            CALL M3WARN( 'MODGCTP/SETLAM', 0, 0, MESG )
            SETLAM = .FALSE.
            RETURN
        ELSE IF ( A .GT. B ) THEN
            WRITE( MESG, '(A, 1PG14.5, :, 2X )' ) 'Bad latitudes A ', A, 'B =', B
            CALL M3WARN( 'MODGCTP/SETLAM', 0, 0, MESG )
            SETLAM = .FALSE.
            RETURN
        ELSE IF ( B .GE.   90.0 ) THEN
            WRITE( MESG, '(A, 1PG14.5, :, 2X )' ) 'Bad second latitude B =', B
            CALL M3WARN( 'MODGCTP/SETLAM', 0, 0, MESG )
            SETLAM = .FALSE.
            RETURN
        ELSE IF ( C .LT. -180.0 ) THEN
            WRITE( MESG, '(A, 1PG14.5, :, 2X )' ) 'Bad central longitude C =', C
            CALL M3WARN( 'MODGCTP/SETLAM', 0, 0, MESG )
            SETLAM = .FALSE.
            RETURN
        ELSE IF ( C .GT.  180.0 ) THEN
            WRITE( MESG, '(A, 1PG14.5, :, 2X )' ) 'Bad central longitude C =', C
            CALL M3WARN( 'MODGCTP/SETLAM', 0, 0, MESG )
            SETLAM = .FALSE.
            RETURN
        ELSE IF ( X .LT. -180.0 ) THEN
            WRITE( MESG, '(A, 1PG14.5, :, 2X )' ) 'Bad origin longitude X =', X
            CALL M3WARN( 'MODGCTP/SETLAM', 0, 0, MESG )
            SETLAM = .FALSE.
            RETURN
        ELSE IF ( X .GT.  180.0 ) THEN
            WRITE( MESG, '(A, 1PG14.5, :, 2X )' ) 'Bad origin longitude X =', X
            CALL M3WARN( 'MODGCTP/SETLAM', 0, 0, MESG )
            SETLAM = .FALSE.
            RETURN
        ELSE IF ( Y .LT. -90.0 ) THEN
            WRITE( MESG, '(A, 1PG14.5, :, 2X )' ) 'Bad origin latitude Y =', Y
            CALL M3WARN( 'MODGCTP/SETLAM', 0, 0, MESG )
            SETLAM = .FALSE.
            RETURN
        ELSE IF ( Y .GE.   90.0 ) THEN
            WRITE( MESG, '(A, 1PG14.5, :, 2X )' ) 'Bad origin latitude Y =', Y
            CALL M3WARN( 'MODGCTP/SETLAM', 0, 0, MESG )
            SETLAM = .FALSE.
            RETURN
        END IF

        !!.......   Convert to double

        P_ALPL  = DBLEFIX( A )
        P_BETL  = DBLEFIX( B )
        P_GAML  = DBLEFIX( C )
        XCENTL  = DBLEFIX( X )
        YCENTL  = DBLEFIX( Y )
        SETLAM = .TRUE.

        RETURN

    END FUNCTION SETLAM


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !!  Set up a Polar Stereographic projection by angles:
    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

    LOGICAL FUNCTION SETPOL( A, B, C, X, Y )

        REAL, INTENT(IN   ) :: A, B, C, X, Y

        CHARACTER*256   MESG

        IF ( .NOT. INITSPHERES() ) THEN
            CALL M3WARN( 'MODGCTP/SETPOL',0,0,'Bad geodetic sphere' )
        END IF

        !!.......   Check validity of input parameters:

        IF ( NINT( ABS( A ) ) .NE. 1 ) THEN
            WRITE( MESG, '(A, 1PG14.5, :, 2X )' ) 'Bad pole A =', A
            CALL M3WARN( 'MODGCTP/SETPOL', 0, 0, MESG )
            SETPOL = .FALSE.
            RETURN
        ELSE IF ( B .GT.   90.0 ) THEN
            WRITE( MESG, '(A, 1PG14.5, :, 2X )' ) 'Bad secant latitude B =', B
            CALL M3WARN( 'MODGCTP/SETPOL', 0, 0, MESG )
            SETPOL = .FALSE.
            RETURN
        ELSE IF ( B .LT.  0.0 ) THEN
            WRITE( MESG, '(A, 1PG14.5, :, 2X )' ) 'Bad secant latitude B =', B
            CALL M3WARN( 'MODGCTP/SETPOL', 0, 0, MESG )
            SETPOL = .FALSE.
            RETURN
        ELSE IF ( C .LT. -180.0 ) THEN
            WRITE( MESG, '(A, 1PG14.5, :, 2X )' ) 'Bad central longitude C =', C
            CALL M3WARN( 'MODGCTP/SETPOL', 0, 0, MESG )
            SETPOL = .FALSE.
            RETURN
        ELSE IF ( C .GT.  180.0 ) THEN
            WRITE( MESG, '(A, 1PG14.5, :, 2X )' ) 'Bad central longitude C =', C
            CALL M3WARN( 'MODGCTP/SETPOL', 0, 0, MESG )
            SETPOL = .FALSE.
            RETURN
        ELSE IF ( X .LT. -180.0 ) THEN
            WRITE( MESG, '(A, 1PG14.5, :, 2X )' ) 'Bad origin longitude X =', X
            CALL M3WARN( 'MODGCTP/SETPOL', 0, 0, MESG )
            SETPOL = .FALSE.
            RETURN
        ELSE IF ( X .GT.  180.0 ) THEN
            WRITE( MESG, '(A, 1PG14.5, :, 2X )' ) 'Bad origin longitude X =', X
            CALL M3WARN( 'MODGCTP/SETPOL', 0, 0, MESG )
            SETPOL = .FALSE.
            RETURN
        ELSE IF ( Y .LT.    0.0 ) THEN
            WRITE( MESG, '(A, 1PG14.5, :, 2X )' ) 'Bad origin latitude Y =', Y
            CALL M3WARN( 'MODGCTP/SETPOL', 0, 0, MESG )
            SETPOL = .FALSE.
            RETURN
        ELSE IF ( Y .GT.   90.0 ) THEN
            WRITE( MESG, '(A, 1PG14.5, :, 2X )' ) 'Bad origin latitude Y =', Y
            CALL M3WARN( 'MODGCTP/SETPOL', 0, 0, MESG )
            SETPOL = .FALSE.
            RETURN
        END IF

        !!.......   Convert to double

        P_ALPP  = DBLEFIX( A )
        P_BETP  = DBLEFIX( B )
        P_GAMP  = DBLEFIX( C )
        XCENTP  = DBLEFIX( X )
        YCENTP  = DBLEFIX( Y )

        !!.......    Convert <XCENT,YCENT> to POL-Cartesian offsets
        !!.......   Set up input arguments for GTPZ0():

        TPAIN = 0.0D0
        TPARO = 0.0D0

        CRDIN( 1 ) = XCENTP
        CRDIN( 2 ) = YCENTP
        INSYS  = 0       !  projection default (lat-lon)
        INUNIT = 4       !  input units:  degrees
        INSPH  = 8       !  GRS 1980 spheroid
        IPR    = 0       !  print error messages, if any
        JPR    = 1       !  do NOT print projection parameters
        LEMSG  = INIT3() !  unit number for log file
        LPARM  = LEMSG   !  projection parameters file
        IOSYS  = 6       !  Polar stereographic
        IOZONE = PZONE   !  POL zone
        IOUNIT = 2       !  output units: meters
        TPARO( 1 ) = 0.0D0
        TPARO( 5 ) = DDDMMMSSS( P_GAMP )
        TPARO( 6 ) = DDDMMMSSS( P_BETP )

        IF ( .NOT.SPHEREDAT( INSPH, TPAIN, TPARO ) ) THEN
            CALL M3WARN( 'MODGCTP/SETPOL',0,0,'Bad geodetic sphere info' )
        END IF

        CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,     &
                    IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,   &
                    TPARO, IOUNIT, LN27, LN83, FN27, FN83, LENGTH, IFLG )

        IF ( IFLG .NE. 0 ) THEN
            IFLG = MAX( MIN( 9, IFLG ), 1 )     !  between 1 and 9
            CALL M3WARN( 'MODGCTP/SETPOL', 0,0, GCTPMESG( IFLG ) )
            SETPOL = .FALSE.
            RETURN
        END IF

        !!.......   Decode output arguments for GTPZ0();
        !!.......   update PZONE for the new false-easting/false-northing offsets

        XCENTP = -CRDIO( 1 )
        YCENTP = -CRDIO( 2 )
        PZONE  =  PZONE + 5

        SETPOL = .TRUE.

        RETURN

    END FUNCTION SETPOL


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !!  Set up a particular named Lambert projection:
    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION SETTRM( A, B, C, X, Y )

        REAL, INTENT(IN   ) :: A, B, C, X, Y

        CHARACTER*256   MESG

        IF ( .NOT. INITSPHERES() ) THEN
            CALL M3WARN( 'MODGCTP/SETPOL',0,0,'Bad geodetic sphere' )
        END IF

        !!.......   Check validity of input parameters:

        IF ( ABS( A ) .GT. 90.0 ) THEN
            WRITE( MESG, '(A, 1PG14.5, :, 2X )' ) 'Bad origin latitude', A
            CALL M3WARN( 'MODGCTP/SETTRM', 0, 0, MESG )
            SETTRM = .FALSE.
            RETURN
        ELSE IF ( B .GT.   1.0 ) THEN
            WRITE( MESG, '(A, 1PG14.5, :, 2X )' ) 'Bad scale factor B =', B
            CALL M3WARN( 'MODGCTP/SETTRM', 0, 0, MESG )
            SETTRM = .FALSE.
            RETURN
        ELSE IF ( B .LE.  0.0 ) THEN
            WRITE( MESG, '(A, 1PG14.5, :, 2X )' ) 'Bad scale factor B =', B
            CALL M3WARN( 'MODGCTP/SETTRM', 0, 0, MESG )
            SETTRM = .FALSE.
            RETURN
        ELSE IF ( ABS( C ) .GT.  180.0 ) THEN
            WRITE( MESG, '(A, 1PG14.5, :, 2X )' ) 'Bad central longitude C =', C
            CALL M3WARN( 'MODGCTP/SETTRM', 0, 0, MESG )
            SETTRM = .FALSE.
            RETURN
        ELSE IF ( ABS( X ) .GT.  180.0 ) THEN
            WRITE( MESG, '(A, 1PG14.5, :, 2X )' ) 'Bad origin longitude X =', X
            CALL M3WARN( 'MODGCTP/SETTRM', 0, 0, MESG )
            SETTRM = .FALSE.
            RETURN
        ELSE IF ( Y .LT.    0.0 ) THEN
            WRITE( MESG, '(A, 1PG14.5, :, 2X )' ) 'Bad origin latitude Y =', Y
            CALL M3WARN( 'MODGCTP/SETTRM', 0, 0, MESG )
            SETTRM = .FALSE.
            RETURN
        ELSE IF ( Y .GE.   90.0 ) THEN
            WRITE( MESG, '(A, 1PG14.5, :, 2X )' ) 'Bad origin latitude Y =', Y
            CALL M3WARN( 'MODGCTP/SETTRM', 0, 0, MESG )
            SETTRM = .FALSE.
            RETURN
        END IF

        !!.......   Convert to double

        P_ALPT  = DBLEFIX( A )
        P_BETT  = DBLEFIX( B )
        P_GAMT  = DBLEFIX( C )
        XCENTT  = DBLEFIX( X )
        YCENTT  = DBLEFIX( Y )

        TZONE   = TZONE + 5

        !!.......    Convert <XCENT,YCENT> to POL-Cartesian offsets
        !!.......   Set up input arguments for GTPZ0():

        TPAIN = 0.0D0
        TPARO = 0.0D0

        CRDIN( 1 ) = XCENTT
        CRDIN( 2 ) = YCENTT
        INSYS  = 0       !  projection default (lat-lon)
        INUNIT = 4       !  input units:  degrees
        INSPH  = 8       !  GRS 1980 spheroid
        IPR    = 0       !  print error messages, if any
        JPR    = 1       !  do NOT print projection parameters
        LEMSG  = INIT3() !  unit number for log file
        LPARM  = LEMSG   !  projection parameters file
        IOSYS  = 6       !  Polar stereographic
        IOZONE = PZONE   !  POL zone
        IOUNIT = 2       !  output units: meters
        TPARO( 1 ) = 0.0D0
        TPARO( 5 ) = DDDMMMSSS( P_GAMT )
        TPARO( 6 ) = DDDMMMSSS( P_BETT )

        IF ( .NOT.SPHEREDAT( INSPH, TPAIN, TPARO ) ) THEN
            CALL M3WARN( 'MODGCTP/SETTRM',0,0,'Bad geodetic sphere info' )
        END IF

        CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,     &
                    IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,   &
                    TPARO, IOUNIT, LN27, LN83, FN27, FN83, LENGTH, IFLG )

        IF ( IFLG .NE. 0 ) THEN
            IFLG = MAX( MIN( 9, IFLG ), 1 )     !  between 1 and 9
            CALL M3WARN( 'MODGCTP/SETTRM', 0,0, GCTPMESG( IFLG ) )
            SETTRM = .FALSE.
            RETURN
        END IF

        !!.......   Decode output arguments for GTPZ0();
        !!.......   update ZONE for the new false-easting/false-northing offsets

        XCENTT =  CRDIO( 1 )
        YCENTT =  CRDIO( 2 )
        TZONE  =  TZONE + 5
        SETTRM = .TRUE.
        RETURN

    END FUNCTION SETTRM


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !!  Set up a particular named Lambert projection:
    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION SETEQM( A, B, C, X, Y )

        REAL, INTENT(IN   ) :: A, B, C, X, Y

        CHARACTER*256   MESG

        IF ( .NOT. INITSPHERES() ) THEN
            CALL M3WARN( 'MODGCTP/SETPOL',0,0,'Bad geodetic sphere' )
        END IF

        !!.......   Check validity of input parameters:

        IF ( NINT( ABS( A ) ) .NE. 1 ) THEN
            WRITE( MESG, '(A, 1PG14.5, :, 2X )' ) 'Bad pole A =', A
            CALL M3WARN( 'MODGCTP/SETEQM', 0, 0, MESG )
            SETEQM = .FALSE.
            RETURN
        ELSE IF ( C .LT. -180.0 ) THEN
            WRITE( MESG, '(A, 1PG14.5, :, 2X )' ) 'Bad central longitude C =', C
            CALL M3WARN( 'MODGCTP/SETEQM', 0, 0, MESG )
            SETEQM = .FALSE.
            RETURN
        ELSE IF ( C .GT.  180.0 ) THEN
            WRITE( MESG, '(A, 1PG14.5, :, 2X )' ) 'Bad central longitude C =', C
            CALL M3WARN( 'MODGCTP/SETEQM', 0, 0, MESG )
            SETEQM = .FALSE.
            RETURN
        ELSE IF ( X .LT. -180.0 ) THEN
            WRITE( MESG, '(A, 1PG14.5, :, 2X )' ) 'Bad origin longitude X =', X
            CALL M3WARN( 'MODGCTP/SETEQM', 0, 0, MESG )
            SETEQM = .FALSE.
            RETURN
        ELSE IF ( X .GT.  180.0 ) THEN
            WRITE( MESG, '(A, 1PG14.5, :, 2X )' ) 'Bad origin longitude X =', X
            CALL M3WARN( 'MODGCTP/SETEQM', 0, 0, MESG )
            SETEQM = .FALSE.
            RETURN
        ELSE IF ( Y .LT.    0.0 ) THEN
            WRITE( MESG, '(A, 1PG14.5, :, 2X )' ) 'Bad origin latitude Y =', Y
            CALL M3WARN( 'MODGCTP/SETEQM', 0, 0, MESG )
            SETEQM = .FALSE.
            RETURN
        ELSE IF ( Y .GE.   90.0 ) THEN
            WRITE( MESG, '(A, 1PG14.5, :, 2X )' ) 'Bad origin latitude Y =', Y
            CALL M3WARN( 'MODGCTP/SETEQM', 0, 0, MESG )
            SETEQM = .FALSE.
            RETURN
        END IF

        !!.......   Convert to double

        P_ALPE  = DBLEFIX( A )
        P_BETE  = DBLEFIX( B )
        P_GAME  = DBLEFIX( C )
        XCENTE  = DBLEFIX( X )
        YCENTE  = DBLEFIX( Y )

        EZONE   = EZONE + 5

        !!.......    Convert <XCENT,YCENT> to POL-Cartesian offsets
        !!.......   Set up input arguments for GTPZ0():

        TPAIN = 0.0D0
        TPARO = 0.0D0

        CRDIN( 1 ) = XCENTT
        CRDIN( 2 ) = YCENTT
        INSYS  = 0       !  projection default (lat-lon)
        INUNIT = 4       !  input units:  degrees
        INSPH  = 8       !  GRS 1980 spheroid
        IPR    = 0       !  print error messages, if any
        JPR    = 1       !  do NOT print projection parameters
        LEMSG  = INIT3() !  unit number for log file
        LPARM  = LEMSG   !  projection parameters file
        IOSYS  = 6       !  Polar stereographic
        IOZONE = PZONE   !  POL zone
        IOUNIT = 2       !  output units: meters
        TPARO( 1 ) = 0.0D0
        TPARO( 5 ) = DDDMMMSSS( P_GAME )
        TPARO( 6 ) = DDDMMMSSS( P_BETE )

        IF ( .NOT.SPHEREDAT( INSPH, TPAIN, TPARO ) ) THEN
            CALL M3WARN( 'MODGCTP/TRMERC',0,0,'Bad geodetic sphere info' )
        END IF

        CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,     &
                    IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,   &
                    TPARO, IOUNIT, LN27, LN83, FN27, FN83, LENGTH, IFLG )

        IF ( IFLG .NE. 0 ) THEN
            IFLG = MAX( MIN( 9, IFLG ), 1 )     !  between 1 and 9
            CALL M3WARN( 'MODGCTP/SETEQM', 0,0, GCTPMESG( IFLG ) )
            SETEQM = .FALSE.
            RETURN
        END IF

        !!.......   Decode output arguments for GTPZ0();
        !!.......   update PZONE for the new false-easting/false-northing offsets

        XCENTE =  CRDIO( 1 )
        YCENTE =  CRDIO( 2 )
        EZONE  =  EZONE + 5

        RETURN

    END FUNCTION SETEQM


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !!  Set up a particular named Lambert projection:
    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION SETALB( A, B, C, X, Y )

        REAL, INTENT(IN   ) :: A, B, C, X, Y

        CHARACTER*256   MESG

        IF ( .NOT. INITSPHERES() ) THEN
            CALL M3WARN( 'MODGCTP/SETPOL',0,0,'Bad geodetic sphere' )
        END IF

        !!.......   Check validity of input parameters:

        IF ( A .LT. -90.0 ) THEN
            WRITE( MESG, '(A, 1PG14.5, :, 2X )' ) 'Bad first latitude A =', A
            CALL M3WARN( 'MODGCTP/SETALB', 0, 0, MESG )
            SETALB = .FALSE.
            RETURN
        ELSE IF ( A .GT. B ) THEN
            WRITE( MESG, '(A, 1PG14.5, :, 2X )' ) 'Bad latitudes A ', A, 'B =', B
            CALL M3WARN( 'MODGCTP/SETALB', 0, 0, MESG )
            SETALB = .FALSE.
            RETURN
        ELSE IF ( B .GE.   90.0 ) THEN
            WRITE( MESG, '(A, 1PG14.5, :, 2X )' ) 'Bad second latitude B =', B
            CALL M3WARN( 'MODGCTP/SETALB', 0, 0, MESG )
            SETALB = .FALSE.
            RETURN
        ELSE IF ( C .LT. -180.0 ) THEN
            WRITE( MESG, '(A, 1PG14.5, :, 2X )' ) 'Bad central longitude C =', C
            CALL M3WARN( 'MODGCTP/SETALB', 0, 0, MESG )
            SETALB = .FALSE.
            RETURN
        ELSE IF ( C .GT.  180.0 ) THEN
            WRITE( MESG, '(A, 1PG14.5, :, 2X )' ) 'Bad central longitude C =', C
            CALL M3WARN( 'MODGCTP/SETALB', 0, 0, MESG )
            SETALB = .FALSE.
            RETURN
        ELSE IF ( X .LT. -180.0 ) THEN
            WRITE( MESG, '(A, 1PG14.5, :, 2X )' ) 'Bad origin longitude X =', X
            CALL M3WARN( 'MODGCTP/SETALB', 0, 0, MESG )
            SETALB = .FALSE.
            RETURN
        ELSE IF ( X .GT.  180.0 ) THEN
            WRITE( MESG, '(A, 1PG14.5, :, 2X )' ) 'Bad origin longitude X =', X
            CALL M3WARN( 'MODGCTP/SETALB', 0, 0, MESG )
            SETALB = .FALSE.
            RETURN
        ELSE IF ( Y .LT. -90.0 ) THEN
            WRITE( MESG, '(A, 1PG14.5, :, 2X )' ) 'Bad origin latitude Y =', Y
            CALL M3WARN( 'MODGCTP/SETALB', 0, 0, MESG )
            SETALB = .FALSE.
            RETURN
        ELSE IF ( Y .GE.   90.0 ) THEN
            WRITE( MESG, '(A, 1PG14.5, :, 2X )' ) 'Bad origin latitude Y =', Y
            CALL M3WARN( 'MODGCTP/SETALB', 0, 0, MESG )
            SETALB = .FALSE.
            RETURN
        END IF

        !!.......   Convert to double

        P_ALPA  = DBLEFIX( A )
        P_BETA  = DBLEFIX( B )
        P_GAMA  = DBLEFIX( C )
        XCENTA  = DBLEFIX( X )
        YCENTA  = DBLEFIX( Y )
        AZONE   = AZONE + 5
        SETALB  = .TRUE.

        RETURN

    END FUNCTION SETALB


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !!  Set up a particular named Lambert projection:
    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION LL2LAM( LON, LAT, X, Y )

        REAL, INTENT(IN   ) :: LON, LAT
        REAL, INTENT(  OUT) :: X, Y

        REAL*8  XX, YY, UU, VV

        IF ( LZONE .LT. 64 ) THEN
            CALL M3WARN( 'MODGCTP/LL2LAM', 0, 0, 'LAMBERT Projection not initialized' )
            LL2LAM = .FALSE.
            RETURN
        END IF

        XX = LON
        YY = LAT
        CALL XY2XY( LATGRD3,  0.0D0,  0.0D0,  0.0D0,  0.0D0,  0.0D0,     &
                    LAMGRD3, P_ALPL, P_BETL, P_GAML, XCENTL, YCENTL,     &
                    XX, YY, UU, VV )
        X = UU
        Y = VV
        LL2LAM = .TRUE.

        RETURN

    END FUNCTION LL2LAM


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION LAM2LL( X, Y, LON, LAT )

        REAL, INTENT(IN   ) :: X, Y
        REAL, INTENT(  OUT) :: LON, LAT

        REAL*8  XX, YY, UU, VV

        IF ( LZONE .LT. 64 ) THEN
            CALL M3WARN( 'MODGCTP/LAM2LL', 0, 0, 'LAMBERT Projection not initialized' )
            LAM2LL = .FALSE.
            RETURN
        END IF

        XX = X
        YY = Y
        CALL XY2XY( LAMGRD3, P_ALPL, P_BETL, P_GAML, XCENTL, YCENTL,     &
                    LATGRD3,  0.0D0,  0.0D0,  0.0D0,  0.0D0,  0.0D0,     &
                    XX, YY, UU, VV )
        LON = UU
        LAT = VV

        LAM2LL = .TRUE.
        RETURN

    END FUNCTION LAM2LL


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE UTM2LLI( U, V, Z, X, Y )

        REAL   , INTENT(IN   ) :: U, V
        INTEGER, INTENT(IN   ) :: Z
        REAL   , INTENT(  OUT) :: X, Y

        REAL*8  XX, YY, UU, VV, ZZ

        XX = U
        YY = V
        ZZ    = DBLE( Z )
        CALL XY2XY( UTMGRD3,     ZZ, 0.0D0,  0.0D0,  0.0D0,  0.0D0,     &
                    LATGRD3,  0.0D0, 0.0D0,  0.0D0,  0.0D0,  0.0D0,     &
                    XX, YY, UU, VV )
        X = UU
        Y = VV

        RETURN

    END SUBROUTINE UTM2LLI


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE UTM2LLR( U, V, Z, X, Y )

        REAL   , INTENT(IN   ) :: U, V
        REAL   , INTENT(IN   ) :: Z
        REAL   , INTENT(  OUT) :: X, Y

        REAL*8  XX, YY, UU, VV, ZZ

        XX = U
        YY = V
        ZZ    = DBLE( Z )
        CALL XY2XY( UTMGRD3,     ZZ, 0.0D0,  0.0D0,  0.0D0,  0.0D0,     &
                    LATGRD3,  0.0D0, 0.0D0,  0.0D0,  0.0D0,  0.0D0,     &
                    XX, YY, UU, VV )
        X = UU
        Y = VV

        RETURN

    END SUBROUTINE UTM2LLR


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE LL2UTMI( X, Y, Z, U, V )

        REAL   , INTENT(IN   ) :: X, Y
        INTEGER, INTENT(IN   ) :: Z
        REAL   , INTENT(  OUT) :: U, V

        REAL*8  XX, YY, UU, VV, ZZ

        XX = X
        YY = Y
        ZZ    = DBLE( Z )
        CALL XY2XY( LATGRD3, 0.0D0, 0.0D0,  0.0D0,  0.0D0,  0.0D0,     &
                    UTMGRD3,    ZZ, 0.0D0,  0.0D0,  0.0D0,  0.0D0,     &
                    XX, YY, UU, VV )
        U = UU
        V = VV
        RETURN

    END SUBROUTINE LL2UTMI


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE LL2UTMR( X, Y, Z, U, V )

        REAL   , INTENT(IN   ) :: X, Y
        REAL   , INTENT(IN   ) :: Z
        REAL   , INTENT(  OUT) :: U, V

        REAL*8  XX, YY, UU, VV, ZZ

        XX = X
        YY = Y
        ZZ    = DBLE( Z )
        CALL XY2XY( LATGRD3, 0.0D0, 0.0D0,  0.0D0,  0.0D0,  0.0D0,     &
                    UTMGRD3,    ZZ, 0.0D0,  0.0D0,  0.0D0,  0.0D0,     &
                    XX, YY, UU, VV )
        U = UU
        V = VV
        RETURN

    END SUBROUTINE LL2UTMR


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION UTM2LAM( U, V, Z, X, Y )

        REAL   , INTENT(IN   ) :: U, V
        INTEGER, INTENT(IN   ) :: Z
        REAL   , INTENT(  OUT) :: X, Y

        REAL*8  XX, YY, UU, VV, ZZ

        IF ( LZONE .LT. 64 ) THEN
            CALL M3WARN( 'MODGCTP/UTM2LAM', 0, 0, 'LAMBERT Projection not initialized' )
            UTM2LAM = .FALSE.
            RETURN
        END IF

        XX = U
        YY = V
        ZZ    = DBLE( Z )
        CALL XY2XY( UTMGRD3,     ZZ,  0.0D0,  0.0D0,  0.0D0,  0.0D0,     &
                    LAMGRD3, P_ALPL, P_BETL, P_GAML, XCENTL, YCENTL,     &
                    XX, YY, UU, VV )
        X = UU
        Y = VV

        UTM2LAM = .TRUE.
        RETURN

    END FUNCTION UTM2LAM


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION LAM2UTM( X, Y, Z, U, V )

        REAL   , INTENT(IN   ) :: X, Y
        INTEGER, INTENT(IN   ) :: Z
        REAL   , INTENT(  OUT) :: U, V

        REAL*8  XX, YY, UU, VV, ZZ

        IF ( LZONE .LT. 64 ) THEN
            CALL M3WARN( 'MODGCTP/LAM2UTM', 0, 0, 'Lambert Projection not initialized' )
            LAM2UTM = .FALSE.
            RETURN
        END IF

        XX = X
        YY = Y
        ZZ    = DBLE( Z )
        CALL XY2XY( LAMGRD3, P_ALPL, P_BETL, P_GAML, XCENTL, YCENTL,     &
                    UTMGRD3,     ZZ,  0.0D0,  0.0D0,  0.0D0,  0.0D0,     &
                    XX, YY, UU, VV )
        U = UU
        V = VV

        LAM2UTM = .TRUE.
        RETURN

    END FUNCTION LAM2UTM


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION LL2POL( LON, LAT, X, Y )

        REAL, INTENT(IN   ) :: LON, LAT
        REAL, INTENT(  OUT) :: X, Y

        REAL*8  XX, YY, UU, VV

        IF ( PZONE .LT. 64 ) THEN
            CALL M3WARN( 'LAMBERT/LL2POL', 0, 0, 'Polar projection not initialized' )
            LL2POL = .FALSE.
            RETURN
        END IF

        XX = LON
        YY = LAT
        CALL XY2XY( LATGRD3,  0.0D0,  0.0D0,  0.0D0,  0.0D0,  0.0D0,     &
                    POLGRD3, P_ALPP, P_BETP, P_GAMP, XCENTP, YCENTP,     &
                    XX, YY, UU, VV )
        X = UU
        Y = VV

        LL2POL = .TRUE.
        RETURN

    END FUNCTION LL2POL


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION POL2LL( X, Y, LON, LAT )

        REAL, INTENT(IN   ) :: X, Y
        REAL, INTENT(  OUT) :: LON, LAT

        REAL*8  XX, YY, UU, VV

        IF ( PZONE .LT. 64 ) THEN
            CALL M3WARN( 'LAMBERT/POL2LL', 0, 0, 'Polar projection not initialized' )
            POL2LL = .FALSE.
            RETURN
        END IF

        XX = X
        YY = Y
        CALL XY2XY( POLGRD3, P_ALPP, P_BETP, P_GAMP, XCENTP, YCENTP,     &
                    LATGRD3,  0.0D0,  0.0D0,  0.0D0,  0.0D0,  0.0D0,     &
                    XX, YY, UU, VV )
        LON = UU
        LAT = VV

        POL2LL = .TRUE.
        RETURN

    END FUNCTION POL2LL


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION POL2LAM( X, Y, U, V )

        REAL, INTENT(IN   ) :: X, Y
        REAL, INTENT(  OUT) :: U, V

        REAL*8  XX, YY, UU, VV

        IF ( LZONE .LT. 64 ) THEN
            CALL M3WARN( 'MODGCTP/POL2LAM', 0, 0, 'Lambert Projection not initialized' )
            POL2LAM = .FALSE.
            RETURN
        END IF

        IF ( PZONE .LT. 64 ) THEN
            CALL M3WARN( 'LAMBERT/POL2LAM', 0, 0, 'Polar projection not initialized' )
            POL2LAM = .FALSE.
            RETURN
        END IF

        XX = X
        YY = Y
        CALL XY2XY( POLGRD3, P_ALPP, P_BETP, P_GAMP, XCENTP, YCENTP,     &
                    LAMGRD3, P_ALPL, P_BETL, P_GAML, XCENTL, YCENTL,     &
                    XX, YY, UU, VV )
        U = UU
        V = VV

        POL2LAM = .TRUE.
        RETURN

    END FUNCTION POL2LAM


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION LAM2POL( X, Y, U, V )

        REAL, INTENT(IN   ) :: X, Y
        REAL, INTENT(  OUT) :: U, V

        REAL*8  XX, YY, UU, VV

        IF ( LZONE .LT. 64 ) THEN
            CALL M3WARN( 'MODGCTP/LAM2POL', 0, 0, 'Lambert Projection not initialized' )
            LAM2POL = .FALSE.
            RETURN
        END IF

        IF ( PZONE .LT. 64 ) THEN
            CALL M3WARN( 'LAMBERT/LAM2POL', 0, 0, 'Polar projection not initialized' )
            LAM2POL = .FALSE.
            RETURN
        END IF

        XX = X
        YY = Y
        CALL XY2XY( LAMGRD3, P_ALPL, P_BETL, P_GAML, XCENTL, YCENTL,     &
                    POLGRD3, P_ALPP, P_BETP, P_GAMP, XCENTP, YCENTP,     &
                    XX, YY, UU, VV )
        U = UU
        V = VV

        LAM2POL = .TRUE.
        RETURN

    END FUNCTION LAM2POL


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION UTM2POL( U, V, Z, X, Y )

        REAL   , INTENT(IN   ) :: U, V
        INTEGER, INTENT(IN   ) :: Z
        REAL   , INTENT(  OUT) :: X, Y

        REAL*8  XX, YY, UU, VV, ZZ

        IF ( PZONE .LT. 64 ) THEN
            CALL M3WARN( 'MODGCTP/UTM2POL', 0, 0, 'POLAR Projection not initialized' )
            UTM2POL = .FALSE.
            RETURN
        END IF

        XX = U
        YY = V
        ZZ    = DBLE( Z )
        CALL XY2XY( UTMGRD3,     ZZ,  0.0D0,  0.0D0,  0.0D0,  0.0D0,     &
                    POLGRD3, P_ALPP, P_BETP, P_GAMP, XCENTP, YCENTP,     &
                    XX, YY, UU, VV )
        X = UU
        Y = VV

        UTM2POL = .TRUE.
        RETURN

    END FUNCTION UTM2POL


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION POL2UTM( U, V, Z, X, Y )

        REAL   , INTENT(IN   ) :: U, V
        INTEGER, INTENT(IN   ) :: Z
        REAL   , INTENT(  OUT) :: X, Y

        REAL*8  XX, YY, UU, VV, ZZ

        IF ( PZONE .LT. 64 ) THEN
            CALL M3WARN( 'MODGCTP/POL2UTM', 0, 0, 'POLAR Projection not initialized' )
            POL2UTM = .FALSE.
            RETURN
        END IF

        XX = U
        YY = V
        ZZ    = DBLE( Z )
        CALL XY2XY( POLGRD3, P_ALPP, P_BETP, P_GAMP, XCENTP, YCENTP,     &
                    UTMGRD3,     ZZ,  0.0D0,  0.0D0,  0.0D0,  0.0D0,     &
                    XX, YY, UU, VV )
        X = UU
        Y = VV

        POL2UTM = .TRUE.
        RETURN

    END FUNCTION POL2UTM


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION TRM2LL( X, Y, LON, LAT )

        REAL, INTENT(IN   ) :: X, Y
        REAL, INTENT(  OUT) :: LON, LAT

        REAL*8  XX, YY, UU, VV

        IF ( TZONE .LT. 64 ) THEN
            CALL M3WARN( 'MODGCTP/TRM2LL', 0, 0, 'TRM Projection not initialized' )
            TRM2LL = .FALSE.
            RETURN
        END IF

        XX = X
        YY = Y
        CALL XY2XY( TRMGRD3, P_ALPT, P_BETT, P_GAMT, XCENTT, YCENTT,     &
                    LATGRD3,  0.0D0,  0.0D0,  0.0D0,  0.0D0,  0.0D0,     &
                    XX, YY, UU, VV )
        LON = UU
        LAT = VV

        TRM2LL = .TRUE.
        RETURN

    END FUNCTION TRM2LL


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION LL2TRM( LON, LAT, X, Y )

        REAL, INTENT(IN   ) :: LON, LAT
        REAL, INTENT(  OUT) :: X, Y

        REAL*8  XX, YY, UU, VV

        IF ( TZONE .LT. 64 ) THEN
            CALL M3WARN( 'LAMBERT/LL2TRM', 0, 0, 'TRM projection not initialized' )
            LL2TRM = .FALSE.
            RETURN
        END IF

        XX = LON
        YY = LAT
        CALL XY2XY( LATGRD3,  0.0D0,  0.0D0,  0.0D0,  0.0D0,  0.0D0,     &
                    TRMGRD3, P_ALPT, P_BETT, P_GAMT, XCENTT, YCENTT,     &
                    XX, YY, UU, VV )
        X = UU
        Y = VV
        LL2TRM = .TRUE.
        RETURN

    END FUNCTION LL2TRM


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION TRM2LAM( X, Y, U, V )

        REAL, INTENT(IN   ) :: X, Y
        REAL, INTENT(  OUT) :: U, V

        REAL*8  XX, YY, UU, VV

        IF ( LZONE .LT. 64 ) THEN
            CALL M3WARN( 'MODGCTP/TRM2LAM', 0, 0, 'Lambert Projection not initialized' )
            TRM2LAM = .FALSE.
            RETURN
        END IF

        IF ( TZONE .LT. 64 ) THEN
            CALL M3WARN( 'LAMBERT/TRM2LAM', 0, 0, 'TRM projection not initialized' )
            TRM2LAM = .FALSE.
            RETURN
        END IF

        XX = X
        YY = Y
        CALL XY2XY( TRMGRD3, P_ALPT, P_BETT, P_GAMT, XCENTT, YCENTT,     &
                    LAMGRD3, P_ALPL, P_BETL, P_GAML, XCENTL, YCENTL,     &
                    XX, YY, UU, VV )
        U = UU
        V = VV

        TRM2LAM = .TRUE.

        RETURN

    END FUNCTION TRM2LAM


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION LAM2TRM( X, Y, U, V )

        REAL, INTENT(IN   ) :: X, Y
        REAL, INTENT(  OUT) :: U, V

        REAL*8  XX, YY, UU, VV

        IF ( LZONE .LT. 64 ) THEN
            CALL M3WARN( 'MODGCTP/LAM2TRM', 0, 0, 'Lambert Projection not initialized' )
            LAM2TRM = .FALSE.
            RETURN
        END IF

        IF ( TZONE .LT. 64 ) THEN
            CALL M3WARN( 'LAMBERT/LAM2TRM', 0, 0, 'TRM projection not initialized' )
            LAM2TRM = .FALSE.
            RETURN
        END IF

        XX = X
        YY = Y
        CALL XY2XY( LAMGRD3, P_ALPL, P_BETL, P_GAML, XCENTL, YCENTL,     &
                    TRMGRD3, P_ALPT, P_BETT, P_GAMT, XCENTT, YCENTT,     &
                    XX, YY, UU, VV )
        U = UU
        V = VV

        LAM2TRM = .TRUE.

        RETURN

    END FUNCTION LAM2TRM


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION TRM2UTM( U, V, Z, X, Y )

        REAL   , INTENT(IN   ) :: U, V
        INTEGER, INTENT(IN   ) :: Z
        REAL   , INTENT(  OUT) :: X, Y

        REAL*8  XX, YY, UU, VV, ZZ

        IF ( TZONE .LT. 64 ) THEN
            CALL M3WARN( 'MODGCTP/TRM2UTM', 0, 0, 'TRM Projection not initialized' )
            TRM2UTM = .FALSE.
            RETURN
        END IF

        XX = U
        YY = V
        ZZ    = DBLE( Z )
        CALL XY2XY( TRMGRD3, P_ALPT, P_BETT, P_GAMT, XCENTT, YCENTT,     &
                    UTMGRD3,     ZZ,  0.0D0,  0.0D0,  0.0D0,  0.0D0,     &
                    XX, YY, UU, VV )
        X = UU
        Y = VV

        TRM2UTM = .TRUE.

        RETURN

    END FUNCTION TRM2UTM


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION UTM2TRM( U, V, Z, X, Y )

        REAL   , INTENT(IN   ) :: U, V
        INTEGER, INTENT(IN   ) :: Z
        REAL   , INTENT(  OUT) :: X, Y

        REAL*8  XX, YY, UU, VV, ZZ

        IF ( TZONE .LT. 64 ) THEN
            CALL M3WARN( 'MODGCTP/UTM2TRM', 0, 0, 'TRM Projection not initialized' )
            UTM2TRM = .FALSE.
            RETURN
        END IF

        XX = U
        YY = V
        ZZ    = DBLE( Z )
        CALL XY2XY( UTMGRD3,     ZZ,  0.0D0,  0.0D0,  0.0D0,  0.0D0,     &
                    TRMGRD3, P_ALPT, P_BETT, P_GAMT, XCENTT, YCENTT,     &
                    XX, YY, UU, VV )
        X = UU
        Y = VV

        UTM2TRM = .TRUE.

        RETURN

    END FUNCTION UTM2TRM


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION TRM2POL( X, Y, U, V )

        REAL, INTENT(IN   ) :: X, Y
        REAL, INTENT(  OUT) :: U, V

        REAL*8  XX, YY, UU, VV

        IF ( PZONE .LT. 64 ) THEN
            CALL M3WARN( 'MODGCTP/TRM2POL', 0, 0, 'POLAR Projection not initialized' )
            TRM2POL = .FALSE.
            RETURN
        END IF

        IF ( TZONE .LT. 64 ) THEN
            CALL M3WARN( 'LAMBERT/TRM2POL', 0, 0, 'TRM projection not initialized' )
            TRM2POL = .FALSE.
            RETURN
        END IF

        XX = X
        YY = Y
        CALL XY2XY( TRMGRD3, P_ALPT, P_BETT, P_GAMT, XCENTT, YCENTT,     &
                    POLGRD3, P_ALPP, P_BETP, P_GAMP, XCENTP, YCENTP,     &
                    XX, YY, UU, VV )
        U = UU
        V = VV

        TRM2POL = .TRUE.

        RETURN

    END FUNCTION TRM2POL


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION POL2TRM( X, Y, U, V )

        REAL, INTENT(IN   ) :: X, Y
        REAL, INTENT(  OUT) :: U, V

        REAL*8  XX, YY, UU, VV

        IF ( PZONE .LT. 64 ) THEN
            CALL M3WARN( 'MODGCTP/POL2TRM', 0, 0, 'POLAR Projection not initialized' )
            POL2TRM = .FALSE.
            RETURN
        END IF

        IF ( TZONE .LT. 64 ) THEN
            CALL M3WARN( 'LAMBERT/POL2TRM', 0, 0, 'TRM projection not initialized' )
            POL2TRM = .FALSE.
            RETURN
        END IF

        XX = X
        YY = Y
        CALL XY2XY( POLGRD3, P_ALPP, P_BETP, P_GAMP, XCENTP, YCENTP,     &
                    TRMGRD3, P_ALPT, P_BETT, P_GAMT, XCENTT, YCENTT,     &
                    XX, YY, UU, VV )
        U = UU
        V = VV

        POL2TRM = .TRUE.

        RETURN

    END FUNCTION POL2TRM


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION EQM2LL( X, Y, LON, LAT )

        REAL, INTENT(IN   ) :: X, Y
        REAL, INTENT(  OUT) :: LON, LAT

        REAL*8  XX, YY, UU, VV

        IF ( EZONE .LT. 64 ) THEN
            CALL M3WARN( 'MODGCTP/EQM2LL', 0, 0, 'EQM Projection not initialized' )
            EQM2LL = .FALSE.
            RETURN
        END IF

        XX = X
        YY = Y
        CALL XY2XY( EQMGRD3, P_ALPE, P_BETE, P_GAME, XCENTE, YCENTE,     &
                    LATGRD3,  0.0D0,  0.0D0,  0.0D0,  0.0D0,  0.0D0,     &
                    XX, YY, UU, VV )
        LON = UU
        LAT = VV

        EQM2LL = .TRUE.
        RETURN

        RETURN

    END FUNCTION EQM2LL


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION LL2EQM( LON, LAT, X, Y )

        REAL, INTENT(IN   ) :: LON, LAT
        REAL, INTENT(  OUT) :: X, Y

        REAL*8  XX, YY, UU, VV

        IF ( EZONE .LT. 64 ) THEN
            CALL M3WARN( 'LAMBERT/LL2EQM', 0, 0, 'EQM projection not initialized' )
            LL2EQM = .FALSE.
            RETURN
        END IF

        XX = LON
        YY = LAT
        CALL XY2XY( LATGRD3,  0.0D0,  0.0D0,  0.0D0,  0.0D0,  0.0D0,     &
                    EQMGRD3, P_ALPE, P_BETE, P_GAME, XCENTE, YCENTE,     &
                    XX, YY, UU, VV )
        X = UU
        Y = VV
        LL2EQM = .TRUE.
        RETURN

        RETURN

    END FUNCTION LL2EQM


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION EQM2LAM( X, Y, U, V )

        REAL, INTENT(IN   ) :: X, Y
        REAL, INTENT(  OUT) :: U, V

        REAL*8  XX, YY, UU, VV

        IF ( LZONE .LT. 64 ) THEN
            CALL M3WARN( 'MODGCTP/EQM2LAM', 0, 0, 'Lambert Projection not initialized' )
            EQM2LAM = .FALSE.
            RETURN
        END IF

        IF ( EZONE .LT. 64 ) THEN
            CALL M3WARN( 'LAMBERT/EQM2LAM', 0, 0, 'EQM projection not initialized' )
            EQM2LAM = .FALSE.
            RETURN
        END IF

        XX = X
        YY = Y
        CALL XY2XY( EQMGRD3, P_ALPE, P_BETE, P_GAME, XCENTE, YCENTE,     &
                    LAMGRD3, P_ALPL, P_BETL, P_GAML, XCENTL, YCENTL,     &
                    XX, YY, UU, VV )
        U = UU
        V = VV

        EQM2LAM = .TRUE.
        RETURN

    END FUNCTION EQM2LAM


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION LAM2EQM( X, Y, U, V )

        REAL, INTENT(IN   ) :: X, Y
        REAL, INTENT(  OUT) :: U, V

        REAL*8  XX, YY, UU, VV

        IF ( LZONE .LT. 64 ) THEN
            CALL M3WARN( 'MODGCTP/LAM2EQM', 0, 0, 'Lambert Projection not initialized' )
            LAM2EQM = .FALSE.
            RETURN
        END IF

        IF ( EZONE .LT. 64 ) THEN
            CALL M3WARN( 'LAMBERT/LAM2EQM', 0, 0, 'EQM projection not initialized' )
            LAM2EQM = .FALSE.
            RETURN
        END IF

        XX = X
        YY = Y
        CALL XY2XY( LAMGRD3, P_ALPL, P_BETL, P_GAML, XCENTL, YCENTL,     &
                    EQMGRD3, P_ALPE, P_BETE, P_GAME, XCENTE, YCENTE,     &
                    XX, YY, UU, VV )
        U = UU
        V = VV

        LAM2EQM = .TRUE.

        RETURN

    END FUNCTION LAM2EQM


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION EQM2UTM( U, V, Z, X, Y )

        REAL   , INTENT(IN   ) :: U, V
        INTEGER, INTENT(IN   ) :: Z
        REAL   , INTENT(  OUT) :: X, Y

        REAL*8  XX, YY, UU, VV, ZZ

        IF ( EZONE .LT. 64 ) THEN
            CALL M3WARN( 'MODGCTP/EQM2UTM', 0, 0, 'EQM Projection not initialized' )
            EQM2UTM = .FALSE.
            RETURN
        END IF

        XX = U
        YY = V
        ZZ    = DBLE( Z )
        CALL XY2XY( EQMGRD3, P_ALPE, P_BETE, P_GAME, XCENTE, YCENTE,     &
                    UTMGRD3,     ZZ,  0.0D0,  0.0D0,  0.0D0,  0.0D0,     &
                    XX, YY, UU, VV )
        X = UU
        Y = VV

        EQM2UTM = .TRUE.

        RETURN

    END FUNCTION EQM2UTM


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION UTM2EQM( U, V, Z, X, Y )

        REAL   , INTENT(IN   ) :: U, V
        INTEGER, INTENT(IN   ) :: Z
        REAL   , INTENT(  OUT) :: X, Y

        REAL*8  XX, YY, UU, VV, ZZ

        IF ( EZONE .LT. 64 ) THEN
            CALL M3WARN( 'MODGCTP/UTM2EQM', 0, 0, 'EQM Projection not initialized' )
            UTM2EQM = .FALSE.
            RETURN
        END IF

        XX = U
        YY = V
        ZZ    = DBLE( Z )
        CALL XY2XY( UTMGRD3,     ZZ,  0.0D0,  0.0D0,  0.0D0,  0.0D0,     &
                    EQMGRD3, P_ALPE, P_BETE, P_GAME, XCENTE, YCENTE,     &
                    XX, YY, UU, VV )
        X = UU
        Y = VV

        UTM2EQM = .TRUE.

        RETURN

    END FUNCTION UTM2EQM


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION EQM2TRM( X, Y, U, V )

        REAL, INTENT(IN   ) :: X, Y
        REAL, INTENT(  OUT) :: U, V

        REAL*8  XX, YY, UU, VV

        IF ( TZONE .LT. 64 ) THEN
            CALL M3WARN( 'MODGCTP/EQM2TRM', 0, 0, 'TRM Projection not initialized' )
            EQM2TRM = .FALSE.
            RETURN
        END IF

        IF ( EZONE .LT. 64 ) THEN
            CALL M3WARN( 'TRMBERT/EQM2TRM', 0, 0, 'EQM projection not initialized' )
            EQM2TRM = .FALSE.
            RETURN
        END IF

        XX = X
        YY = Y
        CALL XY2XY( EQMGRD3, P_ALPE, P_BETE, P_GAME, XCENTE, YCENTE,     &
                    TRMGRD3, P_ALPT, P_BETT, P_GAMT, XCENTT, YCENTT,     &
                    XX, YY, UU, VV )
        U = UU
        V = VV

        EQM2TRM = .TRUE.

        RETURN

    END FUNCTION EQM2TRM


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION TRM2EQM( X, Y, U, V )

        REAL, INTENT(IN   ) :: X, Y
        REAL, INTENT(  OUT) :: U, V

        REAL*8  XX, YY, UU, VV

        IF ( TZONE .LT. 64 ) THEN
            CALL M3WARN( 'MODGCTP/TRM2EQM', 0, 0, 'TRM Projection not initialized' )
            TRM2EQM = .FALSE.
            RETURN
        END IF

        IF ( EZONE .LT. 64 ) THEN
            CALL M3WARN( 'TRMBERT/TRM2EQM', 0, 0, 'EQM projection not initialized' )
            TRM2EQM = .FALSE.
            RETURN
        END IF

        XX = X
        YY = Y
        CALL XY2XY( TRMGRD3, P_ALPT, P_BETT, P_GAMT, XCENTT, YCENTT,     &
                    EQMGRD3, P_ALPE, P_BETE, P_GAME, XCENTE, YCENTE,     &
                    XX, YY, UU, VV )
        U = UU
        V = VV

        TRM2EQM = .TRUE.

        RETURN

    END FUNCTION TRM2EQM


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION EQM2POL( X, Y, U, V )

        REAL, INTENT(IN   ) :: X, Y
        REAL, INTENT(  OUT) :: U, V

        REAL*8  XX, YY, UU, VV

        IF ( PZONE .LT. 64 ) THEN
            CALL M3WARN( 'MODGCTP/EQM2POL', 0, 0, 'POL Projection not initialized' )
            EQM2POL = .FALSE.
            RETURN
        END IF

        IF ( EZONE .LT. 64 ) THEN
            CALL M3WARN( 'POLBERT/EQM2POL', 0, 0, 'EQM projection not initialized' )
            EQM2POL = .FALSE.
            RETURN
        END IF

        XX = X
        YY = Y
        CALL XY2XY( EQMGRD3, P_ALPE, P_BETE, P_GAME, XCENTE, YCENTE,     &
                    POLGRD3, P_ALPP, P_BETP, P_GAMP, XCENTP, YCENTP,     &
                    XX, YY, UU, VV )
        U = UU
        V = VV

        EQM2POL = .TRUE.
        RETURN

    END FUNCTION EQM2POL


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION POL2EQM( X, Y, U, V )

        REAL, INTENT(IN   ) :: X, Y
        REAL, INTENT(  OUT) :: U, V

        REAL*8  XX, YY, UU, VV

        IF ( PZONE .LT. 64 ) THEN
            CALL M3WARN( 'MODGCTP/POL2EQM', 0, 0, 'POL Projection not initialized' )
            POL2EQM = .FALSE.
            RETURN
        END IF

        IF ( EZONE .LT. 64 ) THEN
            CALL M3WARN( 'POLBERT/POL2EQM', 0, 0, 'EQM projection not initialized' )
            POL2EQM = .FALSE.
            RETURN
        END IF

        XX = X
        YY = Y
        CALL XY2XY( POLGRD3, P_ALPP, P_BETP, P_GAMP, XCENTP, YCENTP,     &
                    EQMGRD3, P_ALPE, P_BETE, P_GAME, XCENTE, YCENTE,     &
                    XX, YY, UU, VV )
        U = UU
        V = VV

        POL2EQM = .TRUE.
        RETURN

    END FUNCTION POL2EQM


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION ALB2LL( X, Y, LON, LAT )

        REAL, INTENT(IN   ) :: X, Y
        REAL, INTENT(  OUT) :: LON, LAT

        REAL*8  XX, YY, UU, VV

        IF ( AZONE .LT. 64 ) THEN
            CALL M3WARN( 'MODGCTP/ALB2LL', 0, 0, 'ALB Projection not initialized' )
            ALB2LL = .FALSE.
            RETURN
        END IF

        XX = X
        YY = Y
        CALL XY2XY( ALBGRD3, P_ALPA, P_BETA, P_GAMA, XCENTA, YCENTA,     &
                    LATGRD3,  0.0D0,  0.0D0,  0.0D0,  0.0D0,  0.0D0,     &
                    XX, YY, UU, VV )
        LON = UU
        LAT = VV

        ALB2LL = .TRUE.
        RETURN

        RETURN

    END FUNCTION ALB2LL


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION LL2ALB( LON, LAT, X, Y )

        REAL, INTENT(IN   ) :: LON, LAT
        REAL, INTENT(  OUT) :: X, Y

        REAL*8  XX, YY, UU, VV

        IF ( AZONE .LT. 64 ) THEN
            CALL M3WARN( 'LAMBERT/LL2ALB', 0, 0, 'ALB projection not initialized' )
            LL2ALB = .FALSE.
            RETURN
        END IF

        XX = LON
        YY = LAT
        CALL XY2XY( LATGRD3,  0.0D0,  0.0D0,  0.0D0,  0.0D0,  0.0D0,     &
                    ALBGRD3, P_ALPA, P_BETA, P_GAMA, XCENTA, YCENTA,     &
                    XX, YY, UU, VV )
        X = UU
        Y = VV
        LL2ALB = .TRUE.
        RETURN

    END FUNCTION LL2ALB


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION ALB2LAM( X, Y, U, V )

        REAL, INTENT(IN   ) :: X, Y
        REAL, INTENT(  OUT) :: U, V

        REAL*8  XX, YY, UU, VV

        IF ( LZONE .LT. 64 ) THEN
            CALL M3WARN( 'MODGCTP/ALB2LAM', 0, 0, 'Lambert Projection not initialized' )
            ALB2LAM = .FALSE.
            RETURN
        END IF

        IF ( AZONE .LT. 64 ) THEN
            CALL M3WARN( 'LAMBERT/ALB2LAM', 0, 0, 'ALB projection not initialized' )
            ALB2LAM = .FALSE.
            RETURN
        END IF

        XX = X
        YY = Y
        CALL XY2XY( ALBGRD3, P_ALPA, P_BETA, P_GAMA, XCENTA, YCENTT,     &
                    LAMGRD3, P_ALPL, P_BETL, P_GAML, XCENTL, YCENTL,     &
                    XX, YY, UU, VV )
        U = UU
        V = VV

        ALB2LAM = .TRUE.
        RETURN

    END FUNCTION ALB2LAM


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION LAM2ALB( X, Y, U, V )

        REAL, INTENT(IN   ) :: X, Y
        REAL, INTENT(  OUT) :: U, V

        REAL*8  XX, YY, UU, VV

        IF ( LZONE .LT. 64 ) THEN
            CALL M3WARN( 'MODGCTP/LAM2ALB', 0, 0, 'Lambert Projection not initialized' )
            LAM2ALB = .FALSE.
            RETURN
        END IF

        IF ( AZONE .LT. 64 ) THEN
            CALL M3WARN( 'LAMBERT/LAM2ALB', 0, 0, 'ALB projection not initialized' )
            LAM2ALB = .FALSE.
            RETURN
        END IF

        XX = X
        YY = Y
        CALL XY2XY( LAMGRD3, P_ALPL, P_BETL, P_GAML, XCENTL, YCENTL,     &
                    ALBGRD3, P_ALPA, P_BETA, P_GAMA, XCENTA, YCENTA,     &
                    XX, YY, UU, VV )
        U = UU
        V = VV

        LAM2ALB = .TRUE.

        RETURN

    END FUNCTION LAM2ALB


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION ALB2UTM( U, V, Z, X, Y )

        REAL   , INTENT(IN   ) :: U, V
        INTEGER, INTENT(IN   ) :: Z
        REAL   , INTENT(  OUT) :: X, Y

        REAL*8  XX, YY, UU, VV, ZZ

        IF ( AZONE .LT. 64 ) THEN
            CALL M3WARN( 'MODGCTP/ALB2UTM', 0, 0, 'ALB Projection not initialized' )
            ALB2UTM = .FALSE.
            RETURN
        END IF

        XX = U
        YY = V
        ZZ    = DBLE( Z )
        CALL XY2XY( ALBGRD3, P_ALPA, P_BETA, P_GAMA, XCENTA, YCENTA,     &
                    UTMGRD3,     ZZ,  0.0D0,  0.0D0,  0.0D0,  0.0D0,     &
                    XX, YY, UU, VV )
        X = UU
        Y = VV

        ALB2UTM = .TRUE.

        RETURN

    END FUNCTION ALB2UTM


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION UTM2ALB( U, V, Z, X, Y )

        REAL   , INTENT(IN   ) :: U, V
        INTEGER, INTENT(IN   ) :: Z
        REAL   , INTENT(  OUT) :: X, Y

        REAL*8  XX, YY, UU, VV, ZZ

        IF ( AZONE .LT. 64 ) THEN
            CALL M3WARN( 'MODGCTP/UTM2ALB', 0, 0, 'ALB Projection not initialized' )
            UTM2ALB = .FALSE.
            RETURN
        END IF

        XX = U
        YY = V
        ZZ    = DBLE( Z )
        CALL XY2XY( UTMGRD3,     ZZ,  0.0D0,  0.0D0,  0.0D0,  0.0D0,     &
                    ALBGRD3, P_ALPA, P_BETA, P_GAMA, XCENTA, YCENTA,     &
                    XX, YY, UU, VV )
        X = UU
        Y = VV

        UTM2ALB = .TRUE.

        RETURN

    END FUNCTION UTM2ALB


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION ALB2TRM( X, Y, U, V )

        REAL, INTENT(IN   ) :: X, Y
        REAL, INTENT(  OUT) :: U, V

        REAL*8  XX, YY, UU, VV

        IF ( TZONE .LT. 64 ) THEN
            CALL M3WARN( 'MODGCTP/ALB2TRM', 0, 0, 'TRM Projection not initialized' )
            ALB2TRM = .FALSE.
            RETURN
        END IF

        IF ( AZONE .LT. 64 ) THEN
            CALL M3WARN( 'TRMBERT/ALB2TRM', 0, 0, 'ALB projection not initialized' )
            ALB2TRM = .FALSE.
            RETURN
        END IF

        XX = X
        YY = Y
        CALL XY2XY( ALBGRD3, P_ALPA, P_BETA, P_GAMA, XCENTA, YCENTA,     &
                    TRMGRD3, P_ALPT, P_BETT, P_GAMT, XCENTT, YCENTT,     &
                    XX, YY, UU, VV )
        U = UU
        V = VV

        ALB2TRM = .TRUE.

        RETURN

    END FUNCTION ALB2TRM


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION TRM2ALB( X, Y, U, V )

        REAL, INTENT(IN   ) :: X, Y
        REAL, INTENT(  OUT) :: U, V

        REAL*8  XX, YY, UU, VV

        IF ( TZONE .LT. 64 ) THEN
            CALL M3WARN( 'MODGCTP/TRM2ALB', 0, 0, 'TRM Projection not initialized' )
            TRM2ALB = .FALSE.
            RETURN
        END IF

        IF ( AZONE .LT. 64 ) THEN
            CALL M3WARN( 'TRMBERT/TRM2ALB', 0, 0, 'ALB projection not initialized' )
            TRM2ALB = .FALSE.
            RETURN
        END IF

        XX = X
        YY = Y
        CALL XY2XY( TRMGRD3, P_ALPT, P_BETT, P_GAMT, XCENTT, YCENTT,     &
                    ALBGRD3, P_ALPA, P_BETA, P_GAMA, XCENTA, YCENTA,     &
                    XX, YY, UU, VV )
        U = UU
        V = VV

        TRM2ALB = .TRUE.

        RETURN

    END FUNCTION TRM2ALB


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION ALB2POL( X, Y, U, V )

        REAL, INTENT(IN   ) :: X, Y
        REAL, INTENT(  OUT) :: U, V

        REAL*8  XX, YY, UU, VV

        IF ( PZONE .LT. 64 ) THEN
            CALL M3WARN( 'MODGCTP/ALB2POL', 0, 0, 'POL Projection not initialized' )
            ALB2POL = .FALSE.
            RETURN
        END IF

        IF ( AZONE .LT. 64 ) THEN
            CALL M3WARN( 'POLBERT/ALB2POL', 0, 0, 'ALB projection not initialized' )
            ALB2POL = .FALSE.
            RETURN
        END IF

        XX = X
        YY = Y
        CALL XY2XY( ALBGRD3, P_ALPA, P_BETA, P_GAMA, XCENTA, YCENTA,     &
                    POLGRD3, P_ALPP, P_BETP, P_GAMP, XCENTP, YCENTP,     &
                    XX, YY, UU, VV )
        U = UU
        V = VV

        ALB2POL = .TRUE.
        RETURN

    END FUNCTION ALB2POL


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION POL2ALB( X, Y, U, V )

        REAL, INTENT(IN   ) :: X, Y
        REAL, INTENT(  OUT) :: U, V

        REAL*8  XX, YY, UU, VV

        IF ( PZONE .LT. 64 ) THEN
            CALL M3WARN( 'MODGCTP/POL2ALB', 0, 0, 'POL Projection not initialized' )
            POL2ALB = .FALSE.
            RETURN
        END IF

        IF ( AZONE .LT. 64 ) THEN
            CALL M3WARN( 'POLBERT/POL2ALB', 0, 0, 'ALB projection not initialized' )
            POL2ALB = .FALSE.
            RETURN
        END IF

        XX = X
        YY = Y
        CALL XY2XY( POLGRD3, P_ALPP, P_BETP, P_GAMP, XCENTP, YCENTP,     &
                    ALBGRD3, P_ALPA, P_BETA, P_GAMA, XCENTA, YCENTA,     &
                    XX, YY, UU, VV )
        U = UU
        V = VV

        POL2ALB = .TRUE.
        RETURN

    END FUNCTION POL2ALB


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION ALB2EQM( X, Y, U, V )

        REAL, INTENT(IN   ) :: X, Y
        REAL, INTENT(  OUT) :: U, V

        REAL*8  XX, YY, UU, VV

        IF ( EZONE .LT. 64 ) THEN
            CALL M3WARN( 'MODGCTP/ALB2EQM', 0, 0, 'EQM Projection not initialized' )
            ALB2EQM = .FALSE.
            RETURN
        END IF

        IF ( AZONE .LT. 64 ) THEN
            CALL M3WARN( 'EQMBERT/ALB2EQM', 0, 0, 'ALB projection not initialized' )
            ALB2EQM = .FALSE.
            RETURN
        END IF

        XX = X
        YY = Y
        CALL XY2XY( ALBGRD3, P_ALPA, P_BETA, P_GAMA, XCENTA, YCENTA,     &
                    EQMGRD3, P_ALPE, P_BETE, P_GAME, XCENTE, YCENTE,     &
                    XX, YY, UU, VV )
        U = UU
        V = VV

        ALB2EQM = .TRUE.

        RETURN

    END FUNCTION ALB2EQM


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION EQM2ALB( X, Y, U, V )

        REAL, INTENT(IN   ) :: X, Y
        REAL, INTENT(  OUT) :: U, V

        REAL*8  XX, YY, UU, VV

        IF ( EZONE .LT. 64 ) THEN
            CALL M3WARN( 'MODGCTP/EQM2ALB', 0, 0, 'EQM Projection not initialized' )
            EQM2ALB = .FALSE.
            RETURN
        END IF

        IF ( AZONE .LT. 64 ) THEN
            CALL M3WARN( 'EQMBERT/EQM2ALB', 0, 0, 'ALB projection not initialized' )
            EQM2ALB = .FALSE.
            RETURN
        END IF

        XX = X
        YY = Y
        CALL XY2XY( EQMGRD3, P_ALPE, P_BETE, P_GAME, XCENTE, YCENTE,     &
                    ALBGRD3, P_ALPA, P_BETA, P_GAMA, XCENTA, YCENTA,     &
                    XX, YY, UU, VV )
        U = UU
        V = VV

        EQM2ALB = .TRUE.

        RETURN

    END FUNCTION EQM2ALB


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !!  convert to GCTP  DDDMMMSSS.SSS format

    REAL*8 FUNCTION DDDMMMSSS( ANGLE )

        REAL*8, INTENT(IN   ) :: ANGLE

        INTEGER  DEG, MNT
        REAL*8   REM

        DEG = INT( ANGLE )                            !  int degrees
        REM = 60.0D0 * ( ANGLE - DBLE( DEG ) )        !  minutes
        MNT = INT( REM )                              !  int minutes
        REM = 60.0D0 * ( REM - DBLE( MNT ) )        !  seconds

        DDDMMMSSS = REM + 1000.0D0 * ( MNT + 1000 * DEG ) !  dddmmmsss.sssD0
        RETURN

    END FUNCTION DDDMMMSSS


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !!  convert ANGLE to DOUBLE, with fix-up for nesting-style roundoff


    REAL*8 FUNCTION DBLEFIX( ANGLE )

        REAL, INTENT(IN   ) :: ANGLE

        INTEGER*8, PARAMETER :: IFAC = 2*2*2*2*2*3*3*3*3*5
        REAL*8   , PARAMETER :: DFAC = DBLE( IFAC )
        REAL*8   , PARAMETER :: RFAC = 1.0D0 / DFAC

        DBLEFIX = RFAC * DBLE( NINT( DFAC * DBLE( ANGLE ) ) )
        RETURN

    END FUNCTION DBLEFIX


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !!  Double-precision "definitely unequal"


    LOGICAL FUNCTION DBLERR( P, Q )
        REAL*8, INTENT( IN ) :: P, Q
        DBLERR = ( (P - Q)**2  .GT.  1.0D-10*( P*P + Q*Q + 1.0D-5 ) )
    END FUNCTION DBLERR


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !!  Do two sets of map-projection parameters "match"?


    LOGICAL FUNCTION  SAMEPROJ( N1, A1, B1, C1, X1, Y1, N2, A2, B2, C2, X2, Y2 )
        INTEGER, INTENT( IN ) :: N1, N2
        REAL*8,  INTENT( IN ) :: A1, B1, C1, X1, Y1, A2, B2, C2, X2, Y2

        IF ( N1 .NE. N2 ) THEN
            SAMEPROJ = .FALSE.
        ELSE IF ( DBLERR( A1, A2 ) ) THEN
            SAMEPROJ = .FALSE.
        ELSE IF ( DBLERR( B1, B2 ) ) THEN
            SAMEPROJ = .FALSE.
        ELSE IF ( DBLERR( C1, C2 ) ) THEN
            SAMEPROJ = .FALSE.
        ELSE IF ( DBLERR( X1, X2 ) ) THEN
            SAMEPROJ = .FALSE.
        ELSE IF ( DBLERR( Y1, Y2 ) ) THEN
            SAMEPROJ = .FALSE.
        ELSE
            SAMEPROJ = .TRUE.
        END IF

        RETURN

    END FUNCTION SAMEPROJ


    LOGICAL FUNCTION  SAMEPROJ2( N1, A1, B1, C1, X1, Y1, S1, N2, A2, B2, C2, X2, Y2, S2 )
        INTEGER, INTENT( IN ) :: N1, N2
        REAL*8,  INTENT( IN ) :: A1, B1, C1, X1, Y1, S1, A2, B2, C2, X2, Y2, S2

        IF ( N1 .NE. N2 ) THEN
            SAMEPROJ2 = .FALSE.
        ELSE IF ( DBLERR( A1, A2 ) ) THEN
            SAMEPROJ2 = .FALSE.
        ELSE IF ( DBLERR( B1, B2 ) ) THEN
            SAMEPROJ2 = .FALSE.
        ELSE IF ( DBLERR( C1, C2 ) ) THEN
            SAMEPROJ2 = .FALSE.
        ELSE IF ( DBLERR( X1, X2 ) ) THEN
            SAMEPROJ2 = .FALSE.
        ELSE IF ( DBLERR( Y1, Y2 ) ) THEN
            SAMEPROJ2 = .FALSE.
        ELSE IF ( DBLERR( S1, S2 ) ) THEN
            SAMEPROJ2 = .FALSE.
        ELSE
            SAMEPROJ2 = .TRUE.
        END IF

        RETURN

    END FUNCTION SAMEPROJ2


END MODULE MODGCTP
