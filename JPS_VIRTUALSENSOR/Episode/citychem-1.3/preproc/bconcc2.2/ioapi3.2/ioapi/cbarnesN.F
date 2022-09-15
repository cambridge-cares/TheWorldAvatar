
      SUBROUTINE CBARNESN( NG, LAT, LON,
     &                     NP, NV, YLAT, XLON, Z, WL50, GRID )

C***********************************************************************
C Version "$Id: cbarnesN.F 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C    subroutine body starts at line  124
C
C  FUNCTION:
C
C    This routine grids multiple variables (or layers) Z( NP,1...NV )
C    at a time from locations with lat-lon coordinates
C    ( YLAT(1...NP), XLON(1...NP) ) and produces output array
C    GRID( NG,NV ) on the locations ( LAT(1...NG), LON(1...NG) ).
C    For gridding purposes, NG should be NCOLS*NROWS; CBARNESN will
C    regard the corresponding array as singly-indexed.
C
C    This routine grids multiple variables (or layers) from
C    source data located at latitudes and longitudes YLAT,XLON.
C    to the locations specified by LAT,LON)
C
C    This routine is an extension of a spatial analysis technique
C    with scale dependent filtering that was originally proposed
C    by Stanly Barnes in 1964 and was expanded in 1973.  The scale
C    dependent response function of the filtering effect is
C    analytically calculable and can be adjusted by the choice of
C    two parameters in the Gaussian weighting function used.
C
C    Input data point locations should be specified in
C    latitude and longitude degrees.  The separation distances
C    between these data points and the grid points where the
C    estimates are made is calculated from a function that is
C    determined by spherical geometry under the assumption of a
C    spherical Earth.
C
C  REVISION HISTORY:
C    CGRID1():
C    5/88   Modified for ROMNET
C    9/88   Modified by CJC -- Subexpression elimination and other code
C           improvements; precalculation of scanning radii
C    7/90   Modified by CJC  for ROM 2.2 -- use weights which are Gaussian
C           where exp ( AR**2 ) > 10**-30; 1/R otherwise.  Error exit via EXWST.
C           Get PI and earth-radius related constants from PI.EXT
C    2/91   Adapted by CJC  from BGRID1.FOR -- uses Barnes analysis to
C           data locations in a single corrective iteration, rather than
C           multiple biquadratic back-interpolation from the predicted grid.
C   ?????   Modified by ?Steve Fudge? for UAM BEIS
C   CBARNESN():
C   12/95   Modified by CJC to fit EDSS/Models-3 conventions from
C           the UAM BEIS CGRID1().  Uses "exact" spherical-geometry
C           distance formula.
C   08/2000 Bug-fix by CJC in SAVED-variables list
C
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C       Version    9/2014 by CJC:  modifications for OpenMP parallel
C***********************************************************************

        IMPLICIT NONE

        INCLUDE 'CONST3.EXT'

C.......   ARGUMENTS:

        INTEGER, INTENT(IN   ) ::    NG               !  Number of output values (=NCOLS*NROWS)
        REAL   , INTENT(IN   ) ::    LAT( NG )        !  single-indexed output latitudes
        REAL   , INTENT(IN   ) ::    LON( NG )        !  single-indexed output longitudes
        INTEGER, INTENT(IN   ) ::    NP               !  number of input sources
        INTEGER, INTENT(IN   ) ::    NV               !  number of input variables
        REAL   , INTENT(IN   ) ::    YLAT( NP )       !  input latitudes
        REAL   , INTENT(IN   ) ::    XLON( NP )       !  input longitudes
        REAL   , INTENT(IN   ) ::    Z   ( NP, NV )   !  input values
        REAL   , INTENT(IN   ) ::    WL50             !  50 Percent filtered wavelength  (KM)
        REAL   , INTENT(  OUT) ::    GRID( NG, NV )   !  output values


C...........   PARAMETERS:

        REAL, PARAMETER :: G      =  0.4
        REAL, PARAMETER :: GINV   =  2.5
        REAL, PARAMETER :: R0LN50 =  2.1058923
        REAL, PARAMETER :: ALOG10 =  2.3025850929
        REAL, PARAMETER :: R2DSQ  =  RPI180 * RPI180
        REAL, PARAMETER :: AC     =  30.0 * ALOG10 / D2KMSQ


C...........   LOCAL VARIABLES:

        INTEGER    I, J, M, N    !  cell, variable, station counters
        REAL       C             !  scaling-filter constant
        REAL       YG , XG       !  temporaries for lat, lon of current cell
        REAL       W1, W2        !  weights for Gauss-weighted average
        REAL       WTOT1, WTOT2  !  sum of weights
        REAL       FTOT1( NV )   !  accumulator for values
        REAL       FTOT2( NV )   !  accumulator for corrections
        REAL       DSQ           !  DELX**2  +  DELY**2
        REAL       XA, YA        !  LON, LAT of current station
        REAL       XB, YB        !  LON, LAT of current station
        REAL       CY, SY        !  cos, sin of lat
        REAL       DZ( NP, NV )  !  work array
        CHARACTER*250  MESG

C...........   STATE VARIABLES:

        REAL, SAVE :: WL501 = -1.0  !  last value of WL50
        REAL, SAVE :: C4K           !  Gauss constant for first pass
        REAL, SAVE :: GC4K          !  Gauss constant for second pass
        REAL, SAVE :: RMAXA         !  maximum scanning radii, for first
        REAL, SAVE :: RMAXB         !  and second passes
        REAL, SAVE :: ANUMA         !  numerator, beyond scanning radius,
        REAL, SAVE :: ANUMB         !  for first and second passes


C..................................................................
C.......   begin body of  CBARNESN:

C...........   Calculate the scaling filter parameter

      IF  ( WL50 .NE. WL501 )  THEN

          C  =  R0LN50 * ( WL50 / PI )**2

          C4K   = -D2KMSQ / C
          RMAXA =  C * AC           !   exp ( c4k * rmaxa ) = 1.0e-30
          ANUMA =  1.0E-30 * RMAXA

          GC4K  =  GINV * C4K
          RMAXB =  RMAXA * G
          ANUMB =  1.0E-30 * RMAXB

          WL501 = WL50                        !  Record "WL50"

      ELSE IF ( NP .LE. 0 ) THEN
          CALL M3EXIT( 'CBARNESN', 0, 0, 'NP=0 points input', 2 )
      END IF


C.......   Prediction pass:  Scan each input data point and construct
C.......   estimated error  DZ  at that point:

!$OMP   PARALLEL DO
!$OMP&    DEFAULT( NONE ),
!$OMP&     SHARED( NP, NV, XLON, YLAT, RMAXA, ANUMA, C4K, Z, DZ ),
!$OMP&    PRIVATE( N, I, FTOT1, XA, YA, CY, WTOT1, M, XB, YB, DSQ, W1  )

      DO 155   N = 1 , NP

          DO  111  I = 1 , NV
              FTOT1( I ) = 0.0
111       CONTINUE              !  end loop on variables I

          XA    =  XLON( N )
          YA    =  YLAT( N )
          CY    =  COS( PI180 * YA )  **2
          WTOT1 =  0.0

          DO  133  M = 1 , NP

              XB = XLON( M ) - XA
              YB = YLAT( M ) - YA
              DSQ   =  R2DSQ * ( YB**2 + CY * XB**2 )

              IF ( DSQ .LE. RMAXA )  THEN
                  W1     =  EXP ( DSQ * C4K )
              ELSE          !  use matching  1/R**2  weight
                  W1     =  ANUMA / DSQ
              END IF
              WTOT1  =  WTOT1  +  W1

              DO  122  I = 1, NV
                  FTOT1( I )  =  FTOT1( I )  +  W1 * Z ( M,I )
122           CONTINUE              !  end loop on variables I

133       CONTINUE              !  end loop on sites M

          WTOT1 = 1.0 / WTOT1

          DO  144  I = 1 , NV
              DZ ( N,I ) = Z ( N,I )  -  FTOT1( I ) * WTOT1
144       CONTINUE              !  end loop on variables I

155   CONTINUE          !  end prediction loop on sites N


C.......   Grid-prediction pass:  generate estimate using first set of
C.......   weights, and correction using error estimates  DZ and second
C.......   set of weights

!$OMP   PARALLEL DO
!$OMP&    DEFAULT( NONE ),
!$OMP&     SHARED( NG, NP, NV, LAT, LON, XLON, YLAT, RMAXA, RMAXB,
!$OMP&             ANUMA, ANUMB, C4K, GC4K, Z, DZ, GRID ),
!$OMP&    PRIVATE( J, N, I, FTOT1, FTOT2, XG, YG, XA, YA, CY, SY,
!$OMP&             WTOT1, WTOT2, M, DSQ, W1, W2  )

      DO  255  J = 1 , NG

          DO  211  I = 1 , NV
              FTOT1( I ) = 0.0
              FTOT2( I ) = 0.0
211       CONTINUE              !  end loop on variables I

          YG   =  PI180 * LAT( J )    !  LAT of grid nodes at index J
          XG   =  PI180 * LON( J )    !  LON of grid nodes at index J
          CY   =  COS( YG )
          SY   =  SIN( YG )

C.......   Scan each input data point

          WTOT1 = 0.0
          WTOT2 = 0.0

          DO  233  N = 1, NP

              XA    =  XLON( N ) - XG
              YA    =  YLAT( N ) - YG
              DSQ   =  R2DSQ * ( YA**2 + CY*XA**2 )

              IF ( DSQ .LE. RMAXB )  THEN
                  W1  =  EXP ( DSQ * C4K )
                  W2  =  EXP ( DSQ * GC4K )
              ELSE IF ( DSQ .LE. RMAXA )  THEN
                  W1  =  EXP ( DSQ * C4K )
                  W2  =  ANUMB / DSQ
              ELSE          !  use matching  1/R**2  weight
                  W1  =  ANUMA / DSQ
                  W2  =  G * W1     ! = ANUMB / DSQ
              END IF

              WTOT1  =  WTOT1  +  W1
              WTOT2  =  WTOT2  +  W2

              DO  222  I = 1 , NV
                  FTOT1( I ) = FTOT1( I )  +  W1 * Z ( N,I )
                  FTOT2( I ) = FTOT2( I )  +  W2 * DZ( N,I )
222           CONTINUE              !  end loop on variables I

233       CONTINUE          !  end loop on data sites nn

          WTOT1 = 1.0 / WTOT1
          WTOT2 = 1.0 / WTOT2
          DO  244  I = 1 , NV
              GRID( J,I ) = WTOT1 * FTOT1( I )  +  WTOT2 * FTOT2( I )
244       CONTINUE              !  end loop on variables I

255   CONTINUE          !  end loop on rows J


      RETURN

      END SUBROUTINE CBARNESN
