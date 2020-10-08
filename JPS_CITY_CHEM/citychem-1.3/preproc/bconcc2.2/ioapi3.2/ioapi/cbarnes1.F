
      SUBROUTINE CBARNES1( NG, LAT, LON,
     &                     N, YLAT, XLON, Z, WL50, GRID)

C***********************************************************************
C Version "$Id: cbarnes1.F 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C    subroutine body starts at line  119
C
C  FUNCTION:
C
C    This routine grids one variable Z at a time from locations with 
C    lat-lon coordinates ( YLAT(1...NP), XLON(1...NP) ) and produces 
C    one output array GRID(1...NG) on the locations ( LAT(1...NG),LON(1...NG) ).
C    For gridding purposes, NG should be NCOLS*NROWS; CBARNES1 will
C    regard the corresponding array as singly-indexed.
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
C   CBARNES1():
C   12/95   Modified by CJC to fit EDSS/Models-3 conventions from 
C           the UAM BEIS CGRID1().  Uses "exact" spherical-geometry
C           distance formula.
C   08/2000 Bug-fix by CJC in SAVED-variables list. 
C   03/2004 Back to linar-approx distance formula
C
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C       Version    9/2014 by CJC:  modifications for OpenMP parallel
C***********************************************************************

        IMPLICIT NONE

        INCLUDE 'CONST3.EXT'

C.......   ARGUMENTS:

        INTEGER, INTENT(IN   ) :: NG            !  Number of output values (=NCOLS*NROWS)
        REAL   , INTENT(IN   ) :: LAT( NG )     !  single-indexed output latitudes
        REAL   , INTENT(IN   ) :: LON( NG )     !  single-indexed output longitudes
        INTEGER, INTENT(IN   ) :: N             !  number of input values
        REAL   , INTENT(IN   ) :: YLAT( N )     !  input latitudes
        REAL   , INTENT(IN   ) :: XLON( N )     !  input longitudes
        REAL   , INTENT(IN   ) :: Z   ( N )     !  input values
        REAL   , INTENT(IN   ) :: WL50          !  50 Percent filtered wavelength  (KM)
        REAL   , INTENT(  OUT) :: GRID( NG )    !  output values

C...........   PARAMETERS:

        REAL, PARAMETER :: G      =  0.4
        REAL, PARAMETER :: GINV   =  2.5
        REAL, PARAMETER :: R0LN50 =  2.1058923
        REAL, PARAMETER :: ALOG10 =  2.3025850929
        REAL, PARAMETER :: R2DSQ  =  RPI180 * RPI180
        REAL, PARAMETER :: AC     =  30.0 * ALOG10 / D2KMSQ


C...........   LOCAL VARIABLES:

        INTEGER    J             !  col, row counters
        INTEGER    MM , NN       !  station counters

        REAL       C             !  scaling-filter constant
        REAL       YG , XG       !  temporaries for lat, lon of current cell
        REAL       CY            !  temporaries for sin, cos of lat, lon
        REAL       W1, W2        !  weights for Gauss-weighted average
        REAL       WTOT1, WTOT2  !  sum of weights
        REAL       FTOT1, FTOT2  !  accumulators for values, corrections
        REAL       DSQ           !  DELX**2  +  DELY**2
        REAL       XA, YA        !  LON, LAT of current station
        REAL       XB, YB        !  LON, LAT of current station
        REAL       DZ( N )
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
C.......   begin body of  CBARNES1:

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

      ELSE IF ( N .LE. 0 ) THEN
          CALL M3EXIT( 'CBARNESN', 0, 0, 'NP=0 points input', 2 )
      END IF


C.......   Prediction pass:  Scan each input data point and construct
C.......   estimated error  DZ  at that point:

!$OMP   PARALLEL DO
!$OMP&    DEFAULT( NONE ),
!$OMP&     SHARED( N, XLON, YLAT, RMAXA, ANUMA, C4K, Z, DZ ),
!$OMP&    PRIVATE( NN, MM, XA, YA, CY, WTOT1, FTOT1, XB, YB, DSQ, W1  )

      DO  222  NN = 1 , N

          XA    =  XLON( NN )
          YA    =  YLAT( NN )
          CY    =  COS( PI180 * YA )**2
          WTOT1 =  0.0
          FTOT1 =  0.0

          DO  111  MM = 1 , N

              XB = XLON( MM ) - XLON( NN )
              YB = YLAT( MM ) - YLAT( NN )
              DSQ   =  R2DSQ * ( YB**2 + CY * XB**2 )

              IF ( DSQ .LE. RMAXA )  THEN
                  W1     =  EXP ( DSQ * C4K )
              ELSE          !  use matching  1/R**2  weight
                  W1     =  ANUMA / DSQ
              END IF
              WTOT1  =  WTOT1  +  W1
              FTOT1  =  FTOT1  +  W1 * Z ( MM )

111       CONTINUE              !  end loop on sites M

          DZ ( NN ) = Z ( NN )  -  FTOT1 / WTOT1

222   CONTINUE          !  end prediction loop on sites NN


C.......   Grid-prediction pass:  generate estimate using first set of
C.......   weights, and correction using error estimates  DZ and second
C.......   set of weights

!$OMP   PARALLEL DO
!$OMP&    DEFAULT( NONE ),
!$OMP&     SHARED( NG, N, LAT, LON, XLON, YLAT, RMAXA, RMAXB,
!$OMP&             ANUMA, ANUMB, C4K, GC4K, Z, DZ, GRID ),
!$OMP&    PRIVATE( J, NN, XG, YG, CY, FTOT1, FTOT2, WTOT1, WTOT2,
!$OMP&             XA, YA, DSQ, W1, W2  )

      DO  555  J = 1 , NG

          YG   =  LAT( J )    !  LAT of grid nodes at index J
          XG   =  LON( J )    !  LON of grid nodes at index J
          CY   =  COS( PI180 * YG )**2
           
C.......   Scan each input data point
      
          FTOT1 = 0.0
          WTOT1 = 0.0
          FTOT2 = 0.0
          WTOT2 = 0.0

          DO  333  NN = 1, N
           
              XA    =  XLON( NN ) - XG
              YA    =  YLAT( NN ) - YG
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
              FTOT1  =  FTOT1  +  W1 * Z ( NN )
              FTOT2  =  FTOT2  +  W2 * DZ( NN )
           
333       CONTINUE          !  end loop on data sites nn

          GRID( J ) = FTOT1 / WTOT1  +  FTOT2 / WTOT2

555   CONTINUE          !  end loop on rows J


      RETURN

      END SUBROUTINE CBARNES1
