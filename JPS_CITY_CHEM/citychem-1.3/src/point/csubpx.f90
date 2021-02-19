! <csubpx.f90 - A component of the City-scale
!                 Chemistry Transport Model EPISODE-CityChem>
!*****************************************************************************! 
!* 
!* EPISODE - An urban-scale air quality model
!* ========================================== 
!* Copyright (C) 2018  NILU - Norwegian Institute for Air Research
!*                     Instituttveien 18
!*                     PO Box 100
!*                     NO-2027 Kjeller
!*                     Norway
!*
!*                     Contact persons: Gabriela Sousa Santos - gss@nilu.no
!*                                      Paul Hamer - pdh@nilu.no
!*
!* Unless explicitly acquired and licensed from Licensor under another license,
!* the contents of this file are subject to the Reciprocal Public License ("RPL")
!* Version 1.5, https://opensource.org/licenses/RPL-1.5 or subsequent versions as
!* allowed by the RPL, and You may not copy or use this file in either source code
!* or executable form, except in compliance with the terms and conditions of the RPL. 
!*
!* All software distributed under the RPL is provided strictly on an "AS IS" basis, 
!* WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESS OR IMPLIED, AND LICENSOR HEREBY 
!* DISCLAIMS ALL SUCH WARRANTIES, INCLUDING WITHOUT LIMITATION, ANY WARRANTIES OF
!* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, QUIET ENJOYMENT, OR NON-INFRINGEMENT.
!* See the RPL for specific language governing rights and limitations under the RPL.
!*
!* ========================================== 
!* The dispersion model EPISODE (Grønskei et. al., 1993; Larssen et al., 1994;
!* Walker et al., 1992, 1999; Slørdal et al., 2003, 2008) is an Eulerian grid model 
!* with embedded subgrid models for calculations of pollutant concentrations resulting 
!* from different types of sources (area-, line- and point sources). EPISODE solves the 
!* time dependent advection/-diffusion equation on a 3 dimensional grid. 
!* Finite difference numerical methods are applied to integrate the solution forward in time. 
!* It also includes extensions as the implementation of a simplified EMEP photochemistry 
!* scheme for urban areas (Walker et al. 2004) and a scheme for Secondary Organic Aerosol 
!* implemented by Håvard Slørdal
!*
!* Grønskei, K.E., Walker, S.E., Gram, F. (1993) Evaluation of a model for hourly spatial
!*    concentrations distributions. Atmos. Environ., 27B, 105-120.
!* Larssen, S., Grønskei, K.E., Gram, F., Hagen, L.O., Walker, S.E. (1994) Verification of 
!*    urban scale time-dependent dispersion model with sub-grid elements in Oslo, Norway. 
!*    In: Air poll. modelling and its appl. X. New York, Plenum Press.
!* Slørdal, L.H., McInnes, H., Krognes, T. (2008): The Air Quality Information System AirQUIS. 
!*    Info. Techn. Environ. Eng., 1, 40-47, 2008.
!* Slørdal, L.H., Walker, S.-E., Solberg, S. (2003) The Urban Air Dispersion Model EPISODE 
!*    applied in AirQUIS. Technical Description. NILU TR 12/2003. ISBN 82-425-1522-0.
!* Walker, S.E., Grønskei, K.E. (1992) Spredningsberegninger for on-line overvåking i Grenland. 
!*    Programbeskrivelse og brukerveiledning. Lillestrøm, 
!*    Norwegian Institute for Air Research (NILU OR 55/92).
!* Walker, S.E., Slørdal, L.H., Guerreiro, C., Gram, F., Grønskei, K.E. (1999) Air pollution 
!*    exposure monitoring and estimation. Part II. Model evaluation and population exposure. 
!*    J. Environ. Monit, 1, 321-326.
!* Walker, S.-E., Solberg, S., Denby, B. (2003) Development and implementation of a 
!*    simplified EMEP photochemistry scheme for urban areas in EPISODE. NILU TR 13/2013. 
!*    ISBN 82-425-1524-7
!*****************************************************************************! 

      subroutine CSUBPX(ICV,IDV,IWV,TSV)

! The subroutine calculates subgrid scale concentrations and dry and
! wet depositions for all plume segments in all irregular receptor
! points.
! ----------------------------------------------------------------------------------
! Based on:
! Version Episode 5.5 (May29, 2012) prepared for BB-Stand-Alone
! Original source code of EPISODE by Sam-Erik Walker (NILU)
!
! Sam-Erik Walker
! Norwegian Institute for Air Research (NILU)
! Instituttveien 18 P.O. Box 100
! N-2027 Kjeller, NORWAY
! Tel: +47 63898000 Fax: +47 63898050
! E-mail: sam-erik.walker@nilu.no
!
! ----------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------
! REVISION HISTORY
!
!           2016  M. Karl: Lower limits for the Gaussian plume equation terms
!           2017  M. Karl: Conversion from num/s to g/s (num = number of particles)
!
! ----------------------------------------------------------------------------------

      use mod_util
      use mod_main
      use mod_site
      use mod_time
      use mod_mete
      use mod_conc
      use mod_depo
      use mod_psrc

      implicit none

      REAL TSV
      INTEGER ICV
      INTEGER IDV
      INTEGER IWV

! TSV - Time since last deposition
! ICV - Update concentration  indicator
! IDV - Update dry deposition indicator
! IWV - Update wet deposition indicator

! Local variables

      REAL AEROV
      REAL ALFA0
      REAL AREAV
      REAL ADD1
      REAL ADD2
      REAL CRVV
      REAL CRV0
      REAL DD
      REAL DDRVV
      REAL DELYW
      REAL DRV
      REAL DSZDX0
      REAL DTDEPO
      REAL DXW
      REAL DYW
      REAL HMIXV
      REAL HV
      REAL FAC1
      REAL FF
      REAL FMAX1
      REAL FMAX2
      REAL FNEW
      REAL LAMBV
      REAL LXW
      REAL LYW
      REAL PRECV
      REAL QEMV
      REAL QHSV
      REAL QXV
      REAL QYV
      REAL QZV
      REAL RFXW
      REAL RFYW
      REAL SIGVV
      REAL SIGWV
      REAL SIGYV
      REAL SIGZV
      REAL TA
      REAL TADD
      REAL TF
      REAL THETA
      REAL TL
      REAL TL4
      REAL TNEW
      REAL TNUL
      REAL TOPRV
      REAL TTOT
      REAL TY
      REAL TZ
      REAL TZ2
      REAL UU
      REAL VDV
      REAL VV
      REAL V0
      REAL WDRVV
      REAL XPV
      REAL XRV
      REAL XRVS
      REAL XRW
      REAL YPV
      REAL YRV
      REAL YRVS
      REAL YRW
      REAL ZPV
      REAL ZRV

      INTEGER IC
      INTEGER ILS
      INTEGER IP
      INTEGER IQ
      INTEGER IXP
      INTEGER IXE
      INTEGER IXS
      INTEGER IXW
      INTEGER IYP
      INTEGER IYE
      INTEGER IYS
      INTEGER IYW
      INTEGER IZP
      INTEGER NXW
      INTEGER NYW
      INTEGER IZE

! AEROV  - Aerodynamic resistance
! ALFA0  - Dry deposition reflection coefficient
! AREAV  - Area of plume segment subgrid cell
! ADD1   - Addend in Gaussian plume segment formula
! ADD2   - Addend in Gaussian plume segment formula
! CRVV   - Concentration value
! CRV0   - Concentration value
! DD     - Wind direction
! DDRVV  - Receptor dry deposition value
! DELYW  - Plume segment crosswind distance
! DRV    - Relative distance from point source to receptor point
! DSZDX0 - Derivative of sigma-z with respect to x
! DTDEPO - Timestep for deposition
! DXW    - Length of plume segment cell in x-direction
! DYW    - Length of plume segment cell in y-direction
! HMIXV  - Mixing height value
! HV     - Plume segment height value
! FAC1   - Factor in Gaussian plume segment formula
! FF     - Wind speed
! FMAX1  - Factor in Gaussian plume segment formula
! FMAX2  - Factor in Gaussian plume segment formula
! FNEW   - Used in calculating sigma-y and sigma-z
! LAMBV  - Wet deposition lambda value
! LXW    - Plume segment length
! LYW    - Plume segment width
! PRECV  - Precipitation value
! QEMV   - Plume segment emission value
! QHSV   - Plume segment stack height value
! QXV    - Plume segment stack x-coordinate value
! QYV    - Plume segment stack y-coordinate value
! QZV    - Plume segment stack z-coordinate value
! RFXW   - Refinement factor in x-direction
! RFYW   - Refinement factor in y-direction
! SIGVV  - Sigma-v value
! SIGWV  - Sigma-w value
! SIGYV  - Plume segment sigma-y value
! SIGZV  - Plume segment sigma-z value
! TA     - Used in calculating sigma-y and sigma-z
! TADD   - Used in calculating sigma-y and sigma-z
! TF     - Used in calculating sigma-y and sigma-z
! THETA  - Plume segment rotation angle
! TL     - Lagrangian time-scale value
! TL4    - Lagrangian time-scale value
! TNEW   - Used in calculating sigma-y and sigma-z
! TNUL   - Used in calculating sigma-y and sigma-z
! TOPRV  - Topography height at receptor point 
! TTOT   - Total elapsed simulation time
! TY     - Used in calculating sigma-y and sigma-z
! TZ     - Used in calculating sigma-y and sigma-z
! TZ2    - Used in calculating sigma-y and sigma-z
! UU     - Wind u- and v-components
! VDV    - Dry deposition velocity value
! VV     - Wind u- and v-components
! V0     - Velocity value
! WDRVV  - Receptor wet deposition value
! XPV    - Plume segment x-coordinate
! XRV    - Receptor x-coordinate
! XRVS   - Receptor x-coordinate
! XRW    - Receptor point wind directed x-coordinate
! YPV    - Plume segment y-coordinate
! YRV    - Receptor y-coordinate
! YRVS   - Receptor y-coordinate
! YRW    - Receptor point wind directed y-coordinate
! ZPV    - Plume segment z-coordinate
! ZRV    - Receptor z-coordinate
! IC     - Compound index
! ILS    - Subgrid layer index
! IP     - Plume segment index
! IQ     - Plume segment point source index
! IXP    - Plume segment main grid index in x-direction
! IXE    - Main grid index in x-direction
! IXS    - Sub  grid index in x-direction
! IXW    - Plume segment subgrid index in x-direction
! IYP    - Plume segment main grid index in y-direction
! IYE    - Main grid index in y-direction
! IYS    - Sub  grid index in y-direction
! IYW    - Plume segment subgrid index in y-direction
! IZE    - Main grid index in z-direction
! IZP    - Plume segment main grid index in z-direction
! NXW    - Number of plume segment gridcells in x-direction
! NYW    - Number of plume segment gridcells in y-direction

! External functions

!MSK      REAL EXP4      !(is in mod_util)
!MSK      REAL FTOP      !(is in mod_psrc)

! EXP4 - Exponent function
! FTOP - Topography height function

! Calculate total elapsed simulation time

      TTOT = THOUR*3600. + ITS*DT

! Go through all plume segments

      DO 100 IP = 1,NP

! Calculate dry and wet deposition timestep

      DTDEPO = MIN(TTOT - PU(5,IP),TSV)

! Get plume segment coordinates

      XPV = PU(1,IP)
      YPV = PU(2,IP)
      ZPV = PU(7,IP)

! Get plume segment main grid indices

      CALL GETMGI(1,XPV,YPV,ZPV,IXP,IYP,IZP)

! Get plume segment wind speed and direction

      UU = U(IXP,IYP,IZP)
      VV = V(IXP,IYP,IZP)
      FF = SQRT(UU*UU + VV*VV)
      DD = ABS(PU(8,IP))

! Lower bound on windspeed for the calculations below

      FF = MAX(FF,PSRCFFMIN)

! Get aerodynamic resistance (s/m), precipitation (mm/h) and mixing
! height (m)

      AEROV = AERO(IXP,IYP)
      PRECV = PREC(IXP,IYP)
      HMIXV = HMIX(IXP,IYP)

! Convert precipitation from mm/h to m/s

      PRECV = PRECV/3.6E+6

! Get plume segment length

      LXW = PU(6,IP)

! Calculate wind directed coordinate system rotation angle

      THETA = (270. - DD)*RAD

! Get plume segment point source index

      IQ = INT(PU(9,IP))

! Get plume segment point source coordinates and stack height

       QXV =  QX(IQ)
       QYV =  QY(IQ)
       QZV =  QZ(IQ)
      QHSV = QHS(IQ)

! All calculations performed at 1 m above ground level

       ZRV = 1.0

! Set refinement factor in x-direction

      RFXW = 0.1

! Calculate target subgrid cell size in x-direction

      DXW = RFXW*MIN(DX,DY)

! Calculate target number of subgrid cells

      NXW = INT(LXW/DXW) + 1
      NXW = MAX(NXW,3)

! Calculate actual subgrid cell size in x-direction

      DXW = LXW/NXW

! Go through all plume segment subgrid cells in x-direction

      DO 200 IXW = 1,NXW

! Calculate plume segment subgrid cell x-coordinate
 
      XRW = (IXW - 0.5)*DXW

! Calculate advection time from start of plume segment to receptor point

      TADD = XRW/FF

! Growth of sigma-y (Irwin 1983)

      SIGYV = PU(3,IP)
      SIGVV = SIGV(IXP,IYP,IZP)
      TY    = SIGYV/SIGVV
      TF    = 0.9*TY/(2.*SQRT(1000.))
      TA    = TF + SQRT(TF*TF + TY)
      TNUL  = TA*TA
      TNEW  = TNUL + TADD
      FNEW  = 1./(1. + 0.9*SQRT(TNEW/1000.))
      SIGYV = SIGVV*TNEW*FNEW

! Growth of sigma-z (Venkatram 1984)

      SIGZV = PU(4,IP)
      SIGWV = SIGWP(IXP,IYP,IZP)
      TL    = TLGRP(IXP,IYP,IZP)
      TL4   = 4.*TL
      TZ    = SIGZV/SIGWV
      TZ2   = TZ*TZ
      TNUL  = TZ2/TL4 + SQRT(TZ2*TZ2/(TL4*TL4) + TZ2)
      TNEW  = TNUL + TADD
      FNEW  = 1./SQRT(1. + TNEW/(2.*TL))
      SIGZV = SIGWV*TNEW*FNEW

! Calculate derivative of sigma-z with respect to x at receptor point

      DSZDX0 = SIGZV*(TL4 + TNEW)/(TNEW*(TL4 + 2.*TNEW))/FF

! Calculate Gaussian plume equation terms

      FMAX1 = 1./(2.*PI*FF*SIGYV*SIGZV)
      FMAX2 = 1./(SQRT(2.*PI)*FF*SIGYV)

! Calculate plume segment length in y-direction

      LYW = 6.*SIGYV

! Set refinement factor in y-direction

      RFYW = 0.1

! Calculate target subgrid cell size in y-direction

      DYW = RFYW*MIN(DX,DY)

! Calculate number of subgrid cells in y-direction

      NYW = INT(LYW/DYW) + 1
      NYW = MAX(NYW,10)
      IF (MOD(NYW,2) .EQ. 1) NYW = NYW + 1

! Calculate actual subgrid cell size in y-direction

      DYW = LYW/NYW

! Go through all plume segment cells in y-direction

      DO 210 IYW = 1,NYW

! Calculate plume segment subgrid cell y-coordinate

      YRW = -LYW/2. + (IYW - 0.5)*DYW

! Transform plume segment cell coordinates to original coordinates

      XRV = XPV + COS(THETA)*XRW - SIN(THETA)*YRW
      YRV = YPV + SIN(THETA)*XRW + COS(THETA)*YRW

! Get extended main grid indices

      CALL GETMGI(0,XRV,YRV,ZRV,IXE,IYE,IZE)

! Find receptor point topography height

      CALL CTOPO(XRV,YRV,ZRV,TOPRV)

! Calculate distance from point source to receptor point relative to
! stack height

      DRV = SQRT((XRV - QXV)*(XRV - QXV) +      &
                (YRV - QYV)*(YRV - QYV))/QHSV

! Calculate efficient plume segment height for concentration
! calculations at the receptor point

      HV = ZPV - (TOPRV - QZV)*FTOP(DRV)
      HV = MAX(HV,0.)

! Calculate plume segment subgrid cell area

      AREAV = DXW*DYW

! Calculate compound independent Gaussian plume equation terms

      DELYW = YRW/SIGYV
      FAC1 = EXP4(-0.5*DELYW*DELYW)

      ADD1 = EXP4(-0.5*((0. - HV)/SIGZV)*((0. - HV)/SIGZV)) 
      ADD2 = EXP4(-0.5*((0. + HV)/SIGZV)*((0. + HV)/SIGZV)) 

!MSK start
      if (ADD1 .gt. 0.)  ADD1 = MAX(1.e-12, ADD1)
      if (ADD2 .gt. 0.)  ADD2 = MAX(1.e-12, ADD2)
      if (FAC1 .gt. 0.)  FAC1 = MAX(1.e-12, FAC1)
      if (FMAX1 .gt. 0.) FMAX1 = MAX(1.e-12, FMAX1)

      if (ADD1 .lt. 0.)  ADD1 = MIN(-1.e-12,ADD1)
      if (ADD2 .lt. 0.)  ADD2 = MIN(-1.e-12,ADD2)
      if (FAC1 .lt. 0.)  FAC1 = MIN(-1.e-12,FAC1)
      if (FMAX1 .lt. 0.)  FMAX1 = MIN(-1.e-12,FMAX1)
!MSK end

! Go through all compounds

      DO 220 IC = 1,NC

! Calculate dry deposition velocity (m/s)

          VDV = DDEPV(IC)/(DDEPV(IC)*AEROV + 1.)

! Calculate wet scavenging coefficient

          LAMBV = PRECV*WDEPSR(IC)/HMIXV

! Get plume segment emission value (g/s)

          QEMV = PU(9 + 2*IC,IP)

! Adjust plume segment emission to receptor point

          QEMV = QEMV*EXP4(-LAMBV*TADD)

! Calculate dry deposition partial reflection coefficient for ground
! level receptor point (Overcamp, 1976)

          V0 = VDV + FF*HV*DSZDX0/SIGZV
          IF (V0 .EQ. 0.) THEN
              ALFA0 = 1.
          ELSE
              ALFA0 = 1. - 2.*VDV/V0
          ENDIF

!MSK start
          IF ( QEMV .lt. 1.e-20)    QEMV = 0.0
!MSK end


! Calculate Gaussian concentration at ground level (g/m3)

          CRV0 = QEMV*FMAX1*FAC1*(ADD1 + ALFA0*ADD2)
          !print *,'csubpx ',ic,QEMV,CRV0

! Concentration converted to ug/m3

!MSK start
! MSK new comment 23.01.2017
! Conversion from num/s to g/s   (num = number of particles)
! Number emission has the right time unit.
! We only have to take care that concentration CRV for PNC 
! Need to convert from number/m^3 to number/cm^3:

          IF ((IQU .EQ. 4).or.(CUNIT(ICV)(1:3) == 'num') ) then
            CRVV = 1.E-6*CRV0
          ELSE
            ! ug/m3
            CRVV = 1.E+6*CRV0
          ENDIF
!MSK end

! Calculate dry deposition flux in receptor point (g/m2*s)
!MSK start
          IF ( VDV .lt. 1.e-20)    VDV = 0.0
          IF ( CRV0 .lt. 1.e-20)   CRV0 = 0.0
!MSK end

          DDRVV = VDV*CRV0

! Calculate wet deposition flux in receptor point (g/m2*s)

          WDRVV = LAMBV*QEMV*FMAX2*FAC1

! Integrate depositions over actual timestep (g/m2)

          DDRVV = DDRVV*DTDEPO
          WDRVV = WDRVV*DTDEPO

! If main grid cell then update main grid concentration and
! deposition

          IF (IXE .GE. 1 .AND. IXE .LE. NX .AND.      &
             IYE .GE. 1 .AND. IYE .LE. NY) THEN

! Add concentration to main grid (ug/m3)

!!!===============================================
!!! for averaged main grid concentration, by Kang Dec.23,2019
!!! need to be double check 
!!!===============================================
! orig          IF (ICV .EQ. 1)      &
!            CM(IXE,IYE,IC) =    CM(IXE,IYE,IC) +  CRVV*AREAV/(DX*DY)
        if(averaged_output) then
         IF (ICV .EQ. 1) CM(IXE,IYE,IC) =  CM(IXE,IYE,IC) +  CRVV*AREAV/(DX*DY)/FLOAT(NTS)
        else
          IF (ICV .EQ. 1) CM(IXE,IYE,IC) =  CM(IXE,IYE,IC) +  CRVV*AREAV/(DX*DY)
        endif
!!!===============================================        
! Add dry deposition to main grid (g/m2)

          IF (IDV .EQ. 1)      &
         DDEPM(IXE,IYE,IC) = DDEPM(IXE,IYE,IC) + DDRVV*AREAV/(DX*DY)

! Add wet deposition to main grid (g/m2)

          IF (IWV .EQ. 1)      &
         WDEPM(IXE,IYE,IC) = WDEPM(IXE,IYE,IC) + WDRVV*AREAV/(DX*DY)

!MSK No sub grid concentration calculated
!MSK
!MSK ! If subgrid cell then update subgrid concentration and deposition
!MSK
!MSK          IF (SUBF(IXE,IYE) .GT. 0.) THEN
!MSK
!MSK ! Calculate subgrid layer index
!MSK
!MSK          ILS = INT(SUBF(IXE,IYE))
!MSK
!MSK ! Calculate subgrid indices
!MSK
!MSK          XRVS = XRV - (IXE - 1)*DX - SITEX0
!MSK          YRVS = YRV - (IYE - 1)*DY - SITEY0
!MSK          IXS  = INT(XRVS/DXS) + 1
!MSK          IYS  = INT(YRVS/DYS) + 1
!MSK
!MSK ! Add concentration to sub grid (ug/m3)
!MSK
!MSK          IF (ICV .EQ. 1)      &
!MSK            CS(IC,IXS,IYS,ILS) =    CS(IC,IXS,IYS,ILS) +      &
!MSK                                 CRVV*AREAV/(DXS*DYS)
!MSK
!MSK         print *,'csubpx CS ', IC, IXS, IYS, ILS,  CS(IC,IXS,IYS,ILS)
!MSK
!MSK ! Add dry deposition to sub grid (g/m2)
!MSK
!MSK          IF (IDV .EQ. 1)      &
!MSK         DDEPS(IC,IXS,IYS,ILS) = DDEPS(IC,IXS,IYS,ILS) +      &
!MSK                                 DDRVV*AREAV/(DXS*DYS)
!MSK
!MSK ! Add wet deposition to sub grid (g/m2)
!MSK
!MSK          IF (IWV .EQ. 1)      &
!MSK         WDEPS(IC,IXS,IYS,ILS) = WDEPS(IC,IXS,IYS,ILS) +      &
!MSK                                 WDRVV*AREAV/(DXS*DYS)
!MSK
!MSK ! Within main grid subgrid cell
!MSK
!MSK          ENDIF

! Within main grid cell

          ENDIF

! Next compound

  220 CONTINUE  

! Next plume segment subgrid cell

  210 CONTINUE  
  200 CONTINUE  

! Next compound

  230 CONTINUE

! Next plume segment

  100 CONTINUE  

      RETURN

! End of subroutine CSUBPX

      end subroutine csubpx
