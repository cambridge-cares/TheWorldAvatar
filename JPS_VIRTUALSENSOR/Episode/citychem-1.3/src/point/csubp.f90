! <csubp.f90 - A component of the City-scale
!                 Chemistry Transport Model EPISDE-CityChem>
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

      subroutine CSUBP(IP,XRV,YRV,ZRV,TSV,NCV,MCV,CRV,DDRV,WDRV)

! The subroutine calculates subgrid scale concentrations and dry and
! wet depositions for one plume segment in the given receptor point.
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

      integer :: IP
      integer :: NCV
      integer :: MCV

      REAL XRV
      REAL YRV
      REAL ZRV
      REAL TSV
      REAL CRV(MCV)
      REAL DDRV(MCV)
      REAL WDRV(MCV)

! IP   - Plume segment index
! NCV  - Number of compounds
! MCV  - Maximum number of compounds
! XRV  - Receptor x-coordinate
! YRV  - Receptor y-coordinate
! ZRV  - Receptor z-coordinate
! TSV  - Timestep
! CRV  - Receptor concentration
! DDRV - Receptor dry deposition
! WDRV - Receptor wet deposition

! Local variables

      REAL ADD10
      REAL ADD1R
      REAL ADD20
      REAL ADD2R
      REAL AEROV
      REAL ALFAR
      REAL ALFA0
      REAL CRVR
      REAL CRVV
      REAL CRV0
      REAL DD
      REAL DDRVV
      REAL DELYW
      REAL DRV
      REAL DSZDXG
      REAL DSZDXR
      REAL DTDEPO
      REAL HMIXV
      REAL HV
      REAL FAC1
      REAL FF
      REAL FMAX1
      REAL FMAX2
      REAL FNEW
      REAL LAMBV
      REAL LXW
      REAL PRECV
      REAL QEMV
      REAL QHSV
      REAL QXV
      REAL QYV
      REAL QZV
      REAL SIGVV
      REAL SIGWV
      REAL SIGYR
      REAL SIGYV
      REAL SIGZG
      REAL SIGZR
      REAL SIGZV
      REAL TA
      REAL TADD
      REAL TF
      REAL TGV
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
      REAL XGV
      REAL XPV
      REAL XRW
      REAL YPV
      REAL YRW
      REAL ZPV

      integer :: ICV
      integer :: IQ
      integer :: IXP
      integer :: IYP
      integer :: IZP

! ADD10  - Addend in Gaussian plume segment formula
! ADD1R  - Addend in Gaussian plume segment formula
! ADD20  - Addend in Gaussian plume segment formula
! ADD2R  - Addend in Gaussian plume segment formula
! AEROV  - Aerodynamic resistance
! ALFAR  - Reflection coefficient for receptor point
! ALFA0  - Reflection coefficient for ground level
! CRVR   - Concentration in receptor point
! CRVV   - Concentration value
! CRV0   - Concentration value at ground level
! DD     - Wind direction
! DDRVV  - Dry deposition at reseptor point
! DELYW  - Delta Y in wind directed coordinate system
! DRV    - Distance from plume segment to receptor point
! DSZDXG - Derivative of sigma-z with respect to x
! DSZDXR - Derivative of sigma-z with respect to x
! DTDEPO - Timestep for deposition
! HMIXV  - Mixing height
! HV     - Plume segment effective height above terrain
! FAC1   - Factor in Gaussian plume formula
! FF     - Wind speed
! FMAX1  - Factor in Gaussian plume formula
! FMAX2  - Factor in Gaussian plume formula
! FNEW   - Used in calculating sigma-y and sigma-z
! LAMBV  - Wet deposition scavenging coefficient
! LXW    - Plume segment length
! PRECV  - Precipitation
! QEMV   - Plume segment emission
! QHSV   - Plume segment stack height
! QXV    - Plume segment stack x-coordinate
! QYV    - Plume segment stack y-coordinate
! QZV    - Plume segment stack z-coordinate
! SIGVV  - Sigma-v value
! SIGWV  - Sigma-w value
! SIGYR  - Plume segment sigma-y in receptor point
! SIGYV  - Plume segment sigma-y
! SIGZG  - Plume segment sigma-z
! SIGZR  - Plume segment sigma-z in receptor point
! SIGZV  - Plume segment sigma-z
! TA     - Used in calculating sigma-y and sigma-z
! TADD   - Used in calculating sigma-y and sigma-z
! TF     - Used in calculating sigma-y and sigma-z
! TGV    - Virtual time to plume reaches ground
! THETA  - Weight value
! TL     - Lagrangian time-scale value
! TL4    - Lagrangian time-scale value
! TNEW   - Used in calculating sigma-y and sigma-z
! TNUL   - Used in calculating sigma-y and sigma-z
! TOPRV  - Topography height at receptor point
! TTOT   - Total elapsed simulation time
! TY     - Used in calculating sigma-y and sigma-z
! TZ     - Used in calculating sigma-y and sigma-z
! TZ2    - Used in calculating sigma-y and sigma-z
! UU     - Wind u-component
! VDV    - Dry deposition velocity
! VV     - Wind v-component
! V0     - Used in calculating partial reflection coefficient
! WDRVV  - Wet deposition in receptor point
! XGV    - Virtual distance to plume reaches ground
! XPV    - Plume segment x-coordinate
! XRW    - Receptor point wind directed x-coordinate
! YPV    - Plume segment y-coordinate
! YRW    - Receptor point wind directed y-coordinate
! ZPV    - Plume segment z-coordinate
! ICV    - Compound index
! IQ     - Plume segment point source index
! IXP    - Plume segment main grid index in x-direction
! IYP    - Plume segment main grid index in y-direction
! IZP    - Plume segment main grid index in z-direction

! Function type declarations

!MSK      REAL EXP4     !(is in mod_util)
!MSK      REAL FTOP     !(is in mod_psrc)

! EXP4 - Exponential function
! FTOP - Topography height function

! Calculate total elapsed simulation time

      TTOT = THOUR*3600. + ITS*DT

! Calculate deposition timesteps for flux calculations and reduction
! of emission due to wet deposition

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

! Calculate minimum and maximum plume segment coordinates

      LXW = PU(6,IP)

! Calculate wind directed coordinate system rotation angle

      THETA = (270. - DD)*RAD

! Transform receptor coordinates to wind directed coordinates

      XRW = +COS(THETA)*(XRV - XPV) + SIN(THETA)*(YRV - YPV)
      YRW = -SIN(THETA)*(XRV - XPV) + COS(THETA)*(YRV - YPV)

! If plume segment does not contribute to receptor point then return

      IF (XRW .LT. 0. .OR. XRW .GT. LXW) GOTO 999

! Get plume segment point source index

      IQ = INT(PU(9,IP))

! Get plume segment point source coordinates and stack height

       QXV =  QX(IQ)
       QYV =  QY(IQ)
       QZV =  QZ(IQ)
      QHSV = QHS(IQ)

! Calculate distance from point source to receptor point relative to
! stack height

      DRV = SQRT((XRV - QXV)*(XRV - QXV) +      &
                (YRV - QYV)*(YRV - QYV))/QHSV

! Find receptor point topography height

      call ctopo(XRV,YRV,ZRV,TOPRV)

! Calculate efficient plume segment height above the terrain

      HV = ZPV - (TOPRV - QZV)*FTOP(DRV)
      HV = MAX(HV,0.)

! Calculate advection time from start of plume segment to receptor point

      TADD = XRW/FF

! Calculate growth of sigma-y (Irwin 1983)

      SIGYV = PU(3,IP)
      SIGVV = SIGV(IXP,IYP,IZP)
      TY    = SIGYV/SIGVV
      TF    = 0.9*TY/(2.*SQRT(1000.))
      TA    = TF + SQRT(TF*TF + TY)
      TNUL  = TA*TA
      TNEW  = TNUL + TADD
      FNEW  = 1./(1. + 0.9*SQRT(TNEW/1000.))
      SIGYV = SIGVV*TNEW*FNEW

      SIGYR = SIGYV

! Calculate growth of sigma-z (Venkatram 1984)

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

      SIGZR = SIGZV

! Calculate derivative of sigma-z with respect to x at receptor point

      DSZDXR = SIGZR*((TL4 + TNEW)/(TNEW*(TL4 + 2.*TNEW)))/FF

! Calculate virtual time and distance to where the plume reaches the
! ground (Overcamp, 1976)

      SIGZV = (HV/(ZRV + HV))*SIGZV
      SIGWV = SIGWP(IXP,IYP,IZP)
      TL    = TLGRP(IXP,IYP,IZP)
      TL4   = 4.*TL
      TZ    = SIGZV/SIGWV
      TZ2   = TZ*TZ
      TNUL  = TZ2/TL4 + SQRT(TZ2*TZ2/(TL4*TL4) + TZ2)

      TGV = TNUL
      XGV = FF*TGV

      SIGZG = SIGZV

! Calculate derivative of sigma-z with respect to x at the point where
! the plume reaches the ground

!MSK start
      TGV   = MAX(1.e-10, TGV)
      SIGZG = MAX(1.e-10, SIGZG)
!MSK end

      DSZDXG = SIGZG*((TL4 + TGV)/(TGV*(TL4 + 2.*TGV)))/FF

! Calculate compound independent Gaussian plume equation terms

      FMAX1 = 1./(2.*PI*FF*SIGYR*SIGZR)
      FMAX2 = 1./(SQRT(2.*PI)*FF*SIGYR)

      DELYW = YRW/SIGYR
      FAC1  = EXP4(-0.5*DELYW*DELYW)

      ADD10 = EXP4(-0.5*(( 0. - HV)/SIGZR)*(( 0. - HV)/SIGZR)) 
      ADD1R = EXP4(-0.5*((ZRV - HV)/SIGZR)*((ZRV - HV)/SIGZR)) 
      ADD20 = EXP4(-0.5*(( 0. + HV)/SIGZR)*(( 0. + HV)/SIGZR)) 
      ADD2R = EXP4(-0.5*((ZRV + HV)/SIGZR)*((ZRV + HV)/SIGZR)) 

! Get aerodynamic resistance (s/m), precipitation (mm/h) and mixing
! height (m)

!MSK start
      if (AERO(IXP,IYP) .lt. 1.e-9) AERO(IXP,IYP) = 0.0

      if (ADD1R .gt. 0.)  ADD1R = MAX(1.e-9, ADD1R)
      if (ADD10 .gt. 0.)  ADD10 = MAX(1.e-9, ADD10)
      if (ADD2R .gt. 0.)  ADD2R = MAX(1.e-9, ADD2R)
      if (ADD20 .gt. 0.)  ADD20 = MAX(1.e-9, ADD20)
      if (FAC1  .gt. 0.)  FAC1  = MAX(1.e-9, FAC1)
      if (FMAX1 .gt. 0.)  FMAX1 = MAX(1.e-9, FMAX1)

      if (ADD1R .lt. 0.)  ADD1R = MIN(-1.e-9,ADD1R)
      if (ADD10 .lt. 0.)  ADD10 = MIN(-1.e-9,ADD10)
      if (ADD2R .lt. 0.)  ADD2R = MIN(-1.e-9,ADD2R)
      if (ADD20 .lt. 0.)  ADD20 = MIN(-1.e-9,ADD20)
      if (ADD20 .lt. 0.)  ADD20 = MIN(-1.e-9,ADD20)
      if (FAC1  .lt. 0.)  FAC1  = MIN(-1.e-9,FAC1)
      if (FMAX1 .lt. 0.)  FMAX1 = MIN(-1.e-9,FMAX1)
!MSK end

      AEROV = AERO(IXP,IYP)
      PRECV = PREC(IXP,IYP)
      HMIXV = HMIX(IXP,IYP)

! Convert precipitation from mm/h to m/s

      PRECV = PRECV/3.6E+6

! Go through all compounds

      DO 100 ICV = 1,NCV

! Calculate dry deposition velocity (m/s)

          VDV = DDEPV(ICV)/(DDEPV(ICV)*AEROV + 1.)

! Calculate wet scavenging coefficient

          LAMBV = PRECV*WDEPSR(ICV)/HMIXV

! Get plume segment emission value

          QEMV = PU(9 + 2*ICV,IP)

! Adjust plume segment emission to receptor point

          QEMV = QEMV*EXP4(-LAMBV*TADD)


! Calculate dry deposition partial reflection coefficient for
! receptor point (Overcamp, 1976)

          V0 = VDV + FF*HV*DSZDXG/SIGZG

!MSK start
          if (QEMV .lt. 1.e-9) QEMV = 0.0
          if (V0   .lt. 1.e-9) V0   = 0.0
!MSK end
          IF (V0 .EQ. 0.) THEN
              ALFAR = 1.
          ELSE
              ALFAR = 1. - 2.*VDV/V0
          ENDIF

!MSK start
          if (ALFAR .gt. 0.)  ALFAR = MAX(1.e-9, ALFAR)
          if (ALFAR .lt. 0.)  ALFAR = MIN(-1.e-9,ALFAR)

! Calculate Gaussian concentration in receptor point (ug/m3)
! MSK new comment 23.01.2017
! Conversion from num/s to g/s   (num = number of particles)
! Number emission has the right time unit.
! We only have to take care that concentration CRV for PNC 
! Need to convert from number/m^3 to number/cm^3:

          IF ((IQU .EQ. 4).or.(CUNIT(ICV)(1:3) == 'num') ) then

           ! print *,'crvr ',ICV,QEMV,FMAX1,FAC1,ADD1R,ALFAR*ADD2R

            CRVR = 1.0E-6*QEMV*FMAX1*FAC1*(ADD1R + ALFAR*ADD2R)
          ELSE
            CRVR = 1.0E+6*QEMV*FMAX1*FAC1*(ADD1R + ALFAR*ADD2R)
          ENDIF
!MSK end

          CRVV = CRVR

! Calculate dry deposition partial reflection coefficient for ground
! level receptor point (Overcamp 1976)

          V0 = VDV + FF*HV*DSZDXR/SIGZR
          IF (V0 .EQ. 0.) THEN
              ALFA0 = 1.
          ELSE
              ALFA0 = 1. - 2.*VDV/V0
          ENDIF

! Calculate Gaussian concentration in ground level receptor point
! (g/m3)

          CRV0 = QEMV*FMAX1*FAC1*(ADD10 + ALFA0*ADD20)

! Calculate dry deposition flux in receptor point (g/m2*s)

          DDRVV = VDV*CRV0

! Calculate wet deposition flux in receptor point (g/m2*s)

          WDRVV = LAMBV*QEMV*FMAX2*FAC1

! Integrate dry and wet depositions over actual timestep (g/m2)

          DDRVV = DDRVV*DTDEPO
          WDRVV = WDRVV*DTDEPO

! Set concentrations and dry and wet depositions in receptor point

           CRV(ICV) =  CRVV
          DDRV(ICV) = DDRVV
          WDRV(ICV) = WDRVV

! Next compound

  100 CONTINUE  

      RETURN

  999 CONTINUE

! Set concentrations and dry and wet depositions in receptor point to
! zero and return

      DO 110 ICV = 1,NCV

           CRV(ICV) = 0.
          DDRV(ICV) = 0.
          WDRV(ICV) = 0.

  110 CONTINUE

      RETURN

! End of subroutine CSUBP

      end subroutine csubp
