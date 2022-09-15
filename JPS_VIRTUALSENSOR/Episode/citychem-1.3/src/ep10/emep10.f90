! <emep10.f90 - A component of the City-scale
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
!* ========================================== 
!*
!*****************************************************************************!
!*
!*        CITY-scale CHEMistry Transport Extension
!*
!*        Copyright (C) 2018  Matthias Steffen Karl
!*
!*        Contact Information: 
!*            Institute of Coastal Research
!*            Helmholtz-Zentrum Geesthacht
!*            Max-Planck-Str. 1
!*            21502 Geesthacht
!*            Germany
!*            email:  matthias.karl@hzg.de
!*
!*      EPISODE-CityChem, developed at Helmholtz-Zentrum Geesthacht (HZG) is designed
!*      for treating complex atmospheric chemistry in urban areas (Karl, 2018). The model
!*      is an extension of the EPISODE dispersion model to enable chemistry/transport
!*      simulations of reactive pollutants on city scale. EPISODE is an Eulerian dispersion
!*      model developed at the Norwegian Institute for Air Research (NILU) appropriate for
!*      air quality studies at the local scale (Slørdal et al. 2003 &2008). The model is an
!*      open source code subject to the Reciprocal Public License ("RPL") Version 1.5,
!*      https://opensource.org/licenses/RPL-1.5.
!*
!*        Reference:
!*      Karl, M. (2018):  Development of the city-scale chemistry transport model 
!*      CityChem-EPISODE and its application to the city of Hamburg, 
!*      Geosci. Model Dev. Discuss.,
!*      https://doi.org/10.5194/gmd-2018-8, 2018.
!*
!*****************************************************************************!

      subroutine EMEP10(DTP,MSPEC,NSPEC,CP,DDEP,WDEP,                   &
                        SITELAV,SITELOV,IX,IY,IZ,IR,                    &
                        YEARV,MNTHV,DAYMV,HOURV,MINUV,SECOV,DAYYV,      &
                        TAIRV,DTDZV,RHUMV,PRECV,CLOUV,MOBULV,USTARV,    &
                        DDEPVV,WDEPSRV,HINVD,HINVW,IDRY,ILAND)

!       2016 Matthias Karl, HZG, This is an original CityChem subroutine
!       The subroutine performs photochemical calculations for all irregular
!       receptor points using the new EMEP10 photochemistry scheme.
!       Short-lifed radicals (OD,OP,OH) are treated as quasi-stationary C=P/L
!       Routines for dry and wet depositon are not actively used (DryDep10 and
!       WetDEp10).
!       It uses the Gauss-Seidel iterative solver from Walker et al.(2003).
!       The subroutine BoxChem10 is adopted from the EMEP03 BoxChem03.
!       This solver integrates the photochemistry equations
!       using a variable time step by using a combination of the
!       implicit Euler method and a BDF second order formulation
!       and by using the Gauss-Seidel iterative numerical method
!       for solving the non-linear (implicit) equations.
!       Walker, S.-E., Solberg, S., Denby, B. (2003): Development and 
!         implementation of a simplified EMEP photochemistry scheme for
!         urban areas in EPISODE, NILU TR 13/2003. ISBN 82-425-1524-7.
!
! ----------------------------------------------------------------------------------
!       M. Karl Changed 04.05.2016:
! *** Reaction (R1) OP + O2 --> O3    activated (this is the only O3-producing reaction !!!)
! *** Reaction (R2) OD + M  --> OP         not directly produce O3 (but OP)
! *** Photolysis (DJ3)  NO2 --> OP + NO    not directly produce O3 (but OP)
! *** Photolysis (DJ14) NO3 --> OP + NO2   not directly produce O3 (but OP)
! *** Photolysis (DJ2)   O3 --> OP    activated as O3 loss
! *** Reaction (R3) OP + NO --> NO2   deactivated as O3 loss
! ----------------------------------------------------------------------------------
! REVISION HISTORY
!
!    03 May 2018  M. Karl: L598        DT2, prevent division by zero if ERRW=0
!    04 May 2018  M. Karl: L184        set emission rates to zero: RC(200), RC(201),
!                                      RC(205),RC(207)
!    04 May 2018  M. Karl:             deleted routine Emissions10
!    04 May 2018  M. Karl: L969        RC(166) and ddG in routine DryDep10
!    04 May 2018  M. Karl: L969        RC(180) and wdA in routine WetDep10
!    22 Oct 2018  M. Karl:             H2O2 removed but HNO3 added to EMEP10-Plume
!    22 Oct 2018  M. Karl: L1020       RC(163) and ddD in routine DryDep10
!    22 Oct 2018  M. Karl: L1047       RC(181) and wdB in routine WetDep10
!    22 Oct 2018  M. Karl: L233        set O2 mixing ratio
!    22 Oct 2018  M. Karl: L241        O2 included in call to CalcRC10
!    22 OCt 2018  M. Karl: L313-323    new indices from chemspecs10.inc
!
! ----------------------------------------------------------------------------------

      use mod_emep03
      use mod_conc
      use mod_mete    !PSIH(ETA)
      use mod_phot

      implicit none

      INTEGER MSPEC

      REAL    :: DTP
      double precision, dimension(MSPEC) :: CP
      REAL    :: DDEP(MSPEC)
      REAL    :: WDEP(MSPEC)
      REAL    :: SITELAV
      REAL    :: SITELOV
      REAL    :: TAIRV
      REAL    :: DTDZV
      REAL    :: RHUMV
      REAL    :: SHUMV
      REAL    :: PRESV
      REAL    :: PRECV
      REAL    :: CLOUV
      REAL    :: MOBULV(0:1)
      REAL    :: USTARV(0:1)
      REAL    :: DDEPVV(MSPEC)
      REAL    :: WDEPSRV(MSPEC)
      REAL    :: HINVD
      REAL    :: HINVW

      INTEGER :: NSPEC
      INTEGER :: IX
      INTEGER :: IY
      INTEGER :: IZ
      INTEGER :: YEARV
      INTEGER :: MNTHV
      INTEGER :: DAYMV
      INTEGER :: HOURV
      INTEGER :: MINUV
      INTEGER :: SECOV
      INTEGER :: DAYYV
      INTEGER :: IDRY
      INTEGER :: ILAND

! CP      - Concentration values (mol/cm3)
! GCCOR   - Ground concentration correction factors
! DDEP    - Dry deposition array
! WDEP    - Wet deposition array
! SITELAV - Latitude (deg)
! SITELOV - Longitude (deg)
! TAIRV   - Air temperature (K)
! DTDZV   - Vertical temperature gradient (K/m)
! RHUMV   - Relative humidity (0-1)
! PRECV   - Precipitation in mm/h
! CLOUV   - Cloud fraction (0-1) 
! HMIXV   - Mixing height value
! MOBULV  - Monin-Obukhov length scale
! USTARV  - Friction velocity
! DDEPVV  - Dry deposition velocities
! WDEPSRV - Wet deposition scavenging ratios
! MSPEC   - Maximum number of photochemistry compounds 
! NSPEC   - Actual  number of photochemistry compounds
! YEARV   - Year
! MNTHV   - Month
! DAYMV   - Day of month
! HOURV   - Hour
! MINUV   - Minute
! SECOV   - Second
! IDRY    - Dry deposition indicators
! ILAND   - Land/sea indicators (land=1/sea=0)

! Local variables:
! dry and wet deposition is not used
!      double precision :: ddA,ddB,ddC,ddD,ddE,ddF,ddG,ddH
!      double precision :: wdA,wdB,wdC,wdD,wdE,wdF

      REAL :: H2O
      REAL :: M
      REAL :: O2
      REAL :: SunHr
      REAL :: WASH
      REAL :: ZenAng
      double precision :: phidif

! H2O    - Water vapour concentration (cm-3)
! M      - Molecular density of air (molec/cm3)
! SunHr  - Hour
! WASH   - Wet deposition wash value
! ZenAng - Zenith angle

      integer :: I,IC,NS,IR

! Set molecular density of air (molec/cm3)

      M = 2.55E19

! Set oxygen molecule concentration (molec/cm3)
      O2 = 0.2095*M
      
! Calculate water vapor concentration h2o (cm-3)

      if (MeteExternalData == 2) then
        H2O = SH2CONC(SHUMV,TAIRV,PRESV)
      else
        H2O = RH2CONC(RHUMV,TAIRV)
      endif

! Set photochemistry reaction coefficients

      CALL CalcRC10(M,H2O,O2,TAIRV)

! Set dissociation reaction coefficients

      SunHr = HOURV + (60.*MINUV + SECOV)/3600.

! Call new subroutine cphotrate
      call cphotrate(SITELAV,SITELOV,DAYYV,SunHr,ZenAng,CLOUV)

!MSK start
!MSK 04.05.2018 set the old emission rates to zero
      RC(200) = 0.0
      RC(201) = 0.0
      RC(205) = 0.0
      RC(207) = 0.0
!MSK end

! Calculate EMEP model photochemistry

      call BoxChem10(DTP,MSPEC,NSPEC,CP,M,H2O,IX,IY,IZ)

      RETURN

! End of subroutine EMEP10

      end subroutine emep10

      subroutine BoxChem10(DTP,MSPEC,NSPEC,CP,Mtot,H2Oconc,IX,IY,IZ)

! The subroutine integrates the photochemistry equations
! using a variable time step by using a combination of the
! implicit Euler method and a BDF second order formulation
! and by using the Gauss-Seidel iterative numerical method
! for solving the non-linear (implicit) equations.

      USE mod_emep03

      REAL DTP
      INTEGER MSPEC
      INTEGER NSPEC

      double precision, dimension(MSPEC) :: CP
      REAL Mtot
      REAL H2Oconc

      INTEGER IX
      INTEGER IY
      INTEGER IZ

! DTP     - Total integration time step (s)
! MSPEC   - Maximum number of species
! NSPEC   - Actual number of species
! CP      - Species concentration (mol/cm3)
! Mtot    - Total molecular air density (mol/cm3)
! H2Oconc - Water vapour concentration
! IX      - Main grid cell index in x-direction
! IY      - Main grid cell index in y-direction
! IZ      - Main grid cell index in z-direction

      INTEGER NrGSEul
      INTEGER NrGSBDF

! NrGSEul - Number of Gauss-Seidel iterations for the implicit Euler   method
! NrGSBDF - Number of Gauss-Seidel iterations for the BDF second order method

! new from chemspecs10.inc
integer, parameter :: &
       OP          =   1  &
      , OD          =   2  &
      , OH          =   3  &
      , HO2         =   4  &
      , O3          =   5  &
      , NO          =   6  &
      , NO2         =   7  &
      , HNO3        =   8  &
      , HCHO        =   9  &
      , CO          =  10

      INTEGER M
      INTEGER O2
      INTEGER H2O

      PARAMETER (M   = 11)
      PARAMETER (O2  = 12)
      PARAMETER (H2O = 13)

      REAL ATOL(MSPEC)
      REAL RTOL(MSPEC)

      double precision F(MSPEC)
      double precision X(MSPEC+3,0:2)
      double precision W(MSPEC)
      double precision ERRVEC(MSPEC)
      double precision ERRW

      REAL ATOLF
      REAL DTMin
      REAl DTMax
      REAL DTLeft
      double precision DT1
      double precision DT2
      REAL RDT
      REAL A1
      REAL A2
      REAL A3
      REAL A4
      double precision Xmin
      double precision P,L

      INTEGER INITV
      INTEGER NFAIL

      real    :: Ldt
      integer :: NS,I


! If zero timestep then we need to do nothing

      IF (DTP .LE. 0.) RETURN

! Set number of Gauss-Seidel iterations of the
! implicit Euler and the BDF second order method

      NrGSEul = 2
      NrGSBDF = 2

! Set absolute (mol/cm3) and relative (%/100) error
! tolerances

!MSK      DO NS = 1,NSPEC
      DO NS = 1,MSPEC
        RTOL(NS) = 0.10
        ATOL(NS) = 0.0
      ENDDO

      ATOLF = 1.0


! new from chemspecs10.inc
      !+
      ! Assigns ATOL
      !+
      ATOL(OP          ) = 1.00e+15*ATOLF 
      ATOL(OD          ) = 1.00e+15*ATOLF 
      ATOL(OH          ) = 1.00e+15*ATOLF 
      ATOL(HO2         ) = 1.00e+15*ATOLF 
      ATOL(O3          ) = 5.00e+10*ATOLF 
      ATOL(NO          ) = 2.50e+08*ATOLF 
      ATOL(NO2         ) = 2.50e+09*ATOLF 
      ATOL(HNO3        ) = 2.50e+08*ATOLF 
      ATOL(HCHO        ) = 2.50e+09*ATOLF 
      ATOL(CO          ) = 1.00e+15*ATOLF


! Set minimum and maximum time step (s)

      DTMin = 0.1
      DTMax = DTP

! Set initial concentrations

!MSK start
!MSK      DO NS = 1,NSPEC
      DO NS = 1,MSPEC
        if (CP(NS) .gt. 0.0) then
          X(NS,1) = CP(NS)
        else
          X(NS,1) = 0.0
        endif
      ENDDO
!MSK end

     ! print *,'emep10 X(O3,1)', X(O3,1), X(5,1), CP(5)

! Setting some extra concentrations used by the
! dc/dt, implicit Euler and 2nd order BDF formulations

      X(H2O,1) = H2Oconc
      X(M  ,1) = Mtot
      X(O2 ,1) = 0.2095*Mtot

      X(H2O,2) = H2Oconc
      X(M  ,2) = Mtot
      X(O2 ,2) = 0.2095*Mtot

! Set time step left initially

      DTLeft = DTP

! Main total time step loop

  100 CONTINUE

! Set number of failures to zero

      NFAIL = 0

! Set initialization (or restart) indicator

      INITV = 1

! Calculate W-vector

!MSK      DO NS = 1,NSPEC
      DO NS = 1,MSPEC
        W(NS) = ATOL(NS) + RTOL(NS)*X(NS,1)
      ENDDO

! Calculate the tendency vector F = dc/dt
!MSK start initialize tendency vector
!MSK      DO NS = 1,NSPEC
      DO NS = 1,MSPEC
        F(NS) = 0.
      ENDDO
      if ( X(O3,1) .lt. 1.e-28 )  X(O3,1) = 1.e-28 

      INCLUDE 'dcdt10.inc'

     ! print *,'emep10 X(O3,1) after dcdt10.inc', X(1,1),X(4,1), CP(4)

     ! print *,'emep10 X(O3,1) after dcdt10', X(O3,1), X(5,1), CP(5) 

! Calculate first time step

      DT1 = DTLeft

!MSK      DO NS = 4,NSPEC
      DO NS = 4,MSPEC

        if ( ABS(F(NS)) .lt. 1.e-28 )    F(NS) = 0. 

        IF (F(NS) .NE. 0.) THEN
          DT1 = MIN(DT1,W(NS)/ABS( real(F(NS)) ))
        ENDIF

      ENDDO

! Reduce first time step if larger than what is left
! of total time step

      IF (DT1 .GT. DTLeft) THEN
        DT1 = DTLeft
      ENDIF

! Reduce first time step if larger than maximum set

      IF (DT1 .GT. DTMax) THEN
        DT1 = DTMax
      ENDIF

! Increase first time step if smaller than minimum set

      IF (DT1 .LT. DTMin) THEN
        DT1 = DTMin
      ENDIF
 
! OK, now we have the first time step

! Set initial iterate

!MSK      DO NS = 1,NSPEC
      DO NS = 1,MSPEC
        X(NS,2) = X(NS,1)
      ENDDO

    !  print *,'emep10 X(O3,1) init iter', X(4,1), X(4,2), X(1,2)


! Integrate the differential equation over the first time step
! by using the implicit Euler equation and by using a number
! of iterations with the Gauss-Seidel iterative technique

      Ldt = DT1

      if ( ABS(X(NS,2)) .lt. 1.e-28 )  X(NS,2) = 0. 

      DO I = 1,NrGSEul
        INCLUDE 'euler10.inc'
      ENDDO

    !  print *,'emep10 X(O3,2) after euler10.inc', X(O3,2),  X(OP,2) , X(O2,2)

! OK, found new accepted concentrations

! Update the time step left

      DTLeft = DTLeft - DT1

! Are we finished?

      IF (DTLeft .LT. DTMin) GOTO 999

! No!

  200 CONTINUE

! Tentatively set second time step equal to first

      IF (INITV .EQ. 1) THEN
        DT2 = DT1
      ENDIF

! Reduce second time step if larger than what is left
! of total time step

      IF (DT2 .GT. DTLeft) THEN
        DT2 = DTLeft
      ENDIF

! Reduce second time step if larger than maximum set

      IF (DT2 .GT. DTMax) THEN
        DT2 = DTMax
      ENDIF

! Increase second time step if smaller than minimum set

      IF (DT2 .LT. DTMin) THEN
        DT2 = DTMin
      ENDIF
 
! OK, now we have the second time step

! Update (shift) the concentration array

      IF (NFAIL .EQ. 0) THEN

!MSK      DO NS = 1,NSPEC
      DO NS = 1,MSPEC
        X(NS,0) = X(NS,1)
        X(NS,1) = X(NS,2)
      ENDDO

      ENDIF

! Calculate coefficients used by the BDF two-step
! formulation for variable time step

      RDT = DT1/DT2

      A1 = (RDT + 1.)*((RDT + 1.)/RDT)
      A2 = -1./RDT
      A3 = RDT + 1.
      A4 = RDT + 2.

! Calculate initial iterate by simple extrapolation

!MSK      DO NS = 1,NSPEC
      DO NS = 1,MSPEC
        X(NS,2) = X(NS,1) + (X(NS,1) - X(NS,0))/RDT
      ENDDO

! Check initial iterate

!MSK      DO NS = 1,NSPEC
      DO NS = 1,MSPEC

! If negative concentrations then restart the whole integration
! procedure by using the implicit Euler method which is guaranteed
! to be positive

        IF (X(NS,2) .LT. 0.) GOTO 100

      ENDDO

! Integrate the differential equation over the second time step
! by using the second order BDF equation and by using a number
! of iterations with the Gauss-Seidel iterative technique

      Ldt = DT2

      DO I = 1,NrGSBDF
        INCLUDE 'bdf10.inc'
      ENDDO

     ! print *,'emep10 X(O3,2) after bdf10.inc', X(5,2), CP(5)

! OK, found new concentrations

! Find smallest value

      Xmin = X(1,2)
!MSK      DO NS = 2,NSPEC
      DO NS = 2,MSPEC
        Xmin = MIN(Xmin, X(NS,2) )
      ENDDO

! If negative concentrations then restart the whole integration
! procedure by using the implicit Euler method which is guaranteed
! to be positive

      IF (Xmin .LT. 0.) GOTO 100

! Calculate error vector

!MSK      DO NS = 1,NSPEC
      DO NS = 1,MSPEC
        ERRVEC(NS) = (2./(RDT*(RDT+1)))*               &
                    (RDT*X(NS,2) - (1. + RDT)*X(NS,1) - X(NS,0))
      ENDDO

! Calculate W-vector

!MSK      DO NS = 1,NSPEC
      DO NS = 1,MSPEC
        W(NS) = ATOL(NS) + RTOL(NS)*X(NS,1)
      ENDDO

! Calculate weighted error norm

      ERRW = 0.

!MSK      DO NS = 4,NSPEC
      DO NS = 4,MSPEC
        W(NS) = max(W(NS),dble(1.e-28))
        IF (ABS(ERRVEC(NS))/W(NS) .GT. ERRW) THEN
            ERRW = ABS(ERRVEC(NS))/W(NS)
        ENDIF
      ENDDO

! Do we accept this second time step?

      IF (ERRW .LT. 1.0) THEN

! Yes!

! Update the time step left

        DTLeft = DTLeft - DT2

! Are we finished?

        IF (DTLeft .LT. DTMin) GOTO 999

! Calculate new first and second time step

        DT1 = DT2

        if (ERRW.eq.0.0) then
           DT2 = dble(2.0)*DT2
        else
           DT2 = MAX(dble(0.5),MIN(dble(2.0),dble(0.8)/SQRT(ERRW)))*DT2
        endif

! Set initialization (restart) indicator and number of
! failures to zero and go up and perform a new second
! order BDF integration

        INITV = 0
        NFAIL = 0
        GOTO 200

      ELSE

! No!

! Number of failures increased by 1

        NFAIL = NFAIL + 1

! If two failures then restart the whole integration
! procedure by using the implicit Euler method

        IF (NFAIL .EQ. 2) GOTO 100

! Calculate new second time step

        DT2 = MAX(dble(0.5),MIN(dble(2.0),dble(0.8)/SQRT(ERRW)))*DT2

! Set initialization (restart) indicator to zero and
! go up and try a new second order BDF integration

        INITV = 0
        GOTO 200

      ENDIF

! No!

  999 CONTINUE

! Set new concentrations

!MSK      DO NS = 1,NSPEC
      DO NS = 1,MSPEC
        if ( X(NS,2) .lt. 1.e-28 )  X(NS,2) = 0. 
        CP(NS) = real( X(NS,2) )
      ENDDO

    !  print *,'emep10 X(O3,1) new', X(4,2), CP(4)

      RETURN

! End of subroutine BoxChem

      end subroutine BoxChem10


      SUBROUTINE CalcRC10(M,H2O,O2,TEMP)

! The subroutine calculates the photochemistry reaction rates

      USE mod_emep03

!  Total molecular density

      REAL M
      REAL H2O
      REAL O2
      REAL TEMP

! M   - Molecular air density (mol/cm3)
! H2O - Water vapour concentration (1% V/V)
! T   - Temperature (K)

! Local variables

      REAL CM
      INTEGER I
      real :: Log300divT
      real :: LogTdiv300
      real :: TINV

      DO I = 1,199
        RC(I) = 0.0
      ENDDO

      ! CM = M*1.E-30

      Log300divT = LOG(300./TEMP) 
      LogTdiv300 = LOG(TEMP/300.) 
      TINV       = 1.0/TEMP 

! from chemrates10.inc
! Generated by GenChem.py - DO NOT EDIT

      RC(1) = 5.67e-34*(TEMP/300.0)**(-2.8)*M*O2
      RC(2) = 0.21*3.2e-11*M*EXP(67.0*TINV)+0.79*1.8e-11*M*EXP(107.0*TINV)
      RC(3) = IUPAC_TROE(1.0e-31*EXP(1.6*LOG300DIVT),  &
            & 3.0e-11*EXP(-0.3*LOG300DIVT),  &
            & 0.85,  &
            & M,  &
            & 0.75-1.27*LOG10(0.85))
      RC(4) = 2.2e-10*H2O
      RC(5) = 1.4e-12*EXP(-1310.0*TINV)
      RC(6) = 1.7e-12*EXP(-940.0*TINV)
      RC(7) = 2.03e-16*EXP(-4.57*LOG300DIVT)*EXP(693.0*TINV)
      RC(8) = 3.6e-12*EXP(270.0*TINV)
      RC(9) = IUPAC_TROE(3.3e-30*EXP(3.0*LOG300DIVT),  &
            & 4.1e-11,  &
            & 0.40,  &
            & M,  &
            & 0.75-1.27*LOG10(0.4))
      RC(10) = 1.25e-17*(TEMP**2)*EXP(615.0*TINV)
      RC(11) = 1.44e-13+M*3.43e-33
      RC(12) = RC(160)
      RC(13) = RC(162)
      RC(14) = RC(163)
      RC(15) = RC(166)
      RC(16) = RC(181)
      RC(17) = RC(182)

!old
!      RC(1) = 5.67E-34*(T/300.)**(-2.8)*M ! IUPAC, 2001
!      RC(2) = 0.21*3.2E-11*EXP(+67./T) +     &
!             0.79*1.8E-11*EXP(+107./T) ! IUPAC, 2001
!      RC(3) = RC3B(1.0E-31*(T/300.)**(-1.6)*M,    &
!                  3.0E-11*(T/300.)**0.3,0.85) ! IUPAC, 2001
!      RC(4) = 2.2E-10 ! IUPAC, 2001
!      RC(5) = 1.4E-12*EXP(-1310./T) ! IUPAC, 2001
!      RC(6) = 1.4E-13*EXP(-2470./T) ! IUPAC, 2001
!      RC(7) = 1.7E-12*EXP(-940./T) ! IUPAC, 2001
!      RC(8) = 2.03E-16*EXP(693./T)*(T/300)**4.57 ! IUPAC, 2001
!      RC(9) = 1.8E-11*EXP(+110./T) ! IUPAC, 2001
!      RC(10) = 3.6E-12*EXP(+270./T) ! IUPAC, 2001
!!C    RC(11) = 4.5E-14*EXP(-1260./T) ! DeMore, 1992
!      RC(12) = RC3B(3.6E-30*(T/300.)**(-4.1)*M,   &
!                   1.9E-12*(T/300.)**(0.2),0.35) ! IUPAC, 2001
!      RC(13) = RC3B(2.6E-30*(T/300.)**(-2.9)*M,4.1E-11,0.4) ! DeMore, 1992
!      RC(17) = RC3B(1.0E-3*(T/300.)**(-3.5)*EXP(-11000./T)*M,   &
!                      9.7E14*(T/300.)**(0.1)*EXP(-11080./T),0.35) ! IUPAC, 2001
!      RC(20) = 7.7E-12*EXP(-2100./T) ! IUPAC, 2001
!      RC(21) = 4.8E-11*EXP(+250./T) ! IUPAC, 2001
!      RC(22) = 2.9E-12*EXP(-160./T) ! IUPAC, 2001
!      RC(24) = (2.2E-13*EXP(+600./T) + 1.5E-33*EXP(+980./T)*M)*   &
!                 (1. + 1.4E-21*EXP(+2200./T)*H2O ) ! IUPAC, 2001
!      RC(25) = 2.4E-14*EXP(+460./T) + 6.5E-34*EXP(+1335./T)*M/    &
!              (1. + 6.5E-34*EXP(+1335./T)*M/                     &
!              (2.7E-17*EXP(+2199./T))) ! IUPAC, 2001
!      RC(26) = RC3B(4.0E-31*(T/300.)**(-3.3)*M,2.E-12,0.45) ! IUPAC, 2001
!      RC(27) = 4.0E-17 ! EMEP
! Aerosol formation
!      RC(40) = 5.0E-6*M/2.55E19 ! EMEP
!      RC(41) = 5.0E-6*M/2.55E19 ! EMEP
!      RC(42) = 5.0E-6*M/2.55E19 ! EMEP
!      RC(43) = 5.0E-6*M/2.55E19 ! EMEP
!      RC(45) = 5.0E-6*M/2.55E19 ! EMEP
! Methane chemistry
!      RC(60) = 1.85E-12*EXP(-1690./T) ! IUPAC, 2001
!      RC(61) = 2.8E-12*EXP(+285./T) ! IUPAC, 2001
!      RC(64) = 1.4E-13 + 3.1E-12*EXP(-360./T) ! IUPAC, 2001
!      RC(66) = 3.8E-13*EXP(+780./T) ! IUPAC, 2001
!      RC(67) = 1.0E-12*EXP(+190./T) ! EMEP
!      RC(68) = 1.9E-12*EXP(+190./T) ! EMEP
!      RC(69) = 8.2E-12*EXP(+40./T) ! IUPAC, 2001
!      RC(70) = 5.8E-16 ! IUPAC, 2001 
!      RC(71) = 1.3E-13*(1. + 0.6*(M/2.55E19)*(300./T)) ! IUPAC, 2001
! Ethane chemistry
!      RC(80) = 6.9E-12*EXP(-1000./T) ! IUPAC, 2001
!      RC(82) = 2.5E-12*EXP(+380./T) ! IUPAC, 2001
!      RC(84) = 4.4E-12*EXP(+365./T) ! IUPAC, 2001
!      RC(86) = RC3B(2.7E-28*(T/300.)**(-7.1)*M,   &
!                   1.2E-11*(T/300.)**(-0.9),0.3) ! IUPAC, 2001
!      RC(87) = RC3B(5.5E-3*EXP(-12064./T)*M,      &
!                   3.9E16*EXP(-13628./T),0.3) ! LACTOZ, 1992
!      RC(89) = 7.8E-12*EXP(+300./T) ! IUPAC, 2001
!      RC(93) = 4.1E-12*EXP(-70./T) ! IUPAC, 2001
! N-buthane chemistry
!      RC(100) = 9.1E-12*EXP(-405./T) ! IUPAC, 2001
!      RC(101) = 4.2E-12*EXP(180./T) ! EMEP, 2002
!      RC(103) = 1.3E-12*EXP(-25./T) ! IUPAC, 2001
!      RC(104) = 5.0E-12 ! Lesclaux, 1992
! Ethene chemistry
!      RC(110) = RC3B(7.E-29*(T/300.)**(-3.1)*M,9.E-12,0.48) ! IUPAC, 2001
!      RC(111) = 4.2E-12*EXP(180./T) ! EMEP, 2002
!      RC(113) = 9.1E-15*EXP(-2580./T) ! IUPAC, 2001
! Propene chemistry
!      RC(120) = RC3B(8.0E-27*(T/300.)**(-3.5)*M,3.E-11,0.5) ! IUPAC, 2001
!      RC(121) = 4.2E-12*EXP(180./T) ! EMEP, 2002
!      RC(123) = 5.5E-15*EXP(-1880./T) ! IUPAC, 2001
! O-xylene chemistry
!      RC(130) = 1.4E-11 ! Atkinson, 1990
!      RC(131) = 4.0E-12 ! Lesclaux, 1992
!      RC(132) = 5.6E-11 ! Bierbach, 1994
!      RC(133) = 9.0E-12 ! Lesclaux, 1992
!      RC(134) = 1.7E-11 ! EMEP
!      RC(135) = 1.2E-11 ! EMEP
! Isoprene chemistry
!      RC(140) = 2.7E-11*EXP(+390./T) ! IUPAC, 2001
!      RC(141) = 4.2E-12*EXP(+180./T) ! Assumed equal to rate 121
!      RC(142) = 4.1E-12*EXP(+453./T) ! Paulson and Seinfeld, 1992
!      RC(143) = 1.4E-12*EXP(-180./T) ! Paulson and Seinfeld, 1992

      RETURN

! End of subroutine CalcRC

      END

      SUBROUTINE OldCalcRC10(M,H2O,T)

! The subroutine calculates the photochemistry reaction rates

      USE mod_emep03

!  Total molecular density

      REAL M
      REAL H2O
      REAL T

! M   - Molecular air density (mol/cm3)
! H2O - Water vapour concentration (1% V/V)
! T   - Temperature (K)

! Local variables

      REAL CM
      INTEGER I

      DO I = 1,199
        RC(I) = 0.0
      ENDDO

      CM = M*1.E-30

!  Inorganic chemistry

      RC(1) = 6.0E-34*(T/300.)**(-2.3)*M ! DeMore, 1992
      RC(2) = 0.21*3.2E-11*EXP(+70./T) +            &
             0.79*1.8E-11*EXP(+110./T) ! DeMore, 1992
      RC(3) = RC3B(9.0E-32*(T/300.)**(-1.5)*M,      &
                  3.0E-11,0.6) ! DeMore, 1992
      RC(4) = 2.2E-10 ! DeMore, 1992
      RC(5) = 2.0E-12*EXP(-1400./T) ! DeMore, 1992
      RC(6) = 1.2E-13*EXP(-2450./T) ! DeMore, 1992
      RC(7) = 1.6E-12*EXP(-940./T) ! DeMore, 1992
      RC(8) = 1.1E-14*EXP(-500./T) ! DeMore, 1992
      RC(9) = 1.5E-11*EXP(+170./T) ! DeMore, 1992
      RC(10) = 3.7E-12*EXP(+250./T) ! DeMore, 1992
      RC(11) = 4.5E-14*EXP(-1260./T) ! DeMore, 1992
      RC(12) = RC3B(2.2E-30*(T/300.)**(-3.9)*M,      &
                   1.5E-12*(T/300.)**(-0.7),0.6) ! DeMore, 1992
      RC(13) = RC3B(2.6E-30*(T/300.)**(-3.2)*M,      &
                   2.4E-11*(T/300.)**(-1.3),0.6) ! DeMore, 1992
      RC(17) = 2.5E+26*EXP(-10930./T)*RC(12) ! DeMore, 1992
      RC(20) = 5.5E-12*EXP(-2000./T) ! DeMore, 1992
      RC(21) = 4.8E-11*EXP(+250./T) ! DeMore, 1992
      RC(22) = 2.9E-12*EXP(-160./T) ! DeMore, 1992
      RC(24) = (2.3E-13*EXP(+600./T) +                 &
               1.7E-33*EXP(+1000./T)*M)*               &
              (1. + 1.4E-21*EXP(+2200./T)*H2O) ! DeMore, 1992
      RC(25) = 7.2E-15*EXP(+785./T) + 1.9E-33*EXP(+725./T)*M/        &
              (1. + 1.9E-33*EXP(+725./T)*M/                          &
              (4.1E-16*EXP(+1440./T))) ! DeMore, 1992
      RC(26) = RC3B(3.0E-31*(T/300.)**(-3.3)*M,                      &
                   1.5E-12,0.6) ! DeMore, 1992
      RC(27) = 4.0E-17 ! EMEP

! Aerosol formation

      RC(40) = 5.0E-6*M/2.55E19 ! EMEP
      RC(41) = 5.0E-6*M/2.55E19 ! EMEP
      RC(42) = 5.0E-6*M/2.55E19 ! EMEP
      RC(43) = 5.0E-6*M/2.55E19 ! EMEP
      RC(45) = 5.0E-6*M/2.55E19 ! EMEP

! Methane chemistry

      RC(60) = 2.9E-12*EXP(-1820./T) ! DeMore, 1992
      RC(61) = 4.2E-12*EXP(+180./T) ! DeMore, 1992
      RC(62) = 3.0E-14*EXP(+416./T) ! Lightfoot, 1992
      RC(63) = 6.1E-14*EXP(+416./T) ! Lightfoot, 1992
      RC(64) = 6.7E-12*EXP(-600./T) ! DeMore, 1992
      RC(66) = 4.1E-13*EXP(+790./T) ! Lightfoot, 1992
      RC(67) = 1.0E-12*EXP(+190./T) ! EMEP
      RC(68) = 1.9E-12*EXP(+190./T) ! EMEP
      RC(69) = 1.6E-11*EXP(-110./T) ! EMEP
      RC(70) = 5.8E-16 ! DeMore, 1992
      RC(71) = 1.5E-13*(1. + 0.6*M/2.55E19) ! DeMore, 1992

! Ethane chemistry

      RC(80) = 8.7E-12*EXP(-1070./T) ! DeMore, 1992
      RC(82) = 8.9E-12 ! Lightfoot, 1992
      RC(84) = 6.0E-12*EXP(+250./T) ! DeMore, 1992
      RC(86) = RC3B(2.7E-28*(T/298.)**(-7.1)*M,                         &
                   1.2E-11*(T/298.)**(-0.9),0.3) ! Lightfoot, 1992 
      RC(87) = RC3B(5.5E-3*EXP(-12064./T)*M,                            &
                   3.9E16*EXP(-13628./T),0.3) ! LACTOZ, 1992
      RC(89) = 2.0E-11 ! Lightfoot, 1992
      RC(90) = 3.1E-5*EXP(-3870./T)/(2.2E6*EXP(-3870./T) + 1.) ! Horie, 1992
      RC(91) = 2.8E-12*EXP(+530./T) ! Lightfoot, 1992
      RC(93) = 7.0E-12*EXP(-235./T) ! Atm. Environ 25A, 1991

! N-buthane chemistry

      RC(100) = 1.51E-17*T**2*EXP(190./T) ! Atkinson, 1990
      RC(101) = 4.1E-12 ! Lesclaux, 1992
      RC(103) = 1.62E-18*T**2*EXP(414./T) ! Atkinson, 1990
      RC(104) = 5.0E-12 ! Lesclaux, 1992

! Ethene chemistry

      RC(110) = RC3B(1.0E-28*(T/300.)**(-0.8)*M,                  &
                    8.8E-12,0.6) ! DeMore, 1992
      RC(111) = 9.0E-12 ! Becker, 1991
      RC(113) = 1.2E-14*EXP(-2630./T) ! DeMore

! Propene chemistry

      RC(120) = RC3B(3.0E-27*(T/298.)**(-3.0)*M,                            &
                    2.8E-11*(T/298.)**(-1.3),0.6) ! Atkinson, 1990
      RC(121) = 9E-12 ! Lesclaux, 1992
      RC(123) = 4.0E-15*EXP(-1900./T) ! DeMore

! O-xylene chemistry

      RC(130) = 1.4E-11 ! Atkinson, 1990
      RC(131) = 4.0E-12 ! Lesclaux, 1992
      RC(132) = 5.6E-11 ! Bierbach, 1994
      RC(133) = 9.0E-12 ! Lesclaux, 1992
      RC(134) = 1.7E-11 ! EMEP
      RC(135) = 1.2E-11 ! EMEP

! Isoprene chemistry

      RC(140) = 2.55E-11*EXP(+410./T) ! Paulson and Seinfeld, 1992
      RC(141) = 9E-12 ! Assumed equal to rate 121
      RC(142) = 4.1E-12*EXP(+453./T) ! Paulson and Seinfeld, 1992
      RC(143) = 1.4E-12*EXP(-180./T) ! Paulson and Seinfeld, 1992

      RETURN

! End of subroutine OldCalcRC

      END



!      SUBROUTINE DryDep(ddA,ddB,ddC,ddD,ddE,ddF,ddG,ddH)
      SUBROUTINE DryDep10(ddA,ddB,ddC,ddD,ddG)
      
! The subroutine sets dry deposition reaction rates (s-1)

!MSK      USE mod_emep45
      USE mod_emep03

      double precision ddA,ddB,ddC,ddG,ddD !,ddE,ddF,ddH

! Set dry deposition reaction rates (s-1)

      RC(160) = ddA ! O3
      RC(161) = ddB ! H2O2
      RC(162) = ddC ! NO2
      RC(163) = ddD ! HNO3
!!     RC(164) = ddE ! H2      
!      RC(167) = ddE ! PAN
!      RC(165) = ddF ! CH3O2H
      RC(166) = ddG ! CO
!      RC(168) = ddG ! SO2
!      RC(169) = ddH ! Sulphate

      RETURN

! End of subroutine DryDep

      END

!      SUBROUTINE WetDep(wdA,wdB,wdC,wdD,wdE,wdF)
      SUBROUTINE WetDep10(wdA,wdB,wdC)
!HCHO
! The subroutine sets wet deposition reaction rates (s-1)

      USE mod_emep03

!      double precision wdA,wdB,wdC,wdD,wdE,wdF
      double precision wdA,wdB,wdC

! Set wet deposition reaction rates (s-1)

      RC(180) = wdA ! H2O2
      RC(181) = wdB ! HNO3
      RC(182) = wdC ! HCHO
!      RC(183) = wdD ! CH3O2H
!!     RC(184) = wdE ! CH3COCHO
!!     RC(185) = wdF ! HCOCHO
!      RC(186) = wdE ! SO2
!      RC(187) = wdF ! Sulphate

      RETURN

! End of subroutine WetDep

      END
