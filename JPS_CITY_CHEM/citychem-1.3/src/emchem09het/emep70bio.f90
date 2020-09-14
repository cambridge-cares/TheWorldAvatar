! <emep70bio.f90 - A component of the City-scale
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

      subroutine EMEP70BIO(DTP,MSPEC,NSPEC,CP,                          &
                        SITELAV,SITELOV,IX,IY,IZ,                       &
                        YEARV,MNTHV,DAYMV,HOURV,MINUV,SECOV,DAYYV,      &
                        MeteExternalDataa,TAIRV,DTDZV,RHUMV,SHUMV,      &
                        PRESV,PRECV,CLOUV,TSRADV,MOBULV,USTARV,         &
                        HINVD,HINVW,IDRY,ILAND,                         &
                        JNO2V,JO3V,RC1V,RC3V,RC5V,RC6V,RC11V,RC32V,RC36V)

!       2018 Matthias Karl, HZG, This is an original CityChem subroutine
!       The subroutine performs photochemical calculations using the
!       CC70BIO scheme for the main grid.
!       CC70BIO contains BVOC and the S/IVOC gas species.
!       Now 25 photolysis reactions are defined.
!       Short-lifed radicals (OD,OP,OH) are not treated as quasi-stationary.
!       Chemistry mechanism CC70BIO is generated by GenChem kinetic
!       preprocessor. All reactions and species are also created dynamically
!       and listed in the include files chemrates70.inc and chemspecs70.inc.
!       Dry deposition and wet deposition removed because grid concentration
!       is updated for deposition losses in module gmdepo, which is called 
!       by module tsgrid at every time step.
!       It uses the Gauss-Seidel iterative solver from Walker et al.(2003).
!       The subroutine BoxChem integrates the photochemistry equations
!       using a variable time step by using a combination of the
!       implicit Euler method and a BDF second order formulation
!       and by using the Gauss-Seidel iterative numerical method
!       for solving the non-linear (implicit) equations.
!       No changes were made to the solver procedure.
!
!       Walker, S.-E., Solberg, S., Denby, B. (2003): Development and 
!         implementation of a simplified EMEP photochemistry scheme for
!         urban areas in EPISODE, NILU TR 13/2003. ISBN 82-425-1524-7.  
!
! ----------------------------------------------------------------------------------
! REVISION HISTORY
!
!    24 Apr 2018  M. Karl: created subroutine emep70bio
!    19 Aug 2019  M. Karl: extended call to CalcRC70 by TSRADV
!
! ----------------------------------------------------------------------------------

      use mod_emep03, only : rc,dj
      use mod_emep03, only : SH2CONC, RH2CONC
      use mod_mete    !PSIH(ETA)
      use mod_phot

      implicit none

      INTEGER MSPEC

      REAL    :: DTP
      double precision, dimension(MSPEC) :: CP
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
      REAL    :: HINVD
      REAL    :: HINVW
      REAL    :: TSRADV
      REAL    :: JNO2V
      REAL    :: JO3V
      REAL    :: RC1V
      REAL    :: RC3V
      REAL    :: RC5V
      REAL    :: RC6V
      REAL    :: RC11V
      REAL    :: RC32V
      REAL    :: RC36V

      INTEGER :: MeteExternalDataa
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

! External functions:
!MSK now in mod_emep03
!MSK      REAL :: RH2CONC
!MSK      REAL :: SH2CONC
!MSK      double precision :: DEPSIH
!MSK  rc() and dj() defined in mod_emep03

! Local variables:

      REAL :: H2O
      REAL :: M
      REAL :: O2
      REAL :: SunHr
      REAL :: ZenAng
      
      double precision :: phidif

! H2O    - Water vapour concentration (cm-3)
! M      - Molecular density of air (molec/cm3)
! SunHr  - Hour
! WASH   - Wet deposition wash value
! ZenAng - Zenith angle

      integer :: I,IC,NS

! Set molecular density of air (molec/cm3)

      M = PRESV/(TAIRV*1.3807E-19)
      
! Calculate water vapor concentration h2o (cm-3)

      if (MeteExternalData == 2) then
        H2O = SH2CONC(SHUMV,TAIRV,PRESV)
      else
        H2O = RH2CONC(RHUMV,TAIRV)
      endif

! Set oxygen molecule concentration (molec/cm3)
      O2 = 0.2095*M

! Set photochemistry reaction coefficients

      CALL CalcRC70(M,H2O,O2,TAIRV,TSRADV)

! HONO ground source only in first layer
      if (IZ.ne.1) then
        RC(24) = 0.0
      endif

! Set dissociation reaction coefficients

      SunHr = HOURV + (60.*MINUV + SECOV)/3600.

      if (TSRADFE) then
          if (TSRADV.eq.0.0) then
             ! Nighttime cloud cover not relevant
             CLOUV = 1.0
          else
             CLOUV = cloudfraction(SunHr,DAYMV,SITELAV,TSRADV)
          endif
       endif

! Call new subroutine cphotrate
      call cphotratej25(SITELAV,SITELOV,DAYYV,SunHr,ZenAng,CLOUV)

! Return photolysis rate of NO2 and R(OP+O2)
      JNO2V = DJ(3)
      JO3V  = DJ(1)+DJ(2)
      RC1V  = RC(1)
      RC3V  = RC(3)
      RC5V  = RC(5)
      RC6V  = RC(6)
      RC11V = RC(11)
! HAVE TO CHANGE WHEN ACTIVATING HET (RC23++)
      !RC32V = RC(32)   ! CO + OH
      !RC36V = RC(38)   ! CH3COO2 + NO2
      RC32V = RC(33)
      RC36V = RC(39) 

! Calculate EMEP model photochemistry

      call BoxChem70(DTP,MSPEC,NSPEC,CP,M,H2O,IX,IY,IZ)

      RETURN

! End of subroutine EMEP45

      end subroutine emep70bio



      subroutine BoxChem70(DTP,MSPEC,NSPEC,CP,Mtot,H2Oconc,IX,IY,IZ)

! The subroutine integrates the photochemistry equations
! using a variable time step by using a combination of the
! implicit Euler method and a BDF second order formulation
! and by using the Gauss-Seidel iterative numerical method
! for solving the non-linear (implicit) equations.

      use mod_emep03, only : rc,dj

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


      REAL ATOL(MSPEC)
      REAL RTOL(MSPEC)

      double precision F(MSPEC)
      double precision X(MSPEC+3,0:2)   !+3 for M,O2,H2O
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

      INCLUDE 'chemspecs70.inc'

!! If zero timestep then we need to do nothing
!
!      IF (DTP .LE. 0.) RETURN
!
!! Set number of Gauss-Seidel iterations of the
!! implicit Euler and the BDF second order method
!
!      NrGSEul = 2
!      NrGSBDF = 2
!
!! Set absolute (mol/cm3) and relative (%/100) error
!! tolerances
!
!      DO NS = 1,NSPEC
!        RTOL(NS) = 0.10
!      ENDDO
!
!      ATOLF = 1.0
!
!  ATOL(OP          ) = 1.00e+15*ATOLF 
!  ATOL(OD          ) = 1.00e+15*ATOLF 
!  ATOL(OH          ) = 1.00e+15*ATOLF 
!  ATOL(HO2         ) = 1.00e+15*ATOLF 
!  ATOL(CH3O2       ) = 1.00e+15*ATOLF 
!  ATOL(C2H5O2      ) = 1.00e+15*ATOLF 
!  ATOL(secC4H9O2   ) = 1.00e+15*ATOLF 
!  ATOL(IsopO2      ) = 1.00e+15*ATOLF 
!  ATOL(CH2O2CH2OH  ) = 1.00e+15*ATOLF 
!  ATOL(CH3CHO2CH2OH) = 1.00e+15*ATOLF 
!  ATOL(oXylOHO2    ) = 1.00e+15*ATOLF 
!  ATOL(CH3COCHO2CH3) = 1.00e+15*ATOLF 
!  ATOL(MemalO2     ) = 1.00e+15*ATOLF 
!  ATOL(MVKO2       ) = 1.00e+15*ATOLF 
!  ATOL(MTO2        ) = 1.00e+15*ATOLF 
!  ATOL(O3          ) = 5.00e+10*ATOLF 
!  ATOL(NO          ) = 2.50e+08*ATOLF 
!  ATOL(NO2         ) = 2.50e+09*ATOLF 
!  ATOL(ProdApinO3  ) = 2.50e+09*ATOLF 
!  ATOL(ProdApinNO3 ) = 2.50e+09*ATOLF 
!  ATOL(ProdApinOH  ) = 2.50e+09*ATOLF 
!  ATOL(ProdLimoO3  ) = 2.50e+09*ATOLF 
!  ATOL(ProdLimoNO3 ) = 2.50e+09*ATOLF 
!  ATOL(ProdLimoOH  ) = 2.50e+09*ATOLF 
!  ATOL(PAN         ) = 2.50e+09*ATOLF 
!  ATOL(NO3         ) = 1.00e+15*ATOLF 
!  ATOL(N2O5        ) = 1.00e+15*ATOLF 
!  ATOL(HNO3        ) = 2.50e+08*ATOLF 
!  ATOL(HONO        ) = 1.00e+15*ATOLF 
!  ATOL(CH3COO2     ) = 1.00e+15*ATOLF 
!  ATOL(MVKetone    ) = 1.00e+15*ATOLF 
!  ATOL(HCOCHO      ) = 1.00e+15*ATOLF 
!  ATOL(CH3COCHO    ) = 1.00e+15*ATOLF 
!  ATOL(Memaldial   ) = 1.00e+15*ATOLF 
!  ATOL(CH3COC2H5   ) = 1.00e+15*ATOLF 
!  ATOL(MTKetone    ) = 1.00e+15*ATOLF 
!  ATOL(HCHO        ) = 2.50e+09*ATOLF 
!  ATOL(CH3CHO      ) = 2.50e+09*ATOLF 
!  ATOL(C2H6        ) = 5.00e+09*ATOLF 
!  ATOL(nC4H10      ) = 5.00e+09*ATOLF 
!  ATOL(C2H4        ) = 2.50e+09*ATOLF 
!  ATOL(C3H6        ) = 2.50e+09*ATOLF 
!  ATOL(oXylene     ) = 2.50e+09*ATOLF 
!  ATOL(Isoprene    ) = 2.50e+08*ATOLF 
!  ATOL(Apinene     ) = 2.50e+08*ATOLF 
!  ATOL(Limonene    ) = 2.50e+08*ATOLF 
!  ATOL(CH3O2H      ) = 2.50e+09*ATOLF 
!  ATOL(C2H5OOH     ) = 1.00e+15*ATOLF 
!  ATOL(BURO2H      ) = 1.00e+15*ATOLF 
!  ATOL(ETRO2H      ) = 1.00e+15*ATOLF 
!  ATOL(PRRO2H      ) = 1.00e+15*ATOLF 
!  ATOL(OXYO2H      ) = 1.00e+15*ATOLF 
!  ATOL(MEKO2H      ) = 1.00e+15*ATOLF 
!  ATOL(MemalO2H    ) = 1.00e+15*ATOLF 
!  ATOL(MVKO2H      ) = 1.00e+15*ATOLF 
!  ATOL(ISRO2H      ) = 1.00e+15*ATOLF 
!  ATOL(MTO2H       ) = 1.00e+15*ATOLF 
!  ATOL(H2O2        ) = 2.50e+09*ATOLF 
!  ATOL(CH3COO2H    ) = 1.00e+15*ATOLF 
!  ATOL(CH3OH       ) = 2.50e+09*ATOLF 
!  ATOL(C2H5OH      ) = 2.50e+09*ATOLF 
!  ATOL(CH3COCH2OH  ) = 1.00e+15*ATOLF 
!  ATOL(H2          ) = 1.00e+15*ATOLF 
!  ATOL(CO          ) = 1.00e+15*ATOLF 
!  ATOL(CH4         ) = 1.00e+15*ATOLF 
!  ATOL(SO2         ) = 2.50e+09*ATOLF 
!  ATOL(Sulphate    ) = 2.50e+09*ATOLF 
!  ATOL(BLOC        ) = 2.50e+09*ATOLF 
!  ATOL(BSOC        ) = 2.50e+09*ATOLF 
!  ATOL(ALOC        ) = 2.50e+09*ATOLF    

! Set minimum and maximum time step (s)

      DTMin = 0.1
      DTMax = DTP

! Set initial concentrations

      DO NS = 1,NSPEC
        if (CP(NS) .gt. 1.e-20) then
          X(NS,1) = CP(NS)
        else
          X(NS,1) = 0.0
        endif
      ENDDO


     !print *,'emep70bio X(NO,1)', X(NO ,1), CP(NO )

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

      DO NS = 1,NSPEC
        W(NS) = ATOL(NS) + RTOL(NS)*X(NS,1)
      ENDDO

! Calculate the tendency vector F = dc/dt
!MSK: initialize tendency vector
      DO NS = 1,NSPEC
        F(NS) = 0.
      ENDDO
      !print *,'emep70bio before dcdt45 O3 ', H2Oconc,Mtot,X(H2O,1),X(O2,1),X(O3,1)
      if ( X(O3,1) .lt. 1.e-28 )  X(O3,1) = 1.e-28 

      INCLUDE 'dcdt70.inc'

      !debug
      !print *,'emep70bio X(NO,1) after dcdt45', X(NO ,1), CP(NO )

! Calculate first time step

      DT1 = DTLeft

      DO NS = 4,NSPEC
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

      DO NS = 1,NSPEC
        X(NS,2) = X(NS,1)
      ENDDO

      !debug
      !print *,'emep70bio X(NO,1) after init', X(NO ,1), CP(NO )
      !print *,'emep70bio X(NO,2) after init', X(NO ,2), CP(NO )

! Integrate the differential equation over the first time step
! by using the implicit Euler equation and by using a number
! of iterations with the Gauss-Seidel iterative technique

      Ldt = DT1

      if ( ABS(X(NS,2)) .lt. 1.e-28 )  X(NS,2) = 0. 

      DO I = 1,NrGSEul
        INCLUDE 'euler70.inc'
      ENDDO

      !debug
      !print *,'emep70bio X(NO,1) after euler45', X(NO ,1), CP(NO )
      !print *,'emep70bio X(NO,2) after euler45', X(NO ,2), CP(NO )

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

      DO NS = 1,NSPEC
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

      DO NS = 1,NSPEC
        X(NS,2) = X(NS,1) + (X(NS,1) - X(NS,0))/RDT
      ENDDO

! Check initial iterate

      DO NS = 1,NSPEC

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
        INCLUDE 'bdf70.inc'
      ENDDO

      !debug
      !print *,'emep70bio X(NO,1) after bdf45', X(NO ,1), CP(NO )
      !print *,'emep70bio X(NO,2) after bdf45', X(NO ,2), CP(NO )

! OK, found new concentrations

! Find smallest value

      Xmin = X(1,2)
      DO NS = 2,NSPEC
        Xmin = MIN(Xmin, X(NS,2) )
      ENDDO

! If negative concentrations then restart the whole integration
! procedure by using the implicit Euler method which is guaranteed
! to be positive

      IF (Xmin .LT. 0.) GOTO 100

! Calculate error vector

      DO NS = 1,NSPEC
        ERRVEC(NS) = (2./(RDT*(RDT+1)))*               &
                    (RDT*X(NS,2) - (1. + RDT)*X(NS,1) - X(NS,0))
      ENDDO

! Calculate W-vector

      DO NS = 1,NSPEC
        W(NS) = ATOL(NS) + RTOL(NS)*X(NS,1)
      ENDDO

! Calculate weighted error norm

      ERRW = 0.

      DO NS = 4,NSPEC
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
        DT2 = MAX(dble(0.5),MIN(dble(2.0),dble(0.8)/SQRT(ERRW)))*DT2

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

      DO NS = 1,NSPEC
        if ( X(NS,2) .lt. 1.e-28 )  X(NS,2) = 0. 
        CP(NS) = real( X(NS,2) )
      ENDDO

      RETURN

! End of subroutine BoxChem70

      end subroutine BoxChem70



      SUBROUTINE CalcRC70(M,H2O,O2,TEMP,SRAD)

! The subroutine calculates the photochemistry reaction rates

      use mod_emep03

!  Total molecular density

      real :: M
      real :: H2O
      real :: O2
      real :: TEMP
      real :: SRAD

! M   - Molecular air density (mol/cm3)
! H2O - Water vapour concentration (1% V/V)
! T   - Temperature (K)

! Local variables

      INTEGER I
      real :: Log300divT
      real :: LogTdiv300
      real :: TINV
 
      DO I = 1,199
        RC(I) = 0.0
      ENDDO

      Log300divT = LOG(300./TEMP) 
      LogTdiv300 = LOG(TEMP/300.) 
      TINV       = 1.0/TEMP 

        INCLUDE 'chemrates70.inc'

      RETURN

! End of subroutine CalcRC70

      end subroutine CalcRC70
