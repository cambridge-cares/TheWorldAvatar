! <mod_emep03.f90 - A component of the City-scale
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

      module mod_emep03

!******************************************************************
! ***
! *** Module of the chemistry solver on the Eulerian grid
! ***
!******************************************************************
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
!           2016  M. Karl: Moved functions OptAir,RH2CONC,SH2CONC,RC3B and other
!                          rate constant function from EMEP45.for to this module
!           2017  M. Karl: Copied rate constant function from the EMEP/MSC-W model
!                          open source code to this module.
!                          These functions are used in EMEP45_NEW and CC70BIO.  
!    25 Apr 2018  M. Karl: L77    New parameter jrmax for number photolysis reactions
!    19 Aug 2019  M. Karl: Added function RCGROUND(T,SR) for ground source of HONO
!
! ----------------------------------------------------------------------------------

!MSK start
      integer,parameter :: rcmax = 300
      integer,parameter :: jrmax = 25
!MSK end

! rc - Reaction rates

!MSK start
!MSK    real rc(rcmax)
      double precision,dimension(rcmax) :: rc
!MSK end


!  Reaction rates (  1- 39 -> inorganic chem. rates in molecules cm-3 s-1):
!  Reaction rates ( 40- 59 -> aerosol form. rates in molecules cm-3 s-1):
!  Reaction rates ( 60-159 -> organic chem. rates in molecules cm-3 s-1):
!  Reaction rates (160-179 -> dry deposition rates in molecules cm-3 s-1):
!  Reaction rates (180-199 -> wet deposition rates in molecules cm-3 s-1):
!  Reaction rates (200-219 -> emissions in molecules cm-3 s-1):
!  Reaction rates (221-254 -> dms reactions in molecules cm-3 s-1):

! dj - Photodissociation values
      
!MSK start
!MSK      real dj(16)
      double precision, dimension(jrmax) :: dj

!MSK end

      contains


!MSK copied here form emep03.for and emep45.for
! *********************************************************************
      REAL FUNCTION OptAir(ZenAng)

! The subroutine calculates the optical air mass as a function of
! zenith angle

      REAL ZenAng

! ZenAng - Zenith angle

      REAL Pi
      REAL Rad
      integer i

      PARAMETER (Pi = 3.1415927)
      PARAMETER (Rad = Pi/180.0)

! Pi  - The mathematical constant Pi
! Rad - Degrees to radians

! Values of the optical air mass for ZenAng = 60-89

      REAL OptA(60:89)

      DATA (OptA(i), i = 60, 89) &
       /2.00, 2.06, 2.12, 2.20, 2.27, 2.36, 2.45, 2.55, 2.65, 2.78, &
        2.90, 3.06, 3.21, 3.40, 3.59, 3.83, 4.07, 4.40, 4.72, 5.16, &
        5.60, 6.18, 6.88, 7.77, 8.90,10.39,12.44,15.36,19.79,26.96/  

      IF (ZenAng .LT. 60.) THEN
        OptAir = 1./COS(Rad*ZenAng)
      ELSEIF (ZenAng .LT. 89.) THEN
        OptAir = OptA(INT(ZenAng))*(INT(ZenAng) + 1 - ZenAng) +  &
                OptA(INT(ZenAng) + 1)*(ZenAng - INT(ZenAng))
      ELSE 
        OptAir = OptA(89)
      ENDIF

      RETURN

! End of subroutine OptAir

      END FUNCTION
! *********************************************************************

!MSK copied here form emep03.for and emep45.for
! *********************************************************************

      REAL FUNCTION RH2CONC(RH,T)

! The subroutine calculates the concentration of water vapor (molec/cm3)
!C from the relative humidity and temperature
!
! RH - Relative humidity (%)
! T  - Temperature (K)
      real XZ,ESAT,H2O,RH,T

      XZ   = (597.3 - 0.57*(T - 273.16))*18./1.986*(1./T - 1./273.16)
      ESAT = 6.1078*EXP(-XZ)
!MSK start
! from fraction to percentage
      RH   = RH * 100.0
      RH   = max(RH, 10.0)
!MSK end
      H2O  = RH*ESAT/(1.38E-17*T)

      RH2CONC = H2O

      RETURN

! End of subroutine RH2CONC

      END FUNCTION


      REAL FUNCTION SH2CONC(SH,T,P)

! The subroutine calculates the concentration of water vapor (molec/cm3)
! from the specific humidity and temperature

! SH - Specific humidity (%)
! T  - Temperature (K)
! P  - Pressure (mb)

      real SH,T,P
      real H2O,pv

      pv = (P*SH)/0.622
      H2O  = pv/(T * 1.38E-19)

      SH2CONC = H2O

      RETURN

! End of subroutine RH2CONC

      END FUNCTION


      double precision FUNCTION RC3B(k0,k8,Fc)

! The subroutine calculates the reaction rates for three
! body and unimolecular decomposition processes

      real k0,k8,Fc

      RC3B = k0*k8/(k0 + k8)*Fc**(1./(1. + LOG10(k0/k8)**2))

      RETURN

! End of subroutine RC3B

      END FUNCTION

! *********************************************************************
!MSK copied here from emep45.for
! *********************************************************************
! The subroutines below are not used yet

      REAL FUNCTION ARRH(A,ER,T)

! Calculates reaction rates on the standard Arrhenius form
! k = A*EXP(-E/(R*T))

      REAL A
      REAL ER
      REAL T

! A  - Coefficient
! ER - E/R
! T  - Atmos. temp (K)
      
      ARRH = A*EXP(-ER/T)

      RETURN

! End of subroutine ARRH

      END FUNCTION
! *********************************************************************
      REAL FUNCTION K_CO_OH(M)

      REAL M

! M - Atm. molecular air density (molec/cm3)
      
      K_CO_OH = 1.3e-13*(1 + 0.6*M/2.55e19)

      RETURN

! End of subroutine K_CO_OH

      END FUNCTION
! *********************************************************************
      REAL FUNCTION K_HO2_HO2(H2O,T,M)

! Calculates the reaction rate HO2 + HO2 -> H2O2. This consists of
! two paths, both giving H2O2 which is added together here.

! According to David Simpson the factor FH2O should be included to
! take into account the enhancement in rate when water is present

      REAL H2O
      REAL T
      REAL M

! H2O - Water vapour concentration (cm-3)
! M   - Atm. molecular air density (molec/cm3)
! T   - Atm. temp. (K)

! Local variables

      REAL FH2O

!MSK start
      real Reac1, Reac2
!MSK end

! Calculate enhancement factor (Ref. D. Simpson)

      FH2O  = (1.0 + 1.4e-21*H2O*EXP(2200.0/T))

      Reac1 = FH2O*2.3e-13*EXP( 600.0/T)
      Reac2 = FH2O*1.7e-33*EXP(1000.0/T)*M

      K_HO2_HO2 = Reac1 + Reac2

      RETURN

! End of subroutine K_HO2_HO2

      END FUNCTION
! *********************************************************************
      REAL FUNCTION K_HO2_RO2()

! Generic rate for all HO2 + RO2 -> reactions
    
      K_HO2_RO2 = 1.0e-11

      RETURN

! End of subroutine K_HO2_RO2

      END FUNCTION
! *********************************************************************
      REAL FUNCTION K_RO2_NO(T)

! Arrhennius expression used for all RO2 + NO -> reactions

      REAL T

! T - Atm. temp. (K)
      
      K_RO2_NO = 4.2e-12*EXP(180.0/T)

      RETURN

! End of subroutine K_RO2_NO

      END FUNCTION
! *********************************************************************
      REAL FUNCTION K_RO2H_OH_a(T)

! Arrhennius expression used for all RO2H + OH -> RO2 reactions

      REAL T

! T - Atm. temp. (K)
      
      K_RO2H_OH_a = 1.9e-12*EXP(190.0/T)

      RETURN

! End of subroutine K_RO2H_OH_a

      END FUNCTION
! *********************************************************************
      REAL FUNCTION K_RO2H_OH_b(T)

! Arrhennius expression used for (nearly) all RO2H + OH -> carbonyl + ... reactions

      REAL T

! T - Atm. temp. (K)
      
      K_RO2H_OH_b = 5.8e-12*EXP(190.0/T)

      RETURN

! End of subroutine K_RO2H_OH_b

      END FUNCTION
! *********************************************************************
      REAL FUNCTION TROE(k0c,pot0,k8c,pot8,Fc,M,T)

! Calculates the reaction rates for three body and unimolecular
! decomposition processes

      REAL k0c
      REAL pot0
      REAL k8c
      REAL pot8
      REAL Fc
      REAL M
      REAL T

! K0c, k8c - The constant factor in the expressions
! M        - Atm. molecular air density (molec/cm3)
! T        - Atm. temp. (K)

! Local variables

      REAL k0
      REAL k0M
      REAL k8

! k0, k8 - Low and high pressure limits

! Note that we use T0c/T compared to IUPACs T/T0c which means that the
! sign of the exponent (pot) should be switched relative to IUPAC!

      k0  = k0c*(300./T)**pot0
      k0M = k0*M
      k8  = k8c*(300./T)**pot8

      TROE = (k0M*k8/(k0M + k8))*Fc**(1./(1. + LOG10(k0M/k8)**2))

      RETURN

! End of subroutine TROE

      END FUNCTION
! *********************************************************************
      REAL FUNCTION TROE_N2O5(M,T)

! Calculates the reaction rates for three body and unimolecular
! decomposition processes

      REAL M
      REAL T

! M - Atm molecular air density (molec/cm3)
! T - Atm. temp. (K)

! Local variables

      REAL k0c
      REAL pot0
      REAL k0
      REAL k8c
      REAL pot8
      REAL k8
      REAL Fc
      REAL k0M

! k0, k8   - Low and high pressure limits
! K0c, k8c - The constant factor in the expressions 

! Note that we use T0c/T compared to IUPACs T/T0c which means that the
! sign of the exponent (pot) should be switched relative to IUPAC!

      k0c  = 1.0e-3
      pot0 = 3.5
      k8c  = 9.70e14
      pot8 = -0.1
      Fc   = 0.33

      k0  = k0c*((300./T)**pot0)*EXP(-11000/T)
      k0M = k0*M
      k8  = k8c*((300./T)**pot8)*EXP(-11080/T)

      TROE_N2O5 = (k0M*k8/(k0M + k8))*Fc**(1./(1. + LOG10(k0M/k8)**2))

      RETURN

! End of subroutine TROE_N2O5

      END FUNCTION
! *********************************************************************
      REAL FUNCTION TROE_OP_O2(k0c,pot0,M,T)

! Calculates the reaction rates for three body reaction O(P) + O2 -> O3  
! Equal to the low pressure limit formula, k0, in a troe expression

      REAL k0c
      REAL pot0
      REAL M
      REAL T

! K0c  - The constant factor in the low limit, k0
! pot0 - The exponent in the troe expression
! M    - Atm. molecular air density (molec./cm3)
! T    - Atm. temp. (K)

! Note that we use T0c/T compared to IUPACs T/T0c which means that the
! sign of the exponent (pot) should be switched relative to IUPAC!

      TROE_OP_O2 = k0c*M*(300./T)**pot0

      RETURN

! End of subroutine TROE_OP_O2

      END FUNCTION
! *********************************************************************
      REAL FUNCTION TROE_PAN(k0c,T0c,k8c,T8C,Fc,M,T)

! Calculates the reaction rates for the thermal decomposition 
! of PAN (CH3CO(O2)NO2 -> CH3CO(O2) + NO2)

      REAL k0c
      REAL T0c
      REAL k8c
      REAL T8c
      REAL Fc
      REAL M
      REAL T

! K0c, k8c - The constant factor in the k-expressions 
! T0c, T8c - The ref. temp used  in the k-expressions, normally 300K
!            but not always (as e.g. PAN) 
! M        - Atm. molecular air density (molec/cm3)
! T        - Atm. temp. (K)

      REAL k0
      REAL k0M
      REAL k8

! k0, k8 - Low and high pressure limits
 
! Note that we use T0c/T compared to IUPACs T/T0c which means that the
! sign of the exponent (pot) should be switched relative to IUPAC

      k0  = k0c*EXP(T0c/T)
      k0M = k0*M
      k8  = k8c*EXP(T8c/T)

      TROE_PAN = (k0M*k8/(k0M + k8))*Fc**(1./( 1. + LOG10(k0M/k8)**2))

      RETURN

! End of subroutine TROE_PAN

      END FUNCTION

! *********************************************************************
! ******* Functions from EMEP open source (ChemFunctions.f90 **********

  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  ! KMT3 uses air concenrtation (M) and inverse Temp (tinv) from Zmet
  !
! *********************************************************************
      REAL FUNCTION kmt3(M,TINV,a1,c1,a3,c3,a4,c4)

        real  :: M,TINV
        real  :: a1,c1,a3,c3,a4,c4
        real  ::  k1, k3, k4

       k1 = a1 * EXP(C1*tinv)
       k3 = a3 * EXP(C3*tinv)
       k4 = a4 * EXP(C4*tinv)

       kmt3 = k1 + (k3*M)/(1.0+(k3*M)/k4)
   
       !print *,'kmt3',M,tinv,k1,k3,k4,a1,a3,a4,kmt3

      RETURN

! End of subroutine KMT3

      END FUNCTION


! *********************************************************************
      REAL FUNCTION IUPAC_troe(k0,kinf,Fc,M,N)

  !+ Calculates Troe expression 
  ! -----------------------------------------------------------
  ! rb note - this isn't checked or optimised yet. Taken from
  ! Atkinson et al. ACP 2006, 6, 3625-4055. 

  ! Input arguments are intended to represent:
  !   M may be O2+N2 or just N2 or just O2.
  ! NOTE that in the IUPAC nomenclature k0 already contains [M] so the k0(IUPAC)=k0*M here
  !   N=[0.75-1.27*log10(Fc)]

     real   :: k0,kinf,Fc,M,N
     !-- local
     real :: x,y, K0M               ! temp variable

        k0M = k0 * M
     

     !- use the power function replacement, m**n == exp(n*log m) 
     !-k0M   = a*(T/300.0)**(-2.3) * M
     !-kinf = p*(T/300.0)**(-1.4)

     ! k0M   = a * exp( b*log(t/300.0) ) * M
     ! kinf = p * exp( q*log(t/300.0) )

        ! factors for Fc:
        y    = k0M/kinf    ! used also below
        x    = log10(y)/N
        x    = 1.0/( 1.0 + x*x )

     !- F**x == exp(x*logF)

          IUPAC_troe = k0M / ( 1.0 + y) * exp(x*log(Fc))

      RETURN

! End of subroutine IUPAC_troe

      END FUNCTION
!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


! *********************************************************************
! ******* Functions for Heterogeneous Reactions on Aerosol   **********
! ******* and other Surfaces                                 **********

! *********************************************************************
      REAL FUNCTION RCGROUND(T,SR)

  !+ Ground surface source of HONO in urban areas
  ! -----------------------------------------------------------
  !  Reference:
  !  Zhang, L., Wang, T. Zhang, Q., Zheng, J., Xu, Z., Lv, M. (2016)
  !  Potential sources of nitrous acid (HONO) and their impacts on ozone: 
  !   A WRF-Chem study in a polluted subtropical region
  !   J. Geophys. Res., 121, pp. 3645-3662, doi: 10.1002/2015JD024468.
  ! -----------------------------------------------------------
  !  The heterogneous reation of 2 NO2 with H2O on surfaces to give HONO
  !    Kg(R5) = 1/8 * v_NO2 * (Sg/V) * gamma_g-NO2 [Li et al., 2010]
  !  Constant at night but enhanced by sunlight during day.
  !  T  is air temperature (K)
  !  SR is total solar radiation (w/m^2)
  !+ note: the dependency on light intensity is used during day
  !        when solar radiation exceeds 400 W/m^2.

     real            :: T,SR
     real            :: velNO2                    ! m/s
     real, parameter :: msNO2 = 46.0              ! g/mol
     real, parameter :: surface_to_volume = 0.3   ! 1/m (urban areas)
     real, parameter :: gammaNO2_night = 1.0e-6   ! ---


  ! Calculate mean velocity of NO2 in air [m/s]
          velNO2 = sqrt( (3.0*8314.7*T)/(msNO2) )

          if (SR .lt. 400.0) then

            RCGROUND = (1./8.)*velNO2*surface_to_volume*gammaNO2_night

          else

            RCGROUND = (1./8.)*velNO2*surface_to_volume*  &
                       2.e-5*(SR/400.0)

          endif

          !print *,'RCGROUND:',T,SR,velNO2,RCGROUND

    ! The surface reaction can only take place in the first vertical layer

      RETURN

! End of subroutine RCGROUND

      END FUNCTION
!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


! *********************************************************************

      subroutine FreeEmep03Memory()

!DEC$ ATTRIBUTES DLLEXPORT,STDCALL,ALIAS:'FreeEmep03Memory' :: FreeEmep03Memory

      implicit none

      rc = 0
      dj = 0

! End of subroutine FreeEmep03Memory

      end subroutine

      end module mod_emep03
