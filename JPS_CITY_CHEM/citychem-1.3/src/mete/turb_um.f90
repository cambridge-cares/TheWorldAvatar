! <turb_ebudget.f90 - A component of the City-scale
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

      subroutine turb_um(kappa,grav,c_p,R_dair,P_ref,                  &
                        MOM_FLUX,SHEAT_FLUX,LHEAT_FLUX,MOIST_FLUX,     &
                        HMIX,P_mb,T_inK,Q,                             &
                        BUOY_FLUX,USTAR,THSTAR,THvSTAR,QSTAR,MOBUL)

!   --------------------------------------------------------------------
!      Subroutine to calculate Monin-Obukhov scaling variables based on
!      the UM calculated: Surface momentum flux, Surface sensible
!      heat flux, Surface latent heat flux, Planetary boundary layer 
!      height, and top of lower layer temperature (Kelvin), top of lower
!      layer specific humidity (kg vapour/kg moist air), and top of 
!      lower layer pressure (in mb). 
!
!      INPUT:
!            kappa      : von Karmans constant            [Dimless]
!            grav       : Acceleration due to gravity     [m/s2]
!            c_p        : Specific heat cap. at const. p. [J/(kg K)]
!            R_dair     : Gas constant for dry air.       [J/(kg K)]
!            P_ref      : Reference pres. (1000 mb)       [mb]
!            MOM_FLUX   : Surface momentum flux           [N/m2]
!            SHEAT_FLUX : Surface sensible heat flux      [W/m2]
!            LHEAT_FLUX : Latent heat flux                [W/m2]
!            MOIST_FLUX : Surface moisture flux           [kg/(m2s)]
!   
!            HMIX       : Planetary boundary layer height [m]
!            P_mb       : UM-Pressure at top of layer 1   [mb]  
!            T_inK      : UM-Temp. at top of layer 1      [K]
!            Q          : UM-Spec. hum. at top of layer 1 [kg/kg]
!
!      OUTPUT:
!            BUOY_FLUX  : Surface bouancy flux            [W/m2]
!            USTAR      : Surface friction velocity       [m/s]
!            THSTAR     : Surface temperature scale       [K]
!            THvSTAR    : Surface buoyancy temp. scale    [K]
!            QSTAR      : Surface humidity scale [kg vap/kg moist air]
!            MOBUL      : Surface Monin-Obukhov length    [m]
!
!   --------------------------------------------------------------------
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
!    xx Dat 201x  Name: Line  Description of Change
!
! ----------------------------------------------------------------------------------

          implicit none

! *** Global variables:
      real :: kappa
      real :: grav
      real :: c_p
      real :: R_dair
      real :: P_ref
      real :: MOM_FLUX
      real :: SHEAT_FLUX
      real :: LHEAT_FLUX
      real :: MOIST_FLUX
      real :: HMIX
      real :: P_mb
      real :: T_inK
      real :: Q

      real :: BUOY_FLUX
      real :: USTAR
      real :: THSTAR
      real :: THvSTAR
      real :: QSTAR
      real :: MOBUL

! *** Local variables (can be redefined as Output variables if needed):

      real :: Tv_inK

      real :: T2TH
      real :: TH_inK
      real :: THv_inK

      real :: rho_air

!     real :: lambda
!     real :: T_Celsius
!     real :: BOWEN_ratio

      real :: kin_sheat_flux
      real :: kin_moist_flux
      real :: kin_buoy_flux

! *** Calculate Virtual temperature (K) [Garratt, (eq. 2.24) page 22]:  
          Tv_inK   = T_inK * (1.0 + (0.608 * Q))

! *** Compute Density of moist air [Garratt, (eq. 2.25) page 22]:
      rho_air  = (P_mb*100.0)/(R_dair * Tv_inK) 

      T2TH  = R_dair/c_p
      T2TH  = (P_ref/P_mb)**T2TH

!***  Calculate the Potential temperature [Garratt, (eq. 2.26) page 22]:
        TH_inK   = T_inK  * T2TH

!***  Calculate the Potential virtual temperature [Garratt, (eq. 2.27) page 22]:
        THv_inK  = Tv_inK * T2TH

! *** The surface friction velocity [Garratt, (eq. 1.3) and (eq. 1.7) page 10]:
      USTAR = SQRT(MOM_FLUX/rho_air) 

!! *** Estimating the latent heat of vaporization of water:
!      T_Celsius = T_inK - 273.15
!      if(T_Celsius >= 0)then
!! ***   See Eq (2.48) page 31 in Jacobsen (1999):
!        lambda = 2.501E+6 - (2370.0 * T_Celsius)
!      else
!! ***   Alternatively, Eq (2.50) page 31 in Jacobsen (1999):
!        lambda = 2.835E+6 - (T_Celsius*(340.0 + (10.46*T_Celsius)))
!      endif

!      lambda = LHEAT_FLUX / MOIST_FLUX

!      BOWEN_ratio = SHEAT_FLUX / LHEAT_FLUX

! *** The kinematic sensible heat flux [Garratt, (eq. 1.4) page 10]:	
      kin_sheat_flux = SHEAT_FLUX / (rho_air * c_p)

! *** The kinematic moisture flux [Garratt, (eq. 1.6) page 10]:
!      kin_moist_flux = LHEAT_FLUX / (rho_air * lambda)
      kin_moist_flux = MOIST_FLUX / rho_air

! *** Kinematic bouyancy flux [Garratt, (eq. 2.79) page 36]:
      kin_buoy_flux =   kin_sheat_flux                            &
                    + ( 0.61 * TH_inK * kin_moist_flux)

! *** The bouyancy flux [Garratt, (eq. 1.5) page 10]:
      BUOY_FLUX = rho_air * c_p * kin_buoy_flux

! *** Temperature (sensible heat flux) scale [Garratt: (eq. 1.8) page 10]: 
      THSTAR  = - kin_sheat_flux / USTAR

! *** Temperature (buoyancy flux) scale [Garratt: (Eq. 1.9) page 10]:
      THvSTAR = - kin_buoy_flux  / USTAR

! *** Humidity scale  [Garratt, (eq. 1.10) page 10]:
      QSTAR   = - kin_moist_flux / USTAR

! *** Monin-Obukhov length [Garratt, (eq. 1.11) page 10]:
!_LHS_Change_08Feb2012_Start:
      if ( abs(THvSTAR) .LT. 1.0E-10 ) THvSTAR = 1.0E-11
!_LHS_Change_08Feb2012_End. 
      MOBUL = (USTAR * USTAR * THv_inK)/(kappa * grav * THvSTAR)
 
      RETURN

!_End of subroutine TURB_UM.

      end subroutine turb_um
