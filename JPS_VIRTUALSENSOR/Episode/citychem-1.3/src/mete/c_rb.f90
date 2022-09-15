! <c_rb.f90 - A component of the City-scale
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

      subroutine c_rb(method,kappa,g_acc,PR,ustar,z0,z0_p,NY_AIR,DIFF_p, &
                 Vs,Rb)
!
!     This subroutine calculates the Quasi-laminar resistance Rb
!     for particles.
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
!           2017  M. Karl: Calculation of RB with method 1 modified
!
! ----------------------------------------------------------------------------------

      implicit none

!     GLOBAL VARIABLES:
!     *****************
!     Input:

      integer :: method
      real ::    kappa,g_acc,PR,ustar,z0,z0_p,NY_AIR,DIFF_p,Vs

!     method - 1 = Rb calculated based on EMEP Unified and Seinfeld & 
!                  Pandis: eq. (19.18).
!              2 = Rb calculated based on EMEP Unified and Binkowski
!              3 = Rb calculated based on Jacobson eq. (20.14).
!     kappa  - The von Karmans constant (dimless).
!     g_acc  - The acceleration due to gravity (ms^-2).
!     PR     - The Molecular Prandtl number (dimless).
!     ustar  - The friction velocity at the surface (m/s).
!     z0     - The roughness length for momentum (m).
!     z0_p   - The roughness length for particles (m).
!     NY_AIR - The kinematic viscosity of air (m^2/s).
!     DIFF_p - The diffusion coefficient for particles (m^2/s)
!     Vs     _ The gravitational settling (or fall) velocity (m/s).
!
!
!     Output:
!
      REAL Rb
!
!     Rb  - The Quasi-Laminar Resistance (s/m).

!     LOCAL VARIABLES:
!     ****************

      real :: Sc,St
!MSK start
      real :: EB,epsilon0,R1
!MSK end
      logical :: run_again

      run_again = .FALSE.

 100  CONTINUE
!     Sc  - The Schmidt Number.
!MSK start
      DIFF_p = max(DIFF_p,1.e-20)
      epsilon0 = 3.0
!MSK end
      Sc = NY_AIR / DIFF_p


      IF(method .eq. 1)THEN

!        Calculate Rb based on EMEP Unified and Seinfeld & 
!        Pandis: eq. (19.18) or Binkowski

!        St  - The Stokes Number. See Seinfeld and Pandis 
!              end of section 19.2.2 page 965.
          St = (Vs * ustar * ustar) / (g_acc * NY_AIR)
!MSK start
          St = max(St,1.e-20)

          EB = Sc**(-2.0/3.0)                          !Eq (19.21)
  
          R1 = exp( (-1.)* (St**0.5) )                 !Eq (19.26)

!MSK          Rb = Sc**(-2.0/3.0) + 10.0**(-3.0/St)
!MSK          Rb = 1.0 / (ustar * Rb)
          Rb = 1.0 / (epsilon0 * ustar * EB * R1)      !Eq (19.27)

!MSK end
      ELSEIF(method .eq. 2)THEN

!        Calculate Rb based on Jacobson eq. (20.14):
          Rb = (Sc/Pr)**(2.0/3.0)
          Rb = Rb * ALOG(z0/z0_p)
          Rb = Rb / (kappa * ustar)

      ELSE

!        Alternative expressions ?
!        If not, then apply method = 1.
           method = 1
           run_again = .TRUE.

      ENDIF

      IF(run_again) GO TO 100

      RETURN

!     End of SUBROUTINE C_RB
      end subroutine c_rb
