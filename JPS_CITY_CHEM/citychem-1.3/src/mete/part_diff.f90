! <cpart_diff.f90 - A component of the City-scale
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

      subroutine part_diff(Method,D_p,T_ZREF,MY_AIR,C_c,Diff_p_scal, &
                      DIFF_p)
!
!     This subroutine calculates commonly used air variables, based on
!     ambient temperature and pressure at the height of z_ref.
!
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

!     GLOBAL VARIABLES:
!     *****************
!     Input:


      integer :: Method

!     If Method is equal to 1, the Method from Seinfeld and Pandis is used.
!     If Method is equal to 2, the Method from Binkowski is used.

      real :: D_p,T_ZREF,MY_AIR,C_c,Diff_p_scal

!     D_p    - Particle diameter (given in micrometers = 10^-6 m)
!     T_ZREF - Temperature (in Celsius) at the height zref
!     MY_AIR - Dynamicic viscosity of air (kg m^-1 s^-1).     
!     C_c    _ The Cunningham Slip Flow Correction factor (Dimless).
!     Diff_p_scal - Log-Normal scaling factor from Binkowski.
!
!     Output:
!
      real :: DIFF_p
!
!     DIFF_p  - Diffusion coefficent for particles of diameter D_p at 
!               the current met conditions. (m^2 s^-1)

!     LOCAL VARIABLES:
!     ****************

      real :: KB,T,D

!     KB  - Botzmanns constant (1.3807E-23 kg m^2 s^-2 K^-1 molecule^-1)
!     T   - T_ZREF converted to Kelvin.
!     D   - Particle diameter in meters.

        KB = 1.3807E-23
        T  = T_ZREF + 273.15 
        D  = D_p * 1.0E-6

!     Calculate the molecular diffusivity for particles of diameter D_p
!     at the current meteorological conditions:

        IF (Method .EQ. 1) THEN
           DIFF_p = (KB * T * C_c)/(3.0 * 3.14 * D * MY_AIR)
        ELSEIF (Method .EQ. 2) THEN
           DIFF_p = (KB * T * Diff_p_scal)/(3.0 * 3.14 * D * MY_AIR)
        ENDIF


      RETURN

! End of SUBROUTINE PART_DIFF

      end subroutine part_diff
