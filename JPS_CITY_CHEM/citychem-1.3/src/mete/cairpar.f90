! <cairpar.f90 - A component of the City-scale
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

      subroutine cairpar(T_ZREF,P_ZREF,NY_AIR,MY_AIR,RHO_AIR, &
                    PR,LAMDA_AIR)
!
!     This subroutine calculates commonly used air variables, based on
!     ambient temperature and pressure at the height of z_ref.
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
!
      implicit none

!     GLOBAL VARIABLES:
!     *****************
!     Input:

      REAL T_ZREF,P_ZREF

!     T_ZREF - Temperature (in Celsius) at the height zref
!              (Note that T_ZREF ideally should be the 
!               Virtual Temperature).
!     P_ZREF - Pressure in hPa (or mmbar)(typically 1013)
!
!
!     Output:
!
      REAL NY_AIR,MY_AIR,RHO_AIR,PR,LAMDA_AIR
!
!     NY_AIR    - Kinematic viscosity of air.  (m^2 s^-1)
!     MY_AIR    - Dynamic viscosity of air     (kg m^-1 s^-1)
!     RHO_AIR   - (Dry) air density            (kg m^-3)
!     PR        - Moelcular Prandtl number     (dimless)
!     LAMDA_AIR - Free molecular path of air   (m)
!
!     LOCAL VARIABLES:
!     ****************

      REAL KB,Rg,M,T,P,kappa_heat,cp

!     KB  - Boltzmanns constant (1.3807E-23 kg m^2 s^-2 K^-1 molecule^-1)
!     Rg  - Gas constant for dry air (287.04 J kg^-1 K^-1)
!     M   - Mass of a dry air molecule (4.8096E-26 kg)
!     T   - T_ZREF converted to Kelvin.
!     P   - P_ZREF converted to kPa.
!     kappa_heat - Conductivity of air (J m^-1 s^-1 K^-1).
!     cp    - Specific heat of dry air at constant pressure (J kg^-1 K^-1)

       KB = 1.3807E-23
       Rg = 287.04
       M  = 4.8096E-26
       T  = T_ZREF + 273.15
       P  = P_ZREF / 10.
       cp = 1004.67

!     Calculate (dry) air density: (If Tv is used instead of T,
!     then RHO_AIR will be the moist air density.)
       RHO_AIR = (P * 1000.0)/(Rg*T)
      IF(RHO_AIR .GT. 1.5 .OR. RHO_AIR .LT. 0.8)THEN
          PRINT *,' RHO_AIR = ',RHO_AIR
      ENDIF
!     Calculate the Thermal Conductivity of dry air (J m^-1 s^-1 K^-1)
!     according to eq. (2.3) in Jacobson.
      kappa_heat = 0.023807 + (7.1128E-5 * (T - 273.15))

!     Calculate kinematic viscosity of air (m^2 s^-1):
!     From AERMOD-description:
      NY_AIR = (P/101.3)*(1+0.0132*(P-101.3))
      NY_AIR = NY_AIR * 0.1505E-4 * (T/273.15)**1.772

!     Calculate dynamic viscosity of air (kg m^-1 s^-1):
      MY_AIR = RHO_AIR * NY_AIR 

!     Calculate the molecular Prandtl number Pr (unitless)
!     from eq. (17.32) in Jacobson:
      Pr = MY_AIR * cp / kappa_heat

!     Calculate the free molecular path of air (m):

      LAMDA_AIR = NY_AIR * SQRT((3.14*M)/(2.0*KB*T))

      RETURN

!     End of SUBROUTINE CAIRPAR
      end subroutine cairpar
