! <wsprof.f90 - A component of the City-scale
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

      subroutine wsprof(Z0,ZU1,ZU2,U1,U2,UST,CL,NHV,MHV,HVEC,FFVEC)

! The subroutine calculates windspeeds at the given heights.
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
!           2016  M. Karl: Functions PSIM now in mod_mete.f90
!                          Modifications to use double precision variables
!
! ----------------------------------------------------------------------------------

       use mod_mete

          implicit none

      integer :: NHV, MHV
      real :: Z0
      real :: ZU1
      real :: ZU2
      real :: U1
      real :: U2
      real :: UST
      real :: CL
      real :: HVEC(MHV)
      real :: FFVEC(MHV)

! Input

! Z0   - Surface roughness
! ZU1  - Height of wind observation 1
! ZU2  - Height of wind observation 2
! U1   - Wind speed at height ZU1
! U2   - Wind speed at height ZU2
! UST  - Friction velocity
! CL   - Monin-Obukhov length
! NHV  - Number of height values
! MHV  - Dimension of *VEC arrays as declared in the calling program
! HVEC - Height values

! Output

! FFVEC - Wind speed values (profile)

! External functions

!MSK      REAL PSIM    !(is in mod_mete)

! Local variables
 
      real :: H, PH, P2
      integer :: I

! H  - Height value
! PH - Psi-expression at height H
! P2 - Psi-expression at height ZU2
! I  - Height index

! Go through all heights
 
      DO 100 I = 1,NHV

! Calculate windspeed

          H = HVEC(I)
!MSK start
          if (H .le. 1.e-28)  H=0.0
!MSK end
          IF (0.6*H .GE. Z0) THEN

! Above surface roughness layer

!MSK              PH = ALOG(  H/Z0) - PSIM(  H/CL) + PSIM(Z0/CL)
!MSK              P2 = ALOG(ZU2/Z0) - PSIM(ZU2/CL) + PSIM(Z0/CL)
              PH = ALOG(  H/Z0) - PSIM(DBLE(  H/CL)) + PSIM(DBLE(Z0/CL))
              P2 = ALOG(ZU2/Z0) - PSIM(DBLE(ZU2/CL)) + PSIM(DBLE(Z0/CL))

              FFVEC(I) = U2*PH/P2

          ELSE

! Close to surface roughness layer, use friction velocity

              FFVEC(I) = UST

          ENDIF

  100 continue
 
      RETURN

! End of subroutine WSPROF

      end subroutine wsprof
