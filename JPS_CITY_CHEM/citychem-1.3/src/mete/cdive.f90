! <cdive.f90 - A component of the City-scale
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

      subroutine cdive(K)

! The subroutine calculates the horisontal wind divergence in vertical
! layer K and multiplies it with the size (height) of layer K.
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
!           2016  M. Karl: DIVE = 0 if not calculated
!
! ----------------------------------------------------------------------------------

      use mod_site
      use mod_mete

      implicit none

      INTEGER K

! K - Index of vertical layer

! Local variables

      REAL DELTA
      INTEGER I
      INTEGER J

! DELTA - Multiplicative factor
! I     - Main grid index in x-direction
! J     - Main grid index in y-direction

! Calculate wind divergence in x-dir

!_SIGMA_LHS: ??? Is this way of calculating the divergence in agreement with
!_SIGMA_LHS      the way horizontal advective transport is calulated ??????

      DELTA = .5/DX

!MSK start
      IF ( (NX.gt.1).and.(NY.gt.1) ) then
!MSK end

!_SIGMA_Start:

        DO 100 J = 1,NY

!_SIGMA_LHS:  In the line below we are assuming that U(0,J,K) = U(1,J,K) ?
!
!_SIGMA_Old: DIVE(1,J,K) = (U(2,J,K) - U(1,J,K))*DELTA
               DIVE(1,J,K) = ( (U(2,J,K)*DEPTHM(2,J)) -  &
                        (U(1,J,K)*DEPTHM(1,J)) ) * DELTA
           DO 110 I = 2,NXM1
!_SIGMA_Old:   DIVE(I,J,K) = (U(I+1,J,K) - U(I-1,J,K))*DELTA
                 DIVE(I,J,K) = ( (U(I+1,J,K)*DEPTHM(I+1,J)) -  &
                          (U(I-1,J,K)*DEPTHM(I-1,J)) ) * DELTA
  110      CONTINUE

!_SIGMA_Old:        DIVE(NX,J,K) = (U(NX,J,K) - U(NX-1,J,K))*DELTA
           DIVE(NX,J,K) = ( (U(NX,J,K)*DEPTHM(NX,J))     -  &
                     (U(NX-1,J,K)*DEPTHM(NX-1,J)) ) * DELTA
  100   CONTINUE
!_SIGMA_End.

! Calculate wind divergence in y-dir

        DELTA = .5/DY

!_SIGMA_Start:
        DO 200 I = 1,NX
!_SIGMA_Old:    DIVE(I,1,K) = DIVE(I,1,K) + (V(I,2,K) - V(I,1,K))*DELTA
                DIVE(I,1,K) = DIVE(I,1,K) +  &
                       ( (V(I,2,K)*DEPTHM(I,2)) -  &
                         (V(I,1,K)*DEPTHM(I,1)) ) * DELTA
  200   CONTINUE

        DO 210 J = 2,NYM1
           DO 220 I = 1,NX
!_SIGMA_Old:   DIVE(I,J,K) = DIVE(I,J,K) + 
!_SIGMA_Old:     .           (V(I,J+1,K) - V(I,J-1,K))*DELTA
               DIVE(I,J,K) = DIVE(I,J,K) +  &
                    ( (V(I,J+1,K)*DEPTHM(I,J+1)) -  &
                      (V(I,J-1,K)*DEPTHM(I,J-1)) ) * DELTA
  220      CONTINUE
  210   CONTINUE

        DO 230 I = 1,NX
!_SIGMA_Old:  DIVE(I,NY,K) = DIVE(I,NY,K) + (V(I,NY,K) - V(I,NY-1,K))*DELTA
           DIVE(I,NY,K) = DIVE(I,NY,K) +  &
                   ( (V(I,NY,K)*DEPTHM(I,NY))     -  &
                     (V(I,NY-1,K)*DEPTHM(I,NY-1)) ) * DELTA
  230   CONTINUE
!_SIGMA_End.

!MSK start
      else
        do I = 1,NX
          do J = 1,NY
           DIVE(I,J,K) = 0.0 
          enddo
        enddo
      endif
!MSK end

      RETURN

! End of subroutine CDIVE

      end subroutine cdive
