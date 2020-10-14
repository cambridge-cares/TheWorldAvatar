! <dxyf.f90 - A component of the City-scale
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

      subroutine DXYF(IC,K)

! The subroutine computes second partial derivatives of C with respect
! to x and y in vertical layer K for compound IC.
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
!           2016  M. Karl: Indices of C and BC has been shifted by +1 in x and
!                          y direction.
!           2017  M. Karl: Avoid very small concentration values
!
! ----------------------------------------------------------------------------------

      use mod_site
      use mod_mete
      use mod_conc
      use mod_grid

      implicit none

! Scalar arguments

      integer :: IC,K

! IC - Index of compound
! K  - Index of vertical layer

! Local variables

      integer :: I,J

!MSK start
! initial loop to avoid very small numbers
      do I = 1,NX
        do J = 1,NY
          if ( C(IC,I+1,J+1,K) .lt. 1.e-20 )    C(IC,I+1,J+1,K)   = 0.0
        enddo
      enddo

!MSK end


! I,J - Indices

      DO 100 J = 1,NY

! Compute second partial derivatives of C with respect to x along
! left boundary

!_NEST_Start:
!_org     D2CDX2(1,J) = (BC(IC) + C(IC,2,J,K) - 2.*C(IC,1,J,K))/DX2

!MSK start
          if ( BC(IC,1,J+1,K) .lt. 1.e-28 )     BC(IC,1,J+1,K) = 0.0
          if (  C(IC,3,J+1,K) .lt. 1.e-28 )      C(IC,3,J+1,K) = 0.0
          if (  C(IC,2,J+1,K) .lt. 1.e-28 )      C(IC,2,J+1,K) = 0.0
!MSK end

          D2CDX2(1,J) =     &
!MSK                 (BC(IC,0,J,K) + C(IC,2,J,K) - 2.*C(IC,1,J,K))/DX2
                 (BC(IC,1,J+1,K) + C(IC,3,J+1,K) - 2.*C(IC,2,J+1,K))/DX2
!_NEST_End.

! Compute second partial derivatives of C with respect to x in the
! interior

          DO 110 I = 2,NXM1
!MSK              D2CDX2(I,J) = (C(IC,I+1,J,K) + C(IC,I-1,J,K) -  &
!MSK                            2.*C(IC,I,J,K))/DX2
!MSK correct indices for BC and C
              D2CDX2(I,J) = (C(IC,I+2,J+1,K) + C(IC,I,J+1,K) -  &
                            2.*C(IC,I+1,J+1,K))/DX2
  110     CONTINUE

! Compute second partial derivatives of C with respect to x along right
! boundary

!_NEST_Start:
!_org     D2CDX2(NX,J) = (BC(IC) + C(IC,NX-1,J,K) - 2.*C(IC,NX,J,K))/DX2

!MSK start
          if ( BC(IC,NX+2,J+1,K) .lt. 1.e-28 )   BC(IC,NX+2,J+1,K) = 0.0
          if (  C(IC,NX,J+1,K)   .lt. 1.e-28 )    C(IC,NX,J+1,K) = 0.0
          if (  C(IC,NX+1,J+1,K) .lt. 1.e-28 )    C(IC,NX+1,J+1,K) = 0.0
!MSK end
          D2CDX2(NX,J) =   &
!MSK               (BC(IC,NX+1,J,K) + C(IC,NX-1,J,K) - 2.*C(IC,NX,J,K))/DX2
!MSK correct indices for BC and C
               (BC(IC,NX+2,J+1,K) + C(IC,NX,J+1,K) - 2.*C(IC,NX+1,J+1,K))/DX2
!_NEST_End.
  100 CONTINUE

! Compute second partial derivatives of C with respect to y along
! bottom boundary

      DO 200 I = 1,NX
!_NEST_Start:
!_org     D2CDY2(I,1) = (BC(IC) + C(IC,I,2,K) - 2.*C(IC,I,1,K))/DY2

!MSK start
          if ( BC(IC,I+1,1,K) .lt. 1.e-28 )     BC(IC,I+1,1,K) = 0.0
          if (  C(IC,I+1,3,K) .lt. 1.e-28 )      C(IC,I+1,3,K) = 0.0
          if (  C(IC,I+1,2,K) .lt. 1.e-28 )      C(IC,I+1,2,K) = 0.0
!MSK end

          D2CDY2(I,1) =   &
!MSK                 (BC(IC,I,0,K) + C(IC,I,2,K) - 2.*C(IC,I,1,K))/DY2
!MSK correct indices for BC and C
                 (BC(IC,I+1,1,K) + C(IC,I+1,3,K) - 2.*C(IC,I+1,2,K))/DY2
!_NEST_End.
  200 CONTINUE

! Compute second partial derivatives of C with respect to y in the
! interior

      DO 210 J = 2,NYM1
      DO 220 I = 1,NX
!MSK          D2CDY2(I,J) = (C(IC,I,J+1,K) + C(IC,I,J-1,K) -  &
!MSK                        2.*C(IC,I,J,K))/DY2
!MSK correct indices for BC and C
          D2CDY2(I,J) = (C(IC,I+1,J+2,K) + C(IC,I+1,J,K) -  &
                        2.*C(IC,I+1,J+1,K))/DY2
  220 CONTINUE
  210 CONTINUE

! Compute second partial derivatives of C with respect to y along
! top boundary

      DO 230 I = 1,NX
!_NEST_Start:
!_org     D2CDY2(I,NY) = (BC(IC) + C(IC,I,NY-1,K) - 2.*C(IC,I,NY,K))/DY2

!MSK start
          if ( BC(IC,I+1,NY+2,K) .lt. 1.e-28 )   BC(IC,I+1,NY+2,K) = 0.0
          if (  C(IC,I+1,NY,K)   .lt. 1.e-28 )    C(IC,I+1,NY,K) = 0.0
          if (  C(IC,I+1,NY+1,K) .lt. 1.e-28 )    C(IC,I+1,NY+1,K) = 0.0
!MSK end

          D2CDY2(I,NY) =     &
!MSK               (BC(IC,I,NY+1,K) + C(IC,I,NY-1,K) - 2.*C(IC,I,NY,K))/DY2
!MSK correct indices for BC and C
               (BC(IC,I+1,NY+2,K) + C(IC,I+1,NY,K) - 2.*C(IC,I+1,NY+1,K))/DY2
!_NEST_End.
  230 CONTINUE


      RETURN

! End of subroutine DXYF

      end subroutine dxyf
