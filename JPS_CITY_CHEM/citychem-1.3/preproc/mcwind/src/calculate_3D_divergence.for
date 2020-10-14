! <calculate_3D_divergence.for - A component of the City-scale
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
!* Walker, S.-E., Solberg, S., Denby, B. (2003) Development and implementation of a simplified 
!*    EMEP photochemistry scheme for urban areas in EPISODE. NILU TR 13/2013. 
!*    ISBN 82-425-1524-7
!*
!* ==========================================
!*
!**********************************************************************

      subroutine calculate_3D_divergence(im,jm,km,dx,dy,delta_sigma, &
                                    depth,depthu,depthv,u,v,omega, &
                                    div_3d)

!**********************************************************************
!
! This routine calculates the 3-dimensional divergence in the internal
!     lambda-points.

! Modifications:
!    29 Jan 2019  M. Karl: replaced real*8 by double precision
!
!
!**********************************************************************
!
! We require the use of IMPLICIT NONE:
      implicit none

! Global declarations:

      integer :: im,jm,km
      real    :: dx,dy
      double precision  :: delta_sigma(0:km+1)
      double precision  :: depth(0:im+1,0:jm+1)
      double precision  :: depthu(0:im,jm)
      double precision  :: depthv(im,0:jm)
      double precision  :: u(0:im,jm,km)
      double precision  :: v(im,0:jm,km)
      double precision  :: omega(im,jm,0:km)
      double precision  :: div_3d(im,jm,km)

! Local declarations:

      integer :: i,j,k

      double precision  :: xflux_i,xflux_im1
      double precision  :: yflux_j,yflux_jm1

! Content of the routine:


! Calculate the 3D-divergence of the given wind field:

      do k = 1,km
        do j = 1,jm
          do i = 1,im

            xflux_i   = depthu(i,j)   * u(i,j,k)
            xflux_im1 = depthu(i-1,j) * u(i-1,j,k)

            yflux_j   = depthv(i,j)   * v(i,j,k)
            yflux_jm1 = depthv(i,j-1) * v(i,j-1,k)

            div_3d(i,j,k) = (xflux_i - xflux_im1)/(depth(i,j)*dx) &
                     + (yflux_j - yflux_jm1)/(depth(i,j)*dy) &
                     + ((omega(i,j,k) - omega(i,j,k-1)) &
                      / delta_sigma(k))

          end do
        end do
      end do

      return
      end subroutine calculate_3D_divergence
