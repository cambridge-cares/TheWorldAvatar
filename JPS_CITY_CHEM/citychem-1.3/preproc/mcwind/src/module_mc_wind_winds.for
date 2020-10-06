! <module_mc_wind_winds.for - A component of the City-scale
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
!DEC$ FIXEDFORMLINESIZE: 132
!***********************************************************************

      module module_mc_wind_winds


! This module contains the declaration of the initial (first guess)
! wind field and the final adjusted wind field.

! Modifications:
!    29 Jan 2019  M. Karl: replaced real*8 by double precision
!    29 Jan 2019  M. Karl: replaced TAB by spaces
!
!***********************************************************************

! Applied 3D-arrays:

! *** 3D-wind fields based on surface observations:
      real, allocatable :: u0_surface(:,:,:)
      real, allocatable :: v0_surface(:,:,:)
      real, allocatable :: w0_surface(:,:,:)
      real, allocatable :: omega0_surface(:,:,:)

! *** 3D-wind fields based on geostrophic upper air information:
      real, allocatable :: u0_geostrophic(:,:,:)
      real, allocatable :: v0_geostrophic(:,:,:)
      real, allocatable :: w0_geostrophic(:,:,:)
      real, allocatable :: omega0_geostrophic(:,:,:)

! *** First guess 3D-wind field based on combination of the above fields
      double precision, allocatable :: u0(:,:,:)
      double precision, allocatable :: v0(:,:,:)
      double precision, allocatable :: w0(:,:,:)
      double precision, allocatable :: omega0(:,:,:)

! *** Final adjusted 3D-wind fiels.
      double precision, allocatable :: u(:,:,:)
      double precision, allocatable :: v(:,:,:)
      double precision, allocatable :: w(:,:,:)
      double precision, allocatable :: omega(:,:,:)

      contains

! *** DOMAIN WINDS INTERFACE ROUTINES

      SUBROUTINE ReceiveWindResults(iv_im,iv_jm,iv_km,rav3_u,rav3_v)


      IMPLICIT NONE

      integer :: iv_im
      integer :: iv_jm
      integer :: iv_km

      real :: rav3_u(iv_im,iv_jm,iv_km)
      real :: rav3_v(iv_im,iv_jm,iv_km)
!      real :: rav3_w(iv_im,iv_jm,iv_km)
!      real :: rav3_omega(iv_im,iv_jm,iv_km)

! *** Local variables:

      integer :: ix
      integer :: iy
      integer :: iz
      logical :: in_centerpoint

! *** Assigning the variables declared above from the corresponding
! *** variables declared in: "module_mc_wind_winds"

      in_centerpoint = .TRUE.

      if (in_centerpoint) then

! ***    Export the wind components in the central lamda point locations:
         do iz = 1,iv_km
           do iy = 1,iv_jm
             do ix = 1,iv_im

              rav3_u(ix,iy,iz)     = 0.5*(u(ix-1,iy,iz) + u(ix,iy,iz))
              rav3_v(ix,iy,iz)     = 0.5*(v(ix,iy-1,iz) + v(ix,iy,iz))
!              rav3_w(ix,iy,iz)     = 0.5*(w(ix,iy,iz-1) + w(ix,iy,iz))
!             rav3_omega(ix,iy,iz) =
!     &                       0.5*(omega(ix,iy,iz-1) + omega(ix,iy,iz))

            end do
          end do
        end do

      else

! ***    Export the original wind components defined on the grid cell walls:
         do iz = 1,iv_km
           do iy = 1,iv_jm
             do ix = 1,iv_im

              rav3_u(ix,iy,iz)     = u(ix,iy,iz)
              rav3_v(ix,iy,iz)     = v(ix,iy,iz)
!              rav3_w(ix,iy,iz)     = w(ix,iy,iz)
!             rav3_omega(ix,iy,iz) = omega(ix,iy,iz)

            end do
          end do
        end do

      endif

      END SUBROUTINE ReceiveWindResults

      SUBROUTINE Receive3DWindResults(iv_im,iv_jm,iv_km, &
                               rav3_u,rav3_v,rav3_w)

!DEC$ ATTRIBUTES DLLEXPORT,STDCALL,ALIAS:'Receive3DWindResults' :: Receive3DWindResults

      IMPLICIT NONE

      integer :: iv_im
      integer :: iv_jm
      integer :: iv_km

      real :: rav3_u(iv_im,iv_jm,iv_km)
      real :: rav3_v(iv_im,iv_jm,iv_km)
      real :: rav3_w(iv_im,iv_jm,iv_km)
!      real :: rav3_omega(iv_im,iv_jm,iv_km)

! *** Local variables:

      integer :: ix
      integer :: iy
      integer :: iz
      logical :: in_centerpoint

! *** Assigning the variables declared above from the corresponding
! *** variables declared in: "module_mc_wind_winds"

      in_centerpoint = .TRUE.

      if (in_centerpoint) then

! ***    Export the wind components in the central lamda point locations:
         do iz = 1,iv_km
           do iy = 1,iv_jm
             do ix = 1,iv_im

              rav3_u(ix,iy,iz)     = 0.5*(u(ix-1,iy,iz) + u(ix,iy,iz))
              rav3_v(ix,iy,iz)     = 0.5*(v(ix,iy-1,iz) + v(ix,iy,iz))
              rav3_w(ix,iy,iz)     = 0.5*(w(ix,iy,iz-1) + w(ix,iy,iz))
!             rav3_omega(ix,iy,iz) =
!     &                       0.5*(omega(ix,iy,iz-1) + omega(ix,iy,iz))

            end do
          end do
        end do

      else

! ***    Export the original wind components defined on the grid cell walls:
         do iz = 1,iv_km
           do iy = 1,iv_jm
             do ix = 1,iv_im

              rav3_u(ix,iy,iz)     = u(ix,iy,iz)
              rav3_v(ix,iy,iz)     = v(ix,iy,iz)
              rav3_w(ix,iy,iz)     = w(ix,iy,iz)
!             rav3_omega(ix,iy,iz) = omega(ix,iy,iz)

            end do
          end do
        end do

      endif

      END SUBROUTINE Receive3DWindResults


      SUBROUTINE FreeWinds_Memory()


      implicit none

      if (allocated(u)) deallocate(u)
      if (allocated(v)) deallocate(v)
      if (allocated(w)) deallocate(w)
      if (allocated(omega)) deallocate(omega)

      END SUBROUTINE FreeWinds_Memory


! End of module module_mc_wind_winds
      end module module_mc_wind_winds
