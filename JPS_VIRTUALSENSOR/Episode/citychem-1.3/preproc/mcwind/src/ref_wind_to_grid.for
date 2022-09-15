! <ref_wind_to_grid.for - A component of the City-scale
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
!***********************************************************************

      subroutine ref_wind_to_grid(n_stat,im,jm,dx,dy,posx,posy,ff,dd, &
                             stat_pwr,stat_scale,u,v)

!***********************************************************************
!
! This routine interpolates the observed surface_reference winds to
! the u and v grid points:
!
! Main input:
!
!           n_stat               = number of stations
!
!           ff(1:n_stat)         = Wind speed at each station
!           dd(1:n_stat)         = Wind direction at each station
!           posx(1:n_stat)       = Eastward position in meters from
!                                  model origo to each station
!           posy(1:n_stat)       = Nortward position in meters from
!                                  model origo to each station
!           stat_pwr(1:n_stat)   = Inverse distance power value for
!                                  each station (default = 2).
!           stat_scale(1:n_stat) = Scaling parameter for each station
!                                  (default = 1.0). If zero for a
!                                  station this measurement is skipped.
!
! Main output:
!
!           u(0:im,jm) and v(im,0:jm) = Gridded wind field based on an
!                                       inverse distance weighting
!                                       interpolation formula.
!
! Modifications:
!     29 Jan 2019  M. Karl: replace TAB by spaces
!
!***********************************************************************
!
!      use module_mc_wind_files
!      use module_mc_wind_domain
! TEST      use module_mc_wind_met
!      use module_mc_wind_winds

!     We require the use of IMPLICIT NONE:
      implicit none

      integer, intent(IN) :: n_stat,im,jm
      real, intent(IN) :: dx,dy
      real, intent(IN) :: ff(1:n_stat),dd(1:n_stat)
      real, intent(IN) :: posx(1:n_stat),posy(1:n_stat)
      real, intent(IN) :: stat_pwr(1:n_stat),stat_scale(1:n_stat)

      real, intent(OUT) :: u(0:im,jm),v(im,0:jm)


!     Local declarations:
      integer :: i
      integer :: j
      integer :: n

!    real :: stat_pwr

      real  :: x_gp
      real  :: y_gp
      real  :: pos_x
      real  :: pos_y
      real  :: sum_w
      real  :: sum_w_val

      real, allocatable :: u_ref(:)
      real, allocatable :: v_ref(:)
      real, allocatable :: d_st(:)
      real, allocatable :: w_st(:)

!     Allocate the required internal memory:

      if (.not. allocated (u_ref)) allocate (u_ref(n_stat))
      if (.not. allocated (v_ref)) allocate (v_ref(n_stat))
      if (.not. allocated (d_st))  allocate (d_st(n_stat))
      if (.not. allocated (w_st))  allocate (w_st(n_stat))

!     Content of the routine:

!=======================================================================

      if (n_stat == 1) then

!       Convert from speed (ffref) and direction (wdir) to u_ref
!       and v_ref:
        call comp_vel(ff(1),dd(1),u_ref(1),v_ref(1))

!       First we go through all the u-grid points:
        do j = 1,jm
          do i = 0,im
            u(i,j) = u_ref(1)
          end do
        end do

!       Then the v-grid points:
        do j = 0,jm
          do i = 1,im
            v(i,j) = v_ref(1)
          end do
        end do


      else

!       Here we must start the horizontal interpolation procedure in
!       order to construct the 2D "surface layer" wind field:
!
!        u(0:im,jm)  and  v(im,0:jm)
!
!       These arrays are constructed from interpolation of the wind
!       speed, wind direction and x- and y-posistions, given by the
!       following arrays:
!
!        ff(1:n_surface_stat)
!        dd(1:n_surface_stat)
!        posx(1:n_surface_stat)
!        posy(1:n_surface_stat)


!       First we go through all the u-grid points:

        do j = 1,jm
          do i = 0,im

!           The coordinate values (meters from origo) for the u-point:
            x_gp = (dx * i)
            y_gp = (dy * j) - (0.5*dy)

!           Go through all the surface stations:
            sum_w     = 0.0
            sum_w_val = 0.0

            do n = 1,n_stat

                pos_x  = posx(n) * 1000.0
                pos_y  = posy(n) * 1000.0

!             Convert from speed (ff) and direction (dd)
!             to u_ref and v_ref:
              call comp_vel(ff(n),dd(n),u_ref(n),v_ref(n))

!             Compute the distance in meters from each station to the
!             grid point: If less than 50 m the observation is used
!             directly:
              d_st(n) = SQRT(  (pos_x - x_gp)**2  &
                        + (pos_y - y_gp)**2 )

              if (d_st(n) < (0.001*dx) ) d_st(n) = 0.001*dx

              w_st(n) = stat_scale(n)/(d_st(n)**stat_pwr(n))
              sum_w     = sum_w + w_st(n)
              sum_w_val = sum_w_val + (u_ref(n) * w_st(n))

              if (n == n_stat) then
                if (sum_w > 0.0)then
                  u(i,j) = sum_w_val / sum_w
                else
                  PRINT*, ' FATAL ERROR IN: ref_wind_to_grid '
                  PRINT*, ' Sum of weights is zero or negative '
                  PRINT*, ' PROGRAM TERMINATES '
                  STOP
                end if
              end if

            end do  ! do n = 1,n_stat

          end do  ! i = 0,im
        end do  ! j = 1,jm

!       Then the v-grid points:
        do j = 0,jm
          do i = 1,im

!           The coordinate values (meters from origo) for the v-point:
            x_gp = (dx * i) - (0.5*dx)
            y_gp = (dy * j)

!           Go through all the surface stations:
            sum_w     = 0.0
            sum_w_val = 0.0

            do n = 1,n_stat

                pos_x  = posx(n) * 1000.0
                pos_y  = posy(n) * 1000.0

!             Convert from speed (ff) and direction (dd)
!             to u_ref and v_ref:
              call comp_vel(ff(n),dd(n), &
                       u_ref(n),v_ref(n))

!             Compute the distance from each station to the grid point:
              d_st(n) = SQRT(  (pos_x - x_gp)**2  &
                        + (pos_y - y_gp)**2 )

!             In order to maintaining the "scaling" possibility:
              if (d_st(n) < (0.001*dy) ) d_st(n) = 0.001*dy

              w_st(n) = stat_scale(n)/(d_st(n)**stat_pwr(n))
              sum_w     = sum_w + w_st(n)
              sum_w_val = sum_w_val + (v_ref(n) * w_st(n))

              if (n == n_stat) then
                if (sum_w > 0.0)then
                  v(i,j) = sum_w_val / sum_w
                else
                  PRINT*, ' FATAL ERROR IN: ref_wind_to_grid '
                  PRINT*, ' Sum of weights is zero or negative '
                  PRINT*, ' PROGRAM TERMINATES '
                  STOP
                end if
              end if

            end do  ! do n = 1,n_stat

          end do  ! i = 1,im
        end do ! j = 0,jm

      end if ! (n_stat == 1)

! **********************************************************************

!     Deallocate the local arrays:

      if (allocated (u_ref)) deallocate (u_ref)
      if (allocated (v_ref)) deallocate (v_ref)
      if (allocated (d_st))  deallocate (d_st)
      if (allocated (w_st))  deallocate (w_st)

      return
      end subroutine ref_wind_to_grid
