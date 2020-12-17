! <construct_initial_wind.for - A component of the City-scale
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

      subroutine construct_initial_wind

!_LHS Change 5: October_2010_Start:
!_LHS   !DEC$ ATTRIBUTES DLLEXPORT,STDCALL,ALIAS:'construct_initial_wind' :: construct_initial_wind
!_LHS Change 5: October_2010_End.
!
!     This routine constructs the initial (first guess) wind field.
!     This is typically based on measurements of the horizontal wind at
!     surface and upper air stations. These values are then interpolated
!     (and extrapolated) to the model grid structure.
!
!     Output:
!               u0(0:im,jm,km)     : Initial wind field u-component.
!               v0(im,0:jm,km)     : Initial wind field v-component.
!               w0(im,jm,0:km)     : Initial Cartesian vertical comp.
!               omega0(im,jm,0:km) : Initial terrain vertical comp.
!
!     Usually "w0" is identically zero, while "omega0" is different
!     from zero, due to the effect of the variable topography.

! Modifications:
!    29 Jan 2019  M. Karl: Commented pre-processor directive 'main_program'
!    29 Jan 2019  M. Karl: replaced TAB by spaces
!
!***********************************************************************

      use module_mc_wind_files
      use module_mc_wind_domain
      use module_mc_wind_met
      use module_mc_wind_winds

!     We require the use of IMPLICIT NONE:
      implicit none

!     Local declarations:
      integer :: i,j,k
      integer :: i_gstrop,j_gstrop

      real    :: speed_gstrop,wdir_gstrop
!      real    :: omega0_below,omega0_above,
      real    :: w_below,w_above
      real    :: weight_surface,weight_geostrophic
      real    :: u0_i,u0_im1
      real    :: v0_j,v0_jm1
      real    :: max_u0,max_v0
      real    :: min_u0,min_v0
      real    :: lhs_test_par

!     Content of the routine:
!************************************************************************************
!      if (fe_log) then
!        write(funit_log,*)
!        write(funit_log,'(A11)') domain_name(1:11)
!        write(funit_log,'(A19,F12.5)') 'wspeed station 1 = ', surface_stat_wspeed(1)
!        write(funit_log,'(A19,F12.5)') 'wdir station 1   = ', surface_stat_wdir(1)
!        write(funit_log,'(A19,F12.5)') 'tmp station 1    = ', surface_stat_tmp(1)
!        write(funit_log,'(A19,F12.5)') 'tm_kel station 1    = ',tm_kel
!        write(funit_log,'(A19,F12.5)') 'DT station 1     = ', surface_stat_dt(1)
!        write(funit_log,'(A19,F12.5)') 'RH station 1     = ', surface_stat_rh(1)
!        write(funit_log,*)
!
!      end if
!************************************************************************************

      if(fe_in_surface_obs)then

!
!       Estimate the observed horizontal wind speed at the "surface
!       reference height" above the ground using surface layer Monin-
!       Obukhov similarity theory. The "surface reference height" may
!       either be the variable height above ground of the midpoint of
!       the first sigma-layer, or a user specified constant height
!       above ground (ex. 10 m).

        call get_surf_ref_obs
        if(SKIP_THIS_HOUR) RETURN

!       The output from the above routine are the two vectors:
!
!               "surface_stat_ffref(1:n_surface_stat)"
!       and     " surface_stat_wdir(1:n_surface_stat)"
!
!       ================================================================
!
!       The values of the two vectors:
!
!            "surface_stat_ffref(1:n_surface_stat)"
!       and  " surface_stat_wdir(1:n_surface_stat)"
!
!       are then interpolated "horizontally" to the model grid points,
!       and are stored in the two 2D-arrays:
!
!            u0_surface_ref(0:im,jm)  and  v0_surface_ref(im,0:jm)
!
!       It is preferable to do this in component form since u and v
!       points are not defined in the same geographical positions.
!
!       The "horizontal" interpolation, applying a square-distance
!       interpolation formula, is done in the following subroutine:
!
        call ref_wind_to_grid(n_surface_stat,im,jm,dx,dy, &
                         surface_stat_posx,surface_stat_posy, &
                         surface_stat_ffref,surface_stat_wdir, &
                         surface_stat_pwr,app_surface_stat_scale, &
                         u0_surface_ref,v0_surface_ref)

!      NOTE: There is a lower limit on the observed windspeed, but not
!            on the profile-calculated reference height wind speed.
!
!     ==================================================================
        if (fe_log) then
          max_u0     = MAXVAL(u0_surface_ref)
          max_v0     = MAXVAL(v0_surface_ref)

          write(funit_log,*)
          write(funit_log,*) ' ************************************ '
          write(funit_log,*)
          write(funit_log,*) ' After the call for ref_wind_to_grid: '
          write(funit_log,*)
          write(funit_log,'(36X,2A4)') '  I ','  J '
          write(funit_log,'(1X,A24,F10.5,2I4)') &
          'Max u0_surface_ref: ',max_u0,MAXLOC(u0_surface_ref)
          write(funit_log,'(1X,A24,F10.5,2I4)') &
          'Max v0_surface_ref: ',max_v0,MAXLOC(v0_surface_ref)

          min_u0     = MINVAL(u0_surface_ref)
          min_v0     = MINVAL(v0_surface_ref)

          write(funit_log,*)
          write(funit_log,'(36X,2A4)') '  I ','  J '
          write(funit_log,'(1X,A24,F10.5,2I4)') &
          'Min u0_surface_ref: ',min_u0,MINLOC(u0_surface_ref)
          write(funit_log,'(1X,A24,F10.5,2I4)') &
          'Min v0_surface_ref: ',min_v0,MINLOC(v0_surface_ref)
          write(funit_log,*)
          write(funit_log,*)  &
         ' Now calling profile_initial_surface_wind: '
          write(funit_log,*)
        end if


!       ================================================================
!
!       By applying a profile method (at present either a power law
!       formula, or the Zilitinkevich method applied in WINDS) in
!       each u and v grid point, a surface-based 3D first guess field
!
!          u0_surface(0:im,jm,km)  and  v0_surface(im,0:jm,km)
!
!       can be computed. This is done in the routine below.
!       Note: Here gridded values of the surface roughness are used.
!
!       NOTE: The interpolated field: u0_surface_ref(0:im,jm)  and
!       v0_surface_ref(im,0:jm) is applied directly as the first layer
!       wind field in the surfaced based 3D first guess wind field:
!
!             u0_surface(0:im,jm,k=1) =  u0_surface_ref(0:im,jm)
!       and
!             v0_surface(im,0:jm,k=1) =  v0_surface_ref(im,0:jm)
!
!
        call profile_initial_surface_wind
!
!       ================================================================

      end if

! **********************************************************************
! ***
! *** If vertical profiles observations exist:
! ***

       if (fe_in_profile_obs) then

!       1) Import Meteorological Profile Observations
!       2) Interpolate linearly? into the vertical model grid heights.
!       3) Convert to U and V components and interpolate to the U(i,j,k)
!          and V(i,j,k) grid points, by use of the same inverse distance
!          formula as applied for the surface observations, leading to a
!          profile-based 3D wind field:
!
!             u0_profile(0:im,jm,km)  and   v0_profile(im,0:jm,km)

      end if


! **********************************************************************
! ***
! *** If upper air geostrophic wind information exist:
! ***
!
!      fe_in_geostrophic_obs = .TRUE.
!      fe_in_geostrophic_obs = .FALSE.

!	if (fe_in_geostrophic_obs) then
        if (n_geostrophic_stat > 0) then


!       The values of the two vectors:
!
!            "geostrophic_stat_ffref(1:n_geostrophic_stat)"
!       and  "geostrophic_stat_ddref(1:n_geostrophic_stat)"
!
!       are the speed and direction of the assumed  barotropic" upper
!       air geostrohic wind, at the "n_geostrophic_stat" stations.
!       The geographic position of these stations (relative to the model
!       origo) is given by the 1D-vectors:
!
!             "geostrophic_stat_posx(1:n_geostrophic_stat)"
!       and   "geostrophic_stat_posy(1:n_geostrophic_stat)"
!
!       The geostrophic wind speed and direction are then interpolated
!       "horizontally" to the model grid points,and are stored in the
!       two 2D-arrays:
!
!          u0_geostrophic_ref(0:im,jm)  and  v0_geostrophic_ref(im,0:jm)
!
!       It is preferable to do this in component form since u and v
!       points are not defined in the same geographical positions.
!
!       The "horizontal" interpolation, applying a square-distance
!       interpolation formula, is done in the following subroutine:
!
!        call ref_wind_to_grid(n_geostrophic_stat,im,jm,dx,dy,
!     &                        geostrophic_stat_posx,
!     &                        geostrophic_stat_posy,
!     &                        geostrophic_stat_ffref,
!     &                        geostrophic_stat_ddref,
!     &                        geostrophic_stat_pwr,geostrophic_scale,
!     &                        u0_geostrophic_ref,v0_geostrophic_ref)
!
!       NOTE: There is now no lower limit imposed on the wind speed.
!
!       ================================================================
!
!       By applying the Zilitinkevich profile method (WINDS-4.2) in each
!       u and v grid point, a geostrophic-based 3D first guess field
!
!          u0_geostrophic(0:im,jm,km)  and  v0_geostrophic(im,0:jm,km)
!
!       can be computed. This is done in the routine below.
!       Note: Here a user defined domain average value of the surface
!             roughness values is used.
!
!        call profile_initial_geostrophic_wind
!
!       ================================================================
!
!       1) Simplistic alternative:
!          We apply the coloumn of the calculated arrays: u0_surface
!          and v0_surface that are closest to the surface station that
!          is the least influenced by the topography.
!          In the Oslo case with 7 surface stations this is Fornebu.
!          In the one station case we have to use the main met station,
!          which in the Oslo case is Hovin.
!
!          FORNEBU (4,8): I_gstrop = 4
!                         J_gstrop = 8
!
!          HOVIN (14,12): I_gstrop = 14
!                         J_gstrop = 12
!
!
        if(n_geostrophic_stat == 1)then
          i_gstrop = INT(geostrophic_stat_posx(1)*1000.0/dx) + 1
          j_gstrop = INT(geostrophic_stat_posy(1)*1000.0/dy) + 1

!MSK!DEC$ IF DEFINED (main_program)
        else
          if (fe_log) then
            write(funit_log,*)
            write(funit_log,*) ' Only one geostrophic station can '
            write(funit_log,*) ' be applied at the moment. '
            write(funit_log,*) ' PROGRAM TERMINATES '
            write(funit_log,*)
          end if
          STOP
!MSK!DEC$ ENDIF
        end if

!       The u-component:

        do j = 1,jm
          do i = 0,im
            do k = 1,km
              u0_geostrophic(i,j,k) =  &
             0.5 * (  u0_surface(i_gstrop - 1,j_gstrop,k) &
                    + u0_surface(i_gstrop    ,j_gstrop,k))
            end do
          end do
        end do

!       The v-component:
        do j = 0,jm
          do i = 1,im
            do k = 1,km
              v0_geostrophic(i,j,k) =  &
            0.5 * (  v0_surface(i_gstrop,j_gstrop - 1,k) &
                   + v0_surface(i_gstrop,j_gstrop    ,k))
            end do
          end do
        end do

      if (fe_log) then
        write(funit_log,*)
        write(funit_log,*)  &
   ' Geostrophic wind taken from surface profile in grid square:'
        write(funit_log,'(1X,A11,I4,A16,I4)') &
        'i_gstrop = ',i_gstrop,' and j_gstrop = ',j_gstrop
        write(funit_log,*)
        write(funit_log,'(1X,A30,F10.5)') &
        'u0_geostrophic(i,j,1) = ',u0_geostrophic(1,1,1)
        write(funit_log,'(1X,A30,F10.5)') &
        'v0_geostrophic(i,j,1) = ',v0_geostrophic(1,1,1)

        speed_gstrop = (u0_geostrophic(1,1,1)*u0_geostrophic(1,1,1)) &
                 +(v0_geostrophic(1,1,1)*v0_geostrophic(1,1,1))
        speed_gstrop = SQRT(speed_gstrop)
        wdir_gstrop=cwdir(u0_geostrophic(1,1,1),v0_geostrophic(1,1,1))

        write(funit_log,'(1X,A30,F10.5)') &
        'speed_gstrop(i,j,1)   = ',speed_gstrop
        write(funit_log,'(1X,A30,F10.5)') &
        'wdir_gstrop(i,j,1)    = ',wdir_gstrop
        write(funit_log,*)

        write(funit_log,'(1X,A30,F10.5)') &
        'u0_geostrophic(i,j,km-1) = ',u0_geostrophic(1,1,km-1)
        write(funit_log,'(1X,A30,F10.5)') &
        'v0_geostrophic(i,j,km-1) = ',v0_geostrophic(1,1,km-1)
        speed_gstrop = (u0_geostrophic(1,1,km-1) &
                 *u0_geostrophic(1,1,km-1)) &
                 +(v0_geostrophic(1,1,km-1) &
                 *v0_geostrophic(1,1,km-1))
        speed_gstrop = SQRT(speed_gstrop)
        wdir_gstrop=cwdir(u0_geostrophic(1,1,km-1), &
                        v0_geostrophic(1,1,km-1))
        write(funit_log,'(1X,A30,F10.5)') &
        'speed_gstrop(i,j,km-1)    = ',speed_gstrop
        write(funit_log,'(1X,A30,F10.5)') &
        'wdir_gstrop(i,j,km-1)     = ',wdir_gstrop
        write(funit_log,*)

        write(funit_log,'(1X,A30,F10.5)') &
        'u0_geostrophic(i,j,km) = ',u0_geostrophic(1,1,km)
        write(funit_log,'(1X,A30,F10.5)') &
        'v0_geostrophic(i,j,km) = ',v0_geostrophic(1,1,km)
        speed_gstrop = (u0_geostrophic(1,1,km)*u0_geostrophic(1,1,km)) &
                 +(v0_geostrophic(1,1,km)*v0_geostrophic(1,1,km))
        speed_gstrop = SQRT(speed_gstrop)
        wdir_gstrop=cwdir(u0_geostrophic(1,1,km),v0_geostrophic(1,1,km))
        write(funit_log,'(1X,A30,F10.5)') &
        'speed_gstrop(i,j,km)    = ',speed_gstrop
        write(funit_log,'(1X,A30,F10.5)') &
        'wdir_gstrop(i,j,km)     = ',wdir_gstrop
        write(funit_log,*)

      end if

      end if

! **********************************************************************
!
!      if (some_model_generated_results_are_available) then
!
!         1) Import the available model generated wind field.
!         2) Interpolate linearly? into the vertical model grid heights.
!         3) Convert to U and V components and interpolate to the
!            U(i,j,k) and V(i,j,k) grid points, by use of the same
!            inverse distance formula as applied for the surface
!            observations.
!
!         We then end up with a model-based 3D wind field:
!
!           u0_model(0:im,jm,km) and  v0_model(im,0:jm,km)
!
!      end if
!
! **********************************************************************


      if ( .NOT. fe_in_profile_obs .AND.  &
      n_geostrophic_stat == 0   ) then


!       The surface based 3D-field is applied directly as the
!       FINAL FIRST GUESS WIND FIELD, i.e.,
!
!       The u-component:
        do j = 1,jm
          do i = 0,im
            do k = 1,km
              u0(i,j,k) = u0_surface(i,j,k)
            end do
          end do
        end do

!       The v-component:
        do j = 0,jm
          do i = 1,im
            do k = 1,km
              v0(i,j,k) = v0_surface(i,j,k)
            end do
          end do
        end do

!       In this case the vertical wind component is assumed zero.
           w0     = 0.0

        elseif ( n_surface_stat > 0 .AND. n_geostrophic_stat > 0   ) then
!
        if (fe_log) then
          write(funit_log,*)
          write(funit_log,*)  &
          ' The final first guess wind is interpolated betw surf.'
           write(funit_log,*)  &
          ' observation profiles and geostrophic upper airvalue.'
          write(funit_log,*)
        end if
!     <A procedure has to be defined inn which the FINAL FIRST GUESS
!      WIND FIELD is calculated from a weighted average of the
!      available gridded 3D fields of u0 and v0. i.e., >
!
        do k = 1,km
          weight_geostrophic =  &
       (FLOAT(k-1)/FLOAT(km-1))**geostrophic_vertical_weight_pwr
          weight_surface     = 1.0 - weight_geostrophic

          if (fe_log) then
            write(funit_log,'(1X,A5,I3,A20,F6.4)') ' k = ',k, &
         ' weight_surface = ',weight_surface
          end if

!         The u-component:
          do j = 1,jm
            do i = 0,im
              u0(i,j,k) =  weight_surface * u0_surface(i,j,k) &
                    + weight_geostrophic * u0_geostrophic(i,j,k)
            end do
          end do

!         The v-component:
          do j = 0,jm
            do i = 1,im
              v0(i,j,k) =  weight_surface * v0_surface(i,j,k) &
                    + weight_geostrophic * v0_geostrophic(i,j,k)
            end do
          end do

        end do

!       In this case the vertical wind component is assumed zero.
         w0     = 0.0

      end if

!     ==================================================================


      if ( .NOT. fe_in_surface_obs .AND. .NOT. fe_in_profile_obs .AND. &
      .NOT. fe_in_geostrophic_obs) then

!     Simple TEST setup where the initial wind field is set equal to a
!     given constant values.

        u0     = u0_test
        v0     = v0_test
        w0     = w0_test
        omega0 = 0.0

        call get_alfa

        if (fe_log) then

          write(funit_log,*)
          write(funit_log,'(1X,A)')  &
           'Get_initial_wind: The Constant Wind field is applied.'

        end if  ! if (fe_log)

      end if


! **********************************************************************
!     Compute the omega0(i,j,k) values from the given values of: u0, v0,
!     and w0.

      do k = 1,km-1

        w_below = (sigmal(k+1) - sigma(k))/(sigmal(k+1) - sigmal(k))
        w_above = (sigma(k) - sigmal(k))/(sigmal(k+1) - sigmal(k))

        do j = 1,jm
          do i = 1,im

            u0_i   =  w_below*u0(i,j,k)   + w_above*u0(i,j,k+1)
            u0_im1 =  w_below*u0(i-1,j,k) + w_above*u0(i-1,j,k+1)
            v0_j   =  w_below*v0(i,j,k)   + w_above*v0(i,j,k+1)
            v0_jm1 =  w_below*v0(i,j-1,k) + w_above*v0(i,j-1,k+1)


            lhs_test_par =  h0*w0(i,j,k)/depth(i,j) &
                      + h0msig(k)*(dRD_dx(i,j)*0.5*(u0_i + u0_im1) &
                                 + dRD_dy(i,j)*0.5*(v0_j + v0_jm1))

            omega0(i,j,k) =  lhs_test_par

          end do
        end do
      end do

!=======================================================================


      return
      end subroutine construct_initial_wind
