! <get_observations.for - A component of the City-scale
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

      subroutine get_observations

!_LHS Change 4: October_2010_Start:
!_LHS  !DEC$ ATTRIBUTES DLLEXPORT,STDCALL,ALIAS:'get_observations' :: get_observations
!_LHS Change 4: October_2010_End.
!
!     This routine collects the available observational data.
!     This is typically measurements of the horizontal wind at
!     surface and upper air stations.

! Modifications:
!    28 Jan 2019  M. Karl: Commented pre-processor directive 'main_program'
!    29 Jan 2019  M. Karl: hh=hh+1 (because hours go from 0-23)
!    29 Jan 2019  M. Karl: replaced TAB by spaces
!
!***********************************************************************

      use module_mc_wind_files
      use module_mc_wind_domain
      use module_mc_wind_met
      use module_mc_wind_adjust
      use module_mc_wind_winds

!     We require the use of IMPLICIT NONE:

      implicit none

!     Local declarations:

      integer :: n

!!DEC$ IF DEFINED (main_program)


      integer :: yyyy,mm,dd,hh,hhm1,nmax,icount
      real    :: dir_sc
      real, allocatable :: obs_vector(:)

!     Content of the routine:

!     ******************************************************************
      if (fe_log) then
        write(funit_log,*)
        write(funit_log,'(1X,A36,I4)')  &
              ' Get observational data for (hour): ', &
               n_counter
        write(funit_log,*)  &
              '************************************************'
        write(funit_log,*)
        write(funit_log,*) ' In Subroutine get_observations: '
        write(funit_log,*) '     fe_in_surface_obs = ', &
     fe_in_surface_obs
        write(funit_log,*) '     fe_in_profile_obs = ', &
     fe_in_profile_obs
        write(funit_log,*) ' fe_in_geostrophic_obs = ', &
     fe_in_geostrophic_obs
        write(funit_log,*)
      end if
!     ==================================================================

       if (fe_in_surface_obs) then

!       1) Import Meteorological Surface Station Observations
!       2) Apply Surface similarity profiles (M-O theory) to calculate
!          the wind speed and wind direction at a user-defined reference
!          height (either a constant height above ground, or the height
!          of the mid-point of the first model layer).
!       3) Interpolate the surface reference height wind speed
!       ================================================================
!
!       Read meteorolgical surface observations from the met-obs file:

          dir_sc = 10.0
!         Hovin       :  FF(10m), DD(10m), T(10m), DT(10m-2m)
!         Alle øvrige :  FF(10m), DD(10m)
!          nmax = 4 + 2*(n_surface_stat - 1)
!MSK
!MSK          nmax = 7 + 2*(n_surface_stat -1)
!MSK include pressure
          nmax = 8 + 2*(n_surface_stat -1)

        allocate(obs_vector(nmax))

        n_valid_obs = 0
        call read_obs(n_counter,funit_in_surface_obs,nmax,yyyy,mm,dd,hh, &
                 obs_vector)
!       print*,'obsvector',obs_vector

!MSK 29.01.2019 start
        hh = hh + 1
!MSK end
        hhm1 = hh - 1

        g_imon   = mm
        g_iday   = dd
        g_ihour  = hhm1

        do n = 1,n_surface_stat


! ***     OSLO (1992):
          if( n == 1)then

! ***       Main met-station.
! ***       Hovin: FF(10m), DD(10m), T(10m), DT(10m-2m)
            surface_stat_wspeed(n) = obs_vector(1)
!            surface_stat_wdir(n)   = obs_vector(2)
! MSK reads one zero at end, thus divide by 10
            surface_stat_wdir(n)   = obs_vector(2) * 0.1

! ***       NOTE:
! ***       Since temperature and delta-T is only read from the Main
! ***       met-station, these values are just not updated if the last
! ***       read value is UNDEF. This means that we automaticcally apply
! ***       the previous temp and delta-t value that were valid values!
! ***       THE FIRST HOUR temperature and delta_T values at the main
! ***       station therefore need to be valid values.

            if(obs_vector(3) > -9000.0)  &
               surface_stat_tmp(n)    = obs_vector(3)
            if(obs_vector(4) > -9000.0) &
               surface_stat_dt(n)     = obs_vector(4)

            if(surface_stat_tmp(n) > 150.0)then
               tm_kel = surface_stat_tmp(n)
              if (fe_log) then
                write(funit_log,*)  &
             'The surface station temperature is given in Kelvin?'
              endif
              else
              tm_kel = surface_stat_tmp(n) + 273.15
              if (fe_log) then
                write(funit_log,*)
                write(funit_log,*)  &
             'The surface station temperature is given in Celsius?'
              endif
            endif

! ***       Additional met variables that should be included.
! ***       Not used at present:
!            surface_stat_press(n) = 1013.0  ! Pressure in hPa.
!            surface_stat_rh(n)    =   79.0  ! Relative humidity (0-100)
!            surface_stat_mm(n)    =    0.0  ! Precipitation
!
!            icount = 5

! ***       Additional met variables that should be included.
! ***       Not used at present:

            surface_stat_rh(n)    = obs_vector(5)
            surface_stat_mm(n)    = obs_vector(6)  ! Precipitation (mm)
! MSK: cloud cover
            surface_stat_clc(n)   = obs_vector(7)  ! cloud cover 0...1
            surface_stat_press(n) = obs_vector(8)  ! Pressure in hPa. (=mb)

!            icount = 7 !cloud
!            icount = 8 !press
            icount = 9

          elseif( n > 1) then

! ***       Station 2 - n_surface_stat.
! ***       Rest of Oslo stations: FF(10m), DD(10m)
            surface_stat_wspeed(n) = obs_vector(icount)
            icount = icount + 1
!            surface_stat_wdir(n)   = obs_vector(icount)
! MSK reads one zero at end, thus divide by 10
            surface_stat_wdir(n)   = obs_vector(icount) * 0.1
            icount = icount + 1

            surface_stat_tmp(n)   = surface_stat_tmp(1)
            surface_stat_dt(n)    = surface_stat_dt(1)
            surface_stat_press(n) = surface_stat_press(1)
            surface_stat_rh(n)    = surface_stat_rh(1)
            surface_stat_mm(n)    = surface_stat_mm(1)
            surface_stat_clc(n)   = surface_stat_clc(1)
          endif

            call allowed_obs_wind_data(dir_sc,surface_stat_wspeed(n), &
                                surface_stat_wdir(n), &
                                minimum_obs_windspeed, &
                                maximum_obs_windspeed, &
                                surface_stat_scale(n), &
                                app_surface_stat_scale(n))

          if(method_alfa == 0)then
            if(app_surface_stat_scale(n) > 0.0  .AND.  &
          m1_applied_stations >= n) n_valid_obs = n_valid_obs + 1
          else
            if(app_surface_stat_scale(n) > 0.0)  &
                                    n_valid_obs = n_valid_obs + 1
          endif

        end do ! n = 1,n_surface_stat


       if (fe_log) then
          write(funit_log,*)
          write(funit_log,'(A29,I2.2,A,I2.2,A,I4,A11,I2.2,A,I2.2)')  &
       'Applied Met. Obs. valid for: ',dd,'/',mm,'-',yyyy, &
       ' for hour: ',hhm1,' - ',hh
          write(funit_log,*)
          write(funit_log,'(I3,A52)') n_valid_obs, &
       ' wind observations are to be applied for this hour.'
          write(funit_log,*)
          write(funit_log,'(A27,F12.3)') 'surface_stat_wspeed(01) = ', &
                                    surface_stat_wspeed(1)
          write(funit_log,'(A27,F12.3)') 'surface_stat_wdir(01)   = ', &
                                    surface_stat_wdir(1)
          write(funit_log,'(A27,F12.3)') 'surface_stat_tmp(01)    = ', &
                                    surface_stat_tmp(1)
          write(funit_log,'(A27,F12.3)') 'surface_stat_dt(01)     = ', &
                                    surface_stat_dt(1)
          do n = 2,n_surface_stat
            write(funit_log,'(A21,I2.2,A,F12.3)')  &
          'surface_stat_wspeed(',n,') = ',surface_stat_wspeed(n)
            write(funit_log,'(A19,I2.2,A6,F12.3)')  &
          'surface_stat_wdir(',n,')   = ',surface_stat_wdir(n)
          end do

        end if  ! if (fe_log)

        deallocate(obs_vector)


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
!	if (fe_in_geostrophic_obs) then
!
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
!
!      end if
!
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
! **********************************************************************************
!!DEC$ ELSE
!MSK 28.01.2019 commented all the below

!       do n = 1,n_surface_stat

!         if(surface_stat_tmp(n) > 150.0)then
!           tm_kel = surface_stat_tmp(n)
!           if (fe_log) then
!             write(funit_log,*)  &
!          'NOTE: The surface station temperature in K?'
!           endif
!         else
!           tm_kel = surface_stat_tmp(n) + 273.15
!         endif

!         app_surface_stat_scale(n) = surface_stat_scale(n)

!!         if(surface_stat_wspeed(n) < (0.9*UNDEF) .OR. surface_stat_wdir(n) < (0.9*UNDEF))then
!         if(surface_stat_wspeed(n) <= 0.0 .OR.  &
!                surface_stat_wdir(n) <= 0.0)then
!              surface_stat_wspeed(n) = UNDEF
!              surface_stat_wdir(n)   = UNDEF
!              app_surface_stat_scale(n) = 0.0
!         endif

!         if(surface_stat_wdir(n) > 360.0)then
!           surface_stat_wspeed(n) = UNDEF
!           surface_stat_wdir(n)   = UNDEF
!           app_surface_stat_scale(n) = 0.0
!         endif

!         if(surface_stat_wspeed(n) > 0.0 .AND.  &
!          surface_stat_wspeed(n) < minimum_obs_windspeed)  &
!             surface_stat_wspeed(n) = minimum_obs_windspeed

!         if(surface_stat_wspeed(n) > maximum_obs_windspeed)  &
!      surface_stat_wspeed(n) = maximum_obs_windspeed

!         if (fe_log) then
!           write(funit_log,*)
!           write(funit_log,'(A27,I3.3,A4,F12.3,A5)')  &
!         '      surface_stat_wspeed(',n,') = ', &
!        surface_stat_wspeed(n),' m/s.'
!           write(funit_log,'(A25,I3.3,A6,F12.3,A5)')  &
!         '      surface_stat_wdir(',n,')   = ', &
!        surface_stat_wdir(n),' deg.'
!           write(funit_log,'(A24,I3.3,A7,F12.3,A7)') &
!         '      surface_stat_tmp(',n,')    = ', &
!        surface_stat_tmp(n),' deg C.'
!           write(funit_log,'(A24,I3.3,A7,F12.3,A3)')  &
!        '  i.e. surface_stat_tmp(',n,')    = ', &
!        tm_kel,' K.'
!           write(funit_log,'(A23,I3.3,A8,F12.3,A3)')  &
!        '       surface_stat_dt(',n,')     = ', &
!        surface_stat_dt(n),' K.'
!           write(funit_log,'(A26,I3.3,A5,F12.3,A4)')  &
!        '       surface_stat_press(',n,')  = ', &
!        surface_stat_press(n),' mb.'
!           write(funit_log,'(A23,I3.3,A8,F12.3,A3)')  &
!        '       surface_stat_rh(',n,')     = ', &
!        surface_stat_rh(n),' %.'
!           write(funit_log,'(A23,I3.3,A8,F12.3,A6)')  &
!        '       surface_stat_mm(',n,')     = ', &
!        surface_stat_mm(n),' mm/h.'
!           write(funit_log,'(A23,I3.3,A8,F12.3,A6)')  &
!        '       surface_stat_sr(',n,')     = ', &
!        surface_stat_sr(n),' W/m2.'
!           write(funit_log,'(A25,I3.3,A5,F12.3,A11)')  &
!        '  app_surface_stat_scale(',n,')  = ', &
!        app_surface_stat_scale(n),' (non-dim).'
!           write(funit_log,*)
!         endif


!       end do
!!DEC$ ENDIF

      return

      end subroutine get_observations
