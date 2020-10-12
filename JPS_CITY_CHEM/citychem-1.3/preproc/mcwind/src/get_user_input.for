! <get_user_input.for - A component of the City-scale
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

      subroutine get_user_input

!_LHS Change 1: October_2010_Start:
!_LHS  !DEC$ ATTRIBUTES DLLEXPORT,STDCALL,ALIAS:'get_user_input' :: get_user_input
!_LHS Change 1: October_2010_End.

!     This routine reads (if preprocessor directive is: main_program)
!     the input parameters from the RUN_FILE, or
!     checks (if preprocessor directive is: dll_version)the imported
!     input parameters from AirQUIS.

! Modifications:
!    28 Jan 2019  M. Karl: Commented pre-processor directive 'main_program'
!    29 Jan 2019  M. Karl: Replaced TAB by spaces
!
!***********************************************************************

!     Declarations of variables by using the MODULES feature:

      use module_mc_wind_files
      use module_mc_wind_domain
      use module_mc_wind_met
      use module_mc_wind_adjust

!     The USE statement above need to be placed before the IMPLICIT NONE
!     statement below in order to avoid compilation errors.

      implicit none

!***********************************************************************

!     Local declarations:

      integer :: n
      integer :: word_len
      real    :: z0_temp

!!DEC$ IF DEFINED (main_program)
      character (len=256) :: tekststreng
!!DEC$ ELSE
!      character (len=256) :: spec_format
!!DEC$ ENDIF

!     Content of subroutine:

!     Definition of som simple input and output files:
      funit_run  = 10


       fname_run  = 'run_file.asc'

      open (unit=funit_run, file=fname_run,status='old')

!     Start reading parameters from the run-file:
!

!     Name of model-domain:
!MSK      read(funit_run,*) domain_name

!     Various file-names:
!
!     Files containing input-data:
      read(funit_run,*) fname_in_top
      read(funit_run,*) fname_in_landuse
      read(funit_run,*) fname_in_surfrough
      read(funit_run,*) fname_in_surface_obs
      read(funit_run,*) fname_in_profile_obs
      read(funit_run,*) fname_in_geostrophic_obs
!
!     Files containing output-data:
      read(funit_run,*) fname_log
      read(funit_run,*) fname_top
      read(funit_run,*) fname_wind_res
      read(funit_run,*) fname_temp_res
      read(funit_run,*) fname_rhum_res
      read(funit_run,*) fname_prec_res
      read(funit_run,*) fname_clou_res
      read(funit_run,*) fname_ts
      read(funit_run,*) fname_ts_alfa
      read(funit_run,*) fname_ts_preproc
      read(funit_run,*) fname_z0
      read(funit_run,*) fname_lu
! MSK new output files for Transphorm run
      read(funit_run,*) fname_tsrad_res
      read(funit_run,*) fname_albedo_res
      read(funit_run,*) fname_nrad_res
      read(funit_run,*) fname_pres_res
      read(funit_run,*) fname_tsmet
!
!MSK  Origo latitude and longitude
      read(funit_run,*)  clat,clon
      clon = (-1.)*clon
!MSK  Time shift of local time (UTM-LT)
      read(funit_run,*)  g_ltogmt

!     The number of wind-fields (hours) to compute:
      read(funit_run,*) n_max

!     Model dimensions:
      read(funit_run,*) im,jm,km

!     Horizontal grid dimensions (in meters):
      read(funit_run,*) dx,dy
!_LHS_Aug2007_Start:
      read(funit_run,*) first_layer_depth,stretch_factor

      if (.not. allocated (user_def_deltasigma))  &
            allocate (user_def_deltasigma(km))
       do n = 1,km
          read(funit_run,*) user_def_deltasigma(n)
       end do
!_LHS_Aug2007_End.

      read(funit_run,*) elev_max

!     Test parameters for simplified test wind input:
      read(funit_run,*) u0_test,v0_test,w0_test
      read(funit_run,*) z0_constant
      read(funit_run,*) stab_class

!     Deciding the surface profile formula to be applied:
!     Power-law formula:     i_surface_prof_method = 1
!     Zilitinkevich formula: i_surface_prof_method = 2
      read(funit_run,*) i_surface_prof_method

!     Deciding the method for calculating the PBL-height:
!     nst_method:  Method for Stable and Neutral.  Either 1 or 2.
      read(funit_run,*) nst_method

!     nust_method: Method for Unstable conditions. Either 1 or 4.
      read(funit_run,*) nust_method

!     Deciding whether any adjustment is performed on the
!     first guess field: adjust = .TRUE. (default)
      read(funit_run,*) adjust

!     If "method_alfa" == 4 we apply a constant alfa-value:
      read(funit_run,*) constant_alfa

!     The applied method for calculating "alfa" (0, 1, 2 or 3):
!         method_alfa = 0 : Alfa is estimated for each hour.
!         method_alfa = 1 : MATHEW method,
!         method_alfa = 2 : WINDS method,
!         method_alfa = 3 : Applying user-defined stab_class
!                           listed above.
!         method_alfa = 4 : Applying a constant alfa ("constant_alfa")
      read(funit_run,*) method_alfa

!     If method_alfa is equal to zero, then only the first
!     "m1_applied_stations" surface measurements in the below list are
!     applied in the iteration process:
      read(funit_run,*) m1_applied_stations

!     If method_alfa is equal to zero, then a total number
!     of "m2_tested_alfas" alfa-values are tested. These alfa-
!     values are read from the input file as programmed below:

      read(funit_run,*) m2_tested_alfas
      if (.not. allocated (pre_defined_alfa)) &
           allocate (pre_defined_alfa(m2_tested_alfas))
       do n = 1,m2_tested_alfas
         read(funit_run,*) pre_defined_alfa(n)
       end do

        if (method_alfa /= 0) m2_tested_alfas = 1

!     Parameters related to the iterative SOR method:
      read(funit_run,*) rel_par
      read(funit_run,*) lim_rel_par
      read(funit_run,*) max_iter
      read(funit_run,*) lim_iter
      read(funit_run,*) tolerance
      read(funit_run,*) lim_tolerance
      read(funit_run,*) rel_tolerance
      read(funit_run,*) lim_rel_tolerance

!     method_adjust: 1 = Div-free omega (default),
!                    2 = Test of direct calculation of omega
!                        from the lambdas.
      read(funit_run,*) method_adjust

!     Choices regarding the calculation of the Monin-Obukhov
!     stability parameters.
      read(funit_run,*) analytic
      read(funit_run,*) lower_lim_L
      read(funit_run,*) lower_lim_hmix

!     Deciding the surface ref-height.
!        Selecting mid-point of lowermost model layer
!        if: "const_ref_height" = .FALSE. (default)
      read(funit_run,*) const_ref_height
!        Selecting "surf_obs_ref_height"
!        if: "const_ref_height" = .TRUE.
      read(funit_run,*) surf_obs_ref_height

!     Minimum limit for observed wind speed:
      read(funit_run,*) minimum_obs_windspeed

!     Maximum limit for observed wind speed:
      read(funit_run,*) maximum_obs_windspeed

!     Information about the met-input data:
!     Number of surface met stations:
      read(funit_run,*) n_surface_stat

        if(method_alfa == 0)then
        if(n_surface_stat < m1_applied_stations) &
     m1_applied_stations = n_surface_stat
        m1_initial_wfields = m1_applied_stations
        if (.not. allocated (station_error)) &
      allocate (station_error(m1_applied_stations,m2_tested_alfas))
        if (.not. allocated (err_average)) &
      allocate (err_average(m2_tested_alfas))
      else
        m1_initial_wfields = 1
      end if

        if(n_surface_stat > 0)then

        if (.not. allocated (surface_stat_name)) &
              allocate (surface_stat_name(n_surface_stat))

        if (.not. allocated (surface_stat_posx)) &
              allocate (surface_stat_posx(n_surface_stat))

        if (.not. allocated (surface_stat_posy)) &
              allocate (surface_stat_posy(n_surface_stat))

        if (.not. allocated (surface_stat_hgt_vel)) &
              allocate (surface_stat_hgt_vel(n_surface_stat))

        if (.not. allocated (surface_stat_hgt_tmp)) &
              allocate (surface_stat_hgt_tmp(n_surface_stat))

        if (.not. allocated (surface_stat_hgt_dtup)) &
              allocate (surface_stat_hgt_dtup(n_surface_stat))

        if (.not. allocated (surface_stat_hgt_dtlo)) &
              allocate (surface_stat_hgt_dtlo(n_surface_stat))

        if (.not. allocated (surface_stat_wspeed)) &
              allocate (surface_stat_wspeed(n_surface_stat))

        if (.not. allocated (surface_stat_wdir)) &
              allocate (surface_stat_wdir(n_surface_stat))

        if (.not. allocated (surface_stat_tmp)) &
              allocate (surface_stat_tmp(n_surface_stat))

        if (.not. allocated (surface_stat_dt)) &
              allocate (surface_stat_dt(n_surface_stat))

        if (.not. allocated (surface_stat_sr)) &
              allocate (surface_stat_sr(n_surface_stat))

        if (.not. allocated (surface_stat_press)) &
              allocate (surface_stat_press(n_surface_stat))

        if (.not. allocated (surface_stat_rh)) &
              allocate (surface_stat_rh(n_surface_stat))

        if (.not. allocated (surface_stat_mm)) &
              allocate (surface_stat_mm(n_surface_stat))
!MSK start
        if (.not. allocated (surface_stat_clc)) &
              allocate (surface_stat_clc(n_surface_stat))

!MSK end

        if (.not. allocated (surface_stat_z0)) &
              allocate (surface_stat_z0(n_surface_stat))

        if (.not. allocated (surface_stat_pwr)) &
              allocate (surface_stat_pwr(n_surface_stat))

        if (.not. allocated (surface_stat_scale)) &
              allocate (surface_stat_scale(n_surface_stat))

        if (.not. allocated (app_surface_stat_scale)) &
              allocate (app_surface_stat_scale(n_surface_stat))

        if (.not. allocated (surface_stat_ffref)) &
              allocate (surface_stat_ffref(n_surface_stat))

! *** The variables below are linked to the application of
! *** Delta-T measurements. For the tested Oslo application
! *** this variable is only measured at Surface station 1.

        read(funit_run,*) surface_stat_name(1)
        read(funit_run,*) surface_stat_posx(1),surface_stat_posy(1)
        read(funit_run,*) surface_stat_hgt_vel(1)
        read(funit_run,*) surface_stat_hgt_tmp(1)
        read(funit_run,*) surface_stat_hgt_dtup(1)
        read(funit_run,*) surface_stat_hgt_dtlo(1)
        read(funit_run,*) surface_stat_z0(1)
        read(funit_run,*) surface_stat_pwr(1)
        read(funit_run,*) surface_stat_scale(1)

        do n = 2,n_surface_stat
          read(funit_run,*) surface_stat_name(n)
          read(funit_run,*) surface_stat_posx(n),surface_stat_posy(n)
          read(funit_run,*) surface_stat_hgt_vel(n)
!          read(funit_run,*) surface_stat_hgt_tmp(n)
!          read(funit_run,*) surface_stat_hgt_dtup(n)
!          read(funit_run,*) surface_stat_hgt_dtlo(n)
          read(funit_run,*) surface_stat_z0(n)
          read(funit_run,*) surface_stat_pwr(n)
          read(funit_run,*) surface_stat_scale(n)
        end do

      end if  ! if(n_surface_stat > 0)

!     Information about the geostrophic met-input data:
!     Number of geostrophic met stations.
      read(funit_run,*) n_geostrophic_stat

      if(n_geostrophic_stat > 0 .AND. n_surface_stat > 0)then

        if (.not. allocated (geostrophic_stat_name)) &
              allocate (geostrophic_stat_name(n_surface_stat))


        if (.not. allocated (geostrophic_stat_posx)) &
              allocate (geostrophic_stat_posx(n_geostrophic_stat))

        if (.not. allocated (geostrophic_stat_posy)) &
              allocate (geostrophic_stat_posy(n_geostrophic_stat))

        if (.not. allocated (geostrophic_stat_pwr)) &
              allocate (geostrophic_stat_pwr(n_geostrophic_stat))

        if (.not. allocated (geostrophic_stat_scale)) &
              allocate (geostrophic_stat_scale(n_geostrophic_stat))

        do n = 1,n_geostrophic_stat
          read(funit_run,*) geostrophic_stat_name(n)
          read(funit_run,*) geostrophic_stat_posx(n), &
                       geostrophic_stat_posy(n)
          read(funit_run,*) geostrophic_stat_pwr(n)
          read(funit_run,*) geostrophic_stat_scale(n)
        end do

!       Power of weight determinimg the influence of the geostrophic
!       upper air wind above the lowermost model layer.
!       Increasing importance of geostrophic value if power decrease
!       toward zero.
!       Increasing importance of surface observations if power increase
!       from 1 and upwards. A value of 1 means linear interpolation.
        if (n_geostrophic_stat > 0) &
       read(funit_run,*) geostrophic_vertical_weight_pwr

      end if  ! if(n_geostrophic_stat > 0 .AND. n_surface_stat > 0)

      close(funit_run)

!DEC$ ENDIF

! **********************************************************************

      funit_log  = 11

      fe_log     = .false.
      if (fname_log(1:1) /= ' ') then
        open (unit=funit_log, file=fname_log,status='unknown')
!        open (unit=funit_log, file=fname_log,status='new')
!        open (unit=funit_log, file=fname_log)
        fe_log = .true.
      end if

! **********************************************************************


!!DEC$ IF DEFINED (main_program)

      if (fe_log) then
        write(funit_log,'(1X,A)')         &
    '************************************************************'

        word_len = LEN_TRIM(fname_run)
        write(funit_log,'(1X,3A)') &
    'Input parameters read from the RUN_FILE: "', &
    fname_run(1:word_len),'"'

        write(funit_log,'(1X,A)') &
    '************************************************************'

        write(funit_log,*)
        write(funit_log,'(1X,A)') 'Files containing INPUT-data: '
        write(funit_log,'(1X,A)') '**************************** '
        write(funit_log,*)

        word_len = LEN_TRIM(fname_in_top)
        write(funit_log,'(1X,A58,2A)')  &
    'The name of the input TOPOGRAPHY_FILE = "', &
    fname_in_top(1:word_len),'"'

        word_len = LEN_TRIM(fname_in_landuse)
        write(funit_log,'(1X,A58,2A)')  &
    'The name of the input LANDUSE_FILE = "', &
    fname_in_landuse(1:word_len),'"'

        word_len = LEN_TRIM(fname_in_surfrough)
        write(funit_log,'(1X,A58,2A)')  &
    'The name of the input SURF_ROUGHNESS_FILE = "', &
    fname_in_surfrough(1:word_len),'"'

        word_len = LEN_TRIM(fname_in_surface_obs)
        write(funit_log,'(1X,A58,2A)')  &
    'The name of the input MET_SURFACE_OBSERVATION_FILE = "', &
    fname_in_surface_obs(1:word_len),'"'

        word_len = LEN_TRIM(fname_in_profile_obs)
        write(funit_log,'(1X,A58,2A)')  &
    'The name of the input MET_PROFILE_OBSERVATION_FILE = "', &
    fname_in_profile_obs(1:word_len),'"'

        word_len = LEN_TRIM(fname_in_geostrophic_obs)
        write(funit_log,'(1X,A58,2A)')  &
    'The name of the input MET_GEOSTROPHIC_OBSERVATION_FILE = "', &
    fname_in_geostrophic_obs(1:word_len),'"'

        write(funit_log,*)
        write(funit_log,'(1X,A)') 'Files containing OUTPUT-data: '
        write(funit_log,'(1X,A)') '***************************** '
        write(funit_log,*)

        word_len = LEN_TRIM(fname_log)
        write(funit_log,'(1X,A58,2A)')  &
    'The name of the applied LOG_FILE = "', &
    fname_log(1:word_len),'"'

        word_len = LEN_TRIM(fname_top)
        write(funit_log,'(1X,A58,2A)')  &
    'The name of the output TOPOGRAPHY_FILE = "', &
    fname_top(1:word_len),'"'

        word_len = LEN_TRIM(fname_wind_res)
        write(funit_log,'(1X,A58,2A)')  &
    'The name of the WIND RESULT_FILE = "', &
    fname_wind_res(1:word_len),'"'

        word_len = LEN_TRIM(fname_temp_res)
        write(funit_log,'(1X,A58,2A)')  &
    'The name of the TEMP RESULT_FILE = "', &
    fname_temp_res(1:word_len),'"'

        word_len = LEN_TRIM(fname_rhum_res)
        write(funit_log,'(1X,A58,2A)')  &
    'The name of the RHUM RESULT_FILE = "', &
    fname_rhum_res(1:word_len),'"'

        word_len = LEN_TRIM(fname_prec_res)
        write(funit_log,'(1X,A58,2A)')  &
    'The name of the PREC RESULT_FILE = "', &
    fname_prec_res(1:word_len),'"'

        word_len = LEN_TRIM(fname_clou_res)
        write(funit_log,'(1X,A58,2A)')  &
    'The name of the CLOU RESULT_FILE = "', &
    fname_clou_res(1:word_len),'"'

        word_len = LEN_TRIM(fname_ts)
        write(funit_log,'(1X,A58,2A)')  &
    'The name of the output TIME-SERIES_FILE = "', &
    fname_ts(1:word_len),'"'

        word_len = LEN_TRIM(fname_ts_alfa)
        write(funit_log,'(1X,A58,2A)')  &
    'The name of the ALFA_time-series_file = "', &
    fname_ts_alfa(1:word_len),'"'

        word_len = LEN_TRIM(fname_ts_preproc)
        write(funit_log,'(1X,A58,2A)')  &
    'The name of the HMIX_time-series_file = "', &
    fname_ts_preproc(1:word_len),'"'

        word_len = LEN_TRIM(fname_z0)
        write(funit_log,'(1X,A58,2A)')  &
    'The name of the output z0_FILE = "', &
    fname_z0(1:word_len),'"'

        word_len = LEN_TRIM(fname_lu)
        write(funit_log,'(1X,A58,2A)')  &
    'The name of the output LANDUSE_FILE = "', &
    fname_lu(1:word_len),'"'

! MSK new output for Transphorm run
        word_len = LEN_TRIM(fname_tsrad_res)
        write(funit_log,'(1X,A58,2A)')  &
    'The name of the output TSRAD_FILE = "', &
    fname_tsrad_res(1:word_len),'"'

        word_len = LEN_TRIM(fname_albedo_res)
        write(funit_log,'(1X,A58,2A)')  &
    'The name of the output ALBEDO_FILE = "', &
    fname_albedo_res(1:word_len),'"'

        word_len = LEN_TRIM(fname_nrad_res)
        write(funit_log,'(1X,A58,2A)')  &
    'The name of the output NRAD_FILE = "', &
    fname_nrad_res(1:word_len),'"'

        word_len = LEN_TRIM(fname_pres_res)
        write(funit_log,'(1X,A58,2A)')  &
    'The name of the output PRESSURE_FILE = "', &
    fname_pres_res(1:word_len),'"'

        word_len = LEN_TRIM(fname_tsmet)
        write(funit_log,'(1X,A58,2A)')  &
    'The name of the output TSMET_FILE = "', &
    fname_tsmet(1:word_len),'"'

        write(funit_log,*)
        write(funit_log,'(1X,A)') 'User-defined steering parameters: '
        write(funit_log,'(1X,A)') '********************************* '
        write(funit_log,*)

        write(funit_log,'(1X,A,F8.2)')  &
        'Origo latitutde = ',clat
        write(funit_log,'(1X,A,F8.2)')  &
        'Origo longitude = ',clon
        write(funit_log,'(1X,A,I5)')  &
        'Timeshift UTM-LT = ',g_ltogmt
        write(funit_log,*)
        write(funit_log,'(1X,A,I5)')  &
        'Number of fields (hours) to be calculated = ',n_max
        write(funit_log,*)
        write(funit_log,'(1X,A,I5)')  &
        'West-East   model dimension = ',im
        write(funit_log,'(1X,A,I5)')  &
    'South-North model dimension = ',jm
        write(funit_log,'(1X,A,I5)')  &
    'Ground-Top  model dimension = ',km
        write(funit_log,*)
        write(funit_log,'(1X,A,F8.2)')  &
    'West-East   grid spacing in meters       = ',dx
        write(funit_log,'(1X,A,F8.2)')  &
    'South-North grid spacing in meters       = ',dy
!_LHS_Aug2007_Start:
        write(funit_log,'(1X,A,F8.2)')  &
    'Depth of lowermost sigma layer in meters = ',first_layer_depth
        write(funit_log,'(1X,A,F8.4)')  &
    'Stretch factor for the above layers      = ',stretch_factor
        write(funit_log,*)
        write(funit_log,*) ' If the stretch_factor is negative, then: '
        do n = 1,km
         write(funit_log,'(1X,A21,I3,A13,F8.2)')  &
      'Depth of sigma layer ',n,' in meters = ', &
       user_def_deltasigma(n)
        end do
        write(funit_log,*)
!_LHS_Aug2007_End.

        write(funit_log,*)
!        if (fname_in_top(1:1) == ' ') then
          write(funit_log,'(1X,A)')  &
       '------------------------------ TEST TOPOGRAPHY -----------'
          write(funit_log,'(1X,A,F8.2)')  &
       'The maximum elevation of a possible TEST topography = ', &
        elev_max
          write(funit_log,'(1X,A,F8.2)')  &
       'The eastward comp. of the constant TEST wind speed = ', &
        u0_test
          write(funit_log,'(1X,A,F8.2)')  &
       'The northward comp. of the constant TEST wind speed = ', &
        v0_test
          write(funit_log,'(1X,A,F8.2)')  &
       'The upward Cartesian constant TEST wind speed = ', &
        w0_test
          write(funit_log,'(1X,A,F8.2)')  &
       'The constant TEST surface roughness value = ', &
        z0_constant
          write(funit_log,'(1X,A,I2)')  &
       'The applied TEST stab_class = ', &
        stab_class
          write(funit_log,'(1X,A)') &
       'The above TEST values are applied if the first character '
          write(funit_log,'(1X,A)') &
       'of the name of the input TOPOGRAPHY_FILE above is blank.'
          write(funit_log,'(1X,A)') &
       '------------------------------ TEST TOPOGRAPHY -----------'
!        end if ! fname_in_top(1:1) == ' '
        write(funit_log,*)

!        Profile formula applied above the surface observations:
         if (i_surface_prof_method == 1) then
           write(funit_log,'(1X,A)') 'A power-law profile is applied'
           write(funit_log,'(1X,A,I2)') &
         'This means that: "i_surface_prof_method" = ', &
         i_surface_prof_method
         else if (i_surface_prof_method == 2) then
           write(funit_log,'(1X,A)') 'Zilitinkevich profile is applied'
           write(funit_log,'(1X,A,I2)') &
         'This means that: "i_surface_prof_method" = ', &
         i_surface_prof_method
         else
           write(funit_log,'(1X,A)') 'Wrong value of the '//  &
         'variable: "i_surface_prof_method" has been applied.'
           write(funit_log,'(1X,A)')  &
         'The value of this variable should be: 1 or 2.'
           write(funit_log,'(1X,A)')  &
         'PROGRAM TERMINATES '
           STOP
         end if
         write(funit_log,*)

!        PBL-height formula applied for Stable and Neutral conditions
!        in the simulation.
         if (nst_method == 1) then
           write(funit_log,'(1X,A)')  &
         'Original MEPDIM formula applied for Stable '// &
         'and Neutral conditions. '
           write(funit_log,'(1X,A,I2)') &
         'This means that: "nst_method" = ', &
          nst_method
         else if (nst_method == 2) then
           write(funit_log,'(1X,A)')  &
         'Niewstadt formula applied for Stable '// &
         'and Neutral conditions. '
           write(funit_log,'(1X,A,I2)') &
         'This means that: "nst_method" = ', &
         nst_method
         else
           write(funit_log,'(1X,A)') 'Wrong value of the '//  &
         'variable: "nst_method" has been applied.'
           write(funit_log,'(1X,A)')  &
         'The value of this variable should be: 1 or 2.'
           write(funit_log,'(1X,A)')  &
         'PROGRAM TERMINATES '
           STOP
         end if
         write(funit_log,*)

!        PBL-height formula applied for Unstable conditions
!        in the simulation.
         if (nust_method == 1) then
           write(funit_log,'(1X,A)')  &
         'Original MEPDIM formula applied for Unstable conditions.'
           write(funit_log,'(1X,A,I2)') &
         'This means that: "nust_method" = ', &
          nust_method
         else if (nust_method == 2) then
           write(funit_log,'(1X,A)')  &
         'Encroachment method applied for Unstable conditions. '
           write(funit_log,'(1X,A,I2)') &
         'This means that: "nust_method" = ', &
         nust_method
         else if (nust_method == 3) then
           write(funit_log,'(1X,A)')  &
         'Simple G&B method applied for Unstable conditions. '
           write(funit_log,'(1X,A,I2)') &
         'This means that: "nust_method" = ', &
         nust_method
         else if (nust_method == 4) then
           write(funit_log,'(1X,A)')  &
         'Advanced G&B method applied for Unstable conditions. '
           write(funit_log,'(1X,A,I2)') &
         'This means that: "nust_method" = ', &
         nust_method
         else
           write(funit_log,'(1X,A)') 'Wrong value of the '//  &
         'variable: "nust_method" has been applied.'
           write(funit_log,'(1X,A)')  &
         'The value of this variable should be: 1, 2, 3 or 4.'
           write(funit_log,'(1X,A)')  &
         'PROGRAM TERMINATES '
           STOP
         end if
         write(funit_log,*)

        write(funit_log,*)
        if (adjust) then
          write(funit_log,'(1X,A)')  &
       'The mass-consistent adjustment is performed.'
          write(funit_log,'(1X,A)') &
       'This means that the input variable:  "adjust" = .TRUE.'

          if (method_alfa == 0) then
            write(funit_log,'(1X,A)') 'The alfa-values are estimated '// &
         'so as to minimize the error at the observation sites.'
            write(funit_log,'(1X,A,I2)') &
         'This means that: "method_alfa" = ',method_alfa
            write(funit_log,*)
            write(funit_log,'(1X,A,I4,A,I4,A)') 'The first ', &
         m1_applied_stations,' of the ',n_surface_stat, &
         ' surface stations are applied in the ALFA estimation'// &
         ' process.'
            write(funit_log,*)
            write(funit_log,'(1X,A,I4,A)') 'The applied alfa-value is'// &
         ' selected among the following ',m2_tested_alfas, &
         ' alfa-values: '
            write(funit_log,*)
            do n=1,m2_tested_alfas
              write(funit_log,'(1X,A,F9.5)')  &
           ' alfa = ',pre_defined_alfa(n)
            end do
            write(funit_log,*)
          else if (method_alfa == 1) then
            write(funit_log,'(1X,A)') 'The MATHEW method for '// &
         'calculating the alfa-values is applied'
            write(funit_log,'(1X,A,I2)') &
         'This means that: "method_alfa" = ',method_alfa
          else if (method_alfa == 2) then
            write(funit_log,'(1X,A)') 'The WINDS method for '//  &
         'calculating the alfa-values is applied'
            write(funit_log,'(1X,A,I2)') &
         'This means that: "method_alfa" = ',method_alfa
          else if (method_alfa == 3) then
            write(funit_log,'(1X,A)')  &
         'The User specified  STAB_CLASS defines the alfa-values.'
            write(funit_log,'(1X,A,I2)') &
         'This means that: "method_alfa" = ',method_alfa
          else if (method_alfa == 4) then
            write(funit_log,'(1X,A,F9.4,A)')  &
         'A constant alfa-value of: ',constant_alfa, &
         ' is applied throughout the simulation'
            write(funit_log,'(1X,A,I2)') &
         'This means that: "method_alfa" = ',method_alfa
          else
            write(funit_log,'(1X,A)') 'Wrong value of the '//  &
         'variable: "method_alfa" has been applied.'
            write(funit_log,'(1X,A)')  &
         'The value of this variable should be: 1, 2 or 3.'
            write(funit_log,'(1X,A)')  &
         'PROGRAM TERMINATES '
            STOP
          end if
          write(funit_log,*)

          write(funit_log,*)
          write(funit_log,'(1X,A)') 'User defined parameters '// &
       'applied in the Successive Over-Relaxation (SOR) '// &
       'procedure:'
          write(funit_log,'(1X,A,I5)')  &
       'Maximum number of SOR iterations = ',max_iter
          write(funit_log,'(1X,A,I5,A)') &
       'Changing tolerance limits and relaxation parameter after ', &
        lim_iter, ' iterations.'
          write(funit_log,*)
          write(funit_log,'(1X,A,I5,A,F5.3)')  &
       'Relaxation parameter applied for the first',lim_iter, &
       ' iterations = ',rel_par
          write(funit_log,'(1X,A,I5,A,F8.6)')  &
       'Absolute tolerance for the first',lim_iter, &
       ' iterations = ',tolerance
          write(funit_log,'(1X,A,I5,A,F8.6)')  &
       'Relative tolerance for the first',lim_iter, &
       ' iterations = ',rel_tolerance
          write(funit_log,*)
          write(funit_log,'(1X,A)') &
       'The relative tolerance must be satisfied under '// &
       'the condition that the '
          write(funit_log,'(1X,A)') &
       'absolute tolerance is simultaneously satisfied.'
          write(funit_log,*)
          write(funit_log,'(1X,A,I5,A,F5.3)')  &
       'Relaxation parameter applied after the first',lim_iter, &
       ' iterations = ',lim_rel_par
          write(funit_log,'(1X,A,I5,A,F8.6)')  &
       'Absolute tolerance after the first',lim_iter, &
       ' iterations = ',lim_tolerance
          write(funit_log,'(1X,A,I5,A,F8.6)')  &
       'Relative tolerance after the first',lim_iter, &
       ' iterations = ',lim_rel_tolerance
          write(funit_log,*)

          write(funit_log,*)
          if (method_adjust == 1) then
            write(funit_log,'(1X,A)')  &
         'The adjustment of omega is performed so that the '// &
         'resulting wind field have zero divergence.'
            write(funit_log,'(1X,A,I2)') &
         'This means that: "method_adjust" = ',method_adjust
          elseif (method_adjust == 2) then
            write(funit_log,'(1X,A)')  &
         'As a test OMEGA is calculated from the lambda-field'
            write(funit_log,'(1X,A)')  &
         'resulting in an adjusted wind field without zero '//  &
         'divergence.'
            write(funit_log,'(1X,A,I2)') &
         'This means that: "method_adjust" = ',method_adjust
          else
            write(funit_log,'(1X,A)')  &
         'Wrong value of the variable: "method_adjust" is applied.'
            write(funit_log,'(1X,A)')  &
         'The PROGRAM TERMINATES '
            STOP
          end if
          write(funit_log,*)
        else
          write(funit_log,'(1X,A)')  &
       'Only the initial first-guess field is exported. '
          write(funit_log,'(1X,A)') &
       'This means that the input variable:  "adjust" = .FALSE. '
          write(funit_log,*)
        end if ! (adjust)

        write(funit_log,*)
        if(analytic)then
          write(funit_log,'(1X,A)')  &
       'The analytic solution for very stable conditions '// &
       'is applied in the subroutine PROFILE_TURB.'
          write(funit_log,'(1X,A)') &
       'This means that the input parameter: "analytic" = .TRUE.'
        else
          write(funit_log,'(1X,A)')  &
       'The analytic solution for very stable conditions '// &
       'is NOT applied in the subroutine PROFILE_TURB.'
          write(funit_log,'(1X,A)') &
       'This means that the input parameter: "analytic" = .FALSE.'
        end if ! (analytic)

        write(funit_log,*)
        write(funit_log,'(1X,A,F8.2)')  &
    'The lower limit for the positive Monin-Obukhov length: '// &
    '"lower_lim_L" = ',lower_lim_L

        write(funit_log,*)
        write(funit_log,'(1X,A,F8.2)')  &
    'The lower limit for the PBL-height: '// &
    '"lower_lim_hmix" = ',lower_lim_hmix
        write(funit_log,*)

        write(funit_log,*)
        if(const_ref_height)then
          write(funit_log,'(1X,A56)')  &
       'The horizontal interpolation of the surface wind is done'
          write(funit_log,'(1X,A24,F6.2,A21)')  &
       'at a constant height of ',surf_obs_ref_height, &
       ' meters above ground.'
          write(funit_log,'(1X,A)') &
       'Thus, the input parameter:  "const_ref_height" = .TRUE.'
        else
          write(funit_log,'(1X,A56)')  &
       'The horizontal interpolation of the surface wind is done'
          write(funit_log,'(1X,A52)')  &
       'along the mid-point height of the first sigma layer.'
          write(funit_log,'(1X,A)') &
       'Thus, the input parameter:  "const_ref_height" = .FALSE.'
        end if
        write(funit_log,*)

        write(funit_log,*)
        write(funit_log,'(1X,A,I5)')  &
    'Number of surface observation stations to be considered = ', &
     n_surface_stat
        write(funit_log,*)
        do n = 1,n_surface_stat
          tekststreng = surface_stat_name(n)
          word_len = LEN_TRIM(tekststreng)
          write(funit_log,'(1X,A,I3,3A)')  &
       'Name of surface station ',n,' = "', &
        tekststreng(1:word_len),'"'
          write(funit_log,'(1X,A,F9.4)')  &
       'Eastward  position (in km) from model origo = ', &
        surface_stat_posx(n)
          write(funit_log,'(1X,A,F9.4)')  &
       'Northward position (in km) from model origo = ', &
        surface_stat_posy(n)
          write(funit_log,'(1X,A,F7.3)')  &
       'Wind measurement height above ground (in m) = ', &
        surface_stat_hgt_vel(n)
          if (n == 1) then
            write(funit_log,'(1X,A,F7.3)')  &
         'Temperature measurement height above ground (in m) = ', &
          surface_stat_hgt_tmp(n)
            write(funit_log,'(1X,A,F7.3)')  &
         'Upper measurement height above ground (in m) '// &
         'of Delta-T = ',surface_stat_hgt_dtup(n)
            write(funit_log,'(1X,A,F7.3)')  &
         'Lower measurement height above ground (in m) '// &
         'of Delta-T = ',surface_stat_hgt_dtlo(n)
          end if ! (n == 1)
          write(funit_log,'(1X,A,F9.4)')  &
       'Surface roughness (in m) estimated for this station = ', &
        surface_stat_z0(n)
          write(funit_log,'(1X,A,F9.4)')  &
       'Power-value applied in the IDW interpolation procedure = ', &
        surface_stat_pwr(n)
          write(funit_log,'(1X,A,F9.4)')  &
       'Scaling-value (1 or 0) deciding if this station is '// &
       'included in the IDW interpolation = ', &
        surface_stat_scale(n)
          write(funit_log,*)
        end do

        write(funit_log,*)
        write(funit_log,'(1X,A,I5)')  &
    'Number of geostrophy observation stations = ', &
     n_geostrophic_stat
        write(funit_log,*)
        do n = 1,n_geostrophic_stat
          tekststreng = geostrophic_stat_name(n)
          word_len = LEN_TRIM(tekststreng)
          write(funit_log,'(1X,A,I3,3A)')  &
       'Name of geostrophic station ',n,' = "', &
        tekststreng(1:word_len),'"'
          write(funit_log,'(1X,A,F9.4)')  &
       'Eastward  position (in km) from model origo = ', &
        geostrophic_stat_posx(n)
          write(funit_log,'(1X,A,F9.4)')  &
       'Northward position (in km) from model origo = ', &
        geostrophic_stat_posy(n)
          write(funit_log,'(1X,A,F9.4)')  &
       'Power-value applied in the IDW interpolation procedure = ', &
        geostrophic_stat_pwr(n)
          write(funit_log,'(1X,A,F9.4)')  &
       'Scaling-value (1 or 0) deciding if this station is '// &
       'included in the IDW interpolation = ', &
        geostrophic_stat_scale(n)
          write(funit_log,*)
        end do

        write(funit_log,*)
        if (n_geostrophic_stat > 0) then
!         Power of weight determinimg the influence of the geostrophic
!         upper air wind above the lowermost model layer.
!         Increasing importance of geostrophic value if power decrease
!         toward zero.
!         Increasing importance of surface observations if power
!         increase from 1 and upwards. 1 means linear interpolation.
          write(funit_log,'(1X,A)')  &
       'Scaling-value (larger than zero) deciding the vertical '// &
       'interpolation between the surface layer values '
          write(funit_log,'(1X,A,F7.4)')  &
       'and the upper air geostrophic wind: '// &
       '"geostrophic_vertical_weight_pwr" = ', &
        geostrophic_vertical_weight_pwr
        end if ! (n_geostrophic_stat > 0)

        write(funit_log,*)

        write(funit_log,*)
        write(funit_log,'(1X,A,F8.2)')  &
    'The lower bound on observed wind speed: '// &
    '"minimum_obs_windspeed" = ',minimum_obs_windspeed
        write(funit_log,'(1X,A,F8.2)')  &
    'The upper bound on observed wind speed: '// &
    '"maximum_obs_windspeed" = ',maximum_obs_windspeed

        write(funit_log,'(1X,A)') &
    'END of input parameters read from the applied RUN_FILE:'
        write(funit_log,'(1X,A)') &
    '************************************************************'
      end if

!!DEC$ ELSE

! *** Start reporting the static parameters imported from AirQUIS:
! *** I.e. when the program is run as a DLL in AirQUIS.
!
!      if (fe_log) then
!
!        word_len = LEN_TRIM(sim_id)
!        write(spec_format,'(A2,I3.3,A8)') '(A',word_len,',3X,A39)'
!        write(funit_log,spec_format) sim_id(1:word_len),
!     &    '!*** The unique ID for this simulation.'
!        write(funit_log,'(A80)') '******************************** '
!
!        write(funit_log,*)
!        write(funit_log,'(1X,A)') 'File containing LOG-data: '
!        write(funit_log,'(1X,A)') '************************* '
!        write(funit_log,*)
!
!        word_len = LEN_TRIM(fname_log)
!        write(funit_log,'(1X,A40,2A)')
!     &   'The original name of this LOG_FILE = "',
!     &   fname_log(1:word_len),'"'
!
! ***   "fname_log" from AirQUIS through:
! ***    Subroutine SendFilesDefinitions(cv_fname_log,iv_fname_log_len)
! ***   (See: module_mc_wind_files.for)
! **********************************************************************
!        write(funit_log,*)
!        write(funit_log,'(1X,A)') 'User-defined steering parameters: '
!        write(funit_log,'(1X,A)') '********************************* '
!        write(funit_log,*)
!        write(funit_log,*)
!
!        write(funit_log,'(1X,A,I5)')
!     &   'West-East   model dimension = ',im
!        write(funit_log,'(1X,A,I5)')
!     &   'South-North model dimension = ',jm
!        write(funit_log,'(1X,A,I5)')
!     &   'Ground-Top  model dimension = ',km
!        write(funit_log,*)
!        write(funit_log,'(1X,A,F8.2)')
!     &   'West-East   grid spacing in meters       = ',dx
!        write(funit_log,'(1X,A,F8.2)')
!     &   'South-North grid spacing in meters       = ',dy
!
! ***   "im,jm,km,dx,dy",  from AirQUIS through: Subroutine
! ***    SendDomainDefinitions(...,iv_im,iv_jm,iv_km,rv_dx,rv_dy,...)
! ***   (See: module_mc_wind_domain.for)
!
! ----------------------------------------------------------------------
!
!_LHS_Advanced_October_2008: first_layer_depth =  0.0
!_LHS_Advanced_October_2008: stretch_factor    = -1.0
!
!        write(funit_log,*)
!        write(funit_log,'(1X,A,F8.4)')
!     &   'Stretch factor between vertical layers = ',stretch_factor
!
! ***   "stretch_factor" harcoded to -1.0 presently.
! ----------------------------------------------------------------------
!
!        write(funit_log,*)
!        write(funit_log,*) ' If the stretch_factor is negative, then: '
!        do n = 1,km
!         write(funit_log,'(1X,A21,I3,A13,F8.2)')
!     &   'Depth of sigma layer ',n,' in meters = ',
!     &    user_def_deltasigma(n)
!        end do
!
! ***   "user_def_deltasigma(km)" from AirQUIS through:
! ***    Subroutine SendDomainDefinitions(...,rav1_ds,...)
! ***   (See: module_mc_wind_domain.for)
!       write(funit_log,*)
! ----------------------------------------------------------------------
!
! ***  MET-Station Static Data:
!
! ***   "n_surface_stat",  from AirQUIS through:
! ***    Subroutine SendMetObsStaticData(n_surface_stat,...)  and
! ***    Subroutine SendMetObsDynamicData(n_surface_stat,...)
! ***   (See: module_mc_met_domain.for)
!
!        write(funit_log,*)
!        write(funit_log,'(1X,A,I5)')
!     &   'Number of surface observation stations to be applied = ',
!     &    n_surface_stat
!        write(funit_log,*)
!        do n = 1,n_surface_stat
!          write(funit_log,'(1X,A29,I3,A3,I8)')
!     &      'ID number of surface station ',n,' = ',
!     &       surface_stat_id(n)
!          write(funit_log,'(1X,A,F9.4)')
!     &      'Eastward  position (in km) from model origo = ',
!     &       surface_stat_posx(n)
!          write(funit_log,'(1X,A,F9.4)')
!     &      'Northward position (in km) from model origo = ',
!     &       surface_stat_posy(n)
!          write(funit_log,'(1X,A,F7.3)')
!     &      'Wind measurement height above ground (in m) = ',
!     &       surface_stat_hgt_vel(n)
!          if (n == 1) then
!            write(funit_log,'(1X,A,F7.3)')
!     &        'Temperature measurement height above ground (in m) = ',
!     &         surface_stat_hgt_tmp(n)
!            write(funit_log,'(1X,A,F7.3)')
!    &        'Upper measurement height above ground (in m) '//
!     &        'of Delta-T = ',surface_stat_hgt_dtup(n)
!            write(funit_log,'(1X,A,F7.3)')
!     &        'Lower measurement height above ground (in m) '//
!    &        'of Delta-T = ',surface_stat_hgt_dtlo(n)
!_LHS_January2011_Start:
!
! ***       The code below should be removed.
!            z0_temp = 0.1 * surface_stat_z0(n)
!            if(surface_stat_hgt_dtlo(n) .LT. z0_temp)then
!               write(funit_log,'(1X,A,F12.8)')
!     &          'z0_temp = ',z0_temp
!               write(funit_log,'(1X,A)')
!     &          'NOTE: Since it is less than z0_temp it is '//
!     &          'reset to z0_temp.'
!               surface_stat_hgt_dtlo = z0_temp  ! Resets all values.
!               write(funit_log,'(1X,A,F7.3)')
!     &        'Lower measurement height above ground (in m) '//
!     &        'of Delta-T is now = ',surface_stat_hgt_dtlo(n)
!            endif
!_LHS_January2011_End.
!		end if ! (n == 1)
!          write(funit_log,'(1X,A,F9.4)')
!     &      'Surface roughness (in m) estimated for this station = ',
!     &       surface_stat_z0(n)
!          write(funit_log,'(1X,A,F9.4)')
!     &      'Power-value applied in the IDW interpolation procedure = ',
!     &       surface_stat_pwr(n)
!          write(funit_log,'(1X,A,F9.4)')
!     &      'Scaling-value (1 or 0) deciding if this station is '//
!     &      'included in the IDW interpolation = ',
!     &       surface_stat_scale(n)
!          write(funit_log,*)
!        end do
!
!       write(funit_log,*)
!        write(funit_log,'(1X,A,I5)')
!     &   'Number of geostrophy observation stations = ',
!     &    n_geostrophic_stat
!        write(funit_log,*)
!        do n = 1,n_geostrophic_stat
!          write(funit_log,'(1X,A33,I3,A3,I8)')
!     &      'ID number of geostrophic station ',n,' = ',
!     &      geostrophic_stat_id(n)
!          write(funit_log,'(1X,A,F9.4)')
!     &      'Eastward  position (in km) from model origo = ',
!     &       geostrophic_stat_posx(n)
!          write(funit_log,'(1X,A,F9.4)')
!     &      'Northward position (in km) from model origo = ',
!     &       geostrophic_stat_posy(n)
!          write(funit_log,'(1X,A,F9.4)')
!     &      'Power-value applied in the IDW interpolation procedure = ',
!     &       geostrophic_stat_pwr(n)
!          write(funit_log,'(1X,A,F9.4)')
!     &      'Scaling-value (1 or 0) deciding if this station is '//
!     &      'included in the IDW interpolation = ',
!     &       geostrophic_stat_scale(n)
!          write(funit_log,*)
!        end do
!
!        write(funit_log,*)
!        if (n_geostrophic_stat > 0) then
!         Power of weight determinimg the influence of the geostrophic
!         upper air wind above the lowermost model layer.
!         Increasing importance of geostrophic value if power decrease
!         toward zero.
!         Increasing importance of surface observations if power
!         increase from 1 and upwards. 1 means linear interpolation.
!          write(funit_log,'(1X,A)')
!     &      'Scaling-value (larger than zero) deciding the vertical '//
!     &      'interpolation between the surface layer values '
!          write(funit_log,'(1X,A,F7.4)')
!     &      'and the upper air geostrophic wind: '//
!     &      '"geostrophic_vertical_weight_pwr" = ',
!     &       geostrophic_vertical_weight_pwr
!        end if ! (n_geostrophic_stat > 0)
!
! ----------------------------------------------------------------------
!
!_LHS_Advanced_October_2008: i_surface_prof_method = 2
!
! ***   Selection of profile formula applied above the surface observations:
!        if (i_surface_prof_method == 1) then
!          write(funit_log,*)
!          write(funit_log,'(1X,A)') 'A power-law profile is applied'
!          write(funit_log,'(1X,A,I2)')
!     &        'This means that: "i_surface_prof_method" = ',
!     &        i_surface_prof_method
!        else if (i_surface_prof_method == 2) then
!          write(funit_log,*)
!          write(funit_log,'(1X,A)') 'Zilitinkevich profile is applied'
!          write(funit_log,'(1X,A,I2)')
!     &        'This means that: "i_surface_prof_method" = ',
!     &        i_surface_prof_method
!        end if
!        write(funit_log,*)
! ----------------------------------------------------------------------
!
!_LHS_Advanced_October_2008: nst_method = 2
!
! ***   PBL-height formula applied for Stable and Neutral conditions
! ***   in the simulation.
!        if (nst_method == 1) then
!          write(funit_log,'(1X,A)')
!     &        'Original MEPDIM formula applied for Stable '//
!     &        'and Neutral conditions. '
!          write(funit_log,'(1X,A,I2)')
!     &        'This means that: "nst_method" = ',
!     &         nst_method
!        else if (nst_method == 2) then
!          write(funit_log,'(1X,A)')
!     &        'Niewstadt formula applied for Stable '//
!     &        'and Neutral conditions. '
!          write(funit_log,'(1X,A,I2)')
!     &        'This means that: "nst_method" = ',
!     &        nst_method
!        end if
!        write(funit_log,*)
! ----------------------------------------------------------------------
!
!_LHS_Advanced_October_2008: nust_method = 4
!
! ***   PBL-height formula applied for Unstable conditions
! **    in the simulation.
!        if (nust_method == 1) then
!          write(funit_log,'(1X,A)')
!     &        'Original MEPDIM formula applied for Unstable conditions.'
!          write(funit_log,'(1X,A,I2)')
!     &        'This means that: "nust_method" = ',
!     &         nust_method
!         else if (nust_method == 2) then
!          write(funit_log,'(1X,A)')
!     &        'Encroachment method applied for Unstable conditions. '
!          write(funit_log,'(1X,A,I2)')
!     &        'This means that: "nust_method" = ',
!     &        nust_method
!         else if (nust_method == 3) then
!          write(funit_log,'(1X,A)')
!     &        'Simple G&B method applied for Unstable conditions. '
!          write(funit_log,'(1X,A,I2)')
!     &        'This means that: "nust_method" = ',
!     &        nust_method
!        else if (nust_method == 4) then
!          write(funit_log,'(1X,A)')
!     &        'Advanced G&B method applied for Unstable conditions. '
!          write(funit_log,'(1X,A,I2)')
!     &        'This means that: "nust_method" = ',
!     &        nust_method
!        end if
!        write(funit_log,*)
! ----------------------------------------------------------------------
!
! ***   Deciding whether adjustment is performed and which values of
! ***   alfa to apply:
!
!_LHS_Advanced_October_2008: adjust      = .true.
!_LHS_Advanced_October_2008: method_alfa = 2
!_LHS_Advanced_October_2008: constant_alfa = 1.0  ! Applied if method_alfa = 4
!
!        if (adjust) then
!          write(funit_log,'(1X,A)')
!     &      'The mass-consistent adjustment is performed.'
!          write(funit_log,'(1X,A)')
!     &      'This means that the input variable:  "adjust" = .TRUE.'
!
!          if (method_alfa == 1) then
!            write(funit_log,'(1X,A)') 'The MATHEW method for '//
!     &        'calculating the alfa-values is applied'
!            write(funit_log,'(1X,A,I2)')
!     &        'This means that: "method_alfa" = ',method_alfa
!         else if (method_alfa == 2) then
!            write(funit_log,'(1X,A)') 'The WINDS method for '//
!     &        'calculating the alfa-values is applied'
!            write(funit_log,'(1X,A,I2)')
!     &        'This means that: "method_alfa" = ',method_alfa
!         else if (method_alfa == 4) then
!            write(funit_log,*)
!            write(funit_log,'(1X,A,F9.4,A)')
!     &        'A constant alfa-value of: ',constant_alfa,
!     &        ' is applied throughout the simulation'
!            write(funit_log,'(1X,A,I2)')
!     &        'This means that: "method_alfa" = ',method_alfa
!          end if
!          write(funit_log,*)
! ----------------------------------------------------------------------
!
!          write(funit_log,*)
!          write(funit_log,'(1X,A)') 'User defined parameters '//
!     &      'applied in the Successive Over-Relaxation (SOR) '//
!     &      'procedure:'
!          write(funit_log,*)
!
!_LHS_Advanced_October_2008: max_iter      = 500
!_LHS_Advanced_October_2008: lim_iter      = 300
!_LHS_Advanced_October_2008: rel_par       = 1.78 ! SOR relax par for "lim_iter" iterat.
!_LHS_Advanced_October_2008: tolerance     = 0.01 ! Abs tolerance for "lim_iter" iterat.
!_LHS_Advanced_October_2008: rel_tolerance = 0.01 ! Rel tolerance for "lim_iter" iterat.
!
! ***     After lim_iter iterations the "lim" values below are used.
!
!_LHS_Advanced_October_2008: lim_rel_par       = 1.0
!_LHS_Advanced_October_2008: lim_tolerance     = 0.01
!_LHS_Advanced_October_2008: lim_rel_tolerance = 0.01
!
!          write(funit_log,*)
!          write(funit_log,'(1X,A,I5)')
!     &      'Maximum number of SOR iterations = ',max_iter
!          write(funit_log,*)
!          write(funit_log,'(1X,A,I5,A)')
!     &      'Changing tolerance limits and relaxation parameter after ',
!     &       lim_iter, ' iterations.'
!          write(funit_log,*)
!          write(funit_log,'(1X,A,I5,A,F5.3)')
!     &      'Relaxation parameter applied for the first',lim_iter,
!     &      ' iterations = ',rel_par
!          write(funit_log,*)
!          write(funit_log,'(1X,A,I5,A,F8.6)')
!     &      'Absolute tolerance for the first',lim_iter,
!     &      ' iterations = ',tolerance
!          write(funit_log,'(1X,A,I5,A,F8.6)')
!     &      'Relative tolerance for the first',lim_iter,
!     &      ' iterations = ',rel_tolerance
!          write(funit_log,*)
!          write(funit_log,'(1X,A)')
!     &      'The relative tolerance must be satisfied under '//
!     &      'the condition that the '
!          write(funit_log,'(1X,A)')
!     &      'absolute tolerance is simultaneously satisfied.'
!          write(funit_log,*)
!          write(funit_log,'(1X,A,I5,A,F5.3)')
!     &      'Relaxation parameter applied after the first',lim_iter,
!     &      ' iterations = ',lim_rel_par
!          write(funit_log,*)
!          write(funit_log,'(1X,A,I5,A,F8.6)')
!     &      'Absolute tolerance after the first',lim_iter,
!     &      ' iterations = ',lim_tolerance
!          write(funit_log,'(1X,A,I5,A,F8.6)')
!     &      'Relative tolerance after the first',lim_iter,
!     &      ' iterations = ',lim_rel_tolerance
!          write(funit_log,*)
! ----------------------------------------------------------------------
!
!          write(funit_log,*)
!
!_LHS_Advanced_October_2008: method_adjust   = 1  ! 1 = OMEGA calculated from Cont. Eq.
!                                                  ! 2 = OMEGA calculated from LAMBDA.
!
!          write(funit_log,*)
!          if (method_adjust == 1) then
!            write(funit_log,'(1X,A)')
!     &        'The adjustment of omega is performed so that the '//
!     &        'resulting wind field have zero divergence.'
!            write(funit_log,'(1X,A,I2)')
!     &        'This means that: "method_adjust" = ',method_adjust
!         elseif (method_adjust == 2) then
!            write(funit_log,'(1X,A)')
!     &        'As a test OMEGA is calculated from the lambda-field'
!            write(funit_log,'(1X,A)')
!     &        'resulting in an adjusted wind field without zero '//
!     &        'divergence.'
!            write(funit_log,'(1X,A,I2)')
!     &        'This means that: "method_adjust" = ',method_adjust
!        end if
!          write(funit_log,*)
!        else
!          write(funit_log,'(1X,A)')
!     &      'Only the initial first-guess field is exported. '
!          write(funit_log,'(1X,A)')
!     &      'This means that the input variable:  "adjust" = .FALSE. '
!          write(funit_log,*)
!        end if ! (adjust)
!        write(funit_log,*)
! ----------------------------------------------------------------------
!
! ***   MO-parameters:
!
!_LHS_Advanced_October_2008: analytic = .TRUE.  ! If .TRUE. analytic solution
!                           ! is applied in TURB1.
!
!        if(analytic)then
!          write(funit_log,'(1X,A)')
!     &      'The analytic solution for very stable conditions '//
!     &      'is applied in the subroutine PROFILE_TURB.'
!          write(funit_log,'(1X,A)')
!     &      'This means that the input parameter: "analytic" = .TRUE.'
!        else
!          write(funit_log,'(1X,A)')
!     &      'The analytic solution for very stable conditions '//
!     &      'is NOT applied in the subroutine PROFILE_TURB.'
!          write(funit_log,'(1X,A)')
!     &      'This means that the input parameter: "analytic" = .FALSE.'
!        end if ! (analytic)
!        write(funit_log,*)
! ----------------------------------------------------------------------
!
!_LHS_Advanced_October_2008: lower_lim_L    = 20.0
!_LHS_Advanced_October_2008: lower_lim_hmix = 50.0
!
!        write(funit_log,'(1X,A,F8.2)')
!     &   'The lower limit for the positive Monin-Obukhov length: '//
!     &   '"lower_lim_L" = ',lower_lim_L
!
!        write(funit_log,*)
!        write(funit_log,'(1X,A,F8.2)')
!     &   'The lower limit for the PBL-height: '//
!     &   '"lower_lim_hmix" = ',lower_lim_hmix
!        write(funit_log,*)
!
!        write(funit_log,*)
! ----------------------------------------------------------------------
!
!_LHS_Advanced_October_2008: const_ref_height    = .FALSE.
!_LHS_Advanced_October_2008: surf_obs_ref_height = 10.0
!
!        if(const_ref_height)then
!          write(funit_log,'(1X,A56)')
!     &      'The horizontal interpolation of the surface wind is done'
!          write(funit_log,'(1X,A24,F6.2,A21)')
!     &      'at a constant height of ',surf_obs_ref_height,
!     &      ' meters above ground.'
!          write(funit_log,'(1X,A)')
!     &      'Thus, the input parameter:  "const_ref_height" = .TRUE.'
!        else
!          write(funit_log,'(1X,A56)')
!     &      'The horizontal interpolation of the surface wind is done'
!          write(funit_log,'(1X,A52)')
!     &      'along the mid-point height of the first sigma layer.'
!          write(funit_log,'(1X,A)')
!     &      'Thus, the input parameter:  "const_ref_height" = .FALSE.'
!        end if
!        write(funit_log,*)
! ----------------------------------------------------------------------
!_LHS_Advanced_October_2008: minimum_obs_windspeed =  0.2
!_LHS_Advanced_October_2008: maximum_obs_windspeed = 40.0
!
!        write(funit_log,*)
!        write(funit_log,'(1X,A,F8.2)')
!     &   'The lower bound on observed wind speed: '//
!     &   '"minimum_obs_windspeed" = ',minimum_obs_windspeed
!        write(funit_log,'(1X,A,F8.2)')
!     &   'The upper bound on observed wind speed: '//
!     &   '"maximum_obs_windspeed" = ',maximum_obs_windspeed
! ----------------------------------------------------------------------
!        write(funit_log,*)
!        write(funit_log,'(1X,A)')
!     &   'END of input parameters read from the applied RUN_FILE:'
!        write(funit_log,'(1X,A)')
!     &   '************************************************************'
!      end if
!
!!DEC$ ENDIF

      return
      end subroutine get_user_input
