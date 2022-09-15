! <mc_wind.for  - A component of the City-scale
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
!*****************************************************************************! 
!  PROGRAM: MCWIND
!
!  VERSION: 1.0
!
!  PURPOSE:    This program adjust a first guess wind field to a given 
!              topography in such a manner that it becomes mass-consistent, 
!              i.e. divergence-free. This is done by use of a variational 
!              calculus technique.
!
! Based on:
!              Original source code of MCWIND by Leif Harvard Slørdal (NILU)
!
! Contact:
!              See License header
!
!*****************************************************************************!

      program mc_wind

! *** This program adjust a first guess wind field to a given
! *** topography in such a manner that it becomes mass-consistent,
! *** i.e. divergence-free. This is done by use of a variational
! *** calculus technique.

!     Modifications:
!    29 Jan 2019  M. Karl: replaced TAB by spaces
!
!
!**********************************************************************

! *** Declarations of variables by using the MODULES feature:

      use module_mc_wind_files             ! Needed in the DLL_version?
      use module_mc_wind_domain            ! Needed in the DLL_version?
      use module_mc_wind_met               ! Needed in the DLL_version?
      use module_mc_wind_winds             ! Needed in the DLL_version?
      use module_mc_wind_adjust            ! Needed in the DLL_version?

      implicit none

!**********************************************************************

! *** Local declarations:
      integer :: n,m1,m2,m3,m4,i_stat,j_stat
      real    :: u_obs,v_obs,u_mod,v_mod
      real    :: min_station_error,min_average_station_error
      real    :: scale_memory,scale_memory_previous
      real    :: max_number,least_value

! *** Content of program:

      max_number = 1.0E+36

! **********************************************************************
! *** Start special treatment for DLL_Version:
!
! *** From "module_mc_wind_files.for":
!
!      CALL SendFilesDefinitions(cv_fname_log,iv_fname_log_len)
!
!
! *** From "module_mc_wind_domain.for":
!
!      CALL SendDomainDefinitions(rv_clon,rv_clat,
!     &           iv_im,iv_jm,iv_km,rv_dx,rv_dy,rav1_ds,
!     &           rav2_in_top)
!
!
! *** From "module_mc_wind_met.for":
!
!      CALL SendMetSurfroughData(iv_im,iv_jm,rav2_in_surfrough)
!
!      CALL SendMetObsStaticData(iv_n_surface_stat,
!     &                          iav1_surface_stat_id,
!     &                          rav1_surface_stat_posx,
!     &                          rav1_surface_stat_posy,
!     &                          rav1_surface_stat_hgt_vel,
!     &                          rv_surface_stat_hgt_tmp,
!     &                          rv_surface_stat_hgt_dtup,
!     &                          rv_surface_stat_hgt_dtlo,
!     &                          iv_n_geostrophic_stat,
!     &                          iav1_geostrophic_stat_id,
!     &                          rav1_geostrophic_stat_posx,
!     &                          rav1_geostrophic_stat_posy)
!
! *** End special treatment for DLL_Version.
! **********************************************************************
!
! *** Get the user defined input defining this simulation:
      call get_user_input                   ! Needed in the DLL_version.

! *** Construct the model domain:
      call construct_domain                 ! Needed in the DLL_version.

! *** Initialising the static model variables:
      call initialize_static                ! Needed in the DLL_version.

!      n_max = 1

! *** Start of time loop:
      do n = 1,n_max

! ***   Update field_counter to help keep track of the fields:
        n_counter = n

!            write(6,'(1X,A32,I3)')
!     &           ' Starting calculation for hour: ', n


! **********************************************************************
! ***   Start special treatment for DLL_Version:
!
! ***   From "module_mc_wind_met.for":
!
!       CALL SendMetObsDynamicData(iv_n_surface_stat,
!     &                            rav1_surface_stat_wspeed,
!     &                            rav1_surface_stat_wdir,
!     &                            rv_surface_stat_tmp,
!     &                            rv_surface_stat_dt,
!     &                            rv_surface_stat_rh,
!     &                            rv_surface_stat_mm)
!
! ***   End special treatment for DLL_Version.
! **********************************************************************

          call get_observations           ! Not needed in the DLL_version.

! **********************************************************************
! ***   Start special treatment for Main_Program Version:

        if (method_alfa == 0)then

            write(funit_log,*)
            write(funit_log,*)
            write(funit_log,*)  &
        ' Starts the procedure for determining tha Alfa-value '// &
        'that minimizes the average met-station errors: '
            write(funit_log,*)

!          if(n_counter == 31)then
!            fe_log = .TRUE.
!          else
            fe_log = .FALSE.
!          end if

          do m1 = 1,m1_initial_wfields

            if(n_valid_obs > 1)then
              scale_memory = app_surface_stat_scale(m1)
              app_surface_stat_scale(m1) = 0.0
              if(m1 > 1) app_surface_stat_scale(m1-1) = &
                    scale_memory_previous
            else
! ***         Terminate program.
              PRINT*, ' FATAL ERROR IN MAIN PROGRAM: mc_wind '
              PRINT*, ' ITERATE-ALFA procedure applied whith only '
              PRINT*, ' one observation station. '
              PRINT*, ' PROGRAM TERMINATES '
              STOP
            end if

! ***       Construct the initial, or first guess, 3D wind field:

            call construct_initial_wind

            if(adjust)then
! ***         Adjust the first guess wind field:

              do m2 = 1,m2_tested_alfas
                if(m2_tested_alfas > 1)then
                  alfa = pre_defined_alfa(m2)
                else
! ***             Terminate program.
                  PRINT*, ' FATAL ERROR IN MAIN PROGRAM: mc_wind '
                  PRINT*, ' TEST-alfa procedure applied whith only '
                  PRINT*, ' one available ALFA-value to be tested. '
                  PRINT*, ' PROGRAM TERMINATES '
                  STOP
                end if

                call adjust_wfield

! ***           Compute error at station m1 for pre_defined_alfa(m2)
! ***           Store this error value, with station and alfa info

                i_stat = INT(surface_stat_posx(m1)*1000.0/dx) + 1
                j_stat = INT(surface_stat_posy(m1)*1000.0/dy) + 1

                u_mod = 0.5*(u(i_stat-1,j_stat,1)+u(i_stat,j_stat,1))
                v_mod = 0.5*(v(i_stat,j_stat-1,1)+v(i_stat,j_stat,1))

                if(surface_stat_ffref(m1) < 0.0)then
                  station_error(m1,m2) = max_number
                else
                  call comp_vel(surface_stat_ffref(m1), &
                           surface_stat_wdir(m1),u_obs,v_obs)
! ***             Absolute Error:
                  station_error(m1,m2) = (SQRT( (u_obs - u_mod)**2 &
                                          +(v_obs - v_mod)**2 ))
! ***             Error relative to observed wind speed:
!                 station_error(m1,m2) =
!     &                    station_error(m1,m2)/surface_stat_ffref(m1)
                end if

              end do ! do m2 = 1,m2_tested_alfas
            else
! ***         Terminate program.
              PRINT*, ' FATAL ERROR IN MAIN PROGRAM: mc_wind '
              PRINT*, ' TEST-alfa procedure applied when no '
              PRINT*, ' adjustment is performed. '
              PRINT*, ' PROGRAM TERMINATES '
              STOP
            end if

            scale_memory_previous = scale_memory

          end do  ! do m1 = 1,m1_initial_wfields

          app_surface_stat_scale(m1_initial_wfields) = &
                    scale_memory

! ***     Analyze the errors for all of the stations.
! ***     Write out the alfa's that give the overall smallest error.

          fe_log = .TRUE.

          if (fe_log .AND. m1_initial_wfields > 1) then

            write(funit_log,*)
            write(funit_log,*)  &
        ' Analysis of the computed errors at the various '// &
        'met-stations: '
            write(funit_log,*)

            min_station_error = MINVAL(station_error)

            write(funit_log,*)
            write(funit_log,'(28X,2A4)') '  I ','  J '
            write(funit_log,'(1X,A16,F10.5,2I4)') &
          'Min error: ',min_station_error,MINLOC(station_error)
            write(funit_log,*)

            do m1 = 1,m1_initial_wfields
              do m2 = 1,m2_tested_alfas
                write(funit_log,'(1X,A22,A13,A8,F8.3,A25,E12.5)') &
             'Not applying station: ',surface_stat_name(m1), &
             ' Alfa = ',pre_defined_alfa(m2), &
             ' Error at this station = ',station_error(m1,m2)
              end do
            end do
            write(funit_log,*)

            least_value = max_number
            do m2 = 1,m2_tested_alfas
              err_average(m2) = 0
              m4 = m1_initial_wfields
              do m1 = 1,m1_initial_wfields
                if(station_error(m1,m2) > (0.9*max_number))then
                  m4 = m4 - 1
                else
                  err_average(m2) =  err_average(m2)  &
                              + station_error(m1,m2)
                end if
              end do
              err_average(m2) = err_average(m2) / m4

              write(funit_log,'(1X,A8,F8.3,A25,F10.5)') &
              ' Alfa = ',pre_defined_alfa(m2), &
              ' Station-averaged Error = ',err_average(m2)

              if(err_average(m2) < least_value)then
                least_value = err_average(m2)
                m3 = m2
              endif
            end do

            min_average_station_error = least_value

            write(funit_log,*)
            write(funit_log,'(1X,A16,F10.5,I4)') &
          'Min av-error: ',min_average_station_error,m3

              alfa = pre_defined_alfa(m3)

            write(funit_log,*)
            write(funit_log,'(1X,A22,F12.6,A10,I3,A25)') &
            ' Chosen ALFA-value = ',alfa,' based on ',n_valid_obs, &
            ' valid wind observations.'
            write(6,'(1X,A22,F12.6,A10,I3,A25)') &
            ' Chosen ALFA-value = ',alfa,' based on ',n_valid_obs, &
            ' valid wind observations.'
          end if  ! (fe_log .AND. m1_initial_wfields > 1)

! ***     We have now estimated the optimal alfa-value by sequentially
! ***     neglecting one of the measurement stations listed in the
! ***     user_input list. We now re-calculate this solution, with ALL
! ***     of the user specified stations applied. Thus, we apply the
! ***     "memory_scale"-values.

        end if  ! (method_alfa == 0)


! ***   End special treatment for Main_Program Version.
! **********************************************************************


! ***   The following is done for      "method_alfa" > 0 and
! ***   for the Final calculation when "method_alfa" = 0.

! ***   Construct the initial, or first guess, 3D wind field:
        call construct_initial_wind         ! Needed in the DLL_version.

        if(.NOT. SKIP_THIS_HOUR)then
          if(adjust)then
! ***       Adjust the first guess wind field:
            call adjust_wfield              ! Needed in the DLL_version.
          else
! ***       Just export the initial guess field:
            call use_first_guess_wfield     ! Needed in the DLL_version.
          end if
        end if

! ***   Export the calculated wind field:
        call export_results             ! Not needed in the DLL_version.

! **********************************************************************
! ***   Start special treatment for DLL_Version:
!
! ***   From "module_mc_wind_winds.for":
!
!       CALL ReceiveWindResults(iv_im,iv_jm,iv_km,rav3_u,rav3_v)
!
! ***   End special treatment for DLL_Version:
! **********************************************************************


! *** End of time loop:
      end do  ! do n = 1,n_max

! *** Deallocate all the allocated arrays:
      call cleanup_adjust                   ! Needed in the DLL_version?
      call cleanup_winds                    ! Needed in the DLL_version?
      call cleanup_met                      ! Needed in the DLL_version?
      call cleanup_domain                   ! Needed in the DLL_version?

! *** Write the END OF PROGRAM message to log-file and close it:

      if (fe_log) then
        write(funit_log,*)
        write(funit_log,'(1X,A,I4,A,I4,A)') 'Tolerance not reached in ', &
     n_tol_not_reached,' of the ',n_max,' fields.'
        write(funit_log,*)
        write(funit_log,'(1X,A)') 'END of PROGRAM: MC_WIND'
        close(funit_log)
      end if

      write(6,*)
      write(6,'(1X,A)') 'END of PROGRAM: MC_WIND'

      end program mc_wind
