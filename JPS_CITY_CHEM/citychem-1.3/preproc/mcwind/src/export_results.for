! <export_results.for - A component of the City-scale
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

      subroutine export_results

!     This routine exports the calculated wind field to files.

! Modifications:
!     28 Jan 2019  M. Karl: i_ts and j_ts not greater im and jm
!     29 Jan 2019  M. Karl: replace TAB by spaces
!
!**********************************************************************

!     Insert the necessary MODULE calls:
      use module_mc_wind_files
      use module_mc_wind_domain
      use module_mc_wind_met
      use module_mc_wind_adjust
      use module_mc_wind_winds

!     We require the use of IMPLICIT NONE:
      implicit none

!     Local declarations:
      integer            :: i,j,k

! MSK  for dew point temperature calculation
      real, parameter    :: e_0  = 0.6112   !kPa
      real, parameter    :: l_rv = 5423.    !L/Rv in K
      real, parameter    :: t_0  = 273.15   !K
! MSK  for ground temperature calculation
      real, parameter    :: ch0  = 9.e-3
      real               :: e_t, e_sat, tdew, gtmp

      integer            :: n,i_ts,j_ts
      character (len=10) :: text1,text2

      real, allocatable  :: hor_speed(:)
      real, allocatable  :: vert_vel(:)
      real, allocatable  :: hor_dir(:)

      real, allocatable  :: topo_lam(:,:)
      real, allocatable  :: depth_lam(:,:)
      real, allocatable  :: z0_lam(:,:)
      real, allocatable  :: lu_lam(:,:)

      real, allocatable  :: u_lam(:,:,:)
      real, allocatable  :: v_lam(:,:,:)
      real, allocatable  :: w_lam(:,:,:)

!      real, allocatable :: omega_lam(:,:,:)

      real, allocatable  :: t_lam(:,:)
      real, allocatable  :: dtdz_lam(:,:)
      real, allocatable  :: rhum_lam(:,:)
      real, allocatable  :: prec_lam(:,:)
      real, allocatable  :: clou_lam(:,:)

! MSK new output for Transphorm
      real, allocatable  :: tsrad_lam(:,:)
      real, allocatable  :: albedo_lam(:,:)
      real, allocatable  :: nrad_lam(:,:)
      real, allocatable  :: pres_lam(:,:)

! Initialize
      tdew  = -9900.
      e_t   = 0.0
      e_sat = 0.0

!     Allocate the required internal memory:

      if (.not. allocated (topo_lam))  allocate (topo_lam(im,jm))
      if (.not. allocated (depth_lam)) allocate (depth_lam(im,jm))
      if (.not. allocated (z0_lam))    allocate (z0_lam(im,jm))
      if (.not. allocated (lu_lam))    allocate (lu_lam(im,jm))

      if (.not. allocated (hor_speed)) &
                      allocate (hor_speed(n_surface_stat))
      if (.not. allocated (vert_vel)) &
                      allocate (vert_vel(n_surface_stat))
      if (.not. allocated (hor_dir))   &
                      allocate (hor_dir(n_surface_stat))
      if (.not. allocated (u_lam)) allocate (u_lam(im,jm,km))
      if (.not. allocated (v_lam)) allocate (v_lam(im,jm,km))
      if (.not. allocated (w_lam)) allocate (w_lam(im,jm,km))
!      if (.not. allocated (omega_lam))
!     &  allocate (omega_lam(im,jm,km))

      if (.not. allocated (t_lam))    allocate (t_lam(im,jm))
      if (.not. allocated (dtdz_lam)) allocate (dtdz_lam(im,jm))
      if (.not. allocated (rhum_lam)) allocate (rhum_lam(im,jm))
      if (.not. allocated (prec_lam)) allocate (prec_lam(im,jm))
      if (.not. allocated (clou_lam)) allocate (clou_lam(im,jm))
! MSK new output for Transphorm
      if (.not. allocated (tsrad_lam)) allocate (tsrad_lam(im,jm))
      if (.not. allocated (albedo_lam)) allocate (albedo_lam(im,jm))
      if (.not. allocated (nrad_lam)) allocate (nrad_lam(im,jm))
      if (.not. allocated (pres_lam)) allocate (pres_lam(im,jm))


!     Select the interior points from the z0 and lu fields:
      do j = 1,jm
        do i = 1,im
          topo_lam(i,j)  = topo(i,j)
          depth_lam(i,j) = depth(i,j)
          z0_lam(i,j)    = surfrough(i,j)
          lu_lam(i,j)    = landuse(i,j)
        end do
      end do


       if(n_counter == 1)then

!       If necessary opens the result file:
!MSK wind data as binary
         fm_wind_res =2
        call opofil(fname_ts,funit_ts,fe_ts,fm_ts)
        call opofil(fname_wind_res,funit_wind_res,fe_wind_res, &
               fm_wind_res)
        call opofil(fname_temp_res,funit_temp_res,fe_temp_res, &
               fm_temp_res)
        call opofil(fname_rhum_res,funit_rhum_res,fe_rhum_res, &
               fm_rhum_res)
        call opofil(fname_prec_res,funit_prec_res,fe_prec_res, &
               fm_prec_res)
        call opofil(fname_clou_res,funit_clou_res,fe_clou_res, &
               fm_clou_res)

        call opofil(fname_ts_alfa,funit_ts_alfa,fe_ts_alfa,fm_ts_alfa)
        call opofil(fname_ts_preproc,funit_ts_preproc,fe_ts_preproc, &
               fm_ts_preproc)

        call opofil(fname_top,funit_top,fe_top,fm_top)
        call opofil(fname_z0,funit_z0,fe_z0,fm_z0)
        call opofil(fname_lu,funit_lu,fe_lu,fm_lu)
! MSK new output files for Transphorm run
        call opofil(fname_tsrad_res,funit_tsrad_res,fe_tsrad_res, &
               fm_tsrad_res)
        call opofil(fname_albedo_res,funit_albedo_res,fe_albedo_res, &
               fm_albedo_res)
        call opofil(fname_nrad_res,funit_nrad_res,fe_nrad_res, &
               fm_nrad_res)
        call opofil(fname_pres_res,funit_pres_res,fe_pres_res, &
               fm_pres_res)
        call opofil(fname_tsmet,funit_tsmet,fe_tsmet,fm_tsmet)

!       In the present version the fields below are written to file
!       just once:

!       Write the model topography and model depth to the topo-file.
        write (text2,'(A10)') '.in.meters'
        write (text1,'(A10)') 'Topography'
        call w2dfld(funit_top,fm_top,text1,text2,im,jm,topo_lam)
        write (text1,'(A10)') 'Modeldepth'
        call w2dfld(funit_top,fm_top,text1,text2,im,jm,depth_lam)

!       Write the surf_rough data to the surfrough-file.
        write (text2,'(A10)') '.in.meters'
        write (text1,'(A10)') 'z0.for.mom'
        call w2dfld(funit_z0,fm_z0,text1,text2,im,jm,z0_lam)

!       Write the land-use data to the landuse-file.
        write (text2,'(A10)') '.category.'
        write (text1,'(A10)') '.Landuse..'
        call w2dfld(funit_lu,fm_lu,text1,text2,im,jm,lu_lam)

      end if  ! (n_counter == 1)


      if (SKIP_THIS_HOUR) then
        u_lam = UNDEF
        v_lam = UNDEF
        w_lam = UNDEF
!        omega_lam = UNDEF

        t_lam    = UNDEF
        dtdz_lam = UNDEF
        rhum_lam = UNDEF
        prec_lam = UNDEF
        clou_lam = UNDEF
!MSK
        tsrad_lam = UNDEF
        albedo_lam= UNDEF
        nrad_lam  = UNDEF
        pres_lam  = UNDEF

        else

!       Calculate the wind vel. in the internal lamda point locations:
        do k = 1,km
          do j = 1,jm
            do i = 1,im
              u_lam(i,j,k)     = 0.5*(u(i-1,j,k) + u(i,j,k))
              v_lam(i,j,k)     = 0.5*(v(i,j-1,k) + v(i,j,k))
              w_lam(i,j,k)     = 0.5*(w(i,j,k-1) + w(i,j,k))
!              omega_lam(i,j,k) = 0.5*(omega(i,j,k-1) + omega(i,j,k))
            end do
          end do
        end do

        do j = 1,jm
          do i = 1,im
              t_lam(i,j)    = surface_stat_tmp(1)
              dtdz_lam(i,j) = surface_stat_dt(1)/ &
           ( surface_stat_hgt_dtup(1)- surface_stat_hgt_dtlo(1) )
!             The relative humidity is given to AirQUIS as a 0-1 number.
              rhum_lam(i,j) = surface_stat_rh(1)/100.
              prec_lam(i,j) = surface_stat_mm(1)
              clou_lam(i,j) = surface_stat_clc(1)
              pres_lam(i,j) = surface_stat_press(1)
! MSK dummy output (see subroutine RADIAT in module_mc_wind_met)
              albedo_lam(i,j) = 0.25    ! ralb value in get_surf_ref_obs
              nrad_lam(i,j)   = -9900.
              tsrad_lam(i,j)  = -9900.
          end do
        end do


      end if ! (SKIP_THIS_HOUR)


! *** All of the (main_program) code below is for writing
! *** results to file:

      if (fe_in_surface_obs .AND. fe_ts) then

! ***   Write time-series for the surface stations:
       if(n_counter == 1)then
! ***      Write heading for the time-series file:


          if (n_surface_stat == 1) then
            write(funit_ts,'(6X,5(1X,A11))')  &
        'Obs_Ho_ff','Obs_Ho_dd','Mod_Ho_ff','Mod_Ho_dd','Mod_Ho_w'
            elseif (n_surface_stat == 2) then
              write(funit_ts,'(6X,10(1X,A11))')  &
        'Obs_Ho_ff','Obs_Ho_dd','Mod_Ho_ff','Mod_Ho_dd','Mod_Ho_w', &
        'Obs_By_ff','Obs_By_dd','Mod_By_ff','Mod_By_dd','Mod_By_w'
            elseif (n_surface_stat == 7) then
              write(funit_ts,'(5X,35(1X,A11))') &
        'Obs_Ho_ff','Obs_Ho_dd','Mod_Ho_ff','Mod_Ho_dd','Mod_Ho_w', &
        'Obs_By_ff','Obs_By_dd','Mod_By_ff','Mod_By_dd','Mod_By_w', &
        'Obs_Bj_ff','Obs_Bj_dd','Mod_Bj_ff','Mod_Bj_dd','Mod_Bj_w', &
        'Obs_Sk_ff','Obs_Sk_dd','Mod_Sk_ff','Mod_Sk_dd','Mod_Sk_w', &
        'Obs_NB_ff','Obs_NB_dd','Mod_NB_ff','Mod_NB_dd','Mod_NB_w', &
        'Obs_Et_ff','Obs_Et_dd','Mod_Et_ff','Mod_Et_dd','Mod_Et_w', &
        'Obs_Fo_ff','Obs_Fo_dd','Mod_Fo_ff','Mod_Fo_dd','Mod_Fo_w'
           else
            if (fe_log) then
              write(funit_log,*) 'Error in subroutine export_results'
              write(funit_log,*) 'PROGRAM TERMINATES'
              STOP
            end if !(fe_log)
          end if !(n_surface_stat == ...

        end if ! (n_counter == 1)

        do n = 1,n_surface_stat

          i_ts = INT(surface_stat_posx(n)*1000.0/dx) + 1
          j_ts = INT(surface_stat_posy(n)*1000.0/dy) + 1

!MSK 28.01.2019 start
          i_ts = min(i_ts, im)
          j_ts = min(j_ts, jm)
!MSK 28.01.2019 end

          if(SKIP_THIS_HOUR)then
            hor_speed(n) = UNDEF
            hor_speed(n) = UNDEF
            vert_vel(n)  = UNDEF
            hor_dir(n)   = UNDEF
          else
            hor_speed(n) = (u_lam(i_ts,j_ts,1) * u_lam(i_ts,j_ts,1)) &
                     +(v_lam(i_ts,j_ts,1) * v_lam(i_ts,j_ts,1))
            hor_speed(n) = SQRT(hor_speed(n))
            vert_vel(n)  = 1000.0 * w_lam(i_ts,j_ts,1)
            hor_dir(n)   = cwdir(u_lam(i_ts,j_ts,1),v_lam(i_ts,j_ts,1))
          end if ! (SKIP_THIS_HOUR)
        end do ! n = 1,n_surface_stat



         if (n_surface_stat == 1) then
          write(funit_ts,'(1X,I4,5(1X,F11.2))')  &
       n_counter,surface_stat_wspeed(1), &
       surface_stat_wdir(1),hor_speed(1),hor_dir(1),vert_vel(1)
          elseif (n_surface_stat == 2) then
           write(funit_ts,'(1X,I4,10(1X,F11.2))')  &
       n_counter,surface_stat_wspeed(1), &
       surface_stat_wdir(1),hor_speed(1),hor_dir(1),vert_vel(1), &
       surface_stat_wspeed(2), surface_stat_wdir(2), &
       hor_speed(2),hor_dir(2),vert_vel(2)
          elseif (n_surface_stat == 7) then
           write(funit_ts,'(1X,I4,35(1X,F11.2))') &
       n_counter, &
       surface_stat_wspeed(1), surface_stat_wdir(1), &
       hor_speed(1),hor_dir(1),vert_vel(1), &
       surface_stat_wspeed(2), surface_stat_wdir(2), &
       hor_speed(2),hor_dir(2),vert_vel(2), &
       surface_stat_wspeed(3), surface_stat_wdir(3), &
       hor_speed(3),hor_dir(3),vert_vel(3), &
       surface_stat_wspeed(4), surface_stat_wdir(4), &
       hor_speed(4),hor_dir(4),vert_vel(4),       &
       surface_stat_wspeed(5), surface_stat_wdir(5), &
       hor_speed(5),hor_dir(5),vert_vel(5), &
       surface_stat_wspeed(6), surface_stat_wdir(6), &
       hor_speed(6),hor_dir(6),vert_vel(6), &
       surface_stat_wspeed(7), surface_stat_wdir(7), &
       hor_speed(7),hor_dir(7),vert_vel(7)
         else

          if (fe_log) then
            write(funit_log,*) 'Error in subroutine export_results'
            write(funit_log,*) 'PROGRAM TERMINATES'
            STOP
          end if !(fe_log)
         end if !(n_surface_stat == ...
!MSK collection of diagnostic output in tsmet_episode.asc
!    TAIR,
!MSK at the moment do the same for tsmet
          if (fe_tsmet) then

! calculate dew point temperature
           e_sat = e_0 * exp( l_rv *  &
           ( (1/t_0) - (1/(surface_stat_tmp(1)+273.15))) )
           e_t   = (surface_stat_rh(1)/100.) * e_sat
           tdew  = (-1.) / ( (log(e_t/e_0)/l_rv) - (1/t_0) )

! (MSK: GTMP should be ground/surface temperature)
! simple equation to calculate ground temperature
! Eq.(17) in G. Cautenet and Y. Coulibaly, Tellus, 37B, 64-77, 1985
!   Ch0 = u* T* / (ua(gtmp - Ta))
!            gtmp = surface_stat_tmp(1) + (ustarv*tstarv /
!     &           (ch0*surface_stat_wspeed(1)))
! Eq.(2a)
!   H   = rho_air Cp Ch0 ua (gtmp - Ta)
            gtmp = surface_stat_tmp(1) + (hsenv /  &
            (rho_air*cp*ch0*surface_stat_wspeed(1)))

! RICH: Richardson number (not calculated in MCWIND)

        if(n_counter == 1)then
! ***       Write heading for the time-series file:
          write(funit_tsmet,'(4X,18A11)')  &
         'TAIR','DTDZ','FF  ','DD  ', &
         'HMIX','PREC','RHUM','CLOU', &
         'TDEW','GTMP', &
         'MFLX','LHFX','SHFX','RICH',&
         'invL','USTAR','TSTAR','DTHDZ'
        end if

! ***   Write time-series for the surface stations:
       write(funit_tsmet,'(1X,I4,18(1X,F10.4))')  &
       n_counter,surface_stat_tmp(1),surface_stat_dt(1)/ &
       ( surface_stat_hgt_dtup(1)- surface_stat_hgt_dtlo(1) ), &
       surface_stat_wspeed(1),surface_stat_wdir(1), &
       domain_hmix, surface_stat_mm(1), &
       surface_stat_rh(1)/100., surface_stat_clc(1), &
!MSK
       tdew,gtmp, &
       tau_0v,hlatv,hsenv,-9900.0, &
       1./domain_L,ustarv,tstarv,dthdzv

       end if ! (fe_tsmet)


       end if !(fe_in_surface_obs .AND. fe_ts)

       if (fe_ts_alfa) then

! ***   Write time-series for the surface stations:
         if(n_counter == 1)then
! ***       Write heading for the time-series file:
          write(funit_ts_alfa,'(7X,A12,A10,2(A16,3A4),8A16)')  &
         'Alfa value','Obs used', &
         'Max w (m/s):','I','J','K', &
         'Min w (m/s):','I','J','K', &
         'Max modtop-w:','Min modtop-w:', &
         'Max In. 3Ddiv','Min In. 3Ddiv:', &
         'Max Fi. 3Ddiv','Min Fi. 3Ddiv:', &
         'Max 3D-Lambda','Min 3D-Lambda'
          end if
          write(funit_ts_alfa, &
           '(1X,I6,F12.5,I10,2(F16.9,3I4),8E16.6)')  &
           n_counter,alfa,n_valid_obs,max_w,MAXLOC(w),min_w,MINLOC(w), &
           max_top_omega,min_top_omega, &
           MAXVAL(div0_3D),MINVAL(div0_3D), &
           MAXVAL(div_3D),MINVAL(div_3D), &
           MAXVAL(lambda),MINVAL(lambda)
        end if ! (fe_ts_alfa)

       if (fe_ts_preproc) then

! ***   Write time-series for the surface stations:
         if(n_counter == 1)then
! ***       Write heading for the time-series file:
            write(funit_ts_preproc,'(6X,A11)')  &
        'Domain hmix'
        end if
        write(funit_ts_preproc,'(1X,I4,1X,F11.2)') n_counter,domain_hmix
      end if ! (fe_ts_preproc)

      if (km < 50 )then
!MSK wind data as binary
         fm_wind_res =2
! *** Write the total Cartesian 3D (horizontal) wind field (u, v and w
! *** components) to the res-file:
       write (text2,'(A10)') '..in..m/s.'
       do k = 1,km
        write (text1,'(A10)') 'u:.x-comp.'
        call w3dfld(funit_wind_res,fm_wind_res,k,text1,text2, &
               im,jm,km,u_lam)
        write (text1,'(A10)') 'v:.y-comp.'
        call w3dfld(funit_wind_res,fm_wind_res,k,text1,text2, &
               im,jm,km,v_lam)
        write (text1,'(A10)') 'w:.z-comp.'
        call w3dfld(funit_wind_res,fm_wind_res,k,text1,text2, &
               im,jm,km,w_lam)
!        write (text1,('A10')) 'o:.s-comp.'
!        call w3dfld(funit_wind_res,fm_wind_res,k,text1,text2,
!     &              im,jm,km,omega_lam)
       end do


      write (text2,'(A10)') '..in..C...'
      write (text1,'(A10)') '..Temp....'
      call w2dfld(funit_temp_res,fm_temp_res,text1,text2,im,jm,t_lam)

      write (text2,'(A10)') '.in.deg/m.'
      write (text1,'(A10)') '.Tempgrad.'
      call w2dfld(funit_temp_res,fm_temp_res,text1,text2,im,jm,dtdz_lam)

      write (text2,'(A10)') '..in..%...'
      write (text1,'(A10)') '..Rhum....'
      call w2dfld(funit_rhum_res,fm_rhum_res,text1,text2,im,jm,rhum_lam)

      write (text2,'(A10)') '..in..mm..'
      write (text1,'(A10)') '..Prec....'
      call w2dfld(funit_prec_res,fm_prec_res,text1,text2,im,jm,prec_lam)

      write (text2,'(A10)') '.in (0-1).'
      write (text1,'(A10)') '.Cloudcov.'
      call w2dfld(funit_clou_res,fm_clou_res,text1,text2,im,jm,clou_lam)

! MSK new result files for Transphorm run
      write (text2,'(A10)') '.in..Wm-2.'
      write (text1,'(A10)') '..TSRAD...'
      call w2dfld(funit_tsrad_res,fm_tsrad_res,text1,text2,im,jm, &
             tsrad_lam)

      write (text2,'(A10)') '.in (0-1).'
      write (text1,'(A10)') '..ALBEDO..'
      call w2dfld(funit_albedo_res,fm_albedo_res,text1,text2,im,jm, &
             albedo_lam)

      write (text2,'(A10)') '.in..Wm-2.'
      write (text1,'(A10)') '..NRAD....'
      call w2dfld(funit_nrad_res,fm_nrad_res,text1,text2,im,jm,nrad_lam)

      write (text2,'(A10)') '.in..mb...'
      write (text1,'(A10)') '.Pressure.'
      call w2dfld(funit_pres_res,fm_pres_res,text1,text2,im,jm,pres_lam)


      end if ! (km < 50)

      if(n_counter == n_max)then
! ***   If necessary close the result file:
        call clofil(funit_lu)
        call clofil(funit_z0)
        call clofil(funit_top)
        call clofil(funit_wind_res)
        call clofil(funit_temp_res)
        call clofil(funit_rhum_res)
        call clofil(funit_prec_res)
        call clofil(funit_clou_res)
        call clofil(funit_ts)
        call clofil(funit_ts_alfa)
        call clofil(funit_ts_preproc)
        call clofil(funit_tsrad_res)
        call clofil(funit_albedo_res)
        call clofil(funit_nrad_res)
        call clofil(funit_pres_res)
        call clofil(funit_tsmet)

      end if  !(n_counter == n_max)


! *** Deallocate the local arrays:

      if (allocated (topo_lam))  deallocate (topo_lam)
      if (allocated (depth_lam)) deallocate (depth_lam)
      if (allocated (z0_lam))    deallocate (z0_lam)
      if (allocated (lu_lam))    deallocate (lu_lam)

      if (allocated (hor_speed))  deallocate (hor_speed)
      if (allocated (vert_vel))   deallocate (vert_vel)
      if (allocated (hor_dir))    deallocate (hor_dir)
      if (allocated (u_lam))      deallocate (u_lam)
      if (allocated (v_lam))      deallocate (v_lam)
      if (allocated (w_lam))      deallocate (w_lam)
!      if (allocated (omega_lam)) deallocate (omega_lam)

      if (allocated (t_lam))      deallocate (t_lam)
      if (allocated (dtdz_lam))   deallocate (dtdz_lam)
      if (allocated (rhum_lam))   deallocate (rhum_lam)
      if (allocated (prec_lam))   deallocate (prec_lam)
      if (allocated (clou_lam))   deallocate (clou_lam)
!MSK
      if (allocated (tsrad_lam))  deallocate (tsrad_lam)
      if (allocated (albedo_lam)) deallocate (albedo_lam)
      if (allocated (nrad_lam))   deallocate (nrad_lam)
      if (allocated (pres_lam))   deallocate (pres_lam)

      return
      end subroutine export_results
