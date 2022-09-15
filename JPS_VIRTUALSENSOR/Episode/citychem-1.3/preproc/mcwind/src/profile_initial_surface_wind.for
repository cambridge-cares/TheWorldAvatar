! <profile_initial_surface_wind.for - A component of the City-scale
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

      subroutine profile_initial_surface_wind

!**********************************************************************
!
!     Main input:
!
!         u0_surface_ref(0:im,jm)  and
!         v0_surface_ref(im,0:jm)
!
!     Main output:
!
!         u0_surface(0:im,jm,km)    and      v0_surface(im,0:jm,km).
!     w0_surface(im,jm,0:km)    and  omega0_surface(im,jm,0:km).
!
! Modifications:
!    29 Jan 2019  M. Karl: replaced TAB characters with space
!
!
!**********************************************************************
      use module_mc_wind_files
      use module_mc_wind_domain
      use module_mc_wind_met
      use module_mc_wind_winds

!     We require the use of IMPLICIT NONE:
      implicit none

!     Local declarations:

      integer :: i,j,k

      real    :: h_above_g,h_ref,max_domain_hmix

      real    :: pi,ratio

      real    :: uu,vv,u_rot,v_rot,fi_angle,wspeed,wdir
      real    :: ust,tst,qst,mo_l

      real    :: absmo_l

      real    :: my,a_my,a_my_st,b_my,b_my_st
      real    :: zdz0,zmz0dh

      real    :: max_u0
      real    :: max_v0
      real    :: max_w0
      real    :: max_omega0

      real    :: min_u0
      real    :: min_v0
      real    :: min_w0
      real    :: min_omega0


!     move to global site:
      real ::  zh
      real ::  zr
      real ::  rnn
      real ::  ralb

      real :: ag_day,ag_night




! **********************************************************************
!     Content of the routine:
!
!     If a power-law formula is to be applied:
!      i_surface_prof_method = 1
!
!     If the Zilitinkevich profile formula is to be applied:
!      i_surface_prof_method = 2
!
! **********************************************************************
!
!     In the present version the 3D-array "w0_surface(im,jm,0:km)" 
!     is zero:

      w0_surface     = 0.0
      omega0_surface = 0.0

!     NOTE: The array "omega0_surface(im,jm,0:km)" is computed from
!          "u0_surface", "v0_surface" and "w0_surface" in subroutine
!          construct_initial_wind.
! ----------------------------------------------------------------------
!
!     In the present MC_WIND version the data from the main surface
!     observation station is used to compute:
!
!              The DOMAIN AVERAGE PBL-Height:   domain_hmix
!         and  The DOMAIN AVERAGE MONIN-OBUKHOV length: domain_L
!
!     This is done by the sequence of routines below:
!
!
! OPTIONAL: Calculate met_variables that are used by some of the
!           TURB-routines.

        call met_variables(tm_kel,surface_stat_press(1), &
                    surface_stat_rh(1),dry_tay_pri, &
                    rho_air,lambda_e,s_slope, &
                    sat_press,q,q_sat,tay_pri)


!     Calculate the M-O scaling parameters:
!     "ustarv" : The friction velocity        (in m/s).
!     "tstarv" : The temperature scale value  (in Kelvin).
!     "mobulv" : The Monin-Obukhov length     (in meters).
!     "hmixv"  : The PBL-height               (in meters).

        if(iturb == 1)then

        call turb_profile(analytic,g_acc,d_adiab,kappa, &
                     surface_stat_hgt_vel(1), &
                     domain_station_wspeed, &
                     surface_stat_z0(1), &
                     uzero, &
                     surface_stat_hgt_dtup(1), &
                     tm_kel, &
                     surface_stat_hgt_dtlo(1), &
                     tm_kel - surface_stat_dt(1), &
                     lower_lim_L, &
                     ustarv,tstarv,mobulv)

! OPTIONAL: The humidity scaling parameter is estimated by some of the
!           TURB-routines.

        qstarv = 0.0

      else

! ***   Below we gather all calculations of thermodynamic relations:
!
! ***   Calculating the (dry?) air density
        rho_air = surface_stat_press(1)/(2.8704*tm_kel)
!
! ***   The latent heat of vaporization of water:
!        lambda_e = (2465. - 2.38*(tm-15.))*1.  !Used for ORG_MEPDIM!
! ***   Eliassen & Pedersen (1977) page 52:
        lambda_e = (2501. - 2.37*(tm_kel - 273.15))*1000.
!
! ***   Slope (S) of the saturation enthalphy curve, Eq.(32) vU&H,1985:
!       According to vU&H,1985 this is quite well approximated for
!       270 < T < 310 (i.e. -3 < T < +40 in Celsius).
        s_slope=EXP(0.055*(tm_kel - 279.))     !Used for ORG_MEPDIM!
!
! ***   Garratt (1992) page 284: "Bolton's (1980) fit to Wexler (1976):
        sat_press = 6.112*EXP(17.67*(tm_kel - 273.15)/(tm_kel - 29.65))
!
! ***   Eliassen & Pedersen (1977) page 61: Approximately we have:
!         q_sat = 0.622*sat_pres/surface_stat_press(1)
!         q_2m  = surface_stat_rh(1) * q_sat
!
! ***   The Taylor-Priestly constant
        IF(surface_stat_rh(1) > 95.0)THEN
          tay_pri = 1.0                        !Used for ORG_MEPDIM!!
        ELSE
          tay_pri = dry_tay_pri
        ENDIF
!
!       Surface roughness for heat:
!        zh = 0.1 * surface_stat_z0(1)
        zh = 0.01 * surface_stat_z0(1)

        zr = surface_stat_hgt_dtup(1)
        rnn = 0.5
        ralb = 0.25
        ag_day = 5.0
        ag_night = 5.0

        call turb_ebudget(analytic,g_acc,sb_const,d_adiab,kappa, &
                     cp,rho_air,lambda_e,s_slope,tay_pri, &
                     ag_day,ag_night,theta_d,uzero, &
                     domain_station_wspeed, &
                     surface_stat_z0(1), &
                     surface_stat_hgt_vel(1), &
                     zh,zr,tm_kel,rnn,surface_stat_sr(1), &
                     ralb,g_imon,g_iday,g_ihour, &
                     g_ltogmt,clon,clat,lower_lim_L, &
                     ustarv,tstarv,qstarv,mobulv)



          end if

! OPTIONAL: Calculate fluxes of momentum and heat:
!
!         tau_0v: Surface momentum flux (N/m2)
!         hkinv:  Kinematic sensible heat flux (mK/s)
!         hsenv:  Sensible heat flux (W/m2)
!         hlatv:  Latent heat flux (W/m2) [Only when TURB2 is applied]

      call flux_1(cp,rho_air,lambda_e,ustarv,tstarv,qstarv, &
             tau_0v,hkinv,hsenv,hlatv)

!     Calculate domain averaged PBL-height:
!     Original MEPDIM:                NST_METHOD  = 1, NUST_METHOD = 1
!     Nieuwstadt  Stable and Neutral: NST_METHOD  = 2
!     Three alt. for Unstable:        NUST_METHOD = 2,3 or 4
! **********************************************************************
!	nst_method  = 1
!	nust_method = 1
! **********************************************************************
!	nst_method  = 2
!	nust_method = 4
! **********************************************************************
        domain_hmix =  MAX(domain_hmix,lower_lim_hmix)

        if (nst_method >= 1 .AND. nust_method /= 1)then

!         domain_hmix =  MAX(domain_hmix,lower_lim_hmix)

!       The routine below finds the potential temperature gradient
!       at the height HMIXV of the previous hour from an idealized
!       atmosphere:

      if (fe_log) then
        write(funit_log,*)
        write(funit_log,*) ' In: profile_initial_surface_wind. '
        write(funit_log,*) ' Just before call pot_tmpgr(...) '
        write(funit_log,*)
        write(funit_log,'(A,F12.5)') 'a_prog_hmix = ', a_prog_hmix
        write(funit_log,'(A,F12.5)') 'b_prog_hmix = ', b_prog_hmix
        write(funit_log,'(A,F12.5)') 'c_prog_hmix = ', c_prog_hmix
        write(funit_log,'(A,F12.5)') 'domain_hmix = ', domain_hmix
        write(funit_log,'(A,F12.5)') 'dthdzv_min  = ', dthdzv_min
        write(funit_log,*)
      endif

! ***    Erronous version corrected 20 December 2010:
!        call pot_tmpgr(a_prog_hmix,a_prog_hmix,a_prog_hmix,
!     &                 domain_hmix,dthdzv_min,dthdzv)

        call pot_tmpgr(a_prog_hmix,b_prog_hmix,c_prog_hmix, &
                  domain_hmix,dthdzv_min,dthdzv)

      if (fe_log) then
        write(funit_log,*)
        write(funit_log,*) ' In: profile_initial_surface_wind. '
        write(funit_log,*) ' Just after call pot_tmpgr(...) '
        write(funit_log,*)
        write(funit_log,'(A,F12.5)') 'dthdzv = ', dthdzv
        write(funit_log,*)
      endif

      endif

      if (fe_log) then
        write(funit_log,*)
        write(funit_log,*) ' In: profile_initial_surface_wind. '
        write(funit_log,*) ' Just before call mixht_new(...) '
        write(funit_log,*)
        write(funit_log,'(A,F12.4)') 'g_acc  = ', g_acc
        write(funit_log,'(A,F12.4)') 'kappa  = ', kappa
        write(funit_log,'(A,E12.5)') 'fc     = ', fc
        write(funit_log,'(A,F12.4)') 'tm_kel = ', tm_kel
        write(funit_log,'(A,F12.4)') 'ustarv = ', ustarv
        write(funit_log,'(A,F12.4)') 'mobulv = ', mobulv
        write(funit_log,'(A,F12.4)') 'hkinv  = ', hkinv
        write(funit_log,'(A,F12.4)') 'dthdzv = ', dthdzv
        write(funit_log,'(A,F12.4)') 'lower_lim_hmix = ', lower_lim_hmix
        write(funit_log,*)
      endif
!     Calculate Planetary boundary layer height (or mixing height):
        call mixht_new(nst_method,nust_method,g_acc,kappa,fc,tm_kel, &
                ustarv,mobulv,hkinv,dthdzv,lower_lim_hmix, &
                domain_hmix)

      if (fe_log) then
        write(funit_log,*)
        write(funit_log,*) ' In: profile_initial_surface_wind. '
        write(funit_log,*) ' Just after call mixht_new(...) '
        write(funit_log,*)
        write(funit_log,'(A,F12.4)') 'domain_hmix  = ', domain_hmix
        write(funit_log,*)
      endif

        domain_hmix =  MAX(domain_hmix,lower_lim_hmix)
      max_domain_hmix = 0.5 * h0
        if(domain_hmix >= max_domain_hmix) domain_hmix = max_domain_hmix
      domain_L = mobulv

      if (fe_log) then
        write(funit_log,*)
        write(funit_log,*) ' In: profile_initial_surface_wind. '
        write(funit_log,*) ' Just before call flux_2(...) '
        write(funit_log,*)
      endif

! OPTIONAL: Calculate the convective velocity scale: wstarv (m/s)
      call flux_2(g_acc,tm_kel,domain_hmix,hsenv,wstarv)


      if (fe_log) then
        write(funit_log,*)
        write(funit_log,*)  &
    ' Inside subroutine profile_initial_surface_wind: '
        write(funit_log,*)
        write(funit_log,'(A46,E12.6)')  &
    '  Profile_initial_surface_wind: ustarv      = ',ustarv
        write(funit_log,'(A46,E12.6)') &
    '  Profile_initial_surface_wind: tstarv      = ',tstarv
        write(funit_log,'(A46,E12.6)') &
    '  Profile_initial_surface_wind: domain_L    = ',domain_L
        write(funit_log,'(A46,E12.6)')  &
    '  Profile_initial_surface_wind: domain_hmix = ',domain_hmix
        write(funit_log,*)
      endif

! **********************************************************************

      if (fe_log) then
        write(funit_log,*)
        write(funit_log,*) ' In: profile_initial_surface_wind. '
        write(funit_log,*)
        write(funit_log,'(A,I4)') ' i_surface_prof_method = ', &
                               i_surface_prof_method
        write(funit_log,*)
      endif

      if (i_surface_prof_method == 1)then

!     Applying the power-law formula:
!     ===============================

!     The surface layer observations have been interpolated from their
!     values at either the mid-point of the first sigma-layer (if:
!     "const_ref_height" = .TRUE.) or at a constant reference height,
!     i.e. at "surf_obs_ref_height" (if: "const_ref_height" = .FALSE.)

      h_ref = surf_obs_ref_height

!     The u-component:
      do j = 1,jm
        do i = 0,im

          if (.NOT. const_ref_height) h_ref = sigmal(1)*depthu(i,j)/h0

          do k = 1,km

            h_above_g = sigmal(k)*depthu(i,j)/h0

!           Using the profile as long as we are below PBL-height:

            if (h_above_g <= domain_hmix)then

              u0_surface(i,j,k) = u0_surface_ref(i,j)* &
                    (h_above_g / h_ref)**pwrexp

            else

!             Here we assume a minimum setting of the PBL-height that
!             assures that the height of u0_surface(i,j,1) is below 
!             this height.

              u0_surface(i,j,k) = u0_surface(i,j,k-1)

            end if

          end do
        end do
      end do

!     The v-component:
      do j = 0,jm
        do i = 1,im

          if (.NOT. const_ref_height) h_ref = sigmal(1)*depthv(i,j)/h0

          do k = 1,km

            h_above_g = sigmal(k)*depthv(i,j)/h0

!           Using the profile as long as we are below PBL-height:

            if (h_above_g <= domain_hmix)then

              v0_surface(i,j,k) = v0_surface_ref(i,j)* &
                    (h_above_g / h_ref)**pwrexp
            else

!             Here we assume a minimum setting of the PBL-height that
!             assures that the height of v0_surface(i,j,1) is below this height.

              v0_surface(i,j,k) = v0_surface(i,j,k-1)

            end if

          end do
        end do
      end do

!=======================================================================

      else if (i_surface_prof_method == 2) then

!     Applying Zilitinkevich (WINDS) formula:
!     =======================================

      pi    = 2.0 * ASIN(1.0)
      h_ref = surf_obs_ref_height

!     The u-component:
      do j = 1,jm
        do i = 0,im

          if (.NOT. const_ref_height) h_ref = sigmal(1)*depthu(i,j)/h0

          uu = u0_surface_ref(i,j)

          if (i == 0) then
            vv = 0.5 * ( v0_surface_ref(i+1,j) +  &
                    v0_surface_ref(i+1,j-1) )
          else if (i == im) then
            vv = 0.5 * ( v0_surface_ref(i,j) +   &
                    v0_surface_ref(i,j-1) )
          else
            vv = 0.25 * (  v0_surface_ref(i,j)   +  &
                      v0_surface_ref(i+1,j) &
                    + v0_surface_ref(i,j-1) +  &
                      v0_surface_ref(i+1,j-1) )
          end if

          if (uu == 0.0 .AND. vv >= 0.0) then
            fi_angle = 0.5*pi
          else if(uu == 0.0 .AND. vv < 0.0) then
            fi_angle = 1.5*pi
          end if

          if (uu /= 0.0) then
            ratio = ABS(vv/uu)
            fi_angle    = ATAN(ratio)
!           Surface wind vector pointing toward the South-Eest Quadrant:
            if (uu > 0. .AND. vv <  0.) fi_angle = (2.0*pi) - fi_angle
!           Surface wind vector pointing toward the North-West Quadrant:
            if (uu < 0. .AND. vv >= 0.) fi_angle = pi - fi_angle
!           Surface wind vector pointing toward the South-West Quadrant:
            if (uu < 0. .AND. vv <  0.) fi_angle = pi + fi_angle
          end if

          wspeed = SQRT(uu*uu + vv*vv)
          wdir   = cwdir(uu,vv)

          if (fe_log) then
           !if (domain_name(1:4) == 'Oslo') then
            if (i == 4 .AND. j == 8) then
                write(funit_log,*)
                write(funit_log,'(1X,A49,F10.5)') &
           'Profile_initial_surface: u-point  wspeed(4,8) =', &
            wspeed
                write(funit_log,'(1X,A49,F10.5)') &
           'Profile_initial_surface: u-point    wdir(4,8) =', &
            wdir
                write(funit_log,'(1X,A49,F10.5)') &
           'Profile_initial_surface: u-point   angle(4,8) =', &
            fi_angle*180.0/pi

            end if

            if (i == 14 .AND. j == 12) then
                write(funit_log,*)
                write(funit_log,'(1X,A49,F10.5)') &
           'Profile_initial_surface: u-point  wspeed(14,12) =', &
            wspeed
                write(funit_log,'(1X,A49,F10.5)') &
           'Profile_initial_surface: u-point    wdir(14,12) =', &
            wdir
                write(funit_log,'(1X,A49,F10.5)') &
           'Profile_initial_surface: u-point   angle(14,12) =', &
            fi_angle*180.0/pi

            end if
           !end if
          end if

! *** Calculate the local: ust, tst, and mo_l values, using the
! *** local value of the surface roughness height [u0_surfrough(i,j)],
! *** the local estimated wind speed [wspeed], but the observed 
! *** Delta-T value at the main surface met-station.
! ***

      if(iturb == 1)then
          call turb_profile(analytic,g_acc,d_adiab,kappa, &
                       h_ref,wspeed, &
                       u0_surfrough(i,j),uzero, &
                       surface_stat_hgt_dtup(1),tm_kel, &
                       surface_stat_hgt_dtlo(1), &
                       tm_kel - surface_stat_dt(1), &
                       lower_lim_L,ust,tst,mo_l)


      else

! ***   Calculating the (dry?) air density
        rho_air = surface_stat_press(1)/(2.8704*tm_kel)
!
! ***   The latent heat of vaporization of water:
!         lambda_e = (2465. - 2.38*(tm-15.))*1.  !Used for ORG_MEPDIM!
! ***   Eliassen & Pedersen (1977) page 52:
        lambda_e = (2501. - 2.37*(tm_kel - 273.15))*1000.
!
! ***   Slope (S) of the saturation enthalphy curve, Eq.(32) vU&H,1985:
!       According to vU&H,1985 this is quite well approximated for
!       270 < T < 310 (i.e. -3 < T < +40 in Celsius).
        s_slope=EXP(0.055*(tm_kel - 279.))     !Used for ORG_MEPDIM!
!
! ***   Garratt (1992) page 284: "Bolton's (1980) fit to Wexler (1976):
        sat_press = 6.112*EXP(17.67*(tm_kel - 273.15)/(tm_kel - 29.65))
!
! ***   Eliassen & Pedersen (1977) page 61: Approximately we have: 
!         q_sat = 0.622*sat_pres/surface_stat_press(1)
!         q_2m  = surface_stat_rh(1) * q_sat
!
! ***   The Taylor-Priestly constant
        IF(surface_stat_rh(1) > 95.0)THEN
          tay_pri = 1.0                        !Used for ORG_MEPDIM!!
        ELSE
          tay_pri = dry_tay_pri
        ENDIF
!
!       Surface roughness for heat:
!        zh = 0.1 * u0_surfrough(i,j)
        zh = 0.01 * u0_surfrough(i,j)

        zr = surface_stat_hgt_dtup(1)
        rnn = 0.5
        ralb = 0.25
        ag_day = 5.0
        ag_night = 5.0

         call turb_ebudget(analytic,g_acc,sb_const,d_adiab,kappa, &
                     cp,rho_air,lambda_e,s_slope,tay_pri, &
                     ag_day,ag_night,theta_d,uzero, &
                     surface_stat_wspeed(1), &
                     u0_surfrough(i,j), &
                     surface_stat_hgt_vel(1), &
                     zh,zr,tm_kel,rnn,surface_stat_sr(1), &
                     ralb,g_imon,g_iday,g_ihour, &
                     g_ltogmt,clon,clat,lower_lim_L, &
                     ust,tst,qst,mo_l)



          end if

! In the following we keep the HMIX value found at Surface station 1,
! that is we keep the value: "domain_hmix".

          my = (kappa*ust)/(mo_l*ABS(fc))
          absmo_l = ABS(mo_l)

          if (mo_l < 0.0 .AND. mo_l >= -20.0) then
!           Extremely unstable:
            a_my    =   10.0/(1.0 + 1.581*SQRT(-my))
            a_my_st =  - 5.5/(1.0 + 1.581*SQRT(-my))
            b_my    = -34.0 + (38.0/(1.0 + 0.027*SQRT(-my)))
            b_my_st =  24.0 - (28.5/(1.0 + 0.027*SQRT(-my)))
          else if (mo_l < -20.0 .AND. mo_l >= -40.0) then
!           Very unstable:
            a_my    =   10.0/(1.0 + 1.581*SQRT(-my))
            a_my_st =  - 5.5/(1.0 + 1.581*SQRT(-my))
            b_my    = -34.0 + (38.0/(1.0 + 0.027*SQRT(-my)))
            b_my_st =  24.0 - (28.5/(1.0 + 0.027*SQRT(-my)))
          else if(mo_l < -40.0 .AND. mo_l >= -200.0) then
!           Unstable:
            a_my    =   10.0/(1.0 + 1.581*SQRT(-my))
            a_my_st =  - 5.5/(1.0 + 1.581*SQRT(-my))
            b_my    = -34.0 + (38.0/(1.0 + 0.027*SQRT(-my)))
            b_my_st =  24.0 - (28.5/(1.0 + 0.027*SQRT(-my)))
          else if (absmo_l > 200.0) then
!           Neutral:
            a_my    = 10.0
            a_my_st = -5.5
            b_my    =  4.0
            b_my_st = -4.5
          else if(mo_l <= 200.0 .AND. mo_l > 40.0) then
!           Stable:
            a_my    =  10.0
            a_my_st = - 5.5 + (1.765*SQRT(my))
            b_my    =   4.0 + (10.2 *SQRT(my))
            b_my_st = - 4.5
          else if(mo_l <= 40.0 .AND. mo_l >= 0.0) then
!           Very stable:
            a_my    =  10.0
            a_my_st = - 5.5 + (1.765*SQRT(my))
            b_my    =   4.0 + (10.2 *SQRT(my))
            b_my_st = - 4.5
          end if

          do k = 1,km

            if (k == 1)then
              u0_surface(i,j,k) = u0_surface_ref(i,j)
            else
              h_above_g = sigmal(k)*depthu(i,j)/h0

              zdz0   = MAX(h_above_g/u0_surfrough(i,j),1.0)
              zmz0dh = MAX((h_above_g - u0_surfrough(i,j))/domain_hmix, &
                      0.0)

              if( h_above_g > domain_hmix)then
                zdz0   =  domain_hmix/u0_surfrough(i,j)
                zmz0dh = (domain_hmix - u0_surfrough(i,j))/domain_hmix
              endif

              u_rot =  (ust/kappa) * ( LOG(zdz0) + b_my*zmz0dh &
                                + b_my_st*zmz0dh*zmz0dh   )

              v_rot = -(ust/kappa) * ( a_my*zmz0dh &
                                + a_my_st*zmz0dh*zmz0dh   )

!             The formula below must be checked:
              u0_surface(i,j,k) =  &
                     u_rot * COS(fi_angle) - v_rot * SIN(fi_angle)

            end if

            if (fe_log) then
           !  if (domain_name(1:4) == 'Oslo') then
              if (i == 4 .AND. j == 8 .AND.  k == 1) then

                  write(funit_log,*)
                  write(funit_log,'(1X,A40,F10.5)') &
               'Surface_profile:  u0_surface(4,8,1) =', &
                u0_surface(i,j,k)

              end if

              if (i == 14 .AND. j == 12 .AND.  k == 1) then

                  write(funit_log,*)
                  write(funit_log,'(1X,A40,F10.5)') &
               'Surface_profile:  u0_surface(14,12,1) =', &
                u0_surface(i,j,k)

              end if

              if (i == 4 .AND. j == 8 .AND.  k == 2) then

                  write(funit_log,*)
                  write(funit_log,'(1X,A40,F10.5)') &
               'Surface_profile:  u0_surface(4,8,2) =', &
                u0_surface(i,j,k)

              end if

              if (i == 14 .AND. j == 12 .AND.  k == 2) then

                  write(funit_log,*)
                  write(funit_log,'(1X,A40,F10.5)') &
               'Surface_profile:  u0_surface(14,12,2) =', &
                u0_surface(i,j,k)

              end if
           !  end if
            end if

          end do

        end do
      end do

!     The v-component:
      do j = 0,jm
        do i = 1,im

          if (.NOT. const_ref_height) h_ref = sigmal(1)*depthv(i,j)/h0

          vv = v0_surface_ref(i,j)

          if (j == 0) then
            uu = 0.5 * ( u0_surface_ref(i,j+1) +  &
                    u0_surface_ref(i-1,j+1) )
          else if (j == jm) then
            uu = 0.5 * ( u0_surface_ref(i,j)   +  &
                    u0_surface_ref(i-1,j) )
          else
            uu = 0.25 * (  u0_surface_ref(i,j)   +  &
                      u0_surface_ref(i,j+1) &
                    + u0_surface_ref(i-1,j) +  &
                      u0_surface_ref(i-1,j+1) )
          end if

!         The computation of "fi_angle" below must be checked:

          if (uu == 0.0 .AND. vv >= 0.0) then
            fi_angle = 0.5*pi
          else if(uu == 0.0 .AND. vv < 0.0) then
            fi_angle = 1.5*pi
          end if

          if (uu /= 0.0) then
            ratio = ABS(vv/uu)
            fi_angle    = ATAN(ratio)
!           Surface wind vector pointing toward the South-Eest Quadrant:
            if (uu > 0. .AND. vv <  0.) fi_angle = (2.0*pi) - fi_angle
!           Surface wind vector pointing toward the North-West Quadrant:
            if (uu < 0. .AND. vv >= 0.) fi_angle = pi - fi_angle
!           Surface wind vector pointing toward the South-West Quadrant:
            if (uu < 0. .AND. vv <  0.) fi_angle = pi + fi_angle
          end if

          wspeed = SQRT(uu*uu + vv*vv)
          wdir   = cwdir(uu,vv)

          if (fe_log) then
         !  if (domain_name(1:4) == 'Oslo') then
            if (i == 4 .AND. j == 8) then

                write(funit_log,*)
                write(funit_log,'(1X,A49,F10.5)') &
           'Profile_initial_surface: v-point  wspeed(4,8) =', &
            wspeed
                write(funit_log,'(1X,A49,F10.5)') &
           'Profile_initial_surface: v-point    wdir(4,8) =', &
            wdir
                write(funit_log,'(1X,A49,F10.5)') &
           'Profile_initial_surface: v-point   angle(4,8) =', &
            fi_angle*180.0/pi

            end if


            if (i == 14 .AND. j == 12) then

                write(funit_log,*)
                write(funit_log,'(1X,A49,F10.5)') &
           'Profile_initial_surface: v-point  wspeed(14,12) =', &
            wspeed
                write(funit_log,'(1X,A49,F10.5)') &
           'Profile_initial_surface: v-point    wdir(14,12) =', &
            wdir
                write(funit_log,'(1X,A49,F10.5)') &
           'Profile_initial_surface: v-point   angle(14,12) =', &
            fi_angle*180.0/pi

            end if
        !   end if
          end if

! *** Calculate the local: ust, tst, and mo_l values, using the
! *** local value of the surface roughness height [v0_surfrough(i,j)],
! *** the local estimated wind speed [wspeed], but the observed 
! *** Delta-T value at the main surface met-station.
! ***

      if(iturb == 1)then

          call turb_profile(analytic,g_acc,d_adiab,kappa, &
                       h_ref,wspeed, &
                       v0_surfrough(i,j),uzero, &
                       surface_stat_hgt_dtup(1),tm_kel, &
                       surface_stat_hgt_dtlo(1), &
                       tm_kel - surface_stat_dt(1), &
                       lower_lim_L,ust,tst,mo_l)

      else

! ***   Calculating the (dry?) air density
         rho_air = surface_stat_press(1)/(2.8704*tm_kel)
!
! ***   The latent heat of vaporization of water:
!        lambda_e = (2465. - 2.38*(tm-15.))*1.  !Used for ORG_MEPDIM!
! ***   Eliassen & Pedersen (1977) page 52:
        lambda_e = (2501. - 2.37*(tm_kel - 273.15))*1000.
!
! ***   Slope (S) of the saturation enthalphy curve, Eq.(32) vU&H,1985:
!       According to vU&H,1985 this is quite well approximated for
!       270 < T < 310 (i.e. -3 < T < +40 in Celsius).
        s_slope=EXP(0.055*(tm_kel - 279.))     !Used for ORG_MEPDIM!
!
! ***   Garratt (1992) page 284: "Bolton's (1980) fit to Wexler (1976):
        sat_press = 6.112*EXP(17.67*(tm_kel - 273.15)/(tm_kel - 29.65))
!
! ***   Eliassen & Pedersen (1977) page 61: Approximately we have: 
!         q_sat = 0.622*sat_pres/surface_stat_press(1)
!         q_2m  = surface_stat_rh(1) * q_sat
!
! ***   The Taylor-Priestly constant
        IF(surface_stat_rh(1) > 95.0)THEN
          tay_pri = 1.0                        !Used for ORG_MEPDIM!!
        ELSE
          tay_pri = dry_tay_pri
        ENDIF
!
!       Surface roughness for heat:
!        zh = 0.1 * v0_surfrough(i,j)
        zh = 0.01 * v0_surfrough(i,j)

        zr = surface_stat_hgt_dtup(1)
        rnn = 0.5
        ralb = 0.25
        ag_day = 5.0
        ag_night = 5.0

          call turb_ebudget(analytic,g_acc,sb_const,d_adiab,kappa, &
                     cp,rho_air,lambda_e,s_slope,tay_pri, &
                     ag_day,ag_night,theta_d,uzero, &
                     surface_stat_wspeed(1), &
                     v0_surfrough(i,j), &
                     surface_stat_hgt_vel(1), &
                     zh,zr,tm_kel,rnn,surface_stat_sr(1), &
                     ralb,g_imon,g_iday,g_ihour, &
                     g_ltogmt,clon,clat,lower_lim_L, &
                     ust,tst,qst,mo_l)


          end if

! In the following we keep the HMIX value found at Surface station 1,
! that is we keep the value: domain_hmix

          my = (kappa*ust)/(mo_l*ABS(fc))
          absmo_l = ABS(mo_l)

          if (mo_l < 0.0 .AND. mo_l >= -20.0) then
!           Extremely unstable:
            a_my    =   10.0/(1.0 + 1.581*SQRT(-my))
            a_my_st =  - 5.5/(1.0 + 1.581*SQRT(-my))
            b_my    = -34.0 + (38.0/(1.0 + 0.027*SQRT(-my)))
            b_my_st =  24.0 - (28.5/(1.0 + 0.027*SQRT(-my)))
          else if (mo_l < -20.0 .AND. mo_l >= -40.0) then
!           Very unstable:
            a_my    =   10.0/(1.0 + 1.581*SQRT(-my))
            a_my_st =  - 5.5/(1.0 + 1.581*SQRT(-my))
            b_my    = -34.0 + (38.0/(1.0 + 0.027*SQRT(-my)))
            b_my_st =  24.0 - (28.5/(1.0 + 0.027*SQRT(-my)))
          else if(mo_l < -40.0 .AND. mo_l >= -200.0) then
!           Unstable:
            a_my    =   10.0/(1.0 + 1.581*SQRT(-my))
            a_my_st =  - 5.5/(1.0 + 1.581*SQRT(-my))
            b_my    = -34.0 + (38.0/(1.0 + 0.027*SQRT(-my)))
            b_my_st =  24.0 - (28.5/(1.0 + 0.027*SQRT(-my)))
          else if (absmo_l > 200.0) then
!           Neutral:
            a_my    = 10.0
            a_my_st = -5.5
            b_my    =  4.0
            b_my_st = -4.5
          else if(mo_l <= 200.0 .AND. mo_l > 40.0) then
!           Stable:
            a_my    =  10.0
            a_my_st = - 5.5 + (1.765*SQRT(my))
            b_my    =   4.0 + (10.2 *SQRT(my))
            b_my_st = - 4.5
          else if(mo_l <= 40.0 .AND. mo_l >= 0.0) then
!           Very stable:
            a_my    =  10.0
            a_my_st = - 5.5 + (1.765*SQRT(my))
            b_my    =   4.0 + (10.2 *SQRT(my))
            b_my_st = - 4.5
          end if

          do k = 1,km

            if (k == 1) then

             v0_surface(i,j,k) =  v0_surface_ref(i,j)

            else

              h_above_g = sigmal(k)*depthv(i,j)/h0

              zdz0   = MAX(h_above_g/v0_surfrough(i,j),1.0)
              zmz0dh = MAX((h_above_g - v0_surfrough(i,j))/domain_hmix, &
                      0.0)

              if(h_above_g > domain_hmix)then
                zdz0   = domain_hmix/v0_surfrough(i,j)
                zmz0dh = (domain_hmix - v0_surfrough(i,j))/domain_hmix
              endif

              u_rot =  (ust/kappa) * ( LOG(zdz0) + b_my*zmz0dh &
                                + b_my_st*zmz0dh*zmz0dh   )

              v_rot = -(ust/kappa) * ( a_my*zmz0dh &
                                + a_my_st*zmz0dh*zmz0dh   )

!     The formula below must be checked:

              v0_surface(i,j,k) =  &
                     u_rot * SIN(fi_angle) + v_rot * COS(fi_angle)

            end if

            if (fe_log) then
           !  if (domain_name(1:4) == 'Oslo') then
              if (i == 4 .AND. j == 8 .AND.  k == 1) then

                  write(funit_log,*)
                  write(funit_log,'(1X,A40,F10.5)') &
             'Surface_profile:  v0_surface(4,8,1) =', &
              v0_surface(i,j,k)

              end if

              if (i == 14 .AND. j == 12 .AND.  k == 1) then

                  write(funit_log,*)
                  write(funit_log,'(1X,A40,F10.5)') &
             'Surface_profile:  v0_surface(14,12,1) =', &
              v0_surface(i,j,k)

              end if

              if (i == 4 .AND. j == 8 .AND.  k == 2) then

                  write(funit_log,*)
                  write(funit_log,'(1X,A40,F10.5)') &
             'Surface_profile:  v0_surface(4,8,2) =', &
              v0_surface(i,j,k)

              end if

              if (i == 14 .AND. j == 12 .AND.  k == 2) then

                  write(funit_log,*)
                  write(funit_log,'(1X,A40,F10.5)') &
             'Surface_profile:  v0_surface(14,12,2) =', &
              v0_surface(i,j,k)

              end if
         !    end if
            end if

          end do
        end do
      end do

      end if
!=======================================================================

      if (fe_log) then
        max_u0     = MAXVAL(u0_surface)
        max_v0     = MAXVAL(v0_surface)
        max_w0     = MAXVAL(w0_surface)
        max_omega0 = MAXVAL(omega0_surface)


        write(funit_log,*)
        write(funit_log,'(39X,3A4)') '  I ','  J ','  K '
        write(funit_log,'(1X,A27,F10.5,3I4)') &
        'Max u0_surface_based: ',max_u0,MAXLOC(u0_surface)
        write(funit_log,'(1X,A27,F10.5,3I4)') &
        'Max v0_surface_based: ',max_v0,MAXLOC(v0_surface)
        write(funit_log,'(1X,A27,F10.5,3I4)') &
        'Max w0_surface_based: ',max_w0,MAXLOC(w0_surface)
        write(funit_log,'(1X,A27,F10.5,3I4)') &
        'Max omega0_surface_based: ',max_omega0, &
         MAXLOC(omega0_surface)

        min_u0     = MINVAL(u0_surface)
        min_v0     = MINVAL(v0_surface)
        min_w0     = MINVAL(w0_surface)
        min_omega0 = MINVAL(omega0_surface)

        write(funit_log,*)
        write(funit_log,'(39X,3A4)') '  I ','  J ','  K '
        write(funit_log,'(1X,A27,F10.5,3I4)') &
        'Min u0_surface_based: ',min_u0,MINLOC(u0_surface)
         write(funit_log,'(1X,A27,F10.5,3I4)') &
        'Min v0_surface_based: ',min_v0,MINLOC(v0_surface)
        write(funit_log,'(1X,A27,F10.5,3I4)') &
        'Min w0_surface_based: ',min_w0,MINLOC(w0_surface)
        write(funit_log,'(1X,A27,F10.5,3I4)') &
        'Min omega0_surface_based: ',min_omega0, &
         MINLOC(omega0_surface)
        write(funit_log,*)
        write(funit_log,*) ' End: Profile_initial_surface_wind: '
        write(funit_log,*)
      end if

      return
      end subroutine profile_initial_surface_wind
