! <get_surf_ref_obs.for - A component of the City-scale
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

      subroutine get_surf_ref_obs

!     This routine adjusts the measured wind speed to its value at a
!     user defined reference height. This is done by the use of Monin-
!     Obukhov similarity profiles.
!
!     If the surface wind speed observation are to be interpolated along
!     the conformal surface of the midpoint of the first sigma-layer
!     the logical variable "const_ref_height" should be set to .FALSE.
!     If the interpolatation instead are to be performed along a surface
!     with a constant height of "surf_obs_ref_height" meters above
!     ground, then "const_ref_height" should be .TRUE.

! Modifications:
!    29 Jan 2019  M. Karl: replaced TAB by spaces
!
!***********************************************************************

      use module_mc_wind_files
      use module_mc_wind_domain
      use module_mc_wind_met
      use module_mc_wind_winds
      use module_mc_wind_adjust

! *** We require the use of IMPLICIT NONE:
      implicit none

! *** Local declarations:

      integer :: n

      real    :: href
      real    :: ffref
      real    :: x,y

!     move to global site:
      real ::  zh
      real ::  zr
      real ::  rnn
      real ::  ralb

      real :: ag_day,ag_night


! *** External function:
      real    :: depthxy

!=======================================================================
!
      SKIP_THIS_HOUR = .FALSE.
!     First we use the data from the main observation station to
!     compute:
!
!     "ustarv" : The friction velocity        (in m/s).
!     "tstarv" : The temperature scale value  (in Kelvin).
!     "mobulv" : The Monin-Obukhov length     (in meters).

! NOTE: In SUBROUTINE GET_OBSERVATIONS we make sure that the stability
!       data at the main station: "tm_kel" and "surface_stat_dt(1)" or
!       "surface_stat_sr(1)" are always valid data (by applying the
!       previous valid value if UNDEF values are read, and making sure
!       that we start with valid data). If, however, the Wind data from
!       the Main met station is missing, then we use the observed wind
!       speed and direction from the next non-missing station:

         n = 1
          DO

!         We assume that delta_T is measured.

          if(surface_stat_wspeed(n) < 0.99*UNDEF .AND.  &
             tm_kel                  > 0.99*UNDEF .AND. &
             surface_stat_dt(1)      > 0.99*UNDEF      )then

!           Observed wind speed is UNDEF, but stability data is valid.
            n = n + 1

          else if(surface_stat_wspeed(n) > 0.99*UNDEF .AND.  &
             tm_kel                 > 0.99*UNDEF .AND. &
             surface_stat_dt(1)     > 0.99*UNDEF      )then

!           Valid data both for wind and stability:
            domain_station_wspeed = surface_stat_wspeed(n)
            EXIT

          else
            if (fe_log) then
              write(funit_log,*)
              write(funit_log,*)  &
           ' UNDEF - values for Temp and DT at Main Met station. '
              write(funit_log,*)  &
           ' PROGRAM TERMINATES in: subroutine get_surf_ref_obs'
              write(funit_log,*)
            end if

            STOP
          end if

          if(n > n_surface_stat)then
            SKIP_THIS_HOUR = .TRUE.
            EXIT
          endif

      END DO

      IF(SKIP_THIS_HOUR) RETURN

! ********************************

! *** All of the static site information and the hourly meteorological
!     input data are now given in the proper form for the standard
!     MPP-routines to be applicable:
!
!     ------------------------------------------------------------------
!     The TURB-routines below calculates:
!
!        ustar:  The friction velocity (m/s)
!        thstar: The temperature scale (K)
!        mobul:  The Monin-Obukhov length (m)
!        qstar:  The Humidity scale (g/kg) [Only when TURB2 is applied]
!

!**********************************

      iturb = 1

      IF(iturb == 1)THEN

         call turb_profile(analytic,g_acc,d_adiab,kappa, &
                     surface_stat_hgt_vel(n), &
                     surface_stat_wspeed(n), &
                     surface_stat_z0(n),uzero, &
                     surface_stat_hgt_dtup(1),tm_kel, &
                     surface_stat_hgt_dtlo(1), &
                     tm_kel - surface_stat_dt(1), &
                     lower_lim_L,ustarv,tstarv,mobulv)

        ELSEIF(iturb == 2 .OR. iturb == 3 .OR. iturb == 4)THEN



! ***   Below we gather all calculations of thermodynamic relations:
!
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
!MSK        zh = 0.1 * surface_stat_z0(n)
!MSK        zh = 0.01 * surface_stat_z0(n)
!MSK: want zh to be same as z0

        zr = surface_stat_hgt_dtup(1)
        rnn = 0.5
        ralb = 0.25
        ag_day = 5.0
        ag_night = 5.0

        call turb_ebudget(analytic,g_acc,sb_const,d_adiab,kappa, &
                     cp,rho_air,lambda_e,s_slope,tay_pri, &
                     ag_day,ag_night,theta_d,uzero, &
                     surface_stat_wspeed(n), &
                     surface_stat_z0(n), &
                     surface_stat_hgt_vel(n), &
                     zh,zr,tm_kel,rnn,surface_stat_sr(1), &
                     ralb,g_imon,g_iday,g_ihour, &
                     g_ltogmt,clon,clat,lower_lim_L, &
                     ustarv,tstarv,qstarv,mobulv)



!	ELSEIF(iturb .eq. 3)THEN
!
!        call turb_qnet(analytic,g_acc,sigma,d_adiab,kappa,
!     &                 cp,rho_air,lambda_e,s_slope,tay_pri,
!     &                 ag_day,ag_night,theta_d,u1,u2,zu1,zu2,
!     &                 zh,zr,tm,rnn,qnet,lower_lim_L,
!                       ustar,thstar,qstar,mobul)
!
!	ELSEIF(iturb .eq. 4)THEN
!
!        call turb_bowen(analytic,g_acc,sigma,d_adiab,kappa,
!     &                  cp,rho_air,lambda_e,s_slope,tay_pri,
!     &                  ag_day,ag_night,bowen,u1,u2,zu1,zu2,
!     &                  zh,zr,tm,rnn,rkin,ralb,imon,iday,ihour,
!     &                  ltogmt,clon,clat,lower_lim_L,
!     &                  ustar,thstar,qstar,mobul)

       ENDIF


       call get_alfa

      if (fe_log) then
        write(funit_log,*)
        write(funit_log,*) ' Start info: Get_surf_ref_obs: Station ',n
        write(funit_log,*)
        write(funit_log,*) ' ustarv = ',ustarv
        write(funit_log,*) ' tstarv = ',tstarv
        write(funit_log,*) ' mobulv = ',mobulv
        write(funit_log,*)
        write(funit_log,'(1X,A,F8.4)')  &
                      ' And the corresponding ALFA =',alfa
        write(funit_log,*)
        write(funit_log,*) ' End info: Get_surf_ref_obs: Station ',n
        write(funit_log,*)
       end if
!
!=======================================================================
!
!     Calculate the wind speed at the reference height for the
!     met-observation stations.

      do n = 1,n_surface_stat

        if (const_ref_height) then

!         The surface layer observations are to be interpolated from
!         their value at a constant reference height given in meters.

          href = surf_obs_ref_height

        else

!         The surface layer observations are to be interpolated from
!         their value at the height (in meter) of the mid-point of the
!         first model layer.

          x    = surface_stat_posx(n) * 1000.0     ! Converts to meters.
          y    = surface_stat_posy(n) * 1000.0     ! Converts to meters.
          href = depthxy(im,jm,x,y,dx,dy,depth)

          href = sigmal(1)*href/h0

        end if

!       NB: At present we assume that Delta-T measurements are only
!           available at Surface station 1, and for the moment we
!           therefore apply the Station 1 Delta-T value at the other
!           surface stations as well!

        if (surface_stat_wspeed(n) > 0.99*UNDEF) then

          IF(iturb == 1)THEN

            call turb_profile(analytic,g_acc,d_adiab,kappa, &
                         surface_stat_hgt_vel(n), &
                         surface_stat_wspeed(n), &
                         surface_stat_z0(n),uzero, &
                         surface_stat_hgt_dtup(1),tm_kel, &
                         surface_stat_hgt_dtlo(1), &
                         tm_kel-surface_stat_dt(1), &
                         lower_lim_L,ustarv,tstarv,mobulv)

            ELSEIF(iturb == 2 .OR. iturb == 3 .OR. iturb == 4)THEN

! ***       Below we gather all calc. of thermodynamic relations:
!
! ***       Calculating the (dry?) air density
            rho_air = surface_stat_press(1)/(2.8704*tm_kel)
!
! ***       The latent heat of vaporization of water:
!             lambda_e = (2465. - 2.38*(tm-15.))*1.  !Used for ORG_MEPDIM!
! ***       Eliassen & Pedersen (1977) page 52:
            lambda_e = (2501. - 2.37*(tm_kel - 273.15))*1000.
!
! ***       Slope (S) of the saturation enthalphy curve, Eq.(32) in
! ***       vU&H,1985: According to vU&H,1985 this is quite well
! ***       approximated for 270 < T < 310 (i.e. -3 < T < +40 in deg C).
            s_slope=EXP(0.055*(tm_kel - 279.))     !Used for ORG_MEPDIM!
!
! ***       Garratt (1992) p 284: "Bolton's (1980) fit to Wexler (1976):
            sat_press = 6.112*EXP(17.67*(tm_kel - 273.15)/ &
                                   (tm_kel - 29.65)   )
!
! ***       Eliassen & Pedersen (1977) page 61: Approximately we have:
!             q_sat = 0.622*sat_pres/surface_stat_press(1)
!             q_2m  = surface_stat_rh(1) * q_sat
!
! ***       The Taylor-Priestly constant
            IF(surface_stat_rh(1) > 95.0)THEN
              tay_pri = 1.0                    !Used for ORG_MEPDIM!!
            ELSE
              tay_pri = dry_tay_pri
            ENDIF
!
! ***       Surface roughness for heat:
! ***       zh = 0.1 * surface_stat_z0(n)
! MSK            zh = 0.01 * surface_stat_z0(n)
! MSK: want zh to be same as z0

            zr       = surface_stat_hgt_dtup(1)
            rnn      = 0.5
            ralb     = 0.25
            ag_day   = 5.0
            ag_night = 5.0

            call turb_ebudget(analytic,g_acc,sb_const,d_adiab,kappa, &
                         cp,rho_air,lambda_e,s_slope,tay_pri, &
                         ag_day,ag_night,theta_d,uzero, &
                         surface_stat_wspeed(n), &
                         surface_stat_z0(n), &
                         surface_stat_hgt_vel(n), &
                         zh,zr,tm_kel,rnn,surface_stat_sr(1), &
                         ralb,g_imon,g_iday,g_ihour, &
                         g_ltogmt,clon,clat,lower_lim_L, &
                         ustarv,tstarv,qstarv,mobulv)


!           ELSEIF(iturb .eq. 3)THEN
!
!        call turb_qnet(analytic,g_acc,sigma,d_adiab,kappa,
!     &                    cp,rho_air,lambda_e,s_slope,tay_pri,
!     &                    ag_day,ag_night,theta_d,u1,u2,zu1,zu2,
!     &                    zh,zr,tm,rnn,qnet,lower_lim_L,
!                          ustar,thstar,qstar,mobul)
!
!           ELSEIF(iturb .eq. 4)THEN
!
!        call turb_bowen(analytic,g_acc,sigma,d_adiab,kappa,
!     &                     cp,rho_air,lambda_e,s_slope,tay_pri,
!     &                     ag_day,ag_night,bowen,u1,u2,zu1,zu2,
!     &                     zh,zr,tm,rnn,rkin,ralb,imon,iday,ihour,
!     &                     ltogmt,clon,clat,lower_lim_L,
!     &                     ustar,thstar,qstar,mobul)

         ENDIF

!          call turb_profile(analytic,g_acc,d_adiab,kappa,
!     &                      surface_stat_hgt_vel(n),
!     &                      surface_stat_wspeed(n),
!     &                      surface_stat_z0(n),uzero,
!     &                      surface_stat_hgt_dtup(1),tm_kel,
!     &                      surface_stat_hgt_dtlo(1),
!     &                      tm_kel-surface_stat_dt(1),
!     &                      lower_lim_L,ustarv,tstarv,mobulv)


          call ws_ref_height(surface_stat_z0(n), &
                        surface_stat_hgt_vel(n), &
                        surface_stat_wspeed(n), &
                        ustarv,mobulv,href,ffref)

          surface_stat_ffref(n) = ffref

        else
          ustarv                = UNDEF
          tstarv                = UNDEF
          mobulv                = UNDEF
          surface_stat_ffref(n) = UNDEF
        endif

        if (fe_log) then
          write(funit_log,*)
          write(funit_log,*) ' Start info: Get_surf_ref_obs: Station ',n
          write(funit_log,*)
          write(funit_log,*) ' ustarv = ',ustarv
          write(funit_log,*) ' tstarv = ',tstarv
          write(funit_log,*) ' mobulv = ',mobulv
          write(funit_log,*) ' ffref  = ',surface_stat_ffref(n)
          write(funit_log,*)
          write(funit_log,*) ' End info: Get_surf_ref_obs: Station ',n
          write(funit_log,*)
        end if

      end do

!=======================================================================
!
!     The values of the two vectors
!
!     "surface_stat_ffref(1:n_surface_stat)"  and
!     " surface_stat_wdir(1:n_surface_stat)
!
!     (declared in: module_mc_wind_met) now contain the wind speed and
!     the wind direction at either the user defined:
!            "surface_obs_ref_height"
!      or at the variable height of the midpoint of the first layer,
!      i.e. at: "sigmal(1)*depth(station-position)/h0".
!=======================================================================

      return
      end subroutine get_surf_ref_obs
