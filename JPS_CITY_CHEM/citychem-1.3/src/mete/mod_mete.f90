! <mod_mete.f90 - A component of the City-scale
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
!* Walker, S.-E., Solberg, S., Denby, B. (2003) Development and implementation of a 
!*    simplified EMEP photochemistry scheme for urban areas in EPISODE. NILU TR 13/2013. 
!*    ISBN 82-425-1524-7
!*****************************************************************************! 

      module mod_mete

! ----------------------------------------------------------------------------------
! Based on:
! Version Episode 5.5 (May29, 2012) prepared for BB-Stand-Alone
! Original source code of EPISODE by Sam-Erik Walker (NILU)
!
! Sam-Erik Walker
! Norwegian Institute for Air Research (NILU)
! Instituttveien 18 P.O. Box 100
! N-2027 Kjeller, NORWAY
! Tel: +47 63898000 Fax: +47 63898050
! E-mail: sam-erik.walker@nilu.no
!
! ----------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------
! REVISION HISTORY
!
!           2016  M. Karl: Commented kz_hfn and kz_mfn, not used.
!           2017  M. Karl: Copied here functions OBUK, PSIH, PSIM, TST_EBUDGET,
!                          TST_QNET and CWDIR
!           2017  M. Karl: Copied here functions from Sam Erik Walker's WORM
!
! ----------------------------------------------------------------------------------

      integer :: MeteExternalData

! MeteExternalData - If only data from AirQUIS DB then 0 else 1

      character(len=256) :: aerofn
      character(len=256) :: cloufn
      character(len=256) :: hmixfn
      character(len=256) :: precfn
      character(len=256) :: rhumfn
      character(len=256) :: sdvwfn
      character(len=256) :: tempfn
      character(len=256) :: windfn
!_LHS_08_Oct_2004_Start:
      character(len=256) :: ustrfn
      character(len=256) :: shflfn
      character(len=256) :: lhflfn
      character(len=256) :: lanufn
!MSK      character(len=256) :: kz_hfn
!MSK      character(len=256) :: kz_mfn
!_LHS_08_Oct_2004_End.
! MSK start TAPM files
      character(len=256) :: wstrfn
      character(len=256) :: pvstrfn
      character(len=256) :: ptstrfn
! total solar radiation
      character(len=256) :: tsradfn
      character(len=256) :: gtmpfn
      character(len=256) :: gpotfn
      character(len=256) :: pot_tfn
      character(len=256) :: ins_tfn
!MM5
      character(len=256) :: mflxfn
      character(len=256) :: tausfn
! MSK end

! aerofn - Aerodynamic resistance ... file name
! cloufn - Cloud cover .............. file name
! hmixfn - Mixing height ............ file name
! precfn - Precipitation ............ file name
! rhumfn - Relative humidity ........ file name
! sdvwfn - Standard deviation of wind file name
! tempfn - Air temperature .......... file name
! windfn - Wind ..................... file name

!_LHS_08_Oct_2004_Start:
! ustrfn - Surface friction velocity.... file name
! shflfn - Surface sensible heat flux... file name 
! lhflfn - Surface latent heat flux..... file name
! lanufn - Land use categories.......... file name
! kz_hfn - Vertical Heat Diffusivity.... file name NOT USED
! kz_mfn - Vertical Momentum Diffusivity file name NOT USED
!_LHS_08_Oct_2004_End.

      integer :: aeroun
      integer :: clouun
      integer :: hmixun
      integer :: precun
      integer :: rhumun
      integer :: sdvwun
      integer :: tempun
      integer :: windun
!_LHS_08_Oct_2004_Start:
      integer :: ustrun
      integer :: shflun
      integer :: lhflun
      integer :: lanuun
!MSK      integer :: kz_hun
!MSK      integer :: kz_mun
!_LHS_08_Oct_2004_End.
! MSK start TAPM files
      integer :: wstrun
      integer :: pvstrun
      integer :: ptstrun
      integer :: gtmpun
      integer :: tsradun
      integer :: gpotun
      integer :: pot_tun
      integer :: ins_tun
!MM5
      integer :: mflxun
      integer :: tausun
! MSK end

! aeroun - Aerodynamic resistance ... file unit
! clouun - Cloud cover .............. file unit
! hmixun - Mixing height ............ file unit
! precun - Precipitation ............ file unit
! rhumun - Relative humidity ........ file unit
! sdvwun - Standard deviation of wind file unit
! tempun - Air temperature .......... file unit
! windun - Wind ..................... file unit

!_LHS_08_Oct_2004_Start:
! ustrun - Surface friction velocity.... file unit
! shflun - Surface sensible heat flux... file unit
! lhflun - Surface latent heat flux..... file unit
! lanuun - Land use categories.......... file unit
! kz_hun - Vertical Heat Diffusivity.... file unit
! kz_mun - Vertical Momentum Diffusivity file unit
!_LHS_08_Oct_2004_End.

      logical :: aerofe
      logical :: cloufe
      logical :: hmixfe
      logical :: precfe
      logical :: rhumfe
      logical :: sdvwfe
      logical :: tempfe
      logical :: windfe
      logical :: ustrfe
      logical :: shflfe
      logical :: lhflfe
      logical :: lanufe
!MSK      logical :: kz_hfe
!MSK      logical :: kz_mfe
! MSK start TAPM files
      logical :: wstrfe
      logical :: pvstrfe
      logical :: ptstrfe
      logical :: gtmpfe
      logical :: tsradfe
      logical :: gpotfe
      logical :: pot_tfe
      logical :: ins_tfe
!MM5
      logical :: mflxfe
      logical :: tausfe
! MSK end

! aerofe - Aerodynamic resistance ... file exists
! cloufe - Cloud cover .............. file exists
! hmixfe - Mixing height ............ file exists
! precfe - Precipitation ............ file exists
! rhumfe - Relative humidity ........ file exists
! sdvwfe - Standard deviation of wind file exists
! tempfe - Air temperature .......... file exists
! windfe - Wind ..................... file exists

!_LHS_08_Oct_2004_Start:
! ustrfe - Surface friction velocity.... file exists
! shflfe - Surface sensible heat flux... file exists
! lhflfe - Surface latent heat flux..... file exists
! lanufe - Land use categories.......... file exists
! kz_hfe - Vertical Heat Diffusivity.... file exists
! kz_mfe - Vertical Momentum Diffusivity file exists
!_LHS_08_Oct_2004_End.

      real             :: aerofv
      real             :: cloufv
      real             :: hmixfv
      real             :: precfv
      real             :: rhumfv
      real,allocatable :: sdvwfv(:)
      real             :: tempfv(2)
      real,allocatable :: windfv(:)
!_LHS_08_Oct_2004_Start:
      real             :: ustrfv
      real             :: shflfv
      real             :: lhflfv
      real             :: lanufv
!MSK      real,allocatable :: kz_hfv(:)
!MSK      real,allocatable :: kz_mfv(:)
!_LHS_08_Oct_2004_End.
! MSK start TAPM files
      real             :: wstrfv
      real             :: pvstrfv
      real             :: ptstrfv
      real             :: gtmpfv
      real             :: tsradfv
      real             :: gpotfv
      real             :: pot_tfv
      real             :: ins_tfv
!MM5
      real             :: mflxfv
      real             :: tausfv(2)
! MSK end

! aerofv - Aerodynamic resistance ... file value
! cloufv - Cloud cover .............. file value
! hmixfv - Mixing height ............ file value
! precfv - Precipitation ............ file value
! rhumfv - Relative humidity ........ file value
! sdvwfv - Standard deviation of wind file values (2*nz)
! tempfv - Air temperature .......... file values
! windfv - Wind ..................... file values (2*nz)

!_LHS_08_Oct_2004_Start:
! ustrfv - Surface friction velocity.... file value
! shflfv - Surface sensible heat flux... file value
! lhflfv - Surface latent heat flux..... file value
! lanufv - Land use categories.......... file value
! kz_hfv - Vertical Heat Diffusivity.... file values (nz)
! kz_hfv - Vertical Momentum Diffusivity file values (nz)
!_LHS_08_Oct_2004_End.

      integer :: aerofm
      integer :: cloufm
      integer :: hmixfm
      integer :: precfm
      integer :: rhumfm
      integer :: sdvwfm
      integer :: tempfm
      integer :: windfm
!_LHS_08_Oct_2004_Start:
      integer :: ustrfm
      integer :: shflfm
      integer :: lhflfm
      integer :: lanufm
!MSK      integer :: kz_hfm
!MSK      integer :: kz_mfm
!_LHS_08_Oct_2004_End.
! MSK start TAPM files
      integer :: wstrfm
      integer :: pvstrfm
      integer :: ptstrfm
      integer :: gtmpfm
      integer :: tsradfm
      integer :: gpotfm
      integer :: pot_tfm
      integer :: ins_tfm
!MM5
      integer :: mflxfm
      integer :: tausfm
! MSK end

! aerofm - Aerodynamic resistance ... file format (index)
! cloufm - Cloud cover .............. file format (index)
! hmixfm - Mixing height ............ file format (index)
! precfm - Precipitation ............ file format (index)
! rhumfm - Relative humidity ........ file format (index)
! sdvwfm - Standard deviation of wind file format (index)
! tempfm - Air temperature .......... file format (index)
! windfm - Wind ..................... file format (index)

!_LHS_08_Oct_2004_Start:
! ustrfm - Surface friction velocity.... file format (index)
! shflfm - Surface sensible heat flux... file format (index)
! lhflfm - Surface latent heat flux..... file format (index)
! lanufm - Land use categories.......... file format (index)
! kz_hfm - Vertical Heat Diffusivity.... file format (index)
! kz_mfm - Vertical Momentum Diffusivity file format (index)
!_LHS_08_Oct_2004_End.

      real,   allocatable :: dtdz(:,:)
      real,   allocatable :: tair(:,:)
      integer,allocatable :: stab(:,:)

! dtdz - Air temperature vertical gradient (nx,ny)
! tair - Air temperature (nx,ny)
! stab - Stability class (nx,ny)

      real             :: dd25
      real             :: ff25
      real,allocatable :: u(:,:,:)
      real,allocatable :: v(:,:,:)

!MSK      real,allocatable :: w(:,:,:)
      double precision,allocatable :: w(:,:,:)

! dd25 - Wind direction measured in 25 meters
! ff25 - Wind speed ... measured in 25 meters
! u    - U-component of wind (along x-axis) (nx,ny,nz) 
! v    - V-component of wind (along y-axis) (nx,ny,nz)
! w    - W-component of wind (along z-axis) (nx,ny,nz)

      real,allocatable :: clou(:,:)
      real,allocatable :: hmix(:,:)
      real,allocatable :: prec(:,:)
      real,allocatable :: rhum(:,:)
!_LHS_08_Oct_2004_Start:
      real,allocatable :: ustr(:,:)
      real,allocatable :: shfl(:,:)
      real,allocatable :: lhfl(:,:)
      real,allocatable :: lanu(:,:)
!MSK      real,allocatable :: kz_h(:,:,:)
!MSK      real,allocatable :: kz_m(:,:,:)
!_LHS_08_Oct_2004_End.

      real,allocatable :: pres(:,:,:)
      real,allocatable :: ins_t(:,:,:)
      real,allocatable :: shum(:,:,:)
      real,allocatable :: rhum3D(:,:,:)

! clou - Cloud cover (0-1) (nx,ny)
! hmix - Mixing height (m) (nx,ny)
! prec - Precipitation (mm/h) (nx,ny)
! rhum - Relative humidity (0-1) (nx,ny)

!_LHS_08_Oct_2004_Start:
! ustr - Surface friction velocity.... from MM5 (m/s)  (nx,ny)
! shfl - Surface sensible heat flux... from MM5 (W/m2) (nx,ny) 
! lhfl - Surface latent heat flux..... from MM5 (W/m2) (nx,ny)
! lanu - Land-Use Categories ......... from MM5 ( - )  (nx,ny)
! kz_h - Vertical Heat Diffusivity.... from MM5 (m2/s) (nx,ny,nz)
! kz_m - Vertical Momentum Diffusivity from MM5 (m2/s) (nx,ny,nz)
!_LHS_08_Oct_2004_End.

      real,allocatable :: aero(:,:)
      real,allocatable :: d(:)
      real,allocatable :: sigv(:,:,:)
      real,allocatable :: sigw(:,:,:)

! aero - Aerodynamic resistance (nx,ny)
! d    - Horisontal diffusion coefficient (nz)
! sigv - Standard deviation of v-component of wind (nx,ny,nz)
! sigw - Standard deviation of w-component of wind (nx,ny,nz)

      real,allocatable :: dive(:,:,:)

!MK      real,allocatable :: dzdt(:,:,:)
      double precision,allocatable :: dzdt(:,:,:)

      real,allocatable :: tlgr(:,:,:)

! dive - Horizontal wind divergence (nx,ny,nz)
! dzdt - First momentum of vertical exchange process (nx,ny,nz)
! tlgr - Lagrangian time scale (nx,ny,nz)

      real,allocatable :: mobul(:,:)
      real,allocatable :: tstar(:,:)
      real,allocatable :: ustar(:,:)
!_LHS_15_Oct_2004_Start:
      real,allocatable :: wstar(:,:)
!_LHS_15_Oct_2004_End.

! mobul - Monin-Obukhov length (nx,ny)
! tstar - T* (nx,ny)
! ustar - Friction velocity (nx,ny)
!_LHS_15_Oct_2004_Start:
! wstar - Convective velocity scale (nx,ny)
!_LHS_15_Oct_2004_End.

      real,allocatable :: sigwp(:,:,:)
      real,allocatable :: tlgrp(:,:,:)

! sigwp - Standard deviation of w-component of wind for plumes (nx,ny,nz)
! tlgrp - Lagrangian time scale for plumes (nx,ny,nz)

      real,allocatable :: sigvmin(:)
      real,allocatable :: sigwmin(:)

! sigvmin - Minimum allowed value for sigma-v (nz)
! sigwmin - Minimum allowed value for sigma-w (nz)

      real zt_lower
      real zt_upper

! zt_lower = Delta temperature lower height
! zt_upper = Delta temperature upper height

!MSK start
      real,allocatable :: pot_t(:,:,:)
      real,allocatable :: pot_t_top(:,:,:)

!MM5
      real,allocatable :: taus_x(:,:)
      real,allocatable :: taus_y(:,:)
      real,allocatable :: mflx(:,:)
      real,allocatable :: nrad(:,:)

!TAPM
      real,allocatable :: tsrad(:,:)
      real,allocatable :: gpot(:,:,:)
      real,allocatable :: gtmp(:,:)
      real,allocatable :: ptstr(:,:)
      real,allocatable :: pvstr(:,:)
      real,allocatable :: wstr(:,:)

      real,allocatable :: wmixstar(:,:)
      real,allocatable :: tmixstar(:,:)

      real,allocatable :: K_h(:,:,:)

! *** wstar - Convective velocity (nx,ny)
! ... wmixstar - Convective velocity scale (nx,ny)
! ... tmixstar - Convective temperature scale (nx,ny)

      integer :: iturb  = 1             !  Default value = 1

! *** New option for vertical eddy diffusivity
! *** Related to NILU_METHOD in cdzdt_new.f90
      integer :: ivertdiff = 2

      real    :: standard_press          = 1013.0
      logical :: standard_press_applied  = .FALSE.
      real    :: default_rh              = 0.8
      logical :: default_rh_applied      = .FALSE.
      real    :: St_Bo_const             = 5.67E-8

      real :: A_prog_hmix         =   2.0
      real :: B_prog_hmix         = 100.0
      real :: C_prog_hmix         =   0.005

      real    :: HMIX_MIN         = 50.0
! *** HMIX_MIN :: Default minimum value of the mixing height.
 
      real    :: w_adiab          = 0.0069
! *** w_adiab  : Simplified wet adiabatic lapse rate; Highly variable; 0.0069 (at 0 C) and 0.0036 (at 30 C).

      real    :: dry_tay_pri      = 0.0
! *** dry_tay_pri : Taylor-Priestly constant applied for "dry" conditions; [0=dry,1=wet].
! ***               See Eq. (31) in vU&H,1985 and discussion thereafter:

! *** SELECTING the parameterisation methods for Hmix:

      integer :: NST_METHOD  = 2
! *** Stable and Neutral Conditions:
! *** NST_METHOD    = 1  ! Original MEPDIM method.
! *** NST_METHOD    = 2  ! Niewstadt method.

      integer :: NUST_METHOD = 4
! *** Unstable Conditions:
! *** NUST_METHOD   = 1  ! Original MEPDIM method (i.e. as Neutral).
! *** NUST_METHOD   = 2  ! Encroachment method (prognostic).
! *** NUST_METHOD   = 3  ! Simplified B-G method (prognostic).
! *** NUST_METHOD   = 4  ! Advanced B-G method (prognostic).

!MSK end

      contains


!MSK copied funtion OBUK into mod_mete from OBUK.for
!
!   ******************************************************************
!
      REAL FUNCTION OBUK(UST, TST, TM)

!MSK
      REAL UST,TST,TM,VONK
      VONK=0.41
!   -----------------------------------------------------------------
!
      IF (TST .EQ. 0.) THEN
         OBUK=-1E6
      ELSE
         OBUK=(TM*UST**2)/(TST*9.8*VONK)
      ENDIF
      RETURN
      END FUNCTION OBUK

!MSK copied funtion PSIH into mod_mete from PSIH.for
!
!   ******************************************************************
!
      double precision FUNCTION PSIH(ETA)
!MSK      REAL FUNCTION PSIH(ETA)


!   -----------------------------------------------------------------
!         STABILITY CORRECTION FUNCTION IN THE SURFACE LAYER
!         TEMPERATURE PROFILE
!
!         INPUT:
!                 ETA   : STABILITY PARAMETER Z/L
!         OUTPUT:
!                 PSIH : CORRECTION IN LOGARITHMIC TEMPERATURE PROFILE
!
!         THE PRESENT MODEL IS AN EMPIRICAL FIT BY HOLTSLAG AND
!         DE BRUIN(1987, ......................................)
!         OF DATA BY HICKS(1976, QUART. J. R. METEOR. SOC., 102,
!         535-551); SEE ALSO HOLTSLAG(1984, BLM, 29, 225-250)
!  ------------------------------------------------------------------

!MSK      REAL ETA
      double precision :: ETA

! Local variables

      double precision :: Y
      double precision :: expeta
      double precision, parameter :: YMIN = -87.3
      double precision, parameter :: YMAX = 88.7

! Y - Dummy variable

      IF (ETA .LT. 0.) THEN
         Y    = SQRT(1 - 16.*ETA)
         PSIH = 2*ALOG((1 + real(Y))/2)
      ELSE

! Calculate exp(-0.35*ETA)
         Y = -0.35*ETA
         if (Y .gt. YMIN .and. Y .lt. YMAX) then
            expeta = EXP(Y)
         elseif (Y .le. YMIN) then
            expeta = 0.
         else
            expeta = EXP(YMAX)
         endif

!MSK         PSIH = -0.7*ETA - (0.75*ETA - 10.72)*EXP(-0.35*ETA) - 10.72
!MSK     Below is the expression of Eq. (2.1.26) from COST 710 Book.
!MSK      Replaced EXP4(-0.35*ETA) by expeta       
!         PSIH = - ( (1.0 + 0.6667*ETA)**(1.5) + (0.667*ETA - 9.529)*EXP(-0.35*ETA) + 8.529)
         PSIH = - ( (1.0 + 0.6667*ETA)**(1.5) + (0.667*ETA - 9.529)*expeta + 8.529)

      ENDIF

      RETURN

! End of real function PSIH

      END FUNCTION PSIH

!MSK copied funtion PSIM into mod_mete from PSIM.for
!
!   ******************************************************************
!
      double precision FUNCTION PSIM(ETA)
!      REAL FUNCTION PSIM(ETA)

!  -------------------------------------------------------------
!         STABILITY CORRECTION FUNCTION IN THE SURFACE LAYER WIND
!         PROFILE
!
!         INPUT:
!                 ETA   : STABILITY PARAMETER Z/L
!         OUTPUT:
!                 PSIM : CORRECTION IN LOGARITHMIC WIND PROFILE
!
!         THE PRESENT MODEL IS AN EMPIRICAL FIT BY HOLTSLAG AND
!         DE BRUIN(1987, ......................................)
!         OF DATA BY HICKS(1976, QUART. J. R. METEOR. SOC., 102,
!         535-551); SEE ALSO HOLTSLAG(1984, BLM, 29, 225-250)
!
!                                    NILU 30-5-92 TROND BOEHLER
!   -----------------------------------------------------------

!MSK      REAL ETA
     double precision :: ETA

! Local variables

     real :: PID2
     double precision :: X
     double precision :: expeta
     double precision, parameter :: XMIN = -87.3
     double precision, parameter :: XMAX = 88.7

! X    - Dummy variable
! PID2 - Pi/2

      DATA PID2/1.5707963/

      IF (ETA .LT. 0.) THEN
         X    = SQRT(SQRT(1-16.*ETA))
         PSIM = ALOG((1 + real(X))**2*(1 + real(X)**2)/8) - 2.*ATAN(X) + PID2
      ELSE
!MSK taken ETA.gt.20.0 branch from MCWIND module_mc_wind_met
         if (ETA.gt.20.0) then
           PSIM = -0.7*ETA -10.72
         else
! Calculate exp(-0.35*ETA)
           X = -0.35*ETA
           if (X .gt. XMIN .and. X .lt. XMAX) then
              expeta = EXP(X)
           elseif (X .le. XMIN) then
              expeta = 0.
           else
             expeta = EXP(XMAX)
           endif          
!MSK      Below is the expression of Eq. (2.1.25) from the COST 710 Book:
!MSK      Replaced EXP4(-0.35*ETA) by expeta
!         PSIM = - (ETA    + (0.667 - 9.529)*EXP(-0.35*ETA) + 9.529)
           PSIM = - (ETA    + (0.667 - 9.529)*expeta + 9.529)
         endif
      ENDIF

      RETURN


! End of real function psim

      END FUNCTION PSIM

!MSK copied funtion TST_EBUDGET into mod_mete from tst_ebudget.for in BDE_EPISODE
!
!   ******************************************************************
!
! *********************************************************************

      REAL FUNCTION TST_EBUDGET(QST1,SIGMA,D_ADIAB,KAPPA,CP,RHO_AIR,   &
                               LAMBDA_E,S_SLOPE,TAY_PRI,AG_DAY,       &
                               AG_NIGHT,THETA_D,UST,Z0H,ZR,TM,QSTI)

! ---------------------------------------------------------------------
!       Function that determines T* as a function of u*, according to
!       the surface flux parameterisation by van Ulden and Holtslag.
!       (1985, JCAM, 24, 1196-1207).
!
!         ATTENTION: This routine can only be called after QSTI (Q*_i)
!                    has been computed by means of SUBROUTINE RADIAT.
!
!
!         Input:
!               SIGMA    : Stefan-Boltzmanns constant: 5.67E-8
!               D_ADIAB  : The dry adiabat: 0.01 K/m
!               KAPPA    : von Karman's constant
!               CP       : Isobaric specific heat [J/(kgK)]
!               RHO_AIR  : Density of the air
!               LAMBDA_E : Latent heat of water vaporization
!               S_SLOPE : Slope of the saturation enthalphy curve.
!               TAY_PRI  : Taylor-Priestly parameter.
!               AG_DAY   : Daytime Soil conduction parameter 
!                          (5 for grass; in vU&H,85)
!               AG_NIGHT : Nighttime Soil conduction parameter
!                          (5 for grass; in vU&H,85)
!               THETA_D  : "Constant set to 0.033 K in vU&H,85"
!               UST      : Friction velocity (m/s)
!               Z0H      : Surface roughness for heat (temperature)
!               ZR       : Height of the TM-temperature.
!               TM       : Ambient temperature (Degrees C)
!               QSTI     : Isothermal net radiation  (W/m2)
!
!         Output:
!               TST_EBUDGET ("TST"): Temperature scale (Degrees C)
!               QST1             : Humidity scale    (g/Kg)
!
!                                       NILU  25-09-92  Trond Bøhler
!          NILU: Additional comments included 25-10-03  L .H. Slørdal                             
! *********************************************************************
!    
! *** The reference height (should be above the height with the largest
!     temperature gradient, vU&H,1985), i.e. ZR = 50.0 meter.
!
! *** TAY_PRI: "Modified" Priestley-Taylor parameter. The value 
!              of 1 corresponds to wet grass in a moderate climate.
!              For "Prairie grass" conditions with rather dry 
!              vegetation a value of 0.5 is recommended, and for 
!              dry bare soil ALFA vanishes (vU&H,1985).
!  
! *** AG:      Empirical soil heat transfer coeff. equal to 5 W/(m2K) 
!              for a grass surface.
! *** THETA_D: Empirical temperature scale (Estimated to be 0.033 K)
!              See Eq. (31) in vU&H,1985 and discussion thereafter.
! 
! *** D1 =     (1.0/(2*0.4))*ln(50./Z0H) = 15. typic. for short grass.
!
! *** Z0H:     Roughness length for heat (or water).  Z0H = 0.03 cm in 
!              order to get D1 = 15.
! *** GAMMAD:  The dry-adiabatic lapse rate (0.01 K/m)
!
! *** SIGMA:   Stefan-Boltzmanns constant. (5.67E-08 W/m2K)
!
! *** THETA_D:   Empirical temperature scale (Estimated to be 0.033 K)
!                See Eq. (31) in vU&H,1985 and discussion thereafter.

        IMPLICIT NONE

! *** Global variables:
      REAL,INTENT(OUT) :: QST1
      REAL,INTENT(IN)  :: SIGMA,D_ADIAB,KAPPA,CP,RHO_AIR,LAMBDA_E,   &
                         S_SLOPE,TAY_PRI,AG_DAY,AG_NIGHT,THETA_D,     &
                         UST,Z0H,ZR,TM,QSTI

! *** Local variables:
      REAL :: S,THETAD,RD1,VST,D1,D2,D3,D4,TST,QMG,CH,CG,AG

! *** Content of function:
      S      = S_SLOPE
      THETAD = THETA_D

      IF (QSTI .LT. 0.) THEN
! ***  Night-time scheme:
       AG = AG_NIGHT
!
! ***  Redefining S when TAY_PRI is different from 1:
!_LHS  The redefinition of S and THETAD below should be
!_LHS  commented out if a wet ground surface is assumed
!_LHS  as default nighttime condition! 
       S      = ((S + 1.0)/((1-TAY_PRI)*S + 1.0)) - 1.0
       THETAD = TAY_PRI*THETAD
! ***
!      See Eq.(34) in vU&H,1985:
       RD1=SQRT(5.*9.81*ZR)
       VST=UST/RD1
!      See Eq.(35) in vU&H,1985:
       D1 = (1.0/(2.0*KAPPA))*ALOG(ZR/Z0H)
!	 D1 = 15.0                                 !Used for ORG_MEPDIM!!
!      See Eq.(36) in vU&H,1985:
       D2=0.5*(1+S)*RHO_AIR*CP*RD1/(4*SIGMA*TM**3+AG)
!      See Eq.(37) in vU&H,1985:
       D3=-QSTI/(4*SIGMA*TM**4+AG*TM) + D_ADIAB*ZR/TM
!      See Eq.(38) in vU&H,1985:
       D4=D2*THETAD*2.0/TM
!      See Eq.(33) in vU&H,1985:
       TST=TM*( SQRT( (D1*VST**2+D2*VST**3)**2 +D3*VST**2     &
                           +D4*VST**3 )                       &
                 -D1*VST**2-D2*VST**3 )
!      Calculating the sum of the Sensible Heat flux and the 
!      Latent heat flux, i.e. equal to  Q_net - G.
          QMG=QSTI                                             &
             +(4*SIGMA*TM**3+AG)*(2*TST*D1+(TST/VST)**2/TM-D_ADIAB*ZR)
!
      ELSE
! ***  Day-time scheme:
       AG = AG_DAY
!      See Eq.(23) in vU&H,1985: 
       CH=0.38*( ((1.-TAY_PRI)*S+1.)/(S+1.) )
!      See Eq.(27) in vU&H,1985: 
       CG=( AG/(4.*SIGMA*TM**3) )*CH
!      Calculating the sum of the Sensible Heat flux and the 
!      Latent heat flux, i.e. equal to  Q_net - G.
       QMG=(1.-CG)*QSTI/(1.+CH)
!      See Eq.(30) in vU&H,1985:
       TST= ( -((1.-TAY_PRI)*S+1.)*QMG/(S+1.) )/(RHO_AIR*CP*UST)    &
            + TAY_PRI*THETAD
      ENDIF

! *** Returning the THETA_star value as the FUNCTION value.
      TST_EBUDGET = TST

! *** Calculating the surface humidity scale, q_star which is
!     defined as:  q_star = (LAMBDA * E_0) / (RO * LAMBDA * UST)
      QST1=(-QMG-RHO_AIR*CP*UST*TST)/(RHO_AIR*LAMBDA_E*UST)

      RETURN

      END FUNCTION TST_EBUDGET


!MSK copied funtion TST_QNET into mod_mete from tst_qnet.for in BDE_EPISODE
!
!   ******************************************************************
! *********************************************************************

      REAL FUNCTION TST_QNET(QST1,SIGMA,D_ADIAB,KAPPA,CP,RHO_AIR,       &
                            LAMBDA_E,S_SLOPE,TAY_PRI,AG_DAY,AG_NIGHT,  &
                            THETA_D,UST,Z0H,ZR,TM,QNET)
! ---------------------------------------------------------------------
!       Function that determines T* as a function of u*, according to
!       the surface flux parameterisation by van Ulden and Holtslag.
!       (1985, JCAM, 24, 1196-1207).
!
!         ATTENTION: This routine can only be called after QSTI (Q*_i)
!                    has been computed by means of SUBROUTINE RADIAT.
!
!
!         Input:
!               SIGMA    : Stefan-Boltzmanns constant: 5.67E-8
!               D_ADIAB  : The dry adiabat: 0.01 K/m
!               KAPPA    : von Karman's constant
!               CP       : Isobaric specific heat [J/(kgK)]
!               RHO_AIR  : Density of the air
!               LAMBDA_E : Latent heat of water vaporization
!               S_SLOPE : Slope of the saturation enthalphy curve.
!               TAY_PRI  : Taylor-Priestly parameter.
!               AG_DAY   : Daytime Soil conduction parameter 
!                          (5 for grass; in vU&H,85)
!               AG_NIGHT : Nighttime Soil conduction parameter
!                          (5 for grass; in vU&H,85)
!               THETA_D  : "Constant set to 0.033 K in vU&H,85"
!               UST      : Friction velocity (m/s)
!               Z0H      : Surface roughness for heat (temperature)
!               ZR       : Height of the TM-temperature.
!               TM       : Ambient temperature (Degrees C)
!               QNET     : Measured net radiation  (W/m2)
!
!         Output:
!               TST2  : Temperature scale (Degrees C)
!               QST1  : Humidity scale    (g/Kg)
!
!                                       NILU  25-09-92  Trond Bøhler
!          NILU: Additional comments included 25-10-03  L .H. Slørdal                             
! *********************************************************************
!    
! *** The reference height (should be above the height with the largest
!     temperature gradient, vU&H,1985), i.e. ZR = 50.0 meter.
!
! *** TAY_PRI: "Modified" Priestley-Taylor parameter. The value 
!              of 1 corresponds to wet grass in a moderate climate.
!              For "Prairie grass" conditions with rather dry 
!              vegetation a value of 0.5 is recommended, and for 
!              dry bare soil ALFA vanishes (vU&H,1985).
!  
! *** AG:      Empirical soil heat transfer coeff. equal to 5 W/(m2K) 
!              for a grass surface.
! *** THETA_D: Empirical temperature scale (Estimated to be 0.033 K)
!              See Eq. (31) in vU&H,1985 and discussion thereafter.
! 
! *** D1 =     (1.0/(2*0.4))*ln(50./Z0H) = 15. typic. for short grass.
!
! *** Z0H:     Roughness length for heat (or water).  Z0H = 0.03 cm in 
!              order to get D1 = 15.
! *** GAMMAD:  The dry-adiabatic lapse rate (0.01 K/m)
!
! *** SIGMA:   Stefan-Boltzmanns constant. (5.67E-08 W/m2K)
!
! *** THETA_D:   Empirical temperature scale (Estimated to be 0.033 K)
!                See Eq. (31) in vU&H,1985 and discussion thereafter.

         IMPLICIT NONE

! *** Global variables:
      REAL,INTENT(OUT) :: QST1
      REAL,INTENT(IN)  :: SIGMA,D_ADIAB,KAPPA,CP,RHO_AIR,LAMBDA_E,    &
                         S_SLOPE,TAY_PRI,AG_DAY,AG_NIGHT,THETA_D,    &
                         UST,Z0H,ZR,TM,QNET

! *** Local variables:
      REAL :: S,THETAD,RD1,VST,D1,D2,D3,D4,TST,QMG,CH,CG,AG

! *** Content of function:
      S      = S_SLOPE
      THETAD = THETA_D

      IF (QNET .LT. 0.) THEN
! ***  Night-time scheme:
       AG = AG_NIGHT
! ***  Redefining S when TAY_PRI is different from 1:
!_LHS  The redefinition of S and THETAD below should be
!_LHS  commented out if a wet ground surface is assumed
!_LHS  as default nighttime condition!
!      See Eq. (A3) Slørdal, 2006: 
         S      = ((S + 1.0)/((1-TAY_PRI)*S + 1.0)) - 1.0
!      See Eq. (A4) Slørdal, 2006:
         THETAD = TAY_PRI*THETAD
! ***
!      See Eq.(34) in vU&H,1985:
         RD1 = SQRT(5.*9.81*ZR)
         VST = UST/RD1
!      See Eq.(35) in vU&H,1985:
         D1 = (1.0/(2.0*KAPPA))*ALOG(ZR/Z0H)
!	 D1 = 15.0                                 !Used for ORG_MEPDIM!!
!      See Eq.(36c) in Slørdal,2006:
         D2 = 0.5*(1+S)*RHO_AIR*CP*RD1/AG
!      See Eq.(37c) in Slørdal,2006:
         D3 = -QNET/(AG*TM) + D_ADIAB*ZR/TM
!      See Eq.(38c) in Slørdal,2006:
         D4 = D2*THETAD*2.0/TM
!      See Eq.(33) in vU&H,1985:
         TST = TM*( SQRT( (D1*VST**2+D2*VST**3)**2 +D3*VST**2   &
                        +D4*VST**3 )                           &
               -D1*VST**2-D2*VST**3 ) 
!      Calculating the sum of the Sensible Heat flux and the 
!      Latent heat flux, i.e. equal to  Q_net - G.
         QMG = QNET + AG*(2.0*TST*D1+(TST/VST)**2/TM - D_ADIAB*ZR)
!
      ELSE
! ***  Day-time scheme:
       AG = AG_DAY
! ***
!      See Eq.(23) in vU&H,1985: 
       CH = 0.38*( ((1.0 - TAY_PRI)*S + 1.0)/(S + 1.0) )
!      See Eq.(27) in vU&H,1985: 
       CG = ( AG/(4.*SIGMA*TM**3) )*CH
!      Calculating the sum of the Sensible Heat flux and the 
!      Latent heat flux, i.e. equal to  Q_net - G.
       QMG =(1.0-CG)*QNET
!      See Eq.(30b) in Slørdal,2006:
       TST = ( -((1.0-TAY_PRI)*S+1.0)*QMG/(S+1.0) )/(RHO_AIR*CP*UST)   &
           + TAY_PRI*THETAD
      ENDIF

! *** Returning the THETA_star value as the FUNCTION value.
      TST_QNET = TST

! *** Calculating the surface humidity scale, q_star which is
!     defined as:  q_star = (LAMBDA * E_0) / (RO * LAMBDA * UST)
      QST1 = (-QMG-RHO_AIR*CP*UST*TST)/(RHO_AIR*LAMBDA_E*UST)

      RETURN
      END FUNCTION TST_QNET
! *********************************************************************

! *********************************************************************
      REAL FUNCTION CWDIR(UU,VV)

! The function calculates the wind direction given the u- and v-
! components UU and VV of the wind.

! Scalar arguments

      REAL UU
      REAL VV

! UU,VV - u- and v-components of wind

! Local variables

      real :: DD, RATIO
      real, parameter :: pi  = 3.141592654
      real, parameter :: rad = pi/180.

! DD    - Wind direction
! RATIO - Ratio between v- and u-component of wind

! Calculate wind direction

      IF (UU .EQ. 0.0) THEN
          IF (VV .GT. 0.0) DD = 180.0
          IF (VV .EQ. 0.0) DD =   0.0
          IF (VV .LT. 0.0) DD = 360.0
      ENDIF

      IF (UU .NE. 0.0) THEN
          RATIO = ABS(VV/UU)
          DD    = ATAN(RATIO)/RAD
          IF (UU .GT. 0.0 .AND. VV .GE. 0.0) DD = 270.0 - DD
          IF (UU .GT. 0.0 .AND. VV .LT. 0.0) DD = 270.0 + DD
          IF (UU .LT. 0.0 .AND. VV .GE. 0.0) DD =  90.0 + DD
          IF (UU .LT. 0.0 .AND. VV .LT. 0.0) DD =  90.0 - DD
      ENDIF

      CWDIR = DD

      RETURN

! End of real function CWDIR

      end function cwdir

! *********************************************************************
! *********************************************************************
!
! WIND SPEED PROFILE FUNCTION START
! WIND PROFILE FUNCTIONS FROM S.E.WALKER'S WORM
!
! *********************************************************************

      real function uz_f(z,uz_min,zref,uref,ustar,lmo_inv)

! The subroutine calculates the wind speed at height z above ground

! Arguments

      real, intent(in) :: z
      real, intent(in) :: uz_min
      real, intent(in) :: zref
      real, intent(in) :: uref
      real, intent(in) :: ustar
      real, intent(in) :: lmo_inv
      real, parameter  :: kappa = 0.4
! z       - Actual height above ground (m)
! uz_min  - Minimum allowed wind speed value (m/s)
! zref    - Reference height above ground (m)
! uref    - Wind speed at reference height (m/s)
! ustar   - Monin-Obukhov friction velocity (m/s)
! lmo_inv - Inverse Monin-Obukhov length (1/m)

! Calculate the wind speed at height z above ground

      uz_f = uref + (ustar/kappa)*phi_m_i_f(zref,z,lmo_inv)

! Ensure wind speed is not below minimum allowed value

      uz_f = max(uz_f,uz_min)

      RETURN

      end function uz_f
! WIND SPEED PROFILE FUNCTION END
! *********************************************************************


! PHI_M_I FUNCTION START
      real function phi_m_i_f(za,zb,lmo_inv)

! The subroutine calculates the integral of the function phi_m between the two heights
! za and zb

! Arguments

      real, intent(in) :: za
      real, intent(in) :: zb
      real, intent(in) :: lmo_inv

! za      - Height above ground (m)
! zb      - Height above ground (m)
! lmo_inv - Inverse Monin-Obukhov length (1/m)

      real, parameter :: beta_m = 5.3

! Local variables

      real :: am
      real :: bm
      real :: cm
      real :: dm
      real :: lmo
      real :: xl
      real :: xu
      real :: zl
      real :: zu

! xl - Phi_m function value at zl
! xu - Phi_m function value at zu
! zl - Lower height above ground (m)
! zu - Upper height above ground (m)

! Initialize
      phi_m_i_f = 0.0

! Calculate ordered heights

      if (za <= zb) then
        zl = za
        zu = zb
      else
        zl = zb
        zu = za
      end if

      if (lmo_inv < 0) then

! Unstable case

        xl = phi_m_f(zl,lmo_inv)
        xu = phi_m_f(zu,lmo_inv)

        phi_m_i_f = log(zu/zl) - 2.0*log((1.0 + xu)/(1.0 + xl)) -     &
                  log((1.0 + xu**2)/(1.0 + xl**2)) +                  &
                  2.0*(atan(xu) - atan(xl)) 

      else if (lmo_inv > 0) then

! Stable case

        am = 1.0
        bm = beta_m
        cm = beta_m
        dm = 1.0

        lmo = 1.0/lmo_inv

        if (zl <= zu .and. zu <= lmo) then
          phi_m_i_f = am*log(zu/zl) + bm*((zu - zl)/lmo)
        else if (zl < lmo .and. lmo < zu) then
          phi_m_i_f = am*log(lmo/zl) + bm*(1.0 - zl/lmo) +    &
                     cm*log(zu/lmo) + dm*(zu/lmo - 1.0)
        else if (lmo <= zl .and. zl <= zu) then
          phi_m_i_f = cm*log(zu/zl) + dm*((zu - zl)/lmo)
        end if

      else

! Neutral case

        phi_m_i_f = log(zu/zl)

      end if

! Change sign of integral if za > zb

      if (za > zb) phi_m_i_f = -phi_m_i_f

      RETURN

      end function phi_m_i_f
! PHI_M_I FUNCTION END
! *********************************************************************

! PHI_M FUNCTION START
      real function phi_m_f(z,lmo_inv)

! The subroutine calculates the Monin-Obukhov universal stability function for wind speed
! at height z above ground

! Arguments

      real, intent(in) :: z
      real, intent(in) :: lmo_inv

! z       - Height above ground (m)
! lmo_inv - Inverse Monin-Obukhov length (1/m)

      real, parameter :: alfa_m = -19.0
      real, parameter :: beta_m = 5.3
      real, parameter :: mete_miss = -9900.0

! Local variables

      real :: zeta

! zeta - Stability parameter

! Negative height is not allowed

      if (z < 0.0) then
        phi_m_f = mete_miss
        return
      end if

      if (lmo_inv < 0) then

! Unstable case

        zeta = z*lmo_inv

        if (-2.0 <= zeta .and. zeta <= 0.0) then

! Use formulation from (Hoegstroem, 1996) (in validity range)

          phi_m_f = (1.0 + alfa_m*zeta)**(-0.25)

        else

! Use formulation from (Hoegstroem, 1996) (extended)

          phi_m_f = (1.0 + alfa_m*zeta)**(-0.25)

        end if

      else if (lmo_inv > 0) then

! Stable case

        zeta = z*lmo_inv

        if (0.0 <= zeta .and. zeta <= 0.5) then

! Use formulation from (Hoegstroem, 1996) (in validity range)

          phi_m_f = 1.0 + beta_m*zeta

        else if (0.0 <= zeta .and. zeta <= 1.0) then

! Use formulation from (Hoegstroem, 1996) (extended)

          phi_m_f = 1.0 + beta_m*zeta

        else

! Use formulation from (Holtslag, 1990) (very stable conditions)

          phi_m_f = beta_m + zeta
          !write(6,*) 'very stable',zeta
          !stop
          
          ! limitation z/L in [0,1]
          !phi_m_f = beta_m + min(zeta,1)

        end if

      else

! Neutral case

        phi_m_f = 1.0

      end if

      RETURN

      end function phi_m_f
! PHI_M FUNCTION END
! *********************************************************************

!MSK end


      subroutine FreeMeteMemory()

!DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'_FreeMeteMemory' :: FreeMeteMemory

      implicit none

      if (allocated(sdvwfv))  deallocate(sdvwfv)
      if (allocated(windfv))  deallocate(windfv)
      if (allocated(dtdz))    deallocate(dtdz)
      if (allocated(tair))    deallocate(tair)
      if (allocated(stab))    deallocate(stab)
      if (allocated(u))       deallocate(u)
      if (allocated(v))       deallocate(v)
      if (allocated(w))       deallocate(w)
      if (allocated(clou))    deallocate(clou)
      if (allocated(hmix))    deallocate(hmix)
      if (allocated(prec))    deallocate(prec)
      if (allocated(rhum))    deallocate(rhum)
!_LHS_08_Oct_2004_Start:
      if (allocated(ustr))    deallocate(ustr)
      if (allocated(shfl))    deallocate(shfl)
      if (allocated(lhfl))    deallocate(lhfl)
      if (allocated(lanu))    deallocate(lanu)
!MSK      if (allocated(kz_h))    deallocate(kz_h)
!MSK      if (allocated(kz_hfv))  deallocate(kz_hfv)
!MSK      if (allocated(kz_m))    deallocate(kz_m)
!MSK      if (allocated(kz_mfv))  deallocate(kz_mfv)

      if (allocated(pres))      deallocate(pres)
      if (allocated(ins_t))     deallocate(ins_t)
      if (allocated(shum))      deallocate(shum)
      if (allocated(rhum3D))    deallocate(rhum3D)

!_LHS_08_Oct_2004_End.
      if (allocated(aero))    deallocate(aero)
      if (allocated(d))       deallocate(d)
      if (allocated(sigv))    deallocate(sigv)
      if (allocated(sigw))    deallocate(sigw)
      if (allocated(dive))    deallocate(dive)
      if (allocated(dzdt))    deallocate(dzdt)
      if (allocated(tlgr))    deallocate(tlgr)
      if (allocated(mobul))   deallocate(mobul)
      if (allocated(tstar))   deallocate(tstar)
!MSK      if (allocated(ustar))   deallocate(ustar)
!_LHS_08_Oct_2004_Start:
      if (allocated(wstar))   deallocate(wstar)
!_LHS_08_Oct_2004_End.
      if (allocated(sigwp))   deallocate(sigwp)
      if (allocated(tlgrp))   deallocate(tlgrp)
      if (allocated(sigvmin)) deallocate(sigvmin)
      if (allocated(sigwmin)) deallocate(sigwmin)

!MSK start
      if (allocated(gpot))     deallocate(gpot)
      if (allocated(gtmp))     deallocate(gtmp)
      if (allocated(tsrad))    deallocate(tsrad)
      if (allocated(taus_x))   deallocate(taus_x)
      if (allocated(taus_y))   deallocate(taus_y)
      if (allocated(mflx))     deallocate(mflx)
      if (allocated(ptstr))    deallocate(ptstr)
      if (allocated(pvstr))    deallocate(pvstr)
      if (allocated(wstr))     deallocate(wstr)
      if (allocated(nrad))     deallocate(nrad)
      if (allocated(tsrad))    deallocate(tsrad)
      if (allocated(K_h))      deallocate(K_h)
      if (allocated(wmixstar)) deallocate(wmixstar)
      if (allocated(tmixstar)) deallocate(tmixstar)
      if (allocated(pot_t))    deallocate(pot_t)
!MSK end

      aerofn = ' '
      cloufn = ' '
      hmixfn = ' '
      precfn = ' '
      rhumfn = ' '
      sdvwfn = ' '
      tempfn = ' '
      windfn = ' '
!_LHS_08_Oct_2004_Start:
      ustrfn = ' '
      shflfn = ' '
      lhflfn = ' '
      lanufn = ' '
!MSK      kz_hfn = ' '
!MSK      kz_mfn = ' '
!MSK start
      wstrfn  = ' '
      pvstrfn = ' '
      ptstrfn = ' '
      gtmpfn  = ' '
      tsradfn = ' '
      gpotfn  = ' '
      pot_tfn = ' '
      ins_tfn = ' '
      mflxfn  = ' '
      tausfn  = ' '
!MSK end
!_LHS_08_Oct_2004_End.
      aeroun = 0
      clouun = 0
      hmixun = 0
      precun = 0
      rhumun = 0
      sdvwun = 0
      tempun = 0
      windun = 0
!_LHS_08_Oct_2004_Start:
      ustrun = 0
      shflun = 0
      lhflun = 0
      lanuun = 0
!MSK      kz_hun = 0
!MSK      kz_mun = 0
!MSK start
      wstrun  = 0
      pvstrun = 0
      ptstrun = 0
      gtmpun  = 0
      tsradun = 0
      gpotun  = 0
      pot_tun = 0
      ins_tun = 0
      mflxun  = 0
      tausun  = 0
!MSK end
!_LHS_08_Oct_2004_End.
      aerofe = .false.
      cloufe = .false.
      hmixfe = .false.
      precfe = .false.
      rhumfe = .false.
      sdvwfe = .false.
      tempfe = .false.
      windfe = .false.
!_LHS_08_Oct_2004_Start:
      ustrfe = .false.
      shflfe = .false.
      lhflfe = .false.
      lanufe = .false.
!MSK      kz_hfe = .false.
!MSK      kz_mfe = .false.
!MSK start
      wstrfe  = .false.
      pvstrfe = .false.
      ptstrfe = .false.
      tsradfe = .false.
      gtmpfe  = .false.
      gpotfe  = .false.
      pot_tfe = .false.
      ins_tfe = .false.
      mflxfe  = .false.
      tausfe  = .false.
!MSK end
!_LHS_08_Oct_2004_End.
      aerofv = 0.
      cloufv = 0.
      hmixfv = 0.
      precfv = 0.
      rhumfv = 0.
      tempfv = 0.
!_LHS_08_Oct_2004_Start:
      ustrfv = 0.
      shflfv = 0.
      lhflfv = 0.
      lanufv = 0.
!MSK start
      wstrfv  = 0.
      pvstrfv = 0.
      ptstrfv = 0.
      gtmpfv  = 0.
      tsradfv = 0.
      gpotfv  = 0.
      pot_tfv = 0.
      ins_tfv = 0.
      mflxfv  = 0.
      tausfv  = 0.
!MSK end
!_LHS_08_Oct_2004_End.
      aerofm = 0
      cloufm = 0
      hmixfm = 0
      precfm = 0
      rhumfm = 0
      sdvwfm = 0
      tempfm = 0
      windfm = 0
!_LHS_08_Oct_2004_Start:
      ustrfm = 0
      shflfm = 0
      lhflfm = 0
      lanufm = 0
!MSK      kz_hfm = 0
!MSK      kz_mfm = 0
!MSK start
      wstrfm  = 0
      pvstrfm = 0
      ptstrfm = 0
      gtmpfm  = 0
      tsradfm = 0
      gpotfm  = 0
      pot_tfm = 0
      ins_tfm = 0
      mflxfm  = 0
      tausfm  = 0
!MSK end
!_LHS_08_Oct_2004_End.
      dd25 = 0.
      ff25 = 0.
      zt_lower = 0.
      zt_upper = 0.

! End of subroutine FreeMeteMemory

      end subroutine

      end module mod_mete
