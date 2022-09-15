! <module_mc_wind_met.for - A component of the City-scale
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

      module module_mc_wind_met

!     This module contains the declarations of the observational data
!     and the variables needed to construct the initial (first guess)
!     wind field. At present it also contains the met. pre-processor
!     routines.

! Modifications:
!    29 Jan 2019  M. Karl: replaced TAB by spaces
!
!***********************************************************************



! *** Gravitational acceleration:
      real, parameter :: g_acc = 9.81

! *** von Karman's constant:
      real, parameter :: kappa = 0.41

! *** Stefan-Boltzmanns constant:
      real, parameter :: sb_const = 5.67E-8

! *** The specific isobaric heat for (dry?) air:
      real, parameter :: cp = 1005.

! *** Defining constant values for the adiabats:
      real, parameter :: d_adiab = 0.01    ! 0.0098
      real, parameter :: w_adiab = 0.0069  ! Highly variable,
!                                            0.0069 (at  0 C) and
!                                            0.0036 (at 30 C).

! *** Taylor-Priestly constant applied for "dry" conditions:
      real, parameter :: dry_tay_pri = 0.0

! *** Defining the "Empirical temp. scale" (Estimated to be 0.033 K)
! *** See Eq. (31) in vU&H,1985 and discussion thereafter:
      real, parameter :: theta_d = 0.033
!
!     Selecting the constant for the average night time potential
!     temperature profile:
      real, parameter :: a_prog_hmix      =   2.0
      real, parameter :: b_prog_hmix      = 100.0
      real, parameter :: c_prog_hmix      =   0.005

      real, parameter :: dthdzv_min = 1.0E-04

      real, parameter :: uzero = 0.0

! *** Variable declarations:


      integer  :: n_max,n_counter

      integer  :: n_surface_stat


      integer  :: n_profile_stat
      integer  :: n_geostrophic_stat

      integer  :: m1_applied_stations
      integer  :: n_valid_obs

      integer :: g_iday,g_imon,g_ihour,g_ltogmt

      integer  :: stab_class

      integer  :: i_surface_prof_method
      integer  :: iturb
      integer  :: nst_method,nust_method

      real     :: surf_obs_ref_height
      real     :: geostrophic_vertical_weight_pwr

      real     :: tm_kel

      real     :: pwrexp
      real     :: domain_station_wspeed,domain_hmix,domain_L
      real     :: ustarv,tstarv,mobulv,hmixv,hkinv,dthdzv
      real     :: tau_0v,hsenv,hlatv,qstarv,wstarv
      real     :: rho_air,lambda_e,s_slope,sat_press,q,q_sat,tay_pri

      real     :: lower_lim_L
      real     :: lower_lim_hmix
      real     :: minimum_obs_windspeed
      real     :: maximum_obs_windspeed

      logical  :: analytic
      logical  :: const_ref_height
      logical  :: SKIP_THIS_HOUR

      real     :: u0_test
      real     :: v0_test
      real     :: w0_test
      real     :: z0_constant

!     Applied 1D-arrays (vectors):
!
!     Surface observation information:

      character (len=256), allocatable :: surface_stat_name(:)

      integer, allocatable :: surface_stat_id(:)

      real, allocatable :: surface_stat_posx(:)
      real, allocatable :: surface_stat_posy(:)
      real, allocatable :: surface_stat_hgt_vel(:)
      real, allocatable :: surface_stat_hgt_tmp(:)
      real, allocatable :: surface_stat_hgt_dtup(:)
      real, allocatable :: surface_stat_hgt_dtlo(:)
      real, allocatable :: surface_stat_wspeed(:)
      real, allocatable :: surface_stat_wdir(:)
      real, allocatable :: surface_stat_tmp(:)
      real, allocatable :: surface_stat_dt(:)
      real, allocatable :: surface_stat_sr(:)
      real, allocatable :: surface_stat_press(:)
      real, allocatable :: surface_stat_rh(:)
      real, allocatable :: surface_stat_mm(:)
! MSK
      real, allocatable :: surface_stat_clc(:)

      real, allocatable :: surface_stat_z0(:)
      real, allocatable :: surface_stat_pwr(:)
      real, allocatable :: surface_stat_scale(:)
      real, allocatable :: app_surface_stat_scale(:)
      real, allocatable :: surface_stat_ffref(:)
!     Geostrophic upper air information:

      character (len=256), allocatable :: geostrophic_stat_name(:)

      integer, allocatable :: geostrophic_stat_id(:)
      integer, allocatable :: geostrophic_stat_used(:)
      real, allocatable    :: geostrophic_stat_posx(:)
      real, allocatable    :: geostrophic_stat_posy(:)
      real, allocatable    :: geostrophic_stat_ffref(:)
      real, allocatable    :: geostrophic_stat_ddref(:)
      real, allocatable    :: geostrophic_stat_pwr(:)
      real, allocatable    :: geostrophic_stat_scale(:)

!     Applied 2D-arrays:
      real, allocatable :: in_surfrough(:,:)
      real, allocatable :: surfrough(:,:)
      real, allocatable :: u0_surfrough(:,:)
      real, allocatable :: v0_surfrough(:,:)

      real, allocatable :: in_landuse(:,:)
      real, allocatable :: landuse(:,:)

      real, allocatable :: u0_surface_ref(:,:)
      real, allocatable :: v0_surface_ref(:,:)

      real, allocatable :: u0_geostrophic_ref(:,:)
      real, allocatable :: v0_geostrophic_ref(:,:)

!=======================================================================

      contains

! **********************************************************************
      SUBROUTINE TURB_PROFILE(ANALYTIC,G,D_ADIAB,KAPPA,ZU2,U2,ZU1,U1, &
                         ZT2,T2,ZT1,T1,LOWER_LIM_L,UST,TST,CL)
! **********************************************************************
!     INPUT:
!           ANALYTIC    : If ANALYTIC is .TRUE. the Holtslag (1984)
!                         analytic solution is applied with use of
!                         the constant value of 0.08 K for theta_star.
!                         If ANALYTIC is .FALSE. the given LOWER_LIM_L
!                         is applied as a lower boundary of the positive
!                         values of L. This value should be rather close
!                         to ZU2, since this will give Z/L close to 1,
!                         i.e. the border of the validity of the applied
!                         M-O theory.
!           G           : Acceleration due to gravity [9.81 m/s2]
!           D_ADIAB     : The dry_adiabatic temp. gradient [0.01 K/m]
!           KAPPA       : von Karmans constant [0.4]
!           U1, U2      : Wind speed at ZU1 and ZU2 [m/s]
!                         (e.g. U1 = 0. and U2 = U10 for ZU2 = 10.
!                          and ZU1 = Z0.)
!           ZU1, ZU2    : Observ. Heights for Wind (ZU2 > ZU1) [m]
!                         (e.g. Z0 and 10. respectively.)
!           T1,T2       : Temperature at ZT1 and ZT2    [in Kelvin!]
!           ZT1,ZT2     : Observ. heights for Temp. (ZT2 > ZT1) [m].
!           LOWER_LIM_L : User given lower boundary on the positive
!                         values of the Monin-Obukhov length [m].
!
!     OUTPUT:
!            UST        : Velocity scale                       [m/s]
!            TST        : Temperature scale                    [K]
!             CL        : Monin-Obukhov length                 [m]
!
!
!                                           11-09-2006  L. H. Slørdal
! ----------------------------------------------------------------------
! **********************************************************************
          IMPLICIT NONE

!     External functions:
!      REAL PSIH,PSIM,OBUK ! Not needed in the module

!     Global variables:
      LOGICAL,INTENT(IN) :: ANALYTIC
      REAL,INTENT(IN)    :: G,D_ADIAB,KAPPA,ZU2,U2,ZU1,U1,ZT2,T2,ZT1,T1, &
                       LOWER_LIM_L

         REAL,INTENT(OUT)   :: UST,TST,CL

!	Local variables:
        INTEGER :: N,NRIT
        REAL    :: ALFA,DTET,CLP,ZETA,CL0,TCL,CLN,DL,ADL

! ----------------------------------------------------------------------

      NRIT = 20
      ALFA = 5.0

      DTET = (T2-T1) + (D_ADIAB*(ZT2-ZT1))
      CL   = 36.0
      IF (DTET < 0.0) CL = -36.0

      DO 20 N=1,NRIT
        UST  = (U2-U1)*KAPPA/(ALOG(ZU2/ZU1)-PSIM(ZU2/CL)+PSIM(ZU1/CL))
        TST  = DTET   *KAPPA/(ALOG(ZT2/ZT1)-PSIH(ZT2/CL)+PSIH(ZT1/CL))
        CLP  = CL
        CL   = OBUK(KAPPA,G,UST,TST,T2)
        ZETA = ZU2/CL
        IF(ANALYTIC)THEN
           IF(ZETA > 1.0) THEN
! ***       Very stable, critical CL
            TST = 0.08
            CL0 = ALFA*ZU2/ALOG(ZU2/ZU1)
            CLN = KAPPA*U2*U2*T2/(2.0*G*TST*ALOG(ZU2/ZU1)*ALOG(ZU2/ZU1))
            TCL = 2.0*CL0
            IF(CLN >= TCL) THEN
              CL = (CLN-CL0) + SQRT(CLN*CLN - 2.0*CL0*CLN)
            ELSE
              CL = SQRT(0.5*CL0*CLN)
            ENDIF
! ****      Critical CL = CL0, No extrapolation below CL0
            IF(CL < CL0) CL = CL0
          ENDIF
        ELSE
          IF(CL > 0 .AND. CL < LOWER_LIM_L) CL = LOWER_LIM_L
        ENDIF
        DL  = (CL-CLP)/CLP
        ADL = ABS(DL)
        IF(ADL < 0.05) GO TO 25

  20  CONTINUE

  25  CONTINUE

      RETURN
      END SUBROUTINE TURB_PROFILE


! **********************************************************************

      SUBROUTINE TURB_EBUDGET(ANALYTIC,G,SIGMA,D_ADIAB,KAPPA,CP,RHO_AIR, &
                         LAMBDA_E,S_SLOPE,TAY_PRI,AG_DAY,AG_NIGHT, &
                         THETA_D,U1,U2,ZU1,ZU2,Z0H,ZR,TM,RNN,RKIN, &
                         RALB,IMON,IDAY,IHOUR,LTOGMT,CLON,CLAT, &
                         LOWER_LIM_L,UST,TST,QST,CL)

! ----------------------------------------------------------------------
!
!     SUBROUTINE TURB_EBUDGET computes FRICTION VELOCITY from wind
!     measurements at two heights and parameterized surface heat flux
!    (van Ulden and Holtslag, 1985).
!
!     The DYER AND HICKS RELATIONS ARE USED MODIFIED BYE
!     HOLTSLAG FOR VERY STABLE SITUATIONS (SEE WESSELS, 1984,
!     KNMI REPORT WR 84-6  SEE ALSO STABILITY FUNCTIONS
!     PSIM AND PSIH).
!
!     INPUT:
!           ANALYTIC    : If ANALYTIC is .TRUE. the Holtslag (1984)
!                         analytic solution is applied with use of
!                         the constant value of 0.08 K for theta_star.
!                         If ANALYTIC is .FALSE. the given LOWER_LIM_L
!                         is applied as a lower boundary of the positive
!                         values of L. This value should be rather close
!                         to ZU2, since this will give Z/L close to 1,
!                         i.e. the border of the validity of the applied
!                         M-O theory.
!            G          : Acceleration due to gravity (9.81 m/s2).
!            SIGMA      : Stefan-Boltzmanns constant (5.67E-8 Jm-2K-4)
!            D_ADIAB    : The dry_adiabatic temp. gradient (0.01 K/m).
!            KAPPA      : von Karmans constant (0.4).
!            CP         : Specific heat at constant pressure.
!            RHO_AIR    : Air density.
!            LAMBDA_E   : Heat of vaporization of liquid water.
!            S_SLOPE    : Slope of the sat. specific humidity with T.
!            TAY_PRI    : Taylor-Priestly constant [0=dry,1=wet].
!            AG_DAY     : Daytime Soil conduction parameter
!                         (5 for grass  in vU&H,85)
!            AG_NIGHT   : Nighttime Soil conduction parameter
!                         (5 for grass  in vU&H,85)
!            THETA_D    : Temperature scale (0.033 in vU&H,85).
!
!            U1, U2     : Wind speed at ZU1 and ZU2 [m/s]
!                         (e.g. U1 = 0. and U2 = U10 for ZU2 = 10.
!                          and ZU1 = Z0.)
!            ZU1, ZU2   : Observ. Heights for Wind (ZU2 > ZU1) [m]
!                         (e.g. Z0 and 10. respectively.)
!            Z0H        : Surface roughness for heat (temperature) [m]
!                         (e.g. should be approx: 0.01*z0.
!            ZR         : Height of the TM-temperature below. [m]
!            TM         : Ambient temp. (rough estimate is OK) [K]
!            RNN        : Cloud cover fraction  [0.- 1.]
!            RKIN       : Incoming shortwave radiation
!                         (-9900. if missing  parameterized by RADIAT)
!            RALB       : Surface albedo, Net SW: RKIN*(1-RALB)
!                         Surface albedo:
!                               SURFACE      ALBEDO(%)   RALB
!                       1  :   DARK SOIL       10         0.10
!                       2  :   FOREST          15         0.15
!                       3  :   GRASS,LUSH      20         0.20
!                       4  :   SAND            25         0.25
!                       5  :   ICE,OLD         30         0.30
!                       6  :   ICE,NEW         35         0.35
!                       7  :   SNOW,OLD        40         0.40
!                       8  :   SNOW,NORMAL     60         0.60
!                       9  :   SNOW,FRESH      80         0.80
!
!           IMON        : Month (Local Time)
!           IDAY        : Day (LocalTime)
!           IHOUR       : Hour (Local Time)
!           LTOGMT      : Hour beteen GMT and Local Time (-1 for Norway wintertime)
!           CLON        : Longitude for current location
!           CLAT        : Latitude for current position
!           LOWER_LIM_L : User given lower boundary on the positive
!                         values of the Monin-Obukhov length.
!
!      Local Variables:
!            SINPHI     : Sine of solar elevation (from SINSUN)
!            QSTI       : Isothermal net radiation(from RADIAT)
!
!     OUTPUT:
!            UST        : Velocity scale                       [m/s]
!            TST        : Temperature scale                    [K]
!            QST        : Humidity scale                       [g/kg]
!            CL         : Monin-Obukhov length                 [m]
!
!
!                                           11-09-2006  L. H. Slørdal
! ---------------------------------------------------------------------

       IMPLICIT NONE

!     External functions:
!      REAL PSIM,TST_EBUDGET,OBUK

!     Global variables:
      LOGICAL,INTENT(IN) :: ANALYTIC
      INTEGER,INTENT(IN) :: IDAY,IMON,IHOUR,LTOGMT
      REAL,INTENT(IN)    :: G,SIGMA,D_ADIAB,KAPPA,CP,RHO_AIR,LAMBDA_E, &
                       S_SLOPE,TAY_PRI,AG_DAY,AG_NIGHT,THETA_D, &
                       U1,U2,ZU1,ZU2,Z0H,ZR,TM,RNN,RKIN,RALB, &
                       CLON,CLAT,LOWER_LIM_L
      REAL,INTENT(OUT)   :: UST,TST,QST,CL

!	Local variables:
        INTEGER :: N,NRIT
        REAL    :: ALFA,SINPHI,QSTI,CLP,ZETA,CL0,TCL,CLN,DL,ADL

! ----------------------------------------------------------------------

      NRIT=20
      ALFA=5.

       IF(RKIN < 0.0)THEN
! ***   Calculation of solar elevation  Time input is given as LOCAL TIME.
! ***   LTOGMT is the number of hours between LOCAL TIME and GMT/ZULU.
! ***   Examples:  LTOGMT = -1  for Norway in wintertime.
! ***              LTOGMT = -2  for Norway in summertime.
! ***              LTOGMT = -4  for ABU DHABI summertime.
        CALL SINSUN(SINPHI,CLON,CLAT,IMON,IDAY,IHOUR,LTOGMT)
      ENDIF
!
! *** Initialise for Surface Flux Parameterisation
      CALL RADIAT(SIGMA,SINPHI,TM,RNN,RKIN,RALB,QSTI)

      CL=36.
      IF (QSTI .LT. 0.) CL=-36.

      DO 20 N=1,NRIT
        UST=(U2-U1)*KAPPA/( ALOG(ZU2/ZU1)-PSIM(ZU2/CL)+PSIM(ZU1/CL))
        TST=TST_EBUDGET(QST,SIGMA,D_ADIAB,KAPPA,CP,RHO_AIR,LAMBDA_E, &
                   S_SLOPE,TAY_PRI,AG_DAY,AG_NIGHT,THETA_D, &
                   UST,Z0H,ZR,TM,QSTI)
        CLP=CL
        CL=OBUK(KAPPA,G,UST,TST,TM)
        ZETA= ZU2/CL
        IF(ANALYTIC)THEN
          IF(ZETA .GT. 1.0) THEN
! ***       Very stable, critical CL
            TST=0.09*(1.-0.5*RNN*RNN)
            CL0=ALFA*ZU2/ALOG(ZU2/ZU1)
            CLN=KAPPA*U2*U2*TM/(2.*G*TST*ALOG(ZU2/ZU1)*ALOG(ZU2/ZU1))
            TCL=2.*CL0
            IF(CLN .GE. TCL) THEN
                CL=(CLN-CL0) + SQRT(CLN*CLN-2.*CL0*CLN)
            ELSE
                CL=SQRT(0.5*CL0*CLN)
            ENDIF
! ****      Critical CL=CL0, No extrapolation below CL0
            IF(CL .LT. CL0) CL=CL0
            ENDIF
         ELSE
          IF(CL .GT. 0 .AND. CL .LT. LOWER_LIM_L) CL = LOWER_LIM_L
        ENDIF
          DL=(CL-CLP)/CLP
          ADL=ABS(DL)
          IF(ADL.LT.0.05) GO TO 25
  20  CONTINUE

  25  CONTINUE

      RETURN
      END SUBROUTINE TURB_EBUDGET




! **********************************************************************
      REAL FUNCTION OBUK(KAPPA,G,UST,TST,TM)
! **********************************************************************
        IMPLICIT NONE

        REAL :: KAPPA,G,UST,TST,TM

      IF(TST == 0.0)THEN
        OBUK = -1.0E6
      ELSE
        OBUK = (TM*UST**2)/(TST*G*KAPPA)
      ENDIF
      RETURN
      END FUNCTION OBUK

! **********************************************************************
      REAL FUNCTION PSIM(ETA)
! **********************************************************************
!     Stability correction function for momentum, used in calculating
!     the surface layer wind profile
!
!     Input:    ETA   = Stability parameter (Z/L)
!     Output:   PSIM  = Stability function for momentum
!
!     The present model is an empirical fit proposed by
!     Holtslag and De Bruin (1987, J. Appl. Meteorol., 27, 689-704)
!     of data by Hicks (1976, Quart. J. R. Meteor. Soc., 102,535-551)

!     see also Holtslag(1984, BLM, 29, 225-250).
! **********************************************************************

        IMPLICIT NONE

!     Global variables:
      REAL,INTENT(IN) :: ETA

!     Local variables:
      REAL,PARAMETER :: PID2 = 1.5707963

      REAL :: X

      IF(ETA .LT. 0.0)THEN
        X    = SQRT(SQRT(1-16.0*ETA))
        PSIM = ALOG((1+X)**2*(1+X**2)/8) -2.*ATAN(X) +PID2
      ELSE
        IF(ETA .GT. 20.0)THEN
!_LHS     PSIM = -5.0*ETA
!_LHS     PSIM = -17.0*(1.0 - EXP(-0.29*ETA))
          PSIM = -0.7*ETA -10.72
        ELSE
!_LHS     PSIM = -5.0*ETA                      ! Dyer (1974)
!_LHS     PSIM = -17.0*(1.0 - EXP(-0.29*ETA))  ! Eq. (54) vU&H,1985.
!MSK          PSIM=-0.7*ETA -(0.75*ETA-10.72)*EXP(-0.35*ETA) - 10.72
!MSK      Below is the expression of Eq. (2.1.25) from the COST 710 Book:
         PSIM = - (ETA    + (0.667 - 9.529)*EXP(-0.35*ETA) + 9.529)
        ENDIF
      ENDIF

      RETURN
      END FUNCTION PSIM

! **********************************************************************
      REAL FUNCTION PSIH(ETA)
! **********************************************************************
!     Stability correction function for heat, used in calculating
!     the surface layer temperature profile
!
!     Input:    ETA   = Stability parameter (Z/L)
!     Output:   PSIH  = Stability function for heat
!
!     The present model is an empirical fit proposed by
!     Holtslag and De Bruin (1988, J. Appl. Meteorol., 27, 689-704)
!     of data by Hicks (1976, Quart. J. R. Meteor. Soc., 102,535-551)

!     see also Holtslag(1984, BLM, 29, 225-250)
! **********************************************************************

       IMPLICIT NONE

!     Global variables:
      REAL,INTENT(IN) :: ETA

!     Local variables:
      REAL :: Y

      IF(ETA .LT. 0.0)THEN
        Y    = SQRT(1.0 - 16.0*ETA)
        PSIH = 2.0*ALOG((1 + Y)/2)
      ELSE
        IF(ETA .GT. 20.0)THEN
          PSIH = -0.7*ETA - 10.72
        ELSE
!MSK          PSIH = -0.7*ETA -(0.75*ETA - 10.72)*EXP(-0.35*ETA) - 10.72
!MSK Below is the expression of Eq. (2.1.26) from COST 710 Book.
              PSIH = - ( (1.0 + 0.6667*ETA)**(1.5) +       &
                         (0.667*ETA - 9.529)*EXP(-0.35*ETA) + 8.529)
        ENDIF
      ENDIF

      RETURN
      END FUNCTION PSIH


! *********************************************************************

      REAL FUNCTION TST_EBUDGET(QST1,SIGMA,D_ADIAB,KAPPA,CP,RHO_AIR, &
                           LAMBDA_E,S_SLOPE,TAY_PRI,AG_DAY, &
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
!                          (5 for grass  in vU&H,85)
!               AG_NIGHT : Nighttime Soil conduction parameter
!                          (5 for grass  in vU&H,85)
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
      REAL,INTENT(IN)  :: SIGMA,D_ADIAB,KAPPA,CP,RHO_AIR,LAMBDA_E, &
                     S_SLOPE,TAY_PRI,AG_DAY,AG_NIGHT,THETA_D, &
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
!      See Eq. (A3) Slørdal, 2006: 
       S      = ((S + 1.0)/((1-TAY_PRI)*S + 1.0)) - 1.0
!      See Eq. (A4) Slørdal, 2006:
       THETAD = TAY_PRI*THETAD
! ***
!      See Eq.(34) in vU&H,1985:
       RD1=SQRT(5.*9.81*ZR)
       VST=UST/RD1
!      See Eq.(35) in vU&H,1985:
       D1 = (1.0/(2.0*KAPPA))*ALOG(ZR/Z0H)
!       D1 = 15.0                                 !Used for ORG_MEPDIM!!
!      See Eq.(36) in vU&H,1985:
       D2=0.5*(1+S)*RHO_AIR*CP*RD1/(4*SIGMA*TM**3+AG)
!      See Eq.(37) in vU&H,1985:
       D3=-QSTI/(4*SIGMA*TM**4+AG*TM) + D_ADIAB*ZR/TM
!      See Eq.(38) in vU&H,1985:
       D4=D2*THETAD*2.0/TM
!      See Eq.(33) in vU&H,1985:
       TST=TM*( SQRT( (D1*VST**2+D2*VST**3)**2 +D3*VST**2 &
                    +D4*VST**3 ) &
          -D1*VST**2-D2*VST**3 )
!      Calculating the sum of the Sensible Heat flux and the
!      Latent heat flux, i.e. equal to  Q_net - G.
       QMG=QSTI &
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
         TST= ( -((1.-TAY_PRI)*S+1.)*QMG/(S+1.) )/(RHO_AIR*CP*UST)  &
      + TAY_PRI*THETAD
      ENDIF

! *** Returning the THETA_star value as the FUNCTION value.
      TST_EBUDGET = TST

! *** Calculating the surface humidity scale, q_star which is
!     defined as:  q_star = (LAMBDA * E_0) / (RO * LAMBDA * UST)
      QST1=(-QMG-RHO_AIR*CP*UST*TST)/(RHO_AIR*LAMBDA_E*UST)

      RETURN

      END FUNCTION TST_EBUDGET


! *********************************************************************
!
      SUBROUTINE SINSUN(SINPHI,CLON,CLAT,IMON,IDAY,IHOUR,LTOGMT)
! ---------------------------------------------------------------------
!
!       Subroutine SINSUN determines the sine of the solar elevation
!       (Appendix B: Holtslag and van Ulden, 1983, JCAM, 22, 517-529)
!
!          Input:
!              CLON      : Longitude (Degrees, EAST is positive)
!              CLAT      : Latitude  (Degrees, NORTH is positive)
!              IMON      : Month     (1-12, Local Time)
!              IDAY      : Day       (1-31, Local Time)
!              IHOUR     : Hour      (0-24, Local Time)
!              LTOGMT    : Number of hour between GMT and Local Time
!                          LTOGMT = -1 for Norway wintertime.
!                          LTOGMT = -2 for Norway summertime.
!
!          Output:
!              SINPHI    : Sine of solar elevation
!
! ---------------------------------------------------------------------

         IMPLICIT NONE
! *** Global variables:
        INTEGER,INTENT(IN) :: IMON,IDAY,IHOUR,LTOGMT
        REAL,INTENT(IN)    :: CLON,CLAT
        REAL,INTENT(OUT)   :: SINPHI
! *** Local variables
      INTEGER,PARAMETER :: IMONTH(12) &
              = (/0,31,59,90,120,151,181,212,243,273,304,334/)
!      DATA IMONTH /0,31,59,90,120,151,181,212,243,273,304,334 /
      REAL,PARAMETER :: PI    = 3.141592654
      REAL,PARAMETER :: PI180 = 57.29577951

      INTEGER :: JMON,JDAY,JHOUR
      REAL    :: D,RLON,RLAT,TERM,SL,SINDEL,COSDEL,H

! **********************************************************************
! *** Convert to GMT. Assumes that IHOUR = [1,24] !!!!


      IF(IHOUR == 0)THEN
        PRINT *,'SINSUN: IHOUR is zero, should be 1 - 24'
        PRINT *,'SINSUN: PROGRAM TERMINATES'
        STOP
        ENDIF

        JMON = IMON
        JDAY = IDAY
        JHOUR = IHOUR + LTOGMT
        IF(JHOUR < 1)THEN
        JHOUR = 24 + JHOUR
!       Reduce JDAY by 1:
        JDAY = IDAY -1
        IF(JDAY == 0)THEN
!         Reduce the JMON by 1:
          JMON = JMON - 1
          IF(JMON == 0) JMON = 12
        ENDIF
        ELSEIF(JHOUR > 24)THEN
        JHOUR = JHOUR - 24
!       Increase JDAY by 1:
        JDAY = IDAY + 1
!       Since the formula for calculatng the solar height only
!       apply the expression: D=IMONTH(JMON) + JDAY  we only
!       need to be concerned with the last day of the year.
        JMON = IMON
        IF(JDAY == 32 .AND. JMON == 12)THEN
          JMON = 1
          JDAY = 1
          ENDIF
        ENDIF
!
! TEST:
!      JMON  = IMON
!      JDAY  = IDAY
!      JHOUR = IHOUR
!
! **********************************************************************
!
! *** Calculate Day-Number from Eq.(B1) H&vU,1983:
      D=IMONTH(JMON) + JDAY

! *** Converting longitude and latitude from degrees to radians:
      RLON=CLON/PI180
      RLAT=CLAT/PI180

! *** Calculating solar longitude (SL) from Eq.(B2) H&vU,1983:
      TERM=0.033*SIN(0.0175*D)
      SL=4.871+0.0175*D+TERM

! *** The sine of the solar declination follows from Eq.(B3) H&vU,1983:
      SINDEL=0.398*SIN(SL)
      COSDEL=SQRT(1-SINDEL**2)

! *** The hour angle (H) is given by Eq.(B4) H&vU,1983:
!     NOTE: In Eq.(B4) (H&vU,1983) this is: H = -RLON + ...., but this
!           is because the formula is assuming western longitude.
!     NOTE: IHOUR should be the "universal time in hours", i.e GMT.
      H=RLON+0.043*SIN(2*SL)-TERM+0.262*JHOUR-PI

! *** The sine of the solar elevation is given by Eq.(B5) H&vU,1983:
      SINPHI=SINDEL*SIN(RLAT)+COSDEL*COS(RLAT)*COS(H)

      RETURN
      END SUBROUTINE SINSUN

! *********************************************************************

      SUBROUTINE RADIAT(SIGMA,SINPHI,TM,RNN,RKIN,RALB,QSTI)
! ---------------------------------------------------------------------
!         Radiation scheme according to van Ulden and Holtslag
!         (1985,JCAM, 24, 1196-1207).
!
!         Input:
!             SIGMA  : Stefan Boltzmanns constant
!             SINPHI : Sine of the solar elevation
!             RNN    : Cloud cover fraction (0-1)
!             RKIN   : Incoming short wave radiation (W/M2)
!                      (MISSING : -9900. or 0.0)
!             RALB   : Surface Albedo [0.0 - 1.0(total reflection)]
!
!                      SURFACE      ALBEDO(%)   RALB
!               1  :   DARK SOIL       10       0.10
!               2  :   FOREST          15       0.15
!               3  :   GRASS,LUSH      20       0.20
!               4  :   SAND            25       0.25
!               5  :   ICE,OLD         30       0.30
!               6  :   ICE,NEW         35       0.35
!               7  :   SNOW,OLD        40       0.40
!               8  :   SNOW,NORMAL     60       0.60
!               9  :   SNOW,FRESH      80       0.80
!
!         Output:
!             QSTI   : Isothermal net radiation, Q*_i. (W/M2)

!    ------------------------------------------------------------------

       IMPLICIT NONE

! *** Input variables:
      REAL,INTENT(IN)  :: SIGMA,SINPHI,TM,RNN,RKIN,RALB

! *** Output variables
      REAL,INTENT(OUT) :: QSTI

! *** Local variables:
      REAL :: RKST,RLSTI

! *** The values of the constants below are identical with the ones
! *** given in vU&H, 1985.
      REAL,PARAMETER :: A1    = 990.0
      REAL,PARAMETER :: A2    = -30.0
      REAL,PARAMETER :: B1    =   0.75
      REAL,PARAMETER :: B2    =   3.4
      REAL,PARAMETER :: C1    =   9.35E-6
      REAL,PARAMETER :: C2    =  60.0
      REAL,PARAMETER :: SPHI0 =   0.03

!     Selection of albedo (input parameter)

       IF(RKIN >= 0.0) THEN
! ***   If measured values of the incoming short wave rad. exists:
          RKST = RKIN*(1.0 - RALB)
      ELSE
!       For PHI < 1.7 degrees : No short wave radiation (vU&H, 1985).
        IF (SINPHI .LT. SPHI0) THEN
          RKST = 0.0
        ELSE
! ***     The net shortwave radiation, see Eq. (17) in vU&H, 1985:
          RKST = (A1*SINPHI + A2)*(1.0 - B1*RNN**B2)*(1.0 - RALB)
        ENDIF
      ENDIF

! *** Isothermal longwave radiation, L*_i, see Eq.(21) in vU&H,1985:
      RLSTI = -SIGMA*TM**4*(1.0 - C1*TM**2) + C2*RNN

! *** Isothermal net radiation, Q*_i, see Eq.(28) in vU&H,1985:
      QSTI = RKST + RLSTI

      RETURN

      END SUBROUTINE RADIAT


! **********************************************************************
      SUBROUTINE WS_REF_HEIGHT(Z0,ZU,U,UST,CL,ZREF,FF_ZREF)
! **********************************************************************
!
!     The subroutine calculates windspeed (in m/s) at the ZREF height.
!
! **********************************************************************

       IMPLICIT NONE

!	Global variables:
!     Input:

      REAL,INTENT(IN) :: Z0,ZU,U,UST,CL,ZREF

!     Z0    - Surface roughness
!     ZU    - Height of wind observation
!     U     - Wind speed at height ZU
!     UST   - Friction velocity
!     CL    - Monin-Obukhov length
!     ZREF  - Height where wind speed is to be calculated.


!     Output:
      REAL,INTENT(OUT) :: FF_ZREF

!     FF_ZREF  - Wind speed value at the Z_REF height.


!     External function:
!     REAL :: PSIM         ! Declaration not needed here.
!     PSIM - A REAL FUNCTION calculating the stability correction
!            function for the vertical wind profile

!     Local variables:
      REAL :: H,PH,P2

!     H    - Height value
!     PH   - Psi-expression at height ZREF
!     P2   - Psi-expression at height ZU

! ----------------------------------------------------------------------

!     Calculate windspeed

      H = ZREF

      IF (U < 0) THEN
        FF_ZREF = -9900.0
        ELSE
        IF (0.6*H .GE. Z0) THEN

!         Above surface roughness layer

          PH = ALOG( H/Z0) - PSIM( H/CL) + PSIM(Z0/CL)
          P2 = ALOG(ZU/Z0) - PSIM(ZU/CL) + PSIM(Z0/CL)

          FF_ZREF = U*PH/P2

        ELSE

!         Close to surface roughness layer, use friction velocity
          FF_ZREF = UST

        END IF
        END IF

      RETURN
      END SUBROUTINE WS_REF_HEIGHT

!***********************************************************************
      REAL FUNCTION CWDIR(UU,VV)

! *** This function calculates the wind direction given the u- and v-
! *** components "UU" and "VV" of the wind.
!
!***********************************************************************
      IMPLICIT NONE

! *** Global declarations:
      REAL,INTENT(IN) :: UU
      REAL,INTENT(IN) :: VV

! *** "UU","VV" - The u- and v-components of the wind vector.

! *** Local declarations:
      REAL :: DD
      REAL :: RATIO

      REAL,PARAMETER :: PI  = 3.141592654
      REAL,PARAMETER :: RAD = pi/180.0


! *** "DD"    - The wind direction
! *** "RATIO" - The ratio between the v- and u-component of the wind.
! *** "PI"    - The constant pi.
! *** "RAD"   - The conversion from degrees to radians.

! *** Calculate the wind direction:
! ***
! ***    0 degrees means wind from the north blowing southards.
! ***   90 degrees means wind from the east  blowing westwards.

      IF(UU == 0.0)THEN
        IF(VV >  0.0) DD = 180.0
        IF(VV == 0.0) DD =   0.0
        IF(VV <  0.0) DD = 360.0
      ENDIF

      IF(UU /= 0.0)THEN
        RATIO = ABS(VV/UU)
        DD    = ATAN(RATIO)/RAD
        IF(UU > 0.0 .AND. VV >= 0.0) DD = 270.0 - DD
        IF(UU > 0.0 .AND. VV <  0.0) DD = 270.0 + DD
        IF(UU < 0.0 .AND. VV >= 0.0) DD =  90.0 + DD
        IF(UU < 0.0 .AND. VV <  0.0) DD =  90.0 - DD
      ENDIF

      CWDIR = DD

      RETURN
      END FUNCTION CWDIR

!***********************************************************************

      SUBROUTINE ALLOWED_OBS_WIND_DATA(DIR_SC,WS_OBS,WD_OBS, &
                                  MIN_WS,MAX_WS, &
                                  SCALING,APP_SCALING)

!       Negative values of observed wind speeds or wind directions are
!       considered undefined, i.e., equal to -9900. Then this obs. is
!       not taken into account when constructing the initial first guess
!       field.
!
!       Wind direction scaling factor is given in DIR_SC
!       If:
!          DIR_SC = 1.0   =>   Wind-direction given in degrees.
!          DIR_SC = 10.0  =>   Wind-direction given in deca-grades.
!
!       At the moment DIR_SC is specified according to the site-name,
!       i.e.  DIR_SC = 10.0  if (domain_name(1:4) == 'Oslo')
!       and   DIR_SC = 1.0  for all other domain_name.
!
!       Positive wind speeds less than MIN_WS is set equal to MIN_WS.
!       Positive wind speeds larger than MAX_WS is also set undefined.
!
!       The values of MIN_WS and MAX_WS is defined after analyzing the
!       available observations.
!
!       SCALING: This variable is a user specified scaling factor for
!                each observation station. If SCALING = 1.0 then the
!                observation is fully included in the construction of
!                the initial first guess field. If SCALING = 0.0 then
!                this observation station is not applied in the making
!                of the initial first guess field.
!
!       APP_SCALING: This is the scaling that is actually applied.
!
!***********************************************************************

!     We require the use of IMPLICIT NONE:

      IMPLICIT NONE

!     Global declarations:
      REAL    :: DIR_SC,WS_OBS,WD_OBS,MIN_WS,MAX_WS,SCALING,APP_SCALING

!     Local declarations:
      REAL :: REDUCED_WEIGHT

      REDUCED_WEIGHT = 0.5

      IF(WS_OBS < -9000.0 .OR. WD_OBS < -9000.0)THEN
        WS_OBS      = -9900.0
        WD_OBS      = -9900.0
        APP_SCALING = 0.0
      ELSE

          IF(DIR_SC > 9.999 .AND. DIR_SC < 10.001)THEN
!         NILU_DB data for Oslo and met.no data for Fornebu.
          IF(WD_OBS > 1999.9)THEN
!_LHS_Jan2008_Start:
!_TEST            WS_OBS      = -9900.0                  !_TEST
!_TEST            WD_OBS      = -9900.0                  !_TEST
!_TEST            APP_SCALING = 0.0                      !_TEST
            WD_OBS      = (WD_OBS - 2000.0)*DIR_SC       !_NEW
            APP_SCALING = 1.0                            !_NEW
          ELSEIF(WD_OBS > 999.9)THEN
            WD_OBS      = (WD_OBS - 1000.0)*DIR_SC
!_TEST            APP_SCALING = REDUCED_WEIGHT           !_TEST
            APP_SCALING = 1.0                            !NEW
!_LHS_Jan2008_End.
          ELSEIF(WD_OBS > 37.5 .AND. WD_OBS < 38.5)THEN
!           Wind direction given by 38 (Wøyfle?) is skipped.
            WS_OBS      = -9900.0
            WD_OBS      = -9900.0
            APP_SCALING = 0.0
          ELSEIF(WD_OBS > 36.5 .AND. WD_OBS < 37.5)THEN
!           Wind direction given by 37 (Wøyfle?) is for "Calm".
!           The wind speed is then zero and wind direction
!           undertermined.
            WD_OBS      = 1.0*DIR_SC
            APP_SCALING = REDUCED_WEIGHT
          ELSEIF(WS_OBS == 0.0 .AND. WD_OBS == 0.0)THEN
!           Specific treatment of Fornebu.
            WS_OBS      = -9900.0
            WD_OBS      = -9900.0
            APP_SCALING = 0.0
          ELSE
!           The observation data should now be OK:
            WD_OBS = WD_OBS*DIR_SC
            APP_SCALING = SCALING
          ENDIF
        ELSEIF(DIR_SC > 0.999 .AND. DIR_SC < 1.001)THEN
!         The observation data should now be OK:
          APP_SCALING = SCALING
        ENDIF

          IF(WS_OBS >= 0.0 .AND. WS_OBS < MIN_WS)THEN
            WS_OBS = MIN_WS
          APP_SCALING = REDUCED_WEIGHT
          END IF
        IF(WS_OBS > MAX_WS)THEN
          WS_OBS = MAX_WS
          APP_SCALING = REDUCED_WEIGHT
        END IF

      END IF

        RETURN
        END SUBROUTINE ALLOWED_OBS_WIND_DATA

! ======================================================================
      SUBROUTINE MIXHT_NEW(NST_METHOD,NUST_METHOD,G,KAPPA,FC,TM, &
                      UST,CL,HKIN,DTHDZ,HMIX_MIN,HMIX)
! ======================================================================
!     Subroutine for mixing height calculations.
!
!     Input:
!
!     NST_METHOD  : Integer selecting the method applied for calculating
!                   the neutral and stabel (mechanical) PBL height
!     NUST_METHOD : Integer selecting the method applied for calculating
!                   the unstable (convective) PBL height (Mixing height)
!     G           : Acceleration due to gravity (~ 9.8 m/s2)
!     KAPPA       : The von Karman constant (~0.4)
!     FC          : The Coriolis parameter (1/s)
!     TM          : The ambient temperature (in Kelvin)
!     UST         : The friction velocity (m/s)
!     CL          : The Monin-Obukhov length (m)
!     HKIN        : The kinematic turbulent heat flux (mK/s)
!     DTHDZ       : The vertical gradient of the pot. temp. just above
!                   the mixing height (K/m)
!     HMIX_MIN    : Specified minimum value, ex. 1.0 m
!
!     Output:
!
!     HMIX        : The mixing height for the present hour (m)
!                  (NOTE: HMIX should enter this routine with its value
!                         from the prceeding hour.)
! ----------------------------------------------------------------------
      IMPLICIT NONE

! *** Declaration of global input variables:
      INTEGER,INTENT(IN) :: NST_METHOD,NUST_METHOD
      REAL,INTENT(IN)    :: G,KAPPA,FC,TM,UST,CL,HKIN,DTHDZ,HMIX_MIN

! *** Declaration of the global output variables:
      REAL,INTENT(INOUT)   :: HMIX

! *** Local variables:
      INTEGER :: NMAX,n,ICONT
      REAL    :: ACL,CLCRIT,DT,F1,F2,PROJ,nevner
      REAL    :: A,B,C,C1,C2,C3,C4,C5,C6,C7
! ----------------------------------------------------------------------

!        print * , 'NST_METHOD  = ', NST_METHOD
!        print * , 'NUST_METHOD = ', NUST_METHOD
!        print * , 'HMIX        = ', HMIX
!        print * , 'G           = ', G
!        print * , 'KAPPA       = ', KAPPA
!        print * , 'FC          = ', FC
!        print * , 'TM          = ', TM
!        print * , 'UST         = ', UST
!        print * , 'CL          = ', CL
!        print * , 'HKIN        = ', HKIN
!        print * , 'DTHDZ       = ', DTHDZ
!        print * , 'HMIX_MIN    = ', HMIX_MIN
!        print * , ' '

! *** The minimum setting of HMIX is input:
        IF(HMIX < HMIX_MIN) HMIX = HMIX_MIN
!
! *** The minimum setting of DTHDZ is done in SUBROUTINE POT_TMPGR
!	IF(DTHDZ .LT. 1.0E-04) DTHDZ  = 1.0E-04

!      print *, ' 1) Press 1 and enter to continue: '
!      read *, ICONT
!      if (ICONT == 0) STOP

      ACL = ABS(CL)

! *** NOTE: Critical L for choiche of neutral conditions (Taken
! ***       from MEPDIM!):
!     CLCRIT = 3906.25*UST
!
! *** A more stringent demand would be: |u*/(fL)| < 4 (van Ulden and
!                                                      Holtslag, 1985)
      CLCRIT = UST / (4.0 * ABS(FC) )
!
! ----------------------------------------------------------------------
         IF(NST_METHOD == 1 .AND. NUST_METHOD == 1)THEN

! ***    Original MEPDIM:

! ***    Calculations of mixing heigth
         IF(CL < 0.0 .OR. ACL > CLCRIT) THEN
! ***       Unstable or neutral conditions:
            HMIX = 0.25*UST/FC
         ELSE
! ***       Stable conditions:
            HMIX = 0.4*SQRT(UST*CL/FC)
         ENDIF
! ***    End of original MEPDIM.

      ELSEIF(NST_METHOD >= 1 .AND. NUST_METHOD > 1)THEN
! ----------------------------------------------------------------------
! ***    Below either the Original MEPDIM method or the diagnostic
! ***    expression of Nieuwstadt (1981) are applied for stable and
! ***    neutral conditions. Different prognostic formulas are
! ***    applied for unstable conditions: either
! ***         1) The Encroachment method, or
! ***         2) A simplified Batchvarova and Gryning (1991) method,
! ***      or 3) An advanced Batchvarova and Gryning (1991) method.
!
            IF(ACL > CLCRIT) THEN
!           Neutral conditions:
            IF(NST_METHOD == 1)THEN
!              Original MEPDIM method applied for neutral conditions:
               HMIX = 0.25*UST/FC
            ELSEIF(NST_METHOD == 2)THEN
!              Nieuwstadt (1981) for stable and neutral conditions,
!              see also Seibert et al., (2000).
                 HMIX = SQRT(1.0 +((2.28 * UST)/(FC*ACL)))
                 HMIX = ACL * ( -1.0 + HMIX )/3.8
            ELSE
                PRINT *, ' Wrong NST_METHOD input values.'
                PRINT *, ' PROGRAM TERMINATES'
                STOP
            ENDIF
          print *,'hmix1',nst_method,nust_method,FC,UST,ACL,HMIX

            ELSEIF(CL > 0.0 .AND. CL <= CLCRIT)THEN
!           Stable conditions:
            IF(NST_METHOD == 1)THEN
!              Original MEPDIM method applied for stable conditions:
               HMIX = 0.4*SQRT(UST*CL/FC)
            ELSEIF(NST_METHOD == 2)THEN
!              Nieuwstadt (1981) for stable and neutral conditions,
!              see also Seibert et al., (2000).
                 HMIX = SQRT(1.0 +((2.28 * UST)/(FC*ACL)))
                 HMIX = ACL * ( -1.0 + HMIX )/3.8
            ELSE
               PRINT *, ' Wrong NST_METHOD input values.'
               PRINT *, ' PROGRAM TERMINATES'
               STOP
            ENDIF
          print *,'hmix2',nst_method,nust_method,FC,UST,ACL,HMIX

          ELSE
!           UNSTABLE conditions:
!
!           Encroachment method:          A = B = C = 0.0
!           Simple Batchvarova-Gryning:   A = 0.2
            B = 2.5
            C = 0.0
!           Advanced Batchvarova-Gryning: A = 0.2
            B = 2.5
            C = 8.0
!
            IF(NUST_METHOD == 2)THEN
!             Encroachment method:
                A = 0.0
                B = 0.0
                C = 0.0
            ELSEIF(NUST_METHOD == 3)THEN
!             Simple B-G method:
                A = 0.2
                B = 2.5
                C = 0.0
             ELSEIF(NUST_METHOD == 4)THEN
!             Advanced B-G method:
                A = 0.2
                B = 2.5
                C = 8.0
             ELSE

               PRINT *, ' Wrong NUST_METHOD input values.'
               PRINT *, ' PROGRAM TERMINATES'
               STOP

            ENDIF


!           Nieuwstadt (1981), Seibert et al., (2000):
!
!           Applying Heun's method (2. order Runge Kutta) to solve the
!           prognostic encroachment equation. The encrochment equation
!           is also second order in h: h**2 = Const. * t, so 1 iteration
!           is enough!
!
!           ------------------------------------------------------------
!
!           Deciding on the time-step:
!
!           DT = 3600 => NMAX =  1
!           DT = 360  => NMAX = 10
!           DT = 180  => NMAX = 20
!           DT =  90  => NMAX = 40

              DT   = 360.0
              NMAX = 10
              C1 = (1.0 + 2*A)*HKIN/DTHDZ

              nevner = DTHDZ * G
!           print *, ' nevner = ', nevner
!           print *, ''
!           print *, ' 2) Press 1 and enter to continue: '
!           read *, ICONT
!           if (ICONT == 0) STOP
!
!            C2 = 2.0*B*UST*UST*UST*TM/(DTHDZ*G)

             C2 = 2.0*B*UST*UST*UST*TM/nevner

            DO n=1,NMAX

              IF(HMIX < HMIX_MIN) HMIX = HMIX_MIN

!             ----------------------------------------------------------
!             Start..  Encroachment method:
!
!             F1 = HKIN/(DTHDZ * HMIX)
!             PROJ = HMIX + (DT*F1)
!             IF(PROJ .LT. 0.001)THEN
!              PROJ = 0.001
!               PRINT *, 'WARNING: PROJ IS LESS THAN 0.001!'
!             ENDIF
!             F2 = HKIN/(DTHDZ * PROJ  )
!             HMIX = HMIX + (0.5*DT*(F1+F2))
!
!             Slutt..  Encroachment method:
!             ----------------------------------------------------------
!
!             Start..  Simple Batchvarova and Gryning, 1991:
!
!             F1 = (C1 / HMIX) + (C2 / (HMIX* HMIX))
!             PROJ = HMIX + (DT*F1)
!
!             F2 = (C1 / PROJ)   + (C2 / (PROJ  * PROJ  ))
!
!             HMIX = HMIX + (0.5*DT*(F1+F2))
!
!               Slutt..  Simple Batchvarova and Gryning, 1991:
!             ----------------------------------------------------------
!
!               Start..  Advanced Batchvarova and Gryning, 1991:
!
              C3 = (HKIN/DTHDZ)
              C4 = ((1.0 + (2.0*A))*HMIX) - (2.0*B*KAPPA*CL)
              C5 = C*UST*UST*TM

              C6 = ( (1+A)*HMIX - B*KAPPA*CL )*DTHDZ*G
              C7 = HMIX * HMIX

              nevner = (C7/C4) + (C5/C6)
              if (nevner < 1.0E-15) then
                print *, ' nevner = ', nevner
                print *, ''
                print *, ' 3) Press 1 and enter to continue: '
                read *, ICONT
                if (ICONT == 0) STOP
              endif

!              F1 = C3 / (  (C7/C4) + (C5/C6) )

              F1 = C3 / nevner

              PROJ = HMIX + (DT*F1)

              C4 = ((1.0 + (2.0*A))*PROJ) - (2.0*B*KAPPA*CL)

              C6 = ( (1+A)*PROJ - B*KAPPA*CL )*DTHDZ*G
              C7 = PROJ * PROJ

              nevner = (C7/C4) + (C5/C6)
              if (nevner < 1.0E-15) then
                print *, ' nevner = ', nevner
                print *, ''
                print *, ' 4) Press 1 and enter to continue: '
                read *, ICONT
                if (ICONT == 0) STOP
              endif

!             F2 = C3 / (  (C7/C4) + (C5/C6) )

              F2 = C3 / nevner

              HMIX = HMIX + (0.5*DT*(F1+F2))

!            Slutt..  Advanced Batchvarova and Gryning, 1991.
!
!             We have calculated HMIX for the: previous hour + n * DT
!             and when n=NMAX we are at: previous hour + 3600.


           PRINT *, ' Unstable iteration HMIX = ',HMIX

          ENDDO
          print *,'hmix3',nst_method,nust_method,FC,UST,ACL,HMIX

       ENDIF

! ----------------------------------------------------------------------

      ELSE

           PRINT *, ' Wrong NST_METHOD or NUST_METHOD input values.'
           PRINT *, ' PROGRAM TERMINATES'
           STOP

      ENDIF

      RETURN
      END SUBROUTINE MIXHT_NEW

! ======================================================================
      SUBROUTINE MET_VARIABLES(TM,PRESS,RH,DRY_TAY_PRI,RHO_AIR,LAMBDA_E, &
                          S_SLOPE,SAT_PRESS,Q,Q_SAT,TAY_PRI)
! ======================================================================
!
!	Input:
!             TM                 : Ambient temperature (K).
!             PRESS              : Ambient pressure (Pa).
!             RH                 : Relative humidity (0-100).
!             DRY_TAY_PRI        : User specified Taylor-Priestly const.
!
!     Output:
!             RHO_AIR            : Air density (kg/m3)
!             LAMBDA_E           :
!             S_SLOPE            :
!             SAT_PRESS          :
!             Q                  :
!             Q_SAT              :
!             TAY_PRI            :
! ----------------------------------------------------------------------
      IMPLICIT NONE

!	Global variables:
      REAL,INTENT(IN)  :: TM,PRESS,RH,DRY_TAY_PRI
      REAL,INTENT(OUT) :: RHO_AIR,LAMBDA_E,S_SLOPE,SAT_PRESS,Q,Q_SAT, &
                     TAY_PRI

!     Local variables:

! ----------------------------------------------------------------------
! *** Air density when the ambient pressure is given in hPa
! *** and the ambient temperature in Kelvin.
      RHO_AIR = PRESS/(2.8704*TM)
!
! *** The latent heat of vaporization of water:
! *** Eliassen & Pedersen (1977) page 52:
      LAMBDA_E = (2501. - 2.37*(TM-273.15))*1000.
!
! *** Slope (S) of the saturation enthalphy curve, Eq.(32) vU&H,1985:
!     According to vU&H,1985 this is quite well approximated for
!     270 < TM < 310 (i.e. -3 < T < +40 in Celsius).
      S_SLOPE =EXP(0.055*(TM-279.))
!
! *** Garratt (1992) page 284: "Bolton's (1980) fit to Wexler (1976):
      SAT_PRESS = 6.112*EXP(17.67*(TM-273.15)/(TM-29.65))
!
! *** Eliassen & Pedersen (1977) page 61: Approximately we have:
      Q_SAT = 0.622 * SAT_PRESS/PRESS
      Q     = RH * Q_SAT
!
! *** The Taylor-Priestly constant
      IF(RH > 95.0)THEN
        TAY_PRI = 1.0                           !Used for ORG_MEPDIM!!
      ELSE
        TAY_PRI = DRY_TAY_PRI
      ENDIF

      RETURN
      END SUBROUTINE MET_VARIABLES

! **********************************************************************
      SUBROUTINE FLUX_1(CP,RHO_AIR,LAMBDA_E,UST,TST,QST, &
                   TAU,HKIN,HSEN,HLAT)
! **********************************************************************
!     Subroutine to calculate heat fluxes
!
!     INPUT:
!            CP       : Specific heat at constant pressure.
!            RHO_AIR  : Air density.
!            LAMBDA_E : Heat of vaporization of liquid water.
!            UST      : FRICTION VELOCITY
!            TST      : TEMPERATURE SCALE
!            QST      : HUMIDITY SCALE
!
!     OUTPUT:
!            TAU      : SURFACE STRESS               (N/M2)
!            HKIN     : Kinematic Sensible Heat Flux (    )
!            HSEN     : SENSIBLE HEAT FLUX           (W/M2)
!            HLAT     : LATENT HEAT OF EVAPORATION   (W/M2)
!
!   ----------------------------------------------------------------
        IMPLICIT NONE

!	Global variables:
!	Input:
        REAL,INTENT(IN)  :: CP,RHO_AIR,LAMBDA_E,UST,TST,QST

!	Output:
        REAL,INTENT(OUT) :: TAU,HKIN,HSEN,HLAT

! *** Content of Routine:
      TAU  =  RHO_AIR*UST**2
      HSEN = -RHO_AIR*CP*TST*UST
      HKIN =  HSEN/(RHO_AIR*CP)
      HLAT = -RHO_AIR*LAMBDA_E*QST*UST

      RETURN
      END SUBROUTINE FLUX_1

! **********************************************************************
      SUBROUTINE FLUX_2(G,TM,HMIX,HSEN,WST)
! **********************************************************************
!
! *** Subroutine to calculate heat fluxes
!
!    INPUT:   TM   :  TEMPERATURE
!             HSEN :  SENSIBLE HEAT FLUX   (W/m2)
!             HMIX :  MIXING HEIGHT           (m)
!
!    OUTPUT:  WST  : CHARACTERISTIC VELOCITY SCALE
!
!   ----------------------------------------------------------------
      IMPLICIT NONE

! *** Global variables:
! *** Input:
      REAL,INTENT(IN)  :: G,TM,HMIX,HSEN

! *** Output:
      REAL,INTENT(OUT) :: WST

! *** Local variables:
      REAL :: RWST,POT

      RWST = G*HSEN*HMIX/TM
      POT  = 1.0/3.0

      IF(RWST .LT. 0) THEN
        WST=0.00
      ELSE
        WST=RWST**POT
      ENDIF

      RETURN
      END SUBROUTINE FLUX_2

! **********************************************************************
      SUBROUTINE POT_TMPGR(A,B,C,Z,DTHDZ_MIN,DTHDZ)
! **********************************************************************
!
! *** Subroutine for calculations of the slightly stable vertical
! *** potential temperature gradient just above the HMIX height.
!
! *** THETA = THETA_0 + A*(1 - exp(-z/B)) + C * z
!
! *** d(THETA)/dz = (A/B) * exp(-z/B)  +  C
!
!     INPUT:  A        = f.eks. 2 Kelvin
!             B         = f.eks. 100 m
!             C         = f.eks. 0.003 K/m = Minimum value
!                         (The gradient must be larger than zero.)
!             Z         = The height (m) at which DTHDZ is computed
!             DTHDZ_MIN = User specified minimum value
!
!     OUTPUT: DTHDZ(Z=HMIX_+) : The vertical gradient of the
!                               pot. temperature just above HMIX
! ----------------------------------------------------------------------
        IMPLICIT NONE

!	Global variables:
        REAL,INTENT(IN)  :: A,B,C,Z,DTHDZ_MIN
        REAL,INTENT(OUT) :: DTHDZ

        DTHDZ = C + ( (A/B)* EXP(- (Z/B)) )

      IF(DTHDZ < DTHDZ_MIN) DTHDZ  = DTHDZ_MIN

      PRINT *, ' The computed value of DTHDZ = ', DTHDZ

      RETURN
      END SUBROUTINE POT_TMPGR

! **********************************************************************
! *** MET MODULE INTERFACE ROUTINES


! **********************************************************************
      SUBROUTINE SendMetSurfroughData(iv_im,iv_jm,rav2_in_surfrough)
! **********************************************************************

!DEC$ ATTRIBUTES DLLEXPORT,STDCALL,ALIAS:'SendMetSurfroughData' :: SendMetSurfroughData

      IMPLICIT NONE

      integer :: iv_im
      integer :: iv_jm

      real    :: rav2_in_surfrough(iv_im,iv_jm)

! *** Local variables:

! *** Initializing the variables declared above in: "module_mc_wind_met"

      if (.not. allocated(in_surfrough))  &
           allocate(in_surfrough(iv_im,iv_jm))

      in_surfrough = rav2_in_surfrough

      END SUBROUTINE SendMetSurfroughData


! **********************************************************************
      SUBROUTINE SendMetObsStaticData(iv_n_surface_stat, &
                                 iav1_surface_stat_id, &
                                 rav1_surface_stat_posx, &
                                 rav1_surface_stat_posy, &
                                 rav1_surface_stat_hgt_vel, &
                                 rv_surface_stat_hgt_tmp, &
                                 rv_surface_stat_hgt_dtup, &
                                 rv_surface_stat_hgt_dtlo, &
                                 iv_n_geostrophic_stat, &
                                 iav1_geostrophic_stat_id, &
                                 rav1_geostrophic_stat_posx, &
                                 rav1_geostrophic_stat_posy)
! **********************************************************************

!DEC$ ATTRIBUTES DLLEXPORT,STDCALL,ALIAS:'SendMetObsStaticData' :: SendMetObsStaticData

      IMPLICIT NONE

! *** Surface station information:

      integer :: iv_n_surface_stat

      integer :: iav1_surface_stat_id(iv_n_surface_stat)

      real    :: rav1_surface_stat_posx(iv_n_surface_stat)
      real    :: rav1_surface_stat_posy(iv_n_surface_stat)
      real    :: rav1_surface_stat_hgt_vel(iv_n_surface_stat)
      real    :: rv_surface_stat_hgt_tmp
      real    :: rv_surface_stat_hgt_dtup
      real    :: rv_surface_stat_hgt_dtlo

! *** The following variables are presently not sent from AirQUIS:                  
!      real    :: rav1_surface_stat_hgt_tmp(iv_n_surface_stat)
!      real    :: rav1_surface_stat_hgt_dtup(iv_n_surface_stat)
!      real    :: rav1_surface_stat_hgt_dtlo(iv_n_surface_stat)

!      real    :: rav1_surface_stat_z0(iv_n_surface_stat)
!      real    :: rav1_surface_stat_pwr(iv_n_surface_stat)
!      real    :: rav1_surface_stat_scale(iv_n_surface_stat)


! *** Variable description:
!
! *** iv_n_surface_stat                            : Number of meteorological surface stations.        
! *** iav1_surface_stat_id(iv_n_surface_stat)      : Station ID number.
! *** rav1_surface_stat_posx(iv_n_surface_stat)    : X-position (in km) from origo.
! *** rav1_surface_stat_posy(iv_n_surface_stat)    : Y-position (in km) from origo.
! *** rav1_surface_stat_hgt_vel(iv_n_surface_stat) : Height (in m) of velocity measurement.
! *** rv_surface_stat_hgt_tmp                      : Height (in m) of temp. measurement.
! *** rv_surface_stat_hgt_dtup                     : Upper height (in m) of DT-measurement.
! *** rv_surface_stat_hgt_dtlo                     : Lower height (in m) of DT-measurement.

! *** Upper air geostrophic wind:
! *** One of the surface stations is selected as the one from which we
! *** estimate the upper air geostrophic wind velocity.

      integer :: iv_n_geostrophic_stat   ! = 1
      integer :: iav1_geostrophic_stat_id(iv_n_geostrophic_stat)

      real    :: rav1_geostrophic_stat_posx(iv_n_geostrophic_stat)
      real    :: rav1_geostrophic_stat_posy(iv_n_geostrophic_stat)

! *** The following variables are presently not sent from AirQUIS:
!      real    :: rav1_geostrophic_stat_pwr(iv_n_geostrophic_stat)
!      real    :: rav1_geostrophic_stat_scale(iv_n_geostrophic_stat)
!      real    :: rv_geostrophic_vertical_weight_pwr = 0.5

! *** Local variables:
      integer n

! *** Initializing the variables declared above in: "module_mc_wind_met"

      n_surface_stat = iv_n_surface_stat

      if (.not. allocated(surface_stat_id)) &
           allocate(surface_stat_id(n_surface_stat))
      if (.not. allocated(surface_stat_posx)) &
           allocate(surface_stat_posx(n_surface_stat))
      if (.not. allocated(surface_stat_posy)) &
           allocate(surface_stat_posy(n_surface_stat))
      if (.not. allocated(surface_stat_hgt_vel)) &
           allocate(surface_stat_hgt_vel(n_surface_stat))

      if (.not. allocated(surface_stat_hgt_tmp)) &
           allocate(surface_stat_hgt_tmp(n_surface_stat))
      if (.not. allocated(surface_stat_hgt_dtup)) &
           allocate(surface_stat_hgt_dtup(n_surface_stat))
      if (.not. allocated(surface_stat_hgt_dtlo)) &
           allocate(surface_stat_hgt_dtlo(n_surface_stat))
      if (.not. allocated(surface_stat_z0)) &
           allocate(surface_stat_z0(n_surface_stat))
      if (.not. allocated(surface_stat_pwr)) &
           allocate(surface_stat_pwr(n_surface_stat))
      if (.not. allocated(surface_stat_scale)) &
           allocate(surface_stat_scale(n_surface_stat))

! *** Start: Inserted during debugging 11 June 2008.
      if (.not. allocated(app_surface_stat_scale)) &
           allocate(app_surface_stat_scale(n_surface_stat))
      if (.not. allocated(surface_stat_ffref)) &
           allocate(surface_stat_ffref(n_surface_stat))
! *** End:   Inserted during debugging 11 June 2008.

      do n = 1,n_surface_stat

        surface_stat_id(n)       = iav1_surface_stat_id(n)
        surface_stat_posx(n)     = rav1_surface_stat_posx(n)
        surface_stat_posy(n)     = rav1_surface_stat_posy(n)
        surface_stat_hgt_vel(n)  = rav1_surface_stat_hgt_vel(n)

! ***   At the moment these are treated as scalars in AirQUIS:

        surface_stat_hgt_tmp(n)  = rv_surface_stat_hgt_tmp
        surface_stat_hgt_dtup(n) = rv_surface_stat_hgt_dtup
        surface_stat_hgt_dtlo(n) = rv_surface_stat_hgt_dtlo

! ***   At the moment the station arrays below are hardcoded here
! ***   and in Subroutine get_observations:

!_LHS_January2011_Start:
!        surface_stat_z0(n)        = 1.0   ! Z0 applied for the station.
                                           ! This should be user specified.
        surface_stat_z0(n)        = 0.4   ! Z0 applied for the station.
                                          ! This should be user specified.
!_LHS_January2011_End.

        surface_stat_pwr(n)       = 2.0   ! Inverse square interpolation
                                          ! applied in the for the first
                                          ! guess wind field.

        surface_stat_scale(n)     = 1.0   ! If zero this station is not
! ***                                     ! included in the first guess.
        app_surface_stat_scale(n) =  &
             surface_stat_scale(n)   ! If there are something wrong
                                          ! with one of the surface
                                          ! stations, it is excluded

                                          ! see also: get_observations.

      enddo

      n_geostrophic_stat = iv_n_geostrophic_stat

      if (.not. allocated(geostrophic_stat_id)) &
           allocate(geostrophic_stat_id(n_geostrophic_stat))
      if (.not. allocated(geostrophic_stat_posx)) &
           allocate(geostrophic_stat_posx(n_geostrophic_stat))
      if (.not. allocated(geostrophic_stat_posy)) &
           allocate(geostrophic_stat_posy(n_geostrophic_stat))

      if (.not. allocated(geostrophic_stat_pwr)) &
           allocate(geostrophic_stat_pwr(n_geostrophic_stat))
      if (.not. allocated(geostrophic_stat_scale)) &
           allocate(geostrophic_stat_scale(n_geostrophic_stat))

      do n = 1,n_geostrophic_stat
        geostrophic_stat_id(n)      = iav1_geostrophic_stat_id(n)
        geostrophic_stat_posx(n)    = rav1_geostrophic_stat_posx(n)
        geostrophic_stat_posy(n)    = rav1_geostrophic_stat_posy(n)
        geostrophic_stat_pwr(n)   = 2.0
        geostrophic_stat_scale(n) = 1.0
      enddo

       geostrophic_vertical_weight_pwr = 0.5
      ! geostrophic_vertical_weight_pwr = 0.25  ! For more shallow surface influence.
      ! geostrophic_vertical_weight_pwr = 0.2
      ! geostrophic_vertical_weight_pwr = 0.1

      END SUBROUTINE SendMetObsStaticData

! ****************************************************************************************
      SUBROUTINE SendMetObsDynamicData(iv_n_surface_stat, &
                                 rav1_surface_stat_wspeed, &
                                 rav1_surface_stat_wdir, &
                                 rv_surface_stat_tmp, &
                                 rv_surface_stat_dt, &
                                 rv_surface_stat_press, &
                                 rv_surface_stat_rh, &
                                 rv_surface_stat_mm, &
                                 rv_surface_stat_clou)

!DEC$ ATTRIBUTES DLLEXPORT,STDCALL,ALIAS:'SendMetObsDynamicData' :: SendMetObsDynamicData

!-----------------------------------------------------------------------------------------

      IMPLICIT NONE

! *** Meteorological parameters measured at the various stations:
      integer :: iv_n_surface_stat

      real    :: rav1_surface_stat_wspeed(iv_n_surface_stat)
      real    :: rav1_surface_stat_wdir(iv_n_surface_stat)

      real    :: rv_surface_stat_tmp
      real    :: rv_surface_stat_dt
      real    :: rv_surface_stat_press
      real    :: rv_surface_stat_rh
      real    :: rv_surface_stat_mm
      real    :: rv_surface_stat_clou

!     real    :: rav1_surface_stat_tmp(iv_n_surface_stat)
!     real    :: rav1_surface_stat_dt(iv_n_surface_stat)
!     real    :: rav1_surface_stat_press(iv_n_surface_stat)
!     real    :: rav1_surface_stat_rh(iv_n_surface_stat)
!     real    :: rav1_surface_stat_mm(iv_n_surface_stat)
!     real    :: rav1_surface_stat_sr(iv_n_surface_stat)


! *** Local variables:
      integer n

! *** Initializing the variables declared above in: "module_mc_wind_met"

      n_surface_stat = iv_n_surface_stat

      if (.not. allocated(surface_stat_wspeed)) &
           allocate(surface_stat_wspeed(n_surface_stat))
      if (.not. allocated(surface_stat_wdir)) &
           allocate(surface_stat_wdir(n_surface_stat))
      if (.not. allocated(surface_stat_tmp)) &
           allocate(surface_stat_tmp(n_surface_stat))
      if (.not. allocated(surface_stat_dt)) &
           allocate(surface_stat_dt(n_surface_stat))
      if (.not. allocated(surface_stat_press)) &
           allocate(surface_stat_press(n_surface_stat))
      if (.not. allocated(surface_stat_rh)) &
           allocate(surface_stat_rh(n_surface_stat))
      if (.not. allocated(surface_stat_mm)) &
           allocate(surface_stat_mm(n_surface_stat))
      if (.not. allocated(surface_stat_sr)) &
           allocate(surface_stat_sr(n_surface_stat))
!MSK
      if (.not. allocated(surface_stat_clc)) &
           allocate(surface_stat_clc(n_surface_stat))

      do n = 1,n_surface_stat

        surface_stat_wspeed(n) = rav1_surface_stat_wspeed(n)
        surface_stat_wdir(n)   = rav1_surface_stat_wdir(n)

! ***   At the moment these are treated as scalars in AirQUIS:
        surface_stat_tmp(n)    = rv_surface_stat_tmp
        surface_stat_dt(n)     = rv_surface_stat_dt
        surface_stat_press(n)  = rv_surface_stat_press
        surface_stat_rh(n)     = rv_surface_stat_rh
        surface_stat_mm(n)     = rv_surface_stat_mm
        surface_stat_clc(n)    = rv_surface_stat_clou

        surface_stat_sr(n)     = -9900.0

      enddo


      END SUBROUTINE SendMetObsDynamicData

!*****************************************************************************************************

      subroutine call_get_observations
!DEC$ ATTRIBUTES DLLEXPORT,STDCALL,ALIAS:'call_get_observations' :: call_get_observations
      implicit none

      call get_observations

      return

      end subroutine call_get_observations

!*****************************************************************************************************

      subroutine call_construct_initial_wind
!DEC$ ATTRIBUTES DLLEXPORT,STDCALL,ALIAS:'call_construct_initial_wind' :: call_construct_initial_wind
      implicit none

      call construct_initial_wind

      return

      end subroutine call_construct_initial_wind

!*****************************************************************************************************

      SUBROUTINE SendMetAdvanced(iv_i_surface_prof_method, &
                            iv_nst_method, &
                            iv_nust_method, &
                            bv_const_ref_height, &
                            rv_surf_obs_ref_height, &
                            rv_minimum_obs_windspeed, &
                            rv_maximum_obs_windspeed, &
                            rv_lower_lim_L, &
                            rv_lower_lim_hmix, &
                            bv_analytic)

!DEC$ ATTRIBUTES DLLEXPORT,STDCALL,ALIAS:'SendMetAdvanced' :: SendMetAdvanced

!-----------------------------------------------------------------------------------------

      IMPLICIT NONE

      integer :: iv_i_surface_prof_method
      integer :: iv_nst_method
      integer :: iv_nust_method

      real    :: rv_surf_obs_ref_height
      real    :: rv_minimum_obs_windspeed
      real    :: rv_maximum_obs_windspeed
      real    :: rv_lower_lim_L
      real    :: rv_lower_lim_hmix

      logical :: bv_const_ref_height
      logical :: bv_analytic

! *** Variable explanation and Default values:
! ***
! *** iv_i_surface_prof_method = 2    ! Selection of profile formula applied above the 
! ***                                 ! surface observations:
! ***                                 ! 1 = Power-law formula

! ***                                 ! 2 = Zilitinkevich vertical profile.
! ***
! *** iv_nst_method   = 2             ! Selection of PBL-height formula applied for 
! ***                                 ! stable and neutral conditions.
! ***
! *** iv_nust_method  = 4             ! Selection of PBL-height formula applied for 
! ***                                 ! unstable conditions.
! ***
! *** bv_const_ref_height  = .FALSE.  ! If "bv_const_ref_height" = .false. the horizontal 
! ***                                 ! interpolation of the surface wind is done along 
! ***                                 ! the variable mid-point height of the first 
! ***                                 ! sigma layer.
! ***
! *** rv_surf_obs_ref_height = 10.0   ! If "bv_const_ref_height" = .true. the horizontal 
! ***                                 ! interpolation of the surface wind is done at the 
! ***                                 ! constant height "rv_surf_obs_ref_height" above 
! ***                                 ! ground level.
! ***
! *** rv_minimum_obs_windspeed = 0.2  ! Minimum accepted value (m/s)of obs. wind speed.
! *** rv_maximum_obs_windspeed = 40.0 ! Maximum accepted value (m/s)of obs. wind speed.
! ***
! *** rv_lower_lim_L    = 20.0        ! Lower limit value (m) for positive values of the 
! ***                                 ! estimated value of the Monin-Obukhov length, L.   
! ***
! *** rv_lower_lim_hmix = 50.0        ! Lower limit value (m) of the estimated hmix.
! ***
! *** bv_analytic  = .TRUE.           ! If .true. the analytic solution is applied in 
! ***                                 ! SUBROUTINE TURB1 for very stable conditions.
! ***

! *** Initializing the variables declared above in: "module_mc_wind_met"

      i_surface_prof_method  = iv_i_surface_prof_method
      nst_method             = iv_nst_method
      nust_method            = iv_nust_method
      const_ref_height       = bv_const_ref_height
      surf_obs_ref_height    = rv_surf_obs_ref_height
      minimum_obs_windspeed  = rv_minimum_obs_windspeed
      maximum_obs_windspeed  = rv_maximum_obs_windspeed
      lower_lim_L            = rv_lower_lim_L
      lower_lim_hmix         = rv_lower_lim_hmix
      analytic               = bv_analytic

      END SUBROUTINE SendMetAdvanced

! **********************************************************************
      SUBROUTINE FreeMet_Memory()
! **********************************************************************

!DEC$ ATTRIBUTES DLLEXPORT,STDCALL,ALIAS:'FreeMet_Memory' :: FreeMet_Memory

      implicit none

      if (allocated(in_surfrough))  &
    deallocate(in_surfrough)

      if (allocated(surface_stat_id)) &
    deallocate(surface_stat_id)

      if (allocated(surface_stat_posx)) &
    deallocate(surface_stat_posx)

      if (allocated(surface_stat_posy)) &
    deallocate(surface_stat_posy)

      if (allocated(surface_stat_hgt_vel)) &
    deallocate(surface_stat_hgt_vel)

      if (allocated(surface_stat_hgt_tmp)) &
    deallocate(surface_stat_hgt_tmp)

      if (allocated(surface_stat_hgt_dtup)) &
    deallocate(surface_stat_hgt_dtup)

      if (allocated(surface_stat_hgt_dtlo)) &
    deallocate(surface_stat_hgt_dtlo)

      if (allocated(surface_stat_z0)) &
    deallocate(surface_stat_z0)

      if (allocated(surface_stat_pwr)) &
    deallocate(surface_stat_pwr)

      if (allocated(surface_stat_scale)) &
    deallocate(surface_stat_scale)

! Deallocate "module_mc_wind_winds" arrays:

      if (allocated (surface_stat_wspeed)) &
    deallocate (surface_stat_wspeed)

      if (allocated (surface_stat_wdir)) &
    deallocate (surface_stat_wdir)

      if (allocated (surface_stat_tmp)) &
    deallocate (surface_stat_tmp)

      if (allocated (surface_stat_dt)) &
    deallocate (surface_stat_dt)

      if (allocated (surface_stat_sr)) &
    deallocate (surface_stat_sr)

      if (allocated (surface_stat_press)) &
    deallocate (surface_stat_press)

      if (allocated (surface_stat_rh)) &
    deallocate (surface_stat_rh)

      if (allocated (surface_stat_mm)) &
    deallocate (surface_stat_mm)
!MSK
      if (allocated (surface_stat_clc)) &
    deallocate (surface_stat_clc)

! ----------------------------------------------------------------------

      if (allocated (app_surface_stat_scale)) &
    deallocate (app_surface_stat_scale)

      if (allocated (surface_stat_ffref)) &
    deallocate (surface_stat_ffref)

      if (allocated (surface_stat_name))  &
    deallocate (surface_stat_name)

      if (allocated (geostrophic_stat_name))  &
    deallocate (geostrophic_stat_name)

      if (allocated (geostrophic_stat_posx)) &
    deallocate (geostrophic_stat_posx)

      if (allocated (geostrophic_stat_posy)) &
    deallocate (geostrophic_stat_posy)

      if (allocated (geostrophic_stat_ffref)) &
    deallocate (geostrophic_stat_ffref)

      if (allocated (geostrophic_stat_ddref)) &
    deallocate (geostrophic_stat_ddref)

      if (allocated (geostrophic_stat_pwr)) &
    deallocate (geostrophic_stat_pwr)

      if (allocated (geostrophic_stat_scale)) &
    deallocate (geostrophic_stat_scale)

      if (allocated (surfrough)) &
    deallocate (surfrough)

      if (allocated (u0_surfrough)) &
    deallocate (u0_surfrough)

      if (allocated (v0_surfrough)) &
    deallocate (v0_surfrough)

      if (allocated (in_landuse)) &
    deallocate (in_landuse)

      if (allocated (landuse)) &
    deallocate (landuse)

      if (allocated (u0_surface_ref)) &
    deallocate (u0_surface_ref)

      if (allocated (v0_surface_ref)) &
    deallocate (v0_surface_ref)

      if (allocated (u0_geostrophic_ref)) &
    deallocate (u0_geostrophic_ref)

      if (allocated (v0_geostrophic_ref)) &
    deallocate (v0_geostrophic_ref)

      END SUBROUTINE FreeMet_Memory

!     End of module module_mc_wind_met
      end module module_mc_wind_met
