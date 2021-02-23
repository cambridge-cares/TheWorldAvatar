! <mprof_new.f90 - A component of the City-scale
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

      subroutine mprof_new

! ***  The subroutine calculates scaling parameters: 
! ***  Monin-Obukhov length
! ***  q-star.
! ***
! ***  Also parameters like:
! ***
! ***  mixing height, and 
! ***  turbulence (sigma-v and sigma-w) are estimated.
!
! ***  April 2011: Leiv Håvard Slørdal
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
!           2016  M. Karl: Uses functions CWDIR and PSIH in mod_mete
!           2016  M. Karl: Four configurations possible, depending on pimeteexternaldata
!           pimeteexternaldata = 0 to 3.  If 0 then read from external file inside of EPISODE.
!                                         If 1 then read from McWIND wind field.
!                                         If 2 then read from   UM Met-input.
!                                         If 3 then read from TAPM Met-input.
!
! ----------------------------------------------------------------------------------

      use mod_util
      use mod_main
      use mod_site
      use mod_time
      use mod_conc
      use mod_asrc
      use mod_mete

       implicit none

! *** External functions:
!_NOT_USED:      REAL :: CWDIR
!MSK      REAL    :: PSIH
!MSK  functions are in mod_mete.for now

!MSK      LOGICAL :: ATTIME

! *** Local variables:

      real,allocatable :: FFVEC(:)
      real,allocatable :: HVEC(:)
      real,allocatable :: APP_HVEC(:)

! *** Start Interpolation variables:
      real,allocatable :: U_LININT(:)
      real,allocatable :: V_LININT(:)

      real :: Z_EP_abg
      real :: Z_high_UM_abg
      real :: Z_low_UM_abg
! *** End Interpolation variables.

      real :: FFV
      real :: HMIXV
      real :: MOBULV
      real :: SCALEV
      real :: SVV
      real :: SWV
      real :: TM
      real :: T1
      real :: T2
      real :: USTARV
      real :: UU
      real :: U1
      real :: U2
      real :: VV
      real :: ZT1
      real :: ZT2
      real :: ZU1
      real :: ZU2
      real :: Z0V

      real :: ZT_m
      real :: ZT_t

      real :: QSTARV
      real :: TAUV
      real :: SHFLV
      real :: LHFLV
      real :: MFLXV
      real :: SHKINV
      real :: BUOYFLV
      real :: BUOYKINV
      real :: THSTARV
      real :: THvSTARV
      real :: WmixSTARV
      real :: TmixSTARV
      real :: P_mb_t
      real :: T_K_t
      real :: Th_K_t
      real :: Q_t

      real :: THETA_GRAD_ABOVE
      real :: max_hmix
      real :: min_hmix

! *** FFVEC  - Windspeeds ... at heights given in HVEC (m/s) (2*nz)
! *** FFV    - Windspeed value
! *** HMIXV  - Mixing height (m)
! *** HVEC   - Height values (2*nz)
! *** MOBULV - Monin-Obhukov length
! *** SCALEV - Scale value
! *** SVV    - Sigma-v value
! *** SWV    - Sigma-w value
! *** TM     - Temperature value
! *** THvSTARV - T*
! *** T1     - Temperature at height ZT1 (deg C)
! *** T2     - Temperature at height ZT2 (deg C)
! *** USTARV - Friction velocity
! *** UU     - Wind component
! *** U1     - Windspeed at height Z1 (m/s)
! *** U2     - Windspeed at height Z2 (m/s)
! *** VV     - Wind component
! *** ZT1    - Height for temperature nr. 1 (m)
! *** ZT2    - Height for temperature nr. 2 (m)
! *** ZU1    - Height for windspeed nr. 1 (m)
! *** ZU2    - Height for windspeed nr. 2 (m)
! *** Z0V    - Surface roughness (m)
! *** IX     - Main grid index in x-direction
! *** IY     - Main grid index in y-direction
! *** IZ     - Main grid index in z-direction
! *** NHV    - Number of height values
!
! *** SHFLV    - Sensible heat flux  (W/m2)
! *** LHFLV    - Latent heat flux    (W/m2)
! *** QSTARV   - Q_star (kg vap/kg moist air)
	
      real :: max_INV_L
      real :: min_pos_L
      real :: min_INV_L
      real :: max_neg_L
      real :: max_surfrough
      real ::  min_surfrough
      real :: max_surfrough_t
      real ::  min_surfrough_t
      real :: MIN_WINDSPEED
      real :: APPLIED_Z0_IF_ZERO
      real :: min_allowed_TAUV
      real :: TEST_GROUNDT

      real :: pot_fac
      real :: gpot_t
      real :: gpot_m
      real :: T_m
      real :: t2pot
      real :: Wt_low
      real :: Wt_high
      real :: th_grad
      
      real    :: First_layer_limit
      real    :: lhs_test_tel,lhs_test_nevn

      integer :: IX
      integer :: IY
      integer :: IZ
      integer :: IZ2
      integer :: NHV
      integer :: MHV
      integer :: I_hmax
      integer :: J_hmax
      integer :: I_hmin
      integer :: J_hmin
      integer :: I_INV_Lmax
      integer :: J_INV_Lmax
      integer :: I_INV_Lmin
      integer :: J_INV_Lmin
      integer :: profile_dt_comp

      logical :: interpolate_windfield = .FALSE.   ! Default value
      logical :: profile_method_anyway = .FALSE.   ! Default value


      real    :: qnet       ! Net radiation scalar value    [W/m2]
      real    :: rkin       ! Global radiation scalar value [W/m2]
      integer :: imon
      integer :: iday
      integer :: ihour

      real    :: tay_pri
      real    :: rho_air
      real    :: lambda_e
      real    :: s_slope
      real    :: sat_press
      real    :: rnn        ! Cloud cover scalar value [0,1]
      real    :: ralb       ! Albedo scalar value
      real    :: zh         ! z0_heat scalar value (set to z0_t(IX,IY) below).

!MSK start
      real    :: c_p          = 1005.0        ! [J/(kg K)]
      real    :: grav         = 9.81        ! [m/s2]
      real    :: R_dair       = 287.0       ! [J/(kg K)]
      real    :: P_ref        = 1000.0      ! [mb] Used for pot. temp reference.
      real    :: surfpres
      real    :: ny_air
      real    :: Re_star

      logical :: analytic     = .FALSE.
! *** analytic    : If "analytic" is .TRUE. the Holtslag (1984) analytic solution is applied in 
! ***               the TURB_PROFILE routine with use of the constant value of 0.08 K for theta_star.
!      real    :: lower_lim_L  = 1.0
      real    :: lower_lim_L  = 50.0 !BRUCE Important impact on stabile dispersion VDIV as it sets the length scale L/5

! *** lower_lim_L : If "analytic" is .FALSE. the given LOWER_LIM_L is applied as a lower boundary of 
! ***               the positive values of L. This value should be rather close to ZU2, since this 
! ***               will give Z/L close to 1, i.e. the border of the validity of the applied M-O theory.
      real    :: d_adiab      = 0.01                                            
! *** d_adiab  : Simplified dry adiabatis lapse rate.

      real :: ag_day   = 5.0
      real :: ag_night = 5.0
! *** ag_day     : Daytime Soil conduction parameter 
! ***              (5 for grass; in vU&H,85)
! *** ag_night   : Nighttime Soil conduction parameter
! ***             (5 for grass; in vU&H,85)
      real    :: theta_d          = 0.033
! *** theta_d     : Defining the "Empirical temp. scale" (Estimated to be 0.033 K in (vU&H,85) ).
!MSK end


! *** End of DECLARATIONS.

! *** Start of routine: ***************************************************************************
      
      NHV = 2*NZ
      MHV = NHV
      
! *** Allocate memory:
      IF (.NOT. ALLOCATED(FFVEC))    ALLOCATE (FFVEC(NHV))
      IF (.NOT. ALLOCATED(HVEC))     ALLOCATE (HVEC(NHV))
      IF (.NOT. ALLOCATED(APP_HVEC)) ALLOCATE (APP_HVEC(NHV))

      IF (.NOT. ALLOCATED(U_LININT)) ALLOCATE (U_LININT(NZ))
      IF (.NOT. ALLOCATED(V_LININT)) ALLOCATE (V_LININT(NZ))

!MSK start
      IF (.NOT. ALLOCATED(WMIXSTAR)) ALLOCATE (WMIXSTAR(NX,NY))
      IF (.NOT. ALLOCATED(TMIXSTAR)) ALLOCATE (TMIXSTAR(NX,NY))
!MSK end


! *** Testing the availability of various input data at the start of the simulation:  
      if (ATTIME(BDAT) .AND. ISH == 1 .AND. ITS == 1) then

        if (.not. allocated(z0_t)) then
          allocate(z0_t(NX,NY))
          z0_t = z0 * 0.1
        endif

!MSK start commented
!        if (.not. allocated(surfpres)) then 
!          allocate (surfpres(NX,NY))
!          standard_press_applied = .TRUE.
!          surfpres = standard_press
!        elseif (MINVAL(surfpres) <= 0.0) then
!          standard_press_applied = .TRUE.
!          surfpres = standard_press
!        endif
        surfpres = standard_press
!MSK end

        if (.not. allocated(rhum)) then
          allocate (rhum(NX,NY))
          default_rh_applied = .TRUE.
          rhum = default_rh  
        elseif(MINVAL(rhum) < 0.0) then
          default_rh_applied = .TRUE.
          rhum     = default_rh        
        endif
      
        if (.not. allocated(albedo)) then 
          allocate (albedo(NX,NY))
          default_albedo_applied = .TRUE.
          albedo = default_albedo
        elseif (MINVAL(albedo) <= 0.0) then
          default_albedo_applied = .TRUE.
          albedo = default_albedo
        endif
        
      endif  ! if (ATTIME(BDAT) .AND. ISH == 1 .AND. ITS == 1)
      
      if (messfe) write(messun,'(A,I3)') 'MPROF_NEW: iturb = ',iturb
      
      max_hmix    =  0.0      ! Help variables for reporting the maximum HMIXV value.
      min_hmix    =  1.E+36   ! Help variables for reporting the minimum HMIXV value.
      max_INV_L   = -1.E+36   ! Help variables for reporting the maximum INV_L value.
      min_INV_L   =  1.E+36   ! Help variables for reporting the minimum INV_L value.

      pot_fac   = R_dair/c_p  ! [dimless]

!_LHS_Change_26Jan2012_Start:
! *** Selecting the Minimum value for Hmix (Setting it to 50m  as in McWIND):
!      HMIX_MIN = DZ(1)
      HMIX_MIN = 50.0
!_LHS_Change_26Jan2012_End.
            
! *** Selecting the Minimum value for observed wind speeds and z0:
!	MIN_WINDSPEED      = 0.1
      MIN_WINDSPEED      = 0.5 !BRUCE
      APPLIED_Z0_IF_ZERO = 0.01

!_LHS_Change_08Feb2012_Start:
! *** Minimum setting for the surface momentum fluxes, in order to avoid division by zero.      
      min_allowed_TAUV = 1.0E-11
!_LHS_Change_08Feb2012_End.
! *************************************************************************************************

! *** The value of "First_layer_limit" decide wether to use surface and first layer values,
! *** or first- and second layer values for the estimation of surface layer profiles:

      First_layer_limit = 15.0
      
!MSK      if (MeteExternalData >= 2) then
        if (pimeteexternaldata >= 2) then

! ***   UM or TAPM input considered:

! ***   "profile_method_anyway" can be used if we want to test the use of UM and TAPM fluxes.
! ***   Default value of profile_method_anyway" is .FALSE. (See local declaration above) 

!        profile_method_anyway = .TRUE.
        if (DZ(1) .GT. First_layer_limit) then
          profile_dt_comp = 1  ! Applying ground-temp and 1 layer temp.
        else
          profile_dt_comp = 2  ! Applying first and second layer temp.
        endif

! ***   Vertical interpolation is applied for UM-input:
!MSK        if (MeteExternalData == 2) interpolate_windfield = .TRUE.
        if (pimeteexternaldata == 2) interpolate_windfield = .TRUE.

        IF (MESSFE) THEN
          WRITE (MESSUN,*)
!MSK          IF(MeteExternalData == 2)THEN
          if (pimeteexternaldata == 2) then
            WRITE (MESSUN,'(A)') 'MPROF_NEW: UM Met-data read from external files.'
!MSK          ELSEIF(MeteExternalData == 3)THEN
          elseif (pimeteexternaldata == 3) then
            WRITE (MESSUN,'(A)') 'MPROF_NEW: TAPM Met-data read from external files.'
          ENDIF
          
          IF(interpolate_windfield)THEN
            WRITE (MESSUN,'(A)') 'MPROF_NEW: The applied wind fiels is interpolated vertically.'
          ELSE
            WRITE (MESSUN,'(A)') 'MPROF_NEW: The applied wind fiels is not interpolated vertically.'
          ENDIF
          
          IF(profile_method_anyway)THEN
            WRITE (MESSUN,'(A)') 'MPROF_NEW: Met. model Fluxes are not applied, Fluxes are parameterized!'
          ELSE
            WRITE (MESSUN,'(A)') 'MPROF_NEW: Met. model Fluxes are applied directly.'
          ENDIF
          
          WRITE (MESSUN,*)

        ENDIF  ! IF (MESSFE)
        
      endif  ! if (MeteExternalData >= 2) 


! *** ------------------------------------------------------------------
!
! *** Define height values at the midpoint and at the top of 
! *** the vertical sigma layers: 
! *** HVEC(1)    = Midpoint of AirQUIS (or EPISODE) layer 1.
! *** HVEC(2)    = Midpoint of AirQUIS (or EPISODE) layer 2.
! ***   :
! *** HVEC(NZ)   = Midpoint of AirQUIS (or EPISODE) layer NZ.
! *** HVEC(NZ+1) = Top of AirQUIS (or EPISODE) layer 1.
! *** HVEC(NZ+2) = Top of AirQUIS (or EPISODE) layer 2.
! ***   :

      HVEC(     1) = DZ(1)/2.
      HVEC(NZ + 1) = Z(1)
      DO 10 IZ = 2,NZ
        HVEC(     IZ) = Z(IZ - 1) + DZ(IZ)/2.
        HVEC(NZ + IZ) = Z(IZ    )
   10 CONTINUE

! ***
! *** Go through all horizontal grid points:   *************************
! ***

      DO IY = 1,NY
       DO IX = 1,NX

!MSK start ! Initialization of wmixstar and tmixstar
           WMIXSTAR(IX,IY) = 0.0
           TMIXSTAR(IX,IY) = 0.0
!MSK end

! ***    Estimating the Surface Layer (Monin-Obukhov) scaling parameters:
! ***    u_star, theta-star, and Monin-Obukhov length.

! ***    --------------------------------------------------------------------------
! ***    First we define values of: ZT1, ZT2, T1 and T2; and TM = 0.5*(T1+T2).
! ***                          and: ZU1, ZU2, U1 and U2
! ***    for each grid-square (IX,IY):


! ***    For each horizontal grid square (IX,IY) we define surface layer
! ***    values of: ZT1, ZT2, T1 and T2; and TM = 0.5*(T1+T2).
! ***    For the Bedre-Byluft application we also create values that
! ***    are stored in TAIR(IX,IY) = ins_t(IX,IY,1) - CTOK 
! ***    and           DTDZ(IX,IY) = (T2 - T1)/(ZT2 - ZT1).

!MSK        if (MeteExternalData >= 2) then
        if (pimeteexternaldata >= 2) then

! ***      UM or TAPM input considered:

           if(profile_dt_comp == 1)then

! ***        Applying GTMP and temperature in First model layer:

! ***        If applying GTMP and First layer temp (UM: at 20m):
! ***        Height above ground of the ground temperature:
             ZT1 = z0_t(IX,IY)

!MSK             if (MeteExternalData == 2) then
             if (pimeteexternaldata == 2) then
! ***          Find the height (above ground) of the top of the UM-layer 1:
               ZT2 = 2.0 * ( gpot(IX,IY,1) - topm(IX,IY) )
!MSK             elseif (MeteExternalData == 3) then
             elseif (pimeteexternaldata == 3) then
! ***          Find the height (above ground) of the midpoint of the TAPM-layer 1:
               ZT2 = gpot(IX,IY,1)
             endif
! ***        In situ temperature (K) at ZT2:
             T2  = ins_t(IX,IY,1)
             
!MSK             if (MeteExternalData == 2) then
             if (pimeteexternaldata == 2) then
! ***          The ground temperature is given in degree Celsius in the UM-model:
               T1  = gtmp(IX,IY) + CTOK
!MSK             elseif (MeteExternalData == 3) then
             elseif (pimeteexternaldata == 3) then
! ***          The ground temperature is given in degree Kelvin in the TAPM-model:
               T1  = gtmp(IX,IY)
             endif

           elseif(profile_dt_comp == 2)then

! ***        Applying the two lowermost temperature values (K)
! ***        and using the right heights (ZT2-ZT1 <= 60m)
!MSK             if (MeteExternalData == 2) then
             if (pimeteexternaldata == 2) then
! ***          Height above ground level of the top of UM-layer 1:
               ZT1 = 2.0 * ( gpot(IX,IY,1) - topm(IX,IY) )
! ***          Height above ground level of the top of UM-layer 2:
               ZT2 = ZT1 + 2.0 * ( gpot(IX,IY,2) - topm(IX,IY) - ZT1 )
!MSK             elseif (MeteExternalData == 3) then
             elseif (pimeteexternaldata == 3) then
! ***          Height above ground level of the midpoint of TAPM-layer 1:
               ZT1 = gpot(IX,IY,1)
! ***          Height above ground level of the midpoint of TAPM-layer 2:
               ZT2 = gpot(IX,IY,2)
             endif
             
! ***        In situ temperature (in K) at the top of UM-layer 1:
             T1  = ins_t(IX,IY,1)
! ***        In situ temperature (in K) at the top of UM-layer 2:
             T2  = ins_t(IX,IY,2)
           endif

! ***      In situ temperature (Celsius) at the top of UM-layer 1:
           TAIR(IX,IY) = ins_t(IX,IY,1) - CTOK
! ***      Calculate the "surface layer" vertical temperature gradient:
           DTDZ(IX,IY) = (T2 - T1)/(ZT2 - ZT1)

!MSK         elseif (MeteExternalData == 1) then
         elseif (pimeteexternaldata == 1) then

! ***      McWIND and Integrated AirQUIS input considered:

! ***      Applying the original EPISODE TAIR - DTDZ and TURB_PROFILE preprocessing:
           ZT1 = ZT_LOWER
           ZT2 = ZT_UPPER
           if (ZT2 <= ZT1 .AND. iturb == 1) then
             if (MESSFE) then
               WRITE (MESSUN,'(A)')      &
                   'MPROF_NEW: ZT_UPPER less or equal than ZT_LOWER. Program terminates!'
               STOP
             endif             
           endif
           T2  = TAIR(IX,IY)
           if( iturb == 1 ) then
             T1  = TAIR(IX,IY) - (ZT2 - ZT1)*DTDZ(IX,IY)
           elseif (iturb == 2 .OR. iturb == 3) then
             T1 = T2
           else
             if (MESSFE) then
               WRITE (MESSUN,'(A)')       &
                   'MPROF_NEW: iturb is neither 1, 2 or 3. Program terminates!'
               STOP
             endif
           endif
! ***      Convert temperatures from deg. Celsius to deg. K:
           T1 = T1 + CTOK
           T2 = T2 + CTOK
           
         endif   ! elseif (MeteExternalData == 1)

! ***    Defines an average surface layer temperature (in Kelvin):
         TM = (T1 + T2)/2.

! *** --------------------------------------------------------------------------------------

! ***    For each horizontal grid square (IX,IY) we define surface layer
! ***    values of: ZU1, ZU2, U1 and U2.

         if(DZ(1) .GT. First_layer_limit)then
! ***      If the first layer is more than 15 m, we apply the wind
! ***      speed in the midpoint of the first layer, and U=0 at z0: 

! ***      Windspeed at z0-height and at midpoint of layer 1:
           Z0V = z0(IX,IY)
           ZU1 = Z0V

           if (pimeteexternaldata == 3) then
             ZU2       = gpot(IX,IY,1)

           elseif (pimeteexternaldata == 2) then
             ZU2 = gpot(IX,IY,1) - topm(IX,IY)

           elseif (pimeteexternaldata == 1) then
             ZU2 = DZ(1)/2.0
             ZU2 = ZU2 *(DEPTHM(IX,IY)/MOD_H)
           endif

           IF (ZU1 .EQ. 0.0) ZU1 = APPLIED_Z0_IF_ZERO

           U1 = 0.
           U2 = SQRT(U(IX,IY,1)*U(IX,IY,1) + V(IX,IY,1)*V(IX,IY,1))

!_LHS_Bedre-Byluft_March-2008_Start:  
!
! ***      Applying a minimum setting to the wind speed applied for 
! ***      estimating the surface layer scaling parameters. 

           U2 = MAX(U2,MIN_WINDSPEED)
           
         else

! ***      Now the first layer thickness is so shallow that we apply
! ***      the wind velocities in layer 1 and 2:
! ***

!MSK           if (MeteExternalData == 3) then
           if (pimeteexternaldata == 3) then
! ***        Height a.g.l. of midpoint of TAPM-layer 1:
             ZU1 = gpot(IX,IY,1)
! ***        Height a.g.l. of midpoint of TAPM-layer 2:
             ZU2 = gpot(IX,IY,2)
!MSK           elseif (MeteExternalData == 2) then
           elseif (pimeteexternaldata == 2) then
! ***        Height a.g.l. of midpoint of UM-layer 1:
             ZU1 = gpot(IX,IY,1) - topm(IX,IY)
! ***        Height a.g.l. of midpoint of UM-layer 2:
             ZU2 = gpot(IX,IY,2) - topm(IX,IY)
!MSK           elseif (MeteExternalData == 1) then
           elseif (pimeteexternaldata == 1) then
! ***        Height a.g.l. of midpoint of AirQUIS-layer 1:
             ZU1 = HVEC(1)
! ***        Height a.g.l. of midpoint of AirQUIS-layer 2:
             ZU2 = HVEC(2)
           endif

           U1  = SQRT(U(IX,IY,1)*U(IX,IY,1) + V(IX,IY,1)*V(IX,IY,1))
           U2  = SQRT(U(IX,IY,2)*U(IX,IY,2) + V(IX,IY,2)*V(IX,IY,2))

! ***      Check the use of "MIN_WINDSPEED" below:???
!MSK start
!MSK taking 2*MIN_WINDSPEED to ensure USTAR gets large enough
!MSK           if ( (U2-U1) .LT. MIN_WINDSPEED) then
           if ( (U2-U1) .LT. 2.*MIN_WINDSPEED) then
               U2 = U1 + 2.*MIN_WINDSPEED
           endif
!MSK end		 
          endif

! ***    The above has been done in order to be able to continue the calculation if
! ***    unrealistic Met values are imported.
! ***    -------------------------------------------------------------------------------

! ******************************************************************************************************
! ***    The IF-test below contains all of the treatment in case of Met. input from UM or TAPM:

!MSK         if (MeteExternalData >= 2) then
         if (pimeteexternaldata >= 2) then
! ***      UM- or TAPM-meteorology applied:
         
!MSK           if (MeteExternalData == 2) then
           if (pimeteexternaldata == 2) then
! ***        UM-input applied:

! ***        We now apply the surface flux values from the UM-model to
! ***        compute the Surface layer (Monin-Obukhov) scaling parameters.

 ! ***        Surface momentum stress: From "sfws_episode.asc", given in Nm-2.
             TAUV    =   ( taus_x(IX,IY) * taus_x(IX,IY) )     &
                       + ( taus_y(IX,IY) * taus_y(IX,IY) )
             TAUV    = SQRT(TAUV)
        
! ***        Surface water vapor flux: From "mflx_episode.asc", given in kg m-2 s-1.
             MFLXV   = mflx(IX,IY)

! ***   TAPM Input  *********

!MSK           elseif (MeteExternalData == 3) then
           elseif (pimeteexternaldata == 3) then
! ***        TAPM-input applied:

             USTARV    = ustr(IX,IY)
!MSK             P_mb_t    = pres(IX,IY,1)   ! TAPM outa does not contain pressure
             T_K_t     = ins_t(IX,IY,1)
             Th_K_t    = pot_t(IX,IY,1)
!MSK start: 
! ***        Applying Equation (2.26) page 22 of Garratt 1992:
             P_mb_t    = 1000.0*((Th_K_t/T_K_t)**(R_dair/c_p))
!MSK end
             rho_air = (P_mb_t*100.0)/(R_dair * T_K_t)

             THvSTARV  = pvstr(IX,IY)
             THSTARV   = ptstr(IX,IY)
!             QSTARV  =  MFLXV / (rho_air * USTARV)
! ***        Applying Equation (1.5) and (1.9) page 10 of Garratt 1992:
             BUOYFLV = - rho_air * c_p * THvSTARV  * USTARV
             
           endif

! ***      Surface sensible heat flux: From "shfx_episode.asc",
! ***      given in Wm-2.
           SHFLV   = shfl(IX,IY)

! ***      Surface latent heat flux: From "lhfx_episode.asc",
! ***      given in Wm-2.
           LHFLV   = lhfl(IX,IY)

! ***      Boundary layer height: From "hmix_episode.asc",
! ***      given in m. 
           HMIXV   = hmix(IX,IY)

!MSK           if (MeteExternalData == 2) then
           if (pimeteexternaldata == 2) then
! ***        UM-input applied:
           
! ***        Calculate pressure at the top of UM-layer 1.

! ***        Height a.s.l. of UM-layer 1:
             gpot_t  =  topm(IX,IY)                                      &
                     + 2.0 * ( gpot(IX,IY,1) - topm(IX,IY) )

             Wt_low  =  (gpot(IX,IY,2) - gpot_t)                         &
                      /(gpot(IX,IY,2) - gpot(IX,IY,1))
             Wt_high = 1.0 - Wt_low

! ***        P_mb_t: Pressure [mb] at the top of UM-layer 1.
             P_mb_t =   (Wt_low  * pres(IX,IY,1))                        &
                     + (Wt_high * pres(IX,IY,2))

! ***        T_K_t: In-situ temperature (K) at the top of UM-layer 1.
             T_K_t  = ins_t(IX,IY,1)

! ***        Th_K_t: Potential temperature (K) at the top of UM-layer 1. 
             t2pot  = (P_ref/P_mb_t) ** pot_fac
             Th_K_t = T_K_t * t2pot

! ***        Q_t:   Specific humidity (kg/kg) at the top of UM-layer 1.
             Q_t    = shum(IX,IY,1)

!_LHS_Change_08Feb2012_Start:
! ***             if(TAUV /= 0.0  .AND. .NOT. profile_method_anyway)then
             if(TAUV > min_allowed_TAUV  .AND. .NOT. profile_method_anyway)then
!_LHS_Change_08Feb2012_End.
! ***          FISHY: Sometimes zero TAUV values are produced by UM???

                   CALL TURB_UM(kappa,grav,c_p,R_dair,P_ref,                       &
                           TAUV,SHFLV,LHFLV,MFLXV,HMIXV,                        &
                           P_mb_t,T_K_t,Q_t,                                    &
                           BUOYFLV,USTARV,THSTARV,THvSTARV,QSTARV,MOBULV)

             else

! ***          If the UM-model gives zero surface momentum fluxes or if
! ***          "profile_method_anyway" = .TRUE. then we need to estimate
! ***          a value based on the "profile method":

               if (messfe .and. TAUV == 0.0) then
                 write (messun,2222) MOD(YEAR,100),MNTH,DAYM,HOUR,IX,IY
 2222            FORMAT                                                         &
                  ('MPROF_NEW: ZERO value of TAUV encountered for time ',      &
                   4I2.2,'  I= ',I4,'  J= ',I4)
               endif

               CALL TURB_PROFILE(analytic,grav,d_adiab,kappa,                   &
                                ZU2,U2,ZU1,U1,                                 &
                                ZT2,T2,ZT1,T1,lower_lim_L,                     &
                                USTARV,THvSTARV,MOBULV)  

! ***          rho_air: Estimate of air density at the top of UM-layer 1.
               rho_air = (P_mb_t*100.0)/(R_dair * T_K_t)

               TAUV = rho_air * USTARV *USTARV

               if( .NOT. profile_method_anyway)then
! ***            The UM heat- and moisture fluxes are applied
! ***            to recalculate the scaling parameters.
                    CALL TURB_UM(kappa,grav,c_p,R_dair,P_ref,                     &
                             TAUV,SHFLV,LHFLV,MFLXV,HMIXV,                      &
                             P_mb_t,T_K_t,Q_t,                                  &
                             BUOYFLV,USTARV,THSTARV,THvSTARV,QSTARV,MOBULV)
               else
! ***            Now the original profile method is fully applied (i.e. Not applying
! ***            UM computed surface fluxes at all):
                 THSTARV = THvSTARV
                 QSTARV  = MFLXV / (rho_air * USTARV)
                 BUOYFLV = - rho_air * c_p * THvSTARV  * USTARV
               endif

             endif
             
!MSK           elseif (MeteExternalData == 3) then
           elseif (pimeteexternaldata == 3) then
! ***        TAPM-input applied:

! ***        Inserting an IF-test to make sure that we do not divide by zero:
             if (ABS(THvSTARV)  .LT. 1.0E-15) then
               !if (messfe) write (messun,'(A,E15.5)')                        &
               !           'MPROF_NEW:  TAPM      THvSTARV = ',THvSTARV
               THvSTARV = 1.0E-15
               !if (messfe) write (messun,'(A,E15.5)')                        &
               !           'MPROF_NEW:  Reset to: THvSTARV = ',THvSTARV   
             endif
             
! ***        Applying Equation (1.11) page 10 of Garratt 1992:             
             MOBULV = (USTARV * USTARV * Th_K_t) /                           &
                     (grav * kappa * THvSTARV)

           endif
           
! ***      SHKINV: Kinetic sensible heat flux in the "constant stress" surface layer.
! ***      Applying Equation (1.8) page 10 of Garratt 1992:
           SHKINV   = -THSTARV  * USTARV

! ***      BUOYKINV: Kinetic buoyancy flux in the "constant stress" surface layer.
! ***      Applying Equation (1.9) page 10 of Garratt 1992:
           BUOYKINV = -THvSTARV * USTARV

! ***      Now all necessary scaling parameters have been estimated in grid cell (IX,IY),
! ***      either fully by the flux values from the external model (TURB_UM), or partly 
! ***      by use of TURB_PROFILE and TURB_UM if TAUV = 0, or fully by the profile model (TURB_PROFILE)
! ***      if "profile_method_anyway" = .TRUE.

!MSK           if (MeteExternalData == 2) then
           if (pimeteexternaldata == 2) then
! ***        UM-input applied:

! ***        CALCULATE POTENTIAL TEMPERATURE IN MID-POINT OF EACH UM-LAYER:
! ***        **************************************************************
!
! ***        The height a.s.l of the midpoint of the first UM-layer: 
             gpot_m  = gpot(IX,IY,1)
 
! ***        The height above ground of the midpoint of UM-layer 1:
             ZT_m = gpot_m - topm(IX,IY)

! ***        The height above ground of the top of UM-layer 1:
             ZT_t = gpot_t - topm(IX,IY)

! ***        Th_K_t: The potential temperature (K) at the top of UM-layer 1.

! ***        pot_t_mid(IX,IY,1): Pot. temp. (K) in midpoint of UM-layer 1.
             pot_t(IX,IY,1) = Th_K_t + THvSTARV * (ALOG(ZT_m/ZT_t)                     &
                                                  - PSIH(DBLE(ZT_m/MOBULV))            &
                                                  + PSIH(DBLE(ZT_t/MOBULV)) ) / kappa

! ***        T_m: In situ temperature in midpoint of layer 1:
             t2pot = (P_ref/pres(IX,IY,1)) ** pot_fac
             T_m =  pot_t(IX,IY,1) / t2pot

! ***        START: TEST writeout to the EPISODE LOG-file.   *************************
             TEST_GROUNDT = T_m - 0.01*(z0_t(IX,IY) - ZT_t)                             &
                               +  THvSTARV * (ALOG(z0_t(IX,IY)/ZT_m)                    &
                                              - PSIH(DBLE(z0_t(IX,IY)/MOBULV))          &
                                              + PSIH(DBLE(ZT_m/MOBULV)) ) / kappa

             DO IZ = 1,NZ-1

               Wt_low  =  (gpot(IX,IY,IZ+1) - gpot_t)                                 &
                        /(gpot(IX,IY,IZ+1) - gpot(IX,IY,IZ))
               Wt_high = 1.0 - Wt_low

! ***          P_mb_t: Pressure [mb] at the top of UM-layer IZ.
               P_mb_t =   (Wt_low  * pres(IX,IY,IZ))                                   &
                       + (Wt_high * pres(IX,IY,IZ+1))

! ***          rho_air: Estimate of air density at the top of UM-layer IZ.
               rho_air = (P_mb_t*100.0)/(R_dair * ins_t(IX,IY,NZ))

!_LHS_Bedre-Byluft_March-2008_Start:
!
! ***          kshfl3d_t(IX,IY,IZ): Kinematic Sensible heat flux interpolated
! ***                               to the top of UM-layer IZ.
!              kshfl3d_t(IX,IY,IZ) =  (Wt_low  * shfl3d(IX,IY,IZ))
!     &                             + (Wt_high * shfl3d(IX,IY,IZ+1))
!
! ***          Dividing by (c_p * rho_air) to get the kinematic value:
!
!              kshfl3d_t(IX,IY,IZ) = kshfl3d_t(IX,IY,IZ)/(c_p * rho_air)

               t2pot = (P_ref/P_mb_t) ** pot_fac
! ***          pot_t_top(IX,IY,IZ): Pot. temp. [K] at the top of UM-layer IZ.
               pot_t_top(IX,IY,IZ) = ins_t(IX,IY,IZ) * t2pot

! ***          gpot_t: UM-height (a.s.l) of top of UM-layer (IZ + 1). 
               gpot_t = gpot_t + 2.0 * (gpot(IX,IY,IZ+1) - gpot_t)

             ENDDO

! ***        rho_air: Rough estimate of air density at the top of UM-layer NZ.
             rho_air = (pres(IX,IY,NZ)*100.0)/(R_dair * ins_t(IX,IY,NZ))

! ***        P_mb_t: Estimate of the pressure [mb] at the top of UM-layer NZ.
             P_mb_t  = P_mb_t - 0.01*(rho_air * grav                              &
                                   * (gpot_t - gpot(IX,IY,NZ)))

! ***        kshfl3d_t(IX,IY,NZ): Kinematic Sensible heat flux at the top of
! ***                             UM-layer NZ assumed equal to the value at
! ***                             the mid-point of layer NZ.
!
!            kshfl3d_t(IX,IY,NZ) = shfl3d(IX,IY,NZ)/(c_p * rho_air)

! ***        pot_t_top(IX,IY,NZ): Pot. temp. [K] at the top of UM-layer NZ.
             t2pot = (P_ref/P_mb_t) ** pot_fac
             pot_t_top(IX,IY,NZ) = ins_t(IX,IY,NZ) * t2pot  

! ***        Finding the potential temperature in the mid-point of each UM-
! ***        layer by interpolating linearly from the values at the layer
! ***        interfaces (have already the value in midpoint of UM-layer 1):

             DO IZ = 2,NZ

               pot_t(IX,IY,IZ) = 0.5 * (  pot_t_top(IX,IY,IZ)                   &
                               + pot_t_top(IX,IY,IZ-1) )

               th_grad = (pot_t(IX,IY,IZ) - pot_t(IX,IY,IZ-1))                  &
                       /(gpot(IX,IY,IZ) - gpot(IX,IY,IZ-1))

! ***          K_h(IX,IY,IZ-1): Eddy diffusivity for Heat [m2/s] defined at
! ***                           the top of UM-layer 1 - (NZ-1):
!
!              K_h(IX,IY,IZ-1) = - kshfl3d_t(IX,IY,IZ-1)/th_grad

             ENDDO

! ***        K_h(IX,IY,NZ): Eddy diffusivity for Heat [m2/s] at the top of
! ***                       UM-layer NZ is assumed equal to the value at the
! ***                       layer below.

!            K_h(IX,IY,NZ) = K_h(IX,IY,NZ-1)

           endif  ! if (MeteExternalData == 2)

!***********************************************  ENDIF UM-met.


!MSK           IF(interpolate_windfield .AND. MeteExternalData == 2 )THEN
           IF(interpolate_windfield .AND. pimeteexternaldata == 2) then
 
! ***        Interpolate the horizontal UM wind components to the
! ***        midpoints of the EPISODE layers.
!
! ***        We assume that the lowermost layer UM or TAPM components can be
! ***        directly applied in the midpoint of EPISODE layer 1.

             U_LININT(1) = U(IX,IY,1)
             V_LININT(1) = V(IX,IY,1)

! ***        gpot(IX,IY,IZ) - topm(IX,IY):   Height above ground of mid-
! ***                                        point of UM-layer IZ.
! ***
! ***        gpot(IX,IY,IZ):                 Height above ground of mid-
! ***                                        point of TAPM-layer IZ.
! ***
! ***        HVEC(IZ)*(DEPTHM(IX,IY)/MOD_H): Height above ground of mid-
! ***                                        point of EPISODE-layer IZ.

             DO IZ = 2,NZ

               IF(topm(IX,IY) .LT. 1.0)THEN
                 U_LININT(IZ) = U(IX,IY,IZ)
                 V_LININT(IZ) = V(IX,IY,IZ)
               ELSE

                 Z_EP_abg = HVEC(IZ)*(DEPTHM(IX,IY)/MOD_H)

                 IZ2 = 2

                 DO
!                   IF (MESSFE) WRITE (MESSUN,'(A,I3)') 
!     &               'MPROF_NEW: Vertcal Wind-interpolation: IZ2 = ',IZ2

                   Z_high_UM_abg = gpot(IX,IY,IZ2) - topm(IX,IY)
                   Z_low_UM_abg  = gpot(IX,IY,IZ2-1) - topm(IX,IY)
                   
                   IF(Z_EP_abg > Z_high_UM_abg)THEN
                     IZ2 = IZ2 + 1
                     IF (IZ2 == NZ+1) THEN
                       Wt_low  = 1.0
                       Wt_high = 0.0 
                       U_LININT(IZ) = U(IX,IY,NZ)
                       V_LININT(IZ) = V(IX,IY,NZ)
                       EXIT                    
                     ENDIF
                   ELSEIF(Z_EP_abg <= Z_high_UM_abg .AND.              &
                     Z_EP_abg  > Z_low_UM_abg)THEN
                     lhs_test_tel  = Z_high_UM_abg - Z_EP_abg
                     lhs_test_nevn = Z_high_UM_abg - Z_low_UM_abg
                     if(lhs_test_nevn .lt. 0.1)then
                       Wt_low = 1.0
                     else  
                       Wt_low  =  (Z_high_UM_abg - Z_EP_abg)           &
                                /(Z_high_UM_abg - Z_low_UM_abg)
                     endif
                     Wt_high = 1.0 - Wt_low

                     U_LININT(IZ) = (Wt_low  * U(IX,IY,IZ2-1))         &
                                 + (Wt_high * U(IX,IY,IZ2))
                     V_LININT(IZ) = (Wt_low  * V(IX,IY,IZ2-1))         &
                                 + (Wt_high * V(IX,IY,IZ2))

                     EXIT

                   ENDIF

                 ENDDO

               ENDIF

             ENDDO

             DO IZ = 1,NZ
               U(IX,IY,IZ) = U_LININT(IZ) 
               V(IX,IY,IZ) = V_LININT(IZ)
             ENDDO

           ENDIF  ! IF(interpolate_windfield .and. MeteExternalData == 2)

         endif  ! if (MeteExternalData >= 2)

! *************************************************************************************************

! ***    Standard AirQUIS application without met from either UM or TAPM:
!MSK         if (MeteExternalData == 1) then
         if (pimeteexternaldata == 1) then 

! ***      Now the traditional EPISODE met preprocessing is applied 
! ***      and the stability calculations are based on surface
! ***      layer measurements of wind speed and:
! ***      if (iturb == 1): delta-T
! ***      if (iturb == 2): Global radiation
! ***      if (iturb == 3): Net radiation

! ***      Calculate the M-O scaling parameters:
! ***      "ustarv" : The friction velocity        (in m/s).
! ***      "tstarv" : The temperature scale value  (in Kelvin).
! ***      "mobulv" : The Monin-Obukhov length     (in meters).
! ***      "hmixv"  : The PBL-height               (in meters).

           if (iturb == 1) then

             CALL TURB_PROFILE(analytic,grav,d_adiab,kappa,     &
                              ZU2,U2,ZU1,U1,                   &
                              ZT2,T2,ZT1,T1,lower_lim_L,       &
                              USTARV,THvSTARV,MOBULV)  

           else

! ***        Now "iturb" is either 2 (Input: Global radiation) or 3 (Input: Net radiation)

! ***        Below we gather all common calculations of thermodynamic relations:
!
! ***        Calculating the (dry?) air density:
!MSK             rho_air = (surfpres(IX,IY) * 100.0)/(R_dair*TM)
             rho_air = (surfpres * 100.0)/(R_dair*TM)
             
! ***        The latent heat of vaporization of water:
!	       lambda_e = (2465. - 2.38*(tm-15.))*1.  !Used for ORG_MEPDIM!
! ***        Eliassen & Pedersen (1977) page 52:
             lambda_e = (2501. - 2.37*(TM - 273.15))*1000.
!
! ***        Slope (S) of the saturation enthalphy curve, Eq.(32) vU&H,1985:
! ***        According to vU&H,1985 this is quite well approximated for
! ***        270 < T < 310 (i.e. -3 < T < +40 in Celsius).
             s_slope=EXP(0.055*(TM - 279.))     !Used for ORG_MEPDIM!
!
! ***        Garratt (1992) page 284: "Bolton's (1980) fit to Wexler (1976):
             sat_press = 6.112*EXP(17.67*(TM - 273.15)/(TM - 29.65))

! ***        Eliassen & Pedersen (1977) page 61: Approximately we have: 
! ***            q_sat = 0.622*sat_press/surfpres(IX,IY)
! ***            q_2m  = rhum(IX,IY) * q_sat
!
! ***        The Taylor-Priestly constant
             IF (rhum(IX,IY) > 0.95) THEN
               tay_pri = 1.0                        !Used for ORG_MEPDIM!!
             ELSE
               tay_pri = dry_tay_pri
             ENDIF
!
! ***        Surface roughness for heat:
             zh = z0_t(IX,IY)
!             ag_day = 5.0    ! Set as defaults in "mod_mete"
!             ag_night = 5.0  ! Set as defaults in "mod_mete"
             rnn    = clou(IX,IY)
             
             if (iturb == 2) then
               rkin   = tsrad(IX,IY)
               ralb   = albedo(IX,IY)
               IMON   = MNTH
               IDAY   = DAYM
               IHOUR  = HOUR

               CALL TURB_EBUDGET(analytic,grav,St_Bo_const,                           &
                                d_adiab,kappa,c_p,rho_air,                           &
                                lambda_e,s_slope,tay_pri,ag_day,ag_night,            &
                                theta_d,U1,U2,ZU1,ZU2,zh,ZT2,TM,rnn,rkin,            &
                                ralb,IMON,IDAY,IHOUR,LTOGMT,sitelo,sitela,           &
                                lower_lim_L,USTARV,THvSTARV,QSTARV,MOBULV)
         

  
  
             elseif (iturb == 3) then
             
               qnet   = nrad(IX,IY)
               CALL TURB_QNET(analytic,grav,St_Bo_const,                             &
                             d_adiab,kappa,c_p,rho_air,                             &
                             lambda_e,s_slope,tay_pri,ag_day,ag_night,              &
                             theta_d,U1,U2,ZU1,ZU2,zh,ZT2,TM,rnn,qnet,              &
                             lower_lim_L,USTARV,THvSTARV,QSTARV,MOBULV)
         
             
             endif
             
           endif
           
           
! ...      Calculate Surface Momentum flux, Kinematic and Total Sensible 
! ...      Heat Flux, and Latent Heat Flux:

           QSTARV = 0.0
!MSK           CALL FLUX_1(USTARV,THvSTARV,QSTARV,TM,TAUV,SHKINV,SHFLV,LHFLV)
           CALL FLUX_1_NEW(USTARV,THvSTARV,QSTARV,TM,TAUV,SHKINV,SHFLV,LHFLV)

         if (messfe .and. IX == 15 .and. IY == 15) then
           write (messun,*)
           write (messun,'(A,E15.5)') 'MPROF_NEW:  TAUV(15,15)   = ',TAUV
           write (messun,'(A,E15.5)') 'MPROF_NEW:  SHKINV(15,15) = ',SHKINV
           write (messun,'(A,E15.5)') 'MPROF_NEW:  SHFLV(15,15)  = ',SHFLV
           write (messun,'(A,E15.5)') 'MPROF_NEW:  LHFLV(15,15)  = ',LHFLV                 
         endif

! ***      Calculate mixing height (The values are set in: "mod_mete"):
! ***      Original MERPDIM:  NST_METHOD  = 1  and  NUST_METHOD = 1
! ***      Nieuwstadt for Stable and Neutral: NST_METHOD  = 2
! ***      3 Alternatives for Unstable:
! ***        NUST_METHOD   = 2  ! Encroachment method (prognostic).
! ***        NUST_METHOD   = 3  ! Simplified B-G method (prognostic).
! ***        NUST_METHOD   = 4  ! Advanced B-G method (prognostic).

           HMIXV =  MAX(HMIX(IX,IY),HMIX_MIN)

! ***      The routine below finds the potential temperature gradient at the height
! ***      HMIXV of the previous hour. We apply default values (set in "mod_mete"):
! ***      A_prog_hmix  =   2.0 ;  B_prog_hmix  = 100.0 ; C_prog_hmix  =   0.005

!MSK           CALL POT_TMPGR(A_prog_hmix,B_prog_hmix,C_prog_hmix,HMIXV,THETA_GRAD_ABOVE)
           CALL POT_TMPGR_NEW(A_prog_hmix,B_prog_hmix,C_prog_hmix,HMIXV,THETA_GRAD_ABOVE)

           if (messfe .and. IX == 15 .and. IY == 15) then
             write (messun,'(A,E15.5)') 'MPROF_NEW:  prev_HMIX(15,15)        = ',HMIXV
             write (messun,'(A,E15.5)') 'MPROF_NEW:  THETA_GRAD_ABOVE(15,15) = ',THETA_GRAD_ABOVE     
           endif

           CALL MIXHT_NEW(NST_METHOD,NUST_METHOD,f_cor,TM,USTARV,     &
                            MOBULV,SHKINV,THETA_GRAD_ABOVE,HMIXV)

           HMIX(IX,IY) = MAX(HMIXV,HMIX_MIN)
           HMIXV       = HMIX(IX,IY)
           

           if (messfe .and. IX == 15 .and. IY == 15) then
             write (messun,'(A,E15.5)') 'MPROF_NEW:  new_HMIX(15,15)        = ',HMIXV
           endif
           
         endif  ! if (MeteExternalData == 1)


! *****************************************************************************************************************

!MSK         if (MeteExternalData == 3) then
         if (pimeteexternaldata == 3) then 
! ***      TAPM-input applied:

           WMIXSTAR(IX,IY) = wstr(IX,IY)
           TMIXSTAR(IX,IY) =                                    &
                    (pot_t(IX,IY,1) * BUOYKINV * BUOYKINV)/(grav*HMIXV)

!MSK         elseif (MeteExternalData <= 2) then
         elseif (pimeteexternaldata <= 2) then 
! ***      UM-input applied:
	  
! ***      Calculate Convective Velocity scale:

           CALL FLUX_2_NEW(TM,HMIXV,SHKINV,WmixSTARV,TmixSTARV)

           WMIXSTAR(IX,IY) = WmixSTARV
           TMIXSTAR(IX,IY) = TmixSTARV

         endif

!_LHS_Change_Jan2012_Start:

!MSK         if (MeteExternalData == 1 .AND. iturb >= 2) then
         if (pimeteexternaldata == 1 .AND. iturb >= 2) then
! ***      Creating surface layer temperature gradient in the case when 
! ***      radiation measurements are applied in AirQUIS.
           ZT2     = 10.0
           ZT1     = 2.0

           ZT_LOWER = ZT1
           ZT_UPPER = ZT2
                      
           DTDZ(IX,IY) =  THvSTARV * (  ALOG(ZT2/ZT1)                              &
                                     - PSIH(DBLE(ZT2/MOBULV))                           &
                                     + PSIH(DBLE(ZT1/MOBULV)) ) / (kappa * (ZT2 - ZT1))
         endif
         
!_LHS_Change_Jan2012_End.

         USTAR(IX,IY) = USTARV    ! NOTE: USTAR(NX,NY) is used hereafter.
         TSTAR(IX,IY) = THvSTARV  ! NOTE: TSTAR(NX,NY) is used hereafter.
!        QSTAR(IX,IY) = QSTARV    ! NOTE: QSTAR(NX,NY) is used hereafter.
         MOBUL(IX,IY) = MOBULV    ! NOTE: MOBUL(NX,NY) is used hereafter.

         if (messfe .and. IX == 15 .and. IY == 15) then
           write (messun,'(A,E15.5)') 'MPROF_NEW:  USTAR(15,15)    = ',USTAR(IX,IY)
           write (messun,'(A,E15.5)') 'MPROF_NEW:  TSTAR(15,15)    = ',TSTAR(IX,IY)
           write (messun,'(A,E15.5)') 'MPROF_NEW:  MOBUL(15,15)    = ',MOBUL(IX,IY)                             
           write (messun,'(A,E15.5)') 'MPROF_NEW:  WMIXSTAR(15,15) = ',WMIXSTAR(IX,IY)
           write (messun,'(A,E15.5)') 'MPROF_NEW:  TMIXSTAR(15,15) = ',TMIXSTAR(IX,IY)
           write (messun,*)      
         endif

         IF((1.0/MOBUL(IX,IY)) .GT. max_INV_L)THEN
           max_INV_L  = 1.0/MOBUL(IX,IY)
           min_pos_L  = MOBUL(IX,IY)
           I_INV_Lmax = IX
           J_INV_Lmax = IY
         ENDIF
        
         IF((1.0/MOBUL(IX,IY)) .LT. min_INV_L)THEN
           min_INV_L  = 1.0/MOBUL(IX,IY)
           max_neg_L  = MOBUL(IX,IY)
           I_INV_Lmin = IX
           J_INV_Lmin = IY
         ENDIF

         IF(USTAR(IX,IY) .EQ. 0.0)THEN
           IF (MESSFE) THEN
             WRITE (MESSUN,'(A14,I3.3,A1,I3.3,A4,E12.4)')  &
                  'MPROF_NEW: WARNING USTARV(',IX,',',IY,') = ',USTAR(IX,IY)
           ENDIF
         ENDIF

         IF(HMIXV .GT. max_hmix)THEN
           max_hmix = HMIXV
           I_hmax = IX
           J_hmax = IY
         ENDIF

         IF(HMIXV .LT. min_hmix)THEN
           min_hmix = HMIXV
           I_hmin = IX
           J_hmin = IY
         ENDIF

        ENDDO   ! DO IY = 1,NY
      ENDDO   ! DO IX = 1,NX
           
      if (messfe) then
      
         max_surfrough = MAXVAL(z0)
         min_surfrough = MINVAL(z0)
         max_surfrough_t = MAXVAL(z0_t)
         min_surfrough_t = MINVAL(z0_t)      
      
         WRITE (MESSUN,'(A,F10.7)') 'MPROF_NEW: max_surfrough   = ',max_surfrough
         WRITE (MESSUN,'(A,F10.7)') 'MPROF_NEW: min_surfrough   = ',min_surfrough
         WRITE (MESSUN,'(A,F10.7)') 'MPROF_NEW: max_surfrough_t = ',max_surfrough_t
         WRITE (MESSUN,'(A,F10.7)') 'MPROF_NEW: min_surfrough_t = ',min_surfrough_t         
         WRITE (MESSUN,2000)   &
          MOD(YEAR,100),MNTH,DAYM,HOUR,max_INV_L,I_INV_Lmax,J_INV_Lmax
         WRITE (MESSUN,2004)   &
          MOD(YEAR,100),MNTH,DAYM,HOUR,min_pos_L,I_INV_Lmax,J_INV_Lmax
         WRITE (MESSUN,2001)   &
          MOD(YEAR,100),MNTH,DAYM,HOUR,min_INV_L,I_INV_Lmin,J_INV_Lmin
         WRITE (MESSUN,2005)   &
          MOD(YEAR,100),MNTH,DAYM,HOUR,max_neg_L,I_INV_Lmin,J_INV_Lmin
         WRITE (MESSUN,2002)   &
          MOD(YEAR,100),MNTH,DAYM,HOUR,max_hmix,I_hmax,J_hmax
         WRITE (MESSUN,2003)   &
          MOD(YEAR,100),MNTH,DAYM,HOUR,min_hmix,I_hmin,J_hmin

      endif

!MSK      if (MeteExternalData <= 0) then
      if (pimeteexternaldata <= 0) then

! *** Calculate wind profile:

      DO 200 IY = 1,NY
        DO 210 IX = 1,NX

          USTARV = USTAR(IX,IY)
          MOBULV = MOBUL(IX,IY)

! ***     Windspeed in surface roughness height and in midpoint
! ***     of layer 1

          Z0V = z0(IX,IY)
          ZU1 = Z0V
          ZU2 = DZ(1)/2.
          ZU2 = ZU2 *(DEPTHM(IX,IY)/MOD_H)
          U1 = 0.
          U2 = SQRT(U(IX,IY,1)*U(IX,IY,1) + V(IX,IY,1)*V(IX,IY,1))

          U2 = MAX(U2,MIN_WINDSPEED)

! ***     Calculate windspeeds at the given heights:

          DO IZ = 1,NZ+1
            APP_HVEC(IZ) = HVEC(IZ)* DEPTHM(IX,IY)/MOD_H
          ENDDO

          print *,'mprof_new call WSPROF ',Z0V,ZU1,ZU2,U1,U2,USTARV,MOBULV,NHV,MHV,   &
                     APP_HVEC,FFVEC
          CALL WSPROF(Z0V,ZU1,ZU2,U1,U2,USTARV,MOBULV,NHV,MHV,   &
                     APP_HVEC,FFVEC)

! ***     Calculate wind:

          DO 220 IZ = 2,NZ

! ***       Current wind

            UU = U(IX,IY,IZ)
            VV = V(IX,IY,IZ)

! ***       Missing data?

            IF (UU .EQ. MISS .OR. VV .EQ. MISS) THEN

! ***         Copy from first layer:

              U(IX,IY,IZ) = U(IX,IY,1)
              V(IX,IY,IZ) = V(IX,IY,1)

! ***         Scale the wind according to current windprofile:

              FFV = SQRT(U(IX,IY,1)*U(IX,IY,1) +  &
                        V(IX,IY,1)*V(IX,IY,1))
              SCALEV = MAX(FFVEC(IZ),0.3)/MAX(FFV,0.3)

              U(IX,IY,IZ) = SCALEV*U(IX,IY,IZ)
              V(IX,IY,IZ) = SCALEV*V(IX,IY,IZ)

              IF (IX .EQ. 14 .AND. IY .EQ. 12) THEN
              IF (MESSFE) WRITE (MESSUN,2010)   &
                        MOD(YEAR,100),MNTH,DAYM,HOUR,SCALEV,IZ
              ENDIF

          ENDIF

  220     CONTINUE

  210 CONTINUE
  200 CONTINUE

! *** Finished constructing artificial wind profile.

      endif  ! if (MeteExternalData <= 0)

! *** Calculate sigma-v and sigma-w values

      DO 300 IY = 1,NY
       DO 310 IX = 1,NX

         HMIXV  = HMIX(IX,IY)
         USTARV = USTAR(IX,IY)
         MOBULV = MOBUL(IX,IY)

! ***    Sigma-v

         DO 320 IZ = 1,NZ

           IF (HVEC(IZ) .LT. HMIXV) THEN

! ***        Calculate sigma-v (profile method)

             CALL DIFFPAR(HVEC(IZ),HMIXV,USTARV,MOBULV,SVV,SWV)
              
             SVV = MAX(SVV,SIGVMIN(IZ))

             IF (SIGV(IX,IY,IZ) .EQ. MISS) THEN
               SIGV(IX,IY,IZ) = SVV
!               IF (IX .EQ. 11 .AND. IY .EQ. 11) THEN
!                 IF (MESSFE) WRITE (MESSUN,2020)
!     &             MOD(YEAR,100),MNTH,DAYM,HOUR,SVV,IZ
!               ENDIF
             ENDIF

           ELSE

! ***        Above mixing height

             CALL DIFFPAR(HMIXV,HMIXV,USTARV,MOBULV,SVV,SWV)
             SVV = MAX(SVV,0.1)
             IF (SIGV(IX,IY,IZ) .EQ. MISS) THEN
               SIGV(IX,IY,IZ) = SVV
!               IF (IX .EQ. 11 .AND. IY .EQ. 11) THEN
!                 IF (MESSFE) WRITE (MESSUN,2020)
!     &             MOD(YEAR,100),MNTH,DAYM,HOUR,SVV,IZ
!               ENDIF
             ENDIF

           ENDIF

  320    CONTINUE

! ***    Sigma-w

         DO 330 IZ = 1,NZ

           IF (HVEC(NZ + IZ) .LT. HMIXV) THEN

! ***        Calculate sigma-w (profile method)

             CALL DIFFPAR(HVEC(NZ + IZ),HMIXV,USTARV,MOBULV,SVV,SWV)

             SWV = MAX(SWV,SIGWMIN(IZ))

             IF (SIGW(IX,IY,IZ) .EQ. MISS) THEN
               SIGW(IX,IY,IZ) = SWV
!               IF (IX .EQ. 11 .AND. IY .EQ. 11) THEN
!                 IF (MESSFE) WRITE (MESSUN,2030)
!     &             MOD(YEAR,100),MNTH,DAYM,HOUR,SWV,IZ
!               ENDIF
             ENDIF

           ELSE

! ***        Above mixing height

             IF (SIGW(IX,IY,IZ) .EQ. MISS) THEN
               SIGW(IX,IY,IZ) = 0.1
!               IF (IX .EQ. 11 .AND. IY .EQ. 11) THEN
!                 IF (MESSFE) WRITE (MESSUN,2030)
!     &              MOD(YEAR,100),MNTH,DAYM,HOUR,SWV,IZ
!               ENDIF
               ENDIF

             ENDIF

! ***        Define sigma-w for the segmented plume model

             SIGWP(IX,IY,IZ) = SIGW(IX,IY,IZ)           

  330    CONTINUE

  310  CONTINUE
  300 CONTINUE

! *** Define universal minimum setting on HMIX

!      if (MeteExternalData <= 0) then
      if (pimeteexternaldata <= 0) then

        DO IY = 1,NY
          DO IX = 1,NX
                HMIX(IX,IY) = MAX(HMIX(IX,IY), HMIX_MIN)
          ENDDO
        ENDDO

      endif  ! if (MeteExternalData <= 0)

      IF (ALLOCATED(FFVEC))    DEALLOCATE (FFVEC)
      IF (ALLOCATED(HVEC))     DEALLOCATE (HVEC)
      IF (ALLOCATED(APP_HVEC)) DEALLOCATE (APP_HVEC)

      IF (ALLOCATED(U_LININT))  DEALLOCATE (U_LININT)
      IF (ALLOCATED(V_LININT))  DEALLOCATE (V_LININT)

      RETURN

 2000 format ('MPROF_NEW: Maximum Invers M-O length for time ',   &
             4I2.2,' = ',E14.6,' m.  I= ',I4,'  J= ',I4)
 2001 format ('MPROF_NEW: Minimum Invers M-O length for time ',   &
             4I2.2,' = ',E14.6,' m.  I= ',I4,'  J= ',I4)
 2004 format ('MPROF_NEW: i.e. Minimum Positive M-O length for time ',   &
             4I2.2,' = ',E14.6,' m.  I= ',I4,'  J= ',I4)
 2005 format ('MPROF_NEW: i.e. Maximum Negative M-O length for time ',   &
             4I2.2,' = ',E14.6,' m.  I= ',I4,'  J= ',I4)
 2002 format ('MPROF_NEW: Maximum mixing height for time ',       &
             4I2.2,' = ',F10.3,' m.  I= ',I4,'  J= ',I4)
 2003 format ('MPROF_NEW: Minimum mixing height for time ',       &
             4I2.2,' = ',F10.3,' m.  I= ',I4,'  J= ',I4)

 2010 format ('MPROF_NEW: Homogeneous windspeed scaling    for time ',   &
             4I2.2,' = ',F10.3,' for layer ',I3)
 2020 format ('MPROF_NEW: Sample sigma-v (14,12) ......... for time ',   &
             4I2.2,' = ',F10.3,' m/s',' for layer ',I3)
 2030 format ('MPROF_NEW: Sample sigma-w (14,12) ......... for time ',   &
             4I2.2,' = ',F10.3,' m/s',' for layer ',I3)
 2040 format ('MPROF_NEW: Sample friction velocity (14,12) . for time ',  &
             4I2.2,' = ',F10.3,' m/s')
 2050 format ('MPROF_NEW: Sample Monin-Obukov length (14,12) for time ',  &
             4I2.2,' = ',F10.3,' m')


! *** End of subroutine MPROF_NEW

       end subroutine mprof_new
