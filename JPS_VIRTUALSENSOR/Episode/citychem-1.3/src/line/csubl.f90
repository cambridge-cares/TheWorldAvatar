! <csubl.f90 - A component of the City-scale
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
!* ========================================== 
!*
!*****************************************************************************!
!*
!*        CITY-scale CHEMistry Transport Extension
!*
!*        Copyright (C) 2018  Matthias Steffen Karl
!*
!*        Contact Information: 
!*            Institute of Coastal Research
!*            Helmholtz-Zentrum Geesthacht
!*            Max-Planck-Str. 1
!*            21502 Geesthacht
!*            Germany
!*            email:  matthias.karl@hzg.de
!*
!*      EPISODE-CityChem, developed at Helmholtz-Zentrum Geesthacht (HZG) is designed
!*      for treating complex atmospheric chemistry in urban areas (Karl, 2018). The model
!*      is an extension of the EPISODE dispersion model to enable chemistry/transport
!*      simulations of reactive pollutants on city scale. EPISODE is an Eulerian dispersion
!*      model developed at the Norwegian Institute for Air Research (NILU) appropriate for
!*      air quality studies at the local scale (Slørdal et al. 2003 &2008). The model is an
!*      open source code subject to the Reciprocal Public License ("RPL") Version 1.5,
!*      https://opensource.org/licenses/RPL-1.5.
!*
!*        Reference:
!*      Karl, M. (2018):  Development of the city-scale chemistry transport model 
!*      CityChem-EPISODE and its application to the city of Hamburg, 
!*      Geosci. Model Dev. Discuss.,
!*      https://doi.org/10.5194/gmd-2018-8, 2018.
!*
!*****************************************************************************!

      subroutine CSUBL(IQL,XRV,YRV,ZRV,NCV,MCV,CRV,DDRV,WDRV)

!       The subroutine calculates integrated line source concentrations at
!       the given receptor point based on US EPA HIWAY2 (ver. 80346) and
!       NEWAY routines.
!       2017 Matthias Karl, HZG, CityChem extension:
!            Simplified Street Canyon Model
!            Coupling of HIWAY2 with the SSCM
! ----------------------------------------------------------------------------------
! SSCM is based on OSPM model formulation given in:
!
! Berkowicz, R., Hertel, O., Larsen, S.E., Sorensen, N.N., and Nielsen, M. (1997):
! Modelling traffic pollution in streets, Ministry of Environment and Energy,
! National Environmental Research Institute, Roskilde, Denmark, available at:
! http://www2.dmu.dk/1_viden/2_Miljoe-tilstand/3_luft/4_spredningsmodeller/
! 5_OSPM/5_description/ModellingTrafficPollution_report.pdf.
!
! And Equations + Tables given in:
!
! Ottosen, T.-B., Kakosimos, K.E., Johansson, C., Hertel, O., Brandt, J.,
! Skov, H., Berkowicz, R., Ellermann, T., Jensen, S.S., Ketzel, M. (2015):
! Analysis of the impact of inhomogeneous emissions in the Operational Street
! Pollution Model (OSPM), Geosci. Model Dev., 8, 3231-3245, doi:10.5194/
! gmd-8-3231-2015.  
!
! ----------------------------------------------------------------------------------
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
!    20 Apr 2018  M. Karl: L623           wcanyon = 2* WIDTH
!    23 Apr 2018  M. Karl: L1167          windward CSDIR scaled by 1.0
!    13 Aug 2018  M. Karl: L659           wdist increased by 2 m
!    13 Aug 2018  M. Karl: L1113-1117     correction of fext
!    13 Aug 2018  M. Karl: L677           minimum theta_street set to 1.0  
!    14 Aug 2018  M. Karl: L769-778       leeward if XW1+XW2 > 0.0   
!    14 Aug 2018  M. Karl: L1232          leeward CSDIR scaled by fext      
!    20 Aug 2018  M. Karl: L553           Set UE to be initial FF (roof level)   
!    20 Aug 2018  M. Karl: L728           use the smaller of FF and ff_street (receptor level)
!    22 Aug 2018  M. Karl: L676           if roadangle < 0 then roadangle + 180
!    23 Aug 2018  M. Karl: L730           adjusted ff_street
!    23 Aug 2018  M. Karl: L1292          windward CSDIR not scaled with fext
!    29 Aug 2018  M. Karl: L1148          xend bounded between 2m and 10m
!    29 Aug 2018  M. Karl: L1175          maximum integdx set to 1.4
!    30 Aug 2018  M. Karl: L1090          sigma_vent scaled by 2 (has been 6)
!    30 Aug 2018  M. Karl: L1111          maximum lratio set to 1.4
!    23 Jan 2019  M. Karl: L1380          format 2000 statement
!
! Note (24 Sep 18) M. Karl: the recirculation contribution is currently too
! small because sigma_vent is scaled by factor 2 (maybe better not to scale)
! and lratio is limited to 1.4 (it might be larger)  
!
! ----------------------------------------------------------------------------------

      use mod_util
      use mod_main
      use mod_site
      use mod_time
      use mod_mete
      use mod_conc
      use mod_depo
      use mod_lsrc
      use mod_phot
!MSK start 20.10.2017
      use mod_depo
!MSK end 20.10.2017

      implicit none

      integer :: IQL
      integer :: NCV
      integer :: MCV

      real :: CRV(MCV)
      real :: DDRV(MCV)
      real :: XRV
      real :: YRV
      real :: ZRV
      real :: WDRV(MCV)

! IQL  - Line source index
! NCV  - Number of compounds
! MCV  - Maximum number of compounds
! CRV  - Receptor  concentrations
! DDRV - Receptor dry depositions
! WDRV - Receptor wet depositions
! XRV  - Receptor x-coordinate
! YRV  - Receptor y-coordinate
! ZRV  - Receptor z-coordinate

! External functions

!MSK      REAL CWDIR     (is in mod_mete)
!MSK      REAL PSIM      (is in mod_mete)
      
! Local variables

      REAL,ALLOCATABLE :: CRVV(:)
      REAL CSUM
      REAL DD
      REAL DELR
      REAL DELS
      REAL DIST
      REAL DL
      REAL DM
      REAL DMV
      REAL DUM
      REAL FF
      REAL H
      REAL HL
      REAL QLLV
      REAL,ALLOCATABLE :: QLN(:)
      REAL RA
      REAL,ALLOCATABLE :: RAQ(:)
      REAL RB
      REAL,ALLOCATABLE :: RBQ(:)
      REAL REP1
      REAL REP2
      REAL RMINRV
      REAL RQ
      REAL SA
      REAL,ALLOCATABLE :: SAQ(:)
      REAL SB
      REAL,ALLOCATABLE :: SBQ(:)
      REAL SEP1
      REAL SEP2
      REAL SIGVE
      REAL SIGWE
      REAL SIGW0
      REAL SIGYE
      REAL SIGY0
      REAL THETA
      REAL TLGRE
      REAL UE
      REAL UU
      REAL U1
      REAL U2
      REAL VV
      REAL WIDTH
      REAL WINFL
      REAL WL
      REAL WROAD
      REAL XLV
      REAL XM
      REAL XW1
      REAL XW2
      REAL X0
      REAL X1
      REAL X2
      REAL YLV
      REAL YM
      REAL YW1
      REAL YW2
      REAL Y0
      REAL Y1
      REAL Y2
      REAL ZLV
      REAL Z1
      REAL Z2
      REAL ZFF_LSRC
      REAL ZFF_GRID
      REAL PH_FF, PFF_LSRC
      REAL P2_FF, PFF_GRID
      REAL scaletest
      REAL SCALEQL
      REAL Z0_LSRC

!MSK  .false.      LOGICAL use_timescale_no
      REAL theta_no
      REAL beta_no
      REAL dist_no
      REAL time_no
!      REAL MW_O3_no
!      REAL AVOG_no
      REAL TAIRV_no
!      REAL O3V_no
!      REAL RK1_no
!      REAL tau_no
      REAL init_timescale_no

      INTEGER IC
      INTEGER IQLL
      INTEGER IXL
      INTEGER IYL
      INTEGER IZL
      INTEGER KST

!MSK start
      real :: roadlink_length
      double precision :: cmo3
      double precision :: O3V_no
      double precision :: RK1_no
      double precision :: tau_no
      double precision :: MW_O3_no
      double precision :: AVOG_no
      real             :: K_COAG
      real             :: LAMBV
      real             :: TADD, TNUL, TNEW
      real             :: VDV
      real             :: QDV
      real             :: TL4, TZ2
      real             :: XD
      real             :: SIGZO, SIGZA, SIGZ
      !SIGZA, SIGZ
!MSK parameters of the street canyon treatment
! sigma_w0=0.25 from Kastner-Klein et al.(2000)
      real             :: building_height
      real             :: wcanyon
      real             :: lstreet
      real             :: ucr
      real             :: roadangle
      real             :: theta_street
      real             :: ff_street
      real             :: sigma_velo
      real             :: sigma_vent
      real             :: wdist
      real             :: xesc
      real             :: lmax
      real             :: lrec
      real             :: lbase
      real             :: ltop
      real             :: lhyp
      real             :: lratio
      real             :: fext
      real             :: xstart, xend, xend2
      real             :: integ2dx
      real             :: integdx
      real             :: const1, const2
      real             :: CSREC
      real             :: CSDIR  
      real, parameter  :: h0       = 2.00
      real, parameter  :: c0       = 1.85
      real, parameter  :: uprop    = 0.164
      real, parameter  :: sigma_w0 = 0.25
      logical          :: canyon=.false.
      logical          :: leeward=.false.
      logical          :: winward=.false.
      logical          :: extend=.false.
!MSK end

! CRVV   - Receptor concentration values
! CSUM   - Concentration sum
! DD     - Wind direction
! DELR   - Delta-y
! DELS   - Delta-x
! DIST   - Distance
! DL     - Distance
! DM     - Minimum distance
! DMV    - Minimum distance
! DUM    - Dummy value
! FF     - Wind speed
! H      - Line source emission height
! HL     - Height of the limiting lid
! QLLV   - Line source length
! QLN    - Line source emission
! RA     - Line source end point x-coordinate
! RAQ    - Line source end point x-coordinates
! RB     - Line source end point x-coordinate
! RBQ    - Line source end point x-coordinates
! REP1   - Line source end point x-coordinate
! REP2   - Line source end point x-coordinate
! RMINRV - Line source influence minimum distance
! RQ     - Dummy value
! SA     - Line source end point y-coordinate
! SAQ    - Line source end point y-coordinates
! SB     - Line source end point y-coordinate
! SBQ    - Line source end point y-coordinates
! SEP1   - Line source end point y-coordinate
! SEP2   - Line source end point y-coordinate
! SIGVE  - Sigma-v value
! SIGWE  - Sigma-w value
! SIGW0  - Sigma-w value close to line source
! SIGYE  - Sigma-y value
! SIGY0  - Sigma-y value close to line source
! THETA  - Weight coefficient
! TLGRE  - Langrangian timescale
! UE     - Wind speed
! UU     - Wind u-component
! U1     - Normal vector x-coordinate
! U2     - Normal vector y-coordinate
! VV     - Wind v-component
! WIDTH  - Line source width
! WINFL  - Line source width of lane influence sone
! WL     - Line source width of lane
! WROAD  - Line source width
! XLV    - Line source midpoint x-coordinate
! XM     - Line source to receptor minimum distance x-coordinate
! XW1    - Line source end point x-coordinate
! XW2    - Line source end point x-coordinate
! X0     - Receptor point x-coordinate
! X1     - Line source end point x-coordinate
! X2     - Line source end point x-coordinate
! YLV    - Line source midpoint y-coordinate
! YM     - Line source to receptor minimum distance y-coordinate
! YW1    - Line source end point x-coordinate
! YW2    - Line source end point x-coordinate
! Y0     - Receptor point y-coordinate
! Y1     - Line source end point y-coordinate
! Y2     - Line source end point y-coordinate
! ZLV    - Line source midpoint z-coordinate
! Z1     - Line source end point z-coordinate
! Z2     - Line source end point z-coordinate
! IC     - Compound index
! IQLL   - Line source lane index
! IXL    - Line source midpoint main grid index in x-direction
! IYL    - Line source midpoint main grid index in y-direction
! IZL    - Line source midpoint main grid index in z-direction
! KST    - Stability class

! Allocate memory

      IF (.NOT. ALLOCATED(CRVV)) ALLOCATE(CRVV(MQLL))
      IF (.NOT. ALLOCATED(QLN))  ALLOCATE(QLN(MQLL))
      IF (.NOT. ALLOCATED(RAQ))  ALLOCATE(RAQ(MQLL))
      IF (.NOT. ALLOCATED(RBQ))  ALLOCATE(RBQ(MQLL))
      IF (.NOT. ALLOCATED(SAQ))  ALLOCATE(SAQ(MQLL))
      IF (.NOT. ALLOCATED(SBQ))  ALLOCATE(SBQ(MQLL))

!MSK      max_scaletest = 0.0  !not used

! Get line source length

      QLLV = QLL(IQL)

! If line source length is nonpositive then return

      IF (QLLV .LE. 0.) GOTO 999

! Get line source width

      WIDTH = QLW(IQL)

! If line source width is nonpositive then return

      IF (WIDTH .LE. 0.) GOTO 999

! Get line source minimum influence distance

      RMINRV = QLRMIN(IQL)

! Get line source coordinates

      X1 = QLX(IQL,1)
      Y1 = QLY(IQL,1)
      Z1 = QLZ(IQL,1)
      X2 = QLX(IQL,2)
      Y2 = QLY(IQL,2)
      Z2 = QLZ(IQL,2)

! Calculate minimum distance from receptor point to line source

      call distrl(XRV,YRV,X1,Y1,X2,Y2,XM,YM,DM)

! Get receptor point coordinates

      X0 = XRV
      Y0 = YRV

      DMV = MAX(DM,RMINRV + WIDTH/2.)


! Perturb receptor point to distance DMV from current road

      IF (DMV .GT. DM) THEN

          IF (DM .GT. 0.) THEN

! Move receptor point to distance DMV from current road along the
! minimum distance calculated direction

              X0 = XM + (DMV/DM)*(X0 - XM)
              Y0 = YM + (DMV/DM)*(Y0 - YM)

              DM = DMV
              XRV = X0
              YRV = Y0

          ELSE

! Calculate normal unit vector to line source

              IF (Y2 .NE. Y1) THEN
                  RQ = (X2 - X1)/(Y2 - Y1)
                  U1 = 1./SQRT(1. + RQ*RQ)
                  U2 = -U1*RQ
              ELSE
                  U1 = 0.
                  U2 = 1.
              ENDIF

! Move receptor point to distance DMV from current road along the
! normal direction

              X0 = XM + DMV*U1
              Y0 = YM + DMV*U2

             IF (MESSFE) WRITE (MESSUN,2000) XRV,YRV,IQL,DM,DMV

              DM = DMV
              XRV = X0
              YRV = Y0

          ENDIF

      ENDIF

! Calculate line source midpoint coordinate

      XLV = (X1 + X2)/2.
      YLV = (Y1 + Y2)/2.
      ZLV = (Z1 + Z2)/2.

! Get midpoint of line source main grid indices

      call getmgi(1,XLV,YLV,ZLV,IXL,IYL,IZL)

!MSK start
!MSK   Now we have the grid cell coordinates of the line source
!MSK   Check land use if street canyon option has been set to 1
!MSK   Land use 32 and 33 is sparse-built urban area
!MSK   Land use 34 and 35 is dense-built urban area
!MSK   Average building heights are crude assumptions
!MSK   Martin Ramacher: Building heights from Hamburg 3-D city
!MSK   model LoD1-DE (LGV, 2014)
!MSK   For class 33 the mean of classes 32 and 34 is used

      IF (lsrccanyon.eq.1) then
         if (LANU(IXL,IYL).eq.32.0) then
           canyon = .true.
           building_height = 6.6
         else if (LANU(IXl,IYL).eq.33.0) then
           canyon = .true.
           building_height = 12.3          
         else if (LANU(IXl,IYL).eq.34.0) then
           canyon = .true.
           building_height = 18.0
         else if (LANU(IXl,IYL).eq.35.0) then
           canyon = .true.
           building_height = 18.0
         else
!MSK no street canyon in this grid cell
           canyon = .false.
           building_height = 5.0
         endif
      ELSE
!MSK street canyon option off
        canyon = .false.
        building_height = 5.0
      ENDIF
!MSK end

! Get windspeed and direction at midpoint of line source

      UU = U(IXL,IYL,IZL)
      VV = V(IXL,IYL,IZL)
      FF = SQRT(UU*UU + VV*VV)
      Z0_LSRC = Z0(IXL,IYL)
      ! BRUCE: Have included this so that other z0 can be given than the grid
      Z0_LSRC = 0.4
      ZRV = 2.0

!MSK 20.08.2018, roof level wind UE
      UE    = FF
      UE    = MAX(UE,LSRCFFMIN)

!_LHS_Change_19January_and_07March_2012_Start:
!Bruce: Changed from 15 to 7.5 to allow interpolation with 10 m grid
!Bruce: Prescribed z0 and wind speed height independent of receptor height

      if (DZ(1) > 7.5) then
! ***   Change to either 2 m or 3.5m above the local Z0-value.
        !ZFF_LSRC  = ZRV + Z0_LSRC
        ZFF_LSRC  = 2.0 
!        ZFF_LSRC  = 3.5 + Z0(IXL,IYL)
        PFF_LSRC  =  ALOG(ZFF_LSRC/Z0_LSRC)       &
!MSK                   - PSIM(ZFF_LSRC/MOBUL(IXL,IYL))        &
!MSK                   + PSIM(Z0_LSRC/MOBUL(IXL,IYL))
                   - PSIM(DBLE(ZFF_LSRC/MOBUL(IXL,IYL)))        &
                   + PSIM(DBLE(Z0_LSRC/MOBUL(IXL,IYL)))

        ZFF_GRID  = (0.5*DZ(1)*DEPTHM(IXL,IYL))/MOD_H
        PFF_GRID  =  ALOG(ZFF_GRID/Z0_LSRC)        &
!MSK                   - PSIM(ZFF_GRID/MOBUL(IXL,IYL))        &
!MSK                   + PSIM(Z0_LSRC/MOBUL(IXL,IYL))     
                   - PSIM(DBLE(ZFF_GRID/MOBUL(IXL,IYL)))        &
                   + PSIM(DBLE(Z0_LSRC/MOBUL(IXL,IYL)))  

        if (PFF_GRID .NE. 0.0) then
        !BRUCE: included this max here for the occasional receptor above the first grid
          scaletest = min(PFF_LSRC/PFF_GRID,1.0)
          if (scaletest > 1.0) then
            if(messfe) then 
              write (messun,'(A,E12.5)') 'CSUBL: scaletest greater than one. scaletest = ',scaletest
              write (messun,'(A,I2.2,A,I2.2)') 'CSUBL: For IXL = ',IXL,' IYL = ',IYL
              write (messun,'(A)') 'CSUBL: PROGRAM TERMINATES! '
            endif
            CALL STOPIT('CSUBL: scaletest > 1!')
          elseif (scaletest == 0.0) then
            if(messfe) then 
              write (messun,'(A,E12.5)') 'CSUBL: scaletest equal to zero. scaletest = ',scaletest
              write (messun,'(A,I2.2,A,I2.2)') 'CSUBL: For IXL = ',IXL,' IYL = ',IYL
              write (messun,'(A)') 'CSUBL: PROGRAM TERMINATES! '
            endif
            CALL STOPIT('CSUBL: scaletest = 0!')
          elseif (scaletest < 0.0) then
            if(messfe) then 
              write (messun,'(A,E12.5)') 'CSUBL: scaletest less than zero. scaletest = ',scaletest
              write (messun,'(A,I2.2,A,I2.2)') 'CSUBL: For IXL = ',IXL,' IYL = ',IYL
              write (messun,'(A)') 'CSUBL: PROGRAM TERMINATES! '
            endif
            CALL STOPIT('CSUBL: scaletest < 0!')          
          endif
        else
          if(messfe) then 
            write (messun,'(A,E12.5)') 'CSUBL: PFF_GRID equal to zero. PFF_GRID = ',PFF_GRID
            write (messun,'(A,I2.2,A,I2.2)') 'CSUBL: For IXL = ',IXL,' IYL = ',IYL
            write (messun,'(A)') 'CSUBL: PROGRAM TERMINATES! '
          endif
          CALL STOPIT('CSUBL: PFF_GRID = 0!')
        endif
   
!_LHS:  The height adjustment can be skipped by simply commenting out the line below:
!        FF = FF*PFF_LSRC/PFF_GRID
!     BRUCE: have commented this out to check its impact
        FF = FF*scaletest
      endif   ! if (DZ(1) > 15.0)

!MSK: after the previous step, FF is the first guess of the wind speed at the receptor


      if(FF < 0.0)then
        if(messfe) then 
          write (messun,'(A,E12.5)') 'CSUBL: Negative windspeed applied.  FF = ',FF
          write (messun,'(A)') 'CSUBL: PROGRAM TERMINATES! '
        endif
        CALL STOPIT('CSUBL: negative windspeed!')
      elseif(FF == 0.0)then
!MSK start  Set FF to min value LSRCFFMIN
!MSK        Because wind speed at receptor should not be lower than the user-provided
!MSK        minimum wind speed
        FF = LSRCFFMIN
!MSK end
        if(messfe) then 
          write (messun,'(A,E12.5)') 'CSUBL: ZERO windspeed applied.  FF = ',FF
          write (messun,'(A,E12.5)') 'CSUBL: Windspeed value reset to LSRCFFMIN = ',LSRCFFMIN
        endif
      endif


!MSK start  Below will be replaced by a new treatment for the minimum wind speed
!MSK!_LHS: Treating wind speeds below 2 m/s, with a linear decrease
!MSK!_LHS: down to the user given LSRCFFMIN which must be <= 1.0 m/s.
!MSK!     BRUCE: THIS MEANS THAT IF FF=2 THEN SUDDENLY FF=3 IF LSRCFFMIN=1. CHANGED
!MSK!      if (FF < 2.0) FF = LSRCFFMIN + (2.0 - LSRCFFMIN)*FF
!MSK      if (FF < 2.0) FF = LSRCFFMIN + (2.0 - LSRCFFMIN)/2.0*FF
!MSK end

      DD = CWDIR(UU,VV)
      !print *,'csubl cwdir', DD

! Calculate wind directed coordinate system rotation angle

      THETA = (270. - DD)*RAD

! Transform line source coordinates to wind directed coordinates
! with origin at receptor point

      XW1 = +COS(THETA)*(X1 - XRV) + SIN(THETA)*(Y1 - YRV)             
!      YW1 = -SIN(THETA)*(X1 - XRV) + COS(THETA)*(Y1 - YRV)
      XW2 = +COS(THETA)*(X2 - XRV) + SIN(THETA)*(Y2 - YRV)
!      YW2 = -SIN(THETA)*(X2 - XRV) + COS(THETA)*(Y2 - YRV)


! Set windspeed and direction for line source calculations

!MSK start     
!MSK Orientation angle wind direction / street
      if (X2-X1.ne.0) then
        roadangle=ATAN((Y2-Y1)/(X2-X1))*(1/RAD)
      else
        roadangle=90.
      endif
      if (roadangle.lt.0) then
!MSK       roadangle = 360. + roadangle
        roadangle = 180. + roadangle
      endif

!MSK Calculate the angle of the wind direction with respect to the street axis, theta_street
!MSK This angle (theta_street) can go from 1 to 179 degree.
      theta_street = ABS(DD-roadangle)      
      if (theta_street.ge.180.) then
        theta_street = theta_street - 180.
      else if (theta_street.ge.90.) then
        theta_street = 180.-theta_street
      endif
!MSK avoid theta_street of 90.0
      if ((theta_street.ge.89.9).and.(theta_street.le.90.1)) then
        theta_street = 89.9
      endif
!MSK theta_street must not equal 180.0
      if (theta_street.eq.180.) then
        theta_street = 179.0
      endif
!MSK 13.08.2018, theta_street must not be zero either
      theta_street = MAX(1.0, theta_street)

!MSK Maximum distance for street canyon receptor
!MSK      wdist = RMINRV+WIDTH/2.+2.0
!MSK increased by 2m
      wdist = RMINRV+WIDTH/2. + 4.0

!MSK Street canyon receptor
      leeward = .false.
      winward = .false.

      
!MSK New lower limit for wind speed for line source calculation (13.06.2017)
!MSK 13.08.2018
!MSK Wind speed at receptor level corrected by the air drag correction
      ucr   = c0 * (FF**uprop) * max(0.0,COS(theta_street*RAD)) * max(0.0,COS(theta_street*RAD))
      FF    = max(FF,ucr)


!MSK calculate street canyon parameters
      if (canyon) then
!MSK canyon width = 2 times street width
!MSK        wcanyon    = WIDTH + 2.0
        wcanyon = 2* WIDTH
!MSK 20.04.2018 test        wcanyon = 3* WIDTH

!MSK distance from receptor to end of street (maximum 2*wcanyon)
        lstreet    = min(0.5*QLLV,2*wcanyon)
!MSK wind speed at street level (Berkowicz et al. 1997)
        ff_street  = UE * LOG(h0/Z0_LSRC) / LOG(building_height/Z0_LSRC)
!MSK        ff_street  = ff_street * (1-0.2*SIN(theta_street*RAD))
!MSK 23.08.2018 adjusting dependence on theta_street
        ff_street  = ff_street * (1-0.5*SIN(theta_street*RAD))

!MSK 20.08.2018: FF and ff_street are independent estimates of the wind speed
!MSK             at receptor/street level. Now we use the smaller of FF and 
!MSK             ff_street for wind speed at street level.
        ff_street  = min(FF,ff_street)


!MSK maximum integration path (Ottosen et al., GMD 2015; Table 4)
        if (theta_street.ge.45.0) then
          lmax     = wcanyon/SIN(theta_street*RAD) 
        else
          lmax     = lstreet/COS(theta_street*RAD)
        endif
!MSK set a minimum lmax
        lmax = MAX(lmax,WIDTH)

      else
        wcanyon    = WIDTH
        ff_street  = UE
        sigma_velo = 0.1
        sigma_vent = 0.1
        lmax       = WIDTH
        xesc       = 2*WIDTH
      endif
!MSK end

!MSK start 
! *** Length of the current line (road link) between
!     start coordinate (X1,Y1) and end coordinate (X2,Y2), in km.
!     It is now used as roadlink length for emission calculation

      roadlink_length = QLL(IQL)
      if (roadlink_length == 0.0)  roadlink_length=1.0

      !write(6,*) 'csubl',XRV,YRV,DMV,DM,wdist,canyon,roadlink_length


!MSK end

! If line source is upwind of receptor point then return

!MSK start
!MSK Changes for Street Canyon treatment
!MSK Distance of receptor from line source is only criterion
!MSK for identification as street canyon receptor
!MSK Special rule for STATIONS?

!MSK    IF (XW1 .GE. 0. .AND. XW2 .GE. 0.) GOTO 999
      if (XW1 .GE. 0. .AND. XW2 .GE. 0.) then
!MSK jump to Street Canyon model if conditions fulfilled:
!MSK    (1) DMV is less than canyon width
!MSK    (2) roadlink_length is greater than 8.0 m
!MSK leeward receptor in street canyon
!MSK upwind wind-direction in recirculation zone is mirrored
          if (canyon.and.(DMV.le.wdist).and.(roadlink_length.gt.8.0)) then
            leeward=.true.
            GOTO 600
          else
            GOTO 999
          endif
      endif  

!MSK jump to Street Canyon model if conditions fulfilled
!MSK 14.08.2018 Leeward case always when XW1+XW2 greater zero
      if (canyon .and. (DMV.le.wdist).and.(roadlink_length.gt.8.0)) then
         if ((XW1+XW2) .GT. 0.) then
           leeward=.true.
           GOTO 600
         else
!MSK windward receptor in street canyon 
           winward=.true.
           GOTO 600
         endif
      endif
!MSK end

      THETA = DD

! Get stability class and height of limiting lid

!_LHS_Change_11Jan2012_Start:

!      IF (STAB(IXL,IYL) .EQ. 1 .OR. STAB(IXL,IYL) .EQ. 2) THEN
      IF (STAB(IXL,IYL) .EQ. 1) THEN
! ***     Unstable conditions:
          KST = 3
      ELSEIF (STAB(IXL,IYL) .EQ. 2) THEN
! ***     Neutral conditions:
          KST = 4
      ELSEIF (STAB(IXL,IYL) .EQ. 3 .OR. STAB(IXL,IYL) .EQ. 4) THEN
! ***     Stable conditions:
          KST = 5
      ELSE
          if(messfe) then 
            write (messun,'(A)')        &
           'CSUBL:  STAB(I,J) is not containing valid values. '
            write (messun,'(A)')        &
           'CSUBL:  NEUTRAL conditions assumed in the LSRC model. '
          endif
          KST = 4
      ENDIF

!MSK Mixing height HL
      HL = HMIX(IXL,IYL)

      if(HL <= 0.0)then
        if(messfe)then 
          write (messun,'(A,E12.5)') 'CSUBL: UM mixing height HL = ',HL
          write (messun,'(A)') 'CSUBL: PROGRAM TERMINATES! '
        endif
        CALL STOPIT('CSUBL: negative or zero mixing height!')
      elseif(HL > 0.0 .AND. HL < DZ(1))then
        if(messfe)then 
          write (messun,'(A,E12.5)') 'CSUBL: WARNING, The applied mixing height HL = ',HL
        endif      
      endif

!_LHS_Change_11Jan2012_End.

! Define some parameters for current line source

!MSK Set release height H to zero
      H = 0.

! *** convert to km
      REP1 = X1/1000.
      SEP1 = Y1/1000.
      REP2 = X2/1000.
      SEP2 = Y2/1000.

      RA = REP1
      RB = REP2
      SA = SEP1
      SB = SEP2
      DELR = RB - RA
      DELS = SB - SA
      DIST = SQRT(DELS*DELS + DELR*DELR)

      WL = WIDTH/NQLL
      DL = -0.5*WL
      DUM = DL*0.001/DIST
      RAQ(1) = RA + DELS*DUM
      RBQ(1) = RB + DELS*DUM
      SAQ(1) = SA - DELR*DUM
      SBQ(1) = SB - DELR*DUM
      DL = 0.5*WL
      DUM = DL*0.001/DIST
      RAQ(2) = RA + DELS*DUM
      RBQ(2) = RB + DELS*DUM
      SAQ(2) = SA - DELR*DUM
      SBQ(2) = SB - DELR*DUM

      WROAD = QLW(IQL)
      WINFL = 1.0
      SIGW0 = QLSW0(IQL)
      SIGY0 = 1.5
      SIGVE = SIGV(IXL,IYL,IZL)
      SIGWE = SIGW(IXL,IYL,IZL)
      TLGRE = TLGR(IXL,IYL,IZL)


! Set unit emission on the lanes (1 g/s*m)
! NQLL: number of lanes. Set to 1

      DO 100 IQLL = 1,NQLL
          QLN(IQLL) = 1.
  100 CONTINUE


! Call HIWAY-2 routine to integrate concentration from line source to
! receptor point
      
      call hwylne(XRV,YRV,ZRV,MQLL,NQLL,QLN,RAQ,SAQ,RBQ,SBQ,WROAD,          &
                 WINFL,THETA,UE,KST,HL,SIGY0,SIGVE,SIGW0,SIGWE,       &
                 TLGRE,building_height,canyon,CRVV)

! Go through all compounds

!MSK start
      do IC = 1,NCV
         if (CMPND(IC)(1:2) == 'O3')  CMO3 = DBLE( cm(IXL,IYL,IC) )
      enddo
!MSK end

      DO 110 IC = 1,NCV

! Set actual emission on the lanes [g/(s*m)]

          SCALEQL = 1.0

          !ALLOWS FOR PARTICLE NUMBER EMISSIONS BY CONVERTING FROM UG/M3 TO NUM/CM3
          !GRAMS TO UGRAMS IS ALREADY DONE IN HWYLINE SO CONVERTING BACK IS 10E-6
          !CONVERSION FROM M3 TO CM3 IS ALSO A FACTOR OF 10E-6 TOTAL 10e-6
          ! line source emissions for pnc are in num/(m*s)

!MSK start PNC treatment
          if ( CUNIT(IC)(1:3) == 'num' ) then
          
              SCALEQL=1.0E-12
              roadlink_length = 1.0

!MSK 18.10.2017 PNC-Loss by Deposition
! Calculate advection time from start of plume segment to receptor point
              TADD  = DM/UE
! Calculate wet scavenging coefficient
              LAMBV = PREC(IXL,IYL)*WDEPSR(IC)/HL
! Calculate dry deposition velocity (m/s) at 2m
              VDV   = DDEPV(IC)/(DDEPV(IC)*AERO(IXL,IYL) + 1.)

! Standard treatment for open roads < 300m (X in km)
              XD    = MIN(DM,300.)
              IF (KST.LE.3) SIGZA=110.62*((XD*0.001)**0.93198)
              IF (KST.EQ.4) SIGZA=86.49 *((XD*0.001)**0.92332)
              IF (KST.GE.5) SIGZA=61.141*((XD*0.001)**0.91465)
              SIGZO = 5.0
              SIGZ  = SQRT(SIGZA*SIGZA+SIGZO*SIGZO)

! Calculate derivative of sigma-z with respect to x at receptor point
              TL4   = 4.* TLGRE
              TZ2   = (SIGZ/SIGWE)*(SIGZ/SIGWE)
              TNUL  = TZ2/TL4 + SQRT(TZ2*TZ2/(TL4*TL4) + TZ2)
              TNEW  = TNUL + TADD
! Limit advection time to time step DT
              TNEW  = MIN(DT,TNEW)

              !write(6,*) 'plume',KST,SIGZA,SIGZ,TNUL,TNEW

          endif
!MSK end

!----------------------------------------------------------------------------------
! Reduce the available NO from receptor point based on distance and wind.
! This paramterises the conversion of NO to NO2 on the given time scale
! Only works if the compund index is correctly given, i.e. NO=6 and n_comp=3
! THis will give the wrong answer for NO but hoepfully a better answer for NO2
!MSK I have set use_timescale_no to .false. in mod_lsrc
!MSK          use_timescale_no = .true.
          MW_O3_no = 48.0
          AVOG_no = 6.023E+23   
          init_timescale_no=60  !initial time scale for chemistry.

          !Assumes that compound IC = 1 is NO2,IC = 2 is NO and IC = 3 is O3
          !Also assumes the compund ID for NO to be 6, as was before (not new AirQUIS)
          !Decide if to do NO reduction if the compound is number 2 (NO), the number of compounds is 3,
          !the compound identifier is 6 and the use_timescale_no flag is true
!MSK          if (IC.EQ.2 .and. NCV.EQ.3 .and. ICMPND(IC).eq.6 .and. use_timescale_no) then
          if ( (CMPND(IC) == 'NO        ') .and. (PHOTPS.ge.1) .and. use_timescale_no) then


            if (X2-X1.ne.0) then
!MSK                theta_no=atand((Y2-Y1)/(X2-X1)) !Road angle
                   theta_no=ATAN((Y2-Y1)/(X2-X1))*(1/RAD)     !Road angle
            else
                theta_no=90
            endif
            beta_no=90-(theta_no+THETA)!Angle of wind (THETA) relative to road (theta_no). 0 is parallel 
!MSK            dist_no=min(10000.0,DMV/max(0.0001,abs(sind(beta_no))))!Max distance 10 km, min sin 0.0001
            dist_no=min(10000.0,DMV/max(0.0001,abs(SIN(beta_no))))!Max distance 10 km, min sin 0.0001
            time_no=dist_no/UE+init_timescale_no !UE must never be 0, which it isn't
            TAIRV_no = TAIR(IXL,IYL) + 273.13
!MSK start
!MSK            O3V_no  = cm(IXL,IYL,3)*(AVOG_no*1.0E-12)/MW_O3_no !Grid ozone num molecules
            O3V_no  = CMO3 * (AVOG_no*1.0E-12)/MW_O3_no !Grid ozone num molecules
!MSK use EXP4() as exponential function instead of exp()
!MSK            RK1_no = 1.4D-12*EXP(-1310./TAIRV_no)!Reaction rate
            RK1_no = 1.4D-12*EXP4(-1310./TAIRV_no)!Reaction rate
!            print *,'csubl tau_no',ic,rk1_no,o3v_no
            O3V_no = max(O3V_no, dble(1.e5))
            tau_no=1.0/(RK1_no*O3V_no)     !time scale

!MSK            SCALEQL=(1-exp(-time_no/tau_no))    !NO scaling parameter
            SCALEQL=(1 - EXP4(-time_no/real(tau_no)))    !NO scaling parameter
!MSK end
            !write(*,*) DMV,dist_no,UE,tau_no,SCALEQL
          endif
!----------------------------------------------------------------------------------
          
          CSUM = 0.
          DO 120 IQLL = 1,NQLL

!MSK              CSUM = CSUM + QL(IC,IQL,IQLL)*CRVV(IQLL)*SCALEQL
              CSUM = CSUM + (QL(IC,IQL,IQLL)/roadlink_length)  &
                     * CRVV(IQLL) * SCALEQL

  120     CONTINUE

!MSK start 18.10.2017 PNC treatment
          if ( CUNIT(IC)(1:3) == 'num' ) then

! Coagulation of particles (only intramodal)
              if (CSUM.lt.1.e-10)   CSUM=1.e-10
! if C0 is zero, there will be no coagulation loss
              K_COAG = THALF(IC)*CSUM
              CSUM   = CSUM*EXP4(-DT*K_COAG)
! Wet deposition of particles
! if precip. is zero, there will be no wet removal
              CSUM  = CSUM*EXP4(-LAMBV*TADD)              
! Dry deposition of particles
              QDV   = VDV / SIGZ
              CSUM  = CSUM*EXP4(-QDV*TNEW)

          endif
!MSK end 18.10.2017

!MSK start debug
!MSK debug: Check receptor point NO
!MSK debug: TLGRE is the (grid) 3-D Lagrangian time scale
!MSK debug        if  ((IQL==226).and.(IC==2)) then
!MSK debug           print *,'csubl crv', XRV,YRV,TLGRE, SCALEQL
!MSK debug           print *,'csubl crv NO ',roadlink_length, QL(2,IQL,1), QL(2,IQL,1)/roadlink_length, SCALEQL, &
!MSK debug                                     (QL(2,IQL,1)/roadlink_length)*SCALEQL, CSUM
!MSK debug        endif
!MSK debug        if  ((IQL==226).and.(IC==8)) then
!MSK debug            print *,'csubl crv CO ', roadlink_length, QL(8,IQL,1), QL(8,IQL,1)/roadlink_length, SCALEQL, &
!MSK debug                                      (QL(8,IQL,1)/roadlink_length)*SCALEQL, CSUM
!MSK debug        endif
!MSK end debug

! Set concentration and dry and wet depositions in the given receptor
! point

           CRV(IC) = CSUM

!MSK start debug
!MSK debug: Testing the effect of turning off the LSRC Subgrid model, i.e. only apply GRID model:
!MSK debug
!MSK debug           CRV(IC) = 0.0
!MSK debug
!MSK end debug

          DDRV(IC) = 0.
          WDRV(IC) = 0.
          
          !BRUCE: Should put in PNC condensation here using wind speed (FF) and min distance (DMV) for the aging      real             :
          !Not exactly right but OK for now.

  110 CONTINUE


!MSK start
      RETURN


  600 CONTINUE

!MSK *******  Street Canyon Model with Simple Geometry     *******

!MSK vertical velocity fluctuation (Berkowicz et al. 1997)
!MSK set alpha to default 0.1
      sigma_velo = SQRT( (0.1*ff_street)*(0.1*ff_street) + (sigma_w0)**2)
      
!MSK ventilation velocity  (Berkowicz et al. 1997)
!MSK set alpha_roof to default 0.1
      sigma_vent = SQRT( (0.1*UE)*(0.1*UE) + 0.4*(sigma_w0)**2 ) 
!MSK Would be good to know which values sigma_vent can take in reality
!MSK scale sigma_vent by factor 2
      sigma_vent = 2.0* sigma_vent
      sigma_vent = max(sigma_vent,sigma_w0)

!MSK *******   Recirculation Contribution (Csrec)   *******

!MSK Geometry of the recirculation zone (trapezium)
      if (ff_street.lt.2.0) then
         lrec = 2.0*building_height*SQRT(0.5*ff_street)
      else
         lrec = 2.0*building_height
      endif
      lrec   = min(lrec,wcanyon)
      lbase  = min(lrec,lmax)
      ltop   = 0.5*lbase
      lhyp   = SQRT((0.5*lbase)*(0.5*lbase)+(building_height)**2)

!MSK I use sigma_vent instead of sigma_hyp
      lratio = lbase/(sigma_vent*ltop+sigma_vent*lhyp)

!MSK limit lratio
!MSK Set to 1.4
      lratio = MIN(lratio,1.4)


!MSK *******   Direct Contribution (Csdir)          *******

!MSK distance of plume escape through canyon top (Ottosen et al., GMD 2015)
!MSk AVOID TOO SMALL xesc
      xesc       = ff_street*(building_height-h0)/sigma_velo

       !print *,'distances:',lrec,lmax,xesc

!MSK Limits of the integration path for the direct contribution
      xstart = 0
      extend = .false.
      if ((lrec.gt.xesc).and.(xesc.gt.lmax)) then
        xend   = lmax
      elseif ((lrec.gt.lmax).and.(lmax.gt.xesc)) then
        xend   = xesc
        xend2  = lmax
        extend = .true.
      elseif ((xesc.gt.lrec).and.(lrec.gt.lmax)) then
        xend   = lmax
      elseif ((xesc.gt.lmax).and.(lmax.gt.lrec)) then
        xend   = lrec
      elseif ((lmax.gt.xesc).and.(xesc.gt.lrec)) then
        xend   = lrec
      elseif ((lmax.gt.lrec).and.(lrec.gt.xesc)) then
        xend   = xesc
        xend2  = lrec
        extend = .true.
      else
        xend   = wcanyon
      endif

!MSK For windward receptor the integration path is canyon width
!MSK minus recirculation zone length, because only contribution
!MSK from outside the recirculation zone is counted
      if (winward) then
        xend = wcanyon - lrec
!MSK xend bounded between 2m and 10m
        xend = MIN(10.0,xend)
        xend = MAX(2.0,xend)
      endif

!MSK Contribution outside recirculation zone (windward receptor) multiplied
!MSK by fext defined according to Eq.(6) in Ottosen et al.(2015).
!MSK 13.08.2018 Corrected fext to depend on theta_street and lower limit of zero
      if (ff_street.lt.2.0) then
        fext = MAX( 0.0, cos(2.0*theta_street*RAD*SQRT(0.5*ff_street)) )
      else
        fext = MAX( 0.0, cos(2.0*theta_street*RAD) )
      endif

!MSK Set up integral for the direct contribution from xtart to xend
      const1 = ff_street*h0/sigma_velo


!MSK Integration of the direct contribution along the straight path
!MSK between receptor point and the traffic line soure until xesc.
!MSK Integral of the form [1/(x+b)]dx is ln|x+b|

      integdx = LOG(ABS(xend+const1)) - LOG(ABS(xstart+const1))

      ! print *,'street', IQL,DD,roadangle,theta_street,lrec,lmax,lbase,lhyp
      ! print *,'integx', IQL,xstart,xend,ff_street,const1,integdx

!MSK limit integdx
!MSK Set to 1.4
      integdx = MIN(integdx,1.4)

!MSK When xesc is exceeded the additional contribution follows
!MSK an exponential decay by the ventilation rate through the
!MSK street canyon until xend2.
!MSK Set up integral for the direct contribution from xesc to xend2

      if (extend.and.leeward) then

        const2   = (-1.)*sigma_vent/(building_height*ff_street)
        integ2dx = ABS((1/const2)*(EXP4(const2*xend2) - EXP4(const2*xesc)))

!MSK limit integdx to 1
        integ2dx = MIN(integ2dx,1.0)
        ! print *,'after extra integration:',const2,xesc,xend2,integ2dx

      else
        integ2dx = 0.0
      endif


!MSK Go through all compounds
      DO 610 IC = 1,NCV

! Set actual emission on the lanes [g/(s*m)]
!MSK emission Q in [g/(s*m)] = QL/roadlink_length

        SCALEQL = 1.0
        if ( CUNIT(IC)(1:3) == 'num' ) then
          SCALEQL=1.0E-12
          roadlink_length = 1.0 ! PN emission is in g/(s*m)
        endif

!MSK *******   Recirculation Contribution (Csrec)   *******

        CSREC = 0.0

        if (leeward) then

          DO 620 IQLL = 1,NQLL

!MSK *** CSREC in g/m^3
              CSREC = CSREC + (QL(IC,IQL,IQLL)/roadlink_length)    &
                     * (1/wcanyon)* lratio                         &
                     * SCALEQL

  620     CONTINUE


        elseif (winward) then

          if (lrec.eq.wcanyon) then
!MSK recirculation extends through whole canyon (lrec cannot be > wcanyon)
!MSK then the windward receptor receives a diluted recirculation contr.
!MSK the dilution rate is taken as sigma_velo/H_building

             DO 630 IQLL = 1,NQLL

!MSK *** CSREC in g/m^3
                CSREC = CSREC + (QL(IC,IQL,IQLL)/roadlink_length)  &
                     *(1/wcanyon)* lratio                          &
!MSK 22.08.2018 dilution of recirculation contribution
                     * EXP4((-1.)*sigma_vent/building_height)      &
                     * SCALEQL

  630        CONTINUE

          else
!MSK windward receptor outside recirculation zone

             CSREC = 0.0

          endif

        endif

!!!MSK TEST REMOVE CSREC
!!!        CSREC = 0.0


!MSK *******   Direct Contribution (Csdir)          *******

        CSDIR = 0.0

        if (leeward) then

          DO 640 IQLL = 1,NQLL

!MSK *** CSDIR in g/m^3
              CSDIR = CSDIR + (QL(IC,IQL,IQLL)/roadlink_length)                   &
                     * (1/(wcanyon*sigma_velo)) * real(SQRT(2/pi)) * integdx      &
!MSK 14.08.2018 multiplication with fext
                     * fext                                                       &
                     * SCALEQL

!MSK if extending beyond xesc (long canyon), extra contribution to be added
              if (extend) then
                CSDIR = CSDIR + (QL(IC,IQL,IQLL)/roadlink_length)                 &
                       * (1/(wcanyon*building_height*ff_street))                  &
                       * EXP4(sigma_vent/(building_height*ff_street)) * xesc      &
                       * real(SQRT(2/pi)) * integ2dx                              &
                       * SCALEQL

              endif

  640     CONTINUE


        elseif (winward) then

          if ((lrec.lt.wcanyon).and.(xend.gt.0.)) then

! INTEGRATION PATH IS WSC-LREC
             DO 650 IQLL = 1,NQLL

!MSK *** CSDIR in g/m^3
                CSDIR = CSDIR + (QL(IC,IQL,IQLL)/roadlink_length)                 &
                     * (1/(wcanyon*sigma_velo)) * real(SQRT(2/pi)) *integdx       &
                     * SCALEQL                                                    &
!MSK 04.12.2017: tuning the direct contribution
!MSK 23.04.2018      * 0.36
                     * 1.00                                                       &
!MSK 22.08.2018 dilution of direct contribution
                     * EXP4((-1.)*sigma_vent/building_height)


  650        CONTINUE

          else
!MSK if recirculation zone extends through whole canyon
!MSK no direct contribution to windward receptor

             CSDIR = 0.0

          endif
        
        endif


!MSK ******* Add to receptor concentration          *******

!MSK *** CSREC in ug/m^3
        CSREC = CSREC * 1.E06

!MSK *** CSDIR in ug/m^3
        CSDIR = CSDIR * 1.E06

!MSK *** Concentration in the street canyon receptor point

        CRV(IC) = CSREC + CSDIR


!MSK debug
!       if ( (IC==20) ) then
!         if ( (XRV.gt.568430 ) .and. (XRV.lt.568530 ) ) then
!          print *,'CS receptor',XRV,YRV,IQL,LANU(IXL,IYL),leeward,winward,extend,DD,theta_street,wcanyon,sigma_vent,sigma_velo
!          print *,'           ',integdx,fext,integ2dx,lrec,xend,ff_street,UE,roadlink_length,roadangle
!          print *,'           ',CSDIR,lratio,CSREC
!         endif
!       endif

        DDRV(IC) = 0.
        WDRV(IC) = 0.

  610 CONTINUE


!MSK end


      RETURN

  999 CONTINUE

! Set concentration and deposition values to zero and return

      DO 130 IC = 1,NCV

          CRV(IC) = 0.
         DDRV(IC) = 0.
         WDRV(IC) = 0.

  130 CONTINUE

! Deallocate memory

      IF (ALLOCATED(CRVV)) DEALLOCATE(CRVV)
      IF (ALLOCATED(QLN))  DEALLOCATE(QLN)
      IF (ALLOCATED(RAQ))  DEALLOCATE(RAQ)
      IF (ALLOCATED(RBQ))  DEALLOCATE(RBQ)
      IF (ALLOCATED(SAQ))  DEALLOCATE(SAQ)
      IF (ALLOCATED(SBQ))  DEALLOCATE(SBQ)

      RETURN

 2000 FORMAT('CSUBL: Perturbed receptor point ',2F10.1,       &
            ' Min. dist. from line source nr. ',I7,       &
            ' is changed from ',F10.3,' m',' to ',F10.3,' m')

! End of subroutine CSUBL

      end subroutine csubl
