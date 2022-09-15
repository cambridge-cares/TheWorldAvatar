! <lsgrid.f90 - A component of the City-scale
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

      subroutine LSGRID(I)

! *** The subroutine calculates integrated line source concentrations to be
! *** used by the grid model.
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
!           2016  M. Karl: SCALEQL for unit conversion of particle numbers
!
! ----------------------------------------------------------------------------------

      use mod_util
      use mod_main
      use mod_site
      use mod_time
      use mod_mete
      use mod_conc
      use mod_grid
      use mod_lsrc

      implicit none

      INTEGER I

! *** I - Indicator of pre (0) or post (1) processing

! *** Local variables:

      REAL ACPSI
      REAL ATPSI

      REAL,ALLOCATABLE :: CGRIDV(:,:,:,:)
      REAL,ALLOCATABLE :: CRV(:)
      REAL,ALLOCATABLE :: CSUM(:)
      REAL DD
      REAL,ALLOCATABLE :: DDRV(:)
      REAL DELX
      REAL DL
      REAL DNL
      REAL FF
      REAL,ALLOCATABLE :: MASSV(:)
      REAL PSI
      REAL QLLV
      REAL QLXV
      REAL QLYV
      REAL,ALLOCATABLE :: QMV(:)
      REAL RMAXRV
      REAL RMAXJV
      REAL SV
      REAL THETA
      REAL TMAXJV
      REAL TTOT
      REAL UL
      REAL UU
      REAL VL
      REAL VV
      REAL,ALLOCATABLE :: WDRV(:)
      REAL WV
      REAL XRV
      REAL YRV
      REAL ZRV
      REAL XLCV
      REAL XLJV
      REAL XV(1:4)
      REAL XW(1:4)
      REAL X1
      REAL X2
      REAL YV(1:4)
      REAL YW(1:4)
      REAL Y1
      REAL Y2
      REAL ZV(1:4)
      REAL Z1
      REAL Z2
!_LHS_Bedre-Byluft_January-2010_Start:
      REAL WINDHEIGHT
      REAL rsum_DCDT
      REAL rmax_DCDT
!_LHS_Bedre-Byluft_January-2010_End.
      REAL SCALEQL !BRUCE

!_LHS_Change_24April2012_Start:
      integer siteidlen
      integer cmpndlen
!_LHS_Change_24April2012_End.

      INTEGER IC
      INTEGER IJV
      INTEGER IQL
      INTEGER IQLL
      INTEGER IV
      INTEGER IX
      INTEGER IXMAX
      INTEGER IXMIN
      INTEGER IXV(1:4)
      INTEGER IY
      INTEGER IYMAX
      INTEGER IYMIN
      INTEGER IYV(1:4)
      INTEGER IZ
      INTEGER IZMAX
      INTEGER IZMIN
      INTEGER IZV(1:4)
      INTEGER NJV

      integer iloc_4D(4)
      integer ii
      integer unit_len

!MSK start
      integer :: ixv0,iyv0,izv0
      real    :: xv0,yv0,zv0
      real    :: roadlink_length
!MSK end
      
!MSK start
! *** The subroutine calculates the removal/increase factor to be used
! *** when transferring mass from the subgrid scale line source model
! *** to the grid model. Default value is 1.0 (no change).
      real, parameter  :: l2a = 1.0
! *** Introduced a source scaling factor for NO as in csubl.f90
! *** This is very roughly estimated for each grid cell with lines
      real             :: scale_qno
      real             :: time_no
      real             :: dist_no
      real             :: beta_no
      real             :: UU_no
      real             :: VV_no
      real             :: THETA_no
      real             :: TAIRV_no
      double precision :: tau_no
      double precision :: cmo3
      double precision :: O3V_no
      double precision :: RK1_no
      double precision,parameter :: MW_O3_no= 48.0
      double precision,parameter :: AVOG_no = 6.023E+23
      real, parameter  :: UE_no             = 2.0
      real, parameter  :: init_timescale_no = 60
!MSK end

! ACPSI  - Absolute value of cosine  of angle
! ATPSI  - Absolute value of tangent of angle
! CGRIDV - Grid model concentrations
! CRV    - Receptor  concentrations
! CSUM   - Sum of concentrations
! DD     - Wind direction
! DELX   - Line source delta-x
! DL     - Line source direction
! DNL    - Line source normal direction
! DDRV   - Receptor dry depositions
! FF     - Wind speed
! MASSV  - Mass emitted during influence time
! QLLV   - Line source length value
! QLXV   - Line source x-coordinate
! QLYV   - Line source y-coordinate
! QMV    - Emission intensity
! RMAXRV - Line source maximum influence distance
! RMAXJV - Line source segment influence distance
! SV     - Sign value
! THETA  - Coordinate system rotation angle
! TMAXJV - Line source segment influence time
! TTOT   - Total time during current hour
! UL     - Line source x-component
! VL     - Line source y-component
! UU     - Wind u-component
! VV     - Wind v-component
! WDRV   - Receptor wet depositions
! WV     - Line source segment position weight
! XRV    - Receptor x-coordinate
! YRV    - Receptor y-coordinate
! ZRV    - Receptor z-coordinate
! XV     - Line source influence zone x-coordinates
! XW     - Line source influence zone x-coordinates transformed
! X1     - Line source end point x-coordinate
! X2     - Line source end point x-coordinate
! YV     - Line source influence zone y-coordinates
! YW     - Line source influence zone y-coordinates transformed
! Y1     - Line source end point y-coordinate
! Y2     - Line source end point y-coordinate
! ZV     - Line source influence zone z-coordinates
! Z1     - Line source end point y-coordinate
! Z2     - Line source end point y-coordinate
! IC     - Compound index
! IJV    - Line source segment index
! IQL    - Line source index
! IQLL   - Line source lane index
! IV     - Corner coordinates index
! IX     - Main grid cell index in x-direction
! IXMAX  - Main grid cell index in x-direction maximum
! IXMIN  - Main grid cell index in x-direction minimum
! IXV    - Main grid cell indices in x-direction
! IY     - Main grid cell index in y-direction
! IYMAX  - Main grid cell index in y-direction maximum
! IYMIN  - Main grid cell index in y-direction minimum
! IYV    - Main grid cell indices in y-direction
! IZ     - Main grid cell index in z-direction
! IZMAX  - Main grid cell index in z-direction maximum
! IZMIN  - Main grid cell index in z-direction minimum
! IZV    - Main grid cell indices in z-direction
! NJV    - Number of line source segments

! Function type declarations

!MSK      logical ATTIME
!MSK      REAL CWDIR
!MSK      REAL L2A   !only sugrid, default l2a = 1.0

! Allocate memory

      IF (.NOT. ALLOCATED(CGRIDV)) ALLOCATE(CGRIDV(NC,NX,NY,NZ))
      IF (.NOT. ALLOCATED(   CRV)) ALLOCATE(CRV(NC))
      IF (.NOT. ALLOCATED(  CSUM)) ALLOCATE(CSUM(NC))
      IF (.NOT. ALLOCATED(  DDRV)) ALLOCATE(DDRV(NC))
      IF (.NOT. ALLOCATED( MASSV)) ALLOCATE(MASSV(NC))
      IF (.NOT. ALLOCATED(   QMV)) ALLOCATE(QMV(NC))
      IF (.NOT. ALLOCATED(  WDRV)) ALLOCATE(WDRV(NC))

! Calculate total time within current simulation period (s)

      TTOT = ITS*DT
      

      if (messfe .AND. ATTIME(BDAT) .AND. ISH == 1 .AND. ITS == 1) then
        siteidlen = LEN_TRIM(siteid)  
        write(messun,*)
        write(messun,'(3A)') 'LSGRID: The Applied L2A function defined for the present ',       &
                                     siteid(1:siteidlen),' is: '
        write(messun,*)
        do IC = 1,NC
          cmpndlen  = LEN_TRIM(cmpnd(IC))
          write(messun,'(A,I3,3A,F8.3)') 'LSGRID: For IC = ',IC,' i.e: ',        &
                     cmpnd(IC)(1:cmpndlen),       &
!MSK                      '  L2A = ',L2A(siteid,cmpnd(IC))
                      '  L2A = ',l2a
        enddo
      endif


! Go through all line sources

!_lhs_LSGRID_reduction_START_June_2011:
      if (ITS == 1) then
      
      lsrc_dcdt = 0.0
      
 
!_lhs_LSGRID_reduction_END_June_2011.
     
      DO 100 IQL = 1,NQL

! Initialize average maximum influence time for the line source
! to zero

          QLTMAX(IQL) = 0.

! Get line source end coordinates

          X1 = QLX(IQL,1)
          Y1 = QLY(IQL,1)
          Z1 = QLZ(IQL,1)
          X2 = QLX(IQL,2)
          Y2 = QLY(IQL,2)
          Z2 = QLZ(IQL,2)

! Get line source length

          QLLV = QLL(IQL)
!MSK start
          roadlink_length = QLL(IQL)
          if (roadlink_length == 0.0)  roadlink_length=1.0
          !print *,'iql, roadlink m ', iql, roadlink_length
!MSk end

! Get line source maximum influence distance

          RMAXRV = QLRMAX(IQL)

! Calculate line source direction

          UL = X2 - X1
          VL = Y2 - Y1
          DL = CWDIR(UL,VL)

! Calculate line source midpoint coordinates

!MSK          XV(0) = (X1 + X2)/2.
!MSK          YV(0) = (Y1 + Y2)/2.
!MSK          ZV(0) = (Z1 + Z2)/2.
          xv0 = (X1 + X2)/2.
          yv0 = (Y1 + Y2)/2.
          zv0 = (Z1 + Z2)/2.

! Get line source midpoint main grid indices

          CALL GETMGI(1,xv0, yv0, zv0, ixv0, iyv0, izv0 )
 
         ! print *,'after getmgi: ',xv0, yv0, zv0,ixv0,iyv0,izv0


! Get line source midpoint windspeed and direction

!MSK          UU = U(IXV(0),IYV(0),IZV(0))
!MSK          VV = V(IXV(0),IYV(0),IZV(0))
          UU = U(ixv0,iyv0,izv0)
          VV = V(ixv0,iyv0,izv0)
          FF = SQRT(UU*UU + VV*VV)
          DD = CWDIR(UU,VV)

! Calculate winddirection in the line source oriented system

          PSI = DL - DD
          IF (PSI .LT. 0.) PSI = PSI + 360.

! Calculate direction indicator

          SV = 1.
          IF (PSI .GT. 90. .AND. PSI .LT. 270.) SV = -1.

          PSI = PSI*RAD

! Calculate number of line segments

          NJV = INT(2.*QLLV/MIN(DX,DY)) + 1

! Go through all compounds

          DO 110 IC = 1,NC

! Calculate emission intensity for one line segment (g/s)

!MSK start
! QL(IC,IQL,IQLL) should be in g/(s*m) but given in g/s
! Therefore QL needs to be divided by roadlink_length
! QLLV is line source length in m
! NJV is number of line segments
! QMV is line segment source strength in g/s

            if ( CUNIT(IC)(1:3) == 'num' ) then
                roadlink_length = 1.0
            endif
            !print *,'lsgrid',X1,X2,Y1,Y2,roadlink_length

            QMV(IC) = 0.
            DO 120 IQLL = 1,NQLL
!MSK               QMV(IC) = QMV(IC) + QL(IC,IQL,IQLL)*(QLLV/NJV)
                QMV(IC) = QMV(IC) + (QL(IC,IQL,IQLL)/roadlink_length) &
                        * (QLLV/NJV)
  120       CONTINUE
!MSK end

! Initialise total mass emitted during the influence time to zero

          MASSV(IC) = 0.

! Next compound

  110     CONTINUE

! Go through all line source segments

          DO 130 IJV = 1,NJV

! Calculate weight value

              WV = REAL(IJV - 0.5)/REAL(NJV)

! Calculate segment midpoint coordinates

              QLXV = (1. - WV)*X1 + WV*X2
              QLYV = (1. - WV)*Y1 + WV*Y2

              XLJV = WV*QLLV

! Calculate position where emissions crosses the influence
! boundary

              ATPSI = ABS(TAN(PSI))

              IF (ATPSI .EQ. 0.) THEN

                  IF (SV .LT. 0) XLCV = -RMAXRV
                  IF (SV .GT. 0) XLCV = QLLV + RMAXRV

              ELSE

                  DELX = RMAXRV/ATPSI
                  XLCV = XLJV + SV*DELX
                  XLCV = MAX(XLCV,-RMAXRV)
                  XLCV = MIN(XLCV,QLLV + RMAXRV)

              ENDIF

! Calculate correct delta-x

              DELX = ABS(XLCV - XLJV)

! Calculate maximum influence distance for current line segment (m)

              ACPSI = ABS(COS(PSI))

              IF (ACPSI .EQ. 0.) THEN
                  RMAXJV = RMAXRV
              ELSE
                  RMAXJV = DELX/ACPSI
              ENDIF

!_LHS_Change_19Jan2012_Start:
!
! ***         Removing the preprocessor directive ("linesource_changes")
! ***         overriding of the TMAXJV specification. Instead the TMAXJV
! ***         variable is always set to zero below:

! ***         Calculate maximum influence time for current line segment (s)

!              TMAXJV = MIN(RMAXJV/FF,TSIMRES)

! ***         Always put emissions into Eulerian grid, No matter the 
! ***         influence zone (Bruce):

              TMAXJV = 0.

!_LHS_Change_19Jan2012_End.


! Add to average maximum influence time for current line source

              QLTMAX(IQL) = QLTMAX(IQL) + TMAXJV/NJV

! Go through all compounds

              DO 140 IC = 1,NC

! Calculate total mass emitted from current line segment during the
! influence time (g)

              MASSV(IC) = MASSV(IC) + QMV(IC)*TMAXJV

! Next compound

  140         CONTINUE

! If reached influence time zone then do not add emission anymore from
! current line segment to grid model grid

              IF (TTOT .GT. (TSIMRES - TMAXJV)) GOTO 130

! Calculate corresponding grid model grid cell

              CALL GETMGI(0,QLXV,QLYV,1.0,IX,IY,IZ)

! If outside of grid model grid then goto next line segment

              IF (IX .LT. 1 .OR. IX .GT. NX) GOTO 130
              IF (IY .LT. 1 .OR. IY .GT. NY) GOTO 130

              do ic=1,nc
                 if (CMPND(IC)(1:2) == 'O3')  cmo3 = DBLE( C(IC,IX+1,IY+1,1) )
              enddo

! Go through all compounds

              DO 150 IC = 1,NC

! Add emission intensity from line source to grid model grid (ug/m3*s)


               SCALEQL=1.0E+6 !Converts to ug/m3
!MSK start
               !ALLOWS FOR PARTICLE NUMBER EMISSIONS BY CONVERTING FROM UG/M3 TO NUM/CM3
               !CONVERSION FROM M3 TO CM3 IS ALSO A FACTOR OF 10E-6
               if ( CUNIT(IC)(1:3) == 'num' ) then
                 SCALEQL=1.0E-6 !Converts to num/cm3
               endif

               scale_qno = 1.0

               if (CMPND(IC) == 'NO        ' .and. use_timescale_no) then
                 UU_no     = U(IX,IY,IZ)
                 VV_no     = V(IX,IY,IZ)
                 THETA_no  = CWDIR(UU_no,VV_no)
                 beta_no   = 90-(45+THETA_no)
                 dist_no   = min( 10000.0, (0.5*DX)/max(0.0001,abs(SIN(beta_no))) )
                 time_no   = dist_no/UE_no + init_timescale_no
                 O3V_no    = cmo3 * (AVOG_no*1.0E-12)/MW_O3_no   !molec O3
                 O3V_no    = max(O3V_no, dble(1.e5))
                 TAIRV_no  = TAIR(IX,IY) + 273.13
                 RK1_no    = 1.4e-12*EXP4(-1310./TAIRV_no)
                 tau_no    = 1.0/(RK1_no*O3V_no)
                 scale_qno = (1.0 - EXP4(-time_no/real(tau_no)))
                 !print *,'LSGRID scale_qno ', IX, IY, CMO3, RK1_no, O3V_no, time_no, scale_qno
               endif
!MSK end

               lsrc_dcdt(IC,IX,IY,IZ) = lsrc_dcdt(IC,IX,IY,IZ) +      &
!MSK start
!MSK                                 SCALEQL*L2A(siteid,cmpnd(IC))*       &
                                 SCALEQL * l2a *                      &
                                 scale_qno *                          &
!MSK end
                                 QMV(IC)/(VOL(IZ)*DEPTHM(IX,IY)/MOD_H)


!_lhs_LSGRID_reduction_END_June_2011.

! Next compound

  150         CONTINUE

! Next line segment

  130     CONTINUE

! If not last timestep then goto next line source

          IF (ITS .LT. NTS) GOTO 100

! If preprocessing then goto next line source

          IF (I .EQ. 0) GOTO 100

! Calculate the corner coordinates in the normal to line source
! directed system

          XW(1) = -RMAXRV
          XW(2) = -RMAXRV
          XW(3) = +RMAXRV
          XW(4) = +RMAXRV

          YW(1) = -QLLV/2. - RMAXRV
          YW(2) = +QLLV/2. + RMAXRV
          YW(3) = -QLLV/2. - RMAXRV
          YW(4) = +QLLV/2. + RMAXRV

! Calculate line source normal direction

          DNL = MOD(DL + 90.,360.)

! Calculate normal to line source coordinate system rotation angle

          THETA = (270. - DNL)*RAD

! Transform corner coordinates to original coordinates

          DO 200 IV = 1,4
!MSK              XV(IV) = XV(0) + COS(THETA)*XW(IV) - SIN(THETA)*YW(IV)
!MSK              YV(IV) = YV(0) + SIN(THETA)*XW(IV) + COS(THETA)*YW(IV)
              XV(IV) = xv0 + COS(THETA)*XW(IV) - SIN(THETA)*YW(IV)
              YV(IV) = yv0 + SIN(THETA)*XW(IV) + COS(THETA)*YW(IV)
              ZV(IV) = 1.0
  200     CONTINUE

! Get extended grid model grid indices

          DO 210 IV = 1,4

              CALL GETMGI(0,XV(IV),YV(IV),ZV(IV),IX,IY,IZ)

              IXV(IV) = IX
              IYV(IV) = IY
              IZV(IV) = IZ

  210     CONTINUE

! Calculate extended extreme indices

          IXMIN = MIN(IXV(1),IXV(2),IXV(3),IXV(4))
          IXMAX = MAX(IXV(1),IXV(2),IXV(3),IXV(4))
          IYMIN = MIN(IYV(1),IYV(2),IYV(3),IYV(4))
          IYMAX = MAX(IYV(1),IYV(2),IYV(3),IYV(4))
          IZMIN = 1
          IZMAX = 1

! Initialize sum of line source grid model concentrations to zero

          DO 220 IC = 1,NC
              CSUM(IC) = 0.
  220     CONTINUE

! Go through all extended grid model grid cells

          DO 230 IZ = IZMIN,IZMAX
          DO 240 IY = IYMIN,IYMAX
          DO 250 IX = IXMIN,IXMAX

          XRV = SITEX0 + (IX - 0.5)*DX
          YRV = SITEY0 + (IY - 0.5)*DY
          ZRV = 1.0

! Calculate concentrations and dry and wet depositions

          call csubl(IQL,XRV,YRV,ZRV,NC,MC,CRV,DDRV,WDRV)

! Go through all compounds

          DO 260 IC = 1,NC

! Add total concentration sum

          CSUM(IC) = CSUM(IC) + CRV(IC)

! If outside of grid model grid then goto next compound

          IF (IX .LT. 1 .OR. IX .GT. NX) GOTO 260
          IF (IY .LT. 1 .OR. IY .GT. NY) GOTO 260
          IF (IZ .LT. 1 .OR. IZ .GT. NZ) GOTO 260

! Set line source concentration in grid model grid cell

          CGRIDV(IC,IX,IY,IZ) = CRV(IC)

! Next compound

  260     CONTINUE

! Next extended grid model grid cell

  250     CONTINUE
  240     CONTINUE
  230     CONTINUE

! Go through all compounds

          do ic=1,nc
             if (CMPND(IC)(1:2) == 'O3')  cmo3 = DBLE( C(IC,IX+1,IY+1,1) )
          enddo

          DO 330 IC = 1,NC

! If sum of concentrations is equal to zero then use midpoint of
! line source

          IF (CSUM(IC) .LE. 0.) THEN

!MSK          IX = IXV(0)
!MSK          IY = IYV(0)
!MSK          IZ = IZV(0)

           IX = ixv0
           IY = iyv0
           IZ = izv0

! If outside of Eulerian model grid then goto next compound:

          IF (IX .LT. 1 .OR. IX .GT. NX) GOTO 330
          IF (IY .LT. 1 .OR. IY .GT. NY) GOTO 330
          IF (IZ .LT. 1 .OR. IZ .GT. NZ) GOTO 330

!_LHS_08_nov_2002:  The above is still incorrect since a (long) line 
!                 source that has its midpoint outside the grid, may 
!                 still have line segments inside the model domain. 
!                 The mass from these segments are lost.
!_LHS_08_nov_2002.

! Add line source concentration to grid model concentration:
!MSK This is equation (16) in NILU TR12/2003 Appendix A.3 (Special Case)

              SCALEQL=1.0E+6 !Converts to ug/m3
              if ( CUNIT(IC)(1:3) == 'num' ) then
                   SCALEQL=1.0E-6 !Converts to num/cm3
              endif

!MSK start
              scale_qno = 1.0
              if (CMPND(IC) == 'NO        ' .and. use_timescale_no) then
                 UU_no     = U(IX,IY,IZ)
                 VV_no     = V(IX,IY,IZ)
                 THETA_no  = CWDIR(UU_no,VV_no)
                 beta_no   = 90-(45+THETA_no)
                 dist_no   = min( 10000.0, (0.5*DX)/max(0.0001,abs(SIN(beta_no))) )
                 time_no   = dist_no/UE_no + init_timescale_no
                 O3V_no    = cmo3 * (AVOG_no*1.0E-12)/MW_O3_no
                 O3V_no    = max(O3V_no, dble(1.e5))
                 TAIRV_no  = TAIR(IX,IY) + 273.13
                 RK1_no    = 1.4e-12*EXP4(-1310./TAIRV_no)
                 tau_no    = 1.0/(RK1_no*O3V_no)
                 scale_qno = (1.0 - EXP4(-time_no/real(tau_no)))
              endif
!MSK end

!MSK start
!MSK              C(IC,IX,IY,IZ) = C(IC,IX,IY,IZ) +       &
              C(IC,IX+1,IY+1,IZ) = C(IC,IX+1,IY+1,IZ) +      &
!MSK                              SCALEQL*L2A(siteid,cmpnd(IC))*       &
                              SCALEQL * l2a *                &
                              scale_qno *                    &
!MSK end
                              MASSV(IC)/(VOL(IZ)*DEPTHM(IX,IY)/MOD_H)


          ELSE

! Go through all extended grid model grid cells

          DO 300 IZ = IZMIN,IZMAX
          DO 310 IY = IYMIN,IYMAX
          DO 320 IX = IXMIN,IXMAX

! If outside of Eulerian model grid then goto next grid cell:

          IF (IX .LT. 1 .OR. IX .GT. NX) GOTO 320
          IF (IY .LT. 1 .OR. IY .GT. NY) GOTO 320
          IF (IZ .LT. 1 .OR. IZ .GT. NZ) GOTO 320

! Add line source concentration to grid model concentration
!MSK This is equation (14) in NILU TR12/2003 Appendix A.3

              SCALEQL=1.0E+6 !Converts to ug/m3

              if ( CUNIT(IC)(1:3) == 'num' ) then
                 SCALEQL=1.0E-6 !Converts to num/cm3
              endif

!MSK start
              scale_qno = 1.0
              if (CMPND(IC) == 'NO        ' .and. use_timescale_no) then
                 UU_no     = U(IX,IY,IZ)
                 VV_no     = V(IX,IY,IZ)
                 THETA_no  = CWDIR(UU_no,VV_no)
                 beta_no   = 90-(45+THETA_no)
                 dist_no   = min( 10000.0, (0.5*DX)/max(0.0001,abs(SIN(beta_no))) )
                 time_no   = dist_no/UE_no + init_timescale_no
                 O3V_no    = cmo3 * (AVOG_no*1.0E-12)/MW_O3_no
                 O3V_no    = max(O3V_no, dble(1.e5))
                 TAIRV_no  = TAIR(IX,IY) + 273.13
                 RK1_no    = 1.4e-12*EXP4(-1310./TAIRV_no)
                 tau_no    = 1.0/(RK1_no*O3V_no)
                 scale_qno = (1.0 - EXP4(-time_no/real(tau_no)))
              endif
!MSK end

!MSK start
!MSK             C(IC,IX,IY,IZ) = C(IC,IX,IY,IZ) +       &
             C(IC,IX+1,IY+1,IZ) = C(IC,IX+1,IY+1,IZ) +          &
                              (CGRIDV(IC,IX,IY,IZ)/CSUM(IC))*   &
!MSK                              SCALEQL*L2A(siteid,cmpnd(IC))*       &
                              SCALEQL * l2a *                   &
                              scale_qno *                       &
!MSK end
                              MASSV(IC)/(VOL(IZ)*DEPTHM(IX,IY)/MOD_H)


! Next extended grid model grid cell

  320     CONTINUE
  310     CONTINUE
  300     CONTINUE

          ENDIF

! Next compound

  330 CONTINUE

! Next line source

  100 CONTINUE


!_lhs_LSGRID_reduction_START_June_2011:

        if (messfe) then
          write(messun,*)       
          write(messun,'(A)') 'LSGRID: The LSRC-emis to the grid model.'
          write(messun,'(A,I4)') 'LSGRID: for ITS = ',ITS
!_LHS_Change_30Nov2011_Start:
          do IC = 1,NC
            rsum_DCDT     = SUM(lsrc_dcdt(IC,:,:,:))
            write(messun,'(A,I2,A,E12.5)')       &
              'LSGRID: Domain sum LSRC_DCDT(IC=',IC,',:,:,:) = ',rsum_DCDT
          enddo
!_LHS_Change_30Nov2011_End.
          rmax_DCDT     = MAXVAL(lsrc_dcdt)
          iloc_4D       = MAXLOC(lsrc_dcdt)
          unit_len      = LEN_TRIM(cunit(iloc_4D(1)))         
          write(messun,'(44X,4A4)') ' IC ','  I ','  J ','  K '
          write(messun,'(A,E12.5,1X,A,2X,4I4)')       &
            'LSGRID: Max LSRC_DCDT: ',rmax_DCDT,cunit(iloc_4d(1))(1:unit_len),(iloc_4d(ii),ii=1,4)          
        endif  ! if (messfe)
 
      endif  ! if (ITS == 1)

! *** Adding the linesource contributions to the grid emissions:
! *** NB: done for every NTS timesteps since dcdt is built up every timestep.

!_LHS_Change_24April2012_Start:
!_LHS      dcdt = dcdt + (lsrc_dcdt * scale_line2area)
!MSK start
!MSK       dcdt = dcdt + lsrc_dcdt

      DO 400 IZ = 1,NZ
      DO 410 IY = 1,NY
      DO 420 IX = 1,NX
      DO 430 IC = 1,NC

           DCDT(IC,IX,IY,IZ) = DCDT(IC,IX,IY,IZ) + lsrc_dcdt(IC,IX,IY,IZ)

  430 CONTINUE
  420 CONTINUE
  410 CONTINUE
  400 CONTINUE
!MSK end

!_LHS_Change_24April2012_End.
       
      if (ITS == 1 .AND. messfe) then

          write(messun,*)       
          write(messun,'(A)') 'LSGRID: The LSRC-emis to the grid model.'
          write(messun,'(A,I4)') 'LSGRID: for ITS = ',ITS
          do IC = 1,NC
            rsum_DCDT     = SUM(dcdt(IC,:,:,:))
            write(messun,'(A,I2,A,E12.5)')       &
              'LSGRID: Domain sum DCDT(IC=',IC,',:,:,:) = ',rsum_DCDT
          enddo
          rmax_DCDT     = MAXVAL(dcdt)
          iloc_4D       = MAXLOC(dcdt)
          unit_len      = LEN_TRIM(cunit(iloc_4d(1)))
          write(messun,'(44X,4A4)') ' IC ','  I ','  J ','  K '
          write(messun,'(A,E12.5,1X,A,2X,4I4)')       &
            'LSGRID: Max DCDT:      ',rmax_DCDT,cunit(iloc_4d(1))(1:unit_len),(iloc_4d(ii),ii=1,4)
      endif      

!_lhs_LSGRID_reduction_END_June_2011.

! Deallocate memory

      IF (ALLOCATED(CGRIDV)) DEALLOCATE(CGRIDV)
      IF (ALLOCATED(   CRV)) DEALLOCATE(CRV)
      IF (ALLOCATED(  CSUM)) DEALLOCATE(CSUM)
      IF (ALLOCATED(  DDRV)) DEALLOCATE(DDRV)
      IF (ALLOCATED( MASSV)) DEALLOCATE(MASSV)
      IF (ALLOCATED(   QMV)) DEALLOCATE(QMV)
      IF (ALLOCATED(  WDRV)) DEALLOCATE(WDRV)

      RETURN

! End of subroutine LSGRID

      end subroutine lsgrid
