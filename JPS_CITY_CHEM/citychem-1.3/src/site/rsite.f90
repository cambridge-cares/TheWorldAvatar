! <rsite.f90 - A component of the City-scale
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

      subroutine RSITE

!     This subroutine Checks and Initialize additional Site Variables within the Site Module.

!     NOTE: For the Stand-alone version of EPISODE:  MAINFE = .FALSE.
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
!           2017  M. Karl: New read of STATIONS, UTMZONE, EPSGN
!                          Commented all NEST grid parts
!
! ----------------------------------------------------------------------------------

      use mod_main
      use mod_site
      
         implicit none

!     Local variables

      INTEGER I,K,IX,IY
      REAL sid_freq
      CHARACTER(len=256) TXTSTR
      LOGICAL LEOF

!     I      - Index
!     K      - Index
!     TXTSTR - Textstring
!     LEOF   - If end of file then true else false

!     Read site name:
      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,*) SITEID
          ENDIF
          print *,'sitename',SITEID
      ENDIF

      IF (MESSFE) WRITE (MESSUN,2000)  &
                 (SITEID(I:I),I=1,INDEX(SITEID,' ') - 1)

!     Read site latitude (deg) and longitude (deg):
      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
           IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,*) SITELA,SITELO
          ENDIF
          print *,'sitelalo',SITELA,SITELO
      ENDIF

      IF (MESSFE) WRITE (MESSUN,2010) SITELA,SITELO

!_CITYDELTA_Start:
      sid_freq = 0.7292E-4
      IF(SITELA .GT. 20.0)THEN
          f_cor = 2.0 * sid_freq*SIN(SITELA * 3.1415927/180.0)
      ELSE
          f_cor = 2.0 * sid_freq*SIN(20.0 * 3.1415927/180.0)
      ENDIF

      IF (MESSFE) WRITE (MESSUN,'(A15,E12.4)')  &
                'RSITE: F_COR = ',f_cor
!_CITYDELTA_End.

!MSK start
      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
           IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,*) STATIONS
          ENDIF
          print *,'number of stations',STATIONS
      ENDIF

      IF (MESSFE) WRITE (MESSUN,2015) STATIONS
!MSK end


!_NEST_Start:

!     Read main (coarse) grid origo (m)
      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,*) SITEX0,SITEY0
          ENDIF
          print *,'sitexy',SITEX0,SITEY0
      ENDIF

      IF (MESSFE) WRITE (MESSUN,2020) SITEX0,SITEY0

!MSK start
! Read UTM zone of main grid 
      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,*) UTMZONE
          ENDIF
          print *,'utm zone',UTMZONE
      ENDIF

      IF (MESSFE) WRITE (MESSUN,2025)  &
                 (UTMZONE(I:I),I=1,INDEX(UTMZONE,' ') - 1)
! Read EPSG Number
      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,*) EPSGN
          ENDIF
          print *,'epsg number',EPSGN
      ENDIF

      IF (MESSFE) WRITE (MESSUN,2025)  &
                 (UTMZONE(I:I),I=1,INDEX(EPSGN,' ') - 1)
                 
!MSK end


!     Read main (coarse) grid size and boundary zone size
      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF) 
          IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,*) NX,NY,NZ,NBCX,NBCY,NBCZ
          ENDIF
          print *,'main grid',NX,NY,NZ,NBCX,NBCY,NBCZ
      ENDIF

      IF (MESSFE) WRITE (MESSUN,2030) NX,NY,NZ
      IF (MESSFE) WRITE (MESSUN,2031) NBCX,NBCY,NBCZ
!_NEST_End.


      IF (.NOT. ALLOCATED(DZ))     ALLOCATE(DZ(NZ))
      IF (.NOT. ALLOCATED(VOL))    ALLOCATE(VOL(NZ))
      IF (.NOT. ALLOCATED(Z))      ALLOCATE(Z(NZ))
      IF (.NOT. ALLOCATED(HBLD))   ALLOCATE(HBLD(NX,NY))
      IF (.NOT. ALLOCATED(SUBF))   ALLOCATE(SUBF(NX,NY))
      IF (.NOT. ALLOCATED(TOPF))   ALLOCATE(TOPF(NX,NY))

      IF (.NOT. ALLOCATED(TOPM))   ALLOCATE(TOPM(NX,NY))

      IF (.NOT. ALLOCATED(Z0))     ALLOCATE(Z0(NX,NY))
!_SIGMA_Start:
      IF (.NOT. ALLOCATED(DEPTHM)) ALLOCATE(DEPTHM(NX,NY))
!_SIGMA_End.

      MX = NX
      MY = NY
      MZ = NZ
!_NEST_Start:
      MBCX = NBCX
      MBCY = NBCY
      MBCZ = NBCZ
!_NEST_End.

      MXYP2 = MX + MY + 2

!     Read angle between grid x-axis and East-West-axis for coarse grid (deg):
      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,*) ANGLE
          ENDIF
          print *,'angle ',ANGLE
      ENDIF

      IF (MESSFE) WRITE (MESSUN,2040) ANGLE

!     Calculate main grid sizes minus 1:
      NXM1 = NX - 1
      NYM1 = NY - 1
      NZM1 = NZ - 1

!     Read horizontal and vertical sizes of main coarse gridcells
!     in meters:
      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF) 
          IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,*) DX,DY,(DZ(K),K=1,NZ)
          ENDIF
         print *,'length main grid ',DX,DY,(DZ(K),K=1,NZ)
      ENDIF

      IF (MESSFE) THEN
          WRITE (MESSUN,2050) DX,DY
          WRITE (MESSUN,2060) NZ,(DZ(K),K=1,NZ)
      ENDIF

!     Calculate main grid boundaries (in meters):
      XMIN = 0.0
      YMIN = 0.0
      XMAX = NX*DX
      YMAX = NY*DY

!     Calculate main gridcell sizes squared (in meters):
      DX2 = DX*DX
      DXY = DX*DY
      DY2 = DY*DY

!     Calculate height above ground of the NZ main grid layers
!_SIGMA-comment_Start: 
!     This will be the height of each sigma layer (in meters)
!_SIGMA-comment_End.

      Z(1) = DZ(1)
      DO 100 K = 2,NZ
          Z(K) = Z(K-1) + DZ(K)
  100 CONTINUE

!_SIGMA_Start:
!     MOD_H will be the constant coarse grid model height in meter.
!     It is equal to the model depth, DEPTHM, above the lowest 
!     topography gridpoint.

      MOD_H = Z(NZ)
      IF (MESSFE) WRITE (MESSUN,2062) MOD_H

!_SIGMA_End.

! Calculate grid volumes of the NZ main grid layers (in m**3)

!_SIGMA-comment_Start:
!     The product below is not a correct measure of the physical grid
!     box volume. However, this is corrected within the code whenever
!     VOL(1:NZ) is applied, i.e. VOL(K) is multiplied with the stretch
!     factor (or Jacobian), DEPTHM(I,J)/MOD_H).
!_SIGMA-comment_End. 
      DO 110 K = 1,NZ
          VOL(K) = DXY*DZ(K)
       print *,'rsite: VOL',VOL(K)
  110 CONTINUE


!MSK start
!MSK ALL SUBGRID STUFF COMMENTED
!MSK  The sub grid is not used in CityChem
!MSK
!MSK  !_NEST_Start:
!MSK
!MSK !     Read Nested fine grid origo (m):
!MSK      IF (MAINFE) THEN
!MSK          CALL GETDAT(MAINUN,TXTSTR,LEOF)
!MSK          IF (TXTSTR(1:1) .NE. '<') THEN
!MSK              READ (TXTSTR,*) sitex0_nest,sitey0_nest
!MSK          ENDIF
!MSK         print *,'sub grid origo',sitex0_nest,sitey0_nest
!MSK      ENDIF
!MSK
!MSK      IF (MESSFE) WRITE (MESSUN,2021) sitex0_nest,sitey0_nest
!MSK
!MSK !     Read Nested fine grid size:
!MSK      IF (MAINFE) THEN
!MSK          CALL GETDAT(MAINUN,TXTSTR,LEOF) 
!MSK          IF (TXTSTR(1:1) .NE. '<') THEN 
!MSK              READ (TXTSTR,*) NX_nest,NY_nest,NZ_nest,      &
!MSK                             NBCX_nest,NBCY_nest,NBCZ_nest
!MSK          ENDIF
!MSK         print *,'sub grid size',NX_nest,NY_nest,NZ_nest,      &
!MSK                             NBCX_nest,NBCY_nest,NBCZ_nest
!MSK      ENDIF
!MSK
!MSK      IF (MESSFE) WRITE (MESSUN,2032) NX_nest,NY_nest,NZ_nest
!MSK
!MSK inserted calculated of subgrid size
!MSK      MXS = NX_nest
!MSK      MYS = NY_nest
!MSK !     Calculate subgrid cell size in meter:
!MSK      DXS = DX/NX_nest
!MSK      DYS = DY/NY_nest
!MSK      DZS = 0.0
!MSK    IF (NZ_nest .GT. 1) DZS = DZ(1)/(NZ_nest - 1)
!MSK
!MSK      IF (MESSFE) WRITE (MESSUN,2033) NBCX_nest,NBCY_nest,NBCZ_nest
!MSK
!MSK
!MSK      IF (.NOT. ALLOCATED(DZ_nest))     ALLOCATE(DZ_nest(NZ_nest))
!MSK      IF (.NOT. ALLOCATED(Z_nest))      ALLOCATE(Z_nest(NZ_nest))
!MSK      IF (.NOT. ALLOCATED(topm_nest))    &
!MSK               ALLOCATE(topm_nest(1 - NBCX_nest:NX_nest + NBCX_nest,  &
!MSK                                  1 - NBCY_nest:NY_nest + NBCY_nest))
!MSK
!MSK        MX_nest = NX_nest
!MSK        MY_nest = NY_nest
!MSK        MZ_nest = NZ_nest
!MSK
!MSK        MBCX_nest = NBCX_nest
!MSK        MBCY_nest = NBCY_nest
!MSK        MBCZ_nest = NBCZ_nest
!MSK
!MSK !     Read angle between grid x-axis and East-West-axis (deg):
!MSK      IF (MAINFE) THEN
!MSK          CALL GETDAT(MAINUN,TXTSTR,LEOF)
!MSK          IF (TXTSTR(1:1) .NE. '<') THEN
!MSK              READ (TXTSTR,*) ANGLE_nest
!MSK          ENDIF
!MSK         print *,'sub grid angle', ANGLE_nest
!MSK      ENDIF
!MSK
!MSK      IF (MESSFE) WRITE (MESSUN,2041) ANGLE_nest
!MSK
!MSK !     Read horizontal and vertical sizes of main gridcells in meter:
!MSK
!MSK !_SIGMA-comment_Start:
!MSK !     DZ_nest(1) - DZ_nest(NZ_nest) is then the height of the fine grid
!MSK !     sigma layers.
!MSK !_SIGMA-comment_End.
!MSK
!MSK    IF (MAINFE) THEN
!MSK          CALL GETDAT(MAINUN,TXTSTR,LEOF) 
!MSK          IF (TXTSTR(1:1) .NE. '<') THEN
!MSK              READ (TXTSTR,*) DX_nest,DY_nest,(DZ_nest(K),K=1,NZ_nest)
!MSK          ENDIF
!MSK         print *,'sub grid length', DX_nest,DY_nest,(DZ_nest(K),K=1,NZ_nest)
!MSK      ENDIF
!MSK
!MSK      IF (MESSFE) THEN
!MSK          WRITE (MESSUN,2051) DX_nest,DY_nest
!MSK          WRITE (MESSUN,2061) NZ_nest,(DZ_nest(K),K=1,NZ_nest)
!MSK      ENDIF
!MSK
!MSK !     Calculate height above ground of the NZ_nest main grid layers:
!MSK !_SIGMA_Start: 
!MSK !     This will be the height of each fine grid sigma layer.
!MSK
!MSK      Z_nest(1) = DZ_nest(1)
!MSK      DO K = 2,NZ_nest
!MSK          Z_nest(K) = Z_nest(K-1) + DZ_nest(K)
!MSK      ENDDO
!MSK
!MSK !_SIGMA-comment_Start:
!MSK !     mod_h_nest will be the constant model height. Above the
!MSK !     lowest topography gridpoint mod_h_nest is equal to the 
!MSK !     fine grid model depth.
!MSK !_SIGMA-comment_End.
!MSK
!MSK    mod_h_nest = Z_nest(NZ_nest)
!MSK       IF (MESSFE) WRITE (MESSUN,2063) mod_h_nest
!MSK
! Since we do not work with subgrid nests, set SUBF to zero
      DO IY = 1,NY
        DO IX = 1,NX
          SUBF(IX,IY) = 0.0
        ENDDO
      ENDDO
!MSK end

!_SIGMA_End.
!_NEST_End.

!MSK Back to MAIN GRID stuff
!
!     Read main grid topography filename or value (m)
!
!_SIGMA-comment_Start:
!     The 2-D main coarse grid topography field will be necessary for 
!     the computation of actual height above ground for the coarse 
!     grid model domain.
!_SIGMA-comment_End.

      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
         print *,'topography file ', TXTSTR
          CALL GETFNV(TXTSTR,TOPMFN,TOPMFE,1,1,TOPMFV)
      ELSE
          TXTSTR(1:1) = '<'
          CALL GETFNV(TXTSTR,TOPMFN,TOPMFE,1,1,TOPMFV)
      ENDIF

!     Read main grid topography:
      call rtopm


!     Read surface roughness filename or value (m):
      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
         print *,'surface roughness file ', TXTSTR
          CALL GETFNV(TXTSTR,SURFFN,SURFFE,1,1,SURFFV)
      ELSE
          TXTSTR(1:1) = '<'
          CALL GETFNV(TXTSTR,SURFFN,SURFFE,1,1,SURFFV)
      ENDIF

!     Read surface roughness:
      call rsurf


!MSK skip reading albedo field from TAPM. It is not used.     
      call GETDAT(MAINUN,TXTSTR,LEOF)
      print *,'albedo file (not used) ', TXTSTR

!MSK sufrace roughness subgrid flags does not exist in main input => commented
!     Read subgrid flags filename or value: WHAT IS THIS FLAGS STUFF???
!      IF (MAINFE) THEN
!          CALL GETDAT(MAINUN,TXTSTR,LEOF)
!          CALL GETFNV(TXTSTR,SUBFFN,SUBFFE,1,1,SUBFFV)
!      ELSE
!          TXTSTR(1:1) = '<'
!          CALL GETFNV(TXTSTR,SUBFFN,SUBFFE,1,1,SUBFFV)    
!      ENDIF

!     Read receptor points filename:
      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,1000) RECPFN
          ENDIF
         print *,'receptor point file ', RECPFN
      ELSE
! ***     By applying this ELSE branch RECPFE is set to .FALSE.
! ***     Furthermore: RECPFN is set to ' ', and RECPFV to MISS.
          TXTSTR(1:1) = '<'
          CALL GETFNV(TXTSTR,RECPFN,RECPFE,1,1,RECPFV)
      ENDIF


!     Read receptor points:
      call rrecp


!MSK topography flags does not exist in main input => commented
!     Read topography flags filename or value
!      IF (MAINFE) THEN
!          CALL GETDAT(MAINUN,TXTSTR,LEOF)
!          CALL GETFNV(TXTSTR,TOPFFN,TOPFFE,1,1,TOPFFV)          
!      ELSE
!          TXTSTR(1:1) = '<'
!          CALL GETFNV(TXTSTR,TOPFFN,TOPFFE,1,1,TOPFFV)        
!      ENDIF

!MSK start
! Since we do not work with subgrid nests, set TOPF to zero
      DO IY = 1,NY
        DO IX = 1,NX
          TOPF(IX,IY) = 0.0
        ENDDO
      ENDDO
!MSK end

!_LHS      IF (MESSFE) THEN
!_LHS	         WRITE (MESSUN,2110)
!_LHS     &          (SUBFFN(I:I),I=1,INDEX(SUBFFN,' ') - 1)
!_LHS          WRITE (MESSUN,2115) SUBFFV
!_LHS      ENDIF

!MSK commented CALL RSUBF
!     Read subgrid flags: WHAT IS THIS??????
!      CALL RSUBF

!_LHS      IF (MESSFE) THEN
!_LHS          WRITE (MESSUN,2120)
!_LHS     &          (TOPFFN(I:I),I=1,INDEX(TOPFFN,' ') - 1)
!_LHS          WRITE (MESSUN,2125) TOPFFV
!_LHS      ENDIF

!MSK commented CALL RTOPF
!     Read topography flags
!      CALL RTOPF

!_LHS      IF (MESSFE) THEN
!_LHS	         WRITE (MESSUN,2130)
!_LHS     &          (TOPMFN(I:I),I=1,INDEX(TOPMFN,' ') - 1)
!_LHS          WRITE (MESSUN,2135) TOPMFV
!_LHS      ENDIF



!MSK subgrid topography does not exist in main input => commented
!     Read subgrid topography number of fields:
!      IF (MAINFE) THEN
!          CALL GETDAT(MAINUN,TXTSTR,LEOF)
!          IF (TXTSTR(1:1) .NE. '<') THEN
!              READ (TXTSTR,*) NLT
!          ENDIF
!      ENDIF
!
!      IF (MESSFE) WRITE (MESSUN,2136) NLT
!
!      IF (.NOT. ALLOCATED(TOPS)) ALLOCATE(TOPS(NXS,NYS,NLT))
!
!      MLT = NLT
!
!     Read sub grid (subgrid arsrc) topography filename or value (m):
!      IF (MAINFE) THEN
!          CALL GETDAT(MAINUN,TXTSTR,LEOF)
!          CALL GETFNV(TXTSTR,TOPSFN,TOPSFE,1,1,TOPSFV)
!      ELSE
!          TXTSTR(1:1) = '<'
!          CALL GETFNV(TXTSTR,TOPSFN,TOPSFE,1,1,TOPSFV)
!      ENDIF
!!      TOPSFV = LHS_TOPSFV(1)
!!_LHS      IF (MESSFE) THEN
!!_LHS          WRITE (MESSUN,2140)
!!_LHS     &          (TOPSFN(I:I),I=1,INDEX(TOPSFN,' ') - 1)
!!_LHS          WRITE (MESSUN,2145) TOPSFV
!!_LHS      ENDIF

!MSK commented CALL RTOPS
!     Read sub grid (subgrid asrc) topography:
!      CALL RTOPS

!_NEST_Start:
!
!_SIGMA-comment_Start:
!     The 2-D Nested fine grid topography field will be necessary for
!     writing the values at the correct heights above ground for the 
!     nested fine grid model domain.
!_SIGMA-comment_End.
!MSK commented
!      IF (MAINFE) THEN
!          CALL GETDAT(MAINUN,TXTSTR,LEOF)
!          CALL GETFNV(TXTSTR,topmfn_nest,topmfe_nest,1,1,topmfv_nest)
!      ELSE
!          TXTSTR(1:1) = '<'
!          CALL GETFNV(TXTSTR,topmfn_nest,topmfe_nest,1,1,topmfv_nest)
!      ENDIF
!!_LHS      IF (MESSFE) THEN
!!_LHS          WRITE (MESSUN,2131)
!!_LHS     &          (topmfn_nest(I:I),I=1,INDEX(topmfn_nest,' ') - 1)
!!_LHS          WRITE (MESSUN,2137) topmfv_nest
!!_LHS      ENDIF
!     Read main grid topography for the FINE grid:
!      CALL RTOPM_F

!_NEST_End.


!MSK building height file not included in main input file
!MSK therefore commented. Use later?
!     Read height of buildings filename or value (m):
!      IF (MAINFE) THEN
!          CALL GETDAT(MAINUN,TXTSTR,LEOF)
!          CALL GETFNV(TXTSTR,HBLDFN,HBLDFE,1,1,HBLDFV)          
!      ELSE
!          TXTSTR(1:1) = '<'
!          CALL GETFNV(TXTSTR,HBLDFN,HBLDFE,1,1,HBLDFV)
!      ENDIF

!!_LHS      IF (MESSFE) THEN
!!_LHS	         WRITE (MESSUN,2150)
!!_LHS     &          (HBLDFN(I:I),I=1,INDEX(HBLDFN,' ') - 1)
!!_LHS          WRITE (MESSUN,2155) HBLDFV
!!_LHS      ENDIF

!MSK commented CALL RHBLD
! Read height of buildings
!      CALL RHBLD

!MSK Skip last two entries in the site section of main input file
      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF) 
          print *,'export points (airquis)', TXTSTR
          CALL GETDAT(MAINUN,TXTSTR,LEOF) 
          print *,'total population  (airquis)', TXTSTR
      ENDIF

      RETURN

 1000 FORMAT (A256)

 2000 FORMAT ('RSITE: Site name: ',256A1)
 2010 FORMAT ('RSITE: Site latitude  = ',F10.3,' deg',  &
             ' longitude = ',F10.3,' deg')
 2020 FORMAT ('RSITE: Main grid origo X = ',F12.3,' Y = ',F12.3)
!MSK start
 2015 FORMAT ('RSITE: Number of stations = ',I3)
 2025 FORMAT ('RSITE: UTM zone: ',256A1)
!MSK end
!_NEST_Start:
 2021 FORMAT ('RSITE: Fine grid origo X = ',F12.3,' Y = ',F12.3)
 2030 FORMAT ('RSITE: Main grid NX  = ',I3,' NY  = ',I3,' NZ  = ',I3)
 2032 FORMAT ('RSITE: Fine grid NX_nest  = ',I3,' NY_nest  = ',I3,  &
                                             ' NZ_nest  = ',I3)
 2031 FORMAT ('RSITE: Main grid NBCX= ',I3,' NBCY= ',I3,' NBCZ= ',I3)
 2033 FORMAT ('RSITE: Fine grid NBCX_nest= ',I3,' NBCY_nest= ',I3,  &
                                            ' NBCZ_nest= ',I3)
 2040 FORMAT ('RSITE: Main grid rotation from east = ',F10.3,' deg')
 2041 FORMAT ('RSITE: Fine grid rotation from east = ',F10.3,' deg')

 2050 FORMAT ('RSITE: Main grid DX  = ',F10.3,' m DY  = ',F10.3,' m')
 2051 FORMAT ('RSITE: Fine grid DX_nest  = ',F10.3,' m DY_nest  = ',  &
                                          F10.3,' m')
 2060 FORMAT ('RSITE: Main grid',' DZ(1:',I2,') = ',12(7(F8.1,' m ')/))
 2061 FORMAT ('RSITE: Main grid',' DZ_nest(1:',I2,') = ',12(7(F8.1,' m ')/))
 2062 FORMAT ('RSITE: Coarse grid  MOD_H     = ',F9.3,' m ')
 2063 FORMAT ('RSITE: Fine grid    MOD_H_nest = ',F9.3,' m ')
 2070 FORMAT ('RSITE: Sub  grid NXS = ',I3,' NYS = ',I3,' NZS = ',I3)
 2080 FORMAT ('RSITE: Receptor points .... file      = ',256A1)
 2100 FORMAT ('RSITE: Surface roughness .. filename  = ',256A1)
 2105 FORMAT ('RSITE: Surface roughness .. filevalue = ',F10.3,' m')
 2110 FORMAT ('RSITE: Subgrid flags ...... filename  = ',256A1)
 2115 FORMAT ('RSITE: Subgrid flags ...... filevalue = ',F10.3)
 2120 FORMAT ('RSITE: Topography flags ... filename  = ',256A1)
 2125 FORMAT ('RSITE: Topography flags ... filevalue = ',F10.3)
 2130 FORMAT ('RSITE: Main grid topography filename  = ',256A1)
 2135 FORMAT ('RSITE: Main grid topography filevalue = ',F10.3,' m')
 2136 FORMAT ('RSITE: Sub  grid topography number of fields = ',I4)
 2131 FORMAT ('RSITE: Fine grid topography filename  = ',256A1)
 2137 FORMAT ('RSITE: Fine grid topography filevalue = ',F10.3,' m')
 2140 FORMAT ('RSITE: Sub  grid topography filename  = ',256A1)
 2145 FORMAT ('RSITE: Sub  grid topography filevalue = ',F10.3,' m')
 2150 FORMAT ('RSITE: Height of buildings  filename  = ',256A1)
 2155 FORMAT ('RSITE: Height of buildings  filevalue = ',F10.3,' m')
!_NEST_End.

!     End of subroutine RSITE
      end subroutine rsite
