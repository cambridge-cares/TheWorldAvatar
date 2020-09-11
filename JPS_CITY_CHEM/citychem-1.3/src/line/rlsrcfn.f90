! <rlsrcfn.f90 - A component of the City-scale
!                 Chemistry Transport Model EPISDOE-CityChem>
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

      subroutine RLSRCFN

! *** The subroutine reads main line sources filenames.
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
!           2016  M. Karl: error handling for missing line source file
!           2017  M. Karl: reading LSRCSCALE and LSRCCANYON
!
! ----------------------------------------------------------------------------------

      use mod_util
      use mod_main
      use mod_site
      use mod_time
      use mod_mete
      use mod_conc
      use mod_lsrc

      implicit none

! *** Local variables

      REAL QLRMINV
      REAL QLRDV(2)
      INTEGER I
      INTEGER IQL
      character(len=256) :: TXTSTR
      LOGICAL LEOF
      INTEGER IC

! *** QLRMINV - Line source minimum influence distance
! *** QLRDV   - Line source distance to receptor points
! *** I       - Index
! *** IQL     - Line source index
! *** TXTSTR  - Textstring
! *** LEOF    - If end of file then true else false

! *** Read line sources subgrid model add to result indicator:
      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,*) LSRCAI       ! Default = 1
          ENDIF
          print *,'rlsrcfn: add line sources: ',LSRCAI  
      ENDIF

      IF (LSRCAI .LT. 0) LSRCAI = 0
      IF (LSRCAI .GT. 1) LSRCAI = 1

      IF (MAINFE) WRITE (MESSUN,2000) LSRCAI

! *** Read line sources static data filename:
      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          print *,'rlsrcfn: static line src file: ',TXTSTR
          IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,1000) LSRSFN
          ELSE

! ***        Read minimum influence distance and distances to receptor
! ***        points on each side of each road. These data are not
! ***        coming from AirQUIS.

              IF (NQL .GT. 0) THEN
                  IF (.NOT. ALLOCATED(QLRMIN)) ALLOCATE (QLRMIN(NQL))
                  IF (.NOT. ALLOCATED(QLRD))   ALLOCATE (QLRD(2*NQL))
              ENDIF


              READ (TXTSTR(2:256),*) QLRMINV,QLRDV(1),QLRDV(2)
              DO IQL = 1,NQL
                  QLRMIN(IQL)     = QLRMINV
                  QLRD(2*IQL - 1) = QLRDV(1)
                  QLRD(2*IQL    ) = QLRDV(2)
              ENDDO
              
          ENDIF
          
!MSK      ELSE
!MSK! ***     If we get data from AirQUIS:
!MSK          TXTSTR(1:1) = '<'
!MSK          CALL GETFNV(TXTSTR,LSRSFN,LSRSFE,1,1,LSRSFV)
      ENDIF


!      IF (MESSFE) WRITE (MESSUN,2010)
!     &            (LSRSFN(I:I),I=1,INDEX(LSRSFN,' ') - 1)

      IF (.NOT. ALLOCATED(LSRVFN)) ALLOCATE(LSRVFN(NC))
      IF (.NOT. ALLOCATED(LSRVUN)) ALLOCATE(LSRVUN(NC))
      IF (.NOT. ALLOCATED(LSRVFE)) ALLOCATE(LSRVFE(NC))
      IF (.NOT. ALLOCATED(LSRVFV)) ALLOCATE(LSRVFV(NC))
      IF (.NOT. ALLOCATED(LSRVFM)) ALLOCATE(LSRVFM(NC))

! *** Read line sources variable data filename:
!MSK There is a variable emission file for each compound
      DO 110 IC = 1,NC

! *** Read line source filenames or values:
        IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          print *,'rlsrcfn: line sources: ',TXTSTR
!MSK start
          if(TXTSTR(1:1)==' ') then
             LSRVFE(IC) = .false.
             LSRVFV(IC) = MISS
             print *,'no line src file for ',ic
          else
            CALL GETFNV(TXTSTR,LSRVFN(IC),LSRVFE(IC),1,1,LSRVFV(IC))
          endif
!MSK end
        ELSE
!MSK start
          IF ( LSRVFN(IC)(1:1) .EQ. ' ') THEN
            TXTSTR(1:1) = '<'      
          ELSE
            TXTSTR = LSRVFN(IC)
          ENDIF
!MSK end
          CALL GETFNV(TXTSTR,LSRVFN(IC),LSRVFE(IC),1,1,LSRVFV(IC))
        ENDIF

  110 CONTINUE

!          CALL GETDAT(MAINUN,TXTSTR,LEOF)
!          print *,'rlsrcfn: dynamic line src file: ',TXTSTR
!          IF (TXTSTR(1:1) .NE. '<') THEN
!              READ (TXTSTR,1000) LSRVFN
!          ENDIF
!      ELSE
!          TXTSTR(1:1) = '<'
!          CALL GETFNV(TXTSTR,LSRVFN,LSRVFE,1,1,LSRVFV)
!      ENDIF
!MSK end

!MSK start
! *** Read emission scaling factor for subgrid scale line source model:
      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,*) LSRCSCALE
          ENDIF
          print *,'rlsrcfn:  emission scaling factor: ',LSRCSCALE
      ENDIF

      IF (MESSFE) WRITE (MESSUN,2040) LSRCSCALE
!MSK end

!MSK start
! *** Read street canyon option for subgrid scale line source model:
      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,*) LSRCCANYON
          ENDIF
          print *,'rlsrcfn:  street canyon option: ',LSRCCANYON
      ENDIF

      IF (MESSFE) WRITE (MESSUN,2050) LSRCCANYON
!MSK end

! *** Read minimum windspeed for subgrid scale line source model:
      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,*) LSRCFFMIN
          ENDIF
          print *,'rlsrcfn:  min wind speed: ',LSRCFFMIN
      ENDIF

      IF (MESSFE) WRITE (MESSUN,2030) LSRCFFMIN

      RETURN

 1000 format (A256)

 2000 format ('RLSRCFN: Line sources sub grid model add to',       &
             ' indicator = ',I3)
 2010 format ('RLSRCFN: Line sources static   data file = ',256A1)
 2020 format ('RLSRCFN: Line sources variable data file = ',256A1)
 2030 format ('RLSRCFN: Minimum windspeed for line source subgrid',       &
             ' model = ',F7.2,' m/s')
!MSK start 
 2040 format ('RLSRCFN: Emission scaling for line source subgrid',       &
             ' model = ',F7.2)
 2050 format ('RLSRCFN: Line sources sub grid model street canyon',       &
             ' option = ',I3)
!MSK end

! *** End of subroutine RLSRCFN

      end subroutine rlsrcfn
