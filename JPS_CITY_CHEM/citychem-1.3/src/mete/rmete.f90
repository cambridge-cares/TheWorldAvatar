! <rmete.f90 - A component of the City-scale
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

      subroutine rmete

! The subroutine reads main meteorological data from file.
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
!           2016  M. Karl: Commented read minimum values of sigma-v and sigma-w
!           2017  M. Karl: if pimeteexternaldata>2 TAPM input files must be provided
!
! ----------------------------------------------------------------------------------

      use mod_util
      use mod_main
      use mod_site
      use mod_time
      use mod_mete

          implicit none

! Local variables

      INTEGER I
      INTEGER IZ
      CHARACTER(len=256) TXTSTR
      LOGICAL LEOF

! I      - Textstring index
! IZ     - Main grid index in z-direction
! TXTSTR - Text string
! LEOF   - If end of file then true else false 

      IF (.NOT. ALLOCATED(SDVWFV))  ALLOCATE(SDVWFV(2*NZ))
      IF (.NOT. ALLOCATED(WINDFV))  ALLOCATE(WINDFV(2*NZ))
      IF (.NOT. ALLOCATED(DTDZ))    ALLOCATE(DTDZ(NX,NY))
      IF (.NOT. ALLOCATED(TAIR))    ALLOCATE(TAIR(NX,NY))
      IF (.NOT. ALLOCATED(STAB))    ALLOCATE(STAB(NX,NY))
      IF (.NOT. ALLOCATED(U))       ALLOCATE(U(NX,NY,NZ))
      IF (.NOT. ALLOCATED(V))       ALLOCATE(V(NX,NY,NZ))
      IF (.NOT. ALLOCATED(W))       ALLOCATE(W(NX,NY,NZ))
      IF (.NOT. ALLOCATED(CLOU))    ALLOCATE(CLOU(NX,NY))
      IF (.NOT. ALLOCATED(HMIX))    ALLOCATE(HMIX(NX,NY))
      IF (.NOT. ALLOCATED(PREC))    ALLOCATE(PREC(NX,NY))
      IF (.NOT. ALLOCATED(RHUM))    ALLOCATE(RHUM(NX,NY))
      IF (.NOT. ALLOCATED(AERO))    ALLOCATE(AERO(NX,NY))
      IF (.NOT. ALLOCATED(D))       ALLOCATE(D(NZ))
      IF (.NOT. ALLOCATED(SIGV))    ALLOCATE(SIGV(NX,NY,NZ))
      IF (.NOT. ALLOCATED(SIGW))    ALLOCATE(SIGW(NX,NY,NZ))
      IF (.NOT. ALLOCATED(DIVE))    ALLOCATE(DIVE(NX,NY,NZ))
      IF (.NOT. ALLOCATED(DZDT))    ALLOCATE(DZDT(NX,NY,NZ))
      IF (.NOT. ALLOCATED(TLGR))    ALLOCATE(TLGR(NX,NY,NZ))
      IF (.NOT. ALLOCATED(MOBUL))   ALLOCATE(MOBUL(NX,NY))
      IF (.NOT. ALLOCATED(TSTAR))   ALLOCATE(TSTAR(NX,NY))
      IF (.NOT. ALLOCATED(USTAR))   ALLOCATE(USTAR(NX,NY))
!_LHS_15_Oct_2004_Start:
      IF (.NOT. ALLOCATED(WSTAR))   ALLOCATE(WSTAR(NX,NY))
!_LHS_15_Oct_2004_End.
      IF (.NOT. ALLOCATED(SIGWP))   ALLOCATE(SIGWP(NX,NY,NZ))
      IF (.NOT. ALLOCATED(TLGRP))   ALLOCATE(TLGRP(NX,NY,NZ))
      IF (.NOT. ALLOCATED(SIGVMIN)) ALLOCATE(SIGVMIN(NZ))
      IF (.NOT. ALLOCATED(SIGWMIN)) ALLOCATE(SIGWMIN(NZ))
!_LHS_08_Oct_2004_Start:
      IF (.NOT. ALLOCATED(USTR))    ALLOCATE(USTR(NX,NY))
      IF (.NOT. ALLOCATED(SHFL))    ALLOCATE(SHFL(NX,NY))
      IF (.NOT. ALLOCATED(LHFL))    ALLOCATE(LHFL(NX,NY))
      IF (.NOT. ALLOCATED(LANU))    ALLOCATE(LANU(NX,NY))
!MSK      IF (.NOT. ALLOCATED(KZ_H))    ALLOCATE(KZ_H(NX,NY,NZ))
!MSK      IF (.NOT. ALLOCATED(KZ_HFV))  ALLOCATE(KZ_HFV(NZ))
!MSK      IF (.NOT. ALLOCATED(KZ_M))    ALLOCATE(KZ_M(NX,NY,NZ))
!MSK      IF (.NOT. ALLOCATED(KZ_MFV))  ALLOCATE(KZ_MFV(NZ))
!MSK start
      IF (.NOT. ALLOCATED(PVSTR))    ALLOCATE(PVSTR(NX,NY))
      IF (.NOT. ALLOCATED(PTSTR))    ALLOCATE(PTSTR(NX,NY))
      IF (.NOT. ALLOCATED(WSTR))     ALLOCATE(WSTR(NX,NY))
      IF (.NOT. ALLOCATED(GTMP))     ALLOCATE(GTMP(NX,NY))
      IF (.NOT. ALLOCATED(TSRAD))    ALLOCATE(TSRAD(NX,NY))
      IF (.NOT. ALLOCATED(GPOT))     ALLOCATE(GPOT(NX,NY,NZ))
      IF (.NOT. ALLOCATED(POT_T))    ALLOCATE(POT_T(NX,NY,NZ))
      IF (.NOT. ALLOCATED(INS_T))    ALLOCATE(INS_T(NX,NY,NZ))
      IF (.NOT. ALLOCATED(MFLX))     ALLOCATE(MFLX(NX,NY))
      IF (.NOT. ALLOCATED(TAUS_X))   ALLOCATE(TAUS_X(NX,NY))
      IF (.NOT. ALLOCATED(TAUS_Y))   ALLOCATE(TAUS_Y(NX,NY))
!MSK end

!_LHS_08_Oct_2004_End.

!MSK  Skip one entry
      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          print *,'RMETE: skip line ', TXTSTR
       ENDIF

! Read * ITURB (= 1 If Delta-T, = 2 Global-radiation data)

      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,*) ITURB
          ENDIF
          print *,'rmete: ITURB method ',ITURB
      ENDIF

!MSK start
! Read IVERTDIFF (1 = NILU Standard (NILU_METHOD 4), 2 = New Urban (NILU_METHOD 6))

      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,*) IVERTDIFF
          ENDIF
          print *,'rmete: IVERTDIFF method ',IVERTDIFF
      ENDIF

!MSK end 

! Read delta temperature lower and upper measurement heights

      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,*) ZT_LOWER,ZT_UPPER     
          ENDIF
          print *,'rmete: dT heights ',ZT_LOWER,ZT_UPPER
      ENDIF


! Read air temperature filename or values

      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          print *,'rmete: temperature file: ',TXTSTR
          CALL GETFNV(TXTSTR,TEMPFN,TEMPFE,2,2,TEMPFV)
      ELSE
          IF (TEMPFN(1:1) .EQ. ' ') THEN
            TXTSTR(1:1) = '<'
          ELSE
            TXTSTR = TEMPFN
          ENDIF
          CALL GETFNV(TXTSTR,TEMPFN,TEMPFE,2,2,TEMPFV)
      ENDIF

      if (.not.TEMPFE) then
         CALL STOPIT('T_and_DTDZ FILE NOT EXISTING')
      endif

      IF (MESSFE) THEN
          WRITE (MESSUN,2000) &
           (TEMPFN(I:I),I=1,INDEX(TEMPFN,' ') - 1)
          WRITE (MESSUN,2005) (TEMPFV(I),I=1,2)
      ENDIF

!MSK  Skip iant_comp entry
      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          print *,'RMETE: skip line ', TXTSTR
       ENDIF

! Read wind filename or values

      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          print *,'rmete: wind file: ',TXTSTR
          CALL GETFNV(TXTSTR,WINDFN,WINDFE,2*NZ,2*MZ,WINDFV)
      ELSE
          IF (WINDFN(1:1) .EQ. ' ') THEN
            TXTSTR(1:1) = '<'
          ELSE
            TXTSTR = WINDFN
          ENDIF
          CALL GETFNV(TXTSTR,WINDFN,WINDFE,2*NZ,2*MZ,WINDFV)
      ENDIF

      if (.not.WINDFE) then
         CALL STOPIT('WIND FILE NOT EXISTING')
      endif

      IF (MESSFE) THEN
          WRITE (MESSUN,2010) &
           (WINDFN(I:I),I=1,INDEX(WINDFN,' ') - 1)
          WRITE (MESSUN,2015) (WINDFV(I),I=1,2*NZ)
      ENDIF

!MSK Could not find the next meteo files in main input
! Read turbulence sigma-vw filename or values

      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          print *,'rmete: turbulence sigma-vw file: ',TXTSTR
          if(TXTSTR(1:1)==' ') then
             SDVWFE=.false.
             print *,'no sigma-vw file'
          else
            CALL GETFNV(TXTSTR,SDVWFN,SDVWFE,2*NZ,2*MZ,SDVWFV)
          endif
      ELSE
          TXTSTR(1:1) = '<'
          CALL GETFNV(TXTSTR,SDVWFN,SDVWFE,2*NZ,2*MZ,SDVWFV)
      ENDIF

      IF (MESSFE) THEN
          WRITE (MESSUN,2020) &
           (SDVWFN(I:I),I=1,INDEX(SDVWFN,' ') - 1)
          WRITE (MESSUN,2025) (SDVWFV(I),I=1,2*NZ)
      ENDIF

!MSK start - no such file
! Read minimum values of sigma-v and sigma-w
!
!      IF (MAINFE) THEN
!          CALL GETDAT(MAINUN,TXTSTR,LEOF)
!          IF (TXTSTR(1:1) .NE. '<') THEN
!              READ (TXTSTR,*) (SIGVMIN(I),SIGWMIN(I),I=1,NZ)      
!          ENDIF
!      ENDIF
!
!      DO IZ = 1,NZ
!          IF (SIGVMIN(IZ) .LT. 0.) SIGVMIN(IZ) = 0.2
!          IF (SIGWMIN(IZ) .LT. 0.) SIGWMIN(IZ) = 0.1
!      ENDDO
!
!      IF (MESSFE) THEN
!	    WRITE (MESSUN,2026) (SIGVMIN(I),SIGWMIN(I),I=1,NZ)
!      ENDIF
      DO IZ = 1,NZ
         SIGVMIN(IZ) = 0.2
         SIGWMIN(IZ) = 0.1
      ENDDO
!MSK end

! Read aerodynamic resistance filename or value

      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          print *,'rmete: aerodynamic resistence file: ',TXTSTR
          if(TXTSTR(1:1)==' ') then
             AEROFE=.false.
!MSK start
             AEROFV=MISS
!MSK end
             print *,'no aero resist file'
          else
            CALL GETFNV(TXTSTR,AEROFN,AEROFE,1,1,AEROFV)
          endif
      ELSE
          TXTSTR(1:1) = '<'
          CALL GETFNV(TXTSTR,AEROFN,AEROFE,1,1,AEROFV)
      ENDIF

      IF (MESSFE) THEN
          WRITE (MESSUN,2030)    &
               (AEROFN(I:I),I=1,INDEX(AEROFN,' ') - 1)
          WRITE (MESSUN,2035) AEROFV
      ENDIF

! Read mixing height filename or value

      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          print *,'rmete: hmix file: ',TXTSTR
          if(TXTSTR(1:1)==' ') then
             HMIXFE=.false.
             print *,'no hmix file'
             if (pimeteexternaldata>2) then
               CALL STOPIT('TAPM HMIX FILE MUST BE PROVIDED')
             endif
          else
            CALL GETFNV(TXTSTR,HMIXFN,HMIXFE,1,1,HMIXFV)
            if ((.not.HMIXFE).and.(pimeteexternaldata>2)) then
              CALL STOPIT('TAPM HMIX FILE NOT EXISTING')
            endif
          endif
      ELSE
          IF (HMIXFN(1:1) .EQ. ' ') THEN
            TXTSTR(1:1) = '<'
          ELSE
            TXTSTR = HMIXFN
          ENDIF
          CALL GETFNV(TXTSTR,HMIXFN,HMIXFE,1,1,HMIXFV)
      ENDIF

      IF (MESSFE) THEN
          WRITE (MESSUN,2040) &
           (HMIXFN(I:I),I=1,INDEX(HMIXFN,' ') - 1)
          WRITE (MESSUN,2045) HMIXFV
      ENDIF

!_LHS_08_Oct_2004_Start:

! Read (MM5) Surface friction velocity filename or value

      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          print *,'rmete: friction velocity file: ',TXTSTR
          if(TXTSTR(1:1)==' ') then
             USTRFE=.false.
             print *,'no ustar file'
             if (pimeteexternaldata>2) then
               CALL STOPIT('TAPM USTAR FILE MUST BE PROVIDED')
             endif
          else
            CALL GETFNV(TXTSTR,USTRFN,USTRFE,1,1,USTRFV)
            if ((.not.USTRFE).and.(pimeteexternaldata>2)) then
              CALL STOPIT('TAPM USTAR FILE NOT EXISTING')
            endif
          endif
      ELSE
          IF (USTRFN(1:1) .EQ. ' ') THEN
            TXTSTR(1:1) = '<'
          ELSE
            TXTSTR = USTRFN
          ENDIF
          CALL GETFNV(TXTSTR,USTRFN,USTRFE,1,1,USTRFV)
      ENDIF

      IF (MESSFE) THEN
          WRITE (MESSUN,2041) &
           (USTRFN(I:I),I=1,INDEX(USTRFN,' ') - 1)
          WRITE (MESSUN,2046) USTRFV
      ENDIF

! Read (MM5) Surface sensible heat flux filename or value

      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          print *,'rmete: sensible heat flux file: ',TXTSTR
          if(TXTSTR(1:1)==' ') then
             SHFLFE=.false.
             print *,'no sens heat file'
             if (pimeteexternaldata>2) then
               CALL STOPIT('TAPM SHFL FILE MUST BE PROVIDED')
             endif
          else
            CALL GETFNV(TXTSTR,SHFLFN,SHFLFE,1,1,SHFLFV)
            if ((.not.SHFLFE).and.(pimeteexternaldata>2)) then
              CALL STOPIT('TAPM SHFL FILE NOT EXISTING')
            endif
         endif
      ELSE
          IF (SHFLFN(1:1) .EQ. ' ') THEN
            TXTSTR(1:1) = '<'
          ELSE
            TXTSTR = SHFLFN
          ENDIF
          CALL GETFNV(TXTSTR,SHFLFN,SHFLFE,1,1,SHFLFV)
      ENDIF

      IF (MESSFE) THEN
          WRITE (MESSUN,2042) &
           (SHFLFN(I:I),I=1,INDEX(SHFLFN,' ') - 1)
          WRITE (MESSUN,2047) SHFLFV
      ENDIF

! Read (MM5) Surface latent heat flux filename or value

      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          print *,'rmete: latent heat flux file: ',TXTSTR
          if(TXTSTR(1:1)==' ') then
             LHFLFE=.false.
             print *,'no lat heat file'
             if (pimeteexternaldata>2) then
               CALL STOPIT('TAPM LHFL FILE MUST BE PROVIDED')
             endif
          else
            CALL GETFNV(TXTSTR,LHFLFN,LHFLFE,1,1,LHFLFV)
            if ((.not.LHFLFE).and.(pimeteexternaldata>2)) then
              CALL STOPIT('TAPM LHFL FILE NOT EXISTING')
            endif
          endif
      ELSE
          IF (LHFLFN(1:1) .EQ. ' ') THEN
            TXTSTR(1:1) = '<'
          ELSE
            TXTSTR = LHFLFN
          ENDIF
          CALL GETFNV(TXTSTR,LHFLFN,LHFLFE,1,1,LHFLFV)
      ENDIF

      IF (MESSFE) THEN
          WRITE (MESSUN,2043) &
           (LHFLFN(I:I),I=1,INDEX(LHFLFN,' ') - 1)
          WRITE (MESSUN,2048) LHFLFV
      ENDIF

! Read (MM5) Land-use category filename or value

      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          print *,'rmete: land use file: ',TXTSTR
          if(TXTSTR(1:1)==' ') then
             LANUFE=.false.
             print *,'no land use file'
          else
            CALL GETFNV(TXTSTR,LANUFN,LANUFE,1,1,LANUFV)
            if (.not.LANUFE) then
              CALL STOPIT('TAPM LANU FILE NOT EXISTING')
            endif
          endif
      ELSE
          IF (LANUFN(1:1) .EQ. ' ') THEN
            TXTSTR(1:1) = '<'
          ELSE
            TXTSTR = LANUFN
          ENDIF
          CALL GETFNV(TXTSTR,LANUFN,LANUFE,1,1,LANUFV)
      ENDIF

      IF (MESSFE) THEN
          WRITE (MESSUN,2044) &
           (LANUFN(I:I),I=1,INDEX(LANUFN,' ') - 1)
          WRITE (MESSUN,2049) LANUFV
      ENDIF


!MSK start  TAPM met input files

! Read TAPM 3D insitu temperature filename
!

      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          print *,'rmete: 3d-ins_T file: ',TXTSTR
          if(TXTSTR(1:1)==' ') then
             INS_TFE=.false.
             print *,'no 3d-ins_T file'
             if (pimeteexternaldata>2) then
               CALL STOPIT('TAPM 3D INST FILE MUST BE PROVIDED')
             endif
          else
            CALL GETFNV(TXTSTR,INS_TFN,INS_TFE,1,1,INS_TFV)
            if ((.not.INS_TFE).and.(pimeteexternaldata>2)) then
              CALL STOPIT('TAPM 3D INST FILE NOT EXISTING')
            endif
          endif
      ELSE
          IF (INS_TFN(1:1) .EQ. ' ') THEN
            TXTSTR(1:1) = '<'
          ELSE
            TXTSTR = INS_TFN
          ENDIF
          CALL GETFNV(TXTSTR,INS_TFN,INS_TFE,1,1,INS_TFV)
      ENDIF

      IF (MESSFE) THEN
          WRITE (MESSUN,2095) &
           (INS_TFN(I:I),I=1,INDEX(INS_TFN,' ') - 1)
          WRITE (MESSUN,2105) INS_TFV
      ENDIF


! Read TAPM 3D potential temperature filename
!

      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          print *,'rmete: 3d-pot_T file: ',TXTSTR
          if(TXTSTR(1:1)==' ') then
             POT_TFE=.false.
             print *,'no 3d-pot_T file'
             if (pimeteexternaldata>2) then
               CALL STOPIT('TAPM 3D INST FILE MUST BE PROVIDED')
             endif
          else
            CALL GETFNV(TXTSTR,POT_TFN,POT_TFE,1,1,POT_TFV)
            if ((.not.POT_TFE).and.(pimeteexternaldata>2)) then
              CALL STOPIT('TAPM 3D POTT FILE NOT EXISTING')
            endif
          endif
      ELSE
          IF (POT_TFN(1:1) .EQ. ' ') THEN
            TXTSTR(1:1) = '<'
          ELSE
            TXTSTR = POT_TFN
          ENDIF
          CALL GETFNV(TXTSTR,POT_TFN,POT_TFE,1,1,POT_TFV)
      ENDIF

      IF (MESSFE) THEN
          WRITE (MESSUN,2096) &
           (POT_TFN(I:I),I=1,INDEX(POT_TFN,' ') - 1)
          WRITE (MESSUN,2106) POT_TFV
      ENDIF

! Read TAPM level midpoint filename
!

      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          print *,'rmete: 3d-gpot file: ',TXTSTR
          if(TXTSTR(1:1)==' ') then
             GPOTFE=.false.
             print *,'no 3d-gpot file'
             if (pimeteexternaldata>2) then
               CALL STOPIT('TAPM 3D GPOT FILE MUST BE PROVIDED')
             endif
          else
            CALL GETFNV(TXTSTR,GPOTFN,GPOTFE,1,1,GPOTFV)
            if ((.not.GPOTFE).and.(pimeteexternaldata>2)) then
              CALL STOPIT('TAPM 3D GPOT FILE NOT EXISTING')
            endif
          endif
      ELSE
          IF (GPOTFN(1:1) .EQ. ' ') THEN
            TXTSTR(1:1) = '<'
          ELSE
            TXTSTR = GPOTFN
          ENDIF
          CALL GETFNV(TXTSTR,GPOTFN,GPOTFE,1,1,GPOTFV)
      ENDIF

      IF (MESSFE) THEN
          WRITE (MESSUN,2097) &
           (GPOTFN(I:I),I=1,INDEX(GPOTFN,' ') - 1)
          WRITE (MESSUN,2107) GPOTFV
      ENDIF

!MSK end

!MSK start     UM met input files

! Read Surface water vapor flux: From "mflx_episode.asc", given in kg m-2 s-1.

      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          print *,'rmete: surface moisture flux file: ',TXTSTR
          if(TXTSTR(1:1)==' ') then
             MFLXFE=.false.
             print *,'no surf moist file'
          else
            CALL GETFNV(TXTSTR,MFLXFN,MFLXFE,1,1,MFLXFV)
          endif
      ELSE
          IF (MFLXFN(1:1) .EQ. ' ') THEN
            TXTSTR(1:1) = '<'
          ELSE
            TXTSTR = MFLXFN
          ENDIF
          CALL GETFNV(TXTSTR,MFLXFN,MFLXFE,1,1,MFLXFV)
      ENDIF

      IF (MESSFE) THEN
          WRITE (MESSUN,2080) &
           (LHFLFN(I:I),I=1,INDEX(MFLXFN,' ') - 1)
          WRITE (MESSUN,2085) MFLXFV
      ENDIF


! Read Surface momentum stress: From "sfws_episode.asc", given in Nm-2.

      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          print *,'rmete: momentum stress file: ',TXTSTR
          if(TXTSTR(1:1)==' ') then
             TAUSFE=.false.
             print *,'no momentum stress file'
          else
            CALL GETFNV(TXTSTR,TAUSFN,TAUSFE,1,1,TAUSFV)
          endif
      ELSE
          IF (TAUSFN(1:1) .EQ. ' ') THEN
            TXTSTR(1:1) = '<'
          ELSE
            TXTSTR = TAUSFN
          ENDIF
          CALL GETFNV(TXTSTR,TAUSFN,TAUSFE,1,1,TAUSFV)
      ENDIF

      IF (MESSFE) THEN
          WRITE (MESSUN,2081) &
           (LHFLFN(I:I),I=1,INDEX(TAUSFN,' ') - 1)
          WRITE (MESSUN,2086) TAUSFV
      ENDIF

!MSK end


!MSK start  TAPM met input files

! Read potential temperature scale filename
!

      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          print *,'rmete: ptstar file: ',TXTSTR
          if(TXTSTR(1:1)==' ') then
             PTSTRFE=.false.
             print *,'no ptstar file'
             if (pimeteexternaldata>2) then
               CALL STOPIT('TAPM PTSTAR FILE MUST BE PROVIDED')
             endif
          else
            CALL GETFNV(TXTSTR,PTSTRFN,PTSTRFE,1,1,PTSTRFV)
            if ((.not.PTSTRFE).and.(pimeteexternaldata>2)) then
              CALL STOPIT('TAPM PTSTAR FILE NOT EXISTING')
            endif
          endif
      ELSE
          IF (PTSTRFN(1:1) .EQ. ' ') THEN
            TXTSTR(1:1) = '<'
          ELSE
            TXTSTR = PTSTRFN
          ENDIF
          CALL GETFNV(TXTSTR,PTSTRFN,PTSTRFE,1,1,PTSTRFV)
      ENDIF

      IF (MESSFE) THEN
          WRITE (MESSUN,2098) &
           (PTSTRFN(I:I),I=1,INDEX(PTSTRFN,' ') - 1)
          WRITE (MESSUN,2108) PTSTRFV
      ENDIF


! Read potential virtual temperature scale filename
!

      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          print *,'rmete: pvstar file: ',TXTSTR
          if(TXTSTR(1:1)==' ') then
             PVSTRFE=.false.
             print *,'no pvstar file'
             if (pimeteexternaldata>2) then
               CALL STOPIT('TAPM PVSTAR FILE MUST BE PROVIDED')
             endif
          else
            CALL GETFNV(TXTSTR,PVSTRFN,PVSTRFE,1,1,PVSTRFV)
            if ((.not.PVSTRFE).and.(pimeteexternaldata>2)) then
              CALL STOPIT('TAPM PVSTAR FILE NOT EXISTING')
            endif
          endif
      ELSE
          IF (PVSTRFN(1:1) .EQ. ' ') THEN
            TXTSTR(1:1) = '<'
          ELSE
            TXTSTR = PVSTRFN
          ENDIF
          CALL GETFNV(TXTSTR,PVSTRFN,PVSTRFE,1,1,PVSTRFV)
      ENDIF

      IF (MESSFE) THEN
          WRITE (MESSUN,2099) &
           (PVSTRFN(I:I),I=1,INDEX(PVSTRFN,' ') - 1)
          WRITE (MESSUN,2109) PVSTRFV
      ENDIF


! Read convective velocity scale filename
!

      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          print *,'rmete: wstar file: ',TXTSTR
          if(TXTSTR(1:1)==' ') then
             WSTRFE=.false.
             print *,'no wstar file'
             if (pimeteexternaldata>2) then
               CALL STOPIT('TAPM WSTAR FILE MUST BE PROVIDED')
             endif
          else
            CALL GETFNV(TXTSTR,WSTRFN,WSTRFE,1,1,WSTRFV)
            if ((.not.WSTRFE).and.(pimeteexternaldata>2)) then
              CALL STOPIT('TAPM WSTAR FILE NOT EXISTING')
            endif
          endif
      ELSE
          IF (WSTRFN(1:1) .EQ. ' ') THEN
            TXTSTR(1:1) = '<'
          ELSE
            TXTSTR = WSTRFN
          ENDIF
          CALL GETFNV(TXTSTR,WSTRFN,WSTRFE,1,1,WSTRFV)
      ENDIF

      IF (MESSFE) THEN
          WRITE (MESSUN,2100) &
           (WSTRFN(I:I),I=1,INDEX(WSTRFN,' ') - 1)
          WRITE (MESSUN,2110) WSTRFV
      ENDIF

! Read total solar radiation filename
!

      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          print *,'rmete: tsrad file: ',TXTSTR
          if(TXTSTR(1:1)==' ') then
             TSRADFE=.false.
             print *,'no tsrad file'
             if (pimeteexternaldata>2) then
               CALL STOPIT('TAPM TSRAD FILE MUST BE PROVIDED')
             endif
          else
            CALL GETFNV(TXTSTR,TSRADFN,TSRADFE,1,1,TSRADFV)
            if ((.not.TSRADFE).and.(pimeteexternaldata>2)) then
              CALL STOPIT('TAPM TSRAD FILE NOT EXISTING')
            endif
          endif
      ELSE
          IF (TSRADFN(1:1) .EQ. ' ') THEN
            TXTSTR(1:1) = '<'
          ELSE
            TXTSTR = TSRADFN
          ENDIF
          CALL GETFNV(TXTSTR,TSRADFN,TSRADFE,1,1,TSRADFV)
      ENDIF

      IF (MESSFE) THEN
          WRITE (MESSUN,2101) &
           (TSRADFN(I:I),I=1,INDEX(TSRADFN,' ') - 1)
          WRITE (MESSUN,2111) TSRADFV
      ENDIF


! Read ground temperature filename
!

      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          print *,'rmete: gtmp file: ',TXTSTR
          if(TXTSTR(1:1)==' ') then
             GTMPFE=.false.
             print *,'no gtmp file'
             if (pimeteexternaldata>2) then
               CALL STOPIT('TAPM GTMP FILE MUST BE PROVIDED')
             endif
          else
            CALL GETFNV(TXTSTR,GTMPFN,GTMPFE,1,1,GTMPFV)
            if ((.not.GTMPFE).and.(pimeteexternaldata>2)) then
              CALL STOPIT('TAPM GTMP FILE NOT EXISTING')
            endif
          endif
      ELSE
          IF (GTMPFN(1:1) .EQ. ' ') THEN
            TXTSTR(1:1) = '<'
          ELSE
            TXTSTR = GTMPFN
          ENDIF
          CALL GETFNV(TXTSTR,GTMPFN,GTMPFE,1,1,GTMPFV)
      ENDIF

      IF (MESSFE) THEN
          WRITE (MESSUN,2102) &
           (GTMPFN(I:I),I=1,INDEX(GTMPFN,' ') - 1)
          WRITE (MESSUN,2112) GTMPFV
      ENDIF

!MSK end

! Read precipitation filename or value

      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          print *,'rmete: precipitation file: ',TXTSTR
          CALL GETFNV(TXTSTR,PRECFN,PRECFE,1,1,PRECFV)
      ELSE
          IF (PRECFN(1:1) .EQ. ' ') THEN
            TXTSTR(1:1) = '<'
          ELSE
            TXTSTR = PRECFN
          ENDIF
          CALL GETFNV(TXTSTR,PRECFN,PRECFE,1,1,PRECFV)
      ENDIF

      if (.not.PRECFE) then
         CALL STOPIT('PREC FILE NOT EXISTING')
      endif

      IF (MESSFE) THEN
          WRITE (MESSUN,2050) &
           (PRECFN(I:I),I=1,INDEX(PRECFN,' ') - 1)
          WRITE (MESSUN,2055) PRECFV
      ENDIF

! Read relative humidity filename or value

      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          print *,'rmete: rel humidity file: ',TXTSTR
          CALL GETFNV(TXTSTR,RHUMFN,RHUMFE,1,1,RHUMFV)
      ELSE
          IF (RHUMFN(1:1) .EQ. ' ') THEN
            TXTSTR(1:1) = '<'
          ELSE
            TXTSTR = RHUMFN
          ENDIF
          CALL GETFNV(TXTSTR,RHUMFN,RHUMFE,1,1,RHUMFV)
      ENDIF

      if (.not.RHUMFE) then
         CALL STOPIT('RHUM FILE NOT EXISTING')
      endif

      IF (MESSFE) THEN
          WRITE (MESSUN,2060) &
           (RHUMFN(I:I),I=1,INDEX(RHUMFN,' ') - 1)
          WRITE (MESSUN,2065) RHUMFV
      ENDIF

! Read cloud cover filename or value (0-1)

      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          print *,'rmete: cloud file: ',TXTSTR
          CALL GETFNV(TXTSTR,CLOUFN,CLOUFE,1,1,CLOUFV)
      ELSE
          IF (CLOUFN(1:1) .EQ. ' ') THEN
            TXTSTR(1:1) = '<'
          ELSE
            TXTSTR = CLOUFN
          ENDIF
          CALL GETFNV(TXTSTR,CLOUFN,CLOUFE,1,1,CLOUFV)
      ENDIF

      if (.not.CLOUFE) then
         CALL STOPIT('CLOU FILE NOT EXISTING')
      endif

      IF (MESSFE) THEN
          WRITE (MESSUN,2070) &
           (CLOUFN(I:I),I=1,INDEX(CLOUFN,' ') - 1)
          WRITE (MESSUN,2075) CLOUFV
      ENDIF


      RETURN

 1000 FORMAT (A256)

 2000 FORMAT ('RMETE: Air temperature ...... filename   = ',256A1)
 2005 FORMAT ('RMETE: Air temperature ...... filevalues = ',99F10.3)
 2010 FORMAT ('RMETE: Wind u and v ......... filename   = ',256A1)
 2015 FORMAT ('RMETE: Wind u and v ......... filevalues = ',99F10.3)
 2020 FORMAT ('RMETE: Turbulence sigma-vw .. filename   = ',256A1)
 2025 FORMAT ('RMETE: Turbulence sigma-vw .. filevalues = ',99F10.3)
 2026 FORMAT ('RMETE: Turbulence sigma-vw min. values   = ',99F10.3)
 2030 FORMAT ('RMETE: Aerodynamic resistance filename   = ',256A1)
 2035 FORMAT ('RMETE: Aerodynamic resistance filevalue  = ',F10.3, &
         ' s/m')
 2040 FORMAT ('RMETE: Mixing height ........ filename   = ',256A1)
 2045 FORMAT ('RMETE: Mixing height ........ filevalue  = ',F10.3, &
         ' m')

!_LHS_08_Oct_2004_Start:
 2041 FORMAT ('RMETE: Friction velocity..... filename   = ',256A1)
 2046 FORMAT ('RMETE: Friction velocity..... filevalue  = ',F10.4, &
         ' m/s')

 2042 FORMAT ('RMETE: Sensible heat flux.... filename   = ',256A1)
 2047 FORMAT ('RMETE: Sensible heat flux.... filevalue  = ',F10.3, &
         ' W/m2')

 2043 FORMAT ('RMETE: Latent heat flux...... filename   = ',256A1)
 2048 FORMAT ('RMETE: Latent heat flux...... filevalue  = ',F10.3, &
         ' W/m2')

 2044 FORMAT ('RMETE: Land-use category..... filename   = ',256A1)
 2049 FORMAT ('RMETE: Land-use category..... filevalue  = ',F10.3)

 2080 FORMAT ('RMETE: Surface moist flux ..... filename   = ',256A1)
 2085 FORMAT ('RMETE: Surface moist flux ..... filevalues = ',99F10.3)

 2081 FORMAT ('RMETE: Momentum stress . filename   = ',256A1)
 2086 FORMAT ('RMETE: Momentum sress . filevalues = ',99F10.3)

!MSK start

 2095 FORMAT ('RMETE: 3D Insitu air T . filename   = ',256A1)
 2105 FORMAT ('RMETE: 3D Insitu air T . filevalues = ',99F10.3)

 2096 FORMAT ('RMETE: 3D Potential T . filename   = ',256A1)
 2106 FORMAT ('RMETE: 3D Potential T . filevalues = ',99F10.3)

 2097 FORMAT ('RMETE: Level midpoint height . filename   = ',256A1)
 2107 FORMAT ('RMETE: Level midpoint height . filevalues = ',99F10.3)

 2098 FORMAT ('RMETE: Potential T scale . filename   = ',256A1)
 2108 FORMAT ('RMETE: Potential T scale . filevalues = ',99F10.3)

 2099 FORMAT ('RMETE: Potential virtual T scale . filename   = ',256A1)
 2109 FORMAT ('RMETE: Potential virtual T scale . filevalues = ',99F10.3)

 2100 FORMAT ('RMETE: Conv. velocity scale . filename   = ',256A1)
 2110 FORMAT ('RMETE: Conv. velocity scale . filevalues = ',99F10.3)

 2101 FORMAT ('RMETE: Total solar radiaion . filename   = ',256A1)
 2111 FORMAT ('RMETE: Total solar radiation . filevalues = ',99F10.3)

 2102 FORMAT ('RMETE: Ground temperature . filename   = ',256A1)
 2112 FORMAT ('RMETE: Ground temperature . filevalues = ',99F10.3)

!MSK end

!_LHS_08_Oct_2004_End.

 2050 FORMAT ('RMETE: Precipitation ........ filename   = ',256A1)
 2055 FORMAT ('RMETE: Precipitation ........ filevalue  = ',F10.3, &
         ' mm/h')
 2060 FORMAT ('RMETE: Relative humidity .... filename   = ',256A1)
 2065 FORMAT ('RMETE: Relative humidity .... filevalue  = ',F10.3)
 2070 FORMAT ('RMETE: Cloud cover .......... filename   = ',256A1)
 2075 FORMAT ('RMETE: Cloud cover .......... filevalue  = ',F10.3)

! End of subroutine RMETE

      END
