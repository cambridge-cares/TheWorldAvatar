! <rdepo.f90 - A component of the City-scale
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

      subroutine rdepo

! *** The subroutine reads main depositions data from file.
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
!           2016  M. Karl: no output of wet deposition fields wanted for now
!
! ----------------------------------------------------------------------------------

      USE mod_util
      USE mod_main
      USE mod_site
      USE mod_conc
      USE mod_depo

! *** Local variables

      INTEGER I
      INTEGER IC
      CHARACTER(len=256) TXTSTR
      LOGICAL LEOF

! *** I      - Index
! *** IC     - Index of compound
! *** TXTSTR - Text string
! *** LEOF   - If end of file then true else false

!MSK      IF (.NOT. ALLOCATED(DDEPMFN)) ALLOCATE(DDEPMFN(NC))
!MSK      IF (.NOT. ALLOCATED(WDEPMFN)) ALLOCATE(WDEPMFN(NC))
      IF (.NOT. ALLOCATED(DDEPSFN)) ALLOCATE(DDEPSFN(NC))
!MSK      IF (.NOT. ALLOCATED(WDEPSFN)) ALLOCATE(WDEPSFN(NC))

!MSK      IF (.NOT. ALLOCATED(DDEPMUN)) ALLOCATE(DDEPMUN(NC))
!MSK      IF (.NOT. ALLOCATED(WDEPMUN)) ALLOCATE(WDEPMUN(NC))
      IF (.NOT. ALLOCATED(DDEPSUN)) ALLOCATE(DDEPSUN(NC))
!MSK      IF (.NOT. ALLOCATED(WDEPSUN)) ALLOCATE(WDEPSUN(NC))

!MSK      IF (.NOT. ALLOCATED(DDEPMFE)) ALLOCATE(DDEPMFE(NC))
!MSK      IF (.NOT. ALLOCATED(WDEPMFE)) ALLOCATE(WDEPMFE(NC))
      IF (.NOT. ALLOCATED(DDEPSFE)) ALLOCATE(DDEPSFE(NC))
!MSK      IF (.NOT. ALLOCATED(WDEPSFE)) ALLOCATE(WDEPSFE(NC))

!MSK      IF (.NOT. ALLOCATED(DDEPMFV)) ALLOCATE(DDEPMFV(NC))
!MSK      IF (.NOT. ALLOCATED(WDEPMFV)) ALLOCATE(WDEPMFV(NC))
      IF (.NOT. ALLOCATED(DDEPSFV)) ALLOCATE(DDEPSFV(NC))
!MSK      IF (.NOT. ALLOCATED(WDEPSFV)) ALLOCATE(WDEPSFV(NC))

!MSK      IF (.NOT. ALLOCATED(DDEPMFM)) ALLOCATE(DDEPMFM(NC))
!MSK      IF (.NOT. ALLOCATED(WDEPMFM)) ALLOCATE(WDEPMFM(NC))
      IF (.NOT. ALLOCATED(DDEPSFM)) ALLOCATE(DDEPSFM(NC))
!MSK      IF (.NOT. ALLOCATED(WDEPSFM)) ALLOCATE(WDEPSFM(NC))

      IF (.NOT. ALLOCATED(DDEPOI))  ALLOCATE(DDEPOI(NC))
      IF (.NOT. ALLOCATED(WDEPOI))  ALLOCATE(WDEPOI(NC))
      IF (.NOT. ALLOCATED(DDEPV))   ALLOCATE(DDEPV(NC))
      IF (.NOT. ALLOCATED(WDEPSR))  ALLOCATE(WDEPSR(NC))

      IF (.NOT. ALLOCATED(DDEPM))   ALLOCATE(DDEPM(NX,NY,NC))

      IF (NR .GT. 0) THEN
          IF (.NOT. ALLOCATED(DDEPR)) ALLOCATE(DDEPR(NC,NR))
      ENDIF

      IF (NLS .GT. 0) THEN
          IF (.NOT. ALLOCATED(DDEPS)) ALLOCATE(DDEPS(NC,NXS,NYS,NLS))
      ENDIF
      IF (.NOT. ALLOCATED(WDEPM)) ALLOCATE(WDEPM(NX,NY,NC))

      IF (NR .GT. 0) THEN
          IF (.NOT. ALLOCATED(WDEPR)) ALLOCATE(WDEPR(NC,NR))
      ENDIF

      IF (NLS .GT. 0) THEN
          IF (.NOT. ALLOCATED(WDEPS)) ALLOCATE(WDEPS(NC,NXS,NYS,NLS))
      ENDIF

! *** Read main grid Dry Deposition filenames:
!MSK      DO 100 IC = 1,NC
!MSK
!MSK      IF (MAINFE) THEN
!MSK          CALL GETDAT(MAINUN,TXTSTR,LEOF)
!MSK          IF (TXTSTR(1:1) .NE. '<') THEN
!MSK              READ (TXTSTR,1000) DDEPMFN(IC)
!MSK          ENDIF
!MSK          print *,'main grid drydep filename',DDEPMFN(IC)
!MSK      ELSE
!MSK          TXTSTR(1:1) = '<'
!MSK          CALL GETFNV(TXTSTR,DDEPMFN(IC),DDEPMFE(IC),1,1, &
!MSK                     DDEPMFV(IC))
!MSK      ENDIF
!MSK  100 CONTINUE

! *** Read main grid Wet Deposition filenames:
!MSK      DO 110 IC = 1,NC
!MSK
!MSK      IF (MAINFE) THEN
!MSK          CALL GETDAT(MAINUN,TXTSTR,LEOF)
!MSK          IF (TXTSTR(1:1) .NE. '<') THEN
!MSK              READ (TXTSTR,1000) WDEPMFN(IC)
!MSK          ENDIF
!MSK          print *,'main grid wetdep filename',WDEPMFN(IC)
!MSK      ELSE
!MSK          TXTSTR(1:1) = '<'
!MSK          CALL GETFNV(TXTSTR,WDEPMFN(IC),WDEPMFE(IC),1,1,  &
!MSK                     WDEPMFV(IC))
!MSK      ENDIF
!MSK
!MSK  110 CONTINUE





!MSK Commented the below files not found in main input
! *** Read  sub grid Dry Deposition filenames:
!      DO 120 IC = 1,NC
!
!      IF (MAINFE) THEN
!          CALL GETDAT(MAINUN,TXTSTR,LEOF)
!          IF (TXTSTR(1:1) .NE. '<') THEN
!              READ (TXTSTR,1000) DDEPSFN(IC)
!          ENDIF
!      ELSE
!          TXTSTR(1:1) = '<'
!          CALL GETFNV(TXTSTR,DDEPSFN(IC),DDEPSFE(IC),1,1,  &
!                     DDEPSFV(IC))
!      ENDIF
!!      IF (MESSFE) THEN
!!          WRITE (MESSUN,2100)
!!     &    (DDEPSFN(IC)(I:I),I=1,INDEX(DDEPSFN(IC),' ') - 1)
!!      ENDIF
!
!  120 CONTINUE

! *** Read  sub grid Wet Deposition filenames:
!      DO 130 IC = 1,NC
!
!      IF (MAINFE) THEN
!          CALL GETDAT(MAINUN,TXTSTR,LEOF)
!          IF (TXTSTR(1:1) .NE. '<') THEN
!              READ (TXTSTR,1000) WDEPSFN(IC)
!          ENDIF
!      ELSE
!          TXTSTR(1:1) = '<'
!          CALL GETFNV(TXTSTR,WDEPSFN(IC),WDEPSFE(IC),1,1,  &
!                     WDEPSFV(IC))
!      ENDIF
!!      IF (MESSFE) THEN
!!          WRITE (MESSUN,2110)
!!     &    (WDEPSFN(IC)(I:I),I=1,INDEX(WDEPSFN(IC),' ') - 1)
!!      ENDIF
!
!  130 CONTINUE

!! *** Read receptors Dry Deposition filename (Only one compund?):
      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,1000) DDEPRFN
          ENDIF
          print *,'receptor point drydep filename',DDEPRFN
      ELSE
          TXTSTR(1:1) = '<'
          CALL GETFNV(TXTSTR,DDEPRFN,DDEPRFE,1,1,  &
                     DDEPRFV)
      ENDIF
!      IF (MESSFE) THEN
!          WRITE (MESSUN,2200)
!     &    (DDEPRFN(I:I),I=1,INDEX(DDEPRFN,' ') - 1)
!      ENDIF

! *** Read receptors Wet Deposition filename (Only one compund?):
      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,1000) WDEPRFN
          ENDIF
          print *,'receptor point wetdep filename',WDEPRFN
      ELSE
          TXTSTR(1:1) = '<'
          CALL GETFNV(TXTSTR,WDEPRFN,WDEPRFE,1,1,  &
                     WDEPRFV)
      ENDIF
!      IF (MESSFE) THEN
!          WRITE (MESSUN,2210)
!     &    (WDEPRFN(I:I),I=1,INDEX(WDEPRFN,' ') - 1)
!      ENDIF

! *** Read line sources Dry Deposition filename (Only one compound?):
      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,1000) DDEPLFN
          ENDIF
          print *,'line source drydep filename', DDEPLFN
      ELSE
          TXTSTR(1:1) = '<'
          CALL GETFNV(TXTSTR,DDEPLFN,DDEPLFE,1,1,  &
                     DDEPLFV)
      ENDIF
!      IF (MESSFE) THEN
!          WRITE (MESSUN,2300)
!     &    (DDEPLFN(I:I),I=1,INDEX(DDEPLFN,' ') - 1)
!      ENDIF

! *** Read line sources Wet Deposition filename (Only one compound?):
      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,1000) WDEPLFN
          ENDIF
          print *,'line source wetdep filename', WDEPLFN
      ELSE
          TXTSTR(1:1) = '<'
          CALL GETFNV(TXTSTR,WDEPLFN,WDEPLFE,1,1,  &
                     WDEPLFV)
      ENDIF
!      IF (MESSFE) THEN
!          WRITE (MESSUN,2310)
!     &    (WDEPLFN(I:I),I=1,INDEX(WDEPLFN,' ') - 1)
!      ENDIF


      RETURN

 1000 format (A256)

 2000 format ('RDEPO: Main grid dry depositions file = ',256A1)
 2010 format ('RDEPO: Main grid wet depositions file = ',256A1)
 2100 format ('RDEPO: Sub  grid dry depositions file = ',256A1)
 2110 format ('RDEPO: Sub  grid wet depositions file = ',256A1)
 2200 format ('RDEPO: Receptors dry depositions file = ',256A1)
 2210 format ('RDEPO: Receptors wet depositions file = ',256A1)
 2300 format ('RDEPO: Line src. dry depositions file = ',256A1)
 2310 format ('RDEPO: Line src. wet depositions file = ',256A1)

! *** End of subroutine RDEPO

      end subroutine rdepo
