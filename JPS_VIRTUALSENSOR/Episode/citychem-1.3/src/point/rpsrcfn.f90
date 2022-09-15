! <rpsrcfn.f90 - A component of the City-scale
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

      subroutine RPSRCFN

! *** The subroutine reads main point sources data.
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
!           2017  M. Karl: handling of missing point source files
!    15 Feb 2018  M. Karl: L124 Set PSRCFE true if point source file given
!
! ----------------------------------------------------------------------------------

      use mod_util
      use mod_main
      use mod_site
      use mod_time
      use mod_mete
      use mod_conc
      use mod_psrc

      implicit none

! *** Local variables

      INTEGER I
      INTEGER IZ
      CHARACTER(len=256) TXTSTR
      LOGICAL LEOF

! *** I      - Textstring index
! *** IZ     - Main grid index in z-direction
! *** TXTSTR - Textstring
! *** LEOF   - If end of file then true else false

      IF (.NOT. ALLOCATED(IQC)) ALLOCATE(IQC(NC))
      IF (.NOT. ALLOCATED(ZLF)) ALLOCATE(ZLF(NZ))

! *** Read point sources subgrid model type:
      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,*) PSRCMTYPE
          ENDIF
          print *,'rpsrcfn:  psrc subgrid model: ',PSRCMTYPE
      ENDIF

      IF (PSRCMTYPE .LT. 0) PSRCMTYPE = 0
      IF (PSRCMTYPE .GT. 5) PSRCMTYPE = 5

      IF (MESSFE) WRITE (MESSUN,2000) PSRCMTYPE

! *** Read point sources subgrid model add to result indicator:
      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,*) PSRCAI        ! Default = 1
          ENDIF
          print *,'rpsrcfn:  add psrc subgrid: ',PSRCAI
      ENDIF

      IF (PSRCAI .LT. 0) PSRCAI = 0
      IF (PSRCAI .GT. 1) PSRCAI = 1

      IF (MESSFE) WRITE (MESSUN,2005) PSRCAI

! *** Read point sources filename:
      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
!MSK start
          !IF (TXTSTR(1:1) .NE. '<') THEN
          !    READ (TXTSTR,1000) PSRCFN
          !ENDIF
          if(TXTSTR(1:1)==' ') then
             PSRCFE = .false.
             PSRCFV = MISS
             print *,'no point src file'
          else
             READ (TXTSTR,1000) PSRCFN
             print *,'rpsrcfn: point src filename: ',PSRCFN
             !PSRCFE = .false.
             PSRCFE = .true.
          endif
!MSK end
      ELSE
          PSRCFN = ' '
          PSRCFE = .FALSE.
      ENDIF

      IF (MESSFE) WRITE (MESSUN,2010)   &
                (PSRCFN(I:I),I=1,INDEX(PSRCFN,' ') - 1)

! *** Read old plume segment data filename:
      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,1000) OLDPFN
          ENDIF
          print *,'rpsrcfn: old plume segment filename: ',OLDPFN
      ELSE
          OLDPFN = ' '
          OLDPFE = .FALSE.
      ENDIF

!_LHS      IF (MESSFE) WRITE (MESSUN,2020)
!_LHS     &           (OLDPFN(I:I),I=1,INDEX(OLDPFN,' ') - 1)

! *** Read new plume segment data filename:
      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,1000) NEWPFN
          ENDIF
          print *,'rpsrcfn: new plume segment filename: ',NEWPFN
      ELSE
          NEWPFN = 'plume.dat'
!_SEW_UncertWeb_110824_Start
          NEWPFN = ' '
!_SEW_UncertWeb_110824_End
          NEWPFE = .FALSE.
      ENDIF

      IF (MESSFE) WRITE (MESSUN,2030)      &
                (NEWPFN(I:I),I=1,INDEX(NEWPFN,' ') - 1)

! *** Read plume segment redirection limit:
      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,*) RDL
          ENDIF
          print *,'rpsrcfn: plume segment redirect: ',RDL
      ENDIF

      IF (MESSFE) WRITE (MESSUN,2040) RDL

! *** Read limits for plume segment size:
      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,*) YLF,(ZLF(IZ),IZ=1,NZ)
          ENDIF
          print *,'rpsrcfn: plume segment limits: ',YLF,(ZLF(IZ),IZ=1,NZ)
      ENDIF

      IF (MESSFE) THEN
          WRITE (MESSUN,2050) YLF
          WRITE (MESSUN,2055) NZ,(ZLF(IZ),IZ=1,NZ)
      ENDIF

! *** Read plume model minimum windspeed:
      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,*) PSRCFFMIN
          ENDIF
          print *,'rpsrcfn: min wind speed: ',PSRCFFMIN
      ENDIF

      IF (MESSFE) WRITE (MESSUN,2070) PSRCFFMIN

      RETURN

 1000 format (A256)

 2000 format ('RPSRCFN: Point sources sub grid model type',      &
             ' = ',I3)
 2005 format ('RPSRCFN: Point sources sub grid model add to indicator',      &
             ' = ',I3)
 2010 format ('RPSRCFN: Point sources .... file = ',256A1)
 2020 format ('RPSRCFN: Old plume segments file = ',256A1)
 2030 format ('RPSRCFN: New plume segments file = ',256A1)
 2040 format ('RPSRCFN: Plume segment redirection limit = ',      &
             F10.3,' deg')
 2050 format ('RPSRCFN: Plume segment Y limit fraction  = ',      &
             F10.3)
 2055 format ('RPSRCFN: Plume segment Z limit fractions (1:',I2,      &
             ') = ',99(7F10.3/))
 2060 format ('RPSRCFN: NO2 plume mod. dist. fact. = ',      &
             F10.3,' /m')
 2070 format ('RPSRCFN: Minimum windspeed for point source subgrid',      &
             ' model = ',F10.3,' m/s')

! *** End of subroutine RPSRCFN

      end subroutine rpsrcfn
