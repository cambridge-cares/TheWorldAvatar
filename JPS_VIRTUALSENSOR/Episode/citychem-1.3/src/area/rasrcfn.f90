! <rasrcfn.f90 - A component of the City-scale
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

      subroutine RASRCFN

! The subroutine reads main area sources filenames.
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
!    xx Dat 201x  Name: Line  Description of Change
!           2017  M. Karl: netCDF output for main grid 3D area source emissions
!    20 Nov 2019  M. karl: L162  if area source file not missing, set fill value 0
!
! ----------------------------------------------------------------------------------

      use mod_util
      use mod_main
      use mod_site
      use mod_conc
      use mod_asrc

      implicit none

! Local variables

      INTEGER I,IA,IC
      CHARACTER(len=256) TXTSTR
      LOGICAL LEOF

! I      - Index of textstring
! IA     - Index of area source
! IC     - Index of compound
! TXTSTR - Text string
! LEOF   - If end of file then true else false

! Read area sources subgrid model add to result indicator

      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,*) ASRCAI
          ENDIF
          print *,'rasrcfn:  add subrid area src',ASRCAI
      ENDIF

      IF (ASRCAI .LT. 0) ASRCAI = 0
      IF (ASRCAI .GT. 1) ASRCAI = 1

      IF (MESSFE) WRITE (MESSUN,2000) ASRCAI

! Read number of area sources

      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,*) NA
          ENDIF
          print *,'rasrcfn: number area src',NA
      ENDIF

      IF (MESSFE) WRITE (MESSUN,2010) NA

      IF (.NOT. ALLOCATED(ASRCFN)) ALLOCATE(ASRCFN(NC,NA))
      IF (.NOT. ALLOCATED(ASRCUN)) ALLOCATE(ASRCUN(NC,NA))
      IF (.NOT. ALLOCATED(ASRCFE)) ALLOCATE(ASRCFE(NC,NA))
      IF (.NOT. ALLOCATED(ASRCFV)) ALLOCATE(ASRCFV(NC,NA))
      IF (.NOT. ALLOCATED(ASRCFM)) ALLOCATE(ASRCFM(NC,NA))
      IF (.NOT. ALLOCATED(QAORIG)) ALLOCATE(QAORIG(NC,NX,NY,NA))
      IF (.NOT. ALLOCATED(QA))     ALLOCATE(QA(NC,NX,NY,NZ))
      IF (.NOT. ALLOCATED(AFLD))   ALLOCATE(AFLD(NX,NY))


      MA = NA

! Go through all area sources and compounds

      DO 100 IA = 1,NA
      DO 110 IC = 1,NC

!MSK: it should be possible to have a missing area source file for a species

! *** Read area source filenames or values:
      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          print *,'rasrcfn: area sources: ',TXTSTR
!MSK start
          if(TXTSTR(1:1)==' ') then
             ASRCFE(IC,IA) = .false.
             ASRCFV(IC,IA) = MISS
             print *,'no area src file for ',ic
          else
            CALL GETFNV(TXTSTR,ASRCFN(IC,IA),ASRCFE(IC,IA),1,1,  &
                            ASRCFV(IC,IA))
!MSK 20.11.2019: file not missing, set fill value to 0
            ASRCFV(IC,IA) = 0.0
          endif
!MSK end
      ELSE
!MSK start
          IF ( ASRCFN(IC,IA)(1:1) .EQ. ' ') THEN
            TXTSTR(1:1) = '<'
          ELSE
            TXTSTR = ASRCFN(IC,IA)
          ENDIF
!MSK end
          CALL GETFNV(TXTSTR,ASRCFN(IC,IA),ASRCFE(IC,IA),1,1,   &
                     ASRCFV(IC,IA))
      ENDIF

      print *,'rasrcfn: ',ASRCFN(IC,IA)
      print *,'rasrcfn: ',ASRCFV(IC,IA)

  110 CONTINUE
  100 CONTINUE

! Read minimum windspeed for subgrid scale area source model

      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,*) ASRCFFMIN
          ENDIF
          print *,'rasrcfn: min wind speed: ',ASRCFFMIN
      ENDIF

      IF (MESSFE) WRITE (MESSUN,2030) ASRCFFMIN

!MSK start
!MSK New netCDF output 3D for area source emissions
      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          print *,'netcdf area emissions hourly 3D filename ',TXTSTR
          if(TXTSTR(1:1)==' ') then
             NH1ASRCFE=.false.
             print *,'no netcdf area emission hourly 3D output'
          else
             CALL GETFNV(TXTSTR,NH1ASRCFN,NH1ASRCFE,1,1,NH1ASRCFV)
             NH1ASRCFE=.true.
          endif
      ELSE
          TXTSTR(1:1) = '<'          
          CALL GETFNV(TXTSTR,NH1ASRCFN,NH1ASRCFE,1,1,NH1ASRCFV)
      ENDIF
!MSK end

      RETURN

 2000 format ('RASRCFN: Area source subgrid model add to indicator: ',   &
             I3)
 2010 format ('RASRCFN: Number of area sources = ',I3)
 2015 format ('RASRCFN: Too many area sources, maximum = ',I3)
 2020 format ('RASRCFN: Area  source nr. ',I3,' and compound nr. ',   &
             I3,' filename  = ',256A1)
 2025 format ('RASRCFN: Area  source nr. ',I3,' and compound nr. ',   &
             I3,' filevalue = ',F12.1)
 2030 format ('RASRCFN: Minimum windspeed for area source subgrid',   &
             ' model = ',F7.2,' m/s')

! End of subroutine RASRCFN

      end subroutine rasrcfn
