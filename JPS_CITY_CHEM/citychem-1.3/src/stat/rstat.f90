! <rstat.f90 - A component of the City-scale
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

      subroutine RSTAT

! The subroutine reads main statistics data from file.
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
!           2017  M. Karl: read filenames for netcdf statistic output
!                          CLAVEA and CLAVED cannot be allocated in rstat because 
!                          NQL is not known yet
!
! ----------------------------------------------------------------------------------

      use mod_util
      use mod_main
      use mod_site
      use mod_conc
      use mod_depo
      use mod_lsrc
      use mod_stat

      implicit none

! Local variables

      CHARACTER(len=256) TXTSTR
      LOGICAL LEOF
!MSK start
      INTEGER IC,IP
!MSK end

! TXTSTR - Text string
! LEOF   - If end of file then true else false

!MSK      IF (.NOT. ALLOCATED(MSTAFN)) ALLOCATE(MSTAFN(NC))
      IF (.NOT. ALLOCATED(SSTAFN)) ALLOCATE(SSTAFN(NC))
!MSK      IF (.NOT. ALLOCATED(MSTAUN)) ALLOCATE(MSTAUN(NC))
      IF (.NOT. ALLOCATED(SSTAUN)) ALLOCATE(SSTAUN(NC))
!MSK      IF (.NOT. ALLOCATED(MSTAFE)) ALLOCATE(MSTAFE(NC))
      IF (.NOT. ALLOCATED(SSTAFE)) ALLOCATE(SSTAFE(NC))
!MSK      IF (.NOT. ALLOCATED(MSTAFV)) ALLOCATE(MSTAFV(NC))
      IF (.NOT. ALLOCATED(SSTAFV)) ALLOCATE(SSTAFV(NC))
!MSK      IF (.NOT. ALLOCATED(MSTAFM)) ALLOCATE(MSTAFM(NC))
      IF (.NOT. ALLOCATED(SSTAFM)) ALLOCATE(SSTAFM(NC))

      IF (.NOT. ALLOCATED(CMAVEA)) ALLOCATE(CMAVEA(NX,NY,NC))
      IF (.NOT. ALLOCATED(CMAVED)) ALLOCATE(CMAVED(NX,NY,NC))
      IF (NR .GT. 0) THEN
          IF (.NOT. ALLOCATED(CRAVEA)) ALLOCATE(CRAVEA(NC,NR))
          IF (.NOT. ALLOCATED(CRAVED)) ALLOCATE(CRAVED(NC,NR))
      ENDIF
!MSK CLAVEA and CLAVED cannot be allocated in rstat because NQL is not known yet
!MSK      IF (NQL .GT. 0) THEN
!MSK          IF (.NOT. ALLOCATED(CLAVEA)) ALLOCATE(CLAVEA(NQL))
!MSK          IF (.NOT. ALLOCATED(CLAVED)) ALLOCATE(CLAVED(NQL))
!MSK      ENDIF

      IF (.NOT. ALLOCATED(IMHIGHDMIN)) ALLOCATE(IMHIGHDMIN(NX,NY))
      IF (.NOT. ALLOCATED(IMHIGHHMIN)) ALLOCATE(IMHIGHHMIN(NX,NY))
      IF (NR .GT. 0) THEN
          IF (.NOT. ALLOCATED(IRHIGHDMIN)) ALLOCATE(IRHIGHDMIN(NR))
          IF (.NOT. ALLOCATED(IRHIGHHMIN)) ALLOCATE(IRHIGHHMIN(NR))
      ENDIF

!MSK ILHIGHDMIN cannot be allocated in rstat because NQL is not known yet
!MSK      IF (NQL .GT. 0) THEN
!MSK          IF (.NOT. ALLOCATED(ILHIGHDMIN)) ALLOCATE(ILHIGHDMIN(NQL))
!MSK          IF (.NOT. ALLOCATED(ILHIGHHMIN)) ALLOCATE(ILHIGHHMIN(NQL))
!MSK      ENDIF

      IF (.NOT. ALLOCATED(WMTOTA)) ALLOCATE(WMTOTA(NX,NY,NC))
      IF (NR .GT. 0) THEN
          IF (.NOT. ALLOCATED(WRTOTA)) ALLOCATE(WRTOTA(NC,NR))
      ENDIF

      

! Read number of highest hourly concentration values

      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF) 
          IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,*) NHIGHH
          ENDIF
          print *,'rstat: highest hourly conc: ',NHIGHH
      ENDIF

      IF (MESSFE) WRITE (MESSUN,2000) NHIGHH

! Read number of highest daily concentration values

      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF) 
          IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,*) NHIGHD
          ENDIF
          print *,'rstat: highest daily conc: ',NHIGHD
      ENDIF

      IF (MESSFE) WRITE (MESSUN,2020) NHIGHD

      IF (NHIGHD .GT. 0 .AND. NX .GT. 0 .AND. NY .GT. 0) THEN
          IF (.NOT. ALLOCATED(CMHIGHD)) ALLOCATE(CMHIGHD(NHIGHD,NX,NY))
      ENDIF
      IF (NHIGHD .GT. 0 .AND. NR .GT. 0) THEN
          IF (.NOT. ALLOCATED(CRHIGHD)) ALLOCATE(CRHIGHD(NHIGHD,NR))
      ENDIF
!MSK CLHIGHD cannot be allocated in rstat because NQL is not known yet
!MSK      IF (NHIGHD .GT. 0 .AND. NQL .GT. 0) THEN
!MSK          IF (.NOT. ALLOCATED(CLHIGHD)) ALLOCATE(CLHIGHD(NHIGHD,NQL))
!MSK      ENDIF

      IF (NHIGHH .GT. 0 .AND. NX .GT. 0 .AND. NY .GT. 0) THEN
          IF (.NOT. ALLOCATED(CMHIGHH)) ALLOCATE(CMHIGHH(NHIGHH,NX,NY))
      ENDIF
      IF (NHIGHH .GT. 0 .AND. NR .GT. 0) THEN
          IF (.NOT. ALLOCATED(CRHIGHH)) ALLOCATE(CRHIGHH(NHIGHH,NR))
      ENDIF

!MSK CLHIGHH cannot be allocated in rstat because NQL is not known yet
!MSK      IF (NHIGHH .GT. 0 .AND. NQL .GT. 0) THEN
!MSK          IF (.NOT. ALLOCATED(CLHIGHH)) ALLOCATE(CLHIGHH(NHIGHH,NQL))
!MSK      ENDIF

! Read calculate daily mean indicator

      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF) 
          IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,*) IAVED
          ENDIF
          print *,'rstat: daily mean indicator: ',IAVED
      ENDIF

      IF (MESSFE) WRITE (MESSUN,2040) IAVED

! Read calculate overall mean indicator

      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF) 
          IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,*) IAVEA
          print *,'rstat: overall mean indicator: ',IAVEA
          ENDIF
      ENDIF

      IF (MESSFE) WRITE (MESSUN,2050) IAVEA


!MSK start
!MSK netCDF output for all ic compounds
!MSK required: netcdf output files for main grid hourly average
      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          print *,'netcdf output main grid hourly average filename ',TXTSTR
          if(TXTSTR(1:1)==' ') then
             ND1STAFE=.false.
             print *,'no netcdf main grid hourly average output'
          else
             CALL GETFNV(TXTSTR,ND1STAFN,ND1STAFE,1,1,ND1STAFV)
             ND1STAFE=.true.
          endif
      ELSE
          TXTSTR(1:1) = '<'          
          CALL GETFNV(TXTSTR,ND1STAFN,ND1STAFE,1,1,ND1STAFV)
      ENDIF

!MSK required: netcdf output files for receptor grid
!MSK           output is written every hour
      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          print *,'netcdf hourly receptor grid output filename ',TXTSTR
          if(TXTSTR(1:1)==' ') then
             ND2STAFE=.false.
             print *,'no netcdf receptor grid hourly output'
          else
             CALL GETFNV(TXTSTR,ND2STAFN,ND2STAFE,1,1,ND2STAFV)
             ND2STAFE=.true.
          endif
      ELSE
          TXTSTR(1:1) = '<'          
          CALL GETFNV(TXTSTR,ND2STAFN,ND2STAFE,1,1,ND2STAFV)
      ENDIF

!MSK required: netcdf output files for (receptor) stations
!MSK           output is written every hour
      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          print *,'netcdf hourly station output filename ',TXTSTR
          if(TXTSTR(1:1)==' ') then
             ND3STAFE=.false.
             print *,'no netcdf stations hourly output'
          else
             CALL GETFNV(TXTSTR,ND3STAFN,ND3STAFE,1,1,ND3STAFV)
             ND3STAFE=.true.
          endif
      ELSE
          TXTSTR(1:1) = '<'          
          CALL GETFNV(TXTSTR,ND3STAFN,ND3STAFE,1,1,ND3STAFV)
      ENDIF
!MSK end
  

      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          print *,'line source receptor daily average filename ',TXTSTR
          IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,1000) LSTAFN
          ENDIF
      ELSE
          TXTSTR(1:1) = '<'          
          CALL GETFNV(TXTSTR,LSTAFN,LSTAFE,1,1,LSTAFV)
      ENDIF


! * Calculate daily statistics indicator (1=Yes,0=No)
      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF) 
          IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,*) IDSTA
          print *,'rstat: daily statistics indicator: ',IDSTA
          ENDIF
      ENDIF


!MSK start
!MSK required: netcdf output files for overall main grid
      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          print *,'netcdf output main grid filename ',TXTSTR
          if(TXTSTR(1:1)==' ') then
             NC1STAFE=.false.
             print *,'no netcdf main grid output'
          else
             CALL GETFNV(TXTSTR,NC1STAFN,NC1STAFE,1,1,NC1STAFV)
             NC1STAFE=.true.
          endif
      ELSE
          TXTSTR(1:1) = '<'          
          CALL GETFNV(TXTSTR,NC1STAFN,NC1STAFE,1,1,NC1STAFV)
      ENDIF

!MSK required: netcdf output files for overall receptor points
      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          print *,'netcdf output receptor points filename ',TXTSTR
          if(TXTSTR(1:1)==' ') then
             NC2STAFE=.false.
             print *,'no netcdf receptor points output'
          else
             CALL GETFNV(TXTSTR,NC2STAFN,NC2STAFE,1,1,NC2STAFV)
             NC2STAFE=.true.
          endif
      ELSE
          TXTSTR(1:1) = '<'          
          CALL GETFNV(TXTSTR,NC2STAFN,NC2STAFE,1,1,NC2STAFV)
      ENDIF
!MSK end

!MSK required: netcdf output files for overall stations
      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          print *,'netcdf output stations filename ',TXTSTR
          if(TXTSTR(1:1)==' ') then
             NC3STAFE=.false.
             print *,'no netcdf stations output'
          else
             CALL GETFNV(TXTSTR,NC3STAFN,NC3STAFE,1,1,NC3STAFV)
             NC3STAFE=.true.
          endif
      ELSE
          TXTSTR(1:1) = '<'          
          CALL GETFNV(TXTSTR,NC3STAFN,NC3STAFE,1,1,NC3STAFV)
      ENDIF
!MSK end


      RETURN

 1000 format (A256)

 2000 format ('RSTAT: Number of highest hourly values = ',I2)
 2010 format ('RSTAT: Too many highest hourly values, maximum = ',I2)
 2020 format ('RSTAT: Number of highest  daily values = ',I2)
 2030 format ('RSTAT: Too many highest  daily values, maximum = ',I2)
 2040 format ('RSTAT: Calculate daily   mean indicator = ',I2)
 2050 format ('RSTAT: Calculate overall mean indicator = ',I2)

! End of subroutine RSTAT

      end subroutine rstat
