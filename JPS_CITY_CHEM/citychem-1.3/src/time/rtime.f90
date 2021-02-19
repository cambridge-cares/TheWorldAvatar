! <rtime.f90 - A component of the City-scale
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

       subroutine RTIME

! The subroutine reads time data from file.
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
!
! ----------------------------------------------------------------------------------

      use mod_main
      use mod_time

        implicit none
    
! Local variables

      INTEGER I
      CHARACTER(len=256) TXTSTR

      LOGICAL LEOF

! I      - Index
! TXTSTR - Text string
! LEAP   - If leap year   then true else false
! LEOF   - If end of file then true else false

! Define name of weekdays

      TXTDAY(1) = 'Mon'
      TXTDAY(2) = 'Tue'
      TXTDAY(3) = 'Wed'
      TXTDAY(4) = 'Thu'
      TXTDAY(5) = 'Fri'
      TXTDAY(6) = 'Sat'
      TXTDAY(7) = 'Sun'

! Define name of months

      TXTMON( 1) = 'Jan'
      TXTMON( 2) = 'Feb'
      TXTMON( 3) = 'Mar'
      TXTMON( 4) = 'Apr'
      TXTMON( 5) = 'May'
      TXTMON( 6) = 'Jun'
      TXTMON( 7) = 'Jul'
      TXTMON( 8) = 'Aug'
      TXTMON( 9) = 'Sep'
      TXTMON(10) = 'Oct'
      TXTMON(11) = 'Nov'
      TXTMON(12) = 'Dec'

! Define number of days in each month

      NDAY( 1) = 31
      NDAY( 2) = 28
      NDAY( 3) = 31
      NDAY( 4) = 30
      NDAY( 5) = 31
      NDAY( 6) = 30
      NDAY( 7) = 31
      NDAY( 8) = 31
      NDAY( 9) = 30
      NDAY(10) = 31
      NDAY(11) = 30
      NDAY(12) = 31

! Read hours betweem UCP and LOCAL time

      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,*) LTOGMT
          ENDIF
          print *,'rtime: time offset:',LTOGMT
      ENDIF

! Read begin datetime

      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,*) (BDAT(I),I=1,4)
          ENDIF
          print *,'rtime: begin datetime:',(BDAT(I),I=1,4)
      ENDIF

      IF (MESSFE) WRITE (MESSUN,2000) BDAT(3),TXTMON(BDAT(2)),     &
                                 BDAT(1),BDAT(4)

! Set begin datetime minute and second

      BDAT(5) = 0
      BDAT(6) = 0

! Set start time and day of week

      YEAR = BDAT(1)
      MNTH = BDAT(2)
      DAYM = BDAT(3)
      HOUR = BDAT(4)
      MINU = BDAT(5)
      SECO = BDAT(6)

      THOUR = 0.

! Check for leap year

      IF (LEAP(YEAR)) NDAY(2) = 29

! Calculate day of week

      CALL CDAYW

! Calculate day of year

      CALL CDAYY

! Read end datetime

      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,*) (EDAT(I),I=1,4)
          ENDIF
          print *,'rtime: end datetime:',(EDAT(I),I=1,4)
      ENDIF

      IF (MESSFE) WRITE (MESSUN,2010) EDAT(3),TXTMON(EDAT(2)),     &
                                 EDAT(1),EDAT(4)

! Set end datetime minute and second

      EDAT(5) = 0
      EDAT(6) = 0

! Read timestep scale factor

      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,*) DTF
          ENDIF
          print *,'rtime: timescale factor:',DTF
      ENDIF

      IF (MESSFE) WRITE (MESSUN,2020) DTF

! Read simulation time between result output

      IF (MAINFE) THEN
          CALL GETDAT(MAINUN,TXTSTR,LEOF)
          IF (TXTSTR(1:1) .NE. '<') THEN
              READ (TXTSTR,*) TSIMRES
          ENDIF
          print *,'rtime: simulation time:',TSIMRES
      ENDIF

      NSH = NINT(3600./TSIMRES)
      ISH = 0

      IF (MESSFE) WRITE (MESSUN,2030) TSIMRES

      RETURN

 2000 FORMAT ('RTIME: Begin time ',I2,1X,A3,1X,I4,1X,I2,'h')
 2010 FORMAT ('RTIME: End   time ',I2,1X,A3,1X,I4,1X,I2,'h')
 2020 FORMAT ('RTIME: Timestep scale factor  = ',F5.2)
 2030 FORMAT ('RTIME: Simulation time between result output = ',     &
         F7.1,' s')

! End of subroutine RTIME

     end subroutine RTIME
