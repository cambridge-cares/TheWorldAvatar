! <rprec.f90 - A component of the City-scale
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

      subroutine rprec

! The subroutine reads precipitation from file. 
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

      use mod_util
      use mod_main
      use mod_site
      use mod_time
      use mod_mete

                implicit none

! Local variables
      integer IX
      integer IY
      character(len=10) TEXT1
      character(len=10) TEXT2

! IX    - Main grid index in x-direction
! IY    - Main grid index in y-direction
! TEXT1 - Textstring
! TEXT2 - Textstring

      IF (PRECFE) THEN

! Read precipitation from file

          CALL R2DFLD(PRECUN,PRECFM,TEXT1,TEXT2,NX,NY,MX,MY,PREC)

      ELSE

! Main data file with file value exists?

          IF (MAINFE) THEN

! Set homogeneous precipitation

              CALL H2DFLD(PRECFV,NX,NY,MX,MY,PREC)

          ENDIF

      ENDIF

! Checking data

      DO 100 IY = 1,NY
      DO 110 IX = 1,NX

!_LHS_08_Oct_2004_Start:
!
! Wet Deposition test:
!
! Drizzle:     0.5 mm/h
! Heavy Rain: 25.0 mm/h.
!
!        PREC(IX,IY) = 1.0
!
!_LHS_08_Oct_2004_End.


! Missing data is set equal to zero

          IF (PREC(IX,IY) .EQ. MISS) PREC(IX,IY) = 0.

! Negative data is not tolerated

          IF (PREC(IX,IY) .LT. 0.) THEN
              IF (MESSFE) WRITE (MESSUN,2030) IX,IY,PREC(IX,IY)
              CALL STOPIT('RPREC: Negative precipitation!')
          ENDIF

!MSK start
! Correct the cloud fraction if prec>0

          IF (PREC(IX,IY) .GT. 0.) THEN
              CLOU(IX,IY) = 0.9 
          ENDIF
          !print *,'rprec X Y PREC CLOU',IX,IY,PREC(IX,IY),CLOU(IX,IY)

  110 CONTINUE
  100 CONTINUE

! Write out precipitation for selected gridcell

! Defaults

          IX = 1
          IY = 1

! Write precipitation to log-file

      IF (MESSFE) WRITE (MESSUN,2020) IX,IY,PREC(IX,IY)

! Write message

      IF (PRECFE) THEN
          IF (MESSFE) WRITE (MESSUN,2000) &
                 MOD(YEAR,100),MNTH,DAYM,HOUR
      ELSE
          IF (MAINFE) THEN
              IF (MESSFE) WRITE (MESSUN,2010) &
                     MOD(YEAR,100),MNTH,DAYM,HOUR,PREC(1,1)
          ENDIF
      ENDIF

      RETURN

 2000 format ('RPREC: Read precipitation from file ... for time ', &
         4I2.2)
 2010 format ('RPREC: Homogeneous precipitation ...... for time ', &
         4I2.2,' = ',F10.3,' mm/h')
 2020 format ('RPREC: Sample precipitation PREC(',I3,',',I3,') = ', &
         F10.3,' mm/h')
 2030 format ('RPREC: Precipitation PREC(',I3,',',I3,') = ', &
         F10.3,' mm/h',' is negative!')

! End of subroutine RPREC

      end subroutine rprec
