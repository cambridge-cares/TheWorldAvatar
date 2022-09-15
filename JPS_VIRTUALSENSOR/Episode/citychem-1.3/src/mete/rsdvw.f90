! <rsdvw.f90 - A component of the City-scale
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

      subroutine RSDVW

! The subroutine reads turbulence sigma-vw from file or uses user defined values.
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
!           2016  M. Karl: Set to min values if no sigma-vw file provided
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
      integer IZ
      character(len=10) TEXT1
      character(len=10) TEXT2

! IX    - Main grid index in x-direction
! IY    - Main grid index in y-direction
! IZ    - Main grid index in z-direction
! TEXT1 - Textstring
! TEXT2 - Textstring

      IF (SDVWFE) THEN

! Read turbulence sigma-v and sigma-w from file

          DO 100 IZ = 1,NZ
              CALL R3DFLD(SDVWUN,SDVWFM,IZ,TEXT1,TEXT2, &
                     NX,NY,NZ,MX,MY,MZ,SIGV)
              CALL R3DFLD(SDVWUN,SDVWFM,IZ,TEXT1,TEXT2, &
                     NX,NY,NZ,MX,MY,MZ,SIGW)
  100     CONTINUE

      ELSE


          DO 110 IZ = 1,NZ

!MSK start: no file -> set to minimum

          SDVWFV(2*IZ-1) = SIGVMIN(IZ)
          SDVWFV(2*IZ  ) = SIGWMIN(IZ)

!MSK end

! Main data file with file value exists?

          IF (MAINFE) THEN

! Setting homogeneous turbulence sigma-v and sigma-w

              CALL H3DFLD(SDVWFV(2*IZ-1),IZ,NX,NY,NZ,MX,MY,MZ,SIGV)

          ENDIF

! Main data file with file value exists?

          IF (MAINFE) THEN

! Setting homogeneous turbulence sigma-v and sigma-w

              CALL H3DFLD(SDVWFV(2*IZ  ),IZ,NX,NY,NZ,MX,MY,MZ,SIGW)

          ENDIF

  110     CONTINUE

      ENDIF

! Checking data

      DO 200 IZ = 1,NZ
      DO 210 IY = 1,NY
      DO 220 IX = 1,NX

! Missing data is tolerated here. Will later be substituted with
! values calculated by NILU's meteorological preprocessor.

! Negative data different from missing data is not tolerated.

          IF (SIGV(IX,IY,IZ) .LT. 0. .AND. &
         SIGV(IX,IY,IZ) .NE. MISS) THEN
              IF (MESSFE) WRITE (MESSUN,2020) IX,IY,IZ,SIGV(IX,IY,IZ)
              CALL STOPIT('RSDVW: Negative sigma-v value!')
          ENDIF

          IF (SIGW(IX,IY,IZ) .LT. 0. .AND. &
         SIGW(IX,IY,IZ) .NE. MISS) THEN
              IF (MESSFE) WRITE (MESSUN,2030) IX,IY,IZ,SIGW(IX,IY,IZ)
              CALL STOPIT('RSDVW: Negative sigma-w value!')
          ENDIF

  220 CONTINUE
  210 CONTINUE
  200 CONTINUE

! Write message

      IF (SDVWFE) THEN
          IF (MESSFE) WRITE (MESSUN,2000) MOD(YEAR,100),MNTH,DAYM,HOUR
      ELSE
          IF (MAINFE) THEN
              IF (MESSFE) WRITE (MESSUN,2010) &
                     MOD(YEAR,100),MNTH,DAYM,HOUR, &
                     (SDVWFV(IZ),IZ=1,2*NZ)
          ENDIF
      ENDIF

      RETURN

 2000 format ('RSDVW: Read turbulence sigma-vw ....... for time ', &
         4I2.2)
 2010 format ('RSDVW: Homogeneous turbulence sigma-vw  for time ', &
         4I2.2,' = ',99(F10.3,','))
 2020 format ('RSDVW: Turbulence sigma-v SIGV(',I3,',',I3,',',I3, &
         ') = ',F10.3,' m/s',' is negative!')
 2030 format ('RSDVW: Turbulence sigma-w SIGW(',I3,',',I3,',',I3, &
         ') = ',F10.3,' m/s',' is negative!')

! End of subroutine RSDVW

      end subroutine rsdvw
