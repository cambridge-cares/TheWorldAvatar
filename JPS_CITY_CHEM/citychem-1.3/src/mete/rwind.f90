! <rtemp.f90 - A component of the City-scale
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

      subroutine RWIND

! The subroutine reads wind from file.
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
!           2017  M. Karl: Allocate U,V,W. Call R3DFLD for wind component W
!
! ----------------------------------------------------------------------------------

      use mod_util
      use mod_main
      use mod_site
      use mod_time
      use mod_mete

          implicit none

! Local variables
      real DDV
      real FFV
      real UU
      real UVAL
      real VV
      real VVAL
      integer IX
      integer IY
      integer IZ
      character(len=10) TEXT1
      character(len=10) TEXT2

! DDV   - Wind direction
! FFV   - Wind speed
! UU    - Wind u-component
! UVAL  - Wind u-component
! VV    - Wind v-component
! VVAL  - Wind v-component
! IX    - Main grid index in x-direction
! IY    - Main grid index in x-direction
! IZ    - Main grid index in x-direction
! TEXT1 - Textstring
! TEXT2 - Textstring

! External function
!MSK function CWDIR in mod_mete
!MSK      REAL CWDIR

!MSK start
      IF (.NOT. ALLOCATED(U)) ALLOCATE (U(NX,NY,NZ))
      IF (.NOT. ALLOCATED(V)) ALLOCATE (V(NX,NY,NZ))
      IF (.NOT. ALLOCATED(W)) ALLOCATE (W(NX,NY,NZ))
!MSK end

      IF (WINDFE) THEN

! Read wind u- and v-components from file
! MSK: MCWIND and TAPM4CCHEM files also contain w-component

          DO 100 IZ = 1,NZ
              CALL R3DFLD(WINDUN,WINDFM,IZ,TEXT1,TEXT2, &
                     NX,NY,NZ,MX,MY,MZ,U)
              CALL R3DFLD(WINDUN,WINDFM,IZ,TEXT1,TEXT2, &
                     NX,NY,NZ,MX,MY,MZ,V)
!MSK start
              CALL R3DFLD(WINDUN,WINDFM,IZ,TEXT1,TEXT2, &
                      NX,NY,NZ,MX,MY,MZ,W)
!MSK end
  100     CONTINUE


      ELSE

          DO 110 IZ = 1,NZ

! Main data file with file value exists?

              IF (MAINFE) THEN

! Set homogeneous wind u-components

                  CALL H3DFLD(WINDFV(2*IZ-1),IZ,NX,NY,NZ,MX,MY,MZ,U)

              ENDIF

! Main data file with file value exists?

              IF (MAINFE) THEN

! Set homogeneous wind v-components

                  CALL H3DFLD(WINDFV(2*IZ  ),IZ,NX,NY,NZ,MX,MY,MZ,V)

              ENDIF

  110     CONTINUE
 
      ENDIF

! Checking data

      DO 200 IZ = 1,1
      DO 210 IY = 1,NY
      DO 220 IX = 1,NX

! If missing data then no model run

          IF (U(IX,IY,IZ) .EQ. MISS .OR. V(IX,IY,IZ) .EQ. MISS) THEN
              IF (MESSFE) WRITE (MESSUN,2020)
              RUNOK = .FALSE.
              RETURN
          ENDIF

  220 CONTINUE
  210 CONTINUE
  200 CONTINUE

! Write out windspeed and direction for selected gridcell

! Defaults

      IX = 1
      IY = 1

! Shanxi testing

      IF (NX .GE. 12 .AND. NY .GE. 13) THEN
          IX = 12
          IY = 13
      ENDIF

! Oslo - Valle Hovin

      IF (NX .EQ. 22 .AND. NY .EQ. 18) THEN
          IX = 14
          IY = 12
      ENDIF

! Bergen - Florida

      IF (NX .EQ. 11 .AND. NY .EQ. 23) THEN
          IX =  8
          IY = 12
      ENDIF

! Drammen - Marienlyst

      IF (NX .EQ. 18 .AND. NY .EQ. 10) THEN
          IX = 13
          IY =  6
      ENDIF

! Trondheim - Midtbyen

      IF (NX .EQ. 14 .AND. NY .EQ. 16) THEN
          IX =  6
          IY = 13
      ENDIF
	
! Stavanger - ?

      IF (NX .EQ.  1 .AND. NY .EQ.  1) THEN
          IX =  1
          IY =  1
      ENDIF

! Lillehammer - Stampesletta

      IF (NX .EQ.  9 .AND. NY .EQ. 10) THEN
          IX =  6
          IY =  7
      ENDIF

! Grenland - Ås

      IF (NX .EQ. 16 .AND. NY .EQ. 23) THEN
          IX = 10
          IY =  8
      ENDIF

! Calculate windspeed and direction

      UVAL = U(IX,IY,1)
      VVAL = V(IX,IY,1)
       FFV = SQRT(UVAL*UVAL + VVAL*VVAL)
       DDV = CWDIR(UVAL,VVAL)

! Write windspeed and direction to log-file

      IF (MESSFE) WRITE (MESSUN,2030) IX,IY,FFV,DDV

! Write message

      IF (WINDFE) THEN
          IF (MESSFE) WRITE (MESSUN,2000) &
                 MOD(YEAR,100),MNTH,DAYM,HOUR
      ELSE
          IF (MAINFE) THEN
              IF (MESSFE) WRITE (MESSUN,2010) &
                     MOD(YEAR,100),MNTH,DAYM,HOUR, &
                     (U(1,1,IZ),V(1,1,IZ),IZ=1,NZ)
          ENDIF
      ENDIF

! Rotate windfield according to the rotation of the grid with respect
! to a strict north-south coordinate system

      DO 300 IZ = 1,NZ
      DO 310 IY = 1,NY
      DO 320 IX = 1,NX

          UU = U(IX,IY,IZ)
          VV = V(IX,IY,IZ)
          U(IX,IY,IZ) = UU*COS(ANGLE*RAD) + VV*SIN(ANGLE*RAD)
          V(IX,IY,IZ) = VV*COS(ANGLE*RAD) - UU*SIN(ANGLE*RAD)

  320 CONTINUE
  310 CONTINUE
  300 CONTINUE

      RETURN

 2000 format ('RWIND: Read wind u- and v-components .. for time ', &
         4I2.2)
 2010 format ('RWIND: Homogeneous wind uv-comp. ...... for time ', &
         4I2.2,' = ',99(F10.3,','))
 2020 format ('RWIND: Wind field contains missing data!')
 2030 format ('RWIND: Sample wind IX,IY,FF,DD = ',I3,I3,F7.1,F7.1)

! End of subroutine RWIND

      end subroutine rwind
