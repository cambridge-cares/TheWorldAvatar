! <rsurf.f90 - A component of the City-scale
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

      subroutine RSURF

! The subroutine reads surface roughness from file.
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

        implicit none

! Local variables

      REAL Z0MAXV,Z0MINV
      INTEGER IX,IY
      CHARACTER(len=10) TEXT1,TEXT2

! Z0MAXV  - Surface roughness maximum value
! Z0MINV  - Surface roughness minimum value
! IX,IY   - Main grid indices
! TEXT1,2 - Textstrings

! Open file

      call OPIFIL(SURFFN,SURFUN,SURFFE,SURFFM)

! File exists?

      IF (SURFFE) THEN

! Yes!

! Read surface roughness from file

          call R2DFLD(SURFUN,SURFFM,TEXT1,TEXT2,NX,NY,MX,MY,Z0)

      ELSE

! No!

! Main data file with file value exists?

          IF (MAINFE .AND. SURFFV(1) .NE. MISS) THEN

! Setting homogeneous surface roughness field

              call H2DFLD(SURFFV(1),NX,NY,MX,MY,Z0)

          ENDIF

      ENDIF

! Checking data

      DO 100 IY = 1,NY
      DO 110 IX = 1,NX

! Missing data is not tolerated

          IF (Z0(IX,IY) .EQ. MISS) THEN
              IF (MESSFE) WRITE (MESSUN,2030) IX,IY,Z0(IX,IY)
              call STOPIT('RSURF: Missing surface roughness!')
          ENDIF

! Negative data is not tolerated

!MSK start if z0=0 set to very small
          IF (Z0(IX,IY) .EQ. 0.) THEN
             Z0(IX,IY) = 0.0001
          ENDIF
!MSK          IF (Z0(IX,IY) .LE. 0.) THEN
!MSK end
          IF (Z0(IX,IY) .LT. 0.) THEN
              IF (MESSFE) WRITE (MESSUN,2040) IX,IY,Z0(IX,IY)
              call STOPIT('RSURF: Zero or negative surface roughness!')
          ENDIF

  110 CONTINUE
  100 CONTINUE

! Calculate minimum and maximum of surface roughness

      Z0MINV = +1.E6
      Z0MAXV = -1.E6

      DO IY = 1,NY
        DO IX = 1,NX
          Z0MINV = MIN(Z0(IX,IY),Z0MINV)
          Z0MAXV = MAX(Z0(IX,IY),Z0MAXV)
        ENDDO
      ENDDO

! Write surface roughness minimum and maximum values

      IF (MESSFE) THEN
          WRITE (MESSUN,2000) Z0MINV
          WRITE (MESSUN,2005) Z0MAXV
      ENDIF

! Write message

      IF (SURFFE) THEN
          IF (MESSFE) WRITE (MESSUN,2010)
      ELSE
          IF (MAINFE .AND. SURFFV(1) .NE. MISS) THEN
              IF (MESSFE) WRITE (MESSUN,2020) Z0(1,1)
          ENDIF
      ENDIF

! Close file

      call CLIFIL(SURFFN,SURFUN,SURFFE,SURFFM)

      RETURN

 2000 format ('RSURF: Surface roughness minimum = ',E12.3)
 2005 format ('RSURF: Surface roughness maximum = ',E12.3)
 2010 format ('RSURF: Read surface roughness field from file')
 2020 format ('RSURF: Homogeneous surface roughness field = ',  &
              F9.3,' m')
 2030 format ('RSURF: Surface roughness Z0(',I3,',',I3,') = ',  &
             E10.1,' m',' is missing data!')
 2040 format ('RSURF: Surface roughness Z0(',I3,',',I3,') = ',  &
             E10.1,' m',' is negative!')

! End of subroutine RSURF
      end subroutine rsurf
