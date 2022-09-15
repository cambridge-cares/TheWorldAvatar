! <psgrid.f90 - A component of the City-scale
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

      subroutine PSGRID

! The subroutine calculates grid model emission from plume segments.
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
!           2017  M. Karl: Conversion from num/s to g/s   (num = number of particles)
!
! ----------------------------------------------------------------------------------

      use mod_util
      use mod_main
      use mod_site
      use mod_time
      use mod_mete
      use mod_conc
      use mod_grid
      use mod_psrc

      implicit none

! Local variables

      real :: DD
      real :: MASSV
      real :: SIGYV
      real :: SIGZV
      real :: XPV
      real :: YL
      real :: YPV
      real :: ZL
      real :: ZPV

      integer :: IC
      integer :: IP
      integer :: IXP
      integer :: IYP
      integer :: IZM
      integer :: IZP

! DD    - Plume segment direction
! MASSV - Plume segment mass
! SIGYV - Plume segment sigma-y
! SIGZV - Plume segment sigma-z
! XPV   - Plume segment x-coordinates
! YPV   - Plume segment y-coordinates
! ZPV   - Plume segment z-coordinates
! IC    - Compound index
! IP    - Plume segment index
! IXP   - Plume segment grid model grid cell index in x-direction
! IYP   - Plume segment grid model grid cell index in y-direction
! IZM   - Plume segment grid model grid cell index in z-direction
! IZP   - Plume segment grid model grid cell index in z-direction

! External functions
!MSK function ilay now in mod_site
!MSK      INTEGER ILAY

! Go through all plume segments

      DO 100 IP = 1,NP

! Get plume segment coordinates

          XPV = PU(1,IP)
          YPV = PU(2,IP)
          ZPV = PU(7,IP)

! Get plume segment extended grid model grid indices

          CALL GETMGI(0,XPV,YPV,ZPV,IXP,IYP,IZP)

! Get plume segment direction

          DD = PU(8,IP)

! If the plume segment has negative direction then add plume mass as
! emission for grid model

          IF (DD .LT. 0.) GOTO 110

! If the plume segment is over a subgrid cell then goto next plume
! segment

          IF (IXP .GE. 1 .AND. IXP .LE. NX .AND.      &
             IYP .GE. 1 .AND. IYP .LE. NY) THEN

              IF (SUBF(IXP,IYP) .NE. 0.) GOTO 100

          ENDIF

! Get plume segment sigma-y and sigma-z

          SIGYV = PU(3,IP)
          SIGZV = PU(4,IP)

! Calculate plume segment sigma-y and sigma-z limit sizes

          IZM = IZP
          IZM = MAX(IZM, 1)
          IZM = MIN(IZM,NZ)

          YL = YLF*MIN(DX,DY)
          ZL = ZLF(IZM)*DZ(IZM)

! If the plume segment sigma-y or sigma-z are too large then add
! plume mass as emission to grid model

          IF (SIGYV .GT. YL .OR. SIGZV .GT. ZL) GOTO 110

! Goto next plume segment

          GOTO 100

  110     CONTINUE

! Current plume segment to be added as emission for grid model

! If the plume segment is outside of grid model then the mass is lost

          IF (IXP .LT. 1 .OR. IXP .GT. NX) GOTO 210
          IF (IYP .LT. 1 .OR. IYP .GT. NY) GOTO 210
          IF (IZP .LT. 1 .OR. IZP .GT. NZ) GOTO 210

! Plume segment inside grid model grid

! Go through all compounds

          DO 200 IC = 1,NC

! Get plume segment mass (g)

              MASSV = PU(10 + 2*IC,IP)

! Add plume segment mass as grid model emission (ug/m3*s)
!_SIGMA_Start:
!              DCDT(IC,IXP,IYP,IZP) = DCDT(IC,IXP,IYP,IZP) +
!     .                               1.0E+6*MASSV/(VOL(IZP)*DT)

!MSK start
! MSK new comment 23.01.2017
! Conversion from num/s to g/s   (num = number of particles)
! Number emission has the right time unit.
! We only have to take care that concentration CRV for PNC 
! Need to convert from number/m^3 to number/cm^3:
             IF ((IQU .EQ. 4).or.(CUNIT(IC)(1:3) == 'num') ) then
               DCDT(IC,IXP,IYP,IZP) = DCDT(IC,IXP,IYP,IZP) +      &
                 1.0E-6*MASSV/(DT*VOL(IZP)*DEPTHM(IXP,IYP)/MOD_H)
             ELSE
               DCDT(IC,IXP,IYP,IZP) = DCDT(IC,IXP,IYP,IZP) +      &
                 1.0E+6*MASSV/(DT*VOL(IZP)*DEPTHM(IXP,IYP)/MOD_H)
             ENDIF
!MSK end

!_SIGMA_End.

! Mark current plume segment to be deleted by setting new mass
! equal to zero

              PU(10 + 2*IC,IP) = 0.

  200     CONTINUE

         ! print *,'psgrid dcdt ', DCDT( 1,IXP,IYP,IZP) 

! Goto next plume segment

          GOTO 100

  210     CONTINUE

! Plume segment outside of grid

! Go through all compounds

          DO 220 IC = 1,NC

! Mark current plume segment to be deleted by setting new mass
! equal to zero

              PU(10 + 2*IC,IP) = 0.

  220     CONTINUE

! Next plume segment

  100 CONTINUE

      RETURN

! End of subroutine PSGRID

      end subroutine psgrid
