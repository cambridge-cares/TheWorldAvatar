! <psadif.f90 - A component of the City-scale
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

      subroutine PSADIF

! The subroutine performs advection and diffusion for all plume segments.
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
      use mod_conc
      use mod_psrc

      implicit none

! Local variables

      real :: DD
      real :: DDIF
      real :: DDNEW
      real :: FF
      real :: FFNEW
      real :: FNEW
      real :: SIGVV
      real :: SIGWV
      real :: SIGYV
      real :: SIGZV
      real :: TL
      real :: TL4
      real :: TA
      real :: TF
      real :: TNEW
      real :: TNUL
      real :: TY
      real :: TZ
      real :: TZ2
      real :: UU
      real :: VV

      integer :: IP
      integer :: IX
      integer :: IY
      integer :: IZ

! DD    - Wind direction
! DDIF  - Wind direction difference
! DDNEW - Wind direction
! FF    - Wind speed
! FFNEW - Wind speed
! FNEW  - Used in calculating sigma-y or sigma-z
! SIGVV - Sigma-v and sigma-w values
! SIGWV - Sigma-v and sigma-w values
! SIGYV - Plume segment sigma-y and sigma-z values
! SIGZV - Plume segment sigma-y and sigma-z values
! TL    - Lagrangian time-scale values
! TL4   - Lagrangian time-scale values
! TA    - Used in calculating sigma-y or sigma-z
! TF    - Used in calculating sigma-y or sigma-z
! TNUL  - Used in calculating sigma-y or sigma-z
! TNEW  - Used in calculating sigma-y or sigma-z
! TY    - Used in calculating sigma-y or sigma-z
! TZ    - Used in calculating sigma-y or sigma-z
! TZ2   - Used in calculating sigma-y or sigma-z
! UU    - Wind u-component
! VV    - Wind v-component
! IP    - Plume segment index
! IX    - Plume segment main grid index in x-direction
! IY    - Plume segment main grid index in y-direction
! IZ    - Plume segment main grid index in z-direction

! Function type declaration
! function CWDIR is in mod_mete
!MSK      REAL CWDIR

! Go through all plume segments

      DO 100 IP = 1,NP

! Calculate plume segment main gridcell

          CALL GETMGI(1,PU(1,IP),PU(2,IP),PU(7,IP),IX,IY,IZ)

! Calculate plume segment wind speed and direction

          UU = U(IX,IY,IZ)
          VV = V(IX,IY,IZ)
          FF = SQRT(UU*UU + VV*VV)
          DD = CWDIR(UU,VV)

! Lower bound on windspeed for the calculations below

          FF = MAX(FF,PSRCFFMIN)

! Calculate plume segment advection

          PU(1,IP) = PU(1,IP) + UU*DT
          PU(2,IP) = PU(2,IP) + VV*DT

! Calculate plume segment diffusion

! Increase sigma-y (Irwin 1983)

          SIGYV    = PU(3,IP)
          SIGVV    = SIGV(IX,IY,IZ)
          TY       = SIGYV/SIGVV
          TF       = 0.9*TY/(2.*SQRT(1000.))
          TA       = TF + SQRT(TF*TF + TY)
          TNUL     = TA*TA
          TNEW     = TNUL + DT
          FNEW     = 1./(1. + 0.9*SQRT(TNEW/1000.))
          PU(3,IP) = SIGVV*TNEW*FNEW

! Increase sigma-z (Venkatram 1984)

          SIGZV    = PU(4,IP)
          SIGWV    = SIGWP(IX,IY,IZ)
          TL       = TLGRP(IX,IY,IZ)
          TL4      = 4.*TL
          TZ       = SIGZV/SIGWV
          TZ2      = TZ*TZ
          TNUL     = TZ2/TL4 + SQRT(TZ2*TZ2/(TL4*TL4) + TZ2)
          TNEW     = TNUL + DT
          FNEW     = 1./SQRT(1. + TNEW/(2.*TL))
          PU(4,IP) = SIGWV*TNEW*FNEW

! Calculate new plume segment time (s)

          PU(5,IP) = PU(5,IP) + DT

! Calculate new plume segment main gridcell

          CALL GETMGI(1,PU(1,IP),PU(2,IP),PU(7,IP),IX,IY,IZ)

! Calculate new plume segment wind speed and direction

          UU    = U(IX,IY,IZ)
          VV    = V(IX,IY,IZ)
          FFNEW = SQRT(UU*UU + VV*VV)
          DDNEW = CWDIR(UU,VV)

! Lower bound on windspeed for the calculations below

          FFNEW = MAX(FFNEW,PSRCFFMIN)

! Calculate new plume segment length

!C        PU(6,IP) = PU(6,IP) + (FFNEW - FF)*DT

! If the wind direction for the plume segment is changed too much then
! the plume segment becomes desoriented

          DDIF = ABS(DDNEW - PU(8,IP))
          IF (DDIF .GE. 180.) DDIF = 360. - DDIF
          IF (DDIF .GT.  RDL) PU(8,IP) = -PU(8,IP)

! Set new plume segment direction (deg)

          IF (PU(8,IP) .GE. 0.) PU(8,IP) = DDNEW

  100 CONTINUE

      RETURN

! End of subroutine PSADIF

      end subroutine psadif
