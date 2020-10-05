! <clsrcs.f90 - A component of the City-scale
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

      subroutine CLSRCS

! The subroutine calculates on static line sources data.
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
      use mod_depo
      use mod_lsrc

      implicit none

! Local variables

      real :: DV1
      real :: DV2
      real :: QLRDV
      real :: RQ
      real :: U1
      real :: U2
      real :: X1
      real :: Y1
      real :: X2
      real :: Y2

      integer :: IC
      integer :: IQL
      integer :: IRL

! IC  - Compound index
! IQL - Line source index
! IRL - Line source associated receptor point index

! Initially there are no line source associated receptor points

      NRL = 0
      NRLP = 0

! Go through all line sources

      DO 100 IQL = 1,NQL

! Define end points of the line source

          X1 = QLX(IQL,1)
          X2 = QLX(IQL,2)
          Y1 = QLY(IQL,1)
          Y2 = QLY(IQL,2)

! Calculate line source length (m)

          QLL(IQL) = SQRT((X2 - X1)*(X2 - X1) + (Y2 - Y1)*(Y2 - Y1))

! Check if receptor points are to be created associated with the
! current line source

          DV1 = QLRD(2*IQL - 1)
          DV2 = QLRD(2*IQL)

          IF (DV1 .GT. 0. .AND. DV2 .GT. 0.) THEN

! Calculate normal unit vector to line source

              IF (Y2 .NE. Y1) THEN
                  RQ = (X2 - X1)/(Y2 - Y1)
                  U1 = 1./SQRT(1. + RQ*RQ)
                  U2 = -U1*RQ
              ELSE
                  U1 = 0.
                  U2 = 1.
              ENDIF

! Create one receptor point on each side of the current line source

              NRL = NRL + 1
              XRL(NRL) = (X1 + X2)/2. + DV1*U1
              YRL(NRL) = (Y1 + Y2)/2. + DV1*U2
              ZRL(NRL) = 1.0
              NRL = NRL + 1
              XRL(NRL) = (X1 + X2)/2. - DV2*U1
              YRL(NRL) = (Y1 + Y2)/2. - DV2*U2
              ZRL(NRL) = 1.0
              NRLP = NRLP + 2

          ELSE

! No line source associated receptor coordinates

              NRL = NRL + 1
              XRL(NRL) = MISS
              YRL(NRL) = MISS
              ZRL(NRL) = MISS
              NRL = NRL + 1
              XRL(NRL) = MISS
              YRL(NRL) = MISS
              ZRL(NRL) = MISS

          ENDIF

! Next line source

  100 CONTINUE

       print *,'clsrcs: NQL NRL: ', NQL, NRL

!MSK start
      IF (NRL .GT. 0 .AND. .NOT. ALLOCATED(CRL)) ALLOCATE(CRL(NC,NRL))
      IF (NRL .GT. 0 .AND. .NOT. ALLOCATED(DDEPRL)) ALLOCATE(DDEPRL(NC,NRL))
      IF (NRL .GT. 0 .AND. .NOT. ALLOCATED(WDEPRL)) ALLOCATE(WDEPRL(NC,NRL))
!MSK end

! Go through all line source associated receptor points

      DO 110 IRL = 1,NRL

! Get distance from line source to receptor point

          QLRDV = QLRD(IRL)

! If distance is positive then goto next receptor point

          IF (QLRDV .GT. 0.) GOTO 110

! Go through all compounds

          DO 120 IC = 1,NC

! No calculations, set missing data

                 CRL(IC,IRL) = MISS
              DDEPRL(IC,IRL) = MISS
              WDEPRL(IC,IRL) = MISS

! Next compound

  120     CONTINUE

! Next receptor point

  110 CONTINUE

      RETURN

! End of subroutine CLSRCS

      end subroutine clsrcs
