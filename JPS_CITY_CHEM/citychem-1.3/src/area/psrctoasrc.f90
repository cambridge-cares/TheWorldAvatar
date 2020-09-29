! <PSRCtoASRC.f90 - A component of the City-scale
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

      subroutine PSRCtoASRC

! *** This subroutine converts point source emissions 
! *** to area emissions.
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
      use mod_asrc

      implicit none

! *** Local variables:

      REAL ABH
      REAL ABW
      REAL ADI
      REAL AHE
      REAL AHS
      REAL ATA
      REAL ATG
      REAL AVG
      REAL AWS
      REAL HMIXV
      REAL HNEWV
      REAL MOBULV
      REAL PSV
      REAL SDV
      REAL UDV
      REAL USTARV
      REAL UU
      REAL VV
      REAL XFV
      REAL Z0V
      REAL Z2V

      INTEGER IDHV
      INTEGER IDV
      INTEGER IQ
      INTEGER IC
      INTEGER IX
      INTEGER IY
      INTEGER IZ

! *** ABH    - Stack building height
! *** ABW    - Stack building width
! *** ADI    - Stack diameter
! *** AHE    - Stack efficient emission height
! *** AHS    - Stack physical height
! *** ATA    - Air temperature
! *** ATG    - Gas temperature
! *** AVG    - Gas velocity
! *** AWS    - Windspeed in height Z2V
! *** HMIXV  - Mixing height
! *** HNEWV  - Final plume emission height
! *** MOBULV - Monin-Obukhov length
! *** PSV    - Stack penetration
! *** SDV    - Stack downwash height
! *** UDV    - Windspeed value
! *** USTARV - Friction velocity
! *** UU     - Wind u-component
! *** VV     - Wind v-component
! *** XFV    - Final distance
! *** Z0V    - Surface roughness
! *** Z2V    - Height for windspeed AWS
! *** IDHV   - Trapped in cavity zone indicator
! *** IDV    - Calculate stack downwash indicator
! *** IQ     - Point source index
! *** IC     - Compound index
! *** IX     - Main grid indices
! *** IY     - Main grid indices
! *** IZ     - Main grid indices

! *** Go through all point sources

      DO IQ = 1,NQ

! ***     Calculate point source main grid coordinates

          CALL GETMGI(1,QX(IQ),QY(IQ),QHS(IQ),IX,IY,IZ)

! ***     Get site and meteorological data

          Z0V    = Z0(IX,IY)
          Z2V    = DZ(1)/2.
          ATA    = TAIR(IX,IY) + CTOK 
          UU     = U(IX,IY,1)
          VV     = V(IX,IY,1)
          AWS    = SQRT(UU*UU + VV*VV)
          USTARV = USTAR(IX,IY)
          MOBULV = MOBUL(IX,IY)
          HMIXV  = HMIX(IX,IY)

! ***     Lower bound on windspeed for the calculations below

          AWS = MAX(AWS,PSRCFFMIN)

! ***     Get point source data

          AHS = QHS(IQ)
          ATG = QTG(IQ)
          AVG = QVG(IQ)
          ADI = QDI(IQ)
          ABH = QHB(IQ)
          ABW = QWB(IQ)

! ***     If not a volume source then calculate the actual 
! ***     emission height

          IF (QDI(IQ) .GT. 0. .AND. QVG(IQ) .GT. 0) THEN

              IDV = 0
              ATG = MAX(ATG,ATA)

! ***         Calculate plume rise:

              call heff(IDV,AHS,ATG,AVG,ADI,ABH,ABW,Z0V,Z2V,ATA,AWS, &
                       USTARV,MOBULV,AHE,XFV,UDV,HMIXV,HNEWV,PSV,   &
                       SDV,IDHV)

! ***         Final emission height and degree of penetration

              QHE(IQ) = HNEWV
              QPS(IQ) = PSV

          ENDIF

! ***     Find grid cell in which to introduce the emission from 
! ***     point source IQ:

          CALL GETMGI(1,QX(IQ),QY(IQ),QHE(IQ),IX,IY,IZ)
!_LHS_Change_20Oct2011_Start:
! ***     Reset to first layer for test purposes:
!          IZ = 1
!_LHS_Change_20Oct2011_End.
          DO IC = 1,NC
            QA(IC,IX,IY,IZ) = QA(IC,IX,IY,IZ) + QEM(IQ,IC)
          ENDDO

      ENDDO

      RETURN

! *** End of subroutine PSRCtoASRC

      end subroutine PSRCtoASRC
