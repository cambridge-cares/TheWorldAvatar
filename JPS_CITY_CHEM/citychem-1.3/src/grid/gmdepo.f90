! <gmdepo.f90 - A component of the City-scale
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

      subroutine GMDEPO

! *** The subroutine calculates grid model dry and wet deposition.
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
!           2016  M. Karl: Indices of C has been shifted by +1 in x and y direction.
!           2017  M. Karl: Avoid very small concentration values
!     17.10.2017  M. Karl: Use DDEPV2D(IC,IX,IY) from Resistance approach (cddepvel.f90)
!
! ----------------------------------------------------------------------------------

      use mod_util
      use mod_main
      use mod_site
      use mod_time
      use mod_mete
      use mod_conc
      use mod_depo
      use mod_grid

! *** External functions

!MSK      REAL EXP4   !(is in mod_util)

      implicit none

! *** Local variables

!MSK      real :: C0
!MSK      real :: C1
      double precision :: C0,C1

      real :: PRECV
      real :: QDV
      real :: QTV
      real :: QWV
      integer :: IC
      integer :: IX
      integer :: IY
      integer :: IZ

! *** C0    - Concentration value
! *** C1    - Concentration value
! *** PRECV - Precipitation value
! *** QDV   - Dry   deposition rate value
! *** QWV   - Wet   deposition rate value
! *** QTV   - Total deposition rate value
! *** IC    - Compound index
! *** IX    - Grid model grid index in x-direction
! *** IY    - Grid model grid index in y-direction
! *** IZ    - Grid model grid index in z-direction

! *** Go through all grid squares

      DO IY = 1,NY
        DO IX = 1,NX

! ***     Convert precipitation from mm/h to m/s:

          PRECV = PREC(IX,IY)/3.6E+6

! ***     Go through all compounds:

          DO IC = 1,NC

! ***       Calculate dry deposition rate (/s):

!_LHS_SOA_May_2007_Start: 
!
! ***       The stuff below is taken from the 3.2 version.

!MSK start 17.10.2017 Use DDEPV2D(IC,IX,IY) from Resistance approach (cddepvel.f90)
!MSK                  Valid for Gases and Particles
!MSK            QDV = DDEPV(IC)/((DDEPV(IC)*AERO(IX,IY) + 1.)*DZ(1))
!MSK
            if (griddds.gt.0) then
              QDV = DDEPV2D(IC,IX,IY) / DZ(1)
            else
              QDV = 0
            endif
            !print *,'gmdepo QDV ',IC,DZ(1),QDV,DDEPV2D(IC,IX,IY),DDEPV2D(IC,IX,IY)*100.

! ***       Calculate dry deposition and update grid model concentration
! ***       and dry deposition

!MSK ***  C is shifted by 1 in x and y direction

!MSK            C0 = C(IC,IX,IY,1)
            C0 = C(IC,IX+1,IY+1,1)
            if (C0 .lt. 1.e-21)  C0 = 0.
            C1 = C0*EXP4(-QDV*DT)
!MSK            C(IC,IX,IY,1) = C1
            C(IC,IX+1,IY+1,1) = C1

            !print *,'gmdepo C0 C1', IX,IY,IC,C0,C1

            DDEPM(IX,IY,IC) = DDEPM(IX,IY,IC) + 1.E-6*(C0 - C1)*DZ(1)


! ***       Calculate wet deposition for whole column:

            DO IZ = 1,NZ

! ***         Calculate wet deposition rate in layer IZ (/s):

              QWV = WDEPSR(IC)*PRECV/DZ(IZ)


! ***         Calculate wet deposition and update grid model 
! ***         concentration and wet deposition:
!START BRUCE: Changed so that it happens properly
!MSK              C0 = C(IC,IX,IY,IZ)
              C0 = C(IC,IX+1,IY+1,IZ)
              C1 = C0*EXP4(-QWV*DT)
!MSK start
              C0 = MAX(DBLE(1.e-22),C0)
              C1 = MAX(DBLE(1.e-22),C1)
              if (C0 .lt. 1.e-21)  C0 = 0.
              if (C1 .lt. 1.e-21)  C1 = 0.
!MSK end
!MSK              C(IC,IX,IY,IZ) = C1
              C(IC,IX+1,IY+1,IZ) = C1
              WDEPM(IX,IY,IC) = WDEPM(IX,IY,IC) +   &
                              1.E-6*(C0 - C1)*DZ(IZ)

              !print *,'gmdepo wetdep: ', IC, IX, IY, IZ, WDEPM(IX,IY,IC), WDEPSR(IC), PRECV, DZ(IZ)

            END DO  ! IZ = 1,NZ

            
! ***       Next compound

!_LHS_SOA_May_2007_End.

          END DO ! IC = 1,NC

! ***     Next grid model grid square


        END DO
      END DO

      RETURN

! *** End of subroutine GMDEPO

      end subroutine gmdepo
