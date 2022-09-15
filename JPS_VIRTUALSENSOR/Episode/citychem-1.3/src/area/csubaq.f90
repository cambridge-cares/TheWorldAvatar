! <csubaq.f90 - A component of the City-scale
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

      subroutine CSUBAQ 

! The subroutine adds emission from the area sources to the grid model.
!
!     Note that the standard unit for emissions is: g/s
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
      use mod_grid
      use mod_asrc

      implicit none

! Local variables

      REAL,ALLOCATABLE :: SCALV(:,:)
      INTEGER IC,IX,IY,IZ

! SCALV    - Scale factors
! IC       - Compound index
! IX,IY,IZ - Grid model indices

!_DYN_ALLOC_Start:
      IF (.NOT. ALLOCATED(SCALV)) ALLOCATE(SCALV(NC,NZ))
!_DYN_ALLOC_End.

! Set emission to grid model concentration rates scale factors

      DO 100 IZ = 1,NZ
      DO 110 IC = 1,NC

          SCALV(IC,IZ) = 0.0

! If mass unit is ug then concentration unit is ug/m3

          IF (CUNIT(IC)(1:2) .EQ. 'ug' ) &
             SCALV(IC,IZ) = 1.E6/(VOL(IZ))

          !BRUCE. Emissions are in numbers and concentrations are in num/cm3
          if (CUNIT(IC)(1:3) .EQ. 'num') then
             SCALV(IC,IZ) = 1/(VOL(IZ)*1.E+6)
          endif


! If mass unit is mol then concentration unit is: molecules/cm3


!         In the general case where the emissions are in [g/s] and
!         the applied concentration unit is [molecules/cm3] the
!         scaling factor should be:
!
          IF (CUNIT(IC)(1:3) .EQ. 'mol') &
                 SCALV(IC,IZ) = avogad/(CMOLW(IC)*VOL(IZ)*1.E+6)


  110 CONTINUE
  100 CONTINUE

! Add emission from the area sources (ug/m3*s)

      DO 120 IZ = 1,NZ
      DO 130 IY = 1,NY
      DO 140 IX = 1,NX
      DO 150 IC = 1,NC
!_SIGMA Start:
!          DCDT(IC,IX,IY,IZ) = DCDT(IC,IX,IY,IZ) +
!     .                        SCALV(IC,IZ)*QA(IC,IX,IY,IZ)

          DCDT(IC,IX,IY,IZ) = DCDT(IC,IX,IY,IZ) + &
             (QA(IC,IX,IY,IZ)*SCALV(IC,IZ)*MOD_H/DEPTHM(IX,IY))
!_SIGMA End.
  150 CONTINUE
  140 CONTINUE
  130 CONTINUE
  120 CONTINUE

!MSK debug start pnc check
!debug       do IC=21,23
!debug           print *,'csubaq DCDT', IC, DCDT(IC,12,10,1),QA(IC,12,10,1),SCALV(IC,1)
!debug       enddo
!MSK debug end

!_DYN_ALLOC_Start:
      IF (ALLOCATED(SCALV)) DEALLOCATE(SCALV)
!_DYN_ALLOC_End.

      RETURN

! End of subroutine CSUBAQ

      end subroutine csubaq
