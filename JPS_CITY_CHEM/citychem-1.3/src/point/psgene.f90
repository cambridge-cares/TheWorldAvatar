! <psgene.f90 - A component of the City-scale
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

      subroutine psgene

! The subroutine generates a set of new plume segments from all the point sources.
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
      real :: FF
      real :: QEMV
      real :: UU
      real :: VV

      integer :: I
      integer :: IC
      integer :: IX
      integer :: IY
      integer :: IZ
      integer :: K

      real, allocatable :: PU2(:,:)

      integer :: j 

! DD   - Wind direction
! FF   - Wind speed
! QEMV - Point source emission value
! UU   - Wind u-component
! VV   - Wind v-component
! I    - Point source index
! IC   - Compound index
! IX   - Plume segment main grid index in x-direction
! IY   - Plume segment main grid index in y-direction
! IZ   - Plume segment main grid index in z-direction
! K    - Plume segment index

! External function type declarations

!MSK      REAL CWDIR           !mod_mete
!MSK      INTEGER ILAY         !mod_site
!MSK      LOGICAL BFTIME       !mod_time

! New plume segments are introduced, the number increases from NP
! to NP + NQ

! Enough allocated memory for puffs?

      IF (NP + NQ .GT. MP) THEN

! No, will increase puff array size to NP + NQ

! Allocate temporary puff array

      IF (NP .GT. 0) THEN
          IF (.NOT. ALLOCATED(PU2)) ALLOCATE(PU2(10+2*NC,NP))
      ENDIF

! Copy values to temporary array

      DO  K = 1,NP
      DO IC = 1,10+2*NC
        PU2(IC,K) = PU(IC,K)
      ENDDO
      ENDDO

! Deallocate existing array

      IF (ALLOCATED(PU)) DEALLOCATE(PU)

! Allocate new array

      ALLOCATE(PU(10+2*NC,NP+NQ))
      MP = NP + NQ

! Copy values from temporary array to existing array

      DO  K = 1,NP
      DO IC = 1,10+2*NC
        PU(IC,K) = PU2(IC,K)
      ENDDO
      ENDDO

! Deallocate temporary array

      IF (ALLOCATED(PU2)) DEALLOCATE(PU2)

      ENDIF

      K = NP

! Go through all point sources

      DO 100 I = 1,NQ

          K = K + 1

! Plume segment X, Y, sigma-Y, sigma-Z (m) and time (s)

          PU(1,K) = QX(I)
          PU(2,K) = QY(I)
          PU(3,K) = QSY(I)
          PU(4,K) = QSZ(I)
          PU(5,K) = 0.

! Plume segment height above ground (m)

          PU(7,K) = QHE(I)

! Plume segment point source number and vertical layer index

          PU( 9,K) = I
          PU(10,K) = ILAY(QHE(I))

! Calculate plume segment main gridcell

          CALL GETMGI(1,PU(1,K),PU(2,K),PU(7,K),IX,IY,IZ)

! Calculate plume segment wind speed and direction

          UU = U(IX,IY,IZ)
          VV = V(IX,IY,IZ)
          FF = SQRT(UU*UU + VV*VV)
          DD = CWDIR(UU,VV)

! Lower bound on windspeed for the calculations below

          FF = MAX(FF,PSRCFFMIN)

! Calculate plume segment length (m)

          PU(6,K) = FF*DT

! Set plume segment direction (deg)

          PU(8,K) = DD

! Plume segment emission (g/s) and mass (g)

          DO 120 IC = 1,NC

              QEMV = QEM(I,IC)

! Reduce emission due to eventual penetration of mixing layer

              QEMV = QEMV*(1. - QPS(I))

              PU( 9 + 2*IC,K) = QEMV
              PU(10 + 2*IC,K) = QEMV*DT

              !if ( (i==1).and.(ic==2).and.(k==1) ) then
              !  print *,'psgene np,nq, i,ic,k',np,nq,i,ic,k,QEM(I,IC),QEMV,PU(9+2*nc,k),PU(10+2*nc,k)
              !endif

  120     CONTINUE

! Next point source


  100 CONTINUE

       !print *,'psgene: PU ', (PU(J,1),J=1,2 + 2*NC)

! Now there are K plume segments

      NP = K
      RETURN

  999 CONTINUE

! Too many plume segments, stop

      IF (MESSFE) WRITE (MESSUN,2000) MP
      CALL STOPIT('PSGENE: Too many plume segments!')

 2000 format('PSGENE: Too many plume segments, maximum allowed = ',      &
            I7)

! End of subroutine PSGENE

      end subroutine psgene
