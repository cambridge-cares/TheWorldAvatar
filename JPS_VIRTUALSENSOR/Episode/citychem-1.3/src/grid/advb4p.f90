! <advb4p.f90 - A component of the City-scale
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

      subroutine ADVB4P(IC,K)

! The subroutine calculates 2D advection for compound IC in layer K by
! using the positive definite Bott scheme B4P and by using Marchuk
! timesplitting.
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
!           2016  M. Karl: Indices of C and BC has been shifted by +1 in x and
!                          y direction. CE defined as double precision.
!
! ----------------------------------------------------------------------------------

      use mod_site
      use mod_time
      use mod_mete
      use mod_conc
      use mod_grid

      implicit none

      integer :: IC,K

! IC - Index of compound
! K  - Index of vertical layer

! Local variables

!MSK      REAL,ALLOCATABLE :: CE(:,:)
      double precision,allocatable :: CE(:,:)
      real,allocatable :: COURU(:,:)
      real,allocatable :: COURV(:,:)
      real,allocatable :: COURX(:)
      real,allocatable :: COURY(:)
      real,allocatable :: VX(:)
      real,allocatable :: VY(:)
      real,allocatable :: FMX(:)
      real,allocatable :: FMY(:)
      real,allocatable :: FPX(:)
      real,allocatable :: FPY(:)
      real,allocatable :: WX(:)
      real,allocatable :: WY(:)

      integer :: I
      integer :: J
      integer :: MXP2
      integer :: MYP2
      integer :: NXP2
      integer :: NYP2

! CE    - Expanded concentration grid
! COURU - Courant numbers in x-dir satisfying the CFL-criterion
! COURV - Courant numbers in y-dir satisfying the CFL-criterion
! COURX - Courant numbers in x-dir satisfying the CFL-criterion
! COURY - Courant numbers in y-dir satisfying the CFL-criterion
! VX    - Concentration values for advection in x-dir
! VY    - Concentration values for advection in y-dir
! FMX   - Fluxes in x-dir for u(i) < 0 and u(i) > 0, respectively
! FMX   - Fluxes in y-dir for v(i) < 0 and v(i) > 0, respectively
! WX    - The positive definite flux limiters in x-dir
! WY    - The positive definite flux limiters in y-dir
! I,J   - Index of grid cell
! MXP2  - MX + 2
! MXP2  - MY + 2
! NXP2  - Expanded grid X-dimension = NX + 2
! NYP2  - Expanded grid Y-dimension = NY + 2

!_DYN_ALLOC_Start:
      IF (.NOT. ALLOCATED(CE))    ALLOCATE(CE(MX+2,MY+2))
      IF (.NOT. ALLOCATED(COURU)) ALLOCATE(COURU(MX+2,MY+2))
      IF (.NOT. ALLOCATED(COURV)) ALLOCATE(COURV(MX+2,MY+2))
      IF (.NOT. ALLOCATED(COURX)) ALLOCATE(COURX(MX+2))
      IF (.NOT. ALLOCATED(COURY)) ALLOCATE(COURY(MY+2))
      IF (.NOT. ALLOCATED(VX))    ALLOCATE(VX(MX+2))
      IF (.NOT. ALLOCATED(VY))    ALLOCATE(VY(MY+2))
      IF (.NOT. ALLOCATED(FMX))   ALLOCATE(FMX(MX+2))
      IF (.NOT. ALLOCATED(FMY))   ALLOCATE(FMY(MY+2))
      IF (.NOT. ALLOCATED(FPX))   ALLOCATE(FPX(MX+2))
      IF (.NOT. ALLOCATED(FPY))   ALLOCATE(FPY(MY+2))
      IF (.NOT. ALLOCATED(WX))    ALLOCATE(WX(MX+2))
      IF (.NOT. ALLOCATED(WY))    ALLOCATE(WY(MY+2))
!_DYN_ALLOC_End.

!MSK *** Indices of C and BC has been shifted by +1 in x and y direction

! Size of the expanded grid is (NX + 2,NY + 2)

      MXP2 = MX + 2
      MYP2 = MY + 2

      NXP2 = NX + 2
      NYP2 = NY + 2

! Calculate Courant numbers for the expanded grid (including gridcell
! boundaries)

      DO 100 J = 1,NY
          COURU(1,J+1) = DT*U(1,J,K)/DX
          DO 110 I = 1,NX-1
              COURU(I+1,J+1) = DT*0.5*(U(I,J,K) + U(I+1,J,K))/DX
  110     CONTINUE
          COURU(NX+1,J+1) = DT*U(NX,J,K)/DX
          COURU(NX+2,J+1) = COURU(NX+1,J+1)
  100 CONTINUE

      DO 120 I = 1,NX
          COURV(I+1,1) = DT*V(I,1,K)/DY
          DO 130 J = 1,NY-1
              COURV(I+1,J+1) = DT*0.5*(V(I,J,K) + V(I,J+1,K))/DY
  130     CONTINUE
          COURV(I+1,NY+1) = DT*V(I,NY,K)/DY
          COURV(I+1,NY+2) = COURV(I+1,NY+1)
  120 CONTINUE

! Copy concentrations to expanded grid

      DO 140 J = 1,NY
      DO 150 I = 1,NX
!_SIGMA_Start:
!_org     CE(I+1,J+1) = C(IC,I,J,K)

!MSK          CE(I+1,J+1) = C(IC,I,J,K) * DEPTHM(I,J)
          CE(I+1,J+1) = C(IC,I+1,J+1,K) * DEPTHM(I,J)
!_SIGMA_End.
  150 CONTINUE
  140 CONTINUE

! Left (I = 1) and right (I = NXP2) boundary of the expanded grid
!MSK 11.09.2017
!MSK BC value must be from outer row & column

      DO 160 J = 1,NY

          IF (COURU(   1,J+1) .GT. 0.) THEN
!_SIGMA_NEST_Start:
!_org         CE( 1,J+1) = BC(IC)
!MSK start
!MSK              CE( 1,J+1) = BC(IC,0,J,K) * DEPTHM(1,J)
              if ( BC(IC,1,J+1,K) .lt. 1.e-28 )   BC(IC,1,J+1,K) = 0.0
              CE( 1,J+1) = BC(IC,1,J,K) * DEPTHM(1,J)
!test              CE( 1,J+1) = BC(IC,1,J+1,K) * DEPTHM(1,J)
!MSK end
          ELSE
!_org         CE( 1,J+1) = C(IC,1,J,K)
!MSK start
!MSK              CE( 1,J+1) = C(IC,1,J,K) * DEPTHM(1,J)
              if ( C(IC,2,J+1,K) .lt. 1.e-28 )   C(IC,2,J+1,K) = 0.0
              CE( 1,J+1) = C(IC,2,J+1,K) * DEPTHM(1,J)
!MSK end 

!_SIGMA_NEST_End.
          ENDIF

          IF (COURU(NXP2-1,J+1) .LT. 0.) THEN
!_SIGMA_NEST_Start:
!_org         CE(NXP2,J+1) = BC(IC)
!MSK start
!MSK              CE(NXP2,J+1) = BC(IC,NX+1,J,K) * DEPTHM(NX,J)
              if ( BC(IC,NX+2,J+1,K) .lt. 1.e-28 )   BC(IC,NX+2,J+1,K) = 0.0
              CE(NXP2,J+1) = BC(IC,NX+2,J,K) * DEPTHM(NX,J)
!test              CE(NXP2,J+1) = BC(IC,NX+2,J+1,K) * DEPTHM(NX,J)
!MSK end
          ELSE
!_org         CE(NXP2,J+1) = C(IC,NX,J,K)
!MSK start
!MSK              CE(NXP2,J+1) = C(IC,NX,J,K) * DEPTHM(NX,J)
              if ( C(IC,NX+1,J+1,K) .lt. 1.e-28 )   C(IC,NX+1,J+1,K) = 0.0
              CE(NXP2,J+1) = C(IC,NX+1,J+1,K) * DEPTHM(NX,J)
!MSK end

!_SIGMA_NEST_End.
          ENDIF

  160 CONTINUE

! Lower (J = 1) and upper (J = NYP2) boundary of the expanded grid
!MSK 11.09.2017
!MSK BC value must be from outer row & column

      DO 170 I = 1,NX

          IF (COURV(I+1,   1) .GT. 0.) THEN
!_SIGMA_NEST_Start:
!_org         CE(I+1, 1) = BC(IC)
!MSK start
!MSK              CE(I+1, 1) = BC(IC,I,0,K) * DEPTHM(I,1)
              if ( BC(IC,I+1,1,K) .lt. 1.e-28 )   BC(IC,I+1,1,K) = 0.0
              CE(I+1, 1) = BC(IC,I,1,K) * DEPTHM(I,1)
!test              CE(I+1, 1) = BC(IC,I+1,1,K) * DEPTHM(I,1)
!MSK end
          ELSE
!_org         CE(I+1, 1) = C(IC,I,1,K)
!MSK start
!MSK              CE(I+1, 1) = C(IC,I,1,K) * DEPTHM(I,1)
              if ( C(IC,I+1,2,K) .lt. 1.e-28 )   C(IC,I+1,2,K) = 0.0
              CE(I+1, 1) = C(IC,I+1,2,K) * DEPTHM(I,1)
!MSK end

!_SIGMA_NEST_End.
          ENDIF

          IF (COURV(I+1,NYP2-1) .LT. 0.) THEN
!_SIGMA_NEST_Start: 
!_org         CE(I+1,NYP2) = BC(IC)

!MSK              CE(I+1,NYP2) = BC(IC,I,NY+1,K) * DEPTHM(I,NY)
              CE(I+1,NYP2) = BC(IC,I,NY+2,K) * DEPTHM(I,NY)
!test              CE(I+1,NYP2) = BC(IC,I+1,NY+2,K) * DEPTHM(I,NY)
          ELSE
!_org         CE(I+1,NYP2) = C(IC,I,NY,K)

!MSK              CE(I+1,NYP2) = C(IC,I,NY,K) * DEPTHM(I,NY)
              CE(I+1,NYP2) = C(IC,I+1,NY+1,K) * DEPTHM(I,NY)
!_SIGMA_NEST_End.
          ENDIF

  170 CONTINUE

! If odd timestep then advx -> advy else advy -> advx

      IF (MOD(ITS,2) .EQ. 0) GOTO 299

! Perform advection in the x-direction

      DO 200 J = 2,NYP2-1

          DO 210 I = 1,NXP2
              VX(I) = CE(I,J)
              COURX(I) = COURU(I,J)
  210     CONTINUE

          call b4p(VX,COURX,MXP2,NXP2,FMX,FPX,WX)

          DO 220 I = 2,NXP2-1
              CE(I,J) = VX(I)
  220     CONTINUE

  200 CONTINUE

! Perform advection in the y-direction

      DO 230 I = 2,NXP2-1

          DO 240 J = 1,NYP2
              VY(J) = CE(I,J)
              COURY(J) = COURV(I,J)
  240     CONTINUE

          call b4p(VY,COURY,MYP2,NYP2,FMY,FPY,WY)

          DO 250 J = 2,NYP2-1
              CE(I,J) = VY(J)
  250     CONTINUE

  230 CONTINUE

      GOTO 399

  299 CONTINUE

! Perform advection in the y-direction

      DO 300 I = 2,NXP2-1

          DO 310 J = 1,NYP2
              VY(J) = CE(I,J)
              COURY(J) = COURV(I,J)
  310     CONTINUE

          call b4p(VY,COURY,MYP2,NYP2,FMY,FPY,WY)

          DO 320 J = 2,NYP2-1
              CE(I,J) = VY(J)
  320     CONTINUE

  300 CONTINUE

! Perform advection in the x-direction

      DO 330 J = 2,NYP2-1

          DO 340 I = 1,NXP2
              VX(I) = CE(I,J)
              COURX(I) = COURU(I,J)
  340     CONTINUE

          call b4p(VX,COURX,MXP2,NXP2,FMX,FPX,WX)

          DO 350 I = 2,NXP2-1
              CE(I,J) = VX(I)
  350     CONTINUE

  330 CONTINUE

  399 CONTINUE

! Copy concentrations back from the expanded grid

      DO 400 J = 1,NY
      DO 410 I = 1,NX
!_SIGMA_Start:
!_org          C(IC,I,J,K) = CE(I+1,J+1)
!MSK start
          if ( CE(I+1,J+1) .lt. 1.e-28 )   CE(I+1,J+1) = 0.0


!MSK          C(IC,I,J,K) = CE(I+1,J+1) / DEPTHM(I,J)
          C(IC,I+1,J+1,K) = CE(I+1,J+1) / DEPTHM(I,J)
!MSK end
!_SIGMA_End.

  410 CONTINUE
  400 CONTINUE

!_DYN_ALLOC_Start:
      IF (ALLOCATED(CE))    DEALLOCATE(CE)
      IF (ALLOCATED(COURU)) DEALLOCATE(COURU)
      IF (ALLOCATED(COURV)) DEALLOCATE(COURV)
      IF (ALLOCATED(COURX)) DEALLOCATE(COURX)
      IF (ALLOCATED(COURY)) DEALLOCATE(COURY)
      IF (ALLOCATED(VX))    DEALLOCATE(VX)
      IF (ALLOCATED(VY))    DEALLOCATE(VY)
      IF (ALLOCATED(FMX))   DEALLOCATE(FMX)
      IF (ALLOCATED(FMY))   DEALLOCATE(FMY)
      IF (ALLOCATED(FPX))   DEALLOCATE(FPX)
      IF (ALLOCATED(FPY))   DEALLOCATE(FPY)
      IF (ALLOCATED(WX))    DEALLOCATE(WX)
      IF (ALLOCATED(WY))    DEALLOCATE(WY)
!_DYN_ALLOC_End.


      RETURN

! End of subroutine ADVB4P

      end subroutine advb4p
