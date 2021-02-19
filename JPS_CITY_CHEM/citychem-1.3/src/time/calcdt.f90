! <calcdt.f90 - A component of the City-scale
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

      subroutine CALCDT

! The subroutine calculates the timestep for the current simulation period.
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
!    17 Oct 2017  M. Karl: L185-237  Fixing missing initialization
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

         implicit none

! Local variables

      REAL DMAX
      REAL DTIC
      REAL DUMV
      REAL,ALLOCATABLE :: DZMAX(:)
!MSK      REAL DZMAXV
      double precision DZMAXV
      REAL RATIO
      REAL RMAX
      REAL RMAX1
      REAL RMAX2
      REAL R1
      REAL R2
      REAL R3
      REAL R4
      REAL R5
      REAL R6
      REAL SCAL
      REAL,ALLOCATABLE :: SVMAX(:)
      REAL SVMAXV
      REAL TADIV
      REAL TADVX
      REAL TADVY
      REAL TADVZ
      REAL TDIFH
      REAL TDIFZ
      REAL UU,VV
      REAL UUMAX
      REAL VVMAX
      REAL WABSV
      REAL,ALLOCATABLE :: WMAX(:)
      REAL WMAXV

      INTEGER I
      INTEGER J
      INTEGER K
!MSK
      integer IC

! DMAX   - Maximum diffusion coefficient
! DTIC   - Minimum timestep for current compound
! DUMV   - Dummy value
! DZMAX  - Maximum value of DZDT
! DZMAXV - Maximum value of DZDT
! RATIO  - Ratio
! RMAX   - Maximum ratio
! RMAX1  - Maximum ratio
! RMAX2  - Maximum ratio
! SCAL   - Grid scale factor
! SVMAX  - Maximum wind v-component standard deviation
! SVMAXV - SVMAX value
! TADIV  - Maximum timestep for vertical advection and diffusion
! TADVX  - Maximum timestep for advection in x-direction
! TADVY  - Maximum timestep for advection in y-direction
! TADVZ  - Maximum timestep for advection in z-direction
! TDIFH  - Maximum timestep for horisontal diffusion
! TDIFV  - Maximum timestep for vertical diffusion
! UU     - Absolute u-component of wind
! VV     - Absolute v-component of wind
! UUMAX  - Maximum value of UU 
! VVMAX  - Maximum value of VV 
! WABSV  - Wind w-component absolute value
! WMAX   - Wind w-component maximum  value
! WMAXV  - WMAX value
! I      - Main grid indices
! J      - Main grid indices
! K      - Main grid indices

! Model run OK?

      IF (.NOT. RUNOK) THEN

          DT  = TSIMRES
          NTS = 1
          IF (MESSFE) WRITE (MESSUN,2000) NTS,DT

          RETURN

      END IF

! Allocate memory

      IF (.NOT. ALLOCATED(DZMAX)) ALLOCATE(DZMAX(MZ))
      IF (.NOT. ALLOCATED(SVMAX)) ALLOCATE(SVMAX(MZ))
      IF (.NOT. ALLOCATED(WMAX))  ALLOCATE(WMAX(MZ))

! Calculate maximum u- and v-components of wind

      UUMAX = 0.
      VVMAX = 0.

!MSK start 17.10.2017
      if (ITS.eq.0) then
        UUMAX = 0.
        VVMAX = 0.
      else
!MSK end      
        DO K = 1,NZ
          DO J = 1,NY
          DO I = 1,NX
!MSK start
               if (ITS .EQ. 0) then
                 UU = 0.0
                 VV = 0.0
               else
!MSK end
                 UU = ABS(U(I,J,K))
                 VV = ABS(V(I,J,K))
!MSK start
               endif
               if (UU .lt. 1.e-5)  UU = 0.0
               if (VV .lt. 1.e-5)  VV = 0.0
!MSK end
               UUMAX = MAX(UUMAX,UU)
               VVMAX = MAX(VVMAX,VV)
          END DO
          END DO
        END DO
!MSK start 17.10.2017
      end if
!MSK end
      
! Calculate maximum sigma-v values

      DO K = 1,NZ
          SVMAXV = 0.
          DO J = 1,NY
          DO I = 1,NX
              SVMAXV = MAX(SVMAXV,SIGV(I,J,K))
          END DO
          END DO
          SVMAX(K) = SVMAXV
      END DO

!MSK start
     do K = 1, NZ
       if (SVMAX(K).lt.1.e-32)  SVMAX(K)=0.0
       do J = 1,NY
         do I = 1,NX
           if (W(I,J,K).lt.1.e-32)  W(I,J,K)=0.0
         enddo
       enddo
     enddo
!MSK end

! Calculate maximum horisontal diffusion values

      SCAL = 0.1*MIN(DX,DY)
      DMAX = 0.

      DO K = 1,NZ
          D(K) = SVMAX(K)*SCAL
          DMAX = MAX(DMAX,D(K))
      END DO

! Calculate critical timestep for each horisontal advection and diffusion
! operator

      R1 = UUMAX/DX
      R2 = VVMAX/DY
      R3 = 2.*DMAX/DX2
      R4 = 2.*DMAX/DY2

      IF (R1 .GT. 0.) THEN
          TADVX = 1./R1
      ELSE
          TADVX = TSIMRES
      END IF
      IF (R2 .GT. 0.) THEN
          TADVY = 1./R2
      ELSE
          TADVY = TSIMRES
      END IF
      IF (R3 + R4 .GT. 0.) THEN
          TDIFH = 1./(R3 + R4)
      ELSE
          TDIFH = TSIMRES
      END IF

      IF (MESSFE) WRITE (MESSUN,2001) TADVX
      IF (MESSFE) WRITE (MESSUN,2002) TADVY
      IF (MESSFE) WRITE (MESSUN,2004) TDIFH

! Calculate maximum vertical windspeeds

      DO K = 1,NZ
          WMAXV = 0.
          DO J = 1,NY
          DO I = 1,NX
              WABSV = ABS(W(I,J,K))
              WMAXV = MAX(WMAXV,WABSV) 
          END DO
          END DO
          WMAX(K) = WMAXV
      END DO

! Go through all compounds and find minimum time step

      DT = TSIMRES

      DO IC = 1,NC

      IF (GRIDAS(IC) .GT. 0) DT = MIN(DT,TADVX,TADVY)
      IF (GRIDDS(IC) .GT. 0) DT = MIN(DT,TDIFH)


      IF (GRIDVS(IC) .EQ. 1) THEN

! Calculate maximum vertical diffusive exchanges for the combined
! advection and diffusion vertical scheme

          DO K = 1,NZ
              DZMAXV = 0.
              DO J = 1,NY
              DO I = 1,NX
                  DZMAXV = MAX(DZMAXV,DZDT(I,J,K))
              END DO
              END DO
              DZMAX(K) = DZMAXV
          END DO

! Calculate maximum ratio for combined vertical advection and diffusion

          RMAX = (DZMAX(1) + WMAX(1))/DZ(1)
          DO K = 2,NZ
              RATIO = ((DZMAX(K  ) + WMAX(K  ))/DZ(K) +      &
                  (DZMAX(K-1) + WMAX(K-1))/DZ(K-1))
              RMAX = MAX(RMAX,RATIO)
          END DO

! Calculate critical time step for the combined advection and
! diffusion scheme

          R5 = RMAX

          IF (R5 .GT. 0.) THEN
              TADIV = 1./R5
          ELSE
              TADIV = TSIMRES
          END IF

          IF (IC .EQ. 1 .AND. MESSFE) WRITE (MESSUN,2006) TADIV

! Calculate total critical time step

          DTIC = TADIV

      ELSEIF (GRIDVS(IC) .EQ. 2) THEN

! Calculate maximum vertical diffusive exchanges for the timesplitted
! advection and diffusion vertical scheme

          DO K = 1,NZ
              DZMAXV = 0.
              DO J = 1,NY
              DO I = 1,NX
                  DZMAXV = MAX(DZMAXV,DZDT(I,J,K))
              END DO
              END DO
              DZMAX(K) = DZMAXV
          END DO

! Calculate maximum ratio for vertical advection

          RMAX1 = WMAX(1)/DZ(1)
          DO K = 2,NZ
              RATIO = ((WMAX(K  )/DZ(K  )) +      &
                  (WMAX(K-1)/DZ(K-1)))
              RMAX1 = MAX(RMAX1,RATIO)
          END DO

! Calculate maximum ratio for vertical diffusion

          RMAX2 = DZMAX(1)/DZ(1)
          DO K = 2,NZ
              RATIO = ((DZMAX(K  )/DZ(K)) +      &
                  (DZMAX(K-1)/DZ(K-1)))
              RMAX2 = MAX(RMAX2,RATIO)
          END DO

! Calculate critical time step for the timesplitted advection and
! diffusion scheme

          R5 = RMAX1
          R6 = RMAX2

          IF (R5 .GT. 0.) THEN
              TADVZ = 1./R5
          ELSE
              TADVZ = TSIMRES
          END IF
          IF (R6 .GT. 0.) THEN
              TDIFZ = 1./R6
          ELSE
              TDIFZ = TSIMRES
          END IF

          IF (IC .EQ. 1 .AND. MESSFE) WRITE (MESSUN,2003) TADVZ
          IF (IC .EQ. 1 .AND. MESSFE) WRITE (MESSUN,2005) TDIFZ

! Calculate total critical time step

          DTIC = MIN(TADVZ,TDIFZ)

      ELSEIF (GRIDVS(IC) .EQ. 3 .OR. GRIDVS(IC) .EQ. 4) THEN

! Calculate critical timestep for vertical advection

          RMAX = WMAX(1)/DZ(1)
          DO K = 2,NZ
              RATIO = (WMAX(K  )/DZ(K) +      &
                  WMAX(K-1)/DZ(K-1))
              RMAX = MAX(RMAX,RATIO)
          END DO

          R5 = RMAX
          IF (R5 .GT. 0.) THEN
              TADVZ = 1./R5
          ELSE
              TADVZ = TSIMRES
          END IF

! When the Crank-Nicholson scheme is applied for vertical diffusion, a reasonable
! limit on the timestep should be imposed for accuracy reasons. At present this
! limit is calculated from the requirement that the tri-diagonal equation system
! should be strictly diagonally dominant for the columns as well as for the rows
! of the tri-diagonal matrix. The rows always fulfill this criteria for all values
! of the timestep.

!MSK start
          do K = 1, NZ
            do J = 1,NY
              do I = 1,NX
                if (DZDT(I,J,K).lt.1.e-32)  DZDT(I,J,K)=0.0
              enddo
            enddo
          enddo
!MSK end

! Calculate "critical" timestep for vertical diffusion

          TDIFZ = TSIMRES

          DO J = 1,NY
          DO I = 1,NX

              DUMV = DZDT(I,J,1)*(DZ(1) - DZ(2))

              IF (DUMV .GT. 0.0 )THEN
                  DUMV = (2.0*DZ(1)*DZ(2))/DUMV
                  TDIFZ = MIN(TDIFZ,DUMV)
              ENDIF

          END DO
          END DO

          DO K = 2,NZ-1
          DO J = 1,NY
          DO I = 1,NX

              DUMV = (DZDT(I,J,K  )*DZ(K-1)*(DZ(K) - DZ(K+1))) +     &
                (DZDT(I,J,K-1)*DZ(K+1)*(DZ(K) - DZ(K-1)))

              IF (DUMV .GT. 0.) THEN
                  DUMV = (2.0*DZ(K-1)*DZ(K)*DZ(K+1))/DUMV
                  TDIFZ = MIN(TDIFZ,DUMV)
              ENDIF

          END DO
          END DO
          END DO
 
          DO J = 1,NY
          DO I = 1,NX

              DUMV = (DZDT(I,J,NZ-1)*(DZ(NZ)-DZ(NZ-1))) -     &
                (DZDT(I,J,NZ)*DZ(NZ-1))

              IF (DUMV .GT. 0.) THEN
                  DUMV = (2.0*DZ(NZ)*DZ(NZ-1))/DUMV
                  TDIFZ = MIN(TDIFZ,DUMV)
              ENDIF

          END DO
          END DO

          IF (IC .EQ. 1 .AND. MESSFE) WRITE (MESSUN,2003) TADVZ
          IF (IC .EQ. 1 .AND. MESSFE) WRITE (MESSUN,2005) TDIFZ

! Calculate minimum timestep

!_LHS_Change_24April2012_Start:
!
! ***     For the BEDRE BYLUFT application with a very shallow lowermost layer,
! ***     the "artificial" TDIFZ requirement leads to very short timesteps.
! ***     Therefore we let TADVZ determine the timestep length:
 
!          DTIC = MIN(TADVZ,TDIFZ)
          DTIC = TADVZ          

!_LHS_Change_24April2012_End.

      END IF

      IF (GRIDVS(IC) .GT. 0) DT = MIN(DT,DTIC)

! Next compound

      END DO

! Multiply with timestep factor

      DT = DT*DTF

      !write(6,*) 'calcdt DT',TSIMRES,DT,DTF
      
! Adjust timestep so that we always ensure an even number of timesteps
! for each simulation period

      NTS = INT(TSIMRES/DT) + 1
      IF (MOD(NTS,2) .EQ. 1) NTS = NTS + 1
      DT = TSIMRES/NTS
      IF (MESSFE) WRITE (MESSUN,2000) NTS,DT

! Deallocate memory

      IF (ALLOCATED(DZMAX)) DEALLOCATE(DZMAX)
      IF (ALLOCATED(SVMAX)) DEALLOCATE(SVMAX)
      IF (ALLOCATED(WMAX))  DEALLOCATE(WMAX)

      RETURN

 2000 format ('CALCDT: ',I5,' timesteps of ',F6.1,' s')
 2001 format ('CALCDT: Min. TADVX timestep = ',F12.1,' s')
 2002 format ('CALCDT: Min. TADVY timestep = ',F12.1,' s')
 2003 format ('CALCDT: Min. TADVZ timestep = ',F12.1,' s')
 2004 format ('CALCDT: Min. TDIFH timestep = ',F12.1,' s')
 2005 format ('CALCDT: Min. TDIFZ timestep = ',F12.1,' s')
 2006 format ('CALCDT: Min. TADIV timestep = ',F12.1,' s')

! End of subroutine CALCDT

      end subroutine calcdt
