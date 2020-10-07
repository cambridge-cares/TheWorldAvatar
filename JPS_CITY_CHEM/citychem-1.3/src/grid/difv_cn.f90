! <difv_cn.f90 - A component of the City-scale
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

      subroutine DIFV_CN(IC)

! *** The subroutine calculates vertical diffusion applying 
! *** the Crank-Nicholson method
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
!                          y direction.
!           2017  M. Karl: Avoid very small concentration values
!
! ----------------------------------------------------------------------------------

      use mod_util
      use mod_main
      use mod_site
      use mod_time
      use mod_mete
      use mod_conc
      use mod_grid

      implicit none

! *** Scalar arguments

      integer :: IC

! *** IC - Index of compound

! *** Local variables

      integer :: I
      integer :: J
      integer :: K
! *** I,J,K               - Main grid indices
!
! *** DRYDEP_VEL(IC,I,J)  - Efficient deposition velocity


!      real,allocatable :: V_DIFF(:,:,:)
!
!      V_DIFF  - Verical eddy diffusivity. Equal to DZDT * (Height between grid midpoints).

      double precision gg_cn
      double precision,allocatable :: aa_k_cn(:,:,:)
      double precision,allocatable :: bb_k_cn(:,:,:)
      double precision,allocatable :: alfa_k_cn(:)
      double precision,allocatable :: beta_k_cn(:)
      double precision,allocatable :: gama_k_cn(:)
      double precision,allocatable :: rhs_k_cn(:)
      double precision,allocatable :: delta_k_cn(:)
      double precision,allocatable :: ceta_k_cn(:)
      double precision,allocatable :: meta_k_cn(:)

!_LHS_SOA_May_2007_Start:
      IF (.NOT. ALLOCATED(aa_k_cn))    ALLOCATE(aa_k_cn(NX,NY,NZ))
      IF (.NOT. ALLOCATED(bb_k_cn))    ALLOCATE(bb_k_cn(NX,NY,NZ))

!_LHS_SOA_May_2007_End.

      IF (.NOT. ALLOCATED(alfa_k_cn))  ALLOCATE (alfa_k_cn(NZ))
      IF (.NOT. ALLOCATED(beta_k_cn))  ALLOCATE (beta_k_cn(NZ))
      IF (.NOT. ALLOCATED(gama_k_cn))  ALLOCATE (gama_k_cn(NZ))
      IF (.NOT. ALLOCATED(rhs_k_cn))   ALLOCATE (rhs_k_cn(NZ))
      IF (.NOT. ALLOCATED(delta_k_cn)) ALLOCATE (delta_k_cn(NZ))
      IF (.NOT. ALLOCATED(ceta_k_cn))  ALLOCATE (ceta_k_cn(NZ))
      IF (.NOT. ALLOCATED(meta_k_cn))  ALLOCATE (meta_k_cn(NZ))


!MSK *** Indices of C and BC has been shifted by +1 in x and y direction

!_CITYDELTA_Start:
! 
! *** Preparation for the coefficients of the tri-diagonal eq.-system
! *** for the Crank-Nicolson solver of the Vertical Eddy Diffusion process.

      gg_cn = dble(1./DT)

      DO K = 1,NZ
        DO J = 1,NY
          DO I = 1,NX

            IF(K .EQ. 1)THEN
              aa_k_cn(I,J,K) = 0.0
              bb_k_cn(I,J,K) = DZDT(I,J,K) / (2.0*DZ(K))
            ELSE
              aa_k_cn(I,J,K) = DZDT(I,J,K-1) / (2.0 * DZ(K))
              bb_k_cn(I,J,K) = DZDT(I,J,K) / (2.0*DZ(K))
            ENDIF

          END DO
        END DO
      END DO

!MSK debug start pnc check
!debug       if (ic.eq.22) then
!debug         print *,'difv_cn DCDT before loop', IC, DCDT(IC,12,10,1),C(IC,13,11,1)
!debug       endif
!MSK debug
      DO J = 1,NY
        DO I = 1,NX

! ***     For each model coloumn calculate the tri-diagonal matrix 
! ***     constants and the right hand side of the matrix system.

            alfa_k_cn(1) = gg_cn + aa_k_cn(I,J,1) + bb_k_cn(I,J,1) 
            beta_k_cn(1) = - aa_k_cn(I,J,1)
            gama_k_cn(1) = - bb_k_cn(I,J,1)
            rhs_k_cn(1)  =                                                          &
!MSK              + ((gg_cn - bb_k_cn(I,J,1) - aa_k_cn(I,J,1)) * C(IC,I,J,1))           &
!MSK              + ( bb_k_cn(I,J,1) * C(IC,I,J,2))                                     &
              + ((gg_cn - bb_k_cn(I,J,1) - aa_k_cn(I,J,1)) * C(IC,I+1,J+1,1))           &
              + ( bb_k_cn(I,J,1) * C(IC,I+1,J+1,2))                                     &
              +   DCDT(IC,I,J,1)

!MSK debug start pnc check
!debug       if((ic.eq.22).and.(i.eq.12).and.(j.eq.10))then
!debug          print *,'difv_cn gg aa bb dzdt dz (1)',gg_cn, aa_k_cn(12,10,1), bb_k_cn(12,10,1), DZDT(12,10,1), DZ(1)
!debug          print *,'difv_cn r a g (1)',rhs_k_cn(1),alfa_k_cn(1),gama_k_cn(1)
!debug       endif
!MSK debug

!	&    - ( DRYDEP_VEL(IC,I,J) * C(IC,I,J,1) / DZ(1) )
!     &    - ( WETDEP_VEL(IC,I,J,1) * C(IC,I,J,1) / DZ(1) ) 

!_LHS_SOA_May_2007_Start:
!
! ***     Sam-Erik (og/eller Bruce) Har fjernet divisjonen med DZ(1) i
! ***     utrykket for tørravsetningstapet ovenfor.
!
!_LHS_SOA_May_2007_End.

!_LHS_Sep_2004: If the above expression for the dry deposition sink
! ***           is to be used, the reduced value of the concentration
! ***           should be used, i,e the value valid for the height
! ***           where the dry-deposition velocity is defined. (2 m?)

             DO K = 2,NZ-1

               alfa_k_cn(K) = gg_cn + aa_k_cn(I,J,K) + bb_k_cn(I,J,K) 
               beta_k_cn(K) = - aa_k_cn(I,J,K)
               gama_k_cn(K) = - bb_k_cn(I,J,K)
!MSK               rhs_k_cn(K)  =  ( aa_k_cn(I,J,K) * C(IC,I,J,K-1))               &
!MSK                  + ((gg_cn - bb_k_cn(I,J,K) - aa_k_cn(I,J,K)) * C(IC,I,J,K))  &
!MSK                  + ( bb_k_cn(I,J,K) * C(IC,I,J,K+1))                          &
               rhs_k_cn(K)  =  ( aa_k_cn(I,J,K) * C(IC,I+1,J+1,K-1))               &
                  + ((gg_cn - bb_k_cn(I,J,K) - aa_k_cn(I,J,K)) * C(IC,I+1,J+1,K))  &
                  + ( bb_k_cn(I,J,K) * C(IC,I+1,J+1,K+1))                          &
                  +   DCDT(IC,I,J,K)

!     &    - ( WETDEP_VEL(IC,I,J,K) * C(IC,I,J,K) / DZ(K) ) 

            ENDDO

             alfa_k_cn(NZ) = gg_cn + aa_k_cn(I,J,NZ) + bb_k_cn(I,J,NZ) 
             beta_k_cn(NZ) = - aa_k_cn(I,J,NZ)
             gama_k_cn(NZ) = - bb_k_cn(I,J,NZ)
!MSK             rhs_k_cn(NZ)  = ( aa_k_cn(I,J,NZ) * C(IC,I,J,NZ-1))              &
!MSK               + ((gg_cn - bb_k_cn(I,J,NZ) - aa_k_cn(I,J,NZ))*C(IC,I,J,NZ))   &
!MSK               + ( bb_k_cn(I,J,NZ) * 2.0 * BC(IC,I,J,NZ+1))                   &
             rhs_k_cn(NZ)  = ( aa_k_cn(I,J,NZ) * C(IC,I+1,J+1,NZ-1))              &
               + ((gg_cn - bb_k_cn(I,J,NZ) - aa_k_cn(I,J,NZ))*C(IC,I+1,J+1,NZ))   &
               + ( bb_k_cn(I,J,NZ) * 2.0 * BC(IC,I+1,J+1,NZ+1))                   &
               +   DCDT(IC,I,J,NZ)

!     &    - ( WETDEP_VEL(IC,I,J,NZ) * C(IC,I,J,NZ) / DZ(NZ) ) 

! ***     Start the algortihm for the tri-diagonal solver:

            delta_k_cn(1) = alfa_k_cn(1)
            ceta_k_cn(1)  = rhs_k_cn(1)

            DO K = 2,NZ
              meta_k_cn(K)  = beta_k_cn(K) / delta_k_cn(K-1)
              delta_k_cn(K) = alfa_k_cn(K) - (meta_k_cn(K)*gama_k_cn(K-1))
              ceta_k_cn(K)  = rhs_k_cn(K)  - (meta_k_cn(K)*ceta_k_cn(K-1))
            ENDDO
!MSK start avoid very small real
    !        if (ceta_k_cn(NZ) .lt. 1.e-28)  ceta_k_cn(NZ) = 0.0
!MSK end

!MSK            C(IC,I,J,NZ) = MAX(0.,real(ceta_k_cn(NZ)/delta_k_cn(NZ)) )
            C(IC,I+1,J+1,NZ) = MAX(DBLE(0.),ceta_k_cn(NZ)/delta_k_cn(NZ) )

!MSK debug start pnc check
!debug       if((ic.eq.22).and.(i.eq.12).and.(j.eq.10))then
!debug          print *,'difv_cn c d g C',ceta_k_cn(1),delta_k_cn(1),gama_k_cn(1),C(22,13,11,2)
!debug          print *,'difv_cn DCDT in the loop', IC, DCDT(IC,12,10,1),C(IC,13,11,1)
!debug       endif
!MSK debug 

            DO K = NZ-1,1,-1
!MSK start avoid very small real
      !        if ( ceta_k_cn(K) .lt. 1.e-28)  ceta_k_cn(K) = 0.0
              if ( C(IC,I+1,J+1,K+1).lt. 1.e-28)  C(IC,I+1,J+1,K+1) = 0.0
!MSK end

!MSK               C(IC,I,J,K) = MAX(0.,real((ceta_k_cn(K)                        &
!MSK                    - (gama_k_cn(K) * C(IC,I,J,K+1)))/delta_k_cn(K)) )
               C(IC,I+1,J+1,K) = MAX(DBLE(0.),(ceta_k_cn(K)                        &
                    - (gama_k_cn(K) * C(IC,I+1,J+1,K+1)))/delta_k_cn(K) )

            ENDDO

! ***     Finished with the tri-diagonal solver algotithm. The values
! ***     of C(IC,I,J,K) are now updated for: t = t+DT.  

        ENDDO
      ENDDO

!MSK debug start pnc check
!debug       if((ic.eq.21).or.(ic.eq.22).or.(ic.eq.23) )then
!debug         print *,'difv_cn DCDT after loop', IC, DCDT(IC,12,10,1),C(IC,13,11,1)
!debug       endif
!MSK debug

! ***  Alternative to using the MAX-expressions above:
!
!      DO 200 K = 1,NZ
!        DO 210 J = 1,NY
!          DO 220 I = 1,NX
!
! ***        If concentrations are negative then write a message
!
!            IF (C(IC,I,J,K) .LT. -1.E-37 .AND. IC .NE. 39) THEN
!
!              C_UTSKR  = C(IC,I,J,K)*CMOLW(IC)/avogad*1.0E+12
!              IF (MESSFE) WRITE (MESSUN,1000)
!     &                      ITS,IC,I,J,K,C_UTSKR,DCDT(IC,I,J,K)
!
! ***          Set negative concentrations to zero:
!              IF (C(IC,I,J,K) .LT. 0.) C(IC,I,J,K) = 0.
!
!            ENDIF
!              
!		 END DO
!        END DO
!      END DO
!
! ***  End of alternative to using the MAX-expressions above.

      if (ALLOCATED(aa_k_cn))    deallocate(aa_k_cn)
      if (ALLOCATED(bb_k_cn))    deallocate(bb_k_cn)
      if (ALLOCATED(alfa_k_cn))  deallocate(alfa_k_cn)
      if (ALLOCATED(beta_k_cn))  deallocate(beta_k_cn)
      if (ALLOCATED(gama_k_cn))  deallocate(gama_k_cn)
      if (ALLOCATED(rhs_k_cn))   deallocate(rhs_k_cn)
      if (ALLOCATED(delta_k_cn)) deallocate(delta_k_cn)
      if (ALLOCATED(ceta_k_cn))  deallocate(ceta_k_cn)
      if (ALLOCATED(meta_k_cn))  deallocate(meta_k_cn)

      RETURN

 1000 format ('DIFV_CN: Timestep = ',I3,' IC = ',I2,' I = ',I3,' J = ',      &
             I3,' K = ',I2,1P,' C(IC,I,J,K) = ',E12.4,                       &
             ' DCDT(IC,I,J,K) = ',E12.4)

! *** End of subroutine DIFV_CN

      end subroutine difv_cn
