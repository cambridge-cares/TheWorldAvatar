! <adiv.f90 - A component of the City-scale
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

      subroutine ADIV(IC)

! *** The subroutine calculates vertical advection and diffusion.
!
! *** A preliminary version of dry deposition can also be included
! *** in the expression for the lowermost model layer.
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
!           2016  M. Karl: Defined VTR as double precision
!
! ----------------------------------------------------------------------------------


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

!MSK      real    :: VTR
      double precision :: VTR
      integer :: I
      integer :: J
      integer :: K

! *** VTR   - Vertical flux of concentration
! *** IC    - Index of compound
! *** I     - Main grid index in x-direction
! *** J     - Main grid index in y-direction
! *** K     - Main grid index in z-direction

!MSK *** Indices of C and BC has been shifted by +1 in x and y direction

! *** Calculate derivatives in vertical layer 1 (lowermost layer):

      do J = 1,NY
        do I = 1,NX

! ***     The divergence determines the direction of mass flux, 
! ***     and it has been multiplied by the grid height in order
! ***     to give the change in vertical speed.

          VTR = DZDT(I,J,1)*(C(IC,I+1,J+1,1) - C(IC,I+1,J+1,2)) +   &
               MAX(W(I,J,1),DBLE(0.))*C(IC,I+1,J+1,1) +   &
                MIN(W(I,J,1),DBLE(0.))*C(IC,I+1,J+1,2)

          DCDT(IC,I,J,1) = DCDT(IC,I,J,1) - (VTR/DZ(1))

!_LHS_September_2004_Start:
!
! ***  If loss by dry-deposition is to be included:
!
!         DCDT(IC,I,J,1) =  DCDT(IC,I,J,1) 
!    &                    - ( DRYDEP_VEL(IC,I,J) * C(IC,I,J,1) )
!    &                    - (VTR/DZ(1))
!
! ***  The first term on the right hand side is the emissions,
! ***  the second is the dry deposition loss (C should be reduced
! ***  to the value at the height where the deposition velocity
! ***  is valid) and the third is the advective and turbulent flux
! ***  between layer 1 and layer two.
!     
!_LHS_September_2004_End. 

          EXCZ(IC,I,J) = VTR

        enddo
      enddo

! *** Calculate derivatives in vertical layers 2 to NZ - 1:

      do K = 2,NZM1
        do J = 1,NY
          do I = 1,NX

            VTR = DZDT(I,J,K)*(C(IC,I+1,J+1,K) - C(IC,I+1,J+1,K+1)) +   &
                 MAX(W(I,J,K),DBLE(0.))*C(IC,I+1,J+1,K) +   &
                 MIN(W(I,J,K),DBLE(0.))*C(IC,I+1,J+1,K+1)

            DCDT(IC,I,J,K) = DCDT(IC,I,J,K) -   &
                           (VTR - EXCZ(IC,I,J))/DZ(K)
            EXCZ(IC,I,J) = VTR

          enddo
        enddo
      enddo

! *** Calculate derivatives in vertical layer NZ (uppermost layer):

      do J = 1,NY
        do I = 1,NX


!_NEST_Start:

         VTR = DZDT(I,J,NZ) * (C(IC,I+1,J+1,NZ) - BC(IC,I+1,J+1,NZ+1))    &
            + MAX(W(I,J,NZ),DBLE(0.)) * C(IC,I+1,J+1,NZ)    &
            + MIN(W(I,J,NZ),DBLE(0.)) * BC(IC,I+1,J+1,NZ+1)

!_NEST_End.

          DCDT(IC,I,J,NZ) = DCDT(IC,I,J,NZ) -   &
                           (VTR - EXCZ(IC,I,J))/DZ(NZ)

        enddo
      enddo

! *** Integrate concentrations:

      do K = 1,NZ
        do J = 1,NY
          do I = 1,NX

            C(IC,I+1,J+1,K) = C(IC,I+1,J+1,K) + DCDT(IC,I,J,K)*DT

! ***       If concentrations are negative then write a message:

            IF (C(IC,I+1,J+1,K) .LT. -1.E-37 .AND. IC .NE. 39) THEN

              IF (MESSFE) WRITE (MESSUN,1000)   &
             ITS,IC,I,J,K,C(IC,I+1,J+1,K),DCDT(IC,I,J,K)

! ***         Set negative concentrations to zero:

              IF (C(IC,I+1,J+1,K) .LT. 0.) C(IC,I+1,J+1,K) = 0.

            ENDIF
              
          enddo
        enddo
      enddo

      RETURN

 1000 format ('ADIV: Timestep = ',I3,' IC = ',I2,' I = ',I3,' J = ',   &
             I3,' K = ',I2,1P,' C(IC,I,J,K) = ',E12.4,   &
             ' DCDT(IC,I,J,K) = ',E12.4)

! *** End of subroutine ADIV

      end subroutine adiv
