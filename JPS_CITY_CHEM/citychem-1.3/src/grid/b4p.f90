! <b4p.f90 - A component of the City-scale
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

      subroutine B4P(V,COUR,M,N,FM,FP,W)

! The subroutine calculates 1D advection using the area preserving and
! positive definite 4th degree flux-form version of Botts scheme.

! Ref: Bott, A. (1989), A positive definite advection scheme obtained by
! nonlinear renormalization of the advective fluxes, Monthly Weather
! Review 117, pp. 1006-1015.
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
!           2016  M. Karl: Cutoffs for flux variables to avoid too small numbers
!
! ----------------------------------------------------------------------------------

      integer :: M
      integer :: N

      real :: V(M)
      real :: COUR(M)
      real :: FM(M)
      real :: FP(M)
      real :: W(M)


! M     - Array dimensions as declared in the calling program
! N     - Number of gridcells including the two boundary cells
! V     - Concentration values, input and output
!         Boundary conditions: V(1) = const, V(N) = const
! COUR  - Courant numbers satisfying the CFL criterion
! FM,FP - Fluxes for u(i) < 0 and u(i) > 0, respectively
! W     - The positive definite flux limiters

! Local variables

      real :: A0
      real :: A1
      real :: A2
      real :: A3
      real :: A4
      real :: CL
      real :: CR
      real :: X1
      real :: X2
      real :: X3

      integer :: I

! A0-4  - Coefficients of polynomials in gridcells
!       - At i = 1 and i = n     first  order polynomials are used
!       - At i = 2 and i = n - 1 second order polynomials are used
!       - At 3 <= i <= n-2 fourth order polynomials are used
! CL,CR - Courant numbers
! X1-3  - Dummy values
! I     - Index of grid cell

! The quantities COUR(I), FM(I) and FP(I) are given at the right
! boundary of grid cell number I. Thus, FM(I) is flux from gridcell I+1
! into gridcell I for COUR(I) < 0, FP(I) is flux from gridcell I into
! gridcell I+1 for COUR(I) > 0.

! Calculate fluxes

      CR = MAX(0.,COUR(1))

!MSK start
! V is concentration value
      if ( V(1)   .lt. 1.e-28)  V(1)   = 0.0
      if ( V(2)   .lt. 1.e-28)  V(2)   = 0.0
!MSK end

      FP(1) = MIN(V(1),CR*(V(1) + (1. - CR)*(V(2) - V(1))*0.5))
      W(1) = 1.

      A0 = (26.*V(2) - V(3) - V(1))/24.
      A1 = (V(3) - V(1))/16.
      A2 = (V(3) + V(1) - 2.*V(2))/48.
      CL = -MIN(0.,COUR(1))
      X1 = 1. - 2.*CL
      X2 = X1*X1

!MSK start check A0,A1,A2,X1,X2,CL
      if (A0 .gt. 0.)  A0 = MAX(1.e-28,A0)
      if (A1 .gt. 0.)  A1 = MAX(1.e-28,A1)
      if (A2 .gt. 0.)  A2 = MAX(1.e-28,A2)
      if (X1 .gt. 0.)  X1 = MAX(1.e-28,X1)
      if (X2 .gt. 0.)  X2 = MAX(1.e-28,X2)
      if (CL .gt. 0.)  CL = MAX(1.e-28,CL)
      if (A0 .lt. 0.)  A0 = MIN(-1.e-28,A0)
      if (A1 .lt. 0.)  A1 = MIN(-1.e-28,A1)
      if (A2 .lt. 0.)  A2 = MIN(-1.e-28,A2)
      if (X1 .lt. 0.)  X1 = MIN(-1.e-28,X1)
      if (X2 .lt. 0.)  X2 = MIN(-1.e-28,X2)
      if (CL .lt. 0.)  CL = MIN(-1.e-28,CL)
!MSK end

      FM(1) = MAX(0.,A0*CL - A1*(1. - X2) + A2*(1. - X1*X2))

      CR = MAX(0.,COUR(2))
      X1 = 1. - 2.*CR
      X2 = X1*X1

      FP(2) = MAX(0.,A0*CR + A1*(1. - X2) + A2*(1. - X1*X2))

      IF (FM(1) + FP(2) .GT. V(2)) THEN
          W(2) = V(2)/(FM(1) + FP(2))
      ELSE
          W(2) = 1.
      ENDIF

      DO 100 I = 3,N-2

!MSK start
! V is concentration value
      if ( V(I)   .lt. 1.e-28)  V(I)   = 0.0
      if ( V(I-1) .lt. 1.e-28)  V(I-1) = 0.0
      if ( V(I+1) .lt. 1.e-28)  V(I+1) = 0.0
      if ( V(I-2) .lt. 1.e-28)  V(I-2) = 0.0
      if ( V(I+2) .lt. 1.e-28)  V(I+2) = 0.0
!MSK end

      A0 = (9.*(V(I+2) + V(I-2)) - 116.*(V(I+1) + V(I-1)) +  &
           2134.*V(I))/1920.
      A1 = (-5.*(V(I+2) - V(I-2)) + 34.*(V(I+1) - V(I-1)))/384.
      A2 = (-V(I+2) + 12.*(V(I+1) + V(I-1)) - 22.*V(I) - V(I-2))/384.
      A3 = (V(I+2) - 2.*(V(I+1) - V(I-1)) - V(I-2))/768.
      A4 = (V(I+2) - 4.*(V(I+1) + V(I-1)) + 6.*V(I) + V(I-2))/3840.
      CL = -MIN(0.,COUR(I-1))
      X1 = 1. - 2.*CL
      X2 = X1*X1
      X3 = X1*X2

!MSK start check A0,A1,A2,A3,A4,CL
      if (A0 .gt. 0.)  A0 = MAX(1.e-28,A0)
      if (A1 .gt. 0.)  A1 = MAX(1.e-28,A1)
      if (A2 .gt. 0.)  A2 = MAX(1.e-28,A2)
      if (A3 .gt. 0.)  A3 = MAX(1.e-28,A3)
      if (A4 .gt. 0.)  A4 = MAX(1.e-28,A4)
      if (CL .gt. 0.)  CL = MAX(1.e-28,CL)
      if (A0 .lt. 0.)  A0 = MIN(-1.e-28,A0)
      if (A1 .lt. 0.)  A1 = MIN(-1.e-28,A1)
      if (A2 .lt. 0.)  A2 = MIN(-1.e-28,A2)
      if (A3 .lt. 0.)  A3 = MIN(-1.e-28,A3)
      if (A4 .lt. 0.)  A4 = MIN(-1.e-28,A4)
      if (CL .lt. 0.)  CL = MIN(-1.e-28,CL)

      if (A0 .gt. 0. .and. A0 .lt. 1.e-27)  A0 = 0.
      if (A1 .gt. 0. .and. A1 .lt. 1.e-27)  A1 = 0.
      if (A2 .gt. 0. .and. A2 .lt. 1.e-27)  A2 = 0.
      if (A3 .gt. 0. .and. A3 .lt. 1.e-27)  A3 = 0.
      if (A4 .gt. 0. .and. A4 .lt. 1.e-27)  A4 = 0.

      if (A0 .lt. 0. .and. A0 .gt. -1.e-27)  A0 = 0.
      if (A1 .lt. 0. .and. A1 .gt. -1.e-27)  A1 = 0.
      if (A2 .lt. 0. .and. A2 .gt. -1.e-27)  A2 = 0.
      if (A3 .lt. 0. .and. A3 .gt. -1.e-27)  A3 = 0.
      if (A4 .lt. 0. .and. A4 .gt. -1.e-27)  A4 = 0.

!MSK end

      FM(I-1) = MAX(0.,A0*CL - A1*(1. - X2) + A2*(1. - X3)  &
                      -A3*(1.-X1*X3) + A4*(1. - X2*X3))


      CR = MAX(0.,COUR(I))
      X1 = 1. - 2.*CR
      X2 = X1*X1
      X3 = X1*X2

!MSK start check CR,X1,X2,X3
      if (CR .gt. 0.)  CR = MAX(1.e-28,CR)
      if (X1 .gt. 0.)  X1 = MAX(1.e-28,X1)
      if (X2 .gt. 0.)  X2 = MAX(1.e-28,X2)
      if (X3 .gt. 0.)  X3 = MAX(1.e-28,X3)
      if (CR .lt. 0.)  CR = MIN(-1.e-28,CR)
      if (X1 .lt. 0.)  X1 = MIN(-1.e-28,X1)
      if (X2 .lt. 0.)  X2 = MIN(-1.e-28,X2)
      if (X3 .lt. 0.)  X3 = MIN(-1.e-28,X3)
!MSK end

      FP(I) = MAX(0.,A0*CR + A1*(1. - X2) + A2*(1. - X3)  &
                    + A3*(1. - X1*X3) + A4*(1. - X2*X3))

!MSK start check FM,FP (>0)
      if ( FM(I-1) .lt. 1.e-27)  FM(I-1) = 0.0
      if ( FP(I)   .lt. 1.e-27)  FP(I)   = 0.0
!MSK end

      IF (FM(I-1) + FP(I) .GT. V(I)) THEN
!MSK          W(I) = V(I)/(FM(I-1) + FP(I))
!MSK more secure:
          W(I) = V(I)/(MAX(1.e-15,(FM(I-1) + FP(I))))
      ELSE
          W(I) = 1.
      ENDIF

  100 CONTINUE

      A0 = (26.*V(N-1) - V(N) - V(N-2))/24.
      A1 = (V(N) - V(N-2))/16.
      A2 = (V(N) + V(N-2) - 2.*V(N-1))/48.
      CL = -MIN(0.,COUR(N-2))
      X1 = 1. - 2.*CL
      X2 = X1*X1

!MSK start check A0,A1,A2,CL
      if (A0 .gt. 0.)  A0 = MAX(1.e-28,A0)
      if (A1 .gt. 0.)  A1 = MAX(1.e-28,A1)
      if (A2 .gt. 0.)  A2 = MAX(1.e-28,A2)
      if (CL .gt. 0.)  CL = MAX(1.e-28,CL)
      if (A0 .lt. 0.)  A0 = MIN(-1.e-28,A0)
      if (A1 .lt. 0.)  A1 = MIN(-1.e-28,A1)
      if (A2 .lt. 0.)  A2 = MIN(-1.e-28,A2)
      if (CL .lt. 0.)  CL = MIN(-1.e-28,CL)
!MSK end

      FM(N-2) = MAX(0.,A0*CL - A1*(1. - X2) + A2*(1. - X1*X2))

      CR = MAX(0.,COUR(N-1))

!MSK start check CR
      if (CR .gt. 0.)  CR = MAX(1.e-28,CR)
      if (CR .lt. 0.)  CR = MIN(-1.e-28,CR)
!MSK end

      X1 = 1. - 2.*CR
      X2 = X1*X1

      FP(N-1) = MAX(0.,A0*CR + A1*(1. - X2) + A2*(1. - X1*X2))

!MSK start check FM,FP (>0)
      if ( FM(N-2) .lt. 1.e-27)  FM(N-2) = 0.0
      if ( FP(N-1) .lt. 1.e-27)  FP(N-1) = 0.0
!MSK end

      IF (FM(N-2) + FP(N-1) .GT. V(N-1)) THEN
!MSK          W(N-1) = V(N-1)/(FM(N-2) + FP(N-1))
!MSK more secure:
          W(N-1) = V(N-1)/(MAX(1.e-15,(FM(N-2) + FP(N-1))))
      ELSE
          W(N-1) = 1.
      ENDIF

      CL = -MIN(0.,COUR(N-1))

!MSK start check CL
      if (CL .gt. 0.)  CL = MAX(1.e-28,CL)
      if (CL .lt. 0.)  CL = MIN(-1.e-28,CL)

! V is concentration value
      if ( V(N)   .lt. 1.e-28)  V(N)   = 0.0
      if ( V(N-1) .lt. 1.e-28)  V(N-1) = 0.0
!MSK end

      FM(N-1) = MIN(V(N),CL*(V(N) - (1. - CL)*(V(N) - V(N-1))*0.5))
      W(N  ) = 1.

! Perform actual advection

      DO 110 I = 2,N-1
!MSK start some checks
! V is concentration value
          if (  V(I) .lt. 1.e-20)   V(I) = 0.0
          if ( FM(I) .lt. 1.e-20)  FM(I) = 0.0
          if ( FP(I) .lt. 1.e-20)  FP(I) = 0.0
!MSK end
! debug
          !print *,'b4p: I',I,V(I),FM(I-1),FP(I),W(I),FM(I),W(I+1),FP(I-1),W(I-1)
          V(I) = V(I) - (FM(I-1) + FP(I))*W(I) + FM(I)*W(I+1) +  &
                FP(I-1)*W(I-1)
          V(I) = MAX(0.,V(I))
  110 CONTINUE

      RETURN

! End of subroutine B4P

      end subroutine b4p
