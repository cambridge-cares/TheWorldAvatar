! <b4m.f90 - A component of the City-scale
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

      subroutine B4M(V,COUR,DF,M,N,M2,FM,FP,FMM,FPP,W,A0,A1,A2,A3,A4)

! The subroutine calculates 1D advection using the area preserving and
! positive definite and monotone 4th degree flux form version of Botts
! scheme.

! Ref: Bott, A. (1992), Monotone flux limitation in the area-preserving
! flux-form advection algorithm, Monthly Weather Review 120, pp. 2592-
! 2602.
!      Bott, A. (1993), The monotone area-preserving flux-form advection
! algorithm: Reducing the time-splitting error in two-dimensional flow
! fields, Monthly Weather Review 121, pp. 2637-2641.
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
      integer :: M2

      real :: V(M)
      real :: COUR(M)
      real :: DF(M)
      real :: FM(M2)
      real :: FP(M2)
      real :: FMM(M2)
      real :: FPP(M2)
      real :: W(M2)
      real :: A0(M2)
      real :: A1(M2)
      real :: A2(M2)
      real :: A3(M2)
      real :: A4(M2)

! M       - Array dimensions in the calling program
! N       - Number of gridcells including the two boundary cells
! M2      - Array dimensions in the calling program
! V       - Concentration values, input and output
!           Boundary conditions: V(1) = const, V(N) = const
! COUR    - Courant numbers satisfying the CFL criterion
! DF      - Deformational terms
! FM,FP   - Fluxes for u(i) < 0 and u(i) > 0, respectively
! FMM,FPP - Monotone fluxes for u(i) < 0 and u(i) > 0, respectively
! W       - The positive definite flux limiters
! A0-4    - Coefficients of polynomials in gridcells
!         - At i = 1 and i = n     first  order polynomials are used
!         - At i = 2 and i = n - 1 second order polynomials are used
!         - At 3 <= i <= n-2 fourth order polynomials are used

! Local variables

      real :: CL
      real :: CR
      real :: FMI
      real :: FPI
      real :: VMIN
      real :: VMAX
      real :: X0
      real :: X1
      real :: X2
      real :: X3

      integer :: I

! CL      - Courant number
! CR      - Courant number
! FMI     - Flux minimum value
! FPI     - Flux minimum value
! VMIN    - Minimum value
! VMAX    - Maximum value
! X0-3    - Dummy values
! I       - Dummy index

! The quantities COUR(I), FM(I) and FP(I) are given at the right
! boundary of grid cell number I. Thus, FM(I) is flux from gridcell I+1
! into gridcell I for COUR(I) < 0, FP(I) is flux from gridcell I into
! gridcell I+1 for COUR(I) > 0.

! Calculate polynomial coefficients

      A0(2) = (26.*V(2) - V(3) - V(1))/24.
      A1(2) = (V(3) - V(1))/16.
      A2(2) = (V(3) + V(1) - 2.*V(2))/48.
      A3(2) = 0.
      A4(2) = 0.

      DO 100 I = 3,N-2

      A0(I) = (9.*(V(I+2) + V(I-2)) - 116.*(V(I+1) + V(I-1)) +  &
              2134.*V(I))/1920.
      A1(I) = (-5.*(V(I+2) - V(I-2)) + 34.*(V(I+1) - V(I-1)))/384.
      A2(I) = (-V(I+2) + 12.*(V(I+1) + V(I-1)) - 22.*V(i) - V(I-2))/384.
      A3(I) = (V(I+2) - 2.*(V(I+1) - V(I-1)) - V(I-2))/768.
      A4(I) = (V(I+2) - 4.*(V(I+1) + V(I-1)) + 6.*V(I) +  &
              V(I-2))/3840.

  100 CONTINUE

      A0(N-1) = (26.*V(N-1) - V(N) - V(N-2))/24.
      A1(N-1) = (V(N) - V(N-2))/16.
      A2(N-1) = (V(N) + V(N-2) - 2.*V(N-1))/48.
      A3(N-1) = 0.
      A4(N-1) = 0.

! Calculate fluxes

      CL = -MIN(0.,COUR(N-1))

       FM(N-1) = MIN(V(N),CL*(V(N) - (1. - CL)*(V(N) - V(N-1))*0.5))
      FMM(N-1) = FM(N-1)

      DO 120 I = N-1,2,-1

      CL = -MIN(0.,COUR(I))
      X1 = 1.-2.*CL
      X2 = X1*X1
      X3 = X1*X2

      VMIN = MIN(V(I),V(I+1))
      VMAX = MAX(V(I),V(I+1))

      FMI = MAX(0., A0(I)*CL - A1(I)*(1. - X2) +   &
                   A2(I)*(1. - X3) - A3(I)*(1. - X1*X3) +  &
                   A4(I)*(1. - X2*X3))

!MSK start check
      if ( FMI .gt. 0.)        FMI  = MAX(1.e-28, FMI )
      if ( V(I) .gt. 0.)       V(I) = MAX(1.e-28, V(I) )
      if ( FM(I) .gt. 0.)     FM(I) = MAX(1.e-28, FM(I) )
      if ( VMIN .gt. 0.)       VMIN = MAX(1.e-28, VMIN )

      if ( V(I) .lt. 0.)       V(I) = MIN(-1.e-28, V(I) )
      if ( FM(I) .lt. 0.)     FM(I) = MIN(-1.e-28, FM(I) )
      if ( VMIN .lt. 0.)       VMIN = MIN(-1.e-28, VMIN )
!MSK end

      FMI = MIN(FMI,V(I) - VMIN + FM(I))

      FMM(I-1) = MAX(FMI,V(I) - VMAX + FM(I))

      FM(I-1) = 0.
      IF (COUR(I-1) .LT. 0.)  &
        FM(I-1) = MAX(0.,FMM(I-1) - (CL + COUR(I-1))*V(I))

  120 CONTINUE

      CR = MAX(0.,COUR(1))
      FP(1) = MIN(V(1),CR*(V(1) + (1. - CR)*(V(2) - V(1))*0.5))
      FPP(1) = FP(1)

      DO 130 I = 2,N-1

      CR = MAX(0.,COUR(I-1))
      X1 = 1. - 2.*CR
      X2 = X1*X1
      X3 = X1*X2

      VMIN = MIN(V(I-1),V(I))
      VMAX = MAX(V(I-1),V(I))

      FPI = MAX(0.,A0(I)*CR + A1(I)*(1. - X2) + A2(I)*(1. - X3) +  &
                  A3(I)*(1. - X1*X3) + A4(I)*(1. - X2*X3))
      FPI = MIN(FPI,V(I) - VMIN + FP(I-1))

      FPP(I) = MAX(FPI,V(I) - VMAX + FP(I-1))

      FP(I) = 0.

!MSK start check
      if ( FPP(I) .gt. 0.)   FPP(I) = MAX(1.e-28, FPP(I) )
      if ( CR .gt. 0.)           CR = MAX(1.e-28, CR )
      if ( COUR(I) .gt. 0.) COUR(I) = MAX(1.e-28, COUR(I) )
      if ( V(I) .gt. 0.)       V(I) = MAX(1.e-28, V(I) )

      if ( FPP(I) .lt. 0.)   FPP(I) = MIN(-1.e-28, FPP(I) )
      if ( COUR(I) .lt. 0.) COUR(I) = MIN(-1.e-28, COUR(I) )
      if ( V(I) .lt. 0.)       V(I) = MIN(-1.e-28, V(I) )
!MSK end

      IF (COUR(I) .GT. 0.) FP(I) = MAX(0.,FPP(I) - (CR - COUR(I))*V(I))

  130 CONTINUE

      DO 140 I = 2,N-1

       V(I) = V(I) + DF(I)

       IF (FMM(I-1) + FPP(I) .GT. V(I)) THEN
           X0 = V(I)/(FMM(I-1) + FPP(I))
       ELSE
           X0 = 1.
       ENDIF

       IF (FM(I-1) + FP(I) .GT. V(I)) THEN
           W(I) = V(I)/(FM(I-1) + FP(I))
       ELSE
           W(I) = 1.
       ENDIF

       V(I) = V(I) - (FMM(I-1) + FPP(I))*X0
      DF(I) = X0*(FMM(I-1) + FPP(I)) - (FM(I-1) + FP(I))*W(I)

  140 CONTINUE

      W(1) = W(2  )
      W(N) = W(N-1)

! Perform the actual advection

      DO 150 I = 2,N-1

!MSK start check
         if ( V(I) .gt. 0.)       V(I) = MAX(1.e-28, V(I) )
         if ( FM(I) .gt. 0.)     FM(I) = MAX(1.e-28, FM(I) )
         if ( W(I+1) .gt. 0.)   W(I+1) = MAX(1.e-28, W(I+1) )
         if ( W(I-1) .gt. 0.)   W(I-1) = MAX(1.e-28, W(I-1) )
         if ( FP(I-1) .gt. 0.) FP(I-1) = MAX(1.e-28, FP(I-1) )
         if ( DF(I) .gt. 0.)     DF(I) = MAX(1.e-28, DF(I) )

         if ( V(I) .lt. 0.)       V(I) = MIN(-1.e-28, V(I) )
         if ( FM(I) .lt. 0.)     FM(I) = MIN(-1.e-28, FM(I) )
         if ( W(I+1) .lt. 0.)   W(I+1) = MIN(-1.e-28, W(I+1) )
         if ( W(I-1) .lt. 0.)   W(I-1) = MIN(-1.e-28, W(I-1) )
         if ( FP(I-1) .lt. 0.) FP(I-1) = MIN(-1.e-28, FP(I-1) )
         if ( DF(I) .lt. 0.)     DF(I) = MIN(-1.e-28, DF(I) )
!MSK end

         V(I) = V(I) + FM(I)*W(I+1) + FP(I-1)*W(I-1)
         V(I) = MAX(0.,V(I))
         DF(I) = MAX(DF(I),-V(I))
  150 CONTINUE

      RETURN

! End of subroutine B4M

      end subroutine b4m
