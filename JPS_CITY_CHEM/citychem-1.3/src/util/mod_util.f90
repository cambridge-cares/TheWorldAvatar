! <mod_util.f90 - A component of the City-scale
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

      module mod_util

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
!           2017  M. Karl: copied here the functions: EXP4, XLINE, YLINE (from hwylne.f90).
!                          Added functions func_canyon, qromb, trapzd, polint
!                          from Numerical Recipes
!
! ----------------------------------------------------------------------------------

      double precision :: avogad
      real :: busi
      real :: ctok
      real :: g
      real :: gamma
      real :: kappa
      real :: miss
      double precision :: pi
      real :: rad
      real :: rgas_j
      real :: charn

      parameter (avogad = 6.022045e23)
      parameter (busi   = 4.7)
      parameter (ctok   = 273.15)
      parameter (g      = 9.80665)
      parameter (gamma  = 0.27)
      parameter (kappa  = 0.41)
      parameter (miss   = -9900.)
      parameter (pi     = 3.141592654)
      parameter (rad    = pi/180.)
      parameter (rgas_j = 8.31441)
!MSK start
      parameter (charn  = 0.013)
!MSK end

!     avogad - Avogadros constant
!     busi   - Businger's surface layer theory constant
!     ctok   - Conversion from degrees Celcius to degrees Kelvin
!     g      - Acceleration of gravity (m/s^2)
!     gamma  - Venkatram constant (1984)
!     kappa  - Von Karman's constant
!     miss   - Missing data value
!     pi     - The mathematical constant pi
!     rad    - Conversion from degrees to radians
!     rgas_j - The molar gas constant (J/(mol*K))
!            - When pressure is measured in hPa = mb then rgas_j must be
!            - multiplied by 10^-2 since 1 Pa = 1 N/m^2
!     charn  - Charnock constant = 0.013; Units : none

      contains


!MSK copied function EXP4 into mod_util from EXP4.for
!   ******************************************************************

      REAL FUNCTION EXP4(X)

! The function computes exp(x) for all ranges of x

! Scalar arguments
      
      REAL X

! Parameters

      REAL XMIN,XMAX
      PARAMETER (XMIN = -87.3,XMAX = 88.7)

! Calculate exp(x)
      
      IF (X .GT. XMIN .AND. X .LT. XMAX) THEN
          EXP4 = EXP(X)
      ELSEIF (X .LE. XMIN) THEN
          EXP4 = 0.
      ELSE
          EXP4 = EXP(XMAX)
      ENDIF

      RETURN

! End of real function EXP4
      
      END FUNCTION EXP4
!   ******************************************************************

! MSK functions used in line source module hwylne.f90
!   ******************************************************************
!
! *** MODIFIED OCT. 1979 TO ADD ROMBERG INTEGRATION ENHANCEMENTS.
! *** MODIFIED SEP. 1989 TO IMPROVE LOCATION SEARCH
! *** XLINE is UPWIND DISTANCE OF R,S FROM RREC,SREC

      REAL FUNCTION XLINE(R,S,SINT,COST,RREC,SREC)

      REAL R,S,SINT,COST,RREC,SREC

!   -----------------------------------------------------------------
!
      XLINE = (R-RREC) *SINT + (S-SREC) * COST

      RETURN
      END FUNCTION XLINE

!   ******************************************************************
!
! *** MODIFIED OCT. 1979 TO ADD ROMBERG INTEGRATION ENHANCEMENTS.
! *** MODIFIED SEP. 1989 TO IMPROVE LOCATION SEARCH
! *** YLINE is CROSSWIND DISTANCE OF R,S FROM RREC,SREC

      REAL FUNCTION YLINE(R,S,SINT,COST,RREC,SREC)

      REAL R,S,SINT,COST,RREC,SREC

!   -----------------------------------------------------------------
!
      YLINE = (S-SREC) * SINT - (R-RREC) * COST

      RETURN
      END FUNCTION YLINE

!   ******************************************************************

!MSK functions used in line source module csubl.f90

!   ******************************************************************
!
! *** STREET CANYON
! *** Function in integral(a,b)=func(x)dx
! *** The function is not used because the integrals in OSPM can be
! *** solved analytically.

      real function func_canyon(x,const)

      REAL x,const

!   -----------------------------------------------------------------
!
      func_canyon = 1.0 / (x + const)

      return
      end function func_canyon


!   ******************************************************************

!MSK numerical recipe subroutines

      subroutine qromb(const, a, b, ssv)
    !----------------------------------------------------------------------
    !
    !****
    !
    !      purpose
    !      -------
    !      Romberg Integration of order 2K using polynomial extrapolation
    !
    !      interface
    !      ---------
    !
    !
    !      method
    !      ------
    !      NUMERICAL RECIPES
    !      Returns as ssv the integratl of the function func from a to b.
    !      Integration is performed by Romberg's method of order 2K, where
    !      e.g. K=2 is Simpson's rule.
    !      Parameters:
    !      EPS is the fractional accuracy desired, as determined by the
    !      extrapolation error estimate;
    !      JMAX is the total number of steps
    !      K is the number of points used in the extrapolation
    !      Uses:
    !      subroutine polint
    !      subroutine trapzd
    !
    !      external
    !      --------
    !      none
    !
    !      reference
    !      ---------
    !      Book Numerical Recipes, Ch. 4.3
    !
    !
    !------------------------------------------------------------------
    implicit none

    real, intent(in)                     :: a,b
    real, intent(in)                     :: const
    real, intent(out)                    :: ssv

    real, parameter                      :: EPS   = 1.e-6
    integer, parameter                   :: JMAX  = 20
    integer, parameter                   :: JMAXP = JMAX+1
    integer, parameter                   :: K     = 5
    integer, parameter                   :: KM    = K-1

    real, dimension(JMAXP)               :: h
    real, dimension(JMAXP)               :: s
    integer                              :: j
    real                                 :: dss

    ! Arrays h and s store the succesive trapezoidal approximation
    ! and their relative step sizes.

          h(1) = 1
          !print *,'qromb const',const
          do j=1, JMAX
            call trapzd(const,a,b,s(j),j)
            if (j.ge.K) then

    ! Call polint(xa,ya,PMAX,x, y,dy), polynomial extrapolation
              call polint(h(j-KM),s(j-KM),K,0., ssv,dss)

              if (ABS(dss).le.EPS*ABS(ssv)) return

            endif
            s(j+1) = s(j)
    ! This is a key step: The factor is 0.25 even though the step size
    ! is decreased by only 0.5. This makes the extrapolation a polynomial
    ! in h^2 as allowed in equation (4.2.1) [in NR], not just a polynomial in h.
            h(j+1) = 0.25 * h(j)
          enddo
    ! Should not get here, just for testing
          print *,'Warning: Too many steps in qromb (mod_util)'


      end subroutine qromb

      subroutine trapzd(const, a, b, s, n)
    !----------------------------------------------------------------------
    !
    !****
    !
    !      purpose
    !      -------
    !      Extended trapezoidal rule
    !
    !      interface
    !      ---------
    !
    !
    !      method
    !      ------
    !      NUMERICAL RECIPES
    !      Routine computes the nth stage of refinement of an extended
    !      trapezoidal rule.
    !      Input func is the name of the function to be integrated with
    !      limits a and b, also input. When called with n=1 the routine
    !      returns as s, the crudest estimate of integral(a,b)f(x)dx.
    !      Subsequent calls with n=2, 3,... (in that sequential order) will
    !      improve the accuracy of s, by adding 2^(n-2) additional interior
    !      points. s should NOT be modified between the subsequent calls
    !
    !      external
    !      --------
    !      none
    !
    !      reference
    !      ---------
    !      Book Numerical Recipes, Ch. 4.2
    !
    !
    !------------------------------------------------------------------
    implicit none

    real, intent(in)                     :: a,b
    real, intent(in)                     :: const
    integer, intent(in)                  :: n
    real, intent(out)                    :: s

    integer                              :: it
    integer                              :: j
    real                                 :: del
    real                                 :: summe
    real                                 :: tnm
    real                                 :: x

          if (n.eq.1) then
            s = 0.5 * (b-a) * ( func_canyon(a,const) + func_canyon(b,const) )
          else
            it=2**(n-2)
            tnm=REAL(it)
    ! This is the spacing of the points to be added
            del=(b-a)/tnm
            x=a+0.5*del
            summe=0.
            do j = 1, it
              summe=summe+func_canyon(x,const)
              x=x+del
            enddo
    ! This replaces s by its refined value
            s = 0.5 * ( s+(b-a)*summe/tnm )
          endif

          return

      end subroutine trapzd


      subroutine polint(xa,ya,PMAX,x, y,dy)
    !----------------------------------------------------------------------
    !
    !****
    !
    !      purpose
    !      -------
    !      Polynomial Interpolation and Extrapolation
    !
    !      interface
    !      ---------
    !
    !
    !      method
    !      ------
    !      NUMERICAL RECIPES
    !      Given arrays xa and ya, each of length n, and given a value x, 
    !      this routine returns a value y, and an error estimate dy. 
    !      If P(x) is the polynomial of degree N-1 such that
    !      P(xai) = yai, i = 1, ... , n, then the returned value y = P(x).
    !
    !      external
    !      --------
    !      none
    !
    !      reference
    !      ---------
    !      Book Numerical Recipes, Ch. 3.1
    !
    !
    !------------------------------------------------------------------
    implicit none

    REAL, intent(in)                     :: x 
    integer, intent(in)                  :: PMAX

    REAL, dimension(PMAX), intent(in)    :: xa,ya
    REAL, intent(out)                    :: y
    REAL, intent(out)                    :: dy

    REAL                                 :: den,dif,dift,ho,hp,w

    integer, parameter                   :: NMAX=10
    real, dimension(NMAX)                :: c
    real, dimension(NMAX)                :: d
    integer                              :: i,m,ns

          ns=1
          dif=abs(x-xa(1))
          
          ! Here we find the index ns of the closest table entry
          do i=1,PMAX
            dift=abs(x-xa(i))
            if (dift .lt. dif) then
              ns=i
              dif=dift
            endif
            ! initialize the tableau of c's and d's.
            c(i)=ya(i) 
            d(i)=ya(i)
          end do
          
          ! This is the initial approximation to y.
          y=ya(ns)
          ns=ns-1
          
          ! For each column of the tableau
          ! we loop over the current c's and d's and update them.
          ! After each column in the tableau is completed, we decide
          ! which correction, c or d, we want to add to our accumulating           CRV(IC) = CSUM
          ! value of y, i.e., which path to take through
          ! the tableau forking up or down. We do this in such a
          ! way as to take the most "straight line" route through the
          ! tableau to its apex, updating ns accordingly to keep track
          ! of where we are. This route keeps the partial approximations
          ! centered (insofar as possible) on the target x.
          ! The last dy added is thus the error indication.
          do m=1,PMAX-1 
            do i=1,PMAX-m 
              ho=xa(i)-x
              hp=xa(i+m)-x
              w=c(i+1)-d(i)
              den=ho-hp
              ! if(den.eq.0.)pause 'failure in polint'
              ! This error can occur only if two input xa's are (to within roundoff) identical.
              if (den .eq. 0.0) then
                den= den+1.e-32
              endif
              den=w/den
              d(i)=hp*den 
              c(i)=ho*den
            end do
            if (2*ns .lt. PMAX-m) then
              dy=c(ns+1)
            else
              dy=d(ns)
              ns=ns-1
            endif
            y=y+dy
          end do

      end subroutine polint

!   ******************************************************************

      subroutine FreeUtilMemory()

!DEC$ ATTRIBUTES DLLEXPORT,STDCALL,ALIAS:'FreeUtilMemory' :: FreeUtilMemory

      implicit none

!     End of subroutine FreeUtilMemory
      end subroutine FreeUtilMemory




      end module mod_util
