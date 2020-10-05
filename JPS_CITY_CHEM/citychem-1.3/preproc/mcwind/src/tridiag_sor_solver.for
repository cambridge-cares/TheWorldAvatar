! <tridiag_sor_solver.for - A component of the City-scale
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
!* Walker, S.-E., Solberg, S., Denby, B. (2003) Development and implementation of a simplified 
!*    EMEP photochemistry scheme for urban areas in EPISODE. NILU TR 13/2013. 
!*    ISBN 82-425-1524-7
!*
!* ========================================== 
!*
!**********************************************************************

      subroutine tridiag_sor_solver

! This routine solves the Poisson-equation, using an iterative SOR-
! solver, where each iteration is applied on a vertical grid coloumn
! which is first solved using the Gaussian tridiagonal algorithm.

! Modifications:
!    29 Jan 2019  M. Karl: replaced real*8 by double precision
!    29 Jan 2019  M. Karl: replaced TAB by spaces
!
!**********************************************************************

! Insert the necessary MODULE calls:

      use module_mc_wind_files
      use module_mc_wind_domain
      use module_mc_wind_adjust

      use module_mc_wind_winds

! We require the use of IMPLICIT NONE:

      implicit none

! Local declarations:

      integer :: i,j,k
      integer :: iter
!	real*8    :: term1,term2
      double precision    :: t1,t2,t3
      double precision    :: relax_par
      double precision    :: tol,rel_tol
      double precision    :: prev,max_iteration_change,max_rel_iteration_change

      logical :: not_reached_tol

      double precision, allocatable :: alf(:)
      double precision, allocatable :: bet(:)
      double precision, allocatable :: gam(:)
      double precision, allocatable :: brh(:)

      double precision, allocatable :: del(:)
      double precision, allocatable :: cek(:)
      double precision, allocatable :: mek(:)
      double precision, allocatable :: lam(:)

      double precision, allocatable :: iteration_change(:,:,:)
      double precision, allocatable :: rel_iteration_change(:,:,:)

! Allocate the required internal memory:

      if (.not. allocated (alf))   allocate (alf(km))
      if (.not. allocated (bet))   allocate (bet(km))
      if (.not. allocated (gam))   allocate (gam(km))
      if (.not. allocated (brh))   allocate (brh(km))

      if (.not. allocated (del))   allocate (del(km))
      if (.not. allocated (cek))   allocate (cek(km))
      if (.not. allocated (mek))   allocate (mek(km))
      if (.not. allocated (lam))   allocate (lam(km))

      if (.not. allocated (iteration_change)) &
    allocate (iteration_change(im,jm,km))
      if (.not. allocated (rel_iteration_change)) &
    allocate (rel_iteration_change(im,jm,km))

! Content of the routine:

! Applied "constants" in the iteration procedure:

        t1 = 1.0/(dx*dx)
        t2 = 1.0/(dy*dy)
        t3 = 2.0*(t1 + t2)

! Heading for the iteration information written to the log-file:

      if (fe_log) then
        write(funit_log,*)
        write(funit_log,*)
        write(funit_log,*) ' Iteration information for the adjustment:'
        write(funit_log,*)
        write(funit_log,'(1X,A16,3A4,A16,3A4)') &
         ' MAX ABS CHANGE ','  I ','  J ','  K ', &
         ' MAX REL CHANGE ','  I ','  J ','  K '
      end if

! During the first "lim_iter" iterations the values of:
!     the relaxation-parameter,     "relax_par",
!     the absolute error tolerance, "tol"
! and the relative error tolerance, "rel_tol"
! are applied as specified in the user-input list:

      relax_par = rel_par
      tol       = tolerance
      rel_tol   = rel_tolerance
      not_reached_tol = .TRUE.

! ======================================================================
! Start iteration procedure:

      do iter = 1,max_iter

! Go through all of the internal horizontal grid points (1:im,1:jm).

        do j = 1,jm
          do i = 1,im

! Calculate all the needed coefficients for this (i,j) grid point.
          k = 1

!         --------------------------------------------------------------
!         Version 1:
!
!          alf(k) = (ck(k) + bk(k))*f4(i,j,k)
!     &           + (pk(k) - qk(k))*f1(i,j,k)
!     &           -  t3
!
!          gam(k) =  ak(k)*f4(i,j,k)
!     &           +  rk(k)*f1(i,j,k)
!
!          brh(k) =  g_rhs(i,j,k)
!     &           - (t2 - (bk(k) + ck(k))*f3(i,j,k))*lambda(i,j-1,k)
!     &           - (t1 - (bk(k) + ck(k))*f2(i,j,k))*lambda(i-1,j,k)
!     &           - (t1 + (bk(k) + ck(k))*f2(i,j,k))*lambda(i+1,j,k)
!     &           - (t2 + (bk(k) + ck(k))*f3(i,j,k))*lambda(i,j+1,k)
!     &           +  ak(k)*f3(i,j,k)*lambda(i,j-1,k+1)
!     &           +  ak(k)*f2(i,j,k)*lambda(i-1,j,k+1)
!     &           -  ak(k)*f2(i,j,k)*lambda(i+1,j,k+1)
!     &           -  ak(k)*f3(i,j,k)*lambda(i,j+1,k+1)
!
!
!         --------------------------------------------------------------
!         Version 2:

            alf(k) =  h3(i,j,k) + h8(i,j,k)

            gam(k) =  h13(i,j,k)

            brh(k) =  g_rhs(i,j,k) &
              - (h1(i,j,k) + h6(i,j,k))  * lambda(i,j-1,k) &
              - (h2(i,j,k) + h7(i,j,k))  * lambda(i-1,j,k) &
              - (h4(i,j,k) + h9(i,j,k))  * lambda(i+1,j,k) &
              - (h5(i,j,k) + h10(i,j,k)) * lambda(i,j+1,k) &
              -  h11(i,j,k) * lambda(i,j-1,k+1) &
              -  h12(i,j,k) * lambda(i-1,j,k+1) &
              -  h14(i,j,k) * lambda(i+1,j,k+1) &
              -  h15(i,j,k) * lambda(i,j+1,k+1)

!         --------------------------------------------------------------

! Start Test of specific values:

          if ( ABS(alf(k)) <= ABS(gam(k)) ) then
            write(funit_log,*)  &
        'Not strictly diagonal-dominant in row ',k
            STOP
          end if
!           if ( iter == 1 .AND. i == 20 .AND. j == 16 .AND. k==1) then
!             write(funit_log,*) 'alf(20,16,1) = ',alf(k)
!             write(funit_log,*) 'gam(20,16,1) = ',gam(k)
!             write(funit_log,*) 'brh(20,16,1) = ',brh(k)
!           end if
! End Test of specific values.

          do k = 2,km-1

!         --------------------------------------------------------------
!         Version 1:
!            bet(k) =  ck(k)*f4(i,j,k)
!     &             +  pk(k)*f1(i,j,k)
!
!            alf(k) =  bk(k)*f4(i,j,k)
!     &             -  qk(k)*f1(i,j,k)
!     &             -  t3
!
!            gam(k) =  ak(k)*f4(i,j,k)
!     &             +  rk(k)*f1(i,j,k)
!
!            brh(k) =  g_rhs(i,j,k)
!     &             +  ck(k)*f3(i,j,k)*lambda(i,j-1,k-1)
!     &             +  ck(k)*f2(i,j,k)*lambda(i-1,j,k-1)
!     &             -  ck(k)*f2(i,j,k)*lambda(i+1,j,k-1)
!     &             -  ck(k)*f3(i,j,k)*lambda(i,j+1,k-1)
!     &             - (t2 - bk(k)*f3(i,j,k))*lambda(i,j-1,k)
!     &             - (t1 - bk(k)*f2(i,j,k))*lambda(i-1,j,k)
!     &             - (t1 + bk(k)*f2(i,j,k))*lambda(i+1,j,k)
!     &             - (t2 + bk(k)*f3(i,j,k))*lambda(i,j+1,k)
!     &             +  ak(k)*f3(i,j,k)*lambda(i,j-1,k+1)
!     &             +  ak(k)*f2(i,j,k)*lambda(i-1,j,k+1)
!     &             -  ak(k)*f2(i,j,k)*lambda(i+1,j,k+1)
!     &             -  ak(k)*f3(i,j,k)*lambda(i,j+1,k+1)
!
!
!         --------------------------------------------------------------
!         Version 2:

            bet(k) =  h3(i,j,k)

            alf(k) =  h8(i,j,k)

            gam(k) =  h13(i,j,k)

            brh(k) =  g_rhs(i,j,k) &
               - h1(i,j,k)  * lambda(i,j-1,k-1) &
               - h2(i,j,k)  * lambda(i-1,j,k-1) &
               - h4(i,j,k)  * lambda(i+1,j,k-1) &
               - h5(i,j,k)  * lambda(i,j+1,k-1) &
               - h6(i,j,k)  * lambda(i,j-1,k) &
               - h7(i,j,k)  * lambda(i-1,j,k) &
               - h9(i,j,k)  * lambda(i+1,j,k) &
               - h10(i,j,k) * lambda(i,j+1,k) &
               - h11(i,j,k) * lambda(i,j-1,k+1) &
               - h12(i,j,k) * lambda(i-1,j,k+1) &
               - h14(i,j,k) * lambda(i+1,j,k+1) &
               - h15(i,j,k) * lambda(i,j+1,k+1)

!         --------------------------------------------------------------

            if( ABS(alf(k)) <= (ABS(bet(k)) + ABS(gam(k))) )then
              write(funit_log,*)  &
          'Not strictly diagonal-dominant in row ',k
              STOP
            end if


! Start Test of specific values:
!           if ( iter == 1 .AND. i == 20 .AND. j == 16 .AND. k==2) then
!             write(funit_log,*) 'bet(20,16,2) = ',bet(k)
!             write(funit_log,*) 'alf(20,16,2) = ',alf(k)
!             write(funit_log,*) 'gam(20,16,2) = ',gam(k)
!             write(funit_log,*) 'brh(20,16,2) = ',brh(k)
!           end if
! End Test of specific values.

          end do

          k = km

!         --------------------------------------------------------------
!         Version 1:
!
!          bet(k) =  ck(k)*f4(i,j,k)
!     &           +  pk(k)*f1(i,j,k)
!
!          alf(k) = (ak(k) + bk(k))*f4(i,j,k)
!     &           + (rk(k) - qk(k))*f1(i,j,k)
!     &           -  t3
!
!          brh(k) =  g_rhs(i,j,k)
!     &           +  ck(k)*f3(i,j,k)*lambda(i,j-1,k-1)
!     &           +  ck(k)*f2(i,j,k)*lambda(i-1,j,k-1)
!     &           -  ck(k)*f2(i,j,k)*lambda(i+1,j,k-1)
!     &           -  ck(k)*f3(i,j,k)*lambda(i,j+1,k-1)
!     &           - (t2 - (bk(k) + ak(k))*f3(i,j,k))*lambda(i,j-1,k)
!     &           - (t1 - (bk(k) + ak(k))*f2(i,j,k))*lambda(i-1,j,k)
!     &           - (t1 + (bk(k) + ak(k))*f2(i,j,k))*lambda(i+1,j,k)
!     &           - (t2 + (bk(k) + ak(k))*f3(i,j,k))*lambda(i,j+1,k)
!
!         --------------------------------------------------------------
!         Version 2:

            bet(k) =  h3(i,j,k)

            alf(k) =  h8(i,j,k) + h13(i,j,k)

            brh(k) =  g_rhs(i,j,k) &
              -  h1(i,j,k)  * lambda(i,j-1,k-1) &
              -  h2(i,j,k)  * lambda(i-1,j,k-1) &
              -  h4(i,j,k)  * lambda(i+1,j,k-1) &
              -  h5(i,j,k)  * lambda(i,j+1,k-1) &
              - (h6(i,j,k)  + h11(i,j,k))  * lambda(i,j-1,k) &
              - (h7(i,j,k)  + h12(i,j,k))  * lambda(i-1,j,k) &
              - (h9(i,j,k)  + h14(i,j,k))  * lambda(i+1,j,k) &
              - (h10(i,j,k) + h15(i,j,k))  * lambda(i,j+1,k)

!         --------------------------------------------------------------

            if( ABS(alf(k)) <= ABS(bet(k)) )then
              write(funit_log,*)  &
          'Not strictly diagonal-dominant in row ',k
              STOP
            end if

! Start Test of specific values:
!           if ( iter == 1 .AND. i == 20 .AND. j == 16 .AND. k==km) then
!             write(funit_log,*) 'bet(20,16,km) = ',bet(k)
!             write(funit_log,*) 'alf(20,16,km) = ',alf(k)
!             write(funit_log,*) 'brh(20,16,km) = ',brh(k)
!           end if
! End Test of specific values.

! Now all the coefficients of the TRIDIAGONAL system for the vertical
! coloumn at the grid point (i,j) have been calculated.
! Below a simple tridiagonal solver is implemented. This solver
! assumes that the system is stricktly diagonal dominant.

! Start TRIDIAGONAL SOLVER:

            del(1) = alf(1)
            cek(1) = brh(1)

            do k = 2,km
              mek(k) = bet(k)/del(k-1)
              del(k) = alf(k) - mek(k)*gam(k-1)
              cek(k) = brh(k) - mek(k)*cek(k-1)
            end do

            lam(km) = cek(km)/del(km)
            do k = km-1,1,-1
              lam(k) = (cek(k) - gam(k)*lam(k+1))/del(k)
            end do

! End TRIDIAGONAL SOLVER.

! We have now calculated the new values of lambda(i,j,k) for the
! vertical coloumn (i,j), and these are stored in lam(1:km)

! Start SOR algorithm:

            do k = 1,km
              prev = lambda(i,j,k)
              lambda(i,j,k) = prev + relax_par*(lam(k) - prev)
              iteration_change(i,j,k) = ABS(lambda(i,j,k) - prev)

              if (iteration_change(i,j,k) == 0.0) then
                rel_iteration_change(i,j,k) = 0.0
              else
                if (lambda(i,j,k) == 0.0 ) then
                  rel_iteration_change(i,j,k) =  &
                     iteration_change(i,j,k) / ABS(prev)
                else
                  rel_iteration_change(i,j,k) =  &
                     iteration_change(i,j,k) / ABS(lambda(i,j,k))
                end if
              end if

            end do
! End SOR algorithm.

          end do   ! do i = 1,im
        end do   ! do j = 1,jm

        max_iteration_change     = MAXVAL(iteration_change)
        max_rel_iteration_change = MAXVAL(rel_iteration_change)

! **********************************************************************
!       For test of divergence Calculate the adjusted wind field:
!
!        call calculate_adjusted_wfield
!
!     LHS_TEST_START:
!	bcg = 0.0
!
!      do j = 1,jm
!        do i = 1,im
!          term1 = u(i,j,1) + u(i-1,j,1) - u0(i,j,1) - u0(i-1,j,1)
!		term1 = term1*(depth(i+1,j)-depth(i-1,j))/dx
!          term2 = v(i,j,1) + v(i,j-1,1) - v0(i,j,1) - v0(i,j-1,1)
!		term2 = term2*(depth(i,j+1)-depth(i,j-1))/dy
!          bcg(i,j) = (delta_sigmal(1)*depth(i,j))/(4.0*h0*alfa*alfa)
!          bcg(i,j) = bcg(i,j)*(term1 + term2)
! TEST:         bcg(i,j) = 0.0
!
!          lambda(i,j,km+1) = lambda(i,j,km)
!          lambda(i,j,0)    = lambda(i,j,1) + bcg(i,j)
!
!        end do
!      end do
!
!      do j = 1,jm
!        do i = 1,im
!          g_rhs(i,j,1) = g_rhs(i,j,1)
!     &       + (ck(1)*f3(i,j,1)*bcg(i,j-1))
!     &       + (ck(1)*f2(i,j,1)*bcg(i-1,j))
!     &       - ((pk(1)*f1(i,j,1)+ck(1)*f4(i,j,1))*bcg(i,j))
!     &       - (ck(1)*f2(i,j,1)*bcg(i+1,j))
!     &       - (ck(1)*f3(i,j,1)*bcg(i,j+1))
!        end do
!      end do
!
!        fe_log =.TRUE.
!        if (fe_log) then
!
!       Write time-series for the surface stations:
!
!        if(iter == 1)then
!           Write heading for the time-series file:
!           write(funit_log,'(7X,A12,4(A16,3A4))')
!     &        'Alfa value',
!     &        'Max In. 3Ddiv','I','J','K',
!     &        'Max Fi. 3Ddiv','I','J','K',
!     &        'Min In. 3Ddiv','I','J','K',
!     &        'Min Fi. 3Ddiv','I','J','K'
!        end if
!        write(funit_log,'(1X,I6,F12.5,4(F16.9,3I4))')
!     &      iter,alfa,MAXVAL(div0_3D),MAXLOC(div0_3D),
!     &                MAXVAL(div_3D), MAXLOC(div_3D),
!     &                MINVAL(div0_3D),MINLOC(div0_3D),
!     &                MINVAL(div_3D), MINLOC(div_3D)
!       end if ! (fe_log)
!        fe_log =.FALSE.
!     LHS_TEST_END.
! **********************************************************************
        if (fe_log) then
          write(funit_log,'(F16.7,3I4,F16.7,3I4)') &
         max_iteration_change,MAXLOC(iteration_change), &
         max_rel_iteration_change,MAXLOC(rel_iteration_change)
        end if

        if (max_iteration_change < tol) then

          if(max_rel_iteration_change < rel_tol) then

            not_reached_tol = .FALSE.
! LHS_TEST_START:
!            fe_log =.TRUE.
! LHS_TEST_END.
            if (fe_log) then
              write(funit_log,*)
              write(funit_log,'(1X,A,F8.5,A,I4)')  &
         'The relative tolerance limit ',rel_tol, &
         ' obtained after iteration:',iter
              write(funit_log,'(1X,A,F8.5,A)')  &
         'The absolute tolerance limit',tol,' is obtained as well.'
              write(funit_log,'(1X,A,F8.5)')  &
         'The maximum relative iteration change = ', &
          max_rel_iteration_change
              write(funit_log,'(1X,A,F8.5)')  &
         'The maximum absolute iteration change = ', &
          max_iteration_change
            end if

! The wanted absolute and relative minimum change in the lambda values
! have been reached, and the iteration loop ("do iter = 1,max_iter")
! can be ended by the following EXIT statement:

            EXIT    ! "do iter = 1,max_iter"

          end if  ! "if(max_rel_iteration_change < rel_tol) then"

        end if  ! "if (max_iteration_change < tolerance)"


! After "lim_iter" iterations we can change the values of:
!      the relaxation-parameter,     "relax_par",
!      the absolute error tolerance, "tol"
!  and the relative error tolerance, "rel_tol":

        if (iter == (lim_iter+1) ) then
          relax_par = lim_rel_par
          tol       = lim_tolerance
          rel_tol   = lim_rel_tolerance
        end if

      end do  ! do iter = 1,max_iter

! ======================================================================


! If the wanted absolute and relative tolerance are not reached,
! appropriate messages are written to the log-file:

      if(not_reached_tol)then

        if (fe_log) then
          n_tol_not_reached = n_tol_not_reached + 1
          write(funit_log,*)
          write(funit_log,'(A,F8.6,A,F10.5)') &
        ' Tolerance not achieved. Tolerance = ',tolerance, &
        ' ALFA = ',alfa
          write(funit_log,'(A,F8.6)') &
        ' Max_rel_iteration_change = ',max_rel_iteration_change
          write(funit_log,'(A,F8.6)') &
        ' Max_iteration_change = ',max_iteration_change
          write(funit_log,*)
        end if

       end if  ! (not_reached_tol)


! =====================================================================
! Since each of the horizontal wind components depend on six neigh-
! boring lamda-values, we need lamda values both for k=0 and k=km+1.
! With the applied zero-gradient boundary condition these values are:
!
!
! Since this depend on the actual boundary condition it should
! preferably be done in a separate routine: "call vert_bc_lambda"??

      do j = 0,jm+1
        do i = 0,im+1
          lambda(i,j,0) = lambda(i,j,1)
          lambda(i,j,km+1) = lambda(i,j,km)
        end do
      end do
! =====================================================================

! Deallocate internal memory:

      if (allocated (alf)) deallocate (alf)
      if (allocated (bet)) deallocate (bet)
      if (allocated (gam)) deallocate (gam)
      if (allocated (brh)) deallocate (brh)

      if (allocated (del)) deallocate (del)
      if (allocated (cek)) deallocate (cek)
      if (allocated (mek)) deallocate (mek)
      if (allocated (lam)) deallocate (lam)

      if (allocated (iteration_change)) deallocate (iteration_change)
      if (allocated (rel_iteration_change))  &
                               deallocate (rel_iteration_change)

      return
      end subroutine tridiag_sor_solver
