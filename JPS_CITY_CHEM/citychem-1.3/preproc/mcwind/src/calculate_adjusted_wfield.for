! <calculate_adjusted_wfield.for - A component of the City-scale
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

      subroutine calculate_adjusted_wfield

!**********************************************************************
!
! This routine calculates the 3D wind field, that is:
! [u(0:im,jm,km) v(im,0:jm,km), w(im,jm,0:km) and omega(im,jm,0:km) ]
! from the computed field of lambda, i.e. lamda(0:im+1,0:jm+1,0:km+1).
!
! This routine applies: METHOD 1
!
! METHOD 1: Only u, v and w are calculated from the values of lambda.
!           The omega-values are computed from the discretized
!           continuity equation, i.e. the zero divergence requirement.
!
! METHOD 2: All of the wind components are computed from the values
!           of lambda.

! Modifications:
!    29 Jan 2019  M. Karl: replaced real*8 by double precision
!    29 Jan 2019  M. Karl: replaced TAB by spaces
!
!**********************************************************************
!     If needed, insert the necessary MODULE calls:

      use module_mc_wind_files
      use module_mc_wind_domain
      use module_mc_wind_winds
      use module_mc_wind_adjust

!**********************************************************************

!     We require the use of IMPLICIT NONE:

      implicit none

!     Global declarations:


!     Local declarations:

      integer :: i,j,k

      double precision  :: dlds
      double precision  :: dlamdx
      double precision  :: dlamdy

!      real*8  :: dlds_k
!      real*8  :: dlds_kp1
!      real*8  :: u_om
!      real*8  :: v_om
!      real*8  :: u_below
!      real*8  :: u_above
!      real*8  :: v_below
!      real*8  :: v_above
!      real*8  :: udev
!      real*8  :: udev_below
!      real*8  :: udev_above
!      real*8  :: vdev
!      real*8  :: vdev_below
!      real*8  :: vdev_above
!      real*8  :: weight_below
!      real*8  :: weight_above

      double precision, allocatable :: top_omega(:,:)

!     Content of the routine:

      if (.not. allocated (top_omega)) allocate (top_omega(im,jm))

!     Initialize arrays to zero:
        u          = 0.0
        v          = 0.0
        w          = 0.0
        omega      = 0.0
        top_omega  = 0.0

!     If the divergence is to be calculated during the iteration then
!     the lines below need to be incorporated:
      do j = 0,jm+1
        do i = 0,im+1
          lambda(i,j,0) = lambda(i,j,1)
          lambda(i,j,km+1) = lambda(i,j,km)
        end do
      end do


!     Calculate the x-component (eastward) of the wind field:
      do k = 1,km
        do j = 1,jm
          do i = 0,im

            dlds = (lambda(i+1,j,k+1) + lambda(i,j,k+1))*ak(k) &
            + (lambda(i+1,j,k  ) + lambda(i,j,k  ))*bk(k) &
            + (lambda(i+1,j,k-1) + lambda(i,j,k-1))*ck(k)

            u(i,j,k) =  u0(i,j,k) &
            + ( (lambda(i+1,j,k) - lambda(i,j,k) )/dx ) &
            + ( (dlds*h0msigl(k)*(depth(i+1,j) - depth(i,j)) )/ &
                (dx*2.0*depthu(i,j)) )

          end do
        end do
      end do

!     Calculate the y-component (northward) of the wind field:
      do k = 1,km
        do j = 0,jm
          do i = 1,im

            dlds = (lambda(i,j+1,k+1) + lambda(i,j,k+1))*ak(k) &
            + (lambda(i,j+1,k  ) + lambda(i,j,k  ))*bk(k) &
            + (lambda(i,j+1,k-1) + lambda(i,j,k-1))*ck(k)

            v(i,j,k) = v0(i,j,k) &
            + ( (lambda(i,j+1,k) - lambda(i,j,k)) / dy) &
            + ( (dlds*h0msigl(k)*(depth(i,j+1) - depth(i,j)) )/ &
               (dy*2.0*depthv(i,j)) )

          end do
        end do
      end do

!     Calculate the Cartesian z-component (upward), and the terrain
!     following sigma-component (omega) of the wind field:

      if (method_adjust == 1) then

        do k = 1,km-1

!          weight_above = (sigma(k) - sigmal(k))/
!     &                   (sigmal(k+1) - sigmal(k))
!          weight_below = (sigmal(k+1) - sigma(k))/
!     &                   (sigmal(k+1) - sigmal(k))

          do j = 1,jm
            do i = 1,im

!             Calculate the Cartesian vertical velocity:
!             ----------------------------------------------------------
!             Alternative 1:
!              dlds_kp1 = ak(k+1)*lambda(i,j,k+2)
!     &                 + bk(k+1)*lambda(i,j,k+1)
!     &                 + ck(k+1)*lambda(i,j,k)
!
!              dlds_k   = ak(k)*lambda(i,j,k+1)
!     &                 + bk(k)*lambda(i,j,k)
!     &                 + ck(k)*lambda(i,j,k-1)
!
!              dlds = weight_above*dlds_kp1 + weight_below*dlds_k
!
!             ----------------------------------------------------------
!             Alternative 2:
              dlds = (lambda(i,j,k+1) - lambda(i,j,k))/delta_sigmal(k+1)
!             ----------------------------------------------------------

              w(i,j,k) =  w0(i,j,k) &
                  + ((alfa*alfa*h0*dlds)/depth(i,j))

!             Calculate the Terrain following vertical velocity:
              omega(i,j,k) =  omega(i,j,k-1)  &
           - (delta_sigma(k)/depth(i,j))  &
            *( ((   u(i,j,k)  * depthu(i,j)  &
                  - u(i-1,j,k)* depthu(i-1,j))/dx) &
              +((   v(i,j,k)  * depthv(i,j)  &
                  - v(i,j-1,k)* depthv(i,j-1))/dy) )

            end do
          end do

        end do

!       Check the resulting error vertical velocity at the model top:

        do j = 1,jm
          do i = 1,im

!           Calculate the Terrain following vertical velocity at the
!           model top:

            top_omega(i,j) =  omega(i,j,km-1)  &
         - (delta_sigma(km)/depth(i,j)) &
          *( ((   u(i,j,km)  * depthu(i,j)  &
                - u(i-1,j,km)* depthu(i-1,j))/dx) &
            +((   v(i,j,km)  * depthv(i,j)  &
                - v(i,j-1,km)* depthv(i,j-1))/dy) )

!           We redefine the omega-value at the upper boundary so as
!           to achieve a divergence free uppermost layer as well:
!           For test purposes this is commented out at the moment.
!
!            omega(i,j,km) = top_omega(i,j)

          end do
        end do

      else if (method_adjust == 2) then

        do k = 1,km-1

!          weight_above = (sigma(k) - sigmal(k))/
!     &                   (sigmal(k+1) - sigmal(k))
!          weight_below = (sigmal(k+1) - sigma(k))/
!     &                   (sigmal(k+1) - sigmal(k))

          do j = 1,jm
            do i = 1,im

!             Calculate the Cartesian vertical velocity:
!             ----------------------------------------------------------
!             Alternative 1:
!
!              dlds_kp1 = ak(k+1)*lambda(i,j,k+2)
!     &                 + bk(k+1)*lambda(i,j,k+1)
!     &                 + ck(k+1)*lambda(i,j,k)
!
!              dlds_k   = ak(k)*lambda(i,j,k+1)
!     &                 + bk(k)*lambda(i,j,k)
!     &                 + ck(k)*lambda(i,j,k-1)
!
!              dlds = weight_above*dlds_kp1 + weight_below*dlds_k
!
!             ----------------------------------------------------------
!             Alternative 2 (This gives less divergence than Alt1:

              dlds = (lambda(i,j,k+1) - lambda(i,j,k))/delta_sigmal(k+1)
!             ----------------------------------------------------------

              w(i,j,k) =  w0(i,j,k) &
                   + ((alfa*alfa*h0*dlds)/depth(i,j))

!
!             Calculate the terrain following vertical velocity:
!
!             ----------------------------------------------------------
!             Start Alternative 0:
!
              dlamdx =(  delta_sigma(k)* &
                   (lambda(i+1,j,k+1) - lambda(i-1,j,k+1)) &
                  + delta_sigma(k+1)* &
                   (lambda(i+1,j,k)   - lambda(i-1,j,k)  ) ) &
                 / (4.0*dx*delta_sigmal(k+1))
              dlamdy =(  delta_sigma(k)* &
                   (lambda(i,j+1,k+1) - lambda(i,j-1,k+1)) &
                  + delta_sigma(k+1)* &
                   (lambda(i,j+1,k)   - lambda(i,j-1,k)  ) ) &
                 / (4.0*dy*delta_sigmal(k+1))

              omega(i,j,k) = omega0(i,j,k)  &
           + ( alfa*alfa*(h0/depth(i,j))*(h0/depth(i,j))*dlds ) &
           + ( h0msig(k)*( dRD_dx(i,j)* &
                           (dlamdx + h0msig(k)*dRD_dx(i,j)*dlds) &
                         + dRD_dy(i,j)* &
                           (dlamdy + h0msig(k)*dRD_dy(i,j)*dlds) ))


!           ------------------------------------------------------------
!
!           Alternative 1 (This gives larger 3D-divergences than 4A):
!
!            udev_below = 0.5*(   u(i-1,j,k) +  u(i,j,k)
!     &                        - u0(i-1,j,k) - u0(i,j,k))
!            vdev_below = 0.5*(   v(i,j-1,k) +  v(i,j,k)
!     &                        - v0(i,j-1,k) - v0(i,j,k))
!
!            udev_above = 0.5*(   u(i-1,j,k+1) +  u(i,j,k+1)
!     &                        - u0(i-1,j,k+1) - u0(i,j,k+1))
!            vdev_above = 0.5*(   v(i,j-1,k+1) +  v(i,j,k+1)
!     &                        - v0(i,j-1,k+1) - v0(i,j,k+1))
!
!            udev = weight_below*udev_below + weight_above*udev_above
!            vdev = weight_below*vdev_below + weight_above*vdev_above
!
!
!            omega(i,j,k) =   omega0(i,j,k)
!     &                   + ((alfa*alfa*h0*h0*dlds)/(depth(i,j)**2))
!     &                   + (  dRD_dx(i,j)*udev
!     &                      + dRD_dy(i,j)*vdev )*h0msig(k)
!
!           End Alternative 1.
!
!           ============================================================
!           Start Alternative 4:
!
!            u_below = 0.5*( u(i-1,j,k) + u(i,j,k) )
!            v_below = 0.5*( v(i,j-1,k) + v(i,j,k) )
!
!            u_above = 0.5*( u(i-1,j,k+1) + u(i,j,k+1) )
!            v_above = 0.5*( v(i,j-1,k+1) + v(i,j,k+1) )
!
!           Start Alternative 4A:
!            u_om = weight_below*u_below + weight_above*u_above
!            v_om = weight_below*v_below + weight_above*v_above
!           End Alternative 4A.
!
!           Alternative 4B (This gives larger 3D-divergences than 4A):
!            u_om = u_below
!            v_om = v_below
!           End Alternative 4B.
!
!            omega(i,j,k) = ( h0*w(i,j,k)/depth(i,j)     +
!     &                       h0msig(k)*dRD_dx(i,j)*u_om +
!     &                       h0msig(k)*dRD_dy(i,j)*v_om )
!
!           End Alternative 4.
!           ============================================================

            end do
          end do
        end do

!       Check the resulting error vertical velocity at the model top:

        do j = 1,jm
          do i = 1,im

!           Calculate the Terrain following vertical velocity at the
!           model top:

            top_omega(i,j) =  omega(i,j,km-1)  &
         - (delta_sigma(km)/depth(i,j)) &
          *( ((   u(i,j,km)  * depthu(i,j)  &
                - u(i-1,j,km)* depthu(i-1,j))/dx) &
            +((   v(i,j,km)  * depthv(i,j)  &
                - v(i,j-1,km)* depthv(i,j-1))/dy) )

          end do
        end do

       end if ! method_adjust == 1 or 2.


      if (fe_log) then

        max_u0     = MAXVAL(u0)
        max_v0     = MAXVAL(v0)
        max_w0     = MAXVAL(w0)
        max_omega0 = MAXVAL(omega0)

         write(funit_log,*)
         write(funit_log,'(28X,3A4)') '  I ','  J ','  K '
         write(funit_log,'(1X,A16,F10.5,3I4)') &
        'Max u0: ',max_u0,MAXLOC(u0)
         write(funit_log,'(1X,A16,F10.5,3I4)') &
        'Max v0: ',max_v0,MAXLOC(v0)
         write(funit_log,'(1X,A16,F10.5,3I4)') &
        'Max w0: ',max_w0,MAXLOC(w0)
         write(funit_log,'(1X,A16,F10.5,3I4)') &
        'Max omega0: ',max_omega0,MAXLOC(omega0)

        min_u0     = MINVAL(u0)
        min_v0     = MINVAL(v0)
        min_w0     = MINVAL(w0)
        min_omega0 = MINVAL(omega0)

         write(funit_log,*)
         write(funit_log,'(28X,3A4)') '  I ','  J ','  K '
         write(funit_log,'(1X,A16,F10.5,3I4)') &
        'Min u0: ',min_u0,MINLOC(u0)
         write(funit_log,'(1X,A16,F10.5,3I4)') &
        'Min v0: ',min_v0,MINLOC(v0)
         write(funit_log,'(1X,A16,F10.5,3I4)') &
        'Min w0: ',min_w0,MINLOC(w0)
         write(funit_log,'(1X,A16,F10.5,3I4)') &
        'Min omega0: ',min_omega0,MINLOC(omega0)

        max_u          = MAXVAL(u)
        max_v          = MAXVAL(v)
        max_w          = MAXVAL(w)
        max_omega      = MAXVAL(omega)
        max_top_omega  = MAXVAL(top_omega)

         write(funit_log,*)
         write(funit_log,'(34X,3A4)') '  I ','  J ','  K '
         write(funit_log,'(1X,A23,F10.5,3I4)') &
        'Max u (m/s): ',max_u,MAXLOC(u)
         write(funit_log,'(1X,A23,F10.5,3I4)') &
        'Max v (m/s): ',max_v,MAXLOC(v)
         write(funit_log,'(1X,A23,F10.5,3I4)') &
        'Max w (m/s): ',max_w,MAXLOC(w)
         write(funit_log,'(1X,A23,F10.5,3I4)') &
        'Max omega (m/s): ',max_omega,MAXLOC(omega)
         write(funit_log,'(1X,A23,E10.4,3I4)') &
        'Max top_omega (m/s): ',max_top_omega,MAXLOC(top_omega)

        min_u          = MINVAL(u)
        min_v          = MINVAL(v)
        min_w          = MINVAL(w)
        min_omega      = MINVAL(omega)
        min_top_omega  = MINVAL(top_omega)

         write(funit_log,*)
         write(funit_log,'(34X,3A4)') '  I ','  J ','  K '
         write(funit_log,'(1X,A23,F10.5,3I4)') &
        'Min u (m/s): ',min_u,MINLOC(u)
         write(funit_log,'(1X,A23,F10.5,3I4)') &
        'Min v (m/s): ',min_v,MINLOC(v)
         write(funit_log,'(1X,A23,F10.5,3I4)') &
        'Min w (m/s): ',min_w,MINLOC(w)
         write(funit_log,'(1X,A23,F10.5,3I4)') &
        'Min omega (m/s): ',min_omega,MINLOC(omega)
         write(funit_log,'(1X,A23,E10.4,3I4)') &
        'Min top_omega (m/s): ',min_top_omega,MINLOC(top_omega)

      end if  ! (fe_log)

!     Calculate and print out the maximum and minimum divergence in 
!     the applied initial (first guess) wind field:

      call calculate_3D_divergence(im,jm,km,dx,dy,delta_sigma, &
                              depth,depthu,depthv,u0,v0, &
                              omega0,div0_3d)

      if (fe_log) then

        write(funit_log,*)
        write(funit_log,'(40X,3A4)') '  I ','  J ','  K '
        write(funit_log,'(1X,A28,E10.4,3I4)') &
         'Max. initial 3D-divergence: ', &
         MAXVAL(div0_3D),MAXLOC(div0_3D)
        write(funit_log,'(1X,A28,E10.4,3I4)') &
         'Min. initial 3D-divergence: ', &
         MINVAL(div0_3D),MINLOC(div0_3D)

      end if ! (fe_log)

!     Calculate and print out the maximum and minimum divergence in
!     the final adjusted wind field:

      call calculate_3D_divergence(im,jm,km,dx,dy,delta_sigma, &
                              depth,depthu,depthv,u,v, &
                              omega,div_3d)

      if (fe_log) then

        write(funit_log,*)
        write(funit_log,'(40X,3A4)') '  I ','  J ','  K '
        write(funit_log,'(1X,A28,E10.4,3I4)') &
         'Max. final 3D-divergence: ', &
         MAXVAL(div_3D),MAXLOC(div_3D)
        write(funit_log,'(1X,A28,E10.4,3I4)') &
         'Min. final 3D-divergence: ', &
         MINVAL(div_3D),MINLOC(div_3D)
        write(funit_log,*)
       ! write(funit_log,'(1X,A,E10.4)') &
       !  'Final 3D-divergence(11,9,1): ',div_3D(11,9,1)
        write(funit_log,*)

      end if  ! (fe_log)


!     Deallocate local memory:

      if (allocated (top_omega))  deallocate (top_omega)

      return
      end subroutine calculate_adjusted_wfield
