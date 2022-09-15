! <initialize_static.for - A component of the City-scale
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
!DEC$ FIXEDFORMLINESIZE: 132
!***********************************************************************

      subroutine initialize_static

!_LHS Change 3: October_2010_Start:
!_LHS  !DEC$ ATTRIBUTES DLLEXPORT,STDCALL,ALIAS:'initialize_static' :: initialize_static
!_LHS Change 3: October_2010_End.

!     This subroutine allocates the necessary array dimensiones and
!     calculates the value for arrays that are not changed when the
!     first guess wind field is changed.

! Modifications:
!     21 Jan 2019  M. Karl: Commented pre-processor directive 'main_program'
!     29 Jan 2019  M. Karl: Replaced TAB by spaces
!
!***********************************************************************


      use module_mc_wind_files
      use module_mc_wind_domain
      use module_mc_wind_met
      use module_mc_wind_winds
      use module_mc_wind_adjust


!     We require the use of IMPLICIT NONE:
      implicit none

!     Local declarations:
      integer            :: i,j,k

!!DEC$ IF DEFINED (main_program)
      real               :: max_landuse,min_landuse
!!DEC$ ENDIF

      real               :: max_surfrough,min_surfrough
      real               :: max_u0_surfrough,max_v0_surfrough
      real               :: min_u0_surfrough,min_v0_surfrough

!!DEC$ IF DEFINED (main_program)
      character (len=10) :: txt1,txt2
!!DEC$ ENDIF

!     Content of routine:

!     Allocate the required internal memory:

!     Applied 1D-arrays (vectors):
      if (.not. allocated (h0msig))  allocate (h0msig(0:km))
      if (.not. allocated (h0msigl)) allocate (h0msigl(km))

      if (.not. allocated (ak)) allocate (ak(km))
      if (.not. allocated (bk)) allocate (bk(km))
      if (.not. allocated (ck)) allocate (ck(km))
      if (.not. allocated (pk)) allocate (pk(km))
      if (.not. allocated (qk)) allocate (qk(km))
      if (.not. allocated (rk)) allocate (rk(km))

!     Applied 2D-arrays:

      if (.not. allocated (dD_dx))    allocate (dD_dx(0:im,jm))
      if (.not. allocated (dD_dy))    allocate (dD_dy(im,0:jm))

      if (.not. allocated (dRD_dx))   allocate (dRD_dx(im,jm))
      if (.not. allocated (d2RD_dxx)) allocate (d2RD_dxx(im,jm))
      if (.not. allocated (dRD_dy))   allocate (dRD_dy(im,jm))
      if (.not. allocated (d2RD_dyy)) allocate (d2RD_dyy(im,jm))

      if (.not. allocated (surfrough))  &
            allocate (surfrough(0:im+1,0:jm+1))

      if (.not. allocated (u0_surfrough))  &
            allocate (u0_surfrough(0:im,jm))

      if (.not. allocated (v0_surfrough))  &
            allocate (v0_surfrough(im,0:jm))

      if (.not. allocated (landuse))  &
            allocate (landuse(0:im+1,0:jm+1))

      if (.not. allocated (u0_surface_ref))  &
            allocate (u0_surface_ref(0:im,jm))

      if (.not. allocated (v0_surface_ref))  &
            allocate (v0_surface_ref(im,0:jm))

      if (.not. allocated (u0_geostrophic_ref))  &
            allocate (u0_geostrophic_ref(0:im,jm))

      if (.not. allocated (v0_geostrophic_ref))  &
            allocate (v0_geostrophic_ref(im,0:jm))

!     Applied 3D-arrays:
      if (.not. allocated (f1st))   allocate (f1st(im,jm,km))
      if (.not. allocated (f1))     allocate (f1(im,jm,km))
      if (.not. allocated (f2))     allocate (f2(im,jm,km))
      if (.not. allocated (f3))     allocate (f3(im,jm,km))
      if (.not. allocated (f4))     allocate (f4(im,jm,km))

!     New alternative coefficients:
      if (.not. allocated (h1))     allocate (h1(im,jm,km))
      if (.not. allocated (h2))     allocate (h2(im,jm,km))
      if (.not. allocated (h3st))   allocate (h3st(im,jm,km))
      if (.not. allocated (h3))     allocate (h3(im,jm,km))
      if (.not. allocated (h4))     allocate (h4(im,jm,km))
      if (.not. allocated (h5))     allocate (h5(im,jm,km))
      if (.not. allocated (h6))     allocate (h6(im,jm,km))
      if (.not. allocated (h7))     allocate (h7(im,jm,km))
      if (.not. allocated (h8st))   allocate (h8st(im,jm,km))
      if (.not. allocated (h8))     allocate (h8(im,jm,km))
      if (.not. allocated (h9))     allocate (h9(im,jm,km))
      if (.not. allocated (h10))    allocate (h10(im,jm,km))
      if (.not. allocated (h11))    allocate (h11(im,jm,km))
      if (.not. allocated (h12))    allocate (h12(im,jm,km))
      if (.not. allocated (h13st))    allocate (h13st(im,jm,km))
      if (.not. allocated (h13))    allocate (h13(im,jm,km))
      if (.not. allocated (h14))    allocate (h14(im,jm,km))
      if (.not. allocated (h15))    allocate (h15(im,jm,km))

      if (.not. allocated (g_rhs))  allocate (g_rhs(im,jm,km))
!     LHS_TEST_START:
!      if (.not. allocated (bcg))    allocate (bcg(0:im+1,0:jm+1))
!     LHS_TEST_END.

      if (.not. allocated (u0_surface))  &
            allocate (u0_surface(0:im,jm,km))
      if (.not. allocated (v0_surface)) &
            allocate (v0_surface(im,0:jm,km))
      if (.not. allocated (w0_surface)) &
            allocate (w0_surface(im,jm,0:km))
      if (.not. allocated (omega0_surface))  &
            allocate (omega0_surface(im,jm,0:km))

      if (.not. allocated (u0_geostrophic))  &
            allocate (u0_geostrophic(0:im,jm,km))
      if (.not. allocated (v0_geostrophic)) &
            allocate (v0_geostrophic(im,0:jm,km))
      if (.not. allocated (w0_geostrophic)) &
            allocate (w0_geostrophic(im,jm,0:km))
      if (.not. allocated (omega0_geostrophic))  &
            allocate (omega0_geostrophic(im,jm,0:km))


      if (.not. allocated (u0))     allocate (u0(0:im,jm,km))
      if (.not. allocated (v0))     allocate (v0(im,0:jm,km))
      if (.not. allocated (w0))     allocate (w0(im,jm,0:km))
      if (.not. allocated (omega0)) allocate (omega0(im,jm,0:km))

      if (.not. allocated (u))      allocate (u(0:im,jm,km))
      if (.not. allocated (v))      allocate (v(im,0:jm,km))
      if (.not. allocated (w))      allocate (w(im,jm,0:km))
      if (.not. allocated (omega))  allocate (omega(im,jm,0:km))

      if (.not. allocated (lambda))  &
            allocate (lambda(0:im+1,0:jm+1,0:km+1))

      if (.not. allocated (div0_3D))  allocate (div0_3D(im,jm,km))
      if (.not. allocated (div_3D))   allocate (div_3D(im,jm,km))

!=======================================================================
! LHS_June_2008_INTEL-compiler_Start:
!      fc = 1.0E-04
! LHS_June_2008_INTEL-compiler_End.
!
! LHS_May_2008_INTEL-compiler_Start:
!      u0     = 0.0
!      v0     = 0.0
!      w0     = 0.0
      omega0 = 0.0

!      u      = 0.0
!      v      = 0.0
!      w      = 0.0
!      omega  = 0.0
! LHS_May_2008_INTEL-compiler_End.
!=======================================================================



      fe_in_surface_obs = .false.

       if (fname_in_surface_obs(1:1) /= ' ') &
           call opifil(fname_in_surface_obs,funit_in_surface_obs, &
               fe_in_surface_obs,fm_in_surface_obs)



!=======================================================================

      fe_in_profile_obs = .false.

        if (fname_in_profile_obs(1:1) /= ' ') &
           call opifil(fname_in_profile_obs,funit_in_profile_obs, &
               fe_in_profile_obs,fm_in_profile_obs)


!=======================================================================

!     Applied gridded Land-Use information: landuse(0:im+1,0:jm+1)
!
!     Open and read in the 2D gridded land-use values defined
!     centrally in the grid, i.e. in the lambda-points:
        fe_in_landuse = .false.
        if (fname_in_landuse(1:1) /= ' ') then
          call opifil(fname_in_landuse,funit_in_landuse,fe_in_landuse, &
               fm_in_landuse)
        if (.not. allocated (in_landuse)) allocate (in_landuse(im,jm))
          call r2dfld(funit_in_landuse,fm_in_landuse,txt1,txt2,im,jm, &
               in_landuse)
          call clifil(funit_in_landuse)

        do j = 1,jm
          do i = 1,im
            landuse(i,j) = in_landuse(i,j)
          end do
        end do

      else
        landuse = 14
      end if

!     We must decide on what to do at the boundaries. If the available
!     land-use information just covers the inner domain (i=1,im,  j=1,jm)
!     then we must define the outermost values, i.e roughness(i,j) for
!     i=0, i=im+1, j=0 and j=jm+1. Below this is done by setting these
!     values equal to the neighbor inner domain point.

      do j=1,jm
        landuse(0,j)    = landuse(1,j)
        landuse(im+1,j) = landuse(im,j)
      end do
      do i=0,im+1
        landuse(i,0)    = landuse(i,1)
        landuse(i,jm+1) = landuse(i,jm)
      end do


      if (fe_log) then
        max_landuse = MAXVAL(landuse)
        min_landuse = MINVAL(landuse)
        write(funit_log,*)
        write(funit_log,'(28X,2A4)') '  I ','  J '
        write(funit_log,'(1X,A16,F10.5,3I4)') &
        'Max landuse(i,j): ',max_landuse,MAXLOC(landuse)
        write(funit_log,'(1X,A16,F10.5,3I4)') &
        'Min landuse(i,j): ',min_landuse,MINLOC(landuse)


        write(funit_log,*)
      !  write(funit_log,'(1X,A16,F10.5)') &
      !  'Landuse(14,12): ',landuse(14,12)
        write(funit_log,*)

      end if


! *** At the moment we do not apply LANDUSE information in AirQUIS
! *** applications of MC_WIND.


!=======================================================================
!
!     Applied gridded Surface Roughness: surf_rough(0:im+1,0:jm+1)
!
!
!     Open and read in the 2D gridded surface roughness values defined
!     centrally in the grid, i.e. in the lambda-points.

      fe_in_surfrough = .false.

        if (fname_in_surfrough(1:1) /= ' ') then

         call opifil(fname_in_surfrough,funit_in_surfrough, &
               fe_in_surfrough,fm_in_surfrough)

        if (.not. allocated (in_surfrough))  &
             allocate (in_surfrough(im,jm))

        call r2dfld(funit_in_surfrough,fm_in_surfrough,txt1,txt2,im,jm, &
               in_surfrough)

         call clifil(funit_in_surfrough)

        do j = 1,jm
          do i = 1,im
            surfrough(i,j) = in_surfrough(i,j)
!_TEST            surfrough(i,j) = (in_surfrough(i,j) * 0.1)
!_TEST            surfrough(i,j) = z0_constant
          end do
        end do

      else

!     If no surface roughness file exists, the surface roughness can
!     either be defined based on the Land-Use information above,
!     or set to a predefined (user-specified) constant.

        do j = 1,jm
          do i = 1,im

            if(NINT(landuse(i,j)) == 1)then
!             Urban area: z0 is set to 1.0 meter.
              surfrough(i,j) = 1.0
            elseif(NINT(landuse(i,j)) == 16)then
!             Water bodies: For Oslo that is fjords, not open sea.
              surfrough(i,j) = 0.05
            else
!             Apply 0.5 for all other areas.
              surfrough(i,j) = 0.5
            endif
!     Override:
!              surfrough(i,j) = 0.4
!              surfrough(i,j) = 1.0
!              surfrough(i,j) = 1.0

          end do
        end do

      end if


!     We must decide on what to do at the boundaries. If the available
!     surface roughness just covers the inner domain (i=1,im,  j=1,jm)
!     then we must define the outermost values, i.e roughness(i,j)
!     for i=0, i=im+1, j=0 and j=jm+1. Below this is done by setting
!     these values equal to the neighbor inner domain point.

      do j=1,jm
        surfrough(0,j)    = surfrough(1,j)
        surfrough(im+1,j) = surfrough(im,j)
      end do
      do i=0,im+1
        surfrough(i,0)    = surfrough(i,1)
        surfrough(i,jm+1) = surfrough(i,jm)
      end do

!     Below simple linear interpolation is applied in order to
!     calculate the surface roughness in the grid points of u and v:
      do j = 1,jm
        do i = 0,im
          u0_surfrough(i,j) = 0.5*(surfrough(i,j) + surfrough(i+1,j))
        end do
      end do

      do j = 0,jm
        do i = 1,im
          v0_surfrough(i,j) = 0.5*(surfrough(i,j) + surfrough(i,j+1))
        end do
      end do

! *** Write the max and min surface roughness value for u0 and v0.
      if (fe_log) then

        max_surfrough = MAXVAL(surfrough)
        min_surfrough = MINVAL(surfrough)
        write(funit_log,*)
        write(funit_log,'(28X,2A4)') '  I ','  J '
        write(funit_log,'(1X,A16,F10.5,3I4)') &
        'Max z0(i,j): ',max_surfrough,MAXLOC(surfrough)
        write(funit_log,'(1X,A16,F10.5,3I4)') &
        'Min z0(i,j): ',min_surfrough,MINLOC(surfrough)

        max_u0_surfrough = MAXVAL(u0_surfrough)
        max_v0_surfrough = MAXVAL(v0_surfrough)

        write(funit_log,*)
        write(funit_log,'(28X,2A4)') '  I ','  J '
        write(funit_log,'(1X,A16,F10.5,3I4)') &
        'Max zu0(i,j): ',max_u0_surfrough,MAXLOC(u0_surfrough)
        write(funit_log,'(1X,A16,F10.5,3I4)') &
        'Max zv0(i,j): ',max_v0_surfrough,MAXLOC(v0_surfrough)

        min_u0_surfrough = MINVAL(u0_surfrough)
        min_v0_surfrough = MINVAL(v0_surfrough)

        write(funit_log,*)
        write(funit_log,'(28X,3A4)') '  I ','  J '
        write(funit_log,'(1X,A16,F10.5,3I4)') &
        'Min zu0(i,j): ',min_u0_surfrough,MINLOC(u0_surfrough)
        write(funit_log,'(1X,A16,F10.5,3I4)') &
        'Min zv0(i,j): ',min_v0_surfrough,MINLOC(v0_surfrough)
        write(funit_log,*)

      end if


!=======================================================================
!     Defining the static coefficients that is to be used in the SOR-
!     adjustment process:

! *** NOTE - NOTE - NOTE - NOTE - NOTE - NOTE - NOTE - NOTE - NOTE *****
!
! *** At the ground boundary: h0 - sigma(0) = h0

      h0msig(0) = h0

! *** NOTE: To avoid discretization errors, however, the term:
!     [D*(omega - omega0)] of which we take the vertical sigma-
!     derivative, the value at the ground boundary should be identical
!     zero, according to the boundary condition. This is accomplished
!     by setting the quantity h0 - sigma(0) = 0.0

      h0msig(0) = 0.0

! *** NOTE - NOTE - NOTE - NOTE - NOTE - NOTE - NOTE - NOTE - NOTE *****

      do k = 1,km
        h0msig(k)  = h0 - sigma(k)
        h0msigl(k) = h0 - sigmal(k)

!     ==================================================================
!       Alternative 1:
!
        ak(k) = 1.0/(2.0 * delta_sigmal(k+1))

        bk(k) = 1.0/(2.0 * delta_sigma(k))
        bk(k) = bk(k) * (  (delta_sigma(k+1)/delta_sigmal(k+1)) &
                    - (delta_sigma(k-1)/delta_sigmal(k)  ) )

        ck(k) = - 1.0/(2.0 * delta_sigmal(k))
!
!       ----------------------------------------------------------------
!       Alternative 2:
!
!        ak(k) = 1.0/(delta_sigma(k) + delta_sigma(k+1))
!
!        bk(k) = 1.0/delta_sigma(k)
!        bk(k) = bk(k) *
!     &    ( (delta_sigma(k+1)/(delta_sigma(k) + delta_sigma(k+1)) )
!     &     -(delta_sigma(k-1)/(delta_sigma(k-1) + delta_sigma(k)) ))
!
!        ck(k) = - 1.0/(delta_sigma(k-1) + delta_sigma(k))
!
!       ----------------------------------------------------------------
!       Alternative 4 (Simple centered) Produce much higher divergence.
!
!        ak(k) = 1.0/(delta_sigmal(k+1) + delta_sigmal(k))
!        bk(k) = 0.0
!        ck(k) = - ak(k)
!
!     =================================================================
!
!       Alternative 1:
!        pk(k) =  1.0/(delta_sigma(k)*delta_sigmal(k))
!
!        qk(k) =  (4.0 - (delta_sigma(k+1)/delta_sigmal(k+1))
!     &                - (delta_sigma(k-1)/delta_sigmal(k)  ) )
!     &         * (1.0/(delta_sigma(k)**2)                    )
!
!        rk(k) =  1.0/(delta_sigma(k)*delta_sigmal(k+1))
!
!       Alternative 2:

        pk(k) =  1.0/(delta_sigma(k)*delta_sigmal(k))

        if (k ==1) then
          qk(k) =  1.0/( delta_sigma(k) * delta_sigmal(k+1) )
        else if (k == km) then
          qk(k) =  1.0/( delta_sigma(k) * delta_sigmal(k) )
        else
          qk(k) =  (1.0/delta_sigma(k))*(  (1.0/delta_sigmal(k+1))  &
                                    + (1.0/delta_sigmal(k)  ) )
        end if
        rk(k) =  1.0/(delta_sigma(k)*delta_sigmal(k+1))

!       Alternative 3 (Simple centered) Produce much higher divergence.
!
!        pk(k) =  1.0/(0.5*(delta_sigmal(k+1) + delta_sigmal(k)))**2
!
!        qk(k) =  2.0*pk(k)
!
!        rk(k) =  pk(k)
!
!     ==================================================================
      enddo

!     Define the various derivative terms of the model depth that is
!     applied in the lamda points during the SOR-adjustment process:

      do j = 1,jm
        do i = 0,im

          dD_dx(i,j)=(depth(i+1,j) - depth(i,j))/dx

        end do
      end do

      do j = 0,jm
        do i = 1,im

          dD_dy(i,j)=(depth(i,j+1) - depth(i,j))/dy

        end do
      end do

      do j = 1,jm
        do i = 1,im

          dRD_dx(i,j)   = (depth(i+1,j) - depth(i-1,j)) &
                    /(2.0*dx*depth(i,j))
          d2RD_dxx(i,j) = (depth(i+1,j) - 2.*depth(i,j) + depth(i-1,j)) &
                    /(dx*dx*depth(i,j))
          dRD_dy(i,j)    = (depth(i,j+1) - depth(i,j-1)) &
                    /(2.0*dy*depth(i,j))
          d2RD_dyy(i,j) = (depth(i,j+1) - 2.*depth(i,j) + depth(i,j-1)) &
                    /(dy*dy*depth(i,j))

        end do
      end do


!     Calculate the static coefficients for the Poisson Equation.
!     Note that f2 and f3 are redefined in that they have been divided
!     by 2.0*dx and 2.0*dy, respectively:

      do k = 1,km
        do j = 1,jm
          do i = 1,im

!           Version 1:
            f1st(i,j,k) = (   dRD_dx(i,j)*dRD_dx(i,j)  &
                       + dRD_dy(i,j)*dRD_dy(i,j) )  &
                    *( h0msigl(k)*h0msigl(k) )

            f2(i,j,k) = dRD_dx(i,j)*h0msigl(k)/dx
            f3(i,j,k) = dRD_dy(i,j)*h0msigl(k)/dy

            f4(i,j,k) = (  d2RD_dxx(i,j) + d2RD_dyy(i,j)  &
                    - 2.0*(  dRD_dx(i,j)*dRD_dx(i,j)  &
                           + dRD_dy(i,j)*dRD_dy(i,j)) )*h0msigl(k)

!           Version 2:

            h1(i,j,k) =  &
         - (dD_dy(i,j-1)*h0msigl(k)*ck(k))/(2.0*dy*depth(i,j)) &
         + (dRD_dy(i,j)*h0msig(k-1)*delta_sigma(k)*pk(k))/(4.0*dy)


            h2(i,j,k) = &
         - (dD_dx(i-1,j)*h0msigl(k)*ck(k))/(2.0*dx*depth(i,j)) &
         + (dRD_dx(i,j)*h0msig(k-1)*delta_sigma(k)*pk(k))/(4.0*dx)


            h3st(i,j,k) = (0.5 *( d2RD_dxx(i,j) + d2RD_dyy(i,j) ) &
                          * h0msigl(k)*ck(k) ) &
         + ( ( dRD_dx(i,j)**2 + dRD_dy(i,j)**2 ) &
             *pk(k)*h0msig(k-1)**2 )


            h4(i,j,k) = &
           (dD_dx(i,j)*h0msigl(k)*ck(k))/(2.0*dx*depth(i,j)) &
         - (dRD_dx(i,j)*h0msig(k-1)*delta_sigma(k)*pk(k))/(4.0*dx)


            h5(i,j,k) = &
           (dD_dy(i,j)*h0msigl(k)*ck(k))/(2.0*dy*depth(i,j)) &
         - (dRD_dy(i,j)*h0msig(k-1)*delta_sigma(k)*pk(k))/(4.0*dy)


            h6(i,j,k) = ( depthv(i,j-1)/(dy*dy*depth(i,j)) ) &
         - ((dD_dy(i,j-1)*h0msigl(k)*bk(k))/(2.0*dy*depth(i,j))) &
         - ( dRD_dy(i,j)*(rk(k)*delta_sigma(k+1)*h0msig(k) &
                        - pk(k)*delta_sigma(k-1)*h0msig(k-1) ) &
          /(4.0*dy) )


            h7(i,j,k) = ( depthu(i-1,j)/(dx*dx*depth(i,j)) ) &
         - ((dD_dx(i-1,j)*h0msigl(k)*bk(k))/(2.0*dx*depth(i,j))) &
         - ( dRD_dx(i,j)*(rk(k)*delta_sigma(k+1)*h0msig(k) &
                        - pk(k)*delta_sigma(k-1)*h0msig(k-1) ) &
          /(4.0*dx) )


            h8st(i,j,k)  = - ( ( depthu(i,j) + depthu(i-1,j) ) &
                         /(depth(i,j)*dx*dx) ) &
                      - ( ( depthv(i,j) + depthv(i,j-1) ) &
                         /(depth(i,j)*dy*dy) ) &
         + 0.5*( d2RD_dxx(i,j) + d2RD_dyy(i,j) )*bk(k)*h0msigl(k) &
         - ( dRD_dx(i,j)**2 + dRD_dy(i,j)**2 ) &
          *( rk(k)*(h0msig(k))**2 + pk(k)*(h0msig(k-1))**2 )


            h9(i,j,k) = ( depthu(i,j)/(dx*dx*depth(i,j)) ) &
         + ((dD_dx(i,j)*h0msigl(k)*bk(k))/(2.0*dx*depth(i,j))) &
         + ( dRD_dx(i,j)*(rk(k)*delta_sigma(k+1)*h0msig(k) &
                        - pk(k)*delta_sigma(k-1)*h0msig(k-1) ) &
          /(4.0*dx) )


            h10(i,j,k) = ( depthv(i,j)/(dy*dy*depth(i,j)) ) &
         + ((dD_dy(i,j)*h0msigl(k)*bk(k))/(2.0*dy*depth(i,j))) &
         + ( dRD_dy(i,j)*(rk(k)*delta_sigma(k+1)*h0msig(k) &
                        - pk(k)*delta_sigma(k-1)*h0msig(k-1) ) &
          /(4.0*dy) )


            h11(i,j,k) = &
         - (dD_dy(i,j-1)*h0msigl(k)*ak(k))/(2.0*dy*depth(i,j)) &
         - (dRD_dy(i,j)*h0msig(k)*delta_sigma(k)*rk(k))/(4.0*dy)


            h12(i,j,k) = &
         - (dD_dx(i-1,j)*h0msigl(k)*ak(k))/(2.0*dx*depth(i,j)) &
         - (dRD_dx(i,j)*h0msig(k)*delta_sigma(k)*rk(k))/(4.0*dx)


            h13st(i,j,k) = (0.5 *( d2RD_dxx(i,j) + d2RD_dyy(i,j) ) &
                           * h0msigl(k)*ak(k) ) &
         + ( ( dRD_dx(i,j)**2 + dRD_dy(i,j)**2 ) &
             *rk(k)*h0msig(k)**2 )


            h14(i,j,k) =  &
           (dD_dx(i,j)*h0msigl(k)*ak(k))/(2.0*dx*depth(i,j)) &
         + (dRD_dx(i,j)*h0msig(k)*delta_sigma(k)*rk(k))/(4.0*dx)


            h15(i,j,k) =  &
           (dD_dy(i,j)*h0msigl(k)*ak(k))/(2.0*dy*depth(i,j)) &
         + (dRD_dy(i,j)*h0msig(k)*delta_sigma(k)*rk(k))/(4.0*dy)

          end do
        end do
      end do

!     Write some essential information about vertical model depth and
!     vertical grid spacing to the log-file:

      if (fe_log) then

        n_tol_not_reached  = 0

        write(funit_log,*)
      !   write(funit_log,*) 'Initialize_static:  f1st(20,16,1) = ', &
      !                 f1st(20,16,1)
      !  write(funit_log,*) 'Initialize_static:    f2(20,16,1) = ', &
      !                 f2(20,16,1)
      !  write(funit_log,*) 'Initialize_static:    f3(20,16,1) = ', &
      !                 f3(20,16,1)
      !  write(funit_log,*) 'Initialize_static:    f4(20,16,1) = ', &
      !                 f4(20,16,1)
      end if

      return
      end subroutine initialize_static
