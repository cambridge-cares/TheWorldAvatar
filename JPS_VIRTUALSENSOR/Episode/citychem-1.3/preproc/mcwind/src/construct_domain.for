! <construct_domain.for - A component of the City-scale
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

      subroutine construct_domain

!_LHS Change 2: October_2010_Start:
!_LHS  !DEC$ ATTRIBUTES DLLEXPORT,STDCALL,ALIAS:'construct_domain' :: construct_domain
!_LHS Change 2: October_2010_End.

!     This subroutine allocates the necessary array dimensiones and
!     construct all the variables and arrays that define the gridded
!     model domain.

! Modifications:
!    29 Jan 2019  M. Karl: replaced TAB by spaces
!
!***********************************************************************

      use module_mc_wind_files
      use module_mc_wind_domain

!     We require the use of IMPLICIT NONE:
      implicit none

!     Local declarations:
      integer            :: i,j,k


      character (len=10) :: txt1,txt2


!     Content of routine:


! *** Calculating the appropriate value of the Coriolis parameter:

       IF(ABS(clat) .GT. 20.0)THEN
         fc = 2.0 * sid_freq*SIN(clat * 3.1415927/180.0)
       ELSE
         fc = 2.0 * sid_freq*SIN(20.0 * 3.1415927/180.0)
       ENDIF
!	fc = 1.2E-4

      if (fe_log) then
        write(funit_log,'(A,F12.5)')  ' Model domain longitude = ',clon
        write(funit_log,'(A,F12.5)')  ' Model domain latitude  = ',clat
        write(funit_log,'(A,E12.5)')  &
       ' Thus the computed Coriolis parameter = ',fc
        write(funit_log,*)
      endif


!     Allocate the required internal memory:

!     Applied 1D-arrays (vectors):

      if (.not. allocated (sigma))       allocate (sigma(0:km+1))
      if (.not. allocated (sigmal))      allocate (sigmal(km+1))
      if (.not. allocated (delta_sigma)) allocate (delta_sigma(0:km+1))
      if (.not. allocated (delta_sigmal)) allocate (delta_sigmal(km+1))

!     Applied 2D-arrays:
      if (.not. allocated (topo))   allocate (topo(0:im+1,0:jm+1))
      if (.not. allocated (topou))  allocate (topou(0:im,jm))
      if (.not. allocated (topov))  allocate (topov(im,0:jm))
      if (.not. allocated (depth))  allocate (depth(0:im+1,0:jm+1))
      if (.not. allocated (depthu)) allocate (depthu(0:im,jm))
      if (.not. allocated (depthv)) allocate (depthv(im,0:jm))

!_LHS_Aug2007_Start:
!     The vertical grid spacing in the terrain following sigma-
!     coordinates:
      sigma(0) = 0.0

       if (stretch_factor > 0)then

!      Using the user specified first layer thickness and stretch_factor
       sigma(1) = first_layer_depth
       do k = 1,km-1
        sigma(k+1) = stretch_factor*(sigma(k) - sigma(k-1)) + sigma(k)
       end do

       else

!      Using the user specified layer thicknesses
       sigma(1) = user_def_deltasigma(1)
       do k = 2,km
        sigma(k) = sigma(k-1) + user_def_deltasigma(k)
       end do

      endif

       sigma(km+1) = sigma(km) + (sigma(km) - sigma(km-1))

!_LHS_Aug2007_End.
!
!     Defining artificial vertical layers below and above the lowermost
!     and uppermost layers, with equal thickness as these layers,
!     respectively, we get:
      do k = 1,km+1
        sigmal(k)      = 0.5*(sigma(k) + sigma(k-1))
        delta_sigma(k) = sigma(k) - sigma(k-1)
      enddo

      delta_sigma(0)  = delta_sigma(1)
      delta_sigmal(1) = delta_sigma(1)

      do k = 2,km
        delta_sigmal(k) = sigmal(k) - sigmal(k-1)
      enddo

      delta_sigma(km+1)  = delta_sigma(km)
      delta_sigmal(km+1) = delta_sigma(km)

!     The maximum model height in meters is thus:
      h0 = sigma(km)

        if (fe_log) then
         write(funit_log,*)
        do k = 1,km+1
          write(funit_log,'(1X,A5,I3,A17,F8.2)') ' k = ',k,'     sigmal(k) = ',sigmal(k)
        end do
        write(funit_log,*)
        do k = 1,km+1
          write(funit_log,'(1X,A5,I3,A17,F8.2)') ' k = ',k,'      sigma(k) = ',sigma(k)
        end do
        end if

! **********************************************************************


! *** Read the filename of the topography file:
      fe_in_top = .false.
      if (fname_in_top(1:1) /= ' ') then
        call opifil(fname_in_top,funit_in_top,fe_in_top,fm_in_top)
        if (.not. allocated (in_top)) allocate (in_top(im,jm))

        call r2dfld(funit_in_top,fm_in_top,txt1,txt2,im,jm,in_top)

        call clifil(funit_in_top)
      end if


!     Get the gridded model topography and compute the model depth:
      call get_model_depth

! **********************************************************************

!     Define the model depth at the u-points by linear interpolation:
      do j = 1,jm
        do i = 0,im
          topou(i,j)  = 0.5*(topo(i,j)  + topo(i+1,j))
          depthu(i,j) = 0.5*(depth(i,j) + depth(i+1,j))

        end do
      end do

!     Define the model depth at the v-points by linear interpolation:
      do i = 1,im
        do j = 0,jm
          topov(i,j)  = 0.5*(topo(i,j)  + topo(i,j+1))
          depthv(i,j) = 0.5*(depth(i,j) + depth(i,j+1))
        end do
      end do

!     Write some essential information about vertical model depth and
!     vertical grid spacing to the log-file:
        if (fe_log) then
        write(funit_log,*)
        write(funit_log,'(1X,A)')      &
           'The distance from the lowermost point of the ground to the'
        write(funit_log,'(1X,A,F8.2,A)')   &
            'horizontal upper model boundary is: H0 = ',h0,' meters.'
        write(funit_log,*)
        write(funit_log,'(A13,F14.5,3I4)')     &
            ' Max Depth : ',MAXVAL(depth),MAXLOC(depth)
        write(funit_log,'(A13,F14.5,3I4)')     &
            ' Min Depth : ',MINVAL(depth),MINLOC(depth)
        write(funit_log,'(A13,F14.5,3I4)')     &
            ' Max Topo  : ',MAXVAL(topo),MAXLOC(topo)
        write(funit_log,*)
        write(funit_log,*)
        write(funit_log,*) 'Delta_sigma(0)    = ',delta_sigma(0)
        write(funit_log,*) 'Delta_sigma(1)    = ',delta_sigma(1)
        write(funit_log,*) 'Delta_sigma(km)   = ',delta_sigma(km)
        write(funit_log,*) 'Delta_sigma(km+1) = ',delta_sigma(km+1)
        write(funit_log,*)
        write(funit_log,*) 'Delta_sigmal(1)    = ',delta_sigmal(1)
        write(funit_log,*) 'Delta_sigmal(km)   = ',delta_sigmal(km)
        write(funit_log,*) 'Delta_sigmal(km+1) = ',delta_sigmal(km+1)
        write(funit_log,*)

      end if

      return
      end subroutine construct_domain
