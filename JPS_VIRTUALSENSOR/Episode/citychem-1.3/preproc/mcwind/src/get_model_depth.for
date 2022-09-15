! <get_model_depth.for - A component of the City-scale
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

      subroutine get_model_depth

!***********************************************************************
!
! This routine either reads the topography from some external source
! or calculates some idealized topography for test purposes,
! and calculates the model-depth.

! Modifications:
!    29 Jan 2019  M. Karl: Commented pre-processor directive 'main_program'
!    29 Jan 2019  M. Karl: replaced TAB by spaces
!
!***********************************************************************
! If needed, insert the necessary MODULE calls:
!
      use module_mc_wind_files
      use module_mc_wind_domain
!
!***********************************************************************

! *** We require the use of: implicit none:
      implicit none

! *** Local declarations:
      integer :: i,j
      real    :: min_topo
      real    :: topo_scale

!MSK!DEC$ IF DEFINED (main_program)

      integer :: k
      integer :: iant_smooth,i_smooth_formula
      real    :: pi,x,y,xlength,ylength
      real    :: bigD
      real    :: wsm_sum
      real    :: wsm(1:25)

      real, allocatable :: sm_topo(:,:)

      if (.not. allocated (sm_topo))  &
            allocate (sm_topo(0:im+1,0:jm+1))

! *** Content of the routine:

      if (fe_in_top) then

        min_topo   = MINVAL(in_top)
        topo_scale = 1.0

        do i = 1,im
          do j = 1,jm
            topo(i,j) = (in_top(i,j) - min_topo) * topo_scale
          end do
        end do

        else

! ***   If an idealized topography is needed, it is computed below:
        pi = 2.0 * ASIN(1.0)

        xlength = (im-1)*dx
        ylength = (jm-1)*dy

        topo = 0.0

        do i = 2,im-1
          do j = 2,jm-1
            x = (i-1)*dx
            y = (j-1)*dy
                topo(i,j) = SIN((pi*x)/xlength)*SIN((pi*y)/ylength)* &
                       elev_max
          end do
        end do

      end if  ! (fe_in_top)

!MSK!DEC$ ELSE
!
! ***   DLL_Version:
!
!        min_topo   = MINVAL(in_top)
!        topo_scale = 1.0
!
!        do i = 1,im
!          do j = 1,jm
!            topo(i,j) = (in_top(i,j) - min_topo) * topo_scale
!          end do
!        end do
!
!MSK!DEC$ ENDIF

! *** If the topography is read from some external source we must
! *** decide on what to do at the boundaries. If the available
! *** topography just covers the inner domain (i=1,im , j=1,jm) then
! *** we must define the outermost values, i.e topo(i,j) for i=0,
! *** i=im+1, j=0 and j=jm+1. Below this is done by setting these
! *** values equal to the neighbour inner domain point:
      do j=1,jm
        topo(0,j)    = topo(1,j)
        topo(im+1,j) = topo(im,j)
      end do
      do i=0,im+1
        topo(i,0)    = topo(i,1)
        topo(i,jm+1) = topo(i,jm)
      end do

!MSK!DEC$ IF DEFINED (main_program)

! ----------------------------------------------------------------------
! *** Smoothing the topography:
      iant_smooth = 1

      i_smooth_formula = 1
!      i_smooth_formula = 5
!      i_smooth_formula = 8
!       i_smooth_formula = 13
!      i_smooth_formula = 24

      wsm = 0.0
      wsm_sum = 0.0

      if ( i_smooth_formula == 1 ) then

        wsm(13) = 1.0

      else if (i_smooth_formula == 5 ) then
        wsm(8)  = 1.0/8.0
        wsm(12) = 1.0/8.0
        wsm(13) = 1.0/2.0
        wsm(14) = 1.0/8.0
        wsm(18) = 1.0/8.0

      else if ( i_smooth_formula == 13 ) then

        wsm(3)  = 1.0/24.0
        wsm(7)  = 1.0/24.0
        wsm(8)  = 1.0/12.0
        wsm(9)  = 1.0/24.0
        wsm(11) = 1.0/24.0
        wsm(12) = 1.0/12.0
        wsm(13) = 1.0/3.0
        wsm(14) = 1.0/12.0
        wsm(15) = 1.0/24.0
        wsm(17) = 1.0/24.0
        wsm(18) = 1.0/12.0
        wsm(19) = 1.0/24.0
        wsm(23) = 1.0/24.0

      else if ( i_smooth_formula == 8 ) then

        bigD   = 1.0/(1.5*dx)

        wsm(1) = dx*SQRT(1.25)
        wsm(1) = 1.0*EXP(-wsm(1)*bigD)
        wsm(3) = 1.5*dx
        wsm(3) = 1.0*EXP(-wsm(3)*bigD)
        wsm(4) = 0.5*dx
        wsm(4) = 1.0*EXP(-wsm(4)*bigD)

        wsm(2) = wsm(1)
        wsm(5) = wsm(4)
        wsm(6) = wsm(3)
        wsm(7) = wsm(1)
        wsm(8) = wsm(1)

      else if ( i_smooth_formula == 24 ) then

        bigD   = 1.0/(2.0*dx)

        wsm(1) = SQRT(2.5*dx)
        wsm(1) = 1.0*EXP(-wsm(1)*bigD)
        wsm(2) = 1.5*dx
        wsm(2) = 1.0*EXP(-wsm(2)*bigD)
        wsm(5) = SQRT(1.5*dx)
        wsm(5) = 1.0*EXP(-wsm(5)*bigD)
        wsm(9) = dx
        wsm(9) = 1.0*EXP(-wsm(9)*bigD)

        wsm(3)  = wsm(1)
        wsm(4)  = wsm(1)
        wsm(7)  = wsm(1)
        wsm(18) = wsm(1)
        wsm(21) = wsm(1)
        wsm(22) = wsm(1)
        wsm(24) = wsm(1)

        wsm(11) = wsm(2)
        wsm(14) = wsm(2)
        wsm(23) = wsm(2)

        wsm(6)  = wsm(5)
        wsm(8)  = wsm(5)
        wsm(10) = wsm(5)
        wsm(15) = wsm(5)
        wsm(17) = wsm(5)
        wsm(19) = wsm(5)
        wsm(20) = wsm(5)

        wsm(12) = wsm(9)
        wsm(13) = wsm(9)
        wsm(16) = wsm(9)

      end if

      do k = 1,25
        wsm_sum = wsm_sum + wsm(k)
      end do
      do k = 1,25
        wsm(k) = wsm(k)/wsm_sum
      end do

      do k = 1,25
        print *,' wsm(k) = ',wsm(k),' for k = ',k
      end do


      do k = 1,iant_smooth

        if (i_smooth_formula == 1 .OR. i_smooth_formula == 5 .OR. &
       i_smooth_formula ==13)then

          do i = 2,im-1
            do j = 2,jm-1
              sm_topo(i,j) =  &
                    wsm(1) *topo(i-2,j+2)  + wsm(2) *topo(i-1,j+2) &
                  + wsm(3) *topo(i,j+2)    + wsm(4) *topo(i+1,j+2) &
                  + wsm(5) *topo(i+2,j+2)  + wsm(6) *topo(i-2,j+1) &
                  + wsm(7) *topo(i-1,j+1)  + wsm(8) *topo(i,j+1) &
                  + wsm(9) *topo(i+1,j+1)  + wsm(10)*topo(i+2,j+1) &
                  + wsm(11)*topo(i-2,j)    + wsm(12)*topo(i-1,j) &
                  + wsm(13)*topo(i,j)      + wsm(14)*topo(i+1,j) &
                  + wsm(15)*topo(i+2,j)    + wsm(16)*topo(i-2,j-1) &
                  + wsm(17)*topo(i-1,j-1)  + wsm(18)*topo(i,j-1) &
                  + wsm(19)*topo(i+1,j-1)  + wsm(20)*topo(i+2,j-1) &
                  + wsm(21)*topo(i-2,j-2)  + wsm(22)*topo(i-1,j-2) &
                  + wsm(23)*topo(i,j-2)    + wsm(24)*topo(i+1,j-2) &
                  + wsm(25)*topo(i+2,j-2)
            end do
          end do

        else if(i_smooth_formula == 24) then

          do i = 2,im-1
            do j = 2,jm-1
              sm_topo(i,j) =  &
                    wsm(1)  * 0.5*(topo(i-1,j+2) + topo(i-1,j+1)) &
                  + wsm(2)  * 0.5*(topo(i,j+2)   + topo(i,j+1)) &
                  + wsm(3)  * 0.5*(topo(i+1,j+2) + topo(i+1,j+1)) &
                  + wsm(4)  * 0.5*(topo(i-2,j+1) + topo(i-1,j+1)) &
                  + wsm(5)  * 0.5*(topo(i-1,j+1) + topo(i,j+1)) &
                  + wsm(6)  * 0.5*(topo(i,j+1)   + topo(i+1,j+1)) &
                  + wsm(7)  * 0.5*(topo(i+1,j+1) + topo(i+2,j+1)) &
                  + wsm(8)  * 0.5*(topo(i-1,j+1) + topo(i-1,j)) &
                  + wsm(9)  * 0.5*(topo(i,j+1)   + topo(i,j)) &
                  + wsm(10) * 0.5*(topo(i+1,j+1) + topo(i+1,j)) &
                  + wsm(11) * 0.5*(topo(i-2,j)   + topo(i-1,j)) &
                  + wsm(12) * 0.5*(topo(i-1,j)   + topo(i,j)) &
                  + wsm(13) * 0.5*(topo(i,j)     + topo(i+1,j)) &
                  + wsm(14) * 0.5*(topo(i+1,j)   + topo(i+2,j)) &
                  + wsm(15) * 0.5*(topo(i-1,j)   + topo(i-1,j-1)) &
                  + wsm(16) * 0.5*(topo(i,j)     + topo(i,j-1)) &
                  + wsm(17) * 0.5*(topo(i+1,j)   + topo(i+1,j-1)) &
                  + wsm(18) * 0.5*(topo(i-2,j-1) + topo(i-1,j-1)) &
                  + wsm(19) * 0.5*(topo(i-1,j-1) + topo(i,j-1)) &
                  + wsm(20) * 0.5*(topo(i,j-1)   + topo(i+1,j-1)) &
                  + wsm(21) * 0.5*(topo(i+1,j-1) + topo(i+2,j-1)) &
                  + wsm(22) * 0.5*(topo(i-1,j-1) + topo(i-1,j-2)) &
                  + wsm(23) * 0.5*(topo(i,j-1)   + topo(i,j-2)) &
                  + wsm(24) * 0.5*(topo(i+1,j-1) + topo(i+1,j-2))
            end do
          end do

        else if(i_smooth_formula == 8) then

          do i = 1,im-1
            do j = 1,jm
              topou(i,j) =  wsm(1) * topo(i,  j+1) &
                     + wsm(2) * topo(i+1,j+1) &
                     + wsm(3) * topo(i-1,j  ) &
                     + wsm(4) * topo(i  ,j  ) &
                     + wsm(5) * topo(i+1,j  ) &
                     + wsm(6) * topo(i+2,j  ) &
                     + wsm(7) * topo(i  ,j-1) &
                     + wsm(8) * topo(i+1,j-1)
            end do
          end do

          do j = 1,im
            topou(0,j)  = topou(i,1)
            topou(im,j) = topou(i,im-1)
          end do

          do i = 1,im
            do j = 1,jm-1
              topov(i,j) =  wsm(1) * topo(i-1,j+1) &
                     + wsm(2) * topo(i+1,j+1) &
                     + wsm(3) * topo(i  ,j+2) &
                     + wsm(4) * topo(i  ,j+1) &
                     + wsm(5) * topo(i  ,j  ) &
                     + wsm(6) * topo(i  ,j-1) &
                     + wsm(7) * topo(i-1,j  ) &
                     + wsm(8) * topo(i+1,j  )
            end do
          end do

          do i = 1,im
            topov(i,0)  = topov(i,1)
            topov(i,im) = topov(i,im-1)
          end do

        end if


        do i = 2,im-1
         do j = 2,jm-1
           topo(i,j) = sm_topo(i,j)
         end do
        end do

      end do ! k = 1,iant_smooth

! *** Deallocate local variables:
      if (allocated (sm_topo)) deallocate (sm_topo)
! ----------------------------------------------------------------------

!MSK!DEC$ ENDIF

! Define the model depth: "depth(0:im+1,0:jm+1)" from the
! maximum model depth, "h0", and the topography: "topo(0:im+1,0:jm+1)":


      do j = 0,jm+1
        do i = 0,im+1
          depth(i,j) = h0 - topo(i,j)
        end do
      end do

      return
      end subroutine get_model_depth
