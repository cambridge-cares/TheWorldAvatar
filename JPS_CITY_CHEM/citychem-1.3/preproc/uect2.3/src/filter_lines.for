! <filter_lines.for - A component of the City-scale
!                 Chemistry Transport Model EPISODE-CityChem>
!*****************************************************************************!
!*
!*        CITY-scale CHEMistry Transport Extension
!*
!*        Copyright (C) 2018  Matthias Steffen Karl
!*
!*        Contact Information: 
!*            Institute of Coastal Research
!*            Helmholtz-Zentrum Geesthacht
!*            Max-Planck-Str. 1
!*            21502 Geesthacht
!*            Germany
!*            email:  matthias.karl@hzg.de
!*
!*      EPISODE-CityChem, developed at Helmholtz-Zentrum Geesthacht (HZG) is designed
!*      for treating complex atmospheric chemistry in urban areas (Karl, 2018). The model
!*      is an extension of the EPISODE dispersion model to enable chemistry/transport
!*      simulations of reactive pollutants on city scale. EPISODE is an Eulerian dispersion
!*      model developed at the Norwegian Institute for Air Research (NILU) appropriate for
!*      air quality studies at the local scale (Slørdal et al. 2003 &2008). The model is an
!*      open source code subject to the Reciprocal Public License ("RPL") Version 1.5,
!*      https://opensource.org/licenses/RPL-1.5.
!*
!*        Reference:
!*      Karl, M. (2018):  Development of the city-scale chemistry transport model 
!*      CityChem-EPISODE and its application to the city of Hamburg, 
!*      Geosci. Model Dev. Discuss.,
!*      https://doi.org/10.5194/gmd-2018-8, 2018.
!*
!*
!*  Unless explicitly acquired and licensed from Licensor under another license,
!*  the contents of this file are subject to the Reciprocal Public License ("RPL")
!*  Version 1.5, https://opensource.org/licenses/RPL-1.5 or subsequent versions as
!*  allowed by the RPL, and You may not copy or use this file in either source code
!*  or executable form, except in compliance with the terms and conditions of the RPL. 
!*
!*  All software distributed under the RPL is provided strictly on an "AS IS" basis, 
!*  WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESS OR IMPLIED, AND LICENSOR HEREBY 
!*  DISCLAIMS ALL SUCH WARRANTIES, INCLUDING WITHOUT LIMITATION, ANY WARRANTIES OF
!*  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, QUIET ENJOYMENT, OR NON-INFRINGEMENT.
!*  See the RPL for specific language governing rights and limitations under the RPL.
!*
!*****************************************************************************!
!
!***********************************************************************
!***
!***      UECT
!***      Urban Emission Conversion Tool
!***
!***********************************************************************

      subroutine filter_lines(incols,nlines,nhours,     &
                           snap,data_array,nlines_new,filter_snap,filter_array) 

!***********************************************************************
!***  Subroutine filters line sources for which start and end point are 
!***  outside of the model domain defined by the user.
!***  A buffer area of 3000m to each side is allowed to include even
!***  some lines of the proximity
!***********************************************************************

!     Declarations of variables by using the MODULES feature:

      use module_uect_exe
      use module_uect_emis
      use module_uect_io

      implicit none

!***********************************************************************

!in
      integer, intent(in)             :: incols
      integer, intent(in)             :: nlines
      integer, intent(in)             :: nhours
      integer, dimension(nlines),intent(in)      :: snap
      real, dimension(nlines,incols),intent(in)  :: data_array
!out
      integer, dimension(nlines),intent(out)     :: filter_snap
      real, dimension(nlines,incols),intent(out) :: filter_array
      integer, intent(out)            :: nlines_new

!     Local declarations:

      integer            :: n,k
      real, parameter    :: buffer = 3000
      real               :: xcor_start,xcor_end
      real               :: ycor_start,ycor_end

!***********************************************************************
!     Content of subroutine:

! *** Set the extent of the model domain
! *** Add a buffer 1000m around the domain

      !!! Grid origo is SW corner !!!
      ! W border in UTM m
      borderw = sitex0 - buffer
      ! S border in UTM m
      borders = sitey0 - buffer
      ! E border in UTM m
      bordere = sitex0 + dxout*n_nx + buffer
      ! N border in UTM m
      bordern = sitey0 + dxout*n_ny + buffer

      ! print *,'filter borders', borderw,borders,bordere,borderw

! *** Now loop through emission input data_array and filter.
! *** Only the points inside the domain are written to
! *** the filter_array
! *** n_points_new is the new number of domain's point sources

! counter for filter array
      k = 1
      nlines_new = 0

      do n = 1, nlines

        ! get line source start coordinates
        xcor_start = data_array(n,1)
        ycor_start = data_array(n,2)
        ! get line source start coordinates
        xcor_end   = data_array(n,3)
        ycor_end   = data_array(n,4)

        if ( (xcor_start>1.e6).or.(xcor_end>1.e6) ) then
          call stopit('Line source UTM x-coordinate has to be without leading digits for UTM zone')
        endif

        if ( ((xcor_start.lt.borderw).and.(xcor_end.lt.borderw)) .or. &
             ((xcor_start.gt.bordere).and.(xcor_end.gt.bordere)) .or. &
             ((ycor_start.lt.borders).and.(ycor_end.lt.borders)) .or. &
             ((ycor_start.gt.bordern).and.(ycor_end.gt.bordern))      ) then
! *** These line sources will not be included
          if (fe_log) then
            write(funit_log,'(1X,A78)')  &
              'Line source outside domain - excluded. Input line no., x y(start), x y(end)'
            write(funit_log,'(40X,I5,1X,4F11.1)')  &
               n, xcor_start, ycor_start, xcor_end, ycor_end
          endif
       else
! *** These line sources will be included
          filter_array(k,:) = data_array(n,:)
          filter_snap(k)    = snap(n)
          nlines_new = nlines_new + 1
          k = k + 1
       endif

      enddo


      return


      end subroutine filter_lines
