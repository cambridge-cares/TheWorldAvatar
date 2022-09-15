! <Qsort_Module.for - A component of the City-scale
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
!
!* *****************************************************************
!* Closest-pair problem/Fortran                                    *
!* http://rosettacode.org/wiki/Closest-pair_problem/Fortran        *
!* *****************************************************************

module Qsort_Module

  use Points_Module

  implicit none
 
contains
 
  recursive subroutine qsort(a, comparator)
    type(point), intent(inout) :: a(:)
    interface
       integer function comparator(aa, bb)
         use Points_Module
         type(point), intent(in) :: aa, bb
       end function comparator
    end interface
 
    integer :: split
 
    if(size(a) > 1) then
       call partition(a, split, comparator)
       call qsort(a(:split-1), comparator)
       call qsort(a(split:), comparator)
    end if
 
  end subroutine qsort
 
  subroutine partition(a, marker, comparator)
    type(point), intent(inout) :: a(:)
    integer, intent(out) :: marker
 
    interface
       integer function comparator(aa, bb)
         use Points_Module
         type(point), intent(in) :: aa, bb
       end function comparator
    end interface
 
    type(point) :: pivot, temp
    integer :: left, right
 
    left = 0                       
    right = size(a) + 1
    pivot = a(size(a)/2)
 
    do while (left < right)
       right = right - 1
       do while (comparator(a(right), pivot) > 0)
          right = right - 1
       end do
       left = left + 1
       do while (comparator(a(left), pivot) < 0)
          left = left + 1
       end do
       if ( left < right ) then
          temp = a(left)
          a(left) = a(right)
          a(right) = temp
       end if
    end do
 
    if (left == right) then
       marker = left + 1
    else
       marker = left
    end if
  end subroutine partition
 
end module Qsort_Module
