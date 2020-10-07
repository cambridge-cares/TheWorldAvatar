! <Closest_Pair.for - A component of the City-scale
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

module Closest_Pair

  use Points_Module
  use Order_By_XY
  use Qsort_Module

  implicit none
 
  private :: closest_pair_real
 
contains
 
  function closest_pair_simple(p, pair) result(distance)
    type(point), dimension(:), intent(in) :: p
    type(point), dimension(:), intent(out), optional :: pair
    real :: distance
 
    type(point), dimension(2) :: cp
    integer :: i, j
    real :: d
 
    if ( size(P) < 2 ) then
       distance = huge(0.0)
    else
       distance = len(p(1) - p(2))
       cp = (/ p(1), p(2) /)
       do i = 1, size(p) - 1
          do j = i+1, size(p)
             d = len(p(i) - p(j))
             if ( d < distance ) then
                distance = d
                cp = (/ p(i), p(j) /)
             end if
          end do
       end do
       if ( present(pair) ) pair = cp
    end if
  end function closest_pair_simple
 
 
  function closest_pair_generic(p, pair) result(distance)
    type(point), dimension(:), intent(in) :: p
    type(point), dimension(:), intent(out), optional :: pair

    real :: distance
 
    type(point), dimension(2) :: dp
    type(point), dimension(size(p)) :: xp, yp
    integer :: i
 
    xp = p
    yp = p

 
    call qsort(xp, order_by_x)
    call qsort(yp, order_by_y)
 
    distance = closest_pair_real(xp, yp, dp)
    if ( present(pair) ) pair = dp
  end function closest_pair_generic
 
 
  recursive function closest_pair_real(xp, yp, pair) result(distance)
    type(point), dimension(:), intent(in) :: xp, yp
    type(point), dimension(:), intent(out) :: pair
    real :: distance
 
    type(point), dimension(2) :: pairl, pairr
    type(point), dimension(:), allocatable :: ys
    type(point), dimension(:), allocatable :: pl, yl
    type(point), dimension(:), allocatable :: pr, yr
    real :: dl, dr, dmin, xm, d
    integer :: ns, i, k, j, midx

 
    if ( size(xp) <= 3 ) then
       distance = closest_pair_simple(xp, pair)
    else
       midx = ceiling(size(xp)/2.0)
 
       allocate(ys(size(xp)))
       allocate(pl(midx), yl(midx))
       allocate(pr(size(xp)-midx), yr(size(xp)-midx))
 
       pl = xp(1:midx)
       pr = xp((midx+1):size(xp))
 
       xm = xp(midx)%x
 
       k = 1; j = 1
       do i = 1, size(yp)
          if ( yp(i)%x > xm ) then
             ! guard ring; this is an "idiosyncrasy" that should be fixed in a
             ! smarter way
             if ( k <= size(yr) ) yr(k) = yp(i)
             k = k + 1
          else
             ! guard ring (see above)
             if ( j <= size(yl) ) yl(j) = yp(i)
             j = j + 1
          end if
       end do
 
       dl = closest_pair_real(pl, yl, pairl)
       dr = closest_pair_real(pr, yr, pairr)
 
       pair = pairr
       dmin = dr
       if ( dl < dr ) then
          pair = pairl
          dmin = dl
       end if
 
       ns = 0
       do i = 1, size(yp)
          if ( abs(yp(i)%x - xm) < dmin ) then
             ns = ns + 1
             ys(ns) = yp(i)
          end if
       end do
 
       distance = dmin
       do i = 1, ns-1
          k = i + 1
          do while ( k <= ns .and. abs(ys(k)%y - ys(i)%y) < dmin )
             d = len(ys(k) - ys(i))
             if ( d < distance ) then
                distance = d
                pair = (/ ys(k), ys(i) /)
             end if
             k = k + 1
          end do
       end do
       deallocate(ys)
       deallocate(pl, yl)
       deallocate(pr, yr)
    end if
  end function closest_pair_real


end module Closest_Pair
