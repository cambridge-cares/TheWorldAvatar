! <filter_areas.for - A component of the City-scale
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

      subroutine filter_areas(incols,nareas,nhours,          &
                           snap,data_array,nareas_new,       &
                           filter_snap,filter_xi,filter_yi,  &
                           filter_array) 

!***********************************************************************
!***  Subroutine filters area sources.
!***  First step is to produce a mask for the domain with the
!***  TestClosestPair program.
!***  Second step is to filter the area sources indicated as
!***  being outside the model domain
!***  The routine also does an aggregation of smaller area parts
!***  into the model grid cells.
!***********************************************************************

!     Declarations of variables by using the MODULES feature:

      use module_uect_exe
      use module_uect_emis
      use module_uect_io

      use Closest_Pair

      implicit none

!***********************************************************************

!in
      integer, intent(in)             :: incols
      integer, intent(in)             :: nareas
      integer, intent(in)             :: nhours
      integer, dimension(nareas),intent(in)      :: snap
      real, dimension(nareas,incols),intent(in)  :: data_array
!out
      integer, dimension(nareas),intent(out)     :: filter_snap
      real, dimension(nareas,incols),intent(out) :: filter_array
      integer, dimension(nareas), intent(out)    :: filter_xi
      integer, dimension(nareas), intent(out)    :: filter_yi
      integer, intent(out)                       :: nareas_new

!     Local declarations:

      integer            :: n,k
      integer            :: i,ix,iy
      integer            :: t
      integer            :: ai1,ai2
      integer            :: lenc

      real               :: xcor_start,xcor_end
      real               :: ycor_start,ycor_end
      real               :: dp,dr
      real               :: limd

      type(point), dimension(:), allocatable :: points 

      type(point), dimension(2) :: p

      integer,dimension(nareas) :: aind
      integer,dimension(nareas) :: xi
      integer,dimension(nareas) :: yi

!***********************************************************************
!     Content of subroutine:

! ***  PART 1

! *** Set the extent of the model domain
! *** Add a buffer dxarea (TAPM point distance m)

       !!! Grid origo is SW corner !!!
       ! W border in UTM m
       borderw = sitex0 - dxarea
       ! S border in UTM m
       borders = sitey0 - dxarea
       ! E border in UTM m
       bordere = sitex0 + dxout*n_nx + dxarea
       ! N border in UTM m
       bordern = sitey0 + dxout*n_ny + dxarea

       !print *,'filter borders', borderw,borders,bordere,borderw

! ***   dxout is EPISODE grid distance dx m
       limd = dxout + dxout/5

       !print *,'test', borderw,borders,bordere,bordern,limd

! ***   ALLOCATE
       if (.not. allocated(points))      allocate( points(n_nx*n_ny+1) )


! ***  Construct the CityChem model grid, point by point
! ***  and write into points pair array 

       i = 1
       do ix = 1, n_nx
         do iy = 1, n_ny
           points(i)%x = sitex0 + (ix-1)*dxout
           points(i)%y = sitey0 + (iy-1)*dxout
           !print *,i, points(i)
           i = i + 1
         enddo
       enddo


! ***   now read the grid points from input "area_src_file" file
       do t = 1, n_soaa

! ***   check x-coordinates of the area source
           if ( (data_array(t,1)>1.e6).or.(data_array(t,4)>1.e6) ) then
             call stopit('Area source UTM x-coordinate has to be without leading digits for UTM zone')
           endif


           aind(t) = 0

           ! Mid-Point coordinate
           points(n_nx*n_ny+1)%x = data_array(t,1) + 0.5*dxarea

           points(n_nx*n_ny+1)%y = data_array(t,2) + 0.5*dxarea

           print *, "new point", points(n_nx*n_ny+1)

! *** exclude the point if outside of the model domain
           if ( (points(n_nx*n_ny+1)%x .lt. borderw ) .or. &
                (points(n_nx*n_ny+1)%x .gt. bordere ) .or. &
                (points(n_nx*n_ny+1)%y .lt. borders ) .or. &
                (points(n_nx*n_ny+1)%y .gt. bordern )         ) then

              aind(t) = 0

           else

             ! Simple closest pair works best for this purpose
             dp = closest_pair_simple(points, p)
             !print *, "sim ", dp

             if (dp .le. limd) then

               ai1 = 0
               ai2 = 0
               do i = 1, (n_nx*n_ny)
                 if ((p(1)%x.eq.points(i)%x).and.(p(1)%y.eq.points(i)%y)) then
                   ai1=i
                 endif
                 if ((p(2)%x.eq.points(i)%x).and.(p(2)%y.eq.points(i)%y)) then
                   ai2=i
                 endif
               end do
               if (  (ai1>0).and.(ai2>0).and.(ai1.ne.ai2) ) then   ! error
                 print *,"WARNING: two different indices ai1 ai2: ",ai1,ai2
                 print *,"p ",p(1)%x,p(1)%y,p(2)%x,p(2)%y
                 print *,"Point will not be included."
                 if (fe_log) then
                   write(funit_log,'(1X,A56,I5,I5)')  &
                   'Warning: two different indices ai1 ai2: ',ai1,ai2
                   write(funit_log,'(1X,A56,1X,I5,4F11.1)')  &
                   'Point will not be included: ',t,p(1)%x,p(1)%y,p(2)%x,p(2)%y
                 endif
                 aind(t) = 0
               else
                 if (ai1>0) aind(t)=ai1
                 if (ai2>0) aind(t)=ai2
               endif

!*** point somewhere close but outside
             else            
                print *,"Point not close enough to any grid cell:"
                print *, t,  dp, points(n_nx*n_ny+1)%x, points(n_nx*n_ny+1)%y
                 if (fe_log) then
                   write(funit_log,'(1X,A56,I5,F5.1,2F11.1)')  &
                   'Point not close enough to any grid cell:',   &
                     t,dp,points(n_nx*n_ny+1)%x, points(n_nx*n_ny+1)%y
                 endif
                aind(t) = 0
             endif

           endif

       enddo

       if (allocated(points) )      deallocate(points )

! ***  PART 2

! *** Now loop through emission input data_array and filter
! *** using the area mask (index aind)
! *** Only the points inside the domain are written to
! *** the filter_array
! *** nareas_new is the new number of domain's area sources

       do n = 1, nareas

          if ( aind(n) == 0 ) then
             xi(n) = 0
             yi(n) = 0
          else

             lenc = 0

! *** Outer loop over x-coordinate seems to be ok.
             do ix = 1,n_nx
                do iy = 1,n_ny
                  lenc = lenc +1
                  if ( lenc == aind(n) ) then
                    xi(n) = ix
                    yi(n) = iy
                  endif
                enddo
             enddo

          endif    
          !print *,'xi yi ', n, xi(n), yi(n)

       enddo


! MAYBE THE XI AND YI HAS BE ADDED TO FILTER_ARRAY???

! counter for filter array
       k = 1
       nareas_new = 0
       do n = 1, nareas

          if ( (xi(n).gt.0).and.(yi(n).gt.0) ) then

! *** These line sources will be included
            filter_array(k,:) = data_array(n,:)
            filter_snap(k)    = snap(n)
            filter_xi(k)      = xi(n)
            filter_yi(k)      = yi(n)
            nareas_new = nareas_new + 1
            k = k + 1

          else

! *** These line sources will not be included
            if (fe_log) then
               write(funit_log,'(1X,A78)')  &
                'Area source outside domain - excluded. Input area no., x y(SW), x y(NE)'
               write(funit_log,'(40X,I5,1X,4F11.1)')  &
                 n,data_array(n,1),data_array(n,2),data_array(n,4),data_array(n,5)
            endif
          endif

       enddo


      return


      end subroutine filter_areas
