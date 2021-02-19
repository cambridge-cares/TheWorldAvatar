! <Interpol_Module.for - A component of the City-scale
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

!* *****************************************************************
!* Collection of interpolation tools                               *
!* *****************************************************************

module Interpol_Module

      use module_uect_exe


  implicit none

   TYPE proj_info
!      INTEGER          :: nlat     ! For Gaussian -- number of latitude points 
!                                   !  north of the equator 
!      INTEGER          :: nlon     !
!                                   !
      INTEGER          :: ixdim    ! For Rotated Lat/Lon -- number of mass points
                                   !  in an odd row
      INTEGER          :: jydim    ! For Rotated Lat/Lon -- number of rows
!      INTEGER          :: stagger  ! For Rotated Lat/Lon -- mass or velocity grid 
!      REAL             :: phi      ! For Rotated Lat/Lon -- domain half-extent in 
!!                                   !  degrees latitude
!      REAL             :: lambda   ! For Rotated Lat/Lon -- domain half-extend in
!                                   !  degrees longitude
      REAL             :: lat1     ! SW latitude (1,1) in degrees (-90->90N)
      REAL             :: lon1     ! SW longitude (1,1) in degrees (-180->180E)
!      REAL             :: lat0     ! For Cassini, latitude of projection pole
!      REAL             :: lon0     ! For Cassini, longitude of projection pole
      REAL             :: dx       ! Grid spacing in meters at truelats, used
                                   !  only for ps, lc, and merc projections
!      REAL             :: dy       ! Grid spacing in meters at truelats, used
!                                   !  only for ps, lc, and merc projections
!      REAL             :: latinc   ! Latitude increment for cylindrical lat/lon
!      REAL             :: loninc   ! Longitude increment for cylindrical lat/lon
!                                   !  also the lon increment for Gaussian grid
!      REAL             :: dlat     ! Lat increment for lat/lon grids
      REAL             :: dlon     ! Lon increment for lat/lon grids
!      REAL             :: stdlon   ! Longitude parallel to y-axis (-180->180E)
      REAL             :: truelat1 ! First true latitude (all projections)
!      REAL             :: hemi     ! 1 for NH, -1 for SH
!      REAL             :: polei    ! Computed i-location of pole point
!      REAL             :: polej    ! Computed j-location of pole point
      REAL             :: rsw      ! Computed radius to SW corner
!      REAL             :: rebydx   ! Earth radius divided by dx
      REAL             :: knowni   ! X-location of known lat/lon
      REAL             :: knownj   ! Y-location of known lat/lon
      REAL             :: re_m     ! Radius of spherical earth, meters
!      LOGICAL          :: init     ! Flag to indicate if this struct is 
!                                   !  ready for use
!      LOGICAL          :: wrap     ! For Gaussian -- flag to indicate wrapping 
!                                   !  around globe?
!      LOGICAL          :: comp_ll  ! Work in computational lat/lon space for Cassini
   END TYPE proj_info

      TYPE(proj_info) :: proj

      real, parameter :: PI = 3.141592653589793
      real, parameter :: OMEGA_E = 7.292e-5 ! Angular rotation rate of the earth
      real, parameter :: DEG_PER_RAD = 180./PI
      real, parameter :: RAD_PER_DEG = PI/180.
      real, parameter :: EARTH_RADIUS_M = 6370000.   ! same as MM5 system

contains


      subroutine area_interpolate(nlon,nlat,lon_edge,lat_edge, &
                                  megan_data,wrk_data_out )

!----------------------------------------------------------------------
!
!****
!
!      purpose
!      -------
!      Area interpolation from regular lon-lat grid to various
!      projections for given cell widths.
!      The UTM projection has to be added!
!
!      interface
!      ---------
!      input field: megan_data
!      interpolated output field: wrk_data_out
!
!
!      method
!      ------
!      The routine area_interpolation is taken from area_mapper.f90
!      which is part of the MEGAN preprocessing tool  written for
!      WRF-Chem by Gabriele Pfister, Stacy Walters, and Xiaoyan Jiang
!      of NCAR.  
!
!      external
!      --------
!      none
!
!      reference
!      ---------
!      WRF-Chem, MEGAN v2.1
!
!
!------------------------------------------------------------------

!     Declarations of variables by using the MODULES feature:

      use module_uect_emis


      implicit none

!***********************************************************************

! *** IN
        integer, intent(in)             :: nlon
        integer, intent(in)             :: nlat
        real(8), dimension(nlon+1),intent(in)  :: lon_edge
        real(8), dimension(nlat+1),intent(in)  :: lat_edge
        double precision, dimension(nlon,nlat),intent(in)  :: megan_data

! *** OUT
        double precision, dimension(n_nx,n_ny),intent(out) :: wrk_data_out

! *** local computations
        integer             :: i,j,l
        integer             :: ix,iy,jx,jy
        integer             :: isone
        integer             :: istat
        integer             :: mlat_ndx, mlon_ndx
        integer             :: m_lat_s, m_lon_s
        integer             :: m_lat_e, m_lon_e
        integer             :: lon_s, lon_e
        integer             :: lat_s, lat_e
        integer             :: cnt_dvtx_n_mcell
        integer             :: dcell_ndx
        integer             :: dlong_ndx
        !integer             :: dcell_cnt

        real                :: clain
        real                :: dcell_area
        real                :: wrk_sum
        real                :: target_lon
        real                :: target_lat
        real                :: lon2, lat2
        !real                :: mcell_area
        real                :: upper_lon
        real                :: upper_lat

        real, dimension(n_nx,n_ny) :: wrk_data

        real,dimension(4)   :: x
        real,dimension(4)   :: y
        real,dimension(4)   :: model_x
        real,dimension(4)   :: model_y
        real,dimension(4)   :: data_lon
        real,dimension(4)   :: data_lat
        real,dimension(2)   :: minmax_x
        real,dimension(2)   :: minmax_y
        real,dimension(2)   :: minmax_lon
        real,dimension(2)   :: minmax_lat
        real,dimension(2,2) :: model_lon
        real,dimension(2,2) :: model_lat

        logical             :: dcell_outside_mcell
        logical             :: x_n_mcell(4)
        logical             :: y_n_mcell(4)

        real, parameter     :: dxef = 100.0
        real, allocatable   :: city_utme(:,:) 
        real, allocatable   :: city_utmn(:,:) 


!***********************************************************************
!     Content of subroutine:

      if (.not. allocated(city_utme) )      allocate( city_utme(n_nx+1,n_ny+1) )
      if (.not. allocated(city_utmn) )      allocate( city_utmn(n_nx+1,n_ny+1) )


!***  Input is the lat_edge and the lon_edge vectors of the EF map

         !print *,'lat_edge',lat_edge(:)

!***  Construct the target (=city) domain
!***  First using the user-provided UTM coordinates,
!***  uses the sw corner as grid point.

      do iy = 1, n_ny+1
        do ix = 1, n_nx+1

          city_utme(ix,iy) = sitex0 + (ix-1)*dxout 
          city_utmn(ix,iy) = sitey0 + (iy-1)*dxout

          !print *,'city grid:',ix,iy,  city_utme(ix,iy), city_utmn(ix,iy)

        enddo
      enddo


!***  Get utm zone as integer & SW corner lon/lat coordinate

      call str2int(utmzone(1:2),isone,istat)

      call utm2ll(isone,sitey0,sitex0,target_lat,target_lon) 
      call utm2ll(isone,sitey0+dxef,sitex0+dxef,lat2,lon2) 

      print *,'inter',target_lat,target_lon

      call utm2ll(isone,(sitey0+n_ny*dxout),(sitex0+n_nx*dxout),upper_lat,upper_lon) 

      print *,'upper',upper_lat,upper_lon
      !stop

!***  Set up the TYPE proj_info for the city domain
      proj%lon1     = target_lon
      proj%lat1     = target_lat
      proj%truelat1 = target_lat
      proj%re_m     = EARTH_RADIUS_M

      proj%dx       = dxout
      clain         = COS(rad_per_deg*proj%truelat1)
      proj%dlon     = proj%dx / (proj%re_m * clain)

      proj%ixdim  = n_nx+1
      proj%jydim  = n_ny+1
      proj%knowni = 1   !proj%ixdim/2.
      proj%knownj = 1   !proj%jydim/2. 
      proj%rsw = 0.
      IF (proj%lat1 .NE. 0.) THEN
         proj%rsw = (ALOG(TAN(0.5*((proj%lat1+90.)*rad_per_deg))))/proj%dlon
      ENDIF

      print *,'dlon rsw',proj%dlon, proj%rsw


!***  Initialize the model grid loop

!MSK    model_area_type => grid_specs(grid_ndx)%model_area_type
      m_lat_s = 1
      m_lat_e = proj%jydim-1
      m_lon_s = 1
      m_lon_e = proj%ixdim-1

      wrk_data(:,:) = 0.
      !mcell_area = 0.

      print *,'m_lat_e, m_lon_e', m_lat_e, m_lon_e

!***  Start the area interpolation loop over latitude and longitude

model_lat_loop : &
   do mlat_ndx = m_lat_s,m_lat_e
     model_y(1:2) = real(mlat_ndx)  - .5
     model_y(3:4) = real(mlat_ndx)  + .5

model_lon_loop : &
     do mlon_ndx = m_lon_s,m_lon_e
         model_x(1:4:3) = real(mlon_ndx)  -.5
         model_x(2:3)   = real(mlon_ndx)  +.5


!-------------------------------------------------------------
!  map model cell vertices to lat,lon
!-------------------------------------------------------------

!***  Here we do some checking which part of the map is inside the city domain

!-------------------------------------------------------------
!** 1. re-projection of UTM coordinates to latlon
!-------------------------------------------------------------

        !print *,'ml,',mlon_ndx,mlat_ndx

         do j = 1,2
           do i = 1,2        
             call utm2ll(isone,city_utmn(i+mlon_ndx-1,j+mlat_ndx-1)-0.5*dxout,  &
                         city_utme(i+mlon_ndx-1,j+mlat_ndx-1)-0.5*dxout,        &
                         model_lat(i,j),model_lon(i,j))

              !print *,'ll',j,i,i+mlon_ndx-1,j+mlat_ndx-1,model_lat(i,j),model_lon(i,j)
           enddo
         enddo

         minmax_lat(1) = minval( model_lat(:,:) )
         minmax_lat(2) = maxval( model_lat(:,:) )
         minmax_lon(1) = minval( model_lon(:,:) )
         minmax_lon(2) = maxval( model_lon(:,:) )


      !   print *,'after reprj: ',minmax_lat(1),lat_edge(nlat+1),minmax_lat(2),lat_edge(1)

!-------------------------------------------------------------
!** 2. check for model cell out of range of data grid
!-------------------------------------------------------------
         if( minmax_lat(1) >= lat_edge(nlat+1) .or.  minmax_lat(2) <= lat_edge(1) ) then

             print *,'outside of EFmap',minmax_lat(1),lat_edge(nlat+1),minmax_lat(2),lat_edge(1)
             !call stopit('Interpol_Module: BVOC Emission Factors are outside of area')

           cycle model_lon_loop
         endif

! MSK
! skipped the is_new_grid part for now
! I think this makes some longitude-crossing check

!is_new_grid : &
       !if( new_grid ) then
       !  model_area_type(mlon_ndx,mlat_ndx)%has_data = .true.
!-------------------------------------------------------------
!  check for any model grid cell edge "crossing"
!  from positive to negative longitude
!-------------------------------------------------------------
       !  lon_cross(:,1)  = (/ (model_lon(1,1)*model_lon(2,1) < 0. .and. model_lon(1,1) > 0.), &
       !                       (model_lon(2,1)*model_lon(2,2) < 0. .and. model_lon(2,1) > 0.) /)
       !  lon_cross(:,2)  = (/ (model_lon(1,2)*model_lon(1,1) < 0. .and. model_lon(1,2) > 0.), &
       !                       (model_lon(2,2)*model_lon(1,2) < 0. .and. model_lon(2,2) > 0.) /)
       !  lat_mask(:,1)  = (/ model_lat(1,1) /= pole_lat .and. model_lat(2,1) /= pole_lat, &
       !                      model_lat(2,1) /= pole_lat .and. model_lat(2,2) /= pole_lat /)
       !  lat_mask(:,2)  = (/ model_lat(1,2) /= pole_lat .and. model_lat(1,1) /= pole_lat, &
       !                      model_lat(2,2) /= pole_lat .and. model_lat(1,2) /= pole_lat /)
       !  lon_cross(:,:) = lon_cross(:,:) .and. lat_mask(:,:)
       !  has_lon_cross = any( lon_cross(:,:) )
       !
       !  if( has_lon_cross ) then
!cross_loop : &
       !    do j = 1,2
       !      do i = 1,2
       !        if( lon_cross(i,j) ) then
       !          exit cross_loop
       !        endif
       !      end do
       !    end do cross_loop

       !    delta_lat = 0.
       !    delta_lon = 0.
       !    if( i == j ) then
       !      if( j == 1 ) then
       !        delta_lon = .01
       !      else
       !        delta_lon = -.01
       !      endif
       !    else
       !      if( j == 1 ) then
       !        delta_lat = .01
       !      else
       !        delta_lat = -.01
       !      endif
       !    endif
       !    xc = real(i+mlon_ndx-1) - .5 + delta_lon
       !    yc = real(j+mlat_ndx-1) - .5 + delta_lat
       !    call ijll_ps( xc, yc, proj, delta_lat, delta_lon )
!-------------------------------------------------------------
!  check for crossing of date line
!-------------------------------------------------------------
       !    has_lon_cross = delta_lon > model_lon(i,j)
       !    if( has_lon_cross ) then
       !      minmax_lon(1) = maxval( model_lon(:,:),mask=model_lon(:,:)<0. .and. model_lat(:,:)/=pole_lat )
       !      minmax_lon(2) = minval( model_lon(:,:),mask=model_lon(:,:)>0. .and. model_lat(:,:)/=pole_lat )
       !    endif
       !  endif   ! has_lon_cross

!-------------------------------------------------------------
!** find lons,lats of data cells enclosing model cell
!-------------------------------------------------------------

       !  target_lon = minmax_lon(1)
       !  do lon_s = 1,nlon+1
       !    if( lon_edge(lon_s) > target_lon ) then 
       !      exit
       !    endif
       !  enddo
       !  lon_s = min( max( 1,lon_s-1 ),nlon+1 )
       !  target_lon = minmax_lon(2)
       !  do lon_e = 1,nlon+1
       !    if( lon_edge(lon_e) >= target_lon ) then 
       !      exit
       !    endif
       !  enddo
       !  lon_e = min( lon_e,nlon+1)

       !  target_lat = minmax_lat(1)
       !  do lat_s = 1,nlat+1
       !    if( lat_edge(lat_s) > target_lat ) then 
       !      exit
       !    endif
       !  enddo
       !  lat_s = min( max( 1,lat_s-1 ),nlat+1 )
       !  target_lat = minmax_lat(2)
       !  if( target_lat >= lat_edge(nlat+1) ) then
       !    lat_e = nlat
       !  else
       !    do lat_e = lat_s,nlat+1
       !      if( lat_edge(lat_e) >= target_lat ) then 
       !        exit
       !      endif
       !    end do
       !  endif
       !  lat_e = min( lat_e,nlat+1)

!-------------------------------------------------------------
!  check data cell long range for cross over longitude endpoint
!-------------------------------------------------------------
       !  if( has_lon_cross ) then
       !    i = lon_s
       !    lon_s = lon_e
       !    lon_e = i + nlon
       !  endif

       !  model_area_type(mlon_ndx,mlat_ndx)%lon_s = lon_s
       !  model_area_type(mlon_ndx,mlat_ndx)%lon_e = lon_e
       !  model_area_type(mlon_ndx,mlat_ndx)%lat_s = lat_s
       !  model_area_type(mlon_ndx,mlat_ndx)%lat_e = lat_e
       !  dcell_cnt = (lon_e - lon_s + 1)*(lat_e - lat_s + 1)
       !  mcell_area = mcell_area + real(dcell_cnt)

       !  allocate( dcell_partial_lon_ndx(dcell_cnt), dcell_partial_lat_ndx(dcell_cnt), &
       !            partial_wght(dcell_cnt),stat=astat )
       !  if( astat /= 0 ) then
       !    write(*,*) 'area_interp; failed to allocate dcell_partial_lon_ndx ... partial_wght: error = ',astat
       !    stop
       !  endif

       ! else is_new_grid
       !   lon_s = model_area_type(mlon_ndx,mlat_ndx)%lon_s
       !   lon_e = model_area_type(mlon_ndx,mlat_ndx)%lon_e
       !   lat_s = model_area_type(mlon_ndx,mlat_ndx)%lat_s
       !   lat_e = model_area_type(mlon_ndx,mlat_ndx)%lat_e
       ! endif is_new_grid

!-------------------------------------------------------------
!** 3. find lons,lats of data cells enclosing model cell
!-------------------------------------------------------------

         target_lon = minmax_lon(1)
         do lon_s = 1,nlon+1
           if( lon_edge(lon_s) > target_lon ) then
             !print *,'lon_s',lon_s,lon_edge(lon_s)
             exit
           endif
         enddo
         lon_s = min( max( 1,lon_s-1 ),nlon+1 )
         target_lon = minmax_lon(2)
         do lon_e = 1,nlon+1
           if( lon_edge(lon_e) >= target_lon ) then
             !print *,'lon_e',lon_e,lon_edge(lon_e)
             exit
           endif
         enddo
         lon_e = min( lon_e,nlon+1)

         target_lat = minmax_lat(1)
         do lat_s = 1,nlat+1
           if( lat_edge(lat_s) > target_lat ) then
             !print *,'lat_s',lat_s,lat_edge(lat_s)
             exit
           endif
         enddo
         lat_s = min( max( 1,lat_s-1 ),nlat+1 )
         target_lat = minmax_lat(2)
         if( target_lat >= lat_edge(nlat+1) ) then
           lat_e = nlat
         else
           do lat_e = lat_s,nlat+1
             if( lat_edge(lat_e) >= target_lat ) then
               !print *,'lat_e',lat_e,lat_edge(lat_e)
               exit
             endif
           end do
         endif
         lat_e = min( lat_e,nlat+1)

       !  print  *,'lon,lat',lon_s,lon_e,lat_s,lat_e

!-------------------------------------------------------------
!** 3. now we can do the interpolation
!-------------------------------------------------------------
!***  Commented all new_grid stuff
!***  lat_s and lat_e here should correspond to the EFmap (300x300)
!***  data. So for city domain res=1000 this loops 10 times over 
!***  lat and10 times over lon; then sums up the weighted EFmap data 
!***  in the 1x1 km2 cell

         dcell_ndx = 0
         wrk_sum   = 0.

!-------------------------------------------------------------
!  loop over data cells
!-------------------------------------------------------------

         !print *,'lat_s,lat_e',lat_s,lat_e
         !print *,'lon_s,lon_e',lon_s,lon_e

data_lat_loop : &
        do j = lat_s,lat_e-1   ! index range of the EFmap
          data_lat(1) = real(lat_edge(j),kind=4)
          data_lat(2) = data_lat(1)
          data_lat(3) = real(lat_edge(j+1),kind=4)
          data_lat(4) = data_lat(3)

data_lon_loop : &
          do dlong_ndx = lon_s,lon_e-1  ! index range of the EFmap
            i = mod( (dlong_ndx-1),nlon ) + 1
            data_lon(1) = real(lon_edge(i),kind=4)
            data_lon(4) = data_lon(1)
            data_lon(2) = real(lon_edge(i+1),kind=4)
            data_lon(3) = data_lon(2)

          !  print *,'data_lat',minmax_lat(1),data_lat
          !  print *,'data_lon',minmax_lon(1),data_lon

!***  Compute the i/j index from lon/lat data for mercator or other projection of the city domain
!***  real values x and y are the indices of the EFdata domain
!***  assuming that the city domain has Mercator projection

            call llij_merc( real(lat_edge(j),kind=4), real(lon_edge(i),kind=4), proj, x(1), y(1) )
            call llij_merc( real(lat_edge(j),kind=4), real(lon_edge(i+1),kind=4), proj, x(2), y(2) )
            call llij_merc( real(lat_edge(j+1),kind=4), real(lon_edge(i+1),kind=4), proj, x(3), y(3) )
            call llij_merc( real(lat_edge(j+1),kind=4), real(lon_edge(i),kind=4), proj, x(4), y(4) )

            !print *,'x',x(:)
            !print *,'y',y(:)

            minmax_x(1) = minval( x(:) )
            minmax_x(2) = maxval( x(:) )
            minmax_y(1) = minval( y(:) )
            minmax_y(2) = maxval( y(:) )

!***  model_x and model_y are indices of the city domain grid

!MSK            if( new_grid ) then
!MSK              model_area_type(mlon_ndx,mlat_ndx)%total_dcell_cnt = &
!MSK                model_area_type(mlon_ndx,mlat_ndx)%total_dcell_cnt + 1
!MSK            endif
            dcell_outside_mcell = minmax_x(1) >= model_x(2) .or. minmax_x(2) <= model_x(1) .or. &
                                 minmax_y(1) >= model_y(3) .or. minmax_y(2) <= model_y(1)

!debug
!       if( .not. dcell_outside_mcell ) then
!            print *,'minmax_x1',minmax_x(1),model_x(2),i,j,dcell_outside_mcell
!            !print *,'minmax_x2',minmax_x(2),model_x(1)
!            print *,'minmax_y1',minmax_y(1),model_y(3),i,j,dcell_outside_mcell
!            !print *,'minmax_y2',minmax_y(2),model_y(1)
!       endif

      dcell_outside_mcell = .false.

inside_mcell : &
            if( .not. dcell_outside_mcell ) then
!MSK              if( new_grid ) then
!MSK                model_area_type(mlon_ndx,mlat_ndx)%active_dcell_cnt = &
!MSK                  model_area_type(mlon_ndx,mlat_ndx)%active_dcell_cnt + 1
!MSK              endif

              do l = 1,4
                x_n_mcell(l) = model_x(1) <= x(l) .and. x(l) <= model_x(2)
                y_n_mcell(l) = model_y(1) <= y(l) .and. y(l) <= model_y(4)
              end do
              cnt_dvtx_n_mcell = count( x_n_mcell(:) .and. y_n_mcell(:) )

              if( cnt_dvtx_n_mcell == 4 ) then

                dcell_area = poly_area( 4, x, y )
                !print *,'count=4',dcell_area

!MSK             elseif( new_grid ) then
!MSK               dvtx_shadow_ndx(:) = 0
!MSK               do l = 1,4
!MSK                 if( x_n_mcell(l) .neqv. y_n_mcell(l) ) then
!MSK                   dvtx_shadow_ndx(l) = shadow_map( x(l),y(l), n_shadow_zone=.true. )
!MSK                 elseif( .not. (x_n_mcell(l) .or. y_n_mcell(l)) ) then
!MSK                   dvtx_shadow_ndx(l) = shadow_map( x(l),y(l), n_shadow_zone=.false. )
!MSK                 endif
!MSK               end do
!MSK               dcell_ndx = dcell_ndx + 1
!MSK               dcell_partial_lon_ndx(dcell_ndx) = dlong_ndx
!MSK               dcell_partial_lat_ndx(dcell_ndx) = j
!MSK               dcell_area = area_map()
!MSK               partial_wght(dcell_ndx) = dcell_area

              else
                dcell_ndx  = dcell_ndx + 1
!MSK             dcell_area = model_area_type(mlon_ndx,mlat_ndx)%wght(dcell_ndx)

!***  Should be weighted by ratio 100x100m2 / 1000x1000m^2   = 1/100
                dcell_area = (dxef*dxef)/(dxout*dxout)

              endif

              wrk_sum = wrk_sum + dcell_area  * megan_data(i,j)

           else inside_mcell
             
               !  print *,'THIS IS OUTSIDE MCELL',i,j,lat_s,lon_s,data_lat(1),data_lon(1)

           endif inside_mcell

         end do data_lon_loop

       end do data_lat_loop


        !  print *,'wrk_sum',  wrk_sum



!***  Now we have the interpolation data

          wrk_data(mlon_ndx,mlat_ndx) = wrk_sum


         end do model_lon_loop

      end do model_lat_loop

!***  Final reprojection of the wrk_data field

         !upside down, left-to-right & mirror (only possible if n_nx = n_ny)
         do jx = 1,n_nx
           do jy = 1,n_ny

             wrk_data_out(jx,jy) = dble(wrk_data(n_ny-jy+1, n_nx-jx+1) )  !RR+HH

             !catch negative values from interpolation
             if (wrk_data_out(jx,jy).lt.0.0) then
               wrk_data_out(jx,jy) = 0.0
             endif

           enddo
         enddo


         !deallocate interpolation variables
         if( allocated( city_utme ) ) deallocate( city_utme )
         if( allocated( city_utmn ) ) deallocate( city_utmn )


         !print *,'STOP AFTER INTERPOLATED EF NETCDF'
         !stop

         return

      end subroutine area_interpolate




!CONCEPTUAL CODE FOR XY-DATA INTERPOLATION
!subroutine interp1( xData, yData, xVal, yVal )
!% Inputs: xData = a vector of the x-values of the data to be interpolated
!%         yData = a vector of the y-values of the data to be interpolated
!%         xVal  = a vector of the x-values where interpolation should be performed
!% Output: yVal  = a vector of the resulting interpolated values
!
!  implicit none
!
!  real, intent(in) :: xData(:), yData(:), xVal(:)
!  real, intent(out) :: yVal(:)
!  integer :: inputIndex, dataIndex
!  real :: minXdata, minYdata, xRange, weight
!
!  % Possible checks on inputs could go here
!  % Things you may want to check:
!  %   monotonically increasing xData
!  %   size(xData) == size(yData)
!  %   size(xVal) == size(yVal)
!
!  minXData = xData(1)
!  maxXData = xData(size(xData))
!  xRange = maxXData - minXData
!
!  for inputIndex = 1, size(xVal)
!      % possible checks for out of range xVal could go here
!
!      % this will work if x is uniformly spaced, otherwise increment
!      % dataIndex until xData(dataIndex+1)>xVal(inputIndex)
!      dataIndex = floor((xVal(inputIndex)-minXData)/xRange);
!
!      weight = (xVal - xData(dataIndex))/(xData(dataIndex+1)-xData(dataIndex));
!      yVal(inputIndex) = (1.0-weight)*yData(dataIndex) + ...
!                         weight*yData(dataIndex+1);
!  end
!end subroutine


      subroutine polint(PMAX, xa,ya,x,y)
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

!in
    double precision, intent(in)                  :: x 
    integer, intent(in)                           :: PMAX
    double precision, dimension(PMAX), intent(in) :: xa,ya

!out
    double precision, intent(out)                 :: y

!local
    double precision                          :: den,dif,dift,ho,hp,w,dy
    
    double precision, dimension(PMAX)         :: c,d
    integer                                   :: i,m,ns

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
          ! which correction, c or d, we want to add to our accumulating
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


   SUBROUTINE llij_merc(lat, lon, proj, i, j)

      ! Compute i/j coordinate from lat lon for mercator projection

      IMPLICIT NONE
      REAL, INTENT(IN)              :: lat
      REAL, INTENT(IN)              :: lon
      TYPE(proj_info),INTENT(IN)    :: proj
      REAL,INTENT(OUT)              :: i
      REAL,INTENT(OUT)              :: j
      REAL                          :: deltalon


      deltalon = lon - proj%lon1
      IF (deltalon > -180.) deltalon = deltalon + 360.
      IF (deltalon > 180.)  deltalon = deltalon - 360.
      i = proj%knowni + (deltalon/(proj%dlon*deg_per_rad))
      j = proj%knownj + (ALOG(TAN(0.5*((lat + 90.) * rad_per_deg)))) / &
             proj%dlon - proj%rsw

      !print *,'lljjmerc',lon,lat, proj%lon1,deltalon,i,j


   END SUBROUTINE llij_merc


      real function poly_area( nv, x, y )
!---------------------------------------------------------------
!  calculate the area of polynomial with nv vertices
!---------------------------------------------------------------

!---------------------------------------------------------------
!  dummy arguments
!---------------------------------------------------------------
       integer, intent(in) :: nv
       real, intent(in)    :: x(nv)
       real, intent(in)    :: y(nv)
!---------------------------------------------------------------
!  local variables
!---------------------------------------------------------------
       integer :: i, im1, ip1
       real    :: wrk(nv)

       do i = 1,nv
         ip1 = mod( i,nv ) + 1
         im1 = i - 1
         if( im1 == 0 ) im1 = nv
         wrk(i) = (x(ip1) - x(im1))*y(i)
       end do

       poly_area = -.5*sum( wrk(:) )

      end function poly_area


end module Interpol_Module
