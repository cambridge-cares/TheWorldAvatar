! <calc_bvocemis_activ.for - A component of the City-scale
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

      subroutine calc_bvocemis_activ(incols,nareas,nareas_new,snap,data_array, &
                                     nxe,nye,EFfilename,efisop,efapin,eflimo)

!***********************************************************************
!***  Subroutine calc_bvocemis_activ cuts and interpolates the domain's
!***  BVOC emission factors, foliar density and LAI and calculates
!***  a preliminary emission potential, which is multiplied by the
!***  input emission of unity
!***    efisop = wrk2D1  ! L/T-synth
!***    efapin = wrk2D3  ! T-pool
!***    eflimo = wrk2D2  ! L/T-synth
!***  EF emission factor unit: ug/g(leaf)/h
!***  FD foliar density  unit: g(leaf)/m^2
!***  output             unit: g/s
!***********************************************************************

!     Declarations of variables by using the MODULES feature:

      use module_uect_exe
      use module_uect_io
      use module_uect_emis

      use module_readnc
      use Closest_Pair
      use Interpol_Module
      use module_writenc

      implicit none

!***********************************************************************

! *** IN
        character (len=256), intent(in) :: EFfilename
        integer, intent(in)             :: incols
        integer, intent(in)             :: nareas
        integer, intent(in)             :: nareas_new
        integer, dimension(nareas),intent(in)     :: snap
        real, dimension(nareas,incols),intent(in) :: data_array
        integer, intent(in)             :: nxe
        integer, intent(in)             :: nye


! *** OUT
        real,dimension(nareas_new),intent(out)    :: efisop
        real,dimension(nareas_new),intent(out)    :: efapin
        real,dimension(nareas_new),intent(out)    :: eflimo

! *** local variables for netcdf read
        integer             :: ncid
        integer             :: dimid
        integer             :: varid
        integer             :: nlon_megan
        integer             :: nlat_megan
        integer(2)          :: missing_value
        real                :: fill_value
        integer,parameter   :: ns=12

        character(lEN=80)   :: message
        character(len=12)   :: varname1
        character(len=24)   :: varname2
        character(len=12)   :: varname3
        character(len=12)   :: varname4

        integer             :: nstart
        integer             :: one
        integer,parameter   :: nvalue = 24
        integer             :: houd
        integer             :: yeai
        integer             :: mony
        integer             :: dayw
        integer             :: daymm
        integer             :: dayy

! *** local computations
        integer             :: n
        integer             :: i,j,k,m,h
        integer             :: ix,iy,jx,jy
        integer             :: ai1,ai2
        integer             :: isone
        integer             :: istat
        integer             :: lenc
        integer             :: cutnorth
        integer             :: cutwest

        real,parameter      :: dxef = 100.0
        real                :: lat0,lon0
        real                :: dp
        real                :: limd
        real                :: corrnorth
        real                :: corrwest

        integer,dimension(nareas_new) :: aind
        integer,dimension(nareas_new) :: xi
        integer,dimension(nareas_new) :: yi

    ! Scaling for diurnal cycle
    !    integer             :: day       !day number based on 365 days a year
    !    integer             :: ntim      !number of timesteps during one day
    !    integer,parameter   :: ntim_max=24    !maximum, used to declare array 
    !    integer             :: lat_start !southern edge of the domain (integer!)
    !    real                :: dlat      !increment   (real!)
    !    integer             :: idayy     !# days this year

    !    real                :: lat,lon
    !    real                :: gammat1,gammat2
    !    integer             :: lati


    ! T-dependency parameterisation G93
        !real, parameter     :: beta=0.09          ! K-1 G93
        !real, parameter     :: betasqt=0.17       ! K-1 Helmig et al., 2007
        !real, parameter     :: tsc=303.           ! standard temperature (K)

    ! T- and PAR-dependency parameterisation G97
        !real, parameter     :: recalc=4.405
        !real, parameter     :: ct1=95000.         ! J mol-1
        !real, parameter     :: ct2=230000.        ! J mol-1
        !real, parameter     :: ct3=0.961             
        !real, parameter     :: tm=314.            ! leaf temperature (K)
        !real, parameter     :: rgas=8.314         ! J K-1 mol-1

        type(point), dimension(:), allocatable :: efpoints 
        type(point), dimension(2)              :: p

        double precision, allocatable :: field2Def(:,:)
        double precision, allocatable :: field2Dfd(:,:)
        double precision, allocatable :: field2Defl(:,:)
        double precision, allocatable :: field2Deft(:,:)
        double precision, allocatable :: field2Def_sn(:,:)
        double precision, allocatable :: field2Defl_sn(:,:)
        double precision, allocatable :: field2Deft_sn(:,:)
        double precision, allocatable :: field2D1(:,:)
        double precision, allocatable :: field2D2(:,:)
        double precision, allocatable :: field2D3(:,:)
        double precision, allocatable :: wrk2D1(:,:)
        double precision, allocatable :: wrk2D2(:,:)
        double precision, allocatable :: wrk2D3(:,:)
        real(8), allocatable          :: xedge(:)
        real(8), allocatable          :: yedge(:)
        real, allocatable             :: megan_lons(:)
        real, allocatable             :: megan_lats(:)
        real, allocatable             :: megan_lons_sn(:)
        real, allocatable             :: megan_lats_sn(:)
        real, allocatable             :: field_lons(:)
        real, allocatable             :: field_lats(:)

        character (len=256) :: fname_nc
        character (len=256) :: epsgn_u
        real,   allocatable :: axm_i(:,:)
        real,   allocatable :: axm_j(:,:)
        real,   allocatable :: z(:)
        double precision,   allocatable :: field2D_test(:,:)

!***********************************************************************
!     Content of subroutine:


! *** read here the air temperature file and calculate the 24h mean
! *** from the 2D surface field air temperature (z=1)
! *** for the day of month (nstart=daym*24)
! input
         if (.not. allocated(field2Def) )    allocate( field2Def(nye,nxe) )
         if (.not. allocated(field2Dfd) )    allocate( field2Dfd(nye,nxe) )
         if (.not. allocated(field2Defl))    allocate( field2Defl(nye,nxe) )
         if (.not. allocated(field2Deft))    allocate( field2Deft(nye,nxe) )
         if (.not. allocated(field2Def_sn))  allocate( field2Def_sn(nye,nxe) )
         if (.not. allocated(field2Defl_sn)) allocate( field2Defl_sn(nye,nxe) )
         if (.not. allocated(field2Deft_sn)) allocate( field2Deft_sn(nye,nxe) )

! local
         if (.not. allocated(field2D1) )     allocate( field2D1(n_nx*ns,n_ny*ns) )
         if (.not. allocated(field2D2) )     allocate( field2D2(n_nx*ns,n_ny*ns) )
         if (.not. allocated(field2D3) )     allocate( field2D3(n_nx*ns,n_ny*ns) )
         if (.not. allocated(field_lons) )   allocate( field_lons(n_nx*ns) )
         if (.not. allocated(field_lats) )   allocate( field_lats(n_ny*ns) )
         if (.not. allocated(xedge) )        allocate( xedge(n_nx*ns+1) )
         if (.not. allocated(yedge) )        allocate( yedge(n_ny*ns+1) )
         if (.not. allocated(wrk2D1) )       allocate( wrk2D1(n_nx,n_ny) )
         if (.not. allocated(wrk2D2) )       allocate( wrk2D2(n_nx,n_ny) )
         if (.not. allocated(wrk2D3) )       allocate( wrk2D3(n_nx,n_ny) )

! output
         if (.not. allocated(axm_i) )        allocate( axm_i(n_nx,n_ny) )
         if (.not. allocated(axm_j) )        allocate( axm_j(n_nx,n_ny) )
         if (.not. allocated(z) )            allocate( z(n_nz) )
         if (.not. allocated(field2D_test))  allocate( field2D_test(n_ny,n_nx) )
         if (.not. allocated(efpoints))      allocate( efpoints(n_nx*n_ny+1) )


! *** Initialize some local variables
         varname1 = 'Isoprene_EF'
         varname2 = 'Foliar_Density_EF'
         varname3 = 'MTS_LT_EF'
         varname4 = 'MTS_T_EF'
         one      = 1

! *** dxout is EPISODE grid distance dx m
         limd     = dxout + dxout/5

! *** Read 2-D nc file with EF data (no time-variant data, 100x100 m2)
         call ReadNCfile(varname1,EFfilename,field2Def(:,:),nye,nxe,1,one,one)
         call ReadNCfile(varname2,EFfilename,field2Dfd(:,:),nye,nxe,1,one,one)
         call ReadNCfile(varname3,EFfilename,field2Defl(:,:),nye,nxe,1,one,one)
         call ReadNCfile(varname4,EFfilename,field2Deft(:,:),nye,nxe,1,one,one)


!---------------------------------------------------------------------
!   get EF field dataset dimesions
!---------------------------------------------------------------------
         message = 'bioemiss: Failed to open ' // trim(EFfilename)
         call handle_ncerr( nf_open( trim(EFfilename), nf_noclobber, ncid ), message )  
         message = 'bioemiss: Failed to get lon dimension id'
         call handle_ncerr( nf_inq_dimid( ncid, 'longitude', dimid ), message )
         message = 'bioemiss: Failed to get lon dimension'
         call handle_ncerr( nf_inq_dimlen( ncid, dimid, nlon_megan ), message )
         message = 'bioemiss: Failed to get lat dimension id'
         call handle_ncerr( nf_inq_dimid( ncid, 'latitude', dimid ), message )
         message = 'bioemiss: Failed to get lat dimension'
         call handle_ncerr( nf_inq_dimlen( ncid, dimid, nlat_megan ), message )
         write(*,*) 'bioemiss:  nlon_megan, nlat_megan = ',nlon_megan,nlat_megan


         if (.not. allocated(megan_lons) )    allocate( megan_lons(nlon_megan) )
         if (.not. allocated(megan_lats) )    allocate( megan_lats(nlat_megan) )
         if (.not. allocated(megan_lons_sn))  allocate( megan_lons_sn(nlon_megan) )
         if (.not. allocated(megan_lats_sn))  allocate( megan_lats_sn(nlat_megan) )

         message = 'bioemiss: Failed to get lon variable id'
         call handle_ncerr( nf_inq_varid( ncid, 'longitude', varid ), message )
         message = 'bioemiss: Failed to read lon variable'
         call handle_ncerr( nf_get_var_real( ncid, varid, megan_lons ), message )
         message = 'bioemiss: Failed to get lat variable id'
         call handle_ncerr( nf_inq_varid( ncid, 'latitude', varid ), message )
         message = 'bioemiss: Failed to read lat variable'
         call handle_ncerr( nf_get_var_real( ncid, varid, megan_lats ), message )
         !print *,'megan_lons',megan_lons
         !print *,'megan_lats',megan_lats


! *** Turn y-coordinate upside down: new field has unit: ug/h/m^2
! *** We represent emissions of alpha-pinene as 'MTS_T_EF' (T dependent)
! *** and emissions of limonene as 'MTS_LT_EF' (T- and light dependent).
         do j = 1, nye
           ! isoprene
           field2Def_sn(j,:) = field2Def(nye-j+1,:)  *field2Dfd(nye-j+1,:)
           ! limonene (efl)
           field2Defl_sn(j,:)= field2Defl(nye-j+1,:) *field2Dfd(nye-j+1,:)
           ! apinene (eft)
           field2Deft_sn(j,:)= field2Deft(nye-j+1,:) *field2Dfd(nye-j+1,:)
         enddo
         do j = 1, nye
            megan_lats_sn(j) = megan_lats(nye-j+1)
            !print *,'lat',j,megan_lats_sn(j)
         enddo
         

! *** Cut out field2D from the centre (300x300) of the original field
! *** Swap x and y coordinates of the original field

         if (fe_in_bvocrr) then
           cutnorth = 340
           cutwest  = 210
           corrnorth= -0.03
           corrwest = -0.015
         endif
         if (fe_in_bvochh) then
           cutnorth = 370
           cutwest  = 290
           corrnorth= -0.097
           corrwest = -0.055
         endif

         do i = 1, n_nx*ns
            do j = 1, n_ny*ns
              field2D1(i,j) = field2Def_sn( cutwest+j,cutnorth+i)
              field2D2(i,j) = field2Defl_sn(cutwest+j,cutnorth+i)
              field2D3(i,j) = field2Deft_sn(cutwest+j,cutnorth+i)
            enddo
         enddo

     !!! compare field_lons(1) and field_lats(1) to domain origin
     !!! change x-/y-offset accordingly.
     !!! also take care of the N-S and W-E corrections.
     !!! ADVICE: do not add correction values here
     ! Rheinruhr: x:210 y:340

         !!!CORRECTION IN NORTH DIRECTION
         do i = 1, n_nx*ns
           field_lons(i) =   megan_lons( cutnorth+i) + corrnorth
           !print *,'lon',i,field_lons(i)
         enddo
         !!!CORRECTION IN WEST DIRECTION
         do j = 1, n_ny*ns
           field_lats(j) = megan_lats_sn( cutwest+j) +corrwest
           !print *,'lat',j,field_lats(j)
         enddo
         !stop


! *** Free the original-size arrays
         if (allocated(field2Def))         deallocate(field2Def)
         if (allocated(field2Dfd))         deallocate(field2Dfd)
         if (allocated(field2Defl))        deallocate(field2Defl)
         if (allocated(field2Deft))        deallocate(field2Deft)
         if (allocated(field2Def_sn))      deallocate(field2Def_sn)
         if (allocated(field2Defl_sn))     deallocate(field2Defl_sn)
         if (allocated(field2Deft_sn))     deallocate(field2Deft_sn)
         if (allocated(megan_lons))        deallocate(megan_lons)
         if (allocated(megan_lats))        deallocate(megan_lats)
         if (allocated(megan_lons_sn))     deallocate(megan_lons_sn)
         if (allocated(megan_lats_sn))     deallocate(megan_lats_sn)

         missing_value=-1
         fill_value = -1.0

         where( field2D1(:,:) == missing_value )
           field2D1(:,:) = 0.0
         endwhere
         where( field2D1(:,:) == fill_value )
           field2D1(:,:) = 0.0
         endwhere

           !do i = 1, n_nx*ns
           !   print*,'EF',i,field2D1(i,1)
           ! enddo
           !do j = 1, n_ny*ns
           !   print*,'EF',j,field2D1(1,j)
           !enddo
           !stop


         where( field2D2(:,:) == missing_value )
           field2D2(:,:) = 0.0
         endwhere
         where( field2D3(:,:) == missing_value )
           field2D3(:,:) = 0.0
         endwhere
         !print *,'field2d',field2D1(:,:)


! *** Define the edge vectors of the EF 300x300 map

         xedge(2:n_nx*ns) = .5_8*(field_lons(1:n_nx*ns-1) + field_lons(2:n_nx*ns))
         xedge(1)         = field_lons(1) - .5_8*(field_lons(2) - field_lons(1))
         xedge(n_nx*ns+1) = field_lons(n_nx*ns) + .5_8*(field_lons(n_nx*ns) - field_lons(n_nx*ns-1))

         yedge(2:n_ny*ns) = .5_8*(field_lats(1:n_ny*ns-1) + field_lats(2:n_ny*ns))
         yedge(1)            = field_lats(1) - .5_8*(field_lats(2) - field_lats(1))
         yedge(n_ny*ns+1) = field_lats(n_ny*ns) + .5_8*(field_lats(n_ny*ns) - field_lats(n_ny*ns-1))

         print *,'xedge 1',xedge(1)  ! xedge(n_nx*ns+1)
         print *,'yedge 1',yedge(1)

         print *,'xedge max',xedge(n_nx*ns+1) ! xedge(1)
         print *,'yedge max',yedge(n_ny*ns)
         print *,'yedge max+1',yedge(n_ny*ns+1)

!***  Compare xedge (1 | max) and yedge (1 | max) with target_lat,target_lon and upper_lat,upper_lon 


! *** Spatial interpolation of EF 300x300 map to the city domain (wrk2D)
         call area_interpolate(n_nx*ns,n_ny*ns,xedge,yedge,field2D1,wrk2D1)
         call area_interpolate(n_nx*ns,n_ny*ns,xedge,yedge,field2D2,wrk2D2)
         call area_interpolate(n_nx*ns,n_ny*ns,xedge,yedge,field2D3,wrk2D3)


!***  Create a netcdf file for testing

         fname_nc = 'efmapemis.nc'

! *** Define EPSGN code
         epsgn_u = trim('326'//utmzone(1:2))

! *** Create a grid with the x- and y- coordinates of the grid centre-points
         do jx = 1, n_nx
           do jy = 1, n_ny
             axm_i(jy,jx) = sitex0 + (jx-1)*dxout
             axm_j(jy,jx) = sitey0 + (jy-1)*dxout
           enddo
         enddo
         z(1) = 10

         call CreateNCfileGrid(fname_nc,n_nx,n_ny,1,axm_i,axm_j,z,dxout,utmzone,epsgn_u,sitex0,sitey0)

! *** Write the netCDF output file of all species,
! *** Area source emissions hourly 2D field [instantaneous, QA()]

         unitname = "g/s"
         validity = 'averaged'
         dopacking = .false.
         dofloat   = .true.
         domirror  = .true.

         do h = 1,1

           ! one time step 
           Nhh_in = 1
           ! current simulation date
           mdate(1,Nhh_in) = 2012
           mdate(2,Nhh_in) = 7
           mdate(3,Nhh_in) = 7
           mdate(4,Nhh_in) = 14

           namefield = "EFMAP      "

               do jx = 1,n_nx
                 do jy = 1,n_ny

                     field2D_test(jy,jx) = wrk2D1(jx,jy)

                 enddo
               enddo

               call writeconcfield(fname_nc,namefield,unitname,       &
                                   field2D_test(1:n_ny,1:n_nx), n_ny, n_nx, 1,      &
                                   Nhh_in, mdate, validity, dopacking, dofloat, domirror)
 
         enddo


         !deallocate netcdf variables
         if (allocated(axm_i) )       deallocate( axm_i )
         if (allocated(axm_j) )       deallocate( axm_j )
         if (allocated(z) )           deallocate( z )
         if (allocated(field2D_test)) deallocate( field2D_test )



! ***  Construct the EF data grid, point by point
! ***  and write into points pair array 

         i = 1
         do ix = 1, n_nx
           do iy = 1, n_ny
             efpoints(i)%x = sitex0 + (ix-1)*dxout
             efpoints(i)%y = sitey0 + (iy-1)*dxout
             !print *,i, efpoints(i)
             i = i + 1
           enddo
         enddo


! *** We need to loop over all area sources of SNAP cat. 11 
! *** and find the corresponding EF of the closest grid cell.
! *** We use the closest pair routine to find the EF values of
! *** the cell which is closest to the SNAP11 area point in data_array
! *** Code taken from filter_areas.for

! *** The routine stops if any area source has no SW-coordinate

! *** Loop over the nareas_new sources

      do n = 1, nareas_new

! *** Check if SNAP cat. 11

        if (snap(n).eq.11) then

          ! X SW-coordinate
          if (data_array(n,1).le.missval) then
            print *,'X SW-coor missing for area source ',n
            call stopit('Area source with missing X SW-coordinate')
          endif

          ! Y SW-coordinate
          if (data_array(n,2).le.missval) then
            print *,'Y SW-coor missing for point source ',n
            call stopit('Area source with missing Y SW-coordinate')
          endif

          print *,'snap11 ',n

          ! efpoints are the x/y-coordinates of the SNAP11 area sources
          ! Mid-Point coordinates of area source n
          ! Last value in points pair array
          efpoints(n_nx*n_ny+1)%x = data_array(n,1) + 0.5*dxout
          efpoints(n_nx*n_ny+1)%y = data_array(n,2) + 0.5*dxout

          ! Simple closest pair works best for this purpose
          dp = closest_pair_simple(efpoints, p)
          !print *, "sim ", dp

          if (dp .le. limd) then

             ai1 = 0
             ai2 = 0
             do i = 1, (n_nx*n_ny)
               if ((p(1)%x.eq.efpoints(i)%x).and.(p(1)%y.eq.efpoints(i)%y)) then
                 ai1=i
               endif
               if ((p(2)%x.eq.efpoints(i)%x).and.(p(2)%y.eq.efpoints(i)%y)) then
                 ai2=i
               endif
             end do
             if (  (ai1>0).and.(ai2>0).and.(ai1.ne.ai2) ) then   ! error
               aind(n) = 0
             else
               if (ai1>0) aind(n)=ai1
               if (ai2>0) aind(n)=ai2
             endif

           else

             aind(n) = 0

           endif

           !print *,'efpoint x y,',efpoints(n_nx*n_ny+1)%x ,efpoints(n_nx*n_ny+1)%y,aind(n)


! *** Now loop through emission input data_array and filter
! *** using the area mask (index aind)
! *** Only the points inside the domain are written to
! *** the filter_array
 
           if ( aind(n) == 0 ) then
             xi(n) = 0
             yi(n) = 0
           else

             lenc = 0

             do ix = 1,n_nx
                do iy = 1,n_ny
                  lenc = lenc +1
                  if ( lenc == aind(n) ) then
                    xi(n) = ix
                    yi(n) = iy
                    ! here hand over the EF value ...
                  endif
                enddo
             enddo

           endif    

! *** If xi and yi are in the SNAP11 area source input
! *** then attribute the interpolated BVOC value for that cell
! ***   efisop = wrk2D1  ! L/T-synth
! ***   efapin = wrk2D3  ! T-pool
! ***   eflimo = wrk2D2  ! L/T-synth
           if ( (xi(n).gt.0).and.(yi(n).gt.0) ) then
             efisop(n) = wrk2D1(xi(n),yi(n))
             efapin(n) = wrk2D3(xi(n),yi(n))
             eflimo(n) = wrk2D2(xi(n),yi(n))

             print *,'n xi yi efisop', n, xi(n), yi(n), efisop(n)

           else

             efisop(n) = 0.0
             efapin(n) = 0.0
             eflimo(n) = 0.0

           endif

           ! convert ug/h/m^2 to ug/h/cell
           efisop(n) = efisop(n) * dxout**2
           efapin(n) = efapin(n) * dxout**2
           eflimo(n) = eflimo(n) * dxout**2

           ! convert ug/h/cell to g/h/cell
           efisop(n) = efisop(n) * 1.e-6
           efapin(n) = efapin(n) * 1.e-6
           eflimo(n) = eflimo(n) * 1.e-6

           ! convert g/h/cell to g/s
           efisop(n) = efisop(n) /3600.0
           efapin(n) = efapin(n) /3600.0
           eflimo(n) = eflimo(n) /3600.0

        else  ! not snap 11

           efisop(n) = 0.0
           efapin(n) = 0.0
           eflimo(n) = 0.0

        endif

       enddo ! loop over number of area sources



         if (allocated(field2D1))        deallocate(field2D1)
         if (allocated(field2D2))        deallocate(field2D2)
         if (allocated(field2D3))        deallocate(field2D3)
         if (allocated(wrk2D1))          deallocate(wrk2D1)
         if (allocated(wrk2D2))          deallocate(wrk2D2)
         if (allocated(wrk2D3))          deallocate(wrk2D3)
         if (allocated(xedge))           deallocate(xedge)
         if (allocated(yedge))           deallocate(yedge)

         if (allocated(efpoints))        deallocate(efpoints)

 
         return



      end subroutine calc_bvocemis_activ
