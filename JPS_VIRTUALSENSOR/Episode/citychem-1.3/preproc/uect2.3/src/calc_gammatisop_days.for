! <calc_gammatisop_days.for - A component of the City-scale
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

      subroutine calc_gammatisop_days(nhours,TTfilename,gammat_dayy,  &
                                      scaleisop)

!***********************************************************************
!***  Subroutine calc_gammatisop_days calculates T-dependence gamma
!***********************************************************************

!     Declarations of variables by using the MODULES feature:

      use module_uect_exe
      use module_uect_io
      use module_uect_emis
      use module_uect_time
      use module_readnc


      implicit none

!***********************************************************************

        character (len=256), intent(in) :: TTfilename
        integer, intent(in)             :: nhours
        
        real,dimension(366,n_ny,n_nx),intent(out)    :: gammat_dayy
        real,dimension(366,24),intent(out)           :: scaleisop 

! *** local variables
 
        character(len=10)   :: varname 
        integer             :: nstart
        integer             :: one
        integer,parameter   :: nvalue = 24
        integer             :: houd
        integer             :: yeai
        integer             :: mony
        integer             :: dayw
        integer             :: daymm
        integer             :: dayy
        integer             :: h,i,j,k


    ! Scaling for diurnal cycle
        integer             :: day       !day number based on 365 days a year
        integer             :: ntim      !number of timesteps during one day
        integer,parameter   :: ntim_max=24    !maximum, used to declare array 
        integer             :: lat_start !southern edge of the domain (integer!)
        real                :: dlat      !increment   (real!)
        integer             :: idayy     !# days this year

        real                :: lat,lon
        real                :: gammat1,gammat2
        integer             :: lati
        integer             :: isone
        integer             :: istat

    ! T-dependency parameterisation G93
        !real, parameter     :: beta=0.09          ! K-1 G93
        !real, parameter     :: betasqt=0.17       ! K-1 Helmig et al., 2007
        real, parameter     :: tsc=303.           ! standard temperature (K)

    ! T- and PAR-dependency parameterisation G97
        !real, parameter     :: recalc=4.405
        real, parameter     :: ct1=95000.         ! J mol-1
        real, parameter     :: ct2=230000.        ! J mol-1
        real, parameter     :: ct3=0.961             
        real, parameter     :: tm=314.            ! leaf temperature (K)
        real, parameter     :: rgas=8.314         ! J K-1 mol-1

        double precision, allocatable :: field2D(:,:)
        real,   allocatable           :: sum24h(:,:)
        real,   allocatable           :: t24h(:,:)   ! in degree Celsius
    ! Scaling factor for each hour of day (ntime_max=25) and all 
    ! increments in x-direction
        real,   allocatable           :: scalef(:)

!***********************************************************************
!     Content of subroutine:


! *** read here the air temperature file and calculate the 24h mean
! *** from the 2D surface field air temperature (z=1)
! *** for the day of month (nstart=daym*24)

         if (.not. allocated(field2D) )    allocate( field2D(n_ny,n_nx) )
         if (.not. allocated(sum24h) )     allocate( sum24h(n_ny,n_nx) )
         if (.not. allocated(t24h) )       allocate( t24h(n_ny,n_nx) )
         if (.not. allocated(scalef) )     allocate( scalef(ntim_max) )
         varname = 'TT'
         one     = 1


!** TM5(old) scaling of isoprene emission with solar radiation (zenith angle dependent)
!** to emulate a diurnal profile of incoming radiation
!** The routine scale_isop gives the scaling factors for each hour of the day
!** We only want one value for the whole domain because it is small enough

         ntim = 24
         dlat = n_nx*0.01      ! degree increment for entire domain

!** 1. get utm zone as integer

         call str2int(utmzone(1:2),isone,istat)

         if (istat/=0) then
           call stopit('cannot convert utm zone string (calc_gammatisop_days)')
         endif

!** 2. calculate the latitude of the origin

         call utm2ll(isone,sitey0,sitex0,lat,lon)         

!** 3. convert latitude value to integer

         lati = int(lat)

!** 4. check if southern or northern hemisphere

         if (utmzone(3:3)=='N') then
           lat_start = 90 + lati
         else if (utmzone(3:3)=='S') then
           lat_start = lati
         else
           call stopit('utm string in cctapm_meta.inp not in right format')
         endif

         !print *,'lat  ',isone,lati,lat_start


         houd  = 1
         daymm = daym
         dayw  = dayweek
         mony  = mnth
         yeai  = year
         call cdayyear(yeai,mony,daymm,dayy)

         do h = 1, nhours

! *** first hour of day
           if (houd == 1) then

! *** get hour in temperature file for start reading
             if (nhours > 760) then    ! more than one month
              nstart = (dayy-1)*24 + 1
             else
               nstart = (daymm-1)*24 + 1
             endif

             sum24h(:,:) = 0.0
             do k = 1, nvalue
               call ReadNCfile(varname,TTfilename,field2D(:,:),n_ny,n_nx,1,nstart,one)
               do i = 1, n_ny
                 do j = 1, n_nx
                  sum24h(i,j) = sum24h(i,j) + real(field2D(i,j))
                 enddo
               enddo
               nstart=nstart+1
               !write(6,*) 'sumT 1,1 i',i,nstart,sum24h
             enddo


             t24h(:,:) = 0.0
             do i = 1, n_ny
               do j = 1, n_nx

!** daily mean temperature
                 t24h(i,j) = sum24h(i,j)/24.0
!** convert to temperature in Kelvin
                 t24h(i,j) = t24h(i,j) + 273.15

!** g97 algorithm to calculate gammaT for T-dependence
                 gammat1 =  exp( (ct1*(t24h(i,j)-tsc)) / (rgas*tsc*t24h(i,j)) )
                 gammat2 = ct3 + exp( (ct2*(t24h(i,j)-tm)) / (rgas*tsc*t24h(i,j)) ) 
 
                 gammat_dayy(dayy,i,j) = gammat1/gammat2 

              !   write(6,*) 'TT x,y, h', dayy,i,j,gammat_dayy(dayy,i,j)

               enddo
             enddo

!** call the isoprene emission scaling to get diurnal variation by sunlight

             call scale_isop(dayy,ntim,ntim_max,scalef,lat_start,dlat,dayy)


             do k = 1, ntim
               ! print *,'after scaling',scalef(k)
               scaleisop(dayy,k) = scalef(k)
             enddo


          endif

! *** Change date by one hour
           houd = houd + 1
           if ( houd == 25 ) then
              houd  = 1
              daymm = daymm + 1
! *** Check if new month?
              if ( daymm > NDAY(mony) ) then
                daymm = 1
                mony = mony + 1
              endif
              if (mony == 13) then
                mony = 1
              endif
! *** Get new calendar dayweek
              call cdayweek(yeai,mony,daymm,dayw)
! *** Get day of year (for heating degree days)
              call cdayyear(yeai,mony,daymm,dayy)
           endif

         enddo   

         if (allocated(field2D))         deallocate(field2D)
         if (allocated(sum24h))          deallocate(sum24h)
         if (allocated(t24h))            deallocate(t24h)


 
         return



      end subroutine calc_gammatisop_days
