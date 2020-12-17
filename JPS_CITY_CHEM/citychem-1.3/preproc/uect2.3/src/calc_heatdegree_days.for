! <calc_heatdegree_days.for - A component of the City-scale
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

      subroutine calc_heatdegree_days(nhours,TTfilename,fc_dayy)

!***********************************************************************
!***  Subroutine calc_heatdegree_days calculates heating degree day
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
        
        real,dimension(366,n_ny,n_nx),intent(out)    :: fc_dayy

! *** local variables
        real                :: hdd_dayy      
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
        
        double precision, allocatable :: field2D(:,:)
        real,   allocatable           :: sum24h(:,:)
        real,   allocatable           :: t24h(:,:)   ! in degree Celsius

!***********************************************************************
!     Content of subroutine:


! *** read here the air temperature file and calculate the 24h mean
! *** from the 2D surface field air temperature (z=1)
! *** for the day of month (nstart=daym*24)

         if (.not. allocated(field2D) )    allocate( field2D(n_ny,n_nx) )
         if (.not. allocated(sum24h) )     allocate( sum24h(n_ny,n_nx) )
         if (.not. allocated(t24h) )       allocate( t24h(n_ny,n_nx) )

         varname = 'TT'
         one     = 1


         houd  = 1
         daymm = daym
         dayw  = dayweek
         mony  = mnth
         yeai  = year
         call cdayyear(yeai,mony,daymm,dayy)

         do h = 1, nhours

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
                 t24h(i,j) = sum24h(i,j)/24.0
         
                 hdd_dayy = max((18.000 - t24h(i,j)), 1.000)

                 fc_dayy(dayy,i,j) = fcb + (1.000 - fcb) * (hdd_dayy/hddmean)
                 !write(6,*) 'TT x,y, h', dayy,i,j,fc_dayy(dayy,i,j)
               enddo
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


      end subroutine calc_heatdegree_days
