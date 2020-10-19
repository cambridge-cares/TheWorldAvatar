
! <emission_points.for - A component of the City-scale
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

      subroutine emission_points(incols,npoints,npoints_new,nhours,     &
                       data_array,snap, pse_src_snap, pse_src_param,    &
                       pse_src_qemhour)

!***********************************************************************
!***  Subroutine emision_points converts the emission totals (kg/year)
!***  to hourly emissions (g/s) for point sources
!!!!!!!
!!!  modified by Kang @ Jan. 22, 2020 @ Cambridge CARES for moving point source (circular or straight line) and building parameters, and emission rate calculation
!!! a, add 5 more columns for moving points:  vec (m/s), direction (0-360), circ_ang (>0, clockwise, <0, counter-colockwise, =0, striaght line), t_start_moving(s),t_stop_moving(s). 
!!! b, read the building height and width from input excel file (2 more columns), intead of default definition in "emission_point.for"
!!! will read into "in_array_pse(:,7-13)". So for emission indicator, it needs be adding 7. 
!!! other varibles: new_array_pse(:,7-13), out_params_pse() 
!!  3, change start and end simulation date value so that the correct date (hour) can be 
!!      used for estimating the time factor for default long-term emission rate conversion. 
!!     For simulation with known emission data, time factor is not used. 
!!!
!!! Note: for moving point source, it needs to update the location and emissions rate and other parameters
!!        of point sources every hour.  Otherwise, the point source location and emission rate will be reset
!!        every hour, which is not accurate. 
!***********************************************************************
!***********************************************************************

!     Declarations of variables by using the MODULES feature:

      use module_uect_exe
      use module_uect_emis
      use module_uect_time
      use module_uect_io

      implicit none

!***********************************************************************

!in
      integer, intent(in)             :: incols
      integer, intent(in)             :: npoints
      integer, intent(in)             :: npoints_new
      integer, intent(in)             :: nhours
      integer, dimension(npoints),intent(in)            :: snap
      real, dimension(npoints,incols),intent(in)        :: data_array
!out
      real, dimension(npoints_new,ncomp,nhours),intent(out) :: pse_src_qemhour
      real, dimension(npoints_new,n_pse_params),intent(out) :: pse_src_param
      integer, dimension(npoints_new),intent(out)           :: pse_src_snap

!     Local declarations:

      integer                                :: i, j, n, h
      integer                                :: houd
      integer                                :: yeai
      integer                                :: mony
      integer                                :: dayw
      integer                                :: daymm
      integer                                :: dayy
      real, dimension(npoints,n_inp_compds)  :: pse_src_qemvec
      real, dimension(npoints,n_voc_compds)  :: pse_src_qemvoc
      real, dimension(npoints,n_nox_compds)  :: pse_src_nox
      real, dimension(n_snap)                :: timedis
      real,dimension(366,n_ny,n_nx)          :: gammat_dayy
      real,dimension(366,24)                 :: scaleisop 

!***********************************************************************
!     Content of subroutine:


! *** Open and read temperature file
      fname_nc_airtemp = trim(fname_inpath_tapm)//'/T_and_dtdz_'//trim(startdate)//'_'//trim(enddate)//'.nc'

      if (fe_in_tapm) then
        call calc_gammatisop_days(nhours,fname_nc_airtemp,gammat_dayy,scaleisop)
      end if

      do n = 1, npoints_new

! *** SNAP of the filtered point sources
        pse_src_snap(n)  = snap(n)

! *** Get source parameters
! *** xcor;ycor;Hi;Vi;Ti;radi
! *** 1    2    3  4  5  6  
!!!!============================================================================= 
!!*** changed by kang for adding 6 more variables for moving point and building: 
!!*** Bheight, Bwidth, Pvec (m/s), Pdir (deg), cir_ang(deg),Pstart(s), Pend(s)
!!*** 7      , 8     , 9,        , 10        , 11          , 12      , 13
!!!!============================================================================= 
! *** X-coor  Y-coor  Z-coor  Hstack   Hdiam   Bheigth Bwidth  Qenergy Qtemp   Qvelo
! *** 1       2       3       4        5       6       7       8       9       10
! *** QXV     QYV     QZV     QHSV     QDIV    QHBV    QWBV    QIEV    QTGV    QVGV
!!!!============================================================================= 
!!*** changed by kang for adding 4 more variables for moving point (building height and width have been defined as 6 and 7): 
!!*** Pvec (m/s), Pdir (deg), cir_ang(deg), Pstart(s), Pend(s)
!!*** 11        , 12        , 13            , 14      ,15  
! *** vel_point(n),dd_point(n),t_mpoint1(n),t_mpoint2(n)
!!!!============================================================================= 
        if (data_array(n,1).gt.missval) then
          pse_src_param(n, 1) = data_array(n,1)
        else
          print *,'X-coor missing for point source ',n
          call stopit('Point source with missing x-coordinate')
        endif
        if (data_array(n,2).gt.missval) then
          pse_src_param(n, 2) = data_array(n,2)
        else
          print *,'Y-coor missing for point source ',n
          call stopit('Point source with missing Y-coordinate')
        endif

! Default values for some of the required stack parameters
        pse_src_param(n, 3) = 0.0

!!!        
!!!================================================================
!!! changed by Kang @ CARES for building parameters (height and width @m)
!!! orig:
!        pse_src_param(n, 6) = 10.0
!        pse_src_param(n, 7) = 20.0
!!!!!
        if (data_array(n,7).gt.missval) then      
          pse_src_param(n, 6) = data_array(n,7)   !!! building height @ m
        else
          print *,'Building height missing for point source ',n
          call stopit('Point source with missing Building height')
        endif
        
        if (data_array(n,8).gt.missval) then
          pse_src_param(n, 7) = data_array(n,8) !!! building width @ m
        else
          print *,'Building width missing for point source ',n
          call stopit('Point source with missing Building width')
        endif
!!!! then for moving sources:
        if (data_array(n,9).gt.missval) then
          pse_src_param(n,11) = data_array(n,9)   !!! point source moving speed @ m/s
        else
          print *,'moving speed missing for point source ',n
          call stopit('Point source with missing - moving speed')
        endif
        
        if (data_array(n,10).gt.missval) then
          pse_src_param(n,12) = data_array(n,10)   !!! point source moving direction @ degree (0-360):  0: moving north, 90: east; 180: south.
        else
          print *,'moving direction missing for point source ',n
          call stopit('Point source with missing - moving direction')
        endif 

        if (data_array(n,11).gt.missval) then
          pse_src_param(n,13) = data_array(n,11)   !!! point source moving circular angle.  >0, clockwise, <0, counter-clockwise.  =0, straight line. 
        else
          print *,'moving starttime missing for point source ',n
          call stopit('Point source with missing - moving starttime')
        endif    

        if (data_array(n,12).gt.missval) then
          pse_src_param(n,14) = data_array(n,12)   !!! point source moving start time in current hour @ s (=0~3600 s)
        else
          print *,'moving starttime missing for point source ',n
          call stopit('Point source with missing - moving starttime')
        endif  

        if (data_array(n,13).gt.missval) then
          pse_src_param(n,15) = data_array(n,13)   !!! point source moving end time in current hour @ s (=0~3600 s)
        else
          print *,'moving endtime missing for point source ',n
          call stopit('Point source with missing - moving endtime')
        endif  
!!!        
!!!! end of change.               
!!!================================================================
!!!
        pse_src_param(n, 8) = -9900

        ! stack height
        if (data_array(n,3).gt.missval) then
          pse_src_param(n, 4) = data_array(n,3)
          if (pse_src_param(n, 4).gt.220.0) then
            print *,'Stack height for point source ',n,' is ', pse_src_param(n, 4)
            call stopit('Point source with stack height >220m')
          endif
        else
          pse_src_param(n, 4) = stack_HS_default(snap(n))      !m height
        endif
        ! stack radius
        if (data_array(n,6).gt.missval) then
          pse_src_param(n, 5) = data_array(n,6)       !radius  (CC uses diameter)
        else
          pse_src_param(n, 5) = stack_RA_default(snap(n))      !m
        endif
        ! exhaust temp
        if (data_array(n,5).gt.missval) then
          pse_src_param(n, 9) = data_array(n,5)       !T in degC
        else
          pse_src_param(n, 9) = stack_ET_default(snap(n))     !degC
        endif
        ! exhaust velo
        if (data_array(n,4).gt.missval) then
          pse_src_param(n,10) = data_array(n,4)
        else
          pse_src_param(n,10) = stack_EV_default(snap(n))      !m/s
        endif

       
        ! if radius > 10m --> AREA SOURCE TYPE
        ! Take default parameters from SNAP7 traffic area source
        if ( (pse_src_param(n, 5).gt.10.0).and.  &
             (pse_src_snap(n).gt.1)  )              then
          pse_src_param(n, 4) = stack_HS_default(7)       !m Hs
          pse_src_param(n, 5) = stack_RA_default(7)       !m radius
          pse_src_param(n, 9) = stack_ET_default(7)       !degC
          pse_src_param(n,10) = stack_EV_default(7)       !m/s
         if (fe_log) then
            write(funit_log,'(1X,A58,I5)')  &
            'Warning: stack radius exceeded max. value 10 m for source ',n
          endif     
        endif

! ***
! *** Additional CHECKS:
! ***

! *** Maximum Stack exit velocity (m/s)
        if ( pse_src_param(n,10).gt.30.0 ) then
          pse_src_param(n, 10) = 30.0
          if (fe_log) then
            write(funit_log,'(1X,A61,I5)')  &
            'Warning: stack exit velocity exceeded max. value 30 m/s for: ',n
          endif         
        endif
       !print *,'HS after checks: ', n, pse_src_param(n, 4)

! *** Minimum Stack exhaust temperature (degC)
        if ( pse_src_param(n, 9).lt.5.0 ) then
          pse_src_param(n, 9) = 5.0
          if (fe_log) then
            write(funit_log,'(1X,A61,I5)')  &
            'Warning: stack exit temperature below min. value 5 degC for: ',n
          endif         
        endif

! *** Maximum Stack exhaust temperature (degC)
        if ( pse_src_param(n, 9).gt.450.0 ) then
          if (fe_log) then
            write(funit_log,'(1X,A61,I5)')  &
            'Warning: stack exit temperature >450 degC. TG in Celsius? ',n
          endif         
        endif

!!!        
!!!================================================================
!!! changed by Kang @ CARES after adding moving point parameters and building parameters in input excel file
!!
! *** Get total emissions
! *** orignally, data_array(n,7:13)
!!*** now, should be data_array(n,13:19) as 4 moving parameters and 2 building parameters are added.
!!***
! *** NOx     NMVOC   CO     SO2     NH3    PM25    PM10
! *** 1       2       3      4       5      6       7
!!!!!!
!!! orignal:
!        j = 1
!        do i = 13, 19  !!! changed by kang from i=7, 13 to i=13, 19
!          if (data_array(n, i).gt.missval) then
!            pse_src_qemvec(n, j) = data_array(n, i) * pse_funit
!          else
!            pse_src_qemvec(n, j) = 0.0
!          endif
!          j = j +1
!        enddo

        j = 1
        do i = 14, 20  !!! changed by kang from i=7, 13 to i=14, 20
          if (data_array(n, i).gt.missval) then
            pse_src_qemvec(n, j) = data_array(n, i) * pse_funit
          else
            pse_src_qemvec(n, j) = 0.0
          endif
          j = j +1
        enddo
!===================++++++++++++++++++++++++++++++++++++++++++++


! *** Split VOC and NOx to CityChem / TAPM specific emissions
! *** For NOx the standard split for all sectors is NO:NO2  95:5
! *** RSMOG = 0.0067*NMVOC; for TAPM (tapm v4.0 docu, pp. 27; cite Johnson, 1984)
! *** VOC split depends on SNAP
! *** Isoprene recognized if in SNAP11
        pse_src_nox(n,    1) = pse_src_qemvec(n, 1)  * f_no_pse        ! NO
        pse_src_nox(n,    2) = pse_src_qemvec(n, 1)  * (1.0-f_no_pse)  ! NO2
        pse_src_nox(n,    3) = 0.0                                     ! HONO
        pse_src_qemvoc(n, 1) = pse_src_qemvec(n, 2)  * voc2rsmog
        pse_src_qemvoc(n, 2) = pse_src_qemvec(n, 2)  * voc2hcho( snap(n) )
        pse_src_qemvoc(n, 3) = pse_src_qemvec(n, 2)  * voc2etha( snap(n) )
        pse_src_qemvoc(n, 4) = pse_src_qemvec(n, 2)  * voc2aldh( snap(n) )
        pse_src_qemvoc(n, 5) = pse_src_qemvec(n, 2)  * voc2buta( snap(n) )
        pse_src_qemvoc(n, 6) = pse_src_qemvec(n, 2)  * voc2ethe( snap(n) )
        pse_src_qemvoc(n, 7) = pse_src_qemvec(n, 2)  * voc2prop( snap(n) )
        pse_src_qemvoc(n, 8) = pse_src_qemvec(n, 2)  * voc2oxyl( snap(n) )
        pse_src_qemvoc(n, 9) = pse_src_qemvec(n, 2)  * voc2meke( snap(n) )
        if (snap(n) .eq. 11) then
           pse_src_qemvoc(n,10) = pse_src_qemvec(n, 2) * fctoisop
        else
           pse_src_qemvoc(n,10) = 0.0
        endif

! *** houd - hour of day   (1-24) h=1 is first hour
! *** dayw - day of week   (1-7 )
! *** mony - month of year (1-12)
!!!==============================================
!!! changed by Kang @ CARES
!!! change the simluation time appeared in point source output file  
!!! orig:        houd  = 1
        houd  = hour+1
!!!==================================================        
        daymm = daym
        dayw  = dayweek
        mony  = mnth
        yeai  = year
        call cdayyear(yeai,mony,daymm,dayy)

        do h = 1, nhours

       ! different hourf for weekdays and weekend? dayw=6 and dayw=7

          ! write this line to log file:
          !print *,'PSE day of week', h, mony, daymm, dayw

          if (dayw < 6) then
            timedis( snap(n) ) = hourfw(snap(n),houd)*daywf(snap(n),dayw)*mnthf(snap(n),mony)
          else
            timedis( snap(n) ) = hourfe(snap(n),houd)*daywf(snap(n),dayw)*mnthf(snap(n),mony)
          endif

        ! biogenic emissions
          if ( (snap(n)==11) .and.(fe_in_tapm) ) then
            ! daily variation of T, hourly variation of P
            timedis( snap(n) ) = gammat_dayy(dayy,1,1) * scaleisop(dayy,houd)
          endif


!!!!==============================================================================
!!! changed by kang @ CARES.  
!!! dont need time factor if hourly or accurate emission data is known. 

!!! read from input control file - 'cctamp.inp'
           if(ntime_factor .eq. 0) timedis( snap(n))= 1.0
!!!!==============================================================================


        ! original pollutants
          pse_src_qemhour(n, 1,h) = pse_src_qemvec(n,1) * timedis(snap(n))
          pse_src_qemhour(n, 2,h) = pse_src_qemvec(n,2) * timedis(snap(n))
          pse_src_qemhour(n, 3,h) = pse_src_qemvec(n,3) * timedis(snap(n))
          pse_src_qemhour(n, 4,h) = pse_src_qemvec(n,4) * timedis(snap(n))
          pse_src_qemhour(n, 5,h) = pse_src_qemvec(n,5) * timedis(snap(n))
          pse_src_qemhour(n, 6,h) = pse_src_qemvec(n,6) * timedis(snap(n))
          pse_src_qemhour(n, 7,h) = pse_src_qemvec(n,7) * timedis(snap(n))

        ! NO and NO2 and HONO
          pse_src_qemhour(n, 8,h) =    pse_src_nox(n,1) * timedis(snap(n))
          pse_src_qemhour(n, 9,h) =    pse_src_nox(n,2) * timedis(snap(n))
          pse_src_qemhour(n,20,h) =    pse_src_nox(n,3)   ! hono

        ! RSMOG
          pse_src_qemhour(n,10,h) = pse_src_qemvoc(n,1) * timedis(snap(n))
        ! NMVOC
          pse_src_qemhour(n,11,h) = pse_src_qemvoc(n,2) * timedis(snap(n))
          pse_src_qemhour(n,12,h) = pse_src_qemvoc(n,3) * timedis(snap(n))
          pse_src_qemhour(n,13,h) = pse_src_qemvoc(n,4) * timedis(snap(n))
          pse_src_qemhour(n,14,h) = pse_src_qemvoc(n,5) * timedis(snap(n))
          pse_src_qemhour(n,15,h) = pse_src_qemvoc(n,6) * timedis(snap(n))
          pse_src_qemhour(n,16,h) = pse_src_qemvoc(n,7) * timedis(snap(n))
          pse_src_qemhour(n,17,h) = pse_src_qemvoc(n,8) * timedis(snap(n))
          pse_src_qemhour(n,18,h) = pse_src_qemvoc(n,9) * timedis(snap(n))

          if (snap(n) .eq. 11) then
            pse_src_qemhour(n,19,h) = pse_src_qemvoc(n,10) * timedis(snap(n))
          else
            pse_src_qemhour(n,19,h) = 0.0
          end if


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
          endif


        enddo
      enddo

        ! print *,'snap cat.',snap(:)
        ! print *,'pse_src_param ',pse_src_param(:,:)


      return


      end subroutine emission_points
