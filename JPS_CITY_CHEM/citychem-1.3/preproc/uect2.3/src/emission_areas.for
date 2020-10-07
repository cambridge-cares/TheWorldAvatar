! <emission_areas.for - A component of the City-scale
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

      subroutine emission_areas(incols,nareas,nareas_new,nhours,        &
                       data_array,snap,xi,yi,ase_src_snap,              &
                       ase_src_param,ase_src_qemhour)

!***********************************************************************
!***  Subroutine emision_areas converts the emission totals (kg/year)
!***  to hourly emissions (g/s) for areas sources
!***********************************************************************
!
! Modification change log
! -----------------------
! 24.07.2018 M.Karl(HZG) L116-118  Don't calculate heat degree if no TAPM file
! 27.08.2018 M.Karl(HZG) L124-126  Stop if SNAP sector of a source is > 10
! 11.09.2018 M.Karl(HZG) L121-123  Call to calc_gammatisop_days
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
      integer, intent(in)             :: nareas
      integer, intent(in)             :: nareas_new
      integer, intent(in)             :: nhours
      integer, dimension(nareas), intent(in)           :: xi
      integer, dimension(nareas), intent(in)           :: yi
      integer, dimension(nareas),intent(in)            :: snap
      real, dimension(nareas,incols),intent(in)        :: data_array
!out
      real, dimension(nareas_new,ncomp,nhours),intent(out) :: ase_src_qemhour
      real, dimension(nareas_new,n_ase_params),intent(out) :: ase_src_param
      integer, dimension(nareas_new),intent(out)           :: ase_src_snap

!     Local declarations:

      integer                                :: i, j, n, h
      integer                                :: houd
      integer                                :: yeai
      integer                                :: mony
      integer                                :: dayw
      integer                                :: daymm
      integer                                :: dayy
      integer                                :: nxe
      integer                                :: nye 
      real, dimension(nareas,n_inp_compds)   :: ase_src_qemvec
      real, dimension(nareas,n_voc_compds)   :: ase_src_qemvoc
      real, dimension(nareas,n_nox_compds)   :: ase_src_nox
      real, dimension(n_snap)                :: timedis
      real,dimension(366,n_ny,n_nx)          :: fc_dayy
      real,dimension(366,n_ny,n_nx)          :: gammat_dayy
      real,dimension(366,24)                 :: scaleisop 
      real,dimension(nareas_new)             :: efisop
      real,dimension(nareas_new)             :: efapin
      real,dimension(nareas_new)             :: eflimo

!***********************************************************************
!     Content of subroutine:

! *** Open and read temperature file
      fname_nc_airtemp = trim(fname_inpath_tapm)//'/T_and_dtdz_'//trim(startdate)//'_'//trim(enddate)//'.nc'

      if (fe_in_bvocrr) then
         fname_nc_bvocef  = trim('./testinput/'//'BVOC_4_UECT_RheinRuhr.nc')
         nxe = 1051
         nye = 1059
      endif

      if (fe_in_bvochh) then
         fname_nc_bvocef  = trim('./testinput/'//'BVOC_4_UECT_HH.nc')
         nxe = 1021
         nye = 1018
      endif

! *** calculate heating degree daily factor as function of day, x and y (3-dim)
      if (fe_in_tapm) then
        call calc_heatdegree_days(nhours,fname_nc_airtemp,fc_dayy)
      end if

! *** this calculates the pseudo-temperature dependent isoprene emission activity
! *** deactivate this function once the bvoc emission potential can be calculated
      if (fe_in_tapm) then
        call calc_gammatisop_days(nhours,fname_nc_airtemp,gammat_dayy,scaleisop)
      end if

      write(6,*) 'TAPM T_and_dtdz ncfile: ',fname_nc_airtemp
      write(6,*) 'BVOC EF and FD ncfile:  ',fname_nc_bvocef
      write(6,*) 'Option Air-T and BVOC:  ',fe_in_tapm,fe_in_bvoc


! *** try to read the bvoc pre-emission data
! *** A short check if there exists SNAP11 area source and if not,
! *** jump over the next time-consuming routine
      if (fe_in_bvoc) then
        call calc_bvocemis_activ(incols,nareas,nareas_new,snap,data_array, &
                                 nxe,nye,fname_nc_bvocef,efisop,efapin,eflimo)
      end if


      do n = 1, nareas_new

! *** Check SNAP sector, must be 1-10
        if ( (snap(n).gt.11) .or. (snap(n).lt.1) ) then
          call stopit('SNAP sector for area source has to be between 1 and 11')
        end if

! *** SNAP of the filtered point sources
        ase_src_snap(n)  = snap(n)

! *** Get source parameters
! *** xcor_sw ycor_sw zcor_sw xcor_ne ycor_ne zcor_ne
! *** 1       2       3       4       5       6
! *** TAPM ase format meta data
! *** x0     y0       h0      x1      y1      h1  f_no  f_fpm
! *** 1      2        3       4       5       6   7     8

        ! X SW-coordinate
        if (data_array(n,1).gt.missval) then
          ase_src_param(n, 1) = data_array(n,1)
        else
          print *,'X SW-coor missing for area source ',n
          call stopit('Area source with missing X SW-coordinate')
        endif
        ! Y SW-coordinate
        if (data_array(n,2).gt.missval) then
          ase_src_param(n, 2) = data_array(n,2)
        else
          print *,'Y SW-coor missing for point source ',n
          call stopit('Area source with missing Y SW-coordinate')
        endif
        ! Z SW-coordinate = elevation (m)
        if (data_array(n,3).gt.missval) then
          ase_src_param(n, 3) = data_array(n,3)
        else
          ase_src_param(n, 3) = 0.0
        endif
        ! X NE-coordinate
        if (data_array(n,4).gt.missval) then
          ase_src_param(n, 4) = data_array(n,4)
        else
          print *,'X NE-coor missing for line source ',n
          call stopit('Area source with missing X NE-coordinate')
        endif
        ! Y NE-coordinate
        if (data_array(n,5).gt.missval) then
          ase_src_param(n, 5) = data_array(n,5)
        else
          print *,'Y NE-coor missing for point source ',n
          call stopit('Area source with missing Y NE-coordinate')
        endif
        ! Z NE-coordinate = elevation (m)
        if (data_array(n,6).gt.missval) then
          ase_src_param(n, 6) = data_array(n,6)
        else
          ase_src_param(n, 6) = 0.0
        endif


! *** Get total emissions
! *** data_array(n,7:13)
! *** NOx     NMVOC   CO     SO2     NH3    PM25    PM10
! *** 1       2       3      4       5      6       7
        j = 1
        do i = 7, 13
          if (data_array(n, i).gt.missval) then
            ase_src_qemvec(n, j) = data_array(n, i) * ase_funit
          else
            ase_src_qemvec(n, j) = 0.0
          endif
          j = j +1
        enddo

! Values for some of the required area parameters
! need to be calculated based on emission ratios
!  f_no   = NO/NOx
!  f_fpm  = PM2.5/PM10
        ase_src_param(n, 7) = 1.0
        ase_src_param(n, 8) = 1.0


! *** Split VOC and NOx to CityChem / TAPM specific emissions
! *** For NOx the standard split for all sectors is NO:NO2  95:5
! *** RSMOG = 0.0067*NMVOC; for TAPM (tapm v4.0 docu, pp. 27; cite Johnson, 1984)
! *** VOC split depends on SNAP
! *** Isoprene emissions given in kgC/year in SNAP11
        ase_src_nox(n,    1) = ase_src_qemvec(n, 1)  * 0.95       ! NO
        ase_src_nox(n,    2) = ase_src_qemvec(n, 1)  * 0.05       ! NO2
        ase_src_qemvoc(n, 1) = ase_src_qemvec(n, 2)  * voc2rsmog
        ase_src_qemvoc(n, 2) = ase_src_qemvec(n, 2)  * voc2hcho( snap(n) )
        ase_src_qemvoc(n, 3) = ase_src_qemvec(n, 2)  * voc2etha( snap(n) )
        ase_src_qemvoc(n, 4) = ase_src_qemvec(n, 2)  * voc2aldh( snap(n) )
        ase_src_qemvoc(n, 5) = ase_src_qemvec(n, 2)  * voc2buta( snap(n) )
        ase_src_qemvoc(n, 6) = ase_src_qemvec(n, 2)  * voc2ethe( snap(n) )
        ase_src_qemvoc(n, 7) = ase_src_qemvec(n, 2)  * voc2prop( snap(n) )
        ase_src_qemvoc(n, 8) = ase_src_qemvec(n, 2)  * voc2oxyl( snap(n) )
        ase_src_qemvoc(n, 9) = ase_src_qemvec(n, 2)  * voc2meke( snap(n) )
        if (snap(n) .eq. 11) then
          if (fe_in_bvoc) then
             ase_src_qemvoc(n,10) = efisop(n)*ase_src_qemvec(n, 2)/ase_funit
             ase_src_qemvoc(n,11) = efapin(n)*ase_src_qemvec(n, 2)/ase_funit
             ase_src_qemvoc(n,12) = eflimo(n)*ase_src_qemvec(n, 2)/ase_funit
          else
             ase_src_qemvoc(n,10) = ase_src_qemvec(n, 2) * fctoisop
             ase_src_qemvoc(n,11) = 0.0
             ase_src_qemvoc(n,12) = 0.0
          endif
        else
           ase_src_qemvoc(n,10) = 0.0
           ase_src_qemvoc(n,11) = 0.0
           ase_src_qemvoc(n,12) = 0.0
        endif

      !  print *,'emis voc',n,snap(n),data_array(n, :),ase_src_qemvec(n, 2),ase_src_qemvoc(n, 2),ase_src_qemvoc(n,10)

! *** houd - hour of day   (1-24) h=1 is first hour
! *** dayw - day of week   (1-7 )
! *** mony - month of year (1-12)
        houd  = 1
        daymm = daym
        dayw  = dayweek
        mony  = mnth
        yeai  = year
        call cdayyear(yeai,mony,daymm,dayy)
        write(6,*) 'area src n', n

        do h = 1, nhours

       ! different hourf for weekdays and weekend? dayw=6 and dayw=7

          ! write this line to log file:
          !print *,'PSE day of week', h, mony, daymm, dayw
         
          if ( (snap(n)==2) .and.(fe_in_tapm) ) then
            ! no month variation needed if daily factors are used
            if (dayw < 6) then
              timedis( snap(n) ) = hourfw(snap(n),houd)* fc_dayy(dayy,yi(n),xi(n))
            else
              timedis( snap(n) ) = hourfe(snap(n),houd)* fc_dayy(dayy,yi(n),xi(n))
            endif
            !write(6,*) 'N, DAY,hr, X, Y, fcday, f_all', n,dayy,h,xi(n),yi(n),fc_dayy(dayy,yi(n),xi(n)),timedis(snap(n))
          else
            if (dayw < 6) then
              timedis( snap(n) ) = hourfw(snap(n),houd)*daywf(snap(n),dayw)*mnthf(snap(n),mony)
            else
              timedis( snap(n) ) = hourfe(snap(n),houd)*daywf(snap(n),dayw)*mnthf(snap(n),mony)
            endif
            !if (snap(n)==10)   then
            !    write(6,*) 'N, DAY, X, Y, f_mnth, f_all', n,dayy,xi(n),yi(n),mnthf(snap(n),mony),timedis(snap(n))
            !endif
          endif

        ! biogenic emissions
        ! daily variation of T, hourly variation of P
          if (snap(n)==11) then
            if (fe_in_tapm) then
              timedis( snap(n) ) = gammat_dayy(dayy,yi(n),xi(n))
            else
              timedis( snap(n) ) = 1.0
            endif
          endif

        ! original pollutants
          ase_src_qemhour(n, 1,h) = ase_src_qemvec(n,1) * timedis(snap(n))
          ase_src_qemhour(n, 2,h) = ase_src_qemvec(n,2) * timedis(snap(n))
          ase_src_qemhour(n, 3,h) = ase_src_qemvec(n,3) * timedis(snap(n))
          ase_src_qemhour(n, 4,h) = ase_src_qemvec(n,4) * timedis(snap(n))
          ase_src_qemhour(n, 5,h) = ase_src_qemvec(n,5) * timedis(snap(n))
          ase_src_qemhour(n, 6,h) = ase_src_qemvec(n,6) * timedis(snap(n))
          ase_src_qemhour(n, 7,h) = ase_src_qemvec(n,7) * timedis(snap(n))

        ! NO and NO2
          ase_src_qemhour(n, 8,h) =    ase_src_nox(n,1) * timedis(snap(n))
          ase_src_qemhour(n, 9,h) =    ase_src_nox(n,2) * timedis(snap(n))

        ! RSMOG
          ase_src_qemhour(n,10,h) = ase_src_qemvoc(n,1) * timedis(snap(n))
        ! NMVOC
          ase_src_qemhour(n,11,h) = ase_src_qemvoc(n,2) * timedis(snap(n))
          ase_src_qemhour(n,12,h) = ase_src_qemvoc(n,3) * timedis(snap(n))
          ase_src_qemhour(n,13,h) = ase_src_qemvoc(n,4) * timedis(snap(n))
          ase_src_qemhour(n,14,h) = ase_src_qemvoc(n,5) * timedis(snap(n))
          ase_src_qemhour(n,15,h) = ase_src_qemvoc(n,6) * timedis(snap(n))
          ase_src_qemhour(n,16,h) = ase_src_qemvoc(n,7) * timedis(snap(n))
          ase_src_qemhour(n,17,h) = ase_src_qemvoc(n,8) * timedis(snap(n))
          ase_src_qemhour(n,18,h) = ase_src_qemvoc(n,9) * timedis(snap(n))
          if (snap(n) .eq. 11) then
            ase_src_qemhour(n,19,h) = ase_src_qemvoc(n,10) * timedis(snap(n)) &    !isop
                                      *scaleisop(dayy,houd)
            ase_src_qemhour(n,20,h) = ase_src_qemvoc(n,11) * timedis(snap(n))      !apin
            ase_src_qemhour(n,21,h) = ase_src_qemvoc(n,12) * scaleisop(dayy,houd)  !lim

            !print *,'emis voc hour',n,snap(n),ase_src_qemhour(n,19,h),houd,gammat_dayy(dayy,yi(n),xi(n)),scaleisop(dayy,houd)
          else
            ase_src_qemhour(n,19,h) = 0.0
            ase_src_qemhour(n,20,h) = 0.0
            ase_src_qemhour(n,21,h) = 0.0
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
! *** Get day of year (for heating degree days)
             call cdayyear(yeai,mony,daymm,dayy)

          endif


        enddo
      enddo

         !print *,'snap cat.',snap(:)
         !print *,'ase_src_param ',ase_src_param(:,:)
         !stop

     !    if (allocated(fc_dayy))         deallocate(fc_dayy)

      return


      end subroutine emission_areas
