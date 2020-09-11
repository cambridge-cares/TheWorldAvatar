! <emission_lines.for - A component of the City-scale
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

      subroutine emission_lines(incols,nlines,nlines_new,nhours,     &
                       data_array,snap, lse_src_snap, lse_src_param,    &
                       lse_src_qemhour)

!***********************************************************************
!***  Subroutine emision_lines converts the emission totals (g/s per
!***  year) to hourly emissions (g/s per hour) for line sources
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
      integer, intent(in)             :: nlines
      integer, intent(in)             :: nlines_new
      integer, intent(in)             :: nhours
      integer, dimension(nlines),intent(in)            :: snap
      real, dimension(nlines,incols),intent(in)        :: data_array
!out
      real, dimension(nlines_new,ncomp,nhours),intent(out) :: lse_src_qemhour
      real, dimension(nlines_new,n_lse_params),intent(out) :: lse_src_param
      integer, dimension(nlines_new),intent(out)           :: lse_src_snap

!     Local declarations:

      integer                                :: i, j, n, h
      integer                                :: houd
      integer                                :: yeai
      integer                                :: mony
      integer                                :: dayw
      integer                                :: daymm
      real, dimension(nlines,n_inp_compds)   :: lse_src_qemvec
      real, dimension(nlines,n_voc_compds)   :: lse_src_qemvoc
      real, dimension(nlines,n_nox_compds)   :: lse_src_nox
      real, dimension(n_snap)                :: timedis


!***********************************************************************
!     Content of subroutine:


      do n = 1, nlines_new

! *** SNAP of the filtered point sources
        lse_src_snap(n)  = snap(n)

! *** Get source parameters
! *** xcor_start;ycor_start;xcor_end;ycor_end;Elevation;Width
! *** 1          2          3        4        5         6     
! *** X-coo1  X-coo2  Y-coo1  Y-coo2   Z-coo1 Z-coo2   Width   MaxInfL QLR
! *** 1       2       3       4        5       6       7       8       9
! *** X1V    X2V      Y1V     Y2V      Z1V     Z2V     WV      RMAXV   INDICES

        ! X start-coordinate
        if (data_array(n,1).gt.missval) then
          lse_src_param(n, 1) = data_array(n,1)
        else
          print *,'X start-coor missing for area source ',n
          call stopit('Line source with missing X start-coordinate')
        endif
        ! X end-coordinate
        if (data_array(n,3).gt.missval) then
          lse_src_param(n, 2) = data_array(n,3)
        else
          print *,'X end-coor missing for area source ',n
          call stopit('Line source with missing X end-coordinate')
        endif
        ! Y start-coordinate
        if (data_array(n,2).gt.missval) then
          lse_src_param(n, 3) = data_array(n,2)
        else
          print *,'Y start-coor missing for area source ',n
          call stopit('Line source with missing Y start-coordinate')
        endif
        ! Y end-coordinate
        if (data_array(n,4).gt.missval) then
          lse_src_param(n, 4) = data_array(n,4)
        else
          print *,'Y end-coor missing for area source ',n
          call stopit('Line source with missing Y end-coordinate')
        endif
        ! Z start-coordinate = elevation (m)
        if (data_array(n,5).gt.missval) then
          lse_src_param(n, 5) = data_array(n,5)
        else
          lse_src_param(n, 5) = 0.0
        endif
        ! Z end-coordinate = elevation (m)
        if (data_array(n,6).gt.missval) then
          lse_src_param(n, 6) = data_array(n,5)
        else
          lse_src_param(n, 6) = 0.0
        endif
        ! Width of Lane = Width of street/2
        if (data_array(n,6).gt.missval) then
          lse_src_param(n, 7) = data_array(n,6)   !street  (CC uses lane)
        else
          lse_src_param(n, 7) = 12.0
        endif

! Default values for some of the required line parameters
        lse_src_param(n, 8) = dxout*0.5   !RMAX = dx/2
        lse_src_param(n, 9) = 0.0         !QLR Index 0|1 (integer)



! *** Get total emissions
! *** data_array(n,7:13)
! *** NOx     NMVOC   CO     SO2     NH3    PM25    PM10
! *** 1       2       3      4       5      6       7
        j = 1
        do i = 7, 13
          if (data_array(n, i).gt.missval) then
            lse_src_qemvec(n, j) = data_array(n, i) * lse_funit
          else
            lse_src_qemvec(n, j) = 0.0
          endif
          j = j +1
        enddo


! *** Split VOC and NOx to CityChem / TAPM specific emissions
! *** For NOx the standard split for line sources is NO:NO2  90:10
! *** (based on Carslaw & Beevers, AE2005: 10.6 vol% NO2)
! *** Changed on 11.09.2017 to NO:NO2 80:20
! *** Changed on 01.11.2017 to NO:NO2 70:30
! *** Changed on 12.08.2019 to include 2% HONO in the NOx split
! *** RSMOG = 0.0067*NMVOC; for TAPM (tapm v4.0 docu, pp. 27; cite Johnson, 1984)
! *** VOC split depends on SNAP
        lse_src_nox(n,    1) = lse_src_qemvec(n, 1)  * f_no_lse        ! NO
        lse_src_nox(n,    2) = lse_src_qemvec(n, 1)  * (1.0-f_no_lse-f_hono_lse)  ! NO2
        lse_src_nox(n,    3) = lse_src_qemvec(n, 1)  * f_hono_lse      ! HONO
        lse_src_qemvoc(n, 1) = lse_src_qemvec(n, 2)  * voc2rsmog
        lse_src_qemvoc(n, 2) = lse_src_qemvec(n, 2)  * voc2hcho( snap(n) )
        lse_src_qemvoc(n, 3) = lse_src_qemvec(n, 2)  * voc2etha( snap(n) )
        lse_src_qemvoc(n, 4) = lse_src_qemvec(n, 2)  * voc2aldh( snap(n) )
        lse_src_qemvoc(n, 5) = lse_src_qemvec(n, 2)  * voc2buta( snap(n) )
        lse_src_qemvoc(n, 6) = lse_src_qemvec(n, 2)  * voc2ethe( snap(n) )
        lse_src_qemvoc(n, 7) = lse_src_qemvec(n, 2)  * voc2prop( snap(n) )
        lse_src_qemvoc(n, 8) = lse_src_qemvec(n, 2)  * voc2oxyl( snap(n) )
        lse_src_qemvoc(n, 9) = lse_src_qemvec(n, 2)  * voc2meke( snap(n) )

! *** houd - hour of day   (1-24) h=1 is first hour
! *** dayw - day of week   (1-7 )
! *** mony - month of year (1-12)
        houd  = 1
        daymm = daym
        dayw  = dayweek
        mony  = mnth
        yeai  = year

        do h = 1, nhours

       ! different hourf for weekdays and weekend? dayw=6 and dayw=7

          ! write this line to log file:
          !print *,'LSE day of week', h, mony, daymm, dayw

          if (dayw < 6) then
            timedis( snap(n) ) = hourfw(snap(n),houd)*daywf(snap(n),dayw)*mnthf(snap(n),mony)
            !special summer snap7 (MJJ)
            if ((snap(n)==7).and.( (mony==5).or.(mony==6).or.(mony==7) )) then
              timedis( snap(n) ) = hourfws(houd)*daywf(snap(n),dayw)*mnthf(snap(n),mony)
            endif
          else
            timedis( snap(n) ) = hourfe(snap(n),houd)*daywf(snap(n),dayw)*mnthf(snap(n),mony)
          endif

        ! original pollutants
          lse_src_qemhour(n, 1,h) = lse_src_qemvec(n,1) * timedis(snap(n))
          lse_src_qemhour(n, 2,h) = lse_src_qemvec(n,2) * timedis(snap(n))
          lse_src_qemhour(n, 3,h) = lse_src_qemvec(n,3) * timedis(snap(n))
          lse_src_qemhour(n, 4,h) = lse_src_qemvec(n,4) * timedis(snap(n))
          lse_src_qemhour(n, 5,h) = lse_src_qemvec(n,5) * timedis(snap(n))
          lse_src_qemhour(n, 6,h) = lse_src_qemvec(n,6) * timedis(snap(n))
          lse_src_qemhour(n, 7,h) = lse_src_qemvec(n,7) * timedis(snap(n))

        ! NO and NO2 and HONO
          lse_src_qemhour(n, 8,h) =    lse_src_nox(n,1) * timedis(snap(n))
          lse_src_qemhour(n, 9,h) =    lse_src_nox(n,2) * timedis(snap(n))
          lse_src_qemhour(n,22,h) =    lse_src_nox(n,3) * timedis(snap(n))

        ! RSMOG
          lse_src_qemhour(n,10,h) = lse_src_qemvoc(n,1) * timedis(snap(n))
        ! NMVOC
          lse_src_qemhour(n,11,h) = lse_src_qemvoc(n,2) * timedis(snap(n))
          lse_src_qemhour(n,12,h) = lse_src_qemvoc(n,3) * timedis(snap(n))
          lse_src_qemhour(n,13,h) = lse_src_qemvoc(n,4) * timedis(snap(n))
          lse_src_qemhour(n,14,h) = lse_src_qemvoc(n,5) * timedis(snap(n))
          lse_src_qemhour(n,15,h) = lse_src_qemvoc(n,6) * timedis(snap(n))
          lse_src_qemhour(n,16,h) = lse_src_qemvoc(n,7) * timedis(snap(n))
          lse_src_qemhour(n,17,h) = lse_src_qemvoc(n,8) * timedis(snap(n))
          lse_src_qemhour(n,18,h) = lse_src_qemvoc(n,9) * timedis(snap(n))
          lse_src_qemhour(n,19,h) = 0.0   ! isoprene

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


        !  print *,'snap cat.',snap(:)

          ! print *,'lse_src_param ',lse_src_param(:,:)


      return


      end subroutine emission_lines
