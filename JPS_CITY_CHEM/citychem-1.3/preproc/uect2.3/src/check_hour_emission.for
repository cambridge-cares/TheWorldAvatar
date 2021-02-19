! <check_hour_emission.for - A component of the City-scale
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

      subroutine check_hour_emission(incols,nhours,npoints,npoints_new,  &
                       data_array, snap, out_src_qemhour)

!***********************************************************************
!***  Subroutine check_hour_emission compares the sums of the hourly 
!***  emissions (g/s) to the emission totals (kg/year) for a random
!***  sample
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
      integer, dimension(npoints_new),intent(in)     :: snap
      real, dimension(npoints,incols),intent(in)     :: data_array
      real, dimension(npoints_new,ncomp,nhours),intent(in) :: out_src_qemhour

!     Local declarations:

      integer                         :: h,so
      real                            :: tothrs
      real                            :: cntrl_conv
      real                            :: total_conv
      real                            :: yrfrac
      real                            :: controlsum_nox
      real                            :: controlsum_voc
      real                            :: controlsum_co
      real                            :: controlsum_pmc
      real                            :: total_nox
      real                            :: total_voc
      real                            :: total_co
      real                            :: total_pmc

!***********************************************************************
!     Content of subroutine:

      print *,'check_hour: snap',npoints_new,snap

! *** Total hours per year
      tothrs  = 365.*24.


! *** We compare the emissions for one year
! *** from the emission input file
! *** total_poll
! *** and from then hourly emission fields 
! *** calculated in the emission routine
! *** control_poll  (unit: g/s)
! *** Unit for comparison is kg/year

      if (source=='PSE') then
         cntrl_conv = 3600*1.e-3
         total_conv   = 1.0
      else if (source=='LSE') then
         cntrl_conv = 3600*1.e-3
      ! convert total_poll emission g/s --> kg/year
         total_conv = tothrs*3600*1.e-3
      else if (source=='ASE') then
         cntrl_conv = 3600*1.e-3
         total_conv = 1.0
      endif

      
      if (fe_log) then
        write(funit_log,'(1X,A56,A7,A10,A10)')        &
        '                                       ',     &
        'source','control','input'
         write(funit_log,'(1X,A56,A7,A10,A10)')        &
        '                                           ', &
        'no.  ','kg/year','kg/year'

      endif


!     Loop over filtered point sources

      do so = 1, npoints_new

! *** Get total emissions
! *** NOx     NMVOC   CO     SO2     NH3    PM25    PM10
! *** 1       2       3      4       5      6       7
!     Check emissions of NOX, NMVOC and CO

        controlsum_nox = 0.
        controlsum_voc = 0.
        controlsum_co  = 0.
        controlsum_pmc = 0.
        total_nox      = 0.
        total_voc      = 0.
        total_co       = 0.
        total_pmc      = 0.

        ! convert to unit kg/hour and sum up for all hours --> kg per period nhours
        do h = 1, nhours
          if(snap(so)/=2) controlsum_nox = controlsum_nox + out_src_qemhour(so,1,h) * cntrl_conv
          if(snap(so)/=2) controlsum_voc = controlsum_voc + out_src_qemhour(so,2,h) * cntrl_conv
          if(snap(so)/=2) controlsum_co  = controlsum_co  + out_src_qemhour(so,3,h) * cntrl_conv
          if(snap(so)/=2) controlsum_pmc = controlsum_pmc + out_src_qemhour(so,7,h) * cntrl_conv
        enddo
       
       print *,so,mnth
       
! *** Correct for month time factor
        if (nhours < 760) then
          controlsum_nox = controlsum_nox / mnthf(snap(so),mnth)
          controlsum_voc = controlsum_voc / mnthf(snap(so),mnth)
          controlsum_co  = controlsum_co  / mnthf(snap(so),mnth)
          controlsum_pmc = controlsum_pmc / mnthf(snap(so),mnth)
        endif

! **** Extrapolate to the full year kg/nhours --> kg/year 
        yrfrac = tothrs/nhours
        controlsum_nox = controlsum_nox * yrfrac
        controlsum_voc = controlsum_voc * yrfrac
        controlsum_co  = controlsum_co  * yrfrac
        controlsum_pmc = controlsum_pmc * yrfrac 

        
! *** Total yearly emission (kg/year)
        if(snap(so)/=2)  total_nox      = data_array(so, 7) * total_conv
        if(snap(so)/=2)  total_voc      = data_array(so, 8) * total_conv
        if(snap(so)/=2)  total_co       = data_array(so, 9) * total_conv
        if(snap(so)/=2)  total_pmc      = data_array(so,13) * total_conv
        

! *** Compare control sum to input emission
! *** Allow deviation of 5 % for month
! *** Print to Log-file

        ! print *,'check source ',so,' snap ',snap(so)

        if (data_array(so, 7).gt.0) then
          if ((controlsum_nox.lt.total_nox*1.07).and.(controlsum_nox.gt.total_nox*0.93)) then
             if (fe_log) then
               write(funit_log,'(1X,A58,I6,E10.2,E10.2)')  &
                 'NOx control sum and input sum agree within 7%',so,controlsum_nox,total_nox
             endif
          else
             if (controlsum_nox/=0.0) then
               print *,'NOx control sum does not match: ',so,controlsum_nox,total_nox
               call stopit('NOx Emission sums do not agree')
             endif
          endif
        else
          if (fe_log) then
            write(funit_log,'(1X,A58,I6)')  &
              'NOx emission missing for no.',so
          endif
        endif

        if (data_array(so, 8).gt.0) then
          if ((controlsum_voc.lt.total_voc*1.07).and.(controlsum_voc.gt.total_voc*0.93)) then
             if (fe_log) then
               write(funit_log,'(1X,A58,I6,E10.2,E10.2)')  &
                 'VOC control sum and input sum agree within 7%',so,controlsum_voc,total_voc
             endif
          else
             if ( (controlsum_voc/=0.0).and.(snap(so)/=11) ) then
               print *,'VOC control sum does not match: ',so,controlsum_voc,total_voc
               call stopit('VOC Emission sums do not agree')
             endif
             if (snap(so)==11) then
               write(funit_log,'(1X,A58,I6,E10.2,E10.2)')  &
                 'VOC control sum: SNAP11 (G97 isoprene)',so,controlsum_voc,total_voc
             endif

          endif
        else
          if (fe_log) then
            write(funit_log,'(1X,A58,I6)')  &
              'VOC emission missing for source no.',so
          endif
        endif

        if (data_array(so, 9).gt.0) then
          if ((controlsum_co.lt.total_co*1.07).and.(controlsum_co.gt.total_co*0.93)) then
             if (fe_log) then
               write(funit_log,'(1X,A58,I6,E10.2,E10.2)')  &
                 'CO  control sum and input sum agree within 7%',so,controlsum_co,total_co
             endif
          else
             if (controlsum_co/=0.0) then
               print *,'CO  control sum does not match: ',so,controlsum_co,total_co
               call stopit('CO Emission sums do not agree')
             endif
          endif
        else
          if (fe_log) then
            write(funit_log,'(1X,A58,I6)')  &
              'CO emission missing for source no.',so
          endif
        endif

        if (data_array(so, 13).gt.0) then
          if ((controlsum_pmc.lt.total_pmc*1.07).and.(controlsum_pmc.gt.total_pmc*0.93)) then
             if (fe_log) then
               write(funit_log,'(1X,A58,I6,E10.2,E10.2)')  &
                 'PM10 control sum and input sum agree within 7%',so,controlsum_pmc,total_pmc
             endif
          else
             if (controlsum_pmc/=0.0) then
               print *,'PM10 control sum does not match: ',so,controlsum_pmc,total_pmc
               call stopit('PM10 Emission sums do not agree')
             endif
          endif
        else
          if (fe_log) then
            write(funit_log,'(1X,A58,I6)')  &
              'PM10 emission missing for source no.',so
          endif

        endif
      enddo   !new_points


      return


      end subroutine check_hour_emission
