! <get_user_input.for - A component of the City-scale
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
! ----------------------------------------------------------------------------------
! Original source code of this routine is written by Leif Harvard Slørdal (NILU)
!
! Contact Information:
! Sam-Erik Walker
! Norwegian Institute for Air Research (NILU)
! Instituttveien 18 P.O. Box 100
! N-2027 Kjeller, NORWAY
! Tel: +47 63898000 Fax: +47 63898050
! E-mail: sam-erik.walker@nilu.no
!
!!!!!========================================================================
!!!  modified by Kang @ CARES, @ Dec. 2019
!!   1, add time factor control variable for point source emission rate calculation;  
!       ntime_factor=0, don't use time factor; =1, use the default time factor   
!!   2, change start and end simulation date value so that the correct date (hour) can be 
!!      printed in point source output file.    
! ----------------------------------------------------------------------------------

      subroutine get_user_input

!***********************************************************************
!***  Subroutine get_user_input reads the meta information
!***  for preparation of emission input files for city-scale
!***  air quality models CityChem and TAPM
!***********************************************************************

!***  NOTE: THE META INFORMATION MAY BE EXTENDED FOR METEO AND BCON

!     Declarations of variables by using the MODULES feature:

      use module_uect_io
      use module_uect_exe
      use module_uect_time


      implicit none

!***********************************************************************

!     Local declarations:

      integer :: i
      integer :: word_len
      
!***********************************************************************
!     Content of subroutine:


!     Definition of some simple input and output files:

      funit_run  = 10
      fname_run  = 'cctapm_meta.inp'

      open (unit=funit_run, file=fname_run,status='old')

!     Start reading parameters from the meta info file:

!     SIM-ID
      read(funit_run,*) simid

!     Various file-names:
!
!     Input path for TAPM and CMAQ data (not used)
      read(funit_run,*) fname_inpath_tapm
!     TAPM file for temperature is used in SNAP2 emission calculation
!     if it is not present, the normal way to calculate SNAP2 is used
      read(funit_run,*) fname_inpath_cmaq

!     Files containing input-data:
      read(funit_run,*) fname_in_points
      read(funit_run,*) fname_in_lines
      read(funit_run,*) fname_in_area_sector

!
!     Check if files exists
      INQUIRE(FILE=fname_in_points,EXIST = fe_in_points)
      INQUIRE(FILE=fname_in_lines,EXIST = fe_in_lines)
      INQUIRE(FILE=fname_in_area_sector,EXIST = fe_in_area_sector)

!     Files containing output-data:
      read(funit_run,*) fname_outpath

!     Log file
      read(funit_run,*) fname_log
      !print *,'fname_log ',fname_log

!     Open Log file
      funit_log  = 11
      
      fe_log     = .false.
      if (fname_log(1:1) /= ' ') then
        open (unit=funit_log, file=fname_log,status='unknown')
        fe_log = .true.
      end if

!     Selected model
      read(funit_run,*) model
      !print *,'model ',model

      if ((model.ne.'CC').and.(model.ne.'TP')) then
        call stopit('Indicate which model: CC or TP')
      endif
     
!     Selected output
      read(funit_run,*) source

      if ((source.ne.'PSE').and.(source.ne.'LSE').and.  &
          (source.ne.'ASE').and.(source.ne.'ALL')) then
        call stopit('Enter output type: PSE, LSE, ASE or ALL')
      endif
      
! IF ANY OF THE EMISSION INPUT IS MISSING STOP THE PROGRAM
      if ((source.eq.'PSE').and.(.not.fe_in_points)) then
        call stopit('Point source file does not exist. Check filename.')
      endif
      if ((source.eq.'LSE').and.(.not.fe_in_lines)) then
        call stopit('Line source file does not exist. Check filename.')
      endif
      if ((source.eq.'ASE').and.(.not.fe_in_area_sector)) then
        call stopit('Area source file does not exist. Check filename.')
      endif
      if ((source.eq.'ALL').and.(.not.fe_in_points)) then
        call stopit('Point source file does not exist. Check filename.')
      endif
      if ((source.eq.'ALL').and.(.not.fe_in_lines)) then
        call stopit('Line source file does not exist. Check filename.')
      endif
      if ((source.eq.'ALL').and.(.not.fe_in_area_sector)) then
        call stopit('Area source file does not exist. Check filename.')
      endif

            print *,source
      
!     ASCII or Binary
      read(funit_run,*) EP_fm

!     The number of hours to compute:
      read(funit_run,*) n_hours

!     Start/End date of input
      read(funit_run,*) startdate
      read(funit_run,*) enddate
!!!==============================================
!!! changed by Kang @ CARES
!!! change the simluation time appeared in point source output file
!!! here, it is hour value.      
! orig:      read(funit_run,*)  (bdat(i),i=1,3)
!      read(funit_run,*)  (edat(i),i=1,3)
      read(funit_run,*)  (bdat(i),i=1,4)
      read(funit_run,*)  (edat(i),i=1,4)
!!!==============================================      
!     TAPM file for temperature is used in SNAP2 emission calculation
!     if it is not present, the normal way to calculate SNAP2 is used
!     !   INQUIRE(DIRECTORY=fname_inpath_tapm,EXIST = fe_in_tapm)
!     Problem: The DIRECTORY statement in INQUIRE() works with ifort 
!     but not with gfortran.
      INQUIRE(FILE=trim(fname_inpath_tapm)//'/T_and_dtdz_'//trim(startdate)//'_'//trim(enddate)//'.nc',EXIST = fe_in_tapm)

      print *,'fe_tapm',fe_in_tapm,'  ',fname_inpath_tapm

!     Model dimensions:
      read(funit_run,*) n_nx
      read(funit_run,*) n_ny
!!!  deleted by Kang @ Apr. 30, 2020.  For no square case. 
!      if (n_nx /= n_ny) then
!        call stopit('UECT can only be used for quadratic domains (nx=ny)')
!      endif
!!   end of change

!     Horizontal grid resolution (in meters):
      read(funit_run,*) dxarea   ! gridded area source
      read(funit_run,*) dxout    ! output grid

      if (dxarea > dxout) then
        call stopit('Area emission resolution has to be <= model resolution')
      endif

!     Grid origo
      read(funit_run,*) sitex0,sitey0

      if (sitex0 > 1.e6) then
        call stopit('Enter truncate UTM x-coordinate (no leading digits for UTM zone)')
      endif

!     Grid UTM zone
      read(funit_run,*) utmzone


!     Number of sources in the input files:
      read(funit_run,*) n_sopp
      read(funit_run,*) n_soll
      read(funit_run,*) n_soaa

      if (n_sopp > 99999) then
        call stopit('UECT allows max. 99999 point sources')
      endif
      if (n_soll > 999999) then
        call stopit('UECT allows max. 999999 line sources')
      endif

!     Optional netCDF output for checking
      read(funit_run,*) NC_out

!!!!==============================================================================
!!! changed by kang @ CARES.  
!!! define a new variable to control if using time factor for emission rate calculation on different date

      read(funit_run,*) ntime_factor
!!!!==============================================================================



!     Construct the begin date
!     Set start time and day of week
!!!==============================================
!!! changed by Kang @ CARES
!!! change the simluation time appeared in point source output file
!!! here, it is hour value.   
!      bdat(4) = 0               ! hour(0-23)
!      bdat(5) = 0
!      bdat(6) = 0
!      edat(4) = 23              ! hour(0-23)
!      edat(5) = 0
!      edat(6) = 0
!      year = bdat(1)
!      mnth = bdat(2)
!      daym = bdat(3)
!      hour = bdat(4)
!      minu = bdat(5)
!      seco = bdat(6)


      bdat(5) = 0
      bdat(6) = 0

      edat(5) = 0
      edat(6) = 0
      year = bdat(1)
      mnth = bdat(2)
      daym = bdat(3)
      hour = bdat(4)
      minu = bdat(5)
      seco = bdat(6)
!!!==============================================

!     UECT2.3: we check if the biogenic VOC pre-emission files are there
      INQUIRE(FILE=trim('./input/'//'BVOC_4_UECT_RheinRuhr.nc'),EXIST = fe_in_bvocrr)
      INQUIRE(FILE=trim('./input/'//'BVOC_4_UECT_HH.nc'),EXIST = fe_in_bvochh)

      if ((fe_in_bvocrr).or.(fe_in_bvochh)) then
         fe_in_bvoc = .true.
      endif

      print *,'fe_in_bvoc RR HH',fe_in_bvocrr,fe_in_bvochh

      if ((fe_in_bvocrr).and.(fe_in_bvochh)) then
        call stopit('There must only be one EF map file in ./testinput')
      endif

      close(funit_run)

! **********************************************************************

!     Write the Log file:

      if (fe_log) then
        write(funit_log,'(1X,A)')         &
    '************************************************************'

        word_len = LEN_TRIM(fname_run)
        write(funit_log,'(1X,3A)') &
    'Input parameters read from the RUN_FILE: "', &
    fname_run(1:word_len),'"'

        write(funit_log,'(1X,A)') &
    '************************************************************'

        write(funit_log,*)
        write(funit_log,'(1X,A)') 'Files containing INPUT-data: '
        write(funit_log,'(1X,A)') '**************************** '
        write(funit_log,*)

        word_len = LEN_TRIM(fname_log)
        write(funit_log,'(1X,A58,2A)')  &
    'The name of the applied LOG_FILE = "', &
    fname_log(1:word_len),'"'


        write(funit_log,*)
        write(funit_log,'(1X,A)') 'User-defined steering parameters: '
        write(funit_log,'(1X,A)') '********************************* '
        write(funit_log,*)

        
        write(funit_log,'(1X,A)') &
    'END of input parameters read from the user-supplied META info:'
        write(funit_log,'(1X,A)') &
    '************************************************************'
      end if



      return
      end subroutine get_user_input
