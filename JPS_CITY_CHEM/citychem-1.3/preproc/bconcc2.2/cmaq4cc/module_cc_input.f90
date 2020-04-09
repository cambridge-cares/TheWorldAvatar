! <module_cc_input.f90  - A component of the City-scale
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
!***********************************************************************
!***
!***      User meta info input file for EPISODE-CityChem
!***
!***********************************************************************

      module module_cc_input

!***********************************************************************
!***  Module module_cc_input declares variables and parameters
!***  for input/output interfaces of CityChem.
!***********************************************************************

      implicit none

!***********************************************************************

!     Declarations:

! file units input
      integer             :: funit_run
      integer             :: funit_log
      integer             :: funit_in_points
      integer             :: funit_in_lines
      integer             :: funit_in_area_sector

! file units output
      integer             :: funit_out_points
      integer             :: funit_out_lines
      integer             :: funit_out_area_sector
      integer             :: funit_inpath_tapm
      integer             :: funit_inpath_cmaq
! file names

      character (len=256) :: fname_run
      character (len=256) :: fname_log
      character (len=256) :: fname_in_points
      character (len=256) :: fname_in_lines
      character (len=256) :: fname_in_area_sector

      character (len=256) :: fname_outpath
      character (len=256) :: fname_out_points
      character (len=256) :: fname_out_lines
      character (len=256) :: fname_out_area_sector

      character (len=256) :: fname_inpath_tapm
      character (len=256) :: fname_inpath_cmaq

! file exist flags

      logical             :: fe_log
      logical             :: fe_in_points
      logical             :: fe_in_lines
      logical             :: fe_in_area_sector
      logical             :: fe_outpath
      logical             :: fe_inpath_tapm
      logical             :: fe_inpath_cmaq
      integer             :: NC_out

! i/o control
      integer             :: EP_fm
      character(len=60)   :: startdate
      character(len=60)   :: enddate
      character(len=8)    :: utmzone = ' '

!     Declarations:

      integer             :: hh
      integer             :: nx
      integer             :: ny
      integer             :: n_sopp
      integer             :: n_soll
      integer             :: n_soaa

      real                :: dxarea
      real                :: dxout
      real                :: sitex0
      real                :: sitey0


      character(len=38)   :: simid
      character(len=2 )   :: model 
      character(len=3 )   :: source

!  Date and time declarations:

      integer             :: bdat(6)
      integer             :: edat(6)

      integer             :: year
      integer             :: mnth
      integer             :: daym
      integer             :: hour
      integer             :: minu
      integer             :: seco
      integer             :: dayweek
      integer             :: dayyear

!     Routines and Functions:

! ************
!      contains


! End of module module_cc_input

      end module module_cc_input
