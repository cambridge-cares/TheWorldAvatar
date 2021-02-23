! <module_uect_exe.for - A component of the City-scale
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

      module module_uect_exe

!***********************************************************************
!***  Module module_uect_exe declares variables and parameters
!***  for controlling the program flow of UECT


!!!!!!!
!!!  modified by Kang @ Jan. 22, 2020 @ Cambridge CARES for moving point source (circular or straight line) and building parameters, and emission rate calculation
!!! a, add 5 more columns for moving points:  vec (m/s), direction (0-360), circ_ang (>0, clockwise, <0, counter-colockwise, =0, striaght line), t_start_moving(s),t_stop_moving(s). 
!!! b, read the building height and width from input excel file (2 more columns), intead of default definition in "emission_point.for"
!!! will read into "in_array_pse(:,7-13)". So for emission indicator, it needs be adding 7. 
!!! other varibles: new_array_pse(:,7-13), out_params_pse() 
!!! c, define a new variable to control if using time factor for emission rate calculation on different date.  
!!!    time_factor=0, dont use time factor; =1, use time factor esimated in CityChem on different date; 
!!!    time_factor will be read from input file.
!***********************************************************************
!***********************************************************************

      implicit none

!***********************************************************************

!     Declarations:

      integer            :: n_hours
      integer            :: n_nx
      integer            :: n_ny
      integer            :: n_sopp
      integer            :: n_sopp2
      integer            :: n_soll
      integer            :: n_soll2
      integer            :: n_soaa
      integer            :: n_soaa2

      real               :: dxarea
      real               :: dxout
      real               :: sitex0
      real               :: sitey0

      integer,parameter  :: n_nz = 1

      character(len=38)  :: simid
      character(len=2 )  :: model 
      character(len=3 )  :: source
      character(len=8)   :: utmzone = ' '

!     Chemical compounds

! Input compounds
      integer,parameter    :: n_inp_compds =  7
! VOC = RSMOG + single VOCs (9 anthropogenic and 3 biogenic [isoprene,apinene,limonene])
      integer,parameter    :: n_voc_compds =  12
! NOX = NO + NO2 + HONO
      integer,parameter    :: n_nox_compds =  3
! Total output compounds (in + nox + voc) 
      integer, parameter   :: ncomp = n_inp_compds + n_nox_compds + n_voc_compds

      integer, parameter   :: ncc    = 22
      integer, parameter   :: nctapm = 4
      
!!!!!===========================================
!!!! for moving point source by Kang @ CARES
!!! orig:       integer, parameter  :: n_pse_params = 10      
      integer,parameter    :: n_pse_params = 15 
      !!! add 4 more columns for moving points:  vec (m/s), direction (0-360), circ_ang (>0, clockwise, <0, counter-colockwise, =0, striaght line)
      !!! , t_start_moving(s),t_stop_moving(s)  will output into out_params_pse(:,11-14).
      !!!  building height and width are already defined as out_params_pse(:,6-7).

!!!  also define a new variable to control if using time factor for emission rate calculation on different date
      integer            :: ntime_factor  !! =0, dont use time factor;  !! =1, use the default time factor calculated in CityChem for different date.
      
!!!!===============================================      
      integer,parameter    :: n_lse_params =  9
      integer,parameter    :: n_ase_params =  8

!     Emission fields

      real, allocatable    :: out_array_pse(:,:,:)
      real, allocatable    :: out_array_lse(:,:,:)
      real, allocatable    :: out_array_ase(:,:,:)

      real, allocatable    :: out_params_pse(:,:)
      real, allocatable    :: out_params_lse(:,:)
      real, allocatable    :: out_params_ase(:,:)

      real, allocatable    :: in_array_pse(:,:)
      real, allocatable    :: in_array_lse(:,:)
      real, allocatable    :: in_array_ase(:,:)

      real, allocatable    :: new_array_pse(:,:)
      real, allocatable    :: new_array_lse(:,:)
      real, allocatable    :: new_array_ase(:,:)

      integer, allocatable :: out_snap_pse(:)
      integer, allocatable :: snap_new_pse(:)
      integer, allocatable :: snap_vec_pse(:)

      integer, allocatable :: out_snap_lse(:)
      integer, allocatable :: snap_new_lse(:)
      integer, allocatable :: snap_vec_lse(:)

      integer, allocatable :: out_snap_ase(:)
      integer, allocatable :: snap_new_ase(:)
      integer, allocatable :: snap_vec_ase(:)

      integer, allocatable :: xi_ase(:)
      integer, allocatable :: yi_ase(:)

! *** CITYCHEM compound names

      character(len=10)    :: cpname( ncc ) 

! *** CITYCHEM model domain borders
      real                 :: borderw    ! W border in UTM m
      real                 :: borders    ! S border in UTM m
      real                 :: bordere    ! E border in UTM m
      real                 :: bordern    ! N border in UTM m

!     Routines and Functions:

! ************
!      contains


! End of module module_uect_exe

      end module module_uect_exe
