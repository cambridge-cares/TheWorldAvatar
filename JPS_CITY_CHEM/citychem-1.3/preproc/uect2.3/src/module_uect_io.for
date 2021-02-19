! <module_uect_io.for - A component of the City-scale
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
! Original source code of the routines nxtdat, write_2dfield, nextun
! are written by Sam-Erik Walker (NILU)
!
! Sam-Erik Walker
! Norwegian Institute for Air Research (NILU)
! Instituttveien 18 P.O. Box 100
! N-2027 Kjeller, NORWAY
! Tel: +47 63898000 Fax: +47 63898050
! E-mail: sam-erik.walker@nilu.no
!
! ----------------------------------------------------------------------------------

      module module_uect_io

!***********************************************************************
!***  Module module_uect_io declares variables and parameters
!***  for input/output interfaces of UECT.
!***  Module contains all routines for reading and writing files

!!!!!!!
!!!  modified by Kang @ Jan. 22, 2020 @ Cambridge CARES for moving point source (circular or straight line) and building parameters, and emission rate calculation
!!! a, add 5 more columns for moving points:  vec (m/s), direction (0-360), circ_ang (>0, clockwise, <0, counter-colockwise, =0, striaght line), t_start_moving(s),t_stop_moving(s). 
!!! b, read the building height and width from input excel file (2 more columns), intead of default definition in "emission_point.for"
!!! will read into "in_array_pse(:,7-13)". So for emission indicator, it needs be adding 7. 
!!! other varibles: new_array_pse(:,7-13), out_params_pse() 
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
      integer             :: funit_out_points_iso
      integer             :: funit_out_lines
      integer             :: funit_out_areas
      integer             :: funit_nc_adome_emis
      integer             :: funit_nc_asolv_emis
      integer             :: funit_nc_aothe_emis

! file names

      character (len=256) :: fname_run
      character (len=256) :: fname_log
      character (len=256) :: fname_in_points
      character (len=256) :: fname_in_lines
      character (len=256) :: fname_in_area_sector

      character (len=256) :: fname_outpath
      character (len=256) :: fname_out_points
      character (len=256) :: fname_out_points_iso
      character (len=256) :: fname_out_lines
      character (len=256) :: fname_out_areas

      character (len=256) :: fname_inpath_tapm
      character (len=256) :: fname_inpath_cmaq

      character (len=256) :: fname_nc_adome_emis
      character (len=256) :: fname_nc_asolv_emis
      character (len=256) :: fname_nc_aothe_emis
      character (len=256) :: fname_nc_aship_emis

      character (len=256) :: fname_nc_airtemp
      character (len=256) :: fname_nc_bvocef

! file exist flags

      logical             :: fe_log
      logical             :: fe_in_points
      logical             :: fe_in_lines
      logical             :: fe_in_area_sector
      logical             :: fe_outpath
      logical             :: fe_in_tapm
      logical             :: fe_in_bvoc
      logical             :: fe_in_bvocrr
      logical             :: fe_in_bvochh

! file formatted read
      character (len=256) :: format_pse
      character (len=256) :: format_lse
      character (len=256) :: format_ase

! i/o control
      integer             :: EP_fm
      integer             :: NC_out
      character(len=60)   :: startdate
      character(len=60)   :: enddate

! columns in input files (excl. SNAP)

!!!!!===========================================
!!!! for moving point source by Kang @ CARES
!!! orig:       integer, parameter  :: colp = 13
!!!
      integer, parameter  :: colp = 20  
 !!! add 4 more columns for moving points:  vec (m/s), direction (0-360),  circ_ang (>0, clockwise, <0, counter-colockwise, =0, striaght line), t_start_moving(s),t_stop_moving(s) 
 !!!and 2 more columns for building height and width. will read into in_array_pse(:,7-12).
!!!!===============================================
      integer, parameter  :: coll = 13
      integer, parameter  :: cola = 13

!   Variables for netCDF output
      integer             :: Nhh_in
      integer, dimension(4,1) :: mdate
      character(len=10)   :: namefield
      character(len=10)   :: unitname
      character(len=23)   :: validity
      logical             :: dopacking  = .false.
      logical             :: dofloat    = .false.
      logical             :: domirror   = .false.

! citychem output
      integer, parameter   :: iqu = 1    ! unit emission is g/s
      !integer, parameter :: iqu = 2    ! unit emission is (unknown)
      !integer, parameter :: iqu = 3    ! unit emission is (unknown)
      !integer, parameter :: iqu = 4    ! unit emission is num/s (pnc)
      integer, parameter   :: iqt = 1    ! unit temperature is degC

! citychem emission compounds
      integer,parameter         :: nccout = 17
      character(len=10) ,dimension(nccout) :: cpnameo

      integer,dimension(nccout) :: funit_out_lines_emis
      integer,dimension(nccout) :: funit_out_adome_emis
      integer,dimension(nccout) :: funit_out_asolv_emis
      integer,dimension(nccout) :: funit_out_aothe_emis
      integer,dimension(nccout) :: funit_out_aship_emis

      character(len=256),dimension(nccout) :: fname_out_lines_emis
      character(len=256),dimension(nccout) :: fname_out_adome_emis
      character(len=256),dimension(nccout) :: fname_out_asolv_emis
      character(len=256),dimension(nccout) :: fname_out_aothe_emis
      character(len=256),dimension(nccout) :: fname_out_aship_emis


!     Routines and Functions:

! ************
      contains

!**********************************************************************

      subroutine nxtdat(un,leof)

! The subroutine prepares for reading the next uncommented 
! line of data from file.

!***********************************************************************

      implicit none

! Scalar arguments

      integer             :: un
      character(len=256)  :: TXTSTR
      logical             :: leof

! UN     - Fileunit
! TXTSTR - Textstring
! LEOF   - If end of file then true else false 

! If fileunit is nonpositive then just return

      IF (un .LE. 0) RETURN

! Fileunit is positive

      leof = .FALSE.

! Read lines from file

  100 CONTINUE
      READ (un,1000,END=999) TXTSTR
      IF (TXTSTR(1:1) .EQ. '*') GOTO 100
      BACKSPACE(un)

      RETURN

  999 CONTINUE

      leof = .TRUE.
      return

 1000 FORMAT (A256)

! End of subroutine nxtdat

      end subroutine nxtdat

!***********************************************************************

      subroutine write_2dfield(nx,ny,txt1,txt2,unx,fm,f2d)

      IMPLICIT NONE
      
! *** Global variables:
     
      integer           :: nx,ny
      integer           :: unx
      integer           :: fm
      real              :: f2d(nx,ny)
      character(len=10) :: txt1,txt2

! *** Local variables:          
      integer           :: ix,iy

! *** Start of routine:

! *** Gridded fields are written either binary (fm = 2) or as (default) ASCII:

      if (fm == 2) then
! ***   Write data using simple binary format:
        !write (un) txt1,txt2,nx,ny,((f2D(ix,iy),ix=1,nx),iy=1,ny)
        write (unx  ) txt1,txt2,((f2D(ix,iy),ix=1,nx),iy=1,ny)
      else
! ***   Write data using simple ASCII  format (The use of 32767 below means
! ***   that the field must have dimensions not larger than: 181 x 181.
! ***   The last slash at the end of the inner parenthesis produce one empty
! ***   line after the final data line.:
!        write (un,'(2A10,2I3,/,32767(2I3,1P,E16.8,/))') txt1,txt2,nx,ny, &
!       &                             ((ix,iy,f2D(ix,iy),ix=1,nx),iy=1,ny)
        write (unx ,'(A10,A10,2I3,/,32767(1P,E14.4,/))') txt1,txt2,nx,ny, &
       &                             ((f2D(ix,iy),ix=1,nx),iy=1,ny)
      endif

      return
      end subroutine write_2dfield


!***********************************************************************
!***********************************************************************
! Functions

      integer function nextun()

! The function returns the next free filunit.

!***********************************************************************

! Local variables

      INTEGER IU
      LOGICAL ISOPEN
      character(len=256) :: FN

! IU     - Fileunit
! ISOPEN - If fileunit is open then true else false

! Start with unit 12 (10 is metadata, 11 is logfile)
      IU     = 12
      ISOPEN = .FALSE.

  100 CONTINUE

! Check if current unit is opened

      INQUIRE(IU,OPENED=ISOPEN,NAME=FN)
      IF (ISOPEN) THEN
          print *,'nextun',IU,FN
          IU = IU + 1
          GOTO 100
      ENDIF

! Found free fileunit

      NEXTUN = IU

      RETURN

! End of integer function NEXTUN

      end function nextun

!***********************************************************************

! End of module module_uect_io

      end module module_uect_io
