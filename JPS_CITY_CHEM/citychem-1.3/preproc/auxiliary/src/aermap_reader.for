! <static4cchem.f90 - A component of the City-scale
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
!*****************************************************************************! 
!  PROGRAM: STATIC4CC
!
!  VERSION: 1.0
!
!  PURPOSE:    This program creates static files for MCWIND and CityChem
!
! Based on:
!              Subroutine write_2dfield is written by Leif Harvard Slørdal (NILU)
!
! Contact:
!              See License header
!
!*****************************************************************************!

      program static4cchem

! **** Creates static files for MCWIND and CityChem
! ****
! ****      reads topography from AERMAP output
! ****      creates:
! ****      surfrough_episode.asc
! ****      topo.asc
! ****      landuse_episode.asc
! ****
! ******************************************

      use module_cc_input

        implicit none

! User edit section
      integer, parameter :: nrec = 500          ! 25 values in one line of AERMAP.REC file
      character(len=256) :: aermap_name = 'AERMAP_NAD83_NED.REC'       ! name of AERMAP.REC file
! End User edit section

! *** Filename of AERMAP elevation output
      character(len=256) :: fname_aermap
! *** Filename of the 2D time-independent surface roughness z0 (m):
      character(len=256) :: EP_2D_surfz0
! *** Filename of the 2D time-independent land use:
      character(len=256) :: EP_2D_landuse
! *** Filename of the 2D time-independent terrain elevation in m
      character(len=256) :: EP_2D_topo
! *** Filename of the 2D time-dependent cloud cover (fraction 0-1):
      character(len=256) :: EP_2D_cloud


! *** Variables
      real, allocatable     ::    data_array(:,:)

      real, allocatable     ::    z0(:,:)                 !! z0:       Surface roughness length (m)
      real, allocatable     ::    lu(:,:)                 !! lu:       Land Use class corresponding to z0
      real, allocatable     ::    topo(:,:)               !! topo:     Terrain elevation (m)


! *** Additional declarations:
      integer :: i, j, n, a, b
      integer :: inlines

! *** File formatting
      integer             :: un
      integer             :: res1
      integer             :: resin
      integer             :: ind
      character(len=10)   :: txt1, txt2
      character (len=256) :: aerheader1
      character (len=256) :: aerheader2
      character (len=256) :: aerheader3
      character (len=256) :: aerheader4
      character (len=20)  :: comm1
      character (len=20)  :: comm2
      character (len=20)  :: comm3
      logical             :: leof
      character(len=18)   :: filedate


! *******************************************************************************************************

!     Get user-supplied meta information
      call get_user_input  

      fname_aermap = trim(fname_outpath)//'/'//trim(aermap_name)


! ***   ALLOCATE
      if (.not. allocated(z0))      allocate( z0(nx, ny) )
      if (.not. allocated(lu))      allocate( lu(nx, ny) )
      if (.not. allocated(topo))    allocate( topo(nx, ny) )

      inlines = ny*nrec
!MSK      if (.not. allocated(data_array)) allocate( data_array(inlines,6) )
!MSK: now aermap.f changed to write 5 values per line into the recorder file
      if (.not. allocated(data_array)) allocate( data_array(inlines,5) )

! ***   Open AERMAP output file (AERMAP_NAD83_NED.REC)

      print *,'AERMAP.REC file: ',fname_aermap
      open(11, file=fname_aermap,  access='sequential',form="formatted",iostat=res1)

      filedate = '_'//trim(startdate)//'_'//trim(enddate)
      print *,'filedate: ',filedate

      if (EP_fm /= 2) then
! ***   Giving filename extension indicating ASCII-files (".txt" or ".asc"):

        EP_2D_surfz0          = trim(fname_outpath)//'/surfrough_episode'//trim(filedate)//'.asc'
        EP_2D_landuse         = trim(fname_outpath)//'/landuse_episode'//trim(filedate)//'.asc'
        EP_2D_topo            = trim(fname_outpath)//'/topo'//trim(filedate)//'.asc'

      else

! ***   Giving filename extension indicating BINARY output files (".fld"):

        EP_2D_surfz0          = trim(fname_outpath)//'/surfrough_episode'//trim(filedate)//'.fld'
        EP_2D_landuse         = trim(fname_outpath)//'/landuse_episode'//trim(filedate)//'.fld'
        EP_2D_topo            = trim(fname_outpath)//'/topo'//trim(filedate)//'.fld'
       
      endif

      if (EP_fm /= 2) then
! ***   OPEN the OUTPUT FILES as ASCII-Files:
      
! ***   Open the 2D time-independent output files:
        open (21, file = EP_2D_surfz0,             status = 'unknown', form  = 'formatted', action = 'write')
        open (22, file = EP_2D_landuse,            status = 'unknown', form  = 'formatted', action = 'write')
        open (23, file = EP_2D_topo,               status = 'unknown', form  = 'formatted', action = 'write')

      else
      
! ***   OPEN the OUTPUT FILES as BINARY-Files:
      
! ***   Open the 2D time-independent output files:
        open (21, file = EP_2D_surfz0,             status = 'unknown', form  = 'unformatted', action = 'write')
        open (22, file = EP_2D_landuse,            status = 'unknown', form  = 'unformatted', action = 'write')
        open (22, file = EP_2D_topo,               status = 'unknown', form  = 'unformatted', action = 'write')

      endif

! *** Read AERMAP output file header ******************
! *** Lines 10-13
      call nxtdat(11,leof)
      read(11,fmt='(A)', iostat=resin) aerheader1
      call nxtdat(11,leof)
      read(11,fmt='(A)', iostat=resin) aerheader2
      call nxtdat(11,leof)
      read(11,fmt='(A)', iostat=resin) aerheader3
      call nxtdat(11,leof)
      read(11,fmt='(A)', iostat=resin) aerheader4

! *** Read AERMAP output file data  *******************

!MSK: how to handle lines that are not completely filled with values?
!MSK: AERMAP should write 5 values per line!!!

      do a = 1, inlines

        call nxtdat(11,leof)
        read(11,*,end=999) comm1,comm2,comm3,ind,data_array(a,:)

      end do

  999    continue

! *** Write data_array to topo(i,j)  ******************

      a = 1
      b = 1
      do j = 1, ny
        do i = 1, nx

             topo(i,j) = data_array(a,b)
             b = b + 1
!MSK             if (b == 7) then
!MSK now 5 data values per line
             if (b == 6) then
               a = a + 1
               b = 1
             endif

        enddo
      enddo


! *** Static 2d Fields ********************************

! *** Generate 2d field for surface roughness, land use, topography

      do i = 1,nx
        do j = 1,ny

     ! MSK set height asl <0 to zero
           if (topo(i,j) .lt. 0.0)  topo(i,j) = 0.0

           if (topo(i,j) == 0.0) then
     ! 0 water
             z0(i,j)   = 0.001
             lu(i,j)   = 0.0
           else
     ! 32 Urban Low; TAPM V4 p.1; Table 2
             z0(i,j)   = 0.4
             lu(i,j)   = 32.0
           endif

        enddo
      enddo

! *** Write out the 2D-surface roughness; z0(nx,ny)
      un    = 21
      txt1  = 'z0.in.m'
      txt2  = '_from_TAPM'
      call write_2dfield(nx,ny,txt1,txt2,un,EP_fm,z0)
      write(6,      '(A18,2A10,2I3)')  'Finished writing: ',txt1,txt2,nx,ny

! *** Write out the 2D-land use; lu(nx,ny)
      un    = 22
      txt1  = 'landuse.in.m'
      txt2  = '_from_TAPM'
      call write_2dfield(nx,ny,txt1,txt2,un,EP_fm,lu)
      write(6,      '(A18,2A10,2I3)')  'Finished writing: ',txt1,txt2,nx,ny

! *** Write out the 2D-topography; topo(nx,ny)
      un    = 23
      txt1  = 'topo.in.m'
      txt2  = '_from_TAPM'
      call write_2dfield(nx,ny,txt1,txt2,un,EP_fm,topo)
      write(6,      '(A18,2A10,2I3)')  'Finished writing: ',txt1,txt2,nx,ny      


! *** Deallocation of variables

      if (allocated(z0) )      deallocate(z0 )
      if (allocated(lu) )      deallocate(lu )
      if (allocated(topo) )    deallocate(topo )
      if (allocated(data_array) )    deallocate(data_array )

      contains
      
! ******************************************************************************************************

      subroutine write_2dfield(nx,ny,txt1,txt2,un,fm,f2d)

      IMPLICIT NONE
      
! *** Global variables:
     
      integer           :: nx,ny
      integer           :: un,fm
      real              :: f2d(nx,ny)
      character(len=10) :: txt1,txt2

! *** Local variables:          
      integer           :: ix,iy

! *** Start of routine:

! *** Gridded fields are written either binary (fm = 2) or as (default) ASCII:

      if (fm == 2) then
! ***   Write data using simple binary format:
        write (un) txt1,txt2,nx,ny,((f2D(ix,iy),ix=1,nx),iy=1,ny)
      else
! ***   Write data using simple ASCII  format (The use of 32767 below means
! ***   that the field must have dimensions not larger than: 181 x 181.
! ***   The last slash at the end of the inner parenthesis produce one empty
! ***   line after the final data line.:
        write (un,'(2A10,2I3,/,32767(2I4,1P,E16.8,/))') txt1,txt2,nx,ny, &
       &                             ((ix,iy,f2D(ix,iy),ix=1,nx),iy=1,ny)
      endif

      return
      end subroutine write_2dfield

!**********************************************************************

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


      end program static4cchem
