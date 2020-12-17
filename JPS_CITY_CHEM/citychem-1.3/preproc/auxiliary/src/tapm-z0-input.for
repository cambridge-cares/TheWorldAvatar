! <tapm-z0-input.for  - A component of the City-scale
!                 Chemistry Transport Model EPISODE-CityChem>
!*****************************************************************************!
!* 
!* EPISODE - An urban-scale air quality model
!* ========================================== 
!* Copyright (C) 2018  NILU - Norwegian Institute for Air Research
!*                     Instituttveien 18
!*                     PO Box 100
!*                     NO-2027 Kjeller
!*                     Norway
!*
!*                     Contact persons: Gabriela Sousa Santos - gss@nilu.no
!*                                      Paul Hamer - pdh@nilu.no
!*
!* Unless explicitly acquired and licensed from Licensor under another license,
!* the contents of this file are subject to the Reciprocal Public License ("RPL")
!* Version 1.5, https://opensource.org/licenses/RPL-1.5 or subsequent versions as
!* allowed by the RPL, and You may not copy or use this file in either source code
!* or executable form, except in compliance with the terms and conditions of the RPL. 
!*
!* All software distributed under the RPL is provided strictly on an "AS IS" basis, 
!* WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESS OR IMPLIED, AND LICENSOR HEREBY 
!* DISCLAIMS ALL SUCH WARRANTIES, INCLUDING WITHOUT LIMITATION, ANY WARRANTIES OF
!* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, QUIET ENJOYMENT, OR NON-INFRINGEMENT.
!* See the RPL for specific language governing rights and limitations under the RPL.
!*
!* ========================================== 
!* The dispersion model EPISODE (Grønskei et. al., 1993; Larssen et al., 1994;
!* Walker et al., 1992, 1999; Slørdal et al., 2003, 2008) is an Eulerian grid model 
!* with embedded subgrid models for calculations of pollutant concentrations resulting 
!* from different types of sources (area-, line- and point sources). EPISODE solves the 
!* time dependent advection/-diffusion equation on a 3 dimensional grid. 
!* Finite difference numerical methods are applied to integrate the solution forward in time. 
!* It also includes extensions as the implementation of a simplified EMEP photochemistry 
!* scheme for urban areas (Walker et al. 2004) and a scheme for Secondary Organic Aerosol 
!* implemented by Håvard Slørdal
!*
!* Grønskei, K.E., Walker, S.E., Gram, F. (1993) Evaluation of a model for hourly spatial
!*    concentrations distributions. Atmos. Environ., 27B, 105-120.
!* Larssen, S., Grønskei, K.E., Gram, F., Hagen, L.O., Walker, S.E. (1994) Verification of 
!*    urban scale time-dependent dispersion model with sub-grid elements in Oslo, Norway. 
!*    In: Air poll. modelling and its appl. X. New York, Plenum Press.
!* Slørdal, L.H., McInnes, H., Krognes, T. (2008): The Air Quality Information System AirQUIS. 
!*    Info. Techn. Environ. Eng., 1, 40-47, 2008.
!* Slørdal, L.H., Walker, S.-E., Solberg, S. (2003) The Urban Air Dispersion Model EPISODE 
!*    applied in AirQUIS. Technical Description. NILU TR 12/2003. ISBN 82-425-1522-0.
!* Walker, S.E., Grønskei, K.E. (1992) Spredningsberegninger for on-line overvåking i Grenland. 
!*    Programbeskrivelse og brukerveiledning. Lillestrøm, 
!*    Norwegian Institute for Air Research (NILU OR 55/92).
!* Walker, S.E., Slørdal, L.H., Guerreiro, C., Gram, F., Grønskei, K.E. (1999) Air pollution 
!*    exposure monitoring and estimation. Part II. Model evaluation and population exposure. 
!*    J. Environ. Monit, 1, 321-326.
!* Walker, S.-E., Solberg, S., Denby, B. (2003) Development and implementation of a simplified 
!*    EMEP photochemistry scheme for urban areas in EPISODE. NILU TR 13/2013. 
!*    ISBN 82-425-1524-7
!*
!* ========================================== 
!*
!*****************************************************************************! 
!  PROGRAM: Z0TOP4CC
!
!  VERSION: 1.0
!
!  PURPOSE:    Creates spatial heterogenous z0 file for CITYCHEM
!              based on the landuse.top file from TAPM
!
! Based on:
!              Original source code of TAPM4CC by Leif Harvard Slørdal (NILU)
!              and Bruce Denby (NILU, MET.NO)
!
! Contact:
!              See License header
!
!*****************************************************************************!

      program z0top4cchem

! ******************************************
! **** Creates spatial heterogenous z0 file for CITYCHEM
! **** based on the landuse.top file from TAPM
! ****
! ******************************************

      use module_cc_input

        implicit none

      character(len=256) :: TAPM_landuse_name
! *** TAPM Filename of land use information file (m)
      character(len=256) :: TAPM_landuse_file

! *** Filename of the 2D time-independent surface roughness z0 (m):
      character(len=256) :: EP_2D_surfz0
! *** Filename of the 2D time-independent land use:
      character(len=256) :: EP_2D_landuse
! *** Filename of the 2D time-dependent cloud cover (fraction 0-1):
      character(len=256) :: EP_2D_cloud

! *** Variables
      integer,allocatable   :: data_array(:,:)
      real, allocatable     ::    z0(:,:)                 !! z0:       Surface roughness length (m)
      real, allocatable     ::    lu(:,:)                 !! lu:       Land Use class corresponding to z0
      real, allocatable     ::    clou(:,:,:)             !! clou:     Cloud coverage (fraction 0-1)
      integer, allocatable  ::    luvar(:,:)              !! tapm land use from *.top file

      integer :: i, j, n, hr
      integer :: inline,a,b
      integer :: ceilxy

      real, dimension(38), parameter :: z0table = (/ 4.200,3.650,2.500,1.700,1.200,1.000,0.900,0.700,0.550,  &  ! 1- 9
                                                     0.300,0.250,0.200,0.100,0.060,0.050,0.050,0.045,0.075,  &  !10-18
                                                     0.060,0.045,0.040,0.060,0.060,0.045,0.045,0.035,0.030,  &  !19-27
                                                     0.250,0.000,0.000,1.000,0.400,0.600,0.800,2.000,0.500,  &  !28-36
                                                     1.000,1.500 /)                                             !37-38

!     daily cycle of cloud fraction (0 ... 1) 

      real, dimension(24), parameter :: cloutab = (/ 1.0, 1.0, 1.0, 1.0, 0.9, 0.8, 0.8, 0.7, 0.6, 0.5, 0.4, 0.4,  &
                                                     0.5, 0.6, 0.7, 0.8, 0.8, 0.9, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 /)

! *** File formatting
      integer            :: un
      integer            :: unbc
      character(len=10)  :: txt1, txt2
      character(len=18)  :: filedate
      logical            :: leof

! *******************************************************************************************************

! USER EDIT FILENAME
!      TAPM_landuse_name = 'SHEBA_RO_landuseCC.csv'
!      TAPM_landuse_name = 'SHEBA_GD_TAPM_Landuse.csv'
!      TAPM_landuse_name = 'SHEBA_RI_TAPM_Landuse.csv'
!      TAPM_landuse_name = 'SHEBA_RO_landuseCC_400m.csv'
!      TAPM_landuse_name = "SHEBA_TAPM_1000m_HH.csv"
      TAPM_landuse_name = "landuse.top"

!     Get user-supplied meta information
      call get_user_input   

! MSK 26.08.2019  Fix for domain size that can't be divided by 10
      ceilxy = ceiling((nx*ny)*0.1)


! ***   ALLOCATE
!** TAPM *.top files have 10 columns
      if (.not. allocated(data_array)) allocate( data_array( ceilxy, 10) )
      if (.not. allocated(z0))         allocate( z0(nx, ny) )
      if (.not. allocated(lu))         allocate( lu(nx, ny) )
      if (.not. allocated(luvar))      allocate( luvar(nx, ny) )
      if (.not. allocated(clou))       allocate( clou(nx, ny,hh) )

! ***   OPEN the INPUT FILE as ASCII-File:

      TAPM_landuse_file = trim(fname_outpath)//'/'//trim(TAPM_landuse_name)

      print *, TAPM_landuse_file

      unbc = 19

      open (unbc, file = TAPM_landuse_file,  status = 'unknown', form  = 'formatted', action = 'read')

! ***   OPEN the OUTPUT FILES as ASCII-Files:

      filedate = '_'//trim(startdate)//'_'//trim(enddate)
      print *,'filedate: ',filedate

      if (EP_fm /= 2) then
! ***   Giving filename extension indicating ASCII-files (".txt" or ".asc"):

        EP_2D_surfz0          = trim(fname_outpath)//'/surfrough_episode'//trim(filedate)//'.asc'
        EP_2D_landuse         = trim(fname_outpath)//'/landuse_episode'//trim(filedate)//'.asc'
        EP_2D_cloud           = trim(fname_outpath)//'/clou_episode'//trim(filedate)//'.asc'

      else

! ***   Giving filename extension indicating BINARY output files (".fld"):

        EP_2D_surfz0          = trim(fname_outpath)//'/surfrough_episode'//trim(filedate)//'.fld'
        EP_2D_landuse         = trim(fname_outpath)//'/landuse_episode'//trim(filedate)//'.fld'
        EP_2D_cloud           = trim(fname_outpath)//'/clou_episode'//trim(filedate)//'.fld'
       
      endif

      if (EP_fm /= 2) then
      
! ***   Open the 2D time-independent output files:
        open (21, file = EP_2D_surfz0,             status = 'unknown', form  = 'formatted', action = 'write')

        open (22, file = EP_2D_landuse,            status = 'unknown', form  = 'formatted', action = 'write')

! ***   Open the 2D time-dependent output files:
        open (31, file = EP_2D_cloud,              status = 'unknown', form  = 'formatted', action = 'write')

      else
      
! ***   OPEN the OUTPUT FILES as BINARY-Files:
      
! ***   Open the 2D time-independent output files:
        open (21, file = EP_2D_surfz0,             status = 'unknown', form  = 'unformatted', action = 'write')

        open (22, file = EP_2D_landuse,            status = 'unknown', form  = 'unformatted', action = 'write')

! ***   Open the 2D time-dependent output files:
        open (31, file = EP_2D_cloud,              status = 'unknown', form  = 'unformatted', action = 'write')

      endif


! *** Static 2d Fields ********************************

! *** Read land use class from TAPM *.top file

      !!!read (unbc,fmt='(10i2)') ((luvar(i,j), i=1, nx), j=1,ny)

      ! read data table
      do inline = 1, ceilxy

        print *,'in',inline
        call nxtdat(unbc,leof)

        read(unbc,*,end=999) data_array(inline,:)
        !print *,'lu data ', data_array(inline,:)

      end do

! *** Generate 2d field for land use
      a = 1
      b = 1
      do j = 1,ny
        do i = 1,nx

             lu(i,j)    = real( data_array(a,b) )
             luvar(i,j) = data_array(a,b)
             b = b + 1
             if (b == 11) then
               a = a + 1
               b = 1
             endif

        enddo
      enddo
      !   print *,lu(1:nx,1)

! *** Write out the 2D-land use; lu(nx,ny)
      un    = 22
      txt1  = 'landuse.in.m'
      txt2  = '_from_TAPM'
      call write_2dfield(nx,ny,txt1,txt2,un,EP_fm,lu)
      write(6,      '(A18,2A10,2I3)')  'Finished writing: ',txt1,txt2,nx,ny


! *** Generate 2d field for surface roughness
! *** Look-up table (1 to 38) for surface roughness length (m)

      do i = 1,nx
        do j = 1,ny

          !print *,i,j,luvar(i,j)
          if (luvar(i,j).eq.-1) then
            z0(i,j) = 0.0001
          elseif (luvar(i,j).eq.0) then
            z0(i,j) = 0.001
          else
            z0(i,j) = z0table(luvar(i,j))
          endif
          !print *,i,j,z0(i,j)

        enddo
      enddo


! *** Write out the 2D-surface roughness; z0(nx,ny)
      un    = 21
      txt1  = 'z0.in.m'
      txt2  = '_from_TAPM'
      call write_2dfield(nx,ny,txt1,txt2,un,EP_fm,z0)
      write(6,      '(A18,2A10,2I3)')  'Finished writing: ',txt1,txt2,nx,ny


! *** Dynamic 2d Fields ********************************

! *** Going through the time-loop:
      hr = 1
      do n = 1,hh

! *** Generate 2d field for cloud cover

         do i = 1,nx
           do j = 1,ny
             clou(i,j,n) = cloutab(hr)      
           enddo
         enddo

! *** After 24 hours start again from hour 0
         hr = hr + 1
         if (hr==25) then
           hr = 1
         endif

      enddo  ! do n = 1,hh


! *** Write 2D net radiation; net(nx, ny, hh); (W/m2)
      un    = 31
!      EP_fm =  1
      write(txt1,'(A10)') '.Cloudcov.'
      do n = 1,hh
        write(txt2,'(A7,I3.3)') '_hour=_',n
        call write_2dfield(nx,ny,txt1,txt2,un,EP_fm,clou(:,:,n))
      enddo
      write(6,      '(A18,2A10,2I3)')  'Finished writing: ',txt1,txt2,nx,ny

! *** End write Dynamic 2d Fields ********************************


! *** Deallocation of variables

      if (allocated(data_array)) deallocate(data_array)
      if (allocated(z0) )        deallocate(z0 )
      if (allocated(lu) )        deallocate(lu )
      if (allocated(luvar) )     deallocate(luvar )
      if (allocated(clou) )      deallocate(clou )

      stop
  999    continue
      if (fe_log) then
          WRITE (funit_log,2010)
      endif
      call stopit('read end of landuse input source file!')
 2010 format('read end of landuse input source file!')


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
        write (un,'(2A10,2I3,/,32767(2I3,1P,E16.8,/))') txt1,txt2,nx,ny, &
       &                             ((ix,iy,f2D(ix,iy),ix=1,nx),iy=1,ny)
      endif

      return
      end subroutine write_2dfield

!*******************************************************************************

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

      end program z0top4cchem
