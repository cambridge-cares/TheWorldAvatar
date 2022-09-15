! <module_mc_wind_files.for - A component of the City-scale
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
!DEC$ FIXEDFORMLINESIZE: 132
!***********************************************************************

      module module_mc_wind_files

!     This module contains the declarations of filenumbers, filenames,
!     etc. and contains routines for opening, reading, writing and
!     closing of
!     these files.

! Modifications:
!    21 Jan 2019  M. Karl: Commented pre-processor directive 'main_program'
!    29 Jan 2019  M. Karl: Replaced TAB by spaces
!
!***********************************************************************
      real, parameter    :: UNDEF = -9900.

!!DEC$ IF DEFINED (main_program)

      integer :: funit_log
      integer :: funit_run
      integer :: funit_in_top
      integer :: funit_in_surfrough
      integer :: funit_in_landuse
      integer :: funit_in_surface_obs
      integer :: funit_in_profile_obs
      integer :: funit_in_geostrophic_obs
      integer :: funit_top
      integer :: funit_wind_res
      integer :: funit_temp_res
      integer :: funit_rhum_res
      integer :: funit_prec_res
      integer :: funit_clou_res
      integer :: funit_ts
      integer :: funit_ts_alfa
      integer :: funit_ts_preproc
      integer :: funit_z0
      integer :: funit_lu
! MSK new output files for Transphorm run
      integer :: funit_tsrad_res
      integer :: funit_albedo_res
      integer :: funit_nrad_res
      integer :: funit_pres_res
      integer :: funit_tsmet

      character (len=256) :: fname_log
      character (len=256) :: fname_run
      character (len=256) :: fname_in_top
      character (len=256) :: fname_in_surfrough
      character (len=256) :: fname_in_landuse
      character (len=256) :: fname_in_surface_obs
      character (len=256) :: fname_in_profile_obs
      character (len=256) :: fname_in_geostrophic_obs
      character (len=256) :: fname_top
      character (len=256) :: fname_wind_res
      character (len=256) :: fname_temp_res
      character (len=256) :: fname_rhum_res
      character (len=256) :: fname_prec_res
      character (len=256) :: fname_clou_res
      character (len=256) :: fname_ts
      character (len=266) :: fname_ts_alfa
      character (LEN=256) :: fname_ts_preproc
      character (len=256) :: fname_z0
      character (len=256) :: fname_lu
! MSK new output files for Transphorm run
      character (len=256) :: fname_tsrad_res
      character (len=256) :: fname_albedo_res
      character (len=256) :: fname_nrad_res
      character (len=256) :: fname_pres_res
      character (len=256) :: fname_tsmet

      logical :: fe_log
      logical :: fe_in_top
      logical :: fe_in_surfrough
      logical :: fe_in_landuse
      logical :: fe_in_surface_obs
      logical :: fe_in_profile_obs
      logical :: fe_in_geostrophic_obs
      logical :: fe_top
      logical :: fe_wind_res
      logical :: fe_temp_res
      logical :: fe_rhum_res
      logical :: fe_prec_res
      logical :: fe_clou_res
      logical :: fe_ts
      logical :: fe_ts_alfa
      logical :: fe_ts_preproc
      logical :: fe_z0
      logical :: fe_lu
! MSK new output files for Transphorm run
      logical :: fe_tsrad_res
      logical :: fe_albedo_res
      logical :: fe_nrad_res
      logical :: fe_pres_res
      logical :: fe_tsmet

      integer :: fm_in_top
      integer :: fm_in_surfrough
      integer :: fm_in_landuse
      integer :: fm_in_surface_obs
      integer :: fm_in_profile_obs
      integer :: fm_in_geostrophic_obs
      integer :: fm_top
      integer :: fm_wind_res
      integer :: fm_temp_res
      integer :: fm_rhum_res
      integer :: fm_prec_res
      integer :: fm_clou_res
      integer :: fm_ts
      integer :: fm_ts_alfa
      integer :: fm_ts_preproc
      integer :: fm_z0
      integer :: fm_lu
! MSK new output files for Transphorm run
      integer :: fm_tsrad_res
      integer :: fm_albedo_res
      integer :: fm_nrad_res
      integer :: fm_pres_res
      integer :: fm_tsmet

!!DEC$ ELSE

!      integer :: funit_log
!      character (len=256) :: fname_log
!      logical :: fe_log

!!DEC$ ENDIF

! ************
      contains

!**********************************************************************

      subroutine opofil(fn,un,fe,fm)

! This subroutine opens an output file in either binary or ASCII
! mode depending on the filename extension. It also defines a file
! format index associated with the file.

!***********************************************************************

      implicit none

! Global declarations:

      character (len=256), intent(in) :: fn
      integer, intent(out) :: un
      integer, intent(out) :: fm
      logical, intent(out) :: fe

! fn - File name
! un - File unit
! fm - File format index
! fe - If the file exists then true else false

! External functions

!      integer  :: NEXTUN

! Local variables

      integer :: i
      integer :: irwf
      logical :: isopen

! I      - Index
! irwf   - File read/write format
! isopen - If file is open then true else false

! Filename exists?

      if (fn(1:1) /= ' ') then

! Yes!

! Get file unit

          isopen = .false.
          INQUIRE (FILE=fn,NUMBER=un,OPENED=isopen)
          if (.not. isopen) un = NEXTUN()

! Get file format depending on filename extension:

          call getrwf(fn,irwf)
          fm = irwf

! Open file in either binary or ASCII mode

          if (fm == 2) then

! Binary mode:

              if (.not. isopen)  &
             OPEN (UNIT=un,FILE=fn,FORM='UNFORMATTED',ERR=9000)

          else

! ASCII mode

              if (.not. isopen)  &
             open (UNIT=un,FILE=fn,ERR=9000)

          end if

! File existent

          fe = .true.

      else

! No!

! No filename implies missing unit and format

          un = INT(UNDEF)
          fm = INT(UNDEF)

! File nonexistent

          fe = .false.

      end if

      return

 9000 continue

! If error in open of file then stop

      if (funit_log.gt.0) then

        write(funit_log,*) 'OPOFIL: Error in open of output file: ', &
                        (fn(i:i),i=1,INDEX(fn,' ') - 1)
        write(funit_log,*) 'OPOFIL: Error in open of output file!'

      end if

      return
! End of subroutine opofil

      end subroutine opofil

! **********************************************************************

      subroutine clofil(un)

! The subroutine closes an output file.

      implicit none

      integer :: un

! un - File unit

! Local variables

      logical isopen

! isopen - If file is open then true else false

      if (un >= 10) then

! Check if file is open

        INQUIRE(un,OPENED=isopen)

! Close the file

        if (isopen) CLOSE(un)

      end if

      return

! End of subroutine clofil

      end subroutine clofil

! **********************************************************************

      subroutine w2dfld(un,irwf,text1,text2,nx,ny,fld)

! The subroutine writes to fileunit 'un' a 2D-field 'fld' using
! read/write format (index) 'irwf'.

      implicit none

! Global variables:

! Scalar arguments

      integer            :: un
      integer            :: irwf
      integer            :: nx
      integer            :: ny
      character (len=10) :: text1
      character (len=10) :: text2

! un          - Fileunit
! irwf        - Read/write format index
! nz,ny       - Dimensions of: fld
! text1,text2 - Text strings read from file

! Array arguments

      real :: fld(nx,ny)

! fld - The field to be written to the file.

! Local variables:

      integer :: i
      integer :: j

! i,j - Indices

! Write field data depending on read/write format:

      if (irwf <= 0) then

! No data is written

          return

      end if

      if (irwf == 1) then

! Write data using simple ASCII  format:

        write(un,2100) text1,text2,nx,ny, &
                 ((i,j,fld(i,j),i=1,nx),j=1,ny)

      end if

      if (irwf == 2) then

! Write data using simple binary format:

        write(un) text1,text2,nx,ny,((fld(i,j),i=1,nx),j=1,ny)

      end if

      return

 2100 format (2A10,2I3,/,32767(2I4,1P,E16.8,/))

! End of subroutine w2dfld

      end subroutine w2dfld

! **********************************************************************

      subroutine w3dfld(un,irwf,k,text1,text2,nx,ny,nz,fld)

! The subroutine writes to fileunit 'un' a 2D-field which are
! layer 'k' of the 3D-field 'fld' using read/write format
! (index) 'irwf'.

      implicit none

! Global variables:

! Scalar arguments

      integer            :: un
      integer            :: irwf
      integer            :: k
      integer            :: nx
      integer            :: ny
      integer            :: nz
      character (len=10) :: text1
      character (len=10) :: text2

! un          - Fileunit
! irwf        - Read/write format index
! k           - Index vertical layer
! nz,ny,nz    - Dimensions of: fld
! text1,text2 - Text strings read from file

! Array arguments

      real :: fld(nx,ny,nz)

! fld - The field to be written to the file.

! Local variables:

      integer :: i
      integer :: j

! i,j - Indices

! Write field data for layer 'k' depending on read/write format:

      if (irwf <= 0) then

! No data is written

          return

      end if

      if (irwf == 1) then

! Write data using simple ASCII  format:

        write(un,2100) text1,text2,nx,ny, &
                 ((i,j,fld(i,j,k),i=1,nx),j=1,ny)

      end if

      if (irwf == 2) then

! Write data using simple binary format:

        write(un) text1,text2,nx,ny,((fld(i,j,k),i=1,nx),j=1,ny)

      end if

      return

 2100 format (2A10,2I3,/,32767(2I4,1P,E16.8,/))

! End of subroutine w3dfld

      end subroutine w3dfld

! **********************************************************************

      integer function NEXTUN()

! The function returns the next free filunit.

      implicit none

! Local variables

      integer iu
      logical isopen

! iu     - Fileunit
! isopen - If fileunit is open then true else false

      iu     = 10
      isopen = .false.

  100 continue

! Check if current unit is opened

      INQUIRE(iu,OPENED=isopen)
      if (isopen) then
          iu = iu + 1
          go to 100
      end if

! Found free fileunit

      NEXTUN = iu

      return

! End of integer function NEXTUN

      end function NEXTUN

! **********************************************************************

      subroutine getrwf(fn,irwf)

! The subroutine returns a read or write format index 'iwf' depending on
! the given filename 'fn'.

      implicit none

! Global variables:

      character (len=256) :: fn
      integer             :: irwf

! fn  - Filename
! iwf - Read or write format index

! Local variables:

      integer :: i
      integer :: i1
      integer :: i2

! I     - Dummy index
! I1,I2 - Filename extension indices

! Find read or write format index depending on filename extension

      i1 = -9900
      i2 = INDEX(fn,' ') - 1
      do 100 i = i2,1,-1
          if (fn(i:i) == '.') then
              i1 = i + 1
              go to  110
          end if
  100 continue

  110 continue

      if (i1 == -9900) then

! No filename extension found

          irwf = 1

      else

! Filename extension found

          irwf = 1

! NILUs simple field ASCII  format:

          if (fn(i1:i2) == 'asc' .OR. fn(i1:i2) == 'csv') irwf = 1

! NILUs simple field Binary format:

          if (fn(i1:i2) == 'fld') irwf = 2

! NetCDF field format

          if (fn(i1:i2) == 'nc' ) irwf = 3

! HDF field format

          if (FN(i1:i2) == 'hdf') irwf = 4

      end if

      return

! End of subroutine getrwf

      end subroutine getrwf

! **********************************************************************

      subroutine opifil(fn,un,fe,fm)

! The subroutine opens an input file in either binary or ASCII mode
! depending on the filename extension. It also defines a file format
! index associated with the file.

      implicit none

! Global declaratione:

      character (len=256), intent(in) :: fn
      integer, intent(out)            :: un
      integer, intent(out)            :: fm
      logical, intent(out)            :: fe

! fn - File name
! un - File unit
! fm - File format index
! fe - If the file exists then true else false

! Local variables

      integer :: i
      integer :: irwf
      logical :: isopen

! i      - Index
! irwf   - File read/write format
! isopen - If file is open then true else false

! Check if file exists

      INQUIRE(FILE = fn, EXIST = fe)

! If first character is blank then no file

      if (fn(1:1) == ' ') fe = .false.

! File exists?

      if (fe) then

! Yes!

! Get file unit

        isopen = .false.
        INQUIRE (FILE=fn,NUMBER=un,OPENED=isopen)
        if (.not. isopen) un = NEXTUN()

! Get file format depending on filename extension

        call getrwf(fn,irwf)
        fm = irwf

! Open file in either binary or ASCII mode

        if (fm == 2) then

! Binary mode

          if (.not. isopen) &
       open(UNIT=un,FILE=fn,FORM='UNFORMATTED',ACTION='READ', &
            ERR=9000)

        else

! ASCII mode

          if (.not. isopen) &
       open(UNIT=un,FILE=fn,ACTION='READ',ERR=9000)

        end if

      else

! No!

! If the file is nonexistent then set missing unit and format

        un = INT(UNDEF)
        fm = INT(UNDEF)

      end if

      return

 9000 continue

! If error in open of file then stop

      if (fe_log) then

        write(funit_log,*) 'OPIFIL: Error in open of input file: ', &
                      (fn(i:i),i=1,INDEX(fn,' ') - 1)

        write(funit_log,*) 'OPIFIL: Error in open of input file!'

      end if

! End of subroutine opifil

      end subroutine opifil

! **********************************************************************

      subroutine clifil(un)

! The subroutine closes an input file.

      implicit none

      integer :: un

! un - File unit

! Local variables

      logical isopen

! isopen - If file is open then true else false

      if (un >= 10) then

! Check if file is open

        INQUIRE(un,OPENED=isopen)

! Close the file

        if (isopen) CLOSE(un)

      end if

      return

! End of subroutine clifil

      end subroutine clifil

! **********************************************************************

      subroutine r2dfld(un,irwf,text1,text2,nx,ny,fld)

! The subroutine reads from fileunit 'un' a 2D-field 'fld' using
! read/write format (index) 'irwf'.

      implicit none

! Scalar arguments

      integer un
      integer irwf
      integer nx
      integer ny
      character (len=10) :: text1
      character (len=10) :: text2

! un          - Fileunit
! irwf        - Read/write format index
! nx,ny       - Dimensions of 'fld'
! text1,text2 - Text strings read from file

! Array arguments

      real fld(nx,ny)

! FLD - The field

! Local variables

      integer :: i
      integer :: ii
      integer :: j
      integer :: jj
      integer :: ix
      integer :: iy

! i,ii,j,jj - Indices
! ix,iy     - Dimensions read from file

! Read field data depending on read/write format

      if (irwf < 0) THEN

! No data is read

          return

      end if

!     Below is a special treatment for Praha_topography:
      if (irwf == 0) then

! Read data using simple ASCII  format

        read (un,2100,END=120) text1,text2,ix,iy

        do i = 1,ix
          do j = 1,iy
             read (un,*,END=120) ii,jj,fld(i,j)
             if (ii /= i .OR. jj /= j) STOP
          end do
        end do

        read (un,*,END=120)

        if (ix /= nx .or. iy /= ny) go to 130

      end if
! *** End of special treatment of Praha_topography.

! *** Start of normal reading from 2D ascii format files:
      if (irwf == 1) then

! Read data using simple ASCII  format

!        read (un,2100,END=120) text1,text2,ix,iy
        read (un,2100,END=120) text1,text2,ix,iy
        print *,'text1,text2,ix,iy',text1,text2,ix,iy


        do 100 j = 1,iy
          do 110 i = 1,ix
             read (un,*,END=120) ii,jj,fld(i,j)
  110     continue
  100   continue

        read (un,*,END=120)

        if (ix /= nx .or. iy /= ny) go to 130

      end if

      if (irwf == 2) then

! Read data using simple binary format

        read (un,END=120) text1,text2,ix,iy, &
                    ((fld(i,j),i=1,ix),j=1,iy)

        if (ix /= nx .or. iy /= ny) go to 130

      end if

      return

  120 continue

! End of file

      if (fe_log) write (funit_log,2000) un,text1,text2
      print *, 'R2DFLD: End of file!'

  130 continue

! Checking field dimensions

      print *, 'debug ix iy' , ix, iy

      if (ix == 1 .and. iy == 1) then

! Homogeneous field

        do 140 j = 1,ny
          do 150 i = 1,nx
              fld(i,j) = fld(1,1)
  150     continue
  140   continue

        return

      else

! Field dimensions different from expected

          if (fe_log) write (funit_log,2010) un,nx,ny,ix,iy
          print *, 'R2DFLD: Error in field dimensions!'

      end if

 2000 FORMAT ('R2DFLD: End of file ',I2,' Last read ',2A10)
 2010 FORMAT ('R2DFLD: Error in field dimensions on file ',I2, &
         ' expects ',2I4,' but reads ',2I4,'!')
 2100 FORMAT (2A10,2I3)

! End of subroutine r2fld

      end subroutine r2dfld

! **********************************************************************
!
      subroutine read_obs(n_count,un,nmax,yyyy,mm,dd,hh,vector)
!
!     The subroutine reads the meteorological observations.
!
! **********************************************************************
      implicit none

!     Global declarations:
      integer, intent(IN)  :: n_count
      integer, intent(IN)  :: un
      integer, intent(IN)  :: nmax
      integer, intent(OUT) :: yyyy
      integer, intent(OUT) :: mm
      integer, intent(OUT) :: dd
      integer, intent(OUT) :: hh
      real,    intent(OUT) :: vector(nmax)

!     Local declarations:

      character (len=256) :: heading

!     Content of routine:

      vector = 0.0

      if(n_count == 1)then
        read(un,*) heading
        read(un,*) yyyy,mm,dd,hh,vector
      else
        read(un,*) yyyy,mm,dd,hh,vector
      end if !(n_count == 1)

!     End of subroutine read_obs

      end subroutine read_obs

! **********************************************************************
! *** FILES MODULE INTERFACE ROUTINES

! **************************************************************************************

      subroutine call_get_user_input
!DEC$ ATTRIBUTES DLLEXPORT,STDCALL,ALIAS:'call_get_user_input' :: call_get_user_input
      implicit none

      call get_user_input

      return

      end subroutine call_get_user_input



! **************************************************************************************


      SUBROUTINE SendFilesDefinitions(cv_fname_log,iv_fname_log_len)

!DEC$ ATTRIBUTES DLLEXPORT,STDCALL,ALIAS:'SendFilesDefinitions' :: SendFilesDefinitions
      IMPLICIT NONE

      integer                         :: iv_fname_log_len
      character(len=iv_fname_log_len) :: cv_fname_log

!DEC$ ATTRIBUTES REFERENCE :: cv_fname_log

! *** Local variables:

! *** Initializing the variables declared above in: "module_mc_wind_files"

      fname_log = cv_fname_log

      END SUBROUTINE SendFilesDefinitions



! **************************************************************************************     

      SUBROUTINE FreeFiles_Memory()

!DEC$ ATTRIBUTES DLLEXPORT,STDCALL,ALIAS:'FreeFiles_Memory' :: FreeFiles_Memory

      implicit none


      END SUBROUTINE FreeFiles_Memory


! End of module module_mc_wind_files

      end module module_mc_wind_files
