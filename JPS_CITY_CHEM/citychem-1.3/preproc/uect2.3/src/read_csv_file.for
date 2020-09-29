! <read_csv_file.for - A component of the City-scale
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

      subroutine read_csv_file(inunit,incols,inlines,data_array,snap_vec)

!***********************************************************************
!***  Subroutine read_csv_file reads emission files in CSV-format
!***********************************************************************

!     Declarations of variables by using the MODULES feature:

      use module_uect_io


      implicit none

!***********************************************************************

      integer, intent(in)             :: inunit
      integer, intent(in)             :: incols
      integer, intent(in)             :: inlines
      real, dimension(inlines,incols),intent(out) :: data_array
      integer, dimension(inlines),intent(out)     :: snap_vec

!     Local declarations:

      integer                         :: resin
      integer                         :: in
      character (len=256)             :: header
      logical                         :: leof


!***********************************************************************
!     Content of subroutine:


      ! read header line
      call nxtdat(inunit,leof)
      read(inunit,fmt='(A)', iostat=resin) header
      print *,inunit
      print *,header

      print *,'inlines',inlines
      print *,'if not read: open in nedit and save as unix file!'

      ! read data table
      do in = 1, inlines

        print *,'in',in
        call nxtdat(inunit,leof)

        read(inunit,*,end=999) snap_vec(in), data_array(in,:)
        !print *,'snap data ',snap_vec(in), data_array(in,:)

      end do

      return


  999    continue
      if (fe_log) then
          WRITE (funit_log,2010)
      endif
      call stopit('read_csv_file: end of input source file!')

 2010 format('read_csv_file: end of input source file!')

      end subroutine read_csv_file
