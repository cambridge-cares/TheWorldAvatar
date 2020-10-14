! <output_citychem_lse.for - A component of the City-scale
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

      subroutine output_citychem_lse(nhours,nps,lse_src_param,lse_src_qemhour)

!***********************************************************************
!***  Subroutine output_citychem_lse writes output for line sources
!***  in CityChem format:
!***  one file with meta data one file with emission values
!***********************************************************************

!     Declarations of variables by using the MODULES feature:

      use module_uect_io
      use module_uect_exe
      use module_uect_time


      implicit none

!***********************************************************************

      integer, intent(in)             :: nhours
      integer, intent(in)             :: nps

      real, dimension(nps,ncomp,nhours),intent(in) :: lse_src_qemhour
      real, dimension(nps,n_lse_params),intent(in) :: lse_src_param


!     Local declarations:

      integer                           :: i,n,p,h
      integer                           :: yy, mony
      integer                           :: daynn, dayne
      integer                           :: hournn
      !integer                           :: minunn, secunn
      !integer                           :: secu
      integer                           :: qlrindex
      integer,dimension(ncc)            :: icp
      character(len=10)                 :: fb1
      character(len=16)                 :: fb2='(A10,I3,3X)'
      character(len=90)                 :: fb
      character(len=60)                 :: startdate_out
      character(len=60)                 :: enddate_out
      character(len=10)                 :: yyout, mmout
      character(len=10)                 :: ddout, deout
      character (len=256)               :: fname_lines_loc
      real, dimension(nps,n_lse_params) :: lse_src_outparm
      real, dimension(nps,nccout,nhours):: lse_src_outhour


!***********************************************************************
!     Content of subroutine:

! ***   Open the line source output metadata file:
       fname_out_lines      = trim(fname_outpath)//'/linesrc_meta_'//trim(startdate)//'_'//trim(enddate)//'.txt'
       funit_out_lines      = nextun()
       open (funit_out_lines, file = fname_out_lines, status = 'unknown', form  = 'formatted', action = 'write')

       do i=1,ncc
         icp( i ) = i
       enddo

! *** CITYCHEM compound names
!nccout: all citychem output species
       cpnameo( 1) = 'no        '
       cpnameo( 2) = 'no2       '
       cpnameo( 3) = 'so2       '
       cpnameo( 4) = 'co        '
       cpnameo( 5) = 'c2h6      '
       cpnameo( 6) = 'hcho      '
       cpnameo( 7) = 'ch3cho    '
       cpnameo( 8) = 'c2h4      '
       cpnameo( 9) = 'nc4h10    '
       cpnameo(10) = 'ch3coc2h5 ' 
       cpnameo(11) = 'c3h6      '
       cpnameo(12) = 'oxylen    '
       cpnameo(13) = 'pm2.5     '
       cpnameo(14) = 'pm10      '
       cpnameo(15) = 'isoprene  '
       cpnameo(16) = 'hono      '


! ***   Open the line source output emission value files:

       do i=1,nccout-1   ! 16 lse files
         fname_lines_loc         = trim(fname_outpath)//'/linesrc_traffic_'
         fname_out_lines_emis(i) = trim(fname_lines_loc)//trim(cpnameo(i))//'_'//trim(startdate)//'_'//trim(enddate)//'.txt'
         funit_out_lines_emis(i) = nextun()
         open (funit_out_lines_emis(i), file = fname_out_lines_emis(i), status = 'unknown', form  = 'formatted', action = 'write')
       enddo

! ***   Formatted output of the header lines in the line source meta information file
! 8 comment lines
       write (funit_out_lines,'(A38)'       )    simid
       write (funit_out_lines,'(A19)'       )    '* ADT_limit:   1000'
       write (funit_out_lines,'(A33,1X,I6)' )    '* Total number of lines sources =',n_soll2
       write (funit_out_lines,'(A33)'       )    '* Number of lines put into grid ='
       write (funit_out_lines,'(A27)'       )    '* Number of lines ferries ='
       write (funit_out_lines,'(A25)'       )    '* Number of line sources:'
       write (funit_out_lines,'(I6)'        )    n_soll2
       write (funit_out_lines,'(A47)'       )    '*	IQLV	X1V	X2V	Y1V	Y2V	Z1V	Z2V	WV	RMAXV	INDICES'

! ***   Write meta information file for line sources

       lse_src_outparm = lse_src_param

       ! CityChem needs width of lane = 0.5*Width
       lse_src_outparm(:,7) = 0.5* lse_src_param(:,7)

       do n = 1, nps

           qlrindex = int(lse_src_outparm (n, 9))

           write (funit_out_lines, 2000) n, (lse_src_outparm (n,p), p=1,8 ), qlrindex

       enddo

! ***   End writing meta data

! ***   Write header for line source emission files

! ***   Set date
        yy     = year
        mony   = mnth
        daynn  = daym
        dayne  = edat(3)
        write (yyout,'(I4)') yy
        write (mmout,'(I2)') mony
        write (ddout,'(I2)') daynn
        write (deout,'(I2)') dayne

        startdate_out = trim(yyout)//' '//trim(mmout)//' '//trim(ddout)//'  0'
        enddate_out   = trim(yyout)//' '//trim(mmout)//' '//trim(deout)//' 23'

! ***   Write first 2 info lines
        do i=1,nccout-1 ! 16 lse files
            write (funit_out_lines_emis(i),'(A38)'  )    simid
            write (funit_out_lines_emis(i),'(A41)'  )    '* Hourly average linesource emission file'
            write (funit_out_lines_emis(i),'(A33)'  )    '*      Compound: '//trim(cpnameo(i))//'              '
            write (funit_out_lines_emis(i),'(A19)'  )    '* ADT_limit:   1000'
            write (funit_out_lines_emis(i),'(A17,A14)')  '* Starting date: ',trim(startdate_out)
            write (funit_out_lines_emis(i),'(A17,A14)')  '*   Ending date: ',trim(enddate_out)
            write (funit_out_lines_emis(i),'(A12)'  )    '*       QL1V'
            write (funit_out_lines_emis(i),'(A12)'  )    '*    compact'
        enddo



! ***   Write hourly emission values

       do h = 1, nhours
          do n = 1, nps

            lse_src_outhour(n, 1,h ) =  lse_src_qemhour(n, 8,h)   ! NO
            lse_src_outhour(n, 2,h ) =  lse_src_qemhour(n, 9,h)   ! NO2
            lse_src_outhour(n, 3,h ) =  lse_src_qemhour(n, 4,h)   ! SO2
            lse_src_outhour(n, 4,h ) =  lse_src_qemhour(n, 3,h)   ! CO
            lse_src_outhour(n, 5,h ) =  lse_src_qemhour(n,12,h)   ! c2h6
            lse_src_outhour(n, 6,h ) =  lse_src_qemhour(n,11,h)   ! hcho
            lse_src_outhour(n, 7,h ) =  lse_src_qemhour(n,13,h)   ! ch3cho
            lse_src_outhour(n, 8,h ) =  lse_src_qemhour(n,15,h)   ! c2h4
            lse_src_outhour(n, 9,h ) =  lse_src_qemhour(n,14,h)   ! nc4h10
            lse_src_outhour(n,10,h ) =  lse_src_qemhour(n,18,h)   ! ch3coc2h5
            lse_src_outhour(n,11,h ) =  lse_src_qemhour(n,16,h)   ! c3h6
            lse_src_outhour(n,12,h ) =  lse_src_qemhour(n,17,h)   ! oxylen
            lse_src_outhour(n,13,h ) =  lse_src_qemhour(n, 6,h)   ! pm25
            lse_src_outhour(n,14,h ) =  lse_src_qemhour(n, 7,h)   ! pm10

            lse_src_outhour(n,15,h ) =  lse_src_qemhour(n,19,h)   ! isoprene
            lse_src_outhour(n,16,h ) =  lse_src_qemhour(n,22,h)   ! hono

             do i=1,nccout-1  ! 16 lse files

               write (funit_out_lines_emis(i),'(E10.3)' )  lse_src_outhour(n,i,h)
 
             enddo

          enddo

       enddo


      return


 2000 format(I6 ,1X, 8F11.1, I6)

      end subroutine output_citychem_lse
