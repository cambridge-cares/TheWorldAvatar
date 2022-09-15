! <output_citychem_pse.for - A component of the City-scale
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

      subroutine output_citychem_pse(nhours,nps,pse_src_param,pse_src_qemhour)

!***********************************************************************
!***  Subroutine output_citychem_pse writes output for point sources
!***  in CityChem format (one file with meta data and emission values)

!!!+++++++++++++++++++++++++++++
!!  modified by Kang @ CARES, @ Dec, 2019:
!!  1, adding moving point 
!!  2, editing the point source output file date value to correct simluation time (hour)      
!***********************************************************************

!     Declarations of variables by using the MODULES feature:

      use module_uect_io
      use module_uect_exe
      use module_uect_time


      implicit none

!***********************************************************************

      integer, intent(in)             :: nhours
      integer, intent(in)             :: nps

      real, dimension(nps,ncomp,nhours),intent(in) :: pse_src_qemhour
      real, dimension(nps,n_pse_params),intent(in) :: pse_src_param


!     Local declarations:

      integer                           :: i,n,p,h
      integer                           :: yy, mony
      integer                           :: daynn, hournn
      integer                           :: minunn, secunn
      integer                           :: secu
      integer                           :: penergy
      integer,dimension(ncc)            :: icp
      character(len=10)                 :: fb1,fb3
      character(len=16)                 :: fb2='(A10,I3,3X)'
      character(len=90)                 :: fb
      character(len=90)                 :: fbiso
      character(len=60)                 :: startdate_out
      character(len=10)                 :: pointid
!!!==============================================
!!! changed by Kang @ CARES
!!! change the simluation time appeared in point source output file  
!! orig:      character(len=10)                 :: yyout, mmout, ddout
      character(len=10)                 :: yyout, mmout, ddout, hhout
!!!============================================== 
           
      real, dimension(nps,ncc,nhours)   :: pse_src_outhour
      real, dimension(nps,n_pse_params) :: pse_src_outparm

!***********************************************************************
!     Content of subroutine:

! ***   Open the point source output files:

       fname_out_points      = trim(fname_outpath)//'/pointsrc_'//trim(startdate)//'_'//trim(enddate)//'.txt'
       funit_out_points      = nextun()
       open (funit_out_points, file = fname_out_points, status = 'unknown', form  = 'formatted', action = 'write')

       !fname_out_points_iso  = trim(fname_outpath)//'/pointsrc_'//trim(startdate)//'_'//trim(enddate)//'iso.txt'
       !funit_out_points_iso  = nextun()
       !open (funit_out_points_iso, file = fname_out_points_iso, status = 'unknown', form  = 'formatted', action = 'write')

! ***   Set date
       yy     = year-2000
       mony   = mnth
       daynn  = daym
!!!==============================================
!!! changed by Kang @ CARES
!!! change the simluation time appeared in point source output file  
!! orig:       hournn = 0
       hournn = hour
!!!==============================================
       
       minunn = 0
       secunn = 0

        do i=1,ncc
          icp( i ) = i
        enddo

! *** CITYCHEM compound names
!ncc: all citychem output species
! When adding new compound, FORMAT 2000 has to be changed
       cpname( 1) = 'o3        '
       cpname( 2) = 'no        '
       cpname( 3) = 'no2       '
       cpname( 4) = 'h2o2      '
       cpname( 5) = 'n2o5      '
       cpname( 6) = 'hno3      '
       cpname( 7) = 'hono      '
       cpname( 8) = 'so2       '
       cpname( 9) = 'sulphate  '
       cpname(10) = 'co        '
       cpname(11) = 'c2h6      '
       cpname(12) = 'hcho      '
       cpname(13) = 'ch3cho    '
       cpname(14) = 'c2h4      '
       cpname(15) = 'pan       '
       cpname(16) = 'nc4h10    '
       cpname(17) = 'ch3coc2h5 ' 
       cpname(18) = 'c3h6      '
       cpname(19) = 'oxylen    '
       cpname(20) = 'isoprene  '
       cpname(21) = 'pm2.5     '
       cpname(22) = 'pm10      '



! ***   Formatting line output
       write (fb1,'(I6)') 2*ncc
       write (fb3,'(I6)') 2*ncc+1
       fb = "("//'(I4,4X)'//trim(adjustl(fb1))//fb2//")" 
       fbiso = "("//'(I4,4X)'//trim(adjustl(fb3))//fb2//")" 

       write (yyout,'(I2)') yy
       write (mmout,'(I2)') mony
       write (ddout,'(I2)') daynn
!!!==============================================
!!! changed by Kang @ CARES
!!! change the simluation time appeared in point source output file        
!!  orig:       startdate_out = trim(yyout)//'        '//trim(mmout)//'         '//trim(ddout)//'         0         0         0         '
       write (hhout,'(I2)') hournn
       startdate_out = trim(yyout)//'        '//trim(mmout)//'         '//trim(ddout)//'         '&
                      //trim(hhout)//'         0         0         '
!!!==============================================
       
! ***   Write 4 info lines
       write (funit_out_points ,fb)                ncc,( cpname( i ), icp( i ),i=1,ncc)
       write (funit_out_points ,'(I4,4X,I4)'  )    iqu,iqt
       write (funit_out_points ,'(A61)'       )    startdate_out
       write (funit_out_points ,'(I4)'        )    n_sopp2

       !write (funit_out_points_iso ,fbiso)         ncc,( cpname( i ), icp( i ),i=1,ncc)
       !write (funit_out_points_iso ,'(I4,4X,I4)'  )    iqu,iqt
       !write (funit_out_points_iso ,'(A61)'       )    startdate_out
       !write (funit_out_points_iso ,'(I4)'        )    n_sopp2

! ***   Write to output array

       pse_src_outparm = pse_src_param
       ! CityChem needs stack diameter = 2*radius
       pse_src_outparm(:,5) = 2.* pse_src_param(:,5)

       do h = 1, nhours
          do n = 1, nps

             pse_src_outhour(n, 1,h ) =  0.00                      ! O3
             pse_src_outhour(n, 2,h ) =  pse_src_qemhour(n, 8,h)   ! NO
             pse_src_outhour(n, 3,h ) =  pse_src_qemhour(n, 9,h)   ! NO2
             pse_src_outhour(n, 4,h ) =  0.00                      ! H2O2
             pse_src_outhour(n, 5,h ) =  0.00                      ! N2O5
             pse_src_outhour(n, 6,h ) =  0.00                      ! HNO3
             pse_src_outhour(n, 7,h ) =  0.00                      ! HONO
             pse_src_outhour(n, 8,h ) =  pse_src_qemhour(n, 4,h)   ! SO2
             pse_src_outhour(n, 9,h ) =  0.00                      ! H2SO4
             pse_src_outhour(n,10,h ) =  pse_src_qemhour(n, 3,h)   ! CO
             pse_src_outhour(n,11,h ) =  pse_src_qemhour(n,12,h)   ! c2h6
             pse_src_outhour(n,12,h ) =  pse_src_qemhour(n,11,h)   ! hcho
             pse_src_outhour(n,13,h ) =  pse_src_qemhour(n,13,h)   ! ch3cho
             pse_src_outhour(n,14,h ) =  pse_src_qemhour(n,15,h)   ! c2h4
             pse_src_outhour(n,15,h ) =  0.00                      ! PAN
             pse_src_outhour(n,16,h ) =  pse_src_qemhour(n,14,h)   ! nc4h10
             pse_src_outhour(n,17,h ) =  pse_src_qemhour(n,18,h)   ! ch3coc2h5
             pse_src_outhour(n,18,h ) =  pse_src_qemhour(n,16,h)   ! c3h6
             pse_src_outhour(n,19,h ) =  pse_src_qemhour(n,17,h)   ! oxylen
             pse_src_outhour(n,20,h ) =  0.0                       ! isoprene
             pse_src_outhour(n,21,h ) =  pse_src_qemhour(n, 6,h)   ! pm25
             pse_src_outhour(n,22,h ) =  pse_src_qemhour(n, 7,h)   ! pm10


             if (n<10) then
               write (pointid,'(I1)') n
               pointid = '000000'//trim(pointid)
             else if (n<100) then
               write (pointid,'(I2)') n
               pointid = '00000'//trim(pointid)
             else if (n<1000) then
               write (pointid,'(I3)') n
               pointid = '0000'//trim(pointid)
             else if (n<10000) then
               write (pointid,'(I4)') n
               pointid = '000'//trim(pointid)
             endif

             penergy = int(pse_src_outparm (n, 8))

! *** Change write format (16E10.2) if ncc changed
!             write (funit_out_points, 2000) pointid, (pse_src_outparm (n,p), p=1,7 ),             &
!                                            penergy, (pse_src_outparm (n,p), p=9,n_pse_params),   &
!                                            ( pse_src_outhour(n,i,h), i=1,ncc )
!!!!!!!===========================================================================================
!!!===================================
!!! for moving point by Kang
!!!                                                  
             write (funit_out_points, 2020) pointid, (pse_src_outparm (n,p), p=1,7 ),             &
                                            penergy, (pse_src_outparm (n,p), p=9,n_pse_params),   &
                                            ( pse_src_outhour(n,i,h), i=1,ncc )
!!!!!!!===========================================================================================                                            
! *** Same output but including isoprene
!             write (funit_out_points_iso, 2010) pointid, (pse_src_outparm (n,p), p=1,7 ),             &
!                                            penergy, (pse_src_outparm (n,p), p=9,n_pse_params),   &
!                                            ( pse_src_outhour(n,i,h), i=1,ncc )

           !print *,'HS after output: ', n, pse_src_outparm(n, 4)


            enddo

! *** Write Date line
          write (funit_out_points,'(I4,4X,I4,4X,I4,4X,I4,4X,I4,4X,I4,4X)')  yy,mony,daynn,hournn,minu,secunn
          !write (funit_out_points_iso,'(I4,4X,I4,4X,I4,4X,I4,4X,I4,4X,I4,4X)')  yy,mony,daynn,hournn,minu,secunn

! *** Change date by one hour
          hournn = hournn + 1
          if ( hournn == 24 ) then
            hournn  = 0
            daynn   = daynn + 1
! *** Check if new month?
            if ( daynn > NDAY(mony) ) then
              daynn = 1
              mony  = mony + 1
           endif
          endif


       enddo

      return

!!!===================================
!!! for moving point by Kang
!!!
 2020 format(A10,2X, 7F11.2, I6, 7F11.2,2X, 22E10.2)
!!!  2000 format(A10,2X, 7F11.2, I6, 2F11.2,2X, 22E10.2)
!!! 2010 format(A10,2X, 7F11.2, I6, 2F11.2,2X, 22E10.2)

      end subroutine output_citychem_pse
