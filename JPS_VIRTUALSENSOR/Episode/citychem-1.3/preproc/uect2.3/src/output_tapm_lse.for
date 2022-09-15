! <output_tapm_lse.for - A component of the City-scale
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
!*****************************************************************************!
!
!***********************************************************************
!***
!***      UECT
!***      Urban Emission Conversion Tool
!***
!***********************************************************************

      subroutine output_tapm_lse(nhours,nps,lse_src_param,lse_src_qemhour)

!***********************************************************************
!***  Subroutine output_tapm_lse writes output for line sources
!***  in TAPM format:
!***  one file with meta data and emission values
!***********************************************************************

!     Declarations of variables by using the MODULES feature:

! ***   Open the line source output emission value files:

      use module_uect_io
      use module_uect_exe
      use module_uect_time
      use module_uect_emis

      implicit none

!***********************************************************************

      integer, intent(in)             :: nhours
      integer, intent(in)             :: nps

      real, dimension(nps,ncomp,nhours),intent(in) :: lse_src_qemhour
      real, dimension(nps,n_lse_params),intent(in) :: lse_src_param


!     Local declarations:

! ***   Open the line source output emission value files:

      integer                           :: i,n,p,h
      integer                           :: yy, mony
      integer                           :: daynn, dayne
      integer                           :: hournn
      integer                           :: mode
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
      real, dimension(nps)              :: lse_pmratio

!***********************************************************************
!     Content of subroutine:

! ***   Open the line source output file:
       fname_out_lines      = trim(fname_outpath)//'/linesrc_uect_'//trim(startdate)//'_'//trim(enddate)//'.lse'
       funit_out_lines      = nextun()
       open (funit_out_lines, file = fname_out_lines, status = 'unknown', form  = 'formatted', action = 'write')


! ***   Write meta information file for line sources

       lse_src_outparm = lse_src_param

! ***   TAPM specific parameters
       mode = 0     ! use EGM

       write (funit_out_lines, 1990) nps,nhours
      
       
       do n = 1, nps

           if (lse_src_qemhour(n,7,1).gt.0.0) then
             lse_pmratio(n) = lse_src_qemhour(n,6,1) / lse_src_qemhour(n,7,1)
           else
             lse_pmratio(n) = 0.80     ! default
           endif

           write (funit_out_lines, 2000) mode,lse_src_outparm (n,1),lse_src_outparm (n,3), &
                        lse_src_outparm (n,5),lse_src_outparm (n,2),lse_src_outparm (n,4), &
                        lse_src_outparm (n,6),f_no_lse,lse_pmratio(n)

       enddo

! ***   End writing meta data


! ***   Write hourly emission values

       do h = 1, nhours
          do n = 1, nps

            lse_src_outhour(n, 1,h ) =  lse_src_qemhour(n, 7,h)   ! pm10
            lse_src_outhour(n, 2,h ) =  lse_src_qemhour(n, 8,h) + lse_src_qemhour(n, 9,h)   ! NOx
            lse_src_outhour(n, 3,h ) =  lse_src_qemhour(n, 4,h)   ! SO2
            lse_src_outhour(n, 4,h ) =  lse_src_qemhour(n,10,h)   ! RSMOG

            write (funit_out_lines,'(4E10.3)' ) lse_src_outhour(n,1,h),lse_src_outhour(n,2,h), &
                                                 lse_src_outhour(n,3,h),lse_src_outhour(n,4,h)
                                                 
          enddo

       enddo


      return

 1990 format(I8 , I8)
 2000 format(I1 ,1X, 6F11.2, 2F11.4)

      end subroutine output_tapm_lse
