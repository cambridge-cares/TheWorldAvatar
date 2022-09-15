! <output_tapm_pse.for - A component of the City-scale
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

      subroutine output_tapm_pse(nhours,nps,pse_src_param,pse_src_qemhour)

!***********************************************************************
!***  Subroutine output_citychem_pse writes output for point sources
!***  in TAPM format (one file with meta data and emission values)
!***********************************************************************

!     Declarations of variables by using the MODULES feature:

      use module_uect_io
      use module_uect_exe
      use module_uect_time
      use module_uect_emis

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
      integer                           :: mode
      integer                           :: enha
      integer,dimension(ncc)            :: icp
      character(len=10)                 :: fb1
      character(len=16)                 :: fb2='(A10,I3,3X)'
      character(len=90)                 :: fb
      character(len=60)                 :: startdate_out
      character(len=10)                 :: pointid
      character(len=10)                 :: yyout, mmout, ddout
      real, dimension(nps,ncc,nhours)   :: pse_src_outhour
      real, dimension(nps,n_pse_params) :: pse_src_outparm
      real, dimension(nps)              :: pse_pmratio

!***********************************************************************
!     Content of subroutine:

! ***   Open the point source output files:

       fname_out_points      = trim(fname_outpath)//'/pointsrc_uect_'//trim(startdate)//'_'//trim(enddate)//'.pse'
       funit_out_points      = nextun()
       open (funit_out_points, file = fname_out_points, status = 'unknown', form  = 'formatted', action = 'write')


! ***   Write to output array

       pse_src_outparm = pse_src_param

! ***   TAPM needs stack T in degree K
       pse_src_outparm(:,9) = pse_src_param(:,9) + 273.15

! ***   TAPM specific parameters
       mode = 0     ! use EGM
       enha = 1     ! no enhancement of buoyancy due to near-by stacks


       write (funit_out_points, 1990) nps,nhours

       do n = 1, nps

           if (pse_src_qemhour(n,7,1).gt.0.0) then
             pse_pmratio(n) = pse_src_qemhour(n, 6,1) / pse_src_qemhour(n, 7,1)
           else
             pse_pmratio(n) = 0.50     ! default
           endif
           write(6,*) 'pmratio n',n,pse_pmratio(n)


           write (funit_out_points, 2000)  mode, pse_src_outparm(n,1),     &
                        pse_src_outparm(n,2), pse_src_outparm(n,4),        &
                        pse_src_outparm(n,5), enha, f_no_pse, pse_pmratio(n)

       enddo

! ***   End writing meta data

! ***   Write hourly emission values

       do h = 1, nhours
          do n = 1, nps
          
             pse_src_outhour(n, 1,h ) =  pse_src_qemhour(n, 7,h)   ! pm10
             pse_src_outhour(n, 2,h ) =  pse_src_qemhour(n, 8,h) + pse_src_qemhour(n, 9,h)   ! NOx
             pse_src_outhour(n, 3,h ) =  pse_src_qemhour(n, 4,h)   ! SO2
             pse_src_outhour(n, 4,h ) =  pse_src_qemhour(n,10,h)   ! RSMOG

             
! include stack velocity and stack temperature every hour
             write (funit_out_points,'(2F8.2, 2X, 4E10.2)' )  pse_src_outparm(n,10),pse_src_outparm(n,9),  &
                                            pse_src_outhour(n,1,h),pse_src_outhour(n,2,h),      &
                                            pse_src_outhour(n,3,h),pse_src_outhour(n,4,h)
                                                 
          enddo

       enddo

       
      return

 1990 format(I8 , I8)
 2000 format(I1 ,1X, 4F11.2, 2X, I1, 1X, 2F11.4)

      end subroutine output_tapm_pse
