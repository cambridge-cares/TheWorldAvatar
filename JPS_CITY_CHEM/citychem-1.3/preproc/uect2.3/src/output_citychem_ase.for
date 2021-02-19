! <output_citychem_ase.for - A component of the City-scale
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

      subroutine output_citychem_ase(nhours,nps,ase_snap,xi,yi,     &
                                     ase_src_param,ase_src_qemhour)

!***********************************************************************
!***  Subroutine output_citychem_ase writes output for area sources
!***  in CityChem format:
!***  one file with emission values for each compound
!***  for 3 different CityChem emission categories
!***  1 Residential Heating                  (=SNAP 2        )
!***  2 Commercial and Industrial sources    (=SNAP 1,3,4,5,9)
!***  3 Solvent Use and Traffic              (=SNAP 6,7,10   )
!***  (4 Shipping is not applied in UECT)
!***  The routine will stop for area sources with a different SNAP
!***  EMEP vertical profiles:
!***  1    0.0   0.00  0.15  0.40  0.30  0.15  0.00    ! SNAP1
!***  2    1.0   0.00  0.00  0.00  0.00  0.00  0.0     ! SNAP2
!***  3    0.1   0.10  0.15  0.30  0.30  0.05  0.0     ! SNAP3
!***  4    0.9   0.10  0.00  0.00  0.00  0.00  0.0     ! SNAP4
!***  5    0.9   0.10  0.00  0.00  0.00  0.00  0.0     ! SNAP5
!***  6    1.0   0.00  0.00  0.00  0.00  0.00  0.0     ! SNAP6
!***  7    1.0   0.00  0.00  0.00  0.00  0.00  0.0     ! SNAP7
!***  8    1.0   0.00  0.00  0.00  0.00  0.00  0.0     ! SNAP8
!***  9    0.1   0.15  0.40  0.35  0.00  0.00  0.0     ! SNAP9
!*** 10    1.0   0.00  0.00  0.00  0.00  0.00  0.0     ! SNAP10
!*** 11    1.0   0.00  0.00  0.00  0.00  0.00  0.0     ! SNAP11
!***********************************************************************
!
! Modification change log
! -----------------------
! 14.06.2018 M.Karl(HZG)    new netcdf output for SNAP8 (ship emission)
!***********************************************************************
!     Declarations of variables by using the MODULES feature:

      use module_uect_io
      use module_uect_exe
      use module_uect_time
      use module_writenc

      implicit none

!***********************************************************************

      integer, intent(in)             :: nhours
      integer, intent(in)             :: nps

      integer, dimension(nps),intent(in)           :: ase_snap
      real, dimension(nps,ncomp,nhours),intent(in) :: ase_src_qemhour
      real, dimension(nps,n_ase_params),intent(in) :: ase_src_param
      integer, dimension(nps), intent(in)          :: xi
      integer, dimension(nps), intent(in)          :: yi

!     Local declarations:

      integer                           :: i,n,p,h
      integer                           :: jx,jy
      integer                           :: hourc
      integer                           :: narea_dome
      integer                           :: narea_solv
      integer                           :: narea_othe
      integer                           :: narea_ship

      integer                           :: yy, mony
      integer                           :: daynn, dayne
      integer                           :: hournn
      integer                           :: yeai
      integer                           :: daymm

      integer, dimension(12), parameter :: monlen  = (/ 31, 28, 31, 30, 31, 30,  &
                                                        31, 31, 30, 31, 30, 31   /)

      character(len=60)         :: startdate_out
      character(len=60)         :: enddate_out
      character(len=10)         :: yyout, mmout
      character(len=10)         :: ddout, deout
      character(len=48)         :: line
      character(len=43)         :: comm1

      character (len=256)       :: fname_area_loc1
      character (len=256)       :: fname_area_loc2
      character (len=256)       :: fname_area_loc3
      character (len=256)       :: fname_area_loc4
      character(len=10)         :: txt1
      character(len=10)         :: txt2
      character (len=256)       :: epsgn_u

! *** Emission: hourly compound emission amount per category, in g/s
      real, dimension(:,:,:,:), allocatable :: ase_src_dome
      real, dimension(:,:,:,:), allocatable :: ase_src_solv
      real, dimension(:,:,:,:), allocatable :: ase_src_othe
      real, dimension(:,:,:,:), allocatable :: ase_src_ship

      real,   allocatable :: axm_i(:,:)
      real,   allocatable :: axm_j(:,:)
      real,   allocatable :: z(:)
      double precision,   allocatable :: field2D_1(:,:)
      double precision,   allocatable :: field2D_2(:,:)
      double precision,   allocatable :: field2D_3(:,:)
      double precision,   allocatable :: field2D_4(:,:)

!***********************************************************************
!     Content of subroutine:


! ***  ALLOCATE
       if (.not. allocated(ase_src_dome))  allocate( ase_src_dome(n_nx,n_ny,nhours,nccout) )
       if (.not. allocated(ase_src_solv))  allocate( ase_src_solv(n_nx,n_ny,nhours,nccout) )
       if (.not. allocated(ase_src_othe))  allocate( ase_src_othe(n_nx,n_ny,nhours,nccout) )
       if (.not. allocated(ase_src_ship))  allocate( ase_src_ship(n_nx,n_ny,nhours,nccout) )
       if (.not. allocated(axm_i) )        allocate( axm_i(n_nx,n_ny) )
       if (.not. allocated(axm_j) )        allocate( axm_j(n_nx,n_ny) )
       if (.not. allocated(z) )            allocate( z(n_nz) )
       if (.not. allocated(field2D_1) )    allocate( field2D_1(n_ny,n_nx) )
       if (.not. allocated(field2D_2) )    allocate( field2D_2(n_ny,n_nx) )
       if (.not. allocated(field2D_3) )    allocate( field2D_3(n_ny,n_nx) )
       if (.not. allocated(field2D_4) )    allocate( field2D_4(n_ny,n_nx) )

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
       cpnameo(16) = 'apinene   '
       cpnameo(17) = 'limonene  '

! ***   Open the area source output emission value files:

       do i=1,nccout
         fname_area_loc1         = trim(fname_outpath)//'/asrc_domestic_'
         fname_area_loc2         = trim(fname_outpath)//'/asrc_solvent_'
         fname_area_loc3         = trim(fname_outpath)//'/asrc_other_'
         fname_area_loc4         = trim(fname_outpath)//'/asrc_ships_'

         fname_out_adome_emis(i) = trim(fname_area_loc1)//trim(cpnameo(i))//'_'//trim(startdate)//'_'//trim(enddate)//'.txt'
         funit_out_adome_emis(i) = nextun()
         open (funit_out_adome_emis(i), file = fname_out_adome_emis(i), status = 'unknown', form  = 'formatted', action = 'write')

         fname_out_asolv_emis(i) = trim(fname_area_loc2)//trim(cpnameo(i))//'_'//trim(startdate)//'_'//trim(enddate)//'.txt'
         funit_out_asolv_emis(i) = nextun()
         open (funit_out_asolv_emis(i), file = fname_out_asolv_emis(i), status = 'unknown', form  = 'formatted', action = 'write')

         fname_out_aothe_emis(i) = trim(fname_area_loc3)//trim(cpnameo(i))//'_'//trim(startdate)//'_'//trim(enddate)//'.txt'
         funit_out_aothe_emis(i) = nextun()
         open (funit_out_aothe_emis(i), file = fname_out_aothe_emis(i), status = 'unknown', form  = 'formatted', action = 'write')

         fname_out_aship_emis(i) = trim(fname_area_loc4)//trim(cpnameo(i))//'_'//trim(startdate)//'_'//trim(enddate)//'.txt'
         funit_out_aship_emis(i) = nextun()
         open (funit_out_aship_emis(i), file = fname_out_aship_emis(i), status = 'unknown', form  = 'formatted', action = 'write')

       enddo


! ***   Write header for area source emission files

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

! ***   Set comment lines
        line  = '* ----------------------------------------------'
        comm1 = '* Hourly average gridded area emission file'

! ***   Write header lines

        do i=1,nccout
            write (funit_out_adome_emis(i),'(A38)'  )    simid
            write (funit_out_asolv_emis(i),'(A38)'  )    simid
            write (funit_out_aothe_emis(i),'(A38)'  )    simid
            write (funit_out_aship_emis(i),'(A38)'  )    simid
            write (funit_out_adome_emis(i),'(A48)'  )    line
            write (funit_out_asolv_emis(i),'(A48)'  )    line
            write (funit_out_aothe_emis(i),'(A48)'  )    line
            write (funit_out_aship_emis(i),'(A48)'  )    line
            write (funit_out_adome_emis(i),'(A43)'  )    comm1
            write (funit_out_asolv_emis(i),'(A43)'  )    comm1
            write (funit_out_aothe_emis(i),'(A43)'  )    comm1
            write (funit_out_aship_emis(i),'(A43)'  )    comm1
            write (funit_out_adome_emis(i),'(A33)'  )    '*      Compound: '//trim(cpnameo(i))//'              '
            write (funit_out_asolv_emis(i),'(A33)'  )    '*      Compound: '//trim(cpnameo(i))//'              '
            write (funit_out_aothe_emis(i),'(A33)'  )    '*      Compound: '//trim(cpnameo(i))//'              '
            write (funit_out_aship_emis(i),'(A33)'  )    '*      Compound: '//trim(cpnameo(i))//'              '
            write (funit_out_adome_emis(i),'(A20)'  )    '*         Units: g/s'
            write (funit_out_asolv_emis(i),'(A20)'  )    '*         Units: g/s'
            write (funit_out_aothe_emis(i),'(A20)'  )    '*         Units: g/s'
            write (funit_out_aship_emis(i),'(A20)'  )    '*         Units: g/s'
            write (funit_out_adome_emis(i),'(A17,A14)')  '* Starting date: ',trim(startdate_out)
            write (funit_out_asolv_emis(i),'(A17,A14)')  '* Starting date: ',trim(startdate_out)
            write (funit_out_aothe_emis(i),'(A17,A14)')  '* Starting date: ',trim(startdate_out)
            write (funit_out_aship_emis(i),'(A17,A14)')  '* Starting date: ',trim(startdate_out)
            write (funit_out_adome_emis(i),'(A17,A14)')  '*   Ending date: ',trim(enddate_out)
            write (funit_out_asolv_emis(i),'(A17,A14)')  '*   Ending date: ',trim(enddate_out)
            write (funit_out_aothe_emis(i),'(A17,A14)')  '*   Ending date: ',trim(enddate_out)
            write (funit_out_aship_emis(i),'(A17,A14)')  '*   Ending date: ',trim(enddate_out)
            write (funit_out_adome_emis(i),'(A48)'  )    line
            write (funit_out_asolv_emis(i),'(A48)'  )    line
            write (funit_out_aothe_emis(i),'(A48)'  )    line
            write (funit_out_aship_emis(i),'(A48)'  )    line
        enddo


! ***   Write hourly emission values
       ! hour counter
       hourc      = 1
       ! source counter
       narea_dome = 0
       narea_solv = 0
       narea_othe = 0
       narea_ship = 0
       !Initialize emission output fields
       ase_src_dome( :,:,:,: ) = 0.0
       ase_src_solv( :,:,:,: ) = 0.0
       ase_src_othe( :,:,:,: ) = 0.0
       ase_src_ship( :,:,:,: ) = 0.0


       do h = 1, nhours

      
           do n = 1, nps
           
          ! print *,'out_ase snap',n,ase_snap(n),ase_src_qemhour(n, 8,h) 
           
            select case (ase_snap(n))

              case(1)
                ! combustion for energy production
                ! call stopit('SNAP 1 not allowed for area sources')
                ! now attributed to 'other'
                ase_src_othe(xi(n),yi(n),h,1 ) = ase_src_othe(xi(n),yi(n),h,1 ) + ase_src_qemhour(n, 8,h)   ! NO
                ase_src_othe(xi(n),yi(n),h,2 ) = ase_src_othe(xi(n),yi(n),h,2 ) + ase_src_qemhour(n, 9,h)   ! NO2
                ase_src_othe(xi(n),yi(n),h,3 ) = ase_src_othe(xi(n),yi(n),h,3 ) + ase_src_qemhour(n, 4,h)   ! SO2
                ase_src_othe(xi(n),yi(n),h,4 ) = ase_src_othe(xi(n),yi(n),h,4 ) + ase_src_qemhour(n, 3,h)   ! CO
                ase_src_othe(xi(n),yi(n),h,5 ) = ase_src_othe(xi(n),yi(n),h,5 ) + ase_src_qemhour(n,12,h)   ! c2h6
                ase_src_othe(xi(n),yi(n),h,6 ) = ase_src_othe(xi(n),yi(n),h,6 ) + ase_src_qemhour(n,11,h)   ! hcho
                ase_src_othe(xi(n),yi(n),h,7 ) = ase_src_othe(xi(n),yi(n),h,7 ) + ase_src_qemhour(n,13,h)   ! ch3cho
                ase_src_othe(xi(n),yi(n),h,8 ) = ase_src_othe(xi(n),yi(n),h,8 ) + ase_src_qemhour(n,15,h)   ! c2h4
                ase_src_othe(xi(n),yi(n),h,9 ) = ase_src_othe(xi(n),yi(n),h,9 ) + ase_src_qemhour(n,14,h)   ! nc4h10
                ase_src_othe(xi(n),yi(n),h,10) = ase_src_othe(xi(n),yi(n),h,10) + ase_src_qemhour(n,18,h)   ! ch3coc2h5
                ase_src_othe(xi(n),yi(n),h,11) = ase_src_othe(xi(n),yi(n),h,11) + ase_src_qemhour(n,16,h)   ! c3h6
                ase_src_othe(xi(n),yi(n),h,12) = ase_src_othe(xi(n),yi(n),h,12) + ase_src_qemhour(n,17,h)   ! oxylen
                ase_src_othe(xi(n),yi(n),h,13) = ase_src_othe(xi(n),yi(n),h,13) + ase_src_qemhour(n, 6,h)   ! pm25
                ase_src_othe(xi(n),yi(n),h,14) = ase_src_othe(xi(n),yi(n),h,14) + ase_src_qemhour(n, 7,h)   ! pm10
                if (h==1) narea_othe = narea_othe +1
              case(2)
                ! domestic heating
                ase_src_dome(xi(n),yi(n),h,1 ) = ase_src_dome(xi(n),yi(n),h,1 ) + ase_src_qemhour(n, 8,h)   ! NO
                ase_src_dome(xi(n),yi(n),h,2 ) = ase_src_dome(xi(n),yi(n),h,2 ) + ase_src_qemhour(n, 9,h)   ! NO2
                ase_src_dome(xi(n),yi(n),h,3 ) = ase_src_dome(xi(n),yi(n),h,3 ) + ase_src_qemhour(n, 4,h)   ! SO2
                ase_src_dome(xi(n),yi(n),h,4 ) = ase_src_dome(xi(n),yi(n),h,4 ) + ase_src_qemhour(n, 3,h)   ! CO
                ase_src_dome(xi(n),yi(n),h,5 ) = ase_src_dome(xi(n),yi(n),h,5 ) + ase_src_qemhour(n,12,h)   ! c2h6
                ase_src_dome(xi(n),yi(n),h,6 ) = ase_src_dome(xi(n),yi(n),h,6 ) + ase_src_qemhour(n,11,h)   ! hcho
                ase_src_dome(xi(n),yi(n),h,7 ) = ase_src_dome(xi(n),yi(n),h,7 ) + ase_src_qemhour(n,13,h)   ! ch3cho
                ase_src_dome(xi(n),yi(n),h,8 ) = ase_src_dome(xi(n),yi(n),h,8 ) + ase_src_qemhour(n,15,h)   ! c2h4
                ase_src_dome(xi(n),yi(n),h,9 ) = ase_src_dome(xi(n),yi(n),h,9 ) + ase_src_qemhour(n,14,h)   ! nc4h10
                ase_src_dome(xi(n),yi(n),h,10) = ase_src_dome(xi(n),yi(n),h,10) + ase_src_qemhour(n,18,h)   ! ch3coc2h5
                ase_src_dome(xi(n),yi(n),h,11) = ase_src_dome(xi(n),yi(n),h,11) + ase_src_qemhour(n,16,h)   ! c3h6
                ase_src_dome(xi(n),yi(n),h,12) = ase_src_dome(xi(n),yi(n),h,12) + ase_src_qemhour(n,17,h)   ! oxylen
                ase_src_dome(xi(n),yi(n),h,13) = ase_src_dome(xi(n),yi(n),h,13) + ase_src_qemhour(n, 6,h)   ! pm25
                ase_src_dome(xi(n),yi(n),h,14) = ase_src_dome(xi(n),yi(n),h,14) + ase_src_qemhour(n, 7,h)   ! pm10
                if (h==1) narea_dome = narea_dome +1
              case(3)
                ! Commercial and Industrial sources
                ase_src_othe(xi(n),yi(n),h,1 ) = ase_src_othe(xi(n),yi(n),h,1 ) + ase_src_qemhour(n, 8,h)   ! NO
                ase_src_othe(xi(n),yi(n),h,2 ) = ase_src_othe(xi(n),yi(n),h,2 ) + ase_src_qemhour(n, 9,h)   ! NO2
                ase_src_othe(xi(n),yi(n),h,3 ) = ase_src_othe(xi(n),yi(n),h,3 ) + ase_src_qemhour(n, 4,h)   ! SO2
                ase_src_othe(xi(n),yi(n),h,4 ) = ase_src_othe(xi(n),yi(n),h,4 ) + ase_src_qemhour(n, 3,h)   ! CO
                ase_src_othe(xi(n),yi(n),h,5 ) = ase_src_othe(xi(n),yi(n),h,5 ) + ase_src_qemhour(n,12,h)   ! c2h6
                ase_src_othe(xi(n),yi(n),h,6 ) = ase_src_othe(xi(n),yi(n),h,6 ) + ase_src_qemhour(n,11,h)   ! hcho
                ase_src_othe(xi(n),yi(n),h,7 ) = ase_src_othe(xi(n),yi(n),h,7 ) + ase_src_qemhour(n,13,h)   ! ch3cho
                ase_src_othe(xi(n),yi(n),h,8 ) = ase_src_othe(xi(n),yi(n),h,8 ) + ase_src_qemhour(n,15,h)   ! c2h4
                ase_src_othe(xi(n),yi(n),h,9 ) = ase_src_othe(xi(n),yi(n),h,9 ) + ase_src_qemhour(n,14,h)   ! nc4h10
                ase_src_othe(xi(n),yi(n),h,10) = ase_src_othe(xi(n),yi(n),h,10) + ase_src_qemhour(n,18,h)   ! ch3coc2h5
                ase_src_othe(xi(n),yi(n),h,11) = ase_src_othe(xi(n),yi(n),h,11) + ase_src_qemhour(n,16,h)   ! c3h6
                ase_src_othe(xi(n),yi(n),h,12) = ase_src_othe(xi(n),yi(n),h,12) + ase_src_qemhour(n,17,h)   ! oxylen
                ase_src_othe(xi(n),yi(n),h,13) = ase_src_othe(xi(n),yi(n),h,13) + ase_src_qemhour(n, 6,h)   ! pm25
                ase_src_othe(xi(n),yi(n),h,14) = ase_src_othe(xi(n),yi(n),h,14) + ase_src_qemhour(n, 7,h)   ! pm10
                if (h==1) narea_othe = narea_othe +1
              case(4)
                ! Industry production
                ase_src_othe(xi(n),yi(n),h,1 ) = ase_src_othe(xi(n),yi(n),h,1 ) + ase_src_qemhour(n, 8,h)   ! NO
                ase_src_othe(xi(n),yi(n),h,2 ) = ase_src_othe(xi(n),yi(n),h,2 ) + ase_src_qemhour(n, 9,h)   ! NO2
                ase_src_othe(xi(n),yi(n),h,3 ) = ase_src_othe(xi(n),yi(n),h,3 ) + ase_src_qemhour(n, 4,h)   ! SO2
                ase_src_othe(xi(n),yi(n),h,4 ) = ase_src_othe(xi(n),yi(n),h,4 ) + ase_src_qemhour(n, 3,h)   ! CO
                ase_src_othe(xi(n),yi(n),h,5 ) = ase_src_othe(xi(n),yi(n),h,5 ) + ase_src_qemhour(n,12,h)   ! c2h6
                ase_src_othe(xi(n),yi(n),h,6 ) = ase_src_othe(xi(n),yi(n),h,6 ) + ase_src_qemhour(n,11,h)   ! hcho
                ase_src_othe(xi(n),yi(n),h,7 ) = ase_src_othe(xi(n),yi(n),h,7 ) + ase_src_qemhour(n,13,h)   ! ch3cho
                ase_src_othe(xi(n),yi(n),h,8 ) = ase_src_othe(xi(n),yi(n),h,8 ) + ase_src_qemhour(n,15,h)   ! c2h4
                ase_src_othe(xi(n),yi(n),h,9 ) = ase_src_othe(xi(n),yi(n),h,9 ) + ase_src_qemhour(n,14,h)   ! nc4h10
                ase_src_othe(xi(n),yi(n),h,10) = ase_src_othe(xi(n),yi(n),h,10) + ase_src_qemhour(n,18,h)   ! ch3coc2h5
                ase_src_othe(xi(n),yi(n),h,11) = ase_src_othe(xi(n),yi(n),h,11) + ase_src_qemhour(n,16,h)   ! c3h6
                ase_src_othe(xi(n),yi(n),h,12) = ase_src_othe(xi(n),yi(n),h,12) + ase_src_qemhour(n,17,h)   ! oxylen
                ase_src_othe(xi(n),yi(n),h,13) = ase_src_othe(xi(n),yi(n),h,13) + ase_src_qemhour(n, 6,h)   ! pm25
                ase_src_othe(xi(n),yi(n),h,14) = ase_src_othe(xi(n),yi(n),h,14) + ase_src_qemhour(n, 7,h)   ! pm10
                if (h==1) narea_othe = narea_othe +1
                !print *,'ase_othe ',h,n,ase_src_qemhour(n, 7,h),ase_src_othe(xi(n),yi(n),h,14),xi(n),yi(n)
              case(5)
                ! Distribution of fossil fuels
                ase_src_othe(xi(n),yi(n),h,1 ) = ase_src_othe(xi(n),yi(n),h,1 ) + ase_src_qemhour(n, 8,h)   ! NO
                ase_src_othe(xi(n),yi(n),h,2 ) = ase_src_othe(xi(n),yi(n),h,2 ) + ase_src_qemhour(n, 9,h)   ! NO2
                ase_src_othe(xi(n),yi(n),h,3 ) = ase_src_othe(xi(n),yi(n),h,3 ) + ase_src_qemhour(n, 4,h)   ! SO2
                ase_src_othe(xi(n),yi(n),h,4 ) = ase_src_othe(xi(n),yi(n),h,4 ) + ase_src_qemhour(n, 3,h)   ! CO
                ase_src_othe(xi(n),yi(n),h,5 ) = ase_src_othe(xi(n),yi(n),h,5 ) + ase_src_qemhour(n,12,h)   ! c2h6
                ase_src_othe(xi(n),yi(n),h,6 ) = ase_src_othe(xi(n),yi(n),h,6 ) + ase_src_qemhour(n,11,h)   ! hcho
                ase_src_othe(xi(n),yi(n),h,7 ) = ase_src_othe(xi(n),yi(n),h,7 ) + ase_src_qemhour(n,13,h)   ! ch3cho
                ase_src_othe(xi(n),yi(n),h,8 ) = ase_src_othe(xi(n),yi(n),h,8 ) + ase_src_qemhour(n,15,h)   ! c2h4
                ase_src_othe(xi(n),yi(n),h,9 ) = ase_src_othe(xi(n),yi(n),h,9 ) + ase_src_qemhour(n,14,h)   ! nc4h10
                ase_src_othe(xi(n),yi(n),h,10) = ase_src_othe(xi(n),yi(n),h,10) + ase_src_qemhour(n,18,h)   ! ch3coc2h5
                ase_src_othe(xi(n),yi(n),h,11) = ase_src_othe(xi(n),yi(n),h,11) + ase_src_qemhour(n,16,h)   ! c3h6
                ase_src_othe(xi(n),yi(n),h,12) = ase_src_othe(xi(n),yi(n),h,12) + ase_src_qemhour(n,17,h)   ! oxylen
                ase_src_othe(xi(n),yi(n),h,13) = ase_src_othe(xi(n),yi(n),h,13) + ase_src_qemhour(n, 6,h)   ! pm25
                ase_src_othe(xi(n),yi(n),h,14) = ase_src_othe(xi(n),yi(n),h,14) + ase_src_qemhour(n, 7,h)   ! pm10
                if (h==1) narea_othe = narea_othe +1
                !print *,'5', ase_src_qemhour(n,7,h),ase_src_qemhour(n,11,h),ase_src_qemhour(n,13,h),ase_src_qemhour(n,17,h)
              case(6)
                ! Solvent Use
                ase_src_solv(xi(n),yi(n),h,1 ) = ase_src_solv(xi(n),yi(n),h,1 ) + ase_src_qemhour(n, 8,h)   ! NO
                ase_src_solv(xi(n),yi(n),h,2 ) = ase_src_solv(xi(n),yi(n),h,2 ) + ase_src_qemhour(n, 9,h)   ! NO2
                ase_src_solv(xi(n),yi(n),h,3 ) = ase_src_solv(xi(n),yi(n),h,3 ) + ase_src_qemhour(n, 4,h)   ! SO2
                ase_src_solv(xi(n),yi(n),h,4 ) = ase_src_solv(xi(n),yi(n),h,4 ) + ase_src_qemhour(n, 3,h)   ! CO
                ase_src_solv(xi(n),yi(n),h,5 ) = ase_src_solv(xi(n),yi(n),h,5 ) + ase_src_qemhour(n,12,h)   ! c2h6
                ase_src_solv(xi(n),yi(n),h,6 ) = ase_src_solv(xi(n),yi(n),h,6 ) + ase_src_qemhour(n,11,h)   ! hcho
                ase_src_solv(xi(n),yi(n),h,7 ) = ase_src_solv(xi(n),yi(n),h,7 ) + ase_src_qemhour(n,13,h)   ! ch3cho
                ase_src_solv(xi(n),yi(n),h,8 ) = ase_src_solv(xi(n),yi(n),h,8 ) + ase_src_qemhour(n,15,h)   ! c2h4
                ase_src_solv(xi(n),yi(n),h,9 ) = ase_src_solv(xi(n),yi(n),h,9 ) + ase_src_qemhour(n,14,h)   ! nc4h10
                ase_src_solv(xi(n),yi(n),h,10) = ase_src_solv(xi(n),yi(n),h,10) + ase_src_qemhour(n,18,h)   ! ch3coc2h5
                ase_src_solv(xi(n),yi(n),h,11) = ase_src_solv(xi(n),yi(n),h,11) + ase_src_qemhour(n,16,h)   ! c3h6
                ase_src_solv(xi(n),yi(n),h,12) = ase_src_solv(xi(n),yi(n),h,12) + ase_src_qemhour(n,17,h)   ! oxylen
                ase_src_solv(xi(n),yi(n),h,13) = ase_src_solv(xi(n),yi(n),h,13) + ase_src_qemhour(n, 6,h)   ! pm25
                ase_src_solv(xi(n),yi(n),h,14) = ase_src_solv(xi(n),yi(n),h,14) + ase_src_qemhour(n, 7,h)   ! pm10
                if (h==1) narea_solv = narea_solv +1
              case(7)
                ! Traffic on small roads
                !call stopit('SNAP 7 not allowed for area sources')
                ! now attributed to 'solvents'
                ase_src_solv(xi(n),yi(n),h,1 ) = ase_src_solv(xi(n),yi(n),h,1 ) + ase_src_qemhour(n, 8,h)   ! NO
                ase_src_solv(xi(n),yi(n),h,2 ) = ase_src_solv(xi(n),yi(n),h,2 ) + ase_src_qemhour(n, 9,h)   ! NO2
                ase_src_solv(xi(n),yi(n),h,3 ) = ase_src_solv(xi(n),yi(n),h,3 ) + ase_src_qemhour(n, 4,h)   ! SO2
                ase_src_solv(xi(n),yi(n),h,4 ) = ase_src_solv(xi(n),yi(n),h,4 ) + ase_src_qemhour(n, 3,h)   ! CO
                ase_src_solv(xi(n),yi(n),h,5 ) = ase_src_solv(xi(n),yi(n),h,5 ) + ase_src_qemhour(n,12,h)   ! c2h6
                ase_src_solv(xi(n),yi(n),h,6 ) = ase_src_solv(xi(n),yi(n),h,6 ) + ase_src_qemhour(n,11,h)   ! hcho
                ase_src_solv(xi(n),yi(n),h,7 ) = ase_src_solv(xi(n),yi(n),h,7 ) + ase_src_qemhour(n,13,h)   ! ch3cho
                ase_src_solv(xi(n),yi(n),h,8 ) = ase_src_solv(xi(n),yi(n),h,8 ) + ase_src_qemhour(n,15,h)   ! c2h4
                ase_src_solv(xi(n),yi(n),h,9 ) = ase_src_solv(xi(n),yi(n),h,9 ) + ase_src_qemhour(n,14,h)   ! nc4h10
                ase_src_solv(xi(n),yi(n),h,10) = ase_src_solv(xi(n),yi(n),h,10) + ase_src_qemhour(n,18,h)   ! ch3coc2h5
                ase_src_solv(xi(n),yi(n),h,11) = ase_src_solv(xi(n),yi(n),h,11) + ase_src_qemhour(n,16,h)   ! c3h6
                ase_src_solv(xi(n),yi(n),h,12) = ase_src_solv(xi(n),yi(n),h,12) + ase_src_qemhour(n,17,h)   ! oxylen
                ase_src_solv(xi(n),yi(n),h,13) = ase_src_solv(xi(n),yi(n),h,13) + ase_src_qemhour(n, 6,h)   ! pm25
                ase_src_solv(xi(n),yi(n),h,14) = ase_src_solv(xi(n),yi(n),h,14) + ase_src_qemhour(n, 7,h)   ! pm10
                if (h==1) narea_solv = narea_solv +1
              case(8)
                ! Shipping traffic = ship
                ase_src_ship(xi(n),yi(n),h,1 ) = ase_src_ship(xi(n),yi(n),h,1 ) + ase_src_qemhour(n, 8,h)   ! NO
                ase_src_ship(xi(n),yi(n),h,2 ) = ase_src_ship(xi(n),yi(n),h,2 ) + ase_src_qemhour(n, 9,h)   ! NO2
                ase_src_ship(xi(n),yi(n),h,3 ) = ase_src_ship(xi(n),yi(n),h,3 ) + ase_src_qemhour(n, 4,h)   ! SO2
                ase_src_ship(xi(n),yi(n),h,4 ) = ase_src_ship(xi(n),yi(n),h,4 ) + ase_src_qemhour(n, 3,h)   ! CO
                ase_src_ship(xi(n),yi(n),h,5 ) = ase_src_ship(xi(n),yi(n),h,5 ) + ase_src_qemhour(n,12,h)   ! c2h6
                ase_src_ship(xi(n),yi(n),h,6 ) = ase_src_ship(xi(n),yi(n),h,6 ) + ase_src_qemhour(n,11,h)   ! hcho
                ase_src_ship(xi(n),yi(n),h,7 ) = ase_src_ship(xi(n),yi(n),h,7 ) + ase_src_qemhour(n,13,h)   ! ch3cho
                ase_src_ship(xi(n),yi(n),h,8 ) = ase_src_ship(xi(n),yi(n),h,8 ) + ase_src_qemhour(n,15,h)   ! c2h4
                ase_src_ship(xi(n),yi(n),h,9 ) = ase_src_ship(xi(n),yi(n),h,9 ) + ase_src_qemhour(n,14,h)   ! nc4h10
                ase_src_ship(xi(n),yi(n),h,10) = ase_src_ship(xi(n),yi(n),h,10) + ase_src_qemhour(n,18,h)   ! ch3coc2h5
                ase_src_ship(xi(n),yi(n),h,11) = ase_src_ship(xi(n),yi(n),h,11) + ase_src_qemhour(n,16,h)   ! c3h6
                ase_src_ship(xi(n),yi(n),h,12) = ase_src_ship(xi(n),yi(n),h,12) + ase_src_qemhour(n,17,h)   ! oxylen
                ase_src_ship(xi(n),yi(n),h,13) = ase_src_ship(xi(n),yi(n),h,13) + ase_src_qemhour(n, 6,h)   ! pm25
                ase_src_ship(xi(n),yi(n),h,14) = ase_src_ship(xi(n),yi(n),h,14) + ase_src_qemhour(n, 7,h)   ! pm10
                if (h==1) narea_ship = narea_ship +1
              case(9)
                ! Waste incineration
                call stopit('SNAP 9 not allowed for area sources')
                ase_src_othe(xi(n),yi(n),h,1 ) = ase_src_othe(xi(n),yi(n),h,1 ) + ase_src_qemhour(n, 8,h)   ! NO
                ase_src_othe(xi(n),yi(n),h,2 ) = ase_src_othe(xi(n),yi(n),h,2 ) + ase_src_qemhour(n, 9,h)   ! NO2
                ase_src_othe(xi(n),yi(n),h,3 ) = ase_src_othe(xi(n),yi(n),h,3 ) + ase_src_qemhour(n, 4,h)   ! SO2
                ase_src_othe(xi(n),yi(n),h,4 ) = ase_src_othe(xi(n),yi(n),h,4 ) + ase_src_qemhour(n, 3,h)   ! CO
                ase_src_othe(xi(n),yi(n),h,5 ) = ase_src_othe(xi(n),yi(n),h,5 ) + ase_src_qemhour(n,12,h)   ! c2h6
                ase_src_othe(xi(n),yi(n),h,6 ) = ase_src_othe(xi(n),yi(n),h,6 ) + ase_src_qemhour(n,11,h)   ! hcho
                ase_src_othe(xi(n),yi(n),h,7 ) = ase_src_othe(xi(n),yi(n),h,7 ) + ase_src_qemhour(n,13,h)   ! ch3cho
                ase_src_othe(xi(n),yi(n),h,8 ) = ase_src_othe(xi(n),yi(n),h,8 ) + ase_src_qemhour(n,15,h)   ! c2h4
                ase_src_othe(xi(n),yi(n),h,9 ) = ase_src_othe(xi(n),yi(n),h,9 ) + ase_src_qemhour(n,14,h)   ! nc4h10
                ase_src_othe(xi(n),yi(n),h,10) = ase_src_othe(xi(n),yi(n),h,10) + ase_src_qemhour(n,18,h)   ! ch3coc2h5
                ase_src_othe(xi(n),yi(n),h,11) = ase_src_othe(xi(n),yi(n),h,11) + ase_src_qemhour(n,16,h)   ! c3h6
                ase_src_othe(xi(n),yi(n),h,12) = ase_src_othe(xi(n),yi(n),h,12) + ase_src_qemhour(n,17,h)   ! oxylen
                ase_src_othe(xi(n),yi(n),h,13) = ase_src_othe(xi(n),yi(n),h,13) + ase_src_qemhour(n, 6,h)   ! pm25
                ase_src_othe(xi(n),yi(n),h,14) = ase_src_othe(xi(n),yi(n),h,14) + ase_src_qemhour(n, 7,h)   ! pm10
                if (h==1) narea_othe = narea_othe +1
              case(10)
                ! Agriculture => solvent/traffic
                ase_src_solv(xi(n),yi(n),h,1 ) = ase_src_solv(xi(n),yi(n),h,1 ) + ase_src_qemhour(n, 8,h)   ! NO
                ase_src_solv(xi(n),yi(n),h,2 ) = ase_src_solv(xi(n),yi(n),h,2 ) + ase_src_qemhour(n, 9,h)   ! NO2
                ase_src_solv(xi(n),yi(n),h,3 ) = ase_src_solv(xi(n),yi(n),h,3 ) + ase_src_qemhour(n, 4,h)   ! SO2
                ase_src_solv(xi(n),yi(n),h,4 ) = ase_src_solv(xi(n),yi(n),h,4 ) + ase_src_qemhour(n, 3,h)   ! CO
                ase_src_solv(xi(n),yi(n),h,5 ) = ase_src_solv(xi(n),yi(n),h,5 ) + ase_src_qemhour(n,12,h)   ! c2h6
                ase_src_solv(xi(n),yi(n),h,6 ) = ase_src_solv(xi(n),yi(n),h,6 ) + ase_src_qemhour(n,11,h)   ! hcho
                ase_src_solv(xi(n),yi(n),h,7 ) = ase_src_solv(xi(n),yi(n),h,7 ) + ase_src_qemhour(n,13,h)   ! ch3cho
                ase_src_solv(xi(n),yi(n),h,8 ) = ase_src_solv(xi(n),yi(n),h,8 ) + ase_src_qemhour(n,15,h)   ! c2h4
                ase_src_solv(xi(n),yi(n),h,9 ) = ase_src_solv(xi(n),yi(n),h,9 ) + ase_src_qemhour(n,14,h)   ! nc4h10
                ase_src_solv(xi(n),yi(n),h,10) = ase_src_solv(xi(n),yi(n),h,10) + ase_src_qemhour(n,18,h)   ! ch3coc2h5
                ase_src_solv(xi(n),yi(n),h,11) = ase_src_solv(xi(n),yi(n),h,11) + ase_src_qemhour(n,16,h)   ! c3h6
                ase_src_solv(xi(n),yi(n),h,12) = ase_src_solv(xi(n),yi(n),h,12) + ase_src_qemhour(n,17,h)   ! oxylen
                ase_src_solv(xi(n),yi(n),h,13) = ase_src_solv(xi(n),yi(n),h,13) + ase_src_qemhour(n, 6,h)   ! pm25
                ase_src_solv(xi(n),yi(n),h,14) = ase_src_solv(xi(n),yi(n),h,14) + ase_src_qemhour(n, 7,h)   ! pm10
                if (h==1) narea_solv = narea_solv +1
              case(11)
                ! Natural = other
                ase_src_othe(xi(n),yi(n),h,1 ) = ase_src_othe(xi(n),yi(n),h,1 ) + ase_src_qemhour(n, 8,h)   ! NO
                ase_src_othe(xi(n),yi(n),h,2 ) = ase_src_othe(xi(n),yi(n),h,2 ) + ase_src_qemhour(n, 9,h)   ! NO2
                ase_src_othe(xi(n),yi(n),h,3 ) = ase_src_othe(xi(n),yi(n),h,3 ) + ase_src_qemhour(n, 4,h)   ! SO2
                ase_src_othe(xi(n),yi(n),h,4 ) = ase_src_othe(xi(n),yi(n),h,4 ) + ase_src_qemhour(n, 3,h)   ! CO
                ase_src_othe(xi(n),yi(n),h,5 ) = ase_src_othe(xi(n),yi(n),h,5 ) + ase_src_qemhour(n,12,h)   ! c2h6
                ase_src_othe(xi(n),yi(n),h,6 ) = ase_src_othe(xi(n),yi(n),h,6 ) + ase_src_qemhour(n,11,h)   ! hcho
                ase_src_othe(xi(n),yi(n),h,7 ) = ase_src_othe(xi(n),yi(n),h,7 ) + ase_src_qemhour(n,13,h)   ! ch3cho
                ase_src_othe(xi(n),yi(n),h,8 ) = ase_src_othe(xi(n),yi(n),h,8 ) + ase_src_qemhour(n,15,h)   ! c2h4
                ase_src_othe(xi(n),yi(n),h,9 ) = ase_src_othe(xi(n),yi(n),h,9 ) + ase_src_qemhour(n,14,h)   ! nc4h10
                ase_src_othe(xi(n),yi(n),h,10) = ase_src_othe(xi(n),yi(n),h,10) + ase_src_qemhour(n,18,h)   ! ch3coc2h5
                ase_src_othe(xi(n),yi(n),h,11) = ase_src_othe(xi(n),yi(n),h,11) + ase_src_qemhour(n,16,h)   ! c3h6
                ase_src_othe(xi(n),yi(n),h,12) = ase_src_othe(xi(n),yi(n),h,12) + ase_src_qemhour(n,17,h)   ! oxylen
                ase_src_othe(xi(n),yi(n),h,13) = ase_src_othe(xi(n),yi(n),h,13) + ase_src_qemhour(n, 6,h)   ! pm25
                ase_src_othe(xi(n),yi(n),h,14) = ase_src_othe(xi(n),yi(n),h,14) + ase_src_qemhour(n, 7,h)   ! pm10
                ! BVOC emission (EF*FD)
                ase_src_othe(xi(n),yi(n),h,15) = ase_src_othe(xi(n),yi(n),h,15) + ase_src_qemhour(n,19,h)   ! isoprene
                ase_src_othe(xi(n),yi(n),h,16) = ase_src_othe(xi(n),yi(n),h,16) + ase_src_qemhour(n,20,h)   ! apinene
                ase_src_othe(xi(n),yi(n),h,17) = ase_src_othe(xi(n),yi(n),h,17) + ase_src_qemhour(n,21,h)   ! limonene
                if (h==1) narea_othe = narea_othe +1

              case DEFAULT
                call stopit('Area source has no valid SNAP code')

            end select


           enddo   ! n sources
           
! *** Write Area Source datalines

         !stop

         write(txt1,'(A6,I4)')    '    H:',h
         write(txt2,'(A10)')      '  no_index'

         do i = 1, nccout
            if(narea_dome>0)  call write_2dfield(n_nx,n_ny,txt1,txt2,funit_out_adome_emis(i),EP_fm, ase_src_dome( :,:,h,i ))
            if(narea_solv>0)  call write_2dfield(n_nx,n_ny,txt1,txt2,funit_out_asolv_emis(i),EP_fm, ase_src_solv( :,:,h,i ))
            if(narea_othe>0)  call write_2dfield(n_nx,n_ny,txt1,txt2,funit_out_aothe_emis(i),EP_fm, ase_src_othe( :,:,h,i ))
            if(narea_ship>0)  call write_2dfield(n_nx,n_ny,txt1,txt2,funit_out_aship_emis(i),EP_fm, ase_src_ship( :,:,h,i ))
         enddo

! *** Change date by one hour

         ! print *,hourc,daynn,mony
         hourc = hourc + 1
         if ( hourc == 24 ) then
           write(6,      '(A27,I4)')  'Finished writing for day:  ',daynn
           hourc = 0
           daynn = daynn + 1
         endif
         if ( daynn > monlen(mony) ) then
           daynn = 1
           if (mony==12) then
             mony=1
           else
             mony=mony+1
           endif
         endif

       enddo     ! h hours



! ***  Write number of category sources to LOG
       if (fe_log) then
           write(funit_log,'(1X,A56,I7)')        &
          'domestic heating area sources: ',     narea_dome
           write(funit_log,'(1X,A56,I7)')        &
          'solvents area sources: ', narea_solv
           write(funit_log,'(1X,A56,I7)')        &
          'industrial/commercial/agric. area sources: ',narea_othe
           write(funit_log,'(1X,A56,I7)')        &
          'shipping traffic area sources: ',narea_ship
       endif


! ***  Optional: Create netCDF output of the 2D area emission fields for checking
! *** Set netCDF output files for area emission 2D-fields
! *** The nccout chemical species will be written to the file

       if (NC_out==1) then
         write(6, '(A29)')  'Write ASE to netCDF files ...'
         write(6, '(A29)')  'This can take several minutes'

         fname_nc_adome_emis = trim(fname_outpath)//'/asrc_domestic_'//trim(startdate)//'_'//trim(enddate)//'.nc'
         fname_nc_asolv_emis =  trim(fname_outpath)//'/asrc_solvent_'//trim(startdate)//'_'//trim(enddate)//'.nc'
         fname_nc_aothe_emis =    trim(fname_outpath)//'/asrc_other_'//trim(startdate)//'_'//trim(enddate)//'.nc'
         fname_nc_aship_emis =    trim(fname_outpath)//'/asrc_ships_'//trim(startdate)//'_'//trim(enddate)//'.nc'

! *** 14.06.2017 Define EPSGN code
         epsgn_u = trim('326'//utmzone(1:2))

! *** Create a grid with the x- and y- coordinates of the grid centre-points
         do jx = 1, n_nx
           do jy = 1, n_ny
             axm_i(jy,jx) = sitex0 + (jx-1)*dxout
             axm_j(jy,jx) = sitey0 + (jy-1)*dxout
           enddo
         enddo
         z(1) = 10

         call CreateNCfileGrid(fname_nc_adome_emis,n_nx,n_ny,1,axm_i,axm_j,z,dxout,utmzone,epsgn_u,sitex0,sitey0)
         call CreateNCfileGrid(fname_nc_asolv_emis,n_nx,n_ny,1,axm_i,axm_j,z,dxout,utmzone,epsgn_u,sitex0,sitey0)
         call CreateNCfileGrid(fname_nc_aothe_emis,n_nx,n_ny,1,axm_i,axm_j,z,dxout,utmzone,epsgn_u,sitex0,sitey0)
         call CreateNCfileGrid(fname_nc_aship_emis,n_nx,n_ny,1,axm_i,axm_j,z,dxout,utmzone,epsgn_u,sitex0,sitey0)


! *** Write the netCDF output file of all species,
! *** Area source emissions hourly 2D field [instantaneous, QA()]

         unitname = "g/s"
         validity = 'averaged'
         dopacking = .false.
         dofloat   = .true.
         domirror  = .true.

! *** houd - hour of day   (1-24) h=1 is first hour
! *** dayw - day of week   (1-7 )
! *** mony - month of year (1-12)
         ! hour counter
         hourc = 1
         daymm = daym
         mony  = mnth
         yeai  = year

         ! for now: write only 3 days
         do h = 1, 24*3   !nhours

           ! one time step 
           Nhh_in = 1
           ! current simulation date
           mdate(1,Nhh_in) = yeai
           mdate(2,Nhh_in) = mony
           mdate(3,Nhh_in) = daymm
           mdate(4,Nhh_in) = hourc

           do i = 1, nccout

!07.08.2019: initialize the output fields
               field2D_1(:,:) = 0.0
               field2D_2(:,:) = 0.0
               field2D_3(:,:) = 0.0
               field2D_4(:,:) = 0.0

               namefield = cpnameo(i)
               if (trim(cpnameo(i)).eq."pm2.5") then
                  namefield = "pm25      "
               endif

               do jx = 1,n_nx
                 do jy = 1,n_ny
                     field2D_1(jy,jx) = dble( ase_src_dome(jx,jy,h,i) )
                     field2D_2(jy,jx) = dble( ase_src_solv(jx,jy,h,i) )
                     field2D_3(jy,jx) = dble( ase_src_othe(jx,jy,h,i) )
                     field2D_4(jy,jx) = dble( ase_src_ship(jx,jy,h,i) )
                 enddo
               enddo

               call writeconcfield(fname_nc_adome_emis,namefield,unitname,       &
                                   field2D_1(1:n_ny,1:n_nx), n_ny, n_nx, 1,      &
                                   Nhh_in, mdate, validity, dopacking, dofloat, domirror)
               call writeconcfield(fname_nc_asolv_emis,namefield,unitname,       &
                                   field2D_2(1:n_ny,1:n_nx), n_ny, n_nx, 1,      &
                                   Nhh_in, mdate, validity, dopacking, dofloat, domirror)
               call writeconcfield(fname_nc_aothe_emis,namefield,unitname,       &
                                   field2D_3(1:n_ny,1:n_nx), n_ny, n_nx, 1,      &
                                   Nhh_in, mdate, validity, dopacking, dofloat, domirror)
!07.08.2019: field2D_4 has the ship area emissions
               call writeconcfield(fname_nc_aship_emis,namefield,unitname,       &
                                   field2D_4(1:n_ny,1:n_nx), n_ny, n_nx, 1,      &
                                   Nhh_in, mdate, validity, dopacking, dofloat, domirror)

           enddo  ! i compounds


! *** Change date by one hour
           hourc = hourc + 1
           if (hourc == 25 ) then
              write(6, '(A36,I4)')  'Finished writing nc files for day:  ',daymm
              hourc = 1
              daymm = daymm + 1
! *** Check if new month?
              if ( daymm > NDAY(mony) ) then
                 daymm = 1
                 mony = mony + 1
              endif
              if (mony == 13) then
                mony = 1
                if (hourc==1)  yeai = yeai +1
              endif
           endif

         enddo    ! h hours

       endif
! ***   End Writing netCDF files


! ***   Close files, free memory
       do i = 1, nccout
         close (funit_out_adome_emis(i) )
         close (funit_out_asolv_emis(i) )
         close (funit_out_aothe_emis(i) )
         close (funit_out_aship_emis(i) )
       enddo

! Deallocate
       if (allocated(axm_i))     deallocate(axm_i)
       if (allocated(axm_j))     deallocate(axm_j)
       if (allocated(z))         deallocate(z)

       if (allocated(ase_src_dome) )      deallocate(ase_src_dome )
       if (allocated(ase_src_solv) )      deallocate(ase_src_solv )
       if (allocated(ase_src_othe) )      deallocate(ase_src_othe )
       if (allocated(ase_src_ship) )      deallocate(ase_src_ship )

       if (allocated(field2D_1) )    deallocate(field2D_1 )
       if (allocated(field2D_2) )    deallocate(field2D_2 )
       if (allocated(field2D_3) )    deallocate(field2D_3 )
       if (allocated(field2D_4) )    deallocate(field2D_4 )

      return


 2000 format(I6 ,1X, 8F11.1, I6)

      end subroutine output_citychem_ase
