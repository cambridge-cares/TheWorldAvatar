! <output_tapm_ase.for - A component of the City-scale
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

      subroutine output_tapm_ase(nhours,nps,ase_snap,xi,yi,     &
                                     ase_src_param,ase_src_qemhour)

!***********************************************************************
!***  Subroutine output_citychem_ase writes output for area sources
!***  in TAPM format:
!***  one file with emission values for all emission categories
!***********************************************************************

!     Declarations of variables by using the MODULES feature:

      use module_uect_io
      use module_uect_exe
      use module_uect_time
      use module_uect_emis
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


      character(len=10)         :: txt1
      character(len=10)         :: txt2
      character (len=256)       :: epsgn_u

! *** Emission: hourly compound emission amount per category, in g/s
      real, dimension(:,:,:,:), allocatable :: ase_src_dome


      real,   allocatable :: axm_i(:,:)
      real,   allocatable :: axm_j(:,:)
      real,   allocatable :: z(:)
      double precision,   allocatable :: field2D_1(:,:)
      double precision,   allocatable :: field2D_2(:,:)
      double precision,   allocatable :: field2D_3(:,:)

      integer                            :: mode
      real, dimension(nps,n_ase_params)  :: ase_src_outparm
      real, dimension(nps)               :: ase_pmratio

!***********************************************************************
!     Content of subroutine:


! ***  ALLOCATE
       if (.not. allocated(ase_src_dome))  allocate( ase_src_dome(n_nx,n_ny,nhours,nccout) )

       if (.not. allocated(axm_i) )        allocate( axm_i(n_nx,n_ny) )
       if (.not. allocated(axm_j) )        allocate( axm_j(n_nx,n_ny) )
       if (.not. allocated(z) )            allocate( z(n_nz) )
       if (.not. allocated(field2D_1) )    allocate( field2D_1(n_ny,n_nx) )
       if (.not. allocated(field2D_2) )    allocate( field2D_2(n_ny,n_nx) )
       if (.not. allocated(field2D_3) )    allocate( field2D_3(n_ny,n_nx) )


! ***   Open the area source output file:
       fname_out_areas      = trim(fname_outpath)//'/areasrc_uect_'//trim(startdate)//'_'//trim(enddate)//'.ase'
       funit_out_areas      = nextun()
       open (funit_out_areas, file = fname_out_areas, status = 'unknown', form  = 'formatted', action = 'write')

! ***   TAPM output files
       cpnameo( 1) = 'apm       '
       cpnameo( 2) = 'nox       '
       cpnameo( 3) = 'so2       '
       cpnameo( 4) = 'rsmog     '

! ***   Write hourly emission values
       ! hour counter
       hourc      = 1
       ! source counter
       narea_dome = 0
       narea_solv = 0
       narea_othe = 0
       !Initialize emission output fields
       ase_src_dome( :,:,:,: ) = 0.0

! ***   Write meta information file for line sources

       ase_src_outparm = ase_src_param

! ***   TAPM specific parameters
       mode = 0     ! use EGM

       write (funit_out_areas, 1990) nps,nhours

       do n = 1, nps

           if (ase_src_qemhour(n,7,1).gt.0.0) then
             ase_pmratio(n) = ase_src_qemhour(n,6,1) / ase_src_qemhour(n,7,1)
             ase_pmratio(n) = min(1.00,ase_pmratio(n))
             else
             ase_pmratio(n) = 0.90     ! default
           endif
          ! write(6,*) 'pmratio n',n,ase_pmratio(n)
           
           write (funit_out_areas, 2000) mode,ase_src_outparm (n,1),ase_src_outparm (n,2), &
                        ase_src_outparm (n,3),ase_src_outparm (n,4),ase_src_outparm (n,5), &
                        ase_src_outparm (n,6),f_no_pse,ase_pmratio(n)

       enddo

! ***   End writing meta data

! ***   Write hourly emission values
       
       do h = 1, nhours
     
           do n = 1, nps
           
             ase_src_dome(xi(n),yi(n),h,1 ) = ase_src_dome(xi(n),yi(n),h,1 ) + ase_src_qemhour(n, 7,h)   ! pm10
             ase_src_dome(xi(n),yi(n),h,2 ) = ase_src_dome(xi(n),yi(n),h,2 ) + ase_src_qemhour(n, 8,h) + &
                                              ase_src_qemhour(n, 9,h)                                       ! NO+NO2
             ase_src_dome(xi(n),yi(n),h,3 ) = ase_src_dome(xi(n),yi(n),h,3 ) + ase_src_qemhour(n, 4,h)      ! SO2
             ase_src_dome(xi(n),yi(n),h,4 ) = ase_src_dome(xi(n),yi(n),h,4 ) + ase_src_qemhour(n,10,h)      ! RSMOG
 
             write (funit_out_areas,'(4E12.4)' ) ase_src_dome(xi(n),yi(n),h,1),ase_src_dome(xi(n),yi(n),h,2), &
                                                 ase_src_dome(xi(n),yi(n),h,3),ase_src_dome(xi(n),yi(n),h,4)

             if (h==1) narea_dome = narea_dome +1
           enddo   ! n sources
           
       enddo     ! h hours



! ***  Write number of category sources to LOG
       if (fe_log) then
           write(funit_log,'(1X,A56,I7)')        &
          'number of all area sources: ',     narea_dome
       endif


! ***  Optional: Create netCDF output of the 2D area emission fields for checking
! *** Set netCDF output files for area emission 2D-fields
! *** The nccout chemical species will be written to the file

       if (NC_out==1) then
         write(6, '(A29)')  'Write ASE to netCDF files ...'
         write(6, '(A29)')  'This can take several minutes'

         fname_nc_adome_emis = trim(fname_outpath)//'/asrc_uect_'//trim(startdate)//'_'//trim(enddate)//'.nc'

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

           do i = 1, nctapm
               namefield = cpnameo(i)

               do jx = 1,n_nx
                 do jy = 1,n_ny
                     field2D_1(jy,jx) = dble( ase_src_dome(jx,jy,h,i) )
                 enddo
               enddo

               call writeconcfield(fname_nc_adome_emis,namefield,unitname,       &
                                   field2D_1(1:n_ny,1:n_nx), n_ny, n_nx, 1,      &
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


! Deallocate
       if (allocated(axm_i))     deallocate(axm_i)
       if (allocated(axm_j))     deallocate(axm_j)
       if (allocated(z))         deallocate(z)

       if (allocated(ase_src_dome) )      deallocate(ase_src_dome )


      return

 1990 format(I8 , I8)
 2000 format(I1 ,1X, 6F11.2, 2F11.4)

      end subroutine output_tapm_ase
