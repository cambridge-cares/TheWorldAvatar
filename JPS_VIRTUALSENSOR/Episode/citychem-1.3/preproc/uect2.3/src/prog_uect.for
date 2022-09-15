! <prog_uect.for - A component of the City-scale
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

      program prog_uect

!***********************************************************************
!***  This program reads yearly emission totals from point sources,
!***  line sources and gridded area sources and computes the time
!***  variation and emission splits. The output of UECT is hourly emission
!***  data files as input for CityChem and TAPM
!***  Version 2.1:
!***  Sep. 2018   M. Karl   include SNAP11 emissions of isoprene
!***                        and apply temperature and radiation dependence.
!***                        Isoprene emissions have to be provided as
!***                        area sources in units kgC/year
!***  Jun. 2019   M. Karl   special daily profile for snap7 in summer
!***  Jul. 2019   M. Karl   read 100m BVOC EFs and produce area emissions
!***                        of isoprene, apinene, limonene for unity
!***                        gamma factors
!***  Aug. 2019   M. Karl   apply new NOX split for snap7 line sources:
!***                        f_hono_lse = 0.02
!***  Oct. 2019   M. Karl   emission of apinene as 'MTS_T_EF' (T dep.)
!***                        limonene as 'MTS_LT_EF' (T- and light dep.)
!***********************************************************************

!     Declarations of variables by using the MODULES feature:

      use module_uect_io
      use module_uect_emis
      use module_uect_exe
      use module_uect_time

      implicit none

!***********************************************************************

!     Local Declarations:

      integer                :: res1, res2, res3



!***********************************************************************

!     Flow control

!     Get user-supplied meta information
      call get_user_input

      call get_calendar_day
      print *,'day', dayweek,dayyear

!     Get file units
      funit_in_points      = nextun()
      funit_in_lines       = nextun()
      funit_in_area_sector = nextun()

!     Allocate emission arrays
      call allocate_emis_arrs

!     Initialize emission arrays
!     with zeroes
      in_array_pse(:,:)    = 0.0
      in_array_lse(:,:)    = 0.0
      in_array_ase(:,:)    = 0.0
      snap_vec_pse(:)      = 0
      snap_vec_lse(:)      = 0
      snap_vec_ase(:)      = 0

      print *,'UECT after initialization'

!     Emission input

      select case (source)

         case('PSE')
           open(funit_in_points, file=fname_in_points,           access='sequential',form="formatted",iostat=res1)
           call read_csv_file(funit_in_points, colp,n_sopp, in_array_pse,snap_vec_pse)
            !print *,in_array_pse
         case('LSE')
           open(funit_in_lines, file=fname_in_lines,             access='sequential',form="formatted",iostat=res2)
           call read_csv_file(funit_in_lines,  coll,n_soll, in_array_lse,snap_vec_lse)
            !print *,in_array_lse
         case('ASE')
           open(funit_in_area_sector, file=fname_in_area_sector, access='sequential',form="formatted",iostat=res3)
           call read_csv_file(funit_in_area_sector,cola,n_soaa,in_array_ase,snap_vec_ase)
            !print *,in_array_ase
         case('ALL')
           open(funit_in_points, file=fname_in_points,           access='sequential',form="formatted",iostat=res1)
           open(funit_in_lines, file=fname_in_lines,             access='sequential',form="formatted",iostat=res2)
           open(funit_in_area_sector, file=fname_in_area_sector, access='sequential',form="formatted",iostat=res3)
           call read_csv_file(funit_in_points,     colp,n_sopp,in_array_pse,snap_vec_pse)
           call read_csv_file(funit_in_lines,      coll,n_soll,in_array_lse,snap_vec_lse)
           call read_csv_file(funit_in_area_sector,cola,n_soaa,in_array_ase,snap_vec_ase)
         case DEFAULT
           call stopit('Provide source type: "PSE","LSE","ASE" or "ALL"')

      end select

      if (fe_log) then
         if (fe_in_tapm) then
           write(funit_log,*)
           write(funit_log,'(1X,A58)')  'UECT: Air temperature is used to calculate SNAP2 emissions' 
           write(funit_log,*)
         else
           write(funit_log,*)
           write(funit_log,'(1X,A58)')  'UECT: Warning! No air temperature file'
           write(funit_log,'(1X,A58)')  'SNAP2 with standard time variation'
           write(funit_log,*)
         endif
      endif      

      if (fe_log) then
        write(funit_log,*)
        write(funit_log,'(1X,A58)')  'UECT after emission input' 
        write(funit_log,*)
      endif


!     Emission source filter and mask
      select case (source)
         case('PSE')
            !remove point sources outside model domain
            call filter_points(colp,n_sopp,n_hours,snap_vec_pse,in_array_pse,n_sopp2,snap_new_pse,new_array_pse)
            if (n_sopp2==0) then
              call stopit('All point sources are outside of the model domain')
            endif
         case('LSE')
            !remove line sources with start and end outside model domain
            call filter_lines(coll,n_soll,n_hours,snap_vec_lse,in_array_lse,n_soll2,snap_new_lse,new_array_lse)
            if (n_soll2==0) then
              call stopit('All line sources are outside of the model domain')
            endif
         case('ASE')
           !calculate area mask (closest point) and remove areas outside
            call filter_areas(cola,n_soaa,n_hours,snap_vec_ase,in_array_ase,n_soaa2,snap_new_ase,  &
                              xi_ase,yi_ase,new_array_ase)
            if (n_soaa2==0) then
              call stopit('All area sources are outside of the model domain')
            endif
         case('ALL')
            !remove point sources outside model domain
            call filter_points(colp,n_sopp,n_hours,snap_vec_pse,in_array_pse,n_sopp2,snap_new_pse,new_array_pse)
            if (n_sopp2==0) then
              call stopit('All point sources are outside of the model domain')
            endif
            !remove line sources with start and end outside model domain
            call filter_lines(coll,n_soll,n_hours,snap_vec_lse,in_array_lse,n_soll2,snap_new_lse,new_array_lse)
            if (n_soll2==0) then
              call stopit('All line sources are outside of the model domain')
            endif
           !calculate area mask (closest point) and remove areas outside
            call filter_areas(cola,n_soaa,n_hours,snap_vec_ase,in_array_ase,n_soaa2,snap_new_ase,  &
                              xi_ase,yi_ase,new_array_ase)
            if (n_soaa2==0) then
              call stopit('All area sources are outside of the model domain')
            endif
         case DEFAULT

      end select

      if (fe_log) then
        write(funit_log,*)
        write(funit_log,'(1X,A58)')  'UECT after filtering emission input' 
        write(funit_log,*)
      endif


!     Allocate (filtered) output arrays
      select case (source)
         case('PSE')
           if (.not. allocated(out_array_pse))  allocate( out_array_pse(n_sopp2,ncomp,n_hours))
           if (.not. allocated(out_params_pse)) allocate(out_params_pse(n_sopp2,n_pse_params))
           if (.not. allocated(out_snap_pse))   allocate(  out_snap_pse(n_sopp2))
           out_array_pse(:,:,:) = 0.0
           out_params_pse(:,:)  = 0.0
           out_snap_pse(:)      = 0
         case('LSE')
           if (.not. allocated(out_array_lse))  allocate( out_array_lse(n_soll2,ncomp,n_hours))
           if (.not. allocated(out_params_lse)) allocate(out_params_lse(n_soll2,n_lse_params))
           if (.not. allocated(out_snap_lse))   allocate(  out_snap_lse(n_soll2))
           out_array_lse(:,:,:) = 0.0
           out_params_lse(:,:)  = 0.0
           out_snap_lse(:)      = 0
         case('ASE')
           if (.not. allocated(out_array_ase))  allocate( out_array_ase(n_soaa2,ncomp,n_hours))
           if (.not. allocated(out_params_ase)) allocate(out_params_ase(n_soaa2,n_ase_params))
           if (.not. allocated(out_snap_ase))   allocate(  out_snap_ase(n_soaa2))
           out_array_ase(:,:,:) = 0.0
           out_params_ase(:,:)  = 0.0
           out_snap_ase(:)      = 0
         case('ALL')
           if (.not. allocated(out_array_pse))  allocate( out_array_pse(n_sopp2,ncomp,n_hours))
           if (.not. allocated(out_params_pse)) allocate(out_params_pse(n_sopp2,n_pse_params))
           if (.not. allocated(out_snap_pse))   allocate(  out_snap_pse(n_sopp2))
           if (.not. allocated(out_array_lse))  allocate( out_array_lse(n_soll2,ncomp,n_hours))
           if (.not. allocated(out_params_lse)) allocate(out_params_lse(n_soll2,n_lse_params))
           if (.not. allocated(out_snap_lse))   allocate(  out_snap_lse(n_soll2))
           if (.not. allocated(out_array_ase))  allocate( out_array_ase(n_soaa2,ncomp,n_hours))
           if (.not. allocated(out_params_ase)) allocate(out_params_ase(n_soaa2,n_ase_params))
           if (.not. allocated(out_snap_ase))   allocate(  out_snap_ase(n_soaa2))
           out_array_pse(:,:,:) = 0.0
           out_params_pse(:,:)  = 0.0
           out_snap_pse(:)      = 0
           out_array_lse(:,:,:) = 0.0
           out_params_lse(:,:)  = 0.0
           out_snap_lse(:)      = 0
           out_array_ase(:,:,:) = 0.0
           out_params_ase(:,:)  = 0.0
           out_snap_ase(:)      = 0
         case DEFAULT

      end select


!     Emission conversion

      call get_time_table

! LOG        Log warning if hourly CO line emission is > 0.1 g/m/s
! LOG        Log warning if any area NOx is > 3 g/s
! LOG        Log warning if any area total VOC is > 1 g/s

      select case (source)

         case('PSE')
! input: dimension n_sopp / output: dimension n_sopp2
           call emission_points(colp,n_sopp,n_sopp2,n_hours,new_array_pse,snap_new_pse, &
                                out_snap_pse,out_params_pse,out_array_pse)

         case('LSE')
! input: dimension n_soll / output: dimension n_soll2
           call emission_lines(coll,n_soll,n_soll2,n_hours,new_array_lse,snap_new_lse, &
                                out_snap_lse,out_params_lse,out_array_lse)

         case('ASE')
! input: dimension n_soaa / output: dimension n_soaa2
           call emission_areas(cola,n_soaa,n_soaa2,n_hours,new_array_ase,snap_new_ase, &
                                 xi_ase,yi_ase,out_snap_ase,out_params_ase,out_array_ase)
         case('ALL')
           call emission_points(colp,n_sopp,n_sopp2,n_hours,new_array_pse,snap_new_pse, &
                                out_snap_pse,out_params_pse,out_array_pse)
           call emission_lines(coll,n_soll,n_soll2,n_hours,new_array_lse,snap_new_lse, &
                                out_snap_lse,out_params_lse,out_array_lse)
           call emission_areas(cola,n_soaa,n_soaa2,n_hours,new_array_ase,snap_new_ase, &
                                xi_ase,yi_ase,out_snap_ase,out_params_ase,out_array_ase)
         case DEFAULT

           call stopit('Provide source type: "PSE","LSE","ASE" or "ALL"')

      end select

      if (fe_log) then
        write(funit_log,*)
        write(funit_log,'(1X,A58)')  'UECT after emission conversion'
        write(funit_log,*)
        write(funit_log,'(1X,A)') '********************************* '
      endif


!     Emission plausibility test
!     Sum up hourly emission of selected sources
!     and compare to input emission
!     Not done for requested short time periods

      if (n_hours.gt.600) then
        select case (source)

         case('PSE')
           call check_hour_emission(colp,n_hours,n_sopp,n_sopp2,new_array_pse,&
                                    out_snap_pse,out_array_pse)
         case('LSE')
           call check_hour_emission(coll,n_hours,n_soll,n_soll2,new_array_lse,&
                                    out_snap_lse,out_array_lse)
         case('ASE')
           call check_hour_emission(cola,n_hours,n_soaa,n_soaa2,new_array_ase,&
                                    out_snap_ase,out_array_ase)
         case('ALL')
           call check_hour_emission(colp,n_hours,n_sopp,n_sopp2,new_array_pse,&
                                    out_snap_pse,out_array_pse)
           call check_hour_emission(coll,n_hours,n_soll,n_soll2,new_array_lse,&
                                    out_snap_lse,out_array_lse)
           call check_hour_emission(cola,n_hours,n_soaa,n_soaa2,new_array_ase,&
                                    out_snap_ase,out_array_ase)
         case DEFAULT

        end select

        if (fe_log) then
          write(funit_log,'(1X,A)') '********************************* '
          write(funit_log,*)
          write(funit_log,'(1X,A58)')  'UECT after plausibility test'
          write(funit_log,*)
        endif
      endif

 
!     Emission output

     ! switch case on model
     ! CC, TP, xx

      select case (model)

         case('CC')
            if (source == 'PSE') then
              call output_citychem_pse(n_hours,n_sopp2,out_params_pse,out_array_pse)
            else if (source == 'LSE') then
              call output_citychem_lse(n_hours,n_soll2,out_params_lse,out_array_lse)
            else if (source == 'ASE') then
              call output_citychem_ase(n_hours,n_soaa2,out_snap_ase,xi_ase,yi_ase, &
                                       out_params_ase,out_array_ase)

            else if (source == 'ALL') then
              call output_citychem_pse(n_hours,n_sopp2,out_params_pse,out_array_pse)
              call output_citychem_lse(n_hours,n_soll2,out_params_lse,out_array_lse)
              call output_citychem_ase(n_hours,n_soaa2,out_snap_ase,xi_ase,yi_ase, &
                                       out_params_ase,out_array_ase)

            endif
         case('TP')
            if (source == 'PSE') then
              call output_tapm_pse(n_hours,n_sopp2,out_params_pse,out_array_pse)
            else if (source == 'LSE') then
              call output_tapm_lse(n_hours,n_soll2,out_params_lse,out_array_lse)
            else if (source == 'ASE') then
              call output_tapm_ase(n_hours,n_soaa2,out_snap_ase,xi_ase,yi_ase, &
                                       out_params_ase,out_array_ase)
            else if (source == 'ALL') then
              call output_tapm_pse(n_hours,n_sopp2,out_params_pse,out_array_pse)
              call output_tapm_lse(n_hours,n_soll2,out_params_lse,out_array_lse)
              call output_tapm_ase(n_hours,n_soaa2,out_snap_ase,xi_ase,yi_ase, &
                                       out_params_ase,out_array_ase)
              endif
         case DEFAULT
           call stopit('Provide model type: "CC" or "TP"')

      end select


      if (fe_log) then
        write(funit_log,*)
        write(funit_log,'(1X,A58)')  'UECT after emission output'
        write(funit_log,*)
      endif



!     Free memory emission arrays
      !call free_emis_arrs()
      
      select case (source)

         case('PSE')
           if (allocated(out_array_pse))  deallocate(out_array_pse)
           if (allocated(out_params_pse)) deallocate(out_params_pse)
          ! if (allocated(new_array_pse))  deallocate(new_array_pse)
          ! if (allocated(in_array_pse))   deallocate(in_array_pse)
           if (allocated(snap_new_pse))   deallocate(snap_new_pse)
           if (allocated(snap_vec_pse))   deallocate(snap_vec_pse)
         case('LSE')
           if (allocated(out_array_lse))  deallocate(out_array_lse)
           if (allocated(out_params_lse)) deallocate(out_params_lse)
         !  if (allocated(new_array_lse))  deallocate(new_array_lse)
         !  if (allocated(in_array_lse))   deallocate(in_array_lse)
           if (allocated(snap_new_lse))   deallocate(snap_new_lse)
           if (allocated(snap_vec_lse))   deallocate(snap_vec_lse)
         case('ASE')
           if (allocated(out_array_ase))  deallocate(out_array_ase)
           if (allocated(out_params_ase)) deallocate(out_params_ase)
           !if (allocated(new_array_ase))  deallocate(new_array_ase)
           !if (allocated(in_array_ase))   deallocate(in_array_ase)
           if (allocated(snap_new_ase))   deallocate(snap_new_ase)
           if (allocated(snap_vec_ase))   deallocate(snap_vec_ase)
           if (allocated(xi_ase))         deallocate(xi_ase)
           if (allocated(yi_ase))         deallocate(yi_ase)
         case('ALL')
           if (allocated(out_array_pse))  deallocate(out_array_pse)
           if (allocated(out_array_lse))  deallocate(out_array_lse)
           if (allocated(out_array_ase))  deallocate(out_array_ase)
           if (allocated(out_params_pse)) deallocate(out_params_pse)
           if (allocated(out_params_lse)) deallocate(out_params_lse)
           if (allocated(out_params_ase)) deallocate(out_params_ase)
        !   if (allocated(new_array_pse))  deallocate(new_array_pse)
        !   if (allocated(new_array_lse))  deallocate(new_array_lse)
        !   if (allocated(new_array_ase))  deallocate(new_array_ase)
        !   if (allocated(in_array_pse))   deallocate(in_array_pse)
        !   if (allocated(in_array_lse))   deallocate(in_array_lse)
        !   if (allocated(in_array_ase))   deallocate(in_array_ase)
           if (allocated(snap_new_pse))   deallocate(snap_new_pse)
           if (allocated(snap_new_lse))   deallocate(snap_new_lse)
           if (allocated(snap_new_ase))   deallocate(snap_new_ase)
           if (allocated(snap_vec_pse))   deallocate(snap_vec_pse)
           if (allocated(snap_vec_lse))   deallocate(snap_vec_lse)
           if (allocated(snap_vec_ase))   deallocate(snap_vec_ase)
           if (allocated(xi_ase))         deallocate(xi_ase)
           if (allocated(yi_ase))         deallocate(yi_ase)
           
           
       case DEFAULT

      end select
 
!     Close all i/o files

!     call close_all_files

      if (fe_log) then
        write(funit_log,*)
        write(funit_log,'(1X,A58)')  'UECT finished'
        write(funit_log,*)
      endif

!     Close the Log-file
      close(funit_log) 



      end program prog_uect
