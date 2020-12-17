! <read-tapm-outafile.for  - A component of the City-scale
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
!  PROGRAM: TAPM4CC
!
!  VERSION: 2.0
!
!  PURPOSE:    This program reads model output from TAPM (outa-files)
!              and produces the required gridded meteorology input files for 
!              EPISODE and EPISODE-CityChem and AirQUIS stand alone.
!              New in version 2.0 is the ability to produce meteorology files
!              in netcdf format.
!
! Based on:
!              Original source code of TAPM4CC by Leif Harvard Slørdal (NILU)
!              and Bruce Denby (NILU, MET.NO)
!
! Contact:
!              See License header
!
!*****************************************************************************!
!_lhs_30_March_2011_Start:
!DEC$ FIXEDFORMLINESIZE: 132
!_lhs_30_March_2011_End.

      program tapm4cc


! *** Preprocessor program for reading model output from TAPM (outa-files)
! *** and producing the required gridded Met input files for AirQUIS stand alone.

! *** TAPM *.outa files are produced converting selected *.out and *.rfl files with
! *** the tool TAPM2OUTA.exe
! *** Produce a subset with 25 vertical layers (full layers does not work here)

! *** The present version only allow:
! *** 1) Identical grids (i.e. with EPISODE layer thicknesses that vary as: 20, 10, 40, ...)
! ***    Should involve minimal adjustment by the applied linear interpolation.
! *** 2) Use of the approximate formulae where the EPISODE interfaces are placed centrally 
! ***    between the TAPM values. Involve substantial linear interpolation.
! *** 3) User specified EPISODE-thicknesses. (NOT TESTED YET)
! ***
! *** For all variants we require: 
! *** A) The maximum model depth of EPISODE <= Max model depth of TAPM.
! *** B) The lowermost value of 3D TAPM fields == lowermost 3D EPISODE fields.
! ***    This means that the lowermost EPISODE layer should be 17.5 or 20 m (or close to this value). 

! *** The Output from this program is stored on the folder:  '../input_EPISODE_data/.....'

        use module_cc_input
        use module_writenc

      implicit none

! *** START of SET-UP: ********************************************************************************************************
! *** Deciding how to extract the TAPM data:

!      integer, parameter :: hh = 24*31

      integer, parameter :: EXACT = 2                    ! 1 => "Identical", 2 =>  "Approximate", 3 => User def EPISODE grid.
      logical, parameter :: apply_interpolation = .TRUE.  ! Apply vertical interpolation or not.

! *** Large domain: TAPM Filename and Dimensions:
! HH centred extent
!      character(len=256) :: TAPM_input_filename = './input_TAPM_data/tapm_1km_20130101_20130201.outa'
!      character(len=256) :: TAPM_input_filename = './input_TAPM_data/tapm_1km_20130701_20130801.outa'
! HH DiDoPo extent
!      character(len=256) :: TAPM_input_filename = './input_TAPM_data/1km_innergrid_jan13.outa'
!      character(len=256) :: TAPM_input_filename = './input_TAPM_data/TAPM_Juli2013_1km_HH.outa'
!      integer, parameter :: nx=20, ny=20, nz=30, nz_ep=25, dx=5000, dy=5000

      
! *** Small domain: TAPM Filename and Dimensions: 
!      character(len=256) :: TAPM_input_filename = '../input_TAPM_data/KPIZ_small_All-30_layers_sep2010.outa'
!full
      integer, parameter :: nz=30, nz_ep=24, dx=1000, dy=1000
!subset
!      integer, parameter :: nx=30, ny=30, nz=25, nz_ep=24, dx=1000, dy=1000

! *** Applying TAPM file that contain all levels (here 30):
      real, dimension(nz), parameter :: SIGMA  = (/ 10.,  25.,  50.,  75., 100., 150., 200., 250.,  &
     &                                             300., 350., 400., 450., 500., 600., 750.,       &
     &                                            1000.,1250.,1500.,1750.,2000.,2250.,2500.,3000., &
     &                                            3500.,4000.,4500.,5000.,6000.,7000.,8000./)

! *** Applying TAPM file that contain a subset of vertical levels (here 25):
!     real, dimension(nz), parameter :: SIGMA  = (/ 10.,  25.,  50.,  75., 100., 150., 200., 250.,  &
!    &                                             300., 350., 400., 450., 500., 600., 750.,        &
!    &                                            1000.,1250.,1500.,1750.,2000.,2250.,2500.,3000.,  &
!    &                                            3500.,4000./)

! *** If EXACT = 1 or 2:
      real, dimension(nz_ep) :: dsigma_ep      
! *** If EXACT = 3 the parameter declared EPISODE layer thicknesses below are applied:
!      real, dimension(nz_ep) :: dsigma_ep  = (/ 20.,  20.,  25.,  25.,  30.,  40.,  50.,  50.,  &
!     &                                          50.,  50.,  50.,  50.,  50.,  60., 100., 150.,  &
!     &                                         200., 200., 250., 250., 300., 350., 400., 450., 500./)    


! Default Meteorology output by default Fortran Binary format
! *** ASCII or BINARY:       
!      integer :: EP_fm = 1    ! Ascii  - The extensions below are automatically set to ".asc" (Could also be changed to ".txt" 
!      integer :: EP_fm = 2   ! Binary - The extensions below are automatically set to ".fld"

      logical, parameter :: adjust_topo_to_zero = .FALSE. ! Shifting the topography to zero level minimum?

! *** LOGFILE name:
      character(len=256) :: LOG_filename        = 'Log_tapm4cc.txt'

      character(len=256) :: EPfilename 
! *** END of SET-UP. **********************************************************************************************************
      
! *** EPISODE Filename character variables:

! *** Filename of the 2D time-independent topography heights (m):
      character(len=256) :: EP_2D_topo
! *** Filenames of the 3D time-independent height (m) above ground output files:
      character(len=256) :: EP_3D_TAPM_z_abg
      character(len=256) :: EP_3D_EPISODE_z_abg
! *** Filenames of the 2D time-dependent output:       
      character(len=256) :: EP_2D_tot_solar_rad
      character(len=256) :: EP_2D_net_rad
      character(len=256) :: EP_2D_sens_heatfl
      character(len=256) :: EP_2D_evap_heatfl
      character(len=256) :: EP_2D_ustar
      character(len=256) :: EP_2D_ptstar
      character(len=256) :: EP_2D_pvstar
      character(len=256) :: EP_2D_wstar
      character(len=256) :: EP_2D_hmix
      character(len=256) :: EP_2D_T_screen
      character(len=256) :: EP_2D_RH_screen
      character(len=256) :: EP_2D_T_surf
      character(len=256) :: EP_2D_prec
! *** Filenames of the 3D time-dependent output:
      character(len=256) :: EP_3D_temp
      character(len=256) :: EP_3D_rh
      character(len=256) :: EP_3D_pot_T
      character(len=256) :: EP_3D_tke
      character(len=256) :: EP_3D_wind
! *** Filename of the extra EPISODE tair(i,j,1) and DTDZ(i,j,1) file:
      character(len=256) :: EP_2_2D_T_and_dtdz

! *** Additional declarations:
      integer :: nnx, nny, nnz, ddx, ddy
      integer :: i, j, k, n

      integer            :: name_length
      character(len=18)  :: filedate

      !integer            :: funit_log
      integer            :: un
      integer            :: alternativ
      character(len=10)  :: txt1, txt2
  
      integer :: istop

! *** Variables for netCDF output
      integer             :: Nhh_in
      integer, dimension(4,1) :: mdate
      character(len=10)   :: namefield
      character(len=10)   :: unitname

      character(len=23)   :: validity
      logical             :: dopacking  = .false.
      logical             :: dofloat    = .false.
      logical             :: domirror   = .false.
      character (len=256) :: epsgn_u  

      real    :: mod_h_ep
      real    :: sigma_top_ep(nz_ep)     
      real    :: z_mid(nz_ep), sigma_ep(nz_ep)
      real    :: z_top(nz_ep)
      real    :: MOD_DEPTH
      logical :: extrapolation_applied
      real    :: tapm_min_topo
      real    :: fac

! *** Time looping
      integer, allocatable    :: date(:)
      integer, allocatable    :: hourt(:)

! *** Terrain and grid-height information:
      real, allocatable    ::   zs(:,:)           !! zs:     Smoothed terrain height (m)
      real, allocatable    ::    z(:,:,:)         !! z:      Height above the terrain (m)
      real, allocatable    :: z_ep(:,:,:)         !! z_ep:   Height above terrain in Episode (m)
      
! *** For each hour:
      real, allocatable    ::   tsr(:,:,:)        !! tsr:    Total solar radiation (W/m2)
      real, allocatable    ::   net(:,:,:)        !! net:    Net radiation (W/m2)
      real, allocatable    ::  sens(:,:,:)        !! sens:   Sensible heat flux (W/m2) 
      real, allocatable    ::  evap(:,:,:)        !! evap:   Evaporative heat flux (W(m2)
      real, allocatable    :: ustar(:,:,:)        !! ustar:  Friction velocity scale (m/s)
      real, allocatable    :: ptstar(:,:,:)       !! ptstar: Potential temperature scale (K)      
      real, allocatable    :: pvstar(:,:,:)       !! pvstar: Potential virtual temerature scale (K)
      real, allocatable    :: wstar(:,:,:)        !! wstar:  Convective velocity scale (m/s)
      real, allocatable    ::  hmix(:,:,:)        !! hmix:   Mixing height (m)
      real, allocatable    ::  tscr(:,:,:)        !! tscr:   Screen-level temperature (K)
      real, allocatable    :: rhscr(:,:,:)        !! rhscr:  Screen-level relative humidity (%)
      real, allocatable    :: tsurf(:,:,:)        !! tsurf:  Surface temperature (K)
      real, allocatable    ::  rain(:,:,:)        !! rain:   Rainfall (mm/hr)

      real, allocatable    ::  dtdz(:,:,:)        !! dtdz:   DT/DZ based on the temp-diff between layer 1 and 2

! *** For each hour,each level:      
      real, allocatable    ::    ws(:,:,:,:)
      real, allocatable    :: ws_ep(:,:,:)        !! ws:     Horizontal wind speed (m/s)
      real, allocatable    ::    wd(:,:,:,:)
      real, allocatable    :: wd_ep(:,:,:)        !! wd:     Horizonal wind direction (degree)
      real, allocatable    ::    ww(:,:,:,:)
      real, allocatable    :: ww_ep(:,:,:)        !! ww:     Vertical velocity (m/s)
      real, allocatable    ::    tt(:,:,:,:)
      real, allocatable    :: tt_ep(:,:,:)        !! tt:     Temperature (K)
      real, allocatable    ::    rh(:,:,:,:)
      real, allocatable    :: rh_ep(:,:,:)        !! rh:     Relative humidity (%)
      real, allocatable    ::    pt(:,:,:,:)
      real, allocatable    :: pt_ep(:,:,:)        !! pt:     Potential temperature (K)
      real, allocatable    ::   tke(:,:,:,:)
      real, allocatable    :: tke_ep(:,:,:)       !! tke:    Turbulence kinetic energy (m*2/s*2)
      real, allocatable    ::    uu(:,:,:,:)
      real, allocatable    :: uu_ep(:,:,:)        !! uu:     West-east   wind speed (m/s)
      real, allocatable    ::    vv(:,:,:,:)
      real, allocatable    :: vv_ep(:,:,:)        !! vv:     South-north wind speed (m/s)  

! *** Helper variables for netCDF output
      real, allocatable    :: znc(:)
      real, allocatable    :: axm_i(:,:)
      real, allocatable    :: axm_j(:,:)
      double precision, allocatable :: field2D(:,:)
      double precision, allocatable :: field3D(:,:,:)
      
! *** START of program: *******************************************************************************************************


!     Get user-supplied meta information
      call get_user_input      


! *** Allocation of variables
      if (.not. allocated(date))    allocate( date(hh) )
      if (.not. allocated(hourt))   allocate( hourt(hh) )

      if (.not. allocated(zs))      allocate( zs(nx, ny) )
      if (.not. allocated(z))       allocate( z(nx, ny, nz) )
      if (.not. allocated(z_ep))    allocate( z_ep(nx,ny,nz_ep) )
      
! *** For each hour:
      if (.not. allocated(tsr))     allocate( tsr(nx, ny, hh) )
      if (.not. allocated(net))     allocate( net(nx, ny, hh) )
      if (.not. allocated(sens))    allocate( sens(nx, ny, hh) )
      if (.not. allocated(evap))    allocate( evap(nx, ny, hh) )
      if (.not. allocated(ustar))   allocate( ustar(nx, ny, hh) )
      if (.not. allocated(ptstar))  allocate( ptstar(nx, ny, hh) )
      if (.not. allocated(pvstar))  allocate( pvstar(nx, ny, hh) )
      if (.not. allocated(wstar))   allocate( wstar(nx, ny, hh) )
      if (.not. allocated(hmix))    allocate( hmix(nx, ny, hh) )
      if (.not. allocated(tscr))    allocate( tscr(nx, ny, hh) )
      if (.not. allocated(rhscr))   allocate( rhscr(nx, ny, hh) )
      if (.not. allocated(tsurf))   allocate( tsurf(nx, ny, hh) )
      if (.not. allocated(rain))    allocate( rain(nx, ny, hh) )
      if (.not. allocated(dtdz))    allocate( dtdz(nx, ny, hh) )   
      
! *** For each hour,each level:
      if (.not. allocated(ws))      allocate( ws(nx,ny,nz,hh) )
      if (.not. allocated(ws_ep))   allocate( ws_ep(nx,ny,nz_ep) )
      if (.not. allocated(wd))      allocate( wd(nx,ny,nz,hh) )
      if (.not. allocated(wd_ep))   allocate( wd_ep(nx,ny,nz_ep) )
      if (.not. allocated(ww))      allocate( ww(nx,ny,nz,hh) )
      if (.not. allocated(ww_ep))   allocate( ww_ep(nx,ny,nz_ep) )
      if (.not. allocated(tt))      allocate( tt(nx,ny,nz,hh) )
      if (.not. allocated(tt_ep))   allocate( tt_ep(nx,ny,nz_ep) )
      if (.not. allocated(rh))      allocate( rh(nx,ny,nz,hh) )
      if (.not. allocated(rh_ep))   allocate( rh_ep(nx,ny,nz_ep) )
      if (.not. allocated(pt))      allocate( pt(nx,ny,nz,hh) )
      if (.not. allocated(pt_ep))   allocate( pt_ep(nx,ny,nz_ep) )
      if (.not. allocated(tke))     allocate( tke(nx,ny,nz,hh) )
      if (.not. allocated(tke_ep))  allocate( tke_ep(nx,ny,nz_ep) )
    
      if (.not. allocated(uu))      allocate( uu(nx,ny,nz,hh) )
      if (.not. allocated(uu_ep))   allocate( uu_ep(nx,ny,nz_ep) )
      if (.not. allocated(vv))      allocate( vv(nx,ny,nz,hh) )
      if (.not. allocated(vv_ep))   allocate( vv_ep(nx,ny,nz_ep) )

! *** Helper variables for netCDF output
      if (.not. allocated(axm_i) )  allocate( axm_i(nx,ny) )
      if (.not. allocated(axm_j) )  allocate( axm_j(nx,ny) )
      if (.not. allocated(znc) )    allocate( znc(nz_ep) )
      if (.not. allocated(field2D)) allocate( field2D(ny,nx) )
      if (.not. allocated(field3D)) allocate( field3D(ny,nx,nz_ep) )

! ****************************************************************************************************

! *** Open a log-file:
!      funit_log = 11
!      open (funit_log, file = LOG_filename, status = 'unknown', form  = 'formatted', action = 'write')

      
      write(funit_log,'(A)') '********************************************************************************'
      write(funit_log,*)
      write(funit_log,'(A)') 'LOG-FILE from a run of the program: Tapm4CityChem'
      write(funit_log,*)
      write(funit_log,'(A)') '********************************************************************************'
      write(funit_log,*)
      write(funit_log,'(A)')      'This program reads Meteorological parameters from a TAPM outa-file,'
      write(funit_log,'(A,I4,A)') 'and produces: hh = ',hh,' hourly Met-input fields for CityChem.'
      write(funit_log,*)
      if (EXACT == 1) then
        write(funit_log,'(A)')    'Identical vertical grids are applied between TAPM and CityChem.'
      elseif (EXACT == 2) then
         write(funit_log,'(A)')   'Approximate vertical grids are applied between TAPM and CityChem.'
      elseif (EXACT == 3) then
         write(funit_log,'(A)')   'User defined CityChem layer thicknesses applied.'
         !write(funit_log,'(A)')   'Not tested yet. Program terminates.'
         !STOP
      else
         write(funit_log,'(A)')   'Wrong value of EXACT applied. Program terminates.'
         STOP
      endif
      write(funit_log,*)
      if (apply_interpolation) then
        write(funit_log,'(A)')    'Linear interpolation applied in the vertical.'
      else
        write(funit_log,'(A)')    'No interpolation applied in the vertical.'
      endif
      write(funit_log,*)
      write(funit_log,*)
      write(funit_log,'(A,I3,A)') &
     &  'TAPM: The ',nz,          &
     &  ' sigma-heights (m above ground) where the TAPM variables are given:'
      write(funit_log,*)
      do k = 1,nz
        write(funit_log,'(A,I2.2,A,F8.2)') 'TAPM: SIGMA(',k,') = ',SIGMA(k)
      enddo
      write(funit_log,*)

      if (EP_fm /= 2) then
        write(funit_log,'(A)')    'The CityChem output files are produced as standard EPISODE ASCII files.'
      else
        write(funit_log,'(A)')    'The CityChem output files are produced as standard EPISODE BINARY files.'
      endif       
      write(funit_log,*)

! ****************************************************************************************************

      if (EXACT == 1) then

! ***   ALTERNATIVE 1:
! ***   With identical "layer" thicknesses in TAPM and EPISODE:
!       dsigma_ep = (/20.,10.,40.,10.,40.,60.,40.,60.,40.,60.,40.,60.,40.,160.,140.,360.,140.,360.,140.,360./)

        sigma_top_ep(1) = 2.0 * SIGMA(1)
        dsigma_ep(1)    = sigma_top_ep(1)
        do k = 2,nz_ep
          sigma_top_ep(k) = sigma_top_ep(k-1) + (2.0 * ( SIGMA(k) - sigma_top_ep(k-1) ) )
          dsigma_ep(k)    = sigma_top_ep(k) - sigma_top_ep(k-1)
        enddo
        
      elseif (EXACT == 2) then

! ***   ALTERNATIVE 2:
! ***   Defining the top of each EPISODE layer midway between the heights of the TAPM values:
        if (nz_ep < nz) then
          do k = 1,nz_ep
            sigma_top_ep(k) = (SIGMA(k) + SIGMA(k+1))/2.0
          enddo        
        elseif (nz_ep == nz) then      
          do k = 1,nz_ep - 1
            sigma_top_ep(k) = (SIGMA(k) + SIGMA(k+1))/2.0
          enddo
          sigma_top_ep(nz_ep) =   (2 * sigma_top_ep(nz_ep-1)) - sigma_top_ep(nz_ep-2) 
        endif
        
        dsigma_ep(1) = sigma_top_ep(1)
        do k = 2,nz_ep
          dsigma_ep(k) = sigma_top_ep(k) - sigma_top_ep(k-1)
        enddo
        
      elseif (EXACT == 3) then
! ***   ALTERNATIVE 3:
! ***   User defined EPISODE layer thicknesses: NOT TESTED YET.

! ***   See start of program for the setting of the array: "dsigma_ep(1:nz_ep)"
      
      endif

      mod_h_ep  = SUM(dsigma_ep)

      write(funit_log,*)
      write(funit_log,*)
      write(funit_log,'(A,I3,A)') &
     &  'EPISODE: The thickness of the ',nz_ep,' sigma-layers (m) of EPISODE:'
      write(funit_log,*)
      do k = 1,nz_ep
        write(funit_log,'(A,I2.2,A,F8.2)') 'EPISODE: dsigma_ep(',k,') = ',dsigma_ep(k)
      enddo
      write(funit_log,*)
      write(funit_log,'(A,F8.1,A)') &
     &  'EPISODE: Giving a maximum total model depth: mod_eh_ep = ',mod_h_ep,' m.'     
      write(funit_log,*)

 
! *** Open the TAPM *.outa file:
      open (funit_inpath_tapm, file = fname_inpath_tapm, form   = 'formatted', access = 'sequential', action = 'read')

      name_length = LEN_TRIM(fname_inpath_tapm)
      write(funit_log,'(2A)') &
     &  'TAPM: Filename of the applied TAPM *.outa file = ',fname_inpath_tapm(1:name_length)     
      write(funit_log,*)

      filedate = '_'//trim(startdate)//'_'//trim(enddate)
      print *,'filedate: ',filedate

      select case (EP_fm)

       case (1)
! ***   Giving filename extension indicating ASCII-files (".txt" or ".asc"):

! ***   Filename of the 2D time-independent topography heights (m):
        EP_2D_topo          = trim(fname_outpath)//'/topo'//trim(filedate)//'.asc'
! ***   Filenames of the 3D time-independent height (m) above ground output files:
        EP_3D_TAPM_z_abg    = trim(fname_outpath)//'/3D_TAPM_z_abg'//trim(filedate)//'.asc'
        EP_3D_EPISODE_z_abg = trim(fname_outpath)//'/3D_EPISODE_z_abg'//trim(filedate)//'.asc'
! ***   Filenames of the 2D time-dependent output:       
        EP_2D_tot_solar_rad = trim(fname_outpath)//'/tot_solar_rad'//trim(filedate)//'.asc'
        EP_2D_net_rad       = trim(fname_outpath)//'/net_rad'//trim(filedate)//'.asc'
        EP_2D_sens_heatfl   = trim(fname_outpath)//'/sens_heatfl'//trim(filedate)//'.asc'
        EP_2D_evap_heatfl   = trim(fname_outpath)//'/evap_heatfl'//trim(filedate)//'.asc'
        EP_2D_ustar         = trim(fname_outpath)//'/ustar'//trim(filedate)//'.asc'
        EP_2D_ptstar        = trim(fname_outpath)//'/ptstar'//trim(filedate)//'.asc'
        EP_2D_pvstar        = trim(fname_outpath)//'/pvstar'//trim(filedate)//'.asc'
        EP_2D_wstar         = trim(fname_outpath)//'/wstar'//trim(filedate)//'.asc'
        EP_2D_hmix          = trim(fname_outpath)//'/hmix'//trim(filedate)//'.asc'
        EP_2D_T_screen      = trim(fname_outpath)//'/T_screen'//trim(filedate)//'.asc'
        EP_2D_RH_screen     = trim(fname_outpath)//'/RH_screen'//trim(filedate)//'.asc'
        EP_2D_T_surf        = trim(fname_outpath)//'/T_surf'//trim(filedate)//'.asc'
        EP_2D_prec          = trim(fname_outpath)//'/prec'//trim(filedate)//'.asc'
! ***   Filenames of the 3D time-dependent output:
        EP_3D_temp          = trim(fname_outpath)//'/temp'//trim(filedate)//'.asc'
!RH_3D_not_needed        EP_3D_rh            = trim(fname_outpath)//'/rh'//trim(filedate)//'.asc'
        EP_3D_pot_T         = trim(fname_outpath)//'/pot_T'//trim(filedate)//'.asc'
        EP_3D_tke           = trim(fname_outpath)//'/tke'//trim(filedate)//'.asc'
        EP_3D_wind          = trim(fname_outpath)//'/wind'//trim(filedate)//'.asc'
! ***   Filename of the extra EPISODE tair(i,j,1) and DTDZ(i,j,1) file:
        EP_2_2D_T_and_dtdz  = trim(fname_outpath)//'/T_and_dtdz'//trim(filedate)//'.asc'

      case (2)
! ***   Giving filename extension indicating BINARY output files (".fld"):

! ***   Filename of the 2D time-independent topography heights (m):
        EP_2D_topo          = trim(fname_outpath)//'/topo'//trim(filedate)//'.fld'
! ***   Filenames of the 3D time-independent height (m) above ground output files:
        EP_3D_TAPM_z_abg    = trim(fname_outpath)//'/3D_TAPM_z_abg'//trim(filedate)//'.fld'
        EP_3D_EPISODE_z_abg = trim(fname_outpath)//'/3D_EPISODE_z_abg'//trim(filedate)//'.fld'
! ***   Filenames of the 2D time-dependent output:       write_2dfield
        EP_2D_tot_solar_rad = trim(fname_outpath)//'/tot_solar_rad'//trim(filedate)//'.fld'
        EP_2D_net_rad       = trim(fname_outpath)//'/net_rad'//trim(filedate)//'.fld'
        EP_2D_sens_heatfl   = trim(fname_outpath)//'/sens_heatfl'//trim(filedate)//'.fld'
        EP_2D_evap_heatfl   = trim(fname_outpath)//'/evap_heatfl'//trim(filedate)//'.fld'
        EP_2D_ustar         = trim(fname_outpath)//'/ustar'//trim(filedate)//'.fld'
        EP_2D_ptstar        = trim(fname_outpath)//'/ptstar'//trim(filedate)//'.fld'
        EP_2D_pvstar        = trim(fname_outpath)//'/pvstar'//trim(filedate)//'.fld'
        EP_2D_wstar         = trim(fname_outpath)//'/wstar'//trim(filedate)//'.fld'
        EP_2D_hmix          = trim(fname_outpath)//'/hmix'//trim(filedate)//'.fld'
        EP_2D_T_screen      = trim(fname_outpath)//'/T_screen'//trim(filedate)//'.fld'
        EP_2D_RH_screen     = trim(fname_outpath)//'/RH_screen'//trim(filedate)//'.fld'
        EP_2D_T_surf        = trim(fname_outpath)//'/T_surf'//trim(filedate)//'.fld'
        EP_2D_prec          = trim(fname_outpath)//'/prec'//trim(filedate)//'.fld'
! ***   Filenames of the 3D time-dependent output:
        EP_3D_temp          = trim(fname_outpath)//'/temp'//trim(filedate)//'.fld'
!RH_3D_not_needed        EP_3D_rh            = trim(fname_outpath)//'/rh'//trim(filedate)//'.fld'
        EP_3D_pot_T         = trim(fname_outpath)//'/pot_T'//trim(filedate)//'.fld'
        EP_3D_tke           = trim(fname_outpath)//'/tke'//trim(filedate)//'.fld'
        EP_3D_wind          = trim(fname_outpath)//'/wind'//trim(filedate)//'.fld'
! ***   Filename of the extra EPISODE tair(i,j,1) and DTDZ(i,j,1) file:
        EP_2_2D_T_and_dtdz  = trim(fname_outpath)//'/T_and_dtdz'//trim(filedate)//'.fld'

      case (3)
! ***   Giving filename extension indicating netCDF output files (".nc"):

! ***   Filename of the 2D time-independent topography heights (m):
        EP_2D_topo          = trim(fname_outpath)//'/topo'//trim(filedate)//'.nc'
! ***   Filenames of the 3D time-independent height (m) above ground output files:
        EP_3D_TAPM_z_abg    = trim(fname_outpath)//'/3D_TAPM_z_abg'//trim(filedate)//'.nc'
        EP_3D_EPISODE_z_abg = trim(fname_outpath)//'/3D_EPISODE_z_abg'//trim(filedate)//'.nc'
! ***   Filenames of the 2D time-dependent output:       
        EP_2D_tot_solar_rad = trim(fname_outpath)//'/tot_solar_rad'//trim(filedate)//'.nc'
        EP_2D_net_rad       = trim(fname_outpath)//'/net_rad'//trim(filedate)//'.nc'
        EP_2D_sens_heatfl   = trim(fname_outpath)//'/sens_heatfl'//trim(filedate)//'.nc'
        EP_2D_evap_heatfl   = trim(fname_outpath)//'/evap_heatfl'//trim(filedate)//'.nc'
        EP_2D_ustar         = trim(fname_outpath)//'/ustar'//trim(filedate)//'.nc'
        EP_2D_ptstar        = trim(fname_outpath)//'/ptstar'//trim(filedate)//'.nc'
        EP_2D_pvstar        = trim(fname_outpath)//'/pvstar'//trim(filedate)//'.nc'
        EP_2D_wstar         = trim(fname_outpath)//'/wstar'//trim(filedate)//'.nc'
        EP_2D_hmix          = trim(fname_outpath)//'/hmix'//trim(filedate)//'.nc'
        EP_2D_T_screen      = trim(fname_outpath)//'/T_screen'//trim(filedate)//'.nc'
        EP_2D_RH_screen     = trim(fname_outpath)//'/RH_screen'//trim(filedate)//'.nc'
        EP_2D_T_surf        = trim(fname_outpath)//'/T_surf'//trim(filedate)//'.nc'
        EP_2D_prec          = trim(fname_outpath)//'/prec'//trim(filedate)//'.nc'
! ***   Filenames of the 3D time-dependent output:
        EP_3D_temp          = trim(fname_outpath)//'/temp'//trim(filedate)//'.nc'
!RH_3D_not_needed        EP_3D_rh            = trim(fname_outpath)//'/rh'//trim(filedate)//'.fld'
        EP_3D_pot_T         = trim(fname_outpath)//'/pot_T'//trim(filedate)//'.nc'
        EP_3D_tke           = trim(fname_outpath)//'/tke'//trim(filedate)//'.nc'
        EP_3D_wind          = trim(fname_outpath)//'/wind'//trim(filedate)//'.nc'
! ***   Filename of the extra EPISODE tair(i,j,1) and DTDZ(i,j,1) file:
        EP_2_2D_T_and_dtdz  = trim(fname_outpath)//'/T_and_dtdz'//trim(filedate)//'.nc'
      
      case default
        call stopit('Possible output choice: 1, 2 or 3')
      end select


      select case (EP_fm)

       case (1)
! ***   OPEN the OUTPUT FILES as ASCII-Files:
      
! ***   Open the 2D time-independent output files:
        open (21, file = EP_2D_topo,             status = 'unknown', form  = 'formatted', action = 'write')

! ***   Open the 3D time-independent height (m) above ground output files:
        open (31, file = EP_3D_TAPM_z_abg,    status = 'unknown', form  = 'formatted', action = 'write')
        open (32, file = EP_3D_EPISODE_z_abg, status = 'unknown', form  = 'formatted', action = 'write')

! ***   Open the 2D time-dependent output files:
        open (41, file = EP_2D_tot_solar_rad, status = 'unknown', form = 'formatted', action = 'write')
        open (42, file = EP_2D_net_rad,       status = 'unknown', form = 'formatted', action = 'write')
        open (43, file = EP_2D_sens_heatfl,   status = 'unknown', form = 'formatted', action = 'write')
        open (44, file = EP_2D_evap_heatfl,   status = 'unknown', form = 'formatted', action = 'write')
        open (45, file = EP_2D_ustar,         status = 'unknown', form = 'formatted', action = 'write')
        open (46, file = EP_2D_ptstar,        status = 'unknown', form = 'formatted', action = 'write')
        open (47, file = EP_2D_pvstar,        status = 'unknown', form = 'formatted', action = 'write') 
        open (48, file = EP_2D_wstar,         status = 'unknown', form = 'formatted', action = 'write')
        open (49, file = EP_2D_hmix,          status = 'unknown', form = 'formatted', action = 'write')
        open (50, file = EP_2D_T_screen,      status = 'unknown', form = 'formatted', action = 'write')
        open (51, file = EP_2D_RH_screen,     status = 'unknown', form = 'formatted', action = 'write')
        open (52, file = EP_2D_T_surf,        status = 'unknown', form = 'formatted', action = 'write')
        open (53, file = EP_2D_prec,          status = 'unknown', form = 'formatted', action = 'write')
      
! ***   Open the 3D time-dependent output files:
        open (61, file = EP_3D_temp,          status = 'unknown',   form   = 'formatted',  action = 'write')
!RH_3D_not_needed        open (62, file = EP_3D_rh,            status = 'unknown',   form   = 'formatted',  action = 'write')
        open (63, file = EP_3D_pot_T,         status = 'unknown',   form   = 'formatted',  action = 'write')
        open (64, file = EP_3D_tke,           status = 'unknown',   form   = 'formatted',  action = 'write')

        open (71, file = EP_3D_wind,          status = 'unknown',   form   = 'formatted',  action = 'write')
      
! ***   Special Episode time-dependent output files:      
        open (81, file = EP_2_2D_T_and_dtdz,  status = 'unknown',   form   = 'formatted',  action = 'write')

      case (2)
      
! ***   OPEN the OUTPUT FILES as BINARY-Files:
      
! ***   Open the 2D time-independent output files:
        open (21, file = EP_2D_topo,             status = 'unknown', form  = 'unformatted', action = 'write')

! ***   Open the 3D time-independent height (m) above ground output files:
        open (31, file = EP_3D_TAPM_z_abg,    status = 'unknown', form  = 'unformatted', action = 'write')
        open (32, file = EP_3D_EPISODE_z_abg, status = 'unknown', form  = 'unformatted', action = 'write')

! ***   Open the 2D time-dependent output files:
        open (41, file = EP_2D_tot_solar_rad, status = 'unknown', form = 'unformatted', action = 'write')
        open (42, file = EP_2D_net_rad,       status = 'unknown', form = 'unformatted', action = 'write')
        open (43, file = EP_2D_sens_heatfl,   status = 'unknown', form = 'unformatted', action = 'write')
        open (44, file = EP_2D_evap_heatfl,   status = 'unknown', form = 'unformatted', action = 'write')
        open (45, file = EP_2D_ustar,         status = 'unknown', form = 'unformatted', action = 'write')
        open (46, file = EP_2D_ptstar,        status = 'unknown', form = 'unformatted', action = 'write')
        open (47, file = EP_2D_pvstar,        status = 'unknown', form = 'unformatted', action = 'write') 
        open (48, file = EP_2D_wstar,         status = 'unknown', form = 'unformatted', action = 'write')
        open (49, file = EP_2D_hmix,          status = 'unknown', form = 'unformatted', action = 'write')
        open (50, file = EP_2D_T_screen,      status = 'unknown', form = 'unformatted', action = 'write')
        open (51, file = EP_2D_RH_screen,     status = 'unknown', form = 'unformatted', action = 'write')
        open (52, file = EP_2D_T_surf,        status = 'unknown', form = 'unformatted', action = 'write')
        open (53, file = EP_2D_prec,          status = 'unknown', form = 'unformatted', action = 'write')
! ***   Open the 3D time-dependent output files:
        open (61, file = EP_3D_temp,          status = 'unknown',   form   = 'unformatted',  action = 'write')
!RH_3D_not_needed        open (62, file = EP_3D_rh,            status = 'unknown',   form   = 'unformatted',  action = 'write')
        open (63, file = EP_3D_pot_T,         status = 'unknown',   form   = 'unformatted',  action = 'write')
        open (64, file = EP_3D_tke,           status = 'unknown',   form   = 'unformatted',  action = 'write')
        open (71, file = EP_3D_wind,          status = 'unknown',   form   = 'unformatted',  action = 'write')
      
! ***   Special Episode time-dependent output files:      
        open (81, file = EP_2_2D_T_and_dtdz,  status = 'unknown',   form   = 'unformatted',  action = 'write')      


      case (3)
        
! *** Create a grid with the x- and y- coordinates of the grid centre-points
         do i = 1, nx
           do j = 1, ny
             axm_i(j,i) = sitex0 + (i-1)*dxout
             axm_j(j,i) = sitey0 + (j-1)*dxout
           enddo
         enddo
         
         znc(1) = 10
         epsgn_u = trim('326'//utmzone(1:2))

         call CreateNCfileGrid(EP_2D_topo,nx,ny,1,axm_i,axm_j,znc,dxout,utmzone,epsgn_u,sitex0,sitey0)
         call CreateNCfileGrid(EP_2D_tot_solar_rad,nx,ny,1,axm_i,axm_j,znc,dxout,utmzone,epsgn_u,sitex0,sitey0)
         call CreateNCfileGrid(EP_2D_net_rad,nx,ny,1,axm_i,axm_j,znc,dxout,utmzone,epsgn_u,sitex0,sitey0)
         call CreateNCfileGrid(EP_2D_sens_heatfl,nx,ny,1,axm_i,axm_j,znc,dxout,utmzone,epsgn_u,sitex0,sitey0)
         call CreateNCfileGrid(EP_2D_evap_heatfl,nx,ny,1,axm_i,axm_j,znc,dxout,utmzone,epsgn_u,sitex0,sitey0)
         call CreateNCfileGrid(EP_2D_ustar,nx,ny,1,axm_i,axm_j,znc,dxout,utmzone,epsgn_u,sitex0,sitey0)
         call CreateNCfileGrid(EP_2D_ptstar,nx,ny,1,axm_i,axm_j,znc,dxout,utmzone,epsgn_u,sitex0,sitey0)
         call CreateNCfileGrid(EP_2D_pvstar,nx,ny,1,axm_i,axm_j,znc,dxout,utmzone,epsgn_u,sitex0,sitey0)
         call CreateNCfileGrid(EP_2D_wstar,nx,ny,1,axm_i,axm_j,znc,dxout,utmzone,epsgn_u,sitex0,sitey0)
         call CreateNCfileGrid(EP_2D_hmix,nx,ny,1,axm_i,axm_j,znc,dxout,utmzone,epsgn_u,sitex0,sitey0)
         call CreateNCfileGrid(EP_2D_T_screen,nx,ny,1,axm_i,axm_j,znc,dxout,utmzone,epsgn_u,sitex0,sitey0)
         call CreateNCfileGrid(EP_2D_RH_screen,nx,ny,1,axm_i,axm_j,znc,dxout,utmzone,epsgn_u,sitex0,sitey0)
         call CreateNCfileGrid(EP_2D_T_surf,nx,ny,1,axm_i,axm_j,znc,dxout,utmzone,epsgn_u,sitex0,sitey0)
         call CreateNCfileGrid(EP_2D_prec,nx,ny,1,axm_i,axm_j,znc,dxout,utmzone,epsgn_u,sitex0,sitey0)
         call CreateNCfileGrid(EP_2_2D_T_and_dtdz,nx,ny,1,axm_i,axm_j,znc,dxout,utmzone,epsgn_u,sitex0,sitey0)

         validity = 'averaged'
         dopacking = .false.
         dofloat   = .true.
         domirror  = .true.

      case default
        call stopit('Possible output choice: 1, 2 or 3')
      end select


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!READ1: nx, ny, nz, dx, dy
!READ2: ((zs(i,j), I=1, nx), j=1,ny)
!READ2: (((z(i,j,k),i=1,nx), j=1,ny),k=1,nz) 

      read (funit_inpath_tapm, fmt='(10i8)') nnx, nny, nnz, ddx, ddy
      read (funit_inpath_tapm,fmt='(10f8.2)') ((zs(i,j), i=1, nx), j=1,ny)

      read (funit_inpath_tapm, fmt='(10f8.2)') (((z(i,j,k), i=1,nx), j= 1,ny), k=1,nz)

      write(6,'(4(A,I3),2(A,I5))')  &
        &  'nx  = ',nx, ' ny  = ',ny, ' nz  = ',nz, ' nz_ep  = ',nz_ep, ' dx  = ',dx, ' dy  = ',dy
      write(6,'(3(A,I3),2(A,I5))')  &
        &  'nnx = ',nnx,' nny = ',nny,' nnz = ',nz_ep,' ddx = ',ddx,' ddy = ',ddy

      write(funit_log,*)
      write(funit_log,'(A)') 'The user specified dimensions defined initially in this program: '
      write(funit_log,'(4(A,I3),2(A,I5))')  &
        &  'nx  = ',nx, ' ny  = ',ny, ' nz_tapm  = ',nz, ' nz_ep  = ',nz_ep, ' dx  = ',dx, ' dy  = ',dy
      write(funit_log,*)
      write(funit_log,'(A)') 'The dimensions actually read from the defined TAPM outa-file: '
      write(funit_log,'(3(A,I3),2(A,I5))')  &
        &  'nnx = ',nnx,' nny = ',nny,' nnz = ',nnz,' ddx = ',ddx,' ddy = ',ddy
      write(funit_log,*)

! *** Write out the 2D-topography; zs(nx,ny)
      print *,'write topo EP',EP_fm
      un    = 21
!      EP_fm =  1
      txt1  = 'Topography'
      txt2  = '_from_TAPM'
      EPfilename = EP_2D_topo
      namefield  = 'TOPO'
      unitname   = 'm'
      call write_2dfield(nx,ny,txt1,txt2,un,EP_fm,EPfilename,namefield,unitname,n,zs)
      write(funit_log,'(A18,2A10,2I3)')  'Finished writing: ',txt1,txt2,nx,ny
      write(6,      '(A18,2A10,2I3)')  'Finished writing: ',txt1,txt2,nx,ny
      write(funit_log,*)


! *** Should the topography start at 0.0?????
      if (adjust_topo_to_zero) then
        tapm_min_topo = MINVAL(zs)
        if (tapm_min_topo /= 0.0) then
          zs = zs - tapm_min_topo
        endif
        write(funit_log,'(A)') &
       &  'The TAPM topography height has been shifted to zero level minimum.'
        write(funit_log,'(A)') &
       &  'i.e., the variable "adjust_topo_to_zero" = .TRUE. ' 
        write(funit_log,*)             
      else
        write(funit_log,'(A)') &
       &  'The TAPM topography height has not been shifted to zero level minimum.'       
        write(funit_log,'(A)') &
       &  'i.e., the variable "adjust_topo_to_zero" = .FALSE. ' 
        write(funit_log,*)
      endif

! *** Investigating the TAPM grid, assuming that the TAPM sigma-coordinate transform are
! *** identical in the TAPM and the EPISODE model, and that MOD_H_TAPM = 8000 m, always. 

          
! *** The TAPM (and EPISODE) topography, h(x,y), is stored in:              zs(nx,ny)
! *** The midpoint height above ground of the TAPM grid-cells is stored in:  z(nx,ny,nz)

! *** Constructing the height (m) above ground of the midpoints of the EPISODE grid
! *** where the 3D EPISODE grid values are to be interpolated:     
      do i = 1,nx
        do j = 1,ny
          MOD_DEPTH = mod_h_ep - zs(i,j)
          fac = MOD_DEPTH / mod_h_ep
          z_mid(1) = zs(i,j) + ( fac * ( 0.5 * dsigma_ep(1) ) )
          z_top(1)    = zs(i,j) + ( fac * dsigma_ep(1) )          
          z_ep(i,j,1) = z_mid(1) - zs(i,j)
          do k = 2,nz_ep
            z_mid(k) = z_top(k-1) + ( fac * ( 0.5 * dsigma_ep(k) ) )
            z_top(k) = z_top(k-1) + ( fac * dsigma_ep(k) )
            z_ep(i,j,k) = z_mid(k) - zs(i,j)
          enddo
          if (z_ep(i,j,nz_ep) > z(i,j,nz)) then
            write(funit_log,'(A)')  'WARNING: TAPM values are extrapolated upwards.'
            write(funit_log,'(A)')  'WARNING: The EPISODE model height should be decreased.'
          endif
        enddo
      enddo
      write(funit_log,*)

! ***  Now Create the 3-D netCDF files
! ***  3D fields with CityChem vertical dimension
      if (EP_fm == 3) then

         call CreateNCfileGrid(EP_3D_TAPM_z_abg,nx,ny,nz_ep,axm_i,axm_j,SIGMA,dxout,utmzone,epsgn_u,sitex0,sitey0)
         call CreateNCfileGrid(EP_3D_EPISODE_z_abg,nx,ny,nz_ep,axm_i,axm_j,sigma_top_ep,dxout,utmzone,epsgn_u,sitex0,sitey0)
         call CreateNCfileGrid(EP_3D_temp,nx,ny,nz_ep,axm_i,axm_j,sigma_top_ep,dxout,utmzone,epsgn_u,sitex0,sitey0)
         call CreateNCfileGrid(EP_3D_pot_T,nx,ny,nz_ep,axm_i,axm_j,sigma_top_ep,dxout,utmzone,epsgn_u,sitex0,sitey0)
         call CreateNCfileGrid(EP_3D_tke,nx,ny,nz_ep,axm_i,axm_j,sigma_top_ep,dxout,utmzone,epsgn_u,sitex0,sitey0)
         call CreateNCfileGrid(EP_3D_wind,nx,ny,nz_ep,axm_i,axm_j,sigma_top_ep,dxout,utmzone,epsgn_u,sitex0,sitey0)

      endif
      
! *** Write out the TAPM 3D-grid heights above the topography; z(nx,ny,nz)
!     File is not used by CityChem
      un    = 31
!      EP_fm =  1
      txt1  = 'TAPM_h_abg'
      EPfilename =  EP_3D_TAPM_z_abg
      namefield  = 'Z'
      unitname   = 'm'
      if (EP_fm == 3) then
        call write_3dfield(nx,ny,txt1,txt2,un,EP_fm,EPfilename,namefield,unitname,n,z(:,:,:))
      else
        do k = 1,nz
          write(txt2,'(A6,I3.3)') '_Lay_k=',k
          call write_2dfield(nx,ny,txt1,txt2,un,EP_fm,EPfilename,namefield,unitname,n,z(:,:,k))   
        enddo
      endif
      write(funit_log,'(A18,2A10,3I3)')  'Finished writing: ',txt1,txt2,nx,ny,nz
      write(6,      '(A18,2A10,3I3)')  'Finished writing: ',txt1,txt2,nx,ny,nz

! *** Write out the EPISODE 3D-grid heights above the topography; z_ep(nx,ny,nz_ep)
      un    = 32
!      EP_fm =  1
      txt1  = 'EPIS_h_abg'
      EPfilename =  EP_3D_EPISODE_z_abg
      namefield  = 'Z'
      unitname   = 'm'
      if (EP_fm == 3) then
        call write_3dfield(nx,ny,txt1,txt2,un,EP_fm,EPfilename,namefield,unitname,n,z_ep(:,:,:))
      else
        do k = 1,nz_ep
          write(txt2,'(A6,I3.3)') '_Lay_k=',k
          call write_2dfield(nx,ny,txt1,txt2,un,EP_fm,EPfilename,namefield,unitname,n,z_ep(:,:,k))   
        enddo
      endif
      write(funit_log,'(A18,2A10,3I3)')  'Finished writing: ',txt1,txt2,nx,ny,nz_ep
      write(6,      '(A18,2A10,3I3)')  'Finished writing: ',txt1,txt2,nx,ny,nz_ep
      write(funit_log,*)

      
! *** Going through the time-loop:
      do n = 1,hh

         read (funit_inpath_tapm, fmt='(10i8)')   date(n), hourt (n)    
         write(funit_log,'(A,I8,A,I4)')  &
        &  'TAPM: Reading the input file for "date" = ',date(n),' and "hour" = ',hourt(n)
 
! ***    Reading 2D-variables:
         read (funit_inpath_tapm, fmt='(10f8.2)') ((tsr(i,j,n),i=1,nx),j=1,ny)
         if (n == 1) write(funit_log,'(A)')  'TAPM: Reading 2D-variable "tsr".'

         read (funit_inpath_tapm, fmt='(10f8.2)') ((net(i,j,n),i=1,nx),j=1,ny)
         if (n == 1) write(funit_log,'(A)')  'TAPM: Reading 2D-variable "net".'

         read (funit_inpath_tapm, fmt='(10f8.2)') ((sens(i,j,n),i=1,nx),j=1,ny)
         if (n == 1) write(funit_log,'(A)')  'TAPM: Reading 2D-variable "sens".'
         
         read (funit_inpath_tapm, fmt='(10f8.2)') ((evap(i,j,n),i=1,nx),j=1,ny)
         if (n == 1) write(funit_log,'(A)')  'TAPM: Reading 2D-variable "evap".'         

         read (funit_inpath_tapm, fmt='(10f8.2)') ((ustar(i,j,n),i=1,nx),j=1,ny)
         if (n == 1) write(funit_log,'(A)')  'TAPM: Reading 2D-variable "ustar".'

         read (funit_inpath_tapm, fmt='(10f8.2)') ((pvstar(i,j,n),i=1,nx),j=1,ny)
         if (n == 1) write(funit_log,'(A)')  'TAPM: Reading 2D-variable "pvstar".'
         
         read (funit_inpath_tapm, fmt='(10f8.2)') ((ptstar(i,j,n),i=1,nx),j=1,ny)
         if (n == 1) write(funit_log,'(A)')  'TAPM: Reading 2D-variable "ptstar".'
         
         read (funit_inpath_tapm, fmt='(10f8.2)') ((wstar(i,j,n),i=1,nx),j=1,ny)
         if (n == 1) write(funit_log,'(A)')  'TAPM: Reading 2D-variable "wstar".'
         
         read (funit_inpath_tapm, fmt='(10f8.2)') ((hmix(i,j,n),i=1,nx),j=1,ny)
         if (n == 1) write(funit_log,'(A)')  'TAPM: Reading 2D-variable "hmix".'
         
         read (funit_inpath_tapm, fmt='(10f8.2)') ((tscr(i,j,n),i=1,nx),j=1,ny)
         if (n == 1) write(funit_log,'(A)')  'TAPM: Reading 2D-variable "tscr".'
         
         read (funit_inpath_tapm, fmt='(10f8.2)') ((rhscr(i,j,n),i=1,nx),j=1,ny)
         if (n == 1) write(funit_log,'(A)')  'TAPM: Reading 2D-variable "rhscr".'
         
         read (funit_inpath_tapm, fmt='(10f8.2)') ((tsurf(i,j,n),i=1,nx),j=1,ny)
         if (n == 1) write(funit_log,'(A)')  'TAPM: Reading 2D-variable "tsurf".'
         
         read (funit_inpath_tapm, fmt='(10f8.2)') ((rain(i,j,n),i=1,nx),j=1,ny)
         if (n == 1) write(funit_log,'(A)')  'TAPM: Reading 2D-variable "rain".'

! ***    Reading 3D-variables:

         read (funit_inpath_tapm, fmt='(10f8.2)') (((ws(i,j,k,n),i=1,nx),j=1,ny),k=1,nz)
         if (n == 1) write(funit_log,'(A)')  'TAPM: Reading 3D-variable "ws".'
         
         read (funit_inpath_tapm, fmt='(10f8.2)') (((wd(i,j,k,n),i=1,nx),j=1,ny),k=1,nz)
         if (n == 1) write(funit_log,'(A)')  'TAPM: Reading 3D-variable "wd".'
         
         read (funit_inpath_tapm, fmt='(10f8.2)') (((ww(i,j,k,n),i=1,nx),j=1,ny),k=1,nz)
         if (n == 1) write(funit_log,'(A)')  'TAPM: Reading 3D-variable "ww".'
         
         read (funit_inpath_tapm, fmt='(10f8.2)') (((tt(i,j,k,n),i=1,nx),j=1,ny),k=1,nz)
         if (n == 1) write(funit_log,'(A)')  'TAPM: Reading 3D-variable "tt".'
         
         read (funit_inpath_tapm, fmt='(10f8.2)') (((rh(i,j,k,n),i=1,nx),j=1,ny),k=1,nz)
         if (n == 1) write(funit_log,'(A)')  'TAPM: Reading 3D-variable "rh".'
         
         read (funit_inpath_tapm, fmt='(10f8.2)') (((pt(i,j,k,n),i=1,nx),j=1,ny),k=1,nz)
         if (n == 1) write(funit_log,'(A)')  'TEP_2D_wstarAPM: Reading 3D-variable "pt".'
         
         read (funit_inpath_tapm, fmt='(10f8.2)') (((tke(i,j,k,n),i=1,nx),j=1,ny),k=1,nz)
         if (n == 1) write(funit_log,'(A)')  'TAPM: Reading 3D-variable "tke".'

         do k=1,nz
           do j=1,ny
             do i=1,nx
               call comp_vel(ws(i,j,k,n),wd(i,j,k,n),uu(i,j,k,n),vv(i,j,k,n))
             enddo
           enddo
         enddo
      enddo  ! do n = 1,hh

! *** Write out the time-dependent TAPM-data  in the EPISODE format:

      write(funit_log,*)
      write(funit_log,'(A)')  'TAPM: Finished reading the TAPM outa-file. '     
      write(funit_log,*)
      
      if (apply_interpolation) then
        write(funit_log,'(A)')  &
       &  'EPISODE: Interpolate vertically and write out the following fields to the EPISODE ascii files:'      
      else
        write(funit_log,'(A)')  &
       &  'EPISODE: Writing out the following fields to the EPISODE ascii files: (without any interpolation)'
      endif
      write(funit_log,*)
      
! *** Write 2D-fields to file:

! *** Write 2D total solar radiation; tsr(nx, ny, hh); (W/m2)
      un    = 41
!      EP_fm =  1
      write(txt1,'(A10)') 'Tot_solrad'
      EPfilename = EP_2D_tot_solar_rad
      namefield  = 'TSR'
      unitname   = 'W/m^2'
      do n = 1,hh
        write(txt2,'(A7,I3.3)') '_hour=_',n
        call write_2dfield(nx,ny,txt1,txt2,un,EP_fm,EPfilename,namefield,unitname,n,tsr(:,:,n))
      enddo
      write(funit_log,'(A18,2A10,2I3)')  'Finished writing: ',txt1,txt2,nx,ny
      write(6,      '(A18,2A10,2I3)')  'Finished writing: ',txt1,txt2,nx,ny
      
! *** Write 2D net radiation; net(nx, ny, hh); (W/m2)
      un    = 42
!      EP_fm =  1
      write(txt1,'(A10)') 'Net_radiat'
      EPfilename = EP_2D_net_rad
      namefield  = 'NET'
      unitname   = 'W/m^2'
      do n = 1,hh
        write(txt2,'(A7,I3.3)') '_hour=_',n
        call write_2dfield(nx,ny,txt1,txt2,un,EP_fm,EPfilename,namefield,unitname,n,net(:,:,n))
      enddo
      write(funit_log,'(A18,2A10,2I3)')  'Finished writing: ',txt1,txt2,nx,ny
      write(6,      '(A18,2A10,2I3)')  'Finished writing: ',txt1,txt2,nx,ny

! *** Write 2D surface sensible heat flux; sens(nx, ny, hh); (W/m2)
      un    = 43
!      EP_fm =  1
      write(txt1,'(A10)') 'Sens_hflux'
      EPfilename = EP_2D_sens_heatfl
      namefield  = 'SENS'
      unitname   = 'W/m^2'
      do n = 1,hh
        write(txt2,'(A7,I3.3)') '_hour=_',n
        call write_2dfield(nx,ny,txt1,txt2,un,EP_fm,EPfilename,namefield,unitname,n,sens(:,:,n))
      enddo
      write(funit_log,'(A18,2A10,2I3)')  'Finished writing: ',txt1,txt2,nx,ny
      write(6,      '(A18,2A10,2I3)')  'Finished writing: ',txt1,txt2,nx,ny

! *** Write 2D surface evaporative heat flux; evap(nx, ny, hh); (W/m2)
      un    = 44
!      EP_fm =  1
      write(txt1,'(A10)') 'Evap_hflux'
      EPfilename = EP_2D_evap_heatfl
      namefield  = 'EVAP'
      unitname   = 'W/m^2'
      do n = 1,hh
        write(txt2,'(A7,I3.3)') '_hour=_',n
        call write_2dfield(nx,ny,txt1,txt2,un,EP_fm,EPfilename,namefield,unitname,n,evap(:,:,n))
      enddo
      write(funit_log,'(A18,2A10,2I3)')  'Finished writing: ',txt1,txt2,nx,ny
      write(6,      '(A18,2A10,2I3)')  'Finished writing: ',txt1,txt2,nx,ny

! *** Write 2D surface friction velocity; ustar(nx, ny, hh); (m/s)EP_2D_evap_heatfl
      un    = 45
!      EP_fm =  1
      write(txt1,'(A10)') 'Fric_veloc'
      EPfilename =  EP_2D_ustar
      namefield  = 'USTAR'
      unitname   = 'm/s'
      do n = 1,hh
        write(txt2,'(A7,I3.3)') '_hour=_',n
        call write_2dfield(nx,ny,txt1,txt2,un,EP_fm,EPfilename,namefield,unitname,n,ustar(:,:,n))
      enddo
      write(funit_log,'(A18,2A10,2I3)')  'Finished writing: ',txt1,txt2,nx,ny
      write(6,      '(A18,2A10,2I3)')  'Finished writing: ',txt1,txt2,nx,ny

      ! *** Write 2D surface potential temperature scale; ptstar(nx, ny, hh); (K)
      un    = 46
!      EP_fm =  1
      write(txt1,'(A10)') 'potT_scale'
      EPfilename =  EP_2D_ptstar
      namefield  = 'PTSTAR'
      unitname   = 'K'
      do n = 1,hh
        write(txt2,'(A7,I3.3)') '_hour=_',n
        call write_2dfield(nx,ny,txt1,txt2,un,EP_fm,EPfilename,namefield,unitname,n,ptstar(:,:,n))
      enddo
      write(funit_log,'(A18,2A10,2I3)')  'Finished writing: ',txt1,txt2,nx,ny
      write(6,      '(A18,2A10,2I3)')  'Finished writing: ',txt1,txt2,nx,ny

! *** Write 2D surface potential virtual temperature scale; pvstar(nx, ny, hh); (K)
      un    = 47
!      EP_fm =  1
      EPfilename =  EP_2D_pvstar
      namefield  = 'PVSTAR'
      unitname   = 'K'
      write(txt1,'(A10)') 'potTv_scal'
      do n = 1,hh
        write(txt2,'(A7,I3.3)') '_hour=_',n
        call write_2dfield(nx,ny,txt1,txt2,un,EP_fm,EPfilename,namefield,unitname,n,pvstar(:,:,n))
      enddo
      write(funit_log,'(A18,2A10,2I3)')  'Finished writing: ',txt1,txt2,nx,ny
      write(6,      '(A18,2A10,2I3)')  'Finished writing: ',txt1,txt2,nx,ny

! *** Write 2D convective velocity scale; wstar(nx, ny, hh); (m/s)
      un    = 48
!      EP_fm =  1
      write(txt1,'(A10)') 'Conv_velsc'
      EPfilename =  EP_2D_wstar
      namefield  = 'WSTAR'
      unitname   = 'm/s'
      do n = 1,hh
        write(txt2,'(A7,I3.3)') '_hour=_',n
        call write_2dfield(nx,ny,txt1,txt2,un,EP_fm,EPfilename,namefield,unitname,n,wstar(:,:,n))
      enddo
      write(funit_log,'(A18,2A10,2I3)')  'Finished writing: ',txt1,txt2,nx,ny
      write(6,      '(A18,2A10,2I3)')  'Finished writing: ',txt1,txt2,nx,ny      

! *** Write 2D mixing height; hmix(nx, ny, hh); (m)
      un    = 49
!      EP_fm =  1
      write(txt1,'(A10)') 'Hmix______'
      EPfilename =  EP_2D_hmix
      namefield  = 'HMIX'
      unitname   = 'm'
      do n = 1,hh
        write(txt2,'(A7,I3.3)') '_hour=_',n
        call write_2dfield(nx,ny,txt1,txt2,un,EP_fm,EPfilename,namefield,unitname,n,hmix(:,:,n))
      enddo
      write(funit_log,'(A18,2A10,2I3)')  'Finished writing: ',txt1,txt2,nx,ny
      write(6,      '(A18,2A10,2I3)')  'Finished writing: ',txt1,txt2,nx,ny

! *** Write 2D surface screen temperature; ustar(nx, ny, hh); (K)
      un    = 50
!      EP_fm =  1
      write(txt1,'(A10)') 'Screen_T__'
      EPfilename =  EP_2D_T_screen
      namefield  = 'TSCR'
      unitname   = 'K'
      do n = 1,hh
        write(txt2,'(A7,I3.3)') '_hour=_',n
        call write_2dfield(nx,ny,txt1,txt2,un,EP_fm,EPfilename,namefield,unitname,n,tscr(:,:,n))
     enddo
      write(funit_log,'(A18,2A10,2I3)')  'Finished writing: ',txt1,txt2,nx,ny
      write(6,      '(A18,2A10,2I3)')  'Finished writing: ',txt1,txt2,nx,ny

! *** Write 2D surface screen relative humidity; rhscr(nx, ny, hh); (%)
      un    = 51
!      EP_fm =  1
      write(txt1,'(A10)') 'Screen_RH_'
      EPfilename =  EP_2D_RH_screen
      namefield  = 'RHSCR'
      unitname   = ' '
      do n = 1,hh
        write(txt2,'(A7,I3.3)') '_hour=_',n
        call write_2dfield(nx,ny,txt1,txt2,un,EP_fm,EPfilename,namefield,unitname,n,rhscr(:,:,n)*0.01)
      enddo
      write(funit_log,'(A18,2A10,2I3)')  'Finished writing: ',txt1,txt2,nx,ny
      write(6,      '(A18,2A10,2I3)')  'Finished writing: ',txt1,txt2,nx,ny
      
! *** Write 2D surface temperature; tsurf(nx, ny, hh); (K)
      un    = 52
!      EP_fm =  1
      write(txt1,'(A10)') 'surf_Temp_'
      EPfilename =  EP_2D_T_surf
      namefield  = 'TSURF'
      unitname   = 'K'
      do n = 1,hh
        write(txt2,'(A7,I3.3)') '_hour=_',n
        call write_2dfield(nx,ny,txt1,txt2,un,EP_fm,EPfilename,namefield,unitname,n,tsurf(:,:,n))
      enddo
      write(funit_log,'(A18,2A10,2I3)')  'Finished writing: ',txt1,txt2,nx,ny
      write(6,      '(A18,2A10,2I3)')  'Finished writing: ',txt1,txt2,nx,ny

! *** Write 2D precipitation; rain(nx, ny, hh); (mm/hr)
      un    = 53
!      EP_fm =  1
      write(txt1,'(A10)') 'Precip____'
      EPfilename =  EP_2D_prec
      namefield  = 'RAIN'
      unitname   = 'mm/h'
      do n = 1,hh
        write(txt2,'(A7,I3.3)') '_hour=_',n
        call write_2dfield(nx,ny,txt1,txt2,un,EP_fm,EPfilename,namefield,unitname,n,rain(:,:,n))
      enddo
      write(funit_log,'(A18,2A10,2I3)')  'Finished writing: ',txt1,txt2,nx,ny
      write(6,      '(A18,2A10,2I3)')  'Finished writing: ',txt1,txt2,nx,ny
      
! *** Treating the 3D fields:
      
! *** Write 3D Temperature to file;  tt(nx, ny, nz, hh); (K)     
      un    = 61
!      EP_fm =  1
      EPfilename =  EP_3D_temp
      namefield  = 'TT'
      unitname   = 'K'
      do n = 1,hh
! ***   Interpolate the 3D Temperature field:      
        if (apply_interpolation) then
          call lin_int(nx,ny,nz,nz_ep,z,tt(:,:,:,n),z_ep,tt_ep,extrapolation_applied)
        else
          do i = 1,nx
            do j = 1,ny
              do k = 1, nz_ep
                tt_ep(i,j,k) = tt(i,j,k,n)
              enddo
            enddo
          enddo
        endif
        write(txt1,'(A6,I4.4)') 'Tmp_h=',n
        if (EP_fm == 3) then
          call write_3dfield(nx,ny,txt1,txt2,un,EP_fm,EPfilename,namefield,unitname,n,tt_ep(:,:,:))
        else
          do k = 1,nz_ep
            write(txt2,'(A7,I3.3)') '_Lay_k=',k
            call write_2dfield(nx,ny,txt1,txt2,un,EP_fm,EPfilename,namefield,unitname,n,tt_ep(:,:,k))
          enddo
        endif
      enddo
      write(funit_log,'(A18,2A10,3I3)')  'Finished writing: ',txt1,txt2,nx,ny,nz_ep
      write(6      ,'(A18,2A10,3I3)')  'Finished writing: ',txt1,txt2,nx,ny,nz_ep

!RH_3D_not_needed
!RH_3D_not_needed ! *** Write 3D Relative humidity to file;  rh(nx, ny, nz, hh);  (%)
!RH_3D_not_needed      un    = 62
!RH_3D_not_needed!      EP_fm =  1
!RH_3D_not_needed      do n = 1,hh
!RH_3D_not_needed ! ***   Interpolate the 3D Relative humidity field:
!RH_3D_not_needed        if (apply_interpolation) then
!RH_3D_not_needed          call lin_int(nx,ny,nz,nz_ep,z,rh(:,:,:,n),z_ep,rh_ep,extrapolation_applied)
!RH_3D_not_needed        else
!RH_3D_not_needed          do i = 1,nx
!RH_3D_not_needed            do j = 1,ny
!RH_3D_not_needed              do k = 1, nz_ep
!RH_3D_not_needed                rh_ep(i,j,k) = rh(i,j,k,n)
!RH_3D_not_needed              enddo
!RH_3D_not_needed            enddo
!RH_3D_not_needed          enddo
!RH_3D_not_needed        endif
!RH_3D_not_needed        write(txt1,'(A6,I4.4)') 'RH__h=',n
!RH_3D_not_needed        do k = 1,nz_ep
!RH_3D_not_needed          write(txt2,'(A7,I3.3)') '_Lay_k=',k
!RH_3D_not_needed          call write_2dfield(nx,ny,txt1,txt2,un,EP_fm,rh_ep(:,:,k))
!RH_3D_not_needed        enddo
!RH_3D_not_needed      enddo
!RH_3D_not_needed      write(funit_log,'(A18,2A10,3I3)')  'Finished writing: ',txt1,txt2,nx,ny,nz_ep
!RH_3D_not_needed      write(6      ,'(A18,2A10,3I3)')  'Finished writing: ',txt1,txt2,nx,ny,nz_ep   
!RH_3D_not_needed

! *** Write 3D Potential Temperature to file;  pt(nx, ny, nz, hh); (K)
      un    = 63
!      EP_fm =  1
      EPfilename =  EP_3D_pot_T
      namefield  = 'PT'
      unitname   = 'K'
      do n = 1,hh
! ***   Interpolate the 3D Potential Temperature field:
        if (apply_interpolation) then
          call lin_int(nx,ny,nz,nz_ep,z,pt(:,:,:,n),z_ep,pt_ep,extrapolation_applied)
        else
          do i = 1,nx
            do j = 1,ny
              do k = 1, nz_ep
                pt_ep(i,j,k) = pt(i,j,k,n)
              enddo
            enddo
          enddo
        endif
        write(txt1,'(A6,I4.4)') 'pT__h=',n
        if (EP_fm == 3) then
          call write_3dfield(nx,ny,txt1,txt2,un,EP_fm,EPfilename,namefield,unitname,n,pt_ep(:,:,:))
        else
          do k = 1,nz_ep
            write(txt2,'(A7,I3.3)') '_Lay_k=',k
            call write_2dfield(nx,ny,txt1,txt2,un,EP_fm,EPfilename,namefield,unitname,n,pt_ep(:,:,k))
          enddo
        endif
      enddo
      write(funit_log,'(A18,2A10,3I3)')  'Finished writing: ',txt1,txt2,nx,ny,nz_ep
      write(6      ,'(A18,2A10,3I3)')  'Finished writing: ',txt1,txt2,nx,ny,nz_ep

! *** Write 3D Turbulent kinetic energy to file;  tke(nx, ny, nz, hh); (m^2/s^2)
      un    = 64
!      EP_fm =  1
      EPfilename =  EP_3D_tke
      namefield  = 'TKE'
      unitname   = 'm^2/s^2'
      do n = 1,hh
! ***   Interpolate the 3D Turbulent kinetic energy field:
        if (apply_interpolation) then
          call lin_int(nx,ny,nz,nz_ep,z,tke(:,:,:,n),z_ep,tke_ep,extrapolation_applied)
        else
          do i = 1,nx
            do j = 1,ny
              do k = 1, nz_ep
                tke_ep(i,j,k) = tke(i,j,k,n)
              enddo
            enddo
          enddo
        endif
        write(txt1,'(A6,I4.4)') 'TKE_h=',n
        if (EP_fm == 3) then
          call write_3dfield(nx,ny,txt1,txt2,un,EP_fm,EPfilename,namefield,unitname,n,tke_ep(:,:,:))
        else
          do k = 1,nz_ep
            write(txt2,'(A7,I3.3)') '_Lay_k=',k
            call write_2dfield(nx,ny,txt1,txt2,un,EP_fm,EPfilename,namefield,unitname,n,tke_ep(:,:,k))
          enddo
        endif
      enddo
      write(funit_log,'(A18,2A10,3I3)')  'Finished writing: ',txt1,txt2,nx,ny,nz_ep
      write(6      ,'(A18,2A10,3I3)')  'Finished writing: ',txt1,txt2,nx,ny,nz_ep


! *** Write the 3-components of the 3D wind field to file:
      un    = 71
!      EP_fm =  1
      EPfilename =  EP_3D_wind
      unitname   = 'm/s'
      do n = 1,hh
! ***   Interpolate the 3-components of the 3D wind field:
        if (apply_interpolation) then
          call lin_int(nx,ny,nz,nz_ep,z,uu(:,:,:,n),z_ep,uu_ep,extrapolation_applied)
          call lin_int(nx,ny,nz,nz_ep,z,vv(:,:,:,n),z_ep,vv_ep,extrapolation_applied)
          call lin_int(nx,ny,nz,nz_ep,z,ww(:,:,:,n),z_ep,ww_ep,extrapolation_applied)
        else
          do i = 1,nx
            do j = 1,ny
              do k = 1, nz_ep
                uu_ep(i,j,k) = uu(i,j,k,n)
                vv_ep(i,j,k) = vv(i,j,k,n)
                ww_ep(i,j,k) = ww(i,j,k,n)
              enddo
            enddo
          enddo
        endif
        write(txt1,'(A6,I4.4)') 'UVW_h=',n
        if (EP_fm == 3) then
          namefield  = 'UU'
          call write_3dfield(nx,ny,txt1,txt2,un,EP_fm,EPfilename,namefield,unitname,n,uu_ep(:,:,:))
          namefield  = 'VV'
          call write_3dfield(nx,ny,txt1,txt2,un,EP_fm,EPfilename,namefield,unitname,n,vv_ep(:,:,:))
          namefield  = 'WW'
          call write_3dfield(nx,ny,txt1,txt2,un,EP_fm,EPfilename,namefield,unitname,n,ww_ep(:,:,:))
        else
          do k = 1,nz_ep
            write(txt2,'(A6,I4.4)') '_U__k=',k
            namefield  = 'UU'
            call write_2dfield(nx,ny,txt1,txt2,un,EP_fm,EPfilename,namefield,unitname,n,uu_ep(:,:,k))
            write(txt2,'(A6,I4.4)') '_V__k=',k
            namefield  = 'VV'
            call write_2dfield(nx,ny,txt1,txt2,un,EP_fm,EPfilename,namefield,unitname,n,vv_ep(:,:,k))
            write(txt2,'(A6,I4.4)') '_W__k=',k
            namefield  = 'WW'
            call write_2dfield(nx,ny,txt1,txt2,un,EP_fm,EPfilename,namefield,unitname,n,ww_ep(:,:,k))   
          enddo
        endif
      enddo
      write(funit_log,'(A18,2A10,3I3)')  'Finished writing: ',txt1,txt2,nx,ny,nz_ep
      write(6      ,'(A18,2A10,3I3)')  'Finished writing: ',txt1,txt2,nx,ny,nz_ep

! *** Based on the 3D-temperature field, tt_ep(nx,ny,nz), tscr, and the "height_above_ground",
! *** z(nx,ny,nz), the "t and dtdz"-file can be produced:

      alternativ = 1    ! DEFAULT: TSCREEN and  DTDZ = (T(K=1) - T_SCREEN)/(z(K=1) - 2m    )
!      alternativ = 2   ! TEST:    T(K=1)  and  DTDZ = (T(K=2) - T(K=1)  )/(z(K=2) - z(K=1))
      un     = 81
!      EP_fm  = 1
      EPfilename =  EP_2_2D_T_and_dtdz
      do n=1,hh
        write(txt1,'(A6,I4.4)') 'Tmp_h=',n
        namefield  = 'TT'
        unitname   = 'degC'
        if (alternativ == 1) then
! ***     Alternative 1:
          write(txt2,'(A10)')   '__Screen_T'
          call write_2dfield(nx,ny,txt1,txt2,un,EP_fm,EPfilename,namefield,unitname,n,tscr(:,:,n)-273.15)
        else
! ***     Alternative 2:
          write(txt2,'(A10)')   '_Tmp_k=  1'
          call write_2dfield(nx,ny,txt1,txt2,un,EP_fm,EPfilename,namefield,unitname,n,tt(:,:,1,n)-273.15)
        endif
        if (alternativ == 1) then
! ***     Alternative 1:
          write(txt2,'(A10)')   'DTDZ(k1-2)'
        else
! ***     Alternative 2:        
          write(txt2,'(A10)')   'DTDZ_k2-k1'
        endif
        namefield  = 'DTDZ'
        unitname   = 'K/m'
        do j = 1,ny
          do i = 1,nx
            if (alternativ == 1) then
! ***         Alternative 1:
              dtdz(i,j,n) = (tt(i,j,1,n)-tscr(i,j,n))/(z(i,j,1) - 2.0)
            else
! ***         Alternative 2:
              dtdz(i,j,n) = (tt(i,j,2,n)-tt(i,j,1,n))/(z(i,j,2)-z(i,j,1))
            endif
          enddo
        enddo
        call write_2dfield(nx,ny,txt1,txt2,un,EP_fm,EPfilename,namefield,unitname,n,dtdz(:,:,n))
      enddo 
      write(funit_log,'(A18,2A10,2I3,A)')  'Finished writing: ',txt1,txt2,nx,ny,' 2'
      write(6      ,'(A18,2A10,2I3,A)')  'Finished writing: ',txt1,txt2,nx,ny,' 2'


! *** Close all of the applied files:

      close (funit_inpath_tapm) 
      
      close (21)  !  '../input_EPISODE_data/topo.txt'          ;   zs(nx, ny)
      
      close (31)  !  '../input_EPISODE_data/3D_TAPM_z_abg.txt'      ;   z(nx, ny, nz)
      close (32)  !  '../input_EPISODE_data/3D_EPISODE_z_abg.txt'   ;   z_ep(nx, ny, nz_ep)
      
      close (41)  !  '../input_EPISODE_data/tot_solar_rad.txt' ;   tsr(nx, ny, hh)  
      close (42)  !  '../input_EPISODE_data/net_rad.txt'       ;   net(nx, ny, hh)
      close (43)  !  '../input_EPISODE_data/sens_heatfl.txt'   ;   sens(nx, ny, hh)
      close (44)  !  '../input_EPISODE_data/evap_heatfl.txt'   ;   evap(nx, ny, hh)
      close (45)  !  '../input_EPISODE_data/ustar.txt'         ;   ustar(nx, ny, hh)
      close (46)  !  '../input_EPISODE_data/ptstar.txt'        ;   ptstar(nx, ny, hh)
      close (47)  !  '../input_EPISODE_data/pvstar.txt'        ;   pvstar(nx, ny, hh)     
      close (48)  !  '../input_EPISODE_data/wstar.txt'         ;   wstar(nx, ny, hh)
      close (49)  !  '../input_EPISODE_data/hmix.txt'          ;   hmix(nx, ny, hh)
      close (50)  !  '../input_EPISODE_data/T_screen.txt'      ;   tscr(nx, ny, hh)
      close (51)  !  '../input_EPISODE_data/RH_screen.txt'     ;   rhscr(nx, ny, hh)
      close (52)  !  '../input_EPISODE_data/T_surf.txt'        ;   tsurf(nx, ny, hh)
      close (53)  !  '../input_EPISODE_data/prec.txt'          ;   rain(nx, ny, hh)

! *** Close the 3D time-dependent output files:
      close (61)  !  '../input_EPISODE_data/temp.txt'          ;   tt(nx, ny, nz_ep, hh)
!RH_3D_not_needed      close (62)  !  '../input_EPISODE_data/rh.txt'            ;   rh(nx, ny, nz_ep, hh)
      close (63)  !  '../input_EPISODE_data/pot_T.txt'         ;   pt(nx, ny, nz_ep, hh)
      close (64)  !  '../input_EPISODE_data/tke.txt'           ;   tke(nx, ny, nz_ep, hh)
             
      close (71)  !  '../input_EPISODE_data/wind.txt'          ;   uu; vv; ww(nx, ny, nz_ep, hh);
      
      close (81)  !  '../input_EPISODE_data/T_and_dtdz.txt'    ;   dtdz(nx, ny, hh)

      write(funit_log,*) 
      write(funit_log,'(A)')  'Program Sucessfully Finished.'    
  !    close (funit_log)  !  '../input_EPISODE_data/Log_tapm4episode.t      dimd  = 2xt'
      write(6      ,'(A)')  'Program Sucessfully Finished.'

! *** If we run the program in Debug mode, it can be nice to activate the two lines below:      
!       write(6      ,'(A)')  'Continue: press 1 ans <CR>'
!       read(5,*) istop      
      
      goto 100
      
! *** ERROR TREATMENT:
 200  continue


 100  continue
 

! *** Deallocation of variables

      if (allocated(zs))      deallocate( zs )
      if (allocated(z))       deallocate( z )
      if (allocated(z_ep))    deallocate( z_ep )

      if (allocated(tsr))     deallocate( tsr )
      if (allocated(net))     deallocate( net )
      if (allocated(sens))    deallocate( sens )
      if (allocated(evap))    deallocate( evap )
      if (allocated(ustar))   deallocate( ustar )
      if (allocated(ptstar))  deallocate( ptstar )
      if (allocated(pvstar))  deallocate( pvstar )
      if (allocated(wstar))   deallocate( wstar )
      if (allocated(hmix))    deallocate( hmix )
      if (allocated(tscr))    deallocate( tscr )
      if (allocated(rhscr))   deallocate( rhscr )
      if (allocated(tsurf))   deallocate( tsurf )
      if (allocated(rain))    deallocate( rain )
      if (allocated(dtdz))    deallocate( dtdz )

! *** For each hour,each level:
      if (allocated(ws))      deallocate( ws )
      if (allocated(ws_ep))   deallocate( ws_ep )
      if (allocated(wd))      deallocate( wd )
      if (allocated(wd_ep))   deallocate( wd_ep )
      if (allocated(ww))      deallocate( ww )
      if (allocated(ww_ep))   deallocate( ww_ep )
      if (allocated(tt))      deallocate( tt )
      if (allocated(tt_ep))   deallocate( tt_ep )
      if (allocated(rh))      deallocate( rh )
      if (allocated(rh_ep))   deallocate( rh_ep )
      if (allocated(pt))      deallocate( pt )
      if (allocated(pt_ep))   deallocate( pt_ep )
      if (allocated(tke))     deallocate( tke )
      if (allocated(tke_ep))  deallocate( tke_ep )
    
      if (allocated(uu))      deallocate( uu )
      if (allocated(uu_ep))   deallocate( uu_ep )
      if (allocated(vv))      deallocate( vv )
      if (allocated(vv_ep))   deallocate( vv_ep )

! *** Helper variables for netCDF output
      if (allocated(axm_i))   deallocate(axm_i)
      if (allocated(axm_j))   deallocate(axm_j)
      if (allocated(znc))     deallocate(znc)
      if (allocated(field2D)) deallocate(field2D)
      if (allocated(field3D)) deallocate(field3D)

      contains
      
! ******************************************************************************************************

      subroutine write_2dfield(nx,ny,txt1,txt2,un,fm,EPfilename,namefield,unitname,hour,f2d)

      IMPLICIT NONE
      
! *** Global variables:
     
      integer           :: nx,ny
      integer           :: un,fm
      integer           :: hour
      real              :: f2d(nx,ny)
      character(len=10) :: txt1,txt2
      character(len=10) :: namefield
      character(len=10) :: unitname
      
! *** Local variables:          
      integer           :: ix,iy,ik

      character(len=256) :: EPfilename
! *** Start of routine:


! *** Write the variables to netcdf files
       if (fm == 3) then

         Nhh_in = 1
         ! current simulation date
         mdate(1,Nhh_in) = bdat(1)
         mdate(2,Nhh_in) = bdat(2)
         mdate(3,Nhh_in) = bdat(3)
         mdate(4,Nhh_in) = hour

         do ix = 1,nx
           do iy = 1,ny
             field2D(iy,ix) = dble( f2d(ix,iy) )
           enddo
         enddo
       
         call writeconcfield(EPfilename,namefield,unitname,       &
                             field2D(1:ny,1:nx), ny, nx, 1,      &
                             Nhh_in, mdate, validity, dopacking, dofloat, domirror)
                             
       else 
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
      endif

      return
      end subroutine write_2dfield

!*******************************************************************************
      
      subroutine write_3dfield(nx,ny,txt1,txt2,un,fm,EPfilename,namefield,unitname,hour,f3d)

      IMPLICIT NONE
      
! *** Global variables:
     
      integer           :: nx,ny
      integer           :: un,fm
      integer           :: hour
      real              :: f3d(nx,ny,nz_ep)
      character(len=10) :: txt1,txt2
      character(len=10) :: namefield
      character(len=10) :: unitname
      
! *** Local variables:          
      integer           :: ix,iy,ik

      character(len=256) :: EPfilename
! *** Start of routine:


! *** Write the variables to netcdf files

        Nhh_in = 1
        ! current simulation date
        mdate(1,Nhh_in) = bdat(1)
        mdate(2,Nhh_in) = bdat(2)
        mdate(3,Nhh_in) = bdat(3)
        mdate(4,Nhh_in) = hour

        do ix = 1,nx
          do iy = 1,ny
            do ik = 1,nz_ep
              field3D(iy,ix,ik) = dble( f3d(ix,iy,ik) )
            enddo
          enddo
        enddo
        call writeconcfield(EPfilename,namefield,unitname,       &
                             field3D(1:ny,1:nx,1:nz_ep), ny, nx, nz_ep,      &
                             Nhh_in, mdate, validity, dopacking, dofloat, domirror)


      return
      end subroutine write_3dfield

!*******************************************************************************

      subroutine comp_vel(ff,dd,u,v)

! *** This function converts the wind velocity from:   
! ***         speed (ff) and direction (dd) 
! *** to:       u   and           v.


! *** We require the use of IMPLICIT NONE:

      implicit none

! *** Global declarations:
      real,intent(in)  :: ff,dd
      real,intent(out) :: u,v

! *** Local declarations:
      real,parameter :: pi  = 3.141592654
      real,parameter :: rad = pi/180.0

      real :: theta

! *** "pi"    - The constant pi.
! *** "rad"   - The conversion from degrees to radians.
!
! *** "dd" =  0 degrees means wind from the north blowing southards.
! *** "dd" = 90 degrees means wind from the east  blowing westwards.  
!
! *** Converts the wind direction to radians:

      theta = dd * rad

      u = -ff*sin(theta)
      v = -ff*cos(theta)

      return
      
      end subroutine comp_vel

!**********************************************************************************

      subroutine lin_int(nx,ny,nz,nz_ep,z,fi3D,z_ep,fi3D_ep,extrapolation_applied)

! *** This routine interpolates linearly between the verical TAPM layers
! *** to the (midpoint of each)specified EPISODE layer.

! *** The routine assumes that the first layer of the two models is identical,
! *** and therefore: fi3D_ep(i,j,1) = fi3D(i,j,1)

! *** If somehow the uppermost TAPM value is above the uppermost EPISODE
! *** value, the uppermost EPISODE value is set equal to the uppermost TAPM 
! *** value and the logical "extrapolation_applied" variable is returned with the
! *** value .TRUE.

      
      implicit none
      
      integer :: nx,ny,nz,nz_ep
      real    :: z(nx,ny,nz),z_ep(nx,ny,nz_ep) 
      real    :: fi3D(nx,ny,nz),fi3D_ep(nx,ny,nz_ep)
      logical :: extrapolation_applied
      
      integer :: i,j,k,kk,n
      real    :: w_lo,w_hi

      extrapolation_applied = .false.
      
      do i = 1,nx
        do j = 1,ny

! ***     First layer value assumed identical:
          fi3D_ep(i,j,1) = fi3D(i,j,1)

! ***     Go trough the rest of the EPISODE layers
          do k = 2,nz_ep
            
            n = 1
            do
              n = n + 1
              
              if (z_ep(i,j,k) <= z(i,j,n) .AND. z_ep(i,j,k) > z(i,j,n-1)) then
                w_hi = (z_ep(i,j,k) - z(i,j,n-1) )/(z(i,j,n) - z(i,j,n-1))
                w_lo = 1.0 - w_hi
                fi3D_ep(i,j,k) = w_hi * fi3D(i,j,n) +w_lo * fi3D(i,j,n-1)
                exit
              endif
              
              if (n == nz .and. k <= nz_ep) then
                do kk = k,nz_ep
                  fi3D_ep(i,j,kk) = fi3D(i,j,n)
                  extrapolation_applied = .true.
                enddo
                exit
              endif
              
            enddo
            
          enddo
        enddo
      enddo
      
      return
      end subroutine lin_int
      
      End Program tapm4cc








