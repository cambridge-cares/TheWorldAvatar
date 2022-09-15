! <mod_site.f90 - A component of the City-scale
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
!* Walker, S.-E., Solberg, S., Denby, B. (2003) Development and implementation of a 
!*    simplified EMEP photochemistry scheme for urban areas in EPISODE. NILU TR 13/2013. 
!*    ISBN 82-425-1524-7
!*****************************************************************************! 

      module mod_site

! ----------------------------------------------------------------------------------
! Based on:
! Version Episode 5.5 (May29, 2012) prepared for BB-Stand-Alone
! Original source code of EPISODE by Sam-Erik Walker (NILU)
!
! Sam-Erik Walker
! Norwegian Institute for Air Research (NILU)
! Instituttveien 18 P.O. Box 100
! N-2027 Kjeller, NORWAY
! Tel: +47 63898000 Fax: +47 63898050
! E-mail: sam-erik.walker@nilu.no
!
! ----------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------
! REVISION HISTORY
!
!    xx Dat 201x  Name: Line  Description of Change
!
! ----------------------------------------------------------------------------------

      integer :: SiteExternalData   ! = 1  ! Default value.
! *** If SiteExternalData = 0: Some domain data are read internally in Episode. Not applied.
! *** If SiteExternalData = 1: The data are transferred from the "integrated AirQUIS/McWIND".
! *** If SiteExternalData = 2: Domain data linked to the   UM Met-model.
! *** If SiteExternalData = 3: Domain data linked to the TAPM Met-model.

      integer :: mx
      integer :: my
      integer :: mz
!_NEST_Start:
      integer :: mbcx
      integer :: mbcy
      integer :: mbcz

      integer :: mx_nest
      integer :: my_nest
      integer :: mz_nest

      integer :: mbcx_nest
      integer :: mbcy_nest
      integer :: mbcz_nest
!_NEST_End.

      integer :: mr
      integer :: mxs
      integer :: mys
      integer :: mxyp2
      integer :: mls
      integer :: mlt

!     mx    - Max number of main grid cells in x-direction
!     my    - Max number of main grid cells in y-direction
!     mz    - Max number of main grid cells in z-direction
!     mr    - Max number of receptor points
!     mxs   - Max number of sub  grid cells in x-direction
!     mys   - Max number of sub  grid cells in x-direction
!     mxyp2 - Dimension value >= max(mx+2,my+2). Usually set equal to
!             mx + my + 2.
!     mls   - Max number of main grid cells with sub grid concentrations
!     mlt   - Max number of main grid cells with sub grid topography

      parameter (mls = 32)

      character(len=256) :: hbldfn = ' '
      character(len=256) :: recpfn = ' '
      character(len=256) :: subffn = ' '
      character(len=256) :: surffn = ' '     
      character(len=256) :: topffn = ' '
      character(len=256) :: topmfn = ' '
      character(len=256) :: topsfn = ' '
!_NEST_Start:
      character(len=256) :: topmfn_nest = ' '

!     hbldfn   - Height of buildings ....... filename
!     recpfn   - Receptor points ........... filename
!     subffn   - Sub  grid  flag ........... filename
!     surffn   - Surface roughness ......... filename
!     topffn   - Topography flag ........... filename
!     topmfn   - Main grid topography ...... filename
!     topsfn   - Sub  grid topography ...... filename
!     topmfn_nest - Nested Fine grid topography filename
!_NEST_End.

      integer :: hbldun = 0
      integer :: recpun = 0
      integer :: subfun = 0
      integer :: surfun = 0
      integer :: topfun = 0
      integer :: topmun = 0
      integer :: topsun = 0
!_NEST_Start:
      integer :: topmun_nest = 0

!     hbldun   - Height of buildings ....... fileunit
!     recpun   - Receptor points ........... fileunit
!     subfun   - Sub  grid  flag ........... fileunit
!     surfun   - Surface roughness ......... fileunit
!     topfun   - Topography flag ........... fileunit
!     topmun   - Main grid topography ...... fileunit
!     topsun   - Sub  grid topography ...... fileunit
!     topmun_nest - Nested Fine grid topography fileunit
!_NEST_End.

      logical :: hbldfe = .false.
      logical :: recpfe = .false.
      logical :: subffe = .false.
      logical :: surffe = .false.
      logical :: topffe = .false.
      logical :: topmfe = .false.
      logical :: topsfe = .false.
!_NEST_Start:
      logical :: topmfe_nest = .false.

!     hbldfe   - Height of buildings ....... file exists
!     recpfe   - Receptor points ........... file exists
!     subffe   - Sub  grid  flag ........... file exists
!     surffe   - Surface roughness ......... file exists
!     topffe   - Topography flag ........... file exists
!     topmfe   - Main grid topography ...... file exists
!     topsfe   - Sub  grid topography ...... file exists
!     topmfe_nest - Nested Fine grid topography file exists
!_NEST_End.

      real :: hbldfv(1) = 0.
      real :: recpfv(1) = 0.
      real :: subffv(1) = 0.
      real :: surffv(1) = 0.
      real :: topffv(1) = 0.
      real :: topmfv(1) = 0.
      real :: topsfv(1) = 0.
!_NEST_Start:
      real :: topmfv_nest(1) = 0.

!     hbldfv   - Height of buildings ....... file value
!     recpfv   - Receptor points ........... file value
!     subffv   - Sub  grid  flag ........... file value
!     surffv   - Surface roughness ......... file value
!     topffv   - Topography flag ........... file value
!     topmfv   - Main grid topography ...... file value
!     topsfv   - Sub  grid topography ...... file value
!     topmfv_nest - Nested Fine grid topography file value
!_NEST_End.

      integer :: hbldfm = 0
      integer :: recpfm = 0
      integer :: subffm = 0
      integer :: surffm = 0
      integer :: topffm = 0
      integer :: topmfm = 0
      integer :: topsfm = 0
!_NEST_Start:
      integer :: topmfm_nest = 0

!     hbldfm   - Height of buildings ....... file format (index)
!     recpfm   - Receptor points ........... file format (index)
!     subffm   - Sub  grid  flag ........... file format (index)
!     surffm   - Surface roughness ......... file format (index)
!     topffm   - Topography flag ........... file format (index)
!     topmfm   - Main grid topography ...... file format (index)
!     topsfm   - Sub  grid topography ...... file format (index)
!     topmfm_nest - Nested Fine grid topography file format (index)
!_NEST_End.

      character(len=256) :: siteid = ' '
!MSK start
      character(len=8) :: utmzone = ' '
      character(len=8) :: EPSGN   = ' '
      integer :: stations = 0
!MSK end

!     siteid - Site identification

      real :: sitela = 0.
      real :: sitelo = 0.
      real :: sitex0 = 0.
      real :: sitey0 = 0.
!_NEST_Start:
      real :: sitex0_nest = 0.
      real :: sitey0_nest = 0.
	
      real :: f_cor = 0.

!     sitela     - Site latitude
!     sitelo     - Site longitude
!     sitex0     - Site origo x-coordinate coarse grid
!     sitey0     - Site origo y-coordinate coarse grid
!     sitex0_nest - Site origo x-coordinate fine grid
!     sitey0_nest - Site origo y-coordinate fine grid
!     f_cor      - Site Coriolis parameter
!_NEST_End.

      real              :: angle = 0.
      real              :: dx    = 0.
      real              :: dy    = 0.

!MSK      real, allocatable :: dz(:)
      double precision, allocatable :: dz(:)
 
!_NEST_Start:
      real :: angle_nest = 0.
      real :: dx_nest = 0.
      real :: dy_nest = 0.
      real,allocatable :: dz_nest(:)

!     angle     - Rotation of coarse grid x-axis with respect to E-W x-axis (deg)
!     dx        - Size   of coarse gridcells in x-direction (m)
!     dy        - Size   of coarse gridcells in y-direction (m)
!     dz        - Size   of coarse gridcells in z-direction (m) (nz)
!     angle_nest - Rotation of fine   grid x-axis with respect to E-W x-axis (deg)
!     dx_nest    - Size   of  fine  gridcells in x-direction (m)
!     dy_nest    - Size   of  fine  gridcells in y-direction (m)
!     dz_nest    - Size   of  fine  gridcells in z-direction (m)
!_NEST_End.

      integer :: nx = 0
      integer :: ny = 0
      integer :: nz = 0
!_NEST_Start:
      integer :: nbcx = 0
      integer :: nbcy = 0
      integer :: nbcz = 0

!     nx   - Number of gridcells in x-direction
!     ny   - Number of gridcells in y-direction
!     nz   - Number of gridcells in z-direction
!     nbcx - Number of extra gridcells in x-direction for east/west   coarse BC
!     nbcy - Number of extra gridcells in y-direction for south/north coarse BC
!     nbcz - Number of extra gridcells in z-direction for the upper   coarse BC
!_NEST_End.

!_NEST_Start:
      integer :: nx_nest = 0
      integer :: ny_nest = 0
      integer :: nz_nest = 0
      integer :: nbcx_nest = 0
      integer :: nbcy_nest = 0
      integer :: nbcz_nest = 0

!     nx_nest    - Number of  fine gridcells in x-direction
!     ny_nest    - Number of  fine gridcells in y-direction
!     nz_nest    - Number of  fine gridcells in z-direction
!     nbcx_nest  - Number of extra gridcells in x-direction for east/west   fine BC
!     nbcy_nest  - Number of extra gridcells in y-direction for south/north fine BC
!     nbcz_nest  - Number of extra gridcells in z-direction for the upper   fine BC
!_NEST_End.

      real :: dx2 = 0.
      real :: dxy = 0.
      real :: dy2 = 0.
      real,allocatable :: vol(:)
      real :: xmin = 0.
      real :: ymin = 0.
      real :: xmax = 0.
      real :: ymax = 0.
      real,allocatable :: z(:)
!_NEST_Start:
      real,allocatable :: z_nest(:)

!     dx2   - dx*dy (m2)
!     dxy   - Area   of each gridcell dx*dy (m2)
!     dy2   - dy*dy (m2)
!     vol   - Volume of each gridcell in nz layers (m3) (nz)
!     xmin  - 0.0 (m)
!     ymin  - 0.0 (m)
!     xmax  - nx*dx (m)
!     ymax  - ny*dy (m)
!     z     - Height of top of each coarse grid vertical layer above ground (m) (nz + nbcz)
!     z_nest - Height of top of each   fine grid vertical layer above ground (m) (nz_nest + nbcz_nest)
!_NEST_End.

      integer :: nxm1 = 0
      integer :: nym1 = 0
      integer :: nzm1 = 0

!     nxm1 - nx minus 1
!     nym1 - ny minus 1
!     nzm1 - nz minus 1

!_SIGMA_NEST_Start:
      real,allocatable :: hbld(:,:)
      real,allocatable :: subf(:,:)
      real,allocatable :: topf(:,:)
      real,allocatable :: topm(:,:)
      real,allocatable :: tops(:,:,:)
      real,allocatable :: z0(:,:)
!_LHS_UM_06Apr_2011_Start:      
      real,allocatable :: z0_t(:,:)
!_LHS_UM_06Apr_2011_End.

!_LHS_Change_12Oct2011_Start:
      real,allocatable :: albedo(:,:)
      real    :: default_albedo          = 0.3
      logical :: default_albedo_applied = .FALSE.

! *** default_albedo : Default relative humidity  if albedo(nx,ny) is not transferred to EPISODE.
! ***                : The logical is set to .TRUE. in MPROF_NEW if "default_albedo" are to be applied.             
!_LHS_Change_12Oct2011_End.

      real,allocatable :: depthm(:,:)
      real :: mod_h = 0.
      real,allocatable :: topm_nest(:,:)
      real :: mod_h_nest = 0.

!     hbld    - Height of buildings (nx,ny)
!     subf    - Sub  grid  flags (nx,ny)
!     topf    - Topography flags (nx,ny)
!     topm    - Main grid topography (nx,ny)
!     tops    - Sub  grid topography (nxs,nys,nlt)
!     z0      - Surface roughness (nx,ny)
!     z0_t    - Surface roughness for heat (nx,ny)
!     depthm  - Model depth (nx,ny)
!     mod_h   - Constant coarse grid model sigma-height (in metres).
!     topm_nest  - Nested Fine grid topography 
!              (1-nbcx_nest:nx_nest+nbcx_nest,1-nbcy_nest:ny_nest+nbcy_nest)
!     mod_h_nest - Constant   fine grid model sigma-height (in metres).
!
!_SIGMA_NEST_End.

      real :: dxs = 0.
      real :: dys = 0.
      real :: dzs = 0.

!     dxs - Size of subgrid cells in x-direction (m)
!     dys - Size of subgrid cells in y-direction (m)
!     dzs - Size of subgrid cells in z-direction (m)

      integer :: nls = 1
      integer :: nlt = 0
      integer :: nxs        ! = 1
      integer :: nys        ! = 1
      integer :: nzs        ! = 1

!     nls - Number of subgrid layers
!     nxs - Number of subgrid cells in x-direction
!     nys - Number of subgrid cells in y-direction
!     nzs - Number of subgrid cells in z-direction

      real,allocatable :: rminr(:)
      real,allocatable :: xr(:)
      real,allocatable :: yr(:)
      real,allocatable :: zr(:)

!     rminr - Receptor point minimum distance to line source (nr)
!     xr    - Receptor point x-coordinate (nr)
!     yr    - Receptor point y-coordinate (nr)
!     zr    - Receptor point z-coordinate (nr)

      integer :: nr = 0

!     nr - Number of receptor points

      character(len=256),allocatable :: txtr(:)

!     txtr - Receptor point textstring (nr)

      contains


!MSK included ilay.for from ./util as function ilay

      INTEGER FUNCTION ILAY(H)

! The function calculates the index of the vertical layer
! corresponding to height H.

! Scalar arguments

      real :: H

! H - Height

! Local variables

      INTEGER IL,K

! IL - Index of vertical layer

      IL = 0
      DO 100 K = 1,NZ
          IL = IL + 1
          IF (Z(K) .GT. H) GOTO 110
  100 CONTINUE
      ILAY = NZ + 1

      RETURN

  110 CONTINUE
      ILAY = IL

      RETURN

! End of real function ILAY

      end function ilay


      subroutine FreeSiteMemory()

!DEC$ ATTRIBUTES DLLEXPORT,STDCALL,ALIAS:'FreeSiteMemory' :: FreeSiteMemory

      implicit none

      if (allocated(dz))      deallocate(dz)
!_LHS_Change_26Feb2012_Start:
      if (allocated(dz_nest)) deallocate(dz_nest)
!_LHS_Change_26Feb2012_End.
      if (allocated(vol))     deallocate(vol)
      if (allocated(z))       deallocate(z)
!_LHS_Change_26Feb2012_Start:
      if (allocated(z_nest))  deallocate(z_nest)
!_LHS_Change_26Feb2012_End.
      
      if (allocated(hbld))   deallocate(hbld)
      if (allocated(subf))   deallocate(subf)
      if (allocated(topf))   deallocate(topf)
      if (allocated(topm))   deallocate(topm)
      if (allocated(tops))   deallocate(tops)
      if (allocated(z0))     deallocate(z0)
      if (allocated(z0_t))   deallocate(z0_t)
      
      if (allocated(albedo)) deallocate(albedo)
      
      if (allocated(depthm)) deallocate(depthm)

!_LHS_Change_26Feb2012_Start:
      if (allocated(topm_nest))  deallocate(topm_nest)
!_LHS_Change_26Feb2012_End.

      if (allocated(rminr))  deallocate(rminr)
      if (allocated(xr))     deallocate(xr)
      if (allocated(yr))     deallocate(yr)
      if (allocated(zr))     deallocate(zr)
      
      if (allocated(txtr))   deallocate(txtr)

      mx = 0
      my = 0
      mz = 0
      mr = 0
      mxs = 0
      mys = 0
      mxyp2 = 0
      mlt = 0
      hbldfn = ' '
      recpfn = ' '
      subffn = ' '
      surffn = ' '
      topffn = ' '
      topmfn = ' '
      topsfn = ' '
      hbldun = 0
      recpun = 0
      subfun = 0
      surfun = 0
      topfun = 0
      topmun = 0
      topsun = 0
      hbldfe = .false.
      recpfe = .false.
      subffe = .false.
      surffe = .false.
      topffe = .false.
      topmfe = .false.
      topsfe = .false.
      hbldfv = 0.
      recpfv = 0.
      subffv = 0.
      surffv = 0.
      topffv = 0.
      topmfv = 0.
      topsfv = 0.
      hbldfm = 0
      recpfm = 0
      subffm = 0
      surffm = 0
      topffm = 0
      topmfm = 0
      topsfm = 0
      siteid = ' '
!MSK start
      utmzone = ' '
      stations = 0
!MSK end
      sitela = 0.
      sitelo = 0.
      sitex0 = 0.
      sitey0 = 0.
      f_cor = 0.
      angle = 0.
      dx = 0.
      dy = 0.
      nx = 0
      ny = 0
      nz = 0
      dx2 = 0.
      dxy = 0.
      dy2 = 0.
      xmin = 0.
      ymin = 0.
      xmax = 0.
      ymax = 0.
      nxm1 = 0
      nym1 = 0
      nzm1 = 0
      mod_h = 0.
      dxs = 0.
      dys = 0.
      dzs = 0.
      nls = 0
      nlt = 0
      nxs = 0
      nys = 0
      nzs = 0
      nr = 0

!     End of subroutine FreeSiteMemory
      end subroutine

      end module mod_site
