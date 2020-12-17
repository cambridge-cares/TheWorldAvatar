! <mod_grid.f90 - A component of the City-scale
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

      module mod_grid

! *** This is the module for the main grid
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
!           2017  M. Karl: netcdf file control parameters
!           2017  M. Karl: c,dcdt,d2cdx2,d2cdy2,excz,bc defined as double precision
!    18 Apr 2018  M. Karl: L155  Added parameter scalepm
!
! ----------------------------------------------------------------------------------

      character(len=256),allocatable :: bconfn(:)
!MSK      character(len=256),allocatable :: newcfn(:)
!MSK      character(len=256),allocatable :: newbcfn(:)
      character(len=256),allocatable :: oldcfn(:)
!MSK      character(len=256),allocatable :: oldcfn_nest(:)

!      bconfn    - Background     concentrations filename
!      newcfn    - New grid model concentrations filename (nc)
!_NEST newbcfn   - New boundary condition concentrations filename (nc)
!      oldcfn    - Old grid model concentrations filename (nc)
!_NEST oldcfn_nest  - Old FINE grid model concentrations filename (nc)

      integer,allocatable :: bconun(:)
!MSK      integer,allocatable :: newcun(:)
!MSK      integer,allocatable :: newbcun(:)
      integer,allocatable :: oldcun(:)
!MSK      integer,allocatable :: oldcun_nest(:)

!      bconun   - Background     concentrations fileunit
!      newcun   - New grid model concentrations fileunit (nc)
!_NEST newbcun  - New boundary condition concentrations fileunit (nc)
!      oldcun   - Old grid model concentrations fileunit (nc) slurm-8443949.out
!_NEST oldcun_nest - Old FINE grid model concentrations fileunit (nc)

      logical,allocatable :: bconfe(:)
!MSK      logical,allocatable :: newcfe(:)
!MSK      logical,allocatable :: newbcfe(:)
      logical,allocatable :: oldcfe(:)
!MSK      logical,allocatable :: oldcfe_nest(:)

!      bconfe   - Background concentrations file exists
!      newcfe   - New grid model concentrations file exists (nc)
!_NEST newbcfe  - New boundary condition concentrations file exists (nc)
!      oldcfe   - Old grid model concentrations file exists (nc)
!_NEST oldcfe_nest - Old FINE grid model concentrations file exists (nc)

      real,allocatable :: bconfv(:)
!MSK      real,allocatable :: newcfv(:)
!MSK      real,allocatable :: newbcfv(:)
      real,allocatable :: oldcfv(:)
!MSK      real,allocatable :: oldcfv_nest(:)

!      bconfv   - Background concentrations file value
!      newcfv   - New grid model concentrations file value (nc)
!_NEST newbcfv  - New boundary condition concentrations file value (nc)
!      oldcfv   - Old grid model concentrations file value (nc)
!_NEST oldcfv_nest - Old FINE grid model concentrations file value (nc)

      integer,allocatable :: bconfm(:)
!MSK      integer,allocatable :: newcfm(:)
!MSK      integer,allocatable :: newbcfm(:)
      integer,allocatable :: oldcfm(:)
!MSK      integer,allocatable :: oldcfm_nest(:)

!      bconfm   - Background concentrations file format (index)
!      newcfm   - New grid model concentrations file format (index)
!_NEST newbcfm  - New boundary condition concentrations file format (index)
!      oldcfm   - Old grid model concentrations file format (index)
!_NEST oldcfm_nest - Old FINE grid model concentrations file format (index)

!MSK start
!     nh1grid - netCDF control output of EMEP45 photochemistry
      character(len=256)  :: nh1gridfn  = ' '
      integer             :: nh1gridun  = 0
      logical             :: nh1gridfe= .false.
      real                :: nh1gridfv  = 0.
      integer             :: nh1gridfm  = 0

!     ic1grid - netCDF output of icon concentrations for RESTART
      character(len=256)  :: ic1gridfn  = ' '
      integer             :: ic1gridun  = 0
      logical             :: ic1gridfe= .false.
      real                :: ic1gridfv  = 0.
      integer             :: ic1gridfm  = 0

!     ic2grid - netCDF input of icon concentrations for RESTART
      character(len=256)  :: ic2gridfn  = ' '
      integer             :: ic2gridun  = 0
      logical             :: ic2gridfe= .false.
      real                :: ic2gridfv  = 0.
      integer             :: ic2gridfm  = 0

      integer             :: bcictype
      integer             :: restart
      real                :: scaleo3
      real                :: scalepm

!     bcictype - switch for BCON: constant value or 3-D file
!     restart  - switch for restart with ICON 3-D files
!     scaleo3  - scaling factor for O3 BCON values
!MSK end

!_NEST_Start:
      integer :: IMETHOD
      integer :: IZMETHOD

!     IMETHOD  - Horizontal interpolation method when writing to FINE grid
!     IZMETHOD - Vertical   interpolation method when writing to FINE grid
!_NEST_END.

      integer             :: gridai
      integer,allocatable :: gridas(:)
      integer,allocatable :: gridds(:)
      integer,allocatable :: gridvs(:)
      integer             :: griddds     
      integer             :: gridwds
      integer             :: gridps

!     gridai  - Grid model add to results indicator
!     gridas  - Grid model horisontal advection scheme (nc)
!     gridds  - Grid model horisontal diffusion scheme (nc)
!     gridvs  - Grid model vertical adv./diff.  scheme (nc)
!     griddds - Grid model dry deposition       scheme
!     gridwds - Grid model wet deposition       scheme
!     gridps  - Grid model photochemical        scheme

!MSK      real,allocatable :: c(:,:,:,:)
      double precision,allocatable :: c(:,:,:,:)
!_CITYDELTA_Start:
!MSK not used      real,allocatable :: delta_c(:,:,:,:)
!_CITYDELTA_End.
!MSK      real,allocatable :: dcdt(:,:,:,:)
      double precision,allocatable :: dcdt(:,:,:,:)

!_lhs_LSGRID_reduction_START_June_2011:
      real, allocatable :: lsrc_dcdt(:,:,:,:)
!_lhs_LSGRID_reduction_END_June_2011.      

!     c       - Concentration field (nc,nx,ny,nz) NEST: (nc,1-nbcx:nx+nbcx,1-nbcy:ny+nbcy,nz+nbcz)
!     delta_c - Delta concentration field (nc,nx,ny,nz)
!     dcdt    - Time derivative of C (nc,nx,ny,nz)

!MSK      real,allocatable :: d2cdx2(:,:)
      double precision,allocatable :: d2cdx2(:,:)
!MSK      real,allocatable :: d2cdy2(:,:)
      double precision,allocatable :: d2cdy2(:,:)
!MSK      real,allocatable :: excz(:,:,:)
      double precision,allocatable :: excz(:,:,:)

!     d2cdx2 - Second order partial derivative of C with respect to x (nx,ny)
!     d2cdy2 - Second order partial derivative of C with respect to y (nx,ny)
!     excz   - Exchange of mass vertically (nc,nx,ny)

!MSK not used      real,allocatable :: gcc(:,:,:)

! *** gcc - Ground concentration correction factors (nx,ny,nc)

!_NEST_Start:
      real,allocatable :: BC_old(:)

!MSK      real,allocatable :: BC(:,:,:,:)
      double precision,allocatable :: BC(:,:,:,:)

!MSK not used      real,allocatable :: delta_bc(:,:,:,:)
      real,allocatable :: BC_nest(:,:,:,:)
      real,allocatable :: IC_BC_nest(:,:,:,:)
      real,allocatable :: IC_C_nest(:,:,:,:)
      

! *** BC_old     - Background concentration (nc) 
! *** BC         - Boundary Conditions: (nc,1-nbcx:nx+nbcx,1-nbcy:ny+nbcy,nz+nbcz)
! *** delta_bc   - Delta background concentration (nc,1-nbcx:nx+nbcx,1-nbcy:ny+nbcy,nz+nbcz)
! *** BC_nest    - Nest grid background concentration: 
! ***             (nc,1-nbcx_nest:nx_nest+nbcx_nest,1-nbcy_nest:ny_nest+nbcy_nest,nz_nest+nbcz_nest)
! *** IC_BC_nest - Storing the initial BC's for the NEST-domain. (This array is calculated as the first
! ***              BC field and must be stored in a separate array in the first preprocessing timestep
! ***              in TSGRID so as to avoid loosing the data when the ordinary BC_nest data is 
! ***              calculated at the end of the hour in the postprocessing timestep of this routine.
! ***             ((nc,1-nbcx_nest:nx_nest+nbcx_nest,1-nbcy_nest:ny_nest+nbcy_nest,nz_nest+nbcz_nest)
! *** IC_C_nest  - Stores the internal values of IC_BC_nest (as long as the BC-fields contains data for the whole domain
! ***              and not just the boundary cells): (nc,nx_nest,ny_nest,nz_nest)

! *** Control variables for total model mass and volume and their fluxes: (nc)

        double precision, allocatable :: TOTMASS(:)
        double precision, allocatable :: TOTVOL(:)
        double precision, allocatable :: VOLAVG(:)
        double precision, allocatable :: WESTB_VF(:)
        double precision, allocatable :: WESTB_MF(:)
        double precision, allocatable :: NORTHB_VF(:)
        double precision, allocatable :: NORTHB_MF(:)
        double precision, allocatable :: EASTB_VF(:)
        double precision, allocatable :: EASTB_MF(:)
        double precision, allocatable :: SOUTHB_VF(:)
        double precision, allocatable :: SOUTHB_MF(:)
        double precision, allocatable :: BOTB_VF(:)
        double precision, allocatable :: BOTB_MF(:)
        double precision, allocatable :: TOPB_VF(:)
        double precision, allocatable :: TOPB_MF(:)
!_NEST_End.

      contains


      subroutine FreeGridMemory()

!DEC$ ATTRIBUTES DLLEXPORT,STDCALL,ALIAS:'FreeGridMemory' :: FreeGridMemory

      implicit none

      if (allocated(bconfn))       deallocate(bconfn)
!MSK      if (allocated(newcfn))       deallocate(newcfn)      
!MSK      if (allocated(newbcfn))      deallocate(newbcfn)
      if (allocated(oldcfn))       deallocate(oldcfn)      
!MSK      if (allocated(oldcfn_nest))  deallocate(oldcfn_nest)


      if (allocated(bconun))       deallocate(bconun)
!MSK      if (allocated(newcun))       deallocate(newcun)      
!MSK      if (allocated(newbcun))      deallocate(newbcun)
      if (allocated(oldcun))       deallocate(oldcun)      
!MSK      if (allocated(oldcun_nest))  deallocate(oldcun_nest)
     
      if (allocated(bconfe))       deallocate(bconfe)
!MSK      if (allocated(newcfe))       deallocate(newcfe)      
!MSK      if (allocated(newbcfe))      deallocate(newbcfe)
      if (allocated(oldcfe))       deallocate(oldcfe)     
!MSK      if (allocated(oldcfe_nest))  deallocate(oldcfe_nest)

      if (allocated(bconfv))       deallocate(bconfv)
!MSK      if (allocated(newcfv))       deallocate(newcfv)      
!MSK      if (allocated(newbcfv))      deallocate(newbcfv)
      if (allocated(oldcfv))       deallocate(oldcfv)
!MSK      if (allocated(oldcfv_nest))  deallocate(oldcfv_nest)

      if (allocated(bconfm))       deallocate(bconfm)
!MSK      if (allocated(newcfm))       deallocate(newcfm)      
!MSK      if (allocated(newbcfm))      deallocate(newbcfm)
      if (allocated(oldcfm))       deallocate(oldcfm)
!MSK      if (allocated(oldcfm_nest))  deallocate(oldcfm_nest)
      
      if (allocated(gridas))       deallocate(gridas)
      if (allocated(gridds))       deallocate(gridds)
      if (allocated(gridvs))       deallocate(gridvs)
      
      if (allocated(c))            deallocate(c)
!MSK not used      if (allocated(delta_c))      deallocate(delta_c)
      if (allocated(dcdt))         deallocate(dcdt)
!_lhs_LSGRID_reduction_START_June_2011:
      if (allocated(lsrc_dcdt))    deallocate(lsrc_dcdt)
!_lhs_LSGRID_reduction_END_June_2011.
      if (allocated(d2cdx2))       deallocate(d2cdx2)
      if (allocated(d2cdy2))       deallocate(d2cdy2)
      if (allocated(excz))         deallocate(excz)
      
!MSK not used      if (allocated(gcc))          deallocate(gcc)

      if (allocated(bc_old))       deallocate(bc_old)
      if (allocated(bc))           deallocate(bc)
!MSK not used      if (allocated(delta_bc))     deallocate(delta_bc)
      if (allocated(bc_nest))      deallocate(bc_nest)
      if (allocated(ic_bc_nest))   deallocate(ic_bc_nest)
      if (allocated(ic_c_nest))    deallocate(ic_c_nest)

      if (allocated(TOTMASS))      deallocate(TOTMASS)
      if (allocated(TOTVOL))       deallocate(TOTVOL)
      if (allocated(VOLAVG))       deallocate(VOLAVG)

      if (allocated(WESTB_VF))     deallocate(WESTB_VF)
      if (allocated(WESTB_MF))     deallocate(WESTB_MF)
      if (allocated(NORTHB_VF))    deallocate(NORTHB_VF)
      if (allocated(NORTHB_MF))    deallocate(NORTHB_MF)

      if (allocated(EASTB_VF))     deallocate(EASTB_VF)
      if (allocated(EASTB_MF))     deallocate(EASTB_MF)
      if (allocated(SOUTHB_VF))    deallocate(SOUTHB_VF)
      if (allocated(SOUTHB_MF))    deallocate(SOUTHB_MF)

      if (allocated(BOTB_VF))      deallocate(BOTB_VF)
      if (allocated(BOTB_MF))      deallocate(BOTB_MF)
      if (allocated(TOPB_VF))      deallocate(TOPB_VF)
      if (allocated(TOPB_MF))      deallocate(TOPB_MF)

      gridai = 0
      griddds = 0
      gridwds = 0
      gridps = 0

!     End of subroutine FreeGridMemory
      end subroutine

      end module mod_grid
