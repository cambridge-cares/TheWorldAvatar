! <mod_asrc.f90 - A component of the City-scale
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

      module mod_asrc

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
!           2017  M. Karl: netCDF control output of area source emission
!
! ----------------------------------------------------------------------------------

      integer :: ma

      character(len=256),allocatable :: asrcfn(:,:)
!     asrcfn - Area sources filename (nc,na)

      integer,allocatable :: asrcun(:,:)
!     asrcun - Area sources fileunit (nc,na)

      logical,allocatable :: asrcfe(:,:)
!     asrcfe - Area sources file exists (nc,na)

      real,allocatable :: asrcfv(:,:)
!     asrcfv - Area sources file value (nc,na)

      integer,allocatable :: asrcfm(:,:)
!     asrcfm - Area sources file format (index) (nc,na)

!MSK start
!     nh1asrc - netCDF control output of area source emission
      character(len=256)  :: nh1asrcfn  = ' '
      integer             :: nh1asrcun  = 0
      logical             :: nh1asrcfe= .false.
      real                :: nh1asrcfv  = 0.
      integer             :: nh1asrcfm  = 0
!MSK end

      real,allocatable :: qaorig(:,:,:,:)
!     qaorig - Area sources emission fields (original data) (nc,nx,ny,na)

      real,allocatable :: qa(:,:,:,:)
!     qa - Area sources emission fields (for grid model) (nc,nx,ny,nz)

      integer :: na
!     na - Number of area sources

      integer :: asrcai
!     asrcai - Area sources subgrid model add to results indicator

      real :: asrcffmin
!     asrcffmin - Area sources subgrid model minimum windspeed

      real,allocatable :: afld(:,:)
!     afld - Emission field (nx,ny)

!_LHS_ASRC_Distribution_May2011_Start:
! *** Variables applied in the vertical and temporal distribution of the area sources into the 3D model grid:

      integer :: iweeknr
! *** weeknr - Week number (1 to 52). NOTE: Should be given as input, is now HARDCODED in subroutine CASRC.      
      real, allocatable :: hourf(:,:,:)
      real, allocatable :: daywf(:,:)
      real, allocatable :: weekyearf(:,:)

! *** hourf     - Hour factor; hourf(1:24,1,1:na) = working day; hourf(1:24,2,1:na) = weekend 
! *** daywf     - Day of week factor(1:7,1:na)
! *** weekyearf - Week of year factor(1:52,1:na)

      real,    allocatable  :: tdistr(:)
      logical, allocatable  :: asrc_tv(:)
! *** tdistr(1:na)   - The total hourly time variation value for each of the "na" sectors.
! *** iasrc_tv(1:na) - "iasrc_tv(ia)" = .TRUE. if emissions from Sector "ia" varies temporally.

      real,    allocatable :: vdistr(:,:)
! *** vdistr(nz,1:na)   - The vertical distribution over the "nz" layers for each of the "na" sectors.

! *** The "const_scaling(na)" array is to be applied if we need a constant scaling 
! *** of some  compound or sector:
      real, allocatable  :: const_scaling(:,:)
      real, allocatable  :: scale_sector(:)


      contains
      
! ***********************************************************************************************************

      subroutine FreeAsrcMemory()

!DEC$ ATTRIBUTES DLLEXPORT,STDCALL,ALIAS:'FreeAsrcMemory' :: FreeAsrcMemory

      implicit none

      if (allocated(asrcfn))    deallocate(asrcfn)
      if (allocated(asrcun))    deallocate(asrcun)
      if (allocated(asrcfe))    deallocate(asrcfe)
      if (allocated(asrcfv))    deallocate(asrcfv)
      if (allocated(asrcfm))    deallocate(asrcfm)
      if (allocated(qaorig))    deallocate(qaorig)
      if (allocated(qa))        deallocate(qa)
      if (allocated(afld))      deallocate(afld)

      if (allocated(hourf))     deallocate(hourf)
      if (allocated(daywf))     deallocate(daywf)
      if (allocated(weekyearf)) deallocate(weekyearf)
      
      if (allocated(tdistr))    deallocate(tdistr)
      if (allocated(asrc_tv))   deallocate(asrc_tv)

      if (allocated(vdistr))        deallocate(vdistr)
      if (allocated(const_scaling)) deallocate(const_scaling)
      if (allocated(scale_sector))  deallocate(scale_sector)
      
      ma = 0
      na = 0
      asrcai = 0
      asrcffmin = 0

!     End of subroutine FreeAsrcMemory
      end subroutine

      end module mod_asrc
