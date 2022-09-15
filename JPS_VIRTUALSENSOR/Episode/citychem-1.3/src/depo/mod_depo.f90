! <mod_depo.f90 - A component of the City-scale
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

      module mod_depo

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

      character(len=256)             :: ddeplfn
      character(len=256),allocatable :: ddepmfn(:)
      character(len=256)             :: ddeprfn
      character(len=256),allocatable :: ddepsfn(:)
      character(len=256)             :: wdeplfn
      character(len=256),allocatable :: wdepmfn(:)
      character(len=256)             :: wdeprfn
      character(len=256),allocatable :: wdepsfn(:)

! ddeplfn - Line sources dry depositions file name
! ddepmfn - Main grid    dry depositions file name (nc)
! ddeprfn - Receptors    dry depositions file name
! ddepsfn - Sub  grid    dry depositions file name (nc)
! wdeplfn - Line sources wet depositions file name
! wdepmfn - Main grid    wet depositions file name (nc)
! wdeprfn - Receptors    wet depositions file name 
! wdepsfn - Sub  grid    wet depositions file name (nc)

      integer             :: ddeplun
      integer,allocatable :: ddepmun(:)
      integer             :: ddeprun
      integer,allocatable :: ddepsun(:)
      integer             :: wdeplun
      integer,allocatable :: wdepmun(:)
      integer             :: wdeprun
      integer,allocatable :: wdepsun(:)

! ddeplun - Line sources dry depositions fileunit
! ddepmun - Main grid    dry depositions fileunit (nc)
! ddeprun - Receptors    dry depositions fileunit
! ddepsun - Sub  grid    dry depositions fileunit (nc)
! wdeplun - Line sources wet depositions fileunit
! wdepmun - Main grid    wet depositions fileunit (nc)
! wdeprun - Receptors    wet depositions fileunit
! wdepsun - Sub  grid    wet depositions fileunit (nc)

      logical             :: ddeplfe
      logical,allocatable :: ddepmfe(:)
      logical             :: ddeprfe
      logical,allocatable :: ddepsfe(:)
      logical             :: wdeplfe
      logical,allocatable :: wdepmfe(:)
      logical             :: wdeprfe
      logical,allocatable :: wdepsfe(:)

! ddeplfe - Line sources dry depositions file exists
! ddepmfe - Main grid    dry depositions file exists (nc)
! ddeprfe - Receptors    dry depositions file exists
! ddepsfe - Sub  grid    dry depositions file exists (nc)
! wdeplfe - Line sources wet depositions file exists
! wdepmfe - Main grid    wet depositions file exists (nc)
! wdeprfe - Receptors    wet depositions file exists
! wdepsfe - Sub  grid    wet depositions file exists (nc)

      real             :: ddeplfv(1)
      real,allocatable :: ddepmfv(:)
      real             :: ddeprfv(1)
      real,allocatable :: ddepsfv(:)
      real             :: wdeplfv(1)
      real,allocatable :: wdepmfv(:)
      real             :: wdeprfv(1)
      real,allocatable :: wdepsfv(:)

! ddeplfv - Line sources dry depositions file value
! ddepmfv - Main grid    dry depositions file value (nc)
! ddeprfv - Receptors    dry depositions file value
! ddepsfv - Sub  grid    dry depositions file value (nc)
! wdeplfv - Line sources wet depositions file value
! wdepmfv - Main grid    wet depositions file value (nc)
! wdeprfv - Receptors    wet depositions file value
! wdepsfv - Sub  grid    wet depositions file value (nc)

      integer             :: ddeplfm
      integer,allocatable :: ddepmfm(:)
      integer             :: ddeprfm
      integer,allocatable :: ddepsfm(:)
      integer             :: wdeplfm
      integer,allocatable :: wdepmfm(:)
      integer             :: wdeprfm
      integer,allocatable :: wdepsfm(:)

! ddeplfm - Line sources dry depositions file format (index)
! ddepmdm - Main grid    dry depositions file format (index) (nc)
! ddeprfm - Receptors    dry depositions file format (index)
! ddepsfm - Sub  grid    dry depositions file format (index) (nc)
! wdeplfm - Line sources wet depositions file format (index)
! wdepmfm - Main grid    wet depositions file format (index) (nc)
! wdeprfm - Receptors    wet depositions file format (index)
! wdepsfm - Sub  grid    wet depositions file format (index) (nc)

      integer,allocatable :: ddepoi(:)
      integer,allocatable :: wdepoi(:)

! ddepoi - Dry deposition output indicator (nc)
! wdepoi - Wet deposition output indicator (nc)

      real,allocatable :: ddepv(:)
      real,allocatable :: wdepsr(:)

! ddepv  - Dry deposition velocities (nc)
! wdepsr - Wet deposition scavenging ratio (nc)

      real,allocatable :: ddepm(:,:,:)
      real,allocatable :: ddepr(:,:)
      real,allocatable :: ddeps(:,:,:,:)
      real,allocatable :: wdepm(:,:,:)
      real,allocatable :: wdepr(:,:)
      real,allocatable :: wdeps(:,:,:,:)

! ddepm - Main grid dry depositions field (nx,ny,nc)
! ddepr - Receptors dry depositions values (nc,nr)
! ddeps - Sub  grid dry depositions field (nc,nxs,nys,nls)
! wdepm - Main grid wet depositions field (nx,ny,nc)
! wdepr - Receptors wet depositions values (nc,nr)
! wdeps - Sub  grid wet depositions field (nc,nxs,nys,nls)

!_LHS_08_Oct_2004_Start:
!_Deposition:
!
!    The Dry-Deposition velocities above are defined as a constant for
!    each compound. For a more general treatment we need to define
!    spatially variable  (2-dimensional) values.
!    Is this necessary for the wet scavenging ratios ???

      real,allocatable :: ddepv2d(:,:,:)
      real,allocatable :: wdeprate2d(:,:,:)

! ddepv2d    - Two-dimensional Dry-Deposition velocities (nc,nx,ny)
! wdeprate2d  - Two-dimensional Wet-Deposition velocities (nc,nx,ny)
!
!_LHS_08_Oct_2004_End.

!_LHS_EIFAir_change_Start:
      real,allocatable :: ddeprl(:,:)
      real,allocatable :: wdeprl(:,:)

! ddeprl - Line sources receptor points dry depositions (nc,nrl)
! wdeprl - Line sources receptor points wet depositions (nc,nrl)
!_LHS_EIFAir_change_End.

      contains


      subroutine FreeDepoMemory()

!DEC$ ATTRIBUTES DLLEXPORT,STDCALL,ALIAS:'FreeDepoMemory' :: FreeDepoMemory

      implicit none

      if (allocated(ddepmfn)) deallocate(ddepmfn)
      if (allocated(ddepsfn)) deallocate(ddepsfn)
      if (allocated(wdepmfn)) deallocate(wdepmfn)
      if (allocated(wdepsfn)) deallocate(wdepsfn)
      if (allocated(ddepmun)) deallocate(ddepmun)
      if (allocated(ddepsun)) deallocate(ddepsun)
      if (allocated(wdepmun)) deallocate(wdepmun)
      if (allocated(wdepsun)) deallocate(wdepsun)
      if (allocated(ddepmfe)) deallocate(ddepmfe)
      if (allocated(ddepsfe)) deallocate(ddepsfe)
      if (allocated(wdepmfe)) deallocate(wdepmfe)
      if (allocated(wdepsfe)) deallocate(wdepsfe)
      if (allocated(ddepmfv)) deallocate(ddepmfv)
      if (allocated(ddepsfv)) deallocate(ddepsfv)
      if (allocated(wdepmfv)) deallocate(wdepmfv)
      if (allocated(wdepsfv)) deallocate(wdepsfv)
      if (allocated(ddepmfm)) deallocate(ddepmfm)
      if (allocated(ddepsfm)) deallocate(ddepsfm)
      if (allocated(wdepmfm)) deallocate(wdepmfm)
      if (allocated(wdepsfm)) deallocate(wdepsfm)
      if (allocated(ddepoi))  deallocate(ddepoi)
      if (allocated(wdepoi))  deallocate(wdepoi)
      if (allocated(ddepv))   deallocate(ddepv)
      if (allocated(wdepsr))  deallocate(wdepsr)
      if (allocated(ddepm))   deallocate(ddepm)
      if (allocated(wdepm))   deallocate(wdepm)
      if (allocated(ddepr))   deallocate(ddepr)
      if (allocated(wdepr))   deallocate(wdepr)
      if (allocated(ddeps))   deallocate(ddeps)
      if (allocated(wdeps))   deallocate(wdeps)
!_LHS_08_Oct_2004_Start.
      if (allocated(ddepv2d))     deallocate(ddepv2d)
      if (allocated(wdeprate2d))   deallocate(wdeprate2d)
!_LHS_08_Oct_2004_End.
!_LHS_EIFAir_change_Start:      
      if (allocated(ddeprl))  deallocate(ddeprl)
      if (allocated(wdeprl))  deallocate(wdeprl)
!_LHS_EIFAir_change_End.

      ddeplfn = ' '
      ddeprfn = ' '
      wdeplfn = ' '
      wdeprfn = ' '
      ddeplun = 0
      ddeprun = 0
      wdeplun = 0
      wdeprun = 0
      ddeplfe = .false.
      ddeprfe = .false.
      wdeplfe = .false.
      wdeprfe = .false.
      ddeplfv = 0.
      ddeprfv = 0.
      wdeplfv = 0.
      wdeprfv = 0.
      ddeplfm = 0
      ddeprfm = 0
      wdeplfm = 0
      wdeprfm = 0

! End of subroutine FreeDepoMemory

      end subroutine

      end module mod_depo
