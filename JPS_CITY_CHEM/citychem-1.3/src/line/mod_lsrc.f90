! <mod_lsrc.f90 - A component of the City-scale
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

      module mod_lsrc

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

      integer :: mql
      integer :: mqll
      integer :: mrl

      character(len=256) :: lsrsfn
!MSK      character(len=256) :: lsrvfn
      character(len=256),allocatable :: lsrvfn(:)

! lsrsfn - Line sources static   data file name
! lsrvfn - Line sources variable data file name

      integer :: lsrsun
!MSK      integer :: lsrvun
      integer,allocatable    :: lsrvun(:)

! lsrsun - Line sources static   data file unit
! lsrvun - Line sources variable data file unit

      logical :: lsrsfe
!MSK      logical :: lsrvfe
      logical,allocatable :: lsrvfe(:)

! lsrsfe - Line sources static   data file exists
!lsrvfe - Line sources variable data file exists

      real :: lsrsfv(1)
!MSK      real :: lsrvfv(1)
      real,allocatable :: lsrvfv(:)

! lsrsfv - Line sources static   data file value
! lsrvfv - Line sources variable data file value

      integer :: lsrsfm
!MSK      integer :: lsrvfm
      integer,allocatable :: lsrvfm(:)

! lsrsfm - Line sources static   data file format (index)
! lsrvfm - Line sources variable data file format (index)

      real,allocatable :: ql(:,:,:)
      real,allocatable :: qll(:)
      real,allocatable :: qlrmax(:)
      real,allocatable :: qlrmin(:)
      real,allocatable :: qlsw0(:)
      real,allocatable :: qlwredfac(:)
      real,allocatable :: qlx(:,:)
      real,allocatable :: qly(:,:)
      real,allocatable :: qlz(:,:)
      real,allocatable :: qlw(:)
      real,allocatable :: qltmax(:)

! ql        - Line sources total emissions (nc,nql,nqll)
! qll       - Line sources lengths (nql)
! qlrmax    - Line sources maximum distances (nql)
! qlrmin    - Line sources minimum distances (nql)
! qlsw0     - Line sources initial turbulence sigma-w (nql)
! qlwredfac - Line sources wet reduction factor (nql)
! qlx       - Line sources end points x-coordinates (nql,2)
! qly       - Line sources end points y-coordinates (nql,2)
! qlz       - Line sources end points z-coordinates (nql,2)
! qlw       - Line sources widths (nql)
! qltmax    - Line sources maximum influence time (nql)


      integer :: nql
      integer :: nqll

! nql  - Number of line sources
! nqll - Number of line sources lanes

      real  :: qlrmindist
      real  :: qlridist
      
! qlrmindist - Minimum distance (m) from receptor to line source;           Calculated as: MAX(DM,RMINRV + WIDTH/2.)
! qlridist   - Distance (m) from line associates recp point to line source; Calculated as: QLW(IQL)/2. + qlridist

      real,    allocatable :: qlrd(:)
      integer, allocatable :: qlri(:)
      real,    allocatable :: xrl(:)
      real,    allocatable :: yrl(:)
      real,    allocatable :: zrl(:)

! qlrd   - Line sources receptor points distance to line source (nrl)
! qlri   - Line sources receptor points indicators (0/1)
! xrl    - Line sources receptor points x-coordinates (nrl)
! yrl    - Line sources receptor points y-coordinates (nrl)
! zrl    - Line sources receptor points z-coordinates (nrl)

      integer :: nrl
      integer :: nrlp

! nrl  - Number of line sources receptor points (2*nql)
! nrlp - Number of line sources receptor points with positive distance

      integer :: lsrcai

! lsrcai - Line sources subgrid model add to results indicator

      real :: lsrcffmin

! lsrcffmin - Line sources subgrid model minimum windspeed

!MSK start
      real :: lsrcscale
! lsrcscale - Line source subgrid model emission scaling

      integer :: lsrccanyon
! lsrccanyon - Line source subgrid model street canyon option

      logical :: lonce
! lonce - set true after first reading header of line source emission file   

      logical :: use_timescale_no = .false.
! use_timescale_no - always false because scaling of NO emission seems arbitrary
! and the effect of NO-scaling on NOx concentrations is very small. 

!MSK end

      contains


      subroutine FreeLsrcMemory()

!DEC$ ATTRIBUTES DLLEXPORT,STDCALL,ALIAS:'FreeLsrcMemory' :: FreeLsrcMemory

      implicit none

      if (allocated(ql))        deallocate(ql)
      if (allocated(qll))       deallocate(qll)
      if (allocated(qlrmax))    deallocate(qlrmax)
      if (allocated(qlrmin))    deallocate(qlrmin)
      if (allocated(qlsw0))     deallocate(qlsw0)
      if (allocated(qlwredfac)) deallocate(qlwredfac)
      if (allocated(qlx))       deallocate(qlx)
      if (allocated(qly))       deallocate(qly)
      if (allocated(qlz))       deallocate(qlz)
      if (allocated(qlw))       deallocate(qlw)
      if (allocated(qltmax))    deallocate(qltmax)
      if (allocated(qlrd))      deallocate(qlrd)
      if (allocated(qlri))      deallocate(qlri)
      if (allocated(xrl))       deallocate(xrl)
      if (allocated(yrl))       deallocate(yrl)
      if (allocated(zrl))       deallocate(zrl)
!MSK
      if (allocated(lsrvun))    deallocate(lsrvun)
      if (allocated(lsrvfn))    deallocate(lsrvfn)
      if (allocated(lsrvfe))    deallocate(lsrvfe)
      if (allocated(lsrvfv))    deallocate(lsrvfv)
      if (allocated(lsrvfm))    deallocate(lsrvfm)

      mql = 0
      mqll = 0
      lsrsfn = ' '
      lsrvfn = ' '
      lsrsun = 0
      lsrvun = 0
      lsrsfe = .false.
      lsrvfe = .false.
      lsrsfv = 0.
      lsrvfv = 0.
      lsrsfm = 0
      lsrvfm = 0
      nql = 0
      nqll = 0
      nrl = 0
      nrlp = 0
      lsrcai = 0
      lsrcffmin = 0.
      
! End of subroutine FreeLsrcMemory

      end subroutine

      end module mod_lsrc
