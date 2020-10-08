! <mod_conc.f90 - A component of the City-scale
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

      module mod_conc

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

      integer mc

      character(len=256)             :: lconfn    = ' '
!MSK      character(len=256),allocatable :: mconfn(:)
!MSK start
      character(len=256)             :: nh1confn  = ' '
!MSK end
      character(len=256)             :: rconfn    = ' '
      character(len=256),allocatable :: sconfn(:)
!_NEST_Start:
      character(len=256),allocatable :: merrfn(:)
      character(len=256),allocatable :: manafn(:)

!     lconfn - Line sources      concentrations filename
!     mconfn - Main grid         concentrations filename (nc) 
!     rconfn - Receptors         concentrations filename
!     sconfn - Sub  grid         concentrations filename (nc)
!     merrfn - Main grid error   concentrations filename (nc)
!     manafn - Main grid analyt. concentrations filename (nc)
!_NEST_End.

      integer             :: lconun    = 0
!MSK      integer,allocatable :: mconun(:)
!MSK start
      integer             :: nh1conun  = 0
!MSK end
      integer             :: rconun    = 0
      integer,allocatable :: sconun(:)
!_NEST_Start:
      integer,allocatable :: merrun(:)
      integer,allocatable :: manaun(:)
!_NEST_End.

!     lconun - Line sources      concentrations fileunit 
!     mconun - Main grid         concentrations fileunit (nc)
!     rconun - Receptors         concentrations fileunit
!     sconun - Sub  grid         concentrations fileunit (nc)
!_NEST_Start:
!     merrun - Main grid error   concentrations fileunit (nc)
!     manaun - Main grid analyt. concentrations fileunit (nc)
!_NEST_End.

      logical             :: lconfe = .false.
!MSK      logical,allocatable :: mconfe(:)
!MSK start
      logical             :: nh1confe= .false.
!MSK end
      logical             :: rconfe = .false.
      logical,allocatable :: sconfe(:)
!_NEST_Start:
      logical,allocatable :: merrfe(:)
      logical,allocatable :: manafe(:)
!_NEST_end.

!     lconfe - Line sources      concentrations file exists 
!     mconfe - Main grid         concentrations file exists (nc)
!     rconfe - Receptors         concentrations file exists
!     sconfe - Sub  grid         concentrations file exists (nc)
!_NEST_Start:
!     merrfe - Main grid error   concentrations file exists (nc)
!     manafe - Main grid analyt. concentrations file exists (nc)
!_NEST_end.

      real             :: lconfv(1) = 0.
!MSK      real,allocatable :: mconfv(:)
!MSK start
      real             :: nh1confv  = 0.
!MSK end
      real             :: rconfv(1) = 0.
      real,allocatable :: sconfv(:)
!_NEST_Start:
      real,allocatable :: merrfv(:)
      real,allocatable :: manafv(:)
!_NEST_End.

!     lconfv - Line sources      concentrations file value
!     mconfv - Main grid         concentrations file value (nc)
!     rconfv - Receptors         concentrations file value
!     sconfv - Sub  grid         concentrations file value (nc)
!_NEST_Start:
!     merrfv - Main grid error   concentrations file value (nc)
!     manafv - Main grid analyt. concentrations file value (nc)
!_NEST_End.

      integer             :: lconfm    = 0
!MSK      integer,allocatable :: mconfm(:)
!MSK start
      integer             :: nh1confm  = 0
!MSK end
      integer             :: rconfm    = 0
      integer,allocatable :: sconfm(:)
!_NEST_Start:
      integer,allocatable :: merrfm(:)
      integer,allocatable :: manafm(:)
!_NEST_End.

!     lconfm - Line sources      concentrations file format (index)
!     mconfm - Main grid         concentrations file format (index) (nc)
!     rconfm - Receptors         concentrations file format (index)
!     sconfm - Sub  grid         concentrations file format (index) (nc)
!_NEST_Start:
!     merrfm - Main grid error   concentrations file format (index) (nc)
!     manafm - Main grid analyt. concentrations file format (index) (nc)
!_NEST_End.


      integer,allocatable :: icunit(:)
      integer,allocatable :: icmpnd(:)


!     icmpnd - Integer compound name (nc)
!     icunit - Integer compound unit (nc)

!      character(len=256),allocatable :: cmpnd(:)
!      character(len=256),allocatable :: cunit(:)
!MSK changed to char size 10
      character(len=10),allocatable :: cmpnd(:)
      character(len=10),allocatable :: cunit(:)


!     cmpnd - Compound name (nc)
!     cunit - Compound unit (nc)

      real,allocatable :: cmolw(:)
      real,allocatable :: thalf(:)

!     cmolw - Compound molecular weight (nc)
!     thalf - Radioactive half-life time (nc)

      integer,allocatable :: cout(:)

!     cout - Compound output indicator (nc)

      integer :: nc = 0

!     nc              - Number of compounds

!_LHS_SOA_June_2007_Start:
      integer :: n_nochem        ! = 88 - 68 = 20
      integer :: n_advect        ! = 6

      integer :: ic_org_PM2p5    ! = 69
      integer :: ic_aerOM        ! = ic_org_PM2p5 + n_advect = 69 + 6 = 75
      integer :: ic_aerOM_C      ! = ic_aerOM   + 1 = 76
      integer :: ic_aerPOM       ! = ic_aerOM_C + 1 = 77
      integer :: ic_aerPOM_C     ! = 78
      integer :: ic_aerWOOD      ! = 79
      integer :: ic_aerWOOD_C    ! = 80    
      integer :: ic_aerFFUEL     ! = 81
      integer :: ic_aerFFUEL_C   ! = 82
      integer :: ic_aerBGND      ! = 83
      integer :: ic_aerBGND_C    ! = 84
      integer :: ic_aerASOA      ! = 85
      integer :: ic_aerASOA_C    ! = 86
      integer :: ic_aerBSOA      ! = 87
      integer :: ic_aerBSOA_C    ! = 88


!     n_nochem        - Number of additional compounds not 
!                       explicitely treated in the chemistry scheme.
!     n_advect        - Number of the additional compounds that are
!                       advected.
!
!_LHS_SOA_June_2007_End.

!_LHS_Hourly_Averaged_Output_December_2007_Start:
      integer :: i_nts
      logical :: averaged_output
!_LHS_Hourly_Averaged_Output_December_2007_End.

!MSK      real,allocatable :: cm(:,:,:)
!MSK      real,allocatable :: cr(:,:)
!MSK      real,allocatable :: cs(:,:,:,:)
      double precision,allocatable :: cm(:,:,:)
      double precision,allocatable :: cr(:,:)

!     cm   - Main grid concentration field (nx,ny,nc)
!     cr   - Receptors concentration data (nc,nr)
!     cs   - Sub  grid concentration field (nc,nxs,nys,nls)

!MSK      real,allocatable :: crl(:,:)
      double precision,allocatable :: crl(:,:)
!     crl - Line sources receptor points concentrations (nc,nrl)

!_NEST_Start:

      real,allocatable :: c_an(:,:,:)
      real,allocatable :: c_err(:,:,:)

!     c_an  - Main grid analytical concentration field
!     c_err - Main grid error field.

!_NEST_End.

      contains


      subroutine FreeConcMemory()

!DEC$ ATTRIBUTES DLLEXPORT,STDCALL,ALIAS:'FreeConcMemory' :: FreeConcMemory

      implicit none

!MSK      if (allocated(mconfn)) deallocate(mconfn)
      if (allocated(sconfn)) deallocate(sconfn)
      if (allocated(merrfn)) deallocate(merrfn)
      if (allocated(manafn)) deallocate(manafn)
!MSK      if (allocated(mconun)) deallocate(mconun)
      if (allocated(sconun)) deallocate(sconun)
      if (allocated(merrun)) deallocate(merrun)
      if (allocated(manaun)) deallocate(manaun)
!MSK      if (allocated(mconfe)) deallocate(mconfe)
      if (allocated(sconfe)) deallocate(sconfe)
      if (allocated(merrfe)) deallocate(merrfe)
      if (allocated(manafe)) deallocate(manafe)
!MSK      if (allocated(mconfv)) deallocate(mconfv)
      if (allocated(sconfv)) deallocate(sconfv)
      if (allocated(merrfv)) deallocate(merrfv)
      if (allocated(manafv)) deallocate(manafv)
!MSK      if (allocated(mconfm)) deallocate(mconfm)
      if (allocated(sconfm)) deallocate(sconfm)
      if (allocated(merrfm)) deallocate(merrfm)
      if (allocated(manafm)) deallocate(manafm)  

      if (allocated(icunit)) deallocate(icunit)      
      if (allocated(icmpnd)) deallocate(icmpnd)
      if (allocated(cmpnd))  deallocate(cmpnd)
      if (allocated(cunit))  deallocate(cunit)
      if (allocated(cmolw))  deallocate(cmolw)
      if (allocated(thalf))  deallocate(thalf)
      if (allocated(cout))   deallocate(cout)
      
      if (allocated(cm))     deallocate(cm)
      if (allocated(cr))     deallocate(cr)
!MSK      if (allocated(cs))     deallocate(cs)
      if (allocated(crl))    deallocate(crl)
      
      if (allocated(c_an))   deallocate(c_an)
      if (allocated(c_err))  deallocate(c_err)

      mc = 0
      lconfn = ' '
      rconfn = ' '
      lconun = 0
      rconun = 0
      lconfe = .false.
      rconfe = .false.
      lconfv = 0
      rconfv = 0
      lconfm = 0
      rconfm = 0
      nc = 0
      
!     End of subroutine FreeConcMemory
      end subroutine

      end module mod_conc
