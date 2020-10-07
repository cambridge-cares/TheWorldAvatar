! <mod_stat.f90 - A component of the City-scale
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

      module mod_stat

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
!           2017  M. Karl: file operators for netcdf statistic output
!
! ----------------------------------------------------------------------------------

      character(len=256)             :: lstafn
      character(len=256),allocatable :: sstafn(:)

!MSK start
      character(len=256)             :: nc1stafn
      character(len=256)             :: nc2stafn
      character(len=256)             :: nc3stafn
      character(len=256)             :: nd1stafn
      character(len=256)             :: nd2stafn
      character(len=256)             :: nd3stafn
!MSK end


! lstafn - Line sources statistics filename
! mstafn - Main grid    statistics filename (nc)
! rstafn - Receptors    statistics filename
! sstafn - Sub  grid    statistics filename (nc)
! mastafn - Main grid overall statistics filename (nc)
! rastafn - Receptors overall statistics filename (nc)

      integer             :: lstaun
      integer,allocatable :: sstaun(:)

! lstaun - Line sources statistics fileunit 
! mstaun - Main grid    statistics fileunit (nc)
! rstaun - Receptors    statistics fileunit
! sstaun - Sub  grid    statistics fileunit (nc)
! mastaun - Main grid overall statistics fileunit (nc)
! rastaun - Receptors overall statistics fileunit (nc)

      logical             :: lstafe

!MSK start
      logical             :: nc1stafe
      logical             :: nc2stafe 
      logical             :: nc3stafe 
      logical             :: nd1stafe
      logical             :: nd2stafe 
      logical             :: nd3stafe 
!MSK end

      logical,allocatable :: sstafe(:)


! lstafe - Line sources statistics file exists 
! mstafe - Main grid    statistics file exists (nc)
! rstafe - Receptors    statistics file exists
! sstafe - Sub  grid    statistics file exists (nc)
! mastafe - Main grid overall statistics file exists (nc)
! rastafe - Receptors overall statistics file exists (nc)

      real             :: lstafv(1)
      real,allocatable :: sstafv(:)

!MSK start
      real             :: nc1stafv(1)
      real             :: nc2stafv(1)
      real             :: nc3stafv(1)
      real             :: nd1stafv(1)
      real             :: nd2stafv(1)
      real             :: nd3stafv(1)
!MSK end

! lstafv - Line sources statistics file value
! mstafv - Main grid    statistics file value (nc)
! rstafv - Receptors    statistics file value
! sstafv - Sub  grid    statistics file value (nc)
! mastafv - Main grid overall statistics file value (nc)
! rastafv - Receptors overall statistics file value (nc)

      integer             :: lstafm
      integer,allocatable :: sstafm(:)

!MSK start
      integer             :: nc1stafm
      integer             :: nc2stafm
      integer             :: nc3stafm
      integer             :: nd1stafm
      integer             :: nd2stafm
      integer             :: nd3stafm
!MSK end

! lstafm - Line sources statistics file format (index)
! mstafm - Main grid    statistics file format (index) (nc)
! rstafm - Receptors    statistics file format (index)
! sstafm - Sub  grid    statistics file format (index) (nc)
! mastafm - Main grid overall statistics file format (index) (nc)
! rastafm - Receptors overall statistics file format (index) (nc)

!MSK      integer :: icstat
     
      integer :: iavea
      integer :: iaved
      integer :: idsta
      integer :: navea
      integer :: naved
      integer :: nhighd
      integer :: nhighh
!MSK start
      integer :: naveh
!MSk end

! icstat - Index of compound for statistics and exposure calculations.

! iavea  - Calculate overall average    indicator
! iaved  - Calculate daily   average    indicator
! idsta  - Calculate daily   statistics indicator
! navea  - Number of values for overall average
! naved  - Number of values for daily   average
! nhighd - Actual number of highest daily  values
! nhighh - Actual number of highest hourly values

!MSK      real,allocatable :: cmavea(:,:,:)
!MSK      real,allocatable :: cmaved(:,:,:)
      double precision,allocatable :: cmavea(:,:,:)
      double precision,allocatable :: cmaved(:,:,:)

! cmavea - Main grid average overall field values (nx,ny)
! cmaved - Main grid average daily   field values (nx,ny)

!MSK      real,allocatable :: cravea(:,:)
!MSK      real,allocatable :: craved(:,:)
      double precision,allocatable :: cravea(:,:)
      double precision,allocatable :: craved(:,:)

! cravea - Receptor points average overall values (nr)
! craved - Receptor points average daily   values (nr)

!MSK      real,allocatable :: clavea(:)
!MSK      real,allocatable :: claved(:)
      double precision,allocatable :: clavea(:)
      double precision,allocatable :: claved(:)

! clavea - Line receptor points average overall values (nql)
! claved - Line receptor points average daily   values (nql)

      real,   allocatable :: cmhighd(:,:,:)
      real,   allocatable :: cmhighh(:,:,:)
      integer,allocatable :: imhighdmin(:,:)
      integer,allocatable :: imhighhmin(:,:)

! cmhighd    - Main grid highest daily  field values (nhighd,nx,ny)
! cmhighh    - Main grid highest hourly field values (nhighh,nx,ny)
! imhighdmin - Minimum values in cmhighd indices (nx,ny)
! imhighhmin - Minimum values in cmhighh indices (nx,ny)

      real,   allocatable :: crhighd(:,:)
      real,   allocatable :: crhighh(:,:)
      integer,allocatable :: irhighdmin(:)
      integer,allocatable :: irhighhmin(:)

! crhighd    - Receptor points highest daily  values (nhighd,nr)
! crhighh    - Receptor points highest hourly values (nhighh,nr)
! irhighdmin - Minimum values in crhighd indices (nr)
! irhighhmin - Minimum values in crhighh indices (nr)

      real,   allocatable :: clhighd(:,:)
      real,   allocatable :: clhighh(:,:)
      integer,allocatable :: ilhighdmin(:)
      integer,allocatable :: ilhighhmin(:)

! clhighd    - Line receptor points highest daily  values (nhighd,nql)
! clhighh    - Line receptor points highest hourly values (nhighh,nql)
! ilhighdmin - Minimum values in clhighd indices (nql)
! ilhighhmin - Minimum values in clhighh indices (nql)
!_LHS_EIFAir_change_Start:
      real,allocatable :: wmtota(:,:,:)

! wmtota - Main grid total overall deposition field values (nx,ny,nc)

      real,allocatable :: wrtota(:,:)

! wrtota - Receptor points total overall deposition values (nc,nr)
!_LHS_EIFAir_change_End.
      contains


      subroutine FreeStatMemory()

      implicit none

      if (allocated(sstafn))     deallocate(sstafn)
      if (allocated(sstaun))     deallocate(sstaun)
      if (allocated(sstafe))     deallocate(sstafe)
      if (allocated(sstafv))     deallocate(sstafv)
      if (allocated(sstafm))     deallocate(sstafm)

      if (allocated(cmavea))     deallocate(cmavea)
      if (allocated(cmaved))     deallocate(cmaved)
      
      if (allocated(cravea))     deallocate(cravea)
      if (allocated(craved))     deallocate(craved)

      if (allocated(clavea))     deallocate(clavea)
      if (allocated(claved))     deallocate(claved)

      if (allocated(cmhighd))    deallocate(cmhighd)
      if (allocated(cmhighh))    deallocate(cmhighh)
      if (allocated(imhighdmin)) deallocate(imhighdmin)
      if (allocated(imhighhmin)) deallocate(imhighhmin)      
      
      if (allocated(crhighd))    deallocate(crhighd)
      if (allocated(crhighh))    deallocate(crhighh)
      if (allocated(irhighdmin)) deallocate(irhighdmin)
      if (allocated(irhighhmin)) deallocate(irhighhmin)     

      if (allocated(clhighd))    deallocate(clhighd)
      if (allocated(clhighh))    deallocate(clhighh)
      if (allocated(ilhighdmin)) deallocate(ilhighdmin)
      if (allocated(ilhighhmin)) deallocate(ilhighhmin)

      if (allocated(wmtota))     deallocate(wmtota)
      if (allocated(wrtota))     deallocate(wrtota)

      
! End of subroutine FreeStatMemory

      end subroutine FreeStatMemory

      end module mod_stat
