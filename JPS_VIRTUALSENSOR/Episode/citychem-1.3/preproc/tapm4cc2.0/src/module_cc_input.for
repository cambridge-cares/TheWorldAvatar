! <module_cc_input.for  - A component of the City-scale
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
!***********************************************************************
!***
!***      User meta info input file for EPISODE-CityChem
!***
!***********************************************************************

      module module_cc_input

!***********************************************************************
!***  Module module_cc_input declares variables and parameters
!***  for input/output interfaces of the CityChem extension.
!***********************************************************************

      implicit none

!***********************************************************************

!     Declarations:

! file units input
      integer             :: funit_run
      integer             :: funit_log
      integer             :: funit_in_points
      integer             :: funit_in_lines
      integer             :: funit_in_area_sector

! file units output
      integer             :: funit_out_points
      integer             :: funit_out_lines
      integer             :: funit_out_area_sector
      integer             :: funit_inpath_tapm
      integer             :: funit_inpath_cmaq
! file names

      character (len=256) :: fname_run
      character (len=256) :: fname_log
      character (len=256) :: fname_in_points
      character (len=256) :: fname_in_lines
      character (len=256) :: fname_in_area_sector

      character (len=256) :: fname_outpath
      character (len=256) :: fname_out_points
      character (len=256) :: fname_out_lines
      character (len=256) :: fname_out_area_sector

      character (len=256) :: fname_inpath_tapm
      character (len=256) :: fname_inpath_cmaq

! file exist flags

      logical             :: fe_log
      logical             :: fe_in_points
      logical             :: fe_in_lines
      logical             :: fe_in_area_sector
      logical             :: fe_outpath
      logical             :: fe_inpath_tapm
      logical             :: fe_inpath_cmaq

! i/o control
      integer             :: EP_fm
      integer             :: NC_out
      character(len=60)   :: startdate
      character(len=60)   :: enddate

!     Declarations:

      integer             :: hh
      integer             :: nx
      integer             :: ny
      integer             :: n_sopp
      integer             :: n_soll
      integer             :: n_soaa

      real                :: dxarea
      real                :: dxout
      real                :: sitex0
      real                :: sitey0


      character(len=38)   :: simid
      character(len=2 )   :: model 
      character(len=3 )   :: source
      character(len=8)    :: utmzone = ' '

!  Date and time declarations:

      integer             :: bdat(6)
      integer             :: edat(6)

      integer             :: year
      integer             :: mnth
      integer             :: daym
      integer             :: hour
      integer             :: minu
      integer             :: seco
      integer             :: dayweek
      integer             :: dayyear

!     Routines and Functions:

! ************
!      contains


! End of module module_cc_input

      end module module_cc_input
