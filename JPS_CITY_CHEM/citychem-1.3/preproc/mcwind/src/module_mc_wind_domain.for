! <module_mc_wind_domain.for - A component of the City-scale
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
!DEC$ FIXEDFORMLINESIZE: 132
!***********************************************************************

      module module_mc_wind_domain

!     This module contains the declaration of the variables defining
!     the model domain.

! Modifications:
!    29 Jan 2019  M. Karl: replaced real*8 by double precision
!
!***********************************************************************

!***  Unique simulation ID:
      character (len=256) :: sim_id

!***  Sideral day frequency:
      real, parameter :: sid_freq = 0.7292E-4

      integer :: im
      integer :: jm
      integer :: km

      real :: clon,clat
      real :: fc
      real :: dx
      real :: dy

      real :: first_layer_depth
      real :: stretch_factor

      double precision :: h0

!     The variable "elev_max" is just used when testing with
!     an idealized topography:

      real    :: elev_max

!     The name of the domain. This name is used to distinguish
!     various domain-specific inputs:
      character (len=256) :: domain_name

!     Applied 1D-arrays (vectors):

!_LHS_Aug2007_Start:
      real, allocatable   :: user_def_deltasigma(:)
!_LHS_Aug2007_End.

      double precision, allocatable :: sigma(:)
      double precision, allocatable :: sigmal(:)
      double precision, allocatable :: delta_sigma(:)
      double precision, allocatable :: delta_sigmal(:)

      double precision, allocatable :: h0msig(:)
      double precision, allocatable :: h0msigl(:)

! *** Applied 2D-arrays:

      real, allocatable :: in_top(:,:)
      real, allocatable :: topo(:,:)
      real, allocatable :: topou(:,:)
      real, allocatable :: topov(:,:)

! *** Double precissions for the following model variables:

      double precision, allocatable :: depth(:,:)
      double precision, allocatable :: depthu(:,:)
      double precision, allocatable :: depthv(:,:)

      double precision, allocatable :: dD_dx(:,:)
      double precision, allocatable :: dD_dy(:,:)

      double precision, allocatable :: dRD_dx(:,:)
      double precision, allocatable :: dRD_dy(:,:)
      double precision, allocatable :: d2RD_dxx(:,:)
      double precision, allocatable :: d2RD_dyy(:,:)

      contains

! *** DOMAIN MODULE INTERFACE ROUTINES


!*******************************************************************************************

      subroutine call_construct_domain
!DEC$ ATTRIBUTES DLLEXPORT,STDCALL,ALIAS:'call_construct_domain' :: call_construct_domain
      implicit none

      call construct_domain

      return

      end subroutine call_construct_domain

!*******************************************************************************************

      subroutine call_initialize_static
!DEC$ ATTRIBUTES DLLEXPORT,STDCALL,ALIAS:'call_initialize_static' :: call_initialize_static
      implicit none

      call initialize_static

      return

      end subroutine call_initialize_static


! **************************************************************************************

      SUBROUTINE SendSimID(cv_sim_id,iv_sim_id_len)

!DEC$ ATTRIBUTES DLLEXPORT,STDCALL,ALIAS:'SendSimID' :: SendSimID
      IMPLICIT NONE

      integer                      :: iv_sim_id_len
      character(len=iv_sim_id_len) :: cv_sim_id

!DEC$ ATTRIBUTES REFERENCE :: cv_sim_id

! *** Local variables:

! *** Initializing the variables declared above in: "module_mc_wind_domian"

      sim_id = cv_sim_id

      END SUBROUTINE SendSimID

! **************************************************************************************

      SUBROUTINE SendDomainDefinitions(rv_clon,rv_clat, &
            iv_im,iv_jm,iv_km,rv_dx,rv_dy,rav1_ds, &
            rav2_in_top)

!DEC$ ATTRIBUTES DLLEXPORT,STDCALL,ALIAS:'SendDomainDefinitions' :: SendDomainDefinitions

!----------------------------------------------------------------------------------------

      IMPLICIT NONE

      integer :: iv_im
      integer :: iv_jm
      integer :: iv_km

      real :: rv_clon
      real :: rv_clat

      real :: rv_dx
      real :: rv_dy

      real :: rav1_ds(iv_km)
      real :: rav2_in_top(iv_im,iv_jm)


! *** Initializing the McWIND variables declared above in: "module_mc_wind_domain"

      clon = rv_clon
      clat = rv_clat

      im = iv_im
      jm = iv_jm
      km = iv_km

! *** Convert from km to meters:
      dx = rv_dx * 1000.0
      dy = rv_dy * 1000.0

      if (.not. allocated(user_def_deltasigma))  &
           allocate(user_def_deltasigma(iv_km))

      user_def_deltasigma = rav1_ds

      if (.not. allocated(in_top))  &
           allocate(in_top(iv_im,iv_jm))

      in_top = rav2_in_top

      END SUBROUTINE SendDomainDefinitions

!****************************************************************************************

      SUBROUTINE SendDomainAdvanced(rv_stretch_factor, &
                       rv_first_layer_depth)

!DEC$ ATTRIBUTES DLLEXPORT,STDCALL,ALIAS:'SendDomainAdvanced' :: SendDomainAdvanced

!----------------------------------------------------------------------------------------

      IMPLICIT NONE

      real :: rv_stretch_factor
      real :: rv_first_layer_depth

! *** Variable explanation and Default values:
! ***
! *** rv_stretch_factor    = -1.0  ! If positive it indicate the stretch-factor of the 
! ***                              ! thickness of the vertical layers. User defined 
! ***                              ! thicknesses applied if negative.
! ***
! *** rv_first_layer_depth =  0.0  ! Vertical thickness of first layer. Not applied if 
! ***                              ! the "rv_stretch_factor" is negative.

! *** Initializing the variables declared above in: "module_mc_wind_domain"

      stretch_factor    = rv_stretch_factor
      first_layer_depth = rv_first_layer_depth

      END SUBROUTINE SendDomainAdvanced

!***********************************************************************************            

      SUBROUTINE FreeDomain_Memory()

!-----------------------------------------------------------------------------------

      implicit none

      if (allocated(user_def_deltasigma))  &
    deallocate(user_def_deltasigma)
      if (allocated(in_top))  deallocate(in_top)

      END SUBROUTINE FreeDomain_Memory

! *** End of module module_mc_wind_domain.

      end module module_mc_wind_domain
