! <cleanup_met.for - A component of the City-scale
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

      subroutine cleanup_met

!MSK!DEC$ ATTRIBUTES DLLEXPORT,STDCALL,ALIAS:'cleanup_met' :: cleanup_met

! This subroutine deallocate 'module_mc_wind_static' arrays:
! Modifications:
!     21 Jan 2019  M. Karl: Commented pre-processor directive ATTRIBUTES
!     29 Jan 2019  M. karl: Replaced TABS by spaces
!
!***********************************************************************

      use module_mc_wind_files
      use module_mc_wind_met

      implicit none

!DEC$ IF DEFINED (main_program)

! Close the surface observation file:
      if (fe_in_surface_obs) then
         call clifil(funit_in_surface_obs)
      end if

! Close the profile observation file:
      if (fe_in_profile_obs) then
         call clifil(funit_in_profile_obs)
      end if

! Close the geostrophic observation file:
      if (fe_in_geostrophic_obs) then
        call clifil(funit_in_geostrophic_obs)
      end if

!DEC$ ENDIF

! Deallocate "module_mc_wind_winds" arrays:

      if (allocated (surface_stat_posx)) &
    deallocate (surface_stat_posx)

      if (allocated (surface_stat_posy)) &
    deallocate (surface_stat_posy)

      if (allocated (surface_stat_hgt_vel)) &
    deallocate (surface_stat_hgt_vel)

      if (allocated (surface_stat_hgt_tmp)) &
    deallocate (surface_stat_hgt_tmp)

      if (allocated (surface_stat_hgt_dtup))  &
    deallocate (surface_stat_hgt_dtup)

      if (allocated (surface_stat_hgt_dtlo))  &
    deallocate (surface_stat_hgt_dtlo)

      if (allocated (surface_stat_wspeed)) &
    deallocate (surface_stat_wspeed)

      if (allocated (surface_stat_wdir)) &
    deallocate (surface_stat_wdir)

      if (allocated (surface_stat_tmp)) &
    deallocate (surface_stat_tmp)

      if (allocated (surface_stat_dt)) &
    deallocate (surface_stat_dt)

      if (allocated (surface_stat_sr)) &
    deallocate (surface_stat_sr)

      if (allocated (surface_stat_press)) &
    deallocate (surface_stat_press)

      if (allocated (surface_stat_rh)) &
    deallocate (surface_stat_rh)

      if (allocated (surface_stat_mm)) &
    deallocate (surface_stat_mm)
!MSK start
      if (allocated (surface_stat_clc)) &
    deallocate (surface_stat_clc)

!MSK end
      if (allocated (surface_stat_z0)) &
    deallocate (surface_stat_z0)

      if (allocated (surface_stat_pwr)) &
    deallocate (surface_stat_pwr)

      if (allocated (surface_stat_scale)) &
    deallocate (surface_stat_scale)

      if (allocated (app_surface_stat_scale)) &
    deallocate (app_surface_stat_scale)

      if (allocated (surface_stat_ffref)) &
    deallocate (surface_stat_ffref)

      if (allocated (surface_stat_name))  &
    deallocate (surface_stat_name)

      if (allocated (geostrophic_stat_name))  &
    deallocate (geostrophic_stat_name)

      if (allocated (geostrophic_stat_posx)) &
    deallocate (geostrophic_stat_posx)

      if (allocated (geostrophic_stat_posy)) &
    deallocate (geostrophic_stat_posy)

      if (allocated (geostrophic_stat_ffref)) &
    deallocate (geostrophic_stat_ffref)

      if (allocated (geostrophic_stat_ddref)) &
    deallocate (geostrophic_stat_ddref)

      if (allocated (geostrophic_stat_pwr)) &
    deallocate (geostrophic_stat_pwr)

      if (allocated (geostrophic_stat_scale)) &
    deallocate (geostrophic_stat_scale)

      if (allocated (in_surfrough)) &
    deallocate (in_surfrough)

      if (allocated (surfrough)) &
    deallocate (surfrough)

      if (allocated (u0_surfrough)) &
    deallocate (u0_surfrough)

      if (allocated (v0_surfrough)) &
    deallocate (v0_surfrough)

      if (allocated (in_landuse)) &
    deallocate (in_landuse)

      if (allocated (landuse)) &
    deallocate (landuse)

      if (allocated (u0_surface_ref)) &
    deallocate (u0_surface_ref)

      if (allocated (v0_surface_ref)) &
    deallocate (v0_surface_ref)

      if (allocated (u0_geostrophic_ref)) &
    deallocate (u0_geostrophic_ref)

      if (allocated (v0_geostrophic_ref)) &
    deallocate (v0_geostrophic_ref)

      return
      end subroutine cleanup_met
