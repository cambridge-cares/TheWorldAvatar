! <free_emis_arrs.for - A component of the City-scale
!                 Chemistry Transport Model EPISODE-CityChem>
!*****************************************************************************!
!*
!*        CITY-scale CHEMistry Transport Extension
!*
!*        Copyright (C) 2018  Matthias Steffen Karl
!*
!*        Contact Information: 
!*            Institute of Coastal Research
!*            Helmholtz-Zentrum Geesthacht
!*            Max-Planck-Str. 1
!*            21502 Geesthacht
!*            Germany
!*            email:  matthias.karl@hzg.de
!*
!*      EPISODE-CityChem, developed at Helmholtz-Zentrum Geesthacht (HZG) is designed
!*      for treating complex atmospheric chemistry in urban areas (Karl, 2018). The model
!*      is an extension of the EPISODE dispersion model to enable chemistry/transport
!*      simulations of reactive pollutants on city scale. EPISODE is an Eulerian dispersion
!*      model developed at the Norwegian Institute for Air Research (NILU) appropriate for
!*      air quality studies at the local scale (Slørdal et al. 2003 &2008). The model is an
!*      open source code subject to the Reciprocal Public License ("RPL") Version 1.5,
!*      https://opensource.org/licenses/RPL-1.5.
!*
!*        Reference:
!*      Karl, M. (2018):  Development of the city-scale chemistry transport model 
!*      CityChem-EPISODE and its application to the city of Hamburg, 
!*      Geosci. Model Dev. Discuss.,
!*      https://doi.org/10.5194/gmd-2018-8, 2018.
!*
!*
!*  Unless explicitly acquired and licensed from Licensor under another license,
!*  the contents of this file are subject to the Reciprocal Public License ("RPL")
!*  Version 1.5, https://opensource.org/licenses/RPL-1.5 or subsequent versions as
!*  allowed by the RPL, and You may not copy or use this file in either source code
!*  or executable form, except in compliance with the terms and conditions of the RPL. 
!*
!*  All software distributed under the RPL is provided strictly on an "AS IS" basis, 
!*  WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESS OR IMPLIED, AND LICENSOR HEREBY 
!*  DISCLAIMS ALL SUCH WARRANTIES, INCLUDING WITHOUT LIMITATION, ANY WARRANTIES OF
!*  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, QUIET ENJOYMENT, OR NON-INFRINGEMENT.
!*  See the RPL for specific language governing rights and limitations under the RPL.
!*
!*****************************************************************************!
!
!***********************************************************************
!***
!***      UECT
!***      Urban Emission Conversion Tool
!***
!***********************************************************************

      subroutine free_emis_arrs

!***********************************************************************
!***  Subroutine free_emis_arrs deallocates memory of the arrays
!***  used in the emission routines 
!***********************************************************************

!     Declarations of variables by using the MODULES feature:

      use module_uect_exe


      implicit none

!***********************************************************************


!     Local declarations:


!***********************************************************************
!     Content of subroutine:

        if (allocated(out_array_pse))  deallocate(out_array_pse)
        if (allocated(out_array_lse))  deallocate(out_array_lse)
        if (allocated(out_array_ase))  deallocate(out_array_ase)

        if (allocated(out_params_pse)) deallocate(out_params_pse)
        if (allocated(out_params_lse)) deallocate(out_params_lse)
        if (allocated(out_params_ase)) deallocate(out_params_ase)

        if (allocated(new_array_pse))  deallocate(new_array_pse)
        if (allocated(new_array_lse))  deallocate(new_array_lse)
        if (allocated(new_array_ase))  deallocate(new_array_ase)

        if (allocated(in_array_pse))   deallocate(in_array_pse)
        if (allocated(in_array_lse))   deallocate(in_array_lse)
        if (allocated(in_array_ase))   deallocate(in_array_ase)

        if (allocated(snap_new_pse))   deallocate(snap_new_pse)
        if (allocated(snap_new_lse))   deallocate(snap_new_lse)
        if (allocated(snap_new_ase))   deallocate(snap_new_ase)

        if (allocated(snap_vec_pse))   deallocate(snap_vec_pse)
        if (allocated(snap_vec_lse))   deallocate(snap_vec_lse)
        if (allocated(snap_vec_ase))   deallocate(snap_vec_ase)

        if (allocated(xi_ase))         deallocate(xi_ase)
        if (allocated(yi_ase))         deallocate(yi_ase)


      end subroutine free_emis_arrs
