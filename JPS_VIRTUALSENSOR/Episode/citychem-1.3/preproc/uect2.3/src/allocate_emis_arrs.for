! <allocate_emis_arrs.for - A component of the City-scale
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

      subroutine allocate_emis_arrs

!***********************************************************************
!***  Subroutine allocate_emis_arrs allocates memory to the arrays
!***  used in the emission routines
!***********************************************************************

!     Declarations of variables by using the MODULES feature:

      use module_uect_io
      use module_uect_exe


      implicit none

!***********************************************************************


!     Local declarations:


!***********************************************************************
!     Content of subroutine:


        if (.not. allocated(new_array_pse))  allocate(new_array_pse(n_sopp,colp))
        if (.not. allocated(new_array_lse))  allocate(new_array_lse(n_soll,coll))
        if (.not. allocated(new_array_ase))  allocate(new_array_ase(n_soaa,cola))

        if (.not. allocated(in_array_pse))   allocate(in_array_pse(n_sopp,colp))
        if (.not. allocated(in_array_lse))   allocate(in_array_lse(n_soll,coll))
        if (.not. allocated(in_array_ase))   allocate(in_array_ase(n_soaa,cola))

        if (.not. allocated(snap_new_pse))   allocate(snap_new_pse(n_sopp))
        if (.not. allocated(snap_new_lse))   allocate(snap_new_lse(n_soll))
        if (.not. allocated(snap_new_ase))   allocate(snap_new_ase(n_soaa))

        if (.not. allocated(snap_vec_pse))   allocate(snap_vec_pse(n_sopp))
        if (.not. allocated(snap_vec_lse))   allocate(snap_vec_lse(n_soll))
        if (.not. allocated(snap_vec_ase))   allocate(snap_vec_ase(n_soaa))

        if (.not. allocated(xi_ase))         allocate(xi_ase(n_soaa))
        if (.not. allocated(yi_ase))         allocate(yi_ase(n_soaa))

      end subroutine allocate_emis_arrs
