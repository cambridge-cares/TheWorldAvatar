! <wcravea.f90 - A component of the City-scale
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
!* ========================================== 
!*
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
!*****************************************************************************!

      subroutine WCRAVEA

!       2016 Matthias Karl, HZG, This is an original CityChem subroutine
!       The subroutine writes receptor daily mean concentrations to file.
! ----------------------------------------------------------------------------------
! REVISION HISTORY
!
!    xx Dat 201x  Name: Line  Description of Change
!
! ----------------------------------------------------------------------------------

      use mod_main
      use mod_site
      use mod_time
      use mod_conc
      use mod_stat
      use mod_writenc

      implicit none

! Local variables

      character(len=10) :: TEXT1
      character(len=10) :: TEXT2
      character(len=10) :: namefield
      character(len=10) :: unitname
      character(len=23) :: validity

      integer :: IC
      integer :: IR
! IC - Index of compound
! IR - Index of receptor point

      integer :: facres
      integer :: K, I, J
      integer :: Nhh_in

      integer, dimension(4,1) :: mdate

      logical :: dopacking  = .false.
      logical :: dofloat    = .false.
      logical :: domirror   = .false.

      real    :: gridwidth

! Array arguments
      double precision, allocatable :: field2D(:,:,:)
      double precision        :: field1D(STATIONS,STATIONS,1)


! ALLOCATE THE RECEPTOR CONC ARRAYS
        ! Receptor point coordinates: xr(NR), yr(NR)
        ! By default 100m grid.
        gridwidth = 100.

        !** factor between main grid resolution dx and
        !** the 100m raster: dx/100 as integer
        facres = INT(dx/gridwidth)

        if (.not. allocated(field2D) )  allocate(field2D(NX*facres,NY*facres,1))

! Go through all compounds

      DO 100 IC = 1,NC


! Write the netCDF output file of all species, overall receptor points concentration average

             if (NC2STAFE) then

                ! one time step 
                Nhh_in = 1

                ! current simulation date at simulation end
                mdate(1,Nhh_in) = YEAR
                mdate(2,Nhh_in) = MNTH
                mdate(3,Nhh_in) = DAYM
                mdate(4,Nhh_in) = HOUR

                namefield = CMPND(IC)
                if ( trim(CMPND(IC)).eq."PM2.5") then
                  namefield = "PM25      "
                endif

                unitname = "ug/m^3"

                if (averaged_output) then
                   validity = 'averaged'
                else
                   validity = 'instantaneous'
                endif

                dopacking = .false.
                dofloat   = .true.
                domirror  = .false.

!** factor between main grid resolution dx and
!** the 100m raster: dx/100 as integer
                facres = INT(dx/100.)

!**  define receptors coordinates in utm grid
!**  start at point 21 (=number of stations + 1)
!**  distance between points is 100 m
                K = STATIONS + 1

                do J = 1, (NY*facres) 
                  do I = 1, (NX*facres)

                      field2D(I,J,1) = CRAVEA(IC,K)

                      K=K+1

                  end do
                end do

                call writeconcfield(NC2STAFN, namefield,unitname,field2D(1:NX*facres,1:NY*facres,1) , NX*facres ,NY*facres, 1,   &
                                    Nhh_in, mdate, validity, dopacking, dofloat, domirror)                                   


             endif

! Write netCDF output for monitoring stations
! Stations are at the beginning of the receptor point input file
! Number of stations = STATIONS

              print *,'wcravea: write stations', NC3STAFE

             if (NC3STAFE) then

                ! one time step 
                Nhh_in = 1

                ! current simulation date at simulation end
                mdate(1,Nhh_in) = YEAR
                mdate(2,Nhh_in) = MNTH
                mdate(3,Nhh_in) = DAYM
                mdate(4,Nhh_in) = HOUR

                namefield = CMPND(IC)
                if ( trim(CMPND(IC)).eq."PM2.5") then
                  namefield = "PM25      "
                endif

                unitname = "ug/m^3"

                if (averaged_output) then
                   validity = 'averaged'
                else
                   validity = 'instantaneous'
                endif

                dopacking = .false.
                dofloat   = .true.
                domirror  = .false.

                K = 1
                do I = 1, STATIONS
                  do J = 1, STATIONS

                   ! write values in the diagonale
                     if (I.eq.J) then
                       field1D(J,I,1) = CRAVEA(IC,K)          
                       K=K+1
                    else
                       field1D(J,I,1) = 0.0 
                    endif

                  end do
                end do

                call writeconcfield(NC3STAFN, namefield,unitname,field1D(1:STATIONS,1:STATIONS, 1),   &
                                   STATIONS, STATIONS, 1, Nhh_in, mdate, validity, dopacking, dofloat, domirror)

             endif


! Next compound

  100 CONTINUE


! Deallocate
        if (allocated(field2D))     deallocate(field2D)

      RETURN


! End of subroutine WCRAVEA

      end subroutine wcravea
