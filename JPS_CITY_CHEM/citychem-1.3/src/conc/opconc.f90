! <opconc.f90 - A component of the City-scale
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

      subroutine opconc

! The subroutine opens concentrations data files.
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
!           2017  M. Karl: main grid concentration file replaced by netcdf
!
! ----------------------------------------------------------------------------------

      use mod_util
      use mod_main
      use mod_site
      use mod_conc
!MSK start
      use mod_writenc
!MSK end

      implicit none

! Local variables

      integer :: IC

! IC - Index of compound

!MSK start netCDF output
      integer :: IX, IY
      real    :: gridwidth

      real,   allocatable :: mxm_i(:,:)
      real,   allocatable :: mxm_j(:,:)

! Allocate
        if (.not. allocated(mxm_i) )    allocate(mxm_i(NY,NX))
        if (.not. allocated(mxm_j) )    allocate(mxm_j(NY,NX))

!MSK end


!MSK replaced by netCDF output for 2D main grid
!MSK! Go through all compounds
!MSK
!MSK      DO 100 IC = 1,NC
!MSK
!MSK! Set main grid concentrations file unit, file format and open file
!MSK
!MSK          if (MCONFE(IC)) call opofil(MCONFN(IC),MCONUN(IC),MCONFE(IC),MCONFM(IC))

!_NEST_Start:
! Set main grid concentrations error file unit, file format and open file
!
!          CALL OPOFIL(MERRFN(IC),MERRUN(IC),MERRFE(IC),MERRFM(IC))
!
! Set main grid concentrations analyt. file unit, file format and open file
!
!          CALL OPOFIL(MANAFN(IC),MANAUN(IC),MANAFE(IC),MANAFM(IC))
!_NEST_End.

! Set  sub grid concentrations file unit, file format and open file
!
!          CALL OPOFIL(SCONFN(IC),SCONUN(IC),SCONFE(IC),SCONFM(IC))

!MSK  100 CONTINUE

! Set receptors concentrations file unit, file format and open file

      call opofil(RCONFN,RCONUN,RCONFE,RCONFM)

! Set line sources concentrations file unit, file format and open file

      call opofil(LCONFN,LCONUN,LCONFE,LCONFM)

! Set netCDF output files for main grid concentration 3D-fields
! The user-selected chemical species will be written to the file

! MAIN GRID NETCDF

        gridwidth = DX
        !**  create a grid with the x- and y- coordinates of the main grid centre-points
        do IX = 1, NX
          do IY = 1, NY

            mxm_i(IY,IX) = SITEX0 + (IX-1)*DX 
            mxm_j(IY,IX) = SITEY0 + (IY-1)*DY

          enddo
        enddo

!  HOURLY 3D
        if (NH1CONFE) then
          call CreateNCfileGrid(NH1CONFN, NX, NY, NZ,  mxm_i, mxm_j, Z ,gridwidth, utmzone,EPSGN,SITEX0,SITEY0)
        endif

! Deallocate
        if (allocated(mxm_i))     deallocate(mxm_i)
        if (allocated(mxm_j))     deallocate(mxm_j)


      RETURN

! End of subroutine OPCONC

      end subroutine opconc
