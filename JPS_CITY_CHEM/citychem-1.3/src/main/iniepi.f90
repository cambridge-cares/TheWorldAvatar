! <iniepi.f90 - A component of the City-scale
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

      subroutine INIEPI

! *** The subroutine reads input data from the main input data file.
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
!    ----------------------------------------------------------------
!    11 Nov 2019  M. Karl: L133 Comment inquire/close of mainfn -
!                               causes problems with gfortran 7.4.0
!
! ----------------------------------------------------------------------------------

      USE mod_asrc
      USE mod_conc
      USE mod_depo
      USE mod_emep03
      USE mod_grid
      USE mod_lsrc
      USE mod_main
      USE mod_mete
      USE mod_phot
      USE mod_psrc
      USE mod_site
      USE mod_stat
      USE mod_time
      USE mod_util

         implicit none

! *** Local variables

      integer i, ia, ic, iz

! *** I  - Dummy index
! *** IA - Area source index
! *** IC - Compound index
! *** IZ - Vertical layer index


      write (*,*)
      write (*,*) '** Welcome to CityChem ', version(1:25),' **' 


! *** Initially OK to run model

      runok = .true.

! *** Read main input filename. The routine below apply intrinsic
! *** routines to get the argument following the call for EPISODE.
! *** In the run file (typically a ".bat"-file) that reside in the
! *** pro folder, the .bat-file contain a line containing the command:
! ***
! ***   episode ../dat/main_nox.dat
! ***
! *** Here ../dat/main_nox.dat is the name of the input-file: "MAINFN".      

      call init(MAINFN)
      print *,'iniepi.for: after INIT'

! *** Open the main input file:

!MSK 11.11.2019 Commented the next inquire and close 
!MSK close the already open main input-file
!MSK is not needed
!MSK      INQUIRE(FILE=MAINFN,EXIST=MAINFE, NUMBER=MAINUN)
!MSK      close(MAINUN)
!MSK end

      call opifil(MAINFN,MAINUN,MAINFE,MAINFM)
      print *,'iniepi.for: after OPIFIL'

! *** If no main input file then stop

      if(.not. mainfe) then
          call stopit('INIEPI: No main input data file!')
      endif



! *** Read main messages data:
      call rmain
      print *,'iniepi.for: after RMAIN'

! *** Read main site data:
      call rsite
      print *,'iniepi.for: after RSITE'

! *** Initialize Episode time data:
      call rtime
      print *,'iniepi.for: after RTIME'

! *** Read main meteorological data:
      call rmete
      print *,'iniepi.for: after RMETE'

! *** Read main concentrations data:
      call rconc
      print *,'iniepi.for: after RCONC'

! *** Read main depositions data:
      call rdepo
      print *,'iniepi.for: after RDEPO'

! *** Read main statistics data:
      call rstat
      print *,'iniepi.for: after RSTAT'

! *** Read main photochemistry data:
      call rphot
      print *,'iniepi.for: after RPHOT'

! *** Read main grid model data:
      call rgrid
      print *,'iniepi.for: after RGRID'

! *** Read main area  sources subgrid model data:
      call rasrcfn
      print *,'iniepi.for: after RASRCFN'

! *** Read main point sources subgrid model data:
      call rpsrcfn
      print *,'iniepi.for: after RPSRCFN'

! *** Read main line  sources subgrid model data:
      call rlsrcfn
      print *,'iniepi.for: after RLSRCFN'

! *** Close the Main input file:
      call clifil(MAINFN,MAINUN,MAINFE,MAINFM)
      print *,'iniepi.for: after CLIFIL'

! *** Run model OK?
      if (.not. runok) THEN
        if (messfe) THEN
          write (messun,2000)
        endif
      endif

      return

 2000 format('INIEPI: Errors in static input data!')

! *** End of subroutine INIEPI

      end subroutine INIEPI
