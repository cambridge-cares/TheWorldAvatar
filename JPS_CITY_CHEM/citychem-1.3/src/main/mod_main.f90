! <mod_main.f90 - A component of the City-scale
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

      module mod_main

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

!***  Unique simulation ID:
      character (len=38) :: sim_id

!_NEST_Start:  Introducing an additional INFO-file:
      character(len=256) :: mainfn
      character(len=256) :: messfn
      character(len=256) :: infofn

!     mainfn - Main ... input data filename
!     messfn - Messages output ... filename
!     infofn - Info ... output ... filename

      integer :: mainun
      integer :: messun 
      integer :: infoun

!     mainun - Main ... input data fileunit
!     messun - Messages output ... fileunit
!     infoun - Info ... output ... fileunit

      logical :: mainfe
      logical :: messfe
      logical :: infofe

!     mainfe - Main ... input data file exists
!     messfe - Messages output ... file exists
!     infofe - Info ... output ... file exists

      integer :: mainfm
      integer :: messfm
      integer :: infofm

!     mainfm - Main ... input data file format (index)
!     messfm - Messages output ... file format (index)
!     infofm - Info ... output ... file format (index)
!_NEST_End.

      character(len=256) :: version = 'version 1.3'

!     version - Program version textstring

!_LHS_New_episode_dll_October_2008_Start:

      integer :: callcnt

!MSK taken from mod_interface.for
!_METE (Static Data):
      integer   :: pimeteexternaldata        ! If 1 then read from files from AirQUIS/McWIND.
                                             ! If 2 then read from external   UM files.
                                             ! If 3 then read from external   TAPM files.
!MSK end

! *** callcnt - Global counter of number of time runepi has been called
! ***           from AirQuis.

!_LHS_New_episode_dll_October_2008_End.

      contains


      subroutine FreeMainMemory()

!DEC$ ATTRIBUTES DLLEXPORT,STDCALL,ALIAS:'FreeMainMemory' :: FreeMainMemory

      implicit none

      mainfn = ' '
      messfn = ' '
      mainun = 0
      messun = 0
      mainfe = .false.
      messfe = .false.
      mainfm = 0
      messfm = 0
      version = ' '
      

!     End of subroutine FreeMainMemory
      end subroutine

      subroutine FreeEpisodeMemory()

!DEC$ ATTRIBUTES DLLEXPORT,STDCALL,ALIAS:'FreeEpisodeMemory' :: FreeEpisodeMemory

      USE mod_asrc
      USE mod_conc
      USE mod_depo
      USE mod_emep03
 
!MSK      USE mod_emep68
      USE mod_grid
      USE mod_lsrc
      USE mod_mete
      USE mod_phot
      USE mod_psrc
      USE mod_site
      USE mod_stat
      USE mod_time
      USE mod_util

      implicit none

      integer iu
      logical isopen

      CALL FreeAsrcMemory
      CALL FreeConcMemory
      CALL FreeDepoMemory
      CALL FreeEmep03Memory
!MSK      CALL FreeEmep68Memory
      CALL FreeGridMemory
      CALL FreeLsrcMemory
      CALL FreeMainMemory
      CALL FreeMeteMemory
      CALL FreePhotMemory
      CALL FreePsrcMemory
      CALL FreeSiteMemory
      CALL FreeStatMemory
      CALL FreeTimeMemory
      CALL FreeUtilMemory

!     Close all open units

!_LHS_ORG      do iu = 10,99
      iu = 10
      do
        inquire(iu,opened=isopen)
        if (isopen)then
           print * , ' Closing the opened fileunit: iu = ',iu
           close (iu)
        endif
        iu = iu +1
        if (iu == 1000) exit
      enddo

!     End of subroutine FreeEpisodeMemory
      end subroutine

      end module mod_main
