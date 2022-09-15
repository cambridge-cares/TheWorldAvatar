! <tslsrc.f90 - A component of the City-scale
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

      subroutine TSLSRC(I)

! The subroutine reads, calculates and writes line sources subgrid
! model data for one timestep.
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
!           2016  M. Karl: commented the sub grid cells treatment
!
! ----------------------------------------------------------------------------------

      use mod_util
      use mod_main
      use mod_site
      use mod_time
      use mod_mete
      use mod_conc
      use mod_lsrc

      implicit none

      integer :: I

! I - Indicator of pre (0) or post (1) processing

! Local variables

!MSK      LOGICAL ATDATE
!MSK      LOGICAL ATTIME

! ATDATE - If at given date then true else false
! ATTIME - If at given time then true else false


       INTEGER :: I_preprosonly
       integer :: lhs_iql,lhs_ic

! Perform pre- or postprocessing calculations

      IF (I .EQ. 0) GOTO 100
      IF (I .EQ. 1) GOTO 200

  100 CONTINUE

! Preprocessing calculations

! Beginning of simulation period?

      IF (ATTIME(BDAT)) THEN

! Open all line sources files

          call oplsrc

! Read line sources static data (metadata)

          call rlsrcs

! Calculate line sources static data

          call clsrcs

      ENDIF

! First timestep in current simulation period?

      IF (ITS .EQ. 1) THEN

! Read line sources variable data

! read line sources variable data

          call rlsrcv

      ENDIF

! Calculate line sources emissions for the grid model

        I_preprosonly = 0
!MSK start debug
!MSK debug: Testing the effect of turning off line source emissions to the grid, ie only subgrid
!MSK debug comment below line
        IF (RUNOK)  call lsgrid(I_preprosonly) 
!MSK end debug

! Finished with preprocessing

      RETURN

  200 CONTINUE


! Postprocessing calculations

! Last timestep in current simulation period?

      IF (RUNOK .AND. ITS .EQ. NTS) THEN

! Calculate line sources subgrid concentrations and depositions

          IF (MESSFE) THEN
              WRITE (MESSUN,*)
              WRITE (MESSUN,2000) NQL
          ENDIF

! ... in the main grid cells

!
! ***     Removing the preprocessor directive ("linesource_changes").
! ***     Instead the call for CSUBLM below is just commented out to
! ***     avoid the inclusion of line source averaging in the main grid
! ***     concentrations.
!_LHS:     CALL CSUBLM(LSRCAI,LSRCAI,LSRCAI)


! ... in the sub  grid cells
!MSK no subgrid treatment
!MSK          call csubls(LSRCAI,LSRCAI,LSRCAI)


! ... in the irregular receptor points

          call csublr(LSRCAI,LSRCAI,LSRCAI)


! ... in the line source associated receptor points

          call csubll(LSRCAI,LSRCAI,LSRCAI)


      ENDIF

! End of simulation?

      IF (ATDATE(EDAT) .AND. ISH .EQ. NSH .AND. ITS .EQ. NTS) THEN

! Close all line sources files

          call cllsrc

      ENDIF

! Finished with postprocessing

      RETURN

 2000 format('TSLSRC: Calculating on ',I6,' line sources')

! End of subroutine TSLSRC

      end subroutine tslsrc
