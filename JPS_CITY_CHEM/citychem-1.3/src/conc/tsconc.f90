! <tsconc.f90 - A component of the City-scale
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

      subroutine tsconc(I)

!     The subroutine reads, calculates and writes concentration data
!     for one timestep.
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
!           2016  M. Karl: commented calls to subgrid concentration routines
!
! ----------------------------------------------------------------------------------

      use mod_util
      use mod_main
      use mod_site
      use mod_time
      use mod_mete
      use mod_conc
      use mod_grid
      use mod_depo
      use mod_lsrc

      implicit none

      integer :: I

!     I - Indicator of pre (0) or post (1) processing

!     Local variables

      real :: recp_grid_diff
      real :: CV

      integer :: IC
      integer :: ILS
      integer ::IX
      integer :: IY
      integer :: IZ
!MSK      logical :: ATDATE
!MSK      logical :: ATTIME

!     CV     - Concentration value
!     IC     - Index of compound
!     ILS    - Index of subgrid layer
!     IX     - Main grid index in x-direction
!     IY     - Main grid index in y-direction
!     IZ     - Index of vertical layer
!     ATDATE - If at given date then true else false
!     ATTIME - If at given time then true else false


!     Perform pre- or postprocessing calculations

      IF (I .EQ. 0) GOTO 100
      IF (I .EQ. 1) GOTO 200

  100 CONTINUE

!     Preprocessing calculations

!     Beginning of simulation period?

      if (ATTIME(BDAT)) then

!       Open all concentrations files

        call opconc

      endif

!     If run model not ok then set missing data and return

      IF (.NOT. RUNOK) THEN

!       Set main grid concentrations to missing data

        DO 110 IC = 1,NC
          CALL H3DFLD(MISS,IC,NX,NY,NC,MX,MY,MC,CM)
  110   CONTINUE

!MSK no sub grid concentrations
!MSK!       Set sub grid concentrations to missing data
!MSK
!MSK        DO 120 ILS = 1,NLS
!MSK          DO 130  IC = 1,NC
!MSK            CALL H4DFLD(MISS,IC,ILS,NC,NXS,NYS,NLS,MC,MXS,MYS,MLS,CS)
!MSK  130     CONTINUE
!MSK  120   CONTINUE


!       Set receptor concentrations to missing data

        CALL H2DFLD(MISS,NC,NR,MC,MR,CR)

!       Set line source associated receptor concentrations to missing data


        CALL H2DFLD(MISS,NC,MRL,MC,MRL,CRL)

        print *,'tsconc: after line source missing data'

        RETURN

      ENDIF

!     First timestep in current hour?

      IF (ITS .EQ. 1) THEN

!       Set main grid concentrations to zero

        DO 140 IC = 1,NC
          CALL H3DFLD(0.,IC,NX,NY,NC,MX,MY,MC,CM)
  140   CONTINUE

!MSK no sub grid concentrations
!MSK!       Set sub grid concentrations to zero
!MSK
!MSK        DO 150 ILS = 1,NLS
!MSK          DO 160  IC = 1,NC
!MSK            CALL H4DFLD(0.,IC,ILS,NC,NXS,NYS,NLS,MC,MXS,MYS,MLS,CS)
!MSK  160     CONTINUE
!MSK  150   CONTINUE

!       Set receptors concentrations to zero

        CALL H2DFLD(0.,NC,NR,MC,MR,CR)

!       Set line source associated receptor concentrations to zero

        IF (NQL .GT. 0) THEN
          IF (.NOT. ALLOCATED(CRL))       ALLOCATE(CRL(NC,2*NQL))
          IF (.NOT. ALLOCATED(DDEPRL))    ALLOCATE(DDEPRL(NC,2*NQL))
          IF (.NOT. ALLOCATED(QLRD))      ALLOCATE(QLRD(2*NQL))
          IF (.NOT. ALLOCATED(WDEPRL))    ALLOCATE(WDEPRL(NC,2*NQL))
          IF (.NOT. ALLOCATED(XRL))       ALLOCATE(XRL(2*NQL))
          IF (.NOT. ALLOCATED(YRL))       ALLOCATE(YRL(2*NQL))
          IF (.NOT. ALLOCATED(ZRL))       ALLOCATE(ZRL(2*NQL))       

          MRL = 2 * NQL
        
          CALL H2DFLD(0.,NC,MRL,MC,MRL,CRL)

        ENDIF

      ENDIF

!     Set grid model emissions to zero

      DO 170 IZ = 1,NZ
        DO 180 IC = 1,NC
          CALL H4DFLD(0.,IC,IZ,NC,NX,NY,NZ,MC,MX,MY,MZ,DCDT)
  180   CONTINUE
  170 CONTINUE

!     Finished with preprocessing

      RETURN

  200 CONTINUE

!     Postprocessing calculations

!     Last timestep in current hour?

      IF (ITS .EQ. NTS) THEN

!       Calculate corrected main grid concentration

!       Go through all main grid cells

!_LHS_June_2007_Start:
!
!       The 210 and 220 loops below are do nothing loops:
!
!        DO 210 IY = 1,NY
!          DO 220 IX = 1,NX
!
!!           If current cell is not a subgrid cell goto next main grid cell
!
!            IF (SUBF(IX,IY) .LE. 0.) GOTO 220
!
!!           Find subgrid layer index
!
!            ILS = INT(SUBF(IX,IY))
!
!!           Go through all compounds
!
!            DO 230 IC = 1,NC
!
!!             If missing data then no changes
!
!              IF (CM(IX,IY,IC) .EQ. MISS) GOTO 230
!
!!               Calculate average subgrid concentration and update
!!               the main grid concentration
!
!                CALL A4DFLD(CV,IC,ILS,NC,NXS,NYS,NLS,MC,MXS,MYS,
!     &                      MLS,CS)
!                CM(IX,IY,IC) = CV
!
!!               Next compound
!
!  230         CONTINUE
!
!!             Next main grid cell
!
!  220       CONTINUE
!  210     CONTINUE
!
!_LHS_June_2007_End.

          print *,'tsconc: postprocessing'
          !print *,'tsconc: ITS NTS', ITS, NTS

!         Write out concentration data to files

          CALL WCONCM

          !print *,'tsconc: after WCONCM'

!         ... for the sub  grid cells

!MSK          CALL WCONCS

!         ... for the irregular receptor points

          CALL WCONCR

!         ... for the line source associated receptor points

          !print *,'tsconc: before wconcl'
          CALL WCONCL
          !print *,'tsconc: after wconcl'


      ENDIF

!     End of simulation period?

      if (ATDATE(EDAT) .AND. ITS .EQ. NTS) then

!       Close all concentration files

        call clconc

      endif

!     Finished with postprocessing

      RETURN

!     End of subroutine TSCONC

      end subroutine tsconc
