! <tsdepo.f90 - A component of the City-scale
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

      subroutine TSDEPO(I)

! The subroutine reads, calculates and writes deposition data for
! one timestep.
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
!           2016  M. Karl: all writing to deposition files is commented
!
! ----------------------------------------------------------------------------------

      use mod_util
      use mod_main
      use mod_site
      use mod_time
      use mod_mete
      use mod_conc
      use mod_depo
      use mod_lsrc

      implicit none

      integer :: I
! I - Indicator of pre (0) or post (1) processing

! Local variables

      real    :: DV
      integer :: IC
      integer :: ILS
      integer :: IX
      integer :: IY
!MSK      LOGICAL ATDATE
!MSK      LOGICAL ATTIME

! DV     - Deposition value
! IC     - Index of compound
! ILS    - Index of subgrid layer
! IX     - Main grid index in x-direction
! IY     - Main grid index in y-direction
! IZ     - Index of vertical layer
! ATDATE - If at given date then true else false
! ATTIME - If at given time then true else false

! Perform pre- or postprocessing calculations

      IF (I .EQ. 0) GOTO 100
      IF (I .EQ. 1) GOTO 200

  100 CONTINUE

! Preprocessing calculations

! Beginning of simulation?

      IF (ATTIME(BDAT)) THEN

! Open all depositions files

!MSK not wanted          CALL OPDEPO

      ENDIF

! If run model not ok then set missing data and return

      IF (.NOT. RUNOK) THEN

! Set main grid depositions to missing data

          DO 110 IC = 1,NC
              CALL H3DFLD(MISS,IC,NX,NY,NC,MX,MY,MC,DDEPM)
              CALL H3DFLD(MISS,IC,NX,NY,NC,MX,MY,MC,WDEPM)
  110     CONTINUE

! Set  sub grid depositions to missing data

          DO 120 ILS = 1,NLS
          DO 130  IC = 1,NC
              CALL H4DFLD(MISS,IC,ILS,NC,NX,NY,NLS,MC,MX,MY,MLS,DDEPS)
              CALL H4DFLD(MISS,IC,ILS,NC,NX,NY,NLS,MC,MX,MY,MLS,WDEPS)
  130     CONTINUE
  120     CONTINUE

! Set receptors depositions to missing data

          IF (MC .GT. 0 .AND. MR .GT. 0) THEN
              CALL H2DFLD(MISS,NC,NR,MC,MR,DDEPR)
              CALL H2DFLD(MISS,NC,NR,MC,MR,WDEPR)
          ENDIF

! Set line source associated receptors depositions to missing data

          IF (MC .GT. 0 .AND. MRL .GT. 0) THEN
              CALL H2DFLD(MISS,NC,NRL,MC,MRL,DDEPRL)
              CALL H2DFLD(MISS,NC,NRL,MC,MRL,WDEPRL)
          ENDIF

          RETURN

      ENDIF

! First timestep in current simulation period

      IF (ITS .EQ. 1) THEN


! Set main grid depositions to zero

          DO 140 IC = 1,NC
              CALL H3DFLD(0.,IC,NX,NY,NC,MX,MY,MC,DDEPM)
              CALL H3DFLD(0.,IC,NX,NY,NC,MX,MY,MC,WDEPM)
  140     CONTINUE

!MSK start sub grid nests not used
! Set  sub grid depositions to zero
!
!          DO 150 ILS = 1,NLS
!          DO 160  IC = 1,NC
!              CALL H4DFLD(0.,IC,ILS,NC,NX,NY,NLS,MC,MX,MY,MLS,DDEPS)
!              CALL H4DFLD(0.,IC,ILS,NC,NX,NY,NLS,MC,MX,MY,MLS,WDEPS)
!  160     CONTINUE
!  150     CONTINUE
!MSK end

! Set receptors depositions to zero

          IF (MC .GT. 0 .AND. MR .GT. 0) THEN
              CALL H2DFLD(0.,NC,NR,MC,MR,DDEPR)
              CALL H2DFLD(0.,NC,NR,MC,MR,WDEPR)
          ENDIF

! Set line sources associated receptors depositions to zero


          IF (NQL .GT. 0) THEN
            IF (.NOT. ALLOCATED(CRL))       ALLOCATE(CRL(NC,2*NQL))
            IF (.NOT. ALLOCATED(DDEPRL))    ALLOCATE(DDEPRL(NC,2*NQL))
            IF (.NOT. ALLOCATED(QLRD))      ALLOCATE(QLRD(2*NQL))
            IF (.NOT. ALLOCATED(WDEPRL))    ALLOCATE(WDEPRL(NC,2*NQL))
            IF (.NOT. ALLOCATED(XRL))       ALLOCATE(XRL(2*NQL))
            IF (.NOT. ALLOCATED(YRL))       ALLOCATE(YRL(2*NQL))
            IF (.NOT. ALLOCATED(ZRL))       ALLOCATE(ZRL(2*NQL))       
          ENDIF
          MRL = 2 * NQL
          
          IF (MC .GT. 0 .AND. MRL .GT. 0) THEN
              CALL H2DFLD(0.,NC,MRL,MC,MRL,DDEPRL)
              CALL H2DFLD(0.,NC,MRL,MC,MRL,WDEPRL)
          ENDIF

      ENDIF

! Finished with preprocessing


      RETURN

  200 CONTINUE

! Postprocessing calculations

! Last timestep in current simulation period?

      IF (ITS .EQ. NTS) THEN

! Calculate corrected main grid deposition

! Go through all main grid cells

          DO 210 IY = 1,NY
          DO 220 IX = 1,NX

! If current cell is not a subgrid cell goto next main grid cell

              IF (SUBF(IX,IY) .LE. 0.) GOTO 220

! OK, current main grid cell is a subgrid cell

! Find subgrid layer index

              ILS = INT(SUBF(IX,IY))

! Go through all compounds

              DO 230 IC = 1,NC

! If missing data then no changes

                  IF (DDEPM(IX,IY,IC) .EQ. MISS) GOTO 230

! Calculate main grid corrected dry deposition as an average subgrid
! scale dry deposition

                  CALL A4DFLD(DV,IC,ILS,NC,NXS,NYS,NLS,MC,MXS,MYS,  &
                             MLS,DDEPS)
                  DDEPM(IX,IY,IC) = DV

! Calculate main grid corrected wet deposition as an average subgrid
! scale wet deposition

                  CALL A4DFLD(DV,IC,ILS,NC,NXS,NYS,NLS,MC,MXS,MYS,  &
                             MLS,WDEPS)
                  WDEPM(IX,IY,IC) = DV

! Next compound

  230         CONTINUE

! Next main grid cell

  220     CONTINUE
  210     CONTINUE

! Write out deposition data to files

! ... for the main grid cells

!MSK not wanted          CALL WDEPOM

! ... for the sub  grid cells

!MSK          CALL WDEPOS

! ... for the irregular receptor points

!MSK          CALL WDEPOR

! ... for the line source associated receptor points

!MSK          CALL WDEPOL

      ENDIF

! End of simulation?

      IF (ATDATE(EDAT) .AND. ISH .EQ. NSH .AND. ITS .EQ. NTS) THEN

! Close all deposition files

!MSK not wanted           CALL CLDEPO

      ENDIF

! Finished with postprocessing

      RETURN

! End of subroutine TSDEPO

      end subroutine tsdepo
