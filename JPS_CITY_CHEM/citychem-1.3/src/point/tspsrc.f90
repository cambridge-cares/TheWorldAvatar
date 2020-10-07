! <tspsrc.f90 - A component of the City-scale
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

      subroutine TSPSRC(I)

! *** The subroutine reads, calculates and writes point sources subgrid
! *** model data for one timestep.
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
!           2017  M. Karl: No treatment of point sources if PSRCMTYPE == 0
!                          Plume segments are now written every hour (routine WNEWP)
!    15 Feb 2018  M. Karl: L120 Do not read point sources at start of simulation
!
! ----------------------------------------------------------------------------------

      use mod_util
      use mod_main
      use mod_site
      use mod_time
      use mod_mete
      use mod_conc
      use mod_psrc

      implicit none

      integer :: I

! *** I - Indicator of pre (0) or post (1) processing

! *** Local variables

!MSK      LOGICAL ATDATE
!MSK      LOGICAL ATTIME
!MSK      LOGICAL BFTIME

! *** ATDATE - If at     given date then true else false
! *** ATTIME - If at     given time then true else false
! *** BFTIME - If before given time then true else false

! *** Read Haifa emission data specific variables

      integer :: NEXTUN
      integer :: IUN
      integer :: I1
      integer :: I2
      character(LEN=256) TXTSTR
      character(LEN=3) T1
      real    :: Q1
      logical :: ISOPEN

!MSK start
! *** IF ZERO POINT SOURCES LEAVE
       IF (.not.PSRCFE) RETURN
!MSK end

! *** Perform pre- or postprocessing calculations

      IF (I .EQ. 0) GOTO 100
      IF (I .EQ. 1) GOTO 200

  100 CONTINUE

! *** Preprocessing calculations

! *** Beginning of simulation period?

      IF (ATTIME(BDAT)) THEN

! ***    Open all point sources files
           call oppsrc
! ***    Read old plume segments data
           call roldp

      ENDIF

! *** Changes has occurred to point sources data?
!MSK start
      IF (.NOT. ATTIME(BDAT)) THEN
        IF (.NOT. BFTIME(CHTIME) .OR. .NOT. RONCE) THEN

! ***     Read new point sources data
             !print *,'tspsrc: read new psrc data'
             call rpsrc

! ***     Calculate point sources data
            IF (RUNOK) call cpsrc


        ENDIF

! *** First timestep in current simulation period?

        IF (ITS .EQ. 1) THEN
         !print *,'tspsrc: before cpsrc'
! ***    Calculate point sources data
            IF (RUNOK)  call cpsrc

        ENDIF
      ENDIF
!MSK end

!MSK start
! No treatment of point sources if PSRCMTYPE == 0
      if (PSRCMTYPE .eq. 0) then
        goto 200
      endif
!MSK end


      IF (PSRCMTYPE .GE. 2) THEN

! ***    Beginning of simulation period?
         IF (ATTIME(BDAT)) THEN
! ***       Initialize INPUFF model
!MSK            IF (NQ .GE. 1) CALL INI_INPUFF
              IF (NQ .GE. 1) THEN
                print *,'INPUFF currently not available. STOP'
                stop
              ENDIF 
        ENDIF

      ENDIF


!MSK start
      IF (PSRCMTYPE .EQ. 1) THEN
!MSK end

!MSK start
! ***    Beginning of simulation period?
         IF (ATTIME(BDAT)) THEN

            print *,'tspsrc: newpfn ',NEWPFN
            call opofil(NEWPFN,NEWPUN,NEWPFE,NEWPFM)

         ENDIF
!MSK end

! ***    Run the EPISODE segmented plume model for current timestep
         IF (RUNOK) THEN

! ***       Perform advection and diffusion for all plume segments
            call psadif

            !print *,'tspsrc: before psgene'
! ***       Generate new plume segments
            call psgene

! ***       Transfer deletable plume segments to the grid model
            call psgrid

! ***       Delete all plume segments with no mass
            call psdele

         ENDIF

      ENDIF

! *** Finished with preprocessing
      RETURN

  200 CONTINUE

! *** Postprocessing calculations

!MSK start
! No treatment of point sources if PSRCMTYPE == 0
      if (PSRCMTYPE .eq. 0) then
        goto 300
      endif
!MSK end

! *** If run model not ok then return

      IF (PSRCMTYPE .GE. 2) THEN
      
! ***   Last timestep in current simulation period?
        IF (ITS .EQ. NTS) THEN
! ***       Run the INPUFF model for the current simulation period
            IF (RUNOK) THEN
!MSK                IF (NQ .GE. 1) CALL RUN_INPUFF(PSRCAI)
              IF (NQ .GE. 1) THEN
                print *,'INPUFF currently not available. STOP'
                stop
              ENDIF

                IF (MESSFE) THEN
                    WRITE (MESSUN,2000) NQ
                ENDIF
            ENDIF
        ENDIF

      ENDIF

!MSK start
      IF(PSRCMTYPE .EQ. 1) THEN
!MSK end

! ***   Now the segmented plume model is considered
        IF (RUNOK) THEN
! ***      Calculate plume segments depositions
           call psdepo

! ***      Calculate plume segments radioactive decays
           call psradi

! ***      Last timestep in current simulation period?
           IF (ITS .EQ. NTS) THEN
              IF (MESSFE) THEN
                 WRITE (MESSUN,*)
                 WRITE (MESSUN,2000) NQ
                 WRITE (MESSUN,2010) NP
              ENDIF
!MSK start
! ***      Write new plume segments
              call wnewp
!MSK end

! ***         Calculate plume segments concentrations and depositions
! ***         ... in main grid and subgrid cells
              call csubpx(PSRCAI,PSRCAI,PSRCAI,TSIMRES)

! ***         ... in the irregular receptor points
              call csubpr(PSRCAI,PSRCAI,PSRCAI,TSIMRES)

! ***         ... in the line source associated receptor points
              call csubpl(PSRCAI,PSRCAI,PSRCAI,TSIMRES)

           ENDIF
        ENDIF

! ***   End of simulation period?
        IF (ATDATE(EDAT) .AND. ISH .EQ. NSH .AND. ITS .EQ. NTS) THEN
! ***      Write new plume segments
!MSK plume segments are now written every hour
!MSK            CALL WNEWP
!MSK close the plume segment run file
            call clofil(NEWPFN,NEWPUN,NEWPFE,NEWPFM)

        ENDIF

      ENDIF  ! ELSEIF(PSRCMTYPE .EQ. 1) i.e. Segmented Plume treatment.

  300 CONTINUE

! *** End of simulation period?
      IF (ATDATE(EDAT) .AND. ISH .EQ. NSH .AND. ITS .EQ. NTS) THEN
! ***    Close all point sources files
          call clpsrc
      ENDIF

! *** Finished with postprocessing

      RETURN

 2000 format('TSPSRC: Calculating on ',I6,' point sources')
 2010 format('TSPSRC: Calculating on ',I8,' plume segments')

! *** End of subroutine TSPSRC

      end subroutine tspsrc
