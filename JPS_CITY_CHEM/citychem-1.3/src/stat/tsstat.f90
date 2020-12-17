! <tsstat.f90 - A component of the City-scale
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

      subroutine TSSTAT(I)

! The subroutine reads, updates and writes concentrations
! statistics data for one timestep.
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
!           2017  M. Karl: updated to allow multiple compound averages.
!                          Calculation of the hourly mean of multiple compounds
!
! ----------------------------------------------------------------------------------

      use mod_util
      use mod_main
      use mod_site
      use mod_time
      use mod_mete
      use mod_conc
      use mod_grid
      use mod_lsrc
      use mod_stat

      implicit none
      
! *** Global variables:

      INTEGER :: I

! I - Indicator of pre (0) or post (1) processing

! Local variables

      REAL :: CV

      INTEGER :: IC
      INTEGER :: IR
      INTEGER :: IQL
      INTEGER :: IX
      INTEGER :: IY
      INTEGER :: IHIGHH
      INTEGER :: IHIGHD

! CV     - Concentration value
! IC     - Index of compound
! IR     - Irregular receptor point index
! IQL    - Line associated receptor point index
! IX     - Main grid index in x-direction
! IY     - Main grid index in y-direction
! IHIGHH - Highest hourly concentration counter
! IHIGHD - Highesat daily concentration counter

! External functions

!MSK      LOGICAL ATDATE
!MSK      LOGICAL ATTIME

! ATDATE - If at given date then true else false
! ATTIME - If at given time then true else false

!MSK start 
! allocate CMAVED, CRAVED, CLAVED
! cmaved - Main grid average daily   field values (nx,ny)
! craved - Receptor points average daily   values (nr)
! claved - Line receptor points average daily   values (nql)
      IF (.NOT. ALLOCATED(CMAVED))         ALLOCATE( CMAVED(NX,NY,NC) )
      IF (.NOT. ALLOCATED(CRAVED))         ALLOCATE( CRAVED(NC,NR) )
      IF (NQL .GT. 0) THEN
          IF (.NOT. ALLOCATED(CLAVEA))     ALLOCATE(CLAVEA(NQL))
          IF (.NOT. ALLOCATED(CLAVED))     ALLOCATE(CLAVED(NQL))
          IF (.NOT. ALLOCATED(ILHIGHDMIN)) ALLOCATE(ILHIGHDMIN(NQL))
          IF (.NOT. ALLOCATED(ILHIGHHMIN)) ALLOCATE(ILHIGHHMIN(NQL))
      ENDIF
      IF (NHIGHD .GT. 0 .AND. NQL .GT. 0) THEN
          IF (.NOT. ALLOCATED(CLHIGHD)) ALLOCATE(CLHIGHD(NHIGHD,NQL))
      ENDIF
      IF (NHIGHH .GT. 0 .AND. NQL .GT. 0) THEN
          IF (.NOT. ALLOCATED(CLHIGHH)) ALLOCATE(CLHIGHH(NHIGHH,NQL))
      ENDIF
!MSK end

! Perform pre- or postprocessing calculations

      IF (I .EQ. 0) GOTO 100
      IF (I .EQ. 1) GOTO 200

  100 CONTINUE

! *** Preprocessing calculations

! *** Beginning of simulation period?

      IF (ATTIME(BDAT)) THEN

! Initialize number of hours for the daily mean and overall mean
! calculations

          NAVED = 0
          NAVEA = 0
!MSK start hourly mean counter
          naveh = 0
!MSK end

!       Open all concentrations files

         print *,'tsstat before opstat'
         call opstat
         print *,'tsstat after opstat'

! Initialize the daily mean and overall mean main grid and
! receptor points arrays

          DO IC = 1,NC
          
          DO IY = 1,NY
            DO IX = 1,NX
              CMAVED(IX,IY,IC) = 0.
              CMAVEA(IX,IY,IC) = 0.
            ENDDO
          ENDDO

          DO IR = 1,NR
              CRAVED(IC,IR) = 0.
              CRAVEA(IC,IR) = 0.
          ENDDO

          ENDDO

!MSK start
          if (NQL > 0) then

            DO IQL = 1,NQL
                CLAVED(IQL) = 0.
                CLAVEA(IQL) = 0.
            ENDDO

          else

            print *,'TSTAT: No Line source emissions. Stopped here'
            call STOPIT('Provide line source meta file with at least one line source')

          endif
!MSK end

! Initialize the hourly and daily mean main grid and receptor
! points highest concentrations statistics arrays

          DO IY = 1,NY
            DO IX = 1,NX
              DO IHIGHH = 1,NHIGHH
                  CMHIGHH(IHIGHH,IX,IY) = MISS
              ENDDO
              DO IHIGHD = 1,NHIGHD
                  CMHIGHD(IHIGHD,IX,IY) = MISS
              ENDDO
              IMHIGHHMIN(IX,IY) = 1
              IMHIGHDMIN(IX,IY) = 1
          ENDDO
          ENDDO

          DO IR = 1,NR
              DO IHIGHH = 1,NHIGHH
                  CRHIGHH(IHIGHH,IR) = MISS
              ENDDO
              DO IHIGHD = 1,NHIGHD
                  CRHIGHD(IHIGHD,IR) = MISS
              ENDDO
              IRHIGHHMIN(IR) = 1
              IRHIGHDMIN(IR) = 1
          ENDDO

          DO IQL = 1,NQL
              DO IHIGHH = 1,NHIGHH
                  CLHIGHH(IHIGHH,IQL) = MISS
              ENDDO
              DO IHIGHD = 1,NHIGHD
                  CLHIGHD(IHIGHD,IQL) = MISS
              ENDDO
              ILHIGHHMIN(IQL) = 1
              ILHIGHDMIN(IQL) = 1
          ENDDO

! Initialize the total overall deposition main grid and
! receptor points arrays

          DO IC = 1,NC
          DO IY = 1,NY
            DO IX = 1,NX
              WMTOTA(IX,IY,IC) = 0.
            ENDDO
          ENDDO
          ENDDO

          DO IR = 1,NR
          DO IC = 1,NC
              WRTOTA(IC,IR) = 0.
          ENDDO
          ENDDO

! Open all concentrations statistics files

!C        CALL OPSTAT

      ENDIF  ! IF (ATTIME(BDAT)), i.e. Endif at the beginning of sim.


! First timestep in a new day?

      IF (HOUR .EQ. 0 .AND. ISH .EQ. 1 .AND. ITS .EQ. 1) THEN

! Initialize number of hours for the daily mean calculations

          NAVED = 0
!MSK start hourly mean counter
          naveh = 0
!MSK end

! Initialize the daily mean main grid and receptor points arrays

          DO IC = 1,NC
          
            DO IY = 1,NY
              DO IX = 1,NX
                CMAVED(IX,IY,IC) = 0.
              ENDDO
            ENDDO

            DO IR = 1,NR
                CRAVED(IC,IR) = 0.
            ENDDO

          ENDDO

          if (NQL > 0) then
       
            DO IQL = 1,NQL
              CLAVED(IQL) = 0.
            ENDDO

          endif

! Initialize the hourly main grid and receptor points highest
! concentrations statistics arrays

          IF (IDSTA .EQ. 1) THEN

! Zero out highest hourly statistics

              DO IY = 1,NY
              DO IX = 1,NX
                  DO IHIGHH = 1,NHIGHH
                      CMHIGHH(IHIGHH,IX,IY) = MISS
                  ENDDO
                  IMHIGHHMIN(IX,IY) = 1
              ENDDO
              ENDDO

              DO IR = 1,NR
                  DO IHIGHH = 1,NHIGHH
                      CRHIGHH(IHIGHH,IR) = MISS
                  ENDDO
                  IRHIGHHMIN(IR) = 1
              ENDDO

              DO IQL = 1,NQL
                  DO IHIGHH = 1,NHIGHH
                      CLHIGHH(IHIGHH,IQL) = MISS
                  ENDDO
                  ILHIGHHMIN(IQL) = 1
              ENDDO

          ENDIF

      ENDIF  ! If first timestep in a new day,
             ! i.e.: IF (HOUR .EQ. 0 .AND. ISH .EQ. 1 .AND. ITS .EQ. 1)

! Finished with preprocessing

      RETURN

  200 CONTINUE

! Postprocessing calculations

!MSK start
! UPDATE HOURLY MEAN CONCENTRATIONS
! Update for irregular receptor points
! for calculation of the hourly mean
! For all compounds
! Receptor concentration CRAVED is hourly mean
! Main grid concentration CMAVED is hourly mean

! *** First timestep in a new hour
! *** Initialize craved and naveh
      if (ISH .EQ. 1)  then 
          naveh = 0
          do IC = 1,NC
            do IR = 1,NR
              CRAVED(IC,IR) = 0.
            enddo
          enddo
! *** Initialize cmaved
          do IC = 1,NC
            do IY = 1,NY
              do IX = 1,NX
                CMAVED(IX,IY,IC) = 0.
              enddo
            enddo
          enddo
      endif

! *** Calculate daily mean indicator set to zero or missing?

      if (IAVED .gt. 0) then

         do 300 IC = 1,NC

! *** Update hourly mean counter

           if(IC.eq.1)   naveh = naveh + 1

! *** Add receptor concentration to hourly mean arrays every timestep

           do 310 IR = 1,NR

!MSK start
               CR(IC,IR) = MAX(CR(IC,IR), dble(1.e-20))
!MSK end
               CRAVED(IC,IR) = CRAVED(IC,IR) + CR(IC,IR)

!MSK debug start
!MSK debug:   Check hourly NO and PM2.5 at receptor
!MSK debug               if ((ir.eq.10000).and.(ic.eq.2)) then
!MSK debug                 print *,'tsstat CR hourly NO ',   naveh, CRAVED(ic,ir), CR(ic,ir)
!MSK debug               endif
!MSK debug               if ((ir.eq.10000).and.(ic.eq.18)) then
!MSK debug                 print *,'tsstat CR hourly PM2.5 ',naveh, CRAVED(ic,ir), CR(ic,ir)
!MSK debug              endif
!MSK debug end


  310       continue

! ** Add main grid concentration to daily mean arrays

           do 330 IY = 1,NY
             do 340 IX = 1,NX
!MSK start
                if (CM(IX,IY,IC).lt.1.e-32) then
                  CM(IX,IY,IC) = 0.0
                endif
!MSK end
                CMAVED(IX,IY,IC) = CMAVED(IX,IY,IC) + CM(IX,IY,IC)
  340       continue
  330     continue

! *** Next compound
  300    continue

! Last timestep in current hour?

         if (ISH .EQ. NSH .AND. ITS .EQ. NTS) then

            do IC = 1,NC
              do 320 IR = 1,NR

                 CRAVED(IC,IR) = CRAVED(IC,IR)/naveh

  320         continue

              do IY = 1,NY
                do IX = 1,NX
                  CMAVED(IX,IY,IC) = CMAVED(IX,IY,IC)/naveh

                  if ( CMAVED(IX,IY,IC) .lt. 0.0 ) THEN
                    print *,'tsstat: CMAVED < 0', IC,IX,IY,CMAVED(IX,IY,IC)
                    call STOPIT('Stopped in tsstat because CMAVED<0')
                  endif

                enddo
              enddo

            enddo

         endif

      endif    !iaved
  
!MSK end

! Last timestep in current hour?

      IF (ISH .EQ. NSH .AND. ITS .EQ. NTS) THEN

! Calculate statistics 

! ... for main grid

          call cstatm

! ... for irregular receptor points

          call cstatr

! ... for line receptor points

          call cstatl

!MSK start
! Write the hourly mean value for the main grid

          call wstatm

! Write the hourly mean value for irregular receptor points

          call wstatr
!MSK end

      ENDIF


! End of current day?

    IF (HOUR .EQ. 23 .AND. ISH .EQ. NSH .AND. ITS .EQ. NTS) THEN

! Write out daily mean values to files

! ... for the main grid cells
!MSK output is written on hourly basis now
!MSK        call wstatm

! ... for the irregular receptor points
!MSK output is written on hourly basis now
!MSK        call wstatr

! ... for the line source receptor points

         call wstatl

    ENDIF

! End of simulation period?

    IF (ATDATE(EDAT) .AND. ISH .EQ. NSH .AND. ITS .EQ. NTS) THEN

! Write out overall mean values to files

! ... for the main grid cells

         call wcmavea


! ... for the irregular receptor points

         call wcravea


! Write out all highest hourly concentration values

! ... for the main grid cells

!C future       CALL WCMHIGHH

! ... for the receptor points

!C future       CALL WCRHIGHH

! Write out all highest daily concentration values

! ... for the main grid cells

!C future       CALL WCMHIGHD

! ... for the receptor points

!C future       CALL WCRHIGHD

! Close all concentration statistics files

          call clstat

      ENDIF

! Finished with postprocessing

      RETURN

! End of subroutine TSSTAT

      end subroutine tsstat
