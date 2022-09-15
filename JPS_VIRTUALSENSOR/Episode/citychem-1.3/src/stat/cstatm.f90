! <cstatm.f90 - A component of the City-scale
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

      subroutine CSTATM

! The subroutine updates main grid concentration statistics.
! Changed this routine so that averages (only) are calculated for all compounds 1:NC
! Should be further updated for the other statistics
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
!           2017  M. Karl: Calculation of CMAVED is now inside tsstat.for
!
! ----------------------------------------------------------------------------------

      use mod_util
      use mod_main
      use mod_site
      use mod_time
      use mod_mete
      use mod_conc
      use mod_depo
      use mod_stat

      implicit none

! *** Local variables

      real    :: CMHIGHDMAX
      real    :: CMHIGHHMAX
      real    :: CMHIGHDMIN
      real    :: CMHIGHHMIN
      real    :: TMPV

      integer :: IC
      integer :: IHIGHH
      integer :: IHIGHD
      integer :: IV
      integer :: IVMAX
      integer :: IX
      integer :: IY

! CMHIGHDMAX - Maximum of highest daily  concentration values
! CMHIGHDMIN - Minimum of highest daily  concentration values
! CMHIGHHMAX - Maximum of highest daily  concentration values
! CMHIGHHMIN - Minimum of highest hourly concentration values
! TMPV       - Temporary swap value
! IC         - Compound index
! IHIGHH     - Highest hourly concentrations index
! IHIGHD     - Highest daily  concentrations index
! IV         - Index value
! IVMAX      - Index value
! IX         - Main grid index in x-direction
! IY         - Main grid index in y-direction

! *** External functions:

!MSK      LOGICAL ATDATE

! *** If no grid cells then return

      IF (NX .EQ. 0 .OR. NY .EQ. 0) RETURN

! *** UPDATE HIGHEST HOURLY CONCENTRATIONS:

! *** Number of highest hourly values set to zero or missing?
! *** Then skip the treatment of the NHIGHH highest conc.

      IF (NHIGHH .LE. 0) GOTO 199

! *** Go through all compounds

      DO 100 IC = 1,NC

! ***   Only calculate statistics for compound nr. 1:
!        IF (IC .GT. 1) GOTO 100
!MSK        IF (IC /= ICSTAT) GOTO 100
!MSK start
!           if ( cmpnd(ic) .ne. cpnstat(1) )  goto 100
!MSK end

! ***   Check if missing data for current compound, then go to next compund.
        IF (CM(1,1,IC) .EQ. MISS) GOTO 100

! ***   Go through all main grid cells:
        DO 110 IY = 1,NY
          DO 120 IX = 1,NX

! ***       Check if current main grid value is .LE. than the smallest of
! ***       the NHIGHH values collected so far, if so go to next grid cell:

            IV = IMHIGHHMIN(IX,IY)
            IF (CM(IX,IY,IC) .LE. CMHIGHH(IV,IX,IY)) GOTO 120

! ***       No!

! ***       Collect the current main grid value

            CMHIGHH(IV,IX,IY) = CM(IX,IY,IC)

! ***       Find lowest concentration value collected so far

            CMHIGHHMIN = 1.E+30
            DO 130 IHIGHH = 1,NHIGHH
              IF (CMHIGHH(IHIGHH,IX,IY) .LT. CMHIGHHMIN) THEN
                CMHIGHHMIN = CMHIGHH(IHIGHH,IX,IY)
                IV = IHIGHH
              ENDIF
  130       CONTINUE

! ***       Update the minimum value index

            IMHIGHHMIN(IX,IY) = IV

! ***       Next main grid cell

  120     CONTINUE
  110   CONTINUE

! ***   Next compound

  100 CONTINUE

  199 CONTINUE


! *** UPDATE DAILY MEAN CONCENTRATIONS

! *** Calculate daily mean indicator set to zero or missing?

      IF (IAVED .LE. 0) GOTO 299

      DO 200 IC = 1,NC

!MSK start
!MSK For all compounds
!MSK Receptor concentration CMAVED is hourly mean
!MSK Therefore calculation of CMAVED is now inside tsstat.for
!MSK Below is commented
!MSK
!MSK          DO IY = 1,NY
!MSK            DO IX = 1,NX
!MSK             IF (CM(IX,IY,IC) .lt. 0.0) THEN
!MSK               print *,'cstatm: CM(IX,IY,IC)', CM(IX,IY,IC)
!MSK               print *,'cstatm: STOPPED'
!MSK               STOP
!MSK             ENDIF             
!MSK            ENDDO
!MSK          ENDDO
!MSK
!MSK ! Add to daily mean arrays
!MSK
!MSK          DO 210 IY = 1,NY
!MSK            DO 220 IX = 1,NX
!MSK              CMAVED(IX,IY,IC) = CMAVED(IX,IY,IC) + CM(IX,IY,IC)
!MSK  220       CONTINUE
!MSK  210     CONTINUE
!MSK end

!MSK start
! *** Set the NAVED counter for the daily mean here
          IF (IC .eq. 1)  NAVED = NAVED + 1
!MSK end

! Next compound

  200 CONTINUE


      IF (HOUR .NE. 23) GOTO 299

!MSK Below is commented
!MSK ! Calculate daily mean value?
!MSK  ! Yes!
!MSK  ! Calculate daily mean
!MSK
!MSK      IF (NAVED .GE. 20) THEN
!MSK
!MSK          DO IC = 1,NC
!MSK          DO 230 IY = 1,NY
!MSK          DO 240 IX = 1,NX
!MSK              CMAVED(IX,IY,IC) = CMAVED(IX,IY,IC)/NAVED
!MSK                IF ( CMAVED(IX,IY,IC) .lt. 0.0 ) THEN
!MSK                  print *,'CSTATM: CMAVED < 0', IC,IX,IY,CMAVED(IX,IY,IC)
!MSK                  print *,'CSTATM: stopped'
!MSK                  STOP
!MSK                ENDIF
!MSK  240     CONTINUE
!MSK  230     CONTINUE
!MSK
!MSK          ENDDO
!MSK
!MSK
!MSK      ELSE
!MSK               print *,'cstatm: NAVED .lt. 20', NAVED
!MSK               print *,'cstatm: STOPPED'
!MSK               STOP
!MSK
!MSK      ENDIF

  299 CONTINUE


! UPDATE OVERALL MEAN CONCENTRATIONS

! Calculate overall mean indicator set to zero or missing?

      IF (IAVEA .LE. 0) GOTO 399

! Add to overall mean value

      DO 300 IC = 1,NC

! Only add for compound nr. 1

!          IF (IC .GT. 1) GOTO 300
!          IF (IC /= ICSTAT) GOTO 300


! Check if missing data for current compound

!MSK start
!MSK          IF (CM(1,1,IC) .EQ. MISS) GOTO 300
          DO IY = 1,NY
            DO IX = 1,NX
             IF (CM(IX,IY,IC) .lt. 0.0) THEN
               print *,'cstatm: overall mean CM(IX,IY,IC)', CM(IX,IY,IC)
               print *,'cstatm: STOPPED'
               STOP
             ENDIF             
            ENDDO
          ENDDO
!MSK end


! Add to overall mean arrays

          DO 310 IY = 1,NY
          DO 320 IX = 1,NX
              CMAVEA(IX,IY,IC) = CMAVEA(IX,IY,IC) + CM(IX,IY,IC)
  320     CONTINUE
  310     CONTINUE

!MSK start
!      NAVEA = NAVEA + 1
          IF (IC .eq. 1)  NAVEA = NAVEA + 1
!MSK end

! Next compound

  300 CONTINUE



! Calculate overall mean value?

      IF (.NOT. (ATDATE(EDAT) .AND.     &
       ISH .EQ. NSH .AND. ITS .EQ. NTS)) GOTO 399

! Yes!

! Calculate overall mean

      IF (NAVEA .GT. 0) THEN

          DO IC = 1,NC
          DO 350 IY = 1,NY
          DO 360 IX = 1,NX
              CMAVEA(IX,IY,IC) = CMAVEA(IX,IY,IC)/NAVEA
  360     CONTINUE
  350     CONTINUE
          ENDDO

      ELSE

          DO IC = 1,NC
           DO 355 IY = 1,NY
            DO 365 IX = 1,NX
              CMAVEA(IX,IY,IC) = MISS
  365     CONTINUE
  355     CONTINUE
          ENDDO

      ENDIF

  399 CONTINUE


! UPDATE HIGHEST DAILY CONCENTRATIONS

! Number of highest daily values set to zero or missing?

      IF (NHIGHD .LE. 0) GOTO 499

! Go through all compounds

      DO 400 IC = 1,NC

! Only calculate statistics for compound nr. 1

!          IF (IC .GT. 1) GOTO 400
!MSK          IF (IC /= ICSTAT) GOTO 400
!MSK start
!           if ( cmpnd(ic) .ne. cpnstat(1) )  goto 400
!MSK end

! Calculate statistics?

          IF (HOUR .NE. 23) GOTO 400

! Yes!

! Check if missing data

          IF (CMAVED(1,1,IC) .EQ. MISS) GOTO 400

! No!

! Go through all main grid cells

          DO 410 IY = 1,NY
          DO 420 IX = 1,NX

! Check if current main grid value is smaller than the smallest
! of the NHIGHD values collected so far


!MSK: ATTENTION CMHIGHD has to be changed to index IX,IY,IC

              IV = IMHIGHDMIN(IX,IY)
              IF (CMAVED(IX,IY,IC) .LE. CMHIGHD(IV,IX,IY)) GOTO 420

! No!

! Collect the current main grid value and update the minimum
! value index

              CMHIGHD(IV,IX,IY) = CMAVED(IX,IY,IC)

! Find lowest concentration value collected so far

              CMHIGHDMIN = 1.E+30
              DO 430 IHIGHD = 1,NHIGHD
                  IF (CMHIGHD(IHIGHD,IX,IY) .LT. CMHIGHDMIN) THEN
                      CMHIGHDMIN = CMHIGHD(IHIGHD,IX,IY)
                      IV = IHIGHD
                  ENDIF
  430         CONTINUE

! Update lowest value index

              IMHIGHDMIN(IX,IY) = IV

! Next main grid cell

  420     CONTINUE
  410     CONTINUE

! Next compound

  400 CONTINUE

  499 CONTINUE


! SORT HIGHEST HOURLY AND DAILY CONCENTRATIONS

! Check if at end of simulations or daily statistics of highest
! hourly concentrations wanted

         IF (.NOT. ((ATDATE(EDAT) .AND.     &
             ISH .EQ. NSH .AND. ITS .EQ. NTS) .OR.     &
            (HOUR .EQ. 23 .AND. IDSTA .EQ. 1))) RETURN

! Yes!

! Number of highest hourly values set to zero or missing?

      IF (NHIGHH .LE. 0) GOTO 599

! Sort the main grid highest hourly concentration values

      DO 500 IY = 1,NY
        DO 510 IX = 1,NX

! Sort the values in descending order

          IHIGHH = 0
  520     CONTINUE

! Initialize current value

          IHIGHH = IHIGHH + 1
          CMHIGHHMAX = CMHIGHH(IHIGHH,IX,IY)
          IVMAX = IHIGHH

! Find highest value in the rest of the array

          DO 530 IV = IHIGHH+1,NHIGHH
              IF (CMHIGHH(IV,IX,IY) .GT. CMHIGHHMAX) THEN
                  CMHIGHHMAX = CMHIGHH(IV,IX,IY)
                  IVMAX = IV
              ENDIF
  530     CONTINUE
      
! Swap current value with highest value

          IF (CMHIGHHMAX .GT. CMHIGHH(IHIGHH,IX,IY)) THEN
              TMPV = CMHIGHH(IHIGHH,IX,IY)
              CMHIGHH(IHIGHH,IX,IY) = CMHIGHHMAX
              CMHIGHH(IVMAX,IX,IY) = TMPV
          ENDIF

! Finished with array?

          IF (IHIGHH .LT. NHIGHH) GOTO 520

! Yes! Array sorted in descending order

           IMHIGHHMIN(IX,IY) = NHIGHH

  510 CONTINUE
  500 CONTINUE

  599 CONTINUE

! Number of highest daily values set to zero or missing?

      IF (NHIGHD .LE. 0) GOTO 699

! Sort the main grid highest daily concentration values

      DO 600 IY = 1,NY
        DO 610 IX = 1,NX

! Sort the values in descending order

          IHIGHD = 0
  620     CONTINUE

! Initialize current value

          IHIGHD = IHIGHD + 1
          CMHIGHDMAX = CMHIGHD(IHIGHD,IX,IY)
          IVMAX = IHIGHD

! Find highest value in the rest of the array

          DO 630 IV = IHIGHD+1,NHIGHD
              IF (CMHIGHD(IV,IX,IY) .GT. CMHIGHDMAX) THEN
                  CMHIGHDMAX = CMHIGHD(IV,IX,IY)
                  IVMAX = IV
              ENDIF
  630     CONTINUE
      
! Swap current value with highest value

          IF (CMHIGHDMAX .GT. CMHIGHD(IHIGHD,IX,IY)) THEN
              TMPV = CMHIGHD(IHIGHD,IX,IY)
              CMHIGHD(IHIGHD,IX,IY) = CMHIGHDMAX
              CMHIGHD(IVMAX,IX,IY) = TMPV
          ENDIF

! Finished with array?

          IF (IHIGHD .LT. NHIGHD) GOTO 620

! Yes! Array sorted in descending order

           IMHIGHDMIN(IX,IY) = NHIGHD

  610 CONTINUE
  600 CONTINUE

  699 CONTINUE


! UPDATE TOTAL WET DEPOSITIONS

! Add to overall total value

      DO 700 IC = 1,NC

! Check if missing data for current compound

          IF (WDEPM(1,1,IC) .EQ. MISS) GOTO 700

! Add to total overall arrays

          DO 710 IY = 1,NY
          DO 720 IX = 1,NX
              WMTOTA(IX,IY,IC) = WMTOTA(IX,IY,IC) + WDEPM(IX,IY,IC)
  720     CONTINUE
  710     CONTINUE

! Next compound

  700 CONTINUE

      RETURN

! End of subroutine CSTATM

      end subroutine cstatm
