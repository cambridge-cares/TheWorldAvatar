! <cstatr.f90 - A component of the City-scale
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

      subroutine CSTATR

! The subroutine updates receptor concentration statistics.
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
!           2017  M. Karl: Calculation of CRAVED is now inside tsstat.for
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

! Local variables

      real :: CRHIGHDMAX
      real :: CRHIGHDMIN
      real :: CRHIGHHMAX
      real :: CRHIGHHMIN
      real :: TMPV

      integer :: IC
      integer :: IHIGHH
      integer :: IHIGHD
      integer :: IR
      integer :: IV
!MSK
      integer :: IVMAX

! CRHIGHDMAX - Maximum of highest daily  concentration values
! CRHIGHDMIN - Minimum of highest daily  concentration values
! CRHIGHHMAX - Minimum of highest hourly concentration values
! CRHIGHHMIN - Minimum of highest hourly concentration values
! TMPV       - Temporary swap value
! IC         - Compound index
! IHIGHH     - Highest hourly concentrations index
! IHIGHD     - Highest daily  concentrations index
! IR         - Receptor point index
! IV         - Index value
! IVMAX      - Index value

! External functions

!MSK      LOGICAL ATDATE

! If at given date then true else false

! If no receptor points then return

      IF (NR .EQ. 0) RETURN

! UPDATE HIGHEST HOURLY CONCENTRATIONS

! Number of highest hourly values set to zero or missing?

      IF (NHIGHH .LE. 0) GOTO 199

! Go through all compounds

      DO 100 IC = 1,NC

! Only calculate statistics for compound nr. 1

!          IF (IC .GT. 1) GOTO 100
!MSK          IF (IC /= ICSTAT) GOTO 100
!MSK start
!           if ( cmpnd(ic) .ne. cpnstat(1) )  goto 100
!MSK end

! Check if missing data for current compound

          IF (CR(IC,1) .EQ. MISS) GOTO 100

! No!

! Go through all receptor points

          DO 110 IR = 1,NR

! Check if current receptor value is smaller than the smallest
! of the NHIGHH values collected so far

              IV = IRHIGHHMIN(IR)
              IF (CR(IC,IR) .LE. CRHIGHH(IV,IR)) GOTO 110

! No!

! Collect the current receptor value

              CRHIGHH(IV,IR) = CR(IC,IR)

! Find lowest concentration value collected so far

              CRHIGHHMIN = 1.E+30
              DO 120 IHIGHH = 1,NHIGHH
                  IF (CRHIGHH(IHIGHH,IR) .LT. CRHIGHHMIN) THEN
                      CRHIGHHMIN = CRHIGHH(IHIGHH,IR)
                      IV = IHIGHH
                  ENDIF
  120         CONTINUE

! Update the minimum value index

              IRHIGHHMIN(IR) = IV

! Next receptor point

  110     CONTINUE

! Next compound

  100 CONTINUE

  199 CONTINUE


! UPDATE DAILY MEAN CONCENTRATIONS
!MSK For all compounds
!MSK Receptor concentration CRAVED is hourly mean
!MSK Therefore calculation of CRAVED is now inside tsstat.for
!MSK Below is commented
!MSK
!MSK Calculate daily mean indicator set to zero or missing?
!MSK      IF (IAVED .LE. 0) GOTO 299
!MSK Add to daily mean value
!MSK      DO 200 IC = 1,NC
!MSK Check if missing data for current compound
!MSK          IF (CR(IC,1) .EQ. MISS) GOTO 200
!MSK Add to hourly mean arrays
!MSK          DO 210 IR = 1,NR
!MSK              CRAVED(IC,IR) = CRAVED(IC,IR) + CR(IC,IR)
!MSK  210     CONTINUE
!MSK Next compound
!MSK  200 CONTINUE
!MSK start
!MSK      IF (HOUR .NE. 23) GOTO 299
!MSK        DO IC = 1,NC
!MSK         DO 220 IR = 1,NR
!MSK              CRAVED(IC,IR) = CRAVED(IC,IR)/NAVED
!MSK  220     CONTINUE
!MSK        ENDDO
!MSK  299 CONTINUE



! UPDATE OVERALL MEAN CONCENTRATIONS (=whole simulation time average)

! Calculate overall mean indicator set to zero or missing?

      IF (IAVEA .LE. 0) GOTO 399

! Add to overall mean value

      DO 300 IC = 1,NC

! Only add for compound nr. 1

!          IF (IC .GT. 1) GOTO 300
          !IF (IC /= ICSTAT) GOTO 300 !BRUCE: Allows multiple averages

! Check if missing data for current compound

          IF (CR(IC,1) .EQ. MISS) GOTO 300

! Add to overall mean arrays

          DO 310 IR = 1,NR
              CRAVEA(IC,IR) = CRAVEA(IC,IR) + CR(IC,IR)!BRUCE: Allows multiple averages
  310     CONTINUE

! Next compound

  300 CONTINUE

! Calculate overall mean value?

      IF (.NOT. (ATDATE(EDAT) .AND.     &
     ISH .EQ. NSH .AND. ITS .EQ. NTS)) GOTO 399

! Yes!

! Calculate overall mean

      IF (NAVEA .GT. 0) THEN

       DO IC = 1,NC
         DO 330 IR = 1,NR
              CRAVEA(IC,IR) = CRAVEA(IC,IR)/NAVEA !BRUCE: Allows multiple averages
  330     CONTINUE
       ENDDO

      ELSE

       DO IC = 1,NC
        DO 335 IR = 1,NR
              CRAVEA(IC,IR) = MISS
  335     CONTINUE
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

          IF (CRAVED(IC,1) .EQ. MISS) GOTO 400

! No!

! Go through all receptor points

          DO 410 IR = 1,NR

! Check if current receptor value is smaller than the smallest
! of the NHIGHD values collected so far

              IV = IRHIGHDMIN(IR)
              IF (CRAVED(IC,IR) .LE. CRHIGHD(IV,IR)) GOTO 410

! No!

! Collect the current receptor value and update the minimum
! value index

              CRHIGHD(IV,IR) = CRAVED(IC,IR)

! Find lowest concentration value collected so far

              CRHIGHDMIN = 1.E+30
              DO 420 IHIGHD = 1,NHIGHD
                  IF (CRHIGHD(IHIGHD,IR) .LT. CRHIGHDMIN) THEN
                      CRHIGHDMIN = CRHIGHD(IHIGHD,IR)
                      IV = IHIGHD
                  ENDIF
  420         CONTINUE

! Update lowest value index

              IRHIGHDMIN(IR) = IV

! Next receptor point

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

! Sort the receptor point highest hourly concentration values

      DO 500 IR = 1,NR

! Sort the values in descending order

          IHIGHH = 0
  510     CONTINUE

! Initialize current value

          IHIGHH = IHIGHH + 1
          CRHIGHHMAX = CRHIGHH(IHIGHH,IR)
          IVMAX = IHIGHH

! Find highest value in the rest of the array

          DO 520 IV = IHIGHH+1,NHIGHH
              IF (CRHIGHH(IV,IR) .GT. CRHIGHHMAX) THEN
                  CRHIGHHMAX = CRHIGHH(IV,IR)
                  IVMAX = IV
              ENDIF
  520     CONTINUE
      
! Swap current value with highest value

          IF (CRHIGHHMAX .GT. CRHIGHH(IHIGHH,IR)) THEN
              TMPV = CRHIGHH(IHIGHH,IR)
              CRHIGHH(IHIGHH,IR) = CRHIGHHMAX
              CRHIGHH(IVMAX,IR) = TMPV
          ENDIF

! Finished with array?

          IF (IHIGHH .LT. NHIGHH) GOTO 510

! Yes! Array sorted in descending order

           IRHIGHHMIN(IR) = NHIGHH

  500 CONTINUE

  599 CONTINUE

! Number of highest daily values set to zero or missing?

      IF (NHIGHD .LE. 0) GOTO 699

! Sort the receptor point highest daily concentration values

      DO 600 IR = 1,NR

! Sort the values in descending order

          IHIGHD = 0
  610     CONTINUE

! Initialize current value

          IHIGHD = IHIGHD + 1
          CRHIGHDMAX = CRHIGHD(IHIGHD,IR)
          IVMAX = IHIGHD

! Find highest value in the rest of the array

          DO 620 IV = IHIGHD+1,NHIGHD
              IF (CRHIGHD(IV,IR) .GT. CRHIGHDMAX) THEN
                  CRHIGHDMAX = CRHIGHD(IV,IR)
                  IVMAX = IV
              ENDIF
  620     CONTINUE
      
! Swap current value with highest value

          IF (CRHIGHDMAX .GT. CRHIGHD(IHIGHD,IR)) THEN
              TMPV = CRHIGHD(IHIGHD,IR)
              CRHIGHD(IHIGHD,IR) = CRHIGHDMAX
              CRHIGHD(IVMAX,IR) = TMPV
          ENDIF

! Finished with array?

          IF (IHIGHD .LT. NHIGHD) GOTO 610

! Yes! Array sorted in descending order

           IRHIGHDMIN(IR) = NHIGHD

  600 CONTINUE

  699 CONTINUE


! UPDATE TOTAL WET DEPOSITIONS

! Add to overall total value

      DO 700 IC = 1,NC

! Check if missing data for current compound

          IF (WDEPR(IC,1) .EQ. MISS) GOTO 700

! Add to total overall arrays

          DO 710 IR = 1,NR
              WRTOTA(IC,IR) = WRTOTA(IC,IR) + WDEPR(IC,IR)
  710     CONTINUE

! Next compound

  700 CONTINUE


      RETURN

! End of subroutine CSTATR

      end subroutine cstatr
