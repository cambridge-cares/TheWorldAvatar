! <cstatl.f90 - A component of the City-scale
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

      subroutine CSTATL

! The subroutine updates line receptor concentration statistics.
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
!           2016  M. Karl: Changed to write only NO2 to line receptor file.
!                          Change in the array handling for calculating overall mean
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
      use mod_stat

      implicit none

! Local variables

      real,allocatable :: CL(:,:)

      real :: CLHIGHDMAX
      real :: CLHIGHDMIN
      real :: CLHIGHHMAX
      real :: CLHIGHHMIN
      real :: TMPV

      integer :: IC
      integer :: IHIGHH
      integer :: IHIGHD
      integer :: IQL
      integer :: IV
      integer :: IVMAX
!MSK
      real :: cliciql

! CLHIGHDMAX - Maximum of highest daily  concentration values
! CLHIGHDMIN - Minimum of highest daily  concentration values
! CLHIGHHMAX - Minimum of highest hourly concentration values
! CLHIGHHMIN - Minimum of highest hourly concentration values
! TMPV       - Temporary swap value
! IC         - Compound index
! IHIGHH     - Highest hourly concentrations index
! IHIGHD     - Highest daily  concentrations index
! IQL        - Line receptor point index
! IV         - Index value
! IVMAX      - Index value

! External functions

!MSK      LOGICAL ATDATE

! If at given date then true else false

! If no line receptor points then return
! nrl  - Number of line sources receptor points (2*nql)

      IF (NRL .EQ. 0) RETURN
!MSK start
      IF (NQL .EQ. 0) RETURN

! Allocate temporary array for concentrations
! nql  - Number of line sources
!MSK      IF (NQL .GT. 0) THEN
         IF (.NOT. ALLOCATED(CL))  ALLOCATE( CL(NC,NQL)  )
!MSK      ENDIF
!MSK end

      DO IQL = 1,NQL
       DO IC = 1,NC

          CL(IC,IQL) = 0.0
          IF (QLRI(IQL) .GT. 0) THEN
              CL(IC,IQL) = MAX(CRL(IC,2*IQL-1),CRL(IC,2*IQL))
          ELSE
              CL(IC,IQL) = MISS
          ENDIF
       ENDDO
      ENDDO


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
          if ( cmpnd(ic) .ne. 'NO2       ' )  goto 100
!MSK end

! Go through all receptor points

          DO 110 IQL = 1,NQL

! Check if missing data

              IF (CL(IC,IQL) .EQ. MISS) GOTO 110

! No!

! Check if current receptor value is smaller than the smallest
! of the NHIGHH values collected so far

              IV = ILHIGHHMIN(IQL)
              IF (CL(IC,IQL) .LE. CLHIGHH(IV,IQL)) GOTO 110

! No!

! Collect the current receptor value

              CLHIGHH(IV,IQL) = CL(IC,IQL)

! Find lowest concentration value collected so far

              CLHIGHHMIN = 1.E+30
              DO 120 IHIGHH = 1,NHIGHH
                  IF (CLHIGHH(IHIGHH,IQL) .LT. CLHIGHHMIN) THEN
                      CLHIGHHMIN = CLHIGHH(IHIGHH,IQL)
                      IV = IHIGHH
                  ENDIF
  120         CONTINUE

! Update the minimum value index

              ILHIGHHMIN(IQL) = IV

! Next line receptor point

  110     CONTINUE

! Next compound

  100 CONTINUE

  199 CONTINUE



! UPDATE DAILY MEAN CONCENTRATIONS

! Calculate daily mean indicator set to zero or missing?

      IF (IAVED .LE. 0) GOTO 299

! Add to daily mean value

      cliciql = 0.0

      DO 200 IC = 1,NC

! Only add for compound nr. 1

!          IF (IC .GT. 1) GOTO 200
!MSK          IF (IC /= ICSTAT) GOTO 200
!MSK start
!          if ( cmpnd(ic) .ne. cpnstat(1) )  goto 200
          if ( cmpnd(ic) .ne. 'NO2       ' )  goto 200
!MSK end

          DO 210 IQL = 1,NQL

! Check if missing data for current compound

              IF (CL(IC,IQL) .EQ. MISS) GOTO 210

!MSK temporary storage
              cliciql = CL(IC,IQL)

! Add to daily mean arrays

!MSK              CLAVED(IQL) = CLAVED(IQL) + CL(IC,IQL)
              CLAVED(IQL) = CLAVED(IQL) + cliciql

  210     CONTINUE

! Next compound

  200 CONTINUE


! Calculate daily mean value?

      IF (HOUR .NE. 23) GOTO 299

! Yes!

! Calculate daily mean

      IF (NAVED .GE. 20) THEN

          DO 220 IQL = 1,NQL
              IF (QLRI(IQL) .GT. 0) THEN
                  CLAVED(IQL) = CLAVED(IQL)/NAVED
              ELSE
                  CLAVED(IQL) = MISS
              ENDIF
  220     CONTINUE

      ELSE

          DO 225 IQL = 1,NQL
              CLAVED(IQL) = MISS
  225     CONTINUE

      ENDIF

  299 CONTINUE


! UPDATE OVERALL MEAN CONCENTRATIONS

! Calculate overall mean indicator set to zero or missing?

      IF (IAVEA .LE. 0) GOTO 399


!MSK
! clavea - Line receptor points average overall values (nc,nql)
      IF (.NOT. ALLOCATED(CLAVEA))  ALLOCATE( CLAVEA(NQL) )

! Add to overall mean value

      cliciql = 0.0

      DO 300 IC = 1,NC

! Only add for compound nr. 1

!          IF (IC .GT. 1) GOTO 300
!MSK          IF (IC /= ICSTAT) GOTO 300
!MSK start
!           if ( cmpnd(ic) .ne. cpnstat(1) )  goto 300
           if ( cmpnd(ic) .ne. 'NO2       ' )  goto 300
!MSK end

          DO 310 IQL = 1,NQL


! Check if missing data for current compound

              IF (CL(IC,IQL) .EQ. MISS) GOTO 310

!MSK temporary storage
              cliciql = CL(IC,IQL)

! Add to overall mean arrays

!MSK              CLAVEA(IQL) = CLAVEA(IQL) + CL(IC,IQL)
              CLAVEA(IQL) = CLAVEA(IQL) + cliciql

  310     CONTINUE

! Next compound

  300 CONTINUE

! Calculate overall mean value?

      IF (.NOT. (ATDATE(EDAT) .AND.     &
     ISH .EQ. NSH .AND. ITS .EQ. NTS)) GOTO 399

! Yes!


! Calculate overall mean

      IF (NAVEA .GT. 0) THEN

          DO 330 IQL = 1,NQL
              IF (QLRI(IQL) .GT. 0) THEN
                  CLAVEA(IQL) = CLAVEA(IQL)/NAVEA
              ELSE
!MSK                  CLAVEA(IQL) = CLAVEA(IQL)/NAVEA
                  CLAVEA(IQL) = MISS
              ENDIF
  330     CONTINUE

      ELSE

          DO 335 IQL = 1,NQL
              CLAVEA(IQL) = MISS
  335     CONTINUE

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
           if ( cmpnd(ic) .ne. 'NO2       ' )  goto 400
!MSK end   

! Calculate statistics?

          IF (HOUR .NE. 23) GOTO 400

! Yes!

! Go through all line receptor points

          DO 410 IQL = 1,NQL

! Check if missing data

          IF (CLAVED(IQL) .EQ. MISS) GOTO 410

! No!

! Check if current line receptor value is smaller than the smallest
! of the NHIGHD values collected so far

              IV = ILHIGHDMIN(IQL)
              IF (CLAVED(IQL) .LE. CLHIGHD(IV,IQL)) GOTO 410

! No!

! Collect the current receptor value and update the minimum
! value index

              CLHIGHD(IV,IQL) = CLAVED(IQL)

! Find lowest concentration value collected so far

              CLHIGHDMIN = 1.E+30
              DO 420 IHIGHD = 1,NHIGHD
                  IF (CLHIGHD(IHIGHD,IQL) .LT. CLHIGHDMIN) THEN
                      CLHIGHDMIN = CLHIGHD(IHIGHD,IQL)
                      IV = IHIGHD
                  ENDIF
  420         CONTINUE

! Update lowest value index

              ILHIGHDMIN(IQL) = IV

! Next line receptor point

  410     CONTINUE

! Next compound

  400 CONTINUE

  499 CONTINUE


! SORT HIGHEST HOURLY AND DAILY CONCENTRATIONS

! Check if at end of calculations

      IF (.NOT. (ATDATE(EDAT) .AND.     &
     ISH .EQ. NSH .AND. ITS .EQ. NTS)) RETURN

! Yes!

! Number of highest hourly values set to zero or missing?

      IF (NHIGHH .LE. 0) GOTO 599

! Sort the line receptor point highest hourly concentration values

      DO 500 IQL = 1,NQL

! Skip sorting if not line receptor point

          IF (QLRI(IQL) .EQ. 0) GOTO 500

! Sort the values in descending order

          IHIGHH = 0
  510     CONTINUE

! Initialize current value

          IHIGHH = IHIGHH + 1
          CLHIGHHMAX = CLHIGHH(IHIGHH,IQL)
          IVMAX = IHIGHH

! Find highest value in the rest of the array

          DO 520 IV = IHIGHH+1,NHIGHH
              IF (CLHIGHH(IV,IQL) .GT. CLHIGHHMAX) THEN
                  CLHIGHHMAX = CLHIGHH(IV,IQL)
                  IVMAX = IV
              ENDIF
  520     CONTINUE
      
! Swap current value with highest value

          IF (CLHIGHHMAX .GT. CLHIGHH(IHIGHH,IQL)) THEN
              TMPV = CLHIGHH(IHIGHH,IQL)
              CLHIGHH(IHIGHH,IQL) = CLHIGHHMAX
              CLHIGHH(IVMAX,IQL) = TMPV
          ENDIF

! Finished with array?

          IF (IHIGHH .LT. NHIGHH) GOTO 510

! Yes! Array sorted in descending order

           ILHIGHHMIN(IQL) = NHIGHH

  500 CONTINUE

  599 CONTINUE

! Number of highest daily values set to zero or missing?

      IF (NHIGHD .LE. 0) GOTO 699

! Sort the receptor point highest daily concentration values

      DO 600 IQL = 1,NQL

! Skip sorting if not a line receptor point

          IF (QLRI(IQL) .EQ. 0) GOTO 600

! Sort the values in descending order

          IHIGHD = 0
  610     CONTINUE

! Initialize current value

          IHIGHD = IHIGHD + 1
          CLHIGHDMAX = CLHIGHD(IHIGHD,IQL)
          IVMAX = IHIGHD

! Find highest value in the rest of the array

          DO 620 IV = IHIGHD+1,NHIGHD
              IF (CLHIGHD(IV,IQL) .GT. CLHIGHDMAX) THEN
                  CLHIGHDMAX = CLHIGHD(IV,IQL)
                  IVMAX = IV
              ENDIF
  620     CONTINUE
      
! Swap current value with highest value

          IF (CLHIGHDMAX .GT. CLHIGHD(IHIGHD,IQL)) THEN
              TMPV = CLHIGHD(IHIGHD,IQL)
              CLHIGHD(IHIGHD,IQL) = CLHIGHDMAX
              CLHIGHD(IVMAX,IQL) = TMPV
          ENDIF

! Finished with array?

          IF (IHIGHD .LT. NHIGHD) GOTO 610

! Yes! Array sorted in descending order

           ILHIGHDMIN(IQL) = NHIGHD

  600 CONTINUE

  699 CONTINUE

! Deallocate temporary array for concentrations

      IF (ALLOCATED(CL)) DEALLOCATE(CL)

      RETURN

! End of subroutine CSTATL

      end subroutine cstatl
