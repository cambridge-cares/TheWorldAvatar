      SUBROUTINE CALC
C***********************************************************************
C             CALC Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Controls Flow and Processing of mixed Modules
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED: Split RLINE source into RLINEXT and RLINE.
C                  Wood, 03/04/2019
C
C        MODIFIED: Added code to process a RLINE source.
C                  Wood, 07/20/2018
C
C        MODIFIED: Added code to process a buoyant line source; several
C                  routines from the original BLP are incorporated into
C                  this file.
C                  Amec Foster Wheeler, 06/30/2015
C
C        MODIFIED: Added check for runtime error (RUNERR) before
C                  continuing with source loop.
C                  R. W. Brode, U.S. EPA, OAQPS, AQMG, 10/19/2009
C
C        MODIFIED: Moved METHDR assignment statement from SUB. PCALC
C                  to beginning of source loop.
C                  R. W. Brode, MACTEC (f/k/a PES), Inc., 12/28/05
C
C        INPUTS:  Arrays of Source Parameters
C                 Arrays of Receptor Locations
C                 Meteorological Variables for One Hour
C
C        OUTPUTS: Array of 1-hr CONC or DEPOS Values for Each
C                 Source/Receptor
C
C        CALLED FROM:   HRLOOP
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      USE BUOYANT_LINE, ONLY: NUMBLGRPS, BL_UREF
      USE RLINE_DATA, ONLY: RLPROCESSED
      IMPLICIT NONE
      CHARACTER MODNAM*12
      INTEGER  :: KK
      LOGICAL  :: BLPROCESSED

C     Variable Initializations
      MODNAM = 'CALC'
      PATH = 'CN'

!    Added for TTRM; AECOM
!    Initialize array for TTRM calculations
      IF (RUNTTRM) THEN
        TTRMOUT(:,:,:) = 0.0D0
      ENDIF
!    End TTRM insert; Feb. 2021

C --- Set the bouyant line source flag to false to indicate that no
C      lines have been processed for this hour since all lines are
C      processed together
      BLPROCESSED = .FALSE.

C --- Set the RLINE source flag to false to indicate that no
C      RLINES have been processed for this hour since rotation of
C      receptors only occurs for first source.
      RLPROCESSED = .FALSE.

C     Assing METHDR = .TRUE. to print source&receptor-independent
C     meteorology debug information to METEORDBG debug output file.
      METHDR = .TRUE.

C     Begin Source LOOP
      SOURCE_LOOP: DO ISRC = 1, NUMSRC
         IF (SRCTYP(ISRC)(1:5) .EQ. 'POINT') THEN
C           Calculate Point Source Values                   ---   CALL PCALC
            CALL PCALC

         ELSE IF (SRCTYP(ISRC) .EQ. 'VOLUME') THEN
C           Calculate Volume Source Values                  ---   CALL VCALC
            CALL VCALC

         ELSE IF (SRCTYP(ISRC)(1:4) .EQ. 'AREA') THEN
C           Calculate AREA/AREAPOLY/AREACIRC Source Values  ---   CALL ACALC
            CALL ACALC

         ELSE IF (SRCTYP(ISRC) .EQ. 'OPENPIT') THEN
C           Calculate OpenPit Source Values                 ---   CALL OCALC
            CALL OCALC

C ---    Added option to process LINE source, using AREA source calc routines
         ELSE IF (SRCTYP(ISRC) .EQ. 'LINE') THEN
C           Calculate LINE Source Values                    ---   CALL ACALC
            CALL ACALC

C ---    Added option to process RLINE source
         ELSE IF ((SRCTYP(ISRC) .EQ. 'RLINE') .OR.
     &           (SRCTYP(ISRC) .EQ. 'RLINEXT')) THEN
C           Calculate RLINE Source Values                   ---   CALL RLCALC
            CALL RLCALC
            RLPROCESSED = .TRUE.

         ELSE IF (SRCTYP(ISRC) .EQ. 'BUOYLINE' .AND.
     &                                        (.NOT. BLPROCESSED)) THEN

C Multiple_BuoyLines_D41_Wood
C           The original BLP used meteorology from the met processor CRSTER.
C           CRSTER did not allow wind speeds to be less than 1 m/s.  Set the
C           reference wind speed used in the calculations below to be a minimum
C           of 1.0 m/s.  Use the local variable BL_UREF.  Keep the reference
C           wind speed height as UREFHT.
            IF (UREF .GE. 1.0D0) THEN
              BL_UREF = UREF
            ELSE
              BL_UREF = 1.0D0
C             WRITE Message              ! Ref ht wind speed less than minimum
              WRITE(DUMMY,'(''on '',I8.8)') KURDAT
              CALL ERRHDL(PATH,MODNAM,'W','471',DUMMY)
            END IF

C ---       Calculate Bouyant Line Source Values            ---   CALL BL_CALC
C           BLPROCESSED lets AERMOD know that all lines associated with
C            the buoyant line sources were processed on the first pass
C            through the sources

C Multiple_BuoyLines_D41_Wood

            DO KK = 1,NUMBLGRPS
               CALL BL_CALC(KK)
            END DO
            BLPROCESSED = .TRUE.

C        Added for Sidewash
         ELSE IF (SRCTYP(ISRC) .EQ. 'SWPOINT') THEN
C           Calculate SIDEWASH Source Values                    ---   CALL SWCALC
            CALL SWCALC
         END IF

C        Check for runtime error (RUNERR) before continuing loop
         IF (RUNERR) EXIT

      END DO SOURCE_LOOP
C     End Source LOOP

      IF (L_BACKGRND .AND. .NOT.ARM2 .AND.
     &    .NOT.OLM .AND. .NOT.GRSM .AND.
     &    .NOT.RUNTTRM .AND. .NOT.PVMRM) THEN
C ---    User-specified background concentrations are included;
C        add to modeled concentrations by source group, unless
C        NO2 options are specified, which are handled separately
         DO IGRP = 1, NUMGRP
            CALL SUMBACK
         END DO
      END IF

      RETURN
      END

      SUBROUTINE SWCALC
C***********************************************************************
C        PURPOSE: Calculates concentration or deposition values
C                 for SIDEWASH Point sources
C
C        PROGRAMMER: Carlos Szembek (AECOM).
C
C        DATE:    December 23, 2021
C
C        INPUTS:  Source Parameters for Specific Source
C                 Arrays of Receptor Locations
C                 Meteorological Variables for One Hour
C
C        OUTPUTS: 1-hr CONC Values for Each Receptor for
C                 Particular Source ** ONLY WITHIN WAKE ZONE **
C
C        CALLED FROM:   CALC
C
C***********************************************************************
C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      INTEGER :: NDXZHS, NDXZBH
      DOUBLE PRECISION :: STKWD, STKWS, SWEPSILON, ! Now global: THETA_P & THETA_SW
     &                    CHISW,                   ! Now global: SX1, SY1, SZW, CONC_P and CONC_SW
     &                    WTSF,                    ! Added wind tunnel scaling factor
     &                    TMPTHETA,                ! Temporary variable for storing SWTHETA
     &                    CONC_FULL
! Added for Sidewash; temporary variables to save translated receptors
      DOUBLE PRECISION :: SWXT(NREC), SWYT(NREC)
! End sidewash insert
C     Variable Initializations
      MODNAM = 'SWCALC'

C     Set the Source Variables for This Source              ---   CALL SETSRC
      CALL SETSRC
! Initialize wind tunnel scaling factor and scaled building dimensions and stack height
      WTSF = 150.0D0
      BHS = BH/WTSF
      BWS = BW/WTSF
      BLS = BL/WTSF
      HSS = HS/WTSF
C     Apply Variable Emission Rate Factors                  ---   CALL EMFACT
!     CALL EMFACT(QS)                                    <----   Not yet implemented

C---- Locate index below (actual) stack height  ! updated April 2022
      CALL LOCATE(GRIDHT, 1, MXGLVL, HS, NDXZHS)
C----    Get Wind direction at stack height ! updated April 2022
      CALL GINTRP( GRIDHT(NDXZHS), GRIDWD(NDXZHS),
     &     GRIDHT(NDXZHS+1), GRIDWD(NDXZHS+1), HS, STKWD )
C---- Locate index below (actual) building height  ! updated April 19, 2022
      CALL LOCATE(GRIDHT, 1, MXGLVL, BH, NDXZBH)
C----    Get Wind speed at building height ! updated April 19, 2022
      CALL GINTRP( GRIDHT(NDXZBH), GRIDWS(NDXZBH),
     &     GRIDHT(NDXZBH+1), GRIDWS(NDXZBH+1), BH, STKWS )

      U_AMB = STKWS
C     Check for WDIR > 360 or < 0
      IF (STKWD .GT. 360.0D0) THEN
          STKWD = STKWD - 360.0D0
      ELSE IF (STKWD .LE. 0.0D0) THEN
              STKWD = STKWD + 360.0D0
      END IF

! Calculate wind angle on building face
      SWTHETA = BA - (90.-(360. - STKWD))

C     Check for if SWTHETA  > 360 or < 0
      IF (SWTHETA .GE. 359.9D0) THEN
          SWTHETA = SWTHETA - 360.0D0
      ELSE IF (SWTHETA .LT. 0.0D0) THEN
               SWTHETA = SWTHETA + 360.0D0
      END IF

      IF (SWDBG) THEN
C        *** Write out building dimensions, stack height ***
C        *** ambient wind speed and emission rate        ***
         WRITE(SWDBGUNT,*)
         WRITE(SWDBGUNT,*)('OUTPUT FOR GAUSSIAN DISPERSION EQUATION')
         WRITE(SWDBGUNT,*)('***************************************')
         WRITE(SWDBGUNT,*)('Definitions:')
         WRITE(SWDBGUNT,*)('SWCALC:     Final Sidewash Calc.')
         WRITE(SWDBGUNT,*)('CHISW:      Sidewash Chi Conc.')
         WRITE(SWDBGUNT,*)('***************************************')
C
C        *** Write out header for input data ***
         WRITE(SWDBGUNT,887)
 887     FORMAT(//,'Building ','Building ','Building ','Stack ',
     &             '    Ambient        Stack-top  Emission')

         WRITE(SWDBGUNT,897)
 897     FORMAT('Height m ','Length m ','Width m ',' Height m ',
     &          ' Wind Speed m/s Wind Dir.  g/s')

         WRITE(SWDBGUNT,852)
 852     FORMAT(72('-'))
C        *** Write out values of building dimensions          ***
C        *** stack height,ambient wind speed and emisson rate ***
         WRITE(SWDBGUNT,810)BH,BL,BW,HS,U_AMB,STKWD,QS
 810     FORMAT(1X,F4.1,6X,F4.1,2x,F6.1,5X,F6.3,3X,F5.2,10X,F5.1,6x,
     &          F8.5)
C
         WRITE(SWDBGUNT,*)
         WRITE(SWDBGUNT,850)
 850     FORMAT(72('*'))
         WRITE(SWDBGUNT,*)
      END IF

C --- Begin Receptor LOOP
      RECEPTOR_LOOP: DO IREC = 1, NUMREC

!        Added for Sidewash; Adjust and save copy of receptor grid in local coordinates
!        rotated such that the building angle (BA) is 0 degrees
!        TRANSLATION
!        Shift receptor based on X,Y of each sidewash source (AXS, AYS)
!        Note: the SWXR & SWYR arrays for non-sidewash sources are set but
!        not used by AERMOD.
         SWXT(IREC) = AXR(IREC) - AXS(ISRC)
         SWYT(IREC) = AYR(IREC) - AYS(ISRC)

!        ROTATION
!        Rotate the translated X,Y such that flow across the building length is rotated
!        aligned with the X-axis based on the input building angle (BA).
!        Wind direction for sidewash is simarly rotated in sub.SWCALC in calc1.f
         SWXR(IREC) = SWXT(IREC) * DCOS((270.0D0 - STKWD)*DTORAD) +
     &                SWYT(IREC) * DSIN((270.0D0 - STKWD)*DTORAD)
         SWYR(IREC) = SWYT(IREC) * DCOS((270.0D0 - STKWD)*DTORAD) -
     &                SWXT(IREC) * DSIN((270.0D0 - STKWD)*DTORAD)


         SX1 = SWXR(IREC)/BH         ! changed from AXR(IREC)

         IF(SX1 .EQ. 0.00D0) THEN
            SX1 = 1.0D-08
         ENDIF

         SY1 = SWYR(IREC)/WTSF       ! changed from AYR(IREC)
         SWZ = AZELEV(IREC)/WTSF     ! changed from AZELEV(IREC)

         IF (SWDBG) THEN

C           *** Write out header of distance and modifiers of ***
            WRITE(SWDBGUNT,*)
            WRITE(SWDBGUNT,855) IREC
 855        FORMAT('Receptor No: ',I5)
            WRITE(SWDBGUNT,812)
 812        FORMAT('Orig. X (m) Orig. Y (m) Adj. X1 (m) Adj. Y1 (m)',
     &             '     Z1 (m)      W/H         X1/H    Bld. Angle',
     &             '  Theta')
            WRITE(SWDBGUNT,851)
 851        FORMAT(102('-'))
            WRITE(SWDBGUNT,816) AXR(IREC),AYR(IREC),SX1,SY1,SWZ,BW/BH,
     &                          SX1/BH,BA,SWTHETA
 816        FORMAT(7(F10.4,2x),F5.1,7x,F5.1)
            WRITE(SWDBGUNT,*)
         END IF

!        Initialize SWCONC and CHISW
         SWCONC = 0.0D0
         CHISW  = 0.0D0

!        Wind angle cases:
!        April, 2022; Added symmetric conditions between 300 - 360 degrees
         IF ((SWTHETA .EQ. 0.0D0) .OR.
     &       (SWTHETA .EQ. 360.0D0)) THEN

C            CALL PERPEN(BHS, BLS, BWS, HSS, SX1, SY1, SWZ, SWTHETA,
C     &                  U_AMB, QS, SWCONC)
            CALL PERPEN

         ELSEIF ((SWTHETA .GE. 15.0D0) .AND.
     &           (SWTHETA .LE. 60.0D0)) THEN

C            CALL OBLIQUE(BHS, BLS, BWS, HSS, SX1, SY1, SWZ, SWTHETA,
C     &                   U_AMB, QS, SWCONC)
            CALL OBLIQUE

         ELSEIF ((SWTHETA .GT. 0.0D0) .AND.
     &           (SWTHETA .LT. 15.0D0)) THEN

!           Initialize variables
            TMPTHETA = SWTHETA ! Store calculated SWTHETA in temp variable
            CONC_P   = 0.0D0
            CONC_SW  = 0.0D0
            THETA_P  = 0.0D0
            THETA_SW = 15.0D0
            SWEPSILON = 1.0D0-((TMPTHETA-THETA_P)/(THETA_SW-THETA_P))
            SWTHETA = THETA_P

C            CALL PERPEN(BHS, BLS, BWS, HSS, SX1, SY1, SWZ, SWTHETA,
C     &                         U_AMB, QS, SWCONC)
            CALL PERPEN

            CONC_P = SWCONC
            SWCONC = 0.0D0
            SWTHETA = THETA_SW

C            CALL OBLIQUE(BHS, BLS, BWS, HSS, SX1, SY1, SWZ, SWTHETA,
C     &                   U_AMB, QS, SWCONC)
            CALL OBLIQUE

            CONC_SW = SWCONC
            SWTHETA = TMPTHETA ! Restore calculated SWTHETA value
            SWCONC = CONC_P*SWEPSILON + CONC_SW*(1-SWEPSILON)

         ELSEIF ((SWTHETA .GE. 300.0D0) .AND.
     &           (SWTHETA .LE. 345.0D0)) THEN

            SY1 = -1.0D0*SY1
            TMPTHETA = SWTHETA ! Store calculated SWTHETA in temp variable
            SWTHETA = 360.0D0 - SWTHETA

C            CALL OBLIQUE(BHS, BLS, BWS, HSS, SX1, SY1, SWZ, SWTHETA,
C     &                   U_AMB, QS, SWCONC)
            CALL OBLIQUE

            SWTHETA = TMPTHETA ! Restore calculated SWTHETA value

         ELSEIF ((SWTHETA .GT. 345.0D0) .AND.
     &           (SWTHETA .LT. 360.0D0)) THEN

!           Initialize variables
            SY1 = -1.0D0*SY1
            TMPTHETA = SWTHETA ! Store calculated SWTHETA in temp variable
            CONC_P   = 0.0D0
            CONC_SW  = 0.0D0
            THETA_P  = 360.0D0
            THETA_SW = 345.0D0
            SWEPSILON = 1.0D0-((TMPTHETA-THETA_P)/(THETA_SW-THETA_P))
            SWTHETA = THETA_P

C            CALL PERPEN(BHS, BLS, BWS, HSS, SX1, SY1, SWZ, SWTHETA,
C     &                  U_AMB, QS, SWCONC)
            CALL PERPEN

            CONC_P = SWCONC
            SWCONC = 0.0D0
            SWTHETA = 360.0D0 - THETA_SW ! Should equal 15

C            CALL OBLIQUE(BHS, BLS, BWS, HSS, SX1, SY1, SWZ, SWTHETA,
C     &                   U_AMB, QS, SWCONC)
            CALL OBLIQUE

            CONC_SW = SWCONC
            SWTHETA = TMPTHETA ! Restore calculated SWTHETA value
            SWCONC = CONC_P*SWEPSILON + CONC_SW*(1.0D0-SWEPSILON)

!        Added April 2022
         ELSE ! For SWTHETA greater than 60 degrees
            SWCONC = 0.0D0
         ENDIF


!        Calculate CHI for sidewash (April 2022)
         CHISW = SWCONC*U_AMB*BHS**2.0/QS             ! Normailized (unitless) Adjusted for wind tunnel scaling
         HRVAL = 1.0D06*CHISW*QS/(U_AMB*BH**2.0)      ! Adjusted for wind tunnel scaling; scale from g/m3 to ug/3

         IF (SWDBG) THEN

            WRITE(SWDBGUNT,*)
            WRITE(SWDBGUNT,833)
 833        FORMAT(32X,'HRVAL ug/m3',2X,'SWCONC g/m3',2X,'CHI')

C           *** Write out total concentration ***
            WRITE(SWDBGUNT,834) HRVAL, SWCONC, CHISW
 834        FORMAT('SIDEWASH CONCENTRATIONS = ', 3(F12.4,1X))

            WRITE(SWDBGUNT,*)
            WRITE(SWDBGUNT,*)
            WRITE(SWDBGUNT,*)

         END IF

C        CRT 4/20/2022 D113 Sidewash
c        If concentration is negative, set to zero and write warning message
         IF (SWCONC .LT. 0.0D0) THEN
            WRITE(DUMMY,'(I12)') KURDAT
            CALL ERRHDL(PATH, MODNAM, 'W','650',DUMMY)
            HRVAL = 0.0D0
         END IF

!        Call AERMOD averaging
         DO IGRP = 1, NUMGRP
            CALL SUMVAL
         END DO

C        Initialize __VAL arrays (1:NUMTYP)
         HRVAL   = 0.0D0

      END DO RECEPTOR_LOOP

      RETURN
      END

      SUBROUTINE PERPEN!(BH,BL,BW,HS,SX1,SY1,SWZ,SWTHETA,U_AMB,QS,SWCONC)
C***********************************************************************
C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      DOUBLE PRECISION :: BETA_B_U_C1, BETA_B_U_C2, BETA_B_SIG_Y1_C1,
     &                    BETA_B_SIG_Z1, BETA_B_MU_Z1_C1, U, SIG_Y1,
     &                    SIG_Z1, SIG_Y2, SIG_Z2, RHO, TERM1, TERM2,
     &                    TERM2A, TERM2B, TERM2C, TERM2D, TERM3,
     &                    TERM3B, TERM3C, TERM3D, TERM4, TERM5A, TERM5B,
     &                    TERM5C, TERM51, TERM52, TERM5 !, SX1, SY1, SWZ  <-- global April 2022

C     Variable Initializations
      MODNAM = 'PERPEN'

      IF (SX1 .LT. 0.0) THEN
         SWCONC = 0.0
         GOTO 899
      ENDIF

      BETA_B_U_C1      = (-0.0555D0*(BWS/BHS))
      BETA_B_U_C2      = 0.975993D0
      BETA_B_SIG_Y1_C1 =  0.0141D0 + 0.0006D0*(BWS/BHS)
      BETA_B_SIG_Z1    = 1.5D0* (0.0172D0 +(0.0003D0*(BWS/BHS)))
      BETA_B_MU_Z1_C1  = -0.0217D0 + (0.0016D0*(BWS/BHS))

      U      = BETA_B_U_C1 + BETA_B_U_C2*U_AMB
      LAMBDA = 1.0D0
      SIG_Y1 = BETA_B_SIG_Y1_C1*SX1
      SIG_Z1 = BETA_B_SIG_Z1*SX1
      SIG_Y2 = 1.0D0
      SIG_Z2 = 1.0D0
      MU_Y1  = 0.0D0
      MU_Z1  = HSS + BETA_B_MU_Z1_C1*SX1
      MU_Y2  = 0.0D0
      MU_Z2  = 0.0D0
      RHO    = 0.0D0

      IF (SWDBG) THEN

C        *** CHECK SECOND SET OF VALUES ***
         WRITE(SWDBGUNT,*) ('PERPENDICULAR:')

C        *** WRITE OUT LAMBDA RHO AND U_AMB ***
         WRITE(SWDBGUNT,818)LAMBDA,RHO,U
 818     FORMAT('LAMBDA = ',F7.5,3x,'RHO = ',F7.5,3x,'U =',F5.3)

C        ************ WRITE OUT SIG AND MU VALUES *************
C        *** Write out header for sigma values ***
         WRITE(SWDBGUNT,*)('SIG Y1     SIG Z1    SIG Y2    SIG Z2')

C        *** Write our values of sigma values ***
         WRITE(SWDBGUNT,820)SIG_Y1,SIG_Z1,SIG_Y2,SIG_Z2
 820     FORMAT(4(F8.6,2x))

C        *** Write out header for MU values
         WRITE(SWDBGUNT,*)('MU Y1      MU Z1     MU Y2      MU Z2')

C        *** Write out values for MU values
         WRITE(SWDBGUNT,822)MU_Y1,MU_Z1,MU_Y2,MU_Z2
 822     FORMAT(4(F8.6,2X))

      END IF

      TERM1 =(LAMBDA*QS)/((2*PI*U*SIG_Y1*SIG_Z1)*(SQRT(1.0D0-(RHO**2))))
C
C     *** Second Term, parts A,B,C,D and full TERM2  ***
      TERM2A = (-1.0D0)/(2.0D0*(1.0D0-(RHO**2)))
      TERM2B = ((SY1-MU_Y1)**2)/(SIG_Y1**2)
      TERM2C = ((SWZ-MU_Z1)**2)/(SIG_Z1**2)
      TERM2D = (2.0D0*RHO*(SY1-MU_Y1)*(SWZ-MU_Z1))/(SIG_Y1*SIG_Z1)
      TERM2 = DEXP(TERM2A*(TERM2B + TERM2C - TERM2D))

      IF (SWDBG) THEN

         WRITE(SWDBGUNT,*)

C        *** Write out TERM1 and parts A,B,C,D and full of TERM2 ***
         WRITE(SWDBGUNT,824) TERM1
 824     FORMAT( 'TERM1 = ',F14.6)

         WRITE(SWDBGUNT,*)
         WRITE(SWDBGUNT,*)'TERM2A    TERM2B     TERM2C     TERM2D',
     &                     '     TM2'
         WRITE(SWDBGUNT,826)TERM2A,TERM2B,TERM2C,TERM2D,TERM2
 826     FORMAT(2(F9.6,1X),F9.3,2X,2(F9.4,1X))

      END IF
C
C     *** Third Term ***
      TERM2A = (-1.0D0/(2.0D0*(1-RHO**2)))
      TERM3B = ((SY1-MU_Y1)**2)/(SIG_Y1**2)
      TERM3C = ((SWZ+MU_Z1)**2)/(SIG_Z1**2)
      TERM3D = (2.0D0*RHO*(SY1-MU_Y1)*(SWZ+MU_Z1))/(SIG_Y1*SIG_Z1)
      TERM3 = DEXP(TERM2A*(TERM3B + TERM3C - TERM3D))

      IF (SWDBG) THEN

         WRITE(SWDBGUNT,*)

C        *** Write out TERM2A and parts B,C,D and full TERM3 ***
         WRITE(SWDBGUNT,*)'TERM2A    TERM3B     TERM3C     TERM3D',
     &                     '     TM3'
         WRITE(SWDBGUNT,828)TERM2A,TERM3B,TERM3C,TERM3D,TERM3
 828     FORMAT(2(F9.6,1X),F9.3,2X,2(F9.4,1X))

      END IF
C
C     *** Fourth Term ***
      TERM4 = ((1.0D0 - LAMBDA)*QS)/((2*PI*U*SIG_Y2*SIG_Z2))

      IF (SWDBG) THEN

         WRITE(SWDBGUNT,*)

C        *** Write out TERM4 ***
         WRITE(SWDBGUNT,830)TERM4
 830     FORMAT( 'TERM4 = ',F14.6)

      END IF
C
C     *** Fifth Term ***
      TERM5A = ((SY1 - MU_Y2)**2)/(SIG_Y2**2)
      TERM5B = ((SWZ - MU_Z2)**2)/(SIG_Z2**2)
      TERM5C = ((SWZ + MU_Z2)**2)/(SIG_Z2**2)
      TERM51 = DEXP((-0.5D0*(TERM5A + TERM5B)))
      TERM52 = DEXP((-0.5D0*(TERM5A + TERM5C)))
      TERM5 = TERM51 + TERM52

      IF (SWDBG) THEN

         WRITE(SWDBGUNT,*)

C        *** Write out parts A,B,C,1,2 and full TERM5 ***
         WRITE(SWDBGUNT,831)
 831     FORMAT('TERM5A',7X,'TM5B',8X,'TM5C',5X,'T51',7X,'T52',7X,'T5')

         WRITE(SWDBGUNT,832) TERM5A,TERM5B,TERM5C,TERM51,TERM52,TERM5
 832     FORMAT(F9.6,1X,2(F10.4,2X),3(F9.6,1X))

      END IF

C     ************* Calculate the total concentration ***********
C
      SWCONC = (TERM1*(TERM2+TERM3)) + (TERM4*(TERM5))
 899  CONTINUE
      RETURN
      END

      subroutine oblique!(BH,BL,BW,HS,SX1,SY1,SWZ,SWTHETA,U_AMB,QS,SWCONC)
C***********************************************************************
C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      DOUBLE PRECISION :: BETA_B_U_C1, BETA_B_U_C2, BETA_B_LAMBDA,
     &                    BETA_B_SIG_Y1_C1, BETA_B_SIG_Y1_C2,
     &                    BETA_B_SIG_Z1, BETA_B_SIG_Y2,
     &                    BETA_B_SIG_Z2_C1, BETA_B_SIG_Z2_C2,
     &                    BETA_B_MU_Y1_C1, BETA_B_MU_Y1_C2,
     &                    BETA_B_MU_Z1_C1, BETA_B_MU_Z1_C2,
     &                    BETA_B_MU_Y2, BETA_B_MU_Z2,
     &                    BETA_B_RHO_C1, BETA_B_RHO_C2,
     &                    U, SIG_Y1,
     &                    SIG_Z1, SIG_Y2, SIG_Z2, RHO, TERM1, TERM2,
     &                    TERM2A, TERM2B, TERM2C, TERM2D, TERM3,
     &                    TERM3B, TERM3C, TERM3D, TERM4, TERM5A, TERM5B,
     &                    TERM5C, TERM51, TERM52, TERM5!, SX1, SY1, SWZ  <-- global, April 2022

C     Variable Initializations
      MODNAM = 'OBLIQUE'

      IF (SX1 .LT. 0.0D0) THEN
         SWCONC = 0.0D0
         GOTO 898
      ENDIF

      BETA_B_U_C1 = (-0.0555D0*(BWS/BHS))
      BETA_B_U_C2 = 0.975993D0
      BETA_B_LAMBDA = -0.416614D0 + (0.003644D0*SWTHETA) +
     &                            (-0.012852D0*(BWS/BHS))
      BETA_B_SIG_Y1_C1 = 0.026233D0
      BETA_B_SIG_Y1_C2 = -0.00117D0
      BETA_B_SIG_Z1 = 1.83D-02 + (-3.83D-05*SWTHETA) +
     &                           (-5.81D-04*(BWS/BHS))
      BETA_B_SIG_Y2 = 2.67D-02 + (-8.50D-05*SWTHETA) +
     &                           (4.93D-04*(BWS/BHS))
      BETA_B_SIG_Z2_C1 = 0.0385D0 + (-0.000171D0*SWTHETA) +
     &                            (-0.0014D0*(BWS/BHS))
      BETA_B_SIG_Z2_C2 = -0.00191D0 + (0.0000108D0*SWTHETA) +
     &                              (0.000118D0*(BWS/BHS))
      BETA_B_MU_Y1_C1 = 0.015081D0 + (0.000243D0*SWTHETA) +
     &                             (-0.0031D0*(BWS/BHS))
      BETA_B_MU_Y1_C2 = -0.00175D0 + (-0.0000178D0*SWTHETA) +
     &                             (0.000331D0*(BWS/BHS))
      BETA_B_MU_Z1_C1 = -0.03578D0 + (-0.00029D0*SWTHETA) +
     &                             (0.00224D0*(BWS/BHS))
      BETA_B_MU_Z1_C2 = 0.00186D0 + (0.00000833D0*SWTHETA) +
     &                            (-0.000116D0*(BWS/BHS))
      BETA_B_MU_Y2 = 2.21D-02 + (-1.82D-04*SWTHETA) +
     &                          (9.71D-04*(BWS/BHS))
      BETA_B_MU_Z2 = -0.357069D0 + (0.002894D0*SWTHETA) +
     &                           (0.002252D0*(BWS/BHS))
      BETA_B_RHO_C1 = 0.012006D0
      BETA_B_RHO_C2 = -0.00039D0
C
C     *** Start defining terms in Gaussian Dispersion Equation ***
C     *** according to Beta_a(x)
      U = BETA_B_U_C1 + BETA_B_U_C2*U_AMB
      LAMBDA = DEXP(BETA_B_LAMBDA*SX1)
      SIG_Y1 = BETA_B_SIG_Y1_C1*SX1 + BETA_B_SIG_Y1_C2*(SX1**2)
      SIG_Z1 = BETA_B_SIG_Z1*SX1
      SIG_Y2 = BETA_B_SIG_Y2*SX1
      SIG_Z2 = BETA_B_SIG_Z2_C1*SX1 + BETA_B_SIG_Z2_C2*(SX1**2)
      MU_Y1 = BETA_B_MU_Y1_C1*SX1 + BETA_B_MU_Y1_C2*(SX1**2)
      MU_Z1 = HSS + BETA_B_MU_Z1_C1*SX1 + BETA_B_MU_Z1_C2*(SX1**2)
      MU_Y2 = BETA_B_MU_Y2*SX1
      MU_Z2 = BHS*DEXP(BETA_B_MU_Z2*SX1)
      RHO = BETA_B_RHO_C1*SX1 + BETA_B_RHO_C2*(SX1**2)

      IF (SWDBG) THEN

C        *** CHECK SECOND SET OF VALUES ***
         WRITE(SWDBGUNT,*) ('OBLIQUE:')

C        *** WRITE OUT LAMBDA RHO AND U_AMB ***
         WRITE(SWDBGUNT,818)LAMBDA,RHO,U
 818     FORMAT('LAMBDA = ',F7.5,3x,'RHO = ',F7.5,3x,'U =',F5.3)

C        ************ WRITE OUT SIG AND MU VALUES *************
C        *** Write out header for sigma values ***
         WRITE(SWDBGUNT,*) 'SIG Y1     SIG Z1    SIG Y2    SIG Z2'

C        *** Write our values of sigma values ***
         WRITE(SWDBGUNT,820)SIG_Y1,SIG_Z1,SIG_Y2,SIG_Z2
 820     FORMAT(4(F8.6,2x))

C        *** Write out header for MU values
         WRITE(SWDBGUNT,*) 'MU Y1      MU Z1     MU Y2      MU Z2'

C        *** Write out values for MU values
         WRITE(SWDBGUNT,822)MU_Y1,MU_Z1,MU_Y2,MU_Z2
 822     FORMAT(4(F8.6,2X))

      END IF

      TERM1 =(LAMBDA*QS)/((2*PI*U*SIG_Y1*SIG_Z1)*(SQRT(1-(RHO**2))))
C
C     *** Second Term, parts A,B,C,D and full TERM2  ***
      TERM2A = (-1.0D0)/(2.0D0*(1-(RHO**2)))
      TERM2B = ((SY1-MU_Y1)**2)/(SIG_Y1**2)
      TERM2C = ((SWZ-MU_Z1)**2)/(SIG_Z1**2)
      TERM2D = (2.0D0*RHO*(SY1-MU_Y1)*(SWZ-MU_Z1))/(SIG_Y1*SIG_Z1)
      TERM2 = DEXP(TERM2A*(TERM2B + TERM2C - TERM2D))

      IF (SWDBG) THEN

         WRITE(SWDBGUNT,*)

C        *** Write out TERM1 and parts A,B,C,D and full of TERM2 ***
         WRITE(SWDBGUNT,824)TERM1
 824     FORMAT( 'TERM1 = ',F14.6)

         WRITE(SWDBGUNT,*)
         WRITE(SWDBGUNT,*) 'TERM2A    TERM2B     TERM2C     TERM2D',
     &                     '     TM2'
         WRITE(SWDBGUNT,826)TERM2A,TERM2B,TERM2C,TERM2D,TERM2
 826     FORMAT(2(F9.6,1X),F9.3,2X,2(F9.4,1X))

      END IF

C     *** Third Term ***
      TERM2A = (-1.0D0/(2.0D0*(1-RHO**2)))
      TERM3B = ((SY1-MU_Y1)**2)/(SIG_Y1**2)
      TERM3C = ((SWZ+MU_Z1)**2)/(SIG_Z1**2)
      TERM3D = (2.0D0*RHO*(SY1-MU_Y1)*(SWZ+MU_Z1))/(SIG_Y1*SIG_Z1)
      TERM3 = DEXP(TERM2A*(TERM3B + TERM3C - TERM3D))

      IF (SWDBG) THEN

         WRITE(SWDBGUNT,*)

C        *** Write out TERM2A and parts B,C,D and full TERM3 ***
         WRITE(SWDBGUNT,*) 'TERM2A    TERM3B     TERM3C     TERM3D',
     &                     '     TM3'
         WRITE(SWDBGUNT,828)TERM2A,TERM3B,TERM3C,TERM3D,TERM3
 828     FORMAT(2(F9.6,1X),F9.3,2X,2(F9.4,1X))

      END IF
C
C     *** Fourth Term ***
      TERM4 = ((1.0 - LAMBDA)*QS)/((2*PI*U*SIG_Y2*SIG_Z2))

      IF (SWDBG) THEN

         WRITE(SWDBGUNT,*)

C        *** Write out TERM4 ***
         WRITE(SWDBGUNT,830)TERM4
 830    FORMAT( 'TERM4 = ',F14.6)

      END IF

C     *** Fifth Term ***
      TERM5A = ((SY1 - MU_Y2)**2)/(SIG_Y2**2)
      TERM5B = ((SWZ - MU_Z2)**2)/(SIG_Z2**2)
      TERM5C = ((SWZ + MU_Z2)**2)/(SIG_Z2**2)
      TERM51 = DEXP((-0.5D0*(TERM5A + TERM5B)))
      TERM52 = DEXP((-0.5D0*(TERM5A + TERM5C)))
      TERM5 = TERM51 + TERM52

      IF (SWDBG) THEN

         WRITE(SWDBGUNT,*)

C        *** Write out parts A,B,C,1,2 and full TERM5 ***
         WRITE(SWDBGUNT,831)
 831     FORMAT('TERM5A',7X,'TM5B',8X,'TM5C',5X,'T51',7X,'T52',7X,'T5')

         WRITE(SWDBGUNT,832) TERM5A,TERM5B,TERM5C,TERM51,TERM52,TERM5
 832     FORMAT(F9.6,1X,2(F10.4,2X),3(F9.6,1X))

      END IF

C     ************* Calculate the total concentration ***********
C
      SWCONC = (TERM1*(TERM2+TERM3)) + (TERM4*(TERM5))
 898  CONTINUE
      RETURN
      END


      SUBROUTINE PCALC
C***********************************************************************
C             PCALC Module of the AMS/EPA Regulatory Model - AERMOD
c ----------------------------------------------------------------------
c ---    ISC-PRIME     Version 1.0    Level 970812              Modified
c ---        D. Strimaitis, J. Scire
c ---        Earth Tech, Inc.
c            Prepared for EPRI under contract WO3527-01
c ----------------------------------------------------------------------
C
C        PURPOSE: Calculates concentration or deposition values
C                 for POINT sources
C
C        PROGRAMMER: Roger Brode and Jim Paumier, PES, Inc.
C
C        DATE:    September 30, 1993
C
C        CHANGES:
C                  Added computation of AC & BC for PLAT_CUBIC function,
C                  called from PLAT_GRADPLUMADJ for PLATFORM sources.
C                  Michelle G. Snyder, WOOD, 08/05/2021.
C
C                  Modified to calculate conc*travel time for GRSM NO2 option
C                  CERC, 11/30/20
C
C                  Added arrays to save WDSIN and WDCOS by source for use
C                  with PVMRM option.  Corrects potential problem for
C                  PVMRM applications with multi-level wind inputs.
C                  R.W. Brode, U.S. EPA/OAQPS/AQMG, 01/26/2007
C
C                  Added call to subroutine HEFF for calculation
C                  of ZSUBP for deposition applications.
C                  R. W. Brode, U.S. EPA, OAQPS, AQMG, 01/24/2007
C
C                  Moved METHDR assignment statement from SUB. PCALC
C                  to beginning of source loop in SUB. CALC.
C                  R. W. Brode, MACTEC (f/k/a PES), Inc., 12/28/05
C
C                  Modified for capped stack option (POINTCAP) and
C                  for multiple urban area option.
C                  R. W. Brode, MACTEC (f/k/a PES), Inc., 09/30/05
C
C                  Removed code that adjusts DHCRIT based on distance
C                  to well-mixed layer for stable conditions for
C                  consistency with Model Formulation Document.
C                  R. W. Brode, MACTEC (f/k/a PES), Inc., 09/30/05
C
C                  Modified to include initialization of __VAL arrays
C                  at end of receptor loop.
C                  R. W. Brode, MACTEC (f/k/a PES), Inc., 10/26/04
C
C                  Modified to include the PVMRM and OLM options for
C                  modeling conversion of NOx to NO2.
C                  Added debug statement based on ENSR code.
C                  R. W. Brode, MACTEC (f/k/a PES), Inc., 07/27/04
C
CRWB               Modified to call DHPSS to obtain plume centroid height
CRWB               (CENTER) for Schulman-Scire downwash cases.  Modified
CRWB               to compare XFINAL to XMIXED only for unstable cases with
CRWB               HS < ZI.  Added initialization of TGEFF and TGEFF3 as TGS.
CRWB               Additional modifications made to improve consistency with
CRWB               the implementation of Schulman-Scire downwash algorithm
CRWB               in the ISCST3 model.
CRWB               R. Brode, PES - 12/6/99
C
CRWB               Modified to use wind direction at midpoint between
CRWB               stack height and "final" plume height for transport.
CRWB               R. Brode, PES - 1/22/98
C
CRWB               Use effective parameters evaluated at stack height
CRWB               for the indirect plume, as for direct plume.  Also
CRWB               commented out calls to LOCATE and GINTRP with ZIO2.
CRWB               This change is made for the Base Case model.
CRWB               R. Brode, PES - 12/8/94
C
CRWB               Commented out calls to LOCATE and GINTRP with HTEFF in
CRWB               order to use effective parameters evaluated at stack height
CRWB               instead of HTEFF for the direct plume and the stable
CRWB               plume.  This change is made for the Base Case model.
CRWB               R. Brode, PES - 12/7/94
C
C                  Moved calculation of penetration factor from outside
C                  to inside receptor loop, and deleted code related
C                  to indirect and penetrated plumes which is no
C                  longer needed.  (R.F. Lee, 7/13/94)
C
C                  Added true centerline concentration calculations
C                  for EVALFL output.  (R.F. Lee, 7/25/94)
C
CRJP               Changes made to calculations of effective parameters
CRJP               in conjunction with new treatment of inhomogeneity.
CRJP               (Bob Paine, 10/4/94)
C
C
C        INPUTS:  Source Parameters for Specific Source
C                 Arrays of Receptor Locations
C                 Meteorological Variables for One Hour
C
C        OUTPUTS: 1-hr CONC or DEPOS Values for Each Receptor for
C                 Particular Source
C
C        CALLED FROM:   CALC
C
C        Assumptions:
C
C        References:  "A Dispersion Model for the Convective Boundary
C                      Layer", J. Weil, 8/17/93
C                     "Inhomogeneous Boundary Layer", A. Venkatram,
C                      6/25/93
C
C***********************************************************************
C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      INTEGER :: I, KITER, NDXZMID, NDXZPL
      INTEGER :: NDXBH
      DOUBLE PRECISION :: HSPRIM, ZPLM, DHFOLD, SVPM, UPM, TGPM,
     &                    PTPM, PTP, ZMID
      DOUBLE PRECISION :: VALABV, VBELOW
      DOUBLE PRECISION :: USTACK, UBLDG, XBREC, YBREC
      DOUBLE PRECISION :: AERPLM(NUMTYP), AERPAN(NUMTYP), FRAN, FRAN3
      DOUBLE PRECISION :: VSEQ
!   Added for TTRM; AECOM
!      DOUBLE PRECISION :: DEFF, DEFF3, DEFFD
      DOUBLE PRECISION :: CHIN, CHID
!   End TTRM insert; Feb. 2021
      LOGICAL :: L_PLUME

      logical :: ldbhr

CMGS  Michelle G. Snyder 8/5/2021 D063 Variables needed for platform downwash
      DOUBLE PRECISION :: DUM, PLATSZ0, RZ0


C     Variable Initializations
      MODNAM = 'PCALC'

C     Initialize __VAL arrays (1:NUMTYP)
      HRVAL(:)   = 0.0D0
      AERVAL(:)  = 0.0D0
      AERPLM(:)  = 0.0D0
      AERPAN(:)  = 0.0D0
      PRMVAL(:)  = 0.0D0
      IF( ALLOCATED(CHI) ) CHI(:,ISRC,:) = 0.0D0
      IF(GRSM)THEN
        CHI_TTRAVPLM = 0.0D0
        CHI_TTRAVPAN = 0.0D0
        CHI_TTRAVAER = 0.0D0
        CHI_TTRAVPRM = 0.0D0
        CHI_TTRAVCHM(:,ISRC) = 0.0D0
      END IF

C --- Initialize FRAN and FRAN3
      FRAN  = 0.0D0
      FRAN3 = 0.0D0

      IF (AWMADWDBG) THEN
         WRITE(AWMADWDBUNT,*) '========================================'
         WRITE(AWMADWDBUNT,*) ' PCALC: YYMMDDHH ',KURDAT,
     &                        '  Source  ',srcid(isrc)
      END IF

C     Set the Source Variables for This Source              ---   CALL SETSRC
      CALL SETSRC

C     Apply Variable Emission Rate Factors                  ---   CALL EMFACT
      CALL EMFACT(QS)

      IF (QTK .NE. 0.0D0) THEN

C        Set Mixing Height and Profiles for Urban Option if Needed
         IF (URBSRC(ISRC) .EQ. 'Y') THEN
C           Find Urban Area Index for This Source
            DO I = 1, NUMURB
               IF (IURBGRP(ISRC,I) .EQ. 1) THEN
                  IURB = I
                  EXIT
               END IF
            END DO
            IF (STABLE .OR. L_MorningTrans(IURB)) THEN
               URBSTAB = .TRUE.
               ZI = MAX( ZIURB(IURB), ZIMECH )
               GRIDSV = GRDSVU(1:MXGLVL,IURB)
               GRIDSW = GRDSWU(1:MXGLVL,IURB)
               GRIDTG = GRDTGU(1:MXGLVL,IURB)
               GRIDPT = GRDPTU(1:MXGLVL,IURB)
               OBULEN = URBOBULEN(IURB)
               USTAR  = URBUSTR(IURB)
            ELSE
               URBSTAB = .FALSE.
               ZI = ZIRUR
               GRIDSV = GRDSVR
               GRIDSW = GRDSWR
               GRIDTG = GRDTGR
               GRIDPT = GRDPTR
               OBULEN = RUROBULEN
               USTAR  = RURUSTR
            END IF
         ELSE IF (URBAN .AND. URBSRC(ISRC) .EQ. 'N') THEN
            URBSTAB = .FALSE.
            ZI = ZIRUR
            GRIDSV = GRDSVR
            GRIDSW = GRDSWR
            GRIDTG = GRDTGR
            GRIDPT = GRDPTR
            OBULEN = RUROBULEN
            USTAR  = RURUSTR
         ELSE
C ---       Rural
            URBSTAB = .FALSE.
         END IF

C        Calculate the initial meteorological variables     ---   CALL METINI
         CALL METINI

C        Calculate Buoyancy and Momentum Fluxes             ---   CALL FLUXES
         CALL FLUXES(VSEQ)

C        Set Wake and Building Type Switches                ---   CALL WAKFLG
C ---    NOTE:  WAKFLG sets building dimensions based on wind
C        direction at stack top.

C ---    CRT, 1/20/2012: D063 Set dependency for call to WAKFLG
C        If source is subject to platform downwash, set
C        WAKE to false.  Only call WAKFLG if source is not
C        subject to platform downwash.
         IF (OSPLAT(ISRC)) THEN
            WAKE = .FALSE.
         ELSE
            CALL WAKFLG
         END IF

C        Define temporary values of CENTER and SURFAC based on HS
         CENTER = HS
         IF( CENTER .LT. 0.1D0*ZI )THEN
            SURFAC = .TRUE.
         ELSE
            SURFAC = .FALSE.
         END IF

C ---    Check for stack-tip downwash option and adjust if necessary, but
C        first check for POINTCAP option to avoid NOSTD overriding POINTCAP
         IF (SRCTYP(ISRC) .EQ. 'POINTCAP') THEN
C           Apply stack-tip downwash for capped stacks with VS = 0.001m/s
            HSP = HSPRIM ( US, VSEQ, HS, DS )
         ELSE IF (SRCTYP(ISRC) .EQ. 'POINTHOR') THEN
C           Do not apply stack-tip downwash for horizontal releases
            HSP = HS
         ELSE IF( NOSTD )THEN
C           No stack-tip downwash, no adjustments necessary
            HSP = HS
         ELSE
C           Make adjustments for stack-tip downwash
            HSP = HSPRIM ( US, VS, HS, DS )
         END IF

C        Calculate Distance to Final Rise                   ---   CALL DISTF
         CALL DISTF

C        Calculate the plume penetration factor             ---   CALL PENFCT
         CALL PENFCT


         IF(DEBUG) THEN
            WRITE(DBGUNT,6000) DHFAER, UP, TGS
6000        FORMAT(/,5X,'INITIAL PLUME RISE ESTIMATE:  DELH = ',
     &          F6.1,' M; Uplume = ',F5.2,' M/S; DTHDZ = ',
     &          F7.4,' DEG K/M')
         END IF

         IF (STABLE .OR. (UNSTAB.AND.(HS.GE.ZI))) THEN
C           Use iterative approach to stable plume rise calculations
            KITER = 0
50          ZPLM = HSP + 0.5D0 * DHFAER
            DHFOLD = DHFAER

C----       Locate index below ZPLM

            CALL LOCATE(GRIDHT, 1, MXGLVL, ZPLM, NDXZPL)

C----       Get Wind speed at ZPLM; replace UP.  Also, replace TGP,
C           vertical potential temperature gradient, if stable.

            CALL GINTRP( GRIDHT(NDXZPL), GRIDSV(NDXZPL),
     &           GRIDHT(NDXZPL+1), GRIDSV(NDXZPL+1), ZPLM, SVPM )
            CALL GINTRP( GRIDHT(NDXZPL), GRIDWS(NDXZPL),
     &           GRIDHT(NDXZPL+1), GRIDWS(NDXZPL+1), ZPLM, UPM )
            SVPM = MAX( SVPM, SVMIN, SVUMIN*UPM )
            IF( L_VECTORWS )THEN
               UPM = DSQRT( UPM*UPM + 2.0D0*SVPM*SVPM )
            ENDIF

            UPM  = MAX( UPM, WSMIN )


CRWB        Use average of stack top and midpoint wind speeds.
            UP = 0.5D0 * (US + UPM)

            CALL GINTRP( GRIDHT(NDXZPL), GRIDTG(NDXZPL),
     &           GRIDHT(NDXZPL+1), GRIDTG(NDXZPL+1), ZPLM, TGPM )
            CALL GINTRP( GRIDHT(NDXZPL), GRIDPT(NDXZPL),
     &           GRIDHT(NDXZPL+1), GRIDPT(NDXZPL+1), ZPLM, PTPM )
CRWB        Use average of stack top and midpoint temperature gradients.
            TGP = 0.5D0 * (TGS + TGPM)
            PTP = 0.5D0 * (PTS + PTPM)
            BVF = DSQRT( G * TGP / PTP )
            IF(BVF .LT. 1.0D-10) BVF = 1.0D-10
            BVPRIM  = 0.7D0 * BVF

            CALL DISTF

            KITER = KITER + 1

CRJP        Add temporary debugging statements

            IF(DEBUG) THEN
               WRITE(DBGUNT,6001) KITER,DHFOLD, DHFAER, ZPLM, UP,TGP
6001           FORMAT(/,5X,'OPTH2 ITER. #',I1,': OLD DELH = ',
     &          F6.1,' M; NEW DELH = ',F6.1,' M; MET LEVEL = ',
     &          F6.1,' M; NEW Upl = ',F5.2,' M/S; NEW DTHDZ = ',
     &          F7.4,' K/M')
            END IF

C           Check for convergence
            IF(DABS((DHFOLD - DHFAER)/DHFAER) .LT. 0.01D0) GO TO 60

            IF(KITER .GE. 5) THEN
               DHFAER = 0.5D0 * (DHFAER + DHFOLD)
               IF(DEBUG) WRITE(DBGUNT,6002) DHFAER
6002          FORMAT(/,5X,'PLUME RISE ITERATION FAILED TO CONVERGE; ',
     &         ' PLUME RISE SET AT ',F6.1,' METERS.',/)
               GO TO 60
            ELSE
               GO TO 50
            END IF

60          CONTINUE

CRWB        After completing iteration, reset UP and TGP to stack top
CRWB        values for subsequent distance-dependent plume rise calcs.
            UP = US
            TGP = TGS
            PTP = PTS
            BVF = DSQRT( G * TGP / PTP )
            IF(BVF .LT. 1.0D-10) BVF = 1.0D-10
            BVPRIM  = 0.7D0 * BVF
         END IF

C        Initialize PRM_FSTREC Logical Switch for First Receptor of Loop;
         PRM_FSTREC = .TRUE.

C        Initialize 'ARC' Arrays for EVALFILE Output        ---   CALL EVLINI
         IF (EVAL(ISRC)) THEN
            CALL EVLINI
         END IF

         ZMIDMX = 0.5D0 * ZI

CRJP     Add temporary debugging statement.

         IF( DEBUG) THEN
             WRITE(DBGUNT,6010) KURDAT, ZMIDMX
6010         FORMAT(/,72('*'),//,5X,'YR/MN/DY/HR: ',I8,//,
     &                5X,'Height assigned to midpoint of ',
     &         'well-mixed layer for effective parameters = ',
     &         F6.1,' meters.',/)
         END IF
CRJP
CRJP     Calculate distance to uniformly mixed plume within the
CRJP     boundary layer (XMIXED) after Turner's Workbook (1970), page 7:
CRJP     distance is approximately (Zi * UAVG)/SWAVG, where UAVG
CRJP     and SWAVG are wind speed and sigma-w averaged over the depth
CRJP     between the ground and Zi (or the plume height, if higher in
CRJP     stable conditions); this height is denoted as 2 * ZMIDMX.
CRJP
CRJP     First, get refined estimate of final rise and distance to final
CRJP     rise if downwash conditions prevail.
CRJP
         XFINAL = XMAX
         DHCRIT = DHFAER
         XMIXED = ZI * UAVG / SWAVG
         IF (UNSTAB .AND. HS.LT.ZI) THEN
C           Check for XMIXED smaller than 1.25*XFINAL
            IF (XMIXED .LT. 1.25D0*XFINAL) THEN
               XFINAL = 0.8D0 * XMIXED
               CALL CBLPRD (XFINAL)
               DHCRIT = DHP1
            END IF
         END IF


C ---    Initialize PDF parameters for use in calculating ZSUBP
         IF( UNSTAB  .AND.  (HS .LT. ZI) ) THEN
            CALL PDF
         END IF
C        Set Dry Deposition Variables for this Source
         IF (LUSERVD .AND. LDGAS .AND. NPD.EQ.0) THEN
C           Assign user-specified gas dry deposition velocity (GASDEPVD option)
            VDEPG = USERVD
         ELSE IF (LDPART .OR. (.NOT.LUSERVD .AND. LDGAS .AND.
     &                                                   NPD.EQ.0)) THEN
C           Calculate Deposition Velocities for this Source    ---   CALL VDP
            CALL VDP
         END IF
         IF (LWPART .OR. LWGAS) THEN
CPES        Set value of ZSUBP = MAX( ZI, TOP OF PLUME ), where
CPES        TOP OF PLUME is defined as plume height (HE) plus 2.15*SZ,
CPES        evaluated at a distance of 20 kilometers downwind.
CPES        Apply minimum value of 500m and maximum value of 10,000m.
            CALL HEFF (20000.0D0)
            IF( STABLE .OR. (UNSTAB .AND. HS .GE. ZI) )THEN
               HE = HSP + DHCRIT
               CALL SIGZ(20000.0D0)
               ZSUBP = MAX( 500.0D0, ZI, HE + SZCOEF*SZAS )
            ELSE IF (UNSTAB) THEN
               HED1 = HSP + DHCRIT
               IF (PPF .GT. 0.0D0) THEN
                  CALL CBLPR3
               END IF
               CALL SIGZ(20000.0D0)

               IF (PPF .EQ. 0.0D0) THEN
                  ZSUBP=MAX( 500.0D0, ZI, HED1 + SZCOEF*(SZAD1+SZAD2)/
     &                  2.0D0 )
               ELSE IF (PPF .EQ. 1.0D0) THEN
                  ZSUBP=MAX( 500.0D0, ZI, HE3 + SZCOEF*SZA3)
               ELSE
                  ZSUBP=MAX( 500.0D0, ZI, PPF*(HE3+SZCOEF*SZA3) +
     &                (1.0D0-PPF)*(HED1 + SZCOEF*(SZAD1+SZAD2)/2.0D0) )
               END IF
            END IF
            ZSUBP = MIN( 10000.0D0, ZSUBP )
C           Calculate Scavenging Ratios for this Source           ---   CALL SCAVRAT
            CALL SCAVRAT
         END IF

CRWB     Determine transport wind direction using midpoint between
CRWB     stack height and "final" plume height.  R. Brode, PES, 1/22/98
C----    Define ZMID=midpoint between stack height and "final" plume height
         ZMID = MIN( 4000.0D0, (HS + 0.5D0*DHFAER) )

C----    Locate index below ZMID
         CALL LOCATE(GRIDHT, 1, MXGLVL, ZMID, NDXZMID)

C----    Extract WD for grid levels above and below ZMID
         VALABV = GRIDWD(NDXZMID+1)
         VBELOW = GRIDWD(NDXZMID)

C----    Check for 360 crossover and adjust if necessary
         IF( (VALABV-VBELOW) .LT. -180.0D0) THEN
            VALABV = VALABV + 360.0D0
         ELSE IF( (VALABV-VBELOW) .GT. 180.0D0) THEN
            VALABV = VALABV - 360.0D0
         END IF

C----    Assign Wind direction
         IF (VBELOW .EQ. VALABV) THEN
            WDIR = VBELOW
         ELSE
C----       Interpolate to ZMID
            CALL GINTRP( GRIDHT(NDXZMID), VBELOW,
     &                   GRIDHT(NDXZMID+1), VALABV,
     &                   ZMID, WDIR )
         END IF

C        Check for WDIR > 360 or < 0
         IF (WDIR .GT. 360.0D0) THEN
            WDIR = WDIR - 360.0D0
         ELSE IF (WDIR .LE. 0.0D0) THEN
            WDIR = WDIR + 360.0D0
         END IF
C
C----    Convert direction to radians, compute sine and cosine of direction,
C        and determine nearest 10-degree sector.
C
C---->   wind direction = wind direction in degrees * DTORAD

         WDSIN = DSIN(WDIR * DTORAD)
         WDCOS = DCOS(WDIR * DTORAD)

C ---    Save WDSIN and WSCOS for later use by PVMRM/GRSM option
         AWDSIN(ISRC) = WDSIN
         AWDCOS(ISRC) = WDCOS

         AFV = WDIR - 180.0D0
         IF (AFV .LT. 0.0D0) THEN
            AFV = AFV + 360.0D0
         END IF
         AAFV(ISRC) = AFV       ! save flowvector for this source
                                ! for use in MAXDCONT processing
         IFVSEC = IDINT (AFV*0.10D0 + 0.4999D0)
         IF (IFVSEC .EQ. 0) IFVSEC = 36

c
c --- PRIME ---------------------------------------------------------
c ---    Setup computations for numerical plume rise algorithm
c ---    and building wake analysis
         if(WAKE) then
c ---       Store selected data in new variables for future reference
            ustack=us

c ---       Compute wind speed at top of building           ---   CALL WSADJ
c ---       Locate index below building height
            CALL LOCATE(GRIDHT, 1, MXGLVL, DSBH, NDXBH)

            CALL GINTRP( GRIDHT(NDXBH), GRIDWS(NDXBH),
     &           GRIDHT(NDXBH+1), GRIDWS(NDXBH+1), DSBH, UBLDG )

c ---       Refresh /WAKEDAT/ variables                     ---   CALL WAKE_INI
            ldbhr=PRIMEDBG
c ---       Note that logical RURAL has no impact on calculations
            rural = .true.
            call WAKE_INI(kurdat,ldbhr,PRMDBUNT,rural,dsbh,dsbw,dsbl,
     &                    xadj,yadj,ubldg,ustack)
         end if
c ------------------------------------------------------------

C ---- PLATFORM DOWNWASH ADDITIONS (BEGIN) -------------------
C   Compute distance INDENPENDENT AC and BC values for CUBIC (function PLAT_CUBIC) to
C   determine plume rise adjustment (function PLAT_GRADPLUMADJ); AC & BC are global
C   variables in modules.f/MAIN1. Taken from OCD(v5).FOR/PTR at PTR01130.
C   Michelle G. Snyder, Wood 7/29/2021

CCRT   D063
       IF (OSPLAT(ISRC)) THEN
          AC = 0.0D0
          BC = 0.0D0
          PLATSZ0 = 0.0D0
          DUM = 0.0D0
          IF ((PLATWB(ISRC) > 0.0D0) .AND. (PLATHB(ISRC) > 0.0D0)) THEN
               CALL PLAT_DOWNWASH (2.2D0*PLATHB(ISRC),
     &                PLATHB(ISRC)+0.0D0, PLATWB(ISRC), 0.0D0,
     &                PLATSZ0, DUM)
               RZ0 = PLATSZ0/DSQRT(2.0D0 / PI)
               AC = 10.0D0/3.0D0 * RZ0
               BC = 25.0D0/3.0D0 * RZ0 * RZ0
          END IF

          IF (PLATFMDBG) THEN
             WRITE(PLATFMDBUNT,'(A,1(A, 2X, I6), 6(A, 2X, F8.2))')
     &           'calc1.f/PCALC: ',
     &           ' ISRC = ', ISRC,
     &           ' PLATWB(ISRC) = ', PLATWB(ISRC),
     &           ' PLATHB(ISRC) = ', PLATHB(ISRC),
     &           ' PLATSZ0 = ', PLATSZ0,
     &           ' RZ0 = ', RZ0,
     &           ' AC = ', AC,
     &           ' BC = ', BC
          END IF

       END IF
C ---- PLATFORM DOWNWASH ADDITIONS (END) -------------------

CRJP     Add temporary debugging statement.

         IF(DEBUG) THEN
C ---       Include "effective" wind direction here
            WRITE(DBGUNT, 6011) DHCRIT, XFINAL, XMIXED, AFV
6011        FORMAT(5X,'For effective parameter calculations: ',
     &        '"Final" plume rise = ',G14.6, ' m; Distance to final ',
     &        'rise = ',G14.6,' m',/,5x,'Distance to well-mixed ',
     &        'state = ',G14.6,' m;',/,5x,'"Effective" flow vector = ',
     &         F6.2)
CRJP
CRJP        Make call to PSRDEB
CRJP
            CALL PSRDEB

         END IF
C
C        Begin Receptor LOOP *******************************************
CCRFL
CCRFL    Add logical variable METHDR, which is set to TRUE at the start
CCRFL    of the receptor loop, and reset to false after the headers and
CCRFL    non-receptor dependent meteorological variables in the
CCRFL    meteorological debug file are printed.  METHDR is also added
CCRFL    to MAIN.INC and METEXT.FOR (Subroutine METDEB).  9/27/94, R.F. Lee.
CCRFL
CCRWB    METHDR assignment statement moved to SUB. CALC.  12/28/05, R.W. Brode.

         RECEPTOR_LOOP: DO IREC = 1, NUMREC
CCRT        D063 Platform Downwash Debug
            IF (PLATFMDBG .AND. OSPLAT(ISRC)) THEN
               WRITE(PLATFMDBUNT,'(A, A, I4, 3(2X, A, I4), A)')
     &         '-------------',
     &         'JDAY= ', JDAY,
     &         'IHOUR = ', IHOUR,
     &         'ISRC= ', ISRC,
     &         'IREC= ', IREC,
     &         '-------------'
            END IF


C           Calculate Down and Crosswind Distances          ---   CALL XYDIST
            IF (AWMADWDBG) THEN
               WRITE(AWMADWDBUNT,*) '  Receptor  ', irec
            END IF

            IF (EVONLY) THEN
               CALL XYDIST(IEVENT)
            ELSE
               CALL XYDIST(IREC)
            END IF

C ---       First check for receptor exceeding minimum distance (1m) or
C           maximum distance (80km for FASTALL or 1.0D20 otherwise).
            IF (DISTR .LT. 0.99D0 .OR. DISTR .GT. MAXDIST) THEN
C              Receptor distance exceeds the minimum or maximum distance;
C              assign 0.0 to applicable arrays, and cycle to next receptor
               HRVAL(:)  = 0.0D0
               AERPLM(:) = 0.0D0
               AERPAN(:) = 0.0D0
               PRMVAL(:) = 0.0D0

               IF (PVMRM .OR. OLM .OR. ARM2 .OR.
     &            RUNTTRM .OR. GRSM) THEN
C ---             Assign 0.0 to CHI(IREC,ISRC,ITYP) before
C                 cycling to the next receptor to avoid
C                 persisting value from previous hour.
                  CHI(IREC,ISRC,:) = 0.0D0
                  IF(GRSM)THEN
                    CHI_TTRAVPLM = 0.0D0
                    CHI_TTRAVPAN = 0.0D0
                    CHI_TTRAVAER = 0.0D0
                    CHI_TTRAVPRM = 0.0D0
                    CHI_TTRAVCHM(IREC,ISRC) = 0.0D0
                  END IF
                  IF (PVMRM .OR. GRSM) THEN
                     HECNTR(IREC,ISRC)  = 0.0D0
                     UEFFS(IREC,ISRC)   = 0.0D0
                     EPSEF(IREC,ISRC)   = 0.0D0
                     PPFACT(IREC,ISRC)  = 0.0D0
                     HECNTR3(IREC,ISRC) = 0.0D0
                     UEFF3S(IREC,ISRC)  = 0.0D0
                     EPSEF3(IREC,ISRC)  = 0.0D0
                     FOPTS(IREC,ISRC)   = 0.0D0
                  END IF
               END IF
C ---          Cycle to next receptor
               CYCLE RECEPTOR_LOOP
            END IF

C ---       Calculate AERMOD Concentration Without Downwash, AERVAL
C ---       First calculate coherent plume component using downwind distance
            L_PLUME = .TRUE.
C ---       Assign XDIST for use in dry depletion (FUNCTION F2INT)
            XDIST = X

C ---       CALL AERCALC to calculate concentrations without downwash;
C           downwash is handled later in PRMCALC, if applicable
            CALL AERCALC( X, L_PLUME, AERPLM )

C ---       CERC 11/30/20 Calculate the conc*travel time for the Gaussian plume
            IF(GRSM)THEN
              IF (STABLE .OR. (UNSTAB.AND.(HS.GE.ZI))) THEN
                  UCHM=UEFF
              ELSE
                  UCHM=UEFFD
              END IF
              IF (UCHM/=0) THEN
                CHI_TTRAVPLM=MAX(0.0D0,AERPLM(1)*X/UCHM) !Don't allow negative conc*travel times
              ELSE
                !Error - no travel time if UCHM is zero
                CALL ERRHDL(PATH, MODNAM, 'E','601','GRSM')
                RUNERR=.TRUE.
                RETURN
              END IF
            END IF

            IF (L_EFFSIGY) THEN
C ---          No "pancake" calculation for non-DFAULT FASTALL option (EFFSIGY),
C              assign AERPLM to AERVAL and set FRAN and AERPAN to 0.0
               AERVAL = AERPLM
               AERPAN = 0.0D0
               FRAN   = 0.0D0
C ---          In this case travel time is just the conc*travel time in the Gaussian plume
               IF(GRSM)THEN
                 CHI_TTRAVPAN=0.0D0
                 CHI_TTRAVAER=CHI_TTRAVPLM
               END IF

            ELSE

C ---          Next calculate random "pancake" component using radial distance
               L_PLUME = .FALSE.
C ---          Assign XDIST for use in dry depletion (FUNCTION F2INT)
               XDIST = DISTR
C ---          Call AERCALC to get random "pancake" component, AERPAN
               CALL AERCALC( DISTR, L_PLUME, AERPAN )

C ---          CERC 11/30/20 Calculate the conc*travel time for the "pancake" component
               IF(GRSM)THEN
                 IF (STABLE .OR. (UNSTAB.AND.(HS.GE.ZI))) THEN
                   UCHM=UEFF
                 ELSE
                   UCHM=UEFFD
                 END IF
                 IF (UCHM/=0) THEN
                   CHI_TTRAVPAN=MAX(0.0D0,AERPAN(1)*XDIST/UCHM) !Don't allow negative conc*travel time
                 ELSE
                   !Error - no travel time if UCHM is zero
                   CALL ERRHDL(PATH, MODNAM, 'E','601','GRSM')
                   RUNERR=.TRUE.
                   RETURN
                 END IF
               END IF

C ---          Calculate fraction of random kinetic energy to total kinetic energy.
C              Note that these effective parameters are based on the radial dist.
               IF (STABLE .OR. (UNSTAB.AND.(HS.GE.ZI))) THEN
                  CALL MEANDR( UEFF, SVEFF, FRAN )
               ELSE IF (UNSTAB) THEN
                  CALL MEANDR( UEFFD, SVEFFD, FRAN )
                  IF (PPF .GT. 0.0D0) THEN
C                    For penetrated source calculate weighted average of
C                    direct/indirect plume component and penetrated component
                     CALL MEANDR( UEFF3, SVEFF3, FRAN3 )
                     FRAN = PPF*FRAN3 + (1.0D0-PPF)*FRAN
                  END IF
               END IF

! Added for TTRM; AECOM
      IF (RUNTTRM .AND. UNSTAB) THEN
CCRFL
CCRFL    Revised emission rate terms:  for CHID & CHIN, to QTK*(1-PPF),
CCRFL    and for CHI3 to QTK*PPF.  Ref:  P.D.F. Model for Dispersion in
CCRFL    the Convective Boundary Layer, J.C. Weil, 6/27/94.  Changed
CCRFL    7/19/94, R.F. Lee.
CCRFL
       CHID = (QTK*EMIFAC(1) * (1.0D0-PPF) / UEFFD) * (FSUBYD*FSUBZD)
       CHIN = (QTK*EMIFAC(1) * (1.0D0-PPF) / UEFFN) * (FSUBYN*FSUBZN)
      END IF

               IF (RUNTTRM) THEN
                 TTRMOUT(IREC,ISRC,12) = UEFF
                 TTRMOUT(IREC,ISRC,13) = UEFFD
                 TTRMOUT(IREC,ISRC,14) = UEFF3
                 TTRMOUT(IREC,ISRC,15) = FRAN
                 TTRMOUT(IREC,ISRC,16) = PPF
! GAMFACT is saved to TTRMOUT(IREC,ISRC,17) below
                 TTRMOUT(IREC,ISRC,18) = CHIN
                 TTRMOUT(IREC,ISRC,19) = CHID
               END IF
! End TTRM insert; Feb. 2021

C ---          Combine "random" and "coherent" plume contributions
               AERVAL = FRAN*AERPAN + (1.0D0-FRAN)*AERPLM

C ---          CERC 11/30/20 Calculate the conc*travel time for the combined plume
               IF(GRSM)THEN
                 CHI_TTRAVAER=FRAN*CHI_TTRAVPAN +
     &                                         (1.0D0-FRAN)*CHI_TTRAVPLM
               END IF

            END IF

C           ENSR STATEMENT
            IF(DEBUG) THEN
               DO ITYP = 1, NUMTYP
                  WRITE(DBGUNT,10) AERPAN(ITYP), AERPLM(ITYP), FRAN,
     &                             AERVAL(ITYP)
10              FORMAT(/,'AERVAL(ITYP) = FRAN*AERPAN(ITYP) + (1.-FRAN)',
     &                                              '*AERPLM(ITYP)',//,
     &                    'PANCAKE/MEANDER COMPONENT, AERPAN(ITYP) = ',
     &            G16.8,/,'COHERENT PLUME COMPONENT,  AERPLM(ITYP) = ',
     &            G16.8,/,'MEANDER FACTOR, FRAN = ',
     &            G16.8,/,'RESULTANT CONC, AERVAL(ITYP) = ',G16.8,//)
               END DO
            END IF

            IF (WAKE .AND. (STABLE .OR. HS.LE.ZI)) THEN
c ---          Apply wake effects - note that wake effects are not applied
c              for stack heights > ZI.
c ---          Calculate receptor coordinates relative to upwind face of bldg.:
c              xbrec is downwind dist. of receptor from upwind
c              bldg face; ybrec is crosswind dist. of receptor from
c              center of upwind bldg. face
               xbrec=x-xadj
               ybrec=y-yadj

               XDIST = X
C ---          Calculate PRIME Downwash Concentration, PRMVAL
               CALL PRMCALC ( XBREC, YBREC )
C ---          Check for runtime error (RUNERR) before continuing
               IF (RUNERR) RETURN

C ---          CERC 11/30/20 Calculate conc*travel time when buildings effects included
               IF(GRSM)THEN
                 IF (UEFF/=0) THEN
                   CHI_TTRAVPRM=MAX(0.0D0,PRMVAL(1)*XDIST/UEFF) !Don't allow negative conc*travel time
                 ELSE
                   !Error - no travel time if UEFF is zero
                   CALL ERRHDL(PATH, MODNAM, 'E','601','GRSM')
                   RUNERR=.TRUE.
                   RETURN
                 END IF
               END IF

C ---          Calculate Gamma weighting factor, GAMFACT
               CALL GAMCALC ( XBREC, YBREC )

C ---          Calculate hourly concentration from PRIME and AERMOD values (1:NUMTYP)
               HRVAL =  GAMFACT  * PRMVAL + (1.0D0-GAMFACT) * AERVAL

C              CERC 11/30/20
C ---          Calculate conc*travel time from PRIME and AERMOD values
               IF(GRSM)THEN
                 CHI_TTRAVCHM(IREC,ISRC) = GAMFACT*CHI_TTRAVPRM +
     &                                      (1.0D0-GAMFACT)*CHI_TTRAVAER
               END IF

               if (PRIMEDBG) then
                  DO ITYP = 1, NUMTYP
                    write(PRMDBUNT,*) ' '
                    write(PRMDBUNT,*) 'YR/MN/DY/HR:     ',kurdat,
     &                                ' ISRC: ',isrc,
     &                                ' IREC: ',irec
                    write(PRMDBUNT,*)
                    write(PRMDBUNT,*) ' GAMFACT = ',GAMFACT
                    write(PRMDBUNT,*) ' AERVAL  = ',AERVAL(ITYP)
                    write(PRMDBUNT,*) ' PRMVAL  = ',PRMVAL(ITYP)
                    write(PRMDBUNT,*) ' HRVAL   = ',HRVAL(ITYP)
                    write(PRMDBUNT,*) ' '
                  END DO
               end if

            ELSE
C ---          No WAKE effects or HS > ZI, set GAMFACT to 0.0 and use AERVAL only.
               GAMFACT = 0.0D0
C ---          Calculate hourly concentration from PRIME and AERMOD values (1:NUMTYP)
               HRVAL  = AERVAL
               PRMVAL = 0.0D0
C ---          CERC 11/30/20 Calculate conc*travel time (no buildings)
               IF(GRSM)THEN
                 CHI_TTRAVCHM(IREC,ISRC) = CHI_TTRAVAER
               END IF
            END IF

!     Added for TTRM; AECOM
            IF (RUNTTRM) THEN
              TTRMOUT(IREC,ISRC,10) = X
              TTRMOUT(IREC,ISRC,11) = DISTR
              TTRMOUT(IREC,ISRC,20) = Y
              TTRMOUT(IREC,ISRC,17) = GAMFACT
              TTRMOUT(IREC,ISRC,22) = AERVAL(1)
              TTRMOUT(IREC,ISRC,23) = PRMVAL(1)
!              TTRMOUT(IREC,ISRC,24) = UEFFe ! Effective wind speed for downwash (saved in PRM_PCHI)
            END IF
!     End TTRM insert; Feb. 2021

            IF (PVMRM) THEN
C ---          Store data by source and receptor for PVMRM option
               DO ITYP = 1, NUMTYP
                  CHI(IREC,ISRC,ITYP) = HRVAL(ITYP)
               END DO
               IF (STABLE .OR. (UNSTAB.AND.(HS.GE.ZI))) THEN
                  HECNTR(IREC,ISRC) = HE
                  UEFFS(IREC,ISRC)  = UEFF
                  EPSEF(IREC,ISRC)  = EPSEFF
               ELSE
                  HECNTR(IREC,ISRC) = CENTER
                  UEFFS(IREC,ISRC)  = UEFFD
                  EPSEF(IREC,ISRC)  = EPSEFFD
               END IF
               IF (PPF .GT. 0.0D0) THEN
                  PPFACT(IREC,ISRC)  = PPF
                  HECNTR3(IREC,ISRC) = HE3
                  UEFF3S(IREC,ISRC)  = UEFF3
                  EPSEF3(IREC,ISRC)  = EPSEFF3
               ELSE
                  PPFACT(IREC,ISRC)  = 0.0D0
                  HECNTR3(IREC,ISRC) = 0.0D0
                  UEFF3S(IREC,ISRC)  = 0.0D0
                  EPSEF3(IREC,ISRC)  = 0.0D0
               END IF
               FOPTS(IREC,ISRC) = FOPT

C              Initialize __VAL arrays (1:NUMTYP)
               HRVAL   = 0.0D0
               AERVAL  = 0.0D0
               AERPLM  = 0.0D0
               AERPAN  = 0.0D0
               PRMVAL  = 0.0D0

C              Cycle to next receptor & skip call to SUMVAL (will be done later)
               CYCLE RECEPTOR_LOOP

            ELSE IF (OLM) THEN
C ---          Store conc by source and receptor for OLM option
               DO ITYP = 1, NUMTYP
                  CHI(IREC,ISRC,ITYP) = HRVAL(ITYP)
               END DO

C              Initialize __VAL arrays (1:NUMTYP)
               HRVAL   = 0.0D0
               AERVAL  = 0.0D0
               AERPLM  = 0.0D0
               AERPAN  = 0.0D0
               PRMVAL  = 0.0D0

C              Cycle to next receptor & skip call to SUMVAL (will be done later)
               CYCLE RECEPTOR_LOOP
!     Added for TTRM; AECOM
            ELSE IF (RUNTTRM) THEN
C ---          Store conc by source and receptor for TTRM option
               DO ITYP = 1, NUMTYP
                  CHI(IREC,ISRC,ITYP) = HRVAL(ITYP)
               END DO

C              Initialize __VAL arrays (1:NUMTYP)
               HRVAL   = 0.0D0
               AERVAL  = 0.0D0
               AERPLM  = 0.0D0
               AERPAN  = 0.0D0
               PRMVAL  = 0.0D0

C              Cycle to next receptor & skip call to SUMVAL (will be done later)
               CYCLE RECEPTOR_LOOP
!    End TTRM insert; Feb. 2021

            ELSE IF (ARM2) THEN
C ---          Store conc by source and receptor for ARM2 options
               DO ITYP = 1, NUMTYP
                  CHI(IREC,ISRC,ITYP) = HRVAL(ITYP)
               END DO

C              Initialize __VAL arrays (1:NUMTYP)
               HRVAL   = 0.0D0
               AERVAL  = 0.0D0
               AERPLM  = 0.0D0
               AERPAN  = 0.0D0
               PRMVAL  = 0.0D0

C              Cycle to next receptor & skip call to SUMVAL (will be done later)
               CYCLE RECEPTOR_LOOP

C           CERC 11/30/20
            ELSE IF (GRSM) THEN
C ---          Store conc by source and receptor for GRSM options
               DO ITYP = 1, NUMTYP
                  CHI(IREC,ISRC,ITYP) = HRVAL(ITYP)
               END DO

               IF (STABLE .OR. (UNSTAB.AND.(HS.GE.ZI))) THEN
                  HECNTR(IREC,ISRC) = HE
                  UEFFS(IREC,ISRC)  = UEFF
                  EPSEF(IREC,ISRC)  = EPSEFF
               ELSE
                  HECNTR(IREC,ISRC) = CENTER
                  UEFFS(IREC,ISRC)  = UEFFD
                  EPSEF(IREC,ISRC)  = EPSEFFD
               END IF
               IF (PPF .GT. 0.0D0) THEN
                  PPFACT(IREC,ISRC)  = PPF
                  HECNTR3(IREC,ISRC) = HE3
                  UEFF3S(IREC,ISRC)  = UEFF3
                  EPSEF3(IREC,ISRC)  = EPSEFF3
               ELSE
                  PPFACT(IREC,ISRC)  = 0.0D0
                  HECNTR3(IREC,ISRC) = 0.0D0
                  UEFF3S(IREC,ISRC)  = 0.0D0
                  EPSEF3(IREC,ISRC)  = 0.0D0
               END IF
               FOPTS(IREC,ISRC) = FOPT

C              Initialize __VAL arrays (1:NUMTYP)
               HRVAL   = 0.0D0
               AERVAL  = 0.0D0
               AERPLM  = 0.0D0
               AERPAN  = 0.0D0
               PRMVAL  = 0.0D0

C              Cycle to next receptor & skip call to SUMVAL (will be done later)
               CYCLE RECEPTOR_LOOP

            END IF

C           Sum HRVAL to AVEVAL and ANNVAL Arrays  ---   CALL SUMVAL
C           As noted just above, the calls to EV_SUMVAL and SUMVAL
C           are skipped for a PVMRM, OLM, ARM2 or GRSM run; the
C           summing is performed for these options in the
C           respective routines
            IF (EVONLY) THEN
               CALL EV_SUMVAL
            ELSE
               DO IGRP = 1, NUMGRP
                  CALL SUMVAL
               END DO
            END IF

            IF (EVAL(ISRC)) THEN
C              Check ARC centerline values for EVALFILE
C              output                              ---   CALL EVALCK
               CALL EVALCK
            END IF

C           Initialize __VAL arrays (1:NUMTYP)
            HRVAL   = 0.0D0
            AERVAL  = 0.0D0
            AERPLM  = 0.0D0
            AERPAN  = 0.0D0
            PRMVAL  = 0.0D0

         END DO RECEPTOR_LOOP
C        End Receptor LOOP

C        Output 'ARC' Values for EVALFILE                   ---   CALL EVALFL
         IF (EVAL(ISRC)) THEN
            CALL EVALFL
         END IF

      END IF

      RETURN
      END

      SUBROUTINE AERCALC( XARG, L_PLUME, AEROUT )
C***********************************************************************
C             AERCALC Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Calculates the AERMOD concentration without downwash
C
C        PROGRAMMER: Roger Brode, PES, Inc.
C
C        DATE:     November 10, 2000
C
C        CHANGES:
C                  Added gradual plume rise adjustment (PLAT_GRADPLUMADJ)
C                  when PLATFORM is present.
C                  Michelle G. Snyder, WOOD, 8/5/2021
C
C                  Added debug statement based on ENSR code.
C                  R. W. Brode, MACTEC (f/k/a PES), Inc., 07/27/04
C
C        INPUTS:   XARG         - Real - Distance (m), downwind for coherent
C                                        plume component (X) and radial for
C                                        random component (DISTR)
C                  L_PLUME      - Log  - Specifies coherent plume calculation
C                                        if TRUE, otherwise random component
C
C        OUTPUTS:  AEROUT(NTYP) - Real - AERMOD component of concentration
C                                        without building downwash for either
C                                        coherent plume component or for
C                                        random component, depending on
C                                        L_PLUME.
C
C        CALLED FROM:   PCALC
C
C***********************************************************************
C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      INTEGER :: J
      DOUBLE PRECISION :: AEROUT(NUMTYP), AERTMP(NUMTYP), FYOUT, XARG,
     &                    ADJ, FRAN, FRAN3
      LOGICAL :: L_PLUME
C Unused:       INTEGER :: NDXZMID
C Unused:       DOUBLE PRECISION :: ZMID, VALABV, VBELOW

CMGS  Wood 8/5/2021 D063 Added to make plume rise adjustment due to PLATFORM
CMGS  External Functions:
      DOUBLE PRECISION, EXTERNAL  :: PLAT_GRADPLUMADJ

C     Variable Initializations
      MODNAM = 'AERCALC'

C     Initialize AEROUT(NUMTYP) and AERTMP(NUMTYP) arrays
      AEROUT(:) = 0.0D0
      AERTMP(:) = 0.0D0

C --- Initialize FRAN and FRAN3
      FRAN  = 0.0D0
      FRAN3 = 0.0D0

      IF (XARG .LT. 1.0D0) THEN
C        Receptor is too close to source for calculation
C        or upwind of source for coherent plume component
         AEROUT(:) = 0.0D0
         RETURN

      END IF

C     Determine Deposition Correction Factors
      IF (NPD .EQ. 0 .AND. (LDGAS .OR. LWGAS)) THEN
         CALL PDEPG (XARG)
      ELSE
         DQCORG = 1.0D0
         WQCORG = 1.0D0
      END IF
      IF (NPD .GT. 0 .AND. (LDPART .OR. LWPART)) THEN
         CALL PDEP (XARG)
      ELSE IF (NPD .GT. 0) THEN
C        Set DQCOR(NPD) and WQCOR(NPD) arrays to 1.0
         DQCOR = 1.0D0
         WQCOR = 1.0D0
      END IF

C     Set initial effective parameters
      UEFF  = US
      SVEFF = SVS
      SWEFF = SWS
      TGEFF = TGS
      IF ( UNSTAB  .AND.  (HS .LT. ZI) ) THEN
         UEFFD  = US
         SVEFFD = SVS
         SWEFFD = SWS
         UEFFN  = US
         SVEFFN = SVS
         SWEFFN = SWS
         UEFF3  = US
         SVEFF3 = SVS
         SWEFF3 = SWS
         TGEFF3 = TGS
      END IF

CRJP  Add temporary debugging statement here.

C     ENSR ENHANCEMENT OF WRITE STATEMENT TO IDENTIFY COMPONENT CONCENTRATION
      IF(DEBUG) THEN
        WRITE(DBGUNT, 6014) SRCID(ISRC)
6014    FORMAT(//,'SRCID: ', A8)
        IF(L_PLUME)THEN
           WRITE(DBGUNT, 6015) UEFF, SVEFF, SWEFF
6015       FORMAT(//,'COHERENT PLUME COMPONENT',/,5X,
     &       'Initial effective parameters for ',
     &       'stable or direct convective ',
     &       'plume:',//,5x,'Ueff = ',F7.2,' m/s; ',
     &       'SVeff = ',F7.2,
     &       ' m/s; SWeff = ',F7.2,' m/s.',/)
        ELSE
           WRITE(DBGUNT, 6016) UEFF, SVEFF, SWEFF
6016       FORMAT(//,'MEANDER COMPONENT',/,5X,
     &       'Initial effective parameters for ',
     &       'stable or direct convective ',
     &       'plume:',//,5x,'Ueff = ',F7.2,' m/s; ',
     &       'SVeff = ',F7.2,
     &       ' m/s; SWeff = ',F7.2,' m/s.',/)
        END IF
      END IF

C     Define plume centroid height (CENTER) for use in
C     inhomogeneity calculations
      CALL CENTROID ( XARG )

C     Calculate the plume rise                     ---   CALL DELTAH
      CALL DELTAH ( XARG )

C     If the atmosphere is unstable and the stack
C     top is below the mixing height, calculate
C     the CBL PDF coefficients                     ---   CALL PDF
      IF( UNSTAB  .AND.  (HS .LT. ZI) ) THEN
         CALL PDF
      END IF

C     Determine Effective Plume Height             ---   CALL HEFF
      CALL HEFF ( XARG )
C ----------------------------------------------------------------------
C ---------- BEGIN PLATFORM DOWNWASH PLUME RISE ADJUSTMENT -------------
C ----------------------------------------------------------------------
C        Adjust plume rise if PLATFORM is present. This will decrease the
C        effective height when there is a PLATFORM due to downwash effects
C        of the structure on top of the platform. Calls PLAT_GRADPLUMEADJ,
C        PLAT_DOWNWASH, and PLAT_CUBIC (all contained in calc1.f).
C        Michelle G. Snyder, WOOD, 8/5/2021
CCRT     D063
         IF (OSPLAT(ISRC) .AND. (XARG > 0.0D0)) THEN
            IF (STABLE) THEN
               DHP = DHP - PLAT_GRADPLUMADJ( XARG )
            ELSE !modify direct plume downdraft (DHP1) only
               DHP1 = DHP1 - PLAT_GRADPLUMADJ( XARG )
            END IF

C           Get new effective heights based on adjusted plume rise from platform
            CALL HEFF ( XARG )

         END IF

C ----------------------------------------------------------------------
C --------- END OF PLATFORM DOWNWASH PLUME RISE ADJUSTMENT -------------
C----------             RESUME NORMAL PROCESSING           -------------
C ----------------------------------------------------------------------

C     Compute effective parameters using an
C     average through plume layer
      CALL IBLVAL ( XARG )

C     Call PDF & HEFF again for final CBL plume heights
      IF (UNSTAB .AND. (HS.LT.ZI) ) THEN
         CALL PDF
         CALL HEFF ( XARG )
      END IF

C     Determine Dispersion Parameters              ---   CALL PDIS
      CALL PDIS ( XARG )

C     Calculate the 'y-term' contribution to
C     dispersion, FSUBY
      IF (L_PLUME) THEN
         IF (L_EFFSIGY) THEN
C ---       Calculate fraction of random kinetic energy to total kinetic energy
C           for FASTALL option to optimize meander using effective sigma-y.
C           Note that these effective parameters are based on the downwind distance,
C           rather than the radial distance used in the standard meander approach
            IF (STABLE .OR. (UNSTAB.AND.(HS.GE.ZI))) THEN
               CALL MEANDR( UEFF, SVEFF, FRAN )
            ELSE IF (UNSTAB) THEN
               CALL MEANDR( UEFFD, SVEFFD, FRAN )
               IF (PPF .GT. 0.0D0) THEN
C                 For penetrated source calculate weighted average of
C                 direct/indirect plume component and penetrated component
                  CALL MEANDR( UEFF3, SVEFF3, FRAN3 )
                  FRAN = PPF*FRAN3 + (1.0D0-PPF)*FRAN
               END IF
            END IF

C           Calculate effective sigma-y for non-DFAULT FASTALL option (L_EFFSIGY)
            SYEFF = 1.0D0/((FRAN/(SRT2PI*XARG)) + (1.0D0-FRAN)/SY)
            IF (L_EFFSIGY) THEN
               IF (DABS(Y) .GT. NUMSYEFF*SYEFF) THEN
C                 Plume is more than 4 sigmas off centerline, skip calculation
                  FYOUT = 0.0D0
               ELSE
C                 Calculate FSUBY for coherent plume        ---   CALL FYPLM
                  CALL FYPLM(SYEFF,FYOUT)
               END IF
            ELSE
               IF (X.LT.1.0D0 .OR. DABS(Y) .GT. NUMSYEFF*SYEFF) THEN
C                 Receptor is upwind of source or more than 4 sigmas off
C                 ceenterline, skip calculation
                  FYOUT = 0.0D0
               ELSE
C                 Calculate FSUBY for coherent plume        ---   CALL FYPLM
                  CALL FYPLM(SYEFF,FYOUT)
               END IF
            END IF

         ELSE
C           Calculate FSUBY for coherent plume        ---   CALL FYPLM
            CALL FYPLM(SY,FYOUT)

         END IF

      ELSE
C        Calculate FSUBY for random component      ---   CALL FYPAN
         CALL FYPAN(FYOUT)

      END IF

      FSUBY  = FYOUT
      FSUBYD = FSUBY
      FSUBYN = FSUBYD

C     Calculate the 'y-term' contribution to dispersion
C     for the penetrated plume, FSUBY3
      IF( UNSTAB  .AND.  (HS .LT. ZI)  .AND. (PPF .GT. 0.0D0) )THEN
C        Compute meander fraction of horizontal distribution function
C        from Venky's memo of 6/24/98.
         IF (L_PLUME) THEN
            IF (L_EFFSIGY) THEN
C ---          Calculate fraction of random kinetic energy to total kinetic energy
C              for FASTALL option to optimize meander using effective sigma-y.
C              Note that these effective parameters are based on the downwind distance,
C              rather than the radial distance used in the standard meander approach
               SYEFF = 1.0D0/((FRAN/(SRT2PI*XARG)) + (1.0D0-FRAN)/SY3)

               IF (DABS(Y) .GT. NUMSYEFF*SYEFF) THEN
C                 Plume is more than 4 sigmas off centerline, skip calculation
                  FYOUT = 0.0D0
               ELSE
C                 Calculate FSUBY for coherent plume        ---   CALL FYPLM
                  CALL FYPLM(SYEFF,FYOUT)
               END IF

            ELSE
C              Calculate FSUBY for coherent plume        ---   CALL FYPLM
               CALL FYPLM(SY3,FYOUT)
            END IF
         ELSE
C           Calculate FSUBY for random component   ---   CALL FYPAN
            CALL FYPAN(FYOUT)
         END IF

         FSUBY3 = FYOUT
      ELSE
         FSUBY3 = 0.0D0
      END IF

C     Check for zero "y-terms"; if zero then skip calculations
C     and go to next receptor.
      IF( FSUBY.EQ.0.0D0 .AND. FSUBY3.EQ.0.0D0 )THEN
C        Set AEROUT(NUMTYP) array to 0.0
         AEROUT(:) = 0.0D0

      ELSE

         IF (NPD .EQ. 0) THEN
C           Perform calculations for gases
C           Assign plume tilt, HV = 0.0
            HV = 0.0D0

            ADJ = DQCORG * WQCORG

            IF (STABLE .OR. (UNSTAB.AND.(HS.GE.ZI))) THEN
C              Calculate height of the "effective reflecting surface"
               CALL REFL_HT (HE, XARG, SZB, 0.0D0, HSBL)
            ELSEIF ( UNSTAB ) THEN
               HSBL = 0.0D0
            END IF

            IF (UNSTAB .AND. (HS.LT.ZI) .AND. (PPF.GT.0.0D0)) THEN
C              Calculate height of the "effective reflecting surface"
               CALL REFL_HT (HE3, XARG, SZB3, 0.0D0, HPEN)
            ELSE
               HPEN = 0.0D0
            END IF

C           Determine the CRITical Dividing Streamline---   CALL CRITDS
            CALL CRITDS (HE)

C           Calculate the fraction of plume below
C           HCRIT, PHEE                               ---   CALL PFRACT
            CALL PFRACT (HE)

C           Calculate FOPT = f(PHEE)                  ---   CALL FTERM
            CALL FTERM

C           Calculate AERMOD Concentration     ---   CALL AER_PCHI
            CALL AER_PCHI( XARG, ADJ, VDEPG, 0, AEROUT(:) )

         ELSE
C           Perform calculations for particles, loop through particle sizes

C           Begin loop over particle sizes
            DO J = 1, NPD

C              Calculate Plume Tilt Due to Settling, HV
               HV = (XARG/US) * VGRAV(J)

C              Adjust Jth contribution by mass fraction and source
C              depletion
               ADJ = PHI(J) * DQCOR(J) * WQCOR(J)

               IF (STABLE .OR. (UNSTAB.AND.(HS.GE.ZI))) THEN
C                 Calculate height of the "effective reflecting surface"
C                 Calculate Settled Plume Height(s), HESETL
                  HESETL = MAX( 0.0D0, HE - HV )
                  CALL REFL_HT (HESETL, XARG, SZB, 0.0D0, HSBL)
               ELSEIF ( UNSTAB ) THEN
                  HESETL = MAX( 0.0D0, 0.5D0*(HED1+HED2) - HV )
                  HSBL = 0.0D0
               END IF

               IF (UNSTAB .AND. (HS.LT.ZI) .AND. (PPF.GT.0.0D0)) THEN
C                 Calculate height of the "effective reflecting surface"
C                 Calculate Settled Plume Height(s), HE3SETL
                  HE3SETL = MAX( 0.0D0, HE3 - HV )
                  CALL REFL_HT (HE3SETL, XARG, SZB3, 0.0D0, HPEN)
                  HPEN = MAX( HPEN, ZI )
               ELSE
                  HPEN = 0.0D0
               END IF

C              Determine the CRITical Dividing Streamline---   CALL CRITDS
               CALL CRITDS (HESETL)

C              Calculate the fraction of plume below
C              HCRIT, PHEE                               ---   CALL PFRACT
               CALL PFRACT (HESETL)

C              Calculate FOPT = f(PHEE)                  ---   CALL FTERM
               CALL FTERM

C              Calculate AERMOD Concentration            ---   CALL AER_PCHI
               CALL AER_PCHI( XARG, ADJ, VDEP(J), J, AERTMP(:) )
               AEROUT(:) = AEROUT(:) + AERTMP(:)

            END DO
C           End loop over particle sizes

         END IF

      END IF

      RETURN
      END

      SUBROUTINE PRMCALC (XBREC, YBREC)
C***********************************************************************
C             PRMCALC Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Calculates the PRIME downwash component of the
C                 concentration
C
C        PROGRAMMER: Roger Brode, PES, Inc.
C
C        DATE:     November 10, 2000
C
C        MODIFIED:
C                  Modified to place receptor on centerline of cavity
C                  plumes by setting Y2 = 0.0 for SCREEN option.
C                  R. W. Brode, MACTEC (f/k/a PES), Inc., 10/26/04
C
C                  AWMA PRIME2 Subcommittee
C                  - Changed definition of ZLO, ZHI
C                  - Added provisions for U30, SV30, SW30 for use
C                    in wake_u_turb.
C                  - USE module cpp_wakedat to transfer values
C
C        INPUTS:  XBREC - Real    - Downwind distance (m) of receptor
C                                   from upwind edge of building
C                 YBREC - Real    - Lateral distance (m) of receptor from
C                                   center of upwind edge of building
C
C        OUTPUTS: PRMVAL(NTYP) - Real - PRIME downwash component of
C                                       concentration
C
C        CALLED FROM:   PCALC
C
C***********************************************************************
C     Variable Declarations
      USE MAIN1

C     The following are for AWMA ALPHA option
      use PRIME_WAKEDAT, only: hb
      use PRM2_wakedat, only:  Zeff_PRM2, u30, sv30, sw30,
     &                         Ueff_save, SWeff_save, SVeff_save

      IMPLICIT NONE

      CHARACTER MODNAM*12
      INTEGER :: IPOSITN, N1, N2, IS, J
      DOUBLE PRECISION :: ADJ
      DOUBLE PRECISION :: DHPOUT, SYOUT, SZOUT, FYOUT
      DOUBLE PRECISION :: XBREC, YBREC, FQCAV, SYCAV,
     &                    SZCAV
      DOUBLE PRECISION :: ZHI, ZLO

C     The following two lines are for AWMA ALPHA option
      DOUBLE PRECISION :: BetaJ
      double precision :: z30hi, z30lo

      INTEGER :: NDXBHI, NDXBLO, NDXALO
c --- Declare local PRIME arrays for "3-source" data
      DOUBLE PRECISION :: q2(3),y2(3),sy2(3),z2(3),h2(3),sz2(3),qc2(3),
     &                    qtksav,ppfsav

      logical :: L_INWAKE

C     Variable Initializations
      MODNAM = 'PRMCALC'
      PRMVAL = 0.0D0
      Ueff_save  = 0.0D0
      SWeff_save = 0.0D0
      SVeff_save = 0.0D0

c --- PRIME ---------------------------------------------------------
c --- Calculate where receptor is relative to near-wake cavity
c     and building (IPOSITN=1 for within bldg; 2=within
c     near-wake, 3=within far wake; 4=outside)
c --- Note:  xbrec is downwind dist. of receptor from upwind
c     bldg face; ybrec is crosswind dist. of receptor from
c     center of upwind bldg. face                  ---  CALL POSITION
      call POSITION(xbrec,ybrec,zflag,ipositn)

      if(ipositn.EQ.4 .AND. X.LE.0.0D0) then
c ---    Receptor is upwind of sources and is not within
c ---    a building wake - use AERMOD calculation
c ---    Set PRMVAL(NUMTYP) array = AERVAL(NUMTYP) array
         PRMVAL = AERVAL

      elseif(ipositn.NE.2 .AND. distr .LT. 0.99D0) then
c ---    Receptor Too Close to Source for Calculation and is not
c ---    within a building near-wake (cavity) - use AERMOD calculation
c ---    Set PRMVAL(NUMTYP) array = AERVAL(NUMTYP) array
         PRMVAL = AERVAL
c -------------------------------------------------------------

      ELSE IF (.NOT. WAKE) THEN
C ---    No wake effects for this source for this hour - use AERMOD calculation
c ---    Set PRMVAL(NUMTYP) array = AERVAL(NUMTYP) array
         PRMVAL = AERVAL

      ELSE
C ---    Calculate PRIME concentration with downwash

C ---    Calculate effective parameters to define ambient turbulence intensities,
C        as averages across layer from ground to top of wake (as calculated at
C        a downwind distance of 15R).
         ZHI = 1.2D0*RSCALE * (15.0D0 +
     &                  (DSBH/(1.2D0*RSCALE))**3)**THIRD
         IF (UNSTAB) THEN
            ZHI = MIN( ZHI, ZI )
         END IF
         ZLO = 0.0D0

C --- AWMA version D20350
         IF (L_AWMA_UTurb .OR. L_AWMA_UTurbHX) THEN
C---        The values of ZHI and ZLO in the regulatory version of
C           AERMOD are replaced with the values below for AWMADWNW option
            BetaJ = 1.0D0/3.0D0 + US/VS
            Zeff_PRM2 = 0.9D0 * (FM * US/USTAR)**0.5D0 / (US * BetaJ)
            Zeff_PRM2 = Zeff_PRM2 + HS
            Zeff_PRM2 =  max(Zeff_PRM2,HB,5.0D0)

            ZHI = Zeff_PRM2 + 5.0D0
            ZLO = Zeff_PRM2 - 5.0D0
         ENDIF

C ---    Compute average values between ZLO and ZHI
         CALL LOCATE(GRIDHT, 1, MXGLVL, ZHI, NDXBHI)
         CALL LOCATE(GRIDHT, 1, MXGLVL, ZLO, NDXBLO)

         NDXALO = NDXBLO + 1
         CALL ANYAVG ( MXGLVL, GRIDHT, GRIDWS, ZLO,NDXALO,
     &      ZHI,NDXBHI,UEFF )

         CALL ANYAVG ( MXGLVL, GRIDHT, GRIDSV, ZLO,NDXALO,
     &      ZHI,NDXBHI,SVEFF )

         CALL ANYAVG ( MXGLVL, GRIDHT, GRIDSW, ZLO,NDXALO,
     &      ZHI,NDXBHI,SWEFF )

         CALL ANYAVG ( MXGLVL, GRIDHT, GRIDTG, ZLO,NDXALO,
     &      ZHI, NDXBHI, TGEFF )

C ---    AWMA version D20350
C        If the AWMA enhanced downwash options are applied,
C          save the effective parameters so they can be restored later
C
         IF (L_AWMA_UTurbHX) THEN
            Ueff_save  = Ueff
            SWeff_save = SWeff
            SVeff_save = SVeff
         END IF

C ---    AWMA version D20350
         IF (L_AWMA_UTurb  .OR. L_AWMA_UTurbHX) THEN
C---        Additional calculations for AWMADW -
C              "effective" parameters (at 30m) for use in wake_u_turb
C           Note: alternate algorithm to extract 30m value directly may
C                 be preferred, but for now using method parallel to above.
C                 Requires averaging the values over a narrow range
C                 centered on 30m. Does this reliably work?
            z30lo = 28.d0
            z30hi = 32.d0
            CALL LOCATE(GRIDHT, 1, MXGLVL, Z30lo, NDXBLO)
            CALL LOCATE(GRIDHT, 1, MXGLVL, z30hi, NDXBHI)

C           Define index above based on index below to obtain correct
C           values from subr.anyavg
            NDXALO = NDXBLO + 1

            call anyavg (mxglvl, gridht, gridws, z30lo, ndxalo,
     &                   z30hi, ndxbhi, u30)
                                       !---
            call anyavg (mxglvl, gridht, gridsv, z30lo, ndxalo,
     &                   z30hi, ndxbhi, sv30)
                                       !----
            call anyavg (mxglvl, gridht, gridsw, z30lo, ndxalo,
     &                    z30hi, ndxbhi, sw30)
                                        !----
         END IF

CRWB     Modify treatment of low wind/low turbulence cases.
CRWB     R. Brode, PES, 8/15/96

         SWEFF = MAX( SWEFF, SWMIN )

         SVEFF = MAX( SVEFF, SVMIN, SVUMIN*UEFF )

         IF( L_VECTORWS )THEN
            UEFF  = DSQRT( UEFF*UEFF + 2.0D0*SVEFF*SVEFF )
         ENDIF

         UEFF  = MAX( UEFF, WSMIN )

C ---    AWMA version D20350
         IF (L_AWMA_UTurb .OR. L_AWMA_UTurbHX )THEN
            sw30 =  max (sw30, swmin)
            sv30 =  max (sv30, svmin, svumin*u30)
            IF (L_VECTORWS) u30 = dsqrt (u30*u30 +2.d0*sv30*sv30)
            u30 = MAX (u30, wsmin)
         END IF

         if (PRIMEDBG) then
            write(PRMDBUNT,*) 'PRIME Effective Parameters: '
            if (L_AWMA_UTurb . or. L_AWMA_UTurbHX) then
               write(PRMDBUNT,*) '  AWMAUturb/HX applied to eff params'
            end if
            write(PRMDBUNT,*)   '  ZLO, ZHI     = ', zlo, zhi
            write(PRMDBUNT,*)   '  SWEFF, SVEFF = ', sweff, sveff
            write(PRMDBUNT,*)   '  UEFF,  TGEFF = ', ueff, tgeff
         end if

C        Calculate the plume rise                     ---   CALL PRMDELH
         CALL PRMDELH ( X, L_INWAKE )

C ---    Check for runtime error (RUNERR) before continuing
         IF (RUNERR) RETURN

         IF (.NOT. L_INWAKE) THEN
C           Plume is not affected by wake, set PRMVAL = AERVAL and return
            PRMVAL = AERVAL
            RETURN
         END IF

C        Determine Effective Plume Height             ---   CALL PRMHEFF
         CALL PRMHEFF

         IF (UNSTAB .AND. HE .GE. ZI) THEN
C           Plume is above ZI, set PRMVAL = AERVAL and return
            PRMVAL = AERVAL
            RETURN
         END IF

c ---    Calculate sigmas
         dhpout = dhp
         call WAKE_XSIG(x,hs,dhpout,nobid,szout,syout,
     &                  szcav,sycav)
         sy = syout
         sz = szout

c ---    PRIME ---------------------------------------------------
c ---    When there is a building wake, consider treatment of mass in
c ---    cavity as additional sources, or as only source
         qtksav = qtk
         ppfsav = ppf
c ---    Place selected plume data into transfer arrays (first element)
         q2(1)  = qtk
         y2(1)  = y
         sy2(1) = sy
         z2(1)  = zflag
         h2(1)  = he
         sz2(1) = sz
         n1 = 1
         n2 = 1
         if(WAKE) then
c ---       Define cavity source                              ---   CALL CAV_SRC
            call CAV_SRC(x,y,zflag,fqcav,qc2,h2,y2,z2,sz2,sy2,n1,n2)
            if (SCREEN) then
c ---          Force receptor to be on "centerline" for all plumes for SCREEN
               y2 = 0.0D0
            end if
            if(fqcav.GT.0.0D0) then
c ---          Set source strengths
               q2(1)=qtk*(1.0D0-fqcav)
               q2(2)=qtk*fqcav*qc2(2)
               q2(3)=qtk*fqcav*qc2(3)
            end if
         end if

c ---    Initialize PRMVAL(NUMTYP) output array values to zero, because contributions
c ---    due to more than one source are summed here (or do loop may
c ---    not execute if neither source contributes)
         PRMVAL = 0.0D0

c ---    Loop over 3 possible sources (is=1 for primary source,
c ---    is=2 for "outside" cavity source, and is=3 for "inside" cavity source)
c         IF (AWMADWDBG) THEN
c            WRITE(AWMADWDBUNT,*) '-----------------------------'
c            WRITE(AWMADWDBUNT,*) ' In PRMCALC: Src ', SRCID(ISRC),
c     &                           'Recep: ',IREC
c         END IF

         do is = n1, n2
c         IF (AWMADWDBG) THEN
c            WRITE(AWMADWDBUNT,*) ' ---> Processing source "is" ',is
c            WRITE(AWMADWDBUNT,*)'      (1=primary, 2=outside cav,',
c     &                                        ' 3=inside cav)'
c         END IF
c ---       Cycle to next source if emission rate is 0.0
            if (q2(is) .eq. 0.0D0) cycle

c ---       Transfer data for current source
            qtk = q2(is)
            y   = y2(is)
            sy  = sy2(is)
            sz  = sz2(is)
            he  = h2(is)
            zflag = z2(is)

c -------------------------------------------------------------
C           Calculate the 'y-term' contribution to
C           dispersion, FSUBY                              ---   CALL FYPLM
            CALL FYPLM(SY,FYOUT)
            FSUBY  = FYOUT

            IF( FSUBY.EQ.0.0D0 )THEN
C ---          Lateral term is 0.0, set PRMVAL array (1:NUMTYP) to 0.0.
               PRMVAL = 0.0D0

            ELSE

C ---          Set FOPT = 0.5 for PRIME calculation since wake is "near neutral"
               FOPT = 0.5D0

               IF (NPD .EQ. 0) THEN
C                 Determine Deposition Correction Factors
                  IF ((LDGAS.OR.LWGAS) .AND. IS.NE.3 .AND.
     &                                       X.GT.1.0D0) THEN
C                    Do not apply depletion for "inside cavity source", IS=3
                     CALL PRM_PDEPG (X)

C                    Reassign plume height and sigmas, which may have changed
C                    during integration
                     sy  = sy2(is)
                     sz  = sz2(is)
                     he  = h2(is)
                  ELSE
                     DQCORG = 1.0D0
                     WQCORG = 1.0D0
                  END IF

                  ADJ = DQCORG * WQCORG
                  CALL PRM_PCHI( ADJ, VDEPG, 0, is )                    ! ORD (EMM) change

               ELSE
                  IF ((LDPART.OR.LWPART) .AND. IS.NE.3 .AND.
     &                                                 X.GT.1.0D0) THEN
C                    Do not apply depletion for "inside cavity source", IS=3
                     CALL PRM_PDEP (X)

C                    Reassign plume height and sigmas, which may have changed
C                    during integration
                     sy  = sy2(is)
                     sz  = sz2(is)
                     he  = h2(is)
                  ELSE
C                    Set DQCOR(NPD) and WQCOR(NPD) arrays to 1.0
                     DQCOR = 1.0D0
                     WQCOR = 1.0D0
                  END IF

                  DO J = 1, NPD

                     ADJ = PHI(J) * DQCOR(J) * WQCOR(J)
                     HV  = (X/US) * VGRAV(J)
                     HE  = MAX ( 0.0D0, HE - HV )

                     CALL PRM_PCHI( ADJ, VDEP(J), J, is )               ! ORD (EMM) change

                  END DO
               END IF

            END IF

         END DO

c ---    Restore original plume data
         QTK = QTKSAV
         PPF = PPFSAV
         y   = y2(1)
         sy  = sy2(1)
         sz  = sz2(1)
         he  = h2(1)
         zflag = z2(1)

      END IF

      RETURN
      END


      SUBROUTINE GAMCALC ( XARG, YARG )
C***********************************************************************
C             GAMCALC Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Calculates the Gamma weighting factor to combine
C                 AERMOD and PRIME concentrations
C
C        PROGRAMMER: Roger Brode, PES, Inc.
C
C        DATE:     July 19, 2001
C
C        INPUTS:   XARG - Real - Downwind distance (m) of receptor
C                                from upwind edge of building
C                  YARG - Real - Lateral distance (m) of receptor from
C                                center of upwind edge of building
C
C        OUTPUTS:  GAMFACT - Real - Gamma weighting factor to combine
C                                   AERMOD and PRIME concentrations
C
C        CALLED FROM:   PCALC
C
C***********************************************************************
C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      DOUBLE PRECISION :: XARG, YARG
      DOUBLE PRECISION :: WAKE_LEN, WAKE_WID, WAKE_HGT
      DOUBLE PRECISION :: XAY, XAZ, XAMX
      DOUBLE PRECISION :: SIGMA_XG,  SIGMA_YG,  SIGMA_ZG,
     &                    EXPARG_XG, EXPARG_YG, EXPARG_ZG

C --- Variable Initializations
      MODNAM = 'GAMCALC'

      IF (XARG .LE. 0.0D0 .OR. .NOT.WAKE) THEN
C ---    Receptor is upwind of building or no WAKE, set GAMFACT = 0.0 to
C ---    use AERVAL only.
         GAMFACT = 0.0D0

      ELSE

C ---    Calculate the height, half-width and "length" of the wake.
C ---    Length of wake is measured from upwind edge of building.
         WAKE_HGT = 1.2D0*RSCALE * (XARG/RSCALE +
     &              (DSBH/(1.2D0*RSCALE))**3)**THIRD
         WAKE_WID = 0.5D0*DSBW + (RSCALE/3.0D0)*
     &                           (XARG/RSCALE)**THIRD
C ---    Obtain distance to transition from wake to ambient turbulence,
C ---    without cap at 15R.
         call WAKE_XA2(DSBL,RSCALE,xaz,xay)
         XAMX = MAX(XAZ,XAY)
C ---    Set WAKE_LEN as maximum of 15R and transition distance
         WAKE_LEN = MAX(15.0D0 * RSCALE, XAMX)

C ---    Assign wake dimensions to SIGMA_?G terms
         SIGMA_XG = WAKE_LEN
         SIGMA_YG = WAKE_WID
         SIGMA_ZG = WAKE_HGT

C ---    Calculate exponential argument for alongwind dimension
         IF (XARG .LE. SIGMA_XG) THEN
            EXPARG_XG = 0.0D0
         ELSE
            EXPARG_XG = -((XARG-SIGMA_XG)**2 / (2.0D0 * SIGMA_XG**2))
         END IF

C ---    Calculate exponential argument for crosswind dimension
         IF (DABS(YARG) .LE. SIGMA_YG) THEN
            EXPARG_YG = 0.0D0
         ELSE
            EXPARG_YG = -((DABS(YARG)-SIGMA_YG)**2 /
     &                      (2.0D0 * SIGMA_YG**2))
         END IF

C ---    Calculate exponential argument for vertical dimension, using ZRT,
C ---    height of receptor above stack base, including terrain and flagpole.
         IF (ZRT .LE. SIGMA_ZG) THEN
            EXPARG_ZG = 0.0D0
         ELSE
            EXPARG_ZG = -((ZRT-SIGMA_ZG)**2 / (2.0D0 * SIGMA_ZG**2))
         END IF

C        Calculate gamma weighting factor, GAMFACT
         IF (EXPARG_XG.GT.EXPLIM .AND. EXPARG_YG.GT.EXPLIM .AND.
     &                                 EXPARG_ZG.GT.EXPLIM) THEN
            GAMFACT = DEXP(EXPARG_XG)*DEXP(EXPARG_YG)*DEXP(EXPARG_ZG)
         ELSE
            GAMFACT = 0.0D0
         END IF

      END IF

      RETURN
      END


      SUBROUTINE CENTROID (XARG)
C***********************************************************************
C             CENTROID Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Calculates the plume centroid height, and sets the
C                 SURFAC logical variable
C
C        PROGRAMMER: Roger Brode, PES, Inc.
C
C        DATE:     November 10, 2000
C
C        INPUTS:  Downwind distance, XARG (m)
C
C        OUTPUTS: Plume centroid height, CENTER
C                 Surface logical variable, SURFAC
C
C        CALLED FROM:   PCALC, VCALC, ACALC, PLUMEF
C
C***********************************************************************
C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      DOUBLE PRECISION :: XARG, DELX, DELZ, FRAC, xtmp

C     Variable Initializations
      MODNAM = 'CENTROID'

      xtmp = max( xarg, 1.0D0 )

      IF( UNSTAB  .AND.  (HS .LT. ZI) ) THEN
C----    Calculate plume centerline height without PDF adjustments
         IF (SRCTYP(ISRC)(1:5) .EQ. 'POINT') THEN
            CALL CBLPRD ( xtmp )
            HTEFF = MIN (HSP+DHP1, ZI)
         ELSE
            HTEFF = HSP
C ---       Assign DHCRIT = 0.0  and XFINAL = 0.0 for non-POINT sources
            DHCRIT = 0.0D0
            XFINAL = 0.0D0
         END IF
         IF( xtmp .LT. XFINAL) THEN
            CENTER = HTEFF
         ELSE IF( xtmp .GE. XMIXED) THEN
            CENTER = ZMIDMX
         ELSE
            DELX = XMIXED - XFINAL
            DELZ = ZMIDMX - MIN( HSP+DHCRIT, ZI)
            FRAC = (xtmp-XFINAL)/DELX
            CENTER = MIN(HSP+DHCRIT,ZI) + FRAC * DELZ
         END IF

C----    Determine if this is a surface layer release
         IF( CENTER .LT. 0.1D0*ZI )THEN
            SURFAC = .TRUE.
         ELSE
            SURFAC = .FALSE.
         END IF

      ELSE IF( UNSTAB .AND. (HS .GE. ZI) )THEN
         SURFAC = .FALSE.
         CENTER = HSP
C ----   Account for distance-dependent CENTER and SURFAC
         IF( CENTER .LT. 0.1D0*ZI )THEN
            SURFAC = .TRUE.
         ELSE
            SURFAC = .FALSE.
         END IF
      ELSE
C----    Assign centroid height to release height
         CENTER = HSP
C ----   Account for distance-dependent CENTER and SURFAC
         IF( CENTER .LT. 0.1D0*ZI )THEN
            SURFAC = .TRUE.
         ELSE
            SURFAC = .FALSE.
         END IF

      END IF

      RETURN
      END


      SUBROUTINE REFL_HT (HEARG, XARG, SZBARG, VSIGZARG, HEREFL)
C***********************************************************************
C             REFL_HT Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Calculates height of the "effective reflecting surface"
C                 for stable plumes, including penetrated source for
C                 point sources.
C
C        PROGRAMMER: Roger Brode, PES, Inc.
C
C        DATE:     November 10, 2000
C
C        INPUTS:  Height of plume (m),                HEARG
C                 Downwind distance (m),              XARG
C                 Bouyancy induced dispersion (m),    SZBARG
C                 Virtual source dispersion term (m), VSIGZARG
C
C        OUTPUTS: Effective height of the reflecting surface (m), HEREFL
C
C        CALLED FROM:   PCALC, VCALC, ACALC, PLUMEF
C
C***********************************************************************
C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      INTEGER :: NDXHE
      DOUBLE PRECISION :: HEARG, HEREFL, XARG, SZBARG, VSIGZARG,
     &           SVREFL, SWREFL, UREFL, TGREFL, PTREFL,
     &           TTRAVL, BVFRQ, ZTMP, SZREFL, SZRF, SIGF, SZHSBL
C Unused:       DOUBLE PRECISION :: SVREFL2

C     Variable Initializations
      MODNAM = 'REFL_HT'

C---- Compute the height of the "effective reflecting surface"
C---- for stable plumes, HEREFL
C---- First locate index below HEARG
      CALL LOCATE(GRIDHT, 1, MXGLVL, HEARG, NDXHE)

      IF (NDXHE .GE. 1) THEN

C---- Sigma_V at HEARG
         CALL GINTRP( GRIDHT(NDXHE), GRIDSV(NDXHE),
     &                GRIDHT(NDXHE+1), GRIDSV(NDXHE+1),
     &                HEARG, SVREFL )

C---- Sigma_W at HEARG
         CALL GINTRP( GRIDHT(NDXHE), GRIDSW(NDXHE),
     &                GRIDHT(NDXHE+1), GRIDSW(NDXHE+1),
     &                HEARG, SWREFL )

C---- Wind speed at HEARG
         CALL GINTRP( GRIDHT(NDXHE), GRIDWS(NDXHE),
     &                GRIDHT(NDXHE+1), GRIDWS(NDXHE+1),
     &                HEARG, UREFL )

C---- Temperature gradient at HEARG
         CALL GINTRP( GRIDHT(NDXHE), GRIDTG(NDXHE),
     &                GRIDHT(NDXHE+1), GRIDTG(NDXHE+1),
     &                HEARG, TGREFL )

C---- Potential temperature at HEARG
         CALL GINTRP( GRIDHT(NDXHE), GRIDPT(NDXHE),
     &                GRIDHT(NDXHE+1), GRIDPT(NDXHE+1),
     &                HEARG, PTREFL )

      ELSE
         SVREFL = GRIDSV(1)
         SWREFL = GRIDSW(1)
         UREFL  = GRIDWS(1)
         TGREFL = GRIDTG(1)
         PTREFL = GRIDPT(1)
      END IF

C---- Apply minimum wind speed and turbulence checks to values
C     at HEARG
C
      SWREFL = MAX( SWREFL, SWMIN )
      SVREFL = MAX( SVREFL, SVMIN, SVUMIN*UREFL )
      IF( L_VECTORWS )THEN
         UREFL  = DSQRT( UREFL*UREFL + 2.0D0*SVREFL*SVREFL )
      ENDIF
      UREFL  = MAX( UREFL, WSMIN )

C     Compute surface sigma-z term for stable conditions
      IF (STABLE) THEN
         SZSURF = (RTOF2/RTOFPI) * USTAR * (XARG/UREFL) *
     &            (1.0D0 + 0.7D0*XARG/OBULEN)**(-1.0D0*THIRD)
      ELSE
         SZSURF = 0.0D0
      END IF

C     Compute ambient sigma-z term at HEARG
      TTRAVL = XARG / UREFL
C---- Apply Sigma-Z formulation from CTDMPLUS

      BVFRQ = DSQRT( G * TGREFL / PTREFL )
      IF(BVFRQ .LT. 1.0D-10) BVFRQ = 1.0D-10

C     Set height for sigma-z calculation, ZTMP
      ZTMP = MAX( HS, HEARG, 1.0D-4 )
      SZREFL = SWREFL * TTRAVL /
     &  DSQRT( 1.0D0 + SWREFL*TTRAVL * ( 1.0D0/(0.72D0*ZTMP) +
     &  BVFRQ/(0.54D0*SWREFL) ) )

      IF (HEARG .GE. ZI) THEN
         SZRF = SZREFL
      ELSE
         SIGF = MIN( HEARG/ZI, 1.0D0 )
         SZRF = (1.0D0 - SIGF) * SZSURF + SIGF * SZREFL
      END IF

C     Calculate sigma-z at plume height, SZHSBL
      SZHSBL = DSQRT( SZBARG*SZBARG + SZRF*SZRF + VSIGZARG*VSIGZARG)

C     Compute height of effective reflecting surface, HEREFL
      HEREFL = MAX( ZI, HEARG + 2.15D0*SZHSBL )

      RETURN
      END

      SUBROUTINE PSRDEB
C***********************************************************************
C             PSRDEB Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Outputs point source information for debugging
C                 purposes
C
C        PROGRAMMER: Bob Paine.  Implemented by Russ Lee.
C
C        DATE:    August 18, 1994
C
C        INPUTS:  Source Parameters for Specific Source, including
C                 those calculated in PCALC
c
CRJP              DHCRIT = "Final" plume rise (m)
C
C        OUTPUTS: Debugging information for a specific source.
C
C        CALLED FROM:   CALC
C
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'PSRDEB'

      WRITE (DBGUNT, 6130) KURDAT

      WRITE (DBGUNT, 6135) ISRC, QS, TS, VS, DS, FB, FM, HS, DHCRIT
      WRITE (DBGUNT, 6140) HS, WDIR, US, UP, SVS, SWS, TGS

 6130 FORMAT(20('===='),//,' YR/MN/DY/HR:  ', I8,
     &  //,8X,'<----------------- SOURCE INFORMATION ---------',
     1  '------> FINAL PLUME',/,
     2  ' SOURCE   QS    TS      VS     DS   BUOY FLUX  MOM FLUX  ',
     3  ' HS      RISE',/,
     4  '   #    (G/S)   (K)    (M/S)   (M)   (M4/S3)    (M4/S2)  ',
     5  ' (M)      (M)',/)
 6135 FORMAT(I4,F9.1,F7.1,F7.2,F7.2,F10.1,2X,F9.1,F6.1,F9.1,
     1  //,2X,'VARIABLES AT ',T21,'HEIGHT   WDIR   ',
     2  'USCAL  URISE  SIGV   SIGW   DTHDZ ',
     3  /,2X,'STACK HEIGHT:',T21,
     3  '  (M)    (DEG)  (M/S)  (M/S)  (M/S)  (M/S) (DEG/M)',/)
 6140 FORMAT(19X,F7.1,F7.0,F8.2,2F7.2,F7.2,F8.4,/)

      RETURN
      END

      SUBROUTINE VCALC
C***********************************************************************
C        VCALC Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Calculates concentration or deposition values
C                 for VOLUME sources
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:
C                  Modified to calculate conc*travel time for
C                  GRSM NO2 option.
C                  CERC, 11/30/20
C
C                  Added arrays to save WDSIN and WDCOS by source for use
C                  with PVMRM option.  Corrects potential problem for
C                  PVMRM applications with multi-level wind inputs.
C                  R.W. Brode, U.S. EPA/OAQPS/AQMG, 01/26/2007
C
C                  Added call to subroutine HEFF for calculation
C                  of ZSUBP for deposition applications.
C                  R. W. Brode, U.S. EPA, OAQPS, AQMG, 01/24/2007
C
C                  Modified to include initialization of __VAL arrays
C                  at end of receptor loop.
C                  R. W. Brode, MACTEC (f/k/a PES), Inc., 10/26/04
C
C                  Modified to include the PVMRM and OLM options for
C                  modeling conversion of NOx to NO2.
C                  Added debug statement based on ENSR code.
C                  R. W. Brode, MACTEC (f/k/a PES), Inc., 07/27/04
C                  To assign values to XDIST before calls to
C                  SUBROUTINE VOLCALC.
C                  R. W. Brode, MACTEC (f/k/a PES), Inc., 03/19/04
C
C        INPUTS:  Source Parameters for Specific Source
C                 Arrays of Receptor Locations
C                 Meteorological Variables for One Hour
C
C        OUTPUTS: 1-hr CONC or DEPOS Values for Each Receptor for
C                 Particular Source
C
C        CALLED FROM:   CALC
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      INTEGER :: I
      DOUBLE PRECISION :: AERPLM(NUMTYP), AERPAN(NUMTYP), FRAN
      LOGICAL :: L_PLUME

C     Variable Initializations
      MODNAM = 'VCALC'
      WAKE = .FALSE.

C     Initialize __VAL arrays (1:NUMTYP)
      HRVAL   = 0.0D0
      AERVAL  = 0.0D0
      AERPLM  = 0.0D0
      AERPAN  = 0.0D0
      PRMVAL  = 0.0D0
      FRAN    = 0.0D0

      IF( ALLOCATED(CHI) ) CHI(:,ISRC,:) = 0.0D0

      IF(GRSM)THEN
        CHI_TTRAVPLM = 0.0D0
        CHI_TTRAVPAN = 0.0D0
        CHI_TTRAVAER = 0.0D0
        CHI_TTRAVCHM(:,ISRC) = 0.0D0
      END IF

C     Set the Source Variables for This Source              ---   CALL SETSRC
      CALL SETSRC

C     Apply Variable Emission Rate Factors                  ---   CALL EMFACT
      CALL EMFACT(QS)

C     Initialize 'ARC' Arrays for EVALFILE Output           ---   CALL EVLINI
      IF (EVAL(ISRC)) THEN
         CALL EVLINI
      END IF

      IF (QTK .NE. 0.0D0) THEN

C        Set Mixing Height and Profiles for Urban Option if Needed
         IF (URBSRC(ISRC) .EQ. 'Y') THEN
C           Find Urban Area Index for This Source
            DO I = 1, NUMURB
               IF (IURBGRP(ISRC,I) .EQ. 1) THEN
                  IURB = I
                  EXIT
               END IF
            END DO
            IF (STABLE .OR. L_MorningTrans(IURB)) THEN
               URBSTAB = .TRUE.
               ZI = MAX( ZIURB(IURB), ZIMECH )
               GRIDSV = GRDSVU(1:MXGLVL,IURB)
               GRIDSW = GRDSWU(1:MXGLVL,IURB)
               GRIDTG = GRDTGU(1:MXGLVL,IURB)
               GRIDPT = GRDPTU(1:MXGLVL,IURB)
               OBULEN = URBOBULEN(IURB)
               USTAR  = URBUSTR(IURB)
            ELSE
               URBSTAB = .FALSE.
               ZI = ZIRUR
               GRIDSV = GRDSVR
               GRIDSW = GRDSWR
               GRIDTG = GRDTGR
               GRIDPT = GRDPTR
               OBULEN = RUROBULEN
               USTAR  = RURUSTR
            END IF
         ELSE IF (URBAN .AND. URBSRC(ISRC) .EQ. 'N') THEN
            URBSTAB = .FALSE.
            ZI = ZIRUR
            GRIDSV = GRDSVR
            GRIDSW = GRDSWR
            GRIDTG = GRDTGR
            GRIDPT = GRDPTR
            OBULEN = RUROBULEN
            USTAR  = RURUSTR
         ELSE
C ---       Rural
            URBSTAB = .FALSE.
         END IF

C        Initialize meteorological variables                ---   CALL METINI
         CALL METINI

C        Save WDSIN and WSCOS for later use by PVMRM/GRSM option
         AWDSIN(ISRC) = WDSIN
         AWDCOS(ISRC) = WDCOS
         AAFV(ISRC)   = AFV

C        Initialize miscellaneous variables
         FB  = 0.0D0
         FM  = 0.0D0
         PPF = 0.0D0
         HSP = HS
         DHP  = 0.0D0
         DHP1 = 0.0D0
         DHP2 = 0.0D0
         DHP3 = 0.0D0
         DHCRIT = 0.0D0
         XFINAL = 0.0D0
         XMIXED = ZI * UAVG / SWAVG
         IF(XMIXED .LT. XFINAL) XMIXED = XFINAL
         ZMIDMX = 0.5D0 * ZI

C        Calculate Effective Radius
         XRAD = 2.15D0*SYINIT

C ---    Initialize PDF parameters for use in calculating ZSUBP
         IF( UNSTAB  .AND.  (HS .LT. ZI) ) THEN
            CALL PDF
         END IF
C        Set Dry Deposition Variables for this Source
         IF (LUSERVD .AND. LDGAS .AND. NPD.EQ.0) THEN
C           Assign user-specified gas dry deposition velocity (GASDEPVD option)
            VDEPG = USERVD
         ELSE IF (LDPART .OR. (.NOT.LUSERVD .AND. LDGAS .AND.
     &                                                   NPD.EQ.0)) THEN
C           Calculate Deposition Velocities for this Source    ---   CALL VDP
            CALL VDP
         END IF
         IF (LWPART .OR. LWGAS) THEN
CPES        Set value of ZSUBP = MAX( ZI, TOP OF PLUME ), where
CPES        TOP OF PLUME is defined as plume height (HE) plus 2.15*SZ,
CPES        evaluated at a distance of 20 kilometers downwind.
CPES        Apply minimum value of 500m and maximum value of 10,000m.
            CALL HEFF (20000.0D0)
            IF( STABLE .OR. (UNSTAB .AND. HS .GE. ZI) )THEN
               CALL SIGZ(20000.0D0)
               ZSUBP = MAX( 500.0D0, ZI, HS + SZCOEF*SZAS )
            ELSE IF (UNSTAB) THEN
               CALL SIGZ(20000.0D0)
               ZSUBP = MAX( 500.0D0, ZI, HS +
     &                                   SZCOEF*(SZAD1+SZAD2)/2.0D0 )
            END IF
            ZSUBP = MIN( 10000.0D0, ZSUBP )
C           Calculate Scavenging Ratios for this Source           ---   CALL SCAVRAT
            CALL SCAVRAT
         END IF

C        Begin Receptor LOOP
         RECEPTOR_LOOP: DO IREC = 1, NUMREC
C           Calculate Down and Crosswind Distances          ---   CALL XYDIST
            IF (EVONLY) THEN
               CALL XYDIST(IEVENT)
            ELSE
               CALL XYDIST(IREC)
            END IF

C ---       First check for receptor exceeding minimum distance (1m) or
C           maximum distance (80km for FASTALL or 1.0D20 otherwise).
            IF( DISTR.LT.(XRAD+0.99D0) .OR. (DISTR.GT.MAXDIST) )THEN
C              Receptor too close to source for calculation or
C              receptor is beyond the maximum distance from source.
               HRVAL(:)  = 0.0D0
               AERPLM(:) = 0.0D0
               AERPAN(:) = 0.0D0
               IF (PVMRM .OR. OLM .OR. ARM2
     &            .OR. RUNTTRM .OR. GRSM) THEN
C ---             Assign 0.0 to CHI(IREC,ISRC,ITYP) before
C                 cycling to the next receptor to avoid
C                 persisting value from previous hour.
                  CHI(IREC,ISRC,:) = 0.0D0
                  IF(GRSM)THEN
                    CHI_TTRAVPLM = 0.0D0
                    CHI_TTRAVPAN = 0.0D0
                    CHI_TTRAVAER = 0.0D0
                    CHI_TTRAVCHM(IREC,ISRC) = 0.0D0
                  END IF
                  IF (PVMRM .OR. GRSM) THEN
                     HECNTR(IREC,ISRC)  = 0.0D0
                     UEFFS(IREC,ISRC)   = 0.0D0
                     EPSEF(IREC,ISRC)   = 0.0D0
                     PPFACT(IREC,ISRC)  = 0.0D0
                     HECNTR3(IREC,ISRC) = 0.0D0
                     UEFF3S(IREC,ISRC)  = 0.0D0
                     EPSEF3(IREC,ISRC)  = 0.0D0
                     FOPTS(IREC,ISRC)   = 0.0D0
                  END IF
               END IF
C ---          Cycle to next receptor
               CYCLE RECEPTOR_LOOP
            END IF

C ---       Calculate coherent plume component using downwind distance, X
            L_PLUME = .TRUE.

C ---       Assign XDIST for use in dry depletion (FUNCTION F2INT)
            XDIST = X

C ---       Start with coherent plume component, but check for X < 1m
            IF( X .LT. 1.0D0 )THEN
C              Receptor is upwind of source; set AERPLM = 0.0 and skip
C              the call to VOLCALC
               AERPLM = 0.0D0
C              CERC 11/30/20 Also set conc*travel time to zero
               IF(GRSM)THEN
                 CHI_TTRAVPLM=0.0D0
               END IF
            ELSE
C ---          Receptor is downwind of source; call VOLCALC to get AERPLM
               CALL VOLCALC( X, L_PLUME, AERPLM )

C ---          CERC 11/30/20 Calculate the conc*travel time for the Gaussian plume
               IF(GRSM)THEN
                 IF (STABLE .OR. (UNSTAB.AND.(HS.GE.ZI))) THEN
                   UCHM=UEFF
                 ELSE
                   UCHM=UEFFD
                 END IF
                 IF (UCHM/=0) THEN
                   CHI_TTRAVPLM=MAX(0.0D0,AERPLM(1)*X/UCHM) !Don't allow negative conc*travel times
                 ELSE
                   !Error - no travel time if UEFF is zero
                   CALL ERRHDL(PATH, MODNAM, 'E','601','GRSM')
                   RUNERR=.TRUE.
                   RETURN
                 END IF
               END IF
            END IF

C ---       Next determine "random/pancake" component of plume
            IF (L_EFFSIGY) THEN
C ---          No "pancake" calculation for non-DFAULT FASTALL option (L_EFFSIGY)
               HRVAL  = AERPLM
               AERPAN = 0.0D0
               FRAN   = 0.0D0
C              CERC 11/30/20 Also set conc*travel time for GRSM to zero
               IF(GRSM)THEN
                 CHI_TTRAVPAN=0.0D0
               END IF

            ELSE

C ---          Next calculate random "pancake" component using radial distance
               L_PLUME = .FALSE.
C ---          Assign XDIST based on radial distance for use in dry depletion (FUNCTION F2INT)
               XDIST = DISTR
               CALL VOLCALC( DISTR, L_PLUME, AERPAN )
C ---          CERC 11/30/20 Calculate the conc*travel time for the Gaussian plume
               IF(GRSM)THEN
                 IF (STABLE .OR. (UNSTAB.AND.(HS.GE.ZI))) THEN
                   UCHM=UEFF
                 ELSE
                   UCHM=UEFFD
                 END IF
                 IF (UCHM/=0) THEN
                   CHI_TTRAVPAN=MAX(0.0D0,AERPAN(1)*XDIST/UCHM) !Don't allow negative conc*travel times
                 ELSE
                   !Error - no travel time if UEFF is zero
                   CALL ERRHDL(PATH, MODNAM, 'E','601','GRSM')
                   RUNERR=.TRUE.
                   RETURN
                 END IF
               END IF

C ---          Calculate fraction of random kinetic energy to total kinetic energy.
C              Note that these effective parameters are based on the radial dist.
               IF (STABLE .OR. (UNSTAB.AND.(HS.GE.ZI))) THEN
                  CALL MEANDR( UEFF, SVEFF, FRAN )
               ELSE IF (UNSTAB) THEN
                  CALL MEANDR( UEFFD, SVEFFD, FRAN )
               END IF

               HRVAL = FRAN*AERPAN + (1.0D0-FRAN)*AERPLM

C ---          CERC 11/30/20 Combine to calculate the conc*travel time for the combined plume
               IF(GRSM)THEN
                 CHI_TTRAVCHM(IREC,ISRC)=FRAN*CHI_TTRAVPAN +
     &                                         (1.0D0-FRAN)*CHI_TTRAVPLM
               END IF

            END IF

C           ENSR STATEMENT
            IF(DEBUG) THEN
               DO ITYP = 1, NUMTYP
                  WRITE(DBGUNT,10) AERPAN(ITYP), AERPLM(ITYP), FRAN,
     &                             HRVAL(ITYP)
10               FORMAT(/,'HRVAL(ITYP) = FRAN*AERPAN(ITYP) + (1.-FRAN)',
     &                                               '*AERPLM(ITYP)',//,
     &                    'PANCAKE/MEANDER COMPONENT, AERPAN(ITYP) = ',
     &            G16.8,/,'COHERENT PLUME COMPONENT,  AERPLM(ITYP) = ',
     &            G16.8,/,'MEANDER FACTOR, FRAN = ',
     &            G16.8,/,'RESULTANT CONC, HRVAL(ITYP) = ',G16.8,//)
               END DO
            END IF
!  Added for TTRM; AECOM

       IF (RUNTTRM) THEN
         TTRMOUT(IREC,ISRC,12) = UEFF
         TTRMOUT(IREC,ISRC,13) = UEFFD
         TTRMOUT(IREC,ISRC,15) = FRAN
       END IF

       IF (RUNTTRM) THEN
         TTRMOUT(IREC,ISRC,10) = X
         TTRMOUT(IREC,ISRC,11) = DISTR
         TTRMOUT(IREC,ISRC,20) = Y
       END IF
! End TTRM insert; Feb. 2021


            IF (PVMRM) THEN
C ---          Store data by source and receptor for PVMRM option
               DO ITYP = 1, NUMTYP
                  CHI(IREC,ISRC,ITYP) = HRVAL(ITYP)
               END DO
               IF (STABLE .OR. (UNSTAB.AND.(HS.GE.ZI))) THEN
                  HECNTR(IREC,ISRC) = HE
                  UEFFS(IREC,ISRC)  = UEFF
                  EPSEF(IREC,ISRC)  = EPSEFF
               ELSE
                  HECNTR(IREC,ISRC) = CENTER
                  UEFFS(IREC,ISRC)  = UEFFD
                  EPSEF(IREC,ISRC)  = EPSEFFD
               END IF
               IF (PPF .GT. 0.0D0) THEN
                  PPFACT(IREC,ISRC)  = PPF
                  HECNTR3(IREC,ISRC) = HE3
                  UEFF3S(IREC,ISRC)  = UEFF3
                  EPSEF3(IREC,ISRC)  = EPSEFF3
               ELSE
                  PPFACT(IREC,ISRC)  = 0.0D0
                  HECNTR3(IREC,ISRC) = 0.0D0
                  UEFF3S(IREC,ISRC)  = 0.0D0
                  EPSEF3(IREC,ISRC)  = 0.0D0
               END IF
               FOPTS(IREC,ISRC) = FOPT

C              Initialize __VAL arrays (1:NUMTYP)
               HRVAL(:)  = 0.0D0
               AERVAL(:) = 0.0D0
               AERPLM(:) = 0.0D0
               AERPAN(:) = 0.0D0
               PRMVAL(:) = 0.0D0

C              Cycle to next receptor & skip call to SUMVAL (will be done later)
               CYCLE RECEPTOR_LOOP

            ELSE IF (OLM) THEN
C ---          Store data by source and receptor for OLM option
               DO ITYP = 1, NUMTYP
                  CHI(IREC,ISRC,ITYP) = HRVAL(ITYP)
               END DO

C              Initialize __VAL arrays (1:NUMTYP)
               HRVAL(:)  = 0.0D0
               AERVAL(:) = 0.0D0
               AERPLM(:) = 0.0D0
               AERPAN(:) = 0.0D0
               PRMVAL(:) = 0.0D0

C              Cycle to next receptor & skip call to SUMVAL (will be done later)
               CYCLE RECEPTOR_LOOP
!    Added for TTRM; AECOM
            ELSE IF (RUNTTRM) THEN
C ---          Store data by source and receptor for TTRM option
               DO ITYP = 1, NUMTYP
                  CHI(IREC,ISRC,ITYP) = HRVAL(ITYP)
               END DO

C              Initialize __VAL arrays (1:NUMTYP)
               HRVAL(:)  = 0.0D0
               AERVAL(:) = 0.0D0
               AERPLM(:) = 0.0D0
               AERPAN(:) = 0.0D0
               PRMVAL(:) = 0.0D0

C              Cycle to next receptor & skip call to SUMVAL (will be done later)
               CYCLE RECEPTOR_LOOP
!    End TTRM insert; Feb. 2021

            ELSE IF (ARM2) THEN
C ---          Store data by source and receptor for ARM2 options
               DO ITYP = 1, NUMTYP
                  CHI(IREC,ISRC,ITYP) = HRVAL(ITYP)
               END DO

C              Initialize __VAL arrays (1:NUMTYP)
               HRVAL(:)  = 0.0D0
               AERVAL(:) = 0.0D0
               AERPLM(:) = 0.0D0
               AERPAN(:) = 0.0D0
               PRMVAL(:) = 0.0D0

C              Cycle to next receptor & skip call to SUMVAL (will be done later)
               CYCLE RECEPTOR_LOOP

C           CERC 11/30/20
            ELSE IF (GRSM) THEN
C ---          Store data by source and receptor for GRSM options
               DO ITYP = 1, NUMTYP
                  CHI(IREC,ISRC,ITYP) = HRVAL(ITYP)
               END DO

               IF (STABLE .OR. (UNSTAB.AND.(HS.GE.ZI))) THEN
                  HECNTR(IREC,ISRC) = HE
                  UEFFS(IREC,ISRC)  = UEFF
                  EPSEF(IREC,ISRC)  = EPSEFF
               ELSE
                  HECNTR(IREC,ISRC) = CENTER
                  UEFFS(IREC,ISRC)  = UEFFD
                  EPSEF(IREC,ISRC)  = EPSEFFD
               END IF
               IF (PPF .GT. 0.0D0) THEN
                  PPFACT(IREC,ISRC)  = PPF
                  HECNTR3(IREC,ISRC) = HE3
                  UEFF3S(IREC,ISRC)  = UEFF3
                  EPSEF3(IREC,ISRC)  = EPSEFF3
               ELSE
                  PPFACT(IREC,ISRC)  = 0.0D0
                  HECNTR3(IREC,ISRC) = 0.0D0
                  UEFF3S(IREC,ISRC)  = 0.0D0
                  EPSEF3(IREC,ISRC)  = 0.0D0
               END IF
               FOPTS(IREC,ISRC) = FOPT

C              Initialize __VAL arrays (1:NUMTYP)
               HRVAL(:)  = 0.0D0
               AERVAL(:) = 0.0D0
               AERPLM(:) = 0.0D0
               AERPAN(:) = 0.0D0
               PRMVAL(:) = 0.0D0

C              Cycle to next receptor & skip call to SUMVAL (will be done later)
               CYCLE RECEPTOR_LOOP

            END IF

C           Sum HRVAL to AVEVAL and ANNVAL Arrays  ---   CALL SUMVAL
C           As noted just above, the calls to EV_SUMVAL and SUMVAL
C           are skipped for a PVMRM, OLM, ARM2 or GRSM run; the
C           summing is performed for these options in the
C           respective routines
            IF (EVONLY) THEN
               CALL EV_SUMVAL
            ELSE
               DO IGRP = 1, NUMGRP
                  CALL SUMVAL
               END DO
            END IF

            IF (EVAL(ISRC)) THEN
C              Check ARC centerline values for EVALFILE
C              output                              ---   CALL EVALCK
               CALL EVALCK
            END IF

         END DO RECEPTOR_LOOP
C        End Receptor LOOP

C        Output 'ARC' Values for EVALFILE                   ---   CALL EVALFL
         IF (EVAL(ISRC)) THEN
            CALL EVALFL
         END IF

      END IF

      RETURN
      END


      SUBROUTINE VOLCALC( XARG, L_PLUME, AEROUT )
C***********************************************************************
C             VOLCALC Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Calculates the AERMOD concentration without downwash
C
C        PROGRAMMER: Roger Brode, PES, Inc.
C
C        DATE:     November 10, 2000
C
C        CHANGES:
C                  Added debug statement based on ENSR code.
C                  R. W. Brode, MACTEC (f/k/a PES), Inc., 07/27/04
C
C        INPUTS:   XARG         - Real - Distance (m), downwind for coherent
C                                        plume component and radial for
C                                        random component
C                  L_PLUME      - Log  - Specifies coherent plume calculation
C                                        if TRUE, otherwise random component
C
C        OUTPUTS:  AEROUT(NTYP) - Real - AERMOD component of concentration
C                                        without building downwash for either
C                                        coherent plume component or for
C                                        random component, depending on
C                                        L_PLUME.
C
C        CALLED FROM:   VCALC
C
C***********************************************************************
C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      DOUBLE PRECISION :: AEROUT(NUMTYP), AERTMP(NUMTYP), FYOUT, XARG,
     &                    ADJ, FRAN
      INTEGER :: J
      LOGICAL :: L_PLUME

C     Variable Initializations
      MODNAM = 'VOLCALC'

C --- Initialize FRAN
      FRAN  = 0.0D0

C     Initialize AEROUT and AERTMP arrays (1:NUMTYP)
      AEROUT = 0.0D0
      AERTMP = 0.0D0

C     Determine Deposition Correction Factors
      IF (NPD .EQ. 0 .AND. (LDGAS .OR. LWGAS)) THEN
         CALL PDEPG (XARG)
      ELSE
         DQCORG = 1.0D0
         WQCORG = 1.0D0
      END IF
      IF (NPD .GT. 0 .AND. (LDPART .OR. LWPART)) THEN
         CALL PDEP (XARG)
      ELSE IF (NPD .GT. 0) THEN
C        Set DQCOR(NPD) and WQCOR(NPD) arrays to 1.0
         DQCOR = 1.0D0
         WQCOR = 1.0D0
      END IF

C     Set initial effective parameters
      UEFF  = US
      SVEFF = SVS
      SWEFF = SWS
      TGEFF = TGS
      IF ( UNSTAB  .AND.  (HS .LT. ZI) ) THEN
         UEFFD  = US
         SVEFFD = SVS
         SWEFFD = SWS
         UEFFN  = US
         SVEFFN = SVS
         SWEFFN = SWS
      END IF

CRJP  Add temporary debugging statement here.

C     ENSR ENHANCEMENT OF WRITE STATEMENT TO IDENTIFY COMPONENT CONCENTRATION
      IF(DEBUG) THEN
        WRITE(DBGUNT, 6014) SRCID(ISRC)
6014    FORMAT(//,'SRCID: ', A8)
        IF(L_PLUME)THEN
           WRITE(DBGUNT, 6015) UEFF, SVEFF, SWEFF
6015       FORMAT(//,'COHERENT PLUME COMPONENT',/,5X,
     &       'Initial effective parameters for ',
     &       'stable or direct convective ',
     &       'plume:',//,5x,'Ueff = ',F7.2,' m/s; ',
     &       'SVeff = ',F7.2,
     &       ' m/s; SWeff = ',F7.2,' m/s.',/)
        ELSE
           WRITE(DBGUNT, 6016) UEFF, SVEFF, SWEFF
6016       FORMAT(//,'MEANDER COMPONENT',/,5X,
     &       'Initial effective parameters for ',
     &       'stable or direct convective ',
     &       'plume:',//,5x,'Ueff = ',F7.2,' m/s; ',
     &       'SVeff = ',F7.2,
     &       ' m/s; SWeff = ',F7.2,' m/s.',/)
        END IF
      END IF

C     Define plume centroid height (CENTER) for use in
C     inhomogeniety calculations
      CALL CENTROID (XARG)

C     If the atmosphere is unstable and the stack
C     top is below the mixing height, calculate
C     the CBL PDF coefficients                     ---   CALL PDF
      IF( UNSTAB  .AND.  (HS .LT. ZI) ) THEN
         CALL PDF
      END IF

C     Determine Effective Plume Height             ---   CALL HEFF
      CALL HEFF ( XARG )

C     Compute effective parameters using an
C     iterative average through plume rise layer
      CALL IBLVAL ( XARG )

C     Call PDF & HEFF again for final CBL plume heights
      IF (UNSTAB .AND. (HS.LT.ZI) ) THEN
         CALL PDF
         CALL HEFF ( XARG )
      END IF

C     Determine Dispersion Parameters              ---   CALL VDIS
      CALL VDIS ( XARG )

C     Calculate the 'y-term' contribution to
C     dispersion, FSUBY
      IF (L_PLUME) THEN
C        Calculate FSUBY for coherent plume
         IF( L_EFFSIGY )THEN
C ---       Calculate fraction of random kinetic energy to total kinetic energy
C           for FASTALL option to optimize meander using effective sigma-y (EFFSIGY),
C           and for the Beta option that uses an effective sigma-y value.
C           Note that these effective parameters are based on the downwind distance,
C           rather than the radial distance used in the standard meander approach
            IF (STABLE .OR. (UNSTAB.AND.(HS.GE.ZI))) THEN
               CALL MEANDR( UEFF, SVEFF, FRAN )
            ELSE IF (UNSTAB) THEN
               CALL MEANDR( UEFFD, SVEFFD, FRAN )
            END IF

C           Calculate effective sigma-y for non-DFAULT FASTALL option (EFFSIGY)
            SYEFF = 1.0D0/((FRAN/(SRT2PI*XARG)) + (1.0D0-FRAN)/SY)
            IF (DABS(Y) .GT. NUMSYEFF*SYEFF) THEN
C               Plume is more than 4 sigmas off centerline, skip calculation
                FYOUT = 0.0D0
            ELSE
C               Calculate FSUBY for coherent plume        ---   CALL FYPLM
                CALL FYPLM(SYEFF,FYOUT)
            END IF
         ELSE
C           Calculate FSUBY for coherent plume        ---   CALL FYPLM
            CALL FYPLM(SY,FYOUT)

         END IF

      ELSE
C        Calculate FSUBY for random component      ---   CALL FYPAN
         CALL FYPAN(FYOUT)

      END IF

      FSUBY  = FYOUT
      FSUBYD = FSUBY
      FSUBYN = FSUBYD

C     Set lateral term = 0.0 for penetrated source
      FSUBY3 = 0.0D0

C     Check for zero "y-terms"; if zero then skip calculations
C     and go to next receptor.
      IF( FSUBY.EQ.0.0D0 .AND. FSUBY3.EQ.0.0D0 )THEN
         AEROUT = 0.0D0

      ELSE

         IF (NPD .EQ. 0) THEN
C           Perform calculations for gases
C           Assign plume tilt, HV = 0.0

            ADJ = DQCORG * WQCORG

            IF (STABLE .OR. (UNSTAB.AND.(HS.GE.ZI))) THEN
C              Calculate height of the "effective reflecting surface"
               CALL REFL_HT (HE, XARG, 0.0D0, VSIGZ, HSBL)
            ELSE IF ( UNSTAB ) THEN
               HSBL = 0.0D0
            END IF

C           Determine the CRITical Dividing Streamline---   CALL CRITDS
            CALL CRITDS (HE)

C           Calculate the fraction of plume below
C           HCRIT, PHEE                               ---   CALL PFRACT
            CALL PFRACT (HE)

C           Calculate FOPT = f(PHEE)                  ---   CALL FTERM
            CALL FTERM

C           Calculate Conc. for Virtual Point Source  ---   CALL AER_PCHI
            CALL AER_PCHI( XARG, ADJ, VDEPG, 0, AEROUT )

         ELSE
C           Perform calculations for particles, loop through particle sizes

C           Begin loop over particle sizes
            DO J = 1, NPD

C              Calculate Plume Tilt Due to Settling, HV
               HV = (XARG/US) * VGRAV(J)

C              Adjust Jth contribution by mass fraction and source
C              depletion
               ADJ = PHI(J) * DQCOR(J) * WQCOR(J)

               IF (STABLE .OR. (UNSTAB.AND.(HS.GE.ZI))) THEN
C                 Calculate height of the "effective reflecting surface"
                  HESETL = MAX( 0.0D0, HE - HV )
                  CALL REFL_HT (HESETL, XARG, 0.0D0, VSIGZ, HSBL)
               ELSE IF ( UNSTAB ) THEN
                  HESETL = MAX( 0.0D0, 0.5D0*(HED1+HED2) - HV )
                  HSBL = 0.0D0
               END IF

C              Determine the CRITical Dividing Streamline---   CALL CRITDS
               CALL CRITDS (HESETL)

C              Calculate the fraction of plume below
C              HCRIT, PHEE                               ---   CALL PFRACT
               CALL PFRACT (HESETL)

C              Calculate FOPT = f(PHEE)                  ---   CALL FTERM
               CALL FTERM

C              Calculate Conc. for Virtual Point Source  ---   CALL AER_PCHI
               CALL AER_PCHI( XARG, ADJ, VDEP(J), J, AERTMP )
               AEROUT = AEROUT + AERTMP

            END DO
C           End loop over particle sizes

         END IF
      END IF

      RETURN
      END


      SUBROUTINE ACALC
C***********************************************************************
C            ACALC Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Calculates concentration or deposition values
C                 for AREA sources utilizing an integrated line source.
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To calculate conc*travel time for GRSM NO2 option.
C                    CERC, 11/30/20
C
C        MODIFIED:
C                    Added arrays to save WDSIN and WDCOS by source for use
C                    with PVMRM option.  Corrects potential problem for
C                    PVMRM applications with multi-level wind inputs.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 01/26/2007
C
C                    Added call to subroutine HEFF for calculation
C                    of ZSUBP for deposition applications.
C                    R. W. Brode, U.S. EPA, OAQPS, AQMG, 01/24/2007
C
C        MODIFIED:   Modified to initialize HE = HS prior to first
C                    call to sub. ADISY.
C                    R. W. Brode, MACTEC (f/k/a PES), Inc., 07/05/05
C
C        MODIFIED:   Modified to include initialization of __VAL arrays
C                    at end of receptor loop.
C                    R. W. Brode, MACTEC (f/k/a PES), Inc., 10/26/04
C
C        MODIFIED:   To include the PVMRM and OLM options for
C                    modeling conversion of NOx to NO2.
C                    Added debug statement based on ENSR code.
C                    R. W. Brode, MACTEC (f/k/a PES), Inc., 07/27/04
C
C        MODIFIED:   To include tilted plume for point source
C                    approximation of particle emissions.
C                    R. W. Brode, MACTEC (f/k/a PES), Inc., 07/23/04
C
C        MODIFIED:   To allow TOXICS option for AREAPOLY sources.
C                    R. W. Brode, MACTEC (f/k/a PES), Inc., 05/12/04
C
C        MODIFIED:   To assign value to XDIST for use in dry depletion.
C                    R. W. Brode, MACTEC (f/k/a PES), Inc., 03/19/04
C
C        MODIFIED:   To avoid potential math errors for AREAPOLY sources
C                    R.W. Brode, PES, Inc. - 02/25/02
C
C        MODIFIED:   To incorporate numerical integration algorithm
C                    for AREA source - 7/7/93
C
C        INPUTS:  Source Parameters for Specific Source
C                 Arrays of Receptor Locations
C                 Meteorological Variables for One Hour
C
C        OUTPUTS: Array of 1-hr CONC or DEPOS Values for Each Source/Receptor
C
C        CALLED FROM:   CALC
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      INTEGER :: I, J
      DOUBLE PRECISION :: XDEP, WIDTH, LENGTH, XMINR, XMAXR, XPOINT,
     &                    QTKSAV, ADJ
      DOUBLE PRECISION :: AEROUT(NUMTYP), FYOUT

C     Variable Initializations
      MODNAM = 'ACALC'
      WAKE = .FALSE.

C     Initialize __VAL arrays (1:NUMTYP)
      HRVAL   = 0.0D0
      AERVAL  = 0.0D0
      AEROUT  = 0.0D0
      PRMVAL  = 0.0D0
      IF( ALLOCATED(CHI) ) CHI(:,ISRC,:) = 0.0D0
      IF(GRSM)THEN
        CHI_TTRAVCHM(:,ISRC) = 0.0D0
      END IF

C     Set the Source Variables for This Source              ---   CALL SETSRC
      CALL SETSRC

C     Apply Variable Emission Rate Factors                  ---   CALL EMFACT
      CALL EMFACT(QS)

C     Initialize 'ARC' Arrays for EVALFILE Output           ---   CALL EVLINI
      IF (EVAL(ISRC)) THEN
         CALL EVLINI
      END IF

C ---  Write info to AREA debug file if needed
       IF( AREADBG )THEN
          write(AREADBUNT,100) KURDAT
100       format(/'AREA Debug Sub_ACALC: ',2x,'YYMMDDHH = ',I8.8)
       ENDIF

      IF (QTK .NE. 0.0D0) THEN

C ---    Save original emission rate, which may be needed if RECLOOP is cycled
C        and point source approximation is used (FASTAREA or FASTALL options)
         QTKSAV = QTK

C        Set Mixing Height and Profiles for Urban Option if Needed
         IF (URBSRC(ISRC) .EQ. 'Y') THEN
C           Find Urban Area Index for This Source
            DO I = 1, NUMURB
               IF (IURBGRP(ISRC,I) .EQ. 1) THEN
                  IURB = I
                  EXIT
               END IF
            END DO
            IF (STABLE .OR. L_MorningTrans(IURB)) THEN
               URBSTAB = .TRUE.
               ZI = MAX( ZIURB(IURB), ZIMECH )
               GRIDSV = GRDSVU(1:MXGLVL,IURB)
               GRIDSW = GRDSWU(1:MXGLVL,IURB)
               GRIDTG = GRDTGU(1:MXGLVL,IURB)
               GRIDPT = GRDPTU(1:MXGLVL,IURB)
               OBULEN = URBOBULEN(IURB)
               USTAR  = URBUSTR(IURB)
            ELSE
               URBSTAB = .FALSE.
               ZI = ZIRUR
               GRIDSV = GRDSVR
               GRIDSW = GRDSWR
               GRIDTG = GRDTGR
               GRIDPT = GRDPTR
               OBULEN = RUROBULEN
               USTAR  = RURUSTR
            END IF
         ELSE IF (URBAN .AND. URBSRC(ISRC) .EQ. 'N') THEN
            URBSTAB = .FALSE.
            ZI = ZIRUR
            GRIDSV = GRDSVR
            GRIDSW = GRDSWR
            GRIDTG = GRDTGR
            GRIDPT = GRDPTR
            OBULEN = RUROBULEN
            USTAR  = RURUSTR
         ELSE
C ---       Rural
            URBSTAB = .FALSE.
         END IF

C        Initialize meteorological variables                ---   CALL METINI
         CALL METINI

C        Save WDSIN and WSCOS for later use by PVMRM/GRSM option
         AWDSIN(ISRC) = WDSIN
         AWDCOS(ISRC) = WDCOS
         AAFV(ISRC)   = AFV

C        Initialize miscellaneous variables
         FB  = 0.0D0
         FM  = 0.0D0
         PPF = 0.0D0
         HSP = HS
         HE  = HS
         DHP  = 0.0D0
         DHP1 = 0.0D0
         DHP2 = 0.0D0
         DHP3 = 0.0D0
         DHCRIT = 0.0D0
         XFINAL = 0.0D0
         XMIXED = ZI * UAVG / SWAVG
         IF(XMIXED .LT. XFINAL) XMIXED = XFINAL
         ZMIDMX = 0.5D0 * ZI

C ---    Initialize PDF parameters for use in calculating ZSUBP
         IF( UNSTAB  .AND.  (HS .LT. ZI) ) THEN
            CALL PDF
         END IF
C        Set Dry Deposition Variables for this Source
         IF (LUSERVD .AND. LDGAS .AND. NPD.EQ.0) THEN
C           Assign user-specified gas dry deposition velocity (GASDEPVD option)
            VDEPG = USERVD
         ELSE IF (LDPART .OR. (.NOT.LUSERVD .AND. LDGAS .AND.
     &                                                   NPD.EQ.0)) THEN
C           Calculate Deposition Velocities for this Source    ---   CALL VDP
            CALL VDP
         END IF
         IF (LWPART .OR. LWGAS) THEN
CPES        Set value of ZSUBP = MAX( ZI, TOP OF PLUME ), where
CPES        TOP OF PLUME is defined as plume height (HE) plus 2.15*SZ,
CPES        evaluated at a distance of 20 kilometers downwind.
CPES        Apply minimum value of 500m and maximum value of 10,000m.
            CALL HEFF (20000.0D0)
            IF( STABLE .OR. (UNSTAB .AND. HS .GE. ZI) )THEN
               CALL SIGZ(20000.0D0)
               ZSUBP = MAX( 500.0D0, ZI, HS + SZCOEF*SZAS )
            ELSE IF (UNSTAB) THEN
               CALL SIGZ(20000.0D0)
               ZSUBP = MAX( 500.0D0, ZI, HS +
     &                                   SZCOEF*(SZAD1+SZAD2)/2.0D0 )
            END IF
            ZSUBP = MIN( 10000.0D0, ZSUBP )
C           Calculate Scavenging Ratios for this Source           ---   CALL SCAVRAT
            CALL SCAVRAT
         END IF

C        Begin Receptor LOOP
         RECEPTOR_LOOP: DO IREC = 1, NUMREC

!  Added for TTRM; AECOM
            IF (RUNTTRM) THEN
              TTRMOUT(IREC,ISRC,11) = DISTR
              TTRMOUT(IREC,ISRC,12) = UEFF
              TTRMOUT(IREC,ISRC,13) = UEFFD
            END IF
!  End TTRM insert; Feb. 2021

C           Calculate Down and Crosswind Distances          ---   CALL ARDIST
            IF( AREADBG )THEN
               IF( EVONLY )THEN
                  write(AREADBUNT,101) SRCID(ISRC), IEVENT
101               format(/1x,'SRCID= ', A12,'  IEVT=',I7,/)
               ELSE
                  write(AREADBUNT,102) SRCID(ISRC), IREC
102               format(/1x,'SRCID= ', A12,'  IREC=',I7,/)
               ENDIF
            ENDIF

            IF (EVONLY) THEN
               CALL ARDIST(IEVENT,XDEP,WIDTH,LENGTH,XMINR,XMAXR)
            ELSE
               CALL ARDIST(IREC,XDEP,WIDTH,LENGTH,XMINR,XMAXR)
            END IF

C           Check to see if receptor is upwind of area source;
C           also check to see if receptor is beyond the maximum
C           distance from source; assigned value of 80km
C           for obsolescent TOXICS option or new FASTAREA or
C           FASTALL options; otherwise "unlimited" (1.0D20)
            IF (XMAXR .LT. 1.0D0 .OR. DISTR .GT. MAXDIST) THEN
!     Added for TTRM; AECOM
!           For TTRM, reset radial distance if the receptor is upwind or
!           the maximum distance from the source
              IF (RUNTTRM) THEN
                TTRMOUT(IREC,ISRC,11) = 0.0D0
              END IF
!    End TTRM insert; Feb. 2021
C              Receptor is upwind of source or receptor is beyond the
C              maximum distance from source; set CHI = 0.0 for current
C              REC and SRC and set NO2 option variables to 0.0 before
C              cycling to next receptor
               IF (PVMRM .OR. OLM .OR. ARM2
     &            .OR. RUNTTRM .OR. GRSM) THEN
C ---             Assign 0.0 to CHI(IREC,ISRC,ITYP) before
C                 cycling to the next receptor to avoid
C                 persisting value from previous hour.
                  CHI(IREC,ISRC,:) = 0.0D0
                  IF(GRSM)THEN
                    CHI_TTRAVCHM(IREC,ISRC) = 0.0D0
                  END IF
                  IF (PVMRM .OR. GRSM) THEN
                     HECNTR(IREC,ISRC)  = 0.0D0
                     UEFFS(IREC,ISRC)   = 0.0D0
                     EPSEF(IREC,ISRC)   = 0.0D0
                     PPFACT(IREC,ISRC)  = 0.0D0
                     HECNTR3(IREC,ISRC) = 0.0D0
                     UEFF3S(IREC,ISRC)  = 0.0D0
                     EPSEF3(IREC,ISRC)  = 0.0D0
                     FOPTS(IREC,ISRC)   = 0.0D0
                  END IF
               END IF
C ---          Also set HRVAL/AERVAL/AEROUR = 0
               HRVAL(:)  = 0.0D0
               AERVAL(:) = 0.0D0
               AEROUT(:) = 0.0D0

C ---          Reset QTK since it may have been changed if
C              point source approximation has been used
               QTK = QTKSAV
C ---          Cycle to next receptor
               CYCLE RECEPTOR_LOOP
            END IF

C           Initialize HE for initial call to ADISY
            HE = HS
C           Set initial effective parameters
            UEFF  = US
            SVEFF = SVS
            SWEFF = SWS
            TGEFF = TGS
            IF ( UNSTAB  .AND.  (HS .LT. ZI) ) THEN
               UEFFD  = US
               SVEFFD = SVS
               SWEFFD = SWS
               UEFFN  = US
               SVEFFN = SVS
               SWEFFN = SWS
            END IF

C ---       Check to see if receptor is beyond edge of plume laterally.
            IF ( (DABS(Y)-0.5D0*WIDTH) .GT. 0.0D0) THEN
C ---          Receptor is outside "projected" width of effective area source
C              Calculate maximum sigma-y based on XMAXR and compare minimum
C              lateral distance of receptor from source to 5*sigma-y;
C              Note that Y represents the lateral distance to the receptor
C              from the center of the source.

C ---          Calculate SY at XMAXR to determine if receptor is beyond
C              the plume laterally.
               CALL ADISY(XMAXR)
               IF ((DABS(Y)-0.5D0*WIDTH) .GE. 5.0D0*SY) THEN
!     Added for TTRM; AECOM
!           For TTRM, reset radial distance if the receptor is more than
!           5&SY from the edge of the projected area source
              IF (RUNTTRM) THEN
                TTRMOUT(IREC,ISRC,11) = 0.0D0
              END IF
!     End TTRM insert; Feb. 2021
C                 Receptor is more than 5*SY from the edge of the
C                 projected area source; set CHI = 0.0 for current
C                 REC and SRC and set NO2 option variables to 0.0
C                 before cycling to next receptor.
                  IF (PVMRM .OR. OLM .OR. ARM2
     &               .OR. RUNTTRM .OR. GRSM) THEN
C ---                Assign 0.0 to CHI(IREC,ISRC,ITYP) before
C                    cycling to the next receptor to avoid
C                    persisting value from previous hour.
                     CHI(IREC,ISRC,:) = 0.0D0
                     IF(GRSM)THEN
                       CHI_TTRAVCHM(IREC,ISRC) = 0.0D0
                     END IF
                     IF (PVMRM .OR. GRSM) THEN
                        HECNTR(IREC,ISRC)  = 0.0D0
                        UEFFS(IREC,ISRC)   = 0.0D0
                        EPSEF(IREC,ISRC)   = 0.0D0
                        PPFACT(IREC,ISRC)  = 0.0D0
                        HECNTR3(IREC,ISRC) = 0.0D0
                        UEFF3S(IREC,ISRC)  = 0.0D0
                        EPSEF3(IREC,ISRC)  = 0.0D0
                        FOPTS(IREC,ISRC)   = 0.0D0
                     END IF
                  END IF
C                 Also reinitialize HRVAL, AERVAL and AEROUT
                  HRVAL(:)  = 0.0D0
                  AERVAL(:) = 0.0D0
                  AEROUT(:) = 0.0D0

C ---             Reset QTK since it may have been changed if
C                 point source approximation has been used
                  QTK = QTKSAV

C ---             Cycle to next receptor
                  CYCLE RECEPTOR_LOOP
               END IF
            END IF

            IF(DEBUG) THEN
              WRITE(DBGUNT, 6015) UEFF, SVEFF, SWEFF
C             ENSR ENHANCEMENT OF WRITE STATEMENT
6015            FORMAT(//,'AERMOD AREA SOURCE COMPONENT',/,5X,
     &          'Initial effective parameters for ',
     &          'stable or direct convective ',
     &          'plume:',//,5x,'Ueff = ',F7.2,' m/s; ',
     &          'SVeff = ',F7.2,
     &          ' m/s; SWeff = ',F7.2,' m/s.',/)
            END IF

C           Determine the CRITical Dividing Streamline      ---   CALL CRITDS
            CALL CRITDS (HE)

C           Set distance factor for point source approx. for FASTAREA option
C           based on "equivalent" PG stability class (KST)
            IF (URBSTAB) THEN
               VP_FACT = VIRTPNT_URB(KST)
            ELSE
               VP_FACT = VIRTPNT_RUR(KST)
            END IF

C           Calculate distance for switch to point source approx. for FASTAREA
            XPOINT = 1.5D0*LENGTH + VP_FACT*WIDTH

C ---       Assign XDIST for use in dry depletion (FUNCTION F2INT)
            XDIST = X

            IF ( (.NOT.FASTAREA .AND. .NOT.FASTALL) .OR.
     &           ((FASTAREA .OR. FASTALL) .AND. X .LT. XPOINT)) THEN
               IF (ARDPLETE) THEN

                  IF (NPD .EQ. 0 .AND. (LDGAS .OR. LWGAS)) THEN
                     CALL PDEPG (XDEP)
                  ELSE
                     DQCORG = 1.0D0
                     WQCORG = 1.0D0
                  END IF
                  IF (NPD .GT. 0 .AND. (LDPART .OR. LWPART)) THEN
                     CALL PDEP (XDEP)
                  ELSE IF (NPD .GT. 0) THEN
C                    Set DQCOR(NPD) and WQCOR(NPD) arrays to 1.0
                     DQCOR = 1.0D0
                     WQCOR = 1.0D0
                  END IF

               END IF

               DO ITYP = 1, NUMTYP
C                 Calculate Area Source Integral         ---   CALL AREAIN
                  CALL AREAIN
               END DO
            ELSE
C              Use point source approximation
C              Save emissions per unit area and calculate total emissions
               QTKSAV = QTK
               IF (SRCTYP(ISRC) .EQ. 'AREA' .OR.
     &             SRCTYP(ISRC) .EQ. 'LINE' .OR.
     &             SRCTYP(ISRC) .EQ. 'AREAPOLY') THEN
C                 Note that XINIT and YINIT are equivalent values for AREAPOLY
                  QTK = QTK * XINIT * YINIT
               ELSE IF (SRCTYP(ISRC) .EQ. 'AREACIRC') THEN
                  QTK = QTK * PI * RADIUS(ISRC) * RADIUS(ISRC)
               END IF
               SYINIT = 0.0D0

C              Determine Deposition Correction Factors
               IF (NPD .EQ. 0 .AND. (LDGAS .OR. LWGAS)) THEN
                  CALL PDEPG (X)
               ELSE
                  DQCORG = 1.0D0
                  WQCORG = 1.0D0
               END IF
               IF (NPD .GT. 0 .AND. (LDPART .OR. LWPART)) THEN
                  CALL PDEP (X)
               ELSE IF (NPD .GT. 0) THEN
C                 Set DQCOR(NPD) and WQCOR(NPD) arrays to 1.0
                  DQCOR = 1.0D0
                  WQCOR = 1.0D0
               END IF

C              Define plume centroid height (CENTER) for use in
C              inhomogeniety calculations
               CALL CENTROID (X)

C              If the atmosphere is unstable and the stack
C              top is below the mixing height, calculate
C              the CBL PDF coefficients                     ---   CALL PDF
               IF( UNSTAB  .AND.  (HS .LT. ZI) ) THEN
                  CALL PDF
               END IF

C              Determine Effective Plume Height             ---   CALL HEFF
               CALL HEFF ( X )

C              Compute effective parameters using an
C              iterative average through plume rise layer
               CALL IBLVAL (X)

C              Call PDF & HEFF again for final CBL plume heights
               IF (UNSTAB .AND. (HS.LT.ZI) ) THEN
                  CALL PDF
                  CALL HEFF (X)
               END IF

C              Determine Dispersion Parameters              ---   CALL VDIS
               CALL VDIS (X)

C              Calculate the 'y-term' contribution to
C              dispersion, FSUBY, for coherent plume        ---   CALL FYPLM
               CALL FYPLM(SY,FYOUT)
               FSUBY  = FYOUT
               FSUBYD = FSUBY
               FSUBYN = FSUBYD

C              Set lateral term = 0.0 for penetrated source
               FSUBY3 = 0.0D0

C              Check for zero "y-terms"; if zero then skip calculations
C              and go to next receptor.
               IF( FSUBY.EQ.0.0D0 .AND. FSUBY3.EQ.0.0D0 )THEN
!      Added for TTRM; AECOM
!              For TTRM, if the "y-terms" are zero then set the radial
!              distance to zero.
                  IF (RUNTTRM) THEN
                  TTRMOUT(IREC,ISRC,11) = 0.0D0
                  END IF
!      End TTRM insert; Feb. 2021
C                 FSUBY terms are both 0.0; assign 0.0 to arrays
                  IF (PVMRM .OR. OLM .OR. ARM2
     &               .OR. RUNTTRM .OR. GRSM) THEN
C ---                Assign 0.0 to CHI(IREC,ISRC,ITYP) before
C                    cycling to the next receptor to avoid
C                    persisting value from previous hour.
                     CHI(IREC,ISRC,:) = 0.0D0
                     IF(GRSM)THEN
                       CHI_TTRAVCHM(IREC,ISRC) = 0.0D0
                     END IF
                     IF (PVMRM .OR. GRSM) THEN
                        HECNTR(IREC,ISRC)  = 0.0D0
                        UEFFS(IREC,ISRC)   = 0.0D0
                        EPSEF(IREC,ISRC)   = 0.0D0
                        PPFACT(IREC,ISRC)  = 0.0D0
                        HECNTR3(IREC,ISRC) = 0.0D0
                        UEFF3S(IREC,ISRC)  = 0.0D0
                        EPSEF3(IREC,ISRC)  = 0.0D0
                        FOPTS(IREC,ISRC)   = 0.0D0
                     END IF

                  END IF
C                 Also reinitialize HRVAL, AERVAL and AEROUT
                  HRVAL(:)  = 0.0D0
                  AERVAL(:) = 0.0D0
                  AEROUT(:) = 0.0D0

C ---             Reset QTK since it may have been changed if
C                 point source approximation has been used
                  QTK = QTKSAV

C ---             Cycle to next receptor
                  CYCLE RECEPTOR_LOOP

               ELSE

                  IF (NPD .EQ. 0) THEN
C                    Perform calculations for gases
C                    Assign plume tilt, HV = 0.0

                     ADJ = DQCORG * WQCORG

                     IF (STABLE .OR. (UNSTAB.AND.(HS.GE.ZI))) THEN
C                       Calculate height of the "effective reflecting surface"
                        CALL REFL_HT (HE, X, 0.0D0, VSIGZ, HSBL)
                     ELSE IF ( UNSTAB ) THEN
                        HSBL = 0.0D0
                     END IF

C                    Determine the CRITical Dividing Streamline---   CALL CRITDS
                     CALL CRITDS (HE)

C                    Calculate the fraction of plume below
C                    HCRIT, PHEE                               ---   CALL PFRACT
                     CALL PFRACT (HE)

C                    Calculate FOPT = f(PHEE)                  ---   CALL FTERM
                     CALL FTERM

C                    Calculate Conc. for Virtual Point Source  ---   CALL AER_PCHI
                     CALL AER_PCHI( X, ADJ, VDEPG, 0, AEROUT )
                     HRVAL = AEROUT

                  ELSE
C                    Perform calculations for particles, loop through particle sizes

C                    Begin loop over particle sizes
                     DO J = 1, NPD

C                       Calculate Plume Tilt Due to Settling, HV
                        HV = (X/US) * VGRAV(J)

C                       Adjust Jth contribution by mass fraction and source
C                       depletion
                        ADJ = PHI(J) * DQCOR(J) * WQCOR(J)

                        IF (STABLE .OR. (UNSTAB.AND.(HS.GE.ZI))) THEN
C                          Calculate height of the "effective reflecting surface"
C                          Calculate Settled Plume Height(s), HESETL
                           HESETL = MAX( 0.0D0, HE - HV )
                           CALL REFL_HT (HESETL, X, 0.0D0, VSIGZ, HSBL)
                        ELSE IF ( UNSTAB ) THEN
C                          Calculate Settled Plume Height(s), HESETL
                           HESETL = MAX( 0.0D0, 0.5D0*(HED1+HED2) - HV )
                           HSBL = 0.0D0
                        END IF

C                       Determine the CRITical Dividing Streamline---   CALL CRITDS
                        CALL CRITDS (HESETL)

C                       Calculate the fraction of plume below
C                       HCRIT, PHEE                               ---   CALL PFRACT
                        CALL PFRACT (HESETL)

C                       Calculate FOPT = f(PHEE)                  ---   CALL FTERM
                        CALL FTERM

C                       Calculate Conc. for Virtual Point Source  ---   CALL AER_PCHI
                        CALL AER_PCHI( X, ADJ, VDEP(J), J, AEROUT )
                        HRVAL = HRVAL + AEROUT

                     END DO
C                    End loop over particle sizes

                  END IF
               END IF
C ---          Reset QTK since it may have been changed if
C              point source approximation has been used
               QTK = QTKSAV
            END IF

            IF (PVMRM) THEN
C ---          Store data by source and receptor for PVMRM option
               DO ITYP = 1, NUMTYP
                  CHI(IREC,ISRC,ITYP) = HRVAL(ITYP)
               END DO
               IF (STABLE .OR. (UNSTAB.AND.(HS.GE.ZI))) THEN
                  HECNTR(IREC,ISRC) = HE
                  UEFFS(IREC,ISRC)  = UEFF
                  EPSEF(IREC,ISRC)  = EPSEFF
               ELSE
                  HECNTR(IREC,ISRC) = CENTER
                  UEFFS(IREC,ISRC)  = UEFFD
                  EPSEF(IREC,ISRC)  = EPSEFFD
               END IF
               IF (PPF .GT. 0.0D0) THEN
                  PPFACT(IREC,ISRC)  = PPF
                  HECNTR3(IREC,ISRC) = HE3
                  UEFF3S(IREC,ISRC)  = UEFF3
                  EPSEF3(IREC,ISRC)  = EPSEFF3
               ELSE
                  PPFACT(IREC,ISRC)  = 0.0D0
                  HECNTR3(IREC,ISRC) = 0.0D0
                  UEFF3S(IREC,ISRC)  = 0.0D0
                  EPSEF3(IREC,ISRC)  = 0.0D0
               END IF
               FOPTS(IREC,ISRC) = FOPT

C              Initialize __VAL arrays (1:NUMTYP)
               HRVAL(:)  = 0.0D0
               AERVAL(:) = 0.0D0
               AEROUT(:) = 0.0D0

C              Cycle to next receptor & skip call to SUMVAL (will be done later)
               CYCLE RECEPTOR_LOOP

            ELSE IF (OLM) THEN
C ---          Store data by source and receptor for OLM option
               DO ITYP = 1, NUMTYP
                  CHI(IREC,ISRC,ITYP) = HRVAL(ITYP)
               END DO

C              Initialize __VAL arrays (1:NUMTYP)
               HRVAL(:)  = 0.0D0
               AERVAL(:) = 0.0D0
               AEROUT(:) = 0.0D0

C              Cycle to next receptor & skip call to SUMVAL (will be done later)
               CYCLE RECEPTOR_LOOP
!     Added for TTRM; AECOM
            ELSE IF (RUNTTRM) THEN
C ---          Store data by source and receptor for OLM option
               DO ITYP = 1, NUMTYP
                  CHI(IREC,ISRC,ITYP) = HRVAL(ITYP)
               END DO

C              Initialize __VAL arrays (1:NUMTYP)
               HRVAL(:)  = 0.0D0
               AERVAL(:) = 0.0D0
               AEROUT(:) = 0.0D0

C              Cycle to next receptor & skip call to SUMVAL (will be done later)
               CYCLE RECEPTOR_LOOP
!     End TTRM insert; Feb. 2021

            ELSE IF (ARM2) THEN
C ---          Store data by source and receptor for ARM2 options
               DO ITYP = 1, NUMTYP
                  CHI(IREC,ISRC,ITYP) = HRVAL(ITYP)
               END DO

C              Initialize __VAL arrays (1:NUMTYP)
               HRVAL(:)  = 0.0D0
               AERVAL(:) = 0.0D0
               AEROUT(:) = 0.0D0

C              Cycle to next receptor & skip call to SUMVAL (will be done later)
               CYCLE RECEPTOR_LOOP

C           CERC 11/30/20
            ELSE IF (GRSM) THEN

C              Calculate the effective wind speed
               IF (STABLE .OR. (UNSTAB.AND.(HS.GE.ZI))) THEN
                 UCHM=UEFF
               ELSE
                 UCHM=UEFFD
               END IF
C              Calculate the conc x travel time for the chemistry scheme
               IF(UCHM/=0)THEN
                 CHI_TTRAVCHM(IREC,ISRC)=
     &                     MAX(0.0D0,HRVAL(1)*X/UCHM) !Don't allow negative conc*travel times
               ELSE
                 !Error - no travel time if UCHM is zero
                 CALL ERRHDL(PATH, MODNAM, 'E','601','GRSM')
                 RUNERR=.TRUE.
                 RETURN
               END IF

C ---          Store data by source and receptor for GRSM options
               DO ITYP = 1, NUMTYP
                  CHI(IREC,ISRC,ITYP) = HRVAL(ITYP)
               END DO

               IF (STABLE .OR. (UNSTAB.AND.(HS.GE.ZI))) THEN
                  HECNTR(IREC,ISRC) = HE
                  UEFFS(IREC,ISRC)  = UEFF
                  EPSEF(IREC,ISRC)  = EPSEFF
               ELSE
                  HECNTR(IREC,ISRC) = CENTER
                  UEFFS(IREC,ISRC)  = UEFFD
                  EPSEF(IREC,ISRC)  = EPSEFFD
               END IF
               IF (PPF .GT. 0.0D0) THEN
                  PPFACT(IREC,ISRC)  = PPF
                  HECNTR3(IREC,ISRC) = HE3
                  UEFF3S(IREC,ISRC)  = UEFF3
                  EPSEF3(IREC,ISRC)  = EPSEFF3
               ELSE
                  PPFACT(IREC,ISRC)  = 0.0D0
                  HECNTR3(IREC,ISRC) = 0.0D0
                  UEFF3S(IREC,ISRC)  = 0.0D0
                  EPSEF3(IREC,ISRC)  = 0.0D0
               END IF
               FOPTS(IREC,ISRC) = FOPT

C              Initialize __VAL arrays (1:NUMTYP)
               HRVAL(:)  = 0.0D0
               AERVAL(:) = 0.0D0
               AEROUT(:) = 0.0D0

C              Cycle to next receptor & skip call to SUMVAL (will be done later)
               CYCLE RECEPTOR_LOOP

            END IF

C           Sum HRVAL to AVEVAL and ANNVAL Arrays  ---   CALL SUMVAL
C           As noted just above, the calls to EV_SUMVAL and SUMVAL
C           are skipped for a PVMRM, OLM, ARM2 or GRSM run; the
C           summing is performed for these options in the
C           respective routines
            IF (EVONLY) THEN
               CALL EV_SUMVAL
            ELSE
               DO IGRP = 1, NUMGRP
                  CALL SUMVAL
               END DO
            END IF

C           Initialize __VAL arrays (1:NUMTYP)
            HRVAL(:)  = 0.0D0
            AERVAL(:) = 0.0D0
            AEROUT(:) = 0.0D0

         END DO RECEPTOR_LOOP
C        End Receptor LOOP

C        Output 'ARC' Values for EVALFILE                   ---   CALL EVALFL
         IF (EVAL(ISRC)) THEN
            CALL EVALFL
         END IF

      END IF

      RETURN
      END

      SUBROUTINE ARDIST(INDX,XDEP,WIDTH,LENGTH,XMINREC,XMAXREC)
C***********************************************************************
C                 ARDIST Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Sets Receptor Variables and Calculates Downwind (X)
C                 and Crosswind (Y) Distances, Crosswind Width (WIDTH),
C                 Distance used for AREADPLT Option (XDEP), Maximum
C                 Downwind Distance by Vertex (XMAXREC), and
C                 Radial Distance from Source to Receptor (DISTR)
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   Moved calculation of center of effective area
C                    source for OPENPIT sources to subroutine PITEFF.
C                    Previous versions would have skipped calculation
C                    of center coordinates if first receptor was located
C                    inside the actual OPENPIT source.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, XX/YY/2013
C
C        MODIFIED:   Recalculate the center of effective area source
C                    for OPENPIT sources when INDX = 1.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
C
C        INPUTS:  Receptor Index, INDX
C                 Source Location
C                 Arrays of Receptor Locations
C                 SIN and COS of Wind Direction FROM Which Wind
C                 is Blowing, WDSIN and WDCOS
C
C        OUTPUTS: Values of X, Y, and DISTR (m) [in MAIN1]
C                 XDEP (m)
C                 WIDTH (m)
C                 LENGTH (m)
C                 XMAXREC (m)
C
C        CALLED FROM:   ACALC
C                       OCALC
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, INDX
      DOUBLE PRECISION :: XSRC, YSRC, XMINREC, XMAXREC, YMINREC,
     &                    YMAXREC, XDEP, WIDTH, LENGTH

C     Variable Initializations
      MODNAM = 'ARDIST'

C     Set Receptor Coordinates, Terrain Elevation and Flagpole Heights
      XR = AXR(INDX)
      YR = AYR(INDX)
      ZELEV = AZELEV(INDX)
      ZHILL = AZHILL(INDX)
      ZFLAG = AZFLAG(INDX)

C     Initialize min and max distances from source to receptor
      XMINREC =  9.0D10
      XMAXREC = -9.0D10
      YMINREC =  9.0D10
      YMAXREC = -9.0D10

C     Initialize SPA(:,:) array
      SPA(:,:) = 0.0D0

C     Calculate Downwind (X) and Crosswind (Y) Distances for Each Vertex
      DO I = 1, NVERT+1
         XSRC = XVERT(I)
         YSRC = YVERT(I)
         SPA(I,1) = -((XR-XSRC)*WDSIN + (YR-YSRC)*WDCOS)
         SPA(I,2) =   (XR-XSRC)*WDCOS - (YR-YSRC)*WDSIN
         XMINREC = MIN(XMINREC, SPA(I,1))
         XMAXREC = MAX(XMAXREC, SPA(I,1))
         YMINREC = MIN(YMINREC, SPA(I,2))
         YMAXREC = MAX(YMAXREC, SPA(I,2))
      END DO

C     Calculate crosswind width, WIDTH, and alongwind length, LENGTH
      WIDTH  = YMAXREC - YMINREC
      LENGTH = XMAXREC - XMINREC

      IF (AREADBG) THEN
         write(AREADBUNT,*)
         write(AREADBUNT,*) 'AREA Debug Sub_ARDIST: '
         write(AREADBUNT,*)
         IF (EVONLY) THEN
            write(AREADBUNT,*) ' AREA DEBUG for SRC: ',ISRC,
     &                                  ' and EVENT: ',indx
            write(AREADBUNT,*)
         ELSE
            write(AREADBUNT,*) ' AREA DEBUG for SRC: ',ISRC,
     &                                  '   and REC: ',indx
            write(AREADBUNT,*)
         END IF
         write(AREADBUNT,'(2x,"XMINREC, XMAXREC: ",2(F15.6))') XMINREC,
     &                                                         XMAXREC
         write(AREADBUNT,'(2x,"YMINREC, YMAXREC: ",2(F15.6))') YMINREC,
     &                                                         YMAXREC
         write(AREADBUNT,*)

         do i = 1, nvert+1
          write(AREADBUNT,'(a,i4,2(2x,F15.6))') '  IVERT, SPA1, SPA2: ',
     &                                            I, SPA(I,1), SPA(I,2)
         enddo
         write(AREADBUNT,*)
         write(AREADBUNT,*) '   WIDTH = ',width
         write(AREADBUNT,*) '  LENGTH = ',length
      endif

C     Determine downwind distance to use for AREADPLT option, XDEP
      IF (XMINREC .GE. 0.0D0) THEN
         XDEP = XMINREC + LENGTH/3.0D0
      ELSE
         XDEP = XMAXREC/3.0D0
      END IF

      XDEP = MAX( 1.0D0, XDEP )

C     Calculate Downwind (X) and Crosswind (Y) Distances from Center of Source;
C     X and Y are included in module MAIN1
      X = -((XR-XCNTR)*WDSIN + (YR-YCNTR)*WDCOS)
      Y =   (XR-XCNTR)*WDCOS - (YR-YCNTR)*WDSIN

C     Calculate Radial Distance from Center of Source;
C     DISTR is included in module MAIN1
      DISTR = DSQRT(X*X + Y*Y)

C     Calculate height of receptor above stack base, ZRT
      IF (L_FLATSRC(ISRC)) THEN
         ZRT = ZFLAG
      ELSE
         ZRT = ZELEV - ZS + ZFLAG
      END IF

      RETURN
      END

      SUBROUTINE OCALC
C***********************************************************************
C                 OCALC Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Calculates concentration or deposition values
C                 for OPENPIT sources
C
C        PROGRAMMER: Jayant Hardikar, Roger Brode
C        ADAPTED FROM:  SUBROUTINE ACALC
C
C        DATE:    July 19, 1994
C
C        MODIFIED:   To calculate conc*travel time for GRSM NO2 option.
C                    CERC, 11/30/20
C
C        MODIFIED:
C                    Corrected calculation of adjusted emission for the
C                    point source approximation under FASTAREA option
C                    to use length and width of effective area source
C                    rather than length and width of the openpit source.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
C
C                    Added arrays to save WDSIN and WDCOS by source for use
C                    with PVMRM option.  Corrects potential problem for
C                    PVMRM applications with multi-level wind inputs.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 01/26/2007
C
C                    Added call to subroutine HEFF for calculation
C                    of ZSUBP for deposition applications.
C                    R. W. Brode, U.S. EPA, OAQPS, AQMG, 01/24/2007
C
C        MODIFIED:   Modified to initialize HE = HS prior to first
C                    call to sub. ADISY.
C                    R. W. Brode, MACTEC (f/k/a PES), Inc., 07/05/05
C
C        MODIFIED:   Modified to include initialization of __VAL arrays
C                    at end of receptor loop.
C                    R. W. Brode, MACTEC (f/k/a PES), Inc., 10/26/04
C
C        MODIFIED:   To include tilted plume for point source
C                    approximation of particle emissions.
C                    R. W. Brode, MACTEC (f/k/a PES), Inc., 07/23/04
C
C        MODIFIED:   To move call to METINI up since AFV is needed for
C                    SUBROUTINE LWIND.
C                    R. W. Brode, PES Inc., - 1/22/98
C
C        MODIFIED:   To skip calculations if QPTOT = 0.0, avoiding
C                    zero divide error in SUB. AMFRAC.
C                    R. W. Brode, PES Inc., - 4/14/95
C
C        INPUTS:  Source Parameters for Specific Source
C                 Arrays of Receptor Locations
C                 Meteorological Variables for One Hour
C
C        OUTPUTS: Array of 1-hr CONC or DEPOS Values for Each Source/Receptor
C
C        CALLED FROM:   CALC
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      INTEGER :: I, II, J, ICAT, INOUT, NDXR
      DOUBLE PRECISION :: QPTOT, XVM(5), YVM(5), XDEP, WIDTH, LENGTH,
     &                    XMINR, XMAXR, QTKSAV, XPOINT, ADJ, FYOUT
      DOUBLE PRECISION :: AEROUT(NUMTYP)

C     Variable Initializations
      MODNAM = 'OCALC'
      WAKE = .FALSE.

C     Initialize __VAL arrays (1:NUMTYP)
      HRVAL   = 0.0D0
      AERVAL  = 0.0D0
      AEROUT  = 0.0D0
      PRMVAL  = 0.0D0
      IF( ALLOCATED(CHI) ) CHI(:,ISRC,:) = 0.0D0
      IF(GRSM)THEN
        CHI_TTRAVCHM(:,ISRC) = 0.0D0
      END IF

C     Obtain reference wind speed at 10-meters
C---- First locate index below 10.
      CALL LOCATE(GRIDHT, 1, MXGLVL, 10.0D0, NDXR)
      CALL GINTRP( GRIDHT(NDXR), GRIDWS(NDXR),
     &             GRIDHT(NDXR+1), GRIDWS(NDXR+1),
     &             10.0D0, UREF10 )

C     Set Mixing Height and Profiles for Urban Option if Needed
      IF (URBSRC(ISRC) .EQ. 'Y') THEN
C        Find Urban Area Index for This Source
         DO I = 1, NUMURB
            IF (IURBGRP(ISRC,I) .EQ. 1) THEN
               IURB = I
               EXIT
            END IF
         END DO
         IF (STABLE .OR. L_MorningTrans(IURB)) THEN
            URBSTAB = .TRUE.
            ZI = MAX( ZIURB(IURB), ZIMECH )
            GRIDSV = GRDSVU(1:MXGLVL,IURB)
            GRIDSW = GRDSWU(1:MXGLVL,IURB)
            GRIDTG = GRDTGU(1:MXGLVL,IURB)
            GRIDPT = GRDPTU(1:MXGLVL,IURB)
            OBULEN = URBOBULEN(IURB)
            USTAR  = URBUSTR(IURB)
         ELSE
            URBSTAB = .FALSE.
            ZI = ZIRUR
            GRIDSV = GRDSVR
            GRIDSW = GRDSWR
            GRIDTG = GRDTGR
            GRIDPT = GRDPTR
            OBULEN = RUROBULEN
            USTAR  = RURUSTR
         END IF
      ELSE IF (URBAN .AND. URBSRC(ISRC) .EQ. 'N') THEN
         URBSTAB = .FALSE.
         ZI = ZIRUR
         GRIDSV = GRDSVR
         GRIDSW = GRDSWR
         GRIDTG = GRDTGR
         GRIDPT = GRDPTR
         OBULEN = RUROBULEN
         USTAR  = RURUSTR
      ELSE
         URBSTAB = .FALSE.
      END IF

C     Set the Source Variables for This Source              ---   CALL SETSRC
      CALL SETSRC

C     Initialize meteorological variables                   ---   CALL METINI
      CALL METINI

C     Save WDSIN and WSCOS for later use by PVMRM/GRSM option
      AWDSIN(ISRC) = WDSIN
      AWDCOS(ISRC) = WDCOS
      AAFV(ISRC)   = AFV

C*    Initialize the Total Adjusted Emission Rate from
C*    All Particles
      QPTOT = 0.0D0

      IF (NPD .EQ. 0) THEN
C*       Assign input emission to QPTOT variable for gas emissions
         QPTOT = QS

      ELSE

C*       Loop over Particle Size Categories
         DO ICAT = 1,NPD
C*          Calculate the Escape Fraction for Each Category    ---   CALL ESCAPE
            CALL ESCAPE(ICAT)

C*          Adjust the Emission Rate for Each Category         ---   CALL ADJEMI
            CALL ADJEMI(ICAT,QPTOT)

C*       End Loop Over Particle Size Categories
         END DO

      END IF

C*    Skip Calculations if QPTOT = 0.0
      IF (QPTOT .EQ. 0.0D0)  RETURN

      IF (NPD .GT. 0) THEN
C*       Adjust the Mass Fractions for All the Particle
C*       Size Categories                                    ---   CALL AMFRAC
         CALL AMFRAC(QPTOT)
      END IF

C*    Determine the AlongWind Length of the OPENPIT Source  ---   CALL LWIND
      CALL LWIND

C*    Calculate the Relative Depth of the OPENPIT Source    ---   CALL PDEPTH
      CALL PDEPTH

C*    Calculate the Fractional Size of the
C*    Effective Pit Area (PITFRA)                           ---   CALL PTFRAC
      CALL PTFRAC

C*    If DEBUG Requested, Write Out Pit Info; include in AREA debug file if
C     specified, otherwise in the MODEL debug file
      IF (AREADBG) THEN
         WRITE (AREADBUNT,*)
         WRITE (AREADBUNT,*)
     &         'DETAIL INFORMATION ON THE OPENPIT SOURCE :', SRCID(ISRC)
         WRITE (AREADBUNT,*)
C*    WRITE DEBUG INFORMATION
      ELSEIF (DEBUG) THEN
         WRITE (DBGUNT,*)
         WRITE (DBGUNT,*) 'DETAIL INFORMATION ON THE OPENPIT SOURCE :',
     &                     SRCID(ISRC)
         WRITE (DBGUNT,*)
      END IF

C*    Determine the Coordinates of the Effective Pit Area
C*    in Wind Direction Coordinate System                   ---   CALL PITEFF
      CALL PITEFF

C*    Calculate the adusted Emission Rate per unit area (QEFF) for the
C*    Effective Pit Area (PITFRA)                           ---   CALL PITEMI
      CALL PITEMI(QPTOT)

C*    If DEBUG Requested, Write Out Pit Info; include in AREA debug file if
C     specified, otherwise in the MODEL debug file
      IF (AREADBG) THEN
         IF (NPD .GT. 0) THEN
            WRITE (AREADBUNT,*) 'OPENPIT PARTICLE CHARACTERISTICS:'
            WRITE (AREADBUNT,*) '--------------------------------'
            WRITE (AREADBUNT,*)
            WRITE (AREADBUNT,8000) (EFRAC(II),II = 1, NPD)
8000        FORMAT (1X,'ESCAPE FRACTIONS        = ',10(F8.3,2X))
            WRITE (AREADBUNT,8200) (QPART(II),II = 1, NPD)
8200        FORMAT (1X,'ADJUSTED EMISSION RATES = ',10(F8.3,2X))
            WRITE (AREADBUNT,8400) (PHI(II),II = 1, NPD)
8400        FORMAT (1X,'ADJUSTED MASS FRACTIONS = ',10(F8.3,2X))
         END IF
         WRITE (AREADBUNT,*) ' EMISSION RATE OF EFFECTIVE PIT = ',QEFF
         WRITE (AREADBUNT,*)
      ELSEIF (DEBUG) THEN
         IF (NPD .GT. 0) THEN
            WRITE (DBGUNT,*) 'OPENPIT PARTICLE CHARACTERISTICS:'
            WRITE (DBGUNT,*) '--------------------------------'
            WRITE (DBGUNT,*)
            WRITE (DBGUNT,8000) (EFRAC(II),II = 1, NPD)
            WRITE (DBGUNT,8200) (QPART(II),II = 1, NPD)
            WRITE (DBGUNT,8400) (PHI(II),II = 1, NPD)
         END IF
         WRITE (DBGUNT,*) ' EMISSION RATE OF EFFECTIVE PIT = ',QEFF
         WRITE (DBGUNT,*)
      END IF

C     Apply Variable Emission Rate Factors                  ---   CALL EMFACT
      CALL EMFACT(QEFF)

C     Initialize 'ARC' Arrays for EVALFILE Output           ---   CALL EVLINI
      IF (EVAL(ISRC)) THEN
         CALL EVLINI
      END IF

      IF ((QEFF.NE.0.0D0) .AND. (STABLE .OR. (HS.LE.ZI))) THEN
C        Initialize miscellaneous variables
         FB  = 0.0D0
         FM  = 0.0D0
         PPF = 0.0D0
         HSP = HS
         DHP  = 0.0D0
         DHP1 = 0.0D0
         DHP2 = 0.0D0
         DHP3 = 0.0D0
         DHCRIT = 0.0D0
         XFINAL = 0.0D0
         XMIXED = ZI * UAVG / SWAVG
         IF(XMIXED .LT. XFINAL) XMIXED = XFINAL
         ZMIDMX = 0.5D0 * ZI

C ---    Save original emission rate, which may be needed if RECLOOP is cycled
C        and point source approximation is used for FASTAREA or FASTALL options
         QTKSAV = QTK

C ---    Initialize PDF parameters for use in calculating ZSUBP
         IF( UNSTAB  .AND.  (HS .LT. ZI) ) THEN
            CALL PDF
         END IF
C        Set Dry Deposition Variables for this Source
         IF (LUSERVD .AND. LDGAS .AND. NPD.EQ.0) THEN
C           Assign user-specified gas dry deposition velocity (GASDEPVD option)
            VDEPG = USERVD
         ELSE IF (LDPART .OR. (.NOT.LUSERVD .AND. LDGAS .AND.
     &                                                   NPD.EQ.0)) THEN
C           Calculate Deposition Velocities for this Source    ---   CALL VDP
            CALL VDP
         END IF
         IF (LWPART .OR. LWGAS) THEN
CPES        Set value of ZSUBP = MAX( ZI, TOP OF PLUME ), where
CPES        TOP OF PLUME is defined as plume height (HE) plus 2.15*SZ,
CPES        evaluated at a distance of 20 kilometers downwind.
CPES        Apply minimum value of 500m and maximum value of 10,000m.
            CALL HEFF (20000.0D0)
            IF( STABLE .OR. (UNSTAB .AND. HS .GE. ZI) )THEN
               CALL SIGZ(20000.0D0)
               ZSUBP = MAX( 500.0D0, ZI, HS + SZCOEF*SZAS )
            ELSE IF (UNSTAB) THEN
               CALL SIGZ(20000.0D0)
               ZSUBP = MAX( 500.0D0, ZI, HS +
     &                                   SZCOEF*(SZAD1+SZAD2)/2.0D0 )
            END IF
            ZSUBP = MIN( 10000.0D0, ZSUBP )
C           Calculate Scavenging Ratios for this Source           ---   CALL SCAVRAT
            CALL SCAVRAT
         END IF

C        Begin Receptor LOOP
         RECEPTOR_LOOP: DO IREC = 1, NUMREC
C           Check for receptor located inside boundary of open pit source
            DO I = 1, NVERT+1
               XVM(I) = AXVERT(I,ISRC)
               YVM(I) = AYVERT(I,ISRC)
            END DO
            XR = AXR(IREC)
            YR = AYR(IREC)
            CALL PNPOLY(XR,YR,XVM,YVM,4,INOUT)

            IF (INOUT .GT. 0) THEN
C              Receptor is within boundary - skip to next receptor

               IF (PVMRM .OR. OLM .OR. ARM2 .OR. GRSM) THEN
C ---             Assign 0.0 to CHI(IREC,ISRC,ITYP) before
C                 cycling to the next receptor to avoid
C                 persisting value from previous hour.
                  CHI(IREC,ISRC,:)   = 0.0D0
                  IF(GRSM)THEN
                    CHI_TTRAVCHM(IREC,ISRC) = 0.0D0
                  END IF
                  IF (PVMRM .OR. GRSM) THEN
                     HECNTR(IREC,ISRC)  = 0.0D0
                     UEFFS(IREC,ISRC)   = 0.0D0
                     EPSEF(IREC,ISRC)   = 0.0D0
                     PPFACT(IREC,ISRC)  = 0.0D0
                     HECNTR3(IREC,ISRC) = 0.0D0
                     UEFF3S(IREC,ISRC)  = 0.0D0
                     EPSEF3(IREC,ISRC)  = 0.0D0
                     FOPTS(IREC,ISRC)   = 0.0D0
                  END IF
               END IF
C              Also reinitialize HRVAL, AERVAL and AEROUT
               HRVAL(:)  = 0.0D0
               AERVAL(:) = 0.0D0
               AEROUT(:) = 0.0D0

C ---          Reset QTK since it may have been changed if
C              point source approximation has been used
               QTK = QTKSAV

               CYCLE RECEPTOR_LOOP
            END IF

C           Calculate Down and Crosswind Distances          ---   CALL ARDIST
            IF (EVONLY) THEN
               CALL ARDIST(IEVENT,XDEP,WIDTH,LENGTH,XMINR,XMAXR)
            ELSE
               CALL ARDIST(IREC,XDEP,WIDTH,LENGTH,XMINR,XMAXR)
            END IF

C           Check to see if receptor is upwind of area source;
C           also check to see if receptor is beyond the maximum
C           distance from source; assigned value of 80km for
C           the obsolescent TOXICS option or new FASTALL option;
C           otherwise "unlimited" (1.0D20)
            IF (XMAXR .LT. 1.0D0 .OR. DISTR .GT. MAXDIST) THEN
               IF (PVMRM .OR. OLM .OR. ARM2
     &            .OR. RUNTTRM .OR. GRSM) THEN
C ---             Assign 0.0 to CHI(IREC,ISRC,ITYP) before
C                 cycling to the next receptor to avoid
C                 persisting value from previous hour.
                  CHI(IREC,ISRC,:) = 0.0D0
                  IF(GRSM)THEN
                    CHI_TTRAVCHM(IREC,ISRC) = 0.0D0
                  END IF
                  IF (PVMRM .OR. GRSM) THEN
                     HECNTR(IREC,ISRC)  = 0.0D0
                     UEFFS(IREC,ISRC)   = 0.0D0
                     EPSEF(IREC,ISRC)   = 0.0D0
                     PPFACT(IREC,ISRC)  = 0.0D0
                     HECNTR3(IREC,ISRC) = 0.0D0
                     UEFF3S(IREC,ISRC)  = 0.0D0
                     EPSEF3(IREC,ISRC)  = 0.0D0
                     FOPTS(IREC,ISRC)   = 0.0D0
                  END IF
               END IF
C              Also reinitialize HRVAL, AERVAL and AEROUT
               HRVAL(:)  = 0.0D0
               AERVAL(:) = 0.0D0
               AEROUT(:) = 0.0D0

C ---          Reset QTK since it may have been changed if
C              point source approximation has been used
               QTK = QTKSAV

               CYCLE RECEPTOR_LOOP
            END IF

C           Initialize HE for initial call to ADISY
            HE = HS
C           Set initial effective parameters
            UEFF  = US
            SVEFF = SVS
            SWEFF = SWS
            TGEFF = TGS
            IF ( UNSTAB  .AND.  (HS .LT. ZI) ) THEN
               UEFFD  = US
               SVEFFD = SVS
               SWEFFD = SWS
               UEFFN  = US
               SVEFFN = SVS
               SWEFFN = SWS
            END IF

C ---       Check to see if receptor is beyond edge of plume laterally.
            IF ( (DABS(Y)-0.5D0*WIDTH) .GT. 0.0D0) THEN
C ---          Receptor is outside "projected" width of effective area source
C              Calculate maximum sigma-y based on XMAXR and compare minimum
C              lateral distance of receptor from source to 4*sigma-y
               CALL ADISY(XMAXR)

               IF ((DABS(Y)-0.5D0*WIDTH) .GE. 5.0D0*SY) THEN
C ---             Receptor is beyond the edge of plume; set CHI = 0.0
C                 and cycle to next receptor
                  IF (PVMRM .OR. OLM .OR. ARM2
     &               .OR. RUNTTRM .OR. GRSM) THEN
C ---                Assign 0.0 to CHI(IREC,ISRC,ITYP) before
C                    cycling to the next receptor to avoid
C                    persisting value from previous hour.
                     CHI(IREC,ISRC,:) = 0.0D0
                     IF(GRSM)THEN
                       CHI_TTRAVCHM(IREC,ISRC) = 0.0D0
                     END IF
                     IF (PVMRM .OR. GRSM) THEN
                        HECNTR(IREC,ISRC)  = 0.0D0
                        UEFFS(IREC,ISRC)   = 0.0D0
                        EPSEF(IREC,ISRC)   = 0.0D0
                        PPFACT(IREC,ISRC)  = 0.0D0
                        HECNTR3(IREC,ISRC) = 0.0D0
                        UEFF3S(IREC,ISRC)  = 0.0D0
                        EPSEF3(IREC,ISRC)  = 0.0D0
                        FOPTS(IREC,ISRC)   = 0.0D0
                     END IF
                  END IF
C                 Also reinitialize HRVAL, AERVAL and AEROUT
                  HRVAL(:)  = 0.0D0
                  AERVAL(:) = 0.0D0
                  AEROUT(:) = 0.0D0

C ---             Reset QTK since it may have been changed if
C                 point source approximation has been used
                  QTK = QTKSAV

C ---             Cycle to next receptor
                  CYCLE RECEPTOR_LOOP
               END IF

            END IF

            IF(DEBUG) THEN
              WRITE(DBGUNT, 6015) UEFF, SVEFF, SWEFF
6015            FORMAT(//,'AERMOD OPENPIT SOURCE COMPONENT',/,5X,/
     &          'Initial effective parameters for ',
     &          'stable or direct convective ',
     &          'plume:',//,5x,'Ueff = ',F7.2,' m/s; ',
     &          'SVeff = ',F7.2,
     &          ' m/s; SWeff = ',F7.2,' m/s.',/)
            END IF

C           Determine the CRITical Dividing Streamline---   CALL CRITDS
            CALL CRITDS (HE)

C           Set distance factor for point source approx. for FASTAREA option
C           based on "equivalent" PG stability class (KST)
            IF (URBSTAB) THEN
               VP_FACT = VIRTPNT_URB(KST)
            ELSE
               VP_FACT = VIRTPNT_RUR(KST)
            END IF

C           Calculate distance for switch to point source approx. for FASTAREA
            XPOINT = 1.5D0*LENGTH + VP_FACT*WIDTH
            IF ( (.NOT.FASTAREA .AND. .NOT.FASTALL) .OR.
     &           ((FASTAREA .OR. FASTALL) .AND. X .LT. XPOINT)) THEN
               IF (ARDPLETE) THEN

                  IF (NPD .EQ. 0 .AND. (LDGAS .OR. LWGAS)) THEN
                     CALL PDEPG (XDEP)
                  ELSE
                     DQCORG = 1.0D0
                     WQCORG = 1.0D0
                  END IF
                  IF (NPD .GT. 0 .AND. (LDPART .OR. LWPART)) THEN
                     CALL PDEP (XDEP)
                  ELSE IF (NPD .GT. 0) THEN
C                    Set DQCOR(NPD) and WQCOR(NPD) arrays to 1.0
                     DQCOR = 1.0D0
                     WQCOR = 1.0D0
                  END IF

               END IF

               DO ITYP = 1, NUMTYP
C                 Calculate Area Source Integral         ---   CALL AREAIN
                  CALL AREAIN
               END DO
            ELSE
C ---          Use point source approximation
C              Save emissions per unit area (QTK) and calculate total emissions
C              based on effective area (= XEFF*YEFF)
               QTKSAV = QTK
               QTK = QTK * XEFF * YEFF
               SYINIT = 0.0D0

C              Determine Deposition Correction Factors
               IF (NPD .EQ. 0 .AND. (LDGAS .OR. LWGAS)) THEN
                  CALL PDEPG (X)
               ELSE
                  DQCORG = 1.0D0
                  WQCORG = 1.0D0
               END IF
               IF (NPD .GT. 0 .AND. (LDPART .OR. LWPART)) THEN
                  CALL PDEP (X)
               ELSE IF (NPD .GT. 0) THEN
C                 Set DQCOR(NPD) and WQCOR(NPD) arrays to 1.0
                  DQCOR = 1.0D0
                  WQCOR = 1.0D0
               END IF

C              Define plume centroid height (CENTER) for use in
C              inhomogeniety calculations
               CALL CENTROID (X)

C              If the atmosphere is unstable and the stack
C              top is below the mixing height, calculate
C              the CBL PDF coefficients                     ---   CALL PDF
               IF( UNSTAB  .AND.  (HS .LT. ZI) ) THEN
                  CALL PDF
               END IF

C              Determine Effective Plume Height             ---   CALL HEFF
               CALL HEFF ( X )

C              Compute effective parameters using an
C              iterative average through plume rise layer
               CALL IBLVAL (X)

C              Call PDF & HEFF again for final CBL plume heights
               IF (UNSTAB .AND. (HS.LT.ZI) ) THEN
                  CALL PDF
                  CALL HEFF (X)
               END IF

C              Determine Dispersion Parameters              ---   CALL VDIS
               CALL VDIS (X)

C              Calculate the 'y-term' contribution to
C              dispersion, FSUBY, for coherent plume        ---   CALL FYPLM
               CALL FYPLM(SY,FYOUT)
               FSUBY  = FYOUT
               FSUBYD = FSUBY
               FSUBYN = FSUBYD

C              Set lateral term = 0.0 for penetrated source
               FSUBY3 = 0.0D0

C              Check for zero "y-terms"; if zero then skip calculations
C              and go to next receptor.
               IF( FSUBY.EQ.0.0D0 .AND. FSUBY3.EQ.0.0D0 )THEN
C                 FSUBY terms are both 0.0; assign 0.0 to arrays
                  IF (PVMRM .OR. OLM .OR. ARM2 .OR. GRSM)THEN
C ---                Assign 0.0 to CHI(IREC,ISRC,ITYP) before
C                    cycling to the next receptor to avoid
C                    persisting value from previous hour.
                     CHI(IREC,ISRC,:) = 0.0D0
                     IF(GRSM)THEN
                       CHI_TTRAVCHM(IREC,ISRC) = 0.0D0
                     END IF
                     IF (PVMRM .OR. GRSM) THEN
                        HECNTR(IREC,ISRC)  = 0.0D0
                        UEFFS(IREC,ISRC)   = 0.0D0
                        EPSEF(IREC,ISRC)   = 0.0D0
                        PPFACT(IREC,ISRC)  = 0.0D0
                        HECNTR3(IREC,ISRC) = 0.0D0
                        UEFF3S(IREC,ISRC)  = 0.0D0
                        EPSEF3(IREC,ISRC)  = 0.0D0
                        FOPTS(IREC,ISRC)   = 0.0D0
                     END IF
                  END IF
C                 Also reinitialize HRVAL, AERVAL and AEROUT
                  HRVAL(:)  = 0.0D0
                  AERVAL(:) = 0.0D0
                  AEROUT(:) = 0.0D0

C ---             Reset QTK since it may have been changed if
C                 point source approximation has been used
                  QTK = QTKSAV

                  CYCLE RECEPTOR_LOOP
               ELSE

                  IF (NPD .EQ. 0) THEN
C                    Perform calculations for gases
C                    Assign plume tilt, HV = 0.0

                     ADJ = DQCORG * WQCORG

                     IF (STABLE .OR. (UNSTAB.AND.(HS.GE.ZI))) THEN
C                       Calculate height of the "effective reflecting surface"
                        CALL REFL_HT (HE, X, 0.0D0, VSIGZ, HSBL)
                     ELSE IF ( UNSTAB ) THEN
                        HSBL = 0.0D0
                     END IF

C                    Determine the CRITical Dividing Streamline---   CALL CRITDS
                     CALL CRITDS (HE)

C                    Calculate the fraction of plume below
C                    HCRIT, PHEE                               ---   CALL PFRACT
                     CALL PFRACT (HE)

C                    Calculate FOPT = f(PHEE)                  ---   CALL FTERM
                     CALL FTERM

C                    Calculate Conc. for Virtual Point Source  ---   CALL AER_PCHI
                     CALL AER_PCHI( X, ADJ, VDEPG, 0, AEROUT )
                     HRVAL = AEROUT

                  ELSE
C                    Perform calculations for particles, loop through particle sizes

C                    Begin loop over particle sizes
                     DO J = 1, NPD

C                       Calculate Plume Tilt Due to Settling, HV
                        HV = (X/US) * VGRAV(J)

C                       Adjust Jth contribution by mass fraction and source
C                       depletion
                        ADJ = PHI(J) * DQCOR(J) * WQCOR(J)

                        IF (STABLE .OR. (UNSTAB.AND.(HS.GE.ZI))) THEN
C                          Calculate height of the "effective reflecting surface"
C                          Calculate Settled Plume Height(s), HESETL
                           HESETL = MAX( 0.0D0, HE - HV )
                           CALL REFL_HT (HESETL, X, 0.0D0, VSIGZ, HSBL)
                        ELSE IF ( UNSTAB ) THEN
C                          Calculate Settled Plume Height(s), HESETL
                           HESETL = MAX( 0.0D0, 0.5D0*(HED1+HED2) - HV )
                           HSBL = 0.0D0
                        END IF

C                       Determine the CRITical Dividing Streamline---   CALL CRITDS
                        CALL CRITDS (HESETL)

C                       Calculate the fraction of plume below
C                       HCRIT, PHEE                               ---   CALL PFRACT
                        CALL PFRACT (HESETL)

C                       Calculate FOPT = f(PHEE)                  ---   CALL FTERM
                        CALL FTERM

C                       Calculate Conc. for Virtual Point Source  ---   CALL AER_PCHI
                        CALL AER_PCHI( X, ADJ, VDEP(J), J, AEROUT )
                        HRVAL = HRVAL + AEROUT

                     END DO
C                    End loop over particle sizes

                  END IF
               END IF
C ---          Reset QTK since it may have been changed if
C              point source approximation has been used
               QTK = QTKSAV
            END IF

            IF (PVMRM) THEN
C ---          Store data by source and receptor for PVMRM option
               DO ITYP = 1, NUMTYP
                  CHI(IREC,ISRC,ITYP) = HRVAL(ITYP)
               END DO
               IF (STABLE .OR. (UNSTAB.AND.(HS.GE.ZI))) THEN
                  HECNTR(IREC,ISRC) = HE
                  UEFFS(IREC,ISRC)  = UEFF
                  EPSEF(IREC,ISRC)  = EPSEFF
               ELSE
                  HECNTR(IREC,ISRC) = CENTER
                  UEFFS(IREC,ISRC)  = UEFFD
                  EPSEF(IREC,ISRC)  = EPSEFFD
               END IF
               IF (PPF .GT. 0.0D0) THEN
                  PPFACT(IREC,ISRC)  = PPF
                  HECNTR3(IREC,ISRC) = HE3
                  UEFF3S(IREC,ISRC)  = UEFF3
                  EPSEF3(IREC,ISRC)  = EPSEFF3
               ELSE
                  PPFACT(IREC,ISRC)  = 0.0D0
                  HECNTR3(IREC,ISRC) = 0.0D0
                  UEFF3S(IREC,ISRC)  = 0.0D0
                  EPSEF3(IREC,ISRC)  = 0.0D0
               END IF
               FOPTS(IREC,ISRC) = FOPT

C              Initialize __VAL arrays (1:NUMTYP)
               HRVAL(:)  = 0.0D0
               AERVAL(:) = 0.0D0
               AEROUT(:) = 0.0D0

C              Cycle to next receptor & skip call to SUMVAL (will be done later)
               CYCLE RECEPTOR_LOOP

            ELSE IF (OLM) THEN
C ---          Store data by source and receptor for OLM option
               DO ITYP = 1, NUMTYP
                  CHI(IREC,ISRC,ITYP) = HRVAL(ITYP)
               END DO

C              Initialize __VAL arrays (1:NUMTYP)
               HRVAL(:)  = 0.0D0
               AERVAL(:) = 0.0D0
               AEROUT(:) = 0.0D0

C              Cycle to next receptor & skip call to SUMVAL (will be done later)
               CYCLE RECEPTOR_LOOP
!    Added for TTRM; AECOM
            ELSE IF (RUNTTRM) THEN
C ---          Store data by source and receptor for TTRM option
               DO ITYP = 1, NUMTYP
                  CHI(IREC,ISRC,ITYP) = HRVAL(ITYP)
               END DO

C              Initialize __VAL arrays (1:NUMTYP)
               HRVAL(:)  = 0.0D0
               AERVAL(:) = 0.0D0
               AEROUT(:) = 0.0D0

C              Cycle to next receptor & skip call to SUMVAL (will be done later)
               CYCLE RECEPTOR_LOOP
!    End TTRM insert, Feb. 2021

            ELSE IF (ARM2) THEN
C ---          Store data by source and receptor for ARM2 options
               DO ITYP = 1, NUMTYP
                  CHI(IREC,ISRC,ITYP) = HRVAL(ITYP)
               END DO

C              Initialize __VAL arrays (1:NUMTYP)
               HRVAL(:)  = 0.0D0
               AERVAL(:) = 0.0D0
               AEROUT(:) = 0.0D0

C              Cycle to next receptor & skip call to SUMVAL (will be done later)
               CYCLE RECEPTOR_LOOP

C           CERC 11/30/20
            ELSE IF (GRSM) THEN

C              Calculate the effective wind speed
               IF (STABLE .OR. (UNSTAB.AND.(HS.GE.ZI))) THEN
                 UCHM=UEFF
               ELSE
                 UCHM=UEFFD
               END IF
C              Calculate the conc x travel time for the chemistry scheme
               IF(UCHM/=0)THEN
                 CHI_TTRAVCHM(IREC,ISRC)=
     &                     MAX(0.0D0,HRVAL(1)*X/UCHM) !Don't allow negative conc*travel times
               ELSE
                 !Error - no travel time if UEFF is zero
                 CALL ERRHDL(PATH, MODNAM, 'E','601','GRSM')
                 RUNERR=.TRUE.
                 RETURN
               END IF

C ---          Store data by source and receptor for GRSM options
               DO ITYP = 1, NUMTYP
                  CHI(IREC,ISRC,ITYP) = HRVAL(ITYP)
               END DO

               IF (STABLE .OR. (UNSTAB.AND.(HS.GE.ZI))) THEN
                  HECNTR(IREC,ISRC) = HE
                  UEFFS(IREC,ISRC)  = UEFF
                  EPSEF(IREC,ISRC)  = EPSEFF
               ELSE
                  HECNTR(IREC,ISRC) = CENTER
                  UEFFS(IREC,ISRC)  = UEFFD
                  EPSEF(IREC,ISRC)  = EPSEFFD
               END IF
               IF (PPF .GT. 0.0D0) THEN
                  PPFACT(IREC,ISRC)  = PPF
                  HECNTR3(IREC,ISRC) = HE3
                  UEFF3S(IREC,ISRC)  = UEFF3
                  EPSEF3(IREC,ISRC)  = EPSEFF3
               ELSE
                  PPFACT(IREC,ISRC)  = 0.0D0
                  HECNTR3(IREC,ISRC) = 0.0D0
                  UEFF3S(IREC,ISRC)  = 0.0D0
                  EPSEF3(IREC,ISRC)  = 0.0D0
               END IF
               FOPTS(IREC,ISRC) = FOPT

C              Initialize __VAL arrays (1:NUMTYP)
               HRVAL(:)  = 0.0D0
               AERVAL(:) = 0.0D0
               AEROUT(:) = 0.0D0

C              Cycle to next receptor & skip call to SUMVAL (will be done later)
               CYCLE RECEPTOR_LOOP

            END IF

C           Sum HRVAL to AVEVAL and ANNVAL Arrays  ---   CALL SUMVAL
C           As noted just above, the calls to EV_SUMVAL and SUMVAL
C           are skipped for a PVMRM, OLM, ARM2 or GRSM run; the
C           summing is performed for these options in the
C           respective routines
            IF (EVONLY) THEN
               CALL EV_SUMVAL
            ELSE
               DO IGRP = 1, NUMGRP
                  CALL SUMVAL
               END DO
            END IF

C           Initialize __VAL arrays (1:NUMTYP)
            HRVAL(:)  = 0.0D0
            AERVAL(:) = 0.0D0
            AEROUT(:) = 0.0D0

         END DO RECEPTOR_LOOP
C        End Receptor LOOP

C        Output 'ARC' Values for EVALFILE                   ---   CALL EVALFL
         IF (EVAL(ISRC)) THEN
            CALL EVALFL
         END IF

      END IF

      RETURN
      END


      SUBROUTINE SETSRC
C***********************************************************************
C             SETSRC Module of the AMS/EPA Regulatory Model - AERMOD
c ----------------------------------------------------------------------
c ---    ISC-PRIME     Version 1.0    Level 970812              Modified
c ---        D. Strimaitis
c ---        Earth Tech, Inc.
c            Prepared for EPRI under contract WO3527-01
c ----------------------------------------------------------------------
C
C        PURPOSE: Sets the Source Parameters for a Particular Source
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To initialize AREA source dimensions and initial
C                    sigmas to correct initialization problems with PVMRM.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
C
C                    To incorporate factor to adjust initial diameter of
C                    plume for capped stacks for use in the PRIME
C                    algorithm, for BETA-test draft option.
C                    R. W. Brode, U.S. EPA/OAQPS/AQMG, 12/07/06
C
C        MODIFIED:   To incorporate inputs for numerical integration
C                    algorithm for AREA source - 7/7/93
C
C        INPUTS:  Source Parameters Arrays
C                 Source Index
C
C        OUTPUTS: Source Parameters for a Particular Source
C
C        CALLED FROM:   PCALC
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: J

C     Variable Initializations
      MODNAM = 'SETSRC'

C --- Initialize AREA source dimensions and initial sigmas
C     to avoid initialization problems with PVMRM
      XINIT  = 0.0D0
      YINIT  = 0.0D0
      SYINIT = 0.0D0
      SZINIT = 0.0D0
      ANGLE  = 0.0D0
      PALPHA = 0.0D0
      PDEFF  = 0.0D0
      PITLEN = 0.0D0
      PITWID = 0.0D0

C     Assign The Values From Array Elements To Variables
      IF (SRCTYP(ISRC)(1:5) .EQ. 'POINT') THEN
         XS = AXS(ISRC)
         YS = AYS(ISRC)
         ZS = AZS(ISRC)
         QS = AQS(ISRC)
         HS = AHS(ISRC)

         DS = ADS(ISRC)
         VS = AVS(ISRC)
         TS = ATS(ISRC)

         IF (SRCTYP(ISRC) .EQ. 'POINTCAP') THEN
C           Assign factor to adjust initial diameter of plume for
C           capped stacks for use in PRIME algorithm.
            DSFACT = ADSFACT(ISRC)
         ELSE
            DSFACT = 1.0D0
         END IF

      ELSE IF (SRCTYP(ISRC) .EQ. 'VOLUME') THEN
         XS = AXS(ISRC)
         YS = AYS(ISRC)
         ZS = AZS(ISRC)
         QS = AQS(ISRC)
         HS = AHS(ISRC)

         SYINIT = ASYINI(ISRC)
         SZINIT = ASZINI(ISRC)

      ELSE IF (SRCTYP(ISRC) .EQ. 'AREA' .OR.
     &         SRCTYP(ISRC) .EQ. 'LINE') THEN
         XS = AXS(ISRC)
         YS = AYS(ISRC)
         ZS = AZS(ISRC)
         QS = AQS(ISRC)
         HS = AHS(ISRC)

         XINIT = AXINIT(ISRC)
         YINIT = AYINIT(ISRC)
         ANGLE = AANGLE(ISRC)

         SZINIT = ASZINI(ISRC)
         NVERT  = 4

C        Store Vertices in Temporary Arrays
         DO IVERT = 1, NVERT+1
            XVERT(IVERT) = AXVERT(IVERT,ISRC)
            YVERT(IVERT) = AYVERT(IVERT,ISRC)
         END DO

         XCNTR = AXCNTR(ISRC)
         YCNTR = AYCNTR(ISRC)

      ELSE IF (SRCTYP(ISRC) .EQ. 'AREAPOLY') THEN
         XS = AXS(ISRC)
         YS = AYS(ISRC)
         ZS = AZS(ISRC)
         QS = AQS(ISRC)
         HS = AHS(ISRC)

         SZINIT = ASZINI(ISRC)
         NVERT  = NVERTS(ISRC)

C        Store Vertices in Temporary Arrays
         DO IVERT = 1, NVERT+1
            XVERT(IVERT) = AXVERT(IVERT,ISRC)
            YVERT(IVERT) = AYVERT(IVERT,ISRC)
         END DO

C        Assign equivalent values of XINIT and YINIT for calculating area
         XINIT = AXINIT(ISRC)
         YINIT = AYINIT(ISRC)

C        Assign centroid of polygon
         XCNTR = AXCNTR(ISRC)
         YCNTR = AYCNTR(ISRC)

      ELSE IF (SRCTYP(ISRC) .EQ. 'AREACIRC') THEN
         XS = AXS(ISRC)
         YS = AYS(ISRC)
         ZS = AZS(ISRC)
         QS = AQS(ISRC)
         HS = AHS(ISRC)

         SZINIT = ASZINI(ISRC)
         NVERT  = NVERTS(ISRC)

C        Store Vertices in Temporary Arrays
         DO IVERT = 1, NVERT+1
            XVERT(IVERT) = AXVERT(IVERT,ISRC)
            YVERT(IVERT) = AYVERT(IVERT,ISRC)
         END DO

C        Assign equivalent values of XINIT and YINIT for calculating area
         XINIT = AXINIT(ISRC)
         YINIT = AYINIT(ISRC)

         XCNTR = AXCNTR(ISRC)
         YCNTR = AYCNTR(ISRC)

      ELSE IF (SRCTYP(ISRC) .EQ. 'OPENPIT') THEN
         XS = AXS(ISRC)
         YS = AYS(ISRC)
         ZS = AZS(ISRC)
         QS = AQS(ISRC)
C        Set Emission Height of Effective Area, HS = 0.0
         HS = 0.0D0
C        Set Height of Emissions Above Base of Pit, EMIHGT
         EMIHGT = AHS(ISRC)
         NVERT  = 4

         XINIT = AXINIT(ISRC)
         YINIT = AYINIT(ISRC)
         ANGLE = AANGLE(ISRC)
         PALPHA = AALPHA(ISRC)
         PDEFF  = APDEFF(ISRC)
         SZINIT = ASZINI(ISRC)
         PITLEN = MAX(XINIT,YINIT)
         PITWID = MIN(XINIT,YINIT)

C        Store Vertices in Temporary Arrays
         DO IVERT = 1, NVERT+1
            XVERT(IVERT) = AXVERT(IVERT,ISRC)
            YVERT(IVERT) = AYVERT(IVERT,ISRC)
         END DO

         XCNTR = AXCNTR(ISRC)
         YCNTR = AYCNTR(ISRC)

      ELSE IF (SRCTYP(ISRC) .EQ. 'BUOYLINE') THEN
         QS = AQS(ISRC)

C     Added for sidewash
      ELSE IF (SRCTYP(ISRC) .EQ. 'SWPOINT') THEN
         XS = AXS(ISRC)
         YS = AYS(ISRC)
         ZS = AZS(ISRC)
         QS = AQS(ISRC)
         HS = AHS(ISRC)
         BW = ABW(ISRC)
         BL = ABL(ISRC)
         BH = ABH(ISRC)
         BA = ABA(ISRC)

      END IF

      NPD = INPD(ISRC)
      IF (NPD .GT. 0) THEN
         DO J = 1, NPD
            PDIAM(J) = APDIAM(J,ISRC)
            PHI(J)   = APHI(J,ISRC)
            PDENS(J) = APDENS(J,ISRC)
            VGRAV(J) = AVGRAV(J,ISRC)
            TSTOP(J) = ATSTOP(J,ISRC)
         END DO
      END IF

C --- Initialize SURFAC for this source
      IF( HS .LT. 0.1D0 * ZI )THEN
          SURFAC = .TRUE.
      ELSE
          SURFAC = .FALSE.
      END IF

      RETURN
      END

      SUBROUTINE FLUXES(VSEQ)
C***********************************************************************
C             FLUXES Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Calculates the source momentum and buoyancy fluxes
C
C        PROGRAMMER: Roger Brode and Jim Paumier, PES, Inc.
C
C        DATE:    September 30, 1993
C
C        MODIFIED:
C                  To incorporate capped stack (POINTCAP) and
C                  horizontal release (POINTHOR) options.
C                  R. W. Brode, MACTEC (f/k/a PES), Inc., 09/30/05
C                  R. W. Brode, U.S. EPA/OAQPS/AQMG, 12/07/06
C
C        INPUTS:  Ambient temperature at source height, TA
C                 Source gas exit temperature, TS
C                 Source gas exit velocity, VS
C                 Source diameter, DS
C
C        OUTPUTS: Momentum flux, FM, and buoyancy flux, FB
C
C        CALLED FROM:   PCALC
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      DOUBLE PRECISION  TSEQ, DSEQ, VSEQ

C     Variable Initializations
      MODNAM = 'FLUXES'

C     Note:  TA is now ambient temperature AT STACK HEIGHT and
C            was computed in METINI

C     Check for Negative Stack Temperature, Used to
C     Indicate Constant TS-TA
      IF (TS .LT. 0.0D0) THEN
         TS = TA + DABS(TS)
      END IF

      IF (TS .LT. TA)  TS = TA
      FB = (0.25D0 / TS) * (VS * DS * DS) * G * (TS - TA)
      FM = (0.25D0 / TS) * (VS * DS * DS) * VS * TA

C     Set effective exit velocity, diameter and momentum flux for
C     capped stack (POINTCAP) or horizontal release (POINTHOR).
      IF (SRCTYP(ISRC) .EQ. 'POINTCAP') THEN
         VSEQ = 0.001D0
         DSEQ = DS * DSQRT( VS/VSEQ )
         TSEQ = TS
         FM = (0.25D0 / TSEQ) * (VSEQ * DSEQ * DSEQ) * VSEQ * TA
      ELSE IF (SRCTYP(ISRC) .EQ. 'POINTHOR') THEN
         VSEQ = 0.001D0
         DSEQ = DS * DSQRT( VS/VSEQ )
         TSEQ = TS
         FM = (0.25D0 / TSEQ) * (VSEQ * DSEQ * DSEQ) * VSEQ * TA
      ELSE
         VSEQ = VS
         DSEQ = DS
         TSEQ = TS
      END IF

C     To avoid divide by zero or underflow, set FB and FM to a minimum value
      IF( FB .LT. 1.0D-10 ) FB = 1.0D-10
      IF( FM .LT. 1.0D-10 ) FM = 1.0D-10

      RETURN
      END

      SUBROUTINE HEFF ( XARG )
C***********************************************************************
C             HEFF Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Calculates Effective Plume Height (m)
C
C        PROGRAMMER: Roger Brode and Jim Paumier, PES, Inc.
C
C        DATE:    September 30, 1993
C
C        REVISIONS:  Corrected to include plume rise for penetrated
C                    source, DHP3, for purposes of calculating plume
C                    height at 20km for use in wet deposition/depletion
C                    calculations.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, MM/DD/2013
C
C                    Corrected formulations for HEN1 & HEN2 per
C                    Model Formulation Document and conversation
C                    with Russ Lee and Jeff Weil.
C                    Roger Brode, PES, Inc. - 12/7/94
C
C        INPUTS:  Arrays of Source Parameters
C                 Logical Wake Flags
C                 Meteorological Variables for One Hour
C                 Wind Speed Adjusted to Stack Height
C                 Downwind Distance
C                 Terrain Elevation of Receptor
C
C        OUTPUTS: Effective Plume Height (HE)
C
C        CALLED FROM:   PCALC, VCALC, ACALC, PLUMEF
C
C   References:   "A Dispersion Model for the Convective Boundary
C                  Layer", J. Weil, 8/17/93
C                 "Plume Penetration of the CBL and Source 3: Source
C                  Strength and Plume Rise", J. Weil, 9/1/93
C
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      DOUBLE PRECISION :: XARG

C     Variable Initializations
      MODNAM = 'HEFF'

C     Compute the effective plume height
      IF( STABLE  .OR.  (UNSTAB .AND. (HS .GE. ZI) ) )THEN
C        The atmosphere is stable or the release is above the CBL
C        mixing ht.
         HE = HSP + DHP
C        Don't Allow Effective Plume Height to be < 0.0
         HE = MAX( 0.0D0, HE)

         HED1 = 0.0D0
         HED2 = 0.0D0
         HEN1 = 0.0D0
         HEN2 = 0.0D0
         HE3  = 0.0D0

CCRT     D063 Platform Downwash Debug
         IF (PLATFMDBG .AND. OSPLAT(ISRC)) THEN
            WRITE(PLATFMDBUNT,'(A, 1(2X, A, F12.3), 3(2X, A, F7.3))')
     &         'calc1.f/HEFF(STABLE): ',
     &         'XARG = ', XARG,
     &         'HSP = ', HSP,
     &         'DHP = ', DHP,
     &         'HE = ', HE
         END IF

      ELSEIF( UNSTAB )THEN
C        The atmosphere is unstable and the release is below the
C        mixing ht.

C ---    Set HE = 0.0 for unstable conditions
         HE = 0.0D0

C        Compute the effective direct plume height, for both the
C        Plume 1 (HED1) and Plume 2 (HED2)
         HED1 = HSP + DHP1 + (ASUB1 * WSTAR * XARG / UEFFD)
         HED2 = HSP + DHP1 + (ASUB2 * WSTAR * XARG / UEFFD)

C        Compute the effective indirect plume height, for both the
C        updraft (HEN1) and downdraft (HEN2)

         HEN1 = HSP + DHP1 - DHP2 + (ASUB1 * WSTAR * XARG / UEFFN)
         HEN2 = HSP + DHP1 - DHP2 + (ASUB2 * WSTAR * XARG / UEFFN)

C ---    Include calculation of penetrated plume rise and height
         IF( PPF .GT. 0.0D0 )THEN

C           Compute the plume height for the penetrated source
C           (See Eq. 8 in the reference for Source 3)
            IF (PPF .EQ. 1.0D0) THEN
               DHP3 = HEDHH * (ZI-HSP)
            ELSE
               DHP3 = 0.75D0 * (ZI-HSP) * HEDHH + 0.5D0 * (ZI-HSP)
            END IF

CMGS        D063 Platform Downwash
            IF (OSPLAT(ISRC))THEN
              HE3 = HSP + DHP3 - DHP3PLAT
            ELSE
              HE3 = HSP + DHP3
            END IF

         ELSE
            DHP3 = 0.0D0
            HE3  = 0.0D0

         END IF

CCRT     D063 Platform Downwash Debug
         IF (PLATFMDBG .AND. OSPLAT(ISRC)) THEN
            WRITE(PLATFMDBUNT,'(A, 1(2X, A, F12.3), 12(2X, A, F7.3))')
     &         'calc1.f/HEFF(UNSTABLE): ',
     &         'XARG = ', XARG,
     &         'HSP = ', HSP,
     &         'DHP1 = ', DHP1,
     &         'WSTAR = ', WSTAR,
     &         'UEFFD = ', UEFFD,
     &         'ASUB1 = ', ASUB1,
     &         'HED1 = ', HED1,
     &         'ASUB2 = ', ASUB2,
     &         'HED2 = ', HED2,
     &         'DHP2 = ', DHP2,
     &         'HEN1 = ', HEN1,
     &         'HEN2 = ', HEN2,
     &         'HE3 = ', HE3
         END IF

      END IF

      RETURN
      END

      SUBROUTINE PRMHEFF
C***********************************************************************
C             PRMHEFF Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Calculates Effective Plume Height (m)
C
C        PROGRAMMER: Roger Brode and Jim Paumier, PES, Inc.
C
C        DATE:    September 30, 1993
C
C        REVISIONS:  Corrected formulations for HEN1 & HEN2 per
C                    Model Formulation Document and conversation
C                    with Russ Lee and Jeff Weil.
C                    Roger Brode, PES, Inc. - 12/7/94
C
C        INPUTS:  Arrays of Source Parameters
C                 Logical Wake Flags
C                 Meteorological Variables for One Hour
C                 Wind Speed Adjusted to Stack Height
C                 Downwind Distance
C                 Terrain Elevation of Receptor
C
C        OUTPUTS: Effective Plume Height (HE)
C
C        CALLED FROM:   PCALC, VCALC, ACALC, PLUMEF
C
C   References:   "A Dispersion Model for the Convective Boundary
C                  Layer", J. Weil, 8/17/93
C                 "Plume Penetration of the CBL and Source 3: Source
C                  Strength and Plume Rise", J. Weil, 9/1/93
C
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'PRMHEFF'

C     Compute the effective plume height
      IF( STABLE )THEN
C        The atmosphere is stable or the release is above the CBL
C        mixing ht.

         HE = HS + DHP
C        Don't Allow Effective Plume Height to be < 0.0
         HE = MAX( 0.0D0, HE)

      ELSE IF( UNSTAB )THEN
C        The atmosphere is unstable and the release is below the
C        mixing ht.

         HE = HS + DHP
C        Don't Allow Effective Plume Height to be < 0.0
         HE = MAX( 0.0D0, HE)
      END IF

      RETURN
      END

      SUBROUTINE PDIS ( XARG )
C***********************************************************************
C             PDIS module of the AMS/EPA Regulatory Model - AERMOD
c ----------------------------------------------------------------------
c ---    ISC-PRIME     Version 1.0    Level 970812              Modified
c ---        D. Strimaitis
c ---        Earth Tech, Inc.
c            Prepared for EPRI under contract WO3527-01
c ----------------------------------------------------------------------
C
C        PURPOSE: Calculates Dispersion Parameters for POINT Sources
C
C        PROGRAMMER: Roger Brode and Jim Paumier, PES, Inc.
C
C        DATE:    Spetember 30, 1993
C
C        REVISIONS:
C                  Added call to PLAT_DOWNWASH to get additional initial
C                  SIGMAY and SIGMAZ due to PLATFORM.
C                  Michelle G. Snyder, WOOD, 8/5/2021
C
C                 SZSURF calculation reinstated 7/13/94, R.F. Lee
C
C        INPUTS:  Arrays of Source Parameters
C                 Logical Wake Flags
C                 Wake Plume Height, HEMWAK
C                 Meteorological Variables for One Hour
C                 Distance, XARG
C
C        OUTPUTS: Lateral and Vertical Dispersion Coefficients, SY and SZ
C
C        CALLED FROM:   PCALC
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      DOUBLE PRECISION :: XARG

C     Variable Initializations
      MODNAM = 'PDIS'

C     Calculate Sigma-z from formuale                 --- CALL SIGZ
      CALL SIGZ ( XARG )
C     Calculate Sigma-y from formulae                 --- CALL SIGY
      CALL SIGY ( XARG )

C     Set all virtual source terms to 0 for non-downwashing sources
      VSIGY = 0.0D0
      VSYN  = 0.0D0
      VSIGZ = 0.0D0
      VSZD1 = 0.0D0
      VSZD2 = 0.0D0
      VSZN1 = 0.0D0
      VSZN2 = 0.0D0
      VSZ3  = 0.0D0
      VSY3  = 0.0D0

C     Calculate the buoyancy-induced dispersion parameters
      IF( NOBID )THEN
C        Set BID Terms to 0.0
         IF( STABLE  .OR.  (UNSTAB .AND. (HS .GE. ZI) ) )THEN
            SYB = 0.0D0
            SZB = 0.0D0

         ELSE IF( UNSTAB )THEN
            SYB  = 0.0D0
            SZBD = 0.0D0
            SZBN = 0.0D0
            SYB3 = 0.0D0
            SZB3 = 0.0D0

         END IF

      ELSE
C        Specify BID Terms                                 --- CALL BID
         CALL BID

      END IF

C ----------------------------------------------------------------------
C ---------- BEGIN PLATFORM DOWNWASH INTITAL SIGMAY and SIGMAZ ---------
C ----------------------------------------------------------------------
CMGS        Compute additional initial sigmay and sigmaz due to presence of
CMGS        platform. Calls PLAT_DOWNWASH(contained in calc1.f) for
CMGS        effective height of each plume being modeled. The sigma's are
CMGS        added in quadrature in sigmas.f/RMSSIG.
CMGS        Michelle G. Snyder, WOOD, 8/5/2021

CCRT  D063
C     Initialize platform downwash sigmas
      PLATSZ   = 0.0D0  ! stable or unstable injected
      PLATSY   = 0.0D0  ! stable or unstable injected
      PLATSZD1 = 0.0D0  ! unstable, direct, updraft
      PLATSYD1 = 0.0D0  ! unstable, direct, updraft
      PLATSZD2 = 0.0D0  ! unstable, direct, downdraft
      PLATSYD2 = 0.0D0  ! unstable, direct, downdraft
      PLATSZN1 = 0.0D0  ! unstable, indirect, updraft
      PLATSYN1 = 0.0D0  ! unstable, indirect, updraft
      PLATSZN2 = 0.0D0  ! unstable, indirect, downdraft
      PLATSYN2 = 0.0D0  ! unstable, indirect, downdraft
      PLATSZP  = 0.0D0  ! unstable, penetrated
      PLATSYP  = 0.0D0  ! unstable, penetrated

      IF (OSPLAT(ISRC)) THEN


C           Get initial value for downwash sigma-z
C           Consider all conditions in HEFF
            IF (STABLE) THEN

C              The atmosphere is stable or the release is above the CBL
               CALL PLAT_DOWNWASH (XARG, PLATHB(ISRC)+0.0D0,
     &            PLATWB(ISRC), HE, PLATSZ, PLATSY)

            ELSEIF (UNSTAB .AND. (HS .GE. ZI)) THEN

C              Do not apply to injected plume
               CONTINUE


            ELSEIF( UNSTAB )THEN
C              Direct Plume updraft(HED1) and downdraft(HED2)
               CALL PLAT_DOWNWASH (XARG, PLATHB(ISRC)+0.0D0,
     &            PLATWB(ISRC), HED1, PLATSZD1, PLATSYD1)

               CALL PLAT_DOWNWASH (XARG, PLATHB(ISRC)+0.0D0,
     &            PLATWB(ISRC), HED2, PLATSZD2, PLATSYD2)

C              Indirect Plume updraft(HEN1) and downdraft(HEN2)
               CALL PLAT_DOWNWASH (XARG, PLATHB(ISRC)+0.0D0,
     &            PLATWB(ISRC), HEN1, PLATSZN1, PLATSYN1)

               CALL PLAT_DOWNWASH (XARG, PLATHB(ISRC)+0.0D0,
     &            PLATWB(ISRC), HEN2, PLATSZN2, PLATSYN2)

C              Penetrated Plume
               CALL PLAT_DOWNWASH (XARG, PLATHB(ISRC)+0.0D0,
     &            PLATWB(ISRC), HE3, PLATSZP, PLATSYP)

            END IF

      END IF
C ----------------------------------------------------------------------
C ---------- END PLATFORM DOWNWASH INTITAL SIGMAY and SIGMAZ -----------
C----------             RESUME NORMAL PROCESSING           -------------
C ----------------------------------------------------------------------

C---- Calculate the root-mean-square sigma_Y and sigma_Z   --- CALL RMSSIG
      CALL RMSSIG

      RETURN
      END

      SUBROUTINE VDIS (XARG)
C***********************************************************************
C             VDIS Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Calculates Dispersion Parameters for VOLUME Sources
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Arrays of Source Parameters
C                 Meteorological Variables for One Hour
C                 Downwind Distance
C
C        OUTPUTS: Lateral and Vertical Dispersion Coefficients
C
C        CALLED FROM:   VCALC
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      DOUBLE PRECISION :: XARG

C     Variable Initializations
      MODNAM = 'VDIS'

C     Calculate Sigma-y from formulae                 --- CALL SIGY
      CALL SIGY ( XARG )
C     Calculate Sigma-z from formulae                 --- CALL SIGZ
      CALL SIGZ ( XARG )

C     Set virtual source terms based on initial sigmas input by user
      VSIGY = SYINIT
      VSYN  = SYINIT
      VSIGZ = SZINIT
      VSZD1 = SZINIT
      VSZD2 = SZINIT
      VSZN1 = SZINIT
      VSZN2 = SZINIT
      VSZ3  = 0.0D0
      VSY3  = 0.0D0

C     Set BID terms to zero
      SYB  = 0.0D0
      SZB  = 0.0D0
      SZBD = 0.0D0
      SZBN = 0.0D0
      SYB3 = 0.0D0
      SZB3 = 0.0D0

C---- Calculate the root-mean-square sigma_Y and sigma_Z   --- CALL RMSSIG
      CALL RMSSIG

      RETURN
      END

      SUBROUTINE ADISY(XARG)
C***********************************************************************
C                 ADISY Module of the AERMOD Model
C
C        PURPOSE: Calculates Lateral Dispersion Parameters for AREA Sources
C
C        PROGRAMMER: Roger Brode, PES, Inc.
C
C        DATE:    July 21, 1994
C
C        MODIFIED:   To calculate sigma-y and sigma-z separately
C                    for AREA source - R.Brode, PES, 12/9/98
C
C        INPUTS:  Arrays of Source Parameters
C                 Meteorological Variables for One Hour
C                 Downwind Distance
C
C        OUTPUTS: Lateral and Vertical Dispersion Coefficients
C
C        CALLED FROM:   VCALC
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      DOUBLE PRECISION :: XARG

C     Variable Initializations
      MODNAM = 'ADISY'

C     Calculate Sigma-y from formulae                 --- CALL SIGY
      CALL SIGY ( XARG )

C     Set virtual source terms for AREA sources to zero
      VSIGY = 0.0D0
      VSY3  = 0.0D0

C     Set BID terms to zero
      SYB  = 0.0D0
      SYB3 = 0.0D0

      SY = SYAMB

      RETURN
      END

      SUBROUTINE ADISZ(XARG)
C***********************************************************************
C                 ADISZ Module of the AERMOD Model
C
C        PURPOSE: Calculates Vertical Dispersion Parameters for AREA
C                 and LINE Sources
C
C        PROGRAMMER: Roger Brode, PES, Inc.
C
C        DATE:    July 21, 1994
C
C        MODIFIED:   To calculate sigma-y and sigma-z separately
C                    for AREA source - R.Brode, PES, 12/9/98
C
C        INPUTS:  Arrays of Source Parameters
C                 Meteorological Variables for One Hour
C                 Downwind Distance
C
C        OUTPUTS: Lateral and Vertical Dispersion Coefficients
C
C        CALLED FROM:   VCALC
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      DOUBLE PRECISION :: XARG

C     Variable Initializations
      MODNAM = 'ADISZ'

C     Calculate Sigma-z from formulae                 --- CALL SIGZ
      CALL SIGZ ( XARG )

C     Set virtual source terms based on initial sigmas input by user
      VSIGZ = SZINIT
      VSZD1 = SZINIT
      VSZD2 = SZINIT
      VSZN1 = SZINIT
      VSZN2 = SZINIT
      VSZ3  = 0.0D0

C     Set BID terms to zero
      SZB  = 0.0D0
      SZBD = 0.0D0
      SZBN = 0.0D0
      SZB3 = 0.0D0

C---- Calculate the root-mean-square sigma_Y and sigma_Z   --- CALL RMSSIG
      CALL RMSSIG

      RETURN
      END

      SUBROUTINE AER_PCHI( XARG, ADJ, VDINP, JIN, AEROUT )
C***********************************************************************
C        AER_PCHI Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Calculates Hourly Concentration for POINT Sources
C                 Using Gaussian Plume Equation
C
C        PROGRAMMER: Roger Brode, PES, Inc.
C
C        DATE:    November 10, 2000
C
C        MODIFIED:   To include lateral term (FSUBY) in weighting of
C                    direct and penetrated contributions for wet dep.
C                    Added debug statement for CONC based on ENSR.
C                    - R.Brode, MACTEC, 7/27/2004
C
C        MODIFIED:   To correct WETFLUX values for conversion from
C                    seconds to hours and to include SQRT(2PI) in
C                    denominator of integrated vertical term.
C                    - R.Brode, MACTEC, 3/9/2004
C
C        INPUTS:  Distance, XARG (downwind for plume; radial for pancake)
C                 Crosswind Distance
C                 Plume Height
C                 Stack Top Wind Speed
C                 Lateral Dispersion Parameter
C                 Vertical Dispersion Parameter
C                 Stability Class
C                 Mixing Height
C                 Receptor Height Above Ground
C                 Emission Rate and Units Scaling Factor
C                 Source Parameter Arrays
C
C        OUTPUTS: AEROUT, AERMOD Concentration for Particular
C                 Source/Receptor Combination
C
C        CALLED FROM:   AERCALC, VOLCALC, ACALC
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      INTEGER :: JIN
      DOUBLE PRECISION :: AEROUT(NUMTYP), XARG, ADJ, VDINP, DRYFLUX,
     &                    WETFLUX
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'AER_PCHI'
      DRYFLUX = 0.0D0
      WETFLUX = 0.0D0

C---- Calculate the contribution due to horizontal plume, CWRAP
      IF (FOPT .EQ. 0.0D0) THEN
         CWRAP = 0.0D0
      ELSE
         CALL CPLUME (ZRT, CWRAP)
      END IF

C---- Calculate the contribution due to terrain-following plume, CLIFT
      IF (ZRT .EQ. ZFLAG) THEN
C----    Effective receptor heights are equal, therefore CLIFT = CWRAP
         CLIFT = CWRAP
      ELSE IF (FOPT .EQ. 1.0D0) THEN
         CLIFT = 0.0D0
      ELSE
         CALL CPLUME (ZFLAG, CLIFT)
      END IF

C---- Calculate the exponential decay term, D               ---   CALL DECAY
      Call DECAY (XARG)

C---- Calculate the hourly concentration and deposition values
      ITYP = 0
      IF (CONC) THEN
         ITYP = 1
         AEROUT(ITYP) = ADJ * EMIFAC(ITYP) *
     &                 (FOPT * CWRAP + (1.0D0 - FOPT) * CLIFT) * D

C   ENHANCEMENT TO DEBUG OUTPUT BASED ON ENSR
         IF (DEBUG) THEN
            WRITE(DBGUNT,10) ITYP, ADJ, FOPT, CWRAP, CLIFT, D,
     &                       AEROUT(ITYP)
10          FORMAT(/,'ITYP = ',I2,' - CONC:',
     &             /,'AEROUT(ITYP) = ADJ * EMIFAC(ITYP) * (FOPT * ',
     &               'CWRAP + (1.0 -FOPT) * CLIFT) * D',
     &             /,' ADJ   = ',G16.8,
     &             /,' FOPT  = ',G16.8,
     &             /,' CWRAP = ',G16.8,
     &             /,' CLIFT = ',G16.8,
     &             /,' D     = ',G16.8,
     &             /,' AEROUT(ITYP) = ',G16.8,/)
         END IF

      END IF

      IF (DEPOS .OR. DDEP) THEN
C        Calculate DRYFLUX, vertical term for wet deposition
C----    Calculate the contribution due to horizontal plume, CWRAP
         IF (FOPT .EQ. 0.0D0) THEN
            CWRAP = 0.0D0
         ELSE
            CALL CPLUME (ZRT-ZFLAG+ZRDEP, CWRAP)
         END IF

C----    Calculate the contribution due to terrain-following plume, CLIFT
         IF (ZRT .EQ. ZFLAG) THEN
C----       Effective receptor heights are equal, therefore CLIFT = CWRAP
            CLIFT = CWRAP
         ELSE IF (FOPT .EQ. 1.0D0) THEN
            CLIFT = 0.0D0
         ELSE
            CALL CPLUME (ZRDEP, CLIFT)
         END IF

         DRYFLUX = (FOPT * CWRAP + (1.0D0 - FOPT) * CLIFT) * D
     &
      END IF

      IF (DEPOS .OR. WDEP) THEN
C        Calculate WETFLUX, vertical term for wet deposition.
C        Note that the SRT2PI for the integrated vertical term
C        has been removed since it should be divided by SRT2PI.
C        Additional factor of 3600. has been added to denominator
C        to account for conversion from seconds to hours when
C        divided by wind speed below.
         IF (PRATE .GT. 0.0D0) THEN
            IF (NPD .EQ. 0) THEN
               WETFLUX = (ADJ*FRACSAT*PRATE*1.0D6*RGAS*TA)/
     &                   (ZSUBP*HENRY(ISRC)*1.0D9*DENOM*3600.0D0)
            ELSE
               WETFLUX = 1.0D-3*ADJ*WASHOUT(JIN)*PRATE/
     &                   (ZSUBP*3600.0D0)
            END IF
         ELSE
            WETFLUX = 0.0D0
         END IF
      END IF

      IF (DEPOS) THEN
         ITYP = ITYP + 1
         IF( STABLE  .OR.  (UNSTAB .AND. (HS .GE. ZI) ) ) THEN
            AEROUT(ITYP) = ADJ * VDINP * EMIFAC(ITYP) * DRYFLUX +
     &                     QTK * WETFLUX * EMIFAC(ITYP) * FSUBY/UEFF
         ELSE IF (UNSTAB) THEN
            AEROUT(ITYP) = ADJ * VDINP * EMIFAC(ITYP) * DRYFLUX +
     &                     QTK * WETFLUX * EMIFAC(ITYP) *
     &                     (PPF*FSUBY3/UEFF3+(1.0D0-PPF)*FSUBY/UEFFD)
         END IF

         IF (DEBUG) THEN
            WRITE(DBGUNT,11) ITYP, ADJ, VDINP, DRYFLUX, WETFLUX,
     &                       AEROUT(ITYP)
11          FORMAT(/,'ITYP = ',I2,' - DEPOS:',
     &             /,' ADJ     = ',G16.8,
     &             /,' VDINP   = ',G16.8,
     &             /,' DRYFLUX = ',G16.8,
     &             /,' WETFLUX = ',G16.8,
     &             /,' AEROUT(ITYP) = ',G16.8,/)
         END IF

      END IF

      IF (DDEP) THEN
         ITYP = ITYP + 1
         AEROUT(ITYP) = ADJ * VDINP * EMIFAC(ITYP) * DRYFLUX

         IF (DEBUG) THEN
            WRITE(DBGUNT,12) ITYP, ADJ, VDINP, DRYFLUX,
     &                       AEROUT(ITYP)
12          FORMAT(/,'ITYP = ',I2,' - DDEP:',
     &             /,' ADJ     = ',G16.8,
     &             /,' VDINP   = ',G16.8,
     &             /,' DRYFLUX = ',G16.8,
     &             /,' AEROUT(ITYP) = ',G16.8,/)
         END IF

      END IF

      IF (WDEP) THEN
         ITYP = ITYP + 1
         IF( STABLE  .OR.  (UNSTAB .AND. (HS .GE. ZI) ) ) THEN
            AEROUT(ITYP) = QTK * WETFLUX * EMIFAC(ITYP) * FSUBY/UEFF
         ELSE IF (UNSTAB) THEN
            AEROUT(ITYP) = QTK * WETFLUX * EMIFAC(ITYP) *
     &                     (PPF*FSUBY3/UEFF3+(1.0D0-PPF)*FSUBY/UEFFD)
         END IF

         IF (DEBUG) THEN
            WRITE(DBGUNT,13) ITYP, ADJ, ZSUBP, PRATE, WETFLUX,
     &                       AEROUT(ITYP)
13         FORMAT(/,'ITYP = ',I2,' - WDEP:',
     &             /,' ADJ     = ',G16.8,
     &             /,' ZSUBP   = ',G16.8,
     &             /,' PRATE   = ',G16.8,
     &             /,' WETFLUX = ',G16.8,
     &             /,' AEROUT(ITYP) = ',G16.8,/)
         END IF

      END IF


CCRFL Call to METDEB was moved here from METEXT on 9/26/94, R.F. Lee.
CCRFL Print meteorological debug output.                   ---   CALL METDEB
      IF (METEORDBG) CALL METDEB

      IF ( DEBUG ) THEN
C        Print Out Debugging Information                    ---   CALL DEBOUT
         CALL DEBOUT
      END IF

      RETURN
      END

      SUBROUTINE PRM_PCHI(ADJ, VDINP, JIN, INSRC)                       ! ORD(EMM) change
C***********************************************************************
C        PRM_PCHI Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Calculates Hourly Concentration for POINT Sources
C                 with PRIME Downwash Algorithm
C
C        PROGRAMMER: Roger Brode, PES, Inc.
C
C        DATE:     November 10, 2000
C
C        MODIFIED:   To correct WETFLUX values for conversion from
C                    seconds to hours and to include SQRT(2PI) in
C                    denominator of integrated vertical term.
C                    - R.Brode, MACTEC, 3/9/2004
C
C                 Modifications for the ORD_DWNW & AWMADWNW ALPHA options
C                    - August 2018
C
!                 Modified for TTRM2 option to save UEFFe for travel time
!                    - November 2021
C
C        INPUTS:  Downwind Distance
C                 Crosswind Distance
C                 Plume Height
C                 Stack Top Wind Speed
C                 Lateral Dispersion Parameter
C                 Vertical Dispersion Parameter
C                 Stability Class
C                 Mixing Height
C                 Receptor Height Above Ground
C                 Emission Rate and Units Scaling Factor
C                 Source Parameter Arrays
C
C        OUTPUTS: PRMVAL, PRIME Concentration for Particular
C                 Source/Receptor Combination, summed across
C                 three PRIME "sources", i.e., primary source,
C                 inside cavity source and outside cavity source
C
C        CALLED FROM:   PRMCALC
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      USE PRIME_WAKEDAT, only : HB
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER          :: JIN, INSRC, NDXBLDG          !  EMM ADD
      INTEGER          :: NDXBHIe, NDXBLOe             !  EMM ADD
      INTEGER          :: HT_NDX
      DOUBLE PRECISION :: ZPHT, UHB                    !  EMM ADD
      DOUBLE PRECISION :: UEFFe                        !  EMM ADD
      DOUBLE PRECISION :: ADJ, VDINP, DRYFLUX, WETFLUX
      DOUBLE PRECISION :: HT2USE,U_above,U_below
C     ZPHT_MIN is the lowest height to use in the calculations; this is
C       to avoid problems with very short stacks and stack-tip downwash
      DOUBLE PRECISION, PARAMETER :: ZPHT_MIN=0.5D0
C Unused:       INTEGER          :: LOWWFLG
C Unused:       DOUBLE PRECISION :: UZPHT, UZLO

C     Variable Initializations
      MODNAM = 'PRM_PCHI'
      DRYFLUX = 0.0D0
      WETFLUX = 0.0D0

C---- Calculate the exponential decay term, D               ---   CALL DECAY
      Call DECAY (X)

      IF (L_ORD_Ueff) THEN
C ---    ORD formulation
         ZPHT = DHP + HS
         CALL LOCATE(GRIDHT, 1, MXGLVL, ZFLAG, NDXBLOe)
         CALL LOCATE(GRIDHT, 1, MXGLVL, ZPHT, NDXBHIe)

C        Find the wind speed at building height for source 2 calc
         CALL LOCATE(GRIDHT, 1, MXGLVL, HB, NDXBLDG)
         UHB = GRIDWS(NDXBLDG)  ! Wind speed at building height

         CALL ANYAVG ( MXGLVL, GRIDHT, GRIDWS, ZFLAG ,NDXBLOe+1,
     &        ZPHT, NDXBHIe, UEFFe )
         IF ((INSRC.eq.2) .OR. (INSRC .eq. 3)) THEN
            UEFFe = US
         END IF

      ELSE IF (L_AWMA_Ueff) THEN
C ---    AWMA PRIME2 Subcommittee (CPP) formulation
         ZPHT = DHP + HS       ! GET PLUME HEIGHT

C        Set the height to use in calculations to
C        MAX(plume ht, receptor ht)
         HT2USE = MAX(ZPHT,ZPHT_MIN)

C        Identify the grid height immediately below HT2USE
         CALL LOCATE(GRIDHT, 1, MXGLVL, HT2USE, HT_NDX)
         U_below = GRIDWS(HT_NDX)
         U_above = GRIDWS(HT_NDX+1)

         CALL GINTRP(GRIDHT(HT_NDX),U_below, GRIDHT(HT_NDX+1),U_above,
     &               HT2USE,UEFFe)

C        Check if source is inside or outside cavity and if so, use
C          appropriate wind speed for calculations
         IF ((INSRC .eq. 3)) THEN     !! "inside" cavity source
            UEFFe = US                 ! Wind speed at stack height

         ELSE IF((INSRC.eq.2)) THEN    !! "outside" cavity source
C           Get grid height immediately BELOW the building ht
            CALL LOCATE(GRIDHT, 1, MXGLVL, HB, NDXBLDG)
C           Get the wind speed at
            UHB = GRIDWS(NDXBLDG)
            UEFFe = UHB                ! Wind speed at 'building ht'
         END IF

      ELSE
C --- Set UEFFe to the regulatory AERMOD value
        UEFFe = US
      END IF

! Added for TTRM; AECOM, Nov. 2021
      IF (RUNTTRM) THEN
         TTRMOUT(IREC,ISRC,24) = UEFFe
      ENDIF
! End TTRM insert

C---- Calculate the hourly concentration value
      ITYP = 0
      IF (CONC) THEN
         ITYP = 1
C----    Calculate the contribution due to horizontal plume, CWRAP
         IF (FOPT .EQ. 0.0D0) THEN
            CWRAP = 0.0D0
         ELSE
            CALL PRM_PLUME (ZRT, UEFFe, CWRAP)
         END IF

C----    Calculate the contribution due to terrain-following plume, CLIFT
         IF (ZRT .EQ. ZFLAG) THEN
C----       Effective receptor heights are equal, therefore CLIFT = CWRAP
            CLIFT = CWRAP
         ELSE IF (FOPT .EQ. 1.0D0) THEN
            CLIFT = 0.0D0
         ELSE
            CALL PRM_PLUME (ZFLAG, UEFFe, CLIFT)
         END IF

         PRMVAL(ITYP) = PRMVAL(ITYP) + ADJ * EMIFAC(ITYP) *
     &                 (FOPT * CWRAP + (1.0D0 - FOPT) * CLIFT) * D
      END IF

      IF (DEPOS .OR. DDEP) THEN
C        Calculate DRYFLUX, vertical term for wet deposition
C----    Calculate the contribution due to horizontal plume, CWRAP
         IF (FOPT .EQ. 0.0D0) THEN
            CWRAP = 0.0D0
         ELSE
            CALL PRM_PLUME (ZRT-ZFLAG+ZRDEP, UEFFe, CWRAP)
         END IF

C----    Calculate the contribution due to terrain-following plume, CLIFT
         IF (ZRT .EQ. ZFLAG) THEN
C----       Effective receptor heights are equal, therefore CLIFT = CWRAP
            CLIFT = CWRAP
         ELSE IF (FOPT .EQ. 1.0D0) THEN
            CLIFT = 0.0D0
         ELSE
            CALL PRM_PLUME (ZRDEP, UEFFe, CLIFT)
         END IF

         DRYFLUX = (FOPT * CWRAP + (1.0D0 - FOPT) * CLIFT) * D
      END IF

      IF (DEPOS .OR. WDEP) THEN
C        Calculate WETFLUX, vertical term for wet deposition
C        Note that the SRT2PI for the integrated vertical term
C        has been removed since it should be divided by SRT2PI.
C        Additional factor of 3600. has been added to denominator
C        to account for conversion from seconds to hours when
C        divided by wind speed below.
         IF (PRATE .GT. 0.0D0) THEN
            IF (NPD .EQ. 0) THEN
               WETFLUX = (ADJ*FRACSAT*PRATE*1.0D6*RGAS*TA)/
     &                   (ZSUBP*HENRY(ISRC)*1.0D9*DENOM*3600.0D0)
            ELSE
               WETFLUX = 1.0D-3*ADJ*WASHOUT(JIN)*PRATE/
     &                   (ZSUBP*3600.0D0)
            END IF
         ELSE
            WETFLUX = 0.0D0
         END IF
      END IF

      IF (DEPOS) THEN
         ITYP = ITYP + 1
         PRMVAL(ITYP) = PRMVAL(ITYP) + ADJ*VDINP*EMIFAC(ITYP)*DRYFLUX +
     &                  QTK * WETFLUX * EMIFAC(ITYP) * FSUBY/UEFF
      END IF

      IF (DDEP) THEN
         ITYP = ITYP + 1
         PRMVAL(ITYP) = PRMVAL(ITYP) + ADJ*VDINP*EMIFAC(ITYP)*DRYFLUX
      END IF

      IF (WDEP) THEN
         ITYP = ITYP + 1
         PRMVAL(ITYP) = PRMVAL(ITYP)+QTK*WETFLUX*EMIFAC(ITYP)*FSUBY/UEFF
      END IF

      IF ( DEBUG ) THEN
C        Print Out Debugging Information                    ---   CALL DEBOUT
         CALL DEBOUT
      END IF

      RETURN
      END

      SUBROUTINE AER_ACHI( XARG, ADJ, VDINP, JIN, FYARG, POUT )
C***********************************************************************
C        AER_ACHI Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Calculates Hourly Concentration for AREA Sources
C                 Using Gaussian Plume Equation
C
C        PROGRAMMER: Roger Brode, PES, Inc.
C
C        DATE:    November 10, 2000
C
C        MODIFIED:   To correct WETFLUX values for conversion from
C                    seconds to hours and to include SQRT(2PI) in
C                    denominator of integrated vertical term.
C                    - R.Brode, MACTEC, 3/9/2004
C
C        INPUTS:  Distance, XARG (downwind for plume; radial for pancake)
C                 Crosswind Distance
C                 Plume Height
C                 Stack Top Wind Speed
C                 Lateral Dispersion Parameter
C                 Vertical Dispersion Parameter
C                 Stability Class
C                 Mixing Height
C                 Receptor Height Above Ground
C                 Emission Rate and Units Scaling Factor
C                 Source Parameter Arrays
C
C        OUTPUTS: AEROUT, AERMOD Concentration for Particular
C                 Source/Receptor Combination
C
C        CALLED FROM:   AERCALC, VOLCALC, ACALC
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      INTEGER :: JIN
      DOUBLE PRECISION :: POUT, XARG, ADJ, VDINP, FYARG, DRYFLUX,
     &                    WETFLUX
      LOGICAL :: SCONC, SDEPOS, SDDEP, SWDEP
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'AER_ACHI'
      DRYFLUX = 0.0D0
      WETFLUX = 0.0D0

C     Determine appropriate output type for this ITYP, assign output type
C     logicals to local variables, and set others to .FALSE.
      IF (OUTTYP(ITYP) .EQ. 'CONC') THEN
         SCONC  = .TRUE.
         SDEPOS = .FALSE.
         SDDEP  = .FALSE.
         SWDEP  = .FALSE.
      ELSE IF (OUTTYP(ITYP) .EQ. 'DEPOS') THEN
         SCONC  = .FALSE.
         SDEPOS = .TRUE.
         SDDEP  = .FALSE.
         SWDEP  = .FALSE.
      ELSE IF (OUTTYP(ITYP) .EQ. 'DDEP') THEN
         SCONC  = .FALSE.
         SDEPOS = .FALSE.
         SDDEP  = .TRUE.
         SWDEP  = .FALSE.
      ELSE IF (OUTTYP(ITYP) .EQ. 'WDEP') THEN
         SCONC  = .FALSE.
         SDEPOS = .FALSE.
         SDDEP  = .FALSE.
         SWDEP  = .TRUE.
      ELSE   ! this condition should never happen, set all F
         SCONC  = .FALSE.
         SDEPOS = .FALSE.
         SDDEP  = .FALSE.
         SWDEP  = .FALSE.
         RETURN
      END IF

      POUT = 0.0D0

C---- Calculate the exponential decay term, D               ---   CALL DECAY
      Call DECAY (XARG)

      IF (SCONC) THEN
C----    Get Concentration or Deposition due to horizontal plume, CWRAP
         IF (FOPT .EQ. 0.0D0) THEN
            CWRAP = 0.0D0
         ELSE
            CALL ACPLUME (ZRT, FYARG, CWRAP)
         END IF

C----    Calculate the contribution due to terrain-following plume, CLIFT
         IF (ZRT .EQ. ZFLAG) THEN
C----       Effective receptor heights are equal, therefore CLIFT = CWRAP
            CLIFT = CWRAP
         ELSE IF (FOPT .EQ. 1.0D0) THEN
            CLIFT = 0.0D0
         ELSE
C           Get Concentration or Deposition due to LIFT algorithm
            CALL ACPLUME (ZFLAG, FYARG, CLIFT)
         END IF

C----    Calculate the hourly concentration value
C        Now compute the function
         POUT = ADJ * (FOPT * CWRAP + (1.0D0 - FOPT) * CLIFT) * D

      END IF
      IF (SDEPOS .OR. SDDEP) THEN
C----    Get Concentration or Deposition due to horizontal plume, CWRAP
         IF (FOPT .EQ. 0.0D0) THEN
            CWRAP = 0.0D0
         ELSE
            CALL ACPLUME (ZRT-ZFLAG+ZRDEP, FYARG, CWRAP)
         END IF

C----    Calculate the contribution due to terrain-following plume, CLIFT
         IF (ZRT .EQ. ZFLAG) THEN
C----       Effective receptor heights are equal, therefore CLIFT = CWRAP
            CLIFT = CWRAP
         ELSE IF (FOPT .EQ. 1.0D0) THEN
            CLIFT = 0.0D0
         ELSE
C           Get Concentration or Deposition due to LIFT algorithm
            CALL ACPLUME (ZRDEP, FYARG, CLIFT)
         END IF

C----    Calculate the hourly concentration value
C        Now compute the function
         DRYFLUX = ADJ * (FOPT * CWRAP + (1.0D0 - FOPT) * CLIFT) * D

      END IF
      IF (SDEPOS .OR. SWDEP) THEN
C        Calculate WETFLUX, vertical term for wet deposition
C        Note that the SRT2PI for the integrated vertical term
C        has been removed since it should be divided by SRT2PI.
C        Additional factor of 3600. has been added to denominator
C        to account for conversion from seconds to hours when
C        divided by wind speed below.
         IF (PRATE .GT. 0.0D0) THEN
            IF (NPD .EQ. 0) THEN
               WETFLUX = (ADJ*FRACSAT*PRATE*1.0D6*RGAS*TA)/
     &                   (ZSUBP*HENRY(ISRC)*1.0D9*DENOM*3600.0D0)
            ELSE
               WETFLUX = 1.0D-3*ADJ*WASHOUT(JIN)*PRATE/
     &                   (ZSUBP*3600.0D0)
            END IF
         ELSE
            WETFLUX = 0.0D0
         END IF
      END IF
      IF (SDEPOS) THEN
         IF( STABLE  .OR.  (UNSTAB .AND. (HS .GE. ZI) ) ) THEN
            POUT = VDINP*DRYFLUX + WETFLUX*FYARG/UEFF
         ELSE IF (UNSTAB) THEN
            POUT = VDINP*DRYFLUX + WETFLUX*FYARG/UEFFD
         END IF
      END IF
      IF (SDDEP) THEN
         POUT = VDINP*DRYFLUX
      END IF
      IF (SWDEP) THEN
         IF( STABLE  .OR.  (UNSTAB .AND. (HS .GE. ZI) ) ) THEN
            POUT = WETFLUX*FYARG/UEFF
         ELSE IF (UNSTAB) THEN
            POUT = WETFLUX*FYARG/UEFFD
         END IF
      END IF


CCRFL Call to METDEB was moved here from METEXT on 9/26/94, R.F. Lee.
CCRFL Print meteorological debug output.                   ---   CALL METDEB
      IF (METEORDBG) CALL METDEB

      IF ( DEBUG ) THEN
C        Print Out Debugging Information                    ---   CALL DEBOUT
         CALL DEBOUT
      END IF

      RETURN
      END

      SUBROUTINE DEBOUT
C***********************************************************************
C             DEBOUT Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Outputs Debugging Information: Sigmas, Plume Heights,
C                 etc., for Each Calculation
C
C        PROGRAMMER: Roger Brode and Jim Paumier, PES, Inc.
C
C        DATE:    October 8, 1993
C
C        REVISIONS:  Revised emission rate terms:  for CHID & CHIN,
C                    to QTK*(1-PPF), and for CHI3 to QTK*PPF.
C                    Ref:  P.D.F. Model for Dispersion in the
C                    Convective Boundary Layer, J.C. Weil, 6/27/94.
C                    Changed 7/19/94, R.F. Lee.
C
C                    Revised by Bob Paine to improve readability
C                    of debugging output.  Changed 8/18/94, R.F. Lee
C                   & R. Paine.
C
C
C        INPUTS:  Downwind Distance
C                 Crosswind Distance
C                 Plume Height
C                 Stack Top Wind Speed
C                 Lateral Dispersion Parameter
C                 Vertical Dispersion Parameter
C                 Stability Class
C                 Mixing Height
C                 Receptor Height Above Ground
C                 Emission Rate and Units Scaling Factor
C                 Source Parameter Arrays
C
C        OUTPUTS: Debug Outputs
C
C        CALLED FROM:   PCHI, PDEP, AREAIN
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      DOUBLE PRECISION :: CHID, CHIN, CHI3

C     Variable Initializations
      MODNAM = 'DEBOUT'

C     Calculate contributions from each "plume"
      IF (STABLE .OR. (UNSTAB .AND. HS.GE.ZI)) THEN
         CHID = HRVAL(1)
      ELSE IF (UNSTAB) THEN
CCRFL
CCRFL    Revised emission rate terms:  for CHID & CHIN, to QTK*(1-PPF),
CCRFL    and for CHI3 to QTK*PPF.  Ref:  P.D.F. Model for Dispersion in
CCRFL    the Convective Boundary Layer, J.C. Weil, 6/27/94.  Changed
CCRFL    7/19/94, R.F. Lee.
CCRFL
         CHID = (QTK*EMIFAC(1) * (1.0D0-PPF) / UEFFD) * (FSUBYD*FSUBZD)
         CHIN = (QTK*EMIFAC(1) * (1.0D0-PPF) / UEFFN) * (FSUBYN*FSUBZN)
         IF (PPF.GT.0.0D0 .AND. UEFF3.GT.0.0D0) THEN
            CHI3 = (QTK * EMIFAC(1) * PPF / UEFF3) * (FSUBY3*FSUBZ3)
         ELSE
            CHI3 = 0.0D0
         END IF
      END IF

C     Write a blank line to separate the groupings
      WRITE ( DBGUNT , 101 )

C     Write the debug output for the receptor data
      WRITE ( DBGUNT , 320 ) IREC, XR, YR, ZELEV, ZHILL, ZFLAG,
     &                       X,  Y, ZELEV-ZS, HCRIT, PHEE, FOPT,
     &                       D, CWRAP*EMIFAC(1), CLIFT*EMIFAC(1),
     &                       AERVAL(1),PRMVAL(1)
C
C     Write header for plume sigma information
C
      WRITE ( DBGUNT, 330 )
c
C     Write the data that was used in the plume computations,
C      which is stability-dependent.
C
      IF( STABLE  .OR.  (UNSTAB .AND. (HS .GE. ZI) ) )THEN
         WRITE ( DBGUNT, 400 ) PPF,QTK,HE,SYAMB,VSIGY,SYB,SY,
     &      FSUBY,SZAMB,VSIGZ,SZB,SZSURF,SZ,FSUBZ,CHID

      ELSEIF ( UNSTAB ) THEN
         IF((1.0D0 - PPF) * QTK .GT. 0.0D0) THEN
            WRITE ( DBGUNT, 410 ) PPF,(1.0D0-PPF)*QTK,HED1,SYAMB,
     &         VSIGY,SYB,SY,SZAD1,VSZD1,SZBD,SZSURF,SZD1
            WRITE ( DBGUNT, 420 ) (1.0D0-PPF)*QTK,HED2,SYAMB,VSIGY,
     &         SYB,SY,FSUBYD,SZAD2,VSZD2,SZBD,SZSURF,SZD2,FSUBZD,CHID
CCRFL
CCRFL  SZSURF has been added to the indirect plume sigma z calculations--
CCRFL  add it also to the debug output for the indirect plume.
CCRFL  Changed 9/12/94.  R.F. Lee.  (Format statements 430 and 440 were
CCRFL  changed also.)
            WRITE ( DBGUNT, 430 ) PPF,(1.0D0-PPF)*QTK,HEN1,SYAMB,
     &         VSIGY,SYB,SY,SZAN1,VSZN1,SZBN,SZSURF,SZN1
            WRITE ( DBGUNT, 440 ) (1.0D0-PPF)*QTK,HEN2,SYAMB,
     &         VSIGY,SYB,SY,FSUBYN,SZAN2,VSZN2,SZBN,SZSURF,SZN2,FSUBZN,
     &         CHIN
       END IF
       IF ((PPF*QTK) .GT. 0.0D0) THEN
             WRITE ( DBGUNT, 450 ) PPF, PPF*QTK, HE3,SYA3,VSY3,
     &          SYB3,SY3,FSUBY3,SZA3,VSZ3,SZB3,SZ3,FSUBZ3,CHI3
         END IF
      END IF
C
C     FORMAT STATEMENTS
C
  101 FORMAT ( 1X )
  320 FORMAT('  REC   REC-X    REC-Y    REC-Z    HILLHT  FLAGPL    ',
     & 'DEL-X   DEL-Y  DEL-Z   HCRIT   PHEE  FOPT  DECAY   CWRAP ',
     & '     CLIFT      AERVAL    PRMVAL',/,
     & '    #    (M)      (M)      (M)      (M)     (M)       (M)   ',
     & '  (M)    (M)     (M)                       (UG/M3)    ',
     & '(UG/M3)    (UG/M3)',//,
     & I5,F9.1,F10.1,F8.1,F9.1,F8.1,F10.1,F8.1,F7.1,F7.1,F7.3,F6.3,
     & F7.3,4E11.4,/)
  330 FORMAT('   PLUME   PART.  SOURCE  PLUME  <----- SIGMA-Y TERMS --',
     &  '--->   GAUSS.     <--------- SIGMA-Z TERMS -------->   GAUSS.',
     &  /,
     &      ' COMPONENT PEN.     Q     HEIGHT  AMB.  DOWNW.  BUOY.  TO',
     &  'TAL   HORIZ.      AMB.  DOWNW.  BUOY.  SURF.  TOTAL   VERT.  ',
     &  '       CHI ',/,
     &       '   TYPE    FRAC.   (G/S)   (M)    (M)    (M)     (M)    ',
     &  '(M)    TERM        (M)    (M)     (M)    (M)    (M)    TERM ',
     &  '       (UG/M3)',/)
  400    FORMAT(' GAUSSIAN ',F6.3,F9.2,F7.1,4F7.1,E11.4,2X,5F7.1,E11.4,
     &      E12.4)
  410    FORMAT(' DIRECT #1',F6.3,F9.2,F7.1,4F7.1,13X,5F7.1)
  420    FORMAT(' DIRECT #2',6X,F9.2,F7.1,4F7.1,E11.4,2X,5F7.1,E11.4,
     &      E12.4)
  430    FORMAT(' INDIRECT1',F6.3,F9.2,F7.1,4F7.1,13X,4F7.1,
     &      F7.1)
  440    FORMAT(' INDIRECT2',6X,F9.2,F7.1,4F7.1,E11.4,2X,5F7.1,
     &      E11.4,E12.4)
  450    FORMAT(' PENETRATE',F6.3,F9.2,F7.1,4F7.1,E11.4,2X,3F7.1,7X,
     &      F7.1,E11.4,E12.4,/)
C
      RETURN
      END


      SUBROUTINE PENFCT
C***********************************************************************
C             PENFCT Module of the AMS/EPA Regulatory Model - AERMOD
C
C   PURPOSE: Calculate the plume penetration factor
C
C   PROGRAMMER: Roger Brode and Jim Paumier, PES, Inc.
C
C   DATE:    September 30, 1993
C
C   REVISED: To use VPTGZI = 0.01 for Base Case model. R.Brode, PES - 12/7/94
C
C   INPUTS:  Stability, STABLE/UNSTAB
C            Buoyancy flux, FB
C            Wind speed at release height, UP (computed in METINI)
C            Potential temperature at ZI
C            Potential temperature gradient above ZI, VPTGZI (from
C            AERMET)
C
C   OUTPUTS: Plume penetration factor, PPF
C
C   CALLED FROM:   PCALC
C
C   Assumptions:
C
C   References:   "Plume Penetration of the CBL and Source 3: Source
C                 Strength and Plume Rise", J. Weil, 9/1/93
C                 "A Dispersion Model for the Convective Boundary Layer",
C                 J. Weil, 8/17/93
C
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      DOUBLE PRECISION    BVZI2

      MODNAM = 'PENFCT'

      IF( STABLE ) THEN
         PPF = 0.0D0

      ELSE IF (UNSTAB .AND. (HS .GE. ZI) )THEN
         PPF = 1.0D0

      ELSE
C        Compute the square of the Brunt-Vaisala frequency at ZI, BVZI2

         BVZI2 = (G / PTATZI) * VPTGZI

C        Compute the value of PsubS, Eq. 26b in the 2nd reference
         PSUBS = FB / ( UP * BVZI2 * (ZI-HSP)*(ZI-HSP)*(ZI-HSP) )

C        Compute the ratio of delta(Hsub_e)/delta(Hsub_h), HEDHH
C        (Eq. 25 in the 2nd ref.
C        NOTE: 17.576 = (2.6)**3 and 0.296296 is (2/3)**3
         HEDHH = (17.576D0 * PSUBS + 0.296296D0) ** THIRD

C        Check the value of HEDHH and compute the plume penetration, P
         IF( HEDHH .LT. (2.0D0*THIRD) )THEN
            PPF = 0.0D0

         ELSE IF( HEDHH .GT. 2.0D0 )THEN
            PPF = 1.0D0

         ELSE
            PPF = 1.5D0 - (1.0D0 / HEDHH)

         END IF

      END IF

      RETURN
      END

      SUBROUTINE CPLUME (ZARG, COUT)
C***********************************************************************
C             CPLUME Module of the AMS/EPA Regulatory Model - AERMOD
C
C   PURPOSE: Calculate the contribution to the concentration due to
C            plume component, either horizontal or terrain-following,
C            depending on the input receptor height, ZARG
C
C   PROGRAMMER: Roger Brode, PES, Inc.
C
C   DATE:    September 30, 1993
C
C   REVISIONS:
C               Make stable plume reflections dependent on the
C               developmental option switch, OPTG1 & OPTG2,
C               R. Brode, PES, 1/6/95
C
C               Remove stable plume reflections off of ZI for
C               Base Case model.  R. Brode, PES - 12/7/94
C
C               Revised emission rates for each plume to QTK*(1.-PPF)
C               for the direct and indirect plumes, and to QTK*PPF
C               for the penetrated plume.  Ref:  P.D.F. Model for
C               Dispersion in the Convective Boundary Layer,
C               J.C. Weil, 6/27/94. Changes made 7/19/94, R.F. Lee.
C
C               Added true centerline concentration calculations.
C               Changes made 7/25/94, R.F. Lee.
C
C   INPUTS:  Stability, STABLE/UNSTAB
C            Fraction of plume vertical flux remaining in the CBL, FOPT
C            Mixing height, ZI
C            Plume heights, HE/HED1/HED2/HEN1/HEN2
C            sigma_Z's: SZ, SZD1, SZD2, SZN1, SZN2, SZ3
C            Receptor height, ZARG
C
C   OUTPUTS: Contribution due to WRAP, CWRAP
C
C   CALLED FROM:   PCHI
C
C   Assumptions:  For receptor height (ZR) above the mixing height (ZI)
C                 for unstable conditions, the direct and indirect plume
C                 impacts are set to zero.
C
C   References:   "A Dispersion Model for the Convective Boundary
C                 Layer", J. Weil, 8/17/93
C
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      DOUBLE PRECISION :: ZARG, COUT

      MODNAM = 'CPLUME'

C     Assign receptor height for vertical term calculations
      ZR = ZARG

      IF( STABLE  .OR.  (UNSTAB .AND. (HS .GE. ZI) ) ) THEN
C        Calculate the vertical term, FSUBZ              ---   CALL VRTSBL
C        With stable plume reflections and effective Zi
         IF (ZR .LE. HSBL) THEN
            CALL VRTSBL (SZ, MAX( 0.0D0, HE-HV ), HSBL)
         ELSE
            CALL VRTSBN (SZ, MAX( 0.0D0, HE-HV ))
         END IF

C        Calculate the concentration for a stable atmosphere
         COUT = (QTK / UEFF) * ( FSUBY * FSUBZ )

      ELSEIF( UNSTAB )THEN
         IF (PPF .LT. 1.0D0) THEN
C           Calculate the vertical term for the direct plume, FSUBZD
            IF (ZR .LE. ZI) THEN
C              Calculation for Receptor below Zi      ---   CALL VRTCBL
               CALL VRTCBL ( HED1-HV, HED2-HV, SZD1, SZD2, 1.0D0)
               FSUBZD = FSUBZ
            ELSE
C              Set FSUBZ = 0.0 for "receptor height" (ZR) > ZI
               FSUBZD = 0.0D0
            END IF

C           Calculate the vertical term for the indirect plume, FSUBZN
            IF (ZR .LE. ZI) THEN
C              Calculation for Receptor below Zi      ---   CALL VRTCBL
               CALL VRTCBL ( HEN1-HV, HEN2-HV, SZN1, SZN2, -1.0D0 )
               FSUBZN = FSUBZ
            ELSE
C              Set FSUBZ = 0.0 for "receptor height" (ZR) > ZI
               FSUBZN = 0.0D0
            END IF
         ELSE
            FSUBZD = 0.0D0
            FSUBZN = 0.0D0

         END IF

C        Note that UEFF and UEFF3 can never be zero, since they get
C        set to a minimum value earlier on.

         IF( PPF .GT. 0.0D0 )THEN
C           Calculate the vertical term for the penetrated
C           plume, FSUBZ3                                ---   CALL VRTSBL
            IF (ZR .LE. HPEN) THEN
               CALL VRTSBL (SZ3, MAX(0.0D0,HE3-HV), HPEN)
            ELSE
               CALL VRTSBN (SZ3, MAX(0.0D0,HE3-HV))
            END IF
            FSUBZ3 = FSUBZ

            IF (PPF .LT. 1.0D0) THEN
               COUT = (QTK * (1.0D0-PPF) / UEFFD) * ( FSUBYD*FSUBZD ) +
     &                (QTK * (1.0D0-PPF) / UEFFN) * ( FSUBYN*FSUBZN ) +
     &                (QTK * PPF / UEFF3) * ( FSUBY3*FSUBZ3 )

            ELSE
               COUT = (QTK * PPF / UEFF3) * ( FSUBY3*FSUBZ3 )

            END IF

         ELSE
            FSUBZ3 = 0.0D0
            HPEN   = 0.0D0
            COUT = (QTK / UEFFD) * ( FSUBYD * FSUBZD ) +
     &             (QTK / UEFFN) * ( FSUBYN * FSUBZN )

         END IF

      END IF

      RETURN
      END

      SUBROUTINE PRM_PLUME (ZARG, UEFFPH, COUT)                   ! ORD (EMM) change
C***********************************************************************
C             PRM_PLUME Module of the AMS/EPA Regulatory Model - AERMOD
C
C   PURPOSE: Calculate the contribution to the concentration due to
C            PRIME downwash component
C
C   PROGRAMMER: Roger Brode, PES, Inc.
C
C   DATE:    July 5, 2001
C
C   INPUTS:  Receptor height, ZARG
C
C   OUTPUTS: Contribution due to PRIME, COUT
C
C   CALLED FROM:   PRM_PCHI
C
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      DOUBLE PRECISION :: ZARG, COUT
      DOUBLE PRECISION :: UEFFPH

      MODNAM = 'PRM_PLUME'

C     Assign receptor height for vertical term calculations
      ZR = ZARG

      IF (STABLE) THEN
         CALL VRTSBN (SZ, HE)
      ELSE IF (UNSTAB .AND. HE.LE.ZI) THEN
         CALL VRTSBL (SZ, HE, ZI)
      ELSE
         FSUBZ = 0.0D0
      END IF

C --- Calculate the WRAP term for a stable atmosphere
C      UEFFPH is defined properly in PRM_PCHI (the calling program)
C      for regulatory AERMOD, AWMA_DOWNWASH, and ORD_DOWNWASH
C      COUT = (QTK / US) * ( FSUBY * FSUBZ )                    ! Original AERMOD
      COUT = (QTK / UEFFPH) * ( FSUBY * FSUBZ )
c      IF (AWMADWDBG) THEN
c         WRITE(AWMADWDBUNT,*) '     Contribution due to PRIME:'
c         WRITE(AWMADWDBUNT,*) '     SY=',SY,' SZ=',SZ,
c     &                        ' UEFFPH=',UEFFPH
c         WRITE(AWMADWDBUNT,*) '     FSUBY=',FSUBY,  ', FSUBZ=',FSUBZ
c         WRITE(AWMADWDBUNT,*) '     QTK / UEFFPH)*( FSUBY*FSUBZ )=',COUT
c      END IF

      RETURN
      END

      SUBROUTINE ACPLUME (ZARG, FYARG, COUT)
C***********************************************************************
C             ACPLUME Module of the AMS/EPA Regulatory Model - AERMOD
C
C   PURPOSE: Calculate the contribution to the concentration due to
C            plume component, either horizontal or terrain-following,
C            for AREA sources
C
C   PROGRAMMER: Roger Brode, PES, Inc.
C
C   DATE:    September 30, 1993
C
C   REVISIONS:
C               Make stable plume reflections dependent on the
C               developmental option switch, OPTG1 & OPTG2,
C               R. Brode, PES, 1/6/95
C
C               Remove stable plume reflections off of ZI for
C               Base Case model.  R. Brode, PES - 12/7/94
C
C               Revised emission rates for each plume to QTK*(1.-PPF)
C               for the direct and indirect plumes, and to QTK*PPF
C               for the penetrated plume.  Ref:  P.D.F. Model for
C               Dispersion in the Convective Boundary Layer,
C               J.C. Weil, 6/27/94. Changes made 7/19/94, R.F. Lee.
C
C               Added true centerline concentration calculations.
C               Changes made 7/25/94, R.F. Lee.
C
C   INPUTS:  Stability, STABLE/UNSTAB
C            Fraction of plume vertical flux remaining in the CBL, FOPT
C            Mixing height, ZI
C            Plume heights, HE/HED1/HED2/HEN1/HEN2
C            sigma_Z's: SZ, SZD1, SZD2, SZN1, SZN2, SZ3
C
C   OUTPUTS: Contribution due to WRAP, CWRAP
C
C   CALLED FROM:   ACHI
C
C   Assumptions:  For receptor height (ZR) above the mixing height (ZI)
C                 for unstable conditions, the direct and indirect plume
C                 impacts are set to zero.
C
C   References:   "A Dispersion Model for the Convective Boundary
C                 Layer", J. Weil, 8/17/93
C
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      DOUBLE PRECISION :: ZARG, FYARG, COUT

      MODNAM = 'ACPLUME'

C     Assign receptor height for vertical term calculations
      ZR = ZARG

C     Assign lateral term
      FSUBY = FYARG

      IF( STABLE  .OR.  (UNSTAB .AND. (HS .GE. ZI) ) ) THEN
C        Calculate the vertical term, FSUBZ              ---   CALL VRTSBL
C        With stable plume reflections
         IF (ZR .LE. HSBL) THEN
            CALL VRTSBL (SZ, MAX( 0.0D0, HE-HV ), HSBL)
         ELSE
            CALL VRTSBN (SZ, MAX( 0.0D0, HE-HV ))
         END IF

C        Calculate the WRAP term for a stable atmosphere
         COUT = (1.0D0 / UEFF) * ( FSUBY * FSUBZ )

      ELSEIF( UNSTAB )THEN
C        Calculate the vertical term for the direct plume, FSUBZD
         IF (ZR .LE. ZI) THEN
C           Calculation for Receptor below Zi         ---   CALL VRTCBL
            CALL VRTCBL ( HED1-HV, HED2-HV, SZD1, SZD2, 1.0D0 )
            FSUBZD = FSUBZ
         ELSE
C           Set FSUBZ = 0.0 for "receptor height" (ZR) > ZI
            FSUBZD = 0.0D0
         END IF

C        Calculate the vertical term for the indirect plume, FSUBZN
         IF (ZR .LE. ZI) THEN
C           Calculation for Receptor below Zi         ---   CALL VRTCBL
            CALL VRTCBL ( HEN1-HV, HEN2-HV, SZN1, SZN2, -1.0D0 )
            FSUBZN = FSUBZ
         ELSE
C           Set FSUBZ = 0.0 for "receptor height" (ZR) > ZI
            FSUBZN = 0.0D0
         END IF

C        Note that UEFF and UEFF3 can never be zero, since they get
C        set to a minimum value earlier on.

         FSUBZ3 = 0.0D0
         HPEN   = 0.0D0
         COUT = (1.0D0 / UEFFD) * ( FSUBY * FSUBZD ) +
     &          (1.0D0 / UEFFN) * ( FSUBY * FSUBZN )

      END IF

      RETURN
      END

      SUBROUTINE LTOPG(LSTAB)
C-----------------------------------------------------------------------
C                LTOPG Module of AERMOD Model
C
C        PURPOSE:  Converts Monin-Obukhov length to PG stability class
C                  for use with FASTAREA option and buoyant line
C                  processing, based on Golder (1972)
C
C        MODIFIED: The original LTOPG routine in AERMOD was replaced
C                  with the LSTAB function from the CTDMPLUS model, which
C                  more closely matches the PG-class curves in Figure 4
C                  of the Golder (1972) paper.
C                  R. W. Brode, U.S. EPA, OAQPS, AQMG, 02/29/2012
C
C LTOPG is based on the LSTAB function in the CTDMPLUS model:
C
C FUNCTION: LSTAB
C
C PURPOSE: THIS FUNCTION CALCULATES A P-G STABILITY CLASS GIVEN THE
C               MONIN-OBUKHOV LENGTH (L) AND THE SURFACE ROUGHNESS
C               LENGTH (Z0).
C
C ASSUMPTIONS: THE DIVIDING LINES BETWEEN CATEGORIES ARE ASSUMED TO BE
C               LINEAR.
C
C LIMITATIONS: THIS FUNCTION IS ONLY VALID FOR 0.01 <= Z0 <= 0.5(M).
C              HOWEVER, RESULTS ARE EXTENDED TO OTHER VALUES OF Z0 BY
C              USING Z0 = 0.01 IF Z0 < 0.01 M, AND BY USING Z0 = 0.5
C              IF Z0 > 0.5 M.
C
C ARGUMENTS
C  PASSED:
C       EL      REAL    MONIN-OBUKHOV LENGTH (M)
C       ZR0     REAL    SURFACE ROUGHNESS LENGTH (M)
C  RETURNED FUNCTION VALUE:
C       LSTAB   INT     P-G STABILITY CATEGORY 1=A, 2=B, ETC.
C
C CALLING ROUTINES: SEQMOD
C
C EXTERNAL ROUTINES: NONE
C
C INTERNAL FUNCTIONS:
C       XL - EQUATION OF DIVIDING LINE BETWEEN P-G STABILITY CLASSES
C
C INTRINSIC FUNCTIONS: ALOG
C
C REFERENCES:
C       GOLDER, D. (1972): RELATIONS AMONG STABILITY PARAMETERS IN THE
C                       SURFACE LAYER, BOUNDARY-LAYER METEOROLOGY, 3:56.
C
C-----------------------------------------------------------------------
C
C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
C Unused:      CHARACTER MODNAM*12

      DOUBLE PRECISION :: EL, XEL, XL, Z0, ZR0, YY, XM, B

      INTEGER :: LSTAB

      XL(YY,XM,B)=XM/(DLOG(YY)-B)
C
C
      EL  = OBULEN
      ZR0 = SFCZ0
C
      Z0 = ZR0
      IF(Z0 .GT. 0.5D0) Z0 = 0.5D0
      IF(Z0 .LT. 0.01D0) Z0 = 0.01D0
      IF(EL .LT. 0.0D0) THEN
          XEL = -EL
          IF(XEL .LE. XL(Z0,-70.0D0,4.35D0)) THEN
C             STABILITY A
              LSTAB=1
            ELSE IF(XEL .LE. XL(Z0,-85.2D0,0.502D0)) THEN
C             STABILITY B
              LSTAB=2
            ELSE IF(XEL .LE. XL(Z0,-245.0D0,0.050D0)) THEN
C             STABILITY C
              LSTAB=3
            ELSE
C             STABILITY D
              LSTAB=4
          ENDIF
        ELSE
          IF(EL .GE. XL(Z0,-327.0D0,0.627D0)) THEN
C             STABILITY D
              LSTAB=4
            ELSE IF(EL .GE. XL(Z0,-70.0D0,0.295D0)) THEN
C             STABILITY E
              LSTAB=5
            ELSE
C             STABILITY F
              LSTAB=6
          ENDIF
      ENDIF
C
      RETURN
      END

c----------------------------------------------------------------------
      subroutine VDP
c----------------------------------------------------------------------
c
c --- AERMOD     R.W. Brode, PES, Inc.
c
c --- PURPOSE:  Compute particle and gas dry deposition velocities
c               based on ANL report,
c               Wesely, et. al. (2001)
c
c --- MODIFIED: To add lower limit to dq to avoid zero-divide.
c               R. W. Brode, U.S. EPA/OAQPS/AQMG, 01/24/2007
c
c --- MODIFIED: To add SCHMIDT number as global array.
c               R. W. Brode, MACTEC (f/k/a PES), Inc., 03/19/04
c
c --- INPUTS (and other variables):
c
c      DEFINITIONS OF DRY DEPOSITION VARIABLES AND CONSTANTS
c      C0-C6 = coefficients used in computing saturation vapor pressure
c      de = water vapor deficit computed from Ta and ambient RH (kPa)
c      D_suba = diffusivity in air of gas of interest (m**2/s) (User Input)
c      D_sub_b = diffusivity in air of particle (m**2/s)
c      dq = specific humidity deficit (g/kg)
c      Dv = diffusivity of water vapor in air (0.219e-04m**2/s)
c      el = Monin-Obukhov stability length scale (m)
c      EsTa = saturation vapor pressure at the ambient temperature (kPa)
c             (calculated outside source loop and passed through MODULE MAIN1)
c      F =  factor used in specifying LAIr
c      fo = measure of reactivity
c      f1 = factor for the variation of Rs with solar irradiance
c      f2 = factor for the variation of Rs with available soil moisture (global)
c      f3 = factor for the variation of Rs with water vapor deficit
c      f4 = factor for the variation of Rs with temperature
c      QSW = solar irradiance (W/m**2) (Calculated from AERMET outputs)
c      Gr = reference solar irradiance (30 W/m**2 for forests, otherwise
c           100 W/m**2)
c      Gust_Adj = unstable gusty wind adjustment for Rd
c      HENRY = Henry's Law coefficient for gas of interest (Pa*m**3/mol)
c              (User Input)
c      ISEA5 = Wesely season category (1-5) (Based on User Input)
c      iseas = assign Wesely season category by month for the locale of the
c              meteorological data
c      LANUSE = land use category (1-9) (User Input)
c      P = ambient pressure (kPa)  (Provided by AERMET)
c      Po = reference pressure (101.3 kPa)
c      Prate = precipitation total for the current hour (mm) (Provided by AERMET)
c      prec1 = precipitation one hour back
c      prec2 = precipitation two hours back
c      q = ambient specific humidity (g/kg)
c      qsat = specific humidity at saturation (g/kg)
c      rLAI = relative leaf area index factor
c      Ra = aerodynamic resistance (s/m)
c      Rac = gas-phase aerodymanic resistance within the canopy (s/m)
c      Raci = in-canopy aerodynamic resistance appropriate for Ustar=0.3 (s/m)
c      Rb = quasiliminar resistance for bulk surface (s/m)
c      Rc = bulk surface resistance (s/m)
c      Rcl = bulk cuticle resistance to uptake associated with lipid
c            solubility (s/m)
c      Rcli = resistance to uptake by lipids in cuticles for individual
c             leaves (s/m) (User Input)
c      Rcox = cuticle resistance for ozone, wetted (s/m)
c      Rcs = bulk surface resistance for sulfur dioxide, wetted (s/m)
c      Rcut = cuticle resistance (s/m)
c      Restab = table of resistances that vary with land use and season
c               categories only.
c      Rg = ground resistance (s/m)
c      Rgo = ground resistance for ozone, wetted (s/m)
c      Rgs = ground resistance for sulfur dioxide, wetted (s/m)
c      Ri = surface resistance component (from table) (s/m)
c      RH = relative humidity (%)  (Provided by AERMET)
c      rLAI = relative leaf area index
c      Rm = mesophyll resistance (s/m)
c      Rp = resistance component for particles (s/m)
c      Rs = bulk canopy stomatal resistance (s/m)
cpes   Rx = term used to adjust components of cuticular and ground resistances
c           in the event of a hard freeze
c      S = scaling factor used to estimate cuticle resistance by land use category
c      Stab = table of S by land use category
c      Ta = ambient temperature (deg K) (Provided by AERMET)
c      Tcel = ambient temperature in celsius
c      To = reference temperature (273.16 K)
c      ustar  = friction velocity at the meteorological site (Provided by AERMET)
c      uref = wind speed at anemometer height from the meteorological site
c             (Provided by AERMET)
c      Vdepg = gaseous deposition velocity (m/s)
c      Vdep(i) = particle deposition velocity for i-th particle size category (m/s)
c      vd1 = submicron particle deposition velocity (cm/s)
c      vd2 = coarse particle deposition velocity (cm/s)
c      VONKAR = von Karman constant (0.4)
c      vp = vapor pressure (kPa)
c      Wnew = available rootzone water for current hour (mm) (calculated
c             outside source loop and passed through MODULE MAIN1)
c      Wold = available rootzone water for previous hour (mm) (global)
c      Xnu = kinematic viscosity of air (0.1505 x 10-4 m**2/s, before
c            correction for ambient temp. and pressure)
c      Zrdep = reference height (m)

c
c --- OUTPUT:  Deposition velocity for gases, Vdepg (m/s), or
c              Deposition velocity for particles, Vdep(i) (m/s) by
c                 particle size for Method 1 or for single category
c                 for Method 2
c
c --- VDP_TOX called by:  VDP
c --- VDP_TOX calls:      none
c----------------------------------------------------------------------
c
      USE MAIN1
      IMPLICIT NONE

C Unused:      DOUBLE PRECISION, PARAMETER :: a1=1.257D0, a2=0.4D0, a3=0.55D0,
C     &                               xmfp=6.5D-6
      INTEGER LANUSE, ILAND_NDX
C     JAT 7/22/21 D065 Tcel not used
C      DOUBLE PRECISION Tcel, Ri, Rcs, Rco, Rx
      DOUBLE PRECISION Ri, Rcs, Rco, Rx
      DOUBLE PRECISION Raci, Rgs, Rgo, F, rLAI, Gr
      DOUBLE PRECISION de, Rp, f1, alfa, f3, f4, ppp, Rcox, Rac
      DOUBLE PRECISION D_suba, Rb, Rcl, vd1, vd2, Sfact, D_sub_b
      DOUBLE PRECISION Stab(9), Restab(9,6,5), Dv
      DOUBLE PRECISION Po, To, Pressure, a, q, dq, qsat, vp
      INTEGER j, k, isea5

      INTEGER :: I, N
      DOUBLE PRECISION :: RA, T1, ST, XINERT,
     &        rd(npdmax), RG, RS, RC, GUST_ADJ

CCRT  Initialize variables 12/27/2017
      Sfact = 0.0D0
      rLAI =  0.0D0
      Ri =    0.0D0
      Rgs =   0.0D0
      Rgo =   0.0D0
      Rcs =   0.0D0
      Rco =   0.0D0
      Raci =  0.0D0
      isea5 = 0.0D0
      Gr =    0.0D0
      F =     0.0D0

c
C     Initialize stability factor array by land use category
      data Stab/1.0D-5,6.0D0,5.0D0,7.0D0,3.0D0,4.0D0,1.0D-5,
     &          1.0D-5,3.0D0/

C     Initialize resistance table by land use category and season
C     Split DATA statement into 2 parts to avoid continuation line limit
      data (((Restab(i,j,k),i=1,9),j=1,6),k=1,3)/
     &1.D07,   60.D0, 120.D0, 100.D0, 200.D0, 150.D0,1.D07,1.D07,80.D0,
     &1.D07, 2000.D0,2000.D0,2000.D0,2000.D0,2000.D0,1.D07,1.D07,2.5D3,
     &1.D07, 1000.D0,1000.D0,1000.D0,2000.D0,2000.D0,1.D07,1.D07, 1.D3,
     &100.D0, 200.D0, 100.D0,2000.D0, 100.D0,1500.D0,0.D0, 0.D0, 300.D0,
     &400.D0, 150.D0, 350.D0, 300.D0, 500.D0, 450.D0,0.D0,1000.D0, 0.D0,
     &300.D0, 150.D0, 200.D0, 200.D0, 300.D0, 300.D0,2.D3,400.D0, 1.D3,
     &1.D07,   1.D07,  1.D07, 350.D0,  1.D07, 700.D0,1.D07,1.D07, 1.D07,
     &1.D07, 6500.D0,6500.D0,3000.D0,2000.D0,2000.D0,1.D07,1.D07, 6.5D3,
     &1.D07,  400.D0, 300.D0, 500.D0, 600.D0,1000.D0,1.D07,1.D07,300.D0,
     &100.D0, 150.D0, 100.D0,1700.D0, 100.D0,1200.D0, 0.D0, 0.D0,200.D0,
     &400.D0, 200.D0, 350.D0, 300.D0, 500.D0, 450.D0, 0.D0,1.D3,  0.D0,
     &300.D0, 150.D0, 200.D0, 200.D0, 300.D0, 300.D0,2.0D3,400.D0,8.D2,
     &1.D07,   1.D07,  1.D07, 500.D0,  1.D07,1000.D0,1.D07, 1.D07,1.D07,
     &1.D07,   1.D07,9000.D0,6000.D0,2000.D0,2000.D0,1.D07, 1.D07, 9.D3,
     &1.D07,   1.D07, 400.D0, 600.D0, 800.D0,1600.D0,1.D07, 1.D07,8.D2,
     &100.D0,   0.D0, 100.D0,1500.D0, 100.D0,1000.D0, 0.D0,  0.D0,1.D2,
     &400.D0, 150.D0, 350.D0, 300.D0, 500.D0, 450.D0, 0.D0,  0.D0,1.D3,
     &300.D0, 150.D0, 200.D0, 200.D0, 300.D0, 300.D0,2.D3, 400.D0,1.D3/
      data (((Restab(i,j,k),i=1,9),j=1,6),k=4,5)/
     &1.D07,   1.D07,  1.D07, 800.D0,  1.D07,1600.D0,1.D07, 1.D07,1.D07,
     &1.D07,   1.D07,  1.D07, 400.D0,  1.D07, 800.D0,1.D07, 1.D07, 9.D3,
     &1.D07, 2000.D0,1000.D0, 600.D0,2000.D0,1200.D0,1.D07, 1.D07,8.D2,
     &100.D0,   0.D0,  10.D0,1500.D0, 100.D0,1000.D0, 0.D0, 0.D0, 50.D0,
     &100.D0, 100.D0, 100.D0, 100.D0, 200.D0, 200.D0, 0.D0, 1.D3,100.D0,
     &600.D0,3500.D0,3500.D0,3500.D0, 500.D0, 500.D0,2.D3,400.D0,3.5D3,
     &1.D07,  100.D0, 120.D0, 100.D0, 200.D0, 150.D0,1.D07, 1.D07,80.D0,
     &1.D07, 2000.D0,2000.D0,1500.D0,2000.D0,2000.D0,1.D07, 1.D07, 2.D3,
     &1.D07, 1000.D0, 250.D0, 350.D0, 500.D0, 700.D0,1.D07,1.D07,300.D0,
     &100.D0,  50.D0,  80.D0,1500.D0, 100.D0,1000.D0, 0.D0, 0.D0,200.D0,
     &500.D0, 150.D0, 350.D0, 300.D0, 500.D0, 450.D0, 0.D0,1.D3,  0.D0,
     &300.D0, 150.D0, 200.D0, 200.D0, 300.D0, 300.D0,2.D3, 400.D0, 1.D3/

      Dv = 0.219D-04
      Po = 101.3D0
      To = 273.16D0
CPES  Define alfa based on Eqn. 10
      alfa = 0.1D0


c ... Convert surface pressure and temperature to proper units.
      Pressure = SFCP/10.0D0
C --- Assume 100 kPa if missing
      if (Pressure .lt. 10.0D0) pressure = 100.0D0
C     JAT 7/22/21 D065 Tcel not used
C      Tcel = Ta-To

CPESc ... check to catch errors in temperature input
CPESc     This code is not used since currently no dew point data (Td)
CPESc     provided from AERMET.
CPES      if (Tcel.lt.Td .and. Td.lt.50.) then
CPES       Tcel = Tdry
CPES       Ta = Tcel+273.2
CPES      end if

      if (npd .eq. 0 .AND. LDGAS .AND. .NOT.LUSERVD) then
c ...    Assign parameters for gas deposition

c ...    Assign land use category for this direction based on user input.
c        Use method consistent with specification of IFVSEC
         ILAND_NDX = IDINT (AFV*0.10D0 + 0.4999D0)
         IF (ILAND_NDX .EQ. 0) ILAND_NDX = 36
         LANUSE = ILAND_GD(ILAND_NDX)

c ...    Assign Wesely "seasonal" category (1-5) based on calendar month
         ISEA5 = iseas_gd(imonth)

c ...    Assign surface roughness, stability factor and resistance terms
         Sfact   = Stab(LANUSE)
         Ri      = Restab(LANUSE,1,ISEA5)
         Rcs     = Restab(LANUSE,2,ISEA5)
         Rco     = Restab(LANUSE,3,ISEA5)
         Raci    = Restab(LANUSE,4,ISEA5)
         Rgs     = Restab(LANUSE,5,ISEA5)
         Rgo     = Restab(LANUSE,6,ISEA5)
c ...    Compute rLAI and reference solar irradiance as a function of
c        land use category and season.
         if (ISEA5.EQ.1 .OR. ISEA5.EQ.3 .OR. ISEA5.EQ.4) THEN
            F = 1.0D0
         else if (ISEA5 .EQ. 2) then
c           Assign user-supplied value for season 2, default it 0.50
            F = FSEAS2
         else if (ISEA5 .EQ. 5) then
c           Assign user-supplied value for season 5, default it 0.25
            F = FSEAS5
         end if
         if (LANUSE.eq.4 .or. LANUSE.eq.6) then
            rLAI = F
            Gr = 30.0D0
         else
            rLAI = DSQRT(F)
            Gr = 100.0D0
         end if

      else if (npd .eq. 0 .AND. LDGAS .AND. LUSERVD) then
         return

      end if

c ... Use Zrdep of SFCZ0 plus 1.0 meter for deposition option
      Zrdep = SFCZ0 + 1.0D0

c ... Check to avoid corruption by bad humidity input data
      if (RH.gt.100.0D0) RH = 100.0D0
      if (RH.lt.5.0D0) RH = 5.0D0

c ... Compute vapor pressure deficit
      de = ((100.0D0-RH)/100.0D0)*EsTa
      if (de.lt.0.0D0) de = 0.0D0

c ... Compute specific humidity at saturation (g/kg)
      qsat = 1.0D03*0.622D0*EsTa/(Pressure-0.378D0*EsTa)

c ... Compute ambient specific humidity (g/kg)
      vp = (RH/100.0D0)*EsTa
      q = 1.0D03*0.622D0*vp/(Pressure-0.378D0*vp)

c ... Compute specific humidity deficit (g/kg)
      dq = qsat-q
c ... For negative or zero humidity deficit, set dq=0.001 to avoid zero-divide
      if (dq.le.0.0D0) dq = 0.001D0

c ... Compute atmospheric resistance Ra

      if (obulen.ge.0.0D0) then
         Ra = (1.0D0/(VONKAR*ustar))*(DLOG(Zrdep/SFCZ0) +
     &                                 5.0D0*Zrdep/obulen)

      else
c        Ra = (1.0D0/(VONKAR*ustar))*(log(Zrdep/SFCZ0)-
c    1   2.*log(0.5D0*(1.+sqrt(1.0D0-16.0D0*(Zrdep/obulen)))))
c ...   The following is the expanded form of the unstable Ra equation (2c)
        Ra = (1.0D0/(VONKAR*ustar))*DLOG(((DSQRT(1.0D0-
     &       16.0D0*Zrdep/obulen)-1.0D0)*
     &   (DSQRT(1.0D0-16.0D0*SFCZ0/obulen)+1.0D0))/
     &  ((DSQRT(1.0D0-16.0D0*Zrdep/obulen)+1.0D0)*(DSQRT(1.0D0-
     &       16.0D0*SFCZ0/obulen)-1.0D0)))
      end if

c ... Compute kinematic viscosity of air (m**2/s), with temp and presssure corrections
      Xnu = 0.1505D0*1.0D-4*((Ta/To)**1.772D0)*(Pressure/Po)*
     &     (1.0D0+0.0132D0*(Pressure-Po))

C***
      if (npd .eq. 0 .AND. LDGAS) then

c ...    Compute gas deposition velocity, vdepg

c ...    compute parameters necessary for bulk stomatal resistance
         f1 = ((QSW/Gr)+0.01D0)/((QSW/Gr)+1.0D0)
         if (f1 .le. 0.01D0) f1 = 0.01D0
         if (f1 .gt. 1.0D0)  f1 = 1.0D0
         if (Ri .eq. 1.0D07) f1 = 0.01D0
CPES     Calculation of Wnew and f2 moved outside the source loop

         f3 = 1.0D0/(1.0D0+alfa*de)
         if (f3.le.0.01D0) f3 = 0.01D0

         f4 = 1.0D0-0.0016D0*(298.2D0-Ta)**2
         if (f4.le.0.01D0) f4 = 0.01D0


c ...    Modify certain resistances if the surface is wetted (high humidity/
c        weak mixing or rain during current or previous two hours)
         ppp = Prate+prec1+prec2

CPES     Adjust Rcs and Rco based on cloud cover from AERMET
c ...    Determine factor "a" as a function of cloud cover
         a = 0.30D0
         if (NCLOUD.le.2) a = 0.45D0
         if (NCLOUD.ge.8) a = 0.15D0
c        JAT 12/11/17
c        correct ISEAS to be 4 "winter with snow" to be consistent with science document
c         if (ISEA5 .EQ. 5 .and. IPCODE .gt.18 .and. Ta .LT. To) then
         if (ISEA5 .EQ. 4 .and. IPCODE .gt.18 .and. Ta .LT. To) then
c ...       Skip adjustments for wetted surface if surface is snow covered
            Rcox = Rco
            continue
         else if ((ustar.lt.(a/dq).and.((ihour.lt.8).or.(ihour.gt.19)))
     &       .or. ppp.gt.0.0D0) then
            Rcs  = 50.0D0
            Rcox = 0.75D0*Rco
            Rgs  = 50.0D0
            if ((ustar.lt.(a/dq)).and.
     &         ((ihour.lt.8).or.(ihour.gt.19)))then
c ...          Limit Ra for gases if surface is wetted by dew
               if (Ra.lt.1000.0D0) Ra = 1000.0D0
            end if
         else
            Rcox = Rco
         end if

c ...    Calculate Rx term, used to adjust cuticular and ground terms for
c        hard freeze
         Rx = 1.0D03*DEXP(-(Ta-269.2D0))

c ...    drive some resistances to high values if there is a hard freeze
         Rcs  = Rcs + Rx
         Rgs  = Rgs + Rx
         Rgo  = Rgo + Rx
         Rcox = Rcox + Rx

c ...    then compute in-canopy aerodynamic resistance, Racx
         Rac = 0.3D0*Raci/ustar

c ...    Assign diffusivity for this source. Note that conversion of
c        diffusivity to m2/s is made in VDP1
         D_suba = pdiff(ISRC)

c ...    Compute quasiliminar resistance for bulk surface, Rb

         Rb = 2.2D0*((Xnu/D_suba)**(2.0D0*THIRD))/(VONKAR*ustar)

c ...    Compute bulk surface resistance, Rc

c ...    First compute the bulk canopy stomatal resistance, Rs

         Rs = Ri*(Dv/D_suba)/(f1*f2*f3*f4)
         if (Rs.gt.1.0D07) Rs = 1.0D07

c ...    Next compute bulk canopy leaf mesophyll resistance, Rm
c ...    The fo factor applies to ozone (fo=1.0) and nitrogen oxide (fo=0.1).
c        The fo factor should also be set to 1.0 for titanium tetrachloride
c        and divalent mercury, otherwise fo is 0.0

         Rm = 1.0D0/((0.034D0/HENRY(ISRC))+100.0D0*fo)
         if (Rm.gt.1.0D07) Rm = 1.0D07

c ...    Then compute cuticular resistance, Rcut
c        Note that Rcli is converted from s/cm to s/m in SUB. VDP1

         Rcl = Rcli(ISRC)/(rLAI*Sfact)
c ...    Adjust Rcl for hard freeze
         Rcl = Rcl + Rx
         if (Rcl.lt.100.0D0) Rcl = 100.0D0

         Rcut = 1.0D0/((1.0D-3/(HENRY(ISRC)*Rcs))+
     &        ((fo+fo*fo/HENRY(ISRC))/Rcox)+(1.0D0/Rcl))

c ...    Next compute ground resistance Rg

         Rg=1.0D0/((1.0D-3/(HENRY(ISRC)*Rgs))+((fo+(0.1D0*fo*fo))/Rgo))
         if (Rg.gt.1.0D07) Rg = 1.0D07

c ...    Finally, combine to compute Rc

         Rc = 1.0D0/((rLAI/(Rs+Rm))+(rLAI/Rcut)+(1.0D0/(Rac+Rg)))

c ...    Add the parallel resistances and take the inverse to compute the
c        deposition velocity for gases, Vdepg.

         Vdepg = 1.0D0/(Ra+Rb+Rc)

C***
      else if (npd .gt. 0) then
c ...    Compute particle deposition velocity, vdep

         if (.NOT. L_METHOD2(ISRC)) then
c ...       Calculate using existing ISCST3 method with modified Rb (Eq. 21)

c ---       Calculate t1 using Xnu, adjusted for ambient temp and pressure
            t1 = ustar*ustar/Xnu
c
c ---       LOOP OVER SIZE INTERVALS
            do i=1,npd
c
               st=tstop(i)*t1
c
c ---          Compute inertial impaction term
               xinert=10.0D0**(-3.0D0/st)
c
c ---          Calculate Schmidt number based on ambient temp and pressure
               D_sub_b = 8.09D-14 * (TA*SCF(I)/PDIAM(I))
               Schmidt(i) = Xnu/D_sub_b

c ---          Calculate unstable gusty wind adjustment for rd,
c              set factor to 1.0 for DFAULT option
               if (wstar .le. 0.0D0) then
                  gust_adj = 1.0D0
               else
                  gust_adj = 1.0D0 + 0.24D0*wstar*wstar/(ustar*ustar)
               end if
c
c ---          Compute the deposition layer resistance (s/m)
               rd(i) = 1.0D0 / (gust_adj * ustar *
     &                       (Schmidt(i)**(-2.0D0*THIRD) + xinert))
c
c ---          Deposition velocity for this particle size category
               vdep(i) =1.0D0/(ra+rd(i)+ra*rd(i)*vgrav(i))+vgrav(i)+
     &                                                     vdphor

            end do
c ***
            if(DEBUG .OR. DEPOSDBG)then
               write(DBGUNT,*)
               write(DBGUNT,*)'RA (s/m)    = ',ra
               write(DBGUNT,*)'RD (s/m)    = ',(rd(n),n=1,npd)
               write(DBGUNT,*)'VDEP (m/s)  = ',(vdep(n),n=1,npd)
            end if

         else if (L_METHOD2(ISRC)) then
c ...       Calculate deposition velocities for fine (vd1) and
c           coarse (vd2) particles for METHOD 2
            if (obulen .ge. 0.0D0) then
               Rp = 500.0D0/ustar
            else
               Rp = 500.0D0/(ustar*(1.0D0-(300.0D0/obulen)))
            end if

c
c ---       Calculate Schmidt number based on ambient temp and pressure
            D_sub_b = 8.09D-14 * (TA*SCF(1)/PDIAM(1))
            Schmidt(1) = Xnu/D_sub_b

            vd1 = 1.0D0/(Ra+Rp)
c ...       Assign value of 0.002 m/s for Vgrav for coarse mode of METHOD 2
            vd2 = 0.002D0+(1.0D0/(Ra+Rp+0.002D0*Ra*Rp))

c ...       Combine fine and coarse terms to get total deposition velocity.
c           Note that subscript 1 for vdep is used since NPD = 1 for METHOD 2.
            vdep(1) = finemass(isrc)*vd1 + (1.0D0-finemass(isrc))*vd2
         end if

      end if

c ... Write resistances and deposition velocities to separate files:
c     GDEP.DAT for gas deposition and PDEP.DAT for particle deposition.

      IF (DEBUG .OR. DEPOSDBG) THEN
         if (LDGAS .AND. npd .eq. 0) then
            write(GDEPDBG,1001) kurdat, isrc, ra, rb, rc, vdepg
1001        format(1x,i8,1x,i6,4(2x,e12.6))
         end if
         if (LDPART .AND. .NOT. L_METHOD2(ISRC)) then
            do i = 1, npd
               write(PDEPDBG,1002) kurdat, isrc, i, ra, rd(i), vgrav(i),
     &                         vdep(i)
1002           format(1x,i8,1x,i6,2x,i3,'  METHOD_1 ',4(2x,e12.6))
            end do
         else if (LDPART .AND. L_METHOD2(ISRC)) then
            write(PDEPDBG,1003) kurdat, isrc, ra, rp, vgrav(1), vdep(1)
1003        format(1x,i8,1x,i6,4x,'-  METHOD_2 ',4(2x,e12.6))
         end if
      END IF

      return
      end


c----------------------------------------------------------------------
      subroutine scavrat
c----------------------------------------------------------------------
c
c --- AERMOD     R.W. Brode, PES
c
c --- PURPOSE:  Compute the wet SCAVenging RATio for particles, as a
c               function of particle size, and for gases, based on
c               new algorithms developed by Chris Walcek
c
c --- MODIFIED: To add calculation of collision efficiency as a
c               function of particle size and raindrop size for
c               particulate emissions, based on Slinn (1984) and
c               Seinfeld and Pandis (1998).
c               R. W. Brode, MACTEC (f/k/a PES), Inc., 03/19/04
c
c --- INPUTS:
c     Global variables:
c            IPCODE - integer    - Precip. code (00-45)
c             PRATE - real       - Precip. rate (mm/hr)
c                TA - real       - Ambient Temperature (deg K)
c               NPD - integer    - Number of particle size categories
c
c --- OUTPUT:
c     Global variables:
c            WASHOUT- real array - Washout coefficient for particles
c            PSCVRT - real array - Scavenging ratio for particles (1/s)
c            GSCVRT - real       - Scavenging ratio for gases (1/s)
c            ECOLL  - real array - Collision efficiency for particles
c
c     Local variables:
c
c --- SCAVRAT called by:  PCALC, VCALC, ACALC
c --- SCAVRAT calls:      none
c----------------------------------------------------------------------
c
c --- Include common blocks
      USE MAIN1
      IMPLICIT NONE

C --- Assign RHOW, density of water (g/m^3), as a parameter = 1.0E6
      DOUBLE PRECISION, PARAMETER :: RHOW = 1.0D6
      INTEGER :: I, N
      DOUBLE PRECISION :: VFALL, RDROP, FSUBG, FSUBL,
     &           TABS, TRES, REYNOLD, STOKE, SSTAR, KAPPA,
     &           TERM1, TERM2, TERM3

      if(DEBUG .OR. DEPOSDBG)then
         write(DBGUNT,*)
         write(DBGUNT,*)'SUBR. SCAVRAT -- Inputs'
         write(DBGUNT,*)'IPCODE               = ',ipcode
         write(DBGUNT,*)'PRATE (mm/hr)        = ',prate
         write(DBGUNT,*)'TA (deg K)           = ',ta
         write(DBGUNT,*)'NPD                  = ',npd
         write(DBGUNT,*)
      end if

c --- If no precipitation, no wet removal
      if(prate .EQ. 0.0D0) then
         if (npd .gt. 0) then
c ---       Set pscvrt(npd), washout(npd), and ecoll(npd) arrays to 0
            pscvrt = 0.0D0
            washout= 0.0D0
            ecoll  = 0.0D0
         else
            gscvrt = 0.0D0
         end if

      else if (npd .gt. 0) then
CPES --- Apply deposition option based on Wesely, et. al. (2001), with
CPES     with modifications based on Chris Walcek, for particles.
CPES     ZSUBP is calculated in PCALC as the top of the plume or the PBL
CPES     height (ZI), whichever is greater.  The top of the plume is defined
CPES     as plume centerline height plus 2.15 sigma-z, evaluated at a downwind
CPES     distance of 20 kilometers.  Since STABLE hours are modeled as
CPES     unlimited mixing, ZSUBP is simply the top of the plume for those
CPES     hours.

C ---    Calculate the precipitaion fall speed, VFALL (m/s) based on
C        precipitation rate in mm/hr.
         VFALL = 3.75D0 * PRATE**0.111D0

C ---    Calculate rainfall droplet radius, RDROP (cm), based on precipitation
C        rate in mm/hr.
         RDROP = (PRATE**0.232D0) / 18.11D0

         DO I = 1, NPD
c           JAT 6/25/19 added from 18081
c           add code to calculate terms for scavenging ratio
c           on the basis of whether Method 1 or Method 2 is used.
c           This change is added because the inputs needed for
c           scavenging ratio calculations are not input by Method 2
c           and some of the assumptions used for the calculations are
c           incorrect for Method 2.  This includes that Vg is calculated
c           for the fine particle for wet deposition but not dry deposition
c           and the coarse mode is ignored for wet deposition for method 2
c           using the current calculations.  Particle density is also
c           needed but not input by Method 2.
c           When using method 2 for dry dep and also calculating wet dep
c           the modification is to calculate washout from Wesley et al. 2001
c           and then solve for Ecoll to calculate the scavenging ratio
            if (.not. L_METHOD2(ISRC)) then
C ---       Calculate collision efficiency, ECOLL, as function of particle
C           size and raindrop size based on Slinn (1984) and Seinfeld and
C           Pandis (1998).

C ---       Calculate Reynolds number for raindrop
            REYNOLD = RDROP*0.01D0*VFALL/XNU
C           Calculate diffusion term, TERM1
            TERM1 = (4.0D0/(REYNOLD*SCHMIDT(I)))*(1.0D0+0.4D0*
     &               DSQRT(REYNOLD)*SCHMIDT(I)**THIRD+
     &               0.16D0*DSQRT(REYNOLD*SCHMIDT(I)))

C ---       Calculate ratio of particle diameter and raindrop diameter,
C           KAPPA, with adjustments for units
            KAPPA = (PDIAM(I)*1.0D-6)/(RDROP*0.02D0)
C           Calculate interception term, TERM2
C           The constant term 1.81E-2 is ratio of viscosity or air to water
            TERM2 = 4.0D0*KAPPA*(1.81D-2+KAPPA*
     &              (1.0D0+2.0D0*DSQRT(REYNOLD)))

C ---       Calculate Stokes number for raindrop
            STOKE = TSTOP(I)*(VFALL-VGRAV(I))/(RDROP*0.01D0)
C           Calculate critical Stokes number
            SSTAR = (1.2D0+DLOG(1.0D0+REYNOLD)/12.0D0)/
     &              (1.0D0+DLOG(1.0D0+REYNOLD))
            SSTAR = MIN( SSTAR, STOKE )
C           Calculate inertial impaction term, TERM3
            TERM3 = ((STOKE-SSTAR)/
     &               (STOKE-SSTAR+2.0D0*THIRD))**(1.5D0)
C           Scale TERM3, inertial impaction term,by ratio of water
C           density (1 g/cm**3) to particle density
            TERM3 = TERM3 * DSQRT(1.0D0/PDENS(I))

            ECOLL(I) = MIN( 1.0D0, TERM1 + TERM2 + TERM3 )

C ---       Calculate washout coefficient from Equation 29; factor of 0.01
C           converts drop radius from cm to m.
            WASHOUT(I) = 1.5D0 * (ZSUBP * ECOLL(I)) /
     &                         (2.0D0*RDROP*0.01D0)
            ELSE  !NEW METHOD 2 code
c             washout is calculated by 10^5*D (see Wesley et al, 2001)
c             modify to account for fine and coarse fractions
              washout(i)=1.0d5*pdiam(i)*finemass(isrc)+
     +        1.0d5*6.0D0*(1-finemass(isrc))
              ecoll(i)=((2.0D0*RDROP*0.01D0)*washout(i))/(1.5D0*ZSUBP)
              ecoll(i)=min(1.0d0,ecoll(i))
            ENDIF
            IF (PRATE .GT. 0.0D0) THEN
C ---          Calculate scavenging rate (1/s); factor of 3.6E4 converts drop
C              radius from cm to mm, and converts hours to seconds.
               PSCVRT(I) = 1.5D0 * ECOLL(I) * PRATE /
     &                          (2.0D0*RDROP * 3.6D4)
            ELSE
               PSCVRT(I) = 0.0D0
            END IF
         END DO
      else
CPES --- Apply deposition option based on Wesely, et. al. (2001),
CPES     with modifications based on Chris Walcek, for gases.
CPES     ZSUBP is calculated in PCALC as the top of the plume or the PBL
CPES     height (ZI), whichever is greater.  The top of the plume is defined
CPES     as plume centerline height plus 2.15 sigma-z, evaluated at a downwind
CPES     distance of 20 kilometers.

C ---    Calculate the precipitaion fall speed, VFALL (m/s) based on
C        precipitation rate in mm/hr.
         VFALL = 3.75D0 * PRATE**0.111D0

C ---    Calculate rainfall droplet radius, RDROP (cm), based on precipitation
C        rate in mm/hr.
         RDROP = (PRATE**0.232D0) / 18.11D0

C ---    Calculate liquid content of falling rain, LIQCONT (g/m^3), based on
C        precipitation rate in mm/hr.
         LIQCONT = (PRATE**0.889D0) / 13.28D0

C ---    Calculate gas-side diffusion enhancement factor, FSUBG (unitless),
C        based on droplet radius (cm).  Linear approximation based on
C        Figure 13-20 of "Microphysics of Clouds and Precipitation" by
C        Hans Pruppacher and James Klett.
         FSUBG = 80.0D0*RDROP + 1.0D0

C ---    Set the liquid-side diffusion enhancement factor, FSUBL (unitless),
C        based on droplet radius.
         IF (RDROP .LT. 0.01D0) THEN
            FSUBL = 1.0D0
         ELSE IF (RDROP .LE. 0.05D0) THEN
            FSUBL = 2.6D0
         ELSE
            FSUBL = 20.0D0
         END IF

C ---    Calculate the absorption time scale, TABS (s); first calculate
C        term in the denominator.
         DENOM = (1.0D0 + (LIQCONT*RGAS*TA)/(HENRY(ISRC)*RHOW))
         TABS=(RDROP**2*RGAS*TA/(HENRY(ISRC)*3.0D0*PDIFF(ISRC)*
     &        1.0D4*FSUBG)+(4.0D0*RDROP*RGAS*TA)/(HENRY(ISRC)*3.0D0*
     &        50000.0D0*0.01D0)+(RDROP**2*0.17D0)/(3.0D0*PDIFFW(ISRC)*
     &        1.0D4*FSUBL))/DENOM

C ---    Calculate the residence time of drops in the plume, TRES (s);
C        ZSUBP is defined in PCALC.
         TRES = ZSUBP/VFALL

C ---    Calculate the fraction of saturation, FRACSAT, based on time scales.
         FRACSAT = MIN( 1.0D0, TRES/TABS )

C ---    Calculate equivalent scavenging rate (1/s)
         GSCVRT = (FRACSAT*RGAS*TA*PRATE)/
     &            (3600.0D0*ZSUBP*HENRY(ISRC)*1.0D3*DENOM)

      end if

      if(DEBUG .OR. DEPOSDBG)then
         write(DBGUNT,*)'SUBR. SCAVRAT -- Results'
         if (npd .eq. 0) then
            write(DBGUNT,*)'GSCVRT (1/s)= ',gscvrt
         else if (npd .gt. 0) then
            write(DBGUNT,*)'PSCVRT (1/s)= ',(pscvrt(n),n=1,npd)
            write(DBGUNT,*)'COLL. EFF.  = ',(ecoll(n),n=1,npd)
            write(DBGUNT,*)'WASHOUT COEF= ',(washout(n),n=1,npd)
         end if
         write(DBGUNT,*)
      end if

      return
      end

      SUBROUTINE PDEP (XARG)
C***********************************************************************
C               PDEP Module of AERMOD Model
C
C        PURPOSE: Calculates Deposition Adjustment Factors from DEPLETE
C
C        PROGRAMMER: R. W. Brode, MACTEC/PES, Inc.
C
C        DATE:       September 20, 2003
C
C
C        INPUTS:
C
C
C        OUTPUTS:
C
C
C        CALLED FROM:
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I
      DOUBLE PRECISION :: XARG

C     Variable Initializations
      MODNAM = 'PDEP'

C     Loop over particle sizes
      DO I = 1, NPD
         DQCOR(I) = 1.0D0
         WQCOR(I) = 1.0D0
         IF (DDPLETE) THEN
C           Determine factor for dry depletion
            VSETL = VGRAV(I)
            CALL DEPLETE(VDEP(I),XARG,ROMBERG,DQCOR(I))
         END IF
         IF (WDPLETE .AND. PSCVRT(I).GT.0.0D0) THEN
C           Determine source depletion factor from wet removal
C           Simple Terrain Model
            WQCOR(I) = DEXP(-PSCVRT(I)*XARG/US)
         END IF
      END DO

      RETURN
      END


      SUBROUTINE PDEPG (XARG)
C***********************************************************************
C               PDEPG Module of AERMOD Model
C
C        PURPOSE: Calculates Deposition Adjustment Factors from DEPLETE
C                 for Gases
C
C        PROGRAMMER: R. W. Brode, MACTEC/PES, Inc.
C
C        DATE:       September 29, 2003
C
C        INPUTS:
C
C
C        OUTPUTS:
C
C
C        CALLED FROM:
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      DOUBLE PRECISION :: XARG

C     Variable Initializations
      MODNAM = 'PDEPG'

C     Initialize source depletion factors to unity.
      DQCORG  = 1.0D0
      WQCORG  = 1.0D0
      IF (DDPLETE) THEN
C        Determine factor for dry depletion
         CALL DEPLETE( VDEPG, XARG, ROMBERG, DQCORG)
      END IF
      IF (WDPLETE .AND. GSCVRT.GT.0.0D0) THEN
C        Determine source depletion factor
C        from wet removal (GASES)
C        Simple Terrain Model
         WQCORG = DEXP(-GSCVRT*XARG/US)
      END IF

      RETURN
      END

c-----------------------------------------------------------------------
      subroutine deplete(vdi, xri, lromb, qcor)
c-----------------------------------------------------------------------
c
c --- DEPLETE Module of AERMOD
c              R.W. Brode, MACTEC/PES
c
c PURPOSE:     Subroutine DEPLETE provides the value of the integral of
c              the vertical distribution function over the travel of the
c              plume from the source to the receptor.  Integration is
c              performed by 2-point gaussian quadrature or Romberg
c              integration method, depending on logical argument, lromb.
c
c ARGUMENTS:
c    PASSED:   vdi      deposition velocity (m/s)              [r]
c              vsi      gravitational settling velocity (m/s)  [r]
c              xri      distance from source to receptor (m)   [r]
c              hmixi    mixing height (m)                      [r]
c              lromb    logical for use of Romberg integration [l]
c
c  RETURNED:   qcor     ratio of depleted emission rate to original  [r]
c
c CALLING ROUTINES:   PDEP, PDEPG
c
c EXTERNAL ROUTINES:  F2INT, QATR2, QG2D2
c-----------------------------------------------------------------------

c     Set up call to QATR2(xl,xu,eps,ndim2,fct,y,ier,num,aux2)
c     Declare parameter to fix the size of the aux2 array
      IMPLICIT NONE

      LOGICAL LROMB
      DOUBLE PRECISION :: VDI, XRI, QCOR, EPS, VALUE
      DOUBLE PRECISION, external :: F2INT
      INTEGER NUM, IER
      INTEGER, parameter :: ndim2=12
      DOUBLE PRECISION aux2(ndim2)

C     Evaluate integral, Use Romberg if LROMB=.T., otherwise use
C     two-point Gaussian Quadrature:
      IF (LROMB) THEN
C        Use ROMBERG Integration
         eps = 0.050D0
         call QATR2(1.0D0,xri,eps,ndim2,F2INT,value,ier,num,aux2)
      ELSE
C        Use 2-point Gaussian Quadrature
         CALL QG2D2(1.0D0,XRI,F2INT,VALUE)
      END IF

      if (vdi*value .gt. 50.0D0) then
c        Potential underflow, limit product to 50.0
         value = 50.0D0/vdi
      else if (vdi*value .lt. -50.0D0) then
c        Potential overflow, limit product to 50.0
         value = -50.0D0/vdi
      end if

      qcor=DEXP(-vdi*value)

      return
      end

c-----------------------------------------------------------------------
      function f2int(xi)
c-----------------------------------------------------------------------
c
c --- F2INT Module of AERMOD
c              R.W. Brode, MACTEC/PES
c
c PURPOSE:     Function is the integrand of integral over the travel
c              distance to obtain the fraction of material removed from
c              the plume. Module MAIN1 is used to pass data that are
c              constant during the integration, so QATR (the integrator)
c              only needs to pass values of distance.
c
c ARGUMENTS:
c    PASSED:  xi        distance from source                         [r]
c
c  RETURNED:  f2int     value of integrand                           [r]
c
c CALLING ROUTINES:   QATR2, QG2D2
c
c EXTERNAL ROUTINES:
c
c-----------------------------------------------------------------------
c
      USE MAIN1
      IMPLICIT NONE

      DOUBLE PRECISION XI, VWRAP, VLIFT, F2INT, ZETMP, ZHTMP

C     Initialize VWRAP and VLIFT
      VWRAP = 0.0D0
      VLIFT = 0.0D0

C     Set initial effective parameters
      UEFF  = US
      SVEFF = SVS
      SWEFF = SWS
      TGEFF = TGS
      IF ( UNSTAB  .AND.  (HS .LT. ZI) ) THEN
         UEFFD  = US
         SVEFFD = SVS
         SWEFFD = SWS
         UEFFN  = US
         SVEFFN = SVS
         SWEFFN = SWS
         UEFF3  = US
         SVEFF3 = SVS
         SWEFF3 = SWS
         TGEFF3 = TGS
      END IF

C     Set temporary receptor elevation and height scale
      ZETMP = ZELEV
      ZELEV = ZS + (ZETMP-ZS)*XI/XDIST

      ZHTMP = ZHILL
      ZHILL = HS + (ZHTMP-HS)*XI/XDIST

C     Define plume centroid height (CENTER) for use in
C     inhomogeneity calculations
      CALL CENTROID ( XI )

      IF (SRCTYP(ISRC)(1:5) .EQ. 'POINT') THEN
C        Calculate the plume rise                     ---   CALL DELTAH
         CALL DELTAH ( XI )
      END IF

C     If the atmosphere is unstable and the stack
C     top is below the mixing height, calculate
C     the CBL PDF coefficients                     ---   CALL PDF
      IF( UNSTAB  .AND.  (HS .LT. ZI) ) THEN
         CALL PDF
      END IF

C     Determine Effective Plume Height             ---   CALL HEFF
      CALL HEFF ( XI )

C     Compute effective parameters using an
C     average through plume layer
      CALL IBLVAL ( XI )

C     Call PDF & HEFF again for final CBL plume heights
      IF (UNSTAB .AND. (HS.LT.ZI) ) THEN
         CALL PDF
         CALL HEFF ( XI )
      END IF

C     Determine Dispersion Parameters              ---   CALL PDIS
      CALL PDIS ( XI )

C     Calculate Plume Tilt Due to Settling, HV
      HV = (XI/US) * VSETL

      IF (STABLE .OR. (UNSTAB.AND.(HS.GE.ZI))) THEN
C        Calculate height of the "effective reflecting surface"
C        Calculate Settled Plume Height(s), HESETL
         HESETL = MAX( 0.0D0, HE - HV )
         CALL REFL_HT (HESETL, XI, SZB, 0.0D0, HSBL)
      ELSE IF ( UNSTAB ) THEN
         HESETL = MAX( 0.0D0, 0.5D0*(HED1+HED2) - HV )
         HSBL = 0.0D0
      END IF

      IF (UNSTAB .AND. (HS.LT.ZI) .AND. (PPF.GT.0.0D0)) THEN
C        Calculate height of the "effective reflecting surface"
C        Calculate Settled Plume Height(s), HE3SETL
         HE3SETL = MAX( 0.0D0, HE3 - HV )
         CALL REFL_HT (HE3SETL, XI, SZB3, 0.0D0, HPEN)
         HPEN = MAX( HPEN, ZI )
      ELSE
         HPEN = 0.0D0
      END IF

C     Determine the CRITical Dividing Streamline---   CALL CRITDS
      CALL CRITDS (HESETL)

C     Calculate the fraction of plume below
C     HCRIT, PHEE                               ---   CALL PFRACT
      CALL PFRACT (HESETL)

C     Calculate FOPT = f(PHEE)                  ---   CALL FTERM
      CALL FTERM

      IF (FOPT .EQ. 0.0D0) THEN
         VWRAP = 0.0D0
      ELSE
C        Assign receptor height for vertical term calculations
         ZR = ZRT + ZRDEP

         IF( STABLE  .OR.  (UNSTAB .AND. (HS .GE. ZI) ) ) THEN
C           Calculate the vertical term, FSUBZ              ---   CALL VRTSBL
C           With stable plume reflections and effective Zi
            IF (ZR .LE. HSBL) THEN
               CALL VRTSBL (SZ, MAX( 0.0D0, HE-HV ), HSBL)
            ELSE
               CALL VRTSBN (SZ, MAX( 0.0D0, HE-HV ))
            END IF

C           Calculate value of integral, VWRAP
            VWRAP = FSUBZ/UEFF

         ELSE IF( UNSTAB )THEN
            IF (PPF .LT. 1.0D0) THEN
C              Calculate the vertical term for the direct plume, FSUBZD
               IF (ZR .LE. ZI) THEN
C                 Calculation for Receptor below Zi      ---   CALL VRTCBL
                  CALL VRTCBL ( HED1-HV, HED2-HV, SZD1, SZD2, 1.0D0 )
                  FSUBZD = FSUBZ
               ELSE
C                 Set FSUBZ = 0.0 for "receptor height" (ZR) > ZI
                  FSUBZD = 0.0D0
               END IF

C              Calculate the vertical term for the indirect plume, FSUBZN
               IF (ZR .LE. ZI) THEN
C                 Calculation for Receptor below Zi      ---   CALL VRTCBL
                  CALL VRTCBL ( HEN1-HV, HEN2-HV, SZN1, SZN2, -1.0D0 )
                  FSUBZN = FSUBZ
               ELSE
C                 Set FSUBZ = 0.0 for "receptor height" (ZR) > ZI
                  FSUBZN = 0.0D0
               END IF
            ELSE
               FSUBZD = 0.0D0
               FSUBZN = 0.0D0

            END IF

C           Note that UEFF and UEFF3 can never be zero, since they get
C           set to a minimum value earlier on.

            IF( PPF .GT. 0.0D0 )THEN
C              Calculate the vertical term for the penetrated
C              plume, FSUBZ3                                ---   CALL VRTSBL
               IF (ZR .LE. HPEN) THEN
                  CALL VRTSBL (SZ3, MAX(0.0D0,HE3-HV), HPEN)
               ELSE
                  CALL VRTSBN (SZ3, MAX(0.0D0,HE3-HV))
               END IF
               FSUBZ3 = FSUBZ

C              Calculate value of integral, VWRAP
               IF (PPF .LT. 1.0D0) THEN
                  VWRAP = (1.0D0-PPF)*FSUBZD/UEFFD +
     &                    (1.0D0-PPF)*FSUBZN/UEFFN +
     &                    PPF*FSUBZ3/UEFF3
               ELSE
                  VWRAP = PPF*FSUBZ3/UEFF3
               END IF

            ELSE
               FSUBZ3 = 0.0D0
               HPEN   = 0.0D0

C              Calculate value of integral, VWRAP
               VWRAP = FSUBZD/UEFFD +
     &                 FSUBZN/UEFFN

            END IF

         END IF
      END IF

C---- Calculate the contribution due to terrain-following plume, VLIFT
      IF (ZRT .EQ. 0.0D0) THEN
C----    Effective receptor heights are equal, therefore VLIFT = VWRAP
         VLIFT = VWRAP
      ELSE IF (FOPT .EQ. 1.0D0) THEN
         VLIFT = 0.0D0
      ELSE
C        Assign receptor height for vertical term calculations
         ZR = ZRDEP

         IF( STABLE  .OR.  (UNSTAB .AND. (HS .GE. ZI) ) ) THEN
C           Calculate the vertical term, FSUBZ              ---   CALL VRTSBL
C           With stable plume reflections and effective Zi
            IF (ZR .LE. HSBL) THEN
               CALL VRTSBL (SZ, MAX( 0.0D0, HE-HV ), HSBL)
            ELSE
               CALL VRTSBN (SZ, MAX( 0.0D0, HE-HV ))
            END IF

C           Calculate value of integral, VLIFT
            VLIFT = FSUBZ/UEFF

         ELSE IF( UNSTAB )THEN
            IF (PPF .LT. 1.0D0) THEN
C              Calculate the vertical term for the direct plume, FSUBZD
               IF (ZR .LE. ZI) THEN
C                 Calculation for Receptor below Zi      ---   CALL VRTCBL
                  CALL VRTCBL ( HED1-HV, HED2-HV, SZD1, SZD2, 1.0D0 )
                  FSUBZD = FSUBZ
               ELSE
C                 Set FSUBZ = 0.0 for "receptor height" (ZR) > ZI
                  FSUBZD = 0.0D0
               END IF

C              Calculate the vertical term for the indirect plume, FSUBZN
               IF (ZR .LE. ZI) THEN
C                 Calculation for Receptor below Zi      ---   CALL VRTCBL
                  CALL VRTCBL ( HEN1-HV, HEN2-HV, SZN1, SZN2, -1.0D0 )
                  FSUBZN = FSUBZ
               ELSE
C                 Set FSUBZ = 0.0 for "receptor height" (ZR) > ZI
                  FSUBZN = 0.0D0
               END IF
            ELSE
               FSUBZD = 0.0D0
               FSUBZN = 0.0D0

            END IF

C           Note that UEFF and UEFF3 can never be zero, since they get
C           set to a minimum value earlier on.

            IF( PPF .GT. 0.0D0 )THEN
C              Calculate the vertical term for the penetrated
C              plume, FSUBZ3                                ---   CALL VRTSBL
               IF (ZR .LE. HPEN) THEN
                  CALL VRTSBL (SZ3, MAX(0.0D0,HE3-HV), HPEN)
               ELSE
                  CALL VRTSBN (SZ3, MAX(0.0D0,HE3-HV))
               END IF
               FSUBZ3 = FSUBZ

C              Calculate value of integral, VLIFT
               IF (PPF .LT. 1.0D0) THEN
                  VLIFT = (1.0D0-PPF)*FSUBZD/UEFFD +
     &                    (1.0D0-PPF)*FSUBZN/UEFFN +
     &                    PPF*FSUBZ3/UEFF3
               ELSE
                  VLIFT = PPF*FSUBZ3/UEFF3
               END IF

            ELSE
               FSUBZ3 = 0.0D0
               HPEN   = 0.0D0

C              Calculate value of integral, VLIFT
               VLIFT = FSUBZD/UEFFD +
     &                 FSUBZN/UEFFN

            END IF

         END IF
      END IF

C     Blend horizontal and terrain-responding components of integral
      F2INT = FOPT * VWRAP + (1.0D0 - FOPT) * VLIFT

C     Reassign receptor elevation and height scales
      ZELEV = ZETMP
      ZHILL = ZHTMP

      RETURN
      END

      SUBROUTINE PRM_PDEP (XARG)
C***********************************************************************
C               PRM_PDEP Module of AERMOD Model
C
C        PURPOSE: Calculates Deposition Adjustment Factors from
C                 PRM_DEPLETE for PRIME component
C
C        PROGRAMMER: R. W. Brode, PES, Inc.
C
C        DATE:       September 29, 1994
C
C
C        INPUTS:
C
C
C        OUTPUTS:
C
C
C        CALLED FROM:
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I
      DOUBLE PRECISION :: XARG

C     Variable Initializations
      MODNAM = 'PRM_PDEP'

C     Loop over particle sizes
      DO I = 1, NPD
         DQCOR(I) = 1.0D0
         WQCOR(I) = 1.0D0
         IF (DDPLETE) THEN
C           Determine factor for dry depletion
            VSETL = VGRAV(I)
            CALL PRM_DEPLETE(VDEP(I),XARG,ROMBERG,DQCOR(I))
         END IF
         IF (WDPLETE .AND. PSCVRT(I).GT.0.0D0) THEN
C           Determine source depletion factor from wet removal
C           Simple Terrain Model
            WQCOR(I) = DEXP(-PSCVRT(I)*XARG/US)
         END IF
      END DO

      RETURN
      END


      SUBROUTINE PRM_PDEPG (XARG)
C***********************************************************************
C               PRM_PDEPG Module of AERMOD Model
C
C        PURPOSE: Calculates Deposition Adjustment Factors from
C                 PRM_DEPLETE for PRIME component for gases
C
C        PROGRAMMER: R. W. Brode, MACTEC/PES, Inc.
C
C        DATE:       September 29, 2003
C
C        INPUTS:
C
C
C        OUTPUTS:
C
C
C        CALLED FROM:
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      DOUBLE PRECISION :: XARG

C     Variable Initializations
      MODNAM = 'PRM_PDEPG'

C     Initialize source depletion factors to unity.
      DQCORG  = 1.0D0
      WQCORG  = 1.0D0
      IF (DDPLETE) THEN
C        Determine factor for dry depletion
         CALL PRM_DEPLETE( VDEPG, XARG, ROMBERG, DQCORG)
      END IF
      IF (WDPLETE .AND. GSCVRT.GT.0.0D0) THEN
C        Determine source depletion factor
C        from wet removal (GASES)
C        Simple Terrain Model
         WQCORG = DEXP(-GSCVRT*XARG/US)
      END IF

      RETURN
      END

c-----------------------------------------------------------------------
      subroutine PRM_deplete(vdi, xri, lromb, qcor)
c-----------------------------------------------------------------------
c
c --- PRM_DEPLETE Module of AERMOD
c
c
c PURPOSE:     Subroutine PRM_DEPLETE provides the value of the integral of
c              the vertical distribution function over the travel of the
c              plume from the source to the receptor for the PRIME downwash
c              component.  Integration is performed by 2-point gaussian
c              quadrature or Romberg integration method, depending on
c              logical argument, lromb.
c
c ARGUMENTS:
c    PASSED:   vdi      deposition velocity (m/s)              [r]
c              vsi      gravitational settling velocity (m/s)  [r]
c              xri      distance from source to receptor (m)   [r]
c              hmixi    mixing height (m)                      [r]
c              lromb    logical for use of Romberg integration [l]
c
c  RETURNED:   qcor     ratio of depleted emission rate to original  [r]
c
c CALLING ROUTINES:   PRM_PDEP, PRM_PDEPG
c
c EXTERNAL ROUTINES:  PRM_F2INT, QATR2, QG2D2
c-----------------------------------------------------------------------

c     Set up call to QATR2(xl,xu,eps,ndim2,fct,y,ier,num,aux2)
c     Declare parameter to fix the size of the aux2 array
      IMPLICIT NONE

      LOGICAL LROMB
      DOUBLE PRECISION :: VDI, XRI, QCOR, EPS, VALUE
      DOUBLE PRECISION, external :: PRM_F2INT
      INTEGER NUM, IER
      INTEGER, parameter :: ndim2=12
      DOUBLE PRECISION aux2(ndim2)

C     Evaluate integral, Use Romberg if LROMB=.T., otherwise use
C     two-point Gaussian Quadrature:
      IF (LROMB) THEN
C        Use ROMBERG Integration
         eps = 0.05D0
         call QATR2(1.0D0,xri,eps,ndim2,PRM_F2INT,value,ier,num,aux2)
      ELSE
C        Use 2-point Gaussian Quadrature
         CALL QG2D2(1.0D0,XRI,PRM_F2INT,VALUE)
      END IF

      if (vdi*value .gt. 50.0D00) then
c        Potential underflow, limit product to 50.0
         value = 50.0D0/vdi
      else if (vdi*value .lt. -50.0D0) then
c        Potential overflow, limit product to 50.0
         value = -50.0D0/vdi
      end if

      qcor=DEXP(-vdi*value)

      return
      end

c-----------------------------------------------------------------------
      function PRM_f2int(xi)
c-----------------------------------------------------------------------
c
c --- PRM_F2INT Module of AERMOD
c              R.W. Brode, MACTEC/PES
c
c PURPOSE:     Function is the integrand of integral over the travel
c              distance to obtain the fraction of material removed from
c              the plume for the PRIME downwash component. Module MAIN1
c              is used to pass data that are constant during the
c              integration, so QATR (the integrator) only needs to
c              pass values of distance.
c
c ARGUMENTS:
c    PASSED:  xi         distance from source                        [r]
c
c  RETURNED:  PRM_f2int  value of integrand                          [r]
c
c CALLING ROUTINES:   QATR2, QG2D2
c
c EXTERNAL ROUTINES:
c
c-----------------------------------------------------------------------
      USE MAIN1
      IMPLICIT NONE

      DOUBLE PRECISION :: XI, VWRAP, VLIFT, PRM_F2INT, ZETMP, ZHTMP
      DOUBLE PRECISION :: DHPOUT, SYOUT, SZOUT, SYCAV, SZCAV
      LOGICAL L_INWAKE

C     Set temporary receptor elevation and height scale
      ZETMP = ZELEV
      ZELEV = ZS + (ZETMP-ZS)*XI/X

      ZHTMP = ZHILL
      ZHILL = HS + (ZHTMP-HS)*XI/X

C     Calculate the plume rise                     ---   CALL PRMDELH
      CALL PRMDELH ( XI, L_INWAKE )

C     Determine Effective Plume Height             ---   CALL PRMHEFF
      CALL PRMHEFF

c --- Calculate sigmas
      dhpout = dhp
      call WAKE_XSIG(XI,hs,dhpout,nobid,szout,syout,
     &               szcav,sycav)
      sy = syout
      sz = szout

C     Calculate Plume Tilt Due to Settling, HV
      HV = (XI/US) * VSETL
      HE = MAX( 0.0D0, HE - HV )

C     Calculate FOPT = f(PHEE)                  ---   CALL FTERM
      FOPT = 0.5D0

C     Assign receptor height for vertical term calculations
      ZR = ZRT + ZRDEP

      IF (STABLE) THEN
         CALL VRTSBN (SZ, HE)
      ELSE IF (UNSTAB .AND. HE.LE.ZI) THEN
         CALL VRTSBL (SZ, HE, ZI)
      ELSE
         FSUBZ = 0.0D0
      END IF

      VWRAP = FSUBZ/UEFF

C---- Calculate the contribution due to terrain-following plume, VLIFT
      IF (ZRT .EQ. 0.0D0) THEN
C----    Effective receptor heights are equal, therefore VLIFT = VWRAP
         VLIFT = VWRAP
      ELSE IF (FOPT .EQ. 1.0D0) THEN
         VLIFT = 0.0D0
      ELSE
C        Assign receptor height for vertical term calculations
         ZR = ZRDEP
         IF (STABLE) THEN
            CALL VRTSBN (SZ, HE)
         ELSE IF (UNSTAB .AND. HE.LE.ZI) THEN
            CALL VRTSBL (SZ, HE, ZI)
         ELSE
            FSUBZ = 0.0D0
         END IF
         VLIFT = FSUBZ/UEFF
      END IF

C     Blend horizontal and terrain-responding components of integral
      PRM_F2INT = FOPT * VWRAP + (1.0D0 - FOPT) * VLIFT

C     Reassign receptor elevation and height scales
      ZELEV = ZETMP
      ZHILL = ZHTMP

      RETURN
      END

c-----------------------------------------------------------------------
      subroutine qatr2(xl,xu,eps,ndim,fct,y,ier,i,aux)
c-----------------------------------------------------------------------
c
c --- AERMOD    QATR2
c
c PURPOSE:      Integration routine adapted from the IBM SSP program
c               DQATR.  Modified for single precision.  This is a COPY
c               of QATR for use in double integrations.
c
c MODIFIED:     To use new convergence criteria, including a lower
c               threshold in the value of the integral (1.0E-10), and
c               to check for "delta-x" < 1.0 meters (delta-x = hh).
c               R. W. Brode, PES, Inc. - 9/30/94
c
c ARGUMENTS:
c    PASSED:    xl,xu   lower and upper limits of integration        [r]
c               eps     fractional error used to define convergence  [r]
c               ndim    dimension of array aux (parameter)           [p]
c               fct     external function (integrand)
c               aux     working array, passed to allow variable dim. [r]
c  RETURNED:    y       value of integral                            [r]
c               ier     status flag at terminatio                    [i]
c               i       number of subdivision steps                  [i]
c
c CALLING ROUTINES:     DEPLETE
c
c EXTERNAL ROUTINES:    none
c-----------------------------------------------------------------------

c  NOTES: status flags denote the following --
c               ier=0   value of integral converged to within eps
c               ier=1   value of integral is diverging
c               ier=2   value of integral did not converge to within
c                       eps before ndim limit was reached

c  NDIM Note:  The aux(ndim) array keeps track of the average value of
c              the integrand for each of the steps in subdividing the
c              interval.  For example, when i=4 in the "do 7 i=2,ndim"
c              loop, aux(4) contains the mean value as obtained from
c              the trapezoidal rule, while aux(1 through 3) contain
c              a set of current Romberg extrapolations.  At each new
c              value of i, the interval is subdivided again, and the
c              integrand is evaluated at jj=2**(i-2) new points.
c              Therefore, at i=5, there will be jj=8 new points added
c              to the 9 points already used in the interval.  When i=17
c              there will be jj=32,768 new points added to the 32,769
c              already used.  This is the maximum number of new points
c              that are allowed as jj is an INTEGER*2 variable, with
c              a maximum value of 2**15.  Therefore, i should not exceed
c              17, and probably should be no larger than 16.  This means
c              that NDIM should be set at 16.  Larger values of NDIM
c              could be accepted if the INTEGER*2 variables were changed
c              to INTEGER*4, but for most applications, 30000 to 60000
c              points ought to be sufficient for evaluating an integral.

      IMPLICIT NONE

      INTEGER :: NDIM, IER
      DOUBLE PRECISION :: Y, EPS, XU, XL, AUX(NDIM), HALF, FCT, H, HH,
C     JAT 7/22/21 D065, DELT1 NOT USED
C     &                    DELT2, P, DELT1, HD, X, SM, Q
     &                    DELT2, P, HD, X, SM, Q
      EXTERNAL fct
      integer i,ii,ji,j,jj
      half=0.5D0

c     Preparations for Romberg loop
      aux(1)=half*(fct(xl)+fct(xu))
      h=xu-xl

      if(h .EQ. 0.0D0 .OR. aux(1) .EQ. 0.0D0) then
         ier=0
         y = 0.0D0
         return
      end if

      hh=h
      delt2=0.0D0
      p=1.0D0
      jj=1

      do i=2,ndim
         y=aux(1)
C     JAT 7/22/21 D065, DELT1 NOT USED
c        delt1=delt2
         hd=hh
         hh=half*hh
         p=half*p
         x=xl+hh
         sm=0.0D0

         do j=1,jj
            sm=sm+fct(x)
            x=x+hd
         end do

c  A new approximation to the integral is computed by means
c  of the trapezoidal rule
         aux(i)=half*aux(i-1)+p*sm

c  Start of Rombergs extrapolation method

         q=1.0D0
         ji=i-1
         do j=1,ji
            ii=i-j
            q=q+q
            q=q+q
            aux(ii)=aux(ii+1)+(aux(ii+1)-aux(ii))/(q-1.0D0)
         end do

c  End of Romberg step

         delt2=DABS(y-aux(1))

         if (i .GE. 3) then
c  Modification for cases in which function = 0 over interval
crwb        add lower threshold convergence test
            if (aux(1) .LT. 1.0D-10) then
               ier=0
               y=h*aux(1)
               return
            elseif (delt2 .LE. eps*DABS(aux(1)) ) then
               ier=0
               y=h*aux(1)
               return
crwb        add lower limit on "delta-x" of 1.0m
            elseif (hh .LT. 1.0D0) then
               ier=0
               y=h*aux(1)
               return
c           elseif (delt2 .GE. delt1)then
c              ier=1
c              y=h*y
c              return
            endif
         endif
        jj=jj+jj
      end do

      ier=2
      y=h*aux(1)

      return
      end



      SUBROUTINE QG2D2(XL,XU,FCT,Y)
C     ..................................................................
C
C        SUBROUTINE QG2D2
C
C        PURPOSE
C           TO COMPUTE INTEGRAL(FCT(X), SUMMED OVER X FROM XL TO XU)
C
C        USAGE
C           CALL QG2 (XL,XU,FCT,Y)
C           PARAMETER FCT REQUIRES AN EXTERNAL STATEMENT
C
C        DESCRIPTION OF PARAMETERS
C           XL     - THE LOWER BOUND OF THE INTERVAL.
C           XU     - THE UPPER BOUND OF THE INTERVAL.
C           FCT    - THE NAME OF AN EXTERNAL FUNCTION SUBPROGRAM USED.
C           Y      - THE RESULTING INTEGRAL VALUE.
C
C        REMARKS
C           NONE
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           THE EXTERNAL FUNCTION SUBPROGRAM FCT(X) MUST BE FURNISHED
C           BY THE USER.
C
C        METHOD
C           EVALUATION IS DONE BY MEANS OF 2-POINT GAUSS QUADRATURE
C           FORMULA, WHICH INTEGRATES POLYNOMIALS UP TO DEGREE 3
C           EXACTLY.
C           FOR REFERENCE, SEE
C           V.I.KRYLOV, APPROXIMATE CALCULATION OF INTEGRALS,
C           MACMILLAN, NEW YORK/LONDON, 1962, PP.100-111 AND 337-338.
C
C     ..................................................................
C
C
      IMPLICIT NONE

      DOUBLE PRECISION :: A, B, Y, XL, XU, FCT
      EXTERNAL FCT

      A = 0.5D0*(XU+XL)
      B = XU-XL
      Y = 0.2886751D0*B
      Y = 0.5D0*B*(FCT(A+Y)+FCT(A-Y))

      RETURN
      END

      SUBROUTINE OLM_CALC
C***********************************************************************
C             OLM_CALC Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes Hourly Results for OLM Option
C
C        PROGRAMMER: Roger W. Brode, PES, Inc.
C
C        DATE:    May 6, 2002
C
C        MODIFIED: Incorporated equilibrium ratio for OLM option,
C                  with default of 0.90, as with PVMRM option.
C                  R. W. Brode, U.S. EPA, OAQPS, AQMG, 02/28/2011
C
C        MODIFIED: Corrected initialization problem with OLMGROUP
C                  keyword option.  NO2VAL and NO_VAL arrays need
C                  to be reinitialized for each receptor.
C                  R. W. Brode, U.S. EPA, OAQPS, AQMG, 10/19/2009
C
C        INPUTS:
C
C
C        OUTPUTS:
C
C
C        CALLED FROM:   HRLOOP
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12, BLNK8*8
      INTEGER :: IASTAT
C --- Declare allocatable arrays that are needed if OLMGROUPs
C     are used
      DOUBLE PRECISION, ALLOCATABLE :: OLMVAL(:,:),
     &                                 NO2VAL(:), NO_VAL(:),
     &                                 OLMGrp_PercentNO2(:)
C     JAT 7/22/21 D065, BCKGRD AND DUMVAL NOT USED
      DOUBLE PRECISION :: NO2, NO, OLMTEMP, PercentNO2
c      DOUBLE PRECISION :: NO2, NO, OLMTEMP, PercentNO2, BCKGRD,
c     &                                                  DUMVAL

C     Variable Initializations
      MODNAM = 'OLM_CALC'
C     JAT 7/22/21 D065, BCKGRD AND DUMVAL NOT USED
C      BCKGRD = 0.0D0
C      DUMVAL = 0.0D0
      HRVAL(:) = 0.0D0
      IASTAT = 0
      BLNK8  = '        '


C --- Allocate OLMGROUP arrays if needed for OLMGROUPs
      IF( NUMOLM .GE. 1 )THEN
         IF( .NOT.ALLOCATED(OLMVAL) )THEN
            ALLOCATE( OLMVAL(NUMOLM,NUMTYP),
     &                STAT=IASTAT )
         ENDIF
         IF( .NOT.ALLOCATED(NO2VAL) )THEN
            ALLOCATE( NO2VAL(NUMOLM),
     &                STAT=IASTAT )
         ENDIF
         IF( .NOT.ALLOCATED(NO_VAL) )THEN
            ALLOCATE( NO_VAL(NUMOLM),
     &                STAT=IASTAT )
         ENDIF
         IF( .NOT.ALLOCATED(OLMGrp_PercentNO2) )THEN
            ALLOCATE( OLMGrp_PercentNO2(NUMOLM),
     &                STAT=IASTAT )

         ENDIF
         IF (IASTAT .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','409','OLMGROUP')
            ALLOC_ERR = .TRUE.
            WRITE(IOUNIT,*) '  Error Occurred During Allocation ',
     &                      'Arrays for OLM_CALC!'
            RETURN
         END IF
      END IF

C     Initialize scalar variables
      IOLM = 0
      NO2  = 0.0D0
      NO   = 0.0D0
      OLMTEMP = 0.0D0

C     Initialize NO2VAL(NUMOLM) and NO_VAL(NUMOLM) arrays, if allocated
      IF (ALLOCATED(NO2VAL)) NO2VAL(:) = 0.0D0
      IF (ALLOCATED(NO_VAL)) NO_VAL(:) = 0.0D0
C     Initialize OLMVAL(NUMOLM,NUMTYP) array, if allocated
      IF (ALLOCATED(OLMVAL)) OLMVAL(:,:) = 0.0D0
      IF (ALLOCATED(OLMGrp_PercentNO2)) OLMGrp_PercentNO2(:) = 0.0D0

C --- Begin Receptor LOOP
      RECEPTOR_LOOP: DO IREC = 1, NUMREC
C        Reinitialize the scalar variables
         NO2 = 0.0D0
         NO  = 0.0D0
         OLMTEMP = 0.0D0
         PercentNO2 = 0.0D0
C        Reinitialize the NO2VAL, NO_VAL, and OLMVAL arrays
         IF (ALLOCATED(NO2VAL)) NO2VAL(:) = 0.0D0
         IF (ALLOCATED(NO_VAL)) NO_VAL(:) = 0.0D0
         IF (ALLOCATED(OLMVAL)) OLMVAL(:,:) = 0.0D0
         IF (ALLOCATED(OLMGrp_PercentNO2)) OLMGrp_PercentNO2(:) = 0.0D0

C ---    First determine whether OLMGROUP(s) are being used;
C        if OLMGROUPs then first calculate the OLMGROUP-specific
C        PercentNO2 value(s) for all OLMGROUP(s)
         IF (NUMOLM .GE. 1) THEN
C ---       Loop through all OLMGROUPs to determine OLMGrp_PercentNO2 for each OLMGRP
            OLMGRP_LOOP: DO IOLM = 1, NUMOLM
C              Loop through SOURCEs to determine NO2VAL, NO_VAL, and OLMVAL for each
C              OLMGRP
               SOURCE_LOOP: DO ISRC = 1, NUMSRC
                 IF (L_OLMGRP(ISRC)) THEN
                    IF (IGRP_OLM(ISRC,IOLM) .EQ. 1) THEN

C ---                Source is included in this OLMGRoup;
C                    Calculate NO2 and NO CONV Values for OLMGROUP
                     NO2VAL(IOLM) = NO2VAL(IOLM) +
     &                      ANO2_RATIO(ISRC)* CHI(IREC,ISRC,1)
                     NO_VAL(IOLM) = NO_VAL(IOLM) +
     &                  (1.0D0-ANO2_RATIO(ISRC))*CHI(IREC,ISRC,1)*
     &                                            (30.0D0/46.0D0)
                     DO ITYP = 1, NUMTYP
C                       Calculate Hourly Values (Full Conversion) for OLMGROUP
                        OLMVAL(IOLM,ITYP) = OLMVAL(IOLM,ITYP) +
     &                                      CHI(IREC,ISRC,ITYP)
                     END DO
                    END IF
                 END IF
               END DO SOURCE_LOOP
C              Determine if combined plume is O3 limited, and assign
C              PercentNO2 to OLMGroup for CONC (ITYP=1); Apply full
C              conversion if O3MISS
               IF (.NOT. O3MISS .AND. (O3CONC/48.0D0) .LT.
     &                        ((NO_VAL(IOLM))/30.0D0)) THEN
C ---             .NOT. O3MISS and NO2 conversion is O3-limited;
C                 compute O3-limited NO2 concentration (OLMTEMP)
C                 and then compute PercentNO2 conversion for this
C                 OLMGroup
                  OLMTEMP  = NO2VAL(IOLM)+(O3CONC*(46.0D0/48.0D0))
                  IF (OLMVAL(IOLM,1) .GT. 0.0D0) THEN
                     OLMGrp_PercentNO2(IOLM) = OLMTEMP/OLMVAL(IOLM,1)
                  ELSE
                     OLMGrp_PercentNO2(IOLM) = NO2Equil
                  END IF
               ELSE
C ---             O3MISS or not O3-limited; apply full conversion
C                 (subject later to equilibrium ratio)
                  OLMGrp_PercentNO2(IOLM) = 1.0D0
               END IF

               OLMGrp_PercentNO2(IOLM) = MIN( OLMGrp_PercentNO2(IOLM),
     &                                                         1.0D0 )
C              Limit to equilibrium concentration of NO2 (default set at 90 percent)
               IF (OLMGrp_PercentNO2(IOLM) .GT. NO2Equil) THEN
                   OLMGrp_PercentNO2(IOLM) = NO2Equil
               END IF
            END DO OLMGRP_LOOP
         END IF

         IF( .NOT.EVONLY )THEN
C ---      Begin processing for NON-EVENT application

C ---      Begin Source Group LOOP to sum values
           GROUP_LOOP: DO IGRP = 1, NUMGRP

C ---      Loop through all SOURCEs again to apply OLMGrp_PercentNO2
C          as appropriate and sum values for this source group
           SOURCE_LOOP2: DO ISRC = 1, NUMSRC

            IF( IGROUP(ISRC,IGRP) .NE. 1 )THEN
C ---          This source is not included in the current SRCGROUP
               CYCLE SOURCE_LOOP2
            ENDIF

            IF( L_OLMGRP(ISRC) )THEN
C ---          This source is included in the current SRCGROUP and is also
C              included in an OLMGROUP; apply appropriate OLMGrp_PercentNO2
               OLMGRP_LOOP2: DO IOLM = 1, NUMOLM
                  IF( IGRP_OLM(ISRC,IOLM) .EQ. 1 )THEN
C                    Apply equivalent PercentNO2 to all ITYPs (CONC, DDEP, WDEP or DEPOS)
                     DO ITYP = 1, NUMTYP
                        HRVAL(ITYP) = OLMGrp_PercentNO2(IOLM) *
     &                                       CHI(IREC,ISRC,ITYP)
!! Added for TTRM2
!! Perform comparison of calculated hourly NO2 values between OLM and TTRM
                       IF (RUNTTRM2) THEN
                         IF (TTRM2DBG .AND. (CMETH .LT. 3)) THEN
                           WRITE(TTRM2TMP(2),PSTFRM,ERR=85) AXR(IREC),
     &                           AYR(IREC), HRVAL(1), AZELEV(IREC),
     &                           AZHILL(IREC), AZFLAG(IREC), CHRAVE(1),
     &                           GRPID(IGRP), KURDAT, SRCID(ISRC)
                           GO TO 885
 85                        WRITE(DUMMY,'("PSTFL",I4.4)') TTRM2TMP(2)
                           CALL ERRHDL(PATH,MODNAM,'E','520',DUMMY)
                           RUNERR = .TRUE.
 885                       CONTINUE
                         ENDIF
        IF (TTRMCOMPARE(IGRP,ISRC,IREC,ITYP) .LT. HRVAL(ITYP)) THEN
             HRVAL(ITYP) = TTRMCOMPARE(IGRP,ISRC,IREC,ITYP)
        ENDIF
                         IF (TTRM2DBG .AND. (CMETH .LT. 3)) THEN
                           WRITE(TTRM2TMP(3),PSTFRM,ERR=95) AXR(IREC),
     &                           AYR(IREC), HRVAL(1), AZELEV(IREC),
     &                           AZHILL(IREC), AZFLAG(IREC), CHRAVE(1),
     &                           GRPID(IGRP), KURDAT, SRCID(ISRC)
                           GO TO 895
 95                        WRITE(DUMMY,'("PSTFL",I4.4)') TTRM2TMP(3)
                           CALL ERRHDL(PATH,MODNAM,'E','520',DUMMY)
                           RUNERR = .TRUE.
 895                       CONTINUE
                         ENDIF
                       ENDIF
!! End of TTRM2 insert; Nov. 2021
                     END DO
C ---                Exit OLMGRP_LOOP2 since a source can only be in one OLMGRP
                     EXIT OLMGRP_LOOP2
                  ENDIF
               ENDDO OLMGRP_LOOP2

C ---          Call SUMVAL to add current source's contribution to AVEVAL
               CALL SUMVAL

C ---          Write data to OLM debugging file
               if (OLMDEBUG) then
                  if (IGROUP(ISRC,IGRP) .EQ. 1) then
                     write(OLMDBG,9988,ERR=999) kurdat, irec,
     &                 grpid(igrp),  isrc, srcid(isrc), iolm,
     &                 olmid(iolm), o3conc,
     &                 olmval(iolm,1), ANO2_RATIO(isrc),
     &                 no2val(iolm), no_val(iolm),
     &                 chi(irec,isrc,1),
     &                 OLMGrp_PercentNO2(IOLM), hrval(1),
     &                 aveval(irec,igrp,1,1)
                  end if
9988              format(1x,i8.8,i6,2x,a8,i6,2x,a12,2x,i4,2x,a8,
     &                                              9(1x,e12.5:))
               end if

            ELSE
C ---          Source is in this SRCGROUP, but NOT in an OLMGROUP;
C              apply OLM to individual source
               NO2 = ANO2_RATIO(ISRC) * CHI(IREC,ISRC,1)
               NO  = (1.0D0-ANO2_RATIO(ISRC))*CHI(IREC,ISRC,1)*
     &                                         (30.0D0/46.0D0)

C ---          Determine if CONC is O3-limited; if not, then no conversion needed
C              First check for missing O3 data (O3MISS) and apply full conversion
C              (subject to equilibrium ratio)
               IF (.NOT. O3MISS .AND. (O3CONC/48.0D0) .LT.
     &                                   (NO/30.0D0)) THEN
C ---             .NOT. O3MISS and NO2 conversion is O3-limited;
C                 compute O3-limited NO2 concentration (OLMTEMP)
C                 and then compute PercentNO2 conversion for this
C                 OLMGroup
                  HRVAL(1) = NO2 + (O3CONC*(46.0D0/48.0D0))
               ELSE
C ---             HRVAL is NOT O3-limited
                  HRVAL(1) = CHI(IREC,ISRC,1)
               END IF

               IF (CHI(IREC,ISRC,1) .GT. 0.0D0) THEN
C                 Calculate an equivalent Percent NO2 for CONC
                  PercentNO2 = HRVAL(1)/CHI(IREC,ISRC,1)
               ELSE
                  PercentNO2 = NO2Equil
               END IF

C ---          Ensure the PercentNO2 is not > 1.0
               PercentNO2 = MIN( PercentNO2, 1.0D0 )

C              Limit PercentNO2 to equilibrium concentration of NO2 (default=90 percent)
               IF (PercentNO2 .GT. NO2Equil) PercentNO2 = NO2Equil

C              Apply equivalent PercentNO2 to all ITYPs (CONC, DDEP, WDEP or DEPOS)
               DO ITYP = 1, NUMTYP
                  HRVAL(ITYP) = PercentNO2 * CHI(IREC,ISRC,ITYP)
!! Added for TTRM2
!! Perform comparison of calculated hourly NO2 values between OLM and TTRM
                  IF (RUNTTRM2) THEN
                    IF (TTRM2DBG .AND. (CMETH .LT. 3)) THEN
                      WRITE(TTRM2TMP(2),PSTFRM,ERR=86) AXR(IREC),
     &                      AYR(IREC), HRVAL(1),  AZELEV(IREC),
     &                      AZHILL(IREC), AZFLAG(IREC), CHRAVE(1),
     &                      GRPID(IGRP), KURDAT, SRCID(ISRC)
                      GO TO 886
 86                   WRITE(DUMMY,'("PSTFL",I4.4)') TTRM2TMP(2)
                      CALL ERRHDL(PATH,MODNAM,'E','520',DUMMY)
                      RUNERR = .TRUE.
 886                  CONTINUE
                    ENDIF
         IF (TTRMCOMPARE(IGRP,ISRC,IREC,ITYP) .LT. HRVAL(ITYP)) THEN
             HRVAL(ITYP) = TTRMCOMPARE(IGRP,ISRC,IREC,ITYP)
         ENDIF
                    IF (TTRM2DBG .AND. (CMETH .LT. 3)) THEN
                      WRITE(TTRM2TMP(3),PSTFRM,ERR=96) AXR(IREC),
     &                      AYR(IREC), HRVAL(1), AZELEV(IREC),
     &                      AZHILL(IREC), AZFLAG(IREC), CHRAVE(1),
     &                      GRPID(IGRP), KURDAT, SRCID(ISRC)
                      GO TO 896
 96                   WRITE(DUMMY,'("PSTFL",I4.4)') TTRM2TMP(3)
                      CALL ERRHDL(PATH,MODNAM,'E','520',DUMMY)
                      RUNERR = .TRUE.
 896                  CONTINUE
                   ENDIF
                 ENDIF
!! End of TTRM2 insert; Nov. 2021
               END DO

C ---          Call SUMVAL to add current source's contribution to AVEVAL
               CALL SUMVAL

C ---          Write data to OLM debugging file; since this source is
C              NOT in an OLMGROUP, assign IOLM = 0 and use no_val, no2val,
C              olmval, and PercentNO2 scalar variables; also use BLNK8 field
C              instead of OLMID
               IOLM = 0
               if (OLMDEBUG) then
                  if (IGROUP(ISRC,IGRP) .EQ. 1) then
                    write(OLMDBG,9988,ERR=9981) kurdat, irec,
     &                grpid(igrp), isrc, srcid(isrc), iolm,
     &                BLNK8, o3conc, chi(irec,isrc,1),
     &                ANO2_RATIO(isrc), no2, no, chi(irec,isrc,1),
     &                PercentNO2, hrval(1), aveval(irec,igrp,1,1)
                  end if
               end if

            END IF    ! source not in olmgrp

            go to 1234
9981        CALL ERRHDL(PATH,MODNAM,'E','520','OLMDEBUG')
1234        continue

            IF (EVAL(ISRC)) THEN
C              Check ARC centerline values for EVALFILE
C              output                              ---   CALL EVALCK
               CALL EVALCK
            END IF

           END DO SOURCE_LOOP2

           IF (GRP_BACK(IGRP)) THEN
C ---         Call SUMBACK_NO2 to update BACKGROUND contributions
              CALL SUMBACK_NO2

              if (OLMDEBUG) then
C ---            Include BACKGROUND contribution in OLM debug file
                 write(OLMDBG,99872,ERR=999) kurdat, irec,
     &               grpid(IGRP), 'BACKGROUND',
     &               bgconc, aveval(irec,igrp,1,1)
99872            format(1x,i8.8,i6,2x,a8,8x,a10,109x,
     &                                            2(1x,e12.5))
              end if

           END IF

          END DO GROUP_LOOP
C ---     End of .NOT.EVONLY processing

         ELSE IF (EVONLY) THEN
C ---      Check for inclusion of BACKGROUND in this EVENT and add to DEBUG file
C          as a separate "source" contribution
           isrc = 0
           iolm = 0

C ---      Loop through all SOURCEs again to apply OLMGrp_PercentNO2
C          as appropriate and sum values for this group
           EV_SOURCE_LOOP: DO ISRC = 1, NUMSRC

            ITYP = 1

            IF( IGROUP(ISRC,IDXEV(IEVENT)) .NE. 1 )THEN
C ---          This source is not included in the current SRCGROUP
               CYCLE EV_SOURCE_LOOP
            ENDIF

            IF( L_OLMGRP(ISRC) )THEN
C ---         This source is included in an OLMGROUP; apply appropriate OLMGrp_PercentNO2
              EV_OLMGRP_LOOP: DO IOLM = 1, NUMOLM
                 IF( IGRP_OLM(ISRC,IOLM) .EQ. 1 )THEN
C                   Apply equivalent PercentNO2 to all ITYPs (CONC, DDEP, WDEP or DEPOS)
                    DO ITYP = 1, NUMTYP
                       HRVAL(ITYP) = OLMGrp_PercentNO2(IOLM) *
     &                                      CHI(1,ISRC,ITYP)
!! Added for TTRM2
!! Perform comparison of calculated hourly NO2 values between OLM and TTRM
       IF (RUNTTRM2) THEN
         IF (TTRMCOMPARE(IGRP,ISRC,1,ITYP) .LT. HRVAL(ITYP)) THEN
             HRVAL(ITYP) = TTRMCOMPARE(IGRP,ISRC,1,ITYP) !! <----- for EVENT, lock receptor
         ENDIF
       ENDIF
!! End of TTRM2 insert; Nov. 2021
                    END DO
                    EXIT EV_OLMGRP_LOOP
                 ENDIF
              ENDDO EV_OLMGRP_LOOP

C ---         Call EV_SUMVAL to add current source's contribution to
C             EV_AVEVAL, GRPAVE, etc.
              CALL EV_SUMVAL

C ---         Write data to OLM debugging file
              if (OLMDEBUG) then
                 if (IGROUP(ISRC,IDXEV(IEVENT)) .EQ. 1) then
                    write(OLMDBG,9987,ERR=998) kurdat, ievent,
     &                EVNAME(IEVENT), EVAPER(IEVENT),
     &                grpid(IDXEV(IEVENT)), isrc, srcid(isrc),
     &                iolm, olmid(iolm), o3conc,
     &                olmval(iolm,1), ANO2_RATIO(isrc),
     &                no2val(iolm), no_val(iolm),
     &                chi(1,isrc,1),
     &                OLMGrp_PercentNO2(IOLM), hrval(1),
     &                grpave(IDXEV(IEVENT))
                 end if
9987             format(1x,i8.8,i6,2x,a10,2x,i3,2x,a8,i6,2x,a12,2x,
     &                                i4,2x,a8,1x,9(1x,e12.5))
               end if

            ELSE
C ---          Source is NOT in and OLMGROUP; apply OLM to individual source
               NO2 = ANO2_RATIO(ISRC) * CHI(1,ISRC,1)
               NO  = (1.0D0-ANO2_RATIO(ISRC))*CHI(1,ISRC,1)*
     &                                         (30.0D0/46.0D0)

C ---          Determine if CONC is O3-limited; if not, then no conversion needed
C              First check for missing O3 data (O3MISS) and apply full conversion
C              (subject to equilibrium ratio)
               IF (.NOT. O3MISS .AND. (O3CONC/48.0D0) .LT.
     &                                   (NO/30.0D0)) THEN
C ---             .NOT. O3MISS and NO2 conversion is O3-limited;
C                 compute O3-limited NO2 concentration (OLMTEMP)
C                 and then compute PercentNO2 conversion for this
C                 OLMGroup
                  HRVAL(1) = NO2 + (O3CONC*(46.0D0/48.0D0))
               ELSE
C ---             HRVAL is NOT O3-limited
                  HRVAL(1) = CHI(1,ISRC,1)
               END IF

               IF (CHI(1,ISRC,1) .GT. 0.0D0) THEN
C                 Calculate an equivalent Percent NO2 for CONC
                  PercentNO2 = HRVAL(1)/CHI(1,ISRC,1)
               ELSE
                  PercentNO2 = NO2Equil
               END IF

C ---          Ensure the PercentNO2 is not > 1.0
               PercentNO2 = MIN( PercentNO2, 1.0D0 )

C              Limit PercentNO2 to equilibrium concentration of NO2 (default=90 percent)
               IF (PercentNO2 .GT. NO2Equil) PercentNO2 = NO2Equil

C              Apply equivalent PercentNO2 to all ITYPs (CONC, DDEP, WDEP or DEPOS)
               DO ITYP = 1, NUMTYP
                  HRVAL(ITYP) = PercentNO2 * CHI(1,ISRC,ITYP)
!! Added for TTRM2
!! Perform comparison of calculated hourly NO2 values between OLM and TTRM
       IF (RUNTTRM2) THEN
         IF (TTRMCOMPARE(IGRP,ISRC,1,ITYP) .LT. HRVAL(ITYP)) THEN
             HRVAL(ITYP) = TTRMCOMPARE(IGRP,ISRC,1,ITYP)  !! <---- for EVENT processing, locking receptor
         ENDIF
       ENDIF
!! End of TTRM2 insert; Nov. 2021
               END DO

C ---          Call EV_SUMVAL to add current source's contribution to
C              EV_AVEVAL, GRPAVE, etc.
               CALL EV_SUMVAL

C ---          Write data to OLM debugging file; since this source is
C              NOT in an OLMGROUP, assign IOLM = 0 and use no_val, no2val,
C              olmval, and PercentNO2 scalar variables; also use BLNK8 field
C              instead of OLMID
               IOLM = 0
               if (OLMDEBUG) then
                  if (IGROUP(ISRC,IDXEV(IEVENT)) .EQ. 1) then
                     write(OLMDBG,9987,ERR=998) kurdat, ievent,
     &                 EVNAME(IEVENT), EVAPER(IEVENT),
     &                 grpid(IDXEV(IEVENT)), isrc,
     &                 srcid(isrc), iolm, BLNK8, o3conc,
     &                 chi(1,isrc,1), ANO2_RATIO(isrc), no2,
     &                 no, chi(1,isrc,1),
     &                 PercentNO2, hrval(1),
     &                 grpave(IDXEV(IEVENT))
                  end if
               end if

            END IF    ! source not in olmgrp IF-THEN block

            go to 2345
998         CALL ERRHDL(PATH,MODNAM,'E','520','OLMDEBUG')
2345        continue

            IF (EVAL(ISRC)) THEN
C              Check ARC centerline values for EVALFILE
C              output                              ---   CALL EVALCK
               CALL EVALCK
            END IF

           END DO EV_SOURCE_LOOP
C ---      End of EVENT Source Loop

           IF ( GRP_BACK(IDXEV(IEVENT)) ) THEN
C ---         Call EV_SUMBACK to update BACKGROUND contributions
              CALL EV_SUMBACK
              if (OLMDEBUG) then
C ---            Include BACKGROUND contribution in OLM debug file
                 write(OLMDBG,99873,ERR=999) kurdat, ievent,
     &               EVNAME(IEVENT), EVAPER(IEVENT),
     &               grpid(IDXEV(IEVENT)), 'BACKGROUND',
     &               EV_BGCONC(IHOUR), grpave(IDXEV(IEVENT))
99873            format(1x,i8.8,i6,2x,a10,2x,i3,2x,a8,8x,a10,110x,
     &                                            2(1x,e12.5))
              end if
           END IF
         END IF

C       ReInitialize __VAL arrays (1:NUMTYP)
        HRVAL  = 0.0D0

      END DO RECEPTOR_LOOP
C     End Receptor LOOP

      go to 3456
999   CALL ERRHDL(PATH,MODNAM,'E','520','OLMDEBUG')
3456  continue

      RETURN
      END


      SUBROUTINE TTRM_CALC
C***********************************************************************
C             TTRM_CALC Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes Hourly Results for Ozone Reaction Rate Option
C
C        PROGRAMMER: Carlos Szembek.
C
C        DATE:    October 16, 2020
C
C
C        INPUTS: O3 value (hourly or other specified value), ambient temp.
C
C
C        OUTPUTS:
C
C
C        CALLED FROM:   HRLOOP
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
C     JAT 7/22/21 D065, BLNK8 NOT USED
C      CHARACTER MODNAM*12, BLNK8*8
      CHARACTER MODNAM*12
C     JAT 7/22/21 D065, IASTAT NOT USED
C      INTEGER :: IASTAT

!      DOUBLE PRECISION :: NO2, NOX, BCKGRD, CHIN, CHID,
!     &                    O3PPB, KO3, DUMVAL
C     JAT 7/22/21 D065, BCKGRD AND DUMVAL NOT USED
c      DOUBLE PRECISION :: NO2, NOX, BCKGRD, O3PPB, KO3, DUMVAL
      DOUBLE PRECISION :: NO2, NOX, O3PPB, KO3

C     Variable Initializations
      MODNAM = 'TTRM_CALC'
C     JAT 7/22/21 D065, BCKGRD AND DUMVAL NOT USED
c      BCKGRD = 0.0D0
c      DUMVAL = 0.0D0
      HRVAL(:) = 0.0D0
C     JAT 7/22/21 D065, IASTAT AND BLNK8 NOT USED
C      IASTAT = 0
C      BLNK8  = '        '

C     Initialize scalar variables
      ITTRM = 0
      NO2 = 0.0D0
      NOX = 0.0D0
      TTRMTIME = 0.0D0
      TTRMTIME_PRM = 0.0D0
      GAMF = 0.0D0

C     Initialize TTRMVAL(NUMTYP) array
      TTRMINST(:) = 0.0D0
      TTRMFRAC(:) = 0.0D0
      TTRMFRAC_PRM(:) = 0.0D0
      TTRMFRAC_AER(:) = 0.0D0
      TTRMNO2(:)  = 0.0D0
      TTRMSUB(:)  = 0.0D0

C --- Begin Receptor LOOP
      RECEPTOR_LOOP: DO IREC = 1, NUMREC
C        Reinitialize the scalar variables
         NO2 = 0.0D0
         NOX = 0.0D0
         TTRMTIME = 0.0D0
         TTRMTIME_PRM = 0.0D0

         IF( .NOT.EVONLY )THEN
C ---      Begin processing for NON-EVENT application

C ---      Begin Source Group LOOP to sum values
           GROUP_LOOP: DO IGRP = 1, NUMGRP

C ---      Loop through all SOURCEs
C          as appropriate and sum values for this source group
           SOURCE_LOOPTTRM: DO ISRC = 1, NUMSRC

            IF( IGROUP(ISRC,IGRP) .NE. 1 )THEN
C ---          This source is not included in the current SRCGROUP
               CYCLE SOURCE_LOOPTTRM
            ENDIF

! Calculate hourly k1 value (units: ppb^-1 sec^-1)
           k1 = (15.33 / TA)*DEXP(-1450.0 / TA)
! Convert hourly ozone value back to ppb
           O3PPB = O3CONC / O3_PPB
! Calculate the coefficient of time
           KO3 = -1.0*k1*O3PPB

! Save hourly data to TTRM debug file
           IF (TTRMDBG) THEN
             TTRMOUT(IREC,ISRC,1) = KURDAT
             TTRMOUT(IREC,ISRC,2) = AXR(IREC)
             TTRMOUT(IREC,ISRC,3) = AYR(IREC)
             TTRMOUT(IREC,ISRC,4) = O3PPB ! O3 (in PPB)
             TTRMOUT(IREC,ISRC,5) = TA ! ambient temperature
             TTRMOUT(IREC,ISRC,6) = k1
           ENDIF

C ---          Source is in this SRCGROUP
C              Calculate in-stack converted NO2 and available NOx
               NO2 = ANO2_RATIO(ISRC) * CHI(IREC,ISRC,1)
               NOX = (1.0D0-ANO2_RATIO(ISRC))*CHI(IREC,ISRC,1)  ! Total NO

!     Perform TTRM for POINT, VOLUME or AREA type sources
               IF ((SRCTYP(ISRC)(1:5) .EQ. 'POINT') .OR.
     &             (SRCTYP(ISRC) .EQ. 'VOLUME') .OR.
     &             (SRCTYP(ISRC)(1:4) .EQ. 'AREA')) THEN
!                Calculate the final TTRM time based on the source type,
!                stability and plume dispersion type
                 CALL TTRM_STEP
C ---            Determine if CONC is O3-limited; if not, then no conversion needed
C                First check for missing O3 data (O3MISS) and apply full conversion
C                (subject to equilibrium ratio)
                 GAMF = TTRMOUT(IREC,ISRC,17) !Saved GAMFACT
                 IF (.NOT. O3MISS) THEN !
                    DO ITYP = 1, NUMTYP  ! Calculate TTRM for all ITYPs (CONC, DDEP, WDEP or DEPOS)
!                       TTRMINST(ITYP) = NO2
                       IF(TTRMTIME .GT. 0.0) THEN
                         TTRMFRAC(ITYP) = (1.0 - DEXP(KO3*TTRMTIME))
                       ELSE
                         TTRMFRAC(ITYP) = 0.0
                       ENDIF
!                Check on whether the TTRM fraction is equal to or less than the NO2Equil (default set to 90%)
                       IF (TTRMFRAC(ITYP) .GE. NO2Equil) THEN
                         TTRMFRAC(ITYP) = NO2Equil
                         HRVAL(ITYP)   = NO2Equil*CHI(IREC,ISRC,ITYP)
                         NO2           = 0.0
                         NOX           = CHI(IREC,ISRC,ITYP) ! Total NO
                         TTRMSUB(ITYP)  = HRVAL(ITYP)
                       ELSE
                         TTRMNO2(ITYP)  = TTRMFRAC(ITYP)*NOX
                         HRVAL(ITYP)   = TTRMNO2(ITYP) + NO2
                         TTRMSUB(ITYP)  = HRVAL(ITYP)
                       END IF
! Calculate for downwash cases
                       IF (WAKE .AND. (STABLE .OR. HS.LE.ZI)) THEN
                         TTRMFRAC_AER(ITYP) = TTRMFRAC(ITYP)
!   Calculate Prime contribution (using travel time for downwash)
                         IF (TTRMTIME_PRM .GT. 0.0) THEN
                           TTRMFRAC_PRM(ITYP) = (1.0 - DEXP(KO3*
     &                                           TTRMTIME_PRM))
                           IF (TTRMFRAC_PRM(ITYP) .GE. NO2Equil) THEN
                             TTRMFRAC_PRM(ITYP) = NO2Equil
                           ENDIF
                         ELSE
                           TTRMFRAC_PRM(ITYP) = 0.0
                         ENDIF
                         TTRMFRAC(ITYP) = GAMF*TTRMFRAC_PRM(ITYP) +
     &                                (1.0D0 - GAMF)*TTRMFRAC_AER(ITYP)
                         TTRMNO2(ITYP)  = TTRMFRAC(ITYP)*NOX
                         HRVAL(ITYP)   = TTRMNO2(ITYP) + NO2
                         TTRMSUB(ITYP)  = HRVAL(ITYP)
                       ENDIF
!! Added for TTRM2;
                       IF (RUNTTRM2) THEN
                         TTRMCOMPARE(IGRP,ISRC,IREC,ITYP) = HRVAL(ITYP)
                       ENDIF
!! End of TTRM2 insert; Nov. 2021
                    END DO
                 ELSE
C ---               For missing O3, scale by default NO2Equil (0.9)
                    DO ITYP = 1, NUMTYP
                      HRVAL(ITYP)  = NO2Equil*CHI(IREC,ISRC,ITYP)
                      TTRMSUB(ITYP) = HRVAL(ITYP)
!! Added for TTRM2
                      IF (RUNTTRM2) THEN
                        TTRMCOMPARE(IGRP,ISRC,IREC,ITYP) = HRVAL(ITYP)
                      ENDIF
                    ENDDO
!! End of TTRM2 insert; Nov. 2021
                 END IF
               ELSE
!              For LINE, BUOYLINE, OPENPIT and RLINE sources
!              scale by default NO2Equil (0.9) and issue warning
                 DO ITYP = 1, NUMTYP
                   HRVAL(ITYP)  = NO2Equil*CHI(IREC,ISRC,ITYP)
                   TTRMSUB(ITYP)  = HRVAL(ITYP)
!! Added for TTRM2
                   IF (RUNTTRM2) THEN
                       TTRMCOMPARE(IGRP,ISRC,IREC,ITYP) = HRVAL(ITYP)
                   ENDIF
!! End of TTRM2 insert; Nov. 2021
                   IF (.NOT. L_TTRMSRCTYP(ISRC)) THEN
C                     Write Warning Message: TTRM not configured for source type
                      CALL ERRHDL(PATH,MODNAM,'W','710',SRCID(ISRC))
                      L_TTRMSRCTYP(ISRC) = .TRUE.
                   END IF
                 ENDDO
               END IF
! Save values for TTRM2 evaluation file, if requested
           IF (TTRM2DBG .AND. (CMETH .LT. 3)) THEN
             WRITE(TTRM2TMP(1),PSTFRM,ERR=77) AXR(IREC), AYR(IREC),
     &                           TTRMCOMPARE(IGRP,ISRC,IREC,1),
     &                           AZELEV(IREC), AZHILL(IREC),
     &                           AZFLAG(IREC), CHRAVE(1),
     &                           GRPID(IGRP), KURDAT, SRCID(ISRC)
             GO TO 877
 77          WRITE(DUMMY,'("PSTFL",I3.3)') TTRM2TMP(1)
             CALL ERRHDL(PATH,MODNAM,'E','520',DUMMY)
             RUNERR = .TRUE.
 877         CONTINUE
           ENDIF

! Once all the sources in a group are complete, if requested, write output to debug file
         IF (IGROUP(ISRC,IGRP) .EQ. 1) THEN
           IF (TTRMDBG .AND. (CMETH .LT. 3)) THEN
             IF (SRCTYP(ISRC)(1:5) .EQ. 'POINT') THEN
                    write(TTRMUNT,77882,ERR=7781) kurdat, AXR(IREC),
     &                AYR(IREC), grpid(igrp), srcid(isrc), O3CONC,
     &                O3PPB, TA, k1, KO3,   TTRMOUT(IREC,ISRC,7),
     &                TTRMOUT(IREC,ISRC,8),  TTRMOUT(IREC,ISRC,9),
     &                TTRMOUT(IREC,ISRC,24), TTRMOUT(IREC,ISRC,25),
     &                TTRMSRC(IREC,ISRC),
     &                TTRMOUT(IREC,ISRC,10), TTRMOUT(IREC,ISRC,11),
     &                TTRMOUT(IREC,ISRC,20), TTRMOUT(IREC,ISRC,12),
     &                TTRMOUT(IREC,ISRC,13), TTRMOUT(IREC,ISRC,14),
     &                TTRMOUT(IREC,ISRC,15), TTRMOUT(IREC,ISRC,16),
     &                TTRMOUT(IREC,ISRC,17), TTRMOUT(IREC,ISRC,18),
     &                TTRMOUT(IREC,ISRC,19), CHI(IREC,ISRC,1), NO2, NOX,
     &                TTRMFRAC(1), TTRMNO2(1), TTRMSUB(1)

77882            format(1x,I8.8,',',F9.2,',',F10.2,',',A8,',',A10,',',
     &                  F7.2,',',F7.3,',',F5.1,',',E8.2,',',E8.2,',',
     &                  F10.4,',',F9.4,',',F9.3,',',F9.4,',',F9.3,',',
     &                  A10,',',F8.2,',',
     &                  F8.2,',',F8.2,',',F5.2,',',F5.2,',',F5.2,',',
     &                  F5.3,',',F5.3,',',F5.3,',',F8.3,',',F8.3,',',
     &                  F8.3,',',F8.3,',',F8.3,',',F8.3,',',F8.3,',',
     &                  F8.3)
               go to 5678
7781           CALL ERRHDL(PATH,MODNAM,'W','720','TTRMDEBUG')
5678           continue
             ELSE IF (SRCTYP(ISRC) .EQ. 'VOLUME') THEN
C ---          Write data to TTRM debugging file
                    write(TTRMUNT,77782,ERR=7771) kurdat, AXR(IREC),
     &                AYR(IREC), grpid(igrp), srcid(isrc), O3CONC,
     &                O3PPB, TA, k1, KO3,   TTRMOUT(IREC,ISRC,7),
     &                TTRMOUT(IREC,ISRC,8),  TTRMOUT(IREC,ISRC,9),
     &                TTRMSRC(IREC,ISRC),
     &                TTRMOUT(IREC,ISRC,10), TTRMOUT(IREC,ISRC,11),
     &                TTRMOUT(IREC,ISRC,20), TTRMOUT(IREC,ISRC,12),
     &                TTRMOUT(IREC,ISRC,13),
     &                TTRMOUT(IREC,ISRC,15),
     &                CHI(IREC,ISRC,1), NO2, NOX,
     &                TTRMFRAC(1), TTRMNO2(1), TTRMSUB(1)

77782            format(1x,I8.8,',',F9.2,',',F10.2,',',A8,',',A10,',',
     &                  F7.2,',',F7.3,',',F5.1,',',E8.2,',',E8.2,',',
     &                  F10.4,',',F9.4,',',F9.3,',',A10,',',F8.2,',',
     &                  F8.2,',',F8.2,',',F5.2,',',F5.2,',','n/a',',',
     &                  F5.3,',','n/a',',','n/a',',','n/a',',','n/a',
     &                  ',',F8.3,',',F8.3,',',F8.3,',',F8.3,',',F8.3,
     &                  ',',F8.3)
               go to 5778
7771           CALL ERRHDL(PATH,MODNAM,'W','720','TTRMDEBUG')
5778           continue
             ELSE IF (SRCTYP(ISRC)(1:4) .EQ. 'AREA') THEN
C ---          Write data to TTRM debugging file for AREA, AREAPOLY or AREACIRC sources
                    write(TTRMUNT,77772,ERR=7772) kurdat, AXR(IREC),
     &                AYR(IREC), grpid(igrp), srcid(isrc), O3CONC,
     &                O3PPB, TA, k1, KO3,   TTRMOUT(IREC,ISRC,7),
     &                TTRMOUT(IREC,ISRC,8),  TTRMOUT(IREC,ISRC,9),
     &                TTRMSRC(IREC,ISRC),
     &                TTRMOUT(IREC,ISRC,11), TTRMOUT(IREC,ISRC,12),
     &                TTRMOUT(IREC,ISRC,13),
     &                CHI(IREC,ISRC,1), NO2, NOX,
     &                TTRMFRAC(1), TTRMNO2(1), TTRMSUB(1)

77772            format(1x,I8.8,',',F9.2,',',F10.2,',',A8,',',A10,',',
     &                  F7.2,',',F7.3,',',F5.1,',',E8.2,',',E8.2,',',
     &                  F10.4,',',F9.4,',',F9.3,',',A10,',n/a,',F8.2,
     &                  ',n/a,',F5.2,',',F5.2,',','n/a',',','n/a',',',
     &                 'n/a',',','n/a',',','n/a',',','n/a',',',F8.3,',',
     &                  F8.3,',',F8.3,',',F8.3,',',F8.3,',',F8.3)
               go to 7778
7772           CALL ERRHDL(PATH,MODNAM,'W','720','TTRMDEBUG')
7778           continue
             ELSE
C ---          For all other (non-supported) type of sources
                    write(TTRMUNT,77773,ERR=7773) kurdat, AXR(IREC),
     &                AYR(IREC), grpid(igrp), srcid(isrc)

77773            format(1x,I8.8,',',F9.2,',',F10.2,',',A8,',',A10,
     &                  18(',n/a'))
               go to 7779
7773           CALL ERRHDL(PATH,MODNAM,'W','720','TTRMDEBUG')
7779           continue
             END IF
           END IF
         END IF

C ---          Call SUMVAL to add current source's contribution to AVEVAL
               IF (.NOT. RUNTTRM2) THEN
                 CALL SUMVAL
               ENDIF

            IF (EVAL(ISRC)) THEN
C              Check ARC centerline values for EVALFILE
C              output                              ---   CALL EVALCK
               CALL EVALCK
            END IF

           END DO SOURCE_LOOPTTRM

           IF (GRP_BACK(IGRP)) THEN
C ---         Call SUMBACK_NO2 to update BACKGROUND contributions
              IF (.NOT. RUNTTRM2) THEN
                CALL SUMBACK_NO2
              ENDIF
           END IF

          END DO GROUP_LOOP
         ELSE IF (EVONLY) THEN
C ---      Check for inclusion of BACKGROUND in this EVENT
!C ---      Begin Source Group LOOP to sum values
!           EV_GROUP_LOOP: DO IGRP = 1, NUMGRP

C ---      Loop through all SOURCEs
C          as appropriate and sum values for this source group
           EV_SOURCE_LOOPTTRM: DO ISRC = 1, NUMSRC
           ITYP = 1

            IF( IGROUP(ISRC,IGRP) .NE. 1 )THEN
C ---          This source is not included in the current SRCGROUP
               CYCLE EV_SOURCE_LOOPTTRM
            ENDIF

! Calculate hourly k1 value (units: ppb^-1 sec^-1)
           k1 = (15.33 / TA)*DEXP(-1450.0 / TA)
! Convert hourly ozone value back to ppb
           O3PPB = O3CONC / O3_PPB
! Calculate the coefficient of time
           KO3 = -1.0*k1*O3PPB

! Save hourly data to TTRM debug file
           IF (TTRMDBG) THEN
             TTRMOUT(IREC,ISRC,1) = KURDAT
             TTRMOUT(IREC,ISRC,2) = AXR(1)
             TTRMOUT(IREC,ISRC,3) = AYR(1)
             TTRMOUT(IREC,ISRC,4) = O3PPB ! O3 (in PPB)
             TTRMOUT(IREC,ISRC,5) = TA ! ambient temperature
             TTRMOUT(IREC,ISRC,6) = k1
           ENDIF

C ---          Source is in this SRCGROUP
C              Calculate in-stack converted NO2 and available NOx
               NO2 = ANO2_RATIO(ISRC) * CHI(1,ISRC,1)
               NOX = (1.0D0-ANO2_RATIO(ISRC))*CHI(1,ISRC,1) ! Total NO
!
!     Perform TTRM for POINT, VOLUME or AREA type sources
               IF ((SRCTYP(ISRC)(1:5) .EQ. 'POINT') .OR.
     &             (SRCTYP(ISRC) .EQ. 'VOLUME') .OR.
     &             (SRCTYP(ISRC)(1:4) .EQ. 'AREA')) THEN
!                Calculate the final TTRM time based on the source type,
!                stability and plume dispersion type
                 CALL TTRM_STEP
C ---            Determine if CONC is O3-limited; if not, then no conversion needed
C                First check for missing O3 data (O3MISS) and apply full conversion
C                (subject to equilibrium ratio)
                 IF (.NOT. O3MISS) THEN !
                    DO ITYP = 1, NUMTYP  ! Calculate TTRM for all ITYPs (CONC, DDEP, WDEP or DEPOS)
!                       TTRMINST(ITYP) = NO2
                       IF(TTRMTIME .GT. 0.0) THEN
                         TTRMFRAC(ITYP) = (1.0 - DEXP(KO3*TTRMTIME))
                       ELSE
                         TTRMFRAC(ITYP) = 0.0
                       ENDIF
!                Check on whether the TTRM fraction is equal to or less than the NO2Equil (default set to 90%)
                       IF (TTRMFRAC(ITYP) .GE. NO2Equil) THEN
                         TTRMFRAC(ITYP) = NO2Equil
                         HRVAL(ITYP)   = NO2Equil*CHI(1,ISRC,1)
                         NO2           = 0.0
                         NOX           = CHI(1,ISRC,1) ! Total NO
                         TTRMSUB(ITYP)  = HRVAL(ITYP)
                       ELSE
                         TTRMNO2(ITYP)  = TTRMFRAC(ITYP)*NOX
                         HRVAL(ITYP)   = TTRMNO2(ITYP) + NO2
                         TTRMSUB(ITYP)  = HRVAL(ITYP)
                       END IF
! Calculate for downwash cases
                       IF (WAKE .AND. (STABLE .OR. HS.LE.ZI)) THEN
                         TTRMFRAC_AER(ITYP) = TTRMFRAC(ITYP)
!   Calculate Prime contribution (using travel time for downwash)
                         IF (TTRMTIME_PRM .GT. 0.0) THEN
                           TTRMFRAC_PRM(ITYP) = (1.0 - DEXP(KO3*
     &                                           TTRMTIME_PRM))
                           IF (TTRMFRAC_PRM(ITYP) .GE. NO2Equil) THEN
                             TTRMFRAC_PRM(ITYP) = NO2Equil
                           ENDIF
                         ELSE
                           TTRMFRAC_PRM(ITYP) = 0.0
                         ENDIF
                         TTRMFRAC(ITYP) = GAMF*TTRMFRAC_PRM(ITYP) +
     &                                (1.0D0 - GAMF)*TTRMFRAC_AER(ITYP)
                         TTRMNO2(ITYP)  = TTRMFRAC(ITYP)*NOX
                         HRVAL(ITYP)   = TTRMNO2(ITYP) + NO2
                         TTRMSUB(ITYP)  = HRVAL(ITYP)
                      ENDIF
!! Added for TTRM2
                      IF (RUNTTRM2) THEN
                        TTRMCOMPARE(IGRP,ISRC,1,ITYP) = HRVAL(ITYP)
                      ENDIF
!! End of TTRM2 insert; Nov. 2021
                    END DO
                 ELSE
C ---               For missing O3, scale by default NO2Equil (0.9)
                    DO ITYP = 1, NUMTYP
                      HRVAL(ITYP)  = NO2Equil*CHI(1,ISRC,ITYP)
                      TTRMSUB(ITYP) = HRVAL(ITYP)
!! Added for TTRM2
                      IF (RUNTTRM2) THEN
                        TTRMCOMPARE(IGRP,ISRC,1,ITYP) = HRVAL(ITYP)
                      ENDIF
                    ENDDO
!! End of TTRM2 insert; Nov. 2021
                 END IF
               ELSE
!              For LINE, BUOYLINE, OPENPIT and RLINE sources
!              scale by default NO2Equil (0.9) and issue warning
                 DO ITYP = 1, NUMTYP
                   HRVAL(ITYP)  = NO2Equil*CHI(1,ISRC,ITYP)
                   TTRMSUB(ITYP) = HRVAL(ITYP)
!! Added for TTRM2
                   IF (RUNTTRM2) THEN
                       TTRMCOMPARE(IGRP,ISRC,1,ITYP) = HRVAL(ITYP)
                   ENDIF
                 ENDDO
!! End of TTRM2 insert; Nov. 2021
                 IF (.NOT. L_TTRMSRCTYP(ISRC)) THEN
C                   Write Warning Message: TTRM not configured for source type
                    CALL ERRHDL(PATH,MODNAM,'W','710',SRCID(ISRC))
                    L_TTRMSRCTYP(ISRC) = .TRUE.
                 END IF
               END IF

C ---          Call EV_SUMVAL to add current source's contribution to
C              EV_AVEVAL, GRPAVE, etc.
               IF (.NOT. RUNTTRM2) THEN
                 CALL EV_SUMVAL
               ENDIF
!               CALL EV_SUMVAL

! If requested, write output to debug file
         IF (IGROUP(ISRC,IGRP) .EQ. 1) THEN
           IF (TTRMDBG .AND. (CMETH .LT. 3)) THEN
             IF (SRCTYP(ISRC)(1:5) .EQ. 'POINT') THEN
                    write(TTRMUNT,97882,ERR=9781) kurdat,
     &                ievent, EVNAME(IEVENT), EVAPER(IEVENT),
     &                grpid(igrp), srcid(isrc), O3CONC,
     &                O3PPB, TA, k1, KO3,   TTRMOUT(1,ISRC,7),
     &                TTRMOUT(1,ISRC,8),  TTRMOUT(1,ISRC,9),
     &                TTRMOUT(IREC,ISRC,24), TTRMOUT(IREC,ISRC,25),
     &                TTRMSRC(IREC,ISRC),
     &                TTRMOUT(1,ISRC,10), TTRMOUT(1,ISRC,11),
     &                TTRMOUT(1,ISRC,20), TTRMOUT(1,ISRC,12),
     &                TTRMOUT(1,ISRC,13), TTRMOUT(1,ISRC,14),
     &                TTRMOUT(1,ISRC,15), TTRMOUT(1,ISRC,16),
     &                TTRMOUT(1,ISRC,17), TTRMOUT(1,ISRC,18),
     &                TTRMOUT(1,ISRC,19), CHI(1,ISRC,1), NO2, NOX,
     &                TTRMFRAC(1), TTRMNO2(1), TTRMSUB(1)

97882            format(1x,I8.8,',',i6,',',a10,',',
     &                  i3,',',A8,',',A10,',',
     &                  F7.2,',',F7.3,',',F5.1,',',E8.2,',',E8.2,',',
     &                  F10.4,',',F9.4,',',F9.3,',',F9.4,',',F9.3,',',
     &                  A10,',',F8.2,',',
     &                  F8.2,',',F8.2,',',F5.2,',',F5.2,',',F5.2,',',
     &                  F5.3,',',F5.3,',',F5.3,',',F8.3,',',F8.3,',',
     &                  F8.3,',',F8.3,',',F8.3,',',F8.3,',',F8.3,',',
     &                  F8.3)
               go to 56789
9781           CALL ERRHDL(PATH,MODNAM,'W','720','TTRMDEBUG')
56789          continue
             ELSE IF (SRCTYP(ISRC) .EQ. 'VOLUME') THEN
C ---          Write data to TTRM debugging file
                    write(TTRMUNT,97782,ERR=9771) kurdat,
     &                ievent, EVNAME(IEVENT), EVAPER(IEVENT),
     &                grpid(igrp), srcid(isrc), O3CONC,
     &                O3PPB, TA, k1, KO3,TTRMOUT(1,ISRC,7),
     &                TTRMOUT(1,ISRC,8),  TTRMOUT(1,ISRC,9),
     &                TTRMSRC(IREC,ISRC),
     &                TTRMOUT(1,ISRC,10), TTRMOUT(1,ISRC,11),
     &                TTRMOUT(1,ISRC,20), TTRMOUT(1,ISRC,12),
     &                TTRMOUT(1,ISRC,13),
     &                TTRMOUT(1,ISRC,15),
     &                CHI(1,ISRC,1), NO2, NOX,
     &                TTRMFRAC(1), TTRMNO2(1), TTRMSUB(1)

97782            format(1x,I8.8,',',i6,',',a10,',',
     &                  i3,',',A8,',',A10,',',
     &                  F7.2,',',F7.3,',',F5.1,',',E8.2,',',E8.2,',',
     &                  F10.4,',',F9.4,',',F9.3,',',A10,',',F8.2,',',
     &                  F8.2,',',F8.2,',',F5.2,',',F5.2,',','n/a',',',
     &                  F5.3,',','n/a',',','n/a',',','n/a',',','n/a',
     &                  ',',F8.3,',',F8.3,',',F8.3,',',F8.3,',',F8.3,
     &                  ',',F8.3)
               go to 57789
9771           CALL ERRHDL(PATH,MODNAM,'W','720','TTRMDEBUG')
57789          continue
             ELSE IF (SRCTYP(ISRC)(1:4) .EQ. 'AREA') THEN
C ---          Write data to TTRM debugging file for AREA, AREAPOLY or AREACIRC sources
                    write(TTRMUNT,97772,ERR=9772) kurdat,
     &                ievent, EVNAME(IEVENT), EVAPER(IEVENT),
     &                grpid(igrp), srcid(isrc), O3CONC,
     &                O3PPB, TA, k1, KO3,   TTRMOUT(1,ISRC,7),
     &                TTRMOUT(1,ISRC,8),  TTRMOUT(1,ISRC,9),
     &                TTRMSRC(IREC,ISRC),
     &                TTRMOUT(1,ISRC,11), TTRMOUT(1,ISRC,12),
     &                TTRMOUT(1,ISRC,13),
     &                CHI(1,ISRC,1), NO2, NOX,
     &                TTRMFRAC(1), TTRMNO2(1), TTRMSUB(1)

97772            format(1x,I8.8,',',i6,',',a10,',',
     &                  i3,',',A8,',',A10,',',
     &                  F7.2,',',F7.3,',',F5.1,',',E8.2,',',E8.2,',',
     &                  F10.4,',',F9.4,',',F9.3,',',A10,',n/a,',F8.2,
     &                  ',n/a,',F5.2,',',F5.2,',','n/a',',','n/a',',',
     &                 'n/a',',','n/a',',','n/a',',','n/a',',',F8.3,',',
     &                  F8.3,',',F8.3,',',F8.3,',',F8.3,',',F8.3)
               go to 77789
9772           CALL ERRHDL(PATH,MODNAM,'W','720','TTRMDEBUG')
77789          continue

             ELSE
C ---          for all other (non-supported) source types
                    write(TTRMUNT,97773,ERR=9773) kurdat,
     &                ievent, EVNAME(IEVENT), EVAPER(IEVENT),
     &                grpid(igrp), srcid(isrc)

97773            format(1x,I8.8,',',i6,',',a10,',',
     &                  i3,',',A8,',',A10,18(',n/a'))
               go to 77790
9773           CALL ERRHDL(PATH,MODNAM,'W','720','TTRMDEBUG')
77790          continue

             END IF
           END IF
         END IF

             IF (EVAL(ISRC)) THEN
C              Check ARC centerline values for EVALFILE
C              output                              ---   CALL EVALCK
               CALL EVALCK
             END IF

           END DO EV_SOURCE_LOOPTTRM
C ---      End of EVENT Source Loop

           IF ( GRP_BACK(IDXEV(IEVENT)) ) THEN
C ---         Call EV_SUMBACK to update BACKGROUND contributions
              IF (.NOT. RUNTTRM2) THEN
                CALL EV_SUMBACK
              ENDIF
           END IF
         END IF !  End of Event_or_Non-Event IF BLOCK

C       ReInitialize __VAL arrays (1:NUMTYP)
        HRVAL  = 0.0D0

      END DO RECEPTOR_LOOP
C     End Receptor LOOP

      RETURN
      END

      SUBROUTINE TTRM_STEP
C***********************************************************************
C             TTRM_STEP Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Steps through Hourly Calcs for the Travel Time Reaction
C                 Method and saves intermediary variables for an optional
C                 debug file.
C
C        PROGRAMMER: Carlos Szembek.
C
C        DATE:    February 17, 2021
C
C
C        INPUTS: O3 value (hourly or other specified value), ambient temp.
C
C
C        OUTPUTS:
C
C
C        CALLED FROM:   TTRM_CALC
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
!      CHARACTER MODNAM*12, BLNK8*8
      CHARACTER MODNAM*12
!      INTEGER :: IASTAT

!      DOUBLE PRECISION :: NO2, NOX, BCKGRD, CHIN, CHID,
!     &                    O3PPB, KO3, DUMVAL

!      DOUBLE PRECISION :: TTRMNO2(NUMTYPE)

C     Variable Initializations
      MODNAM = 'TTRM_STEP'

!     Perform TTRM calculations based on SRCTYPE, dominant plume dispersion type and stability
      IF (SRCTYP(ISRC)(1:5) .EQ. 'POINT') THEN
!       Calculate travel time if POINT source is subject to downwash
        IF (WAKE .AND. (STABLE .OR. HS.LE.ZI)) THEN
          IF(TTRMOUT(IREC,ISRC,24) .GT. 0.0) THEN
             TTRMTIME_PRM = DABS(TTRMOUT(IREC,ISRC,10) /
     &                      TTRMOUT(IREC,ISRC,24)) ! X / UEFFe ! Use downwind distance and wind speed for downwash (UEFFe)
          ELSE
             TTRMTIME_PRM = 0.0
          ENDIF
          TTRMOUT(IREC,ISRC,25) = TTRMTIME_PRM
        ENDIF
!       Calculate travel time for portion of plume not subject to downwash
!     Calculate the travel time conservatively based on the radial distance (DISTR) and effective wind speed (ueff)
!!!         The effective wind speed based on the meander fraction; stability and plume type:
!!!         If the meander fraction (FRAN) <= 0.5 then
!!!           If STABLE use [ueff]
!!!           Else if the partial penetration fration (PPF) > 0.5 use [ueff3]
!!!           Else if the concentration from the direct plume (CHID) > indirect (CHIN) use [ueffd]
!!!           Otherwise for the indirect plume use [ueff]
!!!         Else for FRAN > 0.5
!!!           If STABLE use [ueff]
!!!           Else use [ueffd]
        IF (TTRMOUT(IREC,ISRC,15) .GE. 0.5) THEN !MEANDER FRACTION >= 0.5
         IF (STABLE) THEN
           IF(TTRMOUT(IREC,ISRC,12) .GT. 0.0) THEN
             TTRMTIME = DABS(TTRMOUT(IREC,ISRC,11) /
     &                    TTRMOUT(IREC,ISRC,12)) ! DISTR / UEFF ! Use radial distance: TTRMOUT(IREC,ISRC,11)
           ELSE
             TTRMTIME = 0.
           ENDIF
           IF (TTRMDBG) THEN
             TTRMOUT(IREC,ISRC,7) = TTRMOUT(IREC,ISRC,11) ! DISTR
             TTRMOUT(IREC,ISRC,8) = TTRMOUT(IREC,ISRC,12) ! UEFF
             TTRMOUT(IREC,ISRC,9) = TTRMTIME
             TTRMSRC(IREC,ISRC) = 'STAB-M-GAU'
           END IF
         ELSE
           IF (TTRMOUT(IREC,ISRC,16) .GE. 0.5) THEN ! For partial penetrated dominant meander plume
             IF(TTRMOUT(IREC,ISRC,14) .GT. 0.0) THEN
               TTRMTIME = DABS(TTRMOUT(IREC,ISRC,11) /
     &                      TTRMOUT(IREC,ISRC,14)) ! DISTR / UEFF3
             ELSE
               TTRMTIME = 0.
             ENDIF
             IF (TTRMDBG) THEN
               TTRMOUT(IREC,ISRC,7) = TTRMOUT(IREC,ISRC,11) ! DISTR
               TTRMOUT(IREC,ISRC,8) = TTRMOUT(IREC,ISRC,14) ! UEFF3
               TTRMOUT(IREC,ISRC,9) = TTRMTIME
               TTRMSRC(IREC,ISRC) = 'UNST-M-PEN'
             END IF
           ELSE IF (TTRMOUT(IREC,ISRC,19) .GE.
     &              TTRMOUT(IREC,ISRC,18)) THEN! CHID >= CHIN; For direct dominant meander plume
             IF(TTRMOUT(IREC,ISRC,13) .GT. 0.0) THEN
               TTRMTIME = DABS(TTRMOUT(IREC,ISRC,11) /
     &                      TTRMOUT(IREC,ISRC,13)) ! DISTR / UEFFD
             ELSE
               TTRMTIME = 0.
             ENDIF
             IF (TTRMDBG) THEN
               TTRMOUT(IREC,ISRC,7) = TTRMOUT(IREC,ISRC,11) ! DISTR
               TTRMOUT(IREC,ISRC,8) = TTRMOUT(IREC,ISRC,13) ! UEFFD
               TTRMOUT(IREC,ISRC,9) = TTRMTIME
               TTRMSRC(IREC,ISRC) = 'UNST-M-DIR'
             END IF
           ELSE ! for indirect dominant meander plume
             IF(TTRMOUT(IREC,ISRC,12) .GT. 0.0) THEN
               TTRMTIME = DABS(TTRMOUT(IREC,ISRC,11) /
     &                      TTRMOUT(IREC,ISRC,12)) ! DISTR / UEFF
             ELSE
               TTRMTIME = 0.
             ENDIF
             IF (TTRMDBG) THEN
               TTRMOUT(IREC,ISRC,7) = TTRMOUT(IREC,ISRC,11) ! DISTR
               TTRMOUT(IREC,ISRC,8) = TTRMOUT(IREC,ISRC,12) ! UEFF
               TTRMOUT(IREC,ISRC,9) = TTRMTIME
               TTRMSRC(IREC,ISRC) = 'UNST-M-IND'
             END IF
           END IF
         END IF
        ELSE ! For the COHERENT Plume
          IF (TTRMOUT(IREC,ISRC,10) .GE. 0.) THEN ! If the downwind distance is >= 0 use downwind distance
            IF (STABLE) THEN
              IF(TTRMOUT(IREC,ISRC,12) .GT. 0.0) THEN
                TTRMTIME = DABS(TTRMOUT(IREC,ISRC,10) /
     &                       TTRMOUT(IREC,ISRC,12)) ! X / UEFF ! Use downwind distance: TTRMOUT(IREC,ISRC,10)
              ELSE
                TTRMTIME = 0.
              ENDIF
              IF (TTRMDBG) THEN
                TTRMOUT(IREC,ISRC,7) = TTRMOUT(IREC,ISRC,10) ! X
                TTRMOUT(IREC,ISRC,8) = TTRMOUT(IREC,ISRC,12) ! UEFF
                TTRMOUT(IREC,ISRC,9) = TTRMTIME
                TTRMSRC(IREC,ISRC) = 'STAB-C-GAU'
              END IF
            ELSE
              IF (TTRMOUT(IREC,ISRC,16) .GE. 0.5) THEN ! For partial penetrated dominant COHERENT plume
                IF(TTRMOUT(IREC,ISRC,14) .GT. 0.0) THEN
                  TTRMTIME = DABS(TTRMOUT(IREC,ISRC,10)/
     &                         TTRMOUT(IREC,ISRC,14)) ! X / UEFF3
                ELSE
                  TTRMTIME = 0.
                ENDIF
                IF (TTRMDBG) THEN
                  TTRMOUT(IREC,ISRC,7) = TTRMOUT(IREC,ISRC,10) ! X
                  TTRMOUT(IREC,ISRC,8) = TTRMOUT(IREC,ISRC,14) ! UEFF3
                  TTRMOUT(IREC,ISRC,9) = TTRMTIME
                  TTRMSRC(IREC,ISRC) = 'UNST-C-PEN'
                END IF
              ELSE IF (TTRMOUT(IREC,ISRC,19) .GE.
     &                 TTRMOUT(IREC,ISRC,18)) THEN! CHID >= CHIN; For direct dominant COHERENT plume
                IF(TTRMOUT(IREC,ISRC,13) .GT. 0.0) THEN
                  TTRMTIME = DABS(TTRMOUT(IREC,ISRC,10)/
     &                         TTRMOUT(IREC,ISRC,13)) ! X / UEFFD
                ELSE
                  TTRMTIME = 0.
                ENDIF
                IF (TTRMDBG) THEN
                  TTRMOUT(IREC,ISRC,7) = TTRMOUT(IREC,ISRC,10) ! X
                  TTRMOUT(IREC,ISRC,8) = TTRMOUT(IREC,ISRC,13) ! UEFFD
                  TTRMOUT(IREC,ISRC,9) = TTRMTIME
                  TTRMSRC(IREC,ISRC) = 'UNST-C-DIR'
                END IF
              ELSE ! for indirect dominant COHERENT plume
                IF(TTRMOUT(IREC,ISRC,12) .GT. 0.0) THEN
                TTRMTIME = DABS(TTRMOUT(IREC,ISRC,10)/
     &                         TTRMOUT(IREC,ISRC,12)) ! X / UEFF
                ELSE
                  TTRMTIME = 0.
                ENDIF
                IF (TTRMDBG) THEN
                  TTRMOUT(IREC,ISRC,7) = TTRMOUT(IREC,ISRC,10) ! X
                  TTRMOUT(IREC,ISRC,8) = TTRMOUT(IREC,ISRC,12) ! UEFF
                  TTRMOUT(IREC,ISRC,9) = TTRMTIME
                  TTRMSRC(IREC,ISRC) = 'UNST-C-IND'
                END IF
              END IF
            END IF
          ELSE ! If the downwind distance is < 0, use the RADIAL distance
            IF (STABLE) THEN
              IF(TTRMOUT(IREC,ISRC,12) .GT. 0.0) THEN
                TTRMTIME = DABS(TTRMOUT(IREC,ISRC,11) /
     &                       TTRMOUT(IREC,ISRC,12)) ! DISTR / UEFF ! Use downwind distance: TTRMOUT(IREC,ISRC,10)
              ELSE
                TTRMTIME = 0.
              ENDIF
              IF (TTRMDBG) THEN
                TTRMOUT(IREC,ISRC,7) = TTRMOUT(IREC,ISRC,11) ! DISTR
                TTRMOUT(IREC,ISRC,8) = TTRMOUT(IREC,ISRC,12) ! UEFF
                TTRMOUT(IREC,ISRC,9) = TTRMTIME
                TTRMSRC(IREC,ISRC) = 'STAB-m-GAU'
              END IF
            ELSE
              IF (TTRMOUT(IREC,ISRC,16) .GE. 0.5) THEN ! For partial penetrated dominant COHERENT plume
                IF(TTRMOUT(IREC,ISRC,14) .GT. 0.0) THEN
                  TTRMTIME = DABS(TTRMOUT(IREC,ISRC,11)/
     &                         TTRMOUT(IREC,ISRC,14)) ! DISTR / UEFF3
                ELSE
                  TTRMTIME = 0.
                ENDIF
                IF (TTRMDBG) THEN
                  TTRMOUT(IREC,ISRC,7) = TTRMOUT(IREC,ISRC,11) ! DISTR
                  TTRMOUT(IREC,ISRC,8) = TTRMOUT(IREC,ISRC,14) ! UEFF3
                  TTRMOUT(IREC,ISRC,9) = TTRMTIME
                  TTRMSRC(IREC,ISRC) = 'UNST-m-PEN'
                END IF
              ELSE IF (TTRMOUT(IREC,ISRC,19) .GE.
     &                 TTRMOUT(IREC,ISRC,18)) THEN! CHID >= CHIN; For direct dominant COHERENT plume
                IF(TTRMOUT(IREC,ISRC,13) .GT. 0.0) THEN
                  TTRMTIME = DABS(TTRMOUT(IREC,ISRC,11)/
     &                         TTRMOUT(IREC,ISRC,13)) ! DISTR / UEFFD
                ELSE
                  TTRMTIME = 0.
                ENDIF
                IF (TTRMDBG) THEN
                  TTRMOUT(IREC,ISRC,7) = TTRMOUT(IREC,ISRC,11) ! DISTR
                  TTRMOUT(IREC,ISRC,8) = TTRMOUT(IREC,ISRC,13) ! UEFFD
                  TTRMOUT(IREC,ISRC,9) = TTRMTIME
                  TTRMSRC(IREC,ISRC) = 'UNST-m-DIR'
                END IF
              ELSE ! for indirect dominant COHERENT plume
                IF(TTRMOUT(IREC,ISRC,12) .GT. 0.0) THEN
                  TTRMTIME = DABS(TTRMOUT(IREC,ISRC,11)/
     &                         TTRMOUT(IREC,ISRC,12)) ! DISTR / UEFF
                ELSE
                  TTRMTIME = 0.
                ENDIF
                IF (TTRMDBG) THEN
                  TTRMOUT(IREC,ISRC,7) = TTRMOUT(IREC,ISRC,11) ! DISTR
                  TTRMOUT(IREC,ISRC,8) = TTRMOUT(IREC,ISRC,12) ! UEFF
                  TTRMOUT(IREC,ISRC,9) = TTRMTIME
                  TTRMSRC(IREC,ISRC) = 'UNST-m-IND'
                END IF
              END IF
            END IF
          ENDIF
        ENDIF
      ELSE IF (SRCTYP(ISRC) .EQ. 'VOLUME') THEN
        IF (TTRMOUT(IREC,ISRC,15) .GE. 0.5) THEN ! Meander plume; Use RADIAL distance (DISTR)
          IF (STABLE) THEN ! Use  eff. wspd (UEFF)
            IF(TTRMOUT(IREC,ISRC,12) .GT. 0.0) THEN
              TTRMTIME = DABS(TTRMOUT(IREC,ISRC,11) /
     &                     TTRMOUT(IREC,ISRC,12)) ! DISTR / UEFF
            ELSE
              TTRMTIME = 0.
            ENDIF
            IF (TTRMDBG) THEN
             TTRMOUT(IREC,ISRC,7) = TTRMOUT(IREC,ISRC,11) ! DISTR
             TTRMOUT(IREC,ISRC,8) = TTRMOUT(IREC,ISRC,12) ! UEFFD
             TTRMOUT(IREC,ISRC,9) = TTRMTIME
             TTRMSRC(IREC,ISRC) = 'STAB-M-UEF'
            END IF
          ELSE IF (TTRMOUT(IREC,ISRC,13) .GT.
     &             TTRMOUT(IREC,ISRC,12)) THEN ! If UEFFD > UEFF: Direct meander plume
            IF(TTRMOUT(IREC,ISRC,13) .GT. 0.0) THEN
              TTRMTIME = DABS(TTRMOUT(IREC,ISRC,11) /
     &                     TTRMOUT(IREC,ISRC,13)) ! DISTR / UEFFD
            ELSE
              TTRMTIME = 0.
            ENDIF
            IF (TTRMDBG) THEN
              TTRMOUT(IREC,ISRC,7) = TTRMOUT(IREC,ISRC,11) ! DISTR
              TTRMOUT(IREC,ISRC,8) = TTRMOUT(IREC,ISRC,13) ! UEFFD
              TTRMOUT(IREC,ISRC,9) = TTRMTIME
              TTRMSRC(IREC,ISRC) = 'UNST-M-DIR'
            END IF
          ELSE ! If UEFFD < UEFF: INDIRECT Meander plume
            IF(TTRMOUT(IREC,ISRC,12) .GT. 0.0) THEN
              TTRMTIME = DABS(TTRMOUT(IREC,ISRC,11) /
     &                     TTRMOUT(IREC,ISRC,12)) ! DISTR / UEFF
            ELSE
              TTRMTIME = 0.
            ENDIF
            IF (TTRMDBG) THEN
              TTRMOUT(IREC,ISRC,7) = TTRMOUT(IREC,ISRC,11) ! DISTR
              TTRMOUT(IREC,ISRC,8) = TTRMOUT(IREC,ISRC,12) ! UEFF
              TTRMOUT(IREC,ISRC,9) = TTRMTIME
              TTRMSRC(IREC,ISRC) = 'UNST-M-IND'
            END IF
          END IF
        ELSE ! for COHERENT plume
          IF (TTRMOUT(IREC,ISRC,10) .GE. 0.0) THEN ! For downwind distances >= 0.0 use the DOWNWIND distance
            IF (STABLE) THEN ! Use  eff. wspd (UEFF)
              IF(TTRMOUT(IREC,ISRC,12) .GT. 0.0) THEN
                TTRMTIME = DABS(TTRMOUT(IREC,ISRC,10)/
     &                       TTRMOUT(IREC,ISRC,12)) ! X / UEFF
              ELSE
                TTRMTIME = 0.
              ENDIF
              IF (TTRMDBG) THEN
                TTRMOUT(IREC,ISRC,7) = TTRMOUT(IREC,ISRC,10) ! X
                TTRMOUT(IREC,ISRC,8) = TTRMOUT(IREC,ISRC,12) ! UEFF
                TTRMOUT(IREC,ISRC,9) = TTRMTIME
                TTRMSRC(IREC,ISRC) = 'STAB-C-UEF'
              END IF
            ELSE IF (TTRMOUT(IREC,ISRC,13) .GT.
     &               TTRMOUT(IREC,ISRC,12)) THEN ! IF UEFFD > UEFF: DIRECT COHERENT
              IF(TTRMOUT(IREC,ISRC,13) .GT. 0.0) THEN
                TTRMTIME = DABS(TTRMOUT(IREC,ISRC,10)/
     &                       TTRMOUT(IREC,ISRC,13)) ! X / UEFFD
              ELSE
                TTRMTIME = 0.
              ENDIF
              IF (TTRMDBG) THEN
                TTRMOUT(IREC,ISRC,7) = TTRMOUT(IREC,ISRC,10) ! X
                TTRMOUT(IREC,ISRC,8) = TTRMOUT(IREC,ISRC,13) ! UEFFD
                TTRMOUT(IREC,ISRC,9) = TTRMTIME
                TTRMSRC(IREC,ISRC) = 'UNST-C-DIR'
              END IF
            ELSE ! UEFFD < UEFF: INDIRECT COHERENT
              IF(TTRMOUT(IREC,ISRC,12) .GT. 0.0) THEN
                TTRMTIME = DABS(TTRMOUT(IREC,ISRC,10)/
     &                       TTRMOUT(IREC,ISRC,12)) ! X / UEFF
              ELSE
                TTRMTIME = 0.
              ENDIF
              IF (TTRMDBG) THEN
                TTRMOUT(IREC,ISRC,7) = TTRMOUT(IREC,ISRC,10) ! X
                TTRMOUT(IREC,ISRC,8) = TTRMOUT(IREC,ISRC,12) ! UEFF
                TTRMOUT(IREC,ISRC,9) = TTRMTIME
                TTRMSRC(IREC,ISRC) = 'UNST-C-IND'
              END IF
            END IF
          ELSE ! for DOWNWIND distance < 0, use RADIAL distance
            IF (STABLE) THEN ! Use  eff. wspd (UEFFD)
              IF(TTRMOUT(IREC,ISRC,12) .GT. 0.0) THEN
                TTRMTIME = DABS(TTRMOUT(IREC,ISRC,11)/
     &                       TTRMOUT(IREC,ISRC,12)) ! DISTR / UEFF
              ELSE
                TTRMTIME = 0.
              ENDIF
              IF (TTRMDBG) THEN
                TTRMOUT(IREC,ISRC,7) = TTRMOUT(IREC,ISRC,11) ! DISTR
                TTRMOUT(IREC,ISRC,8) = TTRMOUT(IREC,ISRC,12) ! UEFF
                TTRMOUT(IREC,ISRC,9) = TTRMTIME
                TTRMSRC(IREC,ISRC) = 'STAB-m-UEF'
              END IF
            ELSE IF (TTRMOUT(IREC,ISRC,13) .GT.
     &               TTRMOUT(IREC,ISRC,12)) THEN ! IF UEFFD > UEFF: DIRECT COHERENT
              IF(TTRMOUT(IREC,ISRC,13) .GT. 0.0) THEN
                TTRMTIME = DABS(TTRMOUT(IREC,ISRC,11)/
     &                       TTRMOUT(IREC,ISRC,13)) ! DISTR / UEFFD
              ELSE
                TTRMTIME = 0.
              ENDIF
              IF (TTRMDBG) THEN
                TTRMOUT(IREC,ISRC,7) = TTRMOUT(IREC,ISRC,11) ! X
                TTRMOUT(IREC,ISRC,8) = TTRMOUT(IREC,ISRC,13) ! UEFFD
                TTRMOUT(IREC,ISRC,9) = TTRMTIME
                TTRMSRC(IREC,ISRC) = 'UNST-m-DIR'
              END IF
            ELSE ! UEFFD < UEFF: INDIRECT COHERENT
              IF(TTRMOUT(IREC,ISRC,12) .GT. 0.0) THEN
                TTRMTIME = DABS(TTRMOUT(IREC,ISRC,11)/
     &                       TTRMOUT(IREC,ISRC,12)) ! DISTR / UEFF
              ELSE
                TTRMTIME = 0.
              ENDIF
              IF (TTRMDBG) THEN
                TTRMOUT(IREC,ISRC,7) = TTRMOUT(IREC,ISRC,11) ! DISTR
                TTRMOUT(IREC,ISRC,8) = TTRMOUT(IREC,ISRC,12) ! UEFF
                TTRMOUT(IREC,ISRC,9) = TTRMTIME
                TTRMSRC(IREC,ISRC) = 'UNST-m-IND'
              END IF
            END IF
          END IF
        END IF
      ELSE IF (SRCTYP(ISRC)(1:4) .EQ. 'AREA') THEN
!     Perform TTRM calcs for AREA, AREAPOLY or AREACIRC sources
        IF (STABLE) THEN ! Use  eff. wspd (UEFF)
          IF(TTRMOUT(IREC,ISRC,12) .GT. 0.0) THEN
            TTRMTIME = DABS(TTRMOUT(IREC,ISRC,11)/
     &                   TTRMOUT(IREC,ISRC,12)) ! DISTR / UEFF
          ELSE
            TTRMTIME = 0.
          ENDIF
          IF (TTRMDBG) THEN
            TTRMOUT(IREC,ISRC,7) = TTRMOUT(IREC,ISRC,11) ! DISTR
            TTRMOUT(IREC,ISRC,8) = TTRMOUT(IREC,ISRC,12) ! UEFF
            TTRMOUT(IREC,ISRC,9) = TTRMTIME
            TTRMSRC(IREC,ISRC) = 'AREA-STAB'
          END IF
        ELSE IF (TTRMOUT(IREC,ISRC,13) .GT.
     &           TTRMOUT(IREC,ISRC,12)) THEN ! IF UEFFD > UEFF: DIRECT COHERENT
          IF(TTRMOUT(IREC,ISRC,13) .GT. 0.0) THEN
            TTRMTIME = DABS(TTRMOUT(IREC,ISRC,11)/
     &                   TTRMOUT(IREC,ISRC,13)) ! DISTR / UEFFD
          ELSE
            TTRMTIME = 0.
          ENDIF
          IF (TTRMDBG) THEN
            TTRMOUT(IREC,ISRC,7) = TTRMOUT(IREC,ISRC,11) ! DISTR
            TTRMOUT(IREC,ISRC,8) = TTRMOUT(IREC,ISRC,13) ! UEFFD
            TTRMOUT(IREC,ISRC,9) = TTRMTIME
            TTRMSRC(IREC,ISRC) = 'AREA-U-DIR'
          END IF
        ELSE ! UEFFD < UEFF: INDIRECT COHERENT
          IF(TTRMOUT(IREC,ISRC,12) .GT. 0.0) THEN
            TTRMTIME = DABS(TTRMOUT(IREC,ISRC,11)/
     &                   TTRMOUT(IREC,ISRC,12)) ! DISTR / UEFF
          ELSE
            TTRMTIME = 0.
          ENDIF
          IF (TTRMDBG) THEN
            TTRMOUT(IREC,ISRC,7) = TTRMOUT(IREC,ISRC,11) ! DISTR
            TTRMOUT(IREC,ISRC,8) = TTRMOUT(IREC,ISRC,12) ! UEFF
            TTRMOUT(IREC,ISRC,9) = TTRMTIME
            TTRMSRC(IREC,ISRC) = 'AREA-U-IND'
          END IF
        END IF
      END IF

      RETURN
      END


      SUBROUTINE ARM2_CALC
C***********************************************************************
C             ARM2_CALC Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes Hourly Results for ARM2 Option
C
C        PROGRAMMER: Roger W. Brode, U.S. EPA/AQMG
C
C        DATE:    November 19, 2013
C
C
C        INPUTS:
C
C
C        OUTPUTS:
C
C
C        CALLED FROM:   HRLOOP
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      DOUBLE PRECISION :: ARM2_Coef(7)
      DOUBLE PRECISION :: NOXCONC(NUMREC,NUMTYP)

C     Variable Initializations
      DATA ARM2_Coef / -1.1723D-17, 4.2795D-14, 5.8345D-11, 3.4555D-08,
     &                  5.6062D-06, 2.7383D-03, 1.2441D+00 /

      MODNAM = 'ARM2_CALC'
      NOXCONC(:,:) = 0.0D0
      HRVAL(:)     = 0.0D0
      RatioARM2    = 0.0D0

      IF (.NOT.EVONLY) THEN
C ---    Non-EVENT processing mode

C ---    Calculate total NOXCONC for each receptor from all sources
C        based on the CHI(IREC,ISRC,ITYP) array
         NOXCONC(:,:) = SUM(CHI,DIM=2)

C ---    Begin Receptor LOOP
         RECEPTOR_LOOP: DO IREC = 1, NUMREC

C ---       Initialize ARM2 Ratio for this receptor
            RatioARM2 = 0.0D0

C           Calculate ARM2 ratio, RatioARM2, based on NOXCONC for this receptor for ITYP=1
            RatioARM2 = (ARM2_Coef(1)*NOXCONC(IREC,1)**6) +
     &                  (ARM2_Coef(2)*NOXCONC(IREC,1)**5) -
     &                  (ARM2_Coef(3)*NOXCONC(IREC,1)**4) +
     &                  (ARM2_Coef(4)*NOXCONC(IREC,1)**3) -
     &                  (ARM2_Coef(5)*NOXCONC(IREC,1)**2) -
     &                   ARM2_Coef(6)*NOXCONC(IREC,1) + ARM2_Coef(7)

C           Adjust RatioARM2 if needed based on ARM2_Max and ARM2_min
            IF( RatioARM2 .gt. ARM2_Max )THEN
                RatioARM2 = ARM2_Max
            ELSEIF( RatioARM2 .lt. ARM2_min )THEN
                RatioARM2 = ARM2_min
            ENDIF

C ---       Begin Source Group Loop
            GROUP_LOOP: DO IGRP = 1, NUMGRP

C ---        Begin Source Loop
             SOURCE_LOOP: DO ISRC = 1, NUMSRC

               DO ITYP = 1, NUMTYP

C ---             Assign CHI value adjusted by RatioARM2 to HRVAL
                  HRVAL(ITYP) = CHI(IREC,ISRC,ITYP) * RatioARM2
!! Added for TTRM2
       IF (RUNTTRM2) THEN
         IF (TTRM2DBG .AND. (CMETH .LT. 3)) THEN
           WRITE(TTRM2TMP(2),PSTFRM,ERR=87) AXR(IREC), AYR(IREC),
     &                 HRVAL(1),
     &                 AZELEV(IREC), AZHILL(IREC), AZFLAG(IREC),
     &                 CHRAVE(1), GRPID(IGRP), KURDAT, SRCID(ISRC)
           GO TO 887
 87        WRITE(DUMMY,'("PSTFL",I4.4)') TTRM2TMP(2)
           CALL ERRHDL(PATH,MODNAM,'E','520',DUMMY)
           RUNERR = .TRUE.
 887       CONTINUE
         ENDIF
         IF (TTRMCOMPARE(IGRP,ISRC,IREC,ITYP) .LT. HRVAL(ITYP)) THEN
             HRVAL(ITYP) = TTRMCOMPARE(IGRP,ISRC,IREC,ITYP)
         ENDIF
         IF (TTRM2DBG .AND. (CMETH .LT. 3)) THEN
           WRITE(TTRM2TMP(3),PSTFRM,ERR=97) AXR(IREC), AYR(IREC),
     &                 HRVAL(1),
     &                 AZELEV(IREC), AZHILL(IREC), AZFLAG(IREC),
     &                 CHRAVE(1), GRPID(IGRP), KURDAT, SRCID(ISRC)
           GO TO 897
 97        WRITE(DUMMY,'("PSTFL",I4.4)') TTRM2TMP(3)
           CALL ERRHDL(PATH,MODNAM,'E','520',DUMMY)
           RUNERR = .TRUE.
 897       CONTINUE
         ENDIF
       ENDIF
!! End of TTRM2 insert; Nov. 2021
               END DO

C              Call SUMVAL to update AVEVAL, ANNVAL, and SHVALS Arrays          ---   CALL SUMVAL
               CALL SUMVAL

C ---          Write data to ARM2 debugging file if needed
               if (ARM2DEBUG) then
                   if (IGROUP(ISRC,IGRP) .EQ. 1) then
                    write(ARM2DBG,9987,ERR=998) kurdat, irec,
     &                       grpid(igrp), isrc, srcid(isrc),
     &                       NOXCONC(IREC,1), CHI(IREC,ISRC,1),
     &                       RatioARM2, HRVAL(1),
     &                       AVEVAL(IREC,IGRP,1,1)
9987                format(1x,i8.8,i6,2x,a8,i6,2x,a12,2x,
     &                                          5(1x,e12.5))
                   end if
               end if

               go to 1234
998            CALL ERRHDL(PATH,MODNAM,'E','520','ARM2DEBUG')
               RUNERR = .TRUE.
1234           continue

            END DO SOURCE_LOOP

            IF (GRP_BACK(IGRP)) THEN

               CALL SUMBACK_NO2

C ---          Write data to ARM2 debugging file
               if (ARM2DEBUG) then
                  write(ARM2DBG,99871,ERR=9981) kurdat, irec,
     &                      grpid(IGRP), 'BACKGROUND',
     &                      BGCONC, AVEVAL(IREC,IGRP,1,1)
99871             format(1x,i8.8,i6,2x,a8,8x,a10,43x,2(1x,e12.5))
               end if

               GO TO 2345
C ---          Issue error message for writing to ARMDEBUG file
9981           CALL ERRHDL(PATH,MODNAM,'E','520','ARMDEBUG')
               RUNERR = .TRUE.
2345           CONTINUE

            END IF

           END DO GROUP_LOOP

         END DO RECEPTOR_LOOP

      ELSE IF (EVONLY) THEN
C ---    EVENT Processing; initialize arrays
         EV_AVEVAL(:) = 0.0D0
C ---    Initialize ARM2 Ratio to 0.0D0
         RatioARM2    = 0.0D0

C ---    Calculate total NOXCONC for this EVENT from all sources
C        based on the CHI(IREC,ISRC,ITYP) array
         NOXCONC = SUM(CHI,DIM=2)

C        Calculate ARM2 ratio, RatioARM2, based on NOXCONC
         RatioARM2 = (ARM2_Coef(1)*NOXCONC(1,1)**6) +
     &               (ARM2_Coef(2)*NOXCONC(1,1)**5) -
     &               (ARM2_Coef(3)*NOXCONC(1,1)**4) +
     &               (ARM2_Coef(4)*NOXCONC(1,1)**3) -
     &               (ARM2_Coef(5)*NOXCONC(1,1)**2) -
     &                ARM2_Coef(6)*NOXCONC(1,1) + ARM2_Coef(7)

C        Adjust RatioARM2 if needed based on ARM2_Max and ARM2_min
         IF (RatioARM2 .gt. ARM2_Max) THEN
             RatioARM2 = ARM2_Max
         ELSE IF (RatioARM2 .lt. ARM2_min) THEN
             RatioARM2 = ARM2_min
         END IF

C ---    Begin Source Loop
         EV_SOURCE_LOOP: DO ISRC = 1, NUMSRC

            ITYP = 1

C ---       Apply RatioARM2 to hourly value from CHI array and
C           assign to HRVAL array; then call EV_SUMVAL
            HRVAL(ITYP) = CHI(1,ISRC,ITYP) * RatioARM2
!! Added for TTRM2
       IF (RUNTTRM2) THEN
         IF (TTRMCOMPARE(IGRP,ISRC,1,ITYP) .LT. HRVAL(ITYP)) THEN
             HRVAL(ITYP) = TTRMCOMPARE(IGRP,ISRC,1,ITYP)
         ENDIF
       ENDIF
!! End of TTRM2 insert; Nov. 2021
C           Sum HRVAL to HRVALS and EV_AVEVAL    ---   CALL EV_SUMVAL
            IF (IGROUP(ISRC,IDXEV(IEVENT)) .EQ. 1) THEN
               CALL EV_SUMVAL

C ---          Write data to ARM2 debugging file
               if (ARM2DEBUG) then
                  if (IGROUP(ISRC,IDXEV(IEVENT)) .EQ. 1) then
                     write(ARM2DBG,99872,ERR=999) kurdat, ievent,
     &                     EVNAME(IEVENT), EVAPER(IEVENT),
     &                     grpid(IDXEV(IEVENT)), isrc, srcid(isrc),
     &                     NOXCONC(1,1), CHI(1,ISRC,1), RatioARM2,
     &                     HRVAL(1), GRPAVE(IDXEV(IEVENT))
                  end if
               end if
99872          format(1x,i8.8,i6,2x,a10,i5,2x,a8,1x,i5,2x,a12,2x,
     &                                               5(1x,e12.5))

            END IF

            go to 3456
999         CALL ERRHDL(PATH,MODNAM,'E','520','ARM2DEBUG')
            RUNERR = .TRUE.
3456        continue

         END DO EV_SOURCE_LOOP

C        Sum BACKGRND to HRVALS, EV_AVEVAL, GRPVAL and GRPAV Arrays ---   CALL EV_SUMBACK
           IF (IDXEV(IEVENT) .EQ. IGRP .AND.
     &                            GRP_BACK(IDXEV(IEVENT))) THEN
            CALL EV_SUMBACK

C ---       Write data to ARM2 debugging file
            if (ARM2DEBUG) then
               write(ARM2DBG,99881,ERR=9991) kurdat, ievent,
     &               EVNAME(IEVENT), EVAPER(IEVENT),
     &               grpid(IDXEV(IEVENT)), 'BACKGROUND',
     &               EV_BGCONC(IHOUR), GRPAVE(IDXEV(IEVENT))
99881          format(1x,i8.8,i6,2x,a10,1x,i4,2x,a8,8x,a10,43x,
     &                                            2(1x,e12.5))
            end if

            GO TO 4567
C ---       Issue error message for writing to ARM2DEBUG file
9991        CALL ERRHDL(PATH,MODNAM,'E','520','ARM2DEBUG')
            RUNERR = .TRUE.
4567        CONTINUE

         END IF

      END IF

      RETURN
      END

      SUBROUTINE PVMRM_CALC(SRCS2USE)
C***********************************************************************
C             PVMRM_CALC Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes Hourly Results for PVMRM Option
C
C        PROGRAMMER: Roger W. Brode, MACTEC, Inc. (f/k/a PES, Inc.)
C
C        DATE:    May 12, 2004
C
C        MODIFIED:   Include calls to SETSRC for all source types
C                    to correct initialization problems with PVMRM.
C                    Also include call to subroutine EMFACT to apply
C                    emission factors, if appropriate, in order to
C                    use the EMISFACT-adjusted emission rates in
C                    the calculation of the moles of NOx.
C                    Also assigned file unit 9 to variable PVMDBG
C                    for the PVMRM debug file, and adjusted the
C                    PVMRM debug output to report total PercentNO2,
C                    including in-stack contribution.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
C
C                    Retrieve WDSIN and WDCOS for dominant source from
C                    new arrays that save values by source. Corrects
C                    potential problem for PVMRM applications with
C                    multi-level wind inputs.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 01/26/2007
C
C                    Removed code related to EVENT processing and
C                    EVALCART options that are not supported for
C                    PSDCREDIT option.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 01/24/2007
C
C        INPUTS:  SRCS2USE - 'ALLSRCS' = all three PSDGROUPs
C                            'NAAQSRC' = existing baseline and
C                                        increment consuming PSDGROUPs
C                            'ALLBASE' = existing and retired baseline
C                                        PSDGROUPs
C
C        OUTPUTS:
C
C
C        CALLED FROM:   HRLOOP
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER SRCS2USE*7
      CHARACTER MODNAM*12

      DOUBLE PRECISION :: MAXCHI(NUMREC,NUMTYP), MAXCONC
      DOUBLE PRECISION :: BVERT,BVERT3, BHORIZ,BHORIZ3, VOLDOM,VOLSUM,
C     JAT 7/22/21 YDOM NOT USED
C     &                    XDOM, YDOM, DISTDOM, UDOM, FDOM
     &                    XDOM, DISTDOM, UDOM, FDOM
      DOUBLE PRECISION :: domO3moles, sumO3moles, domNOxmoles,
     &                    sumNOxmoles, domConverted, sumConverted,
     &                    PercentNO2
      DOUBLE PRECISION :: AVE_NO2RATIO, AVE_NO2RATIO3
      DOUBLE PRECISION :: ZCORR, PCORR, TCORR, PTZ, TAP
      DOUBLE PRECISION :: XDEP, WIDTH, LENGTH, XMINR, XMAXR
      DOUBLE PRECISION :: QPTOT
      INTEGER :: DOMIDX(NUMREC,NUMTYP), IDOM, NDXBLZ

C     Variable Initializations
      MODNAM = 'PVMRM_CALC'
      WIDTH  = 0.0D0
      LENGTH = 0.0D0
      BHORIZ = 0.0D0
      BHORIZ3= 0.0D0
      BVERT  = 0.0D0
      BVERT3 = 0.0D0

C --- Initialize AVE_NO2RATIO and other variables:
      AVE_NO2RATIO = 0.0D0
      AVE_NO2RATIO3= 0.0D0
      domO3moles   = 0.0D0
      sumO3moles   = 0.0D0
      domNOxmoles  = 0.0D0
      sumNOxmoles  = 0.0D0
      domConverted = 0.0D0
      sumConverted = 0.0D0
      PercentNO2   = 0.0D0

C --- Initialize MAXCHI and DOMIDX?
      MAXCHI = 0.0D0
      DOMIDX = 0

C     Determine maximum values and corresponding index by receptor
      IF (TRIM(SRCS2USE) .EQ. 'ALLSRCS') THEN
C        Standard (Non-PSDCREDIT) run; use the global array operators
C        MAXVAL and MAXLOC to determine max chi val and receptor index
         MAXCHI = MAXVAL(CHI,DIM=2)
         DOMIDX = MAXLOC(CHI,DIM=2)
      ELSE
C        PSDCREDIT run; use the routine AERMAX to limit which sources to
C        use in determining the dominant and major contributing sources
         CALL AERMAX(SRCS2USE,MAXCHI,DOMIDX)
      END IF

C     Begin Receptor LOOP
      RECEPTOR_LOOP: DO IREC = 1, NUMREC

C        Get max concentration and index for dominant source for
C        this receptor
         MAXCONC = MAXCHI(IREC,1)
         IDOM    = DOMIDX(IREC,1)
         IF (MAXCONC .EQ. 0.0D0) THEN
C           No impacts, cycle to next receptor
            HRVAL  = 0.0D0
            AERVAL = 0.0D0
            CYCLE RECEPTOR_LOOP
         END IF

C        Assign effective wind speed and terrain weighting factor
C        for dominant source, IDOM
         UDOM = UEFFS(IREC,IDOM)
         FDOM = FOPTS(IREC,IDOM)

C        Assign wind direction based on dominant source
         WDSIN = AWDSIN(IDOM)
         WDCOS = AWDCOS(IDOM)
         AFV   = AAFV(IDOM)

C        Determine extent of major contributing sources;
C        first initialize variables
         HMNT  =   1.0D10
         HMXT  =  -1.0D10
         HMNH  =   1.0D10
         HMXH  =  -1.0D10
         HMNT3 =   1.0D10
         HMXT3 =  -1.0D10
         HMNH3 =   1.0D10
         HMXH3 =  -1.0D10
         CWMIN =   1.0D20
         CWMAX =  -1.0D20
         CWMIN3=   1.0D20
         CWMAX3=  -1.0D20
         DWMIN =   1.0D20
         DWMAX =  -1.0D20
         DWMIN3=   1.0D20
         DWMAX3=  -1.0D20
         XMINR =   1.0D20
         XMAXR =  -1.0D20
         WIDTH  =  0.0D0
         LENGTH =  0.0D0
         BHORIZ =  0.0D0
         BHORIZ3=  0.0D0
         BVERT  =  0.0D0
         BVERT3 =  0.0D0

         NUMCONT = 0
C ---    Loop through sources to obtain major contributing sources
C        Consideration given as to whether this is an emission credit run
C         'ALLSRCS' = full PVMRM run
C         'NAAQSRC' = increment-consuming and non-retired baseline
C         'ALLBASE' = non-retired baseline and retired baseline
C
C        MAJOR_CONT subroutine also defines the lateral (crosswind)
C        and longitudinal (alongwind) extent of major contributing
C        sources, through CWMIN/CWMAX and DWMIN/DWMAX
         SOURCE_LOOP1: DO ISRC = 1, NUMSRC

            IF (.NOT. PSDCREDIT) THEN
C              Full PVMRM run
C              Included FDOM fraction in call to MAJOR_CONT (via IDOM srcid)
               CALL MAJOR_CONT(MAXCONC,IDOM)

            ELSE IF (PSDCREDIT .AND. TRIM(SRCS2USE) .EQ. 'NAAQSRC') THEN
C              Emission credit run: Increment Consumption+Nonretired Baseline=NAAQS
               IF (PSDSRCTYP(ISRC) .EQ. 'IC' .OR.
     &             PSDSRCTYP(ISRC) .EQ. 'NB') THEN
                  CALL MAJOR_CONT(MAXCONC,IDOM)
               ELSE
                  CYCLE SOURCE_LOOP1
               END IF

            ELSE IF (PSDCREDIT .AND. TRIM(SRCS2USE) .EQ. 'ALLBASE') THEN
C              Emission credit run: Nonretired Baseline and Retired Baseline
               IF (PSDSRCTYP(ISRC) .EQ. 'NB' .OR.
     &             PSDSRCTYP(ISRC) .EQ. 'RB') THEN
                  CALL MAJOR_CONT(MAXCONC,IDOM)
               ELSE
                  CYCLE SOURCE_LOOP1
               END IF
            END IF

         END DO SOURCE_LOOP1

C        Set vertical dimensions of "box" for major cont. sources.
C        Use terrain weighting factor for dominant source (FDOM) to
C        combine heights for horizontal and terrain responding plumes.
         BVERT = FDOM*(HMXH - HMNH) + (1.0D0-FDOM)*(HMXT - HMNT)
         IF (UNSTAB .AND. PPFACT(IREC,IDOM) .GT. 0.0D0) THEN
            BVERT3 = FDOM*(HMXH3 - HMNH3) + (1.0D0-FDOM)*(HMXT3 - HMNT3)
         ELSE
            BVERT3 = 0.0D0
         END IF

C        Set horizontal dimensions of "box" for major cont. sources.
         IF (CWMAX .GT. CWMIN) THEN
            BHORIZ = CWMAX - CWMIN
         ELSE
            BHORIZ = 0.0D0
         END IF

C        Set horizontal dimensions of "box" for major cont. sources.
C        for penetrated plumes
         IF (CWMAX3 .GT. CWMIN3) THEN
            BHORIZ3 = CWMAX3 - CWMIN3
         ELSE
            BHORIZ3 = 0.0D0
         END IF

C ---    Assign temporary global source index based on dominant source
         ISRC = IDOM

C ---    Calculate Distance from Dominant Source (and other parameters)
         IF (SRCTYP(IDOM)(1:4) .EQ. 'AREA' .OR.
     &            SRCTYP(IDOM) .EQ. 'LINE') THEN
C ---       Calculations for AREA source types
            CALL SETSRC
            CALL EMFACT(QS)

            IF (EVONLY) THEN
               CALL ARDIST(IEVENT,XDEP,WIDTH,LENGTH,XMINR,XMAXR)
            ELSE
               CALL ARDIST(IREC,XDEP,WIDTH,LENGTH,XMINR,XMAXR)
            END IF
C           Assign downwind distance (XDOM) and crosswind distance (YDOM) based on
C           distance from center of AREA/LINE source
            XDOM = X
C     JAT 7/22/21 YDOM NOT USED
C            YDOM = Y

C ---       Use maximum of downwind distance from center of source to
C           receptor (X) and 1m for PVMRM option
            DISTDOM = MAX( X, 1.0D0 )

C ---       Apply maximum value of 4.3*SY (based on distance from center of
C           the source) for WIDTH of AREA/LINE source for the PVMRM option;
C           otherwise use full projected WIDTH of source
            IF (PVMRM) THEN
               CALL ADISY(DISTDOM)
               IF (4.3D0*SY .LT. WIDTH) THEN
                  WIDTH = 4.3D0*SY
               ENDIF
            END IF

C           Calculate volume of dominant plume, passing WIDTH for BHORIZ
            CALL PLUME_VOL(DISTDOM,IREC,IDOM,0.0D0,0.0D0,WIDTH,
     &                                             0.0D0,VOLDOM)

         ELSE IF (SRCTYP(IDOM) .EQ. 'OPENPIT') THEN
C ---       Calculations for OPENPIT sources
            CALL SETSRC
C*          Determine the AlongWind Length of the OPENPIT Source  ---   CALL LWIND
            CALL LWIND

C*          Calculate the Relative Depth of the OPENPIT Source    ---   CALL PDEPTH
            CALL PDEPTH

C*          Calculate the Fractional Size of the
C*          Effective Pit Area (PITFRA)                           ---   CALL PTFRAC
            CALL PTFRAC

C*          Determine the Coordinates of the Effective Pit Area
C*          in Wind Direction Coordinate System                   ---   CALL PITEFF
            CALL PITEFF

C*          Calculate the adusted Emission Rate per unit area (QEFF) for the
C*          Effective Pit Area (PITFRA)                           ---   CALL PITEMI
C*          First assign source emission rate to QPTOT to get adjusted
C*          emission rate per unit area of effective source
            QPTOT = QS
            CALL PITEMI(QPTOT)
            CALL EMFACT(QEFF)

            IF (EVONLY) THEN
               CALL ARDIST(IEVENT,XDEP,WIDTH,LENGTH,XMINR,XMAXR)
            ELSE
               CALL ARDIST(IREC,XDEP,WIDTH,LENGTH,XMINR,XMAXR)
            END IF
C           Assign downwind distance (XDOM) and crosswind distance (YDOM) based on
C           distance from center of area source
            XDOM = X
C     JAT 7/22/21 YDOM NOT USED
C            YDOM = Y

C ---       Use maximum of downwind distance from center of source to
C           receptor (X) and 1m for PVMRM option
            DISTDOM = MAX( X, 1.0D0 )

C ---       Apply maximum value of 4.3*SY (based on distance from center of
C           the source) for WIDTH of OPENPIT source for the new PVMRM option;
C           otherwise use full projected WIDTH of source
            IF (PVMRM) THEN
               CALL ADISY(DISTDOM)
               IF (4.3D0*SY .LT. WIDTH) THEN
                  WIDTH = 4.3D0*SY
               ENDIF
            END IF

C           Calculate volume of dominant plume, passing WIDTH for BHORIZ
            CALL PLUME_VOL(DISTDOM,IREC,IDOM,0.0D0,0.0D0,WIDTH,
     &                                             0.0D0,VOLDOM)

         ELSE
C ---       Calculations for POINT or VOLUME sources
            CALL SETSRC
            CALL EMFACT(QS)

            IF (EVONLY) THEN
               CALL XYDIST(IEVENT)
            ELSE
               CALL XYDIST(IREC)
            END IF

            IF( SRCTYP(IDOM) .EQ. 'VOLUME' )THEN
C ---          Assign XDOM, based on X, and WIDTH for VOLUME source
               XDOM  = MAX( X, 1.0D0 )
C              Assign WIDTH for VOLUME source based on initial sigma-y
               WIDTH = 4.3D0*ASYINI(IDOM)
            ELSE
C ---          Assign XDOM, based on X, and WIDTH for POINT source
               XDOM  = MAX( X, 1.0D0 )
               WIDTH = 0.0D0
            END IF
C ---       Assign YDOM based on Y
C     JAT 7/22/21 YDOM NOT USED
C           YDOM = Y

C ---       Use maximum of downwind distance to receptor (X) and 1m for PVMRM option
            DISTDOM = MAX( XDOM, 1.0D0 )

C           Calculate volume of dominant plume
            CALL PLUME_VOL(DISTDOM,IREC,IDOM,0.0D0,0.0D0,WIDTH,
     &                                             0.0D0,VOLDOM)
         END IF

         IF (UNSTAB .AND. DISTDOM .GE. XMIXED) THEN
C ---       Set BVERT = 0.0 if X is >/= distance to uniform vertical mixing
            BVERT = 0.0D0
         ELSE
            BVERT = FDOM*(HMXH - HMNH) + (1.0D0-FDOM)*(HMXT - HMNT)
         END IF

         IF (BVERT .GT. 0.0D0 .OR. BVERT3 .GT. 0.0D0 .OR.
     &      BHORIZ .GT. 0.0D0 .OR. BHORIZ3 .GT. 0.0D0) THEN
C           Calculate volume of dominant plus major contributing sources
            CALL PLUME_VOL(DISTDOM,IREC,IDOM,BVERT,BVERT3,
     &                                      BHORIZ,BHORIZ3,VOLSUM)
         ELSE
            VOLSUM = VOLDOM
         END IF

C        Re-Assign wind direction based on dominant source, since values
C        are overwritten in SUB. PLUME_VOL (call to METINI).
         WDSIN = AWDSIN(IDOM)
         WDCOS = AWDCOS(IDOM)
         AFV   = AAFV(IDOM)          ! Add AAFV array

C        Correct plume volume to conditions of standard temperature
C        and pressure (Hanrahan, 1999)
C        0.028966 is the average kg/mole of air

C        First obtain a height to use in the correction
         ZCORR = AZS(IDOM) + HECNTR(IREC,IDOM)

C        Calculate ambient temperature at plume height from pot. temp. profile
         CALL LOCATE(GRIDHT, 1, MXGLVL, ZCORR, NDXBLZ)
         IF (NDXBLZ .GE. 1) THEN
C----       Potential temperature
            CALL GINTRP( GRIDHT(NDXBLZ), GRIDPT(NDXBLZ),
     &                   GRIDHT(NDXBLZ+1), GRIDPT(NDXBLZ+1),
     &                   ZCORR, PTZ )
         ELSE
C           Use GRID value for lowest level
            PTZ = GRIDPT(1)
         END IF
         TAP = PTZ - GOVRCP * ZCORR

         PCORR = DEXP(-G * 0.028966D0 * ZCORR / ( RGAS * TAP))
         TCORR = 273.15D0 / TAP

         VOLDOM = VOLDOM * PCORR * TCORR
         VOLSUM = VOLSUM * PCORR * TCORR

C ---    Check for O3MISS and adjust calculations
         IF (.NOT. O3MISS) THEN
c           get moles of ozone in dominant plume
C           O3CONC expressed in ug/m3
            domO3moles= VOLdom * O3CONC/(1.0D6 * 48.0D0)

c           get moles of ozone in the combined plume
            sumO3moles= VOLsum * O3CONC/(1.0D6 * 48.0D0)
         ELSE
C ---       Assign domO3moles and sumO3moles = 0.0 for full conversion
C           if O3MISS
            domO3moles= 0.0D0
            sumO3moles= 0.0D0
         END IF

c        get moles of NOx in the dominant plume
C        Use molecular weight of NO2 (46) since emission calcs are based on NO2
         IF (SRCTYP(IDOM) .EQ. 'AREA' .OR.
     &       SRCTYP(IDOM) .EQ. 'AREAPOLY' .OR.
     &       SRCTYP(IDOM) .EQ. 'LINE') THEN
            domNOxmoles = QTK*XINIT*YINIT *
     &           (1.0D0-ANO2_RATIO(IDOM)) * DISTDOM/(UDOM*46.0D0)
         ELSE IF (SRCTYP(IDOM) .EQ. 'AREACIRC') THEN
            domNOxmoles = QTK*PI*RADIUS(IDOM)*RADIUS(IDOM) *
     &              (1.0D0-ANO2_RATIO(IDOM))*DISTDOM/(UDOM*46.0D0)
         ELSE IF (SRCTYP(IDOM) .EQ. 'OPENPIT') THEN
            domNOxmoles = QEFF*XINIT*YINIT *
     &            (1.0D0-ANO2_RATIO(IDOM)) * DISTDOM/(UDOM*46.0D0)
         ELSE
C ---       Calculate domNOxmoles for POINT and VOLUME sources
            domNOxmoles = QTK * (1.0D0-ANO2_RATIO(IDOM)) *
     &                          DISTDOM/(UDOM * 46.0D0)
         END IF

c        get moles of NOx in the merged plume
C        Sum emissions for sources within projected width of major
C        contributing sources
         QSUM   = 0.0D0
         QSUM3  = 0.0D0
         SUM_NO2RAT  = 0.0D0
         SUM3_NO2RAT = 0.0D0

         SOURCE_LOOP2: DO ISRC = 1, NUMSRC
C ---       Loop through sources to determine the moles of NOx emitted by all
C           sources located within the box defined laterally and vertically
C           by the major contributing sources

            IF (.NOT. PSDCREDIT) THEN
C              Full PVMRM run
               CALL MOLES_NOX(IDOM)

            ELSE IF (PSDCREDIT .AND. TRIM(SRCS2USE) .EQ. 'NAAQSRC') THEN
C              Emission credit run: Increment Consumption+Nonretired Baseline=NAAQS
               IF (PSDSRCTYP(ISRC) .EQ. 'IC' .OR.
     &             PSDSRCTYP(ISRC) .EQ. 'NB') THEN
                  CALL MOLES_NOX(IDOM)
               ELSE
                  CYCLE SOURCE_LOOP2
               END IF

            ELSE IF (PSDCREDIT .AND. TRIM(SRCS2USE) .EQ. 'ALLBASE') THEN
C              Emission credit run: Nonretired Baseline and Retired Basline
               IF (PSDSRCTYP(ISRC) .EQ. 'NB' .OR.
     &             PSDSRCTYP(ISRC) .EQ. 'RB') THEN
                  CALL MOLES_NOX(IDOM)
               ELSE
                  CYCLE SOURCE_LOOP2
               END IF
            END IF

         END DO SOURCE_LOOP2

C----    Check for QSUM = 0.0 error (i.e., dominant source not inc. in sum)
C        This condition should never be encountered.
         IF (QSUM .EQ. 0.0D0 .AND. QSUM3 .EQ. 0.0D0) THEN
C ---      Issue warning messages for cases with QSUM = 0.0
           IF (EVONLY) THEN
              IF (IEVENT .LE. 999) THEN
                 WRITE(DUMMY,'(I3.3,1X,I8.8)') IEVENT, KURDAT
              ELSE IF (IEVENT .LE. 9999) THEN
                 WRITE(DUMMY,'(I4.4,I8.8)') IEVENT, KURDAT
              ELSE
                 WRITE(DUMMY,'(''000'',1X,I8.8)') KURDAT
              END IF
              CALL ERRHDL(PATH,MODNAM,'W','412',EVNAME(IEVENT))
           ELSE
              IF (IREC .LE. 999) THEN
                 WRITE(DUMMY,'(I3.3,1X,I8.8)') IREC, KURDAT
              ELSE IF (IREC .LE. 9999) THEN
                 WRITE(DUMMY,'(I4.4,I8.8)') IREC, KURDAT
              ELSE
                 WRITE(DUMMY,'(''000'',1X,I8.8)') KURDAT
              END IF
              CALL ERRHDL(PATH,MODNAM,'W','411',DUMMY)
           END IF
C          Assign 0.0 to AVE_NO2RATIO
           AVE_NO2RATIO = 0.0D0
C ---      Assign 0.0 to HRVAL and AERVAL, CYCLE RECEPTOR_LOOP unless
C          this is the last receptor, otherwise RETURN
           HRVAL(:)  = 0.0D0
           AERVAL(:) = 0.0D0
           IF (IREC .LT. NUMREC) THEN
              CYCLE RECEPTOR_LOOP
           ELSE
              RETURN
           END IF
         END IF

C        Calculate NOx moles for combined plume, and average in-stack ratio
C        Use molecular weight of NO2 (46) since emission calcs are based on NO2;
C        Use XDOM (downwind distance), which better reflects transport time from
C        source to receptor, instead of DISTDOM (radial distance)
         IF( QSUM .GT. 0.0D0 )THEN
            AVE_NO2RATIO = SUM_NO2RAT/QSUM
         ELSE
            AVE_NO2RATIO = 0.0D0
         ENDIF

         IF( QSUM3 .GT. 0.0D0 )THEN
            AVE_NO2RATIO3 = SUM3_NO2RAT/QSUM3
         ELSE
            AVE_NO2RATIO3 = 0.0D0
         ENDIF

         sumNOxmoles = QSUM * (1.0D0-AVE_NO2RATIO) *
     &                        DISTDOM/(UDOM * 46.0D0)
     &               + QSUM3* (1.0D0-AVE_NO2RATIO3) *
     &                        DISTDOM/(UDOM * 46.0D0)

C ---    Calculate NOx conversion ratios for dominant plume and combined plume;
C        excluding in-stack NO2/NOx ratio(s)
C        Ensure that conversion ratios do not exceed 1.0
         IF( domNOxmoles .GT. 0.0D0 )THEN
            domConverted = MIN( domO3moles/domNOxmoles, 1.0D0 )
         ELSE
            domConverted = 0.0D0
         ENDIF
         IF( sumNOxmoles .GT. 0.0D0 )THEN
            sumConverted = MIN( sumO3moles/sumNOxmoles, 1.0D0 )
         ELSE
            sumConverted = 0.0D0
         ENDIF

C ---    Find which is more important -- the dominant plume or the combined plume;
C        also include appropriate in-stack NO2/NOx contribution.
C        Select the minimum conversion since the conversion for combined plumes
C        should not be any larger than conversion for the single dominant plume.
C ---    Set PercentNO2 = 1.0 if O3MISS, subject to NO2Equil
         IF (O3MISS) THEN
            PercentNO2 = 1.0D0
         ELSE
C ---       Calculate PercentNO2 based on the lessor of domConverted or
C           sumConverted (accounting for in-stack NO2 ratios)
            IF ( (domConverted + ANO2_RATIO(IDOM)) .LE.
     &           (sumConverted + AVE_NO2RATIO) ) THEN
               PercentNO2 = domConverted + ANO2_RATIO(IDOM)
            ELSE
               PercentNO2 = sumConverted + AVE_NO2RATIO
            END IF
         END IF

C ---    Limit conversion to equilibrium concentration of NO2 (default set at 90 percent)
         IF (PercentNO2 .gt. NO2Equil) PercentNO2 = NO2Equil

C        Update HRVAL, AVEVAL and ANNVAL Arrays                         ! jop, 9/30/06
         IF (.NOT. PSDCREDIT) THEN
C ---      Non-PSDCREDIT run; loop through srcgrps and sources for this receptor and
C          this hour or event; also output results to PVMRM debug files if needed

           GROUP_LOOP: DO IGRP = 1, NUMGRP

            SOURCE_LOOP3: DO ISRC = 1, NUMSRC
C ---          Loop through sources again, and apply PercentNO2 to the hourly
C              values before summing the group/event values
               DO ITYP = 1, NUMTYP
                  HRVAL(ITYP) = CHI(IREC,ISRC,ITYP) * PercentNO2
!! Added for TTRM2
!! Perform comparison of calculated hourly NO2 values between PVMRM and TTRM
       IF (RUNTTRM2) THEN
         IF(TTRM2DBG .AND. (CMETH .LT. 3)) THEN
           WRITE(TTRM2TMP(2),PSTFRM,ERR=47) AXR(IREC), AYR(IREC),
     &                    HRVAL(1),
     &                    AZELEV(IREC), AZHILL(IREC), AZFLAG(IREC),
     &                    CHRAVE(1), GRPID(IGRP), KURDAT, SRCID(ISRC)
           GO TO 847
 47        WRITE(DUMMY,'("PSTFL",I4.4)') TTRM2TMP(2)
           CALL ERRHDL(PATH,MODNAM,'E','520',DUMMY)
           RUNERR = .TRUE.
 847       CONTINUE
         ENDIF
         IF (TTRMCOMPARE(IGRP,ISRC,IREC,ITYP) .LT. HRVAL(ITYP)) THEN
             HRVAL(ITYP) = TTRMCOMPARE(IGRP,ISRC,IREC,ITYP)
         ENDIF
         IF (TTRM2DBG .AND. (CMETH .LT. 3)) THEN
           WRITE(TTRM2TMP(3),PSTFRM,ERR=37) AXR(IREC), AYR(IREC),
     &               HRVAL(1),
     &               AZELEV(IREC), AZHILL(IREC), AZFLAG(IREC),
     &               CHRAVE(1), GRPID(IGRP), KURDAT, SRCID(ISRC)
           GO TO 837
 37        WRITE(DUMMY,'("PSTFL",I4.4)') TTRM2TMP(3)
           CALL ERRHDL(PATH,MODNAM,'E','520',DUMMY)
           RUNERR = .TRUE.
 837       CONTINUE
         ENDIF
       ENDIF
!! End of TTRM2 insert; Nov. 2021
               END DO

               IF (EVONLY) THEN
C                 Sum HRVAL to EV_AVEVAL, GRPVAL and GRPAVE Arrays  ---   CALL EV_SUMVAL

                  IF (IDXEV(IEVENT) .EQ. IGRP .AND.
     &                                   IGROUP(ISRC,IGRP) .EQ. 1) THEN
                     CALL EV_SUMVAL

C ---                Write data to PVMRM debug output file
C                    Check whether dominant plume or combined plume is controlling
                     if (PVMRMDBG) then
                       IF ( (domConverted + ANO2_RATIO(IDOM)) .LE.
     &                      (sumConverted + AVE_NO2RATIO) ) THEN
                         write(PVMDBG,9987) 'DOM: ',srcid(idom), kurdat,
     &                     ievent, evname(ievent), evaper(ievent),
     &                     grpid(idxev(ievent)),isrc, srcid(isrc),
     &                     numcont, distdom, maxconc, o3conc,
     &                     domo3moles, domnoxmoles, bhoriz, bvert,
     &                     bvert3, voldom, chi(1,isrc,1), percentno2,
     &                     hrval(1), grpave(idxev(ievent))
                       else
                         write(PVMDBG,9987) 'SUM: ',srcid(idom), kurdat,
     &                     ievent, evname(ievent), evaper(ievent),
     &                     grpid(idxev(ievent)),isrc, srcid(isrc),
     &                     numcont, distdom, maxconc, o3conc,
     &                     sumo3moles, sumnoxmoles, bhoriz, bvert,
     &                     bvert3, volsum, chi(1,isrc,1), percentno2,
     &                     hrval(1), grpave(idxev(ievent))
                       endif
                     endif
9987                 format(1x,a5,1x,a12,2x,i8.8,1x,i5,2x,a12,1x,
     &                      i5,2x,a8,1x,i5,2x,a12,1x,i4,13(2x,e12.5))

                  END IF

               ELSE IF (.NOT. EVONLY) THEN
C ---             Sum HRVAL to AVEVAL, ANNVAL and SHVALS Arrays     ---   CALL SUMVAL

                 if (IGROUP(ISRC,IGRP) .EQ. 1) then
                    CALL SUMVAL

C ---               Write data to PVMRM debug output file
C                   Check whether dominant plume or combined plume is controlling
                    if (PVMRMDBG) then
                     IF ( (domConverted + ANO2_RATIO(IDOM)) .LE.
     &                    (sumConverted + AVE_NO2RATIO) ) THEN
                       write(PVMDBG,99871) 'DOM: ',srcid(idom), kurdat,
     &                     irec, grpid(igrp), isrc, srcid(isrc),
     &                     numcont, distdom, maxconc, o3conc,
     &                     domo3moles, domnoxmoles, bhoriz, bvert,
     &                     bvert3,
     &                     voldom, chi(irec,isrc,1), percentno2,
     &                     hrval(1), aveval(irec,igrp,1,1)
                     else
                       write(PVMDBG,99871) 'SUM: ',srcid(idom), kurdat,
     &                     irec, grpid(igrp), isrc, srcid(isrc),
     &                     numcont, distdom, maxconc, o3conc,
     &                     sumo3moles, sumnoxmoles, bhoriz, bvert,
     &                     bvert3,
     &                     volsum, chi(irec,isrc,1), percentno2,
     &                     hrval(1), aveval(irec,igrp,1,1)
                     end if
99871                format(1x,a5,1x,a12,2x,i8.8,1x,i5,2x,a8,1x,i5,
     &                         2x,a12,2x,i5,2x,13(2x,e12.5))

                    end if
                 end if
               END IF
               IF (EVAL(ISRC)) THEN
C                 Check ARC centerline values for EVALFILE output   ---   CALL EVALCK
                  CALL EVALCK
               END IF

            END DO SOURCE_LOOP3

            IF (EVONLY) THEN
C --          Include BACKGROUND contributions in PVMRM debug files
C             in needed

              IF (IDXEV(IEVENT) .EQ. IGRP .AND.
     &                                GRP_BACK(idxev(ievent)) )THEN
C ---            This SRCGROUP includes BACKGROUND; Call EV_SUMBACK which
C                sums values for the current EVENT
                 CALL EV_SUMBACK
                 if (PVMRMDBG) then
                    IF ( (domConverted + ANO2_RATIO(IDOM)) .LE.
     &                   (sumConverted + AVE_NO2RATIO) ) THEN
                      write(PVMDBG,99872) 'DOM: ',srcid(idom), kurdat,
     &                    ievent, evname(ievent), evaper(ievent),
     &                    grpid(idxev(ievent)), 'BACKGROUND',
     &                    EV_BGCONC(IHOUR), grpave(IDXEV(IEVENT))

                    else
                      write(PVMDBG,99872) 'SUM: ', srcid(idom), kurdat,
     &                    ievent, evname(ievent), evaper(ievent),
     &                    grpid(idxev(ievent)), 'BACKGROUND',
     &                    EV_BGCONC(IHOUR), grpave(IDXEV(IEVENT))
                    end if
                 end if
99872            format(1x,a5,1x,a12,2x,i8.8,i6,4x,a10,i6,2x,a8,
     &                  8x,a10,161x,2(2x,e12.5))
              END IF

            ELSEIF (.NOT. EVONLY) THEN

               IF (GRP_BACK(IGRP)) THEN
C ---             This SRCGROUP includes BACKGROUND; Call SUMBACK_NO2 which
C                 sums values for the current receptor only, whereas sub SUMBACK
C                 sums values across the full receptor network
                  CALL SUMBACK_NO2
                  if (PVMRMDBG) then
                     IF ( (domConverted + ANO2_RATIO(IDOM)) .LE.
     &                    (sumConverted + AVE_NO2RATIO) ) THEN
                       write(PVMDBG,99873) 'DOM: ',srcid(idom), kurdat,
     &                   irec, grpid(igrp), 'BACKGROUND',
     &                   BGCONC, AVEVAL(IREC,IGRP,1,1)
                     else
                       write(PVMDBG,99873) 'SUM: ',srcid(idom), kurdat,
     &                   irec, grpid(igrp), 'BACKGROUND',
     &                   BGCONC, AVEVAL(IREC,IGRP,1,1)
                     end if
                  end if
99873             format(1x,a5,1x,a12,2x,i8.8,i6,2x,a8,8x,a10,165x,
     &                 2(2x,e12.5))
               END IF

            END IF

         END DO GROUP_LOOP

         ELSE IF (PSDCREDIT .AND. TRIM(SRCS2USE) .EQ. 'NAAQSRC') THEN
C           IC = increment consuming source
C           NB = nonretired baseline source
C           Use ABVAL to store combined results rather than HRVAL
            DO ISRC = 1, NUMSRC
               DO ITYP = 1, NUMTYP
                  IF( PSDSRCTYP(ISRC) .EQ. 'IC' .OR.
     &                PSDSRCTYP(ISRC) .EQ. 'NB' )THEN
                     ABVAL(IREC,ITYP) = ABVAL(IREC,ITYP) +
     &                                  CHI(IREC,ISRC,ITYP) * PercentNO2
                  END IF
               END DO
            END DO

C           Sum ABVAL to AVEVAL and ANNVAL Arrays  ---   CALL SUMVALPSD
            CALL SUMVALPSD(SRCS2USE)

         ELSE IF (PSDCREDIT .AND. TRIM(SRCS2USE) .EQ. 'ALLBASE') THEN
C           NB = nonretired baseline source
C           RB =    retired baseline source
C           Use BCVAL to store combined results rather than HRVAL
            DO ISRC = 1, NUMSRC
               DO ITYP = 1, NUMTYP
                  IF( PSDSRCTYP(ISRC) .EQ. 'NB' .OR.
     &                PSDSRCTYP(ISRC) .EQ. 'RB' )THEN
                     BCVAL(IREC,ITYP) = BCVAL(IREC,ITYP) +
     &                                  CHI(IREC,ISRC,ITYP) * PercentNO2
                  END IF
               END DO
            END DO

C           Sum ABVAL-BCVAL to AVEVAL and ANNVAL Arrays  ---   CALL SUMVALPSD
            CALL SUMVALPSD(SRCS2USE)

         END IF

C        Initialize __VAL arrays (1:NUMTYP)
         HRVAL   = 0.0D0
         AERVAL  = 0.0D0

      END DO RECEPTOR_LOOP
C     End Receptor LOOP

      RETURN
      END

      SUBROUTINE MAJOR_CONT(MAXCONC,IDOM)
C***********************************************************************
C             MAJOR_CONT Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Identify major contributing souces for PVMRM option,
C                 based on source impacts at the receptor of at least
C                 50% of the dominant source impact.  Also calculates
C                 the lateral and vertical range of the plumes from the
C                 major contributing sources.
C
C        PROGRAMMER: James Paumier, MACTEC FPI
C
C        DATE:    September 30, 2006
C
C        MODIFIED:   Include calls to SETSRC for all sources types
C                    to correct initialization problems with PVMRM.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
C
C        MODIFICATIONS:
C           Moved from SUBROUTINE PVMRM_CALC to be able to apply
C           emission credit calculations more effectively
C
C        INPUTS:
C
C
C        OUTPUTS:
C
C
C        CALLED FROM:   PVMRM_CALC
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C --- Define threshold for major contributing sources,
C     based on sources which contribute at least 0.5 times
C     the contribution from the dominant source, for each
C     receptor
      DOUBLE PRECISION, PARAMETER :: MAJCONT_THRESH = 0.5D0

      DOUBLE PRECISION :: MAXCONC, WIDTH, LENGTH, XMINR, XMAXR
      DOUBLE PRECISION :: XDEP
C Unused: DOUBLE PRECISION :: XAREA

      INTEGER          :: IDOM

C     Variable Initializations
      MODNAM = 'MAJOR_CONT'

      IF (CHI(IREC,ISRC,1) .GE. MAJCONT_THRESH*MAXCONC) THEN
C ---    This is a major contributing source
         IF (SRCTYP(ISRC)(1:4) .EQ. 'AREA' .OR.
     &            SRCTYP(ISRC) .EQ. 'LINE') THEN
            CALL SETSRC
C           Calls to ARDIST modified to include XMINR, minimum downwind distance
C           from the source to the receptor
            IF (EVONLY) THEN
               CALL ARDIST(IEVENT,XDEP,WIDTH,LENGTH,XMINR,XMAXR)
            ELSE
               CALL ARDIST(IREC,XDEP,WIDTH,LENGTH,XMINR,XMAXR)
            END IF

C ---       Check for receptor > MAXDIST from source
            IF( DISTR .GT. MAXDIST )THEN
               RETURN
            ELSE
               NUMCONT = NUMCONT + 1
C              Store max and min values of crosswind and downwind distances
               CWMAX = MAX( CWMAX, Y )
               CWMIN = MIN( CWMIN, Y )
               DWMAX = MAX( DWMAX, X )
               DWMIN = MIN( DWMIN, X )
C              Apply lower limit of 1.0 on DWMIN and DWMAX for PVMRM option
               IF( DWMAX .LT. 1.0D0 ) DWMAX = 1.0D0
               IF( DWMIN .LT. 1.0D0 ) DWMIN = 1.0D0
            ENDIF
         ELSE IF (SRCTYP(ISRC) .EQ. 'OPENPIT') THEN
            CALL SETSRC
C*          Determine the AlongWind Length of the OPENPIT Source  ---   CALL LWIND
            CALL LWIND

C*          Calculate the Relative Depth of the OPENPIT Source    ---   CALL PDEPTH
            CALL PDEPTH

C*          Calculate the Fractional Size of the
C*          Effective Pit Area (PITFRA)                           ---   CALL PTFRAC
            CALL PTFRAC

C*          Determine the Coordinates of the Effective Pit Area
C*          in Wind Direction Coordinate System                   ---   CALL PITEFF
            CALL PITEFF

            IF (EVONLY) THEN
               CALL ARDIST(IEVENT,XDEP,WIDTH,LENGTH,XMINR,XMAXR)
            ELSE
               CALL ARDIST(IREC,XDEP,WIDTH,LENGTH,XMINR,XMAXR)
            END IF

            IF( DISTR .GT. MAXDIST )THEN
               RETURN
            ELSE
               NUMCONT = NUMCONT + 1
C              Store max and min values of crosswind and downwind distances
C ---          Use distance from center of source for PVMRM option
               CWMAX = MAX( CWMAX, Y )
               CWMIN = MIN( CWMIN, Y )
               DWMAX = MAX( DWMAX, X )
               DWMIN = MIN( DWMIN, X )
C              Apply lower limit of 1.0 on DWMIN and DWMAX for PVMRM option
               IF( DWMAX .LT. 1.0D0 ) DWMAX = 1.0D0
               IF( DWMIN .LT. 1.0D0 ) DWMIN = 1.0D0
            ENDIF
         ELSE
C ---       POINT or VOLUME Source
            CALL SETSRC
            IF (EVONLY) THEN
               CALL XYDIST(IEVENT)
            ELSE
               CALL XYDIST(IREC)
            END IF

            IF( DISTR .GT. MAXDIST )THEN
               RETURN

            ELSE
               NUMCONT = NUMCONT + 1
C              Store max and min values of crosswind and downwind distances
               CWMAX = MAX( CWMAX, Y )
               CWMIN = MIN( CWMIN, Y )
               DWMAX = MAX( DWMAX, X )
               DWMIN = MIN( DWMIN, X )
C              Apply lower limit of 1.0 on DWMIN and DWMAX for PVMRM option
               IF( DWMAX .LT. 1.0D0 ) DWMAX = 1.0D0
               IF( DWMIN .LT. 1.0D0 ) DWMIN = 1.0D0
               IF (PPFACT(IREC,ISRC) .GT. 0.0D0) THEN
C                 Include lateral extent of penetrated plume
                  CWMAX3 = MAX( CWMAX3, Y )
                  CWMIN3 = MIN( CWMIN3, Y )
                  DWMAX3 = MAX( DWMAX3, X )
                  DWMIN3 = MIN( DWMIN3, X )
C                 Apply lower limit of 1.0 on DWMIN3 and DWMAX3 for PVMRM option
                  IF( DWMAX3 .LT. 1.0D0 ) DWMAX3 = 1.0D0
                  IF( DWMIN3 .LT. 1.0D0 ) DWMIN3 = 1.0D0
               ENDIF
            ENDIF

         END IF

C        Assign receptor height above stack base for dominant source
         IF (L_FLATSRC(IDOM)) THEN
            ZRT = ZFLAG
         ELSE
            ZRT = ZELEV - AZS(IDOM) + ZFLAG
         END IF

C        Check min/max plume height ranges for terrain-responding (HMNT/HMXT)
C        and horizontal (HMNH/HMXH) plumes
         IF (HECNTR(IREC,ISRC) .LT. HMNT) THEN
            HMNT = HECNTR(IREC,ISRC)
         ENDIF
         IF (HECNTR(IREC,ISRC) .GT. HMXT) THEN
            HMXT = HECNTR(IREC,ISRC)
         ENDIF

         IF ( (HECNTR(IREC,ISRC)-ZRT) .LT. HMNH) THEN
            HMNH = HECNTR(IREC,ISRC) - ZRT
         ENDIF
         IF ( (HECNTR(IREC,ISRC)-ZRT) .GT. HMXH) THEN
            HMXH = HECNTR(IREC,ISRC) - ZRT
         ENDIF

         IF (PPFACT(IREC,ISRC) .GT. 0.0D0) THEN
            IF (HECNTR3(IREC,ISRC).LT.HMNT3) THEN
               HMNT3 = HECNTR3(IREC,ISRC)
            ENDIF
            IF (HECNTR3(IREC,ISRC).GT.HMXT3) THEN
               HMXT3 = HECNTR3(IREC,ISRC)
            ENDIF

            IF ( (HECNTR3(IREC,ISRC)-ZRT) .LT. HMNH3) THEN
               HMNH3 = HECNTR3(IREC,ISRC) - ZRT
            ENDIF
            IF ( (HECNTR3(IREC,ISRC)-ZRT) .GT. HMXH3) THEN
               HMXH3 = HECNTR3(IREC,ISRC) - ZRT
            ENDIF

         END IF
      END IF

      RETURN
      END

      SUBROUTINE MOLES_NOX(IDOM)
C***********************************************************************
C             MOLES_NOX Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Calculates the moles of NOx in the combined plume
C
C        PROGRAMMER: James Paumier, MACTEC FPI
C
C        DATE:    September 30, 2006
C
C        MODIFIED:   Include calls to SETSRC for all sources types
C                    to correct initialization problems with PVMRM.
C                    Also include call subroutine EMFACT to apply
C                    emission factors, if appropriate, in order to
C                    use the EMISFACT-adjusted emission rates in
C                    the calculations of the moles of NOx.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
C
C        MODIFICATIONS:
C           Moved from SUBROUTINE PVMRM_CALC to be able to apply
C           emission credit calculations more effectively
C
C        INPUTS:
C
C
C        OUTPUTS:
C            QSUM
C            SUM_NO2RAT
C
C
C        CALLED FROM:   PVMRM_CALC
C***********************************************************************
C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      DOUBLE PRECISION, PARAMETER :: EPS = 1.0D-5
      DOUBLE PRECISION :: WIDTH, LENGTH, XMINR, XMAXR
      DOUBLE PRECISION :: CWDIST, DWDIST
      DOUBLE PRECISION :: QAREA, XDEP
      DOUBLE PRECISION :: QPTOT, FDOM
      INTEGER          :: IDOM

C     Variable Initializations
      MODNAM = 'MOLES_NOX'

      IF (SRCTYP(ISRC)(1:4) .EQ. 'AREA' .OR.
     &         SRCTYP(ISRC) .EQ. 'LINE') THEN
C ---    Determine NOX moles for AREA, AREACIRC, AREAPOLY,
C        and LINE sources
         CALL SETSRC
         CALL EMFACT(QS)
         IF (EVONLY) THEN
            CALL ARDIST(IEVENT,XDEP,WIDTH,LENGTH,XMINR,XMAXR)
         ELSE
            CALL ARDIST(IREC,XDEP,WIDTH,LENGTH,XMINR,XMAXR)
         END IF
C        Assign Y and X to CWDIST and DWDIST based on distance from
C        center of area source to receptor.  If center of area source
C        is within "box" of major contributing sources, then include
C        it's emissions in calculation of NOx moles.
         CWDIST = Y
         DWDIST = MAX( X, 1.0D0 )
      ELSE IF (SRCTYP(ISRC) .EQ. 'OPENPIT') THEN
C ---    Determine NOX moles for OPENPIT sources
         CALL SETSRC
C*       Determine the AlongWind Length of the OPENPIT Source  ---   CALL LWIND
         CALL LWIND

C*       Calculate the Relative Depth of the OPENPIT Source    ---   CALL PDEPTH
         CALL PDEPTH

C*       Calculate the Fractional Size of the
C*       Effective Pit Area (PITFRA)                           ---   CALL PTFRAC
         CALL PTFRAC

C*       Determine the Coordinates of the Effective Pit Area
C*       in Wind Direction Coordinate System                   ---   CALL PITEFF
         CALL PITEFF

C*       Calculate the adusted Emission Rate per unit area (QEFF) for the
C*       Effective Pit Area (PITFRA)                           ---   CALL PITEMI
C*       First assign source emission rate to QPTOT to get adjusted
C*       emission rate per unit area of effective source
         QPTOT = QS
         CALL PITEMI(QPTOT)
         CALL EMFACT(QEFF)

         IF (EVONLY) THEN
            CALL ARDIST(IEVENT,XDEP,WIDTH,LENGTH,XMINR,XMAXR)
         ELSE
            CALL ARDIST(IREC,XDEP,WIDTH,LENGTH,XMINR,XMAXR)
         END IF

C        Assign Y and X to CWDIST and DWDIST based on distance from
C        center of OPENPIT source to receptor.  If center of effective
C        source is within "box" of major contributing sources, then
C        include it's emissions in calculation of NOx moles.
         CWDIST = Y
         DWDIST = MAX( X, 1.0D0 )
      ELSE
C ---    Determine NOX moles for POINT and VOLUME sources
         CALL SETSRC
         CALL EMFACT(QS)
         IF (EVONLY) THEN
            CALL XYDIST(IEVENT)
         ELSE
            CALL XYDIST(IREC)
         END IF
C        Assign Y and X to CWDIST and DWDIST based on distance from
C        center of POINT or VOLUME source to receptor.  If center of
C        source is within "box" of major contributing sources, then
C        include it's emissions in calculation of NOx moles.
         CWDIST = Y
         DWDIST = MAX( X, 1.0D0 )
      END IF

C --- Get FDOM based on dominant source
      FDOM = FOPTS(IREC,IDOM)

C     Check for crosswind distance between MIN and MAX of projected width
C     and for downwind distance between MIN and MAX of downwind distances
C     of major contributing sources
      IF( (CWDIST .GE. CWMIN-EPS .AND. CWDIST .LE. CWMAX+EPS) .AND.
     &    (DWDIST .GE. DWMIN-EPS .AND. DWDIST .LE. DWMAX+EPS) )THEN
C           Source is located within the horizontal "box" of major contributing sources;
C           Also check min/max plume height ranges for terrain-responding (HMNT/HMXT)
C           and horizontal (HMNH/HMXH) plumes
            IF( HECNTR(IREC,ISRC) .GE. HMNT-EPS .AND.
     &          HECNTR(IREC,ISRC) .LE. HMXT+EPS )THEN
             IF (SRCTYP(ISRC).EQ.'AREA' .OR.
     &           SRCTYP(ISRC).EQ.'AREAPOLY' .OR.
     &           SRCTYP(ISRC).EQ.'LINE') THEN
C ---           Add contribution from AREA, AREAPOLY, and LINE sources
C ---           Note that XINIT and YINIT are "effective" dimensions for
C               AREAPOLY sources that are used to define the area of the
C               source.
                QAREA = QTK*XINIT*YINIT
                QSUM  = QSUM + QAREA*(1.0D0-FDOM)
                SUM_NO2RAT = SUM_NO2RAT + ANO2_RATIO(ISRC)*QAREA*
     &                                               (1.0D0-FDOM)
             ELSE IF (SRCTYP(ISRC) .EQ. 'AREACIRC') THEN
C ---           Add contribution from AREACIRC sources
                QAREA = QTK*PI*RADIUS(ISRC)*RADIUS(ISRC)
                QSUM  = QSUM + QAREA*(1.0D0-FDOM)
                SUM_NO2RAT = SUM_NO2RAT + ANO2_RATIO(ISRC)*QAREA*
     &                                               (1.0D0-FDOM)
             ELSE IF (SRCTYP(ISRC).EQ.'OPENPIT') THEN
C ---           Add contribution from OPENPIT sources
                QAREA = QEFF*XINIT*YINIT
                QSUM  = QSUM + QAREA*(1.0D0-FDOM)
                SUM_NO2RAT = SUM_NO2RAT + ANO2_RATIO(ISRC)*QAREA*
     &                                               (1.0D0-FDOM)
             ELSE
C ---           Add contribution from POINT and VOLUME sources
                QSUM = QSUM + QTK*(1.0D0-PPFACT(IREC,ISRC))*(1.0D0-FDOM)
                SUM_NO2RAT = SUM_NO2RAT + ANO2_RATIO(ISRC)*QTK*
     &                                 (1.0D0-PPFACT(IREC,ISRC))*
     &                                                      (1.0D0-FDOM)
             END IF
            ENDIF

            IF( (HECNTR(IREC,ISRC)-ZRT) .GE. HMNH-EPS .AND.
     &          (HECNTR(IREC,ISRC)-ZRT) .LE. HMXH+EPS )THEN
             IF (SRCTYP(ISRC).EQ.'AREA' .OR.
     &           SRCTYP(ISRC).EQ.'AREAPOLY' .OR.
     &           SRCTYP(ISRC).EQ.'LINE') THEN
C ---           Add contribution from AREA, AREAPOLY, and LINE sources
C ---           Note that XINIT and YINIT are "effective" dimensions for
C               AREAPOLY sources that are used to define the area of the
C               source.
                QAREA = QTK*XINIT*YINIT
                QSUM  = QSUM + QAREA*FDOM
                SUM_NO2RAT = SUM_NO2RAT + ANO2_RATIO(ISRC)*QAREA*FDOM
             ELSE IF (SRCTYP(ISRC) .EQ. 'AREACIRC') THEN
C ---           Add contribution from AREACIRC sources
                QAREA = QTK*PI*RADIUS(ISRC)*RADIUS(ISRC)
                QSUM  = QSUM + QAREA*FDOM
                SUM_NO2RAT = SUM_NO2RAT + ANO2_RATIO(ISRC)*QAREA*FDOM
             ELSE IF (SRCTYP(ISRC).EQ.'OPENPIT') THEN
C ---           Add contribution from OPENPIT sources
                QAREA = QEFF*XINIT*YINIT
                QSUM  = QSUM + QAREA*FDOM
                SUM_NO2RAT = SUM_NO2RAT + ANO2_RATIO(ISRC)*QAREA*FDOM
             ELSE
C ---           Add contribution from POINT and VOLUME sources
                QSUM = QSUM + QTK*(1.0D0-PPFACT(IREC,ISRC))*FDOM
                SUM_NO2RAT = SUM_NO2RAT + ANO2_RATIO(ISRC)*QTK*
     &                                 (1.0D0-PPFACT(IREC,ISRC))*FDOM
             END IF
            ENDIF
      ENDIF

      IF( (CWDIST .GE. CWMIN3-EPS .AND. CWDIST .LE. CWMAX3+EPS) .AND.
     &    (DWDIST .GE. DWMIN3-EPS .AND. DWDIST .LE. DWMAX3+EPS) )THEN
            IF( HECNTR3(IREC,ISRC) .GE. HMNT3-EPS .AND.
     &          HECNTR3(IREC,ISRC) .LE. HMXT3+EPS )THEN
              IF( SRCTYP(ISRC)(1:5) .EQ. 'POINT' )THEN
C ---            Add contribution from POINT and VOLUME sources
                 QSUM3 = QSUM3 + QTK*PPFACT(IREC,ISRC)*(1.0D0-FDOM)
                 SUM3_NO2RAT = SUM3_NO2RAT + ANO2_RATIO(ISRC)*QTK*
     &                               PPFACT(IREC,ISRC)*(1.0D0-FDOM)
              END IF
            ENDIF

            IF( (HECNTR3(IREC,ISRC)-ZRT) .GE. HMNH3-EPS .AND.
     &          (HECNTR3(IREC,ISRC)-ZRT) .LE. HMXH3+EPS )THEN
              IF( SRCTYP(ISRC)(1:5) .EQ. 'POINT' )THEN
C ---            Add contribution from POINT and VOLUME sources
                 QSUM3 = QSUM3 + QTK*PPFACT(IREC,ISRC)*FDOM
                 SUM3_NO2RAT = SUM3_NO2RAT + ANO2_RATIO(ISRC)*QTK*
     &                               PPFACT(IREC,ISRC)*FDOM
              END IF
            ENDIF

      END IF

      RETURN
      END

      SUBROUTINE PLUME_VOL(XARG,IRDX,ISDX,BVARG,BVARG3,
     &                                    BHARG,BHARG3,VOLOUT)
C***********************************************************************
C             PLUME_VOL Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Calculates plume volume for PVMRM option
C
C        PROGRAMMER: Roger W. Brode, PES, Inc.
C
C        DATE:    May 6, 2002
C
C        MODIFICATIONS:
C
C               Modified XTABLE array of distances to use a more
C               logical progression of distance intervals and to
C               reduce the number of distances used.
C               R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
C
C               Added call to PENFCT to calculate plume penetration
C               factor.
C               R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
C
C               Remove calls to VDP and SCAVRAT.
C               R. Brode, MACTEC (f/k/a PES), Inc. - 08/02/05
C
C        INPUTS:
C
C
C        OUTPUTS:
C
C
C        CALLED FROM:   PVMRM_CALC
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      INTEGER, PARAMETER :: NTAB = 59
C---  Declare parameters for the number of sigma-z's to define the volume of
C     of the plume for stable (NSUMZS) and unstable (NSUMZU) conditions,
C     and for the minimum values of sigma-r for stable (SRMINS) and unstable
C     (SRMINU), to distinguish between total dispersion coefficents (used for
C     stable conditions) and relative dispersion (used for unstable conditions).

C --- Declare NSUBZS (number of sigma's to define the plume volume) and SRMINS (minimum value of sigma-r)
C     for STABLE conditions based on use of "total" dispersion
      DOUBLE PRECISION, PARAMETER :: NSUBZS = 1.282D0, SRMINS = 15.0D0         ! NSUBZS = approx 80% of plume

C --- Declare NSUBZU (number of sigma's to define the plume volume) and
C     SRMINU (minimum value of sigma-r) for UNSTABLE conditions under the new PVMRM option:
      DOUBLE PRECISION, PARAMETER :: NSUBZU = 2.58D0,  SRMINU =  9.0D0         ! NSUBZU = approx 99% of plume

      DOUBLE PRECISION ::  NSUBZ, SRMIN, SRMIN3
C     JAT 7/22/21 DO65 FRAN3 NOT USED
C      DOUBLE PRECISION ::  FRAN, FRAN3
      DOUBLE PRECISION ::  FRAN
      DOUBLE PRECISION ::  XARG, BVARG, BVARG3, BHARG, BHARG3,
     &                     VERT, VERT3, VOLOUT, SUM, SUM3
      DOUBLE PRECISION ::  XTABLE(NTAB), SR(NTAB), SR3(NTAB), SIGR,
C     JAT 7/22/21 DO65 SZTMP NOT USED
C     &                     SIGR3, SZTMP
     &                     SIGR3
      DOUBLE PRECISION ::  XTAB, XINP, DELTAX, BVTMP, SIGRZTMP
      INTEGER ::  I, ISDX, IRDX
      INTEGER ::  KITER, NDXZPL
      DOUBLE PRECISION :: HSPRIM, ZPLM, DHFOLD, SVPM, UPM, TGPM, PTPM,
     &                    PTP !, SVPM2
      DOUBLE PRECISION :: VSEQ

C --- Variable Initializations:
C     Distance table for plume volume calculation, XTABLE
      DATA XTABLE/
     &   10.D0,20.D0,30.D0,40.D0,50.D0,60.D0,70.D0,80.D0,90.D0,100.D0,
     &   120.D0,140.D0,160.D0,180.D0,200.D0,
     &   250.D0,300.D0,350.D0,400.D0,450.D0,500.D0,
     &   600.D0,700.D0,800.D0,900.D0,1000.D0,
     &   1200.D0,1400.D0,1600.D0,1800.D0,2000.D0,
     &   2500.D0,3000.D0,3500.D0,4000.D0,4500.D0,5000.D0,
     &   6000.D0,7000.D0,8000.D0,9000.D0,10000.D0,
     &   12000.D0,14000.D0,16000.D0,18000.D0,20000.D0,
     &   25000.D0,30000.D0,35000.D0,40000.D0,45000.D0,50000.D0,
     &   60000.D0,70000.D0,80000.D0,90000.D0,100000.D0,
     &   500000.D0/

      DATA SR/NTAB*0.0D0/, SR3/NTAB*0.0D0/

      MODNAM = 'PLUME_VOL'

C     Assign source index to global variable
      ISRC   = ISDX
      IREC   = IRDX
      SUM    = 0.0D0
      SUM3   = 0.0D0
C --- Initialize SR and SR3 arrays
      SR(:)  = 0.0D0
      SR3(:) = 0.0D0
      SIGR   = 0.0D0
      SIGR3  = 0.0D0
C     JAT 7/22/21 DO65 SZTMP NOT USED
C      SZTMP  = 0.0D0

      VERT   = 0.0D0
      VERT3  = 0.0D0

      FRAN   = 0.0D0
C     JAT 7/22/21 DO65 FRAN3 NOT USED
C      FRAN3  = 0.0D0

C --- PVMRM option uses "total" dispersion coefficients for
C     STABLE conditions, including injected plumes (UNSTAB .and. HS < ZI),
C     and uses "relative" dispersion coefficients for UNSTABLE conditions.
C     Assign SRMIN and NSUBZ based on stability:
C     Use HS based on ISDX
      IF (STABLE .OR. (UNSTAB .AND. (AHS(ISDX) .GE. ZI))) THEN
C ---    Assign SRMIN and NSUBZ for STABLE or UNSTAB and HS .ge. ZI
         SRMIN = SRMINS
         NSUBZ = NSUBZS
      ELSE
C ---    Assign SRMIN and NSUBZ for UNSTAB and HS .lt. ZI
         SRMIN = SRMINU
         NSUBZ = NSUBZU
      END IF

      if( PVMRMDBG .AND. DEBUG .AND. KURDAT .GT. 0 )then
C ---    PVMRM and MODEL DEBUG options have been selected; include the
C        RELDISP Calcs in the MODEL DEBUG file
         write(RDISPUNT,*)
         write(RDISPUNT,*) 'RELDISP Calcs: '
         if( EVONLY )then
            write(RDISPUNT,*) ' DATE: ' ,KURDAT,'  IEVT: ',IEVENT
         else
            write(RDISPUNT,*) ' DATE: ' ,KURDAT,'  IREC: ',IREC
         endif
         write(RDISPUNT,*)
     &    '  DOM SRCID   IDX      DIST       SR(IDX)      SR3(IDX)',
     &    '      SIGR        SRMIN'
      endif

C     Set Mixing Height and Profiles for Urban Option if Needed
      IF (URBSRC(ISRC) .EQ. 'Y') THEN
C        Find Urban Area Index for This Source
         DO I = 1, NUMURB
            IF (IURBGRP(ISRC,I) .EQ. 1) THEN
               IURB = I
               EXIT
            END IF
         END DO
         IF (STABLE .OR. L_MorningTrans(IURB)) THEN
            URBSTAB = .TRUE.
            ZI = MAX( ZIURB(IURB), ZIMECH )
            GRIDSV = GRDSVU(1:MXGLVL,IURB)
            GRIDSW = GRDSWU(1:MXGLVL,IURB)
            GRIDTG = GRDTGU(1:MXGLVL,IURB)
            GRIDPT = GRDPTU(1:MXGLVL,IURB)
            OBULEN = URBOBULEN(IURB)
            USTAR  = URBUSTR(IURB)
         ELSE
            URBSTAB = .FALSE.
            ZI = ZIRUR
            GRIDSV = GRDSVR
            GRIDSW = GRDSWR
            GRIDTG = GRDTGR
            GRIDPT = GRDPTR
            OBULEN = RUROBULEN
            USTAR  = RURUSTR
         END IF
      ELSE IF (URBAN .AND. URBSRC(ISRC) .EQ. 'N') THEN
         URBSTAB = .FALSE.
         ZI = ZIRUR
         GRIDSV = GRDSVR
         GRIDSW = GRDSWR
         GRIDTG = GRDTGR
         GRIDPT = GRDPTR
         OBULEN = RUROBULEN
         USTAR  = RURUSTR
      ELSE
C ---    Rural
         URBSTAB = .FALSE.
      END IF

C     Set the Source Variables for This Source           ---   CALL SETSRC
      CALL SETSRC

C     Calculate the initial meteorological variables     ---   CALL METINI
      CALL METINI

      IF (SRCTYP(ISRC) .EQ. 'VOLUME' .OR.
     &    SRCTYP(ISRC) .EQ. 'LINE' .OR.
     &    SRCTYP(ISRC)(1:4) .EQ. 'AREA' .OR.
     &    SRCTYP(ISRC) .EQ. 'OPENPIT') THEN
         FB  = 0.0D0
         FM  = 0.0D0
         PPF = 0.0D0
         HSP = HS
         DHP  = 0.0D0
         DHP1 = 0.0D0
         DHP2 = 0.0D0
         DHP3 = 0.0D0
         DHCRIT = 0.0D0
         XFINAL = 0.0D0
         XMIXED = ZI * UAVG / SWAVG
         IF(XMIXED .LT. XFINAL) XMIXED = XFINAL
         ZMIDMX = 0.5D0 * ZI
C        Define temporary values of CENTER and SURFAC based on HS
C        to account for non-POINT sources
         CENTER = HECNTR(IREC,ISRC)
         IF( CENTER .LT. 0.1D0*ZI )THEN
            SURFAC = .TRUE.
         ELSE
            SURFAC = .FALSE.
         END IF

      ELSE IF (SRCTYP(ISRC)(1:5) .EQ. 'POINT') THEN
C        Calculate Buoyancy and Momentum Fluxes             ---   CALL FLUXES
         CALL FLUXES(VSEQ)

C        Set Wake and Building Type Switches                ---   CALL WAKFLG
C ---    NOTE:  WAKFLG sets building dimensions based on wind
C        direction at stack top.
C ---    WAKE is set to false for purposes of calculating plume volume,
C        since minimum volume is intended
         WAKE = .FALSE.

C        Define temporary values of CENTER and SURFAC based on HS
         CENTER = HECNTR(IREC,ISRC)
         IF( CENTER .LT. 0.1D0*ZI )THEN
            SURFAC = .TRUE.
         ELSE
            SURFAC = .FALSE.
         END IF

C ---    Apply HSP for POINTCAP and POINTHOR first to avoid NOSTD option
C        overriding POINTCAP
         IF (SRCTYP(ISRC) .EQ. 'POINTCAP') THEN
C           Apply stack-tip downwash for capped stacks with VS = 0.001m/s
            HSP = HSPRIM ( US, VSEQ, HS, DS )
         ELSE IF (SRCTYP(ISRC) .EQ. 'POINTHOR') THEN
C           Do not apply stack-tip downwash for horizontal releases
            HSP = HS
         ELSE IF( NOSTD )THEN
C           No stack-tip downwash, no adjustments necessary
            HSP = HS
         ELSE
C           Make adjustments for stack-tip downwash
            HSP = HSPRIM ( US, VS, HS, DS )
         END IF

C        Calculate Distance to Final Rise                   ---   CALL DISTF
         CALL DISTF

C        Calculate the plume penetration factor             ---   CALL PENFCT
         CALL PENFCT

         IF (STABLE .OR. (UNSTAB.AND.(HS.GE.ZI))) THEN
C           Use iterative approach to stable plume rise calculations
            KITER = 0
50          ZPLM = HSP + 0.5D0 * DHFAER
            DHFOLD = DHFAER

C----       Locate index below ZPLM

            CALL LOCATE(GRIDHT, 1, MXGLVL, ZPLM, NDXZPL)

C----       Get Wind speed at ZPLM; replace UP.  Also, replace TGP,
C           vertical potential temperature gradient, if stable.

            CALL GINTRP( GRIDHT(NDXZPL), GRIDSV(NDXZPL),
     &           GRIDHT(NDXZPL+1), GRIDSV(NDXZPL+1), ZPLM, SVPM )
            CALL GINTRP( GRIDHT(NDXZPL), GRIDWS(NDXZPL),
     &           GRIDHT(NDXZPL+1), GRIDWS(NDXZPL+1), ZPLM, UPM )

            SVPM = MAX( SVPM, SVMIN, SVUMIN*UPM )
            IF( L_VECTORWS )THEN
               UPM = DSQRT( UPM*UPM + 2.0D0*SVPM*SVPM )
            ENDIF
            UPM  = MAX( UPM, WSMIN )


CRWB        Use average of stack top and midpoint wind speeds.
            UP = 0.5D0 * (US + UPM)

            CALL GINTRP( GRIDHT(NDXZPL), GRIDTG(NDXZPL),
     &           GRIDHT(NDXZPL+1), GRIDTG(NDXZPL+1), ZPLM, TGPM )
            CALL GINTRP( GRIDHT(NDXZPL), GRIDPT(NDXZPL),
     &           GRIDHT(NDXZPL+1), GRIDPT(NDXZPL+1), ZPLM, PTPM )
CRWB        Use average of stack top and midpoint temperature gradients.
            TGP = 0.5D0 * (TGS + TGPM)
            PTP = 0.5D0 * (PTS + PTPM)
            BVF = DSQRT( G * TGP / PTP )
            IF(BVF .LT. 1.0D-10) BVF = 1.0D-10
            BVPRIM  = 0.7D0 * BVF

            CALL DISTF

            KITER = KITER + 1

C           Check for convergence
            IF(DABS((DHFOLD - DHFAER)/DHFAER) .LT. 0.01D0) GO TO 60

            IF(KITER .GE. 5) THEN
               DHFAER = 0.5D0 * (DHFAER + DHFOLD)
               GO TO 60
            ELSE
               GO TO 50
            END IF

60          CONTINUE

CRWB        After completing iteration, reset UP and TGP to stack top
CRWB        values for subsequent distance-dependent plume rise calcs.
            UP = US
            TGP = TGS
            PTP = PTS
            BVF = DSQRT( G * TGP / PTP )
            IF(BVF .LT. 1.0D-10) BVF = 1.0D-10
            BVPRIM  = 0.7D0 * BVF
         END IF

C        Initialize PRM_FSTREC Logical Switch for First Receptor of Loop;
         PRM_FSTREC = .TRUE.

         ZMIDMX = 0.5D0 * ZI

CRJP
CRJP     Calculate distance to uniformly mixed plume within the
CRJP     boundary layer (XMIXED) after Turner's Workbook (1970), page 7:
CRJP     distance is approximately (Zi * UAVG)/SWAVG, where UAVG
CRJP     and SWAVG are wind speed and sigma-w averaged over the depth
CRJP     between the ground and Zi (or the plume height, if higher in
CRJP     stable conditions); this height is denoted as 2 * ZMIDMX.
CRJP
CRJP     First, get refined estimate of final rise and distance to final
CRJP     rise if downwash conditions prevail.
CRJP
         XFINAL = XMAX
         DHCRIT = DHFAER
         XMIXED = ZI * UAVG / SWAVG
         IF (UNSTAB .AND. HS.LT.ZI) THEN
C           Check for XMIXED smaller than 1.25*XFINAL
            IF (XMIXED .LT. 1.25D0*XFINAL) THEN
               XFINAL = 0.8D0 * XMIXED
               CALL CBLPRD (XFINAL)
               DHCRIT = DHP1
            END IF
         END IF

      END IF

C     First build table of relative dispersion coefficients for
C     dominant source (source index = ISDX)
      DO I = 1, NTAB

         XTAB = XTABLE(I)

C ---    Assign XINP depending on whether XARG (src-rec dist) is
C        greater or less than current table value (XTAB)
         IF (XARG .GT. XTAB) THEN
            XINP = XTAB
            IF (I .GT. 1) THEN
              DELTAX = XTABLE(I) - XTABLE(I-1)
            ELSE
              DELTAX = XTABLE(1)
            END IF
         ELSE
            XINP = XARG
            IF (I .GT. 1) THEN
              DELTAX = XARG - XTABLE(I-1)
            ELSE
              DELTAX = XARG
            END IF
         END IF

C        Receptor distance is > current table value, XTAB; calculate values at XTAB
C        Define plume centroid height (CENTER) for use in
C        inhomogeneity calculations
         CALL CENTROID ( XINP )

         IF (SRCTYP(ISRC)(1:5) .EQ. 'POINT') THEN
C           Calculate the plume rise                  ---   CALL DELTAH
            CALL DELTAH ( XINP )
         ELSE
C ---       Assign 0.0 to DHP plume rise variables for non-POINT sources
            DHP  = 0.0D0
            DHP1 = 0.0D0
            DHP2 = 0.0D0
            DHP3 = 0.0D0
         END IF

C        If the atmosphere is unstable and the stack
C        top is below the mixing height, calculate
C        the CBL PDF coefficients                     ---   CALL PDF
         IF( UNSTAB  .AND.  (HS .LT. ZI) ) THEN
            CALL PDF
         END IF

C        Determine Effective Plume Height             ---   CALL HEFF
         CALL HEFF ( XINP )

C        Compute effective parameters using an
C        average through plume rise layer
         CALL IBLVAL ( XINP )

C        Call PDF & HEFF again for final CBL plume heights
         IF (UNSTAB .AND. (HS.LT.ZI) ) THEN
            CALL PDF
            CALL HEFF ( XINP )
         END IF

         IF (SRCTYP(ISRC)(1:5) .EQ. 'POINT' .AND. .NOT.NOBID) THEN
C           Call BID to get buoyancy-induced dispersion terms
            CALL BID
         ELSE
C           Set BID Terms to 0.0
            IF( STABLE  .OR.  (UNSTAB .AND. (HS .GE. ZI) ) )THEN
               SYB = 0.0D0
               SZB = 0.0D0

            ELSE IF( UNSTAB )THEN
               SYB  = 0.0D0
               SZBD = 0.0D0
               SZBN = 0.0D0
               SYB3 = 0.0D0
               SZB3 = 0.0D0

            END IF
         END IF

C ---    Calculate "total" dispersion coefficients for STABLE conditions
C        and UNSTAB conditions with HS .GE. ZI and use "relative"
C        dispersion coefficients for UNSTAB conditions with HS .LT. ZI
C
         IF (STABLE .OR. (UNSTAB  .AND.  (HS .GE. ZI)) )THEN
           IF (SRCTYP(ISRC)(1:5) .EQ. 'POINT') THEN
C ---         Call PDIS to get SY and SZ for POINT sources
              CALL PDIS( XINP )
C ---         Assign geometric mean of SY and SZ for SR
              SR(I) = DSQRT( SZ*SY )

C ---         Set SR3 = 0.0 for "stable" plume
              SR3(I) = 0.0D0
           ELSE IF (SRCTYP(ISRC) .EQ. 'VOLUME') THEN
C ---         Call VDIS to get SY and SZ for VOLUME sources
              CALL VDIS( XINP )
C ---         Assign geometric mean of SY and SZ for SR
              SR(I) = DSQRT( SY*SZ )

C ---         Assign SR3 = 0.0 for non-POINT sources
              SR3(I) = 0.0D0
           ELSE IF (SRCTYP(ISRC)(1:4) .EQ. 'AREA' .OR.
     &              SRCTYP(ISRC)      .EQ. 'OPENPIT') THEN
C ---         Call ADISY and ADISZ to get SY and SZ for AREA and OPENPIT sources
              CALL ADISY( XINP )
              CALL ADISZ( XINP )
C ---         Assign geometric mean of SY and SZ for SR
              SR(I) = DSQRT( SY*SZ )

C ---         Assign SR3 = 0.0 for non-POINT sources?
              SR3(I) = 0.0D0
           END IF

C          First calculate the "effective" sigma-y value that
C          replicates the centerline concentration with the effect
C          of horizontal meander, but based on a "coherent" plume
           CALL MEANDR( UEFF, SVEFF, FRAN )

C ---      Calculate effective sigma-y associated with non-DFAULT FASTALL option (EFFSIGY)
           SYEFF = 1.0D0/((FRAN/(SRT2PI*XINP)) + (1.0D0-FRAN)/SY)

C ---      Use the value of NSUBZ based on original Hanrahan value of 1.282 (NSUBZS),
C          adjusted by the ratio of SYEFF/SY to account for meander, or a value of 2.15,
C          whichever is lower
           NSUBZ = MIN( 2.15D0,  NSUBZS*(SYEFF/SY) )

C ---      Adjust SRMIN based on SYEFF/SY ratio for stable conditions
           SRMIN = SRMINS * (1.282D0/NSUBZ)

           IF (I .GT. 1) THEN
              SIGR = MAX( SRMIN, 0.5D0 * (SR(I) + SR(I-1)) )
           ELSE
              SIGR = MAX( SRMIN, 0.5D0 * SR(I) )
           END IF

         ELSE

C ---      Use relative dispersion coefficients for UNSTABLE conditions with HS < ZI;
C          Determine Relative Dispersion Parameters     ---   CALL RELDISP
           CALL RELDISP ( XINP, SR(I), SR3(I) )

           IF (I .GT. 1) THEN
              SIGR = MAX( SRMIN, 0.5D0 * (SR(I) + SR(I-1)) )
           ELSE
              SIGR = MAX( SRMIN, 0.5D0 * SR(I) )
           END IF

C ---      Use PPF-weighted average of SRMINS and SRMINU for SRMIN3
C          penetrated plumes
           IF( PPFACT(IREC,ISRC) .GT. 0.0D0 )THEN
              PPF = PPFACT(IREC,ISRC)
              SRMIN3 = PPF*SRMINS + (1.0D0-PPF)*SRMINU
           ENDIF

           IF (PPFACT(IREC,ISRC) .GT. 0.0D0) THEN
              IF (I .GT. 1) THEN
                 SIGR3 = MAX( SRMIN3, (0.5D0 * (SR3(I) + SR3(I-1))) )
              ELSE
                 SIGR3 = MAX( SRMIN3, (0.5D0 * SR3(I)) )
              END IF
           ELSE
              SIGR3 = 0.0D0
           END IF

         END IF

         if( PVMRMDBG .AND. DEBUG )then
C ---       PVMRM and MODEL DEBUG options have been selected; include the
C           RELDISP Calcs in the MODEL DEBUG file
            write(RDISPUNT,111) SRCID(ISRC), I, XINP, SR(I), SR3(I),
     &                                                     SIGR, SRMIN
111        format(2x,a12,i4,5(2x,E11.5))
         endif

C        Calculate vertical dimension (VERT) taking into account ZI limit
         VERT = BVARG + 2.0D0*NSUBZ*SIGR
         IF (UNSTAB .AND. VERT .GT. ZI) THEN
            VERT  = ZI
            BVTMP = MAX( 0.0D0, ZI-2.0D0*NSUBZ*SIGR )
            IF (2.0D0*NSUBZ*SIGR .GT. ZI) THEN
               SIGRZTMP = ZI/(2.0D0*NSUBZ)
            ELSE
               SIGRZTMP = SIGR
            END IF
         ELSE
            BVTMP = BVARG
            SIGRZTMP = SIGR
         END IF

C        Plume volume calculation based on rectangle with rounded corners
C        First component is for major contributing plume only
         SUM = SUM + (PI*(NSUBZ*SIGR)*(NSUBZ*SIGRZTMP) +
     &                VERT*BHARG +
     &                2.0D0*NSUBZ*SIGR*BVTMP) * DELTAX

         IF (UNSTAB .AND. PPFACT(IREC,ISRC) .GT. 0.0D0) THEN
C ---       Compute SUM3 for penetrated source plume if needed

C ---       Use a PPF-weighted average of NSUBZS and NSUBZU for penetrated plumes
            NSUBZ = PPF*NSUBZS + (1.0D0-PPF)*NSUBZU

            VERT3 = BVARG3 + 2.0D0*NSUBZ*SIGR3

            SUM3  = SUM3 + (PI*(NSUBZ*SIGR3)*(NSUBZ*SIGR3) +
     &                      VERT3*BHARG3 +
     &                      2.0D0*NSUBZ*SIGR3*BVARG3) * DELTAX

         ELSE

            SUM3 = 0.0D0
         END IF

C ---    Check for XARG .LE. XTAB, then volume calc is done; exit DO loop
         IF (XARG .LE. XTAB) EXIT

      END DO    ! End of loop on distance table

C     Combine volume for "direct" plume volume and penetrated plume volume
      VOLOUT = SUM*(1.0D0-PPFACT(IREC,ISRC)) + SUM3*PPFACT(IREC,ISRC)

      RETURN
      END

      SUBROUTINE RELDISP(XARG,SROUT,SROUT3)
C***********************************************************************
C             RELDISP Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Calculates relative dispersion coefficients for use
C                 in calculating plume volume for the PVMRM option
C
C        PROGRAMMER: Roger W. Brode, PES, Inc.
C
C        DATE:    May 14, 2002
C
C        MODIFICATIONS:
C
C               Modified treatment of virtual source term, VSIGR,
C               to account for cases when SYINIT or SZINIT may be
C               zero, such as AREA sources.
C               R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
C
C        INPUTS:
C
C
C        OUTPUTS:
C
C
C        CALLED FROM:   PLUME_VOL
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      DOUBLE PRECISION, PARAMETER :: A1 = 0.57D0, A2 = 0.62D0*A1,
     &                               AR1 = 0.46D0
      DOUBLE PRECISION :: XARG, SROUT, SROUT3, SRAMB, SRAMB3, TLR, VSIGR

C     Variable Initializations
      MODNAM = 'RELDISP'

C --- Use square root of the product of lateral and vertical initial sigmas
C     to define the effective virtual source initial sigma, VSIGR;
C     however, need to check for one or the other being zero, e.g., for
C     area source types, szinit may be non-zero but syinit is zero.
      IF (SZINIT .GE. 1.0D-8) THEN
         IF (SYINIT .GE. 1.0D-8) THEN
            VSIGR = DSQRT(SYINIT*SZINIT)
         ELSE
            VSIGR = SZINIT
         END IF
      ELSE IF (SZINIT .LT. 1.0D-8) THEN
         IF (SYINIT .GE. 1.0D-8) THEN
            VSIGR = SYINIT
         ELSE
            VSIGR = 0.0D0
         END IF
      ELSE
         VSIGR = 0.0D0
      END IF

      IF( STABLE .OR. (UNSTAB .AND. (HS .GE. ZI)) )THEN
C        The atmosphere is stable or the release is above the CBL mixing ht.
         TLR = AR1 * ZI/SWEFF
         SRAMB = (A1 * DSQRT(EPSEFF) * (XARG/UEFF)**1.5D0)/
     &           (1.0D0 + A2 * XARG/(UEFF*TLR))
         SROUT = (SRAMB**3 + SZB**3 + VSIGR**3)**THIRD

C ---    Assign SROUT3 = 0.0 for stable or injected plume
         SROUT3 = 0.0D0

      ELSEIF( UNSTAB )then
C        The atmosphere is unstable and the release is below the CBL mixing ht.
         TLR = AR1 * ZI/SWEFFD
         SRAMB = (A1 * DSQRT(EPSEFFD) * (XARG/UEFFD)**1.5D0)/
     &           (1.0D0 + A2 * XARG/(UEFFD*TLR))
         SROUT = (SRAMB**3 + SZBD**3 + VSIGR**3)**THIRD

C        Calculate relative dispersion for a penetrated plume, SROUT3
         IF( PPFACT(IREC,ISRC) .GT. 0.0D0 )THEN
            TLR = AR1 * ZI/SWEFF3
            SRAMB3 = (A1 * DSQRT(EPSEFF3) * (XARG/UEFF3)**1.5D0)/
     &               (1.0D0 + A2 * XARG/(UEFF3*TLR))
            SROUT3 = (SRAMB3**3 + SZB3**3 + VSIGR**3)**THIRD

         ELSE
            SROUT3 = 0.0D0
         END IF

      END IF

      RETURN
      END

      SUBROUTINE AERMAX(SRCS2USE,MAXARG,DOMARG)
C***********************************************************************
C             AERMAX Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: To populate the MAXCHI and DOMIDX arrays
C
C        PROGRAMMER: J Paumier, MACTEC
C
C        DATE:    September 30, 2006
C
C        INPUTS:
C
C
C        OUTPUTS:
C
C
C        CALLED FROM:   PVMRM_CALC
C***********************************************************************

      USE MAIN1
      IMPLICIT NONE

      DOUBLE PRECISION :: MAXARG(NUMREC,NUMTYP)
      INTEGER :: DOMARG(NUMREC,NUMTYP)
      INTEGER :: I, J, K
      CHARACTER SRCS2USE*7
      CHARACTER MODNAM*12

      MODNAM = 'AERMAX'

C     Initialize MAXARG(NUMREC,NUMTYP) and DOMARG(NUMREC,NUMYTP) arrays
      MAXARG = 0.0D0
      DOMARG = 0

      IF (TRIM(SRCS2USE) .EQ. 'NAAQSRC') THEN
C        Emission credit run: Increment Consumption+Nonretired Baseline=NAAQS
         DO I = 1, NUMTYP
            DO J = 1, NUMREC
               DO K = 1, NUMSRC
                  IF (PSDSRCTYP(K) .EQ. 'IC' .OR.
     &                PSDSRCTYP(K) .EQ. 'NB') THEN
                     IF(CHI(J,K,I) .GT. MAXARG(J,I) )THEN
                        MAXARG(J,I) = CHI(J,K,I)
                        DOMARG(J,I) = K
                     END IF
                  END IF
               END DO
            END DO
         END DO

      ELSE IF (TRIM(SRCS2USE) .EQ. 'ALLBASE') THEN
C        Emission credit run: Nonretired Baseline and Retired Baseline
         DO I = 1, NUMTYP
            DO J = 1, NUMREC
               DO K = 1, NUMSRC
                  IF (PSDSRCTYP(K) .EQ. 'NB' .OR.
     &                PSDSRCTYP(K) .EQ. 'RB') THEN
                     IF(CHI(J,K,I) .GT. MAXARG(J,I) )THEN
                        MAXARG(J,I) = CHI(J,K,I)
                        DOMARG(J,I) = K
                     END IF
                  END IF
               END DO
            END DO
         END DO

      END IF

      RETURN
      END

CCRT   D063
C***** BEGIN OCD PLATFORM ADDITIONS ************************************

      SUBROUTINE PLAT_DOWNWASH(XD,HB,W,HE,SIGZ0,SIGY0)
C***********************************************************************
C             PLAT_DOWNWASH Subroutine
C
C        PURPOSE: COMPUTE PLATFORM DOWNWASH EFFECTS BASED ON PETERSON, 1986
C
C        PROGRAMMER: Clint Tillerson AMEC 2012, copied from OCD.FOR/DOWNWASH
C
C        DATE:    2012
C
C        MODIFICATIONS:
C                 Changed name of subroutine to have PLAT_ to denote it is
C                 used for PLATFORM sources.  Reformatted to AERMOD style.
C                 Added debug statements for PLATFORM debug file.
C                 Michelle G. Snyder, WOOD 8/5/2021.
C
C        INPUTS:  XD - DOWNWIND DISTANCE (meters)
C                 HB - BUILDING HEIGHT ABOVE WATER SURFACE (meters)
C                 W  - BUILDING WIDTH (meters)
C                 HE - EFFECTIVE STACK HEIGHT (meters)
C
C        OUTPUTS: SIGZ0 - INITIAL SIGMA-Z due to PLATFORM (meters)
C                 SIGY0 - INITIAL SIGMA-Y due to PLATFORM (meters)
C
C        CALLED FROM:   AERCALC
C***********************************************************************

      USE MAIN1, ONLY: PLATFMDBG, PLATFMDBUNT
      IMPLICIT NONE

C     Variable Declarations
      DOUBLE PRECISION, INTENT(IN)  :: XD, HB, W, HE
      DOUBLE PRECISION, INTENT(OUT) :: SIGZ0, SIGY0

C     Local Variable Declarations
      DOUBLE PRECISION  :: A, LY, LZ, CHK, X, SYOP, SZOP
      DOUBLE PRECISION, PARAMETER ::
     &                    AY = 1.9D0, BY = 48.2D0, CY = -1.4D0,
     &                    AZ = 3.0D0, BZ = 40.2D0, CZ = -1.4D0

      SIGZ0 = 0.0D0
      SIGY0 = 0.0D0

      LY = W/2.0D0
      LZ = HB
      CHK = XD/HB

      IF( CHK .LT. 2.2D0) THEN
         X = 2.2D0*HB
         SYOP = 0.071D0*X*DSQRT(AY+(BY*(X/LY)**CY)-1.0D0)
C   3.915 = SQRT PORTION OF SZOP EQ. SOLVED USING X/LZ = 2.2
         SZOP = (.11D0*X**0.81D0)*3.915D0
      ELSEIF( CHK .GT. 12.6D0) THEN
         X = 12.6D0*HB
         SYOP = 0.071D0*X*DSQRT(AY+(BY*(X/LY)**CY)-1.0D0)
C   3.915 = SQRT PORTION OF SZOP EQ. SOLVED USING X/LZ = 12.6
         SZOP = (.11D0*X**.81D0)*1.777D0
      ELSE
         SYOP = 0.071D0*XD*DSQRT(AY+(BY*(XD/LY)**CY)-1.0D0)
         SZOP = 0.11D0*((XD)**0.81D0)*DSQRT(AZ+(BZ*(XD/LZ)**CZ)-1.0D0)
      ENDIF

      A =HE/HB
      IF( A .GT. 3.0D0 ) THEN
         SIGY0 = 0.0D0
         SIGZ0 = 0.0D0
      ELSEIF( (1.2D0 .LT. A) .AND. (A .LE. 3.0D0) ) THEN
         SIGY0 = 0.0D0
         SIGZ0 = 0.5D0*(3.0D0-A)*SZOP
      ELSEIF( (1.0D0 .LT. A) .AND. (A. LE. 1.2D0) ) THEN
         SIGY0 = 0.5D0*(6.0D0-5.0D0*A)*SYOP
         SIGZ0 = 0.5D0*(3.0D0-A)*SZOP
      ELSE
         SIGY0 = SYOP
         SIGZ0 = SZOP
      ENDIF

      IF (PLATFMDBG) THEN
         WRITE(PLATFMDBUNT,'(A, 4(A, 2X, F10.2),5(A, 2X, F8.5))')
     &           'calc1.f/PLAT_DOWNWASH: ',
     &           ' XD = ', XD,
     &           ' HB = ', HB,
     &           ' W = ', W,
     &           ' HE = ', HE,
     &           ' SZOP = ', SZOP,
     &           ' SYOP = ', SYOP,
     &           ' A = ', A,
     &           ' SIGZ0 = ', SIGZ0,
     &           ' SIGY0 = ', SIGY0
      END IF

      RETURN
      END

      DOUBLE PRECISION FUNCTION PLAT_GRADPLUMADJ( XARG )
C***********************************************************************
C        PLAT_GRADPLUMADJ Function
C
C        PURPOSE: Set-up distance dependent CC for CUBIC function. Output of
C                 CUBIC is DELH used to adjust plumerise based on platform influence.
C
C        PROGRAMMER: Michelle G. Snyder
C
C        DATE:    July 29, 2021
C
C        MODIFICATIONS:
C                 Changed name of subroutine to have PLAT_ to denote it is
C                 used for PLATFORM sources.  Reformatted to AERMOD style.
C                 Added debug statements for PLATFORM debug file.
C                 Michelle G. Snyder, WOOD 8/5/2021.
C
C        INPUTS:  XARG - Downwind distance in meters
C
C        OUTPUTS: Adjustment for gradual plume rise due to platform downwash
C
C        CALLED FROM:   AERCALC
C***********************************************************************
      USE MAIN1, ONLY: FB, UP, AC, BC, DHP, DHP1, STABLE, UNSTAB, HS,
     &                 ZI, XMAX, PLATFMDBG, PLATFMDBUNT
      IMPLICIT NONE

C     Variable declarations - input
      DOUBLE PRECISION, INTENT(IN)    :: XARG      ! Downwind distance (input)

C     Local variables
      DOUBLE PRECISION    :: DELH  ! Plume rise adjustment from PLAT_CUBIC
      DOUBLE PRECISION    :: CC    ! Distance-dependent 3rd coefficient of CUBIC
      DOUBLE PRECISION    :: SCOEFF  ! check before CUBIC
      DOUBLE PRECISION, PARAMETER  :: TOL =   1.0D-4 !Set tolerance for CUBIC eqn solution

C     External Functions:
      DOUBLE PRECISION, EXTERNAL  :: PLAT_CUBIC

C ----------------------------------------------------------------------
C                    OCD Gradual Plume Rise Adjustment
C
C        IN STABLE CASES:
C           OCD calculates a distance dependent neutral rise CC (OCD PRI01260), then computes a
C           stable rise CC (OCD PRI01620); the lesser is used. This CC is kept and used in cubic
C
C        IN UNSTABLE CASES:
C           Compute cubic coefficient for gradual rise - from OCD model
C           C = (160)*(Fb**1/3) * (x**2/3) / (u)
C            where: Fb = bouyancy flux
C                   x = distance from source (km)
C                   u = stack top windspeed (m/s)
C
C           OCD assumes x is in km and C is in meters
C           C**3 = 4096000 * Fb * x**2 / u**3
C
C           Convert to x in meters for use in AERMOD
C           C**3 = 4.096 * Fb * x**2 / u**3
C ----------------------------------------------------------------------

      DELH = 0.0D0 !intitalize

      IF (STABLE  .OR.  (UNSTAB  .AND.  (HS .GE. ZI))) THEN
         CC = -1.0D0 * DHP * DHP * DHP !OCD PRI01260 & PRI01620; AERMOD calculates distance-dependent DHP in prise.f/SBLRIS, Neutral limit is applied in SBLRIS
         SCOEFF=AC+BC+CC !AC & BC are distance independent (MAIN1) and computed in calc1.f/PCALC
         IF((SCOEFF.NE.0.0D0) .AND. (XARG < XMAX)) THEN
            DELH = PLAT_CUBIC(AC,BC,CC,DHP,TOL)
         END IF
      ELSE !modify direct plume downdraft (DHP1) only
         CC = -4.096D0*FB*XARG*XARG/(UP*UP*UP) !OCD REC00970, but XARG in meters see note above.
         SCOEFF=AC+BC+CC !AC & BC are distance independent (MAIN1) and computed in calc1.f/PCALC
         IF(SCOEFF.NE.0.0D0) THEN
            DELH = PLAT_CUBIC(AC,BC,CC,DHP1,TOL)
         END IF
      END IF

      IF (PLATFMDBG) THEN
         WRITE(PLATFMDBUNT,'(A, 7(A, 2X, F10.2))')
     &           'calc1.f/PLAT_GRADPLUMADJ: ',
     &           ' XARG = ', XARG,
     &           ' XMAX = ', XMAX,
     &           ' FB = ', FB,
     &           ' UP = ', UP,
     &           ' CC = ', CC,
     &           ' SCOEFF = ', SCOEFF,
     &           ' DELH = ', DELH
      END IF

      PLAT_GRADPLUMADJ = DELH

      RETURN
      END

      DOUBLE PRECISION FUNCTION PLAT_CUBIC(A,B,C,ZINIT,TOL)
C***********************************************************************
C             CUBIC Module of the AMS/EPA Regulatory Model
C             (Adapted from AERMOD v.98314)
C
C        PURPOSE: Solves Cubic Equation Using Newton's Method
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C                 Adapted by Clint Tillerson for use with
C                 OCD platform downwash, 1/23/2012
C
C        MODIFICATIONS:
C                 Changed from subroutine to function. Changed name of
C                 function to have PLAT_ to denote it is used for PLATFORM
C                 sources. Michelle G. Snyder, WOOD 8/5/2021
C
C        INPUTS:  Coefficients (A, B and C) of Cubic Equation
c                 Initial Guess for Variable
C                 Tolerance Level for Iteration
C
C        OUTPUTS: Solution to Cubic Equation;
C                    Z**3 + A*Z**2 + B*Z + C = 0
C
C        CALLED FROM:   PLAT_GRADPLUMADJ
C***********************************************************************

      IMPLICIT NONE

C     Variable Declarations

      DOUBLE PRECISION, INTENT(IN)   :: A, B, C, ZINIT, TOL
      DOUBLE PRECISION   :: Z(25), FZ, FP
      INTEGER            :: N                      ! Loop counter

C     Assign Initial Guess to Z(1)
      Z(1) = ZINIT

C     Begin Iterative LOOP (24 iterations)
      DO 20 N = 1, 24
C        Calculate Cubic Function and First Derivative With Current Guess
         FZ = Z(N)*Z(N)*Z(N) + A*Z(N)*Z(N) + B*Z(N) + C
         FP = 3.0D0*Z(N)*Z(N) + 2.0D0*A*Z(N) + B

C        Calculate New Guess
         Z(N+1) = Z(N) - FZ/FP

C        Check successive iterations for specified tolerance level
         IF (ABS(Z(N+1) - Z(N)) .LE. TOL) THEN
            PLAT_CUBIC = Z(N+1)
C           Convergence, thus Exit Loop
            GO TO 999
         END IF

 20   CONTINUE
C     End Iterative LOOP

C     If No Convergence In Loop, Then Use Average of Last Two Estimates,
      PLAT_CUBIC = 0.5D0 * (Z(24) + Z(25))

 999  RETURN
      END

C***** END OCD PLATFORM ADDITIONS **************************************
