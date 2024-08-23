      SUBROUTINE OUCARD
C***********************************************************************
C                 OUCARD Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: To process OUtput Pathway card images
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:  Added new MAXDAILY, MXDYBYYR, MAXDCONT, and NOHEADER
C                   keywords for new output options.
C                   R. W. Brode, U.S. EPA, OAQPS, AQMG, 02/28/2011
C
C        MODIFIED:  Added new SUMMFILE keyword for optional summary
C                   file of high ranked values and new FILEFORM
C                   keyword for specifying use of exponential-formatted
C                   output for results files.
C                   R. W. Brode, U.S. EPA, OAQPS, AQMG, 10/19/2009
C
C        MODIFIED:  To add subroutine call for TOXXFILE option - 9/29/92
C
C        INPUTS:  Pathway (OU) and Keyword
C
C        OUTPUTS: Output Option Switches
C                 Output Setup Status Switches
C
C        CALLED FROM:   SETUP
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'OUCARD'

      IF (KEYWRD .EQ. 'STARTING') THEN
C        Set Status Switch
         IOSTAT(1) = IOSTAT(1) + 1
         IF (IOSTAT(1) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         END IF
      ELSE IF (KEYWRD .EQ. 'RECTABLE') THEN
C        Process High Value Output Option                   ---   CALL OUHIGH
         CALL OUHIGH
C        Set Status Switch
         IOSTAT(2) = IOSTAT(2) + 1
      ELSE IF (KEYWRD .EQ. 'MAXTABLE') THEN
C        Process Maximum 50 Table Option                    ---   CALL OUMXVL
         CALL OUMXVL
C        Set Status Switch
         IOSTAT(3) = IOSTAT(3) + 1
      ELSE IF (KEYWRD .EQ. 'DAYTABLE') THEN
C        Process Daily Value Table Option                   ---   CALL OUDALY
         CALL OUDALY
C        Set Status Switch
         IOSTAT(4) = IOSTAT(4) + 1
         IF (IOSTAT(4) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         END IF
      ELSE IF (KEYWRD .EQ. 'MAXIFILE') THEN
C        Process Maximum Value (Threshold) File Option      ---   CALL OUMXFL
         CALL OUMXFL
C        Set Status Switch
         IOSTAT(5) = IOSTAT(5) + 1
      ELSE IF (KEYWRD .EQ. 'POSTFILE') THEN
C        Process Postprocessing File Output Option          ---   CALL OUPOST
         CALL OUPOST
C        Set Status Switch
         IOSTAT(6) = IOSTAT(6) + 1
      ELSE IF (KEYWRD .EQ. 'PLOTFILE') THEN
C        Process Plotting File Output Option                ---   CALL OUPLOT
         CALL OUPLOT
C        Set Status Switch
         IOSTAT(7) = IOSTAT(7) + 1
      ELSE IF (KEYWRD .EQ. 'TOXXFILE') THEN
C        Process TOXXFILE Output Option                     ---   CALL OUTOXX
         CALL OUTOXX
C        Set Status Switch
         IOSTAT(8) = IOSTAT(8) + 1
      ELSE IF (KEYWRD .EQ. 'SEASONHR') THEN
         IF (.NOT. SCIM) THEN
C           Process Season by Hour-of-Day Output Option     ---   CALL OUSEAS
            CALL OUSEAS
C           Set Status Switch
            IOSTAT(9) = IOSTAT(9) + 1
         ELSE
C           Write Error Message: Conflicting Options SCIM and SEASONHR
            CALL ERRHDL(PATH,MODNAM,'E','154',KEYWRD)
         END IF
      ELSE IF (KEYWRD .EQ. 'RANKFILE') THEN
C        Process RANKFILE Output Option                     ---   CALL OURANK
         CALL OURANK
C        Set Status Switch
         IOSTAT(10) = IOSTAT(10) + 1
      ELSE IF (KEYWRD .EQ. 'EVALFILE') THEN
C        Process EVALFILE Output Option                     ---   CALL OUEVAL
         CALL OUEVAL
C        Set Status Switch
         IOSTAT(11) = IOSTAT(11) + 1
      ELSE IF (KEYWRD .EQ. 'SUMMFILE') THEN
C        Process SUMMFILE Output Option                     ---   CALL OUSUMM
         CALL OUSUMM
C        Set Status Switch
         IOSTAT(12) = IOSTAT(12) + 1
         IF (IOSTAT(12) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         END IF
      ELSE IF (KEYWRD .EQ. 'FILEFORM') THEN
C        Process FILEFORM Output Option                     ---   CALL FILEFORM
         CALL FILEFORM
C        Set Status Switch
         IOSTAT(13) = IOSTAT(13) + 1
         IF (IOSTAT(13) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         END IF
      ELSE IF (KEYWRD .EQ. 'MAXDAILY') THEN
         IF (NO2AVE .OR. SO2AVE .OR. PM25AVE) THEN
            DO IAVE = 1, NUMAVE
               IF (KAVE(IAVE) .EQ. 1 .OR. KAVE(IAVE) .EQ. 24) THEN
C                 Process Maximum Daily 1-hr File Output Option ---   CALL OUMAXDLY
                  CALL OUMAXDLY
C                 Set Status Switch
                  IOSTAT(14) = IOSTAT(14) + 1
                  EXIT
               END IF
            END DO
         ELSE
C ---       MAXDAILY option is only applicable to NO2AVE or SO2AVE 1-hr processing
            CALL ERRHDL(PATH,MODNAM,'E','162',KEYWRD)
            GO TO 999
         END IF
         IF (IOSTAT(14) .LT. 1) THEN
C           WRITE Error Message: MAXDAILY Option without 1-hr averages
            CALL ERRHDL(PATH,MODNAM,'E','162',KEYWRD)
         END IF
      ELSE IF (KEYWRD .EQ. 'MXDYBYYR') THEN
         IF (NO2AVE .OR. SO2AVE .OR. PM25AVE) THEN
            DO IAVE = 1, NUMAVE
               IF (KAVE(IAVE) .EQ. 1 .OR. KAVE(IAVE) .EQ. 24) THEN
C                 Process Maximum Daily 1-hr File Output Option ---   CALL OUMXDLY_BYYR
                  CALL OUMXDLY_BYYR
C                 Set Status Switch
                  IOSTAT(15) = IOSTAT(15) + 1
                  EXIT
               END IF
            END DO
         ELSE
C ---       MXDYBYYR option is only applicable to NO2AVE or SO2AVE 1-hr processing
            CALL ERRHDL(PATH,MODNAM,'E','162',KEYWRD)
            GO TO 999
         END IF
         IF (IOSTAT(15) .LT. 1) THEN
C           WRITE Error Message: MXDYBYYR Option without 1-hr averages
            CALL ERRHDL(PATH,MODNAM,'E','162',KEYWRD)
         END IF
      ELSE IF (KEYWRD .EQ. 'MAXDCONT') THEN
         IF (NO2AVE .OR. SO2AVE .OR. PM25AVE) THEN
            DO IAVE = 1, NUMAVE
               IF (KAVE(IAVE) .EQ. 1 .OR. KAVE(IAVE) .EQ. 24) THEN
C                 Process Maximum Daily 1-hr File Output Option ---   CALL OUMAXD_CONT
                  CALL OUMAXD_CONT
C                 Set Status Switch
                  IOSTAT(16) = IOSTAT(16) + 1
                  EXIT
               END IF
            END DO
         ELSE
C ---       MAXDCONT option is only applicable to PM25 24-hr or NO2AVE or SO2AVE 1-hr processing
            CALL ERRHDL(PATH,MODNAM,'E','163',KEYWRD)
            GO TO 999
         END IF
         IF (IOSTAT(16) .LT. 1) THEN
C           WRITE Error Message: MAXDCONT Option without 1-hr or 24-hr averages
            CALL ERRHDL(PATH,MODNAM,'E','163',KEYWRD)
         END IF
      ELSE IF (KEYWRD .EQ. 'NOHEADER') THEN
C        Process NOHEADER Output Option                     ---   CALL NOHEADER
         CALL NOHEADER
C        Set Status Switch
         IOSTAT(18) = IOSTAT(18) + 1
         IF (IOSTAT(18) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         END IF
      ELSE IF (KEYWRD .EQ. 'FINISHED') THEN
C        Set Status Switch
         IOSTAT(50) = IOSTAT(50) + 1
         IF (IOSTAT(50) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         END IF
C        Check The Consistency of The Output Options
         CALL OUTQA
      ELSE
C        Write Error Message: Invalid Keyword for This Pathway
         CALL ERRHDL(PATH,MODNAM,'E','110',KEYWRD)
      END IF

999   CONTINUE

      RETURN
      END

      SUBROUTINE OUTQA
C***********************************************************************
C                 OUTQA Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: To process OUtput Pathway card images QA Check
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:  Included logical variable PLFILE for PLOTFILEs
C                   in the check for output file options that use
C                   the FILEFORM keyword specifying whether
C                   fixed-format or exp-format is used. Previous
C                   version erroneously issued warning message '399'
C                   if PLOTFILE was the only relevant output option
C                   used with the OU FILEFORM keyword.
C                   R.W. Brode, U.S. EPA/OAQPS/AQMG, 12/19/2011
C
C                   Corrected write statement for message '540'
C                   to accommodate MONTH average option, labeled
C                   as 720-hr option.  Also modified to include 
C                   error checking for file name and file unit 
C                   conflicts across output file options.
C                   R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
C
C                   To check for EVALFILE option without EVALCART.
C                   R.W. Brode, MACTEC/PES - 10/26/04
C
C                   To Include TOXXFILE Option - 9/29/92
C
C        INPUTS:  Pathway (OU) and Keyword
C
C        OUTPUTS: Output Messages
C
C        CALLED FROM: OUCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, J, K, l, m, IVAL, IDCST1, NumNoHeader
C     JAT D065 8/9/21, FOUND SET BUT NEVER USED
C      LOGICAL OUTOPT, FOUND
      LOGICAL OUTOPT
      CHARACTER KEYMSG*8, MSG1*3

C     Variable Initializations
      MODNAM = 'OUTQA'
      MSG1   = '-HR'
      OUTOPT = .FALSE.
C     JAT D065 8/9/21, FOUND SET BUT NEVER USED
C      FOUND  = .FALSE.

C     Check If Missing Mandatory Keyword
      IF (IOSTAT(1) .EQ. 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','130','STARTING')
      END IF

C     Check For Lack of Any Output Option Cards
      DO I = 2, 12
         IF (IOSTAT(I) .GT. 0) THEN
            OUTOPT = .TRUE.
         END IF
      END DO
      IF (.NOT.OUTOPT .AND. .NOT.PERIOD .AND. .NOT.ANNUAL) THEN
C        WRITE Error Message - No Output Keywords and No PERIOD Averages
         CALL ERRHDL(PATH,MODNAM,'E','189','  ')
      END IF

      DO IAVE = 1, NUMAVE
         IDCST1 = 0
         DO IVAL = 1, NVAL
            IF (NHIAVE(IVAL,IAVE) .EQ. 1) THEN
               IDCST1 = 1
            END IF
         END DO
         IF (IDCST1.EQ.0 .AND. MAXAVE(IAVE).EQ.0 .AND.
     &                         IDYTAB(IAVE).EQ.0) THEN
            WRITE(KEYMSG,'(I3.3,A3)') KAVE(IAVE), MSG1
            CALL ERRHDL(PATH,MODNAM,'W','540',KEYMSG)
         END IF
      END DO

C     Check for DAYTABLE Option With SAVEFILE or INITFILE Options
      IF (DAYTAB .AND. (RSTSAV .OR. RSTINP)) THEN
C        WRITE Warning Message: DAYTABLE Results Overwritten on Re-start
         CALL ERRHDL(PATH,MODNAM,'W','190','DAYTABLE')
      END IF
C     Check for TOXXFILE Option With SAVEFILE or INITFILE Options
      IF (TXFILE .AND. (RSTSAV .OR. RSTINP)) THEN
C        WRITE Error Message: Incompatible Options
         CALL ERRHDL(PATH,MODNAM,'E','190','TOXXFILE')
      END IF
C     Check for EVALFILE Option With SAVEFILE or INITFILE Options
      IF (RSTSAV .OR. RSTINP) THEN
         DO I = 1, NUMSRC
            IF (EVAL(I)) THEN
C              WRITE Warning Message: EVALFILE results overwritten on Re-start
               CALL ERRHDL(PATH,MODNAM,'W','190','EVALFILE')
               EXIT
            END IF
         END DO
      END IF

C     Check for PM-2.5, NO2, or SO2 processing with EVENTFIL and no MAXIFILE
      IF ((PM25AVE .OR. NO2AVE .OR. SO2AVE) .AND. EVENTS .AND. 
     &                                               .NOT.MXFILE) THEN
C        Write Warning Message:  EVENTFIL option not compatible
C        with PM-2.5, NO2, or SO2 processing without MAXIFILE option
         CALL ERRHDL(PATH,MODNAM,'W','191','EVENTFIL Opt')
         EVENTS = .FALSE.
      END IF

C     Check for EVALFILE Option without EVALCART Inputs
      IF (IOSTAT(11) .GT. 0 .AND. NUMARC .EQ. 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','256','NUMARC=0')
      END IF

C --- Check for FILEFORM keyword without applicable output file options
      IF (IOSTAT(13) .GT. 0 .AND. FILE_FORMAT .EQ. 'EXP') THEN
         IF (.NOT.MXFILE .AND. .NOT.PPFILE .AND. .NOT.PLFILE .AND.
     &       .NOT.RKFILE .AND.
     &       .NOT.ANPOST .AND. .NOT.ANPLOT .AND. .NOT.SEASONHR .AND.
     &       .NOT.MXDAILY .AND. .NOT.MXDAILY_BYYR .AND. 
     &                                            .NOT.L_MAXDCONT) THEN
C           Write Warning Message: FILEFORM keyword ignored
            CALL ERRHDL(PATH,MODNAM,'W','399','FILEFORM')
         END IF
      END IF

C --- Check for NOHEADER for non-selected output file option
      NumNoHeader = 0
      DO I = 1, 8
         IF (L_NoHeader(I)) THEN
            NumNoHeader = NumNoHeader + 1
         END IF
      END DO
      IF (NumNoHeader .GT. 0 .AND. NumNoHeader .LT. 8) THEN
C ---    NOHEADER option selected for at least one output 
C        type, but not all; check for output type on the
C        NOHEADER keyword that was not specified by user
         IF (L_NoHeader(1) .AND. IOSTAT(5) .LT. 1) THEN
C           Write Error Message:  Invalid output file type
            CALL ERRHDL(PATH,MODNAM,'E','164','MAXIFILE')
         END IF
         IF (L_NoHeader(2) .AND. IOSTAT(6) .LT. 1) THEN
C           Write Error Message:  Invalid output file type
            CALL ERRHDL(PATH,MODNAM,'E','164','POSTFILE')
         END IF
         IF (L_NoHeader(3) .AND. IOSTAT(7) .LT. 1) THEN
C           Write Error Message:  Invalid output file type
            CALL ERRHDL(PATH,MODNAM,'E','164','PLOTFILE')
         END IF
         IF (L_NoHeader(4) .AND. IOSTAT(9) .LT. 1) THEN
C           Write Error Message:  Invalid output file type
            CALL ERRHDL(PATH,MODNAM,'E','164','SEASONHR')
         END IF
         IF (L_NoHeader(5) .AND. IOSTAT(10) .LT. 1) THEN
C           Write Error Message:  Invalid output file type
            CALL ERRHDL(PATH,MODNAM,'E','164','RANKFILE')
         END IF
         IF (L_NoHeader(6) .AND. IOSTAT(14) .LT. 1) THEN
C           Write Error Message:  Invalid output file type
            CALL ERRHDL(PATH,MODNAM,'E','164','MAXDAILY')
         END IF
         IF (L_NoHeader(7) .AND. IOSTAT(15) .LT. 1) THEN
C           Write Error Message:  Invalid output file type
            CALL ERRHDL(PATH,MODNAM,'E','164','MXDYBYYR')
         END IF
         IF (L_NoHeader(8) .AND. IOSTAT(16) .LT. 1) THEN
C           Write Error Message:  Invalid output file type
            CALL ERRHDL(PATH,MODNAM,'E','164','MAXDCONT')
         END IF
      END IF
      
C---- Check for filename & fileunit conflicts across different types
C     of output files:      
      DO I = 1, NHIVAL
         DO J = 1, NUMGRP
            DO K = 1, NUMAVE
            DO L = 1, NUMGRP
            DO M = 1, NUMAVE
             IF (PLTFIL(I,J,K) .EQ. PSTFIL(L,M) .AND.
     &           IPLUNT(I,J,K) .NE. IPSUNT(L,M)) THEN
C              Write Error Message: Conflicting Inputs
               IF (L .GT. 999) THEN
                WRITE(DUMMY,'("999+ ",I3)') KAVE(M)
               ELSE 
                WRITE(DUMMY,'(I4,1X,I3)') L, KAVE(M)
               END IF
               CALL ERRHDL(PATH,MODNAM,'E','555',DUMMY)
               CYCLE
             ELSE IF (PLTFIL(I,J,K) .NE. PSTFIL(L,M) .AND.
     &                IPLUNT(I,J,K) .EQ. IPSUNT(L,M)) THEN
C              Write Error Message: Conflicting Inputs
               IF (L .GT. 999) THEN
                WRITE(DUMMY,'("999+ ",I3)') KAVE(M)
               ELSE 
                WRITE(DUMMY,'(I4,1X,I3)') L, KAVE(M)
               END IF
               CALL ERRHDL(PATH,MODNAM,'E','555',DUMMY)
               CYCLE 
             ELSE IF (PLTFIL(I,J,K) .EQ. THRFIL(L,M) .AND.
     &                IPLUNT(I,J,K) .NE. IMXUNT(L,M)) THEN
C              Write Error Message: Conflicting Inputs
               IF (L .GT. 999) THEN
                WRITE(DUMMY,'("999+ ",I3)') KAVE(M)
               ELSE 
                WRITE(DUMMY,'(I4,1X,I3)') L, KAVE(M)
               END IF
               CALL ERRHDL(PATH,MODNAM,'E','555',DUMMY)
               CYCLE
             ELSE IF (PLTFIL(I,J,K) .NE. THRFIL(L,M) .AND.
     &                IPLUNT(I,J,K) .EQ. IMXUNT(L,M)) THEN
C              Write Error Message: Conflicting Inputs
               IF (L .GT. 999) THEN
                WRITE(DUMMY,'("999+ ",I3)') KAVE(M)
               ELSE 
                WRITE(DUMMY,'(I4,1X,I3)') L, KAVE(M)
               END IF
               CALL ERRHDL(PATH,MODNAM,'E','555',DUMMY)
               CYCLE
             ELSE IF (PLTFIL(I,J,K) .EQ. ANNPST(L) .AND.
     &                IPLUNT(I,J,K) .NE. IAPUNT(L)) THEN
C              Write Error Message: Conflicting Inputs
               IF (L .GT. 999) THEN
                WRITE(DUMMY,'("999+ ",I3)') KAVE(M)
               ELSE 
                WRITE(DUMMY,'(I4,1X,I3)') L, KAVE(M)
               END IF
               CALL ERRHDL(PATH,MODNAM,'E','555',DUMMY)
               CYCLE
             ELSE IF (PLTFIL(I,J,K) .NE. ANNPST(L) .AND.
     &                IPLUNT(I,J,K) .EQ. IAPUNT(L)) THEN
C              Write Error Message: Conflicting Inputs
               IF (L .GT. 999) THEN
                WRITE(DUMMY,'("999+ ",I3)') KAVE(M)
               ELSE 
                WRITE(DUMMY,'(I4,1X,I3)') L, KAVE(M)
               END IF
               CALL ERRHDL(PATH,MODNAM,'E','555',DUMMY)
               CYCLE
             ELSE IF (PLTFIL(I,J,K) .EQ. ANNPLT(L) .AND.
     &                IPLUNT(I,J,K) .NE. IPPUNT(L)) THEN
C              Write Error Message: Conflicting Inputs
               IF (L .GT. 999) THEN
                WRITE(DUMMY,'("999+ ",I3)') KAVE(M)
               ELSE 
                WRITE(DUMMY,'(I4,1X,I3)') L, KAVE(M)
               END IF
               CALL ERRHDL(PATH,MODNAM,'E','555',DUMMY)
               CYCLE
             ELSE IF (PLTFIL(I,J,K) .NE. ANNPLT(L) .AND.
     &                IPLUNT(I,J,K) .EQ. IPPUNT(L)) THEN
C              Write Error Message: Conflicting Inputs
               IF (L .GT. 999) THEN
                WRITE(DUMMY,'("999+ ",I3)') KAVE(M)
               ELSE 
                WRITE(DUMMY,'(I4,1X,I3)') L, KAVE(M)
               END IF
               CALL ERRHDL(PATH,MODNAM,'E','555',DUMMY)
               CYCLE
             END IF
            end do
            end do
            END DO
         END DO
      END DO

      RETURN
      END

      SUBROUTINE OUHIGH
C***********************************************************************
C                 OUHIGH Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: To process High Value By Receptor Table
C                 Output Selections
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To allow user-specified ranks up to the 
C                    999th-highest values.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
C               
C        INPUTS:  Input Runstream Parameters
C
C        OUTPUTS: Output Option Switches
C
C        CALLED FROM:   OUCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, J, ILOCH(NAVE), IPRDT, IPRDT1, IPRDT2, ISPRD, IEPRD,
     &           HIGHST(NVAL)
      CHARACTER LPRD*8, HPRD*8, NCHR1(10)*8, NCHR2(10)*5
      LOGICAL FOUND, RMARK

C     Variable Initializations
      DATA (NCHR1(I),I=1,10) /'FIRST','SECOND','THIRD','FOURTH',
     &                        'FIFTH','SIXTH','SEVENTH','EIGHTH',
     &                        'NINTH','TENTH'/
      DATA (NCHR2(I),I=1,10) /'1ST','2ND','3RD','4TH','5TH',
     &                        '6TH','7TH','8TH','9TH','10TH'/
      MODNAM = 'OUHIGH'
      FOUND  = .FALSE.

      DO I = 1, NVAL
         HIGHST(I) = 0
      END DO

      DO I = 1, NAVE
         ILOCH(I) = 0
      END DO

C     Check If Enough Fields
      IF (IFC .EQ. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .EQ. 3) THEN
C        Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. NVAL+3) THEN
C        Error Message: Too Many Fields
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Retrieve Averaging Period
      IF (FIELD(3) .EQ. 'ALLAVE') THEN
C        Go For All Averaging Periods
         DO I = 1, NUMAVE
            INHI(I) = 1
            ILOCH(I) = 1
         END DO
         FOUND = .TRUE.
      ELSE IF (FIELD(3) .EQ. 'MONTH' .AND. MONTH) THEN
C        Set Value of IPRDT = 720 for MONTHly Averages
         IPRDT = 720
C        Search The Period to find out the Location
         DO I = 1, NUMAVE
            IF (IPRDT .EQ. KAVE(I)) THEN
               FOUND = .TRUE.
               INHI(I) = 1
               ILOCH(I) = 1
            END IF
         END DO
      ELSE
         CALL FSPLIT(PATH,KEYWRD,FIELD(3),ILEN_FLD,'-',RMARK,LPRD,HPRD)
C        Single Time Period
         IF (HPRD .EQ. LPRD) THEN
            CALL STONUM(HPRD,8,FNUM,IMIT)
            IF (IMIT .NE. 1) THEN
C              Write Error Message:Invalid Numerical Field and assign value of 0
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               IPRDT1 = 0
               GO TO 115
            ELSE
               IPRDT1 = NINT(FNUM)
            END IF
C           Search The Period to find out the Location
            DO I = 1, NUMAVE
               IF (IPRDT1 .EQ. KAVE(I)) THEN
                  FOUND = .TRUE.
                  INHI(I) = 1
                  ILOCH(I) = 1
               END IF
            END DO
         ELSE
C           Find The Lower Boundary
            CALL STONUM(LPRD,8,FNUM,IMIT)
            IF (IMIT .NE. 1) THEN
C              Write Error Message:Invalid Numerical Field and assign value of 0
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               IPRDT1 = 0
               GO TO 114
            ELSE
               IPRDT1 = NINT(FNUM)
            END IF
C           Find The Upper Boundary
 114        CALL STONUM(HPRD,8,FNUM,IMIT)
            IF (IMIT .NE. 1) THEN
C              Write Error Message:Invalid Numerical Field
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               GO TO 115
            END IF
            IPRDT2 = NINT(FNUM)
C           Search The Period to find out the Location
            DO I = 1, NUMAVE
               IF (KAVE(I).GE.IPRDT1 .AND.
     &             KAVE(I).LE.IPRDT2) THEN
                  FOUND = .TRUE.
                  INHI(I) = 1
                  ILOCH(I) = 1
               END IF
            END DO
C           Multi Time Period
         END IF
      END IF

 115  CONTINUE

C     Check Averaging Period Against KAVE Array,
      IF (.NOT. FOUND) THEN
C        Error Message:E203 AVEPER Not Match With Pre-Defined One
         CALL ERRHDL(PATH,MODNAM,'E','203','AVEPER')
         GO TO 999
      END IF

C     Begin LOOP Through Fields
      DO I = 4, IFC
C        Retrieve The High Value
         CALL FSPLIT(PATH,KEYWRD,FIELD(I),ILEN_FLD,'-',RMARK,LPRD,HPRD)
C        Fit To The Status Array
         ISPRD = 0
         IEPRD = 0
C ---    First check for simple numeric value
         CALL STONUM(LPRD,ILEN_FLD,FNUM,IMIT)
         IF (IMIT .EQ. 1) THEN
            ISPRD = INT(FNUM)
         END IF   
         CALL STONUM(HPRD,ILEN_FLD,FNUM,IMIT)
         IF (IMIT .EQ. 1) THEN
            IEPRD = INT(FNUM)
         END IF
         IF (ISPRD .GT. 999 .OR. IEPRD .GT. 999) THEN
C           Write Error Message:Illegal Parameter Field
            CALL ERRHDL(PATH,MODNAM,'E','203','HIVALU')
            CYCLE
         END IF
         IF (ISPRD .EQ. 0 .OR. IEPRD .EQ. 0) THEN
C           Check for acceptable character string if non-numeric
            DO J = 1, 10
               IF (LPRD.EQ.NCHR1(J) .OR.
     &             LPRD.EQ.NCHR2(J)) ISPRD = J
               IF (HPRD.EQ.NCHR1(J) .OR.
     &             HPRD.EQ.NCHR2(J)) IEPRD = J
            END DO
         END IF
         IF (ISPRD.EQ.0 .OR. IEPRD.EQ.0) THEN
C           Write Error Message:Illegal Parameter Field
            CALL ERRHDL(PATH,MODNAM,'E','203','HIVALU')
            CYCLE
         END IF
         IF (ISPRD.GT.NVAL .OR. IEPRD.GT.NVAL) THEN
C           Write Error Message: High Value Requested Exceeds NVAL
C           This shouldn't occur since limits are dynamically allocated
            WRITE(DUMMY,'(''NVAL='',I7)') NVAL
            CALL ERRHDL(PATH,MODNAM,'E','290',DUMMY)
            CYCLE
         END IF
         DO J = ISPRD,IEPRD
            HIGHST(J) = 1
         END DO
C     End LOOP Through Fields
      END DO

C     Set Array Switch to Indicate Which High Values to Report
C     And Set the Maximum Number of High Values, NHIVAL
      DO I = 1, NUMAVE
         DO J = 1, NVAL
            IF (HIGHST(J).EQ.1 .AND. ILOCH(I).EQ.1) THEN
               NHIAVE(J,I) = 1
               IF (J .GT. NHIVAL) THEN
                  NHIVAL = J
               END IF
            END IF
         END DO
      END DO

 999  RETURN
      END

      SUBROUTINE OUMXVL
C***********************************************************************
C                 OUMXVL Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: To Process Maximum (Overall) Value Table
C                 Output Selections for MAXTABLE keyword
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Parameters
C
C        OUTPUTS: Output Option Switches
C
C        CALLED FROM:   OUCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, J, IPRDT
      LOGICAL FOUND

C     Variable Initializations
      MODNAM = 'OUMXVL'
      FOUND = .FALSE.

C     Check for Appropriate Number of Fields
      IF (IFC .EQ. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .EQ. 3) THEN
C        Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 4) THEN
C        Error Message: Too Many Fields
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Retrieve Averaging Period
      IF (FIELD(3) .EQ. 'ALLAVE') THEN
C        Go For All Averaging Periods
         DO I = 1, NUMAVE
            MAXAVE(I) = 1
         END DO
         FOUND = .TRUE.
      ELSE
         IF (FIELD(3) .EQ. 'MONTH' .AND. MONTH) THEN
C           Set Value of IPRDT = 720 for MONTHly Averages
            IPRDT = 720
         ELSE
            CALL STONUM(FIELD(3),ILEN_FLD,FNUM,IMIT)
            IF (IMIT .NE. 1) THEN
C              Write Error Message:Invalid Numerical Field
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               GO TO 999
            END IF
            IPRDT = NINT(FNUM)
         END IF
C        Check Averaging Period Against KAVE Array
         J = 1
         DO WHILE (.NOT.FOUND .AND. J.LE.NUMAVE)
            IF (IPRDT .EQ. KAVE(J)) THEN
               FOUND = .TRUE.
               INDAVE = J
               MAXAVE(J) = 1
            END IF
            J = J + 1
         END DO
      END IF
      IF (.NOT. FOUND) THEN
C        Error Message: E203 AVEPER Not Match With Pre-Defined One
         CALL ERRHDL(PATH,MODNAM,'E','203','AVEPER')
         GO TO 999
      END IF

C     Set Number of Maximum Values to Sort
      CALL STONUM(FIELD(4),ILEN_FLD,FNUM,IMIT)
      IF (IMIT .NE. 1) THEN
C        Write Error Message:Invalid Numerical Field
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      END IF
      INUM = NINT(FNUM)

      IF (FIELD(3) .EQ. 'ALLAVE') THEN
C        Go For All Averaging Periods
         DO I = 1, NUMAVE
            IMXVAL(I) = INUM
         END DO
      ELSE
         IMXVAL(INDAVE) = INUM
      END IF

 999  RETURN
      END

      SUBROUTINE OUDALY
C***********************************************************************
C                 OUDALY Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: To Process Daily Concurrent Value Table
C                 Output Selections
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Parameters
C
C        OUTPUTS: Output Option Switches
C
C        CALLED FROM:   OUCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, J, IPRDT
      LOGICAL FOUND

C     Variable Initializations
      MODNAM = 'OUDALY'
      FOUND  = .FALSE.

C     Check for Appropriate Number of Fields
      IF (IFC .EQ. 2) THEN
C        Error Message: No AvePer And High Value
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. NUMAVE+2) THEN
C        Error Message: Too Many Fields
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Retrieve Averaging Period(s)
      IF (FIELD(3) .EQ. 'ALLAVE') THEN
C        Go For All Averaging Periods
         DO I = 1, NUMAVE
            IDYTAB(I) = 1
         END DO
C        Set Logical Switch Indicating That Daily Value Tables Are Generated
         DAYTAB = .TRUE.
      ELSE
         DO I = 3, IFC
            IF (FIELD(I) .EQ. 'MONTH' .AND. MONTH) THEN
C              Set Value of IPRDT = 720 for MONTHly Averages
               IPRDT = 720
            ELSE
               FOUND = .FALSE.
               CALL STONUM(FIELD(I),ILEN_FLD,FNUM,IMIT)
               IF (IMIT .NE. 1) THEN
C                 Write Error Message:Invalid Numerical Field
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  GO TO 999
               END IF
               IPRDT = NINT(FNUM)
            END IF
C           Check Averaging Period Against KAVE Array
            J = 1
            DO WHILE (.NOT.FOUND .AND. J.LE.NUMAVE)
               IF (IPRDT .EQ. KAVE(J)) THEN
                  FOUND = .TRUE.
                  IDYTAB(J) = 1
               END IF
               J = J + 1
            END DO
            IF (.NOT. FOUND) THEN
C              Error Message:E203 KAVE Not Match With Pre-Defined One
               CALL ERRHDL(PATH,MODNAM,'E','203','AVEPER')
               GO TO 999
            END IF
         END DO
C        Set Logical Switch Indicating That Daily Value Tables Are Generated
         DAYTAB = .TRUE.
      END IF

 999  RETURN
      END

      SUBROUTINE OUMXFL
C***********************************************************************
C                 OUMXFL Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: To Process Threshold Value Output Selections
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:  Modified to correct problems with MAXIFILE outputs 
C                   for re-started model runs using the SAVEFILE/INITFILE 
C                   option.  Previous versions used the 8-digit date 
C                   from the MAXIFILE files to compare with the "full" 
C                   12-digit date read from the INITFILE.  Also added
C                   error handling for missing MAXIFILE files with 
C                   INITFILE option, and for MAXIFILE files with data 
C                   past the start date for the MULTYEAR option.
C                   R. W. Brode, U.S. EPA, OAQPS, AQMG, 10/19/2009
C
C        MODIFIED:  To Change File Length Limit To 40 - 9/29/92
C        MODIFIED BY D. Strimaitis, SRC (for Wet & Dry DEPOSITION) - 11/8/93
C        MODIFIED:  To skip writing of header records if FATAL error
C                   has been encountered.  R.W. Brode, PES, Inc. - 6/20/95
C
C        INPUTS:  Input Runstream Parameters
C
C        OUTPUTS: Output Option Switches
C
C        CALLED FROM:   OUCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, J, IPRDT, IDAT, IDAT8
      INTEGER :: IRSYR, IRSDATE
      CHARACTER INPGRP*8, BUF90*90
      LOGICAL :: FOUND, L_EXISTS

C     Variable Initializations
      MODNAM = 'OUMXFL'
      L_EXISTS = .FALSE.
      BUF90  = ' '
      IDAT   = 0
      IDAT8  = 0

C     Set Logical Switch Indicating That Maximum Value File(s) Are Generated
      MXFILE = .TRUE.

C     Check If Enough Field
      IF (IFC .EQ. 2) THEN
C        Error Message: No Fields
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 6) THEN
C        Error Message: Not Enough Fields
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 7) THEN
C        Error Message: Too Many Fields
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Retrieve Averaging Period
      IF (FIELD(3) .EQ. 'MONTH' .AND. MONTH) THEN
C        Set Value of IPRDT = 720 for MONTHly Averages
         IPRDT = 720
      ELSE
         CALL STONUM(FIELD(3),ILEN_FLD,FNUM,IMIT)
         IF (IMIT .NE. 1) THEN
C           Write Error Message:Invalid Numerical Field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 999
         END IF
         IPRDT = NINT(FNUM)
      END IF

C     Check Averaging Period Against KAVE Array
      FOUND = .FALSE.
      INDAVE = 0
      J = 1
      DO WHILE (.NOT.FOUND .AND. J.LE.NUMAVE)
         IF (IPRDT .EQ. KAVE(J)) THEN
            FOUND = .TRUE.
            INDAVE = J
         END IF
         J = J + 1
      END DO
      IF (.NOT. FOUND) THEN
C        Error Message:E203 AVEPER Not Match With Pre-Defined One
         CALL ERRHDL(PATH,MODNAM,'E','203','AVEPER')
         GO TO 999
      END IF

C     Retrieve Source Group ID
      INPGRP = FIELD(4)
C     Check Source Group ID
      FOUND = .FALSE.
      INDGRP = 0
      J = 1
      DO WHILE (.NOT.FOUND .AND. J.LE.NUMGRP)
         IF (INPGRP .EQ. GRPID(J)) THEN
            FOUND = .TRUE.
            INDGRP = J
         END IF
         J = J + 1
      END DO
      IF (.NOT. FOUND) THEN
C        Error Message:E203 GRPID Not Match With Pre-Defined One
         CALL ERRHDL(PATH,MODNAM,'E','203','GRPID')
         GO TO 999
      END IF

C     Set Switch and Check for Previous MAXIFILE Card
C     for This Averaging Period & Group ID
      MAXFLE(INDGRP,INDAVE) = MAXFLE(INDGRP,INDAVE) + 1
      IF (MAXFLE(INDGRP,INDAVE) .GT. 1) THEN
C        WRITE Error Message
         CALL ERRHDL(PATH,MODNAM,'E','211',KEYWRD)
         GO TO 999
      END IF

C     Retrieve Threshold
      CALL STODBL(FIELD(5),ILEN_FLD,DNUM,IMIT)
C     Check for Valid Threshold Value
      IF (IMIT .NE. 1) THEN
C        Write Error Message:Invalid Numerical Field
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      END IF
      THRESH(INDGRP,INDAVE) = DNUM

      IF ((LOCE(6)-LOCB(6)) .LE. (ILEN_FLD - 1) ) THEN
C        Retrieve Filename as Character Substring to Maintain Original Case
C        Also Check for Filename Larger Than ILEN_FLD Characters
         THRFIL(INDGRP,INDAVE) = RUNST1(LOCB(6):LOCE(6))
      ELSE
C        WRITE Error Message:  THRFIL Field is Too Long
         WRITE(DUMMY,'(I8)') ILEN_FLD
         CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
      END IF

C     Retrieve File Unit If Input, or Assign File Unit and OPEN File
      IF (IFC .EQ. 7) THEN
         CALL STONUM(FIELD(7),ILEN_FLD,FNUM,IMIT)
C        Check for Valid File Unit Value
         IF (IMIT .NE. 1) THEN
C           Write Error Message:Invalid Numerical Field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 999
         END IF
C        Check for Conflict With System Files
         IF (NINT(FNUM) .LE. 30) THEN
C           WRITE Error Message:  Invalid File Unit Specified
            CALL ERRHDL(PATH,MODNAM,'E','560',KEYWRD)
            GO TO 999
         ELSE IF (NINT(FNUM) .GT. 100) THEN
C           WRITE Warning Message:  Suspect File Unit Specified
C           Unit May Conflict With Dynamically Allocated File Units
            CALL ERRHDL(PATH,MODNAM,'W','565',KEYWRD)
            IMXUNT(INDGRP,INDAVE) = NINT(FNUM)
         ELSE
            IMXUNT(INDGRP,INDAVE) = NINT(FNUM)
         END IF
      ELSE
C        Dynamically Allocate File Unit (100's)
         IMXUNT(INDGRP,INDAVE) = 100 + INDGRP*10 + INDAVE
         IF (INDGRP .GE. 10 .OR. INDAVE .GE. 10) THEN
C           WRITE Warning Message: Dynamic FUnit Allocation May Have Conflict
            CALL ERRHDL(PATH,MODNAM,'W','565',KEYWRD)
         END IF
      END IF

C     Check for Earlier Use of This Filename and File Unit
      FOUND = .FALSE.
      DO J = 1, NUMAVE
         DO I = 1, NUMGRP
            IF (I .NE. INDGRP .OR. J .NE. INDAVE) THEN
               IF (THRFIL(INDGRP,INDAVE) .EQ. THRFIL(I,J) .AND.
     &             IMXUNT(INDGRP,INDAVE) .EQ. IMXUNT(I,J)) THEN
                  FOUND = .TRUE.
               ELSE IF (THRFIL(INDGRP,INDAVE) .EQ. THRFIL(I,J) .AND.
     &                  IMXUNT(INDGRP,INDAVE) .NE. IMXUNT(I,J)) THEN
C                 Write Error Message: Conflicting Inputs
                  CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
                  GO TO 999
               ELSE IF (THRFIL(INDGRP,INDAVE) .NE. THRFIL(I,J) .AND.
     &                  IMXUNT(INDGRP,INDAVE) .EQ. IMXUNT(I,J)) THEN
C                 Write Error Message: Conflicting Inputs
                  CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
                  GO TO 999
               END IF
            END IF
         END DO
      END DO

      IF (.NOT. FOUND) THEN
C        First Time File is Identified; check for re-start option (RSTINP)
C        before OPENing File
         IF (RSTINP) THEN
C           Results Arrays Are To Be Initialized From Re-start File.
C           Check for existence of file first, then 
C           Read Start Date From File and Rewind.
            INQUIRE (FILE=THRFIL(INDGRP,INDAVE),EXIST=L_EXISTS)
            DUMMY = 'MAXIFILE'
            IF (L_EXISTS) THEN
               OPEN(IMXUNT(INDGRP,INDAVE),ERR=99,
     &              FILE=THRFIL(INDGRP,INDAVE),
     &              IOSTAT=IOERRN,STATUS='OLD')
            ELSE
C              File does not exist; restart option will not work
               CALL ERRHDL(PATH,MODNAM,'E','585',DUMMY)
               RETURN
            END IF
C           Results Arrays Are To Be Initialized From Re-start File.
C           Read Start Date From File, IRSDATE, and Rewind.
            DUMMY = 'INITFILE'
            READ(IRSUNT,ERR=919,END=919) IRSDATE
            REWIND IRSUNT
C           Now Position MAXIFILE To End of File, But Not Past IRSDATE.
            DUMMY = 'MAXIFILE'
            EOF = .FALSE.
            DO WHILE (.NOT. EOF)
               READ(IMXUNT(INDGRP,INDAVE),'(A90:)',ERR=919,
     &                                             END=199) BUF90
               IF (BUF90(1:1) .NE. '*') THEN
C                 Record Is Not Part of Header - Read Date For This Record
                  READ(BUF90(15:22),'(I8)',ERR=919) IDAT8
C                 Convert 8-digit IDAT8 to 10-digit IDAT for comparison to IRSDATE
                  IRSYR = IDAT8/1000000
                  IF (IRSYR .GE. ISTRT_WIND .AND. IRSYR .LE. 99) THEN
                     IRSYR = ISTRT_CENT*100 + IRSYR
                     IDAT  = ISTRT_CENT*100000000 + IDAT8
                  ELSE IF (IRSYR .LT. ISTRT_WIND) THEN
                     IRSYR = (ISTRT_CENT+1)*100 + IRSYR
                     IDAT  = (ISTRT_CENT+1)*100000000 + IDAT8
                  END IF
                  IF (IDAT .GT. IRSDATE) THEN
C                    Date of MAXIFILE Event Is Greater Than Start Date
C                    From Save File.  Treat As End of File to Exit Loop.
                     IF (MULTYR) THEN
C ---                   If this is a MULTYEAR run, then MAXIFILE date should not
C                       be greater than IRSDATE; issue error message
                        WRITE(DUMMY,'(I8.8)') IDAT8
                        CALL ERRHDL(PATH,MODNAM,'E','592',DUMMY)
                     END IF
                     GO TO 199
                  END IF
               END IF
               GO TO 11
 199           EOF = .TRUE.
 11            CONTINUE
            END DO
            EOF = .FALSE.
C           End of file or IRSDATE has been passed; backspace file
            BACKSPACE IMXUNT(INDGRP,INDAVE)
C           Skip Header Records
            GO TO 999
         ELSE
C ---       This is not a restarted run; just open the file
            OPEN(IMXUNT(INDGRP,INDAVE),ERR=99,
     &           FILE=THRFIL(INDGRP,INDAVE),
     &           IOSTAT=IOERRN,STATUS='REPLACE')
         END IF
      ELSE IF (FOUND .AND. RSTINP) THEN
C        This file is already open, and this run is a
C        Re-start from an earlier run
C        Skip Header Records
         GO TO 999
      END IF

C     Write Header to File.  Skip Header if FATAL.
      IF (RUN .AND. .NOT.FATAL .AND. .NOT.L_NoHeader(1)) THEN
         WRITE(IMXUNT(INDGRP,INDAVE),9005) VERSN, TITLE1(1:68), 
     &                                     RUNDAT
 9005    FORMAT('* AERMOD (',A6,'): ',A68,5X,A8)
         WRITE(IMXUNT(INDGRP,INDAVE),9007) C_METVER, RUNTIM, 
     &                          MODOPS_String(1:LEN_TRIM(MODOPS_String))
 9007    FORMAT('* AERMET (',A6,'):',T93,A8,
     &         /'* MODELING OPTIONS USED: ',A:)
         WRITE(IMXUNT(INDGRP,INDAVE),9010) CHRAVE(INDAVE),
     &               THRESH(INDGRP,INDAVE), GRPID(INDGRP), 
     &                                      THRFRM(1:LEN_TRIM(THRFRM))
 9010    FORMAT('*',9X,'MAXI-FILE FOR ',A5,' VALUES ',
     &          '>= A THRESHOLD OF ',G12.4,
     &         /'*',9X,'FOR SOURCE GROUP: ',A8,
     &         /'*',9X,'FORMAT: ',A:)
         WRITE(IMXUNT(INDGRP,INDAVE),9020) CHIDEP(1,1), CHIDEP(2,1),
     &                                     CHIDEP(3,1)
 9020    FORMAT('*AVE',3X,'GRP',5X,'DATE',11X,'X',13X,'Y',8X,'ZELEV',
     &          3X,'ZHILL',3X,'ZFLAG',2X,3A4,
     &          /'*___',2(1X,'________'),2(2X,'____________'),
     &          3(2X,'______'),'  ____________')
      END IF

      GO TO 999

C     WRITE Error Message for Error Opening File
 99   WRITE(DUMMY,'("MAXFL",I3.3)') IMXUNT(INDGRP,INDAVE)
      CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

      GO TO 999

C     WRITE Error Message for Error Reading File
 919  CALL ERRHDL(PATH,MODNAM,'E','510',DUMMY)

 999  CONTINUE

      RETURN
      END

      SUBROUTINE OUPOST
C***********************************************************************
C                 OUPOST Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: To Process Post-processor File Output Selections
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:  Modified to correct problems with POSTFILE outputs 
C                   for re-started model runs using the SAVEFILE/INITFILE 
C                   option.  Previous versions used the 8-digit date 
C                   from the POSTFILE files to compare with the "full" 
C                   12-digit date read from the INITFILE and also read 
C                   the 8-digit date from the wrong columns for POSTFILEs.
C                   Also added error handling for missing POSTFILE files 
C                   with INITFILE option, and for POSTFILE files with data 
C                   past the start date for the MULTYEAR option.
C                   R. W. Brode, U.S. EPA, OAQPS, AQMG, 10/19/2009
C
C        MODIFIED:  Increased length of HDRFRM variable to avoid
C                   potential problems with portability of code.
C                   R. Brode, MACTEC/PES, 10/17/2005
C
C        MODIFIED:  To Change File Length Limit To 40 - 9/29/92
C
C        MODIFIED BY D. Strimaitis, SRC (for Wet & Dry DEPOSITION) - 11/8/93
C
C        MODIFIED:  To skip writing of header records if FATAL error
C                   has been encountered.  R.W. Brode, PES, Inc. - 6/20/95
C
C        MODIFIED:  To change buffer length for PLOT format to 132, and
C                   change read statement to allow for multiple output
C                   types (CONC/DEPOS/etc.).   R.W. Brode, PES, Inc., 6/20/95
C
C        INPUTS:  Input Runstream Parameters
C
C        OUTPUTS: Output Option Switches
C
C        CALLED FROM:   OUCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, J, IPRDT, IDAT, IDAT8, ISTR, ISTP
      INTEGER :: IRSYR, IRSDATE
      CHARACTER INPGRP*8, HDRFRM*400
      CHARACTER (LEN = ILEN_FLD) :: BUFFER
      LOGICAL :: FOUND, L_EXISTS

C     Variable Initializations
      MODNAM = 'OUPOST'
      L_EXISTS = .TRUE.
      BUFFER = ' '
      HDRFRM = ' '
      IDAT   = 0
      IDAT8  = 0

C     Set Logical Switch Indicating That Postprocessor File(s) Are Generated
      PPFILE = .TRUE.

C     Create Header Format for Columns
      WRITE(HDRFRM,9020) NUMTYP, NUMTYP+2

C     Check If Enough Fields
      IF (IFC .EQ. 2) THEN
C        Error Message: No Fields
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 6) THEN
C        Error Message: Not Enough Fields
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 7) THEN
C        Error Message: Too Many Fields
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Retrieve Averaging Period
      IF (FIELD(3) .EQ. 'PERIOD' .AND. PERIOD) THEN
C        Post File is for PERIOD Averages                   ---   CALL PERPST
         CALL PERPST
C        Exit to End
         GO TO 999
      ELSE IF (FIELD(3) .EQ. 'ANNUAL' .AND. ANNUAL) THEN
C        Post File is for PERIOD Averages                   ---   CALL PERPST
         CALL PERPST
C        Exit to End
         GO TO 999
      ELSE IF (FIELD(3).EQ.'PERIOD' .OR. FIELD(3).EQ.'ANNUAL') THEN
C        Period Post File Selected But No PERIOD Averages Calculated
C        WRITE Error Message: Invalid Averaging Period Selected for POSTFILE
         CALL ERRHDL(PATH,MODNAM,'E','203',' AVEPER ')
         GO TO 999
      ELSE IF (FIELD(3) .EQ. 'MONTH' .AND. MONTH) THEN
C        Set Value of IPRDT = 720 for MONTHly Averages
         IPRDT = 720
      ELSE
         CALL STONUM(FIELD(3),ILEN_FLD,FNUM,IMIT)
         IF (IMIT .NE. 1) THEN
C           Write Error Message:Invalid Numerical Field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 999
         END IF
         IPRDT = NINT(FNUM)
      END IF

C     Check Averaging Period Against KAVE Array
      FOUND = .FALSE.
      INDAVE = 0
      J = 1
      DO WHILE (.NOT.FOUND .AND. J.LE.NUMAVE)
         IF (IPRDT .EQ. KAVE(J)) THEN
            FOUND = .TRUE.
            INDAVE = J
         END IF
         J = J + 1
      END DO
      IF (.NOT. FOUND) THEN
C        Error Message:E203 AVEPER Not Match With Pre-Defined One
         CALL ERRHDL(PATH,MODNAM,'E','203','AVEPER')
         GO TO 999
      END IF

C     Retrieve Source Group ID
      INPGRP = FIELD(4)
C     Check Source Group ID
      FOUND = .FALSE.
      INDGRP = 0
      J = 1
      DO WHILE (.NOT.FOUND .AND. J.LE.NUMGRP)
         IF (INPGRP .EQ. GRPID(J)) THEN
            FOUND = .TRUE.
            INDGRP = J
         END IF
         J = J + 1
      END DO
      IF (.NOT. FOUND) THEN
C        Error Message:E203 GRPID Not Match With Pre-Defined One
         CALL ERRHDL(PATH,MODNAM,'E','203','GRPID')
         GO TO 999
      END IF

C     Retrieve Format Secondary Keyword
      IF (FIELD(5) .EQ. 'UNFORM') THEN
         IPSFRM(INDGRP,INDAVE) = 0
      ELSE IF (FIELD(5) .EQ. 'PLOT') THEN
         IPSFRM(INDGRP,INDAVE) = 1
      ELSE
C        Error Message: Invalid Format Specified for POSTFILE
         CALL ERRHDL(PATH,MODNAM,'E','203','FORMAT')
         GO TO 999
      END IF

C     Set Switch and Check for Previous POSTFILE Card
C     for This Averaging Period & Group ID
      IPSTFL(INDGRP,INDAVE) = IPSTFL(INDGRP,INDAVE) + 1
      IF (IPSTFL(INDGRP,INDAVE) .GT. 1) THEN
C        WRITE Error Message
         CALL ERRHDL(PATH,MODNAM,'E','211',KEYWRD)
         GO TO 999
      END IF

      IF ((LOCE(6)-LOCB(6)) .LE. (ILEN_FLD - 1) ) THEN
C        Retrieve Filename as Character Substring to Maintain Original Case
C        Also Check for Filename Larger Than ILEN_FLD Characters
         PSTFIL(INDGRP,INDAVE) = RUNST1(LOCB(6):LOCE(6))
      ELSE
C        WRITE Error Message:  PSTFIL Field is Too Long
         WRITE(DUMMY,'(I8)') ILEN_FLD
         CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
      END IF

C     Retrieve File Unit If Input, or Assign File Unit and OPEN File
      IF (IFC .EQ. 7) THEN
         CALL STONUM(FIELD(7),ILEN_FLD,FNUM,IMIT)
C        Check for Valid File Unit Value
         IF (IMIT .NE. 1) THEN
C           Write Error Message: Invalid Numerical Field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 999
         END IF
C        Check for Conflict With System Files
         IF (NINT(FNUM) .LE. 30) THEN
C           WRITE Error Message:  Invalid File Unit Specified
            CALL ERRHDL(PATH,MODNAM,'E','560',KEYWRD)
            GO TO 999
         ELSE IF (NINT(FNUM) .GT. 100) THEN
C           WRITE Warning Message:  Suspect File Unit Specified
C           Unit May Conflict With Dynamically Allocated File Units
            CALL ERRHDL(PATH,MODNAM,'W','565',KEYWRD)
            IPSUNT(INDGRP,INDAVE) = NINT(FNUM)
         ELSE
            IPSUNT(INDGRP,INDAVE) = NINT(FNUM)
         END IF
      ELSE
C        Dynamically Allocate File Unit (200's)
         IPSUNT(INDGRP,INDAVE) = 200 + INDGRP*10 + INDAVE
         IF (INDGRP .GE. 10 .OR. INDAVE .GE. 10) THEN
C           WRITE Warning Message: Dynamic FUnit Allocation May Have Conflict
            CALL ERRHDL(PATH,MODNAM,'W','565',KEYWRD)
         END IF
      END IF

C     Check for Earlier Use of This Filename and File Unit
      FOUND = .FALSE.
      DO J = 1, NUMAVE
         DO I = 1, NUMGRP
            IF (I .NE. INDGRP .OR. J .NE. INDAVE) THEN
               IF (PSTFIL(INDGRP,INDAVE) .EQ. PSTFIL(I,J) .AND.
     &             IPSUNT(INDGRP,INDAVE) .EQ. IPSUNT(I,J)) THEN
                  FOUND = .TRUE.
               ELSE IF (PSTFIL(INDGRP,INDAVE) .EQ. PSTFIL(I,J) .AND.
     &                  IPSUNT(INDGRP,INDAVE) .NE. IPSUNT(I,J)) THEN
C                 Write Error Message: Conflicting Inputs
                  CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
                  GO TO 999
               ELSE IF (PSTFIL(INDGRP,INDAVE) .NE. PSTFIL(I,J) .AND.
     &                  IPSUNT(INDGRP,INDAVE) .EQ. IPSUNT(I,J)) THEN
C                 Write Error Message: Conflicting Inputs
                  CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
                  GO TO 999
               END IF
            END IF
         END DO
      END DO

C     Check Against POSTFILEs for PERIOD Averages
      DO I = 1, NUMGRP
         IF (PSTFIL(INDGRP,INDAVE) .EQ. ANNPST(I) .AND.
     &       IPSUNT(INDGRP,INDAVE) .EQ. IAPUNT(I)) THEN
            FOUND = .TRUE.
         ELSE IF (PSTFIL(INDGRP,INDAVE) .EQ. ANNPST(I) .AND.
     &            IPSUNT(INDGRP,INDAVE) .NE. IAPUNT(I)) THEN
C           Write Error Message: Conflicting Inputs
            CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
            GO TO 999
         ELSE IF (PSTFIL(INDGRP,INDAVE) .NE. ANNPST(I) .AND.
     &            IPSUNT(INDGRP,INDAVE) .EQ. IAPUNT(I)) THEN
C           Write Error Message: Conflicting Inputs
            CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
            GO TO 999
         END IF
      END DO

      IF (.NOT. FOUND) THEN
C        First Time File is Identified - OPEN File
         IF (FIELD(5) .EQ. 'UNFORM') THEN
C           First Time File is Identified; check for re-start option (RSTINP)
C           before OPENing File
            IF (RSTINP) THEN
C              Results Arrays Are To Be Initialized From Re-start File.
C              Check for existence of file first, then 
C              Read Start Date From File and Rewind.
               INQUIRE (FILE=PSTFIL(INDGRP,INDAVE),EXIST=L_EXISTS)
               DUMMY = 'POSTFILE'
               IF (L_EXISTS) THEN
                  OPEN(IPSUNT(INDGRP,INDAVE),ERR=99,
     &                 FILE=PSTFIL(INDGRP,INDAVE),IOSTAT=IOERRN,
     &                 FORM='UNFORMATTED',STATUS='OLD')
               ELSE
C                 File does not exist; restart option will not work
                  CALL ERRHDL(PATH,MODNAM,'E','585',DUMMY)
                  RETURN
               END IF
C              Results Arrays Are To Be Initialized From Re-start File.
C              Read Start Date From File, IRSDATE, and Rewind.
               DUMMY = 'INITFILE'
               READ(IRSUNT,ERR=919,END=919) IRSDATE
               REWIND IRSUNT
C              Now Position POSTFILE To End of File, But Not Past IRSDATE.
               DUMMY = 'POSTFILE'
               EOF = .FALSE.
               DO WHILE (.NOT. EOF)
                  READ(IPSUNT(INDGRP,INDAVE),ERR=919,END=199) IDAT8
C                 Convert 8-digit IDAT8 to 10-digit IDAT for comparison to IRSDATE
                  IRSYR = IDAT8/1000000
                  IF (IRSYR .GE. ISTRT_WIND .AND. IRSYR .LE. 99) THEN
                     IRSYR = ISTRT_CENT*100 + IRSYR
                     IDAT  = ISTRT_CENT*100000000 + IDAT8
                  ELSE IF (IRSYR .LT. ISTRT_WIND) THEN
                     IRSYR = (ISTRT_CENT+1)*100 + IRSYR
                     IDAT  = (ISTRT_CENT+1)*100000000 + IDAT8
                  END IF
                  IF (IDAT .GT. IRSDATE) THEN
C                    Date of POSTFILE Record Is Greater Than Start Date
C                    From Save File.  Treat As End of File to Exit Loop.
                     IF (MULTYR) THEN
C ---                   If this is a MULTYEAR run, then POSTFILE date should not
C                       be greater than IRSDATE; issue error message
                        WRITE(DUMMY,'(I8.8)') IDAT8
                        CALL ERRHDL(PATH,MODNAM,'E','593',DUMMY)
                     END IF
                     GO TO 199
                  END IF
                  GO TO 11
 199              EOF = .TRUE.
 11               CONTINUE
               END DO
               EOF = .FALSE.
C              End of file or IRSDATE has been passed; backspace file
               BACKSPACE IPSUNT(INDGRP,INDAVE)
C              Skip Header Records
               GO TO 999
            ELSE
C ---          This is not a restarted run; just open the file
               OPEN(IPSUNT(INDGRP,INDAVE),ERR=99,
     &              FILE=PSTFIL(INDGRP,INDAVE),IOSTAT=IOERRN,
     &              FORM='UNFORMATTED',STATUS='REPLACE')
            END IF
         ELSE IF (FIELD(5) .EQ. 'PLOT') THEN
C           First Time File is Identified; check for re-start option (RSTINP)
C           before OPENing File
            IF (RSTINP) THEN
C              Results Arrays Are To Be Initialized From Re-start File.
C              Check for existence of file first, then 
C              Read Start Date From File and Rewind.
               INQUIRE (FILE=PSTFIL(INDGRP,INDAVE),EXIST=L_EXISTS)
               DUMMY = 'POSTFILE'
               IF (L_EXISTS) THEN
                  OPEN(IPSUNT(INDGRP,INDAVE),ERR=99,
     &                 FILE=PSTFIL(INDGRP,INDAVE),
     &                 IOSTAT=IOERRN,FORM='FORMATTED',STATUS='OLD')
               ELSE
C                 File does not exist; restart option will not work
                  CALL ERRHDL(PATH,MODNAM,'E','585',DUMMY)
                  RETURN
               END IF
C              Results Arrays Are To Be Initialized From Re-start File.
C              Read Start Date From File, IRSDATE, and Rewind.
               DUMMY = 'INITFILE'
               READ(IRSUNT,ERR=919,END=919) IRSDATE
               REWIND IRSUNT
C              Now Position POSTFILE To End of File, But Not Past IRSDATE.
               DUMMY = 'POSTFILE'
               EOF = .FALSE.
               DO WHILE (.NOT. EOF)
                  READ(IPSUNT(INDGRP,INDAVE),'(A:)',
     &                                        ERR=919,END=299) BUFFER
                  IF (BUFFER(1:1) .NE. '*') THEN
C                    Record Is Not Part of Header - Read Date For This Record
C                    First calculate start & end of date string based on NUMTYP
                     ISTR = 90 + 14*(NUMTYP-1)
                     ISTP = ISTR + 7
                     READ(BUFFER(ISTR:ISTP),'(I8)',ERR=919) IDAT8
C                    Convert 8-digit IDAT8 to 10-digit IDAT for comparison to IRSDATE
                     IRSYR = IDAT8/1000000
                     IF (IRSYR .GE. ISTRT_WIND .AND. IRSYR .LE. 99) THEN
                        IRSYR = ISTRT_CENT*100 + IRSYR
                        IDAT  = ISTRT_CENT*100000000 + IDAT8
                     ELSE IF (IRSYR .LT. ISTRT_WIND) THEN
                        IRSYR = (ISTRT_CENT+1)*100 + IRSYR
                        IDAT  = (ISTRT_CENT+1)*100000000 + IDAT8
                     END IF
                  ELSE
C                    Header record - cycle to next record
                     CYCLE
                  END IF
                  IF (IDAT .GT. IRSDATE) THEN
C                    Date of POSTFILE Record Is Greater Than Start Date
C                    From Save File.  Treat As End of File to Exit Loop.
                     IF (MULTYR) THEN
C ---                   If this is a MULTYEAR run, then POSTFILE date should not
C                       be greater than IRSDATE; issue error message
                        WRITE(DUMMY,'(I8.8)') IDAT8
                        CALL ERRHDL(PATH,MODNAM,'E','593',DUMMY)
                     END IF
                     GO TO 299
                  END IF
                  GO TO 21
 299              EOF = .TRUE.
 21               CONTINUE
               END DO
               EOF = .FALSE.
C              End of file or IRSDATE has been passed; backspace file
               BACKSPACE IPSUNT(INDGRP,INDAVE)
C              Skip Header Records
               GO TO 999
            ELSE
C ---          This is not a restarted run; just open the file
               OPEN(IPSUNT(INDGRP,INDAVE),ERR=99,
     &              FILE=PSTFIL(INDGRP,INDAVE),
     &              IOSTAT=IOERRN,FORM='FORMATTED',STATUS='REPLACE')
            END IF
         END IF
      ELSE IF (FOUND .AND. RSTINP) THEN
C        This file is already open, and this run is a
C        Re-start from an earlier run
C        Skip Header Records
         GO TO 999
      END IF

C     Write Header to File for Formatted Plot Files.  Skip Header if FATAL.
      IF (RUN .AND. .NOT.FATAL .AND. FIELD(5).EQ.'PLOT' .AND. 
     &                                          .NOT.L_NoHeader(2)) THEN
         WRITE(IPSUNT(INDGRP,INDAVE),9005) VERSN, TITLE1(1:68), 
     &                                     RUNDAT
 9005    FORMAT('* AERMOD (',A6,'): ',A68,5X,A8)
         WRITE(IPSUNT(INDGRP,INDAVE),9007) C_METVER, TITLE2(1:68), 
     &                                    RUNTIM, 
     &                          MODOPS_String(1:LEN_TRIM(MODOPS_String))
 9007    FORMAT('* AERMET (',A6,'): ',A68,T93,A8,
     &         /'* MODELING OPTIONS USED: ',A:)
         WRITE(IPSUNT(INDGRP,INDAVE),9010) CHRAVE(INDAVE),GRPID(INDGRP),
     &                                     NUMREC, PSTFRM
 9010    FORMAT('*',9X,'POST/PLOT FILE OF CONCURRENT ',A5,' VALUES',
     &         ' FOR SOURCE GROUP: ',A8,
     &         /'*',9X,'FOR A TOTAL OF ',I5,' RECEPTORS.',
     &         /'*',9X,'FORMAT: ',A:)
         WRITE(IPSUNT(INDGRP,INDAVE),HDRFRM) (CHIDEP(1,ITYP),
     &                   CHIDEP(2,ITYP), CHIDEP(3,ITYP), ITYP=1,NUMTYP)
 9020 FORMAT('(''*'',8X,''X'',13X,''Y'',4X,',I1,'(2X,3A4),4X,''ZELEV'',
     &  4X,''ZHILL'',4X,''ZFLAG'',4X,''AVE'',5X,''GRP'',7X,''DATE'',5X,
     &  ''NET ID'',/,''*'',',I1,'(1X,''____________ ''),1X,
     & 3('' ______  ''),''______  ________  ________  ________'')')

      END IF

      GO TO 999

C     WRITE Error Message for Error Opening File
 99   WRITE(DUMMY,'("PSTFL",I3.3)') IPSUNT(INDGRP,INDAVE)
      CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

      GO TO 999

C     WRITE Error Message for Error Reading File
 919  CALL ERRHDL(PATH,MODNAM,'E','510',DUMMY)

 999  CONTINUE

      RETURN
      END

      SUBROUTINE OUPLOT
C***********************************************************************
C                 OUPLOT Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: To Process Plot File Output Selections
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:  To Change File Length Limit To 40 - 9/29/92
C
C        INPUTS:  Input Runstream Parameters
C
C        OUTPUTS: Output Option Switches
C
C        CALLED FROM:   OUCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, J, K, IPRDT
      CHARACTER NCHR1(10)*8, NCHR2(10)*5, INPGRP*8
      LOGICAL FOUND

C     Variable Initializations
      DATA (NCHR1(I),I=1,10) /'FIRST','SECOND','THIRD','FOURTH',
     &                        'FIFTH','SIXTH','SEVENTH','EIGHTH',
     &                        'NINTH','TENTH'/
      DATA (NCHR2(I),I=1,10) /'1ST','2ND','3RD','4TH','5TH',
     &                        '6TH','7TH','8TH','9TH','10TH'/
      MODNAM = 'OUPLOT'

C     Set Logical Switch Indicating That Plot File(s) Are Generated
      PLFILE = .TRUE.

C     Check If Enough Field
      IF (IFC .EQ. 2) THEN
C        Error Message: No Fields
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 5) THEN
C        Error Message: Not Enough Fields
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 7) THEN
C        Error Message: Too Many Fields
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Retrieve Averaging Period
      IF (FIELD(3) .EQ. 'PERIOD' .AND. PERIOD) THEN
C        Plot File is for PERIOD Averages                   ---   CALL PERPLT
         CALL PERPLT
C        Exit to End
         GO TO 999
      ELSE IF (FIELD(3) .EQ. 'ANNUAL' .AND. ANNUAL) THEN
C        Plot File is for PERIOD Averages                   ---   CALL PERPLT
         CALL PERPLT
C        Exit to End
         GO TO 999
      ELSE IF (FIELD(3).EQ.'PERIOD' .OR. FIELD(3).EQ.'ANNUAL') THEN
C        Period Plot File Selected But No PERIOD Averages Calculated
C        WRITE Error Message: Invalid Averaging Period Selected for PLOTFILE
         CALL ERRHDL(PATH,MODNAM,'E','203',' AVEPER ')
         GO TO 999
      ELSE IF (FIELD(3) .EQ. 'MONTH' .AND. MONTH) THEN
C        Set Value of IPRDT = 720 for MONTHly Averages
         IPRDT = 720
      ELSE
         CALL STONUM(FIELD(3),ILEN_FLD,FNUM,IMIT)
         IF (IMIT .NE. 1) THEN
C           Write Error Message:Invalid Numerical Field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 999
         END IF
         IPRDT = NINT(FNUM)
      END IF

C     Check Short Term Averaging Period Against KAVE Array
      FOUND = .FALSE.
      INDAVE = 0
      J = 1
      DO WHILE (.NOT.FOUND .AND. J.LE.NUMAVE)
         IF (IPRDT .EQ. KAVE(J)) THEN
            FOUND = .TRUE.
            INDAVE = J
         END IF
         J = J + 1
      END DO
      IF (.NOT. FOUND) THEN
C        Error Message:E203 AVEPER Not Match With Pre-Defined One
         CALL ERRHDL(PATH,MODNAM,'E','203','AVEPER')
         GO TO 999
      END IF

C     Retrieve Source Group ID
      INPGRP = FIELD(4)
C     Check Source Group ID
      FOUND = .FALSE.
      INDGRP = 0
      J = 1
      DO WHILE (.NOT.FOUND .AND. J.LE.NUMGRP)
         IF (INPGRP .EQ. GRPID(J)) THEN
            FOUND = .TRUE.
            INDGRP = J
         END IF
         J = J + 1
      END DO
      IF (.NOT. FOUND) THEN
C        Error Message:E203 GRPID Not Match With Pre-Defined One
         CALL ERRHDL(PATH,MODNAM,'E','203','GRPID')
         GO TO 999
      END IF

C     Retrieve High Value
      FOUND = .FALSE.
      INDVAL = 0
C     First check for numeric value
      CALL STONUM(FIELD(5),ILEN_FLD,FNUM,IMIT)
      IF (IMIT .EQ. 1) THEN
         FOUND = .TRUE.
         INDVAL = INT(FNUM)
      ELSE
         DO I = 1, 10
            IF (FIELD(5).EQ.NCHR1(I) .OR. FIELD(5).EQ.NCHR2(I)) THEN
               FOUND = .TRUE.
               INDVAL = I
            END IF
         END DO
      END IF
      IF (.NOT. FOUND) THEN
C        Error Message:E203 INDVAL Not Match With Options
         CALL ERRHDL(PATH,MODNAM,'E','203','HIVALU')
         GO TO 999
      ELSE IF (INDVAL .GT. NHIVAL) THEN
C        Error Message:E203 INDVAL Not Match With Options
         CALL ERRHDL(PATH,MODNAM,'E','203','HIVALU')
         GO TO 999
      END IF

C     Check High Value Specified Against Previous Options
      IF (NHIAVE(INDVAL,INDAVE) .NE. 1) THEN
C        WRITE Error Message
         CALL ERRHDL(PATH,MODNAM,'E','203','HIVALU')
         GO TO 999
      END IF

C     Set Switch and Check for Previous PLOTFILE Card
C     for This Averaging Period & Group ID
      IPLTFL(INDVAL,INDGRP,INDAVE) = IPLTFL(INDVAL,INDGRP,INDAVE) + 1
      IF (IPLTFL(INDVAL,INDGRP,INDAVE) .GT. 1) THEN
C        WRITE Error Message
         CALL ERRHDL(PATH,MODNAM,'E','211',KEYWRD)
         GO TO 999
      END IF

      IF ((LOCE(6)-LOCB(6)) .LE. (ILEN_FLD - 1) ) THEN
C        Retrieve Filename as Character Substring to Maintain Original Case
C        Also Check for Filename Larger Than ILEN_FLD Characters
         PLTFIL(INDVAL,INDGRP,INDAVE) = RUNST1(LOCB(6):LOCE(6))
      ELSE
C        WRITE Error Message:  PLTFIL Field is Too Long
         WRITE(DUMMY,'(I8)') ILEN_FLD
         CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
      END IF

C     Retrieve File Unit If Input, or Assign File Unit and OPEN File
      IF (IFC .EQ. 7) THEN
         CALL STONUM(FIELD(7),ILEN_FLD,FNUM,IMIT)
C        Check for Valid File Unit Value
         IF (IMIT .NE. 1) THEN
C           Write Error Message: Invalid Numerical Field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 999
         END IF
C        Check for Conflict With System Files
         IF (NINT(FNUM) .LE. 30) THEN
C           WRITE Error Message:  Invalid File Unit Specified
            CALL ERRHDL(PATH,MODNAM,'E','560',KEYWRD)
            GO TO 999
         ELSE IF (NINT(FNUM) .GT. 100) THEN
C           WRITE Warning Message:  Suspect File Unit Specified
C           Unit May Conflict With Dynamically Allocated File Units
            CALL ERRHDL(PATH,MODNAM,'W','565',KEYWRD)
            IPLUNT(INDVAL,INDGRP,INDAVE) = NINT(FNUM)
         ELSE
            IPLUNT(INDVAL,INDGRP,INDAVE) = NINT(FNUM)
         END IF
      ELSE
C        Dynamically Allocate File Unit (> 400)
         IPLUNT(INDVAL,INDGRP,INDAVE) = (INDVAL+3)*100+INDGRP*10+INDAVE
         IF (INDGRP .GE. 10 .OR. INDAVE .GE. 10) THEN
C           WRITE Warning Message: Dynamic FUnit Allocation May Have Conflict
            CALL ERRHDL(PATH,MODNAM,'W','565',KEYWRD)
         END IF
      END IF

C     Check for Earlier Use of This Filename and File Unit
      FOUND = .FALSE.
      DO K = 1, NUMAVE
        DO J = 1, NUMGRP
          DO I = 1, NHIVAL
            IF (I.NE.INDVAL .OR. J.NE.INDGRP .OR. K.NE.INDAVE) THEN
              IF (PLTFIL(INDVAL,INDGRP,INDAVE) .EQ. PLTFIL(I,J,K) .AND.
     &            IPLUNT(INDVAL,INDGRP,INDAVE) .EQ. IPLUNT(I,J,K)) THEN
                FOUND = .TRUE.
              ELSEIF (PLTFIL(INDVAL,INDGRP,INDAVE).EQ.PLTFIL(I,J,K).AND.
     &                IPLUNT(INDVAL,INDGRP,INDAVE).NE.IPLUNT(I,J,K))THEN
C               Write Error Message: Conflicting Inputs
                CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
                GO TO 999
              ELSEIF (PLTFIL(INDVAL,INDGRP,INDAVE).NE.PLTFIL(I,J,K).AND.
     &                IPLUNT(INDVAL,INDGRP,INDAVE).EQ.IPLUNT(I,J,K))THEN
C               Write Error Message: Conflicting Inputs
                CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
                GO TO 999
              END IF
            END IF
          END DO
        END DO
      END DO

C     Check Against PLOTFILEs for PERIOD Averages
      DO I = 1, NUMGRP
         IF (PLTFIL(INDVAL,INDGRP,INDAVE) .EQ. ANNPLT(I) .AND.
     &       IPLUNT(INDVAL,INDGRP,INDAVE) .EQ. IPPUNT(I)) THEN
           FOUND = .TRUE.
         ELSE IF (PLTFIL(INDVAL,INDGRP,INDAVE) .EQ. ANNPLT(I) .AND.
     &            IPLUNT(INDVAL,INDGRP,INDAVE) .NE. IPPUNT(I)) THEN
C          Write Error Message: Conflicting Inputs
           CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
           GO TO 999
         ELSE IF (PLTFIL(INDVAL,INDGRP,INDAVE) .NE. ANNPLT(I) .AND.
     &            IPLUNT(INDVAL,INDGRP,INDAVE) .EQ. IPPUNT(I)) THEN
C          Write Error Message: Conflicting Inputs
           CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
           GO TO 999
         END IF
      END DO

      IF (.NOT. FOUND) THEN
C        First Time File is Identified - OPEN File
         OPEN(IPLUNT(INDVAL,INDGRP,INDAVE),ERR=99,
     &        FILE=PLTFIL(INDVAL,INDGRP,INDAVE),
     &        IOSTAT=IOERRN,STATUS='REPLACE')
      END IF

      GO TO 999

C     WRITE Error Message for Error Opening File
 99   WRITE(DUMMY,'("PLTFL",I3.3)') IPLUNT(INDVAL,INDGRP,INDAVE)
      CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

 999  RETURN
      END

      SUBROUTINE PERPST
C***********************************************************************
C                 PERPST Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: To Process Postprocessor File Output Selection for PERIOD
C                 Averages
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:  To Change File Length Limit To 40 - 9/29/92
C
C        INPUTS:  Input Runstream Parameters
C
C        OUTPUTS: Output Option Switches
C
C        CALLED FROM:   OUPLOT
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, J
      CHARACTER INPGRP*8
      LOGICAL FOUND

C     Variable Initializations
      MODNAM = 'PERPST'

C     Set Logical Switch Indicating That Post File(s) Are Generated
      ANPOST = .TRUE.

C     Check If Too Many Fields
      IF (IFC .GT. 7) THEN
C        Error Message: Too Many Fields
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Retrieve Source Group ID
      INPGRP = FIELD(4)
C     Check Source Group ID
      FOUND = .FALSE.
      INDGRP = 0
      J = 1
      DO WHILE (.NOT.FOUND .AND. J.LE.NUMGRP)
         IF (INPGRP .EQ. GRPID(J)) THEN
            FOUND = .TRUE.
            INDGRP = J
         END IF
         J = J + 1
      END DO
      IF (.NOT. FOUND) THEN
C        Error Message: E203 GRPID Not Match With Pre-Defined One
         CALL ERRHDL(PATH,MODNAM,'E','203','GRPID')
         GO TO 999
      END IF

C     Set Switch and Check for Previous POSTFILE Card
C     for This Averaging Period & Group ID
      IANPST(INDGRP) = IANPST(INDGRP) + 1
      IF (IANPST(INDGRP) .GT. 1) THEN
C        WRITE Error Message
         CALL ERRHDL(PATH,MODNAM,'E','211',KEYWRD)
         GO TO 999
      END IF

C     Retrieve Format Secondary Keyword
      IF (FIELD(5) .EQ. 'UNFORM') THEN
         IANFRM(INDGRP) = 0
      ELSE IF (FIELD(5) .EQ. 'PLOT') THEN
         IANFRM(INDGRP) = 1
      ELSE
C        Error Message: Invalid Format Specified for POSTFILE
         CALL ERRHDL(PATH,MODNAM,'E','203','FORMAT')
         GO TO 999
      END IF

      IF ((LOCE(6)-LOCB(6)) .LE. (ILEN_FLD - 1) ) THEN
C        Retrieve Filename as Character Substring to Maintain Original Case
C        Also Check for Filename Larger Than ILEN_FLD Characters
         ANNPST(INDGRP) = RUNST1(LOCB(6):LOCE(6))
      ELSE
C        WRITE Error Message:  ANNPST Field is Too Long
         WRITE(DUMMY,'(I8)') ILEN_FLD
         CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
      END IF

C     Retrieve File Unit If Input, or Assign File Unit and OPEN File
      IF (IFC .EQ. 7) THEN
         CALL STONUM(FIELD(7),ILEN_FLD,FNUM,IMIT)
C        Check for Valid File Unit Value
         IF (IMIT .NE. 1) THEN
C           Write Error Message: Invalid Numerical Field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 999
         END IF
C        Check for Conflict With System Files
         IF (NINT(FNUM) .LE. 30) THEN
C           WRITE Error Message:  Invalid File Unit Specified
            CALL ERRHDL(PATH,MODNAM,'E','560',KEYWRD)
            GO TO 999
         ELSE IF (NINT(FNUM) .GT. 100) THEN
C           WRITE Warning Message:  Suspect File Unit Specified
C           Unit May Conflict With Dynamically Allocated File Units
            CALL ERRHDL(PATH,MODNAM,'W','565',KEYWRD)
            IAPUNT(INDGRP) = NINT(FNUM)
         ELSE
            IAPUNT(INDGRP) = NINT(FNUM)
         END IF
      ELSE
C        Dynamically Allocate File Unit (300's)
         IAPUNT(INDGRP) = 300 + INDGRP*10 - 5
         IF (INDGRP .GE. 10) THEN
C           WRITE Warning Message: Dynamic FUnit Allocation May Have Conflict
            CALL ERRHDL(PATH,MODNAM,'W','565',KEYWRD)
         END IF
      END IF

C     Check for Earlier Use of This Filename and File Unit
      FOUND = .FALSE.
      DO I = 1, NUMGRP
         IF (I .NE. INDGRP) THEN
            IF (ANNPST(INDGRP) .EQ. ANNPST(I) .AND.
     &          IAPUNT(INDGRP) .EQ. IAPUNT(I)) THEN
              FOUND = .TRUE.
            ELSE IF (ANNPST(INDGRP) .EQ. ANNPST(I) .AND.
     &               IAPUNT(INDGRP) .NE. IAPUNT(I)) THEN
C             Write Error Message: Conflicting Inputs
              CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
              GO TO 999
            ELSE IF (ANNPST(INDGRP) .NE. ANNPST(I) .AND.
     &               IAPUNT(INDGRP) .EQ. IAPUNT(I)) THEN
C             Write Error Message: Conflicting Inputs
              CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
              GO TO 999
            END IF
         END IF
      END DO

C     Check Against POSTFILEs for Short Term Averages
      DO J = 1, NUMAVE
         DO I = 1, NUMGRP
            IF (ANNPST(INDGRP) .EQ. PSTFIL(I,J) .AND.
     &          IAPUNT(INDGRP) .EQ. IPSUNT(I,J)) THEN
               FOUND = .TRUE.
            ELSE IF (ANNPST(INDGRP) .EQ. PSTFIL(I,J) .AND.
     &               IAPUNT(INDGRP) .NE. IPSUNT(I,J)) THEN
C              Write Error Message: Conflicting Inputs
               CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
               GO TO 999
            ELSE IF (ANNPST(INDGRP) .NE. PSTFIL(I,J) .AND.
     &               IAPUNT(INDGRP) .EQ. IPSUNT(I,J)) THEN
C              Write Error Message: Conflicting Inputs
               CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
               GO TO 999
            END IF
         END DO
      END DO

      IF (.NOT. FOUND) THEN
C        First Time File is Identified - OPEN File
         IF (FIELD(5) .EQ. 'UNFORM') THEN
            OPEN(IAPUNT(INDGRP),ERR=99,FILE=ANNPST(INDGRP),
     &           IOSTAT=IOERRN,FORM='UNFORMATTED',STATUS='REPLACE')
         ELSE IF (FIELD(5) .EQ. 'PLOT') THEN
            OPEN(IAPUNT(INDGRP),ERR=99,FILE=ANNPST(INDGRP),
     &           IOSTAT=IOERRN,FORM='FORMATTED',STATUS='REPLACE')
         END IF
      END IF

      GO TO 999

C     WRITE Error Message for Error Opening File
 99   WRITE(DUMMY,'("PSTFL",I3.3)') IAPUNT(INDGRP)
      CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

 999  RETURN
      END

      SUBROUTINE PERPLT
C***********************************************************************
C                 PERPLT Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: To Process Plot File Output Selection for PERIOD
C                 Averages
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:  To check correct field for long filenames.
C                   R. Brode, PES, 12/1/97
C
C        MODIFIED:  To Change File Length Limit To 40 - 9/29/92
C
C        INPUTS:  Input Runstream Parameters
C
C        OUTPUTS: Output Option Switches
C
C        CALLED FROM:   OUPLOT
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, J, K
      CHARACTER INPGRP*8
      LOGICAL FOUND

C     Variable Initializations
      MODNAM = 'PERPLT'

C     Set Logical Switch Indicating That Plot File(s) Are Generated
      ANPLOT = .TRUE.

C     Check If Too Many Fields
      IF (IFC .GT. 6) THEN
C        Error Message: Too Many Fields
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Retrieve Source Group ID
      INPGRP = FIELD(4)
C     Check Source Group ID
      FOUND = .FALSE.
      INDGRP = 0
      J = 1
      DO WHILE (.NOT.FOUND .AND. J.LE.NUMGRP)
         IF (INPGRP .EQ. GRPID(J)) THEN
            FOUND = .TRUE.
            INDGRP = J
         END IF
         J = J + 1
      END DO
      IF (.NOT. FOUND) THEN
C        Error Message: E203 GRPID Not Match With Pre-Defined One
         CALL ERRHDL(PATH,MODNAM,'E','203','GRPID')
         GO TO 999
      END IF

C     Set Switch and Check for Previous PLOTFILE Card
C     for This Averaging Period & Group ID
      IANPLT(INDGRP) = IANPLT(INDGRP) + 1
      IF (IANPLT(INDGRP) .GT. 1) THEN
C        WRITE Error Message
         CALL ERRHDL(PATH,MODNAM,'E','211',KEYWRD)
         GO TO 999
      END IF

      IF ((LOCE(5)-LOCB(5)) .LE. (ILEN_FLD - 1) ) THEN
C        Retrieve Filename as Character Substring to Maintain Original Case
C        Also Check for Filename Larger Than ILEN_FLD Characters
         ANNPLT(INDGRP) = RUNST1(LOCB(5):LOCE(5))
      ELSE
C        WRITE Error Message:  ANNPLT Field is Too Long
         WRITE(DUMMY,'(I8)') ILEN_FLD
         CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
      END IF

C     Retrieve File Unit If Input, or Assign File Unit and OPEN File
      IF (IFC .EQ. 6) THEN
         CALL STONUM(FIELD(6),ILEN_FLD,FNUM,IMIT)
C        Check for Valid File Unit Value
         IF (IMIT .NE. 1) THEN
C           Write Error Message: Invalid Numerical Field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 999
         END IF
C        Check for Conflict With System Files
         IF (NINT(FNUM) .LE. 30) THEN
C           WRITE Error Message:  Invalid File Unit Specified
            CALL ERRHDL(PATH,MODNAM,'E','560',KEYWRD)
            GO TO 999
         ELSE IF (NINT(FNUM) .GT. 100) THEN
C           WRITE Warning Message:  Suspect File Unit Specified
C           Unit May Conflict With Dynamically Allocated File Units
            CALL ERRHDL(PATH,MODNAM,'W','565',KEYWRD)
            IPPUNT(INDGRP) = NINT(FNUM)
         ELSE
            IPPUNT(INDGRP) = NINT(FNUM)
         END IF
      ELSE
C        Dynamically Allocate File Unit (300's)
         IPPUNT(INDGRP) = 300 + INDGRP*10
         IF (INDGRP .GE. 10) THEN
C           WRITE Warning Message: Dynamic FUnit Allocation May Have Conflict
            CALL ERRHDL(PATH,MODNAM,'W','565',KEYWRD)
         END IF
      END IF

C     Check for Earlier Use of This Filename and File Unit
      FOUND = .FALSE.
      DO I = 1, NUMGRP
         IF (I .NE. INDGRP) THEN
            IF (ANNPLT(INDGRP) .EQ. ANNPLT(I) .AND.
     &          IPPUNT(INDGRP) .EQ. IPPUNT(I)) THEN
              FOUND = .TRUE.
            ELSE IF (ANNPLT(INDGRP) .EQ. ANNPLT(I) .AND.
     &               IPPUNT(INDGRP) .NE. IPPUNT(I)) THEN
C             Write Error Message: Conflicting Inputs
              CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
              GO TO 999
            ELSE IF (ANNPLT(INDGRP) .NE. ANNPLT(I) .AND.
     &               IPPUNT(INDGRP) .EQ. IPPUNT(I)) THEN
C             Write Error Message: Conflicting Inputs
              CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
              GO TO 999
            END IF
         END IF
      END DO

C     Check Against PLOTFILEs for Short Term Averages
      DO K = 1, NUMAVE
         DO J = 1, NUMGRP
            DO I = 1, NHIVAL
               IF (ANNPLT(INDGRP) .EQ. PLTFIL(I,J,K) .AND.
     &             IPPUNT(INDGRP) .EQ. IPLUNT(I,J,K)) THEN
                  FOUND = .TRUE.
               ELSE IF (ANNPLT(INDGRP) .EQ. PLTFIL(I,J,K) .AND.
     &                  IPPUNT(INDGRP) .NE. IPLUNT(I,J,K)) THEN
C                 Write Error Message: Conflicting Inputs
                  CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
                  GO TO 999
               ELSE IF (ANNPLT(INDGRP) .NE. PLTFIL(I,J,K) .AND.
     &                  IPPUNT(INDGRP) .EQ. IPLUNT(I,J,K)) THEN
C                 Write Error Message: Conflicting Inputs
                  CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
                  GO TO 999
               END IF
            END DO
         END DO
      END DO

      IF (.NOT. FOUND) THEN
C        First Time File is Identified - OPEN File
         OPEN(IPPUNT(INDGRP),ERR=99,FILE=ANNPLT(INDGRP),
     &        IOSTAT=IOERRN,STATUS='REPLACE')
      END IF

      GO TO 999

C     WRITE Error Message for Error Opening File
 99   WRITE(DUMMY,'("PLTFL",I3.3)') IPPUNT(INDGRP)
      CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

 999  RETURN
      END

      SUBROUTINE OUTOXX
C***********************************************************************
C                 OUTOXX Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: To Process TOXXFILE Output Selections
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    September 29, 1992
C
C        INPUTS:  Input Runstream Parameters
C
C        OUTPUTS: Output Option Switches
C
C        CALLED FROM:   OUCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, J, IPRDT, NIDUM, NRDUM, NUMPER, RDUM
      CHARACTER BUF12*12
      LOGICAL FOUND

C     Variable Initializations
      MODNAM = 'OUTOXX'
      BUF12  = '            '
      IDUM  = 0
      RDUM  = 0
      NIDUM = 3
      NRDUM = 9

C     Set Logical Switch Indicating That Maximum Value File(s) Are Generated
      TXFILE = .TRUE.

C     Check If Enough Fields
      IF (IFC .EQ. 2) THEN
C        Error Message: No Fields
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 5) THEN
C        Error Message: Not Enough Fields
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 6) THEN
C        Error Message: Too Many Fields
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Retrieve Averaging Period
      IF (FIELD(3) .EQ. 'MONTH' .AND. MONTH) THEN
C        Set Value of IPRDT = 720 for MONTHly Averages
         IPRDT = 720
      ELSE
         CALL STONUM(FIELD(3),ILEN_FLD,FNUM,IMIT)
         IF (IMIT .NE. 1) THEN
C           Write Error Message:Invalid Numerical Field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 999
         END IF
         IPRDT = NINT(FNUM)
      END IF

C     Check Averaging Period Against KAVE Array
      FOUND = .FALSE.
      INDAVE = 0
      J = 1
      DO WHILE (.NOT.FOUND .AND. J.LE.NUMAVE)
         IF (IPRDT .EQ. KAVE(J)) THEN
            FOUND = .TRUE.
            INDAVE = J
         END IF
         J = J + 1
      END DO
      IF (.NOT. FOUND) THEN
C        Error Message:E203 AVEPER Not Match With Pre-Defined One
         CALL ERRHDL(PATH,MODNAM,'E','203','AVEPER')
         GO TO 999
      END IF

C     Check for Averaging Period Other Than 1-HOUR, and Write Warning
      IF (IPRDT .NE. 1) THEN
         WRITE(DUMMY,'(2X,I4,2X)') IPRDT
         CALL ERRHDL(PATH,MODNAM,'W','296',DUMMY)
      END IF

C     Set Switch and Check for Previous TOXXFILE Card
C     for This Averaging Period
      ITOXFL(INDAVE) = ITOXFL(INDAVE) + 1
      IF (ITOXFL(INDAVE) .GT. 1) THEN
C        WRITE Error Message
         CALL ERRHDL(PATH,MODNAM,'E','211',KEYWRD)
         GO TO 999
      END IF

C     Retrieve Threshold
      CALL STODBL(FIELD(4),ILEN_FLD,DNUM,IMIT)
C     Check for Valid Threshold Value
      IF (IMIT .NE. 1) THEN
C        Write Error Message:Invalid Numerical Field
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      END IF
      TOXTHR(INDAVE) = DNUM

      IF ((LOCE(5)-LOCB(5)) .LE. (ILEN_FLD - 1) ) THEN
C        Retrieve Filename as Character Substring to Maintain Original Case
C        Also Check for Filename Larger Than ILEN_FLD Characters
         TOXFIL(INDAVE) = RUNST1(LOCB(5):LOCE(5))
      ELSE
C        WRITE Error Message:  TOXFIL Field is Too Long
         WRITE(DUMMY,'(I8)') ILEN_FLD
         CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
      END IF

C     Retrieve File Unit If Input, or Assign File Unit and OPEN File
      IF (IFC .EQ. 6) THEN
         CALL STONUM(FIELD(6),ILEN_FLD,FNUM,IMIT)
C        Check for Valid Threshold Value
         IF (IMIT .NE. 1) THEN
C           Write Error Message:Invalid Numerical Field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 999
         END IF
C        Check for Conflict With System Files
         IF (NINT(FNUM) .LE. 30) THEN
C           WRITE Error Message:  Invalid File Unit Specified
            CALL ERRHDL(PATH,MODNAM,'E','560',KEYWRD)
            GO TO 999
         ELSE IF (NINT(FNUM) .GT. 100) THEN
C           WRITE Warning Message:  Suspect File Unit Specified
C           Unit May Conflict With Dynamically Allocated File Units
            CALL ERRHDL(PATH,MODNAM,'W','565',KEYWRD)
            ITXUNT(INDAVE) = NINT(FNUM)
         ELSE
            ITXUNT(INDAVE) = NINT(FNUM)
         END IF
      ELSE
C        Dynamically Allocate File Unit (300's)
         ITXUNT(INDAVE) = 300 + INDAVE
         IF (INDAVE .GE. 5) THEN
C           WRITE Warning Message: Dynamic FUnit Allocation May Have Conflict
            CALL ERRHDL(PATH,MODNAM,'W','565',KEYWRD)
         END IF
      END IF

C     Check for Earlier Use of This Filename and File Unit
      FOUND = .FALSE.
      DO I = 1, NUMAVE
         IF (I .NE. INDAVE) THEN
            IF (TOXFIL(INDAVE) .EQ. TOXFIL(I) .AND.
     &          ITXUNT(INDAVE) .EQ. ITXUNT(I)) THEN
               FOUND = .TRUE.
            ELSE IF (TOXFIL(INDAVE) .EQ. TOXFIL(I) .AND.
     &               ITXUNT(INDAVE) .NE. ITXUNT(I)) THEN
C              Write Error Message: Conflicting Inputs
               CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
               GO TO 999
            ELSE IF (TOXFIL(INDAVE) .NE. TOXFIL(I) .AND.
     &               ITXUNT(INDAVE) .EQ. ITXUNT(I)) THEN
C              Write Error Message: Conflicting Inputs
               CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
               GO TO 999
            END IF
         END IF
      END DO

      IF (.NOT. FOUND) THEN
C        First Time File is Identified - OPEN File
         OPEN(ITXUNT(INDAVE),ERR=99,FILE=TOXFIL(INDAVE),
     &        FORM='UNFORMATTED',IOSTAT=IOERRN,STATUS='REPLACE')
      END IF

C     Write Header to File
      IF (RUN) THEN
         NUMPER = NHOURS/IPRDT
C        Write Header Records (BUF12 is used to fill out 80-character title)
         WRITE(ITXUNT(INDAVE)) TITLE1(1:68), BUF12
         WRITE(ITXUNT(INDAVE)) ISYEAR, NUMGRP, NUMREC, NUMPER, ITAB,
     &                         NXTOX, NYTOX, (IDUM,I=1,NIDUM)
         WRITE(ITXUNT(INDAVE)) TOXTHR(INDAVE), (RDUM,I=1,NRDUM)
      END IF

      GO TO 999

C     WRITE Error Message for Error Opening File
 99   WRITE(DUMMY,'("TOXFL",I3.3)') ITXUNT(INDAVE)
      CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

 999  CONTINUE

      RETURN
      END

      SUBROUTINE OUSEAS
C***********************************************************************
C                 OUSEAS Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: To Process Season by Hour Output Selection
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    June 5, 1997
C
C        INPUTS:  Input Runstream Parameters
C
C        OUTPUTS: Output Option Switches
C
C        CALLED FROM:   OUCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, J
      CHARACTER INPGRP*8
      LOGICAL FOUND

C     Variable Initializations
      MODNAM = 'OUSEAS'

C     Set Logical Switch Indicating That Plot File(s) Are Generated
      SEASONHR = .TRUE.

C     Check If Enough Fields
      IF (IFC .EQ. 2) THEN
C        Error Message: No Fields
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 4) THEN
C        Error Message: Not Enough Fields
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 5) THEN
C        Error Message: Too Many Fields
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Retrieve Source Group ID
      INPGRP = FIELD(3)
C     Check Source Group ID
      FOUND = .FALSE.
      INDGRP = 0
      J = 1
      DO WHILE (.NOT.FOUND .AND. J.LE.NUMGRP)
         IF (INPGRP .EQ. GRPID(J)) THEN
            FOUND = .TRUE.
            INDGRP = J
         END IF
         J = J + 1
      END DO
      IF (.NOT. FOUND) THEN
C        Error Message: E203 GRPID Not Match With Pre-Defined One
         CALL ERRHDL(PATH,MODNAM,'E','203','GRPID')
         GO TO 999
      END IF

C     Set Switch and Check for Previous SEASONHR Card
C     for This Group ID
      ISEAHR(INDGRP) = ISEAHR(INDGRP) + 1
      IF (ISEAHR(INDGRP) .GT. 1) THEN
C        WRITE Error Message
         CALL ERRHDL(PATH,MODNAM,'E','211',KEYWRD)
         GO TO 999
      END IF

      IF ((LOCE(4)-LOCB(4)) .LE. (ILEN_FLD - 1) ) THEN
C        Retrieve Filename as Character Substring to Maintain Original Case
C        Also Check for Filename Larger Than ILEN_FLD Characters
         SEAHRS(INDGRP) = RUNST1(LOCB(4):LOCE(4))
      ELSE
C        WRITE Error Message:  SEAHRS Field is Too Long
         WRITE(DUMMY,'(I8)') ILEN_FLD
         CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
      END IF

C     Retrieve File Unit If Input, or Assign File Unit and OPEN File
      IF (IFC .EQ. 5) THEN
         CALL STONUM(FIELD(5),ILEN_FLD,FNUM,IMIT)
C        Check for Valid Threshold Value
         IF (IMIT .NE. 1) THEN
C           Write Error Message: Invalid Numerical Field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 999
         END IF
C        Check for Conflict With System Files
         IF (NINT(FNUM) .LE. 30) THEN
C           WRITE Error Message:  Invalid File Unit Specified
            CALL ERRHDL(PATH,MODNAM,'E','560',KEYWRD)
            GO TO 999
         ELSE IF (NINT(FNUM) .GT. 100) THEN
C           WRITE Warning Message:  Suspect File Unit Specified
C           Unit May Conflict With Dynamically Allocated File Units
            CALL ERRHDL(PATH,MODNAM,'W','565',KEYWRD)
            ISHUNT(INDGRP) = NINT(FNUM)
         ELSE
            ISHUNT(INDGRP) = NINT(FNUM)
         END IF
      ELSE
C        Dynamically Allocate File Unit (300's)
         ISHUNT(INDGRP) = 302 + INDGRP*10
         IF (INDGRP .GE. 10) THEN
C           WRITE Warning Message: Dynamic FUnit Allocation May Have Conflict
            CALL ERRHDL(PATH,MODNAM,'W','565',KEYWRD)
         END IF
      END IF

C     Check for Earlier Use of This Filename and File Unit
      FOUND = .FALSE.
      DO I = 1, NUMGRP
         IF (I .NE. INDGRP) THEN
            IF (SEAHRS(INDGRP) .EQ. SEAHRS(I) .AND.
     &          ISHUNT(INDGRP) .EQ. ISHUNT(I)) THEN
              FOUND = .TRUE.
            ELSE IF (SEAHRS(INDGRP) .EQ. SEAHRS(I) .AND.
     &               ISHUNT(INDGRP) .NE. ISHUNT(I)) THEN
C             Write Error Message: Conflicting Inputs
              CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
              GO TO 999
            ELSE IF (SEAHRS(INDGRP) .NE. SEAHRS(I) .AND.
     &               ISHUNT(INDGRP) .EQ. ISHUNT(I)) THEN
C             Write Error Message: Conflicting Inputs
              CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
              GO TO 999
            END IF
         END IF
      END DO

      IF (.NOT. FOUND) THEN
C        First Time File is Identified - OPEN File
         OPEN(ISHUNT(INDGRP),ERR=99,FILE=SEAHRS(INDGRP),
     &        IOSTAT=IOERRN,STATUS='REPLACE')
      END IF

      GO TO 999

C     WRITE Error Message for Error Opening File
 99   WRITE(DUMMY,'("SEAHR",I3.3)') ISHUNT(INDGRP)
      CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

 999  RETURN
      END

      SUBROUTINE OURANK
C***********************************************************************
C                 OURANK Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: To Process RANKFILE Output Selections
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    November 29, 1993
C
C        INPUTS:  Input Runstream Parameters
C
C        OUTPUTS: Output Option Switches
C
C        CALLED FROM:   OUCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, J, IPRDT
      LOGICAL FOUND

C     Variable Initializations
      MODNAM = 'OURANK'

C     Set Logical Switch Indicating That Ranked Value File(s) Are Generated
      RKFILE = .TRUE.

C     Check If Enough Fields
      IF (IFC .EQ. 2) THEN
C        Error Message: No Fields
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 5) THEN
C        Error Message: Not Enough Fields
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 6) THEN
C        Error Message: Too Many Fields
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Retrieve Averaging Period
      IF (FIELD(3) .EQ. 'MONTH' .AND. MONTH) THEN
C        Set Value of IPRDT = 720 for MONTHly Averages
         IPRDT = 720
      ELSE
         CALL STONUM(FIELD(3),ILEN_FLD,FNUM,IMIT)
         IF (IMIT .NE. 1) THEN
C           Write Error Message:Invalid Numerical Field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 999
         END IF
         IPRDT = NINT(FNUM)
      END IF

C     Check Averaging Period Against KAVE Array
      FOUND = .FALSE.
      INDAVE = 0
      J = 1
      DO WHILE (.NOT.FOUND .AND. J.LE.NUMAVE)
         IF (IPRDT .EQ. KAVE(J)) THEN
            FOUND = .TRUE.
            INDAVE = J
         END IF
         J = J + 1
      END DO
      IF (.NOT. FOUND) THEN
C        Error Message:E203 AVEPER Not Match With Pre-Defined One
         CALL ERRHDL(PATH,MODNAM,'E','203','AVEPER')
         GO TO 999
      END IF

C     Check for MAXTABLE Option for this INDAVE; Then Set Switch and
C     Check for Previous RANKFILE Card for This Averaging Period
      IF (IRNKFL(INDAVE) .EQ. 0) THEN
          IRNKFL(INDAVE) = IRNKFL(INDAVE) + 1
      ELSE
C        Error Message:E203 AVEPER Not Match With MAXTABLE Options
         CALL ERRHDL(PATH,MODNAM,'E','203','AVEPER')
         GO TO 999
      END IF
      IF (IRNKFL(INDAVE) .GT. 1) THEN
C        WRITE Error Message
         CALL ERRHDL(PATH,MODNAM,'E','211',KEYWRD)
         GO TO 999
      END IF

C     Retrieve Rank Number (number of values to output)
      CALL STONUM(FIELD(4),ILEN_FLD,FNUM,IMIT)
C     Check for Valid Threshold Value
      IF (IMIT .NE. 1) THEN
C        Write Error Message:Invalid Numerical Field
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      END IF
      INUM = NINT(FNUM)
      IRKVAL(INDAVE) = INUM

      IF ((LOCE(5)-LOCB(5)) .LE. (ILEN_FLD - 1) ) THEN
C        Retrieve Filename as Character Substring to Maintain Original Case
C        Also Check for Filename Larger Than ILEN_FLD Characters
         RNKFIL(INDAVE) = RUNST1(LOCB(5):LOCE(5))
      ELSE
C        WRITE Error Message:  RNKFIL Field is Too Long
         WRITE(DUMMY,'(I8)') ILEN_FLD
         CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
      END IF

C     Retrieve File Unit If Input, or Assign File Unit and OPEN File
      IF (IFC .EQ. 6) THEN
         CALL STONUM(FIELD(6),ILEN_FLD,FNUM,IMIT)
C        Check for Valid Threshold Value
         IF (IMIT .NE. 1) THEN
C           Write Error Message:Invalid Numerical Field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 999
         END IF
C        Check for Conflict With System Files
         IF (NINT(FNUM) .LE. 30) THEN
C           WRITE Error Message:  Invalid File Unit Specified
            CALL ERRHDL(PATH,MODNAM,'E','560',KEYWRD)
            GO TO 999
         ELSE IF (NINT(FNUM) .GT. 100) THEN
C           WRITE Warning Message:  Suspect File Unit Specified
C           Unit May Conflict With Dynamically Allocated File Units
            CALL ERRHDL(PATH,MODNAM,'W','565',KEYWRD)
            IRKUNT(INDAVE) = NINT(FNUM)
         ELSE
            IRKUNT(INDAVE) = NINT(FNUM)
         END IF
      ELSE
C        Dynamically Allocate File Unit (100's)
         IRKUNT(INDAVE) = 100 + INDAVE
      END IF

C     Check for Earlier Use of This Filename and File Unit
      FOUND = .FALSE.
      DO I = 1, NUMAVE
         IF (I .NE. INDAVE) THEN
            IF (RNKFIL(INDAVE) .EQ. RNKFIL(I) .AND.
     &          IRKUNT(INDAVE) .EQ. IRKUNT(I)) THEN
               FOUND = .TRUE.
            ELSE IF (RNKFIL(INDAVE) .EQ. RNKFIL(I) .AND.
     &               IRKUNT(INDAVE) .NE. IRKUNT(I)) THEN
C              Write Error Message: Conflicting Inputs
               CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
               GO TO 999
            ELSE IF (RNKFIL(INDAVE) .NE. RNKFIL(I) .AND.
     &               IRKUNT(INDAVE) .EQ. IRKUNT(I)) THEN
C              Write Error Message: Conflicting Inputs
               CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
               GO TO 999
            END IF
         END IF
      END DO

      IF (.NOT. FOUND) THEN
C        First Time File is Identified - OPEN File
         OPEN(IRKUNT(INDAVE),ERR=99,FILE=RNKFIL(INDAVE),
     &        FORM='FORMATTED',IOSTAT=IOERRN,STATUS='REPLACE')
      END IF

      GO TO 999

C     WRITE Error Message for Error Opening File
 99   WRITE(DUMMY,'("RNKFL",I3.3)') IRKUNT(INDAVE)
      CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

 999  CONTINUE

      RETURN
      END

      SUBROUTINE OUEVAL
C***********************************************************************
C                 OUEVAL Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: To Process EVALFILE Output Selections
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    November 29, 1993
C
C        INPUTS:  Input Runstream Parameters
C
C        OUTPUTS: Output Option Switches
C
C        CALLED FROM:   OUCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, J, INDSRC
      CHARACTER INPSRC*8
      LOGICAL FOUND

C     Variable Initializations
      MODNAM  = 'OUEVAL'
      INDSRC  = 0
      EVALFIL = .TRUE.

C     Check If Enough Fields
      IF (IFC .EQ. 2) THEN
C        Error Message: No Fields
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         RETURN
      ELSE IF (IFC .LT. 4) THEN
C        Error Message: Not Enough Fields
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         RETURN
      ELSE IF (IFC .GT. 5) THEN
C        Error Message: Too Many Fields
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         RETURN
      END IF

C     Retrieve Source Group ID
      INPSRC = FIELD(3)
C     Check Source ID
      FOUND = .FALSE.
      INDSRC = 0
      J = 1
      DO WHILE (.NOT.FOUND .AND. J.LE.NUMSRC)
         IF (INPSRC .EQ. SRCID(J)) THEN
            FOUND = .TRUE.
            INDSRC = J
         END IF
         J = J + 1
      END DO
      IF (.NOT. FOUND) THEN
C        Error Message: E203 SRCID Not Match With Pre-Defined One
         CALL ERRHDL(PATH,MODNAM,'E','203','SRCID')
         RETURN
      ELSE
C        Set Logical Switch Indicating That EVALFILE is Generated for this Source
         EVAL(INDSRC) = .TRUE.
      END IF

      IF ((LOCE(4)-LOCB(4)) .LE. (ILEN_FLD - 1) ) THEN
C        Retrieve Filename as Character Substring to Maintain Original Case
C        Also Check for Filename Larger Than ILEN_FLD Characters
         EVLFIL(INDSRC) = RUNST1(LOCB(4):LOCE(4))
      ELSE
C        WRITE Error Message:  EVLFIL Field is Too Long
         WRITE(DUMMY,'(I8)') ILEN_FLD
         CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
      END IF

C     Retrieve File Unit If Input, or Assign File Unit and OPEN File
      IF (IFC .EQ. 5) THEN
         CALL STONUM(FIELD(5),ILEN_FLD,FNUM,IMIT)
C        Check for Valid Threshold Value
         IF (IMIT .NE. 1) THEN
C           Write Error Message:Invalid Numerical Field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            RETURN
         END IF
C        Check for Conflict With System Files
         IF (NINT(FNUM) .LE. 30) THEN
C           WRITE Error Message:  Invalid File Unit Specified
            CALL ERRHDL(PATH,MODNAM,'E','560',KEYWRD)
            RETURN
         ELSE IF (NINT(FNUM) .GT. 100) THEN
C           WRITE Warning Message:  Suspect File Unit Specified
C           Unit May Conflict With Dynamically Allocated File Units
            CALL ERRHDL(PATH,MODNAM,'W','565',KEYWRD)
            IELUNT(INDSRC) = NINT(FNUM)
         ELSE
            IELUNT(INDSRC) = NINT(FNUM)
         END IF
      ELSE
C        Dynamically Allocate File Unit
         IELUNT(INDSRC) = 400 + INDSRC*5
      END IF

C     Check for Earlier Use of This Filename and File Unit
      FOUND = .FALSE.
      DO I = 1, NUMSRC
         IF (I .NE. INDSRC) THEN
            IF (EVLFIL(INDSRC) .EQ. EVLFIL(I) .AND.
     &          IELUNT(INDSRC) .EQ. IELUNT(I)) THEN
               FOUND = .TRUE.
            ELSE IF (EVLFIL(INDSRC) .EQ. EVLFIL(I) .AND.
     &               IELUNT(INDSRC) .NE. IELUNT(I)) THEN
C              Write Error Message: Conflicting Inputs
               CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
               EXIT
            ELSE IF (EVLFIL(INDSRC) .NE. EVLFIL(I) .AND.
     &               IELUNT(INDSRC) .EQ. IELUNT(I)) THEN
C              Write Error Message: Conflicting Inputs
               CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
               EXIT
            END IF
         END IF
      END DO

      IF (.NOT. FOUND) THEN
C        First Time File is Identified - OPEN File
         OPEN(IELUNT(INDSRC),ERR=99,FILE=EVLFIL(INDSRC),
     &        FORM='FORMATTED',IOSTAT=IOERRN,STATUS='REPLACE')
      END IF

      GO TO 999

C     WRITE Error Message for Error Opening File
 99   WRITE(DUMMY,'("EVLFL",I3.3)') IELUNT(INDSRC)
      CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

 999  CONTINUE

      RETURN
      END

      SUBROUTINE OUSUMM
C***********************************************************************
C                 OUSUMM Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: To Process SUMMFILE Output Selections
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    October 19, 2009
C
C        INPUTS:  Input Runstream Parameters
C
C        OUTPUTS: Output Option Switches
C
C        CALLED FROM:   OUCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'OUSUMM'

C     Set Logical Switch Indicating That SUMMFILE is Generated 
      SUMMFILE = .TRUE.

C     Check If Enough Fields
      IF (IFC .EQ. 2) THEN
C        Error Message: No Fields
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 3) THEN
C        Error Message: Not Enough Fields
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 3) THEN
C        Error Message: Too Many Fields
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

      IF ((LOCE(3)-LOCB(3)) .LE. (ILEN_FLD - 1) ) THEN
C        Retrieve Filename as Character Substring to Maintain Original Case
C        Also Check for Filename Larger Than ILEN_FLD Characters
         SUMFIL = RUNST1(LOCB(3):LOCE(3))
C        Output SUMMFILE name is too long
      ELSE
         WRITE(DUMMY,'(I8)') ILEN_FLD
         CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
         GO TO 999
      END IF

C     Open SUMMFILE
      OPEN(ISUMUNT,ERR=99,FILE=SUMFIL,FORM='FORMATTED',
     &     IOSTAT=IOERRN,STATUS='REPLACE')

      GO TO 999

C     WRITE Error Message for Error Opening File
 99   CALL ERRHDL(PATH,MODNAM,'E','500','SUMMFILE')

 999  RETURN
      END

      SUBROUTINE FILEFORM
C***********************************************************************
C                 FILEFORM Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: To Process MXFORM Output Selections
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    October 19, 2009
C
C        INPUTS:  Input Runstream Parameters
C
C        OUTPUTS: Output Option Switches
C
C        CALLED FROM:   OUCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'FILEFORM'

C     Check If Enough Fields
      IF (IFC .EQ. 2) THEN
C        Error Message: No Fields
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 3) THEN
C        Error Message: Not Enough Fields
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 3) THEN
C        Error Message: Too Many Fields
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

      IF (FIELD(3)(1:3) .EQ. 'FIX') THEN
C        Output file formats will use FIXed format
         FILE_FORMAT = 'FIX'
      ELSE IF (FIELD(3)(1:3) .EQ. 'EXP') THEN
C        Output file formats will use EXPonential format
         FILE_FORMAT = 'EXP'

         IF (NUMTYP .EQ. 1) THEN
C ---       Mofify affected formats to use EXPonential format;
C           NOTE: Modifications to PLTFRM, PSTFRM and MXDFRM for multiple
C           output types (CONC, DEPOS, etc.) are made in SUBROUTINE MOPOPS.
C           SEASONHR format is assigned in subroutine SHOUT.
            THRFRM = 
     &           '(1X,I3,1X,A8,1X,I8.8,2(1X,F13.5),3(1X,F7.2),1X,E13.6)'
            PSTFRM = 
     &     '(2(1X,F13.5),1X,E13.6,3(1X,F8.2),2X,A6,2X,A8,2X,I8.8,2X,A8)'
            IF (PM25AVE .OR. NO2AVE .OR. SO2AVE) THEN
               PLTFRM =
     &     '(2(1X,F13.5),1X,E13.6,3(1X,F8.2),2X,A6,2X,A8,2X,A5,5X,A8,2X,
     &10(E13.6,2X,I8.8,2X:))'
            ELSE
               PLTFRM =
     & '(2(1X,F13.5),1X,E13.6,3(1X,F8.2),3X,A5,2X,A8,2X,A5,5X,A8,2X,I8)'
            END IF
            RNKFRM = 
     &           '(1X,I6,1X,E13.6,1X,I8.8,2(1X,F13.5),3(1X,F7.2),2X,A8)'
            MXDFRM = 
     &     '(2(1X,F13.5),1X,E13.6,3(1X,F8.2),2X,A6,2X,A8,2X,I4,2X,I3,2X,
     &I8.8,2X,A8)'
         END IF

      ELSE
C        Write Error Message:  Invalid format specified
         CALL ERRHDL(PATH,MODNAM,'E','203',' FORMAT ')
      END IF

  999 RETURN
      END

      SUBROUTINE OUMAXDLY
C***********************************************************************
C                OUMAXDLY Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: To Process Maximum Daily 1-hour Output Selection
C
C        PROGRAMMER: Roger Brode
C
C        DATE:       February 28, 2011
C
C        INPUTS:  Input Runstream Parameters
C
C        OUTPUTS: Output Option Switches
C
C        CALLED FROM:   OUCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, J, IDAT, IDAT8, ISTR, ISTP
      INTEGER :: IRSYR, IRSDATE
      CHARACTER INPGRP*8, HDRFRM*400
      CHARACTER (LEN = ILEN_FLD) :: BUFFER
      LOGICAL :: FOUND, L_EXISTS
C Unused: INTEGER :: IPRDT

C     Variable Initializations
      MODNAM = 'OUMAXDLY'
      L_EXISTS = .TRUE.
      BUFFER = ' '
      HDRFRM = ' '
      IDAT   = 0
      IDAT8  = 0

C     Set Logical Switch Indicating That MAXDAILY File(s) Are Generated
      MXDAILY = .TRUE.

C     Create Header Format for Columns
      WRITE(HDRFRM,9020) NUMTYP, NUMTYP+2

C     Check If Enough Fields
      IF (IFC .EQ. 2) THEN
C        Error Message: No Fields
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 4) THEN
C        Error Message: Not Enough Fields
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 5) THEN
C        Error Message: Too Many Fields
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Retrieve Source Group ID
      INPGRP = FIELD(3)
C     Check Source Group ID
      FOUND = .FALSE.
      INDGRP = 0
      J = 1
      DO WHILE (.NOT.FOUND .AND. J.LE.NUMGRP)
         IF (INPGRP .EQ. GRPID(J)) THEN
            FOUND = .TRUE.
            INDGRP = J
         END IF
         J = J + 1
      END DO
      IF (.NOT. FOUND) THEN
C        Error Message: E203 GRPID Not Match With Pre-Defined One
         CALL ERRHDL(PATH,MODNAM,'E','203','GRPID')
         GO TO 999
      END IF

C     Set Switch and Check for Previous MAXDAILY Card
C     for This Group ID
      IMXDLY(INDGRP) = IMXDLY(INDGRP) + 1
      IF (IMXDLY(INDGRP) .GT. 1) THEN
C        WRITE Error Message
         CALL ERRHDL(PATH,MODNAM,'E','211',KEYWRD)
         GO TO 999
      END IF
      
      IF ((LOCE(4)-LOCB(4)) .LE. (ILEN_FLD - 1) ) THEN
C        Retrieve Filename as Character Substring to Maintain Original Case
C        Also Check for Filename Larger Than ILEN_FLD Characters
         MAXDLY(INDGRP) = RUNST1(LOCB(4):LOCE(4))
      ELSE
C        WRITE Error Message:  MAXDLY Field is Too Long
         WRITE(DUMMY,'(I8)') ILEN_FLD
         CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
      END IF

C     Retrieve File Unit If Input, or Assign File Unit and OPEN File
      IF (IFC .EQ. 5) THEN
         CALL STONUM(FIELD(5),ILEN_FLD,FNUM,IMIT)
C        Check for Valid File Unit Value
         IF (IMIT .NE. 1) THEN
C           Write Error Message: Invalid Numerical Field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 999
         END IF
C        Check for Conflict With System Files
         IF (NINT(FNUM) .LE. 30) THEN
C           WRITE Error Message:  Invalid File Unit Specified
            CALL ERRHDL(PATH,MODNAM,'E','560',KEYWRD)
            GO TO 999
         ELSE IF (NINT(FNUM) .GT. 100) THEN
C           WRITE Warning Message:  Suspect File Unit Specified
C           Unit May Conflict With Dynamically Allocated File Units
            CALL ERRHDL(PATH,MODNAM,'W','565',KEYWRD)
            IMDUNT(INDGRP) = NINT(FNUM)
         ELSE
            IMDUNT(INDGRP) = NINT(FNUM)
         END IF
      ELSE
C        Dynamically Allocate File Unit (500's)
         IMDUNT(INDGRP) = 501 + (INDGRP-1)*2
         IF (INDGRP .GE. 10) THEN
C           WRITE Warning Message: Dynamic FUnit Allocation May Have Conflict
            CALL ERRHDL(PATH,MODNAM,'W','565',KEYWRD)
         END IF
      END IF

C     Check for Earlier Use of This Filename and File Unit
      FOUND = .FALSE.
      DO I = 1, NUMGRP
         IF (I .NE. INDGRP) THEN
            IF (MAXDLY(INDGRP) .EQ. MAXDLY(I) .AND.
     &          IMDUNT(INDGRP) .EQ. IMDUNT(I)) THEN
              FOUND = .TRUE.
            ELSE IF (MAXDLY(INDGRP) .EQ. MAXDLY(I) .AND.
     &               IMDUNT(INDGRP) .NE. IMDUNT(I)) THEN
C             Write Error Message: Conflicting Inputs
              CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
              GO TO 999
            ELSE IF (MAXDLY(INDGRP) .NE. MAXDLY(I) .AND.
     &               IMDUNT(INDGRP) .EQ. IMDUNT(I)) THEN
C             Write Error Message: Conflicting Inputs
              CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
              GO TO 999
            END IF
         END IF
      END DO

      IF (.NOT. FOUND) THEN
C        First Time File is Identified; check for re-start option (RSTINP)
C        before OPENing File
         IF (RSTINP) THEN
C           Results Arrays Are To Be Initialized From Re-start File.
C           Check for existence of file first, then 
C           Read Start Date From File and Rewind.
            INQUIRE (FILE=MAXDLY(INDGRP),EXIST=L_EXISTS)
            DUMMY = 'MAXDAILY'
            IF (L_EXISTS) THEN
               OPEN(IMDUNT(INDGRP),ERR=99, FILE=MAXDLY(INDGRP),
     &              IOSTAT=IOERRN,FORM='FORMATTED',STATUS='OLD')
            ELSE
C              File does not exist; restart option will not work
               CALL ERRHDL(PATH,MODNAM,'E','585',DUMMY)
               RETURN
            END IF
C           Results Arrays Are To Be Initialized From Re-start File.
C           Read Start Date From File, IRSDATE, and Rewind.
            DUMMY = 'INITFILE'
            READ(IRSUNT,ERR=919,END=919) IRSDATE
            REWIND IRSUNT
C           Now Position MAXDAILY To End of File, But Not Past IRSDATE.
            DUMMY = 'MAXDAILY'
            EOF = .FALSE.
            DO WHILE (.NOT. EOF)
               READ(IMDUNT(INDGRP),'(A:)', ERR=919,END=299) BUFFER
               IF (BUFFER(1:1) .NE. '*') THEN
C                 Record Is Not Part of Header - Read Date For This Record
C                 First calculate start & end of date string based on NUMTYP
                  ISTR = 101 
                  ISTP = ISTR + 7
                  READ(BUFFER(ISTR:ISTP),'(I8)',ERR=919) IDAT8
C                 Convert 8-digit IDAT8 to 10-digit IDAT for comparison to IRSDATE
                  IDAT8 = 100*(IDAT8/100)
                  IRSYR = IDAT8/1000000
                  IF (IRSYR .GE. ISTRT_WIND .AND. IRSYR .LE. 99) THEN
                     IRSYR = ISTRT_CENT*100 + IRSYR
                     IDAT  = ISTRT_CENT*100000000 + IDAT8
                  ELSE IF (IRSYR .LT. ISTRT_WIND) THEN
                     IRSYR = (ISTRT_CENT+1)*100 + IRSYR
                     IDAT  = (ISTRT_CENT+1)*100000000 + IDAT8
                  END IF
               ELSE
C                 Header record - cycle to next record
                  CYCLE
               END IF
               IF (IDAT .GT. IRSDATE) THEN
C                 Date of MAXDAILY Record Is Greater Than Start Date
C                 From Save File.  Treat As End of File to Exit Loop.
                  IF (MULTYR) THEN
C ---                If this is a MULTYEAR run, then MAXDAILY date should not
C                    be greater than IRSDATE; issue error message
                     WRITE(DUMMY,'(I8.8)') IDAT8
                     CALL ERRHDL(PATH,MODNAM,'E','593',DUMMY)
                  END IF
                  GO TO 299
               END IF
               GO TO 21
 299           EOF = .TRUE.
 21            CONTINUE
            END DO
            EOF = .FALSE.
C           End of file or IRSDATE has been passed; backspace file
            BACKSPACE IMDUNT(INDGRP)
C           Skip Header Records
            GO TO 999
         ELSE
C ---       This is not a restarted run; just open the file
            OPEN(IMDUNT(INDGRP),ERR=99, FILE=MAXDLY(INDGRP),
     &           IOSTAT=IOERRN,FORM='FORMATTED',STATUS='REPLACE')
         END IF
      ELSE IF (FOUND .AND. RSTINP) THEN
C        This file is already open, and this run is a
C        Re-start from an earlier run
C        Skip Header Records
         GO TO 999
      END IF

C     Write Header to File.  Skip Header if FATAL.
      IF (RUN .AND. .NOT.FATAL .AND. .NOT.L_NoHeader(6)) THEN
         WRITE(IMDUNT(INDGRP),9005) VERSN, TITLE1(1:68), RUNDAT
 9005    FORMAT('* AERMOD (',A6,'): ',A68,5X,A8)
         WRITE(IMDUNT(INDGRP),9007) C_METVER, RUNTIM, 
     &                          MODOPS_String(1:LEN_TRIM(MODOPS_String))
 9007    FORMAT('* AERMET (',A6,'):',T93,A8,
     &         /'* MODELING OPTIONS USED: ',A:)
         WRITE(IMDUNT(INDGRP),9010) GRPID(INDGRP), NUMREC, 
     &                                        MXDFRM(1:LEN_TRIM(MXDFRM))
 9010    FORMAT('*',9X,'MAXDAILY FILE OF DAILY MAXIMUM 1-HR VALUES',
     &         ' BY DAY FOR SOURCE GROUP: ',A8,
     &         /'*',9X,'FOR A TOTAL OF ',I5,' RECEPTORS.',
     &         /'*',9X,'FORMAT: ',A:)
         WRITE(IMDUNT(INDGRP),HDRFRM) (CHIDEP(1,ITYP),
     &                   CHIDEP(2,ITYP), CHIDEP(3,ITYP), ITYP=1,NUMTYP)
 9020 FORMAT('(''*'',8X,''X'',13X,''Y'',4X,',I1,'(2X,3A4),4X,''ZELEV'',
     &  4X,''ZHILL'',4X,''ZFLAG'',4X,''AVE'',5X,''GRP'',5X,''JDAY'',
     &  3X,''HR'',4X,''DATE'',5X,''NET ID'',
     &  /,''*'',',I1,'(1X,''____________ ''),1X,
     & 3('' ______  ''),''______  ________  ____  ___  ________'',
     &  ''  ________'')')

      END IF

      GO TO 999

C     WRITE Error Message for Error Opening File
 99   WRITE(DUMMY,'("MXDLY",I3.3)') IMDUNT(INDGRP)
      CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

      GO TO 999

C     WRITE Error Message for Error Reading File
 919  CALL ERRHDL(PATH,MODNAM,'E','510',DUMMY)

 999  RETURN
      END

      SUBROUTINE OUMXDLY_BYYR
C***********************************************************************
C                OUMAXDLY Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: To Process Maximum Daily 1-hour By Year Output Selection
C
C        PROGRAMMER: Roger Brode
C
C        DATE:       February 28, 2011
C
C        INPUTS:  Input Runstream Parameters
C
C        OUTPUTS: Output Option Switches
C
C        CALLED FROM:   OUCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, J, IDAT, IDAT8, ISTR, ISTP
      INTEGER :: IRSYR, IRSDATE
      CHARACTER INPGRP*8, HDRFRM*400
      CHARACTER (LEN = ILEN_FLD) :: BUFFER
      LOGICAL :: FOUND, L_EXISTS
C Unused: INTEGER :: IPRDT

C     Variable Initializations
      MODNAM = 'OUMXDLY_BYYR'
      L_EXISTS = .TRUE.
      BUFFER = ' '
      HDRFRM = ' '
      IDAT   = 0
      IDAT8  = 0

C     Set Logical Switch Indicating That MXDYBYYR File(s) Are Generated
      MXDAILY_BYYR = .TRUE.

C     Create Header Format for Columns
      WRITE(HDRFRM,9020) NUMTYP, NUMTYP+2

C     Check If Enough Fields
      IF (IFC .EQ. 2) THEN
C        Error Message: No Fields
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 4) THEN
C        Error Message: Not Enough Fields
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 5) THEN
C        Error Message: Too Many Fields
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Retrieve Source Group ID
      INPGRP = FIELD(3)
C     Check Source Group ID
      FOUND = .FALSE.
      INDGRP = 0
      J = 1
      DO WHILE (.NOT.FOUND .AND. J.LE.NUMGRP)
         IF (INPGRP .EQ. GRPID(J)) THEN
            FOUND = .TRUE.
            INDGRP = J
         END IF
         J = J + 1
      END DO
      IF (.NOT. FOUND) THEN
C        Error Message: E203 GRPID Not Match With Pre-Defined One
         CALL ERRHDL(PATH,MODNAM,'E','203','GRPID')
         GO TO 999
      END IF

C     Set Switch and Check for Previous MXDYBYYR Card
C     for This Group ID
      IMXDLY_BYYR(INDGRP) = IMXDLY_BYYR(INDGRP) + 1
      IF (IMXDLY_BYYR(INDGRP) .GT. 1) THEN
C        WRITE Error Message
         CALL ERRHDL(PATH,MODNAM,'E','211',KEYWRD)
         GO TO 999
      END IF
      
      IF ((LOCE(4)-LOCB(4)) .LE. (ILEN_FLD - 1) ) THEN
C        Retrieve Filename as Character Substring to Maintain Original Case
C        Also Check for Filename Larger Than ILEN_FLD Characters
         MAXDLY_BYYR(INDGRP) = RUNST1(LOCB(4):LOCE(4))
      ELSE
C        WRITE Error Message:  MAXDLY_BYYR Field is Too Long
         WRITE(DUMMY,'(I8)') ILEN_FLD
         CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
      END IF

C     Retrieve File Unit If Input, or Assign File Unit and OPEN File
      IF (IFC .EQ. 5) THEN
         CALL STONUM(FIELD(5),ILEN_FLD,FNUM,IMIT)
C        Check for Valid File Unit Value
         IF (IMIT .NE. 1) THEN
C           Write Error Message: Invalid Numerical Field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 999
         END IF
C        Check for Conflict With System Files
         IF (NINT(FNUM) .LE. 30) THEN
C           WRITE Error Message:  Invalid File Unit Specified
            CALL ERRHDL(PATH,MODNAM,'E','560',KEYWRD)
            GO TO 999
         ELSE IF (NINT(FNUM) .GT. 100) THEN
C           WRITE Warning Message:  Suspect File Unit Specified
C           Unit May Conflict With Dynamically Allocated File Units
            CALL ERRHDL(PATH,MODNAM,'W','565',KEYWRD)
            IMDUNT_BYYR(INDGRP) = NINT(FNUM)
         ELSE
            IMDUNT_BYYR(INDGRP) = NINT(FNUM)
         END IF
      ELSE
C        Dynamically Allocate File Unit (600's)
         IMDUNT_BYYR(INDGRP) = 601 + (INDGRP-1)*2
         IF (INDGRP .GE. 10) THEN
C           WRITE Warning Message: Dynamic FUnit Allocation May Have Conflict
            CALL ERRHDL(PATH,MODNAM,'W','565',KEYWRD)
         END IF
      END IF

C     Check for Earlier Use of This Filename and File Unit
      FOUND = .FALSE.
      DO I = 1, NUMGRP
         IF (I .NE. INDGRP) THEN
            IF (MAXDLY_BYYR(INDGRP) .EQ. MAXDLY_BYYR(I) .AND.
     &          IMDUNT_BYYR(INDGRP) .EQ. IMDUNT_BYYR(I)) THEN
              FOUND = .TRUE.
            ELSE IF (MAXDLY_BYYR(INDGRP) .EQ. MAXDLY_BYYR(I) .AND.
     &               IMDUNT_BYYR(INDGRP) .NE. IMDUNT_BYYR(I)) THEN
C             Write Error Message: Conflicting Inputs
              CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
              GO TO 999
            ELSE IF (MAXDLY_BYYR(INDGRP) .NE. MAXDLY_BYYR(I) .AND.
     &               IMDUNT_BYYR(INDGRP) .EQ. IMDUNT_BYYR(I)) THEN
C             Write Error Message: Conflicting Inputs
              CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
              GO TO 999
            END IF
         END IF
      END DO

      IF (.NOT. FOUND) THEN
C        First Time File is Identified; check for re-start option (RSTINP)
C        before OPENing File
         IF (RSTINP) THEN
C           Results Arrays Are To Be Initialized From Re-start File.
C           Check for existence of file first, then 
C           Read Start Date From File and Rewind.
            INQUIRE (FILE=MAXDLY_BYYR(INDGRP),EXIST=L_EXISTS)
            DUMMY = 'MXDYBYYR'
            IF (L_EXISTS) THEN
               OPEN(IMDUNT_BYYR(INDGRP),ERR=99,
     &              FILE=MAXDLY_BYYR(INDGRP),
     &              IOSTAT=IOERRN,FORM='FORMATTED',STATUS='OLD')
            ELSE
C              File does not exist; restart option will not work
               CALL ERRHDL(PATH,MODNAM,'E','585',DUMMY)
               RETURN
            END IF
C           Results Arrays Are To Be Initialized From Re-start File.
C           Read Start Date From File, IRSDATE, and Rewind.
            DUMMY = 'INITFILE'
            READ(IRSUNT,ERR=919,END=919) IRSDATE
            REWIND IRSUNT
C           Now Position MXDYBYYR To End of File, But Not Past IRSDATE.
            DUMMY = 'MXDYBYYR'
            EOF = .FALSE.
            DO WHILE (.NOT. EOF)
               READ(IMDUNT_BYYR(INDGRP),'(A:)', ERR=919,END=299) BUFFER
               IF (BUFFER(1:1) .NE. '*') THEN
C                 Record Is Not Part of Header - Read Date For This Record
C                 First calculate start & end of date string based on NUMTYP
                  ISTR = 101 
                  ISTP = ISTR + 7
                  READ(BUFFER(ISTR:ISTP),'(I8)',ERR=919) IDAT8
C                 Convert 8-digit IDAT8 to 10-digit IDAT for comparison to IRSDATE
                  IRSYR = IDAT8/1000000
                  IF (IRSYR .GE. ISTRT_WIND .AND. IRSYR .LE. 99) THEN
                     IRSYR = ISTRT_CENT*100 + IRSYR
                     IDAT  = ISTRT_CENT*100000000 + IDAT8
                  ELSE IF (IRSYR .LT. ISTRT_WIND) THEN
                     IRSYR = (ISTRT_CENT+1)*100 + IRSYR
                     IDAT  = (ISTRT_CENT+1)*100000000 + IDAT8
                  END IF
               ELSE
C                 Header record - cycle to next record
                  CYCLE
               END IF
               IF (IDAT .GT. IRSDATE) THEN
C                 Date of MXDYBYYR Record Is Greater Than Start Date
C                 From Save File.  Treat As End of File to Exit Loop.
                  IF (MULTYR) THEN
C ---                If this is a MULTYEAR run, then MXDYBYYR date should not
C                    be greater than IRSDATE; issue error message
                     WRITE(DUMMY,'(I8.8)') IDAT8
                     CALL ERRHDL(PATH,MODNAM,'E','593',DUMMY)
                  END IF
                  GO TO 299
               END IF
               GO TO 21
 299           EOF = .TRUE.
 21            CONTINUE
            END DO
            EOF = .FALSE.
C           End of file or IRSDATE has been passed; backspace file
            BACKSPACE IMDUNT_BYYR(INDGRP)
C           Skip Header Records
            GO TO 999
         ELSE
C ---       This is not a restarted run; just open the file
            OPEN(IMDUNT_BYYR(INDGRP),ERR=99, FILE=MAXDLY_BYYR(INDGRP),
     &           IOSTAT=IOERRN,FORM='FORMATTED',STATUS='REPLACE')
         END IF
      ELSE IF (FOUND .AND. RSTINP) THEN
C        This file is already open, and this run is a
C        Re-start from an earlier run
C        Skip Header Records
         GO TO 999
      END IF

C     Write Header to File.  Skip Header if FATAL.
      IF (RUN .AND. .NOT.FATAL .AND. .NOT.L_NoHeader(7)) THEN
         WRITE(IMDUNT_BYYR(INDGRP),9005) VERSN,TITLE1(1:68), RUNDAT
 9005    FORMAT('* AERMOD (',A6,'): ',A68,5X,A8)
         WRITE(IMDUNT_BYYR(INDGRP),9007) C_METVER, RUNTIM,  
     &                          MODOPS_String(1:LEN_TRIM(MODOPS_String))
 9007    FORMAT('* AERMET (',A6,'):',T93,A8,
     &         /'* MODELING OPTIONS USED: ',A:)
         WRITE(IMDUNT_BYYR(INDGRP),9010) GRPID(INDGRP), NUMREC, 
     &                                   MXDFRM(1:LEN_TRIM(MXDFRM))
 9010    FORMAT('*',9X,'MXDYBYYR FILE OF RANKED DAILY MAXIMUM 1-HR ',
     &         ' VALUES BY YEAR FOR SOURCE GROUP: ',A8,
     &         /'*',9X,'FOR A TOTAL OF ',I5,' RECEPTORS.',
     &         /'*',9X,'FORMAT: ',A:)
         WRITE(IMDUNT_BYYR(INDGRP),HDRFRM) (CHIDEP(1,ITYP),
     &                   CHIDEP(2,ITYP), CHIDEP(3,ITYP), ITYP=1,NUMTYP)
 9020 FORMAT('(''*'',8X,''X'',13X,''Y'',4X,',I1,'(2X,3A4),4X,''ZELEV'',
     &  4X,''ZHILL'',4X,''ZFLAG'',4X,''RANK'',4X,''GRP'',5X,''JDAY'',
     &  3X,''HR'',4X,''DATE'',5X,''NET ID'',
     &  /,''*'',',I1,'(1X,''____________ ''),1X,
     & 3('' ______  ''),''______  ________  ____  ___  ________'',
     &  ''  ________'')')

      END IF

      GO TO 999

C     WRITE Error Message for Error Opening File
 99   WRITE(DUMMY,'("MXDLY",I3.3)') IMDUNT_BYYR(INDGRP)
      CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

      GO TO 999

C     WRITE Error Message for Error Reading File
 919  CALL ERRHDL(PATH,MODNAM,'E','510',DUMMY)

 999  RETURN
      END

      SUBROUTINE OUMAXD_CONT
C***********************************************************************
C                OUMAXD_CONT Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: To Process Option for Source Group Contributions to 
C                 Ranked Maximum Daily 1-hour Results
C
C        PROGRAMMER: Roger Brode
C
C        DATE:       February 28, 2011
C
C        MODIFIED:   Modified to include checks on the use of a limited 
C                    range of ranks (specified on the OU RECTABLE keyword) 
C                    with the THRESH option on the OU MAXDCONT keyword. 
C                    A fatal error message is generated if the range of ranks 
C                    specified is less than or equal to the design value rank for 
C                    the specified pollutant plus 4, i.e., a fatal error will be 
C                    generated if the range of ranks is less than or equal to 8 
C                    for 1-hr SO2, or less than or equal to 12 for 1-hr NO2 or 
C                    24-hr PM2.5. A non-fatal warning message is also generated 
C                    if the range of ranks is less than or equal to the design 
C                    value rank plus 20, i.e., if the range of ranks is less than 
C                    or equal to 24 for 1-hr SO2, or less than or equal to 28 for 
C                    1-hr NO2 or 24-hr PM2.5.
C                    R. Brode, US EPA, OAQPS, AQMG, 02/29/2012
C
C        INPUTS:  Input Runstream Parameters
C
C        OUTPUTS: Output Option Switches
C
C        CALLED FROM:   OUCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, J
      CHARACTER INPGRP*8
C     JAT D065, 8/9/21 BUFFER SET BUT NOT USED
C      CHARACTER (LEN = ILEN_FLD) :: BUFFER
      LOGICAL :: FOUND

C     Variable Initializations
      MODNAM = 'OUMAXD_CONT'
      
C --- Set logical flag indicated that source group contributions to
C     maximum daily 1-hour values will be performed (as internal
C     post-processing)
      L_MAXDCONT = .TRUE.
      
C --- Check for use of re-start option (CO INITFILE/SAVEFILE keywords), 
C     or MULTYEAR option, which are not compatible with MAXDCONT option
      IF (MULTYR) THEN
C        Error Message: Incompatible options
         CALL ERRHDL(PATH,MODNAM,'E','153','MULTYEAR')
         GO TO 999
      ELSE IF (RSTSAV) THEN
C        Error Message: Incompatible options
         CALL ERRHDL(PATH,MODNAM,'E','153','SAVEFILE')
         GO TO 999
      ELSE IF (RSTINP) THEN
C        Error Message: Incompatible options
         CALL ERRHDL(PATH,MODNAM,'E','153','INITFILE')
         GO TO 999
      END IF
C     JAT D065, 8/9/21 BUFFER SET BUT NOT USED
C      BUFFER = ' '

C     Check If Enough Fields
      IF (IFC .EQ. 2) THEN
C        Error Message: No Fields
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 6) THEN
C        Error Message: Not Enough Fields
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 8) THEN
C        Error Message: Too Many Fields
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Retrieve Source Group ID
      INPGRP = FIELD(3)
C     Check Source Group ID
      FOUND = .FALSE.
      INDGRP = 0
      J = 1
      DO WHILE (.NOT.FOUND .AND. J.LE.NUMGRP)
         IF (INPGRP .EQ. GRPID(J)) THEN
            FOUND = .TRUE.
            INDGRP = J
         END IF
         J = J + 1
      END DO
      IF (.NOT. FOUND) THEN
C        Error Message: E203 GRPID Not Match With Pre-Defined One
         CALL ERRHDL(PATH,MODNAM,'E','203','GRPID')
         GO TO 999
      ELSE IF (MAXDCONT(INDGRP) .EQ. 1) THEN
C ---    MAXDCONT file already specified for this source group
         CALL ERRHDL(PATH,MODNAM,'E','161',GRPID(INDGRP))
         GO TO 999
      ELSE
C ---    Set flag for MAXDCONT option for this source group
         MAXDCONT(INDGRP) = 1
      END IF

C     Retrieve "upper" bound for rank
      CALL STONUM(FIELD(4),ILEN_FLD,FNUM,IMIT)
C     Check for Valid Rank Value
      IF (IMIT .NE. 1) THEN
C        Write Error Message:Invalid Numerical Field
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      END IF
      MXD_RANK(INDGRP,1) = NINT(FNUM)
      IF (MXD_RANK(INDGRP,1) .GT. NHIVAL) THEN
C        Write Error Message:Rank exceeds # of high values;
C        this shouldn't occur since limits are dynamically allocated
         WRITE(DUMMY,'(''NVAL='',I7)') NHIVAL
         CALL ERRHDL(PATH,MODNAM,'E','290',DUMMY)
         GO TO 999
      END IF

C --- Check for whether user specified a threshold for the
C     MAXDCONT file, rather than upper bound on rank

      IF (FIELD(5) .EQ. 'THRESH') THEN
C ---    Retrieve lower threshold value for MAXDCONT results

         CALL STODBL(FIELD(6),ILEN_FLD,DNUM,IMIT)
C        Check for Valid Threshold Value
         IF (IMIT .NE. 1) THEN
C           Write Error Message:Invalid Numerical Field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 999
         END IF
         MAXD_THRESH(INDGRP) = DNUM
            
C ---    Assign maximum value to "lower" bound of ranks
         MXD_RANK(INDGRP,2) = NHIVAL
      
         IF ((LOCE(7)-LOCB(7)) .LE. (ILEN_FLD - 1) ) THEN
C           Retrieve Filename as Character Substring to Maintain Case
C           Also Check for Filename Larger Than ILEN_FLD Characters
            MAXDCONT_FILE(INDGRP) = RUNST1(LOCB(7):LOCE(7))
         ELSE
C           WRITE Error Message:  MAXDCONT_FILE Field is Too Long
            WRITE(DUMMY,'(I8)') ILEN_FLD
            CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
         END IF
      
C        Retrieve File Unit If Input, or Assign File Unit and OPEN File
         IF (IFC .EQ. 8) THEN
            CALL STONUM(FIELD(8),ILEN_FLD,FNUM,IMIT)
C           Check for Valid File Unit Value
            IF (IMIT .NE. 1) THEN
C              Write Error Message: Invalid Numerical Field
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               GO TO 999
            END IF
C           Check for Conflict With System Files
            IF (NINT(FNUM) .LE. 30) THEN
C              WRITE Error Message:  Invalid File Unit Specified
               CALL ERRHDL(PATH,MODNAM,'E','560',KEYWRD)
               GO TO 999
            ELSE IF (NINT(FNUM) .GT. 100) THEN
C              WRITE Warning Message:  Suspect File Unit Specified
C              Unit May Conflict With Dynamically Allocated File Units
               CALL ERRHDL(PATH,MODNAM,'W','565',KEYWRD)
               IMXDCUNT(INDGRP) = NINT(FNUM)
            ELSE
               IMXDCUNT(INDGRP) = NINT(FNUM)
            END IF
         ELSE
C           Dynamically Allocate File Unit (700's)
            IMXDCUNT(INDGRP) = 701 + (INDGRP)*2
            IF (INDGRP .GE. 10) THEN
C              WRITE Warning Message: Dynamic Unit Allocation May Have Conflict
               CALL ERRHDL(PATH,MODNAM,'W','565',KEYWRD)
            END IF
         END IF

C ---    Check for use of MAXDCONT THRESH option with limited range of 
C        ranks on RECTABLE keyword
         IF ( (SO2AVE .AND. NHIVAL.LE. 8) .OR. 
     &        (NO2AVE .AND. NHIVAL.LE.12) .OR.
     &       (PM25AVE .AND. NHIVAL.LE.12) ) THEN
C ---       NHIVAL is less than or equal to the design value rank + 4
C           for the THRESH option; issue fatal ERROR message 
            WRITE(DUMMY,'(''Max Rank ='',I2)') NHIVAL
            CALL ERRHDL(PATH,MODNAM,'E','273',DUMMY)
         ELSE IF ( (SO2AVE .AND. NHIVAL.LE.24) .OR. 
     &             (NO2AVE .AND. NHIVAL.LE.28) .OR.
     &            (PM25AVE .AND. NHIVAL.LE.28) ) THEN
C ---       NHIVAL is less than or equal to the design value rank + 20
C           for the THRESH option; issue non-fatal WARNING message 
            WRITE(DUMMY,'(''Max Rank ='',I2)') NHIVAL
            CALL ERRHDL(PATH,MODNAM,'W','273',DUMMY)
         END IF

      ELSE
C ---    User specified an "lower" bound for rank, rather than threshold
      
C        Retrieve "lower"  bound for rank
         CALL STONUM(FIELD(5),ILEN_FLD,FNUM,IMIT)
C        Check for Valid Rank Value
         IF (IMIT .NE. 1) THEN
C           Write Error Message:Invalid Numerical Field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 999
         END IF
         MXD_RANK(INDGRP,2) = NINT(FNUM)
         IF (MXD_RANK(INDGRP,2) .GT. NHIVAL) THEN
C           Write Error Message:Rank exceeds # of high values;
C           this shouldn't occur since limits are dynamically allocated
            WRITE(DUMMY,'(''NVAL='',I7)') NHIVAL
            CALL ERRHDL(PATH,MODNAM,'E','290',DUMMY)
            GO TO 999
         ELSE IF (MXD_RANK(INDGRP,2) .LT. MXD_RANK(INDGRP,1)) THEN
C           Write Error Message: "lower" bound rank < "upper" bound
            WRITE(DUMMY,'(''U='',I3,2X,''L='',I3)') MXD_RANK(INDGRP,1),
     &                                              MXD_RANK(INDGRP,2)
            CALL ERRHDL(PATH,MODNAM,'E','272',DUMMY)
            GO TO 999
C ---    Add warning regarding MXD_RANK > 366
         ELSE IF (MXD_RANK(INDGRP,2) .GT. 366) THEN
C           Write Warning Message:Rank exceeds max number of days in a year
            WRITE(DUMMY,'('' NVAL='',I6)') MXD_RANK(INDGRP,2)
            CALL ERRHDL(PATH,MODNAM,'W','290',DUMMY)

         END IF

C ---    Assign value of 0.0 to Threshold value
         MAXD_THRESH(INDGRP) = 0.0D0
      
         IF ((LOCE(6)-LOCB(6)) .LE. (ILEN_FLD - 1) ) THEN
C           Retrieve Filename as Character Substring to Maintain Original Case
C           Also Check for Filename Larger Than ILEN_FLD Characters
            MAXDCONT_FILE(INDGRP) = RUNST1(LOCB(6):LOCE(6))
         ELSE
C           WRITE Error Message:  MAXDCONT_FILE Field is Too Long
            WRITE(DUMMY,'(I8)') ILEN_FLD
            CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
         END IF
      
C        Retrieve File Unit If Input, or Assign File Unit and OPEN File
         IF (IFC .EQ. 7) THEN
            CALL STONUM(FIELD(7),ILEN_FLD,FNUM,IMIT)
C           Check for Valid File Unit Value
            IF (IMIT .NE. 1) THEN
C              Write Error Message: Invalid Numerical Field
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               GO TO 999
            END IF
C           Check for Conflict With System Files
            IF (NINT(FNUM) .LE. 30) THEN
C              WRITE Error Message:  Invalid File Unit Specified
               CALL ERRHDL(PATH,MODNAM,'E','560',KEYWRD)
               GO TO 999
            ELSE IF (NINT(FNUM) .GT. 100) THEN
C              WRITE Warning Message:  Suspect File Unit Specified
C              Unit May Conflict With Dynamically Allocated File Units
               CALL ERRHDL(PATH,MODNAM,'W','565',KEYWRD)
               IMXDCUNT(INDGRP) = NINT(FNUM)
            ELSE
               IMXDCUNT(INDGRP) = NINT(FNUM)
            END IF
         ELSE
C           Dynamically Allocate File Unit (700's)
            IMXDCUNT(INDGRP) = 701 + (INDGRP)*2
            IF (INDGRP .GE. 10) THEN
C              WRITE Warning Message: Dynamic FUnit Allocation May Have Conflict
               CALL ERRHDL(PATH,MODNAM,'W','565',KEYWRD)
            END IF
         END IF

      END IF
      
C     Check for Earlier Use of This Filename and File Unit
      FOUND = .FALSE.
      DO I = 1, NUMGRP
         IF (I .NE. INDGRP) THEN
            IF (MAXDCONT_FILE(INDGRP).EQ.MAXDCONT_FILE(I) .AND.
     &               IMXDCUNT(INDGRP).EQ.IMXDCUNT(I)) THEN
               FOUND = .TRUE.
            ELSEIF(MAXDCONT_FILE(INDGRP).EQ.MAXDCONT_FILE(I) .AND.
     &                  IMXDCUNT(INDGRP).NE.IMXDCUNT(I)) THEN
C              Write Error Message: Conflicting Inputs
               CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
               GO TO 999
            ELSEIF(MAXDCONT_FILE(INDGRP).NE.MAXDCONT_FILE(I) .AND.
     &                  IMXDCUNT(INDGRP).EQ.IMXDCUNT(I)) THEN
C              Write Error Message: Conflicting Inputs
               CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
               GO TO 999
            END IF
         END IF
      END DO

      IF (.NOT. FOUND) THEN
C ---    This is a new file; open the file
         OPEN(IMXDCUNT(INDGRP),ERR=99,FILE=MAXDCONT_FILE(INDGRP),
     &           IOSTAT=IOERRN,FORM='FORMATTED',STATUS='REPLACE')
      END IF

      GO TO 999

C     WRITE Error Message for Error Opening File
 99   WRITE(DUMMY,'("MXDCN",I3.3)') IMXDCUNT(INDGRP)
      CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

 999  RETURN
      END

      SUBROUTINE NOHEADER
C***********************************************************************
C                 NOHEADER Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: To Process NOHEADER Output Selections
C
C        PROGRAMMER: Roger Brode
C
C        DATE:       February 28, 2011
C
C        INPUTS:  Input Runstream Parameters
C
C        OUTPUTS: Output Option Switches
C
C        CALLED FROM:   OUCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      INTEGER :: I
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'NOHEADER'

C     Check If Enough Fields
      IF (IFC .EQ. 2) THEN
C        Error Message: No Fields
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 3) THEN
C        Error Message: Not Enough Fields
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 10) THEN
C        Error Message: Too Many Fields
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

      DO I = 3, IFC
         IF (FIELD(I) .EQ. 'ALL') THEN
C           No headers for any ouput file type
            L_NoHeader(:) = .TRUE.
            EXIT
         ELSE IF (FIELD(I) .EQ. 'MAXIFILE') THEN
C           No headers for MAXIFILE
            L_NoHeader(1) = .TRUE.
         ELSE IF (FIELD(I) .EQ. 'POSTFILE') THEN
C           No headers for POSTFILE
            L_NoHeader(2) = .TRUE.
         ELSE IF (FIELD(I) .EQ. 'PLOTFILE') THEN
C           No headers for PLOTFILE
            L_NoHeader(3) = .TRUE.
         ELSE IF (FIELD(I) .EQ. 'SEASONHR') THEN
C           No headers for SEASONHR
            L_NoHeader(4) = .TRUE.
         ELSE IF (FIELD(I) .EQ. 'RANKFILE') THEN
C           No headers for RANKFILE
            L_NoHeader(5) = .TRUE.
         ELSE IF (FIELD(I) .EQ. 'MAXDAILY') THEN
C           No headers for MAXDAILY
            L_NoHeader(6) = .TRUE.
         ELSE IF (FIELD(I) .EQ. 'MXDYBYYR') THEN
C           No headers for MXDYBYYR
            L_NoHeader(7) = .TRUE.
         ELSE IF (FIELD(I) .EQ. 'MAXDCONT') THEN
C           No headers for MAXDCONT
            L_NoHeader(8) = .TRUE.
         ELSE
C           Write Error Message:  Invalid output file type
            CALL ERRHDL(PATH,MODNAM,'E','203',FIELD(I))
         END IF
      END DO

  999 RETURN
      END

