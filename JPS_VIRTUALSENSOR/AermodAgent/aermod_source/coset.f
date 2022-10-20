      SUBROUTINE COCARD
C***********************************************************************
C                 COCARD Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: To process COntrol Pathway card images
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To add various NOx background options (NOXSECTR,
C                    NOXVALUE, NOX_VALS, NOX_UNIT, NOX_FILE) for use
C                    with the GRSM NO2 option
C                    CERC, 11/30/20
C
C        MODIFIED:   Added the PSDCREDIT option for PVMRM; in this release
C                    specifying PSDCREDIT also requires specifying PVMRM;
C                    specifying PSDCREDIT and OLM is not a valid combination
C                    J Paumier, MACTEC -  09/30/2006
C
C        MODIFIED:   Added undocumentd NODRYDPLT and NOWETDPLT options to
C                    MODOPS header.  Also moved code to write header of
C                    DEBUG output file to AERMOD.FOR to follow SETUP,
C                    to accommodate final setting for DRYDPLT and WETDPLT.
C                    R. W. Brode, PES - 10/26/2004
C
C        MODIFIED:   To allow 24-hour or ANNUAL averages to be modeled
C                    separately for post-1997 PM10 processing.
C                    R. W. Brode, PES - 12/2/98
C
C        MODIFIED:   To add DDEP and WDEP parameters to CONC/DEPOS options
C                    to allow just the wet or just the dry deposition flux
C                    to be reported.  DEPOS now reports the sum of wet and
C                    dry fluxes.
C                    D. Strimaitis, SRC - 11/8/93
C
C        MODIFIED:   To add DEPLETE parameter for plume depletion option
C                    and to allow flagpole receptors with DEPOS option.
C                    D. Strimaitis, SRC - 2/15/93
C
C        INPUTS:  Pathway (CO) and Keyword
C
C        OUTPUTS: Processing Option Switches
C                 Option Setup Status Switches
C
C        CALLED FROM:   SETUP
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      INTEGER :: I

C     Variable Initializations
      MODNAM = 'COCARD'

      IF (KEYWRD .EQ. 'STARTING') THEN
         IURB = 0
C        Set Status Switch
         ISTART = .TRUE.
         ICSTAT(1) = ICSTAT(1) + 1
         IF (ICSTAT(1) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         END IF
      ELSE IF (KEYWRD .EQ. 'TITLEONE') THEN
C        Set Status Switch
         ICSTAT(2) = ICSTAT(2) + 1
         IF (ICSTAT(2) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Titles                                  ---   CALL TITLES
            CALL TITLES
         END IF
      ELSE IF (KEYWRD .EQ. 'TITLETWO') THEN
C        Set Status Switch
         ICSTAT(3) = ICSTAT(3) + 1
         IF (ICSTAT(3) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Titles                                  ---   CALL TITLES
            CALL TITLES
         END IF
      ELSE IF (KEYWRD .EQ. 'MODELOPT') THEN
C        Set Status Switch
         ICSTAT(4) = ICSTAT(4) + 1
         IF (ICSTAT(4) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Modeling Options                        ---   CALL MODOPT
            CALL MODOPT
         END IF
      ELSE IF (KEYWRD .EQ. 'AVERTIME') THEN
C        Set Status Switch
         ICSTAT(5) = ICSTAT(5) + 1
         IF (ICSTAT(5) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Averaging Time Options                  ---   CALL AVETIM
            CALL AVETIM
         END IF
      ELSE IF (KEYWRD .EQ. 'POLLUTID') THEN
C        Set Status Switch
         ICSTAT(6) = ICSTAT(6) + 1
         IF (ICSTAT(6) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Pollutant ID Option                     ---   CALL POLLID
            CALL POLLID
         END IF
      ELSE IF (KEYWRD .EQ. 'HALFLIFE' .OR.
     &         KEYWRD .EQ. 'DCAYCOEF') THEN
         IF (KEYWRD .EQ. 'HALFLIFE') THEN
C           Check for Previous DCAYCOEF Keyword in Runstream File
            IF (ICSTAT(8) .NE. 0) THEN
               CALL ERRHDL(PATH,MODNAM,'W','155',KEYWRD)
               GO TO 999
            ELSE
C              Set Status Switch and Check for Duplicate Keyword
               ICSTAT(7) = ICSTAT(7) + 1
               IF (ICSTAT(7) .NE. 1) THEN
C                 WRITE Error Message: Repeat Non-repeatable Keyword
                  CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
                  GO TO 999
               END IF
            END IF
         ELSE IF (KEYWRD .EQ. 'DCAYCOEF') THEN
C           Check for Previous HALFLIFE Keyword in Runstream File
            IF (ICSTAT(7) .NE. 0) THEN
               CALL ERRHDL(PATH,MODNAM,'W','155',KEYWRD)
               GO TO 999
            ELSE
C              Set Status Switch and Check for Duplicate Keyword
               ICSTAT(8) = ICSTAT(8) + 1
               IF (ICSTAT(8) .NE. 1) THEN
C                 WRITE Error Message: Repeat Non-repeatable Keyword
                  CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
                  GO TO 999
               END IF
            END IF
         END IF
C        Check for Keyword Out of Order
         IF (ICSTAT(4) .NE. 1) THEN
C           WRITE Error Message: Keyword Out of Order (Must Follow MODELOPT)
            CALL ERRHDL(PATH,MODNAM,'E','140',KEYWRD)
         ELSE IF (ICSTAT(6) .NE. 1) THEN
C           WRITE Error Message: Keyword Out of Order (Must Follow POLLUTID)
            CALL ERRHDL(PATH,MODNAM,'E','140',KEYWRD)
         END IF
C        Process Exponential Decay Option                   ---   CALL EDECAY
         CALL EDECAY
      ELSE IF (KEYWRD .EQ. 'FLAGPOLE') THEN
C        Set Status Switch
         ICSTAT(11) = ICSTAT(11) + 1
         IF (ICSTAT(11) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Flagpole Receptor Height Option         ---   CALL FLAGDF
            CALL FLAGDF
         END IF
      ELSE IF (KEYWRD .EQ. 'RUNORNOT') THEN
C        Set Status Switch
         ICSTAT(12) = ICSTAT(12) + 1
         IF (ICSTAT(12) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Option to Run Model or Not              ---   CALL RUNNOT
            CALL RUNNOT
         END IF
      ELSE IF (.NOT.EVONLY .AND. KEYWRD .EQ. 'EVENTFIL') THEN
C        Set Status Switch
         ICSTAT(13) = ICSTAT(13) + 1
         IF (ICSTAT(13) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
            IF (PSDCREDIT) THEN
C              WRITE Warning Message:  PSDCREDIT option cannot be used with EVENT option
               CALL ERRHDL(PATH,MODNAM,'W','147',KEYWRD)
            END IF
C           Process EVENT File Option                       ---   CALL EVNTFL
            CALL EVNTFL
         END IF
      ELSE IF (.NOT.EVONLY .AND. KEYWRD .EQ. 'SAVEFILE') THEN
C        Set Status Switch
         ICSTAT(14) = ICSTAT(14) + 1
         IF (ICSTAT(14) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Model Re-start Save File Option         ---   CALL SAVEFL
            CALL SAVEFL
         END IF
      ELSE IF (.NOT.EVONLY .AND. KEYWRD .EQ. 'INITFILE') THEN
C        Set Status Switch
         ICSTAT(15) = ICSTAT(15) + 1
         IF (ICSTAT(15) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Re-start Initialization File Option     ---   CALL INITFL
            CALL INITFL
         END IF
      ELSE IF (.NOT.EVONLY .AND. KEYWRD .EQ. 'MULTYEAR') THEN
C        Set Status Switch
         ICSTAT(16) = ICSTAT(16) + 1
         IF (ICSTAT(16) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Multiple-Year Run Option                ---   CALL MYEAR
            CALL MYEAR
         END IF
      ELSE IF (KEYWRD .EQ. 'ERRORFIL') THEN
C        Set Status Switch
         ICSTAT(17) = ICSTAT(17) + 1
         IF (ICSTAT(17) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Error File Option                       ---   CALL ERRFIL
            CALL ERRFIL
         END IF
      ELSE IF (KEYWRD .EQ. 'GDSEASON') THEN
C        Set Status Switch
         ICSTAT(18) = ICSTAT(18) + 1
         IF (ICSTAT(18) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Seasons for GASDEP Option              ---   CALL GDSEAS
            CALL GDSEAS
         END IF
      ELSE IF (KEYWRD .EQ. 'GASDEPDF') THEN
C        Set Status Switch
         ICSTAT(19) = ICSTAT(19) + 1
         IF (ICSTAT(19) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process GASDEP Defaults Option                  ---   CALL GDDEF
            CALL GDDEF
         END IF
      ELSE IF (KEYWRD .EQ. 'GDLANUSE') THEN
C        Set Status Switch
         ICSTAT(20) = ICSTAT(20) + 1
         IF (ICSTAT(20) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Error File Option                       ---   CALL GDLAND
            CALL GDLAND
         END IF
      ELSE IF (KEYWRD .EQ. 'GASDEPVD') THEN
C        Set Status Switch
         ICSTAT(21) = ICSTAT(21) + 1
         IF (ICSTAT(21) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           User Specified Deposition Velocity Option       ---   CALL GVSUBD
            CALL GVSUBD
         END IF
      ELSE IF (KEYWRD .EQ. 'DEBUGOPT') THEN
C        Set Status Switch
         ICSTAT(22) = ICSTAT(22) + 1
         IF (ICSTAT(22) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Error File Option                       ---   CALL DEBOPT
            CALL DEBOPT
         END IF
      ELSE IF (KEYWRD .EQ. 'URBANOPT') THEN
C        Set Status Switch
         ICSTAT(23) = ICSTAT(23) + 1
C        Check for Keyword Out of Order
         IF (ICSTAT(4) .NE. 1) THEN
C           WRITE Error Message: Keyword Out of Order (Must Follow MODELOPT)
            CALL ERRHDL(PATH,MODNAM,'E','140',KEYWRD)
         END IF
C        Process Urban Option                               ---   CALL URBOPT
         CALL URBOPT
      ELSE IF (KEYWRD .EQ. 'OZONEVAL') THEN
C        Set Status Switch
         ICSTAT(24) = ICSTAT(24) + 1
         IF (PVMRM .OR. OLM .OR. RUNTTRM .OR. GRSM) THEN
C           Process O3 Value Option                         ---   CALL O3VAL
            CALL O3VAL
         ELSE
CRCO 3/1/2021 Check to see if we want to use 142 or 600 error code. CERC used 600
C           Write Error Message:  OZONEVAL specified w\o PVMRM, OLM, TTRM, GRSM
            CALL ERRHDL(PATH,MODNAM,'E','600',KEYWRD)
         END IF
      ELSE IF (KEYWRD .EQ. 'O3VALUES') THEN
C        Set Status Switch
         ICSTAT(25) = ICSTAT(25) + 1
         IF (PVMRM .OR. OLM .OR. RUNTTRM .OR. GRSM) THEN
C           Process O3 Value Option                         ---   CALL O3VALS
            CALL O3VALS
         ELSE
C           Write Error Message: O3VALUES specified w\o PVMRM, OLM, TTRM, or GRSM
            CALL ERRHDL(PATH,MODNAM,'E','600',KEYWRD)
         END IF
      ELSE IF (KEYWRD .EQ. 'OZONEFIL') THEN
C        Set Status Switch
         ICSTAT(26) = ICSTAT(26) + 1
         IF (PVMRM .OR. OLM .OR. RUNTTRM .OR. GRSM) THEN
C           Process O3 File Option                          ---   CALL O3FILE
            CALL O3FILE
         ELSE
C           Write Error Message: O3VALUES specified w\o PVMRM, OLM, TTRM, or GRSM
            CALL ERRHDL(PATH,MODNAM,'E','600',KEYWRD)
         END IF
      ELSE IF (KEYWRD .EQ. 'OZONUNIT') THEN
C        Set Status Switch
         ICSTAT(27) = ICSTAT(27) + 1
         IF (ICSTAT(27) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
            IF (PVMRM .OR. OLM .OR. RUNTTRM .OR. GRSM) THEN
C              Process the OZONUNIT Card                    ---   CALL OZON_UNIT
               CALL OZON_UNIT
            ELSE
C              Write Error Message: O3VALUES specified w\o PVMRM, OLM, TTRM, or GRSM
               CALL ERRHDL(PATH,MODNAM,'E','600',KEYWRD)
            END IF
         END IF
      ELSE IF (KEYWRD .EQ. 'NO2STACK') THEN
C        Set Status Switch
         ICSTAT(28) = ICSTAT(28) + 1
         IF (ICSTAT(28) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
            IF (PVMRM .OR. OLM .OR. RUNTTRM .OR. GRSM) THEN
C              Process NO2Stack Option                      ---   CALL NO2STK
               CALL NO2STK
            ELSE
C              Write Error Message: NO2STACK specified w/o PVMRM, OLM, TTRM, GRSM
               CALL ERRHDL(PATH,MODNAM,'E','600',KEYWRD)
            END IF
         END IF
      ELSE IF (KEYWRD .EQ. 'NO2EQUIL') THEN
C        Set Status Switch
         ICSTAT(29) = ICSTAT(29) + 1
         IF (ICSTAT(29) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C Add equilibrium limit for GRSM also?
            IF (PVMRM .OR. OLM .OR. RUNTTRM) THEN
C              Process NO2Equil Option                      ---   CALL NO2EQ
               CALL NO2EQ
            ELSE
C              Write Error Message:  NO2EQUIL specified without PVMRM or OLM or TTRM
               CALL ERRHDL(PATH,MODNAM,'E','142',KEYWRD)
            END IF
         END IF
      ELSE IF (KEYWRD .EQ. 'LOW_WIND') THEN
C        Set Status Switch
         ICSTAT(30) = ICSTAT(30) + 1
         IF (ICSTAT(30) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE IF (L_ALPHA) THEN
C            Process LOW_WIND keyword                       ---   CALL LOW_WND
             CALL LOW_WND
         ELSE
C           WRITE Error Message: LOW_WIND option requires ALPHA option
            CALL ERRHDL(PATH,MODNAM,'E','133',KEYWRD)
         END IF
      ELSE IF (KEYWRD .EQ. 'O3SECTOR') THEN
C        Set Status Switch
         ICSTAT(31) = ICSTAT(31) + 1
         IF (ICSTAT(31) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
            IF (PVMRM .OR. OLM .OR. RUNTTRM .OR. GRSM) THEN
C              Process O3SECTOR keyword                     ---   CALL O3SECTOR
               CALL O3SECTOR
            ELSE
C              Write Error Message: O3SECTOR specified w/o PVMRM, OLM, TTRM, GRSM
               CALL ERRHDL(PATH,MODNAM,'E','600',KEYWRD)
            END IF
         END IF
      ELSE IF (KEYWRD .EQ. 'ARMRATIO') THEN
C        Set Status Switch
         ICSTAT(32) = ICSTAT(32) + 1
         IF (ICSTAT(32) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
            IF (ARM2) THEN
C              Process ARM2_Ratios Option                   ---   CALL ARM2_Ratios
               CALL ARM2_Ratios
            ELSE
C              Write Error Message:  ARMRATIO specified without ARM2
               CALL ERRHDL(PATH,MODNAM,'E','145',KEYWRD)
            END IF
         END IF
      ELSE IF (KEYWRD .EQ. 'AWMADWNW') THEN
C        Set Status Switch
         ICSTAT(33) = ICSTAT(33) + 1
         IF (ICSTAT(33) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE IF (L_ALPHA) THEN
C           Process AWMADWNW keyword                         ---   CALL AWMA_DOWNWASH
            CALL AWMA_DOWNWASH
         ELSE
C           WRITE Error Message: AWMADWNW option requires ALPHA option
            CALL ERRHDL(PATH,MODNAM,'E','122',KEYWRD)
         END IF
      ELSE IF (KEYWRD .EQ. 'ORD_DWNW') THEN
C        Set Status Switch
         ICSTAT(34) = ICSTAT(34) + 1
         IF (ICSTAT(34) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE IF (L_ALPHA) THEN
C           Process ORD_DWNW keyword                       ---   CALL ORD_DOWNWASH
            CALL ORD_DOWNWASH
         ELSE
C           WRITE Error Message: ORD_DWNW option requires ALPHA option
            CALL ERRHDL(PATH,MODNAM,'E','123',KEYWRD)
         END IF
C     CERC 11/30/20:
      ELSE IF (KEYWRD .EQ. 'NOXSECTR') THEN
C        Set Status Switch
         ICSTAT(35) = ICSTAT(35) + 1
         IF (ICSTAT(35) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
            IF (GRSM) THEN
C              Process NOXSECTR keyword                    ---   CALL NOXSECTOR
               CALL NOXSECTOR
            ELSE
C              Write Error Message:  NOXSECTR specified without GRSM
               CALL ERRHDL(PATH,MODNAM,'E','602',KEYWRD)
            END IF
         END IF
      ELSE IF (KEYWRD .EQ. 'NOXVALUE') THEN
C        Set Status Switch
         ICSTAT(36) = ICSTAT(36) + 1
         IF (GRSM) THEN
C           Process NOX Value Option                    ---   CALL NOXVAL
            CALL NOXVAL
         ELSE
C           Write Error Message:  NOXVALUE specified without GRSM
            CALL ERRHDL(PATH,MODNAM,'E','602',KEYWRD)
         END IF
      ELSE IF (KEYWRD .EQ. 'NOX_VALS') THEN
C        Set Status Switch
         ICSTAT(37) = ICSTAT(37) + 1
         IF (GRSM) THEN
C           Process NOx Value Option                    ---   CALL NOXVALS
            CALL NOXVALS
         ELSE
C           Write Error Message:  NOX_VALS specified without GRSM
            CALL ERRHDL(PATH,MODNAM,'E','602',KEYWRD)
         END IF
      ELSE IF (KEYWRD .EQ. 'NOX_UNIT') THEN
C        Set Status Switch
         ICSTAT(38) = ICSTAT(38) + 1
         IF (ICSTAT(38) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
            IF (GRSM) THEN
C              Process the NOX_UNIT Card                    ---   CALL NOX_UNIT
               CALL NOX_UNIT
            ELSE
C              Write Error Message:  NOX_UNIT specified without GRSM
               CALL ERRHDL(PATH,MODNAM,'E','602',KEYWRD)
            END IF
         END IF
      ELSE IF (KEYWRD .EQ. 'NOX_FILE') THEN
C        Set Status Switch
         ICSTAT(39) = ICSTAT(39) + 1
         IF (GRSM) THEN
C           Process NOX File Option                    ---   CALL NOXFILE
            CALL NOXFILE
         ELSE
C           Write Error Message:  NOX_FILE specified without GRSM
            CALL ERRHDL(PATH,MODNAM,'E','602',KEYWRD)
         END IF
      ELSE IF (KEYWRD .EQ. 'FINISHED') THEN
C        Set Status Switch
         IFINIS = .TRUE.
C        Set Status Switch
         ICSTAT(50) = ICSTAT(50) + 1
         IF (ICSTAT(50) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
            GO TO 999
         END IF

C        Check for Missing Mandatory Keywords
         IF (ICSTAT(1) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','STARTING')
         END IF
         IF (ICSTAT(2) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','TITLEONE')
         END IF
         IF (ICSTAT(4) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','MODELOPT')
         END IF
         IF (ICSTAT(5) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','AVERTIME')
         END IF
         IF (ICSTAT(6) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','POLLUTID')
         END IF
         IF (ICSTAT(12) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','RUNORNOT')
         END IF

         IF (OLM .OR. PVMRM .OR. RUNTTRM .OR. GRSM) THEN
C ---       Check for background Ozone options for OLM/PVMRM/GRSM
            IF (ICSTAT(24).EQ.0 .AND. ICSTAT(25).EQ.0 .AND.
     &                                ICSTAT(26).EQ.0) THEN
C              Write Error Message:  Ozone value or data file needed
               IF (OLM) THEN
                  DUMMY = 'OLM Option  '
               ELSE IF (PVMRM) THEN
                  DUMMY = 'PVMRM Option'
               ELSE IF (GRSM) THEN
                 DUMMY = 'GRSM Option'
               ELSE IF (RUNTTRM) THEN
                 DUMMY = 'TTRM Option  '
               END IF
               CALL ERRHDL(PATH,MODNAM,'E','283',DUMMY)
            ELSE IF (ICSTAT(24).GT.0 .AND. ICSTAT(25).GT.0) THEN
C ---          Both OZONEVAL and O3VALUES keywords have been specified;
C              issue error message if needed; first check for O3SECTOR option
               IF (.NOT. L_O3Sector) THEN
C                 No O3SECTORs; issue error message
                  IF (OLM) THEN
                     DUMMY = 'for OLM   '
                  ELSE IF (PVMRM) THEN
                     DUMMY = 'for PVMRM '
                  ELSE IF (GRSM) THEN
                     DUMMY = 'for GRSM '
                  ELSE IF (RUNTTRM) THEN
                     DUMMY = 'for TTRM   '
                  END IF
C                 Issue error message for conflicting O3 options
                  CALL ERRHDL(PATH,MODNAM,'E','148',DUMMY)
               ELSE
C ---             Loop through O3SECTORs for both OZONEVAL & O3VALUES
                  DO I = 1, NUMO3sects
                     IF (L_O3VAL(I) .AND. L_O3VALUES(I)) THEN
C                       Issue error message; OZONEVAL & O3VALUES for this sector
                        IF (OLM) THEN
                           WRITE(DUMMY,'(''  OLM SECT'',I1)') I
                        ELSE IF (PVMRM) THEN
                           WRITE(DUMMY,'(''PVMRM SECT'',I1)') I
                        ELSE IF (GRSM) THEN
                           WRITE(DUMMY,'(''GRSM SECT'',I1)') I
                        ELSE IF (RUNTTRM) THEN
                           WRITE(DUMMY,'(''  TTRM SECT'',I1)') I
                        END IF
                        CALL ERRHDL(PATH,MODNAM,'E','148',DUMMY)
                     END IF
                  END DO
               END IF
            END IF
C           CERC 11/30/20
            IF (GRSM) THEN
C ---         Check for background NOx options for GRSM
              IF (ICSTAT(36).EQ.0 .AND. ICSTAT(37).EQ.0 .AND.
     &                                            ICSTAT(39).EQ.0 ) THEN
C               No NOx background has been specified so it will be calculated from NO2 equilibrium
C If the background NOX is missing, is it appropriate to compute from the equilibrium?
C Or use a fill value (default) to make sure it's conservative.
                L_CalcNOXFromNO2 = .TRUE.
C               Write Wng Message:  NOx calculated from NO2
                DUMMY = 'for GRSM'
                CALL ERRHDL(PATH,MODNAM,'W','612',DUMMY)
              ELSE IF (ICSTAT(36).GT.0 .AND. ICSTAT(37).GT.0) THEN
C ---           Both NOXVALUE and NOX_VALS keywords have been specified;
C               issue error message if needed; first check for NOXSECTOR option
                IF (.NOT. L_NOxSector) THEN
C                 No NOXSECTORs; issue error message for conflicting NOx options
                  CALL ERRHDL(PATH,MODNAM,'E','605','for GRSM ')
                ELSE
C ---             Loop through NOxSECTORs for both NOXVALUE & NOX_VALS
                  DO I = 1, NUMNOxsects
                     IF (L_NOXVALUE(I) .AND. L_NOX_VALS(I)) THEN
C                       Issue error message; NOXVALUE & NOX_VALS for this sector
                        WRITE(DUMMY,'(''GRSM SECT'',I1)') I
                        CALL ERRHDL(PATH,MODNAM,'E','605',DUMMY)
                     END IF
                  END DO
                END IF
              END IF
            END IF
            IF ((PVMRM .OR. OLM .OR. RUNTTRM .OR. GRSM)
     &           .AND. ICSTAT(28).EQ.0) THEN
C              No NO2STACK card specified for PVMRM, OLM, TTRM or GRSM options.
C              Reinitialize ANO2_RATIO array to -9.0 to track whether
C              NO2/NOx ratios are applied on the SO Pathway with
C              NO2RATIO card for each source.
               ANO2_RATIO(:) = -9.0D0
            END IF
C ---       Check for OZONUNIT keyword without O3VALUES keyword
            IF (ICSTAT(25) .EQ. 0 .AND. ICSTAT(27) .GT. 0) THEN
               CALL ERRHDL(PATH,MODNAM,'E','193','CO O3VALUES')
            END IF
C ---       CERC 11/30/20 Check for NOXUNIT keyword without NOX_VALS keyword
            IF (ICSTAT(37) .EQ. 0 .AND. ICSTAT(38) .GT. 0) THEN
               CALL ERRHDL(PATH,MODNAM,'E','193','CO NOX_VALS')
            END IF
         END IF

C ---    Check for ARM2 inputs
         IF (ARM2) THEN
            IF (ICSTAT(32) .EQ. 0) THEN
C ---          No ARMRATIO keyword specified; apply default ratios for ARM2
               ARM2_Min = 0.50D0
               ARM2_Max = 0.90D0
            END IF
         END IF

C        OPEN Restart Save and Initialization Files
         IF (RSTSAV) THEN
            DUMMY = 'SAVEFILE'
            OPEN(UNIT=IDPUNT,ERR=99,FILE=SAVFIL,FORM='UNFORMATTED',
     &           IOSTAT=IOERRN,STATUS='REPLACE')
C           Close SAVEFILE since it is re-opened in RSDUMP
            CLOSE (IDPUNT)
            IF (SAVFL2 .NE. SAVFIL) THEN
               OPEN(UNIT=IDPUN2,ERR=99,FILE=SAVFL2,FORM='UNFORMATTED',
     &              IOSTAT=IOERRN,STATUS='REPLACE')
C              Close SAVEFILE since it is re-opened in RSDUMP
               CLOSE (IDPUN2)
            END IF
         END IF
         IF (RSTINP) THEN
            IF (RSTSAV) THEN
C ---          First check for filename conflicts with SAVEFILEs
               IF (INIFIL .EQ. SAVFIL .OR. INIFIL .EQ. SAVFL2) THEN
C ---             The INITFILE name matches a SAVEFILE name;
C                 issue error message
                  CALL ERRHDL(PATH,MODNAM,'E','590','       ')
               ELSE
C ---             No filename conflict, open INITFILE
                  DUMMY = 'INITFILE'
                  OPEN(UNIT=IRSUNT,ERR=99,FILE=INIFIL,
     &                 FORM='UNFORMATTED',IOSTAT=IOERRN,STATUS='OLD')
               END IF
            ELSE
C ---          No SAVEFILEs, so open INITFILE
               DUMMY = 'INITFILE'
               OPEN(UNIT=IRSUNT,ERR=99,FILE=INIFIL,FORM='UNFORMATTED',
     &              IOSTAT=IOERRN,STATUS='OLD')
            END IF
         END IF

C        Check Averaging Periods Selected for SCREEN Mode Option
         IF (SCREEN) THEN
            IF (NUMAVE .GT. 1) THEN
C              WRITE Error Message:  Too Many Averaging Periods Selected
               CALL ERRHDL(PATH,MODNAM,'E','295',' 1h Only')
            ELSE IF (KAVE(1) .NE. 1) THEN
C              WRITE Error Message:  Invalid Averaging Period Selected
               CALL ERRHDL(PATH,MODNAM,'E','295',' 1h Only')
            END IF
            IF (PERIOD) THEN
C              WRITE Error Message:  Too Many Averaging Periods Selected
               CALL ERRHDL(PATH,MODNAM,'E','295',' 1h Only')
            END IF
         END IF

C ---    Check for non-DFAULT gas deposition options
c JAT 7/2/19 CHECK FOR ALPHA AND GAS DEPOSITION PARAMETERS
C         IF GAS DEPOSITION PARAMETERS AND NO ALPHA OPTION
C     ISSUE ERROR.  THERE IS ALSO A CHECK FOR GASDEPOS AND
C     ALPHA IN SOCARD
         IF (DFAULT .AND. ICSTAT(18) .GT. 0) THEN
C           Write Error Message:  Gas Dry Deposition Option w/ DFAULT Option
            CALL ERRHDL(PATH,MODNAM,'E','196','GDSEASON')
         ELSEIF (.NOT. L_ALPHA .AND. ICSTAT(18) .GT. 0) THEN !JAT ADDED 7/2/19
C           Write Error Message:  Gas Dry Deposition Option w/o ALPHA Option
            CALL ERRHDL(PATH,MODNAM,'E','198','GDSEASON')
         ELSE IF (ICSTAT(18) .GT. 0) THEN
C           Set flag for use of non-DEFAULT option
            L_NonDFAULT = .TRUE.
         END IF
         IF (DFAULT .AND. ICSTAT(19) .GT. 0) THEN
C           Write Error Message:  Gas Dry Deposition Option w/ DFAULT Option
            CALL ERRHDL(PATH,MODNAM,'E','196','GASDEPDF')
         ELSEIF (.NOT. L_ALPHA .AND. ICSTAT(19) .GT. 0) THEN !JAT ADDED 7/2/19
C           Write Error Message:  Gas Dry Deposition Option w/o ALPHA Option
            CALL ERRHDL(PATH,MODNAM,'E','198','GASDEPDF')
         ELSE IF (ICSTAT(19) .GT. 0) THEN
C           Set flag for use of non-DEFAULT option
            L_NonDFAULT = .TRUE.
         END IF
         IF (DFAULT .AND. ICSTAT(20) .GT. 0) THEN
C           Write Error Message:  Gas Dry Deposition Option w/ DFAULT Option
            CALL ERRHDL(PATH,MODNAM,'E','196','GDLANUSE')
         ELSEIF (.NOT. L_ALPHA .AND. ICSTAT(20) .GT. 0) THEN !JAT ADDED 7/2/19
C           Write Error Message:  Gas Dry Deposition Option w/o ALPHA Option
            CALL ERRHDL(PATH,MODNAM,'E','198','GDLANUSE')
         ELSE IF (ICSTAT(20) .GT. 0) THEN
C           Set flag for use of non-DEFAULT option
            L_NonDFAULT = .TRUE.
         END IF
         IF (DFAULT .AND. ICSTAT(21) .GT. 0) THEN
C           Write Error Message:  Gas Dry Deposition Option w/ DFAULT Option
            CALL ERRHDL(PATH,MODNAM,'E','196','GASDEPVD')
         ELSEIF (.NOT. L_ALPHA .AND. ICSTAT(21) .GT. 0) THEN !JAT ADDED 7/2/19
C           Write Error Message:  Gas Dry Deposition Option w/o ALPHA Option
            CALL ERRHDL(PATH,MODNAM,'E','198','GASDEPVD')
         ELSE IF (ICSTAT(21) .GT. 0) THEN
C           Set flag for use of non-DEFAULT option
            L_NonDFAULT = .TRUE.
         END IF

C ---    Check for incompatibilities with user-specified deposition velocity
         IF (LUSERVD .AND. (DEPOS .OR. WDEP .OR. WDPLETE)) THEN
C           Write Error Message: Wet deposition output incompatible with GASDEPVD option
            CALL ERRHDL(PATH,MODNAM,'E','243','GASDEPVD')
         END IF

C ---    Check for incompatible gas deposition inputs with GASDEPVD option for
C        user-specified gas dry deposition velocity
         IF (LUSERVD .AND. ICSTAT(18) .GT. 0) THEN
C           Write Error Message:  Gas Dry Deposition Option w/ user-specified GASDEPVD
            CALL ERRHDL(PATH,MODNAM,'E','195','GDSEASON')
         END IF
         IF (LUSERVD .AND. ICSTAT(19) .GT. 0) THEN
C           Write Error Message:  Gas Dry Deposition Option w/ user-specified GASDEPVD
            CALL ERRHDL(PATH,MODNAM,'E','195','GASDEPDF')
         END IF
         IF (LUSERVD .AND. ICSTAT(20) .GT. 0) THEN
C           Write Error Message:  Gas Dry Deposition Option w/ user-specified GASDEPVD
            CALL ERRHDL(PATH,MODNAM,'E','195','GDLANUSE')
         END IF

C        Generate MODOPS Character Array to Summarize Modeling Options
         IF (DFAULT) THEN
            MODOPS(1) = 'RegDFAULT'
         ELSE IF (L_NonDFAULT) THEN
            MODOPS(1) = 'NonDFAULT'
         ELSE
            MODOPS(1) = '         '
         END IF

         IF (CONC) THEN
            MODOPS(2) = 'CONC'
         END IF
         IF (DEPOS) THEN
            MODOPS(3) = 'DEPOS'
         END IF
         IF (DDEP) THEN
            MODOPS(4) = 'DDEP'
         END IF
         IF (WDEP) THEN
            MODOPS(5) = 'WDEP'
         END IF

         IF (FLATSRCS) THEN
            MODOPS(6) = 'FLAT and'
            MODOPS(7) = 'ELEV'
         ELSE IF (FLAT) THEN
            MODOPS(6) = 'FLAT'
         ELSE
            MODOPS(7) = 'ELEV'
         END IF

         IF (FLGPOL) MODOPS(8)  = 'FLGPOL'
         IF (NOSTD)  MODOPS(9)  = 'NOSTD'
         IF (NOCHKD) THEN
            MODOPS(10) = 'NOCHKD'
         ELSE IF (L_WARNCHKD) THEN
            MODOPS(10) = 'WARNCHKD'
         END IF

         IF (FASTALL) THEN
            MODOPS(11) = 'FASTALL'
         ELSE IF (FASTAREA) THEN
            MODOPS(11) = 'FASTAREA'
         ELSE IF (NOWARN) THEN
            MODOPS(11) = 'NOWARN'
         END IF

         IF (SCREEN) MODOPS(12) = 'SCREEN'
         IF (MULTYR) MODOPS(13) = 'MULTYR'

         IF (ARDPLETE) THEN
            MODOPS(14) = 'AREADPLT'
         ELSE IF (ROMBERG) THEN
            MODOPS(14) = 'ROMBERG'
         ELSE IF (DDPLETE) THEN
            MODOPS(14) = 'DRYDPLT'
         ELSE IF (.NOT.DDPLETE) THEN
            MODOPS(14) = 'NODRYDPLT'
         END IF

         IF (WDPLETE) THEN
            MODOPS(15) = 'WETDPLT'
         ELSE IF (.NOT.WDPLETE) THEN
            MODOPS(15) = 'NOWETDPLT'
         END IF

         IF (SCIM) MODOPS(16) = 'SCIM'

         IF (RUNTTRM2) THEN
           IF (PVMRM) THEN
             MODOPS(17) = 'TTRM2_PVMRM'
           ELSE IF (OLM) THEN
             MODOPS(17) = 'TTRM2_OLM'
           ELSE IF (ARM2) THEN
             MODOPS(17) = 'TTRM2_ARM2'
           END IF
         ELSE IF (PVMRM) THEN
            MODOPS(17) = 'PVMRM'
         ELSE IF (OLM) THEN
            MODOPS(17) = 'OLM'
         ELSE IF (ARM2) THEN
            MODOPS(17) = 'ARM2'
         ELSE IF (GRSM) THEN
            MODOPS(17) = 'GRSM'
         ELSE IF (RUNTTRM) THEN
            MODOPS(17) = 'TTRM'
         END IF

         IF (PSDCREDIT) THEN
            MODOPS(18) = 'PSDCREDIT'
         ENDIF

C ---    Add labels for non-DFAULT ALPHA and BETA Options
         IF (.NOT. DFAULT .AND. L_ALPHA) THEN
            MODOPS(19) = 'ALPHA'
         ELSE IF (.NOT. DFAULT .AND. BETA) THEN
            MODOPS(19) = 'BETA'
         ELSE IF (DFAULT .AND. BETA) THEN
            CALL ERRHDL(PATH,MODNAM,'E','204','BETA')
         ELSE IF (DFAULT .AND. L_ALPHA) THEN
            CALL ERRHDL(PATH,MODNAM,'E','204','ALPHA')
         END IF

C ---    Add MODOPS field for RURAL, URBAN, or RUR&URB
         IF (NURBSRC .EQ. 0) THEN
C ---       No URBAN sources
            MODOPS(20) = 'RURAL'
         ELSEIF (NURBSRC .EQ. NSRC) THEN
C ---       All sources are URBAN
            MODOPS(20) = 'URBAN'
         ELSE
C ---       Urban and Rural sources
            MODOPS(20) = 'Urb&Rur'
         END IF

C---     Add label for NoUrbTran Non-regulatory Option
         IF (.NOT. L_UrbanTransition) THEN
            MODOPS(21) = 'NoUrbTran'
         END IF

         IF (L_VectorWS) THEN
            MODOPS(23) = 'VectorWS'
         END IF

!CRCO 1/7/21 D074 Add NoMinO3 to summary of model options
!Question - MODOPS seems to have double setting of some entries. In
!meset MODOPS(23) is also set (along with 24). Then in metext 26 & 26 are
!set. So setting to 28 here, as it seems to be the largest of the array not set
!(current array size is 30). Delete this extra comment during review.
         IF (NOMINO3) THEN
            MODOPS(28) = 'NoMinO3'
         END IF


C ---    Note that the non-DFAULT/BETA options of using met data processed
C        with the ADJ_U* option in AERMET or using MMIF-generated met inputs
C        are identified based on flags included in the surface file header.

         IF (SCIM .AND. NUMAVE.GT.0) THEN
C           Write Error Message:  Cannot use SCIM with short term averages
            CALL ERRHDL(PATH,MODNAM,'E','154','ST AVES')
         END IF
         IF (SCIM .AND. PERIOD) THEN
C           Write Error Message:  Cannot use SCIM with PERIOD average
            CALL ERRHDL(PATH,MODNAM,'E','154','PERIOD')
         END IF
         IF (SCIM .AND. DEPOS) THEN
C           Write Warning Message:  Ignore DEPOS when using SCIM
            DEPOS = .FALSE.
            NUMTYP = NUMTYP - 1
            CALL ERRHDL(PATH,MODNAM,'W','156',' DEPOS ')
         END IF

C        Adjust output label for ANNUAL average deposition fluxes
         IF (ANNUAL) THEN
            DO ITYP = 1, NUMTYP
               IF (.NOT.CONC .OR. ITYP.GT.1) THEN
                  PERLBL(ITYP) = 'GRAMS/M**2/YR'
               END IF
            END DO
         END IF

C ---    Check for completeness of O3 data inputs
         IF (L_O3Sector) THEN
C ---       Check for temporally varying O3VALUES concentrations, OZONEVAL, and/or O3FILEs by sector.
            DO I = 1, NUMO3Sects
               IF (L_O3File(I)) THEN
C                 O3FILE for this sector; check for O3VALUES
                  IF (L_O3VALUES(I)) THEN
C                    O3VALUES values available for this sector; check completeness
                     IF (IO3SET(I) .LT. IO3MAX(I)) THEN
C                       WRITE Error Message: Not Enough O3VALUES values
                        IF (IO3SET(I) .LT. 100) THEN
                           WRITE(DUMMY,'(''O3SECT'',I1,'' N='',I2)')
     &                                                  I, IO3SET(I)
                        ELSE
                           WRITE(DUMMY,'(''SECT'',I1,'' N='',I4)')
     &                                                  I, IO3SET(I)
                        END IF
                        CALL ERRHDL(PATH,MODNAM,'E','261',DUMMY)
                     END IF
                     ELSE IF (.NOT. L_O3VALUES(I).AND.
     &                     .NOT. L_O3VAL(I)) THEN
C ---                WRITE Warning Message: HOURLY O3FILE but no O3VALs avail for this Sector;
C                    full conversion assumed for missing hourly data (subject to equilibrium
C                    ratio)
                     WRITE(DUMMY,'(''O3SECT'',I1)') I
                     CALL ERRHDL(PATH,MODNAM,'W','271',DUMMY)
                  END IF
               ELSE IF (.NOT. L_O3File(I)) THEN
C                 No HOURLY O3 file available for this sector; check for O3VALUES
                  IF (L_O3VALUES(I)) THEN
C                    O3VALUES values available for this sector; check completeness
                     IF (IO3SET(I) .LT. IO3MAX(I)) THEN
C                       WRITE Error Message: Not Enough O3VALUES values
                        IF (IO3SET(I) .LT. 100) THEN
                           WRITE(DUMMY,'(''O3SECT'',I1,'' N='',I2)')
     &                                                  I, IO3SET(I)
                        ELSE
                           WRITE(DUMMY,'(''SECT'',I1,'' N='',I4)')
     &                                                  I, IO3SET(I)
                        END IF
                        CALL ERRHDL(PATH,MODNAM,'E','261',DUMMY)
                     END IF
                  ELSE IF (.NOT. L_O3VALUES(I).AND.
     &                     .NOT. L_O3VAL(I)) THEN
C                    WRITE Error Message: No O3VALUES values and no O3FILE - Missing Sector
                     WRITE(DUMMY,'(''O3SECT'',I1,'' Msg'')') I
                     CALL ERRHDL(PATH,MODNAM,'E','261',DUMMY)
                  END IF
               END IF
            END DO
         ELSE ! .NOT. L_O3Sector
C ---       No O3SECTORs, check for temporally varying O3VALUES concentrations and/or O3FILE
C           Set sector index (I) to 1
            I = 1
            IF (L_O3File(I)) THEN
C              O3FILE for this sector; check for O3VALUES
               IF (L_O3VALUES(I)) THEN
C                 O3VALUES values available for this sector; check completeness
                  IF (IO3SET(I) .LT. IO3MAX(I)) THEN
C                    WRITE Error Message: Not Enough O3VALUES values
                     WRITE(DUMMY,'(''NumVals='',I4)') IO3SET(I)
                     CALL ERRHDL(PATH,MODNAM,'E','261',DUMMY)
                  END IF
                  ELSE IF (.NOT. L_O3VALUES(I) .AND.
     &                     .NOT. L_O3VAL(I)) THEN
C ---             WRITE Warning Message: HOURLY O3FILE but no O3VALs avail for this Sector;
C                 full conversion assumed for missing hourly data (subject to equilibrium
C                 ratio)
                  CALL ERRHDL(PATH,MODNAM,'W','271',' ')
               END IF
            ELSE IF (.NOT. L_O3File(I)) THEN
               IF (L_O3VALUES(I)) THEN
C                 No O3FILE, but O3VALUES values available for this sector; check completeness
                  IF (IO3SET(I) .LT. IO3MAX(I)) THEN
C                    WRITE Error Message: Not Enough O3VALUES values
                     WRITE(DUMMY,'(''NumVals='',I4)') IO3SET(I)
                     CALL ERRHDL(PATH,MODNAM,'E','261',DUMMY)
                  END IF
               END IF
            END IF

         END IF

C ---    CERC 11/30/20 Check for completeness of NOx data inputs
         IF(L_NOxSector)THEN
C ---      Check for temporally varying NOXVALUE, NOX_VALS, NOX_FILE by sector.
           DO I = 1, NUMNOxSects
              IF (L_NOxFile(I)) THEN
C                 NOX_FILE for this sector; check for NOX_VALS
                  IF (L_NOX_VALS(I)) THEN
C                    NOX_VALS values available for this sector; check completeness
                     IF (INOXSET(I) .LT. INOXMAX(I)) THEN
C                       WRITE Error Message: Not Enough NOX_VALS values
                        IF (INOXSET(I) .LT. 100) THEN
                           WRITE(DUMMY,'(''NOXSCT'',I1,'' N='',I2)')
     &                                                  I, INOXSET(I)
                        ELSE
                           WRITE(DUMMY,'(''SECT'',I1,'' N='',I4)')
     &                                                  I, INOXSET(I)
                        END IF
                        CALL ERRHDL(PATH,MODNAM,'E','603',DUMMY)
                     END IF
                  ELSE IF (.NOT. L_NOX_VALS(I).AND.
     &                     .NOT. L_NOXVALUE(I)) THEN
C ---                WRITE Warning Message: HOURLY NOX_FILE but no NOX_VALS avail for this Sector;
C                    give zero concentration
                     WRITE(DUMMY,'(''NOXSECT'',I1)') I
                     CALL ERRHDL(PATH,MODNAM,'W','611',DUMMY)
                  END IF
               ELSE IF (.NOT. L_NOxFile(I)) THEN
C                 No HOURLY NOx file available for this sector; check for NOX_VALS
                  IF (L_NOX_VALS(I)) THEN
C                    NOX_VALS avaialable for this sector; check completeness
                     IF(INOXSET(I) .LT. INOXMAX(I))THEN
C                       WRITE Error Message: Not enough NOX_VALS values
                        IF (INOXSET(I) .LT. 100) THEN
                           WRITE(DUMMY, '(''NOXSCT'',I1,'' N='',I2)')
     &                                              I, INOXSET(I)
                        ELSE
                           WRITE(DUMMY,'(''SECT'',I1,'' N='',I4)')
     &                                            I, INOXSET(I)
                        END IF
                        CALL ERRHDL(PATH,MODNAM,'E','603',DUMMY)
                     END IF
                  ELSE IF (.NOT. L_NOX_VALS(I).AND.
     &                     .NOT. L_NOXVALUE(I)) THEN
C                    WRITE Error Message: No NOX_VALS values and no NOXFILE - Missing Sector
                     WRITE(DUMMY,'(''NOXSECT'',I1,'' Msg'')') I
                     CALL ERRHDL(PATH,MODNAM,'E','603',DUMMY)
                  END IF
               END IF
           END DO
        ELSE ! .NOT. L_NOxSector
C ---      NO NOxSectors, check for temporally-varying NOX_VALS concentrations
C          Set sector index (I) to 1
            I = 1
            IF (L_NOxFile(I)) THEN
C              NOX_FILE for this sector; check for NOX_VALS
               IF (L_NOX_VALS(I)) THEN
C                 NOX_VALS values available for this sector; check completeness
                  IF (INOXSET(I) .LT. INOXMAX(I)) THEN
C                    WRITE Error Message: Not Enough NOX_VALS values
                     WRITE(DUMMY,'(''NumVals='',I4)') INOXSET(I)
                     CALL ERRHDL(PATH,MODNAM,'E','603',DUMMY)
                  END IF
               ELSE IF (.NOT. L_NOX_VALS(I) .AND.
     &                     .NOT. L_NOXVALUE(I)) THEN
C ---             WRITE Warning Message: HOURLY NOX_FILE but no NOXVALs avail for this Sector;
C                 give zero concentration
                  CALL ERRHDL(PATH,MODNAM,'W','611',' ')
               END IF
            ELSE IF (.NOT. L_NOXFile(I)) THEN
               IF(L_NOX_VALS(I))THEN
C                 No NOXFILE but NOX_VALS values available for this sector; check completeness
                  IF (INOXSET(I) . LT. INOXMAX(I)) THEN
C                    WRITE Error Message: Not Enough NOX_VALS values
                     WRITE(DUMMY,'(''NumVals'',I4)') INOXSET(I)
                     CALL ERRHDL(PATH,MODNAM,'E','603',DUMMY)
                  END IF
              END IF
            END IF
         END IF

C ---    Check for user-specified ozone units; apply default if needed
         IF (ICSTAT(27) .NE. 1) THEN
            OzoneUnits = 'PPB'
         END IF

C ---    CERC 11/30/20 Check for user-specified NOx units; apply default if needed
         IF (ICSTAT(38) .NE. 1) THEN
            NOxUnits = 'PPB'
         END IF

C ---    Check for PM25 processing
         IF ((POLLUT .EQ. 'PM25'  .OR. POLLUT .EQ. 'PM-2.5' .OR.
     &        POLLUT .EQ. 'PM-25' .OR. POLLUT .EQ. 'PM2.5')) THEN
            IF(.NOT.L_NO_PM25AVE .AND. .NOT.NOCHKD .AND.
     &         .NOT.L_WARNCHKD   .AND. .NOT.EVONLY) THEN
C ---          Set logical flag for PM25 processing, averaged across years
               PM25AVE = .TRUE.
C ---          Now check for appropriate averaging periods for PM2.5
               IF (NUMAVE.GT.1 .OR. (NUMAVE.EQ.1 .AND.
     &                              KAVE(1).NE.24)) THEN
C ---             Write Error Message: Short Term average must be 24-hr only
                  DO I = 1, NUMAVE
                     IF (KAVE(I) .NE. 24) THEN
                        WRITE(DUMMY,'(I3,''-hr Ave'')') KAVE(I)
                        CALL ERRHDL(PATH,MODNAM,'E','363',DUMMY)
                     END IF
                  END DO
                  PM25AVE = .FALSE.
               END IF
               IF (PERIOD) THEN
C ---             Write Error Message: Long term average must be ANNUAL
                  CALL ERRHDL(PATH,MODNAM,'E','363','PERIOD Ave')
                  PM25AVE = .FALSE.
               END IF
            ELSE IF (.NOT.SCREEN .AND. .NOT.EVONLY) THEN
C ---          Set to false for NOCHKD or WARNCHKD options, without the SCREEN or
C              EVONLY options, and issue warning message
               IF (NOCHKD) THEN
                  DUMMY = 'NOCHKD'
               ELSE IF (L_WARNCHKD) THEN
                  DUMMY = 'WARNCHKD'
               END IF
               CALL ERRHDL(PATH,MODNAM,'W','363',DUMMY)
               PM25AVE = .FALSE.
            ELSE IF (SCREEN .OR. EVONLY .OR. L_NO_PM25AVE) THEN
C ---          Set PM25AVE to .FALSE. for SCREEN or EVONLY options or with
C              L_NO_PM25AVE option, without any warnings
               PM25AVE = .FALSE.
            END IF
         END IF

C ---    Check for NO2 1-hour NAAQS processing (NO2AVE = .T.)
         IF (POLLUT .EQ. 'NO2' .AND. .NOT.L_NO_NO2AVE .AND.
     &             .NOT.NOCHKD .AND. .NOT.L_WARNCHKD  .AND.
     &                               .NOT.SCREEN .AND. .NOT.EVONLY) THEN
C ---       No options precluding NO2 1-hr NAAQS processing are specified;
C           next check for averaging periods to determine if multi-year
C           processing of maximum daily 1-hour averages is being done
            IF (NUMAVE.EQ.1 .AND. KAVE(1).EQ.1 .AND. .NOT.PERIOD
     &                                         .AND. .NOT.ANNUAL) THEN
C ---          Set logical flag for 1-hr NO2 processing, averaged across years,
C              without PERIOD or ANNUAL averages
               NO2AVE = .TRUE.
            ELSE IF (NUMAVE.EQ.1 .AND. KAVE(1).EQ.1 .AND. PERIOD
     &                                              .AND. MULTYR) THEN
C ---          Set logical flag for 1-hr NO2 processing, averaged across years,
C              using MULTYEAR option to address PERIOD averages
               NO2AVE = .TRUE.
            ELSE IF (NUMAVE.EQ.1 .AND. KAVE(1).EQ.1 .AND.
     &                                       (PERIOD .OR. ANNUAL) .AND.
     &                                               .NOT.MULTYR) THEN
C ---          Write Warning Message: PERIOD averages should not be
C              processed with 1-hour averages for NO2 unless only 1 year
C              of met data is being processed or if the MULTYEAR option
C              is specified.
               CALL ERRHDL(PATH,MODNAM,'W','361','MULTYEAR Opt')
C ---          Allow processing to continue, but long-term results
C              may be wrong.
               NO2AVE = .TRUE.
            ELSE IF (NUMAVE.GT.1 .OR.
     &              (NUMAVE.EQ.1 .AND. KAVE(1).NE.1) .AND.
     &                                       (PERIOD .OR. ANNUAL) .AND.
     &                                               .NOT.MULTYR) THEN
C ---          Write Warning Message: PERIOD averages should not be
C              processed with 1-hour averages for NO2 unless only 1 year
C              of met data is being processed or if the MULTYEAR option
C              is specified.  If NUMAVE > 1 then a non-standard short-term
C              average is being selected, and short-term averages other than
C              1-hour cannot be processed with the 1-hr NO2 NAAQS.
               CALL ERRHDL(PATH,MODNAM,'W','361','MULTYEAR Opt')
C ---          Write Warning Message: Non-standard short term average for NO2,
C              and disable special processing for 1-hour NAAQS.
               DO I = 1, NUMAVE
                  IF (KAVE(I) .NE. 1) THEN
                     WRITE(DUMMY,'(I3,''-hr Ave'')') KAVE(I)
                     CALL ERRHDL(PATH,MODNAM,'W','362',DUMMY)
                  END IF
               END DO
               NO2AVE = .FALSE.
            ELSE IF (NUMAVE.GT.1 .OR.
     &              (NUMAVE.EQ.1 .AND. KAVE(1).NE.1) ) THEN
C ---          Write Warning Message: Non-standard short term average for NO2
C ---          Write Warning Message: Short Term average should be 1-hr only for NO2
               DO I = 1, NUMAVE
                  IF (KAVE(I) .NE. 1) THEN
                     WRITE(DUMMY,'(I3,''-hr Ave'')') KAVE(I)
                     CALL ERRHDL(PATH,MODNAM,'W','362',DUMMY)
                  END IF
               END DO
               NO2AVE = .FALSE.
            ELSE
C ---          Period or Annual average only, set NO2AVE = .F. but allow
C              processing
               NO2AVE = .FALSE.
            END IF
         ELSE IF (POLLUT .EQ. 'NO2' .AND. .NOT.SCREEN .AND.
     &                                    .NOT.EVONLY) THEN
C ---       Set NO2AVE to false for NOCHKD, WARNCHKD and L_NO_NO2AVE options, without
C           the SCREEN or EVONLY options; issue warning messages for NOCHKD or WARNCHKD
C           (message has already been issued for L_NO_NO2AVE), and disable special
C           processing for 1-hour NAAQS.
            IF (NUMAVE.EQ.1 .AND. KAVE(1).EQ.1 .AND.
     &                           ((.NOT.PERIOD .AND. .NOT.ANNUAL) .OR.
     &                                      (PERIOD .AND. MULTYR) .OR.
     &                  ((PERIOD .OR. ANNUAL) .AND. .NOT.MULTYR)) )THEN
               IF (NOCHKD) THEN
                  DUMMY = 'NOCHKD'
                  CALL ERRHDL(PATH,MODNAM,'W','362',DUMMY)
               ELSE IF (L_WARNCHKD) THEN
                  DUMMY = 'WARNCHKD'
                  CALL ERRHDL(PATH,MODNAM,'W','362',DUMMY)
               END IF
               NO2AVE = .FALSE.
            END IF
         ELSE IF (POLLUT .EQ. 'NO2' .AND. (SCREEN .OR. EVONLY)) THEN
C ---       Set NO2AVE to .FALSE. for SCREEN or EVONLY options, without any warnings
            NO2AVE = .FALSE.
         END IF

C ---    Check for SO2 1-hour NAAQS processing (SO2AVE = .T.)
         IF (POLLUT .EQ. 'SO2' .AND. .NOT.NOCHKD .AND. .NOT.L_NO_SO2AVE
     &                     .AND. .NOT.L_WARNCHKD .AND. .NOT.EVONLY) THEN
C ---       No options precluding SO2 1-hr NAAQS processing are specified;
C           next check for averaging periods to determine if multi-year
C           processing of maximum daily 1-hour averages is being done
            IF (NUMAVE.EQ.1 .AND. KAVE(1).EQ.1 .AND. .NOT.PERIOD
     &                                         .AND. .NOT.ANNUAL) THEN
C ---          Set logical flag for 1-hr SO2 processing, averaged across years
               SO2AVE = .TRUE.
            ELSE IF (NUMAVE.EQ.1 .AND. KAVE(1).EQ.1 .AND. PERIOD
     &                                              .AND. MULTYR) THEN
C ---          Set logical flag for 1-hr SO2 processing, averaged across years,
C              using MULTYEAR option to address PERIOD averages
               SO2AVE = .TRUE.
            ELSE IF (NUMAVE.EQ.1 .AND. KAVE(1).EQ.1 .AND.
     &                                       (PERIOD .OR. ANNUAL) .AND.
     &                                               .NOT.MULTYR) THEN
C ---          Write Warning Message: PERIOD averages should not be
C              processed with 1-hour averages for SO2 unless only 1 year
C              of met data is being processed or if the MULTYEAR option
C              is specified.
               CALL ERRHDL(PATH,MODNAM,'W','361','MULTYEAR Opt')
C ---          Allow processing to continue, but long-term results
C              may be wrong.
               SO2AVE = .TRUE.
            ELSE IF (NUMAVE.GT.1 .AND. MINVAL(KAVE).EQ.1 .AND.
     &                                       (PERIOD .OR. ANNUAL) .AND.
     &                                               .NOT.MULTYR) THEN
C ---          Write Warning Message: PERIOD averages should not be
C              processed with 1-hour averages for SO2 unless only 1 year
C              of met data is being processed or if the MULTYEAR option
C              is specified. Also, short-term averages other than
C              1-hour cannot be processed with the 1-hr SO2 NAAQS.
               CALL ERRHDL(PATH,MODNAM,'W','361','MULTYEAR Opt')
C ---          Write Warning Message: Non-standard short term average for SO2,
C              and disable special processing for 1-hour NAAQS.
               DO I = 1, NUMAVE
                  IF (KAVE(I) .NE. 1) THEN
                     WRITE(DUMMY,'(I3,''-hr Ave'')') KAVE(I)
                     CALL ERRHDL(PATH,MODNAM,'W','362',DUMMY)
                  END IF
               END DO
C ---          Allow processing to continue, but without special processing
C              of 1-hr values averaged across years
               SO2AVE = .FALSE.
            ELSE IF (NUMAVE.GT.1 .AND. MINVAL(KAVE).EQ.1) THEN
C ---          Write Warning Message: Non-standard short term average for SO2,
C              and disable special processing for 1-hour NAAQS.
               DO I = 1, NUMAVE
                  IF (KAVE(I) .NE. 1) THEN
                     WRITE(DUMMY,'(I3,''-hr Ave'')') KAVE(I)
                     CALL ERRHDL(PATH,MODNAM,'W','362',DUMMY)
                  END IF
               END DO
C ---          Allow processing to continue, but without special processing
C              of 1-hr values averaged across years
               SO2AVE = .FALSE.
            ELSE
C ---          Period or Annual average only, set SO2AVE = .F. but allow
C              processing
               SO2AVE = .FALSE.
            END IF
         ELSE IF (POLLUT .EQ. 'SO2' .AND. .NOT.SCREEN .AND.
     &                                    .NOT.EVONLY) THEN
C ---       Set SO2AVE to false for NOCHKD or WARNCHKD options, without the
C           SCREEN or EVONLY options, issue warning message, and disable
C           special processing for 1-hour NAAQS.
            IF (NUMAVE.EQ.1 .AND. KAVE(1).EQ.1 .AND.
     &                           ((.NOT.PERIOD .AND. .NOT.ANNUAL) .OR.
     &                                      (PERIOD .AND. MULTYR) .OR.
     &                   ((PERIOD .OR. ANNUAL) .AND. .NOT.MULTYR)) )THEN
               IF (NOCHKD) THEN
                  DUMMY = 'NOCHKD'
               ELSE IF (L_WARNCHKD) THEN
                  DUMMY = 'WARNCHKD'
               END IF
               CALL ERRHDL(PATH,MODNAM,'W','362',DUMMY)
               SO2AVE = .FALSE.
            END IF
         ELSE IF (POLLUT .EQ. 'SO2' .AND. (SCREEN .OR. EVONLY)) THEN
C ---       Set SO2AVE to .FALSE. for SCREEN or EVONLY options, without any warnings
            SO2AVE = .FALSE.
         END IF

C ---    Check for pollutant ID = 'NO2' for PVMRM, OLM, ARM2 and GRSM options
         IF ((PVMRM .OR. OLM .OR. ARM2 .OR. GRSM) .AND.
     &                                          POLLUT .NE. 'NO2') THEN
C           Write Error Message:  Pollutant ID doesn't match option
            CALL ERRHDL(PATH,MODNAM,'E','284',' NO2 ')
         END IF

C ---    Check for PM25, NO2, or SO2 processing based on ranked values
C        averaged across years, and adjust PLOTFILE format accordingly
         IF (PM25AVE .OR. NO2AVE .OR. SO2AVE) THEN
           PLTFRM = '(3(1X,F13.5),3(1X,F8.2),2X,A6,2X,A8,2X,A5,5X,A8,2X,
     &               10(F13.5,2X,I8.8,2X:))'
         END IF

C ---    AWMA enhancements to PRIME
C ---    AWMADWNW and ORD_DWNW and have different ways to define
C          the height at which to compute the effective wind speed.
C          One or the other can be specified, or neither, but BOTH CANNOT
C          be specified.  Check for that here.  A similar check is performed
C          in ORD_DOWNWASH

         IF (L_AWMA_UEFF .AND. L_ORD_UEFF) THEN
C ---       Write error message
            CALL ERRHDL(PATH,MODNAM,'E','124','  ')
         END IF

C ---    Check for conflicts between AWMA and ORD Ueff options (which
C         is an error) as well as AWMAUTurb and AWMAUturbHX (which only
C         issues a warning)
         IF (L_AWMA_UTurb .AND. L_AWMA_UTurbHX) THEN
            CALL ERRHDL(PATH,MODNAM,'W','478',' ')
         END IF

         GO TO 1000

C        WRITE Error Message for Error Opening File
 99      CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)
         IF (DUMMY .EQ. 'SAVEFILE') THEN
C           Reset Logical Flag for SAVEFILE Option Due to Error Opening File
            RSTSAV = .FALSE.
         ELSE IF (DUMMY .EQ. 'INITFILE') THEN
C           Reset Logical Flag for INITFILE Option Due to Error Opening File
            RSTINP = .FALSE.
         END IF

 1000    CONTINUE

      ELSE
C        Write Error Message: Invalid Keyword for This Pathway
         CALL ERRHDL(PATH,MODNAM,'E','110',KEYWRD)
      END IF

 999  RETURN
      END

      SUBROUTINE TITLES
C***********************************************************************
C                 TITLES Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Process Title Information From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Title Strings for Model Outputs
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'TITLES'

      IF (KEYWRD .EQ. 'TITLEONE') THEN
         TITLE1 = RUNST1(LOCE(2)+2:MIN(LEN_TRIM(RUNST1),
     &                                         (LOCE(2)+2+ILEN_FLD-1)))
         IF (TITLE1 .EQ. ' ') THEN
C*          Write Error Message: Missing Parameter Title
            CALL ERRHDL(PATH,MODNAM,'W','200',KEYWRD)
         END IF

      ELSE IF (KEYWRD .EQ. 'TITLETWO') THEN
         TITLE2 = RUNST1(LOCE(2)+2:MIN(LEN_TRIM(RUNST1),
     &                                         (LOCE(2)+2+ILEN_FLD-1)))
         IF (TITLE2 .EQ. ' ') THEN
C*          Write Warning Message
            CALL ERRHDL(PATH,MODNAM,'W','200',KEYWRD)
         END IF

      END IF

      RETURN
      END


      SUBROUTINE MODOPT
C***********************************************************************
C                 MODOPT Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Process Modeling Options From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   Added MODELOPT keyword RLINEFDH.
C                    Default FAC_DISPHT == 5.0D0, if RLINEFDH (& ALPHA)is
C                    used FAC_DISPHT == 0.0D0 to match AERMOD's surface
C                    wind profile.
C                    Michelle G. Snyder, Wood - 6/22/2021
C
C        MODIFIED:   To add GRSM NO2 option
C                    CERC, 11/30/20
C
C        MODIFIED:   To incorporate additional options and adjust
C                    the handling of options that conflict with the
C                    regulatory DFAULT option.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG - 10/19/2009
C
C        MODIFIED:   To incorporate undocumented options to turn off
C                    depletion, which is now the default.
C                    R. W. Brode, MACTEC/PES - 10/26/2004
C
C        MODIFIED:   To allow for calculating CONC/DEPOS/DDEP/WDEP in
C                    a single model run.
C                    R. W. Brode, PES - 4/17/95
C
C        MODIFIED:   To add DDEP and WDEP parameters to CONC/DEPOS options
C                    to allow just the wet or just the dry deposition flux
C                    to be reported.  DEPOS now reports the sum of wet and
C                    dry fluxes.
C                    D. Strimaitis, SRC - 11/8/93
C
C        MODIFIED:   To add DEPLETE parameter for plume depletion option
C                    D. Strimaitis, SRC - 2/15/93
C
C        MODIFIED:   To Output Warning Message '206' For Overriding
C                    Non-DEFAULT Option - 9/29/92
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Modeling Option Logical Switch Settings
C
C        ERROR HANDLING:   Checks for Too Few or Too Many Option Keywords;
C                          Checks for Invalid Option Keywords;
C                          Checks for Conflicting or Missing Option Keywords
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      USE RLINE_DATA, ONLY: FAC_DISPHT !needed for RLINEFDH MODELOPT (Wood 6/22/2021)
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I
      CHARACTER KOPT*9

C     Variable Initializations - Initialize All Logical Switches to FALSE
      MODNAM = 'MODOPT'

C     Check for Too Few or Too Many Parameters
      IF (IFC .LT. 3) THEN
C        WRITE Error Message     ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      ELSE IF (IFC .GT. 14) THEN
C        WRITE Warning Message   ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'W','202',KEYWRD)
      END IF

C --- Assign DEFAULT value of 0.2D0 to SVMIN, minimum
C     sigma-v value (formerly assigned as a PARAMETER);
CCRT  value of SVMIN may be increased to 0.5D0 using
CCRT  the ALPHA option and the CO LOW_WIND keyword:
      SVMIN = 0.2D0

C --- Assign DEFAULT value for minimum wind speed of
C     0.2828 m/s, equivalent to SQRT(2*SVMIN*SVMIN)
C     used in previous versions.
CCRT  User can change the value of WSMIN using the ALPHA model
CCRT  option and the CO LOW_WIND keyword.
      WSMIN = 0.2828D0

CCRT  Assign DEFAULT value of 1.0 for the maximum meander
CCRT  factor, FRANMAX. The value of FRANMAX was adjusted
CCRT  to 0.95 under the former LowWind2 BETA option, and the
CCRT  user could modify the value used under the LowWind2
CCRT  option using the CO LOW_WIND keyword, within a
CCRT  range from 0.50 to 1.0, inclusive.
CCRT  1/29/2018 LowWind2 has been removed as a BETA option. The user
CCRT  can now modify the FRANMAX value is using the ALPHA
CCRT  option and the CO LOW_WIND keyword
      FRANMAX = 1.0D0

C**   3/18/2022 add FRANMIN for meander testing
C**   range from 0 to 1. Default value is 0 -Wood
      FRANMIN = 0.0D0

CCRT  CRT 9/11/2020, D062 User Minimum Sigma W
CCRT  Assign DEFAULT value for minimum sigma w of 0.02 m/s
CCRT  (formerly assigned as a PARAMETER;
CCRT  Value can now be assigned by the user using the the ALPHA
CCRT  model option and the CO LOW_WIND keyword.
      SWMIN = 0.02D0

CRCO  RCO 9/27/2020, D061 User BIGT value
CRCO  Assign DEFAULT value for BIGT value of 24 hours
CRCO  (formerly a PARAMETER in MEANDR routine in calc2)
CRCO  Value can now be assigned by the user using the the ALPHA
CRCO  model option and the CO LOW_WIND keyword.
      BIGT = 24.0D0

CMGS Added MODELOPT keyword RLINEFDH, which will change this value
CMGS (from RLINE_v1_2) to =0.0. Wood 6/22/2021
      FAC_DISPHT  = 5.0D0


C --- Assign DEFAULT value of 2.15 for the SZCOEF parameter
C     used in the IBL calculations.
      SZCOEF = 2.15D0

C     First Check for Presence of DFAULT Switch
      DO I = 3, IFC
         KOPT = FIELD(I)
         IF (KOPT .EQ. 'DFAULT' .OR. KOPT .EQ. 'DEFAULT') THEN
            DFAULT      = .TRUE.
            ELEV        = .TRUE.
            FLAT        = .FALSE.
            FLATSRCS    = .FALSE.
            MSGPRO      = .TRUE.
            NOSTD       = .FALSE.
            NOCHKD      = .FALSE.
            SCREEN      = .FALSE.
            SCIM        = .FALSE.
            PSDCREDIT   = .FALSE.
            BETA        = .FALSE.
            L_ALPHA     = .FALSE.
            FASTAREA    = .FALSE.
            FASTALL     = .FALSE.
            L_EFFSIGY   = .FALSE.
            LOW_WIND    = .FALSE.
            L_NonDFAULT = .FALSE.
            L_UrbanTransition = .TRUE.
            GRSM       = .FALSE.
            NOMINO3    = .FALSE.

            EXIT
         END IF
      END DO

C     Next check for presence of both FLAT and ELEV if NOT.DFAULT
      IF (.NOT. DFAULT) THEN
C        First look for FLAT
         DO I = 3, IFC
            KOPT = FIELD(I)
            IF (KOPT .EQ. 'FLAT') THEN
               FLAT = .TRUE.
               ELEV = .FALSE.
               EXIT
            END IF
         END DO
C        If FLAT, next look for ELEV, indicating both FLAT and
C        ELEV sources in the same run (FLATSRCS)
         IF (FLAT) THEN
            DO I = 3, IFC
               KOPT = FIELD(I)
               IF (KOPT .EQ. 'ELEV') THEN
                  ELEV     = .TRUE.
                  FLATSRCS = .TRUE.
                  EXIT
               END IF
            END DO
         END IF
      ELSE
C        Look for FLAT with DFAULT
         DO I = 3, IFC
            KOPT = FIELD(I)
            IF (KOPT .EQ. 'FLAT') THEN
C              WRITE Warning Message     ! Non-DEFAULT Option Overridden
               CALL ERRHDL(PATH,MODNAM,'W','206',KOPT)
            END IF
         END DO
      END IF

C     Next Check for Presence of BETA Switch
      DO I = 3, IFC
         KOPT = FIELD(I)
         IF (KOPT .EQ. 'BETA' .AND. .NOT.DFAULT) THEN
            BETA = .TRUE.
         ELSE IF (KOPT .EQ. 'ALPHA' .AND. .NOT.DFAULT) THEN
            L_ALPHA = .TRUE.
         END IF
      END DO

      NUMTYP = 0
C     Loop Through Fields Again Setting All Swithes
      DO I = 3, IFC
         KOPT = FIELD(I)
         IF (KOPT .EQ. 'DFAULT' .OR. KOPT .EQ. 'DEFAULT') THEN
            DFAULT = .TRUE.
         ELSE IF (KOPT .EQ. 'CONC') THEN
            IF (.NOT. CONC) THEN
               CONC   = .TRUE.
               NUMTYP = NUMTYP + 1
            END IF
         ELSE IF (KOPT .EQ. 'DEPOS') THEN
            IF (.NOT. DEPOS) THEN
               DEPOS  = .TRUE.
               NUMTYP = NUMTYP + 1
            END IF
         ELSE IF (KOPT .EQ. 'DDEP') THEN
            IF (.NOT. DDEP) THEN
               DDEP   = .TRUE.
               NUMTYP = NUMTYP + 1
            END IF
         ELSE IF (KOPT .EQ. 'WDEP') THEN
            IF (.NOT. WDEP) THEN
               WDEP   = .TRUE.
               NUMTYP = NUMTYP + 1
            END IF
         ELSE IF (KOPT .EQ. 'FLAT' .OR. KOPT .EQ. 'ELEV') THEN
            CYCLE
         ELSE IF (KOPT .EQ. 'DRYDPLT' .AND. .NOT.NODRYDPLT) THEN
            DDPLETE = .TRUE.
            DRYDPLT = .TRUE.
         ELSE IF (KOPT .EQ. 'DRYDPLT' .AND. NODRYDPLT) THEN
C ---       Write Error Message        ! Conflicting options specified
            CALL ERRHDL(PATH,MODNAM,'E','149',KOPT)
         ELSE IF (KOPT .EQ. 'NODRYDPLT' .AND. .NOT.DRYDPLT) THEN
C           Dry depletion is now standard - include "option" to override it
            DDPLETE = .FALSE.
C           Set separate logical for user-specified option to ensure that
C           it is reflected in the page header
            NODRYDPLT = .TRUE.
         ELSE IF (KOPT .EQ. 'NODRYDPLT' .AND. DRYDPLT) THEN
C ---       Write Error Message        ! Conflicting options specified
            CALL ERRHDL(PATH,MODNAM,'E','149',KOPT)
         ELSE IF (KOPT .EQ. 'ROMBERG') THEN
            ROMBERG = .TRUE.
            DDPLETE = .TRUE.
         ELSE IF (KOPT .EQ. 'AREADPLT') THEN
            IF (.NOT. DFAULT) THEN
               ARDPLETE = .TRUE.
               DDPLETE  = .TRUE.
            ELSE
C              WRITE Error Message     ! Non-DEFAULT Option Conflict
               CALL ERRHDL(PATH,MODNAM,'E','204',KOPT)
            END IF
         ELSE IF (KOPT .EQ. 'WETDPLT' .AND. .NOT.NOWETDPLT) THEN
            WDPLETE = .TRUE.
            WETDPLT = .TRUE.
         ELSE IF (KOPT .EQ. 'WETDPLT' .AND. NOWETDPLT) THEN
C ---       Write Error Message        ! Conflicting options specified
            CALL ERRHDL(PATH,MODNAM,'E','149',KOPT)
         ELSE IF (KOPT .EQ. 'NOWETDPLT' .AND. .NOT.WETDPLT) THEN
C           Wet depletion is now standard - include "option" to override it
            WDPLETE = .FALSE.
C           Set separate logical for user-specified option to ensure that
C           it is reflected in the page header
            NOWETDPLT = .TRUE.
         ELSE IF (KOPT .EQ. 'NOWETDPLT' .AND. WETDPLT) THEN
C ---       Write Error Message        ! Conflicting options specified
            CALL ERRHDL(PATH,MODNAM,'E','149',KOPT)
         ELSE IF (KOPT .EQ. 'NOSTD') THEN
            IF (.NOT. DFAULT) THEN
               NOSTD = .TRUE.
            ELSE
C              WRITE Warning Message     ! Non-DEFAULT Option Overridden
               CALL ERRHDL(PATH,MODNAM,'W','206',KOPT)
               NOSTD = .FALSE.
            END IF
         ELSE IF (KOPT .EQ. 'NOWARN') THEN
            NOWARN = .TRUE.
         ELSE IF (KOPT .EQ. 'NOCHKD') THEN
            IF (.NOT. DFAULT .AND. .NOT.L_WARNCHKD) THEN
               NOCHKD = .TRUE.
            ELSE IF (.NOT.DFAULT .AND. L_WARNCHKD) THEN
C ---          Write Error Message        ! Conflicting options specified
               CALL ERRHDL(PATH,MODNAM,'E','149',KOPT)
            ELSE
C              WRITE Warning Message     ! Non-DEFAULT Option Overridden
               CALL ERRHDL(PATH,MODNAM,'W','206',KOPT)
               NOCHKD = .FALSE.
            END IF
         ELSE IF (KOPT .EQ. 'WARNCHKD') THEN
            IF (.NOT. NOCHKD) THEN
               L_WARNCHKD = .TRUE.
            ELSE
C ---          Write Error Message        ! Conflicting options specified
               CALL ERRHDL(PATH,MODNAM,'E','149',KOPT)
            END IF
         ELSE IF (KOPT .EQ. 'SCREEN') THEN
            IF (.NOT. DFAULT) THEN
               SCREEN = .TRUE.
C              Set NOCHKD option on for SCREEN mode
               NOCHKD = .TRUE.
            ELSE
C              WRITE Warning Message     ! Non-DEFAULT Option Overridden
               CALL ERRHDL(PATH,MODNAM,'W','206',KOPT)
               SCREEN = .FALSE.
            END IF
         ELSE IF (KOPT .EQ. 'SCIM') THEN
            IF (.NOT. DFAULT) THEN
               SCIM = .TRUE.
            ELSE
C              WRITE Error Message     ! Non-DEFAULT Option Conflict
               CALL ERRHDL(PATH,MODNAM,'E','204',KOPT)
               SCIM = .FALSE.
            END IF
         ELSE IF (KOPT .EQ. 'TOXICS') THEN
C ---       WRITE Warning Message        ! TOXICS option is obsolete
C           If this run includes area or openpit sources, refer to
C           FASTAREA option and set the logical flag
            IF (NAREA .GT. 0 .OR. NLINE .GT. 0 .OR. NPIT .GT. 0) THEN
               CALL ERRHDL(PATH,MODNAM,'W','198','FASTAREA')
            ELSE
               CALL ERRHDL(PATH,MODNAM,'W','198','        ')
            END IF
            IF (.NOT. DFAULT) THEN
               IF (NAREA .GT. 0 .OR. NLINE .GT. 0 .OR. NPIT .GT. 0) THEN
C ---             Assign FASTAREA option to TRUE for consistency with
C                 area source optimizations under obsolete TOXICS option
                  FASTAREA = .TRUE.
               END IF
            ELSE
C              WRITE Error Message     ! Non-DEFAULT Option Conflict
               CALL ERRHDL(PATH,MODNAM,'E','204',KOPT)
            END IF
         ELSE IF (KOPT .EQ. 'PVMRM') THEN
            PVMRM = .TRUE.
         ELSE IF (KOPT .EQ. 'OLM') THEN
            OLM = .TRUE.
         ELSE IF (KOPT .EQ. 'TTRM') THEN
            RUNTTRM = .TRUE.
            IF (.NOT. L_ALPHA) THEN
C              WRITE Error Message     ! ALPHA Option Required for TTRM
               CALL ERRHDL(PATH,MODNAM,'E','198',KOPT)
            ENDIF
!! Added Nov. 2021
         ELSE IF (KOPT .EQ. 'TTRM2') THEN
            RUNTTRM2 = .TRUE.
            RUNTTRM = .TRUE.
            IF (.NOT. L_ALPHA) THEN
C              WRITE Error Message     ! ALPHA Option Required for TTRM2
               CALL ERRHDL(PATH,MODNAM,'E','198',KOPT)
!! End TTRM2 insert
            END IF
            IF (DFAULT) THEN
C              WRITE Error Message     ! Non-DEFAULT Option Conflict
               CALL ERRHDL(PATH,MODNAM,'E','204',KOPT)
            END IF
         ELSE IF (KOPT .EQ. 'ARM2') THEN
            ARM2 = .TRUE.
            IF( DFAULT )THEN
               ARM2_Min = 0.50D0
               ARM2_Max = 0.90D0
            ENDIF
         ELSE IF (KOPT .EQ. 'GRSM') THEN
C          CERC 11/30/20
CCRT 4/1/2022 GRSM in version 21112 updated from ALPHA to BETA
            GRSM = .TRUE.
            IF (.NOT. BETA) THEN
C              WRITE Error Message     ! BETA Option Required for GRSM
               CALL ERRHDL(PATH,MODNAM,'E','199',KOPT)
            END IF
            IF (DFAULT) THEN
C              WRITE Error Message     ! Non-DEFAULT Option Conflict
               CALL ERRHDL(PATH,MODNAM,'E','204',KOPT)
            END IF
CCRT 1/29/2018 Move PSDCREDIT from BETA to ALPHA option
         ELSE IF (KOPT .EQ. 'PSDCREDIT') THEN                           ! jop 093006
            IF (L_ALPHA .AND. .NOT. DFAULT) THEN
               PSDCREDIT = .TRUE.
            ELSE IF (.NOT. L_ALPHA .AND. .NOT. DFAULT) THEN
C              WRITE Error Message     ! ALPHA Option Required for PSDCREDIT
               CALL ERRHDL(PATH,MODNAM,'E','198',KOPT)
            ELSE
C              WRITE Error Message     ! Non-DEFAULT Option Conflict
               CALL ERRHDL(PATH,MODNAM,'E','204',KOPT)
            END IF
CCRT 1/29/2018 Remove restriction that user can only specify ALPHA or BETA
         ELSE IF (KOPT .EQ. 'ALPHA') THEN
CCRT            IF (BETA) THEN
C              WRITE Error Message     ! Non-DEFAULT Option Conflict
CCRT               CALL ERRHDL(PATH,MODNAM,'E','204','ALPHA & BETA')
CCRT            ELSE IF (.NOT. DFAULT) THEN
            IF (.NOT. DFAULT) THEN
               L_ALPHA = .TRUE.
            ELSE
C              WRITE Error Message     ! Non-DEFAULT Option Conflict
               CALL ERRHDL(PATH,MODNAM,'E','204',KOPT)
            END IF
CCRT 1/29/2018 Remove restriction that user can only specify ALPHA or BETA
         ELSE IF (KOPT .EQ. 'BETA') THEN
CCRT            IF (L_ALPHA) THEN
C              WRITE Error Message     ! Non-DEFAULT Option Conflict
CCRT               CALL ERRHDL(PATH,MODNAM,'E','204','ALPHA & BETA')
CCRT            ELSE IF (.NOT. DFAULT) THEN
            IF (.NOT. DFAULT) THEN
               BETA = .TRUE.
            ELSE
C              WRITE Error Message     ! Non-DEFAULT Option Conflict
               CALL ERRHDL(PATH,MODNAM,'E','204',KOPT)
            END IF
         ELSE IF (KOPT .EQ. 'FASTAREA') THEN
            IF (FASTALL) THEN
C              Issue warning message since FASTALL implies FASTAREA
               CALL ERRHDL(PATH,MODNAM,'W','192','        ')
            END IF
            IF (.NOT. DFAULT) THEN
C              Set logical flag for FASTAREA for optimized area source;
C              equivalent to optimizations associated with obsolete TOXICS option
               FASTAREA = .TRUE.
            ELSE
C              WRITE Error Message     ! Non-DEFAULT Option Conflict
               CALL ERRHDL(PATH,MODNAM,'E','204',KOPT)
            END IF
         ELSE IF (KOPT .EQ. 'FASTALL') THEN
            IF (FASTAREA) THEN
C              Issue warning message since FASTALL implies FASTAREA
               CALL ERRHDL(PATH,MODNAM,'W','192','        ')
            END IF
            IF (.NOT. DFAULT) THEN
C ---          Set logical flag for FASTAREA for optimized area source;
C              equivalent to optimizations associated with obsolete TOXICS option.
               FASTAREA = .TRUE.
C ---          Also set L_EFFSIGY option flag for optimized meander option for
C              point and volume sources.
               FASTALL   = .TRUE.
               L_EFFSIGY = .TRUE.
            ELSE
C              WRITE Error Message     ! Non-DEFAULT Option Conflict
               CALL ERRHDL(PATH,MODNAM,'E','204',KOPT)
            END IF
         ELSE IF (KOPT .EQ. 'NOURBTRAN') THEN
            IF (.NOT. DFAULT) THEN
C ---          Non-regulatory option to ignore transition from nighttime urban
C              enhanced boundary layer to daytime convective boundary
C ---          Set logical switch to account for urban transition to .F. and
C              issue warning message
               L_UrbanTransition = .FALSE.
C              WRITE Warning Message
               CALL ERRHDL(PATH,MODNAM,'W','151','Keyword     ')
            ELSE
C              WRITE Error Message     ! Non-DEFAULT Option Conflict
               CALL ERRHDL(PATH,MODNAM,'E','204',KOPT)
            END IF

C -----  Add option for use of vector-mean wind speeds, VECTORWS
         ELSE IF (KOPT .EQ. 'VECTORWS') THEN
            L_VECTORWS = .TRUE.
C ---       Write Warning Message
            CALL ERRHDL(PATH,MODNAM,'W','116',KOPT)

C -----  Add ALPHA option for low-wind-speed modifications, LOWWIND
         ELSE IF (KOPT .EQ. 'LOW_WIND') THEN
            IF (L_ALPHA) THEN
C ---          Assign LOW_WIND = .TRUE.
               LOW_WIND = .TRUE.
               CALL ERRHDL(PATH,MODNAM,'W','136','NonDFAULT')
               DFAULT = .FALSE.
            ELSE
               LOW_WIND = .FALSE.
               CALL ERRHDL(PATH,MODNAM,'E','133','NonDFAULT')
            END IF

            IF (.NOT.DFAULT) THEN
C              WRITE Error Message     ! ALPHA option required
               CALL ERRHDL(PATH,MODNAM,'W','198',KOPT)
            ELSE
C              WRITE Error Message     ! DFAULT option specified;
C                                        Issue ERROR message
               CALL ERRHDL(PATH,MODNAM,'E','204',KOPT)
            END IF


C ---       Assign minimum wind speed, WSMIN
C WSMIN is already set above, approx. line 1241. Currently no "default"
C value for the LOW_WIND option specifically (which would be set here).
            WSMIN = 0.2828D0

C ---       Add option for RLINE to use zero displacement height in wind profile
C            Michelle G. Snyder (Wood 6/22/2021)
         ELSE IF (KOPT .EQ. 'RLINEFDH') THEN
            IF (L_ALPHA) THEN !Require ALPHA on MODELOPT
               FAC_DISPHT = 0.0D0
            ELSE
               CALL ERRHDL(PATH,MODNAM,'E','198','RLINEFDH')
            END IF

CRCO 1/7/21 D074 Add NOMINO3 option
         ELSE IF (KOPT .EQ. 'NOMINO3') THEN
            NOMINO3 = .TRUE.
            IF (.NOT.OLM .AND. .NOT.PVMRM .AND. .NOT.GRSM
     &          .AND. .NOT.RUNTTRM) THEN
C              WRITE Warning Message  ! NOMINO3 set without NO2 technique
               CALL ERRHDL(PATH,MODNAM,'W','621',KOPT)
            END IF

         ELSE !Not a vaild model option
C           WRITE Error Message     ! Invalid Parameter
            CALL ERRHDL(PATH,MODNAM,'E','203',KOPT)


         END IF

      END DO

C --- Check for conflicting NO2 options:
      IF (OLM .AND. PVMRM) THEN
C        WRITE Error Message       ! Can't specify OLM & PVMRM
         CALL ERRHDL(PATH,MODNAM,'E','141','OLM & PVMRM')
      ELSE IF (OLM .AND. ARM2) THEN
C        WRITE Error Message       ! Can't specify OLM & ARM2
         CALL ERRHDL(PATH,MODNAM,'E','141','OLM & ARM2')
      ELSE IF (OLM .AND. GRSM) THEN
C        WRITE Error Message       ! Can't specify OLM & GRSM
         CALL ERRHDL(PATH,MODNAM,'E','141','OLM & GRSM')
       ELSE IF (PVMRM .AND. ARM2) THEN
C        WRITE Error Message       ! Can't specify PVMRM & ARM2
         CALL ERRHDL(PATH,MODNAM,'E','141','PVMRM & ARM2')
       ELSE IF (PVMRM .AND. GRSM) THEN
C        WRITE Error Message       ! Can't specify PVMRM & GRSM
         CALL ERRHDL(PATH,MODNAM,'E','141','PVMRM & GRSM')
       ELSE IF (ARM2 .AND. GRSM) THEN
C        WRITE Error Message       ! Can't specify ARM2 & GRSM
         CALL ERRHDL(PATH,MODNAM,'E','141','ARM2 & GRSM')
      ELSE IF ((.NOT. RUNTTRM2) .AND. (RUNTTRM .AND. ARM2)) THEN
C        WRITE Error Message       ! Can't specify TTRM & ARM2
         CALL ERRHDL(PATH,MODNAM,'E','141','TTRM & ARM2')
      ELSE IF ((.NOT. RUNTTRM2) .AND. (RUNTTRM .AND. OLM)) THEN
C        WRITE Error Message       ! Can't specify TTRM & OLM
         CALL ERRHDL(PATH,MODNAM,'E','141','TTRM & OLM')
      ELSE IF ((.NOT. RUNTTRM2) .AND. (RUNTTRM .AND. PVMRM)) THEN
C        WRITE Error Message       ! Can't specify TTRM & PVMRM
         CALL ERRHDL(PATH,MODNAM,'E','141','TTRM & PVMRM')
      ELSE IF (RUNTTRM .AND. GRSM) THEN
C        WRITE Error Message       ! Can't specify TTRM & GRSM
         CALL ERRHDL(PATH,MODNAM,'E','141','TTRM & GRSM')
!! Added Nov. 2021
      ELSE IF (RUNTTRM2 .AND. GRSM ) THEN
C        WRITE Error Message       ! Can't specify TTRM & ARM2
         CALL ERRHDL(PATH,MODNAM,'E','141','TTRM2 & GRSM')
      ELSE IF (RUNTTRM2 .AND. OLM .AND. ARM2) THEN
C        WRITE Error Message       ! Can't specify TTRM & OLM
         CALL ERRHDL(PATH,MODNAM,'E','141','TTRM2 & >2 NO2 Options')
      ELSE IF (RUNTTRM2 .AND. PVMRM .AND. ARM2) THEN
C        WRITE Error Message       ! Can't specify TTRM & PVMRM
         CALL ERRHDL(PATH,MODNAM,'E','141','TTRM2 & >2 NO2 Options')
      ELSE IF (RUNTTRM2 .AND. OLM .AND. PVMRM) THEN
C        WRITE Error Message       ! Can't specify TTRM & GRSM
         CALL ERRHDL(PATH,MODNAM,'E','141','TTRM2 & >2 NO2 Options')
!! End Nov. 2021 TTRM insert
      END IF

      IF (PSDCREDIT .AND. .NOT.PVMRM) THEN
C        WRITE Error Message       ! Can't specify PSDCREDIT without PVMRM
         CALL ERRHDL(PATH,MODNAM,'E','143','PSDCREDIT')
      END IF

      IF (PSDCREDIT .AND. EVONLY) THEN
C        WRITE Error Message       ! Can't use PSDCREDIT with EVONLY Processing
         CALL ERRHDL(PATH,MODNAM,'E','147',' EVENTS ')
      END IF

C --- Check for Non-DFAULT options used without DFAULT; this will be used to
C     set the option label in the file headers.
C_OLD      IF (.NOT. DFAULT .AND.
C_OLD     &    (FLAT .OR. FLATSRCS .OR. ARDPLETE .OR. NOSTD .OR. NOCHKD .OR.
C_OLD     &     SCREEN .OR. SCIM .OR. PVMRM .OR. PSDCREDIT .OR.
C_OLD     &      OLM .OR. ARM2 .OR. BETA .OR. FASTAREA .OR. FASTALL .OR.
C_OLD     &     L_LowWind1 .OR. L_LowWind2 .OR. L_LowWind3
C_OLD     &                                .OR. .NOT.L_UrbanTransition)) THEN
C_OLD
C_OLD         L_NonDFAULT = .TRUE.
C_OLD      END IF
CCRT  1/29/2018: Added L_ALPHA to conditions for non-default options
CCRT  4/11/2022: D131 FRAN Alpha - Add L_PBal to non-default options
      IF (.NOT. DFAULT .AND.
     &    (FLAT .OR. FLATSRCS .OR. ARDPLETE .OR. NOSTD .OR. NOCHKD .OR.
     &     SCREEN .OR. SCIM .OR. PSDCREDIT .OR. BETA .OR. FASTAREA .OR.
     &     FASTALL .OR. LOW_WIND .OR. L_ALPHA .OR. GRSM .OR. L_PBal
     &     .OR. .NOT.L_UrbanTransition)) THEN

         L_NonDFAULT = .TRUE.
      END IF

C     Setup Label Array for Concentration and Depositions
      IF (NUMTYP .GT. NTYP) THEN
C        WRITE Error Message: Number of output types exceeds maximum
         WRITE(DUMMY,'(I4)') NTYP
         CALL ERRHDL(PATH,MODNAM,'E','280',DUMMY)
      ELSE IF (NUMTYP .EQ. 0) THEN
C        WRITE Warning Message: No Output Types Selected, Assume CONC Only
         CALL ERRHDL(PATH,MODNAM,'W','205','CONC')
         NUMTYP = 1
         ITYP   = 1
         CONC   = .TRUE.
         CHIDEP(1,ITYP) = 'AVER'
         CHIDEP(2,ITYP) = 'AGE '
         CHIDEP(3,ITYP) = 'CONC'
         CHIDEP(4,ITYP) = 'ENTR'
         CHIDEP(5,ITYP) = 'ATIO'
         CHIDEP(6,ITYP) = 'N   '
         EMIFAC(ITYP) = 1.0D06
         EMILBL(ITYP) = 'GRAMS/SEC'
         OUTLBL(ITYP) = 'MICROGRAMS/M**3'
         PERLBL(ITYP) = 'MICROGRAMS/M**3'
         OUTTYP(ITYP) = 'CONC'
      ELSE IF (CONC) THEN
         ITYP = 1
         CHIDEP(1,ITYP) = 'AVER'
         CHIDEP(2,ITYP) = 'AGE '
         CHIDEP(3,ITYP) = 'CONC'
         CHIDEP(4,ITYP) = 'ENTR'
         CHIDEP(5,ITYP) = 'ATIO'
         CHIDEP(6,ITYP) = 'N   '
         EMIFAC(ITYP) = 1.0D06
         EMILBL(ITYP) = 'GRAMS/SEC'
         OUTLBL(ITYP) = 'MICROGRAMS/M**3'
         PERLBL(ITYP) = 'MICROGRAMS/M**3'
         OUTTYP(ITYP) = 'CONC'
         IF (DEPOS) THEN
            ITYP = 2
            CHIDEP(1,ITYP) = '  TO'
            CHIDEP(2,ITYP) = 'TAL '
            CHIDEP(3,ITYP) = 'DEPO'
            CHIDEP(4,ITYP) = 'SITI'
            CHIDEP(5,ITYP) = 'ON  '
            CHIDEP(6,ITYP) = '    '
            EMIFAC(ITYP) = 3600.0D0
            EMILBL(ITYP) = 'GRAMS/SEC'
            OUTLBL(ITYP) = 'GRAMS/M**2'
            PERLBL(ITYP) = 'GRAMS/M**2'
            OUTTYP(ITYP) = 'DEPOS'
            IF (DDEP) THEN
               ITYP = 3
               CHIDEP(1,ITYP) = '    '
               CHIDEP(2,ITYP) = 'DRY '
               CHIDEP(3,ITYP) = 'DEPO'
               CHIDEP(4,ITYP) = 'SITI'
               CHIDEP(5,ITYP) = 'ON  '
               CHIDEP(6,ITYP) = '    '
               EMIFAC(ITYP) = 3600.0D0
               EMILBL(ITYP) = 'GRAMS/SEC'
               OUTLBL(ITYP) = 'GRAMS/M**2'
               PERLBL(ITYP) = 'GRAMS/M**2'
               OUTTYP(ITYP) = 'DDEP'
               IF (WDEP) THEN
                  ITYP = 4
                  CHIDEP(1,ITYP) = '    '
                  CHIDEP(2,ITYP) = 'WET '
                  CHIDEP(3,ITYP) = 'DEPO'
                  CHIDEP(4,ITYP) = 'SITI'
                  CHIDEP(5,ITYP) = 'ON  '
                  CHIDEP(6,ITYP) = '    '
                  EMIFAC(ITYP) = 3600.0D0
                  EMILBL(ITYP) = 'GRAMS/SEC'
                  OUTLBL(ITYP) = 'GRAMS/M**2'
                  PERLBL(ITYP) = 'GRAMS/M**2'
                  OUTTYP(ITYP) = 'WDEP'
               END IF
            ELSE IF (WDEP) THEN
               ITYP = 3
               CHIDEP(1,ITYP) = '    '
               CHIDEP(2,ITYP) = 'WET '
               CHIDEP(3,ITYP) = 'DEPO'
               CHIDEP(4,ITYP) = 'SITI'
               CHIDEP(5,ITYP) = 'ON  '
               CHIDEP(6,ITYP) = '    '
               EMIFAC(ITYP) = 3600.0D0
               EMILBL(ITYP) = 'GRAMS/SEC'
               OUTLBL(ITYP) = 'GRAMS/M**2'
               PERLBL(ITYP) = 'GRAMS/M**2'
               OUTTYP(ITYP) = 'WDEP'
            END IF
         ELSE IF (DDEP) THEN
            ITYP = 2
            CHIDEP(1,ITYP) = '    '
            CHIDEP(2,ITYP) = 'DRY '
            CHIDEP(3,ITYP) = 'DEPO'
            CHIDEP(4,ITYP) = 'SITI'
            CHIDEP(5,ITYP) = 'ON  '
            CHIDEP(6,ITYP) = '    '
            EMIFAC(ITYP) = 3600.0D0
            EMILBL(ITYP) = 'GRAMS/SEC'
            OUTLBL(ITYP) = 'GRAMS/M**2'
            PERLBL(ITYP) = 'GRAMS/M**2'
            OUTTYP(ITYP) = 'DDEP'
            IF (WDEP) THEN
               ITYP = 3
               CHIDEP(1,ITYP) = '    '
               CHIDEP(2,ITYP) = 'WET '
               CHIDEP(3,ITYP) = 'DEPO'
               CHIDEP(4,ITYP) = 'SITI'
               CHIDEP(5,ITYP) = 'ON  '
               CHIDEP(6,ITYP) = '    '
               EMIFAC(ITYP) = 3600.0D0
               EMILBL(ITYP) = 'GRAMS/SEC'
               OUTLBL(ITYP) = 'GRAMS/M**2'
               PERLBL(ITYP) = 'GRAMS/M**2'
               OUTTYP(ITYP) = 'WDEP'
            END IF
         ELSE IF (WDEP) THEN
            ITYP = 2
            CHIDEP(1,ITYP) = '    '
            CHIDEP(2,ITYP) = 'WET '
            CHIDEP(3,ITYP) = 'DEPO'
            CHIDEP(4,ITYP) = 'SITI'
            CHIDEP(5,ITYP) = 'ON  '
            CHIDEP(6,ITYP) = '    '
            EMIFAC(ITYP) = 3600.0D0
            EMILBL(ITYP) = 'GRAMS/SEC'
            OUTLBL(ITYP) = 'GRAMS/M**2'
            PERLBL(ITYP) = 'GRAMS/M**2'
            OUTTYP(ITYP) = 'WDEP'
         END IF
      ELSE IF (DEPOS) THEN
         ITYP = 1
         CHIDEP(1,ITYP) = '  TO'
         CHIDEP(2,ITYP) = 'TAL '
         CHIDEP(3,ITYP) = 'DEPO'
         CHIDEP(4,ITYP) = 'SITI'
         CHIDEP(5,ITYP) = 'ON  '
         CHIDEP(6,ITYP) = '    '
         EMIFAC(ITYP) = 3600.0D0
         EMILBL(ITYP) = 'GRAMS/SEC'
         OUTLBL(ITYP) = 'GRAMS/M**2'
         PERLBL(ITYP) = 'GRAMS/M**2'
         OUTTYP(ITYP) = 'DEPOS'
         IF (DDEP) THEN
            ITYP = 2
            CHIDEP(1,ITYP) = '    '
            CHIDEP(2,ITYP) = 'DRY '
            CHIDEP(3,ITYP) = 'DEPO'
            CHIDEP(4,ITYP) = 'SITI'
            CHIDEP(5,ITYP) = 'ON  '
            CHIDEP(6,ITYP) = '    '
            EMIFAC(ITYP) = 3600.0D0
            EMILBL(ITYP) = 'GRAMS/SEC'
            OUTLBL(ITYP) = 'GRAMS/M**2'
            PERLBL(ITYP) = 'GRAMS/M**2'
            OUTTYP(ITYP) = 'DDEP'
            IF (WDEP) THEN
               ITYP = 3
               CHIDEP(1,ITYP) = '    '
               CHIDEP(2,ITYP) = 'WET '
               CHIDEP(3,ITYP) = 'DEPO'
               CHIDEP(4,ITYP) = 'SITI'
               CHIDEP(5,ITYP) = 'ON  '
               CHIDEP(6,ITYP) = '    '
               EMIFAC(ITYP) = 3600.0D0
               EMILBL(ITYP) = 'GRAMS/SEC'
               OUTLBL(ITYP) = 'GRAMS/M**2'
               PERLBL(ITYP) = 'GRAMS/M**2'
               OUTTYP(ITYP) = 'WDEP'
            END IF
         ELSE IF (WDEP) THEN
            ITYP = 2
            CHIDEP(1,ITYP) = '    '
            CHIDEP(2,ITYP) = 'WET '
            CHIDEP(3,ITYP) = 'DEPO'
            CHIDEP(4,ITYP) = 'SITI'
            CHIDEP(5,ITYP) = 'ON  '
            CHIDEP(6,ITYP) = '    '
            EMIFAC(ITYP) = 3600.0D0
            EMILBL(ITYP) = 'GRAMS/SEC'
            OUTLBL(ITYP) = 'GRAMS/M**2'
            PERLBL(ITYP) = 'GRAMS/M**2'
            OUTTYP(ITYP) = 'WDEP'
         END IF
      ELSE IF (DDEP) THEN
         ITYP = 1
         CHIDEP(1,ITYP) = '    '
         CHIDEP(2,ITYP) = 'DRY '
         CHIDEP(3,ITYP) = 'DEPO'
         CHIDEP(4,ITYP) = 'SITI'
         CHIDEP(5,ITYP) = 'ON  '
         CHIDEP(6,ITYP) = '    '
         EMIFAC(ITYP) = 3600.0D0
         EMILBL(ITYP) = 'GRAMS/SEC'
         OUTLBL(ITYP) = 'GRAMS/M**2'
         PERLBL(ITYP) = 'GRAMS/M**2'
         OUTTYP(ITYP) = 'DDEP'
         IF (WDEP) THEN
            ITYP = 2
            CHIDEP(1,ITYP) = '    '
            CHIDEP(2,ITYP) = 'WET '
            CHIDEP(3,ITYP) = 'DEPO'
            CHIDEP(4,ITYP) = 'SITI'
            CHIDEP(5,ITYP) = 'ON  '
            CHIDEP(6,ITYP) = '    '
            EMIFAC(ITYP) = 3600.0D0
            EMILBL(ITYP) = 'GRAMS/SEC'
            OUTLBL(ITYP) = 'GRAMS/M**2'
            PERLBL(ITYP) = 'GRAMS/M**2'
            OUTTYP(ITYP) = 'WDEP'
         END IF
      ELSE IF (WDEP) THEN
         ITYP = 1
         CHIDEP(1,ITYP) = '    '
         CHIDEP(2,ITYP) = 'WET '
         CHIDEP(3,ITYP) = 'DEPO'
         CHIDEP(4,ITYP) = 'SITI'
         CHIDEP(5,ITYP) = 'ON  '
         CHIDEP(6,ITYP) = '    '
         EMIFAC(ITYP) = 3600.0D0
         EMILBL(ITYP) = 'GRAMS/SEC'
         OUTLBL(ITYP) = 'GRAMS/M**2'
         PERLBL(ITYP) = 'GRAMS/M**2'
         OUTTYP(ITYP) = 'WDEP'
      END IF

      EMICON = 1.0D+06

C --- Modify PLTFRM, PSTFRM and MXDFRM if needed for more than one output type
C     and for EXP format (note that FILE_FORMAT is set during PRESET).

      IF (NUMTYP .GT. 1 .AND. FILE_FORMAT .EQ. 'FIX') THEN
         IF (PM25AVE .OR. NO2AVE .OR. SO2AVE) THEN
            WRITE(PLTFRM,1009) NUMTYP+2
 1009       FORMAT('(',I1,'(1X,F13.5),3(1X,F8.2),2X,A6,2X,A8,2X,A5,5X,',
     &             'A8,2X,10(F13.5,2X,I8.8,2X:))')
         ELSE
            WRITE(PLTFRM,1019) NUMTYP+2
 1019       FORMAT('(',I1,'(1X,F13.5),3(1X,F8.2),3X,A5,2X,A8,2X,A5,5X,',
     &             'A8,2X,I8)')
         END IF
         WRITE(PSTFRM,1029) NUMTYP+2
 1029    FORMAT('(',I1,'(1X,F13.5),3(1X,F8.2),2X,A6,2X,A8,2X,I8.8,2X,',
     &          'A8)')
         WRITE(MXDFRM,1039) NUMTYP+2
 1039    FORMAT('(',I1,'(1X,F13.5),3(1X,F8.2),2X,A6,2X,A8,2X,I4,2X,I3,',
     &          '2X,I8.8,2X,A8)')
      ELSE IF (NUMTYP .GT. 1 .AND. FILE_FORMAT .EQ. 'EXP') THEN
         IF (PM25AVE .OR. NO2AVE .OR. SO2AVE) THEN
            WRITE(PLTFRM,2009) NUMTYP
 2009       FORMAT('(2(1X,F13.5),',I1,'(1X,E13.6),3(1X,F8.2),2X,A6,2X,',
     &             'A8,2X,A5,5X,A8,2X,10(E13.6,2X,I8.8,2X:))')
         ELSE
            WRITE(PLTFRM,2019) NUMTYP
 2019       FORMAT('(2(1X,F13.5),',I1,'(1X,E13.6),3(1X,F8.2),3X,A5,2X,',
     &             'A8,2X,A5,5X,A8,2X,I8)')
         END IF
         WRITE(PSTFRM,2029) NUMTYP
 2029    FORMAT('(2(1X,F13.5),',I1,'(1X,E13.6),3(1X,F8.2),2X,A6,2X,A8,',
     &          '2X,I8.8,2X,A8)')
         WRITE(MXDFRM,2039) NUMTYP
 2039    FORMAT('(2(1X,F13.5),',I1,'(1X,E13.6),3(1X,F8.2),2X,A6,2X,A8,',
     &          '2X,I4,2X,I3,2X,I8.8,2X,A8)')
      END IF

      RETURN
      END


      SUBROUTINE AVETIM
C***********************************************************************
C                 AVETIM Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Process Averaging Time Options From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Averaging Period Array and PERIOD Logical Switch
C
C        ERROR HANDLING:   Checks for Too Many Short Term Averages (>4);
C                          Checks for Invalid Averaging Periods, MOD(24,X) NE 0;
C                          Checks for Duplicate Short Term Averaging Periods
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, J, K
      REAL    :: AVENUM
      CHARACTER (LEN = 8) :: KOPT

C     Variable Initializations
      MODNAM = 'AVETIM'

C     Check for No Parameters
      IF (IFC .LT. 3) THEN
C        WRITE Error Message     ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      END IF

C     First Check for Presence of PERIOD or ANNUAL Switch
      DO 10 I = 3, IFC
         KOPT = FIELD(I)
         IF (KOPT .EQ. 'PERIOD') THEN
            PERIOD = .TRUE.
         ELSE IF (KOPT .EQ. 'ANNUAL') THEN
            ANNUAL = .TRUE.
         END IF
 10   CONTINUE

C --- Check for Both PERIOD and ANNUAL
      IF (PERIOD .AND. ANNUAL) THEN
C        Write Error Message; both PERIOD and ANNUAL specified
         CALL ERRHDL(PATH,MODNAM,'E','294',KEYWRD)
      ELSE IF (PERIOD .OR. ANNUAL) THEN
C        Check for Too Many Averaging Periods
         IF (IFC .GT. NAVE+3) THEN
C           WRITE Error Message: Too Many Period Or Time Fields
            CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
          END IF
      ELSE
         IF (IFC .GT. NAVE+2) THEN
C           WRITE Error Message: Too Many Period Or Time Fields
            CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         END IF
      END IF

C     Loop Through Fields Again, Filling KAVE Array for Short Term Averages
      J = 0
      DO 20 I = 3, IFC
         KOPT = FIELD(I)
         IF (KOPT .NE. 'PERIOD' .AND. KOPT .NE. 'ANNUAL') THEN
            IF (KOPT .NE. 'MONTH') THEN
               CALL STONUM(KOPT,8,AVENUM,IMIT)
               IF (IMIT .NE. 1) THEN
C                 Write Error Message:Invalid Numerical Field
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               END IF
C              Check for Valid Averaging Period
               IF ((MOD(24,NINT(AVENUM)).EQ.0 .AND.
     &                                   IMIT.EQ.1)) THEN
                  J = J + 1
                  IF (J .LE. NAVE) THEN
                     KAVE(J) = NINT(AVENUM)
                     WRITE(CHRAVE(J),'(I2,"-HR")') KAVE(J)
                     NUMAVE = J
C                    Check for Duplicate Averaging Periods
                     DO 15 K = J-1, 1, -1
                        IF (KAVE(J) .EQ. KAVE(K)) THEN
C                          WRITE Error Message    ! Duplicate Averaging Period
                           CALL ERRHDL(PATH,MODNAM,'E','211',KEYWRD)
                        END IF
 15                  CONTINUE
                  ELSE
C                    WRITE Error Message   ! Too Many Short Term Averaging Periods
C                    This shouldn't occur since limits are dynamically allocated
                     WRITE(DUMMY,'(''NAVE='',I7)') NAVE
                     CALL ERRHDL(PATH,MODNAM,'E','290',DUMMY)
                  END IF
               ELSE
C                 WRITE Error Message      ! Invalid Averaging Period
                  CALL ERRHDL(PATH,MODNAM,'E','203','AVEPER')
               END IF
            ELSE
               J = J + 1
               IF (J .LE. NAVE) THEN
                  KAVE(J) = 720
                  MONTH = .TRUE.
                  CHRAVE(J) = 'MONTH'
                  NUMAVE = J
C                 Check for Duplicate Averaging Periods
                  DO K = J-1, 1, -1
                     IF (KAVE(J) .EQ. KAVE(K)) THEN
C                       WRITE Error Message    ! Duplicate Averaging Period
                        CALL ERRHDL(PATH,MODNAM,'E','211',KEYWRD)
                     END IF
                  END DO
               ELSE
C                 WRITE Error Message   ! Too Many Short Term Averaging Periods
C                 This shouldn't occur since limits are dynamically allocated
                  WRITE(DUMMY,'(''NAVE='',I7)') NAVE
                  CALL ERRHDL(PATH,MODNAM,'E','290',DUMMY)
               END IF
            END IF
         END IF
 20   CONTINUE

      RETURN
      END

      SUBROUTINE POLLID
C***********************************************************************
C                 POLLID Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes Pollutant Identification Option
C
C        PROGRAMMER: Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Pollutant Identification Option
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'POLLID'

C     Check The Number Of The Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .EQ. 3) THEN
C        Assign user input to POLLUT variable
         POLLUT = FIELD(3)
      ELSE IF (IFC .EQ. 4) THEN
C ---    Check for POLLUT IDs associated with "special" processing;
C        NO2 or SO2 or PM25 (including all PM25 variants)
         IF (FIELD(3) .EQ. 'NO2'  .OR. FIELD(3) .EQ. 'SO2' .OR.
     &       FIELD(3) .EQ. 'PM25' .OR. FIELD(3) .EQ. 'PM-2.5' .OR.
     &       FIELD(3) .EQ. 'PM-25'.OR. FIELD(3) .EQ. 'PM2.5') THEN
C ---       This POLLID allows optional input in field 4; assign POLLUT
            POLLUT = FIELD(3)
         ELSE
C ---       User-specified POLLID doesn't allow for field 4; assign
C           POLLUT but issue an error message
            POLLUT = FIELD(3)
            IF (FIELD(4) .EQ. 'H1H' .OR. FIELD(4) .EQ. 'H2H' .OR.
     &          FIELD(4) .EQ. 'INC') THEN
C              Error Message: 'H1H', 'H2H', and 'INC' processing not
C              applicable to this POLLUT
               WRITE(DUMMY,'(A,1X,A3)') POLLUT(1:LEN_TRIM(POLLUT)),
     &                                  FIELD(4)(1:3)
               CALL ERRHDL(PATH,MODNAM,'E','277',DUMMY)
C ---          Save FIELD(4) to include in summary of input options
               NO2_FIELD4 = FIELD(4)(1:3)
               GO TO 999
            ELSE
C              Error Message: Too Many Parameters
               CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
               GO TO 999
            END IF
         END IF
C        Now check for options to disable "special" processing for
C        these pollutants
         IF (FIELD(4) .EQ. 'H1H' .OR. FIELD(4) .EQ. 'H2H' .OR.
     &       FIELD(4) .EQ. 'INC') THEN
            IF (POLLUT .EQ. 'NO2') THEN
               L_NO_NO2AVE = .TRUE.
               NO2_FIELD4 = FIELD(4)(1:3)
C              Issue Warning Message: user disabled special processing
               WRITE(DUMMY,'(A,1X,A3)') POLLUT(1:LEN_TRIM(POLLUT)),
     &                                  FIELD(4)(1:3)
               CALL ERRHDL(PATH,MODNAM,'W','276',DUMMY)
            ELSE IF (POLLUT .EQ. 'SO2') THEN
               L_NO_SO2AVE = .TRUE.
               SO2_FIELD4 = FIELD(4)(1:3)
               WRITE(DUMMY,'(A,1X,A3)') POLLUT(1:LEN_TRIM(POLLUT)),
     &                                  FIELD(4)(1:3)
C              Issue Warning Message: user disabled special processing
               CALL ERRHDL(PATH,MODNAM,'W','276',DUMMY)
            ELSE IF (POLLUT .EQ. 'PM25' .OR. POLLUT .EQ. 'PM-2.5' .OR.
     &               POLLUT .EQ. 'PM-25'.OR. POLLUT .EQ. 'PM2.5') THEN
               L_NO_PM25AVE = .TRUE.
               PM25_FIELD4 = FIELD(4)(1:3)
               WRITE(DUMMY,'(A,1X,A3)') POLLUT(1:LEN_TRIM(POLLUT)),
     &                                  FIELD(4)(1:3)
C              Issue Warning Message: user disabled special processing
               CALL ERRHDL(PATH,MODNAM,'W','276',DUMMY)
            ELSE
C              Error Message: 'H1H', 'H2H', and 'INC' processing not
C              applicable to this POLLUT
               WRITE(DUMMY,'(A,1X,A3)') POLLUT(1:LEN_TRIM(POLLUT)),
     &                                  FIELD(4)(1:3)
               CALL ERRHDL(PATH,MODNAM,'E','277',DUMMY)
               GO TO 999
            END IF
         END IF
      ELSE
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

      POLLUT = FIELD(3)

 999  RETURN
      END

      SUBROUTINE EDECAY
C***********************************************************************
C                 EDECAY Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes Exponential Decay Options
C
C        PROGRAMMER: Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Exponental Decay Options
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'EDECAY'

C     Check The Number Of The Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 3) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Start To Get Decay Coef.
      CALL STODBL(FIELD(3),ILEN_FLD,DNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      END IF

      IF (KEYWRD .EQ. 'HALFLIFE') THEN
         HAFLIF = DNUM
C        Calculate Decay Coef. by Halflife
         DECOEF = 0.693D0/HAFLIF
      ELSE IF (KEYWRD .EQ. 'DCAYCOEF') THEN
         DECOEF = DNUM
      END IF

C --- Check for Urban Regulatory Default for SO2; use L_PRESET_URBAN rather then
C     URBAN to allow flexibility in order of keywords
      IF (DFAULT .AND. L_PRESET_URBAN .AND. POLLUT.EQ.'SO2') THEN
         IF (DECOEF .NE. 4.81D-5) THEN
C           WRITE Warning Message: Attempt to Override Regulatory Default
            CALL ERRHDL(PATH,MODNAM,'W','206','DCAYCOEF')
         END IF
         DECOEF = 4.81D-5
      ELSE IF (DFAULT) THEN
         IF (DECOEF .NE. 0.0D0) THEN
C           WRITE Warning Message: Attempt to Override Regulatory Default
            CALL ERRHDL(PATH,MODNAM,'W','206','DCAYCOEF')
         END IF
         DECOEF = 0.0D0
      ELSE IF (.NOT. DFAULT .AND. DECOEF .NE. 0.0D0) THEN
C        Set flag for use of non-DEFAULT option
         L_NonDFAULT = .TRUE.
      END IF

 999  RETURN
      END

      SUBROUTINE RUNNOT
C***********************************************************************
C                 RUNNOT Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Process Option To RUN Or NOT From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Model RUN Logical Switch
C
C        ERROR HANDLING:   Checks for Invalid Parameters;
C                          Checks for No Parameters;
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'RUNNOT'

      IF (IFC .EQ. 3) THEN
         IF (FIELD(3) .EQ. 'RUN') THEN
            RUN = .TRUE.
         ELSE IF (FIELD(3) .EQ. 'NOT') THEN
            RUN = .FALSE.
         ELSE
C           WRITE Error Message  ! Invalid Parameter
            CALL ERRHDL(PATH,MODNAM,'E','203',KEYWRD)
         END IF
      ELSE IF (IFC .GT. 3) THEN
C        WRITE Error Message     ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Error Message     ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      END IF

      RETURN
      END

      SUBROUTINE FLAGDF
C***********************************************************************
C                 FLAGDF Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Process Default Flagpole Receptor Height Option
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Default Flagpole Receptor Heights
C
C        ERROR HANDLING:   Checks for Invalid Parameters;
C                          Checks for No Parameters;
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      DOUBLE PRECISION :: ZFLG
C Unused:      INTEGER :: I

C     Variable Initializations
      MODNAM = 'FLAGDF'
      FLGPOL = .TRUE.

      IF (IFC .EQ. 3) THEN
         CALL STODBL(FIELD(3),ILEN_FLD,ZFLG,IMIT)
         IF (IMIT .NE. 1) THEN
C           Write Error Message:Invalid Numerical Field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         END IF
         IF (ZFLG .GE. 0.0D0 .AND. IMIT .EQ. 1) THEN
            AZFLAG(:) = ZFLG
         ELSE IF (ZFLG .LT. 0.0D0 .AND. IMIT .EQ. 1) THEN
C            WRITE Error Message: Invalid Data. Negative value specified
             CALL ERRHDL(PATH,MODNAM,'E','209','ZFLAG')
         ELSE
C            WRITE Error Message: Invalid Parameter
             CALL ERRHDL(PATH,MODNAM,'E','203',KEYWRD)
         END IF
      ELSE IF (IFC .GT. 3) THEN
C        WRITE Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'W','205','ZFLAG=0.')
      END IF

      RETURN
      END

      SUBROUTINE EVNTFL
C***********************************************************************
C                 EVNTFL Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Process EVENT File Option
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: EVENT File Logical Switch and EVENT Filename
C
C        ERROR HANDLING:   Checks for No Parametes;
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'EVNTFL'

      IF (IFC .EQ. 3) THEN
         EVENTS = .TRUE.
C        Retrieve Included Filename as Character Substring to Maintain Case
         IF ((LOCE(3)-LOCB(3)) .LE. (ILEN_FLD - 1) ) THEN
C           Retrieve Filename as Character Substring to Maintain Original Case
C           Also Check for Filename Larger Than ILEN_FLD Characters
            EVFILE = RUNST1(LOCB(3):LOCE(3))
         ELSE
C           WRITE Error Message:  EVFILE Field is Too Long
            WRITE(DUMMY,'(I8)') ILEN_FLD
            CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
            RETURN
         END IF
         EVPARM = 'DETAIL'
      ELSE IF (IFC .EQ. 4) THEN
         EVENTS  = .TRUE.
C        Retrieve Included Filename as Character Substring to Maintain Case
         IF ((LOCE(3)-LOCB(3)) .LE. (ILEN_FLD - 1) ) THEN
C           Retrieve Filename as Character Substring to Maintain Original Case
C           Also Check for Filename Larger Than ILEN_FLD Characters
            EVFILE = RUNST1(LOCB(3):LOCE(3))
         ELSE
C           WRITE Error Message:  EVFILE Field is Too Long
            WRITE(DUMMY,'(I8)') ILEN_FLD
            CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
            RETURN
         END IF
         EVPARM = FIELD(4)
      ELSE IF (IFC .GT. 4) THEN
C        WRITE Error Message           ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Warning Message         ! No Parameters - Use Default Name
         CALL ERRHDL(PATH,MODNAM,'W','207',KEYWRD)
         EVENTS = .TRUE.
         EVFILE = 'EVENTS.INP'
         EVPARM = 'DETAIL'
      END IF

C     Check for Invalid EVPARM
      IF (EVPARM .NE. 'SOCONT' .AND. EVPARM .NE. 'DETAIL') THEN
C        WRITE Warning Message         ! Invalid Parameter - Use Default
         CALL ERRHDL(PATH,MODNAM,'W','203','EVPARM')
      END IF

C     Open The EVENT Input File
      OPEN(UNIT=IEVUNT,FILE=EVFILE,STATUS='REPLACE',
     &     FORM='FORMATTED')

      RETURN
      END

      SUBROUTINE SAVEFL
C***********************************************************************
C                 SAVEFL Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Process RESTART File Save Option
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: RSTSAV File Logical Switch and RESTART Filename
C
C        ERROR HANDLING:   Checks for No Parametes (uses default name);
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   COCARD
C***********************************************************************
C
C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'SAVEFL'

      IF (MULTYR) THEN
C        WRITE Error Message:  Conflicting Options RE-START and MULTYEAR
         CALL ERRHDL(PATH,MODNAM,'E','150',KEYWRD)
      ELSE IF (IFC .EQ. 3) THEN
         RSTSAV = .TRUE.
C        Retrieve Included Filename as Character Substring to Maintain Case
         IF ((LOCE(3)-LOCB(3)) .LE. (ILEN_FLD - 1) ) THEN
C           Retrieve Filename as Character Substring to Maintain Original Case
C           Also Check for Filename Larger Than ILEN_FLD Characters
            SAVFIL = RUNST1(LOCB(3):LOCE(3))
         ELSE
C           WRITE Error Message:  SAVFIL Field is Too Long
            WRITE(DUMMY,'(I8)') ILEN_FLD
            CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
            RETURN
         END IF
         SAVFL2 = SAVFIL
         INCRST = 1
      ELSE IF (IFC .EQ. 4) THEN
         RSTSAV = .TRUE.
C        Retrieve Included Filename as Character Substring to Maintain Case
         IF ((LOCE(3)-LOCB(3)) .LE. (ILEN_FLD - 1) ) THEN
C           Retrieve Filename as Character Substring to Maintain Original Case
C           Also Check for Filename Larger Than ILEN_FLD Characters
            SAVFIL = RUNST1(LOCB(3):LOCE(3))
         ELSE
C           WRITE Error Message:  SAVFIL Field is Too Long
            WRITE(DUMMY,'(I8)') ILEN_FLD
            CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
            RETURN
         END IF
         SAVFL2 = SAVFIL
         CALL STONUM(FIELD(4),ILEN_FLD,FNUM,IMIT)
         INCRST = NINT(FNUM)
         IF (IMIT .NE. 1) THEN
C           Write Error Message:Invalid Numerical Field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         END IF
      ELSE IF (IFC .EQ. 5) THEN
         RSTSAV = .TRUE.
C        Retrieve Included Filename as Character Substring to Maintain Case
         IF ((LOCE(3)-LOCB(3)) .LE. (ILEN_FLD - 1) ) THEN
C           Retrieve Filename as Character Substring to Maintain Original Case
C           Also Check for Filename Larger Than ILEN_FLD Characters
            SAVFIL = RUNST1(LOCB(3):LOCE(3))
         ELSE
C           WRITE Error Message:  SAVFIL Field is Too Long
            WRITE(DUMMY,'(I8)') ILEN_FLD
            CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
            RETURN
         END IF
         CALL STONUM(FIELD(4),ILEN_FLD,FNUM,IMIT)
         INCRST = NINT(FNUM)
         IF (IMIT .NE. 1) THEN
C           Write Error Message:Invalid Numerical Field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         END IF
C        Retrieve Included Filename as Character Substring to Maintain Case
         IF ((LOCE(5)-LOCB(5)) .LE. (ILEN_FLD - 1) ) THEN
C           Retrieve Filename as Character Substring to Maintain Original Case
C           Also Check for Filename Larger Than ILEN_FLD Characters
            SAVFL2 = RUNST1(LOCB(5):LOCE(5))
         ELSE
C           WRITE Error Message:  SAVFL2 Field is Too Long
            WRITE(DUMMY,'(I8)') ILEN_FLD
            CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
            RETURN
         END IF
      ELSE IF (IFC .GT. 5) THEN
C        WRITE Error Message           ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Warning Message          ! No Parameters - Use Default Name
         CALL ERRHDL(PATH,MODNAM,'W','207',KEYWRD)
         RSTSAV = .TRUE.
         SAVFIL = 'SAVE.FIL'
         SAVFL2 = SAVFIL
         INCRST = 1
      END IF

      RETURN
      END

      SUBROUTINE INITFL
C***********************************************************************
C                 INITFL Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Process RESTART Initialization Input File Option
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To change default filename to SAVE.FIL to match
C                    default name for SAVEFILE card.
C                    R.W. Brode, PES, Inc. - 6/20/95
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: RSTINP Logical Switch and Re-start Input Filename
C
C        ERROR HANDLING:   Checks for No Parametes (uses default name);
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   COCARD
C***********************************************************************
C
C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'INITFL'

      IF (MULTYR) THEN
C        WRITE Error Message:  Conflicting Options RE-START and MULTYEAR
         CALL ERRHDL(PATH,MODNAM,'E','150',KEYWRD)
      ELSE IF (IFC .EQ. 3) THEN
         RSTINP = .TRUE.
C        Retrieve Included Filename as Character Substring to Maintain Case
         IF ((LOCE(3)-LOCB(3)) .LE. (ILEN_FLD - 1) ) THEN
C           Retrieve Filename as Character Substring to Maintain Original Case
C           Also Check for Filename Larger Than ILEN_FLD Characters
            INIFIL = RUNST1(LOCB(3):LOCE(3))
         ELSE
C           WRITE Error Message:  INIFIL Field is Too Long
            WRITE(DUMMY,'(I8)') ILEN_FLD
            CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
            RETURN
         END IF
      ELSE IF (IFC .GT. 3) THEN
C        WRITE Error Message           ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Warning Message          ! No Parameters - Use Default Name
         CALL ERRHDL(PATH,MODNAM,'W','207',KEYWRD)
         RSTINP = .TRUE.
         INIFIL = 'SAVE.FIL'
      END IF

      RETURN
      END

      SUBROUTINE ERRFIL
C***********************************************************************
C                 ERRFIL Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Process Error Message File Option
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Error Message File Logical Switch and ERRMSG Filename
C
C        ERROR HANDLING:   Checks for No Parametes (uses default name);
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   COCARD
C***********************************************************************
C
C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'ERRFIL'

      IF (IFC .EQ. 3) THEN
         ERRLST = .TRUE.
C        Retrieve Included Filename as Character Substring to Maintain Case
         IF ((LOCE(3)-LOCB(3)) .LE. (ILEN_FLD - 1) ) THEN
C           Retrieve Filename as Character Substring to Maintain Original Case
C           Also Check for Filename Larger Than ILEN_FLD Characters
            MSGFIL = RUNST1(LOCB(3):LOCE(3))
         ELSE
C           WRITE Error Message:  MSGFIL Field is Too Long
            WRITE(DUMMY,'(I8)') ILEN_FLD
            CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
            RETURN
         END IF
      ELSE IF (IFC .GT. 3) THEN
C*       WRITE Error Message                ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C*       WRITE Warning Message              ! No Parameters - Use Default Name
         CALL ERRHDL(PATH,MODNAM,'W','207',KEYWRD)
         ERRLST = .TRUE.
         MSGFIL = 'ERRORS.LST'
      END IF
C*#

      RETURN
      END

      SUBROUTINE DEBOPT
C***********************************************************************
C                 DEBOPT Module of AERMOD
C
C        PURPOSE: Process Debug Output File Option
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    September 30, 1993
C
C        MODIFIED:   Added DEBUG option for RLINE source.
C                    Wood, 12/07/2021 Michelle G. Snyder & Laura Kent
C
C        MODIFIED    Modified to allow user to specify debug output
C                    for the GRSM NO2 option.
C                    CERC, 11/30/20
C
C        MODIFIED:   Modified to allow user to specify debug output
C                    for the PRIME downwash algorithm and for the
C                    OLM, ARM, or ARM2 options for modeling NO2.
C                    Portions of the MODEL debug outputs that were
C                    included in the main 'aermod.out' and in the
C                    'model.dbg' file will now be included in a
C                    separate PRIME debug file.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 01/29/2014
C
C        MODIFIED:   Modified to allow user to specify debug output
C                    only for PVMRM or deposition options on the
C                    DEBUGOPT keyword, avoiding large ouput files
C                    under the MODEL debug option. Debug output for
C                    PVMRM and/or deposition options will still be
C                    generated if the MODEL debug option is selected.
C                    See AERMOD User's Guide Addendum for details
C                    on the DEBUGOPT keyword.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Debug File Logical Switches and Filenames
C
C        ERROR HANDLING:   Checks for Too Few Parameters (uses default name);
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   COCARD
C***********************************************************************
C
C     Variable Declarations
      USE MAIN1
      USE RLINE_DATA, ONLY: NRLINES
!CRCO D095 - this has been added to bring NBLP into the logic. Should
! we move NBLP out of BUOYANT_LINE and into MAIN1 instead in modules.f?
      USE BUOYANT_LINE
      IMPLICIT NONE
      CHARACTER MODNAM*12, KOPT*8
CCRT  D063 Add Platform Downwash variable IPLATFM and initialize below
CCRT  D113 Add Sidewash source variable ISW and initialize below
      INTEGER :: I, IMOD, IMET, IAREA, IPRM, IPVM, IOLMD, IARM2, IDEP,
     &           IGRSM, NOPTS, MAXFields, IPRM2, ITTRMD, ITTRM2,
     &           IURBD, IBLP, IPLATFM, IRLINE, ISW

C     Variable Initializations
      MODNAM = 'DEBOPT'
C     Initialize counters for number of debug options and field number
C     associated with debugopts
      IMOD  = 0
      IMET  = 0
      IAREA = 0
      IPRM  = 0
      IPVM  = 0
      IOLMD = 0
      IARM2  = 0
      IDEP  = 0
      IGRSM = 0
      IPRM2 = 0
      IPLATFM = 0
      NOPTS = 0
      MAXFields = 0
      ITTRMD = 0
      ITTRM2 = 0
      IRLINE = 0
      IURBD = 0
      IBLP = 0
      ISW = 0

C     Check for Too Few or Too Many Parameters
      IF (IFC .LT. 3) THEN
C        WRITE Error Message     ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      ELSE IF (IFC .GT. 13) THEN
C        WRITE Warning Message   ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      END IF

C --- First Check for Presence of Debug Switches;
C     also save position to interpret optional
C     filenames
      DO I = 3, IFC
         KOPT = FIELD(I)
         IF (KOPT .EQ. 'MODEL') THEN
            DEBUG = .TRUE.
            NOPTS = NOPTS + 1
            IMOD = I
         ELSE IF (KOPT .EQ. 'METEOR') THEN
            METEORDBG = .TRUE.
            NOPTS = NOPTS + 1
            IMET = I
         ELSE IF (KOPT .EQ. 'AREA') THEN
C ---       Check to see if AREADBG option has already been assigned .T.;
C           user may have entered both AREA and LINE
            IF (.NOT. AREADBG) THEN
C ---          AREADBG option not already = .T.; assign all variables
               AREADBG = .TRUE.
               NOPTS = NOPTS + 1
               IAREA = I
            ELSE
C ---          AREADBG already assigned = .T.; user may have entered
C              both AREA and LINE options; issue ERROR message
               CALL ERRHDL(PATH,MODNAM,'E','194','AREADEBUG')
               AREADBG = .FALSE.
            END IF
         ELSE IF (KOPT .EQ. 'LINE') THEN
C ---       Check to see if AREADBG option has already been assigned .T.;
C           user may have entered both AREA and LINE
            IF (.NOT. AREADBG) THEN
C ---          AREADBG option not already = .T.; assign all variables
               AREADBG = .TRUE.
               NOPTS = NOPTS + 1
               IAREA = I
            ELSE
C ---          AREADBG already assigned = .T.; user may have entered
C              both AREA and LINE options; issue ERROR message
               CALL ERRHDL(PATH,MODNAM,'E','194','LINEDEBUG')
               AREADBG = .FALSE.
            END IF
         ELSE IF (KOPT .EQ. 'PRIME') THEN
            PRIMEDBG = .TRUE.
            NOPTS = NOPTS + 1
            IPRM = I
         ELSE IF (KOPT .EQ. 'PVMRM') THEN
            PVMRMDBG = .TRUE.
            NOPTS = NOPTS + 1
            IPVM = I
         ELSE IF (KOPT .EQ. 'OLM') THEN
            OLMDEBUG = .TRUE.
            NOPTS = NOPTS + 1
            IOLMD = I
         ELSE IF (KOPT .EQ. 'ARM2') THEN
            ARM2DEBUG = .TRUE.
            NOPTS = NOPTS + 1
            IARM2 = I
         ELSE IF (KOPT .EQ. 'GRSM') THEN
            GRSMDEBUG = .TRUE.
            NOPTS = NOPTS + 1
            IGRSM = I
         ELSE IF (KOPT .EQ. 'DEPOS') THEN
            DEPOSDBG = .TRUE.
            NOPTS = NOPTS + 1
            IDEP = I
CCRT     D063 Platform Downwash Debug
         ELSE IF (KOPT .EQ. 'PLATFORM') THEN
            PLATFMDBG = .TRUE.
            NOPTS = NOPTS + 1
            IPLATFM = I
         ELSE IF (KOPT .EQ. 'AWMADW') THEN
            AWMADWDBG   = .TRUE.
            NOPTS = NOPTS + 1
            IPRM2 = I
         ELSE IF (KOPT .EQ. 'RLINE') THEN
            RLINEDBG   = .TRUE.
            NOPTS = NOPTS + 1
            IRLINE = I
         ELSE IF (KOPT .EQ. 'TTRM') THEN
            TTRMDBG = .TRUE.
            NOPTS = NOPTS + 1
            ITTRMD = I
         ELSE IF (KOPT .EQ. 'TTRM2') THEN
            TTRM2DBG = .TRUE.
            NOPTS = NOPTS + 1
            ITTRM2 = I
         ELSE IF (KOPT .EQ. 'URBANDB') THEN
            URBDBUG = .TRUE.
            NOPTS = NOPTS + 1
            IURBD = I
         ELSE IF (KOPT .EQ. 'BLPDBUG') THEN
            BLPDBUG = .TRUE.
            NOPTS = NOPTS + 1
            IBLP = I
         ELSE IF (KOPT .EQ. 'SWPOINT') THEN
            SWDBG = .TRUE.
            NOPTS = NOPTS + 1
            ISW = I
         END IF
      END DO

C --- Determine maximum number of fields allowed based on number of
C     options specified, assuming that user has specified filename
C     for each option (except for DEPOS).
      IF (NOPTS .GT. 0) THEN
         IF (.NOT.DEPOSDBG) THEN
            MAXFields = 2 + NOPTS*2
         ELSE
            MAXFields = 2 + (NOPTS-1)*2 + 1
         END IF
      ELSE
C        No recognizable debug options specified, issue fatal error
         WRITE(DUMMY,'(A:)') FIELD(3)(1:MIN(12,LEN_TRIM(FIELD(3))))
         CALL ERRHDL(PATH,MODNAM,'E','203',DUMMY)
         GO TO 999
      END IF

C --- Check for debug options without associated model option being used
      IF (PVMRMDBG .AND. .NOT. PVMRM) THEN
C        Write Error Message:  PVMRM debug without PVMRM option
         CALL ERRHDL(PATH,MODNAM,'E','194','PVMRMDBG')
      END IF
      IF (OLMDEBUG .AND. .NOT.OLM) THEN
C        Write Error Message:  OLM debug without OLM option
         CALL ERRHDL(PATH,MODNAM,'E','194','OLMDEBUG')
      END IF
      IF (ARM2DEBUG .AND. .NOT.ARM2) THEN
C        Write Error Message:  ARM2 debug without ARM2 option
         CALL ERRHDL(PATH,MODNAM,'E','194','ARM2DEBUG')
      END IF
      IF (GRSMDEBUG .AND. .NOT.GRSM) THEN
C        Write Error Message:  GRSM debug without GRSM option
         CALL ERRHDL(PATH,MODNAM,'E','194','GRSMDEBUG')
      END IF
      IF (TTRMDBG .AND. .NOT. RUNTTRM) THEN
C        Write Error Message:  TTRM debug without TTRM option
         CALL ERRHDL(PATH,MODNAM,'E','194','TTRMDEBUG')
      END IF
      IF (TTRM2DBG .AND. .NOT. RUNTTRM2) THEN
C        Write Error Message:  TTRM2 debug without TTRM2 option
         CALL ERRHDL(PATH,MODNAM,'E','194','TTRM2DEBUG')
      END IF
      IF (DEPOSDBG .AND. .NOT.DEPOS .AND. .NOT.DDEP .AND.
     &                                         .NOT.WDEP) THEN
C        Write Error Message:  DEPOS debug without deposition options
         CALL ERRHDL(PATH,MODNAM,'E','194','DEPOSDBG')
      END IF
      IF (AREADBG .AND. NAREA.EQ.0 .AND. NCIRC.EQ.0 .AND. NLINE.EQ.0
     &            .AND. NPIT.EQ.0) THEN
C        Write Error Message:  AREA/LINE debug without any applicable
C        sources
         IF (FIELD(IAREA) .EQ. 'AREA') THEN
            CALL ERRHDL(PATH,MODNAM,'E','194','AREADEBUG')
         ELSE IF (FIELD(IAREA) .EQ. 'LINE') THEN
            CALL ERRHDL(PATH,MODNAM,'E','194','LINEDEBUG')
         END IF
      END IF
!CRCO D095 Added for urban debug 8/3/2021
      IF (NURB .EQ. 0 .AND. URBDBUG) THEN
C        Write Error Message:  URBDBUG debug without applicable sources
         CALL ERRHDL(PATH,MODNAM,'E','194','URBANDB')
      END IF
!C END URBDBUG insert
      IF (PRIMEDBG .AND. NSEC.EQ.0) THEN
C        Write Error Message:  PRIME debug without any applicable sources
         CALL ERRHDL(PATH,MODNAM,'E','194','PRIMEDBG')
      END IF

      IF (AWMADWDBG .AND. NSEC.EQ.0) THEN
C        Write Error Message:  AWMADW debug without any applicable sources
         CALL ERRHDL(PATH,MODNAM,'E','194','AWMADWDBG')
      END IF
!CRCO D095 added for BLP debug 12/9/2021
      IF (NBLP .EQ. 0 .AND. BLPDBUG) THEN
C        Write Error Message:  BLPDBUG debug without applicable sources
         CALL ERRHDL(PATH,MODNAM,'E','194','BLPDBUG')
      END IF

      IF (RLINEDBG .AND. NRLINES.EQ.0) THEN
C        Write Error Message:  RLINE debug without any applicable sources
         CALL ERRHDL(PATH,MODNAM,'E','194','RLINEDBG')
      END IF

CCRT     D063 Platform Downwash Debug
      IF (PLATFMDBG .AND. NPNT.EQ.0) THEN
C        Write Error Message:  PLATFORM debug without any applicable sources
         CALL ERRHDL(PATH,MODNAM,'E','194','PLATFMDBG')
      END IF

C     CRT 3/25/02022 D113 Added for sidewash debug
      IF (SWDBG .AND. NSWP .EQ. 0) THEN
C        Write Error Message:  SWDBG debug without any applicable sources
         CALL ERRHDL(PATH,MODNAM,'E','194','SWDBG')
      END IF

C --- Check for user-specified filenames, which should immediately
C     follow the keyword option in the input file
      IF (DEBUG) THEN
         IF (IFC .GE. IMOD+1 .AND.
     &       FIELD(IMOD+1) .NE. 'METEOR' .AND.
     &       FIELD(IMOD+1) .NE. 'AREA' .AND.
     &       FIELD(IMOD+1) .NE. 'LINE' .AND.
     &       FIELD(IMOD+1) .NE. 'RLINE' .AND.
     &       FIELD(IMOD+1) .NE. 'PRIME' .AND.
     &       FIELD(IMOD+1) .NE. 'PVMRM' .AND.
     &       FIELD(IMOD+1) .NE. 'OLM' .AND.
     &       FIELD(IMOD+1) .NE. 'ARM2' .AND.
     &       FIELD(IMOD+1) .NE. 'GRSM' .AND.
     &       FIELD(IMOD+1) .NE. 'DEPOS'.AND.
     &       FIELD(IMOD+1) .NE. 'AWMADW' .AND.
     &       FIELD(IMOD+1) .NE. 'TTRM' .AND.
     &       FIELD(IMOD+1) .NE. 'TTRM2' .AND.
     &       FIELD(IMOD+1) .NE. 'PLATFORM' .AND.
     &       FIELD(IMOD+1) .NE. 'URBANDB' .AND.
     &       FIELD(IMOD+1) .NE. 'BLPDBUG' .AND.
     &       FIELD(IMOD+1) .NE. 'SWPOINT') THEN

C ---       Assign user-specified filename for the MODEL debug option
            DBGFIL = RUNST1(LOCB(IMOD+1):LOCE(IMOD+1))
         ELSE
C ---       Assign default MODEL debug filename
            DBGFIL = 'MODEL.DBG'
         END IF
      END IF

      IF (METEORDBG) THEN
         IF (IFC .GE. IMET+1 .AND.
     &       FIELD(IMET+1) .NE. 'MODEL' .AND.
     &       FIELD(IMET+1) .NE. 'AREA' .AND.
     &       FIELD(IMET+1) .NE. 'LINE' .AND.
     &       FIELD(IMET+1) .NE. 'RLINE' .AND.
     &       FIELD(IMET+1) .NE. 'PRIME' .AND.
     &       FIELD(IMET+1) .NE. 'PVMRM' .AND.
     &       FIELD(IMET+1) .NE. 'OLM' .AND.
     &       FIELD(IMET+1) .NE. 'ARM2' .AND.
     &       FIELD(IMET+1) .NE. 'GRSM' .AND.
     &       FIELD(IMET+1) .NE. 'DEPOS' .AND.
     &       FIELD(IMET+1) .NE. 'AWMADW' .AND.
     &       FIELD(IMET+1) .NE. 'TTRM' .AND.
     &       FIELD(IMET+1) .NE. 'TTRM2' .AND.
     &       FIELD(IMET+1) .NE. 'PLATFORM' .AND.
     &       FIELD(IMET+1) .NE. 'URBANDB' .AND.
     &       FIELD(IMET+1) .NE. 'BLPDBUG' .AND.
     &       FIELD(IMET+1) .NE. 'SWPOINT') THEN

C ---       Assign user-specified filename for the METEOR debug option
            DBMFIL = RUNST1(LOCB(IMET+1):LOCE(IMET+1))
         ELSE
C ---       Assign default METEOR debug filename
            DBMFIL = 'METEOR.DBG'
         END IF
      END IF

      IF (AREADBG) THEN
         IF (IFC .GE. IAREA+1 .AND.
     &       FIELD(IAREA+1) .NE. 'MODEL' .AND.
     &       FIELD(IAREA+1) .NE. 'METEOR' .AND.
     &       FIELD(IAREA+1) .NE. 'RLINE' .AND.
     &       FIELD(IAREA+1) .NE. 'PRIME' .AND.
     &       FIELD(IAREA+1) .NE. 'PVMRM' .AND.
     &       FIELD(IAREA+1) .NE. 'OLM' .AND.
     &       FIELD(IAREA+1) .NE. 'ARM2' .AND.
     &       FIELD(IAREA+1) .NE. 'GRSM' .AND.
     &       FIELD(IAREA+1) .NE. 'DEPOS' .AND.
     &       FIELD(IAREA+1) .NE. 'AWMADW' .AND.
     &       FIELD(IAREA+1) .NE. 'TTRM' .AND.
     &       FIELD(IAREA+1) .NE. 'TTRM2' .AND.
     &       FIELD(IAREA+1) .NE. 'PLATFORM' .AND.
     &       FIELD(IAREA+1) .NE. 'URBANDB' .AND.
     &       FIELD(IAREA+1) .NE. 'BLPDBUG' .AND.
     &       FIELD(IAREA+1) .NE. 'SWPOINT') THEN

C ---       Assign user-specified filename for the AREA debug option
            DBAREAFIL = RUNST1(LOCB(IAREA+1):LOCE(IAREA+1))
         ELSE
C ---       Assign default AREA debug filename
            DBAREAFIL = 'AREA.DBG'
         END IF
      END IF

      IF (PRIMEDBG) THEN
         IF (IFC .GE. IPRM+1 .AND.
     &       FIELD(IPRM+1) .NE. 'MODEL' .AND.
     &       FIELD(IPRM+1) .NE. 'METEOR' .AND.
     &       FIELD(IPRM+1) .NE. 'AREA' .AND.
     &       FIELD(IPRM+1) .NE. 'LINE' .AND.
     &       FIELD(IPRM+1) .NE. 'RLINE' .AND.
     &       FIELD(IPRM+1) .NE. 'PVMRM' .AND.
     &       FIELD(IPRM+1) .NE. 'OLM' .AND.
     &       FIELD(IPRM+1) .NE. 'ARM2' .AND.
     &       FIELD(IPRM+1) .NE. 'GRSM' .AND.
     &       FIELD(IPRM+1) .NE. 'DEPOS' .AND.
     &       FIELD(IPRM+1) .NE. 'AWMADW' .AND.
     &       FIELD(IPRM+1) .NE. 'TTRM' .AND.
     &       FIELD(IPRM+1) .NE. 'TTRM2' .AND.
     &       FIELD(IPRM+1) .NE. 'PLATFORM' .AND.
     &       FIELD(IPRM+1) .NE. 'URBANDB' .AND.
     &       FIELD(IPRM+1) .NE. 'BLPDBUG' .AND.
     &       FIELD(IPRM+1) .NE. 'SWPOINT') THEN

C ---       Assign user-specified filename for the PRIME debug option
            DBPRMFIL = RUNST1(LOCB(IPRM+1):LOCE(IPRM+1))
         ELSE
C ---       Assign default PRIME debug filename
            DBPRMFIL = 'PRIME.DBG'
         END IF
      END IF

      IF (PVMRMDBG) THEN
         IF (IFC .GE. IPVM+1 .AND.
     &       FIELD(IPVM+1) .NE. 'MODEL' .AND.
     &       FIELD(IPVM+1) .NE. 'METEOR' .AND.
     &       FIELD(IPVM+1) .NE. 'AREA' .AND.
     &       FIELD(IPVM+1) .NE. 'LINE' .AND.
     &       FIELD(IPVM+1) .NE. 'RLINE' .AND.
     &       FIELD(IPVM+1) .NE. 'PRIME' .AND.
     &       FIELD(IPVM+1) .NE. 'OLM' .AND.
     &       FIELD(IPVM+1) .NE. 'ARM2' .AND.
     &       FIELD(IPVM+1) .NE. 'GRSM' .AND.
     &       FIELD(IPVM+1) .NE. 'DEPOS' .AND.
     &       FIELD(IPVM+1) .NE. 'AWMADW' .AND.
     &       FIELD(IPVM+1) .NE. 'TTRM' .AND.
     &       FIELD(IPVM+1) .NE. 'TTRM2'.AND.
     &       FIELD(IPVM+1) .NE. 'PLATFORM' .AND.
     &       FIELD(IPVM+1) .NE. 'URBANDB' .AND.
     &       FIELD(IPVM+1) .NE. 'BLPDBUG' .AND.
     &       FIELD(IPVM+1) .NE. 'SWPOINT') THEN

C ---       Assign user-specified filename for the PVMRM debug option
            DBPVFIL = RUNST1(LOCB(IPVM+1):LOCE(IPVM+1))
         ELSE
C ---       Assign default PVMRM debug filename
            IF (PVMRM) THEN
               DBPVFIL = 'PVMRM.DBG'
            END IF
         END IF
C ---    Assign default filename for RELDISP debug file for PVMRM option
         RDISPFIL = 'RelDisp.dbg'
      END IF

      IF (OLMDEBUG) THEN
         IF (IFC .GE. IOLMD+1 .AND.
     &       FIELD(IOLMD+1) .NE. 'MODEL' .AND.
     &       FIELD(IOLMD+1) .NE. 'METEOR' .AND.
     &       FIELD(IOLMD+1) .NE. 'AREA' .AND.
     &       FIELD(IOLMD+1) .NE. 'LINE' .AND.
     &       FIELD(IOLMD+1) .NE. 'RLINE' .AND.
     &       FIELD(IOLMD+1) .NE. 'PRIME' .AND.
     &       FIELD(IOLMD+1) .NE. 'PVMRM' .AND.
     &       FIELD(IOLMD+1) .NE. 'ARM2' .AND.
     &       FIELD(IOLMD+1) .NE. 'GRSM' .AND.
     &       FIELD(IOLMD+1) .NE. 'DEPOS' .AND.
     &       FIELD(IOLMD+1) .NE. 'AWMADW' .AND.
     &       FIELD(IOLMD+1) .NE. 'TTRM' .AND.
     &       FIELD(IOLMD+1) .NE. 'TTRM2' .AND.
     &       FIELD(IOLMD+1) .NE. 'PLATFORM' .AND.
     &       FIELD(IOLMD+1) .NE. 'URBANDB' .AND.
     &       FIELD(IOLMD+1) .NE. 'BLPDBUG' .AND.
     &       FIELD(IOLMD+1) .NE. 'SWPOINT') THEN

C ---       Assign user-specified filename for the OLM debug option
            DBOLMFIL = RUNST1(LOCB(IOLMD+1):LOCE(IOLMD+1))
         ELSE
C ---       Assign default OLM debug filename
            DBOLMFIL = 'OLM.DBG'
         END IF
      END IF

      IF (ARM2DEBUG) THEN
         IF (IFC .GE. IARM2+1 .AND.
     &       FIELD(IARM2+1) .NE. 'MODEL' .AND.
     &       FIELD(IARM2+1) .NE. 'METEOR' .AND.
     &       FIELD(IARM2+1) .NE. 'AREA' .AND.
     &       FIELD(IARM2+1) .NE. 'LINE' .AND.
     &       FIELD(IARM2+1) .NE. 'RLINE' .AND.
     &       FIELD(IARM2+1) .NE. 'PRIME' .AND.
     &       FIELD(IARM2+1) .NE. 'PVMRM' .AND.
     &       FIELD(IARM2+1) .NE. 'OLM' .AND.
     &       FIELD(IARM2+1) .NE. 'GRSM' .AND.
     &       FIELD(IARM2+1) .NE. 'DEPOS' .AND.
     &       FIELD(IARM2+1) .NE. 'AWMADW'.AND.
     &       FIELD(IARM2+1) .NE. 'TTRM' .AND.
     &       FIELD(IARM2+1) .NE. 'TTRM2' .AND.
     &       FIELD(IARM2+1) .NE. 'PLATFORM' .AND.
     &       FIELD(IARM2+1) .NE. 'URBANDB' .AND.
     &       FIELD(IARM2+1) .NE. 'BLPDBUG' .AND.
     &       FIELD(IARM2+1) .NE. 'SWPOINT') THEN

C ---       Assign user-specified filename for the ARM2 debug option
            DBARM2FIL = RUNST1(LOCB(IARM2+1):LOCE(IARM2+1))
         ELSE
C ---       Assign default ARM2 debug filename
            DBARM2FIL = 'ARM2.DBG'
         END IF
      END IF

C     CERC 11/30/20 Code for determining GRSM debug file name
      IF (GRSMDEBUG) THEN
         IF (IFC .GE. IGRSM+1 .AND.
     &       FIELD(IGRSM+1) .NE. 'MODEL' .AND.
     &       FIELD(IGRSM+1) .NE. 'METEOR' .AND.
     &       FIELD(IGRSM+1) .NE. 'AREA' .AND.
     &       FIELD(IGRSM+1) .NE. 'LINE' .AND.
     &       FIELD(IGRSM+1) .NE. 'RLINE' .AND.
     &       FIELD(IGRSM+1) .NE. 'PRIME' .AND.
     &       FIELD(IGRSM+1) .NE. 'OLM' .AND.
     &       FIELD(IGRSM+1) .NE. 'PVMRM' .AND.
     &       FIELD(IGRSM+1) .NE. 'ARM2' .AND.
     &       FIELD(IGRSM+1) .NE. 'DEPOS' .AND.
     &       FIELD(IGRSM+1) .NE. 'AWMADW' .AND.
     &       FIELD(IGRSM+1) .NE. 'TTRM' .AND.
     &       FIELD(IGRSM+1) .NE. 'TTRM2' .AND.
     &       FIELD(IGRSM+1) .NE. 'PLATFORM' .AND.
     &       FIELD(IGRSM+1) .NE. 'URBANDB' .AND.
     &       FIELD(IGRSM+1) .NE. 'BLPDBUG' .AND.
     &       FIELD(IGRSM+1) .NE. 'SWPOINT') THEN

C ---       Assign user-specified filename for the GRSM debug option
            DBGRSMFIL = RUNST1(LOCB(IGRSM+1):LOCE(IGRSM+1))
         ELSE
C ---       Assign default GRSM debug filename
            DBGRSMFIL = 'GRSM.DBG'
         END IF
      END IF

      IF (AWMADWDBG) THEN
         IF (IFC .GE. IPRM2+1 .AND.
     &       FIELD(IPRM2+1) .NE. 'MODEL' .AND.
     &       FIELD(IPRM2+1) .NE. 'METEOR' .AND.
     &       FIELD(IPRM2+1) .NE. 'AREA' .AND.
     &       FIELD(IPRM2+1) .NE. 'LINE' .AND.
     &       FIELD(IPRM2+1) .NE. 'RLINE' .AND.
     &       FIELD(IPRM2+1) .NE. 'PRIME' .AND.
     &       FIELD(IPRM2+1) .NE. 'PVMRM' .AND.
     &       FIELD(IPRM2+1) .NE. 'OLM' .AND.
     &       FIELD(IPRM2+1) .NE. 'ARM2' .AND.
     &       FIELD(IPRM2+1) .NE. 'PLATFORM' .AND.
     &       FIELD(IPRM2+1) .NE. 'GRSM' .AND.
     &       FIELD(IPRM2+1) .NE. 'TTRM' .AND.
     &       FIELD(IPRM2+1) .NE. 'TTRM2' .AND.
     &       FIELD(IPRM2+1) .NE. 'DEPOS' .AND.
     &       FIELD(IPRM2+1) .NE. 'URBANDB' .AND.
     &       FIELD(IPRM2+1) .NE. 'BLPDBUG' .AND.
     &       FIELD(IPRM2+1) .NE. 'SWPOINT') THEN

C ---       Assign user-specified filename for the PRIME debug option
            DBAwmaDwFIL = RUNST1(LOCB(IPRM2+1):LOCE(IPRM2+1))
         ELSE
C ---       Assign default AWMADW debug filename
            DBAwmaDwFIL = 'AWMADW.DBG'
         END IF
      END IF

      IF (RLINEDBG) THEN
         IF (IFC .GE. IRLINE +1 .AND.
     &       FIELD(IRLINE+1) .NE. 'MODEL' .AND.
     &       FIELD(IRLINE+1) .NE. 'METEOR' .AND.
     &       FIELD(IRLINE+1) .NE. 'AREA' .AND.
     &       FIELD(IRLINE+1) .NE. 'LINE' .AND.
     &       FIELD(IRLINE+1) .NE. 'PRIME' .AND.
     &       FIELD(IRLINE+1) .NE. 'PVMRM' .AND.
     &       FIELD(IRLINE+1) .NE. 'OLM' .AND.
     &       FIELD(IRLINE+1) .NE. 'ARM2' .AND.
     &       FIELD(IRLINE+1) .NE. 'GRSM' .AND.
     &       FIELD(IRLINE+1) .NE. 'PLATFORM' .AND.
     &       FIELD(IRLINE+1) .NE. 'AWMADW' .AND.
     &       FIELD(IRLINE+1) .NE. 'TTRM' .AND.
     &       FIELD(IRLINE+1) .NE. 'TTRM2' .AND.
     &       FIELD(IRLINE+1) .NE. 'URBANDB'.AND.
     &       FIELD(IRLINE+1) .NE. 'DEPOS' .AND.
     &       FIELD(IRLINE+1) .NE. 'BLPDBUG' .AND.
     &       FIELD(IRLINE+1) .NE. 'SWPOINT') THEN

C ---       Assign user-specified filename for the RLINE debug option
            RLINEDBGFIL = RUNST1(LOCB(IRLINE+1):LOCE(IRLINE+1))
         ELSE
C ---       Assign default RLINE debug filename
            RLINEDBGFIL = 'RLINE.DBG'
         END IF
      END IF
      
CCRT  D063 Platform Downwash Debug
      IF (PLATFMDBG) THEN
         IF (IFC .GE. IPLATFM+1 .AND.
     &       FIELD(IPLATFM+1) .NE. 'MODEL' .AND.
     &       FIELD(IPLATFM+1) .NE. 'METEOR' .AND.
     &       FIELD(IPLATFM+1) .NE. 'AREA' .AND.
     &       FIELD(IPLATFM+1) .NE. 'LINE' .AND.
     &       FIELD(IPLATFM+1) .NE. 'RLINE' .AND.
     &       FIELD(IPLATFM+1) .NE. 'PRIME' .AND.
     &       FIELD(IPLATFM+1) .NE. 'PVMRM' .AND.
     &       FIELD(IPLATFM+1) .NE. 'OLM' .AND.
     &       FIELD(IPLATFM+1) .NE. 'ARM2' .AND.
     &       FIELD(IPLATFM+1) .NE. 'DEPOS' .AND.
     &       FIELD(IPLATFM+1) .NE. 'GRSM' .AND.
     &       FIELD(IPLATFM+1) .NE. 'TTRM' .AND.
     &       FIELD(IPLATFM+1) .NE. 'TTRM2' .AND.
     &       FIELD(IPLATFM+1) .NE. 'URBANDB'.AND.
     &       FIELD(IPLATFM+1) .NE. 'AWMADW' .AND.
     &       FIELD(IPLATFM+1) .NE. 'SWPOINT') THEN

C ---       Assign user-specified filename for the PRIME debug option
            PLATFMDBGFILE = RUNST1(LOCB(IPLATFM+1):LOCE(IPLATFM+1))
         ELSE
C ---       Assign default PLATFORM debug filename
            PLATFMDBGFILE = 'PLATFORM.DBG'
         END IF
      END IF

C --- Now check for DEPOS option; since DEPOS debug filenames are
C     hardwired, issue warning if user appears to have specified
C     a filename
      IF (DEPOSDBG) THEN
C         JAT 05/08/2020 added from version 19191
C         wet deposition parameters are written to debug file
c         regardless if MODEL debug is chosen.  if model debug
c         not chosen, file is fort.24.  change to DEPOS.DBG
c         if dbgfil not named or next field is not MODEL
         IF (TRIM(ADJUSTL(DBGFIL)) .EQ. '' .OR. FIELD(IDEP+1) .NE.
     &   'MODEL') DBGFIL='DEPOS.DBG'
         IF (IFC .GE. IDEP+1 .AND.
     &       FIELD(IDEP+1) .NE. 'MODEL' .AND.
     &       FIELD(IDEP+1) .NE. 'METEOR' .AND.
     &       FIELD(IDEP+1) .NE. 'AREA' .AND.
     &       FIELD(IDEP+1) .NE. 'LINE' .AND.
     &       FIELD(IDEP+1) .NE. 'PRIME' .AND.
     &       FIELD(IDEP+1) .NE. 'PVMRM' .AND.
     &       FIELD(IDEP+1) .NE. 'ARM2' .AND.
     &       FIELD(IDEP+1) .NE. 'OLM' .AND.
     &       FIELD(IDEP+1) .NE. 'PLATFORM' .AND.
     &       FIELD(IDEP+1) .NE. 'GRSM' .AND.
     &       FIELD(IDEP+1) .NE. 'TTRM' .AND.
     &       FIELD(IDEP+1) .NE. 'AWMADW' .AND.
     &       FIELD(IDEP+1) .NE. 'TTRM2' .AND.
     &       FIELD(IDEP+1) .NE. 'RLINE' .AND.
     &       FIELD(IDEP+1) .NE. 'URBANDB' .AND.
     &       FIELD(IDEP+1) .NE. 'BLPDBUG' .AND.
     &       FIELD(IDEP+1) .NE. 'SWPOINT') THEN

C ---       Write warning message regarding DEPOS debug filenames
            CALL ERRHDL(PATH,MODNAM,'W','203','DEPOSDBG')
         END IF
      END IF

!     Added for TTRM; AECOM
      IF (TTRMDBG) THEN
         IF (IFC .GE. ITTRMD+1 .AND.
     &       FIELD(ITTRMD+1) .NE. 'METEOR' .AND.
     &       FIELD(ITTRMD+1) .NE. 'MODEL' .AND.
     &       FIELD(ITTRMD+1) .NE. 'PRIME' .AND.
     &       FIELD(ITTRMD+1) .NE. 'PVMRM' .AND.
     &       FIELD(ITTRMD+1) .NE. 'OLM' .AND.
     &       FIELD(ITTRMD+1) .NE. 'ARM2' .AND.
     &       FIELD(ITTRMD+1) .NE. 'GRSM' .AND.
     &       FIELD(ITTRMD+1) .NE. 'AREA' .AND.
     &       FIELD(ITTRMD+1) .NE. 'LINE' .AND.
     &       FIELD(ITTRMD+1) .NE. 'AWMADW' .AND.
     &       FIELD(ITTRMD+1) .NE. 'RLINE' .AND.
     &       FIELD(ITTRMD+1) .NE. 'PLATFORM' .AND.
     &       FIELD(ITTRMD+1) .NE. 'DEPOS' .AND.
     &       FIELD(ITTRMD+1) .NE. 'TTRM2' .AND.
     &       FIELD(ITTRMD+1) .NE. 'DEPOS'.AND.
     &       FIELD(ITTRMD+1) .NE. 'URBANDB'.AND.
     &       FIELD(ITTRMD+1) .NE. 'BLPDBUG' .AND.
     &       FIELD(ITTRMD+1) .NE. 'SWPOINT') THEN

!      added for TTRM; AECOM
C ---       Assign user-specified filename for the TTRM debug option
            TTRMFIL = RUNST1(LOCB(ITTRMD+1):LOCE(ITTRMD+1))
         ELSE
C ---       Assign default Ozone Reaction Rate debug filename
            TTRMFIL = 'TTRM_DEBUG.DBG'
         END IF
      END IF
!     End TTRM insert; Feb. 2021

!     Open TTRM2 debug files; the filenames are hard-wired
      IF (RUNTTRM2 .AND. TTRM2DBG) THEN
        OPEN(UNIT=TTRM2TMP(1),FILE='AFTER_TTRM.DBG',
     &       ERR=779,STATUS='REPLACE')
!       Assign 2nd TTRM2 debug file based on selected method
        IF (ARM2) THEN
          OPEN(UNIT=TTRM2TMP(2),FILE='AFTER_ARM2.DBG',ERR=780,
     &          STATUS='REPLACE')
        ELSEIF (OLM) THEN
          OPEN(UNIT=TTRM2TMP(2),FILE='AFTER_OLM.DBG',ERR=780,
     &          STATUS='REPLACE')
        ELSE
          OPEN(UNIT=TTRM2TMP(2),FILE='AFTER_PVMRM.DBG',ERR=780,
     &          STATUS='REPLACE')
        ENDIF
        OPEN(UNIT=TTRM2TMP(3),FILE='TTRM2_MERGE.DBG',ERR=781,
     &        STATUS='REPLACE')
!       Write headers for TTRM2 debug files
        DO I = 1, 3
          WRITE(TTRM2TMP(I),7005) VERSN, TITLE1(1:68),
     &                            RUNDAT, RUNTIM
          WRITE(TTRM2TMP(I),7020)
        ENDDO
           GO TO 7799
 7005 FORMAT('* AERMOD (',A6,'): ',A68,5X,A8,4X,A8)

 7020 FORMAT('*        X             Y      AVERAGE CONC    ZELEV    ',
     &       'ZHILL    ZFLAG    AVE     GRP       DATE     SRC ID   ',
     &       /,
     &       '* ____________  ____________  ____________   ______   _',
     &       '_____   ______  ______  ________  ________  ________  ')
!        WRITE Error Message for Error Opening File
 779     WRITE(DUMMY,'("PSTFL",I4.4)') TTRM2TMP(1)
         CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)
 780     WRITE(DUMMY,'("PSTFL",I4.4)') TTRM2TMP(2)
         CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)
 781     WRITE(DUMMY,'("PSTFL",I4.4)') TTRM2TMP(3)
         CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)
 7799    CONTINUE
      ENDIF


CRCO D095 Added for urban debug 8/3/2021
      IF (URBDBUG) THEN
         IF (IFC .GE. IURBD+1 .AND.
     &       FIELD(IURBD+1) .NE. 'METEOR' .AND.
     &       FIELD(IURBD+1) .NE. 'MODEL' .AND.
     &       FIELD(IURBD+1) .NE. 'AREA' .AND.
     &       FIELD(IURBD+1) .NE. 'LINE' .AND.
     &       FIELD(IURBD+1) .NE. 'PRIME' .AND.
     &       FIELD(IURBD+1) .NE. 'PVMRM' .AND.
     &       FIELD(IURBD+1) .NE. 'OLM' .AND.
     &       FIELD(IURBD+1) .NE. 'ARM2' .AND.
     &       FIELD(IURBD+1) .NE. 'GRSM' .AND.
     &       FIELD(IURBD+1) .NE. 'PLATFORM' .AND.
     &       FIELD(IURBD+1) .NE. 'DEPOS'.AND.
     &       FIELD(IURBD+1) .NE. 'AWMADW' .AND.
     &       FIELD(IURBD+1) .NE. 'TTRM' .AND.
     &       FIELD(IURBD+1) .NE. 'TTRM2' .AND.
     &       FIELD(IURBD+1) .NE. 'RLINE' .AND.
     &       FIELD(IURBD+1) .NE. 'BLPDBUG' .AND.
     &       FIELD(IURBD+1) .NE. 'SWPOINT') THEN
C ---       Assign user-specified filename for the URBDBUG debug option
            URBFIL = RUNST1(LOCB(IURBD+1):LOCE(IURBD+1))
            URBFIL1 = RUNST1(LOCB(IURBD+1):LOCE(IURBD+1)) // "1"
         ELSE
C ---       Assign default URBDBUG debug filename
            URBFIL = 'URBDBUG.DBG'
            URBFIL1 = 'URBDBUG1.DBG'
         END IF
      END IF
! End URBDBUG insert

!     CRCO D095 BLP Debug
      IF (BLPDBUG) THEN
         IF (IFC .GE. IBLP+1 .AND.
     &       FIELD(IBLP+1) .NE. 'METEOR' .AND.
     &       FIELD(IBLP+1) .NE. 'MODEL' .AND.
     &       FIELD(IBLP+1) .NE. 'AREA' .AND.
     &       FIELD(IBLP+1) .NE. 'LINE' .AND.
     &       FIELD(IBLP+1) .NE. 'PRIME' .AND.
     &       FIELD(IBLP+1) .NE. 'PVMRM' .AND.
     &       FIELD(IBLP+1) .NE. 'OLM' .AND.
     &       FIELD(IBLP+1) .NE. 'ARM2' .AND.
     &       FIELD(IBLP+1) .NE. 'GRSM' .AND.
     &       FIELD(IBLP+1) .NE. 'PLATFORM' .AND.
     &       FIELD(IBLP+1) .NE. 'DEPOS'.AND.
     &       FIELD(IBLP+1) .NE. 'AWMADW' .AND.
     &       FIELD(IBLP+1) .NE. 'TTRM' .AND.
     &       FIELD(IBLP+1) .NE. 'TTRM2' .AND.
     &       FIELD(IBLP+1) .NE. 'RLINE' .AND.
     &       FIELD(IBLP+1) .NE. 'URBANDB' .AND.
     &       FIELD(IBLP+1) .NE. 'SWPOINT') THEN
C ---       Assign user-specified filename for the DISTANCE debug option
            BLPFIL = RUNST1(LOCB(IBLP+1):LOCE(IBLP+1))
         ELSE
C ---       Assign default BLPDBUG debug filename
            BLPFIL = 'BLPDBUG.DBG'
         END IF
      END IF
!     End BLPDBUG insert

C     CRT, 3/25/2022 D113 Updated for SIDEWASH
      IF (SWDBG) THEN
         IF (IFC .GE. ISW+1 .AND.
     &       FIELD(ISW+1) .NE. 'METEOR' .AND.
     &       FIELD(ISW+1) .NE. 'MODEL' .AND.
     &       FIELD(ISW+1) .NE. 'PRIME' .AND.
     &       FIELD(ISW+1) .NE. 'PVMRM' .AND.
     &       FIELD(ISW+1) .NE. 'OLM' .AND.
     &       FIELD(ISW+1) .NE. 'ARM2' .AND.
     &       FIELD(ISW+1) .NE. 'GRSM' .AND.
     &       FIELD(ISW+1) .NE. 'AREA' .AND.
     &       FIELD(ISW+1) .NE. 'LINE' .AND.
     &       FIELD(ISW+1) .NE. 'AWMADW' .AND.
     &       FIELD(ISW+1) .NE. 'PLATFORM' .AND.
     &       FIELD(ISW+1) .NE. 'DEPOS' .AND.
     &       FIELD(ISW+1) .NE. 'TTRM' .AND.
     &       FIELD(ISW+1) .NE. 'TTRM2' .AND.
     &       FIELD(ISW+1) .NE. 'RLINE' .AND.
     &       FIELD(ISW+1) .NE. 'URBANDB') THEN

C ---       Assign user-specified filename for the SIDEWASH debug option
            SWFIL = RUNST1(LOCB(ISW+1):LOCE(ISW+1))
         ELSE
C ---       Assign default Sidewash source debug filename
            SWFIL = 'SWPOINT.DBG'
         END IF
      END IF


CRCO D095 Added for urban debug 8/3/2021
      IF (URBDBUG) THEN
         IF (IFC .GE. IURBD+1 .AND.
     &       FIELD(IURBD+1) .NE. 'METEOR' .AND.
     &       FIELD(IURBD+1) .NE. 'MODEL' .AND.
     &       FIELD(IURBD+1) .NE. 'AREA' .AND.
     &       FIELD(IURBD+1) .NE. 'LINE' .AND.
     &       FIELD(IURBD+1) .NE. 'PRIME' .AND.
     &       FIELD(IURBD+1) .NE. 'PVMRM' .AND.
     &       FIELD(IURBD+1) .NE. 'OLM' .AND.
     &       FIELD(IURBD+1) .NE. 'ARM2' .AND.
     &       FIELD(IURBD+1) .NE. 'GRSM' .AND.
     &       FIELD(IURBD+1) .NE. 'PLATFORM' .AND.
     &       FIELD(IURBD+1) .NE. 'DEPOS'.AND.
     &       FIELD(IURBD+1) .NE. 'AWMADW' .AND.
     &       FIELD(IURBD+1) .NE. 'TTRM'.AND.
     &       FIELD(IURBD+1) .NE. 'RLINE' .AND.
     &       FIELD(IURBD+1) .NE. 'BLPDBUG') THEN
C ---       Assign user-specified filename for the URBDBUG debug option
            URBFIL = RUNST1(LOCB(IURBD+1):LOCE(IURBD+1))
            URBFIL1 = RUNST1(LOCB(IURBD+1):LOCE(IURBD+1)) // "1"
         ELSE
C ---       Assign default URBDBUG debug filename
            URBFIL = 'URBDBUG.DBG'
            URBFIL1 = 'URBDBUG1.DBG'
         END IF
      END IF
! End URBDBUG insert

!CRCO D095 BLP Debug
      IF (BLPDBUG) THEN
         IF (IFC .GE. IBLP+1 .AND.
     &       FIELD(IBLP+1) .NE. 'METEOR' .AND.
     &       FIELD(IBLP+1) .NE. 'MODEL' .AND.
     &       FIELD(IBLP+1) .NE. 'AREA' .AND.
     &       FIELD(IBLP+1) .NE. 'LINE' .AND.
     &       FIELD(IBLP+1) .NE. 'PRIME' .AND.
     &       FIELD(IBLP+1) .NE. 'PVMRM' .AND.
     &       FIELD(IBLP+1) .NE. 'OLM' .AND.
     &       FIELD(IBLP+1) .NE. 'ARM2' .AND.
     &       FIELD(IBLP+1) .NE. 'GRSM' .AND.
     &       FIELD(IBLP+1) .NE. 'PLATFORM' .AND.
     &       FIELD(IBLP+1) .NE. 'DEPOS'.AND.
     &       FIELD(IBLP+1) .NE. 'AWMADW' .AND.
     &       FIELD(IBLP+1) .NE. 'TTRM'.AND.
     &       FIELD(IBLP+1) .NE. 'RLINE' .AND.
     &       FIELD(IBLP+1) .NE. 'URBANDB') THEN
C ---       Assign user-specified filename for the DISTANCE debug option
            BLPFIL = RUNST1(LOCB(IBLP+1):LOCE(IBLP+1))
         ELSE
C ---       Assign default BLPDBUG debug filename
            BLPFIL = 'BLPDBUG.DBG'
         END IF
      END IF
! End BLPDBUG insert


C --- Open MODEL, METEOR, AREA, PRIME and AWMADW debug files, if selected;
C     note that PVMRM, OLM, ARM2, GRSM and DEPOS debug files are opened
C     elsewhere
C Unused:  200 FORMAT ( ' OPTIONS: ', A /)
c     JAT 05/08/2020 ADD CODE TO OPEN IF DEBUG OR DEPOSDBG
C     BECAUSE IT USES THE DEBUGFIL AS WELL
C      IF (DEBUG) THEN
      IF (DEBUG .OR. DEPOSDBG) THEN
C        Open debug output file
         DUMMY = 'DebugFile'
         OPEN (UNIT=DBGUNT,FILE=DBGFIL,ERR=91,STATUS='REPLACE')
      END IF

      GOTO 101

C     WRITE Error Message:  Error Opening File
 91   CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

 101  CONTINUE

      IF (METEORDBG) THEN
C        Open debug meteorology output file
         DUMMY = 'DbgMetFile'
         OPEN (UNIT=DBMUNT,FILE=DBMFIL,ERR=92,STATUS='REPLACE')
      END IF

      GOTO 102

C     WRITE Error Message:  Error Opening File
 92   CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

 102  CONTINUE

      IF (AREADBG) THEN
C        Open debug AREA output file
         DUMMY = 'AreaDbgFile'
         OPEN (UNIT=AREADBUNT,FILE=DBAREAFIL,ERR=93,STATUS='REPLACE')
      END IF

      GOTO 103

C     WRITE Error Message:  Error Opening File
 93   CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

 103  CONTINUE

      IF (PRIMEDBG) THEN
C        Open debug PRIME output file
         DUMMY = 'PrimeDbgFile'
         OPEN (UNIT=PRMDBUNT,FILE=DBPRMFIL,ERR=94,STATUS='REPLACE')
      END IF

      GOTO 104

C     WRITE Error Message:  Error Opening File
 94   CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

 104  CONTINUE

      IF (AWMADWDBG) THEN
C        Open debug AWMADW output file
         DUMMY = 'AwmaDwDbgFile'
         OPEN (UNIT=AwmaDwDBUNT,FILE=DBAwmaDwFIL,ERR=95,
     &         STATUS='REPLACE')
      END IF

      GOTO 105

C     WRITE Error Message:  Error Opening File
 95   CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

 105  CONTINUE


      IF (TTRMDBG) THEN
!        Open TTRM output file
         DUMMY = 'TTRMFIL'
         OPEN (UNIT=TTRMUNT,FILE=TTRMFIL,ERR=96,STATUS='REPLACE')
         WRITE(TTRMUNT,'(''TTRM Debug File'',51x,a8,/70x,a8)')
     &                                           rundat, runtim
      END IF

      GOTO 106


C     WRITE Error Message:  Error Opening File
 96   CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

 106  CONTINUE

      IF (RLINEDBG) THEN
C        Open RLINE DEBUG output file
         DUMMY = 'RLINEFIL'
         OPEN (UNIT=RLINEDBUNT,FILE=RLINEDBGFIL,ERR=97,
     &         STATUS='REPLACE')
      END IF

      GOTO 107

C     WRITE Error Message:  Error Opening File
 97   CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

 107  CONTINUE

CRCO D095 Added for urban debug 8/3/2021
! -- Open URBDBUG debug file, if selected;
      IF (URBDBUG) THEN
!        Open URBDBUG output file
         DUMMY = 'URBFIL'
         OPEN (UNIT=URBUNT,FILE=URBFIL,ERR=197,STATUS='REPLACE')
         WRITE(URBUNT,'(''URBDBUG Debug File'',51x,a8,/70x,a8)')
     &                                           rundat, runtim

         DUMMY = 'URBFIL1'
         OPEN (UNIT=URBUNT1,FILE=URBFIL1,ERR=197,STATUS='REPLACE')
         WRITE(URBUNT1,'(''URBDBUG Debug File'',51x,a8,/70x,a8)')
     &                                           rundat, runtim
      END IF

      GOTO 108
C     WRITE Error Message:  Error Opening File
 197  CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)
 108  CONTINUE
!   end of URBDBUG insert

!CRCO D095 BLP debug file
      IF (BLPDBUG) THEN
!        Open BLPDBUG output file
         DUMMY = 'BLPFIL'
         OPEN (UNIT=BLPUNT,FILE=BLPFIL,ERR=198,STATUS='REPLACE')
         WRITE(BLPUNT,'(''BLPDBUG Debug File'',51x,a8,/70x,a8)')
     &                                           rundat, runtim
      END IF

      GOTO 109
C     WRITE Error Message:  Error Opening File
 198  CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)
 109  CONTINUE
!   end of BLPDBUG insert


CCRT 3/22/2021: File is checked and opened in aermod.f
CCRT comment out this code - leave for reference if needed later
CCRT      IF (GRSMDEBUG) THEN
CCRT!        Open GRSM output file
CCRT         DUMMY = 'GRSMFIL'
CCRT         OPEN (UNIT=GRSMDBG,FILE=DBGRSMFIL,ERR=97,STATUS='REPLACE')
CCRT         WRITE(GRSMDBG,'(''GRSM Debug File'',51x,a8,/70x,a8)')
CCRT     &                                           rundat, runtim
CCRT      END IF
CCRT
CCRT      GOTO 107
CCRT
CCRTC     WRITE Error Message:  Error Opening File
CCRT 97   CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)
CCRT
C 110  CONTINUE

CCRT  D063 Platform Downwash Debug
      IF (PLATFMDBG) THEN
C        Open debug PLATFORM output file
         DUMMY = 'PLATFMDBGFILE'
         OPEN (UNIT=PLATFMDBUNT,FILE=PLATFMDBGFILE,ERR=98,
     &         STATUS='REPLACE')
      END IF

      GOTO 110

C     WRITE Error Message:  Error Opening File
 98   CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

 110  CONTINUE

CCRT  D113 Sidewash Debug, 3/25/2022
      IF (SWDBG) THEN
C        Open debug Sidewash output file
         DUMMY = 'SWFIL'
         OPEN (UNIT=SWDBGUNT,FILE=SWFIL,ERR=99,
     &         STATUS='REPLACE')
      END IF

      GOTO 111

C     WRITE Error Message:  Error Opening File
 99   CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

 111  CONTINUE

      IF (IFC .GT. MAXFields) THEN
C        Maximum number of fields exceeded, issue warning message,
C        including up to 12 characters from last field
         WRITE(DUMMY,'(A:)') FIELD(IFC)(1:MIN(12,LEN_TRIM(FIELD(IFC))))
         CALL ERRHDL(PATH,MODNAM,'E','203',DUMMY)
      END IF

      GO TO 999

C     WRITE Error Message:  Error Opening File
C Unused:  99   CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

 999  RETURN
      END

      SUBROUTINE MYEAR
C***********************************************************************
C                 MYEAR Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Process RESTART File Save Option
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:  Treat the 'H6H' field as optional, with a warning
C                   that it is no longer required.
C                   R. W. Brode, U.S. EPA, OAQPS, AQMG, 10/19/2009
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: RSTSAV File Logical Switch and RESTART Filename
C
C        ERROR HANDLING:   Checks for No Parametes (uses default name);
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   COCARD
C***********************************************************************
C
C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'MYEAR'

      IF (RSTSAV) THEN
         CALL ERRHDL(PATH,MODNAM,'E','150','SAVEFILE')

      ELSE IF (RSTINP) THEN
         CALL ERRHDL(PATH,MODNAM,'E','150','INITFILE')

      ELSE IF (.NOT. (POLLUT .EQ. 'PM10' .OR. POLLUT .EQ. 'PM-10' .OR.
     &                POLLUT .EQ. 'NO2'  .OR. POLLUT .EQ. 'SO2'   .OR.
     &                POLLUT .EQ. 'LEAD' .OR. POLLUT .EQ. 'OTHER' .OR.
     &                POLLUT .EQ. 'PM25' .OR. POLLUT .EQ. 'PM-2.5'.OR.
     &                POLLUT .EQ. 'PM-25'.OR. POLLUT .EQ. 'PM2.5') )THEN
C        WRITE Error Message:  Conflicting Options MULTYEAR For Wrong POLLUT
         CALL ERRHDL(PATH,MODNAM,'E','150',POLLUT)

      ELSE IF (IFC .GE. 4 .AND. FIELD(3) .EQ. 'H6H') THEN
C ---    Write Warning Message:  The 'H6H' field is no longer required
C        for the MULTYEAR keyword
         CALL ERRHDL(PATH,MODNAM,'W','352','Keyword ')
         IF (IFC .EQ. 4) THEN
            MULTYR = .TRUE.
            RSTSAV = .TRUE.
C           Use Character Substring to Retrieve Filenames to Maintain Case
            IF ((LOCE(4)-LOCB(4)) .LE. (ILEN_FLD - 1) ) THEN
C              Retrieve Filename as Character Substring to Maintain Original Case
C              Also Check for Filename Larger Than ILEN_FLD Characters
               SAVFIL = RUNST1(LOCB(4):LOCE(4))
            ELSE
C              WRITE Error Message:  SAVFIL Field is Too Long
               WRITE(DUMMY,'(I8)') ILEN_FLD
               CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
               RETURN
            END IF
            SAVFL2 = SAVFIL
C ---       Value of INCRST is Set to 365 or 366 in SUB. MECARD
         ELSE IF (IFC .EQ. 5) THEN
            MULTYR = .TRUE.
            RSTSAV = .TRUE.
C           Use Character Substring to Retrieve Filenames to Maintain Case
            IF ((LOCE(4)-LOCB(4)) .LE. (ILEN_FLD - 1) ) THEN
C              Retrieve Filename as Character Substring to Maintain Original Case
C              Also Check for Filename Larger Than ILEN_FLD Characters
               SAVFIL = RUNST1(LOCB(4):LOCE(4))
            ELSE
C              WRITE Error Message:  SAVFIL Field is Too Long
               WRITE(DUMMY,'(I8)') ILEN_FLD
               CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
               RETURN
            END IF
            SAVFL2 = SAVFIL
            RSTINP = .TRUE.
C           Use Character Substring to Retrieve Filenames to Maintain Case
            IF ((LOCE(5)-LOCB(5)) .LE. (ILEN_FLD - 1) ) THEN
C              Retrieve Filename as Character Substring to Maintain Original Case
C              Also Check for Filename Larger Than ILEN_FLD Characters
               INIFIL = RUNST1(LOCB(5):LOCE(5))
            ELSE
C              WRITE Error Message:  INIFIL Field is Too Long
               WRITE(DUMMY,'(I8)') ILEN_FLD
               CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
               RETURN
            END IF
C ---       Value of INCRST is Set to 365 or 366 in SUB. MECARD
         ELSE IF (IFC .GT. 5) THEN
C           WRITE Error Message           ! Too Many Parameters
            CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         END IF
      ELSE IF (IFC .GE. 3 .AND. FIELD(3) .NE. 'H6H') THEN
C ---    Process input parameters without the 'H6H' keyword
         IF (IFC .EQ. 3) THEN
            MULTYR = .TRUE.
            RSTSAV = .TRUE.
C           Use Character Substring to Retrieve Filenames to Maintain Case
            IF ((LOCE(3)-LOCB(4)) .LE. (ILEN_FLD - 1) ) THEN
C              Retrieve Filename as Character Substring to Maintain Original Case
C              Also Check for Filename Larger Than ILEN_FLD Characters
               SAVFIL = RUNST1(LOCB(3):LOCE(3))
            ELSE
C              WRITE Error Message:  SAVFIL Field is Too Long
               WRITE(DUMMY,'(I8)') ILEN_FLD
               CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
               RETURN
            END IF
            SAVFL2 = SAVFIL
C ---       Value of INCRST is Set to 365 or 366 in SUB. MECARD
         ELSE IF (IFC .EQ. 4) THEN
            MULTYR = .TRUE.
            RSTSAV = .TRUE.
C           Use Character Substring to Retrieve Filenames to Maintain Case
            IF ((LOCE(3)-LOCB(3)) .LE. (ILEN_FLD - 1) ) THEN
C              Retrieve Filename as Character Substring to Maintain Original Case
C              Also Check for Filename Larger Than ILEN_FLD Characters
               SAVFIL = RUNST1(LOCB(3):LOCE(3))
            ELSE
C              WRITE Error Message:  SAVFIL Field is Too Long
               WRITE(DUMMY,'(I8)') ILEN_FLD
               CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
               RETURN
            END IF
            SAVFL2 = SAVFIL
            RSTINP = .TRUE.
C           Use Character Substring to Retrieve Filenames to Maintain Case
            IF ((LOCE(4)-LOCB(4)) .LE. (ILEN_FLD - 1) ) THEN
C              Retrieve Filename as Character Substring to Maintain Original Case
C              Also Check for Filename Larger Than ILEN_FLD Characters
               INIFIL = RUNST1(LOCB(4):LOCE(4))
            ELSE
C              WRITE Error Message:  INIFIL Field is Too Long
               WRITE(DUMMY,'(I8)') ILEN_FLD
               CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
               RETURN
            END IF
C ---       Value of INCRST is Set to 365 or 366 in SUB. MECARD
         ELSE IF (IFC .GT. 4) THEN
C           WRITE Error Message           ! Too Many Parameters
            CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         END IF
      ELSE IF (IFC .EQ. 3 .AND. FIELD(3) .EQ. 'H6H') THEN
C        WRITE Error Message           ! Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
      ELSE IF (IFC .LT. 3) THEN
C        WRITE Error Message           ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      END IF

      RETURN
      END

      SUBROUTINE GDDEF
C***********************************************************************
C                 GDDEF Module of AERMOD Model
C
C        PURPOSE: Processes Dry Deposition Default Parameters for Gases
C
C        PROGRAMMER: R. W. Brode, PES, Inc.
C
C        DATE:    May 16, 1996
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Dry Deposition Reference Parameters for Gases
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'GDDEF'

C     Check the Number of Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 5) THEN
C        Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 6) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Read Gas Dry Deposition Parameters
C     Change Them To Numbers
C     First Get Reactivity Value (fo)
      CALL STODBL(FIELD(3),ILEN_FLD,DNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      END IF
C     Assign The Field
      Fo = DNUM

C     Now Get Fraction of Maximum Green LAI for Seasonal Category 2
      CALL STODBL(FIELD(4),ILEN_FLD,DNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      END IF
C     Assign The Field
      FSEAS2 = DNUM

C     Now Get Fraction of Maximum Green LAI for Seasonal Category 5
      CALL STODBL(FIELD(5),ILEN_FLD,DNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      END IF
C     Assign The Field
      FSEAS5 = DNUM

      IF (IFC .EQ. 6) THEN
C        Get the Reference Species (Optional)
         REFSPE = FIELD(6)
      ELSE
         REFSPE = '      '
      END IF

 999  RETURN
      END

      SUBROUTINE GDSEAS
C***********************************************************************
C                 GDSEAS Module of AERMOD Model
C
C        PURPOSE: Define Seasons for Gas Dry Deposition (per Wesely)
C
C        PROGRAMMER: R. W. Brode, PES, Inc.
C
C        DATE:    May 18, 2001
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Dry Deposition Reference Parameters for Gases
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      INTEGER I, J, ISEA_NDX

C     Variable Initializations
      MODNAM = 'GDSEAS'

C     Check the Number of Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 3) THEN
C        Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 14) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

      ISET = 0
      DO I = 3, IFC
C        Change Fields To Numbers
         CALL STONUM(FIELD(I),ILEN_FLD,FNUM,IMIT)
C        Check The Numerical Field
         IF (IMIT .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            CYCLE
         END IF
         DO J = 1, IMIT
            ISET = ISET + 1
C           Assign The Field
            IF (ISET .LE. 12) THEN
               ISEA_NDX = NINT(FNUM)
               IF (ISEA_NDX .GE. 1 .AND. ISEA_NDX .LE. 5) THEN
                  ISEAS_GD(ISET) = ISEA_NDX
               ELSE
C                 WRITE Error Message    ! Season Index out-of-range
                  CALL ERRHDL(PATH,MODNAM,'E','380',KEYWRD)
               END IF
            ELSE
C              WRITE Error Message    ! Too Many Months Input
               CALL ERRHDL(PATH,MODNAM,'E','234',KEYWRD)
            END IF
         END DO
      END DO

 999  RETURN
      END

      SUBROUTINE GVSUBD
C***********************************************************************
C                 GVSUBD Module of AERMOD Model
C
C        PURPOSE: Processes Dry Deposition Reference Parameters for Gases
C
C        PROGRAMMER: R. W. Brode, PES, Inc.
C
C        DATE:    September 3, 1996
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: User-specified Dry Deposition Velocity for Gases
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'GVSUBD'

C     Check the Number of Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 3) THEN
C        Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 3) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Read User-specified Dry Deposition Velocity
C     Change Them To Numbers
      CALL STODBL(FIELD(3),ILEN_FLD,DNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      END IF
C     Assign The Field
      USERVD = DNUM

C     Perform range/validity check
      IF (USERVD .LT. 0.0D0) THEN
C        Write Error Message:  Negative deposition velocity
         CALL ERRHDL(PATH,MODNAM,'E','209',' USERVD ')
      ELSE IF (USERVD .EQ. 0.0D0) THEN
C        Write Error Message:  Deposition velocity = 0.0
         CALL ERRHDL(PATH,MODNAM,'E','380','USERVD=0')
      ELSE IF (USERVD .GT. 0.05D0) THEN
C        Write Warning Message:  Large deposition velocity
         CALL ERRHDL(PATH,MODNAM,'W','320',' USERVD ')
      END IF

C     Set Logical Variable for User-specified Deposition Velocity
      LUSERVD = .TRUE.

 999  RETURN
      END

      SUBROUTINE GDLAND
C***********************************************************************
C                 GDLAND Module of AERMOD Model
C
C        PURPOSE: Define Land Use Categories by Direction for
C                 Gas Dry Deposition (per Wesely, et al, 2001)
C
C        PROGRAMMER: R. W. Brode, PES, Inc.
C
C        DATE:    December 30, 2002
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Dry Deposition Reference Parameters for Gases
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      INTEGER I, J, ILAND_NDX

C     Variable Initializations
      MODNAM = 'GDLAND'

C     Check the Number of Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 3) THEN
C        Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 38) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

      ISET = 0
      DO I = 3, IFC
C        Change Fields To Numbers
         CALL STONUM(FIELD(I),ILEN_FLD,FNUM,IMIT)
C        Check The Numerical Field
         IF (IMIT .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            CYCLE
         END IF
         DO J = 1, IMIT
            ISET = ISET + 1
C           Assign The Field
            IF (ISET .LE. 36) THEN
               ILAND_NDX = NINT(FNUM)
               IF (ILAND_NDX .GE. 1 .AND. ILAND_NDX .LE. 9) THEN
                  ILAND_GD(ISET) = ILAND_NDX
               ELSE
C                 WRITE Error Message    ! Land Use Index out-of-range
                  CALL ERRHDL(PATH,MODNAM,'E','380',KEYWRD)
               END IF
            ELSE
C              WRITE Error Message    ! Too Many Directions Input
               CALL ERRHDL(PATH,MODNAM,'E','234',KEYWRD)
            END IF
         END DO
      END DO

 999  RETURN
      END

      SUBROUTINE URBOPT
C***********************************************************************
C                 URBOPT Module of AERMOD Model
C
C        PURPOSE: Process Urban Option Inputs
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    June 11, 1996
C
C        MODIFIED:   Adjusted the limit for issuing a warning for urban
C                    population out-of-range from 10,000 to 21,206, which
C                    corresponds to a population density of 750/sq-km for
C                    an area within a 3km radius, consistent with the
C                    Appendix W criterion for urban/rural determination
C                    based on the population density.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG - 02/28/2011
C
C        MODIFIED:   To incorporate handling of non-'default' values of
C                    the optional urban roughness length other than 1m
C                    as non-DFAULT.
C                    To prohibit use of urban roughness length .ne. 1.0m
C                    for regulatory DFAULT applications.  Modified limits
C                    on urban roughness length to generate warning messages.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG - 10/19/2009
C
C        MODIFIED:   To allow for multiple urban areas in a single
C                    model run, and adjust range for issuing warning
C                    regarding optional user-specified urban rounghness.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG - 12/07/06
C
C        MODIFIED:   To include optional parameter for urban roughness
C                    length.  Defaults to 1.0 meter if no value input.
C                    R.W. Brode, PES, Inc. - 09/10/02
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: URBPOP  [R]  Urban population
C                 URBNAM  [C]  Name of urban area (optional)
C                 URBZ0   [R]  Urban roughness lenght, m (optional)
C                                defaults to 1.0 meter
C
C        ERROR HANDLING:   Checks for Invalid Parameters;
C                          Checks for No Parameters;
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      INTEGER I
      CHARACTER MODNAM*12, TEMPID*8

C     Variable Initializations
      MODNAM = 'URBOPT'

C     Determine Whether There Are Too Few Or Too Many Parameter Fields
      IF ((.NOT. L_MULTURB .AND. IFC .LT. 3) .OR.
     &          (L_MULTURB .AND. IFC .LT. 4)) THEN
C        WRITE Error Message: Missing Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF ((.NOT. L_MULTURB .AND. IFC .GT. 5) .OR.
     &               (L_MULTURB .AND. IFC .GT. 6)) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

      IF (.NOT. L_URBAN_ALL .AND. L_MULTURB) THEN
C        READ in the Urban ID for multiple urban areas
         IF ((LOCE(3)-LOCB(3)) .LE. 7) THEN
C*          Retrieve Source ID Character Substring
            TEMPID = FIELD(3)
         ELSE
C*          WRITE Error Message:  Urban ID Field is Too Long
            CALL ERRHDL(PATH,MODNAM,'E','219',FIELD(ISC)(1:12))
            RECERR = .TRUE.
            GO TO 999
         END IF
         DO I = 1, NUMURB
            IF (TEMPID .EQ. URBID(I)) THEN
C              WRITE Error Message:  Urban ID already defined
               CALL ERRHDL(PATH,MODNAM,'E','303',TEMPID)
C              Exit to END
               GO TO 999
            END IF
         END DO

C        New Urban ID Defined, Increment Counters
         IURB = IURB + 1
         IF (IURB .GT. NURB) THEN
C           WRITE Error Message    ! Too Many Urban Areas Specified
C           This shouldn't occur since limits are dynamically allocated
            WRITE(DUMMY,'(''NURB='',I7)') NURB
            CALL ERRHDL(PATH,MODNAM,'E','290',DUMMY)
C           Exit to END
            GO TO 999
         END IF
         NUMURB = NUMURB + 1
         URBID(IURB) = TEMPID

         IF (IFC .GE. 4) THEN
            CALL STODBL(FIELD(4),ILEN_FLD,URBPOP(IURB),IMIT)
            IF (IMIT .NE. 1) THEN
C              Write Error Message:Invalid Numerical Field
               CALL ERRHDL(PATH,MODNAM,'E','208','URB-POP')
            ELSE IF (URBPOP(IURB) .LT. 100.0D0) THEN
C ---          Urban population below about 90 will cause math error
C ---          Write Error Message:Invalid Value Specified
               CALL ERRHDL(PATH,MODNAM,'E','203','URB-POP')
            ELSE IF (URBPOP(IURB) .LT. 21206.0D0) THEN
C ---          Flag urban population below 21,206 as potentially out-of-range;
C              this value corresponds with a population density of 750/sq-km
C              across an area of 3km in radius, a criterion cited for urban
C              classification in Section 7.2.3(d) of Appendix W.
               CALL ERRHDL(PATH,MODNAM,'W','320','URB-POP')
            END IF
         END IF

         IF (IFC .GE. 5) THEN
C           Assign name of urban area (optional)
            URBNAM(IURB) = FIELD(5)
         END IF

         IF (IFC .EQ. 6) THEN
C           Assign value of urban roughness length (optional)
            CALL STODBL(FIELD(6),ILEN_FLD,URBZ0(IURB),IMIT)
            IF (IMIT .NE. 1) THEN
C              Write Error Message:Invalid Numerical Field
               CALL ERRHDL(PATH,MODNAM,'E','208','URBAN_Z0')
            ELSE
               IF (DFAULT .AND. URBZ0(IURB) .NE. 1.0D0) THEN
C                 Write Warning Message: Non-default urban roughness length
                  CALL ERRHDL(PATH,MODNAM,'W','206','URBAN_Z0')
                  URBZ0(IURB) = 1.0D0
               ELSE IF (.NOT. DFAULT .AND. URBZ0(IURB) .NE. 1.0D0) THEN
C                 Set flag for use of non-DEFAULT option
                  L_NonDFAULT = .TRUE.
               END IF
               IF (URBZ0(IURB) .LT. 0.80D0) THEN
C                 Write Warning Message: Urban roughness out of range
                  WRITE(DUMMY,'(F8.2)') URBZ0(IURB)
                  CALL ERRHDL(PATH,MODNAM,'W','353',DUMMY)
               ELSE IF (URBZ0(IURB) .GT. 1.50D0 .AND.
     &                  URBZ0(IURB) .LT. 5.0D0) THEN
C                 Write Warning Message: Urban roughness out of range
                  WRITE(DUMMY,'(F8.2)') URBZ0(IURB)
                  CALL ERRHDL(PATH,MODNAM,'W','353',DUMMY)
               ELSE IF (URBZ0(IURB) .GE. 5.0D0) THEN
C                 Write Error Message: Urban roughness out of range
                  CALL ERRHDL(PATH,MODNAM,'E','380','URBAN Z0')
               END IF
            END IF
         ELSE
            URBZ0(IURB) = 1.0D0
         END IF

      ELSE IF (L_URBAN_ALL .AND. L_MULTURB) THEN
C        Write Error Message: URBANSRC ALL option with
C        multiple URBAN areas
         CALL ERRHDL(PATH,MODNAM,'E','279','URBANSRC ALL')

      ELSE
C        Single Urban Area - Process Inputs without URBAN ID

         IURB = 1

         IF (IFC .GE. 3) THEN
            CALL STODBL(FIELD(3),ILEN_FLD,URBPOP(IURB),IMIT)
            IF (IMIT .NE. 1) THEN
C              Write Error Message:Invalid Numerical Field
               CALL ERRHDL(PATH,MODNAM,'E','208','URB-POP')
            ELSE IF (URBPOP(IURB) .LT. 100.0D0) THEN
C ---          Urban population below about 90 will cause math error
C ---          Write Error Message:Invalid Value Specified
               CALL ERRHDL(PATH,MODNAM,'E','203','URB-POP')
            ELSE IF (URBPOP(IURB) .LT. 21206.0D0) THEN
C ---          Flag urban population below 21,206 as potentially out-of-range;
C              this value corresponds with a population density of 750/sq-km
C              across an area of 3km in radius, a criterion cited for urban
C              classification in Section 7.2.3(d) of Appendix W.
               CALL ERRHDL(PATH,MODNAM,'W','320','URB-POP')
            END IF
         END IF

         IF (IFC .GE. 4) THEN
C           Assign name of urban area (optional)
            URBNAM(IURB) = FIELD(4)
         END IF

         IF (IFC .EQ. 5) THEN
C           Assign value of urban roughness length (optional)
            CALL STODBL(FIELD(5),ILEN_FLD,URBZ0(IURB),IMIT)
            IF (IMIT .NE. 1) THEN
C              Write Error Message:Invalid Numerical Field
               CALL ERRHDL(PATH,MODNAM,'E','208','URBAN_Z0')
            ELSE
               IF (DFAULT .AND. URBZ0(IURB) .NE. 1.0D0) THEN
C                 Write Warning Message: Non-default urban roughness length
                  CALL ERRHDL(PATH,MODNAM,'W','206','URBAN_Z0')
                  URBZ0(IURB) = 1.0D0
               ELSE IF (.NOT. DFAULT .AND. URBZ0(IURB) .NE. 1.0D0) THEN
C                 Set flag for use of non-DEFAULT option
                  L_NonDFAULT = .TRUE.
               END IF
               IF (URBZ0(IURB) .LT. 0.80D0) THEN
C                 Write Warning Message: Urban roughness out of range
                  WRITE(DUMMY,'(F8.2)') URBZ0(IURB)
                  CALL ERRHDL(PATH,MODNAM,'W','353',DUMMY)
               ELSE IF (URBZ0(IURB) .GT. 1.50D0 .AND.
     &                  URBZ0(IURB) .LT. 5.0D0) THEN
C                 Write Warning Message: Urban roughness out of range
                  WRITE(DUMMY,'(F8.2)') URBZ0(IURB)
                  CALL ERRHDL(PATH,MODNAM,'W','353',DUMMY)
               ELSE IF (URBZ0(IURB) .GE. 5.0D0) THEN
C                 Write Error Message: Urban roughness out of range
                  CALL ERRHDL(PATH,MODNAM,'E','380','URBAN Z0')
               END IF
            END IF
         ELSE
            URBZ0(IURB) = 1.0D0
         END IF

         NUMURB = 1

      END IF

C     Assign Logical for Urban Option
      URBAN  = .TRUE.

 999  RETURN
      END

      SUBROUTINE O3VAL
C***********************************************************************
C                 O3VAL Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes Non-temporally-varying Ozone Value Option,
C                 CO OZONEVAL
C
C        PROGRAMMER: Roger W. Brode
C
C        DATE:    May 3, 2002
C
C        MODIFIED: To allow for sector-varying values
C                  R. W. Brode, U.S. EPA, OAQPS, AQMG, XX/YY/2013
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS:
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE

      INTEGER :: I

      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'O3VAL'

C --- Check The Number Of The Fields, accounting for sector-varying values
      IF (.NOT.L_O3Sector) THEN
         IF (IFC .LE. 2) THEN
C           Error Message: No Parameters
            CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
            GO TO 999
         ELSE IF (IFC .GT. 4) THEN
C           Error Message: Too Many Parameters
            CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
            GO TO 999
         END IF
C ---    Check for SECT ID in field 3 in case O3SECTOR keyword was omitted
         IF (FIELD(3)(1:4) .EQ. 'SECT') THEN
C           Error Message: SECT ID without O3SECTOR keyword
            CALL ERRHDL(PATH,MODNAM,'E','171',KEYWRD)
            GO TO 999
         END IF
C ---    Assign sector ID to 1 since sector-varying values not being used;
C        also set field index for the user-specified O3VALUES option and
C        assign the option to O3FLAG variable
         IO3SECT = 1
         I = 3
         L_O3VAL(IO3SECT) = .TRUE.

      ELSE
C ---    Process inputs based on O3SECTOR option
         IF (IFC .LE. 2) THEN
C           Error Message: No Parameters
            CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
            GO TO 999
         ELSE IF (IFC .EQ. 4) THEN
            IF (FIELD(3)(1:4) .NE. 'SECT') THEN
C              Error Message: Invalid sector field
               CALL ERRHDL(PATH,MODNAM,'E','203','O3SECTOR ID')
               GO TO 999
            ELSE
C              Error Message: No Numerical Parameters
               CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
               GO TO 999
            END IF
         ELSE IF (IFC .GT. 5) THEN
C           Error Message: Too Many Parameters
            CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
            GO TO 999
         END IF
C ---    Determine user-specified sector
         IF (FIELD(3) .EQ. 'SECT1') THEN
            IO3SECT = 1
         ELSE IF (FIELD(3) .EQ. 'SECT2') THEN
            IO3SECT = 2
         ELSE IF (FIELD(3) .EQ. 'SECT3') THEN
            IO3SECT = 3
         ELSE IF (FIELD(3) .EQ. 'SECT4') THEN
            IO3SECT = 4
         ELSE IF (FIELD(3) .EQ. 'SECT5') THEN
            IO3SECT = 5
         ELSE IF (FIELD(3) .EQ. 'SECT6') THEN
            IO3SECT = 6
         ELSE
C           Error Message: Invalid sector definition
            CALL ERRHDL(PATH,MODNAM,'E','203','O3SECTOR ID')
            GO TO 999
         END IF

C ---    Set field index for the user-specified Ozone Value
         I = 4
         L_O3VAL(IO3SECT) = .TRUE.

      END IF

C     Get Ozone Value, O3BACK, for applicable sector
      CALL STODBL(FIELD(I),ILEN_FLD,DNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      END IF

C     Assign value to O3BACK variable for this sector
      O3BACK(IO3SECT) = DNUM

C     Check for units of ozone value
      IF (IFC .EQ. I+1) THEN
         IF (FIELD(I+1).EQ.'PPM' .OR. FIELD(I+1).EQ.'PPB' .OR.
     &       FIELD(I+1).EQ.'UG/M3') THEN
            O3VALUNITS = FIELD(I+1)
         ELSE
C           Write Error Message:  Invalid units for ozone value
            CALL ERRHDL(PATH,MODNAM,'E','203',' O3UNITS')
         END IF
      END IF

      IF (O3VALUNITS .EQ. 'PPB') THEN
         O3BACK(IO3SECT) = O3BACK(IO3SECT) * O3_PPB
      ELSE IF (O3VALUNITS .EQ. 'PPM') then
         O3BACK(IO3SECT) = O3BACK(IO3SECT) * O3_PPM
      END IF

C     Check range of value
      IF (O3BACK(IO3SECT) .LE. 0.0D0 .OR.
     &    O3BACK(IO3SECT) .GT. 500.0D0)THEN
         CALL ERRHDL(PATH,MODNAM,'W','320',' O3BACK ')
      END IF

 999  RETURN
      END

      SUBROUTINE NOXVAL
C***********************************************************************
C                 NOXVAL Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes Non-temporally-varying NOx Value Option,
C                 CO NOXVALUE
C
C        PROGRAMMER: CERC
C
C        DATE:    November 2020
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS:
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE

      INTEGER :: I

      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'NOXVAL'

C --- Check The Number Of The Fields, accounting for sector-varying values
      IF (.NOT.L_NOXSector) THEN
         IF (IFC .LE. 2) THEN
C           Error Message: No Parameters
            CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
            GO TO 999
         ELSE IF (IFC .GT. 4) THEN
C           Error Message: Too Many Parameters
            CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
            GO TO 999
         END IF
C ---    Check for SECT ID in field 3 in case NOXSECTR keyword was omitted
         IF (FIELD(3)(1:4) .EQ. 'SECT') THEN
C           Error Message: SECT ID without NOXSECTR keyword
            CALL ERRHDL(PATH,MODNAM,'E','171',KEYWRD)
            GO TO 999
         END IF
C ---    Assign sector ID to 1 since sector-varying values not being used;
C        also set field index for the user-specified NOXVALUES option and
C        assign the option to NOXFLAG variable
         INOXSECT = 1
         I = 3
         L_NOXVALUE(INOXSECT) = .TRUE.
!
      ELSE
C ---    Process inputs based on NOXSECTR option
         IF (IFC .LE. 2) THEN
C           Error Message: No Parameters
            CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
            GO TO 999
         ELSE IF (IFC .EQ. 4) THEN
            IF (FIELD(3)(1:4) .NE. 'SECT') THEN
C              Error Message: Invalid sector field
               CALL ERRHDL(PATH,MODNAM,'E','203','NOXSECTOR ID')
               GO TO 999
            ELSE
C              Error Message: No Numerical Parameters
               CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
               GO TO 999
            END IF
         ELSE IF (IFC .GT. 5) THEN
C           Error Message: Too Many Parameters
            CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
            GO TO 999
         END IF
C ---    Determine user-specified sector
         IF (FIELD(3) .EQ. 'SECT1') THEN
            INOXSECT = 1
         ELSE IF (FIELD(3) .EQ. 'SECT2') THEN
            INOXSECT = 2
         ELSE IF (FIELD(3) .EQ. 'SECT3') THEN
            INOXSECT = 3
         ELSE IF (FIELD(3) .EQ. 'SECT4') THEN
            INOXSECT = 4
         ELSE IF (FIELD(3) .EQ. 'SECT5') THEN
            INOXSECT = 5
         ELSE IF (FIELD(3) .EQ. 'SECT6') THEN
            INOXSECT = 6
         ELSE
C           Error Message: Invalid sector definition
            CALL ERRHDL(PATH,MODNAM,'E','203','NOXSECTOR ID')
            GO TO 999
         END IF

C ---    Set field index for the user-specified NOX Value
         I = 4
         L_NOXVALUE(INOXSECT) = .TRUE.
      END IF

C     Get NOX Value, NOXBACK, for applicable sector
      CALL STODBL(FIELD(I),ILEN_FLD,DNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      END IF

C     Assign value to NOXBACK variable for this sector
      NOXBACK(INOXSECT) = DNUM

C     Check for units of NOx value
      IF (IFC .EQ. I+1) THEN
         IF (FIELD(I+1).EQ.'PPM' .OR. FIELD(I+1).EQ.'PPB' .OR.
     &       FIELD(I+1).EQ.'UG/M3') THEN
            NOXVALUNITS = FIELD(I+1)
         ELSE
C           Write Error Message:  Invalid units for NOX value
            CALL ERRHDL(PATH,MODNAM,'E','203',' NOXUNITS')
         END IF
      END IF

      !Convert to UG/M3 using NO2 factors (NOx expressed as 'NOx as NO2')
      IF (NOXVALUNITS .EQ. 'PPB') THEN
         NOXBACK(INOXSECT) = NOXBACK(INOXSECT) / NO2_PPB
      ELSE IF (NOXVALUNITS .EQ. 'PPM') then
         NOXBACK(INOXSECT) = NOXBACK(INOXSECT) / NO2_PPM
      END IF

C     Check range of value
      IF (NOXBACK(INOXSECT) .LE. 0.0D0) THEN
         CALL ERRHDL(PATH,MODNAM,'W','320',' NOXBACK ')
      END IF

 999  RETURN
      END SUBROUTINE NOXVAL

      SUBROUTINE O3FILE
C***********************************************************************
C                 O3FILE Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Process Ozone Data File Option for OZONEFIL keyword
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    May 3, 2002
C
C        MODIFIED: Include checks for potential problem with Fortran
C                  format specifier.  Should include from 1 to 4
C                  integers for date variables, and one real for
C                  ozone data variable.  Warning message is issued
C                  if too many or too few integers/reals are specified.
C                  An error message may also be issued when reading
C                  the ozone file depending on the compiler options.
C                  R. W. Brode, U.S. EPA, OAQPS, AQMG, 04/13/2011
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS:
C
C        ERROR HANDLING:   Checks for No Parametes (uses default name);
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   COCARD
C***********************************************************************
C
C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, NumInt, NumReal
      LOGICAL :: FOPEN

C     Variable Initializations
      MODNAM = 'O3FILE'
      NumInt  = 0
      NumReal = 0
      FOPEN   = .FALSE.

C --- Check The Number Of The Fields, accounting for sector-varying values
      IF (.NOT.L_O3Sector) THEN
         IF (IFC .LE. 2) THEN
C           Error Message: No Parameters
            CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
            GO TO 999
         ELSE IF (IFC .LT. 3) THEN
C           Error Message: Not Enough Parameters
            CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
            GO TO 999
         ELSE IF (IFC .GT. 5) THEN
C           Error Message: Too Many Parameters
            CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
            GO TO 999
         END IF
C ---    Check for SECT ID in field 3 in case O3FILE keyword was omitted
         IF (FIELD(3)(1:4) .EQ. 'SECT') THEN
C           Error Message: SECT ID without O3SECTOR keyword
            CALL ERRHDL(PATH,MODNAM,'E','171',KEYWRD)
            GO TO 999
         END IF
C ---    Assign sector ID to 1 since sector-varying values not being used;
C        also set field index for start of the user-specified options
         IO3SECT = 1
         I = 3
      ELSE
         IF (IFC .LE. 2) THEN
C           Error Message: No Parameters
            CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
            GO TO 999
         ELSE IF (IFC .LT. 4) THEN
C           Error Message: Not Enough Parameters
            CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
            GO TO 999
         ELSE IF (IFC .GT. 6) THEN
C           Error Message: Too Many Parameters
            CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
            GO TO 999
         END IF
         IF (FIELD(3) .EQ. 'SECT1') THEN
            IO3SECT = 1
         ELSE IF (FIELD(3) .EQ. 'SECT2' .AND. NUMO3Sects .GE. 2) THEN
            IO3SECT = 2
         ELSE IF (FIELD(3) .EQ. 'SECT3' .AND. NUMO3Sects .GE. 3) THEN
            IO3SECT = 3
         ELSE IF (FIELD(3) .EQ. 'SECT4' .AND. NUMO3Sects .GE. 4) THEN
            IO3SECT = 4
         ELSE IF (FIELD(3) .EQ. 'SECT5' .AND. NUMO3Sects .GE. 5) THEN
            IO3SECT = 5
         ELSE IF (FIELD(3) .EQ. 'SECT6' .AND. NUMO3Sects .EQ. 6) THEN
            IO3SECT = 6
         ELSE
C           Error Message: Invalid sector field
            CALL ERRHDL(PATH,MODNAM,'E','203','O3SECTOR ID')
            GO TO 999
         END IF
C ---    Assign set field index for start of the user-specified options,
C        accounting for sector IDs
         I = 4
      END IF

C     Set logical flags for hourly ozone file(s)
      L_O3Hourly        = .TRUE.
      L_O3File(IO3SECT) = .TRUE.

C     Retrieve Ozone Data Filename as Character Substring to Maintain Case
      IF ((LOCE(I)-LOCB(I)) .LE. (ILEN_FLD - 1) ) THEN
C        Retrieve Filename as Character Substring to Maintain Original Case
C        Also Check for Filename Larger Than ILEN_FLD Characters
         OZONFL(IO3SECT) = RUNST1(LOCB(I):LOCE(I))
      ELSE
C        WRITE Error Message:  OZONFL Field is Too Long
C        Write error message and return
         WRITE(DUMMY,'(I8)') ILEN_FLD
         CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
         GO TO 999
      END IF

C     Assign file unit for this O3 file and Open The Ozone Input File
C     Open with ACTION='READ' to prevent overwrite and multiple access
      IO3UNT(IO3SECT) = 1000 + IO3SECT

C     Open hourly Ozone File If Not Already Open
      INQUIRE (FILE=OZONFL(IO3SECT),OPENED=FOPEN)

      IF (.NOT. FOPEN) THEN
C        Open Hourly Ozone Data File If Not Already Open
C        Open with ACTION='READ' to prevent overwrite and allow multiple access
         INQUIRE (UNIT=IO3UNT(IO3SECT),OPENED=FOPEN)
         IF (.NOT. FOPEN) THEN
            OPEN(UNIT=IO3UNT(IO3SECT),FILE=OZONFL(IO3SECT),STATUS='OLD',
     &          ERR=998,ACTION='READ',FORM='FORMATTED')

         ELSE
C           Hourly Ozone File is Already Opened With Different Filename
            CALL ERRHDL(PATH,MODNAM,'E','501',KEYWRD)
            GO TO 999
         END IF
      ELSE
C        Hourly Ozone File is Already Opened With Different Filename
         CALL ERRHDL(PATH,MODNAM,'E','501',KEYWRD)
         GO TO 999
      END IF

C     Check for optional units of ozone value
      IF (I .EQ. 3 .AND. IFC .GE. 4) THEN
         IF (FIELD(4).EQ.'PPM' .OR. FIELD(4).EQ.'PPB' .OR.
     &       FIELD(4).EQ.'UG/M3') THEN
            O3FILUNITS = FIELD(4)
         ELSE
C           Write Error Message:  Invalid units for ozone value
            CALL ERRHDL(PATH,MODNAM,'E','203',' O3UNITS')
         END IF
      ELSE IF (I .EQ. 4 .AND. IFC .GE. 5) THEN
         IF (FIELD(5).EQ.'PPM' .OR. FIELD(5).EQ.'PPB' .OR.
     &       FIELD(5).EQ.'UG/M3') THEN
            O3FILUNITS = FIELD(5)
         ELSE
C           Write Error Message:  Invalid units for ozone value
            CALL ERRHDL(PATH,MODNAM,'E','203',' O3UNITS')
         END IF
      ELSE
         O3FILUNITS = 'UG/M3'
      END IF

      IF (IFC .EQ. I+2) THEN
C        Check for Format String > ILEN_FLD PARAMETER
         IF ((LOCE(I+2)-LOCB(I+2)) .LE. (ILEN_FLD - 1)) THEN

C ---       First check for user input of "FREE" for the formaat,
C           using FIELD array which has been converted to upper case
            IF (FIELD(I+2) .EQ. 'FREE') THEN
               O3FORM(IO3SECT) = 'FREE'
            ELSE
C              Retrieve Met Format as Char. Substring
               O3FORM(IO3SECT) = RUNST1(LOCB(I+2):LOCE(I+2))
C ---          Check for correct format specifiers for Ozone file;
C              should be 4 integers for date variables and 1 real for
C              ozone concentration; allow for 1 to 4 integers since
C              format statement may include 4I2, and also allow for
C              either F, E, or D format for the data variable.
               DO I = 1, LEN_TRIM(O3FORM(IO3SECT))
                  IF (O3FORM(IO3SECT)(I:I).EQ.'I' .OR.
     &                O3FORM(IO3SECT)(I:I).EQ.'i') THEN
                     NumInt  = NumInt  + 1
                  ELSE IF (O3FORM(IO3SECT)(I:I).EQ.'F' .OR.
     &                     O3FORM(IO3SECT)(I:I).EQ.'f') THEN
                     NumReal = NumReal + 1
                  ELSE IF (O3FORM(IO3SECT)(I:I).EQ.'E' .OR.
     &                     O3FORM(IO3SECT)(I:I).EQ.'e') THEN
                     NumReal = NumReal + 1
                  ELSE IF (O3FORM(IO3SECT)(I:I).EQ.'D' .OR.
     &                     O3FORM(IO3SECT)(I:I).EQ.'d') THEN
                     NumReal = NumReal + 1
                  END IF
               END DO
               IF (NumInt.LT.1 .OR. NumInt.GT.4) THEN
C                 WRITE Warning Message:  Potential problem with O3FORM
                  WRITE(DUMMY,'(''NumInts= '',I3)') NumInt
                  CALL ERRHDL(PATH,MODNAM,'W','292',DUMMY)
               END IF
               IF (NumReal.NE.1) THEN
C                 WRITE Warning Message:  Potential problem with O3FORM
                  WRITE(DUMMY,'(''NumReal= '',I3)') NumReal
                  CALL ERRHDL(PATH,MODNAM,'W','292',DUMMY)
               END IF
            END IF
         ELSE
C           WRITE Error Message:  O3FORM Field is Too Long
            WRITE(DUMMY,'(''LEN='',I6)') LOCE(5)-LOCB(5)
            CALL ERRHDL(PATH,MODNAM,'E','292',DUMMY)
         END IF
      ELSE
C ---    Use 'free' format as the default
         O3FORM(IO3SECT) = 'FREE'
      END IF

      GO TO 999

C     Process Error Messages; error opening file, include file type and sector
 998  CONTINUE
      WRITE(DUMMY,'("O3FILE SECT",I1)') IO3SECT
      CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

 999  RETURN
      END

      SUBROUTINE NOXFILE
C***********************************************************************
C                 NOXFILE Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Process NOx Data File Option for NOX_FILE keyword
C
C        PROGRAMMER: CERC
C
C        DATE:     November 2020
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS:
C
C        ERROR HANDLING:   Checks for No Parametes (uses default name);
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   COCARD
C***********************************************************************
C
C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, NumInt, NumReal
      LOGICAL :: FOPEN

C     Variable Initializations
      MODNAM = 'NOXFILE'
      NumInt  = 0
      NumReal = 0
      FOPEN   = .FALSE.

C --- Check The Number Of The Fields, accounting for sector-varying values
      IF (.NOT.L_NOxSector) THEN
         IF (IFC .LE. 2) THEN
C           Error Message: No Parameters
            CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
            GO TO 999
         ELSE IF (IFC .LT. 3) THEN
C           Error Message: Not Enough Parameters
            CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
            GO TO 999
         ELSE IF (IFC .GT. 5) THEN
C           Error Message: Too Many Parameters
            CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
            GO TO 999
         END IF
C ---    Check for SECT ID in field 3 in case NOXFILE keyword was omitted
         IF (FIELD(3)(1:4) .EQ. 'SECT') THEN
C           Error Message: SECT ID without NOXSECTR keyword
            CALL ERRHDL(PATH,MODNAM,'E','171',KEYWRD)
            GO TO 999
         END IF
C ---    Assign sector ID to 1 since sector-varying values not being used;
C        also set field index for start of the user-specified options
         INOXSECT = 1
         I = 3
      ELSE
         IF (IFC .LE. 2) THEN
C           Error Message: No Parameters
            CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
            GO TO 999
         ELSE IF (IFC .LT. 4) THEN
C           Error Message: Not Enough Parameters
            CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
            GO TO 999
         ELSE IF (IFC .GT. 6) THEN
C           Error Message: Too Many Parameters
            CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
            GO TO 999
         END IF
         IF (FIELD(3) .EQ. 'SECT1') THEN
            INOXSECT = 1
         ELSE IF (FIELD(3) .EQ. 'SECT2' .AND. NUMNOxSects .GE. 2) THEN
            INOXSECT = 2
         ELSE IF (FIELD(3) .EQ. 'SECT3' .AND. NUMNOxSects .GE. 3) THEN
            INOXSECT = 3
         ELSE IF (FIELD(3) .EQ. 'SECT4' .AND. NUMNOxSects .GE. 4) THEN
            INOXSECT = 4
         ELSE IF (FIELD(3) .EQ. 'SECT5' .AND. NUMNOxSects .GE. 5) THEN
            INOXSECT = 5
         ELSE IF (FIELD(3) .EQ. 'SECT6' .AND. NUMNOxSects .EQ. 6) THEN
            INOXSECT = 6
         ELSE
C           Error Message: Invalid sector field
            CALL ERRHDL(PATH,MODNAM,'E','203','NOXSECTR ID')
            GO TO 999
         END IF
C ---    Assign set field index for start of the user-specified options,
C        accounting for sector IDs
         I = 4
      END IF

C     Set logical flags for hourly NOx file(s)
      L_NOxHourly        = .TRUE.
      L_NOxFile(INOXSECT) = .TRUE.

C     Retrieve NOx Data Filename as Character Substring to Maintain Case
      IF ((LOCE(I)-LOCB(I)) .LE. (ILEN_FLD - 1) ) THEN
C        Retrieve Filename as Character Substring to Maintain Original Case
C        Also Check for Filename Larger Than ILEN_FLD Characters
         NOXFL(INOXSECT) = RUNST1(LOCB(I):LOCE(I))
      ELSE
C        WRITE Error Message:  NOXFL Field is Too Long
C        Write error message and return
         WRITE(DUMMY,'(I8)') ILEN_FLD
         CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
         GO TO 999
      END IF

C     Assign file unit for this NOx file and Open The NOx Input File
C     Open with ACTION='READ' to prevent overwrite and multiple access
      INOXUNT(INOXSECT) = 3000 + INOXSECT

C     Open hourly NOx File If Not Already Open
      INQUIRE (FILE=NOXFL(INOXSECT),OPENED=FOPEN)

      IF (.NOT. FOPEN) THEN
C        Open Hourly NOx Data File If Not Already Open
C        Open with ACTION='READ' to prevent overwrite and allow multiple access
         INQUIRE (UNIT=INOXUNT(INOXSECT),OPENED=FOPEN)
         IF (.NOT. FOPEN) THEN
            OPEN(UNIT=INOXUNT(INOXSECT),FILE=NOXFL(INOXSECT),
     &           STATUS='OLD',ERR=998,ACTION='READ',FORM='FORMATTED')

         ELSE
C           Hourly NOx File is Already Opened With Different Filename
            CALL ERRHDL(PATH,MODNAM,'E','501',KEYWRD)
            GO TO 999
         END IF
      ELSE
C        Hourly NOx File is Already Opened With Different Filename
         CALL ERRHDL(PATH,MODNAM,'E','501',KEYWRD)
         GO TO 999
      END IF

C     Check for optional units of NOx value
      IF (I .EQ. 3 .AND. IFC .GE. 4) THEN
         IF (FIELD(4).EQ.'PPM' .OR. FIELD(4).EQ.'PPB' .OR.
     &       FIELD(4).EQ.'UG/M3') THEN
            NOXFILUNITS = FIELD(4)
         ELSE
C           Write Error Message:  Invalid units for NOx value
            CALL ERRHDL(PATH,MODNAM,'E','203','NOXUNITS')
         END IF
      ELSE IF (I .EQ. 4 .AND. IFC .GE. 5) THEN
         IF (FIELD(5).EQ.'PPM' .OR. FIELD(5).EQ.'PPB' .OR.
     &       FIELD(5).EQ.'UG/M3') THEN
            NOXFILUNITS = FIELD(5)
         ELSE
C           Write Error Message:  Invalid units for NOx value
            CALL ERRHDL(PATH,MODNAM,'E','203','NOXUNITS')
         END IF
      ELSE
         NOXFILUNITS = 'UG/M3'
      END IF

      IF (IFC .EQ. I+2) THEN
C        Check for Format String > ILEN_FLD PARAMETER
         IF ((LOCE(I+2)-LOCB(I+2)) .LE. (ILEN_FLD - 1)) THEN

C ---       First check for user input of "FREE" for the format,
C           using FIELD array which has been converted to upper case
            IF (FIELD(I+2) .EQ. 'FREE') THEN
               NOXFORM(INOXSECT) = 'FREE'
            ELSE
C              Retrieve Format as Char. Substring
               NOXFORM(INOXSECT) = RUNST1(LOCB(I+2):LOCE(I+2))
C ---          Check for correct format specifiers for NOx file;
C              should be 4 integers for date variables and 1 real for
C              NOx concentration; allow for 1 to 4 integers since
C              format statement may include 4I2, and also allow for
C              either F, E, or D format for the data variable.
               DO I = 1, LEN_TRIM(NOXFORM(INOXSECT))
                  IF (NOXFORM(INOXSECT)(I:I).EQ.'I' .OR.
     &                NOXFORM(INOXSECT)(I:I).EQ.'i') THEN
                     NumInt  = NumInt  + 1
                  ELSE IF (NOXFORM(INOXSECT)(I:I).EQ.'F' .OR.
     &                     NOXFORM(INOXSECT)(I:I).EQ.'f') THEN
                     NumReal = NumReal + 1
                  ELSE IF (NOXFORM(INOXSECT)(I:I).EQ.'E' .OR.
     &                     NOXFORM(INOXSECT)(I:I).EQ.'e') THEN
                     NumReal = NumReal + 1
                  ELSE IF (NOXFORM(INOXSECT)(I:I).EQ.'D' .OR.
     &                     NOXFORM(INOXSECT)(I:I).EQ.'d') THEN
                     NumReal = NumReal + 1
                  END IF
               END DO
               IF (NumInt.LT.1 .OR. NumInt.GT.4) THEN
C                 WRITE Warning Message:  Potential problem with NOXFORM
                  WRITE(DUMMY,'(''NumInts= '',I3)') NumInt
                  CALL ERRHDL(PATH,MODNAM,'W','292',DUMMY)
               END IF
               IF (NumReal.NE.1) THEN
C                 WRITE Warning Message:  Potential problem with NOXFORM
                  WRITE(DUMMY,'(''NumReal= '',I3)') NumReal
                  CALL ERRHDL(PATH,MODNAM,'W','292',DUMMY)
               END IF
            END IF
         ELSE
C           WRITE Error Message:  NOXFORM Field is Too Long
            WRITE(DUMMY,'(''LEN='',I6)') LOCE(5)-LOCB(5)
            CALL ERRHDL(PATH,MODNAM,'E','292',DUMMY)
         END IF
      ELSE
C ---    Use 'free' format as the default
         NOXFORM(INOXSECT) = 'FREE'
      END IF

      GO TO 999

C     Process Error Messages; error opening file, include file type and sector
 998  CONTINUE
      WRITE(DUMMY,'("NOXFILE SCT",I1)') INOXSECT
      CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

 999  RETURN
      END SUBROUTINE NOXFILE

      SUBROUTINE NO2EQ
C***********************************************************************
C                 NO2EQ Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes NO2 Equilibrium Value for PVMRM based on
C                 the NO2EQUIL keyword
C
C        PROGRAMMER: Roger W. Brode
C
C        DATE:    May 3, 2004
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS:
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'NO2EQ'

C     Check The Number Of The Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 3) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Start To Get Ozone Value
      CALL STODBL(FIELD(3),ILEN_FLD,DNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      END IF

C     Assign value to NO2Equil variable
      NO2Equil = DNUM

C     Check range of value
      IF (NO2Equil .LT. 0.10D0 .OR. NO2Equil .GT. 1.0D0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','380','NO2Equil')
      END IF

 999  RETURN
      END


      SUBROUTINE NO2STK
C***********************************************************************
C                 NO2STK Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes NO2 Default In-stack Ratio Value for PVMRM
C                 based on the NO2STACK keyword
C
C        PROGRAMMER: Roger W. Brode
C
C        DATE:    September 7, 2005
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS:
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      INTEGER I
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'NO2STK'

C     Check The Number Of The Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 3) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Start To Get Ozone Value
      CALL STODBL(FIELD(3),ILEN_FLD,DNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      END IF

C     Assign value to NO2Stack variable
      NO2Stack = DNUM

C     Check range of value
      IF (NO2Stack .LT. 0.0D0 .OR. NO2Stack .GT. 1.0D0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','380','NO2Stack')
         GO TO 999
      END IF

      DO I = 1, NSRC
         ANO2_RATIO(I) = NO2Stack
      END DO

 999  RETURN
      END

      SUBROUTINE ARM2_Ratios
C***********************************************************************
C                 ARM2_Ratios Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes minimum and maximum NO2/NOx ratios for ARM2 option
C                 under the keyword.
C
C        PROGRAMMER: Roger W. Brode
C
C        DATE:    Nov. 25, 2013
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS:
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'ARM2_Ratios'

C     Check The Number Of The Fields
      IF (ARM2 .AND. IFC .LT. 4) THEN
C        Error Message: Too Few Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      END IF


C --- Get the minimum ARM2 ratio (ARM2_Min)
      CALL STODBL(FIELD(3),ILEN_FLD,DNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      ELSE
C        Assign value to ARM2_Min
         ARM2_Min = DNUM
      END IF

C     Check range of value for ARM2_Min
      IF (ARM2_Min .LE. 0.0D0) THEN
         WRITE(DUMMY,'(''ARM2Min <= 0'')')
         CALL ERRHDL(PATH,MODNAM,'E','380',DUMMY)
      ELSE IF (ARM2_Min .GT. 1.0D0) THEN
         WRITE(DUMMY,'(''ARM2Min > 1'')')
         CALL ERRHDL(PATH,MODNAM,'E','380',DUMMY)
      ELSE IF (ARM2_Min .LT. 0.50D0) THEN
         IF( DFAULT )THEN
            WRITE(DUMMY,'(''ARM2Min <0.5'')')
            CALL ERRHDL(PATH,MODNAM,'E','380',DUMMY)
         ELSE
            WRITE(DUMMY,'(''ARM2Min <0.5'')')
            CALL ERRHDL(PATH,MODNAM,'W','380',DUMMY)
         END IF
      ELSE IF (ARM2_Min .GT. 0.50D0) THEN
         WRITE(DUMMY,'(''ARM2Min >0.5'')')
         CALL ERRHDL(PATH,MODNAM,'W','380',DUMMY)
      END IF

C --- Get the maximum ARM2 ratio (ARM2_Max)
      CALL STODBL(FIELD(4),ILEN_FLD,DNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      ELSE
C        Assign value to ARM2_Max
         ARM2_Max = DNUM
      END IF

C     Check range of value for ARM2_Max
      IF (ARM2_Max .LE. 0.0D0) THEN
         WRITE(DUMMY,'(''ARM2Max <= 0'')')
         CALL ERRHDL(PATH,MODNAM,'E','380',DUMMY)
      ELSE IF (ARM2_Max .GT. 1.0D0) THEN
         WRITE(DUMMY,'(''ARM2Max > 1'')')
         CALL ERRHDL(PATH,MODNAM,'E','380',DUMMY)
      ELSE IF (ARM2_Max .LT. ARM2_Min) THEN
         WRITE(DUMMY,'(''ARM2 Max<Min'')')
         CALL ERRHDL(PATH,MODNAM,'E','380',DUMMY)
      ELSE IF (ARM2_Max .LT. 0.90D0) THEN
         IF( DFAULT )THEN
            WRITE(DUMMY,'(''ARM2Max <0.9'')')
            CALL ERRHDL(PATH,MODNAM,'E','380',DUMMY)
         ELSE
            WRITE(DUMMY,'(''ARM2Max <0.9'')')
            CALL ERRHDL(PATH,MODNAM,'W','380',DUMMY)
         END IF
      END IF

 999  RETURN
      END

      SUBROUTINE O3VALS
C***********************************************************************
C                 O3VALS Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes User-specified Ozone concentrations, using
C                 the O3VALUES keyword, based on same options for
C                 temporal variability as the EMISFACT keyword for
C                 source emissions
C
C        PROGRAMMER:  Roger Brode
C
C        DATE:       February 28, 2011
C
C        MODIFIED: Corrected the test for number of parameters to
C                  be .GE. 4 to allow for the ANNUAL option.
C                  R. W. Brode, U.S. EPA, OAQPS, AQMG, 12/19/2011
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Variable Emmission Rate Factors
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I
C Unused:      INTEGER :: IH, IL, ISDX, NNN
C Unused:      CHARACTER (LEN=12) :: LID, HID, LID1, LID2, HID1, HID2
C Unused:      CHARACTER (LEN=ILEN_FLD) :: SOID
C Unused:      LOGICAL FOUND, INGRP, RMARK

C     Variable Initializations
      MODNAM = 'O3VALS'

C     Check The Number Of The Fields, accounting for sector-varying values
      IF (.NOT.L_O3Sector) THEN
         IF (IFC .LE. 2) THEN
C           Error Message: No Parameters
            CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
            GO TO 999
         ELSE IF (IFC .EQ. 3) THEN
C           Error Message: No Numerical Parameters
            CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
            GO TO 999
         ELSE IF (IFC .LT. 4) THEN
C           Error Message: Not Enough Parameters
            CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
            GO TO 999
         END IF
C ---    Check for SECT ID in field 3 in case O3SECTOR keyword was omitted
         IF (FIELD(3)(1:4) .EQ. 'SECT') THEN
C           Error Message: SECT ID without O3SECTOR keyword
            CALL ERRHDL(PATH,MODNAM,'E','171',KEYWRD)
            GO TO 999
         END IF
C ---    Assign sector ID to 1 since sector-varying values not being used;
C        also set field index for the user-specified O3VALUES option and
C        assign the option to O3FLAG variable
         IO3SECT = 1
         I = 3
         L_O3VALUES(IO3SECT) = .TRUE.
         IF (IO3MAX(IO3SECT) .GE. 1 .AND.
     &       O3FLAG(IO3SECT) .NE. FIELD(I)) THEN
            CALL ERRHDL(PATH,MODNAM,'E','167',FIELD(I))
         ELSE
            O3FLAG(IO3SECT) = FIELD(I)
         END IF
      ELSE
C ---    Process inputs based on O3SECTOR option
         IF (IFC .LE. 2) THEN
C           Error Message: No Parameters
            CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
            GO TO 999
         ELSE IF (IFC .EQ. 4) THEN
            IF (FIELD(3)(1:4) .NE. 'SECT') THEN
C              Error Message: Invalid sector field
               CALL ERRHDL(PATH,MODNAM,'E','203','O3SECTOR ID')
               GO TO 999
            ELSE
C              Error Message: No Numerical Parameters
               CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
               GO TO 999
            END IF
         ELSE IF (IFC .LT. 5) THEN
C           Error Message: Not Enough Parameters
            CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
            GO TO 999
         END IF
C ---    Determine user-specified sector
         IF (FIELD(3) .EQ. 'SECT1') THEN
            IO3SECT = 1
         ELSE IF (FIELD(3) .EQ. 'SECT2' .AND. NUMO3Sects .GE. 2) THEN
            IO3SECT = 2
         ELSE IF (FIELD(3) .EQ. 'SECT3' .AND. NUMO3Sects .GE. 3) THEN
            IO3SECT = 3
         ELSE IF (FIELD(3) .EQ. 'SECT4' .AND. NUMO3Sects .GE. 4) THEN
            IO3SECT = 4
         ELSE IF (FIELD(3) .EQ. 'SECT5' .AND. NUMO3Sects .GE. 5) THEN
            IO3SECT = 5
         ELSE IF (FIELD(3) .EQ. 'SECT6' .AND. NUMO3Sects .EQ. 6) THEN
            IO3SECT = 6
         ELSE
C           Error Message: Invalid sector field
            CALL ERRHDL(PATH,MODNAM,'E','203','O3SECTOR ID')
            GO TO 999
         END IF
C ---    Set field index for the user-specified O3VALUES option and
C        assign the option to O3FLAG variable
         I = 4
         L_O3VALUES(IO3SECT) = .TRUE.
         IF (IO3MAX(IO3SECT) .GE. 1 .AND.
     &       O3FLAG(IO3SECT) .NE. FIELD(I)) THEN
            IF (LEN_TRIM(FIELD(I)) .GT. 6) THEN
               WRITE(DUMMY,'(''SEC'',I1,1X,A)') IO3SECT,
     &                                  FIELD(I)(1:LEN_TRIM(FIELD(I)))
               CALL ERRHDL(PATH,MODNAM,'E','167',DUMMY)
            ELSE
               WRITE(DUMMY,'(''SECT'',I1,1X,A)') IO3SECT,
     &                                  FIELD(I)(1:LEN_TRIM(FIELD(I)))
               CALL ERRHDL(PATH,MODNAM,'E','167',DUMMY)
            END IF
         ELSE
            O3FLAG(IO3SECT) = FIELD(I)
         END IF
      END IF

C --- Assign number of ozone values based on O3FLAG option
      IF (O3FLAG(IO3SECT) .EQ. 'ANNUAL') THEN
         IO3MAX(IO3SECT) = 1
      ELSE IF (O3FLAG(IO3SECT) .EQ. 'SEASON') THEN
         IO3MAX(IO3SECT) = 4
      ELSE IF (O3FLAG(IO3SECT) .EQ. 'MONTH') THEN
         IO3MAX(IO3SECT) = 12
      ELSE IF (O3FLAG(IO3SECT) .EQ. 'HROFDY') THEN
         IO3MAX(IO3SECT) = 24
      ELSE IF (O3FLAG(IO3SECT) .EQ. 'WSPEED') THEN
         IO3MAX(IO3SECT) = 6
      ELSE IF (O3FLAG(IO3SECT) .EQ. 'SEASHR') THEN
         IO3MAX(IO3SECT) = 96
      ELSE IF (O3FLAG(IO3SECT) .EQ. 'HRDOW') THEN
         IO3MAX(IO3SECT) = 72
         L_DayOfWeekOpts = .TRUE.
      ELSE IF (O3FLAG(IO3SECT) .EQ. 'HRDOW7') THEN
         IO3MAX(IO3SECT) = 168
         L_DayOfWeekOpts = .TRUE.
      ELSE IF (O3FLAG(IO3SECT) .EQ. 'SHRDOW') THEN
         IO3MAX(IO3SECT) = 288
         L_DayOfWeekOpts = .TRUE.
      ELSE IF (O3FLAG(IO3SECT) .EQ. 'SHRDOW7') THEN
         IO3MAX(IO3SECT) = 672
         L_DayOfWeekOpts = .TRUE.
      ELSE IF (O3FLAG(IO3SECT) .EQ. 'MHRDOW') THEN
         IO3MAX(IO3SECT) = 864
         L_DayOfWeekOpts = .TRUE.
      ELSE IF (O3FLAG(IO3SECT) .EQ. 'MHRDOW7') THEN
         IO3MAX(IO3SECT) = 2016
         L_DayOfWeekOpts = .TRUE.
      ELSE
C        WRITE Error Message    ! Invalid O3FLAG Field Entered
         CALL ERRHDL(PATH,MODNAM,'E','203','O3FLAG')
         GO TO 999
      END IF

C --- Call subroutine O3FILL to fill the temporally-varying O3 data
      CALL O3FILL

 999  RETURN
      END

      SUBROUTINE NOXVALS
C***********************************************************************
C                 NOXVALS Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes User-specified NOx concentrations, using
C                 the NOX_VALS keyword, based on same options for
C                 temporal variability as the EMISFACT keyword for
C                 source emissions
C
C        PROGRAMMER:  CERC
C
C        DATE:       November 2020
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Variable Emmission Rate Factors
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I

C     Variable Initializations
      MODNAM = 'NOXVALS'

C     Check The Number Of The Fields, accounting for sector-varying values
      IF (.NOT.L_NOXSector) THEN
         IF (IFC .LE. 2) THEN
C           Error Message: No Parameters
            CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
            GO TO 999
         ELSE IF (IFC .EQ. 3) THEN
C           Error Message: No Numerical Parameters
            CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
            GO TO 999
         ELSE IF (IFC .LT. 4) THEN
C           Error Message: Not Enough Parameters
            CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
            GO TO 999
         END IF
C ---    Check for SECT ID in field 3 in case NOXSECTR keyword was omitted
         IF (FIELD(3)(1:4) .EQ. 'SECT') THEN
C           Error Message: SECT ID without NOXSECTOR keyword
            CALL ERRHDL(PATH,MODNAM,'E','171',KEYWRD)
            GO TO 999
         END IF
C ---    Assign sector ID to 1 since sector-varying values not being used;
C        also set field index for the user-specified NOX_VALS option and
C        assign the option to NOXFLAG variable
         INOXSECT = 1
         I = 3
         L_NOX_VALS(INOXSECT) = .TRUE.
         IF (INOXMAX(INOXSECT) .GE. 1 .AND.
     &       NOXFLAG(INOXSECT) .NE. FIELD(I)) THEN
            CALL ERRHDL(PATH,MODNAM,'E','606',FIELD(I))
         ELSE
            NOXFLAG(INOXSECT) = FIELD(I)
         END IF
      ELSE
C ---    Process inputs based on NOXSECTOR option
         IF (IFC .LE. 2) THEN
C           Error Message: No Parameters
            CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
            GO TO 999
         ELSE IF (IFC .EQ. 4) THEN
            IF (FIELD(3)(1:4) .NE. 'SECT') THEN
C              Error Message: Invalid sector field
               CALL ERRHDL(PATH,MODNAM,'E','203','NOXSECTR ID')
               GO TO 999
            ELSE
C              Error Message: No Numerical Parameters
               CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
               GO TO 999
            END IF
         ELSE IF (IFC .LT. 5) THEN
C           Error Message: Not Enough Parameters
            CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
            GO TO 999
         END IF
C ---    Determine user-specified sector
         IF (FIELD(3) .EQ. 'SECT1') THEN
            INOXSECT = 1
         ELSE IF (FIELD(3) .EQ. 'SECT2' .AND. NUMNOxSects .GE. 2) THEN
            INOXSECT = 2
         ELSE IF (FIELD(3) .EQ. 'SECT3' .AND. NUMNOxSects .GE. 3) THEN
            INOXSECT = 3
         ELSE IF (FIELD(3) .EQ. 'SECT4' .AND. NUMNOxSects .GE. 4) THEN
            INOXSECT = 4
         ELSE IF (FIELD(3) .EQ. 'SECT5' .AND. NUMNOxSects .GE. 5) THEN
            INOXSECT = 5
         ELSE IF (FIELD(3) .EQ. 'SECT6' .AND. NUMNOxSects .EQ. 6) THEN
            INOXSECT = 6
         ELSE
C           Error Message: Invalid sector field
            CALL ERRHDL(PATH,MODNAM,'E','203','NOXSECTR ID')
            GO TO 999
         END IF
C ---    Set field index for the user-specified NOX_VALS option and
C        assign the option to NOXFLAG variable
         I = 4
         L_NOX_VALS(INOXSECT) = .TRUE.
         IF (INOXMAX(INOXSECT) .GE. 1 .AND.
     &       NOXFLAG(INOXSECT) .NE. FIELD(I)) THEN
            IF (LEN_TRIM(FIELD(I)) .GT. 6) THEN
               WRITE(DUMMY,'(''SEC'',I1,1X,A)') INOXSECT,
     &                                  FIELD(I)(1:LEN_TRIM(FIELD(I)))
               CALL ERRHDL(PATH,MODNAM,'E','606',DUMMY)
            ELSE
               WRITE(DUMMY,'(''SECT'',I1,1X,A)') INOXSECT,
     &                                  FIELD(I)(1:LEN_TRIM(FIELD(I)))
               CALL ERRHDL(PATH,MODNAM,'E','606',DUMMY)
            END IF
         ELSE
            NOXFLAG(INOXSECT) = FIELD(I)
         END IF
      END IF

C --- Assign number of NOx values based on NOXFLAG option
      IF (NOXFLAG(INOXSECT) .EQ. 'ANNUAL') THEN
         INOXMAX(INOXSECT) = 1
      ELSE IF (NOXFLAG(INOXSECT) .EQ. 'SEASON') THEN
         INOXMAX(INOXSECT) = 4
      ELSE IF (NOXFLAG(INOXSECT) .EQ. 'MONTH') THEN
         INOXMAX(INOXSECT) = 12
      ELSE IF (NOXFLAG(INOXSECT) .EQ. 'HROFDY') THEN
         INOXMAX(INOXSECT) = 24
      ELSE IF (NOXFLAG(INOXSECT) .EQ. 'WSPEED') THEN
         INOXMAX(INOXSECT) = 6
      ELSE IF (NOXFLAG(INOXSECT) .EQ. 'SEASHR') THEN
         INOXMAX(INOXSECT) = 96
      ELSE IF (NOXFLAG(INOXSECT) .EQ. 'HRDOW') THEN
         INOXMAX(INOXSECT) = 72
         L_DayOfWeekOpts = .TRUE.
      ELSE IF (NOXFLAG(INOXSECT) .EQ. 'HRDOW7') THEN
         INOXMAX(INOXSECT) = 168
         L_DayOfWeekOpts = .TRUE.
      ELSE IF (NOXFLAG(INOXSECT) .EQ. 'SHRDOW') THEN
         INOXMAX(INOXSECT) = 288
         L_DayOfWeekOpts = .TRUE.
      ELSE IF (NOXFLAG(INOXSECT) .EQ. 'SHRDOW7') THEN
         INOXMAX(INOXSECT) = 672
         L_DayOfWeekOpts = .TRUE.
      ELSE IF (NOXFLAG(INOXSECT) .EQ. 'MHRDOW') THEN
         INOXMAX(INOXSECT) = 864
         L_DayOfWeekOpts = .TRUE.
      ELSE IF (NOXFLAG(INOXSECT) .EQ. 'MHRDOW7') THEN
         INOXMAX(INOXSECT) = 2016
         L_DayOfWeekOpts = .TRUE.
      ELSE
C        WRITE Error Message    ! Invalid NOXFLAG Field Entered
         CALL ERRHDL(PATH,MODNAM,'E','203','NOXFLG')
         GO TO 999
      END IF

C --- Call subroutine NOXFILL to fill the temporally-varying NOX data
      CALL NOXFILL

 999  RETURN
      END SUBROUTINE NOXVALS

      SUBROUTINE O3FILL
C***********************************************************************
C                 O3FILL Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Fill Variable Ozone Concentration Array
C
C        PROGRAMMER:  Roger Brode
C
C        DATE:       February 28, 2011
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Direction Specific Building Directions
C
C        CALLED FROM:   O3VALS
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, J, K

C --- Variable Initializations
      MODNAM = 'O3FILL'

C --- Initialize counter for number of O3VALUES for this sector
      ISET = IO3SET(IO3SECT)

C --- Assign field number for start of data values based on whether
C     sector-varying values are used
      IF (L_O3Sector) THEN
         I = 5
      ELSE
         I = 4
      END IF

      DO K = I, IFC
C        Change Fields To Numbers
         CALL STODBL(FIELD(K),ILEN_FLD,DNUM,IMIT)
C        Check The Numerical Field
         IF (IMIT .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            CYCLE
         END IF
         DO J = 1, IMIT
            ISET = ISET + 1
C           Assign The Field
            IF (ISET .LE. IO3MAX(IO3SECT)) THEN
               O3VARY(ISET,IO3SECT) = DNUM
               IF (DNUM .LT. 0.0D0) THEN
C                 WRITE Error Message:  Negative Value for O3VALUES
                  CALL ERRHDL(PATH,MODNAM,'E','209',KEYWRD)
               END IF
            ELSE
C              WRITE Error Message    ! Too Many O3VALUES Input
               IF (L_O3Sector) THEN
                  WRITE(DUMMY,'(''O3VALs SECT'',I1)') IO3SECT
               ELSE
                  WRITE(DUMMY,'(''O3VALUES'')')
               END IF
               CALL ERRHDL(PATH,MODNAM,'E','231',DUMMY)
               GO TO 99
            END IF
         END DO
      END DO

99    CONTINUE

C --- Save counter on number of values input so far for this sector
      IO3SET(IO3SECT) = ISET

      RETURN
      END

      SUBROUTINE NOXFILL
C***********************************************************************
C                 NOXFILL Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Fill Variable NOx Concentration Array
C
C        PROGRAMMER:  CERC
C
C        DATE:       November 2020
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Direction Specific Building Directions
C
C        CALLED FROM:   NOXVALS
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, J, K

C --- Variable Initializations
      MODNAM = 'NOXFILL'

C --- Initialize counter for number of NOX_VALS for this sector
      ISET = INOXSET(INOXSECT)

C --- Assign field number for start of data values based on whether
C     sector-varying values are used
      IF (L_NOxSector) THEN
         I = 5
      ELSE
         I = 4
      END IF

      DO K = I, IFC
C        Change Fields To Numbers
         CALL STODBL(FIELD(K),ILEN_FLD,DNUM,IMIT)
C        Check The Numerical Field
         IF (IMIT .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            CYCLE
         END IF
         DO J = 1, IMIT
            ISET = ISET + 1
C           Assign The Field
            IF (ISET .LE. INOXMAX(INOXSECT)) THEN
               NOXVARY(ISET,INOXSECT) = DNUM
               IF (DNUM .LT. 0.0D0) THEN
C                 WRITE Error Message:  Negative Value for NOX_VALS
                  CALL ERRHDL(PATH,MODNAM,'E','209',KEYWRD)
               END IF
            ELSE
C              WRITE Error Message    ! Too Many NOX_VALS Input
               IF (L_NOxSector) THEN
                  WRITE(DUMMY,'(''NOXVAL SECT'',I1)') INOXSECT
               ELSE
                  WRITE(DUMMY,'(''NOX_VALS'')')
               END IF
               CALL ERRHDL(PATH,MODNAM,'E','231',DUMMY)
               GO TO 99
            END IF
         END DO
      END DO

99    CONTINUE

C --- Save counter on number of values input so far for this sector
      INOXSET(INOXSECT) = ISET

      RETURN
      END SUBROUTINE NOXFILL

      SUBROUTINE OZON_UNIT
C***********************************************************************
C                 OZON_UNIT Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes user-specified units for O3VALUES keyword
C                 ozone concentrations, based on the OZONUNIT keyword
C
C        PROGRAMMER:  Roger Brode
C
C        DATE:       February 28, 2011
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Variable Emmission Rate Factors
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'OZON_UNIT'

C     Check The Number Of The Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 3) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Check for units of background values
      IF (FIELD(3).EQ.'PPM' .OR. FIELD(3).EQ.'PPB' .OR.
     &    FIELD(3).EQ.'UG/M3') THEN
         OzoneUnits = FIELD(3)
      ELSE
C        Write Error Message:  Invalid units for O3VALUES
         CALL ERRHDL(PATH,MODNAM,'E','203','OzoneUnits')
      END IF

 999  RETURN
      END

      SUBROUTINE NOX_UNIT
C***********************************************************************
C                 NOX_UNIT Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes user-specified units for NOX_VALS keyword
C                 NOx concentrations, based on the NOX_VALS keyword
C
C        PROGRAMMER:  CERC
C
C        DATE:       November 2020
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Variable Emmission Rate Factors
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'NOX_UNIT'

C     Check The Number Of The Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 3) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Check for units of background values
      IF (FIELD(3).EQ.'PPM' .OR. FIELD(3).EQ.'PPB' .OR.
     &    FIELD(3).EQ.'UG/M3') THEN
         NOxUnits = FIELD(3)
      ELSE
C        Write Error Message:  Invalid units for NOX_VALS
         CALL ERRHDL(PATH,MODNAM,'E','203','NOxUnits')
      END IF

 999  RETURN
      END SUBROUTINE NOX_UNIT

      SUBROUTINE O3SECTOR
C***********************************************************************
C                 O3SECTOR Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes user-specified WD sectors for background
C                 ozone concentrations, based on the O3SECTOR keyword
C
C        PROGRAMMER:  Roger Brode
C
C        DATE:       September 10, 2013
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Variable Emmission Rate Factors
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      INTEGER I
      LOGICAL L_BadData

C     Variable Initializations
      MODNAM = 'O3SECTOR'
      L_BadData = .FALSE.

C     Check The Number Of The Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 4) THEN
C        Error Message: Too Few Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 8) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C --- Set L_O3Sector logical variable
      L_O3Sector = .TRUE.

      DO I = 3, IFC
C        Loop through fields for starting directions for each O3SECTOR
         CALL STODBL(FIELD(I),ILEN_FLD,DNUM,IMIT)
C        Check The Numerical Field
         IF (IMIT .EQ. -1) THEN
            WRITE(DUMMY,'("O3SECT",I1)') I-2
            CALL ERRHDL(PATH,MODNAM,'E','208',DUMMY)
C           Assign logical variable for bad data, but cycle through full record
            L_BadData = .TRUE.
            CYCLE
         END IF
         O3SECT(I-2) = DNUM
         IF (O3SECT(I-2) .LT. 0.0D0 .OR. O3SECT(I-2) .GT. 360.0D0) THEN
C           Sector value out-of-range
            IF (O3SECT(I-2) .GT. 9999.0D0) THEN
               WRITE(DUMMY,'("O3SECT>9999.")')
            ELSE IF (O3SECT(I-2) .LT. -999.0D0) THEN
               WRITE(DUMMY,'("O3SECT<-999.")')
            ELSE
               WRITE(DUMMY,'("O3SECT=",F5.0)') O3SECT(I-2)
            END IF
            CALL ERRHDL(PATH,MODNAM,'E','380',DUMMY)
         END IF
      END DO

C --- Check for presence of bad sector data
      IF (L_BadData) GO TO 999

C --- Assign variable for number of user-specified background O3 sectors
      NUMO3Sects = IFC-2

C --- Check O3SECTs for proper order and minimum sector widths
      DO I = 1, NUMO3Sects-1
         IF (O3SECT(I+1) .LT. O3SECT(I) ) THEN
C           Sector value out-of-order
            WRITE(DUMMY,'("O3SECT",I1," < #",I1)') I+1, I
            CALL ERRHDL(PATH,MODNAM,'E','222',DUMMY)
         ELSE IF (O3SECT(I+1) .LT. O3SECT(I)+30.0D0 ) THEN
C           Sector width < 30 degrees
            WRITE(DUMMY,'("O3SECT",I1," < 30")') I+1
            CALL ERRHDL(PATH,MODNAM,'E','227',DUMMY)
         ELSE IF (O3SECT(I+1) .LT. O3SECT(I)+60.0D0 ) THEN
C           Sector width < 60 degrees
            WRITE(DUMMY,'("O3SECT",I1," < 60")') I+1
            CALL ERRHDL(PATH,MODNAM,'W','227',DUMMY)
         END IF
      END DO
C --- Now check for width of last sector
      IF ( (O3SECT(1)+360.0D0)-O3SECT(NUMO3Sects) .LT. 30.0D0) THEN
C        Sector width < 30 degrees
         WRITE(DUMMY,'("O3SECT",I1," < 30")') NUMO3Sects
         CALL ERRHDL(PATH,MODNAM,'E','227',DUMMY)
      ELSE IF ( (O3SECT(1)+360.0D0)-O3SECT(NUMO3Sects) .LT. 60.0D0) THEN
C        Sector width < 60 degrees
         WRITE(DUMMY,'("O3SECT",I1," < 60")') NUMO3Sects
         CALL ERRHDL(PATH,MODNAM,'W','227',DUMMY)
      END IF

 999  RETURN
      END

      SUBROUTINE NOXSECTOR
C***********************************************************************
C                 NOXSECTOR Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes user-specified WD sectors for background
C                 NOX concentrations, based on the NOXSECTR keyword
C
C        PROGRAMMER:  CERC
C
C        DATE:       November 2020
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Variable Emmission Rate Factors
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      INTEGER I
      LOGICAL L_BadData

C     Variable Initializations
      MODNAM = 'NOXSECTOR'
      L_BadData = .FALSE.

C     Check The Number Of The Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 4) THEN
C        Error Message: Too Few Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 8) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C --- Set L_NOXSector logical variable
      L_NOxSector = .TRUE.

      DO I = 3, IFC
C        Loop through fields for starting directions for each NOXSECTOR
         CALL STODBL(FIELD(I),ILEN_FLD,DNUM,IMIT)
C        Check The Numerical Field
         IF (IMIT .EQ. -1) THEN
            WRITE(DUMMY,'("NOXSECT",I1)') I-2
            CALL ERRHDL(PATH,MODNAM,'E','208',DUMMY)
C           Assign logical variable for bad data, but cycle through full record
            L_BadData = .TRUE.
            CYCLE
         END IF
         NOXSECT(I-2) = DNUM
         IF(NOXSECT(I-2) .LT. 0.0D0 .OR. NOXSECT(I-2) .GT. 360.0D0)THEN
C           Sector value out-of-range
            IF (NOXSECT(I-2) .GT. 999.0D0) THEN
               WRITE(DUMMY,'("NOXSECT>999.")')
            ELSE IF (NOXSECT(I-2) .LT. -99.0D0) THEN
               WRITE(DUMMY,'("NOXSECT<-99.")')
            ELSE
               WRITE(DUMMY,'("NOXSECT=",F4.0)') NOXSECT(I-2)
            END IF
            CALL ERRHDL(PATH,MODNAM,'E','380',DUMMY)
         END IF
      END DO

C --- Check for presence of bad sector data
      IF (L_BadData) GO TO 999

C --- Assign variable for number of user-specified background NOX sectors
      NUMNOxSects = IFC-2

C --- Check NOxSECTs for proper order and minimum sector widths
      DO I = 1, NUMNOXSects-1
         IF (NOXSECT(I+1) .LT. NOXSECT(I) ) THEN
C           Sector value out-of-order
            WRITE(DUMMY,'("NOXSCT",I1," < #",I1)') I+1, I
            CALL ERRHDL(PATH,MODNAM,'E','222',DUMMY)
         ELSE IF (NOXSECT(I+1) .LT. NOXSECT(I)+30.0D0 ) THEN
C           Sector width < 30 degrees
            WRITE(DUMMY,'("NOXSCT",I1," < 30")') I+1
            CALL ERRHDL(PATH,MODNAM,'E','227',DUMMY)
         ELSE IF (NOXSECT(I+1) .LT. NOXSECT(I)+60.0D0 ) THEN
C           Sector width < 60 degrees
            WRITE(DUMMY,'("NOXSCT",I1," < 60")') I+1
            CALL ERRHDL(PATH,MODNAM,'W','227',DUMMY)
         END IF
      END DO
! --- Now check for width of last sector
      IF ( (NOXSECT(1)+360.0D0)-NOXSECT(NUMNOxSects) .LT. 30.0D0) THEN
C        Sector width < 30 degrees
         WRITE(DUMMY,'("NOXSCT",I1," < 30")') NUMNOxSects
         CALL ERRHDL(PATH,MODNAM,'E','227',DUMMY)
      ELSE IF ((NOXSECT(1)+360.0D0)-NOXSECT(NUMNOxSects).LT.60.0D0) THEN
C        Sector width < 60 degrees
         WRITE(DUMMY,'("NOXSCT",I1," < 60")') NUMNOxSects
         CALL ERRHDL(PATH,MODNAM,'W','227',DUMMY)
      END IF

 999  RETURN
      END SUBROUTINE NOXSECTOR

      SUBROUTINE LOW_WND
C***********************************************************************
C                 LOW_WND Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes user inputs for LowWind option parameters
C                 under the LOW_WIND keyword
C
C        PROGRAMMER: Roger W. Brode
C
C        DATE:    December 10, 2012
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS:
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      LOGICAL  L_Error
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'LOW_WIND'
      L_Error = .FALSE.

C     Check The Number Of The Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 3) THEN
C        Error Message: Too Few Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
C     Wood 3/18/2022 D127 Increased number of parameters by one to allow for FRANMIN
CCRT  4/11/2022 D131 Increased number of parameters by one to allow for PBAL FRAN option
      ELSE IF (IFC .GT. 9) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Get Minimum Sigma_V value, SVMIN
      CALL STODBL(FIELD(3),ILEN_FLD,DNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         L_Error = .TRUE.
      ELSE
         L_UserSVmin = .TRUE.
         SVMIN = DNUM
      END IF

C     Check for acceptable range for SVMIN
      IF (.NOT. L_Error) THEN
         IF (SVMIN .LT. 0.01D0 .OR. SVMIN .GT. 1.001D0) THEN
            WRITE(DUMMY,'("SVMIN=",F5.2)') SVMIN
            CALL ERRHDL(PATH,MODNAM,'E','380',DUMMY)
            L_Error = .TRUE.
         ELSE
C           Issue warning message with new SVMIN
            WRITE(DUMMY,'(F6.4)') SVMIN
            CALL ERRHDL(PATH,MODNAM,'W','111',DUMMY)
         END IF
      END IF

      L_Error = .FALSE.

      IF (IFC .GE. 4) THEN
C        Get Minimum wind speed value, WSMIN
         CALL STODBL(FIELD(4),ILEN_FLD,DNUM,IMIT)
C        Check The Numerical Field
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            L_Error = .TRUE.
         ELSE
            L_UserWSmin = .TRUE.
            WSMIN = DNUM
         END IF
C        Check for acceptable range for WSMIN
         IF (.NOT. L_Error) THEN
            IF (WSMIN .LT. 0.01D0 .OR. WSMIN .GT. 1.001D0) THEN
               WRITE(DUMMY,'("WSMIN=",F5.2)') WSMIN
               CALL ERRHDL(PATH,MODNAM,'E','380',DUMMY)
            ELSE
C              Issue warning message with new WSMIN
               WRITE(DUMMY,'(F6.4)') WSMIN
               CALL ERRHDL(PATH,MODNAM,'W','112',DUMMY)
            END IF
         END IF
      END IF

      L_Error = .FALSE.

      IF (IFC .GE. 5) THEN
C        Get maximum meander factor, FRANMAX
         CALL STODBL(FIELD(5),ILEN_FLD,DNUM,IMIT)
C        Check The Numerical Field
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            L_Error = .TRUE.
         ELSE
            FRANMAX = DNUM
            L_UserFRANmax = .TRUE.
         END IF
C        Check for acceptable range for FRANMAX
         IF (.NOT. L_Error) THEN
            IF (FRANMAX .LT. 0.0D0 .OR. FRANMAX .GT. 1.0D0) THEN
               WRITE(DUMMY,'("FRANMAX=",F4.2)') FRANMAX
               CALL ERRHDL(PATH,MODNAM,'E','380',DUMMY)
            ELSE
C              Issue warning message with new FRANMAX
               WRITE(DUMMY,'(F6.4)') FRANMAX
               CALL ERRHDL(PATH,MODNAM,'W','113',DUMMY)
            END IF
         END IF
      END IF

C CRT 9/11/2020, D062 User Minimum Sigma W
      IF (IFC .GE. 6) THEN
C        Get minimum sigma w, SWMIN
         CALL STODBL(FIELD(6),ILEN_FLD,DNUM,IMIT)
C        Check The Numerical Field
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            L_Error = .TRUE.
         ELSE
            L_UserSWmin = .TRUE.
            SWMIN = DNUM
         END IF
C        Check for acceptable range for SWMIN
         IF (.NOT. L_Error) THEN
            IF (SWMIN .LT. 0.0D0 .OR. SWMIN .GT. 3.0D0) THEN
               WRITE(DUMMY,'("SWMIN=",F4.2)') SWMIN
               CALL ERRHDL(PATH,MODNAM,'E','380',DUMMY)
            ELSE
C              Issue warning message with new SWMIN
               WRITE(DUMMY,'(F6.4)') SWMIN
               CALL ERRHDL(PATH,MODNAM,'W','127',DUMMY)
            END IF
         END IF
      END IF


C RCO 9/28/2020, D061 User BIGT
      IF (IFC .GE. 7) THEN
C        Get BIGT
         CALL STODBL(FIELD(7),ILEN_FLD,DNUM,IMIT)
C        Check The Numerical Field
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            L_Error = .TRUE.
         ELSE
            L_UserBigT = .TRUE.
            BIGT = DNUM
         END IF
C        Check for acceptable range for BIGT
         IF (.NOT. L_Error) THEN
            IF (BIGT .LT. 0.5D0 .OR. BIGT .GT. 48.0D0) THEN
               WRITE(DUMMY,'("BIGT=",F4.2)') BIGT
               CALL ERRHDL(PATH,MODNAM,'E','380',DUMMY)
            ELSE
C              Issue warning message with new BIGT
               WRITE(DUMMY,'(F6.2)') BIGT
               CALL ERRHDL(PATH,MODNAM,'W','129',DUMMY)
            END IF
         END IF
      END IF

C     Wood 3/18/22 D127 Added FRANMIN
      IF (IFC .GE. 8) THEN
C        Get maximum meander factor, FRANMIN
         CALL STODBL(FIELD(8),ILEN_FLD,DNUM,IMIT)
C        Check The Numerical Field
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            L_Error = .TRUE.
         ELSE
            L_UserFRANmin = .TRUE.
            FRANMIN = DNUM
         END IF
C        Check for acceptable range for FRANMIN and less than FRANMAX
         IF (.NOT. L_Error) THEN
C           FRANMIN cannot be < 0 or > 100
            IF (FRANMIN .LT. 0.0D0 .OR. FRANMIN .GT. 1.0D0) THEN
               WRITE(DUMMY,'("FRANMIN=",F4.2)') FRANMIN
               CALL ERRHDL(PATH,MODNAM,'E','380',DUMMY)
C           FRANMIN cannot be > FRANMAX
            ELSE IF (FRANMIN .GT. FRANMAX) THEN
               WRITE(DUMMY,'(F4.2," > ",F4.2)') FRANMIN, FRANMAX
               CALL ERRHDL(PATH,MODNAM,'E','426',DUMMY)
C              Issue warning message with new FRANMIN
               WRITE(DUMMY,'(F6.4)') FRANMIN
               CALL ERRHDL(PATH,MODNAM,'W','117',DUMMY)
            END IF
         END IF
      END IF

CCRT  4/11/2022, D131 FRAN Alpha Formulation - Momentum Balance (PBal)
      IF (IFC .GE. 9) THEN
C        Get PBal - momentum balance FRAN option
         IF (FIELD(9) .EQ. 'PBAL' .OR.
     &        FIELD(9) .EQ. 'PBALANCE') THEN
            L_PBal = .TRUE.
C           Issue warning message with new BIGT
            CALL ERRHDL(PATH,MODNAM,'W','128','')
         ELSE
            L_PBal = .FALSE.

C ---       Write Error Message:Illegal Parameter Field
            Dummy = FIELD(9)
            CALL ERRHDL(PATH,MODNAM,'E','203',Dummy)
            L_Error = .TRUE.
         END IF
      END IF


 999  RETURN
      END

      SUBROUTINE AWMA_DOWNWASH
C***********************************************************************
C                PRIME_2 Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Informs AERMOD to use the AWMA downwash algorithms:
C
C        OPTIONS:
C           STREAMLINE, STREAMLINED - structure type
C           AWMAUEFF - height to calculate main plume concentration
C           AWMAUTURB - alternate calculations for wake_u, wake_turb
C
C        PROGRAMMER: Wood
C
C        DATE:    August 15, 2018
C
C        MODIFIED:  Added the AWMA option AWMAENTRAIN to change the
C                   entrainment constants beta0 and betap from 0.6 to
C                   0.35. Requires inclusion of ALPHA MODELOPT (July 2020)
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Logical indicating type of structure
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'AWMA_DOWNWASH'

C --- Initialize logical variables
C     By default, all buildings are assumed to be rectangular
C       (L_RECT_BLDG = .TRUE.); if streamlined buildings is specified
C       (with the parameter 'STREAMLINE' or 'STREAMLINED'),
C       L_RECT_BLDG will be set to .FALSE.

C     L_AWMADW indicates that the AWMADW keyword is being used in the
C       control file

      L_AWMADW       = .FALSE.
      L_RECT_BLDG    = .TRUE.        ! Default is rectangular structures
      L_STRMLN_BLDG  = .FALSE.
      L_AWMA_Ueff  = .FALSE.
      L_AWMA_UTurb = .FALSE.
      L_AWMA_Entrain = .FALSE.
      L_AWMA_UTurbHX = .FALSE.

C     Check The Number Of The Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
CCRT  CRT 2/2/2021: D059 Update max number of fields allowed for new options
CCRT  Number increased from 5 to 7 for two newest options (AWMAUTurbHX and AWMAEntrain).
CCRT  If both AWMAUTurb and AWMAUTurbHX are specified, warning message is output and
CCRT  AWMAUTurbHX is used (overrides AWMAUTurb).
CCRT      ELSE IF (IFC .GT. 5) THEN
      ELSE IF (IFC .GT. 7) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

      L_AWMADW = .TRUE.

      IF (IFC .EQ. 3) THEN
C ---    Only one parameter specified
         IF (FIELD(3) .EQ. 'STREAMLINE' .OR.
     &       FIELD(3) .EQ. 'STREAMLINED') THEN
C ---       Process all structures as streamlined buildings
            L_STRMLN_BLDG = .TRUE.
            L_RECT_BLDG = .FALSE.
         ELSE IF (FIELD(3) .EQ. 'AWMAUEFF') THEN
            L_AWMA_Ueff = .TRUE.
         ELSE IF (FIELD(3) .EQ. 'AWMAUTURB') THEN
            L_AWMA_UTurb = .TRUE.
         ELSE IF (FIELD(3) .EQ. 'AWMAENTRAIN') THEN
CRLP        Change beta0 and betap from 0.60 t0 0.35
            L_AWMA_Entrain = .TRUE.
         ELSE IF (FIELD(3) .EQ. 'AWMAUTURBHX') THEN
CRLP        In WAKE_TURB, get plume rise from trajectory arrays
            L_AWMA_UTurbHX = .TRUE.
         ELSE
C           WRITE Error Message  ! Invalid Parameter
            CALL ERRHDL(PATH,MODNAM,'E','203',FIELD(3))
         END IF
      END IF

      IF (IFC .EQ. 4) THEN
C ---    Two parameters specified
         IF (FIELD(3) .EQ. 'STREAMLINE' .OR.
     &       FIELD(3) .EQ. 'STREAMLINED') THEN
C ---       Process all structures as streamlined buildings
            L_STRMLN_BLDG = .TRUE.
            L_RECT_BLDG = .FALSE.
         ELSE IF (FIELD(3) .EQ. 'AWMAUEFF') THEN
            L_AWMA_Ueff = .TRUE.
         ELSE IF (FIELD(3) .EQ. 'AWMAUTURB') THEN
            L_AWMA_UTurb = .TRUE.
         ELSE IF (FIELD(3) .EQ. 'AWMAENTRAIN') THEN
CRLP        Change beta0 and betap from 0.60 t0 0.35
            L_AWMA_Entrain = .TRUE.
         ELSE IF (FIELD(3) .EQ. 'AWMAUTURBHX') THEN
CRLP        In WAKE_TURB, get plume rise from trajectory arrays
            L_AWMA_UTurbHX = .TRUE.
         ELSE
C           WRITE Error Message  ! Invalid Parameter
            CALL ERRHDL(PATH,MODNAM,'E','203',FIELD(3))
         END IF

         IF (FIELD(4) .EQ. 'STREAMLINE' .OR.
     &       FIELD(4) .EQ. 'STREAMLINED') THEN
C ---       Process all structures as streamlined buildings
            L_STRMLN_BLDG = .TRUE.
            L_RECT_BLDG = .FALSE.
         ELSE IF (FIELD(4) .EQ. 'AWMAUEFF') THEN
            L_AWMA_Ueff = .TRUE.
         ELSE IF (FIELD(4) .EQ. 'AWMAUTURB') THEN
            L_AWMA_UTurb = .TRUE.
         ELSE IF (FIELD(4) .EQ. 'AWMAENTRAIN') THEN
CRLP        Change beta0 and betap from 0.60 t0 0.35
            L_AWMA_Entrain = .TRUE.
         ELSE IF (FIELD(4) .EQ. 'AWMAUTURBHX') THEN
CRLP        In WAKE_TURB, get plume rise from trajectory arrays
            L_AWMA_UTurbHX = .TRUE.
         ELSE
C           WRITE Error Message  ! Invalid Parameter
            CALL ERRHDL(PATH,MODNAM,'E','203',FIELD(4))
         END IF

C ---    Check for duplicate parameters
         IF (FIELD(3) .EQ. FIELD(4)) THEN
C           WRITE Error Message    ! Duplicate Option on KEYWORD
            CALL ERRHDL(PATH,MODNAM,'E','121',KEYWRD)
         END IF
      END IF

      IF (IFC .EQ. 5) THEN
C        Three parameters specified
         IF (FIELD(3) .EQ. 'STREAMLINE' .OR.
     &       FIELD(3) .EQ. 'STREAMLINED') THEN
C ---       Process all structures as streamlined buildings
            L_STRMLN_BLDG = .TRUE.
            L_RECT_BLDG = .FALSE.
         ELSE IF (FIELD(3) .EQ. 'AWMAUEFF') THEN
            L_AWMA_Ueff = .TRUE.
         ELSE IF (FIELD(3) .EQ. 'AWMAUTURB') THEN
            L_AWMA_UTurb = .TRUE.
         ELSE IF (FIELD(3) .EQ. 'AWMAENTRAIN') THEN
CRLP        Change beta0 and betap from 0.60 t0 0.35
            L_AWMA_Entrain = .TRUE.
         ELSE IF (FIELD(3) .EQ. 'AWMAUTURBHX') THEN
CRLP        In WAKE_TURB, get plume rise from trajectory arrays
            L_AWMA_UTurbHX = .TRUE.
         ELSE
C           WRITE Error Message  ! Invalid Parameter
            CALL ERRHDL(PATH,MODNAM,'E','203',FIELD(3))
         END IF

         IF (FIELD(4) .EQ. 'STREAMLINE' .OR.
     &       FIELD(4) .EQ. 'STREAMLINED') THEN
C ---       Process all structures as streamlined buildings
            L_STRMLN_BLDG = .TRUE.
            L_RECT_BLDG = .FALSE.
         ELSE IF (FIELD(4) .EQ. 'AWMAUEFF') THEN
            L_AWMA_Ueff = .TRUE.
         ELSE IF (FIELD(4) .EQ. 'AWMAUTURB') THEN
            L_AWMA_UTurb = .TRUE.
         ELSE IF (FIELD(4) .EQ. 'AWMAENTRAIN') THEN
CRLP        Change beta0 and betap from 0.60 t0 0.35
            L_AWMA_Entrain = .TRUE.
         ELSE IF (FIELD(4) .EQ. 'AWMAUTURBHX') THEN
CRLP        In WAKE_TURB, get plume rise from trajectory arrays
            L_AWMA_UTurbHX = .TRUE.
         ELSE
C           WRITE Error Message  ! Invalid Parameter
            CALL ERRHDL(PATH,MODNAM,'E','203',FIELD(4))
         END IF

         IF (FIELD(5) .EQ. 'STREAMLINE' .OR.
     &       FIELD(5) .EQ. 'STREAMLINED') THEN
C ---       Process all structures as streamlined buildings
            L_STRMLN_BLDG = .TRUE.
            L_RECT_BLDG = .FALSE.
         ELSE IF (FIELD(5) .EQ. 'AWMAUEFF') THEN
            L_AWMA_Ueff = .TRUE.
         ELSE IF (FIELD(5) .EQ. 'AWMAUTURB') THEN
            L_AWMA_UTurb = .TRUE.
         ELSE IF (FIELD(5) .EQ. 'AWMAENTRAIN') THEN
CRLP        Change beta0 and betap from 0.60 t0 0.35
            L_AWMA_Entrain = .TRUE.
         ELSE IF (FIELD(5) .EQ. 'AWMAUTURBHX') THEN
CRLP        In WAKE_TURB, get plume rise from trajectory arrays
            L_AWMA_UTurbHX = .TRUE.
         ELSE
C           WRITE Error Message  ! Invalid Parameter
            CALL ERRHDL(PATH,MODNAM,'E','203',FIELD(5))
         END IF

C ---    Check for duplicate parameters
         IF (FIELD(3) .EQ. FIELD(4)) THEN
C           WRITE Error Message    ! Duplicate Option on KEYWORD
            CALL ERRHDL(PATH,MODNAM,'E','121',KEYWRD)
         END IF

         IF (FIELD(3) .EQ. FIELD(5)) THEN
C           WRITE Error Message    ! Duplicate Option on KEYWORD
            CALL ERRHDL(PATH,MODNAM,'E','121',KEYWRD)
         END IF

         IF (FIELD(4) .EQ. FIELD(5)) THEN
C           WRITE Error Message    ! Duplicate Option on KEYWORD
            CALL ERRHDL(PATH,MODNAM,'E','121',KEYWRD)
         END IF
      ENDIF

CCRT  2/2/2021: D059 - AWMA downwash alpha options AWMAEntrain and AWMAUTurbHX
CRLP  Addition of options requires checking for 5 possible
CCRT  Number of fields increased from 5 to 7 for two newest options
CCRT  (AWMAUTurbHX and AWMAEntrain). If both AWMAUTurb and AWMAUTurbHX are
CCRT  specified, warning message is output and AWMAUTurbHX is used.
      IF (IFC .EQ. 6) THEN

C        Four parameters specified
         IF (FIELD(3) .EQ. 'STREAMLINE' .OR.
     &       FIELD(3) .EQ. 'STREAMLINED') THEN
C ---       Process all structures as streamlined buildings
            L_STRMLN_BLDG = .TRUE.
            L_RECT_BLDG = .FALSE.
         ELSE IF (FIELD(3) .EQ. 'AWMAUEFF') THEN
            L_AWMA_Ueff = .TRUE.
         ELSE IF (FIELD(3) .EQ. 'AWMAUTURB') THEN
            L_AWMA_UTurb = .TRUE.
         ELSE IF (FIELD(3) .EQ. 'AWMAENTRAIN') THEN
CRLP        Change beta0 and betap from 0.60 t0 0.35
            L_AWMA_Entrain = .TRUE.
         ELSE IF (FIELD(3) .EQ. 'AWMAUTURBHX') THEN
CRLP        In WAKE_TURB, get plume rise from trajectory arrays
            L_AWMA_UTurbHX = .TRUE.
         ELSE
C           WRITE Error Message  ! Invalid Parameter
            CALL ERRHDL(PATH,MODNAM,'E','203',FIELD(3))
         END IF

         IF (FIELD(4) .EQ. 'STREAMLINE' .OR.
     &       FIELD(4) .EQ. 'STREAMLINED') THEN
C ---       Process all structures as streamlined buildings
            L_STRMLN_BLDG = .TRUE.
            L_RECT_BLDG = .FALSE.
         ELSE IF (FIELD(4) .EQ. 'AWMAUEFF') THEN
            L_AWMA_Ueff = .TRUE.
         ELSE IF (FIELD(4) .EQ. 'AWMAUTURB') THEN
            L_AWMA_UTurb = .TRUE.
         ELSE IF (FIELD(4) .EQ. 'AWMAENTRAIN') THEN
CRLP        Change beta0 and betap from 0.60 t0 0.35
            L_AWMA_Entrain = .TRUE.
         ELSE IF (FIELD(4) .EQ. 'AWMAUTURBHX') THEN
CRLP        In WAKE_TURB, get plume rise from trajectory arrays
            L_AWMA_UTurbHX = .TRUE.
         ELSE
C           WRITE Error Message  ! Invalid Parameter
            CALL ERRHDL(PATH,MODNAM,'E','203',FIELD(4))
         END IF

         IF (FIELD(5) .EQ. 'STREAMLINE' .OR.
     &       FIELD(5) .EQ. 'STREAMLINED') THEN
C ---       Process all structures as streamlined buildings
            L_STRMLN_BLDG = .TRUE.
            L_RECT_BLDG = .FALSE.
         ELSE IF (FIELD(5) .EQ. 'AWMAUEFF') THEN
            L_AWMA_Ueff = .TRUE.
         ELSE IF (FIELD(5) .EQ. 'AWMAUTURB') THEN
            L_AWMA_UTurb = .TRUE.
         ELSE IF (FIELD(5) .EQ. 'AWMAENTRAIN') THEN
CRLP        Change beta0 and betap from 0.60 t0 0.35
            L_AWMA_Entrain = .TRUE.
         ELSE IF (FIELD(5) .EQ. 'AWMAUTURBHX') THEN
CRLP        In WAKE_TURB, get plume rise from trajectory arrays
            L_AWMA_UTurbHX = .TRUE.
         ELSE
C           WRITE Error Message  ! Invalid Parameter
            CALL ERRHDL(PATH,MODNAM,'E','203',FIELD(5))
         END IF

         IF (FIELD(6) .EQ. 'STREAMLINE' .OR.
     &       FIELD(6) .EQ. 'STREAMLINED') THEN
C ---       Process all structures as streamlined buildings
            L_STRMLN_BLDG = .TRUE.
            L_RECT_BLDG = .FALSE.
         ELSE IF (FIELD(6) .EQ. 'AWMAUEFF') THEN
            L_AWMA_Ueff = .TRUE.
         ELSE IF (FIELD(6) .EQ. 'AWMAUTURB') THEN
            L_AWMA_UTurb = .TRUE.
         ELSE IF (FIELD(6) .EQ. 'AWMAENTRAIN') THEN
CRLP        Change beta0 and betap from 0.60 t0 0.35
            L_AWMA_Entrain = .TRUE.
         ELSE IF (FIELD(6) .EQ. 'AWMAUTURBHX') THEN
CRLP        In WAKE_TURB, get plume rise from trajectory arrays
            L_AWMA_UTurbHX = .TRUE.
         ELSE
C           WRITE Error Message  ! Invalid Parameter
            CALL ERRHDL(PATH,MODNAM,'E','203',FIELD(5))
         END IF

C ---    Check for duplicate parameters
C        Additional checks added with the addition of the AWMAEntrain
C         downwash option
         IF (FIELD(3) .EQ. FIELD(4)) THEN
C           WRITE Error Message    ! Duplicate Option on KEYWORD
            CALL ERRHDL(PATH,MODNAM,'E','121',KEYWRD)
         END IF

         IF (FIELD(3) .EQ. FIELD(5)) THEN
C           WRITE Error Message    ! Duplicate Option on KEYWORD
            CALL ERRHDL(PATH,MODNAM,'E','121',KEYWRD)
         END IF

         IF (FIELD(3) .EQ. FIELD(6)) THEN
C           WRITE Error Message    ! Duplicate Option on KEYWORD
            CALL ERRHDL(PATH,MODNAM,'E','121',KEYWRD)
         END IF

         IF (FIELD(4) .EQ. FIELD(5)) THEN
C           WRITE Error Message    ! Duplicate Option on KEYWORD
            CALL ERRHDL(PATH,MODNAM,'E','121',KEYWRD)
         END IF

         IF (FIELD(4) .EQ. FIELD(6)) THEN
C           WRITE Error Message    ! Duplicate Option on KEYWORD
            CALL ERRHDL(PATH,MODNAM,'E','121',KEYWRD)
         END IF

         IF (FIELD(5) .EQ. FIELD(6)) THEN
C           WRITE Error Message    ! Duplicate Option on KEYWORD
            CALL ERRHDL(PATH,MODNAM,'E','121',KEYWRD)
         END IF

      ENDIF

CCRT  2/2/2021: D059 - AWMA downwash alpha options AWMAEntrain and AWMAUTurbHX
CRLP  Addition of options requires checking for 5 possible
CCRT  Number of fields increased from 5 to 7 for two newest options
CCRT  (AWMAUTurbHX and AWMAEntrain). If both AWMAUTurb and AWMAUTurbHX are
CCRT  specified, warning message is output and AWMAUTurbHX is used.
      IF (IFC .EQ. 7) THEN
C        Five, i.e. all, parameters specified
         IF (FIELD(3) .EQ. 'STREAMLINE' .OR.
     &       FIELD(3) .EQ. 'STREAMLINED') THEN
C ---       Process all structures as streamlined buildings
            L_STRMLN_BLDG = .TRUE.
            L_RECT_BLDG = .FALSE.
         ELSE IF (FIELD(3) .EQ. 'AWMAUEFF') THEN
            L_AWMA_Ueff = .TRUE.
         ELSE IF (FIELD(3) .EQ. 'AWMAUTURB') THEN
            L_AWMA_UTurb = .TRUE.
         ELSE IF (FIELD(3) .EQ. 'AWMAENTRAIN') THEN
CRLP        Change beta0 and betap from 0.60 t0 0.35
            L_AWMA_Entrain = .TRUE.
         ELSE IF (FIELD(3) .EQ. 'AWMAUTURBHX') THEN
CRLP        In WAKE_TURB, get plume rise from trajectory arrays
            L_AWMA_UTurbHX = .TRUE.
         ELSE
C           WRITE Error Message  ! Invalid Parameter
            CALL ERRHDL(PATH,MODNAM,'E','203',FIELD(3))
         END IF

         IF (FIELD(4) .EQ. 'STREAMLINE' .OR.
     &       FIELD(4) .EQ. 'STREAMLINED') THEN
C ---       Process all structures as streamlined buildings
            L_STRMLN_BLDG = .TRUE.
            L_RECT_BLDG = .FALSE.
         ELSE IF (FIELD(4) .EQ. 'AWMAUEFF') THEN
            L_AWMA_Ueff = .TRUE.
         ELSE IF (FIELD(4) .EQ. 'AWMAUTURB') THEN
            L_AWMA_UTurb = .TRUE.
         ELSE IF (FIELD(4) .EQ. 'AWMAENTRAIN') THEN
CRLP        Change beta0 and betap from 0.60 t0 0.35
            L_AWMA_Entrain = .TRUE.
         ELSE IF (FIELD(4) .EQ. 'AWMAUTURBHX') THEN
CRLP        In WAKE_TURB, get plume rise from trajectory arrays
            L_AWMA_UTurbHX = .TRUE.
         ELSE
C           WRITE Error Message  ! Invalid Parameter
            CALL ERRHDL(PATH,MODNAM,'E','203',FIELD(4))
         END IF

         IF (FIELD(5) .EQ. 'STREAMLINE' .OR.
     &       FIELD(5) .EQ. 'STREAMLINED') THEN
C ---       Process all structures as streamlined buildings
            L_STRMLN_BLDG = .TRUE.
            L_RECT_BLDG = .FALSE.
         ELSE IF (FIELD(5) .EQ. 'AWMAUEFF') THEN
            L_AWMA_Ueff = .TRUE.
         ELSE IF (FIELD(5) .EQ. 'AWMAUTURB') THEN
            L_AWMA_UTurb = .TRUE.
         ELSE IF (FIELD(5) .EQ. 'AWMAENTRAIN') THEN
CRLP        Change beta0 and betap from 0.60 t0 0.35
            L_AWMA_Entrain = .TRUE.
         ELSE IF (FIELD(5) .EQ. 'AWMAUTURBHX') THEN
CRLP        In WAKE_TURB, get plume rise from trajectory arrays
            L_AWMA_UTurbHX = .TRUE.
         ELSE
C           WRITE Error Message  ! Invalid Parameter
            CALL ERRHDL(PATH,MODNAM,'E','203',FIELD(5))
         END IF

         IF (FIELD(6) .EQ. 'STREAMLINE' .OR.
     &       FIELD(6) .EQ. 'STREAMLINED') THEN
C ---       Process all structures as streamlined buildings
            L_STRMLN_BLDG = .TRUE.
            L_RECT_BLDG = .FALSE.
         ELSE IF (FIELD(6) .EQ. 'AWMAUEFF') THEN
            L_AWMA_Ueff = .TRUE.
         ELSE IF (FIELD(6) .EQ. 'AWMAUTURB') THEN
            L_AWMA_UTurb = .TRUE.
         ELSE IF (FIELD(6) .EQ. 'AWMAENTRAIN') THEN
CRLP        Change beta0 and betap from 0.60 t0 0.35
            L_AWMA_Entrain = .TRUE.
         ELSE IF (FIELD(6) .EQ. 'AWMAUTURBHX') THEN
CRLP        In WAKE_TURB, get plume rise from trajectory arrays
            L_AWMA_UTurbHX = .TRUE.
         ELSE
C           WRITE Error Message  ! Invalid Parameter
            CALL ERRHDL(PATH,MODNAM,'E','203',FIELD(5))
         END IF

         IF (FIELD(7) .EQ. 'STREAMLINE' .OR.
     &       FIELD(7) .EQ. 'STREAMLINED') THEN
C ---       Process all structures as streamlined buildings
            L_STRMLN_BLDG = .TRUE.
            L_RECT_BLDG = .FALSE.
         ELSE IF (FIELD(7) .EQ. 'AWMAUEFF') THEN
            L_AWMA_Ueff = .TRUE.
         ELSE IF (FIELD(7) .EQ. 'AWMAUTURB') THEN
            L_AWMA_UTurb = .TRUE.
         ELSE IF (FIELD(7) .EQ. 'AWMAENTRAIN') THEN
CRLP        Change beta0 and betap from 0.60 t0 0.35
            L_AWMA_Entrain = .TRUE.
         ELSE IF (FIELD(7) .EQ. 'AWMAUTURBHX') THEN
CRLP        In WAKE_TURB, get plume rise from trajectory arrays
            L_AWMA_UTurbHX = .TRUE.
         ELSE
C           WRITE Error Message  ! Invalid Parameter
            CALL ERRHDL(PATH,MODNAM,'E','203',FIELD(5))
         END IF

C ---    Check for duplicate parameters
C        Additional checks added with the addition of the AWMAUturbHX
C         downwash option
         IF (FIELD(3) .EQ. FIELD(4)) THEN
C           WRITE Error Message    ! Duplicate Option on KEYWORD
            CALL ERRHDL(PATH,MODNAM,'E','121',KEYWRD)
         END IF

         IF (FIELD(3) .EQ. FIELD(5)) THEN
C           WRITE Error Message    ! Duplicate Option on KEYWORD
            CALL ERRHDL(PATH,MODNAM,'E','121',KEYWRD)
         END IF

         IF (FIELD(3) .EQ. FIELD(6)) THEN
C           WRITE Error Message    ! Duplicate Option on KEYWORD
            CALL ERRHDL(PATH,MODNAM,'E','121',KEYWRD)
         END IF

         IF (FIELD(3) .EQ. FIELD(7)) THEN
C           WRITE Error Message    ! Duplicate Option on KEYWORD
            CALL ERRHDL(PATH,MODNAM,'E','121',KEYWRD)
         END IF

         IF (FIELD(4) .EQ. FIELD(5)) THEN
C           WRITE Error Message    ! Duplicate Option on KEYWORD
            CALL ERRHDL(PATH,MODNAM,'E','121',KEYWRD)
         END IF

         IF (FIELD(4) .EQ. FIELD(6)) THEN
C           WRITE Error Message    ! Duplicate Option on KEYWORD
            CALL ERRHDL(PATH,MODNAM,'E','121',KEYWRD)
         END IF

         IF (FIELD(4) .EQ. FIELD(7)) THEN
C           WRITE Error Message    ! Duplicate Option on KEYWORD
            CALL ERRHDL(PATH,MODNAM,'E','121',KEYWRD)
         END IF

         IF (FIELD(5) .EQ. FIELD(6)) THEN
C           WRITE Error Message    ! Duplicate Option on KEYWORD
            CALL ERRHDL(PATH,MODNAM,'E','121',KEYWRD)
         END IF

         IF (FIELD(5) .EQ. FIELD(7)) THEN
C           WRITE Error Message    ! Duplicate Option on KEYWORD
            CALL ERRHDL(PATH,MODNAM,'E','121',KEYWRD)
         END IF

         IF (FIELD(6) .EQ. FIELD(7)) THEN
C           WRITE Error Message    ! Duplicate Option on KEYWORD
            CALL ERRHDL(PATH,MODNAM,'E','121',KEYWRD)
         END IF

      ENDIF

CCRT  6/14/2019: STREAMLINE option requires AWMAUTURB
CCRT  Issue error if STREAMLINE flag is TRUE and
CCRT  AWMAUTURB flag is FALSE.
CJAT  6/20/2020:  ISSUE D53 ADDED FROM 19191
C     GIVE ERROR CODE NEW NUMBER, 126 TO NOT
C     CONFLICT WITH 125 (PATHS NOT FINISHED)
CCRT  2/2/2021: STREAMLINE should also work with AWMAUTurbHX option
CCRT  Update conditional statement to include L_AWMA_UTurbHX
      IF (L_STRMLN_BLDG .AND.
     &   (.NOT. L_AWMA_UTurb .AND. .NOT. L_AWMA_UTurbHX)) THEN
C        WRITE Error Message    ! AWMADWUTurb option required
!         CALL ERRHDL(PATH,MODNAM,'E','125',KEYWRD)
         CALL ERRHDL(PATH,MODNAM,'E','126',KEYWRD)
      END IF

 999  RETURN
      END

      SUBROUTINE ORD_DOWNWASH
C***********************************************************************
C              ORD_DOWNWASH Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Informs AERMOD to use the PRIME algorithms with
C                 ORD modifications
C
C        OPTIONS:
C           ORDCAV -  improved cavity calculations
C           ORDUEFF - height to calculate main plume concentration
C           ORDTURB - Limit on vertical turbulence intensity
C
C        PROGRAMMER: Wood
C
C        DATE:    August 15, 2018
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Logical indicating type of structure
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'PRIME_ORD'

      L_ORDDW    = .FALSE.
      L_ORD_Cav  = .FALSE.
      L_ORD_Ueff = .FALSE.
      L_ORD_Turb = .FALSE.


C     Check The Number Of The Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 5) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

      L_ORDDW = .TRUE.

C --- Decode the parameters to use in the calculations
      IF (IFC .EQ. 3) THEN
C        Only one parameter specified
         IF (FIELD(3) .EQ. 'ORDCAV') THEN
            L_ORD_Cav  = .TRUE.
         ELSE IF (FIELD(3) .EQ. 'ORDUEFF') THEN
            L_ORD_Ueff = .TRUE.
         ELSE IF (FIELD(3) .EQ. 'ORDTURB') THEN
            L_ORD_Turb = .TRUE.
         ELSE
C ---       WRITE Error Message  ! Invalid Parameter
            CALL ERRHDL(PATH,MODNAM,'E','203',FIELD(3))
         END IF
      END IF

      IF (IFC .EQ. 4) THEN
C        Two parameters specified - check each
         IF (FIELD(3) .EQ. 'ORDCAV') THEN
            L_ORD_Cav  = .TRUE.
         ELSE IF (FIELD(3) .EQ. 'ORDUEFF') THEN
            L_ORD_Ueff = .TRUE.
         ELSE IF (FIELD(3) .EQ. 'ORDTURB') THEN
            L_ORD_Turb = .TRUE.
         ELSE
C ---       WRITE Error Message  ! Invalid Parameter
            CALL ERRHDL(PATH,MODNAM,'E','203',FIELD(3))
         END IF

         IF (FIELD(4) .EQ. 'ORDCAV') THEN
            L_ORD_Cav  = .TRUE.
         ELSE IF (FIELD(4) .EQ. 'ORDUEFF') THEN
            L_ORD_Ueff = .TRUE.
         ELSE IF (FIELD(4) .EQ. 'ORDTURB') THEN
            L_ORD_Turb = .TRUE.
         ELSE
C ---       WRITE Error Message  ! Invalid Parameter
            CALL ERRHDL(PATH,MODNAM,'E','203',FIELD(4))
         END IF

C ---    Check for duplicate parameters
         IF (FIELD(3) .EQ. FIELD(4)) THEN
C           WRITE Error Message    ! Duplicate Option on KEYWORD
            CALL ERRHDL(PATH,MODNAM,'E','121',KEYWRD)
         END IF

      END IF

      IF (IFC .EQ. 5) THEN
C        All three parameters specified
         IF (FIELD(3) .EQ. 'ORDCAV') THEN
            L_ORD_Cav  = .TRUE.
         ELSE IF (FIELD(3) .EQ. 'ORDUEFF') THEN
            L_ORD_Ueff = .TRUE.
         ELSE IF (FIELD(3) .EQ. 'ORDTURB') THEN
            L_ORD_Turb = .TRUE.
         ELSE
C ---       WRITE Error Message  ! Invalid Parameter
            CALL ERRHDL(PATH,MODNAM,'E','203',FIELD(3))
         END IF

         IF (FIELD(4) .EQ. 'ORDCAV') THEN
            L_ORD_Cav  = .TRUE.
         ELSE IF (FIELD(4) .EQ. 'ORDUEFF') THEN
            L_ORD_Ueff = .TRUE.
         ELSE IF (FIELD(4) .EQ. 'ORDTURB') THEN
            L_ORD_Turb = .TRUE.
         ELSE
C ---       WRITE Error Message  ! Invalid Parameter
            CALL ERRHDL(PATH,MODNAM,'E','203',FIELD(4))
         END IF

         IF (FIELD(5) .EQ. 'ORDCAV') THEN
            L_ORD_Cav  = .TRUE.
         ELSE IF (FIELD(5) .EQ. 'ORDUEFF') THEN
            L_ORD_Ueff = .TRUE.
         ELSE IF (FIELD(5) .EQ. 'ORDTURB') THEN
            L_ORD_Turb = .TRUE.
         ELSE
C ---       WRITE Error Message  ! Invalid Parameter
            CALL ERRHDL(PATH,MODNAM,'E','203',FIELD(5))
         END IF

C ---    Check for duplicate parameters
         IF (FIELD(3) .EQ. FIELD(4)) THEN
C           WRITE Error Message    ! Duplicate Option
            CALL ERRHDL(PATH,MODNAM,'E','121',KEYWRD)
         END IF

         IF (FIELD(4) .EQ. FIELD(5)) THEN
C           WRITE Error Message    ! Duplicate Option
            CALL ERRHDL(PATH,MODNAM,'E','121',KEYWRD)
         END IF

         IF (FIELD(3) .EQ. FIELD(5)) THEN
C           WRITE Error Message    ! Duplicate Option
            CALL ERRHDL(PATH,MODNAM,'E','121',KEYWRD)
         END IF

      END IF

 999  RETURN
      END
