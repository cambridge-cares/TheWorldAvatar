      SUBROUTINE MECARD
C***********************************************************************
C                 MECARD Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: To process MEteorology Pathway Card Images
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:       March 2, 1992
C
C        MODIFIED:   To allow the user to specify the number of years of 
C                    meteorological data that are being processed for a 
C                    particular run.  The option is exercised with the 
C                    new NUMYEARS keyword on the ME pathway. The value 
C                    specified on the NUMYEARS keyword is used to allocate 
C                    storage for the arrays that are used in the MAXDCONT 
C                    option, and allows the user to reduce the memory storage 
C                    requirements under the MAXDCONT option when less than 
C                    five (5) years of met data are being used. The default 
C                    number of years used for the MAXDCONT array allocation 
C                    without the NUMYEARS keyword is still 5 years (formerly 
C                    specified by the NYEARS PARAMETER).
C                    R. Brode, US EPA, OAQPS, AQMG, 02/29/2012
C
C        MODIFIED:   To remove support for unformatted meteorological
C                    data files.
C                    R.W. Brode, PES, Inc., 4/10/2000
C
C        MODIFIED:  To Include TOXXFILE Option - 9/29/92
C
C        INPUTS:  Pathway (ME) and Keyword
C
C        OUTPUTS: Meteorology Option Switches
C                 Meteorology Setup Status Switches
C
C        CALLED FROM:   SETUP
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, ND, NDYS

C     Variable Initializations
      MODNAM = 'MECARD'

      IF (KEYWRD .EQ. 'STARTING') THEN
C        Set Status Switch
         IMSTAT(1) = IMSTAT(1) + 1
         IF (IMSTAT(1) .NE. 1) THEN
C           WRITE Error Message: Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         END IF

      ELSE IF (KEYWRD .EQ. 'SURFFILE') THEN
C        Set Status Switch
         IMSTAT(2) = IMSTAT(2) + 1
         IF (IMSTAT(2) .NE. 1) THEN
C           WRITE Error Message: Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Surface Meteorology File Information    ---   CALL SURFIL
            CALL SURFIL
         END IF

      ELSE IF (KEYWRD .EQ. 'PROFFILE') THEN
C        Set Status Switch
         IMSTAT(3) = IMSTAT(3) + 1
         IF (IMSTAT(3) .NE. 1) THEN
C           WRITE Error Message: Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Profile Meteorology File Information    ---   CALL PROFIL
            CALL PROFIL
         END IF

      ELSE IF (KEYWRD .EQ. 'SURFDATA') THEN
C        Set Status Switch
         IMSTAT(4) = IMSTAT(4) + 1
         IF (IMSTAT(4) .NE. 1) THEN
C           WRITE Error Message: Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Surface Data Information                ---   CALL SFDATA
            CALL SFDATA
         END IF

      ELSE IF (KEYWRD .EQ. 'UAIRDATA') THEN
C        Set Status Switch
         IMSTAT(5) = IMSTAT(5) + 1
         IF (IMSTAT(5) .NE. 1) THEN
C           WRITE Error Message: Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Upper Air Data Information              ---   CALL UADATA
            CALL UADATA
         END IF

      ELSE IF (.NOT.EVONLY .AND. KEYWRD .EQ. 'STARTEND') THEN
C        Set Status Switch
         IMSTAT(6) = IMSTAT(6) + 1
         IF (SCIM) THEN
C           Write out error message:  STARTEND cannot be used with SCIM option
            CALL ERRHDL(PATH,MODNAM,'E','154',KEYWRD)
         ELSE
            IF (IMSTAT(6) .NE. 1) THEN
C              WRITE Error Message: Non-repeatable Keyword
               CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
            ELSE
C              Process Start and End Dates for Reading      ---   CALL STAEND
               CALL STAEND
            END IF
         END IF

      ELSE IF (.NOT.EVONLY .AND. KEYWRD .EQ. 'DAYRANGE') THEN
C        Set Status Switch
         IMSTAT(7) = IMSTAT(7) + 1
         IF (SCIM) THEN
C           Write out error message:  DAYRANGE cannot be used with SCIM option
            CALL ERRHDL(PATH,MODNAM,'E','154',KEYWRD)
         ELSE
C           Check for First Occurrence of DAYRANGE Card, and
C           Reinitialize IPROC and IPROCL Arrays to 0's
            IF (IMSTAT(7) .EQ. 1) THEN
               IPROC(:)  = 0
               IPROCL(:) = 0
            END IF
C           Process Days and Day Ranges for Processing      ---   CALL DAYRNG
            CALL DAYRNG
         END IF

      ELSE IF (KEYWRD .EQ. 'WDROTATE') THEN
C        Set Status Switch
         IMSTAT(8) = IMSTAT(8) + 1
         IF (IMSTAT(8) .NE. 1) THEN
C           WRITE Error Message: Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Wind Direction Correction Option        ---   CALL WDROTA
            CALL WDROTA
         END IF

      ELSE IF (KEYWRD .EQ. 'SITEDATA') THEN
C        Set Status Switch
         IMSTAT(9) = IMSTAT(9) + 1
         IF (IMSTAT(9) .NE. 1) THEN
C           WRITE Error Message: Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process On-site Data Information                ---   CALL ONDATA
            CALL ONDATA
         END IF

      ELSE IF (KEYWRD .EQ. 'PROFBASE') THEN
C        Set Status Switch
         IMSTAT(10) = IMSTAT(10) + 1
         IF (IMSTAT(10) .NE. 1) THEN
C           WRITE Error Message: Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process On-site Data Information                ---   CALL PRBASE
            CALL PRBASE
         END IF

      ELSE IF (KEYWRD .EQ. 'WINDCATS') THEN
C        Set Status Switch
         IMSTAT(11) = IMSTAT(11) + 1
         IF (IMSTAT(11) .NE. 1) THEN
C           WRITE Error Message: Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Wind Speed Categories                   ---   CALL WSCATS
            CALL WSCATS
         END IF

      ELSE IF (KEYWRD .EQ. 'SCIMBYHR' .AND. SCIM) THEN
C        Set Status Switch
         IMSTAT(12) = IMSTAT(12) + 1
         IF (IMSTAT(12) .NE. 1) THEN
C           WRITE Error Message: Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Wind Speed Categories                   ---   CALL SCIMIT
            CALL SCIMIT
         END IF

      ELSE IF (KEYWRD .EQ. 'NUMYEARS') THEN
C        Set Status Switch
         IMSTAT(13) = IMSTAT(13) + 1
         IF (IMSTAT(13) .NE. 1) THEN
C           WRITE Error Message: Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Number of Years for MAXDCONT arrays     ---  CALL NUMYR
            CALL NUMYR
      END IF

C     JAT 1/29/21 ISSUE D070 TURBULENCE OPTIONS
C     ADD CHECK FOR ONE OF THE TURBULENCE KEYWORDS
      ELSE IF (KEYWRD .EQ. 'NOTURB  ' .OR. KEYWRD .EQ. 'NOTURBST' .OR.
     &KEYWRD .EQ. 'NOTURBCO' .OR. KEYWRD .EQ. 'NOSA    ' .OR.
     &KEYWRD .EQ. 'NOSW    ' .OR. KEYWRD .EQ. 'NOSAST  ' .OR.
     &KEYWRD .EQ. 'NOSWST  ' .OR. KEYWRD .EQ. 'NOSACO  ' .OR.
     &KEYWRD .EQ. 'NOSWCO  ') THEN
C        Set Status Switch
         IMSTAT(14) = IMSTAT(14) + 1
         IF (IMSTAT(14) .NE. 1) THEN
C           WRITE Error Message: Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process turbulence option     ---  CALL TURBOPTS
            CALL TURBOPT
      END IF
      ELSE IF (KEYWRD .EQ. 'FINISHED') THEN
C        Set Status Switch
         IMSTAT(50) = IMSTAT(50) + 1
         IF (IMSTAT(50) .NE. 1) THEN
C           WRITE Error Message: Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
            GO TO 999
         END IF
C        Write Error Messages for Missing Mandatory Keyword(s)
         IF (IMSTAT(1) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','STARTING')
         END IF
         IF (IMSTAT(2) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','SURFFILE')
         END IF
         IF (IMSTAT(3) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','PROFFILE')
         END IF
         IF (IMSTAT(4) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','SURFDATA')
         END IF
         IF (IMSTAT(5) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','UAIRDATA')
         END IF
         IF (IMSTAT(10) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','PROFBASE')
         END IF
         IF (SCIM .AND. IMSTAT(12) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','SCIMBYHR')
         END IF

C        OPEN Met Data File                                 ---   CALL MEOPEN
         IF (IMSTAT(2) .NE. 0 .AND. IMSTAT(3) .NE. 0) THEN
            CALL MEOPEN
         END IF

C ---    Assign L_LeapYear variable
         IF ((MOD(ISYEAR,4) .NE. 0) .OR.
     &       (MOD(ISYEAR,100) .EQ. 0 .AND. MOD(ISYEAR,400) .NE. 0)) THEN
C           Not a Leap Year
            L_LeapYear = .FALSE.
         ELSE
C           Leap Year
            L_LeapYear = .TRUE.
         END IF

         IF (MULTYR) THEN
C           Set the Increment for Saving Results, INCRST, Based on
C           ISYEAR, Surface Data Year, from SURFDATA Keyword
            IF ((MOD(ISYEAR,4) .NE. 0) .OR.
     &          (MOD(ISYEAR,100).EQ.0 .AND. MOD(ISYEAR,400).NE.0)) THEN
C              Not a Leap Year
               INCRST = 365
            ELSE
C              Leap Year
               INCRST = 366
            END IF
         END IF

C        Determine Number of Hours to be Processed, NHOURS, For Use
C        With the TOXXFILE Option - 9/29/92
         IF ((MOD(ISYEAR,4) .NE. 0) .OR.
     &       (MOD(ISYEAR,100).EQ.0 .AND. MOD(ISYEAR,400).NE.0)) THEN
C           Not a Leap Year
            ND = 365
         ELSE
C           Leap Year
            ND = 366
         END IF
         NDYS = 0
         DO I = 1, ND
C ---       Adjust for leap year vs. non-leap year
            IF (ND .EQ. 365) THEN
               IF (IPROC(I) .EQ. 1) THEN
                  NDYS = NDYS + 1
               END IF
            ELSE IF (ND .EQ. 366) THEN
               IF (IPROCL(I) .EQ. 1) THEN
                  NDYS = NDYS + 1
               END IF
            END IF
         END DO
         NHOURS = NDYS * 24

      ELSE
C        Write Error Message: Invalid Keyword for This Pathway
         CALL ERRHDL(PATH,MODNAM,'E','110',KEYWRD)
      END IF

 999  RETURN
      END

      SUBROUTINE SURFIL
C***********************************************************************
C                 SURFIL Module of AERMOD
C
C        PURPOSE: Process Surface Meteorology Input File Options
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, James Paumier
C
C        DATE:    September 30, 1993
C
C        MODIFIED:  Remove optional field for format; FREE format
C                   is used for all met inputs
C                   R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Meteorological Data Filename and Format
C
C        ERROR HANDLING:   Checks for No Parameters;
C                          Checks for No Format (uses default);
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   MECARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'SURFIL'

      IF (IFC .EQ. 3) THEN
C        Retrieve Met Data Filename as Character Substring to Maintain Case
         IF ((LOCE(3)-LOCB(3)) .LE. (ILEN_FLD - 1) ) THEN
C           Retrieve Filename as Character Substring to Maintain Original Case
C           Also Check for Filename Larger Than ILEN_FLD Characters
            METINP = RUNST1(LOCB(3):LOCE(3))
         ELSE
C           WRITE Error Message:  METINP Field is Too Long
            WRITE(DUMMY,'(I8)') ILEN_FLD
            CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
         END IF
      ELSE IF (IFC .EQ. 4) THEN
C        Retrieve Met Data Filename as Character Substring to Maintain Case
         IF ((LOCE(3)-LOCB(3)) .LE. (ILEN_FLD - 1) ) THEN
C           Retrieve Filename as Character Substring to Maintain Original Case
C           Also Check for Filename Larger Than ILEN_FLD Characters
            METINP = RUNST1(LOCB(3):LOCE(3))
         ELSE
C           WRITE Error Message:  METINP Field is Too Long
            WRITE(DUMMY,'(I8)') ILEN_FLD
            CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
         END IF
         IF (FIELD(4) .NE. 'FREE') THEN
C           WRITE Warning Message         ! Format field no longer used
            CALL ERRHDL(PATH,MODNAM,'W','293','format')
         END IF
      ELSE IF (IFC .GT. 4) THEN
C        WRITE Error Message           ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Error Message           ! No Parameters Specified
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      END IF

      RETURN
      END

      SUBROUTINE PROFIL
C***********************************************************************
C                 PROFIL Module of AERMOD
C
C        PURPOSE: Process Profile Meteorology Input File Options
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, James Paumier
C
C        DATE:    September 30, 1993
C
C        MODIFIED:  Remove optional field for format; FREE format
C                   is used for all met inputs
C                   R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
C
C                   Check length of format string for PROFRM.
C                   R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Meteorological Data Filename and Format
C
C        ERROR HANDLING:   Checks for No Parameters;
C                          Checks for No Format (uses default);
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   MECARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'PROFIL'

      IF (IFC .EQ. 3) THEN
C        Retrieve Met Data Filename as Character Substring to Maintain Case
         IF ((LOCE(3)-LOCB(3)) .LE. (ILEN_FLD - 1) ) THEN
C           Retrieve Filename as Character Substring to Maintain Original Case
C           Also Check for Filename Larger Than ILEN_FLD Characters
            PROINP = RUNST1(LOCB(3):LOCE(3))
         ELSE
C           WRITE Error Message:  PROINP Field is Too Long
            WRITE(DUMMY,'(I8)') ILEN_FLD
            CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
         END IF
      ELSE IF (IFC .EQ. 4) THEN
C        Retrieve Met Data Filename as Character Substring to Maintain Case
         IF ((LOCE(3)-LOCB(3)) .LE. (ILEN_FLD - 1) ) THEN
C           Retrieve Filename as Character Substring to Maintain Original Case
C           Also Check for Filename Larger Than ILEN_FLD Characters
            PROINP = RUNST1(LOCB(3):LOCE(3))
         ELSE
C           WRITE Error Message:  PROINP Field is Too Long
            WRITE(DUMMY,'(I8)') ILEN_FLD
            CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
         END IF
         IF (FIELD(4) .NE. 'FREE') THEN
C           WRITE Warning Message         ! Format field no longer used
            CALL ERRHDL(PATH,MODNAM,'W','293','format')
         END IF
      ELSE IF (IFC .GT. 4) THEN
C        WRITE Error Message           ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Error Message           ! No Parameters Specified
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      END IF

      RETURN
      END

      SUBROUTINE SFDATA
C***********************************************************************
C                 SFDATA Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Process Meteorology Surface Data Station Options
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To incorporate modifications to date processing
C                    for Y2K compliance, including use of date window
C                    variables (ISTRT_WIND and ISTRT_CENT).
C                    R.W. Brode, PES, Inc., 5/12/99
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Meteorological Surface Data Station Identification
C
C        ERROR HANDLING:   Checks for Too Few Parameters;
C                          Checks for Invalid Numeric Fields;
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   MECARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'SFDATA'

      IF (IFC .EQ. 2) THEN
C        WRITE Error Message           ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 4) THEN
C        WRITE Error Message           ! Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 7) THEN
C        WRITE Error Message           ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

      CALL STONUM(FIELD(3),ILEN_FLD,FNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         IDSURF = 0
         GO TO 199
      END IF
      IDSURF = NINT(FNUM)

 199  CALL STONUM(FIELD(4),ILEN_FLD,FNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         ISYEAR = 0
         GO TO 299
      END IF
      ISYEAR = NINT(FNUM)
C     Check for 2-digit Input and Convert ISYEAR to Four Digits
      IF (ISYEAR .GE. ISTRT_WIND .AND. ISYEAR .LE. 99) THEN
         ISYEAR = ISTRT_CENT*100 + ISYEAR
      ELSE IF (ISYEAR .LT. ISTRT_WIND) THEN
         ISYEAR = (ISTRT_CENT+1)*100 + ISYEAR
      END IF

 299  IF (IFC .GE. 5) THEN
C        Retrieve Surface Data Station Name (Optional)
         SFNAME = FIELD(5)
      ELSE
         SFNAME = 'UNKNOWN'
      END IF

      IF (IFC .EQ. 7) THEN
C        Retrieve Coordinates for Surface Data Location (Optional)
         CALL STODBL(FIELD(6),ILEN_FLD,SFX,IMIT)
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         END IF
         CALL STODBL(FIELD(7),ILEN_FLD,SFY,IMIT)
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         END IF
      END IF

 999  RETURN
      END

      SUBROUTINE UADATA
C***********************************************************************
C                 UADATA Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Process Meteorology Upper Air Data Station Options
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To incorporate modifications to date processing
C                    for Y2K compliance, including use of date window
C                    variables (ISTRT_WIND and ISTRT_CENT).
C                    R.W. Brode, PES, Inc., 5/12/99
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Meteorological Upper Air Data Station Identification
C
C        ERROR HANDLING:   Checks for Too Few Parameters;
C                          Checks for Invalid Numeric Fields;
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   MECARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'UADATA'

      IF (IFC .EQ. 2) THEN
C        WRITE Error Message           ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 4) THEN
C        WRITE Error Message           ! Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 7) THEN
C        WRITE Error Message           ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

      CALL STONUM(FIELD(3),ILEN_FLD,FNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         IDUAIR = 0
         GO TO 199
      END IF
      IDUAIR = NINT(FNUM)

 199  CALL STONUM(FIELD(4),ILEN_FLD,FNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         IUYEAR = 0
         GO TO 299
      END IF
      IUYEAR = NINT(FNUM)
C     Convert IUYEAR to Four Digits
      IF (IUYEAR .GE. ISTRT_WIND .AND. IUYEAR .LE. 99) THEN
         IUYEAR = ISTRT_CENT*100 + IUYEAR
      ELSE IF (IUYEAR .LT. ISTRT_WIND) THEN
         IUYEAR = (ISTRT_CENT+1)*100 + IUYEAR
      END IF

 299  IF (IFC .GE. 5) THEN
C        Retrieve Surface Data Station Name (Optional)
         UANAME = FIELD(5)
      ELSE
         UANAME = 'UNKNOWN'
      END IF

      IF (IFC .EQ. 7) THEN
C        Retrieve Coordinates for Surface Data Location (Optional)
         CALL STODBL(FIELD(6),ILEN_FLD,UAX,IMIT)
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         END IF
         CALL STODBL(FIELD(7),ILEN_FLD,UAY,IMIT)
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         END IF
      END IF

 999  RETURN
      END

      SUBROUTINE ONDATA
C***********************************************************************
C                 ONDATA Module of AERMOD
C
C        PURPOSE: Process On-site Meteorology Data Station Options
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, James Paumier
C
C        DATE:    September 30, 1993
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: On-site Meteorological Data Station Identification
C
C        ERROR HANDLING:   Checks for Too Few Parameters;
C                          Checks for Invalid Numeric Fields;
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   MECARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'ONDATA'

      IF (IFC .EQ. 2) THEN
C        WRITE Error Message           ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 4) THEN
C        WRITE Error Message           ! Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 7) THEN
C        WRITE Error Message           ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

      CALL STONUM(FIELD(3),ILEN_FLD,FNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 199
      END IF
      IDSITE = NINT(FNUM)

 199  CALL STONUM(FIELD(4),ILEN_FLD,FNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 299
      END IF
      IOYEAR = NINT(FNUM)

 299  IF (IFC .GE. 5) THEN
C        Retrieve Surface Data Station Name (Optional)
         ONNAME = FIELD(5)
      ELSE
         ONNAME = 'UNKNOWN'
      END IF

      IF (IFC .EQ. 7) THEN
C        Retrieve Coordinates for Surface Data Location (Optional)
         CALL STODBL(FIELD(6),ILEN_FLD,ONX,IMIT)
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         END IF
         CALL STODBL(FIELD(7),ILEN_FLD,ONY,IMIT)
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         END IF
      END IF

 999  RETURN
      END

      SUBROUTINE PRBASE
C***********************************************************************
C                 PRBASE Module of the AERMOD Model
C
C        PURPOSE: Process Inputs for Profile Base Elevation
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    November 9, 1998
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Profile Base Elevation (m MSL), ZBASE
C
C        ERROR HANDLING:   Checks for No Parameters;
C                          Checks for No Units (uses default of m);
C                          Checks for Invalid or Suspicious Values of ZBASE;
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   MECARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'PRBASE'

      IF (IFC .EQ. 3 .OR. IFC .EQ. 4) THEN
         CALL STODBL(FIELD(3),ILEN_FLD,ZBASE,IMIT)
C        Check The Numerical Field
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 999
         END IF
         IF (IFC .EQ. 4 .AND. FIELD(4) .EQ. 'FEET') THEN
            ZBASE = 0.3048D0 * ZBASE
         ELSE IF (IFC .EQ. 4 .AND. FIELD(4) .NE. 'METERS') THEN
C           WRITE Warning Message - Invalid ZRUNIT Parameter
            CALL ERRHDL(PATH,MODNAM,'W','203','ZRUNIT')
         END IF
         IF (ZBASE .LT. 0.0D0 .AND. IMIT .EQ. 1) THEN
C           WRITE Warning Message - Possible Error In ZBASE
            CALL ERRHDL(PATH,MODNAM,'W','340',KEYWRD)
         END IF
      ELSE IF (IFC .GT. 4) THEN
C        WRITE Error Message           ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Error Message           ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      END IF

C     Reinitialize AZS, AZELEV, and AZHILL arrays for FLAT terrain
      IF (FLAT) THEN
         IF (.NOT. FLATSRCS) THEN
C           Assign ZBASE to source elevation for all sources
            AZS = ZBASE
         ELSE 
C           Assign ZBASE to source elevation only for FLAT sources
            DO ISRC = 1, NUMSRC
               IF (L_FLATSRC(ISRC)) THEN
                  AZS(ISRC) = ZBASE
               END IF
            END DO
         END IF
         IF (.NOT. FLATSRCS) THEN
C           Assign ZBASE to AZELEV and AZHILL for all receptors
            DO IREC = 1, NUMREC
               AZELEV(IREC) = ZBASE
               AZHILL(IREC) = ZBASE
            END DO
         END IF
      END IF

 999  RETURN
      END

      SUBROUTINE STAEND
C***********************************************************************
C                 STAEND Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Process Start and End Dates for Meteorology File
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   Modified code for setting the month, day, and hour 
C                    for the "end of the year", based on the STARTEND
C                    keyword, to resolve potential problems for PM-2.5 
C                    and ANNUAL average applications in which the first 
C                    hour of the file is not 01.  Also improved the
C                    error handling for STARTEND date inputs that are
C                    out of range, and included check for STARTEND
C                    data range less than 1 complete year for ANNUAL
C                    averages.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
C
C        MODIFIED:   To incorporate modifications to date processing
C                    for Y2K compliance, including use of date window
C                    variables (ISTRT_WIND and ISTRT_CENT) and calculation
C                    of 10-digit variables for start date (ISDATE) and
C                    end date (IEDATE).
C                    R.W. Brode, PES, Inc., 5/12/99
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Start and End Dates to Read from Meteorological File
C
C        ERROR HANDLING:   Checks for Too Few Parameters;
C                          Checks for Invalid Numeric Fields;
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   MECARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER IDYMAX(12)

C     Variable Initializations
      DATA IDYMAX/31,29,31,30,31,30,31,31,30,31,30,31/

      MODNAM = 'STAEND'

      IF (IFC .EQ. 8) THEN
C        Process for YR, MD, DY
         CALL STONUM(FIELD(3),ILEN_FLD,FNUM,IMIT)
C        Check The Numerical Field
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 198
         END IF
         ISYR = NINT(FNUM)
 198     CALL STONUM(FIELD(4),ILEN_FLD,FNUM,IMIT)
C        Check The Numerical Field
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 298
         END IF
         ISMN = NINT(FNUM)
         IF (ISMN .LT. 1 .OR. ISMN .GT. 12) THEN
C           WRITE Error Message    ! Invalid Month
            CALL ERRHDL(PATH,MODNAM,'E','203','MONTH')
         END IF
 298     CALL STONUM(FIELD(5),ILEN_FLD,FNUM,IMIT)
C        Check The Numerical Field
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 398
         END IF
         ISDY = NINT(FNUM)
         IF (ISMN .GE. 1 .AND. ISMN .LE. 12) THEN 
            IF (ISDY .LT. 1 .OR. ISDY .GT. IDYMAX(ISMN)) THEN
C              WRITE Error Message    ! Invalid Day
               CALL ERRHDL(PATH,MODNAM,'E','203','DAY')
            END IF
         END IF
 398     CALL STONUM(FIELD(6),ILEN_FLD,FNUM,IMIT)
C        Check The Numerical Field
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 498
         END IF
         IEYR = NINT(FNUM)
 498     CALL STONUM(FIELD(7),ILEN_FLD,FNUM,IMIT)
C        Check The Numerical Field
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 598
         END IF
         IEMN = NINT(FNUM)
         IF (IEMN .LT. 1 .OR. IEMN .GT. 12) THEN
C           WRITE Error Message    ! Invalid Month
            CALL ERRHDL(PATH,MODNAM,'E','203','MONTH')
         END IF
 598     CALL STONUM(FIELD(8),ILEN_FLD,FNUM,IMIT)
C        Check The Numerical Field
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 698
         END IF
         IEDY = NINT(FNUM)
         IF (IEMN .GE. 1 .AND. IEMN .LE. 12) THEN 
            IF (IEDY .LT. 1 .OR. IEDY .GT. IDYMAX(IEMN)) THEN
C              WRITE Error Message    ! Invalid Day
               CALL ERRHDL(PATH,MODNAM,'E','203','DAY')
            END IF
         END IF
 698     CONTINUE
C        Convert ISYR and IEYR to Four Digits
         IF (ISYR .GE. ISTRT_WIND .AND. ISYR .LE. 99) THEN
            ISYR = ISTRT_CENT*100 + ISYR
         ELSE IF (ISYR .LT. ISTRT_WIND) THEN
            ISYR = (ISTRT_CENT+1)*100 + ISYR
         END IF
         IF (IEYR .GE. ISTRT_WIND .AND. IEYR .LE. 99) THEN
            IEYR = ISTRT_CENT*100 + IEYR
         ELSE IF (IEYR .LT. ISTRT_WIND) THEN
            IEYR = (ISTRT_CENT+1)*100 + IEYR
         END IF
C        Calculate JULIAN Day for Start and End Dates
         CALL JULIAN (ISYR,ISMN,ISDY,ISJDAY)
         CALL JULIAN (IEYR,IEMN,IEDY,IEJDAY)
C        Use 1 for Start Hour and 24 for End Hour
         ISHR = 1
         IEHR = 24
C ---    Calculate 10-digit start date (ISDATE) and end date (IEDATE)
C        including 4-digit year (for comparisons with FULLDATE)
         IF (ISYR .LE. 2147) THEN
            ISDATE = ISYR*1000000 + ISMN*10000 + ISDY*100 + ISHR
         ELSE
            CALL ERRHDL(PATH,MODNAM,'E','365',KEYWRD)
            ISDATE = 2147123124
         END IF
         IF (IEYR .LE. 2147) THEN
            IEDATE = IEYR*1000000 + IEMN*10000 + IEDY*100 + IEHR
         ELSE
            CALL ERRHDL(PATH,MODNAM,'E','365',KEYWRD)
            IEDATE = 2147123124
         END IF
         
      ELSE IF (IFC .EQ. 10) THEN
C        Process for YR, MD, DY, HR
         CALL STONUM(FIELD(3),ILEN_FLD,FNUM,IMIT)
C        Check The Numerical Field
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 199
         END IF
         ISYR = NINT(FNUM)
 199     CALL STONUM(FIELD(4),ILEN_FLD,FNUM,IMIT)
C        Check The Numerical Field
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 299
         END IF
         ISMN = NINT(FNUM)
         IF (ISMN .LT. 1 .OR. ISMN .GT. 12) THEN
C           WRITE Error Message    ! Invalid Month
            CALL ERRHDL(PATH,MODNAM,'E','203','MONTH')
         END IF
 299     CALL STONUM(FIELD(5),ILEN_FLD,FNUM,IMIT)
C        Check The Numerical Field
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 399
         END IF
         ISDY = NINT(FNUM)
         IF (ISMN .GE. 1 .AND. ISMN .LE. 12) THEN 
            IF (ISDY .LT. 1 .OR. ISDY .GT. IDYMAX(ISMN)) THEN
C              WRITE Error Message    ! Invalid Day
               CALL ERRHDL(PATH,MODNAM,'E','203','DAY')
            END IF
         END IF
 399     CALL STONUM(FIELD(6),ILEN_FLD,FNUM,IMIT)
C        Check The Numerical Field
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 499
         END IF
         ISHR = NINT(FNUM)
         IF (ISHR .LT. 1 .OR. ISHR .GT. 24) THEN
C           WRITE Error Message    ! Invalid Hour
            CALL ERRHDL(PATH,MODNAM,'E','203','HOUR')
         END IF
 499     CALL STONUM(FIELD(7),ILEN_FLD,FNUM,IMIT)
C        Check The Numerical Field
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 599
         END IF
         IEYR = NINT(FNUM)
 599     CALL STONUM(FIELD(8),ILEN_FLD,FNUM,IMIT)
C        Check The Numerical Field
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 699
         END IF
         IEMN = NINT(FNUM)
         IF (IEMN .LT. 1 .OR. IEMN .GT. 12) THEN
C           WRITE Error Message    ! Invalid Month
            CALL ERRHDL(PATH,MODNAM,'E','203','MONTH')
         END IF
 699     CALL STONUM(FIELD(9),ILEN_FLD,FNUM,IMIT)
C        Check The Numerical Field
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 799
         END IF
         IEDY = NINT(FNUM)
         IF (IEMN .GE. 1 .AND. IEMN .LE. 12) THEN 
            IF (IEDY .LT. 1 .OR. IEDY .GT. IDYMAX(IEMN)) THEN
C              WRITE Error Message    ! Invalid Day
               CALL ERRHDL(PATH,MODNAM,'E','203','DAY')
            END IF
         END IF
 799     CALL STONUM(FIELD(10),ILEN_FLD,FNUM,IMIT)
C        Check The Numerical Field
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 899
         END IF
         IEHR = NINT(FNUM)
         IF (IEHR .LT. 1 .OR. IEHR .GT. 24) THEN
C           WRITE Error Message    ! Invalid Hour
            CALL ERRHDL(PATH,MODNAM,'E','203','HOUR')
         END IF
 899     CONTINUE
C        Convert ISYR and IEYR to Four Digits
         IF (ISYR .GE. ISTRT_WIND .AND. ISYR .LE. 99) THEN
            ISYR = ISTRT_CENT*100 + ISYR
         ELSE IF (ISYR .LT. ISTRT_WIND) THEN
            ISYR = (ISTRT_CENT+1)*100 + ISYR
         END IF
         IF (IEYR .GE. ISTRT_WIND .AND. IEYR .LE. 99) THEN
            IEYR = ISTRT_CENT*100 + IEYR
         ELSE IF (IEYR .LT. ISTRT_WIND) THEN
            IEYR = (ISTRT_CENT+1)*100 + IEYR
         END IF
C        Calculate JULIAN Day for Start and End Dates
         CALL JULIAN (ISYR,ISMN,ISDY,ISJDAY)
         CALL JULIAN (IEYR,IEMN,IEDY,IEJDAY)
C        Calculate 10-digit start date (ISDATE) and end date (IEDATE)
         IF (ISYR .LE. 2147) THEN
            ISDATE = ISYR*1000000 + ISMN*10000 + ISDY*100 + ISHR
         ELSE
            CALL ERRHDL(PATH,MODNAM,'E','365',KEYWRD)
            ISDATE = 2147123124
         END IF
         IF (IEYR .LE. 2147) THEN
            IEDATE = IEYR*1000000 + IEMN*10000 + IEDY*100 + IEHR
         ELSE
            CALL ERRHDL(PATH,MODNAM,'E','365',KEYWRD)
            IEDATE = 2147123124
         END IF
         
      ELSE IF (IFC .GT. 8) THEN
C        WRITE Error Message           ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
         
      ELSE
C        WRITE Error Message           ! Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      END IF

C     Determine MN, DY, and HR for end-of-the-year check.
C     Subtract one from start hour to set end hour for the year of data
      IF (ISHR .GT. 1) THEN
         IENDHOUR = ISHR - 1
         IENDDY   = ISDY
         IENDMN   = ISMN
      ELSE
         IENDHOUR = 24
         IF (ISDY .GT. 1) THEN
            IENDDY = ISDY - 1
            IENDMN = ISMN
         ELSE
            IENDMN = ISMN - 1
            IF (IENDMN .EQ. 0) IENDMN = 12
            IENDDY = IDYMAX(IENDMN)
         END IF
      END IF

C     Check for End Year .LT. Start Year
      IF (IEYR .LT. ISYR) THEN
C        WRITE Error Message    ! Invalid End Year
         CALL ERRHDL(PATH,MODNAM,'E','203','END YEAR')
         GO TO 999
      END IF

C     Check for STARTEND period less than a complete year if 
C     ANNUAL average is specified
      IF (ANNUAL .OR. MULTYR .OR. L_MAXDCONT) THEN
C        First check for End Year = Start Year,
C        then for End Year = Start Year + 1, otherwise 
C        if End Year - Start Year > 1 no further checks needed
         IF (IEYR .EQ. ISYR) THEN
C           End Year equals Start Year, therefore the
C           Start Month, Start Day, Start Hour must be 01/01/01, and
C           End Month, End Day, End Hour must be 12/31/24
            IF (ISMN .NE.  1 .OR. ISDY .NE.  1 .OR. ISHR .NE.  1 .OR.
     &          IEMN .NE. 12 .OR. IEDY .NE. 31 .OR. IEHR .NE. 24) THEN
C              WRITE Error Message    ! Incomplete Year for MULTYR or ANNUAL
               CALL ERRHDL(PATH,MODNAM,'E','480',KEYWRD)
            END IF
         ELSE IF (IEYR - ISYR .EQ. 1) THEN
C           End Year is Start Year plus 1, therefore the
C           End Month, End Day, End Hour must greater than or equal to
C           Start Month, Start Day, Start Hour
            IF (IEMN .LT. IENDMN) THEN
C              WRITE Error Message    ! Incomplete Year for MULTYR or ANNUAL
               CALL ERRHDL(PATH,MODNAM,'E','480',KEYWRD)
            ELSE IF (IEMN .EQ. IENDMN) THEN
               IF (IEDY .LT. IENDDY) THEN
C                 WRITE Error Message    ! Incomplete Year for MULTYR or ANNUAL
                  CALL ERRHDL(PATH,MODNAM,'E','480',KEYWRD)
               ELSE IF (IEDY .EQ. IENDDY) THEN
                  IF (IEHR .LT. IENDHOUR) THEN
C                    WRITE Error Message ! Incomplete Year for MULTYR or ANNUAL
                     CALL ERRHDL(PATH,MODNAM,'E','480',KEYWRD)
                  END IF
               END IF
            END IF
         END IF
      END IF

 999  RETURN
      END

      SUBROUTINE DAYRNG
C***********************************************************************
C                 DAYRNG Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Process the Selection of Days and Ranges of Days
C                 for Processing from the Meteorology File
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   Modified to account for leap-years vs. non-leap-years
C                    in cases where DAYRANGE inputs are defined based on 
C                    the Month/Day rather than Julian days.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 12/10/2012
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Array of Dates to Process from Meteorological File
C
C        ERROR HANDLING:   Checks for Too Few Parameters;
C                          Checks for Invalid Numeric Fields;
C                          Checks for Improper Combinations of Fields;
C                          Checks for Dates Out of Range
C
C        CALLED FROM:   MECARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, K, IMN, IDY, IMN1, IDY1, IMN2, IDY2, JDAYB, JDAYE
      CHARACTER BEGRNG*8, ENDRNG*8, CMN1*8, CDY1*8, CMN2*8, CDY2*8
      CHARACTER BLNK08*8
      LOGICAL RMARK, GMARK

C     Variable Initializations
      DATA BLNK08/'        '/

      MODNAM = 'DAYRNG'

      IF (IFC .LT. 3) THEN
C        WRITE Error Message           ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      ELSE
         DO 40 I = 3, IFC
C           First Check For Range Marker (-) And Gregorian Day Marker (/)
C           Initialize Character Fields
            BEGRNG = BLNK08
            ENDRNG = BLNK08
            CMN1 = BLNK08
            CDY1 = BLNK08
            CMN2 = BLNK08
            CDY2 = BLNK08
            CALL FSPLIT(PATH,KEYWRD,FIELD(I),ILEN_FLD,'-',RMARK,
     &                  BEGRNG,ENDRNG)
            CALL FSPLIT(PATH,KEYWRD,BEGRNG,8,'/',GMARK,CMN1,CDY1)
            IF (RMARK .AND. GMARK) THEN
               CALL FSPLIT(PATH,KEYWRD,ENDRNG,8,'/',GMARK,CMN2,CDY2)
            END IF

            IF (.NOT.RMARK .AND. .NOT.GMARK) THEN
C              Field Must Be a Single Julian Day
               CALL STONUM(BEGRNG,8,FNUM,IMIT)
C              Check The Numerical Field
               IF (IMIT .NE. 1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  GO TO 40
               ELSE
                  JDAY = NINT(FNUM)
               END IF
               IF (JDAY.GE.1 .AND. JDAY.LE.366 .AND. IMIT.EQ.1) THEN
                  IPROC(JDAY)  = 1
C                 Also need IPROCL array for Leap Years
                  IPROCL(JDAY) = 1
               ELSE
C                 WRITE Error Message    ! Invalid Julian Day
                  CALL ERRHDL(PATH,MODNAM,'E','203','Juli Day')
               END IF
               IF (JDAY.LT.ISJDAY .OR. JDAY.GT.IEJDAY) THEN
C                 WRITE Warning Message  ! Julian Day Out-of-Range
                  WRITE(DUMMY,'(I8)') JDAY
                  CALL ERRHDL(PATH,MODNAM,'W','350',DUMMY)
               END IF

            ELSE IF (RMARK .AND. .NOT.GMARK) THEN
C              Field Must Be a Julian Day Range - Extract Beg & End
               CALL STONUM(BEGRNG,8,FNUM,IMIT)
C              Check The Numerical Field
               IF (IMIT .NE. 1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  GO TO 40
               ELSE
                  JDAYB = NINT(FNUM)
               END IF
               CALL STONUM(ENDRNG,8,FNUM,IMIT)
C              Check The Numerical Field
               IF (IMIT .NE. 1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  GO TO 40
               ELSE
                  JDAYE = NINT(FNUM)
               END IF
               IF ((JDAYB .LE. JDAYE) .AND. (JDAYB .GE. 1) .AND.
     &             (JDAYE .LE. 366)) THEN
                  DO K = JDAYB, JDAYE
                     IPROC(K)  = 1
C                    Also need IPROCL array for Leap Years
                     IPROCL(K) = 1
                  END DO
               ELSE
C                 WRITE Error Message    ! Invalid Julian Day Range
                  CALL ERRHDL(PATH,MODNAM,'E','203','Juli Day')
               END IF
               IF (JDAYB.LT.ISJDAY .OR. JDAYE.GT.IEJDAY) THEN
C                 WRITE Warning Message  ! Julian Day Out-of-Range
                  WRITE(DUMMY,'(I3,"-",I3)') JDAYB, JDAYE
                  CALL ERRHDL(PATH,MODNAM,'W','350',DUMMY)
               END IF

            ELSE IF (.NOT.RMARK .AND. GMARK) THEN
C              Field Must Be a Single Month/Day
               CALL STONUM(CMN1,8,FNUM,IMIT)
C              Check The Numerical Field
               IF (IMIT .NE. 1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  GO TO 40
               ELSE
                  IMN = NINT(FNUM)
               END IF
               CALL STONUM(CDY1,8,FNUM,IMIT)
C              Check The Numerical Field
               IF (IMIT .NE. 1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  GO TO 40
               ELSE
                  IDY = NINT(FNUM)
               END IF
C ---          Determine JULIAN Day Number; For Non-Leap Year First;
C              Note that JDAY for MN/DY inputs will be assigned based on 
C              on the Year specified on the ME SURFFILE keyword. However, 
C              the IPROCL array used to identify which Julian day(s) to be
C              processed for leap years will be assigned based on the MN/DY 
C              input by the user.
               CALL JULIAN(ISYEAR,IMN,IDY,JDAY)
               IF ( (MOD(ISYEAR,4) .NE. 0) .OR.
     &              (MOD(ISYEAR,100) .EQ. 0 .AND. 
     &               MOD(ISYEAR,400) .NE. 0) ) THEN
C                 Not a Leap Year; Get JULIAN day number for specified MN/DY
C ---             Assign specified MN/DY to IPROC array (used for non-leap years)
C                 and IPROCL array (used for leap years)
                  IF (JDAY .GE. 1 .AND. JDAY .LE. 365) THEN
                     IPROC(JDAY) = 1
                     IF (IMN .GT. 2) THEN
                        IPROCL(JDAY+1) = 1
                     ELSE
                        IPROCL(JDAY) = 1
                     END IF
                  ELSE
C                    WRITE Error Message    ! Invalid Julian Day
                     CALL ERRHDL(PATH,MODNAM,'E','203','Juli Day')
                  END IF
C ---             Check for consistency with STARTEND inputs, if provided
                  IF (JDAY.LT.ISJDAY .OR. JDAY.GT.IEJDAY) THEN
C                    WRITE Warning Message  ! Julian Day Out-of-Range
                     WRITE(DUMMY,'(I8)') JDAY
                     CALL ERRHDL(PATH,MODNAM,'W','350',DUMMY)
                  END IF
                  
               ELSE
C ---             Determine JULIAN Day Number; For Leap Year
C ---             Assign specified MD/DY to IPROC array (used for non-leap years)
C                 and IPROCL array (used for leap years)
                  IF (JDAY .GE. 1 .AND. JDAY .LE. 366) THEN
                     IPROCL(JDAY) = 1
                     IF (IMN .GT. 2) THEN
                        IPROC(JDAY-1) = 1
                     ELSE
                        IPROC(JDAY)   = 1
                     END IF
                  ELSE
C                    WRITE Error Message    ! Invalid Julian Day
                     CALL ERRHDL(PATH,MODNAM,'E','203','Juli Day')
                  END IF
C ---             Check for consistency with STARTEND inputs, if provided
                  IF (JDAY.LT.ISJDAY .OR. JDAY.GT.IEJDAY) THEN
C                    WRITE Warning Message  ! Julian Day Out-of-Range
                     WRITE(DUMMY,'(I8)') JDAY
                     CALL ERRHDL(PATH,MODNAM,'W','350',DUMMY)
                  END IF
               END IF

            ELSE IF (RMARK .AND. GMARK) THEN
C              Field Must Be a Greg. Date Range (MN/DY-MN/DY)
               CALL STONUM(CMN1,8,FNUM,IMIT)
C              Check The Numerical Field
               IF (IMIT .NE. 1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  GO TO 41
               ELSE
                  IMN1 = NINT(FNUM)
               END IF
               CALL STONUM(CDY1,8,FNUM,IMIT)
C              Check The Numerical Field
               IF (IMIT .NE. 1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  GO TO 41
               ELSE
                  IDY1 = NINT(FNUM)
               END IF
 41            CALL STONUM(CMN2,8,FNUM,IMIT)
C              Check The Numerical Field
               IF (IMIT .NE. 1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  GO TO 40
               ELSE
                  IMN2 = NINT(FNUM)
               END IF
               CALL STONUM(CDY2,8,FNUM,IMIT)
C              Check The Numerical Field
               IF (IMIT .NE. 1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  GO TO 40
               ELSE
                  IDY2 = NINT(FNUM)
               END IF

C ---          Determine JULIAN Day Number; For Non-Leap Year First
               IF ( (MOD(ISYEAR,4) .NE. 0) .OR.
     &              (MOD(ISYEAR,100) .EQ. 0 .AND. 
     &               MOD(ISYEAR,400) .NE. 0) ) THEN
C                 Not a Leap Year; Get JULIAN day numbers for specified 
C                 Start MN/DY and End MN/DY, based on STARTEND 
                  CALL JULIAN(ISYEAR,IMN1,IDY1,JDAYB)
                  CALL JULIAN(ISYEAR,IMN2,IDY2,JDAYE)
C ---             Assign specified MN/DY range to IPROC array (used for non-leap years)
C                 and IPROCL array (used for leap years)
                  IF ((JDAYB .LE. JDAYE) .AND. (JDAYB .GE. 1) .AND.
     &                (JDAYE .LE. 365)) THEN
C ---                Assign IPROC array for use with non-leap years
                     DO K = JDAYB, JDAYE
                        IPROC(K) = 1
                     END DO
C ---                Assign IPROCL array for use with leap years
                     DO K = JDAYB, JDAYE
                        IPROCL(K) = 1
                     END DO
C ---                Assign IPROCL = 1 for last day if JDAYE = 365
                     IF (JDAYE .EQ. 365) THEN
                        IPROCL(366) = 1
                     END IF
                  ELSE
C                    WRITE Error Message    ! Invalid Julian Day
                     CALL ERRHDL(PATH,MODNAM,'E','203','Juli Day')
                  END IF
C ---             Check for consistency with STARTEND inputs, if provided
                  IF (JDAY.LT.ISJDAY .OR. JDAY.GT.IEJDAY) THEN
C                    WRITE Warning Message  ! Julian Day Out-of-Range
                     WRITE(DUMMY,'(I8)') JDAY
                     CALL ERRHDL(PATH,MODNAM,'W','350',DUMMY)
                  END IF

               ELSE
C ---             Determine JULIAN Day Number; For Leap Year 
C                 Get JULIAN day numbers for specified 
C                 Start MN/DY and End MN/DY
                  CALL JULIAN(ISYEAR,IMN1,IDY1,JDAYB)
                  CALL JULIAN(ISYEAR,IMN2,IDY2,JDAYE)
C ---             Assign specified MN/DY range to IPROC array (used for non-leap years)
C                 and IPROCL array (used for leap years)
                  IF ((JDAYB .LE. JDAYE) .AND. (JDAYB .GE. 1) .AND.
     &                (JDAYE .LE. 366)) THEN
                     DO K = JDAYB, JDAYE
                        IPROCL(K) = 1
                     END DO
                     DO K = JDAYB, JDAYE
                        IF (K .LE. 59 ) THEN
                           IPROC(K) = 1
                        ELSE IF (K .GT. 60) THEN
C ---                      Adjust non-leapyear Jday array for March 1 - Dec 31
                           IPROC(K-1) = 1
                        END IF
                     END DO
                  ELSE
C                    WRITE Error Message    ! Invalid Julian Day
                     CALL ERRHDL(PATH,MODNAM,'E','203','Juli Day')
                  END IF
                  IF (JDAYB.LT.ISJDAY .OR. JDAYE.GT.IEJDAY) THEN
C                    WRITE Warning Message  ! Julian Day Out-of-Range
                     WRITE(DUMMY,'(I3,"-",I3)') JDAYB, JDAYE
                     CALL ERRHDL(PATH,MODNAM,'W','350',DUMMY)
                  END IF
               
               END IF

            ELSE
C               WRITE Error Message    ! Invalid Field
                CALL ERRHDL(PATH,MODNAM,'E','203','DAYRANGE')
            END IF

 40      CONTINUE
      END IF

      RETURN
      END

      SUBROUTINE WDROTA
C***********************************************************************
C                 WDROTA Module of the AMS/EPA Regulatory Model - AERMOD
C
C     PURPOSE:    PROCESSES INPUT FOR ROTATING WIND DIRECTION DATA
C
C     PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C     INPUTS:     Input Runstream Image Parameters
C
C     OUTPUT:     Wind Direction Rotation Angle
C
C     CALLED FROM:   MECARD
C
C     ERROR HANDLING:   Checks for No Parameters;
C                       Checks for Too Many Parameters;
C                       Checks for Invalid Numeric Field
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'WDROTA'

      ROTANG = 0.0D0
      
      IF (IFC .EQ. 3) THEN
         CALL STODBL(FIELD(3),ILEN_FLD,ROTANG,IMIT)
         IF (IMIT .NE. 1) THEN
C            WRITE Error Message  ! Invalid Numeric Field Encountered
             CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         ELSE IF (DABS(ROTANG) .GT. 180.0D0) THEN
C            WRITE Error Message       ! ROTANG Out of Range
             CALL ERRHDL(PATH,MODNAM,'E','380','ROTANG')
         END IF
      ELSE IF (IFC .GT. 3) THEN
C        WRITE Error Message           ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Error Message           ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      END IF

      RETURN
      END

      SUBROUTINE WSCATS
C***********************************************************************
C                 WSCATS Module of the AMS/EPA Regulatory Model - AERMOD
C
C     PURPOSE:    PROCESSES INPUT FOR WIND SPEED CATEGORIES
C
C     PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C     INPUTS:     Input Runstream Image Parameters
C
C     OUTPUT:     Array of Wind Speed Category Limits (5)
C
C     CALLED FROM:   MECARD
C
C     ERROR HANDLING:   Checks for No Parameters;
C                       Checks for Too Many Parameters;
C                       Checks for Invalid Numeric Fields;
C                       Checks for Wind Speed Category Decreasing
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, IWS

C     Variable Initializations
      MODNAM = 'WSCATS'

      IF (IFC .EQ. 7) THEN
C        Fill UCAT Array
         DO I = 3, IFC
            CALL STODBL(FIELD(I),ILEN_FLD,DNUM,IMIT)
            IF (IMIT .NE. 1) THEN
C              WRITE Error Message  ! Invalid Numeric Field Encountered
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE IF (DNUM .LT. 1.0D0 .OR. DNUM .GT. 20.0D0) THEN
C              WRITE Error Message       ! UCAT Out of Range
               CALL ERRHDL(PATH,MODNAM,'E','380','UCAT')
            ELSE
               IWS = I - 2
               UCAT(IWS) = DNUM
               IF (IWS.GT.1 .AND. UCAT(IWS).LE.UCAT(IWS-1)) THEN
C                 WRITE Error Message    ! Invalid UCAT Value, LE Previous
                  CALL ERRHDL(PATH,MODNAM,'E','203','UCAT')
               END IF
            END IF
         END DO
      ELSE IF (IFC .GT. 7) THEN
C        WRITE Error Message           ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Error Message           ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      END IF

      RETURN
      END

      SUBROUTINE MEOPEN
C***********************************************************************
C                 MEOPEN Module of the AERMOD Model
C
C        PURPOSE: Open The Input file for Hourly Meteorological Data,
C                 And Check Header Record
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Meteorology File Specifications
C
C        OUTPUTS: File OPEN Error Status
C
C        CALLED FROM:   SETUP
C
C        REVISION HISTORY:
C         --  Modified code for reading the header record to correct
C             problems with processing of 'SCREEN' option in the
C             AERMET version date field.
C             R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
C         --  Modified code for reading the header record of the surface
C             file to include a separate test on the AERMET version date
C             field under the SCREEN option, to allow for the future use
C             of screening meteorology that is not directly linked to a
C             specific version of the AERMET processor.
C             R.W. Brode, U.S. EPA/OAQPS/AQMG, 01/26/2007
C         --  Modified check of version date in header record of surface
C             file.  Fatal error occurs if version date is greater than
C             90000 OR less than current release date.
C             R. Brode, PES, 09/10/02
C         --  Modified comparisons of met station IDs between SURFFILE
C             header record and ME pathway.  IDs are initially read as
C             integers, and then as characters if an error occurs.  Also
C             changed from fatal error to a warning message if a mismatch
C             occurs, and included SITEDATA ID in the comparison.
C             R. Brode, PES, 11/10/98
C         --  Modified to check for version date associated with of AERMET
C             surface file, and compare to two reference dates.  Versions
C             prior to first date cause fatal error, while version prior to
C             second date cause warning.  R. Brode, PES, 11/21/97
C         --  Removed the comparison of the years defined in the input
C             data file with the years declared in the control file
C         --  Added OPEN for the profile file
C         --  Read and interpreted the latitude and longitude in the
C             first record of the SURFFILE
C
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      INTEGER :: METVER, IOSI, ISSI, IUSI
      integer :: LEVEL, JFLAG
      LOGICAL :: FOPEN, MFOPEN, MPOPEN

C     Set Parameters for AERMET Version Dates.
C     Using data > MDATE1 or < MDATE2 causes a fatal error message.
      INTEGER, PARAMETER :: MDATE1 = 90000,  MDATE2 = 12345
C     Using data < MDATE1, > MDATE2, and .NE. MDATE3 causes warning message.
      INTEGER, PARAMETER :: MDATE3 = 14134

      CHARACTER (LEN=6)   :: ASOSTHRESH

      CHARACTER (LEN=8)   :: CUSI, CSSI, COSI
C Unused:      CHARACTER (LEN=6)   :: SPEC1, SPEC2, SPEC3
      CHARACTER (LEN=256) :: BUFFER

C     Variable Initializations
      MODNAM = 'MEOPEN'
      FOPEN  = .FALSE.
      MFOPEN = .FALSE.
      MPOPEN = .FALSE.

C --- Initialize AERMOD version date (METVER) and ASOSTHRESH flag
      METVER = 0
      ASOSTHRESH = '      '

C     File Unit Initialized in BLOCK DATA INIT
C     File Format Set By Keyword "SURFFILE" on "ME" pathway
C     OPEN Surface Met Data File --- Formatted is the only option
C     Open with ACTION='READ' to prevent overwrite and allow multiple access
C     READ In the Station Numbers for Comparison to SETUP File

C     Open SURFFILE Met File If Not Already Open
      INQUIRE (FILE=METINP,OPENED=FOPEN)

      IF (.NOT. FOPEN) THEN
C        Open SURFFILE Met File If Not Already Open
         INQUIRE (UNIT=MFUNIT,OPENED=FOPEN)
         IF (.NOT. FOPEN) THEN
C           Open with ACTION='READ' to prevent overwrite and allow multiple access
            OPEN(UNIT=MFUNIT,FILE=METINP,STATUS='OLD',
     &          ERR=998,ACTION='READ',FORM='FORMATTED')
            MFOPEN = .TRUE.

         ELSE
            MFOPEN = .FALSE.
C           SURFFILE Met File is Already Opened With Different Filename
            CALL ERRHDL(PATH,MODNAM,'E','501','SURFFILE')
         END IF
      ELSE
         MFOPEN = .TRUE.
      END IF

      GO TO 1000

C     Write Out Error Message for File OPEN Error
 998  CALL ERRHDL(PATH,MODNAM,'E','500','SURFFILE')
C     Skip READ if there is an error opening file

 1000 CONTINUE

C --- Next OPEN the PROFFILE Met File
C     File Format Set By Keyword "PROFFILE" on "ME" pathway
C     Open with ACTION='READ' to prevent overwrite and allow multiple access
C     OPEN Profile Met Data File --- Formatted is the only option

C --- Initialize FOPEN to .FALSE.
      FOPEN = .FALSE.

C     Open PROFFILE Met File If Not Already Open
      INQUIRE (FILE=PROINP,OPENED=FOPEN)

      IF (.NOT. FOPEN) THEN
C        Open PROFFILE Met File If Not Already Open
         INQUIRE (UNIT=MPUNIT,OPENED=FOPEN)
         IF (.NOT. FOPEN) THEN
C           Open with ACTION='READ' to prevent overwrite and allow multiple access
            OPEN(UNIT=MPUNIT,FILE=PROINP,STATUS='OLD',
     &          ERR=999,ACTION='READ',FORM='FORMATTED')
            MPOPEN = .TRUE.

         ELSE
            MPOPEN = .FALSE.
C           PROFFILE Met File is Already Opened With Different Filename
            CALL ERRHDL(PATH,MODNAM,'E','501','PROFFILE')
         END IF
      ELSE
         MPOPEN = .TRUE.
      END IF

      GO TO 1001

C     Write Out Error Message for File OPEN Error
 999  CALL ERRHDL(PATH,MODNAM,'E','500','PROFFILE')
      MPOPEN = .FALSE.

 1001 CONTINUE

      IF( .NOT.MFOPEN .and. .NOT.MPOPEN )THEN
         GOTO 1003
      ELSEIF( MFOPEN )THEN
         CONTINUE
      ENDIF

C --- First read header record as character string to check for AERMET
C     options, THRESH_1MIN, ADJ_U*, CCVR_Sub, and/or TEMP_SUb
C --- Assign SURFFILE to DUMMY variable for read error message
      DUMMY = 'SURFFILE'
      IF (MFOPEN) THEN
         READ(MFUNIT,1200,ERR=99,IOSTAT=IOERRN) BUFFER
 1200    FORMAT(A256)
         IF (IOERRN .NE. 0) GOTO 99 ! 16216 Added to check for empty file
      ELSE
         GO TO 1002
      END IF

C --- First extract AERMET version date, C_METVER
      IF( INDEX(BUFFER,'VERSION:') .NE. 0 )THEN
C        Extract AERMET version date
         READ(BUFFER(INDEX(BUFFER,'VERSION:')+8:
     &               INDEX(BUFFER,'VERSION:')+13),'(A6)')
     &                                           C_METVER
      ELSEIF( BUFFER(93:98) .NE. '      ' )THEN
C        The 'VERSION:' keyword is missing so assign columns 93-98 to C_METVER
         C_METVER = BUFFER(93:98)
      ELSE
         C_METVER = '      '
C        AERMET version not found in header record, issue fatal error message
         CALL ERRHDL(PATH,MODNAM,'E','395','No Version')      
      ENDIF

C --- Next check for THRESH_1MIN indicating that wind speed threshold was 
C     applied to 1-minute ASOS wind data
      IF( INDEX(BUFFER,'THRESH_1MIN') .NE. 0 )THEN
C        Extract 1-min ASOS threshold value and write out Warning message
         READ(BUFFER(INDEX(BUFFER,'=')+1:
     &               INDEX(BUFFER,'m/s')),*)
     &                                   ASOSTHRESH
         CALL ERRHDL(PATH,MODNAM,'W','186',ASOSTHRESH)
      ENDIF

C --- Check for use of various met data processing options and issue
C     message as appropriate
      IF( INDEX(BUFFER,'ADJ_U*') .NE. 0 )THEN
C        Check for use of ADJ_U* option in AERMET
         L_AdjUstar = .TRUE.
         CALL ERRHDL(PATH,MODNAM,'W','187',' ')
C        Assign keyword to MODOPS array
         MODOPS(23) = 'ADJ_U*'
      ENDIF

c     JAT 1/14/21 ISSUE D077; ADD PROG AS WELL AS MMIF TO ACCOMODATE NEW AERMET
C      IF( INDEX(BUFFER,'MMIF') .NE. 0 )THEN
      IF( INDEX(BUFFER,'MMIF') .NE. 0 .OR. INDEX(BUFFER,'PROG') 
     &   .NE. 0 )THEN
C        Check for use of MMIF option in AERMET
         L_MMIF_Data = .TRUE.
         CALL ERRHDL(PATH,MODNAM,'W','182','  ')
C        Assign keyword to MODOPS array
         MODOPS(24) = 'MMIF_Data'
C         IF( INDEX(BUFFER,'MMIF VERSION') .NE. 0 )THEN
         IF( INDEX(BUFFER,'MMIF VERSION') .NE. 0 .OR. 
     &   INDEX(BUFFER,'PROG VERSION') .NE. 0 )THEN 
C           Extract MMIF Version info to include in output file
            IF( INDEX(BUFFER,'MMIF VERSION') .NE. 0) THEN
              MMIF_Version = BUFFER( INDEX(BUFFER,'MMIF VERSION'):
     &                             INDEX(BUFFER,'MMIF VERSION')+28 ) ! CRT 1/11/2021 D077, 26 to 28
            ELSE
               MMIF_Version = BUFFER( INDEX(BUFFER,'PROG VERSION'):
     &                             INDEX(BUFFER,'PROG VERSION')+28 ) ! CRT 1/11/2021 D077, 26 to 28
            ENDIF
         ELSE
            MMIF_Version = ''
         ENDIF  
      ENDIF

      IF( INDEX(BUFFER,'BULKRN') .NE. 0 )THEN
C        Check for use of BULKRN option in AERMET and assign logical variable;
C        Note that BULKRN is NOT a BETA or non-DFAULT option, and message is
C        issued for informational purposes
         L_BULKRN = .TRUE.
C        Assign keyword to MODOPS array, unless MMIF is also being used
         IF( .NOT. L_MMIF_Data )THEN
            MODOPS(24) = 'BULKRN'
         END IF
C ---    Check for use of MMIF data and adjust BULKRN message accordingly
         IF( L_MMIF_Data )THEN
            CALL ERRHDL(PATH,MODNAM,'W','181','with MMIF')
         ELSE
            CALL ERRHDL(PATH,MODNAM,'W','181','in AERMET')
         ENDIF
      ENDIF

      IF( INDEX(BUFFER,'CCVR_Sub') .NE. 0 )THEN
C ---    Set logical flag indicating that CCVR_Sub option was used
         L_CCVR_Sub = .TRUE.
      ELSE
         L_CCVR_Sub = .FALSE.
      ENDIF

      IF( INDEX(BUFFER,'TEMP_Sub') .NE. 0 )THEN
C ---    Set logical flag indicating that TEMP_Sub option was used
         L_TEMP_Sub = .TRUE.
      ELSE
         L_TEMP_Sub = .FALSE.
      ENDIF

C --- Read Lat/Lon from header record BUFFER
      READ(BUFFER,1900,ERR=99,IOSTAT=IOERRN) ALAT, ALON
 1900 FORMAT(2A10)

C --- Now extract UA, SF, and OS station IDs from header record
      IF( INDEX(BUFFER,'UA_ID:') .GT. 0 )THEN
         READ(BUFFER(INDEX(BUFFER,'UA_ID:')+7:
     &               INDEX(BUFFER,'UA_ID:')+15),'(A)') CUSI
      ELSE
         CUSI = '        '
      END IF
      CALL STONUM(CUSI,8,FNUM,IMIT)
      IF (IMIT .EQ. 1) THEN
         IUSI = NINT(FNUM)
      ELSE
         IUSI = 0
      END IF

      IF( INDEX(BUFFER,'SF_ID:') .GT. 0 )THEN
         READ(BUFFER(INDEX(BUFFER,'SF_ID:')+7:
     &               INDEX(BUFFER,'SF_ID:')+15),'(A)') CSSI
      ELSE
         CSSI = '        '
      END IF
      CALL STONUM(CSSI,8,FNUM,IMIT)
      IF (IMIT .EQ. 1) THEN
         ISSI = NINT(FNUM)
      ELSE
         ISSI = 0
      END IF

      IF( INDEX(BUFFER,'OS_ID:') .GT. 0 )THEN
         READ(BUFFER(INDEX(BUFFER,'OS_ID:')+7:
     &               INDEX(BUFFER,'OS_ID:')+15),'(A)') COSI
      ELSE
         COSI = '        '
      END IF
      CALL STONUM(COSI,8,FNUM,IMIT)
      IF (IMIT .EQ. 1) THEN
         IOSI = NINT(FNUM)
      ELSE
         IOSI = 0
      END IF

C     Check for valid version of meteorological data.
      IF (SCREEN .AND. C_METVER .NE. 'SCREEN') THEN
C        Check for use of screening meteorology under the SCREEN option
         CALL ERRHDL(PATH,MODNAM,'W','397',C_METVER)
      ELSE IF (.NOT.SCREEN .AND. C_METVER .EQ. 'SCREEN') THEN
C        Check for use of screening meteorology without SCREEN option
         CALL ERRHDL(PATH,MODNAM,'W','398','        ')
      ELSE IF (.NOT.SCREEN) THEN
C        Read integer Julian date from character field for comparison to
C        acceptable AERMET version number.
         READ(C_METVER,'(1X,I5)',ERR=109) METVER
         IF (METVER.GT.MDATE1 .OR. METVER.LT.MDATE2) THEN
C ---       Issue fatal error for use of invalid met version date
            WRITE(DUMMY,'(2X,I5.5)') METVER
            CALL ERRHDL(PATH,MODNAM,'E','395',DUMMY)
         ELSE IF (METVER.GE.MDATE2 .AND. METVER.LT.MDATE3) THEN
C ---       Issue warning message for use of "outdated" met version date
            WRITE(DUMMY,'(2X,I5.5)') METVER
            CALL ERRHDL(PATH,MODNAM,'W','396',DUMMY)
C ---       Set logical flag for use of "old" met data
            L_OldMetVer = .TRUE.
         END IF

         GO TO 110

109      CONTINUE
C ---    Error reading METVER date as integer from C_METVER character string;
C        issue warning message for "outdated" met version date; this allows for
C        interim version "dates" for AERMET drafts, e.g., 13DFT
         CALL ERRHDL(PATH,MODNAM,'W','396',C_METVER)
C ---    Set logical flag for use of "old" met data
         L_OldMetVer = .TRUE.
      END IF

110   CONTINUE

C --- Check Station IDs in SURFFILE for agreement with ME pathway;
C     First check for blank fields in surface file header record     
      IF( LEN_TRIM(CSSI) .EQ. 0 .AND. IDSURF .NE. 0 )THEN
C        Write Warning Message:  SURFDATA ID missing; 
         CALL ERRHDL(PATH,MODNAM,'W','531','SURFDATA')
      ELSEIF( ISSI .NE. IDSURF )THEN
C        Write Warning Message:  SURFDATA id mismatch
         CALL ERRHDL(PATH,MODNAM,'W','530','SURFDATA')
      ENDIF
      IF( LEN_TRIM(CUSI) .EQ. 0 .AND. IDUAIR .NE. 0 )THEN
C        Write Warning Message:  UAIRDATA ID missing; 
         CALL ERRHDL(PATH,MODNAM,'W','531','UAIRDATA')
      ELSEIF( IUSI .NE. IDUAIR )THEN
C        Write Warning Message:  UAIRDATA id mismatch
         CALL ERRHDL(PATH,MODNAM,'W','530','UAIRDATA')
      ENDIF
      IF( IMSTAT(9) .EQ. 1 )THEN
         IF( LEN_TRIM(COSI) .EQ. 0 .AND. IDSITE .NE. 0 )THEN
C           Write Warning Message:  SITEDATA ID missing; 
            CALL ERRHDL(PATH,MODNAM,'W','531','SITEDATA')
         ELSEIF( IOSI .NE. IDSITE )THEN
C           Write Warning Message:  SITEDATA id mismatch
            CALL ERRHDL(PATH,MODNAM,'W','530','SITEDATA')
         END IF
      ENDIF

C     Get the hemisphere and latitude (from the first record of the
C     scalar file
      CALL DCDLAT ()

C --- Open and read first hour of PROFFILE in order to check for
C     PROFFILE heights that may indicate use of prognostic met data

1002  CONTINUE

C --- Check for whether PROFFILE met file has been opened;
C     otherwise, skip to end
      IF( .NOT. MPOPEN ) GOTO 1003

C---- Initialize the profile data to missing;
C     READ profile data based on free format
C
      CALL PFLINI ()
      LEVEL = 1
      JFLAG = 0
C     Read record from ASCII profile file using FREE format; compute
C     sigma_V from sigma_A and wind speed

C --- Set 'DUMMY' variable = 'PROFFILE' for error handling
      DUMMY = 'PROFFILE'

      DO WHILE( JFLAG .EQ. 0 )
         READ( MPUNIT, *, END=1000, ERR=99, IOSTAT=IOERRN ) KYEAR,
     &       KMONTH, KDAY, KHOUR, PFLHT(LEVEL), JFLAG,
     &       PFLWD(LEVEL), PFLWS(LEVEL), PFLTA(LEVEL),
     &       PFLSA(LEVEL), PFLSW(LEVEL)

C        Convert the data to the required units
         CALL PFLCNV (LEVEL)

C        Set the number of profile levels to current index, store
C        the 'top of profile' flag, and increment level if not at top
C        Check that the level does not exceed the maximum allowable
         NPLVLS = LEVEL
         IFLAG(LEVEL) = JFLAG

C ---    Check for PFLHT > 999m, which could indicate use of
C        MMIF or other gridded met data inputs
         IF( PFLHT(LEVEL) .GT. 999.0D0 .OR. PFLHT(LEVEL) .LE. 0.0D0)THEN
            IF( .NOT. L_MMIF_Data .AND. .NOT. L_MMIF_Profile )THEN
C ---          Issue warning message for PFLHT > 999m if MMIF data 
C              inputs were not specified; message(s) will only be 
C              generated for a single profile
               WRITE(DUMMY,'(''LVL'',I2.2,1X,I5,''m'')') LEVEL, 
     &                                      NINT(PFLHT(LEVEL))
               CALL ERRHDL(PATH,MODNAM,'W','184',DUMMY)
               IF( JFLAG .EQ. 1 )THEN
C ---             Top of profile has been flagged for PFLHT > 999m
C                 Set L_MMIF_Profile flag to .TRUE. to turn off
C                 additional warning messages
                  L_MMIF_Profile = .TRUE.
               ENDIF
            ENDIF
         ENDIF

         IF( JFLAG .EQ. 0 )THEN
            LEVEL = LEVEL + 1

            IF( LEVEL .GT. MXPLVL )THEN
               IF( .NOT. PFLERR )THEN
C                 WRITE Error Message: Number of profile levels
C                                      exceeds maximum allowable
                  WRITE(DUMMY,'(I8)') MXPLVL
                  CALL ERRHDL(PATH,MODNAM,'E','465',DUMMY)
                  PFLERR = .TRUE.
                  RUNERR = .TRUE.
               END IF

C              Limit the number of levels to the maximum allowable
               LEVEL = MXPLVL
            END IF

         END IF

      END DO

      REWIND MPUNIT

      GO TO 1003

C     Write Out Error Message for File READ Error
 99   CALL ERRHDL(PATH,MODNAM,'E','510',DUMMY)
      RETURN

 1003 CONTINUE

      END

      SUBROUTINE SCIMIT
C***********************************************************************
C                 SCIMIT Module of AERMOD Model 
C
C        PURPOSE: Process Sampled Chronological Input Model (SCIM) Options
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    April 14, 1998
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: SCIM parameters:  Start Hour (1-24)
C                                   Number of Hours to Skip
C                                   Optional filename to summarize
C                                      the SCIM's meteorology
C
C        ERROR HANDLING:   Checks for No Parameters;
C                          Checks for Too Many Parameters;
C                          Checks for Invalid Numeric Inputs
C
C        CALLED FROM:   MECARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, IMIT5, IMIT6
      
C     Variable Initializations
      MODNAM = 'SCIMIT'
      IMIT5  = 1
      IMIT6  = 1

      IF (IFC .EQ. 4 .OR. IFC .EQ. 6 .OR. IFC .EQ. 8) THEN
         CALL STONUM(FIELD(3),ILEN_FLD,FNUM,IMIT)
C        Check The Numerical Field
         IF (IMIT .NE. 1) THEN
C           Issue error message: Invalid numeric field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         ELSE
C           Assign value for SCIM starting hour
            NREGSTART = NINT( FNUM )
         END IF
         IF (NREGSTART .LT. 1 .OR. NREGSTART .GT. 24) THEN
C           WRITE Error Message        ! Start Hour out of range
            CALL ERRHDL(PATH,MODNAM,'E','380','StartHr')
         END IF

         CALL STONUM(FIELD(4),ILEN_FLD,FNUM,IMIT)
C        Check The Numerical Field
         IF (IMIT .NE. 1) THEN
C           Issue error message: Invalid numeric field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         ELSE
C           Assign value for SCIM interval
            NREGINT = NINT( FNUM )
         END IF
         IF (NREGINT .LT. 1) THEN
C           WRITE Error Message        ! NRegInt is out of range
            CALL ERRHDL(PATH,MODNAM,'E','380','NRegInt')
         END IF

         IF (IFC .EQ. 8) THEN
C ---       Issue warning message:  Wet scimming not supported
            CALL ERRHDL(PATH,MODNAM,'W','157',KEYWRD)

C ---       Skip wet scimming inputs in fields 5 and 6

C           Assume fields 7 and 8 are optional files for summary of
C           SCIM'd met data.
            SCIMOUT = .TRUE.
C           Retrieve Sfc Met Data Filename as Character Substring to Maintain Case
            IF ((LOCE(7)-LOCB(7)) .LE. (ILEN_FLD - 1) ) THEN
C              Retrieve Filename as Character Substring to Maintain Original Case
C              Also Check for Filename Larger Than ILEN_FLD Characters
               SCIM_SFCFIL = RUNST1(LOCB(7):LOCE(7))
               OPEN(UNIT=ISUNIT,FILE=SCIM_SFCFIL,STATUS='REPLACE')
            ELSE
C              WRITE Error Message:  SCIM_SFCFIL Field is Too Long
               WRITE(DUMMY,'(I8)') ILEN_FLD
               CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
            END IF
C           Retrieve Pfl Met Data Filename as Character Substring to Maintain Case
            IF ((LOCE(8)-LOCB(8)) .LE. (ILEN_FLD - 1) ) THEN
C              Retrieve Filename as Character Substring to Maintain Original Case
C              Also Check for Filename Larger Than ILEN_FLD Characters
               SCIM_PROFIL = RUNST1(LOCB(8):LOCE(8))
               OPEN(UNIT=IPUNIT,FILE=SCIM_PROFIL,STATUS='REPLACE')
            ELSE
C              WRITE Error Message:  SCIM_PROFIL Field is Too Long
               WRITE(DUMMY,'(I8)') ILEN_FLD
               CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
            END IF

         ELSE IF (IFC .EQ. 6) THEN
C ---       Check fields 5 and 6 for optional SCIM'd met data files or for
C           unsupported wet scimming inputs.

C ---       Check the fields for non-numeric characters; if any are found, 
C           then assume that these are optional met data file names;
C           otherwise, assume these are unsupported wet scimming inputs
            DO I = LOCB(5), LOCE(5)
               READ(RUNST1(I:I),'(I1)',ERR=99) IMIT5
            END DO
            DO I = LOCB(6), LOCE(6)
               READ(RUNST1(I:I),'(I1)',ERR=99) IMIT6
            END DO
            
C ---       Neither field 5 nor 6 has non-numeric characters, 
C           assume that these are unsupported wet scimming inputs.
C           Issue warning message:  Wet scimming not supported
            CALL ERRHDL(PATH,MODNAM,'W','157',KEYWRD)

C ---       Done processing, return
            RETURN

C ---       Non-numeric characters found in fields 5 and/or 6;
C           assume these are file names
   99       CONTINUE
   
C ---       At least one field has non-numeric characters, 
C           assume that these are optional SCIM'd met data file names
            SCIMOUT = .TRUE.
C           Retrieve Sfc Met Data Filename as Character Substring to Maintain Case
            IF ((LOCE(5)-LOCB(5)) .LE. (ILEN_FLD - 1) ) THEN
C              Retrieve Filename as Character Substring to Maintain Original Case
C              Also Check for Filename Larger Than ILEN_FLD Characters
               SCIM_SFCFIL = RUNST1(LOCB(5):LOCE(5))
               OPEN(UNIT=ISUNIT,FILE=SCIM_SFCFIL,STATUS='REPLACE')
            ELSE
C              WRITE Error Message:  SCIM_SFCFIL Field is Too Long
               WRITE(DUMMY,'(I8)') ILEN_FLD
               CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
            END IF
C           Retrieve Pfl Met Data Filename as Character Substring to Maintain Case
            IF ((LOCE(6)-LOCB(6)) .LE. (ILEN_FLD - 1) ) THEN
C              Retrieve Filename as Character Substring to Maintain Original Case
C              Also Check for Filename Larger Than ILEN_FLD Characters
               SCIM_PROFIL = RUNST1(LOCB(6):LOCE(6))
               OPEN(UNIT=IPUNIT,FILE=SCIM_PROFIL,STATUS='REPLACE')
            ELSE
C              WRITE Error Message:  SCIM_PROFIL Field is Too Long
               WRITE(DUMMY,'(I8)') ILEN_FLD
               CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
            END IF

         END IF

      ELSE IF (IFC .GT. 8) THEN
C        WRITE Error Message           ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Error Message           ! Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
      END IF

      RETURN
      END

      SUBROUTINE NUMYR
C***********************************************************************
C                 NUMYR Module of the AMS/EPA Regulatory Model - AERMOD
C
C     PURPOSE:    Processes optional keyword to specify the number of
C                 years in the input meteorological data files to
C                 adjust arrays sizes for the MAXDCONT option
C
C     PROGRAMMER: Roger Brode, U.S. EPA, OAQPS, AQMG
C
C        DATE:    February 29, 2012
C
C     INPUTS:     Input Runstream Image Parameters
C
C     OUTPUT:     Wind Direction Rotation Angle
C
C     CALLED FROM:   MECARD
C
C     ERROR HANDLING:   Checks for No Parameters;
C                       Checks for Too Many Parameters;
C                       Checks for Invalid Numeric Field
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'NUMYR'
     
      IF (IFC .EQ. 3) THEN
         CALL STONUM(FIELD(3),ILEN_FLD,FNUM,IMIT)
         IF (IMIT .NE. 1) THEN
C           WRITE Error Message  ! Invalid Numeric Field Encountered
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         ELSE
            NYEARS = NINT(FNUM)
            IF ( ABS(FNUM-REAL(NYEARS)) .GT. 1.0E-5 ) THEN
C              WRITE Error Message  ! Invalid Numeric Field, should be integer
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            END IF
         END IF
      ELSE IF (IFC .GT. 3) THEN
C        WRITE Error Message           ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Error Message           ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      END IF

      RETURN
      END
         
      SUBROUTINE TURBOPT
C***********************************************************************
C                 TURBOPT Module of the AMS/EPA Regulatory Model - AERMOD
C
C     PURPOSE:    Processes optional keyword to specify how to treat
C                 turbulence in the profile file
C
C     PROGRAMMER: James Thurman, U.S. EPA, OAQPS, AQMG
C
C        DATE:    January 29, 2021
C
C     INPUTS:     Input Runstream Image Parameters
C
C     OUTPUT:     set logical variables for TURBOPTS
C
C     CALLED FROM:   MECARD
C
C     ERROR HANDLING:   Checks for correct use of DFAULT keyword;
C                       
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
C     LOOPING VARIABLE
      INTEGER I
      LOGICAL LFOUND
      CHARACTER MODNAM*12
      CHARACTER(LEN=8) KEYS(9)
      
C     Variable Initializations
      MODNAM = 'TURBOPT'
     
      DATA KEYS /'NOTURB  ','NOTURBST','NOTURBCO','NOSA    ',
     &'NOSW    ','NOSAST  ','NOSWST  ','NOSACO  ','NOSWCO  '/
      
      
      I=1
      LFOUND=.FALSE.
C     LOOK FOR THE KEYWORD IN THE KEYS ARRAY AND SET THE APPROPRIATE VALUE OF TURBOPTS TO TRUE
      DO WHILE( I .LE. 9 .AND. .NOT. LFOUND)
          IF (KEYWRD .EQ. KEYS(I)) THEN
              LFOUND=.TRUE.
              TURBOPTS(I)=.TRUE.
          ELSE
              I=I+1
          ENDIF
      ENDDO
      
C     WRITE MESSAGE THAT AN OPTION CHOSEN
      CALL ERRHDL(PATH,MODNAM,'I','443',KEYWRD)
      
C     1:  NOTURB (ignore sigma-theta and sigma-w for all hours)
C     2:  NOTURBST (ignore sigma-theta and sigma-w for stable hours only (OBULEN > 0))
C     3:  NOTURBCO (ignore sigma-theta and sigma-w for convective hours only (OBULEN < 0))
C     4:  NOSA (ignore sigma-theta for all hours)
C     5:  NOSW (ignore sigma-w for all hours)
C     6:  NOSAST (ignore sigma-theta for stable hours only (OBULEN > 0))
C     7:  NOSWST (ignore sigma-w for stable hours only (OBULEN > 0))
C     8:  NOSACO (ignore sigma-theta for convective hours only (OBULEN < 0))
C     9:  NOSWCO (ignore sigma-w for convective hours only (OBULEN < 0))
      
C     ONLY NOTURB AND NOTURBST CAN BE USED WITH THE DEFAULT KEYWORD
C     IF ONE OF THE OTHERS ARE USED WITH DEFAULT, THEN ISSUE WARNING AND
C     RESET TO FALSE
      IF (I .GT. 2 .AND. DFAULT) THEN
          CALL ERRHDL(PATH,MODNAM,'W','444',KEYWRD)
          TURBOPTS(I)=.FALSE.
      ENDIF
      
      RETURN
      END
