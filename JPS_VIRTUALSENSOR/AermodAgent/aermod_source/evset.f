      SUBROUTINE EVCARD
C***********************************************************************
C                 EVCARD Module of AERMOD Model
C
C        PURPOSE: To process EVent Pathway card images
C
C        PROGRAMMER:  Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   Removed code to add 24 hours to IEDATE (end date
C                    of all events) since the EVDATE already represents
C                    the last hour of the event. This avoids a potential
C                    fatal error for out-of-sequence dates between the
C                    met data and ozone or background data files if the 
C                    event occurs on the last day of the data period.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
C
C        MODIFIED:   To incorporate modifications to date processing
C                    for Y2K compliance, including use of date window
C                    variables (ISTRT_WIND and ISTRT_CENT) and calculation
C                    of 10-digit variables for start date (ISDATE) and
C                    end date (IEDATE).
C                    R.W. Brode, PES, Inc., 5/12/99
C
C        MODIFIED:   To remove reassignment of ISYEAR.
C                    R.W. Brode, PES, 4/2/99
C
C        MODIFIED:   To remove mixed-mode math in calculation of
C                    ISDATE and IEDATE - 4/19/93
C
C        INPUTS:  Pathway (EV) and Keyword
C
C        OUTPUTS: Pass Two Event Setup
C
C        CALLED FROM:   SETUP
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, ILSAVE, ITEMPDATE, ITEMPYEAR

C     Variable Initializations
      MODNAM = 'EVCARD'

      IF (KEYWRD .EQ. 'STARTING') THEN
C        Set Status Switch
         IESTAT(1) = IESTAT(1)+1
         IEVENT = 1
         IF (IESTAT(1) .NE. 1) THEN
C           Error Message: Repeat Starting In Same Pathway
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         END IF
      ELSE IF (KEYWRD .EQ. 'EVENTPER') THEN
C        Set Status Switch
         IESTAT(2) = IESTAT(2)+1
C        Check for First Occurrence of EVENTPER Card, and
C        Reinitialize IPROC Array
         IF (IESTAT(2) .EQ. 1) THEN
            DO I = 1, 366
               IPROC(I) = 0
            END DO
         END IF
C        Process Average Period, Date and Source Group      ---   CALL EVPER
         CALL EVPER
      ELSE IF (KEYWRD .EQ. 'EVENTLOC') THEN
C        Set Status Switch
         IESTAT(3) = IESTAT(3)+1
C        Process Discrete Receptor Location                 ---   CALL EVLOC
         CALL EVLOC
      ELSE IF (KEYWRD .EQ. 'INCLUDED') THEN
C        Set Status Switch
         IESTAT(10) = IESTAT(10) + 1
C        Save ILINE as ISAVE
         ILSAVE = ILINE
C        Process the Included Receptor File                 ---   CALL INCLUD
         CALL INCLUD
C        Retrieve ILINE From ISAVE         
         ILINE = ILSAVE
      ELSE IF (KEYWRD .EQ. 'FINISHED') THEN
C        Check for missing EVENTLOC cards
         IF (IESTAT(2) .GT. IESTAT(3)) THEN
C           Write Error Message:  Missing EVENTLOC
            CALL ERRHDL(PATH,MODNAM,'E','130','EVENTLOC')
         END IF
         NUMEVE = IEVENT - 1
C        Set Status Switch
         IESTAT(50) = IESTAT(50)+1
         IF (IESTAT(50) .NE. 1) THEN
C           Error Message: Repeat Finished In Same Pathway
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
            GO TO 999
         END IF

         IF (NUMEVE .GE. 1) THEN
C ---       At least one event has been defined;
C           Get start date, ISDATE, and end date, IEDATE
            ISDATE = EVDATE(1)
            IEDATE = EVDATE(1)
            ISYR = ISDATE/1000000
            IEYR = IEDATE/1000000
C           Convert 8-digit EVDATE to 10-digit ISDATE and IEDATE
            IF (ISYR .GE. ISTRT_WIND .AND. ISYR .LE. 99) THEN
               ISYR   = ISTRT_CENT*100 + ISYR
               ISDATE = ISTRT_CENT*100000000 + ISDATE
            ELSE IF (ISYR .LT. ISTRT_WIND) THEN
               ISYR   = (ISTRT_CENT+1)*100 + ISYR
               ISDATE = (ISTRT_CENT+1)*100000000 + ISDATE
            END IF
C           
            IF (IEYR .GE. ISTRT_WIND .AND. IEYR .LE. 99) THEN
               IEYR   = ISTRT_CENT*100 + IEYR
               IEDATE = ISTRT_CENT*100000000 + IEDATE
            ELSE IF (IEYR .LT. ISTRT_WIND) THEN
               IEYR   = (ISTRT_CENT+1)*100 + IEYR
               IEDATE = (ISTRT_CENT+1)*100000000 + IEDATE
            END IF
C           Loop through events to find start date and end date
            DO I = 1, NUMEVE
               ITEMPDATE = EVDATE(I)
               ITEMPYEAR = ITEMPDATE/1000000
               IF (ITEMPYEAR.GE.ISTRT_WIND .AND. ITEMPYEAR.LE.99) THEN
                  ITEMPDATE = ISTRT_CENT*100000000 + ITEMPDATE
               ELSE IF (ITEMPYEAR .LT. ISTRT_WIND) THEN
                  ITEMPDATE = (ISTRT_CENT+1)*100000000 + ITEMPDATE
               END IF
               IF (ITEMPDATE .LT. ISDATE) ISDATE = ITEMPDATE
               IF (ITEMPDATE .GT. IEDATE) IEDATE = ITEMPDATE
            END DO
C           Set start hour to 00 since EVDATE is for the last hour of event
            ISDATE = (ISDATE/100)*100
            ISYR = ISDATE/1000000
            IEYR = IEDATE/1000000
            ISMN = (ISDATE/10000) - (ISDATE/1000000)*100
            IEMN = (IEDATE/10000) - (IEDATE/1000000)*100
            ISDY = (ISDATE/100) - (ISDATE/10000)*100
            IEDY = (IEDATE/100) - (IEDATE/10000)*100
         END IF

C        Write Out The Error Message: Mandatory Keyword Missing
         IF (IESTAT(1) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','STARTING')
         END IF
         IF (IESTAT(2) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','EVENTPER')
         END IF
         IF (IESTAT(3) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','EVENTLOC')
         END IF

      ELSE
C        Write Error Message: Invalid Keyword for This Pathway
         CALL ERRHDL(PATH,MODNAM,'E','110',KEYWRD)
      END IF

 999  RETURN
      END

      SUBROUTINE EVPER
C***********************************************************************
C                 EVPER Module of AERMOD Model
C
C        PURPOSE: Processes Date, Average Period And Source Group data
C                 for EVENT
C
C        PROGRAMMER: Jeff Wang, Roger Brode
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
C        OUTPUTS: Event Name, Group ID, Average Period, Date
C
C        CALLED FROM:   EVCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: ISDX, IMN, IDY, IEVYR2, IEVYR4
      CHARACTER USEVN*10
      LOGICAL FOUND

C     Variable Initializations
      MODNAM = 'EVPER'
      FOUND = .FALSE.

      IF (IEVENT .GT. NEVE) THEN
C        WRITE Error Message    ! Too Many Events Specified
C        This shouldn't occur since limits are dynamically allocated
         WRITE(DUMMY,'(''NEVE='',I7)') NEVE
         CALL ERRHDL(PATH,MODNAM,'E','290',DUMMY)
         GO TO 999
      END IF

C     Check Whether There Are Enough Parameter Fields
      IF (IFC .EQ. 2) THEN
C        Error Message: Missing Parameter
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 7) THEN
C        Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 7) THEN
C        Error Message: Too Many Parameters
C        Note That FIELD(7) Is Used To Hold Concentration 
C        Value for Events Generated From initial run, and
C        these concentrations are also used during Event 
C        processing to check for consistency of results.
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     READ EVNAME, AVEPER, GRPID, DATE

C     Get The Event Name
      USEVN = FIELD(3)
C     Check for Previous EVNAME
      CALL SINDEX(EVNAME,NEVE,USEVN,ISDX,FOUND)
      IF (.NOT.FOUND) THEN
         EVNAME(IEVENT) = USEVN
      ELSE
C        Error Message: Duplicate EVNAME
         CALL ERRHDL(PATH,MODNAM,'E','313',EVNAME(ISDX))
         GO TO 999
      END IF

C     Get Averaging Period For The Event
      CALL STONUM(FIELD(4),ILEN_FLD,FNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      ELSE
         EVAPER(IEVENT) = NINT(FNUM)
      END IF

C     Check for Valid Averaging Period
      DO IAVE = 1, NUMAVE
         IF (EVAPER(IEVENT) .EQ. KAVE(IAVE)) THEN
            FOUND = .TRUE.
            IF (EVAPER(IEVENT) .GT. 24) THEN
C              Write Error Message for Invalid Averaging Period, Must be <=24
               CALL ERRHDL(PATH,MODNAM,'E','297',EVNAME(IEVENT))
            END IF
         END IF
      END DO
      IF (.NOT. FOUND) THEN
C        Error Message: Averaging Period Does Not Match
         CALL ERRHDL(PATH,MODNAM,'E','203','AVEPER')
      END IF

C     Take The Group ID
      EVGRP(IEVENT) = FIELD(5)

C     Retrieve The Index of The Group Array
      FOUND = .FALSE.
      CALL SINDEX(GRPID,NGRP,EVGRP(IEVENT),ISDX,FOUND)
      IF (.NOT. FOUND) THEN
C        Error Message: Group ID Does Not Match
         CALL ERRHDL(PATH,MODNAM,'E','203','GROUPID')
      ELSE
         IDXEV(IEVENT) = ISDX
      END IF

C     Get The Date Of The Event -
C     First Convert Character String to Double Precision Real
      CALL STODBL(FIELD(6),ILEN_FLD,DNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      ELSE
C        Note - EVDATE is an Integer Array
         EVDATE(IEVENT) = IDNINT(DNUM)
C        Extract 2-digit year from event date
         IEVYR2 = IDNINT(DNUM/1000000.D0)
C        Convert to 4-digit year
         IF (IEVYR2 .GE. ISTRT_WIND .AND. IEVYR2 .LE. 99) THEN
            IEVYR4 = ISTRT_CENT*100 + IEVYR2
         ELSE IF (IEVYR2 .LT. ISTRT_WIND) THEN
            IEVYR4 = (ISTRT_CENT+1)*100 + IEVYR2
         END IF
         IMN = IDNINT(DNUM/10000.D0) - IDNINT(DNUM/1000000.D0)*100
         IDY = IDNINT(DNUM/100.D0) - IDNINT(DNUM/10000.D0)*100
         CALL JULIAN(IEVYR4,IMN,IDY,JDAY)
         IF (JDAY .GE. 1 .AND. JDAY .LE. 366) THEN
            IPROC(JDAY) = 1
            EVJDAY(IEVENT) = JDAY
         ELSE
C           WRITE Error Message    ! Invalid Julian Day
            CALL ERRHDL(PATH,MODNAM,'E','203','Juli Day')
            GO TO 999
         END IF
      END IF

C     Get The Original Modeled Concentration Of The Event -
C     First Convert Character String to Double Precision Real
      CALL STODBL(FIELD(7),ILEN_FLD,DNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      ELSE
C        Save Original Modeled Concentration for QA Purposes
         EV_OrigConc(IEVENT) = DNUM
      END IF

      IEVENT = IEVENT + 1

 999  RETURN
      END

      SUBROUTINE EVLOC
C***********************************************************************
C                 EVLOC Module of AERMOD Model
C
C        PURPOSE: Processes Receptor Location Inputs for Events
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Event Name, AXR, AYR, AZELEV, AZFLAG of the Event
C
C        CALLED FROM:   EVCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      USE BUOYANT_LINE
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: ISDX
      DOUBLE PRECISION :: SETAXR, SETAYR
      CHARACTER USEVN*10, IDNAM1*4, IDNAM2*4
      LOGICAL FOUND

C     Variable Initializations
      MODNAM = 'EVLOC'

C     Check Whether There Are Enough Parameter Fields
      IF (IFC .EQ. 2) THEN
C        Error Message: Missing Parameter
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 8) THEN
C        Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 10) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     READ Event Name, XCOOR,YCOOR,ELEV,FLAG And Assign to Different Array
      USEVN = FIELD(3)
C     Check for Previous EVNAME
      CALL SINDEX(EVNAME,NEVE,USEVN,ISDX,FOUND)
      IF (.NOT.FOUND) THEN
C        Error Message: EVNAME Does Not Match
         CALL ERRHDL(PATH,MODNAM,'E','203','EVNAME')
         GO TO 999
      END IF

      IDNAM1 = FIELD(4)

      CALL STODBL(FIELD(5),ILEN_FLD,DNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
C        Assign value of 0.0, but continue checking all fields
         SETAXR = 0.0D0
      ELSE
         SETAXR = DNUM
      END IF

      IDNAM2 = FIELD(6)

      CALL STODBL(FIELD(7),ILEN_FLD,DNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
C        Assign value of 0.0, but continue checking all fields
         SETAYR = 0.0D0
      ELSE
         SETAYR = DNUM
      END IF

      IF (IFC .GE. 8) THEN
         CALL STODBL(FIELD(8),ILEN_FLD,DNUM,IMIT)
C        Check The Numerical Field
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         ELSE
            AZELEV(ISDX) = DNUM
         END IF
         CALL STODBL(FIELD(9),ILEN_FLD,DNUM,IMIT)
C        Check The Numerical Field
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         ELSE
            AZHILL(ISDX) = DNUM
         END IF
      ELSE
         AZELEV(ISDX) = 0.0D0
         AZHILL(ISDX) = 0.0D0
      END IF

      IF (IFC .EQ. 10) THEN
         CALL STODBL(FIELD(10),ILEN_FLD,DNUM,IMIT)
C        Check The Numerical Field
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         ELSE
            AZFLAG(ISDX) = DNUM
         END IF
      ELSE
         AZFLAG(ISDX) = 0.0D0
      END IF

      IF (IDNAM1.EQ.'XR=' .AND. IDNAM2.EQ.'YR=') THEN
         AXR(ISDX) = SETAXR
         AYR(ISDX) = SETAYR
      ELSE IF (IDNAM1.EQ.'RNG=' .AND. IDNAM2.EQ.'DIR=') THEN
         AXR(ISDX) = SETAXR*DSIN(SETAYR*DTORAD)
         AYR(ISDX) = SETAXR*DCOS(SETAYR*DTORAD)
      ELSE
C        Write Error Message: Illegal Parameter
         CALL ERRHDL(PATH,MODNAM,'E','203','REC-TYPE')
      END IF

 999  RETURN
      END

      SUBROUTINE EV_SETUP
C***********************************************************************
C                 EV_SETUP Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Controls Processing of Run SETUP Information
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C        MODIFIED BY D. Strimaitis, SRC (for GRIDDED TERRAIN Processing)
C
C        MODIFIED:   To remove reference to obsolete TG pathway inherited
C                    from ISCST3 code.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
C
C        MODIFIED:   Moved the code to insert a blank line in temporary event
C                    file after each pathway from SUB EVEFIL.
C                    R.W. Brode, PES, Inc. - November 15, 1995.
C
C        MODIFIED:  Default format for METFRM modified to eliminate the
C                   variable ZDM on input.
C                   BY:  J. Paumier, PES              DATE: 27 July 1994
C
C        DATE:    December 15, 1993
C
C        INPUTS:  Input Runstream File
C
C        OUTPUTS: Processing Option Switches
C                 Arrays of Source Parameters
C                 Arrays of Receptor Locations
C                 Meteorological Data Specifications
C                 Terrain Grid Data Specifications
C                 Output Options
C
C        CALLED FROM:   MAIN
C***********************************************************************
C
C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: NOPS, ILEN

      INTEGER :: I, IFSTAT
      LOGICAL NOPATH, NOKEY
      CHARACTER RDFRM*20
C JAT 06/22/21 D065 REMOVE INPFLD AS UNUSED VARIABLE
C      CHARACTER INPFLD*2, PATHWY(6)*2
      CHARACTER PATHWY(6)*2
      INTERFACE
         SUBROUTINE EXPATH(INPFLD,PATHWY,IPN,NOPATH)
            CHARACTER (LEN=2), INTENT(IN) :: INPFLD
            CHARACTER (LEN=2), INTENT(IN), DIMENSION(:) :: PATHWY
            INTEGER, INTENT(IN) :: IPN
            LOGICAL, INTENT(OUT) :: NOPATH
         END SUBROUTINE EXPATH
      END INTERFACE


C     Variable Initializations
      MODNAM = 'EV_SETUP'
      EOF = .FALSE.
      ILINE  = 0
      IQLINE = 0

C     Initialize PATHWY array
      PATHWY(1) = 'CO'
      PATHWY(2) = 'SO'
      PATHWY(3) = 'ME'
      PATHWY(4) = 'EV'
      PATHWY(5) = 'OU'
      PATHWY(6) = '**'

C     Setup READ format and ECHO format for runstream record,
C     based on the ISTRG PARAMETER (set in MAIN1)
      WRITE(RDFRM,9100) ISTRG, ISTRG
 9100 FORMAT('(A',I4.4,',T1,',I4.4,'A1)')

C     LOOP Through Input Runstream Records
      DO WHILE (.NOT. EOF)

C        Increment the Line Counter
         ILINE = ILINE + 1

C        READ Record to Buffers, as A'num' and 'num'A1, where 'num' = ISTRG
C        Length of ISTRG is Set in PARAMETER Statement in MAIN1
         READ (INUNIT,RDFRM,END=999) RUNST1, (RUNST(I), I = 1, ISTRG)

C        Check for blank input record and cycle, but echo to output file
         IF (LEN_TRIM(RUNST1) .EQ. 0) THEN
            WRITE(IOUNIT,*)
            CYCLE
         END IF

C        Convert Lower Case to Upper Case Letters           ---   CALL LWRUPR
         CALL LWRUPR

C        Define Fields on Card                              ---   CALL DEFINE
         CALL DEFINE

C        Get the Contents of the Fields                     ---   CALL GETFLD
         CALL GETFLD

         IF (ECHO .AND.
     &            (FIELD(1).EQ.'OU' .AND. FIELD(2).EQ.'FINISHED')) THEN
C           Echo Last Input Card to Output File (Use Character Substring to
C           Avoid Echoing ^Z Which May Appear at "End of File" for Some
C           Editors).  Also, Allow for Shift in the Input Runstream File of
C           Up to 3 Columns.
            IF (LOCB(1) .EQ. 1) THEN
               WRITE(IOUNIT,9200) RUNST1(1:11)
 9200          FORMAT(A11)
            ELSE IF (LOCB(1) .EQ. 2) THEN
               WRITE(IOUNIT,9210) RUNST1(1:12)
 9210          FORMAT(A12)
            ELSE IF (LOCB(1) .EQ. 3) THEN
               WRITE(IOUNIT,9220) RUNST1(1:13)
 9220          FORMAT(A13)
            ELSE IF (LOCB(1) .EQ. 4) THEN
               WRITE(IOUNIT,9230) RUNST1(1:14)
 9230          FORMAT(A14)
            END IF
         ELSE IF (ECHO) THEN
C           Echo Full Input Card to Output File
            WRITE(IOUNIT,'(a:)') RUNST1(1:LEN_TRIM(RUNST1))
         END IF

C        Check for 'NO ECHO' In First Two Fields
         IF (FIELD(1) .EQ. 'NO' .AND. FIELD(2) .EQ. 'ECHO') THEN
            ECHO = .FALSE.
            CYCLE
         END IF

C        Extract Pathway ID From Field 1                    ---   CALL EXPATH
         CALL EXPATH(FIELD(1),PATHWY,6,NOPATH)

C        For Invalid Pathway and Comment Lines Skip to Next Record
         IF (NOPATH) THEN
C           WRITE Error Message    ! Invalid Pathway ID
            CALL ERRHDL(PPATH,MODNAM,'E','100',PATH)
            PATH = PPATH
            CYCLE
         ELSE IF (PATH .EQ. '**') THEN
C ---       CYCLE to next runstream record
            CYCLE
         END IF

C        Extract Keyword From Field 2                       ---   CALL EXKEY
         CALL EXKEY(FIELD(2),NOKEY)

         IF (NOKEY) THEN
C           WRITE Error Message    ! Invalid Keyword
            CALL ERRHDL(PATH,MODNAM,'E','105',KEYWRD)
            PKEYWD = KEYWRD
            CYCLE
         END IF

C        Check for Proper Order of Setup Cards              ---   CALL SETORD
         CALL EV_SETORD

C        Process Input Card Based on Pathway
         IF (PATH .EQ. 'CO') THEN
C           Process COntrol Pathway Cards                   ---   CALL COCARD
            CALL COCARD
         ELSE IF (PATH .EQ. 'SO') THEN
C           Process SOurce Pathway Cards                    ---   CALL SOCARD
            CALL SOCARD
         ELSE IF (PATH .EQ. 'ME') THEN
C           Process MEteorology Pathway Cards               ---   CALL MECARD
            CALL MECARD
         ELSE IF (PATH .EQ. 'EV') THEN
C           Process EVent Pathway Cards                     ---   CALL EVCARD
            CALL EVCARD

C ---       Create a character string that includes only those modeling
C           options (MODOPS) that are applicable for this model run
            MODOPS_String = ''
            NOPS = 0
     
            DO I = 1, 30
               IF (LEN_TRIM(MODOPS(I)) .GT. 0) THEN
                  NOPS = NOPS + 1
                  ILEN = 10*(NOPS-1)
                  MODOPS_String = MODOPS_String(1:ILEN)//' '//MODOPS(I)
               END IF
            END DO

         ELSE IF (PATH .EQ. 'OU') THEN
C           Process OUtput Pathway Cards                    ---   CALL OUCARD
            CALL EV_OUCARD
C           Check for 'OU FINISHED' Card.  Exit DO WHILE Loop By Branching
C           to Statement 999 in Order to Avoid Reading a ^Z "End of File"
C           Marker That May Be Present For Some Editors.
            IF (PATH .EQ. 'OU' .AND. KEYWRD .EQ. 'FINISHED') THEN
               GO TO 999
            END IF
         END IF

C        Store the Current Keyword as the Previous Keyword
         PKEYWD = KEYWRD

         GO TO 11
 999     EOF = .TRUE.
 11      CONTINUE
      END DO

C     Reinitialize Line Number Counter to Count Meteorology Data
      ILINE = 0

C     Check That All Pathways Were Finished
      IF (ICSTAT(50).NE.1 .OR. ISSTAT(50).NE.1 .OR. IMSTAT(50).NE.1 .OR.
     &    IESTAT(50).NE.1 .OR. IOSTAT(50).NE.1) THEN
C        Runstream File Incomplete, Save I?STAT to IFSTAT and Write Message
         IFSTAT = ICSTAT(50)*10000 + ISSTAT(50)*1000 + IMSTAT(50)*100 +
     &            IESTAT(50)*10 + IOSTAT(50)
         WRITE(DUMMY,'(I5.5)') IFSTAT
         CALL ERRHDL(PATH,MODNAM,'E','125',DUMMY)
         IF (IFSTAT .EQ. 0) THEN
            WRITE(IOUNIT,9990)
 9990       FORMAT(/1X,'All AERMOD input pathways missing! ',
     &                 'Processing aborted!!!')
            STOP
         END IF
      END IF

C --- Check for non-DFAULT options for "optimized" area source,
C     FASTAREA, or for all source types, FASTALL; set MAXDIST = 80KM
C     if FASTALL or FASTAREA, otherwise MAXDIST = 1.0D20
      IF (FASTALL .OR. FASTAREA) THEN
         MAXDIST = 8.0D04
      ELSE
         MAXDIST = 1.0D20
      END IF

      RETURN
      END

      SUBROUTINE EV_SETORD
C***********************************************************************
C                 EV_SETORD Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: To Check Run Stream Setup Images for Proper
C                 Order
C
C        MODIFIED:   To allow for skipping of TG pathway if no terrain
C                    grid is used.  Roger Brode, PES, Inc. - 11/7/94
C
C        INPUTS:  Input Runstream Card Image
C
C        OUTPUTS: Status Settings and Error Messages
C
C        CALLED FROM:   SETUP
C***********************************************************************
C
C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'EV_SETORD'

      IF (KEYWRD .EQ. 'STARTING') THEN
         IF (ISTART .OR. .NOT.IFINIS) THEN
C           WRITE Error Message: Starting Out of Order
            CALL ERRHDL(PPATH,MODNAM,'E','119',PATH)
         ELSE IF (IPNUM .NE. IPPNUM+1) THEN
C           WRITE Error Message: Pathway Out of Order
            CALL ERRHDL(PPATH,MODNAM,'E','120',PATH)
         END IF
C        Set Starting Indicator
         ISTART = .TRUE.
C        Set Finished Indicator
         IFINIS = .FALSE.
      ELSE IF (KEYWRD .EQ. 'FINISHED') THEN
         IF (IFINIS .OR. .NOT.ISTART) THEN
C           WRITE Error Message: Finished Out of Order
            CALL ERRHDL(PPATH,MODNAM,'E','119',PATH)
         ELSE IF (ISTART .AND. PATH.NE.PPATH) THEN
C           WRITE Warning Message: Pathway Out of Order
            CALL ERRHDL(PPATH,MODNAM,'E','120',PATH)
         END IF
C        Reset Starting Indicator
         ISTART = .FALSE.
C        Set Finished Indicator
         IFINIS = .TRUE.
      ELSE IF (.NOT.ISTART .OR. IFINIS) THEN
C        WRITE Error Message: Starting or Finished Out of Order
         CALL ERRHDL(PPATH,MODNAM,'E','119',PATH)
      ELSE IF (ISTART .AND. PATH.NE.PPATH) THEN
C        WRITE Warning Message: Pathway Out of Order
         CALL ERRHDL(PPATH,MODNAM,'E','120',PATH)
      END IF

C     Save Current Path and Path Number as Previous Path and Number
      PPATH = PATH
      IPPNUM = IPNUM

      RETURN
      END

      SUBROUTINE EV_OUCARD
C***********************************************************************
C                 EV_OUCARD Module of the AMS/EPA Regulatory Model - AERMOD - EVENT
C
C        PURPOSE: To process OUtput Pathway card images
C
C        PROGRAMMER: Jeff Wang
C
C        DATE:    March 2, 1992
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
      MODNAM = 'EV_OUCARD'

      IF (KEYWRD .EQ. 'STARTING') THEN
C        Set Status Switch
         IOSTAT(1) = IOSTAT(1) + 1
         IF (IOSTAT(1) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         END IF
      ELSE IF (KEYWRD .EQ. 'EVENTOUT') THEN
C        Process EVENT Output File Option                ---   CALL OEVENT
         CALL OEVENT
C        Set Status Switch
         IOSTAT(2) = IOSTAT(2) + 1
         IF (IOSTAT(2) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         END IF
      ELSE IF (KEYWRD .EQ. 'FILEFORM') THEN
C        Process FILEFORM Output Option                  ---   CALL FILEFORM
         CALL FILEFORM
C        Set Status Switch
         IOSTAT(13) = IOSTAT(13) + 1
         IF (IOSTAT(13) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         END IF
      ELSE IF (KEYWRD .EQ. 'FINISHED') THEN
C        Set Status Switch
         IOSTAT(50) = IOSTAT(50) + 1
C        Check If Missing Mandatory Keyword
         IF (IOSTAT(1) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','STARTING')
         END IF
         IF (IOSTAT(2) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','EVENTOUT')
         END IF
         IF (IOSTAT(50) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         END IF
      ELSE
C        Write Error Message:  Invalid Keyword for This Pathway
         CALL ERRHDL(PATH,MODNAM,'E','110',KEYWRD)
      END IF

      RETURN
      END

      SUBROUTINE OEVENT
C***********************************************************************
C                 OEVENT Module of the AMS/EPA Regulatory Model - AERMOD - EVENT
C
C        PURPOSE: To Process EVENT File Output Selections
C
C        PROGRAMMER: Jeff Wang
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

      CHARACTER OPTION*6

C     Variable Initializations
      MODNAM = 'OEVENT'

C     Check If Enough Fields
      IF (IFC .EQ. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 3) THEN
C        Error Message: Too Many Fields
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Assign Variable of EVENTOUT
      OPTION = FIELD(3)
      IF (OPTION .EQ. 'SOCONT') THEN
         SOCONT = .TRUE.
      ELSE IF (OPTION .EQ. 'DETAIL') THEN
         DETAIL = .TRUE.
      ELSE
C        WRITE Error Message:  Invalid Parameter Field
         CALL ERRHDL(PATH,MODNAM,'E','203',KEYWRD)
      END IF

 999  RETURN
      END

      SUBROUTINE BLEVRECP (IEV,KK)
C***********************************************************************
C                 BLEVRECP Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Performs a translation and initial rotation of the 
C                 event receptor when buoyant line sources are processed
C
C        PROGRAMMER: Amec Foster Wheeler
C
C        DATE:    March 31, 2016
C
C        MODIFIED: Multiple_BuoyLines_D41_Wood: December 1, 2019
C                  To process multiple buoyant line sources

C        INPUTS:  Receptor locations, translation and rotation parameters
C                 IEV = 
C                 KK = Bouyant line source group number
C
C        OUTPUTS: Translated and rotated receptors with flag set
C                 indicating if a receptor is inside or outside the 
C                 bounding buoyant line source rectangle 
C
C        CALLED FROM:   EVCALC
C***********************************************************************
C ---    If there is a buoyant line source, the event receptor(s) are
C          translated and rotated so they are oriented the same as the
C          translation and rotation for the buoyant line source - use
C          the buoyant line source endpoints to determine translation
C          and rotation
C          (NOTE: this is an intial rotation based on the orientation of
C           buoyant line source relative to the x-y axes (with +y
C           pointing north); the receptors will be rotated again each
C           hour based on the wind direction)
C           XOR, YOR, COSTCOR, and SINTCOR are calculated in BL_ROTATE1
C           for the sources and are in the BUOYANT_LINE module
C        An assumption here is that the SOurce pathway is processed
C           before the REceptor pathway.
C
C        BL_RFLAG indicates if a receptor is inside (.true.) or outside
C         (.false.) of the rectangle defined by the minimum and maximum
C         extents of the translation/first rotation of the buoyant line
C
C     Variable Declarations
      USE MAIN1
      USE BUOYANT_LINE
      IMPLICIT NONE
C JAT 06/22/21 D065 REMOVE NVRECT AS UNUSED VARIABLE
C JAT 7/22/21 D065 REMOVE NRECIN
C      INTEGER :: IEV, KK, NRECIN, LNUM, NVRECT
      INTEGER :: IEV, KK, LNUM 
      DOUBLE PRECISION :: EX,EY, XLMIN, XLMAX, YLMIN, YLMAX
C      DOUBLE PRECISION :: BLREC_X(4), BLREC_Y(4)
      CHARACTER MODNAM*12
C Unused: INTEGER :: ILSAVE

C     Variable Initializations
      MODNAM = 'BLEVRECEP'
C JAT 7/22/21 D065 REMOVE NRECIN
C      NRECIN = 0

      IF (NBLTOTAL. GE. 1) THEN
C        Translate event receptor
         XR_SCS(1,KK) = AXR(IEV) - XOR(KK)
         YR_SCS(1,KK) = AYR(IEV) - YOR(KK)
         EX = XR_SCS(1,KK)
         EY = YR_SCS(1,KK)
C        Rotate event receptor
         EY = -EX*SINTCOR(KK) + EY*COSTCOR(KK)
         YR_SCS(1,KK) = EY
         EX = (EX + EY*SINTCOR(KK))/COSTCOR(KK)
         XR_SCS(1,KK) = EX

C ---    Determine the min, max extents of the bouyant line source
C         after translation and first rotation
C         These are used to determine if a receptor is inside the
C         'footprint' of the buoyant lines and excluded from the
C         calculations by setting a flag for each receptor
C ---    Note that the code to determine the bounding rectangle is 
C         executed each time the event receptor is translated and rotated;
C         better placement of the code would eliminate this redundancy

C Multiple_BuoyLines_D41_Wood
C     Determine the exclusion zone by BL group
         DO LNUM = 1,NBLTOTAL
            IF (BLINEPARMS(LNUM)%IBLPGRPNUM == KK) THEN
               YLMAX = YS_SCS(LNUM)
               XLMAX = BLINEPARMS(LNUM)%XEND_TR1
               XLMIN = BLINEPARMS(LNUM)%XBEG_TR1
               EXIT
            END IF
         END DO
         
         DO LNUM = NBLTOTAL, 1, -1
            IF (BLINEPARMS(LNUM)%IBLPGRPNUM == KK) THEN
               YLMIN = YS_SCS(LNUM)
            END IF
         END DO

         DO LNUM = 1,NBLTOTAL
            IF (BLINEPARMS(LNUM)%IBLPGRPNUM == KK) THEN
               XLMIN = MIN(XLMIN,BLINEPARMS(LNUM)%XBEG_TR1)
               XLMAX = MAX(XLMAX,BLINEPARMS(LNUM)%XEND_TR1)
               YLMIN = MIN(YLMIN,YS_SCS(LNUM))
               YLMAX = MAX(YLMAX,YS_SCS(LNUM))
           END IF
         END DO

C Multiple_BuoyLines_D41_Wood
C        NOT NEEDED - commented out for now
C        Define the vertices of the rectangle
C         NVRECT = 4
C         BLREC_X(1) = XLMIN
C         BLREC_X(2) = XLMAX
C         BLREC_X(3) = XLMAX
C         BLREC_X(4) = XLMIN
C         BLREC_Y(1) = YLMIN
C         BLREC_Y(2) = YLMIN
C         BLREC_Y(3) = YLMAX
C         BLREC_Y(4) = YLMAX

C ---    Check to be certain the event receptor is not inside the 
C         bounding rectangle defined by the extents of the buoyant line 
C         source; write a message if the receptor is inside the rectangle;
C         this should not be the case if the event was generated by 
C         an AERMOD run

C Multiple_BuoyLines_D41_Wood
C        Second dimension added for multiple buoyant line processing
         IF( YR_SCS(1,KK) .LE. (YLMAX + 0.1D0) .AND. 
     &       YR_SCS(1,KK) .GE. (YLMIN - 0.1D0) .AND.
     &       XR_SCS(1,KK) .LE. (XLMAX + 0.1D0) .AND. 
     &       XR_SCS(1,KK) .GE. (XLMIN - 0.1D0)) THEN
            BL_RFLAG(1,KK) = .true.
            WRITE(DUMMY,'(A8," ",I3)') trim(BL_GRPID(KK)),IEV
            CALL ERRHDL(PATH,MODNAM,'W','477',DUMMY)
         END IF
      END IF
      RETURN
      END