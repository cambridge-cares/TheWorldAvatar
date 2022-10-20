      SUBROUTINE SETUP
C***********************************************************************
C                 SETUP Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Controls Processing of Run SETUP Information
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        MODIFIED BY D. Strimaitis, SRC (for GRIDDED TERRAIN Processing)
C
C        MODIFIED:   Removed '1X' from format statements for echoing 
C                    the runstream inputs to the output file.  This
C                    was needed when Fortan carriage-control was 
C                    invoked, which is no longer the case based on 
C                    modifications to subroutine HEADER. 
C                    R.W. Brode, USEPA/OAQPS/AQMG, October 19, 2009
C
C        MODIFIED:   To remove reference to obsolete TG pathway inherited
C                    from ISCST3 code.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
C
C        MODIFIED:   Determine final settings for DRYDPLT and WETDPLT;
C                    reassign as .FALSE. if no deposition calculations
C                    are invoked.  Modify MODOPS header accordingly.
C                    R.W. Brode, MACTEC/PES, Inc. - 10/26/2004
C
C        MODIFIED:   Moved the code to insert a blank line in temporary event
C                    file after each pathway from SUB EVEFIL.
C                    R.W. Brode, PES, Inc. - November 15, 1995.
C
C        MODIFIED:   Default format for METFRM modified to eliminate the
C                    variable ZDM on input.
C                    BY:  J. Paumier, PES              DATE: 27 July 1994
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
C     JAT D065 8/9/21 NOPS AND ILEN SET BUT NOT USED
C      INTEGER :: NOPS, ILEN
C     JAT D065 8/9/21 MOD_LEN SET BUT NOT USED
C      INTEGER :: MOD_Len

      INTEGER :: I, IFSTAT
      LOGICAL NOPATH, NOKEY
      LOGICAL NOCOMMENTECHO
      CHARACTER RDFRM*20
C JAT 06/22/21 D065
C REMOVE INPFLD AS UNUSED VARIABLE
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
      MODNAM = 'SETUP'
      PATH  = '  '
      PPATH = '  '
C     JAT D065 8/9/21 MOD_LEN SET BUT NOT USED
C      MOD_Len = 0
      EOF = .FALSE.
      NOCOMMENTECHO = .FALSE.

C     Initialize line counters: ILINE for met file; IQLINE for HOUREMIS file; IOLINE for OZONEFIL;
C     INOXLINE for NOX_FILE
      ILINE  = 0
      IQLINE = 0
      IOLINE = 0
      IBLINE = 0
      INOXLINE = 0
C     JAT D065 8/9/21 NOPS SET BUT NOT USED
C      NOPS = 0
C     JAT D065 8/9/21 ILEN SET BUT NOT USED
C      ILEN = 0
C --- Initialize counters for arrays to store met data for MAXDCONT option
      IHR_NDX = 0
      IYR_NDX = 0

C     Initialize PATHWY array
      PATHWY(1) = 'CO'
      PATHWY(2) = 'SO'
      PATHWY(3) = 'RE'
      PATHWY(4) = 'ME'
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

C        Check for blank input record; echo the blank record to the 
C        output file and the Temporary Event File and then cycle to
C        the next record; unless PATH = 'RE' for EVENT
         IF (LEN_TRIM(RUNST1) .EQ. 0) THEN
            WRITE(IOUNIT,*)
            IF (PATH .NE. 'RE' .AND. PATH .NE. 'OU') THEN
C              Skip echo of blank field to Event file for RE pathway
               WRITE(ITEVUT,*)
            END IF
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
         ELSE IF (PATH .EQ. 'RE') THEN
            NOCOMMENTECHO = .TRUE.
         ELSE IF (PATH .EQ. 'ME') THEN
            NOCOMMENTECHO = .FALSE.
         ELSE IF (PATH .EQ. 'OU') THEN
            NOCOMMENTECHO = .TRUE.
         END IF

         IF (PATH .EQ. '**') THEN
            IF (NOCOMMENTECHO) THEN
C ---          Skip echo to temporary event file and cycle
               CYCLE
            ELSE
C ---          "Echo" the comment record to the Temporary Event File
C              and then CYCLE to the next record
               WRITE(ITEVUT,'(a:)') RUNST1(1:LEN_TRIM(RUNST1))
               CYCLE
            END IF
         END IF

C        Extract Keyword From Field 2                       ---   CALL EXKEY
         CALL EXKEY(FIELD(2),NOKEY)

C Multiple_BuoyLines_D41_Wood
C        Removed logic that BLPGROUP was not operational in v19191
         IF (NOKEY) THEN
C           WRITE Error Message    ! Invalid Keyword
            CALL ERRHDL(PATH,MODNAM,'E','105',KEYWRD)
            PKEYWD = KEYWRD
            CYCLE
         END IF

C        Check for Proper Order of Setup Cards              ---   CALL SETORD
         CALL SETORD

C        Process Input Card Based on Pathway
         IF (PATH .EQ. 'CO') THEN
C           Process COntrol Pathway Cards                   ---   CALL COCARD
            CALL COCARD
C ---       Echo Runstream Image to Temporary Event File (Except EVENTFIL, 
C           SAVEFILE, INITFILE & MULTYEAR)
            IF (KEYWRD.NE.'EVENTFIL' .AND. KEYWRD.NE.'SAVEFILE' .AND. 
     &          KEYWRD.NE.'INITFILE' .AND. KEYWRD.NE.'MULTYEAR') THEN
               WRITE(ITEVUT,'(a:)') RUNST1(1:LEN_TRIM(RUNST1))
            END IF
         ELSE IF (PATH .EQ. 'SO') THEN
C           Echo Runstream Image to Temporary Event File
            WRITE(ITEVUT,'(a)') RUNST1(1:LEN_TRIM(RUNST1))
C           Process SOurce Pathway Cards                    ---   CALL SOCARD
            CALL SOCARD
         ELSE IF (PATH .EQ. 'RE') THEN
C           Process REceptor Pathway Cards                  ---   CALL RECARD
            CALL RECARD
         ELSE IF (PATH .EQ. 'ME') THEN
C           Process MEteorology Pathway Cards               ---   CALL MECARD
            CALL MECARD
C           Echo Runstream Image to Temporary Event File (Except STARTEND
C           & DAYRANGE)
            IF (KEYWRD.NE.'STARTEND' .AND. KEYWRD.NE.'DAYRANGE') THEN
               WRITE(ITEVUT,'(a:)') RUNST1(1:LEN_TRIM(RUNST1))
            END IF

C ---       Create a character string that includes only those modeling 
C           options (MODOPS) that are applicable for this model run
            MODOPS_String = ''
C         JAT D065 8/9/21 NOPS SET BUT NOT USED
C            NOPS = 0
C     JAT D065 8/9/21 MOD_LEN SET BUT NOT USED
C            MOD_LEN = 0

C ---       Loop through the 30 different options that are flagged
            DO I = 1, 30
               IF (LEN_TRIM(MODOPS(I)) .GT. 0) THEN
C                 JAT D065 8/9/21 MOD_LEN SET BUT NOT USED
C                  MOD_LEN = LEN_TRIM(MODOPS(I))
                  MODOPS_String = 
     &            MODOPS_String(1:LEN_TRIM(MODOPS_String))//'  '//
     &                MODOPS(I)(1:LEN_TRIM(MODOPS(I)))
               END IF
            END DO

         ELSE IF (PATH .EQ. 'OU') THEN
C           Process OUtput Pathway Cards                    ---   CALL OUCARD
            CALL OUCARD
         END IF

C        Store the Current Keyword as the Previous Keyword
         PKEYWD = KEYWRD

C        Check for 'OU FINISHED' Card.  Exit DO WHILE Loop By Branching
C        to Statement 999 in Order to Avoid Reading a ^Z "End of File"
C        Marker That May Be Present For Some Editors.
         IF (PATH .EQ. 'OU' .AND. KEYWRD .EQ. 'FINISHED') THEN
            GO TO 999
         END IF

         GO TO 11
 999     EOF = .TRUE.
 11      CONTINUE
      END DO

C     Reinitialize Line Number Counter to Count Meteorology Data
      ILINE = 0

C     Check That All Pathways Were Finished
      IF (ICSTAT(50).NE.1 .OR. ISSTAT(50).NE.1 .OR. IRSTAT(50).NE.1 .OR.
     &    IMSTAT(50).NE.1 .OR. IOSTAT(50).NE.1) THEN
C        Runstream File Incomplete, Save I?STAT to IFSTAT and Write Message
         IFSTAT = ICSTAT(50)*10000 + ISSTAT(50)*1000 + IRSTAT(50)*100 +
     &            IMSTAT(50)*10 + IOSTAT(50)
         WRITE(DUMMY,'(I5.5)') IFSTAT
         CALL ERRHDL(PATH,MODNAM,'E','125',DUMMY)
C ---    Check for IFSTAT = 0, indicating an empty input file; 
C        issue error message to 'aermod.out' file and abort
         IF (IFSTAT .EQ. 0) THEN
            WRITE(IOUNIT,9990)
 9990       FORMAT(/1X,'All AERMOD input pathways missing! ',
     &                 'Processing aborted!!!')
C ---       Also issue error message to "screen"
            WRITE(*,9990)
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

      SUBROUTINE LWRUPR
C***********************************************************************
C                 LWRUPR Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Transfer All Characters From Lower Case To
C                 Upper Case (Using INDEX Intrinsic Function)
C                 Note that the CHAR*ISTRG RUNST1 Variable Includes
C                 the Original Case for Echoing and for Later Use
C                 To Retrieve Filenames.
C
C        PROGRAMMER: Roger Brode, Kevin Stroupe
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Card Image (ISTRG Character Array)
C                 Number of Characters in String, PARAMETER ISTRG
C
C        OUTPUTS: Input Runstream Card Image (Array) in Uppercase
C
C        CALLED FROM:   SETUP
C***********************************************************************
C
C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, INDCHK
      CHARACTER UPCASE*26
      CHARACTER LWCASE*26

C     Variable Initializations
      DATA UPCASE/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
      DATA LWCASE/'abcdefghijklmnopqrstuvwxyz'/
      MODNAM = 'LWRUPR'
      INDCHK = 0

      DO I = 1, ISTRG
         IF (RUNST(I) .NE. ' ') THEN
            INDCHK = INDEX(LWCASE,RUNST(I))
            IF (INDCHK .NE. 0) THEN
               RUNST(I) = UPCASE(INDCHK:INDCHK)
            END IF
         END IF
      END DO

      RETURN
      END

      SUBROUTINE DEFINE
C***********************************************************************
C                 DEFINE Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Defines Location of Fields on Runstream Input Image
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C*       Revision History:
C*
C*       MODIFIED: October 19, 2009
C*                
C*                 Modified to recognize double quotes (") as 
C*                 field delimiters to allow for filenames with
C*                 embedded spaces.
C
C        INPUTS:   Input Runstream Card Image
C
C        OUTPUTS:  Number of Fields on Card, IFC
C                  Beginning and Ending Columns of Fields, LOCB and LOCE
C
C        CALLED FROM:   SETUP
C***********************************************************************
C

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      
      LOGICAL INQUOTE

      INTEGER :: I

C     Variable Initializations
      MODNAM = 'DEFINE'
      LOCB(3:IFMAX) = 0
      LOCE(3:IFMAX) = 0

C     Initialize the Blank Line and In-field Status Indicators
      BLINE = .TRUE.
      INFLD = .FALSE.
      INQUOTE = .FALSE.

      IF (ILINE .EQ. 1) THEN
C        Define the Starting Column for the Input File In Case File Is Shifted.
C        Allow for Shift of Up to 3 Columns
         LOCB(1) = 0
         IF (RUNST(1) .NE. ' ') THEN
            LOCB(1) = 1
         ELSE IF (RUNST(2) .NE. ' ') THEN
            LOCB(1) = 2
         ELSE IF (RUNST(3) .NE. ' ') THEN
            LOCB(1) = 3
         ELSE IF (RUNST(4) .NE. ' ') THEN
            LOCB(1) = 4
         ELSE
            LOCB(1) = 1
         END IF
         LOCE(1) = LOCB(1) + 1
         LOCB(2) = LOCB(1) + 3
         LOCE(2) = LOCB(1) + 10
      END IF

      IFC = 2

C     Check RUNST1 (full input record string) for Blank Line
      IF (LEN_TRIM(RUNST1) .GT. 0) THEN
         BLINE = .FALSE.
      ELSE
         RETURN
      END IF

C     Loop through the Data Fields
      DO I = LOCB(1)+12, ISTRG

         IF (.NOT.INFLD .AND. RUNST(I).EQ.'"') THEN
C           Location is the Beginning of a Field using "'s
C           Set Mark of not Blank Line
            BLINE = .FALSE.
C           Set Mark of in a Field
            INFLD = .TRUE.
C           Set Mark of in a Quote Field
            INQUOTE = .TRUE.
C           Increment the Field Counter
            IFC = IFC + 1
C           Check for number of fields > IFMAX parameter
            IF (IFC .GT. IFMAX) THEN
C              WRITE Error Message: Too many fields specified
               WRITE(DUMMY,'(I8)') IFMAX
               CALL ERRHDL(PPATH,MODNAM,'E','109',DUMMY)
               EXIT
            END IF
C           Record the Location of Beginning of the Field
            LOCB(IFC) = I + 1
         ELSE IF (.NOT.INFLD .AND. RUNST(I).NE.' ') THEN
C           Location is the Beginning of a Field
C           Set Mark of not Blank Line
            BLINE = .FALSE.
C           Set Mark of in a Field
            INFLD = .TRUE.
C           Increment the Field Counter
            IFC = IFC + 1
C           Check for number of fields > IFMAX parameter
            IF (IFC .GT. IFMAX) THEN
C              WRITE Error Message: Too many fields specified
               WRITE(DUMMY,'(I8)') IFMAX
               CALL ERRHDL(PPATH,MODNAM,'E','109',DUMMY)
               EXIT
            END IF
C           Record the Location of Beginning of the Field
            LOCB(IFC) = I
         ELSE IF (INQUOTE .AND. RUNST(I).EQ.'"') THEN
C           Location is the End of a Field
C           Set Mark of Not In a field
            INFLD = .FALSE.
C           Set Mark of Not in a Quote Field
            INQUOTE = .FALSE.
C           Record the Location of Ending of the Field
            LOCE(IFC) = I - 1
         ELSE IF (.NOT.INQUOTE .AND. INFLD .AND. RUNST(I).EQ.' ') THEN
C           Location is the End of a Field
C           Set Mark of Not In a field
            INFLD = .FALSE.
C           Record the Location of Ending of the Field
            LOCE(IFC) = I - 1
         END IF

C        Check for End of Input String
C        (Length of ISTRG is Set as a PARAMETER in MAIN1)
         IF (INFLD .AND. I.EQ.ISTRG) THEN
            LOCE(IFC) = ISTRG
         END IF

      END DO

      RETURN
      END

      SUBROUTINE GETFLD
C***********************************************************************
C                 GETFLD Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Gets Contents of Fields on Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Card Image
C
C        OUTPUTS: Contents of Fields on Card
C
C        CALLED FROM:   SETUP
C***********************************************************************
C
C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, J
      CHARACTER WRTFRM*20

C     Variable Initializations
      MODNAM = 'GETFLD'
      FIELD(:) = ''

C     Setup WRITE format for internal write to FIELD
C     based on the ILEN_FLD PARAMETER (set in MAIN1)
      WRITE(WRTFRM,9004) ILEN_FLD
 9004 FORMAT('(',I4.4,'(A1:))')

      DO I = 1, IFC
C ---    Skip processing of fields if IFC > IFMAX
         IF (I .GT. IFMAX) EXIT
         IF (LOCE(I)-LOCB(I) .LE. (ILEN_FLD - 1) ) THEN
C           Field Satisfies Limit of ILEN_FLD Characters (set in MAIN1)
            WRITE(FIELD(I),WRTFRM) (RUNST(J),J=LOCB(I),LOCE(I))
         ELSE
C           Field Exceeds ILEN_FLD Character Limit
C           Truncate Field at ILEN_FLD Characters
            WRITE(FIELD(I),WRTFRM) (RUNST(J),J=LOCB(I),
     &                                         LOCB(I)+ILEN_FLD-1)
         END IF
      END DO

      RETURN
      END

      SUBROUTINE EXPATH(INPFLD,PATHWY,IPN,NOPATH)
C***********************************************************************
C                 EXPATH Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Extracts and Verifies Pathway ID from
C                 Runstream Input Card Image
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Card Image
C
C        OUTPUTS: The Extracted Pathway ID
C
C        CALLED FROM:   SETUP
C***********************************************************************
C
C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I
      CHARACTER (LEN=2), INTENT(IN) :: INPFLD
      CHARACTER (LEN=2), INTENT(IN), DIMENSION(:) :: PATHWY
      INTEGER, INTENT(IN) :: IPN
      LOGICAL, INTENT(OUT) :: NOPATH

C     Variable Initializations
      NOPATH = .TRUE.
      MODNAM = 'EXPATH'

C     Begin The Processing
      IF (INPFLD .NE. '  ') THEN
C        Check the Read-in Pathway
         PATH = INPFLD
         DO I = 1, IPN
C           In Case of Match Set NOPATH to FALSE and Set Path Number, IPNUM
            IF (INPFLD .EQ. PATHWY(I)) THEN
               NOPATH = .FALSE.
               IPNUM = I
C              Exit to END
               GO TO 999
            END IF
         END DO
      ELSE
C        In Case of Blank Field Set Pathway to Previous Pathway
         NOPATH = .FALSE.
         PATH   = PPATH
         IPNUM  = IPPNUM
      END IF

 999  RETURN
      END

      SUBROUTINE EXKEY(INPFLD,NOKEY)
C***********************************************************************
C                 EXKEY Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Extracts and Verifies Keyword from
C                 Runstream Input Card Image
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Card Image
C
C        OUTPUTS: The Extracted Keyword
C
C        CALLED FROM:   SETUP
C***********************************************************************
C

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I
      CHARACTER (LEN=8) :: INPFLD
      LOGICAL NOKEY

C     Variable Initializations
      NOKEY  = .TRUE.
      MODNAM = 'EXKEY'

C     Begin The Processing
      IF (INPFLD .NE. '        ') THEN
C        Check the Read-in Keyword
         KEYWRD = INPFLD
         DO I = 1, IKN
C           In Case of Match Set NOKEY to FALSE
            IF (INPFLD .EQ. KEYWD(I)) THEN
               NOKEY = .FALSE.
C              Exit to END
               GO TO 999
            END IF
         END DO
      ELSE IF (PKEYWD .NE. 'STARTING') THEN
C        In Case of Blank Field, Keyword Is Set to Previous Keyword
C        unless previous keyword is 'STARTING'
         NOKEY  = .FALSE.
         KEYWRD = PKEYWD
      ELSE
C        No Keyword is available; keyword field is blank and the 
C        previous keyword is 'STARTING'
         NOKEY  = .TRUE.
         KEYWRD = 'blank   '
      END IF

 999  RETURN
      END

      SUBROUTINE SETORD
C***********************************************************************
C                 SETORD Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: To Check Run Stream Setup Images for Proper
C                 Order
C
C        MODIFIED:   To remove reference to obsolete TG pathway inherited
C                    from ISCST3 code.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
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
      MODNAM = 'SETORD'

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

      SUBROUTINE STONUM(STRVAR,LENGTH,FNUM,IMUTI)
C***********************************************************************
C                 STONUM Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Gets Number From A String Variable
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input String Variable
C                 Length of Character String
C
C        OUTPUTS: Numbers
C
C        CALLED FROM: (This Is A Utility Program)
C***********************************************************************
C
C     Variable Declarations
      IMPLICIT NONE

      CHARACTER STRVAR*(*), CHK, MODNAM*6, NUMS*10
      INTEGER :: I, IMUTI, LENGTH
      REAL    :: FNUM, CNUM, FDEC, FDC1, HEAD
      LOGICAL MEND, IN, NMARK, PMARK, DMARK, MMARK, EMARK

C     Variable Initialization
      MODNAM = 'STONUM'
      NUMS = '0123456789'
      I = 1
      MEND = .FALSE.
      IN = .FALSE.
      NMARK = .FALSE.
      PMARK = .FALSE.
      DMARK = .FALSE.
      MMARK = .FALSE.
      EMARK = .FALSE.
      CNUM  = 0.0
      HEAD  = 0.0
      IMUTI = 1
      FDEC  = 1.

C     Beginning the Processing
      DO WHILE (.NOT.MEND .AND. I.LE.LENGTH)
         CHK = STRVAR(I:I)
         IF (CHK .NE. ' ') THEN
            IN = .TRUE.
            IF (CHK.GE.'0' .AND. CHK.LE.'9') THEN
C              CHK is a Number, Assign a Value
               IF (.NOT. DMARK) THEN
                  CNUM = CNUM*10.+FLOAT(INDEX(NUMS,CHK)-1)
               ELSE
                  FDEC = FDEC/10.
                  FDC1 = FDEC*FLOAT(INDEX(NUMS,CHK)-1)
                  CNUM = CNUM+FDC1
               END IF
            ELSE
C              Handle The E-Type Real Number
               IF (DMARK .AND. .NOT.EMARK .AND. CHK.EQ.'E') THEN
                  EMARK = .TRUE.
                  IF (.NOT.NMARK) THEN
                     HEAD = CNUM
                  ELSE
                     HEAD = -CNUM
                  END IF
                  DMARK = .FALSE.
                  NMARK = .FALSE.
                  CNUM = 0.0
               ELSE IF (.NOT.PMARK .AND. CHK.EQ.'+') THEN
C                 Set Positive Indicator
                  PMARK = .TRUE.
               ELSE IF (.NOT.NMARK .AND. CHK.EQ.'-') THEN
C                 Set Negative Indicator
                  NMARK = .TRUE.
               ELSE IF (.NOT.DMARK .AND. CHK.EQ.'.') THEN
C                 Set Decimal Indicator
                  DMARK = .TRUE.
               ELSE IF (.NOT.MMARK .AND. CHK.EQ.'*' .AND.
     &                  .NOT.NMARK) THEN
C                 Set Repeat Number
                  MMARK = .TRUE.
                  IMUTI = NINT(CNUM)
                  CNUM = 0.0
               ELSE
C                 Error Occurs, Set Switch and Exit Out Of The Subroutine
                  GO TO 9999
               END IF
            END IF
         ELSE IF (IN .AND. CHK.EQ.' ') THEN
            MEND = .TRUE.
         END IF
         I = I + 1
      END DO

      FNUM = CNUM

C     In Case Of Negative Field, Value Set to Negative
      IF (NMARK) THEN
         FNUM = -FNUM
      END IF

C     In Case of E-Format, Check for Exponents Out of Range
      IF (EMARK .AND. ABS(FNUM) .LE. 30.) THEN
         FNUM = HEAD*10.0**(FNUM)
      ELSE IF (EMARK .AND. ABS(FNUM) .GT. 30.) THEN
         IF (FNUM .LT. 0.0) THEN
            FNUM = 0.0
         ELSE IF (FNUM .GT. 0.0) THEN
            FNUM = HEAD * 10.**30.
         END IF
         GO TO 9999
      END IF

      GO TO 1000

C     Set Error Switch for Illegal Numerical Field (WRITE Message and Handle
C     Error in Calling Routine)
 9999 IMUTI = -1

 1000 RETURN
      END

      SUBROUTINE STODBL(STRVAR,LEN,FNUM,IMUTI)
C***********************************************************************
C                 Subroutine STODBL
C
C        PURPOSE: Gets Double Precision of Real Number
C                 From A Character String 
C
C        PROGRAMMER: Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To Change Exponent Limit for Out-of-range
C                    Inputs - 9/29/92
C
C        INPUTS:  Input String Variable
C                 Length of Character String
C
C        OUTPUTS: Double Precision Real Numbers
C
C        CALLED FROM: (This Is A Utility Program)
C***********************************************************************
C
C     Variable Declarations
      IMPLICIT NONE

      CHARACTER STRVAR*(*), CHK, MODNAM*6, NUMS*10
      INTEGER :: IMUTI, LEN, I
      DOUBLE PRECISION :: FDEC, FDC1, HEAD
      DOUBLE PRECISION :: FNUM, CNUM
      LOGICAL MEND, IN, NMARK, PMARK, DMARK, MMARK, EMARK

C     Variable Initialization
      MODNAM = 'STODBL'
      NUMS = '0123456789'
      I = 1
      MEND = .FALSE.
      IN = .FALSE.
      NMARK = .FALSE.
      PMARK = .FALSE.
      DMARK = .FALSE.
      MMARK = .FALSE.
      EMARK = .FALSE.
      CNUM = 0.0D0
      HEAD = 0.0D0
      FDEC = 1.0D0
      IMUTI = 1

C     Beginning the Processing
      DO WHILE (.NOT.MEND .AND. I.LE.LEN)
         CHK = STRVAR(I:I)
         IF (CHK .NE. ' ') THEN
            IN = .TRUE.
            IF (CHK.GE.'0' .AND. CHK.LE.'9') THEN
C              CHK is a Number, Assign a Value
               IF (.NOT. DMARK) THEN
                  CNUM = CNUM*10.0D0+DBLE(INDEX(NUMS,CHK)-1)
               ELSE
                  FDEC = FDEC/10.0D0
                  FDC1 = FDEC*DBLE(INDEX(NUMS,CHK)-1)
                  CNUM = CNUM+FDC1
               END IF
            ELSE
C              Handle The E-Type (or D-Type) Real Number
               IF ((DMARK .AND. .NOT.EMARK .AND. CHK.EQ.'E') .OR.
     &             (DMARK .AND. .NOT.EMARK .AND. CHK.EQ.'D')) THEN
                  EMARK = .TRUE.
                  IF (.NOT.NMARK) THEN
                     HEAD = CNUM
                  ELSE
                     HEAD = -CNUM
                  END IF
                  DMARK = .FALSE.
                  NMARK = .FALSE.
                  CNUM = 0.0D0
               ELSE IF (.NOT.PMARK .AND. CHK.EQ.'+') THEN
C                 Set Positive Indicator
                  PMARK = .TRUE.
               ELSE IF (.NOT.NMARK .AND. CHK.EQ.'-') THEN
C                 Set Negative Indicator
                  NMARK = .TRUE.
               ELSE IF (.NOT.DMARK .AND. CHK.EQ.'.') THEN
C                 Set Decimal Indicator
                  DMARK = .TRUE.
               ELSE IF (.NOT.MMARK .AND. CHK.EQ.'*' .AND.
     &            .NOT.NMARK) THEN
C                 Set Repeat Indicator
                  MMARK = .TRUE.
                  IMUTI = IDNINT(CNUM)
                  CNUM = 0.0D0
               ELSE
C                 Error Occurs, Set Switch and Exit Out Of The Subroutine
                  GO TO 9999
               END IF
            END IF
         ELSE IF (IN .AND. CHK.EQ.' ') THEN
            MEND = .TRUE.
         END IF
         I = I + 1
      END DO

      FNUM = CNUM

C     In Case Of Negative Field, Value set to Negative
      IF (NMARK) THEN
         FNUM = -FNUM
      END IF

C     In Case of *E* Format, Check for Exponents Out of Range
      IF (EMARK .AND. DABS(FNUM) .LE. 30.0D0) THEN
         FNUM = HEAD*10.0D0**(FNUM)
      ELSE IF (EMARK .AND. DABS(FNUM) .GT. 30.0D0) THEN
         IF (FNUM .LT. 0.0D0) THEN
            FNUM = 0.0D0
         ELSE IF (FNUM .GT. 0.0D0) THEN
            FNUM = HEAD * 10.0D0**30.0D0
         END IF
         GO TO 9999
      END IF

      GO TO 1000

C     Set Error Switch for Illegal Numerical Field (WRITE Message and Handle
C     Error in Calling Routine)
 9999 IMUTI = -1

 1000 RETURN
      END

      SUBROUTINE SINDEX(ARRIN,IDIM,ELEM,INDEXS,FOUND)
C***********************************************************************
C                 SINDEX Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Search The Index of An Input Array Element
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Character Element
C
C        OUTPUTS: Index Of This Element in An Array
C
C        CALLED FROM:  (This Is An Utility Programm)
C***********************************************************************
C
C     Variable Declarations
      IMPLICIT NONE

      INTEGER :: I, IDIM, INDEXS
      CHARACTER (LEN=*) :: ARRIN(IDIM), ELEM
      CHARACTER MODNAM*6
      LOGICAL FOUND

C     Variable Initializations
      MODNAM = 'SINDEX'
      FOUND = .FALSE.
      I = 1
      INDEXS = 0

      DO WHILE (.NOT.FOUND .AND. I.LE.IDIM)
         IF (ELEM .EQ. ARRIN(I)) THEN
            FOUND = .TRUE.
            INDEXS = I
         END IF
         I = I + 1
      END DO

      RETURN
      END

      SUBROUTINE FSPLIT(PATHIN,KEYIN,INPFLD,LENGTH,DELIM,LFLAG,
     &                  BEGFLD,ENDFLD)
C***********************************************************************
C                 FSPLIT Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: SPLIT A FIELD, BASED ON AN INPUT DELIMITER
C                 CHARACTER.  SETS A LOGICAL FLAG AND RETURNS
C                 BEGINNING AND ENDING PARTS OF FIELD.
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Pathway for Calling Routine
C                 Keyword for Calling Routine
C                 Input Field Variable
C                 Length of Input Character Field
C                 Delimiter Character
C
C        OUTPUTS: Logical Flag to Indicate Presence of Delimiter
C                 Beginning Part of Field (.LE. 12 Character)
C                 Ending Part of Field (.LE. 12 Character)
C
C        CALLED FROM: (This Is A Utility Program)
C***********************************************************************

C     Variable Declarations
      IMPLICIT NONE

      INTEGER :: I, LENGTH, IDELM
      CHARACTER CHK, INPFLD*(*), BEGFLD*(*), ENDFLD*(*), 
     &          DELIM*1, MODNAM*12, PATHIN*2, KEYIN*8
      LOGICAL LFLAG, MEND, IN

C     Variable Initialization
      MODNAM = 'FSPLIT'
      I = LENGTH
      IDELM = LENGTH
      BEGFLD = ' '
      ENDFLD = ' '
      MEND  = .FALSE.
      IN    = .FALSE.
      LFLAG = .FALSE.

C     Begin the Processing
      DO WHILE (.NOT.MEND .AND. I.GE.1)
         CHK = INPFLD(I:I)
         IF (CHK .NE. ' ') THEN
            IN = .TRUE.
C           Check for the Group Delimiter
            IF (.NOT.LFLAG .AND. CHK.EQ.DELIM) THEN
               LFLAG = .TRUE.
               IDELM = I
               ENDFLD = INPFLD(I+1:LENGTH)
               IF (I .EQ. 1) THEN
C                 Write Error Message for Invalid Range Parameter
                  CALL ERRHDL(PATHIN,MODNAM,'E','203',KEYIN)
                  GO TO 999
               END IF
            ELSE IF (LFLAG .AND. CHK.EQ.DELIM) THEN
C              WRITE Error Message  ! More Than One Delimiter in a Field
               CALL ERRHDL(PATHIN,MODNAM,'E','217',KEYIN)
            END IF
         ELSE IF (IN .AND. CHK.EQ.' ') THEN
            MEND = .TRUE.
            IF (LFLAG) THEN
               BEGFLD = INPFLD(1:IDELM-1)
            ELSE
               BEGFLD = INPFLD
            END IF
         END IF
         I = I - 1
      END DO

      IF (.NOT. MEND) THEN
         IF (LFLAG) THEN
            BEGFLD = INPFLD(1:IDELM-1)
         ELSE
            BEGFLD = INPFLD
         END IF
      END IF

C     In Case Of No Delimiter, Set ENDFLD = BEGFLD
      IF (.NOT. LFLAG) THEN
         ENDFLD = BEGFLD
      END IF

 999  RETURN
      END

      SUBROUTINE VARINI
C***********************************************************************
C                 VARINI Module of the AMS/EPA Regulatory Model - AERMOD
c ----------------------------------------------------------------------
c ---    ISC-PRIME     Version 1.0    Level 970812              Modified
c ---        V. Tino
c ---        Earth Tech, Inc.
c            Prepared for EPRI under contract WO3527-01
c ----------------------------------------------------------------------
C
C        PURPOSE: To Initialize Variables for Setup
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C        MODIFIED BY D. Strimaitis, SRC (for Wet & Dry DEPOSITION, INT.
C                                        TERRAIN, and GRIDDED TERRAIN
C                                        Processing)
C
C        DATE:    December 15, 1993
C
C        MODIFIED:  To remove reference to obsolete TG pathway inherited
C                   from ISCST3 code.
C                   R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
C
C        MODIFIED:  To initialize DDPLETE and WDPLETE to .TRUE. for
C                   dry and wet depletion.
C                   R.W. Brode, MACTEC, Inc. (f/k/a PES, Inc.) - 10/26/04
C
C        MODIFIED:  To use two decimal places for reading temperatures
C                   from the profile file in the default format (PROFRM)
C                   R.W. Brode, PES, Inc. - 8/28/01
C
C        MODIFIED BY D. Strimaitis, SRC (for DEPOSITION)
C        (DATE:    February 15, 1993)
C
C        MODIFIED:  To Include TOXXFILE Option - 9/29/92
C
C        INPUTS:  None
C
C        OUTPUTS: Initialized Variables
C
C        CALLED FROM:   MAIN
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      USE BUOYANT_LINE
      
      IMPLICIT NONE
      CHARACTER MODNAM*12

C Unused: INTEGER :: I, J, K

C     Variable Initializations
      MODNAM = 'VARINI'

C --- Initialize double precision constants based on PI
      PI      = 4.0D0*DATAN(1.0D0)
      TWOPI   = 2.0D0*PI
      RTOFPI  = DSQRT(PI)
      SRT2PI  = DSQRT(TWOPI)
      RTOF2   = DSQRT(2.0D0)
      RTPIBY2 = DSQRT(PI/2.0D0)
      RT2BYPI = DSQRT(2.0D0/PI)
      DTORAD  = PI/180.0D0
      RTODEG  = 180.0D0/PI
      
C --- Initialize constant for 1/3 used as exponent
      THIRD = 1.0D0/3.0D0
      TWOTHIRDS = 2.0D0/3.0D0

C --- Initialize counters to zero
      IPNUM  = 0
      IPPNUM = 0
      NDUMP  = 0
C --- Initialize counter for old met data warning message
      IMETMSG = 0

C --- Initialize pollutant ID, POLLUT*8
      POLLUT = 'undefine'

C     Initialize the Logical Control Variables
      ISTART = .FALSE.
      IFINIS = .TRUE.
      ERRLST = .FALSE.
      DFAULT = .FALSE.
      CONC   = .FALSE.
      DEPOS  = .FALSE.
C     Add logicals to output just wet or just dry deposition fluxes
      DDEP   = .FALSE.
      WDEP   = .FALSE.
      RURAL  = .FALSE.
      URBAN  = .FALSE.
      GRDRIS = .FALSE.
      NOSTD  = .FALSE.
      NOBID  = .FALSE.
      NOWARN = .FALSE.
      MSGPRO = .TRUE.
      CLMPRO = .TRUE.
      PERIOD = .FALSE.
      ANNUAL = .FALSE.
      MONTH  = .FALSE.
      FLAT   = .FALSE.
      ELEV   = .TRUE.
      FLGPOL = .FALSE.
      RUN    = .FALSE.
      EVENTS = .FALSE.
      RSTSAV = .FALSE.
      RSTINP = .FALSE.
      MULTYR = .FALSE.
      DAYTAB = .FALSE.
      MXFILE = .FALSE.
      PPFILE = .FALSE.
      PLFILE = .FALSE.
      SUMMFILE = .FALSE.
C     Add TXFILE Variable for the TOXXFILE Option, 9/29/92
      TXFILE = .FALSE.
      RKFILE = .FALSE.
      ANPOST = .FALSE.
      ANPLOT = .FALSE.
      RECERR = .FALSE.
      PFLERR = .FALSE.
      IF (ALLOCATED(L_MorningTrans)) L_MorningTrans(:) = .FALSE.
      L_UrbanTransition = .TRUE.
      ENDMON = .FALSE.
      CALCS  = .FALSE.
      SURFAC = .FALSE.
      DEBUG  = .FALSE.
      METEORDBG = .FALSE.
      AREADBG  = .FALSE.
      PRIMEDBG = .FALSE.
      OLMDEBUG = .FALSE.
      ARM2DEBUG= .FALSE.
      PVMRMDBG = .FALSE.
      GRSMDEBUG = .FALSE.
      DEPOSDBG = .FALSE.
CCRT  D063 Platform Downwash Debug
      PLATFMDBG  = .FALSE.  
      WAKE   = .FALSE.
      ECHO   = .TRUE.
      SCREEN = .FALSE.
      HOURLY = .FALSE.

      L_BLHOURLY = .FALSE.                                              ! Multiple_BuoyLines_D41_Wood

c     JAT 05/08/20 ADDED FROM 19191
C     Initialized AWMADWDBG to false
      AWMADWDBG=.FALSE.

C     Add logicals to identify use wet and dry removal information
      LDPART  = .FALSE.
      LWPART  = .FALSE.
      LWGAS   = .FALSE.
      LDGAS   = .FALSE.
C     Add logicals to control use of Wet & Dry plume depletion
C     Initialize dry and wet depletion to .F. for now, but if
C     deposition algorithms are used then depletion will be assumed
      DDPLETE   = .FALSE.
      WDPLETE   = .FALSE.
      ARDPLETE  = .FALSE.
C     Initialize logicals for user-specified depletion options
      WETDPLT   = .FALSE.
      DRYDPLT   = .FALSE.
C     Initialize logicals for user-specified override of depletion
      NOWETDPLT = .FALSE.
      NODRYDPLT = .FALSE.

C --- Initialize EVENT processing output options
      SOCONT = .FALSE.
      DETAIL = .FALSE.

C --- Initialize logical for use of vector mean wind speeds, L_VECTORWS
      L_VECTORWS = .FALSE.

C --- Initialize logical for use of ALPHA option for low-wind-speed
C     modifications.
CCRT 3/23/2021 D061/D062: in aermod.f
CCRT  This resets to false after being set to true in aermod.f
CCRT  not needed here
CCRT      LOW_WIND = .FALSE.

CCRT  4/12/2022 Initialize logical variables for LOW_WIND options
      L_UserSVmin = .FALSE.
      L_UserWSmin = .FALSE.
      L_UserFRANmax = .FALSE.
      L_UserSWmin = .FALSE.
      L_UserBigT = .FALSE.
      L_UserFRANmin = .FALSE.
      L_PBal = .FALSE.

!     Added for TTRM; AECOM
!     Initialize logicals for ozone response rate (TTRM)
      RUNTTRM = .FALSE.
      TTRMDBG = .FALSE.
      RUNTTRM2 = .FALSE.
      TTRM2DBG = .FALSE.
!     End TTRM insert, Nov. 2021
      URBDBUG = .FALSE.
      BLPDBUG = .FALSE.
C --- Initialize logicals to indicate various met data options used, 
C     including the 'ADJ_U*' and 'BULKRN' options in AERMET, and the
C     use of MMIF-generated met inputs directly from MMIF or processed
C     through AERMET; L_MMIF_Data indicates that MMIF-generated data
C     have been used based on information in SURFACE file header record;
C     L_MMIF_Profile is used to flag potential use of MMIF-generated
C     data based on profile heights exceeding 999m, absent information 
C     in the SURFACE file header record.
      L_AdjUstar     = .FALSE.
      L_BULKRN       = .FALSE.
      L_MMIF_Data    = .FALSE.
      L_MMIF_Profile = .FALSE.
      L_TurbData     = .FALSE.

C     Add logical to control user-specified deposition velocity for gases
      LUSERVD  = .FALSE.
      SCIM     = .FALSE.
      SCIMOUT  = .FALSE.
      SCIMHR   = .FALSE.
      SEASONHR = .FALSE.
      BETA     = .FALSE.

C --- Initialize logicals for processing design values based on ranked
C     values averaged across years, 24-hr PM2.5, 1-hr NO2 and 1-hr SO2
      PM25AVE = .FALSE.
      NO2AVE  = .FALSE.
      SO2AVE  = .FALSE.
      L_NO_PM25AVE = .FALSE.
      L_NO_NO2AVE  = .FALSE.
      L_NO_SO2AVE  = .FALSE.

C --- Initialize logicals for background ozone, NOx and concentrations
      L_O3File(:)   = .FALSE.
      L_O3Hourly    = .FALSE.
      L_O3VAL(:)    = .FALSE.
      L_O3VALUES(:) = .FALSE.
      L_BACKGRND    = .FALSE.
      L_BGHourly    = .FALSE.
      L_BGFile(:)   = .FALSE.
      L_BGValues(:) = .FALSE.
      L_NOXVALUE(:) = .FALSE.
      L_NOX_VALS(:) = .FALSE.
      L_NOxHourly   = .FALSE.
      L_NOxFile(:)  = .FALSE. 
      L_CalcNOXFromNO2 = .FALSE.
      
C --- Initialize array index counter for temporally-varying background
C     concentrations (other than hourly file) to zero
      IBKGRD(:) = 0
C --- Initialize logical for day-of-week options
      L_DayOfWeekOpts  = .FALSE.
      
C     Add logical for MAXDAILY output file option
      MXDAILY  = .FALSE.
      L_MAXDCONT = .FALSE.
      MXDAILY_BYYR  = .FALSE.
C     Initialize variable to skip messages during MAXDCONT processing
      L_SkipMessages = .FALSE.
      
C --- Set logical flag for whether "New" met data are being used, based 
C     on version 11059 or later of AERMET, using the wind data source
C     and adjustment flags introduced with version 11059 ('NAD' or 'ADJ')
      L_NAD_ADJ_Flags = .FALSE.
C --- Set logical flag for use of "old" met data, i.e., v06341 of AERMET,
C     which is allowed for now, with a warning message
      L_OldMetVer = .FALSE.
      
C     Non-DFAULT option for optimized area source, formerly controlled by 
C     TOXICS option, which is now obsolete
      FASTAREA  = .FALSE.    
C     Non-DFAULT option for optimized meander for POINT and VOLUME sources based
C     on effective sigma-y; also activates the FASTAREA option
      FASTALL   = .FALSE.
C     Logical flag for optimized meander for POINT and VOLUME sources based
C     on effective sigma-y under non-DFAULT FASTALL option
      L_EFFSIGY = .FALSE.
      
C --- Logical flag to indicate whether non-DFAULT options are specified
C     when DFAULT is not specified. This is used to set the option label 
C     in the file headers.
      L_NonDFAULT = .FALSE.
      
C --- Initialize options for checking date sequence in meteorological data
      NOCHKD     = .FALSE.
      L_WARNCHKD = .FALSE.

C --- Initialize logical variables used to flag EVENT or MAXDCONT inconsistency
C     warnings in order to issue a warning to the default output unit at the 
C     end of the run.
      L_EVENT_OrigConc_Warning    = .FALSE.
      L_MAXDCONT_OrigConc_Warning = .FALSE.

C     JAT 1/29/21 D070 TURBULENCE OPTIONS
C     INITIALIZE TURBOPTS TO FALSE
      TURBOPTS=.FALSE.
C*----
C*#

C --- Initialize variable for specifying format for file outputs;
C     Value has been "preset" under subroutine PRESET in order for the
C     postfile format to be adjusted before writing header recordes in sub_OUPOST;
C     default = 'FIX' for fixed-format outputs; 'EXP' indicates user specified
C     use of exponential-format outputs.  This variable applies to 
C     MAXIFILE, PLOTFILE, POSTFILE (plot-formatted), RANKFILE, and SEASONHR outputs
      IF (FILE_FORMAT .NE. 'EXP') THEN
         FILE_FORMAT = 'FIX'
      END IF

C --- Initialize REELEV character variable to 'METERS' for default elevation units
      REELEV = 'METERS'
C --- Initialize SOELEV character variable to 'METERS' for default elevation units
      SOELEV = 'METERS'

C     Initialize Decay Coefficient to 0.0 (Urban SO2 Default Set in POLLUT)
      DECOEF = 0.0D0

C     Initialize variables to hold two previous hours of precipitation
      PREC1  = 0.0D0
      PREC2  = 0.0D0
      IF (ALLOCATED(APREC1)) APREC1 = 0.0D0
      IF (ALLOCATED(APREC2)) APREC2 = 0.0D0
C --- Initialize variable for total precipitation
      TOTAL_PRECIP = 0.0D0

C     Initialize variables used to calculate canopy stomatal resistance, Rs
      Wold = 180.0D0
      f2   = Wold/200.0D0

C     Initialize defaults for Fo, FSEAS2, and FSEAS5 (may be input by user
C     on the CO GASDEPDF card).
      Fo     = 0.0D0
      FSEAS2 = 0.5D0
      FSEAS5 = 0.25D0

C     Initialize the Source Arrays
      ISRC = 0
      AXS(:) = 0.0D0
      AYS(:) = 0.0D0
      AZS(:) = 0.0D0
      AQS(:) = 0.0D0
      AHS(:) = 0.0D0
      ADS(:) = 0.0D0
      AVS(:) = 0.0D0
      ATS(:) = 0.0D0
      ASYINI(:) = 0.0D0
      ASZINI(:) = 0.0D0
      IF (ALLOCATED(AXINIT)) AXINIT(:) = 0.0D0
      IF (ALLOCATED(AYINIT)) AYINIT(:) = 0.0D0
      IF (ALLOCATED(AANGLE)) AANGLE(:) = 0.0D0
      IF (ALLOCATED(RADIUS)) RADIUS(:) = 0.0D0
      IF (ALLOCATED(AXCNTR)) AXCNTR(:) = 0.0D0
      IF (ALLOCATED(AYCNTR)) AYCNTR(:) = 0.0D0
      IF (ALLOCATED(AALPHA)) AALPHA(:) = 0.0D0
      IF (ALLOCATED(APDEFF)) APDEFF(:) = 0.0D0
      IF (ALLOCATED(AVOLUM)) AVOLUM(:) = 0.0D0
      SOPCRD(:) = 'N'
      SOGAS(:)  = 'N'
      URBSRC(:) = 'N'
      SRCID(:)  = ' '
      SRCTYP(:) = ' '
      QFLAG(:)  = ' '
      INPD(:)   = 0
      IF (ALLOCATED(NVERTS)) NVERTS(:) = 0
      IELUNT(:) = 0
      EVAL(:) = .FALSE.
      EVLFIL(:) = ' '
      L_FLATSRC(:) = .FALSE.
      L_HRLYSIG(:) = .FALSE.
      GRP_BACK(:)  = .FALSE.
      L_WakeMessage(:) = .FALSE.
      
C --- 1/20/2012, CRT: D063 Intialize platform parameters
      OSPLAT(:) = .FALSE.  ! Source subject to platform downwash
      PLATELV(:) = 0.0D0   ! Platform base elev. above ocean surface
      PLATHB(:) = 0.0D0    ! Plaform building height above platform base
      PLATWB(:) = 0.0D0    ! Platform building width
      
C --- Initialize BFLAG for BACKGRND concentrations
      BFLAG(:) = ' '
      
C     Add gas dry deposition parameters
      IF (ALLOCATED(PDIFF))      PDIFF(:)  = 0.0D0
      IF (ALLOCATED(PDIFFW))     PDIFFW(:) = 0.0D0
      IF (ALLOCATED(ALPHAS))     ALPHAS(:) = 0.0D0
      IF (ALLOCATED(REACT))      REACT(:)  = 0.0D0
      IF (ALLOCATED(HENRY))      HENRY(:)  = 0.0D0
      IF (ALLOCATED(RCLI))       RCLI(:)   = 0.0D0
      IF (ALLOCATED(L_METHOD2))  L_METHOD2(:) = .FALSE.
      
      IF (ALLOCATED(ADSBH))   ADSBH(:,:)   = 0.0D0
      IF (ALLOCATED(ADSBW))   ADSBW(:,:)   = 0.0D0
      IF (ALLOCATED(ADSBL))   ADSBL(:,:)   = 0.0D0
      IF (ALLOCATED(ADSXADJ)) ADSXADJ(:,:) = 0.0D0
      IF (ALLOCATED(ADSYADJ)) ADSYADJ(:,:) = 0.0D0
      IF (ALLOCATED(AXVERT))  AXVERT(:,:)  = 0.0D0
      IF (ALLOCATED(AYVERT))  AYVERT(:,:)  = 0.0D0

      IF (ALLOCATED(QFACT))   QFACT(:,:)   = 0.0D0

C --- Initialize NUMO3Sects, NUMBGSects and NUMNOxSects to 1
      IF (.NOT. L_O3Sector) NUMO3Sects = 1
      IF (.NOT. L_BGSector) NUMBGSects = 1
      IF (.NOT. L_NOxSector) NUMNOxSects = 1
C --- Initialize counter for number of missing BGHrVal substitutions
      NSubBGHOUR = 0

C --- Initialize O3VARY and NOXVARY arrays across all sectors to -99.0 to facilitate QA 
C     checks on completeness of the inputs
      IF (ALLOCATED(O3VARY))  O3VARY(:,:) = -99.0D0
      IF (ALLOCATED(NOXVARY))  NOXVARY(:,:) = -99.0D0

      O3FLAG(:) = ''
      BFLAG(:)  = ''
      NOXFLAG(:) = ''
      IO3MAX(:) = 0
      IBGMAX(:) = 0
      INOXMAX(:) = 0
      
      IF (ALLOCATED(BACKGRND)) BACKGRND(:,:) = 0.0D0
      IF (ALLOCATED(BACKAVE)) BACKAVE(:) = 0.0D0
      IF (ALLOCATED(IGROUP)) IGROUP(:,:) = 0
      IF (ALLOCATED(CHI)) CHI(:,:,:) = 0.0D0
      IF(GRSM)THEN
        CHI_TTRAVPLM = 0.0D0
        CHI_TTRAVPAN = 0.0D0
        CHI_TTRAVAER = 0.0D0
        CHI_TTRAVPRM = 0.0D0
        CHI_TTRAVCHM(:,:) = 0.0D0
        TTRAVCHM(:) = 0.0D0
      END IF
      IF (OLM) THEN
         OLMID(:) = ' '
         IGRP_OLM(:,:) = 0
         L_OLMGRP(:) = .FALSE.
      END IF
      IF (PVMRM .OR. OLM .OR. GRSM) THEN
         ANO2_RATIO(:) = -9.0D0
         IF (PSDCREDIT) THEN
            PSDSRCTYP(:)  = '  '
            L_PSDGRP(:)   = .FALSE.
            IGRP_PSD(:,:) = 0
         END IF
         IF (PVMRM .OR. GRSM) THEN
            PPFACT(:,:)  = 0.0D0
            HECNTR(:,:)  = 0.0D0
            HECNTR3(:,:) = 0.0D0
            UEFFS(:,:)   = 0.0D0
            UEFF3S(:,:)  = 0.0D0
            EPSEF(:,:)   = 0.0D0
            EPSEF3(:,:)  = 0.0D0
            FOPTS(:,:)   = 0.0D0
         END IF
      END IF

      IF (PSDCREDIT) THEN
         PSDID(:) = ' '
         ABVAL(:,:) = 0.0D0
         BCVAL(:,:) = 0.0D0
      END IF

! Added for TTRM; Nov. 2021
      IF (RUNTTRM) THEN
         TTRMSRC(:,:) = ' '
      END IF
! End of TTRM insert

C     Initialize arrays for deposition; first check if ALLOCATED
      IF (ALLOCATED(APDIAM))   APDIAM   = 0.0D0
      IF (ALLOCATED(APHI))     APHI     = 0.0D0
      IF (ALLOCATED(APDENS))   APDENS   = 0.0D0
      IF (ALLOCATED(AVGRAV))   AVGRAV   = 0.0D0
      IF (ALLOCATED(ATSTOP))   ATSTOP   = 0.0D0
      IF (ALLOCATED(EFRAC))    EFRAC    = 0.0D0
      IF (ALLOCATED(QPART))    QPART    = 0.0D0
      IF (ALLOCATED(PDIAM))    PDIAM    = 0.0D0
      IF (ALLOCATED(PHI))      PHI      = 0.0D0
      IF (ALLOCATED(PDENS))    PDENS    = 0.0D0
      IF (ALLOCATED(VGRAV))    VGRAV    = 0.0D0
      IF (ALLOCATED(TSTOP))    TSTOP    = 0.0D0
      IF (ALLOCATED(SCHMIDT))  SCHMIDT  = 0.0D0
      IF (ALLOCATED(VDEP))     VDEP     = 0.0D0
      IF (ALLOCATED(SCF))      SCF      = 0.0D0
      IF (ALLOCATED(PSCVRT))   PSCVRT   = 0.0D0
      IF (ALLOCATED(WASHOUT))  WASHOUT  = 0.0D0
      IF (ALLOCATED(ECOLL))    ECOLL    = 0.0D0
      IF (ALLOCATED(finemass)) finemass = 0.0D0

C     Initialize URBAN arrays
      IF (ALLOCATED(IURBGRP))   IURBGRP   = 0
      IF (ALLOCATED(URBID))     URBID     = ' '
      IF (ALLOCATED(URBNAM))    URBNAM    = ' '
      IF (ALLOCATED(URBPOP))    URBPOP    = 0.0D0
      IF (ALLOCATED(URBZ0))     URBZ0     = 0.0D0
      IF (ALLOCATED(ZIURB))     ZIURB     = 0.0D0
      IF (ALLOCATED(URBWSTR))   URBWSTR   = 0.0D0
      IF (ALLOCATED(URBUSTR))   URBUSTR   = 0.0D0
      IF (ALLOCATED(URBOBULEN)) URBOBULEN = 0.0D0
      IF (ALLOCATED(GRDSWU))    GRDSWU    = 0.0D0 
      IF (ALLOCATED(GRDSVU))    GRDSVU    = 0.0D0
      IF (ALLOCATED(GRDTGU))    GRDTGU    = 0.0D0
      IF (ALLOCATED(GRDPTU))    GRDPTU    = 0.0D0

C     Initialize source depletion factors to unity.
      DQCORG = 1.0D0
      WQCORG = 1.0D0
      IF (ALLOCATED(WQCOR)) WQCOR = 1.0D0
      IF (ALLOCATED(DQCOR)) DQCOR = 1.0D0

C     Counters for the Receptor Groups
      IREC = 0
      ISTA = .FALSE.
      IEND = .FALSE.
      IRXR = 0
      IRYR = 0
      IRZE = 0
      IRZH = 0
      IRZF = 0
      NEWID = .TRUE.
C     Initialize ITAB, NXTOX, NYTOX Variables for the TOXXFILE Option, 9/29/92
      ITAB  = -9
      NXTOX = 0
      NYTOX = 0

C     Initialize Variables Associated with the Meteorology Data
      ISJDAY = 0
      IEJDAY = 366
      ISDATE = 0
C     Initialize End Date as largest valid date for a 4-byte integer.
      IEDATE = 2147123124
      ISYR   = 0
      ISMN   = 0
      ISDY   = 0
      ISHR   = 0
      IEYR   = 9999
      IEMN   = 99
      IEDY   = 99
      IEHR   = 24
      IPDATE = 0
      IPHOUR = 0
      IMONTH = 0
      IDAY   = 0
      IHOUR  = 0
      IYEAR  = 0
      NUMYRS = 0
      NREMAIN= 0
      IHR_NDX= 0
      IYR_NDX= 0
      INCRST = 1
      SFX = 0.0D0
      SFY = 0.0D0
      UAX = 0.0D0
      UAY = 0.0D0
      ROTANG = 0.0D0
      O3MISS = .FALSE.
      O3BACK = -999.0D0
      O3CONC = -999.0D0
      NO2Equil = 0.90D0
      NO2Stack = 0.10D0
      IOLM = 0
      NOXMISS = .FALSE.
      NOXBACK = -999.0D0
      NOXBGCONC = -999.0D0
      ITTRM = 0

C     Initialize Met Data Filenames and Formats
      METINP = ' '
      PROINP = ' '
      METFRM = 'FREE'
      PROFRM = 'FREE'

C --- Initialize EVENT processing arrays
      IF (EVONLY) THEN
         AXR(:)    = 0.0D0
         AYR(:)    = 0.0D0
         AZELEV(:) = 0.0D0
         AZHILL(:) = 0.0D0
         AZFLAG(:) = 0.0D0
         EVAPER(:) = 0
         EVDATE(:) = 0
         EVJDAY(:) = 0
         IDXEV(:)  = 0
         EVNAME(:) = ' '
         EVGRP(:)  = ' '
         EV_OrigConc(:) = 0.0D0
         EV_HRQS(:,:) = 0.0D0
         EV_HRTS(:,:) = 0.0D0
         EV_HRVS(:,:) = 0.0D0
         EV_HRSY(:,:) = 0.0D0
         EV_HRSZ(:,:) = 0.0D0
      ELSE IF (.NOT. EVONLY) THEN
C ---    Initialize Receptor arrays
         AXR(:)    = 0.0D0
         AYR(:)    = 0.0D0
         AZELEV(:) = 0.0D0
         AZHILL(:) = 0.0D0
         AZFLAG(:) = 0.0D0
         IF (ALLOCATED(XR_SCS)) XR_SCS(:,:) = 0.0D0                     ! Multiple_BuoyLines_D41_Wood
         IF (ALLOCATED(YR_SCS)) YR_SCS(:,:) = 0.0D0                     ! Multiple_BuoyLines_D41_Wood
         IF (ALLOCATED(XR_RCS)) XR_RCS(:) = 0.0D0
         IF (ALLOCATED(YR_RCS)) YR_RCS(:) = 0.0D0
         IREF(:)   = 0
         NETID(:)  = ' '
         RECTYP(:) = ' '
         NDXARC(:) = 0
         XORIG(:)  = 0.0D0
         YORIG(:)  = 0.0D0
         NUMXPT(:) = 0
         NUMYPT(:) = 0
         NETSTA(:) = 0
         NETEND(:) = 0
         NTID(:)   = ' '
         NTTYP(:)  = ' '
         XCOORD(:,:) = 0.0D0
         YCOORD(:,:) = 0.0D0
         XINT = 0.0D0
         YINT = 0.0D0
      END IF

      KAVE(:) = 0

C     Initialize the Outputs
      TITLE1 = ' '
      TITLE2 = ' '
      IPAGE  = 0
      IPGSUM = 0
      NHIVAL = 0
      NMXVAL = 0
      THRFRM ='(1X,I3,1X,A8,1X,I8.8,2(1X,F13.5),3(1X,F7.2),1X,F13.5)'
      PSTFRM ='(3(1X,F13.5),3(1X,F8.2),2X,A6,2X,A8,2X,I8.8,2X,A8)'
      PLTFRM ='(3(1X,F13.5),3(1X,F8.2),3X,A5,2X,A8,2X,A5,5X,A8,2X,I8)'
      RNKFRM ='(1X,I6,1X,F13.5,1X,I8.8,2(1X,F13.5),3(1X,F7.2),2X,A8)'
      MXDFRM ='(3(1X,F13.5),3(1X,F8.2),2X,A6,2X,A8,2X,I4,2X,I3,2X,I8.8,
     &2X,A8)'
      INHI(:)   = 0
      IDYTAB(:) = 0
      MAXAVE(:) = 0
      IMXVAL(:) = 0
      ITOXFL(:) = 0
      ITXUNT(:) = 0
      IRNKFL(:) = 0
      IRKVAL(:) = 0
      IRKUNT(:) = 0
      TOXTHR(:) = 0.0D0
      TOXFIL(:) = ' '
      RNKFIL(:) = ' '
      IDCONC(:,:) = 0
      TXCONC(:,:) = 0.0D0
      NHIAVE(:,:) = 0
      MAXFLE(:,:) = 0
      IPSTFL(:,:) = 0
      IMXUNT(:,:) = 0
      IPSUNT(:,:) = 0
      IPSFRM(:,:) = 0
      THRESH(:,:) = 0.0D0
      THRFIL(:,:) = ' '
      PSTFIL(:,:) = ' '
      IPLTFL(:,:,:) = 0
      IPLUNT(:,:,:) = 0
      PLTFIL(:,:,:) = ' '
      GRPID(:)  = ' '
      IANPST(:) = 0
      IANFRM(:) = 0
      IANPLT(:) = 0
      IAPUNT(:) = 0
      IPPUNT(:) = 0
      ISHUNT(:) = 0
      ISEAHR(:) = 0
      ANNPST(:) = ' '
      ANNPLT(:) = ' '
      IMXDLY(:) = 0
      IMDUNT(:) = 0
      MAXDLY(:) = ' '
      MAXDCONT(:) = 0
      IMXDLY_BYYR(:) = 0
      IMDUNT_BYYR(:) = 0
      MAXDLY_BYYR(:) = ' '
      MAXDCONT_FILE(:) = ' '
      MXD_RANK(:,:) = 0
      MAXD_THRESH(:) = 0.0D0

C     Initialize filenames
      SAVFIL = ' '
      MSGFIL = ' '
      EVFILE = ' '
      SUMFIL = ' '

      BKGRND_File(:) = ' '
      OZONFL(:) = ' '
      NOXFL(:) = ' '

C     Initialize the Number of Error/Warning/Informational Messages, and
C     The Number of Fatal Errors.
      IERROR = 0
      NFATAL = 0
      NWARN  = 0

      RETURN
      END


      SUBROUTINE INCLUD
C***********************************************************************
C*                INCLUD Module of AERMOD Model
C*
C*       PURPOSE: To read an external receptor/source file using the
C*                INCLUDED keyword.
C*
C*       PROGRAMMER: Jayant Hardikar, Roger Brode
C*
C*       DATE:    September 30, 1995
C*
C        MODIFIED:   To remove reference to obsolete TG pathway inherited
C                    from ISCST3 code.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
C
C*                   
C*       INPUTS: 
C*
C*       OUTPUTS:
C*               
C*
C*       CALLED FROM:   MAIN
C***********************************************************************
        
C*    Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, ITEMPL
      LOGICAL NOPATH, NOKEY      
      CHARACTER RDFRM*20
C JAT 06/22/21 D065
C REMOVE INPFLD AS UNUSED VARIABLE
C      CHARACTER INPFLD*2, PATHWY(7)*2
      CHARACTER PATHWY(7)*2
      INTERFACE
         SUBROUTINE EXPATH(INPFLD,PATHWY,IPN,NOPATH)
            CHARACTER (LEN=2), INTENT(IN) :: INPFLD
            CHARACTER (LEN=2), INTENT(IN), DIMENSION(:) :: PATHWY
            INTEGER, INTENT(IN) :: IPN
            LOGICAL, INTENT(OUT) :: NOPATH
         END SUBROUTINE EXPATH
      END INTERFACE

C*    Variable Initializations
      MODNAM = 'INCLUD'
      EOF = .FALSE.
      ILINE  = 0
      ITEMPL = 0

C     Initialize PATHWY array
      PATHWY(1) = 'CO'
      PATHWY(2) = 'SO'
      PATHWY(3) = 'RE'
      PATHWY(4) = 'ME'
      PATHWY(5) = 'OU'
      PATHWY(6) = '**'
      PATHWY(7) = 'EV'

C     Setup READ format and ECHO format for runstream record,
C     based on the ISTRG PARAMETER (set in MAIN1)
      WRITE(RDFRM,9100) ISTRG, ISTRG
 9100 FORMAT('(A',I4.4,',T1,',I4.4,'A1)')

      IF (IFC .EQ. 3) THEN
C        Retrieve Included Filename as Character Substring to Maintain Case
         IF ((LOCE(3)-LOCB(3)) .LE. (ILEN_FLD - 1) ) THEN
C           Retrieve Filename as Character Substring to Maintain Original Case
C           Also Check for Filename Larger Than ILEN_FLD Characters
            INCFIL = RUNST1(LOCB(3):LOCE(3))
            OPEN (INCUNT,FILE=INCFIL,ACTION='READ',STATUS='OLD',ERR=99)
         ELSE
C           WRITE Error Message:  INCFIL Field is Too Long
            WRITE(DUMMY,'(I8)') ILEN_FLD
            CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
            RETURN
         END IF

      ELSE IF (IFC .GT. 4) THEN
C        WRITE Error Message           ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Error Message         ! No Parameters Specified
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      END IF

      GO TO 1001

C     Write Out Error Message for File OPEN Error
99    CALL ERRHDL(PATH,MODNAM,'E','500','INCFILE ')
      RETURN

1001  CONTINUE

      
C     LOOP Through Input Runstream Records
      DO WHILE (.NOT. EOF)

C        Increment the Line Counter.  It was Initially Set to 1, to Handle
C        the Code in Subroutine DEFINE
         ILINE = ILINE + 1

C        READ Record to Buffers, as A'num' and 'num'A1, where 'num' = ISTRG
C        Length of ISTRG is Set in PARAMETER Statement in MAIN1
         READ (INCUNT,RDFRM,END=999,ERR=888) RUNST1, 
     &                                      (RUNST(I), I = 1, ISTRG)

C        Check for blank input record and cycle
         IF (LEN_TRIM(RUNST1) .EQ. 0) CYCLE

C        Convert Lower Case to Upper Case Letters           ---   CALL LWRUPR
         CALL LWRUPR

C        If ILINE=1, reset ILINE temporarily to avoid the
C        check for column shift in subroutine DEFINE
         IF (ILINE .EQ. 1) THEN
            ILINE  = 2
            ITEMPL = 1
         END IF

C        Define Fields on Card                              ---   CALL DEFINE
         CALL DEFINE

C        Reset ILINE if needed
         IF (ITEMPL .EQ. 1) THEN
            ILINE  = 1
            ITEMPL = 0
         END IF

C        Get the Contents of the Fields                     ---   CALL GETFLD
         CALL GETFLD

C        Check for 'NO ECHO' In First Two Fields
         IF (FIELD(1) .EQ. 'NO' .AND. FIELD(2) .EQ. 'ECHO') THEN
C           Skip record with NO ECHO in INCLUDED file, but leave ECHO "on"
            CYCLE
         END IF

C        Extract Pathway ID From Field 1                    ---   CALL EXPATH
         CALL EXPATH(FIELD(1),PATHWY,7,NOPATH)

C        For Invalid Pathway and Comment Lines Skip to Next Record
         IF (NOPATH) THEN
C           WRITE Error Message    ! Invalid Pathway ID
            CALL ERRHDL(PPATH,MODNAM,'E','100',PATH)
            PATH = PPATH
            CYCLE
         ELSE IF (PATH .EQ. '**') THEN
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

C ---    Check for Proper Order of Setup Cards              ---   CALL SETORD
C        Only call SETORD if not STARTING or FINISHED 
C        since these keywords are not allowed in INCLUDED files.
C        This also allows INCLUD to be used for standard 
C        processing and EVENT processing.
         IF (KEYWRD .NE. 'STARTING' .AND.
     &       KEYWRD .NE. 'FINISHED') CALL SETORD

C        First Check for Invalid Keywords (STARTING, FINISHED, INCLUDED)
         IF (KEYWRD .EQ. 'STARTING') THEN
C           Cannot Use the STARTING keyword in the INCLUDED file
            CALL ERRHDL(PATH,MODNAM,'E','140',KEYWRD)

         ELSE IF (KEYWRD .EQ. 'INCLUDED') THEN
C           Cannot Recurse the INCLUDED Keyword in the INCLUDED file
C           Write Error Message: Repeat INCLUDED In Same Pathway
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)

         ELSE IF (KEYWRD .EQ. 'FINISHED') THEN
C           Cannot Use the FINISHED Keyword in the INCLUDED File
            CALL ERRHDL(PATH,MODNAM,'E','140',KEYWRD)

C        Process Input Card Based on Pathway
         ELSE IF (PATH .EQ. 'SO') THEN
C           Process SOurce Pathway Cards                    ---   CALL SOCARD
            CALL SOCARD
         ELSE IF (PATH .EQ. 'RE') THEN
C           Process REceptor Pathway Cards                  ---   CALL RECARD
            CALL RECARD
         ELSE IF (PATH .EQ. 'EV') THEN
C           Process EVent Pathway Cards                     ---   CALL EVCARD
            CALL EVCARD
            
         END IF

C        Store the Current Keyword as the Previous Keyword
         PKEYWD = KEYWRD

C        Cycle to next record
         CYCLE

 999     EOF = .TRUE.
         CONTINUE

      END DO
      EOF = .FALSE.

      GO TO 1002
      
 888  CONTINUE
C --- Error occurred reading the included file, issue error message
      CALL ERRHDL(PATH,MODNAM,'E','510','INCLUDED')
      RUNERR = .TRUE.

1002  CONTINUE
      
C     Close the INCLUDED File
      CLOSE (INCUNT)
      
      RETURN
      END
