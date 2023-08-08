      SUBROUTINE RECARD
C***********************************************************************
C                 RECARD Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: To process REceptor Pathway card images
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   Removed obsolete reference to IRSTAT(7).
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
C
C        MODIFIED:   To remove some restrictions on the order of
C                    the BOUNDELV keyword - 9/29/92
C
C        INPUTS:  Pathway (RE) and Keyword
C
C        OUTPUTS: Receptor Arrays
C                 Receptor Setup Status Switches
C
C        CALLED FROM:   SETUP
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      USE BUOYANT_LINE
      
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: ILSAVE, KK

C     Variable Initializations
      MODNAM = 'RECARD'

      IF (KEYWRD .EQ. 'STARTING') THEN
C        Initialize Counters and Set Status Switch
         IREC = 0
         INNET = 0
         NUMREC = 0
         NUMARC = 0
         IRXR = 0
         IRYR = 0
         IRZE = 0
         IRZH = 0
         IRZF = 0
         PXSOID = ' '
         PESOID = ' '
         ISTA = .FALSE.
         IRSTAT(1) = IRSTAT(1) + 1
         IF (IRSTAT(1) .NE. 1) THEN
C           Error Message: Repeat Starting In Same Pathway
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         END IF
C        Flush the Working Arrays
         ZETMP1(:) = 0.0D0
         ZETMP2(:) = 0.0D0
         ZFTMP1(:) = 0.0D0
         ZFTMP2(:) = 0.0D0
      ELSE IF (KEYWRD .EQ. 'GRIDCART') THEN
C        Set Status Switch
         IRSTAT(2) = IRSTAT(2) + 1
C        Process Cartesian Grid Receptor Network            ---   CALL RECART
         CALL RECART
      ELSE IF (KEYWRD .EQ. 'GRIDPOLR') THEN
C        Set Status Switch
         IRSTAT(3) = IRSTAT(3) + 1
C        Process Polar Receptor Network                     ---   CALL REPOLR
         CALL REPOLR
      ELSE IF (KEYWRD .EQ. 'DISCCART') THEN
C        Set Status Switch
         IRSTAT(4) = IRSTAT(4) + 1
C        Process Discrete Cartesian Receptor Locations      ---   CALL DISCAR
         CALL DISCAR
      ELSE IF (KEYWRD .EQ. 'DISCPOLR') THEN
C        Set Status Switch
         IRSTAT(5) = IRSTAT(5) + 1
C        Process Discrete Polar Receptor Locations          ---   CALL DISPOL
         CALL DISPOL
      ELSE IF (KEYWRD .EQ. 'EVALCART') THEN
C        Set Status Switch
         IRSTAT(8) = IRSTAT(8) + 1
         IF (PSDCREDIT) THEN
C           Write Error Message: EVALCART not valid with PSDCREDIT option
            CALL ERRHDL(PATH,MODNAM,'E','147',KEYWRD)
         ELSE
C           Process Discrete Cartesian Receptor Locations   ---   CALL EVCART
            CALL EVCART
         END IF
      ELSE IF (KEYWRD .EQ. 'ELEVUNIT') THEN
C        Set Status Switch
         IRSTAT(9) = IRSTAT(9) + 1
         IF (IRSTAT(9) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE IF (IRSTAT(2) .GT. 0 .OR. IRSTAT(3) .GT. 0 .OR.
     &            IRSTAT(4) .GT. 0 .OR. IRSTAT(5) .GT. 0 .OR.
     &                                  IRSTAT(8) .GT. 0) THEN
C           Write Error Message: ELEVUNIT must be first card after STARTING
            CALL ERRHDL(PATH,MODNAM,'E','152','  RE')
         ELSE
C           Process Elevation Units for Source Elevations   ---   CALL REELUN
            CALL REELUN
         END IF
      ELSE IF (KEYWRD .EQ. 'INCLUDED') THEN
C        Set Status Switch
         IRSTAT(11) = IRSTAT(11) + 1
C        Save ILINE as ISAVE
         ILSAVE = ILINE
C        Process the Included Receptor File                 ---   CALL INCLUD
         CALL INCLUD
C        Retrieve ILINE From ISAVE         
         ILINE = ILSAVE
      ELSE IF (KEYWRD .EQ. 'FINISHED') THEN
C        Set Status Switch
         IRSTAT(50) = IRSTAT(50) + 1
         IF (IRSTAT(50) .NE. 1) THEN
C           Error Message: Repeat Finished In Same Pathway
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
            GO TO 999
         END IF
C        Write Out The Error Message: Mandatory Keyword Missing
         IF (IRSTAT(1) .EQ. 0)THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','STARTING')
         END IF

         IF (ISTA) THEN
C           WRITE Error Message:  Missing END Keyword for a Grid Network
            CALL ERRHDL(PATH,MODNAM,'E','175',PNETID)
         END IF

C        Set Total Number of Receptors for This Run, NUMREC
         NUMREC = IRXR
         IF (NUMREC .EQ. 0) THEN
C           WRITE Error Message:  No Receptors Defined
            CALL ERRHDL(PATH,MODNAM,'E','185','NUMREC=0')
         END IF

C        Reinitialize ZFLAG array if needed
         IF (.NOT. FLGPOL) THEN
            DO IREC = 1, NUMREC
               AZFLAG(IREC) = 0.0D0
            END DO
         END IF

C ---    Check for missing receptor elevations, coded as -9999.0,
C        and convert from FEET to METERS if needed
         DO IREC = 1, NUMREC
            IF (AZELEV(IREC) .LT. -9998.99D0) THEN
C              WRITE Error Message:  Receptor elevation is missing
               WRITE(DUMMY,'(I8)') IREC
               CALL ERRHDL(PATH,MODNAM,'E','259',DUMMY)
            ELSE IF (AZHILL(IREC) .LT. -9998.99D0) THEN
C              WRITE Error Message:  Receptor hill height scale is missing
               WRITE(DUMMY,'(I8)') IREC
               CALL ERRHDL(PATH,MODNAM,'E','259',DUMMY)
            ELSE IF (REELEV .EQ. 'FEET') THEN
               AZELEV(IREC) = 0.3048D0*AZELEV(IREC)
               AZHILL(IREC) = 0.3048D0*AZHILL(IREC)
            END IF
         END DO

C ---    If there is a buoyant line source, the receptors need to be
C          translated and rotated so they are oriented the same as the
C          translation and rotation for the buoyant line source - use
C          the buoyant line source endpoints to determine translation
C          and rotation. This translation/rotation is only performed once.
C
C          NOTE1: The translation/rotation is BL source group dependent. 
C
C          NOTE2: this is an initial rotation based on the orientation of
C           buoyant line source relative to the x-y axes (with +y 
C           pointing north); the receptors will be rotated again each
C           hour based on the wind direction)

C Multiple_BuoyLines_D41_Wood
C        Process receptors based on BL sources/groups

         IF (NBLTOTAL. GE. 1) THEN
            BL_RFLAG = .false.
            DO KK = 1, NUMBLGRPS
               CALL BLRECP(KK)                      !  ---   CALL BLRECP
            END DO
         END IF

      ELSE
C        Write Error Message:  Invalid Keyword for This Pathway
         CALL ERRHDL(PATH,MODNAM,'E','110',KEYWRD)
      END IF

 999  RETURN
      END

      SUBROUTINE REELUN
C***********************************************************************
C                 REELUN Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Process Elevation Units Option for Receptors
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    November 22, 1994
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Receptor Elevation Units Switch
C
C        ERROR HANDLING:   Checks for Invalid Parameters;
C                          Checks for No Parameters;
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   RECARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'REELUN'

      IF (IFC .EQ. 3) THEN
         IF (FIELD(3) .EQ. 'METERS') THEN
            REELEV = 'METERS'
         ELSE IF (FIELD(3) .EQ. 'FEET') THEN
            REELEV = 'FEET'
         ELSE
C           WRITE Error Message  ! Invalid Parameter
            CALL ERRHDL(PATH,MODNAM,'E','203','RE_ELEV')
         END IF
      ELSE IF (IFC .GT. 3) THEN
C        WRITE Error Message     ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Error Message     ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200','ElevUnit')
      END IF

      RETURN
      END

      SUBROUTINE RECART
C***********************************************************************
C                 RECART Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes Cartesian Grid Receptor Network Inputs
C
C        PROGRAMMER:  Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To Fix Error Checking - Compare NETIDT With
C                    Full Secondary Keywords - 9/29/92
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Cartesian Grid Receptor Network Inputs
C
C        CALLED FROM:   RECARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I

C     Variable Initializations
      MODNAM = 'RECART'

C     READ in the Netid and Nettype
      IF (IFC .LT. 3) THEN
C        Write Error Message: Missing Data Field
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      END IF

      NETIDT = FIELD(3)
      IF (.NOT.NEWID .AND. (NETIDT.EQ.'    ' .OR.
     &    NETIDT.EQ.'XYINC' .OR. NETIDT.EQ.'XPNTS' .OR.
     &    NETIDT.EQ.'YPNTS' .OR. NETIDT.EQ.'ELEV' .OR.
     &    NETIDT.EQ.'HILL'  .OR.
     &    NETIDT.EQ.'FLAG'  .OR. NETIDT.EQ.'END')) THEN
         NETIDT = PNETID
         KTYPE = FIELD(3)
      ELSE IF (.NOT.NEWID .AND. NETIDT.EQ.PNETID) THEN
         KTYPE = FIELD(4)
      ELSE IF (NEWID .AND. NETIDT.NE.' ') THEN
         NEWID = .FALSE.
         KTYPE = FIELD(4)
C        The Keyword Counter
         INNET = INNET + 1
         IF (INNET .GT. NNET) THEN
C           WRITE Error Message:  Too Many Networks
C           This shouldn't occur since limits are dynamically allocated
            WRITE(DUMMY,'(''NNET='',I7)') NNET
            CALL ERRHDL(PATH,MODNAM,'E','290',DUMMY)
            RECERR = .TRUE.
            GO TO 999
         END IF
         INCSET = 0
         IXYSET = 0
         IEVSET = 0
         IFGSET = 0
      ELSE
C        Error Message: Invalid Secondary Keyword
         CALL ERRHDL(PATH,MODNAM,'E','170',PNETID)
         RECERR = .TRUE.
         GO TO 999
      END IF

C     Start to Set Up the Network
      IF (KTYPE .EQ. 'STA') THEN
C        Initialize Logical Control Variables
         ISTA = .TRUE.
         IEND = .FALSE.
         NEWID = .FALSE.
         RECERR = .FALSE.
C        Set Counters of Calculation Field
         ICOUNT = 0
         JCOUNT = 0
         IZE = 0
         IZH = 0
         IZF = 0
         IDC1 = IRXR
C        Check for Previous Grid Network With Same ID
         DO I = 1, INNET-1
            IF (FIELD(3) .EQ. NTID(I)) THEN
C              WRITE Warning Message:  Duplicate Network ID
               CALL ERRHDL(PATH,MODNAM,'W','252',NTID(I))
            END IF
         END DO
      ELSE IF (KTYPE .EQ. 'XYINC') THEN
         IF (.NOT. ISTA) THEN
C*          Write Error: MISSING STA OF THE BLOCK DATA
            CALL ERRHDL(PATH,MODNAM,'E','200','  STA   ')
         END IF
C        Error Message:Conflict Secondary Keyword
         IF (IXYSET .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','180',NETIDT)
         END IF
C        Set the Uniform Spacing Receptor Network           ---   CALL GENCAR
         CALL GENCAR
         INCSET = INCSET + 1
      ELSE IF (KTYPE.EQ.'XPNTS' .OR. KTYPE.EQ.'YPNTS') THEN
         IF (.NOT. ISTA) THEN
C*          Write Error: MISSING STA OF THE BLOCK DATA
            CALL ERRHDL(PATH,MODNAM,'E','200','  STA   ')
         END IF
C        Error Message:Conflict Secondary Keyword
         IF (INCSET .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','180',NETIDT)
         END IF
C        Set the Non-uniform Spacing Receptor Network       ---   CALL XYPNTS
         CALL XYPNTS
         IXYSET = IXYSET + 1
      ELSE IF (KTYPE .EQ. 'ELEV') THEN
         IF (.NOT. ISTA) THEN
C*          Write Error: MISSING STA OF THE BLOCK DATA
            CALL ERRHDL(PATH,MODNAM,'E','200','  STA   ')
         END IF
C        Read in and set the Terrain Elevation              ---   CALL TERHGT
         CALL TERHGT
         IEVSET = IEVSET + 1
      ELSE IF (KTYPE .EQ. 'HILL') THEN
         IF (.NOT. ISTA) THEN
C*          Write Error: MISSING STA OF THE BLOCK DATA
            CALL ERRHDL(PATH,MODNAM,'E','200','  STA   ')
         END IF
C        Read in and set the Terrain Elevation              ---   CALL HILHGT
         CALL HILHGT
         IHLSET = IHLSET + 1
      ELSE IF (KTYPE .EQ. 'FLAG') THEN
         IF (.NOT. ISTA) THEN
C*          Write Error: MISSING STA OF THE BLOCK DATA
            CALL ERRHDL(PATH,MODNAM,'E','200','  STA   ')
         END IF
C        Read in and set the Flagpole Receptor              ---   CALL FLGHGT
         CALL FLGHGT
         IFGSET = IFGSET + 1
      ELSE IF (KTYPE .EQ. 'END') THEN
         IEND = .TRUE.
C        Get The Final Results
         IF (.NOT. ISTA) THEN
C           Write Error: MISSING STA OF THE BLOCK DATA
            CALL ERRHDL(PATH,MODNAM,'E','200','  STA   ')
         ELSE IF (.NOT. RECERR) THEN
            CALL SETCAR
         END IF
         ISTA = .FALSE.
         NEWID = .TRUE.
C        Check If The Secondary Parameter Has Been Specified
         IF (IXYSET.EQ.0 .AND. INCSET.EQ.0) THEN
C           WRITE Error Message: Missing (X,Y) Point Setting
            CALL ERRHDL(PATH,MODNAM,'E','212',NETIDT)
         END IF

C        Warning: Elevated Terrain Inputs Inconsistent With Options
         IF (ELEV .AND. (IEVSET.EQ.0 .OR. IHLSET.EQ.0)) THEN
            CALL ERRHDL(PATH,MODNAM,'W','214',NETIDT)
            IRZE = IRXR
            IRZH = IRZE
         ELSE IF (FLAT .AND. IEVSET.NE.0) THEN
            CALL ERRHDL(PATH,MODNAM,'W','213',NETIDT)
            IRZE = IRXR
            IRZH = IRZE
         ELSE IF (FLAT .AND. IEVSET.EQ.0) THEN
            IRZE = IRXR
            IRZH = IRZE
         END IF

C        Warning: Flagpole Receptor Inputs Inconsistent With Options
         IF (FLGPOL .AND. IFGSET.EQ.0) THEN
            CALL ERRHDL(PATH,MODNAM,'W','216',NETIDT)
            IRZF = IRXR
         ELSE IF (.NOT.FLGPOL .AND. IFGSET.NE.0) THEN
            CALL ERRHDL(PATH,MODNAM,'W','215',NETIDT)
            IRZF = IRXR
         ELSE IF (.NOT.FLGPOL .AND. IFGSET.EQ.0) THEN
            IRZF = IRXR
         END IF

C        Check If The Number of Elev & Flag Is Match
         IF (ELEV .AND. IEVSET.NE.0) THEN
            IF (ICOUNT*JCOUNT .NE. IZE) THEN
C              Write Out The Error Message: No. Of ELEV not match
               CALL ERRHDL(PATH,MODNAM,'E','218','ELEV')
            END IF
            IF (ICOUNT*JCOUNT .NE. IZH) THEN
C              Write Out The Error Message: No. Of ZHILL not match
               CALL ERRHDL(PATH,MODNAM,'E','218','ZHILL')
            END IF
         END IF
         IF (FLGPOL .AND. IFGSET.NE.0) THEN
            IF (ICOUNT*JCOUNT .NE. IZF) THEN
C              Write Out The Error Message: No. Of FLAG not match
               CALL ERRHDL(PATH,MODNAM,'E','218','FLAG')
            END IF
         END IF

      ELSE
C        Error Message: Invalid Secondary Keyword
         CALL ERRHDL(PATH,MODNAM,'E','170',NETIDT)
         RECERR = .TRUE.
         GO TO 999

      END IF

      PNETID = NETIDT

 999  RETURN
      END

      SUBROUTINE GENCAR
C***********************************************************************
C                 GENCAR Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Generates Cartesian Grid Receptor Network With
C                 Uniform Spacing
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Cartesian Grid Receptor Network With Uniform
C                 Spacing
C
C        CALLED FROM:   RECART
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, J, K
      DOUBLE PRECISION :: XDELTA, YDELTA, TEMPP(6)
      LOGICAL ERROR

C     Variable Initializations
      MODNAM = 'GENCAR'
      ERROR = .FALSE.

C     Check for Location of Secondary Keyword, XYINC
      DO I = 1, IFC
         IF (FIELD(I) .EQ. 'XYINC') THEN
            ISC = I + 1
         END IF
      END DO

C     Determine Whether There Are Enough Parameter Fields
      IF (IFC .EQ. ISC-1) THEN
C        Error Message: Missing Parameter
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         RECERR = .TRUE.
         GO TO 999
      ELSE IF (IFC .GT. ISC+5) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KTYPE)
         RECERR = .TRUE.
         GO TO 999
      ELSE IF (IFC .LT. ISC+5) THEN
C        Error Message: Too Few Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KTYPE)
         RECERR = .TRUE.
         GO TO 999
      END IF

C     Input The Numerical Values
      DO K = 1,6
         CALL STODBL(FIELD(ISC + K-1),ILEN_FLD,TEMPP(K),IMIT)
C        Check The Numerical Field
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ERROR = .TRUE.
            RECERR = .TRUE.
         END IF
      END DO

      IF (ERROR) THEN
         ERROR = .FALSE.
         GO TO 999
      END IF

C     Assign Values to Appropriate Variables for Generated Network
      XINT   = TEMPP(1)
      ICOUNT = IDNINT(TEMPP(2))
      XDELTA = TEMPP(3)
      YINT   = TEMPP(4)
      JCOUNT = IDNINT(TEMPP(5))
      YDELTA = TEMPP(6)

C     Assign Them to the Coordinate Arrays
      IF (ICOUNT .LE. IXM) THEN
         DO I = 1, ICOUNT
            XCOORD(I,INNET) = XINT + XDELTA*DBLE(I-1)
         END DO
      ELSE
C        Error Msg: Maximum Number Of X-coordinates Exceeded
C        This shouldn't occur since limits are dynamically allocated
         WRITE(DUMMY,'(''IXM ='',I7)') IXM
         CALL ERRHDL(PATH,MODNAM,'E','290',DUMMY)
      END IF
      IF (JCOUNT .LE. IYM) THEN
         DO J = 1, JCOUNT
            YCOORD(J,INNET) = YINT + YDELTA*DBLE(J-1)
         END DO
      ELSE
C        Error Msg: Maximum Number Of Y-coordinates Exceeded
C        This shouldn't occur since limits are dynamically allocated
         WRITE(DUMMY,'(''IYM ='',I7)') IYM
         CALL ERRHDL(PATH,MODNAM,'E','290',DUMMY)
      END IF

 999  RETURN
      END

      SUBROUTINE XYPNTS
C***********************************************************************
C                 XYPNTS Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes Cartesian Grid x,y Input Value
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To Fix Error Checking - Change Limit for DO 15
C                    To 'JSET -1' - 9/29/92
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Cartesian Grid x,y Input Value
C
C        CALLED FROM:   RECART
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, J, JSET

C     Variable Initializations
      MODNAM = 'XYPNTS'

      IF (KTYPE .EQ. 'XPNTS') THEN
C        Check for Location of Secondary Keyword, XPNTS
         DO I = 1, IFC
            IF (FIELD(I) .EQ. 'XPNTS') THEN
               ISC = I + 1
            END IF
         END DO

C        Determine Whether There Are Enough Parameter Fields
         IF (IFC .EQ. ISC-1) THEN
C           Error Message: Missing Parameter
            CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
            RECERR = .TRUE.
            GO TO 999
         END IF

         ISET = ICOUNT
         DO I = ISC, IFC
            CALL STODBL(FIELD(I),ILEN_FLD,DNUM,IMIT)
C           Check The Numerical Field
            IF (IMIT .NE. 1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               RECERR = .TRUE.
            END IF
            ISET = ISET + 1
            IF (ISET .LE. IXM) THEN
               XCOORD(ISET,INNET) = DNUM
               DO J = 1, ISET-1
                  IF (DNUM .EQ. XCOORD(J,INNET)) THEN
C                    WRITE Warning Message:  X-Coord Specified More Than Once
                     CALL ERRHDL(PATH,MODNAM,'W','250',NETIDT)
                  END IF
               END DO
            ELSE
C              Error Msg: Maximum Number Of X-coordinates Exceeded
C              This shouldn't occur since limits are dynamically allocated
               WRITE(DUMMY,'(''IXM ='',I7)') IXM
               CALL ERRHDL(PATH,MODNAM,'E','290',DUMMY)
               RECERR = .TRUE.
            END IF
         END DO
         ICOUNT = ISET

      ELSE IF (KTYPE .EQ. 'YPNTS') THEN
C        Check for Location of Secondary Keyword, YPNTS
         DO I = 1, IFC
            IF (FIELD(I) .EQ. 'YPNTS') THEN
               ISC = I + 1
            END IF
         END DO

C        Determine Whether There Are Enough Parameter Fields
         IF (IFC .EQ. ISC-1) THEN
C           Error Message: Missing Parameter
            CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
            RECERR = .TRUE.
            GO TO 999
         END IF

         JSET = JCOUNT

         DO I = ISC, IFC
            CALL STODBL(FIELD(I),ILEN_FLD,DNUM,IMIT)
C           Check The Numerical Field
            IF (IMIT .NE. 1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               RECERR = .TRUE.
            END IF
            JSET = JSET + 1
            IF (JSET .LE. IYM) THEN
               YCOORD(JSET,INNET) = DNUM
               DO J = 1, JSET-1
                  IF (DNUM .EQ. YCOORD(J,INNET)) THEN
C                    WRITE Warning Message:  Y-Coord Specified More Than Once
                     CALL ERRHDL(PATH,MODNAM,'W','250',NETIDT)
                  END IF
               END DO
            ELSE
C              Error Msg: Maximum Number Of Y-coordinates Exceeded
C              This shouldn't occur since limits are dynamically allocated
               WRITE(DUMMY,'(''IYM ='',I7)') IYM
               CALL ERRHDL(PATH,MODNAM,'E','290',DUMMY)
               RECERR = .TRUE.
            END IF
         END DO
         JCOUNT = JSET
      END IF

 999  RETURN
      END

      SUBROUTINE SETCAR
C***********************************************************************
C                 SETCAR Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Setup the Final Cartesian Grid Receptor Network Inputs
C
C        PROGRAMMER:  Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:  To Include TOXXFILE Option - 9/29/92
C
C        INPUTS:  The GRIDCART Sub-pathway Input Parameters
C
C        OUTPUTS: Cartesian Grid Receptor Network Inputs
C
C        CALLED FROM:   RECART
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, J, JSET

C     Variable Initializations
      MODNAM = 'SETCAR'

      IF (ICOUNT.NE.0 .AND. JCOUNT.NE.0) THEN
C        Setup The Coordinate Of The Receptors
         NETSTA(INNET) = IRXR + 1
         ISET = IRXR
         JSET = IRYR
         DO J = 1, JCOUNT
            DO I = 1, ICOUNT
               ISET = ISET + 1
               JSET = JSET + 1
               IF (ISET .GT. NREC) THEN
C                 Error Msg: Maximum Number Of Receptor Exceeded
C                 This shouldn't occur since limits are dynamically allocated
                  WRITE(DUMMY,'(''NREC='',I7)') NREC
                  CALL ERRHDL(PATH,MODNAM,'E','290',DUMMY)
                  GO TO 999
               END IF
               IF (ICOUNT .GT. IXM) THEN
C                 Error Msg: Maximum Number Of X-coordinates Exceeded
C                 This shouldn't occur since limits are dynamically allocated
                  WRITE(DUMMY,'(''IXM ='',I7)') IXM
                  CALL ERRHDL(PATH,MODNAM,'E','290',DUMMY)
                  GO TO 999
               END IF
               IF (JCOUNT .GT. IYM) THEN
C                 Error Msg: Maximum Number Of Y-coordinates Exceeded
C                 This shouldn't occur since limits are dynamically allocated
                  WRITE(DUMMY,'(''IYM ='',I7)') IYM
                  CALL ERRHDL(PATH,MODNAM,'E','290',DUMMY)
                  GO TO 999
               END IF
               AXR(ISET) = XCOORD(I,INNET)
               AYR(JSET) = YCOORD(J,INNET)
            END DO
         END DO
         IRXR = ISET
         IRYR = JSET
         NETEND(INNET) = IRXR
         NUMXPT(INNET) = ICOUNT
         NUMYPT(INNET) = JCOUNT
         NTID(INNET)   = NETIDT
         NTTYP(INNET)  = 'GRIDCART'
C        Define ITAB, NXTOX, NYTOX Variables for TOXXFILE Option, 9/29/92
         IF (ITAB .LT. 0) THEN
C           First Receptor Network Defined - Set Variables
            ITAB  = 2
            NXTOX = ICOUNT
            NYTOX = JCOUNT
         ELSE
C           Previous Receptors Have Been Defined - Reset ITAB = 0
            ITAB = 0
         END IF
      END IF

C     Setup The AZELEV Array
      CALL SBYVAL(ZETMP1,ZETMP2,IZE)
      ISET = IRZE
      DO I = 1, IZE
         ISET = ISET + 1
         IF (ISET .GT. NREC) THEN
C           Error Msg: Maximum Number Of Receptor Exceeded
C           This shouldn't occur since limits are dynamically allocated
            WRITE(DUMMY,'(''NREC='',I7)') NREC
            CALL ERRHDL(PATH,MODNAM,'E','290',DUMMY)
            GO TO 999
         END IF
         AZELEV(ISET) = ZETMP2(I)
      END DO
      IRZE = ISET

C     Setup The AZHILL Array
      CALL SBYVAL(ZHTMP1,ZHTMP2,IZH)
      ISET = IRZH
      DO I = 1, IZH
         ISET = ISET + 1
         IF (ISET .GT. NREC) THEN
C           Error Msg: Maximum Number Of Receptor Exceeded
C           This shouldn't occur since limits are dynamically allocated
            WRITE(DUMMY,'(''NREC='',I7)') NREC
            CALL ERRHDL(PATH,MODNAM,'E','290',DUMMY)
            GO TO 999
         END IF
         AZHILL(ISET) = ZHTMP2(I)
      END DO
      IRZH = ISET

C     Setup The AZFLAG Aarry
      CALL SBYVAL(ZFTMP1,ZFTMP2,IZF)
      ISET = IRZF
      DO I = 1, IZF
         ISET = ISET + 1
         IF (ISET .GT. NREC) THEN
C           Error Msg: Maximum Number Of Receptor Exceeded
C           This shouldn't occur since limits are dynamically allocated
            WRITE(DUMMY,'(''NREC='',I7)') NREC
            CALL ERRHDL(PATH,MODNAM,'E','290',DUMMY)
            GO TO 999
         END IF
         AZFLAG(ISET) = ZFTMP2(I)
      END DO
      IRZF = ISET

      DO I = IDC1+1, IRXR
         NETID(I) = NETIDT
         RECTYP(I) = 'GC'
      END DO

 999  RETURN
      END

      SUBROUTINE REPOLR
C***********************************************************************
C                 REPOLR Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes Polar Grid Receptor Network Inputs
C
C        PROGRAMMER:  Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Polar Receptor Network Inputs
C
C        CALLED FROM:   RECARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER       :: I
      INTEGER, SAVE :: IORSET, IXRSET, IDRSET, IGRSET

C     Variable Initializations
      MODNAM = 'REPOLR'

      IF (IFC .LT. 3) THEN
C        Write Error Message: Missing Data Field
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      END IF

C     READ in the Netid and Nettype
      NETIDT = FIELD(3)
      IF (.NOT.NEWID .AND. (NETIDT.EQ.'    ' .OR.
     &    NETIDT.EQ.'ORIG' .OR. NETIDT.EQ.'DIST' .OR.
     &    NETIDT.EQ.'DDIR' .OR. NETIDT.EQ.'ELEV' .OR.
     &    NETIDT.EQ.'HILL' .OR.
     &    NETIDT.EQ.'FLAG' .OR. NETIDT.EQ.'GDIR' .OR.
     &    NETIDT.EQ.'END')) THEN
         NETIDT = PNETID
         KTYPE = FIELD(3)
      ELSE IF (.NOT.NEWID .AND. NETIDT.EQ.PNETID) THEN
         KTYPE = FIELD(4)
      ELSE IF (NEWID .AND. NETIDT.NE.'    ') THEN
         NEWID = .FALSE.
         KTYPE = FIELD(4)
C        The Keyword Counter
         INNET = INNET + 1
         IF (INNET .GT. NNET) THEN
C           WRITE Error Message:  Too Many Networks
C           This shouldn't occur since limits are dynamically allocated
            WRITE(DUMMY,'(''NNET='',I7)') NNET
            CALL ERRHDL(PATH,MODNAM,'E','290',DUMMY)
            RECERR = .TRUE.
            GO TO 999
         END IF
         IORSET = 0
         IXRSET = 0
         IDRSET = 0
         IGRSET = 0
         IEVSET = 0
         IFGSET = 0
      ELSE
C        Error Message: Invalid Secondary Keyword
         CALL ERRHDL(PATH,MODNAM,'E','170',PNETID)
         RECERR = .TRUE.
         GO TO 999
      END IF

C     Start to Set Up the Network
      IF (KTYPE .EQ. 'STA') THEN
         ISTA = .TRUE.
         IEND = .FALSE.
         NEWID = .FALSE.
         RECERR = .FALSE.
         ICOUNT = 0
         JCOUNT = 0
         IZE = 0
         IZH = 0
         IZF = 0
         IDC1 = IRXR
C        Check for Previous Grid Network With Same ID
         DO I = 1, INNET-1
            IF (FIELD(3) .EQ. NTID(I)) THEN
C              WRITE Warning Message:  Duplicate Network ID
               CALL ERRHDL(PATH,MODNAM,'W','252',NTID(I))
            END IF
         END DO
      ELSE IF (KTYPE .EQ. 'ORIG') THEN
         IF (.NOT. ISTA) THEN
C*          Write Error: MISSING STA OF THE BLOCK DATA
            CALL ERRHDL(PATH,MODNAM,'E','200','  STA   ')
         END IF
C        Error Message: Conflict Secondary Keyword
         IF (IORSET .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','160',NETIDT)
         END IF
C        Read In XINT, YINT                                 ---   CALL POLORG
         CALL POLORG
         IORSET = IORSET + 1
      ELSE IF (KTYPE .EQ. 'DIST') THEN
         IF (.NOT. ISTA) THEN
C*          Write Error: MISSING STA OF THE BLOCK DATA
            CALL ERRHDL(PATH,MODNAM,'E','200','  STA   ')
         END IF
C        Read in the Distance Set                           ---   CALL POLDST
         CALL POLDST
         IXRSET = IXRSET + 1
      ELSE IF (KTYPE .EQ. 'GDIR') THEN
         IF (.NOT. ISTA) THEN
C*          Write Error: MISSING STA OF THE BLOCK DATA
            CALL ERRHDL(PATH,MODNAM,'E','200','  STA   ')
         END IF
C        Error Message: Conflict Secondary Keyword
         IF (IDRSET .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','180',NETIDT)
         END IF
C        Set the Uniform Spacing Receptor Network           ---   CALL GENPOL
         CALL GENPOL
         IGRSET = IGRSET + 1
      ELSE IF (KTYPE .EQ. 'DDIR') THEN
         IF (.NOT. ISTA) THEN
C*          Write Error: MISSING STA OF THE BLOCK DATA
            CALL ERRHDL(PATH,MODNAM,'E','200','  STA   ')
         END IF
C        Error Message: Conflict Secondary Keyword
         IF (IGRSET .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','180',NETIDT)
         END IF
C        Set the Non-uniform Spacing Receptor Network       ---   CALL RADRNG
         CALL RADRNG
         IDRSET = IDRSET + 1
      ELSE IF (KTYPE .EQ. 'ELEV') THEN
         IF (.NOT. ISTA) THEN
C*          Write Error: MISSING STA OF THE BLOCK DATA
            CALL ERRHDL(PATH,MODNAM,'E','200','  STA   ')
         END IF
C        Read in and set the Terrain Elevation              ---   CALL TERHGT
         CALL TERHGT
         IEVSET = IEVSET + 1
      ELSE IF (KTYPE .EQ. 'HILL') THEN
         IF (.NOT. ISTA) THEN
C*          Write Error: MISSING STA OF THE BLOCK DATA
            CALL ERRHDL(PATH,MODNAM,'E','200','  STA   ')
         END IF
C        Read in and set the Terrain Elevation              ---   CALL HILHGT
         CALL HILHGT
         IHLSET = IHLSET + 1
      ELSE IF (KTYPE .EQ. 'FLAG') THEN
         IF (.NOT. ISTA) THEN
C*          Write Error: MISSING STA OF THE BLOCK DATA
            CALL ERRHDL(PATH,MODNAM,'E','200','  STA   ')
         END IF
C        Read in and set the Flagpole Receptor              ---   CALL FLGHGT
         CALL FLGHGT
         IFGSET = IFGSET + 1
      ELSE IF (KTYPE .EQ. 'END') THEN
         IEND = .TRUE.
C        Get the Final Result
         IF (.NOT. ISTA) THEN
C           Write Error: MISSING STA OF THE BLOCK DATA
            CALL ERRHDL(PATH,MODNAM,'E','200','  STA   ')
         ELSE IF (.NOT. RECERR) THEN
            CALL SETPOL
         END IF
         ISTA = .FALSE.
         NEWID = .TRUE.
C        Check If The Secondary Parameter Has Been Specified
C        Warning Message: Missing (Xin,Yin) Point Setting
         IF (IORSET .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'W','220',NETIDT)
            XINT = 0.0D0
            YINT = 0.0D0
         END IF
C        Error Message: Missing Distance Point Setting
         IF (IXRSET .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','221',NETIDT)
         END IF
C        Error Message: Missing Degree Or Rad Setting
         IF (IGRSET.EQ.0 .AND. IDRSET.EQ.0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','221',NETIDT)
         END IF

C        Warning: Elevated Terrain Inputs Inconsistent With Options
         IF (ELEV .AND. (IEVSET.EQ.0 .OR. IHLSET.EQ.0)) THEN
            CALL ERRHDL(PATH,MODNAM,'W','214',NETIDT)
            IRZE = IRXR
            IRZH = IRZE
         ELSE IF (FLAT .AND. IEVSET.NE.0) THEN
            CALL ERRHDL(PATH,MODNAM,'W','213',NETIDT)
            IRZE = IRXR
            IRZH = IRZE
         ELSE IF (FLAT .AND. IEVSET.EQ.0) THEN
            IRZE = IRXR
            IRZH = IRZE
         END IF

C        Warning: Flagpole Receptor Inputs Inconsistent With Options
         IF (FLGPOL .AND. IFGSET.EQ.0) THEN
            CALL ERRHDL(PATH,MODNAM,'W','216',NETIDT)
            IRZF = IRXR
         ELSE IF (.NOT.FLGPOL .AND. IFGSET.NE.0) THEN
            CALL ERRHDL(PATH,MODNAM,'W','215',NETIDT)
            IRZF = IRXR
         ELSE IF (.NOT.FLGPOL .AND. IFGSET.EQ.0) THEN
            IRZF = IRXR
         END IF

C        Check If The Number of Elev & Flag Is Match
         IF (ELEV .AND. IEVSET.NE.0) THEN
            IF (ICOUNT*JCOUNT .NE. IZE) THEN
C              Write Out The Error Message: No. Of ELEV not match
               CALL ERRHDL(PATH,MODNAM,'E','218','ELEV')
            END IF
            IF (ICOUNT*JCOUNT .NE. IZH) THEN
C              Write Out The Error Message: No. Of ZHILL not match
               CALL ERRHDL(PATH,MODNAM,'E','218','ZHILL')
            END IF
         END IF
         IF (FLGPOL .AND. IFGSET.NE.0) THEN
            IF (ICOUNT*JCOUNT .NE. IZF) THEN
C              Write Out The Error Message: No. Of FLAG not match
               CALL ERRHDL(PATH,MODNAM,'E','218','FLAG')
            END IF
         END IF

      ELSE
C        Error Message: Invalid Secondary Keyword
         CALL ERRHDL(PATH,MODNAM,'E','170',NETIDT)
         RECERR = .TRUE.
         GO TO 999

      END IF

      PNETID = NETIDT

 999  RETURN
      END

      SUBROUTINE POLORG
C***********************************************************************
C                 POLORG Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Input The Original of The Polar Network
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Polar Network Origin  Coordinates
C
C        CALLED FROM:   REPOLR
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, ISDX
      CHARACTER (LEN=12) :: SOID
      LOGICAL FOUND

C     Variable Initializations
      MODNAM = 'POLORG'
      FOUND = .FALSE.

C     Check for the Location of the Secondary Keyword, ORIG
      DO I = 1, IFC
         IF (FIELD(I) .EQ. 'ORIG') THEN
            ISC = I + 1
         END IF
      END DO

C     Determine Whether There Are Enough Parameter Fields
      IF (IFC .EQ. ISC-1) THEN
C        Error Message: Missing Parameter
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         RECERR = .TRUE.
         GO TO 999
      ELSE IF (IFC .GT. ISC+1) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KTYPE)
         RECERR = .TRUE.
         GO TO 999
      END IF

      IF (IFC .EQ. ISC) THEN
C*       Identify Origin Associated With a Source ID
C*       First check for length of SRCID field <=12
         IF ((LOCE(ISC)-LOCB(ISC)) .LE. 11) THEN
C*          Retrieve Source ID Character Substring
            SOID = FIELD(ISC)
         ELSE
C*          WRITE Error Message:  Source ID Field is Too Long
            CALL ERRHDL(PATH,MODNAM,'E','230',FIELD(ISC)(1:12))
            RECERR = .TRUE.
            GO TO 999
         END IF
C*       Check for valid SRCID         
         CALL SINDEX(SRCID,NSRC,SOID,ISDX,FOUND)
         IF (.NOT. FOUND) THEN
C           Error Message: Source ID Does Not Match Existing Sources
            CALL ERRHDL(PATH,MODNAM,'E','300',KEYWRD)
            RECERR = .TRUE.
         ELSE
            XINT = AXS(ISDX)
            YINT = AYS(ISDX)
         END IF

      ELSE
C        Input Numerical Values, XINT and YINT
         CALL STODBL(FIELD(ISC),ILEN_FLD,XINT,IMIT)
C        Check The Numerical Field
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            RECERR = .TRUE.
         END IF

         CALL STODBL(FIELD(ISC + 1),ILEN_FLD,YINT,IMIT)
C        Check The Numerical Field
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            RECERR = .TRUE.
         END IF
      END IF

 999  RETURN
      END

      SUBROUTINE POLDST
C***********************************************************************
C                 POLDST Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Gets Distances for the Polar Network
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Polar Network Distance Input Value
C
C        CALLED FROM:   REPOLR
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, J

C     Variable Initializations
      MODNAM = 'POLDST'

C     Skip the Unrelated Fields
      DO I = 1, IFC
         IF (FIELD(I) .EQ. 'DIST') THEN
            ISC = I + 1
         END IF
      END DO

C     Determine Whether There Are Enough Parameter Fields
      IF (IFC .EQ. ISC-1) THEN
C        Error Message: Missing Parameter
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         RECERR = .TRUE.
         GO TO 999
      END IF

      ISET = ICOUNT

      DO I = ISC, IFC
         CALL STODBL(FIELD(I),ILEN_FLD,DNUM,IMIT)
C        Check The Numerical Field
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            RECERR = .TRUE.
         END IF
         ISET = ISET + 1
         IF (ISET .LE. IXM) THEN
C           Store Distance to XCOORD Array and Check for Previous Occurrence
            XCOORD(ISET,INNET) = DNUM
            DO J = 1, ISET-1
               IF (DNUM .EQ. XCOORD(J,INNET)) THEN
C                 WRITE Warning Message:  Distance Specified More Than Once
                  CALL ERRHDL(PATH,MODNAM,'W','250',NETIDT)
               END IF
            END DO
         ELSE
C           Error Msg: Maximum Number Of X-coordinates Exceeded
C           This shouldn't occur since limits are dynamically allocated
            WRITE(DUMMY,'(''IXM ='',I7)') IXM
            CALL ERRHDL(PATH,MODNAM,'E','290',DUMMY)
            RECERR = .TRUE.
         END IF
      END DO

      ICOUNT = ISET

 999  RETURN
      END

      SUBROUTINE GENPOL
C***********************************************************************
C                 GENPOL Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Generates Polar Receptor Network With
C                 Uniform Spacing
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Polar Receptor Network With Uniform Direction Spacing
C
C        CALLED FROM:   REPOLR
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, J, K
      DOUBLE PRECISION :: DIRINI, DIRINC, TEMPP(3)
      LOGICAL ERROR

C     Variable Initializations
      MODNAM = 'GENPOL'
      ERROR = .FALSE.

C     Check for the Location of the Secondary Keyword, GDIR
      DO I = 1, IFC
         IF (FIELD(I) .EQ. 'GDIR') THEN
            ISC = I + 1
         END IF
      END DO

C     Determine Whether There Are Enough Parameter Fields
      IF (IFC .EQ. ISC-1) THEN
C        Error Message: Missing Parameter
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         RECERR = .TRUE.
         GO TO 999
      ELSE IF (IFC .LT. ISC+2) THEN
C        Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KTYPE)
         RECERR = .TRUE.
         GO TO 999
      ELSE IF (IFC .GT. ISC+2) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KTYPE)
         RECERR = .TRUE.
         GO TO 999
      END IF

C     Input Numerical Values
      DO K = 1, 3
         CALL STODBL(FIELD(ISC + K-1),ILEN_FLD,TEMPP(K),IMIT)
C        Check The Numerical Field
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            RECERR = .TRUE.
            ERROR = .TRUE.
         END IF
      END DO

      IF (ERROR) THEN
         ERROR = .FALSE.
         GO TO 999
      END IF

      JCOUNT = IDNINT(TEMPP(1))
      DIRINI = TEMPP(2)
      DIRINC = TEMPP(3)

C     Assign Them to the Coordinate Arrays
      IF (JCOUNT .LE. IYM) THEN
         DO J = 1, JCOUNT
            YCOORD(J,INNET) = (DIRINI + DIRINC*DBLE(J-1))
            IF (YCOORD(J,INNET) .GT. 360.0D0) THEN
               YCOORD(J,INNET) = YCOORD(J,INNET) - 360.0D0
            ELSE IF (YCOORD(J,INNET) .LE. 0.0D0) THEN
               YCOORD(J,INNET) = YCOORD(J,INNET) + 360.0D0
            END IF
         END DO
      ELSE
C        Error Msg: Maximum Number Of Y-coordinates Exceeded
C        This shouldn't occur since limits are dynamically allocated
         WRITE(DUMMY,'(''IYM ='',I7)') IYM
         CALL ERRHDL(PATH,MODNAM,'E','290',DUMMY)
         RECERR = .TRUE.
      END IF

 999  RETURN
      END

      SUBROUTINE RADRNG
C***********************************************************************
C                 RADRNG Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes Non-Uniform Polar Network Value
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Polar Network Directions in Non-Uniform Spacing
C
C        CALLED FROM:   REPOLR
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, J

C     Variable Initializations
      MODNAM = 'RADRNG'

C     Skip the non-useful Fields
      DO I = 1, IFC
         IF (FIELD(I) .EQ. 'DDIR') THEN
            ISC = I + 1
         END IF
      END DO

C     Determine Whether There Are Enough Parameter Fields
      IF (IFC .EQ. ISC-1) THEN
C        Error Message: Missing Parameter
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         RECERR = .TRUE.
         GO TO 999
      END IF

      ISET = JCOUNT

      DO I = ISC, IFC
         CALL STODBL(FIELD(I),ILEN_FLD,DNUM,IMIT)
C        Check The Numerical Field
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            RECERR = .TRUE.
         END IF
         ISET = ISET + 1
         IF (ISET .LE. IYM) THEN
C           Store Direction to YCOORD Array, Adjust to 0-360 Range if Needed,
C           and Check for Previous Occurrence
            YCOORD(ISET,INNET) = DNUM
            IF (YCOORD(ISET,INNET) .GT. 360.0D0) THEN
               YCOORD(ISET,INNET) = YCOORD(ISET,INNET) - 360.0D0
            ELSE IF (YCOORD(ISET,INNET) .LE. 0.0D0) THEN
               YCOORD(ISET,INNET) = YCOORD(ISET,INNET) + 360.0D0
            END IF
            DO J = 1, ISET-1
               IF (DNUM .EQ. YCOORD(J,INNET)) THEN
C                 WRITE Warning Message:  Direction Specified More Than Once
                  CALL ERRHDL(PATH,MODNAM,'W','250',NETIDT)
               END IF
            END DO
         ELSE
C           Error Msg: Maximum Number Of Y-coordinates Exceeded
C           This shouldn't occur since limits are dynamically allocated
            WRITE(DUMMY,'(''IYM ='',I7)') IYM
            CALL ERRHDL(PATH,MODNAM,'E','290',DUMMY)
            RECERR = .TRUE.
         END IF
      END DO

      JCOUNT = ISET

 999  RETURN
      END

      SUBROUTINE SETPOL
C***********************************************************************
C                 SETPOL Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Setup the Final Polar Receptor Network Inputs
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:  To Include TOXXFILE Option - 9/29/92
C
C        INPUTS:  The GRIDPOLR Sub-pathway Input Parameters
C
C        OUTPUTS: Polar Receptor Network Arrays
C
C        CALLED FROM:   REPOLR
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, J, JSET
      DOUBLE PRECISION :: YTEMP

C     Variable Initializations
      MODNAM = 'SETPOL'

      IF (ICOUNT.NE.0 .AND. JCOUNT.NE.0) THEN
C        Setup The Coordinate Of The Receptors
         NETSTA(INNET) = IRXR + 1
         ISET = IRXR
         JSET = IRYR
         DO J = 1, JCOUNT
            DO I = 1, ICOUNT
               ISET = ISET + 1
               JSET = JSET + 1
               IF (ISET .GT. NREC) THEN
C                 Error Msg: Maximum Number Of Receptor Exceeded
C                 This shouldn't occur since limits are dynamically allocated
                  WRITE(DUMMY,'(''NREC='',I7)') NREC
                  CALL ERRHDL(PATH,MODNAM,'E','290',DUMMY)
                  GO TO 999
               END IF
               IF (ICOUNT .GT. IXM) THEN
C                 Error Msg: Maximum Number Of X-coordinates Exceeded
C                 This shouldn't occur since limits are dynamically allocated
                  WRITE(DUMMY,'(''IXM ='',I7)') IXM
                  CALL ERRHDL(PATH,MODNAM,'E','290',DUMMY)
                  RECERR = .TRUE.
               END IF
               IF (JCOUNT .GT. IYM) THEN
C                 Error Msg: Maximum Number Of Y-coordinates Exceeded
C                 This shouldn't occur since limits are dynamically allocated
                  WRITE(DUMMY,'(''IYM ='',I7)') IYM
                  CALL ERRHDL(PATH,MODNAM,'E','290',DUMMY)
                  RECERR = .TRUE.
               END IF
               YTEMP = YCOORD(J,INNET) * DTORAD
               AXR(ISET) = XINT + XCOORD(I,INNET)*DSIN(YTEMP)
               AYR(JSET) = YINT + XCOORD(I,INNET)*DCOS(YTEMP)
            END DO
         END DO
         IRXR = ISET
         IRYR = JSET
         XORIG(INNET)  = XINT
         YORIG(INNET)  = YINT
         NETEND(INNET) = IRXR
         NUMXPT(INNET) = ICOUNT
         NUMYPT(INNET) = JCOUNT
         NTID(INNET)   = NETIDT
         NTTYP(INNET)  = 'GRIDPOLR'
C        Define ITAB, NXTOX, NYTOX Variables for TOXXFILE Option, 9/29/92
         IF (ITAB .LT. 0) THEN
C           First Receptor Network Defined - Set Variables
            ITAB  = 1
            NXTOX = ICOUNT
            NYTOX = JCOUNT
         ELSE
C           Previous Receptors Have Been Defined - Reset ITAB = 0
            ITAB = 0
         END IF
      END IF

C     Setup The AZELEV Array
      CALL SBYVAL(ZETMP1,ZETMP2,IZE)
      ISET = IRZE
      DO I = 1, IZE
         ISET = ISET + 1
         IF (ISET .GT. NREC) THEN
C           Error Msg: Maximum Number Of Receptor Exceeded
C           This shouldn't occur since limits are dynamically allocated
            WRITE(DUMMY,'(''NREC='',I7)') NREC
            CALL ERRHDL(PATH,MODNAM,'E','290',DUMMY)
            GO TO 999
         END IF
         AZELEV(ISET) = ZETMP2(I)
      END DO
      IRZE = ISET

C     Setup The AZHILL Array
      CALL SBYVAL(ZHTMP1,ZHTMP2,IZH)
      ISET = IRZH
      DO I = 1, IZH
         ISET = ISET + 1
         IF (ISET .GT. NREC) THEN
C           Error Msg: Maximum Number Of Receptor Exceeded
C           This shouldn't occur since limits are dynamically allocated
            WRITE(DUMMY,'(''NREC='',I7)') NREC
            CALL ERRHDL(PATH,MODNAM,'E','210',DUMMY)
            GO TO 999
         END IF
         AZHILL(ISET) = ZHTMP2(I)
      END DO
      IRZH = ISET

C     Setup The AZFLAG Array
      CALL SBYVAL(ZFTMP1,ZFTMP2,IZF)
      ISET = IRZF
      DO I = 1, IZF
         ISET = ISET + 1
         IF (ISET .GT. NREC) THEN
C           Error Msg: Maximum Number Of Receptor Exceeded
C           This shouldn't occur since limits are dynamically allocated
            WRITE(DUMMY,'(''NREC='',I7)') NREC
            CALL ERRHDL(PATH,MODNAM,'E','290',DUMMY)
            GO TO 999
         END IF
         AZFLAG(ISET) = ZFTMP2(I)
      END DO
      IRZF = ISET

      DO I = IDC1+1, IRXR
         NETID(I) = NETIDT
         RECTYP(I) = 'GP'
      END DO

 999  RETURN
      END

      SUBROUTINE TERHGT
C***********************************************************************
C                 TERHGT Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes Elevated Terrain Inputs for Receptor Network
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To trap on array subscript out-of-bounds
C                    when saving inputs to temporary arrays,
C                    which can occur if there's an input error
C                    in defining the receptor grid.
C                    R.W. Brode, PES, 4/2/99
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Elevated Terrain Input for a Receptor Network
C
C        CALLED FROM:   RECART
C                       REPOLR
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, J
      DOUBLE PRECISION :: ROW

C     Variable Initializations
      MODNAM = 'TERHGT'

C     Check for the Location of the Secondary Keyword, ELEV
      DO I = 1, IFC
         IF (FIELD(I) .EQ. 'ELEV') THEN
            ISC = I + 1
         END IF
      END DO

C     Determine Whether There Are Enough Parameter Fields
      IF (IFC .EQ. ISC-1) THEN
C        Error Message: Missing Parameter
         CALL ERRHDL(PATH,MODNAM,'E','223',KTYPE)
         RECERR = .TRUE.
         GO TO 999
      ELSE IF (IFC .EQ. ISC) THEN
C        Error Message: Missing Numerical Field
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         RECERR = .TRUE.
         GO TO 999
      END IF

      CALL STODBL(FIELD(ISC),ILEN_FLD,DNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         RECERR = .TRUE.
      END IF
      ROW = DNUM

      ISET = IZE

      DO I = ISC+1, IFC
         CALL STODBL(FIELD(I),ILEN_FLD,DNUM,IMIT)
C        Check The Numerical Field
         IF (IMIT .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            RECERR = .TRUE.
         END IF
         DO J = 1, IMIT
            ISET = ISET + 1
            IF (ISET .LE. NREC) THEN
               ZETMP1(ISET) = ROW
               ZETMP2(ISET) = DNUM
            END IF
         END DO
      END DO

      IZE = ISET

 999  RETURN
      END

      SUBROUTINE HILHGT
C***********************************************************************
C                 HILHGT Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes Hill Height Scale Inputs for Receptor Network
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    May 31, 1995
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Hill Height Scale Input for a Receptor Network
C
C        CALLED FROM:   RECART
C                       REPOLR
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, J
      DOUBLE PRECISION :: ROW

C     Variable Initializations
      MODNAM = 'HILHGT'

C     Check for the Location of the Secondary Keyword, ELEV
      DO I = 1, IFC
         IF (FIELD(I) .EQ. 'HILL') THEN
            ISC = I + 1
         END IF
      END DO

C     Determine Whether There Are Enough Parameter Fields
      IF (IFC .EQ. ISC-1) THEN
C        Error Message: Missing Parameter
         CALL ERRHDL(PATH,MODNAM,'E','223',KTYPE)
         RECERR = .TRUE.
         GO TO 999
      ELSE IF (IFC .EQ. ISC) THEN
C        Error Message: Missing Numerical Field
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         RECERR = .TRUE.
         GO TO 999
      END IF

      CALL STODBL(FIELD(ISC),ILEN_FLD,DNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         RECERR = .TRUE.
      END IF
      ROW = DNUM

      ISET = IZH

      DO I = ISC+1, IFC
         CALL STODBL(FIELD(I),ILEN_FLD,DNUM,IMIT)
C        Check The Numerical Field
         IF (IMIT .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            RECERR = .TRUE.
         END IF
         DO J = 1, IMIT
            ISET = ISET + 1
            IF (ISET .LE. NREC) THEN
               ZHTMP1(ISET) = ROW
               ZHTMP2(ISET) = DNUM
            END IF
         END DO
      END DO

      IZH = ISET

 999  RETURN
      END

      SUBROUTINE FLGHGT
C***********************************************************************
C                 FLGHGT Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes Flagpole Receptor Heights for Receptor Network
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To trap on array subscript out-of-bounds
C                    when saving inputs to temporary arrays,
C                    which can occur if there's an input error
C                    in defining the receptor grid.
C                    R.W. Brode, PES, 4/2/99
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Flagpole Receptor Heights for a Receptor Network
C
C        CALLED FROM:   RECART
C                       REPOLR
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, J
      DOUBLE PRECISION :: ROW

C     Variable Initializations
      MODNAM = 'FLGHGT'

C     Check for the Location of the Secondary Keyword, FLAG
      DO I = 1, IFC
         IF (FIELD(I) .EQ. 'FLAG') THEN
            ISC = I + 1
         END IF
      END DO

C     Determine Whether There Are Enough Parameter Fields
      IF (IFC .EQ. ISC-1) THEN
C        Error Message: Missing Parameter
         CALL ERRHDL(PATH,MODNAM,'E','223',KTYPE)
         RECERR = .TRUE.
         GO TO 999
      ELSE IF (IFC .EQ. ISC) THEN
C        Error Message: Missing Numerical Field
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         RECERR = .TRUE.
         GO TO 999
      END IF

      CALL STODBL(FIELD(ISC),ILEN_FLD,DNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         RECERR = .TRUE.
      END IF
      ROW = DNUM

      ISET = IZF

      DO I = ISC+1, IFC
         CALL STODBL(FIELD(I),ILEN_FLD,DNUM,IMIT)
C        Check The Numerical Field
         IF (IMIT .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            RECERR = .TRUE.
         END IF
         DO J = 1, IMIT
            ISET = ISET + 1
            IF (ISET .LE. NREC) THEN
               ZFTMP1(ISET) = ROW
               ZFTMP2(ISET) = DNUM
            END IF
         END DO
      END DO

      IZF = ISET

 999  RETURN
      END

      SUBROUTINE DISCAR
C***********************************************************************
C                 DISCAR Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes Discrete Cartesian Receptor Location Inputs
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:  To include warning message for ELEV and .non.FLGPOL
C                   case, R.W. Brode, MACTEC/PES, 8/25/03
C
C        MODIFIED:  To Include TOXXFILE Option - 9/29/92
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Discrete Cartesian Receptor Location Inputs
C
C        CALLED FROM:   RECARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I1, I2, I3, I4, I5

C     Variable Initializations
      MODNAM = 'DISCAR'
      I1 = IRXR
      I2 = IRYR
      I3 = IRZE
      I4 = IRZF
      I5 = IRZH
C     Determine Whether There Are Too Few Or Too Many Parameter Fields
      IF (IFC .LT. 4) THEN
C        WRITE Error Message: Missing Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 7) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      ELSE IF (ELEV .AND. FLGPOL .AND. IFC.LT.7) THEN
C        WRITE Warning Message: Default(s) Used for Missing Parameter(s)
         CALL ERRHDL(PATH,MODNAM,'W','228',KEYWRD)
      ELSE IF (ELEV .AND. .NOT.FLGPOL .AND. IFC .LT. 6) THEN
C        WRITE Warning Message: Default(s) Used for Missing Parameter(s)
         CALL ERRHDL(PATH,MODNAM,'W','228',KEYWRD)
      ELSE IF (ELEV .AND. .NOT.FLGPOL .AND. IFC .GT. 6) THEN
C        WRITE Warning Message: Parameter Ignored, ZFLAG
         CALL ERRHDL(PATH,MODNAM,'W','229',KEYWRD)
      ELSE IF (FLGPOL .AND. .NOT.ELEV .AND. IFC .GT. 5) THEN
C        WRITE Warning Message: Parameter Ignored, ZELEV & ZHILL
         CALL ERRHDL(PATH,MODNAM,'W','229',KEYWRD)
      ELSE IF (.NOT.ELEV .AND. .NOT.FLGPOL .AND. IFC .GT. 4) THEN
C        WRITE Warning Message: Parameters Ignored, ZELEV ZHILL & ZFLAG
         CALL ERRHDL(PATH,MODNAM,'W','229',KEYWRD)
      END IF

C     Check Whether The Maximum Number of Receptors is Exceeded
      IF (I1.EQ.NREC .OR. I2.EQ.NREC .OR. I3.EQ.NREC .OR.
     &    I4.EQ.NREC .OR. I5.EQ.NREC) THEN
C        Error Msg: Maximum Number Of Receptors Exceeded
C        This shouldn't occur since limits are dynamically allocated
         WRITE(DUMMY,'(''NREC='',I7)') NREC
         CALL ERRHDL(PATH,MODNAM,'E','290',DUMMY)
         GO TO 999
      END IF

C     READ XCOORD,YCOORD,ELEV,ZHILL,FLAG And Assign Them to Different
C     Arrays

      CALL STODBL(FIELD(3),ILEN_FLD,DNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      ELSE
         AXR(I1 + 1) = DNUM
      END IF

      CALL STODBL(FIELD(4),ILEN_FLD,DNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      ELSE
         AYR(I2 + 1) = DNUM
      END IF

      IF (ELEV .AND. FLGPOL) THEN
         IF (IFC .GE. 5) THEN
            CALL STODBL(FIELD(5),ILEN_FLD,DNUM,IMIT)
C           Check The Numerical Field
            IF (IMIT .NE. 1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE
               AZELEV(I3 + 1) = DNUM
            END IF
         END IF
         IF (IFC .GE. 6) THEN
            CALL STODBL(FIELD(6),ILEN_FLD,DNUM,IMIT)
C           Check The Numerical Field
            IF (IMIT .NE. 1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE
               AZHILL(I5 + 1) = DNUM
            END IF
         END IF
         IF (IFC .EQ. 7) THEN
            CALL STODBL(FIELD(7),ILEN_FLD,DNUM,IMIT)
C           Check The Numerical Field
            IF (IMIT .NE. 1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE
               AZFLAG(I4 + 1) = DNUM
            END IF
         END IF
      ELSE IF (ELEV .AND. .NOT.FLGPOL) THEN
         IF (IFC .GE. 5) THEN
            CALL STODBL(FIELD(5),ILEN_FLD,DNUM,IMIT)
C           Check The Numerical Field
            IF (IMIT .NE. 1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE
               AZELEV(I3 + 1) = DNUM
            END IF
         END IF
         IF (IFC .GE. 6) THEN
            CALL STODBL(FIELD(6),ILEN_FLD,DNUM,IMIT)
C           Check The Numerical Field
            IF (IMIT .NE. 1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE
               AZHILL(I5 + 1) = DNUM
            END IF
         ENDIF
      ELSE IF (FLGPOL .AND. .NOT.ELEV) THEN
         IF (IFC .EQ. 5) THEN
            CALL STODBL(FIELD(5),ILEN_FLD,DNUM,IMIT)
C           Check The Numerical Field
            IF (IMIT .NE. 1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE
               AZFLAG(I4 + 1) = DNUM
            END IF
         ELSE IF (IFC .EQ. 7) THEN
            CALL STODBL(FIELD(7),ILEN_FLD,DNUM,IMIT)
C           Check The Numerical Field
            IF (IMIT .NE. 1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE
               AZFLAG(I4 + 1) = DNUM
            END IF
         END IF
      END IF

      IRXR = I1 + 1
      IRYR = I2 + 1
      IRZE = I3 + 1
      IRZF = I4 + 1
      IRZH = I5 + 1
      NETID(IRXR) = ' '
      RECTYP(IRXR) = 'DC'
C     Reset ITAB Variable for TOXXFILE Option, 9/29/92
      ITAB = 0

 999  RETURN
      END

      SUBROUTINE DISPOL
C***********************************************************************
C                 DISPOL Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes Discrete Polar Receptor Location Inputs
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:  To include warning message for ELEV and .non.FLGPOL
C                   case, R.W. Brode, MACTEC/PES, 8/25/03
C
C        MODIFIED:  To Include TOXXFILE Option - 9/29/92
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Discrete Polar Receptor Location Inputs
C
C        CALLED FROM:   RECARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I1, I2, I3, I4, I5, ISDX
      DOUBLE PRECISION :: RANGE, DIRECT
      CHARACTER (LEN=12) :: SOID
      LOGICAL FOUND

C     Variable Initializations
      MODNAM = 'DISPOL'
      I1 = IRXR
      I2 = IRYR
      I3 = IRZE
      I4 = IRZF
      I5 = IRZH

C     Determine Whether There Are Too Few Or Too Many Parameter Fields
      IF (IFC .LT. 5) THEN
C        WRITE Error Message: Missing Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 8) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      ELSE IF (ELEV .AND. FLGPOL .AND. IFC.LT.8) THEN
C        WRITE Warning Message: Default(s) Used for Missing Parameter(s)
         CALL ERRHDL(PATH,MODNAM,'W','228',KEYWRD)
      ELSE IF (ELEV .AND. .NOT.FLGPOL .AND. IFC .LT. 7) THEN
C        WRITE Warning Message: Default(s) Used for Missing Parameter(s)
         CALL ERRHDL(PATH,MODNAM,'W','228',KEYWRD)
      ELSE IF (ELEV .AND. .NOT.FLGPOL .AND. IFC .GT. 7) THEN
C        WRITE Warning Message: Parameter Ignored, ZFLAG
         CALL ERRHDL(PATH,MODNAM,'W','229',' ZFLAG ')
      ELSE IF (FLGPOL .AND. .NOT.ELEV .AND. IFC .GT. 6) THEN
C        WRITE Warning Message: Parameter Ignored, ZELEV & ZHILL
         CALL ERRHDL(PATH,MODNAM,'W','229',KEYWRD)
      ELSE IF (.NOT.ELEV .AND. .NOT.FLGPOL .AND. IFC .GT. 5) THEN
C        WRITE Warning Message: Parameters Ignored, ZELEV ZHILL & ZFLAG
         CALL ERRHDL(PATH,MODNAM,'W','229',KEYWRD)
      END IF

C     Check Whether The Maximum Number of Receptors is Exceeded
      IF (I1.EQ.NREC .OR. I2.EQ.NREC .OR. I3.EQ.NREC .OR.
     &    I4.EQ.NREC .OR. I5.EQ.NREC) THEN
C        Error Msg: Maximum Number Of Receptors Exceeded
C        This shouldn't occur since limits are dynamically allocated
         WRITE(DUMMY,'(''NREC='',I7)') NREC
         CALL ERRHDL(PATH,MODNAM,'E','290',DUMMY)
         GO TO 999
      END IF

C     READ SRCID,RANGE,DIRECT,ELEV,FLAG

C*    First check for length of SRCID field <=12
      IF ((LOCE(3)-LOCB(3)) .LE. 11) THEN
C*       Retrieve Source ID Character Substring
         SOID = FIELD(3)
      ELSE
C*       WRITE Error Message:  Source ID Field is Too Long
         CALL ERRHDL(PATH,MODNAM,'E','230',FIELD(3)(1:12))
         RECERR = .TRUE.
         GO TO 999
      END IF

      CALL STODBL(FIELD(4),ILEN_FLD,RANGE,IMIT)
C     Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      END IF

      CALL STODBL(FIELD(5),ILEN_FLD,DIRECT,IMIT)
C     Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      ELSE IF (DIRECT .GT. 360.0D0) THEN
         DIRECT = DIRECT - 360.0D0
      ELSE IF (DIRECT .LE. 0.0D0) THEN
         DIRECT = DIRECT + 360.0D0
      END IF

      IF (ELEV .AND. FLGPOL) THEN
         IF (IFC .GE. 6) THEN
            CALL STODBL(FIELD(6),ILEN_FLD,DNUM,IMIT)
C           Check The Numerical Field
            IF (IMIT .NE. 1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE
               AZELEV(I3 + 1) = DNUM
            END IF
         END IF
         IF (IFC .GE. 7) THEN
            CALL STODBL(FIELD(7),ILEN_FLD,DNUM,IMIT)
C           Check The Numerical Field
            IF (IMIT .NE. 1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE
               AZHILL(I5 + 1) = DNUM
            END IF
         END IF
         IF (IFC .EQ. 8) THEN
            CALL STODBL(FIELD(8),ILEN_FLD,DNUM,IMIT)
C           Check The Numerical Field
            IF (IMIT .NE. 1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE
               AZFLAG(I4 + 1) = DNUM
            END IF
         END IF
      ELSE IF (ELEV .AND. .NOT.FLGPOL) THEN
         IF (IFC .GE. 6) THEN
            CALL STODBL(FIELD(6),ILEN_FLD,DNUM,IMIT)
C           Check The Numerical Field
            IF (IMIT .NE. 1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE
               AZELEV(I3 + 1) = DNUM
            END IF
         END IF
         IF (IFC .GE. 7) THEN
            CALL STODBL(FIELD(7),ILEN_FLD,DNUM,IMIT)
C           Check The Numerical Field
            IF (IMIT .NE. 1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE
               AZHILL(I5 + 1) = DNUM
            END IF
         ENDIF
      ELSE IF (FLGPOL .AND. .NOT.ELEV) THEN
         IF (IFC .EQ. 6) THEN
            CALL STODBL(FIELD(6),ILEN_FLD,DNUM,IMIT)
C           Check The Numerical Field
            IF (IMIT .NE. 1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE
               AZFLAG(I4 + 1) = DNUM
            END IF
         ELSE IF (IFC .EQ. 8) THEN
            CALL STODBL(FIELD(8),ILEN_FLD,DNUM,IMIT)
C           Check The Numerical Field
            IF (IMIT .NE. 1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE
               AZFLAG(I4 + 1) = DNUM
            END IF
         END IF
      END IF

C     Assign Them to Different Arrays,
C     Retrieve The Origin From Source Coordinates

      CALL SINDEX(SRCID,NSRC,SOID,ISDX,FOUND)
      IF (.NOT. FOUND) THEN
C        Error Message: Source ID Not Match
         CALL ERRHDL(PATH,MODNAM,'E','300',KEYWRD)
      ELSE
         AXR(I1 + 1) = AXS(ISDX) + RANGE*DSIN(DIRECT*DTORAD)
         AYR(I2 + 1) = AYS(ISDX) + RANGE*DCOS(DIRECT*DTORAD)
         IRXR = I1 + 1
         IRYR = I2 + 1
         IRZE = I3 + 1
         IRZF = I4 + 1
         IRZH = I5 + 1
C        Reset ITAB Variable for TOXXFILE Option, 9/29/92
         ITAB = 0
      END IF

      NETID(IRXR)  = ' '
      RECTYP(IRXR) = 'DP'
      IREF(IRXR)   = ISDX

 999  RETURN
      END

      SUBROUTINE SBYVAL(ARRIN1,ARRIN2,INX)
C***********************************************************************
C                 SBYVAL Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Sort Array By Its 'Index Value'
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  ARRIN1: 'Index Array',  ARRIN2: 'Value Array'
C                 INX: Number of Values to Sort
C
C        OUTPUTS: Sorted Array
C
C        CALLED FROM: (This Is A Utility Program)
C***********************************************************************
C
C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, INX, JC, IMIN
C     Declare Input Arrays as Assumed-Size Arrays (Currently Dimensioned NREC
C     in Calling Routines)
      DOUBLE PRECISION :: ARRIN1(*), ARRIN2(*), RMIN, TEMP1, TEMP2

C     Variable Initialization
      MODNAM = 'SBYVAL'
      JC = 1

      DO WHILE (JC .LE. INX)
C        Find out The First Minimum In the Array
         RMIN = ARRIN1(JC)
         IMIN = JC
         DO I = JC, INX
            IF (ARRIN1(I) .LT. RMIN) THEN
               IMIN = I
               RMIN = ARRIN1(I)
            END IF
         END DO
C        Swap The Selected Array Elements
         TEMP1 = ARRIN1(JC)
         TEMP2 = ARRIN2(JC)
         ARRIN1(JC) = ARRIN1(IMIN)
         ARRIN2(JC) = ARRIN2(IMIN)
         ARRIN1(IMIN) = TEMP1
         ARRIN2(IMIN) = TEMP2
C        Increment The Counter
         JC = JC + 1
      END DO

      RETURN
      END


      SUBROUTINE EVCART
C***********************************************************************
C                 EVCART Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes Discrete Cartesian Receptor Location Inputs
C                 for Use with the EVALFILE Option
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    November 29, 1993
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Discrete Cartesian Receptor Location Inputs
C                 With 'Arc' Grouping ID
C
C        CALLED FROM:   RECARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      INTEGER :: I1, I2, I3, I4, I5, J
      CHARACTER MODNAM*12

      LOGICAL FOUND

C     Variable Initializations
      MODNAM = 'EVCART'
      I1 = IRXR
      I2 = IRYR
      I3 = IRZE
      I4 = IRZF
      I5 = IRZH

C     Determine Whether There Are Too Few Or Too Many Parameter Fields
      IF (IFC .LT. 8) THEN
C        WRITE Error Message: Missing Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 9) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Check Whether The Maximum Number of Receptors is Exceeded
      IF (I1.EQ.NREC .OR. I2.EQ.NREC .OR. I3.EQ.NREC .OR.
     &    I4.EQ.NREC .OR. I5.EQ.NREC) THEN
C        Error Msg: Maximum Number Of Receptors Exceeded
C        This shouldn't occur since limits are dynamically allocated
         WRITE(DUMMY,'(''NREC='',I7)') NREC
         CALL ERRHDL(PATH,MODNAM,'E','290',DUMMY)
         GO TO 999
      END IF

C     READ XCOORD,YCOORD,ELEV,HILLZ,FLAG And Assign Them to Different 
C     Arrays

      CALL STODBL(FIELD(3),ILEN_FLD,DNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      ELSE
         AXR(I1 + 1) = DNUM
      END IF

      CALL STODBL(FIELD(4),ILEN_FLD,DNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      ELSE
         AYR(I2 + 1) = DNUM
      END IF

      CALL STODBL(FIELD(5),ILEN_FLD,DNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      ELSE
         AZELEV(I3 + 1) = DNUM
      END IF

      CALL STODBL(FIELD(6),ILEN_FLD,DNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      ELSE
         AZHILL(I5 + 1) = DNUM
      END IF

      CALL STODBL(FIELD(7),ILEN_FLD,DNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      ELSE
         AZFLAG(I4 + 1) = DNUM
      END IF

C     Read ARCID Field, First Check for Previous Occurrence of This ARCID
      FOUND = .FALSE.
      J = 1
      DO WHILE (.NOT.FOUND .AND. J.LE.NUMARC)
         IF (FIELD(8) .EQ. ARCID(J)) THEN
            FOUND = .TRUE.
            NDXARC(I1 + 1) = J
         END IF
         J = J + 1
      END DO
      IF (.NOT. FOUND) THEN
         NUMARC = NUMARC + 1
         IF (NUMARC .GT. NARC) THEN
C           Write Error Message:  Too Many ARCs
C           This shouldn't occur since limits are dynamically allocated
            WRITE(DUMMY,'(''NARC ='',I6)') NARC
            CALL ERRHDL(PATH,MODNAM,'E','290',DUMMY)
            GO TO 999
         ELSE
            ARCID(NUMARC)  = FIELD(8)
            NDXARC(I1 + 1) = NUMARC
         END IF
      END IF

      IRXR = I1 + 1
      IRYR = I2 + 1
      IRZE = I3 + 1
      IRZF = I4 + 1
      IRZH = I5 + 1
      NETID(IRXR) = ' '
      RECTYP(IRXR) = 'DC'
C     Reset ITAB Variable for TOXXFILE Option, 9/29/92
      ITAB = 0

 999  RETURN
      END

      SUBROUTINE BLRECP(KK)
C***********************************************************************
C                 BLRECP Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Performs a translation and initial rotation of the 
C                 receptors when buoyant line sources are processed
C
C        PROGRAMMER: Amec Foster Wheeler
C
C        DATE:    March 31, 2016
C
C        MODIFIED: Wood, December 2019 (Multiple_BuoyLines_D41_Wood)
C                  Many modifications to process receptors by buoyant
C                  line source group; argument KK identifies which BL
C                  group is being processed
C
C        INPUTS:  Receptor locations, translation and rotation parameters
C                 KK = Buoyant line source group number
C
C        OUTPUTS: Translated and rotated receptors with flag set
C                 indicating if a receptor is inside or outside the 
C                 rectangular footprint defined by a buoyant line group 
C
C        CALLED FROM:   RECARD
C***********************************************************************

C ---    If there is one or more buoyant line sources, the receptors 
C          need to be translated and rotated so they are oriented the 
C          same as the translation and rotation for each buoyant line 
C          source - use the buoyant line source endpoints to determine 
C          translation and rotation
C          (NOTE: this is an intial rotation based on the orientation of
C           buoyant line source relative to the x-y axes (with +y
C           pointing north); the receptors will be rotated again each
C           hour based on the wind direction)
C
C           XOR, YOR, COSTCOR, and SINTCOR are calculated in BL_ROTATE1
C            when the sources are processed; these parameters are
C            declared in the BUOYANT_LINE module
C
C           AERMOD does NOT allow the RE pathway to be processed before
C            the SO pathway so the necessary parameters for the rotation
C            are already defined.
C
C        BL_RFLAG indicates if a receptor is inside (.true.) or outside
C         (.false.) of the rectangle defined by the minimum and maximum
C         extents of the translation/first rotation of the buoyant line
C
C     Variable Declarations
      USE MAIN1
      USE BUOYANT_LINE
      IMPLICIT NONE

C JAT 06/22/21 D065
C REMOVE NVRECT AS UNUSED VARIABLE
      INTEGER :: I, KK, NRECIN, LNUM, NR !, NVRECT                         ! Multiple_BuoyLines_D41_Wood
      DOUBLE PRECISION :: EX,EY, XLMIN, XLMAX, YLMIN, YLMAX
C      DOUBLE PRECISION :: BLREC_X(4), BLREC_Y(4)                        ! Multiple_BuoyLines_D41_Wood
      CHARACTER MODNAM*12
C Unused: INTEGER :: ILSAVE

C     Variable Initializations
      MODNAM = 'BLRECEP'
      NRECIN = 0
C D41_Wood      BL_RFLAG = .false.

C --- Translate and rotate receptors for buoyant line source
      DO I = 1,NUMREC
C        Translate
         XR_SCS(I,KK) = AXR(I) - XOR(KK)
         YR_SCS(I,KK) = AYR(I) - YOR(KK)
         EX = XR_SCS(I,KK)
         EY = YR_SCS(I,KK)

C        Initial rotation
         EY = -EX*SINTCOR(KK) + EY*COSTCOR(KK)
         YR_SCS(I,KK) = EY
         EX = (EX + EY*SINTCOR(KK))/COSTCOR(KK)
         XR_SCS(I,KK) = EX
      END DO

C     Determine the min, max extents of the bouyant line source
C      after translation and first rotation by BL group.
C      These are used to determine if a receptor is inside the
C      'footprint' of the buoyant lines, the so-called exclusion zone
C      for the BL source, and are excluded from the calculations by 
C      setting a flag for each receptor
C      The first loop establishes the initial minimum and maximum values
C      for X and Y by finding the smallest X, Y for the maximum and the
C      largest X, Y for the minimum.  The second loop then compares each
C      lines beginning and end points to XLMIN, XLMAX, YLAMIN, YLMAX.

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

      DO LNUM = NBLTOTAL,1,-1
         IF (BLINEPARMS(LNUM)%IBLPGRPNUM == KK) THEN
            YLMIN = YS_SCS(LNUM)
            EXIT
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

C     Define the vertices of the rectangular footprint defined by a 
C     buoyant line source that defines the exclusion zone - 
C     coordinates are specified counterclockwise

C Multiple_BuoyLines_D41_Wood
C     The following are never used - commented out for now
C      NVRECT = 4
C      BLREC_X(1) = XLMIN
C      BLREC_X(2) = XLMAX
C      BLREC_X(3) = XLMAX
C      BLREC_X(4) = XLMIN
C      BLREC_Y(1) = YLMIN
C      BLREC_Y(2) = YLMIN
C      BLREC_Y(3) = YLMAX
C      BLREC_Y(4) = YLMAX

C     Set the flag buoyant line source flag to TRUE if the receptor
C      is inside the rectangular footprint defined by a buoyant line 
C      source (exclusion zone)
C
C Multiple_BuoyLines_D41_Wood
C     Set the receptor flag by BL group
      DO NR = 1,NUMREC
         IF( YR_SCS(NR,KK) .LE. (YLMAX + 0.1D0) .AND. 
     &          YR_SCS(NR,KK) .GE. (YLMIN - 0.1D0) .AND.
     &          XR_SCS(NR,KK) .LE. (XLMAX + 0.1D0) .AND. 
     &          XR_SCS(NR,KK) .GE. (XLMIN - 0.1D0)) THEN
            NRECIN = NRECIN + 1
            BL_RFLAG(NR,KK) = .true.
         END IF
      END DO

      WRITE(DUMMY,'(I3," (",A6,")")') NRECIN, BL_GRPID(KK)
      CALL ERRHDL(PATH,MODNAM,'W','476',DUMMY)

      RETURN
      END