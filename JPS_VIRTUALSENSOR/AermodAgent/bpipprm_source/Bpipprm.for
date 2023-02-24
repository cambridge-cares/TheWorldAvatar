C  **************************************************************************
C  *                                                                        *
C  *         BUILDING PROFILE INPUT PROGRAM for PRIME(DATED 04274)          *
C  *                                                                        *
C  *            *** SEE BPIPPRM MODEL CHANGE BULLETIN MCB#1 ***             *
C  *                                                                        *
C  *     ON THE SUPPORT CENTER FOR REGULATORY AIR MODELS BULLETIN BOARD     *
C  *                                                                        *
C  *                      http://www.epa.gov/scram001                       *
C  *                                                                        *
C  **************************************************************************
C
C        Programmed by: Peter Eckhoff
C                       US EPA
C                       4930 Page Rd, D243-01
C                       Research Triangle Park, NC 27711
C
C  **************************************************************************
C
C        Written to: FORTRAN 90 Standards
C
C  **************************************************************************
C
C        Modifications: June 9, 1995 - Created using BPIP code base. 
C                                    - Being treated as separate from BPIP
C
C                       June 9, 1995 - Added code to produce projected building
C                                      length for input to PRIME model
C                                      V. Tino, Earth Tech
C                       Prepared for EPRI under contract WO3527-01
C
C                       July 31, 1995 - Added code to produce upwind and cross-
C                                       wind distances from stack to midpoint
C                                       of upwind face of projected building
C                                       for input to PRIME model
C                                       D. McNaughton - Earth Tech
C                       Prepared for EPRI under contract WO3527-01
C
C                       January 23, 2004 - Added allocatable arrays.
C                                        - No IMPLICIT variables;  
C                                            all variables defined.
C                                        - Added a switch to calculate downwash
C                                            values for the PRIME algorithm
C                                            in ISCPRIME and AERMOD, etc. or
C                                            values for legacy ISCST, -LT runs.
C                                        - Reedited to Fortran 90 stds. using 
C                                            Compaq Visual Fortran version 6.6.
C                                          Peter Eckhoff - US EPA
C
C                       October 1, 2004 - Corrected code in Subroutine MXBWH where
C                                         three different variables were equated,
C                                         in error, to PBL.
C
C  **************************************************************************
C
C                               INPUT FORMAT
C
C      Note: The tier coordinates need to be entered in a clockwise or
C            counter-clockwise direction.  Input is free format.  Single
C            quotes need to be around 'character input strings'.
C
C   'Run description' (up to 78 characters)
C   'P', 'NP', 'ST', or 'LT' processing for PRIME, no PRIME, 
C                                           ISCST, or ISCLT algorithms 
C   'Input Units Name' Conversion to meters factor
C   'UTMN' (for no UTM) or 'UTMY'(for UTM coordinates processing), Plant North
C    Number of Buildings
C     'Building 1 Name', Number of Tiers for Building 1, Base Elevation
C      Number of Corners for Tier 1, Tier 1 Height
C       Tier 1 Corner 1 X -, and Y - Coordinates
C       Tier 1 Corner 2 X -, and Y - Coordinates
C       Tier 1 Corner c X -, and Y - Coordinates
C        .
C        .
C        .
C      Number of Corners for Tier t, Tier t Height
C       Tier t Corner 1 X -, and Y - Coordinates
C       Tier t Corner 2 X -, and Y - Coordinates
C       Tier t Corner c X -, and Y - Coordinates
C       .
C       .
C       .
C     'Building x Name', Number of Tiers for Building x
C      Number of Corners for Tier 1, Tier 1 Height
C       Tier 1 Corner 1 X -, and Y - Coordinates
C       Tier 1 Corner 2 X -, and Y - Coordinates
C       Tier 1 Corner c X -, and Y - Coordinates
C        .
C        .
C        .
C      Number of Corners for Tier t, Tier t Height
C       Tier t Corner 1 X -, and Y - Coordinates
C       Tier t Corner 2 X -, and Y - Coordinates
C       Tier t Corner c X -, and Y - Coordinates
C  Number of Stacks
C    'Stack 1 name', Base Elevation, Height, Stack 1 X -, and Y - Coordinates
C     .
C     .
C     .
C    'Stack s name', Base Elevation, Height, Stack s X -, and Y - Coordinates
C
C  **************************************************************************
C
C                            EXECUTION STATEMENT
C
C   DOS Prompt> BPIPPRM  Input_filename  Output_filename  Summary_filename
C
C  **************************************************************************
C
C                                UNIT USAGE
C
C              UNIT    PURPOSE
C
C                *  -  READ FROM THE KEYBOARD
C                *  -  WRITE TO THE SCREEN
C               10  -  READ INPUT FROM FILE
C               12  -  WRITE OUTPUT TO FILE
C               14  -  WRITE SUMMARY FILE
C
C  **************************************************************************
C
C                                DEFINITIONS
C
C   A      - TEMPORARY STORAGE VARIABLE ASSOCIATED WITH UTM COORDINATES
C   A1     - AREA OF A SQUARE SQUARED. USED TO DETERMINE IF AN INTERCEPT IS
C             BETWEEN TWO TIER CORNERS. A1 IS BASED ON THE CORNER COORDINATES
C             AND THE INTERCEPT COORDINATES.
C   A2     - AREA OF A SQUARE SQUARED. USED TO DETERMINE IF AN INTERCEPT IS
C             BETWEEN TWO TIER CORNERS. A2 IS BASED ON THE CORNER COORDINATES
C             ONLY.
C   AA     - DOUBLE PRECISION INPUT VALUE; GENERALLY AN X-COORDINATE
C          - ANGLE FORMED BETWEEN TWO ADJACENT CORNERS OF A TIER AND A STACK
C   ADJ    - ADJUST PLANT NORTH COORDINATES TO TRUE NORTH COORDINATES
C   ANG    - ANGLE THROUGH WHICH STACK AND TIERS ARE ROTATED SO THE WIND
C             DIRECTION IS POINTED "TRUE NORTH"
C   AP     - X-COORDINATE TRANSLATED FROM PLANT NORTH
C   AU     - TEMPORARY STORAGE VARIABLE ASSOCIATED WITH UTM COORDINATES
C   B      - TEMPORARY STORAGE VARIABLE ASSOCIATED WITH UTM COORDINATES
C   BB     - DOUBLE PRECISION INPUT VALUE; GENERALLY AN Y-COORDINATE
C   BELEV  - ARRAY OF BUILDING BASE ELEVATIONS
C   BELEVV - DUMMY VARIABLE USED DURING INITIAL READ TO MIMIC BELEV
C   BET    - VALUE IN CNRLIN THAT WHEN A PERPENDICULAR LINE DRAWN FROM A TIER
C             CORNER TO ANOTHER TIER SIDE INTERCEPTS THE SIDE BETWEEN THE TWO
C             CORNERS, BET IS POSITIVE.
CVT BL     - PROJECTED LENGTH OF A TIER OR A GROUP OF COMBINED TIERS
C   BP     - Y-COORDINATE TRANSLATED FROM PLANT NORTH
C   BTN    - NAME OF STRUCTURE
C   BTNV   - USED AS DUMMY VARIABLE IN INITIAL READ
C   BU     - TEMPORARY STORAGE VARIABLE ASSOCIATED WITH UTM COORDINATES
C   C      - NUMBER USED TO INDEX A BUILDING TIER (BLDG# - 1) * MXTRS + TIER#
C   C1     - BUILDING TIER NUMBER COMBINABLE WITH BUILDING TIER NUMBER C2
C   C2     - BUILDING TIER NUMBER COMBINABLE WITH BUILDING TIER NUMBER C1
C   CH     - BUILDING-TIER NUMBER OF THE TIER HEIGHT USED AS A COMMON HEIGHT
C   CNRLIN - SUBROUTINE TO CALCULATE THE DISTANCE BETWEEN A TIER CORNER AND
C             THE SIDE OF ANOTHER TIER.  SUBROUTINE ALSO CALCULATES WHETHER
C             OR NOT A PERPENDICULAR LINE DRAWN FROM THE CORNER TO THE SIDE
C             INTERCEPTS THE SIDE BETWEEN THE TWO CORNERS OF THE TIER.
C   CNVFLG - FLAG TO INDICATE A CONVERSION FACTOR OF 1.00 OR NOT
C   CONV   - FACTOR TO CONVERT USER'S UNITS TO METERS
C   CSA    - COSINE OF ANG
C   CXMN   - X COORDINATE OF FURTHEST WEST POINT OF DISTURBED AIR ZONE
C   CXMX   - X COORDINATE OF FURTHEST EAST POINT OF DISTURBED AIR ZONE
C   CYMN   - Y COORDINATE OF FURTHEST SOUTH POINT OF DISTURBED AIR ZONE
C   CYMX   - Y COORDINATE OF FURTHEST NORTH POINT OF DISTURBED AIR ZONE
C   D      - WIND DIRECTION SUBSCRIPT OR INDEX
C   D1     - DOWNWIND DISTANCE FROM A CORNER TO A STACK 
C   D2     - DOWNWIND DISTANCE FROM A ANOTHER CORNER TO A STACK 
C   DDEG   - INCREMENTAL WIND DIRECTION AND INITIAL WIND DIRECTION
C   DE     - CURRENT DIRECTION - LAGS ACTUAL DIRECTION - USED IN MXBWH
C   DFLG   - TO INDICATE A COMBINED STRUCTURE EXIST FOR A STACK - DIRECTION
C              COMBINATION
C   DHWE   - COMBINED BUILDING WAKE EFFECT HEIGHT FOR A WIND FLOW DIRECTION
C   DIF    - DIFFERENCE IN HEIGHT BETWEEN A STACK AND BUILDING BASE ELEVATION
C   DIRT   - DIRECTION FROM A STACK TO A CORNER
C   DIRTT  - TOTAL OF ANGLES (AA) FORMED BETWEEN A STACK AND CONSECUTIVE TIER
C              CORNERS.  IF TOTAL EQUALS 360 DEGREES, THE STACK IS ON THE ROOF
C              OF THAT TIER.
C   DISLIN - SUBROUTINE THAT CALCULATES DISTANCE BETWEEN A SIDE AND STACK
C             AND CHECKS IT AGAINST 5L
C   DIST   - DISTANCE BETWEEN TWO TIER CORNERS OR THE PERPENDICULAR DISTANCE
C             BETWEEN A TIER CORNER AND A TIER SIDE OF TWO DIFFERENT TIERS
C   DISTMN - MINIMUM DISTANCE BETWEEN BUILDING TIER PAIRS
C   DPADX  - X COORDINATE OF PROJECTED FACE MIDPOINT FOR MULIPLE TIERS
C   DPADY  - Y COORDINATE OF PROJECTED FACE MIDPOINT FOR MULIPLE TIERS
C   DPBH   - COMMON HEIGHT OF A COMBINED TIERS FOR A SPECIFIC WIND FLOW
CVT DPBL   - PROJECTED LENGTH OF COMBINED TIERS FOR A SPECIFIC WIND FLOW
C   DPBW   - PROJECTED WIDTH OF COMBINED TIERS FOR A SPECIFIC WIND FLOW
C   DTR    - DEGREES TO RADIANS CONVERSION FACTOR
C   DTR2   - DOUBLE PRECISION DTR
C   DX1    - MINIMUM OF TWO X-COORDINATES
C   DX2    - MAXIMUM OF TWO X-COORDINATES
C   FLG1   - FLAG FOR STACK INDICATING STACK'S X COORDINATE MAYBE WITHIN SIZ
C   FLG2   - FLAG FOR STACK INDICATING STACK'S Y COORDINATE MAYBE WITHIN SIZ
C   G65    - GEP 65 METER DECISION POINT FOR GEP DETERMINATION
C   GDIRS  - WIND FLOW DIRECTION MAXIMUM GEP STACK HEIGHT OCCURS
C   GEP    - GEP STACK HEIGHT FOR SOURCE s.
C   GEPBH  - GEP STRUCTURE HEIGHT AFFECTING SOURCE
C   GEPBW  - GEP PROJECTED STRUCTURE WIDTH AFFECTING SOURCE
C   GEPIN  - FLAG INDICATING SOURCE IS IN GEP INFLUENCE OF STRUCTURE C
C   GFS    - GAP-FILLING STRUCTURE. USED TO JOIN TWO SUFFICIENTLY TIERS INTO
C             ONE COMBINED STRUCTURE
C   GPC    - SUBROUTINE THAT SET GEPIN TO 1 AND DETERMINES MAX GEP STACK HEIGHT
C   GTLIST - ARRAY OF TIER NUMBERS USED TO CALCULATE A GEP STACK HEIGHT VALUE
C   GTNUM  - COUNTER WITH NUMBER OF TIERS USED TO CALCULATE A GEP STACK HEIGHT
C   HT     - BUILDING HEIGHT - DIMENSIONED BY BUILDING-TIER NUMBER
C   HTA    - BUILDING TIER HEIGHT OR COMMON TIER HEIGHT
C   HTC    - FOCAL TIER HEIGHT
C   HWE    - HEIGHT OF WAKE EFFECT PRODUCED BY A TIER ON A STACK
C   I      - BUILDING SUBSCRIPT OR INDEX
C   IBET   - FLAG IN DISLIN THAT WHEN SET INDICATES THAT A STACK IS UP TO
C             5L DIRECTLY DOWNWIND OF A SIDE OF A TIER OR GFS
C   ICF    - FLAG TO INDICATE FIRST CHARACTER FOUND IN STKN VARIABLE NAME
C   IDAY   - DAY
C   IFILE  - UNIT NUMBER USED IN DEBUGGING STACK - DIRECTION RELATIONSHIPS
C   IG     - FLAG TO INDICATE SINGLE OR COMBINED TIER CALCULATIONS
C   IGMT   - DIFFERENCE BETWEEN LOCAL AND GREENWICH(UNIVERSAL) MEAN TIME 
C   IHR    - HOUR
C   II     - COUNTER
C   IMIN   - MINUTE
C   IMON   - MONTH
C   ISEC   - SECOND
C   ISF    - FLAG TO INDICATE FIRST SPACE FOUND AFTER A CHARACTER IN STKN
C   ISS    - COUNTER FOR (RE)-SETTING STACK RELATED VALUES
C   IX     - TIME RELATED DUMMY VARIABLE
C   IYR    - YEAR
C   IZ     - FOR ISCLT2 ONLY, IF A STACK IS NOT UNDER A WAKE EFFECT COVERED
C             BY THE CENTRAL SIZ THEN CHECK THE OTHER TWO SIZS COVERING THE
C             SECTOR AND USE THE ONE WITH THE HIGHEST WAKE EFFECT HEIGHT.
C   J      - TIER SUBSCRIPT OR INDEX
C   JJ     - TIER SUBSCRIPT FOR A SECOND TIER
C   K      - TIER CORNER SUBSCRIPT
C          - FIRST TIER CORNER SUBSCRIPT OF A FIRST BUILDING
C   K1     - SECOND TIER CORNER SUBSCRIPT OF A FIRST BUILDING
C   K2     - SECOND TIER CORNER SUBSCRIPT OF A SECOND BUILDING
C   KK     - FIRST TIER CORNER SUBSCRIPT OF A SECOND BUILDING
C   L      - COUNTER
C   L2     - TWICE MAXIMUM BUILDING WIDTH OR HEIGHT WHICHEVER IS LESS
C   L5     - FIVE TIMES MAXIMUM BUILDING WIDTH OR HEIGHT WHICHEVER IS <
C   L5SQAT - NUMBER OF ROOF STACKS THAT ARE SUSPECTED OF BEING MORE
C            THAN 5L IN FROM AT LEAST ONE RESPECTIVE ROOF EDGE
C   LB     - BUILDING NUMBER INDEX
C   LD     - TIER CORNER INDEX
C   LD1    - FIRST OF TWO CONSECUTIVE TIER CORNERS
C   LD2    - SECOND OF TWO CONSECUTIVE TIER CORNERS
C   LFLAT  - FLAG THAT INDICATES WHICH STACK & BUILDING-TIER COMBINATIONS
C            FALL UNDER THE L5SQAT DEFINITION
C   LS     - STACK INDEX
C   LT     - TIER INDEX
C   LTN    - GREATER OF LTN1 OR LTN2
C   LTN1   - L OF FIRST COMBINABLE TIER
C   LTN2   - L OF SECOND COMBINABLE TIER OF TWO TIERS
C   M      - SLOPE OF A TIER SIDE
CDM MADX   - ARRAY HOLDING MAXIMUM MIDPOINT X COORDINATES BY STACK AND SECTOR
CDM MADY   - ARRAY HOLDING MAXIMUM MIDPOINT Y COORDINATES BY STACK AND SECTOR
C   MB     - MAXIMUM NUMBER OF BUILDINGS ALLOWED - PARAMETER
CVT MBL    - ARRAY HOLDING MAXIMUM LENGTHS BY STACK AND SECTOR
C   MBT    - MAXIMUM BUILDING TIER NUMBER (MB*MT) - PARAMETER
C   MD     - MAXIMUM NUMBER OF ISCST2 SECTORS - PARAMETER
C   MFLG   - FLAG INDICATES A NEW MAXIMUM WAKE EFFECT HEIGHT HAS BEEN FOUND
C   MH     - ARRAY HOLDING MAXIMUM HEIGHTS BY STACK AND SECTOR
C   MHWE   - MAXIMUM HEIGHT OF WAKE EFFECT BY SOURCE AND WIND DIRECTION
C   MI     - AN ARRAY OF BUILDING NUMBERS
C   MJ     - AN ARRAY OF TIER NUMBERS
C   ML     - MAXIMUM NUMBER OF ISCLT2 SECTORS - PARAMETER
C   MPADX  - X COORDINATE OF PROJECTED FACE MIDPOINT
C   MPADY  - Y COORDINATE OF PROJECTED FACE MIDPOINT
C   MSK    - MAXIMUM NUMBER OF STACK SOURCES - PARAMETER
C   MT     - MAXIMUM NUMBER OF TIERS/BUILDING - PARAMETER
C   MTLIST - ARRAY WITH A LIST OF COMBINED TIERS USED TO CALCULATE A MAXIMUM
C             PROJECTED WIDTH
C   MTNUM  - COUNTER HOLDING THE NUMBER OF COMBINED TIERS FOR A MAXIMUM
C             PROJECTED BUILDING WIDTH
C   MTS    - MAXIMUM NUMBER OF SIDES/TIER - PARAMETER
C   MW     - ARRAY HOLDING MAXIMUM WIDTHS BY STACK AND SECTOR
C   MXBHW  - SUBROUTINE THAT CALCULATES BUILDING WAKE EFFECT HEIGHT
C   MXPBH  - MAXIMUM PROJECTED BUILDING HEIGHT BY SOURCE AND WIND DRCTN
C   MXPBL  - MAXIMUM PROJECTED BUILDING LENGTH BY SOURCE AND WIND DRCTN
C   MXPBW  - MAXIMUM PROJECTED BUILDING WIDTH BY SOURCE AND WIND DRCTN
C   MXTRS  - MAXIMUM NUMBER OF TIERS PER BUILDING
C   NB     - NUMBER OF BUILDINGS ENTERED
C   ND     - NUMBER OF SIDES TO A TIER ENTERED
C   ND16   - NUMBER OF ISCLT2 SECTORS
C   NDIR   - NUMBER OF WIND DIRECTIONS TO BE PROCESSED
C   NDV    - NUMBER OF SIDES TO A TIER ENTERED
C   NS     - NUMBER OF STACKS ENTERED
C   NTRS   - NUMBER OF TIERS PER BUILDING
C   NTRSV  - NUMBER OF TIERS PER BUILDING
C   PBH    - BUILDING HEIGHT BY BUILDING TIER NUMBER
C   PBW    - PROJECTED BUILDING WIDTH BY BUILDING TIER NUMBER
C   PN     - FLAG INDICATING PLANT NORTH IS OTHER THAN TRUE NORTH
C   PNORTH - PLANT NORTH AS OPPOSED TO TRUE NORTH
C   PV     - PRELIMINARY GEP STACK HEIGHT VALUE
C   R      - MINIMUM VALUE OF BUILDING HEIGHT AND PROJECTED BUILDING WIDTH
C             USED IN DETERMINING THE COMBINABILITY OF TWO TIERS
C   S      - STACK SOURCE SUBSCRIPT OR INDEX
C   SB     - STACK BASE ELEVATION
C   SBV    - USED AS DUMMY VARIABLE DURING INITIAL READ
C   SH     - STACK HEIGHT
C   SHV    - USED AS DUMMY VARIABLE DURING INITIAL READ
C   SLOPE  - SLOPE USED IN DEFINING UPWIND FACES
C   SM     - SLOPE OF A LINE
C   SNA    - SINE OF ANG
C   SNM    - TEMPORARY STACK NAME USED TO CHECK FOR BLANK SPACES IN NAME - STKN
C   SS     - USED AS STACK INDEX
C   STKN   - STACK NAME
C   STKNV  - USED AS DUMMY VARIABLE DURING INITIAL READ
C   SWT    - FLAG INDICATING WHETHER TO CALCULATE VALUES FOR ISCLT OR FOR A
C              MODEL UTILIZING OR NOT UTILIZING THE PRIME ALGORITHM SUCH AS
C              ISCST OR AERMOD
C   SWTN   - FLAG INDICATING OUTPUT FOR A MODEL WITH THE PRIME ALGORITHM, 'P';
C              OR NOT, 'NP' (ISCST - 'ST'); OR ISCLT 'LT' 
C   T1     - FOCAL TIER INDEX
C   T2     - CANDIDATE TIER INDEX
C   TH     - HEIGHT OF A TIER - DIMENSIONED BY BUILDING NUMBER AND TIER NO.
C   THV    - USED AS DUMMY VARIABLE DURING INITIAL READ
C   TITLE  - DATA TITLE OR IDENTIFICATION STATEMENT (UP TO 78 CHARACTERS)
C   TL1    - FOCAL TIER NUMBER
C   TL2    - CANDIDATE TIER NUMBER
C   TLIST  - IDENTIFIES WHICH BUILDING TIER NUMBER TO USE IN COMBINING
C   TLIST2 - SUBGROUP OF TLIST - BASED ON A TIER HEIGHT OF A TIER IN TLIST
C            TIER HEIGHT IS USED AS BASIS FOR COMBINING
C   TN1    - SEE TNUM
C   TNM    - LABELLED COMMON CONTAINING A SET OF COMBINED TIERS
C   TNUM   - COUNTER FOR NUMBER OF BUILDING TIERS TO EXAMINE FOR COMBINING.
C   TNUM2  - COUNTER FOR TLIST2
C   TW     - PROJECTED WIDTH OF A TIER OR A GROUP OF COMBINED TIERS
C   UEAST  - LOCAL Y - COORDINATE OF ORIGIN IN UTM COORDINATE VALUE
C   UNORTH - LOCAL X - COORDINATE OF ORIGIN IN UTM COORDINATE VALUE
C   UNTS   - INPUT UNITS NAME
C   UTM    - FLAG TO INDICATE THE INPUT DATA ARE IN UTM COORDINATES
C   UTMP   - INPUT FLAG INDICATING USE OF UTM INPUT
C   W      - TIER WIDTH FOR A WIND DIRECTION
C   WIDTH  - SUBROUTINE THAT CALCULATES PROJECTED WIDTHS AND COORDINATES OF
C             THE MOST SOUTH, EAST, WEST, AND NORTH CORNERS
C   WS     - LESSER OF BUILDING HEIGHT OR PROJECTED BUILDING WIDTH (L)
C   X      - X COORDINATE OF AN UNTRANSLATED TIER CORNER
C   X1     - FIRST OF TWO CONSECUTIVE TIER CORNER X-COORDINATES
C   X11    - X COORDINATE OF FIRST TIER CORNER OF FIRST BUILDING
C   X12    - X COORDINATE OF SECOND TIER CORNER OF FIRST BUILDING
C   X2     - SECOND OF TWO CONSECUTIVE TIER CORNER X-COORDINATES
C   X21    - X COORDINATE OF FIRST TIER CORNER OF SECOND BUILDING
C   X22    - X COORDINATE OF SECOND TIER CORNER OF SECOND BUILDING
C   XBADJ  - ISC X CORRECTION FOR UPWIND CENTER POINT OF PROJ. BUILDING
C   XC     - X COORDINATE OF A TRANSLATED TIER CORNER
C   XCOMP  - DOUBLE PRECISION DIFFERENCE BETWEEN A STACK AND TIER CORNER
C   XI     - X INTERCEPT COORDINATE BETWEEN A CORNER AND A SIDE
C   XKP    - X COORDINATE OF A TIER CORNER
C   XMAX   - X COORDINATE OF FURTHEST EAST OF A TRANSLATED TIER CORNER
C   XMIN   - X COORDINATE OF FURTHEST WEST OF A TRANSLATED TIER CORNER
C   XMN    - TEMPORARY STORAGE OF FURTHEST WEST X COORDINATE VALUE
C   XMX    - TEMPORARY STORAGE OF FURTHEST EAST X COORDINATE VALUE
C   XPSTK  - X COORDINATE OF A TRANSLATED STACK
C   XS     - X COORDINATE OF AN UNTRANSLATED STACK
C   XSP    - STACK X-COORDINATE ARGUMENT IN SUBROUTINE DISLIN
C   Y      - Y COORDINATE OF AN UNTRANSLATED TIER CORNER
C   Y1     - FIRST OF TWO CONSECUTIVE TIER CORNER Y-COORDINATES
C   Y11    - Y COORDINATE OF FIRST TIER CORNER OF FIRST BUILDING
C   Y12    - Y COORDINATE OF SECOND TIER CORNER OF FIRST BUILDING
C   Y2     - SECOND OF TWO CONSECUTIVE TIER CORNER Y-COORDINATES
C   Y21    - Y COORDINATE OF FIRST TIER CORNER OF SECOND BUILDING
C   Y22    - Y COORDINATE OF SECOND TIER CORNER OF SECOND BUILDING
C   YBADJ  - ISC Y CORRECTION FOR UPWIND CENTER POINT OF PROJ. BUILDING
C   YC     - Y COORDINATE OF A TRANSLATED TIER CORNER
C   YCOMP  - DOUBLE PRECISION DIFFERENCE BETWEEN A STACK AND TIER CORNER
C   YI     - Y INTERCEPT COORDINATE BETWEEN A CORNER AND A SIDE.
C   YKP    - Y COORDINATE OF A TIER CORNER
C   YMAX   - Y COORDINATE OF FURTHEST NORTH OF A TRANSLATED TIER CORNER
C   YMIN   - Y COORDINATE OF FURTHEST SOUTH OF A TRANSLATED TIER CORNER
C   YMN    - TEMPORARY STORAGE OF FURTHEST WEST Y COORDINATE VALUE
C   YMX    - TEMPORARY STORAGE OF FURTHEST EAST Y COORDINATE VALUE
C   YPSTK  - Y COORDINATE OF A TRANSLATED STACK
C   YS     - Y COORDINATE OF AN UNTRANSLATED STACK
C   YSP    - STACK Y-COORDINATE ARGUMENT IN SUBROUTINE DISLIN
C
C   **************************************************************************
C
      IMPLICIT NONE

      LOGICAL     FLG1, FLG2
      INTEGER     C, C1, C2, CH, CNVFLG, 
     *            D, DE, DFLG, GEPIN, GTLIST, GTNUM 
      INTEGER     IDAY, IMON, IYR, 
     *            IGMT, IHR, IMIN, ISEC, IX, DATE_TIME(8) 
      INTEGER     I, IBET, ICF, IFILE, IG, 
     *            II, ISF, ISS, IZ,
     *            J, JJ, K, K1, K2, KK, L, L5SQAT, LB, LD, LD1, LD2, LT,
     *            LFLAT, LS
      INTEGER     M, MB, MBT, MD, MI, MJ, ML, MSK, 
     *            MT, MTNUM, MTLIST, MTS, MXTRS  
      INTEGER     NB, ND, ND16, NDIR, NDV,
     *            NS, NTRS, NTRSV, PN, 
     *            S, SWT,
     *            T1, T2, TL1, TL2, TLIST, TLIST2, TN1, TNUM, TNUM2, UTM

      REAL        BL, DPADX, DPADY, DPBL, MXPBL, 
     &            MADX, MADY, MBL, MPADX, MPADY,
     &            XBADJ, YBADJ

      REAL        ADJ, ANG, AP, AU, BELEV, BELEVV, BET, BP, BU, 
     &            CONV, CSA, CXMN, CXMX, CYMN, CYMX, DDEG, DIF, 
     &            DHWE, DIST, DISTMN,
     &            DPBH, DPBW, DTR,
     &            G65, GDIRS, GEP, GEPBH, GEPBW 
      REAL        HT, HTA, HTC, 
     &            L2, L5,  LTN1, LTN2, LTN,
     &            MH, MHWE,
     &            MW, MXPBH, MXPBW, PNORTH, PV, R,
     &            SB, SBV, SH, SHV, SNA, TH, THV, TW, W, WS 
      REAL        X1, X11, X12, X2, X21, X22,
     &            XC, XI, XMAX, XMIN, XMN, XMX, XPSTK, XS, 
     &            Y1, Y11, Y12, Y2, Y21, Y22,
     &            YC, YI, YMAX, YMIN, YMN, YMX, YPSTK, YS

      DOUBLE PRECISION A, B, AA, BB, DIRT, DIRTT, DTR2,
     +                 UEAST, UNORTH, X, XCOMP, Y, YCOMP

      CHARACTER(LEN=2)  SWTN
      CHARACTER(LEN=4)  UTMP
      CHARACTER(LEN=8)  BTN, BTNV, STKN, STKNV, SNM
      CHARACTER(LEN=10) UNTS
      CHARACTER(LEN=12) REAL_CLOCK(3)
      CHARACTER(LEN=78) TITLE

C
C ALLOCATABLE SUBSCRIPT FORMAT: (BUILDING OR STACK #, WD OR TIER #, SIDE #)
C
      ALLOCATABLE :: X(:,:,:), Y(:,:,:), ND(:,:)
      ALLOCATABLE :: XC(:,:), YC(:,:)
      ALLOCATABLE :: DFLG(:,:), DHWE(:), DPBH(:), DPBW(:)
      ALLOCATABLE :: BELEV(:)
      ALLOCATABLE :: GEP(:), GEPBH(:), GEPBW(:),
     +               GEPIN(:,:,:,:)
      ALLOCATABLE :: MHWE(:,:), MXPBH(:,:), MXPBW(:,:)
      ALLOCATABLE :: XMAX(:), XMIN(:), YMAX(:), YMIN(:)
      ALLOCATABLE :: MI(:,:), MJ(:,:)
      ALLOCATABLE :: TNUM2(:), TLIST2(:,:)
      ALLOCATABLE :: GTNUM(:), GTLIST(:,:), GDIRS(:)
      ALLOCATABLE :: MTNUM(:,:), MTLIST(:,:,:)
      ALLOCATABLE :: BTN(:), NTRS(:), W(:), HT(:), TH(:,:)
      ALLOCATABLE :: STKN(:), SB(:), SH(:), XS(:), YS(:)
      ALLOCATABLE :: DIRT(:), DISTMN(:,:), LFLAT(:,:)
      ALLOCATABLE :: TNUM(:), TLIST(:,:)
      ALLOCATABLE :: MH(:,:), MW(:,:)

      ALLOCATABLE :: DPADX(:), DPADY(:), DPBL(:)
      ALLOCATABLE :: MPADX(:,:), MPADY(:,:), MXPBL(:,:)
      ALLOCATABLE :: MBL(:,:), MADX(:,:), MADY(:,:)
C
C  INITIAL PARAMETER SETTINGS
C
C
       IG = 1
       MD = 36
       ML = 16 
       MT = 0
       MTS = 0
       DE = 0
       DTR = 3.141593 / 180.
       DTR2 = 3.141593 / 180.
       G65 = 65.

CVRT HARDWIRE OPEN FILES
C      OPEN(10,FILE='BPIP.INP',STATUS='OLD')
C      OPEN(12,FILE='BPIP.OUT',STATUS='UNKNOWN')
C      OPEN(14,FILE='BPIP.SUM',STATUS='UNKNOWN')
CVRT

C  READ THE INPUT FILE TO FIND THE MAXIMUM VALUES

        READ(10,*) TITLE
          WRITE (12,1) TITLE
          WRITE (14,1) TITLE
        READ(10,*) SWTN
        READ(10,*) UNTS, CONV
        READ(10,*) UTMP, PNORTH
        READ(10,*) NB
        MB = NB
        DO 10 I = 1, NB
           READ(10,*) BTNV, NTRSV, BELEVV
             IF (NTRSV .GE. MT) THEN 
               MT = NTRSV
             END IF
         DO 20 J = 1, NTRSV
            READ(10,*) NDV, THV
              IF (NDV .GE. MTS) THEN 
                MTS = NDV
              END IF
          DO 30 K = 1, NDV
             READ(10,*) AA, BB
30        CONTINUE
20       CONTINUE
10      CONTINUE
        MBT = MB * MT
        MXTRS = MT

        READ(10,*) NS
        MSK = NS
        DO 40 S = 1, NS
           READ (10,*) STKNV, SBV, SHV, AA, BB
40      CONTINUE
        REWIND (10)

C  ALLOCATE AND INITIALIZE THE ARRAYS BASED ON 
C    THE MAXIMUM PARAMETER VALUES FOUND

      ALLOCATE (X(MB, MT, MTS), Y(MB, MT, MTS), ND(MB, MT))
        DO I = 1, MB; DO J = 1, MT; ND(I,J) = 0; 
        DO K = 1, MTS; X(I,J,K) = 0.0; Y(I,J,K) = 0.0; 
        END DO; END DO; END DO

      ALLOCATE (XC(MBT,MTS), YC(MBT,MTS))
        DO I = 1, MBT; DO J = 1, MTS; XC(I,J) = 0.0; YC(I,J) = 0.0
        END DO; END DO

      ALLOCATE (DFLG(MSK, MD), DHWE(MSK), DPBH(MSK),DPBL(MSK),DPBW(MSK))
        DO I = 1, MSK; DHWE(I) = 0.0; DPBH(I) = 0.0; DPBL(I) = 0.0;
                    DPBW(I) = 0.0;
        DO J = 1, MD; DFLG (I,J) = 0; 
        END DO; END DO

      ALLOCATE (BELEV(MB))
        DO I = 1, MB; BELEV(I) = 0.0;END DO 

      ALLOCATE (GEP(MSK), GEPBH(MSK), GEPBW(MSK))                 
        DO I = 1, MSK; GEP(I) = 0.0; GEPBH(I) = 0.0; GEPBW(I) = 0.0
        END DO

      ALLOCATE (GEPIN(MSK,MBT,MBT,2))
        DO I = 1, MSK; DO J = 1, MBT; DO K = 1, MBT; DO L = 1, 2
          GEPIN(I,J,K,L) = 0; 
        END DO; END DO; END DO; END DO 
      
      ALLOCATE (MHWE(MSK, MD), MXPBH(MSK, MD), MXPBL(MSK,MD),
     *   MXPBW(MSK, MD))
        DO I = 1, MSK; DO J = 1, MD; 
         MHWE(I,J) = 0.0; MXPBH(I,J) = 0.0;
            MXPBL(I,J) = 0.0; MXPBW(I,J) = 0.0;
        END DO; END DO    

      ALLOCATE (XMAX(MBT), XMIN(MBT), YMAX(MBT), YMIN(MBT))       
        DO I = 1 , MBT; 
          XMAX(I) = 0.0; XMIN(I) = 0.0; YMAX(I) = 0.0
          YMIN(I) = 0.0
        END DO

      ALLOCATE (MI(MSK,2), MJ(MSK,2))  
        DO I = 1, MSK; DO J = 1,2; MI(I,J) = 0; MJ(I,J) = 0;
        END DO; END DO

      ALLOCATE (TNUM2(MBT), TLIST2(MBT,MBT))                      
        DO I = 1, MBT; TNUM2(I) = 0; 
        DO J = 1,MBT; TLIST2(I,J) = 0; END DO; END DO

      ALLOCATE (GTNUM(MSK), GTLIST(MSK,MBT), GDIRS(MSK))         
        DO I = 1, MSK; GTNUM(I) = 0; GDIRS(I) = 0.0; 
        DO J = 1, MBT; GTLIST(I,J) = 0; END DO; END DO

      ALLOCATE (MTNUM(MSK,2), MTLIST(MSK,MBT,2))                 
        DO I = 1, MSK; DO J = 1, 2; MTNUM(I,J) = 0; 
        DO K = 1, MBT; MTLIST(I,K,J) = 0; END DO; END DO; END DO

      ALLOCATE (BTN(MB), NTRS(MB), W(MBT), HT(MBT), TH(MB, MT))
        DO I = 1, MB; BTN(I) = "        "; NTRS(I) = 0; END DO
        DO I = 1, MBT; W(I) = 0.0; HT(I) = 0.0; END DO
        DO I = 1, MB; DO J = 1, MT; TH(I,J) = 0.0; END DO; END DO
      
      ALLOCATE (STKN(MSK), SB(MSK), SH(MSK), XS(MSK), YS(MSK))
        DO I = 1 , MSK; STKN(I) = "        "; 
          SB(I) = 0.0; SH(I) = 0.0
          XS(I) = 0.0; YS(I) = 0.0; 
        END DO

      ALLOCATE (DIRT(MTS), DISTMN(MBT, MBT), LFLAT(MSK, MBT))
        DO I = 1, MTS; DIRT(I) = 0.0; END DO
        DO I = 1, MBT; DO J = 1, MBT; DISTMN(I,J) = 0.0; END DO;END DO
        DO I = 1, MSK; DO J = 1, MBT; LFLAT(I,J) = 0; END DO; END DO
      
      ALLOCATE (TNUM(MBT), TLIST(MBT,MBT))
        DO I = 1, MBT; TNUM(I) = 0; 
        DO J = 1, MBT; TLIST(I,J) = 0; END DO; END DO

      ALLOCATE (MH(MSK, ML), MW(MSK, ML))
      ALLOCATE (MBL(MSK,ML), MADX(MSK,ML), MADY(MSK,ML))
        DO I = 1, MSK; DO J = 1, ML; MH(I,J) = 0.0; MW(I,J) = 0.0;
                 MBL(I,J) = 0.0; MADX(I,J) = 0.0; MADY(I,J) = 0.0;
        END DO; END DO

      ALLOCATE (DPADX(MSK), DPADY(MSK))
        DO I = 1, MSK; DPADX(I) = 0.0; DPADY(I) = 0.0;
        END DO

      ALLOCATE (MPADX(MSK,MD), MPADY(MSK,MD))
        DO I = 1, MSK; DO J = 1, MD; MPADX(I,J) = 0.0; MPADY(I,J) = 0.0
        END DO; END DO

C
C  READ INPUT DATA CONTAINING BUILDING AND STACK DATA
C

        WRITE(*,*) ' '
        WRITE(*,*) 'READING INPUT DATA FROM FILE.'
        WRITE(*,*) ' '

        READ(10,*) TITLE


            CALL DATE_AND_TIME (REAL_CLOCK (1), REAL_CLOCK (2),
     *                          REAL_CLOCK (3), DATE_TIME) 
              IYR = DATE_TIME(1)
              IMON = DATE_TIME(2)
              IDAY = DATE_TIME(3) 
C              IGMT = DATE_TIME(4) !C TIME DIFFERENCE 
C                                     WRT COORDINATED UNIVERSAL TIME (UTC)
              IHR = DATE_TIME(5)
              IMIN = DATE_TIME(6) 
              ISEC = DATE_TIME(7)
C              IX = DATE_TIME(8)   !C MILLISECONDS

            WRITE (12, 461)
            WRITE (12, 462) IMON, IDAY, IYR
            WRITE (12, 463) IHR, IMIN, ISEC
            WRITE (12,1) TITLE
            WRITE (14, 461)
            WRITE (14, 462) IMON, IDAY, IYR
            WRITE (14, 463) IHR, IMIN, ISEC
            WRITE (14,1) TITLE

        WRITE(12,*) '============================'
        WRITE(12,*) 'BPIP PROCESSING INFORMATION:'
        WRITE(12,*) '============================'
        WRITE(14,*) '============================'
        WRITE(14,*) 'BPIP PROCESSING INFORMATION:'
        WRITE(14,*) '============================'

        READ(10,*) SWTN

          SWT = 3
           IF(SWTN(1:1) .EQ. 'p') SWT = 0
           IF(SWTN(1:1) .EQ. 'P') SWT = 0
           IF(SWTN(1:1) .EQ. 'n') SWT = 2
           IF(SWTN(1:1) .EQ. 'N') SWT = 2
C          LEGACY SWITCHES FOR ISCST AND ISCLT RESPECTIVELY
           IF(SWTN(1:1) .EQ. 's') SWT = 2
           IF(SWTN(1:1) .EQ. 'S') SWT = 2
           IF(SWTN(1:1) .EQ. 'l') SWT = 1
           IF(SWTN(1:1) .EQ. 'L') SWT = 1
          IF(SWT .EQ. 0) THEN
            WRITE(12,2) SWTN
            WRITE(14,2) SWTN
          END IF
          IF(SWT .EQ. 1) THEN
            WRITE(12,9) SWTN
            WRITE(14,9) SWTN
          END IF
          IF(SWT .EQ. 2) THEN
            WRITE(12,16) SWTN
            WRITE(14,16) SWTN
          END IF
          IF(SWT .EQ. 3) THEN
            WRITE(*,*) '**ERR: The SWTN variable, ',SWTN,' is',
     +       ' incorrectly entered.'
            WRITE(*,*) '         Please use P, NP, or L as input.'
            STOP
          END IF

        READ(10,*) UNTS, CONV
          WRITE(12,3) UNTS, CONV
          WRITE(14,3) UNTS, CONV
C             IF CNVFLG IS 1, THE UNITS ARE CONSIDERED TO BE IN METERS
          CNVFLG = 0
           IF (ABS(CONV - 1.00) .LT. .05) CNVFLG = 1

        READ(10,*) UTMP, PNORTH
          UTM = 2
           IF(UTMP(4:4) .EQ. 'n') UTM = 0
           IF(UTMP(4:4) .EQ. 'N') UTM = 0
           IF(UTMP(4:4) .EQ. 'y') UTM = 1
           IF(UTMP(4:4) .EQ. 'Y') UTM = 1
          IF(UTM .EQ. 1) THEN
            WRITE(12,4) UTMP
            WRITE(14,4) UTMP
            WRITE(14,11)
           ELSE
            WRITE(12,5) UTMP
            WRITE(14,5) UTMP
          END IF
          IF(UTM .EQ. 2) THEN
            WRITE(*,*) 'The UTMP variable, ',UTMP,' is incorrectly ',
     +                 'entered.'
            STOP
          END IF

          IF(PNORTH .EQ. 360.0 .OR. PNORTH .EQ. 0.0) THEN
            PN = 0
            WRITE(12,17) PNORTH
            WRITE(14,17) PNORTH
           ELSE
            PN = 1
            WRITE(12,17) PNORTH
            WRITE(12,297)
            WRITE(14,17) PNORTH
            WRITE(14,18)
            WRITE(14,297)
          END IF

          ADJ = 360. - PNORTH
           IF (PN .EQ. 1) THEN
             ADJ = ADJ * DTR
             CSA = COS(ADJ)
             SNA = -SIN(ADJ)
           END IF

        WRITE(14,297)
        WRITE(14,*) '=============='
        WRITE(14,*) 'INPUT SUMMARY:'
        WRITE(14,*) '=============='
        WRITE(14,297)

        READ(10,*) NB
           IF (NB .GT. MB) THEN
             WRITE(*,*)  'WARNING :'
             WRITE(*, *) 'THE NUMBER OF BUILDINGS ENTERED EXCEEDS THE ',
     +                   'PARAMETER, MB'
             STOP
           END IF
          WRITE(14,6) NB

        DO 15 I = 1, NB
           READ(10,*) BTN(I), NTRS(I), BELEV(I)
           IF (NTRS(I) .GT. MT) THEN
             WRITE(*,*)  'WARNING :'
             WRITE(*, *) 'THE NUMBER OF TIERS/BUILDING EXCEEDS THE ',
     *                   'PARAMETER, MT'
             STOP
           END IF

            WRITE(14,12) BTN(I), NTRS(I), BELEV(I), UNTS
            IF (CNVFLG .EQ. 0) THEN
              BELEV(I) = BELEV(I) * CONV
              WRITE(14,13) BELEV(I)
            END IF
              WRITE(14,14)

         DO 25 J = 1, NTRS(I)
             READ(10,*) ND(I, J), TH(I, J)
             IF (ND(I,J) .GT. MTS) THEN
                WRITE(*,*)  'WARNING :'
              WRITE(*, *) 'THE NUMBER OF TIERS/BUILDING EXCEEDS THE ',
     *                    'PARAMETER, MTS'
             STOP
           END IF
             C = (I - 1) * MXTRS + J
             WRITE(14,21) BTN(I), J, C, TH(I,J), ND(I,J)
              IF (CNVFLG .EQ. 0) THEN
                TH(I,J) = TH(I,J) * CONV
                WRITE(14,22)  TH(I,J)
              END IF

          DO 35 K = 1, ND(I, J)
            READ(10,*) AA, BB
              IF (CNVFLG .EQ. 1 .OR.  UTM .EQ. 1) THEN
                WRITE (14,32) AA, BB
              END IF
              IF (CNVFLG .EQ. 0 .AND. UTM .EQ. 0) THEN
                WRITE (14,31) AA, BB, UNTS
                 AA = AA * CONV
                 BB = BB * CONV
                  WRITE (14,32) AA, BB
              END IF

              IF (UTM .EQ. 1) THEN
                IF(I .EQ. 1 .AND. J .EQ. 1 .AND. K .EQ. 1) THEN
                  UEAST  = AA
                  UNORTH = BB
                  A = UEAST
                  B = UNORTH
                  AU = 0.0
                  BU = 0.0
                 ELSE
                  A = AA
                  B = BB
                  AU = A - UEAST
                  BU = B - UNORTH
                END IF
                  WRITE (14,33) AU, BU
                  X(I, J, K) = AU
                  Y(I, J, K) = BU
C                WRITE (14,14) X(I, J, K), Y(I, J, K)
               ELSE
                X(I, J, K) = AA
                Y(I, J, K) = BB
              END IF


              IF (PN .EQ. 1) THEN
                AP = X(I, J, K) * CSA + Y(I, J, K) * SNA
                BP = Y(I, J, K) * CSA - X(I, J, K) * SNA
                X(I, J, K) = AP
                Y(I, J, K) = BP
                WRITE (14,19) X(I, J, K), Y(I, J, K)
              END IF

35        CONTINUE
25       CONTINUE
15    CONTINUE

       READ(10,*) NS
         IF (NS .GT. MSK) THEN
           WRITE(*,*)  'WARNING :'
           WRITE(*, *) 'THE NUMBER OF STACKS ENTERED EXCEEDS THE ',
     +                 'PARAMETER, MKS'
             STOP
           END IF

          WRITE (14,41) NS
          WRITE (14,42)

      DO 55 S = 1, NS
       READ (10,*) STKN(S), SB(S), SH(S), AA, BB
          WRITE (14, 43) STKN(S), SB(S), SH(S), UNTS
            SB(S) = SB(S) * CONV
            SH(S) = SH(S) * CONV
          IF (CNVFLG .EQ. 0) WRITE(14,44) SB(S), SH(S)

          IF (CNVFLG .EQ. 1 .OR.  UTM .EQ. 1) WRITE(14,51) AA, BB
          IF (CNVFLG .EQ. 0 .AND. UTM .EQ. 0) THEN
            WRITE (14,49) AA, BB, UNTS
             AA = AA * CONV
             BB = BB * CONV
            WRITE (14, 45) AA, BB
          END IF

          AU = AA - UEAST
          BU = BB - UNORTH

        IF (UTM .EQ. 1) THEN
          WRITE (14, 45) AU, BU
        END IF

        XS(S) = AU
        YS(S) = BU

        IF (PN .EQ. 1) THEN
          A = XS(S) * CSA + YS(S) * SNA
          B = YS(S) * CSA - XS(S) * SNA
          XS(S) = A
          YS(S) = B
          WRITE (14,46) XS(S), YS(S)
        END IF

C              CHECK FOR BLANK SPACES IN STACK NAMES

          SNM = STKN(S)
          ICF = 0
          ISF = 0
         DO 48 I = 1, 8
           IF (ICF .EQ. 0) THEN
             IF (SNM(I:I) .NE. ' ') THEN
               ICF = 1
               GO TO 48
             END IF
           ELSE
             IF (ISF .EQ. 0) THEN
               IF (SNM(I:I) .EQ. ' ') THEN
                 ISF = 1
                 GO TO 48
               END IF
             ELSE
               IF (SNM(I:I) .NE. ' ') THEN
                  WRITE (14,47)
                  GO TO 55
               END IF
             END IF
           END IF
48        CONTINUE
55    CONTINUE

C     Detect if a stack is on top of a roof
C       where the stack could be > 5L from an upwind roof edge.

        L5SQAT = 0
        DO LS = 1, NS
          DO LB = 1, NB
            DO LT = 1, NTRS(LB)
              C = (LB-1) * MXTRS + LT
              DIRTT = 0.0
              DO LD = 1, ND(LB, LT)
                IF (YS(LS) .EQ. Y(LB,LT,LD)) THEN
                   IF (XS(LS) .GT. X(LB,LT,LD)) THEN
                      DIRT(LD) = 270.
                   END IF
                   IF (XS(LS) .LT. X(LB,LT,LD)) THEN
                      DIRT(LD) = 90.
                   END IF
                   IF (XS(LS) .EQ. X(LB,LT,LD)) THEN
                      GEPIN(LS,C,C,1) = 1
                      LFLAT(LS,C) = 1
                      DIRTT = 360.
                      GOTO 77
                   END IF
                 ELSE
                  XCOMP =  X(LB,LT,LD) - XS(LS)
                  YCOMP =  Y(LB,LT,LD) - YS(LS)
                  DIRT(LD) = DATAN(XCOMP/YCOMP) / DTR2
                   IF (YCOMP .GT. 0.0) DIRT(LD) = 360. + DIRT(LD)
                   IF (YCOMP .LE. 0.0) DIRT(LD) = 180. + DIRT(LD)
                   IF (DIRT(LD) .GT. 360.) DIRT(LD) = DIRT(LD) - 360.
                END IF
              END DO
            DO LD = 1, ND(LB,LT)
              LD1 = LD
              LD2 = LD + 1
                IF (LD2 .GT. ND(LB,LT)) LD2 = 1
              AA = DIRT(LD2) - DIRT(LD1)
              IF (AA .LE. -180.) AA = 360. + AA
              IF (AA .GT.  180.) AA = AA - 360.
                DIRTT = DIRTT + AA
            END DO
   77       CONTINUE
              IF (ABS(360. - ABS(DIRTT)) .LT. 0.02) THEN
                 L5SQAT = L5SQAT + 1
                 IF (L5SQAT .EQ. 1) THEN
                   WRITE (14,71)
                   WRITE (14,72)
                 END IF
                 GEPIN (LS,C,C,1) = 1
                 LFLAT (LS,C) = 1
                 WRITE(14,73) STKN(LS), LS, BTN(LB), LB, LT
              END IF
            END DO
          END DO
        END DO
          IF (L5SQAT .EQ. 0) THEN
            WRITE(14,*) ' '
            WRITE(14,*) '   No stacks have been detected as being atop',
     +                    ' any structures.'
          END IF

        WRITE(*,*) ' '
        WRITE(*,*) 'END OF READING INPUT DATA FROM FILE.'
        WRITE(*,*) ' '
C
C                                     END OF INPUT SECTION
C
        WRITE(*,*) ' '
        WRITE(*,*) 'CALCULATING GEP VALUES.'
        WRITE(*,*) ' '

       IF (SWT .EQ. 0 .OR. SWT .EQ. 2) THEN
        DDEG = 10
        NDIR = MD
       ELSE
        DDEG = 11.25
        NDIR = ML * 2
       END IF
        ND16 = ML
C
C
C                        Calculate the minimum distance between structures
C
        DO 80 I = 1, (NB - 1)
         DO 81 II = (I + 1), NB
          DO 82 J = 1, NTRS(I)
           DO 83 JJ = 1, NTRS(II)
             C1 = (I - 1) * MXTRS + J
             C2 = (II - 1) * MXTRS + JJ
            DISTMN(C1, C2) = 1000000
C                 Cycle through corners and sides of first tier
             DO 84 K = 1, ND(I, J)
               X11 = X(I, J, K)
               Y11 = Y(I, J, K)
                K1 = K + 1
                 IF (K1 .GT. ND(I, J)) K1 = 1
               X12 = X(I, J, K1)
               Y12 = Y(I, J, K1)
C                  Cycle through corners and sides of other building's tier
              DO 85 KK = 1, ND(II, JJ)
                X21 = X(II, JJ, KK)
                Y21 = Y(II, JJ, KK)
                 K2 = KK + 1
                  IF (K2 .GT. ND(II, JJ)) K2 = 1
                X22 = X(II, JJ, K2)
                Y22 = Y(II, JJ, K2)
C              Calculate corner to corner distance between two structures
              DIST = SQRT((X11 - X21) ** 2 + (Y11 - Y21) ** 2)
               IF (DIST .LT. DISTMN(C1, C2)) DISTMN(C1, C2) = DIST
C                   Calculate distance between a corner of one structure
C                    and the side of another structure
               CALL CNRLIN(XI, YI, X21, Y21,X22,Y22,BET,DIST,X11,Y11)
C                  If the intercept is between the two corners of the
C                   other structure, test to see if it is the
C                   shortest distance.
                 IF (BET .GT. -.001) THEN
                   IF (DIST .LT. DISTMN(C1, C2)) DISTMN(C1, C2) = DIST
                 END IF
               CALL CNRLIN(XI,YI,X11,Y11,X12,Y12,BET, DIST, X21, Y21)
C                   If the intercept is between the two corners of the
C                    other structure, test to see if it is the
C                    shortest distance.
                 IF (BET .GT. -.001) THEN
                   IF (DIST .LT. DISTMN(C1, C2)) DISTMN(C1, C2) = DIST
                 END IF
85            CONTINUE
84           CONTINUE
              DISTMN(C2, C1) = DISTMN(C1, C2)
83          CONTINUE
82         CONTINUE
81        CONTINUE
80      CONTINUE
C
C
C             GEP STACK HEIGHT CALCULATIONS
C
C               DETERMINE IF A STACK IS WITHIN A GEP 5L AREA OF INFLUENCE
C
C                 AS STAND ALONE STRUCTURES AND TIERS
C
C                   FOR EVERY QUARTER OF A DEGREE
C
        WRITE(*,*) '  Calculating single tier GEP values.'
      DO 100 D = 1, 1440
        ANG = D * DTR/4
        CSA = COS(ANG)
        SNA = -SIN(ANG)
        DO 101 I = 1, NB
         DO 102 J = 1, NTRS(I)
           C = (I-1) * MXTRS + J
           CH = C
           TNUM2(C) = 0
C                CALCULATE PROJECTED TIER WIDTH, TW
          CALL WIDTH (MB, MXTRS, MTS, MBT, X, Y, ND, XC, YC,
     *                  XMAX, XMIN, YMAX, YMIN, ANG, I, J, C, TW, BL)
             W(C) = TW
              WS = TW
             HTA = TH(I,J)
              IF (HTA .LT. TW) WS = HTA
             L5 = WS * 5
C           X AND Y COORDINATES DEFINING AN INITIAL AREA around GEP 5L segment
             CXMN = XMIN(C)
             CXMX = XMAX(C)
             CYMX = YMAX(C) + L5
             CYMN = YMIN(C)
C
C                   CHECK FOR STACK S LOCATION IN GEP 5L AREA.
C
         DO 103 S = 1, NS
          XPSTK = XS(S) * CSA + YS(S) * SNA
          YPSTK = YS(S) * CSA - XS(S) * SNA
          FLG1 = ((XPSTK .GE. CXMN) .AND. (XPSTK .LE. CXMX))
          FLG2 = ((YPSTK .GE. CYMN))
          IF (FLG1 .AND. FLG2) THEN
C
C                     Calculate distance between a stack and a side or corner
C                     of a tier.  If less than 5L, set flag and calculate
C                     GEP stack height.
C
           DO 106 K = 1, ND(I, J)
             X1 = XC(C, K)
             Y1 = YC(C, K)
             L = K + 1
              IF (L .GT. ND(I, J)) L = 1
             X2 = XC(C, L)
             Y2 = YC(C, L)
            CALL DISLIN(X1, Y1, X2, Y2, L5, IBET, XPSTK, YPSTK)
             IF (LFLAT (S,C) .EQ. 1) IBET = 1
             IF (IBET .EQ. 1) THEN
               TNUM2(C) = 1
               TLIST2(C,1) = C
C                     Call subroutine to calculate a GEP stk ht. and others
               CALL GPC (MB, MBT, MXTRS, MSK, BELEV, 
     *                   SB, GEP,GEPBH,GEPBW,GEPIN, TNUM2, TLIST2,
     *                   GTNUM, GTLIST, GDIRS, MI, MJ, 
     *                   D, I, C, S, TW, WS, HTA, C, 1)
             END IF
106        CONTINUE
          END IF
103      CONTINUE
102     CONTINUE
101    CONTINUE
100   CONTINUE
C
C             GEP STACK HEIGHT CALCULATIONS
C
C               DETERMINE IF A STACK IS WITHIN A GEP 5L AREA OF INFLUENCE
C
C                 AS COMBINED STRUCTURES AND TIERS
C
C                   FOR EVERY QUARTER OF A DEGREE
C
C                     IDENTIFY TIER GROUPS - EXAMINE FOR COMBINING
C
C                     USE ACTUAL HEIGHTS - EACH GROUP FORMED AROUND FIRST TIER
C
C                     EVERY TIER IS USED AS FIRST OR 'FOCAL' TIER IN SUCCESSION
C
        WRITE(*,*) '  Looking for and calculating',
     +             ' any group of tiers GEP values'
        WRITE(*,*) '    for a wind flow starting at 0.25 degrees.'
      DO 110 D = 1, 1440
        ANG = D * DTR/4
        IF (MOD(D/4.,10.) .EQ. 0.) THEN
          WRITE(*,115) D/4
        END IF
        CSA = COS(ANG)
        SNA = -SIN(ANG)
C              First or 'Focal' Tier
      DO 111 I = 1, NB
       DO 112 J = 1, NTRS(I)
          C1 = (I - 1) * MXTRS + J
          CALL WIDTH (MB, MXTRS, MTS, MBT, X, Y, ND, XC, YC,
     *                XMAX, XMIN, YMAX, YMIN, ANG, I, J, C1, TW, BL)
          W(C1) = TW
          HT(C1) = TH(I,J)
           TNUM(C1) = 1
           TLIST(C1, 1) = C1
C                 Can the focal tier be combined with the other tiers ?
        DO 113 II = 1, NB
         IF (I .NE. II) THEN
          DO 114 JJ = 1, NTRS(II)
           C2 = (II - 1) * MXTRS + JJ
           CALL WIDTH (MB, MXTRS, MTS, MBT, X, Y, ND, XC, YC,
     *                  XMAX, XMIN, YMAX, YMIN, ANG, II, JJ, C2, TW, BL)
           W(C2) = TW
           HT(C2) = TH(II,JJ)
C            R is 'L'; combinable if distance between tiers is < L
C              If yes, add tier number to TLIST and increment counter TNUM
             R = MIN( W(C1), HT(C1) )
             IF (DISTMN(C1, C2) .LT. R) THEN
                TNUM(C1) = TNUM(C1) + 1
                TN1 = TNUM(C1)
                TLIST(C1,TN1) = C2
              ELSE
                 R = MIN(W(C2), HT(C2))
               IF (DISTMN(C1, C2) .LT. R) THEN
                  TNUM(C1) = TNUM(C1) + 1
                  TN1 = TNUM(C1)
                  TLIST(C1,TN1) = C2
               END IF
             END IF
114       CONTINUE
         END IF
113     CONTINUE
112    CONTINUE
111   CONTINUE
C
C                      FOR SUFFICIENTLY CLOSE STRUCTURES
C
C
C                                   COMBINE IDENTIFIED STRUCTURES BY GROUPS
       DO 120 I = 1, NB
        DO 121 J = 1, NTRS(I)
          C1 = (I-1) * MXTRS + J
C Combine tiers to each focal tier.
C Proceed, if more than 1 tier can be combined
         IF (TNUM(C1) .GT. 1) THEN
           TN1 = TNUM(C1)
           HTC = HT(TLIST(C1,1))
C Use every height in the TLIST set as the common height for combining
C Create focal subgroups based on common height; store numbers in TLIST2
          DO 122 T1 = 1, TN1
            TL1 = TLIST(C1,T1)
            HTA = HT(TLIST(C1,T1))
            CH = TL1
C use only those heights that are less than or equal to the focal tier height.
           IF (( HTA .LT. HTC) .OR. (C1 .EQ. TL1)) THEN

C Save the focal tier as first structure in the TLIST2 array.
            TNUM2(C1) = 1
            TLIST2(C1,1) = C1
C Initialize max & min X & Y coordinates for focal tier
            XMN = XMIN(C1)
            YMN = YMIN(C1)
            XMX = XMAX(C1)
            YMX = YMAX(C1)

C Check every candidate to see if it meets the combining criteria of L > DISTMN

           DO 123 T2 = 1, TN1

C Is a candidate structure above the common tier height, HTA ?

               TL2 = TLIST(C1,T2)
               C2 = TL2
            IF (C1 .NE. TL2) THEN
C Use only those heights that are greater than the common height.
             IF (HT(TL2) .GE. HTA) THEN

C Is the candidate structure within LTN1 or LTN2 of the focal structure, C1 ?

              LTN1 = MIN(HTA, W(C1))
              LTN2 = MIN(HTA, W(TL2))
              LTN = MAX(LTN1, LTN2)
               IF (DISTMN(C1, TL2) .LT. LTN) THEN
                 TNUM2(C1) = TNUM2(C1) + 1
                 TLIST2(C1,TNUM2(C1)) = TL2

C If so, combine by examining the candidate corner coordinates
C   with previous max & min values to derive overall combined width of
C   tiers in focal subgroup.

                IF (XMIN(TL2) .LT. XMN) XMN = XMIN(TL2)
                IF (XMAX(TL2) .GT. XMX) XMX = XMAX(TL2)
                IF (YMIN(TL2) .LT. YMN) YMN = YMIN(TL2)
                IF (YMAX(TL2) .GT. YMX) YMX = YMAX(TL2)
               END IF
             END IF
            END IF
123        CONTINUE
C
C         Projected width of all structures at or above the fixed height, HTA.
C
          TW = XMX - XMN
C********
          BL = YMX - YMN
          WS = TW
            IF (HTA .LT. TW) WS = HTA
              L5 = WS * 5
C            X AND Y COORDINATES DEFINING AREA CORNERS OF DISTURBED AIR FLOW
              CXMN = XMN
              CXMX = XMX
              CYMX = YMX + L5
              CYMN = YMN
C
C
C Examine width and height of focal tier subgroup with respect to stacks
C
C
C                       EVERY STACK COORDINATE IS ROTATED SO THAT THE FLOW
C                       VECTOR IS ALWAYS POINTING 'NORTH'.
C
C Perform only if more than one tier in focal tier subgroup
        IF (TNUM2(C1) .GT. 1) THEN
         DO 130 S = 1, NS
           XPSTK = XS(S) * CSA + YS(S) * SNA
           YPSTK = YS(S) * CSA - XS(S) * SNA
C
C            ARE STACK COORDINATES WITHIN INITIAL AREA OF DISTURBED AIR FLOW ?
C
          FLG1 = ((XPSTK .GE. CXMN) .AND. (XPSTK .LE. CXMX))
          FLG2 = ((YPSTK .GE. CYMN))
           IF (FLG1 .AND. FLG2) THEN
C            If source is within rectangle, check direct downwind
C            distance from side of focal tier to stack. IBET = 1 if
C            at or within 5L
             DO 131 K = 1, ND(I, J)
               X1 = XC(C1, K)
               Y1 = YC(C1, K)
               L = K + 1
                IF (L .GT. ND(I, J)) L = 1
               X2 = XC(C1, L)
               Y2 = YC(C1, L)
C Set IBET to 1 if stack on or W/I 5L of tier side
              CALL DISLIN(X1, Y1, X2, Y2, L5, IBET, XPSTK, YPSTK)
C If stack on top of roof, set IBET to 1
                IF (LFLAT (S,C1) .EQ. 1) IBET = 1
                IF (IBET .EQ. 1) THEN
                  CALL GPC (MB, MBT, MXTRS, MSK, BELEV, 
     *                      SB, GEP,GEPBH,GEPBW,GEPIN, TNUM2, TLIST2,
     *                      GTNUM, GTLIST, GDIRS, MI, MJ, 
     *                D, I, C1, S, TW, WS, HTA, TL1,2)
                   GO TO 136
                END IF
131          CONTINUE
C
C         If source is within rectangle, check direct downwind
C          distance from side of second tier to stack. IBET = 1 if
C          at or within 5L
C
             DO 135 M = 2, TNUM2(C1)
               C2 = TLIST2(C1,M)
                II = INT((C2-1)/MXTRS) + 1
                JJ = C2 - (II-1) * MXTRS
               DO 132 K = 1, ND(II, JJ)
                X1 = XC(C2, K)
                Y1 = YC(C2, K)
                 L = K + 1
                  IF (L .GT. ND(II, JJ)) L = 1
                X2 = XC(C2, L)
                Y2 = YC(C2, L)
                CALL DISLIN(X1, Y1, X2, Y2, L5, IBET, XPSTK, YPSTK)
                 IF (LFLAT (S,C2) .EQ. 1) IBET = 1
                 IF (IBET .EQ. 1) THEN
                   CALL GPC (MB, MBT, MXTRS, MSK, BELEV, 
     *                       SB, GEP,GEPBH,GEPBW,GEPIN, TNUM2, TLIST2,
     *                       GTNUM, GTLIST, GDIRS, MI, MJ, 
     *                D, I, C1, S, TW, WS, HTA, TL1, 2)
                    GO TO 136
                 END IF
132            CONTINUE
C         If source is within rectangle, check direct downwind
C          distance from side of gap filling structure (GFS) to stack.
C          IBET = 1, if at or within 5L
               DO K = 1, ND(I, J)
                X11 = XC(C1, K)
                Y11 = YC(C1, K)
                 K1 = K + 1
                  IF(K1 .GT. ND(I,J)) K1 = 1
                X12 = XC(C1, K1)
                Y12 = YC(C1, K1)
               DO L = 1, ND(II,JJ)
                X21 = XC(C2, L)
                Y21 = YC(C2, L)
                 K2 = L + 1
                  IF (K2 .GT. ND(II,JJ)) K2 = 1
                X22 = XC(C2,K2)
                Y22 = YC(C2,K2)
                DIST = SQRT ((X11-X21)**2 + (Y11-Y21)**2)
                 IF (DIST .LE. WS) THEN
                   CALL DISLIN(X11, Y11, X21, Y21, L5, IBET,
     *                         XPSTK, YPSTK)
                  IF (DIST .LE. L5) THEN
                   IF (IBET .EQ. 1) THEN
                     CALL GPC (MB, MBT, MXTRS, MSK, BELEV, 
     *                         SB, GEP,GEPBH,GEPBW,GEPIN, TNUM2, TLIST2,
     *                         GTNUM, GTLIST, GDIRS, MI, MJ, 
     *                         D, I, C1, S, TW, WS, HTA, TL1, 2)
                      GO TO 136
                   END IF
                  END IF
                 END IF
C                  calculate corner of one tier perpendicular to side
C                   of the other tier. If shorter than L use as
C                   perimeter of the GFS
                CALL CNRLIN(XI,YI,X11,Y11,X12,Y12,BET, DIST, X21, Y21)
                 IF (DIST .LE. WS .AND. BET .GT. -.001) THEN
                   CALL DISLIN(X21, Y21, XI, YI, L5, IBET, XPSTK, YPSTK)
                  IF (DIST .LE. L5) THEN
                   IF (IBET .EQ. 1) THEN
                     CALL GPC (MB, MBT, MXTRS, MSK, BELEV, 
     *                         SB, GEP,GEPBH,GEPBW,GEPIN, TNUM2, TLIST2,
     *                         GTNUM, GTLIST, GDIRS, MI, MJ, 
     *                         D,I,C1,S,TW,WS, HTA, TL1, 2)
                      GO TO 136
                   END IF
                  END IF
                 END IF
                CALL CNRLIN(XI,YI,X21,Y21,X22,Y22,BET, DIST, X11, Y11)
                 IF (DIST .LE. WS .AND. BET .GT. -.001) THEN
                   CALL DISLIN(X11, Y11, XI, YI, L5, IBET, XPSTK, YPSTK)
                  IF (DIST .LE. L5) THEN
                   IF (IBET .EQ. 1) THEN
                     CALL GPC (MB, MBT, MXTRS, MSK, BELEV, 
     *                         SB, GEP,GEPBH,GEPBW,GEPIN, TNUM2, TLIST2,
     *                         GTNUM, GTLIST, GDIRS, MI, MJ, 
     *                         D, I, C1, S, TW, WS, HTA, TL1, 2)
                      GO TO 136
                   END IF
                  END IF
                 END IF
               END DO 
               END DO
135          CONTINUE
136         CONTINUE
           END IF
130       CONTINUE
           END IF
          END IF
122      CONTINUE
          END IF
121     CONTINUE
120    CONTINUE
110   CONTINUE
C
C        GEP stack height value result(s)
C
        WRITE(12,1) TITLE
        WRITE(12, 297)
        WRITE(12,1005)

        WRITE(14, 1020)
      DO 1010 S = 1, NS
         IF(GTNUM(S) .EQ. 0) THEN
           PV = G65
           WRITE(14,1022) S, STKN(S), SH(S), PV, GEPBH(S), GEPBW(S),
     *                    GEP(S)
           WRITE(14,*) '     No tiers affect this stack.'
           WRITE(12,1001) STKN(S), SH(S), GEP(S), PV
          ELSE
           DIF = SB(S) - BELEV(MI(S,1))
C           GP = GEP(S)
           PV = MAX (G65, GEP(S))
             WRITE(14,1022) S, STKN(S), SH(S), PV, GEPBH(S), GEPBW(S),
     *                      GEP(S)
             WRITE(14,1025) DIF
             WRITE(14,1023) GTNUM(S), GDIRS(S)
             WRITE(14,1024) (GTLIST(S,I), I = 1, GTNUM(S))
             WRITE(12,1000) STKN(S), SH(S), DIF, GEP(S), PV
         END IF
         WRITE(14,297)

1010  CONTINUE
        WRITE(12,1007)
        WRITE(12,297)
        WRITE(12,297)
C
C                  CALCULATE MAXIMUM PROJECTED BUILDING WIDTH BY WIND VECTOR
C                    FOR SINGLE AND MULTIPLE NEARBY STRUCTURES
C                    USING STRUCTURE INFLUENCE ZONES (SIZes)
C
C
C                      Single structure and tier loop
C
C

        WRITE(*,*) ' '
        WRITE(*,*) 'CALCULATING BUILDING DOWNWASH INPUT VALUES.'
        WRITE(*,*) ' '

C Essentially a repeat of most of the code in the previous GEP half.

         WRITE(*,*) '  Calculating single tier downwash values.'
         WRITE(14, 2020)
      DO 300 D = 1, NDIR
        ANG = D * DDEG
          WRITE(14,604) ANG
        ANG = ANG * DTR
        CSA = COS(ANG)
        SNA = -SIN(ANG)
       DO 310 S = 1, NS
         XPSTK = XS(S) * CSA + YS(S) * SNA
         YPSTK = YS(S) * CSA - XS(S) * SNA
        DO 320 I = 1, NB
         DO 330 J = 1, NTRS(I)
           C = (I - 1) * MXTRS + J
           CH = C
           TNUM2(C) = 1
           HTA = TH(I,J)
           CALL WIDTH (MB, MXTRS, MTS, MBT, X, Y, ND, XC, YC,
     *                  XMAX, XMIN, YMAX, YMIN, ANG, I, J, C, TW, BL)
            WS = TW
             IF (TH(I, J) .LT. TW) WS = TH(I, J)
              L2 = WS * 2
              L5 = WS * 5
C                X AND Y COORDINATES DEFINING SIZ CORNERS
              CXMN = XMIN(C) - WS / 2
              CXMX = XMAX(C) + WS / 2
              CYMX = YMAX(C) + L5
              CYMN = YMIN(C) - L2
C                ARE STACK COORDINATES WITHIN SIZ ?
               FLG1 = ((XPSTK .GE. CXMN) .AND. (XPSTK .LE. CXMX))
               FLG2 = ((YPSTK .GE. CYMN))
               IF (FLG1 .AND. FLG2) THEN
C                  If so, calculate the PBW & PBH, save max values
                  TLIST2(C,1) = C
CDJM             SET ADJUSTED COORDINATES OF EFFECTIVE STRUCTURE (COORDINATES
C                        IN ISC REFERENCE FRAME I.E. X ALONG WIND)
               YBADJ = XPSTK - (XMIN(C) + TW * 0.5)
               XBADJ = YMIN(C) - YPSTK
CDJM
CVRT             CALL MXBWH(D, I, S, C, TW, HTA, WS, CH, 1)
CDJM             CALL MXBWH(D, I, S, C, TW, HTA, WS, CH, 1, BL)
                 IG = 1
                 CALL MXBWH(MB, MXTRS, MBT, MSK, MD, DPADX, DPADY, DFLG, 
     *                 DE, DHWE, DPBH, DPBL, DPBW, BELEV, SB,
     *                 GEP, GEPBH, GEPBW, GEPIN, MPADX, MPADY,
     *                 MHWE, MXPBH, MXPBL, MXPBW,
     *                 MI, MJ, TNUM2, TLIST2, MTNUM, MTLIST,
     *                 D, I, S, C, TW, HTA, WS, CH, IG, BL,
     +                     XBADJ, YBADJ)
               END IF
330         CONTINUE
320        CONTINUE
310      CONTINUE
      DO 2011 S = 1, NS
           WRITE(14,2022) S, STKN(S), SH(S)
           WRITE(14,2027) GEPBH(S), GEPBW(S), GEP(S)
         IF (MHWE(S,D) .EQ. 0.0) THEN
           WRITE(14,*) '    No single tier affects this stack for ',
     +                 'this direction.'
          ELSE
CVRT       WRITE(14,1026) MXPBH(S,D), MXPBW(S,D), MHWE(S,D)
           WRITE(14,1026) MXPBH(S,D), MXPBW(S,D),MXPBL(S,D), MHWE(S,D),
     +                    MPADX(S,D),MPADY(S,D)
           DIF = SB(S) - BELEV(MI(S,1))
           WRITE(14,1025) DIF
           WRITE(14,2028) MI(S,1), BTN(MI(S,1)), MJ(S,1)
         END IF
2011  CONTINUE

300   CONTINUE
      DO ISS = 1, NS
        DO D = 1, NDIR
          DFLG(ISS, D) = 0
        END DO
      END DO
C
C
C                      FOR SUFFICIENTLY CLOSE STRUCTURES
C
C
C                               IDENTIFY STRUCTURE GROUPS TO COMBINE
        WRITE(14,297)
         IF (NB .EQ. 1) THEN
           WRITE(14,*) 'Dominant combined buildings:  None'
          ELSE
           WRITE(*,*) '  Calculating group of tiers downwash values.'
           WRITE(14,*) 'Dominant combined buildings:'
         END IF
      DO 350 D = 1, NDIR
        ANG = D * DDEG
          IF (NB .GT. 1) WRITE(14,604) ANG
        ANG = ANG * DTR
        CSA = COS(ANG)
        SNA = -SIN(ANG)
      DO 360 I = 1, NB
       DO 370 J = 1, NTRS(I)
          C1 = (I - 1) * MXTRS + J
           CALL WIDTH (MB, MXTRS, MTS, MBT, X, Y, ND, XC, YC,
     *                 XMAX, XMIN, YMAX, YMIN, ANG, I, J, C1, TW, BL)
            W(C1) = TW
           TNUM(C1) = 1
           TLIST(C1, 1) = C1
        DO 380 II = 1, NB
         IF (I .NE. II) THEN
          DO 390 JJ = 1, NTRS(II)
           C2 = (II - 1) * MXTRS + JJ
            CALL WIDTH (MB, MXTRS, MTS, MBT, X, Y, ND, XC, YC,
     *                  XMAX, XMIN, YMAX, YMIN, ANG, II, JJ, C2, TW, BL)
            W(C2) = TW
               R = MIN(W(C1), HT(C1))
             IF (DISTMN(C1, C2) .LT. R) THEN
                TNUM(C1) = TNUM(C1) + 1
                TN1 = TNUM(C1)
                TLIST(C1,TN1) = C2
              ELSE
                 R = MIN(W(C2), HT(C2))
               IF (DISTMN(C1, C2) .LT. R) THEN
                  TNUM(C1) = TNUM(C1) + 1
                  TN1 = TNUM(C1)
                  TLIST(C1,TN1) = C2
               END IF
             END IF
390       CONTINUE
         END IF
380     CONTINUE
370    CONTINUE
360   CONTINUE
C
C
C                                   COMBINE IDENTIFIED STRUCTURES BY GROUPS
C              Combine sufficiently close structures and tiers
C                for centerline directions
       DO 430 I = 1, NB
        DO 440 J = 1, NTRS(I)
          C = (I-1) * MXTRS + J
C Proceed, if more than 1 tier can be combined
         IF (TNUM(C) .GT. 1) THEN
           TN1 = TNUM(C)
           HTC = HT(TLIST(C,1))
C Use every height in the TLIST set as a criterion for combining

          DO 450 T1 = 1, TN1
            TL1 = TLIST(C,T1)
            HTA = HT(TLIST(C,T1))
            CH = TL1
           IF (( HTA .LE. HTC) .OR. (C .EQ. TL1)) THEN
C Save focal tier height as first structure in the TLIST2 array.
             TNUM2(C) = 1
             TLIST2(C,1) = C
C Initialize max & min X & Y coordinates based on first structure
C  in TLIST array
              XMN = XMIN(C)
              YMN = YMIN(C)
              XMX = XMAX(C)
              YMX = YMAX(C)
C Check every candiate to see if it meets the combining criteria of L > DISTMN

           DO 460 T2 = 1, TN1
              TL2 = TLIST(C,T2)
            IF (C .NE. TL2) THEN

C Is a candidate structure high enough above the reference tier height ?

             IF (HT(TL2) .GE. HTA) THEN

C Is the candidate structure within L of the focal structure, C ?

              LTN1 = MIN(HTA, W(C))
              LTN2 = MIN(HTA, W(TL2))
              LTN = MAX(LTN1,LTN2)
               IF (DISTMN(C, TL2) .LT. LTN) THEN
                 TNUM2(C) = TNUM2(C) + 1
                 TLIST2(C,TNUM2(C)) = TL2

C If so, combine by examining the corner coordinates with max & min values

                IF (XMIN(TL2) .LT. XMN) XMN = XMIN(TL2)
                IF (XMAX(TL2) .GT. XMX) XMX = XMAX(TL2)
                IF (YMIN(TL2) .LT. YMN) YMN = YMIN(TL2)
                IF (YMAX(TL2) .GT. YMX) YMX = YMAX(TL2)
               END IF
             END IF
            END IF
460        CONTINUE
C
C              Projected width of all structures at or above a fixed height.
C
            TW = XMX - XMN
CVRT
            BL = YMX - YMN
CVRT
            WS = TW
C
            IF (HTA .LT. TW) WS = HTA
              L5 = WS * 5
              L2 = WS * 2
C            X AND Y COORDINATES DEFINING AREA CORNERS OF DISTURBED AIR FLOW

             CXMN = XMN - WS / 2
             CXMX = XMX + WS / 2
             CYMX = YMX + L5
             CYMN = YMN - L2
C
C                                   LOOP ON SOURCES FOR COMBINED BUILDINGS
C
C
C      EVERY STACK COORDINATE IS ROTATED SO THAT THE WIND
C      DIRECTION IS ALWAYS POINTING 'NORTH'.
C
         IF (TNUM2(C) .GT. 1) THEN
          IG = 2 
          DO 400 S = 1, NS
            XPSTK = XS(S) * CSA + YS(S) * SNA
            YPSTK = YS(S) * CSA - XS(S) * SNA
C                    ARE STACK COORDINATES WITHIN AREA OF DISTURBED AIR FLOW ?
C            If source is within rectangle, check
            FLG1 = ((XPSTK .GE. CXMN) .AND. (XPSTK .LE. CXMX))
            FLG2 = ((YPSTK .GE. CYMN))
             IF (FLG1 .AND. FLG2) THEN
C               If so, calculate the PBW & PBH, save max values
CDJM             SET ADJUSTED COORDINATES OF EFFECTIVE STRUCTURE (COORDINATES
C                        IN ISC REFERENCE FRAME I.E. X ALONG WIND)
              YBADJ = XPSTK - (XMN + TW * 0.5)
              XBADJ = YMN - YPSTK
CDJMCVRT           CALL MXBWH(D, I, S, C, TW, HTA, WS, CH, 2)
CDJM           CALL MXBWH(D, I, S, C, TW, HTA, WS, CH, 2, BL)
               IG = 2
               CALL MXBWH(MB, MXTRS, MBT, MSK, MD, DPADX, DPADY, DFLG,
     *                 DE, DHWE, DPBH, DPBL, DPBW, BELEV, SB,
     *                 GEP, GEPBH, GEPBW, GEPIN, MPADX, MPADY,
     *                 MHWE, MXPBH, MXPBL, MXPBW,
     *                 MI, MJ, TNUM2, TLIST2, MTNUM, MTLIST,
     *                 D, I, S, C, TW, HTA, WS, CH, IG, BL,
     +                       XBADJ,YBADJ)

             END IF
400       CONTINUE
         END IF
         END IF
450     CONTINUE
         END IF
440     CONTINUE
430    CONTINUE
      IF(NB .GT. 1) THEN
      DO 2012 S = 1, NS
         IFILE = S + 20
         WRITE(14,2022) S, STKN(S), SH(S)
         WRITE(14,2027) GEPBH(S), GEPBW(S), GEP(S)
         IF (MTNUM(S,2) .LT. 2) THEN
           WRITE(14,*)   '     No combined tiers affect this stack ',
     +                 'for this direction.'
C           WRITE(IFILE, 431) S, D
  431        FORMAT(1X,'S,D', 2I2,'  NO COMBINED TIERS FOR THIS DIRCTN')
          ELSE
           IF (DFLG(S,D) .EQ. 1) THEN
CVRT         WRITE(14,2026) DPBH(S), DPBW(S), DHWE(S)
CDJM         WRITE(14,2026) DPBH(S), DPBW(S), DPBL(S),DHWE(S)
             WRITE(14,2026) DPBH(S), DPBW(S), DPBL(S),DHWE(S),
     +                     DPADX(S),DPADY(S)
             DIF = SB(S) - BELEV(MI(S,2))
             WRITE(14,1025) DIF
             WRITE(14,2023) MTNUM(S,2)
             WRITE(14,2024) (MTLIST(S,M,2), M = 1, MTNUM(S,2))
  432        FORMAT(1X,'S,D',2I2, 2F6.2, I4, 2X, 10I3)
           ELSE
             WRITE(14,*) '     No combined tiers affect this stack ',
     +                   'for this direction'
          END IF
         END IF

2012  CONTINUE
      END IF
350   CONTINUE
C
C
C             OF ALL TIERS, PRINT/SAVE WHICH HAS MOST EFFECT BY STACK AND WD
        IF (SWT .EQ. 0 .OR. SWT .EQ. 2) THEN
            WRITE (12, 461)
            WRITE (12, 462) IMON, IDAY, IYR
            WRITE (12, 463) IHR, IMIN, ISEC
            WRITE (12, 297)
            WRITE (12, 1) TITLE
            WRITE (12, *) ' BPIP output is in meters'

          DO 510 S = 1, NS
              L = NDIR / 6
               WRITE(12,297)
            DO I = 1, 6
              J = (I-1) * 6 + 1
              K = I * 6
              WRITE (12,293) STKN(S), (MXPBH(S,D) , D = J,K)
            END DO
            DO I = 1, 6
              J = (I-1) * 6 + 1
              K = I * 6
              WRITE (12,296) STKN(S), (MXPBW(S,D) , D = J,K)
            END DO  
CVRT
            IF (SWT .EQ. 0) THEN
              DO I = 1, 6
                J = (I-1) * 6 + 1
                K = I * 6
                WRITE (12,299) STKN(S), (MXPBL(S,D) , D = J,K)
              END DO
            
              DO I = 1, 6
                J = (I-1) * 6 + 1
                K = I * 6
                WRITE (12,290) STKN(S), (MPADX(S,D) , D = J,K)
              END DO
              DO I = 1, 6
                J = (I-1) * 6 + 1
                K = I * 6
                WRITE (12,291) STKN(S), (MPADY(S,D) , D = J,K)
              END DO
            END IF
CDJM
CVRT
510      CONTINUE
        END IF
C
C            ISCLT2 output.  Use centerline values if greater than 0.00
C              Use values of other 2 of the 3 SIZs per sector, if
C              centerline value 0.00 and others not.
C
        IF (SWT .EQ. 1) THEN
         DO 560 S = 1, NS
          L = 1
          DO 570 D = 2, NDIR, 2
           L = L + 1
           I = D - 1
           J = D + 1
             IF (L .GT. ML) L = L - ML
             IF (J .GT. NDIR) J = J - NDIR
           IF (MHWE(S, D) .GT. 0.0) THEN
              IZ = D
           ELSE
            IF (MHWE(S, I) .GT. MHWE (S, J)) THEN
              IZ = I
            ELSE
              IZ = J
            END IF
           END IF
            MH(S,L) = MXPBH(S,IZ)
            MW(S,L) = MXPBW(S,IZ)
CVRT
            MBL(S,L) = MXPBL(S,IZ)
CVRT
CDJM
            MADX(S,L) = MPADX(S,IZ)
            MADY(S,L) = MPADY(S,IZ)
CDJM
570       CONTINUE
560      CONTINUE
             WRITE (12, 461)
             WRITE (12, 462) IMON, IDAY, IYR
             WRITE (12, 463) IHR, IMIN, ISEC
             WRITE (12, 297)
          DO 580 S = 1, NS
             WRITE (12,297)
              L = ND16 / 8
            DO 582 I = 1, 2
              J = (I-1) * 6 + 1
              K = I * 6
             WRITE (12,293) STKN(S), (MH(S,D) , D = J,K)
582         CONTINUE
             WRITE (12,293) STKN(S), (MH(S,D) , D = (K+1), ML)
            DO 584 I = 1, 2
              J = (I-1) * 6 + 1
              K = I * 6
             WRITE (12,296) STKN(S), (MW(S,D) , D = J,K)
584         CONTINUE
             WRITE (12,296) STKN(S), (MW(S,D) , D = (K+1), ML)
CVRT
C           PRIME in LT or an LT like model? If should happen, ready.
            IF (SWT .EQ. 0) THEN
            DO 586 I = 1, 2
              J = (I-1) * 6 + 1
              K = I * 6
             WRITE (12,299) STKN(S), (MBL(S,D) , D = J,K)
586         CONTINUE
             WRITE (12,299) STKN(S), (MBL(S,D) , D = (K+1), ML)
CVRT
            DO 587 I = 1, 2
              J = (I-1) * 6 + 1
              K = I * 6
             WRITE (12,290) STKN(S), (MADX(S,D) , D = J,K)
587         CONTINUE
             WRITE (12,290) STKN(S), (MADX(S,D) , D = (K+1), ML)
            DO 588 I = 1, 2
              J = (I-1) * 6 + 1
              K = I * 6
             WRITE (12,291) STKN(S), (MADY(S,D) , D = J,K)
588         CONTINUE
             WRITE (12,291) STKN(S), (MADY(S,D) , D = (K+1), ML)
            DO 589 I = 1, 2
              J = (I-1) * 6 + 1
              K = I * 6
589         CONTINUE
            END IF
580       CONTINUE
        END IF

        WRITE(*,*) ' '
        WRITE(*,*) 'END OF BPIP RUN.'
        WRITE(*,*) ' '

CVRT HARDWIRE CLOSE FILES
      CLOSE (10)
      CLOSE (12)
      CLOSE (14)
CVRT

C
C  FORMAT STATEMENTS
C
  1   FORMAT(1X,A78,/)
  2   FORMAT(/3X,'The ',A2,' flag has been set for preparing downwash',
     +       ' related data'
     +       /10X,'for a model run utilizing the PRIME algorithm.'/)
  3   FORMAT(3X,'Inputs entered in ', A10,' will be converted to ',
     + 'meters using '/3X,' a conversion factor of',F10.4,
     + '.  Output will be in meters.'/)
  4   FORMAT(3X,'The UTMP variable is set to ',A4,'.  The input is ',
     +'assumed to be in'/4X,
     +' UTM coordinates.  BPIP will move the UTM origin',
     *' to the first pair of'/4X,' UTM coordinates read.',
     *'  The UTM coordinates of the new origin will '/4X,
     *' be subtracted from all the other UTM coordinates entered to ',
     *'form '/4X,' this new local coordinate system.'/)
  5   FORMAT(3X,'UTMP is set to ',A4,'.  The input is assumed to be in',
     +' a local'/3x,' X-Y coordinate system as opposed to a UTM',
     +' coordinate system.'/3x,' True North is in the positive Y',
     +' direction.'/)
  6   FORMAT(1X,'Number of buildings to be processed :',I4)
  7   FORMAT(37X,'(',2F12.2,')')
  8   FORMAT(' Factor to convert from input units to meters is:',F10.4)
  9   FORMAT(/3X,'The ',A2,' flag has been set for preparing downwash',
     +           ' data for an ISCLT run.'/)
 11   FORMAT(3X,'The new local coordinates will be displayed in parent',
     *'heses just below'/4X,' the UTM coordinates they represent.',/)
 12   FORMAT(//1X,A8,' has',I2,' tier(s) with a base elevation of',
     * F8.2,' ',A10)
 13   FORMAT(47X,'(',F8.2,') meters',/)
 14   FORMAT(' BUILDING  TIER  BLDG-TIER  TIER   NO. OF      CORNER   ',
     +'COORDINATES',/
     *'   NAME   NUMBER   NUMBER  HEIGHT  CORNERS        X           Y'
     */)
 16   FORMAT(/3X,'The ',A2,' flag has been set for preparing downwash',
     +       ' related data'/10X,
     +       'for a model run **not** utilizing the PRIME algorithm.'/)
 17   FORMAT(3X,'Plant north is set to',F7.2,' degrees with respect to',
     *' True North.  '//)
 18   FORMAT(4X,'The plant coordinates will appear as entered in the ',
     *'Summary output'/4x,'file and they will be adjusted to True ',
     *'North prior to processing.'
     */4x,'The True North oriented coordinates appear ',
     *'below between'/4X,'the square brackets.')
 19   FORMAT(41X,'[',2F12.2,'] meters')
 21   FORMAT(1X,A8,I5,5X,I4,4X,F6.2,I6)
 22   FORMAT(27X,F6.2,' meters')
 31   FORMAT(42X,2F12.2, 1X, A10)
 32   FORMAT(42X,2F12.2, ' meters')
 33   FORMAT(41X,'(',2F12.2,') meters')
 41   FORMAT(/1X,'Number of stacks to be processed :',I4/)
 42   FORMAT('                    STACK            STACK   COORDINATES'/
     *       '  STACK NAME     BASE  HEIGHT          X           Y'/)
 43   FORMAT (2X, A8,3X, 2F8.2, 1X, A10)
 44   FORMAT (12X,'(', 2F8.2, ') meters')
 45   FORMAT (30X,'(', 2F12.2,') meters')
 46   FORMAT (30X,'[', 2F12.2,'] meters')
 47   FORMAT('  Caution: Blank spaces are not allowed in Stack names ',
     *'by ISC2 models.')
 49   FORMAT(31X,2F12.2, 1X, A10)
 51   FORMAT (31X, 2F12.2,' meters')
 71   FORMAT (//' The following lists the stacks that have been identi',
     *'fied'/   ' as being atop the noted building-tiers.'/)
 72   FORMAT(9X, ' STACK            BUILDING         TIER'/
     *       9X, ' NAME      NO.    NAME        NO.  NO.')
 73   FORMAT(10X, A8, I4, 5X, A8, 2(1X, I5))
115   FORMAT('      Wind flow passing', I4,' degree direction.')
290   FORMAT(5X,'SO XBADJ    ', A8, 6F8.2)
291   FORMAT(5X,'SO YBADJ    ', A8, 6F8.2)
292   FORMAT(3(/1X, 8F6.2))
293   FORMAT(5X,'SO BUILDHGT ', A8, 6F8.2)
296   FORMAT(5X,'SO BUILDWID ', A8, 6F8.2)
297   FORMAT(/)
299   FORMAT(5X,'SO BUILDLEN ', A8, 6F8.2)
411   FORMAT(/1X, A8)
461   FORMAT(30X,'BPIP (Dated: 04274)')
462   FORMAT(1X,'DATE : ',I2,'/',I2,'/',I4)
463   FORMAT(1X,'TIME : ',I2,':',I2,':',I2)
604   FORMAT(/1X,'Drtcn: ', F6.2/)
1000  FORMAT(8X, A8, 4(F8.2,5X))
1001  FORMAT(8X, A8, F8.2, 10X, 'N/A',5X, 3(F8.2,5X))
1005  FORMAT(16X,'PRELIMINARY* GEP STACK HEIGHT RESULTS TABLE'/
     *       13X,'            (Output Units: meters)'//
     *8X,'                    Stack-Building            Preliminary*'/
     *8X,' Stack    Stack     Base Elevation    GEP**   GEP Stack'/
     *8X,' Name     Height    Differences       EQN1    Height Value'//)
1007  FORMAT(/'   * Results are based on Determinants 1 & 2 on pages 1',
     *' & 2 of the GEP'/ '     Technical Support Document.  Determinan',
     *'t 3 may be investigated for'/'     additional stack height cred',
     *'it.  Final values result after'/'     Determinant 3 has been ta',
     *'ken into consideration.'/
     *'  ** Results were derived from Equation 1 on page 6 of GEP Tech',
     *'nical'/'     Support Document.  Values have been adjusted for a',
     *'ny stack-building'/'     base elevation differences.'//
     *'     Note:  Criteria for determining stack heights for modeling',
     *' emission'/
     *'     limitations for a source can be found in Table 3.1 of the'/
     *'     GEP Technical Support Document.'/)
1020  FORMAT(//'                     Overall GEP Summary Table'/
     *         '                          (Units: meters)'//)
1021  FORMAT(10X,'NOTE: The projected width values below are not always'
     *     ,/10X,'      the maximum width.  They are the minimum value,'
     *     ,/10X,'      valid for the stack in question, to derive the'
     *     ,/10X,'      maximum GEP stack height.'/)
1022  FORMAT(' StkNo:', I3,'  Stk Name:', A8,' Stk Ht:',F7.2,
     *' Prelim. GEP Stk.Ht:',F8.2,
     */11x,' GEP:  BH:',F7.2,'  PBW:',F8.2, 11X, '  *Eqn1 Ht:',F8.2)
1023  FORMAT('  No. of Tiers affecting Stk:', I3,'  Direction occurred:'
     *, F8.2)
1024  FORMAT('   Bldg-Tier nos. contributing to GEP:', 10I4)
1025  FORMAT(10X,'*adjusted for a Stack-Building elevation difference',
     * ' of',F8.2)
CVRT 1026  FORMAT(5X,  'Single tier MAX:  BH:',F7.2,'  PBW:',F7.2,
CVRT * '  PBL:',F7.2,'  *Wake Effect Ht:', F8.2)
1026  FORMAT(5X,  'Single tier MAX:  BH:',F7.2,'  PBW:',F7.2,
     * '  PBL:',F7.2,'  *Wake Effect Ht:', F8.2/5X,
     * 'Relative Coordinates of Projected Width Mid-point: XADJ: ',
     * F7.2,'  YADJ: ',F7.2/5X)
2020  FORMAT(//'                     Summary By Direction Table'/
     *         '                          (Units:  meters)',
     *// ' Dominate stand alone tiers:'/)
2022  FORMAT(' StkNo:', I3,'  Stk Name:', A8, 23X,'   Stack Ht:', F8.2)
CVRT 2026  FORMAT(3X,'Combined tier MAX:  BH:',F7.2,'  PBW:',F7.2,
CVRT * '  PBL:',F7.2,'  *Wake Effect Ht:', F8.2)
CDJM * '  PBL:',F7.2,'  *WE Ht:', F8.2)
2026  FORMAT(3X,'Combined tier MAX:  BH:',F7.2,'  PBW:',F7.2,
     * '  PBL:',F7.2,'  *WE Ht:', F8.2/5X,
     * 'Relative Coordinates of Projected Width Mid-point: XADJ: ',
     * F7.2,'  YADJ: ',F7.2/5X)
2027  FORMAT(11X,'      GEP:  BH:',F7.2,'  PBW:',F7.2,
     * '   *Equation 1 Ht:', F8.2)
2028  FORMAT(15X,' BldNo:', I3,'  Bld Name:', A8, '  TierNo:', I3)
2023  FORMAT('  No. of Tiers affecting Stk:', I3)
2024  FORMAT('   Bldg-Tier nos. contributing to MAX:', 10I4)

999    END

        SUBROUTINE CNRLIN (XI,YI,X1, Y1, X2, Y2, BET, DIST, XKP, YKP)
C            calculate corner perpendicular to side distance,
C             intercept point, and determine if intercept on or between
C             corners.

        IMPLICIT NONE

        REAL A1, A2, BET, DIST, SM, X1, X2, XI, XKP, Y1, Y2, YI, YKP

            IF ((X1 .NE. X2) .AND. (Y1 .NE. Y2)) THEN
               SM = (Y2 - Y1) / (X2 - X1)
               XI = (YKP + XKP / SM - Y1 + X1 * SM) / (SM + 1.0 / SM)
               YI = Y1 + (XI - X1) * SM
            ELSE
              IF ((Y2 .EQ. Y1)) THEN
                 XI = XKP
                 YI = Y1
              ELSE
                 XI = X1
                 YI = YKP
              END IF
            END IF
C
           DIST = SQRT((YI - YKP) ** 2 + (XI - XKP) ** 2)
C
C          Is the intercept point between the two corners of the
C            other structure ?
             A1 = (X1 - XI) ** 2 + (Y1 - YI) ** 2 +
     *            (X2 - XI) ** 2 + (Y2 - YI) ** 2
             A2 = (X1 - X2) ** 2 + (Y1 - Y2) ** 2
            BET = (A2 - A1)
        RETURN
        END
C
        SUBROUTINE DISLIN (X1, Y1, X2, Y2, L5, IBET, XSP, YSP)
C          calculate if stack directly downwind of a side and on or
C           within 5L of side.

          IMPLICIT NONE

          INTEGER IBET

          REAL D1, D2, DIST, DX1, DX2, L5, X1, X2, XSP, Y1, Y2, YI, YSP

          IBET = 0
          DX1 = MIN (X1, X2)
          DX2 = MAX (X1, X2)
          IF ((XSP .LT. DX1) .OR. (XSP .GT. DX2)) RETURN
          IF (Y1 .EQ. Y2) THEN
             DIST = YSP - Y1
             IF ((DIST .GE. 0.0) .AND. (DIST .LE. L5)) THEN
                IBET = 1
             END IF
          END IF
C
          IF (X1 .EQ. X2) THEN
            IF (XSP .EQ. X1) THEN
             D1 = YSP - Y1
               IF ((D1 .LE. L5) .AND. (D1 .GE. 0.0)) THEN
                 IBET = 1
               END IF
             D2 = YSP - Y2
               IF ((D2 .LE. L5) .AND. (D2 .GE. 0.0)) THEN
                 IBET = 1
               END IF
            END IF
          ELSE
            YI = Y2 + (XSP - X2) * (Y1 - Y2) / (X1 - X2)
              DIST = YSP - YI
              IF ((DIST .GE. 0.0) .AND. (DIST .LE. L5)) THEN
                 IBET = 1
              END IF
          END IF
        RETURN
      END

      SUBROUTINE GPC (MB, MBT, MT, MSK, BELEV,
     *                SB, GEP,GEPBH,GEPBW,GEPIN, TNUM2, TLIST2,
     *                GTNUM, GTLIST, GDIRS, MI, MJ,
     *                D, I, C, S, TW, WS, HTA, CH, IG)
C         calculate GEP values

        IMPLICIT NONE

        INTEGER C, CH, D, GEPIN, GTLIST, GTNUM,
     *          I, IG, M, MB, MBT, MSK, MT, MI, MJ, S, TNUM2, TLIST2

        REAL BELEV, GDIRS, GEP, GEPBH, GEPBW, HTA, HWE, SB, TW, WS

        DIMENSION BELEV(MB), SB(MSK)
        DIMENSION GEP(MSK), GEPBH(MSK), GEPBW(MSK),
     *            GEPIN(MSK,MBT,MBT,2)
        DIMENSION TNUM2(MBT), TLIST2(MBT,MBT)
        DIMENSION GTNUM(MSK), GTLIST(MSK,MBT), GDIRS(MSK)
        DIMENSION MI(MSK,2), MJ(MSK,2)

          HWE = HTA + BELEV(I) - SB(S) + 1.5 * WS
           GEPIN(S, C, CH, IG) = 1
            IF (HWE .GT. GEP(S)) THEN
              GEP(S) = HWE
              GEPBH(S) = HTA
              GEPBW(S) = TW
              GTNUM(S) = TNUM2(C)
              MI(S,1) = I
              MJ(S,1) = C - (I-1)*MT
               GDIRS(S) = FLOAT(D)/4
               DO 576 M = 1, GTNUM(S)
                 GTLIST(S,M) = TLIST2(C,M)
576            CONTINUE
            END IF
             IF (HWE .EQ. GEP(S)) THEN
               IF (TW .LT. GEPBW(S)) THEN
                 GEP(S) = HWE
                 GEPBH(S) = HTA
                 GEPBW(S) = TW
                 GTNUM(S) = TNUM2(C)
                 MI(S,1) = I
                 MJ(S,1) = C - (I-1)*MT
                 GDIRS(S) = FLOAT(D)/4
                  DO 578 M = 1, GTNUM(S)
                       GTLIST(S,M) = TLIST2(C,M)
578               CONTINUE
               END IF
             END IF
      RETURN
      END
C
CDJM  SUBROUTINE MXBWH(D, I, S, C, TW, HTA, WS, TL1, IG, BL)
      SUBROUTINE MXBWH(MB, MT, MBT, MSK, MD, DPADX, DPADY, DFLG,
     *                 DE, DHWE, DPBH, DPBL, DPBW, BELEV, SB,
     *                 GEP, GEPBH, GEPBW, GEPIN, MPADX, MPADY,
     *                 MHWE, MXPBH, MXPBL, MXPBW, 
     *                 MI, MJ, TNUM2, TLIST2, MTNUM, MTLIST,
     *                 D, I, S, C, TW, HTA, WS, TL1, IG, BL,
     +                 XBADJ,YBADJ)
C
C DIMENSION SUBSCRIPT FORMAT: (BUILDING OR STACK #, WD OR TIER #, SIDE #)
C
      IMPLICIT NONE

      INTEGER C, D, DE, DFLG, GEPIN, I, IG, 
     *        M, MB, MBT, MD, MFLG, MI, MJ, MSK, MT, MTLIST, MTNUM,
     *        S, SS, TNUM2, TLIST2, TL1

CVRT  REAL         MHWE, MXPBH, MXPBW
      REAL BELEV, BL, DHWE, DPADX, DPADY, DPBL, DPBH, DPBW,
     *     GEP, GEPBH, GEPBW, HWE, HTA,
     *     MHWE, MXPBH, MXPBW, MXPBL, MPADX, MPADY,
     *     PBH, PBL, PBW, SB, TW, WS,
     *     XBADJ, YBADJ

CVRT  COMMON /DE/ DE, DFLG(MSK, MD), DHWE(MSK), DPBH(MSK), DPBW(MSK)
CDJM &            DPBL(MSK)
      DIMENSION DFLG(MSK, MD), DHWE(MSK), DPBH(MSK), DPBW(MSK),
     &          DPBL(MSK),DPADX(MSK),DPADY(MSK)
      DIMENSION BELEV(MB), SB(MSK)
      DIMENSION GEP(MSK), GEPBH(MSK), GEPBW(MSK),
     +          GEPIN(MSK,MBT,MBT,2)
CVRT  COMMON /MXB/ MHWE(MSK, MD), MXPBH(MSK, MD), MXPBW(MSK, MD)
CDJM &             MXPBL(MSK, MD)
      DIMENSION MHWE(MSK, MD), MXPBH(MSK, MD), MXPBW(MSK, MD),
     &             MXPBL(MSK, MD),MPADX(MSK, MD), MPADY(MSK, MD)
      DIMENSION MI(MSK,2), MJ(MSK,2)
      DIMENSION TNUM2(MBT), TLIST2(MBT,MBT)
      DIMENSION MTNUM(MSK,2), MTLIST(MSK,MBT,2)
C
C    Stack is within GEP 5L ?
C
      IF (GEPIN(S,C,TL1,IG) .EQ. 1) THEN

        MFLG = 0
        PBH = HTA
        PBW = TW
CVRT
        PBL = BL
CVRT
        HWE = HTA + BELEV(I) - SB(S) + 1.5 * WS

       IF (DE .NE. D) THEN
         DE = D
         DO SS = 1, MSK
           DHWE(SS) = 0.0
           DPBH(SS) = 0.0
           DPBW(SS) = 0.0
c           MHWE(SS, D) = 0.00
c           MXPBH(SS, D) = 0.00
c           MXPBW(SS, D) = 0.00
CVRT
           DPBL(SS) = 0.0
CVRT
CDJM
           DPADX(SS) = 0.0
           DPADY(SS) = 0.0
CDJM
           MTNUM(SS,IG) = 0
           DFLG(SS, D) = 0
         END DO
       END IF
          IF (HWE - MHWE(S, D) .GT. .001) THEN
            MHWE(S, D) = HWE
            MXPBH(S, D) = PBH
            MXPBW(S, D) = PBW
CVRT
            MXPBL(S,D) = PBL
CVRT
CDJM
            MPADX(S,D) = XBADJ
            MPADY(S,D) = YBADJ
CDJM
            MFLG = 1

          END IF
C When wake effects are equal, use those values with a lesser projected width.
          IF (ABS(HWE - MHWE(S,D)) .LT. .001) THEN
            IF (PBW .LT. MXPBW(S,D)) THEN
              MHWE(S, D) = HWE
              MXPBH(S, D) = PBH
              MXPBW(S, D) = PBW
CVRT
              MXPBL(S,D) = PBL
CVRT
CDJM
              MPADX(S,D) = XBADJ
              MPADY(S,D) = YBADJ
CDJM
              MFLG = 1
            END IF
          END IF
C When a wake effect height is greater than the GEP STK Ht, use the GEP values
          IF (GEP(S) .LT. MHWE(S,D))  THEN
            MHWE(S, D) = GEP(S)
            MXPBW(S,D) = GEPBW(S)
            MXPBH(S,D) = GEPBH(S)
            MXPBL(S,D) = PBL
            MPADX(S,D) = XBADJ
            MPADY(S,D) = YBADJ
            MFLG = 1
          END IF
C Update and retain data for the Summary by Direction Tables
          IF (MFLG .EQ. 1) THEN
            DFLG(S,D) = 1
            MTNUM(S,1) = TNUM2(C)
            MI(S,IG) = I
            MJ(S,IG) = C - (I-1)*MT
             DO M = 1, MTNUM(S,IG)
               MTLIST(S,M,IG) = TLIST2(C,M)
             END DO
            MFLG = 0
          END IF
C Determine if any combined buildings exist for a particular sector
C Save the combination producing the highest HWE for
C the Summary Table
        IF (TNUM2(C) .GT. 1) THEN
          IF (HWE .GT. DHWE(S)) THEN
            MFLG = 2
          END IF
          IF ((ABS(HWE-DHWE(S)).LT. .001) .AND. (PBW .LT. DPBW(S))) THEN
            MFLG = 2
          END IF
          IF (MFLG .EQ. 2) THEN
            DFLG(S, D) = 1
            DHWE(S) = HWE
            DPBW(S) = PBW
            DPBH(S) = PBH
CVRT
            DPBL(S) = PBL
CVRT
CDJM
            DPADX(S) = XBADJ
            DPADY(S) = YBADJ
CDJM
            MFLG = 0
            MTNUM(S,IG) = TNUM2(C)
            MI(S,IG) = I
            MJ(S,IG) = C - (I-1) * MT
            DO M = 1, MTNUM(S,IG)
              MTLIST(S,M,IG) = TLIST2(C,M)
            END DO
          END IF
         END IF
        END IF
      RETURN
      END
C
      SUBROUTINE WIDTH (MB, MT, MTS, MBT, X, Y, ND, XC, YC,
     *                  XMAX, XMIN, YMAX, YMIN, ANG, I, J, C, TW, BL)
C         Calculate projected building width, TW

        IMPLICIT NONE

        INTEGER C, I, J, K, MB, MBT, MT, MTS, ND

        REAL ANG, BL, TW, XC, XMAX, XMIN, YC, YMAX, YMIN

        DOUBLE PRECISION X, Y, CSA, SNA

        DIMENSION X(MB, MT, MTS), Y(MB, MT, MTS), ND(MB, MT)
        DIMENSION XC(MBT, MTS), YC(MBT, MTS)
        DIMENSION XMAX(MBT), XMIN(MBT), YMAX(MBT), YMIN(MBT)

            CSA = DCOS(DBLE(ANG))
            SNA = -DSIN(DBLE(ANG))
          DO 700 K = 1, ND(I,J)
             XC(C, K) = SNGL(X(I, J, K) * CSA + Y(I, J, K) * SNA)
             YC(C, K) = SNGL(Y(I, J, K) * CSA - X(I, J, K) * SNA)
             IF (K .EQ. 1) THEN
               YMIN(C) = YC(C, 1)
               YMAX(C) = YC(C, 1)
               XMIN(C) = XC(C, 1)
               XMAX(C) = XC(C, 1)
             END IF
C           WHICH TIER CORNER IS FURTHEST NORTH, SOUTH, EAST AND WEST
             IF (YC(C, K) .LE. YMIN(C)) YMIN(C) = YC(C, K)
             IF (YC(C, K) .GE. YMAX(C)) YMAX(C) = YC(C, K)
             IF (XC(C, K) .LE. XMIN(C)) XMIN(C) = XC(C, K)
             IF (XC(C, K) .GE. XMAX(C)) XMAX(C) = XC(C, K)
700       CONTINUE
            TW = XMAX(C) - XMIN(C)
CVRT  Add building length calculation.
            BL = YMAX(C) - YMIN(C)

      RETURN
      END

