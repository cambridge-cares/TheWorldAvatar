      SUBROUTINE METEXT
C***********************************************************************
C                METEXT Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Controls Extraction and Quality Assurance of
C                 One Hour of Meteorological Data
C
C        PROGRAMMER: ROGER BRODE, JEFF WANG
C        MODIFIED BY D. Strimaitis, SRC (for Wet & Dry DEPOSITION)
C
C        DATE:    November 8, 1993
C
C        Revision History:
C
C        MODIFIED:   Modified to store additional variables needed
C                    for MAXDCONT option when the URBAN option is 
C                    being used.  Previous version could report
C                    erroneous source group contributions in the
C                    MAXDCONT file for any application that included
C                    urban sources.
C
C                    Modified to only increment hour index and year
C                    index if MAXDCONT option is being used. This
C                    change avoids unnecessary runtime error if the
C                    met data file includes a single hour beyond
C                    the maximum number of years specified by the
C                    NYEARS PARAMETER, which is initialized to 5 in 
C                    the MAIN1 module.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 12/19/2011
C
C        MODIFIED:   Modified code for setting the month, day, and hour 
C                    for the "end of the year", based on the first hour
C                    of the meteorological data file, to resolve potential 
C                    problems for PM-2.5 and ANNUAL average applications 
C                    in which the first hour of the file is not 01.
C
C                    Modified to include check for end-of-file (EOF) on
C                    first data record, and to assign file type for possible
C                    end-of-file error messages.
C
C                    Included code to check for presence of additional 
C                    variables in the surface meteorological file needed 
C                    for use of the deposition options when required.
C                    Also checks for minimum number of data fields for
C                    applications without deposition.
C
C                    Include checks for potential conflicts between 
C                    first date of met data file being later than 
C                    start date from re-start file or user-specified 
C                    start date on STARTEND keyword, with fatal error 
C                    message for conflict with INITFILE restarts and 
C                    warning for MULTYEAR restarts.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
C
C        MODIFIED:   Modified code for processing multi-year data
C                    files to determine if header record is present
C                    between years for concatenated files.  Use presence
C                    of colon (':') as only criterion for header record.
C                    Use warning messages if UAIR and SURF IDs don't
C                    match input runstream file for multiple years since
C                    AERMOD allows mismatch for single year files.
C                    Modified check for stable or missing hours in 
C                    calculation of solar irradiance (QSW) for use
C                    in deposition calculations.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
C
C        MODIFIED:   Modified code for reading the header record of the
C                    surface file to use a character variable for the
C                    AERMET version date field, in order to allow for
C                    the future use of screening meteorology that is not
C                    directly linked to a specific version under the
C                    of the AERMET processor under the SCREEN option.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 01/26/2007
C
C        MODIFIED BY D. Strimaitis, SRC (for Dry DEPOSITION)
C        (DATE:    February 15, 1993)
C
C        MODIFIED:   To avoid potential math error due to negative
C                    ambient temperatures in calculating the square
C                    root of the stability parameter, RTOFS - 4/19/93
C
C        MODIFIED:
C        7/27/94     J. Paumier, PES, Inc.
C                    The variables for displacement height, ZDM and
C                    AZDM(), were removed from the input to and output
C                    from ISC-COMPDEP.  The following format statements
C                    also were affected: 9009, 9026, 9032, 9033
C
C*       7/27/94     J. Hardikar, PES, Inc.
C*                   Added code to calculate reference wind speed at 10m
C*                   to be used for OPENPIT source algorithms
C
C        MODIFIED:   To incorporate modifications to date processing
C                    for Y2K compliance, including use of date window
C                    variables (ISTRT_WIND and ISTRT_CENT) and calculation
C                    of 10-digit date variable (FULLDATE) with 4-digit
C                    year for date comparisons.
C                    Also moved call to METDAT to allow use of single
C                    METDAT routine for normal and EVENT processing.
C                    R.W. Brode, PES, Inc., 5/12/99
C
C        MODIFIED:   To remove support for unformatted meteorological
C                    data files.
C                    R.W. Brode, PES, Inc., 4/10/2000
C
C        MODIFIED:   To correct potential problem with check for
C                    concatenated data files.
C                    R.W. Brode, PES, Inc., 9/15/2000
C
C        MODIFIED:   To incorporate additional variables for dry
C                    and wet deposition, and to remove formatted
C                    read for surface file.  Surface file is now
C                    read FREE format.
C                    R.W. Brode, PES, Inc., 9/29/2003
C
C        MODIFIED:   Moved call to SUB. METDAT ahead of call to
C                    SUB. SET_METDATA to avoid potential problem
C                    with negative precipitation for first hour
C                    of data.
C                    R.W. Brode, MACTEC, 10/26/2004
C
C        INPUTS:  Meteorology File Specifications
C
C        OUTPUTS: Meteorological Variables for One Hour
C
C        CALLED FROM:   HRLOOP
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      USE BUOYANT_LINE
      IMPLICIT NONE
      CHARACTER MODNAM*12

C---- Constants used in the computation of QSW
      DOUBLE PRECISION, PARAMETER :: C1=5.31D-13, C2=60.0D0, C3=1.12D0,
     &                               STEFB= 5.67D-08
      DOUBLE PRECISION :: RN, Es25
      INTEGER :: IDYMAX(12), IJDAY, JFLAG, LEVEL
C     JAT D065 8/9/21
C     IOSI SET BUT NOT USED
C      INTEGER :: IUSI, ISSI, IOSI
      INTEGER :: IUSI, ISSI
      CHARACTER (LEN=8)   :: CUSI, CSSI, COSI
      CHARACTER (LEN=6)   :: Temp_METVER
      CHARACTER (LEN=256) :: BUFFER
      INTEGER :: I, NFLD
C Unused:      INTEGER :: IYR4
C Unused:       CHARACTER (LEN=6)   :: SPEC1, SPEC2, SPEC3
C JAT 06/22/21 D065
C REMOVE NTURB_Warnings AS UNUSED VARIABLE
C      INTEGER :: NTURB_Warnings

C     Variable Initializations
      DATA IDYMAX/31,29,31,30,31,30,31,31,30,31,30,31/
      DATA Temp_METVER/'      '/

C --- Initialize variable to track warnings regarding use of turbulence
C     data with ADJ_U* option
C JAT 06/22/21 D065
C REMOVE NTURB_Warnings INITIALIZATION AS UNUSED VARIABLE
C      DATA NTURB_Warnings/0/

      MODNAM = 'METEXT'
      PATH   = 'MX'

C     Save Value of Last YR/MN/DY/HR and Previous Hour
      IPDATE = KURDAT
      IPYEAR = IYR
      IPHOUR = IHOUR

C     Initialize USTAR, OBULEN, SFCZ0, QSW, IPCODE, AND PRATE to ZERO for hour
      USTAR  = 0.0D0
      OBULEN = 0.0D0
      SFCZ0  = 0.0D0
cjop  ZDM    = 0.0D0
      QSW    = 0.0D0
      IPCODE = 0
      PRATE  = 0.0D0

c     JAT D070 intialize reset_sa and reset_sw
      RESET_SA=.FALSE.
      RESET_SW=.FALSE.
      
C --- Increment line counter for messages related to met data file
      ILINE = ILINE + 1

C --- Set 'DUMMY' variable = 'SURFFILE' for error handling
      DUMMY = 'SURFFILE'

      IF (IMONTH .EQ. 12 .AND. IDAY .EQ. 31 .AND. IHOUR .EQ. 24) THEN
C        End of year has been reached - check for presence of header
C        record at beginning of next year for multi-year data files.
         READ(MFUNIT,'(A256)',ERR=998,END=1000,IOSTAT=IOERRN) BUFFER

         IF (INDEX(BUFFER,':') .EQ. 0) THEN
C           Record does not contain colon. Assume it must be regular
C           met data record, so backspace met file before proceeding.
            BACKSPACE MFUNIT
         ELSE
C           Record contains colon. Assume it is a header record;
C           check for AERMET version date, C_METVER, and also check
C           station IDs before proceeding to flag potential
C           use of different stations in multi-year data files.
C           Convert UAIR and SURF character IDs to integers.
C ---       First extract AERMET version date, Temp_METVER
            NumHeaders = NumHeaders + 1
            IF( INDEX(BUFFER,'VERSION:') .NE. 0 )THEN
C              Extract AERMET version date
               READ(BUFFER(INDEX(BUFFER,'VERSION:')+8:
     &                     INDEX(BUFFER,'VERSION:')+13),'(A6)')
     &                                                 Temp_METVER
            ELSE
C              AERMET version not found in header record, issue fatal error message
               CALL ERRHDL(PATH,MODNAM,'E','395','No Version')      
            ENDIF
            IF (Temp_METVER .NE. C_METVER) THEN
C              AERMET version not found in header record, or different AERMET versions
C              were used; issue fatal error message
               CALL ERRHDL(PATH,MODNAM,'E','395',Temp_METVER)      
            ENDIF

C ---       Now extract UA, SF, and OS station IDs from header record
            IF( INDEX(BUFFER,'UA_ID:') .GE. 0 )THEN
               READ(BUFFER(INDEX(BUFFER,'UA_ID:')+7:
     &                     INDEX(BUFFER,'UA_ID:')+15),'(A)') CUSI
            ELSE
               CUSI = '        '
            END IF
            CALL STONUM(CUSI,8,FNUM,IMIT)
            IF (IMIT .EQ. 1) THEN
               IUSI = NINT(FNUM)
            ELSE
               IUSI = 0
            END IF

            IF( INDEX(BUFFER,'SF_ID:') .GE. 0 )THEN
               READ(BUFFER(INDEX(BUFFER,'SF_ID:')+7:
     &                     INDEX(BUFFER,'SF_ID:')+15),'(A)') CSSI
            ELSE
               CSSI = '        '
            END IF
            CALL STONUM(CSSI,8,FNUM,IMIT)
            IF (IMIT .EQ. 1) THEN
               ISSI = NINT(FNUM)
            ELSE
               ISSI = 0
            END IF

            IF( INDEX(BUFFER,'OS_ID:') .GE. 0 )THEN
               READ(BUFFER(INDEX(BUFFER,'OS_ID:')+7:
     &                     INDEX(BUFFER,'OS_ID:')+15),'(A)') COSI
            ELSE
               COSI = '        '
            END IF
            CALL STONUM(COSI,8,FNUM,IMIT)
C           JAT D065 IOSI NOT USED OTHER THAN BEING SET HERE
C           COMMENT OUT THIS CODE
C            IF (IMIT .EQ. 1) THEN
C               IOSI = NINT(FNUM)
C            ELSE
C               IOSI = 0
C            END IF

C ----      Check for consistency between UA and SF station IDs in header record
C           with values input by user on the ME pathway
            IF (ISSI .NE. IDSURF) THEN
C              Write Warning Message:  SURFDATA id mismatch
               CALL ERRHDL(PATH,MODNAM,'W','530','SURFDATA')
            END IF
            IF (IUSI .NE. IDUAIR) THEN
C              Write Warning Message:  UAIRDATA id mismatch
               CALL ERRHDL(PATH,MODNAM,'W','530','UAIRDATA')
            END IF
         END IF

         GO TO 1001

C        Error reading 'header record' - assume that header record is
C        missing.  Backspace met file and continue processing. 
 998     BACKSPACE MFUNIT

      END IF


1001  CONTINUE

C
C---- READ surface scaling meteorology data based on free format
C

      IF( LDPART .OR. LWPART .OR. LDGAS .OR. LWGAS .OR. GRSM )THEN
C        Check for deposition variables on first data record
C        CERC 11/30/20 Calculation of QSW also needed for GRSM
         IF (ILINE .EQ. 1) THEN
            NFLD  = 0
            INFLD = .FALSE.
            READ(MFUNIT,'(A256)',ERR=99,END=1000,IOSTAT=IOERRN) BUFFER
C ---       Check for wind data source/adjustment flag from version
C           11059 of AERMET and later
C           modified 12/11/17 to account for use of MMIF straight to AERMOD
C           so user doesn't get misleading warning message
C           JAT 5/8/2020 ADDED FROM VERSION 19191
C           Add MMIF-OS to the list of flags below to
C           avoid getting outdated met version warning
c           JAT 1/14/21 ISSUE D077 ADD PROG-OS and PROG-Mod to
c           ACCOMODATE NEW AERMET USING GENERIC PROG MET
            IF( INDEX(BUFFER,'NAD') .GT. 0 .OR. 
     &          INDEX(BUFFER,'ADJ') .GT. 0 .OR. 
     &          INDEX(BUFFER,'MIFF-Mod') .GT. 0 .OR. 
     &          INDEX(BUFFER,'MMIF-OS') .GT. 0 .OR.
     &          INDEX(BUFFER,'PROG-Mod') .GT. 0 .OR.  !JAT D077
     &          INDEX(BUFFER,'PROG-OS') .GT. 0 )THEN !JAT D077
               L_NAD_ADJ_Flags = .TRUE.
            ENDIF
            DO I = 1, LEN_TRIM(BUFFER)
               IF (.NOT.INFLD .AND. BUFFER(I:I).NE.' ' .AND. 
     &                              BUFFER(I:I).NE.',') THEN
C                 Set Mark of in a Field
                  INFLD = .TRUE.
C                 Increment the Field Counter
                  NFLD = NFLD + 1
               ELSE IF (INFLD .AND. (BUFFER(I:I).EQ.' ' .OR.
     &                               BUFFER(I:I).EQ.',')) THEN
C                 Location is the End of a Field
C                 Set Mark of Not In a field
                  INFLD = .FALSE.
               END IF
            END DO
            IF (NFLD .LT. 25 .AND. (LDPART .OR. LWPART .OR. 
     &                               LDGAS .OR. LWGAS .OR. 
     &                               GRSM) ) THEN
C              Met record does not have enough fields, 
C              deposition variables may be missing
               CALL ERRHDL(PATH,MODNAM,'E','495','with DEP')
               RUNERR = .TRUE.
               EOF = .TRUE.
               GO TO 99
            ELSE
               BACKSPACE MFUNIT
            END IF

         ELSE IF (ILINE .GT. 1) THEN
            READ(MFUNIT,'(A256)',ERR=99,END=1000,IOSTAT=IOERRN) BUFFER
C ---       Check for wind data source/adjustment flag from version
C           11059 of AERMET and later
C           JAT add MMIF to AERMOD in flag check 12/11/17
C           JAT 5/8/2020 ADDED FROM VERSION 19191
C           Add MMIF-OS to the list of flags below to
C           avoid getting outdated met version warning
c           JAT 1/14/21 ISSUE D077 ADD PROG-OS and PROG-Mod to
c           ACCOMODATE NEW AERMET USING GENERIC PROG MET
            IF( INDEX(BUFFER,'NAD') .GT. 0 .OR. 
     &          INDEX(BUFFER,'ADJ') .GT. 0 .OR.
     &          INDEX(BUFFER,'MIFF-Mod') .GT. 0 .OR. 
     &          INDEX(BUFFER,'MMIF-OS') .GT. 0 .OR.
     &          INDEX(BUFFER,'PROG-Mod') .GT. 0 .OR. !JAT D077
     &          INDEX(BUFFER,'PROG-OS') .GT. 0)THEN !JAT D077
               L_NAD_ADJ_Flags = .TRUE.
            ELSE
C ---          Wind data source/adjustment flag is missing
               L_NAD_ADJ_Flags = .FALSE.
            ENDIF
            BACKSPACE MFUNIT

         ENDIF
            
C ---    Read record from ASCII scalar parameter file using FREE format
C        with deposition variables
C
         READ( MFUNIT, *, END=1000, ERR=99, IOSTAT=IOERRN ) IYEAR,
     &         IMONTH, IDAY, IJDAY, IHOUR, SFCHF, USTAR, WSTAR,
     &         VPTGZI, ZICONV, ZIMECH, OBULEN, SFCZ0, BOWEN, ALBEDO,
     &         UREF, WDREF, UREFHT, TA, TREFHT, IPCODE, PRATE, RH,
     &         SFCP, NCLOUD

C        Calculate solar irradiance, QSW, from Heat Flux, Bowen ratio,
C        albedo and cloud cover, for use in gas deposition algorithm. 
         IF (OBULEN.GT.0.0D0 .OR. OBULEN.LT.-99990.0D0 .OR. 
     &                            TA.LT.0.0D0 .OR.
     &                        ALBEDO.EQ.1.0D0 .OR. BOWEN.EQ.0.0D0) THEN
C           Hour is stable or missing or inappropriate surface chars.
            QSW = 0.0D0
         ELSE
            RN  = (1.0D0 + 1.0D0/BOWEN)*SFCHF/0.9D0
            QSW = (RN*(1.0D0+C3) - C1*TA**6 + STEFB*TA**4 - 
     &                                    C2*0.1D0*DBLE(NCLOUD))/
     &                                       (1.0D0-ALBEDO)
         END IF
C
C        Set variables for dry deposition
         IF (LDPART .OR. LDGAS) THEN
            IF (Ta.LT.0.0D0 .OR. PRATE.LT.0.0D0) THEN
               Wnew = Wold
            ELSE
c ...          Compute saturation vapor pressure based on CMAQ formula
               EsTa = 0.6112D0 * DEXP(19.83D0 - 5417.4D0/Ta)
               Es25 = 3.167D0
               Wnew = Wold+Prec1-0.5D0*f2*EsTa/Es25
               Wold = Wnew
               f2 = Wnew/200.D0
               if (f2.le.0.01D0) f2 = 0.01D0
               if (f2.gt.1.0D0) f2 = 1.0D0
            END IF
         END IF

      ELSE
C        Read record from ASCII scalar parameter file without deposition
C        parameters, using FREE format
C        Check for number of data fields on first data record
         IF (ILINE .EQ. 1) THEN
            NFLD  = 0
            INFLD = .FALSE.
            READ(MFUNIT,'(A256)',ERR=99,END=1000,IOSTAT=IOERRN) BUFFER
C ---       Check for wind data source/adjustment flag from version
C           11059 of AERMET and later
C           modified 12/11/17 to account for use of MMIF straight to AERMOD
C           so user doesn't get misleading warning message
C           JAT 5/8/2020 ADDED FROM VERSION 19191
C           Add MMIF-OS to the list of flags below to
C           avoid getting outdated met version warning
c           JAT 1/14/21 ISSUE D077 ADD PROG-OS and PROG-Mod to
c           ACCOMODATE NEW AERMET USING GENERIC PROG MET
            IF( INDEX(BUFFER,'NAD') .GT. 0 .OR. 
     &          INDEX(BUFFER,'ADJ') .GT. 0 .OR.
     &          INDEX(BUFFER,'MIFF-Mod') .GT. 0 .OR. 
     &          INDEX(BUFFER,'MMIF-OS') .GT. 0 .OR.
     &          INDEX(BUFFER,'PROG-Mod') .GT. 0 .OR. !JAT D077
     &          INDEX(BUFFER,'PROG-OS') .GT. 0)THEN !JAT D077
               L_NAD_ADJ_Flags = .TRUE.
            ENDIF
            DO I = 1, LEN_TRIM(BUFFER)
               IF (.NOT.INFLD .AND. BUFFER(I:I).NE.' ' .AND. 
     &                              BUFFER(I:I).NE.',') THEN
C                 Set Mark of in a Field
                  INFLD = .TRUE.
C                 Increment the Field Counter
                  NFLD = NFLD + 1
               ELSE IF (INFLD .AND. (BUFFER(I:I).EQ.' ' .OR.
     &                               BUFFER(I:I).EQ.',')) THEN
C                 Location is the End of a Field
C                 Set Mark of Not In a field
                  INFLD = .FALSE.
               END IF
            END DO
            IF (NFLD .LT. 20) THEN
C              Met record does not include enough variables
               CALL ERRHDL(PATH,MODNAM,'E','495','Non-DEP ')
               RUNERR = .TRUE.
               EOF = .TRUE.
               GO TO 99
            ELSE
               BACKSPACE MFUNIT
            END IF

         ELSE IF (ILINE .GT. 1) THEN
            READ(MFUNIT,'(A256)',ERR=99,END=1000,IOSTAT=IOERRN) BUFFER
C ---       Check for wind data source/adjustment flag from version
C           11059 of AERMET and later
C           modified 12/11/17 to account for use of MMIF straight to AERMOD
C           so user doesn't get misleading warning message
C           JAT 5/8/2020 ADDED FROM VERSION 19191
C           Add MMIF-OS to the list of flags below to
C           avoid getting outdated met version warning
c           JAT 1/14/21 ISSUE D077 ADD PROG-OS and PROG-Mod to
c           ACCOMODATE NEW AERMET USING GENERIC PROG MET
            IF( INDEX(BUFFER,'NAD') .GT. 0 .OR. 
     &          INDEX(BUFFER,'ADJ') .GT. 0 .OR.
     &          INDEX(BUFFER,'MIFF-Mod') .GT. 0 .OR. 
     &          INDEX(BUFFER,'MMIF-OS') .GT. 0 .OR. 
     &          INDEX(BUFFER,'PROG-Mod') .GT. 0 .OR. !JAT D077
     &          INDEX(BUFFER,'PROG-OS') .GT. 0)THEN !JAT D077
               L_NAD_ADJ_Flags = .TRUE.
            ELSE
C ---          Wind data source/adjustment flag is missing
               L_NAD_ADJ_Flags = .FALSE.
            ENDIF

            BACKSPACE MFUNIT

         END IF
C
         READ( MFUNIT, *, END=1000, ERR=99, IOSTAT=IOERRN ) IYEAR,
     &         IMONTH, IDAY, IJDAY, IHOUR, SFCHF, USTAR, WSTAR,
     &         VPTGZI, ZICONV, ZIMECH, OBULEN, SFCZ0, BOWEN, ALBEDO,
     &         UREF, WDREF, UREFHT, TA, TREFHT, IPCODE, PRATE, RH,
     &         SFCP, NCLOUD
      END IF

C --- Check for L_NAD_ADJ_Flags; if surface file header shows current
C     version date (i.e., L_OldMetVer=.F.), but the wind data 
C     source/adj flags are missing (i.e., L_NAD_ADJ_Flags = .FALSE.) 
C     issue warning message, but only issue warning once.
      IF( .NOT. L_OldMetVer .AND. .NOT. SCREEN .AND.
     &         IMETMSG.EQ.0 .AND. .NOT. L_NAD_ADJ_Flags )THEN
C ---    Set L_OldMetVer = .T.
         L_OldMetVer = .TRUE.
         CALL ERRHDL(PATH,MODNAM,'W','394','No NAD/ADJ')
         IMETMSG = IMETMSG + 1     
      ENDIF

C     Set the stability logical variables, which are needed in COMPTG
      IF( OBULEN .GT. 0.0D0 ) THEN
         UNSTAB = .FALSE.
         STABLE = .TRUE.
      ELSE
         UNSTAB = .TRUE.
         STABLE = .FALSE.
      ENDIF

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

C --- Initialize logical variable to track for turbulence data
      L_TurbData = .FALSE.

C --- First loop through PROFFILE to determine if turbulence data
C     are present
      DO WHILE( JFLAG .EQ. 0 )
         READ( MPUNIT, *, END=1000, ERR=98, IOSTAT=IOERRN ) KYEAR,
     &       KMONTH, KDAY, KHOUR, PFLHT(LEVEL), JFLAG,
     &       PFLWD(LEVEL), PFLWS(LEVEL), PFLTA(LEVEL),
     &       PFLSA(LEVEL), PFLSW(LEVEL)

C        Convert the data to the required units
         CALL PFLCNV (LEVEL)

C ---    Check for observed turbulence parameters in PROFFILE file
         IF( (PFLSA(LEVEL).GT.0.0D0 .AND. PFLSA(LEVEL).LT.99.0D0) .OR.
     &       (PFLSW(LEVEL).GT.0.0D0 .AND. PFLSW(LEVEL).LT.99.0D0) )THEN
            L_TurbData = .TRUE.
         ENDIF

C        Set the number of profile levels to current index, store
C        the 'top of profile' flag, and increment level if not at top
C        Check that the level does not exceed the maximum allowable
         NPLVLS = LEVEL
         IFLAG(LEVEL) = JFLAG

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

C ---    Check for observed turbulence parameters in PROFFILE file
         IF( PFLSA(LEVEL).GT.0.0D0 .AND. PFLSA(LEVEL).LT.99.0D0 )THEN
            L_TurbData = .TRUE.
            L_Got_SigA = .TRUE.
         ENDIF

         IF( PFLSW(LEVEL).GT.0.0D0 .AND. PFLSW(LEVEL).LT.99.0D0 )THEN
            L_TurbData = .TRUE.
            L_Got_SigW = .TRUE.
         ENDIF

      ENDDO

C --- Need to check for TURB with DFAULT
c --- and adjusted U*, IF both turbulence and
c --- adjusted u* present, error !JAT 10/24/16
C!! --- We should always include turbulence data in AERMOD header
      IF( DFAULT .AND. L_TurbData .AND. L_AdjUstar )THEN
         IF( L_Got_SigA .AND. L_Got_SigW )THEN
            DUMMY = 'SigA & SigW'
         ELSEIF( L_Got_SigA)THEN
            DUMMY = 'SigA'
         ELSEIF( L_Got_SigW )THEN
            DUMMY = 'SigW'
         ENDIF
         CALL ERRHDL(PATH,MODNAM,'E','401',DUMMY)
         RUNERR = .TRUE.
      ENDIF
C --- Always include turbulence data in AERMOD header      ! RWB
      IF( L_TurbData )THEN
         IF( L_Got_SigA .AND. L_Got_SigW )THEN
            DUMMY = 'SigA & SigW'
         ELSEIF( L_Got_SigA )THEN
            DUMMY = 'SigA'
         ELSEIF( L_Got_SigW )THEN
            DUMMY = 'SigW'
         ENDIF
C ---    Issue error message if TurbData is used with ADJ_U*
         IF( L_AdjUstar .AND. DFAULT )THEN
            CALL ERRHDL(PATH,MODNAM,'E','401',DUMMY)
            RUNERR = .TRUE.
         ENDIF
      ENDIF

C     Compute the vertical potential temperature gradient profile
      IF( .NOT. RUNERR ) THEN
         NTGLVL = 0
         CALL COMPTG ()
      ENDIF

      IF (ILINE .EQ. 1) THEN
C        Write Out Sample of the Meteorology Data
C        (Up to the First 24 Hours)                         ---   CALL METDAT
         JDAY = IJDAY      ! Assign IJDAY to global variable JDAY
         IF( .NOT. L_SkipMessages ) CALL METDAT(IOUNIT)
         IF (SUMMFILE) THEN
C ---       Include sample of meteorological data in SUMMFILE
            IF( .NOT. L_SkipMessages ) CALL METDAT(ISUMUNT)
         END IF
      END IF

C     Set Meteorological Variables for Current Hour
      CALL SET_METDATA

      IF (ILINE .EQ. 1) THEN
C ---    First hour of met data file; check for IHOUR .ne. 01 with short-term averages
         IF (IHOUR .GT. 1 .AND. (PM25AVE .OR. NO2AVE .OR. SO2AVE)) THEN
            IF (L_MAXDCONT) THEN
C              Write Error Message: MAXDCONT option requires data file to begin with hour 1
               WRITE(DUMMY,'(''First Hr= '',I2.2)') IHOUR
               CALL ERRHDL(PATH,MODNAM,'E','491',DUMMY)
C              Assign RUNERR logical to .T. 
               RUNERR = .TRUE.
            ELSE IF (NO2AVE .OR. SO2AVE) THEN
C              Write Warning Message: 1hr NO2 & SO2 modeling should begin with hour 1
               WRITE(DUMMY,'(''First Hr= '',I2.2)') IHOUR
               CALL ERRHDL(PATH,MODNAM,'W','488',DUMMY)
            ELSE IF (PM25AVE) THEN
C              Write Warning Message: Short-term averages for first calendar day may not be valid
               CALL ERRHDL(PATH,MODNAM,'W','488','for 1st Day')
            END IF
         ELSE IF (IHOUR .GT. 1 .AND. (NUMAVE.GT.1 .OR. 
     &                               (NUMAVE.EQ.1 .AND. 
     &                                             KAVE(1).NE.1)) ) THEN
C           Write Warning Message: Short-term averages for first calendar day may not be valid
            CALL ERRHDL(PATH,MODNAM,'W','488','for 1st Day')
         END IF

C ---    Check for start year based on met data file matching start year based on 
C        ME SURFDATA keyword; note that IYR based on met data and ISYEAR based on 
C        ME SURFDATA should both be 4-digits (a warning message will be issued if
C        ME SURFDATA input is not 4-digits)

         IF (IYR .NE. ISYEAR .AND. IMSTAT(7).EQ.0) THEN
C ---       Issue warning message that year specified on SURFDATA does not match first year
C           of data file; if DAYRANGE keyword not used (IMSTAT(7)=0), adjust ISYEAR to match 
C           data file (IYR))
            WRITE(DUMMY,'(''StartYR '',I4)') IYR
            CALL ERRHDL(PATH,MODNAM,'W','492',DUMMY)
            ISYEAR = IYR

         ELSE IF (IYR .NE. ISYEAR .AND. IMSTAT(7).GT.0) THEN
C ---       Issue ERROR message that year specified on SURFDATA does not match first year
C           of data file when DAYRANGE keyword is being used (IMSTAT(7)>0).
            WRITE(DUMMY,'(''StartYR '',I4)') IYR
            CALL ERRHDL(PATH,MODNAM,'E','493',DUMMY)
            ISYEAR = IYR

            RUNERR = .TRUE.
            
         END IF

         IF (PM25AVE .OR. NO2AVE .OR. SO2AVE .OR. 
     &                                ANNUAL .OR. MULTYR) THEN
C ---       If PM-2.5 averaging, NO2 1-hour averaging, ANNUAL average, or MULTYEAR, 
C           then need to set the variables for the "end-of-year" check if the STARTEND 
C           keyword is not used
            IF (IMSTAT(6) .EQ. 0) THEN
C ---          Determine MN, DY, and HR for end-of-the-year check.
C              Subtract one from start hour to set end hour for the year of data;
C              adjust start day and month if needed.
               ISYR = IYEAR
               ISHR = IHOUR
               ISDY = IDAY
               ISMN = IMONTH
C              Convert ISYR to Four Digits
               IF (ISYR .GE. ISTRT_WIND .AND. ISYR .LE. 99) THEN
                  ISYR = ISTRT_CENT*100 + ISYR
               ELSE IF (ISYR .LT. ISTRT_WIND) THEN
                  ISYR = (ISTRT_CENT+1)*100 + ISYR
               END IF
               
               CALL JULIAN (ISYR,ISMN,ISDY,ISJDAY)
               
               IF (IHOUR .GT. 1) THEN
                  IENDHOUR = IHOUR - 1
                  IENDDY   = IDAY
                  IENDMN   = IMONTH
               ELSE
                  IENDHOUR = 24
                  IF (IDAY .GT. 1) THEN
                     IENDDY = IDAY - 1
                     IENDMN = IMONTH
                  ELSE
                     IENDMN = IMONTH - 1
                     IF (IENDMN .EQ. 0) IENDMN = 12
                     IENDDY = IDYMAX(IENDMN)
                  END IF
               END IF
C ---          Determine ISDATE based on first hour of data file,
C              unless this is a restarted run
               IF (.NOT.RSTINP) ISDATE = FULLDATE
            END IF
         END IF
         
C ---    Check for potential conflicts between "start dates" and
C        first date of met data file
         IF (.NOT.MULTYR .AND. IMSTAT(6) .EQ. 1 .AND. 
     &                         FULLDATE .GT. ISDATE) THEN
C ---       Write Error Message:  Met data file starts later than 
C           user-specified start date (ISDATE)
            WRITE(DUMMY,'(I10.10)') FULLDATE
            CALL ERRHDL(PATH,MODNAM,'E','483',DUMMY)
            RUNERR = .TRUE.
         ELSE IF (.NOT.MULTYR .AND. RSTINP .AND.
     &                         FULLDATE .GT. ISDATE) THEN
C ---       Write Error Message:  Met data file starts later than 
C           start date (ISDATE) for Re-started model run
            WRITE(DUMMY,'(I10.10)') ISDATE-(ISDATE/100000000)*100000000
            CALL ERRHDL(PATH,MODNAM,'E','484',DUMMY)
            RUNERR = .TRUE.
         ELSE IF (MULTYR .AND. RSTINP .AND.
     &                         FULLDATE .GT. ISDATE) THEN
C ---       Write Warning Message:  Met data file starts later than 
C           start date (ISDATE) for restarted MULTYEAR run, indicating
C           gap between years of met data
            WRITE(DUMMY,'(I10.10)') ISDATE-(ISDATE/100000000)*100000000
            CALL ERRHDL(PATH,MODNAM,'W','485',DUMMY)
         ELSE IF (MULTYR .AND. RSTINP .AND. IMSTAT(6).EQ.0 .AND.
     &                         FULLDATE .LT. ISDATE) THEN
C ---       Write Error Message:  Met data file starts earlier than 
C           start date (ISDATE) for restarted MULTYEAR run, without the
C           STARTEND keyword, indicating a date overlap between years
C           of met data
            WRITE(DUMMY,'(I10.10)') ISDATE-(ISDATE/100000000)*100000000
            CALL ERRHDL(PATH,MODNAM,'E','487',DUMMY)
            RUNERR = .TRUE.
         END IF
         
      END IF

C --- Increment index for hour-of-year and year to save met data for 
C     MAXDCONT option
      IF (FULLDATE .GE. ISDATE .AND. L_MAXDCONT) THEN
         IHR_NDX = IHR_NDX + 1
         IYR_NDX = NUMYRS + 1
      
         IF (IYR_NDX .GT. NYEARS) THEN
C ---       Year index exceeds maximum array limit, set
C           by NYEARS parameter in modules.f
C ---       Write Error Message        ! Too many years
            WRITE(DUMMY,'(''NYEARS='',I4)') NYEARS
            CALL ERRHDL(PATH,MODNAM,'E','482',DUMMY)
            RUNERR = .TRUE.
            GO TO 999
         END IF
            
C ---    Store hourly met data to arrays for MAXDCONT option 
         IF (.NOT.RSTINP .AND. L_MAXDCONT) THEN
            ASFCHF(IHR_NDX,IYR_NDX)  =  SFCHF 
            AUREF(IHR_NDX,IYR_NDX)   =  UREF  
            AUREFHT(IHR_NDX,IYR_NDX) =  UREFHT
            ATA(IHR_NDX,IYR_NDX)     =  TA    
            ATREFHT(IHR_NDX,IYR_NDX) =  TREFHT
            AWDREF(IHR_NDX,IYR_NDX)  =  WDREF 
            AUSTAR(IHR_NDX,IYR_NDX)  =  USTAR 
            AWSTAR(IHR_NDX,IYR_NDX)  =  WSTAR 
            AZICONV(IHR_NDX,IYR_NDX) =  ZICONV
            AZIMECH(IHR_NDX,IYR_NDX) =  ZIMECH
            AOBULEN(IHR_NDX,IYR_NDX) =  OBULEN
            AVPTGZI(IHR_NDX,IYR_NDX) =  VPTGZI
            ASFCZ0(IHR_NDX,IYR_NDX)  =  SFCZ0 
            AKST(IHR_NDX,IYR_NDX)    =  KST
            IF (LDGAS .OR. LDPART .OR. LWPART .OR. LWGAS .OR. 
     &          GRSM)THEN
               ABOWEN(IHR_NDX,IYR_NDX)  =  BOWEN 
               AALBEDO(IHR_NDX,IYR_NDX) =  ALBEDO
               IAPCODE(IHR_NDX,IYR_NDX) =  IPCODE
               APRATE(IHR_NDX,IYR_NDX)  =  PRATE 
               ARH(IHR_NDX,IYR_NDX)     =  RH    
               ASFCP(IHR_NDX,IYR_NDX)   =  SFCP  
               NACLOUD(IHR_NDX,IYR_NDX) =  NCLOUD
               AQSW(IHR_NDX,IYR_NDX)    =  QSW   
               AWnew(IHR_NDX,IYR_NDX)   =  Wnew  
               Af2(IHR_NDX,IYR_NDX)     =  f2    
               AEsTa(IHR_NDX,IYR_NDX)   =  EsTa  
               APrec1(IHR_NDX,IYR_NDX)  =  Prec1 
               APrec2(IHR_NDX,IYR_NDX)  =  Prec2 
            END IF 
            IF (L_BLSOURCE) THEN
               ABLTA(IHR_NDX,IYR_NDX)  = TA
            END IF
            ARURUSTR(IHR_NDX,IYR_NDX)   = RURUSTR
            ARUROBULEN(IHR_NDX,IYR_NDX) = RUROBULEN

            ACLMHR(IHR_NDX,IYR_NDX)   =  CLMHR
            AMSGHR(IHR_NDX,IYR_NDX)   =  MSGHR
            AUNSTAB(IHR_NDX,IYR_NDX)  =  UNSTAB
            ASTABLE(IHR_NDX,IYR_NDX)  =  STABLE
            AURBSTAB(IHR_NDX,IYR_NDX) =  URBSTAB
                          
            ANDX4ZI(IHR_NDX,IYR_NDX) = NDX4ZI
            AUATZI(IHR_NDX,IYR_NDX)  = UATZI
            ASVATZI(IHR_NDX,IYR_NDX) = SVATZI
            ASWATZI(IHR_NDX,IYR_NDX) = SWATZI
            AUAVG(IHR_NDX,IYR_NDX)   = UAVG
            ASVAVG(IHR_NDX,IYR_NDX)  = SVAVG
            ASWAVG(IHR_NDX,IYR_NDX)  = SWAVG
            APTATZI(IHR_NDX,IYR_NDX) = PTATZI
     
            AGRIDWD(IHR_NDX,1:MXGLVL,IYR_NDX) = GRIDWD(1:MXGLVL) 
            AGRIDWS(IHR_NDX,1:MXGLVL,IYR_NDX) = GRIDWS(1:MXGLVL) 
            AGRIDSW(IHR_NDX,1:MXGLVL,IYR_NDX) = GRIDSW(1:MXGLVL) 
            AGRIDSV(IHR_NDX,1:MXGLVL,IYR_NDX) = GRIDSV(1:MXGLVL) 
            AGRIDTG(IHR_NDX,1:MXGLVL,IYR_NDX) = GRIDTG(1:MXGLVL) 
            AGRIDPT(IHR_NDX,1:MXGLVL,IYR_NDX) = GRIDPT(1:MXGLVL) 
            IF (NSEC .GT. 0) THEN
               AGRIDRHO(IHR_NDX,1:MXGLVL,IYR_NDX) = GRIDRHO(1:MXGLVL)
            END IF
            IF (PVMRM .OR. GRSM) THEN
               AGRIDEPS(IHR_NDX,1:MXGLVL,IYR_NDX) = GRIDEPS(1:MXGLVL)
            END IF
            IF (NURB .GT. 0) THEN
               AGRDSWR(IHR_NDX,1:MXGLVL,IYR_NDX) = GRDSWR(1:MXGLVL)  
               AGRDSVR(IHR_NDX,1:MXGLVL,IYR_NDX) = GRDSVR(1:MXGLVL) 
               AGRDTGR(IHR_NDX,1:MXGLVL,IYR_NDX) = GRDTGR(1:MXGLVL) 
               AGRDPTR(IHR_NDX,1:MXGLVL,IYR_NDX) = GRDPTR(1:MXGLVL) 
            
               DO I = 1, NURB 
                  AGRDSWU(IHR_NDX,1:MXGLVL,IYR_NDX,I)=GRDSWU(1:MXGLVL,I)
                  AGRDSVU(IHR_NDX,1:MXGLVL,IYR_NDX,I)=GRDSVU(1:MXGLVL,I)
                  AGRDTGU(IHR_NDX,1:MXGLVL,IYR_NDX,I)=GRDTGU(1:MXGLVL,I)
                  AGRDPTU(IHR_NDX,1:MXGLVL,IYR_NDX,I)=GRDPTU(1:MXGLVL,I)
CRWB              Add variables for URBAN option
                  AZIURB(IHR_NDX,IYR_NDX,I)     = ZIURB(I)
                  AURBWSTR(IHR_NDX,IYR_NDX,I)   = URBWSTR(I)
                  AURBUSTR(IHR_NDX,IYR_NDX,I)   = URBUSTR(I)
                  AURBOBULEN(IHR_NDX,IYR_NDX,I) = URBOBULEN(I)
CRWB              Also include L_MorningTrans array for urban morning transition
                  AL_MorningTrans(IHR_NDX,IYR_NDX,I) = L_MorningTrans(I)
               END DO
            END IF
            
         END IF
                
      END IF

      GO TO 999

C---- End-of-file and error handling for METEXT
C
C     WRITE Error Messages:  Error Reading Met Data File

 98   CALL ERRHDL(PATH,MODNAM,'E','510','PROFFILE')
      RUNERR = .TRUE.
      GO TO 999

 99   CALL ERRHDL(PATH,MODNAM,'E','510','SURFFILE')
      RUNERR = .TRUE.
      GO TO 999

 1000 EOF = .TRUE.
 
C --- Check for EOF on first data record, ILINE=1
      IF (ILINE .EQ. 1) THEN
C        Write Error Message for EOF on first data record
         CALL ERRHDL(PATH,MODNAM,'E','580',DUMMY)
         RUNERR = .TRUE.
      END IF

 999  RETURN
      END

      SUBROUTINE SET_METDATA
C***********************************************************************
C                 SET_METDATA Module of AERMOD Model
C
C        PURPOSE: Sets the meteorological data variables for current hour
C
C        PROGRAMMER: ROGER BRODE
C
C        DATE:    May 12, 1999
C
C        MODIFIED:   To include IPROCL array for identifying which 
C                    Julian days to process for Leap Years (based on
C                    the year specified on the ME SURFDATA keyword)
C                    R. W. Brode, U.S. EPA/OAQPS/AQMG, 06/30/2015
C
C                    To include call to GRDEPS, for calculation of
C                    gridded turbulence dissipation rate for use in the
C                    PVMRM algorithm.
C                    R. W. Brode, MACTEC (f/k/a PES), Inc., 07/27/04
C
C                    To include determination of the day-of-week index
C                    (1 for Weekday [M-F], 2 for Saturday, 3 for Sunday)
C                    for use in the option to vary emissions by season,
C                    hour-of-day, and day-of-week (SHRDOW).
C                    R.W. Brode, PES, Inc., 4/10/2000
C
C        INPUTS:  Meteorological Variables for One Hour
C
C        OUTPUTS: Meteorological Data Error and Status Switches
C
C        CALLED FROM:   METEXT
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      USE BUOYANT_LINE

      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Declare Arrays for Use With Day/Date Calcs
C JAT 06/22/21 D065
C REMOVE ISEA_NDX AS UNUSED VARIABLE
      INTEGER :: NDAY(12) !, ISEA_NDX(12)
      INTEGER :: I, NL, NUMSW
      DOUBLE PRECISION :: SUMSW, FVREF
C Unused:       INTEGER ::  IA, IY, IM, ID,

C     Variable Initializations
      DATA NDAY/31,59,90,120,151,181,212,243,273,304,334,365/
C JAT 06/22/21 D065
C REMOVE ISEA_NDX INITITALIZATION AS UNUSED VARIABLE
C      DATA ISEA_NDX/1,1,2,2,2,3,3,3,4,4,4,1/

      MODNAM = 'SET_METDATA'

C---- Set lower limit of 1.0 meter for mixing heights
      IF (ZICONV.GE.0.0D0 .AND. ZICONV.LT.1.0D0) ZICONV = 1.0D0
      IF (ZIMECH.GE.0.0D0 .AND. ZIMECH.LT.1.0D0) ZIMECH = 1.0D0

C     Set the date variables for this hour
      CALL SET_DATES

      IF (MONTH .AND. IHOUR .EQ. 24) THEN
C        Check for the End of the Month
         IF (IMONTH .EQ. 1 .OR. (MOD(IYR,4) .NE. 0) .OR.
     &      (MOD(IYR,100) .EQ. 0 .AND. MOD(IYR,400) .NE. 0)) THEN
C           Not a Leap Year OR Month = January
            IF (JDAY .EQ. NDAY(IMONTH)) THEN
               ENDMON = .TRUE.
            END IF
         ELSE
C           Leap Year AND Month > January
            IF (JDAY .EQ. NDAY(IMONTH)+1) THEN
               ENDMON = .TRUE.
            END IF
         END IF
      END IF

C     Check Data for Calms, Missing, Out-of-Range Values    ---   CALL METCHK
      IF (.NOT. L_SkipMessages) CALL METCHK

C     Limit ZI to 4000 meters.
      IF (ZICONV .GT. 4000.D0) ZICONV = 4000.D0
      IF (ZIMECH .GT. 4000.D0) ZIMECH = 4000.D0
C     Select appropriate mixing height from convective and mechanical values
      IF (.NOT.MSGHR .AND. .NOT.CLMHR .AND. OBULEN.LT.0.0D0) THEN
         ZI = MAX ( ZICONV, ZIMECH )
      ELSE IF (.NOT.MSGHR .AND. .NOT.CLMHR) THEN
         ZI = ZIMECH
      ELSE
         ZI = -999.0D0
      END IF
C---- Set lower limit of 1.0 meter for mixing height
      IF (ZI.GE.0.0D0 .AND. ZI.LT.1.0D0) ZI = 1.0D0

C --- Assign ZI to ZIRUR for URBAN option
      IF (URBAN) ZIRUR = ZI

C     Apply ROTANG Adjustment to Wind Direction
      IF (DABS(ROTANG) .GT. 0.0000001D0) THEN
         WDREF = WDREF - ROTANG
         IF (WDREF .LE. 0.0D0) THEN
            WDREF = WDREF + 360.0D0
         END IF
         DO NL = 1, NPLVLS
            IF( PFLWD(NL) .GT. 0.0D0 )THEN
               PFLWD(NL) = PFLWD(NL) - ROTANG

               IF( PFLWD(NL) .LE. 0.0D0 )THEN
                  PFLWD(NL) = PFLWD(NL) + 360.0D0
               ENDIF

            ENDIF
         END DO
      END IF

C---- Save the ambient temperature from the 'surface' file to a separate 
C      variable for the bouyant line algorithms
      IF (L_BLSOURCE) THEN
         BLTA = TA
      ENDIF 

C---- Initialize urban stable flag to false.
      IF(.NOT.L_SkipMessages) URBSTAB = .FALSE.

C
C---- Check the RUNERR flag - if it is FALSE, then there is sufficient
C     data to continue processing the data.
C     Also skip this IF-THEN block if L_SkipMessages is TRUE, which 
C     indicates that this call is during the MAXDCONT "post-processing"
C     stage since the gridded profiles and other data have been retrieved
C     from arrays.
      IF( .NOT. RUNERR .AND. .NOT.L_SkipMessages )THEN
C
         IF( .NOT. CLMHR  .AND.  .NOT. MSGHR )THEN
C           Set the stability logical variables
            IF( OBULEN .GT. 0.0D0 )THEN
               UNSTAB = .FALSE.
               STABLE = .TRUE.
            ELSE
               UNSTAB = .TRUE.
               STABLE = .FALSE.
            ENDIF
           

            IF (FULLDATE.GE.ISDATE .AND. 
     &                ( (L_LeapYear .AND. IPROCL(JDAY).EQ.1) .OR.
     &             (.NOT.L_LeapYear .AND. IPROC( JDAY).EQ.1)) ) THEN
C
C              Initialize the gridded profile arrays
               GRIDSV = -99.0D0
               GRIDSW = -99.0D0
               GRIDWS = -99.0D0
               GRIDWD = -99.0D0
               GRIDTG = -99.0D0
               GRIDPT = -99.0D0
               IF (URBAN) THEN
                  GRDSVR = -99.0D0
                  GRDSVU = -99.0D0
                  GRDSWR = -99.0D0
                  GRDSWU = -99.0D0
                  GRDTGR = -99.0D0
                  GRDTGU = -99.0D0
                  GRDPTR = -99.0D0
                  GRDPTU = -99.0D0
               END IF

C              Compute gridded profile of epsilon for PVMRM or GRSM option
               IF (PVMRM .OR. GRSM) THEN
                  GRIDEPS = -99.0D0
               END IF

C              Get the index from the array of gridded heights that
C              corresponds to the height immediately below ZI
               CALL LOCATE( GRIDHT, 1, MXGLVL, ZI, NDX4ZI )

C              Compute THETA_STAR and DTHDZ for the gridded
C              potential temperature gradient

               CALL TGINIT ()
C
C              Profile all variables here except sv and sw; defer sv
C              and sw until u at zi is known.
C
               CALL GRDWS ()
               CALL GRDWD ()
               CALL GRDPTG()
               CALL GRDPT ()

C----------    Compute density profile for PRIME
               CALL GRDDEN

C----------    Compute the parameter values at ZI; if ZI is above the
C              highest gridded profile level, use the value at the high-
C              est level
               IF( NDX4ZI .LT. MXGLVL )THEN
                  CALL GINTRP( GRIDHT(NDX4ZI), GRIDWS(NDX4ZI),
     &                         GRIDHT(NDX4ZI+1), GRIDWS(NDX4ZI+1),
     &                         ZI, UATZI )
                  CALL GINTRP( GRIDHT(NDX4ZI), GRIDPT(NDX4ZI),
     &                         GRIDHT(NDX4ZI+1), GRIDPT(NDX4ZI+1),
     &                         ZI, PTATZI )

               ELSE
                  UATZI  = GRIDWS(MXGLVL)
                  PTATZI = GRIDPT(MXGLVL)

               ENDIF
C
C              Add turbulence variables here
C
               CALL GRDSV ()

C              Obtain residual turbulence value before calling GRDSW
               NUMSW = 0
               SUMSW = 0.0D0

               DO I = 1, NPLVLS
                  IF (PFLHT(I).GE.ZI .AND. PFLSW(I).GE.0.0D0) THEN
                     NUMSW = NUMSW + 1
                     SUMSW = SUMSW + PFLSW(I)
                  END IF
               END DO

               IF (NUMSW .GT. 0) THEN
                  SWRMAX = SUMSW / DBLE(NUMSW)
               ELSE
                  SWRMAX = 0.02D0 * UATZI
               END IF

               CALL GRDSW ()

               IF( NDX4ZI .LT. MXGLVL )THEN
                  CALL GINTRP( GRIDHT(NDX4ZI), GRIDSV(NDX4ZI),
     &                         GRIDHT(NDX4ZI+1), GRIDSV(NDX4ZI+1),
     &                         ZI, SVATZI )
                  CALL GINTRP( GRIDHT(NDX4ZI), GRIDSW(NDX4ZI),
     &                         GRIDHT(NDX4ZI+1), GRIDSW(NDX4ZI+1),
     &                         ZI, SWATZI )
               ELSE
                  SVATZI = GRIDSV(MXGLVL)
                  SWATZI = GRIDSW(MXGLVL)
               END IF

C---           Compute the overbar (average) quantities for sigma_V, sigma_W,
C              and wind speed, from the surface up to ZI (formerly done in METINI)
               CALL ZIAVER (MXGLVL,GRIDHT,GRIDSV,ZI,NDX4ZI,SVAVG,SVATZI)
               CALL ZIAVER (MXGLVL,GRIDHT,GRIDSW,ZI,NDX4ZI,SWAVG,SWATZI)
               CALL ZIAVER (MXGLVL,GRIDHT,GRIDWS,ZI,NDX4ZI,UAVG,UATZI)

C              Compute gridded profile of epsilon for PVMRM/GRSM option
               IF (PVMRM .OR. GRSM) THEN
                  CALL GRDEPS
               END IF

C              Compute Urban Profiles if Needed
               IF (URBAN) THEN
                  CALL URBCALC
                  CALL GRDURBAN
               END IF

         END IF
C JAT D070 WRITE MESSAGE THAT SIGMA-THETA OR SIGMA-W CHANGED DO NOT DO
C FOR NOTURB, NOSA, AND NOSW (UNDERSTOOD ALL HOURS ARE RESET) 
            WRITE(DUMMY,'(I10.10)') FULLDATE
            IF (RESET_SA .AND. (TURBOPTS(2) .OR. TURBOPTS(3) .OR.  
     &        TURBOPTS(6) .OR. TURBOPTS(8))) 
     &        CALL ERRHDL(PATH,MODNAM,'I','445',DUMMY)
            IF (RESET_SW .AND. (TURBOPTS(2) .OR. TURBOPTS(3) .OR.  
     &        TURBOPTS(7) .OR. TURBOPTS(9))) 
     &        CALL ERRHDL(PATH,MODNAM,'I','446',DUMMY)
         ENDIF

      END IF

C --- Assign sector ID for direction-varying background O3 and NOx
C     based on flow vector from surface file, FVREF;
C     first check for valid wind direction
      IF (WDREF .LE. 0.0D0 .OR. WDREF .GT. 360.0D0) THEN
C ---    Hour is calm or missing; set IO3SECT = 0, INOXSECT = 0
         IO3SECT = 0
         INOXSECT = 0
      ELSE
C ---    Valid wind direction is available
         FVREF = WDREF + 180.0D0
         IF (FVREF .GT. 360.0D0) THEN
            FVREF = FVREF - 360.0D0
         END IF
         IF (L_O3Sector) THEN
            IF (FVREF .LT. O3SECT(1) .OR. 
     &          FVREF .GE. O3SECT(NUMO3Sects) ) THEN
               IO3SECT = NUMO3Sects
            ELSE
               DO I = 1, NUMO3Sects-1
                  IF (FVREF .GE. O3SECT(I) .AND. 
     &                FVREF .LT. O3SECT(I+1)) THEN
                     IO3SECT = I
                     EXIT
                  END IF
               END DO
            END IF
         ELSE
            IO3SECT = 1
         END IF
C        CERC 11/30/20
         IF (L_NOxSector) THEN
            IF (FVREF .LT. NOxSECT(1) .OR. 
     &          FVREF .GE. NOxSECT(NUMNOxSects) ) THEN
               INOxSECT = NUMNOxSects
            ELSE
               DO I = 1, NUMNOxSects-1
                  IF (FVREF .GE. NOXSECT(I) .AND. 
     &                FVREF .LT. NOXSECT(I+1)) THEN
                     INOXSECT = I
                     EXIT
                  END IF
               END DO
            END IF
         ELSE
            INOXSECT = 1
         END IF
      END IF

C --- Assign sector ID for direction-varying background
C     based on flow vector from surface file, FVREF;
C     first check for valid wind direction
      IF (WDREF .LE. 0.0D0 .OR. WDREF .GT. 360.0D0) THEN
C ---    Hour is calm or missing; set IBGSECT = 0
         IBGSECT = 0
      ELSE
C ---    Valid wind direction is available
         FVREF = WDREF + 180.0D0
         IF (FVREF .GT. 360.0D0) THEN
            FVREF = FVREF - 360.0D0
         END IF
         IF (L_BGSector) THEN
            IF (FVREF .LT. BGSECT(1) .OR. 
     &          FVREF .GE. BGSECT(NUMBGSects) ) THEN
               IBGSECT = NUMBGSects
            ELSE
               DO I = 1, NUMBGSects-1
                  IF (FVREF .GE. BGSECT(I) .AND. 
     &                FVREF .LT. BGSECT(I+1)) THEN
                     IBGSECT = I
                     EXIT
                  END IF
               END DO
            END IF
         ELSE
            IBGSECT = 1
         END IF
      END IF

C     Set Appropriate Wind Speed Category Index
      IF (UREF .LE. UCAT(1)) THEN
         IUCAT = 1
      ELSE IF (UREF .LE. UCAT(2)) THEN
         IUCAT = 2
      ELSE IF (UREF .LE. UCAT(3)) THEN
         IUCAT = 3
      ELSE IF (UREF .LE. UCAT(4)) THEN
         IUCAT = 4
      ELSE IF (UREF .LE. UCAT(5)) THEN
         IUCAT = 5
      ELSE
         IUCAT = 6
      END IF

      IF (FASTAREA .OR. L_BLSOURCE) THEN
C        Set Stability Category based on Golder (1972) for use with
C        FASTAREA Area Source Optimizations (formerly the TOXICS option)
         CALL LTOPG( KST )
      ELSE
C ---    Assign D stability as default (KST=4)
         KST = 4
      END IF

      IF (MSGHR .AND. .NOT. L_SkipMessages) THEN
         IF (.NOT. MSGPRO) THEN
C           Set Flag for Runtime Met. Error to Prevent Further Calculations
            RUNERR = .TRUE.
C           WRITE Error Message:  Missing Meteorological Data
            WRITE(DUMMY,'(I10.10)') FULLDATE
            CALL ERRHDL(PATH,MODNAM,'E','460',DUMMY)
         ELSE IF (.NOT. L_SkipMessages) THEN
C           WRITE Informational Message:  Missing Meteorological Data
            WRITE(DUMMY,'(I10.10)') FULLDATE
            CALL ERRHDL(PATH,MODNAM,'I','460',DUMMY)
         END IF
      END IF

      RETURN
      END

      SUBROUTINE SET_DATES
C***********************************************************************
C                 SET_DATES Module of AERMOD Model
C
C        PURPOSE: Sets the date variables for current hour
C
C        PROGRAMMER: ROGER BRODE
C
C        DATE:    May 12, 1999
C
C        INPUTS:  Meteorological Variables for One Hour
C
C        OUTPUTS: Meteorological Data Error and Status Switches
C
C        CALLED FROM:   SET_METDATA
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE

C JAT 06/22/21 D065
C REMOVE NDAY AS UNUSED VARIABLE
c      INTEGER :: NDAY(12), ISEA_NDX(12)
      INTEGER :: ISEA_NDX(12)
      INTEGER :: IA, IY, IM, ID
      CHARACTER MODNAM*12
C Unused:       INTEGER :: I, NL

C     Variable Initializations
C JAT 06/22/21 D065
C REMOVE NDAY INITIALIZATION AS UNUSED VARIABLE
C      DATA NDAY/31,59,90,120,151,181,212,243,273,304,334,365/
      DATA ISEA_NDX/1,1,2,2,2,3,3,3,4,4,4,1/

      MODNAM = 'SET_DATES'

      IF (.NOT. L_SkipMessages) THEN
C ---    This call is not part of MAXDCONT processing,
C        otherwise date calculations are skipped.

C        Determine The Current Julian Day and Calculate Current Gregorian Date
C        First Convert Year to 4-Digit Value
         IF (IYEAR .GE. ISTRT_WIND .AND. IYEAR .LE. 99) THEN
            IYR = ISTRT_CENT*100 + IYEAR
         ELSE IF (IYEAR .LT. ISTRT_WIND) THEN
            IYR = (ISTRT_CENT+1)*100 + IYEAR
         ELSE
C           Input IYEAR must be 4-digit:  Save to IYR and convert to 2-digit
            IYR   = IYEAR
            IYEAR = IYR - 100 * (IYR/100)
         END IF

C ---    Assign L_LeapYear variable
         IF ((MOD(IYR,4) .NE. 0) .OR.
     &       (MOD(IYR,100) .EQ. 0 .AND. MOD(IYR,400) .NE. 0)) THEN
C           Not a Leap Year
            L_LeapYear = .FALSE.
         ELSE
C           Leap Year
            L_LeapYear = .TRUE.
         END IF

C ---    Save previous Julian Day value
         IF (JDAY .GE. 1) THEN
            JDAY_PREV = JDAY
         ELSE
            JDAY_PREV = 0
         END IF
         
C        Determine Julian Day (Day of Year) Number, JDAY    ---   CALL JULIAN
         CALL JULIAN(IYR,IMONTH,IDAY,JDAY)
     
C        Calculate 8-digit Integer Variable for Current Date/Hour, KURDAT
C        and 10-digit Integer Variable (with 4-digit year), FULLDATE;
C        IYEAR = 2-digit year and IYR = 4-digit year
         KURDAT = IYEAR*1000000 + IMONTH*10000 + IDAY*100 + IHOUR
         IF (IYR .GE. 2148) THEN
C           Write Error Message:  Input Year is > 2147.
            WRITE(DUMMY,'("YR= ",I4)') IYR
            CALL ERRHDL(PATH,MODNAM,'E','365',DUMMY)
            RUNERR = .TRUE.
            FULLDATE = 2147123124
         ELSE
            FULLDATE = IYR*1000000 + IMONTH*10000 + IDAY*100 + IHOUR
         END IF
     
C        Check for 4-digit year input for profile data
         IF (KYEAR .GE. 100) THEN
            KYEAR = KYEAR - 100 * (KYEAR/100)
         END IF
         KURPFL = KYEAR*1000000 + KMONTH*10000 + KDAY*100 + KHOUR
     
      END IF

C     Determine SEASON index
      ISEAS = ISEA_NDX(IMONTH)

      IF (L_DayOfWeekOpts) THEN
C ---    An option requiring day-of-week is being used (EMISFACT/O3VALUES/BACKGRND)
C        Determine Day of Week (1 = Weekday [M-F], 2 = Saturday, 3 = Sunday).
C        Based on "Frequently Asked Questions about Calendars," Version 2.2,
C        by Claus Tondering, April 9, 2000, available on the web at URL
C        http://www.tondering.dk/claus/calendar.html
         IA = (14-IMONTH)/12
         IY = IYR - IA
         IM = IMONTH + 12*IA - 2
         ID = MOD( (IDAY + IY + IY/4 - IY/100 + IY/400 + (31*IM)/12), 7)
         IF (ID .GE. 1 .AND. ID .LE. 5) THEN
C           This is a weekday
            IDAY_OF_WEEK = 1
         ELSE IF (ID .EQ. 6) THEN
C           This is a Saturday
            IDAY_OF_WEEK = 2
         ELSE IF (ID .EQ. 0) THEN
C           This is a Sunday
            IDAY_OF_WEEK = 3
         END IF
         IF (ID .EQ. 0) THEN
C           This is a Sunday
            IDAY_OF_WEEK7 = 7
         ELSE
C           This is weekday or Saturday
            IDAY_OF_WEEK7 = ID
         END IF
      ELSE
C ---    Use 1 as default
         IDAY_OF_WEEK  = 1
         IDAY_OF_WEEK7 = 1
      END IF

      RETURN
      END

      SUBROUTINE METCHK
C***********************************************************************
C                 METCHK Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Performs Various Checks and Quality Assurance of
C                 One Hour of Meteorological Data
C
C        PROGRAMMER: JEFF WANG, ROGER BRODE
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To skip date sequence checking for EVENT processing,
C                    which is handled separately by EV_CHKDAT.
C                    R.W. Brode, PES, Inc., 5/12/99
C
C        INPUTS:  Meteorological Variables for One Hour
C
C        OUTPUTS: Meteorological Data Error and Status Switches
C
C        CALLED FROM:   METEXT
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'METCHK'
      CLMHR  = .FALSE.
      MSGHR  = .FALSE.

      IF (.NOT.NOCHKD .AND. .NOT.EVONLY) THEN
C----    Check date for record out of sequence on the surface
C        scaling file - NOCHKD=.TRUE. means no date check   ---   CALL CHKDAT
         CALL CHKDAT
      END IF

C---- Compare date & time in the surface and profile files  ---   CALL CMPDAT
      CALL CMPDAT

C---- Check Data for Calm Winds                             ---   CALL CHKCLM
      CALL CHKCLM

      IF (.NOT. CLMHR) THEN
C----    Check data for missing data indicators             ---   CALL CHKMSG
         CALL CHKMSG
      END IF

C---- Check Data for Out-of-Range Values                    ---   CALL METQA
      CALL METQA

      RETURN
      END

      SUBROUTINE CHKDAT
C***********************************************************************
C                 CHKDAT Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Checks Meteorological Data for Record Out of Sequence
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   Include additional checks to identify data gaps
C                    that consist of complete days and/or complete 
C                    years.  A data gap of complete year(s) is treated
C                    as a non-fatal warning, since a year of met data
C                    may be skipped due to data capture problems.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
C
C        MODIFIED:   Include option to issue Warning rather than 
C                    Fatal Error message for meteorological record
C                    out of sequence (WARNCHKD option on MODELOPT).
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
C
C        MODIFIED:   To remove support for unformatted meteorological
C                    data files.
C                    R.W. Brode, PES, Inc., 4/10/2000
C
C        MODIFIED:   To incorporate modifications to date processing
C                    for Y2K compliance.  Specifically, allow for
C                    transition from KURDAT=99123124 to KURDAT=00010101
C                    for new century.
C                    R.W. Brode, PES, Inc., 5/12/99
C
C        INPUTS:  Date Variable
C
C        OUTPUTS: Date Error Messages
C
C        CALLED FROM:   METCHK
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'CHKDAT'

C --- Check for Met Data Record Out of Sequence; 
C     IPDATE and KURDAT are 8-digit date variables (YYMMDDHH) for
C     the previous hour and current hour, respectively
      IF (IPDATE .GT. 0) THEN
         IF (KURDAT .LE. IPDATE) THEN
C ---       Previous date is .LE. current date; check for date crossing 
C           century mark.
            IF (KURDAT.NE.10101 .OR. IPDATE.NE.99123124) THEN
C ---          Record Out of Sequence; current date is same or earlier 
C              than previous date
               WRITE(DUMMY,'(I8.8)') KURDAT
               IF (.NOT. L_WARNCHKD) THEN
C ---             Write Error Message - Record out of sequence
                  CALL ERRHDL(PATH,MODNAM,'E','450',DUMMY)
                  RUNERR = .TRUE.
               ELSE IF (L_WARNCHKD .AND. .NOT.L_SkipMessages) THEN
C ---             Write Warning Message - Record out of sequence
                  CALL ERRHDL(PATH,MODNAM,'W','450',DUMMY)
               END IF
            END IF
         ELSE IF (IHOUR.GT.1) THEN
C ---       Current hour > 01
            IF ((KURDAT-IPDATE) .EQ. 1) THEN
C ---          Record is in sequence - continue with processing
               CONTINUE
            ELSE IF ((KURDAT-IPDATE) .LT. 23) THEN
C ---          Gap is within the same day; issue message with date
C              of the gap and a second message with number of hours
C              within the gap
               WRITE(DUMMY,'(I8.8)') KURDAT
               IF (.NOT. L_WARNCHKD) THEN
C ---             Write Error Message - Record out of sequence
                  CALL ERRHDL(PATH,MODNAM,'E','450',DUMMY)
                  RUNERR = .TRUE.
               ELSE IF (L_WARNCHKD .AND. .NOT.L_SkipMessages) THEN
C ---             Write Warning Message - Record out of sequence
                  CALL ERRHDL(PATH,MODNAM,'W','450',DUMMY)
               END IF
               WRITE(DUMMY,'(I3,'' hour gap'')') KURDAT-IPDATE-1
               IF (.NOT. L_WARNCHKD) THEN
C ---             Write Error Message - Record out of sequence
                  CALL ERRHDL(PATH,MODNAM,'E','450',DUMMY)
                  RUNERR = .TRUE.
               ELSE IF (L_WARNCHKD .AND. .NOT.L_SkipMessages) THEN
C ---             Write Warning Message - Record out of sequence
                  CALL ERRHDL(PATH,MODNAM,'W','450',DUMMY)
               END IF
            ELSE
C ---          Gap extends beyond the current day; issue message with
C              date of the gap
               WRITE(DUMMY,'(I8.8)') KURDAT
               IF (.NOT. L_WARNCHKD) THEN
C ---             Write Error Message - Record out of sequence
                  CALL ERRHDL(PATH,MODNAM,'E','450',DUMMY)
                  RUNERR = .TRUE.
               ELSE IF (L_WARNCHKD .AND. .NOT.L_SkipMessages) THEN
C ---             Write Warning Message - Record out of sequence
                  CALL ERRHDL(PATH,MODNAM,'W','450',DUMMY)
               END IF
            END IF
         ELSE IF (IHOUR.EQ.1 .AND. IPHOUR.EQ.24) THEN
C ---       Current hour is 01 and previous hour is 24; look for 
C           gaps between days
            IF (JDAY.GT.1 .AND. JDAY-JDAY_PREV.EQ.1 .AND.
     &                                     IYR.EQ.IPYEAR) THEN
C ---          No gap between days within same year; continue processing
               CONTINUE
            ELSE IF (JDAY.GT.1 .AND. JDAY-JDAY_PREV.GT.1 .AND.
     &                                     IYR.EQ.IPYEAR) THEN
C ---          Record Out of Sequence; gap of full day(s); issue message
C              with date of gap and second message with # of days in gap
               WRITE(DUMMY,'(I8.8)') KURDAT
               IF (.NOT. L_WARNCHKD) THEN
C ---             Write Error Message - Record out of sequence
                  CALL ERRHDL(PATH,MODNAM,'E','450',DUMMY)
                  RUNERR = .TRUE.
               ELSE IF (L_WARNCHKD .AND. .NOT.L_SkipMessages) THEN
C ---             Write Warning Message - Record out of sequence
                  CALL ERRHDL(PATH,MODNAM,'W','450',DUMMY)
               END IF
               WRITE(DUMMY,'(I3,'' day gap'')') JDAY-JDAY_PREV-1
               IF (.NOT. L_WARNCHKD) THEN
C ---             Write Error Message - Record out of sequence
                  CALL ERRHDL(PATH,MODNAM,'E','450',DUMMY)
               ELSE IF (L_WARNCHKD .AND. .NOT.L_SkipMessages) THEN
C ---             Write Warning Message - Record out of sequence
                  CALL ERRHDL(PATH,MODNAM,'W','450',DUMMY)
               END IF
            ELSE IF (JDAY.GT.1 .AND. JDAY-JDAY_PREV.GT.1 .AND.
     &                                     IYR.GT.IPYEAR) THEN
C ---          Record Out of Sequence; gap between years; issue message
C              with date of gap and second message with # of days in gap
               WRITE(DUMMY,'(I8.8)') KURDAT
               IF (.NOT. L_WARNCHKD) THEN
C ---             Write Error Message - Record out of sequence
                  CALL ERRHDL(PATH,MODNAM,'E','450',DUMMY)
                  RUNERR = .TRUE.
               ELSE IF (L_WARNCHKD .AND. .NOT.L_SkipMessages) THEN
C ---             Write Warning Message - Record out of sequence
                  CALL ERRHDL(PATH,MODNAM,'W','450',DUMMY)
               END IF
               WRITE(DUMMY,'(I2,'' yr;'',I3,'' dy'')') IYR-IPYEAR,
     &                                               JDAY-JDAY_PREV-1
               IF (.NOT. L_WARNCHKD) THEN
C ---             Write Error Message - Record out of sequence
                  CALL ERRHDL(PATH,MODNAM,'E','450',DUMMY)
               ELSE IF (L_WARNCHKD .AND. .NOT.L_SkipMessages) THEN
C ---             Write Warning Message - Record out of sequence
                  CALL ERRHDL(PATH,MODNAM,'W','450',DUMMY)
               END IF
            ELSE IF (JDAY.GT.1 .AND. JDAY_PREV.GE.JDAY .AND.
     &                                     IYR.GT.IPYEAR) THEN
C ---          Record Out of Sequence; gap between years; issue message
C              with date of gap and second message with # of days in gap
               WRITE(DUMMY,'(I8.8)') KURDAT
               IF (.NOT. L_WARNCHKD) THEN
C ---             Write Error Message - Record out of sequence
                  CALL ERRHDL(PATH,MODNAM,'E','450',DUMMY)
                  RUNERR = .TRUE.
               ELSE IF (L_WARNCHKD .AND. .NOT.L_SkipMessages) THEN
C ---             Write Warning Message - Record out of sequence
                  CALL ERRHDL(PATH,MODNAM,'W','450',DUMMY)
               END IF
               WRITE(DUMMY,'(I2,'' yr;'',I3,'' dy'')') IYR-IPYEAR-1,
     &                                           (365-JDAY_PREV)+JDAY-1
               IF (.NOT. L_WARNCHKD) THEN
C ---             Write Error Message - Record out of sequence
                  CALL ERRHDL(PATH,MODNAM,'E','450',DUMMY)
               ELSE IF (L_WARNCHKD .AND. .NOT.L_SkipMessages) THEN
C ---             Write Warning Message - Record out of sequence
                  CALL ERRHDL(PATH,MODNAM,'W','450',DUMMY)
               END IF
            ELSE IF (JDAY.EQ.1 .AND. JDAY_PREV.GE.1 .AND.
     &                                     (IYR-IPYEAR.EQ.1) )THEN
C ---          Check for gap at end of previous year, accounting for leap years
               IF(  (MOD(IPYEAR,4) .NE. 0) .OR.
     &            (MOD(IPYEAR,100) .EQ. 0 .AND. 
     &             MOD(IPYEAR,400) .NE. 0) )THEN
C ---             Previous year is not a leap year, check for previous JDAY < 365
                  IF (JDAY_PREV.LT.365 ) THEN
C ---                Record Out of Sequence; gap at end of previous non-leap year;
C                    issue two warning messages, first with standard warning
C                    indicating location of gap and second warning indicating
C                    number of days in gap
                     WRITE(DUMMY,'(I8.8)') KURDAT
                     IF (.NOT. L_WARNCHKD) THEN
C ---                   Write Error Message - Record out of sequence
                        CALL ERRHDL(PATH,MODNAM,'E','450',DUMMY)
                        RUNERR = .TRUE.
                     ELSE IF (L_WARNCHKD .AND. .NOT.L_SkipMessages) THEN
C ---                   Write Warning Message - Record out of sequence
                        CALL ERRHDL(PATH,MODNAM,'W','450',DUMMY)
                     END IF
                     WRITE(DUMMY,'(I3,'' day gap'')') 365-JDAY_PREV
                     IF (.NOT. L_WARNCHKD) THEN
C ---                   Write Error Message - Record out of sequence
                        CALL ERRHDL(PATH,MODNAM,'E','450',DUMMY)
                     ELSE IF (L_WARNCHKD .AND. .NOT.L_SkipMessages) THEN
C ---                   Write Warning Message - Record out of sequence
                        CALL ERRHDL(PATH,MODNAM,'W','450',DUMMY)
                     END IF
                  END IF
               ELSE IF (JDAY_PREV .LT. 366) THEN
C ---             Previous year is a leap year, and previous JDAY < 366;
C                 Record Out of Sequence; gap at end of previous leap year;
C                 issue two warning messages, first with standard warning
C                 indicating location of gap and second warning indicating
C                 number of days in gap
                  WRITE(DUMMY,'(I8.8)') KURDAT
                  IF (.NOT. L_WARNCHKD) THEN
C ---                Write Error Message - Record out of sequence
                     CALL ERRHDL(PATH,MODNAM,'E','450',DUMMY)
                     RUNERR = .TRUE.
                  ELSE IF (L_WARNCHKD .AND. .NOT.L_SkipMessages) THEN
C ---                Write Warning Message - Record out of sequence
                     CALL ERRHDL(PATH,MODNAM,'W','450',DUMMY)
                  END IF
                  WRITE(DUMMY,'(I3,'' day gap'')') 366-JDAY_PREV
                  IF (.NOT. L_WARNCHKD) THEN
C ---                Write Error Message - Record out of sequence
                     CALL ERRHDL(PATH,MODNAM,'E','450',DUMMY)
                  ELSE IF (L_WARNCHKD .AND. .NOT.L_SkipMessages) THEN
C ---                Write Warning Message - Record out of sequence
                     CALL ERRHDL(PATH,MODNAM,'W','450',DUMMY)
                  END IF
               END IF
            ELSE IF (IHOUR.EQ.1 .AND. JDAY.EQ.1 .AND. 
     &                           JDAY_PREV.GE.1 .AND. 
     &                                        (IYR-IPYEAR).GT.1) THEN
C ---          Record Out of Sequence; gap of at least 1 complete year.
C              First check for additional gaps at the end of the previous
C              year, which would be an error (unless WARNCHKD is specified).
C       
C ---          Check for gap at end of previous year, accounting for leap years
               IF(  (MOD(IPYEAR,4) .NE. 0) .OR.
     &            (MOD(IPYEAR,100) .EQ. 0 .AND. 
     &             MOD(IPYEAR,400) .NE. 0) )THEN
C ---             Previous year is not a leap year, check for previous JDAY < 365
                  IF (JDAY_PREV.LT.365 ) THEN
C ---                Record Out of Sequence; gap at end of previous non-leap year;
C                    issue two warning messages, first with standard warning
C                    indicating location of gap and second warning indicating
C                    number of days in gap
                     WRITE(DUMMY,'(I8.8)') KURDAT
                     IF (.NOT. L_WARNCHKD) THEN
C ---                   Write Error Message - Record out of sequence
                        CALL ERRHDL(PATH,MODNAM,'E','450',DUMMY)
                        RUNERR = .TRUE.
                     ELSE IF (L_WARNCHKD .AND. .NOT.L_SkipMessages) THEN
C ---                   Write Warning Message - Record out of sequence
                        CALL ERRHDL(PATH,MODNAM,'W','450',DUMMY)
                     END IF
                     WRITE(DUMMY,'(I3,'' day gap'')') 365-JDAY_PREV
                     IF (.NOT. L_WARNCHKD) THEN
C ---                   Write Error Message - Record out of sequence
                        CALL ERRHDL(PATH,MODNAM,'E','450',DUMMY)
                     ELSE IF (L_WARNCHKD .AND. .NOT.L_SkipMessages) THEN
C ---                   Write Warning Message - Record out of sequence
                        CALL ERRHDL(PATH,MODNAM,'W','450',DUMMY)
                     END IF
                  END IF
               ELSE IF (JDAY_PREV .LT. 366) THEN
C ---             Previous year is a leap year, and previous JDAY < 366;
C                 Record Out of Sequence; gap at end of previous leap year;
C                 issue two warning messages, first with standard warning
C                 indicating location of gap and second warning indicating
C                 number of days in gap
                  WRITE(DUMMY,'(I8.8)') KURDAT
                  IF (.NOT. L_WARNCHKD) THEN
C ---                Write Error Message - Record out of sequence
                     CALL ERRHDL(PATH,MODNAM,'E','450',DUMMY)
                     RUNERR = .TRUE.
                  ELSE IF (L_WARNCHKD .AND. .NOT.L_SkipMessages) THEN
C ---                Write Warning Message - Record out of sequence
                     CALL ERRHDL(PATH,MODNAM,'W','450',DUMMY)
                  END IF
                  WRITE(DUMMY,'(I3,'' day gap'')') 366-JDAY_PREV
                  IF (.NOT. L_WARNCHKD) THEN
C ---                Write Error Message - Record out of sequence
                     CALL ERRHDL(PATH,MODNAM,'E','450',DUMMY)
                  ELSE IF (L_WARNCHKD .AND. .NOT.L_SkipMessages) THEN
C ---                Write Warning Message - Record out of sequence
                     CALL ERRHDL(PATH,MODNAM,'W','450',DUMMY)
                  END IF
               END IF
        
C ---          Now issue two warning messages regarding full year(s) 
C              data gap, first with standard warning indicating location 
C              of gap and second warning indicating number of years in gap
               WRITE(DUMMY,'(I8.8)') KURDAT
               IF (.NOT. L_WARNCHKD) THEN
C ---             Write Warning Message - Record out of sequence
                  CALL ERRHDL(PATH,MODNAM,'W','450',DUMMY)
               ELSE IF (L_WARNCHKD .AND. .NOT.L_SkipMessages) THEN
C ---             Write Warning Message - Record out of sequence
                  CALL ERRHDL(PATH,MODNAM,'W','450',DUMMY)
               END IF
               WRITE(DUMMY,'(I3,'' year gap'')') IYR-IPYEAR-1
               IF (.NOT. L_WARNCHKD) THEN
C ---             Write Warning Message - Record out of sequence
                  CALL ERRHDL(PATH,MODNAM,'W','450',DUMMY)
               ELSE IF (L_WARNCHKD .AND. .NOT.L_SkipMessages) THEN
C ---             Write Warning Message - Record out of sequence
                  CALL ERRHDL(PATH,MODNAM,'W','450',DUMMY)
               END IF
            END IF
            
         ELSE IF (IHOUR.EQ.1 .AND. IPHOUR.NE.24) THEN
C ---       Record Out of Sequence - gap between days; issue first
C           message with date of data gap, with a second message 
C           with # hours/days in gap
            WRITE(DUMMY,'(I8.8)') KURDAT
            IF (.NOT. L_WARNCHKD) THEN
C ---          Write Error Message - Record out of sequence
               CALL ERRHDL(PATH,MODNAM,'E','450',DUMMY)
               RUNERR = .TRUE.
            ELSE IF (L_WARNCHKD .AND. .NOT.L_SkipMessages) THEN
C ---          Write Warning Message - Record out of sequence
               CALL ERRHDL(PATH,MODNAM,'W','450',DUMMY)
            END IF
            IF (JDAY.GT.1 .AND. JDAY-JDAY_PREV.GT.1 .AND.
     &                                      IYR.EQ.IPYEAR) THEN
C ---          Gap of at least 1 day; issue message with # of hours
C              and # of days.
               WRITE(DUMMY,'(I2,''hr & '',I3,''dy'')') 24-IPHOUR, 
     &                                          JDAY-JDAY_PREV-1
               IF (.NOT. L_WARNCHKD) THEN
C ---             Write Error Message - Record out of sequence
                  CALL ERRHDL(PATH,MODNAM,'E','450',DUMMY)
               ELSE IF (L_WARNCHKD .AND. .NOT.L_SkipMessages) THEN
C ---             Write Warning Message - Record out of sequence
                  CALL ERRHDL(PATH,MODNAM,'W','450',DUMMY)
               END IF
            ELSE IF (JDAY.EQ.1 .AND. JDAY_PREV.GE.1 .AND.
     &                             (IYR-IPYEAR.EQ.1) )THEN
C ---          Record Out of Sequence; gap of full day(s); issue message
C              with date of gap and second message with # of days in gap
               WRITE(DUMMY,'(I8.8)') KURDAT
               IF (.NOT. L_WARNCHKD) THEN
C ---             Write Error Message - Record out of sequence
                  CALL ERRHDL(PATH,MODNAM,'E','450',DUMMY)
                  RUNERR = .TRUE.
               ELSE IF (L_WARNCHKD .AND. .NOT.L_SkipMessages) THEN
C ---             Write Warning Message - Record out of sequence
                  CALL ERRHDL(PATH,MODNAM,'W','450',DUMMY)
               END IF
               WRITE(DUMMY,'(I3,'' day gap'')') JDAY-JDAY_PREV-1
               IF (.NOT. L_WARNCHKD) THEN
C ---             Write Error Message - Record out of sequence
                  CALL ERRHDL(PATH,MODNAM,'E','450',DUMMY)
               ELSE IF (L_WARNCHKD .AND. .NOT.L_SkipMessages) THEN
C ---             Write Warning Message - Record out of sequence
                  CALL ERRHDL(PATH,MODNAM,'W','450',DUMMY)
               END IF
            END IF
         END IF
      END IF

      RETURN
      END

      SUBROUTINE CMPDAT
C***********************************************************************
C             CMPDAT Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Compares the date and time from the scalar and profile
C                 files
C
C        PROGRAMMER: Jim Paumier, PES, Inc.
C
C        DATE:    September 30, 1993
C
C        INPUTS:  Date variables
C
C        OUTPUTS: Date error messages
C
C        ASSUMPTIONS:   <none>
C
C        CALLED FROM:   METCHK
C***********************************************************************

C---- Variable declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C---- Variable initializations
      MODNAM = 'CMPDAT'

C---- Check for a date mismatch between the scalar and profile files
C
      IF (KURDAT .NE. KURPFL) THEN
C        WRITE Error Message - Date mismatch
         WRITE(DUMMY,'(I8.8)') KURDAT
         CALL ERRHDL(PATH,MODNAM,'E','456',DUMMY)
         RUNERR = .TRUE.
C
      END IF

      RETURN
      END
C
      SUBROUTINE CHKCLM
C***********************************************************************
C                 CHKCLM Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Checks One Hour Meteorological Data for Calm Winds
C
C        PROGRAMMER: ROGER BRODE, JEFF WANG
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Meteorological Variables for One Hour
C
C        OUTPUTS: Calm Hour Flag, CLMHR, and Message
C
C        CALLED FROM:   METCHK
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'CHKCLM'

C     Check Data for Calm Winds, defined as UREF = 0.0
      IF (UREF .EQ. 0.0D0) THEN
         CLMHR = .TRUE.
         IF (.NOT. L_SkipMessages) THEN
C           WRITE Informational Message: Calm Hour
            WRITE(DUMMY,'(I10.10)') FULLDATE
            CALL ERRHDL(PATH,MODNAM,'I','440',DUMMY)
         END IF
      END IF

      RETURN
      END

      SUBROUTINE CHKMSG
C***********************************************************************
C                 CHKMSG Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Checks One Hour Meteorological Data for Missing Data
C
C        PROGRAMMER: JEFF WANG
C
C        DATE:    March 2, 1992
C
C        MODIFIED:  To Add Upper Bound on USTAR Range Check - 12/07/2006
C
C        MODIFIED:  To Change Wind Direction Range Check - 10/26/2004
C
C        MODIFIED:  To Change Temperature Range Check - 9/29/92
C
C        INPUTS:  Meteorological Variables for One Hour
C
C        OUTPUTS: Meteorological Data Error and Status Switches
C
C        CALLED FROM:   METCHK
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'CHKMSG'

C---- Check Data for Missing Data Indicators
C
C     Wind speed (meters/second)
      IF( UREF .GE. 90.0D0 .OR. UREF .LT. 0.0D0 )THEN
         MSGHR = .TRUE.
C
C     Wind direction (degrees from north)
      ELSE IF( (WDREF .GT. 900.0D0)  .OR.  (WDREF .LE. -9.0D0) )THEN
         MSGHR = .TRUE.
C
C     Ambient temperature (kelvins)
      ELSE IF( (TA .GT. 900.0D0)  .OR.  (TA .LE. 0.0D0) )THEN
         MSGHR = .TRUE.
C
C     Monin-Obukhov length (meters)
      ELSE IF( OBULEN  .LT.  -99990.0D0 )THEN
         MSGHR = .TRUE.

C     Convective Mixing height (meters)
      ELSE IF( OBULEN .LT. 0.0D0 .AND.
     &        ((ZICONV .GT. 90000.0D0)  .OR.  (ZICONV .LT. 0.0D0)) )THEN
         MSGHR = .TRUE.
C
C     Mechanical Mixing height (meters)
      ELSE IF( (ZIMECH .GT. 90000.0D0)  .OR.  (ZIMECH .LT. 0.0D0) )THEN
         MSGHR = .TRUE.
C
C     Surface friction velocity (meters/second)
      ELSE IF( USTAR  .LT.  0.0D0 .OR. USTAR .GE. 9.0D0 )THEN
         MSGHR = .TRUE.

C     Convective velocity scale (meters/second)
      ELSE IF( WSTAR .LT. 0.0D0 .AND.
     &       (OBULEN .LT. 0.0D0 .AND. OBULEN .GT. -99990.0D0) )THEN
         MSGHR = .TRUE.
C
      END IF

      RETURN
      END

      SUBROUTINE METQA
C***********************************************************************
C                 METQA Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Performs Quality Assurance Checks for
C                 One Hour of Meteorological Data
C
C        PROGRAMMER: JEFF WANG, ROGER BRODE
C
C        DATE:    March 2, 1992
C
C        MODIFIED:  Added range check for vertical potential temperature
C                   gradient above ZI (variable VPTGZI).  Also applies
C                   minimum value of 0.005 K/m for consistency with
C                   AERMET.
C                   R. Brode, MACTEC/PES, 10/17/2005
C
C        MODIFIED:  Included check for absolute values of Monin-Obukhov
C                   length (OBULEN) less than 1.0.  Adjustment of OBULEN
C                   is made to limit ABS(OBULEN) .GE. 1.0.  The sign of
C                   OBULEN is assigned the opposite of the sign of the
C                   heat flux if OBULEN is 0.0.  This limit on OBULEN is
C                   already applied in AERMET, so this change will
C                   only affect input data generated by other means.
C                   R. Brode, MACTEC/PES, 10/26/2004
C
C        MODIFIED:  To adjust warning limit for USTAR from 2.0 to 4.0,
C                   adjust warning limit for WSTAR from 3.0 to 4.0, and
C                   to minimize duplication of warning messages for
C                   missing hours.
C                   R. Brode, MACTEC/PES, 10/26/2004
C
C        MODIFIED:  To check for errors reading surface variables for
C                   new deposition algorithms.  R. Brode, PES, 12/6/94
C
C        MODIFIED:  To Change Temperature Range Check Lower Limit To
C                   230 K - 9/29/92
C
C        INPUTS:  Meteorological Variables for One Hour
C
C        OUTPUTS: Meteorological Data Error and Status Switches
C
C        CALLED FROM:   METCHK
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      INTEGER :: NL

C     Variable Initializations
      MODNAM = 'METQA'

C---- Check Data for Out-of-Range Values:

C---- Wind direction:
      IF( WDREF .EQ. 0.0D0)THEN
         WDREF = 360.0D0
      ENDIF

      DO NL = 1, NPLVLS
         IF( PFLWD(NL) .EQ. 0.0D0 )THEN
            PFLWD(NL) = 360.0D0
         END IF
      END DO

      IF( .NOT. L_SkipMessages )THEN
         IF( (WDREF.LT.  0.0D0 .AND. WDREF.GT.-9.0D0)  .OR.
     &       (WDREF.GT.360.0D0 .AND. WDREF.LT.900.0D0) )THEN
C           WRITE Warning Message: Invalid Wind Dir'n
            WRITE(DUMMY,'(I8.8)') KURDAT
            CALL ERRHDL(PATH,MODNAM,'W','410',DUMMY)
         END IF

C----    Wind speed range:
         IF( UREF.LT.0.0D0 .AND. UREF.GT.-9.0D0)THEN
C           WRITE Warning Message: Invalid Wind Speed;
C           This case is already flagged as missing hour, 
C           but not with standard missing data code
            WRITE(DUMMY,'(I8.8)') KURDAT
            CALL ERRHDL(PATH,MODNAM,'W','420',DUMMY)
         END IF
C    
         IF( UREF .GT. 30.0D0 .AND. UREF .LT. 90.0D0)THEN
C           WRITE Warning Message: Wind Speed Over 30m/s
            WRITE(DUMMY,'(I8.8)') KURDAT
            CALL ERRHDL(PATH,MODNAM,'W','420',DUMMY)
         END IF

C----    Wind data reference height:
         IF( UREFHT  .GT.  100.0D0 )THEN
C        
C           -----------------------------------------------
C           Height of the wind data to be used in the
C           computation is greater than 100m -  warn the user
C           -----------------------------------------------
         
            WRITE ( DUMMY, '(I8.8)' ) KURDAT
            CALL ERRHDL(PATH,MODNAM,'W','475',DUMMY)
         
         ELSE IF( UREFHT .LT. 0.001D0 .AND.  .NOT.CLMHR .AND.
     &                                       .NOT.MSGHR)THEN
C        
C           -----------------------------------------------
C           Height of the wind data to be used in the
C           computation is LT 0.001 for non-calm and 
C           non-missing hour - this is an invalid entry; 
C           issue fatal error message
C           -----------------------------------------------
        
            WRITE ( DUMMY, '(I8.8)' ) KURDAT
            CALL ERRHDL(PATH,MODNAM,'E','474',DUMMY)
            
            RUNERR = .TRUE.
         
         ENDIF
        
C----    Ambient temperature:
         IF( (TA .LT. 220.0D0 .AND. TA .GT. 0.0D0)  .OR.
     &       (TA .GT. 330.0D0 .AND. TA .LT. 900.0D0) )THEN
C           WRITE Warning Message: Ambient Temperature May be Out-of-Range
            WRITE(DUMMY,'(I8.8)') KURDAT
            CALL ERRHDL(PATH,MODNAM,'W','430',DUMMY)
         END IF
         
C----    Friction velocity (meters/second):
         IF( USTAR .GT. 4.0D0 )THEN
C           WRITE Warning Message: Friction velocity may be too large
            WRITE(DUMMY,'(I8.8)') KURDAT
            CALL ERRHDL(PATH,MODNAM,'W','432',DUMMY)
         END IF

      END IF

C---- Convective velocity (meters/second):
      IF( WSTAR .GT. 4.00D0 )THEN
C        WRITE Warning Message: Convective velocity may be too large
         IF( .NOT. L_SkipMessages )THEN
            WRITE(DUMMY,'(I8.8)') KURDAT
            CALL ERRHDL(PATH,MODNAM,'W','438',DUMMY)
         END IF
      ELSE IF( WSTAR .EQ. 0.0D0 )THEN
C        WRITE Warning Message: Convective velocity = 0.0, set to 0.001
         IF( .NOT. L_SkipMessages )THEN
            WRITE(DUMMY,'(I8.8)') KURDAT
            CALL ERRHDL(PATH,MODNAM,'W','438',DUMMY)
         END IF
         WSTAR = 0.001D0
      END IF

C---- Monin-Obukhov length (meters):
      IF( DABS(OBULEN) .LT. 1.00D0 )THEN
C        WRITE Warning Message: Monin-Obukhov length is too small
         IF( .NOT. L_SkipMessages )THEN
            WRITE(DUMMY,'(I8.8)') KURDAT
            CALL ERRHDL(PATH,MODNAM,'W','439',DUMMY)
         END IF
C        Set ABS(OBULEN) = 1.0D0
         IF (OBULEN .LT. 0.0D0) THEN
            OBULEN = -1.0D0
         ELSE IF (OBULEN .GT. 0.0D0) THEN
            OBULEN =  1.0D0
         ELSE
            OBULEN = -1.0D0 * DSIGN( 1.0D0, SFCHF )
         END IF
      END IF

C---- Vertical potential temperature gradient above ZI (K/m)
      IF( ZICONV .GT. 0.0D0 .AND. OBULEN .LT. 0.0D0 .AND.
     &    OBULEN .GT. -99990.0D0 .AND. VPTGZI .LT. 0.005D0 )THEN
C        WRITE Warning Message: VPTGZI less than or equal to 0.005 K/m
         IF( .NOT. L_SkipMessages )THEN
            WRITE(DUMMY,'(I8.8)') KURDAT
            CALL ERRHDL(PATH,MODNAM,'W','441',DUMMY)
         END IF
C        Adjust value to 0.005
         VPTGZI = 0.005D0
      ELSE IF( ZICONV .GT. 0.0D0 .AND. OBULEN .LT. 0.0D0 .AND.
     &         OBULEN .GT. -99990.0D0 .AND. VPTGZI .GT. 0.10D0 )THEN
C        WRITE Warning Message: VPTGZI is greater than 0.10 K/m
         IF( .NOT. L_SkipMessages )THEN
            WRITE(DUMMY,'(I8.8)') KURDAT
            CALL ERRHDL(PATH,MODNAM,'W','442',DUMMY)
         END IF
      END IF

C---- Surface roughness length (m):
      IF (SFCZ0 .LT. 0.0001D0) THEN
         IF (.NOT.MSGHR .AND. .NOT.CLMHR) THEN
C           WRITE Warning Message:  Surface roughness length out-of-range
            IF( .NOT. L_SkipMessages )THEN
               WRITE(DUMMY,'(I8.8)') KURDAT
               CALL ERRHDL(PATH,MODNAM,'W','435',DUMMY)
            END IF
         END IF
C        Set to 0.0001 to avoid divide-by-zero error
         SFCZ0 = 0.0001D0
      END IF

C---- Check for precipitation rate out of range
      IF (PRATE .LT. 0.0D0 .OR. PRATE .GT. 900.0D0) THEN
C        Assume precipitation is missing, set to 0.0
         PRATE = 0.0D0
      ELSE
C ---    Calculate total precipitation
         TOTAL_PRECIP = TOTAL_PRECIP + PRATE
      END IF

      RETURN
      END

      SUBROUTINE METDAT(IOUNT)
C***********************************************************************
C                 METDAT Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Print Out The Summary Of The Meteorology Data
C
C        PROGRAMMER: JEFF WANG
C        MODIFIED BY D. Strimaitis, SRC (for Wet & Dry DEPOSITION)
C
C        DATE:    November 8, 1993
C
C        MODIFIED:   To include output file unit argument to support
C                    output to main 'aermod.out' file and to the
C                    optional SUMMFILE.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
C
C        MODIFIED:   To include met data version date from surface file.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 01/26/2007
C
C        MODIFIED:   To remove support for unformatted meteorological
C                    data files.
C                    R.W. Brode, PES, Inc., 4/10/2000
C
C        MODIFIED BY R.W. Brode, PES, to avoid print string > 132 chars.
C        (DATE:    December 29, 1997)
C
C        MODIFIED BY D. Strimaitis, SRC (for Dry DEPOSITION)
C        (DATE:    February 15, 1993)
C
C        INPUTS:  Meteorology Input Data
C
C        OUTPUTS: Printed Model Outputs
C
C        CALLED FROM:   METEXT, MEREAD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, ILMAX, IJDAY, IOUNT

C     Variable Initializations
      MODNAM = 'METDAT'

C---- WRITE Out Header Information
      CALL HEADER(IOUNT)
      WRITE(IOUNT,9011)
      ILMAX = MIN( 80, ILEN_FLD )
      WRITE(IOUNT,9016) METINP(1:ILMAX), C_METVER, PROINP(1:ILMAX),
     &                  METFRM, PROFRM
      WRITE(IOUNT,9020) IDSURF, IDUAIR, SFNAME, UANAME,
     &                  ISYEAR, IUYEAR
      WRITE(IOUNT,9024)
      IF (LDPART .OR. LDGAS .OR. LWPART .OR. LWGAS .OR. GRSM) THEN
         WRITE(IOUNT,99025)
      ELSE
         WRITE(IOUNT,9025)
      END IF

C---- Since the first record has been read, write out the data to
C     IOUNIT, then read the next record from the scalar file

      DO I = 1, 24
C----    Loop through first 24 hours of data file

C        We use the IF..ELSE structure because the global variable
C        for Julian day that is JDAY, not IJDAY. This avoids overwriting
C        JDAY read in METEXT.
         IF( I .EQ. 1 )THEN

            IF( LDPART .OR. LWPART .OR. LDGAS .OR. LWGAS .OR. 
     &          GRSM)THEN
               WRITE(IOUNT,99026) IYEAR, IMONTH, IDAY, IHOUR,
     &            SFCHF, USTAR, WSTAR, VPTGZI, ZICONV, ZIMECH, OBULEN,
     &            SFCZ0, BOWEN, ALBEDO, UREF, WDREF, UREFHT, TA, TREFHT,
     &            IPCODE, PRATE, RH, SFCP, NCLOUD
            ELSE
               WRITE(IOUNT,9026) IYEAR, IMONTH, IDAY, JDAY, IHOUR,
     &            SFCHF, USTAR, WSTAR, VPTGZI, ZICONV, ZIMECH, OBULEN,
     &            SFCZ0, BOWEN, ALBEDO, UREF, WDREF, UREFHT, TA, TREFHT
            END IF

         ELSE
            IF (IYEAR .GE. 100) THEN
               IYEAR = IYEAR - 100 * (IYEAR/100)
            END IF
            IF( LDPART .OR. LWPART .OR. LDGAS .OR. LWGAS .OR. 
     &          GRSM)THEN
               WRITE(IOUNT,99026) IYEAR, IMONTH, IDAY, IHOUR,
     &            SFCHF, USTAR, WSTAR, VPTGZI, ZICONV, ZIMECH, OBULEN,
     &            SFCZ0, BOWEN, ALBEDO, UREF, WDREF, UREFHT, TA, TREFHT,
     &            IPCODE, PRATE, RH, SFCP, NCLOUD
            ELSE
               WRITE(IOUNT,9026) IYEAR, IMONTH, IDAY, IJDAY, IHOUR,
     &            SFCHF, USTAR, WSTAR, VPTGZI, ZICONV, ZIMECH, OBULEN,
     &            SFCZ0, BOWEN, ALBEDO, UREF, WDREF, UREFHT, TA, TREFHT
            END IF

         END IF

         IF( LDPART .OR. LWPART .OR. LDGAS .OR. LWGAS .OR. GRSM )THEN
C           Read record from ASCII scalar parameter file using FREE format
C           with deposition variables
            READ( MFUNIT, *, END=999, ERR=99, IOSTAT=IOERRN) IYEAR,
     &         IMONTH, IDAY, IJDAY, IHOUR, SFCHF, USTAR, WSTAR,
     &         VPTGZI, ZICONV, ZIMECH, OBULEN, SFCZ0, BOWEN, ALBEDO,
     &         UREF, WDREF, UREFHT, TA, TREFHT, IPCODE, PRATE, RH,
     &         SFCP, NCLOUD
C
         ELSE
C           Read hourly records from ASCII file using FREE format
C           without deposition variables
            READ( MFUNIT, *, END=999, ERR=99, IOSTAT=IOERRN ) IYEAR,
     &         IMONTH, IDAY, IJDAY, IHOUR, SFCHF, USTAR, WSTAR,
     &         VPTGZI, ZICONV, ZIMECH, OBULEN, SFCZ0, BOWEN, ALBEDO,
     &         UREF, WDREF, UREFHT, TA, TREFHT, IPCODE, PRATE, RH,
     &         SFCP, NCLOUD
C
         END IF
C
      END DO
C
C---- REWIND met file, skip first record (with the latitude &
C     longitude), and read first data record to reset variables 
C     to the first hour in the file.
C
 999  CONTINUE
      REWIND MFUNIT
      READ( MFUNIT, '(I2)' )  IDUM
C
      IF( LDPART .OR. LWPART .OR. LDGAS .OR. LWGAS .OR. GRSM )THEN
C        Read record from ASCII scalar parameter file using FREE format
C        with deposition variables
         READ( MFUNIT, *, END=999, ERR=99, IOSTAT=IOERRN) IYEAR,
     &         IMONTH, IDAY, IJDAY, IHOUR, SFCHF, USTAR, WSTAR,
     &         VPTGZI, ZICONV, ZIMECH, OBULEN, SFCZ0, BOWEN, ALBEDO,
     &         UREF, WDREF, UREFHT, TA, TREFHT, IPCODE, PRATE, RH,
     &         SFCP, NCLOUD

C
      ELSE
C        Read hourly records from ASCII file using FREE format
C        without deposition variables
         READ( MFUNIT, *, END=999, ERR=99, IOSTAT=IOERRN ) IYEAR,
     &         IMONTH, IDAY, IJDAY, IHOUR, SFCHF, USTAR, WSTAR,
     &         VPTGZI, ZICONV, ZIMECH, OBULEN, SFCZ0, BOWEN, ALBEDO,
     &         UREF, WDREF, UREFHT, TA, TREFHT, IPCODE, PRATE, RH,
     &         SFCP, NCLOUD

C
      END IF

C---- Write the first hour of profile data to IOUNIT; only 1 hour
C        is written because there can be up to 50 levels of data, which
C        could create a large amount of output.

      IF( NPLVLS .GT. 10 )THEN
         CALL HEADER(IOUNT)
      ENDIF

      WRITE (IOUNT, 9034)
      WRITE (IOUNT, 9035)
      DO I = 1,NPLVLS
         WRITE (IOUNT, 9036) KYEAR, KMONTH, KDAY, KHOUR, PFLHT(I),
     &          IFLAG(I), PFLWD(I), PFLWS(I), PFLTA(I), PFLSA(I),
     &          PFLSW(I), PFLSV(I)

      END DO
      WRITE (IOUNT,9037)

      GO TO 9999
C
C---- FORMAT statements
C
 9011 FORMAT(/36X,'*** UP TO THE FIRST 24 HOURS OF ',
     &       'METEOROLOGICAL DATA ***'/)
 9016 FORMAT(3X,'Surface file:   ',A80,3X,'Met Version: ',A6,
     &     /,3X,'Profile file:   ',A80,
     &     /,3X,'Surface format: ',A105,
     &     /,3X,'Profile format: ',A105 )
 9020 FORMAT(3X,'Surface station no.: ',I8,18X,
     &       'Upper air station no.: ',I8/18X,'Name: ',A40,3X,
     &       'Name: ',A40/18X,'Year: ',I6,37X,'Year: ',I6)
 9024 FORMAT (/' First 24 hours of scalar data')
 9025 FORMAT (' YR',' MO', ' DY', ' JDY', ' HR', '     H0',
     &          '     U*', '     W*', '  DT/DZ', ' ZICNV', ' ZIMCH',
     &          '  M-O LEN', '    Z0', '  BOWEN', ' ALBEDO',
     &          '  REF WS', '   WD', '     HT', '  REF TA', '     HT',
     &         /61('- '))
99025 FORMAT (' YR',' MO', ' DY', ' HR', '     H0',
     &          '     U*', '     W*', '  DT/DZ', ' ZICNV', ' ZIMCH',
     &          '  M-O LEN', '  Z0 ', 'BOWEN', '  ALB',
     &          '  REF WS', '   WD', '   HT', '  REF TA', '  HT',
     &          ' IPCOD',' PRATE','  RH',' SFCP',' CCVR'
     &         /66('- '))
 9026 FORMAT ( 1X,3(I2.2,1X),I3,1X,I2.2,1X,F6.1,1X,3(F6.3,1X),
     &        2(F5.0,1X),F8.1,1X,F5.2,1X,2(F6.2,1X),F7.2,1X,F5.0,
     &        3(1X,F6.1) )
99026 FORMAT ( 1X,3(I2.2,1X),I2.2,1X,F6.1,1X,3(F6.3,1X),
     &        2(F5.0,1X),F8.1,3F5.2,1X,F7.2,1X,F5.0,
     &        1X,F4.0,1X,F6.1,1X,F4.0,I3,F7.2,F6.0,F6.0,I3)
 9034 FORMAT (//,' First hour of profile data')
 9035 FORMAT ( ' YR', ' MO', ' DY', ' HR', ' HEIGHT', ' F', '  WDIR',
     &         '    WSPD',' AMB_TMP', ' sigmaA', '  sigmaW',
     &         '  sigmaV' )
 9036 FORMAT (1X, 4(I2.2,1X),F6.1,1X,I1,1X,F5.0,1X,F7.2,1X,F7.1,1X,
     &        F6.1,1X,F7.2,1X,F7.2)
 9037 FORMAT (/ ' F indicates top of profile (=1) or below (=0)')

C---- WRITE Error Message:  Error Reading Met Data Input File
C
 99   CALL ERRHDL(PATH,MODNAM,'E','510','SURFFILE')
      RUNERR = .TRUE.

 9999 RETURN
      END

      SUBROUTINE METSUM
C***********************************************************************
C                 METSUM Module of AERMOD Model
C
C        PURPOSE: Print Out The Summary Of The Meteorology Data
C                 Sampled Using the SCIM Option
C
C        PROGRAMMER: Roger Brode, PES, Inc.
C
C        DATE:    April 14, 1998
C
C        MODIFIED:  To output missing temperatures correctly in the
C                   SCIM met data file.
C                   R.W. Brode, PES, Inc., - 02/25/02
C
C        INPUTS:  Meteorology Input Data
C
C        OUTPUTS: Printed Model Outputs
C
C        CALLED FROM:   HRLOOP
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      DOUBLE PRECISION :: PFLTEMP   ! Add PFLTEMP to 
      INTEGER :: I, ILMAX

C     Variable Initializations
      MODNAM = 'METSUM'

C     WRITE Out Header Information
      IF (ILINE .EQ. IFIRSTHR) THEN
         WRITE(ISUNIT,9011)
C        Write Surface Data, including user-specified SCIMBYHR parameters
         WRITE(ISUNIT,'(A,1x,I3)')  ' Start Hr: ', IFIRSTHR
         WRITE(ISUNIT,'(A,1x,I3)')  ' Interval: ', NREGINT
         WRITE(ISUNIT,*)
         ILMAX = MIN( 80, ILEN_FLD )
         WRITE(ISUNIT,9016) METINP(1:ILMAX), METFRM
         WRITE(ISUNIT,9020) IDSURF, IDUAIR, SFNAME, UANAME,
     &                      ISYEAR, IUYEAR

C        Write column headers, depending on whether deposition is included,
C        consistent with WRITE statements below.
C ---    Use FORMAT 98025 for all cases
         WRITE(ISUNIT,98025)
      
C        Write Profile Data
         WRITE(IPUNIT,99011)
C        Write Surface Data, including user-specified SCIMBYHR parameters
         WRITE(IPUNIT,'(A,1x,I3)') ' Start Hr: ', IFIRSTHR
         WRITE(IPUNIT,'(A,1x,I3)') ' Interval: ', NREGINT
         WRITE(IPUNIT,*)
         ILMAX = MIN( 80, ILEN_FLD )
         WRITE(IPUNIT,99016) PROINP(1:ILMAX), PROFRM
         WRITE(IPUNIT,99020) IDSURF, IDUAIR, SFNAME, UANAME,
     &                       ISYEAR, IUYEAR
         WRITE(IPUNIT,99025)
      END IF

C     Write surface file record, depending on whether deposition is included
      WRITE(ISUNIT,98026) IYEAR, IMONTH, IDAY, IHOUR,
     &      SFCHF, USTAR, WSTAR, VPTGZI, ZICONV, ZIMECH, OBULEN,
     &      SFCZ0, BOWEN, ALBEDO, UREF, WDREF, UREFHT, TA, TREFHT,
     &      IPCODE, PRATE, RH, SFCP, NCLOUD

      DO I = 1,NPLVLS
         IF (PFLTA(I) .EQ. -999.0D0) THEN
            PFLTEMP = PFLTA(I)
         ELSE
            PFLTEMP = PFLTA(I)-DCTODK
         END IF
         WRITE (IPUNIT, 99026) KYEAR, KMONTH, KDAY, KHOUR, PFLHT(I),
     &          IFLAG(I), PFLWD(I), PFLWS(I), PFLTEMP,
     &          PFLSW(I), PFLSV(I)
      END DO

 9011 FORMAT(/1X,'*** SUMMARY OF THE SAMPLED SURFACE ',
     &       'METEOROLOGICAL DATA USED WITH THE SCIM OPTION ***'/)
 9016 FORMAT(1X,'Surface file:   ',A80,
     &     /,1X,'Surface format: ',A105)
 9020 FORMAT(1X,'SURFACE STATION NO.: ',I6,20X,
     &       'UPPER AIR STATION NO.: ',I6/16X,'NAME: ',A40,3X,
     &       'NAME: ',A40/16X,'YEAR: ',I6,37X,'YEAR: ',I6/)
C Unused: 9025 FORMAT (' YR',' MO', ' DY', ' JDY', ' HR', '     H0',
C     &          '     U*', '     W*', '  DT/DZ', ' ZICNV', ' ZIMCH',
C     &          '  M-O LEN', '    Z0', '  BOWEN', ' ALBEDO',
C     &          '  REF WS', '   WD', '     HT', '  REF TA', '     HT',
C     &         /61(' -'))
98025 FORMAT (' YR',' MO', ' DY', ' HR', '     H0',
     &          '     U*', '     W*', '  DT/DZ', ' ZICNV', ' ZIMCH',
     &          '  M-O LEN', '   Z0 ', 'BOWEN', '  ALB',
     &          '  REF WS', '   WD', '   HT', '  REF TA', '  HT',
     &          ' IPCOD',' PRATE','  RH',' SFCP',' CCVR'
     &         /66(' -'))
C Unused: 9026 FORMAT (1X, 3(I2.2,1X),I3,1X,I2.2,1X,F6.1,1X,3(F6.3,1X),
C     &        2(F5.0,1X),F8.1,1X,F5.3,1X,2(F6.2,1X),F7.2,1X,F5.0,
C     &        3(1X,F6.1) )
98026 FORMAT ( 1X,3(I2.2,1X),I2.2,1X,F6.1,1X,3(F6.3,1X),
     &        2(F5.0,1X),F8.1,1X,F5.3,2F5.2,1X,F7.2,1X,F5.0,
     &        1X,F4.0,1X,F6.1,1X,F4.0,I3,F7.2,F6.0,F6.0,I3,F7.3)

99011 FORMAT(/1X,'*** SUMMARY OF THE SAMPLED PROFILE ',
     &       'METEOROLOGICAL DATA USED WITH THE SCIM OPTION ***'/)
99016 FORMAT(1X,'Profile file:   ',A80,
     &     /,1X,'Profile format: ',A105)
99020 FORMAT(1X,'SURFACE STATION NO.: ',I6,20X,
     &       'UPPER AIR STATION NO.: ',I6/16X,'NAME: ',A40,3X,
     &       'NAME: ',A40/16X,'YEAR: ',I6,37X,'YEAR: ',I6/)
99025 FORMAT ( ' YR', ' MO', ' DY', ' HR', ' HEIGHT', ' F', '  WDIR',
     &         '    WSPD',' AMB_TMP', ' sigmaA', '  sigmaW' ,
     &         /29(' -'))
99026 FORMAT (1X, 4(I2.2,1X),F6.1,1X,I1,1X,F5.0,1X,F7.2,1X,F7.1,1X,
     &        F6.1,1X,F7.2)


      RETURN
      END


      SUBROUTINE PFLCNV( LEVEL )
C***********************************************************************
C             PFLCNV Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Converts data in profile file to the required units
C
C        PROGRAMMER: Jim Paumier, PES, Inc.
C
C        DATE:    September 30, 1993
C
C        INPUTS:  Hourly profile data
C
C        OUTPUTS: Hourly profile data converted to required units
C
C        Revisions:
C                    R. Brode, PES, Inc.                  25 Feb 2002
C                    Modify upper limit on temperature from 900 to 90
C                    so that a value of 99.0 will be treated as
C                    missing
C
C                    R. Brode, PES, Inc.                  22 Jan 1998
C                    Check for wind direction > 900. and recode
C                    as missing (-999.0).
C
C                    J. Paumier, PES, Inc                 16 Dec 1994
C                    Fixed the logic in determining if ambient
C                    temperature is missing in the conversion from
C                    Celsius to kelvin
C
C        ASSUMPTIONS:
C
C        CALLED FROM:  METEXT
C***********************************************************************

C---- Variable Declarations
C
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      LOGICAL          :: L_Turb_Warn
      INTEGER          :: LEVEL, N_Got_SigA, N_Got_SigW, N_Turb_Warn
      DOUBLE PRECISION :: SIGRAD, EPSIL, USUBV

C
C---- Variable Initializations
C
      DATA N_Got_SigA/0/, N_Got_SigW/0/, N_Turb_Warn/0/
      DATA L_Turb_Warn/.FALSE./
      MODNAM = 'PFLCNV'
C-----------------------------------------------------------------------

C     JAT 1/29/21 D070 TURBULENCE OPTIONS
C     RESET PFLSA AND/OR PFLSW BASED ON TURBULENCE OPTIONS
      WRITE(DUMMY,'(I10.10)') FULLDATE
      
      IF (PFLSA(LEVEL) .LT. 99.0D0 .AND. (TURBOPTS(1) .OR. 
     &    (TURBOPTS(2) .AND. STABLE) .OR. (TURBOPTS(3) .AND. UNSTAB) 
     &   .OR. TURBOPTS(4) .OR. (TURBOPTS(6) .AND. STABLE) .OR. 
     &    (TURBOPTS(8) .AND. UNSTAB))) THEN
          PFLSA(LEVEL)=99.0D0
          IF (.NOT. RESET_SA) RESET_SA=.TRUE.
          
      ENDIF
      IF (PFLSW(LEVEL) .LT. 99.0D0 .AND. (TURBOPTS(1) .OR. 
     &    (TURBOPTS(2) .AND. STABLE) .OR. (TURBOPTS(3) .AND. UNSTAB) 
     &   .OR. TURBOPTS(5) .OR. (TURBOPTS(7) .AND. STABLE) .OR. 
     &    (TURBOPTS(9) .AND. UNSTAB))) THEN
          PFLSW(LEVEL)=99.0D0
          IF (.NOT. RESET_SW) RESET_SW=.TRUE.
          
      ENDIF
          
C     Change the missing value indicator for wind speed to -99.0

      IF( PFLWS(LEVEL)  .LT. 0.0D0  .OR.
     &    PFLWS(LEVEL)  .GT. 90.0D0 )THEN
          PFLWS(LEVEL) =  -99.0D0
      ENDIF

C     Change the wind direction from 0.0 to 360.0 if wind speed is nonzero

      IF(( PFLWS(LEVEL) .GT. 0.0D0 .AND. PFLWS(LEVEL) .LE. 90.0D0 ).AND.
     &                                   PFLWD(LEVEL) .EQ. 0.0D0 )THEN
         PFLWD(LEVEL) = 360.0D0
      ENDIF
      
      IF( PFLWD(LEVEL) .GT. 900.0D0 .OR. PFLWD(LEVEL) .LT. 0.0D0 )THEN
         PFLWD(LEVEL) = -999.0D0
      ENDIF

      IF( PFLWS(LEVEL) .EQ. 0.0D0  .AND.
     &    PFLWD(LEVEL) .EQ. 0.0D0 )THEN
         PFLWS(LEVEL) = -99.0D0
         PFLWD(LEVEL) = -999.0D0
      ENDIF

C     Compute sigmaV from nonmissing wind speed and sigmaTHETA
      IF( PFLWS(LEVEL) .GT. 0.0D0  .AND.
     &    PFLSA(LEVEL) .GE. 0.0D0  .AND.
     &    PFLSA(LEVEL) .LT. 99.0D0 )THEN
            SIGRAD = PFLSA(LEVEL) * DTORAD
            EPSIL = DSIN(SIGRAD) * ( 1.0D0 - GSIGV * SIGRAD )           ! GSIGV = 0.073864D0 (in MODULES.f)
            USUBV = PFLWS(LEVEL) * DSQRT( 1.0D0 - EPSIL*EPSIL )
            PFLSV(LEVEL) = SIGRAD * USUBV
C ---       Compare to minimum value PARAMETER, SVMIN
            PFLSV(LEVEL) = MAX( SVMIN, PFLSV(LEVEL) )
            L_Got_SigA = .TRUE.
            N_Got_SigA = N_Got_SigA + 1
      ELSE
         PFLSV(LEVEL) = -99.0D0
      ENDIF

C --- Check for sigmaW data
      IF( PFLSW(LEVEL) .GT. 0.0D0 .AND.
     &    PFLSW(LEVEL) .LT. 90.0D0 )THEN
         L_Got_SigW = .TRUE.
         N_Got_SigW = N_Got_SigW + 1
      ENDIF

C --- Check for turbulence data with ADJ_U
      IF( L_AdjUstar .AND. DFAULT )THEN
C ---    Issue FATAL error if observed turbulence data are
C        used with L_AdjUstar with RegDFAULT option
         IF( L_Got_SigA .AND. L_Got_SigW )THEN
            IF( N_Got_SigA .EQ. 1 .AND. N_Got_SigW .EQ. 1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','401','SigA & SigW')
               FATAL = .TRUE.
            ENDIF
         ELSEIF( L_Got_SigA .AND. .NOT.L_Got_SigW )THEN
            IF( N_Got_SigA .EQ. 1 ) THEN
               CALL ERRHDL(PATH,MODNAM,'E','401','SigA Data')
               FATAL = .TRUE.
            ENDIF
         ELSEIF( .NOT.L_Got_SigA .AND. L_Got_SigW )THEN
            IF( N_Got_SigW .EQ. 1 ) THEN
               CALL ERRHDL(PATH,MODNAM,'E','401','SigW Data')
               FATAL = .TRUE.
            ENDIF
         ENDIF
      ELSEIF( L_AdjUstar )THEN
C ---    Issue Warning error if observed turbulence data are used
C        with L_AdjUstar and Non-DFAULT; limit to one warning
         IF( L_Got_SigA .AND. L_Got_SigW )THEN
            IF( N_Got_SigA .EQ. 1 .AND. N_Got_SigW .EQ. 1) THEN
               CALL ERRHDL(PATH,MODNAM,'W','401','SigA & SigW')
C ---          Assign character string regarding use of turbulence
C              data on MODOPS string
               MODOPS(1)  = 'NonDFAULT'
               MODOPS(26) = 'SigA&SigW'
            ENDIF
         ELSEIF( L_Got_SigA .AND. .NOT.L_Got_SigW )THEN
            IF( N_Got_SigA .EQ. 1 ) THEN
               CALL ERRHDL(PATH,MODNAM,'W','401','SigA Data')
C ---          Assign character string regarding use of turbulence
C              data on MODOPS string
               MODOPS(1)  = 'NonDFAULT'
               MODOPS(26) = 'SigA Data'
            ENDIF
         ELSEIF( .NOT.L_Got_SigA .AND. L_Got_SigW )THEN
            IF( N_Got_SigW .EQ. 1 ) THEN
               CALL ERRHDL(PATH,MODNAM,'W','401','SigW Data')
C ---          Assign character string regarding use of turbulence
C              data on MODOPS string
               MODOPS(1)  = 'NonDFAULT'
               MODOPS(26) = 'SigW Data'
            ENDIF
         ENDIF
         IF( .NOT. L_Turb_Warn .and. (L_Got_Siga .or. L_Got_SigW) )THEN
            L_Turb_Warn = .TRUE.
            N_Turb_Warn = N_Turb_Warn + 1
            IF( N_Turb_Warn .EQ. 1) THEN
               CALL ERRHDL(PATH,MODNAM,'W','402','Option')
            ENDIF
         ENDIF
      ELSE
C ---    ADJ_U* is NOT being used, however include use of turbulence,
C ---    SigA and/or SigW in the MODOPS array even with without ADJ_U*
         IF( L_Got_SigA .AND. L_Got_SigW )THEN
            IF( N_Got_SigA .EQ. 1 .AND. N_Got_SigW .EQ. 1) THEN
               CALL ERRHDL(PATH,MODNAM,'W','403','SigA & SigW')
C ---          Assign character string regarding use of turbulence
C              data on MODOPS string
               MODOPS(26) = 'SigA&SigW'
            ENDIF
         ELSEIF( L_Got_SigA .AND. .NOT.L_Got_SigW )THEN
            IF( N_Got_SigA .EQ. 1 ) THEN
               CALL ERRHDL(PATH,MODNAM,'W','403','SigA Data')
C ---          Assign character string regarding use of turbulence
C              data on MODOPS string
               MODOPS(26) = 'SigA Data'
            ENDIF
         ELSEIF( .NOT.L_Got_SigA .AND. L_Got_SigW )THEN
            IF( N_Got_SigW .EQ. 1 ) THEN
               CALL ERRHDL(PATH,MODNAM,'W','403','SigW Data')
C ---          Assign character string regarding use of turbulence
C              data on MODOPS string
               MODOPS(26) = 'SigW Data'
            ENDIF
         ENDIF

CJAT ISSUE D030: COMMENT OUT DUPLICATE CODE BELOW
C         IF SIGMA-A IS INCLUDED, THE CALCULATION
C         HAS ALREADY BEEN DONE OUTSIDE THIS IF/ELSE
C ---    Make calculation                                            ! 20190709- Poss. duplicate code - Review block below
!         IF( PFLSA(LEVEL) .GT. 0.0D0  .AND.
!     &       PFLSA(LEVEL) .LT. 99.0D0 )THEN   !! May need to reconsider upper bound for SigA
!            SIGRAD = PFLSA(LEVEL) * DTORAD
!            EPSIL = DSIN(SIGRAD) * ( 1.0D0 - GSIGV * SIGRAD )        ! GSIGV = 0.073864D0 (in MODULES.f)
!            USUBV = PFLWS(LEVEL) * DSQRT( 1.0D0 - EPSIL*EPSIL )
!            PFLSV(LEVEL) = SIGRAD * USUBV
!C ---       Compare to minimum value PARAMETER
!            PFLSV(LEVEL) = MAX( SVMIN, PFLSV(LEVEL) )
!         ENDIF                                                       ! 20190709- End review
CJAT  ISSUE D030: END COMMENT OUT
      ENDIF

C     Convert temperature from degrees Celsius to kelvins

      IF( PFLTA(LEVEL)  .GT. -90.0D0  .AND.
     &    PFLTA(LEVEL)  .LT.  90.0D0 )THEN
         PFLTA(LEVEL) = PFLTA(LEVEL) + DCTODK

      ELSE
         PFLTA(LEVEL) = -999.0D0

      ENDIF

C     Change the missing value indicator for sigmaW to -99.0

      IF( PFLSW(LEVEL)  .LT. 0.0D0  .OR.
     &    PFLSW(LEVEL)  .GT. 90.0D0 )THEN
         PFLSW(LEVEL) =  -99.0D0
      ELSE
C        Compare to minimum value PARAMETER, SWMIN = 0.02
         PFLSW(LEVEL) = MAX( SWMIN, PFLSW(LEVEL) )
      ENDIF

      RETURN
      END

      SUBROUTINE PFLINI ()
C***********************************************************************
C             PFLINI Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Initializes the observed profile arrays to missing
C
C        PROGRAMMER: Jim Paumier, PES, Inc.
C
C        DATE:    September 30, 1993
C
C        INPUTS:  None
C
C        OUTPUTS: Initialized observed profile arrays
C
C        ASSUMPTIONS:
C
C        CALLED FROM:  METEXT
C***********************************************************************
C
C---- Variable Declarations
C
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
C Unused:      INTEGER :: I

C
C---- Variable Initializations
C
      MODNAM = 'PFLINI'
      PATH   = 'MX'

C.......................................................................
C     Initialize arrays (1:MXGLVL)
      IFLAG(:)  = 0
      PFLHT(:)  = -99.0D0
      PFLWS(:)  = -99.0D0
      PFLWD(:)  = -99.0D0
      PFLTA(:)  = -99.0D0
      PFLSA(:)  = -99.0D0
      PFLSW(:)  = -99.0D0
      PFLSV(:)  = -99.0D0
      PFLTG(:)  = -99.0D0
      PFLTGZ(:) = -99.0D0

      RETURN
      END

      SUBROUTINE ZIAVER ( NLVLS,HTS,PARRAY,ZI,NDXBLW,PBLAVG,VALZI )
C***********************************************************************
C             ZIAVER Module of the AMS/EPA Regulatory Model - AERMOD
C
C   Purpose:     To compute the average value of the parameter between
C                the surface and the mixing height
C
C   Input:       Number of levels in the profile (NLVLS)
C                Array of gridded profile heights (HTS)
C                Parameter array (PARRAY)
C                Boundary layer height (ZI) (or stack height, if higher)
C                Index of the level gridded profile height immediately
C                   below ZI (NDXBLW)
C                Value of parameter at ZI (VALZI)
C
C   Output:      Average value of parameter in boundary layer (PBLAVG);
C
C   Called by:   METEXT
C
C   Assumptions: If the mixing height (ZI) is above the highest
C                profile height (5000 m), then we assume the profile
C                is constant (= PARRAY(NLVLS)) above ZI and compute
C                the average accordingly.
C
C
C   Programmer:  Jim Paumier PES, Inc.
C
C   Date:        September 30, 1993
C
C   Revision history:
C                <none>
C
C   Reference(s): Inhomgeneous Boundary Layer, A. Venkatram,
C                 June 25, 1993 (GOLF document #5)
C
C***********************************************************************
C
C---- Variable declarations
C
      IMPLICIT NONE

      INTEGER   NDXBLW, NLVLS, I
      DOUBLE PRECISION  HTS(NLVLS), PARRAY(NLVLS), ZI, SUM, PBLAVG, 
     &                  VALZI
C
C---- Data dictionary
C
C---- Data initializations
C
C.......................................................................

      SUM = 0.0D0

C---- Sum over each layer of the gridded profile (PARRAY) to the level
C     immediately below ZI

      DO I = 2, NDXBLW
         SUM = SUM + (HTS(I) - HTS(I-1)) * 0.5D0 *
     &                   (PARRAY(I) + PARRAY(I-1))
      END DO

C---- Finish the summation

      IF( NDXBLW .LT. NLVLS )THEN
C------- Add the area between the level below ZI and ZI to the
C        sum and compute the average.

         SUM = SUM + (ZI - HTS(NDXBLW) ) * 0.5D0 *
     &               (VALZI + PARRAY(NDXBLW) )
         PBLAVG = SUM / ZI

      ELSE
C----    ZI is above the top level (5000 m), assume the parameter is
C        constant above that level and sum accordingly and compute
C        the average
         SUM = SUM + (ZI - HTS(NLVLS)) * PARRAY(NLVLS)
         PBLAVG = SUM / ZI
      ENDIF

      RETURN
      END

      SUBROUTINE GINTRP ( HTBELO,VBELOW, HTABOV,VABOVE, REQDHT,VALUE )
C***********************************************************************
C             GINTRP Module of the AMS/EPA Regulatory Model - AERMOD
C
C   Purpose:     A generalized interpolation routine
C
C   Input:       Height below the required height (HTBELO)
C                Value below the required height (VBELOW)
C                Height above the required height (HTBELO)
C                Value above the required height (VBELOW)
C                Height at which a value is required (REQDHT)
C
C   Output:      Value of the parameter at the required level (VALUE)
C
C   Called by:   Utility routine called by many modules
C
C   Assumptions:
C
C   Programmer:  Jim Paumier, PES, Inc.
C
C   Date:        September 30, 1993
C
C   Revision history:
C                <none>
C
C   Reference(s):
C
C***********************************************************************
C
C---- Variable declarations
C
      IMPLICIT NONE
      DOUBLE PRECISION :: VALUE, HTBELO, VBELOW, HTABOV, VABOVE, REQDHT
C
C---- Data dictionary
C
C---- Data initializations
C
C.......................................................................
C
C---- Interpolate

      VALUE = VBELOW + ( (REQDHT - HTBELO) / (HTABOV - HTBELO) ) *
     &                   (VABOVE - VBELOW)

      RETURN
      END

      SUBROUTINE URBCALC
C***********************************************************************
C             URBCALC Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Calculates Parameters for Urban Stable Boundary Layer
C
C        PROGRAMMER: Roger Brode, PES, Inc.
C
C        DATE:    June 11, 1996
C
C
C        MODIFIED:  Calculate an urban ustar by setting equivalence
C                   between convective sigma-w based on urban wtar
C                   and mechanical sigma-w based on urban ustar at
C                   a height of 7 times the urban roughness length.
C                   R.W. Brode, PES, Inc., - 06/10/02
C
C        MODIFIED:  To set the value for Z_iuo at 400m instead of
C                   500m in calculation of ZIURB, based on observed data.
C                   R.W. Brode, PES, Inc., - 04/08/02
C
C        INPUTS:  Meteorological Variables for One Hour
C
C        OUTPUTS: Urban Mixing Height, Heat Flux, and "Convective
C                 Velocity Scale"
C
C        ASSUMPTIONS:  <none>
C
C        CALLED FROM:  METEXT
C***********************************************************************

C---- Variable declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
C     JAT D065 8/9/21, RHO CALCULATED BUT NOT USED
C      DOUBLE PRECISION :: DELTUR, URBHF, RHO, HT7Z0
      DOUBLE PRECISION :: DELTUR, URBHF, HT7Z0, RHO
      DOUBLE PRECISION :: URBOBLSAV, URBUSTRSAV
C---- Assign the reference mixing height, REFZI, based on a reference
C     population of 2 million. 
!     CP     = specific heat capacity of dry air
      DOUBLE PRECISION, PARAMETER :: REFZI = 400.0D0, CP = 1004


C---- Variable initializations
      MODNAM = 'URBCALC'

C     Save Rural values of USTAR and OBULEN
      RURUSTR   = USTAR
      RUROBULEN = OBULEN

C     Loop Through Urban Areas
      DO IURB = 1, NUMURB

C        Compute Urban-Rural Temperature Difference, DELTUR; 
CRWB     DELTUR = DELTRUR * (0.1046 * LOG(URBPOP/REFPOP) + 0.9983)
CRWB     using DELTRUR = 12.0 K for the reference population 
CRWB     (REFPOP) of 2.0D+6 (assigned in MODULE MAIN1).
CRWB     Use rounded values for parameters
         DELTUR = DELTRUR * (0.1D0 * DLOG(URBPOP(IURB)/REFPOP) + 1.0D0)

         IF (STABLE) THEN
C ---       Compute Urban Convective Mixing Height
            ZIURB(IURB) = REFZI * (URBPOP(IURB)/REFPOP) ** 0.25D0
            L_MorningTrans(IURB) = .FALSE.
         ELSE IF (L_UrbanTransition) THEN 
C ---       Compute Urban pseudo-Convective Mixing Height for morning transition
            ZIURB(IURB) = REFZI * (URBPOP(IURB)/REFPOP) ** 0.25D0
C ---       Check for ZICONV > ZIURB; if so then disable morning transition
            IF (ZICONV .GT. ZIURB(IURB)) THEN
               L_MorningTrans(IURB) = .FALSE.
               CYCLE
            ELSE
               L_MorningTrans(IURB) = .TRUE.
            END IF
         END IF

C     JAT D065 8/9/21, RHO CALCULATED BUT NOT USED
         RHO    = 101325.D0/(287.04D0*TA)

C        Compute Urban Heat Flux, and recalculate Monin-Obukhov length
         URBHF  = 0.03D0 * DELTUR * USTAR * RHO * CP


C        Compute Urban WSTAR
         URBWSTR(IURB) = ((G/TA/RHO/CP) * URBHF * ZIURB(IURB)) ** THIRD


C        Compute Urban USTAR; first set height for equivalence between
C        convective and mechanical sigma-w as 7 times the maximum of the
C        rural and urban surface roughness length.
         HT7Z0 = 7.0D0* MAX( URBZ0(IURB), SFCZ0 )
         URBUSTRSAV = URBWSTR(IURB) *
     &             DSQRT(1.6D0*(HT7Z0/ZIURB(IURB))**(2.0D0*THIRD))/
     &            (1.3D0*DSQRT(1.0D0-HT7Z0/MAX(ZIURB(IURB),ZIMECH)))
         URBUSTR(IURB) = MAX( USTAR, URBUSTRSAV)

C        Compute equivalent Monin-Obukhov length
         URBOBULEN(IURB) = -((RHO*CP*TA*URBUSTR(IURB)**3)/
     &                      (0.4D0*G*URBHF))
         URBOBLSAV = URBOBULEN(IURB)

!RCO D095, compare compute MOL to original MOL, pick the more 
!    neutral for nighttime
!CRCO D120 Check for urban transition, use most convective value
         IF (L_MorningTrans(IURB)) THEN 
!During morning transition hours, RUROBULEN is already negative, so most
!convective value will be maximum of the two negative values
            URBOBULEN(IURB) = max(URBOBULEN(IURB),RUROBULEN)
         ELSE 
!During stable conditions, RUROBULEN is positive, while URBOBULEN. Take the 
!largest of the absolute value of the two to get the most neutral value. 
!Using the positive values to replace downstream calculations in various places
!that were making the convective URBOBULEN stable/neutral.
             URBOBULEN(IURB) = max(abs(URBOBULEN(IURB)),RUROBULEN)
         END IF

CRCO D095 Added for urban debug 8/3/2021
         IF (URBDBUG) THEN
          WRITE(URBUNT,3333) IURB,IYEAR,IMONTH,IDAY,IHOUR,
     &                       URBOBULEN(IURB),URBOBLSAV,RUROBULEN,
     &                       URBUSTR(IURB),URBUSTRSAV,
     &                       USTAR,DELTUR,ZIURB(IURB),ZIMECH,ZICONV,
     &                       URBPOP(IURB),URBHF,URBWSTR(IURB),WSTAR,
     &                       TA,UREF,BOWEN,
     &                       STABLE,L_MorningTrans(IURB)
 3333      FORMAT(1X,5(2X,I2),17(F12.2),(7X,L2),(9X,L2))        
         ENDIF
! End URBDBUG insert
      END DO

      RETURN
      END

      SUBROUTINE GRDURBAN
C***********************************************************************
C             GRDURBAN Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Computes Urban Profiles
C
C        PROGRAMMER: Roger Brode, PES, Inc.
C
C        DATE:    June 11, 1996
C
C        MODIFIED:   Changed subroutine name from GRDURB to GRDURBAN.
C                    R. Brode, PES, 11/21/97
C
C        INPUTS:  Meteorological Variables for One Hour
C
C        OUTPUTS: Urban Profiles of sigma-v, sigma-w, Tlz, VPTG
C
C        ASSUMPTIONS:  <none>
C
C        CALLED FROM:  METEXT
C***********************************************************************

C---- Variable declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      INTEGER :: I
      DOUBLE PRECISION :: ZDCRS, SV2, SVURB, SV2DCR, VAL2, ATZI, SW2, 
     &                    SWURB

C---- Variable initializations
      MODNAM = 'GRDURBAN'

C     Save Rural Profiles

C     Save gridded profile arrays to 'rural' arrays (1:MXGLVL)
      GRDSVR(:) = GRIDSV(:)
      GRDSWR(:) = GRIDSW(:)
      GRDTGR(:) = GRIDTG(:)
      GRDPTR(:) = GRIDPT(:)

C     Loop Through Urban Areas
      DO IURB = 1, NUMURB

         IF (.NOT.STABLE .AND. .NOT.L_MorningTrans(IURB)) CYCLE
         
         ZDCRS  =  AT1PT2 * ZIURB(IURB)

C        Loop Through Grid Levels
         DO I = 1, MXGLVL

            SV2 = 0.35D0 * URBWSTR(IURB)**2
C
            IF( GRIDHT(I)  .LE.  ZIURB(IURB) )THEN
               SVURB = DSQRT( SV2 )

            ELSEIF( GRIDHT(I) .GT. ZIURB(IURB) .AND.
     &              GRIDHT(I) .LE. ZDCRS )THEN
C              COMPUTE sigmaV at 1.2*ZI
               SV2DCR = MIN( SV2, 0.25D0 )
C              INTERPOLATE between value of SV2 at ZI and at 1.2*ZI
               CALL GINTRP ( ZIURB(IURB), SV2, ZDCRS, SV2DCR, GRIDHT(I),
     &                       VAL2 )
               SVURB = DSQRT( VAL2 )

            ELSE   ! requested height is above 1.2*urban mixing height
               ATZI  = DSQRT( SV2 )
               SVURB = MIN( ATZI, 0.5D0 )

            ENDIF
C

            IF( GRIDHT(I)  .LE.  0.1D0*ZIURB(IURB) )THEN
               SW2 = 1.6D0 * ( GRIDHT(I)/ZIURB(IURB) )**(2.0D0*THIRD) *
     &                       URBWSTR(IURB)**2
               SWURB  = DSQRT( SW2 )

            ELSEIF( GRIDHT(I).GT.0.1D0*ZIURB(IURB) .AND.
     &              GRIDHT(I).LE.ZIURB(IURB) )THEN
               SWURB = DSQRT( 0.35D0 * URBWSTR(IURB)**2 )

            ELSE   ! requested height is above urban mixing height
               SW2 = 0.35D0 * URBWSTR(IURB)**2 *
     &               DEXP(-(6.D0*(GRIDHT(I)-ZIURB(IURB))/ZIURB(IURB)))
               SWURB = DSQRT( SW2 )

            ENDIF
C
            GRDSVU(I,IURB) = DSQRT(GRIDSV(I)**2 + SVURB**2)
            GRDSWU(I,IURB) = DSQRT(GRIDSW(I)**2 + SWURB**2)


CRCO D095 Added for urban debug 8/3/2021
         IF (URBDBUG) THEN
            IF (I .LT. 17) THEN 
              WRITE(URBUNT1,3333) IURB,IYEAR,IMONTH,IDAY,IHOUR,I,
     &                       GRIDHT(I),
     &                       GRIDSV(I),SVURB,GRDSVU(I,IURB),
     &                       GRIDSW(I),SWURB,GRDSWU(I,IURB)
 3333          FORMAT(1X,6(2X,I2),9(F10.2))        
            ENDIF
         ENDIF
! End URBDBUG insert

            IF (GRIDHT(I) .LE. ZIURB(IURB)) THEN
               GRDTGU(I,IURB) = 1.0D-5
            ELSE
               GRDTGU(I,IURB) = GRIDTG(I)
            END IF

         END DO

      END DO

C---- Compute potential temperature profile from urban Dtheta/Dz profile
      CALL GRDPTURB

      RETURN
      END

      SUBROUTINE GRDPTURB
C=======================================================================
C                GRDPTURB module of the AERMOD Dispersion Model
C
C   Purpose:     To construct a profile of gridded values of
C                potential temperature for URBAN cases
C
C   Input:       Profile of gridded potential temperature gradients
C                Temperature at the reference height
C                Profile of grid heights
C
C   Output:      Potential temperature profile at the grid heights.
C
C   Assumptions: There is at least one grid level below the reference
C                temperature height (which should be satisfied
C                because the lowest grid level is 0.5 meters)
C
C   Called by:   METEXT
C
C   Programmer:  Jim Paumier                          30 Sept 1993
C                Pacific Environmental Services
C
C   Revision history:
C        12/10/97  - R. Brode, PES, Inc.
C                    Corrected the order of array indices used for profiling
C                    potential temperature above the reference height.
C        12/16/94  - J. Paumier, PES, Inc.
C                  - CALL LOCATE to get the number of levels below the
C                    temperature reference height, replacing the original
C                    method which relied on grid heights being every 10 m
C
C-----------------------------------------------------------------------
C
C---- Variable declarations

      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      INTEGER :: L, NBELOW
      DOUBLE PRECISION :: PTREF

C---- Data definitions
C
C
C---- Data initializations
      MODNAM = 'GRDPTURB'
C
C
C.......................................................................
C

C---- Determine the grid level below the temperature reference
C     height (as defined in the scalar file)               ---- CALL LOCATE

      CALL LOCATE( GRIDHT, 1, MXGLVL, TREFHT, NBELOW )

C---- Compute the potential temperature at the reference level
C     using the reference temperature (TA), the reference
C     temperature height (TREFHT), and the base elevation
C     of the measurement site specified on the ME PROFBASE
C     keyword (ZBASE)

      PTREF = TA + GOVRCP * (TREFHT + ZBASE)

C---- Loop Through Urban Areas
      DO IURB = 1, NUMURB

         IF (.NOT.STABLE .AND. .NOT.L_MorningTrans(IURB)) CYCLE
         
C----    Compute the potential temperature at the grid level below
C        the temperature reference height

         GRDPTU(NBELOW,IURB) = PTREF -
     &          0.5D0 * (GRDTGU(NBELOW+1,IURB) + GRDTGU(NBELOW,IURB)) *
     &          (TREFHT - GRIDHT(NBELOW))


C----    Compute Potential Temp Values for Grid Levels Below Reference Ht.
         DO L = NBELOW-1, 1, -1

            GRDPTU(L,IURB) = GRDPTU(L+1,IURB) - 0.5D0 *
     &                      (GRDTGU(L+1,IURB) + GRDTGU(L,IURB)) *
     &                      (GRIDHT(L+1) - GRIDHT(L))

         END DO


C----    Compute Potential Temp Values for Grid Levels Above Reference Ht.
         DO L = NBELOW+1, MXGLVL

            GRDPTU(L,IURB) = GRDPTU(L-1,IURB) + 0.5D0 *
     &                      (GRDTGU(L,IURB) + GRDTGU(L-1,IURB)) *
     &                      (GRIDHT(L) - GRIDHT(L-1))

         END DO

      END DO

      RETURN
      END

CCRFL
CCRFL  Subroutine METDEB added to improve debug output of meteorological 
CCRFL  data.
CCRFL

      SUBROUTINE METDEB
C***********************************************************************
C             METDEB Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Writes debug output of this hour's meteorological 
C                 data.
C
C        PROGRAMMERS:  Bob Paine (developer) and Russ Lee (implementation)
C
C        DATE:    August 18, 1994;  Revised September 26, 1994.
C
C        MODIFIED:   To include value for Theta* (THSTAR) in the METEOR
C                    debug option.
C                    R. W. Brode, U.S. EPA/OAQPS/AQMG, 06/30/2015
C
C        INPUTS:  Meteorological data input to model
C
C        OUTPUTS: Meteorological data output to debug file
C
C        ASSUMPTIONS:  None
C
C        CALLED FROM:  HRLOOP
C***********************************************************************

C---- Variable declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      INTEGER :: I, NDIST

C---- Variable initializations
      MODNAM = 'METDEB'

      IF (METHDR) THEN
C----    Modified to use 4-digit year (IYR) instead of 2-digit year (IYEAR)
         WRITE (DBMUNT, 6115) IYR, IMONTH, IDAY, IHOUR, ZI, TA, USTAR,
     &   WSTAR, OBULEN, SFCZ0, THSTAR, UAVG, SVAVG, SWAVG, UATZI, 
     &   SVATZI, SWATZI, VPTGZI
         WRITE (DBMUNT, 6118)
         DO I = MXGLVL, 1, -1
           WRITE (DBMUNT, 6120) I, GRIDHT(I), GRIDWD(I), GRIDWS(I),
     &     GRIDSV(I), GRIDSW(I), GRIDPT(I), GRIDTG(I)
         END DO
         WRITE (DBMUNT, 6116)
         METHDR = .FALSE.
      ENDIF

C --- Adjust for distances larger than output field
      IF( DABS(X) .GT. 999999.0D0 )THEN
         IF( X .LT. 0.0D0 )THEN
            NDIST = -999999
         ELSE
            NDIST =  999999
         ENDIF
      ELSE
         NDIST = IDNINT(X)
      ENDIF

      IF( STABLE  .OR.  (UNSTAB .AND. (HS .GE. ZI) ) )THEN
         WRITE (DBMUNT, 6131) IREC, NDIST, UEFF, SVEFF, SWEFF
      ELSE IF(PPF .GE. 1.0D0) THEN
         WRITE (DBMUNT, 6132) IREC, NDIST, UEFF3, SVEFF3, SWEFF3
      ELSE IF(PPF .LE. 0.0D0) THEN
         WRITE (DBMUNT, 6133) IREC, NDIST, UEFFD, SVEFFD, SWEFFD,
     &         UEFFN, SVEFFN, SWEFFN
      ELSE
         WRITE (DBMUNT, 6134) IREC, NDIST, UEFFD, SVEFFD, SWEFFD,
     &         UEFFN, SVEFFN, SWEFFN, UEFF3, SVEFF3, SWEFF3
      ENDIF    
C
 6115 FORMAT( 1X, 80('-'),//,'  SURFACE AND PROFILE MET DATA:',/,
     1  T48, 'MONIN-     SFC',/,T17,
     2  'MIXING   TEMP                  OBUKHOV   ROUGH.',/,T17,
     3  'HEIGHT   @ HS    U*      W*    LENGTH    LENGTH    THSTAR',/,
     4  '  YR  MO DA HR',
     5  '    (M)    (K)   (M/S)   (M/S)    (M)       (M)',//,
     6  1X,I4,3I3,2X,F6.1,2X,F5.1,1X,F6.3,1X,F7.3,2X,F7.1,3X,F7.3,3X,
     6                                                       F7.4///,
     7  ' <--AVG: SFC TO ZI---> <--------VALUE AT ZI-------->',/,
     9  '   U    SIG-V  SIG-W      U    SIG-V  SIG-W   VPTG',/,
     B  ' (M/S)  (M/S)  (M/S)    (M/S)  (M/S)  (M/S)   (K/M)',//,
     D  1X,F5.2,2(2X,F5.2),3X,F5.2,2X,F5.2,2X,F5.2,
     E  1X,F7.4,//)
 6116 FORMAT(//,1X, '            <-STABLE/DIRECT EFF. VALUES-> ',
     & '<-INDIRECT EFF. VALUES-> <-PENETRATED EFF. VALUES->', 
     & /,' RECEPT  DIST.    U    SIG-V  SIG-W   ',
     & '       U    SIG-V  SIG-W          U    SIG-V  SIG-W',/)
CRJP 6117 FORMAT (1X,I5,1X,F6.0,3(2X,F5.2),1X,F7.0,3(2X,F5.2),2(1X,F7.0))
 6118 FORMAT(5X,' GRID     WIND    WIND                    POT.',
     &  /,5X,'HEIGHT    DIR.    SPEED   SIG-V   SIG-W   TEMP.',
     &  '  VPTG',/,
     &  5X,' (M)     (DEG)    (M/S)   (M/S)   (M/S)    (K)  ',
     &  ' (K/M)',/)
 6120 FORMAT (I4,F7.1,2X,F6.1,2X,F7.2,1X,F7.2,1X,F7.2,1X,F6.2,
     &  1X,F9.6)

CRJP  Add new FORMAT statements here.

 6131 FORMAT (I5,1X,I7,1X,3(2X,F5.2))
 6132 FORMAT (I5,1X,I7,1X,54X,3(2X,F5.2))
 6133 FORMAT (I5,1X,I7,1X,2(3(2X,F5.2),6X))
 6134 FORMAT (I5,1X,I7,1X,3(3(2X,F5.2),6X))

      RETURN
      END

      SUBROUTINE GRDEPS
C***********************************************************************
C             GRDEPS Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Computes profile of epsilon, turbulence dissipation
C                 rate, for PVMRM or GRSM option
C
C        PROGRAMMER: Roger Brode, PES, Inc.
C
C        DATE:       May 13, 2002
C
C        INPUTS:
C
C        OUTPUTS:
C
C        ASSUMPTIONS:
C
C        CALLED FROM:  METEXT
C***********************************************************************

C---- Variable declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      INTEGER :: I
      DOUBLE PRECISION :: TSUBLR
      DOUBLE PRECISION, PARAMETER :: AR1 = 0.46D0

C---- Variable initializations
      MODNAM = 'GRDEPS'

C     Loop Through Grid Levels
      DO I = 1, MXGLVL

         TSUBLR = AR1 * ZI/GRIDSW(I)
         GRIDEPS(I) = 0.78D0 * GRIDSW(I)*GRIDSW(I)/TSUBLR

      END DO

      RETURN
      END
