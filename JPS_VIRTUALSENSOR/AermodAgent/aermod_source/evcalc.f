      SUBROUTINE EVCALC
C***********************************************************************
C                 EVCALC Module of AERMOD EVENT Option
C
C        PURPOSE: Controls Flow and Processing of CALCulation Modules
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED: Preparation work for multiple buoyant lines
C                  (Multiple_BuoyLines_D41_Wood)
C
C        MODIFIED: Modified to call RLCALC for RLINE and RLINEXT sources.
C                   Wood, 03/18/2019
C
C        MODIFIED:  Added code to call RLCALC for a RLINE source.
C                   Wood, 7/20/2018
C
C        MODIFIED:  Added code to call BL_CALC, buoyant line source
C                   Amec Foster Wheeler,  03/31/2016
C
C        MODIFIED:  Modified to include user-specified background
C                   concentrations through the SO BACKGRND option.
C                   R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
C
C        MODIFIED:  To set NUMREC = 1 and use PCALC, VCALC, ACALC, and
C                   OCALC subroutines.  R.W. Brode, PES, Inc. - 12/2/98
C
C        INPUTS:  Arrays of Source Parameters
C                 Arrays of Receptor Locations
C                 Meteorological Variables for One Hour
C
C        OUTPUTS: Array of 1-hr CONC or DEPOS Values for Each Source/Receptor
C
C        CALLED FROM:   EVLOOP
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      USE RLINE_DATA, ONLY: RLPROCESSED 
      USE BUOYANT_LINE, ONLY: NUMBLGRPS, BL_GRPID 
      IMPLICIT NONE

      LOGICAL   BLPROCESSED
      INTEGER   KK
      CHARACTER MODNAM*12
      
C     Variable Initializations
      MODNAM = 'EVCALC'
      PATH   = 'CN'

C --- Set the bouyant line source flag to false to indicate that no
C      lines have been processed for this hour since all lines must be
C      processed together
      BLPROCESSED = .FALSE.
      
C --- Set the RLINE source flag to false to indicate that no
C      RLINES have been processed for this hour since rotation of 
C      receptors only needs to happen for first source.
      RLPROCESSED = .FALSE.

C --- Set NUMREC = 1 to allow use of PCALC, VCALC, ACALC, OCALC,
C     ARM2_CALC, OLM_CALC, PVMRM_CALC, GRSM_CALC subroutines for EVENT processing
      NUMREC = 1

C --- Assign IGRP for this event
      IGRP = IDXEV(IEVENT)

C     Assing METHDR = .TRUE. to print source&receptor-independent
C     meteorology debug information to METEOR debug output file.
      METHDR = .TRUE.

C     Begin Source LOOP
      SOURCE_LOOP: DO ISRC = 1, NUMSRC
C ---    Proceed with calcs if ISRC is included in IGRP, or if NO2
C        options are being used since these require full CHI array
         IF (IGROUP(ISRC,IGRP) .EQ. 1 .OR. ARM2 .OR. 
     &                          OLM .OR. PVMRM .OR. GRSM) THEN
            IF (SRCTYP(ISRC)(1:5) .EQ. 'POINT') THEN
C              Calculate Point Source Values                        ---   CALL PCALC
               CALL PCALC
            ELSE IF (SRCTYP(ISRC) .EQ. 'VOLUME') THEN
C              Calculate Volume Source Values                       ---   CALL VCALC
               CALL VCALC
            ELSE IF (SRCTYP(ISRC)(1:4) .EQ. 'AREA' .OR.
     &               SRCTYP(ISRC) .EQ. 'LINE') THEN
C              Calculate AREA/AREAPOLY/AREACIRC/LINE Source Values  ---   CALL ACALC
               CALL ACALC
            ELSE IF ((SRCTYP(ISRC) .EQ. 'RLINE') .OR. 
     &           (SRCTYP(ISRC) .EQ. 'RLINEXT')) THEN
C              Calculate RLINE Source Values                        ---   CALL RLCALC
               CALL RLCALC
               RLPROCESSED = .TRUE.
            ELSE IF (SRCTYP(ISRC) .EQ. 'OPENPIT') THEN
C              Calculate OpenPit Source Values                      ---   CALL OCALC
               CALL OCALC
            ELSE IF (SRCTYP(ISRC) .EQ. 'BUOYLINE' .AND.
     &                                        (.NOT. BLPROCESSED)) THEN
C              Calculate Bouyant Line Source Values                 ---   CALL BL_CALC
C              BLPROCESSED lets AERMOD know that all lines associated with
C               the buoyant line source were processed on the first pass
C               through the sources 
C Multiple_BuoyLines_D41_Wood
C              Determine which BL source group to process
               IF (EVGRP(IEVENT) .EQ. 'ALL') THEN
C                 Process all the buoyant line source group events
                  DO KK = 1,NUMBLGRPS
                     CALL BLEVRECP(IEVENT,KK)
                     CALL BL_CALC(KK)
                  END DO
                  BLPROCESSED = .TRUE.

               ELSE
C                 Process the individual buoyant line source group events
                  DO KK = 1,NUMBLGRPS
                     IF (EVGRP(IEVENT) .EQ. BL_GRPID(KK)) THEN
                        CALL BLEVRECP(IEVENT,KK)
                        CALL BL_CALC(KK)
                        EXIT
                     ENDIF
                  END DO
                  BLPROCESSED = .TRUE.
               END IF                     ! EVGRP(IEVENT) .EQ. 'ALL'
            END IF                        ! SRCTYP(ISRC) .EQ. 'BUOYLINE'
         END IF                           ! IGROUP(ISRC,IGRP) .EQ. 1
      END DO SOURCE_LOOP
C     End Source LOOP

      IF (L_BACKGRND .AND. .NOT.ARM2 .AND.
     &     .NOT.OLM .AND. .NOT.PVMRM .AND. .NOT.GRSM) THEN
C ---    User-specified background concentrations are included; 
C        add to modeled concentrations by source group unless
C        NO2 options are specified (which are handled separately) 
         CALL EV_SUMBACK
      END IF
      
      RETURN
      END

      SUBROUTINE EV_SUMVAL
C***********************************************************************
C                 EV_SUMVAL Module of the AMS/EPA Regulatory Model - AERMOD - EVENT
C
C        PURPOSE: Sums HRVAL to AVEVAL and ANNVAL Arrays
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:  Modified to include GRPAVE variable to account for
C                   user-specified background for the full averaging
C                   period based user-specified background concentrations 
C                   through the SO BACKGRND option.
C                   R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
C
C        INPUTS:  HRVAL - Hourly Value for (IHOUR,ISRC) Combination
C                 Averaging Period Options
C                 Source Groupings
C
C        OUTPUTS: Updated Sums of AVEVAL and ANNVAL Arrays
C
C        CALLED FROM:   PCALC
C                       VCALC
C                       ACALC
C                       OCALC
C                       RLCALC
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'EV_SUMVAL'

      HRVALS(IHOUR,ISRC) = HRVAL(1)
      EV_AVEVAL(ISRC)    = EV_AVEVAL(ISRC) + HRVAL(1)
C --- Update GRPAVE and GRPVAL, using IDXEV to identify 
C     IGRP for this EVENT
      GRPAVE(IDXEV(IEVENT)) = GRPAVE(IDXEV(IEVENT)) + HRVAL(1)
      GRPVAL(IDXEV(IEVENT),IHOUR) = 
     &                  GRPVAL(IDXEV(IEVENT),IHOUR) + HRVAL(1)

      RETURN
      END

      SUBROUTINE EV_SUMBACK
C***********************************************************************
C                 EV_SUMBACK Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Sums Background Values to AVEVAL and ANNVAL Arrays
C
C        PROGRAMMER: Roger Brode
C
C        DATE:       February 28, 2011
C
C        INPUTS:  
C
C        OUTPUTS: Updated Sums of AVEVAL and ANNVAL Arrays
C
C        CALLED FROM:   PCALC
C                       VCALC
C                       ACALC
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      DOUBLE PRECISION :: BCKGRD
      CHARACTER MODNAM*12
c     RCO 7/27/20 ADDED FROM 19191
C     TWO MODIFICATIONS
C     1.  Multiply BGCONC by ratio of EMIFAC/1.0E6 to account for the
C         that background concentrations are in
c         micrograms/m^3 for internal calculations
c         but modeled output may be in other units
c         such as ppb.  This division puts the
c         background in the same units as the modeled
c         concentrations before adding them.
c     2.  Only add background to the concentration output
c         type, i.e. ityp=1.  Background concentration (ug/^3)
c         should not be added to deposition (g/m^2)
C     Variable Initializations
      MODNAM = 'EV_SUMBACK'
      BCKGRD = 0.0D0
      ITYP=1
      BCKGRD = EV_BGCONC(IHOUR)
C     IGRP has been assigned for this EVENT in sub_EVCALC
      IF (GRP_BACK(IDXEV(IEVENT))) THEN
         GRPVAL(IDXEV(IEVENT),IHOUR) = GRPVAL(IDXEV(IEVENT),IHOUR) + 
     &                                 BCKGRD*EMIFAC(ITYP)/1.0D6
         BACKHR(IDXEV(IEVENT),IHOUR) = BACKHR(IDXEV(IEVENT),IHOUR) + 
     &                                 BCKGRD*EMIFAC(ITYP)/1.0D6
         GRPAVE(IDXEV(IEVENT))  = GRPAVE(IDXEV(IEVENT))  + 
     &                            BCKGRD*EMIFAC(ITYP)/1.0D6
         BACKAVE(IDXEV(IEVENT)) = BACKAVE(IDXEV(IEVENT)) + 
     &                            BCKGRD*EMIFAC(ITYP)/1.0D6
      END IF

      RETURN
      END

      SUBROUTINE EVLOOP
C***********************************************************************
C                 EVLOOP Module of AERMOD EVENT Option
C
C        PURPOSE: Controls Main Calculation Loop Through Events
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To remove mixed-mode math in calculation of
C                    IENDHR - 4/19/93
C
C        INPUTS:  Source, Receptor and Setup Options
C
C        OUTPUTS: Update Hourly Results
C
C        CALLED FROM:   MAIN
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: IEVYR, IASTAT
      DOUBLE PRECISION :: GRPAVE_Test
      LOGICAL FOPEN, L_FIRSTCALL
      LOGICAL, ALLOCATABLE :: L_EVT_PROC(:)

C     Variable Initializations
      MODNAM = 'EVLOOP'
      DATA L_FIRSTCALL/.TRUE./
      GRPAVE_Test = 0.0D0

C --- Allocate L_EVT_PROC array to track whether an event has
C     already been processed, and initialize as .FALSE.
      IF( .NOT.ALLOCATED(L_EVT_PROC) )THEN 
         ALLOCATE( L_EVT_PROC(NUMEVE), STAT=IASTAT)
         L_EVT_PROC(:) = .FALSE.
      ENDIF
C --- Initialize other logicals
      EOF   = .FALSE.
      FOPEN = .FALSE.

C     Flush HRVAL, AVEVAL, GRPAVE and GRPVAL    ---   CALL EV_FLUSH
      CALL EV_FLUSH

C --- Assign FULLDATE and IEDATE with hour = 00 to
C     FULL_YYMMDD, IEDATE_YYMMDD local variables, 
C     since hourly events within the same day may 
C     not be in sequence
      FULL_YYMMDD = (FULLDATE/100) * 100
      IEDATE_YYMMDD = (IEDATE/100) * 100

      DAY_LOOP: DO WHILE (FULL_YYMMDD .LT. IEDATE_YYMMDD .AND. .NOT.EOF)
C        Retrieve Hourly Meteorology Data for Current Day   ---   CALL MEREAD
         CALL MEREAD

C ---    Check for runtime errors from MEREAD and exit DAY_LOOP if found
         IF( RUNERR )THEN
            WRITE(*,901) JDAY, IYR
 901        FORMAT('+','MEREAD Error For Day No. ',I4,' of ',I4)
            EXIT DAY_LOOP
         ENDIF

         FULL_YYMMDD = (FULLDATE/100) * 100
         FULLDATE = FULL_YYMMDD + 24

C        Check for Hourly Emissions File
         INQUIRE (UNIT=IHREMI,OPENED=FOPEN)
         IF (FOPEN) THEN
C*          Retrieve Hourly Emissions from File for Current Day---   CALL EV_HRQREAD
C*          Set ILINE = 1 if L_FIRSTCALL for determining whether 
C           VOLUME and AREA source inputs include hourly sigmas
            IF (L_FIRSTCALL) ILINE = 1
            CALL EV_HRQREAD(L_FIRSTCALL)
         END IF

         IF (L_BACKGRND) THEN
C-----      Extract BACKGRND concentrations for the current day, if available
            CALL BGREAD
         END IF

C ---    Check for runtime errors from BGREAD and exit DAY_LOOP if found
         IF( RUNERR )THEN
            WRITE(*,902) JDAY, IYR
 902        FORMAT('+','BGREAD Error For Day No. ',I4,' of ',I4)
            EXIT DAY_LOOP
         ENDIF

         IF (PVMRM .OR. OLM .OR. GRSM) THEN
C-----      Extract Ozone Data for current day; L_FIRSTCALL used 
C           to initialize array of O3 values used to apply minimum 
C           O3 for stable hours
            CALL O3READ
         END IF

        IF (GRSM) THEN
C-----      CERC 11/30/20 Extract NOx Data for current day
            CALL NOXREAD
         END IF

C ---    Check for runtime errors from O3READ and exit DAY_LOOP if found
         IF( RUNERR )THEN
            WRITE(*,903) JDAY, IYR
 903        FORMAT('+','O3READ Error For Day No. ',I4,' of ',I4)
            EXIT DAY_LOOP
         ENDIF

C ---    Set L_FIRSTCALL to .F.
         L_FIRSTCALL = .FALSE.

C        Write Out Update to the Screen for the PC Version
         WRITE(*,909) JDAY, IYR
 909     FORMAT('+','Now Processing Events For Day No. ',I4,' of ',I4)

         EV_LOOP: DO IEVENT = 1, NUMEVE
C ---       Loop through Events and process Events that 
C           occur on this day

C ---       Calculate year of event to account for multiple 
C           year data files
            IEVYR = INT(EVDATE(IEVENT)/1000000)
            IF( L_EVT_PROC(IEVENT) )THEN
C ---          Event has already been processed;
C              cycle to next event
               CYCLE EV_LOOP
            ELSE IF (IEVYR .LT. IYEAR) THEN
C ---          Event year is less than current met year;
C              cycle to next event
               CYCLE EV_LOOP
            ELSE IF (IEVYR .GT. IYEAR) THEN
C ---          Event year is greater than met year;
C              cycle EV_LOOP - event will be processed later
               CYCLE EV_LOOP
            ELSE IF (EVJDAY(IEVENT) .EQ. JDAY) THEN
C ---          Event occurs on this day and year;
C ---          process this event and assign logical 
C              indicating that event has been processed
               L_EVT_PROC(IEVENT) = .TRUE.

               IF( L_BACKGRND )THEN
C ---             Reinitialize BACKAVE array for this event
                  BACKAVE(:) = 0.0D0
               ENDIF
               IF( EVONLY )THEN
C ---             Reinitialize GRPAVE array for this event
                  GRPAVE(:) = 0.0D0
               ENDIF

C ---          Assign start and end hour of the event
               IENDHR = EVDATE(IEVENT) -
     &              INT(EVDATE(IEVENT)/100)*100
               ISTAHR = IENDHR - EVAPER(IEVENT) + 1
              
C ---          Loop through hours for this event
               DO IHOUR = ISTAHR, IENDHR
C ---             Assign IHOUR to KHOUR, used for profile met data
                  KHOUR = IHOUR

C                 Time/Date Marker for DEBUG Output
                  IF (DEBUG) THEN
                     WRITE(DBGUNT,*)
                     WRITE(DBGUNT,*) '--------------------------------',
     &                               '--------------------------------'
                     WRITE(DBGUNT,*) '---  IEVENT, JDAY, IHOUR =  ',
     &                                     IEVENT, JDAY, IHOUR
                     WRITE(DBGUNT,*) '--------------------------------',
     &                               '--------------------------------'
                  END IF

C ---             Assign O3MISS logical for this hour
                  O3MISS = L_AO3MISS(IHOUR)
C ---             Assign NOXMISS logical for this hour
                  NOXMISS = L_ANOXMISS(IHOUR)
C                 Retrieve Hourly Data for Current Event ---   CALL EV_METEXT
                  CALL EV_METEXT
C                 Retrieve Hourly Ozone Value
                  IF (PVMRM .OR. OLM .OR. GRSM) THEN
                     IF (.NOT. O3MISS) THEN
                        O3CONC = EV_O3CONC(IHOUR)
                     ELSE
                        O3CONC = -999.0D0
                     END IF
                  END IF
C                 CERC 11/30/20 Retrieve Hourly NOx Value
                  IF (GRSM) THEN
                     IF (.NOT. NOXMISS) THEN
                        NOXBGCONC = EV_NOXCONC(IHOUR)
                     ELSE
                        NOXBGCONC = -999.0D0
                     END IF
                  END IF
C*                Process Hourly Emissions from File, if needed
                  IF (HOURLY) THEN
C*                   Begin Source Loop
                     DO ISRC = 1, NUMSRC
                       IF (QFLAG(ISRC) .EQ. 'HOURLY') THEN
C*                        Retrieve Source Parameters for This Hour  ---   CALL HRQEXT
                          CALL HRQEXT(ISRC)
                       END IF
                     END DO
C*                   End Source Loop
                  END IF
C*----
                  IF (CLMHR .AND. CLMPRO) THEN
C                    Check for Calm Hr & Processing and
C                    Increment Counters
                     EV_NUMHRS = EV_NUMHRS + 1
                     EV_NUMCLM = EV_NUMCLM + 1
                  ELSE IF (MSGHR .AND. MSGPRO) THEN
C                    Check for Missing Hour & Processing and
C                    Increment Counters
                     EV_NUMHRS = EV_NUMHRS + 1
                     EV_NUMMSG = EV_NUMMSG + 1
                  ELSE IF (ZI .LE. 0.0D0) THEN
C                    Write Out The Informational Message &
C                    Increment Counters
                     WRITE(DUMMY,'(I8.8)') KURDAT
                     CALL ERRHDL(PATH,MODNAM,'I','470',DUMMY)
                     EV_NUMHRS = EV_NUMHRS + 1
                  ELSE
C                    Set CALCS Flag, Increment Counters
C                    & Calculate HRVAL
                     CALCS = .TRUE.
                     EV_NUMHRS = EV_NUMHRS + 1
C                    Calculate CONC or DEPOS Values      ---   CALL EVCALC
                     CALL EVCALC
                  END IF

                  IF (.NOT.CLMHR .AND. .NOT.MSGHR) THEN
C ---                Non-calm, non-missing hour; apply NO2 options as appropriate
           
                     IF (PVMRM .AND. .NOT.PSDCREDIT) THEN
C ---                   Process Hourly Values for PVMRM Option
                        CALL PVMRM_CALC('ALLSRCS')
                        
                     ELSE IF (PVMRM .AND. PSDCREDIT) THEN
C ---                   Process Hourly Values for PVMRM Option and PSD credits
C ---                   Need to process two separate sets of sources - the
C                       increment consumption sources ('NAAQSRC') and the 
C                       increment expanding sources ('ALLBASE')
                        CALL PVMRM_CALC('NAAQSRC')
                        CALL PVMRM_CALC('ALLBASE')
           
                     ELSE IF (OLM) THEN
C ---                   Process Hourly Values for OLM Option
                        CALL OLM_CALC
           
                     ELSE IF (ARM2) THEN
C ---                   Process Hourly Values for ARM2 Option
                        CALL ARM2_CALC
                        
                     ELSE IF (GRSM) THEN
C ---                   CERC 11/30/20 Process Hourly Values for GRSM Option
                        CALL GRSM_CALC
           
                     END IF

                  END IF

               END DO
C              End Hourly LOOP
              
C              Calculate Applicable Averages             ---   CALL AVEREV
               CALL AVEREV

C              Print Out Model Results                   ---   CALL OUTPUT
               CALL EV_OUTPUT

C ---          Compare calculated EVENT concentration (GRPAVE) to "original"   
C              EVENT concentration included on EVENTPER keyword (EV_OrigConc)
               IF( EV_OrigConc(IEVENT) .GT. 0.0D0 ) THEN
C ---             Since "original" EVENT concentration is read from input file
C                 with 5 decimal places, first round the internal results to 
C                 5 decimal places to avoid spurious messages if the original
C                 concentration is less than 10.0.
                  IF( GRPAVE(IDXEV(IEVENT)) .LT. 10.0D0 )THEN
                     GRPAVE_Test = DBLE(IDNINT(1.0D5 * 
     &                             GRPAVE(IDXEV(IEVENT))))/
     &                             1.0D5
                  ELSE
C ---                Use original value for comparison
                     GRPAVE_Test = GRPAVE(IDXEV(IEVENT))
                  END IF

                  IF( DABS((EV_OrigConc(IEVENT)-GRPAVE_Test)/
     &                      EV_OrigConc(IEVENT)) .GT. 2.0D-6 )THEN
C                   WRITE Warning Message
                    CALL ERRHDL(PATH,MODNAM,'W','497',EVNAME(IEVENT))
C ---               Assign logical flag indicating the EVENT consistency warning
C                   has been issued; a warning to the default output unit will be
C                   issued at the end of the run.
                    L_EVENT_OrigConc_Warning = .TRUE.
                  END IF
               END IF

C              Flush HRVAL, AVEVAL, GRPAVE and GRPVAL    ---   CALL EV_FLUSH
               CALL EV_FLUSH

C              Reset CALCS Flag
               CALCS = .FALSE.

C              Reset the Counters
               EV_NUMHRS = 0
               EV_NUMCLM = 0
               EV_NUMMSG = 0

            END IF   ! IF-ENDIF block on events for this JDAY

         END DO EV_LOOP
C        End Event LOOP

      END DO DAY_LOOP
C     End Loop Through Meteorology Data

      RETURN
      END

      SUBROUTINE MEREAD
C***********************************************************************
C                MEREAD Module of the AMS/EPA Regulatory Model - AERMOD - EVENT
C
C        PURPOSE: Controls Extraction and Quality Assurance of
C                 One Day of Meteorological Data for EVENT Processing
C
C        PROGRAMMER: ROGER BRODE, JEFF WANG
C
C        DATE:    March 2, 1992
C
C        MODIFIED:  Modified met data arrays to include an additional
C                   array index, since these arrays are also used by 
C                   the OU MAXDCONT post-processing option.
C                   R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
C
C        MODIFIED:  Modified code for processing multi-year data
C                   files to determine if header record is present
C                   between years for concatenated files.  Use presence
C                   of colon (':') as only criterion for header record.
C                   Use warning messages if UAIR and SURF IDs don't
C                   match input runstream file for multiple years since
C                   AERMOD allows mismatch for single year files.
C                   Modified check for stable or missing hours in 
C                   calculation of solar irradiance (QSW) for use
C                   in deposition calculations.
C                   Modified to check first hour of met data files
C                   to determine if file starts on hour 01. If not,
C                   cycle through hour loop until loop index matches
C                   hour in data file.
C                   R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
C
C        MODIFIED:  Modified code for reading the header record of the
C                   surface file to use a character variable for the
C                   AERMET version date field, in order to allow for
C                   the future use of screening meteorology that is not
C                   directly linked to a specific version under the
C                   of the AERMET processor under the SCREEN option.
C                   R.W. Brode, U.S. EPA/OAQPS/AQMG, 01/26/2007
C
C        MODIFIED:  To assign non-array logicals STABLE and UNSTAB
C                   for use in subroutine COMPTG.
C                   R.W. Brode, U.S. EPA/OAQPS/AQMG, 01/24/2007
C
C        MODIFIED:  To remove support for unformatted meteorological
C                   data files.
C                   R.W. Brode, PES, Inc., 4/10/2000
C
C        MODIFIED:  To incorporate modifications to date processing
C                   for Y2K compliance, including use of date window
C                   variables (ISTRT_WIND and ISTRT_CENT) and calculation
C                   of 10-digit date variable (FULLDATE) with 4-digit
C                   year for date comparisons.
C                   Also modified calls to METDAT insteaad of EV_METDAT
C                   to allow use of same routine for both normal and
C                   EVENT processing.
C                   R.W. Brode, PES, Inc., 5/12/99
C
C        INPUTS:  Meteorology File Specifications
C
C        OUTPUTS: Arrays of Meteorological Variables for One Day
C
C        CALLED FROM:   EVLOOP
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C---- Constants used in the computation of QSW
      DOUBLE PRECISION, PARAMETER :: C1=5.31D-13, C2=60.0D0, C3=1.12D0,
     &                               STEFB= 5.67D-08
      DOUBLE PRECISION :: RN, Es25, FVREF
      
      INTEGER :: I, IHR, IJDAY, IDATCHK, IUSI, ISSI,
     &           JFLAG, LEVEL
C --- Declare integer variable to store the current date in MEREAD
      INTEGER :: MEREAD_Date
     
      CHARACTER (LEN=8)   :: CUSI, CSSI
      CHARACTER (LEN=256) :: BUFFER

      SAVE MEREAD_Date

C     Variable Initializations
      MODNAM = 'MEREAD'
      DATA MEREAD_Date/0/
      PATH   = 'MX'
      IDATCHK = 0

C     READ Meteorology Data Based on Format --
C     When DRY deposition is modeled, U-star, L, and z0 (surface
C     roughness length) are read in addition to the standard 
C     data.  
C
C     When WET deposition is modeled, ipcode (precip.
C     code) and prate (precip. rate in mm/hr) must also be added to
C     each hourly record.
C     The format statement allows for all additional data:

C --- Calculate the MMDDHH variable to check for end of the year
C     based on MEREAD_Date
      IDATCHK = MEREAD_Date - INT(MEREAD_Date/1000000)*1000000

      IF ((IMONTH.EQ.12 .AND. IDAY.EQ.31 .AND. IHOUR.EQ.24) .OR.
     &    IDATCHK .EQ. 123124) THEN
C        End of year has been reached - check for presence of header
C        record at beginning of next year for multi-year data files.
         READ(MFUNIT,'(A256)',ERR=998,END=1000,IOSTAT=IOERRN) BUFFER

C ---    First check for ':' as indicator of header record, then extract 
C        AERMET version date, C_METVER, and station IDs
         IF (INDEX(BUFFER,':') .EQ. 0) THEN
C           Record does not contain colon. Assume it must be regular
C           met data record, so backspace met file before proceeding.
            BACKSPACE MFUNIT
         ELSE
C           Record contains colons. Assume it is a header record, and
C           extract AERMET version date, C_METVER, and check station 
C           IDs before proceeding in order to flag potential for use 
C           of different stations in multi-year data files.
            IF( INDEX(BUFFER,'VERSION:') .NE. 0 )THEN
C              Extract AERMET version date from embedded header record
               READ(BUFFER(INDEX(BUFFER,'VERSION:')+8:
     &                     INDEX(BUFFER,'VERSION:')+13),'(A6)')
     &                                                 C_METVER
            ELSEIF( BUFFER(93:98) .NE. '      ' )THEN
C              The 'VERSION:' keyword is missing from header so assign columns 
C              93-98 to C_METVER
               C_METVER = BUFFER(93:98)
            ELSE
C              AERMET version not found in header record, issue fatal error message
               CALL ERRHDL(PATH,MODNAM,'E','395','No Version')      
            ENDIF

C ---       Read Lat/Lon from header record BUFFER
            READ(BUFFER,1900,ERR=99,IOSTAT=IOERRN) ALAT, ALON
 1900       FORMAT(2A10)

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

      HOUR_LOOP: DO IHR = 1, NHR
C
C---- READ surface scaling meteorology data based on format
C
      IF( LDPART .OR. LWPART .OR. LDGAS .OR. LWGAS .OR. GRSM )THEN
C        Read record from ASCII scalar parameter file using FREE format
C        with deposition variables
C
C ---    First read date variables to check for problems
         READ( MFUNIT, *, END=1000, ERR=99, IOSTAT=IOERRN ) IYEAR,
     &         IMONTH, IDAY, IJDAY, IHOUR

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

         MEREAD_Date = IYR*1000000 + IMONTH*10000 + IDAY*100 + IHOUR
C       
         IF (IHOUR .EQ. IHR) THEN
C ---       Data file hour matches loop hour; backspace and read full record
            BACKSPACE MFUNIT
            READ( MFUNIT, *, END=1000, ERR=99, IOSTAT=IOERRN ) IYEAR,
     &       IMONTH, IDAY, IJDAY, IHOUR, ASFCHF(IHR,1), AUSTAR(IHR,1),
     &       AWSTAR(IHR,1),AVPTGZI(IHR,1),AZICONV(IHR,1),AZIMECH(IHR,1),
     &       AOBULEN(IHR,1), ASFCZ0(IHR,1),ABOWEN(IHR,1),AALBEDO(IHR,1),
     &       AUREF(IHR,1), AWDREF(IHR,1), AUREFHT(IHR,1), ATA(IHR,1),
     &       ATREFHT(IHR,1), IAPCODE(IHR,1), APRATE(IHR,1), ARH(IHR,1),
     &       ASFCP(IHR,1), NACLOUD(IHR,1)
         ELSE IF (IHOUR .GT. IHR) THEN
C ---       Data file starts after hour 01; 
C           Issue warning, backspace file and skip to Profile file
            WRITE(DUMMY,'(2X,3I2)',ERR=99) IMONTH, IDAY, IHR
            CALL ERRHDL(PATH,MODNAM,'W','489',DUMMY)
            BACKSPACE MFUNIT
            GO TO 888
         ELSE
C ---       Data file hour is less than loop hour;
C           could be problem with data file or use of 00-23 hour convention
C           Issue error message:
            WRITE(DUMMY,'(2X,3I2)',ERR=99) IMONTH, IDAY, IHOUR
            CALL ERRHDL(PATH,MODNAM,'E','490',DUMMY)
            EXIT HOUR_LOOP
         END IF

C        Calculate solar irradiance, QSW, from Heat Flux, Bowen ratio,
C        albedo and cloud cover, for use in gas deposition algorithm.
C        Include check for ABOWEN < 0 for non-standard inputs.
         IF (AOBULEN(IHR,1).GT.0.0D0 .OR. AOBULEN(IHR,1).LT.-99990.0D0
     &      .OR. ATA(IHR,1).LT.0.0D0 .OR. 
     &       AALBEDO(IHR,1).EQ.1.0D0 .OR. ABOWEN(IHR,1).LE.0.0D0) THEN
C           Hour is stable or missing or inappropriate surface chars.
            AQSW(IHR,1) = 0.0D0
         ELSE
            RN = (1.D0 + 1.D0/ABOWEN(IHR,1))*ASFCHF(IHR,1)/0.9D0
            AQSW(IHR,1) = (RN*(1.D0+C3)-C1*ATA(IHR,1)**6+
     &                     STEFB*ATA(IHR,1)**4 -
     &                    C2*0.1D0*DBLE(NACLOUD(IHR,1))) / 
     &                   (1.D0-AALBEDO(IHR,1))
         END IF
C
C        Save precipitation rates for two previous hours
         IF (IHR .EQ. 1) THEN
            Aprec2(IHR,1) = APrate(NHR-1,1)
            Aprec1(IHR,1) = APrate(NHR,1)
         ELSE IF (IHR .EQ. 2) THEN
            Aprec2(IHR,1) = APrate(NHR,1)
            Aprec1(IHR,1) = APrate(IHR-1,1)
         ELSE
            Aprec2(IHR,1) = APrate(IHR-2,1)
            Aprec1(IHR,1) = APrate(IHR-1,1)
         END IF

C        Set variables for dry deposition
         IF (LDPART .OR. LDGAS) THEN
            IF (ATA(IHR,1).LT.0.0D0 .OR. APRATE(IHR,1).LT.0.0D0) THEN
               AWNEW(IHR,1) = AWOLD(IHR,1)
            ELSE
c ...          Compute saturation vapor pressure based on CMAQ formula
               AEsTa(IHR,1) = 0.6112D0*DEXP(19.83D0 - 
     &                        5417.4D0/ATA(IHR,1))
               Es25 = 3.167D0
               AWnew(IHR,1) = Wold+APrec1(IHR,1)-
     &                       0.5D0*f2*AEsTa(IHR,1)/Es25
               Wold = AWnew(IHR,1)
               Af2(IHR,1) = AWnew(IHR,1)/200.D0
               if (Af2(IHR,1).le.0.01D0) Af2(IHR,1) = 0.01D0
               if (Af2(IHR,1).gt.1.0D0) Af2(IHR,1) = 1.0D0
               f2 = Af2(IHR,1)
            END IF
         END IF

      ELSE
C        Read record from ASCII scalar parameter file without deposition
C        parameters, using FREE format
C
C ---    Calculate the MMDDHH variable to check for end of the year
C        based on MEREAD_Date
         IDATCHK = MEREAD_Date - INT(MEREAD_Date/1000000)*1000000
         IF ((IMONTH.EQ.12 .AND. IDAY.EQ.31 .AND. IHOUR.EQ.24) .OR.
     &       IDATCHK .EQ. 123124) THEN
C           End of year has been reached - check for presence of header
C           record at beginning of next year for multi-year data files.
            READ(MFUNIT,'(A256)',ERR=9981,END=1000,IOSTAT=IOERRN) BUFFER
      
C ---       First check for ':' as indicator of header record, then extract 
C           AERMET version date, C_METVER, and station IDs
            IF (INDEX(BUFFER,':') .EQ. 0) THEN
C              Record does not contain colon. Assume it must be regular
C              met data record, so backspace met file before proceeding.
               BACKSPACE MFUNIT
            ELSE
C              Record contains colons. Assume it is a header record, and
C              extract AERMET version date, C_METVER, and check station 
C              IDs before proceeding in order to flag potential for use 
C              of different stations in multi-year data files.
               IF( INDEX(BUFFER,'VERSION:') .NE. 0 )THEN
C                 Extract AERMET version date from embedded header record
                  READ(BUFFER(INDEX(BUFFER,'VERSION:')+8:
     &                        INDEX(BUFFER,'VERSION:')+13),'(A6)')
     &                                                    C_METVER
               ELSEIF( BUFFER(93:98) .NE. '      ' )THEN
C                 The 'VERSION:' keyword is missing from header so assign columns 
C                 93-98 to C_METVER
                  C_METVER = BUFFER(93:98)
               ELSE
C                 AERMET version not found in header record, issue fatal error message
                  CALL ERRHDL(PATH,MODNAM,'E','395','No Version')      
               ENDIF
      
C ---          Read Lat/Lon from header record BUFFER
               READ(BUFFER,1900,ERR=99,IOSTAT=IOERRN) ALAT, ALON
      
C ---          Now extract UA, SF, and OS station IDs from header record
               IF( INDEX(BUFFER,'UA_ID:') .GE. 0 )THEN
                  READ(BUFFER(INDEX(BUFFER,'UA_ID:')+7:
     &                        INDEX(BUFFER,'UA_ID:')+15),'(A)') CUSI
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
     &                        INDEX(BUFFER,'SF_ID:')+15),'(A)') CSSI
               ELSE
                  CSSI = '        '
               END IF
               CALL STONUM(CSSI,8,FNUM,IMIT)
               IF (IMIT .EQ. 1) THEN
                  ISSI = NINT(FNUM)
               ELSE
                  ISSI = 0
               END IF
      
               IF (ISSI .NE. IDSURF) THEN
C                 Write Warning Message:  SURFDATA id mismatch
                  CALL ERRHDL(PATH,MODNAM,'W','530','SURFDATA')
               END IF
               IF (IUSI .NE. IDUAIR) THEN
C                 Write Warning Message:  UAIRDATA id mismatch
                  CALL ERRHDL(PATH,MODNAM,'W','530','UAIRDATA')
               END IF
            END IF
      
            GO TO 1002
      
C           Error reading 'header record' - assume that header record is
C           missing.  Backspace met file and continue processing.
 9981       BACKSPACE MFUNIT
      
         END IF
      
1002     CONTINUE

C ---    First read date variables to check for problems
         READ( MFUNIT, *, END=1000, ERR=99, IOSTAT=IOERRN ) IYEAR,
     &         IMONTH, IDAY, IJDAY, IHOUR
C
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

         MEREAD_Date = IYR*1000000 + IMONTH*10000 + IDAY*100 + IHOUR

         IF (IHOUR .EQ. IHR) THEN
C ---       Data file hour matches loop hour; backspace and read full record
            BACKSPACE MFUNIT
            READ( MFUNIT, *, END=1000, ERR=99, IOSTAT=IOERRN ) IYEAR,
     &       IMONTH, IDAY, IJDAY, IHOUR, ASFCHF(IHR,1), AUSTAR(IHR,1),
     &       AWSTAR(IHR,1),AVPTGZI(IHR,1),AZICONV(IHR,1),AZIMECH(IHR,1),
     &       AOBULEN(IHR,1), ASFCZ0(IHR,1),ABOWEN(IHR,1),AALBEDO(IHR,1),
     &       AUREF(IHR,1), AWDREF(IHR,1), AUREFHT(IHR,1), ATA(IHR,1),
     &       ATREFHT(IHR,1), IAPCODE(IHR,1), APRATE(IHR,1), ARH(IHR,1),
     &       ASFCP(IHR,1), NACLOUD(IHR,1)
         ELSE IF (IHOUR .GT. IHR) THEN
C ---       Data file starts after hour 01; 
C           Issue warning, backspace file and skip to Profile file
            WRITE(DUMMY,'(2X,3I2)',ERR=99) IMONTH, IDAY, IHR
            CALL ERRHDL(PATH,MODNAM,'W','489',DUMMY)
            BACKSPACE MFUNIT
            GO TO 888
         ELSE
C ---       Data file hour is less than loop hour;
C           could be problem with data file or use of 00-23 hour convention
C           Issue error message:
            WRITE(DUMMY,'(2X,3I2)',ERR=99) IMONTH, IDAY, IHOUR
            CALL ERRHDL(PATH,MODNAM,'E','490',DUMMY)
            EXIT HOUR_LOOP
         END IF
C
      END IF

C     Set the stability logical variables
      IF( AOBULEN(IHR,1) .GT. 0.0D0 ) THEN
         AUNSTAB(IHR,1) = .FALSE.
         ASTABLE(IHR,1) = .TRUE.
C        Also set non-array variables for use in COMPTG
         UNSTAB = .FALSE.
         STABLE = .TRUE.
      ELSE
         AUNSTAB(IHR,1) = .TRUE.
         ASTABLE(IHR,1) = .FALSE.
C        Also set non-array variables for use in COMPTG
         UNSTAB = .TRUE.
         STABLE = .FALSE.
      END IF

C --- Assign Sector IDs by hour for sector-varying BACKGRND if needed
      IF (L_Backgrnd) THEN
         IF (AWDREF(IHR,1) .LE. 0.0D0 .OR. 
     &       AWDREF(IHR,1) .GT. 360.0D0) THEN
C ---       Hour is calm or missing; set ABGSECT = 0
            ABGSECT(IHR) = 0
         ELSE
C ---       Valid wind direction is available
C ---       Assign sector ID for direction-varying BACKGRND
            FVREF = AWDREF(IHR,1) + 180.0D0
            IF (FVREF .GT. 360.0D0) THEN
               FVREF = FVREF - 360.0D0
            END IF
            IF (L_BGSector) THEN
               IF (FVREF .LT. BGSECT(1) .OR. 
     &             FVREF .GE. BGSECT(NUMBGSects) ) THEN
                  ABGSECT(IHR) = NUMBGSects
               ELSE
                  DO I = 1, NUMBGSects-1
                     IF (FVREF .GE. BGSECT(I) .AND. 
     &                   FVREF .LT. BGSECT(I+1)) THEN
                        ABGSECT(IHR) = I
                        EXIT
                     END IF
                  END DO
               END IF
            ELSE
               ABGSECT(IHR) = 1
            END IF
         END IF
      END IF

C --- Assign Sector IDs by hour for direction-varying background O3 if needed
      IF (L_O3SECTOR) THEN
         IF (AWDREF(IHR,1) .LE. 0.0D0 .OR. 
     &       AWDREF(IHR,1) .GT. 360.0D0) THEN
C ---       Hour is calm or missing; set AO3SECT = 0
            AO3SECT(IHR) = 0
         ELSE
C ---       Valid wind direction is available
C ---       Assign sector ID for direction-varying background O3
            FVREF = AWDREF(IHR,1) + 180.0D0
            IF (FVREF .GT. 360.0D0) THEN
               FVREF = FVREF - 360.0D0
            END IF
            IF (L_O3Sector) THEN
               IF (FVREF .LT. O3SECT(1) .OR. 
     &             FVREF .GE. O3SECT(NUMO3Sects) ) THEN
                  AO3SECT(IHR) = NUMO3Sects
               ELSE
                  DO I = 1, NUMO3Sects-1
                     IF (FVREF .GE. O3SECT(I) .AND. 
     &                   FVREF .LT. O3SECT(I+1)) THEN
                        AO3SECT(IHR) = I
                        EXIT
                     END IF
                  END DO
               END IF
            ELSE
               AO3SECT(IHR) = 1
            END IF
         END IF
      ELSE
C ---    No O3SECTORs; assign 1 to AO3SECT array
         AO3SECT(IHR) = 1
      END IF

C --- CERC 11/30/20 Assign Sector IDs by hour for direction-varying background NOX if needed
      IF (L_NOXSECTOR) THEN
         IF (AWDREF(IHR,1) .LE. 0.0D0 .OR. 
     &       AWDREF(IHR,1) .GT. 360.0D0) THEN
C ---       Hour is calm or missing; set ANOXSECT = 0
            ANOXSECT(IHR) = 0
         ELSE
C ---       Valid wind direction is available
C ---       Assign sector ID for direction-varying background NOX
            FVREF = AWDREF(IHR,1) + 180.0D0
            IF (FVREF .GT. 360.0D0) THEN
               FVREF = FVREF - 360.0D0
            END IF
            IF (FVREF .LT. NOXSECT(1) .OR. 
     &             FVREF .GE. NOXSECT(NUMNOxSects) ) THEN
               ANOXSECT(IHR) = NUMNOxSects
            ELSE
               DO I = 1, NUMNOxSects-1
                  IF (FVREF .GE. NOXSECT(I) .AND. 
     &                   FVREF .LT. NOXSECT(I+1)) THEN
                     ANOXSECT(IHR) = I
                     EXIT
                  END IF
               END DO
            END IF
         END IF
      ELSE
C ---    No NOXSECTORs; assign 1 to ANOXSECT array
         ANOXSECT(IHR) = 1
      END IF

C---- Initialize the profile data to missing;
C     READ profile data based on format
C

C --- Branch here if surface data file starts after hour 01
C
 888  CONTINUE
 
      CALL PFLINI ()
      LEVEL = 1
      JFLAG = 0
C     Read record from ASCII profile file using FREE format; compute
C     sigma_V from sigma_A and wind speed
C --- First read date variables to check for problems
      READ( MPUNIT, *, END=1000, ERR=98, IOSTAT=IOERRN ) KYEAR,
     &         KMONTH, KDAY, KHOUR
C       
      IF (KHOUR .EQ. IHR) THEN
C ---    Data file hour matches loop hour; backspace and read full record
         BACKSPACE MPUNIT

      DO WHILE( JFLAG .EQ. 0 )
         READ( MPUNIT, *, END=1000, ERR=98, IOSTAT=IOERRN ) KYEAR,
     &       KMONTH, KDAY, KHOUR, PFLHT(LEVEL), JFLAG,
     &       PFLWD(LEVEL), PFLWS(LEVEL), PFLTA(LEVEL),
     &       PFLSA(LEVEL), PFLSW(LEVEL)

C        Convert the data to the required units
         CALL PFLCNV (LEVEL)

C        Set the number of profile levels to current index, store
C        the 'top of profile' flag, and increment level if not at top
C        Check that the level does not exceed the maximum allowable
         NPLVLS = LEVEL
         ANPLVLS(IHR,1) = LEVEL
         AIFLAG(IHR,LEVEL,1) = JFLAG
         APFLHT(IHR,LEVEL,1) = PFLHT(LEVEL)
         APFLWD(IHR,LEVEL,1) = PFLWD(LEVEL)
         APFLWS(IHR,LEVEL,1) = PFLWS(LEVEL)
         APFLTA(IHR,LEVEL,1) = PFLTA(LEVEL)
         APFLSA(IHR,LEVEL,1) = PFLSA(LEVEL)
         APFLSV(IHR,LEVEL,1) = PFLSV(LEVEL)
         APFLSW(IHR,LEVEL,1) = PFLSW(LEVEL)
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

      ELSE IF (KHOUR .GT. IHR) THEN
C ---    Data file starts after hour 01; 
C        Backspace file and cycle hour loop
         BACKSPACE MPUNIT
         CYCLE HOUR_LOOP

      ELSE
C ---    Data file hour is less than loop hour;
C        could be problem with data file or use of 00-23 hour convention
C        Issue error message:
         WRITE(DUMMY,'(2X,3I2)',ERR=99) IMONTH, IDAY, IHOUR
         CALL ERRHDL(PATH,MODNAM,'E','489',DUMMY)
         EXIT HOUR_LOOP
      END IF

C     Compute the vertical potential temperature gradient profile
      IF( .NOT. RUNERR ) THEN
         NTGLVL = 0
         CALL COMPTG ()
         ANTGLVL(IHR,1) = NTGLVL
         DO I = 1, NTGLVL
           APFLTG(IHR,I,1)  = PFLTG(I)
           APFLTGZ(IHR,I,1) = PFLTGZ(I)
         END DO
      END IF

      END DO HOUR_LOOP

C --- Set the date variables; but first assign IHOUR = 24 
C     since only full days of met data are read in EVENT mode
      IHOUR = 24
      CALL SET_DATES

      GO TO 999

C     WRITE Error Messages:  Error Reading Met Data File

 98   CALL ERRHDL(PATH,MODNAM,'E','510','PROFFILE')
      RUNERR = .TRUE.
      GO TO 999

 99   CALL ERRHDL(PATH,MODNAM,'E','510','SURFFILE')
      RUNERR = .TRUE.
      GO TO 999

 1000 EOF = .TRUE.
 
C --- Set the date variables; but first assign IHOUR = 24 
C     since only full days of met data are read in EVENT mode
      IHOUR = 24
      CALL SET_DATES

 999  RETURN
      END

      SUBROUTINE EV_METEXT
C***********************************************************************
C                EV_METEXT Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Controls Extraction and Quality Assurance of
C                 One Hour of Meteorological Data
C
C        PROGRAMMER: ROGER BRODE, JEFF WANG
C        MODIFIED BY D. Strimaitis, SRC (for Wet & Dry DEPOSITION)
C
C        DATE:    November 8, 1993
C
C        MODIFIED:   Modified met data arrays to include an additional
C                    array index, since these arrays are also used by 
C                    the OU MAXDCONT post-processing option.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
C
C        MODIFIED:   To remove unused data array (NDAY).
C                    R.W. Brode, PES, Inc., 4/10/2000
C
C        MODIFIED:   To incorporate modifications to date processing
C                    for Y2K compliance, including use of date window
C                    variables (ISTRT_WIND and ISTRT_CENT) and calculation
C                    of 10-digit date variable (FULLDATE) with 4-digit
C                    year for date comparisons.
C                    R.W. Brode, PES, Inc., 5/12/99
C
C        MODIFIED:   To add determination of season index (ISEAS).
C                    R.W. Brode, PES, Inc. - 12/2/98
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
C        INPUTS:  Meteorology File Specifications
C
C        OUTPUTS: Meteorological Variables for One Hour
C
C        CALLED FROM:   EVLOOP
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      USE BUOYANT_LINE
      IMPLICIT NONE
      CHARACTER MODNAM*12
C Unused:      INTEGER I

C     Variable Initializations
      MODNAM = 'EV_METEXT'
      PATH   = 'MX'

C     Save Value of Last YR/MN/DY/HR and Previous Hour
      IPDATE = KURDAT
      IPHOUR = IHOUR

C     Set Meteorological Variables for This Hour
      SFCHF  = ASFCHF(IHOUR,1)
      UREF   = AUREF(IHOUR,1)
      UREFHT = AUREFHT(IHOUR,1)
      TA     = ATA(IHOUR,1)
C     Save the temperature for buoyant line source processing because
C      TA is 'adjusted' when point sources are processed
      IF (L_BLSOURCE) THEN
         BLTA = ATA(IHOUR,1)
      END IF
      TREFHT = ATREFHT(IHOUR,1)
      WDREF  = AWDREF(IHOUR,1)
      USTAR  = AUSTAR(IHOUR,1)
      WSTAR  = AWSTAR(IHOUR,1)
      ZICONV = AZICONV(IHOUR,1)
      ZIMECH = AZIMECH(IHOUR,1)
      OBULEN = AOBULEN(IHOUR,1)
      VPTGZI = AVPTGZI(IHOUR,1)
      SFCZ0  = ASFCZ0(IHOUR,1)
      BOWEN  = ABOWEN(IHOUR,1)
      ALBEDO = AALBEDO(IHOUR,1)
      IPCODE = IAPCODE(IHOUR,1)
      PRATE  = APRATE(IHOUR,1)
      RH     = ARH(IHOUR,1)
      SFCP   = ASFCP(IHOUR,1)
      NCLOUD = NACLOUD(IHOUR,1)
      QSW    = AQSW(IHOUR,1)
      Wnew   = AWnew(IHOUR,1)
      f2     = Af2(IHOUR,1)
      EsTa   = AEsTa(IHOUR,1)
      Prec1  = APrec1(IHOUR,1)
      Prec2  = APrec2(IHOUR,1)

      NPLVLS = ANPLVLS(IHOUR,1)

      IFLAG(1:NPLVLS) = AIFLAG(IHOUR,1:NPLVLS,1)
      PFLHT(1:NPLVLS) = APFLHT(IHOUR,1:NPLVLS,1)
      PFLWD(1:NPLVLS) = APFLWD(IHOUR,1:NPLVLS,1)
      PFLWS(1:NPLVLS) = APFLWS(IHOUR,1:NPLVLS,1)
      PFLTA(1:NPLVLS) = APFLTA(IHOUR,1:NPLVLS,1)
      PFLSA(1:NPLVLS) = APFLSA(IHOUR,1:NPLVLS,1)
      PFLSV(1:NPLVLS) = APFLSV(IHOUR,1:NPLVLS,1)
      PFLSW(1:NPLVLS) = APFLSW(IHOUR,1:NPLVLS,1)

      NTGLVL = ANTGLVL(IHOUR,1)

      PFLTG(1:NTGLVL)  = APFLTG(IHOUR,1:NTGLVL,1)
      PFLTGZ(1:NTGLVL) = APFLTGZ(IHOUR,1:NTGLVL,1)

C     Set Meteorological Variables for Current Hour
C       If a buoyant line source is processed, the PG stability is
C       required.  SET_METDATA calls LTOPG to calculate KST, so it does
C       not need to be done in this subroutine.
      CALL SET_METDATA

      RETURN
      END

      SUBROUTINE EV_HRQREAD(L_FIRSTCALL)
C***********************************************************************
C*                  EV_HQREAD Module of AERMOD
C* 
C*         PURPOSE: To Read a 24-hour Block of Hourly Emissions Data
C* 
C*         PROGRAMMER:  Jayant Hardikar, Roger Brode
C* 
C*         DATE:    September 15, 1993
C* 
C*         INPUTS:  Variable QFLAG and Current Source Number Being Processed
C* 
C*         OUTPUTS: Source Arrays
C*          
C*         MODIFIED:  REMOVED THE 'POINT' SOURCE CONDITION, SO IT APPLIES 
C*                    TO ALL SOURCE TYPES, EXCEPT SAVING THE TEMP & VEL
C*
C*         MODIFIED:  Added code to process hourly values for buoyant
C*                    line source. Amec Foster Wheeler - 03/31/2016
C*
C*         CALLED FROM:  EVLOOP
C************************************************************************
C*
C*    Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: IS, IHR
C     JAT 7/22/21 D065 ILSAVE NOT USED
C      INTEGER :: ILSAVE
      LOGICAL :: L_FIRSTCALL
      LOGICAL :: EOF_SAVE

C*    Variable Initializations
      MODNAM = 'EV_HRQREAD'
C*    Save current value of EOF from MEREAD
      EOF_SAVE = EOF
C*    Reinitialize EOF = .F. for HRQREAD
      EOF = .FALSE.
C     JAT 7/22/21 D065 ILSAVE NOT USED      
C      ILSAVE = ILINE

      HOUR_LOOP: DO IHR = 1, NHR
         IQLINE = IQLINE + 1
         SOURCE_LOOP: DO IS = 1, NUMSRC
            IF (QFLAG(IS) .EQ. 'HOURLY') THEN

               IF (L_FIRSTCALL) ILINE = 1
C ---          Assign ILINE = 1 for first call to HRQREAD

               CALL HRQREAD (IS)

               IF (.NOT.EOF .AND. IHR .EQ. NHR) THEN
C*                Check for Date and Time Consistency with Met Data; 
C*                If Failed, Issue Fatal Error
                  IF (FULLDATE .NE. FULLHRQ) THEN
C*                   WRITE Error Message - Date mismatch
                     WRITE(DUMMY,'(I10.10)') FULLDATE
                     CALL ERRHDL(PATH,MODNAM,'E','455',DUMMY)
                     RUNERR = .TRUE.
                     EXIT HOUR_LOOP
                  END IF
               ELSE IF (EOF) THEN
C ---             EOF reached in HRQREAD; reassign EOF based on MEREAD
C                 Exit hour loop to avoid read error in HRQREAD
                  EOF = EOF_SAVE
                  EXIT HOUR_LOOP
               END IF

               EV_HRQS(IS,IHR) = HRQS

               IF (SRCTYP(IS)(1:5) .EQ. 'POINT') THEN
                  EV_HRTS(IS,IHR) = HRTS
                  EV_HRVS(IS,IHR) = HRVS
               ELSE IF (SRCTYP(IS) .EQ. 'VOLUME' .AND. 
     &                                           L_HRLYSIG(IS)) THEN
                  EV_HRHS(IS,IHR) = HRHS
                  EV_HRSY(IS,IHR) = HRSY
                  EV_HRSZ(IS,IHR) = HRSZ
               ELSE IF ((SRCTYP(IS)(1:4) .EQ. 'AREA' .OR. 
     &                        SRCTYP(IS) .EQ. 'LINE') .AND. 
     &                                           L_HRLYSIG(IS)) THEN
                  EV_HRHS(IS,IHR) = HRHS
                  EV_HRSZ(IS,IHR) = HRSZ
               ELSE IF (SRCTYP(IS) .EQ. 'OPENPIT') THEN
                  EV_HRTS(IS,IHR) = HRTS
               ELSE IF (SRCTYP(IS) .EQ. 'BUOYLINE') THEN
                  EV_HRFP(IS,IHR)  = HRFP
               END IF

            END IF
         END DO SOURCE_LOOP
      END DO HOUR_LOOP

      RETURN
      END

      SUBROUTINE O3READ
C***********************************************************************
C*                  O3READ Module of AERMOD
C* 
C*         PURPOSE: To Read a 24-hour Block of Ozone Data
C* 
C*         PROGRAMMER:  Roger Brode
C* 
C*         DATE:    October 17, 2005
C* 
C          MODIFIED:  Modified to assign EV_O3CONC(IHR) value based on 
C                     the O3BACK variable from CO OZONEVAL keyword when 
C                     no background hourly ozone file or CO O3VALUES inputs
C                     are available.
C                     R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/29/2012
C
C          MODIFIED:  Modified to use background ozone values input
C                     through the CO O3VALUES option to substitute for 
C                     missing hourly ozone concentrations.
C                     R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
C
C*         INPUTS:
C* 
C*         OUTPUTS:
C*          
C*         CALLED FROM:
C************************************************************************
C*
C*    Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      DOUBLE PRECISION :: O3MIN, O3MAX24
      DOUBLE PRECISION :: O3SUB(6)
      DOUBLE PRECISION :: EV_O3TEMP(24)
      INTEGER :: I, IHR, IO3YR, IO3MN, IO3DY, IO3HR, IO3YR2
      INTEGER :: FULLO3HR(6), FULLO3YYMMDD
C Unused:      INTEGER :: J

C*    Variable Initializations
      MODNAM  = 'O3READ'

C --- Initialize full date variable for all sectors to 0
      FULLO3HR(:)  = 0
C --- Initialize O3SUB substitution values to 0
      O3SUB(:)     = 0.0D0
      EV_O3TEMP(:) = 0.0D0
C --- Initialize O3MISS logical array to FALSE for all hours
      L_AO3MISS(:) = .FALSE.

C --- Loop through the current day
      DO IHR = 1, 24

C ---    Initialize EV_O3CONC to 0.0 and other arrays to -99.
         EV_O3CONC(IHR) =   0.0D0
         EV_O3TEMP(IHR) = -99.0D0
         O3SUB(:) = -99.0D0
         O3MIN    = -99.0D0

C ---    Assign local IHR index to global IHOUR index; since this
C        may be used to identify temporally-varying O3 values
         IHOUR = IHR
         
C ---    Assign O3SECT array value to scalar variable
         IO3SECT = AO3SECT(IHR)

         DO I = 1, NUMO3Sects
C ---       Loop through O3SECTORs

C ---       Reinitialize O3SUB for this sector
            O3SUB(I) = -99.0D0

C ---       Check for temporally-varying ozone concentrations from O3VALUES
C           keyword; used to fill in for missing hourly data.
            IF (L_O3VALUES(I)) THEN
               CALL OZONVALS(I,O3SUB(I))
            ELSE IF (L_O3VAL(I)) THEN
               O3SUB(I) = O3BACK(I)
            ELSE
               O3SUB(I) = 0.0D0
            END IF

            IF (L_O3Hourly) THEN
C ---          Hourly O3 data is available; read and process the data

               IF (L_O3FILE(I)) THEN
C ---             Hourly O3 file available for current sector

                  IF (I .EQ. IO3SECT) THEN
C ---                This is the applicable sector for this hour; read next hour of O3 data

                     IF (O3FORM(I) .EQ. 'FREE') THEN
                        READ(IO3UNT(I),*,ERR=99,END=999) IO3YR, IO3MN,
     &                                                   IO3DY, IO3HR,
     &                                                EV_O3CONC(IO3HR)
                     ELSE
                        READ(IO3UNT(I),O3FORM(I),ERR=99,END=999)
     &                                                   IO3YR, IO3MN,
     &                                                   IO3DY, IO3HR, 
     &                                                EV_O3CONC(IO3HR)
                     END IF

C ---                Determine 4-digit year
                     IF (IO3YR .LE. 99) THEN
                        IO3YR2 = IO3YR
                        IF (IO3YR2 .GE. ISTRT_WIND .AND. 
     &                                       IO3YR2 .LE. 99) THEN
                           IO3YR  = ISTRT_CENT*100 + IO3YR2
                        ELSE IF (IO3YR2 .LT. ISTRT_WIND) THEN
                           IO3YR  = (ISTRT_CENT+1)*100 + IO3YR2
                        END IF
                     END IF
                   
C ---                Calculate full date for this hour of O3 data
                     FULLO3HR(I) = IO3YR*1000000 + IO3MN*10000 + 
     &                                               IO3DY*100 + IO3HR

                     IF (EV_O3CONC(IO3HR) .GE. 0.0D0 .AND. 
     &                   EV_O3CONC(IO3HR) .LT. 900.0D0) THEN
C ---                   Valid hourly value; convert to ug/m3 if needed

                        IF (O3FILUNITS .EQ. 'PPB') THEN
                           EV_O3CONC(IO3HR) = EV_O3CONC(IO3HR) * O3_PPB
                        ELSE IF (O3FILUNITS .EQ. 'PPM') then
                           EV_O3CONC(IO3HR) = EV_O3CONC(IO3HR) * O3_PPM
                        END IF

                        IF (.NOT. NOMINO3) THEN !CRCO D074 check for NOMIN03
                         IF (ASTABLE(IO3HR,1)) THEN
C                           Use min of 40 ppb (78.4ug/m3) and max 
C                           from previous 24 hrs
                            O3MAX24 = MIN ( 78.40D0, 
     &                          MAXVAL(O3_Max24hr(:,AO3SECT(IO3HR))))
C                           Adjust minimum O3 value based on OBULEN
                            IF (AOBULEN(IO3HR,1).GT.0.0D0 .AND. 
     &                          AOBULEN(IO3HR,1).LE.50.0D0) THEN
                               O3MIN = O3MAX24
                            ELSE IF (AOBULEN(IO3HR,1) .GT. 250.0D0) THEN
                               O3MIN = 0.0D0
                            ELSE
                              O3MIN = O3MAX24*(250.D0-AOBULEN(IO3HR,1))/
     &                                         200.D0
                            END IF
                         ELSE
                            O3MIN = -9.0D0
                         END IF
C ---                    Save this hour's O3CONC to array of previous 
C                        24 values, before applying minimum value
                           O3_Max24hr(IO3HR,IO3SECT) = EV_O3CONC(IO3HR)
                           EV_O3CONC(IO3HR) = MAX(EV_O3CONC(IO3HR),
     &                                            O3MIN)
                         END IF !End CRCO D074 Add check for NOMIN03
                     ELSE IF (L_O3VALUES(IO3SECT) .OR.
     &                           L_O3VAL(IO3SECT)) THEN
C ---                   Hourly O3 values is missing; assign O3VALS value;
C                       these have already been converted to ug/m3
                        EV_O3CONC(IO3HR) = O3SUB(IO3SECT)
C ---                   Assign 0.0 to O3_Max24hr array for this hour
                        O3_Max24hr(IO3HR,IO3SECT) = 0.0D0
                        IF (.NOT. L_SkipMessages) THEN
                          WRITE(DUMMY,'(I10.10,''S'',I1)') FULLO3HR(I),I
                          CALL ERRHDL(PATH,MODNAM,'I','458',DUMMY)
                        END IF
                     ELSE
C ---                   Assign L_AO3MISS logical to TRUE for this hour
                        L_AO3MISS(IO3HR) = .TRUE.
C ---                   Assign 0.0 to O3_Max24hr array for this hour
                        O3_Max24hr(IO3HR,IO3SECT) = 0.0D0
                        IF (.NOT. L_SkipMessages) THEN
                          WRITE(DUMMY,'(I10.10,''S'',I1)') FULLO3HR(I),I
                          CALL ERRHDL(PATH,MODNAM,'I','459',DUMMY)
                        END IF
                     END IF

                  ELSE
C ---                This is not applicable sector for this hour; however, read 
C                    O3 values to keep track of 24hr max value for this sector
                     IF (O3FORM(I) .EQ. 'FREE') THEN
                       READ(IO3UNT(I),*,ERR=99,END=999) IO3YR, IO3MN,
     &                                                  IO3DY, IO3HR,
     &                                               EV_O3TEMP(IO3HR)
                     ELSE
                       READ(IO3UNT(I),O3FORM(I),ERR=99,END=999)
     &                                                  IO3YR, IO3MN,
     &                                                  IO3DY, IO3HR,
     &                                               EV_O3TEMP(IO3HR)
                     END IF

C ---                Check for use of 2-digit year in OZONEFIL file, adjust to 4-digit
C                    year for comparison with FULLDATE based on met data file
                     IF (IO3YR .LE. 99) THEN
                        IO3YR2 = IO3YR
                        IF (IO3YR2 .GE. ISTRT_WIND .AND. 
     &                                       IO3YR2 .LE. 99) THEN
                           IO3YR  = ISTRT_CENT*100 + IO3YR2
                        ELSE IF (IO3YR2 .LT. ISTRT_WIND) THEN
                           IO3YR  = (ISTRT_CENT+1)*100 + IO3YR2
                        END IF
                     END IF
                   
C ---                Calculate full date for this hour of O3 data
                     FULLO3HR(I) = IO3YR*1000000 + IO3MN*10000 + 
     &                                               IO3DY*100 + IO3HR

                     IF (EV_O3TEMP(IO3HR) .GE. 0.0D0 .AND. 
     &                   EV_O3TEMP(IO3HR) .LT. 900.0D0) THEN
C ---                   Valid hourly value; convert to ug/m3 if needed
                        IF (O3FILUNITS .EQ. 'PPB') THEN
                           EV_O3TEMP(IO3HR) = EV_O3TEMP(IO3HR) * O3_PPB
                        ELSE IF (O3FILUNITS .EQ. 'PPM') then
                           EV_O3TEMP(IO3HR) = EV_O3TEMP(IO3HR) * O3_PPM
                        END IF
C ---                   Save this hour's O3CONC to array of previous 
C                       24 values for this sector
                        O3_Max24hr(IO3HR,I) = EV_O3TEMP(IO3HR)
                     ELSE IF (L_O3VALUES(I) .OR.
     &                           L_O3VAL(I)) THEN
C ---                   Hourly O3 value is missing; assign O3SUB value;
C                       these have already been converted to ug/m3
                        EV_O3TEMP(IO3HR) = O3SUB(I)
C ---                   Assign 0.0 to O3_Max24hr array so that subbed value will
C                       not be used in determining max value from previous 24 hrs
                        O3_Max24hr(IO3HR,I) = 0.0D0
                        IF (.NOT. L_SkipMessages) THEN
                          WRITE(DUMMY,'(I10.10,''S'',I1)') FULLO3HR(I),I
                          CALL ERRHDL(PATH,MODNAM,'I','458',DUMMY)
                        END IF
                     ELSE
C ---                   Assign 0.0 to O3_Max24hr array
                        O3_Max24hr(IO3HR,I) = 0.0D0
                        IF (.NOT. L_SkipMessages) THEN
                          WRITE(DUMMY,'(I10.10,''S'',I1)') FULLO3HR(I),I
                          CALL ERRHDL(PATH,MODNAM,'I','459',DUMMY)
                        END IF
                     END IF
                  END IF

               END IF   ! IF-THEN block for reading hourly O3FILEs

            ELSE
C ---          No hourly O3 data available; apply O3SUB based on non-hourly data
C              if this is the applicable sector
               IF (I .EQ. IO3SECT) THEN
                  EV_O3CONC(IHR) = O3SUB(I)
               END IF

C ---          Calculate full date for this hour of O3 data
C              CERC 11/30/20 - commenting this out as don't think it should
C              be done if O3 is not being read in from a file, since IO3YR
C              etc have not been set.
C               FULLO3HR(I) = IO3YR*1000000 + IO3MN*10000 + 
C     &                                         IO3DY*100 + IO3HR

            END IF

         END DO      ! END of Sector Loop

         IF (L_AO3MISS(IHR)) THEN
C ---       No O3 value available for this hour; full conversion 
C           is assumed (subject to equilibrium ratio); issue an
C           informational message
            EV_O3CONC(IHR) = 0.0D0
            IF (.NOT. L_SkipMessages) THEN
               WRITE(DUMMY,'(I10.10)') 100*(FULLDATE/100)+IHR
               CALL ERRHDL(PATH,MODNAM,'I','459',DUMMY)
            END IF
         END IF
                    
      END DO     ! Hour of Hour Loop

      DO I = 1, NUMO3Sects
C ---    Loop through O3SECTORs
         IF (FULLO3HR(I) .GT. 0) THEN
C*          Recalculate full date with last value of IO3HR (should be = 24) for 
C*          comparison to FULLDATE, since FULLDATE is set by MEREAD based on a 
C*          loop through one day of meteorological data and reflects HR 24.
C*          Check for Date and Time Consistency ; If Failed, Issue Fatal Error
C*          Ignore hour in checking for date consistency with met data
            FULLO3YYMMDD = (FULLO3HR(I)/100) * 100
            IF (FULL_YYMMDD .NE. FULLO3YYMMDD) THEN
C*             WRITE Error Message - Date mismatch
               WRITE(DUMMY,'(I10.10,''S'',I1)') FULLO3HR(I), I
               CALL ERRHDL(PATH,MODNAM,'E','457',DUMMY)
               RUNERR = .TRUE.
            END IF
         END IF
      END DO

      GO TO 1000

C*    Write Error Message for Error Reading Hourly Ozone File
 99   CONTINUE
      WRITE(DUMMY,'(''O3FILE SECT'',I1)') DUMMY
      CALL ERRHDL(PATH,MODNAM,'E','510',DUMMY)
      RUNERR = .TRUE.

 999  CONTINUE
 
C --- End of file reached on O3 file

1000  RETURN
      END

      SUBROUTINE NOXREAD
C***********************************************************************
C*                  NOXREAD Module of AERMOD
C* 
C*         PURPOSE: To Read a 24-hour Block of NOx background Data
C* 
C*         PROGRAMMER:  CERC
C* 
C*         DATE:    November 2020
C* 
C
C*         INPUTS:
C* 
C*         OUTPUTS:
C*          
C*         CALLED FROM:
C************************************************************************
C*
C*    Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      DOUBLE PRECISION :: NOXMIN
      DOUBLE PRECISION :: NOXSUB(6)
      DOUBLE PRECISION :: EV_NOXTEMP(24)
      INTEGER :: I, IHR, INOXYR2, INOXYR, INOXMN, INOXDY, INOXHR 
      INTEGER :: FULLNOXHR(6), FULLNOXYYMMDD
C*    Variable Initializations
      MODNAM  = 'NOXREAD'

C --- Initialize full date variable for all sectors to 0
      FULLNOXHR(:)  = 0
C --- Initialize NOXSUB substitution values to 0
      NOXSUB(:)     = 0.0D0
      EV_NOXTEMP(:) = 0.0D0
C --- Initialise NOXMISS logical array to FALSE for all hours
      L_ANOXMISS(:) = .FALSE.

C --- Loop through the current day
      DO IHR = 1, 24
C ---    Initialize EV_NOXCONC, NOXMIN to 0.0 and other arrays to -99.
         EV_NOXCONC(IHR) =   0.0D0
         EV_NOXTEMP(IHR) = -99.0D0
         NOXSUB(:) = -99.0D0
         NOXMIN    = 0.0D0
!
C ---    Assign local IHR index to global IHOUR index; since this
C        may be used to identify temporally-varying NOX values
         IHOUR = IHR
         
C ---    Assign NOXSECT array value to scalar variable
         INOXSECT = ANOXSECT(IHR)

         DO I = 1, NUMNOxSects
C ---       Loop through NOXSECTORs

C ---       Reinitialize NOXSUB for this sector
            NOXSUB(I) = -99.0D0

C ---       Check for temporally-varying NOX concentrations
            IF (L_NOX_VALS(I)) THEN
               CALL VARYNOXVALS(I,NOXSUB(I))
            ELSE IF (L_NOXVALUE(I)) THEN
               NOXSUB(I) = NOXBACK(I)
            ELSE
               NOXSUB(I) = 0.0D0
            END IF

            IF (L_NOxHourly) THEN
C ---          Hourly NOx data is available; read and process the data

               IF (L_NOxFILE(I)) THEN
C ---             Hourly NOx file available for current sector

                  IF (I .EQ. INOXSECT) THEN
C ---                This is the applicable sector for this hour; read next hour of NOx data

                     IF (NOXFORM(I) .EQ. 'FREE') THEN
                        READ(INOXUNT(I),*,ERR=99,END=999) INOXYR, 
     &                                           INOXMN, INOXDY, INOXHR,
     &                                           EV_NOXCONC(INOXHR)
                     ELSE
                        READ(INOXUNT(I),NOXFORM(I),ERR=99,END=999)
     &                                                   INOXYR, INOXMN,
     &                                                   INOXDY, INOXHR,
     &                                                EV_NOXCONC(INOXHR)
                     END IF

C ---                Determine 4-digit year
                     IF (INOXYR .LE. 99) THEN
                        INOXYR2 = INOXYR
                        IF (INOXYR2 .GE. ISTRT_WIND .AND. 
     &                                       INOXYR2 .LE. 99) THEN
                           INOXYR  = ISTRT_CENT*100 + INOXYR2
                        ELSE IF (INOXYR2 .LT. ISTRT_WIND) THEN
                           INOXYR  = (ISTRT_CENT+1)*100 + INOXYR2
                        END IF
                     END IF
                   
C ---                Calculate full date for this hour of NOx data
                     FULLNOXHR(I) = INOXYR*1000000 + INOXMN*10000 + 
     &                                               INOXDY*100 + INOXHR

                     IF (EV_NOXCONC(INOXHR) .GE. 0.0D0) THEN
C ---                   Valid hourly value; convert to ug/m3 if needed
C ---                   using NO2 factors (NOx expressed as 'NOx as NO2')
                        IF (NOXFILUNITS .EQ. 'PPB') THEN
                           EV_NOXCONC(INOXHR) = EV_NOXCONC(INOXHR) /
     &                                                           NO2_PPB
                        ELSE IF (NOXFILUNITS .EQ. 'PPM') then
                           EV_NOXCONC(INOXHR) = EV_NOXCONC(INOXHR) / 
     &                                                           NO2_PPM
                        END IF
                        !Ensure non-negative
                        EV_NOXCONC(INOXHR)=MAX(EV_NOXCONC(INOXHR),
     &                                                           NOXMIN)
                     ELSE IF (L_NOX_VALS(INOXSECT) .OR.
     &                           L_NOXVALUE(INOXSECT)) THEN
C ---                   Hourly NOx values is missing; assign NOX_VALS value;
C                       these have already been converted to ug/m3
                        EV_NOXCONC(INOXHR) = NOXSUB(INOXSECT)
                        IF (.NOT. L_SkipMessages) THEN
                          WRITE(DUMMY,'(I10.10,''S'',I1)') FULLNOXHR(I),
     &                                                                 I
                          CALL ERRHDL(PATH,MODNAM,'I','610',DUMMY)
                        END IF
                     ELSE
C ---                   Assign L_ANOXMISS logical to TRUE for this hour
                        L_ANOXMISS(INOXHR) = .TRUE.
                        IF (.NOT. L_SkipMessages) THEN
                          WRITE(DUMMY,'(I10.10,''S'',I1)') FULLNOXHR(I),
     &                                                                 I
                          CALL ERRHDL(PATH,MODNAM,'I','609',DUMMY)
                        END IF
                     END IF

                  ELSE
C ---                This is not applicable sector for this hour; however, read 
C                    NOX values to keep track of 24hr max value for this sector
                     IF (NOXFORM(I) .EQ. 'FREE') THEN
                       READ(INOXUNT(I),*,ERR=99,END=999) INOXYR, INOXMN,
     &                                                  INOXDY, INOXHR,
     &                                               EV_NOXTEMP(INOXHR)
                     ELSE
                       READ(INOXUNT(I),NOXFORM(I),ERR=99,END=999)
     &                                                  INOXYR, INOXMN,
     &                                                  INOXDY, INOXHR,
     &                                               EV_NOXTEMP(INOXHR)
                     END IF

C ---                Check for use of 2-digit year in NOX_FILE file, adjust to 4-digit
C                    year for comparison with FULLDATE based on met data file
                     IF (INOXYR .LE. 99) THEN
                        INOXYR2 = INOXYR
                        IF (INOXYR2 .GE. ISTRT_WIND .AND. 
     &                                       INOXYR2 .LE. 99) THEN
                           INOXYR  = ISTRT_CENT*100 + INOXYR2
                        ELSE IF (INOXYR2 .LT. ISTRT_WIND) THEN
                           INOXYR  = (ISTRT_CENT+1)*100 + INOXYR2
                        END IF
                     END IF
                   
C ---                Calculate full date for this hour of NOx data
                     FULLNOXHR(I) = INOXYR*1000000 + INOXMN*10000 + 
     &                                               INOXDY*100 + INOXHR

                     IF (EV_NOXTEMP(INOXHR) .GE. 0.0D0) THEN
C ---                   Valid hourly value; convert to ug/m3 if needed
C ---                   using NO2 factors (NOx expressed as 'NOx as NO2')
                        IF (NOXFILUNITS .EQ. 'PPB') THEN
                           EV_NOXTEMP(INOXHR)=EV_NOXTEMP(INOXHR)/NO2_PPB
                        ELSE IF (NOXFILUNITS .EQ. 'PPM') then
                           EV_NOXTEMP(INOXHR)=EV_NOXTEMP(INOXHR)/NO2_PPM
                        END IF
                     ELSE IF (L_NOX_VALS(I) .OR.
     &                           L_NOXVALUE(I)) THEN
C ---                   Hourly NOx value is missing; assign NOXSUB value;
C                       these have already been converted to ug/m3
                        EV_NOXTEMP(INOXHR) = NOXSUB(I)
                        IF (.NOT. L_SkipMessages) THEN
                          WRITE(DUMMY,'(I10.10,''S'',I1)')FULLNOxHR(I),I
                          CALL ERRHDL(PATH,MODNAM,'I','610',DUMMY)
                        END IF
                     ELSE
                        IF (.NOT. L_SkipMessages) THEN
                          WRITE(DUMMY,'(I10.10,''S'',I1)')FULLNOXHR(I),I
                          CALL ERRHDL(PATH,MODNAM,'I','609',DUMMY)
                        END IF
                     END IF
                  END IF

               END IF   ! IF-THEN block for reading hourly NOXFILEs

            ELSE
C ---          No hourly NOx data available as yet; apply NOXSUB based on non-hourly data
C              if this is the applicable sector
               IF (I .EQ. INOXSECT) THEN
                  EV_NOXCONC(IHR) = NOXSUB(I)
               END IF

C ---          Calculate full date for this hour of NOx data
C              CERC 11/30/20 - commenting this out as don't think it should
C              be done if NOx is not being read in from a file, since INOXYR
C              etc have not been set.
C               FULLNOXHR(I)=INOXYR*1000000 + INOXMN*10000 + 
C     &                                                INOXDY*100 +INOXHR
            END IF
          END DO    ! END of Sector Loop 

         IF(L_ANOXMISS(IHR))THEN
C ---       No NOx value available for this hour; zero conc is assumed
            EV_NOXCONC(IHR)=0.0D0
            IF (.NOT. L_SkipMessages) THEN
              WRITE(DUMMY,'(I10.10)') 100*(FULLDATE/100)+IHR
              CALL ERRHDL(PATH,MODNAM,'I','607',DUMMY)
            END IF
         END IF

       END DO !End of Hour loop
         
       DO I = 1, NUMNOxSects
C ---    Loop through NOxSectors           
         IF (FULLNOXHR(I) .GT. 0) THEN
C*          Recalculate full date with last value of INOXHR (should be =24) for
C*          comparison to FULLDATE, since FULLDATE is set by MEREAD based on a 
C*          loop through one day of meteorological data and reflects HR 24.
C*          Check Date and Time Consistency ; If Failed, Issue Fatal Error
C*          Ignore hour in checking for date consistency with met data   
            FULLNOXYYMMDD = (FULLNOXHR(I)/100)*100     
            IF (FULL_YYMMDD .NE. FULLNOXYYMMDD) THEN
C*             WRITE Error Message - Date mismatch
               WRITE(DUMMY, '(I10.10,''S'',I1)') FULLNOXHR(I), I
               CALL ERRHDL(PATH,MODNAM,'E','608',DUMMY)
               RUNERR = .TRUE.
            END IF
         END IF
      END DO
      
      GOTO 1000
      
C*    Write Error Message for Error Reading Hourly NOx File
 99   CONTINUE
      WRITE(DUMMY,'(''NOXFILE SCT'',I1)') DUMMY
      CALL ERRHDL(PATH,MODNAM,'E','510',DUMMY)
      RUNERR = .TRUE.

 999  CONTINUE
 
C --- End of file reached on NOx file

1000  RETURN
      END SUBROUTINE NOXREAD

      SUBROUTINE BGREAD
C***********************************************************************
C*                  BGREAD Module of AERMOD
C* 
C*         PURPOSE: To Read a 24-hour Block of Background Data
C* 
C*         PROGRAMMER:  Roger Brode
C* 
C*         DATE:    February 28, 2011
C* 
C*         MODIFIED:   Modified subroutine BGREAD to move the unit conversion for 
C*                     hourly background concentrations to follow the READ statements 
C*                     to avoid "double counting" unit conversion for non-hourly 
C*                     background since unit conversion for BGSUB has already been 
C*                     applied in sub_BGVAL.
C*                     R.W. Brode, U.S. EPA/OAQPS/AQMG, XX/YY/2013
C*         
C*         INPUTS:
C* 
C*         OUTPUTS:
C*          
C*         CALLED FROM:
C************************************************************************
C*
C*    Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I
      DOUBLE PRECISION :: BGSUB(6)
      INTEGER :: IHR, IBGYR, IBGMN, IBGDY, IBGHR, IBGYR2
      INTEGER :: FULLBGHR(6), FULLBGYYMMDD

C     Variable Initializations
      MODNAM  = 'BGREAD'
      FULLBGHR(:) = 0
      BGSUB(:)    = -99.0D0

      DO IHR = 1, 24

C ---    Initialize EV_BGCONC to missing
         EV_BGCONC(IHR) = -99.0D0

C ---    Assign local IHR index to global IHOUR index; since this
C        may be used to identify temporally-varying BG values
         IHOUR = IHR
         
C ---    Assign BGSECT array value to scalar variable
         IBGSECT = ABGSECT(IHR)

         DO I = 1, NUMBGSects

C ---       Reinitialize BGSUB for this sector
            BGSUB(I) = 0.0D0

C ---       Check for temporally-varying background to substitute for missing hours
            IF (L_BGValues(I)) THEN
               CALL BGVAL(I,BGSUB(I))
            ELSE
               BGSUB(I) = 0.0D0
            END IF
            
            IF (L_BGFile(I)) THEN
C ---          Hourly BACKGRND data file available

               IF (I .EQ. IBGSECT) THEN
C ---             This is the applicable sector; read hourly BGCONC

C*                Retrieve hourly background concentrations      
                  IF (BGFORM(I) .EQ. 'FREE') THEN
                     READ(IBGUNT(I),*,ERR=99,END=999) IBGYR, IBGMN, 
     &                                                IBGDY, IBGHR, 
     &                                                EV_BGCONC(IBGHR)
                  ELSE
                     READ(IBGUNT(I),BGFORM(I),ERR=99,END=999) 
     &                                                IBGYR, IBGMN,
     &                                                IBGDY, IBGHR,
     &                                                EV_BGCONC(IBGHR)
                  END IF

C ---             Adjust background concentration units to UG/M3 if needed 
C                 for hourly background; unit conversion for BGSUB is 
C                 applied in subroutine BGVAL;
C                 conversion is based on reference temperature (25C) and
C                 pressure (1013.25 mb)
                  IF (EV_BGCONC(IBGHR) .GE. 0.0D0) THEN
                     IF (POLLUT .EQ. 'NO2') THEN
                        IF (BackUnits .EQ. 'PPB') THEN
                           EV_BGCONC(IBGHR) = EV_BGCONC(IBGHR) / NO2_PPB
                        ELSE IF (BackUnits .EQ. 'PPM') THEN
                           EV_BGCONC(IBGHR) = EV_BGCONC(IBGHR) / NO2_PPM
                        END IF
                     ELSE IF (POLLUT .EQ. 'SO2') THEN
                        IF (BackUnits .EQ. 'PPB') THEN
                           EV_BGCONC(IBGHR) = EV_BGCONC(IBGHR) / SO2_PPB
                        ELSE IF (BackUnits .EQ. 'PPM') THEN
                           EV_BGCONC(IBGHR) = EV_BGCONC(IBGHR) / SO2_PPM
                        END IF
                     ELSE IF (POLLUT .EQ. 'CO') THEN
                        IF (BackUnits .EQ. 'PPB') THEN
                           EV_BGCONC(IBGHR) = EV_BGCONC(IBGHR) * CO_PPB
                        ELSE IF (BackUnits .EQ. 'PPM') THEN
                           EV_BGCONC(IBGHR) = EV_BGCONC(IBGHR) * CO_PPM
                        END IF
                     END IF
                  END IF

C ---             Check for use of 2-digit year in background file, adjust to 4-digit
C                 year for comparison with FULLDATE based on met data file
                  IF (IBGYR .LE. 99) THEN
                     IBGYR2 = IBGYR
                     IF (IBGYR2 .GE. ISTRT_WIND .AND. 
     &                                          IBGYR2 .LE. 99) THEN
                        IBGYR  = ISTRT_CENT*100 + IBGYR2
                     ELSE IF (IBGYR2 .LT. ISTRT_WIND) THEN
                        IBGYR  = (ISTRT_CENT+1)*100 + IBGYR2
                     END IF
                  END IF
              
C ---             Calculate full date for this hour of BACKGRND data
                  FULLBGHR(I) = IBGYR*1000000 + IBGMN*10000 + IBGDY*100 
     &                                                      + IBGHR
      
               ELSE
C ---             This is not applicable sector for this hour; read record without data

                  IF (BGFORM(I) .EQ. 'FREE') THEN
                     READ(IBGUNT(I),*,ERR=99,END=999) IBGYR, IBGMN, 
     &                                                IBGDY, IBGHR 
                  ELSE
                     READ(IBGUNT(I),BGFORM(I),ERR=99,END=999) 
     &                                                IBGYR, IBGMN,
     &                                                IBGDY, IBGHR
                  END IF
              
C ---             Check for use of 2-digit year in background file, adjust to 4-digit
C                 year for comparison with FULLDATE based on met data file
                  IF (IBGYR .LE. 99) THEN
                     IBGYR2 = IBGYR
                     IF (IBGYR2 .GE. ISTRT_WIND .AND. 
     &                                          IBGYR2 .LE. 99) THEN
                        IBGYR  = ISTRT_CENT*100 + IBGYR2
                     ELSE IF (IBGYR2 .LT. ISTRT_WIND) THEN
                        IBGYR  = (ISTRT_CENT+1)*100 + IBGYR2
                     END IF
                  END IF

C ---             Calculate full date for this hour of BACKGRND data
                  FULLBGHR(I) = IBGYR*1000000 + IBGMN*10000 + IBGDY*100 
     &                                                      + IBGHR

               END IF

            END IF

         END DO    ! NUMBGSects Loop

         IF (EV_BGCONC(IHR) .LT. 0.0D0) THEN
C ---       Hourly BGCONC is missing; look for substitution values
            IF (IBGSECT .GT. 0) THEN
C ---          Valid BGSECT defined, check for hourly values for this 
C              sector, and then for non-hourly values to substitute
               IF (L_BGFile(IBGSECT)) THEN
                  IF (L_BGValues(IBGSECT)) THEN
C                    Hourly background value is missing but non-hourly 
C                    values have been specified for substitution, 
C                    which were processed in subroutine BGVAL;
                     EV_BGCONC(IHR) = BGSUB(IBGSECT)
C                    Write informational message 
                     WRITE(DUMMY,'(I10.10,''S'',I1)') 
     &                                    100*(FULLDATE/100)+IHR,IBGSECT
                     CALL ERRHDL(PATH,MODNAM,'I','453',DUMMY)
C ---                Increment counter for number of missing BGval substitutions
                     NSubBGHOUR = NSubBGHOUR + 1
                  ELSE
C                    Hourly background value is missing for this sector and no 
C                    non-hourly values specified for substitution;
C                    Write Error message 
                     WRITE(DUMMY,'(I10.10,''s'',I1)') 
     &                                    100*(FULLDATE/100)+IHR,IBGSECT
                     CALL ERRHDL(PATH,MODNAM,'E','452',DUMMY)
                     RUNERR = .TRUE.
                     GO TO 1000
                  END IF
               ELSE
                  IF (L_BGValues(IBGSECT)) THEN
C                    Hourly background value is missing but non-hourly 
C                    values have been specified for substitution, 
C                    which were processed in subroutine BGVAL;
                     EV_BGCONC(IHR) = BGSUB(IBGSECT)
                  END IF
               END IF
            ELSE
C ---          IBGSECT .LE. 0 due to calm/msg hr; Set EV_BGCONC to 0 and exit
               EV_BGCONC(IHR) = 0.0D0
            END IF
         END IF
               
      END DO    ! Hour Loop

C*    Check for Date and Time Consistency Across all Sectors; If Failed, Issue Fatal Error
      DO I = 1, NUMBGSects
         IF (FULLBGHR(I) .GT. 0) THEN
C*          Check for Date and Time Consistency based on YR/MN/DY 
C*          since EVENTS are processed by day, but events within 
C*          the day may not be in sequence. 
C*          If Failed, Issue Fatal Error
            FULLBGYYMMDD = (FULLBGHR(I)/100) * 100
            IF (FULL_YYMMDD .NE. FULLBGYYMMDD) THEN
C*             WRITE Error Message - Date mismatch
               WRITE(DUMMY,'(I10.10,''S'',I1)') FULLBGHR(I), I
               CALL ERRHDL(PATH,MODNAM,'E','454',DUMMY)
               RUNERR = .TRUE.
            END IF
         END IF
      END DO

      GO TO 1000

C*    Write Error Message for Error Reading Hourly BACKGRND File
 99   CONTINUE
      WRITE(DUMMY,'(''BGFILE SECT'',I1)') IBGSECT
      CALL ERRHDL(PATH,MODNAM,'E','510',DUMMY)
      RUNERR = .TRUE.
      GO TO 1000

 999  CONTINUE
 
1000  RETURN
      END
