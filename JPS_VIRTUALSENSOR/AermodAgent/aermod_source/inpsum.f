      SUBROUTINE INPSUM
C***********************************************************************
C                 INPSUM Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Print Out The Input Data Summary
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   Print summary of Model Options and Met Data
C                    to optional SUMMFILE.
C                    R. W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
C
C        INPUTS:  Arrays of Source Parameters
C                 Arrays of Receptor Locations
C                 Arrays of Model Results
C
C        OUTPUTS: Printed Model Outputs
C
C        CALLED FROM:   MAIN
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      INTEGER I
      CHARACTER MODNAM*12
C Unused: INTEGER ILEN, NOPS

C     Variable Initializations
      MODNAM = 'INPSUM'

C     Print Out The Model Options
      CALL PRTOPT(IOUNIT)
      IF (SUMMFILE) THEN
C ---    Include Model Options Summary in SUMMFILE
         CALL PRTOPT(ISUMUNT)
      END IF

C --- Print temporally varying ozone concentrations,
C     if specified on the O3VALUES keyword
      DO I = 1, NUMO3Sects
         IF (L_O3VALUES(I)) THEN
            CALL PRTO3VALS(I)
         END IF
      END DO

C --- CERC 11/30/20 Print temporally varying NOx concentrations,
C     if specified on the NOX_VALS keyword
      DO I = 1, NUMNOxSects
         IF (L_NOX_VALS(I)) THEN
            CALL PRTNOXVALS(I)
         END IF
      END DO

C     Print Out The Input Source Data
      CALL PRTSRC

C --- Print Out Background Concentrations, if specified
      IF (L_BACKGRND) THEN
         DO I = 1, NUMBGSects
            CALL PRTBKG(I)
         END DO
      END IF

      IF (.NOT. EVONLY) THEN
C        Print Out The Input Receptor Coordinates.
         CALL PRTREC

C        Check For Receptors Too Close To Sources (< 1m or < 3Lb)
         CALL CHKREC
      END IF

C     Print Out The Input Met Data Summary
      CALL PRTMET(IOUNIT)
      IF (SUMMFILE) THEN
C ---    Include Met Data Summary in SUMMFILE
         CALL PRTMET(ISUMUNT)
      END IF

      RETURN
      END

      SUBROUTINE PRTOPT(IOUNT)
C***********************************************************************
C                 PRTOPT Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Print Out The Model Options and Keyword Summary
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To summarize information about multiple buoyant lines
C                    (Multiple_BuoyLines_D41_Wood)
C
C        MODIFIED:   To include GRSM NO2 option.
C                    CERC, 11/30/20
C
C        MODIFIED:   Added summary information for a RLINE source,
C                    including Barrier/Depressed ALPHA options.
C                    Wood, 7/20/2018
C
C        MODIFIED:   To include note regarding special processing
C                    requirements for PM.25, 1hr NO2 and 1hr SO2.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
C
C        MODIFIED:   To include DFAULT urban roughness length,
C                    provide a more complete summary of options
C                    when DFAULT option is not specified, clarify
C                    options and emissions/output units related to
C                    deposition algorithms, address EVENT vs.
C                    non-EVENT processing options, and provide a
C                    more "refined" estimate of memory storage
C                    requirements in STORE.
C                    Include output file unit calling argument to
C                    support printing summary to main 'aermod.out'
C                    file and to the optional SUMMFILE.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
C
C        MODIFIED:   To remove reference to "upper bound" values for
C                    supersquat buildings, and to summarize inputs for
C                    multiple urban areas.
C                    Roger Brode, MACTEC (f/k/a PES), Inc., - 08/25/05
C
C        MODIFIED:   To Remove Summary of Keywords Table
C                    Roger Brode, PES, Inc.,  - 11/08/94
C
C        MODIFIED:   To add pathway 'TG' to process input file of Gridded
C                    Terrain data.
C                    D. Strimaitis, SRC - 11/8/93
C
C        MODIFIED:   To add DDEP and WDEP parameters to CONC/DEPOS options
C                    to allow just the wet or just the dry deposition flux
C                    to be reported.  DEPOS now reports the sum of wet and
C                    dry fluxes.  Expand keywords to include input of wet
C                    scavenging coefficients (SO path).  Add override of
C                    Intermediate Terrain so that results are for only the
C                    simple terrain or the complex terrain model.
C                    D. Strimaitis, SRC - 11/8/93
C
C        MODIFIED:  To Include TOXXFILE Option - 9/29/92
C
C        INPUTS:  Model Options and Keyword Summarys
C
C        OUTPUTS: Printed Model Outputs
C
C        CALLED FROM:   INPSUM
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      USE RLINE_DATA, ONLY: NRLINES, L_RDEPRESS, L_RBARRIER
      USE BUOYANT_LINE

      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: NOPS

      INTEGER :: I, ILMAX, IOUNT, NumBack,
     &           NumHrlySect,   NumNonHrlySect,
     &           NumHrlyO3Sect, NumNonHrlyO3Sect,
     &           NumHrlyNOxSect, NumNonHrlyNOxSect,
     &           ILEN, ILEN1, ILEN2
CCRT  CRT, 3/18/2022 D063 Platform Downwash, number of platform sources
      INTEGER :: NumPFSrcs
      CHARACTER (LEN=80) :: BFLAG_String,  BFLAG_TempString,
     &                      BGSECT_String
      CHARACTER (LEN=80) :: O3FLAG_String, O3FLAG_TempString,
     &                      O3SECT_String
C Unused:      CHARACTER (LEN=80) :: AWMADW_String, ORDDW_String
      CHARACTER (LEN=10):: DEBUG_OPTS(11)
      CHARACTER (LEN=80):: DEBUG_OPTS_String
      CHARACTER (LEN=30):: CHRAVES
      CHARACTER (LEN=80) :: NOXFLAG_String, NOXFLAG_TempString,
     &                      NOXSECT_String

C     Variable Initializations
      MODNAM = 'PRTOPT'
      BFLAG_String = ' '
      BFLAG_TempString = ' '
      BGSECT_String = ' '
      O3FLAG_String = ' '
      O3FLAG_TempString = ' '
      O3SECT_String = ' '
      NOXFLAG_String = ' '
      NOXFLAG_TempString = ' '
      NOXSECT_String = ' '
      DEBUG_OPTS = ''
      CHRAVES    = ''
      ILEN  = 0
      ILEN1 = 0
      ILEN2 = 0
      NOPS = 0
      ILEN = 0
C     Initialize counter for number of source groups with background
      NumBack = 0
C     Initialize counters for number of sectors with hourly and non-hourly background
      NumHrlySect = 0
      NumNonHrlySect = 0
      NumHrlyO3Sect = 0
      NumHrlyNOxSect = 0
      NumNonHrlyO3Sect = 0
      NumNonHrlyNOxSect = 0

CCRT  CRT, 3/18/2022 D063 Platform Downwash, initialize then get number of platform sources
      NumPFSrcs = 0

C     Get number of platform sources
      DO I = 1, SIZE(OSPLAT)
         IF (OSPLAT(I)) THEN
            NumPFSrcs = NumPFSrcs + 1
         END IF
      END DO

C     Summarize The Model Options
      CALL HEADER(IOUNT)
      WRITE(IOUNT,9041)
C     How are options specified
      WRITE (IOUNT, *) '** Model Options Selected:'

      IF (DFAULT) THEN
         WRITE(IOUNT,*) '     * Model Uses Regulatory DEFAULT Options'
      ELSE
         WRITE(IOUNT,*) '     * Model Allows User-Specified Options'
      END IF


C     Model Setup is for...


      IF (CONC) THEN
         WRITE(IOUNT,*) '     * Model Is Setup For Calculation of ',
     &        'Average CONCentration Values.'
      END IF
      IF (DEPOS) THEN
         WRITE(IOUNT,*) '     * Model Is Setup For Calculation of ',
     &        'Total DEPOSition Values.'
      END IF
      IF (DDEP) THEN
         WRITE(IOUNT,*) '     * Model Is Setup For Calculation of ',
     &        'Dry DEPosition Values.'
      END IF
      IF (WDEP) THEN
         WRITE(IOUNT,*) '     * Model Is Setup For Calculation of ',
     &        'Wet DEPosition Values.'
      END IF

C     Dry Depostion Logic

      IF (LDGAS .AND. LUSERVD) THEN
         WRITE(IOUNT,*) '     * User-specified GAS DRY ',
     &                  ' DEPOSITION Velocity Provided.'
      ELSE IF (LDGAS .OR. LWGAS) THEN
         WRITE(IOUNT,*) '     * GAS DEPOSITION Data Provided.'
      ELSE
         WRITE(IOUNT,*) '     * NO GAS DEPOSITION Data Provided.'
      END IF

      IF (LDPART) THEN
         WRITE(IOUNT,*) '     * PARTICLE DEPOSITION Data Provided.'
      ELSE
         WRITE(IOUNT,*) '     * NO PARTICLE DEPOSITION Data Provided.'
      END IF

      IF (DDPLETE) THEN
         WRITE(IOUNT,*) '     * Model Uses DRY DEPLETION. ',
     &        'DDPLETE  = ',
     &                    DDPLETE
              IF (ARDPLETE) THEN
              WRITE(IOUNT,*)
     &                  '        with AREADPLT option    AREADPLT = ',
     &                    ARDPLETE
              ELSE IF (ROMBERG) THEN
              WRITE(IOUNT,*)
     &                  '        with ROMBERG option     ROMBERG  = ',
     &                    ROMBERG
              END IF
      ELSE
         WRITE(IOUNT,*) '     * Model Uses NO DRY DEPLETION. ',
     &                  'DDPLETE  = ',
     &                    DDPLETE
      END IF
C     Wet Depostion Logic
      IF (WDPLETE) THEN
         WRITE(IOUNT,*) '     * Model Uses WET DEPLETION. ',
     &           'WETDPLT  = ',
     &                    WDPLETE
      ELSE
         WRITE(IOUNT,*) '     * Model Uses NO WET DEPLETION. '       ,
     &          'WETDPLT  = ',
     &                    WDPLETE
      END IF

C     Stack Tip or Not
      IF (.NOT. DFAULT) THEN
          IF (NOSTD) THEN
             WRITE(IOUNT,*) '     * No Stack-tip Downwash'
          ELSE
             WRITE(IOUNT,*) '     * Stack-tip Downwash.'
         END IF
      ELSE
         WRITE(IOUNT,*) '     * Stack-tip Downwash.'
      END IF

C     Terrain Options
      IF (DFAULT) THEN
         WRITE(IOUNT,*) '     * Model Accounts for ELEVated ',
     &           'Terrain Effects.'
      ELSE
             IF (FLATSRCS) THEN
                 WRITE(IOUNT,*) '     * Allow FLAT/ELEV ',
     &              'Terrain Option by Source,'
                 WRITE(IOUNT,9049) NUMFLAT, NUMSRC-NUMFLAT
             ELSE IF (FLAT) THEN
                 WRITE(IOUNT,*) '     * Model Assumes Receptors on ',
     &              'FLAT Terrain.'
             ELSE IF (ELEV) THEN
                 WRITE(IOUNT,*) '     * Model Accounts for ELEVated ',
     &              'Terrain Effects.'
             END IF
      END IF

C      Calms Processing
      WRITE(IOUNT,*) '     * Use Calms Processing ',
     &           'Routine.'

C     Missing Data Processing
      WRITE(IOUNT,*) '     * Use Missing Data ',
     &           'Processing Routine.'

C     Exponential Decay (SO2)
      IF (DFAULT) THEN
          IF (URBAN .AND. POLLUT .EQ. 'SO2') THEN
              WRITE(IOUNT,*) '     * Half-life of 4 hrs for',
     &              ' URBAN SO2.'
          ELSE
              WRITE(IOUNT,*) '     * No Exponential Decay.'
          END IF
      END IF
      IF (.NOT.DFAULT) THEN
C ---    Check for Non-DFAULT Half-life for URBAN SO2 application
         IF (URBAN .AND. POLLUT.EQ.'SO2') THEN
             IF( ICSTAT(7) .EQ. 1 .OR. ICSTAT(8) .EQ. 1 )THEN
C ----           User-specified HALFLIFE or DCAYCOEF
                 IF (DABS(DECOEF-4.81D-5) .LE. 5.0D-8) THEN
                     WRITE(IOUNT,*) '     * Half-life of 4 hrs for',
     &  ' URBAN SO2.'
                 ELSE
                     WRITE(IOUNT,*) '     * Non-DFAULT Half-life for ',
     &              ' URBAN SO2.'
                 END IF
             ELSE IF (ICSTAT(7) .EQ. 0 .AND. ICSTAT(8) .EQ. 0) THEN
                 WRITE(IOUNT,*) '     * Half-life of 4 hrs for',
     &              ' URBAN SO2.'
             END IF
         ELSE IF (DECOEF .NE. 0.0D0) THEN
             WRITE(IOUNT,*) '     * Non-DFAULT Exponential',
     &              ' Decay.'
         ELSE
             WRITE(IOUNT,*) '     * No Exponential Decay.'
         END IF
      END IF

C     NO2 Conversion
      IF (POLLUT .EQ. 'NO2') THEN
         IF (PVMRM) THEN
             WRITE(IOUNT,*)
     &           '     * Plume Volume Molar Ratio Method (PVMRM)',
     *           'Used for NO2 Conversion'
             WRITE(IOUNT,9090) NO2Equil
9090         FORMAT('        with an Equilibrium NO2/NOx ',
     &         'Ratio of ',F6.3)
             WRITE(IOUNT,9091) NO2Stack
9091         FORMAT('        and with a Default In-stack ',
     &                 'Ratio of ',F6.3)
             IF (PSDCREDIT) THEN
                 WRITE(IOUNT,*)'     * ALPHA Option for  ',
     &           'Calculating PSD Increment'
                 WRITE(IOUNT,*)'     * Consumption with PSD ',
     &                           'Credits Selected.'
             END IF
         ELSE IF (OLM) THEN
             WRITE(IOUNT,*) '     * Ozone Limiting Method (OLM) ',
     &                 'Used for NO2 Conversion'
             WRITE(IOUNT,90901) NO2Equil
90901        FORMAT('        with an Equilibrium NO2/NOx ',
     &                 'Ratio of ',F6.3,' and')
             IF (NUMOLM .GT. 0) THEN
                 WRITE(IOUNT,90911) NUMOLM
90911            FORMAT('        with ',I3,' OLMGROUP(s)')
                     ELSE
                     WRITE(IOUNT,90912)
90912                FORMAT('        with NO OLMGROUPs')
             END IF
         ELSE IF (RUNTTRM .OR. RUNTTRM) THEN
               WRITE(IOUNT,*) '     * Travel Time Reaction Method',
     &                 'Used for NO2 Conversion'
             WRITE(IOUNT,90901) NO2Equil
         ELSE IF (ARM2) THEN
             WRITE(IOUNT,*) '     * Ambient Ratio ',
     &                 'Method Ver 2 (ARM2) Used for NO2 Conversion'
             WRITE(IOUNT,9990) ARM2_Min
9990         FORMAT('         with a Minimum NO2/NOx ',
     &                 'Ratio of ',F6.3)
             WRITE(IOUNT,9991) ARM2_Max
9991         FORMAT('         and a Maximum NO2/NOx ',
     &                 'Ratio of ',F6.3)
         ELSE IF (GRSM) THEN
C            CERC 11/30/20
             WRITE(IOUNT,*) '     * GRS Chemistry ',
     &                'Method (GRSM) Used for NO2 Conversion'
         ELSE
             WRITE(IOUNT,*) '     * Full Conversion ',
     &                 'Assumed for NO2.'
         END IF
      END IF

C     Zo and urban Alg
      IF (.NOT.URBAN) THEN
         WRITE(IOUNT,*) '     * Model Uses RURAL Dispersion Only.'
      ELSE IF (URBAN) THEN
         WRITE(IOUNT,9039) NURBSRC, NUMURB
9039     FORMAT(1X,'     * Model Uses URBAN Dispersion Algorithm ',
     &                   'for the SBL for ',I5,' Source(s),'
     &            /'        for Total of ',I4,' Urban Area(s):')
         IF (NUMURB .GT. 1) THEN
             DO I = 1, NUMURB
                 WRITE(IOUNT,9040) URBID(I), URBPOP(I), URBZ0(I)
9040             FORMAT(8X,'Urban ID = ',A8,' ;  Urban Population = ',
     &             F11.1,' ;  Urban Roughness Length = ',F6.3,' m')
              END DO
         ELSE
             WRITE(IOUNT,90401) URBPOP(1), URBZ0(1)
90401        FORMAT(3X,'Urban Population = ',
     &          F11.1,' ;  Urban Roughness Length = ',F6.3,' m')
         END IF

         IF (MAXVAL(URBZ0).NE.1.0D0 .OR.
     &      MINVAL(URBZ0).NE.1.0D0) THEN
            WRITE(IOUNT,*) '     * Non-DFAULT Urban ',
     &                  'Roughness Length(s) Used.'
         ELSE
            WRITE(IOUNT,*) '     * Urban Roughness Length ' ,
     &                     'of 1.0 Meter Used.'
         END IF
         IF (.NOT. L_UrbanTransition) THEN
            WRITE(IOUNT,90402)
90402       FORMAT(1X,'     * Non-DFAULT option to ignore morning ',
     &          'transition from nighttime urban boundary',
     &         /'        layer (NoUrbTran) selected.')
         END IF
      END IF

C     Capped Option Used
      IF ( NUMCAP.GT.0 .OR. NUMHOR.GT.0 ) THEN
         WRITE(IOUNT,*) '     * Option for Capped &',
     &                   ' Horiz Stacks Selected With:'
         WRITE(IOUNT,9092) NUMCAP, NUMHOR
9092     FORMAT(1X,I10,' Capped Stack(s); and',I10,
     &                          ' Horizontal Stack(s)')
      END IF

C     Other Options
      IF (NOCHKD) THEN
         WRITE(IOUNT,*) '     * NOCHKD   - Suppresses checking',
     &                   ' of date sequence in meteorology files'
      ELSE IF (L_WARNCHKD) THEN
         WRITE(IOUNT,*) '     * WARNCHKD - Issues warning messages',
     &                   ' for records out of sequence'
         WRITE(IOUNT,*) '       in meteorology files'
      END IF

      IF (NOWARN) THEN
         WRITE(IOUNT,*) '     * NOWARN   - Suppresses writing',
     &                   ' of warning messages in main print file'
      END IF
      IF (FASTALL) THEN
         WRITE(IOUNT,*) '     * FASTALL  - Use effective sigma-y to',
     &                   ' optimize meander for '
         WRITE(IOUNT,*) ' POINT and VOLUME',
     &                   ' sources, and hybrid approach'
         WRITE(IOUNT,*) 'to optimize AREA sources',
     &                   ' (formerly TOXICS option)'
      ELSE IF (FASTAREA) THEN
         WRITE(IOUNT,*) '     * FASTAREA - Use hybrid approach to',
     &                   ' optimize AREA sources;'
         WRITE(IOUNT,*) '                   also applies to LINE ',
     &                            'sources (formerly TOXICS option)'
      END IF
      IF (SCIM) THEN
         WRITE(IOUNT,*) '     * SCIM  - Use Sampled',
     &                   ' Chronological Input Model (SCIM) option'
      END IF

      IF (SCREEN) THEN
         WRITE(IOUNT,*) '     * SCREEN   - Use screening option ',
     &                   'which forces calculation of centerline values'
      END IF

C     3/18/22 Wood D127 - added FRANMIN to LOW_WIND option
C     4/12/2022 CRT D131 - added PBAL to LOW_WIND option
C     4/12/2022 CRT - add more summary info for user-specified options
      IF (LOW_WIND) THEN
         WRITE(IOUNT,*)'     * Use LOW_WIND ALPHA option'
         IF (L_UserSVmin .OR. L_UserWSmin .OR. L_UserFRANmax .OR.
     &       L_UserSWmin .OR. L_UserBigT .OR. L_UserFRANmin .OR.
     &       L_PBal) THEN
             WRITE(IOUNT,*)
     &         '        with the following parameters:'
            IF (L_UserSVmin) THEN
                WRITE(IOUNT,'(12x,"* SVMin =   ",F8.2," m/s")') SVMin
            END IF
            IF (L_UserWSmin) THEN
                WRITE(IOUNT,'(12x,"* WSMin =   ",F8.2," m/s")') WSMin
            END IF
            IF (L_UserFRANmin) THEN
                WRITE(IOUNT,'(12x,"* FRANMin = ",F8.2)') FRANMIN
            END IF
            IF (L_UserSWmin) THEN
                WRITE(IOUNT,'(12x,"* SWMin =   ",F8.2," m/s")') SWMin
            END IF
            IF (L_UserBigT) THEN
                WRITE(IOUNT,'(12x,"* BigT =    ",F8.2," hours")') BIGT
            END IF
            IF (L_UserFRANmax) THEN
                WRITE(IOUNT,'(12x,"* FRANMax = ",F8.2)') FRANMAX
            END IF
            IF (L_PBal) THEN
                WRITE(IOUNT,'(12x,"* PBalance")')
            END IF
         END IF
      END IF
      IF (L_AdjUstar) THEN
         WRITE(IOUNT,*) '     * ADJ_U*   - Use ADJ_U* option ',
     &                                     'for SBL in AERMET'
      END IF

      IF (L_MMIF_Data) THEN
         WRITE(IOUNT,*) '     * MMIFData - Use MMIF met data inputs'
         IF( LEN_TRIM(MMIF_Version).GT.0 )THEN
             WRITE(IOUNT,*) '                   ',MMIF_Version
         END IF
      END IF

      IF (L_BULKRN) THEN
         IF( L_MMIF_Data )THEN
         WRITE(IOUNT,*) '     * BULKRN   - Use BULKRN Delta-T and ',
     &                     'SolarRad option for SBL with MMIF'
         ELSE
         WRITE(IOUNT,*) '     * BULKRN   - Use BULKRN Delta-T and ',
     &                     'SolarRad option for SBL in AERMET'
         END IF
      END IF

      IF (L_VECTORWS) THEN
         WRITE(IOUNT,*) '     * VECTORWS - User specifies ',
     &                     'that input wind speeds are VECTOR means'
      END IF
      IF (L_CCVR_Sub) THEN
         WRITE(IOUNT,*) '     * CCVR_Sub - Meteorological data ',
     &                                     'includes CCVR substitutions'
      END IF

      IF (L_TEMP_Sub) THEN
         WRITE(IOUNT,*) '     * TEMP_Sub - Meteorological data ',
     &                                     'includes TEMP substitutions'
      END IF

C     JAT 1/29/21 D070 TURBULENCE OPTIONS
C     ADD TEXT TO WRITE FOR TURBULENCE OPTIONS
      IF (TURBOPTS(1)) THEN
         WRITE(IOUNT,*) '     * NOTURB - Meteorological data ',
     &                                'Ignore turbulence - all hours'
      END IF
      IF (TURBOPTS(2)) THEN
         WRITE(IOUNT,*) '     * NOTURBST - Meteorological data ',
     &                        'Ignore turbulence - stable hours'
      END IF
      IF (TURBOPTS(3)) THEN
         WRITE(IOUNT,*) '     * NOTURBCO - Meteorological data ',
     &                        'Ignore turbulence - convective hours'
      END IF

      IF (TURBOPTS(4)) THEN
         WRITE(IOUNT,*) '     * NOSA - Meteorological data ',
     &                        'Ignore sigma-theta - all hours'
      END IF

      IF (TURBOPTS(5)) THEN
         WRITE(IOUNT,*) '     * NOSW - Meteorological data ',
     &                        'Ignore sigma-w - all hours'
       END IF

      IF (TURBOPTS(6)) THEN
         WRITE(IOUNT,*) '     * NOSAST - Meteorological data ',
     &                        'Ignore sigma-theta - stable hours'
      END IF

      IF (TURBOPTS(7)) THEN
         WRITE(IOUNT,*) '     * NOSWST - Meteorological data ',
     &                        'Ignore sigma-w - stable hours'
       END IF

      IF (TURBOPTS(8)) THEN
         WRITE(IOUNT,*) '     * NOSACO - Meteorological data ',
     &                        'Ignore sigma-theta - convective hours'
      END IF

      IF (TURBOPTS(9)) THEN
         WRITE(IOUNT,*) '     * NOSWCO - Meteorological data ',
     &                        'Ignore sigma-w - convective hours'
      END IF

      IF (L_RDEPRESS) THEN
         WRITE(IOUNT,*) '     * RDEPRESS - Use ALPHA option ',
     &                                     'for RLINEXT with depression'
      END IF

      IF (L_RBARRIER) THEN
         WRITE(IOUNT,*) '     * RBARRIER - Use ALPHA option ',
     &                                     'for RLINEXT with barrier'
      END IF
C
      IF (FLGPOL) THEN
         WRITE(IOUNT,*) '     * Model Accepts FLAGPOLE Receptor .',
     &     ' Heights. '
      ELSE
         WRITE(IOUNT,*) '     * Model Assumes No FLAGPOLE Receptor ',
     &      'Heights. '
      END IF

C     Write Out Pollutant Type
      WRITE(IOUNT,9048) POLLUT

C --- Include note regarding 24-hr PM2.5, 1-hr NO2 and 1-hr SO2 processing;
C     including a note regarding 1hr NO2/SO2 and 24hr PM25 NAAQS processing
C     being disabled by user, if applicable.
      IF ((POLLUT .EQ. 'PM25' .OR. POLLUT .EQ. 'PM-2.5' .OR.
     &     POLLUT .EQ. 'PM-25'.OR. POLLUT .EQ. 'PM2.5') .AND.
     &                                     .NOT.EVONLY) THEN
         IF (PM25AVE) THEN
            WRITE(IOUNT,99090)
         ELSE IF (L_NO_PM25AVE) THEN
            WRITE(IOUNT,99190)
         END IF
      ELSE IF (POLLUT .EQ. 'NO2' .AND. .NOT.EVONLY) THEN
         IF (NO2AVE) THEN
C ---       Special processing for 1-hr NO2 NAAQS
C           is applicable
            WRITE(IOUNT,99091)
         ELSE IF (L_NO_NO2AVE) THEN
C ---       User has disabled special processing for
C           1-hr NO2 NAAQS processing
            WRITE(IOUNT,99191) NO2_FIELD4(1:3)
         ELSE IF (.NOT.NO2AVE .AND. .NOT.L_NO_NO2AVE) THEN
C ---       Special processing for 1-hr NO2 NAAQS is NOT
C           applicable due to non-standard averaging period(s)
            DO I = 1, NUMAVE
               IF (CHRAVE(I) .NE. ' 1-HR') THEN
                 ILEN = LEN_TRIM(CHRAVES)
                 CHRAVES = CHRAVES(1:ILEN)//'  '//CHRAVE(I)
               END IF
            END DO
            WRITE(IOUNT,99291) CHRAVES
         END IF
      ELSE IF (POLLUT .EQ. 'SO2' .AND. .NOT.EVONLY) THEN
         IF (SO2AVE) THEN
C ---       Special processing for 1-hr SO2 NAAQS
C           is applicable
            WRITE(IOUNT,99092)
         ELSE IF (L_NO_SO2AVE) THEN
C ---       User has disabled special processing for
C           1-hr SO2 NAAQS processing
            WRITE(IOUNT,99192) SO2_FIELD4(1:3)
         ELSE IF (.NOT.SO2AVE .AND. .NOT.L_NO_SO2AVE) THEN
C ---       Special processing for 1-hr SO2 NAAQS is NOT
C           applicable due to non-standard averaging period(s)
            DO I = 1, NUMAVE
               IF (CHRAVE(I) .NE. ' 1-HR') THEN
                 ILEN = LEN_TRIM(CHRAVES)
                 CHRAVES = CHRAVES(1:ILEN)//'  '//CHRAVE(I)
               END IF
            END DO
            WRITE(IOUNT,99292) CHRAVES
         END IF
      END IF

C     Modeled average period(s) summary
      WRITE(IOUNT,9099)
      IF (PERIOD) THEN
         IF (NUMAVE .GT. 0) THEN
            WRITE(IOUNT,9042) NUMAVE, (CHRAVE(I),I=1,NUMAVE)
            WRITE(IOUNT,9043)
         ELSE
            WRITE(IOUNT,9045)
         END IF
      ELSE IF (ANNUAL) THEN
         IF (NUMAVE .GT. 0) THEN
            WRITE(IOUNT,9042) NUMAVE, (CHRAVE(I),I=1,NUMAVE)
            WRITE(IOUNT,9143)
         ELSE
            WRITE(IOUNT,9145)
         END IF
      ELSE
         WRITE(IOUNT,9042) NUMAVE, (CHRAVE(I),I=1,NUMAVE)
      END IF

C     Write Out Numbers of Sources, Groups, and Receptors for This Run
      WRITE(IOUNT,9099)
      IF (EVONLY) THEN
         WRITE(IOUNT,9046) NUMSRC, NUMGRP, NUMEVE, NUMPNT, NUMCAP,
     &                     NUMHOR, NUMVOL, NUMAREA, NUMLINE, NRLINES,
     &                     NUMPIT, NUMBLGRPS, NBLTOTAL, NUMSWP
      ELSE IF (.NOT. EVONLY) THEN
         WRITE(IOUNT,9044) NUMSRC, NUMGRP, NUMREC, NUMPNT, NUMCAP,
     &                     NUMHOR, NUMVOL, NUMAREA, NUMLINE, NRLINES,
     &                     NUMPIT, NUMBLGRPS, NBLTOTAL, NUMSWP
      END IF

C     CRT, 3/18/2022 D063 Write number of point, pointhor, pointcap
C     sources subject to platform downwash
      IF ((NUMPNT .GT. 0 .OR. NUMCAP .GT. 0 .OR. NUMHOR .GT. 0) .AND.
     &     NumPFSrcs .GT. 0) THEN
         WRITE (IOUNT, 9059) NumPFSrcs
 9059    FORMAT(1X,'**Number of Platform Point Sources:', I6)
      END IF


      IF (L_AWMADW) THEN
         WRITE (IOUNT, 9060)
 9060    FORMAT(1X, '**AWMA Downwash Options Specified:')
         IF (L_AWMA_Ueff) THEN
            WRITE (IOUNT, 9061)
 9061       FORMAT(10X, 'AWMAUEFF')
         END IF

         IF (L_AWMA_UTurb .AND. .NOT. L_AWMA_UTurbHX) THEN
            WRITE (IOUNT, 9062)
 9062       FORMAT(10X, 'AWMAUTURB')
         END IF

         IF (L_AWMA_UTurbHX) THEN
            WRITE (IOUNT, 9064)
 9064       FORMAT(10X, 'AWMAUTURBHX')
         END IF
      END IF

      IF (L_STRMLN_BLDG) THEN
            WRITE (IOUNT, 9063)
 9063       FORMAT(10X, 'Streamlined Buildings')
      END IF

      IF (L_AWMA_Entrain) THEN
         WRITE (IOUNT, 9200)
 9200    FORMAT(10X, 'AWMAENTRAIN')
      END IF

      IF (L_ORDDW) THEN
         WRITE (IOUNT, 9165)
 9165    FORMAT(1X,'**ORD Downwash Options Specified:')
         IF (L_ORD_Ueff) THEN
            WRITE (IOUNT, 9166)
 9166       FORMAT(10X, 'ORDUEFF')
         END IF
         IF (L_ORD_Turb) THEN
            WRITE (IOUNT, 9167)
 9167       FORMAT(10X, 'ORDTURB')
         END IF

         IF (L_ORD_Cav) THEN
            WRITE (IOUNT, 9168)
 9168       FORMAT(10X, 'ORDCAV')
         END IF
      END IF

C --- Indicate whether background concentrations are included in this run
      IF (L_BACKGRND) THEN
C        Determine how many source groups include background
         NumBack = 0
         DO I = 1, NUMGRP
            IF (GRP_BACK(I)) THEN
               NumBack = Numback + 1
            END IF
         END DO

         IF (L_BGSector) THEN
C ---       BGSECTOR Option; First summarize how many SRCGRPs and how many Sectors
            WRITE(BGSECT_String,'(6I4)')
     &             (NINT(BGSECT(I)),I=1,NUMBGSects)
            WRITE(IOUNT,19047) NumBack, NUMBGSects,
     &                         BGSECT_String(1:LEN_TRIM(BGSECT_String))
19047       FORMAT(/1X,'**This Run Includes BACKGRND Values',
     &      ' for ',I6,' Source Group(s) Varying Across ',I3,
     &      ' Downwind Sector(s): ',A:)

C ---       Determine number of sectors with HOURLY BACKGRND
            BGSECT_String = ''
            DO I = 1, NUMBGsects
               IF (L_BGFile(I)) THEN
                  WRITE(BGSECT_String,'(A,I4)')
     &             BGSECT_String(1:LEN_TRIM(BGSECT_String)),
     &                                      NINT(BGSECT(I))
                  NumHrlySect = NumHrlySect + 1
               END IF
            END DO

C ---       Summarize how many sectors have HOURLY BACKGRND
            IF (NumHrlySect .GT. 0) THEN
               WRITE(IOUNT,19048) NumHrlySect,
     &                        BGSECT_String(1:LEN_TRIM(BGSECT_String))
19048          FORMAT(1X,'             HOURLY BACKGRND Values',
     &                   ' are Available for ',I3,
     &                   ' Downwind Sector(s): ',A:)
            END IF

C ---       Determine number of sectors with Non-HOURLY BACKGRND
            BGSECT_String = ''
            DO I = 1, NUMBGsects
               IF (L_BGValues(I)) THEN
                  WRITE(BGSECT_String,'(A,I4)')
     &             BGSECT_String(1:LEN_TRIM(BGSECT_String)),
     &                                      NINT(BGSECT(I))
                  NumNonHrlySect = NumNonHrlySect + 1
               END IF
            END DO

C ---       Non-HOURLY BACKGRND concentrations available
C           First reinitialize BFLAG_String and BFLAG_TempString
            BFLAG_String = ''
            BFLAG_TempString = ''
            IF (NumNonHrlySect .GT. 0) THEN
               WRITE(IOUNT,19049) NumNonHrlySect,
     &                        BGSECT_String(1:LEN_TRIM(BGSECT_String))
19049          FORMAT(1X,'         Non-HOURLY BACKGRND Values',
     &                   ' are Available for ',I3,
     &                   ' Downwind Sector(s): ',A:)
            END IF

C ---       Summarize Non-HOURLY BACKGRND options for missing HOURLY
            DO I = 1, NUMBGsects
               IF (L_BGValues(I) .AND. L_BGFile(I)) THEN
                  ILEN1 = LEN_TRIM(BFLAG_String)
                  ILEN2 = LEN_TRIM(BFLAG(I))
                  WRITE(BFLAG_TempString,'(A,1x,A)')
     &                  BFLAG_String(1:ILEN1),BFLAG(I)(1:ILEN2)
                  BFLAG_String = BFLAG_TempString
               END IF
            END DO
            IF (LEN_TRIM(BFLAG_String) .GT. 0) THEN
               WRITE(IOUNT,19050)
     &                         BFLAG_String(1:LEN_TRIM(BFLAG_String))
19050          FORMAT(1X,'     Missing HOURLY BACKGRND Values ',
     &            'are Filled Based on Values Varying by: ',
     &             A:)
            END IF

C ---       Summarize all Non-HOURLY BACKGRND options available
            BFLAG_String = ''
            DO I = 1, NUMBGsects
               IF (L_BGValues(I)) THEN
                  ILEN1 = LEN_TRIM(BFLAG_String)
                  ILEN2 = LEN_TRIM(BFLAG(I))
                  WRITE(BFLAG_TempString,'(A,1x,A)')
     &                  BFLAG_String(1:ILEN1),BFLAG(I)(1:ILEN2)
                  BFLAG_String = BFLAG_TempString
               END IF
            END DO
            IF (LEN_TRIM(BFLAG_String) .GT. 0) THEN
               WRITE(IOUNT,19051) BFLAG_String(1:LEN_TRIM(BFLAG_String))
19051       FORMAT(1X,'         Non-HOURLY BACKGRND Values',
     &            ' are Available Varying by: ',A:)
            END IF

         ELSE
C ---       No BGSECTORs

C ---       First indicate how many source groups include BACKGRND
            WRITE(IOUNT,29047) NumBack
29047       FORMAT(/1X,'**This Run Includes BACKGRND Values ',
     &      'for ',I6,' Source Group(s) for a Single Sector')

C ---       Next check for HOURLY or non-hourly BACKGRND values
            IF (L_BGFile(1)) THEN
               WRITE(IOUNT,29048)
29048          FORMAT(1X,'             HOURLY BACKGRND Values',
     &                   ' are Available')
               IF (LEN_TRIM(BFLAG(1)) .GT. 0) THEN
                  WRITE(IOUNT,19050) BFLAG(1)(1:LEN_TRIM(BFLAG(1)))
               END IF
            ELSE
               IF (LEN_TRIM(BFLAG(1)) .GT. 0) THEN
                  WRITE(IOUNT,19051) BFLAG(1)(1:LEN_TRIM(BFLAG(1)))
               END IF
            END IF
         END IF

      END IF

      IF (OLM .OR. PVMRM .OR. GRSM) THEN
C ---    Summarize OZONE data inputs, includeing O3SECTOR options
C ---    First summarize how many Sectors
         IF (L_O3Sector) THEN
            WRITE(O3SECT_String,'(6I4)')
     &             (NINT(O3SECT(I)),I=1,NUMO3Sects)
            WRITE(IOUNT,19057) NUMO3Sects,
     &                         O3SECT_String(1:LEN_TRIM(O3SECT_String))
19057       FORMAT(/1X,'**This Run Includes OZONE    Values',
     &      ' that Vary Across  ',I3,' Downwind Sector(s): ',A:)
C ---       Determine number of sectors with HOURLY OZONE
            O3SECT_String = ''
            DO I = 1, NUMO3sects
               IF (L_O3File(I)) THEN
                  WRITE(O3SECT_String,'(A,I4)')
     &             O3SECT_String(1:LEN_TRIM(O3SECT_String)),
     &                                      NINT(O3SECT(I))
                  NumHrlyO3Sect = NumHrlyO3Sect + 1
               END IF
            END DO

C ---       Summarize how many sectors have HOURLY OZONE
            IF (NumHrlyO3Sect .GT. 0) THEN
               WRITE(IOUNT,19058) NumHrlyO3Sect,
     &                        O3SECT_String(1:LEN_TRIM(O3SECT_String))
19058          FORMAT(1X,'             HOURLY OZONE    Values',
     &                   ' are Available for ',I3,
     &                   ' Downwind Sector(s): ',A:)
            END IF

C ---       Determine number of sectors with Non-HOURLY OZONE
            O3SECT_String = ''
            DO I = 1, NUMO3sects
               IF (IO3SET(I) .GT. 0 .OR. L_O3VAL(I)) THEN
                  WRITE(O3SECT_String,'(A,I4)')
     &             O3SECT_String(1:LEN_TRIM(O3SECT_String)),
     &                                      NINT(O3SECT(I))
                  NumNonHrlyO3Sect = NumNonHrlyO3Sect + 1
               END IF
            END DO

C ---       Non-HOURLY OZONE concentrations available
C           First reinitialize O3FLAG_String and O3FLAG_TempString
            O3FLAG_String = ''
            O3FLAG_TempString = ''
            IF (NumNonHrlyO3Sect .GT. 0) THEN
               WRITE(IOUNT,19059) NumNonHrlyO3Sect,
     &                        O3SECT_String(1:LEN_TRIM(O3SECT_String))
19059          FORMAT(1X,'         Non-HOURLY OZONE    Values',
     &                   ' are Available for ',I3,
     &                   ' Downwind Sector(s): ',A:)
            END IF

C ---       Summarize Non-HOURLY OZONE options for missing HOURLY O3
            DO I = 1, NUMO3sects
               IF ((IO3SET(I).GT.0 .OR. L_O3VAL(I)) .AND.
     &                                 L_O3File(I)) THEN
                  IF (IO3SET(I) .GT. 0) THEN
                     ILEN1 = LEN_TRIM(O3FLAG_String)
                     ILEN2 = LEN_TRIM(O3FLAG(I))
                  WRITE(O3FLAG_TempString,'(A,1x,A)')
     &                  O3FLAG_String(1:ILEN1),O3FLAG(I)(1:ILEN2)
                  ELSE IF (L_O3VAL(I)) THEN
                     ILEN1 = LEN_TRIM(O3FLAG_String)
                     ILEN2 = LEN_TRIM('ANNUAL')
                  WRITE(O3FLAG_TempString,'(A,1x,A)')
     &                  O3FLAG_String(1:ILEN1),'ANNUAL'
                  END IF
                  O3FLAG_String = O3FLAG_TempString
               END IF
            END DO
            IF (LEN_TRIM(O3FLAG_String) .GT. 0) THEN
               WRITE(IOUNT,19060)
     &                         O3FLAG_String(1:LEN_TRIM(O3FLAG_String))
19060          FORMAT(1X,'     Missing HOURLY OZONE    Values ',
     &            'are Filled Based on Values Varying by: ', A:)
            END IF

C ---       O3FLAG_String all Non-HOURLY OZONE options available
            O3FLAG_String = ''
            DO I = 1, NUMO3sects
               IF (IO3SET(I) .GT. 0 .OR. L_O3VAL(I)) THEN
                  IF (IO3SET(I) .GT. 0) THEN
                     ILEN1 = LEN_TRIM(O3FLAG_String)
                     ILEN2 = LEN_TRIM(O3FLAG(I))
                  WRITE(O3FLAG_TempString,'(A,1x,A)')
     &                  O3FLAG_String(1:ILEN1),O3FLAG(I)(1:ILEN2)
                  ELSE IF (L_O3VAL(I)) THEN
                     ILEN1 = LEN_TRIM(O3FLAG_String)
                     ILEN2 = LEN_TRIM('ANNUAL')
                  WRITE(O3FLAG_TempString,'(A,1x,A)')
     &                  O3FLAG_String(1:ILEN1),'ANNUAL'
                  END IF
                  O3FLAG_String = O3FLAG_TempString
               END IF
            END DO
            IF (LEN_TRIM(O3FLAG_String) .GT. 0) THEN
               WRITE(IOUNT,19061)
     &                         O3FLAG_String(1:LEN_TRIM(O3FLAG_String))
19061       FORMAT(1X,'         Non-HOURLY OZONE    Values',
     &            ' are Available Varying by: ',A:)
            END IF

         ELSE
C ---       No O3SECTORs

            WRITE(IOUNT,29057)
29057       FORMAT(/1X,'**This Run Includes OZONE    Values ',
     &                 'for a Single Sector')

C ---       Next check for HOURLY or non-hourly OZONE values
            IF (L_O3File(1)) THEN
               NumHrlyO3Sect = NumHrlyO3Sect + 1
            END IF
            IF (NumHrlyO3Sect .GE. 1) THEN
               WRITE(IOUNT,29058)
29058          FORMAT(1X,'             HOURLY OZONE    Values ',
     &                   'are Available')
               IF (LEN_TRIM(O3FLAG(1)) .GT. 0) THEN
                  WRITE(IOUNT,19060) O3FLAG(1)(1:LEN_TRIM(O3FLAG(1)))
               END IF
            ELSE
               IF (LEN_TRIM(O3FLAG(1)) .GT. 0) THEN
                  WRITE(IOUNT,19061) O3FLAG(1)(1:LEN_TRIM(O3FLAG(1)))
               END IF
            END IF
         END IF
      END IF

C     CERC 11/30/20
      IF (GRSM) THEN
C ---    Summarize NOx data inputs, including NOXSECTR options
C ---    First summarize how many Sectors
         IF (L_NOxSector) THEN
            WRITE(NOXSECT_String,'(6I4)')
     &             (NINT(NOXSECT(I)),I=1,NUMNOxSects)
            WRITE(IOUNT,19062) NUMNOXSects,
     &                        NOXSECT_String(1:LEN_TRIM(NOXSECT_String))
19062       FORMAT(/1X,'**This Run Includes NOx      Values',
     &      ' that Vary Across  ',I3,' Downwind Sector(s): ',A:)

C ---       Determine number of sectors with HOURLY NOX
            NOXSECT_String= ' '
            DO I =1,NUMNOxSects
                IF(L_NOxFile(I))THEN
                    WRITE(NOXSECT_String,'(A,I4)')
     &                NOXSECT_STring(1:LEN_TRIM(NOXSECT_String)),
     &                                          NINT(NOXSECT(I))
                      NumHrlyNOXSect = NumHrlyNOxSect + 1
                END IF
            END DO

C ---      Summarize how many sectors have HOURLY NOX
           IF(NumHrlyNOxSect .GT. 0)THEN
               WRITE(IOUNT,19065) NumHrlyNOxSect,
     &                        NOXSECT_String(1:LEN_TRIM(NOXSECT_String))
19065          FORMAT(1X,'             HOURLY NOX      Values',
     &                   ' are Available for ',I3,
     &                   ' Downwind Sector(s): ',A:)
            END IF

C ---       Determine number of sectors with Non-HOURLY NOx
            NOXSECT_String = ''
            DO I = 1, NUMNOxSects
               IF (INOXSET(I) .GT. 0 .OR. L_NOXVALUE(I)) THEN
                  WRITE(NOXSECT_String,'(A,I4)')
     &             NOXSECT_String(1:LEN_TRIM(NOXSECT_String)),
     &                                      NINT(NOXSECT(I))
                  NumNonHrlyNOXSect = NumNonHrlyNOXSect + 1
               END IF
            END DO
C ---       Non-HOURLY NOx concentrations available
C           First reinitialize NOxFLAG_String and NOxFLAG_TempString
            NOXFLAG_String = ''
            NOXFLAG_TempString = ''
            IF (NumNonHrlyNOxSect .GT. 0) THEN
               WRITE(IOUNT,19063) NumNonHrlyNOxSect,
     &                        NOxSECT_String(1:LEN_TRIM(NOxSECT_String))
19063          FORMAT(1X,'         Non-HOURLY NOx      Values',
     &                   ' are Available for ',I3,
     &                   ' Downwind Sector(s): ',A:)
            END IF

C ---       Summarize Non-Hourly NOX options for missing HOURLY NOx
            DO I = 1, NumNOxsects
                IF ((INOXSET(I).GT.0 .OR. L_NOXVALUE(I)) .AND.
     &                                   L_NOxFile(I)) THEN
                    IF(INOXSET(I).GT.0)THEN
                        ILEN1=LEN_TRIM(NOXFLAG_String)
                        ILEN2=LEN_TRIM(NOXFLAG(I))
                        WRITE(NOXFLAG_TempString,'(A,1x,A)')
     &                       NOXFLAG_String(1:ILEN1),NOXFLAG(I)(1:ILEN2)
                    ELSE IF (L_NOXVALUE(I))THEN
                        ILEN1=LEN_TRIM(NOXFLAG_String)
                        ILEN2=LEN_TRIM('ANNUAL')
                        WRITE(NOXFLAG_TempString,'(A,1X,A)')
     &                        NOXFLAG_String(1:ILEN1),'ANNUAL'
                    END IF
                    NOXFLAG_String=NOXFLAG_TempString
               END IF
           END DO
           IF(LEN_TRIM(NOXFLAG_String).GT.0)THEN
               WRITE(IOUNT,19066)
     &                        NOXFLAG_String(1:LEN_TRIM(NOXFLAG_String))
19066          FORMAT(1X,'     Missing HOURLY NOX      Values ',
     &            'are Filled Based on Values Varying by: ', A:)
            END IF

C ---       NOxFLAG_String all Non-HOURLY NOx options available
            NOxFLAG_String = ''
            DO I = 1, NUMNOxSects
               IF (INOxSET(I) .GT. 0 .OR. L_NOXVALUE(I)) THEN
                  IF (INOxSET(I) .GT. 0) THEN
                     ILEN1 = LEN_TRIM(NOxFLAG_String)
                     ILEN2 = LEN_TRIM(NOxFLAG(I))
                  WRITE(NOxFLAG_TempString,'(A,1x,A)')
     &                  NOxFLAG_String(1:ILEN1),NOxFLAG(I)(1:ILEN2)
                  ELSE IF (L_NOXVALUE(I)) THEN
                     ILEN1 = LEN_TRIM(NOxFLAG_String)
                     ILEN2 = LEN_TRIM('ANNUAL')
                  WRITE(NOxFLAG_TempString,'(A,1x,A)')
     &                  NOxFLAG_String(1:ILEN1),'ANNUAL'
                  END IF
                  NOxFLAG_String = NOxFLAG_TempString
               END IF
            END DO
            IF (LEN_TRIM(NOxFLAG_String) .GT. 0) THEN
               WRITE(IOUNT,19064)
     &                        NOxFLAG_String(1:LEN_TRIM(NOxFLAG_String))
19064       FORMAT(1X,'         Non-HOURLY NOx      Values',
     &            ' are Available Varying by: ',A:)
            END IF
         ELSE
C ---       No NOxSECTORs
            WRITE(IOUNT,29065)
29065       FORMAT(/1X,'**This Run Includes NOx      Values ',
     &                 'for a Single Sector')
C ---       Next check for HOURLY or non-hourly NOX values
            IF(L_NOxFile(1))THEN
                NumHrlyNOxSect = NumHrlyNOxSect + 1
            END IF
            IF(NumHrlyNOxSect .GE. 1)THEN
                WRITE(IOUNT,29066)
29066           FORMAT(1X,'             HOURLY NOX      Values ',
     &                   'are Available')
                IF (LEN_TRIM(NOxFLAG(1)) .GT. 0) THEN
                   WRITE(IOUNT,19066) NOXFLAG(1)(1:LEN_TRIM(NOxFLAG(1)))
                END IF
            ELSE
                IF (LEN_TRIM(NOxFLAG(1)) .GT. 0) THEN
                   WRITE(IOUNT,19064) NOXFLAG(1)(1:LEN_TRIM(NOxFLAG(1)))
                END IF
            END IF
         END IF
      END IF

C     Model Run OR Not Options
      WRITE(IOUNT,9099)
      IF (RUN) THEN
         WRITE(IOUNT,*) '**Model Set To Continue RUNning After the ',
     &         'Setup Testing.'
      ELSE
         WRITE(IOUNT,*) '**Model Will NOT Run After the ',
     &         'Setup Testing.'
      END IF

C --- Write the AERMET version date from the surface file header record
      WRITE(IOUNT,99093) C_METVER

      WRITE(IOUNT,9099)
C     Model Output Options Setting Summary
      WRITE(IOUNT,9070)
      IF (EVONLY) THEN
C        Write output option for EVENT processing
         IF (SOCONT) THEN
            WRITE(IOUNT,98071)
         ELSE IF (DETAIL) THEN
            WRITE(IOUNT,98072)
         END IF
      ELSE
C        Write output options for non-EVENT processing
         IF (PERIOD) THEN
C           PERIOD Averages by Receptor Are Output
            WRITE(IOUNT,9071)
         ELSE IF (ANNUAL) THEN
C           ANNUAL Averages by Receptor Are Output
            WRITE(IOUNT,9171)
         END IF
         IF (IOSTAT(2) .GT. 0) THEN
C           RECTABLE Keyword Used
            WRITE(IOUNT,9072)
         END IF
         IF (IOSTAT(3) .GT. 0) THEN
C           MAXTABLE Keyword Used
            WRITE(IOUNT,9073)
         END IF
         IF (IOSTAT(4) .GT. 0) THEN
C           DAYTABLE Keyword Used
            WRITE(IOUNT,9074)
         END IF
         IF (IOSTAT(5) .GT. 0) THEN
C           MAXIFILE Keyword Used
            WRITE(IOUNT,9075)
         END IF
         IF (IOSTAT(6) .GT. 0) THEN
C           POSTFILE Keyword Used
            WRITE(IOUNT,9076)
         END IF
         IF (IOSTAT(7) .GT. 0) THEN
C           PLOTFILE Keyword Used
            WRITE(IOUNT,9077)
         END IF
         IF (IOSTAT(8) .GT. 0) THEN
C           TOXXFILE Keyword Used
            WRITE(IOUNT,9078)
         END IF
         IF (IOSTAT(9) .GT. 0) THEN
C           SEASONHR Keyword Used
            WRITE(IOUNT,99071)
         END IF
         IF (IOSTAT(10) .GT. 0) THEN
C           RANKFILE Keyword Used
            WRITE(IOUNT,99072)
         END IF
         IF (IOSTAT(11) .GT. 0) THEN
C           EVALFILE Keyword Used
            WRITE(IOUNT,99073)
         END IF
         IF (IOSTAT(12) .GT. 0) THEN
C           SUMMFILE Keyword Used
            WRITE(IOUNT,99074)
         END IF
         IF (IOSTAT(14) .GT. 0) THEN
C           MAXDAILY Keyword Used
            WRITE(IOUNT,99173)
         END IF
         IF (IOSTAT(15) .GT. 0) THEN
C           MXDYBYYR Keyword Used
            WRITE(IOUNT,99273)
         END IF
         IF (IOSTAT(16) .GT. 0) THEN
C           MAXDCONT Keyword Used
            WRITE(IOUNT,99373)
         END IF
      END IF

C --- Check for user-specified option for exponential-format outputs
      IF ( FILE_FORMAT .EQ. 'EXP' .AND.
     &    (MXFILE .OR. PPFILE .OR. RKFILE .OR. ANPOST .OR. ANPLOT .OR.
     &     SEASONHR) ) THEN
         WRITE(IOUNT,9099)
         WRITE(IOUNT,99075)
      END IF

C     Write Explanatory Note About Calm and Missing Flags
      IF (CLMPRO .OR. MSGPRO) THEN
         WRITE(IOUNT,9099)
         WRITE(IOUNT,9079) CHIDEP(3,1)
      END IF

C     Model Misc. Information
      WRITE(IOUNT,9099)
      WRITE(IOUNT,9050) ZBASE, DECOEF, ROTANG
C --- Write out emission and output units
      IF (NUMTYP .EQ. 1) THEN
C ---    Only one output type; write out units without label
         WRITE(IOUNT,9055) EMILBL(1), EMIFAC(1), OUTLBL(1)
      ELSE IF (CONC) THEN
C ---    More than one output type including CONC;
C        Write out CONC units first with label, followed by
C        deposition units (for DEPOS, DDEP and/or WDEP)
         WRITE(IOUNT,90551) EMILBL(1), EMIFAC(1), OUTLBL(1)
         WRITE(IOUNT,90552) EMILBL(2), EMIFAC(2), OUTLBL(2)
      ELSE
C ---    More than one output type but no CONC;
C        Write out units for deposition without label
         WRITE(IOUNT,9055) EMILBL(1), EMIFAC(1), OUTLBL(1)
      END IF

      IF (LUSERVD) THEN
C        Write user-specified gas dry deposition velocity (GASDEPVD)
         WRITE(IOUNT,9099)
         WRITE(IOUNT,9056)  USERVD
      END IF

      IF (.NOT. EVONLY) THEN
C        Write Allocated Storage Requirements (est.)
         WRITE(IOUNT,9099)
         WRITE(IOUNT,9057) STORE
      END IF

C     Model I/O Setting Summary
      WRITE(IOUNT,9099)
      ILMAX = MIN( 96, ILEN_FLD )
      IF (INPFIL .NE. ' ' .OR. OUTFIL .NE. ' ') THEN
         WRITE(IOUNT,9080) INPFIL(1:ILMAX), OUTFIL(1:ILMAX)
      END IF
      IF (RSTINP .AND. .NOT.MULTYR) THEN
         WRITE(IOUNT,9081) INIFIL(1:ILMAX)
      ELSE IF (RSTINP .AND. MULTYR) THEN
         WRITE(IOUNT,99081) INIFIL(1:ILMAX)
      END IF
C --- Summarize DEBUGOPTs selected by the user
CCRT  D063 Platform Downwash Debug PLATFMDBG
      IF( DEBUG .or. METEORDBG .or. AREADBG .or. PRIMEDBG .or. PVMRMDBG
     &  .or. OLMDEBUG .or. ARM2DEBUG .or. DEPOSDBG .or. TTRMDBG .or.
     &  GRSMDEBUG .or. PLATFMDBG )THEN

C ---       Create a character string that includes only those modeling
C           options (MODOPS) that are applicable for this model run
            DEBUG_OPTS_String = ''
            NOPS = 0

         IF( DEBUG )THEN
            DEBUG_OPTS(1) = 'DEBUG'
         ENDIF
         IF( METEORDBG )THEN
            DEBUG_OPTS(2) = 'METEOR'
         ENDIF
         IF( AREADBG )THEN
            DEBUG_OPTS(3) = 'AREADBG'
         ENDIF
         IF( PRIMEDBG )THEN
            DEBUG_OPTS(4) = 'PRIMEDBG'
         ENDIF
         IF( PVMRMDBG )THEN
            DEBUG_OPTS(5) = 'PVMRMDBG'
         ENDIF
         IF( OLMDEBUG )THEN
            DEBUG_OPTS(6) = 'OLMDEBUG'
         ENDIF
         IF( ARM2DEBUG )THEN
            DEBUG_OPTS(8) = 'ARM2DEBUG'
         ENDIF
         IF( DEPOSDBG )THEN
            DEBUG_OPTS(9) = 'DEPOSDBG'
         ENDIF
         IF( GRSMDEBUG )THEN
            DEBUG_OPTS(10) = 'GRSMDEBUG'
         ENDIF
CCRT     3/24/2021, D076 TTRM NO2
CCRT     update for TTRM debug opt display in .out and .sum
         IF( TTRMDBG )THEN
            DEBUG_OPTS(11) = 'TTRMDEBUG'
         ENDIF


C ---    Alpha option AWMADW debug specified
         IF( AWMADWDBG )THEN
            DEBUG_OPTS(7) = 'AWMADWDBG'
         ENDIF

CCRT     D063 Platform Downwash Debug
C ---    Alpha option PLATFORM debug specified
         IF( PLATFMDBG )THEN
            DEBUG_OPTS(10) = 'PLATFMDBG'
         ENDIF

CCRT     3/24/2021, D076 TTRM NO2
CCRT     update for TTRM debug opt display in .out and .sum
C ---    Loop through the 11 different options that are flagged
         DO I = 1, 11
            IF (LEN_TRIM(DEBUG_OPTS(I)) .GT. 0) THEN
               NOPS = NOPS + 1
               ILEN = 10*(NOPS-1)
               DEBUG_OPTS_String =
     &         DEBUG_OPTS_String(1:ILEN)//' '//DEBUG_OPTS(I)
            END IF
         END DO

C ---    Write DEBUG Option String to main output unit
         WRITE(IOUNT,99100)
     &         DEBUG_OPTS_String(1:LEN_TRIM(DEBUG_OPTS_String))
         WRITE(IOUNT,*)
      ENDIF

      IF (RSTSAV) WRITE(IOUNT,9082) SAVFIL(1:ILMAX)
      IF (ERRLST) WRITE(IOUNT,9083) MSGFIL(1:ILMAX)
      IF (EVENTS) WRITE(IOUNT,9084) EVFILE(1:ILMAX)
      IF (SUMMFILE) WRITE(IOUNT,9085) SUMFIL(1:ILMAX)

      IF (MULTYR) THEN
C ---    Write message regarding MULTYEAR applications
         WRITE(IOUNT,9099)
         WRITE(IOUNT,*) '**This Run is Part of a Multi-year (MULTYEAR)',
     &                   ' Application.'
         IF (PERIOD) THEN
            WRITE(IOUNT,*) '  NOTE:  The PERIOD Results Table Reflects',
     &                   ' Current Period Only;'
            WRITE(IOUNT,*) '         The Overall Maximum PERIOD',
     &                   ' Results Table and'
         ELSE IF (ANNUAL) THEN
            WRITE(IOUNT,*) '  NOTE:  Both ANNUAL Average Results and'
         ELSE
            WRITE(IOUNT,*) '  NOTE:'
         END IF
         WRITE(IOUNT,*) '         Short Term Results are Cumulative',
     &                   ' Across All Years Processed.'
      END IF

 9041 FORMAT(/44X,'***     MODEL SETUP OPTIONS SUMMARY       ***'/
     &       63(' -')/)
 9042 FORMAT(1X,'**Model Calculates ',I2,' Short Term Average(s)',
     &       ' of:  ',9(A5,2X,:))
 9043 FORMAT(1X,'    and Calculates PERIOD Averages')
 9045 FORMAT(1X,'**Model Calculates PERIOD Averages Only')
 9143 FORMAT(1X,'    and Calculates ANNUAL Averages')
 9145 FORMAT(1X,'**Model Calculates ANNUAL Averages Only')

 9044 FORMAT(1X,'**This Run Includes: ',I6,' Source(s);  ',I6,
     &       ' Source Group(s); and  ',I6,' Receptor(s)',//
     &       16X,'with: ',I6,' POINT(s), including'/
     &       22X,I6,' POINTCAP(s) and ',I6,' POINTHOR(s)'/
     &       16X,' and: ',I6,' VOLUME source(s)',/,
     &       16X,' and: ',I6,' AREA type source(s)'/,
     &       16X,' and: ',I6,' LINE source(s)',/,
     &       16X,' and: ',I6,' RLINE/RLINEXT source(s)'/,
     &       16X,' and: ',I6,' OPENPIT source(s)',/,
     &       16X,' and: ',I6,' BUOYANT LINE source(s) with a total of',
     &                    I6,' line(s)',/,
     &       16x,' and: ',I6,' SWPOINT source(s)',/)

 9046 FORMAT(1X,'**This Run is for EVENT Processing.',
     &      /1X,'**         and Includes: ',I6,' Source(s);  ',I6,
     &       ' Source Group(s); and  ',I6,' Event(s)',//
     &       16X,'with: ',I6,' POINT(s), including'/
     &       22X,I6,' POINTCAP(s) and ',I6,' POINTHOR(s)'/
     &       16X,' and: ',I6,' VOLUME source(s)',/,
     &       16X,' and: ',I6,' AREA type source(s)'/,
     &       16X,' and: ',I6,' LINE source(s)',/,
     &       16X,' and: ',I6,' RLINE/RLINEXT source(s)'/,
     &       16X,' and: ',I6,' OPENPIT source(s)',/,
     &       16X,' and: ',I6,' BUOYANT LINE source(s) with a total of',
     &                    I6,' line(s)',/,
     &       16x,' and: ',I6,' SWPOINT source(s)',/)

C Unused: 99048 FORMAT(21X,'BACKGRND Concentrations Varying by Direction Across',
C     &       I6,' Sectors')
 9048 FORMAT(1X,'     * The User Specified a Pollutant Type of: ',A8)
 9049 FORMAT(8X,'with ',I6,' FLAT and ',I6,' ELEV Source(s).')
 9050 FORMAT(1X,'**Misc. Inputs:  Base Elev. for Pot. Temp. Profile ',
     &       '(m MSL) = ',F8.2,' ;  Decay Coef. = ',G12.4,' ;',
     &       '  Rot. Angle = ',F7.1)
 9055 FORMAT(18X,'Emission Units = ',A40,' ;  Emission Rate Unit ',
     &       'Factor = ',G13.5,
     &      /18X,'Output Units   = ',A40)
90551 FORMAT(2X,'Concentration:',2X,'Emission Units = ',A40,' ;  ',
     &       'Emission Rate Unit Factor = ',G13.5,
     &      /18X,'Output Units   = ',A40)
90552 FORMAT(5X,'Deposition:',2X,'Emission Units = ',A40,' ;  ',
     &       'Emission Rate Unit Factor = ',G13.5,
     &      /18X,'Output Units   = ',A40)
 9056 FORMAT(18X,'User-Specified Dry Deposition Velocity for Gases ',
     &       '(m/s) = ',G13.5)
 9057 FORMAT(1X,'**Approximate Storage Requirements of Model = ',F8.1,
     &       ' MB of RAM.')
 9070 FORMAT(1X,'**Output Options Selected:')
98071 FORMAT(10X,'Model Outputs Source Contribution Information Only ',
     &           'for EVENT Processing (SOCONT Option)')
98072 FORMAT(10X,'Model Outputs Hourly Average Values for Each Source ',
     &           'for EVENT Processing (DETAIL Option)')
 9071 FORMAT(10X,'Model Outputs Tables of PERIOD Averages by Receptor')
 9171 FORMAT(10X,'Model Outputs Tables of ANNUAL Averages by Receptor')
 9072 FORMAT(10X,'Model Outputs Tables of Highest Short Term Values by',
     &       ' Receptor (RECTABLE Keyword)')
 9073 FORMAT(10X,'Model Outputs Tables of Overall Maximum Short Term',
     &       ' Values (MAXTABLE Keyword)')
 9074 FORMAT(10X,'Model Outputs Tables of Concurrent Short Term Values',
     &       ' by Receptor for Each Day Processed (DAYTABLE Keyword)')
 9075 FORMAT(10X,'Model Outputs External File(s) of Threshold',
     &       ' Violations (MAXIFILE Keyword)')
 9076 FORMAT(10X,'Model Outputs External File(s) of Concurrent Values',
     &       ' for Postprocessing (POSTFILE Keyword)')
 9077 FORMAT(10X,'Model Outputs External File(s) of High Values for',
     &       ' Plotting (PLOTFILE Keyword)')
 9078 FORMAT(10X,'Model Outputs External File(s) of Values for Input',
     &       ' to TOXX Model (TOXXFILE Keyword)')
99071 FORMAT(10X,'Model Outputs External File(s) of Values by Season',
     &       ' and Hour-of-Day (SEASONHR Keyword)')
99072 FORMAT(10X,'Model Outputs External File(s) of Ranked Values',
     &       ' (RANKFILE Keyword)')
99073 FORMAT(10X,'Model Outputs External File(s) of Arc-maximum Values',
     &       ' for Evaluation Purposes (EVALFILE Keyword)')
99173 FORMAT(10X,'Model Outputs External File(s) of Maximum Daily 1-hr',
     &       ' Values by Day (MAXDAILY Keyword)')
99273 FORMAT(10X,'Model Outputs External File(s) of Maximum Daily 1-hr',
     &       ' Values by Year (MXDYBYYR Keyword)')
99373 FORMAT(10X,'Model Outputs External File(s) of Contributions',
     &       ' to Maximum Daily Values Paired in Time & Space',
     &       ' (MAXDCONT Keyword)')
99074 FORMAT(10X,'Model Outputs Separate Summary File of High Ranked',
     &       ' Values (SUMMFILE Keyword)')
99075 FORMAT(10X,'NOTE: Option for EXPonential format used in',
     &       ' formatted output result files (FILEFORM Keyword)')
 9079 FORMAT(1X,'**NOTE:  The Following Flags May Appear Following ',
     &       A4,' Values:  c for Calm Hours',
     &               /65X,'m for Missing Hours',
     &               /65X,'b for Both Calm and Missing Hours')
 9080 FORMAT(1X,'**Input Runstream File:          ',A96,
     &      /1X,'**Output Print File:             ',A96/)
 9081 FORMAT(1X,'**NOTE: This Run was Restarted (INITFILE Keyword):',
     &      /1X,'**File for Initializing Arrays:  ',A96)
99081 FORMAT(1X,'**NOTE: This Run was Restarted (MULTYEAR Keyword):',
     &      /1X,'**File for Initializing Arrays:  ',A96)
 9082 FORMAT(1X,'**File for Saving Result Arrays: ',A96)
 9083 FORMAT(1X,'**Detailed Error/Message File:   ',A96)

99100 FORMAT(1X,'**Debug Options Selected:',8X,A:)

 9084 FORMAT(1X,'**File Created for Event Model:  ',A96)
 9085 FORMAT(1X,'**File for Summary of Results:   ',A96)

99090 FORMAT(/1X,'**Note that special processing requirements apply',
     &    ' for the 24-hour PM2.5 NAAQS - check available guidance.'/,
     &    '   Model will process user-specified ranks of high 24-hour',
     &    ' values averaged across the number of years modeled, and'/,
     &    '   the multi-year average of individual ANNUAL values,',
     &    ' averaged across the number of years modeled.')
99190 FORMAT(/1X,'**NOTE: Special processing requirements applicable',
     &    ' for the 24-hour PM2.5 NAAQS have been disabled!!!'/,
     &    '         High ranked 24-hour values are NOT averaged across',
     &    ' the number of years modeled, and'/,
     &    '         complete years of data are NOT required.')
99091 FORMAT(/1X,'**Note that special processing requirements apply',
     &  ' for the 1-hour NO2 NAAQS - check available guidance.'/,
     &  '   Model will process user-specified ranks of daily maximum',
     &  ' 1-hour values averaged across the number of years modeled.'/,
     &  '   For annual NO2 NAAQS modeling, the multi-year maximum of',
     &  ' PERIOD values can be simulated using the MULTYEAR keyword.'/,
     &  '   Multi-year PERIOD and 1-hour values should only be done',
     &  ' in a single model run using the MULTYEAR option with a'/,
     &  '   single multi-year meteorological data file using STARTEND',
     &  ' keyword.')
99191 FORMAT(/1X,'**NOTE: Special processing requirements applicable',
     &  ' for the 1-hour NO2 NAAQS have been disabled!!!'/,
     &  '         User has specified ',A3,' on the POLLUTID keyword.'/,
     &  '         High ranked 1-hour values are NOT averaged across',
     &  ' the number of years modeled, and'/,
     &  '         complete years of data are NOT required.')
99291 FORMAT(/1X,'**NOTE: Special processing requirements applicable',
     &  ' for the 1-hour NO2 NAAQS have been disabled!!!'/,
     &  '         User has specified non-standard averaging periods: ',
     &  A/,
     &  '         High ranked 1-hour values are NOT averaged across',
     &  ' the number of years modeled, and'/,
     &  '         complete years of data are NOT required.')
99092 FORMAT(/1X,'**Note that special processing requirements apply',
     &  ' for the 1-hour SO2 NAAQS - check available guidance.'/,
     &  '   Model will process user-specified ranks of daily maximum',
     &  ' 1-hour values averaged across the number of years modeled.')
99192 FORMAT(/1X,'**NOTE: Special processing requirements applicable',
     &  ' for the 1-hour SO2 NAAQS have been disabled!!!'/,
     &  '         User has specified ',A3,' on the POLLUTID keyword.'/,
     &  '         High ranked 1-hour values are NOT averaged across',
     &  ' the number of years modeled, and'/,
     &  '         complete years of data are NOT required.')
99292 FORMAT(/1X,'**NOTE: Special processing requirements applicable',
     &  ' for the 1-hour SO2 NAAQS have been disabled!!!'/,
     &  '         User has specified non-standard averaging periods: ',
     &  A/,
     &  '         High ranked 1-hour values are NOT averaged across',
     &  ' the number of years modeled, and'/,
     &  '         complete years of data are NOT required.')
99093 FORMAT(/1X,'**The AERMET Input Meteorological Data Version Date:',
     &            1X,A6)


 9099 FORMAT(1X,' ')

      RETURN
      END

      SUBROUTINE PRTO3VALS(ISECT)
C***********************************************************************
C                 PRTO3VALS Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Print Out The O3VALUES Input Data Summary
C
C        PROGRAMMER: Roger Brode
C
C        DATE:       February 28, 2011
C
C        MODIFIED: Corrected code to use the O3FLAG variable which
C                  identifies the user-specified option for defining
C                  temporally-varying background ozone concentrations.
C                  The previous version erroneously referenced the
C                  BFLAG variable associated with user-specified
C                  BACKGROUND concentrations.
C                  R. W. Brode, U.S. EPA, OAQPS, AQMG, 12/19/2011
C
C        INPUTS:  Model Options and Keyword Summarys
C
C        OUTPUTS: Printed Model Outputs
C
C        CALLED FROM:   INPSUM
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I1, I2, I3, IFR, IDW
      INTEGER :: ISECT
      CHARACTER SEASON(4)*6, MONTHS(12)*9, DAYOFWEEK(3)*8,
     &          DAYOFWEEK7(7)*8
C Unused:       INTEGER :: I, J, K, NL, INDC, INGRP
C Unused:       CHARACTER BLDING*3, URB*3, IQUN*12, CAP*3, CQFLG*7
C Unused:       CHARACTER CNPD*5, CAZS*8

C     Variable Initializations
      DATA SEASON /'WINTER','SPRING','SUMMER',' FALL '/
      DATA MONTHS /'JANUARY  ','FEBRUARY ','MARCH    ','APRIL    ',
     &             'MAY      ','JUNE     ','JULY     ','AUGUST   ',
     &             'SEPTEMBER','OCTOBER  ','NOVEMBER ','DECEMBER '/
      DATA DAYOFWEEK  /'WEEKDAY ','SATURDAY','SUNDAY  '/
      DATA DAYOFWEEK7 /'MONDAY  ','TUESDAY ','WEDNESDY','THURSDAY',
     &                 'FRIDAY  ','SATURDAY','SUNDAY  '/
      MODNAM = 'PRTO3VALS'


C     Print User-specified Background Ozone Concetrations
      IF (O3FLAG(ISECT) .EQ. 'ANNUAL') THEN
         CALL HEADER(IOUNIT)
         WRITE(IOUNIT,9001) ISECT, OzoneUnits
         WRITE(IOUNIT,9006) O3VARY(1,ISECT)
      ELSE IF (O3FLAG(ISECT) .EQ. 'SEASON') THEN
         CALL HEADER(IOUNIT)
         WRITE(IOUNIT,9002) ISECT, OzoneUnits
         WRITE(IOUNIT,9004) (SEASON(I1),I1=1,4)
         WRITE(IOUNIT,9006) (O3VARY(I1,ISECT),I1=1,4)
      ELSE IF (O3FLAG(ISECT) .EQ. 'MONTH') THEN
         CALL HEADER(IOUNIT)
         WRITE(IOUNIT,9007) ISECT, OzoneUnits
         WRITE(IOUNIT,9008)
         WRITE(IOUNIT,9013)
         WRITE(IOUNIT,9010) (O3VARY(I1,ISECT),I1=1,12)
      ELSE IF (O3FLAG(ISECT) .EQ. 'HROFDY') THEN
         CALL HEADER(IOUNIT)
         WRITE(IOUNIT,9011) ISECT, OzoneUnits
         WRITE(IOUNIT,9012)
         WRITE(IOUNIT,9013)
         WRITE(IOUNIT,9014) (I1,O3VARY(I1,ISECT),I1=1,24)
      ELSE IF (O3FLAG(ISECT) .EQ. 'SEASHR') THEN
         CALL HEADER(IOUNIT)
         WRITE(IOUNIT,9018) ISECT, OzoneUnits
         WRITE(IOUNIT,9012)
         WRITE(IOUNIT,9013)
         DO I1 = 1, 4
            IFR = (I1-1)*24
            WRITE(IOUNIT,9019) SEASON(I1)
            WRITE(IOUNIT,9014) (I2,O3VARY(I2+IFR,ISECT),I2=1,24)
         END DO
      ELSE IF (O3FLAG(ISECT) .EQ. 'HRDOW') THEN
         CALL HEADER(IOUNIT)
         WRITE(IOUNIT,99218) ISECT, OzoneUnits
         WRITE(IOUNIT,99012)
         WRITE(IOUNIT,99013)
         DO I1 = 1, 3
            IDW = (I1-1)*24
            WRITE(IOUNIT,99021) DAYOFWEEK(I1)
            WRITE(IOUNIT,99014) (I3,O3VARY(I3+IDW,ISECT),I3=1,24)
         END DO
      ELSE IF (O3FLAG(ISECT) .EQ. 'HRDOW7') THEN
         CALL HEADER(IOUNIT)
         WRITE(IOUNIT,79218) ISECT, OzoneUnits
         WRITE(IOUNIT,99012)
         WRITE(IOUNIT,99013)
         DO I1 = 1, 7
            IDW = (I1-1)*24
            WRITE(IOUNIT,99021) DAYOFWEEK7(I1)
            WRITE(IOUNIT,99014) (I3,O3VARY(I3+IDW,ISECT),I3=1,24)
         END DO
      ELSE IF (O3FLAG(ISECT) .EQ. 'SHRDOW') THEN
         CALL HEADER(IOUNIT)
         WRITE(IOUNIT,99018) ISECT, OzoneUnits
         WRITE(IOUNIT,99012)
         WRITE(IOUNIT,99013)
         DO I1 = 1, 3
            IDW = (I1-1)*96
            DO I2 = 1, 4
               IFR = (I2-1)*24
               WRITE(IOUNIT,99019) SEASON(I2), DAYOFWEEK(I1)
               WRITE(IOUNIT,99014) (I3,O3VARY(I3+IFR+IDW,ISECT),I3=1,24)
            END DO
         END DO
      ELSE IF (O3FLAG(ISECT) .EQ. 'SHRDOW7') THEN
         CALL HEADER(IOUNIT)
         WRITE(IOUNIT,79018) ISECT, OzoneUnits
         WRITE(IOUNIT,99012)
         WRITE(IOUNIT,99013)
         DO I1 = 1, 7
            IDW = (I1-1)*96
            DO I2 = 1, 4
               IFR = (I2-1)*24
               WRITE(IOUNIT,99019) SEASON(I2), DAYOFWEEK7(I1)
               WRITE(IOUNIT,99014) (I3,O3VARY(I3+IFR+IDW,ISECT),I3=1,24)
            END DO
         END DO
      ELSE IF (O3FLAG(ISECT) .EQ. 'MHRDOW') THEN
         DO I1 = 1, 3
            CALL HEADER(IOUNIT)
            WRITE(IOUNIT,99118) ISECT, OzoneUnits
            WRITE(IOUNIT,99012)
            WRITE(IOUNIT,99013)
            IDW = (I1-1)*288
            DO I2 = 1, 12
               IFR = (I2-1)*24
               WRITE(IOUNIT,99020) MONTHS(I2), DAYOFWEEK(I1)
               WRITE(IOUNIT,99014) (I3,O3VARY(I3+IFR+IDW,ISECT),I3=1,24)
            END DO
         END DO
      ELSE IF (O3FLAG(ISECT) .EQ. 'MHRDOW7') THEN
         DO I1 = 1, 7
            CALL HEADER(IOUNIT)
            WRITE(IOUNIT,79118) ISECT, OzoneUnits
            WRITE(IOUNIT,99012)
            WRITE(IOUNIT,99013)
            IDW = (I1-1)*288
            DO I2 = 1, 12
               IFR = (I2-1)*24
               WRITE(IOUNIT,99020) MONTHS(I2), DAYOFWEEK7(I1)
               WRITE(IOUNIT,99014) (I3,O3VARY(I3+IFR+IDW,ISECT),I3=1,24)
            END DO
         END DO
      END IF

 9001 FORMAT(/40X,'* SECT',I1,' ANNUAL (NON-VARYING) OZONE ',
     &       'CONCENTRATION (',A5,') *'/)
 9002 FORMAT(/30X,'* SECT',I1,' OZONE CONCENTRATIONS (',A5,') ',
     &       'WHICH VARY SEASONALLY *'/)
 9004 FORMAT(40X,4(A6,9X)/20X,40('- ')/)
 9006 FORMAT(38X,4(E10.5,5X))
 9007 FORMAT(/42X,'* SECT',I1,' OZONE CONCENTRATIONS (',A5,') ',
     &        'WHICH VARY MONTHLY *'/)
 9008 FORMAT(7X,'JANUARY  FEBRUARY   MARCH     APRIL      MAY       ',
     &  'JUNE      JULY     AUGUST   SEPTEMBER  OCTOBER  NOVEMBER  ',
     &  'DECEMBER'/)
 9010 FORMAT(5X,12(E9.3,1X))
 9011 FORMAT(/29X,'* SECT',I1,' OZONE CONCENTRATIONS (',A5,') ',
     &       'WHICH VARY FOR EACH HOUR OF THE DAY *'/)
 9012 FORMAT(5X,6('HOUR    O3VALS',6X))
99012 FORMAT(2X,8('HOUR   O3VALS',3X))
 9013 FORMAT(1X,65('- ')/)
99013 FORMAT(1X,65('- '))
 9014 FORMAT(4(5X,6(I3,3X,E10.5,4X)/))
99014 FORMAT(2(3X,8(I2,2X,E9.4,3X)/),3X,8(I2,2X,E9.4,3X))
C Unused: 9015 FORMAT(/26X,'* SECT',I1,' OZONE CONCENTRATIONS (',A5,') ',
C     &       ' WHICH VARY WITH WIND SPEED *'/)
 9018 FORMAT(/23X,'* SECT',I1,' OZONE CONCENTRATIONS (',A5,') ',
     &       ' WHICH VARY SEASONALLY AND DIURNALLY (SEASHR) *'/)
99018 FORMAT(/18X,'* SECT',I1,' OZONE CONCENTRATIONS (',A5,') ',
     &       ' WHICH VARY SEASONALLY, DIURNALLY AND BY DAY OF WEEK ',
     &       '(SHRDOW) *'/)
79018 FORMAT(/18X,'* SECT',I1,' OZONE CONCENTRATIONS (',A5,') ',
     &       ' WHICH VARY SEASONALLY, DIURNALLY AND BY DAY OF WEEK ',
     &       '(SHRDOW7) *'/)
99118 FORMAT(/19X,'* SECT',I1,' OZONE CONCENTRATIONS (',A5,') ',
     &       ' WHICH VARY MONTHLY, DIURNALLY AND BY DAY OF WEEK ',
     &       '(MHRDOW) *'/)
79118 FORMAT(/19X,'* SECT',I1,' OZONE CONCENTRATIONS (',A5,') ',
     &       ' WHICH VARY MONTHLY, DIURNALLY AND BY DAY OF WEEK ',
     &       '(MHRDOW7) *'/)
99218 FORMAT(/20X,'* SECT',I1,' OZONE CONCENTRATIONS (',A5,') ',
     &       ' WHICH VARY DIURNALLY AND BY DAY OF WEEK (HRDOW) *'/)
79218 FORMAT(/20X,'* SECT',I1,' OZONE CONCENTRATIONS (',A5,') ',
     &       ' WHICH VARY DIURNALLY AND BY DAY OF WEEK (HRDOW7) *'/)
 9019 FORMAT(59X,'SEASON = ',A6)
99019 FORMAT(46X,'SEASON = ',A6,';  DAY OF WEEK = ',A8)
99020 FORMAT(46X,'MONTH = ',A9,';  DAY OF WEEK = ',A8)
99021 FORMAT(46X,'DAY OF WEEK = ',A8)
C Unused:  9024 FORMAT(26X,6(1X,E12.5))
C Unused: 9025 FORMAT(/26X,6('   WIND SPEED')/26X,6('   CATEGORY',I2))

      RETURN
      END

      SUBROUTINE PRTNOXVALS(ISECT)
C***********************************************************************
C                 PRTNOXVALS Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Print Out The NOX_VALS Input Data Summary
C
C        PROGRAMMER: CERC
C
C        DATE:       November 2020
C
C        INPUTS:  Model Options and Keyword Summarys
C
C        OUTPUTS: Printed Model Outputs
C
C        CALLED FROM:   INPSUM
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I1, I2, I3, IFR, IDW
      INTEGER :: ISECT
      CHARACTER SEASON(4)*6, MONTHS(12)*9, DAYOFWEEK(3)*8,
     &          DAYOFWEEK7(7)*8

C     Variable Initializations
      DATA SEASON /'WINTER','SPRING','SUMMER',' FALL '/
      DATA MONTHS /'JANUARY  ','FEBRUARY ','MARCH    ','APRIL    ',
     &             'MAY      ','JUNE     ','JULY     ','AUGUST   ',
     &             'SEPTEMBER','OCTOBER  ','NOVEMBER ','DECEMBER '/
      DATA DAYOFWEEK  /'WEEKDAY ','SATURDAY','SUNDAY  '/
      DATA DAYOFWEEK7 /'MONDAY  ','TUESDAY ','WEDNESDY','THURSDAY',
     &                 'FRIDAY  ','SATURDAY','SUNDAY  '/
      MODNAM = 'PRTNOXVALS'


C     Print User-specified Background NOx Concetrations
      IF (NOXFLAG(ISECT) .EQ. 'ANNUAL') THEN
         CALL HEADER(IOUNIT)
         WRITE(IOUNIT,9001) ISECT, NOxUnits
         WRITE(IOUNIT,9006) NOXVARY(1,ISECT)
      ELSE IF (NOXFLAG(ISECT) .EQ. 'SEASON') THEN
         CALL HEADER(IOUNIT)
         WRITE(IOUNIT,9002) ISECT, NOxUnits
         WRITE(IOUNIT,9004) (SEASON(I1),I1=1,4)
         WRITE(IOUNIT,9006) (NOXVARY(I1,ISECT),I1=1,4)
      ELSE IF (NOXFLAG(ISECT) .EQ. 'MONTH') THEN
         CALL HEADER(IOUNIT)
         WRITE(IOUNIT,9007) ISECT, NOxUnits
         WRITE(IOUNIT,9008)
         WRITE(IOUNIT,9013)
         WRITE(IOUNIT,9010) (NOXVARY(I1,ISECT),I1=1,12)
      ELSE IF (NOXFLAG(ISECT) .EQ. 'HROFDY') THEN
         CALL HEADER(IOUNIT)
         WRITE(IOUNIT,9011) ISECT, NOxUnits
         WRITE(IOUNIT,9012)
         WRITE(IOUNIT,9013)
         WRITE(IOUNIT,9014) (I1,NOXVARY(I1,ISECT),I1=1,24)
      ELSE IF (NOXFLAG(ISECT) .EQ. 'SEASHR') THEN
         CALL HEADER(IOUNIT)
         WRITE(IOUNIT,9018) ISECT, NOxUnits
         WRITE(IOUNIT,9012)
         WRITE(IOUNIT,9013)
         DO I1 = 1, 4
            IFR = (I1-1)*24
            WRITE(IOUNIT,9019) SEASON(I1)
            WRITE(IOUNIT,9014) (I2,NOXVARY(I2+IFR,ISECT),I2=1,24)
         END DO
      ELSE IF (NOXFLAG(ISECT) .EQ. 'HRDOW') THEN
         CALL HEADER(IOUNIT)
         WRITE(IOUNIT,99218) ISECT, NOxUnits
         WRITE(IOUNIT,99012)
         WRITE(IOUNIT,99013)
         DO I1 = 1, 3
            IDW = (I1-1)*24
            WRITE(IOUNIT,99021) DAYOFWEEK(I1)
            WRITE(IOUNIT,99014) (I3,NOXVARY(I3+IDW,ISECT),I3=1,24)
         END DO
      ELSE IF (NOXFLAG(ISECT) .EQ. 'HRDOW7') THEN
         CALL HEADER(IOUNIT)
         WRITE(IOUNIT,79218) ISECT, NOxUnits
         WRITE(IOUNIT,99012)
         WRITE(IOUNIT,99013)
         DO I1 = 1, 7
            IDW = (I1-1)*24
            WRITE(IOUNIT,99021) DAYOFWEEK7(I1)
            WRITE(IOUNIT,99014) (I3,NOXVARY(I3+IDW,ISECT),I3=1,24)
         END DO
      ELSE IF (NOXFLAG(ISECT) .EQ. 'SHRDOW') THEN
         CALL HEADER(IOUNIT)
         WRITE(IOUNIT,99018) ISECT, NOxUnits
         WRITE(IOUNIT,99012)
         WRITE(IOUNIT,99013)
         DO I1 = 1, 3
            IDW = (I1-1)*96
            DO I2 = 1, 4
               IFR = (I2-1)*24
               WRITE(IOUNIT,99019) SEASON(I2), DAYOFWEEK(I1)
               WRITE(IOUNIT,99014) (I3,
     &                    NOXVARY(I3+IFR+IDW,ISECT),I3=1,24)
            END DO
         END DO
      ELSE IF (NOXFLAG(ISECT) .EQ. 'SHRDOW7') THEN
         CALL HEADER(IOUNIT)
         WRITE(IOUNIT,79018) ISECT, NOxUnits
         WRITE(IOUNIT,99012)
         WRITE(IOUNIT,99013)
         DO I1 = 1, 7
            IDW = (I1-1)*96
            DO I2 = 1, 4
               IFR = (I2-1)*24
               WRITE(IOUNIT,99019) SEASON(I2), DAYOFWEEK7(I1)
               WRITE(IOUNIT,99014) (I3,
     &                     NOXVARY(I3+IFR+IDW,ISECT),I3=1,24)
            END DO
         END DO
      ELSE IF (NOXFLAG(ISECT) .EQ. 'MHRDOW') THEN
         DO I1 = 1, 3
            CALL HEADER(IOUNIT)
            WRITE(IOUNIT,99118) ISECT, NOxUnits
            WRITE(IOUNIT,99012)
            WRITE(IOUNIT,99013)
            IDW = (I1-1)*288
            DO I2 = 1, 12
               IFR = (I2-1)*24
               WRITE(IOUNIT,99020) MONTHS(I2), DAYOFWEEK(I1)
               WRITE(IOUNIT,99014) (I3,
     &                    NOXVARY(I3+IFR+IDW,ISECT),I3=1,24)
            END DO
         END DO
      ELSE IF (NOXFLAG(ISECT) .EQ. 'MHRDOW7') THEN
         DO I1 = 1, 7
            CALL HEADER(IOUNIT)
            WRITE(IOUNIT,79118) ISECT, NOxUnits
            WRITE(IOUNIT,99012)
            WRITE(IOUNIT,99013)
            IDW = (I1-1)*288
            DO I2 = 1, 12
               IFR = (I2-1)*24
               WRITE(IOUNIT,99020) MONTHS(I2), DAYOFWEEK7(I1)
               WRITE(IOUNIT,99014) (I3,
     &                     NOXVARY(I3+IFR+IDW,ISECT),I3=1,24)
            END DO
         END DO
      ELSE IF (NOXFLAG(ISECT) .EQ. 'WSPEED') THEN
          CALL HEADER(IOUNIT)
          WRITE(IOUNIT,9015) ISECT, NOxUnits
          WRITE(IOUNIT,9025) (I1,I1=1,6)
          WRITE(IOUNIT,99013)
          WRITE(IOUNIT,9024) (NOXVARY(I1,ISECT),I1=1,6)
      END IF
!
 9001 FORMAT(/40X,'* SECT',I1,' ANNUAL (NON-VARYING) NOX ',
     &       'CONCENTRATION (',A5,') *'/)
 9002 FORMAT(/30X,'* SECT',I1,' NOX CONCENTRATIONS (',A5,') ',
     &       'WHICH VARY SEASONALLY *'/)
 9004 FORMAT(40X,4(A6,9X)/20X,40('- ')/)
 9006 FORMAT(38X,4(E10.5,5X))
 9007 FORMAT(/42X,'* SECT',I1,' NOX CONCENTRATIONS (',A5,') ',
     &        'WHICH VARY MONTHLY *'/)
 9008 FORMAT(7X,'JANUARY  FEBRUARY   MARCH     APRIL      MAY       ',
     &  'JUNE      JULY     AUGUST   SEPTEMBER  OCTOBER  NOVEMBER  ',
     &  'DECEMBER'/)
 9010 FORMAT(5X,12(E9.3,1X))
 9011 FORMAT(/29X,'* SECT',I1,' NOX CONCENTRATIONS (',A5,') ',
     &       'WHICH VARY FOR EACH HOUR OF THE DAY *'/)
 9012 FORMAT(5X,6('HOUR    NOXVALS',6X))
99012 FORMAT(2X,8('HOUR   NOXVALS',3X))
 9013 FORMAT(1X,65('- ')/)
99013 FORMAT(1X,65('- '))
 9014 FORMAT(4(5X,6(I3,3X,E10.5,4X)/))
99014 FORMAT(2(3X,8(I2,2X,E9.4,3X)/),3X,8(I2,2X,E9.4,3X))
 9015 FORMAT(/34X,'* SECT',I1,' NOX CONCENTRATIONS (',A5,') ',
     &       ' WHICH VARY WITH WIND SPEED *'/)
 9018 FORMAT(/23X,'* SECT',I1,' NOX CONCENTRATIONS (',A5,') ',
     &       ' WHICH VARY SEASONALLY AND DIURNALLY (SEASHR) *'/)
99018 FORMAT(/18X,'* SECT',I1,' NOX CONCENTRATIONS (',A5,') ',
     &       ' WHICH VARY SEASONALLY, DIURNALLY AND BY DAY OF WEEK ',
     &       '(SHRDOW) *'/)
79018 FORMAT(/18X,'* SECT',I1,' NOX CONCENTRATIONS (',A5,') ',
     &       ' WHICH VARY SEASONALLY, DIURNALLY AND BY DAY OF WEEK ',
     &       '(SHRDOW7) *'/)
99118 FORMAT(/19X,'* SECT',I1,' NOX CONCENTRATIONS (',A5,') ',
     &       ' WHICH VARY MONTHLY, DIURNALLY AND BY DAY OF WEEK ',
     &       '(MHRDOW) *'/)
79118 FORMAT(/19X,'* SECT',I1,' NOX CONCENTRATIONS (',A5,') ',
     &       ' WHICH VARY MONTHLY, DIURNALLY AND BY DAY OF WEEK ',
     &       '(MHRDOW7) *'/)
99218 FORMAT(/20X,'* SECT',I1,' NOX CONCENTRATIONS (',A5,') ',
     &       ' WHICH VARY DIURNALLY AND BY DAY OF WEEK (HRDOW) *'/)
79218 FORMAT(/20X,'* SECT',I1,' NOX CONCENTRATIONS (',A5,') ',
     &       ' WHICH VARY DIURNALLY AND BY DAY OF WEEK (HRDOW7) *'/)
 9019 FORMAT(59X,'SEASON = ',A6)
99019 FORMAT(46X,'SEASON = ',A6,';  DAY OF WEEK = ',A8)
99020 FORMAT(46X,'MONTH = ',A9,';  DAY OF WEEK = ',A8)
99021 FORMAT(46X,'DAY OF WEEK = ',A8)
 9024 FORMAT(26X,6(1X,E12.5))
 9025 FORMAT(26X,6('   WIND SPEED')/26X,6('   CATEGORY',I2))

      RETURN
      END SUBROUTINE PRTNOXVALS

      SUBROUTINE PRTSRC
C***********************************************************************
C                 PRTSRC Module of the AMS/EPA Regulatory Model - AERMOD
c ----------------------------------------------------------------------
c ---    ISC-PRIME     Version 1.0    Level 970812              Modified
c ---        V. Tino
c ---        Earth Tech, Inc.
c            Prepared for EPRI under contract WO3527-01
c ----------------------------------------------------------------------
C
C        PURPOSE: Print Out The Input Source Data Summary
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        MODIFIED BY D. Strimaitis, SRC (for Wet & Dry DEPOSITION)
C
C        DATE:    November 8, 1993
C
C        MODIFIED:   Modified to handle two barrier cases for RLINEXT sources.
C                    Dianna Francisco & David Heist, US EPA ORD, 02/12/2021
C
C        MODIFIED:   Modified to handle RLINE and RLINEXT sources.
C                    M. G. Snyder, R. Cleary,  Wood 03/04/2019
C
C        MODIFIED:   To include RLINE source data information.
C                    M. G. Snyder, R. Cleary,  Wood 07/20/2018
C
C        MODIFIED:   To include BUOYLINE source data information for
C                    all lines in buoyant line source.
C                    R. Cleary, Wood, 10/18/2017
C
C        MODIFIED:   To remove reference to "STABILITY CATEGORY" and
C                    correct format statement 9024 for 'WSPEED'
C                    EMISFACT option, inherited from ISCST3 code
C                    for 'STAR' option.
C                    R. W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
C
C        MODIFIED:   To include emission factors that vary by
C                    hour-of-day and day-of-week (HRDOW and HRDOW7).
C                    R. W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
C
C        MODIFIED:   To modify format for outputting QFLAG from A6 to
C                    A7, and other minor adjustments to formatting.
C                    R. W. Brode, U.S. EPA/OAQPS/AQMG, 12/07/06
C
C        MODIFIED:   To incorporate flag for CAPPED and/or HORIZONTAL
C                    stack releases based on BETA-test draft option.
C                    R. W. Brode, U.S. EPA/OAQPS/AQMG, 12/07/06
C
C        MODIFIED:   To remove reference to STAR emission factors,
C                    include identification of capped stacks (POINTCAP),
C                    and emission factors that vary by month, hour-of-day
C                    and day-of-week (MHRDOW and MHRDOW7).
C                    Roger Brode, MACTEC (f/k/a PES), Inc., - 08/25/05
C
C        MODIFIED by YICHENG ZHUANG, SRC to combine version 93188 with
C                 version 93046 - 9/28/93
C
C        MODIFIED BY D. Strimaitis, SRC (for DEPOSITION) - 2/25/93
C
C*       MODIFIED BY PES (for OPENPIT Source) - 7/22/94
C
C*       MODIFIED BY PES to properly handle page breaks in summary
C*                of sources within a source group - 11/19/98
C
C*       MODIFIED BY R. Brode, PES to include additional building
C                 dimensions for PRIME downwash algorithm - 8/9/01
C
C*       MODIFIED BY R. Brode, MACTEC/PES to include identification
C                 of urban and Method 2 sources - 9/29/03
C
C        INPUTS:  Model Options and Keyword Summarys
C
C        OUTPUTS: Printed Model Outputs
C
C        CALLED FROM:   INPSUM
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      USE RLINE_DATA, ONLY: RLSOURCE, RLMOVESCONV
      USE BUOYANT_LINE
      IMPLICIT NONE
      CHARACTER MODNAM*12
      LOGICAL   BLOUTPROCESSED

      INTEGER :: I, J, K, NL, I1, I2, I3, IFR, IDW, INDC, INGRP, NBL
      CHARACTER BLDING*3, URB*3, IQUN*12, CAP*3, CQFLG*7
      CHARACTER SEASON(4)*6, MONTHS(12)*9, DAYOFWEEK(3)*8,
     &          DAYOFWEEK7(7)*8, CNPD*5, CAZS*8

C     Variable Initializations
      DATA SEASON /'WINTER','SPRING','SUMMER',' FALL '/
      DATA MONTHS /'JANUARY  ','FEBRUARY ','MARCH    ','APRIL    ',
     &             'MAY      ','JUNE     ','JULY     ','AUGUST   ',
     &             'SEPTEMBER','OCTOBER  ','NOVEMBER ','DECEMBER '/
      DATA DAYOFWEEK  /'WEEKDAY ','SATURDAY','SUNDAY  '/
      DATA DAYOFWEEK7 /'MONDAY  ','TUESDAY ','WEDNESDY','THURSDAY',
     &                 'FRIDAY  ','SATURDAY','SUNDAY  '/
      MODNAM = 'PRTSRC'

C --- Set the bouyant line summary output process flag to false to
C      indicate that average parameter summary data has not been
C      processed for this hour
      BLOUTPROCESSED = .FALSE.

      IF (ISSTAT(8) .EQ. 0 .AND. ISSTAT(17) .EQ. 0 .AND.
     &                           ISSTAT(18) .EQ. 0) THEN
C        Write Default Emission Rate Units
         IQUN = ' (GRAMS/SEC)'
      ELSE
         IQUN = '(USER UNITS)'
      END IF

C     Write Out The Point Source Data, If Any
      INDC = 0
      DO I = 1, NUMSRC
         IF (SRCTYP(I)(1:5) .EQ. 'POINT') THEN
            INDC = INDC + 1
            IF (URBSRC(I) .EQ. 'Y') THEN
               URB = 'YES'
            ELSE
               URB = 'NO'
            END IF
            BLDING = 'NO'
            IF (NSEC .GT. 0) THEN
C ---          Check for building data for this source
               DO J = 1, NSEC
                  IF(ADSBH(J,I).NE.0.0D0 .AND. ADSBW(J,I).NE.0.0D0
     &                                   .AND. ADSBL(J,I).NE.0.0D0) THEN
c -----------------------------------------------------------------
                     BLDING = 'YES'
                     EXIT
                  END IF
               END DO
            END IF
            IF (SRCTYP(I) .EQ. 'POINTCAP') THEN
               CAP = 'CAP'
            ELSE IF (SRCTYP(I) .EQ. 'POINTHOR') THEN
               CAP = 'HOR'
            ELSE
               CAP = ' NO'
            END IF
            IF (L_METHOD2(I)) THEN
               WRITE(CNPD,'("METH2")')
            ELSE
               WRITE(CNPD,'(I4,1X)') INPD(I)
            END IF
            IF (L_FLATSRC(I)) THEN
               WRITE(CAZS,'(4X,"FLAT")')
            ELSE
               WRITE(CAZS,'(F8.1)') AZS(I)
            END IF
            IF (MOD(INDC-1,40) .EQ. 0) THEN
               CALL HEADER(IOUNIT)
               WRITE(IOUNIT,9046) IQUN
            END IF
            WRITE(IOUNIT,9047) SRCID(I), CNPD, AQS(I),
     &              AXS(I), AYS(I), CAZS, AHS(I), ATS(I), AVS(I),
     &              ADS(I), BLDING, URB, CAP, QFLAG(I)
         END IF
      END DO

C     Write Out The Volume Source Data, If Any
      INDC = 0
      DO I = 1, NUMSRC
         IF (SRCTYP(I) .EQ. 'VOLUME') THEN
            INDC = INDC + 1
            IF (URBSRC(I) .EQ. 'Y') THEN
               URB = 'YES'
            ELSE
               URB = 'NO'
            END IF
            IF (L_METHOD2(I)) THEN
               WRITE(CNPD,'("METH2")')
            ELSE
               WRITE(CNPD,'(I4,1X)') INPD(I)
            END IF
            IF (L_FLATSRC(I)) THEN
               WRITE(CAZS,'(4X,"FLAT")')
            ELSE
               WRITE(CAZS,'(F8.1)') AZS(I)
            END IF
            IF (MOD(INDC-1,40) .EQ. 0) THEN
               CALL HEADER(IOUNIT)
               WRITE(IOUNIT,9074) IQUN
            END IF
            IF (QFLAG(I) .EQ. 'HOURLY' .AND. L_HRLYSIG(I)) THEN
C              Source uses HOUREMIS option with hourly varying sigmas
               CQFLG = 'HRLYSIG'
            ELSE
               CQFLG = QFLAG(I)
            END IF
            WRITE(IOUNIT,9075) SRCID(I), CNPD, AQS(I),
     &              AXS(I), AYS(I), CAZS, AHS(I), ASYINI(I), ASZINI(I),
     &              URB, CQFLG
         END IF
      END DO

C     Write Out The Area Source Data, If Any
      INDC = 0
      DO I = 1, NUMSRC
         IF (SRCTYP(I) .EQ. 'AREA') THEN
            INDC = INDC + 1
            IF (URBSRC(I) .EQ. 'Y') THEN
               URB = 'YES'
            ELSE
               URB = 'NO'
            END IF
            IF (L_METHOD2(I)) THEN
               WRITE(CNPD,'("METH2")')
            ELSE
               WRITE(CNPD,'(I4,1X)') INPD(I)
            END IF
            IF (L_FLATSRC(I)) THEN
               WRITE(CAZS,'(4X,"FLAT")')
            ELSE
               WRITE(CAZS,'(F8.1)') AZS(I)
            END IF
            IF (MOD(INDC-1,40) .EQ. 0) THEN
               CALL HEADER(IOUNIT)
               WRITE(IOUNIT,9076) IQUN
            END IF
            IF (QFLAG(I) .EQ. 'HOURLY' .AND. L_HRLYSIG(I)) THEN
C              Source uses HOUREMIS option with hourly varying sigmas
               CQFLG = 'HRLYSIG'
            ELSE
               CQFLG = QFLAG(I)
            END IF
            WRITE(IOUNIT,9077) SRCID(I), CNPD, AQS(I),
     &              AXS(I), AYS(I), CAZS, AHS(I), AXINIT(I), AYINIT(I),
     &              AANGLE(I), ASZINI(I), URB, CQFLG
         END IF
      END DO

C     Write Out The AREACIRC Source Data, If Any
      INDC = 0
      DO I = 1, NUMSRC
         IF (SRCTYP(I) .EQ. 'AREACIRC') THEN
            INDC = INDC + 1
            IF (URBSRC(I) .EQ. 'Y') THEN
               URB = 'YES'
            ELSE
               URB = 'NO'
            END IF
            IF (L_METHOD2(I)) THEN
               WRITE(CNPD,'("METH2")')
            ELSE
               WRITE(CNPD,'(I4,1X)') INPD(I)
            END IF
            IF (L_FLATSRC(I)) THEN
               WRITE(CAZS,'(4X,"FLAT")')
            ELSE
               WRITE(CAZS,'(F8.1)') AZS(I)
            END IF
            IF (MOD(INDC-1,40) .EQ. 0) THEN
               CALL HEADER(IOUNIT)
               WRITE(IOUNIT,9078) IQUN
            END IF
            IF (QFLAG(I) .EQ. 'HOURLY' .AND. L_HRLYSIG(I)) THEN
C              Source uses HOUREMIS option with hourly varying sigmas
               CQFLG = 'HRLYSIG'
            ELSE
               CQFLG = QFLAG(I)
            END IF
            WRITE(IOUNIT,9079) SRCID(I), CNPD, AQS(I),
     &              AXS(I), AYS(I), CAZS, AHS(I), RADIUS(I),
     &              NVERTS(I), ASZINI(I), URB, CQFLG
         END IF
      END DO

C     Write Out The AREAPOLY Source Data, If Any
      INDC = 0
      DO I = 1, NUMSRC
         IF (SRCTYP(I) .EQ. 'AREAPOLY') THEN
            INDC = INDC + 1
            IF (URBSRC(I) .EQ. 'Y') THEN
               URB = 'YES'
            ELSE
               URB = 'NO'
            END IF
            IF (L_METHOD2(I)) THEN
               WRITE(CNPD,'("METH2")')
            ELSE
               WRITE(CNPD,'(I4,1X)') INPD(I)
            END IF
            IF (L_FLATSRC(I)) THEN
               WRITE(CAZS,'(4X,"FLAT")')
            ELSE
               WRITE(CAZS,'(F8.1)') AZS(I)
            END IF
            IF (MOD(INDC-1,40) .EQ. 0) THEN
               CALL HEADER(IOUNIT)
               WRITE(IOUNIT,9080) IQUN
            END IF
            IF (QFLAG(I) .EQ. 'HOURLY' .AND. L_HRLYSIG(I)) THEN
C              Source uses HOUREMIS option with hourly varying sigmas
               CQFLG = 'HRLYSIG'
            ELSE
               CQFLG = QFLAG(I)
            END IF
            WRITE(IOUNIT,9081) SRCID(I), CNPD, AQS(I),
     &              AXS(I), AYS(I), CAZS, AHS(I), NVERTS(I),
     &              ASZINI(I), URB, CQFLG
         END IF
      END DO

C     Write Out The Line Source Data, If Any
      INDC = 0
      DO I = 1, NUMSRC
         IF (SRCTYP(I) .EQ. 'LINE') THEN
            INDC = INDC + 1
            IF (URBSRC(I) .EQ. 'Y') THEN
               URB = 'YES'
            ELSE
               URB = 'NO'
            END IF
            IF (L_METHOD2(I)) THEN
               WRITE(CNPD,'("METH2")')
            ELSE
               WRITE(CNPD,'(I4,1X)') INPD(I)
            END IF
            IF (L_FLATSRC(I)) THEN
               WRITE(CAZS,'(4X,"FLAT")')
            ELSE
               WRITE(CAZS,'(F8.1)') AZS(I)
            END IF
            IF (MOD(INDC-1,40) .EQ. 0) THEN
               CALL HEADER(IOUNIT)
               WRITE(IOUNIT,89076) IQUN
            END IF
            IF (QFLAG(I) .EQ. 'HOURLY' .AND. L_HRLYSIG(I)) THEN
C              Source uses HOUREMIS option with hourly varying sigmas
               CQFLG = 'HRLYSIG'
            ELSE
               CQFLG = QFLAG(I)
            END IF
            WRITE(IOUNIT,89077) SRCID(I), CNPD, AQS(I),
     &              AXS1(I), AYS1(I), AXS2(I), AYS2(I),CAZS, AHS(I),
     &              AWIDTH(I), ASZINI(I), URB, CQFLG
         END IF
      END DO

C     Write Out The RLINE Source Data, If Any
      INDC = 0
      DO I = 1, NUMSRC
         IF ((SRCTYP(I) .EQ. 'RLINE') .OR.
     &           (SRCTYP(I) .EQ. 'RLINEXT')) THEN
            INDC = INDC + 1
            IF (URBSRC(I) .EQ. 'Y') THEN
               URB = 'YES'
            ELSE
               URB = 'NO'
            END IF
            IF (L_METHOD2(I)) THEN
               WRITE(CNPD,'("METH2")')
            ELSE
               WRITE(CNPD,'(I4,1X)') INPD(I)
            END IF
            IF (L_FLATSRC(I)) THEN
               WRITE(CAZS,'(4X,"FLAT")')
            ELSE
               WRITE(CAZS,'(F8.1)') AZS(I)
            END IF
            IF (MOD(INDC-1,40) .EQ. 0) THEN
               IF (RLMOVESCONV) THEN
                  WRITE (IOUNIT,9109)
               ELSE
                  CALL HEADER(IOUNIT)
                  WRITE (IOUNIT,9110) IQUN
               END IF
            END IF
            WRITE(IOUNIT,9111) SRCID(I), CNPD,
     &              RLSOURCE(I)%QEMIS, RLSOURCE(I)%XSB,
     &              RLSOURCE(I)%YSB, RLSOURCE(I)%ZSB,
     &              RLSOURCE(I)%XSE, RLSOURCE(I)%YSE,
     &              RLSOURCE(I)%ZSE, CAZS,
     &              RLSOURCE(I)%DCL, RLSOURCE(I)%INIT_SIGMAZ,
     &              RLSOURCE(I)%WIDTH, URB, QFLAG(I)
          END IF
      END DO

C     Write Out The RLINE Source Configuration Parameters for
C     Barriers, If Any
      INDC = 0
      DO I = 1, NUMSRC
         IF ((SRCTYP(I) .EQ. 'RLINE') .OR.
     &           (SRCTYP(I) .EQ. 'RLINEXT')) THEN
            IF (RLSOURCE(I)%HTWALL > 0.0) THEN
               INDC = INDC + 1
            IF (MOD(INDC-1,40) .EQ. 0) THEN
               WRITE(IOUNIT,9112)
            END IF
            WRITE(IOUNIT,9113) SRCID(I),
     &              RLSOURCE(I)%HTWALL, RLSOURCE(I)%DCLWALL,
     &              RLSOURCE(I)%HTWALL2, RLSOURCE(I)%DCLWALL2
            END IF
         END IF
      END DO

C     Write Out The RLINE Source Configuration Parameters for
C     Depressed Roadways, If Any
      INDC = 0
      DO I = 1, NUMSRC
         IF ((SRCTYP(I) .EQ. 'RLINE') .OR.
     &           (SRCTYP(I) .EQ. 'RLINEXT')) THEN
            IF (RLSOURCE(I)%DEPTH .NE. 0.0) THEN
               INDC = INDC + 1
            IF (MOD(INDC-1,40) .EQ. 0) THEN
               WRITE(IOUNIT,9114)
            END IF
            WRITE(IOUNIT,9115) SRCID(I),
     &              RLSOURCE(I)%DEPTH, RLSOURCE(I)%WTOP,
     &              RLSOURCE(I)%WBOTTOM
            END IF
         END IF
      END DO

C*    Write Out The OpenPit Source Data, If Any
      INDC = 0
      DO I = 1, NUMSRC
         IF (SRCTYP(I) .EQ. 'OPENPIT') THEN
            INDC = INDC + 1
            IF (URBSRC(I) .EQ. 'Y') THEN
               URB = 'YES'
            ELSE
               URB = 'NO'
            END IF
            IF (L_METHOD2(I)) THEN
               WRITE(CNPD,'("METH2")')
            ELSE
               WRITE(CNPD,'(I4,1X)') INPD(I)
            END IF
            IF (L_FLATSRC(I)) THEN
               WRITE(CAZS,'(4X,"FLAT")')
            ELSE
               WRITE(CAZS,'(F8.1)') AZS(I)
            END IF
            IF (MOD(INDC-1,40) .EQ. 0) THEN
               CALL HEADER(IOUNIT)
               WRITE(IOUNIT,9082) IQUN
            END IF
            WRITE(IOUNIT,9083) SRCID(I), CNPD, AQS(I),
     &              AXS(I), AYS(I), CAZS, AHS(I), AXINIT(I), AYINIT(I),
     &              AANGLE(I), AVOLUM(I), URB, QFLAG(I)
         END IF
      END DO

C     Write Out The Buoyant Line Source Data, If Any
      INDC = 0
      DO I = 1, NUMSRC
         IF  (SRCTYP(I) .EQ. 'BUOYLINE' .AND.
     &         (.NOT. BLOUTPROCESSED)) THEN
C           BLOUTPROCESSED lets AERMOD know that all lines associated
C            with the buoyant line source were written on the first pass
C            through the sources since all the lines are processed
C            as one source
         DO NBL = 1, NBLTOTAL
            INDC = INDC + 1
            IF (URBSRC(I) .EQ. 'Y') THEN
               URB = 'YES'
            ELSE
               URB = 'NO'
            END IF
            IF (L_METHOD2(I)) THEN
               WRITE(CNPD,'("METH2")')
            ELSE
               WRITE(CNPD,'(I4,1X)') INPD(I)
            END IF
            IF (L_FLATSRC(I)) THEN
               WRITE(CAZS,'(4X,"FLAT")')
            ELSE
               WRITE(CAZS,'(F8.1)') BLINEPARMS(NBL)%ELEV
            END IF
            IF (MOD(INDC-1,40) .EQ. 0) THEN
               CALL HEADER(IOUNIT)
               WRITE(IOUNIT,9086) IQUN
            END IF
            WRITE(IOUNIT,9087) BLINEPARMS(NBL)%SRCID, CNPD,
     &              BLINEPARMS(NBL)%BLQS, BLINEPARMS(NBL)%XBEG,
     &              BLINEPARMS(NBL)%YBEG, BLINEPARMS(NBL)%XEND,
     &              BLINEPARMS(NBL)%YEND, CAZS,
     &              BLINEPARMS(NBL)%BLHS, URB,
     &              QFLAG(BLINEPARMS(NBL)%ISRCNUM)                       ! CRT 12/4/2017
 9087         FORMAT(1X,A12,2X,A5,2X,E11.5,4F10.1,A8,F9.2,7X,A3,3X,A7)
         END DO

         WRITE(IOUNIT,9088)
 9088         FORMAT(/6X,'BL GRP',3X,'AVG BLDG',3X,'AVG BLDG',3X,
     &         'AVG BLDG',3X,
     &         'AVG LINE',3X,'AVG SEPARATION',5X,'AVG BUOYANCY',
     &         /16X,'LENGTH',5X,'HEIGHT',5X,'WIDTH',6X,'WIDTH',5X,
     &         'BETWEEN LINES',7X,'PARAMETER',1X,
     &         /15X,'(METERS)',3X,'(METERS)',3X,'(METERS)',3X,
     &         '(METERS)',6X,'(METERS)',6X,'(METER**4/SEC**3)',1X
     &         /66(' -')/)

C Multiple_BuoyLines_D41_Wood
C        Processing for multiple buoyant lines
         DO K = 1,NUMBLAVGINP
            WRITE(IOUNIT,9089) BLAVGINP_GRPID(K),
     &               BLAVGINP_LLEN(K), BLAVGINP_BHGT(K),
     &               BLAVGINP_BWID(K), BLAVGINP_LWID(K),
     &               BLAVGINP_BSEP(K), BLAVGINP_FPRM(K)
         END DO
 9089       FORMAT(6X,A8,3X,F8.1,3X,F8.1,3X,F8.1,3X,F8.1,6X,F8.1,
     &              10X,F8.1)
         BLOUTPROCESSED = .TRUE.
         END IF
      END DO

      IF (.NOT. PSDCREDIT) THEN
C        Print The Source Group IDs with Source IDs
         INDC = 0
         DO J = 1, NUMGRP
            INGRP = 0
            DO K = 1, NUMSRC
               IF (IGROUP(K,J) .EQ. 1) THEN
                  INGRP = INGRP + 1
                  WORKID(INGRP) = SRCID(K)
               END IF
            END DO
C ---       Check for BACKGRND "source" being included
C           in source group
            IF (GRP_BACK(J)) THEN
               INGRP = INGRP + 1
               WORKID(INGRP) = 'BACKGROUND'
            END IF
C           Determine Number of Lines @ 8/Line
            NL = 1 + INT((INGRP-1)/8)
            DO K = 1, NL
               INDC = INDC + 1
               IF (MOD(INDC-1,20) .EQ. 0) THEN
                  CALL HEADER(IOUNIT)
                  WRITE(IOUNIT,9058)
               END IF
               IF (K .EQ. 1 .AND. K .EQ. NL) THEN
                  WRITE(IOUNIT,9068) GRPID(J), (WORKID(I),I=1,INGRP)
               ELSE IF (K .EQ. 1 .AND. K .NE. NL) THEN
                  WRITE(IOUNIT,9068) GRPID(J), (WORKID(I),I=1,8*K)
               ELSE IF (K .EQ. NL) THEN
                  WRITE(IOUNIT,9067) (WORKID(I),I=1+8*(K-1),INGRP)
               ELSE
                  WRITE(IOUNIT,9067) (WORKID(I),I=1+8*(K-1),8*K)
               END IF
            END DO
         END DO
      END IF

C     Print The OLM Source Group IDs with Source IDs
      INDC = 0
      DO J = 1, NUMOLM
         INGRP = 0
         DO K = 1, NUMSRC
            IF (IGRP_OLM(K,J) .EQ. 1) THEN
               INGRP = INGRP + 1
               WORKID(INGRP) = SRCID(K)
            END IF
         END DO
C        Determine Number of Lines @ 8/Line
         NL = 1 + INT((INGRP-1)/8)
         DO K = 1, NL
            INDC = INDC + 1
            IF (MOD(INDC-1,20) .EQ. 0) THEN
               CALL HEADER(IOUNIT)
               WRITE(IOUNIT,9059)
            END IF
            IF (K .EQ. 1 .AND. K .EQ. NL) THEN
               WRITE(IOUNIT,9068) OLMID(J), (WORKID(I),I=1,INGRP)
            ELSE IF (K .EQ. 1 .AND. K .NE. NL) THEN
               WRITE(IOUNIT,9068) OLMID(J), (WORKID(I),I=1,8*K)
            ELSE IF (K .EQ. NL) THEN
               WRITE(IOUNIT,9067) (WORKID(I),I=1+8*(K-1),INGRP)
            ELSE
               WRITE(IOUNIT,9067) (WORKID(I),I=1+8*(K-1),8*K)
            END IF
         END DO
      END DO

C     Print The PSD Source Group IDs with Source IDs for PSDCREDIT Option
      INDC = 0
      DO J = 1, NUMPSD
         INGRP = 0
         DO K = 1, NUMSRC
            IF (IGRP_PSD(K,J) .EQ. 1) THEN
               INGRP = INGRP + 1
               WORKID(INGRP) = SRCID(K)
            END IF
         END DO
C        Determine Number of Lines @ 8/Line
         NL = 1 + INT((INGRP-1)/8)
         DO K = 1, NL
            INDC = INDC + 1
            IF (MOD(INDC-1,20) .EQ. 0) THEN
               CALL HEADER(IOUNIT)
               WRITE(IOUNIT,99059)
            END IF
            IF (K .EQ. 1 .AND. K .EQ. NL) THEN
               WRITE(IOUNIT,9068) PSDID(J), (WORKID(I),I=1,INGRP)
            ELSE IF (K .EQ. 1 .AND. K .NE. NL) THEN
               WRITE(IOUNIT,9068) PSDID(J), (WORKID(I),I=1,8*K)
            ELSE IF (K .EQ. NL) THEN
               WRITE(IOUNIT,9067) (WORKID(I),I=1+8*(K-1),INGRP)
            ELSE
               WRITE(IOUNIT,9067) (WORKID(I),I=1+8*(K-1),8*K)
            END IF
         END DO
      END DO

C     Print The URBAN Areas with applicable Source IDs
      INDC = 0
      DO J = 1, NUMURB
         INGRP = 0
         DO K = 1, NUMSRC
            IF (IURBGRP(K,J) .EQ. 1) THEN
               INGRP = INGRP + 1
               WORKID(INGRP) = SRCID(K)
            END IF
         END DO
C        Determine Number of Lines @ 8/Line
         NL = 1 + INT((INGRP-1)/8)
         DO K = 1, NL
            INDC = INDC + 1
            IF (MOD(INDC-1,20) .EQ. 0) THEN
               CALL HEADER(IOUNIT)
               WRITE(IOUNIT,99058)
            END IF
            IF (K .EQ. 1 .AND. K .EQ. NL) THEN
               WRITE(IOUNIT,99068) URBID(J), URBPOP(J),
     &                                      (WORKID(I),I=1,INGRP)
            ELSE IF (K .EQ. 1 .AND. K .NE. NL) THEN
               WRITE(IOUNIT,99068) URBID(J), URBPOP(J),
     &                                      (WORKID(I),I=1,8*K)
            ELSE IF (K .EQ. NL) THEN
               WRITE(IOUNIT,9067) (WORKID(I),I=1+8*(K-1),INGRP)
            ELSE
               WRITE(IOUNIT,9067) (WORKID(I),I=1+8*(K-1),8*K)
            END IF
         END DO
      END DO

C     Print out NO2_RATIO Data for OLM, PVMRM and TTRM Options
      IF (OLM .OR. PVMRM .OR. RUNTTRM .OR. GRSM) THEN
         INDC = 0
         DO I = 1, NUMSRC, 4
            INDC = INDC + 1
            IF (MOD(INDC-1,40) .EQ. 0) THEN
               CALL HEADER(IOUNIT)
               WRITE(IOUNIT,9060)
            END IF
            IF (I+3 .LE. NUMSRC) THEN
               WRITE(IOUNIT,9061) SRCID(I), ANO2_RATIO(I),
     &                            SRCID(I+1), ANO2_RATIO(I+1),
     &                            SRCID(I+2), ANO2_RATIO(I+2),
     &                            SRCID(I+3), ANO2_RATIO(I+3)
            ELSE IF (I+2 .LE. NUMSRC) THEN
               WRITE(IOUNIT,9061) SRCID(I), ANO2_RATIO(I),
     &                            SRCID(I+1), ANO2_RATIO(I+1),
     &                            SRCID(I+2), ANO2_RATIO(I+2)
            ELSE IF (I+1 .LE. NUMSRC) THEN
               WRITE(IOUNIT,9061) SRCID(I), ANO2_RATIO(I),
     &                            SRCID(I+1), ANO2_RATIO(I+1)
            ELSE
               WRITE(IOUNIT,9061) SRCID(I), ANO2_RATIO(I)
            END IF
         END DO
      END IF

C     Print Out Wet or Dry Deposition Information.
      INDC = 0
      DO I = 1, NUMSRC
         NPD = INPD(I)
         IF (NPD .NE. 0 .AND. .NOT.L_METHOD2(I)) THEN
            INDC = INDC + 1
            IF (MOD(INDC-1,3) .EQ. 0) THEN
               CALL HEADER(IOUNIT)
               WRITE(IOUNIT,9049)
            END IF
            WRITE(IOUNIT,9050) SRCID(I), SRCTYP(I)
            WRITE(IOUNIT,9051) (APHI(J,I),J=1,NPD)
            WRITE(IOUNIT,9052) (APDIAM(J,I),J=1,NPD)
            WRITE(IOUNIT,9053) (APDENS(J,I),J=1,NPD)
         ELSE IF (NPD .NE. 0 .AND. L_METHOD2(I)) THEN
C           Summarize inputs for Method 2 particle deposition
            INDC = INDC + 1
            IF (MOD(INDC-1,3) .EQ. 0) THEN
               CALL HEADER(IOUNIT)
               WRITE(IOUNIT,9049)
            END IF
            WRITE(IOUNIT,9050)  SRCID(I), SRCTYP(I)
            WRITE(IOUNIT,99051) (FINEMASS(I),J=1,NPD)
            WRITE(IOUNIT,9052)  (APDIAM(J,I),J=1,NPD)
            WRITE(IOUNIT,9053)  (APDENS(J,I),J=1,NPD)
         ELSE IF (LWGAS .OR. (LDGAS .AND. .NOT.LUSERVD)) THEN
C           Summarize inputs for gas deposition option
            INDC = INDC + 1
            IF (MOD(INDC-1,3) .EQ. 0) THEN
               CALL HEADER(IOUNIT)
               WRITE(IOUNIT,9049)
            END IF
            WRITE(IOUNIT,9050) SRCID(I), SRCTYP(I)
            IF (LDGAS) THEN
               WRITE(IOUNIT,99090) PDIFF(I)
               WRITE(IOUNIT,99091) PDIFFW(I)
               WRITE(IOUNIT,99093) RCLI(I)
               WRITE(IOUNIT,9094)  HENRY(I)
            END IF

         END IF
      END DO

C     Write Out Direction Specific Bldg. Dimensions, If Present
      INDC = 0
      DO I = 1, NUMSRC
         BLDING = 'NO'
         IF (NSEC .GT. 0) THEN
C ---       Check for building data for this source
            DO J = 1, NSEC
               IF(ADSBH(J,I).NE.0.0D0 .AND. ADSBW(J,I).NE.0.0D0
     &                                .AND. ADSBL(J,I).NE.0.0D0) THEN
c --------------------------------------------------------------
                  BLDING = 'YES'
                  EXIT
               END IF
            END DO
         END IF
         IF (BLDING .EQ. 'YES') THEN
            INDC = INDC + 1
C           Print Out Direction Specific Bldg. Dimensions
            IF (MOD(INDC-1,4) .EQ. 0) THEN
               CALL HEADER(IOUNIT)
               WRITE(IOUNIT,9064)
            END IF
            WRITE(IOUNIT,9062) SRCID(I),
     &          (J,DABS(ADSBH(J,I)),ADSBW(J,I),ADSBL(J,I),ADSXADJ(J,I),
     &            ADSYADJ(J,I),J=1,NSEC)

C ---       Print the type of structure: rectangular or streamlined
C            when running the AWMADWNW options
            IF( L_AWMADW )THEN
               IF (L_STRMLN_BLDG) THEN
                  WRITE (IOUNIT,9065) SRCID(I)
               ELSE IF (L_RECT_BLDG) THEN
                  WRITE (IOUNIT,9066) SRCID(I)
               ENDIF
            END IF
c --------------------------------------------------------------------
         END IF
      END DO

C     Print Source Emission Rate Scalars.
      INDC = 0
      DO I = 1, NUMSRC
         IF (QFLAG(I) .EQ. 'SEASON') THEN
            INDC = INDC + 1
            IF (MOD(INDC-1,6) .EQ. 0) THEN
               CALL HEADER(IOUNIT)
               WRITE(IOUNIT,9002)
               WRITE(IOUNIT,9004) (SEASON(I1),I1=1,4)
            END IF
            WRITE(IOUNIT,9005) SRCID(I),SRCTYP(I)
            WRITE(IOUNIT,9006) (QFACT(I1,I),I1=1,4)
         END IF
      END DO

      INDC = 0
      DO I = 1, NUMSRC
         IF (QFLAG(I) .EQ. 'MONTH') THEN
            INDC = INDC + 1
            IF (MOD(INDC-1,6) .EQ. 0) THEN
               CALL HEADER(IOUNIT)
               WRITE(IOUNIT,9007)
               WRITE(IOUNIT,9008)
               WRITE(IOUNIT,9013)
            END IF
            WRITE(IOUNIT,9009) SRCID(I),SRCTYP(I)
            WRITE(IOUNIT,9010) (QFACT(I1,I),I1=1,12)
         END IF
      END DO

      INDC = 0
      DO I = 1, NUMSRC
         IF (QFLAG(I) .EQ. 'HROFDY') THEN
            INDC = INDC + 1
            IF (MOD(INDC-1,5) .EQ. 0) THEN
               CALL HEADER(IOUNIT)
               WRITE(IOUNIT,9011)
               WRITE(IOUNIT,9012)
               WRITE(IOUNIT,9013)
            END IF
            WRITE(IOUNIT,9009) SRCID(I),SRCTYP(I)
            WRITE(IOUNIT,9014) (I1,QFACT(I1,I),I1=1,24)
         END IF
      END DO

      INDC = 0
      DO I = 1, NUMSRC
         IF (QFLAG(I) .EQ. 'WSPEED') THEN
            INDC = INDC + 1
            IF (MOD(INDC-1,3) .EQ. 0) THEN
               CALL HEADER(IOUNIT)
               WRITE(IOUNIT,9015)
               WRITE(IOUNIT,9013)
            END IF
            WRITE(IOUNIT,9009) SRCID(I),SRCTYP(I)
            WRITE(IOUNIT,9025) (J, J=1,6)
            WRITE(IOUNIT,9024) (QFACT(I2,I),I2=1,6)
         END IF
      END DO

      INDC = 0
      DO I = 1, NUMSRC
         IF (QFLAG(I) .EQ. 'SEASHR') THEN
            INDC = INDC + 1
            CALL HEADER(IOUNIT)
            WRITE(IOUNIT,9018)
            WRITE(IOUNIT,9012)
            WRITE(IOUNIT,9013)
            WRITE(IOUNIT,9009) SRCID(I),SRCTYP(I)
            DO I1 = 1, 4
               IFR = (I1-1)*24
               WRITE(IOUNIT,9019) SEASON(I1)
               WRITE(IOUNIT,9014) (I2,QFACT(I2+IFR,I),I2=1,24)
            END DO
         END IF
      END DO

      INDC = 0
      DO I = 1, NUMSRC
         IF (QFLAG(I) .EQ. 'HRDOW') THEN
            INDC = INDC + 1
            CALL HEADER(IOUNIT)
            WRITE(IOUNIT,99218)
            WRITE(IOUNIT,99009) SRCID(I), SRCTYP(I)
            WRITE(IOUNIT,99012)
            WRITE(IOUNIT,99013)
            DO I1 = 1, 3
               IDW = (I1-1)*24
               WRITE(IOUNIT,99021) DAYOFWEEK(I1)
               WRITE(IOUNIT,99014) (I3,QFACT(I3+IDW,I),I3=1,24)
            END DO
         END IF
      END DO

      INDC = 0
      DO I = 1, NUMSRC
         IF (QFLAG(I) .EQ. 'HRDOW7') THEN
            INDC = INDC + 1
            CALL HEADER(IOUNIT)
            WRITE(IOUNIT,79218)
            WRITE(IOUNIT,99009) SRCID(I), SRCTYP(I)
            WRITE(IOUNIT,99012)
            WRITE(IOUNIT,99013)
            DO I1 = 1, 7
               IDW = (I1-1)*24
               WRITE(IOUNIT,99021) DAYOFWEEK7(I1)
               WRITE(IOUNIT,99014) (I3,QFACT(I3+IDW,I),I3=1,24)
            END DO
         END IF
      END DO

      INDC = 0
      DO I = 1, NUMSRC
         IF (QFLAG(I) .EQ. 'SHRDOW') THEN
            INDC = INDC + 1
            CALL HEADER(IOUNIT)
            WRITE(IOUNIT,99018)
            WRITE(IOUNIT,99009) SRCID(I), SRCTYP(I)
            WRITE(IOUNIT,99012)
            WRITE(IOUNIT,99013)
            DO I1 = 1, 3
               IDW = (I1-1)*96
               DO I2 = 1, 4
                  IFR = (I2-1)*24
                  WRITE(IOUNIT,99019) SEASON(I2), DAYOFWEEK(I1)
                  WRITE(IOUNIT,99014) (I3,QFACT(I3+IFR+IDW,I),I3=1,24)
               END DO
            END DO
         END IF
      END DO

      INDC = 0
      DO I = 1, NUMSRC
         IF (QFLAG(I) .EQ. 'SHRDOW7') THEN
            INDC = INDC + 1
            CALL HEADER(IOUNIT)
            WRITE(IOUNIT,79018)
            WRITE(IOUNIT,99009) SRCID(I), SRCTYP(I)
            WRITE(IOUNIT,99012)
            WRITE(IOUNIT,99013)
            DO I1 = 1, 7
               IDW = (I1-1)*96
               DO I2 = 1, 4
                  IFR = (I2-1)*24
                  WRITE(IOUNIT,99019) SEASON(I2), DAYOFWEEK7(I1)
                  WRITE(IOUNIT,99014) (I3,QFACT(I3+IFR+IDW,I),I3=1,24)
               END DO
            END DO
         END IF
      END DO

      INDC = 0
      DO I = 1, NUMSRC
         IF (QFLAG(I) .EQ. 'MHRDOW') THEN
            DO I1 = 1, 3
               CALL HEADER(IOUNIT)
               WRITE(IOUNIT,99118)
               WRITE(IOUNIT,99009) SRCID(I), SRCTYP(I)
               WRITE(IOUNIT,99012)
               WRITE(IOUNIT,99013)
               IDW = (I1-1)*288
               DO I2 = 1, 12
                  IFR = (I2-1)*24
                  WRITE(IOUNIT,99020) MONTHS(I2), DAYOFWEEK(I1)
                  WRITE(IOUNIT,99014) (I3,QFACT(I3+IFR+IDW,I),I3=1,24)
               END DO
            END DO
         END IF
      END DO

      INDC = 0
      DO I = 1, NUMSRC
         IF (QFLAG(I) .EQ. 'MHRDOW7') THEN
            DO I1 = 1, 7
               CALL HEADER(IOUNIT)
               WRITE(IOUNIT,79118)
               WRITE(IOUNIT,99009) SRCID(I), SRCTYP(I)
               WRITE(IOUNIT,99012)
               WRITE(IOUNIT,99013)
               IDW = (I1-1)*288
               DO I2 = 1, 12
                  IFR = (I2-1)*24
                  WRITE(IOUNIT,99020) MONTHS(I2), DAYOFWEEK7(I1)
                  WRITE(IOUNIT,99014) (I3,QFACT(I3+IFR+IDW,I),I3=1,24)
               END DO
            END DO
         END IF
      END DO

 9002 FORMAT(/39X,'* SOURCE EMISSION RATE SCALARS WHICH VARY ',
     &       'SEASONALLY *'/)
 9004 FORMAT(40X,4(A6,9X)/20X,40('- ')/)
 9005 FORMAT(/10X,' SOURCE ID = ',A12,' ;  SOURCE TYPE = ',A8,' :')
 9006 FORMAT(38X,4(E10.5,5X))
 9007 FORMAT(/41X,'* SOURCE EMISSION RATE SCALARS WHICH VARY MONTHLY *',
     &       /)
 9008 FORMAT(7X,'JANUARY  FEBRUARY   MARCH     APRIL      MAY       ',
     &  'JUNE      JULY     AUGUST   SEPTEMBER  OCTOBER  NOVEMBER  ',
     &  'DECEMBER'/)
 9009 FORMAT(/' SOURCE ID = ',A12,' ; SOURCE TYPE = ',A8,' :')
99009 FORMAT(' SOURCE ID = ',A12,' ; SOURCE TYPE = ',A8,' :')
 9010 FORMAT(5X,12(E9.3,1X))
 9011 FORMAT(/28X,'* SOURCE EMISSION RATE SCALARS WHICH VARY FOR EACH',
     &       ' HOUR OF THE DAY *'/)
 9012 FORMAT(5X,6('HOUR    SCALAR',6X))
99012 FORMAT(2X,8('HOUR   SCALAR',3X))
 9013 FORMAT(1X,65('- ')/)
99013 FORMAT(1X,65('- '))
 9014 FORMAT(4(5X,6(I3,3X,E10.5,4X)/))
99014 FORMAT(2(3X,8(I2,2X,E9.4,3X)/),3X,8(I2,2X,E9.4,3X))
 9015 FORMAT(/25X,'* SOURCE EMISSION RATE SCALARS WHICH VARY WITH',
     &       ' WIND SPEED *'/)
 9018 FORMAT(/22X,'* SOURCE EMISSION RATE SCALARS WHICH VARY',
     &       ' SEASONALLY AND DIURNALLY (SEASHR) *'/)
99018 FORMAT(/17X,'* SOURCE EMISSION RATE SCALARS WHICH VARY',
     &       ' SEASONALLY, DIURNALLY AND BY DAY OF WEEK (SHRDOW) *'/)
79018 FORMAT(/17X,'* SOURCE EMISSION RATE SCALARS WHICH VARY',
     &       ' SEASONALLY, DIURNALLY AND BY DAY OF WEEK (SHRDOW7) *'/)
99118 FORMAT(/18X,'* SOURCE EMISSION RATE SCALARS WHICH VARY',
     &       ' MONTHLY, DIURNALLY AND BY DAY OF WEEK (MHRDOW) *'/)
79118 FORMAT(/18X,'* SOURCE EMISSION RATE SCALARS WHICH VARY',
     &       ' MONTHLY, DIURNALLY AND BY DAY OF WEEK (MHRDOW7) *'/)
99218 FORMAT(/19X,'* SOURCE EMISSION RATE SCALARS WHICH VARY',
     &       ' DIURNALLY AND BY DAY OF WEEK (HRDOW) *'/)
79218 FORMAT(/19X,'* SOURCE EMISSION RATE SCALARS WHICH VARY',
     &       ' DIURNALLY AND BY DAY OF WEEK (HRDOW7) *'/)
 9019 FORMAT(59X,'SEASON = ',A6)
99019 FORMAT(46X,'SEASON = ',A6,';  DAY OF WEEK = ',A8)
99020 FORMAT(46X,'MONTH = ',A9,';  DAY OF WEEK = ',A8)
99021 FORMAT(46X,'DAY OF WEEK = ',A8)
 9024 FORMAT(26X,6(1X,E12.5))
 9025 FORMAT(/26X,6('   WIND SPEED')/26X,6('   CATEGORY',I2))
 9046 FORMAT(//50X,'*** POINT SOURCE DATA ***'//15X,
     & 'NUMBER EMISSION RATE',20X,'BASE     STACK   STACK',4X,
     & 'STACK     STACK    BLDG   URBAN  CAP/  EMIS RATE',/3X,
     & 'SOURCE',7X,'PART. ',A12,5X,'X',8X,'Y',6X,'ELEV.    ',
     & 'HEIGHT  TEMP.   EXIT VEL. DIAMETER',2X,'EXISTS SOURCE ',
     & 'HOR   SCALAR',/4X,' ID         CATS.              ',
     & 1X,2('(METERS) (METERS) '),'(DEG.K) ',' (M/SEC) ',1X,'(METERS)',
     & 22X,'VARY BY'/65(' -')/)
 9047 FORMAT(1X,A12,2X,A5,2X,E11.5,2F10.1,A8,4F9.2,
     &       4X,A3,5X,A3,2x,A3,2X,A7)
 9049 FORMAT(/48X,'*** SOURCE PARTICULATE/GAS DATA ***'//)
 9050 FORMAT(//8X,'*** SOURCE ID = ',A12,'; SOURCE TYPE = ',A8,' ***')
 9051 FORMAT(/8X,'MASS FRACTION ='/4(:10X,10(:F9.5,', ')/))
99051 FORMAT(/8X,'FINE PARTICLE MASS FRACTION ='/4(:10X,10(:F9.5,', ')
     &       /))
 9052 FORMAT(/8X,'PARTICLE DIAMETER (MICRONS) ='/4(:10X,10(:F9.5,', ')
     &       /))
 9053 FORMAT(/8X,'PARTICLE DENSITY (G/CM**3)  ='/4(:10X,10(:F9.5,', ')
     &       /))
 9058 FORMAT(//43X,'*** SOURCE IDs DEFINING SOURCE GROUPS ***'//
     &          1X,'SRCGROUP ID',46X,'SOURCE IDs'/
     &          1X,'-----------',46X,'----------'/)
99058 FORMAT(//42X,'*** SOURCE IDs DEFINED AS URBAN SOURCES ***'//
     &       2X,'URBAN ID',3X,'URBAN POP',36X,'SOURCE IDs'/
     &       2X,'--------',3X,'---------',36X,'----------'/)
 9059 FORMAT(//41X,'*** SOURCE IDs DEFINING OLM SOURCE GROUPS ***'/
     &         41X,'***        FOR COMBINING PLUMES           ***'//
     &          1X,'OLMGROUP ID',47X,'SOURCE IDs'/
     &          1X,'-----------',47X,'----------'/)
99059 FORMAT(/41X,'*** SOURCE IDs DEFINING PSD SOURCE GROUPS ***'/
     &        41X,'***      FOR PVMRM PSDCREDIT OPTION       ***'//
     &       1X,'PSDGROUP ID',47X,'SOURCE IDs'/)
 9060 FORMAT(//39X,'*** IN-STACK NO2 RATIOS FOR ***'/
     &         39X,'*** OLM/PVMRM/GRSM OPTIONS ***'//
     &       /1X,4('SOURCE_ID',4X,'NO2_RATIO',6X)/)
 9061 FORMAT(1X,4(A12,2X,F7.3,7X))
 9068 FORMAT(/2X,A8,2X,8(1X,A12,','))
99068 FORMAT(/2X,A8,3X,F9.0,2X,7(1X,A12,','))
 9067 FORMAT(/12X,8(1X,A12,','))

c --- PRIME --------------------------------------------------
 9062 FORMAT(/' SOURCE ID: ',A12,
     &    /,2('  IFV    BH      BW      BL     XADJ    YADJ',3X)
     &    ,/,18(2(2X,I3,5(F7.1,','),2X)/))
c ------------------------------------------------------------

 9064 FORMAT(/42X,'*** DIRECTION SPECIFIC BUILDING DIMENSIONS ***'/)
 9065 FORMAT(' BUILDING STRUCTURE TYPE: STREAMLINED FOR SRCID: ',A12,/)
 9066 FORMAT(' BUILDING STRUCTURE TYPE: RECTANGULAR FOR SRCID: ',A12,/)

 9074 FORMAT(//50X,'*** VOLUME SOURCE DATA ***'//15X,
     & 'NUMBER EMISSION RATE',20X,'BASE    RELEASE    INIT.',4X,
     & 'INIT.   URBAN  EMISSION RATE',/3X,
     & 'SOURCE',7X,'PART. ',A12,5X,'X',8X,'Y',6X,'ELEV.   ',
     & 'HEIGHT      SY       SZ     SOURCE  SCALAR VARY',
     & /4X,' ID         CATS.              ',
     & 1X,3('(METERS) (METERS) '),13X,'BY'/61(' -')/)
 9075 FORMAT(1X,A12,2X,A5,2X,E11.5,2F10.1,A8,F9.2,1X,F8.2,1X,
     &       F8.2,5X,A3,3X,A7)
 9076 FORMAT(//50X,'*** AREA SOURCE DATA ***'//15X,
     & 'NUMBER EMISSION RATE',2X,'COORD (SW CORNER)',2X,
     & 'BASE     RELEASE  X-DIM     Y-DIM    ORIENT.',4X,
     & 'INIT.   URBAN  ',
     & 'EMISSION RATE',
     & /3X,'SOURCE',7X,'PART. ',A11,7X,'X',8X,'Y',6X,'ELEV.    ',
     & 'HEIGHT  OF AREA   OF AREA   OF AREA     SZ     SOURCE ',
     & ' SCALAR VARY',/4X,' ID         CATS.   /METER**2)  ',
     & 1X,2('(METERS) (METERS) '),2('(METERS)',2X),' (DEG.)  (METERS)',
     & 14X,'BY'/66(' -')/)
 9077 FORMAT(1X,A12,2X,A5,2X,E11.5,2F10.1,A8,F9.2,3(1X,F9.2),1X,F8.2,
     &       5X,A3,3X,A7)
 9078 FORMAT(//48X,'*** AREACIRC SOURCE DATA ***'//15X,
     & 'NUMBER EMISSION RATE',4X,'CENTER OF AREA',3X,
     & 'BASE     RELEASE  RADIUS     NUMBER     INIT.',
     &  3X,'URBAN  EMISSION RATE',
     & /3X,'SOURCE',7X,'PART. ',A11,7X,'X',8X,'Y',6X,'ELEV.    ',
     & 'HEIGHT   OF AREA   OF VERTS.    SZ     SOURCE  SCALAR VARY',
     & /4X,' ID         CATS.   /METER**2)  ',
     & 1X,2('(METERS) (METERS) '),21X,'(METERS)',14X,'BY'
     & /63(' -')/)
 9079 FORMAT(1X,A12,2X,A5,2X,E11.5,2F10.1,A8,F9.2,2X,F9.2,4X,I4,4X,
     &       F8.2,5X,A3,3X,A7)
 9080 FORMAT(//48X,'*** AREAPOLY SOURCE DATA ***'//15X,
     & 'NUMBER EMISSION RATE',3X,'LOCATION OF AREA',2X,
     & 'BASE     RELEASE  NUMBER      INIT.',3X,'URBAN  EMISSION RATE',
     & /3X,'SOURCE',7X,'PART. ',A11,7X,'X',8X,'Y',6X,'ELEV.    ',
     & 'HEIGHT  OF VERTS.     SZ     SOURCE  SCALAR VARY',
     & /4X,' ID         CATS.   /METER**2)  ',
     & 1X,2('(METERS) (METERS) '),11X,'(METERS)              BY'
     & /63(' -')/)
 9081 FORMAT(1X,A12,2X,A5,2X,E11.5,2F10.1,A8,F9.2,4X,I4,5X,F8.2,5X,
     & A3,3X,A7)
89076 FORMAT(//50X,'*** LINE SOURCE DATA ***'//15X,
     & 'NUMBER EMISSION RATE',5X,'FIRST COORD',8X,'SECOND COORD',5X,
     & 'BASE    RELEASE    WIDTH    INIT.   URBAN  EMISSION RATE',
     & /3X,'SOURCE',7X,'PART. ',A11,8X,'X',7X,'Y',11X,'X',7X,'Y',6X,
     & 'ELEV.   ',
     & 'HEIGHT    OF LINE    SZ     SOURCE SCALAR VARY',
     & /4X,' ID         CATS.   /METER**2)  ',1X,
     & '(METERS) (METERS) ',2X,'(METERS) (METERS) ',2('(METERS)',1X),
     & 1X,'(METERS) (METERS)',13X,'BY'/66(' -')/)
89077 FORMAT(1X,A12,2X,A5,2X,E11.5,4F10.1,A8,F9.2,1X,F9.2,
     &       1X,F8.2,5X,A3,3X,A7)
 9082 FORMAT(//50X,'*** OPENPIT SOURCE DATA ***'//15X,
     & 'NUMBER EMISSION RATE',2X,'COORD (SW CORNER)',2X,
     & 'BASE     RELEASE  X-DIM     Y-DIM    ORIENT.',4X,
     & 'VOLUME',3X,'URBAN  EMISSION RATE',
     & /3X,'SOURCE',7X,'PART. ',A11,7X,'X',8X,'Y',6X,'ELEV.    ',
     & 'HEIGHT  OF PIT    OF PIT    OF PIT     OF PIT   ',
     & 'SOURCE  SCALAR VARY',
     & /4X,' ID         CATS.   /METER**2)  ',
     & 1X,2('(METERS) (METERS) '),2('(METERS)',2X),' (DEG.) ',3X,
     & '(M**3)               BY'
     & /66(' -')/)
 9083 FORMAT(1X,A12,2X,A5,2X,E11.5,2F10.1,A8,F9.2,3(1X,F9.2),
     &       3X,E10.5,2X,A3,3X,A7)

 9109 FORMAT(//40X,'*** RLINE/RLINEXT SOURCE DATA (with MOVES units)',
     &  '***'//10X,
     & 'NUMBER EMISSION RATE',6X,'FIRST COORD',13X,'SECOND COORD',6X,
     & 'BASE',3X,4X,'INIT.  NO.  WIDTH  URBAN  EMIS RATE',
     & /3X,'SOURCE',7X,'PART. GRAMS/HOUR',5X,'X',7X,'Y',7X,
     & 'Z',7X,'X',7X,'Y',7X,'Z',4X,'ELEV.',1X,'DCL',3X,
     & 'SZ    OF    PER',3X,'SOURCE',2X,'SCALAR',
     & /4X,' ID         CATS.    ',
     & 1X,('/LINK)'),4X,('(M)     (M)     (M)'),5X,
     & ('(M)     (M)     (M)   (M)   (M)   (M)'),2X,
     & 'LANES',2X,'LANE',9X,'VARY BY',
     & /66(' -')/)

 9110 FORMAT(//50X,'*** RLINE/RLINEXT SOURCE DATA ***'//15X,
     & 'NUMBER EMISSION RATE',6X,'FIRST COORD',13X,'SECOND COORD',9X,
     & 'BASE',8X,'INIT.  WIDTH  URBAN EMIS RATE',
     & /3X,'SOURCE',7X,'PART. ',A11,5X,'X',7X,'Y',7X,
     & 'Z',8X,'X',7X,'Y',8X,'Z',5X,'ELEV.',3X,'DCL',3X,
     & 'SZ  ',5X,3X,'SOURCE',2X,'SCALAR',
     & /4X,' ID         CATS.    ',
     & 1X,('/METER)'),4X,('(M)     (M)     (M)'),6X,
     & ('(M)     (M)      (M)     (M)    (M)   (M)    (M)'),1X,
     & 10X,'VARY BY',
     & /66(' -')/)

 9111 FORMAT(1X,A12,2X,A5,2X,E11.5,1X,F8.1,1X,F9.1,1X,F5.1,1X,F8.1,1X,
     &  F9.1,1X,F5.1,1X,A8,1X,F5.1,1X,F5.2,1X,F5.2,2X,A3,3X,A7)

 9112 FORMAT(//14X, 'BARRIER',6X,'BARRIER',7X,'BARRIER',7X,'BARRIER',
     & /3X,'SOURCE',5X,'HTWALL',7X,'DCLWALL',7X,'HTWALL2',6X,'DCLWALL2',
     & /4X,'ID',7X,'(METERS)',5X,'(METERS)',6X,'(METERS)',6X,'(METERS)',
     & /66(' -')/)

 9113 FORMAT(1X,A12,1X,F6.2,8X,F6.2,8X,F6.2,8X,F6.2)

 9114 FORMAT(//14X, 'DEPRESSED',5X,'DEPRESSED',3X,'DEPRESSED'
     & /3X,'SOURCE',5X,'DEPTH',12X,'WTOP',7X,'WBOTTOM'
     & /4X,'ID',7X,'(METERS)',5X,'(METERS)',5X,'(METERS)'
     & /66(' -')/)


 9115 FORMAT(1X,A12,1X,F6.2,2(8X,F6.2))

 9086 FORMAT(//50X,'*** BUOYANT LINE SOURCE DATA ***'//15X,
     & 'NUMBER EMISSION RATE',5X,'FIRST COORD',8X,'SECOND COORD',5X,
     & 'BASE    RELEASE    URBAN   EMISSION RATE',
     & /3X,'SOURCE',7X,'PART. ',A11,')',7X,'X',7X,'Y',11X,'X',7X,'Y',6X,
     & 'ELEV.   ','HEIGHT',5X,'SOURCE',2x,'SCALAR VARY',
     & /4X,' ID         CATS.               ',
     & 1X,('(METERS) (METERS)'),3X,('(METERS) (METERS)'),1X,
     & ('(METERS) (METERS)'),16X,'BY'
     & /66(' -')/)

99090 FORMAT(/10X,'DIFF IN AIR (M**2/SEC)     =',2X,E9.2)
99091 FORMAT(/10X,'DIFF IN WATER (M**2/SEC)   =',2X,E9.2)
99093 FORMAT(/10X,'LEAF LIPID RESIST (SEC/M)  =',2X,E9.2)
 9094 FORMAT(/10X,'HENRY`S LAW COEFFICIENT    =',2X,E9.2)

      RETURN
      END

      SUBROUTINE PRTBKG(ISECT)
C***********************************************************************
C                 PRTBKG Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Print Out The Summary of BACKGRND Data
C
C        PROGRAMMER: Roger Brode
C
C        DATE:       February 28, 2011
C
C        INPUTS:  Model Options and Keyword Summarys
C
C        OUTPUTS: Printed Model Outputs
C
C        CALLED FROM:   INPSUM
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: ISECT, I1, I2, I3, IFR, IDW
      CHARACTER SEASON(4)*6, MONTHS(12)*9, DAYOFWEEK(3)*8,
     &          DAYOFWEEK7(7)*8
C Unused: CHARACTER BLDING*3, URB*3, IQUN*12, CAP*3, CQFLG*7
C Unused: CHARACTER CNPD*5, CAZS*8
C Unused: INTEGER :: I, J, K, NL, INDC, INGRP

C     Variable Initializations
      DATA SEASON /'WINTER','SPRING','SUMMER',' FALL '/
      DATA MONTHS /'JANUARY  ','FEBRUARY ','MARCH    ','APRIL    ',
     &             'MAY      ','JUNE     ','JULY     ','AUGUST   ',
     &             'SEPTEMBER','OCTOBER  ','NOVEMBER ','DECEMBER '/
      DATA DAYOFWEEK  /'WEEKDAY ','SATURDAY','SUNDAY  '/
      DATA DAYOFWEEK7 /'MONDAY  ','TUESDAY ','WEDNESDY','THURSDAY',
     &                 'FRIDAY  ','SATURDAY','SUNDAY  '/
      MODNAM = 'PRTBKG'

C     Print User-specified Background Concetrations
      IF (BFLAG(ISECT) .EQ. 'ANNUAL') THEN
         CALL HEADER(IOUNIT)
         WRITE(IOUNIT,9001) ISECT, BackUnits
         WRITE(IOUNIT,9006) BACKGRND(1,1)
      ELSE IF (BFLAG(ISECT) .EQ. 'SEASON') THEN
         CALL HEADER(IOUNIT)
         WRITE(IOUNIT,9002) ISECT, BackUnits
         WRITE(IOUNIT,9004) (SEASON(I1),I1=1,4)
         WRITE(IOUNIT,9006) (BACKGRND(I1,ISECT),I1=1,4)
      ELSE IF (BFLAG(ISECT) .EQ. 'MONTH') THEN
         CALL HEADER(IOUNIT)
         WRITE(IOUNIT,9007) ISECT, BackUnits
         WRITE(IOUNIT,9008)
         WRITE(IOUNIT,9013)
         WRITE(IOUNIT,9010) (BACKGRND(I1,ISECT),I1=1,12)
      ELSE IF (BFLAG(ISECT) .EQ. 'HROFDY') THEN
         CALL HEADER(IOUNIT)
         WRITE(IOUNIT,9011) ISECT, BackUnits
         WRITE(IOUNIT,9012)
         WRITE(IOUNIT,9013)
         WRITE(IOUNIT,9014) (I1,BACKGRND(I1,ISECT),I1=1,24)
      ELSE IF (BFLAG(ISECT) .EQ. 'SEASHR') THEN
         CALL HEADER(IOUNIT)
         WRITE(IOUNIT,9018) ISECT, BackUnits
         WRITE(IOUNIT,9012)
         WRITE(IOUNIT,9013)
         DO I1 = 1, 4
            IFR = (I1-1)*24
            WRITE(IOUNIT,9019) SEASON(I1)
            WRITE(IOUNIT,9014) (I2,BACKGRND(I2+IFR,ISECT),I2=1,24)
         END DO
      ELSE IF (BFLAG(ISECT) .EQ. 'HRDOW') THEN
         CALL HEADER(IOUNIT)
         WRITE(IOUNIT,99218) ISECT, BackUnits
         WRITE(IOUNIT,99012)
         WRITE(IOUNIT,99013)
         DO I1 = 1, 3
            IDW = (I1-1)*24
            WRITE(IOUNIT,99021) DAYOFWEEK(I1)
            WRITE(IOUNIT,99014) (I3,BACKGRND(I3+IDW,ISECT),I3=1,24)
         END DO
      ELSE IF (BFLAG(ISECT) .EQ. 'HRDOW7') THEN
         CALL HEADER(IOUNIT)
         WRITE(IOUNIT,79218) ISECT, BackUnits
         WRITE(IOUNIT,99012)
         WRITE(IOUNIT,99013)
         DO I1 = 1, 7
            IDW = (I1-1)*24
            WRITE(IOUNIT,99021) DAYOFWEEK7(I1)
            WRITE(IOUNIT,99014) (I3,BACKGRND(I3+IDW,ISECT),I3=1,24)
         END DO
      ELSE IF (BFLAG(ISECT) .EQ. 'SHRDOW') THEN
         CALL HEADER(IOUNIT)
         WRITE(IOUNIT,99018) ISECT, BackUnits
         WRITE(IOUNIT,99012)
         WRITE(IOUNIT,99013)
         DO I1 = 1, 3
            IDW = (I1-1)*96
            DO I2 = 1, 4
               IFR = (I2-1)*24
               WRITE(IOUNIT,99019) SEASON(I2), DAYOFWEEK(I1)
               WRITE(IOUNIT,99014) (I3,BACKGRND(I3+IFR+IDW,ISECT),
     &                                                     I3=1,24)
            END DO
         END DO
      ELSE IF (BFLAG(ISECT) .EQ. 'SHRDOW7') THEN
         CALL HEADER(IOUNIT)
         WRITE(IOUNIT,79018) ISECT, BackUnits
         WRITE(IOUNIT,99012)
         WRITE(IOUNIT,99013)
         DO I1 = 1, 7
            IDW = (I1-1)*96
            DO I2 = 1, 4
               IFR = (I2-1)*24
               WRITE(IOUNIT,99019) SEASON(I2), DAYOFWEEK7(I1)
               WRITE(IOUNIT,99014) (I3,BACKGRND(I3+IFR+IDW,ISECT),
     &                                                     I3=1,24)
            END DO
         END DO
      ELSE IF (BFLAG(ISECT) .EQ. 'MHRDOW') THEN
         DO I1 = 1, 3
            CALL HEADER(IOUNIT)
            WRITE(IOUNIT,99118) ISECT, BackUnits
            WRITE(IOUNIT,99012)
            WRITE(IOUNIT,99013)
            IDW = (I1-1)*288
            DO I2 = 1, 12
               IFR = (I2-1)*24
               WRITE(IOUNIT,99020) MONTHS(I2), DAYOFWEEK(I1)
               WRITE(IOUNIT,99014) (I3,BACKGRND(I3+IFR+IDW,ISECT),
     &                                                     I3=1,24)
            END DO
         END DO
      ELSE IF (BFLAG(ISECT) .EQ. 'MHRDOW7') THEN
         DO I1 = 1, 7
            CALL HEADER(IOUNIT)
            WRITE(IOUNIT,79118) ISECT, BackUnits
            WRITE(IOUNIT,99012)
            WRITE(IOUNIT,99013)
            IDW = (I1-1)*288
            DO I2 = 1, 12
               IFR = (I2-1)*24
               WRITE(IOUNIT,99020) MONTHS(I2), DAYOFWEEK7(I1)
               WRITE(IOUNIT,99014) (I3,BACKGRND(I3+IFR+IDW,ISECT),
     &                                                     I3=1,24)
            END DO
         END DO
      END IF

 9001 FORMAT(/31X,'* SECT',I1,' ANNUAL (NON-VARYING) BACKGROUND ',
     &       'CONCENTRATION (',A5,') *'/)
 9002 FORMAT(/31X,'* SECT',I1,' BACKGROUND CONCENTRATIONS WHICH VARY ',
     &       'SEASONALLY (',A5,') *'/)
 9004 FORMAT(40X,4(A6,9X)/20X,40('- ')/)
 9006 FORMAT(38X,4(E10.5,5X))
 9007 FORMAT(/33X,'* SECT',I1,' BACKGROUND CONCENTRATIONS WHICH VARY ',
     &       'MONTHLY (',A5,') *'/)
 9008 FORMAT(7X,'JANUARY  FEBRUARY   MARCH     APRIL      MAY       ',
     &  'JUNE      JULY     AUGUST   SEPTEMBER  OCTOBER  NOVEMBER  ',
     &  'DECEMBER'/)
 9010 FORMAT(5X,12(E9.3,1X))
 9011 FORMAT(/20X,'* SECT',I1,' BACKGROUND CONCENTRATIONS WHICH VARY ',
     &       'FOR EACH HOUR OF THE DAY (',A5,') *'/)
 9012 FORMAT(5X,6('HOUR    BKGRND',6X))
99012 FORMAT(2X,8('HOUR   BKGRND',3X))
 9013 FORMAT(1X,65('- ')/)
99013 FORMAT(1X,65('- '))
 9014 FORMAT(4(5X,6(I3,3X,E10.5,4X)/))
99014 FORMAT(2(3X,8(I2,2X,E9.4,3X)/),3X,8(I2,2X,E9.4,3X))
C Unused: 9015 FORMAT(/17X,'* SECT',I1,' BACKGROUND CONCENTRATIONS WHICH VARY ',
C     &       'WITH WIND SPEED (',A5,') *'/)
 9018 FORMAT(/15X,'* SECT',I1,' BACKGROUND CONCENTRATIONS WHICH VARY',
     &       ' SEASONALLY AND DIURNALLY (SEASHR) (',A5,') *'/)
99018 FORMAT(/9X,'* SECT',I1,' BACKGROUND CONCENTRATIONS WHICH VARY',
     &       ' SEASONALLY, DIURNALLY AND BY DAY OF WEEK (SHRDOW)',
     &       ' (',A5,') *'/)
79018 FORMAT(/9X,'* SECT',I1,' BACKGROUND CONCENTRATIONS WHICH VARY',
     &       ' SEASONALLY, DIURNALLY AND BY DAY OF WEEK (SHRDOW7)',
     &       ' (',A5,') *'/)
99118 FORMAT(/10X,'* SECT',I1,' BACKGROUND CONCENTRATIONS WHICH VARY',
     &       ' MONTHLY, DIURNALLY AND BY DAY OF WEEK (MHRDOW)',
     &       ' (',A5,') *'/)
79118 FORMAT(/10X,'* SECT',I1,' BACKGROUND CONCENTRATIONS WHICH VARY',
     &       ' MONTHLY, DIURNALLY AND BY DAY OF WEEK (MHRDOW7)',
     &       ' (',A5,') *'/)
99218 FORMAT(/11X,'* SECT',I1,' BACKGROUND CONCENTRATIONS WHICH VARY',
     &       ' DIURNALLY AND BY DAY OF WEEK (HRDOW)',
     &       ' (',A5,') *'/)
79218 FORMAT(/11X,'* SECT',I1,' BACKGROUND CONCENTRATIONS WHICH VARY',
     &       ' DIURNALLY AND BY DAY OF WEEK (HRDOW7)',
     &       ' (',A5,') *'/)
 9019 FORMAT(59X,'SEASON = ',A6)
99019 FORMAT(46X,'SEASON = ',A6,';  DAY OF WEEK = ',A8)
99020 FORMAT(46X,'MONTH = ',A9,';  DAY OF WEEK = ',A8)
99021 FORMAT(46X,'DAY OF WEEK = ',A8)
C Unused: 9024 FORMAT(26X,6(1X,E12.5))
C Unused: 9025 FORMAT(/26X,6('   WIND SPEED')/26X,6('   CATEGORY',I2))

      RETURN
      END

      SUBROUTINE PRTREC
C***********************************************************************
C                 PRTREC Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Print Out The Receptor Network Values
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To remove reference to Boundary
C                    Receptors - 4/1/2004
C
C        MODIFIED:   To Adjust Format Statement 9082 for Boundary
C                    Receptors - 9/29/92
C
C        INPUTS:  Arrays of Source Parameters
C                 Arrays of Receptor Locations
C                 Arrays of Model Results
C
C        OUTPUTS: Printed Model Outputs
C
C        CALLED FROM:   INPSUM
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, J, K, INDZ, NX, NY, INDC
      DOUBLE PRECISION :: YCOVAL, XRMS, YRMS, RANGE, RADIAL
      CHARACTER BUF132*132

C     Variable Initializations
      MODNAM = 'PRTREC'
      BUF132 = ' '
      INDZ   = 0

      DO I = 1, INNET
         CALL HEADER(IOUNIT)
         WRITE(IOUNIT,9034)
         WRITE(IOUNIT,9037) NTID(I), NTTYP(I)
         IF (NTTYP(I) .EQ. 'GRIDCART') THEN
            WRITE(IOUNIT,9038)
         ELSE
            WRITE(IOUNIT,9036) XORIG(I), YORIG(I)
            WRITE(IOUNIT,9039)
         END IF
         WRITE(IOUNIT,9040) (XCOORD(J,I),J=1,NUMXPT(I))
         IF (NTTYP(I) .EQ. 'GRIDCART') THEN
            WRITE(IOUNIT,9041)
         ELSE
            WRITE(IOUNIT,9042)
         END IF
         WRITE(IOUNIT,9040) (YCOORD(J,I),J=1,NUMYPT(I))
         IF (ELEV) THEN
C           Print Terrain Heights for Network
C           Set Number of Columns Per Page, NCPP
            NCPP = 9
C           Set Number of Rows Per Page, NRPP
            NRPP = 40
C           Begin LOOP Through Networks
C           Calculate Number of Pages Per X-Group, NPPX, & Per Y-Group, NPPY
            NPPX = 1 + INT((NUMXPT(I)-1)/NCPP)
            NPPY = 1 + INT((NUMYPT(I)-1)/NRPP)
            DO NX = 1, NPPX
               DO NY = 1, NPPY
                  CALL HEADER(IOUNIT)
                  WRITE(IOUNIT,9037) NTID(I), NTTYP(I)
                  WRITE(IOUNIT,9011)
                  IF (NX .EQ. NPPX) THEN
                     IF (NTTYP(I) .EQ. 'GRIDCART') THEN
                        WRITE(IOUNIT,9016)
                        WRITE(IOUNIT,9017) (XCOORD(J,I),J=1+NCPP*(NX-1),
     &                                                    NUMXPT(I))
                     ELSE IF (NTTYP(I) .EQ. 'GRIDPOLR') THEN
                        WRITE(IOUNIT,9018)
                        WRITE(IOUNIT,9019) (XCOORD(J,I),J=1+NCPP*(NX-1),
     &                                                    NUMXPT(I))
                     END IF
                  ELSE
                     IF (NTTYP(I) .EQ. 'GRIDCART') THEN
                        WRITE(IOUNIT,9016)
                        WRITE(IOUNIT,9017) (XCOORD(J,I),J=1+NCPP*(NX-1),
     &                                                    NCPP*NX)
                     ELSE IF (NTTYP(I) .EQ. 'GRIDPOLR') THEN
                        WRITE(IOUNIT,9018)
                        WRITE(IOUNIT,9019) (XCOORD(J,I),J=1+NCPP*(NX-1),
     &                                                    NCPP*NX)
                     END IF
                  END IF
                  WRITE(IOUNIT,9010)
                  IF (NY .EQ. NPPY) THEN
                     DO K = 1+NRPP*(NY-1), NUMYPT(I)
                        IF (NTTYP(I) .EQ. 'GRIDCART') THEN
                           INDZ = NETEND(I) - K*NUMXPT(I) + 1
                           YCOVAL = YCOORD(NUMYPT(I)-K+1,I)
                        ELSE IF (NTTYP(I) .EQ. 'GRIDPOLR') THEN
                           INDZ = NETSTA(I) + (K-1)*NUMXPT(I)
                           YCOVAL = YCOORD(K,I)
                        END IF
                        IF (NX .EQ. NPPX) THEN
                           WRITE(IOUNIT,9013) YCOVAL,
     &                   (AZELEV(INDZ+J-1),J=1+NCPP*(NX-1),NUMXPT(I))
                        ELSE
                           WRITE(IOUNIT,9013) YCOVAL,
     &                   (AZELEV(INDZ+J-1),J=1+NCPP*(NX-1),NCPP*NX)
                        END IF
                     END DO
                  ELSE
                     DO K = 1+NRPP*(NY-1), NRPP*NY
                        IF (NTTYP(I) .EQ. 'GRIDCART') THEN
                           INDZ = NETEND(I) - K*NUMXPT(I) + 1
                           YCOVAL = YCOORD(NUMYPT(I)-K+1,I)
                        ELSE IF (NTTYP(I) .EQ. 'GRIDPOLR') THEN
                           INDZ = NETSTA(I) + (K-1)*NUMXPT(I)
                           YCOVAL = YCOORD(K,I)
                        END IF
                        IF (NX .EQ. NPPX) THEN
                           WRITE(IOUNIT,9013) YCOVAL,
     &                   (AZELEV(INDZ+J-1),J=1+NCPP*(NX-1),NUMXPT(I))
                        ELSE
                           WRITE(IOUNIT,9013) YCOVAL,
     &                   (AZELEV(INDZ+J-1),J=1+NCPP*(NX-1),NCPP*NX)
                        END IF
                     END DO
                  END IF
               END DO
            END DO
C           Print Hill Height Scales for Network
C           Set Number of Columns Per Page, NCPP
            NCPP = 9
C           Set Number of Rows Per Page, NRPP
            NRPP = 40
C           Begin LOOP Through Networks
C           Calculate Number of Pages Per X-Group, NPPX, & Per Y-Group, NPPY
            NPPX = 1 + INT((NUMXPT(I)-1)/NCPP)
            NPPY = 1 + INT((NUMYPT(I)-1)/NRPP)
            DO NX = 1, NPPX
               DO NY = 1, NPPY
                  CALL HEADER(IOUNIT)
                  WRITE(IOUNIT,9037) NTID(I), NTTYP(I)
                  WRITE(IOUNIT,9012)
                  IF (NX .EQ. NPPX) THEN
                     IF (NTTYP(I) .EQ. 'GRIDCART') THEN
                        WRITE(IOUNIT,9016)
                        WRITE(IOUNIT,9017) (XCOORD(J,I),J=1+NCPP*(NX-1),
     &                                                    NUMXPT(I))
                     ELSE IF (NTTYP(I) .EQ. 'GRIDPOLR') THEN
                        WRITE(IOUNIT,9018)
                        WRITE(IOUNIT,9019) (XCOORD(J,I),J=1+NCPP*(NX-1),
     &                                                    NUMXPT(I))
                     END IF
                  ELSE
                     IF (NTTYP(I) .EQ. 'GRIDCART') THEN
                        WRITE(IOUNIT,9016)
                        WRITE(IOUNIT,9017) (XCOORD(J,I),J=1+NCPP*(NX-1),
     &                                                    NCPP*NX)
                     ELSE IF (NTTYP(I) .EQ. 'GRIDPOLR') THEN
                        WRITE(IOUNIT,9018)
                        WRITE(IOUNIT,9019) (XCOORD(J,I),J=1+NCPP*(NX-1),
     &                                                    NCPP*NX)
                     END IF
                  END IF
                  WRITE(IOUNIT,9010)
                  IF (NY .EQ. NPPY) THEN
                     DO K = 1+NRPP*(NY-1), NUMYPT(I)
                        IF (NTTYP(I) .EQ. 'GRIDCART') THEN
                           INDZ = NETEND(I) - K*NUMXPT(I) + 1
                           YCOVAL = YCOORD(NUMYPT(I)-K+1,I)
                        ELSE IF (NTTYP(I) .EQ. 'GRIDPOLR') THEN
                           INDZ = NETSTA(I) + (K-1)*NUMXPT(I)
                           YCOVAL = YCOORD(K,I)
                        END IF
                        IF (NX .EQ. NPPX) THEN
                           WRITE(IOUNIT,9013) YCOVAL,
     &                   (AZHILL(INDZ+J-1),J=1+NCPP*(NX-1),NUMXPT(I))
                        ELSE
                           WRITE(IOUNIT,9013) YCOVAL,
     &                   (AZHILL(INDZ+J-1),J=1+NCPP*(NX-1),NCPP*NX)
                        END IF
                     END DO
                  ELSE
                     DO K = 1+NRPP*(NY-1), NRPP*NY
                        IF (NTTYP(I) .EQ. 'GRIDCART') THEN
                           INDZ = NETEND(I) - K*NUMXPT(I) + 1
                           YCOVAL = YCOORD(NUMYPT(I)-K+1,I)
                        ELSE IF (NTTYP(I) .EQ. 'GRIDPOLR') THEN
                           INDZ = NETSTA(I) + (K-1)*NUMXPT(I)
                           YCOVAL = YCOORD(K,I)
                        END IF
                        IF (NX .EQ. NPPX) THEN
                           WRITE(IOUNIT,9013) YCOVAL,
     &                   (AZHILL(INDZ+J-1),J=1+NCPP*(NX-1),NUMXPT(I))
                        ELSE
                           WRITE(IOUNIT,9013) YCOVAL,
     &                   (AZHILL(INDZ+J-1),J=1+NCPP*(NX-1),NCPP*NX)
                        END IF
                     END DO
                  END IF
               END DO
            END DO
         END IF
         IF (FLGPOL) THEN
C           Print The Receptor Heights Above Ground for This Network
C           Set Number of Columns Per Page, NCPP
            NCPP = 9
C           Set Number of Rows Per Page, NRPP
            NRPP = 40
C           Begin LOOP Through Networks
C           Calculate Number of Pages Per X-Group, NPPX, & Per Y-Group, NPPY
            NPPX = 1 + INT((NUMXPT(I)-1)/NCPP)
            NPPY = 1 + INT((NUMYPT(I)-1)/NRPP)
            DO NX = 1, NPPX
               DO NY = 1, NPPY
                  CALL HEADER(IOUNIT)
                  WRITE(IOUNIT,9037) NTID(I), NTTYP(I)
                  WRITE(IOUNIT,9035)
                  IF (NX .EQ. NPPX) THEN
                     IF (NTTYP(I) .EQ. 'GRIDCART') THEN
                        WRITE(IOUNIT,9016)
                        WRITE(IOUNIT,9017) (XCOORD(J,I),J=1+NCPP*(NX-1),
     &                                                    NUMXPT(I))
                     ELSE IF (NTTYP(I) .EQ. 'GRIDPOLR') THEN
                        WRITE(IOUNIT,9018)
                        WRITE(IOUNIT,9019) (XCOORD(J,I),J=1+NCPP*(NX-1),
     &                                                    NUMXPT(I))
                     END IF
                  ELSE
                     IF (NTTYP(I) .EQ. 'GRIDCART') THEN
                        WRITE(IOUNIT,9016)
                        WRITE(IOUNIT,9017) (XCOORD(J,I),J=1+NCPP*(NX-1),
     &                                                    NCPP*NX)
                     ELSE IF (NTTYP(I) .EQ. 'GRIDPOLR') THEN
                        WRITE(IOUNIT,9018)
                        WRITE(IOUNIT,9019) (XCOORD(J,I),J=1+NCPP*(NX-1),
     &                                                    NCPP*NX)
                     END IF
                  END IF
                  WRITE(IOUNIT,9010)
                  IF (NY .EQ. NPPY) THEN
                     DO K = 1+NRPP*(NY-1), NUMYPT(I)
                        IF (NTTYP(I) .EQ. 'GRIDCART') THEN
                           INDZ = NETEND(I) - K*NUMXPT(I) + 1
                           YCOVAL = YCOORD(NUMYPT(I)-K+1,I)
                        ELSE IF (NTTYP(I) .EQ. 'GRIDPOLR') THEN
                           INDZ = NETSTA(I) + (K-1)*NUMXPT(I)
                           YCOVAL = YCOORD(K,I)
                        END IF
                        IF (NX .EQ. NPPX) THEN
                           WRITE(IOUNIT,9013) YCOVAL,
     &                   (AZFLAG(INDZ+J-1),J=1+NCPP*(NX-1),NUMXPT(I))
                        ELSE
                           WRITE(IOUNIT,9013) YCOVAL,
     &                   (AZFLAG(INDZ+J-1),J=1+NCPP*(NX-1),NCPP*NX)
                        END IF
                     END DO
                  ELSE
                     DO K = 1+NRPP*(NY-1), NRPP*NY
                        IF (NTTYP(I) .EQ. 'GRIDCART') THEN
                           INDZ = NETEND(I) - K*NUMXPT(I) + 1
                           YCOVAL = YCOORD(NUMYPT(I)-K+1,I)
                        ELSE IF (NTTYP(I) .EQ. 'GRIDPOLR') THEN
                           INDZ = NETSTA(I) + (K-1)*NUMXPT(I)
                           YCOVAL = YCOORD(K,I)
                        END IF
                        IF (NX .EQ. NPPX) THEN
                           WRITE(IOUNIT,9013) YCOVAL,
     &                   (AZFLAG(INDZ+J-1),J=1+NCPP*(NX-1),NUMXPT(I))
                        ELSE
                           WRITE(IOUNIT,9013) YCOVAL,
     &                   (AZFLAG(INDZ+J-1),J=1+NCPP*(NX-1),NCPP*NX)
                        END IF
                     END DO
                  END IF
               END DO
            END DO
         END IF
      END DO

      IF (IRSTAT(4).NE.0 .OR. IRSTAT(8).NE.0) THEN
C ---    Include EVALCART receptors with DISCCART receptors.
C        Print Out The Coordinates, Height , Hill Height & Flags For
C        Discrete Cart Receptors

         INDC = 0
         DO I = 1, NUMREC
            IF (RECTYP(I) .EQ. 'DC') THEN
               INDC = INDC + 1
               IF (MOD(INDC-1,90) .EQ. 0) THEN
                  CALL HEADER(IOUNIT)
                  WRITE(IOUNIT,9043)
               END IF
               IF (MOD(INDC,2) .NE. 0) THEN
                  WRITE(BUF132(1:65),9045) AXR(I),AYR(I),AZELEV(I),
     &                                     AZHILL(I),AZFLAG(I)
               ELSE
                  WRITE(BUF132(66:130),9045) AXR(I),AYR(I),AZELEV(I),
     &                                       AZHILL(I),AZFLAG(I)
                  WRITE(IOUNIT,9090) BUF132
                  WRITE(BUF132,9095)
               END IF
            END IF
         END DO
         IF (MOD(INDC,2) .NE. 0) THEN
            WRITE(IOUNIT,9090) BUF132
            WRITE(BUF132,9095)
         END IF
      END IF

      IF (IRSTAT(5) .NE. 0) THEN
C        Print Out The Coordinates, Height & Flags For Discrete Polar Receptors
         INDC = 0
         DO I = 1, NUMREC
            IF (RECTYP(I) .EQ. 'DP') THEN
               INDC = INDC + 1
               XRMS = AXR(I) - AXS(IREF(I))
               YRMS = AYR(I) - AYS(IREF(I))
               RANGE  = DSQRT(XRMS*XRMS + YRMS*YRMS)
               RADIAL = DATAN2(XRMS, YRMS) * RTODEG
               IF(RADIAL .LE. 0.0D0) RADIAL = RADIAL + 360.0D0
               IF (MOD(INDC-1,90) .EQ. 0) THEN
                  CALL HEADER(IOUNIT)
                  WRITE(IOUNIT,9044)
               END IF
               IF (MOD(INDC,2) .NE. 0) THEN
                  WRITE(BUF132(1:65),9047) SRCID(IREF(I)),RANGE,RADIAL,
     &                                     AZELEV(I),AZHILL(I),AZFLAG(I)
               ELSE
                  WRITE(BUF132(66:130),9047) SRCID(IREF(I)),RANGE,
     &                                       RADIAL,AZELEV(I),AZHILL(I),
     &                                       AZFLAG(I)
                  WRITE(IOUNIT,9090) BUF132
                  WRITE(BUF132,9095)
               END IF
            END IF
         END DO
         IF (MOD(INDC,2) .NE. 0) THEN
            WRITE(IOUNIT,9090) BUF132
            WRITE(BUF132,9095)
         END IF
      END IF

 9010 FORMAT(66(' -')/)
 9011 FORMAT(/48X,'* ELEVATION HEIGHTS IN METERS *'/)
 9012 FORMAT(/48X,'* HILL HEIGHT SCALES IN METERS *'/)
 9013 FORMAT(2X,F10.2,1X,'|',1X,9(1X,F12.2,:))
 9016 FORMAT(3X,' Y-COORD  |',48X,'X-COORD (METERS)')
 9017 FORMAT(3X,' (METERS) |',1X,9(1X,F12.2,:))
 9018 FORMAT(3X,'DIRECTION |',48X,'DISTANCE (METERS)')
 9019 FORMAT(3X,'(DEGREES) |',1X,9(1X,F12.2,:))
 9035 FORMAT(/44X,'* RECEPTOR FLAGPOLE HEIGHTS IN METERS *'/)
 9034 FORMAT(/40X,'*** GRIDDED RECEPTOR NETWORK SUMMARY ***')
 9036 FORMAT(/42X,'*** ORIGIN FOR POLAR NETWORK ***'/,
     &      32X,'X-ORIG =',F10.2,' ;   Y-ORIG = ',F10.2,'  (METERS)')
 9037 FORMAT(/34X,'*** NETWORK ID: ',A8,' ;  NETWORK TYPE: ',
     &       A8,' ***')
 9038 FORMAT(/42X,'*** X-COORDINATES OF GRID ***'/
     &       52X,'(METERS)'/)
 9039 FORMAT(/42X,'*** DISTANCE RANGES OF NETWORK ***'/
     &       52X,'(METERS)'/)
 9040 FORMAT(100(5X,10(F10.1,',')/))
 9041 FORMAT(/42X,'*** Y-COORDINATES OF GRID *** ',
     &       /52X,'(METERS)'/)
 9042 FORMAT(/42X,'*** DIRECTION RADIALS OF NETWORK *** ',
     &       /52X,'(DEGREES)'/)
 9043 FORMAT(/45X,'*** DISCRETE CARTESIAN RECEPTORS ***',
     &       /43X,'(X-COORD, Y-COORD, ZELEV, ZHILL, ZFLAG)',
     &       /45X,'              (METERS)'/)
 9044 FORMAT(/43X,'      *** DISCRETE POLAR RECEPTORS ***',
     &       /43X,' ORIGIN:    (DIST, DIR, ZELEV, ZHILL, ZFLAG)',
     &       /43X,' SRCID:    (METERS,DEG,METERS,METERS,METERS)'/)
 9045 FORMAT(4X,' (',4(F9.1,', '),F9.1,'); ')
 9047 FORMAT(1X,A12,': (',F9.1,', ',3(F7.1,', '),F7.1,'); ')
 9090 FORMAT(A132)
 9095 FORMAT(132(' '))

      RETURN
      END

      SUBROUTINE CHKREC
C***********************************************************************
C                 CHKREC Module of the AMS/EPA Regulatory Model - AERMOD
c ----------------------------------------------------------------------
c ---    ISC-PRIME     Version 1.0    Level 970812              Modified
c ---        D. Strimaitis
c ---        Earth Tech, Inc.
c            Prepared for EPRI under contract WO3527-01
c ----------------------------------------------------------------------
C
C        PURPOSE: Print Out The Input Met Data Summary and Source Groups
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To include check for receptors beyond MAXDIST from
C                    sources, using center of AREA/AREACIRC/AREAPOLY and
C                    OPENPIT sources.  MAXDIST is set to 80km under the
C                    FASTALL and FASTAREA options.
C                    R. W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
C
C        MODIFIED:   To account for new area source algorithm, which
C                    allows for receptors located within the area - 7/7/93
C
C        MODIFIED:   To account for OpenPit Source - PES - 7/22/94
C
C        INPUTS:  Source and Receptor Inputs
C
C        OUTPUTS: Listing of Receptors Too Close To Sources
C
C        CALLED FROM:   INPSUM
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: INC, INOUT
      DOUBLE PRECISION :: XSRC, YSRC, DIST, XVM(5), YVM(5)

C     Variable Initializations
      MODNAM = 'CHKREC'
      INC = 0
      XSRC = 0.0D0
      YSRC = 0.0D0

C     Begin Source LOOP
      DO ISRC = 1, NUMSRC

C        Set Effective Source Radius Based on Source Type
         IF (SRCTYP(ISRC)(1:5) .EQ. 'POINT') THEN
            XRAD = 0.0D0
            XSRC = AXS(ISRC)
            YSRC = AYS(ISRC)
         ELSE IF (SRCTYP(ISRC) .EQ. 'VOLUME') THEN
            XRAD = 2.15D0 * ASYINI(ISRC)
            XSRC = AXS(ISRC)
            YSRC = AYS(ISRC)
         ELSE IF (SRCTYP(ISRC)(1:4) .EQ. 'AREA' .OR.
     &            SRCTYP(ISRC) .EQ. 'LINE') THEN
C           Set XRAD to -1 since no minimum distance for
C           AREA and LINE source types.  Use center coordinates
C           for comparison to MAXDIST.
            XRAD = -1.0D0
            XSRC = AXCNTR(ISRC)
            YSRC = AYCNTR(ISRC)
         ELSE IF (SRCTYP(ISRC) .EQ. 'OPENPIT') THEN
C           Set XRAD to -1 since minimum distance for
C           OPENPIT source is handled separately to
C           flag receptors within the boundary of the
C           OPENPIT source.  Use center coordinates
C           for comparison to MAXDIST.
            XRAD   = -1.0D0
            XSRC   = AXCNTR(ISRC)
            YSRC   = AYCNTR(ISRC)
            XVM(1) = AXVERT(1,ISRC)
            XVM(2) = AXVERT(2,ISRC)
            XVM(3) = AXVERT(3,ISRC)
            XVM(4) = AXVERT(4,ISRC)
            XVM(5) = AXVERT(5,ISRC)
            YVM(1) = AYVERT(1,ISRC)
            YVM(2) = AYVERT(2,ISRC)
            YVM(3) = AYVERT(3,ISRC)
            YVM(4) = AYVERT(4,ISRC)
            YVM(5) = AYVERT(5,ISRC)
         END IF

C        Begin Receptor LOOP
         DO IREC = 1, NUMREC

C           Calculate DIST From Source to Receptor
            X = AXR(IREC) - XSRC
            Y = AYR(IREC) - YSRC
            DIST = DSQRT (X*X + Y*Y) - XRAD

            IF (DIST .LT. 0.99D0) THEN
C              Receptor Is Too Close To Source
               INC = INC + 1
               IF (MOD((INC-1), 40) .EQ. 0) THEN
                  CALL HEADER(IOUNIT)
                  WRITE(IOUNIT,9002)
               END IF
               WRITE(IOUNIT,9003) SRCID(ISRC), AXR(IREC),
     &                            AYR(IREC), DIST

            ELSE IF (DIST .GT. MAXDIST) THEN
C              Receptor Is Too Far From Source
               INC = INC + 1
               IF (MOD((INC-1), 40) .EQ. 0) THEN
                  CALL HEADER(IOUNIT)
                  WRITE(IOUNIT,9002)
               END IF
               WRITE(IOUNIT,9003) SRCID(ISRC), AXR(IREC),
     &                            AYR(IREC), DIST

            ELSE IF (SRCTYP(ISRC) .EQ. 'OPENPIT') THEN
C              Check for receptors within boundary of an open pit source
               XR = AXR(IREC)
               YR = AYR(IREC)
               CALL PNPOLY(XR,YR,XVM,YVM,4,INOUT)
               IF (INOUT .GT. 0) THEN
C                 Receptor is within boundary
                  INC = INC + 1
                  IF (MOD((INC-1), 40) .EQ. 0) THEN
                     CALL HEADER(IOUNIT)
                     WRITE(IOUNIT,9002)
                  END IF
                  WRITE(IOUNIT,9004) SRCID(ISRC), AXR(IREC),
     &                               AYR(IREC)
               END IF
            END IF

         END DO
C        End Receptor LOOP

      END DO
C     End Source LOOP

 9002 FORMAT(/22X,
     & '* SOURCE-RECEPTOR COMBINATIONS FOR WHICH CALCULATIONS ',
     & 'MAY NOT BE PERFORMED *'/24X,'LESS THAN 1.0 METER;',
c --- PRIME ---------------------------------------------------------
     & ' WITHIN OPENPIT; OR BEYOND 80KM FOR FASTAREA/FASTALL',//
c -------------------------------------------------------------------
     & /30X,'SOURCE',10X,'- - RECEPTOR LOCATION - -',9X,'DISTANCE',
     & /30X,'  ID  ',10X,'XR (METERS)   YR (METERS)',9X,'(METERS)',
     & /28X,31('- ')/)
 9003 FORMAT(29X,A12,3X,F13.1,1X,F13.1,5X,F12.2)
 9004 FORMAT(29X,A12,3X,F13.1,1X,F13.1,5X,'     OPENPIT')

      RETURN
      END

      SUBROUTINE PRTMET(IOUNT)
C***********************************************************************
C                 PRTMET Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Print Out The Input Met Data Summary and Source Groups
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To include output file unit argument to support
C                    output to main 'aermod.out' file and to the
C                    optional SUMMFILE.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
C
C        MODIFIED:   To output 4-digit start year and end year for
C                    Y2K compliance.
C                    R.W. Brode, PES, Inc., 5/12/99
C
C        INPUTS:  Model Options and Keyword Summarys
C
C        OUTPUTS: Printed Model Outputs
C
C        CALLED FROM:   INPSUM
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, IOUNT

C     Variable Initializations
      MODNAM = 'PRTMET'

C     Start New Page and Print The Titles
      CALL HEADER(IOUNT)

C     Print The Meteorology Data Date Array;
C     use IPROC if ISYR is not Leap Year; or
C     use IPROCL if ISYR is a Leap Year
      IF( L_LeapYear )THEN
         WRITE(IOUNT,9037) (IPROCL(I),I = 1, 366)
      ELSE
         WRITE(IOUNT,9037) (IPROC(I),I = 1, 365)
      ENDIF

      IF (ISDATE .NE. 0 .OR. IEDATE .NE. 2147123124) THEN
C        Write Out User-specified Start and End Dates
         WRITE(IOUNT,9038) ISYR, ISMN, ISDY, ISHR,
     &                     IEYR, IEMN, IEDY, IEHR
      END IF

      WRITE(IOUNT,9039)

C     Print the upper bound of the first 5 wind speed categories
      WRITE ( IOUNT, 9001 ) (UCAT(I), I=1,5)

 9001 FORMAT(//34X,'*** UPPER BOUND OF FIRST THROUGH FIFTH WIND SPEED',
     &       ' CATEGORIES ***'/60X,'(METERS/SEC)'//46X,5(F7.2,','))
 9037 FORMAT(/44X,'*** METEOROLOGICAL DAYS SELECTED FOR PROCESSING ***'
     &       /63X,'(1=YES; 0=NO)'//8(11X,5(10I2,2X)/))
 9038 FORMAT(/23X,'METEOROLOGICAL DATA PROCESSED BETWEEN START DATE: ',
     &       I4,1X,3I3,/59X,'AND END DATE: ',I4,1X,3I3)
 9039 FORMAT(/16X,'NOTE:  METEOROLOGICAL DATA ACTUALLY PROCESSED WILL',
     &       ' ALSO DEPEND ON WHAT IS INCLUDED IN THE DATA FILE.'/)

      RETURN
      END

      SUBROUTINE RSINIT
C***********************************************************************
C                 RSINIT Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: To Initialize Results Variables for Restart
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   Use local variable, IRSDATE, to read 10-digit start
C                    date from re-start file.  Include checks on restart
C                    date being ealier than user-specified start date
C                    on STARTEND keyword, with fatal error message for
C                    INITFILE restarts and warning for MULTYEAR restarts.
C                    Also check for overlapping periods for MULTYEAR
C                    applications with start date from STARTEND keyword
C                    being earlier than start date from the MULTYEAR
C                    re-start file.  This condition results in a fatal
C                    error message.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
C
C        MODIFIED:   Added arrays associated with post-1997 PM10
C                    processing.
C                    R.W. Brode, PES, Inc.,  5/12/99
C
C        MODIFIED:   Changed parameter for specifying the number of
C                    high annual/period averages from NVAL to NHIANN.
C                    R.W. Brode, PES, Inc.,  4/3/98
C
C        INPUTS:  None
C
C        OUTPUTS: Initialized Variables
C
C        CALLED FROM:   MAIN
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, J, K, L, M
C     JAT D065 8/9/21
C     JSYR, JSMN, JSDY, AND JSHR NOT USED
C      INTEGER :: JSYR, JSMN, JSDY, JSHR
      INTEGER :: IRSDATE

C JAT 06/22/21 D065
C REMOVE IDYMAX AS UNUSED VARIABLE; ALSO REMOVE ITS DATA initialization
C      INTEGER IDYMAX(12)

C     Variable Initializations
C      DATA IDYMAX/31,29,31,30,31,30,31,31,30,31,30,31/
      MODNAM = 'RSINIT'

C --- Read "start date" from restart file, IRSDATE, based on last
C     hour processed in SAVEFILE; IRSDATE is full 10-digit value
C     based on 4-digit year
      READ(IRSUNT,ERR=99,END=999) IRSDATE, NTOTHRS

C --- Adjust IRSDATE by adding 1 hour since IRSDATE will be used as the
C     first hour of data to process in the restarted run for INITFILE
C     JAT D065 8/9/21
C     JSYR, JSMN, JSDY, AND JSHR NOT USED
C      JSYR = IRSDATE/1000000
C      JSMN = (IRSDATE/10000) - (IRSDATE/1000000)*100
C      JSDY = (IRSDATE/100) - (IRSDATE/10000)*100
C      JSHR = IRSDATE - (IRSDATE/100)*100


C --- Compare "start date" from restart file, IRSDATE, to "start date"
C     from STARTEND keyword (IMSTAT(6)=1), ISDATE.
C     If IRSDATE < ISDATE, then issue fatal error message for INITFILE
C     restarts and warning message for MULTYEAR restarts;
C     IF IRSDATE > ISDATE for MULTYEAR restarts, then issue fatal error
C     message since this implies overlapping data periods.
      IF (.NOT.MULTYR .AND. IMSTAT(6) .EQ. 1 .AND.
     &                                       IRSDATE .LT. ISDATE) THEN
C        Re-start date is less than start date based on STARTEND keyword
         CALL ERRHDL(PATH,MODNAM,'E','484','STARTEND')
         RUNERR = .TRUE.
         GO TO 1000
      ELSE IF (MULTYR .AND. IMSTAT(6) .EQ. 1 .AND.
     &                                       IRSDATE .LT. ISDATE) THEN
         CALL ERRHDL(PATH,MODNAM,'W','485','STARTEND')

      ELSE IF (MULTYR .AND. IMSTAT(6) .EQ. 1 .AND.
     &                                       IRSDATE .GT. ISDATE) THEN
         WRITE(DUMMY,'(I10.10)') IRSDATE - (IRSDATE/100000000)*100000000
         CALL ERRHDL(PATH,MODNAM,'E','486',DUMMY)
         RUNERR = .TRUE.
         GO TO 1000

      ELSE
C        Assign IRSDATE to ISDATE as start date for data processing
         ISDATE = IRSDATE
      END IF

      READ(IRSUNT,ERR=99,END=999) NHIVAL, NMXVAL, NUMREC, NUMGRP,
     &                            NUMAVE, NUMTYP

      IF (NHIVAL .GT. 0) THEN
         READ(IRSUNT,ERR=99,END=999) (((((HIVALU(I,J,K,L,M),I=1,NUMREC),
     &                   J=1,NHIVAL),K=1,NUMGRP),L=1,NUMAVE),M=1,NUMTYP)
         READ(IRSUNT,ERR=99,END=999) (((((NHIDAT(I,J,K,L,M),I=1,NUMREC),
     &                   J=1,NHIVAL),K=1,NUMGRP),L=1,NUMAVE),M=1,NUMTYP)
         READ(IRSUNT,ERR=99,END=999) (((((HCLMSG(I,J,K,L,M),I=1,NUMREC),
     &                   J=1,NHIVAL),K=1,NUMGRP),L=1,NUMAVE),M=1,NUMTYP)

C ---    Include arrays associated with multi-year processing of high
C        ranked values for 24-hr PM2.5, 1-hr NO2, and 1-hr SO2 NAAQS
         IF (PM25AVE .OR. NO2AVE .OR. SO2AVE) THEN
            READ(IRSUNT,ERR=99,END=999) NUMYRS
            READ(IRSUNT,ERR=99,END=999) (((SUMHNH(I,J,K),I=1,NUMREC),
     &                                       J=1,NUMGRP),K=1,NHIVAL)
            READ(IRSUNT,ERR=99,END=999) (((HIMXDLY(I,J,K),I=1,NUMREC),
     &                                       J=1,NUMGRP),K=1,NHIVAL)
            READ(IRSUNT,ERR=99,END=999) (((NHIDATMXD(I,J,K),I=1,NUMREC),
     &                                       J=1,NUMGRP),K=1,NHIVAL)
            READ(IRSUNT,ERR=99,END=999) ((((HIMXDLY_BYYR(I,J,K,L),
     &                   I=1,NUMREC),J=1,NUMGRP),K=1,NHIVAL),L=1,NUMYRS)
            READ(IRSUNT,ERR=99,END=999) ((((NHIDATMXD_BYYR(I,J,K,L),
     &                   I=1,NUMREC),J=1,NUMGRP),K=1,NHIVAL),L=1,NUMYRS)
         END IF

      END IF

      IF (NMXVAL .GT. 0) THEN
         READ(IRSUNT,ERR=99,END=999) ((((RMXVAL(I,J,K,L),I=1,NMXVAL),
     &                               J=1,NUMGRP),K=1,NUMAVE),L=1,NUMTYP)
         READ(IRSUNT,ERR=99,END=999) ((((MXDATE(I,J,K,L),I=1,NMXVAL),
     &                               J=1,NUMGRP),K=1,NUMAVE),L=1,NUMTYP)
         READ(IRSUNT,ERR=99,END=999) ((((MXLOCA(I,J,K,L),I=1,NMXVAL),
     &                               J=1,NUMGRP),K=1,NUMAVE),L=1,NUMTYP)
         READ(IRSUNT,ERR=99,END=999) ((((MCLMSG(I,J,K,L),I=1,NMXVAL),
     &                               J=1,NUMGRP),K=1,NUMAVE),L=1,NUMTYP)
      END IF

      IF (SEASONHR) THEN
C        Initialize the SEASON by HOUR-OF-DAY Arrays
         READ(IRSUNT,ERR=99,END=999) (((((SHVALS(I,J,K,L,M),I=1,NUMREC),
     &                           J=1,NUMGRP),K=1,4),L=1,24),M=1,NUMTYP)
         READ(IRSUNT,ERR=99,END=999) ((NSEAHR(I,J),I=1,4),J=1,24)
         READ(IRSUNT,ERR=99,END=999) ((NSEACM(I,J),I=1,4),J=1,24)
      END IF

      IF (PERIOD) THEN
         READ(IRSUNT,ERR=99,END=999) IANHRS, IANCLM, IANMSG, NUMYRS
         READ(IRSUNT,ERR=99,END=999) (((ANNVAL(I,J,K),I=1,NUMREC),
     &                                    J=1,NUMGRP),K=1,NUMTYP)
         IF (MULTYR) THEN
C           Reinitialize the ANNVAL(NUMREC,NUMGRP,NUMTYP) Array and Annual Counters
            ANNVAL = 0.0D0
            IANHRS = 0
            IANCLM = 0
            IANMSG = 0
C           Read the Maximum Annual Values
            READ(IRSUNT,ERR=99,END=999) (((AMXVAL(I,J,K),I=1,NHIANN),
     &                                       J=1,NUMGRP),K=1,NUMTYP)
            READ(IRSUNT,ERR=99,END=999) (((IMXLOC(I,J,K),I=1,NHIANN),
     &                                       J=1,NUMGRP),K=1,NUMTYP)
         END IF
      ELSE IF (ANNUAL) THEN
         READ(IRSUNT,ERR=99,END=999) IANHRS, IANCLM, IANMSG, NUMYRS
         READ(IRSUNT,ERR=99,END=999) (((ANNVAL(I,J,K),I=1,NUMREC),
     &                                    J=1,NUMGRP),K=1,NUMTYP)
         READ(IRSUNT,ERR=99,END=999) (((SUMANN(I,J,K),I=1,NUMREC),
     &                                    J=1,NUMGRP),K=1,NUMTYP)
      END IF

      GO TO 1000

C     WRITE Error Message:  Error Reading INITFILE
 99   DUMMY = 'INITFILE'
      CALL ERRHDL(PATH,MODNAM,'E','510',DUMMY)
      RUNERR = .TRUE.
      GO TO 1000

C     WRITE Error Message:  End of File Reached for INITFILE
 999  DUMMY = 'INITFILE'
      CALL ERRHDL(PATH,MODNAM,'E','580',DUMMY)
      RUNERR = .TRUE.

 1000 RETURN
      END

      SUBROUTINE RESINI
C***********************************************************************
C                 RESINI Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: To Initialize Results Variables With Zeroes
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   Replaced DO-loops with array assignment statements,
C                    and checked for allocation status of allocatable
C                    arrays.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
C
C        MODIFIED:   Added results arrays for post-1997 PM10 processing
C                    option.  Also replaced labeled DO loop terminators
C                    with unlabeled END DO statements.
C                    R.W. Brode, PES, Inc.,  11/19/98
C
C        MODIFIED:   Changed parameter for specifying the number of
C                    high annual/period averages from NVAL to NHIANN.
C                    R.W. Brode, PES, Inc.,  4/3/98
C
C        INPUTS:  None
C
C        OUTPUTS: Initialized Variables
C
C        CALLED FROM:   MAIN
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'RESINI'

C     Initialize the Results Arrays
      NUMHRS(:) = 0
      NUMCLM(:) = 0
      NUMMSG(:) = 0
      IF (ALLOCATED(HRVAL))  HRVAL  = 0.0D0
      IF (ALLOCATED(AVEVAL)) AVEVAL = 0.0D0
      IF (ALLOCATED(HIVALU)) HIVALU = 0.0D0
      IF (ALLOCATED(NHIDAT)) NHIDAT = 0
      IF (ALLOCATED(HCLMSG)) HCLMSG = ' '
      IF (ALLOCATED(HMAX))   HMAX   = 0.0D0
      IF (ALLOCATED(HMDATE)) HMDATE = 0
      IF (ALLOCATED(HMLOC))  HMLOC  = 0
      IF (ALLOCATED(HMCLM))  HMCLM  = ' '
      IF (ALLOCATED(RMXVAL)) RMXVAL = 0.0D0
      IF (ALLOCATED(MXDATE)) MXDATE = 0
      IF (ALLOCATED(MXLOCA)) MXLOCA = 0
      IF (ALLOCATED(MCLMSG)) MCLMSG = ' '
      IF (ALLOCATED(BACKAVE)) BACKAVE(:) = 0.0D0
      IF (ALLOCATED(BACKANN)) BACKANN(:) = 0.0D0
      IF (ALLOCATED(BACKSEASHR)) BACKSEASHR(:,:,:) = 0.0D0
      IF (ALLOCATED(BACKHR))  BACKHR(:,:) = 0.0D0

      IANHRS = 0
      IANCLM = 0
      IANMSG = 0

C     The following were added as part of implementing the SCIM option
      NSKIPTOT = 0

C     Initialize results arrays for ANNUAL/PERIOD processing; if allocated
      IF (ALLOCATED(ANNVAL))  ANNVAL = 0.0D0
      IF (ALLOCATED(SUMANN))  SUMANN = 0.0D0
      IF (ALLOCATED(AMXVAL))  AMXVAL = 0.0D0
      IF (ALLOCATED(IMXLOC))  IMXLOC = 0

C     Initialize results array for PM-2.5 processing; if allocated
      IF (ALLOCATED(SUMHNH))  SUMHNH  = 0.0D0
      IF (ALLOCATED(MXPMVAL)) MXPMVAL = 0.0D0
      IF (ALLOCATED(MXPMLOC)) MXPMLOC = 0

C     Initialize results array for SEASONHR option; check for allocated first
      IF (ALLOCATED(SHVALS)) SHVALS = 0.0D0
C     Initialize results array for MAXDAILY option; check for allocated first
      IF (ALLOCATED(MXDVAL))  MXDVAL  = 0.0D0
      IF (ALLOCATED(HIMXDLY)) HIMXDLY = 0.0D0
      IF (ALLOCATED(HIMXDLY_BYYR)) HIMXDLY_BYYR = 0.0D0
      IF (ALLOCATED(IMXDHR))  IMXDHR  = 0
      IF (ALLOCATED(NHIDATMXD)) NHIDATMXD = 0
      IF (ALLOCATED(NHIDATMXD_BYYR)) NHIDATMXD_BYYR = 0

      NSEAHR = 0
      NSEACM = 0

      RETURN
      END
