      SUBROUTINE PERAVE
C***********************************************************************
C                 PERAVE Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Calculates PERIOD Averages
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   Removed 75 percent limit on calculation of the
C                    denominator, SNUM - 4/19/93
C
C        INPUTS:  Array of Period Sums and Counters
C
C        OUTPUTS: Array of Period Averages
C
C        CALLED FROM:   MAIN
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      DOUBLE PRECISION :: SNUM, SNUMSCM, STOTHRS

C     Variable Initializations
      MODNAM = 'PERAVE'

C     Calculate Denominator Considering Calms and Missing
      SNUM = DBLE(IANHRS - IANCLM - IANMSG)
      IF (.NOT. SCIM) THEN
         STOTHRS = DBLE(IANHRS)
         SNUMSCM = SNUM
      ELSE IF (SCIM) THEN
         STOTHRS = DBLE(NSKIPTOT)                       ! Total no. of hours
         SNUMSCM = DBLE(IANHRS   - IANCLM   - IANMSG)   ! Sampled SCIM'd hours
      ENDIF

C     Calculate Period Average Concentrations for Each Source Group and Receptor

C     Begin LOOP Over Output Types
      DO ITYP = 1, NUMTYP

         IF (OUTTYP(ITYP) .EQ. 'CONC') THEN

            ANNVAL(1:NUMREC,1:NUMGRP,1) = 
     &      ANNVAL(1:NUMREC,1:NUMGRP,1)/SNUM
        
         ELSE IF (SCIM) THEN
         
            ANNVAL(1:NUMREC,1:NUMGRP,ITYP) = 
     &      ANNVAL(1:NUMREC,1:NUMGRP,ITYP)*(STOTHRS/SNUMSCM)

         END IF

      END DO
C     End LOOP Over Output Types

      RETURN
      END

      SUBROUTINE SHAVE
C***********************************************************************
C                 SHAVE Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Calculates Season/Hour Averages
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    June 5, 1997
C
C        INPUTS:  Array of Season/Hour Sums and Counters
C
C        OUTPUTS: Season/Hour Output Files
C
C        CALLED FROM:   MAIN
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      DOUBLE PRECISION :: SNUM

C     Variable Initializations
      MODNAM = 'SHAVE'

C     Calculate Period Average Concentrations for Each Source Group and Receptor

      DO ISEAS = 1, 4
         DO IHOUR = 1, 24

C           Calculate Denominator Considering Calms and Missing
            SNUM = DBLE(NSEAHR(ISEAS,IHOUR) - NSEACM(ISEAS,IHOUR))

            SHVALS(1:NUMREC,1:NUMGRP,ISEAS,IHOUR,1) = 
     &      SHVALS(1:NUMREC,1:NUMGRP,ISEAS,IHOUR,1) / SNUM

         END DO
      END DO

      RETURN
      END

      SUBROUTINE HIPER
C***********************************************************************
C                 HIPER Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Selects Highest PERIOD Average Values
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   Moved call to RSDUMP for MULTYR option to MAIN.
C                    R.W. Brode, MACTEC (f/k/a PES), Inc.,  09/06/05
C
C        MODIFIED:   Changed parameter for specifying the number of
C                    high annual/period averages from NVAL to NHIANN.
C                    R.W. Brode, PES, Inc.,  4/3/98
C
C        INPUTS:  Array of Period Averages
C
C        OUTPUTS: Array of Highest Period Averages By Source Group
C
C        CALLED FROM:   MAIN
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: J

C     Variable Initializations
      MODNAM = 'HIPER'

C     Begin Source Group LOOP
      DO IGRP = 1, NUMGRP
C        Begin Receptor LOOP
         RECEPTOR_LOOP: DO IREC = 1, NUMREC
            IF (NHIANN .GT. 1) THEN
               IF (ANNVAL(IREC,IGRP,ITYP) .GT.
     &             AMXVAL(NHIANN,IGRP,ITYP)) THEN
                  DO J = NHIANN-1, 1, -1
                     IF (ANNVAL(IREC,IGRP,ITYP) .LE.
     &                      AMXVAL(J,IGRP,ITYP))THEN
                        AMXVAL(J+1,IGRP,ITYP) = ANNVAL(IREC,IGRP,ITYP)
                        IMXLOC(J+1,IGRP,ITYP) = IREC
C                       Exit Block
                        CYCLE RECEPTOR_LOOP
                     ELSE
                        AMXVAL(J+1,IGRP,ITYP) = AMXVAL(J,IGRP,ITYP)
                        IMXLOC(J+1,IGRP,ITYP) = IMXLOC(J,IGRP,ITYP)
                        IF (J .EQ. 1) THEN
                           AMXVAL(1,IGRP,ITYP) = ANNVAL(IREC,IGRP,ITYP)
                           IMXLOC(1,IGRP,ITYP) = IREC
                        END IF
                     END IF
                  END DO
               END IF
            ELSE IF (NHIANN .EQ. 1) THEN
               IF (ANNVAL(IREC,IGRP,ITYP) .GT. AMXVAL(1,IGRP,ITYP)) THEN
                  AMXVAL(1,IGRP,ITYP) = ANNVAL(IREC,IGRP,ITYP)
                  IMXLOC(1,IGRP,ITYP) = IREC
               END IF
            END IF
         END DO RECEPTOR_LOOP
C        End Receptor LOOP
      END DO
C     End Source Group LOOP

      RETURN
      END

      SUBROUTINE PSTANN
C***********************************************************************
C                 PSTANN Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Process Postprocessor Files for PERIOD Results
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   
C                    Modified to properly handle PERIOD vs. ANNUAL
C                    POSTFILEs.  PERIOD postfiles include averages
C                    across the full data period; ANNUAL postfiles
C                    include the average ANNUAL values for each year
C                    in the data period.
C
C                    The PERIOD postfiles include the number of
C                    hours in the data period; ANNUAL postfiles 
C                    include the number of years in the data period.
C                    R. W. Brode, U.S. EPA, 06/30/2015
C
C                    Increased length of HDRFRM variable to avoid
C                    potential problems with portability of code.
C                    R. Brode, MACTEC/PES, 10/17/2005
C
C        MODIFIED BY D. Strimaitis, SRC (for Wet & Dry DEPOSITION) - 11/8/93
C
C        INPUTS:  Array of High Values
C
C        OUTPUTS: File of High Values for Postprocessing
C
C        CALLED FROM:   MAIN
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C Unused:      INTEGER :: I
      CHARACTER PERCHR*6, HDRFRM*400

C     Variable Initializations
      MODNAM = 'PSTANN'

C     Set Averaging Label and Create Header Format for Columns
      IF (PERIOD) THEN
         PERCHR = 'PERIOD'
         WRITE(HDRFRM,9020) NUMTYP, NUMTYP+2
      ELSE IF (ANNUAL) THEN
         PERCHR = 'ANNUAL'
         WRITE(HDRFRM,9021) NUMTYP, NUMTYP+2
      END IF

C     Begin Source Group LOOP
      DO IGRP = 1, NUMGRP
C        Check for Selection of PERIOD POSTFILE for This Group
         IF (IANPST(IGRP) .EQ. 1) THEN
            IF (IANFRM(IGRP) .EQ. 0) THEN
C              WRITE Results to Unformatted POSTFILE
               IF (PERIOD) THEN
                  WRITE(IAPUNT(IGRP),ERR=99) KURDAT, IANHRS,
     &            GRPID(IGRP), ((ANNVAL(IREC,IGRP,ITYP),IREC=1,NUMREC),
     &                           ITYP=1,NUMTYP)
               ELSE IF (ANNUAL) THEN
                  WRITE(IAPUNT(IGRP),ERR=99) KURDAT, NUMYRS,
     &            GRPID(IGRP), ((ANNVAL(IREC,IGRP,ITYP),IREC=1,NUMREC),
     &                           ITYP=1,NUMTYP)
               END IF
            ELSE
C              WRITE Results to Formatted Plot File
C              Write Header Information
               WRITE(IAPUNT(IGRP),9005) VERSN, TITLE1(1:68), RUNDAT
               WRITE(IAPUNT(IGRP),9007) C_METVER, RUNTIM, 
     &                          MODOPS_String(1:LEN_TRIM(MODOPS_String))
               IF (PERIOD) THEN
                  WRITE(IAPUNT(IGRP),9010) PERCHR,GRPID(IGRP),
     &                                     NUMREC,PSTFRM
                  WRITE(IAPUNT(IGRP),HDRFRM) (CHIDEP(1,ITYP),
     &                                        CHIDEP(2,ITYP),
     &                                        CHIDEP(3,ITYP),
     &                                                 ITYP=1,NUMTYP)
               ELSE IF (ANNUAL) THEN
                  WRITE(IAPUNT(IGRP),9011) PERCHR,NUMYRS,GRPID(IGRP),
     &                                     NUMREC,PSTFRM
                  WRITE(IAPUNT(IGRP),HDRFRM) (CHIDEP(1,ITYP),
     &                                        CHIDEP(2,ITYP),
     &                                        CHIDEP(3,ITYP),
     &                                                 ITYP=1,NUMTYP)
               END IF
C              Begin Receptor LOOP
               DO IREC = 1, NUMREC
                  IF (PERIOD) THEN
                     WRITE(IAPUNT(IGRP),PSTFRM,ERR=99)
     &                  AXR(IREC), AYR(IREC), (ANNVAL(IREC,IGRP,ITYP),
     &                                         ITYP=1,NUMTYP),
     &                  AZELEV(IREC), AZHILL(IREC), AZFLAG(IREC),
     &                  PERCHR, GRPID(IGRP), IANHRS, NETID(IREC)
                  ELSE IF (ANNUAL) THEN
                     WRITE(IAPUNT(IGRP),PSTFRM,ERR=99)
     &                  AXR(IREC), AYR(IREC), (ANNVAL(IREC,IGRP,ITYP),
     &                                         ITYP=1,NUMTYP),
     &                  AZELEV(IREC), AZHILL(IREC), AZFLAG(IREC),
     &                  PERCHR, GRPID(IGRP), NUMYRS, NETID(IREC)
                  END IF
               END DO
C              End Receptor LOOP
            END IF
         END IF
      END DO
C     End Source Group LOOP

      GO TO 999

C     WRITE Error Message for Problem Writing to Postprocessor File
 99   WRITE(DUMMY,'("PSTFL",I3.3)') IAPUNT(IGRP)
      CALL ERRHDL(PATH,MODNAM,'E','520',DUMMY)

 9005 FORMAT('* AERMOD (',A6,'): ',A68,5X,A8)
 9007    FORMAT('* AERMET (',A6,'):',T93,A8,
     &      /'* MODELING OPTIONS USED: ',A:)
 9010 FORMAT('*',9X,'POST/PLOT FILE OF ',A6,' VALUES FOR ',
     &       'SOURCE GROUP: ',A8,
     &      /'*',9X,'FOR A TOTAL OF ', I5,' RECEPTORS.',
     &      /'*',9X,'FORMAT: ',A:)
 9011 FORMAT('*',9X,'POST/PLOT FILE OF ',A6,' VALUES FOR YEAR ',
     &       I3,' FOR SOURCE GROUP: ',A8,
     &      /'*',9X,'FOR A TOTAL OF ', I5,' RECEPTORS.',
     &      /'*',9X,'FORMAT: ',A:)
 9020 FORMAT('(''*'',8X,''X'',13X,''Y'',4X,',I1,'(2X,3A4),4X,''ZELEV'',
     &  4X,''ZHILL'',4X,''ZFLAG'',4X,''AVE'',5X,''GRP'',6X,''NUM HRS'',
     &  3X,''NET ID'',/,''*'',',I1,'(1X,''____________ ''),1X,
     & 3('' ______  ''),''______  ________  ________  ________'')')
C --- Modified to show the Year Number for ANNUAL averages, instead of
C     number of years, in v15181
 9021 FORMAT('(''*'',8X,''X'',13X,''Y'',4X,',I1,'(2X,3A4),4X,''ZELEV'',
     &  4X,''ZHILL'',4X,''ZFLAG'',4X,''AVE'',5X,''GRP'',5X,''YEAR NUM'',
     &  3X,''NET ID'',/,''*'',',I1,'(1X,''____________ ''),1X,
     & 3('' ______  ''),''______  ________  ________  ________'')')

 999  RETURN
      END

      SUBROUTINE PLTANN
C***********************************************************************
C                 PLTANN Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Process Files To Plot Annual (i.e. PERIOD) Results
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   
C                    Modified to properly handle PERIOD vs. ANNUAL
C                    PLOTFILEs.  PERIOD plotfiles include averages
C                    across the full data period; ANNUAL plotfiles
C                    include the multi-year average ANNUAL values.
C                    The PERIOD plotfiles include the number of
C                    hours in the data period; ANNUAL plotfiles 
C                    include the number of years in the data period.
C                    R. W. Brode, U.S. EPA, 06/30/2015
C
C                    Increased length of HDRFRM variable to avoid
C                    potential problems with portability of code.
C                    R. Brode, MACTEC/PES, 10/17/2005
C
C        MODIFIED BY D. Strimaitis, SRC (for Wet & Dry DEPOSITION) - 11/8/93
C
C        INPUTS:  Array of High Values
C
C        OUTPUTS: File of High Values for Plotting
C
C        CALLED FROM:   MAIN
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C Unused:      INTEGER :: I
      CHARACTER PERCHR*6, HDRFRM*400

C     Variable Initializations
      MODNAM = 'PLTANN'

C     Set Averaging Label and Create Header Format for Columns
      IF (PERIOD) THEN
         PERCHR = 'PERIOD'
         WRITE(HDRFRM,9020) NUMTYP, NUMTYP+2
      ELSE IF (ANNUAL) THEN
         PERCHR = 'ANNUAL'
         WRITE(HDRFRM,9021) NUMTYP, NUMTYP+2
      END IF

C     Begin Source Group LOOP
      DO IGRP = 1, NUMGRP
C        Check for Selection of PERIOD PLOTFILE for This Group
         IF (IANPLT(IGRP) .EQ. 1) THEN
C           Write Header Information
            WRITE(IPPUNT(IGRP),9005) VERSN, TITLE1(1:68), RUNDAT
            WRITE(IPPUNT(IGRP),9007) C_METVER, RUNTIM, 
     &                          MODOPS_String(1:LEN_TRIM(MODOPS_String))
            WRITE(IPPUNT(IGRP),9011) PERCHR,NUMYRS,GRPID(IGRP),
     &                                     NUMREC,PSTFRM
            WRITE(IPPUNT(IGRP),HDRFRM) (CHIDEP(1,ITYP),CHIDEP(2,ITYP),
     &                                  CHIDEP(3,ITYP),ITYP=1,NUMTYP)
C           Begin Receptor LOOP
            DO IREC = 1, NUMREC
               IF (PERIOD) THEN
                  WRITE(IPPUNT(IGRP),PSTFRM,ERR=99)
     &            AXR(IREC), AYR(IREC), (ANNVAL(IREC,IGRP,ITYP),
     &            ITYP=1,NUMTYP), AZELEV(IREC), AZHILL(IREC), 
     &            AZFLAG(IREC), PERCHR,GRPID(IGRP), IANHRS, NETID(IREC)
               ELSE IF (ANNUAL) THEN
                  WRITE(IPPUNT(IGRP),PSTFRM,ERR=99)
     &            AXR(IREC), AYR(IREC), (ANNVAL(IREC,IGRP,ITYP),
     &            ITYP=1,NUMTYP), AZELEV(IREC), AZHILL(IREC), 
     &            AZFLAG(IREC), PERCHR,GRPID(IGRP), NUMYRS, NETID(IREC)
               END IF
            END DO
C           End Receptor LOOP
         END IF
      END DO
C     End Source Group LOOP

      GO TO 999

C     WRITE Error Message for Problem Writing to Plot File
 99   WRITE(DUMMY,'("PLTFL",I3.3)') IPPUNT(IGRP)
      CALL ERRHDL(PATH,MODNAM,'E','520',DUMMY)

 9005 FORMAT('* AERMOD (',A6,'): ',A68,5X,A8)
 9007 FORMAT('* AERMET (',A6,'):',T93,A8,
     &      /'* MODELING OPTIONS USED: ',A:)
C Unused: 9010 FORMAT('*',9X,'PLOT FILE OF ',A6,' VALUES FOR ',
C     &       'SOURCE GROUP: ',A8,
C     &      /'*',9X,'FOR A TOTAL OF ', I5,' RECEPTORS.',
C     &      /'*',9X,'FORMAT: ',A:)
 9011 FORMAT('*',9X,'PLOT FILE OF ',A6,' VALUES AVERAGED ACROSS ',
     &       I3,' YEARS FOR SOURCE GROUP: ',A8,
     &      /'*',9X,'FOR A TOTAL OF ', I5,' RECEPTORS.',
     &      /'*',9X,'FORMAT: ',A:)
 9020 FORMAT('(''*'',8X,''X'',13X,''Y'',4X,',I1,'(2X,3A4),4X,''ZELEV'',
     &  4X,''ZHILL'',4X,''ZFLAG'',4X,''AVE'',5X,''GRP'',6X,''NUM HRS'',
     &  3X,''NET ID'',/,''*'',',I1,'(1X,''____________ ''),1X,
     & 3('' ______  ''),''______  ________  ________  ________'')')
 9021 FORMAT('(''*'',8X,''X'',13X,''Y'',4X,',I1,'(2X,3A4),4X,''ZELEV'',
     &  4X,''ZHILL'',4X,''ZFLAG'',4X,''AVE'',5X,''GRP'',6X,''NUM YRS'',
     &  3X,''NET ID'',/,''*'',',I1,'(1X,''____________ ''),1X,
     & 3('' ______  ''),''______  ________  ________  ________'')')

 999  RETURN
      END

      SUBROUTINE PLOTFL
C***********************************************************************
C                 PLOTFL Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Process Files To Plot
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   Increased length of HDRFRM variable to avoid
C                    potential problems with portability of code.
C                    R. Brode, MACTEC/PES, 10/17/2005
C
C        MODIFIED:   Corrected to output SUMH4H array for post-1997
C                    PM10 processing.
C                    R.W. Brode, PES, Inc.,  12/2/98
C
C        MODIFIED BY D. Strimaitis, SRC (for Wet & Dry DEPOSITION) - 11/8/93
C
C        INPUTS:  Array of High Values
C
C        OUTPUTS: File of High Values for Plotting
C
C        CALLED FROM:   MAIN
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, J, IVAL, IDEC, IMOD
      CHARACTER NCHR1(10)*3, NCHR2(10)*5, CHRVAL*5, HDRFRM*400

C     Variable Initializations
      DATA (NCHR1(I),I=1,10) /'YR1','YR2','YR3','YR4','YR5',
     &                        'YR6','YR7','YR8','YR9','Y10'/
      DATA (NCHR2(I),I=1,10) /'  1ST','  2ND','  3RD','  4TH','  5TH',
     &                        '  6TH','  7TH','  8TH','  9TH',' 10TH'/
      MODNAM = 'PLOTFL'

C     Create Header Format for Columns
      IF (PM25AVE .OR. NO2AVE .OR. SO2AVE) THEN
         WRITE(HDRFRM,9019) NUMYRS, NUMYRS
      ELSE
         WRITE(HDRFRM,9020) NUMTYP, CHIDEP(3,1), NUMTYP+2
      END IF

C     Begin Averaging Period LOOP
      DO IAVE = 1, NUMAVE
C        Begin Source Group LOOP
         DO IGRP = 1, NUMGRP
C           Begin High Value LOOP
            DO IVAL = 1, NHIVAL
C              Decide if we should go through the processing
               IF (IPLTFL(IVAL,IGRP,IAVE) .EQ. 1) THEN
               
C ---             Assign character label for rank
                  IF (IVAL .LE. 10) THEN
                     CHRVAL = NCHR2(IVAL)
                  ELSE IF (MOD(IVAL,100) .GT. 10 .AND. 
     &                     MOD(IVAL,100) .LT. 20) THEN
                     IDEC = INT(IVAL/10)
                     IMOD = MOD(IVAL,10)
                     WRITE(CHRVAL,'(I2,I1,"TH")') IDEC, IMOD
                  ELSE IF (IVAL .LE. 999) THEN
                     IDEC = INT(IVAL/10)
                     IMOD = MOD(IVAL,10)
                     IF (IMOD .EQ. 0) IMOD = 10
                     WRITE(CHRVAL,'(I2,A3)') IDEC, NCHR2(IMOD)(3:5)
                  END IF

                 IF (.NOT. L_NoHeader(3)) THEN
C                 Write Header Information
                  WRITE(IPLUNT(IVAL,IGRP,IAVE),9005) VERSN,
     &                                               TITLE1(1:68),RUNDAT
                  WRITE(IPLUNT(IVAL,IGRP,IAVE),9007) C_METVER, 
     &                                              TITLE2(1:68),RUNTIM,
     &                          MODOPS_String(1:LEN_TRIM(MODOPS_String))
                  IF (PM25AVE .OR. NO2AVE .OR. SO2AVE) THEN
                     WRITE(IPLUNT(IVAL,IGRP,IAVE),9009) CHRVAL,
     &                       CHRAVE(IAVE), NUMYRS, GRPID(IGRP), NUMREC,
     &                       PLTFRM
                  ELSE
                     WRITE(IPLUNT(IVAL,IGRP,IAVE),9010) CHRVAL,
     &                       CHRAVE(IAVE), GRPID(IGRP), NUMREC, PLTFRM
                  END IF
                  IF (PM25AVE .OR. NO2AVE .OR. SO2AVE) THEN
                     WRITE(IPLUNT(IVAL,IGRP,IAVE),HDRFRM) CHIDEP(1,1),
     &                      CHIDEP(2,1),CHIDEP(3,1),
     &                      (NCHR1(I),NCHR1(I),I=1,NUMYRS)
                  ELSE
                     WRITE(IPLUNT(IVAL,IGRP,IAVE),HDRFRM) 
     &                    (CHIDEP(1,ITYP),CHIDEP(2,ITYP),CHIDEP(3,ITYP),
     &                                                    ITYP=1,NUMTYP)
                  END IF
                 END IF
                  
C                 Begin Receptor LOOP
                  DO IREC = 1, NUMREC
                     IF (PM25AVE .OR. NO2AVE .OR. SO2AVE) THEN
                        WRITE(IPLUNT(IVAL,IGRP,IAVE),PLTFRM,ERR=99)
     &                   AXR(IREC), AYR(IREC), SUMHNH(IREC,IGRP,IVAL),
     &                   AZELEV(IREC),AZHILL(IREC),AZFLAG(IREC),
     &                   CHRAVE(IAVE),GRPID(IGRP),CHRVAL,NETID(IREC), 
     &                  (HIMXDLY_BYYR(IREC,IGRP,IVAL,J),
     &                   NHIDATMXD_BYYR(IREC,IGRP,IVAL,J),J=1,NUMYRS)
                     ELSE
                        WRITE(IPLUNT(IVAL,IGRP,IAVE),PLTFRM,ERR=99)
     &                   AXR(IREC), AYR(IREC), (HIVALU(IREC,IVAL,IGRP,
     &                   IAVE,ITYP),ITYP=1,NUMTYP),
     &                   AZELEV(IREC),AZHILL(IREC),AZFLAG(IREC),
     &                   CHRAVE(IAVE),GRPID(IGRP),CHRVAL,
     &                   NETID(IREC), NHIDAT(IREC,IVAL,IGRP,IAVE,1)
                     END IF
                  END DO
C                 End Receptor LOOP
               END IF
            END DO
C           End High Value LOOP
         END DO
C        End Source Group LOOP
      END DO
C     End Averaging Period LOOP

      GO TO 999

C     WRITE Error Message for Problem Writing to Plot File
 99   WRITE(DUMMY,'("PLTFL",I3.3)') IPLUNT(IVAL,IGRP,IAVE)
      CALL ERRHDL(PATH,MODNAM,'E','520',DUMMY)

 9005 FORMAT('* AERMOD (',A6,'): ',A68,5X,A8)
 9007 FORMAT('* AERMET (',A6,'): ',A68,T93,A8,
     &      /'* MODELING OPTIONS USED: ',A:)
C Unused:  9008 FORMAT('*',9X,'PLOT FILE OF ',A5,'-HIGHEST ',A5,
C     &       ' VALUES AVERAGED OVER ',I3,' YEARS FOR SOURCE GROUP: ',A8,
C     &      /'*',9X,'FOR A TOTAL OF ',I5,' RECEPTORS.',
C     &      /'*',9X,'FORMAT: ',A:)
 9009 FORMAT('*',9X,'PLOT FILE OF ',A5,'-HIGHEST MAX DAILY ',A5,
     &       ' VALUES AVERAGED OVER ',I3,' YEARS FOR SOURCE GROUP: ',A8,
     &      /'*',9X,'FOR A TOTAL OF ',I5,' RECEPTORS.',
     &      /'*',9X,'FORMAT: ',A:)
 9010 FORMAT('*',9X,'PLOT FILE OF  HIGH ',A5,' HIGH ',A5,
     &       ' VALUES FOR SOURCE GROUP: ',A8,
     &      /'*',9X,'FOR A TOTAL OF ',I5,' RECEPTORS.',
     &      /'*',9X,'FORMAT: ',A:)
 9019 FORMAT('(''*'',8X,''X'',13X,''Y'',4X,(2X,3A4),4X,''ZELEV'',4X,
     &''ZHILL'',4X,''ZFLAG'',4X,''AVE'',5X,''GRP'',7X,''RANK'',5X,
     &''NET ID'',1X,',I1,'(2X,''AVER CONC '',A3,''  DATE '',A3),/,''*'',
     &3(1X,''____________ ''),1X,3('' ______  ''),
     &''______  ________  ________  ________'',',
     &I1,'(''  _____________  ________''))')
 9020 FORMAT('(''*'',8X,''X'',13X,''Y'',4X,',I1,'(2X,3A4),4X,''ZELEV'',
     &  4X,''ZHILL'',4X,''ZFLAG'',4X,''AVE'',5X,''GRP'',7X,''RANK'',5X,
     &  ''NET ID'',3X,''DATE(',A4,')'',/,''*'',',I1,'(1X,
     & ''____________ ''),1X,3('' ______  ''),
     & ''______  ________  ________  ________  ________'')')

 999  RETURN
      END

      SUBROUTINE OUTPUT
C***********************************************************************
C                 OUTPUT Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Controls Output of Printed Model Results
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To write out "EV STARTING" and "EV FINSISHED" to
C                    temporary event file if no RECTABLE card is used.
C                    R. Brode, PES, Inc. - 02/19/99
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
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'OUTPUT'
      PATH = 'OU'

      IF (PERIOD .OR. ANNUAL) THEN
         DO ITYP = 1, NUMTYP
C           Print Out Summary of Period Averages            ---   CALL PRTANN
            CALL PRTANN
         END DO
      END IF

      IF (PM25AVE .AND. NUMAVE.EQ.1) THEN
C        Print Out Table of Average High-N-High Values for PM2.5 
         DO ITYP = 1, NUMTYP
            CALL PRTPM25
         END DO
         CALL MAXPM25
      ELSE IF (NO2AVE .AND. NUMAVE.GE.1) THEN
C        Print Out Table of Average High-N-High Values for NO2
         DO ITYP = 1, NUMTYP
            CALL PRTPM25
         END DO
         CALL MAXPM25
      ELSE IF (SO2AVE .AND. NUMAVE.GE.1) THEN
C        Print Out Table of Average High-N-High Values for SO2
         DO ITYP = 1, NUMTYP
            CALL PRTPM25
         END DO
         CALL MAXPM25
      ELSE IF (NHIVAL .GT. 0) THEN
         DO ITYP = 1, NUMTYP
C           Print Out Summary of High Values by Receptor    ---   CALL PRTNHI
            CALL PRTNHI
         END DO
      ELSE IF (EVENTS) THEN
C        Write the 'EV STARTING' and 'EV FINISHED' Cards to the Temp-EVent File
         WRITE(ITEVUT,9000)
         WRITE(ITEVUT,9001)
 9000    FORMAT('EV STARTING')
 9001    FORMAT('EV FINISHED')
      END IF

      IF (NMXVAL .GT. 0) THEN
         DO ITYP = 1, NUMTYP
C           Print Out Summary of Overall Maximum Values     ---   CALL PRTMAX
            CALL PRTMAX
         END DO
      END IF

      IF (PERIOD .OR. ANNUAL .OR. NHIVAL .GT. 0) THEN
         DO ITYP = 1, NUMTYP
C ---       Print Out Summary of Results                    ---   CALL PRTSUM
C           Note that summary of short-term results for PM25 24h, NO2/SO2 1h
C           is provided in separate subroutine PRTPM25SUM
            CALL PRTSUM(IOUNIT)
            IF (SUMMFILE) THEN
               CALL PRTSUM(ISUMUNT)
            END IF
         END DO
      END IF

C --- Print out summary of short-term results for PM25 24h, NO2/SO2 1h
      IF (PM25AVE .AND. NHIVAL .GT. 0) THEN
         DO ITYP = 1, NUMTYP
C           Print Out Summary of PM-2.5 Results             ---   CALL PRTPM25SUM
            CALL PRTPM25SUM(IOUNIT)
            IF (SUMMFILE) THEN
               CALL PRTPM25SUM(ISUMUNT)
            END IF
         END DO
      ELSE IF (NO2AVE .AND. NHIVAL .GT. 0) THEN
         DO ITYP = 1, NUMTYP
C           Print Out Summary of NO2 Results                ---   CALL PRTPM25SUM
            CALL PRTPM25SUM(IOUNIT)
            IF (SUMMFILE) THEN
               CALL PRTPM25SUM(ISUMUNT)
            END IF
         END DO
      ELSE IF (SO2AVE .AND. NHIVAL .GT. 0) THEN
         DO ITYP = 1, NUMTYP
C           Print Out Summary of SO2 Results                ---   CALL PRTPM25SUM
            CALL PRTPM25SUM(IOUNIT)
            IF (SUMMFILE) THEN
               CALL PRTPM25SUM(ISUMUNT)
            END IF
         END DO
      END IF

      IF (SEASONHR) THEN
         CALL SHOUT
      END IF

      IF (RKFILE) THEN
         DO ITYP = 1, NUMTYP
C           Write Short Term High Values to Plot File       ---   CALL RANKFL
            CALL RANKFL
         END DO
      END IF

C     Generate The EVENT Input File                         ---   CALL EVEFIL
      IF (EVENTS) THEN
         CALL EVEFIL
      END IF

      RETURN
      END

      SUBROUTINE PRTANN
C***********************************************************************
C                 PRTANN Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Print Out The Annual Average Data
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To include EVALCART receptors with DISCCART
C                    receptors for output.
C                    R. W. Brode, U.S. EPA/OAQPS/AQMG - 10/19/2009
C
C        MODIFIED:   To remove references to BOUNDARY receptors
C                    R. W. Brode, U.S. EPA/OAQPS/AQMG - 12/07/2006
C
C        MODIFIED:   To adjust format statement 9082 for BOUNDARY receptors
C                    to better accommodate UTM coordinates - 9/29/92
C
C        INPUTS:  Arrays of Source Parameters
C                 Arrays of Receptor Locations
C                 Arrays of Model Results
C
C        OUTPUTS: Printed Model Outputs
C
C        CALLED FROM:   OUTPUT
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, J, K, II, NX, NY, INDZ, INDC, INDEXW
      DOUBLE PRECISION :: YCOVAL, XRMS, YRMS, DIST, DIR
      CHARACTER PERCHR*6, BUF132*132

C     Variable Initializations
      MODNAM = 'PRTANN'
      BUF132 = ' '
      INDZ   = 0

      IF (PERIOD) THEN
         PERCHR = 'PERIOD'
      ELSE IF (ANNUAL) THEN
         PERCHR = 'ANNUAL'
      END IF

C     Begin Source Group LOOP
      DO IGRP = 1, NUMGRP

         IF (.NOT. PSDCREDIT) THEN
C           Fill Work Array With SRCIDs For This Group
            INDGRP = 0
            DO ISRC = 1, NUMSRC
               IF (IGROUP(ISRC,IGRP) .EQ. 1) THEN
                  INDGRP = INDGRP + 1
                  WORKID(INDGRP) = SRCID(ISRC)
               END IF
            END DO
         ELSE
C           Assign 'N/A' for source IDs for PSDCREDIT option
            INDGRP = 1
            WORKID(INDGRP) = 'N/A'
         END IF

C ---    Check for BACKGROUND "source" being included 
C        in source group
         IF (GRP_BACK(IGRP)) THEN
            INDGRP = INDGRP + 1
            WORKID(INDGRP) = 'BACKGROUND'
C           Check for More Than 29 Sources Per Group
            INDEXW = MIN(29,NSRC+1)
         ELSE
            INDEXW = MIN(29,NSRC)      
         END IF
C        Check for More Than 29 Sources Per Group
         IF (INDGRP .GT. INDEXW) THEN
            WORKID(INDEXW) = ' . . . '
            INDGRP = INDEXW
         END IF

C        Print Receptor Network Coordinates:
C        Set Number of Columns Per Page, NCPP
         NCPP = 9
C        Set Number of Rows Per Page, NRPP
         NRPP = 40
C        Begin LOOP Through Networks
         DO I = 1, INNET
C           Calculate Number of Pages Per X-Group, NPPX, & Per Y-Group, NPPY
            NPPX = 1 + INT((NUMXPT(I)-1)/NCPP)
            NPPY = 1 + INT((NUMYPT(I)-1)/NRPP)
            DO NX = 1, NPPX
               DO NY = 1, NPPY
                  CALL HEADER(IOUNIT)
                  IF (PERIOD) THEN
                     WRITE(IOUNIT,9032) PERCHR, IANHRS,
     &     (CHIDEP(II,ITYP),II=1,6),GRPID(IGRP),(WORKID(K),K = 1,INDGRP)
                  ELSE IF (ANNUAL) THEN
                     WRITE(IOUNIT,9033) PERCHR,(CHIDEP(II,ITYP),II=1,6),
     &                     NUMYRS,GRPID(IGRP),(WORKID(K),K = 1,INDGRP)
                  END IF
                  WRITE(IOUNIT,9037) NTID(I), NTTYP(I)
C                 Print The Values By Source Group
                  WRITE(IOUNIT,9011) CHIDEP(3,ITYP), POLLUT,PERLBL(ITYP)
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
     &            (ANNVAL(INDZ+J-1,IGRP,ITYP),J=1+NCPP*(NX-1),NUMXPT(I))
                        ELSE
                           WRITE(IOUNIT,9013) YCOVAL,
     &              (ANNVAL(INDZ+J-1,IGRP,ITYP),J=1+NCPP*(NX-1),NCPP*NX)
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
     &            (ANNVAL(INDZ+J-1,IGRP,ITYP),J=1+NCPP*(NX-1),NUMXPT(I))
                        ELSE
                           WRITE(IOUNIT,9013) YCOVAL,
     &              (ANNVAL(INDZ+J-1,IGRP,ITYP),J=1+NCPP*(NX-1),NCPP*NX)
                        END IF
                     END DO
                  END IF
               END DO
            END DO
         END DO
C        End LOOP Through Networks

         IF (IRSTAT(4).NE.0 .OR. IRSTAT(8).NE.0) THEN
C ---       Include EVALCART receptors with DISCCART receptors.
C           Print Out The Coord. & Concentrations For Discrete Cart Receptors
            INDC = 0
            DO IREC = 1, NUMREC
               IF (RECTYP(IREC) .EQ. 'DC') THEN
                  INDC = INDC + 1
                  IF (MOD(INDC-1,80) .EQ. 0) THEN
                     CALL HEADER(IOUNIT)
                     IF (PERIOD) THEN
                      WRITE(IOUNIT,9032) PERCHR,IANHRS,(CHIDEP(II,ITYP),
     &                 II=1,6),GRPID(IGRP),(WORKID(K),K = 1,INDGRP)
                     ELSE IF (ANNUAL) THEN
                      WRITE(IOUNIT,9033) PERCHR,(CHIDEP(II,ITYP),
     &                 II=1,6),NUMYRS,GRPID(IGRP),(WORKID(K),K=1,INDGRP)
                     END IF
                     WRITE(IOUNIT,9043)
                     WRITE(IOUNIT,9011) CHIDEP(3,ITYP), POLLUT,
     &                                  PERLBL(ITYP)
                     WRITE(IOUNIT,9048) CHIDEP(3,ITYP), CHIDEP(3,ITYP)
                  END IF
                  IF (MOD(INDC,2) .NE. 0) THEN
                     WRITE(BUF132(1:60),9045) AXR(IREC), AYR(IREC),
     &                     ANNVAL(IREC,IGRP,ITYP)
                  ELSE
                     WRITE(BUF132(61:120),9045) AXR(IREC), AYR(IREC),
     &                     ANNVAL(IREC,IGRP,ITYP)
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
C           Print Out The Coord. & Concentrations For Discrete Polar Receptors
            INDC = 0
            DO IREC = 1, NUMREC
               IF (RECTYP(IREC) .EQ. 'DP') THEN
                  INDC = INDC + 1
                  XRMS = AXR(IREC) - AXS(IREF(IREC))
                  YRMS = AYR(IREC) - AYS(IREF(IREC))
                  DIST = DSQRT(XRMS*XRMS + YRMS*YRMS)
                  DIR  = DATAN2(XRMS, YRMS) * RTODEG
                  IF (DIR .LE. 0.0D0) DIR = DIR + 360.0D0
                  IF (MOD(INDC-1,80) .EQ. 0) THEN
                     CALL HEADER(IOUNIT)
                     IF (PERIOD) THEN
                      WRITE(IOUNIT,9032) PERCHR,IANHRS,(CHIDEP(II,ITYP),
     &                 II=1,6),GRPID(IGRP),(WORKID(K),K = 1,INDGRP)
                     ELSE IF (ANNUAL) THEN
                      WRITE(IOUNIT,9033) PERCHR,(CHIDEP(II,ITYP),
     &                 II=1,6),NUMYRS,GRPID(IGRP),(WORKID(K),K=1,INDGRP)
                     END IF
                     WRITE(IOUNIT,9044)
                     WRITE(IOUNIT,9011) CHIDEP(3,ITYP), POLLUT,
     &                                  PERLBL(ITYP)
                     WRITE(IOUNIT,9049) CHIDEP(3,ITYP), CHIDEP(3,ITYP)
                  END IF
                  IF (MOD(INDC,2) .NE. 0) THEN
                     WRITE(BUF132(1:65),9047) SRCID(IREF(IREC)), DIST,
     &                                       DIR, ANNVAL(IREC,IGRP,ITYP)
                  ELSE
                     WRITE(BUF132(66:130),9047) SRCID(IREF(IREC)), DIST,
     &                                       DIR, ANNVAL(IREC,IGRP,ITYP)
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

C     End Source Group Loop
      END DO

 9011 FORMAT(/40X,'** ',A4,' OF ',A8,' IN ',A40,' **'/)
 9010 FORMAT(66(' -')/)
 9013 FORMAT(2X,F10.2,1X,'|',1X,9(F13.5))
 9016 FORMAT(3X,' Y-COORD  |',48X,'X-COORD (METERS)')
 9017 FORMAT(3X,' (METERS) |',1X,9(1X,F12.2,:))
 9018 FORMAT(3X,'DIRECTION |',48X,'DISTANCE (METERS)')
 9019 FORMAT(3X,'(DEGREES) |',1X,9(1X,F12.2,:))
 9032 FORMAT(/30X,'*** THE ',A6,' (',I6,' HRS) ',6A4,
     &       'VALUES FOR SOURCE GROUP:',1X,A8,' ***',
     &       /34X,'INCLUDING SOURCE(S):     ',5(A12,', ',:),
     &       /17X,8(A12,', ',:)/17X,8(A12,', ',:)/17X,8(A12,', ',:))
 9033 FORMAT(/19X,'*** THE ',A6,1X,6A4,' VALUES AVERAGED OVER ',
     &       I3,' YEARS FOR SOURCE GROUP:',1X,A8,' ***',
     &       /34X,'INCLUDING SOURCE(S):     ',5(A12,', ',:),
     &       /17X,8(A12,', ',:)/17X,8(A12,', ',:)/17X,8(A12,', ',:))
 9037 FORMAT(/35X,'*** NETWORK ID: ',A8,' ;  NETWORK TYPE: ',
     &       A8,' ***')
 9043 FORMAT(/45X,'*** DISCRETE CARTESIAN RECEPTOR POINTS ***')
 9044 FORMAT(/47X,'*** DISCRETE POLAR RECEPTOR POINTS ***')
 9045 FORMAT(6X,2(F12.2,2X),F13.5)
 9047 FORMAT(2X,A12,': ',F12.2,2X,F10.2,2X,F13.5)
 9048 FORMAT(6X,' X-COORD (M)   Y-COORD (M)        ',A4,
     &      22X,' X-COORD (M)   Y-COORD (M)        ',A4,/65(' -'))
 9049 FORMAT(5X,'ORIGIN',59X,'ORIGIN',
     &      /5X,' SRCID         DIST (M)   DIR (DEG)        ',A4,
     &      18X,' SRCID         DIST (M)   DIR (DEG)        ',A4,
     &      /65(' -'))
 9090 FORMAT(A132)
 9095 FORMAT(132(' '))

      RETURN
      END

      SUBROUTINE PRTNHI
C***********************************************************************
C                 PRTNHI Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Print Out The Specified Highest Value
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To store high short term values in global arrays
C                    rather than local arrays for later summary table
C                    output.
C                    R.W. Brode, PES, Inc. - August 15, 1995.
C
C        MODIFIED:   To add one more decimal place to receptor elevations
C                    and flagpole heights for the temporary event file.
C                    R.W. Brode, PES, Inc. - November 15, 1995.
C
C        INPUTS:  Arrays of Model Results
C
C        OUTPUTS: Printed Model Outputs for Short Term Values
C
C        CALLED FROM:   MAIN
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: IWHP(NVAL), IHST, IVAL, K, IT1, KWRT, IGRP_TMP
      DOUBLE PRECISION :: XR2, YR2, ZE2, ZH2, ZF2
      CHARACTER NAMEEV*10

C     Variable Initialization
      MODNAM = 'PRTNHI'

C --- Initialize IWHP array
      IWHP = 0

C     Write Out the 'EV STARTING' Card to the Temp-EVent File for
C     First Output Type Only (i.e., ITYP = 1)
      IF (ITYP .EQ. 1) THEN
         WRITE(ITEVUT,9000)
      END IF

      DO IAVE = 1, NUMAVE
C        Decide if Print The Period
         IHST = 0
         DO IVAL = 1, NVAL
            IF (NHIAVE(IVAL,IAVE) .EQ. 1) THEN
               IHST = IHST + 1
               IWHP(IHST) = IVAL
            END IF
         END DO
         IF (IHST .EQ. 0) THEN
C           No High Values for This IAVE; Cycle to Next Averaging Period
            CYCLE
         END IF
C        Print The Data
         DO IVAL = 1, NVAL
            IF (NHIAVE(IVAL,IAVE) .EQ. 1) THEN
C              Print Out High Value By Receptor Table       ---   CALL SPRTHT
               CALL SPRTHT(IVAL)
            END IF
         END DO
C        Print Out The Temporary File
         DO IGRP = 1, NUMGRP
C           Print Out the High Values
            DO IREC = 1, NUMREC
C               Get The Maximum in Nth Highest
                DO K = 1, IHST
                   IF (HIVALU(IREC,IWHP(K),IGRP,IAVE,ITYP) .GT.
     &                  HMAX(K,IGRP,IAVE,ITYP)) THEN
          HMAX(K,IGRP,IAVE,ITYP)   = HIVALU(IREC,IWHP(K),IGRP,IAVE,ITYP)
          HMDATE(K,IGRP,IAVE,ITYP) = NHIDAT(IREC,IWHP(K),IGRP,IAVE,ITYP)
          HMCLM(K,IGRP,IAVE,ITYP)  = HCLMSG(IREC,IWHP(K),IGRP,IAVE,ITYP)
          HMLOC(K,IGRP,IAVE,ITYP)  = IREC
                   END IF
               END DO
            END DO
C
C           Output The Max-Upto-IHST to the TempEVent File for the
C           First Output Type Only (i.e., ITYP = 1)
            IF (ITYP .EQ. 1) THEN
               DO K = 1, IHST
                  IT1 = MIN( 999, IWHP(K) )
                  IF (HMLOC(K,IGRP,IAVE,ITYP) .EQ. 0) THEN
                     XR2 = 0.0D0
                     YR2 = 0.0D0
                     ZE2 = 0.0D0
                     ZH2 = 0.0D0
                     ZF2 = 0.0D0
                  ELSE
                     XR2 = AXR(HMLOC(K,IGRP,IAVE,ITYP))
                     YR2 = AYR(HMLOC(K,IGRP,IAVE,ITYP))
                     ZE2 = AZELEV(HMLOC(K,IGRP,IAVE,ITYP))
                     ZH2 = AZHILL(HMLOC(K,IGRP,IAVE,ITYP))
                     ZF2 = AZFLAG(HMLOC(K,IGRP,IAVE,ITYP))
                  END IF
                  
                  IF (IGRP .GT. 999) THEN
C                    Number of Source Groups Exceeds Limit of EVNAME Field,
C                    Write Warning Message and Reset to 999
                     IF (IGRP .LE. 9999) THEN
                        WRITE(DUMMY,'(I2.2,''hr'',1X,''IG='',I4)') 
     &                                                  IAVE, IGRP
                     ELSE
                        WRITE(DUMMY,'(I2.2,''hr'',1X,''IG>9999'')')
     &                                                  IAVE
                     END IF
                     CALL ERRHDL(PATH,MODNAM,'W','235',DUMMY)
                     IGRP_TMP = 999
                  ELSE
                     IGRP_TMP = IGRP
                  END IF
                  
                  IF (KAVE(IAVE) .LE. 24) THEN
                     WRITE(NAMEEV,'(A1,I3.3,A1,I2.2,I3.3)')
     &                       'H',IT1,'H',KAVE(IAVE),IGRP_TMP
                  ELSE
C                    KAVE > 24 Means MONTH Average; Write Out as 72 (=720/10)
                     KWRT = KAVE(IAVE)/10
                     WRITE(NAMEEV,'(A1,I3.3,A1,I2.2,I3.3)')
     &                       'H',IT1,'H',KWRT,IGRP_TMP
                  END IF
                  WRITE(ITEVUT,9001) NAMEEV, KAVE(IAVE),
     &                  GRPID(IGRP), HMDATE(K,IGRP,IAVE,ITYP),
     &                  HMAX(K,IGRP,IAVE,ITYP)
                  WRITE(ITEVUT,9002) NAMEEV, XR2, YR2, ZE2, ZH2, ZF2
               END DO
            END IF

         END DO

      END DO

C     Write Out the 'EV FINISHED' Card to the Temp-EVent File for
C     First Output Type Only (i.e., ITYP = 1)
      IF (ITYP .EQ. 1) THEN
         WRITE(ITEVUT,9009)
      END IF

 9000 FORMAT('EV STARTING')
 9001 FORMAT(3X,'EVENTPER',1X,A10,1X,I3,2X,A8,3X,I8.8,1X,F17.5)
 9002 FORMAT(3X,'EVENTLOC',1X,A10,1X,'XR= ',F15.6,' YR= ',F15.6,
     &       3(1X,F10.4))
 9009 FORMAT('EV FINISHED')

      RETURN
      END

      SUBROUTINE SPRTHT(IHNUM)
C***********************************************************************
C                 SPRTHT Module of AERMOD Model
C
C        PURPOSE: Print Out The Highest Result Values by Receptor Net
C
C        PROGRAMMER: ROGER BRODE, JEFF WANG
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To include EVALCART receptors with DISCCART
C                    receptors for output.
C                    R. W. Brode, U.S. EPA/OAQPS/AQMG - 10/19/2009
C
C        MODIFIED:   To remove references to BOUNDARY receptors
C                    R. W. Brode, U.S. EPA/OAQPS/AQMG - 12/07/2006
C
C        MODIFIED:   To adjust format statement 9082 for BOUNDARY receptors
C                    to better accommodate UTM coordinates - 9/29/92
C
C        INPUTS:  Arrays of Source Parameters
C                 Arrays of Receptor Locations
C                 Arrays of Model Results
C
C        OUTPUTS: Printed Model Outputs
C
C        CALLED FROM:   LTOUT
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: IHNUM, I, J, K, II, INDZ, INDC, NX, NY, INDEXW
      INTEGER :: IDEC, IMOD
      DOUBLE PRECISION :: YCOVAL, XRMS, YRMS, DIST, DIR
      CHARACTER BUF132*132, CHRVAL*5, CHRVALS(10)*5

C     Variable Initializations
      DATA (CHRVALS(I),I=1,10) /'  1ST','  2ND','  3RD','  4TH','  5TH',
     &                          '  6TH','  7TH','  8TH','  9TH',' 10TH'/
      MODNAM = 'SPRTHT'
      BUF132 = ' '
      INDZ   = 0

C --- Assign character label for rank
      IF (IHNUM .LE. 10) THEN
         CHRVAL = CHRVALS(IHNUM)
      ELSE IF (MOD(IHNUM,100) .GT. 10 .AND. 
     &         MOD(IHNUM,100) .LT. 20) THEN
         IDEC = INT(IHNUM/10)
         IMOD = MOD(IHNUM,10)
         WRITE(CHRVAL,'(I2,I1,"TH")') IDEC, IMOD
      ELSE IF (IHNUM .LE. 999) THEN
         IDEC = INT(IHNUM/10)
         IMOD = MOD(IHNUM,10)
         IF (IMOD .EQ. 0) IMOD = 10
         WRITE(CHRVAL,'(I2,A3)') IDEC, CHRVALS(IMOD)(3:5)
      END IF

      DO IGRP = 1, NUMGRP

         IF (.NOT. PSDCREDIT) THEN
C           Fill Work Array With SRCIDs For This Group
            INDGRP = 0
            DO ISRC = 1, NUMSRC
               IF (IGROUP(ISRC,IGRP) .EQ. 1) THEN
                  INDGRP = INDGRP + 1
                  WORKID(INDGRP) = SRCID(ISRC)
               END IF
            END DO
         ELSE
C           Assign 'N/A' for source IDs for PSDCREDIT option
            INDGRP = 1
            WORKID(INDGRP) = 'N/A'
         END IF

C ---    Check for BACKGROUND "source" being included 
C        in source group
         IF (GRP_BACK(IGRP)) THEN
            INDGRP = INDGRP + 1
            WORKID(INDGRP) = 'BACKGROUND'
C           Check for More Than 29 Sources Per Group
            INDEXW = MIN(29,NSRC+1)
         ELSE
            INDEXW = MIN(29,NSRC)      
         END IF
C        Check for More Than 29 Sources Per Group
         IF (INDGRP .GT. INDEXW) THEN
            WORKID(INDEXW) = ' . . . '
            INDGRP = INDEXW
         END IF

C        Print Receptor Network Coordinates:
C        Set Number of Columns Per Page, NCPP
         NCPP = 5
C        Set Number of Rows Per Page, NRPP
         NRPP = 40
C        Begin LOOP Through Networks
         DO I = 1, INNET
C           Calculate Number of Pages Per X-Group, NPPX, & Per Y-Group, NPPY
            NPPX = 1 + INT((NUMXPT(I)-1)/NCPP)
            NPPY = 1 + INT((NUMYPT(I)-1)/NRPP)
            DO NX = 1, NPPX
               DO NY = 1, NPPY
                  CALL HEADER(IOUNIT)
                  WRITE(IOUNIT,9032) CHRVAL, CHRAVE(IAVE),
     &       (CHIDEP(II,ITYP),II=1,6),GRPID(IGRP),(WORKID(J),J=1,INDGRP)
                  WRITE(IOUNIT,9037) NTID(I), NTTYP(I)
C                 Print The Values By Source Group
                  WRITE(IOUNIT,9011) CHIDEP(3,ITYP), POLLUT,
     &                               OUTLBL(ITYP)
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
     &                       (HIVALU(INDZ+J-1,IHNUM,IGRP,IAVE,ITYP),
     &                        HCLMSG(INDZ+J-1,IHNUM,IGRP,IAVE,ITYP),
     &                        NHIDAT(INDZ+J-1,IHNUM,IGRP,IAVE,ITYP),
     &                        J=1+NCPP*(NX-1),NUMXPT(I))
                        ELSE
                           WRITE(IOUNIT,9013) YCOVAL,
     &                       (HIVALU(INDZ+J-1,IHNUM,IGRP,IAVE,ITYP),
     &                        HCLMSG(INDZ+J-1,IHNUM,IGRP,IAVE,ITYP),
     &                        NHIDAT(INDZ+J-1,IHNUM,IGRP,IAVE,ITYP),
     &                        J=1+NCPP*(NX-1),NCPP*NX)
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
     &                       (HIVALU(INDZ+J-1,IHNUM,IGRP,IAVE,ITYP),
     &                        HCLMSG(INDZ+J-1,IHNUM,IGRP,IAVE,ITYP),
     &                        NHIDAT(INDZ+J-1,IHNUM,IGRP,IAVE,ITYP),
     &                        J=1+NCPP*(NX-1),NUMXPT(I))
                        ELSE
                           WRITE(IOUNIT,9013) YCOVAL,
     &                       (HIVALU(INDZ+J-1,IHNUM,IGRP,IAVE,ITYP),
     &                        HCLMSG(INDZ+J-1,IHNUM,IGRP,IAVE,ITYP),
     &                        NHIDAT(INDZ+J-1,IHNUM,IGRP,IAVE,ITYP),
     &                        J=1+NCPP*(NX-1),NCPP*NX)
                        END IF
                     END DO
                  END IF
               END DO
            END DO
         END DO
C        End LOOP Through Networks

         IF (IRSTAT(4).NE.0 .OR. IRSTAT(8).NE.0) THEN
C ---       Include EVALCART receptors with DISCCART receptors.
C           Print Out The Coord. & Concentrations For Discrete Cart Receptors
            INDC = 0
            DO IREC = 1, NUMREC
               IF (RECTYP(IREC) .EQ. 'DC') THEN
                  INDC = INDC + 1
                  IF (MOD(INDC-1,80) .EQ. 0) THEN
                     CALL HEADER(IOUNIT)
                     WRITE(IOUNIT,9032) CHRVAL, CHRAVE(IAVE),
     &       (CHIDEP(II,ITYP),II=1,6),GRPID(IGRP),(WORKID(J),J=1,INDGRP)
                     WRITE(IOUNIT,9043)
                     WRITE(IOUNIT,9011) CHIDEP(3,ITYP), POLLUT,
     &                                  OUTLBL(ITYP)
                     WRITE(IOUNIT,9048) CHIDEP(3,ITYP), CHIDEP(3,ITYP)
                  END IF
                  IF (MOD(INDC,2) .NE. 0) THEN
                     WRITE(BUF132(1:65),9045) AXR(IREC), AYR(IREC),
     &                     HIVALU(IREC,IHNUM,IGRP,IAVE,ITYP),
     &                     HCLMSG(IREC,IHNUM,IGRP,IAVE,ITYP),
     &                     NHIDAT(IREC,IHNUM,IGRP,IAVE,ITYP)
                  ELSE
                     WRITE(BUF132(66:130),9045) AXR(IREC), AYR(IREC),
     &                     HIVALU(IREC,IHNUM,IGRP,IAVE,ITYP),
     &                     HCLMSG(IREC,IHNUM,IGRP,IAVE,ITYP),
     &                     NHIDAT(IREC,IHNUM,IGRP,IAVE,ITYP)
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
C           Print Out The Coord. & Concentrations For Discrete Polar Receptors
            INDC = 0
            DO IREC = 1, NUMREC
               IF (RECTYP(IREC) .EQ. 'DP') THEN
                  INDC = INDC + 1
                  XRMS = AXR(IREC) - AXS(IREF(IREC))
                  YRMS = AYR(IREC) - AYS(IREF(IREC))
                  DIST = DSQRT(XRMS*XRMS + YRMS*YRMS)
                  DIR  = DATAN2(XRMS, YRMS) * RTODEG
                  IF (DIR .LE. 0.0D0) DIR = DIR + 360.0D0
                  IF (MOD(INDC-1,80) .EQ. 0) THEN
                     CALL HEADER(IOUNIT)
                     WRITE(IOUNIT,9032) CHRVAL, CHRAVE(IAVE),
     &       (CHIDEP(II,ITYP),II=1,6),GRPID(IGRP),(WORKID(J),J=1,INDGRP)
                     WRITE(IOUNIT,9044)
                     WRITE(IOUNIT,9011) CHIDEP(3,ITYP), POLLUT,
     &                                  OUTLBL(ITYP)
                     WRITE(IOUNIT,9049) CHIDEP(3,ITYP), CHIDEP(3,ITYP)
                  END IF
                  IF (MOD(INDC,2) .NE. 0) THEN
                     WRITE(BUF132(1:66),9047) SRCID(IREF(IREC)), DIST,
     &                       DIR, HIVALU(IREC,IHNUM,IGRP,IAVE,ITYP),
     &                            HCLMSG(IREC,IHNUM,IGRP,IAVE,ITYP),
     &                            NHIDAT(IREC,IHNUM,IGRP,IAVE,ITYP)
                  ELSE
                     WRITE(BUF132(67:132),9047) SRCID(IREF(IREC)), DIST,
     &                       DIR, HIVALU(IREC,IHNUM,IGRP,IAVE,ITYP),
     &                            HCLMSG(IREC,IHNUM,IGRP,IAVE,ITYP),
     &                            NHIDAT(IREC,IHNUM,IGRP,IAVE,ITYP)
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

      END DO

 9011 FORMAT(/40X,'** ',A4,' OF ',A8,' IN ',A40,' **'/)
 9010 FORMAT(66(' -')/)
 9016 FORMAT(1X,' Y-COORD  |',50X,'X-COORD (METERS)')
 9017 FORMAT(1X,' (METERS) |',3X,F13.2,4(11X,F13.2,:))
 9018 FORMAT(1X,'DIRECTION |',50X,'DISTANCE (METERS)')
 9019 FORMAT(1X,'(DEGREES) |',3X,F13.2,4(11X,F13.2,:))
 9013 FORMAT(1X,F9.1,1X,'|',5(F13.5,A1,'(',I8.8,')',:))
 9032 FORMAT(/30X,'*** THE ',A5,' HIGHEST ',A5,1X,6A4,
     &       'VALUES FOR SOURCE GROUP:',2X,A8,' ***',
     &       /34X,'INCLUDING SOURCE(S):     ',5(A12,', ',:),
     &       /17X,8(A12,', ',:)/17X,8(A12,', ',:)/17X,8(A12,', ',:))
 9037 FORMAT(/35X,'*** NETWORK ID: ',A8,' ;  NETWORK TYPE: ',
     &       A8,' ***')
 9043 FORMAT(/45X,'*** DISCRETE CARTESIAN RECEPTOR POINTS ***')
 9044 FORMAT(/47X,'*** DISCRETE POLAR RECEPTOR POINTS ***')
 9045 FORMAT(6X,2(F11.2,2X),F13.5,A1,1X,'(',I8.8,')')
 9047 FORMAT(1X,A12,': ',F11.2,1X,F10.2,1X,F13.5,A1,1X,'(',I8.8,')')
 9048 FORMAT(6X,'X-COORD (M)  Y-COORD (M)        ',A4,5X,'(YYMMDDHH)',
     &      14X,'X-COORD (M)  Y-COORD (M)        ',A4,5X,'(YYMMDDHH)',
     &      /66(' -'))
 9049 FORMAT(4X,'ORIGIN',60X,'ORIGIN',
     &      /5X,'SRCID        DIST (M)  DIR (DEG)       ',A4,
     &                                               5X,'(YYMMDDHH)',
     &       8X,'SRCID        DIST (M)  DIR (DEG)       ',A4,
     &                                               5X,'(YYMMDDHH)',
     &      /65(' -'))
 9090 FORMAT(A132)
 9095 FORMAT(132(' '))

      RETURN
      END

      SUBROUTINE PRTMAX
C***********************************************************************
C                 PRTMAX Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Print Out The Overall Maximum Value Tables
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To correct potential error when IMXVAL = 1 and
C                    MXLOCA = 0.  R.W. Brode, PES, Inc. - 12/2/98
C
C        INPUTS:  Arrays of Source Parameters
C                 Arrays of Receptor Locations
C                 Arrays of Model Results
C
C        OUTPUTS: Printed Model Outputs
C
C        CALLED FROM:   OUTPUT
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: J, K, L, NPG, NROWS, JSTRT, II, J1, KMAX1, KMAX2,
     &           INDEXW
      DOUBLE PRECISION :: XR1, YR1, XR2, YR2
      CHARACTER NTY1*2, NTY2*2

C     Variable Initializations
      MODNAM = 'PRTMAX'

      DO IAVE = 1, NUMAVE
C        Check Array to See IF Maximum Values Are Needed For This AVEPER
         IF (MAXAVE(IAVE) .NE. 1) CYCLE

         DO IGRP = 1, NUMGRP
            IF (.NOT. PSDCREDIT) THEN
C              Fill Work Array With SRCIDs For This Group
               INDGRP = 0
               DO ISRC = 1, NUMSRC
                  IF (IGROUP(ISRC,IGRP) .EQ. 1) THEN
                     INDGRP = INDGRP + 1
                     WORKID(INDGRP) = SRCID(ISRC)
                  END IF
               END DO
            ELSE
C              Assign 'N/A' for source IDs for PSDCREDIT option
               INDGRP = 1
               WORKID(INDGRP) = 'N/A'
            END IF

C ---       Check for BACKGROUND "source" being included 
C           in source group
            IF (GRP_BACK(IGRP)) THEN
               INDGRP = INDGRP + 1
               WORKID(INDGRP) = 'BACKGROUND'
C              Check for More Than 29 Sources Per Group
               INDEXW = MIN(29,NSRC+1)
            ELSE
               INDEXW = MIN(29,NSRC)      
            END IF
C           Check for More Than 29 Sources Per Group
            IF (INDGRP .GT. INDEXW) THEN
               WORKID(INDEXW) = ' . . . '
               INDGRP = INDEXW
            END IF

            IF (IMXVAL(IAVE) .GE. 2) THEN
C              Determine Number of Pages @ 80 Per Page, NPG
               NPG = 1 + INT((IMXVAL(IAVE)-1)/80)
               DO L = 1, NPG
C                 Determine Number of Rows for This Page, NROWS
                  IF (L .EQ. NPG) THEN
                     NROWS = (IMXVAL(IAVE)-80*(L-1))/2
                  ELSE
                     NROWS = 40
                  END IF
C                 Write Out Header Information for This Page
                  CALL HEADER(IOUNIT)
                  WRITE(IOUNIT,9032) IMXVAL(IAVE), CHRAVE(IAVE),
     &              (CHIDEP(II,ITYP),II=1,6), GRPID(IGRP), (WORKID(K),
     &                                              K = 1,INDGRP)
                  WRITE(IOUNIT,9011) CHIDEP(3,ITYP), POLLUT,
     &                               OUTLBL(ITYP)
                  WRITE(IOUNIT,1) CHIDEP(3,ITYP), CHIDEP(3,ITYP)
C                 Set Start Row of Loop for This Page, JSTRT
                  JSTRT = 1 + 80*(L-1)
                  DO J = JSTRT, JSTRT+NROWS-1
                     J1 = J + NROWS
                     IF (L.EQ.NPG .AND. MOD(IMXVAL(IAVE),2).NE.0) THEN
                        J1 = J1 + 1
                     END IF
                     KMAX1 = MXLOCA(J,IGRP,IAVE,ITYP)
                     KMAX2 = MXLOCA(J1,IGRP,IAVE,ITYP)
                     IF (KMAX1 .EQ. 0) THEN
                        XR1 = 0.0D0
                        YR1 = 0.0D0
                        NTY1 = ' '
                     ELSE
                        XR1 = AXR(KMAX1)
                        YR1 = AYR(KMAX1)
                        NTY1 = RECTYP(KMAX1)
                     END IF
                     IF (KMAX2 .EQ. 0) THEN
                        XR2 = 0.0D0
                        YR2 = 0.0D0
                        NTY2 = ' '
                     ELSE
                        XR2 = AXR(KMAX2)
                        YR2 = AYR(KMAX2)
                        NTY2 = RECTYP(KMAX2)
                     END IF
                     WRITE(IOUNIT,2) J, RMXVAL(J,IGRP,IAVE,ITYP),
     &               MCLMSG(J,IGRP,IAVE,ITYP), MXDATE(J,IGRP,IAVE,ITYP),
     &                XR1, YR1, NTY1, J1,
     &             RMXVAL(J1,IGRP,IAVE,ITYP), MCLMSG(J1,IGRP,IAVE,ITYP),
     &                 MXDATE(J1,IGRP,IAVE,ITYP), XR2, YR2, NTY2
                  END DO
               END DO
               IF (MOD(IMXVAL(IAVE),2) .NE. 0) THEN
C                 Odd Number of Max Values - Print Out Last Value
                  J = INT(IMXVAL(IAVE)/2) + 1 + 40*(NPG-1)
                  KMAX1 = MXLOCA(J,IGRP,IAVE,ITYP)
                  IF (KMAX1 .EQ. 0) THEN
                     XR1 = 0.0D0
                     YR1 = 0.0D0
                     NTY1 = ' '
                  ELSE
                     XR1 = AXR(KMAX1)
                     YR1 = AYR(KMAX1)
                     NTY1 = RECTYP(KMAX1)
                  END IF
                  WRITE(IOUNIT,3) J, RMXVAL(J,IGRP,IAVE,ITYP),
     &               MCLMSG(J,IGRP,IAVE,ITYP), MXDATE(J,IGRP,IAVE,ITYP),
     &                  XR1, YR1, NTY1
               END IF
            ELSE
               J = 1
               KMAX1 = MXLOCA(J,IGRP,IAVE,ITYP)
               IF (KMAX1 .EQ. 0) THEN
                  XR1 = 0.0D0
                  YR1 = 0.0D0
                  NTY1 = '  '
               ELSE
                  XR1 = AXR(KMAX1)
                  YR1 = AYR(KMAX1)
                  NTY1 = RECTYP(KMAX1)
               END IF
               CALL HEADER(IOUNIT)
               WRITE(IOUNIT,9032) IMXVAL(IAVE), CHRAVE(IAVE),
     &           (CHIDEP(II,ITYP),II=1,6), GRPID(IGRP), (WORKID(K),
     &                                              K = 1,INDGRP)
               WRITE(IOUNIT,9011) CHIDEP(3,ITYP), POLLUT, OUTLBL(ITYP)
               WRITE(IOUNIT,1) CHIDEP(3,ITYP), CHIDEP(3,ITYP)
               WRITE(IOUNIT,3) J, RMXVAL(J,IGRP,IAVE,ITYP),
     &               MCLMSG(J,IGRP,IAVE,ITYP), MXDATE(J,IGRP,IAVE,ITYP),
     &               XR1, YR1, NTY1
            END IF

C           WRITE Out Explanation of Receptor Types
            WRITE(IOUNIT,9050)

         END DO
      END DO

 1    FORMAT(1X,'RANK',8X,A4,4X,'(YYMMDDHH) AT',6X,
     &           'RECEPTOR (XR,YR) OF TYPE ',3X,
     &           'RANK',8X,A4,4X,'(YYMMDDHH) AT',6X,
     &           'RECEPTOR (XR,YR) OF TYPE ',
     &           /66(' -'))
 2    FORMAT(1X,I4,'.',1X,F13.5,A1,'(',I8.8,') AT',1X,
     &          '(',F10.2,', ',F10.2,')  ',A2,5X,
     &          I4,'.',1X,F13.5,A1,'(',I8.8,') AT',1X,
     &          '(',F10.2,', ',F10.2,')  ',A2)
 3    FORMAT(1X,I4,'.',1X,F13.5,A1,'(',I8.8,') AT',1X,
     &          '(',F10.2,', ',F10.2,')  ',A2)
 9011 FORMAT(/40X,'** ',A4,' OF ',A8,' IN ',A40,' **'/)
 9032 FORMAT(/30X,'*** THE MAXIMUM ',I4,2X,A5,1X,6A4,
     &       'VALUES FOR SOURCE GROUP:',2X,A8,' ***'
     &       /34X,'INCLUDING SOURCE(S):     ',5(A12,', ',:),
     &       /17X,8(A12,', ',:)/17X,8(A12,', ',:)/17X,8(A12,', ',:))
 9050 FORMAT(/1X,' *** RECEPTOR TYPES:  GC = GRIDCART',
     &                            /23X,'GP = GRIDPOLR',
     &                            /23X,'DC = DISCCART',
     &                            /23X,'DP = DISCPOLR')

      RETURN
      END

      SUBROUTINE PRTSUM(IOUNT)
C***********************************************************************
C                 PRTSUM Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Print Out the Result Summary Tables
C
C        PROGRAMMER: Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   Adjusted format of column headers and other write
C                    statements, including removal of '1X' used to 
C                    skip the Fortran carriage-control character, which 
C                    is no longer needed.  Also included number of years
C                    for MULTYEAR option.
C                    R. W. Brode, U.S. EPA, OAQPS, AQMG, 10/19/2009
C
C        MODIFIED:   Changed parameter for specifying the number of
C                    high annual/period averages from NVAL to NHIANN.
C                    R.W. Brode, PES, Inc.,  4/3/98
C
C        MODIFIED:   To use arrays for high short term values, rather
C                    than reading from temporary event file.
C                    R.W. Brode, PES, Inc. - August 15, 1995.
C
C        INPUTS:  EVENT.TMP File Which Contains Maximum Values
C
C        OUTPUTS: Result Summary Table By Average Period
C
C        CALLED FROM:   MAIN
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: IWHP(NVAL), I, IVAL, INDMX, IHST, INDLOC, IOUNT
      INTEGER :: IDEC, IMOD
      DOUBLE PRECISION :: AXR1, AYR1, AZELV1, AZHIL1, AZFLG1, XR2, YR2,
     &           ZE2, ZH2,ZF2
      CHARACTER PERCHR*6, RANK(10)*5, CHRVAL*5

C     Variable Initializations
      DATA (RANK(I),I=1,10) /'  1ST','  2ND','  3RD','  4TH','  5TH',
     &                       '  6TH','  7TH','  8TH','  9TH',' 10TH'/
      MODNAM = 'PRTSUM'
      
      IF (PERIOD) THEN
         PERCHR = 'PERIOD'
      ELSE IF (ANNUAL) THEN
         PERCHR = 'ANNUAL'
      END IF

C     Print Maximum PERIOD Averages, If Appropriate
      IF (PERIOD .OR. ANNUAL) THEN
C        Calculate Number of Groups Per Page, NGPP
         NGPP = MAX( 1, INT(50/(NHIANN+1)) )
         DO IGRP = 1, NUMGRP
            IF (MOD(IGRP-1, NGPP) .EQ. 0) THEN
               CALL HEADER(IOUNT)
               IF (PERIOD .AND. MULTYR) THEN
                  WRITE(IOUNT,9020) PERCHR, IANHRS, NUMYRS
               ELSE IF (PERIOD) THEN
                  WRITE(IOUNT,9021) PERCHR, IANHRS
               ELSE IF (ANNUAL) THEN
                  WRITE(IOUNT,9023) PERCHR, NUMYRS
               END IF
               WRITE(IOUNT,9011) CHIDEP(3,ITYP), POLLUT, PERLBL(ITYP)
               WRITE(IOUNT,9022) CHIDEP(1,ITYP), CHIDEP(2,ITYP),
     &                           CHIDEP(3,ITYP)
            END IF
            DO IVAL = 1, NHIANN
               INDMX = IMXLOC(IVAL,IGRP,ITYP)
               IF (IVAL .EQ. 1 .AND. INDMX .NE. 0) THEN
                  WRITE(IOUNT,1012) GRPID(IGRP), RANK(IVAL),
     &                  AMXVAL(IVAL,IGRP,ITYP), AXR(INDMX), AYR(INDMX),
     &                  AZELEV(INDMX), AZHILL(INDMX), AZFLAG(INDMX),
     &                  RECTYP(INDMX), NETID(INDMX)
               ELSE IF (IVAL .EQ. 1 .AND. INDMX .EQ. 0) THEN
                  AXR1 = 0.0D0
                  AYR1 = 0.0D0
                  AZELV1 = 0.0D0
                  AZHIL1 = 0.0D0
                  AZFLG1 = 0.0D0
                  WRITE(IOUNT,1014) GRPID(IGRP), RANK(IVAL),
     &               AMXVAL(IVAL,IGRP,ITYP), AXR1, AYR1, AZELV1, AZHIL1,
     &               AZFLG1
               ELSE IF (INDMX .EQ. 0) THEN
                  AXR1 = 0.0D0
                  AYR1 = 0.0D0
                  AZELV1 = 0.0D0
                  AZHIL1 = 0.0D0
                  AZFLG1 = 0.0D0
                  WRITE(IOUNT,1015) RANK(IVAL),
     &               AMXVAL(IVAL,IGRP,ITYP), AXR1, AYR1, AZELV1, AZHIL1,
     &               AZFLG1
               ELSE
                  WRITE(IOUNT,1013) RANK(IVAL),
     &                  AMXVAL(IVAL,IGRP,ITYP), AXR(INDMX), AYR(INDMX),
     &                  AZELEV(INDMX), AZHILL(INDMX), AZFLAG(INDMX),
     &                  RECTYP(INDMX), NETID(INDMX)
               END IF
            END DO
         END DO
C        WRITE Out Explanation of Receptor Types
         WRITE(IOUNT,9050)
      END IF

C --- Skip "standard" summary of short-term averages for 
C     PM25AVE, NO2AVE, or SO2AVE processing
      IF (PM25AVE .OR. NO2AVE .OR. SO2AVE) RETURN

C ---       
C     Begin LOOP Through Averaging Periods
      DO IAVE = 1, NUMAVE
C ---    Determine which high ranked values are requested for
C        this averaging period
         IHST = 0
         DO IVAL = 1, NVAL
            IF (NHIAVE(IVAL,IAVE) .EQ. 1) THEN
               IHST = IHST + 1
               IWHP(IHST) = IVAL
            END IF
         END DO
         IF (IHST .EQ. 0) THEN
C           No High Values for This IAVE; Cycle to Next Averaging Period
            CYCLE
         END IF
C        Calculate Number of Groups Per Page, NGPP
         NGPP = MAX( 1, INT(50/(IHST+1)) )

C        Begin Source Group LOOP
         DO IGRP = 1, NUMGRP
C           Begin LOOP Through High Values
            DO I = 1, IHST
            
C ---          Assign character label for rank
               IF (IWHP(I) .LE. 10) THEN
                  CHRVAL = RANK(IWHP(I))
               ELSE IF (MOD(IWHP(I),100) .GT. 10 .AND. 
     &                  MOD(IWHP(I),100) .LT. 20) THEN
                  IDEC = INT(IWHP(I)/10)
                  IMOD = MOD(IWHP(I),10)
                  WRITE(CHRVAL,'(I2,I1,"TH")') IDEC, IMOD
               ELSE IF (IWHP(I) .LE. 999) THEN
                  IDEC = INT(IWHP(I)/10)
                  IMOD = MOD(IWHP(I),10)
                  IF (IMOD .EQ. 0) IMOD = 10
                  WRITE(CHRVAL,'(I2,A3)') IDEC, RANK(IMOD)(3:5)
               END IF

               INDLOC = HMLOC(I,IGRP,IAVE,ITYP)
               IF (I .EQ. 1) THEN
                  IF (MOD(IGRP-1,NGPP) .EQ. 0) THEN
                     CALL HEADER(IOUNT)
                     IF (MULTYR) THEN
                        WRITE(IOUNT,9030) CHRAVE(IAVE), NUMYRS
                     ELSE
                        WRITE(IOUNT,9031) CHRAVE(IAVE)
                     END IF
                     WRITE(IOUNT,9011) CHIDEP(3,ITYP),POLLUT,
     &                                 OUTLBL(ITYP)
                     WRITE(IOUNT,9032) CHIDEP(1,ITYP),
     &                                 CHIDEP(2,ITYP),CHIDEP(3,ITYP)
                  END IF
                  WRITE(IOUNT,*) ' '
                  IF (INDLOC .EQ. 0) THEN
                     XR2 = 0.0D0
                     YR2 = 0.0D0
                     ZE2 = 0.0D0
                     ZH2 = 0.0D0
                     ZF2 = 0.0D0
                     WRITE(IOUNT,1004) GRPID(IGRP), CHRVAL,
     &                  HMAX(I,IGRP,IAVE,ITYP),
     &                  HMCLM(I,IGRP,IAVE,ITYP),
     &                  HMDATE(I,IGRP,IAVE,ITYP),
     &                  XR2, YR2, ZE2, ZH2, ZF2
                  ELSE
                     XR2 = AXR(INDLOC)
                     YR2 = AYR(INDLOC)
                     ZE2 = AZELEV(INDLOC)
                     ZH2 = AZHILL(INDLOC)
                     ZF2 = AZFLAG(INDLOC)
                     WRITE(IOUNT,1002) GRPID(IGRP), CHRVAL,
     &                  HMAX(I,IGRP,IAVE,ITYP),
     &                  HMCLM(I,IGRP,IAVE,ITYP),
     &                  HMDATE(I,IGRP,IAVE,ITYP),
     &                  XR2, YR2, ZE2, ZH2, ZF2, RECTYP(INDLOC),
     &                  NETID(INDLOC)
                  END IF
               ELSE
                  IF (INDLOC .EQ. 0) THEN
                     XR2 = 0.0D0
                     YR2 = 0.0D0
                     ZE2 = 0.0D0
                     ZF2 = 0.0D0
                     WRITE(IOUNT,1005) CHRVAL,
     &                  HMAX(I,IGRP,IAVE,ITYP),
     &                  HMCLM(I,IGRP,IAVE,ITYP),
     &                  HMDATE(I,IGRP,IAVE,ITYP),
     &                  XR2, YR2, ZE2, ZH2, ZF2
                  ELSE
                     XR2 = AXR(INDLOC)
                     YR2 = AYR(INDLOC)
                     ZE2 = AZELEV(INDLOC)
                     ZH2 = AZHILL(INDLOC)
                     ZF2 = AZFLAG(INDLOC)
                     WRITE(IOUNT,1003) CHRVAL,
     &                  HMAX(I,IGRP,IAVE,ITYP),
     &                  HMCLM(I,IGRP,IAVE,ITYP),
     &                  HMDATE(I,IGRP,IAVE,ITYP),
     &                  XR2, YR2, ZE2, ZH2, ZF2, RECTYP(INDLOC),
     &                  NETID(INDLOC)
                  END IF
               END IF
            END DO
         END DO

C        WRITE Out Explanation of Receptor Types
         WRITE(IOUNT,9050)

C     End loop through averaging periods
      END DO

 1002 FORMAT(A8,' HIGH ',A5,' HIGH VALUE IS',F14.5,A1,' ON ',
     &       I8.8,': AT ','(',2(F11.2,', '),2(F8.2,', '),F7.2,')',
     &       2X,A2,2X,A8)
 1003 FORMAT(8X,' HIGH ',A5,' HIGH VALUE IS',F14.5,A1,' ON ',
     &       I8.8,': AT ','(',2(F11.2,', '),2(F8.2,', '),F7.2,')',
     &       2X,A2,2X,A8)
 1004 FORMAT(A8,' HIGH ',A5,' HIGH VALUE IS',F14.5,A1,' ON ',
     &       I8.8,': AT ','(',2(F11.2,', '),2(F8.2,', '),F7.2,')')
 1005 FORMAT(8X,' HIGH ',A5,' HIGH VALUE IS',F14.5,A1,' ON ',
     &       I8.8,': AT ','(',2(F11.2,', '),2(F8.2,', '),F7.2,')')
 1012 FORMAT(/A8,A5,' HIGHEST VALUE IS',F14.5,' AT ',
     &       '(',2(F11.2,', '),2(F8.2,', '),F7.2,')',2X,A2,2X,A8)
 1013 FORMAT(8X,A5,' HIGHEST VALUE IS',F14.5,' AT ',
     &       '(',2(F11.2,', '),2(F8.2,', '),F7.2,')',2X,A2,2X,A8)
 1014 FORMAT(/A8,A5,' HIGHEST VALUE IS',F14.5,' AT ',
     &       '(',2(F11.2,', '),2(F8.2,', '),F7.2,')')
 1015 FORMAT(8X,A5,' HIGHEST VALUE IS',F14.5,' AT ',
     &       '(',2(F11.2,', '),2(F8.2,', '),F7.2,')')
 9011 FORMAT(/36X,'** ',A4,' OF ',A8,' IN ',A40,' **'/)
 9020 FORMAT(/40X,'*** THE SUMMARY OF MAXIMUM ',A6,' (',I6,
     &       ' HRS) RESULTS ***',/40X,'***    ACROSS ',I6,
     &       ' YEARS WITH THE MULTYEAR OPTION    ***'/)
 9021 FORMAT(/40X,'*** THE SUMMARY OF MAXIMUM ',A6,' (',I6,
     &       ' HRS) RESULTS ***'/)
 9023 FORMAT(/35X,'*** THE SUMMARY OF MAXIMUM ',A6,' RESULTS ',
     &       'AVERAGED OVER ',I3,' YEARS ***'/)
 9022 FORMAT(109X,'NETWORK',/,'GROUP ID',23X,3A4,
     &       16X,'RECEPTOR  (XR, YR, ZELEV, ZHILL, ZFLAG)',2X,'OF TYPE',
     &       2X,'GRID-ID',/60('- '))
 9030 FORMAT(/48X,'*** THE SUMMARY OF HIGHEST ',A5,' RESULTS ***'/,
     &        44X,'*** ACROSS ',I6,
     &                        ' YEARS WITH THE MULTYEAR OPTION ***'/)
 9031 FORMAT(/48X,'*** THE SUMMARY OF HIGHEST ',A5,' RESULTS ***'/)
 9032 FORMAT(54X,'DATE',68X,'NETWORK',/,'GROUP ID',26X,3A4,5X,
     &       '(YYMMDDHH)',13X,'RECEPTOR  (XR, YR, ZELEV, ZHILL, ZFLAG)',
     &       4X,'OF TYPE',2X,'GRID-ID',/66('- '))
 9050 FORMAT(//' *** RECEPTOR TYPES:  GC = GRIDCART',
     &                          /22X,'GP = GRIDPOLR',
     &                          /22X,'DC = DISCCART',
     &                          /22X,'DP = DISCPOLR')

      RETURN
      END

      SUBROUTINE EVEFIL
C***********************************************************************
C                 EVEFIL Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Generate EVENT Input File
C
C        PROGRAMMER: Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To allow for changes in the ISTRG PARAMETER, currently
C                    set to 132.  Also moved the code to insert a blank line
C                    after each pathway to SUB. SETUP.
C                    R.W. Brode, PES, Inc. - November 15, 1995.
C
C        INPUTS:  EVENT.TMP File Which Contains Maximum 10 Values
C
C        OUTPUTS: EVENT Input Runstream Image File
C
C        CALLED FROM: OUTPUT
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: IAVEP
      DOUBLE PRECISION :: CONC1
      CHARACTER EVFRM*20
      LOGICAL HITIN

C     Variable Initializations
      MODNAM = 'EVEFIL'
      HITIN  = .FALSE.
      EOF    = .FALSE.

C     Setup WRITE format for runstream record,
C     based on the ISTRG PARAMETER (set in MAIN1)
      WRITE(EVFRM,9300) ISTRG
 9300 FORMAT('(A',I4.4,')')

C     Rewind Temporary Event File
      REWIND ITEVUT

C     Read Records From The Temporary Event File
      DO WHILE (.NOT. EOF)
         IF (.NOT. HITIN) THEN
C           Not in the Event Pathway - Echo Input to EVENT File
            READ(ITEVUT,EVFRM,END=999) RUNST1
            IF (RUNST1(1:11) .EQ. 'EV STARTING') THEN
C              Event Pathway Starts - Set Logical Switch
               HITIN = .TRUE.
               IF (LOCB(1) .EQ. 1) THEN
                  WRITE(IEVUNT,'(a:)') RUNST1(1:LEN_TRIM(RUNST1))
               ELSE IF (LOCB(1) .EQ. 2) THEN
                  WRITE(IEVUNT,'(1x,a:)') RUNST1(1:LEN_TRIM(RUNST1))
               ELSE IF (LOCB(1) .EQ. 3) THEN
                  WRITE(IEVUNT,'(2x,a:)') RUNST1(1:LEN_TRIM(RUNST1))
               ELSE IF (LOCB(1) .EQ. 4) THEN
                  WRITE(IEVUNT,'(3x,a:)') RUNST1(1:LEN_TRIM(RUNST1))
               END IF
            ELSE
               WRITE(IEVUNT,'(a:)') RUNST1(1:LEN_TRIM(RUNST1))
            END IF
         ELSE
            READ(ITEVUT,EVFRM,END=999) RUNST1
            IF (RUNST1(1:11) .EQ. 'EV FINISHED') THEN
               IF (MXFILE) THEN
C                 Add Events From Max Value (>Thresh) Files ---   CALL MXEVNT
                  CALL MXEVNT
               END IF
               IF (LOCB(1) .EQ. 1) THEN
                  WRITE(IEVUNT,'(a:)') RUNST1(1:LEN_TRIM(RUNST1))
               ELSE IF (LOCB(1) .EQ. 2) THEN
                  WRITE(IEVUNT,'(1x,a:)') RUNST1(1:LEN_TRIM(RUNST1))
               ELSE IF (LOCB(1) .EQ. 3) THEN
                  WRITE(IEVUNT,'(2x,a:)') RUNST1(1:LEN_TRIM(RUNST1))
               ELSE IF (LOCB(1) .EQ. 4) THEN
                  WRITE(IEVUNT,'(3x,a:)') RUNST1(1:LEN_TRIM(RUNST1))
               END IF
               HITIN = .FALSE.
            END IF
            IF (HITIN .AND. RUNST1(LOCB(1):LOCB(1)+10) .EQ. 
     &                                              '   EVENTPER') THEN
               READ(RUNST1(LOCB(1)+23:),'(I3)') IAVEP
               READ(RUNST1(LOCB(1)+47:),'(F18.5)',ERR=99) CONC1
            END IF

            GO TO 100

C           Write Out Warning Message:  Error Reading CONC From TmpEvent File
 99         CALL ERRHDL(PATH,MODNAM,'W','570',
     &                                   RUNST1(LOCB(1)+12:LOCB(1)+19))
C           Set CONC1 To Large Value for Event File
            CONC1 = 1.0D9

 100        CONTINUE
            IF (HITIN. AND. IAVEP.NE.720 .AND. CONC1.NE.0.0D0) THEN
C              Write Out EVENTPER & EVENTLOC Cards, Allowing for Column Shift
               WRITE(IEVUNT,'(a:)') RUNST1(LOCB(1):LEN_TRIM(RUNST1))
            END IF
         END IF

         GO TO 11

 999     EOF = .TRUE.
 11      CONTINUE
      END DO

C     Write OU Pathway Images to EVENT File, Allowing For Column Shift
      IF (LOCB(1) .EQ. 1) THEN
         WRITE(IEVUNT,1011) EVPARM
      ELSE IF (LOCB(1) .EQ. 2) THEN
         WRITE(IEVUNT,1012) EVPARM
      ELSE IF (LOCB(1) .EQ. 3) THEN
         WRITE(IEVUNT,1013) EVPARM
      ELSE IF (LOCB(1) .EQ. 4) THEN
         WRITE(IEVUNT,1014) EVPARM
      END IF

      CLOSE(UNIT=IEVUNT)

 1011 FORMAT(/'OU STARTING',
     &       /'   EVENTOUT  ',A6,
     &       /'OU FINISHED')
 1012 FORMAT(/' OU STARTING',
     &       /'    EVENTOUT  ',A6,
     &       /' OU FINISHED')
 1013 FORMAT(/'  OU STARTING',
     &       /'     EVENTOUT  ',A6,
     &       /'  OU FINISHED')
 1014 FORMAT(/'   OU STARTING',
     &       /'      EVENTOUT  ',A6,
     &       /'   OU FINISHED')

      RETURN
      END

      SUBROUTINE MXEVNT
C***********************************************************************
C                 MXEVNT Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Generate EVENT File Inputs From
C                 Maximum Value (>Threshold) Files
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To add one more decimal place to receptor elevations
C                    and flagpole heights for the event file.
C                    R.W. Brode, PES, Inc. - November 15, 1995.
C
C        INPUTS:  Maximum Value Files
C
C        OUTPUTS: Events for EVENT Input Runstream File
C
C        CALLED FROM: EVEFIL
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: IAVEP, KDATE
      DOUBLE PRECISION :: CONC1, XR2, YR2, ZE2, ZH2, ZF2
      CHARACTER NAMEEV*10, GID*8, BUFIN*90

C     Variable Initializations
      MODNAM = 'MXEVNT'

C     Begin Averaging Period LOOP
      DO IAVE = 1, NUMAVE
C        Initialize Event Counter for This IAVE
         NUMEVE = 0
C        Begin Source Group LOOP
         DO IGRP = 1, NUMGRP
            IF (MAXFLE(IGRP,IAVE) .EQ. 1) THEN
C              Maximum Value File Exists for This Group and AvePer
C              Rewind File
               REWIND IMXUNT(IGRP,IAVE)
               EOF = .FALSE.

C              Loop Through Threshold File and Write Out Events to EVENT File
               DO WHILE (.NOT. EOF)
                  READ(IMXUNT(IGRP,IAVE),100,ERR=99,END=999) BUFIN
 100              FORMAT(A90)
C                 Skip Record if Part of Header, '*' in Column 1
                  IF (BUFIN(1:1) .EQ. '*') GO TO 11
                  READ(BUFIN,THRFRM,ERR=99) IAVEP,
     &                 GID, KDATE, XR2, YR2, ZE2, ZH2, ZF2, CONC1
                  IF (IAVEP.NE.720 .AND. IAVEP.EQ.KAVE(IAVE) .AND.
     &                                     GID.EQ.GRPID(IGRP)) THEN
C                    Increment Event Counter and Generate Event Name
                     NUMEVE = NUMEVE + 1
                     
                     IF (NUMEVE .GT. 999999) THEN
C                       Number of Events Exceeds Limit of Field,
C                       Write Warning Message and Reset to 1
                        WRITE(DUMMY,'(3X,I2.2,3X)') IAVEP
                        CALL ERRHDL(PATH,MODNAM,'W','413',DUMMY)
                        NUMEVE = 1
                     END IF

                     WRITE(NAMEEV,'("TH",I2.2,I6.6)') IAVEP, NUMEVE
C                    Write EVENTPER & EVENTLOC Cards, Allowing for Col. Shift
                     IF (LOCB(1) .EQ. 1) THEN
                        WRITE(IEVUNT,1901) NAMEEV,IAVEP,GID,KDATE,CONC1
                        WRITE(IEVUNT,1911) NAMEEV, XR2, YR2, ZE2, ZH2,
     &                                     ZF2
                     ELSE IF (LOCB(1) .EQ. 2) THEN
                        WRITE(IEVUNT,1902) NAMEEV,IAVEP,GID,KDATE,CONC1
                        WRITE(IEVUNT,1912) NAMEEV, XR2, YR2, ZE2, ZH2,
     &                                     ZF2
                     ELSE IF (LOCB(1) .EQ. 3) THEN
                        WRITE(IEVUNT,1903) NAMEEV,IAVEP,GID,KDATE,CONC1
                        WRITE(IEVUNT,1913) NAMEEV, XR2, YR2, ZE2, ZH2,
     &                                     ZF2
                     ELSE IF (LOCB(1) .EQ. 4) THEN
                        WRITE(IEVUNT,1904) NAMEEV,IAVEP,GID,KDATE,CONC1
                        WRITE(IEVUNT,1914) NAMEEV, XR2, YR2, ZE2, ZH2,
     &                                     ZF2
                     END IF
                     GO TO 11
                  ELSE
                     GO TO 11
                  END IF

 999              EOF = .TRUE.
 11               CONTINUE
               END DO

            END IF
         END DO
C        End Source Group LOOP
      END DO
C     End Averaging Period LOOP

      GO TO 1000

C     WRITE Error Message for Error Reading Threshold File
 99   WRITE(DUMMY,'("MAXFL",I3.3)') IMXUNT(IGRP,IAVE)
      CALL ERRHDL(PATH,MODNAM,'E','510',DUMMY)

 1901 FORMAT(3X,'EVENTPER',1X,A10,1X,I3,2X,A8,3X,I8.8,1X,F17.5)
 1902 FORMAT(4X,'EVENTPER',1X,A10,1X,I3,2X,A8,3X,I8.8,1X,F17.5)
 1903 FORMAT(5X,'EVENTPER',1X,A10,1X,I3,2X,A8,3X,I8.8,1X,F17.5)
 1904 FORMAT(6X,'EVENTPER',1X,A10,1X,I3,2X,A8,3X,I8.8,1X,F17.5)
 1911 FORMAT(3X,'EVENTLOC',1X,A10,1X,'XR= ',F15.6,' YR= ',F15.6,
     &       3(1X,F10.4))
 1912 FORMAT(4X,'EVENTLOC',1X,A10,1X,'XR= ',F15.6,' YR= ',F15.6,
     &       3(1X,F10.4))
 1913 FORMAT(5X,'EVENTLOC',1X,A10,1X,'XR= ',F15.6,' YR= ',F15.6,
     &       3(1X,F10.4))
 1914 FORMAT(6X,'EVENTLOC',1X,A10,1X,'XR= ',F15.6,' YR= ',F15.6,
     &       3(1X,F10.4))

 1000 RETURN
      END

      SUBROUTINE PRTPM25
C***********************************************************************
C                 PRTPM25 Module of AERMOD Model
C
C        PURPOSE: Print Out The Average H8H Values for PM25
C
C        PROGRAMMER: Roger Brode
C
C        DATE:       June 19, 1998
C
C        MODIFIED:   To allow user-specified rank for PM2.5 processing
C                    to accommodate latest guidance for PM2.5 modeling.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
C               
C        MODIFIED:   To include EVALCART receptors with DISCCART
C                    receptors for output.
C                    R. W. Brode, U.S. EPA/OAQPS/AQMG - 10/19/2009
C
C        MODIFIED:   To remove references to BOUNDARY receptors
C                    R. W. Brode, U.S. EPA/OAQPS/AQMG - 12/07/2006
C
C        INPUTS:  Arrays of Source Parameters
C                 Arrays of Receptor Locations
C                 Arrays of Model Results
C
C        OUTPUTS: Printed Model Outputs
C
C        CALLED FROM:   OUTPUT
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, J, K, II, INDZ, INDC, NX, NY, INDEXW, N
      INTEGER :: IDEC, IMOD
      DOUBLE PRECISION :: YCOVAL, XRMS, YRMS, DIST, DIR
      CHARACTER BUF132*132

      CHARACTER RANK(10)*5, CHRVAL*5

C     Variable Initializations
      DATA (RANK(I),I=1,10) /'  1ST','  2ND','  3RD','  4TH','  5TH',
     &                       '  6TH','  7TH','  8TH','  9TH',' 10TH'/
      MODNAM = 'PRTPM25'
      BUF132 = ' '
      INDZ   = 0

C     Write Out the 'EV STARTING' Card to the Temp-EVent File for
C     First Output Type Only (i.e., ITYP = 1)
      IF (ITYP .EQ. 1) THEN
         WRITE(ITEVUT,9000)
      END IF

C --- Loop through ranks
      DO N = 1, NVAL

C      If this rank not applicable, cycle to next rank
       IF( NHIAVE(N,1) .NE. 1 ) CYCLE
         
C ---  Loop through source groups
       DO IGRP = 1, NUMGRP
        
         IF (.NOT. PSDCREDIT) THEN
C           Fill Work Array With SRCIDs For This Group
            INDGRP = 0
            DO ISRC = 1, NUMSRC
               IF (IGROUP(ISRC,IGRP) .EQ. 1) THEN
                  INDGRP = INDGRP + 1
                  WORKID(INDGRP) = SRCID(ISRC)
               END IF
            END DO
         ELSE
C           Assign 'N/A' for source IDs for PSDCREDIT option
            INDGRP = 1
            WORKID(INDGRP) = 'N/A'
         END IF

C ---    Check for BACKGROUND "source" being included 
C        in source group
         IF (GRP_BACK(IGRP)) THEN
            INDGRP = INDGRP + 1
            WORKID(INDGRP) = 'BACKGROUND'
C           Check for More Than 29 Sources Per Group
            INDEXW = MIN(29,NSRC+1)
         ELSE
            INDEXW = MIN(29,NSRC)      
         END IF
C        Check for More Than 29 Sources Per Group
         IF (INDGRP .GT. INDEXW) THEN
            WORKID(INDEXW) = ' . . . '
            INDGRP = INDEXW
         END IF

C        Print Receptor Network Coordinates:
C        Set Number of Columns Per Page, NCPP
         NCPP = 9
C        Set Number of Rows Per Page, NRPP
         NRPP = 40
C        Begin LOOP Through Networks
         DO I = 1, INNET
C           Calculate Number of Pages Per X-Group, NPPX, & Per Y-Group, NPPY
            NPPX = 1 + INT((NUMXPT(I)-1)/NCPP)
            NPPY = 1 + INT((NUMYPT(I)-1)/NRPP)
            DO NX = 1, NPPX
               DO NY = 1, NPPY
                  CALL HEADER(IOUNIT)
                  IF (NHIAVE(N,1) .EQ. 1) THEN
C ---                Assign character label for rank
                     IF (N .LE. 10) THEN
                        CHRVAL = RANK(N)
                     ELSE IF (MOD(N,100) .GT. 10 .AND. 
     &                        MOD(N,100) .LT. 20) THEN
                        IDEC = INT(N/10)
                        IMOD = MOD(N,10)
                        WRITE(CHRVAL,'(I2,I1,"TH")') IDEC, IMOD
                     ELSE IF (N .LE. 999) THEN
                        IDEC = INT(N/10)
                        IMOD = MOD(N,10)
                        IF (IMOD .EQ. 0) IMOD = 10
                        WRITE(CHRVAL,'(I2,A3)') IDEC, RANK(IMOD)(3:5)
                     END IF

                     IF (NO2AVE .OR. SO2AVE) THEN
                        WRITE(IOUNIT,90321) CHRVAL,
     &                       (CHIDEP(II,ITYP),II=1,6),NUMYRS,
     &                        GRPID(IGRP),(WORKID(K),K = 1,INDGRP)
                     ELSE
                        WRITE(IOUNIT,9032) CHRVAL, CHRAVE(1),
     &                       (CHIDEP(II,ITYP),II=1,6),NUMYRS,
     &                        GRPID(IGRP),(WORKID(K),K = 1,INDGRP)
                     END IF
                  END IF
                  WRITE(IOUNIT,9037) NTID(I), NTTYP(I)
C                 Print The Values By Source Group
                  WRITE(IOUNIT,9011) CHIDEP(3,ITYP), POLLUT,PERLBL(ITYP)
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
     &            (SUMHNH(INDZ+J-1,IGRP,N),J=1+NCPP*(NX-1),NUMXPT(I))
                        ELSE
                           WRITE(IOUNIT,9013) YCOVAL,
     &              (SUMHNH(INDZ+J-1,IGRP,N),J=1+NCPP*(NX-1),NCPP*NX)
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
     &            (SUMHNH(INDZ+J-1,IGRP,N),J=1+NCPP*(NX-1),NUMXPT(I))
                        ELSE
                           WRITE(IOUNIT,9013) YCOVAL,
     &              (SUMHNH(INDZ+J-1,IGRP,N),J=1+NCPP*(NX-1),NCPP*NX)
                        END IF
                     END DO
                  END IF
               END DO
            END DO
         END DO
C        End LOOP Through Networks

         IF (IRSTAT(4).NE.0 .OR. IRSTAT(8).NE.0) THEN
C ---       Include EVALCART receptors with DISCCART receptors.
C           Print Out The Coord. & Concentrations For Discrete Cart Receptors
            INDC = 0
            DO IREC = 1, NUMREC
               IF (RECTYP(IREC) .EQ. 'DC') THEN
                  INDC = INDC + 1
                  IF (MOD(INDC-1,80) .EQ. 0) THEN
                     CALL HEADER(IOUNIT)
                     IF (NHIAVE(N,1) .EQ. 1) THEN
C ---                   Assign character label for rank
                        IF (N .LE. 10) THEN
                           CHRVAL = RANK(N)
                        ELSE IF (MOD(N,100) .GT. 10 .AND. 
     &                           MOD(N,100) .LT. 20) THEN
                           IDEC = INT(N/10)
                           IMOD = MOD(N,10)
                           WRITE(CHRVAL,'(I2,I1,"TH")') IDEC, IMOD
                        ELSE IF (N .LE. 999) THEN
                           IDEC = INT(N/10)
                           IMOD = MOD(N,10)
                           IF (IMOD .EQ. 0) IMOD = 10
                           WRITE(CHRVAL,'(I2,A3)') IDEC, RANK(IMOD)(3:5)
                        END IF

                        IF (NO2AVE .OR. SO2AVE) THEN
                           WRITE(IOUNIT,90321) CHRVAL,
     &                       (CHIDEP(II,ITYP),II=1,6),
     &                        NUMYRS,GRPID(IGRP),(WORKID(K),K=1,INDGRP)
                        ELSE
                           WRITE(IOUNIT,9032) CHRVAL, CHRAVE(1),
     &                       (CHIDEP(II,ITYP),II=1,6),
     &                        NUMYRS,GRPID(IGRP),(WORKID(K),K=1,INDGRP)
                        END IF
                     END IF
                     WRITE(IOUNIT,9043)
                     WRITE(IOUNIT,9011) CHIDEP(3,ITYP), POLLUT,
     &                                  PERLBL(ITYP)
                     WRITE(IOUNIT,9048) CHIDEP(3,ITYP), CHIDEP(3,ITYP)
                  END IF
                  IF (MOD(INDC,2) .NE. 0) THEN
                     WRITE(BUF132(1:60),9045) AXR(IREC), AYR(IREC),
     &                     SUMHNH(IREC,IGRP,N)
                  ELSE
                     WRITE(BUF132(61:120),9045) AXR(IREC), AYR(IREC),
     &                     SUMHNH(IREC,IGRP,N)
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
C           Print Out The Coord. & Concentrations For Discrete Polar Receptors
            INDC = 0
            DO IREC = 1, NUMREC
               IF (RECTYP(IREC) .EQ. 'DP') THEN
                  INDC = INDC + 1
                  XRMS = AXR(IREC) - AXS(IREF(IREC))
                  YRMS = AYR(IREC) - AYS(IREF(IREC))
                  DIST = DSQRT(XRMS*XRMS + YRMS*YRMS)
                  DIR  = DATAN2(XRMS, YRMS) * RTODEG
                  IF (DIR .LE. 0.0D0) DIR = DIR + 360.0D0
                  IF (MOD(INDC-1,80) .EQ. 0) THEN
                     CALL HEADER(IOUNIT)
                     IF (NHIAVE(N,1) .EQ. 1) THEN
C ---                   Assign character label for rank
                        IF (N .LE. 10) THEN
                           CHRVAL = RANK(N)
                        ELSE IF (MOD(N,100) .GT. 10 .AND. 
     &                           MOD(N,100) .LT. 20) THEN
                           IDEC = INT(N/10)
                           IMOD = MOD(N,10)
                           WRITE(CHRVAL,'(I2,I1,"TH")') IDEC, IMOD
                        ELSE IF (N .LE. 999) THEN
                           IDEC = INT(N/10)
                           IMOD = MOD(N,10)
                           IF (IMOD .EQ. 0) IMOD = 10
                           WRITE(CHRVAL,'(I2,A3)') IDEC, RANK(IMOD)(3:5)
                        END IF

                        IF (NO2AVE .OR. SO2AVE) THEN
                           WRITE(IOUNIT,90321) CHRVAL,
     &                       (CHIDEP(II,ITYP),II=1,6),
     &                        NUMYRS,GRPID(IGRP),(WORKID(K),K=1,INDGRP)
                        ELSE
                           WRITE(IOUNIT,9032) CHRVAL, CHRAVE(1),
     &                       (CHIDEP(II,ITYP),II=1,6),
     &                        NUMYRS,GRPID(IGRP),(WORKID(K),K=1,INDGRP)
                        END IF
                     END IF
                     WRITE(IOUNIT,9044)
                     WRITE(IOUNIT,9011) CHIDEP(3,ITYP), POLLUT,
     &                                  PERLBL(ITYP)
                     WRITE(IOUNIT,9049) CHIDEP(3,ITYP), CHIDEP(3,ITYP)
                  END IF
                  IF (MOD(INDC,2) .NE. 0) THEN
                     WRITE(BUF132(1:65),9047) SRCID(IREF(IREC)), DIST,
     &                                       DIR, SUMHNH(IREC,IGRP,N)
                  ELSE
                     WRITE(BUF132(66:130),9047) SRCID(IREF(IREC)), DIST,
     &                                       DIR, SUMHNH(IREC,IGRP,N)
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

       END DO   ! End loop on source groups
      END DO    ! End loop on ranks

C     Write Out the 'EV FINISHED' Card to the Temp-EVent File for
C     First Output Type Only (i.e., ITYP = 1)
      IF (ITYP .EQ. 1) THEN
         WRITE(ITEVUT,9009)
      END IF

 9000 FORMAT('EV STARTING')
 9009 FORMAT('EV FINISHED')
 9011 FORMAT(/40X,'** ',A4,' OF ',A8,' IN ',A40,' **'/)
 9010 FORMAT(66(' -')/)
 9013 FORMAT(2X,F10.2,1X,'|',1X,9(F13.5))
 9016 FORMAT(3X,' Y-COORD  |',48X,'X-COORD (METERS)')
 9017 FORMAT(3X,' (METERS) |',1X,9(1X,F12.2,:))
 9018 FORMAT(3X,'DIRECTION |',48X,'DISTANCE (METERS)')
 9019 FORMAT(3X,'(DEGREES) |',1X,9(1X,F12.2,:))
 9032 FORMAT(/13X,'*** THE ',A5,'-HIGHEST ',A5,1X,6A4,' VALUES ',
     &      'AVERAGED OVER ',I3,' YEARS FOR SOURCE GROUP:',1X,A8,' ***',
     &       /34X,'INCLUDING SOURCE(S):     ',5(A12,', ',:),
     &       /17X,8(A12,', ',:)/17X,8(A12,', ',:)/17X,8(A12,', ',:))
90321 FORMAT(/4X,'*** THE ',A5,'-HIGHEST MAX DAILY 1-HR ',6A4,
     &                                                ' VALUES ',
     &      'AVERAGED OVER ',I3,' YEARS FOR SOURCE GROUP:',1X,A8,' ***',
     &       /34X,'INCLUDING SOURCE(S):     ',5(A12,', ',:),
     &       /17X,8(A12,', ',:)/17X,8(A12,', ',:)/17X,8(A12,', ',:))
 9037 FORMAT(/35X,'*** NETWORK ID: ',A8,' ;  NETWORK TYPE: ',
     &       A8,' ***')
 9043 FORMAT(/45X,'*** DISCRETE CARTESIAN RECEPTOR POINTS ***')
 9044 FORMAT(/47X,'*** DISCRETE POLAR RECEPTOR POINTS ***')
 9045 FORMAT(6X,2(F12.2,2X),F13.5)
 9047 FORMAT(2X,A12,': ',F12.2,2X,F10.2,2X,F13.5)
 9048 FORMAT(6X,' X-COORD (M)   Y-COORD (M)        ',A4,
     &      22X,' X-COORD (M)   Y-COORD (M)        ',A4,/65(' -'))
 9049 FORMAT(5X,'ORIGIN',59X,'ORIGIN',
     &      /5X,' SRCID         DIST (M)   DIR (DEG)        ',A4,
     &      18X,' SRCID         DIST (M)   DIR (DEG)        ',A4,
     &      /65(' -'))
 9090 FORMAT(A132)
 9095 FORMAT(132(' '))

      RETURN
      END

      SUBROUTINE MAXPM25
C***********************************************************************
C                 MAXPM25 Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Update Overall Maximum Value Arrays
C                 NMXPM = 10 Assigned in PARAMETER Statement in MAIN1
C                 Note: For duplicate values, the earlier occurrence keeps
C                       its rank within the array
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    June 19, 1998
C
C        INPUTS:  Maximum Value Table Options
C                 Array of CONC or DEPOS Averages
C                 Averaging Period
C
C        OUTPUTS: Updated Maximum Value Array
C                 Updated Maximum Date Array
C                 Updated Maximum Receptor Array
C
C        CALLED FROM:   HIVALS
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: J, N

C     Variable Initializations
      MODNAM = 'MAXPM25'

C     Begin Source Group LOOP
      DO IGRP = 1, NUMGRP
C        Begin loop through ranks
         DO N = 1, NVAL
C           Cycle to next rank if this rank is not applicable
            IF( NHIAVE(N,1) .NE. 1 ) CYCLE
C           Begin Receptor LOOP
            RECEPTOR_LOOP: DO IREC = 1, NUMREC
               IF (NMXPM .GT. 1) THEN
                  IF (SUMHNH(IREC,IGRP,N) .GT. 
     &                                       MXPMVAL(NMXPM,IGRP,N)) THEN
                     DO J = NMXPM-1, 1, -1
                        IF(SUMHNH(IREC,IGRP,N) .LE. 
     &                                           MXPMVAL(J,IGRP,N)) THEN
                           MXPMVAL(J+1,IGRP,N) = SUMHNH(IREC,IGRP,N)
                           MXPMLOC(J+1,IGRP,N) = IREC
C                          Exit Block
                           CYCLE RECEPTOR_LOOP
                        ELSE
                           MXPMVAL(J+1,IGRP,N) = MXPMVAL(J,IGRP,N)
                           MXPMLOC(J+1,IGRP,N) = MXPMLOC(J,IGRP,N)
                           IF (J .EQ. 1) THEN
                              MXPMVAL(1,IGRP,N) = SUMHNH(IREC,IGRP,N)
                              MXPMLOC(1,IGRP,N) = IREC
                           END IF
                        END IF
                     END DO
                  END IF
               ELSE IF (NMXPM .EQ. 1) THEN
                  IF (SUMHNH(IREC,IGRP,N) .GT. MXPMVAL(1,IGRP,N)) THEN
                     MXPMVAL(1,IGRP,N) = SUMHNH(IREC,IGRP,N)
                     MXPMLOC(1,IGRP,N) = IREC
                  END IF
               END IF
            END DO RECEPTOR_LOOP
C           End Receptor LOOP
         END DO  
C        End loop through ranks
      END DO
C     End Source Group LOOP

      RETURN
      END

      SUBROUTINE PRTPM25SUM(IOUNT)
C***********************************************************************
C                 PRTPM25SUM Module of AERMOD Model
C
C        PURPOSE: Print Out the Result Summary Tables for PM25
C
C        PROGRAMMER: Roger Brode
C
C        DATE:       June 19, 1998
C
C        MODIFIED:   To allow user-specified rank for PM2.5 processing
C                    to accommodate latest guidance for PM2.5 modeling.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
C               
C        MODIFIED:   Adjusted format of column headers and other write
C                    statements, including removal of '1X' used to 
C                    skip the Fortran carriage-control character, which 
C                    is no longer needed.
C                    R. W. Brode, U.S. EPA, OAQPS, AQMG, 10/19/2009
C
C        INPUTS:  Arrays Containing Maximum Values
C
C        OUTPUTS: Result Summary Table By Average Period
C
C        CALLED FROM:   MAIN
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, IVAL, INDMX, IOUNT, N
      INTEGER :: IDEC, IMOD
      DOUBLE PRECISION :: AXR1, AYR1, AZELV1, AZHIL1, AZFLG1
      CHARACTER RANK(10)*5, CHRVAL*5

C     Variable Initializations
      DATA (RANK(I),I=1,10) /'  1ST','  2ND','  3RD','  4TH','  5TH',
     &                       '  6TH','  7TH','  8TH','  9TH',' 10TH'/
      MODNAM = 'PRTPM25SUM'

      IF (NUMAVE .EQ. 1) THEN
C        Calculate Number of Groups Per Page, NGPP
         NGPP = MAX( 1, INT(50/(NMXPM+1)) )
         DO N = 1, NVAL
            IF (NHIAVE(N,1) .NE. 1) CYCLE
            DO IGRP = 1, NUMGRP
               IF (MOD(IGRP-1, NGPP) .EQ. 0) THEN
                  CALL HEADER(IOUNT)
C ---             Assign character label for rank
                  IF (N .LE. 10) THEN
                     CHRVAL = RANK(N)
                  ELSE IF (MOD(N,100) .GT. 10 .AND. 
     &                     MOD(N,100) .LT. 20) THEN
                     IDEC = INT(N/10)
                     IMOD = MOD(N,10)
                     WRITE(CHRVAL,'(I2,I1,"TH")') IDEC, IMOD
                  ELSE IF (N .LE. 999) THEN
                     IDEC = INT(N/10)
                     IMOD = MOD(N,10)
                     IF (IMOD .EQ. 0) IMOD = 10
                     WRITE(CHRVAL,'(I2,A3)') IDEC, RANK(IMOD)(3:5)
                  END IF

                  IF (PM25AVE) THEN
                     WRITE(IOUNT,9091) CHRVAL, CHRAVE(1), NUMYRS
                  ELSE IF (NO2AVE .OR. SO2AVE) THEN
                     WRITE(IOUNT,99091) CHRVAL, CHRAVE(1), NUMYRS
                  END IF
                  WRITE(IOUNT,9011) CHIDEP(3,ITYP), POLLUT, OUTLBL(ITYP)
                  WRITE(IOUNT,9022) CHIDEP(1,ITYP), CHIDEP(2,ITYP),
     &                               CHIDEP(3,ITYP)
               END IF
               DO IVAL = 1, NMXPM
                  INDMX = MXPMLOC(IVAL,IGRP,N)
                  IF (IVAL .EQ. 1 .AND. INDMX .NE. 0) THEN
                     WRITE(IOUNT,1012) GRPID(IGRP), RANK(IVAL),
     &                     MXPMVAL(IVAL,IGRP,N), AXR(INDMX), AYR(INDMX),
     &                     AZELEV(INDMX), AZHILL(INDMX), AZFLAG(INDMX),
     &                     RECTYP(INDMX), NETID(INDMX)
                  ELSE IF (IVAL .EQ. 1 .AND. INDMX .EQ. 0) THEN
                     AXR1 = 0.0D0
                     AYR1 = 0.0D0
                     AZELV1 = 0.0D0
                     AZHIL1 = 0.0D0
                     AZFLG1 = 0.0D0
                     WRITE(IOUNT,1014) GRPID(IGRP), RANK(IVAL),
     &                   MXPMVAL(IVAL,IGRP,N), AXR1, AYR1, AZELV1, 
     &                   AZHIL1, AZFLG1
                  ELSE IF (INDMX .EQ. 0) THEN
                     AXR1 = 0.0D0
                     AYR1 = 0.0D0
                     AZELV1 = 0.0D0
                     AZHIL1 = 0.0D0
                     AZFLG1 = 0.0D0
                     WRITE(IOUNT,1015) RANK(IVAL),
     &                   MXPMVAL(IVAL,IGRP,N), AXR1, AYR1, AZELV1, 
     &                   AZHIL1, AZFLG1
                  ELSE
                     WRITE(IOUNT,1013) RANK(IVAL),
     &                     MXPMVAL(IVAL,IGRP,N), AXR(INDMX), AYR(INDMX),
     &                     AZELEV(INDMX), AZHILL(INDMX), AZFLAG(INDMX),
     &                     RECTYP(INDMX), NETID(INDMX)
                  END IF
               END DO
            END DO
         END DO
C        WRITE Out Explanation of Receptor Types
         WRITE(IOUNT,9050)
      END IF

 1012 FORMAT(/A8,A5,' HIGHEST VALUE IS',F14.5,' AT ',
     &       '(',2(F11.2,', '),2(F8.2,', '),F7.2,')',2X,A2,2X,A8)
 1013 FORMAT(8X,A5,' HIGHEST VALUE IS',F14.5,' AT ',
     &       '(',2(F11.2,', '),2(F8.2,', '),F7.2,')',2X,A2,2X,A8)
 1014 FORMAT(/A8,A5,' HIGHEST VALUE IS',F14.5,' AT ',
     &       '(',2(F11.2,', '),2(F8.2,', '),F7.2,')')
 1015 FORMAT(8X,A5,' HIGHEST VALUE IS',F14.5,' AT ',
     &       '(',2(F11.2,', '),2(F8.2,', '),F7.2,')')
 9011 FORMAT(/36X,'** ',A4,' OF ',A8,' IN ',A40,' **'/)
 9091 FORMAT(/27X,'*** THE SUMMARY OF MAXIMUM ',A5,'-HIGHEST ',
     &       A5,' RESULTS AVERAGED OVER ',I3,' YEARS ***'/)
99091 FORMAT(/22X,'*** THE SUMMARY OF MAXIMUM ',A5,'-HIGHEST ',
     &       'MAX DAILY ',A5,' RESULTS AVERAGED OVER ',I3,' YEARS ***'/)
 9022 FORMAT(109X,'NETWORK',/'GROUP ID',23X,3A4,
     &       16X,'RECEPTOR  (XR, YR, ZELEV, ZHILL, ZFLAG)',2X,'OF TYPE',
     &       2X,'GRID-ID',/60('- '))
 9050 FORMAT(//' *** RECEPTOR TYPES:  GC = GRIDCART',
     &                          /22X,'GP = GRIDPOLR',
     &                          /22X,'DC = DISCCART',
     &                          /22X,'DP = DISCPOLR')

      RETURN
      END

      SUBROUTINE SHOUT
C***********************************************************************
C                 SHOUT Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Process Files of Season/Hour Results
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    June 5, 1997
C
C        MODIFIED:   Adjusted the header format variable (HDFRM) for 
C                    cases with multiple output types.
C                    R. W. Brode, U.S. EPA, OAQPS, AQMG, 10/19/2009
C
C                    Increased length of HDRFRM variable to avoid
C                    potential problems with portability of code.
C                    R. Brode, MACTEC/PES, 10/17/2005
C
C        INPUTS:  Array of Season/Hour Values
C
C        OUTPUTS: File of Season/Hour Values
C
C        CALLED FROM:   OUTPUT
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C Unused:      INTEGER :: I
      CHARACTER HDRFRM*400, SEAFRM*80

C     Variable Initializations
      MODNAM = 'SHOUT'

C     Create Header Format for Columns Based on Number of Output Types
      WRITE(HDRFRM,9020) NUMTYP, NUMTYP+2

C --- Generate ouptut format based on number of output types and 
C     file format ('FIX' or 'EXP')
      IF (FILE_FORMAT .EQ. 'FIX') THEN
C        Use FIXed format (F13.8) for concentrations and fluxes
         WRITE(SEAFRM,1009) NUMTYP
 1009    FORMAT('(2(1X,F13.5),',I1,'(1X,F13.8),3(1X,F7.2),2X,A8,2X,',
     &          '3(I4,2X),A8)')
      ELSE IF (FILE_FORMAT .EQ. 'EXP') THEN
C        Use EXPonential format (E13.6) for concentrations and fluxes
         WRITE(SEAFRM,1010) NUMTYP
 1010    FORMAT('(2(1X,F13.5),',I1,'(1X,E13.6),3(1X,F7.2),2X,A8,2X,',
     &          '3(I4,2X),A8)')
      END IF

C     Begin Source Group LOOP
      DO IGRP = 1, NUMGRP
C        Check for Selection of SEASONHR File for This Group
         IF (ISEAHR(IGRP) .EQ. 1) THEN
            IF (.NOT. L_NoHeader(4)) THEN
C              Write Header Information
               WRITE(ISHUNT(IGRP),9005) VERSN, TITLE1(1:68), RUNDAT
               WRITE(ISHUNT(IGRP),9007) C_METVER, RUNTIM, 
     &                          MODOPS_String(1:LEN_TRIM(MODOPS_String))
               WRITE(ISHUNT(IGRP),9010) GRPID(IGRP), NUMREC, SEAFRM
               WRITE(ISHUNT(IGRP),HDRFRM)(CHIDEP(1,ITYP),CHIDEP(2,ITYP),
     &                                    CHIDEP(3,ITYP),ITYP=1,NUMTYP)
            END IF
C ---       Write data
            DO ISEAS = 1, 4
               DO IHOUR = 1, 24
C                 Begin Receptor LOOP
                  DO IREC = 1, NUMREC
                     INUM = NSEAHR(ISEAS,IHOUR) - NSEACM(ISEAS,IHOUR)
                     WRITE(ISHUNT(IGRP),SEAFRM,ERR=99)
     &                  AXR(IREC), AYR(IREC),
     &               (SHVALS(IREC,IGRP,ISEAS,IHOUR,ITYP),ITYP=1,NUMTYP),
     &                  AZELEV(IREC), AZHILL(IREC), AZFLAG(IREC),
     &                  GRPID(IGRP), INUM, ISEAS, IHOUR, NETID(IREC)
                  END DO
C                 End Receptor LOOP
               END DO
            END DO
         END IF
      END DO
C     End Source Group LOOP

      GO TO 999

C     WRITE Error Message for Problem Writing to Plot File
 99   WRITE(DUMMY,'("SHFIL",I3.3)') ISHUNT(IGRP)
      CALL ERRHDL(PATH,MODNAM,'E','520',DUMMY)

 9005 FORMAT('* AERMOD (',A6,'): ',A68,5X,A8)
 9007 FORMAT('* AERMET (',A6,'):',T93,A8,
     &      /'* MODELING OPTIONS USED: ',A:)
 9010 FORMAT('*',9X,'FILE OF SEASON/HOUR VALUES FOR ',
     &       'SOURCE GROUP: ',A8,
     &      /'*',9X,'FOR A TOTAL OF ', I5,' RECEPTORS.',
     &      /'*',9X,'FORMAT: ',A:)
 9020 FORMAT('(''*'',8X,''X'',13X,''Y'',4X,',I1,'(2X,3A4),3X,''ZELEV'',
     &  3X,''ZHILL'',3X,''ZFLAG'',4X,''GRP'',5X,''NHRS'',2X,''SEAS'',
     &  2X,''HOUR'',3X,''NET ID'',/,''*'',',I1,'(1X,''____________ ''),
     &  3('' ______ ''),'' ________  ____  ____  ____  ________'')')

 999  RETURN
      END

      SUBROUTINE RANKFL
C***********************************************************************
C                 RANKFL Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Process Files To RANK
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   Increased output field width for Rank to allow
C                    for up to 6 digits.
C                    R. Brode, US EPA, OAQPS, AQMG, 10/19/2009
C
C        INPUTS:  Array of High Values
C
C        OUTPUTS: File of High Values for Plotting
C
C        CALLED FROM:   MAIN
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, J, IRANK, IDATSV(NMXVAL)
      LOGICAL FOUND

C     Variable Initializations
      MODNAM = 'RANKFL'

C     Begin Averaging Period LOOP
      DO IAVE = 1, NUMAVE
C        Decide if we should go through the processing
         IF (IRNKFL(IAVE) .EQ. 1) THEN
            IF (.NOT. L_NoHeader(5)) THEN
C              Write Header to File
               WRITE(IRKUNT(IAVE),9005) VERSN, TITLE1(1:68), RUNDAT
               WRITE(IRKUNT(IAVE),9007) C_METVER, RUNTIM, 
     &                          MODOPS_String(1:LEN_TRIM(MODOPS_String))
               WRITE(IRKUNT(IAVE),9010) IRKVAL(IAVE), CHRAVE(IAVE),
     &                                  NUMGRP, RNKFRM
               WRITE(IRKUNT(IAVE),9020) CHIDEP(1,ITYP), CHIDEP(2,ITYP),
     &                                  CHIDEP(3,ITYP)
            END IF
            
C           Begin Source Group LOOP
            DO IGRP = 1, NUMGRP
C              Initialize IDATSV array, which saves dates of ranked values
               IDATSV(:) = 0
               IRANK = 0
C              Begin LOOP Through Max Values
               DO I = 1, IRKVAL(IAVE)
                  FOUND = .FALSE.
                  DO J = 1, NMXVAL
                     IF (MXDATE(I,IGRP,IAVE,ITYP) .EQ. IDATSV(J)) THEN
                        FOUND = .TRUE.
                        EXIT
                     END IF
                  END DO
                  IF (.NOT.FOUND .AND. IRANK.LT.IRKVAL(IAVE)) THEN
                     IRANK = IRANK + 1
                     IREC  = MXLOCA(I,IGRP,IAVE,ITYP)
                     IDATSV(IRANK) = MXDATE(I,IGRP,IAVE,ITYP)
                     WRITE(IRKUNT(IAVE),RNKFRM,ERR=99) IRANK,
     &             RMXVAL(I,IGRP,IAVE,ITYP), MXDATE(I,IGRP,IAVE,ITYP),
     &             AXR(IREC), AYR(IREC), AZELEV(IREC), AZHILL(IREC),
     &             AZFLAG(IREC), GRPID(IGRP)
                  END IF
               END DO
C              End LOOP Through Max Values
            END DO
C           End Source Group LOOP
         END IF
      END DO
C     End Averaging Period LOOP

      GO TO 999

C     WRITE Error Message for Problem Writing to RANK File
 99   WRITE(DUMMY,'("RNKFL",I3.3)') IRKUNT(IAVE)
      CALL ERRHDL(PATH,MODNAM,'E','520',DUMMY)

 9005 FORMAT('* AERMOD (',A6,'): ',A68,5X,A8)
 9007 FORMAT('* AERMET (',A6,'):',T93,A8,
     &      /'* MODELING OPTIONS USED: ',A:)
 9010 FORMAT('*',9X,'RANK-FILE OF UP TO ',I5,' TOP ',A5,' VALUES ',
     &       'FOR ',I5,' SOURCE GROUPS',
     &       /'*',9X,'INCLUDES OVERALL MAXIMUM VALUES WITH DUPLICATE ',
     &               'DATA PERIODS REMOVED',
     &       /'*',9X,'FORMAT: ',A:)
 9020 FORMAT('*  RANK',2X,3A4,3X,'DATE',10X,'X',13X,'Y',8X,'ZELEV',
     &       3X,'ZHILL',3X,'ZFLAG',5X,'GRP',
     &       /'*_______',1X,'____________',1X,'________',
     &       2(2X,'____________'),3(2X,'______'),'  ________')

 999  RETURN
      END
      
      SUBROUTINE MAXDCALC
C***********************************************************************
C                 MAXDCALC Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Recalculates results based on maximmum daily values
C                 (24-hour for PM2.5 and 1-hour averages for NO2 and SO2 
C                 NAAQS) to perform source group contribution analyses 
C                 under the OU MAXDCONT option.
C
C        PROGRAMMER: Roger Brode
C
C        DATE:       February 28, 2011
C
C        MODIFIED:   Incorporated non-DFAULT GRSM option for NO2.
C                    CERC, 11/30/20
C
C        MODIFIED:   Incorporated non-DFAULT/BETA ARM2 option for NO2
C                    Mark Podrez, RTP Environmental
C                    R. Brode, US EPA, OAQPS, AQMG, August 15, 2013
C
C        MODIFIED:   Check for whether the HOURLY emissions option is used 
C                    for specific sources to correct a bug in the MAXDCONT
C                    option for applications that include hourly emissions 
C                    (HOUREMIS) for at least one source, but not all sources. 
C                    R. Brode, US EPA, OAQPS, AQMG, 02/29/2012
C
C        INPUTS:  MAXDAILY file options
C
C        OUTPUTS: 
C
C        CALLED FROM:   MAXDCONT_LOOP
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      
      CHARACTER MODNAM*12
C Unused:      CHARACTER (LEN=6) :: SPEC1, SPEC2, SPEC3
C Unused:      CHARACTER (LEN=8) :: CUSI, CSSI, COSI
C Unused:      INTEGER :: METVER, IOSI, ISSI, IUSI
C     JAT D065, 8/9/21 FOPEN SET BUT NOT USED
c     ILSAVE IS SET BUT NEVER USED
C      LOGICAL :: FOPEN
      INTEGER :: ILSAVE
C Unused:      DOUBLE PRECISION :: RDUM
C Unused:      DOUBLE PRECISION :: O3VALUES(24), O3MIN, O3MAX24
C Unused:       INTEGER :: I
****** ARM2 Modification ******
C Unused:      INTEGER :: Itempp
*******************************

C     Variable Initializations
      MODNAM = 'MAXDCALC'
      EOF   = .FALSE.
C     JAT D065, 8/9/21 FOPEN SET BUT NOT USED
C      FOPEN = .FALSE.
C --- Reset RSTSAV to false for MAXDCALC
      RSTSAV   = .FALSE.

C --- Set IAVE = 1
      IAVE = 1

C --- Extract met data from arrays
      CALL MAXD_METEXT

C     Save ILINE as ILSAVE and Initialize ILINE
C     JAT D065, 8/9/21 ILSAVE IS SET BUT NEVER USED
C      ILSAVE = ILINE

      IF (HOURLY) THEN
C        Process Hourly Emissions from File
C        Begin Source Loop
         DO ISRC = 1, NUMSRC
C ---       Check for HOURLY emissions for this source
            IF (QFLAG(ISRC) .EQ. 'HOURLY') THEN
C ---          Retrieve hourly emissions for MAXDCONT option
               AQS(ISRC) = AAQS(IHR_NDX,IYR_NDX,ISRC)
         
               IF (SRCTYP(ISRC)(1:5) .EQ. 'POINT') THEN
                  ATS(ISRC) =  AATS(IHR_NDX,IYR_NDX,ISRC)
                  AVS(ISRC) =  AAVS(IHR_NDX,IYR_NDX,ISRC)
               ELSE IF (SRCTYP(ISRC) .EQ. 'VOLUME' .AND. 
     &                                     L_HRLYSIG(ISRC)) THEN
                  AHS(ISRC)    =  AAHS(IHR_NDX,IYR_NDX,ISRC)   
                  ASYINI(ISRC) =  AASYINI(IHR_NDX,IYR_NDX,ISRC)
                  ASZINI(ISRC) =  AASZINI(IHR_NDX,IYR_NDX,ISRC)
               ELSE IF (SRCTYP(ISRC)(1:4) .EQ. 'AREA' .AND. 
     &                                     L_HRLYSIG(ISRC)) THEN
                  AHS(ISRC)    =  AAHS(IHR_NDX,IYR_NDX,ISRC)   
                  ASZINI(ISRC) =  AASZINI(IHR_NDX,IYR_NDX,ISRC)
               ELSE IF (SRCTYP(ISRC) .EQ. 'LINE' .AND. 
     &                                     L_HRLYSIG(ISRC)) THEN
                  AHS(ISRC)    =  AAHS(IHR_NDX,IYR_NDX,ISRC)   
                  ASZINI(ISRC) =  AASZINI(IHR_NDX,IYR_NDX,ISRC)
               ELSE IF (SRCTYP(ISRC) .EQ. 'BUOYLINE') THEN 
                  AFP(ISRC) = AAFP(IHR_NDX,IYR_NDX,ISRC)
               END IF
            END IF
         END DO
C*       End Source Loop
      END IF
C*----
C     Save ILINE as ILSAVE and Initialize ILINE
C     JAT D065, 8/9/21 ILSAVE IS SET BUT NEVER USED
C      ILSAVE = ILINE

      IF (L_BACKGRND) THEN
C ---    Assign BACKGRND concentration based on the hour and
C        year index; this value accounts for HOURLY BACKGRND
C        with or without substitution based on other temporally-
C        varying BACKGRND values, or is based soley on the
C        user-specified temporally-varying BACKGRND
         BGCONC = ABGCONC(IHR_NDX,IYR_NDX)
      ELSE
         BGCONC = 0.0D0
      END IF

      IF (PVMRM .OR. OLM .OR. RUNTTRM .OR. GRSM) THEN
C ---    Assign background Ozone concentration based on the 
C        hour and year index; this value accounts for hourly O3
C        with or without substitution based on other temporally-
C        varying O3 values, or is based soley on the
C        user-specified annual or temporally-varying O3 
         O3CONC = AO3CONC(IHR_NDX,IYR_NDX)
C---     Set O3MISS = .T. if stored O3CONC is less than 0.
         IF (O3CONC .LT. 0.0D0) THEN
            O3MISS = .TRUE.
         ELSE
            O3MISS = .FALSE.
         END IF
      END IF

C     CERC 11/30/20
      IF (GRSM) THEN
C ---    Assign background NOx concentration based on the 
C        hour and year index; this value accounts for hourly NOx
C        with or without substitution based on other temporally-
C        varying NOx values, or is based soley on the
C        user-specified annual or temporally-varying NOx 
         NOXBGCONC = ANOXBGCONC(IHR_NDX,IYR_NDX)
C---     Set NOXMISS = .T. if stored NOXBGCONC is less than 0.
         IF (NOXBGCONC .LT. 0.0D0) THEN
            NOXMISS = .TRUE.
         ELSE
            NOXMISS = .FALSE.
         END IF
      END IF

      IF (FULLDATE.GE.ISDATE .AND. FULLDATE.LE.IEDATE) THEN

         IF (CLMHR .AND. CLMPRO) THEN
C           Check for Calm Hr & Processing and Increment Counters
            NUMHRS(1) = NUMHRS(1) + 1
            NUMCLM(1) = NUMCLM(1) + 1
         ELSE IF (MSGHR .AND. MSGPRO) THEN
C           Check for Missing Hour & Processing and Increment Counters
            NUMHRS(1) = NUMHRS(1) + 1
            NUMMSG(1) = NUMMSG(1) + 1
         ELSE IF (ZI .LE. 0.0D0) THEN
C           Write Out The Informational Message & Increment Counters
            NUMHRS(1) = NUMHRS(1) + 1
         ELSE
C           Set CALCS Flag, Increment Counters & Calculate HRVAL
            CALCS = .TRUE.
            NUMHRS(1) = NUMHRS(1) + 1

C           Calculate CONC or DEPOS Values               ---   CALL CALC
            CALL CALC
         END IF

         IF (.NOT.CLMHR .AND. .NOT.MSGHR) THEN
C ---       Non-calm, non-missing hour; apply NO2 options as appropriate

!! Added for TTRM2
!! If TTRM2 (TTRM with the compare option) is requested then
!! perform TTRM >> FIRST <<
            IF (RUNTTRM2) THEN
C ---             Process Hourly Values for TTRM Option
               CMETH = 3
               CALL TTRM_CALC
C ---          Flush HRVAL Arrays (1:NUMTYP)
               HRVAL(:)   = 0.0D0
               IF (PVMRM .AND. .NOT. PSDCREDIT) THEN
                  CALL PVMRM_CALC('ALLSRCS')
               ELSE IF (OLM) THEN
                  CALL OLM_CALC
               ELSE IF (ARM2) THEN
                  CALL ARM2_CALC
               END IF
               GO TO 8857
            END IF
            IF (PVMRM .AND. .NOT.PSDCREDIT) THEN
C ---          Process Hourly Values for PVMRM Option
               CALL PVMRM_CALC('ALLSRCS')
               
            ELSE IF (PVMRM .AND. PSDCREDIT) THEN
C ---          Process Hourly Values for PVMRM Option and PSD credits
C ---          Need to process two separate sets of sources - the
C              increment consumption sources ('NAAQSRC') and the 
C              increment expanding sources ('ALLBASE')
               CALL PVMRM_CALC('NAAQSRC')
               CALL PVMRM_CALC('ALLBASE')

            ELSE IF (OLM) THEN
C ---          Process Hourly Values for OLM Option
               CALL OLM_CALC
!
!   Added for TTRM; AECOM
            ELSE IF (RUNTTRM) THEN
C ---          Process Hourly Values for TTRM Option
               IF (.NOT. RUNTTRM2) THEN
                 CALL TTRM_CALC
               END IF
!   End TTRM insert; Feb. 2021; Modified Nov. 2021

            ELSE IF (ARM2) THEN
C ---          Process Hourly Values for ARM2 Option
               CALL ARM2_CALC
               
            ELSE IF (GRSM) THEN
C ---          CERC 11/30/20 Process Hourly Values for GRSM Option
               CALL GRSM_CALC

            END IF
 8857       CONTINUE
         END IF

         IAVE = 1
C        Check for End of Averaging Period
         IF (KAVE(1).NE.1 .AND. MOD(IHOUR,KAVE(1)).EQ.0) THEN
C           Calculate Applicable Averages          ---   CALL AVER
            CALL AVER
C ---       Reinitialize NUMHRS, NUMCLM and NUMMSG
            NUMHRS(:) = 0
            NUMCLM(:) = 0
            NUMMSG(:) = 0
         END IF

C        Flush HRVAL Arrays (1:NUMTYP)
         HRVAL   = 0.0D0
         AERVAL  = 0.0D0
         PRMVAL  = 0.0D0

         IF (PVMRM .OR. OLM .OR. ARM2 
     &      .OR. RUNTTRM .OR. GRSM) THEN
C           Flush CHI(NUMREC,NUMSRC,NUMTYP) Array 
            CHI(:,:,:) = 0.0D0
            IF(RUNTTRM2)THEN
               TTRMCOMPARE(:,:,:,:) = 0.0D0
            ENDIF
            IF(GRSM)THEN
              CHI_TTRAVPLM = 0.0D0
              CHI_TTRAVPAN = 0.0D0
              CHI_TTRAVAER = 0.0D0
              CHI_TTRAVPRM = 0.0D0
              CHI_TTRAVCHM(:,:) = 0.0D0
            END IF
            IF (PSDCREDIT) THEN
C              Flush ABVAL(NUMREC,NUMTYP) and BCVAL(NUMREC,NUMTYP) Arrays
               ABVAL(:,:) = 0.0D0
               BCVAL(:,:) = 0.0D0
            END IF
         END IF

      END IF

C     Reset CALCS and ENDMON Flags
      CALCS  = .FALSE.
      ENDMON = .FALSE.

C     Save precipitation rates for two previous hours
      prec2 = prec1
      prec1 = Prate

      RETURN
      END

      SUBROUTINE MAXDCNT_FILE(IGRP1,IVAL)
C***********************************************************************
C                 MAXDCONT_FILE Module of AERMOD
C
C        PURPOSE: Process MAXDCONT option for source group contributions
C                 to maximum daily 1-hour values for NO2 or SO2, or
C                 maximum daily (24-hour) values for PM2.5, averaged
C                 across years of met data processed.
C
C        PROGRAMMER: Roger Brode
C
C        DATE:       February 28, 2011
C
C        INPUTS:  Array of High Values
C
C        OUTPUTS: File of High Values for Plotting
C
C        CALLED FROM:   MAIN
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, IVAL, IDEC, IMOD, IGRP1, IGRP2
C JAT 06/22/21 D065 REMOVE NCHR1 AS UNUSED VARIABLE
C      CHARACTER NCHR1(10)*3, NCHR2(10)*5, CHRVAL*5, HDRFRM*400, 
      CHARACTER NCHR2(10)*5, CHRVAL*5, HDRFRM*400,
     &          MXDFMT*100
C Unused:       INTEGER :: J

C     Variable Initializations
C JAT 06/22/21 D065 REMOVE NCHR1 INITIALIZATION AS UNUSED VARIABLE
C      DATA (NCHR1(I),I=1,10) /'YR1','YR2','YR3','YR4','YR5',
C     &                        'YR6','YR7','YR8','YR9','Y10'/
      DATA (NCHR2(I),I=1,10) /'  1ST','  2ND','  3RD','  4TH','  5TH',
     &                        '  6TH','  7TH','  8TH','  9TH',' 10TH'/
      MODNAM = 'MAXDCNT_FILE'

C     Create Header Format for Columns
      WRITE(HDRFRM,9019) NUMGRP, NUMGRP

C --- Generate write format, depending on FILE_FORMAT
      IF (FILE_FORMAT .EQ. 'FIX') THEN
         WRITE(MXDFMT,9001) MIN( NUMGRP, 9999 )
      ELSE IF (FILE_FORMAT .EQ. 'EXP') THEN
         WRITE(MXDFMT,9002) MIN( NUMGRP, 9999 )
      END IF
      
C --- Assign character label for rank
      IF (IVAL .LE. 10) THEN
         CHRVAL = NCHR2(IVAL)
      ELSE IF (MOD(IVAL,100) .GT. 10 .AND. 
     &         MOD(IVAL,100) .LT. 20) THEN
         IDEC = INT(IVAL/10)
         IMOD = MOD(IVAL,10)
         WRITE(CHRVAL,'(I2,I1,"TH")') IDEC, IMOD
      ELSE IF (IVAL .LE. 999) THEN
         IDEC = INT(IVAL/10)
         IMOD = MOD(IVAL,10)
         IF (IMOD .EQ. 0) IMOD = 10
         WRITE(CHRVAL,'(I2,A3)') IDEC, NCHR2(IMOD)(3:5)
      END IF

      IF (.NOT. L_NoHeader(8)) THEN
C        Write Header Information
         WRITE(IMXDCUNT(IGRP1),9005) VERSN, TITLE1(1:68), RUNDAT
         WRITE(IMXDCUNT(IGRP1),9007) C_METVER, RUNTIM, 
     &                          MODOPS_String(1:LEN_TRIM(MODOPS_String))
C
         IF (PM25AVE) THEN
            IF (MAXD_THRESH(IGRP1) .GT. 0.0D0) THEN
               WRITE(IMXDCUNT(IGRP1),9008) CHRVAL,CHRAVE(1),
     &             NUMYRS, GRPID(IGRP1), MAXD_THRESH(IGRP1),
     &             NREC, NUMGRP, MXDFMT
            ELSE
               WRITE(IMXDCUNT(IGRP1),9108) CHRVAL,CHRAVE(1),
     &             NUMYRS, GRPID(IGRP1), NREC, NUMGRP, MXDFMT
            END IF
         ELSE IF (NO2AVE .OR. SO2AVE) THEN
            IF (MAXD_THRESH(IGRP1) .GT. 0.0D0) THEN
               WRITE(IMXDCUNT(IGRP1),9009) CHRVAL,CHRAVE(1),
     &             NUMYRS, GRPID(IGRP1), MAXD_THRESH(IGRP1),
     &             NREC, NUMGRP, MXDFMT
            ELSE
               WRITE(IMXDCUNT(IGRP1),9109) CHRVAL,CHRAVE(1),
     &             NUMYRS, GRPID(IGRP1), NREC, NUMGRP, MXDFMT
            END IF
         END IF
         WRITE(IMXDCUNT(IGRP1),HDRFRM) CHIDEP(1,1),
     &                        CHIDEP(2,1), CHIDEP(3,1), 
     &                       (GRPID(I),I=1,NUMGRP)
      END IF

C --- Begin Receptor LOOP
      DO IREC = 1, NREC
C ---    Write results for specified source group with contributions
C        from other source groups, paired in time and space
         WRITE(IMXDCUNT(IGRP1),MXDFMT,ERR=99) 
     &       AXR_SAV(IREC), AYR_SAV(IREC), 
     &       SUMHNH(IREC,IGRP1,IVAL), AZELEV_SAV(IREC), 
     &       AZHILL_SAV(IREC), AZFLAG_SAV(IREC),
     &       CHRAVE(1), GRPID(IGRP1), CHRVAL, NETID(IREC),
     &       (SUMVAL_MAXD(IVAL,IGRP2,IGRP1,IREC),
     &       IGRP2=1,MIN(NUMGRP,9999))
      END DO
C --- End Receptor LOOP

      GO TO 999

C     WRITE Error Message for Problem Writing to Plot File
 99   WRITE(DUMMY,'("MXDCN",I3.3)') IMXDCUNT(IGRP1)
      CALL ERRHDL(PATH,MODNAM,'E','520',DUMMY)

C --- Generate write formats for FIX and EXP FILE_FORMATs:
 9001 FORMAT('(3(1X,F13.5),3(1X,F8.2),2X,A6,2X,A8,2X,A5,5X,A8,2X,',I4,
     &'(F13.5,2X:))')
 9002 FORMAT('(2(1X,F13.5),1X,E13.6,3(1X,F8.2),2X,A6,2X,A8,2X,A5,5X,A8,'
     &,'2X,',I4,'(E13.6,2X:))')

 9005 FORMAT('* AERMOD (',A6,'): ',A68,5X,A8)
 9007 FORMAT('* AERMET (',A6,'):',T93,A8,
     &      /'* MODELING OPTIONS USED: ',A:)
 9008 FORMAT('*',9X,'MAXDCONT FILE OF ',A5,'-HIGHEST ',A5,
     &       ' VALUES AVERAGED OVER ',I3,' YEARS FOR SOURCE GROUP: ',A8,
     &       ' ; ABOVE THRESH = ',F13.5,
     &      /'*',9X,'FOR A TOTAL OF ',I6,' RECEPTORS AND ',I4,
     &       ' SOURCE GROUPS;  WITH ',
     &       'CONTRIBUTIONS FROM OTHER SOURCE GROUPS PAIRED IN TIME & ',
     &       'SPACE',
     &      /'*',9X,'FORMAT: ',A:)
 9108 FORMAT('*',9X,'MAXDCONT FILE OF ',A5,'-HIGHEST ',A5,
     &       ' VALUES AVERAGED OVER ',I3,' YEARS FOR SOURCE GROUP: ',A8,
     &      /'*',9X,'FOR A TOTAL OF ',I6,' RECEPTORS AND ',I4,
     &       ' SOURCE GROUPS;  WITH ',
     &       'CONTRIBUTIONS FROM OTHER SOURCE GROUPS PAIRED IN TIME & ',
     &       'SPACE',
     &      /'*',9X,'FORMAT: ',A:)
 9009 FORMAT('*',9X,'MAXDCONT FILE OF ',A5,'-HIGHEST MAX DAILY ',A5,
     &       ' VALUES AVERAGED OVER ',I3,' YEARS FOR SOURCE GROUP: ',A8,
     &       ' ; ABOVE THRESH = ',F13.5,
     &      /'*',9X,'FOR A TOTAL OF ',I6,' RECEPTORS AND ',I4,
     &       ' SOURCE GROUPS;  WITH ',
     &       'CONTRIBUTIONS FROM OTHER SOURCE GROUPS PAIRED IN TIME & ',
     &       'SPACE',
     &      /'*',9X,'FORMAT: ',A:)
 9109 FORMAT('*',9X,'MAXDCONT FILE OF ',A5,'-HIGHEST MAX DAILY ',A5,
     &       ' VALUES AVERAGED OVER ',I3,' YEARS FOR SOURCE GROUP: ',A8,
     &      /'*',9X,'FOR A TOTAL OF ',I6,' RECEPTORS AND ',I4,
     &       ' SOURCE GROUPS;  WITH ',
     &       'CONTRIBUTIONS FROM OTHER SOURCE GROUPS PAIRED IN TIME & ',
     &       'SPACE',
     &      /'*',9X,'FORMAT: ',A:)
 9019 FORMAT('(''*'',8X,''X'',13X,''Y'',4X,(2X,3A4),4X,''ZELEV'',4X,
     &''ZHILL'',4X,''ZFLAG'',4X,''AVE'',5X,''GRP'',7X,''RANK'',5X,
     &''NET ID'',1X,',I4,'(2X,''CONT '',A8),/,''*'',
     &3(1X,''____________ ''),1X,3('' ______  ''),
     &''______  ________  ________  ________'',',
     &I4,'(''  _____________''))')

 999  CONTINUE
 
C --- Close and reopen file to flush data from file buffer
      CLOSE(UNIT=IMXDCUNT(IGRP1))
      OPEN(IMXDCUNT(IGRP1),ERR=99,FILE=MAXDCONT_FILE(IGRP1),
     &           IOSTAT=IOERRN,FORM='FORMATTED',STATUS='OLD',
     &           POSITION='APPEND')

      RETURN
      END

