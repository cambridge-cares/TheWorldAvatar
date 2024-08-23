      PROGRAM AERMOD
C=======================================================================
C            MAIN Module of the AMS/EPA Regulatory Model - AERMOD
C                         Draft Version Dated v22xxx
C
C                              April 22, 2021
C
C               *** SEE AERMOD MODEL CHANGE BULLETIN MCB #15 ***
C
C       ON THE SUPPORT CENTER FOR REGULATORY AIR MODELS (SCRAM) WEBSITE
C
C                      https://www.epa.gov/scram
C
C=======================================================================
C
C       This revised version of AERMOD (dated 22xxx) includes numerous
C       updates relative to the previous version (dated 19191);
C       see AERMOD Model Change Bullentin (MCB) #15 and the AERMOD User's
C       Guide.
C
C https://www.epa.gov/scram/air-quality-dispersion-modeling-preferred-and-recommended-models#aermod
C
C-----  MODIFIED BY:    U.S. EPA, OAQPS/AQAD
C                       Air Quality Modeling Group
C
C                       April 22, 2020
C
C-----  MODIFIED FROM:  AERMOD (Version Dated 19191)
C
C=======================================================================
C
C     Variable Declarations
      USE MAIN1
      USE BUOYANT_LINE

      IMPLICIT NONE
      CHARACTER MODNAM*12

C --- Declare character strings for use in the header records of
C     debug files for NO2 options
      CHARACTER (LEN=18) :: NO2DebugString
      CHARACTER (LEN=50) :: NO2DebugStrNonEvt, NO2DebugStrEvent

      INTEGER :: IDSTAT, IASTAT, IBSTAT
C     JAT D065 7/22/21 HIT_THRESH IS SET BUT NOT USED IN THE MAIN PROGRAM
C      LOGICAL :: L_OPENED, HIT_THRESH
      LOGICAL :: L_OPENED
C UnuseD: INTEGER :: I
c     JAT 12/14/17 variables added for getting
c     filenames from command line argument
      integer :: linp, lout, lpre, lerr, levt,i1
      character(len=300) :: modpre, errfil, evtfil
      integer   iargc, status
      character c*300 !JAT changed 30 to 300
C Unused: integer :: len, lstat

C     Variable Initializations
      MODNAM = 'MAIN'
      NO2DebugString    = ''
      NO2DebugStrNonEvt = ''
      NO2DebugStrEvent  = ''

      NTOTHRS = 0
      NYEARS  = 0
      IBSTAT  = 0

      FATAL  = .FALSE.
      RUNERR = .FALSE.
      L_OPENED = .FALSE.
C     JAT 7/22/21 HIT_THRESH SET BUT NOT USED IN MAIN PROGRAM
C      HIT_THRESH = .FALSE.
      L_NoHeader(:) = .FALSE.

C     CRT 8/6/2021 D100 Populate Error/Warning message arrays
      CALL ERRWRNMSG()

c     JAT 12/14/17, added for the command line argument code
!     Set input and output file names based on optional
!     command line arguments and use the base name of
!     the input file as a prefix for other scratch files.
      iargc = command_argument_count()
      if(      iargc .eq. 0 ) then
         inpfil = 'aermod.inp'
         modpre=''
         linp = 10
         outfil = 'aermod.out'
         lout = 10
         lpre=0
      else if( iargc .eq. 1 ) then
         call get_command_argument (1, c, linp, status)
         if (status .ne. 0) then
             write (*,*) 'get_command_argument failed: status = ',
     &       status, ' iargc = ', 1, ' arg = ', 1
             stop
         end if

c        JAT: parse from right to left and look for first
c        '.', which would be the extension.
         outfil=c
         modpre=c
         lpre=linp
         do i1=linp,1,-1
             if (c(i1:i1) .eq. '.' .and. i1 .ne. 1) then
                 outfil=outfil(1:i1-1)
                 modpre=modpre(1:i1-1)
                 lpre=i1-1
                 exit
              endif
         enddo
         write(outfil,'(2(a))')trim(adjustl(outfil)),'.out'
         ! not good if file extension not .inp
c         if ( (linp .le. 4)
c     &        .or. ( c(linp-3:linp-3) .ne. '.' )
c     &        .or. ( c(linp-2:linp-2) .ne. 'i'
c     &               .and. c(linp-2:linp-2) .ne. 'I' )
c     &        .or. ( c(linp-1:linp-1) .ne. 'n'
c     &               .and. c(linp-1:linp-1) .ne. 'N' )
c     &        .or. ( c(linp-0:linp-0) .ne. 'p'
c    &               .and. c(linp-0:linp-0) .ne. 'P' ) ) then
             CALL USAGE
c             write (*,*) ''
c             write (*,*) 'The first argument must be the' //
c     &                   ' input file name ending in .INP'
c             write (*,*) '^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^' //
c     &                   '^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^'
c             stop
c         end if
         inpfil = c(1:linp)

c         ! output file name's extension to match case of input file name's extension
c         outfil = c(1:linp-3)
c         if( c(linp-2:linp-2) .eq. 'i' ) outfil(linp-2:linp-2)='o'
c         if( c(linp-2:linp-2) .eq. 'I' ) outfil(linp-2:linp-2)='O'
c         if( c(linp-1:linp-1) .eq. 'n' ) outfil(linp-1:linp-1)='u'
c         if( c(linp-1:linp-1) .eq. 'N' ) outfil(linp-1:linp-1)='U'
c         if( c(linp-0:linp-0) .eq. 'p' ) outfil(linp-0:linp-0)='t'
c         if( c(linp-0:linp-0) .eq. 'P' ) outfil(linp-0:linp-0)='T'
c         lout = linp
      else if( iargc .eq. 2 ) then
         call get_command_argument (1, c, linp, status)
         if (status .ne. 0) then
             write (*,*) 'get_command_argument failed: status = ',
     &       status, ' iargc = ', 2, ' arg = ', 1
             stop
         end if
         inpfil = c(1:linp)
         modpre=inpfil
         lpre=linp
         do i1=linp,1,-1
             if (c(i1:i1) .eq. '.' .and. i1 .ne. 1) then
                 modpre=modpre(1:i1-1)
                 lpre=i1-1
                 exit
              endif
         enddo
         call get_command_argument (2, c, lout, status)
         if (status .ne. 0) then
             write (*,*) 'get_command_argument failed: status = ',
     &       status, ' iargc = ', 2, ' arg = ', 2
             stop
         end if
         outfil = c(1:lout)
      else
         CALL USAGE
         stop
      end if

C      write(*,*) inpfil(1:linp)//":"
C      write(*,*) inpfil//":"
C      write(*,*) linp,":"
C      write(*,*) outfil(1:lout)//":"
C      write(*,*) outfil//":"
C      write(*,*) lout,":"
c      lpre = 0
C     Open the Temporary File for Error Messages Generated from the Program
c     JAT 12/14/17 added call to uninam to get filename for file formerly
c     called ERMMSG.TMP.  Removing hard-coded filename allows multiple
c     AERMOD runs in same directory
      call uninam( modpre(1:lpre), 'ERRMSG.TMP', errfil, lerr )
      OPEN(UNIT=IERUNT,FILE=errfil(1:lerr),STATUS='REPLACE')

C     Initialize TITLE1 and TITLE2 in case of fatal errors during PRESET phase
      TITLE1 = ' '
      TITLE2 = ' '
C     Get Run Date and Time using Fortran 90 functions      ---   CALL DATIME
      CALL DATIME (RUNDAT, RUNTIM)

C     Open Input and Output Files                           ---   CALL FILOPN
      CALL FILOPN

C     Preprocess Setup Information to Determine Data Storage Needs
      CALL PRESET

C --- Check for FATAL error occurring during PRESET processing, and
C     bypass further processing
      IF (FATAL) GOTO 12345

      IF (NYEARS .EQ. 0) THEN
C ---    Number of years for MAXDCONT arrays has not be specified,
C        set to default value of 5 years
         NYEARS = 5
      END IF

      IF (.NOT. EVONLY) THEN
c       see comments above for temporary error file
         call uninam( modpre(1:lpre), 'EVENT.TMP', evtfil, levt )
         OPEN(UNIT=ITEVUT,FILE=evtfil(1:levt),STATUS='REPLACE')
C        Initialize the Event Counter
         IEVENT = 0
      END IF

C     Allocate SETUP Array Storage
      CALL ALLSETUP

      IF (ALLOC_ERR) THEN
C        Error occurred during allocation of Setup arrays.
C        Issue error messages and ABORT model run.
         WRITE(IOUNIT,*) ' '
         WRITE(IOUNIT,*) '  ERROR OCCURRED DURING ALLOCATION OF SETUP ',
     &                   'ARRAYS! ABORTING MODEL EXECUTION!'
         WRITE(IOUNIT,10901) NSRC,NGRP,NREC,NSEC,NQF,NBF,NPDMAX,NVMAX,
     &                       NURB,NOLM,NPSD,NBLGRP,NNET,IXM,IYM,NAVE,   ! D41_Wood
     &                       NTYP,nval,nhiann,nmax
10901    FORMAT(/'   ARRAY PARAMETER SETTINGS: ',/
     &           '         NSRC   = ', I8,/
     &           '         NGRP   = ', I8,/
     &           '         NREC   = ', I8,/
     &           '         NSEC   = ', I8,/
     &           '         NQF    = ', I8,/
     &           '         NBF    = ', I8,/
     &           '         NPDMAX = ', I8,/
     &           '         NVMAX  = ', I8,/
     &           '         NURB   = ', I8,/
     &           '         NOLM   = ', I8,/
     &           '         NPSD   = ', I8,/
     &           '         NBLGRP = ', I8,/                             ! D41_Wood
     &           '         NNET   = ', I8,/
     &           '         IXM    = ', I8,/
     &           '         IYM    = ', I8,/
     &           '         NAVE   = ', I8,/
     &           '         NTYP   = ', I8,/
     &           '         NVAL   = ', I8,/
     &           '         NHIANN = ', I8,/
     &           '         NMAX   = ', I8)

         WRITE(IOUNIT,*)
         WRITE(IOUNIT,9057) STORE
 9057    FORMAT(/'   Estimated Storage Requirements of Model = ',
     &          F9.1,' MB of RAM.'/)

C        Write error message to terminal
         WRITE(*,*) ' '
         WRITE(*,*)'  ERROR OCCURRED DURING ALLOCATION OF SETUP ',
     &             'ARRAYS! ABORTING MODEL EXECUTION!'
         WRITE(*,9057) STORE

         GO TO 9999
      END IF

C     Variable Initializations                              ---   CALL VARINI
      CALL VARINI

C     Process The Model Setup Information                   ---   CALL SETUP
      IF (EVONLY) THEN
         CALL EV_SETUP
      ELSE
         CALL SETUP
      END IF

C --- Open files for NO2 Debug Options, including PVMRM, OLM,
C     ARM2 and TTRM and write file header information. First
C     define char string explaining order of results for both
C     Non-EVENT and EVENT processing.
      NO2DebugStrNonEvt =
     &   'Hrly non-EVT results sorted by hour/rec/srcgrp/src'
      NO2DebugStrEvent  =
     &   'Hrly & Ave EVT results sorted by day/event/grp/src'

      IF (PVMRM) THEN
         NO2DebugString = 'PVMRM Debug File: '
      ELSE IF (OLM) THEN
         NO2DebugString = 'OLM Debug File:   '
      ELSE IF (ARM2) THEN
         NO2DebugString = 'ARM2 Debug File:  '
      ELSE IF (GRSM) THEN
         NO2DebugString = 'GRSM Debug File: '
      ELSE IF (RUNTTRM) THEN
         NO2DebugString = 'TTRM Debug File:   '
      END IF

c --- Text included in second header record of NO2 option debug files:
c Non-Event runs:
c OLM Debug File:   "Hrly non-EVT results sorted by hour/rec/srcgrp/src"
c ARM2 Debug File:  "Hrly non-EVT results sorted by hour/rec/srcgrp/src"
c PVMRM Debug File: "Hrly non-EVT results sorted by hour/rec/srcgrp/src"
c GRSM Debug File: "Hrly non-EVT results sorted by hour/rec/srcgrp/src"
c Event runs:
c OLM Debug File:   "Hrly & Ave EVT results sorted by day/event/grp/src"
c ARM2 Debug File:  "Hrly & Ave EVT results sorted by day/event/grp/src"
c PVMRM Debug File: "Hrly & Ave EVT results sorted by day/event/grp/src"
c GRSM Debug File: "Hrly & Ave EVT results sorted by day/event/grp/src"

C --- Open file with PVMRM debugging output, but first check
C     for potential file unit conflict
      IF (PVMRMDBG) THEN
         L_OPENED = .FALSE.
         INQUIRE (FILE=DBPVFIL,OPENED=L_OPENED)
         IF( .NOT.L_OPENED )THEN
            INQUIRE (UNIT=PVMDBG,OPENED=L_OPENED)
            IF (.NOT.L_OPENED) THEN
              OPEN(UNIT=PVMDBG,FILE=DBPVFIL,STATUS='REPLACE')
C             Write the standard header information with AERMOD/AERMET
C             versions, 1st title, and rundat/runtim to the debug output file
              WRITE(PVMDBG,9028) VERSN, TITLE1(1:68), RUNDAT
 9028         FORMAT('*** AERMOD - VERSION ',A6,' ***',3X,'*** ',A68,
     &                                                  ' ***',8X,A8)
              IF( .NOT.EVONLY )THEN
C ---          Write header record with column labels for Non-EVENT results
               WRITE(PVMDBG,9029) C_METVER, NO2DebugString,
     &                            NO2DebugStrNonEvt, RUNTIM
 9029          FORMAT('*** AERMET - VERSION ',A6,' ***',3X,'*** ',
     &                                  A18, A50,' ***',8X,A8/)
C              Write the model options (MODOPS) to the debug output file
               WRITE ( PVMDBG, 200 )
     &                        MODOPS_String(1:LEN_TRIM(MODOPS_String))
  200          FORMAT ( ' OPTIONS: ', A /)

               WRITE(PVMDBG,9001)
 9001          FORMAT(1x,'TYPE    DOM_SRCID     DATE    IREC    GRPID',
     &                '   ISRC    SRCID       NUMCONT     DISTDOM     ',
     &                'MAXCONC_NOx     O3CONC        O3MOLES      ',
     &                'NOxMOLES       BHORIZ        BVERT         ',
     &                'BVERT3       ',
     &                'PLUMEVOL      OrigConc  X  PercentNO2 =    ',
     &                'HRVAL         AVEVAL')
              ELSE
C ---          Write header record with column labels for EVENT results
               WRITE(PVMDBG,9029) C_METVER, NO2DebugString,
     &                            NO2DebugStrEvent, RUNTIM
C              Write the model options (MODOPS) to the debug output file
               WRITE ( PVMDBG, 200 )
     &                        MODOPS_String(1:LEN_TRIM(MODOPS_String))
               WRITE(PVMDBG,90011)
90011          FORMAT(1x,'TYPE    DOM_SRCID     DATE    IEVE     ',
     &                'EVENTID    IAVE    GRPID   ISRC    SRCID     ',
     &                'NUMCONT   DISTDOM     MAXCONC_NOx     O3CONC   ',
     &                '     O3MOLES      NOxMOLES       BHORIZ        ',
     &                'BVERT         BVERT3       PLUMEVOL      ',
     &                'OrigConc  X  PercentNO2 =    ',
     &                'HRVAL         AVEVAL')
              ENDIF

            ELSE
C ---          Unit is already opened, issue error message
               CALL ERRHDL(PATH,MODNAM,'E','501','PVMRMDBG')
            END IF
         ELSE
C ---       Unit is already opened, issue error message
            CALL ERRHDL(PATH,MODNAM,'E','501','PVMRMDBG')
         END IF
      END IF

C --- Add separate debug file for relative dispersion coefficients for PVMRM option;
C     the RELDISP debug file will automatically be named RELDISP.DBG
      IF (PVMRMDBG) THEN
         L_OPENED = .FALSE.
         INQUIRE (FILE=RDISPFIL,OPENED=L_OPENED)
         IF( .NOT.L_OPENED )THEN
            INQUIRE (UNIT=RDISPUNT,OPENED=L_OPENED)
            IF (.NOT.L_OPENED) THEN
              OPEN(UNIT=RDISPUNT,FILE=RDISPFIL,STATUS='REPLACE')
C             Write the standard header information with AERMOD/AERMET
C             versions, 1st title, and rundat/runtim to the debug output file
              WRITE(RDISPUNT,9028) VERSN, TITLE1(1:68), RUNDAT
              IF( .NOT.EVONLY )THEN
C ---          Write header record with column labels for Non-EVENT results

               WRITE(RDISPUNT,9029) C_METVER, NO2DebugString,
     &                            NO2DebugStrNonEvt, RUNTIM
C              Write the model options (MODOPS) to the debug output file
               WRITE ( RDISPUNT, 200 )
     &                        MODOPS_String(1:LEN_TRIM(MODOPS_String))

C ---          PVMRM and MODEL DEBUG options have been selected; include the
C              RELDISP Calcs in the RELDISP DEBUG file

              ELSE
C ---          Write header record with column labels for EVENT results
               WRITE(RDISPUNT,9029) C_METVER, NO2DebugString,
     &                            NO2DebugStrEvent, RUNTIM
C              Write the model options (MODOPS) to the debug output file
               WRITE ( RDISPUNT, 200 )
     &                        MODOPS_String(1:LEN_TRIM(MODOPS_String))

C ---          PVMRM and MODEL DEBUG options have been selected; include the
C              RELDISP Calcs in the RELDISP DEBUG file

              END IF
            ELSE
C ---          Unit is already opened, issue error message
               CALL ERRHDL(PATH,MODNAM,'E','501','PVMRMDBG')
            END IF
         ELSE
C ---       Unit is already opened, issue error message
            CALL ERRHDL(PATH,MODNAM,'E','501','PVMRMDBG')
         END IF
      END IF


C --- Open file with OLM debugging output, but first check
C     for potential file unit conflict
      IF (OLMDEBUG) THEN
         L_OPENED = .FALSE.
         INQUIRE (FILE=DBOLMFIL,OPENED=L_OPENED)
         IF (.NOT.L_OPENED) THEN
            INQUIRE (UNIT=OLMDBG,OPENED=L_OPENED)
            IF (.NOT.L_OPENED) THEN
              OPEN(UNIT=OLMDBG,FILE=DBOLMFIL,STATUS='REPLACE')
C             Write the standard header information with AERMOD/AERMET
C             versions, 1st title, and rundat/runtim to the debug output file
              WRITE(OLMDBG,9028) VERSN, TITLE1(1:68), RUNDAT
              IF( .NOT.EVONLY )THEN
                WRITE(OLMDBG,9029) C_METVER, NO2DebugString,
     &                             NO2DebugStrNonEvt, RUNTIM
C               Write the model options (MODOPS) to the debug output file
                WRITE ( OLMDBG, 200 )
     &                        MODOPS_String(1:LEN_TRIM(MODOPS_String))
C ---           Write header record with column labels for Non-EVENT results
                WRITE(OLMDBG,9002)
9002            FORMAT(3X,'DATE    IREC    GRPID   ISRC    SRCID',7X,
     &          'IOLM    OLMID     O3CONC',7X,'OLMVAL',3X,
     &          'X   NO2Ratio  =  NO2VAL',7X,'NO_VAL',6X,'OrigConc  ',
     &          'X  PercentNO2 =  HRVAL',8X,'AVEVAL')

              ELSE
                WRITE(OLMDBG,9029) C_METVER, NO2DebugString,
     &                             NO2DebugStrEvent, RUNTIM
C ---            Write header record with column labels for EVENT results
C                Write the model options (MODOPS) to the debug output file
                 WRITE ( OLMDBG, 200 )
     &                        MODOPS_String(1:LEN_TRIM(MODOPS_String))
                 WRITE(OLMDBG,90021)
90021            FORMAT(3X,'DATE    IEVE   EVENTID   IAVE    GRPID',
     &           3x,'ISRC',4x,'SRCID       IOLM  OLMID       O3CONC',7x,
     &           'OLMVAL   X   NO2Ratio  =  NO2VAL       NO_VAL',6x,
     &           'OrigConc  X  PercentNO2 =  HRVAL       EV_AVEVAL')

             END IF
            ELSE
C ---          Unit is already opened, issue error message
               CALL ERRHDL(PATH,MODNAM,'E','501','OLMDEBUG')
            END IF
         ELSE
C ---       Unit is already opened, issue error message
            CALL ERRHDL(PATH,MODNAM,'E','501','OLMDEBUG')
         END IF
      END IF

C --- Open file with ARM2 debugging output, but first check
C     for potential file unit conflict
      IF (ARM2DEBUG) THEN
         L_OPENED = .FALSE.
         INQUIRE (FILE=DBARM2FIL,OPENED=L_OPENED)
         IF (.NOT.L_OPENED) THEN
             OPEN(UNIT=ARM2DBG,FILE=DBARM2FIL,STATUS='REPLACE')
C            Write the standard header information with AERMOD/AERMET
C            versions, 1st title, and rundat/runtim to the debug output file
             WRITE(ARM2DBG,9028) VERSN, TITLE1(1:68), RUNDAT
             IF( .NOT.EVONLY )THEN
               WRITE(ARM2DBG,9029) C_METVER, NO2DebugString,
     &                            NO2DebugStrNonEvt, RUNTIM
C              Write the model options (MODOPS) to the debug output file
               WRITE ( ARM2DBG, 200 )
     &                        MODOPS_String(1:LEN_TRIM(MODOPS_String))
C ---          Write header record with column labels for Non-EVENT results
               WRITE(ARM2DBG,9004)
9004           FORMAT(3X,'DATE',4x,'IREC    GRPID   ISRC    SRCID',11x,
     &                   'NOXCONC',5x,'OrigConc  X  ARM2Ratio =',3x,
     &                   'HRVAL',8x,'AVEVAL')
             ELSE
               WRITE(ARM2DBG,9029) C_METVER, NO2DebugString,
     &                            NO2DebugStrEvent, RUNTIM
C              Write the model options (MODOPS) to the debug output file
               WRITE ( ARM2DBG, 200 )
     &                        MODOPS_String(1:LEN_TRIM(MODOPS_String))
C ---          Write header record with column labels for EVENT results
               WRITE(ARM2DBG,90041)
90041          FORMAT(3X,'DATE',4x,'IEVE   EVENTID   IAVE    GRPID',3x,
     &              'ISRC',4x,'SRCID',11x,'NOXCONC',6x,'OrigConc X  ',
     &              'ARM2Ratio =',4x,'HRVAL',6x,'EV_AVEVAL')
             END IF
         ELSE
C ---       Unit is already opened, issue error message
            CALL ERRHDL(PATH,MODNAM,'E','501','ARM2DEBUG')
         END IF
      END IF

C     CERC 11/30/20
C --- Open file with GRSM debugging output, but first check
C     for potential file unit conflict
      IF (GRSMDEBUG) THEN
         L_OPENED = .FALSE.
         INQUIRE (FILE=DBGRSMFIL,OPENED=L_OPENED)
         IF (.NOT.L_OPENED) THEN
            INQUIRE (UNIT=GRSMDBG,OPENED=L_OPENED)
            IF (.NOT.L_OPENED) THEN
              OPEN(UNIT=GRSMDBG,FILE=DBGRSMFIL,STATUS='REPLACE')
C             Write the standard header information with AERMOD/AERMET
C             versions, 1st title, and rundat/runtim to the debug output file
              WRITE(GRSMDBG,9028) VERSN, TITLE1(1:68), RUNDAT
              IF( .NOT.EVONLY )THEN
                WRITE(GRSMDBG,9029) C_METVER, NO2DebugString,
     &                             NO2DebugStrNonEvt, RUNTIM
C               Write the model options (MODOPS) to the debug output file
                WRITE ( GRSMDBG, 200 )
     &                        MODOPS_String(1:LEN_TRIM(MODOPS_String))
C ---           Write header record with column labels for Non-EVENT results
                WRITE(GRSMDBG,90023)
90023           FORMAT(1X,'DATE (YYMMDDHH)',2X,'  IREC',2X,'GRPID   ',
     &          2X,'  ISRC',2X,'SRCID       ',2X,'NIGHT-TIME?',2X,
     &          'O3 BGD BEFORE (PPB)',2X,'NOX BGD BEFORE (PPB)',2X,
     &          'NO2 BGD BEFORE (PPB)',2X,'TRAVEL TIME (S)',2X,
     &          'CONC x TRAVEL TIME (MICROG.S/M3)',2X,
     &          'PRIMARY NO2 FRAC',2X,'ORIG NOX CONC (MICROG/M3)',2X,
     &          'NO2 FRAC AFTER',2X,'NO2 BGD AFTER (PPB)',2X,
     &          'HRVAL (MICROG/M3)',2X,'AVEVAL (MICROG/M3)',2X,
     &          'INST. PL. AREA (M2)',2X,'ENSEMBLE PL. AREA (M2)')
              ELSE
                WRITE(GRSMDBG,9029) C_METVER, NO2DebugString,
     &                             NO2DebugStrEvent, RUNTIM
C ---           Write header record with column labels for EVENT results
C               Write the model options (MODOPS) to the debug output file
                WRITE ( GRSMDBG, 200 )
     &                        MODOPS_String(1:LEN_TRIM(MODOPS_String))
                WRITE(GRSMDBG,90025)
90025           FORMAT(1X,'DATE (YYMMDDHH)',2X,'  IEVE',2X,'EVENTID   ',
     &          2X,'IAVE',2X,'GRPID   ',2X,
     &          '  ISRC',2X,'SRCID       ',2X,'NIGHT-TIME?',2X,
     &          'O3 BGD BEFORE (PPB)',2X,'NOX BGD BEFORE (PPB)',2X,
     &          'NO2 BGD BEFORE (PPB)',2X,'TRAVEL TIME (S)',2X,
     &          'CONC x TRAVEL TIME (MICROG.S/M3)',2X,
     &          'PRIMARY NO2 FRAC',2X,'ORIG NOX CONC (MICROG/M3)',2X,
     &          'NO2 FRAC AFTER',2X,'NO2 BGD AFTER (PPB)',2X,
     &          'HRVAL (MICROG/M3)',2X,'EV_AVEVAL (MICROG/M3)',2X,
     &          'INST. PL. AREA (M2)',2X,'ENSEMBLE PL. AREA (M2)')
             END IF
            ELSE
C ---          Unit is already opened, issue error message
               CALL ERRHDL(PATH,MODNAM,'E','501','GRSMDEBUG')
            END IF
         ELSE
C ---       Unit is already opened, issue error message
            CALL ERRHDL(PATH,MODNAM,'E','501','GRSMDEBUG')
         END IF
      END IF

C --- Write the model options and debug data template to the
C     debug file if MODEL is specified on the DEBUGOPT keyword
      IF( DEBUG )THEN
C        Write the standard header information with AERMOD/AERMET
C        versions, 1st title, and rundat/runtim to the debug output file
         WRITE(DBGUNT,9028) VERSN, TITLE1(1:68), RUNDAT
         IF( EVONLY )THEN
C ---       Write header information for EVENT results
            WRITE(DBGUNT,90291) C_METVER,
     &                  'MODEL Debug File: EVENT Processing   ', RUNTIM
         ELSE
C ---       Write header information for Non-EVENT results
            WRITE(DBGUNT,90291) C_METVER,
     &                  'MODEL Debug File: NonEVENT Processing', RUNTIM
         ENDIF
90291    FORMAT('*** AERMET - VERSION ',A6,' ***',3X,'*** ',
     &                             A37,31X,' ***',8X,A8/)
C ---    Include string of MODEL Options in DEBUG file header
         WRITE (DBGUNT, 200)  MODOPS_String(1:LEN_TRIM(MODOPS_String))
      END IF

C --- Write the model options and debug data template to the
C     debug file if METEOR is specified on the DEBUGOPT keyword
      IF( METEORDBG )THEN
         WRITE(DBMUNT,9028) VERSN, TITLE1(1:68), RUNDAT
         IF( EVONLY )THEN
C ---       Write header information for EVENT results
            WRITE(DBMUNT,90292) C_METVER,
     &              'METEOR Debug File: EVENT Processing   ', RUNTIM
         ELSE
C ---       Write header information for Non-EVENT results
            WRITE(DBMUNT,90292) C_METVER,
     &              'METEOR Debug File: NonEVENT Processing', RUNTIM
         ENDIF
90292    FORMAT('*** AERMET - VERSION ',A6,' ***',3X,'*** ',
     &                                 A38,30X,' ***',8X,A8/)
C ---    Include string of MODEL Options in DEBUG file header
         WRITE (DBMUNT, 200)  MODOPS_String(1:LEN_TRIM(MODOPS_String))
      END IF

C --- Write the model options and debug data template to the
C     debug file if PRIME is specified on the DEBUGOPT keyword
C     and model run includes sources with building downwash (NSEC > 0)
      IF (PRIMEDBG .AND. NSEC .GT. 0) THEN
C ---    Write main header records for PRIME debug file
         WRITE(PRMDBUNT,9028) VERSN, TITLE1(1:68), RUNDAT
         IF( EVONLY )THEN
C ---       Write header information for EVENT results
            WRITE(PRMDBUNT,90293) C_METVER,
     &                  'PRIME Debug File: EVENT Processing   ', RUNTIM
         ELSE
C ---       Write header information for NonEVENT results
            WRITE(PRMDBUNT,90293) C_METVER,
     &                  'PRIME Debug File: NonEVENT Processing', RUNTIM
         ENDIF
90293    FORMAT('*** AERMET - VERSION ',A6,' ***',3X,'*** ',
     &                              A37,31X,' ***',8X,A8/)
C        Write the model options (MODOPS) to the debug output file
         WRITE ( PRMDBUNT,200 ) MODOPS_String(1:LEN_TRIM(MODOPS_String))
      END IF

C --- Write the model options and debug data template to the
C     debug file if AREA/LINE is specified on the DEBUGOPT keyword
C     and model run includes AREA, LINE, or OPENPIT sources
      IF( AREADBG .AND. (NAREA.GT.0 .OR. NCIRC.GT.0 .OR.
     &                   NLINE.GT.0 .OR. NPIT.GT.0) )THEN
C ---    Write main header records for METEOR debug file, then reset METDBGHDR = .F.
         WRITE(AREADBUNT,9028) VERSN, TITLE1(1:68), RUNDAT
         IF( EVONLY )THEN
C ---       Write header information for EVENT results
            WRITE(AREADBUNT,90294) C_METVER,
     &                    'AREA Debug File: EVENT Processing   ', RUNTIM
         ELSE
C ---       Write header information for NonEVENT results
            WRITE(AREADBUNT,90294) C_METVER,
     &                    'AREA Debug File: NonEVENT Processing', RUNTIM
         ENDIF
90294    FORMAT('*** AERMET - VERSION ',A6,' ***',3X,'*** ',
     &                                 A36,32X,' ***',8X,A8/)
C        Write the model options (MODOPS) to the debug output file
         WRITE( AREADBUNT,200 ) MODOPS_String(1:LEN_TRIM(MODOPS_String))
      END IF

!     Added for TTRM, AECOM - Feb. 2021; Modified Nov. 2021
      IF (TTRMDBG) THEN
        IF( .NOT.EVONLY )THEN
          WRITE ( TTRMUNT, 6010 )
 6010     FORMAT (' KURDAT,XREC,YREC,SRCGRP,SRCID,O3CONC (μg/m3),',
     &'O3CONC (ppb),Amb. Temp. (K),k1, time coefficent,Distance (m),',
     &'Effective WSPD (m/s),Transit Time (s),PRIME Eff. WSPD (m/s),',
     &'PRIME Transit Time (s),Plume Type,Downwind Distance,',
     &'Radial Distance,Crosswind Distance,UEFF,UEFFD,UEFF3,FRAN,PPF,',
     &'GAMFACT,CHI-Indirect,CHI-DIRECT,TOTAL NO,Instack-NO2,',
     &'Available NO (as NO2),TTRM FRACTIONAL,TTRM Converted NO2,',
     &'Source Sub-total NO2')
        ELSE IF(EVONLY)THEN
          WRITE ( TTRMUNT, 6011 )
 6011     FORMAT (' KURDAT,XREC,YREC,ievent,EVNAME,EVAPER,',
     &'SRCGRP,SRCID,O3CONC (μg/m3),',
     &'O3CONC (ppb),Amb. Temp. (K),k1, time coefficent,Distance (m),',
     &'Effective WSPD (m/s),Transit Time (s),PRIME Eff. WSPD (m/s),',
     &'PRIME Transit Time (s),Plume Type,Downwind Distance,',
     &'Radial Distance,Crosswind Distance,UEFF,UEFFD,UEFF3,FRAN,PPF,',
     &'GAMFACT,CHI-Indirect,CHI-DIRECT,TOTAL NO,Instack-NO2,',
     &'Available NO (as NO2),TTRM FRACTIONAL,TTRM Converted NO2,',
     &'Source Sub-total NO2')
        END IF
      END IF
!     End of TTRM insert

CRCO D095 Added for urban debug 8/3/2021
      IF (URBDBUG) THEN
C        Write the title(s) to the debug output file
         WRITE ( URBUNT, 611 )
         WRITE ( URBUNT, 110 ) TITLE1(1:68), TITLE2(1:68)
         WRITE ( URBUNT,"(' ')")
  110    FORMAT ( ' Title: ',A68,/,'        ',A68,/)
  611    FORMAT (' * * * * * * * URBAN    DEBUG FILE * * * * * * * ',//)
         WRITE( URBUNT, 2222 )
 2222    FORMAT (1X,'URBAN METEOROLOGY OUTPUT',//,
     & 1X,'IURB',2x, 'YR',2X,'MO',2X,'DY',2X,'HR',
     & 4X,'URBOBLEN',3X,'URBOBLSAV',3X,'RUROBULEN',4X,'URBUSTAR',
     & 2x,'URBUSTRSAV',5X,'RURUSTR',
     & 6X,'DELTUR',7X,'ZIURB',6X,'ZIMECH',6X,'ZICONV',6X,'URBPOP',
     & 7X,'URBHF',5X,'URBWSTR',7X,'WSTAR',
     & 4X,'Amb_Temp',8X,'UREF', 7X,'BOWEN',3X,'STABLE',3x,'MornTrns')

C Profile info for turbulence values
         WRITE ( URBUNT1, 611 )
         WRITE ( URBUNT1, 110 ) TITLE1(1:68), TITLE2(1:68)
         WRITE ( URBUNT1,"(' ')")
         WRITE( URBUNT1, 2223 )
 2223    FORMAT (1X,'URBAN METEOROLOGY OUTPUT',//,
     & 1X,'IURB',2x, 'YR',2X,'MO',2X,'DY',2X,'HR',1x,'LVL',4X,'GRIDHT',
     & 4X,'GRIDSv',4x,'SvCURB',4X,'GRDSvU',
     & 4X,'GRIDSw',4X,'SwCURB',4X,'GRDSwU')



CRCO D095 Added for BLP debug 12/8/21
      IF (BLPDBUG) THEN
C        Write the title(s) to the debug output file
         WRITE ( BLPUNT, 612 )
         WRITE ( BLPUNT, 111 ) TITLE1(1:68), TITLE2(1:68)
         WRITE ( BLPUNT,"(' ')")
  111    FORMAT ( ' Title: ',A68,/,'        ',A68,/)
  612    FORMAT (' * * * * * * * BLP-RISE DEBUG FILE * * * * * * * ',//)
         WRITE( BLPUNT, 2224 )
 2224    FORMAT (1X,'PLUME RISE HEIGHTS AND DISTANCES OUTPUT',//,
     & 2X,'SRCID',8x,
     & 'YR',2X,'MO',2X,'DY',2X,'HR',5X,'DH1',5X,'DH2',5X,'DH3',5X,
     & 'DH4',5X,'DH5',5X,'DH6',5X,'DH7',5X,'XF1',5X,'XF2',5X,'XF3',5X,
     & 'XF4',5X,'XF5',5X,'XF6',5X,'XF7',7X,'XFB',5X,'XFS',1X,'STAB',
     & 1X,'URBOBLEN',3X,'OBULEN',7X,'Hs',1X,'FINAL_HT')
      ENDIF

      ENDIF

C     Open file for GDEP output from gas dry deposition algorithms,
C     but first check for potential file unit conflict
      IF ((DEBUG .OR. DEPOSDBG) .AND. LDGAS) THEN
         L_OPENED = .FALSE.
         INQUIRE (FILE='GDEP.DAT',OPENED=L_OPENED)
         IF (.NOT.L_OPENED) THEN
            INQUIRE (UNIT=GDEPDBG,OPENED=L_OPENED)
            IF (.NOT.L_OPENED) THEN
               OPEN(UNIT=GDEPDBG,FILE='GDEP.DAT',STATUS='REPLACE')
            ELSE
C ---          Unit is already opened, issue error message
               CALL ERRHDL(PATH,MODNAM,'E','501','GDEP.DAT')
               RUNERR = .TRUE.
            END IF
         ELSE
C ---       Unit is already opened, issue error message
            CALL ERRHDL(PATH,MODNAM,'E','501','GDEP.DAT')
            RUNERR = .TRUE.
         END IF
      END IF

C     Open file for PDEP output from particle dry deposition algorithms,
C     but first check for potential file unit conflict
      IF ((DEBUG .OR. DEPOSDBG) .AND. LDPART) THEN
         L_OPENED = .FALSE.
         INQUIRE (FILE='PDEP.DAT',OPENED=L_OPENED)
         IF (.NOT.L_OPENED) THEN
            INQUIRE (UNIT=PDEPDBG,OPENED=L_OPENED)
            IF (.NOT.L_OPENED) THEN
               OPEN(UNIT=PDEPDBG,FILE='PDEP.DAT',STATUS='REPLACE')
            ELSE
C ---          Unit is already opened, issue error message
               CALL ERRHDL(PATH,MODNAM,'E','501','PDEP.DAT')
               RUNERR = .TRUE.
            END IF
         ELSE
C ---       Unit is already opened, issue error message
            CALL ERRHDL(PATH,MODNAM,'E','501','PDEP.DAT')
            RUNERR = .TRUE.
         END IF
      END IF

C     Deallocate Temporary Storage
      DEALLOCATE  (IWRK2, STAT=IDSTAT)
      IF (IDSTAT .NE. 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','409','Setup Arrays')
      END IF
      IF (.NOT. EVONLY) THEN
         DEALLOCATE  (ZETMP1,ZETMP2,ZHTMP1,ZHTMP2,ZFTMP1,ZFTMP2,
     &                STAT=IDSTAT)
         IF (IDSTAT .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','409','Setup Arrays')
         END IF
      END IF

C     Allocate Array Storage for Results                    ---   CALL ALLRESULT
      CALL ALLRESULT

      IF (ALLOC_ERR) THEN
C        Error occurred during allocation of Results arrays.
C        Issue error message and skip initialization of results arrays.
         WRITE(IOUNIT,*) ' '
         WRITE(IOUNIT,*) '  ERROR OCCURRED DURING ALLOCATION OF RESULT',
     &                   ' ARRAYS!'
         WRITE(IOUNIT,10902) NSRC,NGRP,NREC,NSEC,NQF,NBF,NPDMAX,NVMAX,
     &                       NURB,NOLM,NPSD,NBLGRP,NNET,IXM,IYM,NAVE,   ! D41_Wood
     &                       NTYP,NHIVAL,NHIANN,NMXVAL,NYEARS
10902    FORMAT(/'   ARRAY PARAMETER SETTINGS: ',/
     &           '         NSRC   = ', I8,/
     &           '         NGRP   = ', I8,/
     &           '         NREC   = ', I8,/
     &           '         NSEC   = ', I8,/
     &           '         NQF    = ', I8,/
     &           '         NBF    = ', I8,/
     &           '         NPDMAX = ', I8,/
     &           '         NVMAX  = ', I8,/
     &           '         NURB   = ', I8,/
     &           '         NOLM   = ', I8,/
     &           '         NPSD   = ', I8,/
     &           '         NBLGRP = ', I8,/                             ! D41_Wood
     &           '         NNET   = ', I8,/
     &           '         IXM    = ', I8,/
     &           '         IYM    = ', I8,/
     &           '         NAVE   = ', I8,/
     &           '         NTYP   = ', I8,/
     &           '         NHIVAL = ', I8,/
     &           '         NHIANN = ', I8,/
     &           '         NMXVAL = ', I8,/
     &           '         NYEARS = ', I8)

         WRITE(IOUNIT,*)
         WRITE(IOUNIT,9057) STORE

C        Write error message to terminal
         WRITE(*,*) ' '
         WRITE(*,*) '  ERROR OCCURRED DURING ALLOCATION OF RESULT',
     &              ' ARRAYS!'
         WRITE(*,9057) STORE

         GO TO 9999

      ELSE IF (.NOT. EVONLY) THEN
C        No Errors During Allocation of Results Arrays
C        Initialize Results Arrays With Zeroes              ---   CALL RESINI
         CALL RESINI
      END IF

C     Determine Number of Setup Messages by Message Type    ---   CALL TERRST
      CALL TERRST

c --- Set up common for PRIME numerical rise algorithm      ---   CALL NUMPR1
      CALL NUMPR1

c --- Set up common for PRIME building cavity model         ---   CALL PRIME1
      CALL PRIME1

      IF (.NOT.RUN .OR. FATAL .OR. IWRN .GT. 0) THEN
C        Write Out Summary Of Setup Error/Message Stats     ---   CALL SUMTBL
         WRITE(IOUNIT,9111)
 9111    FORMAT(//2X,'*** Message Summary For AERMOD Model Setup ***'/)
         CALL SUMTBL(IOUNIT)
      END IF

12345 continue

      IF (FATAL) THEN
         WRITE(*,99111)
99111    FORMAT('+','Fatal Error Occurred During Setup Phase!')
         WRITE(IOUNIT,9112)
 9112    FORMAT(/4X,'**************************************',
     &          /4X,'*** SETUP Finishes UN-successfully ***',
     &          /4X,'**************************************'/)
      ELSE
         WRITE(IOUNIT,9113)
 9113    FORMAT(/1X,'***********************************',
     &          /1X,'*** SETUP Finishes Successfully ***',
     &          /1X,'***********************************'/)

C        Print Summary of the Input Data                       ---   CALL INPSUM
         CALL INPSUM

C        Write Headers to GDEP.DAT and PDEP.DAT Files for new deposition algorithms
         IF ((DEBUG .OR. DEPOSDBG) .AND. LDGAS) THEN
C           Write the model options (MODOPS) to the debug output file
            WRITE ( GDEPDBG, 200 )
     &                      MODOPS_String(1:LEN_TRIM(MODOPS_String))
            WRITE(GDEPDBG,9901)
 9901       FORMAT(1X,'YYMMDDHH',3X,'ISRC',4X,'Ra',12X,'Rb',12X,'Rc',
     &             12X,'Vdepg')
         END IF
         IF ((DEBUG .OR. DEPOSDBG) .AND. LDPART) THEN
            WRITE(PDEPDBG,9902)
C           Write the model options (MODOPS) to the debug output file
            WRITE ( PDEPDBG, 200 )
     &                      MODOPS_String(1:LEN_TRIM(MODOPS_String))
 9902       FORMAT(1X,'YYMMDDHH',3X,'ISRC',1X,'ICAT',2X,'Method No.',
     &             3X,'Ra',12X,'Rp',12X,'Vg(i)',9x,'Vdep(i)')
         END IF

      END IF

      IF (.NOT.FATAL .AND. RUN .AND. EVONLY) THEN
C        No Fatal Errors in Setup and RUN Option Selected and EVENT Processing

C        Process The Data For Each Event                    ---   CALL EVLOOP
         CALL EVLOOP

      ELSE IF (.NOT.FATAL .AND. RUN .AND. .NOT.EVONLY) THEN
C        No Fatal Errors in Setup and RUN Option Selected and Normal Processing

C        Reinitialize Results Arrays With Zeroes            ---   CALL RESINI
         CALL RESINI

         IF (RSTINP) THEN
C           Initialize Results Arrays from Re-start File    ---   CALL RSINIT
            CALL RSINIT
         END IF

C        Process The Hourly Meteorological Data             ---   CALL HRLOOP
         CALL HRLOOP

C ---    Check total precipitation if wet deposition is being used
         IF ((WDPLETE .OR. DEPOS .OR. WDEP) .AND.
     &                                TOTAL_PRECIP .LT. 0.0001D0) THEN
C ---       Write warning message for no precip with wet deposition
            CALL ERRHDL(PATH,MODNAM,'W','496','WetDepos')
         END IF

         IF ((PM25AVE .OR. NO2AVE .OR. SO2AVE .OR. ANNUAL)
     &                                           .AND. MULTYR
     &                                           .AND. .NOT.RUNERR) THEN
C ---       Results arrays for MULTYEAR applications WITH ANNUAL average,
C           or other outputs averaged across years, need to be "dumped" to
C           SAVEFILE BEFORE calculating averages
C ---                                                       ---   CALL RSDUMP
            CALL RSDUMP

            IF (SEASONHR .AND. .NOT.RUNERR) THEN
C ---          Calculate averages for season by hour-of-day results
               IF (CONC) THEN
                  CALL SHAVE
C ---             Check for values exceeding fixed-format field width (F13.8)
C                 without FILE_FORMAT = 'EXP'
                  IF (FILE_FORMAT .NE. 'EXP' .AND.
     &                         MAXVAL(SHVALS) .GT. 9999.99999999D0) THEN
                     CALL ERRHDL(PATH,MODNAM,'W','400','= EXP')
                  END IF
               END IF
            END IF

         END IF

         IF ( (PM25AVE .OR. NO2AVE .OR. SO2AVE .OR. ANNUAL)
     &                                           .AND. .NOT.RUNERR) THEN
C ---       Compute averages of the High-N-High 24-hr PM25, 1-hr NO2,
C           1-hr SO2, and annual values
            IF (NUMYRS .GT. 0) THEN
               DO IGRP = 1, NUMGRP
                  DO IREC = 1, NUMREC
                     IF (PM25AVE .OR. NO2AVE .OR. SO2AVE) THEN
                        SUMHNH(IREC,IGRP,1:NHIVAL) =
     &                  SUMHNH(IREC,IGRP,1:NHIVAL) / DBLE(NUMYRS)
                     END IF
                     IF (ANNUAL) THEN
                        DO ITYP = 1, NUMTYP
                           ANNVAL(IREC,IGRP,ITYP) =
     &                                 SUMANN(IREC,IGRP,ITYP) /
     &                                                      DBLE(NUMYRS)
                        END DO
                     END IF
                  END DO
               END DO
            ELSE
               IF( ANNUAL )THEN
C                 Write Error Message: Number of Years = 0 for ANNUAL
                  CALL ERRHDL(PATH,MODNAM,'E','480','NUMYRS=0')
                  RUNERR = .TRUE.
               ELSEIF( NO2AVE )THEN
C                 Write Error Message: Need complete years for 1-hr SO2/NO2 and 24-hr PM25
                  CALL ERRHDL(PATH,MODNAM,'E','480','1hr NO2AVE')
                  RUNERR = .TRUE.
               ELSEIF( SO2AVE )THEN
C                 Write Error Message: Need complete years for 1-hr SO2/NO2 and 24-hr PM25
                  CALL ERRHDL(PATH,MODNAM,'E','480','1hr SO2AVE')
                  RUNERR = .TRUE.
               ELSEIF( PM25AVE )THEN
C                 Write Error Message: Need complete years for 1-hr SO2/NO2 and 24-hr PM25
                  CALL ERRHDL(PATH,MODNAM,'E','480','24hr PM25AVE')
                  RUNERR = .TRUE.
               END IF
            END IF
            IF (NREMAIN .NE. 0) THEN
C              Write Warning Message: Met Data Remains After End of Last Year
               IF (.NOT. L_SkipMessages) THEN
                  WRITE(DUMMY,'(I8)') NREMAIN
                  CALL ERRHDL(PATH,MODNAM,'W','481',DUMMY)
               END IF
            END IF
         END IF

         IF ((PERIOD.OR.ANNUAL) .AND. (.NOT. RUNERR) .AND.
     &                                                NTOTHRS.GT.0) THEN
C ---       PERIOD Average Selected and No Runtime/Meteorology Errors
            IF (CONC .AND. PERIOD) THEN
C              Calculate Period Average Concentrations      ---   CALL PERAVE
               CALL PERAVE
            END IF
C ---       Check for values exceeding fixed-format field width (F13.5)
C           without FILE_FORMAT = 'EXP'
            IF (FILE_FORMAT .NE. 'EXP' .AND.
     &                        MAXVAL(ANNVAL) .GT. 9999999.99999D0) THEN
               CALL ERRHDL(PATH,MODNAM,'W','400','= EXP')
            END IF
            DO ITYP = 1, NUMTYP
C              Select Highest PERIOD Values by Source Group ---   CALL HIPER
               CALL HIPER
            END DO
C          JAT 9/21/2017: MODIFIED CALL TO PSTANN TO ONLY WHEN PERIOD AVERAGES
C          TO AVOID WRITING ANNUAL AVERAGE ACROSS MODELED PERIOD TO ANNUAL
C          POSTFILE WHEN ONLY INDIVIDUAL YEARS SHOULD BE WRITTEN TO ANNUAL POSTFILE
            IF (ANPOST .AND. PERIOD) THEN
C              Write PERIOD/ANNUAL Results to Post File     ---   CALL PSTANN
               CALL PSTANN
            END IF
            IF (ANPLOT) THEN
C              Write PERIOD/ANNUAL Results to Plot File     ---   CALL PLTANN
               CALL PLTANN
            END IF
         END IF

         IF (MULTYR .AND. .NOT.RUNERR .AND.
     &                    .NOT.(ANNUAL .OR. PM25AVE .OR. NO2AVE .OR.
     &                                       SO2AVE)) THEN
C ---       Results arrays for MULTYEAR applications WITHOUT ANNUAL average,
C           or other outputs averaged across years, need to be "dumped" to
C           SAVEFILE AFTER calculating averages
C ---                                                       ---   CALL RSDUMP
            CALL RSDUMP
         END IF

         IF (.NOT.(MULTYR .AND. (ANNUAL .OR. PM25AVE .OR.
     &                                        NO2AVE .OR.
     &                                        SO2AVE) ) .AND.
     &                         SEASONHR .AND. .NOT.RUNERR) THEN
            IF (CONC) THEN
               CALL SHAVE
C ---          Check for values exceeding fixed-format field width (F13.8)
C              without FILE_FORMAT = 'EXP'
               IF (FILE_FORMAT .NE. 'EXP' .AND.
     &                         MAXVAL(SHVALS) .GT. 9999.99999999D0) THEN
                  CALL ERRHDL(PATH,MODNAM,'W','400','= EXP')
               END IF
            END IF
         END IF

         IF (PLFILE .AND. (.NOT. RUNERR)) THEN
C           Write Short Term High Values to Plot File       ---   CALL PLOTFL
C ---       Check for values exceeding fixed-format field width (F13.5)
C           without FILE_FORMAT = 'EXP'
            IF (FILE_FORMAT .NE. 'EXP') THEN
               IF (PM25AVE .OR. NO2AVE .OR. SO2AVE) THEN
                  IF (MAXVAL(SUMHNH) .GT. 9999999.99999D0) THEN
                     CALL ERRHDL(PATH,MODNAM,'W','400','= EXP')
                  END IF
               ELSE IF (.NOT.PM25AVE .AND. .NOT.NO2AVE .AND. .NOT.SO2AVE
     &                      .AND.MAXVAL(HIVALU).GT.9999999.99999D0) THEN
                  CALL ERRHDL(PATH,MODNAM,'W','400','= EXP')
               END IF
            END IF
C ---       Call plotfile routine
            CALL PLOTFL
         END IF

         IF (.NOT. RUNERR) THEN
C ---       Check for values exceeding fixed-format field width (F13.5)
C           without FILE_FORMAT = 'EXP'
            IF (.NOT.PLFILE .AND. FILE_FORMAT .NE. 'EXP') THEN
               IF (PM25AVE .OR. NO2AVE .OR. SO2AVE) THEN
                  IF (MAXVAL(SUMHNH) .GT. 9999999.99999D0) THEN
                     CALL ERRHDL(PATH,MODNAM,'W','400','= EXP')
                  END IF
               ELSE IF (.NOT.PM25AVE .AND. .NOT.NO2AVE .AND. .NOT.SO2AVE
     &                      .AND.MAXVAL(HIVALU).GT.9999999.99999D0) THEN
                  CALL ERRHDL(PATH,MODNAM,'W','400','= EXP')
               END IF
            END IF
C           Print Out Model Results                         ---   CALL OUTPUT
            CALL OUTPUT
         END IF

C ---    Check for MAXDCONT options to evaluate source group contributions
C        based on rank for PM2.5 24hr, NO2 1hr or SO2 1hr NAAQS
         IF (.NOT. RUNERR .AND.
     &         L_MAXDCONT .AND.
     &            (PM25AVE .OR. NO2AVE .OR. SO2AVE) ) THEN

            IF (PVMRMDBG) THEN
C ---          PVMRM Debug option selected; print header record to delimit
C              debug information related to MAXDCONT processing
               WRITE(PVMDBG,9001)
            END IF

C ---       Allocate arrays to save receptor data;
C           also allocate array to store summed
C           contributions for max daily 1-hour averages
            ALLOCATE  (AXR_SAV(NREC), AYR_SAV(NREC),
     &           AZELEV_SAV(NREC), AZFLAG_SAV(NREC),
     &           AZHILL_SAV(NREC),
     &           SUMVAL_MAXD(NVAL,NGRP,NGRP,NREC),
     &           STAT=IASTAT)
            IF (IASTAT .NE. 0) THEN
               CALL ERRHDL(PATH,MODNAM,'E','409','Setup Arrays')
               ALLOC_ERR = .TRUE.
               WRITE(IOUNIT,*) '  Error Occurred During Allocation ',
     &                         'Rec Arrays for MAXDCONT!'
            END IF

C (Multiple_BuoyLines_D41_Wood)
C           Added second dimension to arrays for multiple buoyant lines
            IF (L_BLSOURCE) THEN
              ALLOCATE (XR_SCS_SAV(NREC,NBLGRP),
     &                  YR_SCS_SAV(NREC,NBLGRP),
     &                  BL_RFLAG_SAV(NREC,NBLGRP), STAT=IBSTAT)
            ENDIF
            IF (IBSTAT .NE. 0) THEN
               CALL ERRHDL(PATH,MODNAM,'E','409','Setup Arrays')
               ALLOC_ERR = .TRUE.
               WRITE(IOUNIT,*) '  Error Occurred During Allocation ',
     &                         'BuoyLine Rec Arrays for MAXDCONT!'
            END IF

            IF( IASTAT .NE. 0 .OR. IBSTAT .NE. 0) THEN
               GO TO 9999
            END IF

            CALL MAXDCONT_LOOP

         END IF

      ELSE
C ---    FATAL error occurred during PRESETUP phase; initialize MODOPS string
C        before call to HEADER to avoid runtime error for undefined string
         MODOPS_String = ''

      END IF

C     Determine Number of Errors/Messages by Message Type   ---   CALL TERRST
      CALL TERRST

C     Write Summary of Message Stats for Model Execution    ---   CALL SUMTBL
      CALL HEADER(IOUNIT)
      WRITE(IOUNIT,9114)
 9114 FORMAT(/1X,'*** Message Summary : AERMOD Model Execution ***'/)

      CALL SUMTBL(IOUNIT)

      IF (SUMMFILE) THEN
C        Write Summary of Messages to optional SUMMFILE
         CALL HEADER(ISUMUNT)
         WRITE(ISUMUNT,9114)
         CALL SUMTBL(ISUMUNT)
      END IF

C     Skip to here if error occurs during allocation of arrays
 9999 CONTINUE

      IF (FATAL .OR. RUNERR) THEN
         IF (RUNERR) THEN
            WRITE(*,99112)
99112       FORMAT('+','Fatal Error Occurred During Runtime Phase!')
         END IF
         WRITE(IOUNIT,9115)
 9115    FORMAT(/4X,'***************************************',
     &          /4X,'*** AERMOD Finishes UN-successfully ***',
     &          /4X,'***************************************'/)
      ELSE
C ---    AERMOD finished without any "fatal" errors, but issue
C        warning to IOUNIT if MAXDCONT or EVENT processing
C        inconsistencies occurred, before message that AERMOD
C        finished successfully
         IF (L_MAXDCONT_OrigConc_Warning) THEN
C ---       MAXDCONT processing inconsistency warning
            WRITE(*,91161)
            WRITE(IOUNIT,91161)
91161       FORMAT(/4X,'NOTE: MAXDCONT Inconsistency Warning Issued!'/)
         ELSE IF (L_EVENT_OrigConc_Warning) THEN
C ---       EVENT processing inconsistency warning
            WRITE(*,91162)
            WRITE(IOUNIT,91162)
91162       FORMAT(/4X,'NOTE: EVENT Inconsistency Warning(s) Issued!')
         END IF
         WRITE(IOUNIT,9116)
9116     FORMAT(/4X,'************************************',
     &          /4X,'*** AERMOD Finishes Successfully ***',
     &          /4X,'************************************'/)
      END IF

      IF (ERRLST) THEN
C        OPEN and Write Out Permanent Error Message File    ---   CALL MSGWRT
         OPEN(UNIT=IERWRT,FILE=MSGFIL,STATUS='REPLACE',
     &        FORM='FORMATTED')
         CALL MSGWRT
         CLOSE(IERWRT)
      END IF

C     Close and Delete The Error Message And EVENT Temporary Files
      CLOSE(IERUNT,STATUS='DELETE')
      CLOSE(ITEVUT,STATUS='DELETE')

      END

c JAT 12/14/17 subroutine usage added to write out command line argument options
      SUBROUTINE USAGE
C***********************************************************************
C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      WRITE(*,*) "usage: 0, 1, or 2 args"
      WRITE(*,*) ""
      WRITE(*,*) "Usage: AERMOD "//VERSN//" takes either no or one"
     &          // " or two parameters."
      WRITE(*,*) "       Either"
      WRITE(*,*) "             AERMOD"
      WRITE(*,*) "       Or"
      WRITE(*,*) "             AERMOD plumetest.inp"
      WRITE(*,*) "       Or"
      WRITE(*,*) "             AERMOD plumetest.inp plumetest.out"
      WRITE(*,*) ""
      WRITE(*,*) "       The first parameter  is the .INP file name,"
c     &          // " with the .INP included."
      WRITE(*,*) "       The second parameter is the .OUT file name,"
c     &          // " with the .OUT included."
      RETURN
      END SUBROUTINE USAGE


      SUBROUTINE HRLOOP
C***********************************************************************
C                 HRLOOP Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Controls Main Calculation Loop Through
C                 Hourly Meteorological Data
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   Incorporated non-DFAULT GRSM option for NO2
C                    CERC, 11/30/20
C
C        MODIFIED:   Incorporated Travel Time Reaction Method (TTRM)
C                    option for NO2 Carlos Szembek, AECOM
C                    Feb. 2021
C
C        MODIFIED:   Incorporated non-DFAULT/BETA ARM2 option for NO2
C                    Mark Podrez, RTP Environmental Associates, Inc.
C                    R. Brode, US EPA, OAQPS, AQMG, December 16, 2013
C
C        MODIFIED:  To include error handling for data mismatches between
C                   the hourly emissions and meteorological data files.
C                   Included error hanlding for end-of-file (EOF) for the
C                   meteorological data files occurring before the user-
C                   specified end-date (STARTEND keyword).  Also removed
C                   code related to "wet scimming" option, which is not
C                   supported in AERMOD.
C                   R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
C
C        MODIFIED:  To include the PVMRM and OLM options for
C                   modeling conversion of NOx to NO2.
C                   R. W. Brode, MACTEC (f/k/a PES), Inc., 07/27/04
C
C        MODIFIED:  To incorporate modifications to date processing
C                   for Y2K compliance, including use of date window
C                   variables (ISTRT_WIND and ISTRT_CENT) and calculation
C                   of 10-digit date variable (FULLDATE) with 4-digit
C                   year for date comparisons.
C                   Also modified to include SCIM option.
C                   R.W. Brode, PES, Inc., 5/12/99
C
C        MODIFIED:  To correct problems with the post-1997 PM10
C                   calculations involving leap years, and to
C                   add the year to the status message.
C                   R.W. Brode, PES, Inc. - 12/2/98
C
C        MODIFIED:  Changes to accommodate the post-1997 PM10
C                   calculations for average H4H 24-hour averages
C                   and ANNUAL averages.
C                   R.W. Brode, PES, Inc. - 8/14/98
C
C        MODIFIED:  Minor change to logic of IF block to correct
C                   potential problem with STARTEND keyword for
C                   non-sequential meteorological data sets.
C                   R.W. Brode, PES, Inc. - 4/22/96
C
C        MODIFIED:  To Include TOXXFILE Option - 9/29/92
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
      CHARACTER BGReadErr*5, BGEndErr*5
      CHARACTER O3ReadErr*5, O3EndErr*5
      CHARACTER NOxReadErr*5, NOxEndErr*5

      INTEGER :: I, ILSAVE

      DOUBLE PRECISION :: RDUM

C     Logical variable to identify READ errors in hourly BACKGROUND files
      LOGICAL :: L_BGReadErr, L_O3ReadErr, L_NOxReadErr

C     Variable Initializations
      MODNAM = 'HRLOOP'
      EOF = .FALSE.
      L_BGReadErr = .FALSE.
      L_O3ReadErr = .FALSE.
      L_NOxReadErr = .FALSE.
      IF (ALLOCATED(L_MorningTrans)) L_MorningTrans(:) = .FALSE.
      KURDAT   = 0
      KURPFL   = 0
      FULLDATE = 0

C     Begin Hourly LOOP
      HOUR_LOOP: DO WHILE (FULLDATE.LE.IEDATE .AND. .NOT.EOF)
C        Retrieve One Hour of Meteorology                   ---   CALL METEXT
C        Call to METEXT also determines the sector IDs for
C        BACKGRND and/or OZONE data and/or NOx data, IBGSECT, IO3SECT and INOXSECT
         CALL METEXT

C ---    Check for runtime error generated in call to METEXT;
C        Exit HOUR_LOOP if runtime error found
         IF (RUNERR) THEN
            EXIT HOUR_LOOP
         END IF

         IF (FULLDATE.GE.ISDATE .AND. FULLDATE.LE.IEDATE .AND.
     &       ( (.NOT.L_LeapYear.AND.IPROC (JDAY).EQ.1) .OR.
     &              (L_LeapYear.AND.IPROCL(JDAY).EQ.1) ) .AND.
     &                                                  .NOT.EOF) THEN
C           Increment counter for total number of hours processed
            IF (.NOT.L_SkipMessages) NTOTHRS = NTOTHRS + 1
         ELSE IF (FULLDATE.LT.IEDATE .AND. IEDATE.LT.2147123124 .AND.
     &                                                         EOF) THEN
C ---       End of met data file(s) reached before user-specified End Date
C           Issue fatal error message
            CALL ERRHDL(PATH,MODNAM,'E','580','MET-DATA')
C           Exit hourly loop
            EXIT HOUR_LOOP
         ELSE IF (EOF .OR. FULLDATE .GT. IEDATE) THEN
C ---       End of File or data period has been reached; EXIT hour loop
            EXIT HOUR_LOOP
         END IF

C        Save ILINE as ILSAVE and Initialize ILINE
         ILSAVE = ILINE

         IF (HOURLY) THEN
C           Process Hourly Emissions from File
C           Begin Source Loop
            DO ISRC = 1, NUMSRC
               IF (QFLAG(ISRC) .EQ. 'HOURLY') THEN
C*                Increment IQLINE counter to reflect line number of HOUREMIS file
                  IQLINE = IQLINE + 1
C*                Retrieve Source Parameters for This Hour     ---   CALL HRQREAD
                  CALL HRQREAD(ISRC)
C*                Check for Date and Time Consistency with Met Data;
C*                If Failed, Issue Fatal Error
                  IF (EOF) THEN
C*                   Write Error Message - EOF reached in hourly emission file
                     CALL ERRHDL(PATH,MODNAM,'E','580','HOUREMIS')
                     RUNERR = .TRUE.
                  ELSE IF (FULLDATE .NE. FULLHRQ) THEN
C*                   WRITE Error Message - Date mismatch
                     WRITE(DUMMY,'(I10.10)') FULLDATE
                     CALL ERRHDL(PATH,MODNAM,'E','455',DUMMY)
                     RUNERR = .TRUE.
                  END IF
C*                Extract source parameters to standard arrays, if not RUNERR
                  IF (.NOT. RUNERR) THEN
                     CALL HRQEXT(ISRC)
                  END IF

                  IF (.NOT.RSTINP .AND. L_MAXDCONT .AND.
     &                                  FULLDATE.GE.ISDATE) THEN
C ---                Save hourly emissions for MAXDCONT option
                     AAQS(IHR_NDX,IYR_NDX,ISRC) = AQS(ISRC)

                     IF (SRCTYP(ISRC)(1:5) .EQ. 'POINT') THEN
                        AATS(IHR_NDX,IYR_NDX,ISRC) = ATS(ISRC)
                        AAVS(IHR_NDX,IYR_NDX,ISRC) = AVS(ISRC)
                     ELSE IF (SRCTYP(ISRC) .EQ. 'VOLUME' .AND.
     &                                           L_HRLYSIG(ISRC)) THEN
                        AAHS(IHR_NDX,IYR_NDX,ISRC)    = AHS(ISRC)
                        AASYINI(IHR_NDX,IYR_NDX,ISRC) = ASYINI(ISRC)
                        AASZINI(IHR_NDX,IYR_NDX,ISRC) = ASZINI(ISRC)
                     ELSE IF (SRCTYP(ISRC)(1:4) .EQ. 'AREA' .AND.
     &                                           L_HRLYSIG(ISRC)) THEN
                        AAHS(IHR_NDX,IYR_NDX,ISRC)    = AHS(ISRC)
                        AASZINI(IHR_NDX,IYR_NDX,ISRC) = ASZINI(ISRC)
                     ELSE IF (SRCTYP(ISRC) .EQ. 'LINE' .AND.
     &                                           L_HRLYSIG(ISRC)) THEN
                        AAHS(IHR_NDX,IYR_NDX,ISRC)    = AHS(ISRC)
                        AASZINI(IHR_NDX,IYR_NDX,ISRC) = ASZINI(ISRC)
                     ELSE IF (SRCTYP(ISRC) .EQ. 'BUOYLINE') THEN
                        AAFP(IHR_NDX,IYR_NDX,ISRC) = AFP(ISRC)
                     END IF
                  END IF

               END IF
            END DO
C*          End Source Loop
         END IF

C ---    Check for runtime error generated in call to HRQREAD;
C        Exit HOUR_LOOP if runtime error found
         IF (RUNERR) EXIT HOUR_LOOP

C*----
C        Save ILINE as ILSAVE and Initialize ILINE
         ILSAVE = ILINE

         IF (L_BACKGRND) THEN
C ---       Process Background Concentration inputs, starting with Hourly BG

            IF (L_BGHourly) THEN
C*             Increment IBLINE counter to reflect line number of hourly BACKGRND file
               IBLINE = IBLINE + 1

C*             Retrieve hourly background concentrations      ---   CALL BGEXT
               CALL BGEXT(L_BGReadErr,BGReadErr,BGEndErr)

C*             Check for issues reading hourly BG file
               IF (EOF) THEN
C*                Write Error Message - EOF reached in hourly background file
                  WRITE(DUMMY,'(''BGFILE '',A5)') BGEndErr
                  CALL ERRHDL(PATH,MODNAM,'E','580',DUMMY)
                  RUNERR = .TRUE.
               ELSE IF (L_BGReadErr) THEN
C*                Write Error Message - READ error in hourly background file
                  WRITE(DUMMY,'(''BGFILE '',A5)') BGReadErr
                  CALL ERRHDL(PATH,MODNAM,'E','510',DUMMY)
                  RUNERR = .TRUE.
               END IF

            ELSE

C ---          Check for temporally-varying background to substitute for missing hours
               IF (IBGSECT .GT. 0) THEN
C                 Valid IBGSECT value
                  IF (L_BGValues(IBGSECT)) THEN
                     CALL BGVAL(IBGSECT,BGCONC)
                  ELSE
                     BGCONC = 0.0D0
                  END IF
               ELSE
C ---             IBGSECT is missing (calm or missing hour); set BGCONC = 0.0
C                 Note: this should result in a fatal error (452)
                  BGCONC = 0.0D0
               END IF
            END IF

         END IF

C ---    Check for runtime error generated in call to BGEXT;
C        Exit HOUR_LOOP if runtime error found
         IF (RUNERR) EXIT HOUR_LOOP

         IF (L_BACKGRND .AND. .NOT.RSTINP .AND. L_MAXDCONT .AND.
     &                         FULLDATE.GE.ISDATE) THEN
C ---       Save hourly background concentration for MAXDCONT option
            ABGCONC(IHR_NDX,IYR_NDX) = BGCONC
         END IF

C*----
C        Retrive ILINE From ILSAVE
         ILINE = ILSAVE

!! Added for TTRM2
         IF (RUNTTRM2) THEN
               TTRMCOMPARE(:,:,:,:) = 0.0
               CMETH = 1
         ENDIF
!! End of TTRM2 insert; Nov. 2021

         IF (PVMRM .OR. OLM .OR. RUNTTRM .OR. GRSM) THEN
C-----      Read Ozone Data File if available
            IF (L_O3Hourly) THEN
C*             Increment IOLINE counter to reflect line number of Hourly O3 file
               IOLINE = IOLINE + 1

C ---          Extract O3 value from hourly data file; O3EXT also reads a record
C              for O3FILEs available for other sectors to keep files synchronized,
C              so premature EOF for one file should result in date mismatch error
               IF (.NOT. EOF) THEN
                  CALL O3EXT(L_O3ReadErr,O3ReadErr,O3EndErr)
               END IF

               IF (EOF) THEN
C*                Write Error Message - EOF reached in hourly O3 file
                  WRITE(DUMMY,'(''O3FILE '',A5)') O3EndErr
                  CALL ERRHDL(PATH,MODNAM,'E','580',DUMMY)
                  RUNERR = .TRUE.
               ELSE IF (L_O3ReadErr) THEN
C*                Write Error Message - EOF reached in hourly O3 file
                  WRITE(DUMMY,'(''O3FILE '',A5)') O3ReadErr
                  CALL ERRHDL(PATH,MODNAM,'E','510',DUMMY)
                  RUNERR = .TRUE.
               END IF

            ELSE IF (IO3SECT .GT. 0) THEN
               IF (L_O3VALUES(IO3SECT)) THEN
C ---             Use ozone concentration from O3VALUES keyword
                  CALL OZONVALS(IO3SECT,O3CONC)
               ELSE IF (L_O3VAL(IO3SECT)) THEN
C ---             Use single "background" O3 value from OZONEVAL keyword
                  O3CONC = O3BACK(IO3SECT)
               ELSE
C ---             Set O3CONC to 0.0 for full conversion (subject to
C                 equilibrium ratio)
                  O3CONC = 0.0D0
               END IF
            ELSE
C ----         IO3SECT is 0 due to calm/missing hour; set O3CONC to 0.0
               O3CONC = 0.0D0
            END IF

            IF (.NOT.RSTINP .AND. L_MAXDCONT .AND.
     &                            FULLDATE.GE.ISDATE) THEN
C ---          Save hourly ozone concentration for MAXDCONT option
CJAT  06/10/2020  ISSUE D47 ADDED FROM 19191
C             IF O3MISS IS TRUE, SET AO3CONC TO -9
C             OTHERWISE SET TO O3CONC
C             SETTING AO3CONC TO -9 MAKES MAXDCONT CONSISTENT WITH
C             BASE AERMOD RUN.
                IF (O3MISS) THEN
                    AO3CONC(IHR_NDX,IYR_NDX)=-9.0D0
                ELSE
                   AO3CONC(IHR_NDX,IYR_NDX) = O3CONC
                ENDIF

C               AO3CONC(IHR_NDX,IYR_NDX) = O3CONC
            END IF
         END IF

C        CERC 11/30/20
         IF (GRSM) THEN
C-----      Read NOx Data File if available
            IF (L_NOxHourly) THEN
C*             Increment INOXLINE counter to reflect line number of Hourly NOX file
               INOXLINE = INOXLINE + 1

C ---          Extract NOx value from hourly data file; NOXEXT also reads a record
C              for NOXFILEs available for other sectors to keep files synchronized,
C              so premature EOF for one file should result in date mismatch error
               IF (.NOT. EOF) THEN
                  CALL NOXEXT(L_NOXReadErr,NOxReadErr,NOxEndErr)
               END IF

               IF (EOF) THEN
C*                Write Error Message - EOF reached in hourly NOx file
                  WRITE(DUMMY,'(''NOXFIL '',A5)') NOxEndErr
                  CALL ERRHDL(PATH,MODNAM,'E','580',DUMMY)
                  RUNERR = .TRUE.
               ELSE IF (L_NOxReadErr) THEN
C*                Write Error Message - Error reading in hourly NOx file
                  WRITE(DUMMY,'(''NOXFIL '',A5)') NOxReadErr
                  CALL ERRHDL(PATH,MODNAM,'E','510',DUMMY)
                  RUNERR = .TRUE.
               END IF
            ELSEIF (INOXSECT .GT. 0) THEN
               IF (L_NOX_VALS(INOXSECT)) THEN
C ---             Use NOX concentration from NOX_VALS keyword
                  CALL VARYNOXVALS(INOXSECT,NOXBGCONC)
               ELSE IF (L_NOXVALUE(INOXSECT)) THEN
C ---             Use single "background" NOX value from NOXVALUE keyword
                  NOXBGCONC = NOXBACK(INOXSECT)
               ELSE
C ---             Set NOXBGCONC to 0.0
                  NOXBGCONC = 0.0D0
               END IF
            ELSE
C ----         INOXSECT is 0 due to calm/missing hour; set NOXBGCONC to 0.0
               NOXBGCONC = 0.0D0
            END IF

            IF (.NOT.RSTINP .AND. L_MAXDCONT .AND.
     &                            FULLDATE.GE.ISDATE) THEN
C ---          Save hourly NOX concentration for MAXDCONT option
               ANOXBGCONC(IHR_NDX,IYR_NDX) = NOXBGCONC
            END IF
         END IF

C ---    Check for runtime error generated in call to O3EXT;
C        Exit HOUR_LOOP if runtime error found
         IF (RUNERR) EXIT HOUR_LOOP

C*----
C        Retrive ILINE From ILSAVE
         ILINE = ILSAVE

C*       Check for IHOUR = 1 and Write Update to the Screen For PC Version
         IF ((IHOUR.EQ.1 .OR. ILINE.EQ.1) .AND. .NOT.NOCHKD) THEN
C*          Write Out Update to the Screen by Julian Day
            WRITE(*,909) JDAY, IYR
 909        FORMAT('+','Now Processing Data For Day No. ',I4,' of ',I4)
         ELSE IF (NOCHKD) THEN
C*          Write Out Update to the Screen by Hour
            WRITE(*,910) KURDAT
 910        FORMAT('+','Now Processing Data For     ',I8.8)
         END IF
C*----
C*#
         IF (SCIM .AND. .NOT.EOF) THEN
            SCIMHR = .FALSE.

C           User has specified SCIM option.  Check for whether current
C           hour is to be sampled, and whether to write sampled met
C           data to output file.

C           Keep track of total no. of hours.
C           Also, keep track of dry & wet, and calm & missing hours
C           Note:  Under SCIM option, IANHRS/IANCLM/IANMSG (see below) pertain
C                  to no. of hours sampled.
            NSKIPTOT = NSKIPTOT + 1

            IF( ILINE .LE. 24 .AND. IHOUR .EQ. NREGSTART )THEN
C              Current hour is to be sampled - first SCIM'd hour.
               IFIRSTHR = ILINE
               SCIMHR   = .TRUE.
            ELSE IF( ILINE .GT. NREGSTART .AND.
     &               MOD( ILINE-IFIRSTHR, NREGINT ) .EQ. 0 )THEN
C              Current hour is to be sampled - SCIM'd hour
               SCIMHR   = .TRUE.
            ELSE
C              Current hour is NOT to be sampled. Check for end of year first.
               CALL CHK_ENDYR
               CYCLE HOUR_LOOP
            END IF

            IF (SCIMOUT) THEN
C              Write sampled meteorology to SCIM'd met data file
               CALL METSUM
            END IF
         END IF

         IF (FULLDATE.GE.ISDATE .AND. FULLDATE.LE.IEDATE .AND.
     &       ( (.NOT.L_LeapYear.AND.IPROC (JDAY).EQ.1) .OR.
     &              (L_LeapYear.AND.IPROCL(JDAY).EQ.1) ) .AND.
     &                .NOT.EOF .AND. .NOT.RUNERR) THEN

C ---       Check for calm winds or missing met data, for which model
C           calculations cannot be made; increment counters for number
C           of hours, but do not include background concentrations, if
C           specified through the BACKGRND keyword.
            IF (CLMHR .AND. CLMPRO) THEN
C              Check for Calm Hr & Processing and Increment Counters
               DO IAVE = 1, NUMAVE
                  NUMHRS(IAVE) = NUMHRS(IAVE) + 1
                  NUMCLM(IAVE) = NUMCLM(IAVE) + 1
               END DO
               IF (PERIOD .OR. ANNUAL) THEN
                  IF (.NOT.SCIM .OR. (SCIM.AND.SCIMHR)) THEN
                     IANHRS = IANHRS + 1
                     IANCLM = IANCLM + 1
                  END IF
               END IF
               IF (SEASONHR) THEN
                  NSEAHR(ISEAS,IHOUR) = NSEAHR(ISEAS,IHOUR) + 1
                  NSEACM(ISEAS,IHOUR) = NSEACM(ISEAS,IHOUR) + 1
               END IF
            ELSE IF (MSGHR .AND. MSGPRO) THEN
C              Check for Missing Hour & Processing and Increment Counters
               DO IAVE = 1, NUMAVE
                  NUMHRS(IAVE) = NUMHRS(IAVE) + 1
                  NUMMSG(IAVE) = NUMMSG(IAVE) + 1
               END DO
               IF (PERIOD .OR. ANNUAL) THEN
                  IF (.NOT.SCIM .OR. (SCIM.AND.SCIMHR)) THEN
                     IANHRS = IANHRS + 1
                     IANMSG = IANMSG + 1
                  END IF
               END IF
               IF (SEASONHR) THEN
                  NSEAHR(ISEAS,IHOUR) = NSEAHR(ISEAS,IHOUR) + 1
                  NSEACM(ISEAS,IHOUR) = NSEACM(ISEAS,IHOUR) + 1
               END IF
            ELSE IF (ZI .LE. 0.0D0) THEN
C              Write Out The Informational Message & Increment Counters
               IF (.NOT. L_SkipMessages) THEN
                  WRITE(DUMMY,'(I8.8)') KURDAT
                  CALL ERRHDL(PATH,MODNAM,'I','470',DUMMY)
               END IF
               DO IAVE = 1, NUMAVE
                  NUMHRS(IAVE) = NUMHRS(IAVE) + 1
               END DO
               IF (PERIOD .OR. ANNUAL) THEN
                  IF (.NOT.SCIM .OR. (SCIM.AND.SCIMHR)) THEN
                     IANHRS = IANHRS + 1
                  END IF
               END IF
               IF (SEASONHR) THEN
                  NSEAHR(ISEAS,IHOUR) = NSEAHR(ISEAS,IHOUR) + 1
               END IF
            ELSE
C              Set CALCS Flag, Increment Counters & Calculate HRVAL
               CALCS = .TRUE.
               DO IAVE = 1, NUMAVE
                  NUMHRS(IAVE) = NUMHRS(IAVE) + 1
               END DO
               IF (PERIOD .OR. ANNUAL) THEN
                  IF (.NOT.SCIM .OR. (SCIM.AND.SCIMHR)) THEN
                     IANHRS = IANHRS + 1
                  END IF
               END IF
               IF (SEASONHR) THEN
                  NSEAHR(ISEAS,IHOUR) = NSEAHR(ISEAS,IHOUR) + 1
               END IF

C              Time/Date Marker for DEBUG Output
               IF (DEBUG) THEN
                  WRITE(DBGUNT,*)
                  WRITE(DBGUNT,*) '--------------------------------',
     &                            '--------------------'
                  WRITE(DBGUNT,*) '---  JDAY, IHOUR =  ',JDAY,IHOUR
                  WRITE(DBGUNT,*) '--------------------------------',
     &                            '--------------------'
               END IF

C ---          Calculate CONC or DEPOS Values               ---   CALL CALC
               CALL CALC
            END IF

            IF (.NOT.CLMHR .AND. .NOT.MSGHR) THEN
C ---          Non-calm, non-missing hour; apply NO2 options as appropriate
!! Added for TTRM2
!! If TTRM2 (TTRM with the compare option) is requested then
!! perform TTRM >> FIRST <<
               IF (RUNTTRM2) THEN
!!                Check if the METHOD flag is set to 1 for HRLOOP;
!!                if not, cycle through the other NO2 options
                  IF (CMETH .EQ. 1) THEN
C ---             Process Hourly Values for TTRM Option
                      CALL TTRM_CALC
                  ENDIF
                  CMETH = 2
C                 Flush HRVAL Arrays (1:NUMTYP)
                  HRVAL(:)   = 0.0D0
               ENDIF
               IF (PVMRM .AND. .NOT.PSDCREDIT) THEN
C ---             Process Hourly Values for PVMRM Option
                  CALL PVMRM_CALC('ALLSRCS')

               ELSE IF (PVMRM .AND. PSDCREDIT) THEN
C ---             Process Hourly Values for PVMRM Option and PSD credits
C ---             Need to process two separate sets of sources - the
C                 increment consumption sources ('NAAQSRC') and the
C                 increment expanding sources ('ALLBASE')
                  CALL PVMRM_CALC('NAAQSRC')
                  CALL PVMRM_CALC('ALLBASE')

               ELSE IF (OLM) THEN
C ---             Process Hourly Values for OLM Option
                  CALL OLM_CALC

               ELSE IF (ARM2) THEN
C ---             Process Hourly Values for ARM2 Option
                  CALL ARM2_CALC
               ELSE IF (GRSM) THEN
C ---             CERC 11/30/20 Process Hourly Values for GRSM Option
!! TTRM2 has not designed to be used with GRSM
                  IF(.NOT. RUNTTRM2) THEN
                    CALL GRSM_CALC
                  ENDIF

!! End of TTRM2 insert; Nov. 2021
               ELSE IF (RUNTTRM) THEN
                  IF (.NOT. RUNTTRM2) THEN
C ---             Process Hourly Values for TTRM Option
                      CALL TTRM_CALC
                  ENDIF
               END IF
            END IF

C           Begin Averaging Period LOOP
            DO IAVE = 1, NUMAVE
C              Check for End of Averaging Period
               IF (MOD(IHOUR,KAVE(IAVE)).EQ.0 .OR.
     &            (KAVE(IAVE).EQ.720 .AND. ENDMON)) THEN
                  IF (CONC) THEN
C                    Calculate Applicable Averages          ---   CALL AVER
                     CALL AVER
                  END IF
C                 Update High Value Arrays                  ---   CALL HIVALS
                  CALL HIVALS

                  IF( (NO2AVE .OR. SO2AVE) .AND. KAVE(IAVE).EQ.1 )THEN
C ---                Loop through SRCGRPs again to get max daily 1hr cumulative value
                     DO IGRP = 1, NUMGRP
                        DO IREC = 1, NUMREC
                           IF (AVEVAL(IREC,IGRP,IAVE,1) .GT.
     &                                          MXDVAL(IREC,IGRP)) THEN
                            MXDVAL(IREC,IGRP) = AVEVAL(IREC,IGRP,IAVE,1)
                            IMXDHR(IREC,IGRP) = IHOUR
                           END IF
                        END DO
                     END DO
                  END IF

                  IF( PM25AVE .AND. MOD(IHOUR,24).EQ.0 .AND.
     &                                           KAVE(IAVE).EQ.24 )THEN
C ---                Loop through source groups again to get max daily 24-hr cumulative value
                     DO IGRP = 1, NUMGRP
                        DO IREC = 1, NUMREC
                           IF (AVEVAL(IREC,IGRP,IAVE,1) .GT.
     &                                          MXDVAL(IREC,IGRP)) THEN
                            MXDVAL(IREC,IGRP) = AVEVAL(IREC,IGRP,IAVE,1)
                            IMXDHR(IREC,IGRP) = IHOUR
                           END IF
                        END DO
                     END DO
                  END IF

                  IF (DAYTAB .AND. IDYTAB(IAVE).EQ.1) THEN
                     DO ITYP = 1, NUMTYP
C                       Print Out Daily Value Tables        ---   CALL PRTDAY
                        CALL PRTDAY
                     END DO
                  END IF
                  IF (MXFILE) THEN
C                    Write Max Values (>Thresh) to File     ---   CALL MAXFIL
                     CALL MAXFIL
                  END IF
                  IF (PPFILE) THEN
C                    Write Values to Postprocessor File     ---   CALL POSTFL
                     CALL POSTFL
                  END IF
                  IF (TXFILE) THEN
C                    Write Values to TOXXFILE File (9/29/92) ---  CALL TOXXFL
                     CALL TOXXFL
                  END IF
C                 Flush Block Average Values in AVEVAL Array for This IAVE
                  AVEVAL(1:NUMREC,1:NUMGRP,IAVE,1:NUMTYP) = 0.0D0
               END IF
            END DO
C           End Averaging Period LOOP

C ---       Check for PM25AVE, NO2AVE or SO2AVE to update daily
C           maximum value arrays; also output to MAXDAILY file,
C           if requested
            IF (PM25AVE .OR. NO2AVE .OR. SO2AVE) THEN
               IF (MOD(IHOUR,24).EQ.0) THEN
C ---             End of day reached, call MXDLYFL
                  CALL MXDLYFL
               END IF
            END IF

            IF (RSTSAV .AND. IHOUR.EQ.24) THEN
               NDAYS = NDAYS + 1
               IF (NDAYS .EQ. INCRST) THEN
C                 Save Results to File for Later Re-start   ---   CALL RSDUMP
                  CALL RSDUMP
                  NDAYS = 0
               END IF
            END IF

C           Flush HRVAL Arrays (1:NUMTYP)
            HRVAL(:)   = 0.0D0
            AERVAL(:)  = 0.0D0
            PRMVAL(:)  = 0.0D0
            IF (ALLOCATED(BACKAVE)) BACKAVE(:)  = 0.0D0
            IF (ALLOCATED(BACKHR))  BACKHR(:,:) = 0.0D0

            IF (PVMRM .OR. OLM .OR. ARM2 .OR.
     &         RUNTTRM .OR. GRSM) THEN
C              Flush CHI(NUMREC,NUMSRC,NUMTYP) Array
               CHI(:,:,:) = 0.0D0
               IF(GRSM)THEN
                 CHI_TTRAVPLM = 0.0D0
                 CHI_TTRAVPAN = 0.0D0
                 CHI_TTRAVAER = 0.0D0
                 CHI_TTRAVPRM = 0.0D0
                 CHI_TTRAVCHM(:,:) = 0.0D0
               END IF
               IF (PSDCREDIT) THEN
C                 Flush ABVAL(NUMREC,NUMTYP) and BCVAL(NUMREC,NUMTYP) Arrays
                  ABVAL(:,:) = 0.0D0
                  BCVAL(:,:) = 0.0D0
               END IF
            END IF

         END IF

C        Check for end of year of data for PM25, NO2, SO2, or MULTYR processing;
C        but skip if NOCHKD option or WARNCHKD option is used (this also includes
C        SCREEN option since SCREEN ==> NOCHKD)
         IF (FULLDATE.GT.ISDATE .AND. .NOT.EOF .AND. .NOT.NOCHKD .AND.
     &                                           .NOT.L_WARNCHKD .AND.
     &                           (PM25AVE .OR. NO2AVE .OR. SO2AVE .OR.
     &                                         ANNUAL .OR. MULTYR)) THEN

            CALL CHK_ENDYR

         ELSEIF(FULLDATE.EQ.ISDATE .AND..NOT.EOF .AND..NOT.NOCHKD .AND.
     &                                            .NOT.L_WARNCHKD .AND.
     &                           (PM25AVE .OR. NO2AVE .OR. SO2AVE .OR.
     &                                         ANNUAL .OR. MULTYR)) THEN

            NREMAIN = NREMAIN + 1

         END IF

C        Reset CALCS and ENDMON Flags
         CALCS  = .FALSE.
         ENDMON = .FALSE.

C        Save precipitation rates for two previous hours
         prec2 = prec1
         prec1 = Prate

      END DO HOUR_LOOP
C     End Hourly LOOP

C     Check for TOXXFILE Option, Fill Buffer and Dump to File - 9/29/92
      IF (TXFILE) THEN
         IDUM = 0
         RDUM = 0.0D0
         DO IAVE = 1, NUMAVE
            IF (ITOXFL(IAVE) .EQ. 1) THEN
C              Fill Rest of Buffer With Zeroes and Write to TOXXFILE
               DO I = IPAIR+1, NPAIR
                  IDCONC(IAVE,I) = IDUM
                  TXCONC(IAVE,I) = RDUM
               END DO
               WRITE(ITXUNT(IAVE)) (IDCONC(IAVE,I),I=1,NPAIR)
               WRITE(ITXUNT(IAVE)) (TXCONC(IAVE,I),I=1,NPAIR)
               CLOSE(ITXUNT(IAVE))
            END IF
         END DO
      END IF

C     Write Out Update to the Screen for PC Version
      WRITE(*,919)
 919  FORMAT('+','Now Processing Output Options               ')

      RETURN
      END

      SUBROUTINE JULIAN(INYR,INMN,INDY,JDY)
C***********************************************************************
C                 JULIAN Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE:    CONVERT YR/MN/DY DATE TO JULIAN DAY (1-366),
C                    INCLUDES TEST FOR 100 AND 400 YEAR CORRECTIONS TO
C                    HANDLE 4 DIGIT YEARS BEYOND 2099 AND BEFORE 1901
C                    (WILL WORK WITH 2 DIGIT YR FOR PERIOD 1901-2099)
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:     YEAR,  INYR (2 OR 4 DIGIT)
C                    MONTH, INMN
C                    DAY,   INDY
C
C        OUTPUT:     JULIAN DAY,  JDY (1-366)
C
C        CALLED FROM:   DAYRNG
C
C        ERROR HANDLING:   Checks for Invalid Month or Day
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: NDAY(12), IDYMAX(12)
      INTEGER :: INYR, INMN, INDY, JDY

C     Variable Initializations
      DATA NDAY/0,31,59,90,120,151,181,212,243,273,304,334/
      DATA IDYMAX/31,29,31,30,31,30,31,31,30,31,30,31/
      MODNAM = 'JULIAN'
      JDY = 0

C     Check for Invalid Month or Day
      IF (INMN.LT.1 .OR. INMN.GT.12) THEN
C        WRITE Error Message    ! Invalid Month
         WRITE(DUMMY,'(''MONTH = '',I2)') INMN
         CALL ERRHDL(PATH,MODNAM,'E','203',DUMMY)
         RUNERR = .TRUE.
         GO TO 999
      ELSE IF (INDY .GT. IDYMAX(INMN)) THEN
C        WRITE Error Message    ! Invalid Day
         WRITE(DUMMY,'(''DAY='',I2,'' MO='',I2)') INDY,INMN
         CALL ERRHDL(PATH,MODNAM,'E','203',DUMMY)
         RUNERR = .TRUE.
         GO TO 999
      END IF

C     Determine JULIAN Day Number; For Non-Leap Year First
      IF ((MOD(INYR,4) .NE. 0) .OR.
     &    (MOD(INYR,100) .EQ. 0 .AND. MOD(INYR,400) .NE. 0)) THEN
C        Not a Leap Year
         IF (INMN.NE.2 .OR. (INMN.EQ.2 .AND. INDY.LE.28)) THEN
            JDY = INDY + NDAY(INMN)
         ELSE
C           WRITE Error Message    ! Invalid Date; 2/29 in a Non-Leap Year
            WRITE(DUMMY,'("YR= ",I4)') INYR
            CALL ERRHDL(PATH,MODNAM,'E','370',DUMMY)
            JDY = 60
            RUNERR = .TRUE.
         END IF
      ELSE
C        Leap Year
         JDY = INDY + NDAY(INMN)
         IF (INMN .GT. 2)  JDY = JDY + 1
      END IF

 999  CONTINUE

      RETURN
      END

      SUBROUTINE GREGOR(INYR,INMN,JDY,IDY)
C***********************************************************************
C                 GREGOR Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE:    CONVERT JULIAN DAY (1-366) TO DAY OF MONTH,
C                    INCLUDES TEST FOR 100 AND 400 YEAR CORRECTIONS TO
C                    HANDLE 4 DIGIT YEARS BEYOND 2099 AND BEFORE 1901
C                    (WILL WORK WITH 2 DIGIT YR FOR PERIOD 1901-2099)
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:     YEAR,       INYR (2 OR 4 DIGIT)
C                    MONTH,      INMN
C                    JULIAN DAY, JDY (1-366)
C
C        OUTPUT:     DAY OF MONTH, IDY
C
C        CALLED FROM:   METEXT
C
C        ERROR HANDLING:   Checks for Invalid Month or Day
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: NDAY(12)
      INTEGER :: INYR, INMN, IDY, JDY

C     Variable Initializations
      DATA NDAY/0,31,59,90,120,151,181,212,243,273,304,334/
      MODNAM = 'GREGOR'

C     Check for Invalid Month or Julian Day
      IF (INMN.LT.1 .OR. INMN.GT.12) THEN
C        WRITE Error Message    ! Invalid Month
         CALL ERRHDL(PATH,MODNAM,'E','203','MONTH')
         GO TO 999
      ELSE IF (JDY.LT.1 .OR. JDY.GT.366) THEN
C        WRITE Error Message    ! Invalid Julian Day
         CALL ERRHDL(PATH,MODNAM,'E','203','Juli Day')
         GO TO 999
      END IF

C     Determine Day-of-Month Number; For Non-Leap Year First
      IF ((MOD(INYR,4) .NE. 0) .OR.
     &    (MOD(INYR,100).EQ.0 .AND. MOD(INYR,400).NE.0)) THEN
C        Not a Leap Year
         IDY = JDY - NDAY(INMN)
      ELSE
C        Leap Year
         IDY = JDY - NDAY(INMN)
         IF (INMN .GT. 2)  IDY = IDY - 1
      END IF

 999  CONTINUE

      RETURN
      END

      SUBROUTINE HRQREAD (IS)
C***********************************************************************
C*                  HRQREAD Module of AERMOD
C*
C*         PURPOSE: To Assign Hourly Source Parameters
C*
C*         PROGRAMMER:  Jayant Hardikar, Roger Brode
C*
C*         DATE:    September 15, 1993
C*
C*         INPUTS:  Current Source Number Being Processed
C*
C*         OUTPUTS: Source Arrays
C*
C*         Revision History:
C*
C*         MODIFIED:  Added possibility of RLINE or RLINEXT source type.
C*                    Wood, 03/18/2019
C*
C*         MODIFIED:  Added processing of hourly emissions file for
C*                    a RLINE source.
C*                    Wood, 7/20/2018
C*
C*         MODIFIED:  Added processing of hourly emissions file for
C*                    a buoyant line source.
C*                    Amec Foster Wheeler, 6/30/2015
C*
C*         MODIFIED:  Included check on length of FIELD(7) before assigning
C*                    to the HRSOID variable to avoid runtime error if
C*                    SRCIDs in HOUREMIS file exceed 12 character limit.
C*                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 03/19/2014
C*
C*         MODIFIED:  Check for use of 4-digit year in HOUREMIS file, and
C*                    adjust if needed for comparison to KURDAT from the
C*                    met data file.
C*                    Incorporated options to specify hourly-varying
C*                    release heights and initial dispersion coefficients
C*                    for VOLUME and AREA sources.
C*                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
C*
C*         MODIFIED:  Corrected processing of missing parameters for
C*                    point sources to assign all parameters to 0.0 if
C*                    any of the parameters are missing, in conformance
C*                    with Section 3.3.9 of the AERMOD User's Guide.
C*                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 01/24/2007
C*
C*         MODIFIED:  REMOVED THE 'POINT' SOURCE CONDITION, SO IT APPLIES
C*                    TO ALL SOURCE TYPES, EXCEPT SAVING THE TEMP & VEL
C*
C*         CALLED FROM:  HRLOOP
C************************************************************************
C*
C*    Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, IS
      INTEGER :: IHYEAR, IHMON, IHDAY, IHHOUR, IHYEAR2
      INTEGER :: ILSAVE
      CHARACTER (LEN=20) :: RDFRM

      CHARACTER (LEN=12) :: HRSOID

C*    Variable Initializations
      MODNAM = 'HRQREAD'

C*    Assign IQLINE counter to ILINE for passing to ERRHDL if needed, save as ILSAVE first
      ILSAVE = ILINE
      ILINE  = IQLINE

C*    READ Record to Buffers, A'num' and 'num'A1, where num=ISTRG
C*    Length of ISTRG is Set in PARAMETER Statement in MAIN1
C     Setup READ format and ECHO format for runstream record,
C     based on the ISTRG PARAMETER (set in MAIN1)
      WRITE(RDFRM,9100) ISTRG, ISTRG
 9100 FORMAT('(A',I4.4,',T1,',I4.4,'A1)')
      READ (IHREMI,RDFRM,END=888,ERR=99) RUNST1, (RUNST(I), I=1, ISTRG)
C*
C*    Convert Lower Case to Upper Case Letters              ---   CALL LWRUPR
      CALL LWRUPR
C*
C*    Define Fields on Card                                 ---   CALL DEFINE
      CALL DEFINE
C*
C*    Get the Contents of the Fields                        ---   CALL GETFLD
      CALL GETFLD
C*
C*    Check for number of fields - error if less than 7.
      IF (IFC .LT. 7) THEN
         WRITE(DUMMY,'(I8)') KURDAT
         CALL ERRHDL(PATH,MODNAM,'E','384',DUMMY)
         RUNERR = .TRUE.
         GO TO 999
      END IF
C*
C*    Assign the Fields to Local Varables and Check The Numerical Field
C*
C*    Date and time variables common to all source types
C*
      CALL STONUM(FIELD(3), ILEN_FLD, FNUM, IMIT)
      IHYEAR = NINT(FNUM)
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208','HOUREMIS')
         RUNERR = .TRUE.
         GO TO 999
      END IF

      CALL STONUM(FIELD(4), ILEN_FLD, FNUM, IMIT)
      IHMON = NINT(FNUM)
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208','HOUREMIS')
         RUNERR = .TRUE.
         GO TO 999
      END IF

      CALL STONUM(FIELD(5), ILEN_FLD, FNUM, IMIT)
      IHDAY = NINT(FNUM)
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208','HOUREMIS')
         RUNERR = .TRUE.
         GO TO 999
      END IF

      CALL STONUM(FIELD(6), ILEN_FLD, FNUM, IMIT)
      IHHOUR = NINT(FNUM)
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208','HOUREMIS')
         RUNERR = .TRUE.
         GO TO 999
      END IF

C --- Check for use of 2-digit year in HOUREMIS file, adjust to 4-digit
C     year for comparison with FULLDATE based on met data file
      IF (IHYEAR .LE. 99) THEN
         IHYEAR2 = IHYEAR
         IF (IHYEAR2 .GE. ISTRT_WIND .AND.
     &                        IHYEAR2 .LE. 99) THEN
            IHYEAR = ISTRT_CENT*100 + IHYEAR2
         ELSE IF (IHYEAR2 .LT. ISTRT_WIND) THEN
            IHYEAR = (ISTRT_CENT+1)*100 + IHYEAR2
         END IF
      END IF

C --- Calculate current date (YYYYMMDDHH) from HOUREMIS file record, FULLHRQ
      FULLHRQ = IHYEAR*1000000 + IHMON*10000 + IHDAY*100 + IHHOUR

C --- Assign source ID but check for field length > 12 first
      IF( LEN_TRIM(FIELD(7)) .LE. 12 ) THEN
         HRSOID = FIELD(7)
      ELSE
         HRSOID = FIELD(7)(1:12)
      END IF

C*    Check for Source ID Consistency ; If Failed Issue Error
      IF ( HRSOID .NE. SRCID(IS) ) THEN
         WRITE(DUMMY,'(A12)') SRCID(IS)
         CALL ERRHDL(PATH,MODNAM,'E','342',SRCID(IS))
         RUNERR = .TRUE.
         GO TO 999
      END IF

      IF (IFC .EQ. 7) THEN
C*       All parameters missing for this hour/source - WRITE Warning Message
C*       Assign zeros to all parameters
         IF (.NOT. L_SkipMessages) THEN
            WRITE(DUMMY,'(I10.10)') FULLHRQ
            CALL ERRHDL(PATH,MODNAM,'W','344',DUMMY)
         END IF
         HRQS = 0.0D0
         HRTS = 0.0D0
         HRVS = 0.0D0
         HRHS = 0.0D0
         HRSY = 0.0D0
         HRSZ = 0.0D0
         HRFP = 0.0D0

C ------------------------ Begin correct # of parameters for source type

      ELSE IF (SRCTYP(IS)(1:5) .EQ. 'POINT' .AND. IFC.EQ.10) THEN
C*       Assign emission rate, exit temperature and exit velocity
C*       for POINT sources

         CALL STODBL(FIELD(8), ILEN_FLD, HRQS, IMIT)
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208','HOUREMIS')
            RUNERR = .TRUE.
            GO TO 999
C ---    Check for large negative values, could be a missing indicator
         ELSE IF ( HRQS .LE. -90.0D0 ) THEN
C*          Assume emissions are missing; assign value of 0.0 and issue Warning;
C*          Note that AERMOD User's Guide (p. 3-38, 3-39) implies that blanks
C*          should be used for missing values.
            HRQS = 0.0D0
            WRITE(DUMMY,'(I10.10)') FULLHRQ
            CALL ERRHDL(PATH,MODNAM,'W','341',DUMMY)
         END IF

         CALL STODBL(FIELD(9), ILEN_FLD, HRTS, IMIT)
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208','HOUREMIS')
            RUNERR = .TRUE.
            GO TO 999
         END IF

         CALL STODBL(FIELD(10), ILEN_FLD, HRVS, IMIT)
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208','HOUREMIS')
            RUNERR = .TRUE.
            GO TO 999
         END IF

      ELSE IF (SRCTYP(IS) .EQ. 'VOLUME' .AND. IFC.EQ.11) THEN
C*       Assign emission rate, release height and initial sigmas
C*       for VOLUME source.
C*       Assign logical variable indicating hourly sigmas, L_HRLYSIG
         IF (ILSAVE .EQ. 1) THEN
            L_HRLYSIG(IS) = .TRUE.
         ELSE IF (ILSAVE .GT. 1 .AND. .NOT. L_HRLYSIG(IS)) THEN
C*          This volume source should not include hourly sigmas;
C*          issue error message
            WRITE(DUMMY,'(I10.10)') FULLHRQ
            CALL ERRHDL(PATH,MODNAM,'E','345',DUMMY)
            HRQS = 0.0D0
            RUNERR = .TRUE.
            GO TO 999
         END IF

         CALL STODBL(FIELD(8), ILEN_FLD, HRQS, IMIT)
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208','HOUREMIS')
            RUNERR = .TRUE.
            GO TO 999
C ---    Check for large negative values, could be a missing indicator
         ELSE IF ( HRQS .LE. -90.0D0 ) THEN
C*          Assume emissions are missing; assign value of 0.0 and issue Warning;
C*          Note that AERMOD User's Guide (p. 3-38, 3-39) implies that blanks
C*          should be used for missing values.
            HRQS = 0.0D0
            WRITE(DUMMY,'(I10.10)') FULLHRQ
            CALL ERRHDL(PATH,MODNAM,'W','341',DUMMY)
         END IF

         CALL STODBL(FIELD(9), ILEN_FLD, HRHS, IMIT)
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208','HOUREMIS')
            RUNERR = .TRUE.
            GO TO 999
         END IF

         CALL STODBL(FIELD(10), ILEN_FLD, HRSY, IMIT)
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208','HOUREMIS')
            RUNERR = .TRUE.
            GO TO 999
         END IF

         CALL STODBL(FIELD(11), ILEN_FLD, HRSZ, IMIT)
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208','HOUREMIS')
            RUNERR = .TRUE.
            GO TO 999
         END IF

      ELSE IF (SRCTYP(IS) .EQ. 'VOLUME' .AND. IFC.EQ.8) THEN
C*       Assign emission rate for volume sources
C*       Check logical variable indicating hourly sigmas, L_HRLYSIG
         IF (L_HRLYSIG(IS)) THEN
C*          WRITE Error Message; Hourly Sigmas must be used for all hours
            WRITE(DUMMY,'(I10.10)') FULLHRQ
            CALL ERRHDL(PATH,MODNAM,'E','345',DUMMY)
            HRQS = 0.0D0
            RUNERR = .TRUE.
            GO TO 999
         END IF

         CALL STODBL(FIELD(8), ILEN_FLD, HRQS, IMIT)
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208','HOUREMIS')
            RUNERR = .TRUE.
            GO TO 999
C ---    Check for large negative values, could be a missing indicator
         ELSE IF ( HRQS .LE. -90.0D0 ) THEN
C*          Assume emissions are missing; assign value of 0.0 and issue Warning;
C*          Note that AERMOD User's Guide (p. 3-38, 3-39) implies that blanks
C*          should be used for missing values.
            HRQS = 0.0D0
            WRITE(DUMMY,'(I10.10)') FULLHRQ
            CALL ERRHDL(PATH,MODNAM,'W','341',DUMMY)
         END IF

      ELSE IF ((SRCTYP(IS)(1:4).EQ.'AREA' .OR. SRCTYP(IS).EQ.'LINE'
     &          .OR. SRCTYP(IS).EQ.'RLINE' .OR. SRCTYP(IS).EQ.'RLINEXT')
     &        .AND. IFC.EQ.10) THEN
C*       Assign emission rate for AREA and LINE sources
C*       Assign logical variable indicating hourly sigmas, L_HRLYSIG
         IF (ILSAVE .EQ. 1) THEN
            L_HRLYSIG(IS) = .TRUE.
         ELSE IF (ILSAVE .GT. 1 .AND. .NOT. L_HRLYSIG(IS)) THEN
            WRITE(DUMMY,'(I10.10)') FULLHRQ
            CALL ERRHDL(PATH,MODNAM,'E','345',DUMMY)
            HRQS = 0.0D0
            RUNERR = .TRUE.
            GO TO 999
         END IF

         CALL STODBL(FIELD(8), ILEN_FLD, HRQS, IMIT)
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208','HOUREMIS')
            RUNERR = .TRUE.
            GO TO 999
C ---    Check for large negative values, could be a missing indicator
         ELSE IF ( HRQS .LE. -90.0D0 ) THEN
C*          Assume emissions are missing; assign value of 0.0 and issue Warning;
C*          Note that AERMOD User's Guide (p. 3-38, 3-39) implies that blanks
C*          should be used for missing values.
            HRQS = 0.0D0
            WRITE(DUMMY,'(I10.10)') FULLHRQ
            CALL ERRHDL(PATH,MODNAM,'W','341',DUMMY)
         END IF

         CALL STODBL(FIELD(9), ILEN_FLD, HRHS, IMIT)
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208','HOUREMIS')
            RUNERR = .TRUE.
            GO TO 999
         END IF

         CALL STODBL(FIELD(10), ILEN_FLD, HRSZ, IMIT)
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208','HOUREMIS')
            RUNERR = .TRUE.
            GO TO 999
         END IF

      ELSE IF ((SRCTYP(IS)(1:4).EQ.'AREA' .OR. SRCTYP(IS).EQ.'LINE'
     &      .OR. SRCTYP(IS).EQ.'RLINE' .OR. SRCTYP(IS).EQ.'RLINEXT')
     &      .AND. IFC.EQ.8) THEN
C*       Assign emission rate for AREA and LINE sources
C*       Check logical variable indicating hourly sigmas, L_HRLYSIG
         IF (L_HRLYSIG(IS)) THEN
C*          WRITE Error Message; Hourly Sigmas must be used for all hours
            WRITE(DUMMY,'(I10.10)') FULLHRQ
            CALL ERRHDL(PATH,MODNAM,'E','345',DUMMY)
            RUNERR = .TRUE.
            HRQS = 0.0D0
            GO TO 999
         END IF

         CALL STODBL(FIELD(8), ILEN_FLD, HRQS, IMIT)
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208','HOUREMIS')
            RUNERR = .TRUE.
            GO TO 999
C ---    Check for large negative values, could be a missing indicator
         ELSE IF ( HRQS .LE. -90.0D0 ) THEN
C*          Assume emissions are missing; assign value of 0.0 and issue Warning;
C*          Note that AERMOD User's Guide (p. 3-38, 3-39) implies that blanks
C*          should be used for missing values.
            HRQS = 0.0D0
            WRITE(DUMMY,'(I10.10)') FULLHRQ
            CALL ERRHDL(PATH,MODNAM,'W','341',DUMMY)
         END IF

      ELSE IF (SRCTYP(IS) .EQ. 'OPENPIT' .AND. IFC.EQ.8) THEN
C*       Assign emission rate for OPENPIT sources
         CALL STODBL(FIELD(8), ILEN_FLD, HRQS, IMIT)
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208','HOUREMIS')
            RUNERR = .TRUE.
            GO TO 999
C ---    Check for large negative values, could be a missing indicator
         ELSE IF ( HRQS .LE. -90.0D0 ) THEN
C*          Assume emissions are missing; assign value of 0.0 and issue Warning;
C*          Note that AERMOD User's Guide (p. 3-38, 3-39) implies that blanks
C*          should be used for missing values.
            HRQS = 0.0D0
            WRITE(DUMMY,'(I10.10)') FULLHRQ
            CALL ERRHDL(PATH,MODNAM,'W','341',DUMMY)
         END IF

      ELSE IF (SRCTYP(IS) .EQ. 'BUOYLINE' .AND. IFC.EQ.9) THEN
C*       Assign emission rate (field 8) and average buoyancy parameter
C        (field 9) for BUOYANT LINE sources

         CALL STODBL(FIELD(8), ILEN_FLD, HRQS, IMIT)
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208','HOUREMIS')
            RUNERR = .TRUE.
            GO TO 999
C ---    Check for large negative values, could be a missing indicator
         ELSE IF ( HRQS .LE. -90.0D0 ) THEN
C*          Assume emissions are missing; assign value of 0.0 and issue Warning;
C*          Note that AERMOD User's Guide (p. 3-38, 3-39) implies that blanks
C*          should be used for missing values.
            HRQS = 0.0D0
            WRITE(DUMMY,'(I10.10)') FULLHRQ
            CALL ERRHDL(PATH,MODNAM,'W','341',DUMMY)
         END IF

         CALL STODBL(FIELD(9), ILEN_FLD, HRFP, IMIT)
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208','HOUREMIS')
            RUNERR = .TRUE.
            GO TO 999
         END IF

C -------------------------- End of correct # parameters for source type
C -------------------------- Begin too many parameters for source type

      ELSE IF (SRCTYP(IS)(1:5).EQ.'POINT' .AND. IFC.GT.10) THEN
C*       Too many parameters - WRITE Error Message
C*       Assign zeros to all parameters
         WRITE(DUMMY,'(I10.10)') FULLHRQ
         CALL ERRHDL(PATH,MODNAM,'E','346',DUMMY)
         HRQS = 0.0D0
         HRTS = 0.0D0
         HRVS = 0.0D0
         RUNERR = .TRUE.

      ELSE IF (SRCTYP(IS).EQ.'VOLUME' .AND.
     &        ((L_HRLYSIG(IS) .AND. IFC.GT.11) .OR.
     &    (.NOT.L_HRLYSIG(IS) .AND. IFC.GT.8))) THEN
C*       Too many parameters - WRITE Error Message
C*       Assign zeros to all parameters
         WRITE(DUMMY,'(I10.10)') FULLHRQ
         CALL ERRHDL(PATH,MODNAM,'E','346',DUMMY)
         HRQS = 0.0D0
         HRHS = 0.0D0
         HRSY = 0.0D0
         HRSZ = 0.0D0
         RUNERR = .TRUE.

      ELSE IF ((SRCTYP(IS)(1:4).EQ.'AREA' .OR. SRCTYP(IS).EQ.'LINE'
     &     .OR. SRCTYP(IS).EQ.'RLINE' .OR. SRCTYP(IS) .EQ. 'RLINEXT')
     &         .AND. ((L_HRLYSIG(IS) .AND. IFC.GT.10) .OR.
     &           (.NOT.L_HRLYSIG(IS) .AND. IFC.GT.8))) THEN
C*       Too many parameters - WRITE Error Message
C*       Assign zeros to all parameters
         WRITE(DUMMY,'(I10.10)') FULLHRQ
         CALL ERRHDL(PATH,MODNAM,'E','346',DUMMY)
         HRQS = 0.0D0
         HRHS = 0.0D0
         HRSZ = 0.0D0
         RUNERR = .TRUE.

      ELSE IF (SRCTYP(IS).EQ.'OPENPIT' .AND. IFC.GT.8) THEN
C*       Too many parameters - WRITE Error Message
C*       Assign zeros to all parameters
         WRITE(DUMMY,'(I10.10)') FULLHRQ
         CALL ERRHDL(PATH,MODNAM,'E','346',DUMMY)
         HRQS = 0.0D0
         RUNERR = .TRUE.

      ELSE IF (SRCTYP(IS) .EQ. 'BUOYLINE' .AND. IFC.GT.9) THEN
C*       Too many parameters - WRITE Error Message
C*       Assign zeros to all parameters
         WRITE(DUMMY,'(I10.10)') FULLHRQ
         CALL ERRHDL(PATH,MODNAM,'E','346',DUMMY)
         HRQS = 0.0D0
         HRFP = 0.0D0
         RUNERR = .TRUE.

C -------------------------- End of too many parameters for source type
C -------------------------- Begin of too few parameters for source type

      ELSE IF (SRCTYP(IS)(1:5) .EQ. 'POINT') THEN
C*       Some missing parameters - WRITE Error Message
C*       Assign zeros to all parameters
         WRITE(DUMMY,'(I10.10)') FULLHRQ
         CALL ERRHDL(PATH,MODNAM,'E','384',DUMMY)
         HRQS = 0.0D0
         HRTS = 0.0D0
         HRVS = 0.0D0
         RUNERR = .TRUE.

      ELSE IF (SRCTYP(IS) .EQ. 'VOLUME' .AND.
     &        ((L_HRLYSIG(IS) .AND. IFC.LT.11) .OR.
     &    (.NOT.L_HRLYSIG(IS) .AND. IFC.LT.8))) THEN
C*       Some missing parameters - WRITE Error Message
C*       Assign zeros to all parameters
         WRITE(DUMMY,'(I10.10)') FULLHRQ
         CALL ERRHDL(PATH,MODNAM,'E','384',DUMMY)
         HRQS = 0.0D0
         HRHS = 0.0D0
         HRSY = 0.0D0
         HRSZ = 0.0D0
         RUNERR = .TRUE.

      ELSE IF ((SRCTYP(IS)(1:4).EQ.'AREA' .OR. SRCTYP(IS).EQ.'LINE'
     &     .OR. SRCTYP(IS).EQ.'RLINE' .OR. SRCTYP(IS) .EQ. 'RLINEXT')
     &        .AND. ((L_HRLYSIG(IS) .AND. IFC.LT.10) .OR.
     &    (.NOT.L_HRLYSIG(IS) .AND. IFC.LT.8))) THEN
C*       Some missing parameters - WRITE Error Message
C*       Assign zeros to all parameters
         WRITE(DUMMY,'(I10.10)') FULLHRQ
         CALL ERRHDL(PATH,MODNAM,'E','384',DUMMY)
         HRQS = 0.0D0
         HRHS = 0.0D0
         HRSZ = 0.0D0
         RUNERR = .TRUE.

      ELSE IF (SRCTYP(IS) .EQ. 'BUOYLINE') THEN
C*       Some missing parameters - WRITE Error Message
C*       Assign zeros to all parameters
         WRITE(DUMMY,'(I10.10)') FULLHRQ
         CALL ERRHDL(PATH,MODNAM,'E','384',DUMMY)
         HRQS = 0.0D0
         HRFP = 0.0D0
         RUNERR = .TRUE.

C ---------------------------- End of too few parameters for source type

      ELSE
C*       Problem processing HOUREMIS record - WRITE Error Message
C*       Assign zeros to emission rate
         WRITE(DUMMY,'(I10.10)') FULLHRQ
         CALL ERRHDL(PATH,MODNAM,'E','345',DUMMY)
         HRQS = 0.0D0
         RUNERR = .TRUE.

      END IF

      GO TO 999

C*    Write Error Message for Error Reading Hourly Emissions File
 99   CALL ERRHDL(PATH,MODNAM,'E','510','HOUREMIS')
      RUNERR = .TRUE.
      GO TO 999

888   CONTINUE

      EOF = .TRUE.

999   RETURN
      END

      SUBROUTINE HRQEXT (IS)
C***********************************************************************
C*                  HRQEXT Module of AERMOD
C*
C*         PURPOSE: To Assign Hourly Source Parameters
C*
C*         PROGRAMMER:  Jayant Hardikar, Roger Brode
C*
C*         DATE:    September 15, 1993
C*
C*         INPUTS:  Current Source Number Being Processed
C*
C*         OUTPUTS: Source Arrays
C*
C*         Revision History:
C*
C*         MODIFIED:  Included options for hourly-varying release height and
C*                    initial dispersion for RLINE and RLINEXT sources.
C*                    Wood 3/27/2019
C*
C*         MODIFIED:  Incorporated options to specify hourly-varying
C*                    release heights and initial dispersion coefficients
C*                    for VOLUME and AREA sources.
C*                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
C*
C*         MODIFIED:  Corrected processing of missing parameters for
C*                    point sources to assign all parameters to 0.0 if
C*                    any of the parameters are missing, in conformance
C*                    with Section 3.3.9 of the AERMOD User's Guide.
C*                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 01/24/2007
C*
C*         MODIFIED:  REMOVED THE 'POINT' SOURCE CONDITION, SO IT APPLIES
C*                    TO ALL SOURCE TYPES, EXCEPT SAVING THE TEMP & VEL
C*
C*         CALLED FROM:  HRLOOP
C************************************************************************
C*
C*    Variable Declarations
      USE MAIN1
      USE BUOYANT_LINE
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: IS

C*    Variable Initializations
      MODNAM = 'HRQEXT'

C*    Assign the Hourly Emission Parameters to the appropriate arrays
      IF (EVONLY) THEN

         AQS(IS) = EV_HRQS(IS,IHOUR)

         IF (SRCTYP(IS)(1:5) .EQ. 'POINT') THEN
            ATS(IS) = EV_HRTS(IS,IHOUR)
            AVS(IS) = EV_HRVS(IS,IHOUR)
         ELSE IF (SRCTYP(IS) .EQ. 'VOLUME' .AND. L_HRLYSIG(IS)) THEN
            AHS(IS)    = EV_HRHS(IS,IHOUR)
            ASYINI(IS) = EV_HRSY(IS,IHOUR)
            ASZINI(IS) = EV_HRSZ(IS,IHOUR)
         ELSE IF (SRCTYP(IS)(1:4) .EQ. 'AREA' .AND. L_HRLYSIG(IS)) THEN
            AHS(IS)    = EV_HRHS(IS,IHOUR)
            ASZINI(IS) = EV_HRSZ(IS,IHOUR)
         ELSE IF (SRCTYP(IS) .EQ. 'LINE' .AND. L_HRLYSIG(IS)) THEN
            AHS(IS)    = EV_HRHS(IS,IHOUR)
            ASZINI(IS) = EV_HRSZ(IS,IHOUR)
         ELSE IF (SRCTYP(IS) .EQ. 'RLINE' .AND. L_HRLYSIG(IS)) THEN
            AHS(IS)    = EV_HRHS(IS,IHOUR)
            ASZINI(IS) = EV_HRSZ(IS,IHOUR)
         ELSE IF (SRCTYP(IS) .EQ. 'RLINEXT' .AND. L_HRLYSIG(IS)) THEN
            AHS(IS)    = EV_HRHS(IS,IHOUR)
            ASZINI(IS) = EV_HRSZ(IS,IHOUR)
         ELSE IF (SRCTYP(IS) .EQ. 'BUOYLINE') THEN
            AFP(IS)    = EV_HRFP(IS,IHOUR)
         END IF

      ELSE

         AQS(IS) = HRQS

         IF (SRCTYP(IS)(1:5) .EQ. 'POINT') THEN
            ATS(IS) = HRTS
            AVS(IS) = HRVS
         ELSE IF (SRCTYP(IS) .EQ. 'VOLUME' .AND. L_HRLYSIG(IS)) THEN
            AHS(IS)    = HRHS
            ASYINI(IS) = HRSY
            ASZINI(IS) = HRSZ
         ELSE IF (SRCTYP(IS)(1:4) .EQ. 'AREA' .AND. L_HRLYSIG(IS)) THEN
            AHS(IS)    = HRHS
            ASZINI(IS) = HRSZ
         ELSE IF (SRCTYP(IS) .EQ. 'LINE' .AND. L_HRLYSIG(IS)) THEN
            AHS(IS)    = HRHS
            ASZINI(IS) = HRSZ
         ELSE IF (SRCTYP(IS) .EQ. 'RLINE' .AND. L_HRLYSIG(IS)) THEN
            AHS(IS)    = HRHS
            ASZINI(IS) = HRSZ
         ELSE IF (SRCTYP(IS) .EQ. 'RLINEXT' .AND. L_HRLYSIG(IS)) THEN
            AHS(IS)    = HRHS
            ASZINI(IS) = HRSZ
         ELSE IF (SRCTYP(IS) .EQ. 'BUOYLINE') THEN
            AFP(IS)    = HRFP
         END IF

      END IF

C*    Perform QA Error Checking on Source Parameters

      IF (SRCTYP(IS)(1:5) .EQ. 'POINT') THEN
         IF (ATS(IS) .EQ. 0.0D0) THEN
C*          Set Temperature to Small Negative Value for Ambient Releases
            ATS(IS) = -1.0D-5
         ELSE IF (ATS(IS) .GT. 2000.0D0) THEN
C*          WRITE Warning Message:  Exit Temp. > 2000K
            IF (.NOT. L_SkipMessages) THEN
               CALL ERRHDL(PATH,MODNAM,'W','320','HRTS')
            END IF
         ELSE IF ( (DABS(AQS(IS)).GT.0.0D0) .AND.
     &             (ATS(IS).GT.0.0D0) .AND. (ATS(IS).LT.200.0D0) .AND.
     &                                      (AVS(IS).GT.200.0D0) ) THEN
C*          Exit temp < 200K (about -100F) and exit velocity > 200m/s
C*          with non-zero emissions; Incorrect units may have been
C*          used or ATS and AVS may have been switched;
C*          WRITE Fatal Error Message
            IF (.NOT. L_SkipMessages) THEN
               CALL ERRHDL(PATH,MODNAM,'E','320','HRTS')
               RUNERR = .TRUE.
            END IF
         ELSE IF ( (DABS(AQS(IS)).GT.0.0D0) .AND.
     &             (ATS(IS).GT.0.0D0) .AND. (ATS(IS).LT.200.0D0) ) THEN
C*          Exit temp < 200K (about -100F) with non-zero emissions;
C*          Incorrect units may have been used or ATS and AVS may
C*          have been switched;
C*          WRITE Warnign Message
            IF (.NOT. L_SkipMessages) THEN
               CALL ERRHDL(PATH,MODNAM,'W','320','HRTS')
            END IF
         END IF

         IF (AVS(IS) .LT. 0.0D0) THEN
C*          WRITE Warning Message:  Negative or Zero Exit Velocity
            IF (.NOT. L_SkipMessages) THEN
               CALL ERRHDL(PATH,MODNAM,'W','325',SRCID(IS))
            END IF
C*          Set to Small Value to Avoid Zero-divide and Underflow
            AVS(IS) = 1.0D-5
         ELSE IF (AVS(IS) .LT. 1.0D-5) THEN
C*          Set to Small Value to Avoid Zero-divide and Underflow
            AVS(IS) = 1.0D-5
         ELSE IF (AVS(IS) .GT. 50.0D0) THEN
C*          WRITE Informational Message:  Exit Velocity > 50.0 m/s
            IF (.NOT. L_SkipMessages) THEN
               CALL ERRHDL(PATH,MODNAM,'I','320','HRVS')
            END IF
         END IF

      ELSE IF (SRCTYP(IS) .EQ. 'VOLUME') THEN
         IF (AHS(IS) .LT. 0.0D0) THEN
C           WRITE Error Message:  Negative Release Height
            CALL ERRHDL(PATH,MODNAM,'E','209','HRHS')
         ELSE IF (AHS(IS) .GT. 100.0D0) THEN
C           WRITE Warning Message:  Large Release Height (> 100M)
            IF (.NOT. L_SkipMessages) THEN
               CALL ERRHDL(PATH,MODNAM,'W','320','HRHS')
            END IF
         ELSE IF (AHS(IS) .GT. 3000.0D0) THEN
C           WRITE Error Message:  Large Release Height (> 3000M)
            CALL ERRHDL(PATH,MODNAM,'E','324',SRCID(IS))
            RUNERR = .TRUE.
         END IF

         IF (ASYINI(IS) .LT. 0.0D0) THEN
C           WRITE Warning Message:  Negative Initial Lateral Parameter
            CALL ERRHDL(PATH,MODNAM,'E','209','HRSY')
C           Set to Small Value to Avoid Zero-divide and Underflow
            ASYINI(IS) = 1.0D-5
         ELSE IF (ASYINI(IS) .LT. 1.0D-5) THEN
C           WRITE Warning Message:  Small Initial Lateral Parameter
            IF (.NOT. L_SkipMessages) THEN
               CALL ERRHDL(PATH,MODNAM,'W','320','HRSY')
            END IF
C           Set to Small Value to Avoid Zero-divide and Underflow
            ASYINI(IS) = 1.0D-5
         ELSE IF (ASYINI(IS) .GT. 200.0D0) THEN
C           WRITE Warning Message:  Large Initial Lateral Parameter (> 200m)
            IF (.NOT. L_SkipMessages) THEN
               CALL ERRHDL(PATH,MODNAM,'W','320','HRSY')
            END IF
         END IF

         IF (ASZINI(IS) .LT. 0.0D0) THEN
C           WRITE Warning Message:  Negative Initial Vertical Parameter
            CALL ERRHDL(PATH,MODNAM,'E','209','HRSZ')
C           Set to Small Value to Avoid Zero-divide and Underflow
            ASZINI(IS) = 1.0D-5
         ELSE IF (ASZINI(IS) .LT. 1.0D-5) THEN
C           WRITE Warning Message:  Small Initial Lateral Parameter
            IF (.NOT. L_SkipMessages) THEN
               CALL ERRHDL(PATH,MODNAM,'W','320','HRSZ')
            END IF
C           Set to Small Value to Avoid Zero-divide and Underflow
            ASZINI(IS) = 1.0D-5
         ELSE IF (ASZINI(IS) .GT. 200.0D0) THEN
C           WRITE Warning Message:  Large Initial Vertical Parameter (> 200m)
            IF (.NOT. L_SkipMessages) THEN
               CALL ERRHDL(PATH,MODNAM,'W','320','HRSZ')
            END IF
         END IF

      ELSE IF (SRCTYP(IS)(1:4).EQ.'AREA' .OR. SRCTYP(IS).EQ.'LINE'
     &         .OR. SRCTYP(IS).EQ.'RLINE'
     &         .OR. SRCTYP(IS).EQ.'RLINEXT') THEN
         IF (AHS(IS) .LT. 0.0D0) THEN
C           WRITE Error Message:  Negative Release Height
            CALL ERRHDL(PATH,MODNAM,'E','209','HRHS')
         ELSE IF (AHS(IS) .GT. 100.0D0) THEN
C           WRITE Warning Message:  Large Release Height (> 100M)
            IF (.NOT. L_SkipMessages) THEN
               CALL ERRHDL(PATH,MODNAM,'W','320','HRHS')
            END IF
         ELSE IF (AHS(IS) .GT. 3000.0D0) THEN
C           WRITE Error Message:  Large Release Height (> 3000M)
            CALL ERRHDL(PATH,MODNAM,'E','324',SRCID(IS))
            RUNERR = .TRUE.
         END IF

         IF (ASZINI(IS) .LT. 0.0D0) THEN
C           WRITE Warning Message:  Negative Initial Vertical Parameter
            CALL ERRHDL(PATH,MODNAM,'E','209','HRSZ')
C           Set to Small Value to Avoid Zero-divide and Underflow
            ASZINI(IS) = 1.0D-5
         ELSE IF (ASZINI(IS) .LT. 1.0D-5) THEN
C           WRITE Warning Message:  Small Initial Lateral Parameter
            IF (.NOT. L_SkipMessages) THEN
               CALL ERRHDL(PATH,MODNAM,'W','320','HRSZ')
            END IF
C           Set to Small Value to Avoid Zero-divide and Underflow
            ASZINI(IS) = 1.0D-5
         ELSE IF (ASZINI(IS) .GT. 200.0D0) THEN
C           WRITE Warning Message:  Large Initial Vertical Parameter (> 200m)
            IF (.NOT. L_SkipMessages) THEN
               CALL ERRHDL(PATH,MODNAM,'W','320','HRSZ')
            END IF
         END IF

      ELSE IF (SRCTYP(IS).EQ.'BUOYLINE') THEN
         IF (AFP(IS) .LT. 0.0D0) THEN
C           WRITE Error Message:  Negative Buoyancy Parameter
            CALL ERRHDL(PATH,MODNAM,'E','209','HRFP')
         END IF

      END IF

      RETURN
      END

      SUBROUTINE O3EXT(L_ReadErr,ReadErr,EndErr)
C***********************************************************************
C*                  O3EXT Module of AERMOD
C*
C*         PURPOSE: To extract hourly ozone data for PVMRM, OLM, TTRM,
C*                   or GRSM options
C*
C*         PROGRAMMER:  Roger W. Brode, PES, Inc.
C*
C*         DATE:    May 6, 2002
C*
C*         MODIFIED: Feb. 2021 for inclusion of Travel Time
C*         Reaction Method (TTRM)
C*
C*         INPUTS:
C*
C*         OUTPUTS:
C*
C*         CALLED FROM:  HRLOOP
C************************************************************************
C*
C*    Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      CHARACTER ReadErr*5, EndErr*5

      DOUBLE PRECISION :: O3SUB(6), O3TEMP, O3MAX24, O3MIN

      INTEGER :: IO3YR, IO3MN, IO3DY, IO3HR, IO3YR2, I
      INTEGER :: FULLO3HR(6)
      LOGICAL :: L_ReadErr

C*    Variable Initializations
      MODNAM  = 'O3EXT'
      ReadErr = ''
      EndErr  = ''
      L_ReadErr = .FALSE.
      FULLO3HR(:) = 0

C --- Initialize logical for missing data to FALSE
      O3MISS = .FALSE.

C*    Assign IOLINE counter to ILINE for passing to ERRHDL if needed
      ILINE = IOLINE
C*
C --- Read a record in all hourly O3 files, but only read concentation
C     based on the applicable file for the current sector
C     First initialize O3CONC to 0.0, and O3TEMP, O3SUB and O3MIN to -99.
      O3CONC   = 0.0D0
      O3TEMP   = -99.0D0
      O3SUB(:) = -99.0D0
      O3MIN    = -99.0D0

      DO I = 1, NUMO3Sects
C ---    Loop through O3SECTORs

C ---    Reinitialize O3SUB for this sector
         O3SUB(I) = -99.0D0

C ---    Check for non-hourly O3 values to substitute for missing data
         IF (L_O3VALUES(I)) THEN
            CALL OZONVALS(I,O3SUB(I))
         ELSE IF (L_O3VAL(I)) THEN
            O3SUB(I) = O3BACK(I)
         ELSE
            O3SUB(I) = 0.0D0
         END IF

C ---    Check for hour O3FILE for this sector
         IF (L_O3File(I)) THEN

C ---       Hourly O3 file available for current sector

            IF (I .EQ. IO3SECT) THEN
C ---          This is the applicable sector for this hour; read next hour of O3 data

C ---          Initialize ReadErr and EndErr for this sector
               WRITE(ReadErr,'(''SECT'',I1)') I

               IF (O3FORM(I) .EQ. 'FREE') THEN
                  READ(IO3UNT(I),*,ERR=99,END=9991) IO3YR, IO3MN,
     &                                              IO3DY, IO3HR,
     &                                              O3CONC
               ELSE
                  READ(IO3UNT(I),O3FORM(I),ERR=99,END=9991)
     &                                              IO3YR, IO3MN,
     &                                              IO3DY, IO3HR,
     &                                              O3CONC
               END IF

C ---          Check for use of 2-digit year in OZONEFIL file, adjust to 4-digit
C              year for comparison with FULLDATE based on met data file
               IF (IO3YR .LE. 99) THEN
                  IO3YR2 = IO3YR
                  IF (IO3YR2 .GE. ISTRT_WIND .AND.
     &                                 IO3YR2 .LE. 99) THEN
                     IO3YR  = ISTRT_CENT*100 + IO3YR2
                  ELSE IF (IO3YR2 .LT. ISTRT_WIND) THEN
                     IO3YR  = (ISTRT_CENT+1)*100 + IO3YR2
                  END IF
               END IF

C ---          Calculate full date for this hour of O3 data
               FULLO3HR(I) = IO3YR*1000000 + IO3MN*10000 + IO3DY*100
     &                                                   + IO3HR

               IF (O3CONC .GE. 0.0D0 .AND. O3CONC .LT. 900.0D0) THEN
C ---             Valid hourly value; convert to ug/m3 if needed
                  IF (O3FILUNITS .EQ. 'PPB') THEN
                     O3CONC = O3CONC * O3_PPB
                  ELSE IF (O3FILUNITS .EQ. 'PPM') then
                     O3CONC = O3CONC * O3_PPM
                  END IF
C ---             Valid hourly O3 value; check for application of O3MIN value
CRCO D074 Add check for NOMIN03 option to turn off minimum ozone 1/7/2021
                  IF (.NOT. NOMINO3) THEN
                     IF (STABLE) THEN
C                       Use min of 40 ppb (78.4ug/m3) and max from previous 24 hrs
                        O3MAX24 = MIN ( 78.40D0,
     &                           MAXVAL( O3_Max24hr(:,IO3SECT) ) )
C                       Adjust minimum O3 value based on OBULEN
                        IF (OBULEN .GT.  0.0D0 .AND.
     &                      OBULEN .LE. 50.0D0) THEN
                           O3MIN = O3MAX24
                        ELSE IF (OBULEN .GT. 250.0D0) THEN
                           O3MIN = 0.0D0
                        ELSE
                           O3MIN = O3MAX24 * (250.D0 - OBULEN) /200.D0
                        END IF
                     ELSE
                        O3MIN = -9.0D0
                     END IF
C ---                Save this hour's O3CONC (in ug/m3) to array of previous
C                    24 values, before applying minimum value
                     O3_Max24hr(IO3HR,IO3SECT) = O3CONC
                     O3CONC = MAX( O3CONC, O3MIN )
                  END IF
               ELSE IF (L_O3VALUES(IO3SECT) .OR.
     &                     L_O3VAL(IO3SECT)) THEN
C ---             Hourly O3 value is missing; assign O3SUB value based on
C                 O3VALUES or OZONEVAL inputs
                  O3CONC = O3SUB(IO3SECT)
C ---             Assign 0.0 to O3_Max24hr array for this hour
                  O3_Max24hr(IO3HR,IO3SECT) = 0.0D0
               ELSE
C ---             Assign O3MISS logical to TRUE
                  O3MISS = .TRUE.
C ---             Assign 0.0 to O3_Max24hr array for this sector
                  O3_Max24hr(IO3HR,IO3SECT) = 0.0D0
               END IF

               GO TO 9992

9991           CONTINUE
C              End-of-file reached, set logical flag
               EOF = .TRUE.

               WRITE(EndErr, '(''SECT'',I1)') I

9992           CONTINUE

            ELSE
C ---          This is not applicable sector for this hour; read record with temp data

C ---          Initialize ReadErr and EndErr for this sector
               WRITE(ReadErr,'(''SECT'',I1)') I

               IF (O3FORM(I) .EQ. 'FREE') THEN
                  READ(IO3UNT(I),*,ERR=99,END=9993) IO3YR, IO3MN,
     &                                              IO3DY, IO3HR,
     &                                              O3TEMP
               ELSE
                  READ(IO3UNT(I),O3FORM(I),ERR=99,END=9993)
     &                                              IO3YR, IO3MN,
     &                                              IO3DY, IO3HR,
     &                                              O3TEMP
               END IF

C ---          Check for use of 2-digit year in OZONEFIL file, adjust to 4-digit
C              year for comparison with FULLDATE based on met data file
               IF (IO3YR .LE. 99) THEN
                  IO3YR2 = IO3YR
                  IF (IO3YR2 .GE. ISTRT_WIND .AND.
     &                                 IO3YR2 .LE. 99) THEN
                     IO3YR  = ISTRT_CENT*100 + IO3YR2
                  ELSE IF (IO3YR2 .LT. ISTRT_WIND) THEN
                     IO3YR  = (ISTRT_CENT+1)*100 + IO3YR2
                  END IF
               END IF

C ---          Calculate full date for this hour of O3 data
               FULLO3HR(I) = IO3YR*1000000 + IO3MN*10000 + IO3DY*100
     &                                                   + IO3HR

               IF (O3TEMP .GE. 0.0D0 .AND. O3TEMP .LT. 900.0D0) THEN
C ---             Valid hourly value; convert to ug/m3 if needed
                  IF (O3FILUNITS .EQ. 'PPB') THEN
                     O3TEMP = O3TEMP * O3_PPB
                  ELSE IF (O3FILUNITS .EQ. 'PPM') then
                     O3TEMP = O3TEMP * O3_PPM
                  END IF
C ---             Save this hour's O3CONC (in ug/m3) to array of previous
C                 24 values for this sector
                  O3_Max24hr(IO3HR,I) = O3TEMP
               ELSE IF (L_O3VALUES(I) .OR.
     &                     L_O3VAL(I)) THEN
C ---             Hourly O3 value is missing; assign O3SUB value;
C                 these have already been converted to ug/m3
                  O3TEMP = O3SUB(I)
C ---             Assign 0.0 to O3_Max24hr array so that substituted value will
C                 not be used in determining max value from previous 24 hours
                  O3_Max24hr(IO3HR,I) = 0.0D0
               ELSE
C ---             Assign 0.0 to O3_Max24hr array
                  O3_Max24hr(IO3HR,I) = 0.0D0
               END IF

               GO TO 9994

9993           CONTINUE
C              End-of-file reached, set logical flag
               EOF = .TRUE.

               WRITE(EndErr, '(''SECT'',I1)') I

9994           CONTINUE

            END IF

         END IF

      END DO   ! END of O3Sector Loop

      IF (O3MISS) THEN
C ---    No O3 value available for this hour; assign 0.0 to O3CONC
C        and issue informational message
         O3CONC = 0.0D0
         IF (.NOT. L_SkipMessages) THEN
            WRITE(DUMMY,'(I10.10)') FULLDATE
            CALL ERRHDL(PATH,MODNAM,'I','459',DUMMY)
         END IF
      END IF

C*    Check for Date and Time Consistency Across all Sectors; If Failed, Issue Fatal Error
      DO I = 1, NUMO3Sects
         IF (L_O3File(I)) THEN
            IF (FULLDATE .NE. FULLO3HR(I)) THEN
C*             WRITE Error Message - Date mismatch
               WRITE(DUMMY,'(I10.10,''S'',I1)') FULLDATE, I
               CALL ERRHDL(PATH,MODNAM,'E','457',DUMMY)
C*             Set RUNERR logical and skip to end
               RUNERR = .TRUE.
               GO TO 1000
            END IF
         END IF
      END DO

C     Date/Time consistency checks were ok; skip to end
      GO TO 1000

C*    Write Error Message for Error Reading Hourly Ozone File
 99   CONTINUE
      L_ReadErr = .TRUE.
      WRITE(ReadErr,'(''SECT'',I1)') I
      CALL ERRHDL(PATH,MODNAM,'E','510',ReadErr)
      RUNERR = .TRUE.

      GO TO 1000

      CONTINUE
C     End-of-file reached, set logical flag
      EOF = .TRUE.

C --- End of file reached on O3 file; check FULLDATE vs. FULLO3HR before returning
C*    for Date and Time Consistency Across all Sectors; If Failed, Issue Fatal Error
      DO I = 1, NUMO3Sects
         IF (L_O3File(I)) THEN
            IF (FULLDATE .NE. FULLO3HR(I)) THEN
C*             WRITE Error Message - Date mismatch
               WRITE(DUMMY,'(I10.10,''S'',I1)') FULLDATE, I
               CALL ERRHDL(PATH,MODNAM,'E','457',DUMMY)
               RUNERR = .TRUE.
            END IF
         END IF
      END DO

1000  RETURN
      END

      SUBROUTINE BGEXT(L_ReadErr,ReadErr,EndErr)
C***********************************************************************
C*                  BGEXT Module of AERMOD
C*
C*         PURPOSE: To extract hourly background concentrations
C*
C*         PROGRAMMER:  Roger W. Brode
C*
C*         DATE:        February 28, 2011
C*
C*         INPUTS:
C*
C*         OUTPUTS:
C*
C*         CALLED FROM:  HRLOOP
C************************************************************************
C*
C*    Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      CHARACTER ReadErr*5, EndErr*5

      INTEGER :: IBGYR, IBGMN, IBGDY, IBGHR, IBGYR2, I
      INTEGER :: FULLBGHR(6)
      DOUBLE PRECISION :: BGHrVal, BGSUB(6)
      LOGICAL :: L_ReadErr

C*    Variable Initializations
      MODNAM  = 'BGEXT'
      ReadErr = ''
      EndErr  = ''
      L_ReadErr = .FALSE.

C*    Assign IBLINE counter to ILINE for passing to ERRHDL if needed
      ILINE = IBLINE
C*
C --- Read a record in all hourly BG files, but only read concentation
C     based on the applicable file for the current sector
C     First initialize BGHrVal to -99.0
      BGHrVal = -99.0D0

C --- Loop through all BACKGRND Sectors
      DO I = 1, NUMBGSects

C ---    Reinitialize BGSUB for this sector
         BGSUB(I) = 0.0D0

C ---    Check for temporally-varying background to substitute for missing hours
         IF (L_BGValues(I)) THEN
            CALL BGVAL(I,BGSUB(I))
         ELSE
C           No temporally-varying background for this sector; set BGSUB = 0.0
            BGSUB(I) = 0.0D0
         END IF

C ---    Check for HOURLY BACKGRDN data for this sector and read the data
         IF (L_BGFile(I)) THEN

C ---       Hourly BG file available for current sector

C ---       Check for whether this is the applicable sector for this hour
            IF (I .EQ. IBGSECT) THEN

C ---          Initialize ReadErr and EndErr for this sector
               WRITE(ReadErr,'(''SECT'',I1)') I

               IF (BGFORM(I) .EQ. 'FREE') THEN
                  READ(IBGUNT(I),*,ERR=99,END=9991) IBGYR,IBGMN,
     &                                              IBGDY,IBGHR,
     &                                              BGHrVal
               ELSE
                  READ(IBGUNT(I),BGFORM(I),ERR=99,END=9991)
     &                                              IBGYR,IBGMN,
     &                                              IBGDY,IBGHR,
     &                                              BGHrVal
               END IF

               IF (BGHrVal .GT. 0.0D0) THEN
C ---             Valid hourly value; convert to ug/m3 if needed
                  IF (POLLUT .EQ. 'NO2') THEN
                     IF (BackUnits .EQ. 'PPB') THEN
                        BGHrVal = BGHrVal / NO2_PPB
                     ELSE IF (BackUnits .EQ. 'PPM') THEN
                        BGHrVal = BGHrVal / NO2_PPM
                     END IF
                  ELSE IF (POLLUT .EQ. 'SO2') THEN
                     IF (BackUnits .EQ. 'PPB') THEN
                        BGHrVal = BGHrVal / SO2_PPB
                     ELSE IF (BackUnits .EQ. 'PPM') THEN
                        BGHrVal = BGHrVal / SO2_PPM
                     END IF
                  ELSE IF (POLLUT .EQ. 'CO') THEN
                     IF (BackUnits .EQ. 'PPB') THEN
                        BGHrVal = BGHrVal * CO_PPB
                     ELSE IF (BackUnits .EQ. 'PPM') THEN
                        BGHrVal = BGHrVal * CO_PPM
                     END IF
                  END IF
               END IF

C ---          Check for use of 2-digit year in BACKGRND file, adjust to 4-digit
C              year for comparison with FULLDATE based on met data file
               IF (IBGYR .LE. 99) THEN
                  IBGYR2 = IBGYR
                  IF (IBGYR2 .GE. ISTRT_WIND .AND.
     &                                 IBGYR2 .LE. 99) THEN
                     IBGYR  = ISTRT_CENT*100 + IBGYR2
                  ELSE IF (IBGYR2 .LT. ISTRT_WIND) THEN
                     IBGYR  = (ISTRT_CENT+1)*100 + IBGYR2
                  END IF
               END IF

C*             Assign full date for this HOURLY BACKGRND file
               FULLBGHR(I) = IBGYR*1000000 + IBGMN*10000 + IBGDY*100 +
     &                                                     IBGHR

               GO TO 9992

9991           CONTINUE
C              End-of-file reached, set logical flag
               EOF = .TRUE.

               WRITE(EndErr, '(''SECT'',I1)') I

9992           CONTINUE

            ELSE
C ---          This is not applicable sector for this hour; read record without data
C
C ---          Initialize ReadErr and EndErr for this sector
               WRITE(ReadErr,'(''SECT'',I1)') I

               IF (BGFORM(I) .EQ. 'FREE') THEN
                  READ(IBGUNT(I),*,ERR=99,END=9993) IBGYR, IBGMN,
     &                                              IBGDY, IBGHR
               ELSE
                  READ(IBGUNT(I),BGFORM(I),ERR=99,END=9993)
     &                                              IBGYR, IBGMN,
     &                                              IBGDY, IBGHR
               END IF

C ---          Check for use of 2-digit year in BACKGRND file, adjust to 4-digit
C              year for comparison with FULLDATE based on met data file
               IF (IBGYR .LE. 99) THEN
                  IBGYR2 = IBGYR
                  IF (IBGYR2 .GE. ISTRT_WIND .AND.
     &                                 IBGYR2 .LE. 99) THEN
                     IBGYR  = ISTRT_CENT*100 + IBGYR2
                  ELSE IF (IBGYR2 .LT. ISTRT_WIND) THEN
                     IBGYR  = (ISTRT_CENT+1)*100 + IBGYR2
                  END IF
               END IF

C*             Assign full date for this HOURLY BACKGRND file
               FULLBGHR(I) = IBGYR*1000000 + IBGMN*10000 + IBGDY*100 +
     &                                                     IBGHR

               GO TO 9994

9993           CONTINUE
C              End-of-file reached, set logical flag
               EOF = .TRUE.

               WRITE(EndErr, '(''SECT'',I1)') I

9994           CONTINUE

            END IF

         END IF   ! End of L_BGFile IF Block

      END DO      ! End of BGSector Loop

C*    Check for Date and Time Consistency Across all Sectors; If Failed, Issue Fatal Error
      DO I = 1, NUMBGSects
         IF (L_BGFile(I)) THEN
            IF (FULLDATE .NE. FULLBGHR(I)) THEN
C*             WRITE Error Message - Date mismatch
               WRITE(DUMMY,'(I10.10,''s'',I1)') FULLDATE, I
               CALL ERRHDL(PATH,MODNAM,'E','454',DUMMY)
               RUNERR = .TRUE.
            END IF
         END IF
      END DO

      IF (RUNERR) RETURN

      IF (BGHrVal .LT. 0.0D0) THEN
C ---    Hourly BGCONC is missing; look for substitution values
         IF (IBGSECT .GT. 0) THEN
C ---       Valid BGSECT defined, check for hourly values for this
C           sector, and then for non-hourly values to substitute
            IF (L_BGFile(IBGSECT)) THEN
               IF (L_BGValues(IBGSECT)) THEN
C                 Hourly background value is missing but non-hourly
C                 values have been specified for substitution,
C                 which were processed in subroutine BGVAL;
                  BGCONC = BGSUB(IBGSECT)
C                 Write informational message
                  WRITE(DUMMY,'(I10.10,''s'',I1)') FULLDATE, IBGSECT
                  CALL ERRHDL(PATH,MODNAM,'I','453',DUMMY)
C                 Increment counter for number of missing BGval substitutions
                  NSubBGHOUR = NSubBGHOUR + 1
               ELSE
C                 Hourly background value is missing for this sector and no
C                 non-hourly values specified for substitution;
C                 Write Error message
                  WRITE(DUMMY,'(I10.10,''s'',I1)') FULLDATE, IBGSECT
                  CALL ERRHDL(PATH,MODNAM,'E','452',DUMMY)
                  RUNERR = .TRUE.
                  GO TO 1000
               END IF
            ELSE
               IF (L_BGValues(IBGSECT)) THEN
C                 Hourly background value is missing but non-hourly
C                 values have been specified for substitution,
C                 which were processed in subroutine BGVAL;
                  BGCONC = BGSUB(IBGSECT)
               END IF
            END IF
         ELSE
C ---       IBGSECT = 0 due to calm or msg hr; BGSUB = 0.0D0; exit
            BGCONC = 0.0D0
         END IF
      ELSE
         BGCONC = BGHrVal
      END IF

      RETURN

C*    Write Error Message for Error Reading Hourly Background File
 99   CONTINUE
      L_ReadErr = .TRUE.

      RETURN

      CONTINUE
C     End-of-file reached, set logical flag
      EOF = .TRUE.

C --- End of file reached on hourly BACKGRND file; check FULLDATE vs. FULLBGHR
C     before returning
      DO I = 1, NUMBGSects
         IF (L_BGFile(I)) THEN
            IF (FULLDATE .NE. FULLBGHR(I)) THEN
C*             WRITE Error Message - Date mismatch
               WRITE(DUMMY,'(I10.10,''S'',I1)') FULLDATE, I
               CALL ERRHDL(PATH,MODNAM,'E','454',DUMMY)
               RUNERR = .TRUE.
            END IF
         END IF
      END DO

1000  RETURN
      END

      SUBROUTINE NOXEXT(L_ReadErr,ReadErr,EndErr)
C***********************************************************************
C*                  NOXEXT Module of AERMOD
C*
C*         PURPOSE: To extract hourly NOx data for GRSM options
C*
C*         PROGRAMMER:  CERC
C*
C*         DATE:    November 2020
C*
C*         INPUTS:
C*
C*         OUTPUTS:
C*
C*         CALLED FROM:  HRLOOP
C************************************************************************
C*
C*    Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      CHARACTER ReadErr*5, EndErr*5

      DOUBLE PRECISION :: NOXSUB(6), NOXTEMP, NOXMIN

      INTEGER :: INOXYR, INOXMN, INOXDY, INOXHR, INOXYR2, I
      INTEGER :: FULLNOXHR(6)
      LOGICAL :: L_ReadErr

C*    Variable Initializations
      MODNAM  = 'NOXEXT'
      ReadErr = ''
      EndErr  = ''
      L_ReadErr = .FALSE.
      FULLNOXHR(:) = 0

C --- Initialize logical for missing data to FALSE
      NOXMISS = .FALSE.

C*    Assign INOXLINE counter to ILINE for passing to ERRHDL if needed
      ILINE = INOXLINE
C*
C --- Read a record in all hourly NOx files, but only read concentation
C     based on the applicable file for the current sector
C     First initialize NOXBGCONC, NOXMIN to 0.0, and NOXTEMP, NOXSUB to  -99.
      NOXBGCONC   = 0.0D0
      NOXTEMP   = -99.0D0
      NOXSUB(:) = -99.0D0
      NOXMIN    = 0.0D0

      DO I = 1, NUMNOxSects
C ---    Loop through NOXSECTORs

C ---    Reinitialize NOXSUB for this sector
         NOXSUB(I) = -99.0D0

C ---    Check for non-hourly NOX values to substitute for missing data
         IF (L_NOX_VALS(I)) THEN
            CALL VARYNOXVALS(I,NOXSUB(I))
         ELSE IF (L_NOXVALUE(I)) THEN
            NOXSUB(I) = NOXBACK(I)
         ELSE
            NOXSUB(I) = 0.0D0
         END IF

C ---    Check for hour NOXFILE for this sector
         IF (L_NOxFile(I)) THEN

C ---       Hourly NOx file available for current sector

            IF (I .EQ. INOXSECT) THEN
C ---          This is the applicable sector for this hour; read next hour of NOx data

C ---          Initialize ReadErr and EndErr for this sector
               WRITE(ReadErr,'(''SECT'',I1)') I

               IF (NOXFORM(I) .EQ. 'FREE') THEN
                  READ(INOXUNT(I),*,ERR=99,END=9991) INOXYR, INOXMN,
     &                                              INOXDY, INOXHR,
     &                                              NOXBGCONC
               ELSE
                  READ(INOXUNT(I),NOXFORM(I),ERR=99,END=9991)
     &                                              INOXYR, INOXMN,
     &                                              INOXDY, INOXHR,
     &                                              NOXBGCONC
               END IF

C ---          Check for use of 2-digit year in NOX_FILE file, adjust to 4-digit
C              year for comparison with FULLDATE based on met data file
               IF (INOXYR .LE. 99) THEN
                  INOXYR2 = INOXYR
                  IF (INOXYR2 .GE. ISTRT_WIND .AND.
     &                                 INOXYR2 .LE. 99) THEN
                     INOXYR  = ISTRT_CENT*100 + INOXYR2
                  ELSE IF (INOXYR2 .LT. ISTRT_WIND) THEN
                     INOXYR  = (ISTRT_CENT+1)*100 + INOXYR2
                  END IF
               END IF

C ---          Calculate full date for this hour of NOx data
               FULLNOXHR(I) = INOXYR*1000000 + INOXMN*10000 + INOXDY*100
     &                                                   + INOXHR

               IF(NOXBGCONC .GE. 0.0D0)THEN
C ---             Valid hourly value; convert to ug/m3 if needed
C ---             using NO2 factors (NOx expressed as 'NOx as NO2')
                  IF (NOXFILUNITS .EQ. 'PPB') THEN
                     NOXBGCONC = NOXBGCONC / NO2_PPB
                  ELSE IF (NOXFILUNITS .EQ. 'PPM') THEN
                     NOXBGCONC = NOXBGCONC / NO2_PPM
                  END IF
                  !Ensure non-negative
                  NOXBGCONC = MAX( NOXBGCONC, NOXMIN )
               ELSE IF (L_NOX_VALS(INOXSECT) .OR.
     &                     L_NOXVALUE(INOXSECT)) THEN
C ---             Hourly NOx value is missing; assign NOXSUB value based on
C                 NOX_VALS or NOXVALUE inputs
                  NOXBGCONC = NOXSUB(INOXSECT)
               ELSE
C ---             Assign NOXMISS logical to TRUE
                  NOXMISS = .TRUE.
               END IF

               GO TO 9992

9991           CONTINUE
C              End-of-file reached, set logical flag
               EOF = .TRUE.

               WRITE(EndErr, '(''SECT'',I1)') I

9992           CONTINUE

            ELSE
C ---          This is not applicable sector for this hour; read record with temp data

C ---          Initialize ReadErr and EndErr for this sector
               WRITE(ReadErr,'(''SECT'',I1)') I

               IF (NOXFORM(I) .EQ. 'FREE') THEN
                  READ(INOXUNT(I),*,ERR=99,END=9993) INOXYR, INOXMN,
     &                                              INOXDY, INOXHR,
     &                                              NOXTEMP
               ELSE
                  READ(INOXUNT(I),NOXFORM(I),ERR=99,END=9993)
     &                                              INOXYR, INOXMN,
     &                                              INOXDY, INOXHR,
     &                                              NOXTEMP
               END IF

C ---          Check for use of 2-digit year in OZONEFIL file, adjust to 4-digit
C              year for comparison with FULLDATE based on met data file
               IF (INOXYR .LE. 99) THEN
                  INOXYR2 = INOXYR
                  IF (INOXYR2 .GE. ISTRT_WIND .AND.
     &                                 INOXYR2 .LE. 99) THEN
                     INOXYR  = ISTRT_CENT*100 + INOXYR2
                  ELSE IF (INOXYR2 .LT. ISTRT_WIND) THEN
                     INOXYR  = (ISTRT_CENT+1)*100 + INOXYR2
                  END IF
               END IF

C ---          Calculate full date for this hour of O3 data
               FULLNOXHR(I) = INOXYR*1000000 + INOXMN*10000 + INOXDY*100
     &                                                   + INOXHR

               IF (NOXTEMP .GE. 0.0D0) THEN
C ---             Valid hourly value; convert to ug/m3 if needed
C ---             using NO2 factors (NOx expressed as 'NOx as NO2')
                  IF (NOXFILUNITS .EQ. 'PPB') THEN
                     NOXTEMP = NOXTEMP / NO2_PPB
                  ELSE IF (NOXFILUNITS .EQ. 'PPM') then
                     NOXTEMP = NOXTEMP / NO2_PPM
                  END IF
               ELSE IF (L_NOX_VALS(I) .OR.
     &                     L_NOXVALUE(I)) THEN
C ---             Hourly NOx value is missing; assign NOXSUB value;
C                 these have already been converted to ug/m3
                  NOXTEMP = NOXSUB(I)
               END IF

               GO TO 9994

9993           CONTINUE
C              End-of-file reached, set logical flag
               EOF = .TRUE.

               WRITE(EndErr, '(''SECT'',I1)') I

9994           CONTINUE

            END IF

         END IF

      END DO   ! END of NOxSector Loop

      IF (NOXMISS) THEN
C ---    No NOx value available for this hour; assign 0.0 to NOXCONC
C        and issue informational message
         NOXBGCONC = 0.0D0
         IF (.NOT. L_SkipMessages) THEN
            WRITE(DUMMY,'(I10.10)') FULLDATE
            CALL ERRHDL(PATH,MODNAM,'I','609',DUMMY)
         END IF
      END IF

C*    Check for Date and Time Consistency Across all Sectors; If Failed, Issue Fatal Error
      DO I = 1, NUMNOxSects
         IF (L_NOxFile(I)) THEN
            IF (FULLDATE .NE. FULLNOXHR(I)) THEN
C*             WRITE Error Message - Date mismatch
               WRITE(DUMMY,'(I10.10,''S'',I1)') FULLDATE, I
               CALL ERRHDL(PATH,MODNAM,'E','608',DUMMY)
C*             Set RUNERR logical and skip to end
               RUNERR = .TRUE.
               GO TO 1000
            END IF
         END IF
      END DO

C     Date/Time consistency checks were ok; skip to end
      GO TO 1000

C*    Write Error Message for Error Reading Hourly NOx File
 99   CONTINUE
      L_ReadErr = .TRUE.
      WRITE(ReadErr,'(''SECT'',I1)') I
      CALL ERRHDL(PATH,MODNAM,'E','510',ReadErr)
      RUNERR = .TRUE.

      GO TO 1000

CRCO 3/4/2021 Label Unused
C 999  CONTINUE
C     End-of-file reached, set logical flag
      EOF = .TRUE.

C --- End of file reached on NOX file; check FULLDATE vs. FULLNOXHR before returning
C*    for Date and Time Consistency Across all Sectors; If Failed, Issue Fatal Error
      DO I = 1, NUMNOxSects
         IF (L_NOXFile(I)) THEN
            IF (FULLDATE .NE. FULLNOXHR(I)) THEN
C*             WRITE Error Message - Date mismatch
               WRITE(DUMMY,'(I10.10,''S'',I1)') FULLDATE, I
               CALL ERRHDL(PATH,MODNAM,'E','608',DUMMY)
               RUNERR = .TRUE.
            END IF
         END IF
      END DO

1000  RETURN
      END SUBROUTINE NOXEXT

      SUBROUTINE ERRHDL(PATHWY,MODNAM,INERTP,INERCD,INPMSG)
C***********************************************************************
C                 ERRHDL Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: A General Error Handling Procedure
C
C        PROGRAMMER: Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:  Sets upper limit on line number included in error
C                   message to avoid overflowing the field; also increased
C                   field length for last message field from 8 to 12 to
C                   accommodate 12 character source IDs.
C                   R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
C
C        INPUTS:  Error Code, Occur Locations
C
C        OUTPUTS: Error Message, Error Statistics..etc.
C
C        CALLED FROM:  (This Is An Utility Programm)
C***********************************************************************
C
C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE

      INTEGER :: I, ILINE_PRT
      CHARACTER ERRMG1*50, PATHWY*2, INERTP*1, INERCD*3, ICODE*3,
     &          INPMSG*(*), MODNAM*(*), TMPMOD*6, TMPMSG*12
      LOGICAL FOUND

C     Variable Initializations
      IERROR = IERROR + 1
      FOUND = .FALSE.
      I = 1

C     Check for Occurrence of 'E' Error Type, and Set FATAL Switch
      IF (INERTP .EQ. 'E') THEN
         FATAL = .TRUE.
         NFATAL = NFATAL + 1
         IF (NFATAL .EQ. 999) THEN
C           Number Of Fatal Errors Has Reached Limit of 999
            ERRMG1 = 'Number of Fatal Errors Has Reached Limit of 999'
            TMPMOD = 'ERRHDL'
            ICODE  = '999'
            TMPMSG = ' '
            ILINE_PRT = MIN(ILINE,99999999)
            WRITE(IERUNT,1111) PATHWY,INERTP,ICODE,ILINE_PRT,TMPMOD,
     &                         ERRMG1,TMPMSG
            GO TO 999
         ELSE IF (NFATAL .GT. 999) THEN
C           Skip Any More Error WRITEs
            GO TO 999
         END IF
      END IF

C     Go To Match The Error Massage
      DO WHILE (.NOT.FOUND .AND. I.LE.IERRN)
         IF (INERCD .EQ. ERRCOD(I)) THEN
            ERRMG1 = ERRMSG(I)
            FOUND = .TRUE.
         END IF
         I = I + 1
      END DO

      IF (.NOT. FOUND) THEN
         WRITE(ERRMG1,1001)
 1001    FORMAT('SYSTEM ERROR: MESSAGE NOT FOUND FOR THIS NUMBER!')
      END IF

C --- Set upper limit on ILINE to avoid write error
      ILINE_PRT = MIN(ILINE,99999999)
C     Write Out The Error Message
      WRITE(IERUNT,1111) PATHWY,INERTP,INERCD,ILINE_PRT,
     &                   MODNAM(1:MIN(LEN_TRIM(MODNAM),12)),ERRMG1,
     &                   INPMSG(1:MIN(LEN_TRIM(INPMSG),12))
 1111 FORMAT(A2,1X,A1,A3,I8,1X,A12,': ',A50,1X,A12)

 999  RETURN
      END

      SUBROUTINE TERRST
C***********************************************************************
C                 TERRST Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: To Determine Total Error/Message Statistics
C
C        PROGRAMMER:  Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:  Corrected issues with determining number of calm
C                   and/or missing hours only for the data period that
C                   is processed.  Also increased field length for last
C                   message field from 8 to 12 to accommodate 12 character
C                   source IDs.
C                   R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
C
C        MODIFIED:  To remove reference to legacy ISCST3 option (HE>ZI)
C                   for plume height above mixing height.
C                   Determine number of calm and/or missing hours only
C                   for the data period processed, and determine number
C                   of hours processed.
C                   R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
C
C        INPUTS:  Error Message Temporary File
C
C        OUTPUTS: Total Number of Messages by Message Type
C
C        CALLED FROM:  This is A Utility Program
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: IERRLN
C     JAT D065 7/22/21 ICHR, ICDAT8 NOT USED
C      INTEGER :: ICYR, ICMN, ICDY, ICHR, ICDAT, ICDAT8, ICJDY
      INTEGER :: ICYR, ICMN, ICDY, ICDAT, ICJDY
      CHARACTER ERRTP*1, ERRCD*3, ERRMG1*50, ERRMG2*12, INPFLD3*3
      CHARACTER INPFLD12*12

C     Variable Initialization
      MODNAM = 'TERRST'
      IFTL = 0
      IWRN = 0
      INFO = 0
      ICLM = 0
      IMSG = 0
      ICYR = 0
      ICMN = 0
      ICDY = 0
C     JAT D065 7/22/21 ICHR NOT USED
C      ICHR = 0
      ICDAT = 0
C     JAT D065 7/22/21 ICDAT8 NOT USED
C      ICDAT8 = 0
      ICJDY  = 0
      DNUM   = 0.0D0
      EOF = .FALSE.

C     Rewind the Temporary Error/Message File
      REWIND IERUNT

      DO WHILE (.NOT. EOF)
         READ(IERUNT,1116,END=99,ERR=9999) PATH,ERRTP,ERRCD,IERRLN,
     &                                     MODNAM,ERRMG1,ERRMG2

C        Convert error code from character string to number
         INPFLD3 = ERRCD
         CALL STONUM(INPFLD3,3,FNUM,IMIT)

         IF (ERRTP .EQ. 'E') THEN
            IFTL = IFTL + 1
         ELSE IF (ERRTP .EQ. 'W') THEN
            IWRN = IWRN + 1
         ELSE IF (ERRTP .EQ. 'I') THEN
            INFO = INFO + 1
            IF (NINT(FNUM) .EQ. 440) THEN
C ---          Determine if this calm hour is during period
C              of data processed; convert date field from
C              character string to number (using double precision)
               INPFLD12 = ERRMG2
               CALL STODBL(INPFLD12,12,DNUM,IMIT)
               ICDAT = IDNINT(DNUM)
               ICYR  = ICDAT/1000000
               IF (RSTINP .OR. IMSTAT(6).EQ.0 .OR.
     &            (ICDAT.GE.ISDATE .AND. ICDAT.LE.IEDATE) ) THEN
C ---             This hour is between start and end dates,
C                 or this is a restarted model run, now
C                 determine Julian day and check IPROC array
C                 for DAYRANGE.
                  ICMN = (ICDAT/10000) - (ICDAT/1000000)*100
                  ICDY = (ICDAT/100) - (ICDAT/10000)*100
                  IF (ICMN.GT.0 .AND. ICDY.GT.0) THEN
                     CALL JULIAN(ICYR,ICMN,ICDY,ICJDY)
                  ELSE
                     ICJDY = 0
                     CYCLE
                  END IF
                  IF (IPROC(ICJDY) .EQ. 1) THEN
C ---                Message for Calm Hour, Increment Calm Counter
                     ICLM = ICLM + 1
                  END IF
               END IF
            ELSE IF (NINT(FNUM) .EQ. 460) THEN
C ---          Determine if this missing hour is during period
C              of data processed;  convert date field from
C              character string to number (using double precision)
               INPFLD12 = ERRMG2
               CALL STODBL(INPFLD12,12,DNUM,IMIT)
               ICDAT = IDNINT(DNUM)
               ICYR   = ICDAT/1000000
               IF (RSTINP .OR. IMSTAT(6).EQ.0 .OR.
     &            (ICDAT.GE.ISDATE .AND. ICDAT.LE.IEDATE) ) THEN
C ---             This hour is between start and end dates,
C                 or this is a restarted model run, now
C                 determine Julian day and check IPROC array
C                 for DAYRANGE.
                  ICMN = (ICDAT/10000) - (ICDAT/1000000)*100
                  ICDY = (ICDAT/100) - (ICDAT/10000)*100
                  IF (ICMN.GT.0 .AND. ICDY.GT.0) THEN
                     CALL JULIAN(ICYR,ICMN,ICDY,ICJDY)
                  ELSE
                     ICJDY = 0
                     CYCLE
                  END IF
                  IF (IPROC(ICJDY) .EQ. 1) THEN
C ---                Message for Missing Hour, Increment Missing Counter
                     IMSG = IMSG + 1
                  END IF
               END IF
            END IF
         END IF

         GO TO 11
 99      EOF = .TRUE.
 11      CONTINUE
      END DO

 1116 FORMAT(A2,1X,A1,A3,I8,1X,A12,2X,A50,1X,A12)

C     Use BACKSPACE To Reposition Temporary Error Message File Ahead of EOF;
C     This Is Needed in Order To Allow For Additional Message Writes
      BACKSPACE IERUNT

      GO TO 1000

C     WRITE Error Message: Error Reading Temp Error Message File
 9999 CALL ERRHDL(PATH,MODNAM,'E','510','ERRORMSG')

 1000 RETURN
      END

      SUBROUTINE SUMTBL(IOUNT)
C***********************************************************************
C                 SUMTBL Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: To Print Out The Error Summary Table
C
C        PROGRAMMER:  Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:  Increased field length for last message field from
C                   8 to 12 to accommodate 12 character source IDs.
C                   R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
C
C        MODIFIED:  To remove reference to legacy ISCST3 option (HE>ZI)
C                   for plume height above mixing height.
C                   Include the number of hours processed from the
C                   meteorological data file.
C                   R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
C
C        INPUTS:  Error Message Temporary File
C
C        OUTPUTS: Summary Of Errors
C
C        CALLED FROM:  This is A Utility Program
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      REAL    :: PERCENT
      INTEGER :: J, IERRLN, IOUNT
      CHARACTER ERRTP*1, ERRCD*3, ERRMG1*50, ERRMG2*12

C     Variable Initialization
      MODNAM = 'SUMTBL'

C     Write Out The Total Error Statistics
      WRITE(IOUNT,*) ' --------- Summary of Total Messages --------'
      WRITE(IOUNT,*) ' '
      WRITE(IOUNT,9014) IFTL
 9014 FORMAT(' A Total of   ',I10,' Fatal Error Message(s)')
      WRITE(IOUNT,9015) IWRN
 9015 FORMAT(' A Total of   ',I10,' Warning Message(s)')
      WRITE(IOUNT,9016) INFO
 9016 FORMAT(' A Total of   ',I10,' Informational Message(s)')
      IF (NTOTHRS .GT. 0) THEN
         WRITE(IOUNT,90171) NTOTHRS
90171    FORMAT(/,' A Total of   ',I10,' Hours Were Processed')
         WRITE(IOUNT,9017) ICLM
 9017    FORMAT(/,' A Total of   ',I10,' Calm Hours Identified')
C        Calculate percentage of missing hours, and check for > 10 percent.
         PERCENT = 100. * (FLOAT(IMSG)/FLOAT(NTOTHRS))
         WRITE(IOUNT,9018) IMSG, PERCENT
 9018    FORMAT(/,' A Total of   ',I10,' Missing Hours Identified (',
     &          F6.2,' Percent)')
         IF (NSubBGHOUR .GT. 0) THEN
            WRITE(IOUNT,99018) NSubBGHOUR
99018       FORMAT(/,' A Total of   ',I10,' Missing Hourly BACKGRND ',
     &            'Values Substituted')
         END IF
         IF (PERCENT .GT. 10.0) THEN
            WRITE(IOUNT,9019)
 9019       FORMAT(/,' CAUTION!:  Number of Missing Hours Exceeds 10 ',
     &             'Percent of Total!',/,12X,'Data May Not Be ',
     &             'Acceptable for Regulatory Applications.',/,12X,
     &             'See Section 5.3.2 of "Meteorological Monitoring ',
     &             'Guidance',/,12X,'for Regulatory Modeling ',
     &             'Applications" (EPA-454/R-99-005).')
         END IF
C ---    Output total precipipation if wet deposition algorithms are used
         IF (WDPLETE .OR. DEPOS .OR. WDEP) THEN
            WRITE(IOUNT,9020) TOTAL_PRECIP, TOTAL_PRECIP/25.4D0
 9020       FORMAT(/,' Met Data File Includes ',F10.2,' Millimeters (',
     &                 F10.3,' Inches) of Precipitation')
         END IF
      END IF
      WRITE(IOUNT,*) ' '

C     Write Out All The Fatal Error Messages
      WRITE(IOUNT,*) ' '
      WRITE(IOUNT,*) '   ******** FATAL ERROR MESSAGES ******** '
      REWIND IERUNT
      EOF = .FALSE.
      J = 0
      DO WHILE (.NOT. EOF)
         READ(IERUNT,1116,END=99,ERR=9999) PATH,ERRTP,ERRCD,IERRLN,
     &                                     MODNAM,ERRMG1,ERRMG2
         IF (ERRTP .EQ. 'E') THEN
            J = J + 1
            WRITE(IOUNT,1117) PATH,ERRTP,ERRCD,IERRLN,MODNAM(1:12),
     &                        ERRMG1,ERRMG2
         END IF
         GO TO 11
 99      EOF = .TRUE.
 11      CONTINUE
      END DO

C     If No Fatal Error Messages, Then Write 'NONE'
      IF (J .EQ. 0) THEN
         WRITE(IOUNT,*) '              ***  NONE  ***         '
         WRITE(IOUNT,*) ' '
      END IF

C     Write Out All The Warning Messages
      WRITE(IOUNT,*) ' '
      WRITE(IOUNT,*) '   ********   WARNING MESSAGES   ******** '
      REWIND IERUNT

C CRT, 12/9/2021  D036 Warnings - Comment EOF = True inside loop -
C     prematurely sets EOF flag to force exit loop
C     Update code needs to read to end of file to position
C     cursor for next write statement.
      EOF = .FALSE.
      J = 0
CCRT      DO WHILE (.NOT. EOF)
CCRT         READ(IERUNT,1116,END=999,ERR=9999) PATH,ERRTP,ERRCD,IERRLN,
CCRT     &                                      MODNAM,ERRMG1,ERRMG2
CCRT         IF (ERRTP .EQ. 'W') THEN
CCRT            J = J + 1
CCRT            IF (.NOT. NOWARN) THEN
CCRT               IF (J .LE. 999) THEN
CCRT                  WRITE(IOUNT,1117) PATH,ERRTP,ERRCD,IERRLN,
CCRT     &                              MODNAM(1:12),ERRMG1,ERRMG2
CCRT               ELSE
CCRT                  WRITE(IOUNT,*) 'More Than 999 Warning Messages ',
CCRT     &                           'Found.  See ERRORFIL Output for',
CCRT     &                           ' the Remainder.'
CCRT                  EOF = .TRUE.
CCRT               END IF
CCRT            END IF
CCRT         END IF
CCRT         GO TO 111
CCRT 999     EOF = .TRUE.
CCRT 111     CONTINUE
CCRT      END DO
      DO WHILE (.NOT. EOF)
         READ(IERUNT,1116,END=999,ERR=9999) PATH,ERRTP,ERRCD,IERRLN,
     &                                      MODNAM,ERRMG1,ERRMG2
         IF (ERRTP .EQ. 'W') THEN
            J = J + 1

            IF (.NOT. NOWARN) THEN
               IF (J .LE. 999) THEN
                  WRITE(IOUNT,1117) PATH,ERRTP,ERRCD,IERRLN,
     &                           MODNAM(1:12),ERRMG1,ERRMG2
               END IF
               IF (J .EQ. 999) THEN
                  WRITE(IOUNT,*) 'More Than 999 Warning Messages ',
     &                        'Found.  See ERRORFIL Output for',
     &                        ' the Remainder.'
               END IF
            END IF
         END IF
         GO TO 111
 999     EOF = .TRUE.
 111     CONTINUE
      END DO

C     If No Warning Messages, Then Write 'NONE'
      IF (J .EQ. 0) THEN
         WRITE(IOUNT,*) '              ***  NONE  ***        '
         WRITE(IOUNT,*) ' '
      ELSE IF (NOWARN) THEN
         WRITE(IOUNT,*) ' ** WARNINGS SUPPRESSED BY NOWARN OPTION **'
         WRITE(IOUNT,*) ' '
      END IF

 1116 FORMAT(A2,1X,A1,A3,I8,1X,A12,2X,A50,1X,A12)
 1117 FORMAT(1X,A2,1X,A1,A3,I8,1X,A12,': ',A50,1X,A12)

C     Use BACKSPACE To Reposition Temporary Error Message File Ahead of EOF;
C     This Is Needed in Order To Allow For Additional Message Writes
      BACKSPACE IERUNT

      GO TO 1000

C     WRITE Error Message: Error Reading Temp Error Message File
 9999 CALL ERRHDL(PATH,MODNAM,'E','510','ERRORMSG')

 1000 RETURN
      END

      SUBROUTINE MSGWRT
C***********************************************************************
C                 MSGWRT Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: To Print Out The Error Summary Table
C
C        PROGRAMMER: Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Error Message File
C
C        OUTPUTS: The Error Message File
C
C        CALLED FROM:  This is A Utility Program
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: IERRLN
      CHARACTER ERRTP*1, ERRCD*3, ERRMG1*50, ERRMG2*12

C     Variable Initialization
      MODNAM = 'MSGWRT'

C     Write Out The Header Of The Message File
      WRITE(IERWRT,*) ' '
      WRITE(IERWRT,*) '   ************ Error Message List *************'
      WRITE(IERWRT,*) ' '
      WRITE(IERWRT,*) '   PW     --- Pathway                           '
      WRITE(IERWRT,*) '   Code   --- Error Type + Error Code           '
      WRITE(IERWRT,*) '   L#     --- The Line Number Where Error Occurs'
      WRITE(IERWRT,*) '   ModNam --- Module Name In Which Error Occurs '
      WRITE(IERWRT,*) '   Hints  --- Hints For The Possible Solution   '
      WRITE(IERWRT,*) '   *********************************************'
      WRITE(IERWRT,*) ' '
      WRITE(IERWRT,1114)
      WRITE(IERWRT,1115)
 1114 FORMAT('PW CODE    L#      MODNAM     ',18X,'ERROR MESSAGES',22X,
     &                                                          'HINTS')
 1115 FORMAT('-- ---- ------- ------------- ',50('-'),' ------------')
      WRITE(IERWRT,*) ' '
      REWIND IERUNT
      EOF = .FALSE.

      DO WHILE (.NOT. EOF)
         READ(IERUNT,1116,END=99,ERR=999) PATH,ERRTP,ERRCD,IERRLN,
     &                                    MODNAM,ERRMG1,ERRMG2
         WRITE(IERWRT,1117) PATH,ERRTP,ERRCD,IERRLN,
     &                      MODNAM(1:12),ERRMG1,ERRMG2
         GO TO 11
 99      EOF = .TRUE.
 11      CONTINUE
      END DO

 1116 FORMAT(A2,1X,A1,A3,I8,1X,A12,2X,A50,1X,A12)
 1117 FORMAT(A2,1X,A1,A3,I8,1X,A12,': ',A50,1X,A12)

      GO TO 1000

C     WRITE Error Message: Error Reading Temp Error Message File
 999  CALL ERRHDL(PATH,MODNAM,'E','510','ERRORMSG')

 1000 RETURN
      END

      SUBROUTINE PNPOLY (PX,PY,X,Y,N,INOUT)
C----------------------------------------------------------------------
C     Courtesy: Jay Sandhu
C               email: jsandhu@esri.com
C
C
C Please cite David H. Douglas, COLLECTED ALGORITHMS, Cambridge MA:
C Harvard Laboratory for Computer Graphics, 1974
C
C This is my reinvention buster.
C 1974 1974 1974 1974 1974 1974 1974 1974 1974 1974 1974 1974
C
C>>>PNPY
C     .................................................................
C
C        SUBROUTINE PNPOLY
C
C        PURPOSE
C           TO DETERMINE WHETHER A POINT IS INSIDE A POLYGON
C
C        USAGE
C           CALL PNPOLY (PX, PY, X, Y, N, INOUT )
C
C        DESCRIPTION OF THE PARAMETERS
C           PX      - X-COORDINATE OF POINT IN QUESTION.
C           PY      - Y-COORDINATE OF POINT IN QUESTION.
C           X       - N LONG VECTOR CONTAINING X-COORDINATES OF
C                     VERTICES OF POLYGON.
C           Y       - N LONG VECTOR CONTAINING Y-COORDINATES OF
C                     VERTICES OF POLYGON.
C           N       - NUMBER OF VERTICES IN THE POLYGON.
C           INOUT   - THE SIGNAL RETURNED:
C                     -1 IF THE POINT IS OUTSIDE OF THE POLYGON,
C                      0 IF THE POINT IS ON AN EDGE OR AT A VERTEX,
C                      1 IF THE POINT IS INSIDE OF THE POLYGON.
C
C        REMARKS
C           THE VERTICES MAY BE LISTED IN CLOCKWISE OR ANTICLOCKWISE
C           ORDER.  FOR THIS SUBROUTINE A POINT IS CONSIDERED INSIDE
C           THE POLYGON IF IT IS LOCATED IN THE ENCLOSED AREA DEFINED
C           BY THE LINE FORMING THE POLYGON.
C           THE INPUT POLYGON MAY BE A COMPOUND POLYGON CONSISTING
C           OF SEVERAL SEPARATE SUBPOLYGONS. IF SO, THE FIRST VERTEX
C           OF EACH SUBPOLYGON MUST BE REPEATED, AND WHEN CALCULATING
C           N, THESE FIRST VERTICES MUST BE COUNTED TWICE.
C           INOUT IS THE ONLY PARAMETER WHOSE VALUE IS CHANGED.
C           PNPOLY CAN HANDLE ANY NUMBER OF VERTICES IN THE POLYGON.
C           WRITTEN BY RANDOLPH FRANKLIN, UNIVERSITY OF OTTAWA, 6/72.
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           NONE
C
C        METHOD
C           A VERTICAL SEMI-INFINITE LINE IS DRAWN UP FROM THE POINT
C           IN QUESTION. IF IT CROSSES THE POLYGON AN ODD NUMBER OF
C           TIMES, THE POINT IS INSIDE THE POLYGON.
C
C --- Modified to use an Internal Function for EOR rather than a
C     Statement Function, which has been identified as obsolescent
C --- in Fortran 95.  R.W. Brode, EPA/OAQPS/AQMG, 10/19/2009
C     .................................................................
C

      IMPLICIT NONE

      INTEGER I, J, N, INOUT
      DOUBLE PRECISION X(N), Y(N), XI, YI, XJ, YJ, PX, PY
      LOGICAL IX, IY
      LOGICAL JX, JY
      LOGICAL L_EOR

      L_EOR = .FALSE.

      INOUT=-1

      DO I=1,N
         XI=X(I)-PX
         YI=Y(I)-PY
C        CHECK WHETHER THE POINT IN QUESTION IS AT THIS VERTEX.
         IF (XI.EQ.0.0D0 .AND. YI.EQ.0.0D0) THEN
            INOUT = 0
            EXIT
         END IF
C        J IS NEXT VERTEX NUMBER OF POLYGON.
         J=1+MOD(I,N)
         XJ=X(J)-PX
         YJ=Y(J)-PY
C        IS THIS LINE OF 0 LENGTH ?
         IF (XI.EQ.XJ .AND. YI.EQ.YJ) CYCLE
         IX=XI.GE.0.0D0
         IY=YI.GE.0.0D0
         JX=XJ.GE.0.0D0
         JY=YJ.GE.0.0D0
C        CHECK WHETHER (PX,PY) IS ON VERTICAL SIDE OF POLYGON.
         L_EOR = EOR(IY,JY)
         IF (XI.EQ.0.0D0 .AND. XJ.EQ.0.0D0 .AND. L_EOR) THEN
            INOUT = 0
            EXIT
         END IF
C        CHECK WHETHER (PX,PY) IS ON HORIZONTAL SIDE OF POLYGON.
         L_EOR = EOR(IX,JX)
         IF (YI.EQ.0.0D0 .AND. YJ.EQ.0.0D0 .AND. L_EOR) THEN
            INOUT = 0
            EXIT
         END IF
C        CHECK WHETHER BOTH ENDS OF THIS SIDE ARE COMPLETELY 1) TO RIGHT
C        OF, 2) TO LEFT OF, OR 3) BELOW (PX,PY).
         L_EOR = EOR(IX,JX)
         IF (.NOT.((IY.OR.JY).AND.L_EOR)) CYCLE
C        DOES THIS SIDE OBVIOUSLY CROSS LINE RISING VERTICALLY FROM (PX,PY)
         L_EOR = EOR(IX,JX)
         IF (.NOT.(IY.AND.JY.AND.L_EOR)) THEN
            IF ((YI*XJ-XI*YJ)/(XJ-XI) .LT. 0.0D0) THEN
               CYCLE
            ELSE IF ((YI*XJ-XI*YJ)/(XJ-XI) .EQ. 0.0D0) THEN
               INOUT = 0
               EXIT
            ELSE
               INOUT = -INOUT
            END IF
         ELSE
            INOUT = -INOUT
         END IF

      END DO

C     "EXCLUSIVE OR" Internal FUNCTION, EOR:
      CONTAINS
        LOGICAL FUNCTION EOR(IX,IY)
          LOGICAL IX, IY
          EOR = (IX.OR.IY) .AND. .NOT.(IX.AND.IY)
        END FUNCTION

      END

      SUBROUTINE ALLSETUP
C***********************************************************************
C                 ALLSETUP Module
C
C        PURPOSE: Allocate Array Storage for SETUP
C
C        PROGRAMMER: Roger Brode, PES, Inc.
C
C        DATE:    September 21, 1996
C
C        MODIFIED:  To include RLINEXT variable BDW_FLAG and allocate
C                   memory for it.
C                   Dianna Francisco & David Heist, EPA ORD, 2/12/21
C
C        MODIFIED:  To include arrays for GRSM NO2 option
C                   CERC, 11/30/20
C
C        MODIFIED:  To include RLINE parameters, MOVES units conversion,
C                   and temporary source and receptor coordinates
C                   for a RLINE source
C                   Wood, 07/20/18
C
C        MODIFIED:  To include CHIBL, PARTCH, buoyant line parameters,
C                   and source and receptor coordinates in a rotated
C                   system for a buoyant line source
C                   Amec Foster Wheeler, 06/30/15
C
C        MODIFIED:  To include ADSFACT, AWDSIN, and AWDCOS.
C                   R. W. Brode, MACTEC (f/k/a PES), Inc., 08/02/05
C
C        INPUTS:
C
C
C        OUTPUTS:
C
C        CALLED FROM:  MAIN
C
C        ERROR HANDLING:   Checks for error allocating arrays
C***********************************************************************
C
C     Variable Declarations
      USE MAIN1
      USE RLINE_DATA, ONLY: RLSOURCE, XRCP_ROT, YRCP_ROT, XSB_ROT,
     &                      YSB_ROT, XSE_ROT, YSE_ROT, NRLINES,
     &                      RLFIRSTHR, RLEMISCONV, RLMOVESCONV,
     &                      BDW_FLAG
      USE BUOYANT_LINE
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: IASTAT

C     Variable Initializations
      MODNAM = 'ALLSET'
      ALLOC_ERR = .FALSE.

      IF (NUMBGsects .LT. 1) NUMBGsects = 1
      IF (NUMO3sects .LT. 1) NUMO3sects = 1
      IF (NUMNOxsects .LT. 1) NUMNOxsects = 1

      ALLOCATE  (KAVE(NAVE), CHRAVE(NAVE), CHIDEP(6,NTYP),
     &           OUTTYP(NTYP),STAT=IASTAT)
      IF (IASTAT .NE. 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','409','Setup Arrays')
         ALLOC_ERR = .TRUE.
      END IF
CCRT  D063 Add platform downwash arrays (PLATELV, PLATHB, PLATWB
CCRT       OSPLAT, SOPLAT
      ALLOCATE  (AXS(NSRC), AYS(NSRC), AZS(NSRC), AQS(NSRC),
     &           AHS(NSRC), ATS(NSRC), AVS(NSRC), ADS(NSRC),
     &           PLATELV(NSRC), PLATHB(NSRC), PLATWB(NSRC),
     &           OSPLAT(NSRC), SOPLAT(NSRC),
     &           ASYINI(NSRC), ASZINI(NSRC), AFP(NSRC),
     &           AXS1(NSRC), AYS1(NSRC), AXS2(NSRC), AYS2(NSRC),
     &           AWIDTH(NSRC), ADSFACT(NSRC), NDXSTK(NSRC),
     &           AWDSIN(NSRC), AWDCOS(NSRC), AAFV(NSRC),
     &           INPD(NSRC), EVAL(NSRC), URBSRC(NSRC),
     &           L_HRLYSIG(NSRC), L_FLATSRC(NSRC),
     &           L_METHOD2(NSRC), L_WakeMessage(NSRC),
     &           IGROUP(NSRC,NGRP), SRCID(NSRC), SRCTYP(NSRC),
     &           SOPCRD(NSRC), SOGAS(NSRC), O3VARY(NO3F,NUMO3sects),
     &           GRPID(NGRP), QFLAG(NSRC), EMILBL(NTYP),
     &           OUTLBL(NTYP), PERLBL(NTYP), BACKGRND(NBF,NUMBGsects),
     &           EMIFAC(NTYP), GRP_BACK(NGRP),
     &           NOXVARY(NNOXF,NumNOxsects),
     &           STAT=IASTAT)

C     AECOM 4/13/2022 D113 Added for SIDEWASH
      ALLOCATE (ABW(NSWP), ABL(NSWP), ABH(NSWP), ABA(NSWP),
     &          SWXS(NSWP), SWYS(NSWP), STAT=IASTAT)
C     end insert for SIDEWASH

      IF (IASTAT .NE. 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','409','Setup Arrays')
         ALLOC_ERR = .TRUE.
         WRITE(IOUNIT,*) '  Error Occurred During Allocation of ',
     &                   'Basic Source Arrays!'

      END IF

      IF (NSEC .GT. 0) THEN
         ALLOCATE  (ADSBH(NSEC,NSRC), ADSBW(NSEC,NSRC),
     &              ADSBL(NSEC,NSRC), ADSXADJ(NSEC,NSRC),
     &              ADSYADJ(NSEC,NSRC),
     &              STAT=IASTAT)
         IF (IASTAT .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','409','Setup Arrays')
            ALLOC_ERR = .TRUE.
            WRITE(IOUNIT,*) '  Error Occurred During Allocation of ',
     &                      'Source Downwash Arrays!'

         END IF
      END IF

      IF (NQF .GT. 0) THEN
         ALLOCATE  (QFACT(NQF,NSRC), STAT=IASTAT)
         IF (IASTAT .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','409','Setup Arrays')
            ALLOC_ERR = .TRUE.
            WRITE(IOUNIT,*) '  Error Occurred During Allocation of ',
     &                      'Source Emission Factor Arrays!'
         END IF

      END IF

      IF (NPDMAX .GT. 0) THEN
         ALLOCATE  (APDIAM(NPDMAX,NSRC), APHI(NPDMAX,NSRC),
     &              APDENS(NPDMAX,NSRC), AVGRAV(NPDMAX,NSRC),
     &              ATSTOP(NPDMAX,NSRC),
     &              EFRAC(NPDMAX), QPART(NPDMAX),
     &              PDIAM(NPDMAX), PHI(NPDMAX), PDENS(NPDMAX),
     &              VGRAV(NPDMAX), TSTOP(NPDMAX), SCHMIDT(NPDMAX),
     &              VDEP(NPDMAX), SCF(NPDMAX), WQCOR(NPDMAX),
     &              DQCOR(NPDMAX), PSCVRT(NPDMAX), WASHOUT(NPDMAX),
     &              ECOLL(NPDMAX), finemass(NSRC),
     &              STAT=IASTAT)
         IF (IASTAT .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','409','Setup Arrays')
            ALLOC_ERR = .TRUE.
            WRITE(IOUNIT,*) '  Error Occurred During Allocation of ',
     &                      'Source Particle Deposition Arrays!'
         END IF

      END IF

      ALLOCATE  (pdiff(NSRC), pdiffw(NSRC), rmolwt(NSRC), alphas(NSRC),
     &           react(NSRC), henry(NSRC), rcli(NSRC),
     &           STAT=IASTAT)
      IF (IASTAT .NE. 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','409','Setup Arrays')
         ALLOC_ERR = .TRUE.
         WRITE(IOUNIT,*) '  Error Occurred During Allocation of ',
     &                   'Gas Deposition Arrays!'
      END IF

C --- Allocate arrays for AREA sources based on NVMAX+1 to address issues with AREAPOLY sources
      IF (NVMAX .GT. 0) THEN
         ALLOCATE  (AXINIT(NSRC), AYINIT(NSRC), AANGLE(NSRC),
     &              AXVERT(NVMAX+1,NSRC), AYVERT(NVMAX+1,NSRC),
     &              UVERT(NVMAX+1), VVERT(NVMAX+1), VNVERT(NVMAX+1),
     &              WVERT(NVMAX+1), UASEGS(NVMAX+1), UBSEGS(NVMAX+1),
     &              XVERT(NVMAX+1), YVERT(NVMAX+1),
     &              SPA(NVMAX+1,2),
     &              AALPHA(NSRC), APDEFF(NSRC), AVOLUM(NSRC),
     &              RADIUS(NSRC), NVERTS(NSRC), AXCNTR(NSRC),
     &              AYCNTR(NSRC),
     &              STAT=IASTAT)
         IF (IASTAT .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','409','Setup Arrays')
            ALLOC_ERR = .TRUE.
            WRITE(IOUNIT,*) '  Error Occurred During Allocation of ',
     &                      'Area Source Arrays!'
         END IF
      END IF

      IF (NRLINES .GT. 0) THEN
         ALLOCATE(RLSOURCE(NSRC), XSB_ROT(NSRC), YSB_ROT(NSRC),
     &             XSE_ROT(NSRC), YSE_ROT(NSRC), RLEMISCONV(NSRC),
     &             BDW_FLAG(NSRC,2),STAT=IASTAT)
C        Initialize MOVES input unit flag and FIRSTHR flag
         RLMOVESCONV = .FALSE.
         RLFIRSTHR = .TRUE.
         IF (IASTAT .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','409','Setup Arrays')
            ALLOC_ERR = .TRUE.
            WRITE(IOUNIT,*) '  Error Occurred During Allocation of ',
     &                      'RLINE Temporary Source Arrays!'
         END IF
         ALLOCATE(XRCP_ROT(NREC), YRCP_ROT(NREC), STAT=IASTAT)
         IF (IASTAT .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','409','Setup Arrays')
            ALLOC_ERR = .TRUE.
            WRITE(IOUNIT,*) '  Error Occurred During Allocation of ',
     &                      'RLINE Temporary Receptor Arrays!'
         END IF
      ENDIF

C (Multiple_BuoyLines_D41_Wood)
C     Changed variable name NBLINES to NBLP
C (Buoyant Line Source Limit D_088)
C     Made array DEL allocatable

C --- Buoyant line allocation
C     NBLP = number of individual buoyant lines
      IF (NBLP .GT. 0) THEN
         ALLOCATE (BLINEPARMS(NBLP), XS_SCS(NBLP,129),
     &             YS_SCS(NBLP), XS_RCS(NBLP,129),
     &             YS_RCS(NBLP,129),DEL(NBLP), STAT=IASTAT)
         IF (IASTAT .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','409','Setup Arrays')
            ALLOC_ERR = .TRUE.
            WRITE(IOUNIT,*) '  Error Occurred During Allocation of ',
     &                      'Buoyant Line Arrays!'
         END IF

         ALLOCATE (CHIBL(NREC), STAT=IASTAT)
         IF (IASTAT .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','409','Setup Arrays')
            ALLOC_ERR = .TRUE.
            WRITE(IOUNIT,*) '  Error Occurred During Allocation of ',
     &                      'Concentration Arrays for Buoyant Line!'
         END IF

C (Multiple_BuoyLines_D41_Wood)
C        Added dimensions to arrays for multiple buoyant lines;
C        also changed some scalar values to 1-D arrays
         ALLOCATE (XR_SCS(NREC,NBLGRP), YR_SCS(NREC,NBLGRP),
     &             XR_RCS(NREC), YR_RCS(NREC), BL_RFLAG(NREC,NBLGRP),
     &             STAT=IASTAT)
         IF (IASTAT .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','409','Setup Arrays')
            ALLOC_ERR = .TRUE.
            WRITE(IOUNIT,*) '  Error Occurred During Allocation of ',
     &                      'Receptor Arrays for Buoyant Line!'
         END IF

         ALLOCATE (BLAVGINP_GRPID(NBLAVGINPalloc),
     &             BLAVGINP_LLEN(NBLAVGINPalloc),
     &             BLAVGINP_BWID(NBLAVGINPalloc),
     &             BLAVGINP_BHGT(NBLAVGINPalloc),
     &             BLAVGINP_BSEP(NBLAVGINPalloc),
     &             BLAVGINP_LWID(NBLAVGINPalloc),
     &             BLAVGINP_FPRM(NBLAVGINPalloc),
     &             NBLINGRP(NBLGRP),ANGRAD(NBLGRP),
     &             HRLYBLCOUNT(NBLGRP), XOR(NBLGRP), YOR(NBLGRP),
     &             TCOR(NBLGRP), SINTCOR(NBLGRP), COSTCOR(NBLGRP),
     &             IGRP_BLP(NSRC,NBLGRP), BL_GRPID(NBLGRP),
     &             L_BLURBAN(NBLGRP), BL_NUMURB(NBLGRP),
     &             STAT=IASTAT)
         IF (IASTAT .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','409','Setup Arrays')
            ALLOC_ERR = .TRUE.
            WRITE(IOUNIT,*) '  Error Occurred During Allocation of ',
     &                      'Arrays for Buoyant Line Groups!'
         END IF
      END IF

      IF (NURB .GT. 0) THEN
         ALLOCATE  (IURBGRP(NSRC,NURB), URBID(NURB), URBNAM(NURB),
     &              URBPOP(NURB), URBZ0(NURB),
     &              ZIURB(NURB), URBWSTR(NURB), URBUSTR(NURB),
     &              URBOBULEN(NURB),
     &              GRDSWU(MXGLVL,NURB), GRDSVU(MXGLVL,NURB),
     &              GRDTGU(MXGLVL,NURB), GRDPTU(MXGLVL,NURB),
     &              L_MorningTrans(NURB),
     &              STAT=IASTAT)
         IF (IASTAT .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','409','Setup Arrays')
            WRITE(IOUNIT,*) '  Error Occurred During Allocation of ',
     &                      'Urban Arrays!'
         END IF
      END IF

      IF (EVONLY) THEN
         ALLOCATE  (EV_HRQS(NSRC,NHR), EV_HRTS(NSRC,NHR),
     &              EV_HRVS(NSRC,NHR), EV_HRHS(NSRC,NHR),
     &              EV_HRSY(NSRC,NHR), EV_HRSZ(NSRC,NHR),
     &              EV_HRFP(NSRC,NHR),
     &              EVAPER(NEVE), EVDATE(NEVE), EVJDAY(NEVE),
     &              IDXEV(NEVE), AXR(NEVE), AYR(NEVE), AZELEV(NEVE),
     &              AZFLAG(NEVE), AZHILL(NEVE), EVNAME(NEVE),
     &              EVGRP(NEVE), EV_OrigConc(NEVE), STAT=IASTAT)
         IF (IASTAT .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','409','Setup Arrays')
            ALLOC_ERR = .TRUE.
            WRITE(IOUNIT,*) '  Error Occurred During Allocation of ',
     &                      'EVENT Processing Arrays!'
         END IF
      END IF

      IF (.NOT. EVONLY) THEN
         ALLOCATE  (AXR(NREC), AYR(NREC), AZELEV(NREC),
     &              AZFLAG(NREC), AZHILL(NREC), IREF(NREC),
     &              NETID(NREC), RECTYP(NREC),
     &              NDXARC(NREC), ARCID(NARC),
     &              NTID(NNET), NTTYP(NNET),
     &              XCOORD(IXM,NNET), YCOORD(IYM,NNET),
     &              XORIG(NNET), YORIG(NNET),
     &              NETSTA(NNET), NETEND(NNET),
     &              NUMXPT(NNET), NUMYPT(NNET),
C                   AECOM 4/13/2022 D113 Added for Sidewash
     &              SWXR(NREC), SWYR(NREC),
C                   End Sidewash insert
     &              STAT=IASTAT)

         IF (IASTAT .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','409','Setup Arrays')
            ALLOC_ERR = .TRUE.
            WRITE(IOUNIT,*) '  Error Occurred During Allocation of ',
     &                      'Receptor Arrays!'
         END IF
      END IF

      ALLOCATE  (NHIAVE(NVAL,NAVE), MAXAVE(NAVE), IMXVAL(NAVE),
     &           IDYTAB(NAVE), MAXFLE(NGRP,NAVE),
     &           IPSTFL(NGRP,NAVE), IPLTFL(NVAL,NGRP,NAVE),
     &           IANPST(NGRP), IANPLT(NGRP), INHI(NAVE),
     &           ITOXFL(NAVE), IRNKFL(NAVE), IRKVAL(NAVE),
     &           THRESH(NGRP,NAVE), TOXTHR(NAVE),
     &           IMXUNT(NGRP,NAVE), IPSUNT(NGRP,NAVE),
     &           IPSFRM(NGRP,NAVE), IPLUNT(NVAL,NGRP,NAVE),
     &           IAPUNT(NGRP), IANFRM(NGRP), IPPUNT(NGRP),
     &           ITXUNT(NAVE), IRKUNT(NAVE), IELUNT(NSRC),
     &           THRFIL(NGRP,NAVE), PSTFIL(NGRP,NAVE),
     &           PLTFIL(NVAL,NGRP,NAVE), ANNPST(NGRP),
     &           ANNPLT(NGRP), TOXFIL(NAVE), RNKFIL(NAVE),
     &           EVLFIL(NSRC), ISEAHR(NGRP), SEAHRS(NGRP),
     &           ISHUNT(NGRP), IMXDLY(NGRP), IMDUNT(NGRP),
     &           MAXDLY(NGRP), IMXDLY_BYYR(NGRP),
     &           IMDUNT_BYYR(NGRP), MAXDLY_BYYR(NGRP),
     &           MAXDCONT(NGRP), IMXDCUNT(NGRP),
     &           MAXDCONT_FILE(NGRP), MXD_RANK(NGRP,2),
     &           MAXD_THRESH(NGRP),
     &           STAT=IASTAT)
      IF (IASTAT .NE. 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','409','Setup Arrays')
         ALLOC_ERR = .TRUE.
         WRITE(IOUNIT,*) '  Error Occurred During Allocation of ',
     &                   'Output Option Arrays!'
      END IF

      ALLOCATE  (IDCONC(NAVE,NPAIR), TXCONC(NAVE,NPAIR), STAT=IASTAT)
      IF (IASTAT .NE. 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','409','Setup Arrays')
         ALLOC_ERR = .TRUE.
         WRITE(IOUNIT,*) '  Error Occurred During Allocation of ',
     &                   'TOXXFILE Arrays!'
      END IF

      ALLOCATE  (WORKID(NSRC+1), IWRK2(NSRC,13), STAT=IASTAT)
      IF (IASTAT .NE. 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','409','Setup Arrays')
         ALLOC_ERR = .TRUE.
         WRITE(IOUNIT,*) '  Error Occurred During Allocation of ',
     &                   'Temporary Source Arrays!'
      END IF

      IF (.NOT. EVONLY) THEN
         ALLOCATE  (ZETMP1(NREC), ZETMP2(NREC),
     &              ZHTMP1(NREC), ZHTMP2(NREC),
     &              ZFTMP1(NREC), ZFTMP2(NREC), STAT=IASTAT)
         IF (IASTAT .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','409','Setup Arrays')
            ALLOC_ERR = .TRUE.
            WRITE(IOUNIT,*) '  Error Occurred During Allocation of ',
     &                      'Temporary Receptor Arrays!'
         END IF
      END IF

      IF (PVMRM .OR. OLM .OR. ARM2
     &    .OR. RUNTTRM .OR. GRSM) THEN
         ALLOCATE (ANO2_RATIO(NSRC), CHI(NREC,NSRC,NTYP), STAT=IASTAT)
         IF (IASTAT .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','409','Setup Arrays')
            ALLOC_ERR = .TRUE.
            WRITE(IOUNIT,*) '  Error Occurred During Allocation of ',
     &                      'PVMRM/OLM/ARM2/TTRM/GRSM CHI Array!'
         END IF

         IF (PVMRM .OR. GRSM) THEN
            ALLOCATE (HECNTR(NREC,NSRC), HECNTR3(NREC,NSRC),
     &                UEFFS(NREC,NSRC), UEFF3S(NREC,NSRC),
     &                EPSEF(NREC,NSRC), EPSEF3(NREC,NSRC),
     &                FOPTS(NREC,NSRC), PPFACT(NREC,NSRC), STAT=IASTAT)
            IF (IASTAT .NE. 0) THEN
               CALL ERRHDL(PATH,MODNAM,'E','409','Setup Arrays')
               ALLOC_ERR = .TRUE.
               WRITE(IOUNIT,*) '  Error Occurred During Allocation of ',
     &                         'PVMRM/GRSM Source Data Arrays!'
            END IF
         END IF

         IF (OLM) THEN
            ALLOCATE (OLMID(NOLM), L_OLMGRP(NSRC),
     &                IGRP_OLM(NSRC,NOLM), STAT=IASTAT)
            IF (IASTAT .NE. 0) THEN
               CALL ERRHDL(PATH,MODNAM,'E','409','Setup Arrays')
               ALLOC_ERR = .TRUE.
               WRITE(IOUNIT,*) '  Error Occurred During Allocation of ',
     &                         'OLMGROUP Source Data Arrays!'
            END IF
         END IF

!     added for TTRM; AECOM
         IF (RUNTTRM) THEN
            ALLOCATE (TTRMOUT(NREC,NSRC,25), STAT=IASTAT)
            ALLOCATE (TTRMSRC(NREC,NSRC), STAT=IASTAT) ! TTRMSRC: reports stability & plume type for debug file
            ALLOCATE (TTRMINST(NTYP), TTRMFRAC(NTYP), TTRMSUB(NTYP),
     &                TTRMNO2(NTYP), L_TTRMSRCTYP(NSRC),
     &                TTRMFRAC_PRM(NTYP),
     &                TTRMCOMPARE(NGRP,NSRC,NREC,NTYP),
     &                TTRMFRAC_AER(NTYP), STAT=IASTAT)
!      Add a logical warning array L_TTRMSRCTYP for source types not currently configured for TTRM
!     end TTRM insert; Feb. 2021
            IF (IASTAT .NE. 0) THEN
               CALL ERRHDL(PATH,MODNAM,'E','409','Setup Arrays')
               ALLOC_ERR = .TRUE.
               WRITE(IOUNIT,*) '  Error Occurred During Allocation of ',
     &                         'TTRM Source Data Arrays!'
            END IF
         END IF

         IF (PSDCREDIT) THEN
            ALLOCATE (PSDSRCTYP(NSRC), PSDID(NPSD), L_PSDGRP(NSRC),
     &                IGRP_PSD(NSRC,NPSD),
     &                ABVAL(NREC,NTYP), BCVAL(NREC,NTYP), STAT=IASTAT)
            IF (IASTAT .NE. 0) THEN
               CALL ERRHDL(PATH,MODNAM,'E','409','Setup Arrays')
               ALLOC_ERR = .TRUE.
               WRITE(IOUNIT,*) '  Error Occurred During Allocation of ',
     &                         'PSDCREDIT Source Data Arrays!'
            END IF
         END IF

C        CERC 11/30/20
         IF (GRSM) THEN
            ALLOCATE (CHI_TTRAVCHM(NREC,NSRC), STAT=IASTAT)
            IF (IASTAT .NE. 0)THEN
               CALL ERRHDL(PATH,MODNAM,'E','409','Setup Arrays')
               ALLOC_ERR = .TRUE.
               WRITE(IOUNIT,*) '  Error Occurred During Allocation of ',
     &                         'GRSM Travel Time Arrays!'
            END IF
            ALLOCATE (TTRAVCHM(NREC), STAT=IASTAT)
            IF (IASTAT .NE. 0)THEN
               CALL ERRHDL(PATH,MODNAM,'E','409','Setup Arrays')
               ALLOC_ERR = .TRUE.
               WRITE(IOUNIT,*) '  Error Occurred During Allocation of ',
     &                         'GRSM Travel Time Arrays!'
            END IF
            !Initialisations
            CHI_TTRAVCHM(:,:)=0.0D0
            TTRAVCHM(:)=0.0D0
         END IF

      END IF

      RETURN
      END

      SUBROUTINE ALLRESULT
C***********************************************************************
C                 ALLRESULT Module
C
C        PURPOSE: Allocate Array Storage for Results
C
C        PROGRAMMER: Roger Brode, PES, Inc.
C
C        DATE:       September 21, 1996
C
C        MODIFIED    Added storage requirement estimate and allocations
C                    for GRSM NO2 option.
C                    CERC, 11/30/20
C
C        MODIFIED:   Modified subroutine ALLRESULT to eliminate the arrays of
C                    profile met data (the observed data in the PROFFILE input)
C                    by hour-of-year, level, and year for use with the MAXDCONT
C                    option. The profile met data arrays are not needed for the
C                    MAXDCONT option and their removal reduces the memory
C                    requirements for that option. Additional adjustments to the
C                    array allocations for the MAXDCONT option were made based
C                    on the options used for a specific model run in order to
C                    minimize the memory requirements for the MAXDCONT option.
C                    Subroutine ALLRESULT was also modified to improve the
C                    accuracy of the memory storage estimates included on the
C                    first page of the AERMOD.OUT file.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/29/2012
C
C        MODIFIED:   Added calculation of STORE, estimated memory
C                    storage requirements, to report if allocation
C                    errors occur.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
C
C                    Changed parameter for allocating the number of
C                    high annual/period averages from NHIVAL to NHIANN.
C                    R.W. Brode, PES, Inc.,  4/3/98
C
C        INPUTS:
C
C
C        OUTPUTS:
C
C        CALLED FROM:  MAIN
C
C        ERROR HANDLING:   Checks for error allocating arrays
C***********************************************************************
C
C     Variable Declarations
      USE MAIN1
      USE BUOYANT_LINE
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: IASTAT
C     Declare Real Variables used to estimate memory requirements
      REAL    :: RSRC, RSEC, RGRP, RREC, RURB, RARC, RAVE,
C     JAT D065 7/22/21 REVE NOT USED
c     &           RHIVAL, RTYP, RMAXVAL, RNET, RXM , RYM , REVE, ROLM,
     &           RHIVAL, RTYP, RMAXVAL, RNET, RXM , RYM , ROLM,
     &           RPSD, RQF, RPDMAX, RVMAX, RPAIR, RHIANN, RHIMXDLY,
C     JAT D065 7/22/21 RBLP NOT USED
C     &           RYEARS, RBLP                                           ! D41_Wood
     &           RYEARS                                         ! D41_Wood
C     Variable Initializations
      MODNAM = 'ALLRESULT'
      ALLOC_ERR = .FALSE.

C     NARC was initially set to NREC prior to SETUP, now set NARC = NUMARC
      NARC = NUMARC

C --- Assign maximum value from IRKVAL array for the OU RANKFILE option
C     and from the IMXVAL array for the OU MAXTABLE option to NMXVAL
      NMXVAL = MAX( MAXVAL(IRKVAL), MAXVAL(IMXVAL) )

C     Assign array limits to REAL for calculation of STORE
      RSRC   = REAL(NSRC)
      RSEC   = REAL(NSEC)
      RGRP   = REAL(NGRP)
      RREC   = REAL(NREC)
      RURB   = REAL(NURB)
      RARC   = REAL(NARC)
      RAVE   = REAL(NAVE)
      RHIVAL = REAL(NHIVAL)
      RTYP   = REAL(NTYP)
      RMAXVAL= REAL(NMXVAL)
      RNET   = REAL(NNET)
      RXM    = REAL(IXM )
      RYM    = REAL(IYM )
C     JAT 7/22/21 REVE NOT USED
C      REVE   = REAL(NEVE)
      ROLM   = REAL(NOLM)
      RPSD   = REAL(NPSD)
C     JAT D065 7/22/21 RBLP NOT USED
C      RBLP   = REAL(NBLGRP)                      ! (Multiple_BuoyLines_D41_Wood)
      RQF    = REAL(NQF)
      RPDMAX = REAL(NPDMAX)
      RVMAX  = REAL(NVMAX)
      RPAIR  = REAL(NPAIR)
      RHIANN = REAL(NHIANN)
      RYEARS = REAL(NYEARS)
      RHIMXDLY = REAL(NHIMXDLY)

!     Added for TTRM, AECOM; For use in the MAXDCONT array allocation
      IF (RUNTTRM) THEN
         ROLM = REAL(1)
      END IF
!     End of TTRM insert, Feb. 2021

      STORE = 0.0
      IF (.NOT. EVONLY) THEN
C        Calculate Approximate Allocated Storage Requirements
         STORE = RSRC*(54.+RQF+5.*RSEC+5.*RPDMAX+2.*RVMAX+
     &           0.5*(RGRP+RURB))+
     &           RPDMAX*14. +
     &           RREC*(9.+RHIVAL*RGRP*RAVE*RTYP*1.75+RGRP*RAVE*RTYP+
     &                 2.*RGRP*RTYP+RGRP) +
     &           RARC*20. + RNET*(9.+RXM+RYM) +
     &           RHIVAL*(RGRP*RAVE*RTYP*3.)+
     &           RMAXVAL*(RGRP*RAVE*RTYP*2.25) +
     &           RHIANN*1.5*RGRP*RTYP +
     &           RAVE*(12.+2.*RPAIR+3.*RHIVAL*RGRP+8.*RGRP) +
     &           RGRP*11. + RTYP*38. + RVMAX*20.
         IF (SEASONHR) THEN
            STORE = STORE + ( 4.*24.*RREC*RGRP*RTYP )
         END IF
         IF (PVMRM .OR. OLM .OR. RUNTTRM .OR.
     &       GRSM .OR. RUNTTRM2) THEN
            !RSRC is for ANO2_RATIO, RSRC*RREC*RTYP is for CHI
            STORE = STORE + ( RSRC + RSRC*RREC*RTYP )
            IF (PVMRM) THEN
               STORE = STORE + ( 8.*RSRC*RREC )
               IF (PSDCREDIT) THEN
                  STORE = STORE + ( RSRC*(2.+RPSD) + RREC*2.*RTYP )
               END IF
            ELSE IF (OLM .OR. RUNTTRM) THEN
               STORE = STORE + ( 0.5*ROLM*RSRC + ROLM + RSRC )
            ELSE IF (GRSM) THEN
              !For HECNTR, UEEFS, EPSEF etc:
              STORE = STORE + ( 8.*RSRC*RREC )
              !RREC is for TTRAVCHEM and RREC*RSRC is for CHI_TTRAVCHM:
              STORE = STORE + RREC + RREC*RSRC
              !For arrays allocated in GRSM_CALC:
              STORE = STORE + ( 6.*RSRC )
            END IF
         END IF
         IF (PM25AVE .OR. NO2AVE .OR. SO2AVE) THEN
            STORE = STORE + RREC*RGRP*(1.5+1.5*RHIMXDLY+
     &                                     1.5*RHIMXDLY*RYEARS)
         END IF
         IF (L_MAXDCONT) THEN
            STORE = STORE + RYEARS*(19.*8784.+10.*8784.+
     &                               7.*8784.*REAL(MXGLVL))
            IF (LDGAS.OR.LDPART.OR.LWGAS.OR.LWPART.OR.GRSM) THEN
               STORE = STORE + RYEARS*14.*8784.
            END IF
            IF (NSEC .GT. 0) THEN
               STORE = STORE + RYEARS*8784.*REAL(MXGLVL)
            END IF
            IF (PVMRM .OR. GRSM) THEN
               STORE = STORE + RYEARS*8784.*REAL(MXGLVL)
            END IF
            IF (PVMRM .OR. OLM .OR. RUNTTRM .OR. GRSM) THEN
               !For Ozone background
               STORE = STORE + RYEARS*8784.
            END IF
            IF(GRSM)THEN
              !For ANOXBGCONC
              STORE = STORE + RYEARS*8784.
            END IF
            IF (L_BACKGRND) THEN
               STORE = STORE + RYEARS*8784.
            END IF
            IF (HOURLY) THEN
               STORE = STORE + RYEARS*REAL(2*8784)*RSRC
               IF (NPNT .GT. 0) THEN
                  STORE = STORE + RYEARS*REAL(2*8784)*RSRC
               END IF
               IF (NVOL .GT. 0 .OR. NVMAX .GT. 0) THEN
                  STORE = STORE + RYEARS*REAL(2*8784)*RSRC
               END IF
            END IF
            IF (NURB .GT. 0) THEN
               STORE = STORE + REAL(4*8784*MXGLVL*NYEARS*NURB) +
     &                         REAL(4*8784*MXGLVL*NYEARS) +
     &                         REAL(5*8784*NYEARS*NURB)
            END IF
         END IF
         STORE = STORE*8./1.048576E6 + 3.5
      END IF

      ALLOCATE  (HRVAL(NUMTYP), AERVAL(NUMTYP), PRMVAL(NUMTYP),
     &           STAT=IASTAT)

C --- Allocate BACKAVE and BACKANN arrays to store total background contribution to GRP value
      ALLOCATE  (BACKAVE(NUMGRP), BACKANN(NUMGRP), STAT=IASTAT)

      IF (NARC .GT. 0) THEN
         ALLOCATE  (ARCMAX(NARC), QMAX(NARC), DXMAX(NARC), UMAX(NARC),
     &              SVMAX(NARC), SWMAX(NARC), SYMAX(NARC), SY3MX(NARC),
     &              U3MAX(NARC), HEMAX(NARC), ARCCL(NARC), SZMAX(NARC),
     &              CHIDMW(NARC), CHINMW(NARC), CHI3MW(NARC),
     &              CHIDML(NARC), CHINML(NARC), CHI3ML(NARC),
     &              HSBLMX(NARC),
     &              STAT=IASTAT)
      END IF

      IF (.NOT. EVONLY) THEN
         ALLOCATE  (AVEVAL(NUMREC,NUMGRP,NUMAVE,NUMTYP),
     &              HIVALU(NUMREC,NHIVAL,NUMGRP,NUMAVE,NUMTYP),
     &              HMAX(NHIVAL,NUMGRP,NUMAVE,NUMTYP),
     &              HMLOC(NHIVAL,NUMGRP,NUMAVE,NUMTYP),
     &              HMDATE(NHIVAL,NUMGRP,NUMAVE,NUMTYP),
     &              NHIDAT(NUMREC,NHIVAL,NUMGRP,NUMAVE,NUMTYP),
     &              STAT=IASTAT)
         IF (IASTAT .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','409','Result Array')
            ALLOC_ERR = .TRUE.
            WRITE(IOUNIT,*) ' '
            WRITE(IOUNIT,*) '  Error Occurred During Allocation of ',
     &                      'Short Term Average Results Arrays!'
         END IF

         IF (PERIOD .OR. ANNUAL) THEN
            ALLOCATE  (ANNVAL(NUMREC,NUMGRP,NUMTYP),
     &                 AMXVAL(NHIANN,NUMGRP,NUMTYP),
     &                 IMXLOC(NHIANN,NUMGRP,NUMTYP),
     &                 STAT=IASTAT)
            IF (IASTAT .NE. 0) THEN
               CALL ERRHDL(PATH,MODNAM,'E','409','Result Array')
               ALLOC_ERR = .TRUE.
               WRITE(IOUNIT,*) ' '
               WRITE(IOUNIT,*) '  Error Occurred During Allocation of ',
     &                         'Long Term Average Results Arrays!'
            END IF
         END IF

         ALLOCATE  (RMXVAL(NMXVAL,NUMGRP,NUMAVE,NUMTYP),
     &              MXDATE(NMXVAL,NUMGRP,NUMAVE,NUMTYP),
     &              MXLOCA(NMXVAL,NUMGRP,NUMAVE,NUMTYP),
     &              NUMHRS(NUMAVE), NUMCLM(NUMAVE), NUMMSG(NUMAVE),
     &              STAT=IASTAT)
         IF (IASTAT .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','409','Result Array')
            ALLOC_ERR = .TRUE.
            WRITE(IOUNIT,*) ' '
            WRITE(IOUNIT,*) '  Error Occurred During Allocation of ',
     &                      'Overall Maximum Results Arrays!'
         END IF

         IF (SEASONHR) THEN
            ALLOCATE (SHVALS(NUMREC,NUMGRP,4,24,NUMTYP),
     &                STAT=IASTAT)
            IF (IASTAT .NE. 0) THEN
               CALL ERRHDL(PATH,MODNAM,'E','409','Result Array')
               ALLOC_ERR = .TRUE.
               WRITE(IOUNIT,*) ' '
               WRITE(IOUNIT,*) '  Error Occurred During Allocation of ',
     &                         'SEASONHR Results Array!'
            END IF

            IF (L_BACKGRND) THEN
               ALLOCATE (BACKSEASHR(NUMGRP,4,24),STAT=IASTAT)
               IF (IASTAT .NE. 0) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','409','Result Array')
                  ALLOC_ERR = .TRUE.
                  WRITE(IOUNIT,*) ' '
                  WRITE(IOUNIT,*) '  Error Occurred During Allocation ',
     &                            'of SEASONHR BACKGRND Array!'
               END IF
            END IF
         END IF

         IF (PM25AVE .OR. NO2AVE .OR. SO2AVE) THEN
            NHIMXDLY = MAX( NHIMXDLY, NHIVAL )
            ALLOCATE (MXDVAL(NUMREC,NUMGRP),
     &                HIMXDLY(NUMREC,NUMGRP,NHIMXDLY),
     &                HIMXDLY_BYYR(NUMREC,NUMGRP,NHIMXDLY,NYEARS),
     &                IMXDHR(NUMREC,NUMGRP),
     &                NHIDATMXD(NUMREC,NUMGRP,NHIMXDLY),
     &                NHIDATMXD_BYYR(NUMREC,NUMGRP,NHIMXDLY,NYEARS),
     &                STAT=IASTAT)
            IF (IASTAT .NE. 0) THEN
               CALL ERRHDL(PATH,MODNAM,'E','409','Result Array')
               ALLOC_ERR = .TRUE.
               WRITE(IOUNIT,*) ' '
               WRITE(IOUNIT,*) '  Error Occurred During Allocation of ',
     &                         'MAXDAILY Results Arrays!'
           END IF
         END IF

         ALLOCATE  (HCLMSG(NUMREC,NHIVAL,NUMGRP,NUMAVE,NUMTYP),
     &              MCLMSG(NMXVAL,NUMGRP,NUMAVE,NUMTYP),
     &              HMCLM(NHIVAL,NUMGRP,NUMAVE,NUMTYP),STAT=IASTAT)
         IF (IASTAT .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','409','Result Array')
            ALLOC_ERR = .TRUE.
            WRITE(IOUNIT,*) ' '
            WRITE(IOUNIT,*) '  Error Occurred During Allocation of ',
     &                      'High Value Result Flag Arrays!'
         END IF

         IF (ANNUAL) THEN
            ALLOCATE  (SUMANN(NUMREC,NUMGRP,NUMTYP),
     &                 STAT=IASTAT)
            IF (IASTAT .NE. 0) THEN
               CALL ERRHDL(PATH,MODNAM,'E','409','Result Array')
               ALLOC_ERR = .TRUE.
               WRITE(IOUNIT,*) ' '
               WRITE(IOUNIT,*) '  Error Occurred During Allocation of ',
     &                         'ANNUAL Results Arrays!'
            END IF
         END IF

         IF (PM25AVE .OR. NO2AVE .OR. SO2AVE) THEN
            ALLOCATE  (SUMHNH(NUMREC,NUMGRP,NHIVAL),
     &                 MXPMVAL(NMXPM,NUMGRP,NHIVAL),
     &                 MXPMLOC(NMXPM,NUMGRP,NHIVAL),
     &                 STAT=IASTAT)
            IF (IASTAT .NE. 0) THEN
               CALL ERRHDL(PATH,MODNAM,'E','409','Result Array')
               ALLOC_ERR = .TRUE.
               WRITE(IOUNIT,*) ' '
               IF (PM25AVE) THEN
                  WRITE(IOUNIT,*) '  Error Occurred During Allocation',
     &                            ' of PM-2.5 24-hr Results Arrays!'
               ELSE IF (NO2AVE) THEN
                  WRITE(IOUNIT,*) '  Error Occurred During Allocation',
     &                            ' of NO2 1-hr Results Arrays!'
               ELSE IF (SO2AVE) THEN
                  WRITE(IOUNIT,*) '  Error Occurred During Allocation',
     &                            ' of SO2 1-hr Results Arrays!'
               END IF
            END IF
         END IF

      END IF

      IF (EVONLY) THEN
         ALLOCATE  (ASFCHF(NHR,1), AUREF(NHR,1),
     &              AUREFHT(NHR,1), ATA(NHR,1),
     &              ATREFHT(NHR,1), AWDREF(NHR,1),
     &              AUSTAR(NHR,1), AWSTAR(NHR,1),
     &              AZICONV(NHR,1), AZIMECH(NHR,1),
     &              AOBULEN(NHR,1), AVPTGZI(NHR,1),
     &              ASFCZ0(NHR,1), ABOWEN(NHR,1),
     &              AALBEDO(NHR,1), AWNEW(NHR,1),
     &              AWOLD(NHR,1), AESTA(NHR,1),
     &              AKST(NHR,1), ABLTA(NHR,1),
     &              AF2(NHR,1), APREC1(NHR,1), AQSW(NHR,1),
     &              APRATE(NHR,1), ARH(NHR,1), ASFCP(NHR,1),
     &              APREC2(NHR,1), IAPCODE(NHR,1), NACLOUD(NHR,1),
     &              ACLMHR(NHR,1), AMSGHR(NHR,1),
     &              AUNSTAB(NHR,1), ASTABLE(NHR,1),
     &              AURBSTAB(NHR,1),
     &              ANPLVLS(NHR,1), ANTGLVL(NHR,1),
     &              AO3CONC(NHR,1), ABGCONC(NHR,1),
     &              ANOXBGCONC(NHR,1),
     &              AAQS(NHR,1,NSRC), AAHS(NHR,1,NSRC),
     &              AAVS(NHR,1,NSRC), AATS(NHR,1,NSRC),
     &              AAFP(NHR,1,NSRC),
     &              AASYINI(NHR,1,NSRC), AASZINI(NHR,1,NSRC),
     &              AIFLAG(NHR,MXPLVL,1),
     &              APFLHT(NHR,MXPLVL,1), APFLWD(NHR,MXPLVL,1),
     &              APFLWS(NHR,MXPLVL,1), APFLTA(NHR,MXPLVL,1),
     &              APFLSA(NHR,MXPLVL,1), APFLSW(NHR,MXPLVL,1),
     &              APFLSV(NHR,MXPLVL,1), APFLTG(NHR,MXPLVL,1),
     &              APFLTGZ(NHR,MXPLVL,1),
     &              EV_AVEVAL(NSRC), HRVALS(NHR,NSRC),
     &              GRPVAL(NGRP,NHR), BACKHR(NGRP,NHR),
     &              GRPAVE(NGRP),
     &              STAT=IASTAT)
         IF (IASTAT .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','409','Result Array')
            ALLOC_ERR = .TRUE.
            WRITE(IOUNIT,*) ' '
            WRITE(IOUNIT,*) '  Error Occurred During Allocation of ',
     &                      'Event Arrays!'
         END IF
      ELSE IF (L_MAXDCONT) THEN
         ALLOCATE (ASFCHF(8784,NYEARS), AUREF(8784,NYEARS),
     &             AUREFHT(8784,NYEARS), ATA(8784,NYEARS),
     &             ATREFHT(8784,NYEARS), AWDREF(8784,NYEARS),
     &             AUSTAR(8784,NYEARS), AWSTAR(8784,NYEARS),
     &             AZICONV(8784,NYEARS), AZIMECH(8784,NYEARS),
     &             AOBULEN(8784,NYEARS), AVPTGZI(8784,NYEARS),
     &             ASFCZ0(8784,NYEARS),
     &             AKST(8784,NYEARS), ABLTA(8784,NYEARS),
     &             ACLMHR(8784,NYEARS), AMSGHR(8784,NYEARS),
     &             AUNSTAB(8784,NYEARS), ASTABLE(8784,NYEARS),
     &             AURBSTAB(8784,NYEARS),
     &             ANTGLVL(8784,NYEARS),
     &             AGRIDHT(8784,MXGLVL,NYEARS),
     &             AGRIDWD(8784,MXGLVL,NYEARS),
     &             AGRIDWS(8784,MXGLVL,NYEARS),
     &             AGRIDSW(8784,MXGLVL,NYEARS),
     &             AGRIDSV(8784,MXGLVL,NYEARS),
     &             AGRIDTG(8784,MXGLVL,NYEARS),
     &             AGRIDPT(8784,MXGLVL,NYEARS),
     &             AUATZI(8784,NYEARS),
     &             ASVATZI(8784,NYEARS),
     &             ASWATZI(8784,NYEARS),
     &             AUAVG(8784,NYEARS),
     &             ASVAVG(8784,NYEARS),
     &             ASWAVG(8784,NYEARS),
     &             APTATZI(8784,NYEARS),
     &             ANDX4ZI(8784,NYEARS),
     &             ARURUSTR(8784,NYEARS),
     &             ARUROBULEN(8784,NYEARS),
     &             STAT=IASTAT)
         IF (IASTAT .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','409','Result Array')
            ALLOC_ERR = .TRUE.
            WRITE(IOUNIT,*) ' '
            WRITE(IOUNIT,*) '  Error Occurred During Allocation of ',
     &                      'MAXDCONT Arrays!'
         END IF

C ---    Allocate arrays for NO2 options, PVMRM, OLM, GRSM, or TTRM
         IF (PVMRM .OR. GRSM) THEN
            ALLOCATE( AGRIDEPS(8784,MXGLVL,NYEARS),
     &                AO3CONC(8784,NYEARS), STAT=IASTAT )
         END IF
         IF (OLM) THEN
            ALLOCATE( AO3CONC(8784,NYEARS), STAT=IASTAT )
         END IF
         IF(GRSM)THEN
            ALLOCATE( ANOXBGCONC(8784,NYEARS), STAT=IASTAT )
         END IF
         IF (RUNTTRM) THEN
            ALLOCATE( AO3CONC(8784,NYEARS), STAT=IASTAT )
         END IF
         IF (L_BACKGRND) THEN
            ALLOCATE( ABGCONC(8784,NYEARS),STAT=IASTAT )
         END IF
         IF (NSEC .GT. 0) THEN
            ALLOCATE( AGRIDRHO(8784,MXGLVL,NYEARS),STAT=IASTAT )
         END IF
         IF (LDGAS .OR. LDPART .OR. LWPART .OR. LWGAS .OR. GRSM) THEN
            ALLOCATE( ABOWEN(8784,NYEARS),
     &           AALBEDO(8784,NYEARS), AWNEW(8784,NYEARS),
     &           AWOLD(8784,NYEARS), AESTA(8784,NYEARS),
     &           AF2(8784,NYEARS), APREC1(8784,NYEARS),
     &           APREC2(8784,NYEARS), APRATE(8784,NYEARS),
     &           ARH(8784,NYEARS), ASFCP(8784,NYEARS),AQSW(8784,NYEARS),
     &           IAPCODE(8784,NYEARS), NACLOUD(8784,NYEARS),STAT=IASTAT)
         END IF

         IF (IASTAT .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','409','Result Array')
            ALLOC_ERR = .TRUE.
            WRITE(IOUNIT,*) ' '
            WRITE(IOUNIT,*) '  Error Occurred During Allocation of ',
     &                      'MAXDCONT Arrays!'
         END IF

         IF (HOURLY) THEN
            ALLOCATE(
     &           AAQS(8784,NYEARS,NSRC), AAHS(8784,NYEARS,NSRC),
     &           STAT=IASTAT)
            IF (NPNT .GT. 0) THEN
               ALLOCATE(
     &           AAVS(8784,NYEARS,NSRC), AATS(8784,NYEARS,NSRC),
     &           STAT=IASTAT)
            END IF
            IF (NVOL .GT. 0 .OR. NVMAX .GT. 0) THEN
               ALLOCATE(
     &           AASYINI(8784,NYEARS,NSRC), AASZINI(8784,NYEARS,NSRC),
     &           STAT=IASTAT)
            END IF
            IF (NBLP .GT. 0) THEN
               ALLOCATE(
     &           AAFP(8784,NYEARS,NSRC), STAT=IASTAT)
            END IF
            IF (IASTAT .NE. 0) THEN
               CALL ERRHDL(PATH,MODNAM,'E','409','Result Array')
               ALLOC_ERR = .TRUE.
               WRITE(IOUNIT,*) ' '
               WRITE(IOUNIT,*) '  Error Occurred During Allocation',
     &                         ' of MAXDCONT Arrays for HOUREMIS!'
            END IF
         END IF

         IF (NURB .GT. 0) THEN
            ALLOCATE(
     &           AGRDSWR(8784,MXGLVL,NYEARS),
     &           AGRDSVR(8784,MXGLVL,NYEARS),
     &           AGRDTGR(8784,MXGLVL,NYEARS),
     &           AGRDPTR(8784,MXGLVL,NYEARS),
     &           AGRDSWU(8784,MXGLVL,NYEARS,NURB),
     &           AGRDSVU(8784,MXGLVL,NYEARS,NURB),
     &           AGRDTGU(8784,MXGLVL,NYEARS,NURB),
     &           AGRDPTU(8784,MXGLVL,NYEARS,NURB),
     &           AZIURB(8784,NYEARS,NURB),
     &           AURBWSTR(8784,NYEARS,NURB),
     &           AURBUSTR(8784,NYEARS,NURB),
     &           AURBOBULEN(8784,NYEARS,NURB),
     &           AL_MorningTrans(8784,NYEARS,NURB),
     &           STAT=IASTAT)
            IF (IASTAT .NE. 0) THEN
               CALL ERRHDL(PATH,MODNAM,'E','409','Result Array')
               ALLOC_ERR = .TRUE.
               WRITE(IOUNIT,*) ' '
               WRITE(IOUNIT,*) '  Error Occurred During Allocation',
     &                         ' of MAXDCONT Arrays for URBANOPT!'
            END IF

         END IF
      END IF

      RETURN
      END


      SUBROUTINE DATIME ( DCALL, TCALL )
C***********************************************************************
C                 DATIME Module
C
C        PURPOSE: Obtain the system date and time
C
C        PROGRAMMER: Jim Paumier, PES, Inc.
C
C        DATE:    April 15, 1994
C
C        MODIFIED:   Uses Fortran 90 DATE_AND_TIME routine.
C                    R.W. Brode, PES, 8/14/98
C
C        INPUTS:  none
C
C        OUTPUTS: Date and time in character format
C
C        CALLED FROM:  RUNTIME
C***********************************************************************
C
C     Variable Declarations
      IMPLICIT NONE

      CHARACTER DCALL*8, TCALL*8
      CHARACTER CDATE*8, CTIME*10, CZONE*5
      INTEGER :: IDATETIME(8)
      INTEGER :: IPTYR, IPTMON, IPTDAY, IPTHR, IPTMIN, IPTSEC

      DCALL = ' '
      TCALL = ' '

C     Call Fortran 90 date and time routine
      CALL DATE_AND_TIME (CDATE, CTIME, CZONE, IDATETIME)

C     Convert year to two digits and store array variables
      IPTYR  = IDATETIME(1) - 100 * INT(IDATETIME(1)/100)
      IPTMON = IDATETIME(2)
      IPTDAY = IDATETIME(3)
      IPTHR  = IDATETIME(5)
      IPTMIN = IDATETIME(6)
      IPTSEC = IDATETIME(7)

C     Write Date and Time to Character Variables, DCALL & TCALL
      WRITE(DCALL, '(2(I2.2,"/"),I2.2)' ) IPTMON, IPTDAY, IPTYR
      WRITE(TCALL, '(2(I2.2,":"),I2.2)' ) IPTHR, IPTMIN, IPTSEC

      RETURN
      END

      SUBROUTINE FILOPN
C***********************************************************************
C                 FILOPN Module
C
C        PURPOSE: Obtain the system date and time
C
C        PROGRAMMER: Roger Brode, PES, Inc.
C
C        DATE:    December 6, 1994
C
C        MODIFIED:   Remove non-standard option for
C                    CARRIAGECONTROL='Fortran' to control
C                    page feed in aermod.out file.  ASCII form
C                    feed character is used in subroutine HEADER
C                    to insert page feed instead of using Fortan
C                    carriage control.
C                    R.W. Brode, USEPA/OAQPS/AQMG, October 19, 2009
C
C        INPUTS:  Input filename, INPFIL
C                 Output filename, OUTFIL
C
C        OUTPUTS: Openned files
C
C        CALLED FROM:  HEADER
C
C        ERROR HANDLING:   Checks errors openning files
C***********************************************************************
C
C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      MODNAM = 'FILOPN'

C     OPEN Input Runstream File, Unit INUNIT=7
      DUMMY = 'RUN-STRM'
c     JAT 12/14/17 use user-supplied filename
c      OPEN (UNIT=INUNIT,FILE='aermod.inp',ACTION='READ',ERR=99,
      OPEN (UNIT=INUNIT,FILE=inpfil,ACTION='READ',ERR=99,
     &      STATUS='OLD')

C     OPEN Print Output File, Unit IOUNIT=8
      DUMMY = 'OUTPUT'
c     JAT 12/14/17 use user-supplied filename
c      OPEN (UNIT=IOUNIT,FILE='aermod.out',
      OPEN (UNIT=IOUNIT,FILE=outfil,
     &      ERR=99,STATUS='REPLACE')

C     Write Out Update to the Screen
      WRITE(*,909)
 909  FORMAT('+','Now Processing SETUP Information')

      GO TO 1000

C     WRITE Error Message:  Error Opening File
 99   CALL ERRHDL('  ',MODNAM,'E','500',DUMMY)

C     Check for Error Opening Runstream File and STOP
      IF (DUMMY .EQ. 'RUN-STRM') THEN
         WRITE(*,919)
 919     FORMAT('+','Error Opening Runstream Input File!  Aborting.')
         STOP
      END IF

 1000 CONTINUE

      RETURN
      END

      SUBROUTINE HEADER(IOUNT)
C***********************************************************************
C                 HEADER Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Control Page Feed and Header Information for
C                 Printed File Output
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    September 28, 1993
C
C        MODIFIED:   Use ASCII form feed character [ACHAR(12)] for
C                    page feed in 'aermap.out' file rather then
C                    CARRIAGECONTROL='Fortran', which is not a
C                    standard Fortran option.
C                    Include adjustments to header format for large
C                    page numbers.
C                    Include output file unit argument to support
C                    output to main 'aermod.out' file and to the
C                    optional SUMMFILE.
C                    R.W. Brode, USEPA/OAQPS/AQMG, October 19, 2009
C
C        MODIFIED:   Replace DEPLETE parameter for plume depletion option
C                    with DDPLETE and WDPLETE in the list of model options
C                    for Wet & Dry depletion.
C                    D. Strimaitis, SRC - 11/8/93
C
C        MODIFIED:   Header modified for draft version of model with new
C                    area source and deposition algorithms - 9/28/93
C
C        MODIFIED:   To add DEPLETE parameter for plume depletion option
C                    to the list of model options
C                    D. Strimaitis, SRC - 2/15/93
C
C        INPUTS:  Page Number from COMMON
C
C        OUTPUTS: Page Feed and Header
C
C        CALLED FROM:  (This Is An Utility Program)
C***********************************************************************
C
C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
CRCO 3/4/2021 Removing unused variable
C      INTEGER :: I, J, IOUNT, ILEN
      INTEGER :: J, IOUNT, ILEN
      CHARACTER FFEED*1
C Unused: INTEGER :: I

C     Variable Initializations
      MODNAM = 'HEADER'

C*    FFEED is ASCII form-feed character
      J = 12
      FFEED  = ACHAR(J)

C     Increment Page Number Counter
      IF (IOUNT .EQ. IOUNIT) THEN
         IPAGE = IPAGE + 1
      ELSE IF (IOUNT .EQ. ISUMUNT) THEN
         IPGSUM = IPGSUM + 1
      END IF

C     Write Header to Printed Output File
      WRITE(IOUNT,9028) FFEED, VERSN, TITLE1(1:68), RUNDAT
      IF (IOUNT .EQ. IOUNIT) THEN
C        Adjust format statement based on page number
         IF (IPAGE .LE. 999) THEN
            WRITE(IOUNT,9029) C_METVER, TITLE2(1:68), RUNTIM, IPAGE
         ELSE IF (IPAGE .LE. 99999) THEN
            WRITE(IOUNT,90291) C_METVER, TITLE2(1:68), RUNTIM, IPAGE
         ELSE IF (IPAGE .LE. 9999999) THEN
            WRITE(IOUNT,90292) C_METVER, TITLE2(1:68), RUNTIM, IPAGE
         ELSE
            WRITE(IOUNT,90292) C_METVER, TITLE2(1:68), RUNTIM,
     &                                              MIN(IPAGE,99999999)
         END IF
      ELSE IF (IOUNT .EQ. ISUMUNT) THEN
         WRITE(IOUNT,9029) C_METVER, TITLE2(1:68), RUNTIM, IPGSUM
      END IF
      ILEN = LEN_TRIM( MODOPS_String )
      IF (ILEN .LE. 110) THEN
         WRITE(IOUNT,9030) MODOPS_String(1:LEN_TRIM(MODOPS_String))
      ELSE
         WRITE(IOUNT,9030) MODOPS_String(1:LEN_TRIM(MODOPS_String))
         WRITE(IOUNT,9040) MODOPS_String(LEN_TRIM(MODOPS_String)+1:)
      END IF

 9028 FORMAT(A1,1X,'*** AERMOD - VERSION ',A6,' ***',3X,'*** ',A68,
     &       ' ***',8X,A8)
 9029 FORMAT(1X,'*** AERMET - VERSION ',A6,' ***',3X,'*** ',A68,' ***',
     &       8X,A8,/T120,'PAGE',I4)
90291 FORMAT(1X,'*** AERMET - VERSION ',A6,' ***',3X,'*** ',A68,' ***',
     &       8X,A8,/T118,'PAGE',I6)
90292 FORMAT(1X,'*** AERMET - VERSION ',A6,' ***',3X,'*** ',A68,' ***',
     &       8X,A8,/T116,'PAGE',I8)
 9030 FORMAT(1X,'*** MODELOPTs: ',1X,A:)
 9040 FORMAT(4X,A:)

      RETURN
      END

      SUBROUTINE DCDLAT ()
C***********************************************************************
C            DCDLAT Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: To decode the hemisphere and latitude from
C                 the character variable ALAT (record 1 in scalar file)
C
C        PROGRAMMER: Jim Paumier, PES, Inc.
C
C        DATE:       September 30, 1993
C
C        INPUTS:  ALAT, the character variable latitude from AERMET
C
C        ASSUMPTIONS:  The first field in the first record of the
C                      scalar input file contains the latitude
C
C        OUTPUTS: Hemisphere (NORTH or SOUTH), latitude and sign (TSIGN)
C                 for turning of wind with height
C
C        CALLED FROM:  HRLOOP
C***********************************************************************

C---- Variable declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER      NORS, SORN

C---- Data initialization
      MODNAM = 'DCDLAT'
      PATH   = 'ME'

C---- Determine if the letter 'N' or 'n' is in the latitude field
      NORS = INDEX(ALAT,'N') + INDEX(ALAT,'n')

      IF( NORS .NE. 0 )THEN

C        The latitude is in the northern hemisphere; decode the latitude

         TSIGN = 1.0D0
         READ( ALAT, '(F9.1)',ERR=1000 ) XLAT

C        Write a message if the latitude is too far north

         IF( XLAT .GT. 90.0D0  .OR.  XLAT .LT. 0.0D0 )THEN
C           Write a warning to the user - latitude out-of-range
            CALL ERRHDL(PATH,MODNAM,'W','381',ALAT)
         END IF

      ELSE

C        The latitude may be in the southern hemisphere

         SORN = INDEX(ALAT,'S') + INDEX(ALAT,'s')
         IF( SORN .NE. 0 )THEN
            TSIGN = -1.0D0
            READ( ALAT, '(F9.1)',ERR=1000 ) XLAT

            IF( XLAT .GT. 90.0D0  .OR.  XLAT .LT. 0.0D0 )THEN
C              Write a warning to the user - latitude out-of-range
               CALL ERRHDL(PATH,MODNAM,'W','381',ALAT)
            END IF


         ELSE
C           Write a warning to the user - error decoding the latitude
            CALL ERRHDL(PATH,MODNAM,'W','382',ALAT)

         END IF

      END IF

      GO TO 999

1000  CONTINUE
C     Write a warning to the user - error decoding the latitude
      CALL ERRHDL(PATH,MODNAM,'W','382',ALAT)

999   RETURN
      END

      SUBROUTINE PRESET
C***********************************************************************
C                 PRESET Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Preprocesses SETUP Information to Determine Data
C                 Storage Requirements
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    September 24, 1996
C
C        MODIFIED:   To include new options related to GRSM NO2 option
C                    CERC, 11/30/20
C
C        MODIFIED:   Added new NUMYEARS option to specify the number
C                    of years of met data for allocating arrays for
C                    the MAXDCONT option. The default number of years
C                    is still five (5) years.
C                    Also modified to allow for the use of URBANSRC ALL
C                    on the SO pathway to indicate that all sources are
C                    to be treated as URBAN sources. This option assumes
C                    that only one (1) urban area has been defined using
C                    the CO URBANOPT keyword.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/29/2012
C
C        MODIFIED:   Added calculation of STORE, estimated memory
C                    storage requirements, to report if allocation
C                    errors occur.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
C
C        MODIFIED:   To include new options incorporated in version
C                    dated 06341.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG - 12/07/2006
C
C        MODIFIED:   To check for NO ECHO in the input file.
C                    R.W. Brode, PES, Inc. - 12/2/98
C
C        INPUTS:  Input Runstream File
C
C        OUTPUTS: Array Sizes
C
C        CALLED FROM:   MAIN
C***********************************************************************
C
C     Variable Declarations
      USE MAIN1
      USE BUOYANT_LINE
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, J, K, ISPRD, IEPRD
C     Declare Real Variables used to estimate memory requirements
C     JAT D065 7/22/21 NONE OF THE VARIABLES BELOW ARE USED IN THIS SUBROUTINE
C     THEY ARE ACTUALLY SET IN ALLRESULT
C      REAL    :: RSRC, RSEC, RGRP, RREC, RURB, RARC, RAVE,
C     &           RVAL, RTYP, RMAX, RNET, RXM , RYM , REVE, ROLM, RPSD,
C     &           RQF, RPDMAX, RVMAX, RPAIR, RHIANN

      LOGICAL NOPATH, NOKEY, L_O3data, L_NOxData
      CHARACTER RDFRM*20
      CHARACTER LPRD*8, HPRD*8, NCHR1(10)*8, NCHR2(10)*5
      LOGICAL RMARK
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

C     Variable Initializations
      DATA (NCHR1(I),I=1,10) /'FIRST','SECOND','THIRD','FOURTH',
     &                        'FIFTH','SIXTH','SEVENTH','EIGHTH',
     &                        'NINTH','TENTH'/
      DATA (NCHR2(I),I=1,10) /'1ST','2ND','3RD','4TH','5TH',
     &                        '6TH','7TH','8TH','9TH','10TH'/

C     Variable Initializations
      MODNAM = 'PRESET'
      PREVSRCID = '        '
      PREVGRPID = '        '
      PATH  = '  '
      PPATH = '  '
      EOF = .FALSE.
      NSEC    = 0
      NPDMAX  = 0
      NQF     = 0
      NBF     = 0
      NURB    = 0
      NVMAX   = 0
      ILINE   = 0

C     Initialize PATHWY array
      PATHWY(1) = 'CO'
      PATHWY(2) = 'SO'
      PATHWY(3) = 'RE'
      PATHWY(4) = 'ME'
      PATHWY(5) = 'OU'
      PATHWY(6) = '**'
      PATHWY(7) = 'EV'

      IPNUM  = 0
      IPPNUM = 0
C     Counters for the Receptor Groups
      IREC = 0
      ISTA = .FALSE.
      IEND = .FALSE.
      NEWID = .TRUE.
C     Initialize logical for urban option and multiple urban areas
      L_PRESET_URBAN = .FALSE.
      L_MULTURB      = .FALSE.
C     Initialize logical for the 'URBANSRC ALL' option
      L_URBAN_ALL = .FALSE.
C     Initialize logical for sector-varying O3 data
      L_O3Sector = .FALSE.
C     Initialize logical for O3 data inputs
      L_O3data = .FALSE.
C     Initialize logical for sector-varying Background data
      L_BGSector = .FALSE.
C     Initialize logical for BG data inputs
      L_BACKGRND = .FALSE.
C     Initialize logical for sector-varying NOx data
      L_NOxSector = .FALSE.
C     Initialize logical for NOx data inputs
      L_NOxData = .FALSE.
C     Initialize logical for BUOYLINE source
      L_BLSOURCE = .FALSE.
C     Initialize file format to 'FIX'; will be overridden if
C     user specified 'EXP' format on OU FILEFORM keyword
      FILE_FORMAT = 'FIX'

C --- Initialize all NO2 options to .FALSE.
      ARM2   = .FALSE.
      OLM    = .FALSE.
      PVMRM  = .FALSE.
      GRSM  = .FALSE.

C --- Initialize MODOPTS_String character variable
      MODOPS_String = ''

C     Setup READ format and ECHO format for runstream record,
C     based on the ISTRG PARAMETER (set in MAIN1)
      WRITE(RDFRM,9100) ISTRG, ISTRG
 9100 FORMAT('(A',I4.4,',T1,',I4.4,'A1)')

C --- First loop through Runstream Records to identify
C     O3SECTOR/NOXSECTR option and BGSECTOR/BACKGRND options
C     since they are interdependant;
C     This avoids imposing a requirement on the
C     order of these keywords:
C     LOOP Through Input Runstream Records
      DO WHILE (.NOT. EOF)

C        Increment the Line Counter
         ILINE = ILINE + 1

C        READ Record to Buffers, as A'num' and 'num'A1 for 'num' = ISTRG.
C        Length of ISTRG is Set in PARAMETER Statement in MAIN1
         READ (INUNIT,RDFRM,END=998) RUNST1, (RUNST(I), I = 1, ISTRG)

C        Check for blank input record and cycle
         IF (LEN_TRIM(RUNST1) .EQ. 0) CYCLE

C        Convert Lower Case to Upper Case Letters           ---   CALL LWRUPR
         CALL LWRUPR

C        Define Fields on Card                              ---   CALL DEFINE
         CALL DEFINE

C        Get the Contents of the Fields                     ---   CALL GETFLD
         CALL GETFLD

C        If Blank Line, Then CYCLE to Next Card
         IF (BLINE) GO TO 10

C        Check for 'NO ECHO' In First Two Fields
         IF (FIELD(1) .EQ. 'NO' .AND. FIELD(2) .EQ. 'ECHO') THEN
C           Skip record with NO ECHO during PRESET stage of processing
            GO TO 10
         END IF

C        Extract Pathway ID From Field 1                    ---   CALL EXPATH
         CALL EXPATH(FIELD(1),PATHWY,7,NOPATH)

C        For Invalid Pathway and Comment Lines Skip to Next Record
         IF (NOPATH) THEN
C           Skip Error Message for PRESET stage of processing
            PATH = PPATH
            GO TO 10
         ELSE IF (PATH .EQ. '**') THEN
            GO TO 10
         END IF

C        Extract Keyword From Field 2                       ---   CALL EXKEY
         CALL EXKEY(FIELD(2),NOKEY)

         IF (NOKEY) THEN
C           No Keyword - Skip Error Message for PRESET stage
               PKEYWD = KEYWRD
            GO TO 10
         END IF

C        Save Current Path and Path Number as Previous Path and Number
         PPATH = PATH
         IPPNUM = IPNUM

C        First process cards to determine whether O3SECTOR and/or BGSECTOR keywords are used
         IF (PATH .EQ. 'CO') THEN

            IF (KEYWRD .EQ. 'O3SECTOR') THEN
C ---          Assign logical variable for O3SECTORs
               L_O3Sector = .TRUE.
               CYCLE

            ELSEIF (KEYWRD .EQ. 'OZONEFIL' .OR.
     &              KEYWRD .EQ. 'O3VALUES' .OR.
     &              KEYWRD .EQ. 'OZONEVAL') THEN
               L_O3Data = .TRUE.
               CYCLE

            ELSEIF (KEYWRD .EQ. 'EVENTFIL') THEN
               CYCLE

            ELSEIF (KEYWRD .EQ. 'LOW_WIND') THEN
               LOW_WIND = .TRUE.

C           CERC 11/30/20
            ELSEIF(KEYWRD .EQ. 'NOXSECTR') THEN
C ---          Assign logical variable for NOxSECTORs
               L_NOxSector = .TRUE.
               CYCLE

            ELSEIF (KEYWRD .EQ. 'NOX_FILE' .OR.
     &              KEYWRD .EQ. 'NOX_VALS' .OR.
     &              KEYWRD .EQ. 'NOXVALUE')THEN
               L_NOxData = .TRUE.
               CYCLE

            END IF

         ELSE IF (PATH .EQ. 'SO') THEN

            IF (KEYWRD .EQ. 'BGSECTOR') THEN
C ---          Assign logical variable for BGSECTORs
               L_BGSector = .TRUE.
               CYCLE
            ELSE IF (KEYWRD .EQ. 'BACKGRND') THEN
C ---          Assign logical variable for BACKGRND
               L_BACKGRND = .TRUE.
               CYCLE
            ELSE IF (KEYWRD .EQ. 'INCLUDED' .AND.
     &               (.NOT.L_BGSector .OR. .NOT.L_BACKGRND)) THEN
C ---          Call PREINCLUD since BGSECTOR and/or BACKGRND, URBANSRC,
C              and/or BLPINPUT may be in an INCLUDED file
               CALL PREINCLUD
               CYCLE
            ELSE IF (KEYWRD .EQ. 'URBANSRC') THEN
               L_Urban = .TRUE.
               IF (FIELD(3) .EQ. 'ALL') THEN
                  L_Rural = .FALSE.
               END IF
               CYCLE

            ELSE IF (KEYWRD .EQ. 'BLPINPUT') THEN
               L_BLSOURCE = .TRUE.
               CYCLE

            ELSE IF (KEYWRD .EQ. 'BLPGROUP') THEN
               L_BLSOURCE = .TRUE.               ! (Multiple_BuoyLines_D41_Wood)
               CYCLE
            END IF

C ---       Exit pre-PRESET loop if BGSECTOR and BACKGRND
C           have been specified
            IF (L_BGSector .AND. L_BACKGRND) THEN
               EXIT
            END IF

         ELSE IF (PATH .EQ. 'RE') THEN
C ---       CO and SO pathways already processed:
C           Exit pre-PRESET loop
            EXIT

         END IF

         GO TO 10
 998     EOF = .TRUE.
 10      CONTINUE
      END DO

C --- Check for O3SECTOR keyword without O3 data inputs
      IF (L_O3Sector .AND. .NOT.L_O3Data) THEN
C ----   Issue warning message for O3SECTOR without O3 data
         CALL ERRHDL(PATH,MODNAM,'W','166','O3SECTOR')
      END IF
      IF (L_BGSector .AND. .NOT.L_BACKGRND) THEN
C ----   Issue warning message for O3SECTOR without O3 data
         CALL ERRHDL(PATH,MODNAM,'W','166','BGSECTOR')
      END IF
C --- Check for NOXSECTR keyword without NOX data inputs
      IF (L_NOXSector .AND. .NOT.L_NOXData) THEN
C ----   Issue warning message for NOXSECTR without NOX data
         CALL ERRHDL(PATH,MODNAM,'W','166','NOXSECTR')
      END IF

C --- Rewind Input Runstream File for complete pre-setup
      REWIND INUNIT

      PATH  = '  '
      PPATH = '  '
      IPNUM  = 0
      IPPNUM = 0
      EOF = .FALSE.

C     LOOP Through Input Runstream Records
      DO WHILE (.NOT. EOF)

C        Increment the Line Counter
         ILINE = ILINE + 1

C        READ Record to Buffers, as A'num' and 'num'A1 for 'num' = ISTRG.
C        Length of ISTRG is Set in PARAMETER Statement in MAIN1
         READ (INUNIT,RDFRM,END=999) RUNST1, (RUNST(I), I = 1, ISTRG)

C        Check for blank input record and cycle
         IF (LEN_TRIM(RUNST1) .EQ. 0) CYCLE

C        Convert Lower Case to Upper Case Letters           ---   CALL LWRUPR
         CALL LWRUPR

C        Define Fields on Card                              ---   CALL DEFINE
         CALL DEFINE

C        Get the Contents of the Fields                     ---   CALL GETFLD
         CALL GETFLD

C        Check for 'NO ECHO' In First Two Fields
         IF (FIELD(1) .EQ. 'NO' .AND. FIELD(2) .EQ. 'ECHO') THEN
C           Skip record with NO ECHO during PRESET stage of processing
            CYCLE
         END IF

C        Extract Pathway ID From Field 1                    ---   CALL EXPATH
         CALL EXPATH(FIELD(1),PATHWY,7,NOPATH)

C        For Invalid Pathway and Comment Lines Skip to Next Record
         IF (NOPATH) THEN
C           Skip Error Message for PRESET stage of processing
            PATH = PPATH
            CYCLE
         ELSE IF (PATH .EQ. '**') THEN
            CYCLE
         END IF

C        Extract Keyword From Field 2                       ---   CALL EXKEY
         CALL EXKEY(FIELD(2),NOKEY)

         IF (NOKEY) THEN
C           Invalid Keyword - Skip Error Message for PRESET stage
            PKEYWD = KEYWRD
            CYCLE
         END IF

C        Save Current Path and Path Number as Previous Path and Number
         PPATH = PATH
         IPPNUM = IPNUM

C        Process Cards to Determine Storage Requirements
         IF (PATH .EQ. 'CO') THEN
            IF (KEYWRD .EQ. 'MODELOPT') THEN
               DO I = 3, IFC
                  IF (FIELD(I) .EQ. 'CONC'  .OR.
     &                FIELD(I) .EQ. 'DEPOS' .OR.
     &                FIELD(I) .EQ. 'DDEP'  .OR.
     &                FIELD(I) .EQ. 'WDEP') THEN
                     NTYP = NTYP + 1
                  END IF
C                 Set PVMRM/OLM/ARM2/GRSM/TTRM logicals for use in ALLSETUP
                  IF (FIELD(I) .EQ. 'PVMRM') THEN
                     PVMRM = .TRUE.
                  ELSE IF (FIELD(I) .EQ. 'PSDCREDIT' )THEN
                     PSDCREDIT = .TRUE.
C----                Number of "source groups" will be set to 2 below for
C                    PSDCREDIT applications, to account for hardwired
C                    'NAAQS' and 'PSDINC' source groups, otherwise it
C                    would be overwritten in SRCSIZ
                  ELSE IF (FIELD(I) .EQ. 'OLM') THEN
                     OLM = .TRUE.
                  ELSE IF (FIELD(I) .EQ. 'ARM2') THEN
                     ARM2 = .TRUE.
                  ELSE IF (FIELD(I) .EQ. 'GRSM') THEN
                     GRSM = .TRUE.
                  ELSE IF (FIELD(I) .EQ. 'TTRM') THEN
                     RUNTTRM = .TRUE.
                  ELSE IF (FIELD(I) .EQ. 'TTRM2') THEN
                     RUNTTRM = .TRUE.
                     RUNTTRM2 = .TRUE.
                  END IF
C---              Check for ALPHA and BETA options
                  IF (FIELD(I) .EQ. 'ALPHA') THEN
                     L_ALPHA = .TRUE.
                  ELSE IF (FIELD(I) .EQ. 'BETA') THEN
                     BETA = .TRUE.
                  END IF
               END DO

            ELSE IF (KEYWRD .EQ. 'AVERTIME') THEN
               DO I = 3, IFC
                  IF (FIELD(I).NE.'PERIOD' .AND.
     &                FIELD(I).NE.'ANNUAL') THEN
                     NAVE = NAVE + 1
                  END IF
               END DO

            ELSE IF (KEYWRD .EQ. 'URBANOPT') THEN
               NURB = NURB + 1
C----          Set preliminary flag for URBAN option, used to allow flexibility in
C              order of CO pathway keywords for URBANOPT
               L_PRESET_URBAN = .TRUE.
               IF (NURB .GT. 1) THEN
                  L_MULTURB = .TRUE.
               END IF

            ELSE IF (KEYWRD .EQ. 'O3SECTOR') THEN
C ---          Assign logical variable for O3SECTORs
               L_O3Sector = .TRUE.
C ---          Set maximum array limit for number of ozone sectors
               NUMO3Sects = IFC - 2

            ELSE IF (KEYWRD .EQ. 'O3VALUES') THEN
C ---          Set maximum array limit for temporally-varying
C              ozone concentrations for O3VALUES keyword
C              Assign field index to 4 if O3SECTOR is used, otherwise 3
               IF (L_O3Sector) THEN
                  K = 4
               ELSE
                  K = 3
               END IF
               IF (FIELD(K) .EQ. 'ANNUAL') THEN
                  NO3F = MAX( NO3F, 1)
               ELSE IF (FIELD(K) .EQ. 'SEASON') THEN
                  NO3F = MAX( NO3F, 4)
               ELSE IF (FIELD(K) .EQ. 'MONTH') THEN
                  NO3F = MAX( NO3F, 12)
               ELSE IF (FIELD(K) .EQ. 'HROFDY') THEN
                  NO3F = MAX( NO3F, 24)
               ELSE IF (FIELD(K) .EQ. 'WSPEED') THEN
                  NO3F = MAX( NO3F, 6)
               ELSE IF (FIELD(K) .EQ. 'SEASHR') THEN
                  NO3F = MAX( NO3F, 96)
               ELSE IF (FIELD(K) .EQ. 'HRDOW') THEN
                  NO3F = MAX( NO3F, 72)
               ELSE IF (FIELD(K) .EQ. 'HRDOW7') THEN
                  NO3F = MAX( NO3F, 168)
               ELSE IF (FIELD(K) .EQ. 'SHRDOW') THEN
                  NO3F = MAX( NO3F, 288)
               ELSE IF (FIELD(K) .EQ. 'SHRDOW7') THEN
                  NO3F = MAX( NO3F, 672)
               ELSE IF (FIELD(K) .EQ. 'MHRDOW') THEN
                  NO3F = MAX( NO3F, 864)
               ELSE IF (FIELD(K) .EQ. 'MHRDOW7') THEN
                  NO3F = MAX( NO3F, 2016)
               END IF

C           CERC 11/30/20
            ELSE IF (KEYWRD .EQ. 'NOXSECTR') THEN
C ---          Assign logical variable for NOxSECTORs
               L_NOxSector = .TRUE.
C ---          Set maximum array limit for number of NOx sectors
               NUMNOxSects = IFC - 2

            ELSE IF (KEYWRD .EQ. 'NOX_VALS') THEN
C ---          Set maximum array limit for temporally-varying
C              NOx concentrations for NOX_VALS keyword
C              Assign field index to 4 if NOXSECTR is used, otherwise 3
               IF (L_NOxSector) THEN
                  K = 4
               ELSE
                  K = 3
               END IF
               IF (FIELD(K) .EQ. 'ANNUAL') THEN
                  NNOXF = MAX( NNOXF, 1)
               ELSE IF (FIELD(K) .EQ. 'SEASON') THEN
                  NNOXF = MAX( NNOXF, 4)
               ELSE IF (FIELD(K) .EQ. 'MONTH') THEN
                  NNOXF = MAX( NNOXF, 12)
               ELSE IF (FIELD(K) .EQ. 'HROFDY') THEN
                  NNOXF = MAX( NNOXF, 24)
               ELSE IF (FIELD(K) .EQ. 'WSPEED') THEN
                  NNOXF = MAX( NNOXF, 6)
               ELSE IF (FIELD(K) .EQ. 'SEASHR') THEN
                  NNOXF = MAX( NNOXF, 96)
               ELSE IF (FIELD(K) .EQ. 'HRDOW') THEN
                  NNOXF = MAX( NNOXF, 72)
               ELSE IF (FIELD(K) .EQ. 'HRDOW7') THEN
                  NNOXF = MAX( NNOXF, 168)
               ELSE IF (FIELD(K) .EQ. 'SHRDOW') THEN
                  NNOXF = MAX( NNOXF, 288)
               ELSE IF (FIELD(K) .EQ. 'SHRDOW7') THEN
                  NNOXF = MAX( NNOXF, 672)
               ELSE IF (FIELD(K) .EQ. 'MHRDOW') THEN
                  NNOXF = MAX( NNOXF, 864)
               ELSE IF (FIELD(K) .EQ. 'MHRDOW7') THEN
                  NNOXF = MAX( NNOXF, 2016)
               END IF

C           3/18/22 Wood, D127Added FRANMIN to LOW_WIND option
CCRT        4/11/2022 D131 FRAN Alpha - Add momentum balance option
            ELSE IF (KEYWRD .EQ. 'LOW_WIND') THEN
C----          Check for LOW_WIND keyword for users to adjust parameters used
C              with the LOW_WIND ALPHA option
               IF( IFC .EQ. 3 )THEN
                  L_UserSVmin = .TRUE.
               ELSE IF( IFC .EQ. 4 )THEN
                  L_UserSVmin = .TRUE.
                  L_UserWSmin = .TRUE.
               ELSE IF( IFC .EQ. 5 )THEN
                  L_UserSVmin = .TRUE.
                  L_UserWSmin = .TRUE.
                  L_UserFRANmax = .TRUE.
               ELSE IF( IFC .EQ. 6 )THEN
                  L_UserSVmin = .TRUE.
                  L_UserWSmin = .TRUE.
                  L_UserFRANmax = .TRUE.
                  L_UserSWmin = .TRUE.
               ELSE IF( IFC .EQ. 7 )THEN
                  L_UserSVmin = .TRUE.
                  L_UserWSmin = .TRUE.
                  L_UserFRANmax = .TRUE.
                  L_UserSWmin = .TRUE.
                  L_UserBigT = .TRUE.
               ELSE IF( IFC .EQ. 8 )THEN
                  L_UserSVmin = .TRUE.
                  L_UserWSmin = .TRUE.
                  L_UserFRANmax = .TRUE.
                  L_UserSWmin = .TRUE.
                  L_UserBigT = .TRUE.
                  L_UserFRANmin = .TRUE.
               ELSE IF( IFC .EQ. 9 )THEN
                  L_UserSVmin = .TRUE.
                  L_UserWSmin = .TRUE.
                  L_UserFRANmax = .TRUE.
                  L_UserSWmin = .TRUE.
                  L_UserBigT = .TRUE.
                  L_UserFRANmin = .TRUE.
                  IF (FIELD(IFC) .EQ. 'PBAL' .OR.
     &                FIELD(IFC) .EQ. 'PBALANCE') THEN
                     L_PBal = .TRUE.
                  END IF
               END IF
C ---          Assign LOW_WIND to MODOPS(22)
               MODOPS(22) = 'LOW_WIND'
            END IF

         ELSE IF (PATH .EQ. 'SO') THEN
            CALL SRCSIZ

         ELSE IF (PATH .EQ. 'RE') THEN
            EVONLY = .FALSE.
            CALL RECSIZ

         ELSE IF (PATH .EQ. 'EV') THEN
            EVONLY = .TRUE.
            IF (KEYWRD .EQ. 'EVENTPER') THEN
               NEVE = NEVE + 1
            ELSE IF (KEYWRD .EQ. 'INCLUDED') THEN
               CALL PREINCLUD
            END IF

         ELSE IF (PATH .EQ. 'ME' .AND. KEYWRD .EQ. 'SURFDATA') THEN
C           Read start year from SURFDATA card to establish date window
            CALL SET_WINDOW

         ELSE IF (PATH .EQ. 'ME' .AND. KEYWRD .EQ. 'NUMYEARS') THEN
C ---       Set number of years for allocating the MAXDCONT arrays
            IF (IFC .EQ. 3) THEN
               CALL STONUM(FIELD(3),ILEN_FLD,FNUM,IMIT)
               IF (IMIT .EQ. 1) THEN
                  NYEARS = NINT(FNUM)
               END IF
            END IF

         ELSE IF (PATH .EQ. 'OU') THEN
            IF(KEYWRD .EQ. 'RECTABLE') THEN
C              Begin LOOP Through Fields
               DO I = 4, IFC
C ---             Skip processing of fields if IFC > IFMAX
                  IF (I .GT. IFMAX) EXIT
C                 Retrieve The High Value
                  CALL FSPLIT(PATH,KEYWRD,FIELD(I),ILEN_FLD,'-',RMARK,
     &                        LPRD,HPRD)
                  ISPRD = 0
                  IEPRD = 0
C ---             First check for simple numeric value
                  CALL STONUM(LPRD,ILEN_FLD,FNUM,IMIT)
                  IF (IMIT .EQ. 1) THEN
                     ISPRD = INT(FNUM)
                  END IF
                  CALL STONUM(HPRD,ILEN_FLD,FNUM,IMIT)
                  IF (IMIT .EQ. 1) THEN
                     IEPRD = INT(FNUM)
                  END IF
C ---             Now check for character strings NCHR1 or NCHR2
                  DO J = 1, 10
                     IF (LPRD.EQ.NCHR1(J) .OR.
     &                   LPRD.EQ.NCHR2(J)) ISPRD = J
                     IF (HPRD.EQ.NCHR1(J) .OR.
     &                   HPRD.EQ.NCHR2(J)) IEPRD = J
                  END DO
                  IF (ISPRD .GT. 999 .OR. IEPRD .GT. 999) THEN
C                    Write Error Message:Illegal Parameter Field
                     CALL ERRHDL(PATH,MODNAM,'E','203','HIVALU')
                     CYCLE
                  END IF
                  IF (ISPRD .GT. NVAL) THEN
                     NVAL = ISPRD
                  END IF
                  IF (IEPRD .GT. NVAL) THEN
                     NVAL = IEPRD
                  END IF
C              End LOOP Through Fields
               END DO

            ELSE IF (KEYWRD .EQ. 'MAXTABLE' .OR.
     &               KEYWRD .EQ. 'RANKFILE') THEN
C              Set Number of Maximum Values to Sort
               CALL STONUM(FIELD(4),ILEN_FLD,FNUM,IMIT)
               IF (IMIT .NE. 1) THEN
C                 Invalid Numerical Field
                  GO TO 999
               END IF
               INUM = NINT(FNUM)
               IF (INUM .GT. NMXVAL) THEN
                  NMXVAL = INUM
               END IF

            ELSE IF (KEYWRD .EQ. 'SEASONHR') THEN
C              Set SEASONHR logical flag to account for SHVALS array needs
               SEASONHR = .TRUE.

            ELSE IF (KEYWRD .EQ. 'MAXDCONT') THEN
C              Set MAXDCONT logical flag to account for MAXDCONT array needs
               L_MAXDCONT = .TRUE.

            ELSE IF (KEYWRD .EQ. 'FILEFORM' .AND.
     &               FIELD(3)(1:3) .EQ. 'EXP') THEN
C ---          Check for FILEFORM keyword with 'EXP' format in order to
C              include correct format in output file headers
               FILE_FORMAT = 'EXP'

            ELSE IF (KEYWRD .EQ. 'NOHEADER') THEN
C              Set NOHEADER logical flag to suppress output file headers
               DO I = 3, IFC
                  IF (FIELD(I) .EQ. 'ALL') THEN
C                    No headers for any ouput file type
                     L_NoHeader(:) = .TRUE.
                     EXIT
                  ELSE IF (FIELD(I) .EQ. 'MAXIFILE') THEN
C                    No headers for MAXIFILE
                     L_NoHeader(1) = .TRUE.
                  ELSE IF (FIELD(I) .EQ. 'POSTFILE') THEN
C                    No headers for POSTFILE
                     L_NoHeader(2) = .TRUE.
                  ELSE IF (FIELD(I) .EQ. 'PLOTFILE') THEN
C                    No headers for PLOTFILE
                     L_NoHeader(3) = .TRUE.
                  ELSE IF (FIELD(I) .EQ. 'SEASONHR') THEN
C                    No headers for SEASONHR
                     L_NoHeader(4) = .TRUE.
                  ELSE IF (FIELD(I) .EQ. 'RANKFILE') THEN
C                    No headers for RANKFILE
                     L_NoHeader(5) = .TRUE.
                  ELSE IF (FIELD(I) .EQ. 'MAXDAILY') THEN
C                    No headers for MAXDAILY
                     L_NoHeader(6) = .TRUE.
                  ELSE IF (FIELD(I) .EQ. 'MXDYBYYR') THEN
C                    No headers for MXDYBYYR
                     L_NoHeader(7) = .TRUE.
                  ELSE IF (FIELD(I) .EQ. 'MAXDCONT') THEN
C                    No headers for MAXDCONT
                     L_NoHeader(8) = .TRUE.
                  END IF
               END DO

            ELSE IF (KEYWRD .EQ. 'FINISHED') THEN
C              Check for 'OU FINISHED' Card.  Exit DO WHILE Loop By Branching
C              to Statement 999 in Order to Avoid Reading a ^Z "End of File"
C              Marker That May Be Present For Some Editors.
               GO TO 999

            END IF

         END IF

C        Store the Current Keyword as the Previous Keyword
         PKEYWD = KEYWRD

         GO TO 11
 999     EOF = .TRUE.
 11      CONTINUE
      END DO

C---- Check for PSDCREDIT option and set number of source groups
      IF (PSDCREDIT) THEN
C----    Set number of "source groups" to 2 for PSDCREDIT applications,
C        to account for hardwired 'NAAQS' and 'PSDINC' source groups
         NGRP = 2
      END IF

C --- Determine maximum number of vertices for AREA sources, including
C     AREAPOLY and AREACIRC source types, LINE sources, and OPENPIT sources.
      IF (NAREA .EQ. 0 .AND. NLINE .EQ. 0 .AND. NPIT .EQ. 0) THEN
C        No area, line, or openpit sources, set NVMAX to 0
         NVMAX = 0
      ELSE
         IF (NPOLY .EQ. 0 .AND. NCIRC .EQ. 0) THEN
C           No AREAPOLY or AREACIRC sources; initialize NVMAX to 8
C           for rectangular AREA, LINE, and/or OPENPIT sources
            NVMAX = 8
         ELSE
C           Assign NVMAX to at least 8 to handle rectangular AREA,
C           LINE, and/or OPENPIT sources
            NVMAX = MAX( NVMAX, 8 )
         END IF
      END IF
C     Calculate value of NVMAX2
      NVMAX2 = NVMAX*2

C     Rewind File and Reinitialize Line Number Counter for SETUP
      REWIND INUNIT
      ILINE = 0
      PNETID = '        '

C     Ensure that certain array limits are not < 1.
      NSRC = MAX( NSRC, 1)
      NGRP = MAX( NGRP, 1)
      NREC = MAX( NREC, 1)
C     Set NARC = NREC temporarily for allocating setup arrays
      NARC = NREC
      NAVE = MAX( NAVE, 1)
      NTYP = MAX( NTYP, 1)
      NNET = MAX( NNET, 1)
      IXM  = MAX( IXM , 1)
      IYM  = MAX( IYM , 1)

C     Assign array limits to REAL for calculation of STORE
C     JAT D065 7/22/21 NONE OF THE VARIABLES BELOW ARE USED IN THIS SUBROUTINE
C     THEY ARE ACTUALLY SET IN ALLRESULT
C      RSRC   = REAL(NSRC)
C      RSEC   = REAL(NSEC)
C      RGRP   = REAL(NGRP)
C      RREC   = REAL(NREC)
C      RURB   = REAL(NURB)
C      RARC   = REAL(NARC)
C      RAVE   = REAL(NAVE)
C      RVAL   = REAL(NVAL)
C      RTYP   = REAL(NTYP)
C      RMAX   = REAL(NMAX)
C      RNET   = REAL(NNET)
C      RXM    = REAL(IXM )
C      RYM    = REAL(IYM )
C      REVE   = REAL(NEVE)
C      ROLM   = REAL(NOLM)
C      RPSD   = REAL(NPSD)
C      RQF    = REAL(NQF)
C      RPDMAX = REAL(NPDMAX)
C      RVMAX  = REAL(NVMAX)
C      RPAIR  = REAL(NPAIR)
C      RHIANN = REAL(NHIANN)

      RETURN
      END

      SUBROUTINE PREINCLUD
C***********************************************************************
C*                PREINCLUD Module of the AMS/EPA Regulatory Model - AERMOD
C*
C*       PURPOSE: To read an external receptor/source file using the
C*                INCLUDED keyword.
C*
C*       PROGRAMMER: Roger Brode
C*
C*       DATE:    September 24, 1996
C*
C*       MODIFIED:
C*
C*       INPUTS:
C*
C*       OUTPUTS:
C*
C*
C*       CALLED FROM:   PRESET, SRCSIZ, RECSIZ
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
      MODNAM = 'PREINCLUD'
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

CMGS  Set logical to indicate processing INCLUDED keyword
      L_PREINC = .TRUE.

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

      ELSE IF (IFC .GE. 4) THEN
C        Too Many Parameters
         RETURN
      ELSE
C        No Parameters Specified
         RETURN
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

C        READ Record to Buffers, as A'num' and 'num'A1 where 'num' = ISTRG.
C        Length of ISTRG is Set in PARAMETER Statement in MAIN1
         READ (INCUNT,RDFRM,END=999,ERR=888) RUNST1,
     &                                      (RUNST(I), I = 1, ISTRG)

C        If ILINE=1, reset ILINE temporarily to avoid the
C        check for column shift in subroutine DEFINE
         IF (ILINE .EQ. 1) THEN
            ILINE  = 2
            ITEMPL = 1
         END IF

C        Check for blank input record and cycle
         IF (LEN_TRIM(RUNST1) .EQ. 0) CYCLE

C        Convert Lower Case to Upper Case Letters           ---   CALL LWRUPR
         CALL LWRUPR

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
C           Skip record with NO ECHO during PREINCLUD stage of processing
            CYCLE
         END IF

C        Extract Pathway ID From Field 1                    ---   CALL EXPATH
         CALL EXPATH(FIELD(1),PATHWY,7,NOPATH)

C        For Invalid Pathway and Comment Lines Skip to Next Record
         IF (NOPATH) THEN
C           Skip Error Message for PREINCLUD stage of processing
            PATH = PPATH
            CYCLE
         ELSE IF (PATH .EQ. '**') THEN
            CYCLE
         END IF

C        Extract Keyword From Field 2                       ---   CALL EXKEY
         CALL EXKEY(FIELD(2),NOKEY)

         IF (NOKEY) THEN
C           Invalid Keyword - Skip Error Message for PREINCLUD stage
            PKEYWD = KEYWRD
            CYCLE
         END IF

C        Save Current Path and Path Number as Previous Path and Number
         PPATH = PATH
         IPPNUM = IPNUM

C        Process Input Card Based on Pathway
         IF (PATH .EQ. 'SO') THEN
C           Process SOurce Pathway Cards                    ---   CALL SRCSIZ
            CALL SRCSIZ
         ELSE IF (PATH .EQ. 'RE') THEN
C           Process REceptor Pathway Cards                  ---   CALL RECSIZ
            CALL RECSIZ
         ELSE IF (PATH .EQ. 'EV') THEN
            IF (KEYWRD .EQ. 'EVENTPER') THEN
               NEVE = NEVE + 1
            END IF
         END IF

C        Store the Current Keyword as the Previous Keyword
         PKEYWD = KEYWRD

         GO TO 11
 999     EOF = .TRUE.
         REWIND INCUNT
 11      CONTINUE

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

CMGS  Set logical to indicate no longer processing INCLUDED keyword
      L_PREINC = .FALSE.
      RETURN
      END


      SUBROUTINE SRCSIZ
C***********************************************************************
C                 SRCSIZ Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: To preprocess source inputs to determine
C                 storage requirements
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    September 24, 1996
C
C        MODIFIED:   Added logical (L_PREINC) when processing an INCLUDED file.
C                    When processing an INCLUDED file the MODELOPTs are not
C                    set, so it will not check for BETA when preprocessing
C                    INCLUDED files with RLINE or RLINEXT sources.
C                    Wood, 02/08/2021
C
C        MODIFIED:   Added possibility for RLINE or RLINEXT source type
C                    and check for required use of BETA flag.
C                    Wood, 03/18/2019
C
C        MODIFIED:   Modified to add RLINE source type and check
C                    for required use of BETA flag.
C                    Wood, 07/20/2018
C
C        MODIFIED:   Modified to add counter for number of lines in a
C                    buoyant line source
C                    Amec Foster Wheeler, 06/30/2015
C
C        MODIFIED:   Modified to allow for the use of URBANSRC ALL on
C                    the SO pathway to indicate that all sources are
C                    to be treated as URBAN sources. This option assumes
C                    that only one (1) urban area has been defined using
C                    the CO URBANOPT keyword.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/29/2012
C
C        MODIFIED:   To include options to vary emissions by
C                    hour-of-day, and day-of-week (HRDOW and HRDOW7).
C                    Modified method for determining maximum number
C                    of vertices for AREAPOLY sources for more precise
C                    and efficient memory allocation.  Also included
C                    allocation of arrays for building downwash data.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
C
C        MODIFIED:   To include new options incorporated in version
C                    dated 06341.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG - 12/07/2006
C
C        MODIFIED:   To include options to vary emissions by month,
C                    hour-of-day, and day-of-week (MHRDOW and MHRDOW7).
C                    R.W. Brode, MACTEC (f/k/a PES), Inc., 06/22/05
C
C        MODIFIED:   To include an option to vary emissions by season,
C                    hour-of-day, and day-of-week (SHRDOW).
C                    R.W. Brode, PES, 4/10/2000
C
C        INPUTS:  Pathway (RE) and Keyword
C
C        OUTPUTS: Receptor Arrays
C                 Receptor Setup Status Switches
C
C        CALLED FROM:   PRESET
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      USE RLINE_DATA
      USE BUOYANT_LINE

      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, K, NUM_SRCIDS
      CHARACTER (LEN=12) :: TMPSRCID, SAVESRCID
      ALLOCATABLE :: TMPSRCID(:), SAVESRCID(:)

      SAVE TMPSRCID, SAVESRCID, NUM_SRCIDS

C     Variable Initializations
      MODNAM = 'SRCSIZ'

      IF (KEYWRD .EQ. 'STARTING') THEN
C        Initialize Counters and Set Status Switch
         NSRC = 0
         NGRP = 0
         NOLM = 0
         NPSD = 0
         NBLGRP = 0                              ! (Multiple_BuoyLines_D41_Wood)
         NQF  = 0
         NBF  = 0
         NSEC = 0
         NPIT = 0
         NPNT = 0
         NVOL = 0
         NAREA = 0
         NPOLY = 0
         NCIRC = 0
         NLINE = 0
C ---    Add variable for number of RLINE sources
         NRLINES = 0
C ---    Variable for total # buoyant lines, independent of BL groups
         NBLP = 0
C ---    Add variable for number of urban sources
         NURBSRC = 0
C ---    D113 Added for SIDEWASH
         NSWP = 0

         NVMAX  = 0
         NVTEMP = 0
         NPTEMP = 0
         NPDMAX = 0
         PREVSRCID = '        '
         PREVGRPID = '        '

      ELSE IF (KEYWRD .EQ. 'LOCATION') THEN
         NSRC = NSRC + 1
         IF (FIELD(4)(1:5) .EQ. 'POINT') THEN
            NPNT = NPNT + 1
         ELSE IF (FIELD(4) .EQ. 'VOLUME') THEN
            NVOL = NVOL + 1
         ELSE IF (FIELD(4) .EQ. 'LINE') THEN
            NLINE = NLINE + 1

C        CRT, 3/25/2022, D113 Added for Sidewash source - requires ALPHA
         ELSE IF (FIELD(4) .EQ. 'SWPOINT') THEN
C           Check if required ALPHA flag is present for SWPOINT sources
C           or if processing INCLUDED keyword (MODELOPTs not set yet!)
            IF(L_ALPHA .OR. L_PREINC) THEN
               NSWP = NSWP + 1
            ELSE
               CALL ERRHDL(PATH,MODNAM,'E','198',' SWPOINT')
            END IF
         ELSE IF ((FIELD(4) .EQ. 'RLINE') .OR.
     &              (FIELD(4) .EQ. 'RLINEXT')) THEN
C           Check that the required BETA flag is present for RLINE sources
CMGS        or if processing INCLUDED keyword (MODELOPTs not set yet!)
            IF(BETA .OR. L_PREINC) THEN
               NRLINES = NRLINES + 1
            ELSE
               CALL ERRHDL(PATH,MODNAM,'E','199',' RLINE BETA Required')
            END IF
         ELSE IF (FIELD(4) .EQ. 'BUOYLINE') THEN
C (Multiple_BuoyLines_D41_Wood)
C ---       Increment number of individual buoyant lines (independent of
C             buoyant line groups that define BL sources)
            NBLP = NBLP + 1
         ELSE IF (FIELD(4)(1:4) .EQ. 'AREA') THEN
            NAREA = NAREA + 1
            NVMAX = MAX( NVMAX, 8 )
            IF (FIELD(4) .EQ. 'AREAPOLY') THEN
               NPOLY = NPOLY + 1
               NVMAX = MAX( NVMAX, 8 )
            ELSE IF (FIELD(4) .EQ. 'AREACIRC') THEN
C ---          Increment counter for number of AREACIRC sources
               NCIRC = NCIRC + 1
C ---          Save AREACIRC source IDs in temporary arrays in order
C              to check SRCPARAM keyword inputs for NVERTS parameter.
C              First check allocation status of TMPSRCID array;
C              if allocated, then save TMPSRCID array, deallocate,
C              and reallocate based on current number of sources.
C              The end result of TMPSRCID will be an array with
C ---          only the source IDs for AREACIRC sources.
               IF (ALLOCATED(TMPSRCID)) THEN
                  IF (ALLOCATED(SAVESRCID)) THEN
                     SAVESRCID = TMPSRCID
                     NUM_SRCIDS = SIZE(TMPSRCID)
                     DEALLOCATE (TMPSRCID)
                     ALLOCATE (TMPSRCID(NSRC))
                     TMPSRCID(1:NUM_SRCIDS)  = SAVESRCID
                     TMPSRCID(NUM_SRCIDS+1:) = ' '
                  END IF
               ELSE
                  ALLOCATE (TMPSRCID(NSRC))
                  ALLOCATE (SAVESRCID(NSRC))
                  NUM_SRCIDS = NSRC
               END IF
               TMPSRCID(NSRC) = FIELD(3)
               NUM_SRCIDS = NSRC
            END IF
         ELSE IF (FIELD(4) .EQ. 'OPENPIT') THEN
            NPIT = NPIT + 1
         ELSE
C ---       Invalid SrcTyp
CRT  12/10/2021  Replace placeholder code 'CCC' with new code string
            CALL ERRHDL(PATH,MODNAM,'E','640',FIELD(4))
         END IF

      ELSE IF (KEYWRD .EQ. 'SRCPARAM') THEN

C ---    Check for AREACIRC sources with user-specified NVERTS
         DO I = 1, NSRC
C ---       Exit loop if number of temporary AREACIRC source IDs
C           is less than current loop index (I)
            IF (NUM_SRCIDS .LT. I) EXIT

              IF( FIELD(3) .EQ. TMPSRCID(I) ) THEN
C ---          This is an AREACIRC source: check for NVERTS input

               IF (IFC .GE. 7) THEN

C ---             Set maximum number of vertices based on
C                 user-specified number for AREACIRC source
                  CALL STODBL(FIELD(7),ILEN_FLD,DNUM,IMIT)
C                 Check The Numerical Field
                  IF (IMIT .NE. 1) THEN
                     CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  ELSE
C ---                Adjust NVMAX if needed based on number of
C                    vertices specified for this AREACIRC source +4
                     NVMAX = MAX( NVMAX, IDNINT(DNUM)+4 )
                  END IF
               ELSE
C ---             User did not specify number of vertices for
C                 this AREACIRC source; adjust NVMAX if needed
C                 based on default value of 20 for NVERTS (+4)
                  NVMAX = MAX( NVMAX, 24 )
               END IF
C ---          AREACIRC source ID was found, EXIT loop
               EXIT
            END IF

         END DO

      ELSE IF (KEYWRD .EQ. 'AREAVERT') THEN
C ---    Check for consistency of SRCID's
         IF (FIELD(3)(1:LEN_TRIM(FIELD(3))) .EQ.
     &      PREVSRCID(1:LEN_TRIM(PREVSRCID))) THEN
            NVTEMP = NVTEMP + IFC - 3
C ---       Set NVMAX based on current number of
C           vertices for this source (NVTEMP/2) + NVPOLY
C           (where the NVPOLY PARAMETER is assigned a value of 12)
C           to account for maximum number of sides
C           for transect through source
            NVMAX  = MAX( NVMAX, NVPOLY+NINT(FLOAT(NVTEMP/2)) )
         ELSE
C ---       This is first AREAVERT keyword for this AREAPOLY source.
C           Assign NVTEMP based on number of data fields specified.
            NVTEMP = IFC - 3
            NVMAX  = MAX( NVMAX, NVPOLY+NINT(FLOAT(NVTEMP/2)) )
            PREVSRCID = FIELD(3)
         END IF

      ELSE IF (KEYWRD.EQ.'PARTDIAM') THEN
         IF (FIELD(3) .EQ. PREVSRCID) THEN
            NPTEMP = NPTEMP + IFC - 3
C ---       Set NPDMAX based on current number of
C           particle size categories for this source
            NPDMAX = MAX( NPDMAX, NPTEMP )
         ELSE
            NPTEMP = IFC - 3
            NPDMAX = MAX( NPDMAX, NPTEMP )
            PREVSRCID = FIELD(3)
         END IF

      ELSE IF ((KEYWRD.EQ.'BUILDHGT' .OR.
     &          KEYWRD.EQ.'BUILDWID' .OR.
     &          KEYWRD.EQ.'BUILDLEN')) THEN
         NSEC = 36

      ELSE IF (KEYWRD .EQ. 'METHOD_2') THEN
         NPDMAX = MAX( NPDMAX, 1 )

      ELSE IF (KEYWRD .EQ. 'EMISFACT') THEN
         IF (FIELD(4) .EQ. 'SEASON') THEN
            NQF = MAX( NQF, 4)
         ELSE IF (FIELD(4) .EQ. 'MONTH') THEN
            NQF = MAX( NQF, 12)
         ELSE IF (FIELD(4) .EQ. 'HROFDY') THEN
            NQF = MAX( NQF, 24)
         ELSE IF (FIELD(4) .EQ. 'WSPEED') THEN
            NQF = MAX( NQF, 6)
         ELSE IF (FIELD(4) .EQ. 'SEASHR') THEN
            NQF = MAX( NQF, 96)
         ELSE IF (FIELD(4) .EQ. 'HRDOW') THEN
            NQF = MAX( NQF, 72)
         ELSE IF (FIELD(4) .EQ. 'HRDOW7') THEN
            NQF = MAX( NQF, 168)
         ELSE IF (FIELD(4) .EQ. 'SHRDOW') THEN
            NQF = MAX( NQF, 288)
         ELSE IF (FIELD(4) .EQ. 'SHRDOW7') THEN
            NQF = MAX( NQF, 672)
         ELSE IF (FIELD(4) .EQ. 'MHRDOW') THEN
            NQF = MAX( NQF, 864)
         ELSE IF (FIELD(4) .EQ. 'MHRDOW7') THEN
            NQF = MAX( NQF, 2016)
         END IF

      ELSE IF (KEYWRD .EQ. 'BGSECTOR') THEN
C ---    Assign logical variable for BGSECTORs
         L_BGSector = .TRUE.
C ---    Set maximum array limit for number of BACKGRND sectors
         NUMBGSects = IFC - 2

      ELSE IF (KEYWRD .EQ. 'BACKGRND') THEN
         L_BACKGRND = .TRUE.
C ---    Assign field index to 4 if BGSECTOR is used, otherwise 3
         IF (L_BGSector) THEN
            K = 4
         ELSE
            K = 3
         END IF
         IF (FIELD(K) .EQ. 'ANNUAL') THEN
            NBF = MAX( NBF, 1)
         ELSE IF (FIELD(K) .EQ. 'SEASON') THEN
            NBF = MAX( NBF, 4)
         ELSE IF (FIELD(K) .EQ. 'MONTH') THEN
            NBF = MAX( NBF, 12)
         ELSE IF (FIELD(K) .EQ. 'HROFDY') THEN
            NBF = MAX( NBF, 24)
         ELSE IF (FIELD(K) .EQ. 'WSPEED') THEN
            NBF = MAX( NBF, 6)
         ELSE IF (FIELD(K) .EQ. 'SEASHR') THEN
            NBF = MAX( NBF, 96)
         ELSE IF (FIELD(K) .EQ. 'HRDOW') THEN
            NBF = MAX( NBF, 72)
         ELSE IF (FIELD(K) .EQ. 'HRDOW7') THEN
            NBF = MAX( NBF, 168)
         ELSE IF (FIELD(K) .EQ. 'SHRDOW') THEN
            NBF = MAX( NBF, 288)
         ELSE IF (FIELD(K) .EQ. 'SHRDOW7') THEN
            NBF = MAX( NBF, 672)
         ELSE IF (FIELD(K) .EQ. 'MHRDOW') THEN
            NBF = MAX( NBF, 864)
         ELSE IF (FIELD(K) .EQ. 'MHRDOW7') THEN
            NBF = MAX( NBF, 2016)
         END IF

      ELSE IF (KEYWRD .EQ. 'URBANSRC' .AND.
     &                     IFC .EQ. 3 .AND. FIELD(3) .EQ. 'ALL'
     &                                .AND. L_PRESET_URBAN) THEN
         IF (.NOT. L_MULTURB) THEN
C           Set logical for URBANSRC ALL option (not applicable for
C           multiple urban areas)
            L_URBAN_ALL = .TRUE.
C ---       Assign number of urban sources
            NURBSRC = NSRC
         ELSE
C           Issue ERROR message for URBANSRC ALL option with multiple
C           urban areas
            CALL ERRHDL(PATH,MODNAM,'E','279','URBANSRC ALL')
         END IF

C --- Check for number of urbansrc
      ELSE IF (KEYWRD .EQ. 'URBANSRC' .AND. .NOT.L_MULTURB) THEN
         NURBSRC = NURBSRC + IFC - 2

      ELSE IF (KEYWRD .EQ. 'URBANSRC' .AND. L_MULTURB) THEN
         NURBSRC = NURBSRC + IFC - 3

      ELSE IF (KEYWRD .EQ. 'OLMGROUP') THEN
         IF (NOLM .EQ. 0) PREVGRPID = '        '
         IF (FIELD(3) .NE. PREVGRPID) THEN
            NOLM = NOLM + 1
            PREVGRPID = FIELD(3)
         END IF

      ELSE IF (KEYWRD .EQ. 'PSDGROUP') THEN
         IF (NPSD .EQ. 0) PREVGRPID = '        '
         IF (FIELD(3) .NE. PREVGRPID) THEN
            NPSD = NPSD + 1
            PREVGRPID = FIELD(3)
         END IF

C (Multiple_BuoyLines_D41_Wood)
C     Process BLPGROUP keyword for multiple buoyant lines
      ELSE IF (KEYWRD .EQ. 'BLPINPUT') THEN
C ---    Average buoyant line source average input parameters specified
C ---       Increment number of buoyant line sources)
            NBLAVGINPalloc = NBLAVGINPalloc + 1

C     Process BLPGROUP keyword for multiple buoyant lines
      ELSE IF (KEYWRD .EQ. 'BLPGROUP') THEN
C ---    A buoyant line source group specified
         IF (NBLGRP .EQ. 0) PREVGRPID = '        '
         IF (FIELD(3) .NE. PREVGRPID) THEN
C ---       Increment number of buoyant line sources)
            NBLGRP = NBLGRP + 1
            PREVGRPID = FIELD(3)
         END IF

      ELSE IF (KEYWRD .EQ. 'SRCGROUP') THEN
         IF (NGRP .EQ. 0) PREVGRPID = '        '
         IF (FIELD(3) .NE. PREVGRPID) THEN
            NGRP = NGRP + 1
            PREVGRPID = FIELD(3)
         END IF

      ELSE IF (KEYWRD .EQ. 'INCLUDED') THEN
         CALL PREINCLUD

      ELSE IF (KEYWRD .EQ. 'FINISHED') THEN

! (Multiple_BuoyLines_D41_Wood)
C ---    For legacy input control files that have a buoyant line source
C         and no BLPGROUP keyword(s); NBLGRP used for array allocation
         IF (NBLP .GT. 0 .AND. NBLGRP .EQ. 0) THEN
            NBLGRP = 1
         END IF
         RETURN

      END IF

      RETURN
      END

      SUBROUTINE RECSIZ
C***********************************************************************
C                 RECSIZ Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: To preprocess receptor inputs to determine
C                 storage requirements
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    September 24, 1996
C
C        INPUTS:  Pathway (RE) and Keyword
C
C        OUTPUTS: Receptor Arrays
C                 Receptor Setup Status Switches
C
C        CALLED FROM:   PRESET
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'RECSIZ'

      IF (KEYWRD .EQ. 'STARTING') THEN
C        Initialize Counters and Set Status Switch
         NREC = 0
         NNET = 0
         IXM  = 0
         IYM  = 0
         PXSOID = ' '
         ISTA = .FALSE.
      ELSE IF (KEYWRD .EQ. 'GRIDCART') THEN
C        Process Cartesian Grid Receptor Network            ---   CALL PRECART
         CALL PRECART
      ELSE IF (KEYWRD .EQ. 'GRIDPOLR') THEN
C        Process Polar Receptor Network                     ---   CALL PREPOLR
         CALL PREPOLR
      ELSE IF (KEYWRD .EQ. 'DISCCART') THEN
         NREC = NREC + 1
      ELSE IF (KEYWRD .EQ. 'EVALCART') THEN
         NREC = NREC + 1
      ELSE IF (KEYWRD .EQ. 'DISCPOLR') THEN
         NREC = NREC + 1
      ELSE IF (KEYWRD .EQ. 'INCLUDED') THEN
         CALL PREINCLUD
      ELSE IF (KEYWRD .EQ. 'FINISHED') THEN
         RETURN
      END IF

      RETURN
      END

      SUBROUTINE PRECART
C***********************************************************************
C                 PRECART Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes Cartesian Grid Receptor Network Inputs
C
C        PROGRAMMER:  Roger Brode
C
C        DATE:    September 24, 1996
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Cartesian Grid Receptor Network Inputs
C
C        CALLED FROM:   RECSIZ
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'PRECART'

C     READ in the Netid and Nettype
      IF (IFC .LT. 3) THEN
C        Missing Data Field
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
         NNET = NNET + 1
      ELSE
C        Invalid Secondary Keyword
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
      ELSE IF (KTYPE .EQ. 'XYINC') THEN
C        Set the Uniform Spacing Receptor Network           ---   CALL PREGENCAR
         CALL PREGENCAR
      ELSE IF (KTYPE.EQ.'XPNTS' .OR. KTYPE.EQ.'YPNTS') THEN
C        Set the Non-uniform Spacing Receptor Network       ---   CALL PREXYPNTS
         CALL PREXYPNTS
      ELSE IF (KTYPE .EQ. 'END') THEN
         IEND = .TRUE.
         IF (.NOT. RECERR) THEN
            NREC = NREC + ICOUNT*JCOUNT
         END IF
         ISTA = .FALSE.
         NEWID = .TRUE.

      ELSE IF (KTYPE.NE.'ELEV' .AND. KTYPE.NE.'FLAG' .AND.
     &         KTYPE.NE.'HILL') THEN
C        Invalid Secondary Keyword
         RECERR = .TRUE.
         GO TO 999

      END IF

      PNETID = NETIDT

 999  RETURN
      END

      SUBROUTINE PREGENCAR
C***********************************************************************
C                 PREGENCAR Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Generates Cartesian Grid Receptor Network With
C                 Uniform Spacing
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    September 24, 1996
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Cartesian Grid Receptor Network With Uniform
C                 Spacing
C
C        CALLED FROM:   PRECART
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, K
      REAL    :: TEMPR(6)
C     JAT 7/22/21 D065 XDELTA AND YDELTA NOT USED
C      DOUBLE PRECISION :: TEMPD(6), XDELTA, YDELTA
      DOUBLE PRECISION :: TEMPD(6)
      LOGICAL ERROR

C     Variable Initializations
      MODNAM = 'PREGENCAR'
      ERROR = .FALSE.

C     Check for Location of Secondary Keyword, XYINC
      DO I = 1, IFC
         IF (FIELD(I) .EQ. 'XYINC') THEN
            ISC = I + 1
         END IF
      END DO

C     Determine Whether There Are Enough Parameter Fields
      IF (IFC .EQ. ISC-1) THEN
C        Missing Parameter
         RECERR = .TRUE.
         GO TO 999
      ELSE IF (IFC .GT. ISC+5) THEN
C        Too Many Parameters
         RECERR = .TRUE.
         GO TO 999
      ELSE IF (IFC .LT. ISC+5) THEN
C        Too Few Parameters
         RECERR = .TRUE.
         GO TO 999
      END IF

C     Input The Numerical Values
      DO K = 1,6
         IF (K .EQ. 2 .OR. K .EQ. 5) THEN
            CALL STONUM(FIELD(ISC + K-1),ILEN_FLD,TEMPR(K),IMIT)
C           Check The Numerical Field
            IF (IMIT .EQ. -1) THEN
               ERROR = .TRUE.
               RECERR = .TRUE.
            END IF
         ELSE
            CALL STODBL(FIELD(ISC + K-1),ILEN_FLD,TEMPD(K),IMIT)
C           Check The Numerical Field
            IF (IMIT .EQ. -1) THEN
               ERROR = .TRUE.
               RECERR = .TRUE.
            END IF
         END IF
      END DO

      IF (ERROR) THEN
         ERROR = .FALSE.
         GO TO 999
      END IF

C     Assign Values to Appropriate Variables for Generated Network
      XINT   = TEMPD(1)
      ICOUNT = NINT(TEMPR(2))
C     JAT 7/22/21 D065 XDELTA NOT USED
C      XDELTA = TEMPD(3)
      YINT   = TEMPD(4)
      JCOUNT = NINT(TEMPR(5))
C     JAT 7/22/21 D065 YDELTA NOT USED
C      YDELTA = TEMPD(6)

C     Assign Them to the Coordinate Arrays
      IF (ICOUNT .GT. IXM) THEN
         IXM = ICOUNT
      END IF
      IF (JCOUNT .GT. IYM) THEN
         IYM = JCOUNT
      END IF

 999  RETURN
      END

      SUBROUTINE PREXYPNTS
C***********************************************************************
C                 PREXYPNTS Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes Cartesian Grid x,y Input Value
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    September 24, 1996
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Cartesian Grid x,y Input Value
C
C        CALLED FROM:   PRECART
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, JSET

C     Variable Initializations
      MODNAM = 'PREXYPNTS'

      IF (KTYPE .EQ. 'XPNTS') THEN
C        Check for Location of Secondary Keyword, XPNTS
         DO I = 1, IFC
            IF (FIELD(I) .EQ. 'XPNTS') THEN
               ISC = I + 1
            END IF
         END DO

C        Determine Whether There Are Enough Parameter Fields
         IF (IFC .EQ. ISC-1) THEN
C           Missing Parameter
            RECERR = .TRUE.
            GO TO 999
         END IF

         ISET = ICOUNT
         DO I = ISC, IFC
            CALL STONUM(FIELD(I),ILEN_FLD,FNUM,IMIT)
C           Check The Numerical Field
            IF (IMIT .EQ. -1) THEN
               RECERR = .TRUE.
            END IF
            ISET = ISET + 1
            IF (ISET .GT. IXM) THEN
               IXM = ISET
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
C           Missing Parameter
            RECERR = .TRUE.
            GO TO 999
         END IF

         JSET = JCOUNT
         DO I = ISC, IFC
            CALL STONUM(FIELD(I),ILEN_FLD,FNUM,IMIT)
C           Check The Numerical Field
            IF (IMIT .EQ. -1) THEN
               RECERR = .TRUE.
            END IF
            JSET = JSET + 1
            IF (JSET .GT. IYM) THEN
               IYM = JSET
            END IF
         END DO
         JCOUNT = JSET

      END IF

 999  RETURN
      END

      SUBROUTINE PREPOLR
C***********************************************************************
C                 PREPOLR Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes Polar Grid Receptor Network Inputs
C
C        PROGRAMMER:  Roger Brode
C
C        DATE:    September 24, 1996
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Polar Receptor Network Inputs
C
C        CALLED FROM:   RECSIZ
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'PREPOLR'

      IF (IFC .LT. 3) THEN
C        Missing Data Field
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
         NNET = NNET + 1
      ELSE
C        Invalid Secondary Keyword
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
      ELSE IF (KTYPE .EQ. 'DIST') THEN
C        Read in the Distance Set                           ---   CALL PREPOLDST
         CALL PREPOLDST
      ELSE IF (KTYPE .EQ. 'GDIR') THEN
         CALL PREGENPOL
      ELSE IF (KTYPE .EQ. 'DDIR') THEN
         CALL PRERADRNG
      ELSE IF (KTYPE .EQ. 'END') THEN
         IEND = .TRUE.
C        Get the Final Result
         IF (.NOT. RECERR) THEN
            NREC = NREC + ICOUNT*JCOUNT
         END IF
         ISTA = .FALSE.
         NEWID = .TRUE.

      ELSE IF (KTYPE.NE.'ELEV' .AND. KTYPE.NE.'FLAG' .AND.
     &         KTYPE.NE.'HILL' .AND. KTYPE.NE.'ORIG') THEN
C        Invalid Secondary Keyword
         RECERR = .TRUE.
         GO TO 999

      END IF

      PNETID = NETIDT

 999  RETURN
      END

      SUBROUTINE PREPOLDST
C***********************************************************************
C                 PREPOLDST Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Gets Distances for the Polar Network
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    September 24, 1996
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Polar Network Distance Input Value
C
C        CALLED FROM:   PREPOLR
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I

C     Variable Initializations
      MODNAM = 'PREPOLDST'

C     Skip the Unrelated Fields
      DO I = 1, IFC
         IF (FIELD(I) .EQ. 'DIST') THEN
            ISC = I + 1
         END IF
      END DO

C     Determine Whether There Are Enough Parameter Fields
      IF (IFC .EQ. ISC-1) THEN
C        Missing Parameter
         RECERR = .TRUE.
         GO TO 999
      END IF

      ISET = ICOUNT

      DO I = ISC, IFC
         CALL STONUM(FIELD(I),ILEN_FLD,FNUM,IMIT)
C        Check The Numerical Field
         IF (IMIT .EQ. -1) THEN
            RECERR = .TRUE.
         END IF
         ISET = ISET + 1
         IF (ISET .GT. IXM) THEN
            IXM = ISET
         END IF
      END DO

      ICOUNT = ISET

 999  RETURN
      END

      SUBROUTINE PREGENPOL
C***********************************************************************
C                 PREGENPOL Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Generates Polar Receptor Network With
C                 Uniform Spacing
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    September 24, 1996
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Polar Receptor Network With Uniform Direction Spacing
C
C        CALLED FROM:   PREPOLR
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, K
C     JAT 7/22/21 D065 DIRINI AND DIRINC NOT USED
C      DOUBLE PRECISION :: TEMPP(3), DIRINI, DIRINC
      DOUBLE PRECISION :: TEMPP(3)
      LOGICAL ERROR

C     Variable Initializations
      MODNAM = 'PREGENPOL'
      ERROR = .FALSE.

C     Check for the Location of the Secondary Keyword, GDIR
      DO I = 1, IFC
         IF (FIELD(I) .EQ. 'GDIR') THEN
            ISC = I + 1
         END IF
      END DO

C     Determine Whether There Are Enough Parameter Fields
      IF (IFC .EQ. ISC-1) THEN
C        Missing Parameter
         RECERR = .TRUE.
         GO TO 999
      ELSE IF (IFC .LT. ISC+2) THEN
C        Not Enough Parameters
         RECERR = .TRUE.
         GO TO 999
      ELSE IF (IFC .GT. ISC+2) THEN
C        Too Many Parameters
         RECERR = .TRUE.
         GO TO 999
      END IF

C     Input Numerical Values
      DO K = 1, 3
         CALL STODBL(FIELD(ISC + K-1),ILEN_FLD,TEMPP(K),IMIT)
C        Check The Numerical Field
         IF (IMIT .EQ. -1) THEN
            RECERR = .TRUE.
            ERROR = .TRUE.
         END IF
      END DO

      IF (ERROR) THEN
         ERROR = .FALSE.
         GO TO 999
      END IF

      JCOUNT = IDNINT(TEMPP(1))
C     JAT 7/22/21 D065 DIRINI AND DIRINC NOT USED
C      DIRINI = TEMPP(2)
C      DIRINC = TEMPP(3)

C     Assign Them to the Coordinate Arrays
      IF (JCOUNT .GT. IYM) THEN
         IYM = JCOUNT
      END IF

 999  RETURN
      END

      SUBROUTINE PRERADRNG
C***********************************************************************
C                 PRERADRNG Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes Non-Uniform Polar Network Value
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    September 24, 1996
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Polar Network Directions in Non-Uniform Spacing
C
C        CALLED FROM:   PREPOLR
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I

C     Variable Initializations
      MODNAM = 'PRERADRNG'

C     Skip the non-useful Fields
      DO I = 1, IFC
         IF (FIELD(I) .EQ. 'DDIR') THEN
            ISC = I + 1
         END IF
      END DO

C     Determine Whether There Are Enough Parameter Fields
      IF (IFC .EQ. ISC-1) THEN
C        Error Message: Missing Parameter
         RECERR = .TRUE.
         GO TO 999
      END IF

      ISET = JCOUNT

      DO I = ISC, IFC
         CALL STONUM(FIELD(I),ILEN_FLD,FNUM,IMIT)
C        Check The Numerical Field
         IF (IMIT .EQ. -1) THEN
            RECERR = .TRUE.
         END IF
         ISET = ISET + 1
         IF (ISET .GT. IYM) THEN
            IYM = ISET
         END IF
      END DO

      JCOUNT = ISET

 999  RETURN
      END

      SUBROUTINE SET_WINDOW
C***********************************************************************
C                 SET_WINDOW Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Preprocess Meteorology Surface Data Card (SURFDATA)
C                 to Set Date Window for Y2K Fixes
C
C        PROGRAMMER: Roger Brode, PES, Inc.
C
C        DATE:    April 29, 1999
C
C        MODIFICATIONS:
C
C                    To subtract 1 from ISTRT_WIND in case data file
C                    contains data from end of previous year.
C                    R.W. Brode, PES, Inc.  8/28/01
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Starting Century, ISTRT_CENT                    [I4]
C                 Starting Year for 2-digit Window, ISTRT_WIND    [I4]
C
C        ERROR HANDLING:   Checks for Too Few Parameters;
C                          Checks for Invalid Numeric Fields;
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   PRESET
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'SET_WINDOW'

      IF (IFC .LT. 4) THEN
         GO TO 999
      ELSE IF (IFC .GT. 7) THEN
         GO TO 999
      END IF

      CALL STONUM(FIELD(4),ILEN_FLD,FNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         GO TO 999
      END IF
      ISYEAR = NINT(FNUM)
      IF (ISYEAR .LT. 100) THEN
C        Write warning message for 2-digit year, and set default "windowing"
C        variables, ISTRT_CENT (=19) and ISTRT_WIND (=50).
         IF (.NOT. L_SkipMessages) THEN
            CALL ERRHDL(PATH,MODNAM,'W','360',KEYWRD)
         END IF
         ISTRT_CENT = 19
         ISTRT_WIND = 50
      ELSE
C        Determine starting century (ISTRT_CENT) and starting year for
C        window (ISTRT_WIND) from 4-digit input
         ISTRT_CENT = ISYEAR/100
         ISTRT_WIND = ISYEAR - ISTRT_CENT*100
C        Subtract 1 from ISTRT_WIND in case data file contains data
C        from end of previous year
         ISTRT_WIND = ISTRT_WIND - 1
         IF (ISTRT_WIND .LT. 0) THEN
            ISTRT_WIND = 0
         END IF
C        Check for year .ge. 2148 to avoid integer overflow on FULLDATE
         IF (ISTRT_CENT .GE. 21 .AND. ISTRT_WIND .GE. 48) THEN
            CALL ERRHDL(PATH,MODNAM,'E','365',KEYWRD)
            ISTRT_CENT = 21
            ISTRT_WIND = 47
         END IF
      END IF

      GO TO 1000

 999  CONTINUE
C     For error in processing assume 1900 for start century and 50 for window
      ISTRT_CENT = 19
      ISTRT_WIND = 50

 1000 RETURN
      END

      SUBROUTINE CHK_ENDYR
C***********************************************************************
C                 CHK_ENDYR Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Checks date for "end-of-year" for use in ANNUAL
C                 averages and PM-2.5 processing.
C
C        PROGRAMMER: Roger Brode
C
C        DATE:
C
C        MODIFIED:   To allow user-specified rank for PM2.5 processing
C                    to accommodate latest guidance for PM2.5 modeling.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
C
C                    To check for allocation status prior to
C                    initializing allocatable arrays, and include
C                    maximum annual value arrays.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Plant Boundary Receptor Location Inputs
C
C        CALLED FROM:   HRLOOP
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: IEND_DAY, N

C     Variable Initializations
      MODNAM = 'CHK_ENDYR'

      IF( (IENDMN.EQ.2.AND.IENDDY.EQ.29.AND.IMONTH.EQ.2) .AND.
     &    (MOD(IYR,4).NE.0) .OR.
     &    (MOD(IYR,100).EQ.0 .AND. MOD(IYR,400).NE.0)) THEN
C        Set End Day to 28 for non-leap year February
         IEND_DAY = 28
      ELSE
         IEND_DAY = IENDDY
      END IF

      IF (IMONTH.EQ.IENDMN .AND. IDAY.EQ.IEND_DAY .AND.
     &    IHOUR.EQ.IENDHOUR) THEN
C        End of year reached, increment counter and store High-N-High (HNH) values
         NUMYRS = NUMYRS + 1
C        Reset hour counter for MAXDCONT met data arrays
         IHR_NDX = 0
         IF (ANNUAL) THEN
C ---       Calculate ANNUAL averages
            CALL PERAVE
C ---       Check for ANNUAL POSTFILE
            IF( ANPOST )THEN
               CALL PSTANN
            ENDIF
C ---       Sum the annual averages
            SUMANN(:,:,:) = SUMANN(:,:,:) + ANNVAL(:,:,:)
C           Re-initialize the annual counters and array
            IANHRS  = 0
            IANCLM  = 0
            IANMSG  = 0
            NSKIPTOT = 0
            IF (ALLOCATED(ANNVAL))  ANNVAL  = 0.0D0
            IF (ALLOCATED(AMXVAL))  AMXVAL  = 0.0D0
            IF (ALLOCATED(IMXLOC))  IMXLOC  = 0
         END IF
         IF ((PM25AVE .OR. NO2AVE .OR. SO2AVE) .AND. NUMAVE.GE.1) THEN
C ---       Sum the High-N-High 24-hour values for PM-2.5,
C           or High-N-High 1-hour values for NO2
            DO N = 1, NVAL
               SUMHNH(1:NUMREC,1:NUMGRP,N) =
     &         SUMHNH(1:NUMREC,1:NUMGRP,N) +
     &            HIMXDLY(1:NUMREC,1:NUMGRP,N)
               IF (NUMYRS .LE. NYEARS) THEN
                  HIMXDLY_BYYR(1:NUMREC,1:NUMGRP,N,NUMYRS) =
     &                 HIMXDLY(1:NUMREC,1:NUMGRP,N)
                  NHIDATMXD_BYYR(1:NUMREC,1:NUMGRP,N,NUMYRS) =
     &                 NHIDATMXD(1:NUMREC,1:NUMGRP,N)
               ELSE
C ---             Write Error Message        ! Too many years
                  WRITE(DUMMY,'(''NYR='',I4)') NYEARS
                  CALL ERRHDL(PATH,MODNAM,'E','482',DUMMY)
                  RUNERR = .TRUE.
               END IF

               IF (MXDAILY_BYYR .AND. NHIAVE(N,1) .EQ. 1) THEN
                  CALL MXDYBYYR(N)
               ENDIF
            END DO
C           Re-initialize the MAXDAILY Value Arrays used for
C           PM25/NO2/SO2 Processing averaged across years
            IF (ALLOCATED(HIMXDLY))   HIMXDLY   = 0.0D0
            IF (ALLOCATED(NHIDATMXD)) NHIDATMXD = 0
         END IF
         NREMAIN = 0
      ELSE
C        Increment counter for number of hours remaining after
C        the end of the last year
         NREMAIN = NREMAIN + 1
      END IF

      RETURN
      END

      SUBROUTINE MAXDCONT_LOOP
C***********************************************************************
C                MAXDCONT_LOOP Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE:    Control "post-processing" for OU MAXDCONT option
C
C        PROGRAMMER: Roger W. Brode
C
C        DATE:       February 28, 2011
C
C        MODIFIED:   Modified to add 'saved' arrays for buoyant line source
C                    Amec Foster Wheeler, 06/30/2015
C
C        MODIFIED:   Modified subroutine MAXDCONT_LOOP to correct problems
C                    with the MAXDCONT option for applications that vary
C                    emissions (EMISFACT), background ozone data (O3VALUES),
C                    or background concentrations (BACKGRND) with a day-of-week
C                    component, e.g., SHRDOW or SHRDOW7, etc. Also modified
C                    subroutine MAXDCONT_LOOP to include checks on the
C                    consistency between results in the SUMVAL_MAXD and
C                    SUMHNH arrays for the "target" source group under
C                    the MAXDCONT option.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/29/2012
C
C        INPUTS:
C
C        OUTPUTS:
C
C        CALLED FROM:   MAIN
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      USE BUOYANT_LINE
      IMPLICIT NONE
      CHARACTER MODNAM*12
      INTEGER I

C JAT 06/22/21 D065
C REMOVE NDAY AS UNUSED VARIABLE
C      INTEGER :: NDAY(12)
      INTEGER :: IJDY
      INTEGER :: ISDATE_SAV, IEDATE_SAV
      INTEGER :: JGRP, IVAL, IR, IG
      INTEGER :: ICYR, ICYR2, IMN, IDY, IHR, ICJDAY, IPJDAY
      LOGICAL :: HIT_THRESH
C Unused: INTEGER :: INYR, INMN, INDY, JDY

C     Variable Initializations
C JAT 06/22/21 DO65
C SINCE NDAY HAS BEEN REMOVED, COMMENT OUT INITIALIZATION OF NDAY
C      DATA NDAY/0,31,59,90,120,151,181,212,243,273,304,334/

C     Variable Initializations

      MODNAM = 'MAXDCONT_LOOP'
      PATH   = 'CN'

      HIT_THRESH = .FALSE.

C --- Reinitialize NUMHRS, NUMCLM and NUMMSG
      NUMHRS(:) = 0
      NUMCLM(:) = 0
      NUMMSG(:) = 0
C     Initialize __VAL arrays (1:NUMTYP)
      HRVAL   = 0.0D0
      AERVAL  = 0.0D0
      AVEVAL  = 0.0D0

C     JAT 9/20/18 added from 18081
C     move NUMREC=1 to here from below
C --- Reset number of receptors (NUMREC) to 1
C     for use in max daily contribution analyses
      NUMREC = 1
      IF (ALLOCATED(BACKAVE)) BACKAVE(:) = 0.0D0

C     JAT 9/20/18
C     Deallocate CHI and reallocate to use NUMREC
c     original CHI allocation is CHI(NREC,NSRC,NUMTYP)
C     for this subroutine it is NUMREC, NSRC,NUMTYP
C     this is to correct problem with MAXCHI and CHI
C     in subroutine PVMRM_CALC during MAXDCONT processing
c     normal processing of PVMRM_CALC during routine AERMOD run
c     is unaffected
      IF (ALLOCATED(CHI)) THEN
          DEALLOCATE(CHI)
          ALLOCATE(CHI(NUMREC,NSRC,NUMTYP))
          CHI(:,:,:) = 0.0D0
      ENDIF
      IF(GRSM)THEN
         CHI_TTRAVPLM = 0.0D0
         CHI_TTRAVPAN = 0.0D0
         CHI_TTRAVAER = 0.0D0
         CHI_TTRAVPRM = 0.0D0
         DEALLOCATE(CHI_TTRAVCHM)
         ALLOCATE(CHI_TTRAVCHM(NUMREC,NSRC))
         CHI_TTRAVCHM(:,:) = 0.0D0
      END IF

      IF(RUNTTRM2)THEN
         DEALLOCATE(TTRMCOMPARE)
         ALLOCATE(TTRMCOMPARE(NGRP,NSRC,NUMREC,NUMTYP))
         TTRMCOMPARE(:,:,:,:) = 0.0D0
      END IF

C --- Copy standard receptor arrays to "saved" arrays
      AXR_SAV = AXR
      AYR_SAV = AYR
      AZELEV_SAV = AZELEV
      AZHILL_SAV = AZHILL
      AZFLAG_SAV = AZFLAG

C     For buoyant line sources - save results of first tranlation/rotation
      IF (L_BLSOURCE) THEN
         XR_SCS_SAV = XR_SCS
         YR_SCS_SAV = YR_SCS
         BL_RFLAG_SAV = BL_RFLAG
      END IF

C --- Save original start and end dates (ISDATE and IEDATE)
C     for processing messages in TERRST; these variables
C     include 4-digit year (for comparisons to FULLDATE)
      ISDATE_SAV = ISDATE
      IEDATE_SAV = IEDATE


C --- Initialized SUMVAL_MAXD array for max daily contributions
      SUMVAL_MAXD = 0.0D0

C --- Set logical flag to skip messages while re-processing
C     the meteorological, BACKGRND, and ozone data
      L_SkipMessages = .TRUE.

C --- Loop through "target" source groups for
C     max daily contributions option (OU MAXDCONT)
      DO JGRP = 1, NUMGRP

         IF (MAXDCONT(JGRP) .GT. 0) THEN
C ---       Max daily contribution results selected
C           for this source group

C ---       Loop through user-specified ranks for max daily
C           contribution analysis for this source group
            DO IVAL = MXD_RANK(JGRP,1), MXD_RANK(JGRP,2)

C ---          Write message to screen indicating current Rank
               WRITE(*,909) GRPID(JGRP), IVAL
 909           FORMAT
     &('+','Now Processing MAXDCONT for Group ',A8,' and Rank No. ',I4)

               DO IR = 1, NREC
C ---             Loop through receptors; but skip receptor if
C                 value is below user-specified threshold
                  IF (SUMHNH(IR,JGRP,IVAL) .LE. 0.0D0 .OR.
     &                SUMHNH(IR,JGRP,IVAL) .LT. MAXD_THRESH(JGRP)) CYCLE

C                  IF (BL_RFLAG(IR) .EQ. .true.) CYCLE

C ---             Assign data from the "saved" arrays
C                 to array index 1 for standard arrays
                  AXR(1) = AXR_SAV(IR)
                  AYR(1) = AYR_SAV(IR)
                  AZELEV(1) = AZELEV_SAV(IR)
                  AZHILL(1) = AZHILL_SAV(IR)
                  AZFLAG(1) = AZFLAG_SAV(IR)
C (Multiple_BuoyLines_D41_Wood)
C                 Added a second dimension to represent BL groups
                  IF (L_BLSOURCE) THEN
                     XR_SCS(1,1:NUMBLGRPS) = XR_SCS_SAV(IR,1:NUMBLGRPS)
                     YR_SCS(1,1:NUMBLGRPS) = YR_SCS_SAV(IR,1:NUMBLGRPS)
                     BL_RFLAG(1,1:NUMBLGRPS) =
     &                                   BL_RFLAG_SAV(IR,1:NUMBLGRPS)
                  END IF
C ---             Loop through number of years processed for
C                 max daily contribution analysis
                  DO I = 1, NUMYRS
C ---                Assign "start date" and "end date" for
C                    max daily 1-hr value associated with
C                    this rank
                     IEDATE = NHIDATMXD_BYYR(IR,JGRP,IVAL,I)
C                    Convert start date from 8-digits to 10-digits
                     ICYR2  = IEDATE/1000000
                     IF (ICYR2 .GE. ISTRT_WIND .AND.
     &                                    ICYR2 .LE. 99) THEN
                        ICYR   = ISTRT_CENT*100 + ICYR2
                        IEDATE = ISTRT_CENT*100000000 +
     &                           NHIDATMXD_BYYR(IR,JGRP,IVAL,I)
                     ELSE IF (ICYR2 .LT. ISTRT_WIND) THEN
                        ICYR   = (ISTRT_CENT+1)*100 + ICYR2
                        IEDATE = (ISTRT_CENT+1)*100000000 +
     &                           NHIDATMXD_BYYR(IR,JGRP,IVAL,I)
                     END IF

                     IF (NO2AVE .OR. SO2AVE) THEN
C ---                   Assign start date to end date for NO2 or SO2
C                       since we're only processing 1 hour at a time
                        ISDATE = IEDATE
                     ELSE IF (PM25AVE) THEN
C ---                   Subtract 23 from end date for PM2.5 since
C                       these will always be 24-hour averages
                        ISDATE = IEDATE - 23
                     END IF

                     DO FULLDATE = ISDATE, IEDATE
C ---                   Calculate IHR_NDX, hour-of-year array index
C                       to extract met data for MAXDCONT option,
C                       based on FULLDATE.
                        IMN = (FULLDATE/10000) -
     &                        (FULLDATE/1000000)*100
                        IDY = (FULLDATE/100) -
     &                        (FULLDATE/10000)*100
                        IHR =  FULLDATE - (FULLDATE/100)*100
                        CALL JULIAN(ICYR,IMN,IDY,IJDY)

C ---                   Determine julian day for current year based on
C                       orignial start month/day
                        CALL JULIAN(ICYR,ISMN,ISDY,ICJDAY)

C ---                   Check for invalid Julian days (=0), indicating a
C                       potential problem with the NHIDATMXD_BYYR array.
C                       An error message will already have been issued in
C                       subroutine Julian, but exit loop to avoid Fortran
C                       runtime errors since IHR_NDX may not be valid.
                        IF (IJDY .EQ. 0 .OR. ICJDAY .EQ. 0) THEN
                           EXIT
                        END IF

                        IF (ISJDAY .EQ. 1) THEN
C ---                      Data starts on Jan. 1
                           IHR_NDX = 24*(IJDY-ISJDAY)+(IHR-ISHR)+1
                        ELSE IF (IJDY .GE. ICJDAY) THEN
C ---                      Data does not start on Jan. 1, but "event"
C                          jday is .ge. start jday
                           IHR_NDX = 24*(IJDY-ICJDAY)+(IHR-ISHR)+1
                        ELSE
C ---                      Data does not start on Jan. 1, and "event"
C                          jday is .lt. start jday; calculation of the
C                          "hour-of-year" index must account for potential
C                          influence of leap year.
C ---                      Determine julian day for previous year based on
C                          start month/day
                           CALL JULIAN(ICYR-1,ISMN,ISDY,IPJDAY)
                           IF (IPJDAY .GT. ICJDAY) THEN
C ---                        Account for leap year
                             IHR_NDX = 24*(IJDY-IPJDAY+366)+(IHR-ISHR)+1
                           ELSE
C ---                        No leap year adjustment needed
                             IHR_NDX = 24*(IJDY-IPJDAY+365)+(IHR-ISHR)+1
                           END IF
                        END IF
C ---                   Assign year arrary index for extracting met data
                        IYR_NDX = I

C ---                   Set other global date variables for this hour
                        IMONTH = IMN
                        IDAY   = IDY
                        IHOUR  = IHR
                        IYEAR  = ICYR2    ! 2-digit year
                        IYR    = ICYR     ! 4-digit year
                        KURDAT = ICYR2*1000000 + IMONTH*10000 +
     &                           IDAY*100 + IHOUR
                        KURPFL = KURDAT

C ---                   Call MAXDCALC subroutine to calculate
C                       max daily contributions for this hour
                        CALL MAXDCALC

                     END DO

                     DO IG = 1, NUMGRP
C ---                   Loop through source groups to determine
C                       contributions to target source group
                        SUMVAL_MAXD(IVAL,IG,JGRP,IR) =
     &                  SUMVAL_MAXD(IVAL,IG,JGRP,IR) +
     &                              AVEVAL(1,IG,1,1)
                     END DO

C ---                Reinitialize AVEVAL array
                     AVEVAL(:,:,:,:) = 0.0D0
C ---                Reinitialize NUMHRS, NUMCLM and NUMMSG
                     NUMHRS(:) = 0
                     NUMCLM(:) = 0
                     NUMMSG(:) = 0
C                    Initialize __VAL arrays (1:NUMTYP)
                     HRVAL   = 0.0D0
                     AERVAL  = 0.0D0
C
                     IF (ALLOCATED(CHI)) CHI(:,:,:) = 0.0D0
                     IF(GRSM)THEN
                       CHI_TTRAVPLM = 0.0D0
                       CHI_TTRAVPAN = 0.0D0
                       CHI_TTRAVAER = 0.0D0
                       CHI_TTRAVPRM = 0.0D0
                       CHI_TTRAVCHM(:,:) = 0.0D0
                     END IF
                     IF(RUNTTRM2)THEN
                       TTRMCOMPARE(:,:,:,:) = 0.0D0
                     ENDIF

                  END DO  ! end loop on years

                  DO IG = 1, NUMGRP
C ---                Divide sumval_maxd results by number of years
C                    to get averages across number of years modeled
                     SUMVAL_MAXD(IVAL,IG,JGRP,IR) =
     &               SUMVAL_MAXD(IVAL,IG,JGRP,IR)/
     &                               DBLE(NUMYRS)
                  END DO

C ---             Check for consistency of SUMVAL_MAXD and SUMHNH arrays for
C                 "target" source group under MAXDCONT option
                  IF (SUMHNH(IR,JGRP,IVAL) .GT. 0.0D0) THEN
                     IF( DABS( (SUMVAL_MAXD(IVAL,JGRP,JGRP,IR) -
     &                          SUMHNH(IR,JGRP,IVAL)) )/
     &                          SUMHNH(IR,JGRP,IVAL) .GT. 5.0D-6) THEN
C ---                   Arrays don't match; issue warning message indicating
C                       potential coding error. A warning to the default
C                       output unit will also be issued at the end of the run.
                        WRITE(DUMMY,'("G",I3.3,"R",I2.2,"R",I4.4)')
     &                        MIN(JGRP,999), MIN(IVAL,99), MIN(IR,9999)
                        CALL ERRHDL(PATH,MODNAM,'W','498',DUMMY)
                        L_MAXDCONT_OrigConc_Warning = .TRUE.
                     END IF
                  END IF
C
               END DO   ! end loop over receptors

C ---          Call subroutine to write MAXDCONT output file
C              for this source group and rank
               CALL MAXDCNT_FILE(JGRP,IVAL)

C ---          Check for value below threshold for MAXDCONT
               IF (MAXVAL(SUMHNH(1:NREC,JGRP,IVAL)) .LT.
     &                                  MAXD_THRESH(JGRP)) THEN
C ---             All values for this rank are below threshold;
C                 reset upper bound of ranks to process, set flag
C                 to indicate that threshold was reached, and
C                 EXIT the loop over ranks
                  MXD_RANK(JGRP,2) = IVAL
                  HIT_THRESH = .TRUE.
                  EXIT
               END IF

            END DO   ! end loop over ranks

C ---       Check for whether MAXD_THRESH specified by user was
C           not reached
            IF (MAXD_THRESH(JGRP) .GT. 0.0D0 .AND.
     &                                    .NOT.HIT_THRESH) THEN
C ---          User-specified threshold was not reached within the
C              range of ranks analyzed, based on the RECTABLE keyword;
C              issue warning message
               WRITE(DUMMY,'(''GRP '',A8)') GRPID(JGRP)
               CALL ERRHDL(PATH,MODNAM,'W','415',DUMMY)
            END IF
C ---       Reset HIT_THRESH flag for next source group
            HIT_THRESH = .FALSE.

         END IF

      END DO    ! end loop over source groups

C --- Copy saved receptor arrays to standard arrays
      AXR = AXR_SAV
      AYR = AYR_SAV
      AZELEV = AZELEV_SAV
      AZHILL = AZHILL_SAV
      AZFLAG = AZFLAG_SAV

      IF (L_BLSOURCE) THEN
         XR_SCS = XR_SCS_SAV
         YR_SCS = YR_SCS_SAV
         BL_RFLAG = BL_RFLAG_SAV
      END IF

C --- Reset number of receptors
      NUMREC = NREC
C --- Reset original start and end dates
      ISDATE = ISDATE_SAV
      IEDATE = IEDATE_SAV

C     JAT 9/20/18 added from 18081
C     reset CHI to original dimensions
      IF (ALLOCATED(CHI)) THEN
          DEALLOCATE(CHI)
          ALLOCATE(CHI(NREC,NSRC,NUMTYP))
          CHI(:,:,:) = 0.0D0
      ENDIF
      IF(GRSM)THEN
        CHI_TTRAVPLM = 0.0D0
        CHI_TTRAVPAN = 0.0D0
        CHI_TTRAVAER = 0.0D0
        CHI_TTRAVPRM = 0.0D0
        DEALLOCATE(CHI_TTRAVCHM)
        ALLOCATE(CHI_TTRAVCHM(NREC,NSRC))
        CHI_TTRAVCHM(:,:) = 0.0D0
      END IF

      IF(RUNTTRM2)THEN
         DEALLOCATE(TTRMCOMPARE)
         ALLOCATE(TTRMCOMPARE(NGRP,NSRC,NREC,NREC))
         TTRMCOMPARE(:,:,:,:) = 0.0D0
      END IF

      RETURN
      END

      SUBROUTINE MAXD_METEXT
C***********************************************************************
C                MAXD_METEXT Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Controls extraction of meteorological data from
C                 arrays for use with OU MAXDCONT option
C
C        PROGRAMMER: Roger W. Brode
C
C        DATE:       February 28, 2011
C
C        MODIFIED:   Modified to PG stability parameter for the buoyant
C                    line source
C                    Amec Foster Wheeler, 06/30/2015
C
C        INPUTS:
C
C        OUTPUTS:
C
C        CALLED FROM:   MAXDCALC
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      USE BUOYANT_LINE
      IMPLICIT NONE
      CHARACTER MODNAM*12
      INTEGER I

C     Variable Initializations
      MODNAM = 'MAXD_METEXT'
      PATH   = 'MX'

C     Save Value of Last YR/MN/DY/HR and Previous Hour
      IPDATE = KURDAT
      IPYEAR = IYR
      IPHOUR = IHOUR

C     Set Meteorological Variables for This Hour
      SFCHF  = ASFCHF(IHR_NDX,IYR_NDX)
      UREF   = AUREF(IHR_NDX,IYR_NDX)
      UREFHT = AUREFHT(IHR_NDX,IYR_NDX)
      TA     = ATA(IHR_NDX,IYR_NDX)
      TREFHT = ATREFHT(IHR_NDX,IYR_NDX)
      WDREF  = AWDREF(IHR_NDX,IYR_NDX)
      USTAR  = AUSTAR(IHR_NDX,IYR_NDX)
      WSTAR  = AWSTAR(IHR_NDX,IYR_NDX)
      ZICONV = AZICONV(IHR_NDX,IYR_NDX)
      ZIMECH = AZIMECH(IHR_NDX,IYR_NDX)
      OBULEN = AOBULEN(IHR_NDX,IYR_NDX)
      VPTGZI = AVPTGZI(IHR_NDX,IYR_NDX)
      SFCZ0  = ASFCZ0(IHR_NDX,IYR_NDX)
      KST    = NINT( AKST(IHR_NDX,IYR_NDX) )
      BLTA   = ABLTA(IHR_NDX,IYR_NDX)

      IF (LDGAS .OR. LDPART .OR. LWPART .OR. LWGAS .OR. GRSM) THEN
         BOWEN  = ABOWEN(IHR_NDX,IYR_NDX)
         ALBEDO = AALBEDO(IHR_NDX,IYR_NDX)
         IPCODE = IAPCODE(IHR_NDX,IYR_NDX)
         PRATE  = APRATE(IHR_NDX,IYR_NDX)
         RH     = ARH(IHR_NDX,IYR_NDX)
         SFCP   = ASFCP(IHR_NDX,IYR_NDX)
         NCLOUD = NACLOUD(IHR_NDX,IYR_NDX)
         QSW    = AQSW(IHR_NDX,IYR_NDX)
         Wnew   = AWnew(IHR_NDX,IYR_NDX)
         f2     = Af2(IHR_NDX,IYR_NDX)
         EsTa   = AEsTa(IHR_NDX,IYR_NDX)
         Prec1  = APrec1(IHR_NDX,IYR_NDX)
         Prec2  = APrec2(IHR_NDX,IYR_NDX)
      END IF

      RURUSTR   = ARURUSTR(IHR_NDX,IYR_NDX)
      RUROBULEN = ARUROBULEN(IHR_NDX,IYR_NDX)

      CLMHR = ACLMHR(IHR_NDX,IYR_NDX)
      MSGHR = AMSGHR(IHR_NDX,IYR_NDX)

      UNSTAB  = AUNSTAB(IHR_NDX,IYR_NDX)
      STABLE  = ASTABLE(IHR_NDX,IYR_NDX)
      URBSTAB = AURBSTAB(IHR_NDX,IYR_NDX)

      NDX4ZI = ANDX4ZI(IHR_NDX,IYR_NDX)
      UATZI  = AUATZI(IHR_NDX,IYR_NDX)
      SVATZI = ASVATZI(IHR_NDX,IYR_NDX)
      SWATZI = ASWATZI(IHR_NDX,IYR_NDX)
      UAVG   = AUAVG(IHR_NDX,IYR_NDX)
      SVAVG  = ASVAVG(IHR_NDX,IYR_NDX)
      SWAVG  = ASWAVG(IHR_NDX,IYR_NDX)
      PTATZI = APTATZI(IHR_NDX,IYR_NDX)

      GRIDWD(1:MXGLVL)  = AGRIDWD(IHR_NDX,1:MXGLVL,IYR_NDX)
      GRIDWS(1:MXGLVL)  = AGRIDWS(IHR_NDX,1:MXGLVL,IYR_NDX)
      GRIDSW(1:MXGLVL)  = AGRIDSW(IHR_NDX,1:MXGLVL,IYR_NDX)
      GRIDSV(1:MXGLVL)  = AGRIDSV(IHR_NDX,1:MXGLVL,IYR_NDX)
      GRIDTG(1:MXGLVL)  = AGRIDTG(IHR_NDX,1:MXGLVL,IYR_NDX)
      GRIDPT(1:MXGLVL)  = AGRIDPT(IHR_NDX,1:MXGLVL,IYR_NDX)
      IF (NSEC .GT. 0) THEN
         GRIDRHO(1:MXGLVL) = AGRIDRHO(IHR_NDX,1:MXGLVL,IYR_NDX)
      END IF
      IF (PVMRM .OR. GRSM) THEN
         GRIDEPS(1:MXGLVL) = AGRIDEPS(IHR_NDX,1:MXGLVL,IYR_NDX)
      END IF

      IF (NURB .GT. 0) THEN
         GRDSWR(1:MXGLVL) = AGRDSWR(IHR_NDX,1:MXGLVL,IYR_NDX)
         GRDSVR(1:MXGLVL) = AGRDSVR(IHR_NDX,1:MXGLVL,IYR_NDX)
         GRDTGR(1:MXGLVL) = AGRDTGR(IHR_NDX,1:MXGLVL,IYR_NDX)
         GRDPTR(1:MXGLVL) = AGRDPTR(IHR_NDX,1:MXGLVL,IYR_NDX)

         DO I = 1, NURB
            GRDSWU(1:MXGLVL,I) = AGRDSWU(IHR_NDX,1:MXGLVL,IYR_NDX,I)
            GRDSVU(1:MXGLVL,I) = AGRDSVU(IHR_NDX,1:MXGLVL,IYR_NDX,I)
            GRDTGU(1:MXGLVL,I) = AGRDTGU(IHR_NDX,1:MXGLVL,IYR_NDX,I)
            GRDPTU(1:MXGLVL,I) = AGRDPTU(IHR_NDX,1:MXGLVL,IYR_NDX,I)
            ZIURB(I)           = AZIURB(IHR_NDX,IYR_NDX,I)
            URBWSTR(I)         = AURBWSTR(IHR_NDX,IYR_NDX,I)
            URBUSTR(I)         = AURBUSTR(IHR_NDX,IYR_NDX,I)
            URBOBULEN(I)       = AURBOBULEN(IHR_NDX,IYR_NDX,I)
            L_MorningTrans(I)  = AL_MorningTrans(IHR_NDX,IYR_NDX,I)
         END DO
      END IF

      IF (.NOT.CLMHR .AND. .NOT.MSGHR) THEN
C ---    Set Meteorological Variables for Current Hour
         CALL SET_METDATA
      END IF

      RETURN
      END
