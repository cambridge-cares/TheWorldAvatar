      SUBROUTINE DEMCHK
C***********************************************************************
C*               DEMCHK Module of AERMAP Terrain Preprocessor
C*
C*       PURPOSE: Checks each DEM file for errors and identifies the 
C*                file format (Continuous ASCII [no CRs no LFs],
C*                Delimited [DOS or UNIX], binary or unidentified.)  
C*
C*       PROGRAMMER: Peter Eckhoff
C*
C*       DATE:    August 1, 2005
C*
C*       Revision History:
C*
C*       MODIFIED BY: Roger W. Brode, U.S. EPA, OAQPS, AQMG
C*
C*                MODIFIED: February 9, 2009
C*                
C*                Incorporated additional error checking, clarified 
C*                and simplified processing to determine DEM file type, 
C*                removed some redundant information from the 
C*                MAPDETAIL.OUT debug file, and included allocatable
C*                arrays for number of profiles within DEM files.
C*
C*       MODIFIED BY: Roger W. Brode, U.S. EPA, OAQPS, AQMG
C*
C*                MODIFIED: December 7, 2006
C*                
C*                Corrected several problems related to NAD conversion
C*                process, procedure for optimizing critical hill height
C*                calculations for neighboring DEM files, and other issues.
C*                See header comments in AERMAP.FOR source file and
C*                AERMAP MCB#1 for more details.
C*
C*       INPUTS:  DEM file
C*
C*       OUTPUTS: Flag indicating structure and a file, MAPDETAIL.OUT 
C*                with DEM header values by file and then grouped by 
C*                variable.  Variables from first three and last three 
C*                records of USGS Record Type B are also printed.
C*
C*       CALLED FROM:   MAIN
C***********************************************************************
C       

      USE MAIN1

      IMPLICIT NONE

      CHARACTER*12 MODNAM
      CHARACTER*34 FTYPE(4)
      CHARACTER*47 VDATN(0:3)
      CHARACTER*26 ACCUN(0:1)
      CHARACTER*22 SVOID(0:3)
      CHARACTER*20 IPLANN(0:2)
      CHARACTER*7 ELVPATN(2)
      CHARACTER*3 CHR(0:32)
      CHARACTER*1 ACHR, HEMA, HEMB

      INTEGER I, J, K, L, M, IOS
      INTEGER LL(0:255), T, IZA, IZB 
      INTEGER MAXT
      
      CHARACTER (LEN=3) :: CFLG
      ALLOCATABLE :: CFLG(:)
      INTEGER, ALLOCATABLE :: FLEN(:)

      ALLOCATE (FLEN(NDEM))
      ALLOCATE (CFLG(NDEM))
      
      DATA LL /256*0/ 
      DATA CHR /'NUL',
     &   'SOH','STX','ETX','EOT','ENQ','ACK','BEL',' BS',' HT',' LF',
     &   ' VT',' FF',' CR',' SO',' SI','DLE','DC1','DC2','DC3','DC4',
     &   'NAK','SYN','ETB','CAN',' EM','SUB','ESC',' FS',' GS',' RS',
     &   ' US','SPC'/
      DATA FTYPE / 'One Continuous Record - ASCII File',
     &             'Delimited File - DOS or UNIX      ', ! AERMAP treats UNIX 
     &             'not used  ', 'Binary'/               ! and DOS files the same
      DATA ELVPATN / 'regular', 'random'/
      DATA IPLANN / 'Geographic (lat/lon)', 'UTM', 'State Plane'/
      DATA ACCUN / 'unknown accuracy', 'See record type C for info'/
      DATA VDATN / 'N.A.',
     &             'local mean sea level',
     &             'National Geodetic Vertical Datum 1929 (NGVD 29)',
     &             'North American Vertical Datum 1988 (NAVD 88)'/
      DATA SVOID / 'none', 'suspect areas', 'void areas',
     &             'suspect and void areas'/


C*    Initialize variables
      MODNAM = 'DEMCHK'
      K = 0
      T = 0

      L_NeedNADCON = .false.

C*    Initialize MAXPRF variable for maximum number of DEM profiles
      MAXPRF = 0
      
C*    Assign default limit on number of characters to read at 10240,
C*    eqivalent to about 10 "records"
      MAXT = 10240

C*    Open MAPDETAIL.OUT debug file
      OPEN(UNIT=DEMK, FILE = MAPDET_FILE, STATUS = 'REPLACE')
C*    Write Version Date Header to MAPDETAIL.OUT File
      WRITE(DEMK,9011) VERSN, RUNDAT, RUNTIM
9011  FORMAT('** AERMAP - VERSION ',A5,T72,A8,/
     &       '**',T72,A8/)

      WRITE(DEMK,*) 'From DEMCHK:'
      
      DEMLOOP: DO IDEM = 1, NUMDEM

C*       Initialize variable for IOSTAT condition
         IOS = 0
         
         WRITE(DEMK,55) IDEM, DEMFIL(IDEM)(1:LEN_TRIM(DEMFIL(IDEM)))

         WRITE( *,55) IDEM, DEMFIL(IDEM)(1:LEN_TRIM(DEMFIL(IDEM)))

  55     FORMAT(/'***********************'/
     &         ' OPENING FILE: '//,
     &         '   DEM File #:    ', I6,/,
     &         '   DEM File Name: ', A,/)

         IF (L_DEMCHECK(IDEM)) THEN
            WRITE(DEMK,551)
            WRITE(*,551)
 551        FORMAT('   Full analysis of file structure on a ',
     &             'byte-by-byte basis.'/)
         ELSE
            WRITE(DEMK,552) MAXT
            WRITE(*,552) MAXT
 552        FORMAT('   Partial analysis of file structure ',
     &             'using first ',I8,' characters.'/)
         END IF

         OPEN(UNIT = IDMUNT(IDEM), FILE= DEMFIL(IDEM),
     &        FORM ='FORMATTED',
     &        STATUS = 'OLD',             
     &        ERR = 93)

C ---    Generate truncated filename, without path (up to 40 characters)
         I = INDEX(DEMFIL(IDEM),'/',BACK=.TRUE.)
         J = INDEX(DEMFIL(IDEM),ACHAR(92),BACK=.TRUE.)         ! ACHAR(92) = backslash
         IF (I .GT. 0) THEN
            IF (LEN_TRIM(DEMFIL(IDEM))-I .LE. 40) THEN
               FLN(IDEM) = DEMFIL(IDEM)(I+1:LEN_TRIM(DEMFIL(IDEM)))
            ELSE
               FLN(IDEM) = DEMFIL(IDEM)(LEN_TRIM(DEMFIL(IDEM))-39:
     &                                  LEN_TRIM(DEMFIL(IDEM)))
            END IF
         ELSE IF (J .GT. 0) THEN
            FLN(IDEM) = DEMFIL(IDEM)(J+1:LEN_TRIM(DEMFIL(IDEM)))
            IF (LEN_TRIM(DEMFIL(IDEM))-J .LE. 40) THEN
               FLN(IDEM) = DEMFIL(IDEM)(J+1:LEN_TRIM(DEMFIL(IDEM)))
            ELSE
               FLN(IDEM) = DEMFIL(IDEM)(LEN_TRIM(DEMFIL(IDEM))-39:
     &                                  LEN_TRIM(DEMFIL(IDEM)))
            END IF
         ELSE
            I = MIN(40, LEN_TRIM(DEMFIL(IDEM)))
            FLN(IDEM) = DEMFIL(IDEM)(1:I)
         END IF

C        **************************************************************
C        File Structure Analysis  *************************************
C        **************************************************************

         T = 0
         CFLG(IDEM) = 'NO '
         DO K = 0, 255
           LL(K) = 0
         END DO
         FT(IDEM) = 1    ! NO DISCERNABLE FILE TYPE (1 CONTINUOUS RECORD)
         EOF1 = .FALSE.

C*       Read through a DEM file byte by byte recording the quantity
C*         of each ASCII code in the file.  Used in determining whether
C*         the file is delimited (UNIX or DOS) or not (continuous record).
C*         AERMAP has been designed to handle either delimited or continuous
C*         data files, defined as FT = 1 or 2, respectively. Files with invalid
C*         characters are assumed to be binary, which cannot be processed by AERMAP.
C*         FT(IDEM) = 1:  No line-feed (LF) or carriage-return (CR) (one continuous record)
C*         FT(IDEM) = 2:  Delimited file, DOS (CR and LF) or UNIX (LF with no CR);
C*                        DOS and UNIX type files are handled the same in AERMAP,
C*                        FT(IDEM) = 3 is no longer used.
C*         FT(IDEM) = 4:  Invalid file type, possible binary file

C*
         DEMCHECK_LOOP: DO  

C*         Increment counter     
           T = T + 1
     
           READ(IDMUNT(IDEM),'(A1)', ADVANCE = 'NO', EOR = 92, ERR = 94,
     &                               END = 95, IOSTAT=IOS) ACHR
     
C*         Check for DEMCHECK switch; Exit Loop if no-check and T > MAXT
           IF (.NOT. L_DEMCHECK(IDEM) .AND. T .GE. MAXT) EXIT

C*         Assign ASCII character code for this character
           K = IACHAR(ACHR)
           LL(K) = LL(K) + 1
           
         END DO DEMCHECK_LOOP

         GOTO 95

 93      CONTINUE
C        Error opening a DEM file 
        
         DEMERR = .TRUE.
         WRITE(DUMMY,'("DEM#",I4)') MIN(IDEM,9999)
         CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)
         CLOSE(IDMUNT(IDEM))

         CYCLE DEMLOOP
         
 94      CONTINUE
C        Error reading a DEM file 

C        Output read counter (T) and IOSTAT value (IOS)
         WRITE(DEMK,594) T, IOS
C MSK  594    FORMAT(/'Error occurred reading DEM file at '
  594    FORMAT(/'Error occurred reading DEM file at ',
     &           'record (character) number: ',I10,
     &          /'   IOSTAT = ',I10)
         DEMERR = .TRUE.
         WRITE(DUMMY,'("DEM#",I4)') MIN(IDEM,9999)
         CALL ERRHDL(PATH,MODNAM,'E','510',DUMMY)
         CLOSE(IDMUNT(IDEM))

         CYCLE DEMLOOP

C        End of File Structure Analysis

C        **************************************************************
C        File Content Review  *****************************************
C        **************************************************************

  92     CONTINUE
C        End-of-record (EOR) encountered  
  
         IF (L_DEMCHECK(IDEM) .AND. T .GT. MAXT) THEN
C           End-of-record reached beyond MAXT, this implies a continuous record file
            FT(IDEM) = 1
         ELSE
C           End-of-record reached before MAXT, this implies DOS or UNIX file (assign 2)
            FT(IDEM) = 2
         END IF

  95     CONTINUE
C        End-of-file (EOF) branch

         CLOSE(IDMUNT(IDEM))

C        Check for evidence that file is binary
         DO K = 0, 255
           IF (LL(K) .GT. 1) THEN
             IF (K .LT. 32 .OR. K .GT. 126) THEN
C              Non-standard character; set flag for "binary" file
               CFLG(IDEM) = 'YES'
               FT(IDEM)   = 4
               EXIT
             END IF
           END IF
         END DO

         FLEN(IDEM) = T
C        File Structure Analysis Writeup         
         WRITE(DEMK,*) '    Number of bytes read: ', FLEN(IDEM)
         WRITE(DEMK,*) '    File Type:            ', FTYPE(FT(IDEM))
         WRITE(*,*)    '    Number of bytes read: ', FLEN(IDEM)
         WRITE(*,*)    '    File Type:            ', FTYPE(FT(IDEM))

         WRITE(DEMK,*)
         WRITE(DEMK,61)
   61    FORMAT(   
     & '                                          ',
     & '   Length    File            '/
     & ' File Name (w/o path up to 40 characters) ',
     & '    Read     Type     Binary?')

         WRITE(DEMK,*)
         WRITE(DEMK, 62) FLN(IDEM)(1:40), FLEN(IDEM), 
     &                   FTYPE(FT(IDEM))(1:10),
     &                   CFLG(IDEM)
   62    FORMAT(1X, A40, I10, 2X, A10, 3X, A3)
         WRITE(DEMK,*)

         IF (FT(IDEM) .EQ. 4) THEN
            WRITE(DEMK,*) '    This may be a compressed or binary file.'
            WRITE(*,*)    '    This may be a compressed or binary file.'

            WRITE(DEMK,*) ''
            WRITE(DEMK,100) FLEN(IDEM)
 100        FORMAT('   Breakdown of the file by ASCII code'
     &            ,' count based on ',I9,' bytes:')

            WRITE(DEMK,*) ''
            WRITE(DEMK,*) '      ASC  SYM    COUNT'
           
            DO K = 0, 255
               IF(K .LT. 32) THEN
                  IF(LL(K) .GT. 0) WRITE (DEMK,56) K, CHR(K), LL(K)
               ELSE
                  IF(LL(K) .GT. 0) WRITE (DEMK,56) K, CHAR(K), LL(K)
               END IF
            END DO
  56        FORMAT (6X, I3, 3X, A3, I9)
  
            WRITE(DUMMY,'("DEM#",I4)') MIN(IDEM,9999)
            CALL ERRHDL(PATH,MODNAM,'E','362',DUMMY)
            DEMERR = .TRUE.
            
C ---       Cycle to next DEM file            
            CYCLE DEMLOOP
         END IF

         IF (FT(IDEM) .NE. 1) THEN
C ---       Record delimited file (DOS or UNIX)         
            OPEN(UNIT = IDMUNT(IDEM), FILE= DEMFIL(IDEM), ERR = 96)
            
         ELSE
C ---       Continuous record file         
            OPEN(UNIT = IDMUNT(IDEM), FILE= DEMFIL(IDEM),
     &           RECL = 1024, 
     &           ACCESS = 'DIRECT', 
     &           FORM ='FORMATTED',
     &           ERR = 96)
         END IF

         WRITE(DEMK,*)
         WRITE(DEMK,*) ' Attempt to read file',
     &               ' using USGS Record Type A format.'

C        USGS Record Type A - header of a DEM file (first line)
C          What does it contain?

         IF (FT(IDEM) .NE. 1) THEN
C ---       Record delimited file (DOS or UNIX)         
            J = IDEM
            READ (IDMUNT(IDEM),51, ERR=97, END=97)
     &        MAPN(J), FREEF(J), FILR1(J), PROCODE(J), FILR2(J), 
     &        SECTNL(J), MCTR(J), DEMLVL(J), ELEVPAT(J),
     &        IPLAN(J), IZO(J), (MPROJ(J,L), L = 1,15), CUNIT(J), 
     &        ELUNIT(J), SIDZ(J), ((DMCNR(J,L,M),L=1,2),M=1,4), 
     &        ELEVMN(J), ELEVMX(J), CNTRC(J), ACCUC(J),
     &        DXM(J), DYM(J),DCI(J), NROW(J), NPROF(J), LPRIM(J),
     &        LPINT(J), SPRIM(J), SPINT(J), 
     &        DDATE(J), DINSP(J), INSPF(J), DVALD(J), SUSF(J), VDAT(J), 
     &        NADD(J), EDITN(J), PVOID(J)
         ELSE
C ---       Continuous record file                  
            J = IDEM
            READ (IDMUNT(IDEM), FMT=51, REC = 1, ERR=97)
     &        MAPN(J), FREEF(J), FILR1(J), PROCODE(J), FILR2(J), 
     &        SECTNL(J), MCTR(J), DEMLVL(J), ELEVPAT(J),
     &        IPLAN(J), IZO(J), (MPROJ(J,L), L = 1,15), CUNIT(J), 
     &        ELUNIT(J), SIDZ(J), ((DMCNR(J,L,M),L=1,2),M=1,4), 
     &        ELEVMN(J), ELEVMX(J), CNTRC(J), ACCUC(J), 
     &        DXM(J),DYM(J),DCI(J), NROW(J), NPROF(J), LPRIM(J), 
     &        LPINT(J), SPRIM(J), SPINT(J), 
     &        DDATE(J), DINSP(J), INSPF(J), DVALD(J), SUSF(J), VDAT(J), 
     &        NADD(J), EDITN(J), PVOID(J)
         END IF

  51     FORMAT(2A40, A55, 2A1, A3, A4, 2I6,
     &          2I6, 15D24.15, I6,
     &          2I6, 4(2D24.15), 2D24.15, D24.15,
     &          I6, 2E12.6, E12.6, 2I6, 2(I5,I1), 
     &          2I4, A1, I1, I2,
     &          2I2, 2I4)

         WRITE(DEMK,*)
         WRITE(DEMK,46) DEMFIL(IDEM)(1:LEN_TRIM(DEMFIL(IDEM)))
  46      FORMAT(4X,'File:        ', A)
         WRITE(DEMK,1) MAPN(IDEM)
   1      FORMAT(4X,'Map Name:    ', A40)
         WRITE(DEMK,2) FREEF(IDEM)
   2      FORMAT(4X,'Addtl Data:  ', A40)
         WRITE(DEMK,3)  FILR1(IDEM)
   3      FORMAT(4X,'Filler:      ', A55)
         WRITE(DEMK,4)  PROCODE(IDEM)
   4      FORMAT(4X,'Process Code:', A1)
         WRITE(DEMK,5)  FILR2(IDEM)
   5      FORMAT(4X,'Filler:      ', A1)
         WRITE(DEMK,6)  SECTNL(IDEM)
   6      FORMAT(4X,'Sectional Indicator:       ', A3)
         WRITE(DEMK,7)  MCTR(IDEM)
   7      FORMAT(4X,'Mapping Center origin Code:', A4)
         IF (DEMLVL(IDEM) .GE. 1 .AND. DEMLVL(IDEM) .LE. 4) THEN
            WRITE(DEMK,8)  DEMLVL(IDEM), CDLVL(DEMLVL(IDEM))
   8        FORMAT(4X,'DEM Level Code:       ', I3, '  :',A5)
         ELSE
            DEMERR = .TRUE.
            WRITE(DUMMY,'("DEM#",I4)') MIN(IDEM,9999)
            CALL ERRHDL(PATH,MODNAM,'E','510',DUMMY)
            WRITE(DEMK,*) ' ERROR!'
            WRITE(DEMK,*) ' !!Invalid DEM Level Code in DEM File: ',
     &                    IDEM
            WRITE(DEMK,*) ' '     
         END IF
         IF (ELEVPAT(IDEM) .GE. 1 .AND. ELEVPAT(IDEM) .LE. 2) THEN
            WRITE(DEMK,9)  ELEVPAT(IDEM), ELVPATN(ELEVPAT(IDEM))
   9        FORMAT(4X,'Elevation Pattern:    ', I3, '  :',A7)
         ELSE
            DEMERR = .TRUE.
            WRITE(DUMMY,'("DEM#",I4)') MIN(IDEM,9999)
            CALL ERRHDL(PATH,MODNAM,'E','510',DUMMY)
            WRITE(DEMK,*) ' ERROR!'
            WRITE(DEMK,*) 
     &            ' !!Invalid Elevation Pattern Code in DEM File: ',
     &                    IDEM            
            WRITE(DEMK,*) ' '     
         END IF
         IF (IPLAN(IDEM) .GE. 0 .AND. IPLAN(IDEM) .LE. 2) THEN
            WRITE(DEMK,10) IPLAN(IDEM), IPLANN(IPLAN(IDEM))
  10        FORMAT(4X,'Planimetric Ref. Sys: ', I3, '  :',A20)
         ELSE
            DEMERR = .TRUE.
            WRITE(DUMMY,'("DEM#",I4)') MIN(IDEM,9999)
            CALL ERRHDL(PATH,MODNAM,'E','510',DUMMY)
            WRITE(DEMK,*) ' ERROR!'
            WRITE(DEMK,*) 
     &            ' !!Invalid Planimetric Reference Code in DEM File: ',
     &                    IDEM            
            WRITE(DEMK,*) ' '     
         END IF
         IF (IZO(IDEM) .EQ. 0) THEN  
            WRITE(DEMK,11) IZO(IDEM)
  11         FORMAT(4X,'Coordinate Zone:      ', I3)
         ELSE
            IZB = 180 - (IZO(IDEM)*6)
            IZA = IZB + 6
            HEMA = 'W'
            IF (IZA .LE. 0 ) HEMA = 'E'
            HEMB = 'W'
            IF (IZB .LE. 0 ) HEMB = 'E'
            IF (IZB .EQ. -180) HEMB = 'W'
            WRITE(DEMK,47) IZO(IDEM), IZA, HEMA, IZB, HEMB
  47         FORMAT(4X,'Coordinate Zone:      ', I3, '  :from',
     &               I4,A1,' to',I4,A1)
         END IF
         WRITE(DEMK,12) (MPROJ(IDEM,L), L = 1,15)
  12      FORMAT(4X,'Map Projection Params:',3(/6X,5F12.4))
         IF (CUNIT(IDEM) .GE. 0 .AND. CUNIT(IDEM) .LE. 3) THEN
            WRITE(DEMK,13)  CUNIT(IDEM), CUNITN(CUNIT(IDEM))
  13        FORMAT(4X,'Coordinate Units:   ', I2, '  :',A11)
         ELSE
            DEMERR = .TRUE.
            WRITE(DUMMY,'("DEM#",I4)') MIN(IDEM,9999)
            CALL ERRHDL(PATH,MODNAM,'E','510',DUMMY)
            WRITE(DEMK,*) ' ERROR!'
            WRITE(DEMK,*) 
     &          ' !!Invalid Planimetric Coordinates Code in DEM File: ',
     &                    IDEM            
            WRITE(DEMK,*) ' '     
         END IF
         IF (ELUNIT(IDEM) .GE. 1 .AND. ELUNIT(IDEM) .LE. 2) THEN
            WRITE(DEMK,14)  ELUNIT(IDEM), LVLN(ELUNIT(IDEM))
  14        FORMAT(4X,'Elevation Units:    ', I2, '  :',A6)
         ELSE
            DEMERR = .TRUE.
            WRITE(DUMMY,'("DEM#",I4)') MIN(IDEM,9999)
            CALL ERRHDL(PATH,MODNAM,'E','510',DUMMY)
            WRITE(DEMK,*) ' ERROR!'
            WRITE(DEMK,*) 
     &          ' !!Invalid Elevation Units Code in DEM File: ',
     &                    IDEM            
            WRITE(DEMK,*) ' '     
         END IF
         WRITE(DEMK,15)  SIDZ(IDEM)
  15      FORMAT(4X,'Number of map sides:', I2)
         IF (DEMERR) THEN
C ---       Error(s) with processing Record A, cycle to next file
            CYCLE DEMLOOP
         END IF
         WRITE(DEMK,16) DMCNR(IDEM,1,1), CUNITN(CUNIT(IDEM))
  16      FORMAT(4X,'SW Corner Easting  or Lon:  ', F12.4, '  :',A11)
         WRITE(DEMK,17) DMCNR(IDEM,2,1), CUNITN(CUNIT(IDEM))
  17      FORMAT(4X,'SW Corner Northing or Lat:  ', F12.4, '  :',A11)
         WRITE(DEMK,18) DMCNR(IDEM,1,2), CUNITN(CUNIT(IDEM))
  18      FORMAT(4X,'NW Corner Easting  or Lon:  ', F12.4, '  :',A11)
         WRITE(DEMK,19) DMCNR(IDEM,2,2), CUNITN(CUNIT(IDEM))
  19      FORMAT(4X,'NW Corner Northing or Lat:  ', F12.4, '  :',A11)
         WRITE(DEMK,20) DMCNR(IDEM,1,3), CUNITN(CUNIT(IDEM))
  20      FORMAT(4X,'NE Corner Easting  or Lon:  ', F12.4, '  :',A11)
         WRITE(DEMK,21) DMCNR(IDEM,2,3), CUNITN(CUNIT(IDEM))
  21      FORMAT(4X,'NE Corner Northing or Lat:  ', F12.4, '  :',A11)
         WRITE(DEMK,22) DMCNR(IDEM,1,4), CUNITN(CUNIT(IDEM))
  22      FORMAT(4X,'SE Corner Easting  or Lon:  ', F12.4, '  :',A11)
         WRITE(DEMK,23) DMCNR(IDEM,2,4), CUNITN(CUNIT(IDEM))
  23      FORMAT(4X,'SE Corner Northing or Lat:  ', F12.4, '  :',A11)
         WRITE(DEMK,24)  ELEVMN(IDEM), LVLN(ELUNIT(IDEM))
  24      FORMAT(4X,'Elevation - Min: ', F12.2, '  :',A6)
         WRITE(DEMK,25)  ELEVMX(IDEM), LVLN(ELUNIT(IDEM))
  25      FORMAT(4X,'Elevation - Max: ', F12.2, '  :',A6)
         WRITE(DEMK,26)  CNTRC(IDEM), CUNITN(1)
  26      FORMAT(4X,'CClockwise angle:', F12.2, '  :',A11)
         WRITE(DEMK,27)  ACCUC(IDEM), ACCUN(ACCUC(IDEM))
  27      FORMAT(4X,'Accuracy elevation code: ',I2, '  :',A26)
         WRITE(DEMK,28)  DXM(IDEM), CUNITN(CUNIT(IDEM))
  28      FORMAT(4X,'Spatial Res. X:   ',F9.4, '  :',A11)
         WRITE(DEMK,29)  DYM(IDEM), CUNITN(CUNIT(IDEM))
  29      FORMAT(4X,'Spatial Res. Y:   ',F9.4, '  :',A11)
         WRITE(DEMK,30)  DCI(IDEM), LVLN(ELUNIT(IDEM))
  30      FORMAT(4X,'Spatial Res. Z:   ',F9.4, '  :',A6)
         WRITE(DEMK,31)  NROW(IDEM)
  31      FORMAT(4X,'Number of Rows:         ', I5)
         WRITE(DEMK,32)  NPROF(IDEM)
  32      FORMAT(4X,'Number of Profiles/Row: ', I5)
         WRITE(DEMK,33)  LPRIM(IDEM)
  33      FORMAT(4X,'Largest Primary Contour Interval: ', I4)
         IF (LPINT(IDEM) .GE. 0 .AND. LPINT(IDEM) .LE. 2) THEN
            WRITE(DEMK,34)  LPINT(IDEM), LVLN(LPINT(IDEM))
  34        FORMAT(4X,'Contour Unit:                     ',
     &                  I4, '  :',A6)
         ELSE
            DEMERR = .TRUE.
            WRITE(DUMMY,'("DEM#",I4)') MIN(IDEM,9999)
            CALL ERRHDL(PATH,MODNAM,'E','510',DUMMY)
            WRITE(DEMK,*) ' ERROR!'
            WRITE(DEMK,*) 
     &          ' !!Invalid Contour Units Code in DEM File: ',
     &                    IDEM            
            WRITE(DEMK,*) ' '     
         END IF
         WRITE(DEMK,35)  SPRIM(IDEM)
  35      FORMAT(4X,'Smallest Primary Contour Interval:',I4)
         IF (LPINT(IDEM) .GE. 0 .AND. LPINT(IDEM) .LE. 2) THEN
            WRITE(DEMK,36)  SPINT(IDEM), LVLN(SPINT(IDEM))
  36        FORMAT(4X,'Contour Unit:                     ',
     &                I4, '  :',A6)
         ELSE
            DEMERR = .TRUE.
            WRITE(DUMMY,'("DEM#",I4)') MIN(IDEM,9999)
            CALL ERRHDL(PATH,MODNAM,'E','510',DUMMY)
            WRITE(DEMK,*) ' ERROR!'
            WRITE(DEMK,*) 
     &          ' !!Invalid Contour Units Code in DEM File: ',
     &                    IDEM            
            WRITE(DEMK,*) ' '     
         END IF
         WRITE(DEMK,37)  DDATE(IDEM)
  37      FORMAT(4X,'Data Source Date:        ', I4, '  :YYMM')
         WRITE(DEMK,38)  DINSP(IDEM)
  38      FORMAT(4X,'Data Insp/Rev Date:      ', I4, '  :YYMM')
         WRITE(DEMK,39)  INSPF(IDEM)
  39      FORMAT(4X,'Insp/Rev. Flag:          ', A1)
         WRITE(DEMK,40)  DVALD(IDEM)
  40      FORMAT(4X,'Data Validation Flg:     ', I4)
         IF (SUSF(IDEM) .GE. 0 .AND. SUSF(IDEM) .LE. 3) THEN
            WRITE(DEMK,41)  SUSF(IDEM), SVOID(SUSF(IDEM))
  41        FORMAT(4X,'Suspect & Void area flag:', I4, '  :',A22)
         ELSE
            DEMERR = .TRUE.
            WRITE(DUMMY,'("DEM#",I4)') MIN(IDEM,9999)
            CALL ERRHDL(PATH,MODNAM,'E','510',DUMMY)
            WRITE(DEMK,*) ' ERROR!'
            WRITE(DEMK,*) 
     &          ' !!Invalid Suspect & Void Flag in DEM File: ',
     &                    IDEM            
            WRITE(DEMK,*) ' '     
         END IF
         IF (VDAT(IDEM) .GE. 0 .AND. VDAT(IDEM) .LE. 3) THEN
            WRITE(DEMK,42)  VDAT(IDEM), VDATN(VDAT(IDEM))
  42        FORMAT(4X,'Vertical Datum:          ', I4, '  :',A47)
         ELSE
            DEMERR = .TRUE.
            WRITE(DUMMY,'("DEM#",I4)') MIN(IDEM,9999)
            CALL ERRHDL(PATH,MODNAM,'E','510',DUMMY)
            WRITE(DEMK,*) ' ERROR!'
            WRITE(DEMK,*) 
     &          ' !!Invalid Vertical Datum Code in DEM File: ',
     &                    IDEM            
            WRITE(DEMK,*) ' '     
         END IF
         IF (NADD(IDEM) .GE. 0 .AND. NADD(IDEM) .LE. 7) THEN
            WRITE(DEMK,43)  NADD(IDEM), NADN(NADD(IDEM))
  43        FORMAT(4X,'Horizontal Datum:        ', I4, '  :',A40)
C ---       Check for whether NADCON grid files will be needed
            IF( (NADA.EQ.1  .OR.  NADA.GE.5) .AND.
     &          (NADD(IDEM).GE.2 .AND. NADD(IDEM).LE.4) )THEN

               L_NeedNADCON = .TRUE.

            ELSE IF( (NADA.GE.2  .AND. NADA.LE.4) .AND.
     &               (NADD(IDEM).EQ.1 .OR.  NADD(IDEM).GE.5) )THEN

               L_NeedNADCON = .TRUE.

            END IF
         ELSE
            DEMERR = .TRUE.
            WRITE(DUMMY,'("DEM#",I4)') MIN(IDEM,9999)
            CALL ERRHDL(PATH,MODNAM,'E','510',DUMMY)
            WRITE(DEMK,*) ' ERROR!'
            WRITE(DEMK,*) 
     &          ' !!Invalid Horizontal Datum Code in DEM File: ',
     &                    IDEM            
            WRITE(DEMK,*) ' '     
         END IF
         WRITE(DEMK,44)  EDITN(IDEM)
  44      FORMAT(4X,'Data Edition:            ', I4)
         WRITE(DEMK,45)  PVOID(IDEM)
  45      FORMAT(4X,'Percent Void:            ', I4,
     &    '  :If greater than zero, please refer to USGS Standards for',
     &    ' Digital Elevation Models')


C ---    Check for agreement between IPLAN, CUNIT, and IZO
C        and generate error and warning messages appropriately
C        When IPLAN = 0 (geographic), CUNIT = 3 (arc-seconds), (IZO should = 0)
C        When IPLAN = 1 (UTM), CUNIT = 2 (meters), IZO = integer (1-60)
C        ERROR: Disagreement btw IPLAN and CUNIT
C        ERROR: IPLAN = 1 (UTM), CUNIT = 2 (meters), IZO = 0
C        WARNING: IPLAN = 0 (geographic), CUNIT = 3 (arc-seconds), IZO = non-zero
C                 (IZO will be reset based on SW corner coordinates)
         
         IF (IPLAN(IDEM) .EQ. 0) THEN 
         ! Coordinate type = Geographic lat/lon
            IF (CUNIT(IDEM) .NE. 3) THEN 
            ! Coordinate units NOT EQUAL arc-seconds
               DEMERR = .TRUE.
               WRITE (DUMMY,'(I8)') IDEM
               CALL ERRHDL(PATH,MODNAM,'E','445',DUMMY)
            
            ELSE IF (CUNIT(IDEM) .EQ. 3 .AND. IZO(IDEM) .NE. 0) THEN
            ! units = arc-seconds, non-zero utm zone
               WRITE (DUMMY,'(I8)') IDEM
               CALL ERRHDL(PATH,MODNAM,'W','447',DUMMY)
            END IF
            
         ELSE IF (IPLAN(IDEM) .EQ. 1) THEN 
         ! Coordinate type = UTM
         
            IF (CUNIT(IDEM) .NE. 2) THEN 
            ! Coordinate units NOT EQUAL meters
               DEMERR = .TRUE.
               WRITE (DUMMY,'(I8)') IDEM
               CALL ERRHDL(PATH,MODNAM,'E','445',DUMMY)
            
            ELSE IF (CUNIT(IDEM) .EQ. 2 .AND. 
     &          (ABS(IZO(IDEM)).LT.1 .OR. ABS(IZO(IDEM)).GT.60)) THEN
            !units = meters, utm zone not in range
               DEMERR = .TRUE.
               WRITE (DUMMY,'(I8)') IDEM
               CALL ERRHDL(PATH,MODNAM,'E','446',DUMMY)
               
            END IF
            
         ELSE
         ! Invalid coordinate type
         
            DEMERR = .TRUE.
            WRITE (DUMMY,'(I8)') IDEM
            CALL ERRHDL(PATH,MODNAM,'E','448',DUMMY)
         END IF

C*       Check Number of Profiles and reset MAXPRF if needed 
         IF (NPROF(IDEM) .GT. MAXPRF) THEN
            MAXPRF = NPROF(IDEM)
         END IF
         
         CLOSE (IDMUNT(IDEM)) 

         WRITE(*,*) ''
         WRITE(*,*) 'CLOSING THE FILE.'
         WRITE(DEMK,*) ''
         WRITE(DEMK,*) 'CLOSING THE FILE.'

         CYCLE DEMLOOP
         
  97     CONTINUE           

         WRITE(DEMK,*) '    Problem Reading Appendix A data from:',
     &                DEMFIL(IDEM)
         WRITE(DUMMY,'("DEM#",I4)') MIN(IDEM,9999)
         CALL ERRHDL(PATH,MODNAM,'E','510',DUMMY)
         DEMERR = .TRUE.
         CLOSE(IDMUNT(IDEM))
         
         CYCLE DEMLOOP

  96     WRITE(DEMK,*) '    Problem Opening ',DEMFIL(IDEM), 
     &               ' as a particular file type (eg DOS)'
         CLOSE(IDMUNT(IDEM))

      END DO DEMLOOP    ! END OF MAIN DEM FILE LOOP 

C*    Allocate arrays based on MAXPRF
      ALLOCATE (XBASE(MAXPRF),YBASE(MAXPRF))

      ALLOCATE (NODES(MAXPRF), IZONP(MAXPRF))
               
      ALLOCATE (LOCEL(MAXPRF))
      ALLOCATE (MINEL(MAXPRF), MAXEL(MAXPRF))
      
      CLOSE(DEMK) ! Close MAPDETAIL.OUT
      
      write(iounit,'(/,a)') ' Exiting DEMCHK'
      write(*,'(/,a)') ' Exiting DEMCHK'

      END SUBROUTINE 
