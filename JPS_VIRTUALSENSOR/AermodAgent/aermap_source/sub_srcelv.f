      SUBROUTINE SRCELV(ISRC,JDEM)
C***********************************************************************
C*               SRCELV Module of AERMAP Terrain Preprocessor
C*
C*       PURPOSE: Determine Source Elevations by a 2-D Interpolation
C*                Between the 4 Closest Raw Terrain Data Points  
C*
C*
C*       PROGRAMMER: Jayant Hardikar, Roger Brode
C*
C*       DATE:    September 29, 1995
C*
C*       Revision History:
C*
C*       MODIFIED: February 9, 2009
C*                
C*                Corrected problems associated with cross UTM zone 
C*                applications associated with NAD datum conversions. 
C*                Modified output format for UTM Zone to I3 to
C*                accommodate Southern Hemisphere applications.
C*                Corrected problems associated with handling of
C*                missing elevations from DEM or NED files. Any
C*                elevation less than or equal to -9000.0m is
C*                treated as missing.  Modified for use of standard
C*                convention of negative for West longitude.
C*                Roger W. Brode, U.S. EPA, OAQPS, AQMG
C*
C*       MODIFIED: December 7, 2006
C*                
C*                Corrected several problems related to NAD conversion
C*                process, procedure for optimizing critical hill height
C*                calculations for neighboring DEM files, and other issues.
C*                See header comments in AERMAP.FOR source file and
C*                AERMAP MCB#1 for more details.
C*                Roger W. Brode, U.S. EPA, OAQPS, AQMG
C*
C*       INPUTS:  
C*
C*       OUTPUTS: 
C*
C*       CALLED FROM:   MAIN
C***********************************************************************
       
C*    Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM

      DOUBLE PRECISION XDM(4), YDM(4)
      DOUBLE PRECISION XSS, YSS, XSSM, YSSM
      DOUBLE PRECISION ARGE, ARGN, XARG, YARG, SLATS, SLONS
      DOUBLE PRECISION NADFLG
      INTEGER ISPHERE, JDEM, ISRC, JRC(4)
      INTEGER IP, IPROF, IERR, JRECRD, JRECMSG, NUMZ
      INTEGER IZONSRC

      LOGICAL L_Opened

      DOUBLE PRECISION :: READZ  ! Function to read elev from direct access file
      
      DOUBLE PRECISION :: ZQUADZ(4), Z(4), AVEZ, SUMZ
      
      DOUBLE PRECISION :: ZSRC

C*    Initializations
      MODNAM   = 'SRCELV'
      L_Opened = .FALSE.
      ZQUADZ   = 0.0D0
      Z        = 0.0D0
      AVEZ     = 0.0D0
      SUMZ     = 0.0D0
      ZSRC     = 0.0D0

      IF (SRCDBG) THEN
         IF (ISRC .EQ. 1) THEN
C*          Determine whether SRCELV debug file is already opened
            INQUIRE(UNIT=SELVK, OPENED=L_Opened)
            IF (.NOT. L_Opened) THEN
               OPEN (UNIT=SELVK, FILE= SRCELV_FILE, STATUS='REPLACE')
C*             Write Version Date Header to SRCELV Debug File
               WRITE(SELVK,9011) VERSN, RUNDAT, RUNTIM
 9011          FORMAT('** AERMAP - VERSION ',A5,T72,A8,/
     &                '**',T72,A8/)
               WRITE(SELVK,100) NUMSRC
  100          FORMAT(
     &      'From SRCELV: ',
     &     /'     Source elevation calculations for ',I7,' sources',
     &     /'     IERR =  0: normal (non-edge) case',
     &     /'     IERR = -1: edge source, no y-adjustment',
     &     /'     IERR = -2: edge source, w/ y-adjustment',
     &     /'     IERR = -3: source beyond range of data;',
     &     /'                possible gap within data file;',
     &     /'                flagged with warning message 420',
     &     /)
            END IF
         END IF
      END IF

      IZONSRC = IZONS(ISRC)

C     Output header to debug file
      IF (SRCDBG) THEN
        WRITE(SELVK,101) ISRC
  101   FORMAT('SOURCE ELEVATION CALCS FOR SRC#',I7)
        WRITE(SELVK,102) TYPDAT, JDEM,
     &                   DEMFIL(JDEM)(1:LEN_TRIM(DEMFIL(JDEM)))
  102   FORMAT(/'LOCATED IN ',A3,' FILE: ',I5,' ;  ',A)
        WRITE(SELVK,103) TYPDAT, NADD(JDEM), NADA
  103   FORMAT(/A3,' FILE NAD:  ',I3' ;  ANCHOR POINT NAD: ',I3/)
      END IF

C     Determine whether to apply NAD shift
      IF (NADA .EQ. 0 .OR. NADA .EQ. NADD(JDEM)) THEN

         NADFLG = 0.0D0                ! No NAD shift needed

      ELSE IF( (NADA.EQ.1    .OR.  NADA.GE.5) .AND.
     &         (NADD(JDEM).GE.2 .AND. NADD(JDEM).LE.4) )THEN

         NADFLG = 1.0D0                ! Include NAD shift

      ELSE IF( (NADA.GE.2    .AND. NADA.LE.4) .AND.
     &         (NADD(JDEM).EQ.1 .OR.  NADD(JDEM).GE.5) )THEN

         NADFLG = 1.0D0                ! Include NAD shift

      ELSE

         NADFLG = 0.0D0                ! No NAD shift needed

      END IF

C*    Apply NAD shift to source coordinates; longitude shift is
C*    assumed to be positive to the East
      SLONS = SLON(ISRC) + (XSDIFS(ISRC) * NADFLG)
      SLATS = SLAT(ISRC) + (YSDIFS(ISRC) * NADFLG)
      
      XSSM = XSRCU(ISRC) + (XSDIFM(ISRC) * NADFLG)
      YSSM = YSRCU(ISRC) + (YSDIFM(ISRC) * NADFLG)
     
      IF (IPLAN(JDEM) .EQ. 1 .AND. IZONS(ISRC) .NE. IZOND(JDEM)) THEN
C*       DEM file based on UTM projection, and source zone doesn't match DEM zone;
C*       Convert source Lat/Lon to UTMs, based on DEM file NADD and DEM Zone

         SELECT CASE (NADD(JDEM))   ! Use datum for DEM file
           CASE (0)
             IF (IPLAN(JDEM) .EQ. 0) THEN
                ISPHERE = 4  ! UTM/LAT-LONG conversions based on GRS80 ellipsoid
             ELSE IF (IPLAN(JDEM) .EQ. 1) THEN
                ISPHERE = 0  ! UTM/LL conversions based on Clarke 1866 ellipsoid
             END IF
           CASE (2:4)
             ISPHERE = 4     ! UTM/LAT-LONG conversions based on GRS80 ellipsoid
           CASE (1,5:6)
             ISPHERE = 0     ! UTM/LL conversions based on Clarke 1866 ellipsoid
           CASE DEFAULT
             ISPHERE = 4     ! DEFAULT CASE shouldn't occur
         END SELECT

         ARGE = SLONS
         ARGN = SLATS
         XARG = 0.0D0
         YARG = 0.0D0
C*       Pass DEM file zone (IZOND(IDEM)) to UTMGEO to get new source
C*       coordinates (XARG, YARG) referenced to IZOND(IDEM).
         CALL UTMGEO (555,IZOND(JDEM),IZONSRC,XARG,YARG,ARGE,ARGN,
     &                                                       ISPHERE)
         XSSM = XARG
         YSSM = YARG
      END IF

C     Select the source coords for FIND4S based on coord units of DEM file
      IF (IPLAN(JDEM) .EQ. 0) THEN
         YSS = SLATS
         XSS = SLONS
      ELSE IF (IPLAN(JDEM) .EQ. 1) THEN
         YSS = YSSM
         XSS = XSSM
      END IF

      IF (SRCDBG) THEN
         WRITE(SELVK, 301) TYPDAT, IZOND(JDEM), SLONS, SLATS, 
     &                                  SLONS/3600.0D0, SLATS/3600.0D0,
     &                                  XSSM, YSSM, IZONSRC
  301    FORMAT (A3,' FILE ZONE: ',I3,' ;  SOURCE LOCATION ',
     &           '(Lon, Lat, arc-seconds): ',2F15.4,
     &      /38X,'(Lon, Lat, dec-degrees): ',2F15.6,
     &           /43X,'(UTMx, UTMy, Zone): ',2F15.2,I4/)
      END IF

C*    Find the 4 Data Points Closest to the Source
C*    First initialize record numbers for nodes
      JREC = 0

C*    Reopen the Index & Direct Access Files
      OPEN (IDXUNT(JDEM),FILE=IDXFIL(JDEM),STATUS='OLD',
     &      ERR=999)
      REWIND (IDXUNT(JDEM))

      OPEN (IDRUNT(JDEM),FILE=DIRFIL(JDEM),
     &      ACCESS='DIRECT',RECL=LREC_DIR,ERR=999,STATUS='OLD')


C*    Read the Index File for this Terrain File
      IF (NUMPRF(JDEM) .GT. 0) THEN
         DO IPROF = 1, NUMPRF(JDEM)
           READ (IDXUNT(JDEM),IDXFRM,ERR=98,END=98) XBASE(IPROF),
     &           YBASE(IPROF), NODES(IPROF), IZONP(IPROF), MAXEL(IPROF)
         END DO

         GO TO 900

      ELSE

         WRITE(IOUNIT,*) 'Direct Access File Empty, IDEM = ',jdem
         WRITE(*,*) 'Direct Access File Empty, IDEM = ',jdem
C*       Error Reading Direct Access File
         WRITE(DUMMY,'("IDR",I5.5)') MIN(IDRUNT(JDEM),99999)
         CALL ERRHDL(PATH,MODNAM,'E','510',DUMMY)
         RUNERR = .TRUE.
         
         RETURN

      END IF

  98  CONTINUE

C*    Error Reading Index File
      WRITE(DUMMY,'("IDX",I5.5)') MIN(IDXUNT(JDEM),99999)
      CALL ERRHDL(PATH,MODNAM,'E','510',DUMMY)
      RUNERR = .TRUE.

      RETURN

 900  CONTINUE

      IF (SRCDBG) THEN
         WRITE(SELVK,303) ISRC, JDEM
 303       FORMAT ('      CALLING FIND4S w/ ISRC, JDEM: ',
     &             I7, I4)
     &
      END IF

C*    Find 4 DEM nodes closest to source location
C*
      CALL FIND4S(JDEM,ISRC,IERR,XSS,YSS)

C*    For the 4 Points, Call Subroutine to Calculate the Record#
C*    to Read the DEM Elevation Data From the Direct Access File

      CALL RECNUM
      
C*    Check for missing node elevation, based on missing indicator of -9999.
      NUMZ = 0
      SUMZ = 0.0D0
      JRECMSG = 0
      DO IP = 1, 4
C*       Read the elevation for this node from the direct access file
         JRECRD = JREC(IP)
         Z(IP) = READZ(JDEM,JRECRD)
         IF (Z(IP) .LE. -9000.0D0 .AND. JRECMSG .EQ. 0) THEN
C*          Elevation is missing for this node; Warning issued below
C*          Save record number for missing node elevation
            JRECMSG = JRECRD
         ELSE IF (Z(IP) .LE. -9000.0D0 .AND. JRECMSG .NE. 0) THEN
C*          Elevation is missing for multiple nodes
            CONTINUE
         ELSE
C*          Valid node elevation; calculate sum and num of valid elevations
            SUMZ = SUMZ + Z(IP)
            NUMZ = NUMZ + 1
         END IF
      END DO

      IF (NUMZ .GT. 0 .AND. NUMZ .LT. 4) THEN
C*       Elevation is missing for at least one node, but not all four
C*       Write Warning Message:

         CALL ERRHDL(PATH,MODNAM,'W','435',SRCID(ISRC))

C*       Calculate average elevation for non-missing nodes for substitution
         AVEZ = SUMZ/DBLE(NUMZ)
         DO IP = 1, 4
            IF (Z(IP) .LE. -9000.0D0) THEN
C*             Assign average value based on adjacent non-missing nodes
               Z(IP) = AVEZ
            END IF
         END DO
         
      ELSE IF (NUMZ .EQ. 0) THEN
C*       All four elevation nodes are missing
C*       Write Fatal Error Message:
         IERR = -3
         
         CALL ERRHDL(PATH,MODNAM,'W','436',SRCID(ISRC))

C*       Assign -9999.0 missing code to all nodes         
         DO IP = 1, 4
            Z(IP) = -9999.0D0
         END DO
         
      END IF

      IF (SRCDBG) THEN
         WRITE(SELVK,50) IERR
 50      FORMAT(7X,'DEM RECNUM:',/11X,'IERR =',I3,
     &                           /12X,'IP    JREC   NX   NY')
         WRITE (SELVK,51)  JREC(1), NX(1), NY1(1)
         WRITE (SELVK,52)  JREC(2), NX(2), NY1(2)
         WRITE (SELVK,53)  JREC(3), NX(2), NY2(2)
         WRITE (SELVK,54)  JREC(4), NX(1), NY2(1)
 51        FORMAT (12X, ' 1', I8, 2I5)
 52        FORMAT (12X, ' 2', I8, 2I5)
 53        FORMAT (12X, ' 3', I8, 2I5)
 54        FORMAT (12X, ' 4', I8, 2I5)
         WRITE(SELVK,304)
  304      FORMAT(/11X, 'Based on the DEM Coding of:'//,
     &             11X, '  IP= 2----3'/,
     &             11X, '      |    |'/,
     &             11X, '  IP= 1----4'/)
      END IF

C     Subroutine FIND4S vs DEM node numbering system:
C
C      4----3              2----3
C      |  x |              |  x |
C      |    |              |    |
C      1----2              1----4
C
C     Convert FIND4S to DEM numbering system:
C
      XDM(1) = X(1)
      XDM(2) = X(1)
      XDM(3) = X(2)
      XDM(4) = X(2)

      YDM(1) = Y(1)
      YDM(2) = Y(4)
      YDM(3) = Y(3)
      YDM(4) = Y(2)

      ZQUADZ(1) = Z(1)
      ZQUADZ(2) = Z(4)
      ZQUADZ(3) = Z(3)
      ZQUADZ(4) = Z(2)

      JRC(1) = JREC(1)
      JRC(2) = JREC(4)
      JRC(3) = JREC(3)
      JRC(4) = JREC(2)

      IF (SRCDBG) THEN
         WRITE(SELVK,305) XSS, YSS
  305      FORMAT(7X,'RECELV:              X-Src       Y-Src'/,
     &            24X,2F12.2,//
     &        9X,'IP    JREC  DEM      XDM         YDM         ELEV')
         DO IP = 1, 4
            WRITE(SELVK,306) IP, JRC(IP), JDEM,
     &                     XDM(IP), YDM(IP), ZQUADZ(IP)
  306       FORMAT(9X,I2,I8,I5, 2F12.2, F10.2)
         END DO

      END IF

      CLOSE (IDXUNT(JDEM))
      CLOSE (IDRUNT(JDEM))

C*    Call Subroutine to Interpolate Z value beween 4 points
      CALL INT2D(Z,X,Y,XSS,YSS,ZSRC)

C --- Check for internal gap sources and assign flag in ISIN array
      IF (IERR .EQ. -3 .AND. ISIN(ISRC,JDEM) .NE. 2) THEN
C*       Internal gap source; increment counter and set flag      
         NSGAP3 = NSGAP3 + 1
         ISIN(ISRC,JDEM) = 3
      END IF
      
C*    Check for IERR = -3 condition or ZSRC = -9999.0
C*    set AZS = -9999.0 unless FILLGAPS option was specified
      IF ((.NOT.FILLGAPS .AND. IERR .EQ. -3) .OR. 
     &                                       ZSRC .EQ. -9999.0D0) THEN
C*       Source is located within a gap or nodes elevations are missing;
C*       FILLGAPS option not selected, set source elevation missing
         AZS(ISRC) = -9999.0D0
         IF (SRCDBG) THEN
            IF (ISIN(ISRC,JDEM) .EQ. 2) THEN
               WRITE(SELVK,3072) ISRC, TYPDAT, JDMS(ISRC), TYPDAT, 
     &                           ISRC, AZS(ISRC)
 3072          FORMAT(/'SOURCE ',I7,' IS LOCATED WITHIN A NAD GAP',
     &              ' ADJACENT TO ',A3,' FILE ',I7,';',
     &           /15X,'ELEVATION ASSIGNED MISSING VALUE = -9999.0 (M)',
     &           /15X,'SUBSEQUENT ',A3,' FILES MAY PROVIDE ',
     &           /15X,'A NON-MISSING VALUE.',
     &            //'ELEVATION (M) FOR ISRC ',I7,' = ',F12.2,
     &            //'---------'/)
            ELSE
               WRITE(SELVK,3073) ISRC, TYPDAT, JDMS(ISRC), TYPDAT, 
     &                           ISRC, AZS(ISRC)
 3073          FORMAT(/'SOURCE ',I7,' IS LOCATED WITHIN A GAP',
     &              ' INSIDE ',A3,' FILE ',I7,';',
     &           /15X,'ELEVATION ASSIGNED MISSING VALUE = -9999.0 (M)',
     &           /15X,'SUBSEQUENT ',A3,' FILES MAY PROVIDE ',
     &           /15X,'A NON-MISSING VALUE.',
     &            //'ELEVATION (M) FOR ISRC ',I7,' = ',F12.2,
     &            //'---------'/)
            END IF
         END IF

      ELSE IF (FILLGAPS .AND. IERR .EQ. -3 .AND. 
     &                                       ZSRC .NE. -9999.0D0) THEN
C*       Source is located within a gap or nodes elevations are missing;
C*       FILLGAPS option selected, use elevation from closest nodes
         LSrc_FILLED(ISRC,JDMS(ISRC)) = .TRUE.
         AZS(ISRC) = ZSRC
         IF (SRCDBG) THEN
            IF (ISIN(ISRC,JDEM) .EQ. 2) THEN
               WRITE(SELVK,3082) ISRC, TYPDAT, JDMS(ISRC), TYPDAT, ISRC,
     &                                                            ZSRC
 3082          FORMAT(/'SOURCE ',I7,' IS LOCATED WITHIN A NAD GAP',
     &              ' ADJACENT TO ',A3,' FILE ',I7,';',
     &      /15X,'FILLGAPS OPTION HAS BEEN SELECTED AND A PRELIMINARY',
     &      /15X,'ELEVATION HAS BEEN ASSIGNED BASED ON CLOSEST NODES.',
     &      /15X,'SUBSEQUENT ',A3,' FILES MAY PROVIDE A NON-GAP VALUE.',
     &         //'PRELIMINARY ELEVATION (M) FOR ISRC ',I7,' = ',F12.2,
     &         //,'---------'/)
            ELSE
               WRITE(SELVK,3083) ISRC, TYPDAT, JDMS(ISRC), TYPDAT, ISRC,
     &                                                            ZSRC
 3083          FORMAT(/'SOURCE ',I7,' IS LOCATED WITHIN A GAP',
     &              ' INSIDE ',A3,' FILE ',I7,';',
     &      /15X,'FILLGAPS OPTION HAS BEEN SELECTED AND A PRELIMINARY',
     &      /15X,'ELEVATION HAS BEEN ASSIGNED BASED ON CLOSEST NODES.',
     &      /15X,'SUBSEQUENT ',A3,' FILES MAY PROVIDE A NON-GAP VALUE.',
     &         //'PRELIMINARY ELEVATION (M) FOR ISRC ',I7,' = ',F12.2,
     &         //,'---------'/)
            END IF
         END IF

      ELSE IF (ISIN(ISRC,JDEM) .EQ. 2 .AND. IERR .LT. 0) THEN
C*       Source located in NAD-shift gap, but close enough to be
C*       treated as edge source.
C*       Assign interpolated value to source elevation array
         AZS(ISRC) = ZSRC
         IF (SRCDBG) THEN
C*          Write elevation to debug file
            WRITE(SELVK,3092) ISRC, TYPDAT, JDMS(ISRC), ISRC, ZSRC
3092        FORMAT(/'SOURCE ',I7,' IS LOCATED WITHIN A NAD GAP',
     &              ' ADJACENT TO ',A3,' FILE ',I7,';',
     &      /15X,'HOWEVER, SOURCE IS WITHIN ONE "GRID" SPACING OF',
     &      /15X,'CLOSEST NODE AND IS TREATED AS AN EDGE SOURCE.',
     &      //'ELEVATION (M) FOR ISRC ',I7,' = ',F12.2,
     &      //,'---------'/)
         END IF

      ELSE 
C*       Assign interpolated value to source elevation array
         AZS(ISRC) = ZSRC
         IF (SRCDBG) THEN
C*          Write elevation to debug file
            WRITE(SELVK,309) ISRC, ZSRC
309         FORMAT(/'ELEVATION (M) FOR ISRC ',I7,' = ',F12.2,
     &           //,'---------'/)

         END IF

      END IF

      RETURN
      
999   CONTINUE
C --- Error opening direct access file; this shouldn't happen
      WRITE(DUMMY,'("DIR:",I4)') MIN(IDXUNT(JDEM),9999)
      CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)
      RUNERR = .TRUE.

      RETURN

      end subroutine


      SUBROUTINE FIND4S(IDM,IR,IERR, XSS, YSS)
C***********************************************************************
C*    PURPOSE:  THIS SUBROUTINE WILL LOOP OVER THE DEM ROWS AND COLUMNS
C*              OF DATA WITHIN THE DOMAIN AND FIND 4 POINTS AND THEIR 
C*              INDEXES WHICH SURROUND A GIVEN SOURCE POINT
C*
C*                4------3
C*                |  X   |
C*                |      |
C*                1------2
C*
C*    PROGRAMMER: Roger Brode, Jayant Hardikar
C*
C*    DATE:       April 14, 1997
C*
C*    MODIFIED: June 30, 2008
C*
C*             Corrected problems with handling edge sources for 
C*             some situations, including sources beyond range of
C*             nodes vertically.  These problems may occur with 
C*             with some DEM files converted from other formats.
C*             Roger W. Brode, U.S. EPA, OAQPS, AQMG
C*
C*    MODIFIED: December 7, 2006
C*
C*             Modified procedure for selecting nodes for edge sources
C*             to account for proximity in both Northing and Easting
C*             direction for 7.5-minute DEM files.
C*             Roger W. Brode, U.S. EPA, OAQPS, AQMG
C*
C*    INPUTS:     DEM file index, IDM
C*                Source index, IR
C*
C*    OUTPUTS:    Locations and indices for four points surrounding
C*                source location
C*
C*    CALLED FROM:   SRCELV
C***********************************************************************

C*----DECLARE VARIABLES-------------------------------------------------
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM

      DOUBLE PRECISION XCOMP1,XCOMP2,YCOMP1,YCOMP2,YY1,YY2
      DOUBLE PRECISION XSS, YSS

      INTEGER IERR, IBEGIN, IDM, IR, IENDS, JX, IP, NXNODE
      INTEGER IN

C*    Initialize Variables
      MODNAM = 'FIND4S'
      
C*    Initialize IERR flag to 0, normal case      
      IERR = 0

      IF (SRCDBG) THEN
         WRITE(SELVK, 100) IDM, IR, XSS, YSS
 100     FORMAT(/7X,'FIND4S:'/
     &     '        DEM  SRC        XSS             YSS',
     &          /8X,I3,I5, 2F16.4/)

         WRITE(SELVK,376) MAPNAME(IDM)
 376     FORMAT(11X,A40)

         WRITE(SELVK,377) SWE_MTRS(IDM), NWE_MTRS(IDM),
     &                    NEE_MTRS(IDM), SEE_MTRS(IDM),
     &                    SWN_MTRS(IDM), NWN_MTRS(IDM),
     &                    NEN_MTRS(IDM), SEN_MTRS(IDM)
 377     FORMAT(25X,'  SW Corner   ',
     &              '  NW Corner   ',
     &              '  NE Corner   ',
     &              '  SE Corner',
     &          /13X,'Easting: ',4F14.3/13X,'Northing:',4F14.3)

         WRITE(SELVK, 412)
     &          SWLAT_DEGS(IDM),NWLAT_DEGS(IDM),
     &          NELAT_DEGS(IDM),SELAT_DEGS(IDM),
     &          SWLON_DEGS(IDM),NWLON_DEGS(IDM),
     &          NELON_DEGS(IDM),SELON_DEGS(IDM)
 412     FORMAT(13X,'Latitude: ',2x, 4(F11.4,3x)/
     &          13X,'Longitude:',2x, 4(F11.4,3x)/)

      END IF

C*    Initialize X and Y values and indices
      X   = 0.0D0
      NX  = 0
      NY1 = 0
      NY2 = 0

      YY1 = 0.0D0
      YY2 = 0.0D0

C*    LOOP OVER ALL X-VALUES OF PROFILES

      IF (IPLAN(IDM) .EQ. 1) THEN
C*       For 7.5-minute files compute index for profile just west of the
C*       source, but make sure it is west of the SE corner of the DEM
C*       to handle edge sources along the east side of the DEM file.
         IBEGIN = MAX(1,IDINT((MIN(XSS,SEE_MTRS(IDM))
     &            -XBASE(1))/DXM(IDM)))
         IENDS  = NUMPRF(IDM)
      ELSE IF (IPLAN(IDM) .EQ. 0) THEN
         IBEGIN = MAX(1,IDINT((MIN(XSS,SELON_ARCS(IDM))
     &            -XBASE(1))/DXM(IDM)))
         IENDS  = NUMPRF(IDM)
      END IF

C*    Adjust loop bounds if IBEGIN > IENDS      
      IF (IBEGIN .GT. IENDS) IBEGIN = IENDS

C*--- Loop over profiles
      DO JX = IBEGIN, IENDS

C*       Loop through profiles in X-direction looking for profiles that
C*       bracket the source location, or profile that is collocated with
C*       source.

         XCOMP1 = XBASE(JX)
         XCOMP2 = XSS

         IF ((XCOMP1 .GT. XCOMP2) .AND.
     &	     (XCOMP1 .LE. XCOMP2+DXM(IDM))) THEN   ! Profile-X has passed Source-X

C*          Check to avoid using distant node in the Northing direction
C*          along edge profiles for 7.5-minute DEM files.
C*          IERR =  0 means normal (non-edge) case
C*          IERR = -1 means edge source, but no adjustment made for y-offset
C*          IERR = -2 means edge source, with adjustment made for y-offset
C*          IERR = -3 means edge source, beyond range of profiles or nodes;
C*                    warning message 420 generated if IERR = -3

            IF ((YBASE(JX).LE.(YSS+DYM(IDM))) .AND.
     &         ((YBASE(JX)+DBLE((NODES(JX)-1)*DYM(IDM))) .GE.
     &                         (YSS-DYM(IDM)))) THEN
C*             Source is within range of nodes within profile     

               IF (NX(1) .LE. 0) THEN
C*                Source location is outside E-W range of profiles, use closest
                  IERR  = MIN(IERR, -1) ! Edge Source, no y-offset
                  NX(1) = JX
                  NX(2) = JX
                  X(1)  = XCOMP1
                  X(2)  = XCOMP1
                  EXIT                  ! Exit loop
               ELSE
                  NX(2) = JX
                  X(2)  = XCOMP1
                  EXIT                  ! Normal (non-edge) case; Exit loop
               END IF

            ELSE IF ( (XCOMP1.GT.SEE_MTRS(IDM) .AND.
     &                 XCOMP1.LE.NEE_MTRS(IDM) .AND.
     &                 YBASE(JX).GT.(YSS+DYM(IDM))) .OR.
     &
     &                (XCOMP1.GT.NEE_MTRS(IDM) .AND.
     &                 XCOMP1.LE.SEE_MTRS(IDM) .AND.
     &               ((YBASE(JX)+DBLE((NODES(JX)-1)*DYM(IDM))) .LT.
     &                             (YSS-DYM(IDM))))) THEN
C*             Source is beyond range of nodes within profile     

               IF ((JX-1 .GE. IBEGIN) .AND.
     &            ((YBASE(JX-1).GT.(YSS+DYM(IDM))) .OR.
     &            ((YBASE(JX-1)+DBLE((NODES(JX-1)-1)*DYM(IDM))) .LT.
     &                             (YSS-DYM(IDM)))) ) THEN
C*                Source is well beyond range of nodes within profile;
C*                skip to next profile for closest nodes
                  NX(1) = MAX(JX-2,IBEGIN)
                  NX(2) = MAX(JX-2,IBEGIN)
                  X(1)  = XCOMP1-2.0D0*DXM(IDM)
                  X(2)  = XCOMP1-2.0D0*DXM(IDM)
C*                Source location is outside range of profiles, use closest
                  IF (IERR .GT. -3) THEN

                     CALL ERRHDL(PATH,MODNAM,'W','420',SRCID(IR))
                  END IF
                  IERR  = -3               ! West edge of DEM
                  EXIT
               ELSE
C*                Source is slightly beyond range of nodes within profile
C*                use closest profile
                  IERR  = MIN(IERR, -2)    ! East edge of DEM, use previous profile
                  NX(1) = MAX(JX-1,IBEGIN)
                  NX(2) = MAX(JX-1,IBEGIN)
                  X(1)  = XCOMP1-DXM(IDM)
                  X(2)  = XCOMP1-DXM(IDM)
                  EXIT                     ! Exit loop
               END IF

            ELSE IF (NX(1) .LE. 0 .AND. JX .LT. IENDS) THEN         
C*             Source location is outside E-W range of profiles, use closest

               IF ((YBASE(JX).GT.(YSS+DYM(IDM))) .OR.
     &            ((YBASE(JX)+DBLE((NODES(JX)-1)*DYM(IDM))) .LT.
     &                           (YSS-DYM(IDM)))) THEN
C*                Source is well beyond range of nodes within profile;
C*                skip to next profile for closest nodes
                  NX(1) = MIN(JX+1,IENDS)
                  NX(2) = MIN(JX+1,IENDS)
                  X(1)  = XCOMP1+DXM(IDM)
                  X(2)  = XCOMP1+DXM(IDM)
               ELSE
C*                Source is slightly beyond range of nodes within profile
C*                use closest profile
                  NX(1) = JX
                  NX(2) = JX
                  X(1)  = XCOMP1
                  X(2)  = XCOMP1
               END IF
C*             Source location is outside range of profiles, use closest
               IF (IERR .GT. -3) THEN
C*                Write warning message
                  CALL ERRHDL(PATH,MODNAM,'W','420',SRCID(IR))
               END IF
               IERR  = -3               ! West edge of DEM     
               EXIT                     ! Exit loop

            ELSE 
               NX(2) = JX               ! File may be bad, appears to
               X(2)  = XCOMP1           ! have data gap. Issue warning.
C*             Source location is outside range of profiles, use closest
               IF (IERR .GT. -3) THEN
C*                Write warning message
                  CALL ERRHDL(PATH,MODNAM,'W','420',SRCID(IR))
               END IF
               IERR  = -3
               EXIT                     ! Exit loop

            END IF

         ELSE IF (XCOMP1 .GT. XCOMP2+DXM(IDM)) THEN   ! Profile-X has passed Source-X

            IF (NX(1) .LE. 0 .AND. JX .LT. IENDS) THEN
C*             Source location is outside range of profiles, use closest
               IF ((YBASE(JX).GT.(YSS+DYM(IDM))) .OR.
     &            ((YBASE(JX)+DBLE((NODES(JX)-1)*DYM(IDM))) .LT.
     &                           (YSS-DYM(IDM)))) THEN
C*                Source is well beyond range of nodes within profile;
C*                skip to next profile for closest nodes
                  NX(1) = MIN(JX+1,IENDS)
                  NX(2) = MIN(JX+1,IENDS)
                  X(1)  = XCOMP1+DXM(IDM)
                  X(2)  = XCOMP1+DXM(IDM)
               ELSE
C*                Source is slightly beyond range of nodes within profile
C*                use closest profile
                  NX(1) = JX
                  NX(2) = JX
                  X(1)  = XCOMP1
                  X(2)  = XCOMP1
               END IF
C*             Set flag and issue warning message for "gap" Source
               IF (IERR .GT. -3) THEN
C*                Write warning message
                  CALL ERRHDL(PATH,MODNAM,'W','420',SRCID(IR))
               END IF
               IERR  = -3               ! West edge of DEM
               EXIT                     ! Exit loop

            ELSE 
               NX(2) = JX               ! File may be bad, appears to have gap
               X(2)  = XCOMP1           ! along North boundary. Issue warning.
C*             Source location is outside range of profiles, use closest
               IF (IERR .GT. -3) THEN
C*                Write warning message
                  CALL ERRHDL(PATH,MODNAM,'W','420',SRCID(IR))
               END IF
               IERR  = -3               ! West edge of DEM
               EXIT                     ! Exit loop

            END IF

         ELSE IF (XCOMP1 .EQ. XCOMP2) THEN   ! Profile-X and Source-X match
C*          Check Northing coordinates for edge sources
            IF ((YBASE(JX).LE.(YSS+DYM(IDM))) .AND.
     &         ((YBASE(JX)+DBLE((NODES(JX)-1)*DYM(IDM))) .GE.
     &                        (YSS-DYM(IDM)))) THEN
C*             Source is within range of nodes within profile     

               NX(1) = JX
               NX(2) = JX
               X(1)  = XCOMP1
               X(2)  = XCOMP1
               EXIT                     ! Normal (non-edge) case; Exit loop

            ELSE IF ( (XCOMP1.GT.NWE_MTRS(IDM) .AND.
     &                 XCOMP1.LE.SWE_MTRS(IDM) .AND.
     &                 YBASE(JX).GT.(YSS+DYM(IDM))) .OR.
     &
     &                (XCOMP1.GT.SWE_MTRS(IDM) .AND.
     &                 XCOMP1.LE.NWE_MTRS(IDM) .AND.
     &               ((YBASE(JX)+DBLE((NODES(JX)-1)*DYM(IDM))) .LT.
     &                             (YSS-DYM(IDM))))) THEN

               IF ((JX+1 .LE. IENDS) .AND.
     &            ((YBASE(JX+1).GT.(YSS+DYM(IDM))) .OR.
     &            ((YBASE(JX+1)+DBLE((NODES(JX+1)-1)*DYM(IDM))) .LT.
     &                             (YSS-DYM(IDM)))) ) THEN
C*                Source is well beyond range of nodes within profile;
C*                skip to next profile for closest nodes
                  NX(1) = MIN(JX+2,IENDS)
                  NX(2) = MIN(JX+2,IENDS)
                  X(1)  = XCOMP1+2.0D0*DXM(IDM)
                  X(2)  = XCOMP1+2.0D0*DXM(IDM)
                  IF (IERR .GT. -3) THEN
C*                   Write warning message
                     CALL ERRHDL(PATH,MODNAM,'W','420',SRCID(IR))
                  END IF
                  IERR  = -3               ! West edge of DEM
                  EXIT                     ! Exit loop
               ELSE
C*                Source is slightly beyond range of nodes within profile
C*                use closest profile
                  IERR  = MIN(IERR, -2)    ! West edge of DEM, use next profile
                  NX(1) = MIN(JX+1,IENDS)
                  NX(2) = MIN(JX+1,IENDS)
                  X(1)  = XCOMP1+DXM(IDM)
                  X(2)  = XCOMP1+DXM(IDM)
                  EXIT                     ! Exit loop
               END IF

            ELSE IF ( (XCOMP1.GT.SEE_MTRS(IDM) .AND.
     &                 XCOMP1.LE.NEE_MTRS(IDM) .AND.
     &                 YBASE(JX).GT.(YSS+DYM(IDM))) .OR.
     &
     &                (XCOMP1.GT.NEE_MTRS(IDM) .AND.
     &                 XCOMP1.LE.SEE_MTRS(IDM) .AND.
     &               ((YBASE(JX)+DBLE((NODES(JX)-1)*DYM(IDM))) .LT.
     &                             (YSS-DYM(IDM))))) THEN
C*             Source is beyond range of nodes within profile     

               IF ((JX-1 .GE. IBEGIN) .AND.
     &            ((YBASE(JX-1).GT.(YSS+DYM(IDM))) .OR.
     &            ((YBASE(JX-1)+DBLE((NODES(JX-1)-1)*DYM(IDM))) .LT.
     &                             (YSS-DYM(IDM)))) ) THEN
C*                Source is well beyond range of nodes within profile;
C*                skip to next profile for closest nodes
                  NX(1) = MAX(JX-2,IBEGIN)
                  NX(2) = MAX(JX-2,IBEGIN)
                  X(1)  = XCOMP1-2.0D0*DXM(IDM)
                  X(2)  = XCOMP1-2.0D0*DXM(IDM)
C*                Source location is outside range of profiles, use closest
                  IF (IERR .GT. -3) THEN
C*                   Write warning message
                     CALL ERRHDL(PATH,MODNAM,'W','420',SRCID(IR))
                  END IF
                  IERR  = -3               ! West edge of DEM
                  EXIT
               ELSE
C*                Source is slightly beyond range of nodes within profile
C*                use closest profile
                  IERR  = MIN(IERR, -2)    ! East edge of DEM, use previous profile
                  NX(1) = MAX(JX-1,IBEGIN)
                  NX(2) = MAX(JX-1,IBEGIN)
                  X(1)  = XCOMP1-DXM(IDM)
                  X(2)  = XCOMP1-DXM(IDM)
                  EXIT                     ! Exit loop
               END IF

            ELSE                        ! West edge of DEM, continue processing

C*             Source location is outside N-S range of profile, but within DYM
               IERR  = MIN(IERR, -1)  ! Edge source, no y-offset
               NX(1) = JX
               NX(2) = JX
               X(1)  = XCOMP1
               X(2)  = XCOMP1
               EXIT                   ! Exit loop
               
            END IF

         ELSE IF (XCOMP1 .LT. XCOMP2) THEN   ! Profile-X hasn't reached Source-X
C*          Check Northing coordinates for edge sources
            IF ((YBASE(JX).LE.(YSS+DYM(IDM))) .AND.
     &         ((YBASE(JX)+DBLE((NODES(JX)-1)*DYM(IDM))) .GE.
     &                                        (YSS-DYM(IDM)))) THEN
               NX(1) = JX
               X(1)  = XCOMP1
               IF (JX .EQ. IENDS) THEN  ! Already at last profile
                  IF (XCOMP1+DXM(IDM) .LT. XCOMP2) THEN
                     IF (IERR .GT. -3) THEN
C*                      Write warning message
                        CALL ERRHDL(PATH,MODNAM,'W','420',SRCID(IR))
                     END IF
                     IERR = -3
                  ELSE
                     IERR = -1
                  END IF
                  NX(2) = JX
                  X(2)  = XCOMP1
                  EXIT                  ! Exit loop
               ELSE
                  CYCLE
               END IF

            ELSE IF ( (XCOMP1+DXM(IDM) .GT. XCOMP2) .AND.
     &                (XCOMP1.GT.NWE_MTRS(IDM) .AND.
     &                 XCOMP1.LE.SWE_MTRS(IDM) .AND.
     &                 YBASE(JX).GT.(YSS+DYM(IDM))) .OR.
     &
     &                (XCOMP1+DXM(IDM) .GT. XCOMP2) .AND.
     &                (XCOMP1.GT.SWE_MTRS(IDM) .AND.
     &                 XCOMP1.LE.NWE_MTRS(IDM) .AND.
     &               ((YBASE(JX)+DBLE((NODES(JX)-1)*DYM(IDM))) .LT.
     &                                         (YSS-DYM(IDM)))) ) THEN

               IF (JX+1 .LE. IENDS .AND.
     &            ((YBASE(JX+1).GT.(YSS+DYM(IDM))) .OR.
     &            ((YBASE(JX+1)+DBLE((NODES(JX+1)-1)*DYM(IDM))) .LT.
     &                                          (YSS-DYM(IDM)))) ) THEN
C*                Source is well beyond range of nodes within profile;
C*                skip to next profile for closest nodes
                  NX(1) = MIN(JX+2,IENDS)
                  NX(2) = MIN(JX+2,IENDS)
                  X(1)  = XCOMP1+2.0D0*DXM(IDM)
                  X(2)  = XCOMP1+2.0D0*DXM(IDM)
                  IF (IERR .GT. -3) THEN
C*                   Write warning message
                     CALL ERRHDL(PATH,MODNAM,'W','420',SRCID(IR))
                  END IF
                  IERR  = -3               ! West edge of DEM
                  EXIT                     ! Exit loop
               ELSE
C*                Source is slightly beyond range of nodes within profile
C*                use closest profile
                  IERR  = MIN(IERR, -2)    ! West edge of DEM, use next profile
                  NX(1) = MIN(JX+1,IENDS)
                  NX(2) = MIN(JX+1,IENDS)
                  X(1)  = XCOMP1+DXM(IDM)
                  X(2)  = XCOMP1+DXM(IDM)
                  EXIT                     ! Exit loop
               END IF

            ELSE IF (XCOMP1+DXM(IDM) .GT. XCOMP2) THEN
               IF (IERR .GT. -3) THEN
C*                Write warning message
                  CALL ERRHDL(PATH,MODNAM,'W','420',SRCID(IR))
               END IF
               IERR = -3
               NX(1) = JX
               X(1)  = XCOMP1
               IF (JX .EQ. IENDS) THEN  ! Already at last profile
                  NX(2) = JX
                  X(2)  = XCOMP1
                  EXIT                  ! Exit loop
               ELSE
                  CYCLE
               END IF
               
            ELSE IF (JX .EQ. IENDS) THEN
               IF (IERR .GT. -3) THEN
C*                Write warning message
                  CALL ERRHDL(PATH,MODNAM,'W','420',SRCID(IR))
               END IF
               IERR = -3

               IF ((YBASE(JX).LE.(YSS+DYM(IDM))) .AND.
     &            ((YBASE(JX)+DBLE((NODES(JX)-1)*DYM(IDM))) .GE.
     &                                          (YSS-DYM(IDM)))) THEN
                  NX(1) = JX
                  NX(2) = JX
                  X(1)  = XCOMP1
                  X(2)  = XCOMP1
               ELSE
                  NX(1) = JX-1
                  NX(2) = JX-1
                  X(1)  = XCOMP1-DXM(IDM)
                  X(2)  = XCOMP1-DXM(IDM)
               END IF
               
               EXIT

            ELSE                        ! West edge of DEM, continue processing
               NX(1) = JX
               X(1)  = XCOMP1
               CYCLE                    ! Continue to next profile
            END IF

         END IF

      END DO   ! End loop over profiles

C*    Assign 'X'-coordinates for points 3 and 4
      X(4) = X(1)
      X(3) = X(2)

C*--- Loop over the 2 'X' points to locate the 'Y' points
      DO IP = 1,2

         NXNODE = NX(IP)

C* ---   Set limits for loop on nodes within the profile         
         IBEGIN = MAX( 1, IDINT((YSS - YBASE(NXNODE))/DYM(IDM)) )
         IENDS  = NODES(NXNODE)

C* ---   Adjust loop bounds if IBEGIN > IENDS      
         IF (IBEGIN .GT. IENDS) IBEGIN = IENDS

C* ---   Loop over nodes within profile
         DO IN = IBEGIN, IENDS

            IF (IN .EQ. IBEGIN) THEN
C*             Start with y-coordinate for node just below source
               YCOMP1 = YBASE(NXNODE) + (IN-1)*DYM(IDM)
               YCOMP2 = YSS
            ELSE
C*             Increment y-coordinate
               YCOMP1 = YCOMP1 + DYM(IDM)
            END IF

            IF (YCOMP1 .GT. YCOMP2) THEN
               IF (NY1(IP) .LE. 0) THEN
C*                Source location is outside range of profiles, use closest
                  IF (YCOMP1-YCOMP2 .GT. DYM(IDM)) THEN
                     IF (IERR .GT. -3) THEN
C*                      Write warning message
                        CALL ERRHDL(PATH,MODNAM,'W','420',SRCID(IR))
                     END IF
                     IERR = -3
                  ELSE
                     IERR = -1
                  END IF
                  NY1(IP) = IN
                  NY2(IP) = IN
                  YY1 = YCOMP1
                  YY2 = YCOMP1
                  EXIT           ! Exit loop on nodes for this 'X' point
               ELSE
                  NY2(IP) = IN
                  YY2     = YCOMP1
                  EXIT           ! Normal (non-edge) case; Exit loop
               END IF

            ELSE IF (YCOMP1 .EQ. YCOMP2) THEN
               NY2(IP) = IN
               NY1(IP) = IN
               YY1 = YCOMP1
               YY2 = YCOMP1
               EXIT              ! Normal (non-edge) case; Exit loop

            ELSE IF (YCOMP1 .LT. YCOMP2) THEN
               NY1(IP) = IN
               YY1 = YCOMP1
               IF (IN .EQ. NODES(NXNODE)) THEN
C*                Source location is outside range of profiles, use closest
                  IF (YCOMP2-YCOMP1 .GT. DYM(IDM)) THEN
                     IF (IERR .GT. -3) THEN
C*                      Write warning message
                        CALL ERRHDL(PATH,MODNAM,'W','420',SRCID(IR))
                     END IF
                     IERR = -3
                  ELSE
                     IERR = -1
                  END IF
                  NY2(IP) = IN
                  YY2 = YCOMP1
                  EXIT           ! Exit loop on nodes for this 'X' point
               END IF
            END IF

         END DO   ! End loop over nodes

         IF (IP .EQ. 1) THEN
            Y(1) = YY1
            Y(4) = YY2
         ELSE
            Y(2) = YY1
            Y(3) = YY2
         END IF

      END DO

      IF (SRCDBG) THEN
         WRITE(SELVK,89)
  89       FORMAT(11X, 'Based on the Find4S Coding of:'//,
     &            11X, '  IP= 4----3'/,
     &            11X, '      |    |'/,
     &            11X, '  IP= 1----2'/)
         WRITE(SELVK,90)
  90       FORMAT(9X,'IP    xbase      nx       xx       X-Source',
     &              '       ybase      ny       yy       Y-Source')
         write(SELVK,91) xbase(1),     nx(1),  x(1),  XSS,
     &                   ybase(nx(1)), ny1(1), y(1),  YSS

         write(SELVK,92) xbase(1),     nx(2),  x(2),  XSS,
     &                   ybase(nx(2)), ny1(2), y(2),  YSS

         write(SELVK,93) xbase(1),     nx(2),  x(2),  XSS,
     &                   ybase(nx(2)), ny2(2), y(3),  YSS

         write(SELVK,94) xbase(1),     nx(1),  x(1),  XSS,
     &                   ybase(nx(1)), ny2(1), y(4),  YSS
  91       format(9X,' 1', F12.2, I5, 2F12.2, 3X,  F12.2, I5, 2F12.2)
  92       format(9X,' 2', F12.2, I5, 2F12.2, 3X,  F12.2, I5, 2F12.2)
  93       format(9X,' 3', F12.2, I5, 2F12.2, 3X,  F12.2, I5, 2F12.2)
  94       format(9X,' 4', F12.2, I5, 2F12.2, 3X,  F12.2, I5, 2F12.2)
         WRITE(SELVK,*) ''
      END IF


999   RETURN
      end subroutine
