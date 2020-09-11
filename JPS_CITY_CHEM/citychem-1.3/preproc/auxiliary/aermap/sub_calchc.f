      SUBROUTINE CALCHC(IREC)
C***********************************************************************
C*               CALCHC Module of AERMAP Terrain Preprocessor
C*
C*       PURPOSE: Determine Height Scales for Each Receptor
C*
C*       PROGRAMMER: Jayant Hardikar, Roger Brode
C*
C*       DATE:    September 29, 1995
C*
C*       Revision History:
C*
C*       MODIFIED: February 9, 2009
C*                
C*       MODIFIED BY: Roger W. Brode, U.S. EPA, OAQPS, AQMG
C*
C*                Corrected problems associated with cross UTM zone 
C*                applications associated with NAD datum conversions. 
C*                Modified procedure for determining which elevation files
C*                can be skipped in hill height scale calculation to 
C*                accommodate Alaska DEMs and "mixed" DEM file applications.
C*                Incorporated use of more refined routine for calculating
C*                distances based on geographic coordinates, based on
C*                NGS INVERSE utility program. Incorporated more robust
C*                optimization of height scale calculations within 'LAT'
C*                data files based on more refined distance calculation.
C*
C*       MODIFIED: December 7, 2006
C*                
C*       MODIFIED BY: Roger W. Brode, U.S. EPA, OAQPS, AQMG
C*
C*                Corrected several problems related to NAD conversion
C*                process, procedure for optimizing critical hill height
C*                calculations for neighboring DEM files, and other issues.
C*                See header comments in AERMAP.FOR source file and
C*                AERMAP MCB#1 for more details.
C*
C*       MODIFIED: January 25, 2002
C*                
C*       MODIFIED BY: Peter Eckhoff
C*
C*                Added a routine that assumes the maximum elevation is at the 
C*                closet point on a DEM file to the receptor.  An HFDEMX is 
C*                calculated and compared to HFMAX to see if HFDEMX might exceed
C*                HFMAX.  If so all the points are analyzed, if not the DEM file 
C*                is skipped thus saving time.
C*
C*                An algorithm was also added whereby the calculations for HFMAX are
C*                skipped if the slope from the receptor to an elevation point is 
C*                less than 10%.
C*                   
C*       INPUTS:  Receptor Coordinates, DEM Data, Maximum elevation for a DEM file
C*
C*       OUTPUTS: Receptor Height Scale
C*
C*       CALLED FROM:   MAIN
C***********************************************************************
       
C*    Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM

      DOUBLE PRECISION HEFMAX, HFDEMX
      DOUBLE PRECISION RLONSHFT, RLATSHFT
      DOUBLE PRECISION XARG, YARG, ARGE, ARGN
      DOUBLE PRECISION PLAT(2), PLON(2), RMIN(NDEM)
      DOUBLE PRECISION DISTLL, BP
      DOUBLE PRECISION XRSHFT, YRSHFT, NADFLG

      INTEGER JRECRD, IPROF, INOD, ISPHERE, IREC
      INTEGER JDEM, IIDEM, IFTM
      INTEGER IZONREC
      INTEGER INDX_DEM

      LOGICAL L_Opened

      DOUBLE PRECISION HEFF, GETDEM, ZNODE

C*    Initialize Hefmax, HEFF, IDAT, IDON, and RDIST
C*    The lowest point on the Earth is in the Dead Sea area
C*      at slightly greater than 1300 feet below MSL.

      MODNAM = 'CALCHC      '
      L_Opened = .FALSE.
      HEFMAX = -1000.00D0
      HEFF   = 0.0D0
      RDIST  = 0.0D0
      BP     = 0.0D0

      IF (HILLDBG .AND. IREC .EQ. 1) THEN
         IF (IREC .EQ. 1) THEN
C*          Determine whether CALCHC debug file is already opened
            INQUIRE(UNIT=HDBG, OPENED=L_Opened)
            IF (.NOT. L_Opened) THEN
               OPEN(UNIT=HDBG,FILE=CALCHC_FILE,ERR=99,STATUS='REPLACE')
C*             Write Version Date Header to CALCHC Debug File
               WRITE(HDBG,9011) VERSN, RUNDAT, RUNTIM
 9011          FORMAT('** AERMAP - VERSION ',A5,T72,A8,/
     &                '**',T72,A8/)
            END IF
         END IF
         WRITE(HDBG,*) 'Entering CALCHC for Receptor: ',IREC
         IF (TYPDAT .EQ. 'DEM') THEN
            WRITE(HDBG,*) 'Starting with Local DEM File: ',JDM(IREC)
         ELSE IF (TYPDAT .EQ. 'NED') THEN
            WRITE(HDBG,*) 'Starting with Local NED File: ',JDM(IREC)
         END IF
      END IF

C*    If the Receptor Elevation Was Not Found, Set ZHILL=-9999.
      IF (AZELEV(IREC) .LT. -9998.999D0) THEN
         AZHILL(IREC) = -9999.0D0
         IF (HILLDBG) THEN
C*          No hill height scale set for this receptor
            WRITE(HDBG,200) IREC
  200       FORMAT(/'Elevation missing (-9999.0) for receptor: ',I8,
     &              ' ; no Hill Height Scale calculation made!'/)
         END IF
         GO TO 999
      END IF

C     Retrieve the DEM file number covering the receptor, JDEM
      JDEM = JDM(IREC)

      IZONREC = IZONR(IREC)

C*    Loop Over All DEM Files; When IIDEM = 0, HEFMAX is calculated first
C*    for the receptor's resident DEM file.

      DEMLOOP: DO IIDEM = 0, NUMDEM

C*      reinitialize max HF for each DEM file, and IFTM flag for debug output
        HFDEMX = -1000.0D0
        IFTM = 0
        
C*      Assign DEM file index, IDEM.
C*      Always start with DEM file containing the receptor (IIDEM = 0)
        IF (IIDEM .EQ. 0) THEN
C*          First pass through loop, assign DEM file index for receptor (JDEM) to IDEM
            IDEM = JDEM
          ELSE IF (IIDEM .EQ. JDEM) THEN
            CYCLE DEMLOOP
          ELSE
            IDEM = IIDEM
        END IF

C*      Check for empty direct access elevation, identified by NUMPRF(IDEM) = 0
        IF (NUMPRF(IDEM) .EQ. 0) CYCLE DEMLOOP

C       Determine whether to apply NAD shift
        IF (NADA .EQ. 0 .OR. NADA .EQ. NADD(IDEM)) THEN

           NADFLG = 0.0D0            ! No NAD shift needed

        ELSE IF( (NADA.EQ.1    .OR.  NADA.GE.5) .AND.
     &           (NADD(IDEM).GE.2 .AND. NADD(IDEM).LE.4) )THEN

           NADFLG = 1.0D0            ! Include NAD shift

        ELSE IF( (NADA.GE.2    .AND. NADA.LE.4) .AND.
     &           (NADD(IDEM).EQ.1 .OR.  NADD(IDEM).GE.5) )THEN

           NADFLG = 1.0D0            ! Include NAD shift

        ELSE

           NADFLG = 0.0D0            ! No NAD shift needed

        END IF

        RLONSHFT = RLON(IREC) + (XRDIFS(IREC) * NADFLG)
        RLATSHFT = RLAT(IREC) + (YRDIFS(IREC) * NADFLG)

C*      Convert receptor Lat/Lon with NAD shift to UTMs, based on DEM file NADD and DEM Zone
        SELECT CASE (NADD(IDEM))   ! Use datum for DEM file
          CASE (0)
            IF (IPLAN(IDEM) .EQ. 0) THEN
               ISPHERE = 4  ! UTM/LAT-LONG conversions based on GRS80 ellipsoid
            ELSE IF (IPLAN(IDEM) .EQ. 1) THEN
               ISPHERE = 0  ! UTM/LL conversions based on Clarke 1866 ellipsoid
            END IF
          CASE (2:4)
            ISPHERE = 4     ! UTM/LAT-LONG conversions based on GRS80 ellipsoid
          CASE (1,5:6)
            ISPHERE = 0     ! UTM/LL conversions based on Clarke 1866 ellipsoid
          CASE DEFAULT
            ISPHERE = 4     ! DEFAULT CASE shouldn't occur
        END SELECT

        ARGE = RLONSHFT
        ARGN = RLATSHFT
        XARG = 0.0D0
        YARG = 0.0D0
C*      Pass DEM file zone (IZOND(IDEM)) to UTMGEO to get new receptor
C*      coordinates (XARG, YARG) referenced to IZOND(IDEM)).
        CALL UTMGEO (555,IZOND(IDEM),IZONREC,XARG,YARG,ARGE,ARGN,
     &                                                      ISPHERE)
C*      Assign new receptor UTMs to XRSHFT and YRSHFT        
        XRSHFT = XARG
        YRSHFT = YARG

C       Convert Latitude and Longitude of Receptor to Radians
        PLAT(1) = (RLATSHFT / 3600.0D0) * dtorad
        PLON(1) = (RLONSHFT / 3600.0D0) * dtorad

C*      Reinitialize RDIST (distance) for new host DEM file
        RDIST = 0.0D0

        IF (HILLDBG .AND. (IDEM .EQ. JDEM)) THEN

           WRITE(HDBG, 300) TYPDAT, IDEM, FLN(IDEM), IREC,
     &                      RLATSHFT/3600.0D0, RLONSHFT/3600.0D0,
     &                      SELAT_DEGS(IDEM), SELON_DEGS(IDEM),
     &                      RDIST,
     &                      ELEVMX(IDEM), AZELEV(IREC)

  300      FORMAT(//' Checking ',A3,' File ',I5,' : ',A40,
     &             ';  for Receptor No.: ',I7,
     &                /90X,'DEM',
     &                /6X,'  RECLAT      RECLON    ',
     &                ' DEM_SELAT   DEM_SELON  ',23X,
     &             '  DIST(m)  MAXELEV  RECELEV'
     &             /2X, 2F12.3, 2X, 2F12.3, 23X, F11.2, 2F9.2)

           WRITE(HDBG,301) TYPDAT, IREC, NADA, IDEM, NADD(IDEM),
     &                     MAPNAME(IDEM)(1:24),
     &                     RLATSHFT, RLONSHFT,
     &                     YRSHFT, XRSHFT, IZONREC

  301      FORMAT(/'    REC NADA ',A3,' NADD        Map Name',
     &             '               Latitude   Longitude',
     &             '      Northing     Easting  Zone'
     &                /I7, I5, I4, I5, 2X, A24, 1X, 2F12.2, 2X,
     &                 2F12.2, 2X, I4/)

        END IF

C       For adjacent DEM files:
C       Calculate whether to do a HEFMAX calculation for the whole DEM file.

        IF (IDEM .NE. JDEM) THEN
C*        Check whether we need to process this DEM file for HILL

          IF (ELEVMX(IDEM) .GT. HEFMAX) THEN
C*           DEM max elevation is > current height scale. Next calculate closest
C*           distance from receptor to DEM file and assume max elev occurs at
C*           that distance.  PLAT(2) & PLON(2) are coordinates for closest
C*           point on DEM quadrangle in radians.

C*           Assign max elevation from DEM file to temporary HEFMAX
             HFDEMX = ELEVMX(IDEM)

C*           Assign INDX_DEM to track location of DEM file relative to
C*           the "host" DEM for receptor:
C*
C*           INDX_DEM:     7   8   9
C*                         4   5   6    where 5 = host DEM
C*                         1   2   3

             IF (SELAT_DEGS(IDEM)*DTORAD .GE. PLAT(1)) THEN 
C*              DEM file North of receptor                  
                PLAT(2) = SELAT_DEGS(IDEM) * DTORAD
                IF (SELON_DEGS(IDEM)*DTORAD .LE. PLON(1)) THEN
C*                 DEM file is Northwest of receptor                
                   PLON(2) = SELON_DEGS(IDEM) * DTORAD
                   INDX_DEM = 7
                ELSE IF (SWLON_DEGS(IDEM)*DTORAD .GE. PLON(1)) THEN
C*                 DEM file is Northeast of receptor                
                   PLON(2) = SWLON_DEGS(IDEM) * DTORAD
                   INDX_DEM = 9
                ELSE
C*                 DEM file is due North of receptor                
                   PLON(2) = PLON(1)
                   INDX_DEM = 8
                END IF
             ELSE IF (NELAT_DEGS(IDEM)*DTORAD .LE. PLAT(1)) THEN
C*              DEM file is South of receptor             
                PLAT(2) = NELAT_DEGS(IDEM) * DTORAD
                IF (NELON_DEGS(IDEM)*DTORAD .LE. PLON(1)) THEN
C*                 DEM file is Southwest of receptor                
                   PLON(2) = NELON_DEGS(IDEM) * DTORAD
                   INDX_DEM = 1
                ELSE IF (NWLON_DEGS(IDEM)*DTORAD .GE. PLON(1)) THEN
C*                 DEM file is Southeast of receptor                
                   PLON(2) = NWLON_DEGS(IDEM) * DTORAD
                   INDX_DEM = 3
                ELSE
C*                 DEM file is due South of receptor                
                   PLON(2) = PLON(1)
                   INDX_DEM = 2
                END IF
             ELSE
C*              DEM file is East or West of receptor             
                PLAT(2) = PLAT(1)
                IF (SELON_DEGS(IDEM)*DTORAD .LE. PLON(1)) THEN
C*                 DEM file is West of receptor                
                   PLON(2) = SELON_DEGS(IDEM) * DTORAD
                   INDX_DEM = 4
                ELSE IF (SWLON_DEGS(IDEM)*DTORAD .GE. PLON(1)) THEN
C*                 DEM file is East of receptor                
                   INDX_DEM = 6
                   PLON(2) = SWLON_DEGS(IDEM) * DTORAD
                ELSE
C ---              This case shouldn't happen unless receptor is in a DEM file
C                  that overlaps the initial "local" DEM file for the receptor.
C                  Process this file the same as DEM "assigned" to receptor.
C                  Assign max elevation within file to HFDEMX and skip DISTLL 
C                  calculation.
                   INDX_DEM = 5
                   PLON(2) = PLON(1)
                   HFDEMX = ELEVMX(IDEM)
                   RMIN(IDEM) = 0.0D0
                   IF (HILLDBG) THEN
                      WRITE(HDBG, 300) TYPDAT, IDEM, FLN(IDEM), IREC,
     &                             RLATSHFT/3600.0D0,RLONSHFT/3600.0D0,
     &                             SELAT_DEGS(IDEM), SELON_DEGS(IDEM),
     &                             RMIN(IDEM), ELEVMX(IDEM),AZELEV(IREC)
      
                      WRITE(HDBG,301) TYPDAT,IREC,NADA,IDEM, NADD(IDEM),
     &                             MAPNAME(IDEM)(1:24),
     &                             RLATSHFT, RLONSHFT,
     &                             YRSHFT, XRSHFT, IZONREC
                   END IF
                   
                   IF (HFDEMX .LE. HEFMAX) THEN
C*                    This file does not contain any potential critical
C*                    hill heights; issue message if needed and cycle DEM loop                   
                      IF (HILLDBG) THEN
                         WRITE (HDBG, 305) TYPDAT, FLN(IDEM)
  305                    FORMAT(' There were NO potential critical ',
     &                        'hill heights found in ',A3,' file:  ',A)
                      END IF
                      
                      CYCLE DEMLOOP
                   END IF
C*                 Skip distance calculation and start processing file                   
                   GO TO 500
                   
                END IF
             END IF

C*           Calculate closest distance from receptor to DEM file (RMIN)
             RMIN(IDEM) = DISTLL(PLAT,PLON)

C*           Compare max elev from DEM file to 10:1 slope based on closest distance
             BP = RMIN(IDEM) * 0.1D0

             IF (HILLDBG) THEN
                WRITE(HDBG, 300) TYPDAT, IDEM, FLN(IDEM), IREC,
     &                           RLATSHFT/3600.0D0,RLONSHFT/3600.0D0,
     &                           SELAT_DEGS(IDEM), SELON_DEGS(IDEM),
     &                           RMIN(IDEM),
     &                           ELEVMX(IDEM), AZELEV(IREC)

                WRITE(HDBG,301) TYPDAT, IREC, NADA, IDEM, NADD(IDEM),
     &                          MAPNAME(IDEM)(1:24),
     &                          RLATSHFT, RLONSHFT,
     &                          YRSHFT, XRSHFT, IZONREC

             END IF

             IF (BP .LE. (ELEVMX(IDEM) - AZELEV(IREC))) THEN
C*              DEM file MAY contain critical hill heights.  Assign temporary
C*              hill height (HFDEMX) for later use.

                HFDEMX = ELEVMX(IDEM)

                IF (HFDEMX .LE. HEFMAX) THEN                
C*                 DEM file will not contain any critical hill heights based on 
C*                 max elevation in file and current controlling hill height    
                   IF (HILLDBG) THEN
                      WRITE (HDBG, 305) TYPDAT, FLN(IDEM)
                   END IF
                   
                   CYCLE DEMLOOP
                END IF
                
             ELSE
C*              DEM file will not contain critical hill heights based on 
C*              receptor elevation and closest distance to DEM file
                IF (HILLDBG) THEN
                   WRITE (HDBG, 305) TYPDAT, FLN(IDEM)
                END IF
                   
                CYCLE DEMLOOP
             END IF

          ELSE 
C*           DEM file max elevation (ELEVMX) is less than current hill height scale (HEFMAX);
C*           skip this DEM file without distance calculation

             IF (HILLDBG) THEN
                WRITE(HDBG, 310) TYPDAT, IDEM, FLN(IDEM), IREC,
     &                           RLATSHFT/3600.0D0,RLONSHFT/3600.0D0,
     &                           SELAT_DEGS(IDEM), SELON_DEGS(IDEM),
     &                           ELEVMX(IDEM), AZELEV(IREC)

  310           FORMAT(//' Checking ',A3,' File ',I5,' : ',A40,
     &                ';  for Receptor No.: ',I7,
     &                /90X,'DEM',
     &                /6X,'  RECLAT      RECLON    ',
     &                ' DEM_SELAT   DEM_SELON  ',23X,
     &                '  DIST(m)  MAXELEV  RECELEV'
     &                /2X, 2F12.3, 2X, 2F12.3, 
     &                28X, '----- ', 2F9.2)

                WRITE(HDBG,301) TYPDAT, IREC, NADA, IDEM, NADD(IDEM),
     &                         MAPNAME(IDEM)(1:24),
     &                         RLATSHFT, RLONSHFT,
     &                         YRSHFT, XRSHFT, IZONREC

                WRITE (HDBG, 305) TYPDAT, FLN(IDEM)

             END IF
             
             CYCLE DEMLOOP

          END IF   ! End of ELEVMX > AZELEV IF-block

        END IF    ! End of DEM File "precheck" IF-block

500     CONTINUE

C       Check for either the first time through to calculate an HEFMAX of the DEM file
C       assigned to the receptor or if another DEM file might produce a higher HEFMAX:

        IF (IDEM .EQ. JDEM .OR. HFDEMX .GT. HEFMAX) THEN
          IFTM = 0
C*        Reopen the Index & Direct Access Files
          OPEN (IDXUNT(IDEM),FILE=IDXFIL(IDEM),STATUS='OLD')
          REWIND (IDXUNT(IDEM))

          OPEN (IDRUNT(IDEM),FILE=DIRFIL(IDEM),
     &          ACCESS='DIRECT',RECL=LREC_DIR,STATUS='OLD')

C*        Initialize record counter for direct access DEM file
          JRECRD = 0

C*        Loop Over All Profiles in this File
          PROFILE_LOOP: DO IPROF = 1,NUMPRF(IDEM)

C*           Read the Index File for this DEM File
             READ (IDXUNT(IDEM),IDXFRM) XBASE(IPROF), YBASE(IPROF),
     &             NODES(IPROF), IZONP(IPROF), MAXEL(IPROF)

C***         See if we can skip this profile based on max elev for profile.
C            Different tests needed for host DEM vs other DEMs.

             IF (IDEM .EQ. JDEM) THEN           ! DEM file contains receptor

                IF (IPLAN(IDEM) .EQ. 0) THEN
C*                 Use closest distance from receptor to profile
C*                 For 1-degree and other Lat/Lon DEMs calculate
C*                 distance based on Lat/Lon
                   PLAT(2) = PLAT(1)
                   PLON(2) = (XBASE(IPROF) / 3600.0D0) * dtorad

                   RDIST = DISTLL(PLAT,PLON)                       ! ---   CALL DISTLL
                   BP = RDIST * 0.10D0
                   
                   IF (BP .GT. (MAXEL(IPROF) - AZELEV(IREC))) THEN
C*                    We can skip this profile; Increment JRECRD and CYCLE
                      JRECRD = JRECRD + NODES(IPROF)

                      CYCLE PROFILE_LOOP
                   END IF
                   
                ELSE IF (IPLAN(IDEM) .EQ. 1) THEN
C*                 Use closest distance from receptor to profile,
C*                 based on difference in Easting (X) values

                   BP = DABS(XRSHFT - XBASE(IPROF)) * 0.1D0
                   
                   IF (BP .GT. (MAXEL(IPROF) - AZELEV(IREC))) THEN
C*                    We can skip this profile; Increment JRECRD and CYCLE
                      JRECRD = JRECRD + NODES(IPROF)

                      CYCLE PROFILE_LOOP
                   END IF

                END IF

             ELSE                               ! Non-local DEM file

                IF (IPLAN(IDEM) .EQ. 0) THEN
C*                 Use closest distance from receptor to profile
C*                 For 1-degree and other Lat/Lon DEMs calculate
C*                 distance based on Lat/Lon; 
C*                 Use base longitude of profile (XBASE) with latitude
C*                 of closest node.
                   IF (INDX_DEM .GE. 1 .AND. INDX_DEM .LE. 3) THEN
C*                    DEM file is South of host DEM; use top node of profile                   
                      PLAT(2) = ((YBASE(IPROF) + 
     &                        NODES(IPROF)*DYM(IDEM))/3600.0D0) * DTORAD
                      PLON(2) = (XBASE(IPROF) / 3600.0D0) * DTORAD

                   ELSE IF (INDX_DEM .GE. 7 .AND. INDX_DEM .LE. 9) THEN
C*                    DEM file is North of host DEM; use bottom node of profile                   
                      PLAT(2) = (YBASE(IPROF) / 3600.0D0) * DTORAD
                      PLON(2) = (XBASE(IPROF) / 3600.0D0) * DTORAD

                   ELSE 
C*                    DEM file is East or West of host DEM; use rec lat                   
                      PLAT(2) = PLAT(1)
                      PLON(2) = (XBASE(IPROF) / 3600.0D0) * DTORAD

                   END IF
                   
                   RDIST = DISTLL(PLAT,PLON)                          ! ---   CALL DISTLL
                   BP = RDIST * 0.10D0
                   
                   IF (BP .GT. (MAXEL(IPROF) - AZELEV(IREC))) THEN
C*                    We can skip this profile; Increment JRECRD and CYCLE
                      JRECRD = JRECRD + NODES(IPROF)

                      CYCLE PROFILE_LOOP
                   END IF
                   
                ELSE IF (IPLAN(IDEM) .EQ. 1) THEN
C*                 Use closest distance from receptor to profile
                   IF (INDX_DEM .GE. 1 .AND. INDX_DEM .LE. 3) THEN
C*                    DEM file is South of host DEM; use top node of profile                   
                      XARG = XBASE(IPROF)
                      YARG = YBASE(IPROF) + NODES(IPROF)*DYM(IDEM)

                   ELSE IF (INDX_DEM .GE. 7 .AND. INDX_DEM .LE. 9) THEN
C*                    DEM file is North of host DEM; use bottom node of profile                   
                      XARG = XBASE(IPROF)
                      YARG = YBASE(IPROF)
                      
                   ELSE 
C*                    DEM file is East or West of host DEM; use rec lat                   
                      XARG = XBASE(IPROF)
                      YARG = YRSHFT

                   END IF
                   
                   RDIST = DSQRT((XRSHFT-XARG)**2 + (YRSHFT-YARG)**2)
                   BP = RDIST * 0.10D0

                   IF (BP .GT. (MAXEL(IPROF) - AZELEV(IREC))) THEN
C*                    We can skip this profile; Increment JRECRD and CYCLE
                      JRECRD = JRECRD + NODES(IPROF)

                      CYCLE PROFILE_LOOP
                   END IF

                END IF

             END IF


C*           Loop Over All Nodes in This Profile
             NODE_LOOP: DO INOD = 1, NODES(IPROF)

C*              Determine the Coordinates of this Point (L/L)        ---  CALL GETNOD
                CALL GETNOD(IPROF,INOD)

C*              Read the Terrain Elevation for this Point from the Direct
C*              Access File                                          ---   CALL GETDEM
                JRECRD = JRECRD + 1
                ZNODE = GETDEM(IDEM,JRECRD)

C*              Determine the Lateral Distance (R) Between
C*              Receptor and that of this Point

                IF (IPLAN(IDEM) .EQ. 0) THEN
                   PLAT(2) = (YNODE / 3600.0D0) * dtorad
                   PLON(2) = (XNODE / 3600.0D0) * dtorad

C*                 For 1-degree and other Lat/Lon DEMs calculate
C*                 distance based on Lat/Lon

                   RDIST = DISTLL(PLAT,PLON)                       ! ---   CALL DISTLL

                ELSE IF (IPLAN(IDEM) .EQ. 1) THEN
C*                 Adjacent DEM file is in UTM coordinates; convert DEM 
C*                 node coordinates to Lat/Lon for distance calculation
                
                   XARG = XNODE
                   YARG = YNODE

C*                 For 7.5-minute DEMs calculate distance based on UTM
C*                 Receptor coordinates (XRSHFT and YRSHFT) aleady include
C*                 NAD shift and UTM zone adjustment, if necessary.

                   RDIST = DSQRT((XRSHFT-XARG)**2 + (YRSHFT-YARG)**2)

                END IF

C*              Calculate 10% slope for this node distance

                BP = RDIST * 0.1D0

C*              If a node's elevation penetrates a 10% slope from the receptor
C*                 use the node's elevation as an effective terrain height

                IF (ZNODE - AZELEV(IREC) .GE. BP) THEN
C*                 Assign node elevation to the Effective Terrain Height (Heff)
                   HEFF = ZNODE

                   IF (HEFF .GT. HEFMAX) THEN
C*                    Update Hefmax and HC
                      HEFMAX = HEFF
                      AZHILL(IREC) = HEFF
                      IF (HILLDBG) THEN
                         IF (IFTM .EQ. 0) THEN
C*                          First time HEFF is updated for this DEM/REC
C*                          Write column headings
                            IF (IPLAN(IDEM) .EQ. 0) THEN
                              WRITE(HDBG,311)
  311                         FORMAT(12X,'DEM    PROF   NODE      ',
     &                           'NODELAT     NODELON      DIST     ',
     &                           'ZNODE     RELEV    HEFMAX')
                            ELSE IF (IPLAN(IDEM) .EQ. 1) THEN
                              WRITE(HDBG,312)
  312                         FORMAT(12X,'DEM    PROF   NODE      ',
     &                           ' NODE-Y      NODE-X      DIST     ',
     &                           'ZNODE     RELEV    HEFMAX')
                            END IF
C*                          Reset "first time" flag to 1
                            IFTM = 1
                         END IF

                         WRITE(HDBG, 303) IDEM, IPROF, INOD,
     &                                   YNODE, XNODE, RDIST, ZNODE,
     &                                   AZELEV(IREC), HEFMAX
  303                    FORMAT(6X,I9,I8,I7,1X,2F12.2,4F10.2)
                      END IF
                   END IF

                END IF

             END DO NODE_LOOP
C*           End Loop on Nodes

          END DO PROFILE_LOOP
C*        End Loop on Profiles

          CLOSE (IDXUNT(IDEM))
          CLOSE (IDRUNT(IDEM))
          
          IF (HILLDBG) THEN
C*          No hill height scales set for this DEM file; issue message to debug file
            IF (IFTM .EQ. 0) THEN
              WRITE (HDBG, 305) TYPDAT, FLN(IDEM)
            END IF
          END IF

        END IF

      END DO DEMLOOP
C*    End Loop on DEM Files


C*    Do Not Allow the Hill Height Scale to be Less
C*    Than the Receptor Elevation
      IF (HEFMAX .LT. AZELEV(IREC) .OR.
     &    AZHILL(IREC) .LT. AZELEV(IREC)) THEN
          AZHILL(IREC) = AZELEV(IREC)
      END IF

      IF (HILLDBG) THEN
C*       Write final critical hill height value to debug file
         WRITE(HDBG,306) IREC, AZHILL(IREC)
  306    FORMAT(/'The Critical Hill Height for Receptor:',I6,
     &           ' is ',F8.2,' meters.'//'---------')
      END IF
      
      GO TO 999
      
 99   CONTINUE
C     Error opening HILLDBG file
      CALL ERRHDL(PATH,MODNAM,'E','500','DEBUGHIL')
      RUNERR = .TRUE.

999   CONTINUE

      RETURN

      end subroutine


      DOUBLE PRECISION FUNCTION DISTLL(PLAT,PLON)
C***********************************************************************
C*               DISTLL Module of AERMAP Terrain Preprocessor
C*
C*       PURPOSE: Determine the Linear Distance Between 2 Lat/Lon Pairs
C*
C*       PROGRAMMER: Jayant Hardikar, Roger Brode
C*
C*       DATE:    September 29, 1995
C*
C*       REVISION HISTORY:
C*                  
C*                Modified to use National Geodetic Survey program,
C*                INVERSE, to calculate geodesic distance between
C*                latitude/longitude pairs based on the GRS80/WGS84
C*                ellipsoid.  This code provides a more accurate 
C*                calculation than previous version based on a
C*                spherical distance.
C*                Roger W. Brode, U.S. EPA, OAQPS, AQMG
C*                September 5, 2008
C*                
C*       MODIFIED BY: Roger W. Brode, U.S. EPA, OAQPS, AQMG
C*
C*                    Incorporated upper bound on argument for DACOS
C*                    function to avoid potential runtime errors for
C*                    receptors collocated with DEM nodes.
C*                    December 7, 2006
C*
C*       MODIFIED:   Trap for negative argument to DACOS function,
C*                   which may occur due to rounding if the two
C*                   points are coincident.  R.W. Brode, PES, 1/22/98
C*                   
C*       INPUTS:  Lat/Long Pair
C*
C*       OUTPUTS: Linear Distance Between them
C*
C*       CALLED FROM:   MAIN
C***********************************************************************
       
C*    Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

C---- Parameters for distance calculation (based on GRS80/WGS84/NAD83):
C     A = semi-major axis of reference ellipsoid 
C     F = flattening
C     ESQ = Eccentricity squared
      DOUBLE PRECISION, PARAMETER :: A = 6378137.0D0,
     &                               F = 1.0D0/298.257223563D0,
     &                               ESQ = F*(2.0D0-F)

C---- PLAT/PLON = Latitude/Longitude Pairs (radians)
      DOUBLE PRECISION PLAT(2), PLON(2)
      
      CHARACTER*12 MODNAM

      MODNAM = 'DISTLL'
     
C---- Call routine to calculate geodesic distance between lat/lon pairs,
C     based on INVERSE program available from National Geodetic Survey.
      CALL GPNHRI(A,F,ESQ,PI,PLAT(1),PLON(1),PLAT(2),PLON(2),DISTLL)
            
      RETURN
      end function

cb::gpnhri
c
crwb  SUBROUTINE GPNHRI extracted from INVERSE program
crwb  available from the National Geodetic Survey website:
crwb  http://www.ngs.noaa.gov/PC_PROD/Inv_Fwd/
c
c name:      inverse
c version:   200208.19
c author:    stephen j. frakes
c purpose:   to compute a geodetic inverse  
c            and then display output information
c
crwb      subroutine gpnhri (a,f,esq,pi,p1,e1,p2,e2,az1,az2,s)      
      subroutine gpnhri (a,f,esq,pi,p1,e1,p2,e2,s)      
c
c********1*********2*********3*********4*********5*********6*********7*
c
c name:        gpnhri
c version:     200208.09
c written by:  robert (sid) safford
c purpose:     subroutine to compute helmert rainsford inverse problem 
c 
c     solution of the geodetic inverse problem after t. vincenty
c     modified rainsford's method with helmert's elliptical terms
c     effective in any azimuth and at any distance short of antipocal
c     from/to stations must not be the geographic pole.
c     parameter a is the semi-major axis of the reference ellipsoid
c     finv=1/f is the inverse flattening of the reference ellipsoid
c     latitudes and longitudes in radians positive north and west
c     forward and back azimuths returned in radians clockwise from south
c     geodesic distance s returned in units of semi-major axis a
c     programmed for ibm 360-195   09/23/75
c
c     note - note - note -
c     1. do not use for meridional arcs and be careful on the equator.
c     2. azimuths are from north(+) clockwise and 
c     3. longitudes are positive east(+) 
c
c input parameters:
c -----------------
c a            semi-major axis of reference ellipsoid      meters
c f            flattening (0.0033528...)
c esq          eccentricity squared 
c pi           3.14159...
c p1           lat station 1                               radians
c e1           lon station 1                               radians
c p2           lat station 2                               radians
c e2           lon station 2                               radians
c
c output parameters:
c ------------------
c az1          azi at sta 1 -> sta 2                       radians
c az2          azi at sta 2 -> sta 1                       radians
c s            geodetic dist between sta(s) 1 & 2          meters
c
c local variables and constants:
c ------------------------------
c aa               constant from subroutine gpnloa                    
c alimit           equatorial arc distance along the equator   (radians)
c arc              meridional arc distance latitude p1 to p2 (in meters)      
c az1              azimuth forward                          (in radians)
c az2              azimuth back                             (in radians)
c bb               constant from subroutine gpnloa                    
c dlon             temporary value for difference in longitude (radians)   
c equ              equatorial distance                       (in meters)
c r1,r2            temporary variables    
c s                ellipsoid distance                        (in meters)
c sms              equatorial - geodesic distance (S - s) "Sms"       
c ss               temporary variable     
c tol0             tolerance for checking computation value         
c tol1             tolerance for checking a real zero value         
c tol2             tolerance for close to zero value  
c twopi            two times constant pi               
c
c global variables and constants:
c -------------------------------
c
c    module called by:    general 
c
c    this module calls:   gpnarc, gpnloa
c       llibfore/ dsin,   dcos,   dsqrt,  dabs,  datan2, write
c
c    include files used:
c    common blocks used:  
c
c    references: microsoft fortran 4.10 optimizing compiler, 1988
c                ms-dos operating system
c    comments:
c********1*********2*********3*********4*********5*********6*********7*
c::modification history
c::197507.05, rws, ver 00 tencol released for field use
c::198311.20, rws, ver 01 mten   released to field
c::198411.26, rws, ver 07 mten2  released to field
c::198506.10, rws, wrk    enhancements released to field
c::198507.22, rws, code   modified for mten3
c::198509.01, rws, ver 11 mten3  released to field
c::198708.10, rws, code   modified to use new mten4 gpn record format
c::199112.31, rws, ver 20 mten4 released to field
c::200001.13, rws, ver 21 mten4 released to field
c::200005.26, rws, code   restructured & documentation added             
c::200012.31, rws, ver 23 mten5 released                                 
c::200104.09, rws, code   added to calblin program                       
c::200208.09, rws, code   added subroutines gpnarc & gpnloa              
c********1*********2*********3*********4*********5*********6*********7*
ce::gpnhri
c  -------------------------------
c     m t e n  (version 3)
c              (version 4.22)
c              (version 5.23)
c  -------------------------------
c
      implicit DOUBLE PRECISION (a-h,o-z)
c
      data tol0 /5.0d-15/
      data tol1 /5.0d-14/
      data tol2 /7.0d-03/
c
      twopi = 2.0d0*pi
c
c     test the longitude difference with tol1
c     tol1 is approximately 0.000000001 arc seconds
c
      ss = e2-e1
      if( dabs(ss).lt.tol1 )then
        e2 = e2+tol1
crwb        write(*,*) ' longitudal difference is near zero '
c                 
        r2 = p2
        r1 = p1
        call gpnarc ( a, f, esq, pi, r1, r2, arc )
        s  = dabs( arc )
c
crwb  az1 & az2 calculations commented out; not needed for AERMAP
crwb        if( p2.gt.p1 )then
crwb          az1 = 0.0d0
crwb          az2 = pi
crwb        else
crwb          az1 = pi   
crwb          az2 = 0.0d0
crwb        endif
        return 
      endif
c
c     test for longitude over 180 degrees
c
      dlon = e2-e1
c
      if( dlon.ge.0.0d0 )then
        if( pi.le.dlon .and. dlon.lt.twopi )then
          dlon = dlon-twopi
        endif
      else
        ss = dabs(dlon)
        if( pi.le.ss .and. ss.lt.twopi )then
          dlon = dlon+twopi
        endif
      endif
c
      ss = dabs( dlon )
      if( ss.gt.pi )then
c::     write(*,*) '  '
c::     write(*,*) ' Longitude difference over 180 degrees  '  
c::     write(*,*) ' Turn it around '
        ss = twopi-ss
      endif
c
c     compute the limit in longitude (alimit), it is equal 
c     to twice the distance from the equator to the pole,
c     as measured along the equator (east/ewst)
c
      alimit = pi*(1.0d0-f)
c
c     test for anti-nodal difference      
c
      if( ss.ge.alimit )then
        r1 = dabs(p1)
        r2 = dabs(p2)
c
c       latitudes r1 & r2 are not near the equator
c
        if( r1.gt.tol2 .and. r2.gt.tol2 )then
          goto 60
        endif
c
c       longitude difference is greater than lift-off point
c       now check to see if  "both"  r1 & r2 are on equator
c
        if( r1.lt.tol1 .and. r2.gt.tol2 )then
          goto 60
        endif
        if( r2.lt.tol1 .and. r1.gt.tol2 )then
          goto 60
        endif
c
c       check for either r1 or r2 just off the equator but < tol2
c
        if( r1.gt.tol1 .or. r2.gt.tol1 )then
          az1 = 0.0d0
          az2 = 0.0d0
          s   = 0.0d0
          return 
        endif
c
c       compute the azimuth to anti-nodal point
c
c::     write(*,*) '  '
c::     write(*,*) ' Longitude difference beyond lift-off point '  
c::     write(*,*) '  '
c
        call gpnloa (a,f,esq,pi,dlon,az1,az2,aa,bb,sms)
c
c       compute the equatorial distance & geodetic
c
        equ = a*dabs(dlon)
        s   = equ-sms
        return 
      endif
c
   60 continue
c
      f0   = (1.0d0-f)
      b    = a*f0
      epsq = esq/(1.0d0-esq)
      f2   = f*f     
      f3   = f*f2    
      f4   = f*f3    
c
c     the longitude difference 
c
      dlon  = e2-e1   
      ab    = dlon      
      kount = 0    
c
c     the reduced latitudes    
c
      u1    = f0*dsin(p1)/dcos(p1)     
      u2    = f0*dsin(p2)/dcos(p2)
c
      u1    = datan(u1)
      u2    = datan(u2)
c
      su1   = dsin(u1)    
      cu1   = dcos(u1)    
c
      su2   = dsin(u2)
      cu2   = dcos(u2)
c
c     counter for the iteration operation
c
    1 kount = kount+1     
c
      clon  = dcos(ab)   
      slon  = dsin(ab)   
c
      csig  = su1*su2+cu1*cu2*clon  
      ssig  = dsqrt((slon*cu2)**2+(su2*cu1-su1*cu2*clon)**2)  
c
      sig   = datan2(ssig,csig)
      sinalf=cu1*cu2*slon/ssig
c
      w   = (1.0d0-sinalf*sinalf)
      t4  = w*w   
      t6  = w*t4   
c
c     the coefficients of type a      
c
      ao  = f-f2*(1.0d0+f+f2)*w/4.0d0+3.0d0*f3*(1.0d0+
     1        9.0d0*f/4.0d0)*t4/16.0d0-25.0d0*f4*t6/128.0d0
      a2  = f2*(1.0d0+f+f2)*w/4.0d0-f3*(1.0d0+9.0d0*f/4.0d0)*t4/4.0d0+
     1        75.0d0*f4*t6/256.0d0
      a4  = f3*(1.0d0+9.0d0*f/4.0d0)*t4/32.0d0-15.0d0*f4*t6/256.0d0
      a6  = 5.0d0*f4*t6/768.0d0
c
c     the multiple angle functions    
c
      qo  = 0.0d0
      if( w.gt.tol0 )then
        qo = -2.0d0*su1*su2/w
      endif     
c
      q2  = csig+qo
      q4  = 2.0d0*q2*q2-1.0d0    
      q6  = q2*(4.0d0*q2*q2-3.0d0)      
      r2  = 2.0d0*ssig*csig      
      r3  = ssig*(3.0d0-4.0d0*ssig*ssig) 
c
c     the longitude difference 
c
      s   = sinalf*(ao*sig+a2*ssig*q2+a4*r2*q4+a6*r3*q6)    
      xz  = dlon+s   
c
      xy  = dabs(xz-ab)    
      ab  = dlon+s   
c
      if( xy.lt.0.5d-13 )then
        goto 4
      endif
c
      if( kount.le.7 )then
        goto 1
      endif
c
c     the coefficients of type b      
c
    4 z   = epsq*w
c
      bo  = 1.0d0+z*(1.0d0/4.0d0+z*(-3.0d0/64.0d0+z*(5.0d0/256.0d0-
     1         z*175.0d0/16384.0d0)))      
      b2  = z*(-1.0d0/4.0d0+z*(1.0d0/16.0d0+z*(-15.0d0/512.0d0+
     1         z*35.0d0/2048.0d0)))  
      b4  = z*z*(-1.0d0/128.0d0+z*(3.0d0/512.0d0-z*35.0d0/8192.0d0))
      b6  = z*z*z*(-1.0d0/1536.0d0+z*5.0d0/6144.0d0)    
c
c     the distance in meters   
c
      s   = b*(bo*sig+b2*ssig*q2+b4*r2*q4+b6*r3*q6) 
c
c     first compute the az1 & az2 for along the equator
c
crwb  az1 & az2 calculations commented out; not needed for AERMAP
crwb      if( dlon.gt.pi )then
crwb        dlon = (dlon-2.0d0*pi)
crwb      endif
crwbc
crwb      if( dabs(dlon).gt.pi )then
crwb        dlon = (dlon+2.0d0*pi)
crwb      endif
crwbc
crwb      az1 = pi/2.0d0
crwb      if( dlon.lt.0.0d0 )then
crwb        az1 = 3.0d0*az1
crwb      endif
crwbc
crwb      az2 = az1+pi
crwb      if( az2.gt.2.0d0*pi )then
crwb        az2 = az2-2.0d0*pi
crwb      endif
crwbc
crwbc     now compute the az1 & az2 for latitudes not on the equator
crwbc
crwb      if( .not.(dabs(su1).lt.tol0 .and. dabs(su2).lt.tol0) )then
crwb        tana1 =  slon*cu2/(su2*cu1-clon*su1*cu2)  
crwb        tana2 =  slon*cu1/(su1*cu2-clon*su2*cu1)  
crwb        sina1 =  sinalf/cu1
crwb        sina2 = -sinalf/cu2      
crwbc
crwbc       azimuths from north,longitudes positive east  
crwbc
crwb        az1   = datan2(sina1,sina1/tana1)   
crwb        az2   = pi-datan2(sina2,sina2/tana2)
crwb      endif
crwbc
crwb      if( az1.lt.0.0d0 )then
crwb        az1 = az1+2.0d0*pi   
crwb      endif
crwbc
crwb      if( az2.lt.0.0d0 )then
crwb        az2 = az2+2.0d0*pi
crwb      endif
c
      return     
      end 

CB::GPNARC
C
      SUBROUTINE GPNARC (AMAX,FLAT,ESQ,PI,P1,P2,ARC)
C
C********1*********2*********3*********4*********5*********6*********7*
C
C NAME:        GPNARC
C VERSION:     200005.26
C WRITTEN BY:  ROBERT (Sid) SAFFORD
C PURPOSE:     SUBROUTINE TO COMPUTE THE LENGTH OF A MERIDIONAL ARC 
C              BETWEEN TWO LATITUDES
C
C INPUT PARAMETERS:
C -----------------
C AMAX         SEMI-MAJOR AXIS OF REFERENCE ELLIPSOID
C FLAT         FLATTENING (0.0033528 ... )
C ESQ          ECCENTRICITY SQUARED FOR REFERENCE ELLIPSOID
C PI           3.14159...
C P1           LAT STATION 1
C P2           LAT STATION 2
C
C OUTPUT PARAMETERS:
C ------------------
C ARC          GEODETIC DISTANCE 
C
C LOCAL VARIABLES AND CONSTANTS:
C ------------------------------
C GLOBAL VARIABLES AND CONSTANTS:
C -------------------------------
C
C    MODULE CALLED BY:    GENERAL 
C
C    THIS MODULE CALLS:   
C       LLIBFORE/ OPEN,   CLOSE,  READ,   WRITE,  INQUIRE
C                 DABS,   DBLE,   FLOAT,  IABS,   CHAR,   ICHAR
C
C    INCLUDE FILES USED:
C    COMMON BLOCKS USED:  
C
C    REFERENCES: Microsoft FORTRAN 4.10 Optimizing Compiler, 1988
C                MS-DOS Operating System
C    COMMENTS:
C********1*********2*********3*********4*********5*********6*********7*
C::MODIFICATION HISTORY
C::197507.05, RWS, VER 00 TENCOL RELEASED FOR FIELD USE
C::198311.20, RWS, VER 01 MTEN   RELEASED TO FIELD
C::198411.26, RWS, VER 07 MTEN2  RELEASED TO FIELD
C::1985xx.xx, RWS, CODE   CREATED               
C::198506.10, RWS, WRK    ENHANCEMENTS RELEASED TO FIELD
C::198509.01, RWS, VER 11 MTEN3  RELEASED TO FIELD
C::198512.18, RWS, CODE   MODIFIED FOR MTEN3
C::198708.10, RWS, CODE   MODIFIED TO USE NEW MTEN4 GPN RECORD FORMAT
C::199112.31, RWS, VER 20 MTEN4 RELEASED TO FIELD
C::200001.13, RWS, VER 21 MTEN4 RELEASED TO FIELD
C::200005.26, RWS, CODE   RESTRUCTURED & DOCUMENTATION ADDED             
C::200012.31, RWS, VER 23 MTEN5 RELEASED                                 
C********1*********2*********3*********4*********5*********6*********7*
CE::GPNARC
C ---------------------------
C     M T E N  (VERSION 3)
C     M T E N  (VERSION 5.23)
C ---------------------------
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      LOGICAL  FLAG
C
      DATA TT/5.0D-15/
C
C     CHECK FOR A 90 DEGREE LOOKUP
C
      FLAG = .FALSE.
C
      S1 = DABS(P1)
      S2 = DABS(P2)
C
      IF( (PI/2.0D0-TT).LT.S2 .AND. S2.LT.(PI/2.0D0+TT) )THEN
        FLAG = .TRUE.
      ENDIF
C
      IF( S1.GT.TT )THEN
        FLAG = .FALSE.
      ENDIF
C
      DA = (P2-P1)
      S1 = 0.0D0
      S2 = 0.0D0
C
C     COMPUTE THE LENGTH OF A MERIDIONAL ARC BETWEEN TWO LATITUDES
C
      E2 = ESQ
      E4 = E2*E2
      E6 = E4*E2
      E8 = E6*E2
      EX = E8*E2
C
      T1 = E2*(003.0D0/4.0D0)
      T2 = E4*(015.0D0/64.0D0)
      T3 = E6*(035.0D0/512.0D0)
      T4 = E8*(315.0D0/16384.0D0)
      T5 = EX*(693.0D0/131072.0D0)
C
      A  = 1.0D0+T1+3.0D0*T2+10.0D0*T3+35.0D0*T4+126.0D0*T5
C
      IF( FLAG )THEN
        GOTO 1
      ENDIF
C
      B  = T1+4.0D0*T2+15.0D0*T3+56.0D0*T4+210.0D0*T5
      C  = T2+06.0D0*T3+28.0D0*T4+120.0D0*T5
      D  = T3+08.0D0*T4+045.0D0*T5
      E  = T4+010.0D0*T5
      F  = T5
C
      DB = DSIN(P2*2.0D0)-DSIN(P1*2.0D0)
      DC = DSIN(P2*4.0D0)-DSIN(P1*4.0D0)
      DD = DSIN(P2*6.0D0)-DSIN(P1*6.0D0)
      DE = DSIN(P2*8.0D0)-DSIN(P1*8.0D0)
      DF = DSIN(P2*10.0D0)-DSIN(P1*10.0D0)
C
C     COMPUTE THE S2 PART OF THE SERIES EXPANSION
C
      S2 = -DB*B/2.0D0+DC*C/4.0D0-DD*D/6.0D0+DE*E/8.0D0-DF*F/10.0D0
C
C     COMPUTE THE S1 PART OF THE SERIES EXPANSION
C
    1 S1 = DA*A
C
C     COMPUTE THE ARC LENGTH
C
      ARC = AMAX*(1.0D0-ESQ)*(S1+S2)
C
      RETURN
      END
cb::gpnhri
c
CB::GPNLOA
C
      SUBROUTINE GPNLOA (AMAX,FLAT,ESQ,PI,DL,AZ1,AZ2,AO,BO,SMS)
C
C********1*********2*********3*********4*********5*********6*********7*
C
C NAME:        GPNLOA
C VERSION:     200005.26
C WRITTEN BY:  ROBERT (Sid) SAFFORD
C PURPOSE:     SUBROUTINE TO COMPUTE THE LIFF-OFF-AZIMUTH CONSTANTS
C
C INPUT PARAMETERS:
C -----------------
C AMAX         SEMI-MAJOR AXIS OF REFERENCE ELLIPSOID
C FLAT         FLATTENING (0.0033528 ... )
C ESQ          ECCENTRICITY SQUARED FOR REFERENCE ELLIPSOID
C PI           3.14159...
C DL           LON DIFFERENCE
C AZ1          AZI AT STA 1 -> STA 2
C
C OUTPUT PARAMETERS:
C ------------------
C AZ2          AZ2 AT STA 2 -> STA 1
C AO           CONST
C BO           CONST
C SMS          DISTANCE ... EQUATORIAL - GEODESIC  (S - s)   "SMS"
C
C LOCAL VARIABLES AND CONSTANTS:
C ------------------------------
C GLOBAL VARIABLES AND CONSTANTS:
C -------------------------------
C
C    MODULE CALLED BY:    GENERAL 
C
C    THIS MODULE CALLS:   
C       LLIBFORE/ DSIN,   DCOS,   DABS,   DASIN 
C
C    INCLUDE FILES USED:
C    COMMON BLOCKS USED:  
C
C    REFERENCES: Microsoft FORTRAN 4.10 Optimizing Compiler, 1988
C                MS-DOS Operating System
C    COMMENTS:
C********1*********2*********3*********4*********5*********6*********7*
C::MODIFICATION HISTORY
C::1985xx.xx, RWS, CODE   CREATED               
C::198506.10, RWS, WRK    ENHANCEMENTS RELEASED TO FIELD
C::198509.01, RWS, VER 11 MTEN3  RELEASED TO FIELD
C::198512.18, RWS, CODE   MODIFIED FOR MTEN3
C::198708.10, RWS, CODE   MODIFIED TO USE NEW MTEN4 GPN RECORD FORMAT
C::199112.31, RWS, VER 20 MTEN4 RELEASED TO FIELD
C::200001.13, RWS, VER 21 MTEN4 RELEASED TO FIELD
C::200005.26, RWS, CODE   RESTRUCTURED & DOCUMENTATION ADDED             
C::200012.31, RWS, VER 23 MTEN5 RELEASED                                 
C********1*********2*********3*********4*********5*********6*********7*
CE::GPNLOA
C ---------------------------
C     M T E N  (VERSION 3)
C              (VERSION 4.22)
C              (VERSION 5.23)
C ---------------------------
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DATA TT/5.0D-13/
C
      DLON = DABS(DL)
      CONS = (PI-DLON)/(PI*FLAT)
      F    = FLAT
C
C     COMPUTE AN APPROXIMATE AZ
C
      AZ   = DASIN(CONS)
C
      T1   =    1.0D0
      T2   =  (-1.0D0/4.0D0)*F*(1.0D0+F+F*F)
      T4   =    3.0D0/16.0D0*F*F*(1.0D0+(9.0D0/4.0D0)*F)
      T6   = (-25.0D0/128.0D0)*F*F*F
C
      ITER = 0
    1 ITER = ITER+1
      S    = DCOS(AZ)
      C2   = S*S
C
C     COMPUTE NEW AO
C
      AO   = T1 + T2*C2 + T4*C2*C2 + T6*C2*C2*C2
      CS   = CONS/AO
      S    = DASIN(CS)
      IF( DABS(S-AZ).LT.TT )THEN
        GOTO 2
      ENDIF
C
      AZ   = S
      IF( ITER.LE.6 )THEN
        GOTO 1
      ENDIF
C
    2 AZ1  = S
      IF( DL.LT.0.0D0 )THEN
        AZ1 = 2.0D0*PI-AZ1
      ENDIF
C
      AZ2  = 2.0D0*PI-AZ1
C
C     EQUATORIAL - GEODESIC  (S - s)   "SMS"
C
      ESQP = ESQ/(1.0D0-ESQ)
      S    = DCOS(AZ1)
C
      U2   = ESQP*S*S
      U4   = U2*U2
      U6   = U4*U2
      U8   = U6*U2
C
      T1   =     1.0D0
      T2   =    (1.0D0/4.0D0)*U2
      T4   =   (-3.0D0/64.0D0)*U4
      T6   =    (5.0D0/256.0D0)*U6
      T8   = (-175.0D0/16384.0D0)*U8
C
      BO   = T1 + T2 + T4 + T6 + T8
      S    = DSIN(AZ1)
      SMS  = AMAX*PI*(1.0D0 - FLAT*DABS(S)*AO - BO*(1.0D0-FLAT))
C
      RETURN
      END
