      SUBROUTINE DOMCNV
C***********************************************************************
C*                DOMCNV Module of AERMAP Terrain Preprocessor
C*
C*       PURPOSE: To Synchronize the Coordinate Systems of the Domain 
C*                and the Terrain File Type.  If the Terrain File is of
C*                Type 'LAT', then the Domain Extents Should be in Lat/Lon
C*                Else if is of Type 'UTM', then the Domain Extents Should
C*                be in UTM.
C*
C*       PROGRAMMER: Jayant Hardikar, Roger Brode
C*
C*       DATE:    September 29, 1995
C*
C*       Revision History:
C*
C*       MODIFIED: February 9, 2009
C*                
C*                Modified code structure to avoid use of undefined 
C*                variables for applications with NADA = 0, i.e.,
C*                no NAD conversion.  Addressed issues with some
C*                cross UTM zone applications with NAD conversions.
C*                Modified for use of standard convention of 
C*                negative for West longitude.
C*                Roger W. Brode, U.S. EPA, OAQPS, AQMG
C*
C*       INPUTS: Coordinates of the Corners of the Domain
C*
C*       OUTPUTS:Coordinates of the Corners of the Domain 
C*               in a System Consistent With the Terrain Data
C*
C*       CALLED FROM:   MAIN
C***********************************************************************
        
C*    Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM

      DOUBLE PRECISION XARG, YARG, ARGE, ARGN
      DOUBLE PRECISION XPTIN, YPTIN, XPTOUT, YPTOUT
      INTEGER IZONE, IZNEW, ISPHERE, KEY
              
C*    Variable Initializations
      MODNAM = 'DOMCNV'

C*    Use the datum from the ANCHORXY input line (NADA) to define the domain.
C*    Default is GRS80 for 1-deg (LAT) and Clarke 1866 for 7.5min (UTM) files.

      SELECT CASE (NADA)
        CASE (0)
          IF (TYPDEM .EQ. 'UTM') THEN
             ISPHERE = 0  ! UTM/LL conversions based on Clarke 1866 ellipsoid
          ELSE IF (TYPDEM .EQ. 'LAT') THEN
             ISPHERE = 4  ! UTM/LAT-LONG conversions based on GRS80 ellipsoid
          END IF
        CASE (2:4)
          ISPHERE = 4     ! UTM/LAT-LONG conversions based on GRS80 ellipsoid
        CASE (1,5:6)
          ISPHERE = 0     ! UTM/LL conversions based on Clarke 1866 ellipsoid
        CASE DEFAULT
          ISPHERE = 4     ! DEFAULT CASE shouldn't occur
        END SELECT

C**** Process Domain Extents based on TYPDEM and DOMTYP.  Ultimately, the
C*    Domain will be interpreted based on TYPDEM so that domain extents will
C*    follow profiles of DEM data files.  Therefore, for 1-degree DEM and other
C*    geographic elevation data (TYPDEM = 'LAT') the domain will be interpreted
C*    in terms of Lat/Lon regardless of whether DOMAINLL or DOMAINXY is used.  
C*    Likewise, for 7.5-minute DEM and other UTM elevation data (TYPDEM = 'UTM')
C**** the domain will be interpreted in terms of UTM-X, UTM-Y and UTM-Zone.

      IF (TYPDEM .EQ. 'LAT') THEN
C ---    DEM/NED data based on geographic coordinates      

         IF (DOMTYP .EQ. 'LAT') THEN
C*          Check the latitude and issue a warning if it exceeds 66 deg -
C*          reason: latitude and longitude may be switched on DOMAINLL card;

            IF( DABS(YDMIN) .GT. 66.0D0 )THEN
               WRITE (DUMMY,'(f8.2)' ) YDMIN
               CALL ERRHDL(PATH,MODNAM,'W','360',DUMMY)
            END IF

            IF( DABS(YDMAX) .GT. 66.0D0 )THEN
               WRITE (DUMMY,'(f8.2)' ) YDMAX
               CALL ERRHDL(PATH,MODNAM,'W','360',DUMMY)
            END IF

C*          Convert domain extents to arc-seconds and
C*          save domain extents in DOMLL array.  First index is 1 for Lat,
C*          2 for Lon; second index is 1 to 4 clockwise from SW corner
C*
            DOMLL(2,1) = XDMIN * 3600.0D0     ! SW corner
            DOMLL(1,1) = YDMIN * 3600.0D0     ! SW corner
            DOMLL(2,3) = XDMAX * 3600.0D0     ! NE corner
            DOMLL(1,3) = YDMAX * 3600.0D0     ! NE corner

         ELSE IF (DOMTYP .EQ. 'UTM') THEN
C*          Convert the Extents from UTM to LAT/LON, using ISPHERE based on NADA
            XARG  = XDMIN
            YARG  = YDMIN
            IZONE = ZONMIN
            ARGE  = 0.0D0
            ARGN  = 0.0D0
            IZDUM = 0
            CALL UTMGEO (333,IZONE,IZDUM,XARG,YARG,ARGE,ARGN,ISPHERE)
            DOMLL(2,1) = ARGE      ! SW corner
            DOMLL(1,1) = ARGN      ! SW corner

            XARG  = XDMAX
            YARG  = YDMAX
            IZONE = ZONMAX
            ARGE  = 0.0D0
            ARGN  = 0.0D0
            IZDUM = 0
            CALL UTMGEO (333,IZONE,IZDUM,XARG,YARG,ARGE,ARGN,ISPHERE)
            DOMLL(2,3) = ARGE      ! NE corner
            DOMLL(1,3) = ARGN      ! NE corner
         END IF

C*       Assign SW and NE DOMLL values to other corners
         DOMLL(2,2) = DOMLL(2,1)     ! NW corner
         DOMLL(1,2) = DOMLL(1,3)     ! NW corner
         DOMLL(2,4) = DOMLL(2,3)     ! SE corner
         DOMLL(1,4) = DOMLL(1,1)     ! SE corner

C*       Determine UTM coordinates for SW and NE corners and store 
C*       in XDMIN/XDMAX and YDMIN/YDMAX variables
         ARGE   = DOMLL(2,1)
         ARGN   = DOMLL(1,1)
         XARG   = 0.0D0
         YARG   = 0.0D0
         CALL UTMGEO (555,0,IZONE,XARG,YARG,ARGE,ARGN,ISPHERE)
         XDMIN  = XARG
         YDMIN  = YARG
         ZONMIN = IZONE

         ARGE   = DOMLL(2,3)
         ARGN   = DOMLL(1,3)
         XARG   = 0.0D0
         YARG   = 0.0D0
         CALL UTMGEO (555,0,IZONE,XARG,YARG,ARGE,ARGN,ISPHERE)
         XDMAX  = XARG
         YDMAX  = YARG
         ZONMAX = IZONE

C*       Determine NAD shift for Domain:
C*       KEY = +1 indicates a transformation of NAD 27 to NAD 83 datums
C*       KEY = -1 indicates a psuedo-transformation of NAD 83 to NAD 27 datums
         KEY = 1
         IF (NADA .GE. 2 .AND. NADA .LE. 4) KEY = -1

         IF (NADA .EQ. 0 .OR. .NOT.L_NeedNADCON) THEN
C*          Skip datum conversion, assign 0.0's to NAD shifts.
            KEY = 0
            XDMNDIFS = 0.0D0
            YDMNDIFS = 0.0D0
            XDMXDIFS = 0.0D0
            YDMXDIFS = 0.0D0

            XDMNDIFM = 0.0D0
            YDMNDIFM = 0.0D0
            XDMXDIFM = 0.0D0
            YDMXDIFM = 0.0D0

            ZONMIN_SHFT = ZONMIN
            ZONMAX_SHFT = ZONMAX

         ELSE
C*          Call NADCON 2.1 code to compute DATUM shifts and convert
C*          geographic coordinates between NAD27 and NAD83, and vice versa.
C*          XPTIN  & YPTIN  parameters are LON/LAT for NAD27;
C*          XPTOUT & YPTOUT parameters are LON/LAT for NAD83.

C*
C*          First calculate shift for SW Corner (XDMIN, YDMIN)
C*
            XPT27 = DOMLL(2,1)/3600.0D0
            YPT27 = DOMLL(1,1)/3600.0D0
            
            XPTIN = XPT27
            YPTIN = YPT27

            CALL NADCON(XPTIN,YPTIN,XPTOUT,YPTOUT,DLOS,DLAS,
     &                  DLOM,DLAM,KEY)
            
C*          Save shifts in geographic coordinates in arc-seconds.
C*          Full UTM shifts are computed below in corporating shifts
C*          due to ellipsoid.
            XDMNDIFS = DLOS * DBLE(key)
            YDMNDIFS = DLAS * DBLE(key)

C*          Don't need UTM Domain Shifts for geographic elevation files
            XDMNDIFM = 0.0D0
            YDMNDIFM = 0.0D0

C*
C*          Next calculate shift for NW Corner (XDMAX, YDMAX)
C*
            XPT27 = DOMLL(2,3)/3600.0D0
            YPT27 = DOMLL(1,3)/3600.0D0

            XPTIN = XPT27
            YPTIN = YPT27
            
            CALL NADCON(XPTIN,YPTIN,XPTOUT,YPTOUT,DLOS,DLAS,
     &                  DLOM,DLAM,KEY)
C*          Save shifts in geographic coordinates in arc-seconds.
C*          Full UTM shifts are computed below in corporating shifts
C*          due to ellipsoid.
            XDMXDIFS = DLOS * DBLE(key)
            YDMXDIFS = DLAS * DBLE(key)
            
C*          Don't need UTM Domain Shifts for geographic elevation files
            XDMXDIFM = 0.0D0
            YDMXDIFM = 0.0D0

C
            ZONMIN_SHFT = ZONMIN
            ZONMAX_SHFT = ZONMAX

         END IF

      ELSE IF (TYPDEM .EQ. 'UTM') THEN
C ---    DEM/NED data based on UTM coordinates      

         IF (DOMTYP .EQ. 'LAT') THEN
C*          Check the latitude and issue a warning if it exceeds 66 deg -
C*          reason: latitude and longitude may be switched on DOMAINLL card;
C*          convert latitude and longitude to arc-seconds and then to UTM

            IF( DABS(YDMIN) .GT. 66.0D0 )THEN
               WRITE (DUMMY,'(f8.2)' ) YDMIN
               CALL ERRHDL(PATH,MODNAM,'W','360',DUMMY)
            END IF

            IF( DABS(YDMAX) .GT. 66.0D0 )THEN
               WRITE (DUMMY,'(f8.2)' ) YDMAX
               CALL ERRHDL(PATH,MODNAM,'W','360',DUMMY)
            END IF
            
C*          Calculate the Coords in UTM for SW and NE corners based on 
C*            Lat/Lon, relative to NADA ellipsoid;
C*            first convert to arc-seconds
            ARGE   =  XDMIN * 3600.0D0
            ARGN   =  YDMIN * 3600.0D0
            XARG   = 0.0D0
            YARG   = 0.0D0
            CALL UTMGEO (555,0,IZONE,XARG,YARG,ARGE,ARGN,ISPHERE)
            XDMIN  = XARG
            YDMIN  = YARG
            ZONMIN = IZONE

            ARGE   =  XDMAX * 3600.0D0
            ARGN   =  YDMAX * 3600.0D0
            XARG   = 0.0D0
            YARG   = 0.0D0
            CALL UTMGEO (555,0,IZONE,XARG,YARG,ARGE,ARGN,ISPHERE)
            XDMAX  = XARG
            YDMAX  = YARG
            ZONMAX = IZONE

         ELSE IF (DOMTYP .EQ. 'UTM') THEN

C*          Convert SW corner UTMs to Lat/Long relative to NADA ellipsoid
            XARG  = XDMIN
            YARG  = YDMIN
            IZONE = ZONMIN
            ARGE  = 0.0D0
            ARGN  = 0.0D0
            IZDUM = 0
            CALL UTMGEO (333,IZONE,IZDUM,XARG,YARG,ARGE,ARGN,ISPHERE)
            DOMLL(2,1) = ARGE
            DOMLL(1,1) = ARGN

C*          Convert back to UTM to check for zone change
            XARG   = 0.0D0
            YARG   = 0.0D0
            CALL UTMGEO (555,0,IZNEW,XARG,YARG,ARGE,ARGN,ISPHERE)
            IF (IZNEW .NE. IZONE) THEN
               DOMLL(2,1) = ARGE
               DOMLL(1,1) = ARGN
C*             Save new UTM coordinates and zone of domain SW corner
               XDMIN  = XARG
               YDMIN  = YARG
               ZONMIN = IZNEW
            END IF

C*          Convert NE corner UTMs to Lat/Long relative to NADA ellipsoid
            XARG  = XDMAX
            YARG  = YDMAX
            IZONE = ZONMAX
            ARGE  = 0.0D0
            ARGN  = 0.0D0
            IZDUM = 0
            CALL UTMGEO (333,IZONE,IZDUM,XARG,YARG,ARGE,ARGN,ISPHERE)
            DOMLL(2,3) = ARGE
            DOMLL(1,3) = ARGN

C*          Convert back to UTM to check for zone change
            XARG = 0.0D0
            YARG = 0.0D0
            CALL UTMGEO (555,0,IZNEW,XARG,YARG,ARGE,ARGN,ISPHERE)
            IF (IZNEW .NE. IZONE) THEN
               DOMLL(2,3) = ARGE
               DOMLL(1,3) = ARGN
C*             Save new UTM coordinates and zone of domain NE corner
               XDMAX  = XARG
               YDMAX  = YARG
               ZONMAX = IZNEW
            END IF

         END IF

C*       Make final conversion to DOMLL array
         XARG = XDMIN
         YARG = YDMIN
         IZONE = ZONMIN
         ARGE  = 0.0D0
         ARGN  = 0.0D0
         IZDUM = 0
         CALL UTMGEO (333,IZONE,IZDUM,XARG,YARG,ARGE,ARGN,ISPHERE)
         DOMLL(2,1) = ARGE
         DOMLL(1,1) = ARGN

         XARG = XDMAX
         YARG = YDMAX
         IZONE = ZONMAX
         ARGE  = 0.0D0
         ARGN  = 0.0D0
         IZDUM = 0
         CALL UTMGEO (333,IZONE,IZDUM,XARG,YARG,ARGE,ARGN,ISPHERE)
         DOMLL(2,3) = ARGE
         DOMLL(1,3) = ARGN

C*       Determine Lat/Lon of NW and SE corners based on UTMs from DOMAINXY
C*       Make final conversion to DOMLL array
C*       NW Corner:
         XARG = XDMIN
         YARG = YDMAX
         IZONE = ZONMIN
         ARGE  = 0.0D0
         ARGN  = 0.0D0
         IZDUM = 0
         CALL UTMGEO (333,IZONE,IZDUM,XARG,YARG,ARGE,ARGN,ISPHERE)
         DOMLL(2,2) = ARGE
         DOMLL(1,2) = ARGN

C*       SE Corner:
         XARG = XDMAX
         YARG = YDMIN
         IZONE = ZONMAX
         ARGE  = 0.0D0
         ARGN  = 0.0D0
         IZDUM = 0
         CALL UTMGEO (333,IZONE,IZDUM,XARG,YARG,ARGE,ARGN,ISPHERE)
         DOMLL(2,4) = ARGE
         DOMLL(1,4) = ARGN

C*       Determine NAD shift for Domain:
C*       KEY = +1 indicates a transformation of NAD 27 to NAD 83 datums
C*       KEY = -1 indicates a psuedo-transformation of NAD 83 to NAD 27 datums
         KEY = 1
         IF (NADA .GE. 2 .AND. NADA .LE. 4) KEY = -1

         IF (NADA .EQ. 0 .OR. .NOT.L_NeedNADCON) THEN
C*          Skip datum conversion, assign 0.0's to NAD shifts.
            KEY = 0
            XDMNDIFS = 0.0D0
            YDMNDIFS = 0.0D0
            XDMXDIFS = 0.0D0
            YDMXDIFS = 0.0D0

            XDMNDIFM = 0.0D0
            YDMNDIFM = 0.0D0
            XDMXDIFM = 0.0D0
            YDMXDIFM = 0.0D0

            ZONMIN_SHFT = ZONMIN
            ZONMAX_SHFT = ZONMAX

         ELSE
C*          Call NADCON 2.1 code to compute DATUM shifts and convert
C*          geographic coordinates between NAD27 and NAD83, and vice versa.
C*          XPTIN  & YPTIN  parameters are LON/LAT for NAD27;
C*          XPTOUT & YPTOUT parameters are LON/LAT for NAD83.

C*
C*          First calculate shift for SW Corner (XDMIN, YDMIN)
C*
            XPT27 = DOMLL(2,1)/3600.0D0
            YPT27 = DOMLL(1,1)/3600.0D0

            XPTIN = XPT27
            YPTIN = YPT27

            CALL NADCON(XPTIN,YPTIN,XPTOUT,YPTOUT,DLOS,DLAS,
     &                  DLOM,DLAM,KEY)
C*          Save shifts in geographic coordinates in arc-seconds.
C*          Full UTM shifts are computed below in corporating shifts
C*          due to ellipsoid.
            XDMNDIFS = DLOS * DBLE(key)
            YDMNDIFS = DLAS * DBLE(key)

            IF (KEY .EQ. 1) THEN
C*             Use _PTOUT variables from NADCON, which represent
C*             NAD83 converted FROM NAD27.  Get adjusted UTMs for
C*             NAD83 Datum using GRS80 ellipsoid

               ARGE = XPTOUT * 3600.0D0
               ARGN = YPTOUT * 3600.0D0
               XARG = 0.0D0
               YARG = 0.0D0
               CALL UTMGEO (555,0,IZONE,XARG,YARG,ARGE,ARGN,4)

            ELSE IF (KEY .EQ. -1) THEN
C*             Use _PTIN variables from NADCON, which represent
C*             NAD27 converted FROM NAD83.  Get adjusted UTMs for
C*             NAD27 Datum using Clarke 1866 ellipsoid

C*             Convert to arc-seconds for input to UTMGEO
               ARGE = XPTIN * 3600.0D0
               ARGN = YPTIN * 3600.0D0
               XARG = 0.0D0
               YARG = 0.0D0
               CALL UTMGEO (555,0,IZONE,XARG,YARG,ARGE,ARGN,0)

            END IF

C*          Now calculate full UTM shift including ellipsoid and datum shifts
            XDMNDIFM = XARG-XDMIN
            YDMNDIFM = YARG-YDMIN
            ZONMIN_SHFT = IZONE
C*
C*          Next calculate shift for NE Corner (XDMAX, YDMAX)
C*
            XPT27 = DOMLL(2,3)/3600.0D0
            YPT27 = DOMLL(1,3)/3600.0D0

            XPTIN = XPT27
            YPTIN = YPT27

            CALL NADCON(XPTIN,YPTIN,XPTOUT,YPTOUT,DLOS,DLAS,
     &                  DLOM,DLAM,KEY)
C*          Save shifts in geographic coordinates in arc-seconds.
C*          Full UTM shifts are computed below in corporating shifts
C*          due to ellipsoid.
            XDMXDIFS = DLOS * DBLE(key)
            YDMXDIFS = DLAS * DBLE(key)

            IF (KEY .EQ. 1) THEN
C*             Use _PTOUT variables from NADCON, which represent
C*             NAD83 converted FROM NAD27.  Get adjusted UTMs for
C*             NAD83 Datum using GRS80 ellipsoid

               ARGE = XPTOUT * 3600.0D0
               ARGN = YPTOUT * 3600.0D0
               XARG = 0.0D0
               YARG = 0.0D0
               CALL UTMGEO (555,0,IZONE,XARG,YARG,ARGE,ARGN,4)

            ELSE IF (KEY .EQ. -1) THEN
C*             Use _PTIN variables from NADCON, which represent
C*             NAD27 converted FROM NAD83.  Get adjusted UTMs for
C*             NAD27 Datum using Clarke 1866 ellipsoid

C*             Convert to arc-seconds for input to UTMGEO
               ARGE = XPTIN * 3600.0D0
               ARGN = YPTIN * 3600.0D0
               XARG = 0.0D0
               YARG = 0.0D0
               CALL UTMGEO (555,0,IZONE,XARG,YARG,ARGE,ARGN,0)

            END IF

C*          Now calculate full UTM shift including ellipsoid and datum shifts
            XDMXDIFM = XARG-XDMAX
            YDMXDIFM = YARG-YDMAX
            ZONMAX_SHFT = IZONE

         END IF

      END IF


C*    Open DOMDETAIL.OUT debug file
      OPEN(UNIT=DOMK, FILE=DOMDET_FILE, STATUS='REPLACE')
C*    Write Version Date Header to DOMDETAIL Debug File
      WRITE(DOMK,9011) VERSN, RUNDAT, RUNTIM
9011  FORMAT('** AERMAP - VERSION ',A5,T72,A8,/
     &          '**',T72,A8/)

      WRITE(DOMK,50) TYPDEM
  50  FORMAT('DOMCNV/CHKEXT:',//'TYPDEM = "',A3,'"')

C*    Write domain SW and NE corner coordinates in both lat/lon and UTMs;
C*    include UTM zone(s) and reference datum (NADA) from ANCHORXY keyword
      WRITE(DOMK,100) DOMLL(1,3), DOMLL(2,3), YDMAX, XDMAX, ZONMAX,NADA,
     &                DOMLL(1,1), DOMLL(2,1), YDMIN, XDMIN, ZONMIN,NADA,
     &                DOMLL(1,3)/3600.0D0, DOMLL(2,3)/3600.0D0,
     &                DOMLL(1,1)/3600.0D0, DOMLL(2,1)/3600.0D0
  100 FORMAT(
     &     /'        Domain                   LAT          LONG',
     &     25X,'UTM-N (m)',5X,'UTM-E (m)',3X,'ZONE',3X,'NADA',
     &                                 /40X,'(neg. for West long.)',
     &     /'  NE corner coordinates: ',2(F14.2),' seconds',10X,
     &                                  2(F14.4),2I6,
     &     /'  SW corner coordinates: ',2(F14.2),' seconds',10X,
     &                                  2(F14.4),2I6,
     &     /'  NE corner coordinates: ',2(F14.6),' decimal deg.',
     &     /'  SW corner coordinates: ',2(F14.6),' decimal deg.'//)

      WRITE(iounit,*) 'Exiting DOMCNV'
      WRITE(*,*) 'Exiting DOMCNV'      

      RETURN
      end subroutine

