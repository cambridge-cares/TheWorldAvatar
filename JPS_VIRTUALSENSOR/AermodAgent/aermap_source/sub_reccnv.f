      SUBROUTINE RECCNV
C***********************************************************************
C*                RECCNV Module of AERMAP Terrain Preprocessor
C*
C*       PURPOSE: To Determine Receptor X,Y Coordinates in Lat/Long 
C*                and write to a Temporary File
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
C*       MODIFIED: December 7, 2006
C*                
C*                Corrected several problems related to NAD conversion
C*                process, procedure for optimizing critical hill height
C*                calculations for neighboring DEM files, and other issues.
C*                See header comments in AERMAP.FOR source file and
C*                AERMAP MCB#1 for more details.
C*                Roger W. Brode, U.S. EPA, OAQPS, AQMG
C*
C*       INPUTS:  Receptor coordinates in cartesian coords
C*
C*       OUTPUTS: Receptor Arrays in Lat/Long
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

      INTEGER ISPHERE, IZNEW, KEY, IREC

C*    Variable Initializations
      MODNAM = 'RECCNV'

      IF (RECDBG) THEN
         OPEN(UNIT=RECK, FILE=RECDET_FILE, STATUS='REPLACE')
C*       Write Version Date Header to RECDETAIL Debug File
         WRITE(RECK,9011) VERSN, RUNDAT, RUNTIM
9011     FORMAT('** AERMAP - VERSION ',A5,T72,A8,/
     &          '**',T72,A8/)

         WRITE(RECK,*) ''
         WRITE(RECK,*) 'Processing Receptor Location Data in RECCNV:'
         WRITE(RECK,*) '  Determining North American Datum (NAD) shift',
     &                 ' values in arc-seconds.'
         WRITE(RECK,*) '  Total shift (meters) includes both datum shi',
     &                 'ft and projection shift'
         WRITE(RECK,*) '  for Lat/Lon to UTM coordinates.'
         WRITE(RECK,*) ''

         IF (NADA .EQ. 0) THEN
            WRITE(RECK,*) '  User-specified NADA = 0:   No NAD shifts ',
     &                    'will be calculated.'
            WRITE(RECK,*) ''
         END IF
      END IF

C*    Select reference ellipsoid for UTM-to-GEO conversions, based on
C*    user-specified reference datum (NADA) for ANCHORXY location.
C*    If user specifies NADA = 0, then assume Clarke 1866 ellipsoid if
C*    7.5-minute/UTM DEMs are used, and assume GRS80 ellipsoid if
C*    1-degree/LAT-LON DEMs are used.

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
      
C*    Loop Over All Receptors
      DO IREC = 1, NUMREC

C*       First Convert Receptor Location to UTM coords
         XRECU(IREC) = AXR(IREC) - XAUSER + XATERR
         YRECU(IREC) = AYR(IREC) - YAUSER + YATERR

C ---    Convert UTMs to Lat/Long relative to NADA ellipsoid,
C        based on UTM zone from ANCHORXY keyword
         XARG = XRECU(IREC)
         YARG = YRECU(IREC)
         IZONR(IREC) = ZATERR
         IZDUM = 0
         ARGE  = 0.0D0
         ARGN  = 0.0D0
         CALL UTMGEO (333,IZONR(IREC),IZDUM,XARG,YARG,ARGE,ARGN,ISPHERE)

         RLON(IREC) = ARGE
         RLAT(IREC) = ARGN
C ---    Convert back to UTM to check for zone change
         XARG = 0.0D0
         YARG = 0.0D0
         CALL UTMGEO (555,0,IZNEW,XARG,YARG,ARGE,ARGN,ISPHERE)

         IF (IZNEW .NE. IZONR(IREC)) THEN
C ---       UTM zone for receptor doesn't match ANCHORXY zone         
            RLON(IREC) = ARGE
            RLAT(IREC) = ARGN
C*          Save new UTM coordinates and zone of receptor
            XRECU(IREC) = XARG
            YRECU(IREC) = YARG
            IZONR(IREC) = IZNEW            
         END IF

C        Determine NAD shift:
C        KEY = +1 indicates a transformation of NAD 27 to NAD 83 datums
C        KEY = -1 indicates a psuedo-transformation of NAD 83 to NAD 27 datums
         KEY = 1
         IF (NADA .GE. 2 .AND. NADA .LE. 4) KEY = -1

         XPT27 = RLON(IREC)/3600.0D0
         YPT27 = RLAT(IREC)/3600.0D0

         XPTIN = XPT27
         YPTIN = YPT27

         IF (NADA .EQ. 0 .OR. .NOT.L_NeedNADCON) THEN
C*          Skip datum conversion, assign 0.0's to NAD shifts.
            KEY = 0
            XRDIFS(IREC) = 0.0D0
            YRDIFS(IREC) = 0.0D0
            XRDIFM(IREC) = 0.0D0
            YRDIFM(IREC) = 0.0D0

            DATUMSHFT(IREC) = 0.0D0
            
            AMAG = 0.0D0

C*          Write message to debug file
            IF (IREC .EQ. 1 .AND. RECDBG) THEN
               WRITE(RECK,100) NADA
  100          FORMAT(3X,'No NAD conversions required. All files ',
     &                   'are consistent with NADA = ',I3/)
            END IF

         ELSE
C*          Call NADCON 2.1 code to compute DATUM shifts and convert
C*          geographic coordinates between NAD27 and NAD83, and vice versa.
C*          XPTIN  & YPTIN  parameters are LON/LAT for NAD27;
C*          XPTOUT & YPTOUT parameters are LON/LAT for NAD83.

            CALL NADCON(XPTIN,YPTIN,XPTOUT,YPTOUT,DLOS,DLAS,
     &                  DLOM,DLAM,KEY)
C ---       Save shifts in geographic coordinates in arc-seconds.
C           Full UTM shifts are computed below in corporating shifts
C           due to ellipsoid.
            XRDIFS(IREC) = DLOS * DBLE(key)
            YRDIFS(IREC) = DLAS * DBLE(key)

            IF (KEY .EQ. 1) THEN
C ---          Use _PTOUT variables from NADCON, which represent
C              NAD83 converted FROM NAD27.  Get adjusted UTMs for
C              NAD83 Datum using GRS80 ellipsoid.
C              Calculate adjustment relative to UTM Zone for receptor
C              prior to NAD adjustment.

C ---          Convert lat/lon to arc-seconds for input to UTMGEO
               ARGE = XPTOUT * 3600.0D0
               ARGN = YPTOUT * 3600.0D0
               IZDUM = 0
               XARG  = 0.0D0
               YARG  = 0.0D0
               CALL UTMGEO (555,IZONR(IREC),IZDUM,XARG,YARG,ARGE,ARGN,4)

            ELSE IF (KEY .EQ. -1) THEN
C ---          Use _PTIN variables from NADCON, which represent
C              NAD27 converted FROM NAD83.  Get adjusted UTMs for
C              NAD27 Datum using Clarke 1866 ellipsoid
C              Calculate adjustment relative to UTM Zone for receptor
C              prior to NAD adjustment.

C ---          Convert lat/lon to arc-seconds for input to UTMGEO
               ARGE = XPTIN * 3600.0D0
               ARGN = YPTIN * 3600.0D0
               IZDUM = 0
               XARG  = 0.0D0
               YARG  = 0.0D0
               CALL UTMGEO (555,IZONR(IREC),IZDUM,XARG,YARG,ARGE,ARGN,0)

            END IF

C           Now calculate full UTM shift including ellipsoid and datum shifts
            XRDIFM(IREC) = XARG-XRECU(IREC)
            YRDIFM(IREC) = YARG-YRECU(IREC)

C           Calculate total Datum shift in arc-seconds
            DATUMSHFT(IREC) = DSQRT(DLOS*DLOS + DLAS*DLAS)

C           Calculate total shift in meters (includes shift due to ellipsoid)
            AMAG = DSQRT(XRDIFM(IREC)**2 + YRDIFM(IREC)**2)

            IF (KEY .EQ. 1) THEN
C*             Assign adjusted coordinates for NAD27 to NAD83 conversion
               XPT27 = XPTIN
               YPT27 = YPTIN
               XPT83 = XPTOUT
               YPT83 = YPTOUT
            
               IF (RECDBG) THEN
                  WRITE(RECK,10) IREC, XPT27, YPT27,
     &                           XRDIFS(IREC), YRDIFS(IREC),
     &                           XRDIFM(IREC), YRDIFM(IREC),
     &                           XPT83, YPT83
  10              FORMAT(' REC# ',I6,'  X-,YPT27:', 2F20.8
     &                           /13X,' SHFT SECS', 2F20.8
     &                           /13X,' SHFT MTRS', 2F20.8
     &                           /13X,' X-,YPT83:', 2F20.8)
            
                  WRITE (RECK,122) DATUMSHFT(IREC)
 122              FORMAT(/13X,' DATUM SHIFT = ',F12.2,' SECONDS')
                  WRITE (RECK,12) AMAG
  12              FORMAT (13X,' TOTAL SHIFT = ',F12.2,' METERS'/)
            
               END IF
            
            ELSE IF (KEY .EQ. -1) THEN
C*             Assign adjusted coordinates for NAD83 to NAD27 conversion
               XPT27 = XPTIN
               YPT27 = YPTIN
               XPT83 = XPTOUT
               YPT83 = YPTOUT
            
               IF (RECDBG) THEN
                  WRITE(RECK,11) IREC, XPT83, YPT83,
     &                           XRDIFS(IREC), YRDIFS(IREC),
     &                           XRDIFM(IREC), YRDIFM(IREC),
     &                           XPT27, YPT27
  11              FORMAT(' REC# ',I6,'  X-,YPT83:', 2F20.8
     &                           /13X,' SHFT SECS', 2F20.8
     &                           /13X,' SHFT MTRS', 2F20.8
     &                           /13X,' X-,YPT27:', 2F20.8)
            
                  WRITE (RECK,122) DATUMSHFT(IREC)
                  WRITE (RECK,12) AMAG
            
               END IF
            
            END IF

         END IF

C*    End Loop Over Receptors
      END DO

      WRITE(iounit,*) 'Exiting RECCNV'
      WRITE(*,*) 'Exiting RECCNV'

      RETURN
      end subroutine
