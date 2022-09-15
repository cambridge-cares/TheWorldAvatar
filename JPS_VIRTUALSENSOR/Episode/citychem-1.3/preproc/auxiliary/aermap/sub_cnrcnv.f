       SUBROUTINE CNRCNV
C***********************************************************************
C*       CNRCNV Module of AERMAP Terrain Preprocessor
C*
C*       PURPOSE: Convert elevation file corner coordinates from
C*                native units (arc-seconds or meters) to the two 
C*                remaining formats (degrees and arc-seconds or meters).
C*                Store all three formats in corner arrays (e.g., SWE_MTRS, 
C*                NWE_MTRS, NEE_MTRS, and SEE_MTRS for UTM Easting values).
C*
C*                Arc-seconds and decimal degrees stored as negative 
C*                for west longitude.
C*
C*       PROGRAMMER: Clint Tillerson, MACTEC 
C*                   Roger Brode, US EPA, OAQPS, AQMG
C*
C*       DATE: February 9, 2009
C*
C*              * For DEM files with geographic coordinates, set reference
C*                utm zone (IZO) based on SW corner of file.
C*                
C*              * When converting corner coordinates in arc-seconds to UTM
C*                meters, nudge SW corner slightly to the east to account
C*                for precision and rounding in cases where the file boundary 
C*                coincides with a UTM boundary.  Precision and rounding
C*                during conversion can place the corner just over the zone
C*                boundary putting it in the next UTM zone.  Use the zone
C*                returned for the SW corner as the reference zone for the
C*                UTM coordinates for the remaining three corners of the file.
C*
C*              * Derive UTM zone for each corner of each file based on corner
C*                longitude.  Nudge all four corners toward the center of the
C*                file western corners to compensate for conversion precision.  
C*                These derived zones are used for validation purposes.
C* 
C*              * Validate ANCHORXY UTM zone against the derived corner UTM 
C*                zones of the DEM or NED files when the DOMAINXY or DOMAINLL
C*                keyword is not used.
C***********************************************************************

C     Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      CHARACTER*12  :: MODNAM
      
      LOGICAL  ::  L_MultiZones  ! logical flag to indicate the 
                                 ! DEM/NED data crosses multiple
                                 ! UTM zone boundaries; 
                                 ! will cause fatal error if no
                                 ! domain is specified
      
      INTEGER  ::  utm_zone ! utm zone function
      INTEGER  ::  ISPHERE  ! "spheroid" code for sub_UTMGEO
      INTEGER  ::  ZONMIN_DEM, ZONMAX_DEM ! min and max UTM zones
                                          ! based on data files
      
      ! temp variable for longitude argument to utm_zone function
      DOUBLE PRECISION  :: tmpLon
      
      MODNAM = 'CNRCNV'

C --- Initialize L_MultiZones flag
      L_MultiZones = .FALSE.

C --- Corner Arrays Reference:     
C     DMCNR(J,L,M): Corners in native units (meters or arc-seconds)
C                   read from elevation file, where:
C                   J: Elev data file index
C                   L: 1=Easting (Longitude), 2=Northing (Latitude)
C                   M: 1=SW, 2=NW, 3=NE, 4=SE
C
C     SWE_MTRS(J):  SW Corner, Easting Coordinate in meters where:
C                   J: Elev file index
C
C     SWN_MTRS(J):  SW Corner, Northing Coordinate in meters where:
C                   J: Elev file index
C
C     Similarly:    SWLAT_ARCS(J), SWLON_ARCS(J) - in arc-seconds
C                   SWLAT_DEGS(J), SWLON_DEGS(J) - in decimal degrees
C
C     Likewise there is a set of arrays for the NW, NE, and SE corners
C --- for the elevation files.
C
C     initialize corner arrays to 0
    
      ! meters
      SWE_MTRS = 0.0D0
      SWN_MTRS = 0.0D0
      NWE_MTRS = 0.0D0
      NWN_MTRS = 0.0D0
      NEE_MTRS = 0.0D0
      NEN_MTRS = 0.0D0
      SEE_MTRS = 0.0D0
      SEN_MTRS = 0.0D0
      
      ! arc-seconds
      SWLON_ARCS = 0.0D0
      SWLAT_ARCS = 0.0D0
      NWLON_ARCS = 0.0D0
      NWLAT_ARCS = 0.0D0
      NELON_ARCS = 0.0D0
      NELAT_ARCS = 0.0D0
      SELON_ARCS = 0.0D0
      SELAT_ARCS = 0.0D0
      
      ! degrees
      SWLAT_DEGS = 0.0D0
      SWLON_DEGS = 0.0D0
      NWLAT_DEGS = 0.0D0
      NWLON_DEGS = 0.0D0
      NELAT_DEGS = 0.0D0
      NELON_DEGS = 0.0D0
      SELAT_DEGS = 0.0D0
      SELON_DEGS = 0.0D0
      
      ! corner UTM Zones
      SW_UTMZ = 0
      NW_UTMZ = 0
      NE_UTMZ = 0
      SE_UTMZ = 0
            
      
      WRITE(MAPK,*) ' '
      WRITE(MAPK,*) 'MAP PARAMETERS'
      WRITE(MAPK,1) TYPDAT
    1 FORMAT(/1X,'TYPDAT: ',A5,/)
      
      WRITE(MAPK,*) 'From CNRCNV:'      

C     Determine spheroid to use when calling UTMGEO
      DEMLOOP: DO IDEM = 1, NUMDEM
      
         WRITE(MAPK,6) TYPDAT, IDEM,
     &                 TYPDAT, DEMFIL(IDEM)(1:LEN_TRIM(DEMFIL(IDEM)))
    6    FORMAT(/,1X,A3,' File #:    ',I6,
     &          /,1X,A3,' File Name: ',A)
    
         SELECT CASE(TYPDAT)
         CASE('NED')         
            SELECT CASE(NADD(IDEM))
            CASE(1,5:6)! NAD27
               ISPHERE = 0 ! Clarke 1866 ellipsoid
            CASE(2) ! WGS72
               ISPHERE = 4 ! GRS80 ellipsoid
            CASE(3) ! WGS84
               ISPHERE = 4 ! GRS80 ellipsoid
            CASE(4) ! NAD83
               ISPHERE = 4 ! GRS80 ellipsoid              
            CASE DEFAULT
               ISPHERE = 4 ! DEFAULT CASE shouldn't occur
            END SELECT
         
         CASE('DEM')
            SELECT CASE (NADD(IDEM))   ! Use datum for DEM file
            CASE (0)
               IF (IPLAN(IDEM) .EQ. 0) THEN
                  IF (DABS((DMCNR(IDEM,2,3)/3600.0D0) -
     &               (DMCNR(IDEM,2,4)/3600.0D0) - 1.0D0) .LE.
     &                                               0.001D0) THEN
C*                   Assume 1-Degree DEM; default WGS72, using GRS80 ellipsoid
                     ISPHERE = 4  ! UTM/LAT-LONG conversions based on GRS80 ellipsoid

                  ELSE
C*                   Assume 7.5-minute DEM; default NAD27, using Clarke 1866 ellipsoid
                     ISPHERE = 0  
                  END IF
                  
               ELSE IF (IPLAN(IDEM) .EQ. 1) THEN
C*                Assume 7.5-Minute DEM
                  ISPHERE = 0  ! UTM/LL conversions based on Clarke 1866 ellipsoid
               END IF
            CASE (2:4)
               ISPHERE = 4     ! UTM/LAT-LONG conversions based on GRS80 ellipsoid
            CASE (1,5:6)
               ISPHERE = 0     ! UTM/LL conversions based on Clarke 1866 ellipsoid
            CASE DEFAULT
               ISPHERE = 4     ! DEFAULT CASE shouldn't occur
            END SELECT
         END SELECT
            
C ---    If native units are in meters, copy coords in meters to corner arrays,
C        convert corner coord from meters to arc-seconds with UTMGEO, based on 
C        UTM zone defined for data file, IZO(idem).
C        CUNIT = 2 ==> meters
C
         IF (CUNIT(IDEM) .EQ. 2) THEN
         
C           Copy coords in meters to corner arrays
            SWE_MTRS(IDEM) = DMCNR(IDEM,1,1)
            SWN_MTRS(IDEM) = DMCNR(IDEM,2,1)
            NWE_MTRS(IDEM) = DMCNR(IDEM,1,2)
            NWN_MTRS(IDEM) = DMCNR(IDEM,2,2)
            NEE_MTRS(IDEM) = DMCNR(IDEM,1,3)
            NEN_MTRS(IDEM) = DMCNR(IDEM,2,3)
            SEE_MTRS(IDEM) = DMCNR(IDEM,1,4)
            SEN_MTRS(IDEM) = DMCNR(IDEM,2,4)

C           Call to UTMGEO to convert meters to arc-seconds
            ! southwest corner
            IZDUM = 0
            CALL UTMGEO (333,IZO(IDEM),IZDUM,
     &             SWE_MTRS(IDEM),SWN_MTRS(IDEM),
     &             SWLON_ARCS(IDEM),SWLAT_ARCS(IDEM),ISPHERE)  

            ! northwest corner            
            IZDUM = 0
            CALL UTMGEO (333,IZO(IDEM),IZDUM,
     &             NWE_MTRS(IDEM),NWN_MTRS(IDEM),
     &             NWLON_ARCS(IDEM),NWLAT_ARCS(IDEM),ISPHERE)
            
            ! northeast corner          
            IZDUM = 0
            CALL UTMGEO (333,IZO(IDEM),IZDUM,
     &             NEE_MTRS(IDEM),NEN_MTRS(IDEM),
     &             NELON_ARCS(IDEM),NELAT_ARCS(IDEM),ISPHERE)
            
            ! southeast corner          
            IZDUM = 0
            CALL UTMGEO (333,IZO(IDEM),IZDUM,
     &             SEE_MTRS(IDEM),SEN_MTRS(IDEM),
     &             SELON_ARCS(IDEM),SELAT_ARCS(IDEM),ISPHERE)

C ---    If native units are in arc-seconds, copy seconds to corner arrays,
C        convert corner coord from seconds to UTM (meters). Store in corner arrays.
C        (NED coordinates are originally in degrees but stored in arc-seconds) 
C        CUNIT = 3 ==> arc-seconds  
C         
         ELSE IF (CUNIT(IDEM) .EQ. 3) THEN

C           Copy arc-seconds into corner arrays. 
            SWLON_ARCS(IDEM) = DMCNR(IDEM,1,1)
            SWLAT_ARCS(IDEM) = DMCNR(IDEM,2,1)
            NWLON_ARCS(IDEM) = DMCNR(IDEM,1,2)
            NWLAT_ARCS(IDEM) = DMCNR(IDEM,2,2)
            NELON_ARCS(IDEM) = DMCNR(IDEM,1,3)
            NELAT_ARCS(IDEM) = DMCNR(IDEM,2,3)
            SELON_ARCS(IDEM) = DMCNR(IDEM,1,4)
            SELAT_ARCS(IDEM) = DMCNR(IDEM,2,4)
          

C ---       Call to UTMGEO to convert arcseconds to meters 
C           This could be NED or DEM files.  In either case,
C           set reference UTM zone (IZO) based on southwest corner.
C           For SW corner, input zone = zero, initialize IZO
C           to zero and pass as output zone.  Call UTMGEO for 
C           the SW corner first.  For all other corners, pass IZO 
C           set for SW corner as input zone and a dummy variable
C           initialized as zero as output zone.  All corner coordinates
C           will then be referenced to the same UTM zone (based on SW corner.)

C           Nudge SW corner longitude eastward to avoid picking neighboring 
C           zone due to limits of precision or rounding. Also nudge SW corner
C           latitude northward to avoid applying negative zone near equator.

            ! southwest corner; determine UTM zone based on SW corner, with nudge
            tmpLon = (SWLON_ARCS(IDEM)+nudge)/3600.0D0  ! nudge to E to avoid picking 
                                                        ! neighboring zone
            IZO(IDEM) = utm_zone(tmpLon)
            IF ((SWLAT_ARCS(IDEM)+nudge) .LT. 0.0D0) THEN
C ---          Adjust for South latitudes
               IZO(IDEM) = -1*IZO(IDEM)
            END IF
            IZDUM = 0
            CALL UTMGEO (555,IZO(IDEM),IZDUM,
     &             SWE_MTRS(IDEM),SWN_MTRS(IDEM),
     &             SWLON_ARCS(IDEM),SWLAT_ARCS(IDEM),ISPHERE)  

            ! northwest corner
            IZDUM = 0
            CALL UTMGEO (555,IZO(IDEM),IZDUM,
     &             NWE_MTRS(IDEM),NWN_MTRS(IDEM),
     &             NWLON_ARCS(IDEM),NWLAT_ARCS(IDEM),ISPHERE)

            ! northeast corner  
            IZDUM = 0  
            CALL UTMGEO (555,IZO(IDEM),IZDUM,
     &             NEE_MTRS(IDEM),NEN_MTRS(IDEM),
     &             NELON_ARCS(IDEM),NELAT_ARCS(IDEM),ISPHERE)

            ! southeast corner
            IZDUM = 0
            CALL UTMGEO (555,IZO(IDEM),IZDUM,
     &             SEE_MTRS(IDEM),SEN_MTRS(IDEM),
     &             SELON_ARCS(IDEM),SELAT_ARCS(IDEM),ISPHERE)
          
         END IF
         
C        Convert arc-seconds to decimal degrees. 
C        Store in corner arrays.
         SWLAT_DEGS(IDEM) = SWLAT_ARCS(IDEM)/3600.0D0 
         SWLON_DEGS(IDEM) = SWLON_ARCS(IDEM)/3600.0D0 
         NWLAT_DEGS(IDEM) = NWLAT_ARCS(IDEM)/3600.0D0 
         NWLON_DEGS(IDEM) = NWLON_ARCS(IDEM)/3600.0D0 
         NELAT_DEGS(IDEM) = NELAT_ARCS(IDEM)/3600.0D0 
         NELON_DEGS(IDEM) = NELON_ARCS(IDEM)/3600.0D0 
         SELAT_DEGS(IDEM) = SELAT_ARCS(IDEM)/3600.0D0 
         SELON_DEGS(IDEM) = SELON_ARCS(IDEM)/3600.0D0  

        
C*       Output Horizontal Datum
         WRITE(MAPK,410) NADN(NADD(IDEM)), TYPDAT
 410     FORMAT(/1X,'Horizontal Datum: ',a40,
     &         //1X,'Corner Coordinates of ',a3,' Data:')
            
C*       Output corner coordinates (degrees)
         WRITE(MAPK,510) SWLON_DEGS(IDEM),
     &                   NWLON_DEGS(IDEM),
     &                   NELON_DEGS(IDEM),
     &                   SELON_DEGS(IDEM),
     &                   SWLAT_DEGS(IDEM),NWLAT_DEGS(IDEM),
     &                   NELAT_DEGS(IDEM),SELAT_DEGS(IDEM)
 510     FORMAT(/1X,'Decimal Degrees (negative for West longitude):',
     &          /8X,'    SW Corner ',                       
     &              '    NW Corner ',                       
     &              '    NE Corner ',                       
     &              '    SE Corner',
     &          /3X,'Lon:',4F14.6/3X,'Lat:',4F14.6)
     
C*       Output corner coordinates (arc-seconds)
         WRITE(MAPK,520) SWLON_ARCS(IDEM),
     &                   NWLON_ARCS(IDEM),
     &                   NELON_ARCS(IDEM),
     &                   SELON_ARCS(IDEM),
     &                   SWLAT_ARCS(IDEM),NWLAT_ARCS(IDEM),
     &                   NELAT_ARCS(IDEM),SELAT_ARCS(IDEM)
 520     FORMAT(/1X,'Arc-seconds (negative for West longitude):',
     &          /8X,'    SW Corner ',                       
     &              '    NW Corner ',                       
     &              '    NE Corner ',                       
     &              '    SE Corner',
     &          /3X,'Lon:',4F14.3/3X,'Lat:',4F14.3)

C*       Output corner coordinates (meters)
         WRITE(MAPK,530) SWE_MTRS(IDEM),NWE_MTRS(IDEM),
     &                   NEE_MTRS(IDEM),SEE_MTRS(IDEM),
     &                   SWN_MTRS(IDEM),NWN_MTRS(IDEM),
     &                   NEN_MTRS(IDEM),SEN_MTRS(IDEM)
 530     FORMAT(/1X,'Meters:',
     &          /15X,'  SW Corner   ',
     &               '  NW Corner   ',
     &               '  NE Corner   ',
     &               '  SE Corner',
     &          /3X,'Easting: ',4F14.3/3X,'Northing:',4F14.3)

      
C----    Determine UTM zone for each corner of the current file 
C        using lat/lon in degrees.  This may duplicate the effort above
C        when the original coordinates in file are in arc-seconds.
C        These are used later to get min and max zones across all files 
C        processed for validation purposes.
C        Corner coordinates are nudged toward center of file to avoid
C        incorrect assignment of zones.

         ! Southwest corner; use UTM zone based on IZO(IDEM),
         ! determined from corner coordinates for geographic data and
         ! NED UTM data, and based on header record for DEM UTM data
          SW_UTMZ(IDEM) = IZO(IDEM)

         ! Northwest corner
         tmpLon = (NWLON_ARCS(IDEM)+nudge)/3600.0D0
         NW_UTMZ(IDEM) = utm_zone(tmpLon)
         IF (NWLAT_ARCS(IDEM)-nudge .LT. 0.0D0) THEN
C ---       Adjust zone for South latitudes 
            NW_UTMZ(IDEM) = -1*NW_UTMZ(IDEM)
         END IF
         
         ! Southeast corner
         tmpLon = (SELON_ARCS(IDEM)-nudge)/3600.0D0
         SE_UTMZ(IDEM) = utm_zone(tmpLon)
         IF (SELAT_ARCS(IDEM)+nudge .LT. 0.0D0) THEN
C ---       Adjust zone for South latitudes
            SE_UTMZ(IDEM) = -1*SE_UTMZ(IDEM)
         END IF
         
         ! Northeast corner
         tmpLon = (NELON_ARCS(IDEM)-nudge)/3600.0D0
         NE_UTMZ(IDEM) = utm_zone(tmpLon) 
         IF (NELAT_ARCS(IDEM)-nudge .LT. 0.0D0) THEN
C ---       Adjust zone for South latitudes
            NE_UTMZ(IDEM) = -1*NE_UTMZ(IDEM)
         END IF
         
C ---    Write DEM/NED file corner UTM zones to MAPPARAMS.OUT file
         IF (SW_UTMZ(IDEM) .EQ. SE_UTMZ(IDEM)) THEN
C ---       File does not cross UTM zone boundary; print SW corner zone
            WRITE(MAPK,540) SW_UTMZ(IDEM)
 540        FORMAT(3X,'UTM Zone: ',I4,' based on SW corner')
         ELSE
C ---       File spans zone boundary; print zone for all four corners
            WRITE(MAPK,541) SW_UTMZ(IDEM), NW_UTMZ(IDEM),
     &                      NE_UTMZ(IDEM), SE_UTMZ(IDEM)
 541        FORMAT(3X,'UTM Zones:',5X,I4,3(10X,I4))
C ---       Check for file that spans more than 2 zones, issue message
            IF (ABS(ABS(SW_UTMZ(IDEM))-ABS(SE_UTMZ(IDEM))) .LE. 1) THEN
C ---          UTM zones span only 1 zone boundary, add informational message
               WRITE(MAPK,542)
 542           FORMAT(/8X,'Note:  ',
     &      'UTM corner coordinates are referenced to SW corner zone.')
            ELSE
C ---          UTM zones span at least 2 zone boundaries, issue warning
C              Additional error handling for this is provided below.
               WRITE(MAPK,543)
 543           FORMAT(/8X,'Note:  ',
     &      'UTM corner coordinates are referenced to SW corner zone.',
     &   /,15X,'UTM coordinates may be erroneous for files that span',
     &   /,15X,'multiple zones. This should not affect results for',
     &   /,15X,'DEM/NED data in geographic coordinates.')
            END IF
         END IF
     
C ---    Assign UTM zone for file based on SW corner zone
         IZOND(IDEM) = IZO(IDEM)
              
      END DO DEMLOOP       ! end loop over elevation files (IDEM)

C---- If the domain was not specified in the input file, 
C     validate the utm zones to ensure the implied domain 
C     (defined by file boundaries) does not span more than
C     2 UTM zones.  Also check for consistency between data
C     zones and ANCHORXY zones.  These checks are made 
C     elsewhere for applications with user-specified DOMAIN

C --- Find min and max of the corner UTM zones from data.  
      ZONMIN_DEM = min(ABS(minval(SW_UTMZ)),ABS(minval(NW_UTMZ)),
     &                 ABS(minval(SE_UTMZ)),ABS(minval(NE_UTMZ)))
      ZONMAX_DEM = max(ABS(maxval(SW_UTMZ)),ABS(maxval(NW_UTMZ)),
     &                 ABS(maxval(SE_UTMZ)),ABS(maxval(NE_UTMZ)))

C---- Check for UTM zone range of DEM/NED data
      IF (ABS(ABS(ZONMAX_DEM)-ABS(ZONMIN_DEM)) .GT. 1 .AND.
     &  .NOT.((ABS(ZONMIN_DEM).EQ. 1 .AND. ABS(ZONMAX_DEM).EQ.60) .OR.
     &        (ABS(ZONMIN_DEM).EQ.60 .AND. ABS(ZONMAX_DEM).EQ. 1))) THEN
C*       DEM/NED data span more than 2 UTM zones
         WRITE(DUMMY,'(I3,2X,I3)') ZONMIN_DEM, ZONMAX_DEM
C ---    Assign L_MultiZones flag indicating that range of DEM/NED data
C        crosses multiple UTM zones; will cause error message if no
C        domain is specified
         L_MultiZones = .TRUE.
         IF (.NOT. GOTDOMFLG) THEN
C           Issue fatal error messages if no domain is defined 
            CALL ERRHDL(PATH,MODNAM,'E','232',DUMMY)
            CALL ERRHDL(PATH,MODNAM,'E','233','Zones   ')
         ELSE
C           Issue warning message if domain is defined; zone
C           consistency for applications with domains is checked
C           in subroutine DOMAIN
            CALL ERRHDL(PATH,MODNAM,'W','234',DUMMY)
         END IF
      END IF

C---- If no domain is specified, all DEM/NED data become the domain, 
C     check for consistency between DEM/NED zones and ANCHORXY zone
      IF (.NOT. GOTDOMFLG) THEN
C----    Next check for consistency between data UTM zones and ANCHORXY zone
         IF (ABS(ZONMIN_DEM) .EQ. ABS(ZONMAX_DEM) .AND. 
     &       ABS(ZONMIN_DEM) .EQ. ABS(ZATERR)) THEN
C----       No apparent conflict between data UTM zones and ANCHORXY zone
            GO TO 999
            
         ELSE IF ((ABS(ZONMIN_DEM) .EQ. ABS(ZONMAX_DEM)) .AND. 
     &            (ABS(ABS(ZONMIN_DEM)-ABS(ZATERR)) .LE. 1)) THEN
C----       No apparent conflict between data UTM zones and ANCHORXY zone
            GO TO 999
      
         ELSE IF ((ABS( ABS(ZONMAX_DEM)-ABS(ZONMIN_DEM) ) .EQ. 1) .AND.
     &            (ABS(ZATERR).EQ.ABS(ZONMIN_DEM) .OR. 
     &             ABS(ZATERR).EQ.ABS(ZONMAX_DEM))) THEN
C----       No apparent conflict between data UTM zones and ANCHORXY zone
            GO TO 999

         ELSE IF ((ABS(ZONMIN_DEM).EQ. 1 .AND.ABS(ZONMAX_DEM).EQ.60.AND.
     &            (ABS(ZATERR).EQ. 1 .OR. ABS(ZATERR).EQ.60))
     &       .OR. (ABS(ZONMIN_DEM).EQ.60 .AND.ABS(ZONMAX_DEM).EQ. 1.AND.
     &            (ABS(ZATERR).EQ. 1 .OR. ABS(ZATERR).EQ.60))) THEN
C----       UTM zones cross the 180E/180W meridian and ANCHORXY zone is ok
            GO TO 999
            
         ELSE IF (.NOT. L_MultiZones) THEN
            IF (ABS(ZATERR) .LT. ABS(ZONMIN_DEM) .OR.
     &          ABS(ZATERR) .GT. ABS(ZONMAX_DEM)) THEN            
C----          Apparent conflict between data UTM zones and ANCHORXY zone
               WRITE(DUMMY,'(I8)') ZATERR
               CALL ERRHDL(PATH,MODNAM,'E','242',DUMMY)
               WRITE(MAPK,544)
 544           FORMAT(
     &    /'ERROR:  UTM Zone specified on ANCHORXY keyword is beyond ',
     &    /'        the domain range defined by the data files.')
               GO TO 999
               
            END IF
            
         END IF
         
      END IF
       
999   CONTINUE
       
      end subroutine
            
      
      integer function utm_zone(lon)
      
c---- Calculate UTM zone based on longitude (degrees)

      implicit none
      double precision  :: lon  ! longitude (degrees)
      
      ! Treat west longitude as negative and east as positive.
      if (lon < 0.0D0) then
         utm_zone = idint((lon+180.0D0)/6.0D0)+1
      else
         utm_zone = idint(lon/6.0D0)+31
      end if
      
      return
      
      end function
