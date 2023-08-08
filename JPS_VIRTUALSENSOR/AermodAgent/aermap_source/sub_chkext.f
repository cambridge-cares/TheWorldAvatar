      SUBROUTINE CHKEXT
C***********************************************************************
C*                CHKEXT Module of AERMAP Terrain Preprocessor
C*
C*       PURPOSE: Check to See That the Extent of the Receptors is Within 
C*                the Extents of the User-Specified Domain and the Domain 
C*                Itself is Within the Supplied Raw dem/user Terrain Files
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
C*                Modified to check for the domain extent based on the
C*                UTM corner coordinates for DEM or NED files with 
C*                boundaries that follow UTM lines when TYPDEM = 'UTM'. 
C*                Modified debug file output to include UTM zone for 
C*                receptors and sources relative to domain, Lat/Lon
C*                coordinates in both arc-sec and degrees, and column
C*                headers.  Modified for use of standard convention of 
C*                negative for West longitude.
C*
C*       MODIFIED: December 7, 2006
C*                
C*                Corrected several problems related to NAD conversion
C*                process, procedure for optimizing critical hill height
C*                calculations for neighboring DEM files, and other issues.
C*                See header comments in AERMAP.FOR source file and
C*                AERMAP MCB#1 for more details.
C*
C*       MODIFIED:   Correct error that can occur with 1-degree DEM data
C*                   when the number of sources exceeds the number of
C*                   receptors.  R.W. Brode, PES, 1/22/98
C*
C*       INPUTS:  Receptor coordinates in cartesian coords
C*
C*       OUTPUTS: Logical Flags Denoting Whether In or Out.
C*
C*       CALLED FROM:   MAIN
C***********************************************************************
        
C*    Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM
      INTEGER I, IDOM, IREC, ISRC, DOMIN(4)
      INTEGER ISPHERE, ZONSHFT, ZMINSHFT, ZMAXSHFT
      DOUBLE PRECISION DOMLLSHFT_1, DOMLLSHFT_2, NADFLG
      DOUBLE PRECISION DOMAINXSHFT, DOMAINYSHFT
      DOUBLE PRECISION RLONSHFT, RLATSHFT
      DOUBLE PRECISION XARG, YARG, ARGE, ARGN

C*    Variable Initializations
      MODNAM = 'CHKEXT'

      DO I = 1, 4
         DOMIN(I) = 0
      END DO

C*    Check to See if the Receptors are Within the User-Specified
C*    Domain after Converting Domain Extents to Lat/Long secs

C*    (If the Terrain Data is of Type 'LAT', then the Domain is in 
C*     Lat/Long. Otherwise, it is in UTM Coords and needs to be Converted
C*     Into Arc Secs)
 
C*    Check Receptors:
C*
      DO IREC = 1,NUMREC

         IF (RECDBG) THEN
            IF (TYPDEM .EQ. 'LAT') THEN
              IF (IREC .EQ. 1) THEN
                 WRITE(RECK,207) 
207              FORMAT(1X,'Checking Receptor Locations Relative to ',
     &                     'the Domain in CHKEXT;',/3X,'Domain and ',
     &                     'Receptor Coordinates in Arc-seconds and ',
     &                     'Degrees:'//18X,
     &       'REC   LAT (sec)   LONG (sec)    LAT (deg)    LONG (deg)'/)
              END IF
              WRITE(RECK,208) DOMLL(1,3), DOMLL(2,3), 
     &                        DOMLL(1,3)/3600.0D0, DOMLL(2,3)/3600.0D0
              WRITE(RECK,209) irec, rlat(irec), rlon(irec),
     &                        rlat(irec)/3600.0D0, rlon(irec)/3600.0D0
              WRITE(RECK,208) DOMLL(1,1), DOMLL(2,1),
     &                        DOMLL(1,1)/3600.0D0, DOMLL(2,1)/3600.0D0
              WRITE(RECK,*) ' '
208           FORMAT(1x,'DOM LAT/LON#: ',6X,2F12.3,2F14.7)
209           FORMAT(1x,'REC LAT/LON#: ',I6,2F12.3,2F14.7)
            
            ELSE IF (TYPDEM .EQ. 'UTM') THEN
              IF (IREC .EQ. 1) THEN
                 WRITE(RECK,210) 
210              FORMAT(1X,'Checking Receptor Locations Relative to ',
     &                     'the Domain in CHKEXT;',/3X,'Domain and ',
     &                     'Receptor Coordinates in UTM Meters:'//26X,
     &                     'REC  ZONE   UTM-Y (m)   UTM-X (m)'/)
              END IF
              WRITE(RECK,211) ZONMAX, YDMAX, XDMAX
              WRITE(RECK,212) IREC, IZONR(IREC), YRECU(IREC), 
     &                                           XRECU(IREC)
              WRITE(RECK,211) ZONMIN, YDMIN, XDMIN
              WRITE(RECK,*) ' '
211           FORMAT(1x,'DOM ZONE/NORTH/EAST#: ',6X,I6,2F12.3)
212           FORMAT(1x,'REC ZONE/NORTH/EAST#: ',2I6,2F12.3)
            END IF
         END IF

C*       If a Receptor is in the Domain, Set the Flag to True and keep looping
C          no flag is set within the condition statements. However, if a 
C          condition turns out to be false, a fatal message is written.
         IF (TYPDEM .EQ. 'LAT') THEN
            IF(RLAT(IREC) .GE. DOMLL(1,1) .AND.
     &         RLAT(IREC) .LE. DOMLL(1,2) .AND.
     &         RLON(IREC) .GE. DOMLL(2,1) .AND.
     &         RLON(IREC) .LE. DOMLL(2,3)) THEN

               CYCLE

            ELSE
C*             Write Fatal Message, Receptor outside domain
               WRITE (DUMMY,'(I8)') IREC
               CALL ERRHDL(PATH,MODNAM,'E','300',DUMMY)
            ENDIF
            
         ELSE IF (TYPDEM .EQ. 'UTM') THEN
            IF (ZONMIN .EQ. ZONMAX) THEN
               IF (XRECU(IREC) .GE. XDMIN .AND.
     &             XRECU(IREC) .LE. XDMAX .AND.
     &             YRECU(IREC) .GE. YDMIN .AND.
     &             YRECU(IREC) .LE. YDMAX) THEN

                  CYCLE

               ELSE
C*                Write Fatal Message, Receptor outside domain
                  WRITE (DUMMY,'(I8)') IREC
                  CALL ERRHDL(PATH,MODNAM,'E','300',DUMMY)
               ENDIF
            ELSE IF (IZONR(IREC) .EQ. ZONMIN) THEN
               IF (XRECU(IREC) .GE. XDMIN .AND.
     &             YRECU(IREC) .GE. YDMIN .AND.
     &             YRECU(IREC) .LE. YDMAX) THEN

                  CYCLE

                ELSE
C*                Write Fatal Message, Receptor outside domain
                  WRITE (DUMMY,'(I8)') IREC
                  CALL ERRHDL(PATH,MODNAM,'E','300',DUMMY)
               ENDIF
            ELSE IF (IZONR(IREC) .EQ. ZONMAX) THEN
               IF (XRECU(IREC) .LE. XDMAX .AND.
     &             YRECU(IREC) .GE. YDMIN .AND.
     &             YRECU(IREC) .LE. YDMAX) THEN

                  CYCLE

                ELSE
C*                Write Fatal Message, Receptor outside domain
                  WRITE (DUMMY,'(I8)') IREC
                  CALL ERRHDL(PATH,MODNAM,'E','300',DUMMY)
               ENDIF
            ENDIF
         ENDIF
         
      END DO

      IF (RECDBG) CLOSE(RECK)


C*    Check sources:
C*
      IF (NUMSRC .GT. 0) THEN

         DO ISRC = 1,NUMSRC

            IF (SRCDBG) THEN
             IF (TYPDEM .EQ. 'LAT') THEN
               IF (ISRC .EQ. 1) THEN
                  WRITE(SRCK,217) 
217               FORMAT(1X,'Checking Source Locations Relative to ',
     &                      'the Domain in CHKEXT;',/3X,'Domain and ',
     &                      'Source Coordinates in Arc-seconds and ',
     &                      'Degrees:'//18X,
     &       'SRC   LAT (sec)   LONG (sec)    LAT (deg)    LONG (deg)'/)
               END IF
               WRITE(SRCK,208) DOMLL(1,3), DOMLL(2,3), 
     &                         DOMLL(1,3)/3600.0D0, DOMLL(2,3)/3600.0D0
               WRITE(SRCK,219) isrc, slat(isrc), slon(isrc),
     &                         slat(isrc)/3600.0D0, slon(isrc)/3600.0D0
               WRITE(SRCK,208) DOMLL(1,1), DOMLL(2,1),
     &                         DOMLL(1,1)/3600.0D0, DOMLL(2,1)/3600.0D0
               WRITE(SRCK,*) ' '
219            FORMAT(1x,'SRC LAT/LON#: ',I6,2F12.3,2F14.7)
             END IF
             IF (TYPDEM .EQ. 'UTM') THEN
               IF (ISRC .EQ. 1) THEN
                  WRITE(SRCK,220) 
220               FORMAT(1X,'Checking Source Locations Relative to ',
     &                      'the Domain in CHKEXT;',/3X,'Domain and ',
     &                      'Source Coordinates in UTM Meters:'//26X,
     &                      'SRC  ZONE   UTM-Y (m)   UTM-X (m)'/)
               END IF
               WRITE(SRCK,211) ZONMAX, YDMAX, XDMAX
               WRITE(SRCK,222) ISRC, IZONS(ISRC), YSRCU(ISRC), 
     &                                            XSRCU(ISRC)
               WRITE(SRCK,211) ZONMIN, YDMIN, XDMIN
               WRITE(SRCK,*) ' '
222            FORMAT(1x,'SRC ZONE/NORTH/EAST#: ',2I6,2F12.3)
             END IF
            END IF

C*          If a Source is in the Domain, Set the Flag to True and keep looping
            IF (TYPDEM .EQ. 'LAT') THEN
               IF(SLAT(ISRC) .GE. DOMLL(1,1) .AND.
     &            SLAT(ISRC) .LE. DOMLL(1,2) .AND.
     &            SLON(ISRC) .GE. DOMLL(2,1) .AND.
     &            SLON(ISRC) .LE. DOMLL(2,3)) THEN

                  CYCLE
               
               ELSE
C*                Write Fatal Message, Source outside domain
                  CALL ERRHDL(PATH,MODNAM,'E','305',SRCID(ISRC))
               ENDIF

            ELSE IF (TYPDEM .EQ. 'UTM') THEN
               IF (ZONMIN .EQ. ZONMAX) THEN
                  IF (XSRCU(ISRC) .GE. XDMIN .AND.
     &                XSRCU(ISRC) .LE. XDMAX .AND.
     &                YSRCU(ISRC) .GE. YDMIN .AND.
     &                YSRCU(ISRC) .LE. YDMAX) THEN
     
                     CYCLE
                     
                  ELSE
C*                   Write Fatal Message, Source outside domain
                     CALL ERRHDL(PATH,MODNAM,'E','305',SRCID(ISRC))
                  ENDIF
               ELSE IF (IZONS(ISRC) .EQ. ZONMIN) THEN
                  IF (XSRCU(ISRC) .GE. XDMIN .AND.
     &                YSRCU(ISRC) .GE. YDMIN .AND.
     &                YSRCU(ISRC) .LE. YDMAX) THEN
     
                     CYCLE
                     
                  ELSE
C*                   Write Fatal Message, Source outside domain
                     CALL ERRHDL(PATH,MODNAM,'E','305',SRCID(ISRC))
                  ENDIF
               ELSE IF (IZONS(ISRC) .EQ. ZONMAX) THEN
                  IF (XSRCU(ISRC) .LE. XDMAX .AND.
     &                YSRCU(ISRC) .GE. YDMIN .AND.
     &                YSRCU(ISRC) .LE. YDMAX) THEN
     
                     CYCLE
                     
                  ELSE
C*                   Write Fatal Message, Source outside domain
                     CALL ERRHDL(PATH,MODNAM,'E','305',SRCID(ISRC))
                  ENDIF
               ENDIF

            ENDIF
         END DO

         IF (SRCDBG) CLOSE(SRCK)

      END IF

C*    Then Check to See if the Domain extents are Within the
C*    Terrain Files.  This is accomplished by assuming that if
C*    all the 4 domain corners are inside a DEM or USER Terrain 
C*    File, the Entire Domain is Inside the Extent of the Files

C*    Loop Over All 4 Domain Corners

C*    Check domain:
C*     
      DO IDOM = 1, 4

C*       If this Domain Corner hasn't Been Located Yet -
         IF (DOMIN(IDOM) .EQ. 0) THEN
         
            DO IDEM = 1, NUMDEM

C*             If this Domain Corner is in the Lat/Long Extent of this
C*             Terrain File, Set the Logical for this Domain Corner 
C*             to True and Quit Domain Loop
C*
C*             First determine whether to apply NAD shift to Domain
C*
               IF (NADA .EQ. 0 .OR. NADA .EQ. NADD(IDEM)) THEN

                  NADFLG = 0.0D0                ! No NAD shift needed

               ELSE IF( (NADA.EQ.1    .OR.  NADA.GE.5) .AND.
     &                  (NADD(IDEM).GE.2 .AND. NADD(IDEM).LE.4) )THEN

                  NADFLG = 1.0D0                ! Include NAD shift

               ELSE IF( (NADA.GE.2    .AND. NADA.LE.4) .AND.
     &                  (NADD(IDEM).EQ.1 .OR.  NADD(IDEM).GE.5) )THEN

                  NADFLG = 1.0D0                ! Include NAD shift

               ELSE

                  NADFLG = 0.0D0                ! No NAD shift needed

               END IF

C*             Apply NAD shift to DOMLL if necessary
               if (idom == 1) then              ! SW corner

                  DOMLLSHFT_1 = DOMLL(1,IDOM) + (YDMNDIFS*NADFLG)
                  DOMLLSHFT_2 = DOMLL(2,IDOM) + (XDMNDIFS*NADFLG)
                  IF (TYPDEM .EQ. 'UTM') THEN
C*                   Apply NAD shift to XDMIN/YDMIN for TYPDEM='UTM'
                     DOMAINXSHFT = XDMIN + (XDMNDIFM*NADFLG)
                     DOMAINYSHFT = YDMIN + (YDMNDIFM*NADFLG)
                     RLATSHFT    = DOMLL(1,IDOM) + (YDMNDIFS*NADFLG)
                     RLONSHFT    = DOMLL(2,IDOM) + (XDMNDIFS*NADFLG)
                     IF (NADFLG .EQ. 0.0D0) THEN
C*                      Use original domain zones if no NAD shift            
                        ZMINSHFT = ZONMIN
                        ZMAXSHFT = ZONMAX
                     ELSE
C*                      Use UTM zones based on shifted domain            
                        ZMINSHFT = ZONMIN_SHFT
                        ZMAXSHFT = ZONMAX_SHFT
                     END IF
                  END IF

               else if (idom == 2) then         ! NW corner

                  DOMLLSHFT_1 = DOMLL(1,IDOM) + (YDMXDIFS*NADFLG)
                  DOMLLSHFT_2 = DOMLL(2,IDOM) + (XDMNDIFS*NADFLG)
                  IF (TYPDEM .EQ. 'UTM') THEN
C*                   Apply NAD shift to XDMIN/YDMIN for TYPDEM='UTM'
                     DOMAINXSHFT = XDMIN + (XDMNDIFM*NADFLG)
                     DOMAINYSHFT = YDMAX + (YDMXDIFM*NADFLG)
                     RLATSHFT    = DOMLL(1,IDOM) + (YDMXDIFS*NADFLG)
                     RLONSHFT    = DOMLL(2,IDOM) + (XDMNDIFS*NADFLG)
                     IF (NADFLG .EQ. 0.0D0) THEN
C*                      Use original domain zones if no NAD shift            
                        ZMINSHFT = ZONMIN
                        ZMAXSHFT = ZONMAX
                     ELSE
C*                      Use UTM zones based on shifted domain            
                        ZMINSHFT = ZONMIN_SHFT
                        ZMAXSHFT = ZONMAX_SHFT
                     END IF
                  END IF

               else if (idom == 3) then         ! NE corner

                  DOMLLSHFT_1 = DOMLL(1,IDOM) + (YDMXDIFS*NADFLG)
                  DOMLLSHFT_2 = DOMLL(2,IDOM) + (XDMXDIFS*NADFLG)
                  IF (TYPDEM .EQ. 'UTM') THEN
C*                   Apply NAD shift to XDMIN/YDMIN for TYPDEM='UTM'
                     DOMAINXSHFT = XDMAX + (XDMXDIFM*NADFLG)
                     DOMAINYSHFT = YDMAX + (YDMXDIFM*NADFLG)
                     RLATSHFT    = DOMLL(1,IDOM) + (YDMXDIFS*NADFLG)
                     RLONSHFT    = DOMLL(2,IDOM) + (XDMXDIFS*NADFLG)
                     IF (NADFLG .EQ. 0.0D0) THEN
C*                      Use original domain zones if no NAD shift            
                        ZMINSHFT = ZONMIN
                        ZMAXSHFT = ZONMAX
                     ELSE
C*                      Use UTM zones based on shifted domain            
                        ZMINSHFT = ZONMIN_SHFT
                        ZMAXSHFT = ZONMAX_SHFT
                     END IF
                  END IF

               else if (idom == 4) then         ! SE corner

                  DOMLLSHFT_1 = DOMLL(1,IDOM) + (YDMNDIFS*NADFLG)
                  DOMLLSHFT_2 = DOMLL(2,IDOM) + (XDMXDIFS*NADFLG)
                  IF (TYPDEM .EQ. 'UTM') THEN
C*                   Apply NAD shift to XDMIN/YDMIN for TYPDEM='UTM'
                     DOMAINXSHFT = XDMAX + (XDMXDIFM*NADFLG)
                     DOMAINYSHFT = YDMIN + (YDMNDIFM*NADFLG)
                     RLATSHFT    = DOMLL(1,IDOM) + (YDMNDIFS*NADFLG)
                     RLONSHFT    = DOMLL(2,IDOM) + (XDMXDIFS*NADFLG)
                     IF (NADFLG .EQ. 0.0D0) THEN
C*                      Use original domain zones if no NAD shift            
                        ZMINSHFT = ZONMIN
                        ZMAXSHFT = ZONMAX
                     ELSE
C*                      Use UTM zones based on shifted domain            
                        ZMINSHFT = ZONMIN_SHFT
                        ZMAXSHFT = ZONMAX_SHFT
                     END IF
                  END IF

               end if

               IF (TYPDEM .EQ. 'UTM' .AND. 
     &            (((IDOM.EQ.1 .OR. IDOM.EQ.2) .AND.
     &                                   ZMINSHFT.NE.IZOND(IDEM))
     &         .OR.
     &             ((IDOM.EQ.3 .OR. IDOM.EQ.4) .AND.
     &                                   ZMAXSHFT.NE.IZOND(IDEM)))) THEN
C*                Domain zone doesn't match DEM zone;
C*                Convert domain Lat/Lon to UTMs, based on DEM file NADD and DEM Zone

                  SELECT CASE (NADD(IDEM))   ! Use datum for DEM file
                    CASE (0)
                      IF (IPLAN(IDEM) .EQ. 0) THEN
                         ISPHERE = 4  ! UTM/LAT-LONG conv. based on GRS80 ellipsoid
                      ELSE IF (IPLAN(IDEM) .EQ. 1) THEN
                         ISPHERE = 0  ! UTM/LL conv. based on Clarke 1866 ellipsoid
                      END IF
                    CASE (2:4)
                      ISPHERE = 4     ! UTM/LAT-LONG conv. based on GRS80 ellipsoid
                    CASE (1,5:6)
                      ISPHERE = 0     ! UTM/LL conv. based on Clarke 1866 ellipsoid
                    CASE DEFAULT
                      ISPHERE = 4     ! DEFAULT CASE shouldn't occur
                  END SELECT
               
                  ARGE = RLONSHFT
                  ARGN = RLATSHFT
                  XARG = 0.0D0
                  YARG = 0.0D0
C*                Pass DEM file zone (IZOND(IDEM)) to UTMGEO to get new receptor
C*                coordinates (XARG, YARG) referenced to IZOND(IDEM)).
                  IZDUM = 0
                  CALL UTMGEO (555,IZOND(IDEM),IZDUM,XARG,YARG,
     &                                               ARGE,ARGN,ISPHERE)
                  DOMAINXSHFT = XARG
                  DOMAINYSHFT = YARG
                  ZONSHFT = IZOND(IDEM)
                  
               ELSE IF (TYPDEM .EQ. 'UTM' .AND. 
     &                 ((IDOM.EQ.1 .OR. IDOM.EQ.2) .AND.
     &                                  ZMINSHFT.EQ.IZOND(IDEM))) THEN
                  ZONSHFT = ZMINSHFT
               ELSE IF (TYPDEM .EQ. 'UTM' .AND.
     &                 ((IDOM.EQ.3 .OR. IDOM.EQ.4) .AND.
     &                                  ZMAXSHFT.EQ.IZOND(IDEM))) THEN
                  ZONSHFT = ZMAXSHFT
               END IF

               WRITE(DOMK,225) IDOM, TYPDAT,
     &                         DEMFIL(IDEM)(1:LEN_TRIM(DEMFIL(IDEM)))
  225          FORMAT('       Checking domain corner ',I2,
     &                      ' relative to ',A3,' file: ',A/)

C ---          Output coords of northeast corner of elev file
C              lat, lon (arc-seconds then degrees) for LAT files
C              UTMx, UTMy, and zone for UTM files; also include NADD(IDEM)
               IF (TYPDEM .EQ. 'UTM' .AND. (IPLAN(IDEM) .EQ. 1 .AND.
     &              DABS(NWE_MTRS(IDEM)-SWE_MTRS(IDEM)) .LT. 
     &                                           0.05D0*DXM(IDEM).AND.
     &              DABS(NEE_MTRS(IDEM)-SEE_MTRS(IDEM)).LT.
     &                                           0.05D0*DXM(IDEM)))THEN

                  WRITE(DOMK,2241)
 2241             FORMAT(39X,'UTM-N (m)',7X,'UTM-E (m)',2X,'ZONE',
     &                                                  4X,'NAD')

C ---             Output NE corner coordinates in UTMs
                  WRITE(DOMK,2271) TYPDAT, NEN_MTRS(IDEM), 
     &                             NEE_MTRS(IDEM), IZOND(IDEM), 
     &                             NADD(IDEM)
 2271             FORMAT(7X,A3,' corner UTMN/UTME/ZON:',2F16.4,2I6)

C ---             Output domain UTMs
                  WRITE(DOMK,2281) DOMAINYSHFT, DOMAINXSHFT, ZONSHFT,
     &                             NADD(IDEM)
 2281             FORMAT('    Domain corner UTMN/UTME/ZON:',2F16.4,2I6)

C ---             Output SW corner coordinates in UTMs
                  WRITE(DOMK,2271) TYPDAT, SWN_MTRS(IDEM), 
     &                             SWE_MTRS(IDEM), IZOND(IDEM),
     &                             NADD(IDEM)

               ELSE

                  WRITE(DOMK,224)
 224              FORMAT(34X,'LAT (sec)',6X,'LONG (sec)',
     &                    6X,'LAT (deg)',6X,'LONG (deg)',4X,'NAD')

C ---             Output NE corner lat/lon
                  WRITE(DOMK,227) TYPDAT, NELAT_ARCS(IDEM), 
     &                  NELON_ARCS(IDEM), NELAT_DEGS(IDEM),
     &                  NELON_DEGS(IDEM), NADD(IDEM)
 227              FORMAT(7X,A3,' corner Lat/Lon:',2F16.2,2F16.6,I6)

C ---             Output domain lat/lon (arc-seconds and degrees)
                  WRITE(DOMK,228) DOMLLSHFT_1, DOMLLSHFT_2,
     &                  DOMLLSHFT_1/3600.0D0,  DOMLLSHFT_2/3600.0D0,
     &                  NADD(IDEM)
 228              FORMAT('    Domain corner Lat/Lon:',2F16.2,2F16.6,I6)

C ---             Output SW corner lat/lon
                  WRITE(DOMK,227) TYPDAT, SWLAT_ARCS(IDEM), 
     &                  SWLON_ARCS(IDEM), SWLAT_DEGS(IDEM),
     &                  SWLON_DEGS(IDEM), NADD(IDEM)

               END IF
               WRITE(DOMK,*) ' '

C ---          Check to see if domain is inside extent of elev file.   
               IF (TYPDEM .EQ. 'UTM' .AND. (IPLAN(IDEM) .EQ. 1 .AND.
     &              DABS(NWE_MTRS(IDEM)-SWE_MTRS(IDEM)) .LT. 
     &                                             0.05D0*DXM(IDEM).AND.
     &              DABS(NEE_MTRS(IDEM)-SEE_MTRS(IDEM)).LT.
     &                                             0.05D0*DXM(IDEM).AND.
     &            (DOMAINXSHFT .GE. SWE_MTRS(IDEM) .AND.
     &             DOMAINXSHFT .LE. NEE_MTRS(IDEM) .AND.
     &             DOMAINYSHFT .GE. SWN_MTRS(IDEM) .AND.
     &             DOMAINYSHFT .LE. NEN_MTRS(IDEM)))
     &         .OR.
     &            (DOMLLSHFT_1 .GE. SWLAT_ARCS(IDEM) .AND.
     &             DOMLLSHFT_1 .LE. NELAT_ARCS(IDEM) .AND.
     &             DOMLLSHFT_2 .GE. SWLON_ARCS(IDEM) .AND.
     &             DOMLLSHFT_2 .LE. NELON_ARCS(IDEM))) THEN
     
C*                Domain Point is INSIDE the DEM file
                  DOMIN(IDOM) = 1

                  WRITE(DOMK,226) IDOM, TYPDAT, IDEM, 
     &                            DEMFIL(IDEM)(1:LEN_TRIM(DEMFIL(IDEM)))
  226             FORMAT('Domain corner ',I2,' Inside ',A3,' file ',I5,
     &                                                           ': ',A)

                  IF (TYPDEM .EQ. 'UTM' .AND. (IPLAN(IDEM) .EQ. 1 .AND.
     &                         DABS(NWE_MTRS(IDEM)-SWE_MTRS(IDEM)) .LT. 
     &                                           0.05D0*DXM(IDEM) .AND.
     &                         DABS(NEE_MTRS(IDEM)-SEE_MTRS(IDEM)) .LT.
     &                                           0.05D0*DXM(IDEM))) THEN

                     WRITE(DOMK,2241)
                     
C ---                Output NE corner coordinates in UTMs
                     WRITE(DOMK,2271) TYPDAT, NEN_MTRS(IDEM), 
     &                                        NEE_MTRS(IDEM),
     &                                        IZOND(IDEM), NADD(IDEM)

C ---                Output domain UTMs
                     WRITE(DOMK,2281) DOMAINYSHFT, DOMAINXSHFT, ZONSHFT,
     &                                NADD(IDEM)

C ---                Output SW corner coordinates in UTMs
                     WRITE(DOMK,2271) TYPDAT, SWN_MTRS(IDEM), 
     &                                        SWE_MTRS(IDEM),
     &                                        IZOND(IDEM), NADD(IDEM)
                  ELSE

                     WRITE(DOMK,224)

C ---                Output NE corner lat/lon
                     WRITE(DOMK,227) TYPDAT, NELAT_ARCS(IDEM), 
     &                     NELON_ARCS(IDEM), NELAT_DEGS(IDEM),
     &                     NELON_DEGS(IDEM), NADD(IDEM)

C ---                Output domain lat/lon (arc-seconds and degrees)
                     WRITE(DOMK,228) DOMLLSHFT_1, DOMLLSHFT_2,
     &                     DOMLLSHFT_1/3600.0D0,  DOMLLSHFT_2/3600.0D0,
     &                     NADD(IDEM)
  
C ---                Output SW corner lat/lon
                     WRITE(DOMK,227) TYPDAT, SWLAT_ARCS(IDEM), 
     &                     SWLON_ARCS(IDEM), SWLAT_DEGS(IDEM),
     &                     SWLON_DEGS(IDEM), NADD(IDEM)

                  END IF

                  WRITE(DOMK,*) ' '
                  WRITE(DOMK,*) ' '

                  EXIT      ! Exit from DEM file loop
                 
               ELSE
               
C*                Domain Point is NOT inside the DEM file
                  DOMIN(IDOM) = 0
                  
               END IF

            END DO    ! Loop over DEM/NED files

         END IF

      END DO          ! Loop over domain corners
      
      DO IDOM = 1,4
C*       If This Domain Point is Not Inside Any Terrain File, 
C*       Then Write an Error Message and Continue.
         IF (DOMIN(IDOM) .EQ. 0) THEN
            WRITE (DUMMY,'(I8)') IDOM
            CALL ERRHDL(PATH,MODNAM,'E','310',DUMMY)
         ENDIF
      END DO

      WRITE(iounit,*) 'Exiting CHKEXT'
      WRITE(*,*) 'Exiting CHKEXT'

      RETURN
      end subroutine
