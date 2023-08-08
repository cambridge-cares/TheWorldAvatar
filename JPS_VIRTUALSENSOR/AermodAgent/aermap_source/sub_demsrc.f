      SUBROUTINE DEMSRC
C***********************************************************************
C*               DEMSRC Module of AERMAP Terrain Preprocessor
C*
C*       PURPOSE: Determine which source falls into which DEM file
C*
C*
C*       PROGRAMMER: Peter Eckhoff
C*
C*       DATE:    January 8, 2001
C*
C*       Revision History:
C*
C*       MODIFIED: April 13, 2011
C*                
C*       MODIFIED BY: Roger W. Brode, U.S. EPA, OAQPS, AQMG
C*
C*                Corrected write and format statements for sources not
C*                assigned to a terrain file to avoid runtime errors 
C*                when such a condition occurs.
C*
C*       MODIFIED: February 9, 2009
C*                
C*       MODIFIED BY: Roger W. Brode, U.S. EPA, OAQPS, AQMG
C*
C*                Modified to use source UTM's and file corner UTM's to
C*                check for source within file for NED data in UTM format.
C*                Incorporated checks for source being assigned to lower
C*                resolution DEM file before encountering a higher res
C*                file.  Modified "gap" check to allow for non-NAD-shift
C*                related gaps.  Modified for use of standard convention of 
C*                negative for West longitude.
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

      INTEGER, ALLOCATABLE :: NSRCD(:)
      
      INTEGER ISRC, JDEM, IZONSRC, ISPHERE
      
      DOUBLE PRECISION, PARAMETER :: EPSILON = 0.001D0
      DOUBLE PRECISION SLONSHFT, SLATSHFT
      DOUBLE PRECISION XSSMSHFT, YSSMSHFT
      DOUBLE PRECISION ARGE, ARGN, XARG, YARG
      DOUBLE PRECISION NADFLG

      LOGICAL GAPSRC
      
      ALLOCATE (NSRCD(NSRC))
      
      MODNAM = 'DEMSRC'

C*    Initialize logical variable for identifying "gap" sources
      GAPSRC = .FALSE.
      JDMS   = 0
      NSRCD  = 0

      IF (SRCDBG) THEN
C*       Open debug output file
         OPEN(SRCD, FILE = SRCNDEM_FILE, STATUS = 'REPLACE')
C*       Write Version Date Header to SRCNDEM Debug File
         WRITE(SRCD,9011) VERSN, RUNDAT, RUNTIM
9011     FORMAT('** AERMAP - VERSION ',A5,T72,A8,/
     &          '**',T72,A8/)
      END IF

C --- Loop through sources to identify DEM (or NED) files that contain
C     the source
      SRCLOOP:DO ISRC = 1, NUMSRC

C*       First loop through DEM files with no distance tolerance
C*       relative to the edges of DEM files

         DEMLOOP:DO IDEM = 1, NUMDEM

C*          Determine whether to apply NAD shift
            IF (NADA .EQ. 0 .OR. NADA .EQ. NADD(IDEM)) THEN

               NADFLG = 0.0D0                ! No NAD shift needed

            ELSE IF( (NADA.EQ.1    .OR.  NADA.GE.5) .AND.
     &               (NADD(IDEM).GE.2 .AND. NADD(IDEM).LE.4) )THEN

               NADFLG = 1.0D0                ! Include NAD shift

            ELSE IF( (NADA.GE.2    .AND. NADA.LE.4) .AND.
     &               (NADD(IDEM).EQ.1 .OR.  NADD(IDEM).GE.5) )THEN

               NADFLG = 1.0D0                ! Include NAD shift

            ELSE

               NADFLG = 0.0D0                ! No NAD shift needed

            END IF

C*          Apply NAD shift if necessary; direction of shift is included in XSDIFS/YSDIFS
            SLONSHFT = SLON(ISRC) + (XSDIFS(ISRC)*NADFLG)
            SLATSHFT = SLAT(ISRC) + (YSDIFS(ISRC)*NADFLG)

            XSSMSHFT = XSRCU(ISRC) + (XSDIFM(ISRC)*NADFLG)
            YSSMSHFT = YSRCU(ISRC) + (YSDIFM(ISRC)*NADFLG)

            IF (IZONS(ISRC).NE.IZOND(IDEM)) THEN
C*             Source zone doesn't match DEM zone;
C*             Convert source Lat/Lon to UTMs, based on DEM file NADD and DEM Zone

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

               ARGE = SLONSHFT
               ARGN = SLATSHFT
               XARG = 0.0D0
               YARG = 0.0D0
C*             Pass DEM file zone (IZOND(IDEM)) to UTMGEO to get new source
C*             coordinates (XARG, YARG) referenced to IZOND(IDEM)).
               CALL UTMGEO (555,IZOND(IDEM),IZONSRC,XARG,YARG,
     &                                              ARGE,ARGN,ISPHERE)
               XSSMSHFT = XARG
               YSSMSHFT = YARG
            END IF

            IF (SRCDBG) THEN
               IF (IDEM .EQ. 1) THEN
                 WRITE(SRCD,730) TYPDAT, ISRC
 730             FORMAT('LOOKING AT ',A3,'s FOR SRC: ',I7)
                 WRITE(SRCD,731) ISRC, SLAT(ISRC)/3600.0D0,
     &                                 SLON(ISRC)/3600.0D0,
     &                                 YSDIFS(ISRC), XSDIFS(ISRC)
 731             FORMAT('SRC#:',I7,'    SLAT/LON:', 2F20.8,
     &                         /13X,'SHIFTL/L(S):', 2F20.8)
                 WRITE(SRCD,73) XSRCU(ISRC), YSRCU(ISRC), IZONS(ISRC),
     &                          XSDIFM(ISRC), YSDIFM(ISRC),
     &                          NADA
 73              FORMAT(12X,'UTM(E/N/Zon):', 2F20.3,1X,I4/
     &                  12X,' SHIFTX/Y(M):', 2F20.8,3X,'SRC NAD:',I3)
               ELSE IF (IDEM .GT. 1 .AND. NSRCD(ISRC) .EQ. 0) THEN
                 WRITE(SRCD,732) TYPDAT, ISRC
 732             FORMAT('STILL LOOKING AT ',A3,'s FOR SRC: ',I7)
               END IF

               IF (NSRCD(ISRC) .EQ. 0) THEN
C ---           File containing source has not been located yet
                IF (IPLAN(IDEM) .EQ. 1 .AND.
     &             (DABS(NWE_MTRS(IDEM)-SWE_MTRS(IDEM)) .LT. 
     &                                            0.05D0*DXM(IDEM)).AND.
     &             (DABS(NEE_MTRS(IDEM)-SEE_MTRS(IDEM)) .LT. 
     &                                            0.05D0*DXM(IDEM)))THEN
C ---             Current file boundaries follow UTM coordinates, 
C                 print corner coordinates in degrees and UTM meters
                  WRITE(SRCD,*)
                  WRITE(SRCD,72) TYPDAT, IDEM, TYPDAT, MAPNAME(IDEM), 
     &                           TYPDAT, NADD(IDEM)
                  WRITE(SRCD,74) TYPDAT, NELAT_DEGS(IDEM),
     &                                   NELON_DEGS(IDEM)
                  WRITE(SRCD,75) SLATSHFT/3600.0D0, SLONSHFT/3600.0D0
                  WRITE(SRCD,76) TYPDAT, SWLAT_DEGS(IDEM), 
     &                                   SWLON_DEGS(IDEM)
                  WRITE(SRCD,*)
                  WRITE(SRCD,77) TYPDAT, NEE_MTRS(IDEM),
     &                                   NEN_MTRS(IDEM)
                  WRITE(SRCD,78) XSSMSHFT, YSSMSHFT
                  WRITE(SRCD,79) TYPDAT, SWE_MTRS(IDEM),
     &                                   SWN_MTRS(IDEM)
 72                FORMAT(3X,A3,' No.:',I5,1X,A3,' Name: ',A40,
     &                       1X,A3,' NAD:', I3)
 74                FORMAT(8X,A3,' Max. LAT/LON(D.D):', 2F15.5)
 75                FORMAT(8X,'     Src LAT/LON(D.D):', 2F15.5)
 76                FORMAT(8X,A3,' Min. LAT/LON(D.D):', 2F15.5)
 77                FORMAT(8X,A3,' Max. UTME/UTMN(M):', 2F15.3)
 78                FORMAT(8X,'     Src UTME/UTMN(M):', 2F15.3)
 79                FORMAT(8X,A3,' Min. UTME/UTMN(M):', 2F15.3/)
                ELSE
C ---             Current file boundaries follow geographic coordinates, 
C                 print corner coordinates in degrees and arc-seconds
                  WRITE(SRCD,*)
                  WRITE(SRCD,72) TYPDAT, IDEM, TYPDAT, MAPNAME(IDEM), 
     &                           TYPDAT, NADD(IDEM)
                  WRITE(SRCD,74) TYPDAT, NELAT_DEGS(IDEM),
     &                                   NELON_DEGS(IDEM)
                  WRITE(SRCD,75) SLATSHFT/3600.0D0, SLONSHFT/3600.0D0
                  WRITE(SRCD,76) TYPDAT, SWLAT_DEGS(IDEM), 
     &                                   SWLON_DEGS(IDEM)
                  WRITE(SRCD,*)
                  WRITE(SRCD,87) TYPDAT, NELAT_ARCS(IDEM),
     &                                   NELON_ARCS(IDEM)
                  WRITE(SRCD,88) SLATSHFT, SLONSHFT
                  WRITE(SRCD,89) TYPDAT, SWLAT_ARCS(IDEM),
     &                                   SWLON_ARCS(IDEM)
 87                FORMAT(8X,A3,' Max. LAT/LON(Sec):', 2F15.5)
 88                FORMAT(8X,'     Src LAT/LON(Sec):', 2F15.5)
 89                FORMAT(8X,A3,' Min. LAT/LON(Sec):', 2F15.5/)
                END IF
               END IF
            END IF

C ---       Determine Which Terrain File(s) a source falls within
C           If file boundaries follow UTM coordinates, compare source
C           UTM to corner UTM's; otherwise compare source Lat/Lon 
C           to corner Lat/Lon
            IF((IPLAN(IDEM) .EQ. 1 .AND.
     &           DABS(NWE_MTRS(IDEM)-SWE_MTRS(IDEM)) .LT. 
     &                                             0.05D0*DXM(IDEM).AND.
     &           DABS(NEE_MTRS(IDEM)-SEE_MTRS(IDEM)).LT.
     &                                             0.05D0*DXM(IDEM).AND.
     &          (XSSMSHFT .GE. SWE_MTRS(IDEM) .AND.
     &           XSSMSHFT .LE. NEE_MTRS(IDEM) .AND.
     &           YSSMSHFT .GE. SWN_MTRS(IDEM) .AND.
     &           YSSMSHFT .LE. NEN_MTRS(IDEM)))
     &        .OR.
     &          (SLONSHFT .GE. SWLON_ARCS(IDEM) .AND.
     &           SLONSHFT .LE. NELON_ARCS(IDEM) .AND.
     &           SLATSHFT .GE. SWLAT_ARCS(IDEM) .AND.
     &           SLATSHFT .LE. NELAT_ARCS(IDEM))) THEN

               IF (JDMS(ISRC) .EQ. 0) THEN
C*                This is the first file that contains the source;
C*                assign file index to JDMS and ISIN arrays.
                  JDMS(ISRC) = IDEM

C*                Set number of files containing source to 1
                  NSRCD(ISRC) = 1

C*                Set ISIN array element for this source and 
C*                DEM file to 1
                  ISIN(ISRC,IDEM) = 1

                  IF (SRCDBG) THEN
C*                   Write message to SRCNDEM debug file
                     WRITE(SRCD,80) TYPDAT, IDEM, ISRC
 80                  FORMAT(A3,' No.:',I5,' Contains Src No.:',
     &                       I7/)
                  END IF
                  
               ELSE
C*                Source falls inside multiple DEM files
                  NSRCD(ISRC) = NSRCD(ISRC) + 1

C*                Set ISIN array element for this source and 
C*                DEM file to 1
                  ISIN(ISRC,IDEM) = 1

C ---             Check for lower resolution file preceding higher resolution file
C                 Use latitude spacing (DYM) for comparisons since this doesn't 
C                 change for higher latitudes
                  IF (IPLAN(JDMS(ISRC)).EQ.0 .AND. 
     &                                        IPLAN(IDEM).EQ.0) THEN
C ---                Source DEM and current DEM both geograhpic     
                     IF (DYM(JDMS(ISRC)) .GT. DYM(IDEM)+EPSILON) THEN
                     
                        CALL ERRHDL(PATH,MODNAM,'E','327',SRCID(ISRC))
                     END IF
                  ELSE IF (IPLAN(JDMS(ISRC)).EQ.1 .AND.
     &                              IPLAN(IDEM).EQ.1) THEN
C ---                Source DEM and current DEM both UTM
                     IF (DYM(JDMS(ISRC)) .GT. DYM(IDEM)+EPSILON) THEN
                     
                        CALL ERRHDL(PATH,MODNAM,'E','327',SRCID(ISRC))
                     END IF
                  ELSE IF (IPLAN(JDMS(ISRC)) .EQ. 0) THEN
C ---                Source DEM geographic but current DEM is UTM; 
C                    adjust spacing units                   
                     IF (DABS(DYM(JDMS(ISRC))-(1.0D0/9.0D0)) .LT. 
     &                                              EPSILON) THEN
                        IF (3.0D0 .GT. DYM(IDEM)+EPSILON) THEN
                           CALL ERRHDL(PATH,MODNAM,'E','327',
     &                                                    SRCID(ISRC))
                        END IF                         
                     ELSE IF (DABS(DYM(JDMS(ISRC))-(1.0D0/3.0D0)) .LT. 
     &                                                   EPSILON) THEN
                        IF (10.0D0 .GT. DYM(IDEM)+EPSILON) THEN
                           CALL ERRHDL(PATH,MODNAM,'E','327',
     &                                                    SRCID(ISRC))
                        END IF                         
                     ELSE IF (DABS(DYM(JDMS(ISRC))-1.0D0) .LT. 
     &                                                   EPSILON) THEN
                        IF (30.0D0 .GT. DYM(IDEM)+EPSILON) THEN
                           CALL ERRHDL(PATH,MODNAM,'E','327',
     &                                                    SRCID(ISRC))
                        END IF                         
                     ELSE IF (DABS(DYM(JDMS(ISRC))-2.0D0) .LT. 
     &                                                   EPSILON) THEN
                        IF (60.0D0 .GT. DYM(IDEM)+EPSILON) THEN
                           CALL ERRHDL(PATH,MODNAM,'E','327',
     &                                                    SRCID(ISRC))
                        END IF                         
                     ELSE IF (DABS(DYM(JDMS(ISRC))-3.0D0) .LT. 
     &                                                   EPSILON) THEN
                        IF (90.0D0 .GT. DYM(IDEM)+EPSILON) THEN
                           CALL ERRHDL(PATH,MODNAM,'E','327',
     &                                                    SRCID(ISRC))
                        END IF                         
                     END IF
                  ELSE IF (IPLAN(JDMS(ISRC)) .EQ. 1) THEN
C ---                Source DEM is UTM but current DEM is geographic; 
C                    adjust spacing units
                     IF (DABS(DYM(JDMS(ISRC))-3.0D0) .LT. 
     &                                              EPSILON) THEN
                        IF ((1.0D0/9.0D0) .GT. DYM(IDEM)+EPSILON) THEN
                           CALL ERRHDL(PATH,MODNAM,'E','327',
     &                                                    SRCID(ISRC))
                        END IF                         
                     ELSE IF (DABS(DYM(JDMS(ISRC))-10.0D0) .LT. 
     &                                                   EPSILON) THEN
                        IF ((1.0D0/3.0D0) .GT. DYM(IDEM)+EPSILON) THEN
                           CALL ERRHDL(PATH,MODNAM,'E','327',
     &                                                    SRCID(ISRC))
                        END IF                         
                     ELSE IF (DABS(DYM(JDMS(ISRC))-30.0D0) .LT. 
     &                                                   EPSILON) THEN
                        IF (1.0D0 .GT. DYM(IDEM)+EPSILON) THEN
                           CALL ERRHDL(PATH,MODNAM,'E','327',
     &                                                    SRCID(ISRC))
                        END IF                         
                     ELSE IF (DABS(DYM(JDMS(ISRC))-90.0D0) .LT. 
     &                                                   EPSILON) THEN
                        IF (3.0D0 .GT. DYM(IDEM)+EPSILON) THEN
                           CALL ERRHDL(PATH,MODNAM,'E','327',
     &                                                    SRCID(ISRC))
                        END IF                         
                     END IF
                  END IF

                  IF (SRCDBG) THEN
                     IF (IPLAN(IDEM) .EQ. 1 .AND.
     &                  (DABS(NWE_MTRS(IDEM)-SWE_MTRS(IDEM)) .LT. 
     &                                            0.05D0*DXM(IDEM)).AND.
     &                  (DABS(NEE_MTRS(IDEM)-SEE_MTRS(IDEM)) .LT. 
     &                                            0.05D0*DXM(IDEM)))THEN
C ---                   Current file boundaries follow UTM coordinates, 
C                       print corner coordinates in degrees and UTM meters
                        WRITE(SRCD,*)
                        WRITE(SRCD,72) TYPDAT, IDEM, TYPDAT, 
     &                                 MAPNAME(IDEM), TYPDAT, NADD(IDEM)
                        WRITE(SRCD,74) TYPDAT, NELAT_DEGS(IDEM),
     &                                         NELON_DEGS(IDEM)
                        WRITE(SRCD,75) SLATSHFT/3600.0D0, 
     &                                 SLONSHFT/3600.0D0
                        WRITE(SRCD,76) TYPDAT, SWLAT_DEGS(IDEM), 
     &                                         SWLON_DEGS(IDEM)
                        WRITE(SRCD,*)
                        WRITE(SRCD,77) TYPDAT, NEE_MTRS(IDEM),
     &                                         NEN_MTRS(IDEM)
                        WRITE(SRCD,78) XSSMSHFT, YSSMSHFT
                        WRITE(SRCD,79) TYPDAT, SWE_MTRS(IDEM),
     &                                         SWN_MTRS(IDEM)
                     ELSE
C ---                   Current file boundaries follow geographic coordinates, 
C                       print corner coordinates in degrees and arc-seconds
                        WRITE(SRCD,*)
                        WRITE(SRCD,72) TYPDAT, IDEM, TYPDAT, 
     &                                 MAPNAME(IDEM), TYPDAT, NADD(IDEM)
                        WRITE(SRCD,74) TYPDAT, NELAT_DEGS(IDEM),
     &                                         NELON_DEGS(IDEM)
                        WRITE(SRCD,75) SLATSHFT/3600.0D0,
     &                                 SLONSHFT/3600.0D0
                        WRITE(SRCD,76) TYPDAT, SWLAT_DEGS(IDEM), 
     &                                         SWLON_DEGS(IDEM)
                        WRITE(SRCD,*)
                        WRITE(SRCD,87) TYPDAT, NELAT_ARCS(IDEM),
     &                                         NELON_ARCS(IDEM)
                        WRITE(SRCD,88) SLATSHFT, SLONSHFT
                        WRITE(SRCD,89) TYPDAT, SWLAT_ARCS(IDEM),
     &                                         SWLON_ARCS(IDEM)
                     END IF
                     WRITE(SRCD,81) TYPDAT, IDEM, ISRC
 81                  FORMAT(A3,' No.:',I5,' Also Contains Src No.:',
     &                        I7/)
                  END IF
               END IF

            END IF

         END DO DEMLOOP

         IF (NSRCD(ISRC) .GT. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'W','333',SRCID(ISRC))
            
         END IF

      END DO SRCLOOP

C*    Check for any Sources Not in Any DEM File; Generate warnings if found
C*    Generate warnings if found
C*    First initialize NSGAP variable
      NSGAP = 0
      DO ISRC = 1, NUMSRC
         IF (JDMS(ISRC) .EQ. 0) THEN
C*          Assign logical variable to indicate that "gap" sources found
            GAPSRC = .TRUE.
            NSGAP  = NSGAP + 1

C*          Write Warning Message for Gap Sources on First Pass
C*          If DEM file not found on second pass, Fatal error will be generated
            CALL ERRHDL(PATH,MODNAM,'W','335',SRCID(ISRC))


            IF (SRCDBG) THEN
               IF (NSGAP .EQ. 1) THEN
                 WRITE(SRCD,*) ' '
                 WRITE(SRCD,*)
     &                        '*** SOURCES NOT ASSIGNED TO FILE - ',
     &                                                  'INITIAL PASS:'
                 WRITE(SRCD,*) ' '
               END IF
               WRITE(SRCD,90) ISRC, XSRCU(ISRC),YSRCU(ISRC),IZONS(ISRC),
     &                        SLAT(ISRC)/3600.0D0, SLON(ISRC)/3600.0D0,
     &                        NADA
 90            FORMAT('SRC#: ',I5,'  UTM(E/N/Z):', 2F20.3, I4/
     &                             16X,'Lat/Lon:', 2F20.7, '  NAD: ',I2)
            END IF

         END IF
      END DO

C*
C*    If DEM files are found for all sources (GAPSRC = F), Skip to bottom
      IF (.NOT. GAPSRC) GO TO 999
C*
      WRITE(IOUNIT,91) TYPDAT, NSGAP
 91   FORMAT(/,'******',
     &       /'DEMSRC:  Reprocessing ',A3,'s for ',I7,'  "Gap" ',
     &                                                'Sources!',
     &       /'******'/)
      IF (SRCDBG) THEN
         WRITE(SRCD,91) TYPDAT, NSGAP
      END IF

C*    Reprocess for "Gap" sources without NAD adjustment to 
C*    determine if sources are located within NAD-shift gaps

      DO ISRC = 1, NUMSRC

         IF (JDMS(ISRC) .EQ. 0) THEN
C           DEM not found for this source yet

            GAPLOOP1:DO IDEM = 1, NUMDEM

C ---          Determine whether a "gap" source falls within
C              a terrain file without applying NAD conversion;
C              this means that the source falls within a "gap"
C              created by the NAD shift.

C ---          First adjust UTM's for Zone change if needed
               IF (IZONS(ISRC) .NE. IZOND(IDEM)) THEN
C*                Source zone doesn't match DEM zone;
C*                Convert source Lat/Lon to UTMs, based on DEM file NADD and DEM Zone

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
         
                  ARGE = SLON(ISRC)
                  ARGN = SLAT(ISRC)
                  XARG = 0.0D0
                  YARG = 0.0D0
C*                Pass DEM file zone (IZOND(IDEM)) to UTMGEO to get new source
C*                coordinates (XARG, YARG) referenced to IZOND(IDEM)).
                  CALL UTMGEO (555,IZOND(IDEM),IZONSRC,XARG,YARG,
     &                                                ARGE,ARGN,ISPHERE)
C ---             Use XSSMSHFT and YSSMSHFT variables, but these only reflect
C                 shift for UTM zone, not for NAD
                  XSSMSHFT = XARG
                  YSSMSHFT = YARG
               ELSE
C ---             Zone shift calculation is not needed, assign original source
C                 UTMs to temporary variables
                  XSSMSHFT = XSRCU(ISRC)
                  YSSMSHFT = YSRCU(ISRC)
                  IZONSRC  = IZONS(ISRC)
               END IF

C ---          Determine whether file contains source based on whether file
C              boundaries follow UTM or Lat/Lon lines
               IF((IPLAN(IDEM) .EQ. 1 .AND.
     &           DABS(NWE_MTRS(IDEM)-SWE_MTRS(IDEM)) .LT. 
     &                                             0.05D0*DXM(IDEM).AND.
     &           DABS(NEE_MTRS(IDEM)-SEE_MTRS(IDEM)).LT.
     &                                             0.05D0*DXM(IDEM).AND.
     &          (XSSMSHFT .GE. SWE_MTRS(IDEM) .AND.
     &           XSSMSHFT .LE. NEE_MTRS(IDEM) .AND.
     &           YSSMSHFT .GE. SWN_MTRS(IDEM) .AND.
     &           YSSMSHFT .LE. NEN_MTRS(IDEM)))
     &         .OR.
     &          (SLON(ISRC) .GE. SWLON_ARCS(IDEM) .AND.
     &           SLON(ISRC) .LE. NELON_ARCS(IDEM) .AND.
     &           SLAT(ISRC) .GE. SWLAT_ARCS(IDEM) .AND.
     &           SLAT(ISRC) .LE. NELAT_ARCS(IDEM))) THEN
               
C*---             Source is contained within this file, which indicates
C*                that source is located within a "gap" due to NAD shift
C*                between adjacent files.
C*                Increment counter for "gap" sources assigned on 
C*                2nd pass and set logical flag
                  NSGAP2 = NSGAP2 + 1
                  
                  GAPSFOUND = .TRUE.

C* ---            Loop through DEM files again with NAD shift 
C                 to find closest file for this gap source;
C                 Use a tolerance of 1/2 of the NAD shift in
C                 each direction

                  GAPLOOP2:DO JDEM = 1, NUMDEM
                  
C                    Determine whether to apply NAD shift
                     IF (NADA .EQ. 0 .OR. NADA .EQ. NADD(JDEM)) THEN
                  
                        NADFLG = 0.0D0                ! No NAD shift needed
                  
                     ELSE IF( (NADA.EQ.1    .OR.  NADA.GE.5) .AND.
     &                     (NADD(JDEM).GE.2 .AND. NADD(JDEM).LE.4) )THEN
                  
                        NADFLG = 1.0D0                ! Include NAD shift
                  
                     ELSE IF( (NADA.GE.2    .AND. NADA.LE.4) .AND.
     &                     (NADD(JDEM).EQ.1 .OR.  NADD(JDEM).GE.5) )THEN
                  
                        NADFLG = 1.0D0                ! Include NAD shift
                  
                     ELSE
                  
                        NADFLG = 0.0D0                ! No NAD shift needed
                  
                     END IF

C*                   Apply NAD shift if necessary; direction of shift is included 
C                    in XRDIFS and YRDIFS
                     SLONSHFT = SLON(ISRC) + (XSDIFS(ISRC)*NADFLG)
                     SLATSHFT = SLAT(ISRC) + (YSDIFS(ISRC)*NADFLG)
                    
                     XSSMSHFT = XSRCU(ISRC) + (XSDIFM(ISRC)*NADFLG)
                     YSSMSHFT = YSRCU(ISRC) + (YSDIFM(ISRC)*NADFLG)
                    
                     IF (IZONS(ISRC) .NE. IZOND(JDEM)) THEN
C*                      Source zone doesn't match DEM zone;
C*                      Convert source Lat/Lon to UTMs, based on DEM file NADD and DEM Zone
                    
                        SELECT CASE (NADD(JDEM))   ! Use datum for DEM file
                          CASE (0)
                            IF (IPLAN(JDEM) .EQ. 0) THEN
                               ISPHERE = 4  ! UTM/LAT-LONG conv. based on GRS80 ellipsoid
                            ELSE IF (IPLAN(JDEM) .EQ. 1) THEN
                               ISPHERE = 0  ! UTM/LL conv. based on Clarke 1866 ellipsoid
                            END IF
                          CASE (2:4)
                            ISPHERE = 4     ! UTM/LAT-LONG conv. based on GRS80 ellipsoid
                          CASE (1,5:6)
                            ISPHERE = 0     ! UTM/LL conv. based on Clarke 1866 ellipsoid
                          CASE DEFAULT
                            ISPHERE = 4     ! DEFAULT CASE shouldn't occur
                        END SELECT
                 
                        ARGE = SLONSHFT
                        ARGN = SLATSHFT
                        XARG = 0.0D0
                        YARG = 0.0D0
C*                      Pass DEM file zone (IZOND(JDEM)) to UTMGEO to get new source
C*                      coordinates (XARG, YARG) referenced to IZOND(JDEM)).
                        CALL UTMGEO (555,IZOND(JDEM),IZONSRC,XARG,YARG,
     &                                               ARGE,ARGN,ISPHERE)
                        XSSMSHFT = XARG
                        YSSMSHFT = YARG
                     END IF
                 
                     IF((IPLAN(JDEM) .EQ. 1 .AND.
     &                  DABS(NWE_MTRS(JDEM)-SWE_MTRS(JDEM)) .LT. 
     &                                        0.05D0*DXM(JDEM).AND.
     &                  DABS(NEE_MTRS(JDEM)-SEE_MTRS(JDEM)).LT.
     &                                        0.05D0*DXM(JDEM).AND.
     &      (XSSMSHFT.GE.SWE_MTRS(JDEM)-DABS(0.5D0*XSDIFM(ISRC)) .AND.
     &       XSSMSHFT.LE.NEE_MTRS(JDEM)+DABS(0.5D0*XSDIFM(ISRC)) .AND.
     &       YSSMSHFT.GE.SWN_MTRS(JDEM)-DABS(0.5D0*YSDIFM(ISRC)) .AND.
     &       YSSMSHFT.LE.NEN_MTRS(JDEM)+DABS(0.5D0*YSDIFM(ISRC))))
     &     .OR.
     &      (SLONSHFT.GE.SWLON_ARCS(JDEM)-DABS(0.5D0*XSDIFS(ISRC)).AND.
     &       SLONSHFT.LE.NELON_ARCS(JDEM)+DABS(0.5D0*XSDIFS(ISRC)).AND.
     &       SLATSHFT.GE.SWLAT_ARCS(JDEM)-DABS(0.5D0*YSDIFS(ISRC)).AND.
     &       SLATSHFT.LE.NELAT_ARCS(JDEM)+DABS(0.5D0*YSDIFS(ISRC))))THEN
         
C*                     This is the first file that contains the source;
C*                     assign file index to JDMS array.
                     
                       JDMS(ISRC) = JDEM
                     
C*                     Assign code of 2 indicating source assigned on
C*                     2nd pass                  
                       ISIN(ISRC,JDEM) = 2
         
                       IF (SRCDBG) THEN
                        
                        WRITE(SRCD,731) ISRC, SLAT(ISRC)/3600.0D0,
     &                                        SLON(ISRC)/3600.0D0,
     &                                        YSDIFS(ISRC), XSDIFS(ISRC)
                        WRITE(SRCD,73) XSRCU(ISRC),YSRCU(ISRC),
     &                                 IZONS(ISRC),
     &                                 XSDIFM(ISRC), YSDIFM(ISRC),
     &                                 NADA
                        
                        IF (IPLAN(JDEM) .EQ. 1 .AND.
     &                     (DABS(NWE_MTRS(JDEM)-SWE_MTRS(JDEM)) .LT. 
     &                                            0.05D0*DXM(JDEM)).AND.
     &                     (DABS(NEE_MTRS(JDEM)-SEE_MTRS(JDEM)) .LT. 
     &                                            0.05D0*DXM(JDEM)))THEN
C ---                      Current file boundaries follow UTM coordinates, 
C                          print corner coordinates in degrees and UTM meters
                           WRITE(SRCD,*)
                           WRITE(SRCD,72) TYPDAT, JDEM, TYPDAT, 
     &                                    MAPNAME(JDEM), TYPDAT, 
     &                                    NADD(JDEM)
                           WRITE(SRCD,74) TYPDAT, NELAT_DEGS(JDEM),
     &                                            NELON_DEGS(JDEM)
                           WRITE(SRCD,75) SLATSHFT/3600.0D0, 
     &                                    SLONSHFT/3600.0D0
                           WRITE(SRCD,76) TYPDAT, SWLAT_DEGS(JDEM), 
     &                                            SWLON_DEGS(JDEM)
                           WRITE(SRCD,*)
                           WRITE(SRCD,77) TYPDAT, NEE_MTRS(JDEM),
     &                                            NEN_MTRS(JDEM)
                           WRITE(SRCD,78) XSSMSHFT, YSSMSHFT
                           WRITE(SRCD,79) TYPDAT, SWE_MTRS(JDEM),
     &                                            SWN_MTRS(JDEM)
                        ELSE
C ---                      Current file boundaries follow geographic coordinates, 
C                          print corner coordinates in degrees and arc-seconds
                           WRITE(SRCD,*)
                           WRITE(SRCD,72) TYPDAT, JDEM, TYPDAT, 
     &                                    MAPNAME(JDEM), TYPDAT, 
     &                                    NADD(JDEM)
                           WRITE(SRCD,74) TYPDAT, NELAT_DEGS(JDEM),
     &                                            NELON_DEGS(JDEM)
                           WRITE(SRCD,75) SLATSHFT/3600.0D0, 
     &                                    SLONSHFT/3600.0D0
                           WRITE(SRCD,76) TYPDAT, SWLAT_DEGS(JDEM), 
     &                                            SWLON_DEGS(JDEM)
                           WRITE(SRCD,*)
                           WRITE(SRCD,87) TYPDAT, NELAT_ARCS(JDEM),
     &                                            NELON_ARCS(JDEM)
                           WRITE(SRCD,88) SLATSHFT, SLONSHFT
                           WRITE(SRCD,89) TYPDAT, SWLAT_ARCS(JDEM),
     &                                            SWLON_ARCS(JDEM)
                        END IF
               
                        WRITE(SRCD,80) TYPDAT, JDMS(ISRC),ISRC
               
                       END IF
                     
                     END IF
                     
                  END DO GAPLOOP2
               
               END IF
               
C*             If DEM is found, then exit the DEM loop to next source
               IF(JDMS(ISRC) .GT. 0)THEN
                  EXIT GAPLOOP1
                  
               ELSE IF (SRCDBG) THEN
                  WRITE(SRCD,*) 'STILL LOOKING FOR ISRC: ', ISRC
                  WRITE(SRCD,731) ISRC, SLAT(ISRC)/3600.0D0,
     &                                  SLON(ISRC)/3600.0D0,
     &                                  YSDIFS(ISRC), XSDIFS(ISRC)
                  WRITE(SRCD,73) XSRCU(ISRC), YSRCU(ISRC), IZONS(ISRC),
     &                           XSDIFM(ISRC), YSDIFM(ISRC),
     &                           NADA
                  
                  IF (IPLAN(IDEM) .EQ. 1 .AND.
     &               (DABS(NWE_MTRS(IDEM)-SWE_MTRS(IDEM)) .LT. 
     &                                         0.05D0*DXM(IDEM)).AND.
     &               (DABS(NEE_MTRS(IDEM)-SEE_MTRS(IDEM)) .LT. 
     &                                         0.05D0*DXM(IDEM)))THEN
C ---                Current file boundaries follow UTM coordinates, 
C                    print corner coordinates in degrees and UTM meters
                     WRITE(SRCD,*)
                     WRITE(SRCD,72) TYPDAT, IDEM, TYPDAT, 
     &                              MAPNAME(IDEM), TYPDAT, NADD(IDEM)
                     WRITE(SRCD,74) TYPDAT, NELAT_DEGS(IDEM),
     &                                      NELON_DEGS(IDEM)
                     WRITE(SRCD,75) SLAT(ISRC)/3600.0D0, 
     &                              SLON(ISRC)/3600.0D0
                     WRITE(SRCD,76) TYPDAT, SWLAT_DEGS(IDEM), 
     &                                      SWLON_DEGS(IDEM)
                     WRITE(SRCD,*)
                     WRITE(SRCD,77) TYPDAT, NEE_MTRS(IDEM),
     &                                      NEN_MTRS(IDEM)
                     WRITE(SRCD,78) XSRCU(ISRC), YSRCU(ISRC)
                     WRITE(SRCD,79) TYPDAT, SWE_MTRS(IDEM),
     &                                      SWN_MTRS(IDEM)
                  ELSE
C ---                Current file boundaries follow geographic coordinates, 
C                    print corner coordinates in degrees and arc-seconds
                     WRITE(SRCD,*)
                     WRITE(SRCD,72) TYPDAT, IDEM, TYPDAT, 
     &                              MAPNAME(IDEM), TYPDAT, NADD(IDEM)
                     WRITE(SRCD,74) TYPDAT, NELAT_DEGS(IDEM),
     &                                      NELON_DEGS(IDEM)
                     WRITE(SRCD,75) SLAT(ISRC)/3600.0D0, 
     &                              SLON(ISRC)/3600.0D0
                     WRITE(SRCD,76) TYPDAT, SWLAT_DEGS(IDEM), 
     &                                      SWLON_DEGS(IDEM)
                     WRITE(SRCD,*)
                     WRITE(SRCD,87) TYPDAT, NELAT_ARCS(IDEM),
     &                                      NELON_ARCS(IDEM)
                     WRITE(SRCD,88) SLAT(ISRC), SLON(ISRC)
                     WRITE(SRCD,89) TYPDAT, SWLAT_ARCS(IDEM),
     &                                      SWLON_ARCS(IDEM)
                  END IF
               END IF

            END DO GAPLOOP1

         END IF
         
      END DO   ! Source Loop

C*    Write message regarding results of 2nd pass for gap sources
      WRITE(IOUNIT,92) NSGAP2, TYPDAT
 92   FORMAT(/,'******',
     &  /'DEMSRC:  A total of ',I7,' "Gap" sources were',
     &  /'         assigned to ',A3,' files on the 2nd Pass.',
     &  /'         Depending on the size of the gap, these',
     &  /'         sources may be assigned missing elevations',
     &  /'         unless the "FILLGAPS" option on the DATATYPE',
     &  /'         keyword is specified.',
     &  /'         These gaps are due to NAD conversion issues,',
     &  /'         which should be resolved with the use of ',
     &  /'         standard NED elevation data.',
     &       /'******'/)
      IF (SRCDBG) THEN
         WRITE(SRCD,92) NSGAP2, TYPDAT
      END IF

C*    Check Again for Sources Not Assigned to Any DEM File
C*    This time fatal errors will be generated by "homeless" sources
      DO ISRC = 1, NUMSRC
         IF (JDMS(ISRC) .EQ. 0) THEN
            RUNERR = .TRUE.

            CALL ERRHDL(PATH,MODNAM,'E','336',SRCID(ISRC))
            WRITE(IOUNIT,*) '*** SOURCE NOT ASSIGNED TO FILE - ',
     &                      'SECOND PASS:'
            WRITE(IOUNIT,90) ISRC, XSRCU(ISRC), YSRCU(ISRC),IZONS(ISRC),
     &                       SLAT(ISRC)/3600.0D0, SLON(ISRC)/3600.0D0,
     &                       NADA

         END IF
      END DO

999   CONTINUE

      IF (SRCDBG) CLOSE(SRCD)

      WRITE(iounit,*) 'Exiting DEMSRC'
      WRITE(*,*) 'Exiting DEMSRC'

      RETURN

      END SUBROUTINE
