      SUBROUTINE CHKADJ
C***********************************************************************
C*                CHKADJ Module of AERMAP Terrain Preprocessor
C*
C*       PURPOSE: Check to See if the DEM Files are Contiguous.  They 
C*                Should Have at Least 1 Common Border With Another DEM File
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
C*       MODIFIED BY: Roger W. Brode, U.S. EPA, OAQPS, AQMG
C*
C*                Modified adjacency checks to handle Alaska DEMs, 
C*                "mixed" DEMs and NED data.  Modified for use of 
C*                standard convention of negative for West longitude.
C*                Included adjustments to adjacency checks to account
C*                for domains that cross the 180E/180W meridian.
C*                Also included code to allow tracking input elevation 
C*                files to ensure that higher resolution data are 
C*                entered first.
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
C*       MODIFIED:   To correct problem with multiple DEM files.
C*                   R.W. Brode, PES, Inc. - 2/27/96
C*                   
C*       INPUTS:  Coordinates of the Corners of DEM Files
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
      CHARACTER*11 LVLNT

      LOGICAL, ALLOCATABLE :: ADJCNT(:)
      
      LOGICAL FIRST
      
      DOUBLE PRECISION, PARAMETER :: EPSILON = 0.001D0
      
      DOUBLE PRECISION DIFLAT, DIFLON, CNRADJ_LON, CNRADJ_LAT
      DOUBLE PRECISION ALAT, ALON, CLAT, CLON
      DOUBLE PRECISION BLAT, BLON
      DOUBLE PRECISION DEMLAT1, DEMLAT2, DEMLON1, DEMLON2
      DOUBLE PRECISION DYM_MINLAT, DYM_MINUTM
      DOUBLE PRECISION, ALLOCATABLE :: DELTA_LAT(:), DELTA_LON(:)

      INTEGER I, J, K
      INTEGER JDEM, NUMADJ
      INTEGER INOD, JNOD
      INTEGER ILAT, ILON

C*    Allocate arrays to hold latitude/longitude extents of files,
C*    and array for ADJCNT flag
      ALLOCATE (DELTA_LAT(NDEM), DELTA_LON(NDEM))
      ALLOCATE (ADJCNT(NDEM))

C*    Variable Initializations
      MODNAM = 'CHKADJ'
      
C*    Initialize last MAPNAME array element to indicate No Map Present
      MAPNAME(NUMDEM + 1) = '   No Map Present'
            
      WRITE(MAPK,*) ' '
      WRITE(MAPK,'(/1x,''From '',A12)') MODNAM

C*    Use DEM file type (LAT or UTM) based on first DEM file
      IF (IPLAN(1) .EQ. 0) THEN
         TYPDEM = 'LAT'
         DYM_MINLAT = DYM(1)
C*       Save "equivalent" horizontal resolution in meters         
         IF (DABS(DYM_MINLAT-(1.0D0/9.0D0)) .LT. EPSILON) THEN
            DYM_MINUTM = 3.0D0
         ELSE IF (DABS(DYM_MINLAT-(1.0D0/3.0D0)) .LT. EPSILON) THEN
            DYM_MINUTM = 10.0D0
         ELSE IF (DABS(DYM_MINLAT-1.0D0) .LT. EPSILON) THEN
            DYM_MINUTM = 30.0D0
         ELSE IF (DABS(DYM_MINLAT-2.0D0) .LT. EPSILON) THEN
            DYM_MINUTM = 60.0D0
         ELSE IF (DABS(DYM_MINLAT-3.0D0) .LT. EPSILON) THEN
            DYM_MINUTM = 90.0D0
         ELSE
            DYM_MINUTM = DYM_MINLAT*30.0D0
         END IF
         
C     If coordinate system is UTM and first DEM file is an Alaska DEM, this should
C     be a 7.5-min DEM. Set DEM type (TYPDEM) to "LAT" (even though it is in UTM). 
      ELSE IF (IPLAN(1) .EQ. 1 .AND.
     &                          SELAT_ARCS(1) .GE. 1.8D5) THEN
C*       First file is AK 7.5-min DEM with UTM coordinates     
         TYPDEM = 'LAT'
         DYM_MINUTM = DYM(1)
C*       Save "equivalent" horizontal resolution in arc-seconds
         IF (DABS(DYM_MINUTM-3.0D0) .LT. EPSILON) THEN
            DYM_MINLAT = 1.0D0/9.0D0
         ELSE IF (DABS(DYM_MINUTM-10.0D0) .LT. EPSILON) THEN
            DYM_MINLAT = 1.0D0/3.0D0
         ELSE IF (DABS(DYM_MINUTM-30.0D0) .LT. EPSILON) THEN
            DYM_MINLAT = 1.0D0
         ELSE IF (DABS(DYM_MINUTM-90.0D0) .LT. EPSILON) THEN
            DYM_MINLAT = 3.0D0
         ELSE
            DYM_MINLAT = DYM_MINUTM/30.0D0
         END IF
         
      ELSE IF (IPLAN(1) .EQ. 1) THEN
         TYPDEM = 'UTM'
         DYM_MINUTM = DYM(1)
C*       Save "equivalent" horizontal resolution in arc-seconds
         IF (DABS(DYM_MINUTM-3.0D0) .LT. EPSILON) THEN
            DYM_MINLAT = 1.0D0/9.0D0
         ELSE IF (DABS(DYM_MINUTM-10.0D0) .LT. EPSILON) THEN
            DYM_MINLAT = 1.0D0/3.0D0
         ELSE IF (DABS(DYM_MINUTM-30.0D0) .LT. EPSILON) THEN
            DYM_MINLAT = 1.0D0
         ELSE IF (DABS(DYM_MINUTM-90.0D0) .LT. EPSILON) THEN
            DYM_MINLAT = 3.0D0
         ELSE
            DYM_MINLAT = DYM_MINUTM/30.0D0
         END IF
         
      ELSE
C*       Unkown file type, assume LAT
         TYPDEM = 'LAT'
         DYM_MINLAT = DYM(1)
C*       Save "equivalent" horizontal resolution in meters         
         IF (DABS(DYM_MINLAT-(1.0D0/9.0D0)) .LT. EPSILON) THEN
            DYM_MINUTM = 3.0D0
         ELSE IF (DABS(DYM_MINLAT-(1.0D0/3.0D0)) .LT. EPSILON) THEN
            DYM_MINUTM = 10.0D0
         ELSE IF (DABS(DYM_MINLAT-1.0D0) .LT. EPSILON) THEN
            DYM_MINUTM = 30.0D0
         ELSE IF (DABS(DYM_MINLAT-2.0D0) .LT. EPSILON) THEN
            DYM_MINUTM = 60.0D0
         ELSE IF (DABS(DYM_MINLAT-3.0D0) .LT. EPSILON) THEN
            DYM_MINUTM = 90.0D0
         ELSE
            DYM_MINUTM = DYM_MINLAT*30.0D0
         END IF
      END IF

      DO IDEM = 1, NUMDEM
         ADJCNT(IDEM) = .FALSE.
      END DO


C*    Read All the Coordinates.  The WGS 72 header is reported not to have all
C     of its header data filled in.
      DO IDEM = 1, NUMDEM

C----    Check for higher resolution files entered after lower res files
C*       Issue warnings for now; error will be issued if receptor is 
C*       found within a higher res file after being assigned to lower res file
         IF (IDEM .GT. 1) THEN
            IF (IPLAN(IDEM) .EQ. 0) THEN
               IF ((DYM(IDEM)-DYM_MINLAT+EPSILON) .LT. 0.0D0) THEN
                  WRITE (DUMMY,'(I8)') IDEM
                  CALL ERRHDL(PATH,MODNAM,'W','325',DUMMY)
               ELSE IF (DYM(IDEM) .GT. DYM_MINLAT) THEN
                  DYM_MINLAT = DYM(IDEM)
                  DYM_MINUTM = DYM_MINLAT*30.0D0
               END IF
            ELSE IF (IPLAN(IDEM) .EQ. 1) THEN
               IF ((DYM(IDEM)-DYM_MINUTM+EPSILON) .LT. 0.0D0) THEN
                  WRITE (DUMMY,'(I8)') IDEM
                  CALL ERRHDL(PATH,MODNAM,'W','325',DUMMY)
               ELSE IF (DYM(IDEM) .GT. DYM_MINUTM) THEN
                  DYM_MINUTM = DYM(IDEM)
                  DYM_MINLAT = DYM_MINUTM/30.0D0
               END IF
            END IF
         END IF
                          
C*       Initialize counter for length of "refined header data segment"
         J = MIN(40, INDEX(MAPN(IDEM),'  '))
         IF (J .GT. 1) THEN
            MAPNAME(IDEM) = MAPN(IDEM)(1:J)
         ELSE
C*          File header info is blank; use filename without path (FLN) instead
            J = MIN(40, LEN_TRIM(FLN(IDEM)))
            MAPNAME(IDEM) = FLN(IDEM)(1:J)
         END IF

         WRITE(MAPK,5012) TYPDAT, IDEM, MAPN(IDEM), MAPNAME(IDEM)
5012       FORMAT(/1X,A3,' File #: ',I6,
     &            /1X,A40,' <-- Raw header data segment',
     &            /1X,A40,' <-- Refined header data segment')


         IF (DABS(DCI(IDEM)-0.1D0) .LT. EPSILON) THEN
            LVLNT = 'deci-' // LVLN(ELUNIT(IDEM))
         ELSE IF (DABS(DCI(IDEM)-10.0D0) .LT. EPSILON) THEN
            LVLNT = 'deca-' // LVLN(ELUNIT(IDEM))
         ELSE
            LVLNT = LVLN(ELUNIT(IDEM))
         END IF

         WRITE(MAPK,5022) CDLVL(DEMLVL(IDEM)),
     &                      PLAN(IPLAN(IDEM)), 
     &                      DYM(IDEM), CUNITN(CUNIT(IDEM)),
     &                      DXM(IDEM), CUNITN(CUNIT(IDEM)),
     &                      IZO(IDEM)

5022       FORMAT(3X,'DEM Level Code:  ',A6/
     &            3X,'Planimetric Ref: ',A25/
     &            3X,'  North-South Node separation: ', F8.4,2X,A11/
     &            3X,'  East-West Node separation:   ', F8.4,2X,A11/
     &            3X,'UTM/State Zone # ',I3)

C*       Check for datum of 0
         IF (NADD(IDEM) .EQ. 0) THEN
            IF (IPLAN(IDEM) .EQ. 0) THEN
               IF (DABS((NELAT_DEGS(IDEM)) -
     &                  (SELAT_DEGS(IDEM)) - 1.0D0) .LE.
     &                                               0.001D0) THEN
C*                Assume 1-Degree DEM, assign NADD(IDEM) = 2
                  NADD(IDEM) = 2
                  WRITE(MAPK,*) ' '
                  WRITE(MAPK,*) '  DEM NADA = 0. DEFAULT HORIZONTAL ',
     &                          'DATUM ASSIGNED: ', NADD(IDEM)
                  WRITE(MAPK,*) ' '
               ELSE
C*                Assume Alaska 7.5-min or 15-min DEM, assign NADD(IDEM) = 1
                  NADD(IDEM) = 1
                  WRITE(MAPK,*) ' '
                  WRITE(MAPK,*) '  DEM NADA = 0. DEFAULT HORIZONTAL ',
     &                          'DATUM ASSIGNED: ', NADD(IDEM)
                  WRITE(MAPK,*) ' '
              END IF
            ELSE IF (IPLAN(IDEM) .EQ. 1) THEN
C*             Assume 7.5-Minute DEM, assign NADD(IDEM) = 1
               NADD(IDEM) = 1
               WRITE(MAPK,*) ' '
               WRITE(MAPK,*) '  DEM NADA = 0. DEFAULT HORIZONTAL ',
     &                       'DATUM ASSIGNED: ', NADD(IDEM)
               WRITE(MAPK,*) ' '
            END IF
         END IF

         WRITE(MAPK,5023) NADN(NADD(IDEM)), NPROF(IDEM)

5023       FORMAT(3X,'Horizontal Datum: ',A40,
     &           /3X,'Num. of Profiles: ',I5)

         IF (TYPDAT .EQ. 'DEM') THEN
            WRITE(MAPK,5024) LVLNT, ELEVMN(IDEM), LVLN(ELUNIT(IDEM)),
     &                       ELEVMX(IDEM), LVLN(ELUNIT(IDEM))

5024        FORMAT(/3x,'Original elevation units of nodes: ',A11,
     &             /3X,'Min. Elevation: ',F8.1, 1X, A6
     &             /3X,'Max. Elevation: ',F8.1, 1X, A6)
         END IF

         IF (TYPDAT .EQ. 'DEM' .AND. ELUNIT(IDEM) .EQ. 1) THEN
           ELEVMN(IDEM) = ELEVMN(IDEM) * 0.3048D+0
           ELEVMX(IDEM) = ELEVMX(IDEM) * 0.3048D+0
           WRITE(MAPK,5025) ELEVMN(IDEM), ELEVMX(IDEM)
5025       FORMAT(3X,'Min & Max Elevations converted to meters:',
     &         /3X,'Min. Elevation: ',F8.1, ' meters'
     &         /3X,'Max. Elevation: ',F8.1, ' meters')
         END IF


         WRITE(MAPK, 5010) SWLAT_DEGS(IDEM), SWLON_DEGS(IDEM)
5010       FORMAT('   SW Corner Lat/Lon (deg.): ',2F14.5)
         WRITE(MAPK, 5011) SWLAT_ARCS(IDEM), 
     &                     SWLON_ARCS(IDEM)
5011       FORMAT('   SW Corner Lat/Lon (sec.): ',2F14.3)

C        Calculate Lat/Lon Domain of DEM File
         DELTA_LAT(IDEM) = NELAT_DEGS(IDEM) - SELAT_DEGS(IDEM)
         DELTA_LON(IDEM) = SELON_DEGS(IDEM) - SWLON_DEGS(IDEM)

         WRITE(MAPK, 5110) DELTA_LAT(IDEM), DELTA_LON(IDEM)
5110       FORMAT('   Latitude Range = ',F7.4,' Deg.;',
     &            '  Longitude Range = ',F7.4,' Deg.')

         WRITE(MAPK,*) '  Writing corner coords; UTM then Dec.Deg ',
     &                 '(negative for West longitude):'
         J = IDEM
         WRITE(MAPK, 5112) 
     &    SWE_MTRS(J),SWN_MTRS(J),NWE_MTRS(J),NWN_MTRS(J),
     &    NEE_MTRS(J),NEN_MTRS(J),SEE_MTRS(J),SEN_MTRS(J),
     &    SWLAT_DEGS(J),SWLON_DEGS(J),NWLAT_DEGS(J),NWLON_DEGS(J),
     &    NELAT_DEGS(J),NELON_DEGS(J),SELAT_DEGS(J),SELON_DEGS(J)
5112      FORMAT(5x,'Southwest Corner',8x,'Northwest Corner',8x,
     &              'Northeast Corner',8x,'Southeast Corner'/
     &     5X,4('Easting    Northing ',4x)/4X,4(f10.2,f12.2,2x)/
     &     5X,4('Latitude   Longitude',4x)/2X,4(F10.4,F12.4,2x)/)

         WRITE(MAPK,*) ' '

      END DO

C --- Assign adjacency array if only one data file is input      
      IF (NUMDEM .EQ. 1) THEN
         ADJCNT(NUMDEM) = .TRUE.
         DO K = 1, 9
            ADJMAP(1,K) = 2
         END DO
         ADJMAP(1,5) = 1
      END IF

C --- Loop through multiple DEM files to see if they are all adjacent 
C     to at least one other file, and create arrays of adjacent DEMs
      IF (NUMDEM .GT. 1) THEN

C ---    First check for full adjacency - two corners in common
C        Apply a stringent test for adjacency of +/- 0.1 arc-second
C        Also create a 3x3 grid of neighboring "maps" for each file
C
C        Begin loop through DEM files
         DO IDEM = 1, NUMDEM
C           Initialize ADJMAP array for 3x3 grid of adjacent files
C           3x3 grid array numbered as follow:
C
C             7   8   9
C             4   5   6    current file is located at position 5
C             1   2   3
C
            DO K = 1, 9
C              Assign value of NUMDEM+1 initially for "No Map Present"            
               ADJMAP(IDEM,K) = NUMDEM+1
            END DO
C           Assign center value of ADJMAP array to local DEM            
            ADJMAP(IDEM,5) = IDEM
            
C ---       Loop through DEM files for comparison
            DO JDEM = 1, NUMDEM
C              Initialize counter for number of adjacent files            
               NUMADJ = 0
               IF (IDEM .EQ. JDEM) CYCLE
               
               DO INOD = 1, 4
C ---             Loop through nodes for IDEM file
                  IF (INOD .EQ. 1) THEN
                     DEMLAT1 = SWLAT_ARCS(IDEM)
                     DEMLON1 = SWLON_ARCS(IDEM)
                  ELSE IF (INOD .EQ. 2) THEN
                     DEMLAT1 = NWLAT_ARCS(IDEM)
                     DEMLON1 = NWLON_ARCS(IDEM)
                  ELSE IF (INOD .EQ. 3) THEN
                     DEMLAT1 = NELAT_ARCS(IDEM)
                     DEMLON1 = NELON_ARCS(IDEM)
                  ELSE IF (INOD .EQ. 4) THEN
                     DEMLAT1 = SELAT_ARCS(IDEM)
                     DEMLON1 = SELON_ARCS(IDEM)
                  END IF
                  
                  DO JNOD = 1, 4
C ---                Loop through nodes for JDEM file                 
                     IF (JNOD .EQ. 1) THEN
                        DEMLAT2 = SWLAT_ARCS(JDEM)
                        DEMLON2 = SWLON_ARCS(JDEM)
                     ELSE IF (JNOD .EQ. 2) THEN
                        DEMLAT2 = NWLAT_ARCS(JDEM)
                        DEMLON2 = NWLON_ARCS(JDEM)
                     ELSE IF (JNOD .EQ. 3) THEN
                        DEMLAT2 = NELAT_ARCS(JDEM)
                        DEMLON2 = NELON_ARCS(JDEM)
                     ELSE IF (JNOD .EQ. 4) THEN
                        DEMLAT2 = SELAT_ARCS(JDEM)
                        DEMLON2 = SELON_ARCS(JDEM)
                     END IF
                     
C ---                Calculate differences in lat. and long. for adjacency tests
                     DIFLAT= DEMLAT1 - DEMLAT2
                     DIFLON= DEMLON1 - DEMLON2
C ---                Adjust for files that cross 180E/180W meridian
                     IF (DIFLON .GT. 648000.0D0) THEN         ! DIFLON > 180 deg.
                        DIFLON = 1296000.0D0 - DIFLON         ! Subtract from 360 deg.
                     ELSE IF (DIFLON .LT. -648000.0D0) THEN   ! DIFLON < -180 deg.
                        DIFLON = DIFLON + 1296000.0D0         ! Add 360 deg.
                     END IF
                     
C ---                Check for difference less than 0.1 second for strict adjacency
C                    (about 3 meter difference)
                     IF (DABS(DIFLAT) .LT. 1.0D-1 .AND.
     &                   DABS(DIFLON) .LT. 1.0D-1) THEN
                        NUMADJ = NUMADJ + 1
                     END IF
                     
C ---                Check for difference less than 45 seconds (about 10% of 7.5-min
C                    quadrangle) for determination of 3x3 grid of neighboring files
C                    Files must share a common corner, with +/- 45 second tolerance
                     IF (DABS(DIFLAT) .LT. 45.0D0 .AND.
     &                   DABS(DIFLON) .LT. 45.0D0) THEN
    
C ---                   Determine relative location of file within 3x3 grid based
C                       on node locations; use first file that satisfies adjacency 
C                       criterion, unless later file with same longitude extent
C                       also satisfies criterion.
                        IF (INOD .EQ. 1) THEN
                           IF (JNOD .EQ. 2) THEN
                              IF (ADJMAP(IDEM,2) .EQ. NUMDEM+1) THEN
                                  ADJMAP(IDEM,2) = JDEM
                              ELSE IF (DABS(DELTA_LON(IDEM)-
     &                                      DELTA_LON(JDEM)) .LT. 
     &                                                     EPSILON) THEN
                                  ADJMAP(IDEM,2) = JDEM
                              END IF
                           ELSE IF (JNOD .EQ. 3) THEN
                              IF (ADJMAP(IDEM,1) .EQ. NUMDEM+1) THEN
                                  ADJMAP(IDEM,1) = JDEM
                              ELSE IF (DABS(DELTA_LON(IDEM)-
     &                                      DELTA_LON(JDEM)) .LT. 
     &                                                     EPSILON) THEN
                                  ADJMAP(IDEM,1) = JDEM
                              END IF
                           ELSE IF (JNOD .EQ. 4) THEN
                              IF (ADJMAP(IDEM,4) .EQ. NUMDEM+1) THEN
                                  ADJMAP(IDEM,4) = JDEM
                              ELSE IF (DABS(DELTA_LON(IDEM)-
     &                                      DELTA_LON(JDEM)) .LT. 
     &                                                     EPSILON) THEN
                                  ADJMAP(IDEM,4) = JDEM
                              END IF
                           END IF
                           
                        ELSE IF (INOD .EQ. 2) THEN
                           IF (JNOD .EQ. 1) THEN
                              IF (ADJMAP(IDEM,8) .EQ. NUMDEM+1) THEN
                                  ADJMAP(IDEM,8) = JDEM
                              ELSE IF (DABS(DELTA_LON(IDEM)-
     &                                      DELTA_LON(JDEM)) .LT. 
     &                                                     EPSILON) THEN
                                  ADJMAP(IDEM,8) = JDEM
                              END IF
                           ELSE IF (JNOD .EQ. 3) THEN
                              IF (ADJMAP(IDEM,4) .EQ. NUMDEM+1) THEN
                                  ADJMAP(IDEM,4) = JDEM
                              ELSE IF (DABS(DELTA_LON(IDEM)-
     &                                      DELTA_LON(JDEM)) .LT. 
     &                                                     EPSILON) THEN
                                  ADJMAP(IDEM,4) = JDEM
                              END IF
                           ELSE IF (JNOD .EQ. 4) THEN
                              IF (ADJMAP(IDEM,7) .EQ. NUMDEM+1) THEN
                                  ADJMAP(IDEM,7) = JDEM
                              ELSE IF (DABS(DELTA_LON(IDEM)-
     &                                      DELTA_LON(JDEM)) .LT. 
     &                                                     EPSILON) THEN
                                  ADJMAP(IDEM,7) = JDEM
                              END IF
                           END IF
                           
                        ELSE IF (INOD .EQ. 3) THEN
                           IF (JNOD .EQ. 1) THEN
                              IF (ADJMAP(IDEM,9) .EQ. NUMDEM+1) THEN
                                  ADJMAP(IDEM,9) = JDEM
                              ELSE IF (DABS(DELTA_LON(IDEM)-
     &                                      DELTA_LON(JDEM)) .LT. 
     &                                                     EPSILON) THEN
                                  ADJMAP(IDEM,9) = JDEM
                              END IF
                           ELSE IF (JNOD .EQ. 2) THEN
                              IF (ADJMAP(IDEM,6) .EQ. NUMDEM+1) THEN
                                  ADJMAP(IDEM,6) = JDEM
                              ELSE IF (DABS(DELTA_LON(IDEM)-
     &                                      DELTA_LON(JDEM)) .LT. 
     &                                                     EPSILON) THEN
                                  ADJMAP(IDEM,6) = JDEM
                              END IF
                           ELSE IF (JNOD .EQ. 4) THEN
                              IF (ADJMAP(IDEM,8) .EQ. NUMDEM+1) THEN
                                  ADJMAP(IDEM,8) = JDEM
                              ELSE IF (DABS(DELTA_LON(IDEM)-
     &                                      DELTA_LON(JDEM)) .LT. 
     &                                                     EPSILON) THEN
                                  ADJMAP(IDEM,8) = JDEM
                              END IF
                           END IF
                           
                        ELSE IF (INOD .EQ. 4) THEN
                           IF (JNOD .EQ. 1) THEN
                              IF (ADJMAP(IDEM,6) .EQ. NUMDEM+1) THEN
                                  ADJMAP(IDEM,6) = JDEM
                              ELSE IF (DABS(DELTA_LON(IDEM)-
     &                                      DELTA_LON(JDEM)) .LT. 
     &                                                     EPSILON) THEN
                                  ADJMAP(IDEM,6) = JDEM
                              END IF
                           ELSE IF (JNOD .EQ. 2) THEN
                              IF (ADJMAP(IDEM,3) .EQ. NUMDEM+1) THEN
                                  ADJMAP(IDEM,3) = JDEM
                              ELSE IF (DABS(DELTA_LON(IDEM)-
     &                                      DELTA_LON(JDEM)) .LT. 
     &                                                     EPSILON) THEN
                                  ADJMAP(IDEM,3) = JDEM
                              END IF
                           ELSE IF (JNOD .EQ. 3) THEN
                              IF (ADJMAP(IDEM,2) .EQ. NUMDEM+1) THEN
                                  ADJMAP(IDEM,2) = JDEM
                              ELSE IF (DABS(DELTA_LON(IDEM)-
     &                                      DELTA_LON(JDEM)) .LT. 
     &                                                     EPSILON) THEN
                                  ADJMAP(IDEM,2) = JDEM
                              END IF
                           END IF
                        END IF
                        
                     END IF
                     
                  END DO   ! JNOD loop
               END DO      ! INOD loop
                       
               IF (NUMADJ .EQ. 2) THEN
C ---             Two corners match, files are adjacent (share a side)
                  ADJCNT(IDEM) = .TRUE.
               END IF
               
            END DO         ! JDEM loop
         END DO            ! IDEM loop

C ---    Write out 3x3 grids to MAPPARAMS.OUT file
         write(MAPK,256)
         do idem = 1, numdem
            WRITE(MAPK,259) IDEM, MAPNAME(IDEM)(1:26),
     &                     (MAPNAME(ADJMAP(IDEM,K))(1:26), K = 7, 9),
     &                     (MAPNAME(ADJMAP(IDEM,K))(1:26), K = 4, 6),
     &                     (MAPNAME(ADJMAP(IDEM,K))(1:26), K = 1, 3)
         end do

256     Format(//'MAP ADJACENCY (Based on +/- 45 arc-seconds)'//
     &         ' No. Base Map Name', 15x,
     &         'Adjacent map names and relative geographic',
     &         ' location to Base Map.',
     &    /33x,'NOTE: Adjacency for NED files, Mixed DEMs, ',
     &                                 'non-standard DEMs, ',
     &    /33x,'      and AK DEMs may not be exact.'/)
259     FORMAT(I3,2X,A26,'  NW: ',A26,'   N: ',A26,'  NE: ',A26/
     &               31X,'   W: ',A26,'   C: ',A26,'   E: ',A26/
     &               31X,'  SW: ',A26,'   S: ',A26,'  SE: ',A26/)


C ---    Assign variables to determine full latitude/longitude 
C        range covered by DEM/NED data
         ALAT = MAXVAL(SELAT_DEGS)
         ALON = MAXVAL(SELON_DEGS)
         BLAT = MINVAL(SELAT_DEGS)
         BLON = MINVAL(SELON_DEGS)

C ---    Create Mapname array (MAPARRAY) - visually check for 
C        missing and errant files
         WRITE(MAPK,257)
257      FORMAT(//'MAPNAME ARRAY (Based on +/- 0.03 degrees)'//
     & '  Mapnames are laid out in relative geographical positions.'
     &/'  "Grid" resolution is based on Lat/Lon extent of first file.',
     &/'  Relative positions are based on SE corner coordinates ',
     & 'relative to full grid of maps.',
     &/'  Look for "No Map Present" within the field of interest.'
     &/'  This may indicate missing files or files with',
     & ' errant data in their header records.',/
     &'  NOTE: Adjacency for NED files, Mixed DEMs, non-standard DEMs,',
     &' and AK DEMs may not be exact.',/
     &'        Higher resolution maps may overwrite lower resolution',
     &' maps that overlap.'/)

C ---    Initialize ILAT and ILON counters for number of "grid" cells in
C        Latitude and Longitude
         ILAT = 0
         ILON = 0
         
C ---    Loop through DEM files to determine size of "DEM grid" in
C        Lat/Lon increments
         DO IDEM = 1, NUMDEM

            CNRADJ_LAT = DABS(SELAT_DEGS(IDEM) - NELAT_DEGS(IDEM))
            CNRADJ_LON = DABS(SELON_DEGS(IDEM) - SWLON_DEGS(IDEM))

            ILAT = MAX(ILAT,IDNINT(DABS(ALAT-BLAT)/(CNRADJ_LAT)))
            ILON = MAX(ILON,IDNINT(DABS(ALON-BLON)/(CNRADJ_LON)))
C ---       Adjust for files that cross the 180E/180W meridian
            IF (ILON .GT. 180) THEN
               ILON = 360 - ILON
            END IF

         END DO

C ---    Add 1 to account for "grid" boundary
         ILAT = ILAT + 1
         ILON = ILON + 1

C ---    Allocate MAPARRAY based on size of "DEM grid", and initialize
         ALLOCATE (MAPARRAY(ILAT,ILON))
         
C ---    Initialize array to NUMDEM+1 to indicate "No Map"
         MAPARRAY = NUMDEM+1
         
C ---    Loop through the Lat/Lon "Grid" to assign DEM files to proper
C        "grid cells" for output array.
C
C ---    Use latitude and longitude range of 1st DEM file to calculate
C        relative locations
         CNRADJ_LAT = DABS(SELAT_DEGS(1) - NELAT_DEGS(1))
         CNRADJ_LON = DABS(SELON_DEGS(1) - SWLON_DEGS(1))
         
C ---    Adjust BLON for files that cross the 180E/180W meridian         
         IF ( (ALON-BLON) .GT. 180.0D0) THEN
            BLON = -1.0D0*ALON
         END IF
         
         DO I = 1, ILAT
            DO J = 1, ILON
            
               DO K = 1, NUMDEM

C ---             Calculate latitude and longitude of current file 
C                 relative to southwest corner of data domain
                  CLAT = BLAT + DBLE(I - 1) * CNRADJ_LAT
                  CLON = BLON + DBLE(J - 1) * CNRADJ_LON

C ---             Apply even less stringent test here than for 3X3 array,
C                 use +/- 0.03 degrees, or about 3,000 meters.
                  IF (DABS(SELAT_DEGS(K) - CLAT) .LT. 0.03D0) THEN
                    DIFLON = SELON_DEGS(K) - CLON
C ---               Adjust for 180E/180W crossover if necessary                    
                    IF (DIFLON .GT. 180.0D0) THEN
                       DIFLON = DIFLON - 360.0D0
                    ELSE IF (DIFLON .LT. -180.0D0) THEN
                       DIFLON = DIFLON + 360.0D0
                    END IF
                    IF (DABS(DIFLON) .LT. 0.03D0) THEN
C ---                 DEM found for this map array element, 
C                     set flag and exit the DEM loop
                      MAPARRAY(I,J) = K
                      EXIT
                    END IF
                  END IF

               END DO    ! DEM loop
               
            END DO       ! ILON loop
         END DO          ! ILAT loop

C ---    Write out full map array to MAPPARAMS.OUT file
         DO I = ILAT, 1, -1
            WRITE(MAPK, 261) (MAPNAME(MAPARRAY(I,J))(1:26), J = 1, ILON)
261         FORMAT(12(1X,A26,1X,:))
         END DO

         WRITE(MAPK,*) ' '

         FIRST = .TRUE.
         DO IDEM = 1, NUMDEM
            IF (.NOT. ADJCNT(IDEM)) THEN
               IF (FIRST) THEN
                  WRITE(MAPK,*) ' '
                  WRITE(MAPK,*) 'THE FOLLOWING DATA FILES ARE ',
     &                          'NOT ADJACENT TO ANY OTHER FILE:'
                  WRITE(MAPK,*) '  (WITHIN +/- 0.1 ARC-SECOND)'
                  WRITE(MAPK,*) '   FILE NO.   LOCATION'
                  WRITE(MAPK,*) '  ---------   ------------------------'
                  FIRST = .FALSE.
               END IF
               WRITE(MAPK,'(4X,I8,3X,A40)') IDEM, MAPNAME(IDEM)(1:40)
               WRITE (DUMMY,'(I8)') IDEM
               CALL ERRHDL(PATH,MODNAM,'W','340',DUMMY)
            END IF
         END DO

      END IF


999   CONTINUE

      WRITE(iounit,*) 'Exiting CHKADJ'
      WRITE(*,*) 'Exiting CHKADJ'

      RETURN

      END SUBROUTINE         
