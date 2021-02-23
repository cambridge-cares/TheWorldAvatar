      SUBROUTINE INITER_DEM
C***********************************************************************
C*               INITER_DEM Module of AERMAP Terrain Preprocessor
C*
C*       PURPOSE: Read Terrain Data from Raw DEM Data Files and Write Out  
C*                the Data to Direct Access Binary Terrain Data Files.  
C*                Also Create Record Index Files
C*
C*
C*       PROGRAMMER: Jayant Hardikar, Roger Brode
C*
C*       DATE:    September 29, 1995
C*
C*       MODIFIED: February 9, 2009
C*                
C*                Modified output format for UTM Zone to I3 to
C*                accommodate Southern Hemisphere applications, 
C*                since Southern zones are negative.  Also 
C*                adjusted output format for XBASE and YBASE
C*                to accommodate double precision values.
C*                Adjusted XBASE (longitude) for LAT files to 
C*                use negative for west longitude.
C*                Modified to account for no-DOMAIN option.
C*                Also modified handling of missing elevation
C*                data and elevation of local datum (LOCEL).
C*                Added information on direct access files to
C*                the DOMDETAIL.OUT debug file.
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
C*       INPUTS:  Raw Terrain Data (DEM) Files
C*
C*       OUTPUTS: Direct Access Binary Terrain Files
C*
C*       CALLED FROM:   MAIN
C***********************************************************************
       
C*    Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM

      LOGICAL NEWPRO
      DOUBLE PRECISION XNODEI,YNODEI
      DOUBLE PRECISION NADFLG
      DOUBLE PRECISION ARGE, ARGN, XARG, YARG
      DOUBLE PRECISION XBUTM, YBUTM, XNUTM, YNUTM
      DOUBLE PRECISION XBLON, YBLAT
      INTEGER IPROF, J, K, L, M, IERR, INOD
      
      INTEGER, ALLOCATABLE :: NUMNOD(:)
      
      INTEGER NSTART, N, JRECRD, RC, ZMINSHFT, ZMAXSHFT
      INTEGER IZONE, ISPHERE

      logical   :: L_FirstProf  ! flg to indicate first profile within domain

      DOUBLE PRECISION :: ELV

      MODNAM = 'INITER_DEM'

      L_FirstProf = .true.

      XBUTM = 0.0D0
      YBUTM = 0.0D0
      XNUTM = 0.0D0
      YNUTM = 0.0D0
      XBLON = 0.0D0
      YBLAT = 0.0D0

C*    Loop Over Terrain Files
      DEMLOOP: DO IDEM = 1, NUMDEM

C*       Assign J=IDEM for READ Statements
         J = IDEM

C*       Initialize Some Variables
         NUMPRF(IDEM) = 0

C*       Open the DEM File * Read Total Number of Elevation
C*       Profiles etc.
         REWIND (IDMUNT(IDEM))

         IF (FT(J) .NE. 1) THEN
C*          Open as standard formatted file
            OPEN(UNIT = IDMUNT(IDEM), FILE= DEMFIL(IDEM), ERR = 95)
            READ (IDMUNT(IDEM),51, ERR=97) 
     &       MAPN(J), FREEF(J), FILR1(J), PROCODE(J), FILR2(J), 
     &       SECTNL(J), MCTR(J), DEMLVL(J), ELEVPAT(J),
     &       IPLAN(J), IZO(J), (MPROJ(J,L), L = 1,15), CUNIT(J), 
     &       ELUNIT(J), SIDZ(J), ((DMCNR(J,L,M),L=1,2),M=1,4), 
     &       ELEVMN(J), ELEVMX(J), CNTRC(J),ACCUC(J), 
     &       DXM(J), DYM(J),DCI(J), NROW(J), NPROF(J), LPRIM(J),
     &       LPINT(J), SPRIM(J), SPINT(J), 
     &       DDATE(J), DINSP(J), INSPF(J), DVALD(J), SUSF(J), 
     &       VDAT(J), NADD(J), EDITN(J), PVOID(J)
         ELSE
C*          For FT(J) = 1 (no record delimiters) open as direct access file
            OPEN(UNIT = IDMUNT(IDEM), FILE= DEMFIL(IDEM),
     &         RECL = 1024, 
     &         ACCESS = 'DIRECT', 
     &         FORM ='FORMATTED',
     &         ERR = 95)
            READ (IDMUNT(IDEM), FMT=51, REC = 1, ERR=97) 
     &       MAPN(J), FREEF(J), FILR1(J), PROCODE(J), FILR2(J), 
     &       SECTNL(J), MCTR(J), DEMLVL(J), ELEVPAT(J),
     &       IPLAN(J), IZO(J), (MPROJ(J,L), L = 1,15), CUNIT(J), 
     &       ELUNIT(J), SIDZ(J), ((DMCNR(J,L,M),L=1,2),M=1,4), 
     &       ELEVMN(J), ELEVMX(J), CNTRC(J), ACCUC(J), 
     &       DXM(J),DYM(J),DCI(J), NROW(J), NPROF(J), LPRIM(J), 
     &       LPINT(J), SPRIM(J), SPINT(J), 
     &       DDATE(J), DINSP(J), INSPF(J), DVALD(J), SUSF(J), 
     &       VDAT(J), NADD(J), EDITN(J), PVOID(J)
         END IF

  51     FORMAT(2A40, A55, 2A1, A3, A4, 2I6,
     &          2I6, 15D24.15, I6,
     &          2I6, 4(2D24.15), 2D24.15, D24.15,
     &          I6, 2E12.6, E12.6, 2I6, 2(I5,I1), 
     &          2I4, A1, I1, I2,
     &          2I2, 2I4)


C*       Assign DEM zone from Record A to IZOND(NDEM) array
         IF (IZO(IDEM) .NE. 0) THEN
            IZOND(IDEM) = IZO(IDEM)
         END IF

C*       Open Temporary Direct Access Terrain Files and Index Files
         OPEN (IDRUNT(IDEM),FILE=DIRFIL(IDEM),
     &         ACCESS='DIRECT',RECL=LREC_DIR)
         OPEN (IDXUNT(IDEM),FILE=IDXFIL(IDEM))


C*       For Each Profile in the DEM File
         RC = 1

C*       Check for blank (0) NADD from DEM Record Type A
C*       This was already done in CHKADJ based on DEMCHK read,
C*       but needs to be checked again here for new read.
         IF (NADD(IDEM) .EQ. 0 .AND. IPLAN(IDEM) .EQ. 0) THEN
C*          Assume WGS72 for LAT files
            NADD(IDEM) = 2
         ELSE IF (NADD(IDEM) .EQ. 0 .AND. IPLAN(IDEM) .EQ. 1) THEN
C*          Assum NAD27 for UTM files
            NADD(IDEM) = 1
         END IF
         
         IF (.NOT. GOTDOMFLG) THEN
C*          No user-specified domain; skip domain shift calcs
            XDMNSHFT = 0.0D0
            YDMNSHFT = 0.0D0
            XDMXSHFT = 0.0D0
            YDMXSHFT = 0.0D0 
            GO TO 777
         END IF            
C*
C*       Determine whether to apply NAD shift to Domain
         IF (NADA .EQ. 0 .OR. NADA .EQ. NADD(IDEM)) THEN

            NADFLG = 0.0D0                ! No NAD shift needed

         ELSE IF( (NADA.EQ.1    .OR.  NADA.GE.5) .AND.
     &            (NADD(IDEM).GE.2 .AND. NADD(IDEM).LE.4) )THEN

            NADFLG = 1.0D0                ! Include NAD shift

         ELSE IF( (NADA.GE.2    .AND. NADA.LE.4) .AND.
     &            (NADD(IDEM).EQ.1 .OR.  NADD(IDEM).GE.5) )THEN

            NADFLG = 1.0D0                ! Include NAD shift

         ELSE

            NADFLG = 0.0D0                ! No NAD shift needed

         END IF

C*       Apply NAD shift if necessary, based on type of DEM data
         IF (TYPDEM .EQ. 'LAT') THEN
C*          Apply datum shift in arc-seconds for LAT files
            XDMNSHFT = DOMLL(2,1) + (XDMNDIFS*NADFLG)
            YDMNSHFT = DOMLL(1,1) + (YDMNDIFS*NADFLG)
            XDMXSHFT = DOMLL(2,3) + (XDMXDIFS*NADFLG)
            YDMXSHFT = DOMLL(1,3) + (YDMXDIFS*NADFLG)
         ELSE IF (TYPDEM .EQ. 'UTM') THEN
C*          Apply total shift in meters for UTM files
            XDMNSHFT = XDMIN + (XDMNDIFM*NADFLG)
            YDMNSHFT = YDMIN + (YDMNDIFM*NADFLG)
            XDMXSHFT = XDMAX + (XDMXDIFM*NADFLG)
            YDMXSHFT = YDMAX + (YDMXDIFM*NADFLG)
            IF (NADFLG .EQ. 0.0D0) THEN
C*             Use original domain zones if no NAD shift            
               ZMINSHFT = ZONMIN
               ZMAXSHFT = ZONMAX
            ELSE
C*             Use UTM zones based on shifted domain            
               ZMINSHFT = ZONMIN_SHFT
               ZMAXSHFT = ZONMAX_SHFT
            END IF
         END IF

C*       Loop through profiles to extract data within domain
C*       for this DEM file:

  777    CONTINUE

C*       Allocate array for number of nodes, NUMNOD(NPROF(IDEM))
C*       First deallocate if needed
         IF (ALLOCATED(NUMNOD)) DEALLOCATE(NUMNOD)
         ALLOCATE (NUMNOD(NPROF(IDEM)))
         
         DO IPROF = 1, NPROF(IDEM)

C*          Initialize Variables      
            NEWPRO = .TRUE.
            XNODEI = 0.0D0
            YNODEI = 0.0D0
            NUMNOD(IPROF) = 0
   
C*          Read Elevation From Each Profile Record.
            CALL GETPRO(IDEM,IPROF,IERR,RC)

C*          Check for error condition from GETPRO
            IF (IERR .NE. 0) GO TO 999

C*          Check to see if this profile lies between the min
C*          and maximum horizontal coords (LONG or UTMX)
            IF (.NOT. GOTDOMFLG) THEN
C*             No user-specified domain; use all profiles in data files
               GO TO 888                  
            END IF
            
            IF (TYPDEM .EQ. 'LAT' .AND. IPLAN(IDEM) .EQ. 0) THEN
C*             DEM/DOMAIN Type matches data file type
               IF (XBASE(IPROF) .LT. XDMNSHFT) THEN
                  CYCLE                        ! Cycle to next profile
               ELSE IF (XBASE(IPROF) .GT. XDMXSHFT) THEN
                  EXIT                         ! Exit profile loop
               END IF
            ELSE IF (TYPDEM .EQ. 'UTM' .AND. IPLAN(IDEM) .EQ. 1) THEN
C*             DEM/DOMAIN Type matches data file type
               IF (ZMINSHFT .EQ. ZMAXSHFT) THEN
                  IF (XBASE(IPROF) .LT. XDMNSHFT) THEN
                     CYCLE                      ! Cycle to next profile
                  ELSE IF (XBASE(IPROF) .GT. XDMXSHFT) THEN
                     EXIT                       ! Exit profile loop
                  END IF
               ELSE IF (IZO(IDEM) .EQ. ZMINSHFT) THEN
                  IF (XBASE(IPROF) .LT. XDMNSHFT) THEN
                     CYCLE                      ! Cycle to next profile
                  END IF
               ELSE IF (IZO(IDEM) .EQ. ZMAXSHFT) THEN
                  IF (XBASE(IPROF) .GT. XDMXSHFT) THEN
                     EXIT                       ! Exit profile loop
                  END IF
               END IF
            ELSE IF (TYPDEM .EQ. 'UTM' .AND. IPLAN(IDEM) .EQ. 0) THEN
C*             DEM/DOMAIN Type does NOT match data file type, convert xbase
C*             Calculate the Coords in UTM relative to NADA ellipsoid

               SELECT CASE (NADD(IDEM))
                 CASE (0)
                   ISPHERE = 4   ! UTM/LAT-LONG conversions based on GRS80 ellipsoid
                 CASE (2:4)
                   ISPHERE = 4   ! UTM/LAT-LONG conversions based on GRS80 ellipsoid
                 CASE (1,5:6)
                   ISPHERE = 0   ! UTM/LL conversions based on Clarke 1866 ellipsoid
                 CASE DEFAULT
                   ISPHERE = 4   ! DEFAULT CASE shouldn't occur
                 END SELECT

               ARGE  = XBASE(IPROF)
               ARGN  = YBASE(IPROF)
               IZONE = 0
               XARG  = 0.0D0
               YARG  = 0.0D0
               CALL UTMGEO (555,0,IZONE,XARG,YARG,ARGE,ARGN,ISPHERE)
               XBUTM = XARG
               YBUTM = YARG
               IF (ZMINSHFT .EQ. ZMAXSHFT) THEN
                  IF (XBUTM .LT. XDMNSHFT) THEN
                     CYCLE                     ! Cycle to next profile
                  ELSE IF (XBUTM .GT. XDMXSHFT) THEN
                     EXIT                      ! Exit profile loop
                  END IF
               ELSE IF (IZONE .EQ. ZMINSHFT) THEN
                  IF (XBUTM .LT. XDMNSHFT) THEN
                     CYCLE                     ! Cycle to next profile
                  END IF
               ELSE IF (IZONE .EQ. ZMAXSHFT) THEN
                  IF (XBUTM .GT. XDMXSHFT) THEN
                     EXIT                      ! Exit profile loop
                  END IF
               END IF
            ELSE IF (TYPDEM .EQ. 'LAT' .AND. IPLAN(IDEM) .EQ. 1) THEN
C*             DEM/DOMAIN Type does NOT match data file type, convert xbase
C*             Calculate the Coords in Lat/Lon relative to NADA ellipsoid

               SELECT CASE (NADD(IDEM))
                 CASE (0)
                   ISPHERE = 4   ! UTM/LAT-LONG conversions based on GRS80 ellipsoid
                 CASE (2:4)
                   ISPHERE = 4   ! UTM/LAT-LONG conversions based on GRS80 ellipsoid
                 CASE (1,5:6)
                   ISPHERE = 0   ! UTM/LL conversions based on Clarke 1866 ellipsoid
                 CASE DEFAULT
                   ISPHERE = 4   ! DEFAULT CASE shouldn't occur
                 END SELECT

               XARG  = XBASE(IPROF)
               YARG  = YBASE(IPROF)
               IZDUM = 0
               ARGE  = 0.0D0
               ARGN  = 0.0D0
               CALL UTMGEO (333,IZO(IDEM),IZDUM,XARG,YARG,
     &                                          ARGE,ARGN,ISPHERE)
               XBLON = ARGE
               YBLAT = ARGN
               IF (XBLON .LT. XDMNSHFT) THEN
                  CYCLE                        ! Cycle to next profile
               ELSE IF (XBLON .GT. XDMXSHFT) THEN
                  EXIT                         ! Exit profile loop
               END IF
            END IF

  888       CONTINUE
  
C*          For Each Node in this Profile
            DO INOD = 1, NODES(IPROF)

C*             Determine the Coordinates of this Point (L/L)   ---  CALL GETNOD
               CALL GETNOD(IPROF,INOD)

               IF (.NOT. GOTDOMFLG) THEN
C*                No user-specified domain; use all nodes in each profile
               
C*                Increment the Number of Nodes in this Profile
C*                Inside the Domain
                  NUMNOD(IPROF) = NUMNOD(IPROF)+1
               
                  IF (NEWPRO) THEN
               
C*                   NUMPRF is the number of profiles written to
C*                   the direct access file.
                     NUMPRF(IDEM) = NUMPRF(IDEM) + 1
               
                     NSTART = INOD
                     NEWPRO = .FALSE.
               
                     XNODEI = XNODE
                     YNODEI = YNODE
               
                  END IF
                  
               ELSE
               
C*                Check to See if This Node Lies Between the Min
C*                and Maximum vertical Coords (LAT or UTMY)

                  IF (TYPDEM .EQ. 'UTM' .AND. IPLAN(IDEM) .EQ. 0) THEN
C*                   DEM/DOMAIN Type does NOT match data file type, convert xbase
C*                   Calculate the Coords in UTM relative to NADA ellipsoid

                     SELECT CASE (NADD(IDEM))
                       CASE (0)
                         ISPHERE = 4   ! UTM/LAT-LONG conversions based on GRS80 ellipsoid
                       CASE (2:4)
                         ISPHERE = 4   ! UTM/LAT-LONG conversions based on GRS80 ellipsoid
                       CASE (1,5:6)
                         ISPHERE = 0   ! UTM/LL conversions based on Clarke 1866 ellipsoid
                       CASE DEFAULT
                         ISPHERE = 4   ! DEFAULT CASE shouldn't occur
                       END SELECT
               
                     ARGE  = XBASE(IPROF)
                     ARGN  = YNODE
                     IZONE = 0
                     XARG  = 0.0D0
                     YARG  = 0.0D0
                     CALL UTMGEO (555,0,IZONE,XARG,YARG,ARGE,ARGN,
     &                                                       ISPHERE)
                     XNUTM = XARG
                     YNUTM = YARG
                     
                  ELSE IF (TYPDEM.EQ.'LAT' .AND. IPLAN(IDEM).EQ.1) THEN
C*                   DEM/DOMAIN Type does NOT match data file type, convert xbase
C*                   Calculate the Coords in Lat/Lon relative to NADA ellipsoid

                     SELECT CASE (NADD(IDEM))
                       CASE (0)
                         ISPHERE = 4   ! UTM/LAT-LONG conversions based on GRS80 ellipsoid
                       CASE (2:4)
                         ISPHERE = 4   ! UTM/LAT-LONG conversions based on GRS80 ellipsoid
                       CASE (1,5:6)
                         ISPHERE = 0   ! UTM/LL conversions based on Clarke 1866 ellipsoid
                       CASE DEFAULT
                         ISPHERE = 4   ! DEFAULT CASE shouldn't occur
                       END SELECT
               
                     XARG  = XBASE(IPROF)
                     YARG  = YNODE
                     IZDUM = 0
                     ARGE  = 0.0D0
                     ARGN  = 0.0D0
                     CALL UTMGEO (333,IZO(IDEM),IZDUM,XARG,YARG,
     &                                                ARGE,ARGN,ISPHERE)
                     XBLON = ARGE
                     YBLAT = ARGN
                  END IF
               
                  IF ((TYPDEM.EQ.'UTM'.AND.IPLAN(IDEM).EQ.0 .AND.
     &                 YNUTM .GE. YDMNSHFT .AND.
     &                 YNUTM .LE. YDMXSHFT) .OR.
     &         
     &                (TYPDEM.EQ.'LAT'.AND.IPLAN(IDEM).EQ.1 .AND.
     &                 YBLAT .GE. YDMNSHFT .AND.
     &                 YBLAT .LE. YDMXSHFT) .OR.
     &         
     &                (YNODE .GE. YDMNSHFT .AND.
     &                 YNODE .LE. YDMXSHFT) ) THEN
     
C*                   This node is within the domain
                              
C*                   Increment the Number of Nodes in this Profile
C*                   Inside the Domain
                     NUMNOD(IPROF) = NUMNOD(IPROF)+1
               
                     IF (NEWPRO) THEN
               
C*                      NUMPRF is the number of profiles written to
C*                      the direct access file.
                        NUMPRF(IDEM) = NUMPRF(IDEM) + 1
               
                        NSTART = INOD
                        NEWPRO = .FALSE.
               
                        XNODEI = XNODE
                        YNODEI = YNODE
               
                     END IF
               
                  ELSE IF ((TYPDEM.EQ.'UTM'.AND.IPLAN(IDEM).EQ.0 .AND.
     &                      YNUTM .GT. YDMXSHFT) .OR.
     &         
     &                     (TYPDEM.EQ.'LAT'.AND.IPLAN(IDEM).EQ.1 .AND.
     &                      YBLAT .GT. YDMXSHFT) .OR.
     &         
     &                     (YNODE .GT. YDMXSHFT) ) THEN
     
C*                   This node is beyond the domain
              
                     EXIT                    ! Exit loop on nodes
               
                  END IF
                  
               END IF                  

C           End Loop on Nodes
            END DO

C*          If there were any nodes in this profile that were inside
C*          the domain, determine record numbers and write out data
C*          to direct access and index files
            IF (NUMNOD(IPROF) .GT. 0) THEN
C*             Write the baseline coords and number of nodes 
C*             in this profile to the master index file
               WRITE (IDXUNT(IDEM),IDXFRM) XNODEI, YNODEI,
     &                NUMNOD(IPROF), IZO(IDEM), MAXEL(IPROF)
               
C*             For each node in the profile inside the domain
               DO N = 1, NUMNOD(IPROF)
            
C*                Calculate record number for the node
                  JRECRD = JRECRD + 1

C*                Extract elevation for this node from array
                  ELV = DBLE(ZDEM(NSTART-1+N))

C ---             Convert elevation units and apply other adjustments, except for missing
                  IF (ELV .GT. -9000.0D0) THEN
C*                   Apply vertical resolution factor from header (DCI) to elevations
C                    from file, adjust for elevation of local datum (LOCEL), and
C                    convert from feet to meters, if necessary
                     IF (ELUNIT(IDEM) .EQ. 1) THEN
C                       Convert from feet to meters                     
                        ELV = ((ELV* DCI(IDEM))+ LOCEL(IPROF))* 0.3048D0
                     ELSE 
C                       Elevations in meters                     
                        ELV = (ELV* DCI(IDEM)) + LOCEL(IPROF)
                     END IF
                  END IF

C*                Write the elevations to a direct access file. 
                  CALL WRITEZ (IDEM,JRECRD,ELV)
                  
C              End Loop on Nodes
               END DO

            END IF

C*       End loop on profiles
         END DO
         
         IF (GOTDOMFLG) THEN
C ---       Write information to DOMDETAIL.OUT debug file
C           regarding direct access files
            IF (IDEM .EQ. 1) THEN
               WRITE(DOMK,50) NUMDEM
  50           FORMAT('From INITER_DEM:',
     &              //'Information on Direct Access Files Within ',
     &                                    'User-specified Domain:',
     &              //'No. of DEM Files = ',I6/)
            END IF
            WRITE(DOMK,60) IDEM, NUMPRF(IDEM)
  60        FORMAT('Direct Access File for DEM File No. ',I6,
     &             ' Contains ',I7,' Profiles.')
C ---       If direct access is not empty, NUMPRF(IDEM) > 0, 
C           loop through profiles to print out first and last 
            IF (NUMPRF(IDEM) .GT. 0) THEN
               L_FirstProf = .TRUE.
               DO K = 1, NPROF(IDEM)
                  IF (L_FirstProf .AND. NUMNOD(K) .GT. 0) THEN
C ---                This is the first non-empty profile, 
C                    or the first profile within the direct access file
                     WRITE(DOMK,70) XBASE(K),YBASE(K),NUMNOD(K)
  70                 FORMAT(/' XBASE, YBASE and NODES for First',
     &                                        ' Profile: ',2F16.4,I7)
                     L_FirstProf = .FALSE.
                  ELSE IF ((.NOT.L_FirstProf .AND. NUMNOD(K).EQ.0)) THEN
C ---                The last non-empty profile has just been passed,
C                    print out info for previous profile
                     WRITE(DOMK,80) XBASE(K-1),YBASE(K-1),NUMNOD(K-1)
  80                 FORMAT(' XBASE, YBASE and NODES for Last ',
     &                                        ' Profile: ',2F16.4,I7/)
                     EXIT
                  ELSE IF (K .EQ. NPROF(IDEM)) THEN
C ---                This is the last profile in the DEM file, this
C                    must also be the last profile in the direct access file
                     WRITE(DOMK,80) XBASE(K),YBASE(K),NUMNOD(K)
                     EXIT
                  END IF
               END DO
            ELSE
               WRITE(DOMK,*)
            END IF
         END IF

         CLOSE (IDXUNT(IDEM))
         CLOSE (IDRUNT(IDEM))
         CLOSE (IDMUNT(IDEM))
         JRECRD = 0
         
         CYCLE DEMLOOP
   
 95      CONTINUE
C        Error opening a DEM file 
        
         RUNERR = .TRUE.
         WRITE(DUMMY,'("DEM#",I4)') MIN(IDEM,9999)
         CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

         CLOSE (IDXUNT(IDEM))
         CLOSE (IDRUNT(IDEM))
         CLOSE (IDMUNT(IDEM))
         JRECRD = 0
         
         CYCLE DEMLOOP
         
 97      CONTINUE
C        Error reading a DEM file 
 
         RUNERR = .TRUE.
         WRITE(DUMMY,'("DEM#",I4)') MIN(IDEM,9999)
         CALL ERRHDL(PATH,MODNAM,'E','510',DUMMY)

         CLOSE (IDXUNT(IDEM))
         CLOSE (IDRUNT(IDEM))
         CLOSE (IDMUNT(IDEM))
         JRECRD = 0
         
         CYCLE DEMLOOP

C     End loop on dem files
      END DO DEMLOOP

C     Close DOMDETAIL.OUT debug file
      CLOSE(DOMK)

      WRITE(*,*) 'Exiting INITER_DEM'

999   RETURN
      end subroutine

      SUBROUTINE GETPRO (IDM,IPROF,IERR, RC)
C***********************************************************************
C*       PURPOSE:  THIS SUBROUTINE WILL READ X,Y NODE OF THE BASELINE AND
C*              THE Z VALUES FROM A DEM PROFILE
C*
C*       PROGRAMMER: Jayant Hardikar, Roger Brode
C*
C*       DATE:    September 29, 1995
C*
C*       MODIFIED: June 30, 2008
C*                
C*                Added error checking for maximum number of nodes,
C*                and modified read format to accept up to 5,000 nodes.
C*                Roger W. Brode, U.S. EPA, OAQPS, AQMG
C*                   
C*       INPUTS:  Raw Terrain Data (DEM) Files
C*
C*       OUTPUTS: Direct Access Binary Terrain Files
C*
C*       CALLED FROM:   MAIN
C***********************************************************************


C*    Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      SAVE

      CHARACTER*12 MODNAM

      INTEGER IERR, IDM, IPROF, ROWNUM, COLNUM, COLZ
      INTEGER L, J1, J2, KK, RR, RC

      MODNAM = 'GETPRO'
      IERR   = 0

      NODES(IPROF) = 0

      IF (FT(IDM) .NE. 1) THEN
         READ (IDMUNT(IDM), 208, ERR = 99, END = 99)
     &         ROWNUM, COLNUM, NODES(IPROF)
  208    FORMAT (3I6)

C*       Deallocate and reallocate ZDEM array for this profile
         IF (ALLOCATED(ZDEM)) DEALLOCATE(ZDEM)
         ALLOCATE(ZDEM(NODES(IPROF)))
         BACKSPACE (IDMUNT(IDM))  

         READ (IDMUNT(IDM),209,ERR=99,END=99)
     &         ROWNUM, COLNUM, 
     &         NODES(IPROF), COLZ, XBASE(IPROF), YBASE(IPROF),
     &         LOCEL(IPROF), MINEL(IPROF), MAXEL(IPROF),
     &         (ZDEM(L), L=1,NODES(IPROF))
C*        Read format supports up to 100,000 nodes per profile   
  209     FORMAT (2I6,2I6,2D24.15,D24.15,2D24.15,
     &            146I6,587(:/170I6),:/64I6,:)

      ELSE

         RC = RC + 1
         READ (IDMUNT(IDM),FMT=209, REC=RC, ERR = 99)
     &         ROWNUM, COLNUM, NODES(IPROF)

C*       Deallocate and reallocate ZDEM array for this profile
         IF (ALLOCATED(ZDEM)) DEALLOCATE(ZDEM)
         ALLOCATE(ZDEM(NODES(IPROF)))

         J1 = 1
         RR = 0
         IF (NODES(IPROF) .GT. 146) THEN
            J2 = 146
            RR = INT((NODES(IPROF) - 146)/170 + 1)
            IF (MODULO((NODES(IPROF) - 146),170) .EQ. 0) RR = RR - 1
         ELSE
            J2 = NODES(IPROF)
         END IF

         READ (IDMUNT(IDM),FMT=209, REC=RC, ERR = 99)
     &         ROWNUM, COLNUM, 
     &         NODES(IPROF), COLZ, XBASE(IPROF), YBASE(IPROF),
     &         LOCEL(IPROF), MINEL(IPROF), MAXEL(IPROF),
     &         (ZDEM(L), L=J1,J2)

         IF (RR .NE. 0) THEN
            DO KK = 1 , RR
              RC = RC + 1
              J1 = J2 + 1
              J2 = J1 + 170 - 1
              IF (J2 .GT. NODES(IPROF)) J2 = NODES(IPROF)
              READ (IDMUNT(IDM), FMT=211, REC=RC, ERR = 99)
     &             (ZDEM(L), L=J1,J2)
  211          FORMAT(170I6)
            END DO
         END IF
      END IF

      GO TO 999
      
99    RUNERR = .TRUE.
      IERR   = -1
      WRITE(DUMMY,'("DEM#",I4)') MIN(IDM,9999)
      CALL ERRHDL(PATH,MODNAM,'E','510',DUMMY)

999   RETURN

      end subroutine

      SUBROUTINE GETNOD(IPR,INO)
C***********************************************************************
C*               GETNOD Module of AERMAP Terrain Preprocessor
C*
C*       PURPOSE: Determine the Coordinates of a DEM Point (L/L)
C*
C*       PROGRAMMER: Jayant Hardikar, Roger Brode
C*
C*       DATE:    September 29, 1995
C*
C*       MODIFIED: June 30, 2008
C*                
C*                Corrected to use DYM, node spacing in 'Y' direction,
C*                to assign YNODE values in profile.
C*                Roger W. Brode, U.S. EPA, OAQPS, AQMG
C*                   
C*       INPUTS:  Profile Number, Node Number, Coordinates of Baseline
C*
C*       OUTPUTS: Coordinates of a DEM Point
C*
C*       CALLED FROM:   INITER
C***********************************************************************
       
C*    Variable Declarations
      USE MAIN1

      IMPLICIT NONE

      CHARACTER*12 MODNAM

      INTEGER INO, IPR

      MODNAM = 'GETNOD'

C*    For the baseline in a profile, the coords of a node are the same
c*    as the coords of the baseline
      IF (INO .EQ. 1) THEN
         XNODE = XBASE(IPR)
         YNODE = YBASE(IPR)

C*    Otherwise, calculate the coords based on the profile and node number
      ELSE
         XNODE = XBASE(IPR)      
         YNODE = YNODE + DYM(IDEM)
      END IF      
      
      RETURN
      end subroutine
