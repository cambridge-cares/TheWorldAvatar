      SUBROUTINE INITER_NED
C**********************************************************************
C*        INITER_NED Subroutine for the AERMAP Terrain Preprocessor
C*
C*        PURPOSE: Read elevation data from National Elevation Dataset 
C*                 (NED) files in GeoTIFF format. Extract elevations using 
C*                 parameters read/derived in NEDCHK from the TIFF Tags
C*                 and GeoKeys.
C*
C*        PROGRAMMER: Clint Tillerson, MACTEC
C*                    Roger Brode, US EPA, OAQPS, AQMG
C*
C*        DATE: February 9, 2009
C*
C*        GENERAL DESCRIPTION:
C*
C*        Elevations can be read as 1, 2, 4, or 8-byte values and 
C*        ultimately converted to 8-byte double precision values.  
C*        Data structure can be stripped or tiled.  Data is read row
C*        by row a single column starting at the northwest corner of 
C*        the file and stored in a 2-dimensional column major array,
C*        geoElev(col,row), to mimic the organization of profiles 
C*        and nodes in DEM files.
C*
C*        Byte-swapping is performed using the Fortran TRANSFER 
C*        function to handle the "endianness" of the raw data.
C* 
C*        The geoElev array is then processed to determine which 
C*        profiles (columns) and nodes (rows) are within the domain,
C*        if a domain is specified.  Elevations within the domain
C*        are written to direct access files, as with DEM data, for
C*        further processing. 
C*
C*        NOTE the difference in determining the max elevation
C*        within a profile when processing NED vs DEM data files: 
C*
C*        NED - max elevation represents only those nodes determined 
C*              to be inside the user domain coordinates
C*        DEM - max elevation is the max elevation read from the DEM
C*              file which represents the max elevation of all nodes 
C*              within a profile, regardless of the domain
C*
C*        The maximum elevations within the profile are used to
C*        optimize the hill height scale calculations within
C*        subroutine CALCHC.

C**********************************************************************      
      USE MAIN1
      Use TiffTags

      Implicit None
      
C --- 
      character (len=12) :: modnam
      
      integer  :: inUnt          ! short name for input file unit
      integer  :: ios            ! file i/o status
      integer  :: j,k,m          ! count     
      integer (kind=8) :: thisrec        ! current record to read in file 
      integer (kind=8) :: tilesAcross    ! width of image in tiles
      integer (kind=8) :: tilesDown      ! length (height) of image in tiles
      integer (kind=8) :: tileNum        ! number of current tile read
      integer (kind=8) :: tileCol        ! tile position (columns of tiles)
      integer (kind=8) :: tileRow        ! tile position (rows of tiles)
      integer (kind=8) :: tileFirstRow   ! first row in current tile
      integer (kind=8) :: tileFirstCol   ! first col in current tile
      integer (kind=8) :: tmprow         ! current data row read in file
      integer (kind=8) :: tmpcol         ! current data col read in file
      
      DOUBLE PRECISION :: ARGE, ARGN, XARG, YARG
      DOUBLE PRECISION :: XBUTM, YBUTM, XNUTM, YNUTM
      DOUBLE PRECISION :: XBLON, YBLAT
      
      INTEGER  :: ISPHERE, IZONE, ZMINSHFT, ZMAXSHFT
     
C --- initrecl:  Record length for unformatted data, set to 1 byte/record.
C     Some compilers/systems assume 4 bytes/record; use compiler option to 
C     interpret as bytes rather than words when needed;
C --- e.g., Compaq and Intel = /assume:byterecl
      integer, parameter :: datarecl = 1  ! record length (bytes) when reading elev data
      
      real (kind=4)     :: tmpflt4        ! temp floating point (real), 4-byte
      integer (kind=2)  :: tmpint2        ! temp integer, 2-byte
      integer (kind=4)  :: tmpint4        ! temp integer, 4-byte
      
C --- 1-byte integer array to read elevation data
      integer (kind=1)  :: tmpElvRd(8)
                                                   
C --- array to hold elevation data
      double precision, allocatable  :: geoElev(:,:)  ! size will be nCols X nRows
      
C --- other params for processing
      DOUBLE PRECISION :: profMaxElev ! max elevation for the current profile processed
      integer   :: elevRec     ! record number for writing elev to direct access file
      
      double precision  :: nadFlg ! nad flag to apply nad shift when needed
      double precision  :: nodeX  ! x-coordinate of node
      double precision  :: nodeY  ! y-coordinate of node
            
      logical   :: gotBase      ! flg to indicate base node found in a profile
      logical   :: L_FirstFile  ! flg to indicate first NED file within domain
      logical   :: L_FirstProf  ! flg to indicate first profile within domain

      modnam = 'INITER_NED'

C --- initialize flag to indicate base node not yet set
      gotBase     = .false.
      L_FirstFile = .true.
      L_FirstProf = .true.

C --- Initialize MAXPRF variable
      MAXPRF = 0      

      XBUTM = 0.0D0
      YBUTM = 0.0D0
      XNUTM = 0.0D0
      YNUTM = 0.0D0
      XBLON = 0.0D0
      YBLAT = 0.0D0
      ZMINSHFT = 0
      ZMAXSHFT = 0

C --- loop over NED files
      NEDLOOP: DO idem = 1, numdem      
 
         !initialilze temp array (1:8) used to read data      
         tmpElvRd = 0

C ----------------------------------------------------------------------        
C        Determine if NAD shift should be applied to the domain based
C        on differences in horizontal datum of domain and NED file. This
C        will be used later on to determine if a profile and node lies
C        within the domain coordinates.
C ----------------------------------------------------------------------  

C ---    No user-specified domain; skip domain shift calcs        
         IF (.NOT. GOTDOMFLG) THEN
            XDMNSHFT = 0.0D0
            YDMNSHFT = 0.0D0
            XDMXSHFT = 0.0D0
            YDMXSHFT = 0.0D0 
            ZMINSHFT = 0
            ZMAXSHFT = 0
            GO TO 777
         END IF      

C ---    Set NAD shift flag (NADFLG)
         IF (NADA .EQ. 0 .OR. NADA .EQ. NADD(IDEM)) THEN
C           No shift needed; NADA from .inp file is 0 or same as datum of NED file
            NADFLG = 0.0D0
         ELSE IF ((NADA.EQ.1 .OR. NADA.GE.5) .AND.
     &            (NADD(IDEM).GE.2 .AND. NADD(IDEM).LE.4)) THEN
C           NAD shift needed     
            NADFLG = 1.0D0 
         ELSE IF ((NADA.GE.2    .AND. NADA.LE.4) .AND.
     &            (NADD(IDEM).EQ.1 .OR.  NADD(IDEM).GE.5)) THEN
C           NAD shift needed     
            NADFLG = 1.0D0     
         ELSE
C           No NAD shift needed     
            NADFLG = 0.0D0
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
         
C ---    First check to see if any portion of NED file overlaps the domain
         IF (TYPDEM .EQ. 'LAT') THEN
            IF ((SELON_ARCS(IDEM) .LE. XDMNSHFT .AND. 
     &           NELON_ARCS(IDEM) .LE. XDMNSHFT) .OR.
     &          (SWLON_ARCS(IDEM) .GE. XDMXSHFT .AND. 
     &           NWLON_ARCS(IDEM) .GE. XDMXSHFT)) THEN
C ---          NED file is beyond longitude range of domain; issue warning,
C              set flag to skip min/max elev output, and cycle to next file
               WRITE (DUMMY,'(I8)') idem
               CALL ERRHDL(PATH,MODNAM,'W','380',DUMMY)
               L_NEDSkip(IDEM) = .TRUE.
               CYCLE NEDLOOP
            ELSE IF ((NELAT_ARCS(IDEM) .LE. YDMNSHFT .AND. 
     &                NWLAT_ARCS(IDEM) .LE. YDMNSHFT) .OR.
     &               (SELAT_ARCS(IDEM) .GE. YDMXSHFT .AND. 
     &                SWLAT_ARCS(IDEM) .GE. YDMXSHFT)) THEN
C ---          NED file is beyond latitude range of domain; issue warning,
C              set flag to skip min/max elev output, and cycle to next file
               WRITE (DUMMY,'(I8)') idem
               CALL ERRHDL(PATH,MODNAM,'W','380',DUMMY)
               L_NEDSkip(IDEM) = .TRUE.
               CYCLE NEDLOOP
            END IF
         ELSE IF (TYPDEM .EQ. 'UTM') THEN
            IF ((SE_UTMZ(IDEM) .LT. ZMINSHFT) .OR.
     &          (SE_UTMZ(IDEM)  .EQ. ZMINSHFT .AND.           
     &          (SEE_MTRS(IDEM) .LE. XDMNSHFT .AND. 
     &           NEE_MTRS(IDEM) .LE. XDMNSHFT)) .OR.
     &          (SW_UTMZ(IDEM)  .GT. ZMAXSHFT) .OR.
     &          (SW_UTMZ(IDEM)  .EQ. ZMAXSHFT .AND.
     &          (SWE_MTRS(IDEM) .GE. XDMXSHFT .AND. 
     &           NWE_MTRS(IDEM) .GE. XDMXSHFT))) THEN
C ---          NED file is beyond Easting range of domain; issue warning,
C              set flag to skip min/max elev output, and cycle to next file
               WRITE (DUMMY,'(I8)') idem
               CALL ERRHDL(PATH,MODNAM,'W','380',DUMMY)
               L_NEDSkip(IDEM) = .TRUE.
               CYCLE NEDLOOP
            ELSE IF ((NEN_MTRS(IDEM) .LE. YDMNSHFT .AND. 
     &                NWN_MTRS(IDEM) .LE. YDMNSHFT) .OR.
     &               (SEN_MTRS(IDEM) .GE. YDMXSHFT .AND. 
     &                SWN_MTRS(IDEM) .GE. YDMXSHFT)) THEN
C ---          NED file is beyond Northing range of domain; issue warning,
C              set min/max elevations to 0.0, and cycle to next file
               WRITE (DUMMY,'(I8)') idem
               CALL ERRHDL(PATH,MODNAM,'W','380',DUMMY)
               L_NEDSkip(IDEM) = .TRUE.
               CYCLE NEDLOOP
            END IF
         END IF
             
C ---    Branch point for NO DOMAIN option         
  777    CONTINUE               
                     
C ---    Allocate elevation array - size is nCols X nRows
         allocate(geoElev(nCols(idem),nRows(idem)))
C ---    Check for limit on other allocatable arrays         
         if (ncols(idem) .gt. MAXPRF) then
C           Deallocate and reallocate arrays         
            if(allocated(xbase)) deallocate(xbase)
            if(allocated(ybase)) deallocate(ybase)
            if(allocated(NODES)) deallocate(NODES)
            if(allocated(IZONP)) deallocate(IZONP)
            if(allocated(MAXEL)) deallocate(MAXEL)
            allocate(xbase(nCols(idem)),ybase(nCols(idem)))
            allocate(NODES(nCols(idem)),IZONP(nCols(idem)))
            allocate(MAXEL(nCols(idem)))
            MAXPRF = ncols(idem)
         end if
C ---    Initialize geoElev(nCols,nRows) array to zero            
         geoElev = 0.0D0

C ----------------------------------------------------------------------
C        Read elevation data from each GeoTIFF file
C ----------------------------------------------------------------------     


C ---    Reopen current NED file.
         open(unit=IDMUNT(idem), file=DEMFIL(idem), iostat=ios, 
     &        form='UNFORMATTED', action='READ', status='OLD', 
     &        access='DIRECT', err=9000, recl=datarecl)

C ---    Assign short name for input file unit, inUnt
         inUnt = IDMUNT(idem)

C ---    read data differently based on tiff type (strips or tiles)
         select case(tiffType(idem)) 
                 
C ---    read data using strip offsets
         case('strip')
            
            ! initialize tmprow; array assigned from top-down
            tmprow = nRows(idem) + 1
  
            ! loop over offsets array (strips)
            do j=1, size(dataOS(idem)%numbytes)           
               
               ! loop over rows in each strip
               do k=1, rowsPerStrip(idem)
               
                  ! increment row counter to keep track of current data row
                  tmprow = tmprow-1 
                  
                  ! last strip may have fewer rows
                  if (tmprow < 1) exit
                  
                  ! Compute record number for beginning of row and add 1
                  thisrec = dataOS(idem)%numbytes(j) + 
     &                 (((k-1)*nCols(idem)-1)*bytesPerSample(idem)) + 1
     
                  ! loop over columns
                  do m=1, nCols(idem)
                  
                     ! incement record number for next read
                     thisrec = thisrec + bytesPerSample(idem)
                    
                     ! read data
                     select case(bytesPerSample(idem))
                        
                     case(1)
                     ! 1-byte data
                        read(unit=inUnt, rec=thisrec, err=9010)
     &                     tmpElvRd(1)
     
                        ! Check for unsigned integer SampleFormat;
                        ! adjust for negative values if needed
                        if (SampleFormat(idem) == 1 .and.  
     &                                               tmpElvRd(1) < 0)
     &                     tmpElvRd(1) = tmpElvRd(1) + intAdj1
                        
                        ! convert to double
                        geoElev(m,tmprow) = MAX(-9999.0D0,
     &                                          DBLE(tmpElvRd(1)))
                           
                     case(2)
                     ! 2-byte data
                        read(unit=inUnt, rec=thisrec, err=9010)
     &                     tmpElvRd(1)
                        read(unit=inUnt, rec=thisrec+1, err=9010)
     &                     tmpElvRd(2)

                        ! transfer to 2-byte integer
                        if (byteSwap(idem)) then
                           tmpint2 = transfer((/tmpElvRd(2), 
     &                               tmpElvRd(1)/),tmpint2)
                        else
                           tmpint2 = transfer((/tmpElvRd(1), 
     &                               tmpElvRd(2)/),tmpint2)
                        end if
     
                        ! Check for unsigned integer SampleFormat;
                        ! adjust for negative values if needed
                        if (SampleFormat(idem) == 1 .and.  tmpint2 < 0)
     &                     tmpint2 = tmpint2 + intAdj2
                        
                        ! convert to double
                        geoElev(m,tmprow) = MAX(-9999.0D0,
     &                                          DBLE(tmpint2))

                     case(4)
                     ! 4-byte data
                        read(unit=inUnt, rec=thisrec, err=9010)
     &                     tmpElvRd(1)
                        read(unit=inUnt, rec=thisrec+1, err=9010)
     &                     tmpElvRd(2)
                        read(unit=inUnt, rec=thisrec+2, err=9010)
     &                     tmpElvRd(3)
                        read(unit=inUnt, rec=thisrec+3, err=9010)
     &                     tmpElvRd(4)
     
                        if (SampleFormat(idem) == 3) then
                        ! transfer to 4-byte floating point (real)
                           if (byteSwap(idem)) then
                              tmpflt4 = transfer((/tmpElvRd(4), 
     &                                 tmpElvRd(3),tmpElvRd(2), 
     &                                 tmpElvRd(1)/),tmpflt4)   
                           else
                              tmpflt4 = transfer((/tmpElvRd(1), 
     &                                 tmpElvRd(2),tmpElvRd(3), 
     &                                 tmpElvRd(4)/),tmpflt4)
                           end if
                              
                           ! convert to double 
                           geoElev(m,tmprow) = MAX(-9999.0D0,
     &                                          DBLE(tmpflt4))

                        else
                        ! transfer to 4-byte integer
                           if (byteSwap(idem)) then
                              tmpint4 = transfer((/tmpElvRd(4), 
     &                                 tmpElvRd(3),tmpElvRd(2), 
     &                                 tmpElvRd(1)/),tmpint4)   
                           else
                              tmpint4 = transfer((/tmpElvRd(1), 
     &                                 tmpElvRd(2),tmpElvRd(3), 
     &                                 tmpElvRd(4)/),tmpint4)
                           end if
                              
                           ! Check for unsigned integer SampleFormat;
                           ! adjust for negative values if needed
                           if (SampleFormat(idem) == 1 .and. 
     &                                                  tmpint4 < 0)
     &                        tmpint4 = tmpint4 + intAdj4

                           ! convert to double 
                           geoElev(m,tmprow) = MAX(-9999.0D0,
     &                                          DBLE(tmpint4))
                              
                        end if

                     case(8)
                     ! 8-byte data
                        read(unit=inUnt, rec=thisrec, err=9010)
     &                     tmpElvRd(1)
                        read(unit=inUnt, rec=thisrec+1, err=9010)
     &                     tmpElvRd(2)
                        read(unit=inUnt, rec=thisrec+2, err=9010)
     &                     tmpElvRd(3)
                        read(unit=inUnt, rec=thisrec+3, err=9010)
     &                     tmpElvRd(4)
                        read(unit=inUnt, rec=thisrec+4, err=9010)
     &                     tmpElvRd(5)
                        read(unit=inUnt, rec=thisrec+5, err=9010)
     &                     tmpElvRd(6)
                        read(unit=inUnt, rec=thisrec+6, err=9010)
     &                     tmpElvRd(7)
                        read(unit=inUnt, rec=thisrec+7, err=9010)
     &                     tmpElvRd(8)
     
                        ! transfer to temp array (row,col)
                        if (byteSwap(idem)) then
                           geoElev(m,tmprow) = MAX(-9999.0D0,
     &                        transfer((/tmpElvRd(8),
     &                        tmpElvRd(7),tmpElvRd(6),tmpElvRd(5),
     &                        tmpElvRd(4),tmpElvRd(3),tmpElvRd(2),
     &                        tmpElvRd(1)/),geoElev(m,tmprow)))
                        else
                           geoElev(m,tmprow) = MAX(-9999.0D0,
     &                        transfer((/tmpElvRd(1),
     &                        tmpElvRd(2),tmpElvRd(3),tmpElvRd(4),
     &                        tmpElvRd(5),tmpElvRd(6),tmpElvRd(7),
     &                        tmpElvRd(8)/),geoElev(m,tmprow)))
                        end if
     
                     end select                      
                        
                  end do
                     
               end do
            end do

C ---    read data using strip offsets
         case('tile')
            
            ! initialize tileNum = 0
            tileNum = 0

            ! compute number of tiles across and down
            tilesAcross = (nCols(idem)+tileWid(idem)-1)/tileWid(idem)
            tilesDown   = (nRows(idem)+tileLen(idem)-1)/tileLen(idem)
  
            ! loop over offsets array (tiles)
            do j=1, size(dataOS(idem)%numbytes)

               ! compute number of current tile, tile row, tile col
               ! first row in tile, first col in tile
               tileNum = tileNum+1
               tileRow = int((tileNum-1)/tilesAcross)+1
               tileCol = mod(tileNum-1,tilesAcross)+1
               tileFirstRow = ((tileRow-1)*tileLen(idem))+1
               tileFirstCol = ((tileCol-1)*tileWid(idem))+1
               
               ! compute current data row as index for geoElev array, 
               ! decreasing from top of file to bottom, incremented by 2
               ! since it is decremented at beginning of loop below
               tmprow = nRows(idem) - tileFirstRow + 2
               
               ! loop over rows in tile
               do k=1,tileLen(idem)
                  ! decrement row and compute column (decrement by 1 since
                  ! incremented at beginning of loop below)
                  tmprow = tmprow-1
                  tmpcol = tileFirstCol-1
                  
                  ! if row padding exists, exit loop when tmprow is less than 1;
                  ! row padding exists on last (bottom) row of tiles if 
                  ! tileLen is not a multiple of total data rows (nRows)
                  if (tmprow < 1) then
                     exit
                  end if
                  
                  ! compute record number for beginning of row, plus 1
                  thisRec = dataOS(idem)%numbytes(j) +
     &                  ((k-1)*tileWid(idem)-1)*bytesPerSample(idem) + 1
     
                  ! loop over columns in tile
                  do m=1,tileWid(idem)
                  
                     ! increment column
                     tmpcol = tmpcol+1

                     ! increment record number for next read
                     thisrec = thisrec + bytesPerSample(idem)

                     ! if column padding exists, exit loop when tmpcol is 
                     ! greater than nCols; no need to cycle through padded data
                     ! col padding exists on last col of tiles if tileWid 
                     ! is not a multiple of the total data cols (nCols)
                     if (tmpcol > nCols(idem)) then
                        exit
                     end if
                     
                     ! read record
                     select case(bytesPerSample(idem))
                     
                     case(1)
                     ! 1-byte data
                        read(unit=inUnt, rec=thisrec, err=9010)
     &                     tmpElvRd(1)
     
                        ! Check for unsigned integer SampleFormat;
                        ! adjust for negative values if needed
                        if (SampleFormat(idem) == 1 .and.
     &                                               tmpElvRd(1) < 0)
     &                      tmpElvRd(1) = tmpElvRd(1) + intAdj1
                        
                        ! convert to double
                        geoElev(tmpcol,tmprow) = MAX(-9999.0D0,
     &                                               DBLE(tmpElvRd(1)))
                        
                     case(2)
                     ! 2-byte data
                        read(unit=inUnt, rec=thisrec, err=9010)
     &                     tmpElvRd(1)
                        read(unit=inUnt, rec=thisrec+1, err=9010)
     &                     tmpElvRd(2)

                        ! transfer to 2-byte integer
                        if (byteSwap(idem)) then
                           tmpint2 = transfer((/tmpElvRd(2), 
     &                               tmpElvRd(1)/),tmpint2)
                        else
                           tmpint2 = transfer((/tmpElvRd(1), 
     &                               tmpElvRd(2)/),tmpint2)
                        end if
     
                        ! Check for unsigned integer SampleFormat;
                        ! adjust for negative values if needed
                        if (SampleFormat(idem) == 1 .and. tmpint2 < 0)
     &                      tmpint2 = tmpint2 + intAdj2
                        
                        ! convert to double
                        geoElev(tmpcol,tmprow) = MAX(-9999.0D0,
     &                                               DBLE(tmpint2))

                     case(4)
                     ! 4-byte data
                        read(unit=inUnt, rec=thisrec, err=9010)
     &                     tmpElvRd(1)
                        read(unit=inUnt, rec=thisrec+1, err=9010)
     &                     tmpElvRd(2)
                        read(unit=inUnt, rec=thisrec+2, err=9010)
     &                     tmpElvRd(3)
                        read(unit=inUnt, rec=thisrec+3, err=9010)
     &                     tmpElvRd(4)
     
                        if (SampleFormat(idem) == 3) then
                        ! transfer to 4-byte floating point (real)
                           if (byteSwap(idem)) then
                              tmpflt4 = transfer((/tmpElvRd(4), 
     &                                 tmpElvRd(3),tmpElvRd(2), 
     &                                 tmpElvRd(1)/),tmpflt4)   
                           else
                              tmpflt4 = transfer((/tmpElvRd(1), 
     &                                 tmpElvRd(2),tmpElvRd(3), 
     &                                 tmpElvRd(4)/),tmpflt4)
                           end if
                           
                           ! convert to double 
                           geoElev(tmpcol,tmprow) = MAX(-9999.0D0,
     &                                                  DBLE(tmpflt4))
                           
                        else 
                        ! transfer to signed 4-byte integer
                           if (byteSwap(idem)) then
                              tmpint4 = transfer((/tmpElvRd(4), 
     &                                 tmpElvRd(3),tmpElvRd(2), 
     &                                 tmpElvRd(1)/),tmpint4)   
                           else
                              tmpint4 = transfer((/tmpElvRd(1), 
     &                                 tmpElvRd(2),tmpElvRd(3), 
     &                                 tmpElvRd(4)/),tmpint4)
                           end if
                           
                           ! Check for unsigned integer SampleFormat;
                           ! adjust for negative values if needed
                           if (SampleFormat(idem) == 1 .and.
     &                                                  tmpint4 < 0)
     &                         tmpint4 = tmpint4 + intAdj4
                                                      
                           ! convert to double 
                           geoElev(tmpcol,tmprow) = MAX(-9999.0D0,
     &                                                  DBLE(tmpint4))
                           
                        end if

                     case(8)
                     ! 8-byte data
                        read(unit=inUnt, rec=thisrec, err=9010)
     &                     tmpElvRd(1)
                        read(unit=inUnt, rec=thisrec+1, err=9010)
     &                     tmpElvRd(2)
                        read(unit=inUnt, rec=thisrec+2, err=9010)
     &                     tmpElvRd(3)
                        read(unit=inUnt, rec=thisrec+3, err=9010)
     &                     tmpElvRd(4)
                        read(unit=inUnt, rec=thisrec+4, err=9010)
     &                     tmpElvRd(5)
                        read(unit=inUnt, rec=thisrec+5, err=9010)
     &                     tmpElvRd(6)
                        read(unit=inUnt, rec=thisrec+6, err=9010)
     &                     tmpElvRd(7)
                        read(unit=inUnt, rec=thisrec+7, err=9010)
     &                     tmpElvRd(8)
     
                        ! transfer to temp array (row,col)
                        if (byteSwap(idem)) then
                           geoElev(tmpcol,tmprow) = MAX(-9999.0D0,
     &                        transfer((/tmpElvRd(8),
     &                        tmpElvRd(7),tmpElvRd(6),tmpElvRd(5),
     &                        tmpElvRd(4),tmpElvRd(3),tmpElvRd(2),
     &                        tmpElvRd(1)/),geoElev(tmpcol,tmprow)))
                        else
                           geoElev(tmpcol,tmprow) = MAX(-9999.0D0,
     &                        transfer((/tmpElvRd(1),
     &                        tmpElvRd(2),tmpElvRd(3),tmpElvRd(4),
     &                        tmpElvRd(5),tmpElvRd(6),tmpElvRd(7),
     &                        tmpElvRd(8)/),geoElev(tmpcol,tmprow)))
                        end if
     
                     end select                      
     
                  end do ! tileWid, cols
               end do ! tileLen, rows
            end do ! tile offsets

         end select

C ---    Adjust values in geoElev array if needed for elevation
C        units, vertical resolution factor (DCI) or vertical 
C        reference elevation (tiePtz); first check to see if 
C ---    elevation is "missing" (-9999.0)
         if (ELUNIT(idem) == 1 .or. DCI(idem) /= 1.0D0 .or.
     &       tiePtz(idem) /= 0.0D0) then         
            do j=1,nRows(idem)
               do k=1,nCols(idem)
                  if (geoElev(k,j) /= -9999.0D0) then
                     if (ELUNIT(idem) == 1) then 
C ---                   Apply vertical resolution factor (DCI) to elevations
C                       from file, adjust for elevation of local datum (tiePtz), 
C                       and convert from feet to meters
                        geoElev(k,j) = 
     &                ((geoElev(k,j)*DCI(idem))+tiePtz(idem)) * 0.3048D0
                     else 
C ---                   Apply adjustments for vertical resolution (DCI) and elevation 
C                       of local datum (tiePtz) (units already in meters)
                        geoElev(k,j) = 
     &                         ((geoElev(k,j)*DCI(idem)) + tiePtz(idem))
                     end if
                  end if
               end do
            end do
         end if

C ---    Assign minimum and maximum elevations for file
         elevmn(idem) = MINVAL(geoElev)
         elevmx(idem) = MAXVAL(geoElev)

C ---    Open temp index file to store profile params         
         OPEN (IDRUNT(idem),FILE=DIRFIL(idem),
     &         ACCESS='DIRECT',RECL=LREC_DIR)

C ---    Open temp direct access file to store elevations     
         OPEN (IDXUNT(idem),FILE=IDXFIL(idem))

C ---    Loop over elevation array extracted from NED file.
c        Derive base node coordinates and determine if profile
c        lies within the domain.  If not, go to next profile.
c        If so, determine coords of each node and determine if 
c        node is in domain.  Keep a count of the number of profiles
c        in the domain and the number of nodes in each profile,
c        and get the max elev. in each profile.

c        If base node is not in profile, then base node should be
c        reset to be the first node of a profile that is in the domain.

         elevRec = 0  ! record reference for write statement in writez
         NODES   = 0  ! number of nodes per profile
         NUMPRF(idem) = 0 ! number of profiles in domain of current file
         do j=1,nCols(idem)

c           initialize flag to indicate base node not yet set
            gotBase = .false.
            profMaxElev = 0.0D0
            
c           For current profile, calculate coordinates of base node
c           (XBASE, YBASE)

            if (IPLAN(idem) .eq. 0) then
c              lat of all base nodes = lat of sw corner
               YBASE(j) = SWLAT_ARCS(idem)
            
c              lon of base node = lon of sw corner+(pixel scale X * (col-1))
               XBASE(j) = SWLON_ARCS(idem) + (DXM(idem) * dble(j-1))
            
            else if (IPLAN(idem) .eq. 1) then
c              Northing of all base nodes = Northing of sw corner
               YBASE(j) = SWN_MTRS(idem)
            
c              Easting of base node = Easting of sw corner+(pixel scale X * (col-1))
               XBASE(j) = SWE_MTRS(idem) + (DXM(idem) * dble(j-1))
               
            end if            

C*          Check to see if this profile lies between the min
C*          and maximum horizontal coords (LONG or UTMX)
            IF (.NOT. GOTDOMFLG) THEN
C*             No user-specified domain; use all profiles in data files
               GO TO 888                  
            END IF
            
            IF (TYPDEM .EQ. 'LAT' .AND. IPLAN(IDEM) .EQ. 0) THEN
C*             DEM/DOMAIN Type matches data file type
               IF (XBASE(j) .LT. XDMNSHFT) THEN
                  CYCLE                        ! Cycle to next profile
               ELSE IF (XBASE(j) .GT. XDMXSHFT) THEN
                  EXIT                         ! Exit profile loop
               END IF
            ELSE IF (TYPDEM .EQ. 'UTM' .AND. IPLAN(IDEM) .EQ. 1) THEN
C*             DEM/DOMAIN Type matches data file type
               IF (ZMINSHFT .EQ. ZMAXSHFT) THEN
                  IF (XBASE(j) .LT. XDMNSHFT) THEN
                     CYCLE                      ! Cycle to next profile
                  ELSE IF (XBASE(j) .GT. XDMXSHFT) THEN
                     EXIT                       ! Exit profile loop
                  END IF
               ELSE IF (IZO(IDEM) .EQ. ZMINSHFT) THEN
                  IF (XBASE(j) .LT. XDMNSHFT) THEN
                     CYCLE                      ! Cycle to next profile
                  END IF
               ELSE IF (IZO(IDEM) .EQ. ZMAXSHFT) THEN
                  IF (XBASE(j) .GT. XDMXSHFT) THEN
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

               ARGE  = XBASE(j)
               ARGN  = YBASE(j)
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

               XARG  = XBASE(j)
               YARG  = YBASE(j)
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

C ---       Initialize number of nodes for this column
            nodes(j) = 0
            
            do k=1,nRows(idem)
            
C ---          Derive coordinates of present node to determine
c              if node is in domain.

c              Lon = XBASE derived above  
               nodeX = XBASE(j)   
               
               if (IPLAN(idem) .eq. 0) then               
c                 Lat =  lat of sw corner + (pixel scale Y * (row-1))    
                  nodeY = SWLAT_ARCS(idem) + (DYM(idem) * dble(k-1))
                  
               else if (IPLAN(idem) .eq. 1) then
c                 Northing =  Northing of sw corner + (pixel scale Y * (row-1))    
                  nodeY = SWN_MTRS(idem) + (DYM(idem) * dble(k-1))
                  
               else
c                 Invalid IPLAN code for this file; write Error message               
                  WRITE(DUMMY,'("NED#",I4)') MIN(IDEM,9999)
                  CALL ERRHDL(PATH,MODNAM,'E','510',DUMMY)
                  RUNERR = .TRUE.
                  goto 7000
                  
               end if

               if (.NOT.GOTDOMFLG) then
c                 Increment number of nodes in current profile
                  nodes(j) = nodes(j) + 1

c                 Increment elevation record for writing                
                  elevRec = elevRec + 1
                  CALL WRITEZ (idem,elevRec,geoElev(j,k)) 
               
c                 If this is the first node of current profile 
c                 found in domain, set base node coordinates  
                  if (.not. gotBase) then
                     XBASE(j) = nodeX
                     YBASE(j) = nodeY
                     gotBase = .true.
                     profMaxElev =  geoElev(j,k)
c                    Increment number of profiles in NED file 
c                    with nodes in the domain              
                     NUMPRF(idem) = NUMPRF(idem) + 1
                  else
                     if (geoElev(j,k) > profMaxElev) then
                        profMaxElev =  geoElev(j,k)
                     end if
                  end if
                  
               else
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
               
                     ARGE  = XBASE(j)
                     ARGN  = nodeY
                     IZONE = 0
                     XARG  = 0.0D0
                     YARG  = 0.0D0
                     CALL UTMGEO (555,0,IZONE,XARG,YARG,
     &                                        ARGE,ARGN,ISPHERE)
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
               
                     XARG  = XBASE(j)
                     YARG  = nodeY
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
     &                (nodeY .GE. YDMNSHFT .AND.
     &                 nodeY .LE. YDMXSHFT) ) THEN
     
C*                   This node is within the domain
                              
c                    Increment number of nodes in current profile
                     nodes(j) = nodes(j) + 1

c                    Increment elevation record for writing                
                     elevRec = elevRec + 1
                     CALL WRITEZ (idem,elevRec,geoElev(j,k)) 
               
c                    If this is the first node of current profile 
c                    found in domain, set base node coordinates  
                     if (.not. gotBase) then
                        XBASE(j) = nodeX
                        YBASE(j) = nodeY
                        gotBase = .true.
                        profMaxElev =  geoElev(j,k)
c                       Increment number of profiles in NED file 
c                       with nodes in the domain              
                        NUMPRF(idem) = NUMPRF(idem) + 1
                     else
                        if (geoElev(j,k) > profMaxElev) then
                           profMaxElev =  geoElev(j,k)
                        end if
                     end if
               
                  ELSE IF ((TYPDEM.EQ.'UTM'.AND.IPLAN(IDEM).EQ.0 .AND.
     &                      YNUTM .GT. YDMXSHFT) .OR.
     &         
     &                     (TYPDEM.EQ.'LAT'.AND.IPLAN(IDEM).EQ.1 .AND.
     &                      YBLAT .GT. YDMXSHFT) .OR.
     &         
     &                     (nodeY .GT. YDMXSHFT) ) THEN
C*                   This node is beyond the domain
              
                     EXIT                    ! Exit loop on nodes (rows)
               
                  END IF
 
               end if
  
            end do ! loop over rows

C ---       Write profile base coordinates, num or nodes, profile UTM zone
C           and profile max elevation to index file               
            WRITE (IDXUNT(idem),IDXFRM) XBASE(j), YBASE(j),
     &             nodes(j), IZO(IDEM), profMaxElev

         end do ! loop over cols              
                     
         IF (GOTDOMFLG) THEN
C ---       Write information to DOMDETAIL.OUT debug file
C           regarding direct access files
            IF (L_FirstFile) THEN
               L_FirstFile = .FALSE.
               WRITE(DOMK,50) NUMDEM
  50           FORMAT('From INITER_NED:',
     &              //'Information on Direct Access Files Within ',
     &                                    'User-specified Domain:',
     &              //'No. of NED Files = ',I6/)
            END IF
            WRITE(DOMK,60) IDEM, NUMPRF(IDEM)
  60        FORMAT('Direct Access File for NED File No. ',I6,
     &             ' Contains ',I7,' Profiles.')
            IF (NUMPRF(IDEM) .GT. 0) THEN
               L_FirstProf = .TRUE.
               DO j = 1, nCols(idem)
                  IF (L_FirstProf .AND. NODES(j) .GT. 0) THEN
                     WRITE(DOMK,70) XBASE(j),YBASE(j),NODES(j)
  70                 FORMAT(/' XBASE, YBASE and NODES for First',
     &                                        ' Profile: ',2F16.4,I7)
                     L_FirstProf = .FALSE.
                  ELSE IF ((.NOT.L_FirstProf .AND. NODES(J).EQ.0)) THEN
                     WRITE(DOMK,80) XBASE(j-1),YBASE(j-1),NODES(j-1)
  80                 FORMAT(' XBASE, YBASE and NODES for Last ',
     &                                        ' Profile: ',2F16.4,I7/)
                     EXIT
                  ELSE IF (J .EQ. nCols(idem)) THEN
                     WRITE(DOMK,80) XBASE(j),YBASE(j),NODES(j)
                     EXIT
                  END IF
               END DO
            ELSE
               WRITE(DOMK,*)
            END IF
         END IF

         goto 7000
         
 9000    write(*,9005) DEMFIL(idem)
 9005    format(/,' There was a problem opening the NED file:'
     &          /,' ',a)
      
         WRITE(DUMMY,'("NED#",I4)') MIN(IDEM,9999)
         CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)
         RUNERR = .TRUE.
         goto 7000 
            
 9010    write(*,9015) DEMFIL(idem)
 9015    format(/,' There was a problem reading the NED file:'
     &          /,' ',a)
      
         WRITE(DUMMY,'("NED#",I4)') MIN(IDEM,9999)
         CALL ERRHDL(PATH,MODNAM,'E','510',DUMMY)
         RUNERR = .TRUE.
         goto 7000 

 7000    continue
     
         close(inUnt)
         close(idxunt(idem))
         close(idrunt(idem))
         deallocate(geoElev)
      
      end do NEDLOOP

      CLOSE(DOMK)

C*    Reopen MAPPARAMS.OUT file to include Min/Max elevations
      OPEN (UNIT = MAPK, FILE = MAPPARAMS_FILE, STATUS = 'OLD',
     &      POSITION = 'APPEND',ERR = 99)
     
      WRITE(MAPK,5010)
5010  FORMAT(//1X,'From INITER_NED:',
     &       //1X,'MIN and MAX Elevations for NED files:')
     
      DO IDEM = 1, NUMDEM
         WRITE(MAPK,5012) IDEM, MAPN(IDEM)
5012     FORMAT(/1X,'NED File #: ',I6,
     &          /1X,A40)

         IF (L_NEDSkip(IDEM)) THEN
C ---       This NED file is entirely outside the domain, processing was skipped
            WRITE(MAPK,5022) 
5022        FORMAT(3X,'This file is outside the domain.',
     &            /3X,'Elevation data were not processed.')
         ELSE IF (L_UserElevUnits(idem)) THEN
C ---       Include message regarding user-specified elevation units         
            WRITE(MAPK,5023) ELEVMN(idem), Chr_UserElevUnits(idem),
     &                       ELEVMX(idem), Chr_UserElevUnits(idem)
5023        FORMAT(3X,'Min. Elevation: ',F8.1, 1X, A11,
     &            /3X,'Max. Elevation: ',F8.1, 1X, A11,
     &            /3X,'Based on user-specified units.')
         ELSE
            WRITE(MAPK,5024) ELEVMN(idem), LVLN(ELUNIT(idem)),
     &                       ELEVMX(idem), LVLN(ELUNIT(idem))
5024        FORMAT(3X,'Min. Elevation: ',F8.1, 1X, A6
     &            /3X,'Max. Elevation: ',F8.1, 1X, A6)
         END IF
      END DO
      CLOSE(MAPK)            
      
      GO TO 999
      
99    CONTINUE

C --- Error opening MAPPARAMS.OUT file
      CALL ERRHDL(PATH,MODNAM,'E','500','MAPPARMS')
      RUNERR = .TRUE.
      
999   CONTINUE      
      
      write(*,*) 'Exiting INITER_NED'
      
      RETURN

      END SUBROUTINE
