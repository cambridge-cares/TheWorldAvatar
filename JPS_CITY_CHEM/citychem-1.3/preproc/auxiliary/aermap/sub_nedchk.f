      SUBROUTINE NEDCHK
C**********************************************************************
C*        NEDCHK Subroutine for the AERMAP Terrain Preprocessor
C*
C*        PURPOSE: Check NED file in GeoTIFF format to determine if it 
C*                 meets minimum criteria to extract NED data.  Calls 
c*                 Subroutine read_tifftags which returns arrays that 
C*                 store all TIFF Tags, GeoKeys and their values.  
C*                 Evaluates certains tags and keys for expected values.
C*
C*        PROGRAMMER: Clint Tillerson, MACTEC
C*                    Roger Brode, US EPA, OAQPS, AQMG
C*
C*        DATE:  February 9, 2009
C*     
C*
C***********************************************************************

      use main1
      use TiffTags

      implicit none
      
      INTEGER  :: utm_zone ! utm zone function
      
      integer  :: i,j,k   ! loop counters
      integer  :: tCnt    ! tiff tag counter
      integer  :: gCnt    ! geokey counter
      
      character (len=12) :: modnam
      
      character (len=8)  :: dumtag  ! character string for tTag for ERRHDL
      character (len=8)  :: dumflg  ! character string for gKey for ERRHDL
       
      ! got tag flags - need these flags for checks that have dependencies
      logical  :: geoTypeFlg      ! geographic type flag (NAD27, NAD83, WGS84)
      logical  :: geoDatumFlg     ! geographic datum flag (NAD27, NAD83, WGS84)
      logical  :: geoEllipseFlg   ! geographic ellipsoid flag (Clarke 1866, GRS80, WGS84)
      logical  :: projCStypeFlg   ! projected CS type flag (UTM projected data)
      logical  :: RasterTypeFlg   ! GT Raster Type flag (PixelIsArea or PixelIsPoint)
      logical  :: PixelIsArea     ! GT Raster Type = PixelIsArea
      logical  :: PixelIsPoint    ! GT Raster Type = PixelIsPoint
      logical  :: tiePntFlg       ! model tie point
      logical  :: pxlSclFlg       ! pixel scale
      logical  :: stripOSFlg      ! data strip offsets 
      logical  :: rowsPerStripFlg ! rows per strip
      logical  :: tileOSFlg       ! data tile offsets
      logical  :: tileLenFlg      ! tile length flag
      logical  :: tileWidFlg      ! tile width flag
      logical  :: elvUntsFlg      ! elevation units flag
      logical  :: geogUntsFlg     ! geographic angular units flag
      logical  :: projUntsFlg     ! projected linear units flag
      
      integer (kind=8) :: ModelType   ! Model Type code (1 for UTM; 2 for geographic)
      integer (kind=8) :: geoType     ! geographic type (NAD27, NAD83, WGS84)
      integer (kind=8) :: geoDatum    ! geographic datum (NAD27, NAD83, WGS84)
      integer (kind=8) :: geoEllipse  ! geographic ellipsoid (Clarke 1866, GRS80, WGS84)
      integer (kind=8) :: projCStype  ! projected CS type value (datum and UTM zone)
      integer (kind=8) :: geogUnts    ! geographic angular units code
      integer (kind=8) :: projUnts    ! projected linear units code
      
      ! tie point anchor in raster space
      double precision :: tiePtAncx   ! anchor point for tie point, x
      double precision :: tiePtAncy   ! anchor point for tie point, y
      double precision :: tiePtAncz   ! anchor point for tie point, z
      
      type tagFlgs
         integer (kind=4)  :: id        ! tag id        
         character(len=20) :: name      ! tag name
         logical           :: gotTag    ! flag to indicate tag is defined in tiff file
         integer (kind=8)  :: tNdx      ! array index in tag array
      end type tagFlgs
      
      type(tagFlgs)        :: tFlgs(14)  ! tiff tags recognized by AERMAP
      type(tagFlgs)        :: gFlgs(9)   ! geokeys recognized by AERMAP

C --- set module name      
      modnam = 'NEDCHK'

      L_NeedNADCON = .false.
      
C --- Allocate SampleFormat array
      allocate (SampleFormat(ndem))
      
C --- initialize tag and geokey counters
      tCnt = 0
      gCnt = 0

C --- initialize SampleFormat array (ndem) to default value per TIFF6.0 specs;
C     SampleFormat = 1 for unsigned integer
      SampleFormat = 1
      
C --- initialize ModelType codes to 0 (undefined)
      ModelType    = 0
      geoType      = 0
      geoDatum     = 0
      geoEllipse   = 0
      projCStype   = 0
      geogUnts     = 0
      projUnts     = 0
      
C --- initialize tiePtAncx,y,z 
      tiePtAncx = 0.0D0      
      tiePtAncy = 0.0D0      
      tiePtAncz = 0.0D0      

C --- load tag flags
      tCnt = tCnt+1
      tFlgs(tCnt) = tagFlgs(256,'ImageWidth',.false.,0)
      
      tCnt = tCnt+1
      tFlgs(tCnt) = tagFlgs(257,'ImageLength',.false.,0)

      tCnt = tCnt+1
      tFlgs(tCnt) = tagFlgs(258,'BitsPerSample',.false.,0)      
      
      tCnt = tCnt+1
      tFlgs(tCnt) = tagFlgs(259,'Compression',.false.,0)
      
      tCnt = tCnt+1
      tFlgs(tCnt) = tagFlgs(273,'StripOffsets',.false.,0)
      
      tCnt = tCnt+1
      tFlgs(tCnt) = tagFlgs(274,'Orientation',.false.,0)
      
      tCnt = tCnt+1
      tFlgs(tCnt) = tagFlgs(277,'SamplesPerPixel',.false.,0)
      
      tCnt = tCnt+1
      tFlgs(tCnt) = tagFlgs(278,'RowsPerStrip',.false.,0)
      
      tCnt = tCnt+1
      tFlgs(tCnt) = tagFlgs(322,'TileWidth',.false.,0)
      
      tCnt = tCnt+1
      tFlgs(tCnt) = tagFlgs(323,'TileLength',.false.,0)
      
      tCnt = tCnt+1
      tFlgs(tCnt) = tagFlgs(324,'TileOffsets',.false.,0)
      
      tCnt = tCnt+1
      tFlgs(tCnt) = tagFlgs(339,'SampleFormat',.false.,0)
      
      tCnt = tCnt+1
      tFlgs(tCnt) = tagFlgs(33550,'ModelPixelScale',.false.,0)
      
      tCnt = tCnt+1
      tFlgs(tCnt) = tagFlgs(33922,'ModelTiePoint',.false.,0)
      
C --- load geokey flags    
      gCnt = gCnt+1
      gFlgs(gCnt) = tagFlgs(1024,'GTModelType',.false.,0)
        
      gCnt = gCnt+1
      gFlgs(gCnt) = tagFlgs(1025,'GTRasterType',.false.,0)
        
      gCnt = gCnt+1
      gFlgs(gCnt) = tagFlgs(2048,'GeographicType',.false.,0)
        
      gCnt = gCnt+1
      gFlgs(gCnt) = tagFlgs(2050,'GeogGeodeticDatum',.false.,0)
        
      gCnt = gCnt+1
      gFlgs(gCnt) = tagFlgs(2054,'GeogAngularUnits',.false.,0)
        
      gCnt = gCnt+1
      gFlgs(gCnt) = tagFlgs(2056,'GeogEllipsoid',.false.,0)
        
      gCnt = gCnt+1
      gFlgs(gCnt) = tagFlgs(3072,'ProjectedCSType',.false.,0)

      gCnt = gCnt+1
      gFlgs(gCnt) = tagFlgs(3076,'ProjLinearUnits',.false.,0)

      gCnt = gCnt+1
      gFlgs(gCnt) = tagFlgs(4099,'VerticalUnits',.false.,0)

C --- Open MAPDETAIL.OUT debug file
      open(unit=demk, file = mapdet_file, status = 'replace') 
C*    Write Version Date Header to MAPDETAIL.OUT File
      WRITE(demk,9011) VERSN, RUNDAT, RUNTIM
9011  FORMAT('** AERMAP - VERSION ',A5,T72,A8,/
     &       '**',T72,A8/)

      WRITE(DEMK,*) 'From NEDCHK:'      

C --- Loop over NED files and process tiff tags and geokeys
      NEDLoop: do idem = 1, numdem
      
C ---    initialize logical flags
         geoTypeFlg      = .false.
         geoDatumFlg     = .false.
         geoEllipseFlg   = .false.
         projCStypeFlg   = .false.
         RasterTypeFlg   = .false.
         PixelIsArea     = .false.
         PixelIsPoint    = .false.
         tiePntFlg       = .false.
         pxlSclFlg       = .false.
         stripOSFlg      = .false.
         rowsPerStripFlg = .false.
         tileOSFlg       = .false.
         tileLenFlg      = .false.
         tileWidFlg      = .false.
         elvUntsFlg      = .false.
         geogUntsFlg     = .false.
         projUntsFlg     = .false.
         
C ---    Initialize gotTag and tNdx fields for tFlgs and gFlgs arrays
         ! loop over local tiff flags array
         do k=1, size(tFlgs)
            ! set gotTag flag to false
            tFlgs(k)%gotTag = .false.
            ! set array index to 0 for id in tag array
            tFlgs(k)%tNdx = 0
         end do
         ! loop over local geokey flags array
         do k=1, size(gFlgs)
            ! set gotTag flag to false
            gFlgs(k)%gotTag = .false.
            ! set index to 0 in tag array
            gFlgs(k)%tNdx = 0
         end do
      
C ---    Generate truncated filename, without path (up to 40 characters)
         I = INDEX(DEMFIL(IDEM),'/',BACK=.TRUE.)
         J = INDEX(DEMFIL(IDEM),ACHAR(92),BACK=.TRUE.)   ! ACHAR(92) = backslash
         IF (I .GT. 0) THEN
            IF (LEN_TRIM(DEMFIL(IDEM))-I .LE. 40) THEN
               FLN(IDEM) = DEMFIL(IDEM)(I+1:LEN_TRIM(DEMFIL(IDEM)))
            ELSE
               FLN(IDEM) = DEMFIL(IDEM)(LEN_TRIM(DEMFIL(IDEM))-39:
     &                                  LEN_TRIM(DEMFIL(IDEM)))
            END IF
         ELSE IF (J .GT. 0) THEN
            FLN(IDEM) = DEMFIL(IDEM)(J+1:LEN_TRIM(DEMFIL(IDEM)))
            IF (LEN_TRIM(DEMFIL(IDEM))-J .LE. 40) THEN
               FLN(IDEM) = DEMFIL(IDEM)(J+1:LEN_TRIM(DEMFIL(IDEM)))
            ELSE
               FLN(IDEM) = DEMFIL(IDEM)(LEN_TRIM(DEMFIL(IDEM))-39:
     &                                  LEN_TRIM(DEMFIL(IDEM)))
            END IF
         ELSE
            I = MIN(40, LEN_TRIM(DEMFIL(IDEM)))
            FLN(IDEM) = DEMFIL(IDEM)(1:I)
         END IF

         ! set map name - generic
         MAPN(idem) = 'NED:'//FLN(idem)(1:36)
                  
C ---    print message to map detail file for file to be read
         write(DEMK,55) idem, DEMFIL(idem)
         write(*,55) idem, DEMFIL(idem)
   55    format(/'***********************'/
     &           ' OPENING FILE: '//,
     &           '   NED File #:    ', I6,/,
     &           '   NED File Name: ', A,/)

C ---    read tiff tags and geokeys
         call read_tifftags(DEMFIL(idem),IDMUNT(idem),MAPDET_FILE,demk,
     &            L_TiffDebug(IDEM),TiffDbgFil(IDEM),ITiffDbg_Unt(IDEM))

C ---    process error and warning flags returned from read_tifftags;
C        messages should have been written to debug file, no need to duplicate

         ! warning         
         if (tiffwrn) then
            WRITE(DUMMY,'(I8)') IDEM
            CALL ERRHDL(PATH,MODNAM,'W','480',DUMMY)
         end if
         
         ! read error (file could not be read as a tiff file)
         if (tifferr) then
            WRITE(DUMMY,'(I8)') IDEM
            CALL ERRHDL(PATH,MODNAM,'E','481',DUMMY)
         end if
         
         ! allocation error (allocatable arrays)
         if (allerr) then
            WRITE(DUMMY,'(I8)') IDEM
            CALL ERRHDL(PATH,MODNAM,'E','482',DUMMY)
         end if
         
         ! if read error or allocation error encountered, 
         ! set fatal flag and exit subroutine
         if (tifferr .or. allerr) then         
            nederr = .TRUE.
            exit NEDLoop
         end if
         
C ---    Assign "DEMLVL" code for NED files to 5 = ' NED '
         DEMLVL(idem) = 5         

C ---    set global byteswap flag returned from Read_TiffTags subroutine
         byteswap(idem) = swapbytes

C ---    Loop over tiff tags array and local tiff flags array
C        Set flags and index refs

         ! loop tiff tags array
         do j=1, size(tTags)    

            ! loop over local tiff flags array
            do k=1, size(tFlgs)
            
               ! process tag if in the tFlgs array, and no problems
               if (tTags(j)%id == tFlgs(k)%id .and.
     &             .not.tTags(j)%L_TiffProb) then

                  ! set gotTag flag to true
                  tFlgs(k)%gotTag = .true.
                  
                  ! set array index for id in tag array
                  tFlgs(k)%tNdx = j
                  
               end if
               
            end do
            
         end do
         
C ---    Loop over geokey array and local geokey flags array
C        Set flags and index refs

         ! loop geokey array
         do j=1, size(gKeys)          

            ! loop over local geokey flags array
            do k=1, size(gFlgs)
            
               ! process tag if in the gFlgs array, and no problems
               if (gKeys(j)%id == gFlgs(k)%id .and.
     &             .not.gKeys(j)%L_TiffProb) then
               
                  ! set gotTag flag to true
                  gFlgs(k)%gotTag = .true.
                  
                  ! set index in tag array
                  gFlgs(k)%tNdx = j
                  
               end if
               
            end do
            
         end do
         
C ---    process tiff tags found to collect/evaluate values

         ! loop over local geokey flags array
         TagLoop: do k=1, size(tFlgs)

            ! set dumtag variable to hold tag id for error trapping
            WRITE (dumtag,'(I8)') tFlgs(k)%id
            
            select case(tFlgs(k)%gotTag)
            case(.false.)
            ! if gotTag is false, store error
            
               ! do not record error if tag id is related to strip or tile 
               ! offsets or elevation units, these are handled separately
               select case(tFlgs(k)%id)
               
               case(273,278,322,323,324)
               ! do not record error
               
               case(259)
               ! compression - default value is 1 (uncompressed)
                  
                  CALL ERRHDL(PATH,MODNAM,'W','450',dumtag)
                  write(*,8005) tFlgs(k)%id, tFlgs(k)%name, idem
                  write(demk,8005) tFlgs(k)%id, tFlgs(k)%name, idem
                  write(*,80051)
                  write(demk,80051)
 8005  format(
     &  /,' WARNING: TIFF Tag ',i5,': ',a,' was not found - default ',
     &                                                     'used for',
     &  /,'          NED file: ',i6,'.')
80051  format(
     &    '          Compression = 1; uncompressed'/)
                  
               case(274)
               ! orientation - default value is 1, 
               !               (0,0) represents upper left
               !               (NW) corner of the data
                  
                  CALL ERRHDL(PATH,MODNAM,'W','450',dumtag)
                  write(*,8005) tFlgs(k)%id, tFlgs(k)%name, idem
                  write(demk,8005) tFlgs(k)%id, tFlgs(k)%name, idem
                  write(*,80052)
                  write(demk,80052)
80052  format(
     &    '          Orientation = 1; row-major with (0,0) = ',
     &                                                'NW corner'/)
                  
               case(277)
               ! sample per pixel - default value is 1, 
               !                    which is expected by AERMAP
                  
                  CALL ERRHDL(PATH,MODNAM,'W','450',dumtag)
                  write(*,8005) tFlgs(k)%id, tFlgs(k)%name, idem
                  write(demk,8005) tFlgs(k)%id, tFlgs(k)%name, idem
                  write(*,80053)
                  write(demk,80053)
80053  format(
     &    '          SamplesPerPixel = 1'/)
                  
               case(339)
               ! sample format - default value is 1 (unsigned int)
                  SampleFormat(idem) = 1
                  
                  CALL ERRHDL(PATH,MODNAM,'W','450',dumtag)
                  write(*,8005) tFlgs(k)%id, tFlgs(k)%name, idem
                  write(demk,8005) tFlgs(k)%id, tFlgs(k)%name, idem
                  write(*,80054)
                  write(demk,80054)
80054  format(
     &    '          SampleFormat = 1; unsigned integer'/)
                  
               ! all other tiff tags
               case default
               ! write error message for required tiff tags that are missing    
               ! the required tags are 256,257,258,33550, and 33920
                  CALL ERRHDL(PATH,MODNAM,'E','450',dumtag)
                  nederr = .TRUE.
                  write(*,8015) tFlgs(k)%id, tFlgs(k)%name, idem
                  write(demk,8015) tFlgs(k)%id, tFlgs(k)%name, idem
               
               end select
 8015 format(
     & /'   ERROR: TIFF Tag ',i5,': ',a,' was not found for NED',
     &                                          ' file: ',i6,'.'/)
     
            case(.true.)
                  
               select case(tFlgs(k)%id)

               case(256)
               ! image width - cols
                  ! set number of columns
                  nCols(idem) = tTags(tFlgs(k)%tNdx)%intVal(1)  
                  write(demk,'(a,i8)') 
     &            ' Columns (profiles):   ', nCols(idem)
                   
                  ! Number of profiles in NED file: Because a NED file is 
                  ! rectangular, the number of profiles is equal to the 
                  ! number of columns, and the number of nodes in a profile
                  ! is equal to the number of rows.
                  nprof(idem) = nCols(idem)   
                           
               case(257)
               ! image length - rows
                  nRows(idem) = tTags(tFlgs(k)%tNdx)%intVal(1)
                  write(demk,'(a,i8)') 
     &            ' Rows (nodes/profile): ', nRows(idem)
                           
               case(258)
               ! bits per sample - should be 8, 16, 32, or 64
               ! compute bytes per sample - should be 1, 2, 4, or 8
                  if (tTags(tFlgs(k)%tNdx)%intVal(1) == 8 .or.
     &                tTags(tFlgs(k)%tNdx)%intVal(1) == 16 .or.
     &                tTags(tFlgs(k)%tNdx)%intVal(1) == 32 .or.
     &                tTags(tFlgs(k)%tNdx)%intVal(1) == 64) then
                     bytesPerSample(idem) = 
     &                tTags(tFlgs(k)%tNdx)%intVal(1)/8
     
                     if (tTags(tFlgs(k)%tNdx)%intVal(1) == 8) then
                     ! issue warning for 1 byte per sample; unsigned
                     ! integer limited to values less than 256
                        CALL ERRHDL(PATH,MODNAM,'W','455',dumtag)
                        write(*,8019) tFlgs(k)%id, tFlgs(k)%name,  
     &                     tTags(tFlgs(k)%tNdx)%intVal(1), idem
                        write(demk,8019) tFlgs(k)%id, tFlgs(k)%name, 
     &                     tTags(tFlgs(k)%tNdx)%intVal(1), idem

 8019 format(
     & /' WARNING: TIFF Tag ',i5,': ',a,' had an unexpected value (',
     &                                                       i10,')',
     & /'          for NED file: ',i6,';',
     & /'          Bytes per sample = 1; limited data range'/)
     
                     end if
                     
                  else
                     CALL ERRHDL(PATH,MODNAM,'E','455',dumtag)
                     nederr = .TRUE.
                     write(*,8020) tFlgs(k)%id, tFlgs(k)%name,  
     &                  tTags(tFlgs(k)%tNdx)%intVal(1), idem
                     write(demk,8020) tFlgs(k)%id, tFlgs(k)%name, 
     &                  tTags(tFlgs(k)%tNdx)%intVal(1), idem
     
                  end if

 8020 format(
     & /'   ERROR: TIFF Tag ',i5,': ',a,' had an unsupported value (',
     &                                                       i10,')',
     & /'          for NED file: ',i6,'.'/)
     
               case(259)
               ! compression - (must be 1, uncompressed)
                  if (tTags(tFlgs(k)%tNdx)%intVal(1) /= 1) then
                     CALL ERRHDL(PATH,MODNAM,'E','455',dumtag)
                     nederr = .TRUE.
                     write(*,8020) tFlgs(k)%id, tFlgs(k)%name,
     &                  tTags(tFlgs(k)%tNdx)%intVal(1), idem
                     write(demk,8020) tFlgs(k)%id, tFlgs(k)%name, 
     &                  tTags(tFlgs(k)%tNdx)%intVal(1), idem
                  end if
            
               case(273)
               ! strip offsets
                  stripOSFlg = .true.
                  allocate(dataOS(idem)%numbytes(size
     &               (tTags(tFlgs(k)%tNdx)%intVal)))
                  dataOS(idem)%numbytes = tTags(tFlgs(k)%tNdx)%intVal
               
               case(274)
               ! orientation (must be 1 for row major starting from 
               !              upper left corner of file)
                  if (tTags(tFlgs(k)%tNdx)%intVal(1) /= 1) then
                     CALL ERRHDL(PATH,MODNAM,'E','455',dumtag)
                     nederr = .TRUE.
                     write(*,8020) tFlgs(k)%id, tFlgs(k)%name,
     &                  tTags(tFlgs(k)%tNdx)%intVal(1), idem
                     write(demk,8020) tFlgs(k)%id, tFlgs(k)%name, 
     &                  tTags(tFlgs(k)%tNdx)%intVal(1), idem
                  end if
         
               case(277)
               ! samples per pixel (must be 1)
                  if (tTags(tFlgs(k)%tNdx)%intVal(1) /= 1) then
                     CALL ERRHDL(PATH,MODNAM,'E','455',dumtag)
                     nederr = .TRUE.
                     write(*,8020) tFlgs(k)%id, tFlgs(k)%name,
     &                  tTags(tFlgs(k)%tNdx)%intVal(1), idem
                     write(demk,8020) tFlgs(k)%id, tFlgs(k)%name, 
     &                  tTags(tFlgs(k)%tNdx)%intVal(1), idem
                  end if
         
               case(278)
               ! rows per strip
                  rowsPerStripFlg = .true.
                  rowsPerStrip(idem) = tTags(tFlgs(k)%tNdx)%intVal(1)
         
               case(322)
               ! tile width
                  tileWidFlg = .true.
                  tileWid(idem) = tTags(tFlgs(k)%tNdx)%intVal(1)
         
               case(323)
               ! tile length
                  tileLenFlg = .true.
                  tileLen(idem) = tTags(tFlgs(k)%tNdx)%intVal(1)
         
               case(324)
               ! tile offsets
                  tileOSFlg = .true.
                  allocate(dataOS(idem)%numbytes(size
     &               (tTags(tFlgs(k)%tNdx)%intVal)))
                  dataOS(idem)%numbytes = tTags(tFlgs(k)%tNdx)%intVal
            
               case(339)
               ! sample format - should be 1 (unsigned int), 2 (signed int),
               !                 or 3 (floating point), 
                  if (tTags(tFlgs(k)%tNdx)%intVal(1) /= 1 .and.
     &                tTags(tFlgs(k)%tNdx)%intVal(1) /= 2 .and.
     &                tTags(tFlgs(k)%tNdx)%intVal(1) /= 3) then
                     CALL ERRHDL(PATH,MODNAM,'E','455',dumtag)
                     nederr = .TRUE.
                     write(*,8020) tFlgs(k)%id, tFlgs(k)%name,
     &                  tTags(tFlgs(k)%tNdx)%intVal(1), idem
                     write(demk,8020) tFlgs(k)%id, tFlgs(k)%name, 
     &                  tTags(tFlgs(k)%tNdx)%intVal(1), idem
                  else
                     SampleFormat(idem) = tTags(tFlgs(k)%tNdx)%intVal(1)
                  end if 
            
               case(33550)
               ! model pixel scale

                  ! set pixel scale for file; x, y, and z
                  pxlScalex(idem) = tTags(tFlgs(k)%tNdx)%dblVal(1)
                  pxlScaley(idem) = tTags(tFlgs(k)%tNdx)%dblVal(2)
                  pxlScalez(idem) = tTags(tFlgs(k)%tNdx)%dblVal(3)
                  
                  pxlSclFlg = .true.
                  
                  ! check for non-zero vertical scale                  
                  if (pxlScalez(idem) /= 0.0D0) then
                  !  non-zero vertical scale; assign to DCI array
                     DCI(idem) = pxlScalez(idem)
                  else
                  !  set vertical scale to 1.0D0
                     DCI(idem) = 1.0D0
                  end if
                                          
               case(33922)
               ! model tie point
               
                  ! set x, y, z anchor point for tie point
                  tiePtAncx = tTags(tFlgs(k)%tNdx)%dblVal(1)
                  tiePtAncy = tTags(tFlgs(k)%tNdx)%dblVal(2)
                  tiePtAncz = tTags(tFlgs(k)%tNdx)%dblVal(3)
                  ! set x, y, z tie point
                  tiePtx(idem) = tTags(tFlgs(k)%tNdx)%dblVal(4)
                  tiePty(idem) = tTags(tFlgs(k)%tNdx)%dblVal(5)
                  tiePtz(idem) = tTags(tFlgs(k)%tNdx)%dblVal(6)
                  
                  tiePntFlg = .true.
                  
               end select ! tFlgs%id
            
            end select    ! tFlgs%gotTag

         end do TagLoop   ! end loop over tFlgs (k)

C ---    Special tag depencency processing
c        Determine tiff type (strip or tile)

         if (stripOSFlg .and. rowsPerStripFlg) then
            tiffType(idem) = 'strip'
            write(demk,'(a)')    ' TIFF Data Organization: Strips'
            write(demk,'(a,i6)') '           RowsPerStrip: ',
     &                                           rowsPerStrip(idem)
         elseif (stripOSFlg) then
C ---       RowPerStrip not defined; default value is number of rows
            tiffType(idem) = 'strip'         
            write(demk,'(a)') ' TIFF Data Organization: Strips'
            rowsPerStrip(idem) = nRows(idem)
            CALL ERRHDL(PATH,MODNAM,'W','450','     278')
            write(*,8005) tFlgs(k)%id, tFlgs(k)%name, idem
            write(demk,8005) tFlgs(k)%id, tFlgs(k)%name, idem
            write(*,80055)
            write(demk,80055)
80055  format(
     &    '          RowsPerStrip = file length (nRows)'/)
                  
         elseif (tileOSFlg .and. tileLenFlg .and. tileWidFlg) then
            tiffType(idem) = 'tile'
            write(demk,'(a)')    ' TIFF Data Organization:  Tiles'
            write(demk,'(a,i6)') '            Tile Length: ',
     &                                                     tileLen(idem)
            write(demk,'(a,i6)') '             Tile Width: ',
     &                                                     tileWid(idem)
         else
            CALL ERRHDL(PATH,MODNAM,'E','475',tifftype(idem))
            nederr = .TRUE.
            write(*,8025) idem
            write(demk,8025) idem
            
 8025 format(
     & /,'   ERROR: GeoTIFF data organization scheme (strips or tiles)',
     & /,'          could not be determined for NED file: ',i6,'.',/) 
                    
         end if

C ---    Write sample format and bytes per sample to MAPDETAIL.OUT file
         if (SampleFormat(idem) == 1) then
            write(demk,'(a,a)')  ' TIFF Data SampleFormat: ',
     &                           'unsigned integer'
         else if (SampleFormat(idem) == 2) then
            write(demk,'(a,a)')  ' TIFF Data SampleFormat: ',
     &                           'signed integer'
         else if (SampleFormat(idem) == 3) then
            write(demk,'(a,a)')  ' TIFF Data SampleFormat: ',
     &                           'floating point'
         end if
         write(demk,'(a,i6)')    '         BytesPerSample: ',
     &                                     bytespersample(idem)

C ---    process GeoKeys found to collect/evaluate values

         ! loop over local geokey flags array
         GeoKeyLoop: do k=1, size(gFlgs)
         
            ! set dumflg variable to hold flag id for error trapping
            WRITE (dumflg,'(I8)') gFlgs(k)%id
             
            select case(gFlgs(k)%gotTag)

            case(.false.)   
            ! if gotTag is false, check for required geokeys that are absent
                 
               select case(gFlgs(k)%id)
               case(2048,2050,2054,2056,3072,3076,4099)
               ! do not record error if specified geo keys are not present,
               ! minimum requirements will be handled separately
               
               ! all other geokeys
               case default
               ! write error message for required geokeys that are missing;
               ! currently GT Model Type (1024) and GT Raster Type (1025) are the 
               ! only required geokeys.
               ! Note: GeoTIFF specs indicate that PixelIsArea is the default
               ! GTRasterType; however, PixelIsPoint is a more appropriate value
               ! for elevation data. Therefore, we require GTRasterType to be 
               ! defined for elevation data in AERMAP.
               ! Other minimum requirements are conditional and determined below
                  
                  CALL ERRHDL(PATH,MODNAM,'E','460',dumflg)
                  nederr = .TRUE.
                  write(*,8032) gFlgs(k)%id, gFlgs(k)%name, idem
                  write(demk,8032) gFlgs(k)%id, gFlgs(k)%name, idem
                         
 8032  format(
     &  /,'   ERROR: GeoKey ',i5,': ',a,' was not found for NED file: ',
     &                                                          i6,'.'/)
               
               end select

     
            case(.true.)
            ! GeoKey is found; processing codes
            
               select case(gFlgs(k)%id)

               case(1024)
               ! GT Model Type; 
               ! (1=Projection, 2=Geographic, 3=Geocentric)
                  select case(gKeys(gFlgs(k)%tNdx)%intVal(1))
                  case(1)
                  
C ---                Assign ModelType code
                     ModelType = gKeys(gFlgs(k)%tNdx)%intVal(1)
                    
C                    Assign TYPDEM based on first data file                  
                     if (idem == 1) TYPDEM = 'UTM'
                     
C                    Assign IPLAN code for ground planimetric reference system
                     IPLAN(idem) = 1        ! UTM
                  
                     write(demk,*) 
     &            'Coordinate Type:  Projection Coordinate System (UTM)'
     
                  case(2)
                  
C ---                Assign ModelType code
                     ModelType = gKeys(gFlgs(k)%tNdx)%intVal(1)
                    
C                    Assign TYPDEM based on first data file                  
                     if (idem == 1) TYPDEM = 'LAT'
                     
C                    Assign IPLAN code for ground planimetric reference system
                     IPLAN(idem) = 0        ! geographic
                  
                     write(demk,*) 
     &                 'Coordinate Type:  Geographic Lat-Lon System'
     
                  case(3)
                  ! Geocentric is not an accepted ModelType for AERMAP
                  ! issue error message
                  
C                    Assign TYPDEM based on first data file
                     if (idem == 1) TYPDEM = 'GEO'
                     
C                    Assign IPLAN code = 9 to indicate missing value
                     IPLAN(idem) = 9
                  
                     CALL ERRHDL(PATH,MODNAM,'E','465',dumflg)
                     nederr = .TRUE.
                     write(*,8035) gFlgs(k)%id, gFlgs(k)%name,
     &                  gKeys(gFlgs(k)%tNdx)%intVal(1), idem
                     write(demk,8035) gFlgs(k)%id, gFlgs(k)%name,
     &                  gKeys(gFlgs(k)%tNdx)%intVal(1), idem

 8035  format(
     &  /,'   ERROR: GeoKey ',i5,': ',a,' had an unsupported value (',
     &                                                        i8,');',
     &  /,'          for NED file: ',i6,'.'/)
     
                  case default
                  ! Model Type code is not recognized, issue error message
                     
                     if (idem == 1) TYPDEM = 'UNK'
                     IPLAN(idem) = 9
                  
                     CALL ERRHDL(PATH,MODNAM,'E','465',dumflg)
                     nederr = .TRUE.
                     write(*,8035) gFlgs(k)%id, gFlgs(k)%name,
     &                  gKeys(gFlgs(k)%tNdx)%intVal(1), idem
                     write(demk,8035) gFlgs(k)%id, gFlgs(k)%name,
     &                  gKeys(gFlgs(k)%tNdx)%intVal(1), idem
                  end select

            
               case(1025)
               ! GT Raster Type; check for 1 or 2
               ! (1=PixelIsArea, 2=PixelIsPoint)
                  if (gKeys(gFlgs(k)%tNdx)%intVal(1) == 1) then
                  !  1=PixelIsArea
                     PixelIsArea   = .TRUE.
                     PixelIsPoint  = .FALSE.
                     RasterTypeFlg = .TRUE.
                  else if (gKeys(gFlgs(k)%tNdx)%intVal(1) == 2) then
                  !  2=PixelIsPoint
                     PixelIsPoint  = .TRUE.
                     PixelIsArea   = .FALSE.
                     RasterTypeFlg = .TRUE.
                  else
                  !  GT raster type not defined; 
                  !  Issue error message
                     PixelIsArea   = .FALSE.
                     PixelIsPoint  = .FALSE.
                     RasterTypeFlg = .FALSE.
                     
                     CALL ERRHDL(PATH,MODNAM,'E','465',dumflg)
                     nederr = .TRUE.
                     write(*,8035) gFlgs(k)%id, gFlgs(k)%name,
     &                  gKeys(gFlgs(k)%tNdx)%intVal(1), idem
                     write(demk,8035) gFlgs(k)%id, gFlgs(k)%name,
     &                  gKeys(gFlgs(k)%tNdx)%intVal(1), idem
                  end if

               case(2048)
               ! geographic type/ellipsoid:
               !  accepted codes for AERMAP:
               !   datum: 4267 = NAD27, 4269 = NAD83, 4322 = WGS72, 4326 = WGS84;
               !   ellipsoid: 4008 = Clarke 1866, 4019 = GRS80, 4030 = WGS84
               
                  select case(gKeys(gFlgs(k)%tNdx)%intVal(1))
                  case(4267,4269,4322,4326)
                     geoTypeFlg = .true.
                     geoType = gKeys(gFlgs(k)%tNdx)%intVal(1)
                  case(4008,4019,4030)
                     geoEllipseFlg = .true.
                     geoEllipse = gKeys(gFlgs(k)%tNdx)%intVal(1)
                  case default
                     ! specified datum/ellipsoid not supported; 
                     ! issue warning but continue processing for now;
                     ! will be addressed later when all geokeys are processed
                     geoTypeFlg = .true.
                     geoType = gKeys(gFlgs(k)%tNdx)%intVal(1)
                     
                     CALL ERRHDL(PATH,MODNAM,'W','465',dumflg)
                     write(*,8036) gFlgs(k)%id, gFlgs(k)%name,
     &                  gKeys(gFlgs(k)%tNdx)%intVal(1), idem
                     write(demk,8036) gFlgs(k)%id, gFlgs(k)%name,
     &                  gKeys(gFlgs(k)%tNdx)%intVal(1), idem
                  end select    

8036   format(
     &  /,' WARNING: GeoKey ',i5,': ',a,' had an unexpected value (',
     &                                                       i8,');',
     &  /,'          for NED file: ',i6,'.'/)
            
            
               case(2050)
               ! geographic datum/ellipsoid
               !  accepted codes for AERMAP:
               !   datum: 6267 = NAD27, 6269 = NAD83, 6322 = WGS72 6326 = WGS84;
               !   ellipsoid: 6008 = Clarke 1866, 6019 = GRS80, 6030 = WGS84
               
                  select case(gKeys(gFlgs(k)%tNdx)%intVal(1))
                  case(6267,6269,6322,6326)
                     geoDatumFlg = .true.
                     geoDatum = gKeys(gFlgs(k)%tNdx)%intVal(1)
                  case(6008,6019,6030)
                     geoEllipseFlg = .true.
                     geoEllipse = gKeys(gFlgs(k)%tNdx)%intVal(1)
                  case default
                     ! specified datum/ellipsoid not supported; 
                     ! issue warning but continue processing for now;
                     ! will be addressed later when all geokeys are processed
                     geoDatumFlg = .true.
                     geoDatum = gKeys(gFlgs(k)%tNdx)%intVal(1)
                     
                     CALL ERRHDL(PATH,MODNAM,'W','465',dumflg)
                     write(*,8036) gFlgs(k)%id, gFlgs(k)%name,
     &                  gKeys(gFlgs(k)%tNdx)%intVal(1), idem
                     write(demk,8036) gFlgs(k)%id, gFlgs(k)%name,
     &                  gKeys(gFlgs(k)%tNdx)%intVal(1), idem
                  end select       
            
               case(2056)
               ! geographic ellipsoid:
               !  accepted codes for AERMAP:
               !   7008 = Clarke 1866, 7019 = GRS80, 7030 = WGS84
               
                  select case(gKeys(gFlgs(k)%tNdx)%intVal(1))
                  case(7008,7019,7030)
                     geoEllipseFlg = .true.
                     geoEllipse = gKeys(gFlgs(k)%tNdx)%intVal(1)
                  case default
                  ! specified ellipsoid not supported; 
                  ! issue warning but continue processing for now;
                  ! will be addressed later when all geokeys are processed
                     geoEllipseFlg = .true.
                     geoEllipse = gKeys(gFlgs(k)%tNdx)%intVal(1)
                     
                     CALL ERRHDL(PATH,MODNAM,'W','465',dumflg)
                     write(*,8036) gFlgs(k)%id, gFlgs(k)%name,
     &                  gKeys(gFlgs(k)%tNdx)%intVal(1), idem
                     write(demk,8036) gFlgs(k)%id, gFlgs(k)%name,
     &                  gKeys(gFlgs(k)%tNdx)%intVal(1), idem
                  end select       
            
               case(2054)
               ! geographic angular units; three codes are
               ! supported by AERMAP: 9201 = Radians, 9102 = Degrees, 
               !                      9104 = Arc-seconds
               ! CUNIT values will be assigned later after all
               ! codes have been processed, except for invalid
               ! codes which are flagged by CUNIT = 9
                  select case(gKeys(gFlgs(k)%tNdx)%intVal(1))
                  case(9101)                ! radians
                     geogUntsFlg = .true.
                     geogUnts    = gKeys(gFlgs(k)%tNdx)%intVal(1)
                  case(9102)               ! degrees
                  !  Note: degrees are not included in DEM Record A options
                     geogUntsFlg = .true.
                     geogUnts    = gKeys(gFlgs(k)%tNdx)%intVal(1)
                  case(9104)               ! arc-seconds
                     geogUntsFlg = .true.
                     geogUnts    = gKeys(gFlgs(k)%tNdx)%intVal(1)
                  case default
                  ! specified angular units code not supported; 
                  ! issue warning message
                     geogUntsFlg = .false.
                     CUNIT(idem) = 9       ! missing
                     
                     CALL ERRHDL(PATH,MODNAM,'W','465',dumflg)
                     write(*,8036) gFlgs(k)%id, gFlgs(k)%name,
     &                  gKeys(gFlgs(k)%tNdx)%intVal(1), idem
                     write(demk,8036) gFlgs(k)%id, gFlgs(k)%name,
     &                  gKeys(gFlgs(k)%tNdx)%intVal(1), idem
                  end select
            
               case(3072)
               ! Projected CS Type; geokey values include datum and UTM zone
               ! Accepted codes for AERMAP:
               !  267nn = NAD27, 268nn = NAD83, 322nn = WGS72N, 323nn = WGS72S, 
               !  326nn = WGS84N, 327nn = WGS84S, where nn = UTM zone
                  
                  select case(gKeys(gFlgs(k)%tNdx)%intVal(1))
                  case(26703:26722)                           ! NAD27
                     projCStypeFlg = .true.
                     projCStype = gKeys(gFlgs(k)%tNdx)%intVal(1)
                     
                  case(26903:26923)                           ! NAD83
                     projCStypeFlg = .true.
                     projCStype = gKeys(gFlgs(k)%tNdx)%intVal(1)
                     
                  case(32201:32260)                           ! WGS72 North
                     projCStypeFlg = .true.
                     projCStype = gKeys(gFlgs(k)%tNdx)%intVal(1)
                     
                  case(32301:32360)                           ! WGS72 South
                     projCStypeFlg = .true.
                     projCStype = gKeys(gFlgs(k)%tNdx)%intVal(1)
                     
                  case(32601:32660)                           ! WGS84 North
                     projCStypeFlg = .true.
                     projCStype = gKeys(gFlgs(k)%tNdx)%intVal(1)
                     
                  case(32701:32760)                           ! WGS84 South
                     projCStypeFlg = .true.
                     projCStype = gKeys(gFlgs(k)%tNdx)%intVal(1)
                     
                  case default
                  ! specified projection type not supported; 
                  ! issue warning but continue processing for now;
                  ! will be addressed later when all geokeys are processed
                     projCStypeFlg = .true.
                     projCStype = gKeys(gFlgs(k)%tNdx)%intVal(1)
                    
                     CALL ERRHDL(PATH,MODNAM,'W','465',dumflg)
                     write(*,8036) gFlgs(k)%id, gFlgs(k)%name,
     &                  gKeys(gFlgs(k)%tNdx)%intVal(1), idem
                     write(demk,8036) gFlgs(k)%id, gFlgs(k)%name,
     &                  gKeys(gFlgs(k)%tNdx)%intVal(1), idem
                  end select    
            
               case(3076)
               ! projected linear units; two codes are
               ! supported by AERMAP: 9001=meters, 9002=feet
               !
               ! CUNIT values will be assigned later after all
               ! codes have been processed, except for invalid
               ! codes which are flagged by CUNIT = 9
                  select case(gKeys(gFlgs(k)%tNdx)%intVal(1))
                  case(9001)               ! meters
                     projUntsFlg = .true.
                     projUnts    = gKeys(gFlgs(k)%tNdx)%intVal(1)
                  case(9002)               ! feet
                     projUntsFlg = .true.
                     projUnts    = gKeys(gFlgs(k)%tNdx)%intVal(1)
                  case default
                  ! specified vertical units code not supported; 
                  ! issue warning message
                     projUntsFlg = .false.
                     CUNIT(idem) = 9       ! missing
                     
                     CALL ERRHDL(PATH,MODNAM,'W','465',dumflg)
                     write(*,8036) gFlgs(k)%id, gFlgs(k)%name,
     &                  gKeys(gFlgs(k)%tNdx)%intVal(1), idem
                     write(demk,8036) gFlgs(k)%id, gFlgs(k)%name,
     &                  gKeys(gFlgs(k)%tNdx)%intVal(1), idem
                  end select
            
               case(4099)
               ! vertical units; two codes are supported by AERMAP:
               ! 9001 = meters, 9002 = feet
                  select case(gKeys(gFlgs(k)%tNdx)%intVal(1))
                  case(9001)
                  ! meters
                     elvUntsFlg = .true.
                     ! assign to ELUNIT array based on DEM Record A codes
                     ELUNIT(idem)  = 2
                     write(demk,'(a)') ' Elevation Units: Meters'
                  case(9002)
                  ! feet
                     elvUntsFlg = .true.
                     ! assign to ELUNIT array based on DEM Record A codes
                     ELUNIT(idem)  = 1
                     write(demk,'(a)') ' Elevation Units: Feet'
                  case default
                     elvUntsFlg = .false.
                     ! assign ELUNIT = 9 to indicate invalid code
                     ELUNIT(idem)  = 9 
                     
                     CALL ERRHDL(PATH,MODNAM,'E','465',dumflg)
                     nederr = .TRUE.
                     write(*,8010) gKeys(gFlgs(k)%tNdx)%intVal(1)
                     write(demk,8010) gKeys(gFlgs(k)%tNdx)%intVal(1)
                  end select
 8010  format(
     &  /,'   ERROR: Elevation units code is not supported: ',i6,/) 
            
               end select
            
            end select
               
         end do GeoKeyLoop     ! end loop over gFlgs (k)         
 
C ------ Begin processing tifTags and geoKeys to determine file type/datum, 
C        and units, and to check for completeness of file specifications. 
C        Requirements based on ModelType (1=Projection, 2=Geographic)
C ------
         select case(ModelType)
         
         case(1)      ! Projected File

C ---       This is a projection file; first check for proper horizontal units
            if (projUntsFlg) then
            ! first loop through gFlgs to find index for geoKey 3076
               do k=1, size(gFlgs)
                  ! process tag if in the gFlgs array, and no problems
                  if (gFlgs(k)%id == 3076) exit
               end do
               
               if (CUNIT(idem) /= 9) then
               ! assign CUNIT codes based on DEM Record A options
                  if (projUnts == 9001) then
                     CUNIT(idem) = 2               ! meters
                  else if (projUnts == 9002) then
                     CUNIT(idem) = 1               ! feet
                  end if
               else
               ! Invalid projected linear units for projected file,
               ! issue error message
                 ! set dumflg variable to hold flag id for error trapping
                  WRITE (dumflg,'(I8)') gFlgs(k)%id
                  CALL ERRHDL(PATH,MODNAM,'E','465',dumflg)
                  nederr = .TRUE.
                  write(*,8035) gFlgs(k)%id, gFlgs(k)%name,
     &               gKeys(gFlgs(k)%tNdx)%intVal(1), idem
                  write(demk,8035) gFlgs(k)%id, gFlgs(k)%name,
     &               gKeys(gFlgs(k)%tNdx)%intVal(1), idem
               end if
               
            else if (.not. projUntsFlg) then
            ! No projected horizontal units have been specified. 
            ! Issue warning message and assume METERS for projected file
               WRITE(dummy,'(I8)') idem
               CALL ERRHDL(PATH,MODNAM,'W','478',DUMMY)
              
              ! Assign CUNIT = 2 for meters
               cunit(idem) = 2
               
               write(*,8038) idem
               write(demk,8038) idem
 8038  format(/
     &  ' WARNING: Projected linear units not found in NED file ',i6,'.'
     & /'          Default units of METERS used for projected file.'/)
     
            end if
            
C ---       Process projCStype codes for datum and UTM zone
            if (projCStypeFlg) then
         
               select case(projCStype)
               case(26703:26722)                           ! NAD27
                  nadd(idem) = 1 
                  write(demk,8040) nadn(nadd(idem))
 8040             format(' Horizontal Datum: ',a)
                  DUMMY = 'NAD27'
                  CALL ERRHDL(PATH,MODNAM,'W','470',DUMMY)
                  write(*,8045) DUMMY, idem
                  write(demk,8045) DUMMY, idem
 8045             format(
     & /,' WARNING: Horizontal Datum ',a,' is unexpected',
     &                                   ' for NED file: ',i6,'.',
     & /,'          NED data are normally based on NAD83.',/)           
                  
                  ! assign UTM zone                     
                  izo(idem) = projCStype - 26700
                  IZOND(idem) = IZO(idem)
                  
               case(26903:26923)                           ! NAD83
                  nadd(idem) = 4
                  write(demk,8040) nadn(nadd(idem))
                  
                  ! assign UTM zone                     
                  izo(idem) = projCStype - 26900
                  IZOND(idem) = IZO(idem)
                  
               case(32201:32260)                           ! WGS72 North
                  nadd(idem) = 2
                  write(demk,8040) nadn(nadd(idem))
                  DUMMY = 'WGS72'
                  CALL ERRHDL(PATH,MODNAM,'W','470',DUMMY)
                  write(*,8045) DUMMY, idem
                  write(demk,8045) DUMMY, idem
                  
                  ! assign UTM zone                     
                  izo(idem) = projCStype - 32200
                  IZOND(idem) = IZO(idem)
                  
               case(32301:32360)                           ! WGS72 South
                  nadd(idem) = 2
                  write(demk,8040) nadn(nadd(idem))
                  DUMMY = 'WGS72'
                  CALL ERRHDL(PATH,MODNAM,'W','470',DUMMY)
                  write(*,8045) DUMMY, idem
                  write(demk,8045) DUMMY, idem
                  
                  ! assign UTM zone                     
                  izo(idem) = -1*(projCStype - 32300)
                  IZOND(idem) = IZO(idem)
                  
               case(32601:32660)                           ! WGS84 North
                  nadd(idem) = 3
                  write(demk,8040) nadn(nadd(idem))
                  DUMMY = 'WGS84'
                  CALL ERRHDL(PATH,MODNAM,'W','470',DUMMY)
                  write(*,8045) DUMMY, idem
                  write(demk,8045) DUMMY, idem
                  
                  ! assign UTM zone                     
                  izo(idem) = projCStype - 32600
                  IZOND(idem) = IZO(idem)
                  
               case(32701:32760)                           ! WGS84 South
                  nadd(idem) = 3
                  write(demk,8040) nadn(nadd(idem))
                  DUMMY = 'WGS84'
                  CALL ERRHDL(PATH,MODNAM,'W','470',DUMMY)
                  write(*,8045) DUMMY, idem
                  write(demk,8045) DUMMY, idem
                  
                  ! assign UTM zone                     
                  izo(idem) = -1*(projCStype - 32700)
                  IZOND(idem) = IZO(idem)
                  
               case default
               ! Unidentified projection type geoKey;
               ! processing depends on value of NADA
                  
                  if (NADA .EQ. 0) then
                  ! specified projection type not recognized, but user specified NADA=0
                  ! for no NAD conversion; assume NAD83 as the default datum;
                  ! issue warning but continue processing
                     ! print warning
                     write(dummy,'(i8)') idem
                     CALL ERRHDL(PATH,MODNAM,'W','471',DUMMY)
                     write(*,80371) projCStype
                     write(demk,80371) projCStype
80371  format(
     & /,' WARNING: Datum from GeoKey had an unexpected value: ',i6,
     & /,'          Assuming NAD83 datum since NADA=0 specified.'/)
     
                     nadd(idem) = 4
                     write(demk,8040) nadn(nadd(idem))
                     
                  else 
                  ! specified projection type not supported and user specified 
                  ! NADA/=0 for NAD conversion; issue error message.
                     
                     ! print error
                     write(dummy,'(i8)') idem
                     CALL ERRHDL(PATH,MODNAM,'E','472',DUMMY)
                     nederr = .TRUE.
                     write(*,80372) projCStype
                     write(demk,80372) projCStype
80372  format(
     & /,'   ERROR: Datum from GeoKey had an unsupported value: ',i6,
     & /,'          Set NADA=0 on ANCHORXY input to skip NAD ',
     &                                                  'conversion.'/)

                  end if
         
               end select 
               
            else
            ! Projection ModelType not adequately defined
            ! issue error message
            ! loop through gFlgs to find index for geoKey 3072
               do k=1, size(gFlgs)
                  if (gFlgs(k)%id == 3072) exit
               end do
               
               WRITE (dumflg,'(I8)') gFlgs(k)%id
               CALL ERRHDL(PATH,MODNAM,'E','460',dumflg)
               nederr = .TRUE.
               write(*,8032) gFlgs(k)%id, gFlgs(k)%name, idem
               write(demk,8032) gFlgs(k)%id, gFlgs(k)%name, idem
            end if 
         
         case(2)      ! Geographic File
            
C ---       This is a geographic file; first check for proper angular units
            if (geogUntsFlg) then
            ! first loop through gFlgs to find index for geoKey 2054
               do k=1, size(gFlgs)
                  ! process tag if in the gFlgs array, and no problems
                  if (gFlgs(k)%id == 2054) exit
               end do
               
               if (CUNIT(idem) /= 9) then
               ! assign CUNIT codes based on DEM Record A options
               ! (note that DEM Record A does not accept degrees as an option)
                  if (geogUnts == 9101) then
                     CUNIT(idem) = 0               ! radians
                  else if (geogUnts == 9102) then
                     CUNIT(idem) = 4               ! degrees
                  else if (geogUnts == 9104) then
                     CUNIT(idem) = 3               ! arc-seconds
                  end if
               else
               ! Invalid angular units for geographic file,
               ! issue error message
                 ! set dumflg variable to hold flag id for error trapping
                  WRITE (dumflg,'(I8)') gFlgs(k)%id
                  CALL ERRHDL(PATH,MODNAM,'E','465',dumflg)
                  nederr = .TRUE.
                  write(*,8035) gFlgs(k)%id, gFlgs(k)%name,
     &               gKeys(gFlgs(k)%tNdx)%intVal(1), idem
                  write(demk,8035) gFlgs(k)%id, gFlgs(k)%name,
     &               gKeys(gFlgs(k)%tNdx)%intVal(1), idem
               end if
               
            else if (.not. geogUntsFlg) then
            ! No geographic angular units have been specified. 
            ! Issue warning message and assume DEGREES for geographic file
               WRITE(dummy,'(I8)') idem
               CALL ERRHDL(PATH,MODNAM,'W','479',DUMMY)
              
              ! Assign CUNIT = 4 for angular degrees
               cunit(idem) = 4
               
               write(*,8039) idem
               write(demk,8039) idem
 8039   format(/
     &' WARNING: Geographic angular units not found in NED file ',i6,'.'
     &/'          Default units of DEGREES used for geographic file.'/)

            end if
            
            if (geoTypeFlg) then
            ! Geographic Type code has been specified
         
               select case(geoType)
               case(4267)
               ! NAD27
                  nadd(idem) = 1 
                  write(demk,8040) nadn(nadd(idem))
                  DUMMY = 'NAD27'
                  CALL ERRHDL(PATH,MODNAM,'W','470',DUMMY)
                  write(*,8045) DUMMY, idem
                  write(demk,8045) DUMMY, idem
         
               case(4269)
               ! NAD83
                  nadd(idem) = 4
                  write(demk,8040) nadn(nadd(idem))
                  
               case(4322)
               ! WGS72
                  nadd(idem) = 2
                  write(demk,8040) nadn(nadd(idem))
                  DUMMY = 'WGS72'
                  CALL ERRHDL(PATH,MODNAM,'W','470',DUMMY)
                  write(*,8045) DUMMY, idem
                  write(demk,8045) DUMMY, idem
                  
               case(4326)
               ! WGS84
                  nadd(idem) = 3
                  write(demk,8040) nadn(nadd(idem))
                  ! print warning if WGS84, may not be NED data from USGS
                  DUMMY = 'WGS84'
                  CALL ERRHDL(PATH,MODNAM,'W','470',DUMMY)
                  write(*,8045) DUMMY, idem
                  write(demk,8045) DUMMY, idem
                  
               case default
               ! Unidentified datum geokey, processing depends and value of NADA
                  
                  if (NADA .EQ. 0) then
                  ! specified DATUM not recognized, but user specified NADA=0
                  ! for no NAD conversion; assume NAD83 as the default datum;
                  ! issue warning but continue processing
                     ! print warning
                     write(dummy,'(i8)') idem
                     CALL ERRHDL(PATH,MODNAM,'W','471',DUMMY)
                     write(*,80371) geoType
                     write(demk,80371) geoType
                     nadd(idem) = 4
                     write(demk,8040) nadn(nadd(idem))

                  else 
                  ! specified DATUM not supported and user specified NADA/=0
                  ! for NAD conversion; issue error message.
                     
                     ! print error
                     write(dummy,'(i8)') idem
                     CALL ERRHDL(PATH,MODNAM,'E','472',DUMMY)
                     nederr = .TRUE.
                     write(*,80372) geoType
                     write(demk,80372) geoType
         
                  end if
                  
               end select
         
            else if (geoDatumFlg) then
            ! Geographic Datum code has been specified
            
               select case(geoDatum)
               case(6267)
               ! NAD27
                  nadd(idem) = 1 
                  write(demk,8040) nadn(nadd(idem))
                  DUMMY = 'NAD27'
                  CALL ERRHDL(PATH,MODNAM,'W','470',DUMMY)
                  write(*,8045) DUMMY, idem
                  write(demk,8045) DUMMY, idem
               case(6269)
               ! NAD83
                  nadd(idem) = 4
                  write(demk,8040) nadn(nadd(idem))
               case(6322)
               ! WGS72
                  nadd(idem) = 2
                  write(demk,8040) nadn(nadd(idem))
                  DUMMY = 'WGS72'
                  CALL ERRHDL(PATH,MODNAM,'W','470',DUMMY)
                  write(*,8045) DUMMY, idem
                  write(demk,8045) DUMMY, idem
               case(6326)
               ! WGS84
                  nadd(idem) = 3
                  write(demk,8040) nadn(nadd(idem))
                  ! print warning if WGS84, may not be NED data from USGS
                  DUMMY = 'WGS84'
                  CALL ERRHDL(PATH,MODNAM,'W','470',DUMMY)
                  write(*,8045) DUMMY, idem
                  write(demk,8045) DUMMY, idem
                  
               case default
               ! Unidentified datum geokey, processing depends and value of NADA
                  
                  if (NADA .EQ. 0) then
                  ! specified DATUM not recognized, but user specified NADA=0
                  ! for no NAD conversion; assume NAD83 as the default datum;
                  ! issue warning but continue processing
                     ! print warning
                     write(dummy,'(i8)') idem
                     CALL ERRHDL(PATH,MODNAM,'W','471',DUMMY)
                     write(*,80371) geoDatum
                     write(demk,80371) geoDatum
                     nadd(idem) = 4
                     write(demk,8040) nadn(nadd(idem))
                     
                  else 
                  ! specified DATUM not supported and user specified NADA/=0
                  ! for NAD conversion; issue error message.
                     
                     ! print error
                     write(dummy,'(i8)') idem
                     CALL ERRHDL(PATH,MODNAM,'E','472',DUMMY)
                     nederr = .TRUE.
                     write(*,80372) geoDatum
                     write(demk,80372) geoDatum
                     
                  end if
                  
               end select
               
            else if (geoEllipseFlg) then
            ! Geographic Ellipsoid code has been specified
         
               select case(geoEllipse)
               case(4008,6008,7008)
               ! Clarke 1866 ellipsoid = NAD27
                  nadd(idem) = 1 
                  write(demk,8040) nadn(nadd(idem))
                  ! print warning if WGS84, may not be NED data from USGS
                  DUMMY = 'NAD27'
                  CALL ERRHDL(PATH,MODNAM,'W','470',DUMMY)
                  write(*,8045) DUMMY, idem
                  write(demk,8045) DUMMY, idem
               case(4019,6019,7019)
               ! GRS80 ellipsoid = NAD83
                  nadd(idem) = 4
                  write(demk,8040) nadn(nadd(idem))
               case(4030,6030,7030)
               ! WGS84 ellipsoid = WGS84
                  nadd(idem) = 3
                  write(demk,8040) nadn(nadd(idem))
                  ! print warning if WGS84, may not be NED data from USGS
                  DUMMY = 'WGS84'
                  CALL ERRHDL(PATH,MODNAM,'W','470',DUMMY)
                  write(*,8045) DUMMY, idem
                  write(demk,8045) DUMMY, idem
               case default
               ! Unidentified ellipsoid geokey, processing depends and value of NADA
                  
                  if (NADA .EQ. 0) then
                  ! specified ellipsoid not recognized, but user specified NADA=0
                  ! for no NAD conversion; assume NAD83 as the default datum;
                  ! issue warning but continue processing
                     ! print warning
                     write(dummy,'(i8)') idem
                     CALL ERRHDL(PATH,MODNAM,'W','471',DUMMY)
                     write(*,80371) geoEllipse
                     write(demk,80371) geoEllipse
                     nadd(idem) = 4
                     write(demk,8040) nadn(nadd(idem))
                     
                  else 
                  ! specified ellipsoid not supported and user specified NADA/=0
                  ! for NAD conversion; issue error message.
                     
                     ! print error
                     write(dummy,'(i8)') idem
                     CALL ERRHDL(PATH,MODNAM,'E','472',DUMMY)
                     nederr = .TRUE.
                     write(*,80372) geoEllipse
                     write(demk,80372) geoEllipse
                     
                  end if
         
               end select
         
            else
            ! Geographic ModelType not adequately defined
            ! issue error messages for all three missing options
            ! (2048, 2050, and 2056)
            
               ! loop through gFlgs to find index for geoKey 2048
               do k=1, size(gFlgs)
                  if (gFlgs(k)%id == 2048) exit
               end do
               
               WRITE (dumflg,'(I8)') gFlgs(k)%id
               CALL ERRHDL(PATH,MODNAM,'E','460',dumflg)
               nederr = .TRUE.
               write(*,8032) gFlgs(k)%id, gFlgs(k)%name, idem
               write(demk,8032) gFlgs(k)%id, gFlgs(k)%name, idem
     
               ! loop through gFlgs to find index for geoKey 2050
               do k=1, size(gFlgs)
                  if (gFlgs(k)%id == 2050) exit
               end do
               
               WRITE (dumflg,'(I8)') gFlgs(k)%id
               CALL ERRHDL(PATH,MODNAM,'E','460',dumflg)
               nederr = .TRUE.
               write(*,8032) gFlgs(k)%id, gFlgs(k)%name, idem
               write(demk,8032) gFlgs(k)%id, gFlgs(k)%name, idem
     
               ! loop through gFlgs to find index for geoKey 2056
               do k=1, size(gFlgs)
                  if (gFlgs(k)%id == 2056) exit
               end do
               
               WRITE (dumflg,'(I8)') gFlgs(k)%id
               CALL ERRHDL(PATH,MODNAM,'E','460',dumflg)
               nederr = .TRUE.
               write(*,8032) gFlgs(k)%id, gFlgs(k)%name, idem
               write(demk,8032) gFlgs(k)%id, gFlgs(k)%name, idem
     
            end if
            
         end select      ! end of ModelType processing
         

C ---    Processing of elevation units geoKeys and optional user-specfied
C        elevation units and vertical scale resolution.
C        If elevation units flag is still false, write message to debug file
         if (.not. elvUntsFlg) then
         ! NED file does not include vertical elevation units geoKey (4099)
            if (.NOT. L_UserElevUnits(idem)) then
            ! No user-specified units defined; assume default of meters;
            ! vertical resolution (DCI) has already be set above based on
            ! pxlScalez
               write(*,8060) idem
               write(demk,8060) idem
 8060  format(/
     &  ' WARNING: Elevation units not found in NED file ',i6,'.'
     & /'          Default units of METERS assumed.'/)
               ELUNIT(idem) = 2
C              DCI (vertical resolution) already set based on pxlScalez               
               write(dummy,'(i8)') idem
               CALL ERRHDL(PATH,MODNAM,'W','473',DUMMY)
               
            else if (L_UserElevUnits(idem)) then
            ! User-specified elevation units:
               ELUNIT(idem) = UserElevUnits(idem)
               ! Check for conflict with vertical scale resolution
               if (UserDCI(idem) /= DCI(idem)) then
                 ! write error message; user-specified vertical scale 
                 ! doesn't match data file
                  write(*,8061) UserDCI(idem), DCI(idem), idem
                  write(demk,8061) UserDCI(idem), DCI(idem), idem
 8061  format(/
     &  '   ERROR: User-specified Elevation Scale Factor: ',f9.4,
     & /'                 Conflicts with NED file GeoKey: ',f9.4,
     & /'                                    in NED file: ',i6/)
                  WRITE(DUMMY,'(I8)') IDEM
                  CALL ERRHDL(PATH,MODNAM,'E','477',DUMMY)
                  nederr = .true.
                  
               else
                 ! User-specified elevation units will be used
                  write(demk,8062) idem, Chr_UserElevUnits(idem)
 8062  format(/
     &  ' WARNING: User-specified Elevation Units used for ',
     &                                           'NED file ',i6,'.'
     & /'          Elevation Units: ',a11/)
                  DCI(idem)    = UserDCI(idem)
                  WRITE(DUMMY,'(I8)') IDEM
                  CALL ERRHDL(PATH,MODNAM,'W','474',DUMMY)
               end if
            end if
            
         else if (elvUntsFlg .and. L_UserElevUnits(idem)) then
         ! Vertical elevation geoKey specified and user-specified units;
         ! will issue error message if user-specified units don't agree
         ! with geoKey
            ! First check units:
            if (ELUNIT(idem) /= UserElevUnits(idem)) then
               write(*,8063) Chr_UserElevUnits(idem), 
     &                       LVLN(ELUNIT(idem)), idem
               write(demk,8063) Chr_UserElevUnits(idem), 
     &                          LVLN(ELUNIT(idem)), idem
 8063  format(/
     &  '   ERROR: User-specified Elevation Units: ',a11,
     & /'           Conflict with NED file GeoKey: ',a6,
     & /'                             in NED file: ',i6/)
               WRITE(DUMMY,'(I8)') IDEM            
               CALL ERRHDL(PATH,MODNAM,'E','476',DUMMY)
               nederr = .true.
            end if
            ! Now check for conflict with vertical scale resolution
            if (UserDCI(idem) /= DCI(idem)) then
              ! write error message; user-specified vertical scale 
              ! doesn't match data file
               write(*,8061) UserDCI(idem), DCI(idem), idem
               write(demk,8061) UserDCI(idem), DCI(idem), idem
               WRITE(DUMMY,'(I8)') IDEM
               CALL ERRHDL(PATH,MODNAM,'E','477',DUMMY)
               nederr = .true.
            end if
            ! Check for user-specified units matching NED file units
            ! If units match, then reset L_UserElevUnits to false for
            ! receptor and sourcloc file headers.
            if (ELUNIT(idem)  == UserElevUnits(idem) .and.
     &          UserDCI(idem) == DCI(idem)) then
               ! vertical units and resolution match, reset L_UserElevUnits
               L_UserElevUnits(idem) = .false.
               write(*,8064) Chr_UserElevUnits(idem), idem
               write(demk,8064) Chr_UserElevUnits(idem), idem
 8064  format(/
     &  ' NOTE: User-specified Elevation Units: ',a11,
     & /'       Match NED file Elevation Units  ',
     & /'                          in NED file: ',i6/)
            end if

         else 
         ! Vertical elevation geoKey specified and no user-specified units;
         ! no further action required

         end if

C ---    Special tag dependency processing
C        Adjust tie points, derive corner coordinates
C        These are dependent on the pixel scale and raster type, 
C        so first need to check for existence of pixel scale,
C        raster type and tie points

         if (pxlSclFlg .and. tiePntFlg .and. RasterTypeFlg) then

C ---       Process GT Raster Type code; determine whether to adjust
C           model tie point
C ---       Assign adjusted or unadjusted value to DMCNR array NW point
            if (PixelIsArea) then
               write(demk,*) 'GT Raster Type = Pixel Is Area'
               write(demk,*)
     &    '  Model Tie Point (NW corner) adjusted to midpoint of pixel.'
               ! adjust model tie point to "data point" location based on
               ! PixelIsArea raster type; (0,0) is upper left corner of pixel
               ! Adjust x-coord to center of pixel, 1/2 pixel scale to the East
               DMCNR(idem,1,2) = tiePtx(idem)+
     &                        (0.5D0-tiePtAncx)*pxlScalex(idem)
               ! Adjust y-coord to center of pixel, 1/2 pixel scale to the South
               DMCNR(idem,2,2) = tiePty(idem)-
     &                        (0.5D0-tiePtAncy)*pxlScaley(idem)

            else if (PixelIsPoint) then
               write(demk,*) 'GT Raster Type = Pixel Is Point'
               write(demk,*)
     &    '  Model Tie Point (NW corner) coincides with data point.'
               ! adjust model tie point to "data point" location based on
               ! PixelIsPoint raster type; (0,0) coincides with data location
               ! Adjust x-coord based on "anchor" point if needed
               DMCNR(idem,1,2) = tiePtx(idem) - 
     &                           tiePtAncx*pxlScalex(idem)
               ! Adjust y-coord based on "anchor" point if needed
               DMCNR(idem,2,2) = tiePty(idem) + 
     &                           tiePtAncy*pxlScaley(idem)

            end if

C ---       Vertical scale:
            ! adjust tiePtz to represent elevation above
            ! vertical anchor point, tiePtAncz;
            ! this is treated the same as the "elevation of
            ! local datum" field in DEM data (data element 4 in Record B)
            if (pxlScalez(idem) /= 0.0D0) then
               ! vertical pixel scale is non-zero, apply to anchor-z
               ! to determine vertical "offset" of elevation data;
               ! pxlScalez is assumed to have the same units as the
               ! elevation data and anchor-z point; the elevation units
               ! should be defined using the VerticalUnitsGeoKey (4099),
               ! but this is typically not included and meters are the
               ! assumed default elevation units.
               ! Also, note that tiePtx, tiePtAncz, and pxlScalez are
               ! typically 0.0 in GeoTIFF elevation files (for now!)
               
               tiePtz(idem) = tiePtz(idem) - tiePtAncz*pxlScalez(idem)
               
            else if (tiePtAncz /= 0.0D0) then
               ! vertical anchor point is non-zero, but vertical pixel
               ! scale is zero; assume pxlScalez = 1.0 to account for
               ! vertical anchor point offset
               
               tiePtz(idem) = tiePtz(idem) - tiePtAncz
               
            else
               ! vertical anchor point and vertical pixel scale are 
               ! both zero (apparently normal practice, for now!);
               ! no adjustments to tiePtz needed
               
            end if

C ---       Check for ground planimetric reference system (geographic or UTM)
C           and horizontal units consistency; conversions to standard units of
C           arc-seconds for geographic and meters for projected (UTM) files is
C           done below.               
C ---       Determine DMCNR coordinates and apply horizontal unit 
C           conversions, if needed
C           Start with corner index = 2 - NW Corner (upper left corner, 
C           corresponds with model tie point, with possible adjusment to
C ---       pixel center)
            if (iplan(idem) == 0) then
C ---          Convert horizontal units to arc-seconds for geographic files
               if (cunit(idem) == 0) then
               ! Geographic data with units of radians (user-specified)
               ! Convert from radians to arc-seconds           
                  DMCNR(idem,1,2) = DMCNR(idem,1,2)*RTODEG*3600.0D0    ! X, Lon
                  DMCNR(idem,2,2) = DMCNR(idem,2,2)*RTODEG*3600.0D0    ! Y, Lat
                 ! Set spatial resolution in x and y directions (arc-seconds)
                  DXM(idem) = pxlScalex(idem)*RTODEG*3600.0D0
                  DYM(idem) = pxlScaley(idem)*RTODEG*3600.0D0
C ---             Write out model tie point and spatial resolution to MAPDETAIL.OUT file                  
                  write(demk,510) tiePtx(idem)*RTODEG, 
     &                            tiePtx(idem)*RTODEG*3600.0D0
                  write(demk,511) tiePty(idem)*RTODEG,
     &                            tiePty(idem)*RTODEG*3600.0D0
 510              format(' X Model Tie Point (NW corner of file):  ',
     &                   f12.6,' (degrees); ',f12.3,' (arc-seconds)')
 511              format(' Y Model Tie Point (NW corner of file):  ',
     &                   f12.6,' (degrees); ',f12.3,' (arc-seconds)')
                  write(demk,520) tiePtz(idem), LVLN(ELUNIT(idem))
 520              format(' Z Model Tie Point (reference elevation): ',
     &                    f11.3,2x,a6)
C ---             Check for unexpected combination of pxlScalez and tiePtAncz
                  if (pxlScalez(idem) == 0.0D0 .and.
     &                      tiePtAncz /= 0.0D0) then
                     ! this deserves a warning message:
                     write(*,528) tiePtAncz, IDEM
                     write(demk,528) tiePtAncz, IDEM
 528                 format(1X,
     &                'TIFF file includes zero vertical pixel scale',
     &                ' with non-zero vertical anchor point; ',
     &               /'     tiePtAncz = ',D24.15,
     &               /'Assuming vertical pixel scale of 1.0.',
     &               /'NED File: ',I6)
                     write(dummy,'(i8)') IDEM
                     CALL ERRHDL(PATH,MODNAM,'W','483',DUMMY)
                  end if

                  write(demk,515) ' Spatial Res. X: ',
     &                            pxlScalex(idem)*RTODEG, 
     &                            pxlScalex(idem)*RTODEG*3600.0D0
                  write(demk,515) ' Spatial Res. Y: ',
     &                            pxlScaley(idem)*RTODEG, 
     &                            pxlScaley(idem)*RTODEG*3600.0D0
 515              format(a,f12.9,' (degrees); ',f12.6,' (arc-seconds)')
                  write(demk,519) DCI(idem), LVLN(ELUNIT(idem))
 519              format(' Spatial Res. Z: ',f12.4,2x,a6)
                     
                 ! Reassign cunit to 3 for arc-seconds            
                  cunit(idem) = 3
                  
               else if (cunit(idem) == 3) then
               ! Geographic data with units of arc-seconds
               ! No conversion necessary, tiePtx/y in arc-seconds           
                  DMCNR(idem,1,2) = DMCNR(idem,1,2)                    ! X, Lon
                  DMCNR(idem,2,2) = DMCNR(idem,2,2)                    ! Y, Lat
                 ! Set spatial resolution in x and y directions (arc-seconds)
                  DXM(idem) = pxlScalex(idem)
                  DYM(idem) = pxlScaley(idem)
C ---             Write out model tie point and spatial resolution to MAPDETAIL.OUT file                  
                  write(demk,510) tiePtx(idem)/3600.0D0, tiePtx(idem)
                  write(demk,511) tiePty(idem)/3600.0D0, tiePty(idem)
                  write(demk,520) tiePtz(idem), LVLN(ELUNIT(idem))
C ---             Check for unexpected combination of pxlScalez and tiePtAncz
                  if (pxlScalez(idem) == 0.0D0 .and.
     &                      tiePtAncz /= 0.0D0) then
                     ! this deserves a warning message:
                     write(*,528) tiePtAncz, IDEM
                     write(demk,528) tiePtAncz, IDEM
                     write(dummy,'(i8)') IDEM
                     CALL ERRHDL(PATH,MODNAM,'W','483',DUMMY)
                  end if
     
                  write(demk,515) ' Spatial Res. X: ',
     &               pxlScalex(idem)/3600.0D0, pxlScalex(idem)
                  write(demk,515) ' Spatial Res. Y: ',
     &               pxlScaley(idem)/3600.0D0, pxlScaley(idem)
                  write(demk,519) DCI(idem), LVLN(ELUNIT(idem))
                          
               else if (cunit(idem) == 4) then
               ! Geographic data with units of decimal degrees
               ! Convert from degrees to arc-seconds           
                  DMCNR(idem,1,2) = DMCNR(idem,1,2)*3600.0D0           ! X, Lon
                  DMCNR(idem,2,2) = DMCNR(idem,2,2)*3600.0D0           ! Y, Lat
                 ! Set spatial resolution in x and y directions (arc-seconds)
                  DXM(idem) = pxlScalex(idem)*3600.0D0
                  DYM(idem) = pxlScaley(idem)*3600.0D0
C ---             Write out model tie point and spatial resolution to MAPDETAIL.OUT file                  
                  write(demk,510) tiePtx(idem), tiePtx(idem)*3600.0D0
                  write(demk,511) tiePty(idem), tiePty(idem)*3600.0D0
                  write(demk,520) tiePtz(idem), LVLN(ELUNIT(idem))
C ---             Check for unexpected combination of pxlScalez and tiePtAncz
                  if (pxlScalez(idem) == 0.0D0 .and.
     &                      tiePtAncz /= 0.0D0) then
                     ! this deserves a warning message:
                     write(*,528) tiePtAncz, IDEM
                     write(demk,528) tiePtAncz, IDEM
                     write(dummy,'(i8)') IDEM
                     CALL ERRHDL(PATH,MODNAM,'W','483',DUMMY)
                  end if

                  write(demk,515) ' Spatial Res. X: ',
     &               pxlScalex(idem), pxlScalex(idem)*3600.0D0
                  write(demk,515) ' Spatial Res. Y: ',
     &               pxlScaley(idem), pxlScaley(idem)*3600.0D0
                  ! Write out vertical resolution
                  write(demk,519) DCI(idem), LVLN(ELUNIT(idem))
                          
                 ! Reassign cunit to 3 for arc-seconds            
                  cunit(idem) = 3
                  
               else
               ! Invalid combination 
                  write(*,518) IDEM
                  write(demk,518) IDEM
 518              format(1X,'TIFF File Mismatch: IPLAN and CUNIT!',
     &                      ' NED File: ',I6)
                  write(dummy,'(i4,i4)') iplan(idem), cunit(idem)
                  CALL ERRHDL(PATH,MODNAM,'E','466',DUMMY)
                  nederr = .TRUE.
                  cycle NEDLoop
                 
               end if
               
            else if (iplan(idem) == 1) then
C ---          Convert horizontal units to meters for UTM projected files
               if (cunit(idem) == 1) then
               ! Projected (UTM) data with units of feet
               ! Convert from feet to meters;
                  DMCNR(idem,1,2) = DMCNR(idem,1,2)*0.3048D0           ! X, East
                  DMCNR(idem,2,2) = DMCNR(idem,2,2)*0.3048D0           ! Y, North
                 !Set spatial resolution in x and y directions (meters)
                  DXM(idem) = pxlScalex(idem)*0.3048D0
                  DYM(idem) = pxlScaley(idem)*0.3048D0
C ---             Write out model tie point and spatial resolution to MAPDETAIL.OUT file                  
                  write(demk,512) tiePtx(idem)*0.3048D0, tiePtx(idem)
                  write(demk,513) tiePty(idem)*0.3048D0, tiePty(idem)
 512              format(' X Model Tie Point (NW corner of file):  ',
     &                   f12.3,' (meters); ',f12.3,' (feet)')
 513              format(' Y Model Tie Point (NW corner of file):  ',
     &                   f12.3,' (meters); ',f12.3,' (feet)')
                  write(demk,520) tiePtz(idem), LVLN(ELUNIT(idem))
C ---             Check for unexpected combination of pxlScalez and tiePtAncz
                  if (pxlScalez(idem) == 0.0D0 .and.
     &                      tiePtAncz /= 0.0D0) then
                     ! this deserves a warning message:
                     write(*,528) tiePtAncz, IDEM
                     write(demk,528) tiePtAncz, IDEM
                     write(dummy,'(i8)') IDEM
                     CALL ERRHDL(PATH,MODNAM,'W','483',DUMMY)
                  end if

                  write(demk,516) ' Spatial Res. X: ',
     &               pxlScalex(idem)*0.3048D0, pxlScalex(idem)
                  write(demk,516) ' Spatial Res. Y: ',
     &               pxlScaley(idem)*0.3048D0, pxlScaley(idem)
 516              format(a,f12.3,' (meters); ',f12.3,' (feet)')
                  write(demk,519) DCI(idem), LVLN(ELUNIT(idem))
                          
                 ! Reassign cunit to 2 for meters
                  cunit(idem) = 2
                  
               else if (cunit(idem) == 2) then
               ! Projected (UTM) data with units of meters
               ! No conversion necessary, tiePtx/y in meters
                  DMCNR(idem,1,2) = DMCNR(idem,1,2)                    ! X, East
                  DMCNR(idem,2,2) = DMCNR(idem,2,2)                    ! Y, North
                 !Set spatial resolution in x and y directions (meters)
                  DXM(idem) = pxlScalex(idem)
                  DYM(idem) = pxlScaley(idem)
C ---             Write out model tie point and spatial resolution to MAPDETAIL.OUT file                  
                  write(demk,512) tiePtx(idem), tiePtx(idem)/0.3048D0
                  write(demk,513) tiePty(idem), tiePty(idem)/0.3048D0
                  write(demk,520) tiePtz(idem), LVLN(ELUNIT(idem))
C ---             Check for unexpected combination of pxlScalez and tiePtAncz
                  if (pxlScalez(idem) == 0.0D0 .and.
     &                      tiePtAncz /= 0.0D0) then
                     ! this deserves a warning message:
                     write(*,528) tiePtAncz, IDEM
                     write(demk,528) tiePtAncz, IDEM
                     write(dummy,'(i8)') IDEM
                     CALL ERRHDL(PATH,MODNAM,'W','483',DUMMY)
                  end if

                  write(demk,516) ' Spatial Res. X: ',
     &               pxlScalex(idem), pxlScalex(idem)/0.3048D0
                  write(demk,516) ' Spatial Res. Y: ',
     &               pxlScaley(idem), pxlScaley(idem)/0.3048D0
                  write(demk,519) DCI(idem), LVLN(ELUNIT(idem))

               else
               ! Invalid combination 
                  write(*,518) IDEM
                  write(demk,518) IDEM
                  write(dummy,'(i4,i4)') iplan(idem), cunit(idem)
                  CALL ERRHDL(PATH,MODNAM,'E','466',DUMMY)
                  nederr = .TRUE.
                  cycle NEDLoop
                 
               end if
            end if               

C ----      Check for appropriate range of horizontal resolution (DXM):
C           Geographic:  current range of standard resolutions is
C                        1/9 arc-second to 3 arc-seconds;
C                        warn for DXM > 3 seconds, error for DXM > 30 seconds
C                        warn for DXM < 0.1 second
C           Projected:   current range of standard resolutions is
C                        3 meters to 90 meters;
C                        warn for DXM > 100 meters, error for DXM > 1000 meters
C                        warn for DXM < 1 meter
C ----
            if (IPLAN(idem) == 0) then
            ! Geographic file
            
               if (DXM(idem) > 60.0D0) then
               ! resolution is too coarse for AERMAP, issue error
                  write(*,8050) DXM(idem), idem
                  write(demk,8050) DXM(idem), idem
                  
                  WRITE(DUMMY,'(I8)') IDEM
                  CALL ERRHDL(PATH,MODNAM,'E','484',DUMMY)
                  nederr = .true.
                  cycle NEDLoop

 8050  format(/
     &  '   ERROR: Horizontal resolution (',D24.15,' arc-seconds) is ',
     &                                                           'too',
     & /'          large in NED file ',i6,'.'/)
     
               else if ((DXM(idem) > 3.0D0 .and. 
     &                   DMCNR(idem,2,2)*3600.0D0 <= 50.0D0) .or.
     &                  (DXM(idem) > 6.0D0 .and.
     &                   DMCNR(idem,2,2)*3600.0D0 <= 70.0D0) .or.
     &                  (DXM(idem) > 9.0D0 .and.
     &                   DMCNR(idem,2,2)*3600.0D0  > 70.0D0)) then
               ! resolution may be too coarse for AERMAP, issue warning
                  write(*,8051) DXM(idem), idem
                  write(demk,8051) DXM(idem), idem
                  WRITE(DUMMY,'(I8)') IDEM
                  CALL ERRHDL(PATH,MODNAM,'W','485',DUMMY)
 8051  format(/
     &  ' WARNING: Horizontal resolution (',D24.15,' arc-seconds) ',
     &                                                        'may be',
     & /'          too large in NED file ',i6,'.'/)
     
               else if (DXM(idem) < 0.1D0) then
               ! resolution may be too small AERMAP, issue warning
                  write(*,8052) DXM(idem), idem
                  write(demk,8052) DXM(idem), idem
                  WRITE(DUMMY,'(I8)') IDEM
                  CALL ERRHDL(PATH,MODNAM,'W','485',DUMMY)
 8052  format(/
     &  ' WARNING: Horizontal resolution (',D24.15,' arc-seconds) ',
     &                                                        'may be',
     & /'          too small in NED file ',i6,'.'/)
     
               end if
               
            else if (IPLAN(idem) == 1) then
            ! Projection (UTM) file
            
               if (DXM(idem) > 1000.0D0) then
               ! resolution is too coarse for AERMAP, issue error
                  write(*,8053) DXM(idem), idem
                  write(demk,8053) DXM(idem), idem
                  
                  WRITE(DUMMY,'(I8)') IDEM
                  CALL ERRHDL(PATH,MODNAM,'E','484',DUMMY)
                  nederr = .true.
                  cycle NEDLoop
 8053  format(/
     &  '   ERROR: Horizontal resolution (',D24.15,' meters) is too',
     & /'          large in NED file ',i6,'.'/)
     
               else if (DXM(idem) > 100.0D0) then
               ! resolution may be too coarse for AERMAP, issue warning
                  write(*,8054) DXM(idem), idem
                  write(demk,8054) DXM(idem), idem
                  WRITE(DUMMY,'(I8)') IDEM
                  CALL ERRHDL(PATH,MODNAM,'W','485',DUMMY)
 8054  format(/
     &  ' WARNING: Horizontal resolution (',D24.15,' meters) may be',
     & /'          too large in NED file ',i6,'.'/)
     
               else if (DXM(idem) < 1.0D0) then
               ! resolution may be too small AERMAP, issue warning
                  write(*,8055) DXM(idem), idem
                  write(demk,8055) DXM(idem), idem
                  WRITE(DUMMY,'(I8)') IDEM
                  CALL ERRHDL(PATH,MODNAM,'W','485',DUMMY)
 8055  format(/
     &  ' WARNING: Horizontal resolution (',D24.15,' meters) may be',
     & /'          too small in NED file ',i6,'.'/)
     
               end if
               
            end if    
           
            ! Derive corner coordinates for remaining corners (SW, NE, and SE)

            if (IPLAN(idem) /= 9 .and. CUNIT(idem) /= 9) then
            ! Determine DMCNR coordinates for remaining corners
               ! 1 - SW Corner
               DMCNR(idem,1,1) = DMCNR(idem,1,2)                    ! X, Lon
               DMCNR(idem,2,1) = DMCNR(idem,2,2) 
     &                        - (DYM(idem) * (dble(nRows(idem)-1))) ! Y, Lat
                 
               ! 3 - NE Corner
               DMCNR(idem,1,3) = DMCNR(idem,1,2)
     &                        + (DXM(idem) * (dble(nCols(idem)-1))) ! X, Lon
               DMCNR(idem,2,3) = DMCNR(idem,2,2)                    ! Y, Lat
                 
               ! 4 - SE Corner
               DMCNR(idem,1,4) = DMCNR(idem,1,3)                    ! X, Lon
               DMCNR(idem,2,4) = DMCNR(idem,2,1)                    ! Y, Lat
           
               if (IPLAN(idem) == 0) then
                  write(demk,500) ' SW Corner Lon (degrees): ',
     &               DMCNR(idem,1,1)/3600.0D0
                  write(demk,500) ' SW Corner Lat (degrees): ',
     &               DMCNR(idem,2,1)/3600.0D0
                  write(demk,500) ' NW Corner Lon (degrees): ',
     &               DMCNR(idem,1,2)/3600.0D0
                  write(demk,500) ' NW Corner Lat (degrees): ',
     &               DMCNR(idem,2,2)/3600.0D0
                  write(demk,500) ' NE Corner Lon (degrees): ',
     &               DMCNR(idem,1,3)/3600.0D0
                  write(demk,500) ' NE Corner Lat (degrees): ',
     &               DMCNR(idem,2,3)/3600.0D0
                  write(demk,500) ' SE Corner Lon (degrees): ',
     &               DMCNR(idem,1,4)/3600.0D0
                  write(demk,500) ' SE Corner Lat (degrees): ',
     &               DMCNR(idem,2,4)/3600.0D0
 500              format(a25,f12.6)
               else if (IPLAN(idem) == 1) then
                  write(demk,501) ' SW Corner Easting  (meters): ',
     &               DMCNR(idem,1,1)
                  write(demk,501) ' SW Corner Northing (meters): ',
     &               DMCNR(idem,2,1)
                  write(demk,501) ' NW Corner Easting  (meters): ',
     &               DMCNR(idem,1,2)
                  write(demk,501) ' NW Corner Northing (meters): ',
     &               DMCNR(idem,2,2)
                  write(demk,501) ' NE Corner Easting  (meters): ',
     &               DMCNR(idem,1,3)
                  write(demk,501) ' NE Corner Northing (meters): ',
     &               DMCNR(idem,2,3)
                  write(demk,501) ' SE Corner Easting  (meters): ',
     &               DMCNR(idem,1,4)
                  write(demk,501) ' SE Corner Northing (meters): ',
     &               DMCNR(idem,2,4)
 501              format(a30,f12.3)
               end if
               
            end if

C----       Get UTM zone for file based on SW corner
            if (IPLAN(idem) == 0) then
               IZO(idem) = utm_zone((DMCNR(idem,1,1)+nudge)/3600.0D0)
               if (((DMCNR(idem,2,1)+nudge)/3600.0D0) .LT. 0.0D0) then
                  IZO(idem) = -1*IZO(idem)
               end if              
               IZOND(idem) = IZO(idem)
            end if
            
            if (ABS(IZO(idem)) .ge.  1 .and.
     &          ABS(IZO(idem)) .le. 60) then            
               write(demk,550) IZO(idem)
 550           format(' Reference UTM Zone (SW corner): ',i3) 
            else
C----          Invalid UTM zone            
               write(*,8080) IDEM, IZO(IDEM)
               write(demk,8080) IDEM, IZO(IDEM)
               write(dummy,'(I8)') IDEM
               CALL ERRHDL(PATH,MODNAM,'E','486',DUMMY)
               nederr = .TRUE.
               
 8080  format(/
     &  '   ERROR: Invalid UTM Zone based on SW corner coordinates ',
     & /'          in NED file: ',i6,
     & /'          UTM Zone =  ',i12,
     & /'          Projection may not be supported or file may not ',
     &          'be NED data.')
                  
               cycle NEDLoop
            end if
            
         end if  ! end of pxlSclFlg, tiePntFlg, and RasterTypeFlg 
                 ! if-then-endif block;
                 ! errors already generated if pixel scale, model
                 ! tie point, or raster type were not defined

C ---    Check for whether NADCON grid files will be needed
         IF( (NADA.EQ.1  .OR.  NADA.GE.5) .AND.
     &       (NADD(IDEM).GE.2 .AND. NADD(IDEM).LE.4) )THEN

            L_NeedNADCON = .TRUE.

         ELSE IF( (NADA.GE.2  .AND. NADA.LE.4) .AND.
     &            (NADD(IDEM).EQ.1 .OR.  NADD(IDEM).GE.5) )THEN

            L_NeedNADCON = .TRUE.

         END IF
      
      end do NEDLoop     ! loop over NED files
      

      close(demk) ! Close MAPDETAIL.OUT
      write(iounit,'(/,a)') ' Exiting NEDCHK'
      write(*,'(/,a)') ' Exiting NEDCHK'
      
      return
      
      end subroutine  
