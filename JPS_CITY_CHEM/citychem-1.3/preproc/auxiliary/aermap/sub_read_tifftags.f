      subroutine read_tifftags(iFile,iUnit,oFile,oUnit,
     &                         L_Debug,dbgFile,dbgUnit)
C**********************************************************************
C*        SUBROUTINE read_tifftags
C*
C*        PURPOSE: Read TIFF Tags and Geo Keys from GeoTIFF data files. 
C*                 Extracts all tiff tags and geo keys within the GeoTIFF
C*                 file.  Data are stored within the tTags and gKeys 
C*                 derived data types, defined in module TiffTags.
C*
C*        PROGRAMMERS: Clint Tillerson, MACTEC
C*                     Roger Brode, US EPA, OAQPS, AQMG
C*
C*        DATE:  February 9, 2009
C*     
C*        IMPLEMENTATION NOTES:
C*
C*                 This routine processes a single GeoTIFF file
C*                 at a time.  The routine outputs information to
C*                 the default output device (unit=*), as well as
C*                 a user-specified filename and file unit.  This
C*                 includes warning and/or error messages. 
C*                 The calling arguments for this routine also allow
C*                 for an optional debug output file to be created
C*                 that contains a summary of all TIFF Tags and 
C*                 Geo Keys found within the input file.  This routine
C*                 will open the optional debug output file if needed,
C*                 but will append to an existing debug file if it
C*                 is already openned.
C*
C*                 Most integer variables are declared as 8-byte 
C*                 integers (KIND=8), to allow maximum flexibility
C*                 regarding the range of field types, and to avoid
C*                 potential issues with precision associated with
C*                 unsigned 4-byte integers.  Fortran does not allow
C*                 for distinguishing between signed and unsigned 
C*                 integer variables.  Adjustments are made, as needed
C*                 for unsigned integers that exceed the range of 
C*                 signed integers.
C*
C*                 The routine supports both "Little Endian" and
C*                 "Big Endian" data files, and will perform byte 
C*                 swapping as needed through use of the Fortran
C*                 TRANSFER intrinsic function.
C*
C*                 TIFF Tags and Geo Keys are processed according to 
C*                 specifications for TIFF Revision 6 (June 3, 1992)
C*                 and GeoTIFF Specification, Version: 1.8.1 (October
C*                 31, 1995).  These specification documents are 
C*                 available at the following locations:
C*                 
C*                   www.alternatiff.com/resources/TIFF6.pdf 
C*
C*                   www.remotesensing.org/geotiff/spec/geotiffhome.html
C*  
C***********************************************************************

      USE TiffTags

      Implicit None

C --- Calling arguments: file/unit, record vars      
      character (len=*)  :: iFile    ! file name: geotiff data file
      integer            :: iUnit    ! file unit: data file
      
      character (len=*)  :: oFile    ! file name: output log file
      integer            :: oUnit    ! file unit: output log file
      
      logical            :: L_Debug  ! logical for debug file option;
                                     ! contains all tTag and gKey data
      character (len=*)  :: dbgFile  ! file name: optional debug file
      integer            :: dbgUnit  ! file unit: optional debug file
      
C --- file i/o status      
      integer            :: ios      ! file i/o status
      logical            :: iod      ! file opened
      
C --- array allocation status
      integer            :: als

C --- initrecl:  Record length for unformatted data, set to 1 byte/record.
C     Some compilers/systems assume 4 bytes/record; use compiler option to 
C     interpret as bytes rather than words when needed;
C --- e.g., Compaq and Intel = /assume:byterecl
      integer, parameter  :: initrecl = 1  ! record length (bytes) when reading TIFF tags
      integer (kind=8)    :: thisrec       ! current record to read in file 
      integer (kind=8)    :: temprec       ! temporary place holder
           
C --- endianness, byteorder        
      character (len=2)   :: byteorder     ! byte order of geotiff (endianess, 
                                           !                        II=little; MM=big)
      character (len=1)   :: cpu_end       ! endianness of host machine [B or L]
      character (len=1)   :: fil_end       ! endianness of data file (tiff file) [B or L]
      
C --- counters      
      integer             :: i,j,k         ! generic counters  

C --- temp vars for reading tiff tags and geokeys
      character (len=1)  :: tmpChr1(8)     ! character temp variable array
      integer (kind=1)   :: tmpInt1(8)     ! integer temp variable array
      integer (kind=2)   :: tmpInt2        ! integer temp
      integer (kind=4)   :: tmpInt4        ! integer temp

C --- variables for tiff and geokey info    
      integer (kind=2)   :: tiffID         ! tiff file ID (magic number 42) 
      integer (kind=8)   :: IFDOffset      ! offset to 1st image file directory 
      integer (kind=8)   :: IFDEntryCnt    ! number of entries (tags) in IFD 
                                           ! (assumes 1 IFD)
            
      logical            :: geoDir         ! geokey  directory exists?
      integer (kind=8)   :: geoDirRef      ! offset (in bytes) to geokey directory
      integer (kind=8)   :: geoDirNdx      ! index for geokey directory in tiff tag array 
      logical            :: geoDbls        ! geokey double values exist?
      integer (kind=8)   :: geoDblsRef     ! offset (in bytes) to geokey double precision values
      logical            :: geoAsc         ! geokey ascii values exist?
      integer (kind=8)   :: geoAscRef      ! offset (in bytes) to geokey ascii values
      
      integer (kind=4)   :: numGeoKeys     ! number of geokeys referenced in geokey 
                                           ! directory (4-byte unsigned integer)
      
      logical            :: readAsVal      ! flag to indicate Tag value should be read 
                                           ! as the value (not offset)

C --- tiff tag field types - text description
      character (len=9) :: typeList(12) = 
     & (/ 'byte     ', 'ascii    ', 'short    ', 'long     ',
     &    'rational ', 'sbyte    ', 'undefined', 'sshort   ',
     &    'slong    ', 'srational', 'float    ', 'double   '/)       

C --- tiff tag field type lengths (length of single value)    
      integer (kind=1) :: typeLengths(12) = 
     &                   (/ 1, 1, 2, 4, 8,
     &                      1, 1, 2, 4, 8,
     &                               4, 8 /)

C --- hexadecimal constants for nul and newline for character replacement
      integer (kind=1)  :: dNewli, dNuli ! for initial integer assignment for some compilers
      character (len=1) :: dNewl, dNul   ! for final character assignment for replacement

C --- data statements for nul and newline
      data dNewli/ Z"0A" /
      data dNuli/ Z"00" /

C --- transer integer assignments to character      
      dNewl = transfer(dNewli,dNewl)
      dNul = transfer(dNuli,dNul)

C --- initialize flag to indicate geokey directory exists (tiff tag 34735)
      geoDir  = .false.
      geoDbls = .false.
      geoAsc  = .false.

C --- intialize tiff read error and warning flags to false
      tiffErr = .false.
      tiffWrn = .false.
      
C --- initialize allocation error flag to false
      allErr =  .false.
      
C --- initialize "geoRef" tags (8-byte integers)
      geoDirRef  = 0_8
      geoDblsRef = 0_8
      geoAscRef  = 0_8
      
C --- initialize temprec (8-byte integer)
      temprec = 0_8

C ---------------------------------------------------------------------
C --- Determine CPU endianness
C ---------------------------------------------------------------------
      call cpuEnd(cpu_end)
      
      ! if endianness could not be determined, print error and return
      if (cpu_end /= 'B' .and. cpu_end /= 'L') then
         write(*,50)
         write(oUnit,50,err=8000)
   50    format(/,' The byte order of the processor'
     &            ' could not be determined.')
         tiffErr = .true.
         return
      end if
      
C ---------------------------------------------------------------------
C --- Open GeoTIFF File to Read Tags and GeoKeys
C ---------------------------------------------------------------------
      open(unit=iUnit, iostat=ios, file=iFile, 
     &    form='UNFORMATTED', action='READ', status='OLD', 
     &    access='DIRECT', err=9000, recl=initrecl)

      write(oUnit,'(/," Attempting to read TIFF file header.")',
     &      err=8000) 

C ---------------------------------------------------------------------
C --- Open Optional GeoTIFF Debug Output File, if Requested
C ---------------------------------------------------------------------
      if (L_Debug) then
         iod = .false.
C ---    Check whether debug file is already openned         
         INQUIRE(unit=dbgUnit,opened=iod)
         if (.not. iod) then
C ---       Open status=REPLACE if not openned         
            OPEN(UNIT=dbgUnit,FILE=dbgFile,STATUS='REPLACE',ERR=9100)
         end if
      end if
      
C ---------------------------------------------------------------------         
C --- Read 8-byte header (endianness, tiff magic number, offset to IFD)
C ---------------------------------------------------------------------  
  
      read(unit=iUnit, rec=1, err=9010) tmpChr1(1)! endianness 
      read(unit=iUnit, rec=2, err=9010) tmpChr1(2)! endianness      
      read(unit=iUnit, rec=3, err=9010) tmpInt1(1)! magic number (42)   
      read(unit=iUnit, rec=4, err=9010) tmpInt1(2)! magic number (42)
      read(unit=iUnit, rec=5, err=9010) tmpInt1(3)! IFD offset part 1 
      read(unit=iUnit, rec=6, err=9010) tmpInt1(4)! IFD offset part 2
      read(unit=iUnit, rec=7, err=9010) tmpInt1(5)! IFD offset part 3 
      read(unit=iUnit, rec=8, err=9010) tmpInt1(6)! IFD offset part 4

C ---------------------------------------------------------------------          
C --- Set file endiannes
C ---------------------------------------------------------------------

      ! transfer bytes to final variable
      byteOrder = transfer((/tmpChr1(1),tmpChr1(2)/),byteOrder)

      ! set file byte order
      if (byteOrder == 'II') then
         fil_end = 'L'
         write(oUnit,75,err=8000) 'Little Endian '
   75    format(/,' Byte order: ',a14)
      elseif (byteOrder == 'MM') then
         fil_end = 'B'
         write(oUnit,75,err=8000) 'Big Endian    '
      else
         write(*,110) iFile(1:LEN_TRIM(iFile))
         write(oUnit,110,err=8000) iFile(1:LEN_TRIM(iFile))
  110    format(/,' The byte order of the TIFF file: ',a,
     &          /,' could not be determined.')
         tiffErr = .true.
         return
      endif

C ---------------------------------------------------------------------
c     Set swap byte flag based on endianness of file Vs. CPU
c     Will need to swap bytes when reading tiff files if they
c     do not match.
C ---------------------------------------------------------------------
      if (cpu_end == fil_end) then
         swapbytes = .false.
      else 
         swapbytes = .true.
      end if

c     Swap bytes if needed; transfer to final variable      
      if (swapbytes) then
         tiffID  = transfer((/tmpInt1(2),tmpInt1(1)/),tiffID)
         tmpInt4 = transfer((/tmpInt1(6),tmpInt1(5),
     &                        tmpInt1(4),tmpInt1(3)/), tmpInt4)
         IFDOffset = INT(tmpInt4,kind=8)
      else
      !  transfer without byte swapping
         tiffID  = transfer((/tmpInt1(1),tmpInt1(2)/),tiffID)
         tmpInt4 = transfer((/tmpInt1(3),tmpInt1(4),
     &                        tmpInt1(5),tmpInt1(6)/), tmpInt4)
         IFDOffset = INT(tmpInt4,kind=8)
      end if

C --- Apply 4-byte unsigned integer offset if needed      
      if (IFDOffset<0) 
     &  IFDOffset = IFDOffset + intAdj4

C --- verify this is a TIFF file
      if (tiffID /= 42) then
         write(*,120) iFile(1:LEN_TRIM(iFile))
         write(oUnit,120,err=8000) iFile(1:LEN_TRIM(iFile))
  120    format(/,' The file: ',a,
     &          /,' is not correctly identified as a TIFF file.')  
         tiffErr = .true.
         return      
      endif

      write(oUnit,'(a23,i12)',err=8000)' IFD Offset (bytes): ',IFDOffset
      
C --- skip to IFD, calculate first record of IFD
      thisrec = IFDOffset

C --- read 2-byte IFD entry count; number of tiff tags
      read(unit=iUnit, rec=thisrec+1, err=9010) tmpInt1(1)     
      read(unit=iUnit, rec=thisrec+2, err=9010) tmpInt1(2)
     
      ! increment record by 2 bytes
      thisrec = thisrec+2
      
C --- Swap bytes if needed; transfer to final variable          
      if (swapbytes) then
         tmpInt2 = 
     &      transfer((/tmpInt1(2),tmpInt1(1)/),tmpInt2)
         IFDEntryCnt = INT(tmpInt2,kind=8)
      else
      !  transfer without byte swapping
         tmpInt2 = 
     &      transfer((/tmpInt1(1),tmpInt1(2)/),tmpInt2)
         IFDEntryCnt = INT(tmpInt2,kind=8)
      end if
      
C --- Apply unsigned 2-byte integer offset if needed      
      if (IFDEntryCnt<0) 
     &  IFDEntryCnt = IFDEntryCnt + intAdj2

C --- de/allocate tTag derived type array
      if (allocated(tTags)) deallocate(tTags)
      allocate(tTags(IFDEntryCnt),stat=als)
      if (als > 0) then
         write(*,130) iFile(1:LEN_TRIM(iFile))
         write(oUnit,130,err=8000) iFile(1:LEN_TRIM(iFile))
         allErr = .true.
         return
      end if
 130  format(
     &  /,' An allocation error occurred while processing the file:',
     &  /,a)

C --- read Image File Directory Entries (tiff tags)
      write(oUnit,'(/,a30,/)',err=8000) ' Attempting to read TIFF tags.'

cccccccccc Write header for optional debug file to print tiff tags ccccc
      if (L_Debug) then
         write(dbgUnit,*,err=7000)  'TIFF Debug Data for File: '
         write(dbgUnit,*,err=7000)
         write(dbgUnit,'(1x,a)',err=7000) iFile(1:LEN_TRIM(ifile))
         write(dbgUnit,*,err=7000)
         write(dbgUnit,*,err=7000)  '**** TIFF Tags:'
      end if
cccccccccccccccccccccccc end TiffTag debug code cccccccccccccccccccccccc


C ------------ Loop Over Image File Directory (IFD)  --------------
c 
c     loop over each IFD entry and read id, data type, count (number
c     of values for current entry), and value/offset.  If value of
c     entry fits in 4 bytes, then last four bytes of entry is the
c     value(s) of the tag.  If > 4 bytes, then last four bytes of the 
c     entry is the offset to the value(s).  Determine if value or
c     offset using count and size (bytes) of each value.  
c     If count*size <= 4 -> value(s)
c     If count*size  > 4 -> offset to value(s)
c
c     Field types (fldType):
c      1: BYTE,       1-byte unsigned integer
c      2: ASCII,      1-byte ASCII
c      3: 4SHORT,     2-byte unsigned integer
c      4: LONG,       4-byte unsigned integer
c      5: RATIONAL,   2 4-byte unsigned integers (numerator, denominator)
c      6: SBYTE,      1-byte signed integer
c      7: UNDEFINED,  8-bit byte that may contain anything depending on
c                       the definition of the field (private)
c      8: SSHORT,     2-byte signed integer
c      9: SLONG,      4-byte signed integer
c     10: SRATIONAL,  2 4-byte signed integers (numerator, denominator)
c     11: FLOAT,      4-byte single precision (IEEE)
c     12: DOUBLE,     8-byte double precision (IEEE)
C -----------------------------------------------------------------

      TagLoop: do i=1, size(tTags)
         
         ! re-initialize allocation error flag
         allErr =  .false.
         
         ! Initialize control flags to false
         readAsVal = .false.
         
         ! Initialize logical flag for problem to false
         tTags(i)%L_TiffProb = .false.

         ! Read tag id, data type, and number of values stored
         read(unit=iUnit, rec=thisrec+1, err=9010) tmpInt1(1) ! id
         read(unit=iUnit, rec=thisrec+2, err=9010) tmpInt1(2) ! id
         read(unit=iUnit, rec=thisrec+3, err=9010) tmpInt1(3) ! fldtype
         read(unit=iUnit, rec=thisrec+4, err=9010) tmpInt1(4) ! fldtype
         read(unit=iUnit, rec=thisrec+5, err=9010) tmpInt1(5) ! count, part 1
         read(unit=iUnit, rec=thisrec+6, err=9010) tmpInt1(6) ! count, part 2
         read(unit=iUnit, rec=thisrec+7, err=9010) tmpInt1(7) ! count, part 3
         read(unit=iUnit, rec=thisrec+8, err=9010) tmpInt1(8) ! count, part 4

         ! Swap bytes if needed; transfer to final variable          
         if (swapbytes) then
         !  transfer with byte swapping
            tmpInt2 = 
     &         transfer((/tmpInt1(2),tmpInt1(1)/),tmpInt2)
            tTags(i)%id = INT(tmpInt2,kind=8)
            
            tmpInt2 = 
     &         transfer((/tmpInt1(4),tmpInt1(3)/),tmpInt2)
            tTags(i)%fldType = INT(tmpInt2,kind=8)
     
            tmpInt4 = 
     &         transfer((/tmpInt1(8),tmpInt1(7),
     &                    tmpInt1(6),tmpInt1(5)/),
     &                    tmpInt4)
            tTags(i)%cnt = INT(tmpInt4,kind=8)
         else
         !  transfer without byte swapping
            tmpInt2 = 
     &         transfer((/tmpInt1(1),tmpInt1(2)/),tmpInt2)
            tTags(i)%id = INT(tmpInt2,kind=8)
            
            tmpInt2 = 
     &         transfer((/tmpInt1(3),tmpInt1(4)/),tmpInt2)
            tTags(i)%fldType = INT(tmpInt2,kind=8)
     
            tmpInt4 = 
     &         transfer((/tmpInt1(5),tmpInt1(6),
     &                    tmpInt1(7),tmpInt1(8)/),
     &                    tmpInt4)
            tTags(i)%cnt = INT(tmpInt4,kind=8)
         end if
         
c        Apply 2-byte unsigned integer adjustment to id if id < 0 
c        ID can be larger than 65536
         if(tTags(i)%id<0) 
     &      tTags(i)%id = tTags(i)%id+intAdj2

c        Apply 4-byte unsigned integer adjustment to cnt if cnt < 0 
         if(tTags(i)%cnt<0) 
     &      tTags(i)%cnt = tTags(i)%cnt+intAdj4

C ---    Allocate tag array value based on field type
c        Field types can be 1-12
         select case(tTags(i)%fldType)
         ! character
         case(2)
            if (allocated(tTags(i)%chrVal)) 
     &         deallocate(tTags(i)%chrVal)
            allocate(tTags(i)%chrVal(tTags(i)%cnt),stat=als)
            if (als > 0) allErr = .true.
         ! integer
         case(1,3:4,6,8:9)
            if (allocated(tTags(i)%intVal)) 
     &         deallocate(tTags(i)%intVal)
            allocate(tTags(i)%intVal(tTags(i)%cnt),stat=als)
            if (als > 0) allErr = .true.
         ! rational
         case(5,10)
            if (allocated(tTags(i)%ratVal)) 
     &         deallocate(tTags(i)%ratVal)
            allocate(tTags(i)%ratVal(tTags(i)%cnt,2),stat=als)
            if (als > 0) allErr = .true.
         ! real/double
         case(11,12)
            if (allocated(tTags(i)%dblVal)) 
     &         deallocate(tTags(i)%dblVal)
            allocate(tTags(i)%dblVal(tTags(i)%cnt),stat=als)
            if (als > 0) allErr = .true.  
         ! Undefined data type
         case default
C ---       Issue warning message and set logical flag indicating problem
C           with this tag         
            write(*,125) iFile(1:LEN_TRIM(iFile)), tTags(i)%id
            write(oUnit,125,err=8000) iFile(1:LEN_TRIM(iFile)), 
     &                                tTags(i)%id
  125       format(/,' The file: ',a,
     &             /,' contains an undefined data type for tag ID: ',
     &             i8)     
            tiffWrn = .true.
            tTags(i)%L_TiffProb = .true.
C ---       Skip to next tag, but include debug output if requested
            go to 991
         end select
        
C ----   Evaluate allocation error flag; if true write error and return
         if (allErr) then
            write(*,130) iFile(1:LEN_TRIM(iFile))
            write(oUnit,130,err=8000) iFile(1:LEN_TRIM(iFile))
            return
         end if
         
C ---    Determine if value/offset (last 4 bytes of entry) 
c        should be read as the value or offset to the value.
c        Set flag in tag array
         if (tTags(i)%cnt*typeLengths(tTags(i)%fldType) <= 4)
     &      readAsVal = .true.

         if (readAsVal) then
            temprec = thisrec+8
         
         else 
                     
C ---       Read offset as 4-byte integer
            read(unit=iUnit, rec=thisrec+9, err=9010) 
     &         tmpInt1(1)
            read(unit=iUnit, rec=thisrec+10, err=9010) 
     &         tmpInt1(2)
            read(unit=iUnit, rec=thisrec+11, err=9010) 
     &         tmpInt1(3)
            read(unit=iUnit, rec=thisrec+12, err=9010) 
     &         tmpInt1(4)

C ---       Swap bytes if needed; transfer to final variable
            if (swapbytes) then
               tmpInt4 = 
     &            transfer((/tmpInt1(4),tmpInt1(3),
     &                       tmpInt1(2),tmpInt1(1)/),
     &                       tmpInt4) 
               tTags(i)%offset = INT(tmpInt4,kind=8)
            else
               tmpInt4 = 
     &            transfer((/tmpInt1(1),tmpInt1(2),
     &                       tmpInt1(3),tmpInt1(4)/),
     &                       tmpInt4) 
               tTags(i)%offset = INT(tmpInt4,kind=8)
            end if

            ! check for unsigned integer if < 0
            ! adjust to allow up to 4 GB file sizes
            if (tTags(i)%offset < 0) then
               tTags(i)%offset = tTags(i)%offset + intAdj4
            end if

            ! set record based on offset
            temprec = tTags(i)%offset/initrecl
         
         end if

C ---    read value(s) - do not read values where fldType = 7 = UNDEFINED
         TagValLoop: do j=1,tTags(i)%cnt
         
            select case(tTags(i)%fldType)
            ! single byte integer - no need to byte swap
            case(1,6)
               read(unit=iUnit, rec=temprec+1, err=9010) 
     &              tmpInt1(1)
               tTags(i)%intVal(j) = INT(tmpInt1(1),kind=8)
               
               temprec = temprec+1
               
               ! adjust for unsigned 1-byte integer if < 0
               if (tTags(i)%fldType == 1 .and. tTags(i)%intVal(j) < 0)
     &             tTags(i)%intVal(j) = tTags(i)%intVal(j)+intAdj1
               
            ! single byte (character) - no need to byteswap 1 byte  
            case(2) 
               ! must read 2 bytes per record
               read(unit=iUnit, rec=temprec+1, err=9010) 
     &              tmpChr1(1)
               tTags(i)%chrVal(j) = tmpChr1(1)

               ! replace nul and newline characters               
               if (tTags(i)%chrVal(j) == dNul)
     &             tTags(i)%chrVal(j) = '|'
               
               if (tTags(i)%chrVal(j) == dNewl)
     &             tTags(i)%chrVal(j) = ' '
     
               temprec = temprec+1
            
            ! 2-byte (short) integer
            case(3,8)
               read(unit=iUnit, rec=temprec+1, err=9010) 
     &             tmpInt1(1)
               read(unit=iUnit, rec=temprec+2, err=9010) 
     &             tmpInt1(2)
     
               ! Swap bytes if needed; transfer to final variable
               if (swapbytes) then
                  tmpInt2 = 
     &               transfer((/tmpInt1(2),tmpInt1(1)/),
     &                                     tmpInt2)
                  tTags(i)%intVal(j) = INT(tmpInt2,kind=8)
               else
                  tmpInt2 = 
     &               transfer((/tmpInt1(1),tmpInt1(2)/),
     &                                     tmpInt2)
                  tTags(i)%intVal(j) = INT(tmpInt2,kind=8)
               end if
               
               ! adjust for unsigned 2-byte integer if < 0
               if (tTags(i)%fldType == 3 .and. tTags(i)%intVal(j) < 0)
     &             tTags(i)%intVal(j) = tTags(i)%intVal(j)+intAdj2
                  
               temprec = temprec+2

            ! 4-byte (long) integer
            case(4,9)
               read(unit=iUnit, rec=temprec+1, err=9010) 
     &             tmpInt1(1)
               read(unit=iUnit, rec=temprec+2, err=9010) 
     &             tmpInt1(2)
               read(unit=iUnit, rec=temprec+3, err=9010) 
     &             tmpInt1(3)
               read(unit=iUnit, rec=temprec+4, err=9010) 
     &             tmpInt1(4)
     
               ! Swap bytes if needed; transfer to final variable
               if (swapbytes) then
                  tmpInt4 = 
     &               transfer((/tmpInt1(4),tmpInt1(3),
     &                          tmpInt1(2),tmpInt1(1)/),
     &                          tmpInt4)
                  tTags(i)%intVal(j) = INT(tmpInt4,kind=8)
               else
                  tmpInt4 = 
     &               transfer((/tmpInt1(1),tmpInt1(2),
     &                          tmpInt1(3),tmpInt1(4)/),
     &                          tmpInt4)
                  tTags(i)%intVal(j) = INT(tmpInt4,kind=8)
               end if
               
               ! adjust for unsigned 4-byte integer if < 0
               if (tTags(i)%fldType == 4 .and. tTags(i)%intVal(j) < 0)
     &             tTags(i)%intVal(j) = tTags(i)%intVal(j)+intAdj4
  
               temprec = temprec+4
            
            ! rational   
            case(5,10)
               read(unit=iUnit, rec=temprec+1, err=9010) 
     &              tmpInt1(1)
               read(unit=iUnit, rec=temprec+2, err=9010) 
     &              tmpInt1(2)
               read(unit=iUnit, rec=temprec+3, err=9010) 
     &              tmpInt1(3)
               read(unit=iUnit, rec=temprec+4, err=9010) 
     &              tmpInt1(4)
               read(unit=iUnit, rec=temprec+5, err=9010) 
     &              tmpInt1(5)
               read(unit=iUnit, rec=temprec+6, err=9010) 
     &              tmpInt1(6)
               read(unit=iUnit, rec=temprec+7, err=9010) 
     &              tmpInt1(7)
               read(unit=iUnit, rec=temprec+8, err=9010) 
     &              tmpInt1(8)
     
               ! Swap bytes if needed; transfer to final variable
               if (swapbytes) then
                  tmpInt4 = 
     &               transfer((/tmpInt1(4),tmpInt1(3),
     &                          tmpInt1(2),tmpInt1(1)/),
     &                          tmpInt4)
                  tTags(i)%ratVal(j,1) = INT(tmpInt4,kind=8)
                  
                  tmpInt4 =
     &               transfer((/tmpInt1(8),tmpInt1(7),
     &                          tmpInt1(6),tmpInt1(5)/),
     &                          tmpInt4) 
                  tTags(i)%ratVal(j,2) = INT(tmpInt4,kind=8)
               else
                  tmpInt4 = 
     &               transfer((/tmpInt1(1),tmpInt1(2),
     &                          tmpInt1(3),tmpInt1(4)/),
     &                          tmpInt4)
                  tTags(i)%ratVal(j,1) = INT(tmpInt4,kind=8)
                  
                  tmpInt4 =
     &               transfer((/tmpInt1(5),tmpInt1(6),
     &                          tmpInt1(7),tmpInt1(8)/),
     &                          tmpInt4) 
                  tTags(i)%ratVal(j,2) = INT(tmpInt4,kind=8)
               end if

               ! adjust for unsigned 4-byte integer if < 0
               if (tTags(i)%fldType == 5 .and. tTags(i)%ratVal(j,1) < 0)
     &            tTags(i)%ratVal(j,1) = tTags(i)%ratVal(j,1)+intAdj4
               if (tTags(i)%fldType == 5 .and. tTags(i)%ratVal(j,2) < 0)
     &            tTags(i)%ratVal(j,2) = tTags(i)%ratVal(j,2)+intAdj4
     
               temprec = temprec+8  
            
            ! float             
            case(11)       
               read(unit=iUnit, rec=temprec+1, err=9010) 
     &              tmpInt1(1)
               read(unit=iUnit, rec=temprec+2, err=9010) 
     &              tmpInt1(2)      
               read(unit=iUnit, rec=temprec+3, err=9010) 
     &              tmpInt1(3)
               read(unit=iUnit, rec=temprec+4, err=9010) 
     &              tmpInt1(4)

               ! Swap bytes if needed; transfer to final variable
               if (swapbytes) then
                  tmpInt4 = 
     &               transfer((/tmpInt1(4),tmpInt1(3),
     &                          tmpInt1(2),tmpInt1(1)/),
     &                          tmpInt4)
                  tTags(i)%dblVal(j) = DBLE(tmpInt4)
               else
                  tmpInt4 = 
     &               transfer((/tmpInt1(1),tmpInt1(2),
     &                          tmpInt1(3),tmpInt1(4)/),
     &                          tmpInt4)
                  tTags(i)%dblVal(j) = DBLE(tmpInt4)
               end if
     
               temprec = temprec+4    
            
            ! double
            case(12)
               read(unit=iUnit, rec=temprec+1, err=9010) 
     &              tmpInt1(1)     
               read(unit=iUnit, rec=temprec+2, err=9010) 
     &              tmpInt1(2)             
               read(unit=iUnit, rec=temprec+3, err=9010) 
     &              tmpInt1(3)     
               read(unit=iUnit, rec=temprec+4, err=9010) 
     &              tmpInt1(4)
               read(unit=iUnit, rec=temprec+5, err=9010) 
     &              tmpInt1(5)     
               read(unit=iUnit, rec=temprec+6, err=9010) 
     &              tmpInt1(6)             
               read(unit=iUnit, rec=temprec+7, err=9010) 
     &              tmpInt1(7)     
               read(unit=iUnit, rec=temprec+8, err=9010) 
     &              tmpInt1(8)

               ! Swap bytes if needed; transfer to final variable
               if (swapbytes) then
                  tTags(i)%dblVal(j) = 
     &               transfer((/tmpInt1(8),tmpInt1(7),
     &                          tmpInt1(6),tmpInt1(5),
     &                          tmpInt1(4),tmpInt1(3),
     &                          tmpInt1(2),tmpInt1(1)/),
     &                          tTags(i)%dblVal(j))
               else
                  tTags(i)%dblVal(j) = 
     &               transfer((/tmpInt1(1),tmpInt1(2),
     &                          tmpInt1(3),tmpInt1(4),
     &                          tmpInt1(5),tmpInt1(6),
     &                          tmpInt1(7),tmpInt1(8)/),
     &                          tTags(i)%dblVal(j))
               end if
     
               temprec = temprec+8  
               
            end select
            
C ---       Set geokey flags and offsets to tiff tags where double 
c           and ascii values are stored
            select case(tTags(i)%id)
            ! geokey directory, tiff tag id 34735
            case(34735)
               geoDirNdx = i
               geoDir = .true.
               geoDirRef = tTags(i)%offset
            ! geokey double values, tiff tag id 34736
            case(34736)
               geoDbls = .true.
               geoDblsRef = tTags(i)%offset
            ! geokey ascii values, tiff tag id 34737
            case(34737)
               geoAsc = .true.
               geoAscRef = tTags(i)%offset
            end select

         end do TagValLoop
         
  991    continue         

ccccccccccccccc some debug code to print tiff tags ccccccccccccccccc
         if (L_Debug) then
          write(dbgUnit,*,err=7000)
          write(dbgUnit,10,err=7000) '   Tag ID:', tTags(i)%id
          write(dbgUnit,11,err=7000) 'Data Type:', tTags(i)%fldType,
     &                                    TypeList(tTags(i)%fldType)
          write(dbgUnit,10,err=7000) ' # Values:', tTags(i)%cnt
          write(dbgUnit,10,err=7000) '   Offset:', tTags(i)%offset
          if (tTags(i)%L_TiffProb) then
            write(dbgUnit,*) 'Problem encountered with this Tag!'
          else 
            do j=1,tTags(i)%cnt
              select case(tTags(i)%fldType)
               ! character
               case(2)
                  if (j==1)
     &               write(dbgUnit,*,err=7000)
     &                  '   Value: ',
     &                  (tTags(i)%chrVal(k),k=1,tTags(i)%cnt)
               ! 1-byte integer
               case(1,6)
                  write(dbgUnit,*,err=7000)
     &                            '   Value',j,':',tTags(i)%intVal(j)
               ! 2-byte (short) integer
               case(3,8)
                  write(dbgUnit,*,err=7000)
     &                            '   Value',j,':',tTags(i)%intVal(j)
               ! 4-byte (long) integer
               case(4,9)
                  write(dbgUnit,*,err=7000)
     &                            '   Value',j,':',tTags(i)%intVal(j)
               ! rational
               case(5,10)
                  write(dbgUnit,*,err=7000)
     &                            '   Value',j,':',tTags(i)%ratVal(j,1),
     &                            tTags(i)%ratVal(j,2)
               ! undefined
               case(7)
                  if (j==1)
     &               write(dbgUnit,*,err=7000) '   Value(s) undefined'
               ! float
               case(11)
                 write(dbgUnit,*,err=7000)
     &                            '   Value',j,':',tTags(i)%dblVal(j)
              ! double
              case(12)
                 write(dbgUnit,*,err=7000)
     &                            '   Value',j,':',tTags(i)%dblVal(j)
             end select
            end do
          end if
          write(dbgUnit,*,err=7000)
  10      format(a15,i12)
  11      format(a15,i12,2x,a9)
         end if      
cccccccccccccccccccccccc end TiffTag debug code ccccccccccccccccccccccc

c        Go to the next tag entry record
         thisrec = thisrec+12 
                                      
      end do TagLoop
      
C ---------------- End of IFD Loop for Baseline Tags ------------------


C ------------------ Read GeoKey Directory Header ---------------------
c
c     The GeoKey directory is stored in the TIFF tag id 34735 - an
c     array of unsigned short integers (2 bytes) grouped in blocks
c     of four.  The first 4 values contain the GeoKey directory
c     header information which includes the version, revision, minor
c     revision, and number of geokeys. 
C --------------------------------------------------------------------- 

C --- If geoKey directory tag is not present, print 
C     error message and return
      if (.not. geoDir) then
         write(*,6005) iFile(1:LEN_TRIM(iFile))
         write(oUnit,6005,err=8000) iFile(1:LEN_TRIM(iFile))
 6005    format(
     &     /,' Georeferencing information could not be found',
     &       ' in the file: ',
     &     /,1x,a,
     &     /,' The file may not be a valid GeoTIFF file.')
         tiffErr = .true.
         if (L_Debug) then
            write(dbgUnit,6005,err=8000) iFile(1:LEN_TRIM(iFile))
         end if
         return
      end if

      ! re-initialize allocation error flag
      allErr =  .false.
      
C --- Read geokey directory header and allocate geokey array

      ! set record number
      thisrec = geoDirRef/initrecl 
      
      ! TIFF version  - read but not used
      read(unit=iUnit, rec=thisrec+1, err=9010) 
     &     tmpInt1(1)     
      read(unit=iUnit, rec=thisrec+2, err=9010) 
     &     tmpInt1(2)     
     
      ! revision - read but not used
      read(unit=iUnit, rec=thisrec+3, err=9010) 
     &     tmpInt1(3)     
      read(unit=iUnit, rec=thisrec+4, err=9010) 
     &     tmpInt1(4)     
     
      ! minor revision - read but not used
      read(unit=iUnit, rec=thisrec+5, err=9010) 
     &     tmpInt1(5)     
      read(unit=iUnit, rec=thisrec+6, err=9010) 
     &     tmpInt1(6)     
     
      ! number of GeoKeys 
      read(unit=iUnit, rec=thisrec+7, err=9010) 
     &     tmpInt1(7)        
      read(unit=iUnit, rec=thisrec+8, err=9010) 
     &     tmpInt1(8)                  

      ! Swap bytes if needed; transfer # geokeys to final variable
      if (swapbytes) then
         tmpInt2 = 
     &      transfer((/tmpInt1(8),tmpInt1(7)/),tmpInt2)
         numGeoKeys = tmpInt2
      else 
         tmpInt2 = 
     &      transfer((/tmpInt1(7),tmpInt1(8)/),tmpInt2)
         numGeoKeys = tmpInt2
      end if

      ! apply adjustment for unsigned 2 byte int if needed
      if (numGeoKeys<0) then
         numGeoKeys = numGeoKeys + intAdj2
      endif

      ! allocate geokey array
      if (allocated(gKeys)) deallocate(gKeys)
      allocate(gKeys(numGeoKeys),stat=als)
      
      ! Evaluate allocation error flag; if true write error and return
      if (als > 0) then
         write(*,130) iFile(1:LEN_TRIM(iFile))
         write(oUnit,130,err=8000) iFile(1:LEN_TRIM(iFile))
         allErr = .true.
         return
      end if

C ------- Loop Over GeoKey Directory to Retrieve GeoKey Values --------
c
c     The header is followed by the key entries each of which include 
c     the id, tag location, count, and value/offset. 
c
c     Tag Location: Specifies which tiff tag contains the value - if 0, 
c     then the value/offset position contains the value and is a 
c     short integer.
c
c     Count: Number of values for the geokey.
c
c     Value/Offset: 2 byte value if tag location is 0; else the index
c     or array element in the values array for the tag location
c     specified.
C ---------------------------------------------------------------------
  
      ! set record
      thisrec = thisrec+8

ccccc Write header for optional debug file to print Geo Keys ccccccccccc
      if (L_Debug) then
        write(dbgUnit,*,err=7000)
        write(dbgUnit,*,err=7000)  '**** GeoKeys:'
      end if
cccccccccccccccccccccccc end GeoKey debug code ccccccccccccccccccccccccc

C --- Loop over directory, read geokeys, get values
      do i=1,size(gKeys) 

C ---    re-initialize allocation error flag
         allErr =  .false.
         
C ---    Initialize control flags to false
         readAsVal = .false.

         ! Initialize logical flag for problem to false
         gKeys(i)%L_TiffProb = .false.

C ---    Read Directory
                  
         ! geokey id
         read(unit=iUnit, rec=thisrec+1, err=9010) 
     &        tmpInt1(1)  
         read(unit=iUnit, rec=thisrec+2, err=9010) 
     &        tmpInt1(2)  
        
         ! tag location
         read(unit=iUnit, rec=thisrec+3, err=9010) 
     &        tmpInt1(3)  
         read(unit=iUnit, rec=thisrec+4, err=9010) 
     &        tmpInt1(4)  
        
         ! count
         read(unit=iUnit, rec=thisrec+5, err=9010) 
     &        tmpInt1(5)
         read(unit=iUnit, rec=thisrec+6, err=9010) 
     &        tmpInt1(6)
  
C ---    Swap bytes if needed; transfer to final variable
         if (swapbytes) then
            tmpInt2 = 
     &         transfer((/tmpInt1(2),tmpInt1(1)/),tmpInt2)
            gKeys(i)%id = INT(tmpInt2,kind=8)
            
            tmpInt2 = 
     &         transfer((/tmpInt1(4),tmpInt1(3)/),tmpInt2)
            gKeys(i)%fldType = INT(tmpInt2,kind=8)
     
            tmpInt2 = 
     &         transfer((/tmpInt1(6),tmpInt1(5)/),tmpInt2)
            gKeys(i)%cnt = INT(tmpInt2,kind=8)
         else
            tmpInt2 = 
     &         transfer((/tmpInt1(1),tmpInt1(2)/),tmpInt2)
            gKeys(i)%id = INT(tmpInt2,kind=8)
            
            tmpInt2 = 
     &         transfer((/tmpInt1(3),tmpInt1(4)/),tmpInt2)
            gKeys(i)%fldType = INT(tmpInt2,kind=8)
     
            tmpInt2 = 
     &         transfer((/tmpInt1(5),tmpInt1(6)/),tmpInt2)
            gKeys(i)%cnt = INT(tmpInt2,kind=8)
         end if

C ---    Apply unsigned integer adjustment if < 0
         if(gKeys(i)%id<0) 
     &      gKeys(i)%id = gKeys(i)%id+intAdj2         
         if(gKeys(i)%fldType<0) 
     &      gKeys(i)%fldType = gKeys(i)%fldType+intAdj2         
         if(gKeys(i)%cnt<0) 
     &      gKeys(i)%cnt = gKeys(i)%cnt+intAdj2
         
C ---    Allocate arrays within geokey array
         select case(gKeys(i)%fldType)
         ! short integer
         case(0)
            if (allocated(gKeys(i)%intVal))
     &         deallocate(gKeys(i)%intVal)
            allocate(gKeys(i)%intVal(gKeys(i)%cnt),stat=als)
            if (als > 0) allErr = .true.
         ! double
         case(34736)
            if (allocated(gKeys(i)%dblVal))
     &         deallocate(gKeys(i)%dblVal)
            allocate(gKeys(i)%dblVal(gKeys(i)%cnt),stat=als)
            if (als > 0) allErr = .true.
         ! character
         case(34737)
            if (allocated(gKeys(i)%chrVal))
     &         deallocate(gKeys(i)%chrVal)
            allocate(gKeys(i)%chrVal(gKeys(i)%cnt),stat=als)
            if (als > 0) allErr = .true.  
         ! Undefined data type
         case default
C ---       Issue warning message and set logical flag indicating problem
C           with this tag         
            write(*,225) iFile(1:LEN_TRIM(iFile)), gKeys(i)%id
            write(oUnit,125,err=8000) iFile(1:LEN_TRIM(iFile)), 
     &                                gKeys(i)%id
  225       format(/,' The file: ',a,
     &             /,' contains an undefined data type for GeoKey ID: ',
     &             i8)     
            tiffWrn = .true.
            gKeys(i)%L_TiffProb = .true.
C ---       Skip to the next tag, but include debug output if requested
            go to 992
         end select    
      
C ---    Evaluate allocation error flag; if true write error and return
         if (allErr) then
            write(*,130) iFile(1:LEN_TRIM(iFile))
            write(oUnit,130,err=8000) iFile(1:LEN_TRIM(iFile))
            return
         end if
         
C ---    Set readAsVal to true if tag location = 0; 
c        Implies count = 1 and data type is a short integer
c        according to the GeoTIFF specs
         if (gKeys(i)%fldType == 0) readAsVal = .true.
         
C ---    Read geokey values
         if (readAsVal) then
            ! set record
            temprec = thisrec+6
         
         else
         
            ! read offset - array element of first value
            read(unit=iUnit, rec=thisrec+7, err=9010) 
     &           tmpInt1(1)
            read(unit=iUnit, rec=thisrec+8, err=9010) 
     &           tmpInt1(2)

            ! Swap bytes if needed; transfer to final variable
            if (swapbytes) then
C ---          Use temporary 2-byte integer since %offset field is 4-byte
               tmpInt2 = 
     &            transfer((/tmpInt1(2),tmpInt1(1)/),tmpInt2)
               gKeys(i)%offset = INT(tmpInt2,kind=8)
            else
C ---          Use temporary 2-byte integer since %offset field is 4-byte
               tmpInt2 = 
     &            transfer((/tmpInt1(1),tmpInt1(2)/),tmpInt2)
               gKeys(i)%offset = INT(tmpInt2,kind=8)
            end if
            
            ! adjust for unsigned integer if < 0
            if (gKeys(i)%offset < 0)
     &          gKeys(i)%offset = gKeys(i)%offset+intAdj2
            
            ! set temp record based on fldType reference set when tiff tags were read
            select case(gKeys(i)%fldType)
            ! double
            case(34736)
               ! set error and return if doubles geokey was not found
               if (geoDbls) then
                  temprec = (geoDblsRef/initrecl)+
     &                  ((gKeys(i)%offset*8)/initrecl)
               else
                  write(*,620) gKeys(i)%id, iFile(1:LEN_TRIM(iFile))
                  write(oUnit,620,err=8000) gKeys(i)%id, 
     &                                      iFile(1:LEN_TRIM(iFile))
                  tiffErr = .true.
                  return
               end if
 620  format(
     &  /,' Georeferencing information could not be found for'
     &    ' GeoKey ',i5,'.',
     &  /,' The file may not be a valid GeoTIFF.',
     &  /,' File: ',a)
            ! character
            case(34737)
               ! set error and return if doubles geokey was not found
               if (geoAsc) then
                  temprec = (geoAscRef/initrecl)+
     &                  ((gKeys(i)%offset)/initrecl)
               else
                  write(*,620) gKeys(i)%id, iFile(1:LEN_TRIM(iFile))
                  write(oUnit,620,err=8000) gKeys(i)%id, 
     &                                      iFile(1:LEN_TRIM(iFile))
                  tiffErr = .true.
                  return
               end if
               
            end select    
         
         end if
         
C ---    Read value(s)
         do j=1, gKeys(i)%cnt
         
            select case(gKeys(i)%fldType)
         
            case(0)
            ! read entry value/offset as value - short integer
            ! should only be one
               read(unit=iUnit, rec=temprec+1, err=9010) 
     &            tmpInt1(1)
               read(unit=iUnit, rec=temprec+2, err=9010) 
     &            tmpInt1(2)

               ! Swap bytes if needed; transfer to final variable
               if (swapbytes) then
                  tmpInt2 = 
     &               transfer((/tmpInt1(2),tmpInt1(1)/),
     &                                     tmpInt2)
                  gKeys(i)%intVal(j) = INT(tmpInt2,kind=8)
               else
                  tmpInt2 = 
     &               transfer((/tmpInt1(1),tmpInt1(2)/),
     &                                     tmpInt2)
                  gKeys(i)%intVal(j) = INT(tmpInt2,kind=8)
               end if
                           
               ! adjust for unsigned integer if < 0
               if (gKeys(i)%intVal(j) < 0)
     &             gKeys(i)%intVal(j) = gKeys(i)%intVal(j)+intAdj2   
     
               temprec = temprec+2
               
               if (gKeys(i)%cnt /= 1) then
               ! Write warning message, should be only 1 value
                  write(*,630) gKeys(i)%id, iFile(1:LEN_TRIM(iFile))
                  write(oUnit,630,err=8000) gKeys(i)%id, 
     &                                      iFile(1:LEN_TRIM(iFile))
                  tiffWrn = .true.
               end if
 630  format(
     &  /,' More than one value for GeoKey ',i6,
     &    ' ; expecting only 1 value.'
     &  /,' The file may not be a valid GeoTIFF.',
     &  /,' File: ',a)
     
               ! skip to end of loop on GeoKey fields
               go to 992
         
            ! double
            case(34736)  
            
               read(unit=iUnit, rec=temprec+1, err=9010) 
     &              tmpInt1(1)     
               read(unit=iUnit, rec=temprec+2, err=9010) 
     &              tmpInt1(2)             
               read(unit=iUnit, rec=temprec+3, err=9010) 
     &              tmpInt1(3)     
               read(unit=iUnit, rec=temprec+4, err=9010) 
     &              tmpInt1(4)            
               read(unit=iUnit, rec=temprec+5, err=9010) 
     &              tmpInt1(5)     
               read(unit=iUnit, rec=temprec+6, err=9010) 
     &              tmpInt1(6)             
               read(unit=iUnit, rec=temprec+7, err=9010) 
     &              tmpInt1(7)     
               read(unit=iUnit, rec=temprec+8, err=9010) 
     &              tmpInt1(8)

               ! Swap bytes if needed; transfer to final variable
               if (swapbytes) then
                  gKeys(i)%dblVal(j) = 
     &               transfer((/tmpInt1(8),tmpInt1(7),
     &                          tmpInt1(6),tmpInt1(5),
     &                          tmpInt1(4),tmpInt1(3),
     &                          tmpInt1(2),tmpInt1(1)/),
     &                          gKeys(i)%dblVal(j))
               else
                  gKeys(i)%dblVal(j) = 
     &               transfer((/tmpInt1(1),tmpInt1(2),
     &                          tmpInt1(3),tmpInt1(4),
     &                          tmpInt1(5),tmpInt1(6),
     &                          tmpInt1(7),tmpInt1(8)/),
     &                          gKeys(i)%dblVal(j))
               end if
         
               temprec = temprec+8
         
            ! character (single byte)
            case(34737) 
            
               ! must read 1 bytes per record - no need to swap bytes
               read(unit=iUnit, rec=temprec+1, err=9010) 
     &              tmpChr1(1)
               gKeys(i)%chrVal(j) = tmpChr1(1)
                              
               ! replace nul and newline characters
               if (gKeys(i)%chrVal(j) == dNul)
     &             gKeys(i)%chrVal(j) = '|'
               
               if (gKeys(i)%chrVal(j) == dNewl)
     &             gKeys(i)%chrVal(j) = ' '
         
               temprec = temprec+1
         
            end select 
            
         end do

 992     continue
 
ccccccccccccccc some debug code to print GeoKeys cccccccccccccccccccccc 
         if (L_Debug) then
          write(dbgUnit,*,err=7000)
          write(dbgUnit,20,err=7000) 'GeoKey ID:', gKeys(i)%id
          write(dbgUnit,20,err=7000) 'Data Type:', gKeys(i)%fldType
          write(dbgUnit,20,err=7000) ' # Values:', gKeys(i)%cnt
          write(dbgUnit,20,err=7000) '   Offset:', gKeys(i)%offset
          do j=1,gKeys(i)%cnt
             select case(gKeys(i)%fldType)
               ! character
               case(34737)
                  if (j==1)
     &               write(dbgUnit,*,err=7000)
     &                '   Value: ',
     &                (gKeys(i)%chrVal(k),k=1,gKeys(i)%cnt)
               ! integer
               case(0)
                  write(dbgUnit,*,err=7000) 
     &                            '   Value',j,':',gKeys(i)%intVal(j)
              ! double
               case(34736)
                 write(dbgUnit,*,err=7000)
     &                            '   Value',j,':',gKeys(i)%dblVal(j)
             end select
          end do
          write(dbgUnit,*,err=7000)
  20      format(a15,i12)
         end if
         
cccccccccccccccccccccccc end GeoKey debug code cccccccccccccccccccccccc        

         
         ! advance record for next geokey entry
         thisrec = thisrec+8

      end do
      
C --------------- End of Loop Over GeoKey Directory -------------------

      
      iod = .false.
      inquire(unit=iUnit, opened=iod)
      if (iod) close(iUnit)
      
      if (L_Debug) then  
         iod = .false.
         inquire(unit=dbgUnit, opened=iod)
         if (iod) close(dbgUnit)
      end if
         
      return

c     problem writing to TiffDebug file      
 7000 continue  
      write(*,7005) dbgFile(1:LEN_TRIM(dbgFile))
      write(oUnit,7005) dbgFile(1:LEN_TRIM(dbgFile))
 7005 format(/,' There was a problem writing to the file:'
     &    /,' ',a)
C --- Call to ERRHDL routine for use in AERMAP; may need to comment out
C     for use in other processors.
      CALL ERRHDL('RN','TIFFTAGS','E','520',' TIFFDBG')
C ---     
      tiffErr = .true.
      iod = .false.
      inquire(unit=dbgUnit, opened=iod)
      if (iod) close(dbgUnit)
      return

c     problem writing to MAPDETAIL.OUT file      
 8000 continue  
      write(*,8005) oFile(1:LEN_TRIM(oFile))
      write(oUnit,8005) oFile(1:LEN_TRIM(oFile))
 8005 format(/,' There was a problem writing to the file:'
     &    /,' ',a)
C --- Call to ERRHDL routine for use in AERMAP; may need to comment out
C     for use in other processors.
      CALL ERRHDL('RN','TIFFTAGS','E','520','  MAPDET')  
C ---     
      tiffErr = .true.
      iod = .false.
      inquire(unit=iUnit, opened=iod)
      if (iod) close(iUnit)
      return

c     problem opening TIFF file      
 9000 continue  
      write(*,9005) iFile(1:LEN_TRIM(iFile))
      write(oUnit,9005) iFile(1:LEN_TRIM(iFile))
 9005 format(/,' There was a problem opening the file:'
     &    /,' ',a)
C --- Call to ERRHDL routine for use in AERMAP; may need to comment out
C     for use in other processors.
      CALL ERRHDL('RN','TIFFTAGS','E','500','TIF File')  
C ---     
      tiffErr = .true.
      iod = .false.
      inquire(unit=iUnit, opened=iod)
      if (iod) close(iUnit)
      return
 
c     problem reading TIFF file   
 9010 continue         
      write(*,9015) iFile(1:LEN_TRIM(iFile))
      write(oUnit,9015) iFile(1:LEN_TRIM(iFile))
 9015 format(/,' There was a problem reading the file:'
     &    /,' ',a)
C --- Call to ERRHDL routine for use in AERMAP; may need to comment out
C     for use in other processors.
      CALL ERRHDL('RN','TIFFTAGS','E','510','TIF File')  
C ---     
      tiffErr = .true.
      iod = .false.
      inquire(unit=iUnit, opened=iod)
      if (iod) close(iUnit)
      return

c     problem opening optional Debug file      
 9100 continue  
      write(*,9005) dbgFile(1:LEN_TRIM(dbgFile))
      write(oUnit,9005) dbgFile(1:LEN_TRIM(dbgFile))
 9105 format(/,' There was a problem opening the file:'
     &    /,' ',a)
C --- Call to ERRHDL routine for use in AERMAP; may need to comment out
C     for use in other processors.
      CALL ERRHDL('RN','TIFFTAGS','E','500','TIFDEBUG')  
C ---     
      tiffErr = .true.
      iod = .false.
      inquire(unit=iUnit, opened=iod)
      if (iod) close(iUnit)
      return
 
      end subroutine read_tifftags
      
C ----------------------------------------------------------------------
      
      subroutine cpuEnd(endn)
      
      implicit none

      integer (kind=2) :: i2
      character*1      :: endn

      
      i2 = 1

      if (btest(i2,0)) then
         endn = 'L'
      elseif (btest(i2,8)) then
         endn = 'B'
      else
         endn = 'X'
      end if
      
      return
      
      end subroutine cpuEnd
      
