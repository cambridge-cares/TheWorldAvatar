      MODULE TiffTags  
 
      implicit none

C --- The "tag" derived types are used to validate the existence of certain tags, 
c     and store the value extracted for those tags when possible.  This format does
c     not accommodate tags that contain multiple values.  They will be stored
c     independently in separate variables.
C --- Declare all non-character fields/arrays as 8 bytes to allow maximum flexibility.

      type tags
         integer (kind=8)  :: id         ! tag id (native TIFF tag id's are 2-bytes unsigned
         integer (kind=8)  :: fldType    ! data type (tiff tags = 1:12; geokeys = 0, 34736, or 34737)
         integer (kind=8)  :: cnt        ! number of values
         integer (kind=8)  :: offset     ! offset to value if applicable 
                                         ! (tiff = bytes, geokey = array element)
         logical           :: L_TiffProb ! logical flag indicating problem with tag
         character(len=1), allocatable  :: chrVal(:)   ! character value
         integer (kind=8), allocatable  :: intVal(:)   ! integer value or code
         double precision, allocatable  :: dblVal(:)   ! real/double value
         integer (kind=8), allocatable  :: ratVal(:,:) ! rational (fraction) values

      end type tags


      type(tags), allocatable  :: tTags(:)   ! tiff tags found in GeoTIFF
      type(tags), allocatable  :: gKeys(:)   ! geokeys found in GeoTIFF
     
      integer (kind=8), allocatable :: SampleFormat(:)  ! code for format of elevation data:
                                                        ! 1 = unsigned integer; 2 = signed integer;
                                                        ! 3 = floating point
      logical  :: swapbytes      ! byteswap flag
      
      logical  :: tiffErr        ! error encountered reading tiff tags or geokeys
      logical  :: tiffWrn        ! warning encountered reading tiff tags or geokeys
      logical  :: allerr         ! allocation error
      
C --- Adjustment for unsigned integers when value is too large for target arrays
c     and is returned as negative; add this value to adjust to the real int value.
C --- Fortran does not distinguish between signed and unsigned integer data types.
      integer (kind=8), parameter :: intAdj4 = 4294967296_8  ! adjustment for 4-byte unsigned integers
                                                             ! (note: '_8' specifies value as 8-bytes)      
      integer (kind=4), parameter :: intAdj2 = 65536         ! adjustment for 2-byte unsigned integers
      integer (kind=2), parameter :: intAdj1 = 256           ! adjustment for 1-byte unsigned integers
      
      END MODULE TiffTags
