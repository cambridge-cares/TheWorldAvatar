!
! pnetcdf fortran defines
!

!
! PnetCDF library version numbers
!
      integer PNETCDF_VERSION_MAJOR
      integer PNETCDF_VERSION_MINOR
      integer PNETCDF_VERSION_SUB

      parameter (PNETCDF_VERSION_MAJOR = 1)
      parameter (PNETCDF_VERSION_MINOR = 6)
      parameter (PNETCDF_VERSION_SUB   = 1)

!
! external netcdf data types: (must conform with netCDF release)
!
      integer nf_byte
      integer nf_int1
      integer nf_char
      integer nf_short
      integer nf_int2
      integer nf_int
      integer nf_float
      integer nf_real
      integer nf_double
      integer nf_ubyte
      integer nf_ushort
      integer nf_uint
      integer nf_int64
      integer nf_uint64

      parameter (nf_byte = 1)
      parameter (nf_int1 = nf_byte)
      parameter (nf_char = 2)
      parameter (nf_short = 3)
      parameter (nf_int2 = nf_short)
      parameter (nf_int = 4)
      parameter (nf_float = 5)
      parameter (nf_real = nf_float)
      parameter (nf_double = 6)
      parameter (nf_ubyte = 7)
      parameter (nf_ushort = 8)
      parameter (nf_uint = 9)
      parameter (nf_int64 = 10)
      parameter (nf_uint64 = 11)

!
! default fill values:
!
      integer           nf_fill_byte
      integer           nf_fill_int1
      integer           nf_fill_char
      integer           nf_fill_short
      integer           nf_fill_int2
      integer           nf_fill_int
      real              nf_fill_float
      real              nf_fill_real
      doubleprecision   nf_fill_double
      integer           nf_fill_ubyte
      integer           nf_fill_ushort
      integer*8         nf_fill_uint
      integer*8         nf_fill_int64
      ! integer*8         nf_fill_uint64    ! no unsigned int*8 in Fortran
      doubleprecision   nf_fill_uint64

      parameter (nf_fill_byte = -127)
      parameter (nf_fill_int1 = nf_fill_byte)
      parameter (nf_fill_char = 0)
      parameter (nf_fill_short = -32767)
      parameter (nf_fill_int2 = nf_fill_short)
      parameter (nf_fill_int = -2147483647)
      parameter (nf_fill_float = 9.9692099683868690e+36)
      parameter (nf_fill_real = nf_fill_float)
      parameter (nf_fill_double = 9.9692099683868690e+36)
      parameter (nf_fill_ubyte = 255)
      parameter (nf_fill_ushort = 65535)
      parameter (nf_fill_uint = 4294967295_8)
      parameter (nf_fill_int64 = -9223372036854775806_8)
      ! parameter (nf_fill_uint64 = 18446744073709551614_8)  ! currently not supported
      parameter (nf_fill_uint64 = 1.8446744073709551614e+19)

!
! mode flags for opening and creating a netcdf dataset:
!
      integer nf_nowrite
      integer nf_write
      integer nf_clobber
      integer nf_noclobber
      integer nf_fill
      integer nf_nofill
      integer nf_lock
      integer nf_share
      integer nf_64bit_offset
      integer nf_32bit
      integer nf_64bit_data
      integer nf_sizehint_default
      integer nf_align_chunk
      integer nf_format_classic
      integer nf_format_64bit
      integer nf_format_64bit_data
      integer nf_format_cdf2
      integer nf_format_cdf5

      parameter (nf_nowrite = 0)
      parameter (nf_write = 1)
      parameter (nf_clobber = 0)
      parameter (nf_noclobber = 4)
      parameter (nf_fill = 0)
      parameter (nf_nofill = 256)
      parameter (nf_lock = 1024)
      parameter (nf_share = 2048)
      parameter (nf_64bit_offset = 512)
      parameter (nf_64bit_data = 32)
      parameter (nf_32bit = 16777216)
      parameter (nf_sizehint_default = 0)
      parameter (nf_align_chunk = -1)
      parameter (nf_format_classic = 1)
      parameter (nf_format_cdf2 = 2)
      parameter (nf_format_cdf5 = 5)
      parameter (nf_format_64bit = nf_format_cdf2)
      parameter (nf_format_64bit_data = nf_format_cdf5)

!
! size argument for defining an unlimited dimension:
!
      integer nf_unlimited
      parameter (nf_unlimited = 0)

      integer*8 nfmpi_unlimited
      parameter (nfmpi_unlimited = 0)

!
! global attribute id:
!
      integer nf_global
      parameter (nf_global = 0)

!
! implementation limits:
!
      integer nf_max_dims
      integer nf_max_attrs
      integer nf_max_vars
      integer nf_max_name
      integer nf_max_var_dims

      parameter (nf_max_dims = 512)
      parameter (nf_max_attrs = 4092)
      parameter (nf_max_vars = 4096)
      parameter (nf_max_name = 128)
      parameter (nf_max_var_dims = nf_max_dims)

!
! error codes: (conform with netCDF release)
!
      integer NF_NOERR
      integer NF2_ERR
      integer NF_EBADID
      integer NF_ENFILE
      integer NF_EEXIST
      integer NF_EINVAL
      integer NF_EPERM
      integer NF_ENOTINDEFINE
      integer NF_EINDEFINE
      integer NF_EINVALCOORDS
      integer NF_EMAXDIMS
      integer NF_ENAMEINUSE
      integer NF_ENOTATT
      integer NF_EMAXATTS
      integer NF_EBADTYPE
      integer NF_EBADDIM
      integer NF_EUNLIMPOS
      integer NF_EMAXVARS
      integer NF_ENOTVAR
      integer NF_EGLOBAL
      integer NF_ENOTNC
      integer NF_ESTS
      integer NF_EMAXNAME
      integer NF_EUNLIMIT
      integer NF_ENORECVARS
      integer NF_ECHAR
      integer NF_EEDGE
      integer NF_ESTRIDE
      integer NF_EBADNAME
      integer NF_ERANGE
      integer NF_ENOMEM
      integer NF_EVARSIZE
      integer NF_EDIMSIZE
      integer NF_ETRUNC
      integer NF_EAXISTYPE
      integer NF_EDAP
      integer NF_ECURL
      integer NF_EIO
      integer NF_ENODATA
      integer NF_EDAPSVC
      integer NF_EDAS
      integer NF_EDDS
      integer NF_EDATADDS
      integer NF_EDAPURL
      integer NF_EDAPCONSTRAINT
      integer NF_ETRANSLATION
      integer NF_EACCESS
      integer NF_EAUTH
      integer NF_ENOTFOUND
      integer NF_ECANTREMOVE

      PARAMETER (NF_NOERR        = 0)     ! No Error
      PARAMETER (NF2_ERR         = -1)    ! Returned for all errors in the v2 API
      PARAMETER (NF_EBADID       = -33)   ! Not a netcdf id
      PARAMETER (NF_ENFILE       = -34)   ! Too many netcdfs open
      PARAMETER (NF_EEXIST       = -35)   ! netcdf file exists and NF_NOCLOBBER
      PARAMETER (NF_EINVAL       = -36)   ! Invalid Argument
      PARAMETER (NF_EPERM        = -37)   ! Write to read only
      PARAMETER (NF_ENOTINDEFINE = -38)   ! Operation not allowed in data mode
      PARAMETER (NF_EINDEFINE    = -39)   ! Operation not allowed in define mode
      PARAMETER (NF_EINVALCOORDS = -40)   ! Index exceeds dimension bound
      PARAMETER (NF_EMAXDIMS     = -41)   ! NF_MAX_DIMS exceeded
      PARAMETER (NF_ENAMEINUSE   = -42)   ! String match to name in use
      PARAMETER (NF_ENOTATT      = -43)   ! Attribute not found
      PARAMETER (NF_EMAXATTS     = -44)   ! NF_MAX_ATTRS exceeded
      PARAMETER (NF_EBADTYPE     = -45)   ! Not a netcdf data type
      PARAMETER (NF_EBADDIM      = -46)   ! Invalid dimension id or name
      PARAMETER (NF_EUNLIMPOS    = -47)   ! NFMPI_UNLIMITED in the wrong index
      PARAMETER (NF_EMAXVARS     = -48)   ! NF_MAX_VARS exceeded
      PARAMETER (NF_ENOTVAR      = -49)   ! Variable not found
      PARAMETER (NF_EGLOBAL      = -50)   ! Action prohibited on NF_GLOBAL varid
      PARAMETER (NF_ENOTNC       = -51)   ! Not a netcdf file
      PARAMETER (NF_ESTS         = -52)   ! In Fortran, string too short
      PARAMETER (NF_EMAXNAME     = -53)   ! NF_MAX_NAME exceeded
      PARAMETER (NF_EUNLIMIT     = -54)   ! NFMPI_UNLIMITED size already in use
      PARAMETER (NF_ENORECVARS   = -55)   ! nc_rec op when there are no record vars
      PARAMETER (NF_ECHAR        = -56)   ! Attempt to convert between text & numbers
      PARAMETER (NF_EEDGE        = -57)   ! Edge+start exceeds dimension bound
      PARAMETER (NF_ESTRIDE      = -58)   ! Illegal stride
      PARAMETER (NF_EBADNAME     = -59)   ! Attribute or variable name contains illegal characters
      PARAMETER (NF_ERANGE       = -60)   ! Math result not representable
      PARAMETER (NF_ENOMEM       = -61)   ! Memory allocation (malloc) failure
      PARAMETER (NF_EVARSIZE     = -62)   ! One or more variable sizes violate format constraints
      PARAMETER (NF_EDIMSIZE     = -63)   ! Invalid dimension size
      PARAMETER (NF_ETRUNC       = -64)   ! File likely truncated or possibly corrupted
      PARAMETER (NF_EAXISTYPE    = -65)   ! Unknown axis type

! Following errors are added for DAP
      PARAMETER (NF_EDAP         = -66)   ! Generic DAP error
      PARAMETER (NF_ECURL        = -67)   ! Generic libcurl error
      PARAMETER (NF_EIO          = -68)   ! Generic IO error
      PARAMETER (NF_ENODATA      = -69)   ! Attempt to access variable with no data
      PARAMETER (NF_EDAPSVC      = -70)   ! DAP server error
      PARAMETER (NF_EDAS         = -71)   ! Malformed or inaccessible DAS
      PARAMETER (NF_EDDS         = -72)   ! Malformed or inaccessible DDS
      PARAMETER (NF_EDATADDS     = -73)   ! Malformed or inaccessible DATADDS
      PARAMETER (NF_EDAPURL      = -74)   ! Malformed DAP URL
      PARAMETER (NF_EDAPCONSTRAINT = -75) ! Malformed DAP Constraint
      PARAMETER (NF_ETRANSLATION = -76)   ! Untranslatable construct
      PARAMETER (NF_EACCESS      = -77)   ! Access Failure
      PARAMETER (NF_EAUTH        = -78)   ! Authorization Failure

! Misc. additional errors
      PARAMETER (NF_ENOTFOUND    = -90)   ! No such file
      PARAMETER (NF_ECANTREMOVE  = -91)   ! Can't remove file

!
! netCDF-4 error codes (copied from netCDF release)
!
      integer NF_EHDFERR
      integer NF_ECANTREAD
      integer NF_ECANTWRITE
      integer NF_ECANTCREATE
      integer NF_EFILEMETA
      integer NF_EDIMMETA
      integer NF_EATTMETA
      integer NF_EVARMETA
      integer NF_ENOCOMPOUND
      integer NF_EATTEXISTS
      integer NF_ENOTNC4
      integer NF_ESTRICTNC3
      integer NF_ENOTNC3
      integer NF_ENOPAR
      integer NF_EPARINIT
      integer NF_EBADGRPID
      integer NF_EBADTYPID
      integer NF_ETYPDEFINED
      integer NF_EBADFIELD
      integer NF_EBADCLASS
      integer NF_EMAPTYPE
      integer NF_ELATEFILL
      integer NF_ELATEDEF
      integer NF_EDIMSCALE
      integer NF_ENOGRP
      integer NF_ESTORAGE
      integer NF_EBADCHUNK
      integer NF_ENOTBUILT
      integer NF_EDISKLESS
      integer NF_ECANTEXTEND
      integer NF_EMPI

      PARAMETER (NF_EHDFERR      = -101)  ! Error at HDF5 layer. 
      PARAMETER (NF_ECANTREAD    = -102)  ! Can't read. 
      PARAMETER (NF_ECANTWRITE   = -103)  ! Can't write. 
      PARAMETER (NF_ECANTCREATE  = -104)  ! Can't create. 
      PARAMETER (NF_EFILEMETA    = -105)  ! Problem with file metadata. 
      PARAMETER (NF_EDIMMETA     = -106)  ! Problem with dimension metadata. 
      PARAMETER (NF_EATTMETA     = -107)  ! Problem with attribute metadata. 
      PARAMETER (NF_EVARMETA     = -108)  ! Problem with variable metadata. 
      PARAMETER (NF_ENOCOMPOUND  = -109)  ! Not a compound type. 
      PARAMETER (NF_EATTEXISTS   = -110)  ! Attribute already exists. 
      PARAMETER (NF_ENOTNC4      = -111)  ! Attempting netcdf-4 operation on netcdf-3 file.   
      PARAMETER (NF_ESTRICTNC3   = -112)  ! Attempting netcdf-4 operation on strict nc3 netcdf-4 file.   
      PARAMETER (NF_ENOTNC3      = -113)  ! Attempting netcdf-3 operation on netcdf-4 file.   
      PARAMETER (NF_ENOPAR       = -114)  ! Parallel operation on file opened for non-parallel access.   
      PARAMETER (NF_EPARINIT     = -115)  ! Error initializing for parallel access.   
      PARAMETER (NF_EBADGRPID    = -116)  ! Bad group ID.   
      PARAMETER (NF_EBADTYPID    = -117)  ! Bad type ID.   
      PARAMETER (NF_ETYPDEFINED  = -118)  ! Type has already been defined and may not be edited. 
      PARAMETER (NF_EBADFIELD    = -119)  ! Bad field ID.   
      PARAMETER (NF_EBADCLASS    = -120)  ! Bad class.   
      PARAMETER (NF_EMAPTYPE     = -121)  ! Mapped access for atomic types only.   
      PARAMETER (NF_ELATEFILL    = -122)  ! Attempt to define fill value when data already exists. 
      PARAMETER (NF_ELATEDEF     = -123)  ! Attempt to define var properties, like deflate, after enddef.
      PARAMETER (NF_EDIMSCALE    = -124)  ! Probem with HDF5 dimscales.
      PARAMETER (NF_ENOGRP       = -125)  ! No group found.
      PARAMETER (NF_ESTORAGE     = -126)  ! Can't specify both contiguous and chunking.
      PARAMETER (NF_EBADCHUNK    = -127)  ! Bad chunksize.
      PARAMETER (NF_ENOTBUILT    = -128)  ! Attempt to use feature that was not turned on when netCDF was built.
      PARAMETER (NF_EDISKLESS    = -129)  ! Error in using diskless  access.
      PARAMETER (NF_ECANTEXTEND  = -130)  ! Attempt to extend dataset during ind. I/O operation.
      PARAMETER (NF_EMPI         = -131)  ! MPI operation failed.

!
! PnetCDF error codes start here
!
      integer NF_ESMALL
      integer NF_ENOTINDEP
      integer NF_EINDEP
      integer NF_EFILE
      integer NF_EREAD
      integer NF_EWRITE
      integer NF_EOFILE
      integer NF_EMULTITYPES
      integer NF_EIOMISMATCH
      integer NF_ENEGATIVECNT
      integer NF_EUNSPTETYPE
      integer NF_EINVAL_REQUEST
      integer NF_EAINT_TOO_SMALL
      integer NF_ENOTSUPPORT
      integer NF_ENULLBUF
      integer NF_EPREVATTACHBUF
      integer NF_ENULLABUF
      integer NF_EPENDINGBPUT
      integer NF_EINSUFFBUF
      integer NF_ENOENT
      integer NF_EINTOVERFLOW
      integer NF_ENOTENABLED
      integer NF_EBAD_FILE
      integer NF_ENO_SPACE
      integer NF_EQUOTA
      integer NF_ENULLSTART
      integer NF_ENULLCOUNT
      integer NF_EINVAL_CMODE
      integer NF_ETYPESIZE
      integer NF_ETYPE_MISMATCH
      integer NF_ETYPESIZE_MISMATCH
      integer NF_ESTRICTCDF2
      integer NF_ENOTRECVAR
      integer NF_ENOTFILL

      integer NF_EMULTIDEFINE
      integer NF_EMULTIDEFINE_OMODE,      NF_ECMODE
      integer NF_EMULTIDEFINE_DIM_NUM,    NF_EDIMS_NELEMS_MULTIDEFINE
      integer NF_EMULTIDEFINE_DIM_SIZE,   NF_EDIMS_SIZE_MULTIDEFINE
      integer NF_EMULTIDEFINE_DIM_NAME,   NF_EDIMS_NAME_MULTIDEFINE
      integer NF_EMULTIDEFINE_VAR_NUM,    NF_EVARS_NELEMS_MULTIDEFINE
      integer NF_EMULTIDEFINE_VAR_NAME,   NF_EVARS_NAME_MULTIDEFINE
      integer NF_EMULTIDEFINE_VAR_NDIMS,  NF_EVARS_NDIMS_MULTIDEFINE
      integer NF_EMULTIDEFINE_VAR_DIMIDS, NF_EVARS_DIMIDS_MULTIDEFINE
      integer NF_EMULTIDEFINE_VAR_TYPE,   NF_EVARS_TYPE_MULTIDEFINE
      integer NF_EMULTIDEFINE_VAR_LEN,    NF_EVARS_LEN_MULTIDEFINE
      integer NF_EMULTIDEFINE_VAR_BEGIN,  NF_EVARS_BEGIN_MULTIDEFINE
      integer NF_EMULTIDEFINE_NUMRECS,    NF_ENUMRECS_MULTIDEFINE
      integer NF_EMULTIDEFINE_ATTR_NUM
      integer NF_EMULTIDEFINE_ATTR_SIZE
      integer NF_EMULTIDEFINE_ATTR_NAME
      integer NF_EMULTIDEFINE_ATTR_TYPE
      integer NF_EMULTIDEFINE_ATTR_LEN
      integer NF_EMULTIDEFINE_ATTR_VAL
      integer NF_EMULTIDEFINE_FNC_ARGS
      integer NF_EMULTIDEFINE_FILL_MODE
      integer NF_EMULTIDEFINE_VAR_FILL_MODE
      integer NF_EMULTIDEFINE_VAR_FILL_VALUE

!
! PnetCDF error codes start from -201
!
      PARAMETER (NF_ESMALL                  = -201)   ! size of off_t too small for format
      PARAMETER (NF_ENOTINDEP               = -202)   ! Operation not allowed in collective data mode
      PARAMETER (NF_EINDEP                  = -203)   ! Operation not allowed in independent data mode
      PARAMETER (NF_EFILE                   = -204)   ! Unknown error in file operation
      PARAMETER (NF_EREAD                   = -205)   ! Unknown error in reading file
      PARAMETER (NF_EWRITE                  = -206)   ! Unknown error in writting to file
      PARAMETER (NF_EOFILE                  = -207)   ! file open/creation failed
      PARAMETER (NF_EMULTITYPES             = -208)   ! Multiple types used in memory data
      PARAMETER (NF_EIOMISMATCH             = -209)   ! Input/Output data amount mismatch
      PARAMETER (NF_ENEGATIVECNT            = -210)   ! Negative count is specified
      PARAMETER (NF_EUNSPTETYPE             = -211)   ! Unsupported etype in memory MPI datatype
      PARAMETER (NF_EINVAL_REQUEST          = -212)   ! invalid nonblocking request ID
      PARAMETER (NF_EAINT_TOO_SMALL         = -213)   ! MPI_Aint not large enough to hold requested value
      PARAMETER (NF_ENOTSUPPORT             = -214)   ! feature is not yet supported
      PARAMETER (NF_ENULLBUF                = -215)   ! trying to attach a NULL buffer
      PARAMETER (NF_EPREVATTACHBUF          = -216)   ! previous attached buffer is found
      PARAMETER (NF_ENULLABUF               = -217)   ! no attached buffer is found
      PARAMETER (NF_EPENDINGBPUT            = -218)   ! pending bput is found, cannot detach buffer
      PARAMETER (NF_EINSUFFBUF              = -219)   ! attached buffer is too small
      PARAMETER (NF_ENOENT                  = -220)   ! File does not exist when calling nfmpi_open()
      PARAMETER (NF_EINTOVERFLOW            = -221)   ! Overflow when type cast to 4-byte integer
      PARAMETER (NF_ENOTENABLED             = -222)   ! feature is not enabled
      PARAMETER (NF_EBAD_FILE               = -223)   ! Invalid file name (e.g., path name too long)
      PARAMETER (NF_ENO_SPACE               = -224)   ! Not enough space
      PARAMETER (NF_EQUOTA                  = -225)   ! Quota exceeded
      PARAMETER (NF_ENULLSTART              = -226)   ! argument start is a NULL pointer
      PARAMETER (NF_ENULLCOUNT              = -227)   ! argument count is a NULL pointer
      PARAMETER (NF_EINVAL_CMODE            = -228)   ! Invalid file create mode, cannot have both NC_64BIT_OFFSET & NC_64BIT_DATA
      PARAMETER (NF_ETYPESIZE               = -229)   ! MPI derived data type size error (bigger than the variable size)
      PARAMETER (NF_ETYPE_MISMATCH          = -230)   ! element type of the MPI derived data type mismatches the variable type
      PARAMETER (NF_ETYPESIZE_MISMATCH      = -231)   ! file type size mismatches buffer type size
      PARAMETER (NF_ESTRICTCDF2             = -232)   ! Attempting CDF-5 operation on CDF-2 file
      PARAMETER (NF_ENOTRECVAR              = -233)   ! Attempting operation only for record variables
      PARAMETER (NF_ENOTFILL                = -234)   ! Attempting to fill a variable when its fill mode is off

!
! PnetCDF header inconsistency errors start from -250
!
      PARAMETER (NF_EMULTIDEFINE            = -250)   ! NC definitions on multiprocesses conflict
      PARAMETER (NF_EMULTIDEFINE_OMODE      = -251)   ! file create/open modes are inconsistent
      PARAMETER (NF_EMULTIDEFINE_DIM_NUM    = -252)   ! inconsistent number of dimensions
      PARAMETER (NF_EMULTIDEFINE_DIM_SIZE   = -253)   ! inconsistent size of dimension
      PARAMETER (NF_EMULTIDEFINE_DIM_NAME   = -254)   ! inconsistent dimension names
      PARAMETER (NF_EMULTIDEFINE_VAR_NUM    = -255)   ! inconsistent number of variables
      PARAMETER (NF_EMULTIDEFINE_VAR_NAME   = -256)   ! inconsistent variable name
      PARAMETER (NF_EMULTIDEFINE_VAR_NDIMS  = -257)   ! inconsistent variable's number of dimensions
      PARAMETER (NF_EMULTIDEFINE_VAR_DIMIDS = -258)   ! inconsistent variable's dimid
      PARAMETER (NF_EMULTIDEFINE_VAR_TYPE   = -259)   ! inconsistent variable's data type
      PARAMETER (NF_EMULTIDEFINE_VAR_LEN    = -260)   ! inconsistent variable's size
      PARAMETER (NF_EMULTIDEFINE_NUMRECS    = -261)   ! inconsistent number of records
      PARAMETER (NF_EMULTIDEFINE_VAR_BEGIN  = -262)   ! inconsistent variable file begin offset (internal use)
      PARAMETER (NF_EMULTIDEFINE_ATTR_NUM   = -263)   ! inconsistent number of attributes
      PARAMETER (NF_EMULTIDEFINE_ATTR_SIZE  = -264)   ! inconsistent memory space used by attribute (internal use)
      PARAMETER (NF_EMULTIDEFINE_ATTR_NAME  = -265)   ! inconsistent attribute name
      PARAMETER (NF_EMULTIDEFINE_ATTR_TYPE  = -266)   ! inconsistent attribute type
      PARAMETER (NF_EMULTIDEFINE_ATTR_LEN   = -267)   ! inconsistent attribute length
      PARAMETER (NF_EMULTIDEFINE_ATTR_VAL   = -268)   ! inconsistent attribute value
      PARAMETER (NF_EMULTIDEFINE_FNC_ARGS   = -269)   ! inconsistent function arguments used in collective API
      PARAMETER (NF_EMULTIDEFINE_FILL_MODE  = -270)   !  inconsistent dataset fill mode
      PARAMETER (NF_EMULTIDEFINE_VAR_FILL_MODE  = -271) ! inconsistent variable fill mode
      PARAMETER (NF_EMULTIDEFINE_VAR_FILL_VALUE = -272) ! inconsistent variable fill value

      PARAMETER(NF_ECMODE                  =NF_EMULTIDEFINE_OMODE)
      PARAMETER(NF_EDIMS_NELEMS_MULTIDEFINE=NF_EMULTIDEFINE_DIM_NUM)
      PARAMETER(NF_EDIMS_SIZE_MULTIDEFINE  =NF_EMULTIDEFINE_DIM_SIZE)
      PARAMETER(NF_EDIMS_NAME_MULTIDEFINE  =NF_EMULTIDEFINE_DIM_NAME)
      PARAMETER(NF_EVARS_NELEMS_MULTIDEFINE=NF_EMULTIDEFINE_VAR_NUM)
      PARAMETER(NF_EVARS_NAME_MULTIDEFINE  =NF_EMULTIDEFINE_VAR_NAME)
      PARAMETER(NF_EVARS_NDIMS_MULTIDEFINE =NF_EMULTIDEFINE_VAR_NDIMS)
      PARAMETER(NF_EVARS_DIMIDS_MULTIDEFINE=NF_EMULTIDEFINE_VAR_DIMIDS)
      PARAMETER(NF_EVARS_TYPE_MULTIDEFINE  =NF_EMULTIDEFINE_VAR_TYPE)
      PARAMETER(NF_EVARS_LEN_MULTIDEFINE   =NF_EMULTIDEFINE_VAR_LEN)
      PARAMETER(NF_ENUMRECS_MULTIDEFINE    =NF_EMULTIDEFINE_NUMRECS)
      PARAMETER(NF_EVARS_BEGIN_MULTIDEFINE =NF_EMULTIDEFINE_VAR_BEGIN)

! error handling modes:
!
      integer nf_fatal
      integer nf_verbose

      parameter (nf_fatal = 1)
      parameter (nf_verbose = 2)


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! begin netcdf 2.4 backward compatibility:
!

!
! functions in the fortran interface
!

      integer ncrdwr
      integer nccreat
      integer ncexcl
      integer ncindef
      integer ncnsync
      integer nchsync
      integer ncndirty
      integer nchdirty
      integer nclink
      integer ncnowrit
      integer ncwrite
      integer ncclob
      integer ncnoclob
      integer ncglobal
      integer ncfill
      integer ncnofill
      integer maxncop
      integer maxncdim
      integer maxncatt
      integer maxncvar
      integer maxncnam
      integer maxvdims
      integer ncnoerr
      integer ncebadid
      integer ncenfile
      integer nceexist
      integer nceinval
      integer nceperm
      integer ncenotin
      integer nceindef
      integer ncecoord
      integer ncemaxds
      integer ncename
      integer ncenoatt
      integer ncemaxat
      integer ncebadty
      integer ncebadd
      integer ncests
      integer nceunlim
      integer ncemaxvs
      integer ncenotvr
      integer nceglob
      integer ncenotnc
      integer ncfoobar
      integer ncsyserr
      integer ncfatal
      integer ncverbos
      integer ncentool


!
! netcdf data types:
!
      integer ncbyte
      integer ncchar
      integer ncshort
      integer nclong
      integer ncfloat
      integer ncdouble

      parameter(ncbyte = 1)
      parameter(ncchar = 2)
      parameter(ncshort = 3)
      parameter(nclong = 4)
      parameter(ncfloat = 5)
      parameter(ncdouble = 6)

!
!     masks for the struct nc flag field; passed in as 'mode' arg to
!     nccreate and ncopen.
!

!     read/write, 0 => readonly
      parameter(ncrdwr = 1)
!     in create phase, cleared by ncendef
      parameter(nccreat = 2)
!     on create destroy existing file
      parameter(ncexcl = 4)
!     in define mode, cleared by ncendef
      parameter(ncindef = 8)
!     synchronise numrecs on change (x'10')
      parameter(ncnsync = 16)
!     synchronise whole header on change (x'20')
      parameter(nchsync = 32)
!     numrecs has changed (x'40')
      parameter(ncndirty = 64)
!     header info has changed (x'80')
      parameter(nchdirty = 128)
!     prefill vars on endef and increase of record, the default behavior
      parameter(ncfill = 0)
!     do not fill vars on endef and increase of record (x'100')
      parameter(ncnofill = 256)
!     isa link (x'8000')
      parameter(nclink = 32768)

!
!     'mode' arguments for nccreate and ncopen
!
      parameter(ncnowrit = 0)
      parameter(ncwrite = ncrdwr)
      parameter(ncclob = nf_clobber)
      parameter(ncnoclob = nf_noclobber)

!
!     'size' argument to ncdimdef for an unlimited dimension
!
      integer ncunlim
      parameter(ncunlim = 0)

!
!     attribute id to put/get a global attribute
!
      parameter(ncglobal  = 0)

!
!     advisory maximums:
!
      parameter(maxncop = 32)
      parameter(maxncdim = 100)
      parameter(maxncatt = 2000)
      parameter(maxncvar = 2000)
!     not enforced
      parameter(maxncnam = 128)
      parameter(maxvdims = maxncdim)

!
!     global netcdf error status variable
!     initialized in error.c
!

!     no error
      parameter(ncnoerr = nf_noerr)
!     not a netcdf id
      parameter(ncebadid = nf_ebadid)
!     too many netcdfs open
      parameter(ncenfile = -31)   ! nc_syserr
!     netcdf file exists && ncnoclob
      parameter(nceexist = nf_eexist)
!     invalid argument
      parameter(nceinval = nf_einval)
!     write to read only
      parameter(nceperm = nf_eperm)
!     operation not allowed in data mode
      parameter(ncenotin = nf_enotindefine)
!     operation not allowed in define mode
      parameter(nceindef = nf_eindefine)
!     coordinates out of domain
      parameter(ncecoord = nf_einvalcoords)
!     maxncdims exceeded
      parameter(ncemaxds = nf_emaxdims)
!     string match to name in use
      parameter(ncename = nf_enameinuse)
!     attribute not found
      parameter(ncenoatt = nf_enotatt)
!     maxncattrs exceeded
      parameter(ncemaxat = nf_emaxatts)
!     not a netcdf data type
      parameter(ncebadty = nf_ebadtype)
!     invalid dimension id
      parameter(ncebadd = nf_ebaddim)
!     ncunlimited in the wrong index
      parameter(nceunlim = nf_eunlimpos)
!     maxncvars exceeded
      parameter(ncemaxvs = nf_emaxvars)
!     variable not found
      parameter(ncenotvr = nf_enotvar)
!     action prohibited on ncglobal varid
      parameter(nceglob = nf_eglobal)
!     not a netcdf file
      parameter(ncenotnc = nf_enotnc)
      parameter(ncests = nf_ests)
      parameter (ncentool = nf_emaxname)
      parameter(ncfoobar = 32)
      parameter(ncsyserr = -31)

!
!     global options variable. used to determine behavior of error handler.
!     initialized in lerror.c
!
      parameter(ncfatal = 1)
      parameter(ncverbos = 2)

!
!     default fill values.  these must be the same as in the c interface.
!
      integer filbyte
      integer filchar
      integer filshort
      integer fillong
      real filfloat
      doubleprecision fildoub

      parameter (filbyte = -127)
      parameter (filchar = 0)
      parameter (filshort = -32767)
      parameter (fillong = -2147483647)
      parameter (filfloat = 9.9692099683868690e+36)
      parameter (fildoub  = 9.9692099683868690e+36)

! NULL request for non-blocking I/O APIs
      integer NF_REQ_NULL
      PARAMETER (NF_REQ_NULL = -1)

!
! PnetCDF APIs
!

!
! miscellaneous routines:
!
      character*80  nfmpi_inq_libvers
      character*80  nfmpi_strerror

      external      nfmpi_inq_libvers
      external      nfmpi_strerror

      logical       nfmpi_issyserr
      external      nfmpi_issyserr

!
! control routines:
!
      integer  nfmpi_create
      integer  nfmpi_open
      integer  nfmpi_inq_format
      integer  nfmpi_inq_file_format
      integer  nfmpi_inq_file_info
      integer  nfmpi_get_file_info
      integer  nfmpi_delete
      integer  nfmpi_enddef
      integer  nfmpi__enddef
      integer  nfmpi_redef
      integer  nfmpi_set_default_format
      integer  nfmpi_inq_default_format
      integer  nfmpi_sync
      integer  nfmpi_abort
      integer  nfmpi_close
      integer  nfmpi_set_fill
      integer  nfmpi_def_var_fill
      integer  nfmpi_inq_var_fill
      integer  nfmpi_fill_var_rec

      external nfmpi_create
      external nfmpi_open
      external nfmpi_inq_format
      external nfmpi_inq_file_format
      external nfmpi_inq_file_info
      external nfmpi_get_file_info
      external nfmpi_delete
      external nfmpi_enddef
      external nfmpi__enddef
      external nfmpi_redef
      external nfmpi_set_default_format
      external nfmpi_inq_default_format
      external nfmpi_sync
      external nfmpi_abort
      external nfmpi_close
      external nfmpi_set_fill
      external nfmpi_def_var_fill
      external nfmpi_inq_var_fill
      external nfmpi_fill_var_rec

!
! general inquiry routines:
!
      integer  nfmpi_inq
      integer  nfmpi_inq_ndims
      integer  nfmpi_inq_nvars
      integer  nfmpi_inq_num_rec_vars
      integer  nfmpi_inq_num_fix_vars
      integer  nfmpi_inq_natts
      integer  nfmpi_inq_unlimdim
      integer  nfmpi_inq_striping
      integer  nfmpi_inq_malloc_size
      integer  nfmpi_inq_malloc_max_size
      integer  nfmpi_inq_malloc_list
      integer  nfmpi_inq_files_opened
      integer  nfmpi_inq_recsize

      external nfmpi_inq
      external nfmpi_inq_ndims
      external nfmpi_inq_nvars
      external nfmpi_inq_num_rec_vars
      external nfmpi_inq_num_fix_vars
      external nfmpi_inq_natts
      external nfmpi_inq_unlimdim
      external nfmpi_inq_striping
      external nfmpi_inq_malloc_size
      external nfmpi_inq_malloc_max_size
      external nfmpi_inq_malloc_list
      external nfmpi_inq_files_opened
      external nfmpi_inq_recsize
!
! dimension routines:
!
      integer  nfmpi_def_dim
      integer  nfmpi_inq_dimid
      integer  nfmpi_inq_dim
      integer  nfmpi_inq_dimname
      integer  nfmpi_inq_dimlen
      integer  nfmpi_rename_dim

      external nfmpi_def_dim
      external nfmpi_inq_dimid
      external nfmpi_inq_dim
      external nfmpi_inq_dimname
      external nfmpi_inq_dimlen
      external nfmpi_rename_dim
!
! general attribute routines:
!
      integer  nfmpi_inq_att
      integer  nfmpi_inq_attid
      integer  nfmpi_inq_atttype
      integer  nfmpi_inq_attlen
      integer  nfmpi_inq_attname
      integer  nfmpi_copy_att
      integer  nfmpi_rename_att
      integer  nfmpi_del_att

      external nfmpi_inq_att
      external nfmpi_inq_attid
      external nfmpi_inq_atttype
      external nfmpi_inq_attlen
      external nfmpi_inq_attname
      external nfmpi_copy_att
      external nfmpi_rename_att
      external nfmpi_del_att

!
! attribute put/get routines:
!
      integer  nfmpi_put_att,        nfmpi_get_att
      integer  nfmpi_put_att_text,   nfmpi_get_att_text
      integer  nfmpi_put_att_int1,   nfmpi_get_att_int1
      integer  nfmpi_put_att_int2,   nfmpi_get_att_int2
      integer  nfmpi_put_att_int,    nfmpi_get_att_int
      integer  nfmpi_put_att_real,   nfmpi_get_att_real
      integer  nfmpi_put_att_double, nfmpi_get_att_double
      integer  nfmpi_put_att_int8,   nfmpi_get_att_int8

      external nfmpi_put_att,        nfmpi_get_att
      external nfmpi_put_att_text,   nfmpi_get_att_text
      external nfmpi_put_att_int1,   nfmpi_get_att_int1
      external nfmpi_put_att_int2,   nfmpi_get_att_int2
      external nfmpi_put_att_int,    nfmpi_get_att_int
      external nfmpi_put_att_real,   nfmpi_get_att_real
      external nfmpi_put_att_double, nfmpi_get_att_double
      external nfmpi_put_att_int8,   nfmpi_get_att_int8

!
! independent data mode routines:
!
      integer  nfmpi_begin_indep_data
      integer  nfmpi_end_indep_data

      external nfmpi_begin_indep_data
      external nfmpi_end_indep_data

!
! general variable routines:
!
      integer  nfmpi_def_var
      integer  nfmpi_inq_var
      integer  nfmpi_inq_varid
      integer  nfmpi_inq_varname
      integer  nfmpi_inq_vartype
      integer  nfmpi_inq_varndims
      integer  nfmpi_inq_vardimid
      integer  nfmpi_inq_varnatts
      integer  nfmpi_rename_var

      external nfmpi_def_var
      external nfmpi_inq_var
      external nfmpi_inq_varid
      external nfmpi_inq_varname
      external nfmpi_inq_vartype
      external nfmpi_inq_varndims
      external nfmpi_inq_vardimid
      external nfmpi_inq_varnatts
      external nfmpi_rename_var

!
! entire variable put/get routines:
!
      integer  nfmpi_put_var
      integer  nfmpi_put_var_text
      integer  nfmpi_put_var_int1
      integer  nfmpi_put_var_int2
      integer  nfmpi_put_var_int
      integer  nfmpi_put_var_real
      integer  nfmpi_put_var_double
      integer  nfmpi_put_var_int8

      external nfmpi_put_var
      external nfmpi_put_var_text
      external nfmpi_put_var_int1
      external nfmpi_put_var_int2
      external nfmpi_put_var_int
      external nfmpi_put_var_real
      external nfmpi_put_var_double
      external nfmpi_put_var_int8

      integer  nfmpi_get_var,        nfmpi_get_var_all
      integer  nfmpi_get_var_text,   nfmpi_get_var_text_all
      integer  nfmpi_get_var_int1,   nfmpi_get_var_int1_all
      integer  nfmpi_get_var_int2,   nfmpi_get_var_int2_all
      integer  nfmpi_get_var_int,    nfmpi_get_var_int_all
      integer  nfmpi_get_var_real,   nfmpi_get_var_real_all
      integer  nfmpi_get_var_double, nfmpi_get_var_double_all
      integer  nfmpi_get_var_int8,   nfmpi_get_var_int8_all

      external nfmpi_get_var,        nfmpi_get_var_all
      external nfmpi_get_var_text,   nfmpi_get_var_text_all
      external nfmpi_get_var_int1,   nfmpi_get_var_int1_all
      external nfmpi_get_var_int2,   nfmpi_get_var_int2_all
      external nfmpi_get_var_int,    nfmpi_get_var_int_all
      external nfmpi_get_var_real,   nfmpi_get_var_real_all
      external nfmpi_get_var_double, nfmpi_get_var_double_all
      external nfmpi_get_var_int8,   nfmpi_get_var_int8_all

!
! single element variable put/get routines:
!
      integer  nfmpi_put_var1,        nfmpi_put_var1_all
      integer  nfmpi_put_var1_text,   nfmpi_put_var1_text_all
      integer  nfmpi_put_var1_int1,   nfmpi_put_var1_int1_all
      integer  nfmpi_put_var1_int2,   nfmpi_put_var1_int2_all
      integer  nfmpi_put_var1_int,    nfmpi_put_var1_int_all
      integer  nfmpi_put_var1_real,   nfmpi_put_var1_real_all
      integer  nfmpi_put_var1_double, nfmpi_put_var1_double_all
      integer  nfmpi_put_var1_int8,   nfmpi_put_var1_int8_all

      external nfmpi_put_var1,        nfmpi_put_var1_all
      external nfmpi_put_var1_text,   nfmpi_put_var1_text_all
      external nfmpi_put_var1_int1,   nfmpi_put_var1_int1_all
      external nfmpi_put_var1_int2,   nfmpi_put_var1_int2_all
      external nfmpi_put_var1_int,    nfmpi_put_var1_int_all
      external nfmpi_put_var1_real,   nfmpi_put_var1_real_all
      external nfmpi_put_var1_double, nfmpi_put_var1_double_all
      external nfmpi_put_var1_int8,   nfmpi_put_var1_int8_all

      integer  nfmpi_get_var1,        nfmpi_get_var1_all
      integer  nfmpi_get_var1_text,   nfmpi_get_var1_text_all
      integer  nfmpi_get_var1_int1,   nfmpi_get_var1_int1_all
      integer  nfmpi_get_var1_int2,   nfmpi_get_var1_int2_all
      integer  nfmpi_get_var1_int,    nfmpi_get_var1_int_all
      integer  nfmpi_get_var1_real,   nfmpi_get_var1_real_all
      integer  nfmpi_get_var1_double, nfmpi_get_var1_double_all
      integer  nfmpi_get_var1_int8,   nfmpi_get_var1_int8_all

      external nfmpi_get_var1,        nfmpi_get_var1_all
      external nfmpi_get_var1_text,   nfmpi_get_var1_text_all
      external nfmpi_get_var1_int1,   nfmpi_get_var1_int1_all
      external nfmpi_get_var1_int2,   nfmpi_get_var1_int2_all
      external nfmpi_get_var1_int,    nfmpi_get_var1_int_all
      external nfmpi_get_var1_real,   nfmpi_get_var1_real_all
      external nfmpi_get_var1_double, nfmpi_get_var1_double_all
      external nfmpi_get_var1_int8,   nfmpi_get_var1_int8_all

!
! variable sub-array put/get routines:
!
      integer  nfmpi_put_vara,        nfmpi_put_vara_all
      integer  nfmpi_put_vara_text,   nfmpi_put_vara_text_all
      integer  nfmpi_put_vara_int1,   nfmpi_put_vara_int1_all
      integer  nfmpi_put_vara_int2,   nfmpi_put_vara_int2_all
      integer  nfmpi_put_vara_int,    nfmpi_put_vara_int_all
      integer  nfmpi_put_vara_real,   nfmpi_put_vara_real_all
      integer  nfmpi_put_vara_double, nfmpi_put_vara_double_all
      integer  nfmpi_put_vara_int8,   nfmpi_put_vara_int8_all

      external nfmpi_put_vara,        nfmpi_put_vara_all
      external nfmpi_put_vara_text,   nfmpi_put_vara_text_all
      external nfmpi_put_vara_int1,   nfmpi_put_vara_int1_all
      external nfmpi_put_vara_int2,   nfmpi_put_vara_int2_all
      external nfmpi_put_vara_int,    nfmpi_put_vara_int_all
      external nfmpi_put_vara_real,   nfmpi_put_vara_real_all
      external nfmpi_put_vara_double, nfmpi_put_vara_double_all
      external nfmpi_put_vara_int8,   nfmpi_put_vara_int8_all

      integer  nfmpi_get_vara,        nfmpi_get_vara_all
      integer  nfmpi_get_vara_text,   nfmpi_get_vara_text_all
      integer  nfmpi_get_vara_int1,   nfmpi_get_vara_int1_all
      integer  nfmpi_get_vara_int2,   nfmpi_get_vara_int2_all
      integer  nfmpi_get_vara_int,    nfmpi_get_vara_int_all
      integer  nfmpi_get_vara_real,   nfmpi_get_vara_real_all
      integer  nfmpi_get_vara_double, nfmpi_get_vara_double_all
      integer  nfmpi_get_vara_int8,   nfmpi_get_vara_int8_all

      external nfmpi_get_vara,        nfmpi_get_vara_all
      external nfmpi_get_vara_text,   nfmpi_get_vara_text_all
      external nfmpi_get_vara_int1,   nfmpi_get_vara_int1_all
      external nfmpi_get_vara_int2,   nfmpi_get_vara_int2_all
      external nfmpi_get_vara_int,    nfmpi_get_vara_int_all
      external nfmpi_get_vara_real,   nfmpi_get_vara_real_all
      external nfmpi_get_vara_double, nfmpi_get_vara_double_all
      external nfmpi_get_vara_int8,   nfmpi_get_vara_int8_all

!
! strided variable put/get routines:
!
      integer  nfmpi_put_vars,        nfmpi_put_vars_all
      integer  nfmpi_put_vars_text,   nfmpi_put_vars_text_all
      integer  nfmpi_put_vars_int1,   nfmpi_put_vars_int1_all
      integer  nfmpi_put_vars_int2,   nfmpi_put_vars_int2_all
      integer  nfmpi_put_vars_int,    nfmpi_put_vars_int_all
      integer  nfmpi_put_vars_real,   nfmpi_put_vars_real_all
      integer  nfmpi_put_vars_double, nfmpi_put_vars_double_all
      integer  nfmpi_put_vars_int8,   nfmpi_put_vars_int8_all

      external nfmpi_put_vars,        nfmpi_put_vars_all
      external nfmpi_put_vars_text,   nfmpi_put_vars_text_all
      external nfmpi_put_vars_int1,   nfmpi_put_vars_int1_all
      external nfmpi_put_vars_int2,   nfmpi_put_vars_int2_all
      external nfmpi_put_vars_int,    nfmpi_put_vars_int_all
      external nfmpi_put_vars_real,   nfmpi_put_vars_real_all
      external nfmpi_put_vars_double, nfmpi_put_vars_double_all
      external nfmpi_put_vars_int8,   nfmpi_put_vars_int8_all

      integer  nfmpi_get_vars,        nfmpi_get_vars_all
      integer  nfmpi_get_vars_text,   nfmpi_get_vars_text_all
      integer  nfmpi_get_vars_int1,   nfmpi_get_vars_int1_all
      integer  nfmpi_get_vars_int2,   nfmpi_get_vars_int2_all
      integer  nfmpi_get_vars_int,    nfmpi_get_vars_int_all
      integer  nfmpi_get_vars_real,   nfmpi_get_vars_real_all
      integer  nfmpi_get_vars_double, nfmpi_get_vars_double_all
      integer  nfmpi_get_vars_int8,   nfmpi_get_vars_int8_all

      external nfmpi_get_vars,        nfmpi_get_vars_all
      external nfmpi_get_vars_text,   nfmpi_get_vars_text_all
      external nfmpi_get_vars_int1,   nfmpi_get_vars_int1_all
      external nfmpi_get_vars_int2,   nfmpi_get_vars_int2_all
      external nfmpi_get_vars_int,    nfmpi_get_vars_int_all
      external nfmpi_get_vars_real,   nfmpi_get_vars_real_all
      external nfmpi_get_vars_double, nfmpi_get_vars_double_all
      external nfmpi_get_vars_int8,   nfmpi_get_vars_int8_all

!
! mapped variable put/get routines:
!
      integer  nfmpi_put_varm,        nfmpi_put_varm_all
      integer  nfmpi_put_varm_text,   nfmpi_put_varm_text_all
      integer  nfmpi_put_varm_int1,   nfmpi_put_varm_int1_all
      integer  nfmpi_put_varm_int2,   nfmpi_put_varm_int2_all
      integer  nfmpi_put_varm_int,    nfmpi_put_varm_int_all
      integer  nfmpi_put_varm_real,   nfmpi_put_varm_real_all
      integer  nfmpi_put_varm_double, nfmpi_put_varm_double_all
      integer  nfmpi_put_varm_int8,   nfmpi_put_varm_int8_all

      external nfmpi_put_varm,        nfmpi_put_varm_all
      external nfmpi_put_varm_text,   nfmpi_put_varm_text_all
      external nfmpi_put_varm_int1,   nfmpi_put_varm_int1_all
      external nfmpi_put_varm_int2,   nfmpi_put_varm_int2_all
      external nfmpi_put_varm_int,    nfmpi_put_varm_int_all
      external nfmpi_put_varm_real,   nfmpi_put_varm_real_all
      external nfmpi_put_varm_double, nfmpi_put_varm_double_all
      external nfmpi_put_varm_int8,   nfmpi_put_varm_int8_all

      integer  nfmpi_get_varm,        nfmpi_get_varm_all
      integer  nfmpi_get_varm_text,   nfmpi_get_varm_text_all
      integer  nfmpi_get_varm_int1,   nfmpi_get_varm_int1_all
      integer  nfmpi_get_varm_int2,   nfmpi_get_varm_int2_all
      integer  nfmpi_get_varm_int,    nfmpi_get_varm_int_all
      integer  nfmpi_get_varm_real,   nfmpi_get_varm_real_all
      integer  nfmpi_get_varm_double, nfmpi_get_varm_double_all
      integer  nfmpi_get_varm_int8,   nfmpi_get_varm_int8_all

      external nfmpi_get_varm,        nfmpi_get_varm_all
      external nfmpi_get_varm_text,   nfmpi_get_varm_text_all
      external nfmpi_get_varm_int1,   nfmpi_get_varm_int1_all
      external nfmpi_get_varm_int2,   nfmpi_get_varm_int2_all
      external nfmpi_get_varm_int,    nfmpi_get_varm_int_all
      external nfmpi_get_varm_real,   nfmpi_get_varm_real_all
      external nfmpi_get_varm_double, nfmpi_get_varm_double_all
      external nfmpi_get_varm_int8,   nfmpi_get_varm_int8_all

!
! Non-blocking APIs
!
! entire variable iput/iget routines:
!
      integer  nfmpi_iput_var
      integer  nfmpi_iput_var_text
      integer  nfmpi_iput_var_int1
      integer  nfmpi_iput_var_int2
      integer  nfmpi_iput_var_int
      integer  nfmpi_iput_var_real
      integer  nfmpi_iput_var_double
      integer  nfmpi_iput_var_int8

      external nfmpi_iput_var
      external nfmpi_iput_var_text
      external nfmpi_iput_var_int1
      external nfmpi_iput_var_int2
      external nfmpi_iput_var_int
      external nfmpi_iput_var_real
      external nfmpi_iput_var_double
      external nfmpi_iput_var_int8

      integer  nfmpi_iget_var
      integer  nfmpi_iget_var_text
      integer  nfmpi_iget_var_int1
      integer  nfmpi_iget_var_int2
      integer  nfmpi_iget_var_int
      integer  nfmpi_iget_var_real
      integer  nfmpi_iget_var_double
      integer  nfmpi_iget_var_int8

      external nfmpi_iget_var
      external nfmpi_iget_var_text
      external nfmpi_iget_var_int1
      external nfmpi_iget_var_int2
      external nfmpi_iget_var_int
      external nfmpi_iget_var_real
      external nfmpi_iget_var_double
      external nfmpi_iget_var_int8

!
! Nonblocking single-element variable iput/iget routines:
!
      integer  nfmpi_iput_var1
      integer  nfmpi_iput_var1_text
      integer  nfmpi_iput_var1_int1
      integer  nfmpi_iput_var1_int2
      integer  nfmpi_iput_var1_int
      integer  nfmpi_iput_var1_real
      integer  nfmpi_iput_var1_double
      integer  nfmpi_iput_var1_int8

      external nfmpi_iput_var1
      external nfmpi_iput_var1_text
      external nfmpi_iput_var1_int1
      external nfmpi_iput_var1_int2
      external nfmpi_iput_var1_int
      external nfmpi_iput_var1_real
      external nfmpi_iput_var1_double
      external nfmpi_iput_var1_int8

      integer  nfmpi_iget_var1
      integer  nfmpi_iget_var1_text
      integer  nfmpi_iget_var1_int1
      integer  nfmpi_iget_var1_int2
      integer  nfmpi_iget_var1_int
      integer  nfmpi_iget_var1_real
      integer  nfmpi_iget_var1_double
      integer  nfmpi_iget_var1_int8

      external nfmpi_iget_var1
      external nfmpi_iget_var1_text
      external nfmpi_iget_var1_int1
      external nfmpi_iget_var1_int2
      external nfmpi_iget_var1_int
      external nfmpi_iget_var1_real
      external nfmpi_iget_var1_double
      external nfmpi_iget_var1_int8

!
! Nonblocking subarray variable iput/iget routines:
!
      integer  nfmpi_iput_vara
      integer  nfmpi_iput_vara_text
      integer  nfmpi_iput_vara_int1
      integer  nfmpi_iput_vara_int2
      integer  nfmpi_iput_vara_int
      integer  nfmpi_iput_vara_real
      integer  nfmpi_iput_vara_double
      integer  nfmpi_iput_vara_int8

      external nfmpi_iput_vara
      external nfmpi_iput_vara_text
      external nfmpi_iput_vara_int1
      external nfmpi_iput_vara_int2
      external nfmpi_iput_vara_int
      external nfmpi_iput_vara_real
      external nfmpi_iput_vara_double
      external nfmpi_iput_vara_int8

      integer  nfmpi_iget_vara
      integer  nfmpi_iget_vara_text
      integer  nfmpi_iget_vara_int1
      integer  nfmpi_iget_vara_int2
      integer  nfmpi_iget_vara_int
      integer  nfmpi_iget_vara_real
      integer  nfmpi_iget_vara_double
      integer  nfmpi_iget_vara_int8

      external nfmpi_iget_vara
      external nfmpi_iget_vara_text
      external nfmpi_iget_vara_int1
      external nfmpi_iget_vara_int2
      external nfmpi_iget_vara_int
      external nfmpi_iget_vara_real
      external nfmpi_iget_vara_double
      external nfmpi_iget_vara_int8

!
! Nonblocking strided variable iput/iget routines:
!
      integer  nfmpi_iput_vars
      integer  nfmpi_iput_vars_text
      integer  nfmpi_iput_vars_int1
      integer  nfmpi_iput_vars_int2
      integer  nfmpi_iput_vars_int
      integer  nfmpi_iput_vars_real
      integer  nfmpi_iput_vars_double
      integer  nfmpi_iput_vars_int8

      external nfmpi_iput_vars
      external nfmpi_iput_vars_text
      external nfmpi_iput_vars_int1
      external nfmpi_iput_vars_int2
      external nfmpi_iput_vars_int
      external nfmpi_iput_vars_real
      external nfmpi_iput_vars_double
      external nfmpi_iput_vars_int8

      integer  nfmpi_iget_vars
      integer  nfmpi_iget_vars_text
      integer  nfmpi_iget_vars_int1
      integer  nfmpi_iget_vars_int2
      integer  nfmpi_iget_vars_int
      integer  nfmpi_iget_vars_real
      integer  nfmpi_iget_vars_double
      integer  nfmpi_iget_vars_int8

      external nfmpi_iget_vars
      external nfmpi_iget_vars_text
      external nfmpi_iget_vars_int1
      external nfmpi_iget_vars_int2
      external nfmpi_iget_vars_int
      external nfmpi_iget_vars_real
      external nfmpi_iget_vars_double
      external nfmpi_iget_vars_int8

!
! Nonblocking mapped variable iput/iget routines:
!
      integer  nfmpi_iput_varm
      integer  nfmpi_iput_varm_text
      integer  nfmpi_iput_varm_int1
      integer  nfmpi_iput_varm_int2
      integer  nfmpi_iput_varm_int
      integer  nfmpi_iput_varm_real
      integer  nfmpi_iput_varm_double
      integer  nfmpi_iput_varm_int8

      external nfmpi_iput_varm
      external nfmpi_iput_varm_text
      external nfmpi_iput_varm_int1
      external nfmpi_iput_varm_int2
      external nfmpi_iput_varm_int
      external nfmpi_iput_varm_real
      external nfmpi_iput_varm_double
      external nfmpi_iput_varm_int8

      integer  nfmpi_iget_varm
      integer  nfmpi_iget_varm_text
      integer  nfmpi_iget_varm_int1
      integer  nfmpi_iget_varm_int2
      integer  nfmpi_iget_varm_int
      integer  nfmpi_iget_varm_real
      integer  nfmpi_iget_varm_double
      integer  nfmpi_iget_varm_int8

      external nfmpi_iget_varm
      external nfmpi_iget_varm_text
      external nfmpi_iget_varm_int1
      external nfmpi_iget_varm_int2
      external nfmpi_iget_varm_int
      external nfmpi_iget_varm_real
      external nfmpi_iget_varm_double
      external nfmpi_iget_varm_int8

!
! Nonblocking entire variable bput routines:
!
      integer  nfmpi_bput_var
      integer  nfmpi_bput_var_text
      integer  nfmpi_bput_var_int1
      integer  nfmpi_bput_var_int2
      integer  nfmpi_bput_var_int
      integer  nfmpi_bput_var_real
      integer  nfmpi_bput_var_double
      integer  nfmpi_bput_var_int8

      external nfmpi_bput_var
      external nfmpi_bput_var_text
      external nfmpi_bput_var_int1
      external nfmpi_bput_var_int2
      external nfmpi_bput_var_int
      external nfmpi_bput_var_real
      external nfmpi_bput_var_double
      external nfmpi_bput_var_int8

!
! Nonblocking single element variable bput routines:
!
      integer  nfmpi_bput_var1
      integer  nfmpi_bput_var1_text
      integer  nfmpi_bput_var1_int1
      integer  nfmpi_bput_var1_int2
      integer  nfmpi_bput_var1_int
      integer  nfmpi_bput_var1_real
      integer  nfmpi_bput_var1_double
      integer  nfmpi_bput_var1_int8

      external nfmpi_bput_var1
      external nfmpi_bput_var1_text
      external nfmpi_bput_var1_int1
      external nfmpi_bput_var1_int2
      external nfmpi_bput_var1_int
      external nfmpi_bput_var1_real
      external nfmpi_bput_var1_double
      external nfmpi_bput_var1_int8

!
! Nonblocking subarray variable bput routines:
!
      integer  nfmpi_bput_vara
      integer  nfmpi_bput_vara_text
      integer  nfmpi_bput_vara_int1
      integer  nfmpi_bput_vara_int2
      integer  nfmpi_bput_vara_int
      integer  nfmpi_bput_vara_real
      integer  nfmpi_bput_vara_double
      integer  nfmpi_bput_vara_int8

      external nfmpi_bput_vara
      external nfmpi_bput_vara_text
      external nfmpi_bput_vara_int1
      external nfmpi_bput_vara_int2
      external nfmpi_bput_vara_int
      external nfmpi_bput_vara_real
      external nfmpi_bput_vara_double
      external nfmpi_bput_vara_int8

!
! Nonblocking strided variable bput routines:
!
      integer  nfmpi_bput_vars
      integer  nfmpi_bput_vars_text
      integer  nfmpi_bput_vars_int1
      integer  nfmpi_bput_vars_int2
      integer  nfmpi_bput_vars_int
      integer  nfmpi_bput_vars_real
      integer  nfmpi_bput_vars_double
      integer  nfmpi_bput_vars_int8

      external nfmpi_bput_vars
      external nfmpi_bput_vars_text
      external nfmpi_bput_vars_int1
      external nfmpi_bput_vars_int2
      external nfmpi_bput_vars_int
      external nfmpi_bput_vars_real
      external nfmpi_bput_vars_double
      external nfmpi_bput_vars_int8

!
! Nonblocking mapped variable bput routines:
!
      integer  nfmpi_bput_varm
      integer  nfmpi_bput_varm_text
      integer  nfmpi_bput_varm_int1
      integer  nfmpi_bput_varm_int2
      integer  nfmpi_bput_varm_int
      integer  nfmpi_bput_varm_real
      integer  nfmpi_bput_varm_double
      integer  nfmpi_bput_varm_int8

      external nfmpi_bput_varm
      external nfmpi_bput_varm_text
      external nfmpi_bput_varm_int1
      external nfmpi_bput_varm_int2
      external nfmpi_bput_varm_int
      external nfmpi_bput_varm_real
      external nfmpi_bput_varm_double
      external nfmpi_bput_varm_int8

!
! Nonblocking control APIs
!
      integer  nfmpi_wait
      integer  nfmpi_wait_all
      integer  nfmpi_cancel

      external nfmpi_wait
      external nfmpi_wait_all
      external nfmpi_cancel

      integer  nfmpi_buffer_attach
      integer  nfmpi_buffer_detach
      integer  nfmpi_inq_buffer_usage
      integer  nfmpi_inq_buffer_size
      integer  nfmpi_inq_put_size
      integer  nfmpi_inq_get_size
      integer  nfmpi_inq_header_size
      integer  nfmpi_inq_header_extent
      integer  nfmpi_inq_varoffset
      integer  nfmpi_inq_nreqs

      external nfmpi_buffer_attach
      external nfmpi_buffer_detach
      external nfmpi_inq_buffer_usage
      external nfmpi_inq_buffer_size
      external nfmpi_inq_put_size
      external nfmpi_inq_get_size
      external nfmpi_inq_header_size
      external nfmpi_inq_header_extent
      external nfmpi_inq_varoffset
      external nfmpi_inq_nreqs

!
! varn routines:
!
      integer  nfmpi_put_varn,        nfmpi_put_varn_all
      integer  nfmpi_put_varn_text,   nfmpi_put_varn_text_all
      integer  nfmpi_put_varn_int1,   nfmpi_put_varn_int1_all
      integer  nfmpi_put_varn_int2,   nfmpi_put_varn_int2_all
      integer  nfmpi_put_varn_int,    nfmpi_put_varn_int_all
      integer  nfmpi_put_varn_real,   nfmpi_put_varn_real_all
      integer  nfmpi_put_varn_double, nfmpi_put_varn_double_all
      integer  nfmpi_put_varn_int8,   nfmpi_put_varn_int8_all

      external nfmpi_put_varn,        nfmpi_put_varn_all
      external nfmpi_put_varn_text,   nfmpi_put_varn_text_all
      external nfmpi_put_varn_int1,   nfmpi_put_varn_int1_all
      external nfmpi_put_varn_int2,   nfmpi_put_varn_int2_all
      external nfmpi_put_varn_int,    nfmpi_put_varn_int_all
      external nfmpi_put_varn_real,   nfmpi_put_varn_real_all
      external nfmpi_put_varn_double, nfmpi_put_varn_double_all
      external nfmpi_put_varn_int8,   nfmpi_put_varn_int8_all

      integer  nfmpi_get_varn,        nfmpi_get_varn_all
      integer  nfmpi_get_varn_text,   nfmpi_get_varn_text_all
      integer  nfmpi_get_varn_int1,   nfmpi_get_varn_int1_all
      integer  nfmpi_get_varn_int2,   nfmpi_get_varn_int2_all
      integer  nfmpi_get_varn_int,    nfmpi_get_varn_int_all
      integer  nfmpi_get_varn_real,   nfmpi_get_varn_real_all
      integer  nfmpi_get_varn_double, nfmpi_get_varn_double_all
      integer  nfmpi_get_varn_int8,   nfmpi_get_varn_int8_all

      external nfmpi_get_varn,        nfmpi_get_varn_all
      external nfmpi_get_varn_text,   nfmpi_get_varn_text_all
      external nfmpi_get_varn_int1,   nfmpi_get_varn_int1_all
      external nfmpi_get_varn_int2,   nfmpi_get_varn_int2_all
      external nfmpi_get_varn_int,    nfmpi_get_varn_int_all
      external nfmpi_get_varn_real,   nfmpi_get_varn_real_all
      external nfmpi_get_varn_double, nfmpi_get_varn_double_all
      external nfmpi_get_varn_int8,   nfmpi_get_varn_int8_all

!
! Nonblocking varn routines:
!
      integer  nfmpi_iput_varn
      integer  nfmpi_iput_varn_text
      integer  nfmpi_iput_varn_int1
      integer  nfmpi_iput_varn_int2
      integer  nfmpi_iput_varn_int
      integer  nfmpi_iput_varn_real
      integer  nfmpi_iput_varn_double
      integer  nfmpi_iput_varn_int8

      external nfmpi_iput_varn
      external nfmpi_iput_varn_text
      external nfmpi_iput_varn_int1
      external nfmpi_iput_varn_int2
      external nfmpi_iput_varn_int
      external nfmpi_iput_varn_real
      external nfmpi_iput_varn_double
      external nfmpi_iput_varn_int8

      integer  nfmpi_iget_varn
      integer  nfmpi_iget_varn_text
      integer  nfmpi_iget_varn_int1
      integer  nfmpi_iget_varn_int2
      integer  nfmpi_iget_varn_int
      integer  nfmpi_iget_varn_real
      integer  nfmpi_iget_varn_double
      integer  nfmpi_iget_varn_int8

      external nfmpi_iget_varn
      external nfmpi_iget_varn_text
      external nfmpi_iget_varn_int1
      external nfmpi_iget_varn_int2
      external nfmpi_iget_varn_int
      external nfmpi_iget_varn_real
      external nfmpi_iget_varn_double
      external nfmpi_iget_varn_int8

      integer  nfmpi_bput_varn
      integer  nfmpi_bput_varn_text
      integer  nfmpi_bput_varn_int1
      integer  nfmpi_bput_varn_int2
      integer  nfmpi_bput_varn_int
      integer  nfmpi_bput_varn_real
      integer  nfmpi_bput_varn_double
      integer  nfmpi_bput_varn_int8

      external nfmpi_bput_varn
      external nfmpi_bput_varn_text
      external nfmpi_bput_varn_int1
      external nfmpi_bput_varn_int2
      external nfmpi_bput_varn_int
      external nfmpi_bput_varn_real
      external nfmpi_bput_varn_double
      external nfmpi_bput_varn_int8

!
! vard routines:
!
      integer  nfmpi_put_vard, nfmpi_put_vard_all
      integer  nfmpi_get_vard, nfmpi_get_vard_all

      external nfmpi_put_vard, nfmpi_put_vard_all
      external nfmpi_get_vard, nfmpi_get_vard_all

