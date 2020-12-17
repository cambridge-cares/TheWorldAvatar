MODULE MODNCFIO

    !!.........................................................................
    !!  Version "$Id: modncfio.F90 9 2017-07-04 23:50:30Z coats $"
    !!  Copyright (c) 2015-2016 UNC Institute for the Environment.
    !!  Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    !!  See file "LGPL.txt" for conditions of use.
    !!........................................................................
    !!
    !!  DESCRIPTION:
    !!      This is a merge of netCDF version 3.x/4.x (version 3.6.2) "fortran/netcdf.inc"
    !!      with PnetCDF (version 1.6.1) "pnetcdf.inc", but under another INCLUDE-file 
    !       naming convention.
    !!      NetCDF copyright 1990-2008 University Corporation for Atmospheric Research;
    !!      see URL  http://www.unidata.ucar.edu/packages/netcdf/index.html
    !!      PnetCDF copyright status unknown; see
    !!      <https://trac.mcs.anl.gov/projects/parallel-netcdf>
    !!
    !!  ALSO CONTAINS: "raw" netCDF access-routines starting at line 1781:
    !!
    !!      DESCNCVAR( FNAME, MXVAR, NVARS, VNAMES, VUNITS, VTYPES, VNDIMS, VDIMS )
    !!        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
    !!        INTEGER      , INTENT(IN   ) :: MXVAR           !!  max # of vars returnede
    !!        INTEGER      , INTENT(  OUT) :: NVARS           !!  min( MXVAR, actual # of bles )
    !!        CHARACTER*(*), INTENT(  OUT) :: VNAMES( MXVAR ) !!  variable name
    !!        CHARACTER*(*), INTENT(  OUT) :: VUNITS( MXVAR ) !!  variable units, or CMISS3
    !!        INTEGER      , INTENT(  OUT) :: VTYPES( MXVAR ) !!  types (M3REAL, M3INT, etc.)
    !!        INTEGER      , INTENT(  OUT) :: VNDIMS( MXVAR ) !!  ranks (number of dimensions)
    !!        INTEGER      , INTENT(  OUT) :: VDIMS(7,MXVAR ) !!  dimensions for variables
    !!      returns the number NVARS of netCDF variables on the file FNAME, 
    !!      together with lists of their names, types, numbers of dimensions,
    !!      and sizes of dimensions:  e.g., if NVARS==5, then the third
    !!      variable has names VNAMES(3), type VTYPES(3) [M3REAL, M3INT, etc],
    !!      VNDIMS(3) dimensions.  If variable-3 is 2-D (so VNDIMS(3)==2), then
    !!      VDIMS(1,3) is the number of columns and VDIMS(2,3) is the number of
    !!      rows for that variable.
    !!
    !!      READNCVAR() is a generic "read-a-variable" routine; the compiler looks
    !!      at the function-call and selects among the following, where the naming
    !!      scheme is:
    !!
    !!          READNCVAR*() return a result with the same dimensioning
    !!                       as on the file (e.g., time independent data), and 
    !!          READNCVEC*() returns a single-indexed (e.g., GRID(NCOLS*NROWS) result.
    !!          READNCVSTEP*() for reading one time step of a TIME STEPPED variable
    !!          READNC*I()   are for INTEGER            netCDF variables
    !!          READNC*R()   are for REAL               netCDF variables
    !!          READNC*D()   are for DOUBLE PRECISION   netCDF variables
    !!          READNC*S()   are for SHORT (INTEGER*2)  netCDF variables
    !!          READNC*B()   are for BYTE  (INTEGER*1)  netCDF variables
    !!
    !!      LOGICAL FUNCTION READNCVAR0DI( FNAME, VNAME, IGRID0 )
    !!      LOGICAL FUNCTION READNCVAR0DR( FNAME, VNAME, RGRID0 )
    !!      LOGICAL FUNCTION READNCVAR0DD( FNAME, VNAME, DGRID0 )
    !!      LOGICAL FUNCTION READNCVAR1DI( FNAME, VNAME, NCOLS, IGRID1 )
    !!      LOGICAL FUNCTION READNCVAR1DR( FNAME, VNAME, NCOLS, RGRID1 )
    !!      LOGICAL FUNCTION READNCVAR1DD( FNAME, VNAME, NCOLS, DGRID1 )
    !!      LOGICAL FUNCTION READNCVAR2DI( FNAME, VNAME, NCOLS, NROWS, IGRID2 )
    !!      LOGICAL FUNCTION READNCVAR2DR( FNAME, VNAME, NCOLS, NROWS, RGRID2 )
    !!      LOGICAL FUNCTION READNCVAR2DD( FNAME, VNAME, NCOLS, NROWS, DGRID2 )
    !!      LOGICAL FUNCTION READNCVAR3DI( FNAME, VNAME, NCOLS, NROWS, NLAYS, IGRID3 )
    !!      LOGICAL FUNCTION READNCVAR3DR( FNAME, VNAME, NCOLS, NROWS, NLAYS, RGRID3 )
    !!      LOGICAL FUNCTION READNCVAR3DD( FNAME, VNAME, NCOLS, NROWS, NLAYS, DGRID3 )
    !!      LOGICAL FUNCTION READNCVEC2DI( FNAME, VNAME, NCOLS, NROWS, IVEC2 )
    !!      LOGICAL FUNCTION READNCVEC2DR( FNAME, VNAME, NCOLS, NROWS, RVEC2 )
    !!      LOGICAL FUNCTION READNCVEC2DD( FNAME, VNAME, NCOLS, NROWS, DVEC2 )
    !!      LOGICAL FUNCTION READNCVEC3DI( FNAME, VNAME, NCOLS, NROWS, NLAYS, IVEC3 )
    !!      LOGICAL FUNCTION READNCVEC3DR( FNAME, VNAME, NCOLS, NROWS, NLAYS, RVEC3 )
    !!      LOGICAL FUNCTION READNCVEC3DD( FNAME, VNAME, NCOLS, NROWS, NLAYS, DVEC3 )
    !!        CHARACTER*(*), INTENT(IN   ) :: FNAME                !!  logical file name
    !!        CHARACTER*(*), INTENT(IN   ) :: VNAME                !!  variable name
    !!        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS, NLAYS  !!  dimensions
    !!        &lt;type&gt;       , INTENT(  OUT) ::  GRID ( NCOLS, [NROWS, [NLAYS,]] )
    !!        INTEGER      , INTENT(  OUT) :: IGRID0
    !!        REAL         , INTENT(  OUT) :: RGRID0
    !!        REAL*8       , INTENT(  OUT) :: DGRID0
    !!        INTEGER      , INTENT(  OUT) :: IGRID1( NCOLS )
    !!        REAL         , INTENT(  OUT) :: RGRID1( NCOLS )
    !!        REAL*8       , INTENT(  OUT) :: DGRID1( NCOLS )
    !!        INTEGER      , INTENT(  OUT) :: IGRID2( NCOLS, NROWS )
    !!        REAL         , INTENT(  OUT) :: RGRID2( NCOLS, NROWS )
    !!        REAL*8       , INTENT(  OUT) :: DGRID2( NCOLS, NROWS )
    !!        INTEGER      , INTENT(  OUT) :: IGRID3( NCOLS, NROWS, NLAYS )
    !!        REAL         , INTENT(  OUT) :: RGRID3( NCOLS, NROWS, NLAYS )
    !!        REAL*8       , INTENT(  OUT) :: DGRID3( NCOLS, NROWS, NLAYS )
    !!        INTEGER      , INTENT(  OUT) ::  IVEC2( NCOLS*NROWS )
    !!        REAL         , INTENT(  OUT) ::  RVEC2( NCOLS*NROWS )
    !!        REAL*8       , INTENT(  OUT) ::  DVEC2( NCOLS*NROWS )
    !!        INTEGER      , INTENT(  OUT) ::  IVEC3( NCOLS*NROWS*NLAYS )
    !!        REAL         , INTENT(  OUT) ::  RVEC3( NCOLS*NROWS*NLAYS )
    !!        REAL*8       , INTENT(  OUT) ::  DVEC3( NCOLS*NROWS*NLAYS )
    !!
    !!      In all cases, the routine will check the caller-supplied
    !!      dimensions (NCOLS, etc.) against the dimensions on the file,
    !!      and the return-array type against the variable-type from
    !!      the file.  If OK, will read the variable into the user supplied
    !!      array.
    !!
    !!  REVISION  HISTORY:
    !!      Prototype  10/2015 by Carlie J. Coats, Jr., UNC IE
    !!
    !!      Version  1/2016 by CJC: Add "raw"-netCDF routines
    !!
    !!      Version  4/2017 by CJC: Bug-fixes in 4-D routines; add "read-timestep"
    !!      routines; eliminate NetCDF-2 EXTERNALs for "g95"
    !!
    !!      Version 05/2017 by CJC:  fix bug in handling of time-dimension,
    !!      bugs in error-messages.
    !!........................................................................
    !!
    !!  Compile with preprocessor definition "-DIOAPI_NCF4=1" for netCDF-4 INTEGER*8 support
    !!
    !!  Compile with preprocessor definition "-DIOAPI_PNCF=1" for pNetCDF parallel-I/O support
    !!
    !!  DO NOT EDIT !!!!
    !!
    !!        The EDSS/Models-3 I/O API depends in an essential manner
    !!        upon the contents of this MODULE file.  ANY CHANGES are
    !!        likely to result in very obscure, difficult-to-diagnose
    !!        bugs caused by an inconsistency between standard "libioapi.a"
    !!        object-libraries and whatever code is compiled with the
    !!        resulting modified MODULE-file.
    !!
    !!        By making any changes to this MODULE file, the user
    !!        explicitly agrees that in the case any assistance is
    !!        required of MCNC or of the I/O API author, Carlie J. Coats, Jr.
    !!        THE USER AND/OR HIS PROJECT OR CONTRACT AGREES TO REIMBURSE
    !!        UNC AND/OR THE I/O API AUTHOR, CARLIE J. COATS, JR., AT A
    !!        RATE TRIPLE THE NORMAL CONTRACT RATE FOR THE SERVICES
    !!        REQUIRED.
    !!
    !!...................................................................................

    IMPLICIT NONE


    !!--------  Public Routines in this module:  -----------------------

    PUBLIC  :: CREATENC, DESCNCVAR, READNCVAR, WRITENCVAR


    !!--------  Generic Interfaces:  -----------------------------------

    INTERFACE READNCVAR
        MODULE PROCEDURE READNCVAR0DR,  READNCVAR0DI,  READNCVAR0DS,  READNCVAR0DB,  READNCVAR0DD,    &
                         READNCVAR1DR,  READNCVAR1DI,  READNCVAR1DS,  READNCVAR1DB,  READNCVAR1DD,    &
                         READNCVAR2DR,  READNCVAR2DI,  READNCVAR2DS,  READNCVAR2DB,  READNCVAR2DD,    &
                         READNCVAR3DR,  READNCVAR3DI,  READNCVAR3DS,  READNCVAR3DB,  READNCVAR3DD,    &
                         READNCVAR4DR,  READNCVAR4DI,  READNCVAR4DS,  READNCVAR4DB,  READNCVAR4DD,    &
                         READNCVEC2DR,  READNCVEC2DI,  READNCVEC2DS,  READNCVEC2DB,  READNCVEC2DD,    &
                         READNCVEC3DR,  READNCVEC3DI,  READNCVEC3DS,  READNCVEC3DB,  READNCVEC3DD,    &
                         READNCVEC4DR,  READNCVEC4DI,  READNCVEC4DS,  READNCVEC4DB,  READNCVEC4DD,    &
                         READNVSTEP0DR, READNVSTEP0DI, READNVSTEP0DS, READNVSTEP0DB, READNVSTEP0DD,   &
                         READNVSTEP1DR, READNVSTEP1DI, READNVSTEP1DS, READNVSTEP1DB, READNVSTEP1DD,   &
                         READNVSTEP2DR, READNVSTEP2DI, READNVSTEP2DS, READNVSTEP2DB, READNVSTEP2DD,   &
                         READNVSTEP3DR, READNVSTEP3DI, READNVSTEP3DS, READNVSTEP3DB, READNVSTEP3DD,   &
                         READNVSTEP4DR, READNVSTEP4DI, READNVSTEP4DS, READNVSTEP4DB, READNVSTEP4DD
    END INTERFACE READNCVAR


    INTERFACE WRITENCVAR
        MODULE PROCEDURE WRITENCVAR0DR,  WRITENCVAR0DI,  WRITENCVAR0DS,  WRITENCVAR0DB,  WRITENCVAR0DD,    &
                         WRITENCVAR1DR,  WRITENCVAR1DI,  WRITENCVAR1DS,  WRITENCVAR1DB,  WRITENCVAR1DD,    &
                         WRITENCVAR2DR,  WRITENCVAR2DI,  WRITENCVAR2DS,  WRITENCVAR2DB,  WRITENCVAR2DD,    &
                         WRITENCVAR3DR,  WRITENCVAR3DI,  WRITENCVAR3DS,  WRITENCVAR3DB,  WRITENCVAR3DD,    &
                         WRITENCVAR4DR,  WRITENCVAR4DI,  WRITENCVAR4DS,  WRITENCVAR4DB,  WRITENCVAR4DD,    &
                         WRITENCVEC2DR,  WRITENCVEC2DI,  WRITENCVEC2DS,  WRITENCVEC2DB,  WRITENCVEC2DD,    &
                         WRITENCVEC3DR,  WRITENCVEC3DI,  WRITENCVEC3DS,  WRITENCVEC3DB,  WRITENCVEC3DD,    &
                         WRITENCVEC4DR,  WRITENCVEC4DI,  WRITENCVEC4DS,  WRITENCVEC4DB,  WRITENCVEC4DD,    &
                         WRITENVSTEP0DR, WRITENVSTEP0DI, WRITENVSTEP0DS, WRITENVSTEP0DB, WRITENVSTEP0DD,   &
                         WRITENVSTEP1DR, WRITENVSTEP1DI, WRITENVSTEP1DS, WRITENVSTEP1DB, WRITENVSTEP1DD,   &
                         WRITENVSTEP2DR, WRITENVSTEP2DI, WRITENVSTEP2DS, WRITENVSTEP2DB, WRITENVSTEP2DD,   &
                         WRITENVSTEP3DR, WRITENVSTEP3DI, WRITENVSTEP3DS, WRITENVSTEP3DB, WRITENVSTEP3DD,   &
                         WRITENVSTEP4DR, WRITENVSTEP4DI, WRITENVSTEP4DS, WRITENVSTEP4DB, WRITENVSTEP4DD
    END INTERFACE WRITENCVAR


    !!--------  Contents of "NETCDF.EXT"    ------------------------------
    !! netCDF version 3 fortran interface:
    !! external netcdf data types:

    INTEGER, PARAMETER :: nf_byte     =  1
    INTEGER, PARAMETER :: nf_int1     =  nf_byte
    INTEGER, PARAMETER :: nf_char     =  2
    INTEGER, PARAMETER :: nf_short    =  3
    INTEGER, PARAMETER :: nf_int2     =  nf_short
    INTEGER, PARAMETER :: nf_int      =  4
    INTEGER, PARAMETER :: nf_float    =  5
    INTEGER, PARAMETER :: nf_real     =  nf_float
    INTEGER, PARAMETER :: nf_double   =  6
    INTEGER, PARAMETER :: nf_ubyte    =  7
    INTEGER, PARAMETER :: nf_ushort   =  8
    INTEGER, PARAMETER :: nf_uint     =  9
    INTEGER, PARAMETER :: nf_int64    = 10
    INTEGER, PARAMETER :: nf_uint64   = 11
    INTEGER, PARAMETER :: nf_string   = 12
    INTEGER, PARAMETER :: nf_vlen     = 13
    INTEGER, PARAMETER :: nf_opaque   = 14
    INTEGER, PARAMETER :: nf_enum     = 15
    INTEGER, PARAMETER :: nf_compound = 16


    !!........ default fill values:

    INTEGER, PARAMETER :: nf_fill_byte   = -127
    INTEGER, PARAMETER :: nf_fill_int1   = nf_fill_byte
    INTEGER, PARAMETER :: nf_fill_char   = 0
    INTEGER, PARAMETER :: nf_fill_short  = -32767
    INTEGER, PARAMETER :: nf_fill_int2   =  nf_fill_short
    INTEGER, PARAMETER :: nf_fill_int    = -2147483647
    REAL   , PARAMETER :: nf_fill_float  = 9.9692099683868690e+36
    REAL   , PARAMETER :: nf_fill_real   = nf_fill_float
    REAL*8 , PARAMETER :: nf_fill_double = 9.9692099683868690e+36
    INTEGER, PARAMETER :: nf_fill_ubyte  = 255
    INTEGER, PARAMETER :: nf_fill_ushort = 65535


    !!........ mode flags for opening and creating a netcdf dataset:

    INTEGER, PARAMETER :: nf_nowrite          =    0
    INTEGER, PARAMETER :: nf_write            =    1
    INTEGER, PARAMETER :: nf_clobber          =    0
    INTEGER, PARAMETER :: nf_noclobber        =    4
    INTEGER, PARAMETER :: nf_fill             =    0
    INTEGER, PARAMETER :: nf_nofill           =  256
    INTEGER, PARAMETER :: nf_lock             = 1024
    INTEGER, PARAMETER :: nf_share            = 2048
    INTEGER, PARAMETER :: nf_64bit_offset     =  512
    INTEGER, PARAMETER :: nf_sizehint_default =    0
    INTEGER, PARAMETER :: nf_align_chunk      =   -1
    INTEGER, PARAMETER :: nf_format_classic   =    1
    INTEGER, PARAMETER :: nf_format_64bit     =    2
    INTEGER, PARAMETER :: nf_format_netcdf4   =    3
    INTEGER, PARAMETER :: nf_format_netcdf4_classic = 4


    !!........ size argument for defining an unlimited dimension:

    INTEGER, PARAMETER :: nf_unlimited = 0


    !!........ global attribute id:

    INTEGER, PARAMETER :: nf_global = 0


    !!........ implementation limits:

    INTEGER, PARAMETER :: nf_max_dims  = 1024
    INTEGER, PARAMETER :: nf_max_attrs = 8192
    INTEGER, PARAMETER :: nf_max_vars  = 8192
    INTEGER, PARAMETER :: nf_max_name  =  256
    INTEGER, PARAMETER :: nf_max_var_dims = nf_max_dims

    !!........ error codes:

    INTEGER, PARAMETER :: nf_noerr        =   0
    INTEGER, PARAMETER :: nf_ebadid       = -33
    INTEGER, PARAMETER :: nf_eexist       = -35
    INTEGER, PARAMETER :: nf_einval       = -36
    INTEGER, PARAMETER :: nf_eperm        = -37
    INTEGER, PARAMETER :: nf_enotindefine = -38
    INTEGER, PARAMETER :: nf_eindefine    = -39
    INTEGER, PARAMETER :: nf_einvalcoords = -40
    INTEGER, PARAMETER :: nf_emaxdims     = -41
    INTEGER, PARAMETER :: nf_enameinuse   = -42
    INTEGER, PARAMETER :: nf_enotatt      = -43
    INTEGER, PARAMETER :: nf_emaxatts     = -44
    INTEGER, PARAMETER :: nf_ebadtype     = -45
    INTEGER, PARAMETER :: nf_ebaddim      = -46
    INTEGER, PARAMETER :: nf_eunlimpos    = -47
    INTEGER, PARAMETER :: nf_emaxvars     = -48
    INTEGER, PARAMETER :: nf_enotvar      = -49
    INTEGER, PARAMETER :: nf_eglobal      = -50
    INTEGER, PARAMETER :: nf_enotnc       = -51
    INTEGER, PARAMETER :: nf_ests         = -52
    INTEGER, PARAMETER :: nf_emaxname     = -53
    INTEGER, PARAMETER :: nf_eunlimit     = -54
    INTEGER, PARAMETER :: nf_enorecvars   = -55
    INTEGER, PARAMETER :: nf_echar        = -56
    INTEGER, PARAMETER :: nf_eedge        = -57
    INTEGER, PARAMETER :: nf_estride      = -58
    INTEGER, PARAMETER :: nf_ebadname     = -59
    INTEGER, PARAMETER :: nf_erange       = -60
    INTEGER, PARAMETER :: nf_enomem       = -61
    INTEGER, PARAMETER :: nf_evarsize     = -62
    INTEGER, PARAMETER :: nf_edimsize     = -63
    INTEGER, PARAMETER :: nf_etrunc       = -64

    !!........ error handling modes:

    INTEGER, PARAMETER :: nf_fatal   = 1
    INTEGER, PARAMETER :: nf_verbose = 2


    !!........ miscellaneous routines:

    CHARACTER*80, EXTERNAL :: nf_inq_libvers
    CHARACTER*80, EXTERNAL :: nf_strerror
    LOGICAL,      EXTERNAL :: nf_issyserr


    !!........ control routines:

    INTEGER, EXTERNAL :: nf_inq_base_pe
    !!........                         (integer             ncid,
    !!........                          integer             pe)

    INTEGER, EXTERNAL :: nf_set_base_pe
    !!........                         (integer             ncid,
    !!........                          integer             pe)

    INTEGER, EXTERNAL :: nf_create
    !!........                         (character*(*)       path,
    !!........                          integer             cmode,
    !!........                          integer             ncid)

    INTEGER, EXTERNAL :: nf__create
    !!........                         (character*(*)       path,
    !!........                          integer             cmode,
    !!........                          integer             initialsz,
    !!........                          integer             chunksizehint,
    !!........                          integer             ncid)

    INTEGER, EXTERNAL :: nf__create_mp
    !!........                         (character*(*)       path,
    !!........                          integer             cmode,
    !!........                          integer             initialsz,
    !!........                          integer             basepe,
    !!........                          integer             chunksizehint,
    !!........                          integer             ncid)

    INTEGER, EXTERNAL :: nf_open
    !!........                         (character*(*)       path,
    !!........                          integer             mode,
    !!........                          integer             ncid)

    INTEGER, EXTERNAL :: nf__open
    !!........                         (character*(*)       path,
    !!........                          integer             mode,
    !!........                          integer             chunksizehint,
    !!........                          integer             ncid)

    INTEGER, EXTERNAL :: nf__open_mp
    !!........                         (character*(*)       path,
    !!........                          integer             mode,
    !!........                          integer             basepe,
    !!........                          integer             chunksizehint,
    !!........                          integer             ncid)

    INTEGER, EXTERNAL :: nf_set_fill
    !!........                         (integer             ncid,
    !!........                          integer             fillmode,
    !!........                          integer             old_mode)

    INTEGER, EXTERNAL :: nf_set_default_format
    !!........                          (integer             format,
    !!........                          integer             old_format)

    INTEGER, EXTERNAL :: nf_redef
    !!........                         (integer             ncid)

    INTEGER, EXTERNAL :: nf_enddef
    !!........                         (integer             ncid)

    INTEGER, EXTERNAL :: nf__enddef
    !!........                         (integer             ncid,
    !!........                          integer             h_minfree,
    !!........                          integer             v_align,
    !!........                          integer             v_minfree,
    !!........                          integer             r_align)

    INTEGER, EXTERNAL :: nf_sync
    !!........                         (integer             ncid)

    INTEGER, EXTERNAL :: nf_abort
    !!........                         (integer             ncid)

    INTEGER, EXTERNAL :: nf_close
    !!........                         (integer             ncid)

    INTEGER, EXTERNAL :: nf_delete
    !!........                         (character*(*)       ncid)


    !!........ general inquiry routines:


    INTEGER, EXTERNAL :: nf_inq
    !!........                         (INTEGER ::     ncid,
    !!........                          INTEGER ::     ndims,
    !!........                          INTEGER ::     nvars,
    !!........                          INTEGER ::     ngatts,
    !!........                          INTEGER ::     unlimdimid)

    INTEGER, EXTERNAL :: nf_inq_ndims
    !!........                         (INTEGER ::     ncid,
    !!........                          INTEGER ::     ndims)

    INTEGER, EXTERNAL :: nf_inq_nvars
    !!........                         (INTEGER ::     ncid,
    !!........                          INTEGER ::     nvars)

    INTEGER, EXTERNAL :: nf_inq_natts
    !!........                         (INTEGER ::     ncid,
    !!........                          INTEGER ::     ngatts)

    INTEGER, EXTERNAL :: nf_inq_unlimdim
    !!........                         (INTEGER ::     ncid,
    !!........                          INTEGER ::     unlimdimid)

    INTEGER, EXTERNAL :: nf_inq_format
    !!........                         (INTEGER ::     ncid,
    !!........                          INTEGER ::     format)


    !!........ dimension routines:


    INTEGER, EXTERNAL :: nf_def_dim
    !!........                         (INTEGER ::     ncid,
    !!........                          character(*):: name,
    !!........                          INTEGER ::     len,
    !!........                          INTEGER ::     dimid)

    INTEGER, EXTERNAL :: nf_inq_dimid
    !!........                         (INTEGER ::     ncid,
    !!........                          character(*):: name,
    !!........                          INTEGER ::     dimid)

    INTEGER, EXTERNAL :: nf_inq_dim
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     dimid,
    !!........                          character(*):: name,
    !!........                         (INTEGER ::     len)

    INTEGER, EXTERNAL :: nf_inq_dimname
    !!........                         (INTEGER ::      ncid,
    !!........                         (INTEGER ::      dimid,
    !!........                          character(*) :: name)

    INTEGER, EXTERNAL :: nf_inq_dimlen
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     dimid,
    !!........                         (INTEGER ::     len)

    INTEGER, EXTERNAL :: nf_rename_dim
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     dimid,
    !!........                          character(*):: name)


    !!........ general attribute routines:


    INTEGER, EXTERNAL :: nf_inq_att
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          character(*):: name,
    !!........                         (INTEGER ::     xtype,
    !!........                         (INTEGER ::     len)

    INTEGER, EXTERNAL :: nf_inq_attid
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          character(*):: name,
    !!........                         (INTEGER ::     attnum)

    INTEGER, EXTERNAL :: nf_inq_atttype
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          character(*):: name,
    !!........                         (INTEGER ::     xtype)

    INTEGER, EXTERNAL :: nf_inq_attlen
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          character(*):: name,
    !!........                         (INTEGER ::     len)

    INTEGER, EXTERNAL :: nf_inq_attname
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     attnum,
    !!........                          character(*):: name)

    INTEGER, EXTERNAL :: nf_copy_att
    !!........                         (INTEGER ::     ncid_in,
    !!........                         (INTEGER ::     varid_in,
    !!........                          character(*):: name,
    !!........                         (INTEGER ::     ncid_out,
    !!........                         (INTEGER ::     varid_out)

    INTEGER, EXTERNAL :: nf_rename_att
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          character(*):: curname,
    !!........                          character(*):: newname)

    INTEGER, EXTERNAL :: nf_del_att
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          character(*):: name)


    !!........ attribute put/get routines:


    INTEGER, EXTERNAL :: nf_put_att_text
    !!........                         (INTEGER ::     ncid,
    !!........                          INTEGER ::     varid,
    !!........                          character(*):: name,
    !!........                          INTEGER ::     len,
    !!........                          character(*):: text)

    INTEGER, EXTERNAL :: nf_get_att_text
    !!........                         (INTEGER ::     ncid,
    !!........                          INTEGER ::     varid,
    !!........                          character(*):: name,
    !!........                          character(*):: text)

    INTEGER, EXTERNAL :: nf_put_att_int1
    !!........                         (INTEGER ::     ncid,
    !!........                          INTEGER ::     varid,
    !!........                          character(*):: name,
    !!........                          INTEGER ::     xtype,
    !!........                          INTEGER ::     len,
    !!........                          nf_int1_t ::   i1vals(*))

    INTEGER, EXTERNAL :: nf_get_att_int1
    !!........                         (INTEGER ::     ncid,
    !!........                          INTEGER ::     varid,
    !!........                          character(*):: name,
    !!........                          nf_int1_t ::   i1vals(*))

    INTEGER, EXTERNAL :: nf_put_att_int2
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          character(*):: name,
    !!........                         (INTEGER ::     xtype,
    !!........                         (INTEGER ::     len,
    !!........                          nf_int2_t ::   i2vals(*))

    INTEGER, EXTERNAL :: nf_get_att_int2
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          character(*):: name,
    !!........                          nf_int2_t ::   i2vals(*))

    INTEGER, EXTERNAL :: nf_put_att_int
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          character(*):: name,
    !!........                         (INTEGER ::     xtype,
    !!........                         (INTEGER ::     len,
    !!........                         (INTEGER ::     ivals(*))

    INTEGER, EXTERNAL :: nf_get_att_int
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          character(*):: name,
    !!........                         (INTEGER ::     ivals(*))

    INTEGER, EXTERNAL :: nf_put_att_real
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          character(*):: name,
    !!........                         (INTEGER ::     xtype,
    !!........                         (INTEGER ::     len,
    !!........                          real                rvals(*))

    INTEGER, EXTERNAL :: nf_get_att_real
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          character(*):: name,

    INTEGER, EXTERNAL :: nf_put_att_double
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          character(*):: name,
    !!........                         (INTEGER ::     xtype,
    !!........                         (INTEGER ::     len,
    !!........                          double              dvals(*))

    INTEGER, EXTERNAL :: nf_get_att_double
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          character(*):: name,
    !!........                          double              dvals(*))


    !!........ general variable routines:


    INTEGER, EXTERNAL :: nf_def_var
    !!........                         (INTEGER ::     ncid,
    !!........                          character(*):: name,
    !!........                         (INTEGER ::     datatype,
    !!........                         (INTEGER ::     ndims,
    !!........                         (INTEGER ::     dimids(*),
    !!........                         (INTEGER ::     varid)

    INTEGER, EXTERNAL :: nf_inq_var
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          character(*):: name,
    !!........                         (INTEGER ::     datatype,
    !!........                         (INTEGER ::     ndims,
    !!........                         (INTEGER ::     dimids(*),
    !!........                         (INTEGER ::     natts)

    INTEGER, EXTERNAL :: nf_inq_varid
    !!........                         (INTEGER ::     ncid,
    !!........                          character(*):: name,
    !!........                         (INTEGER ::     varid)

    INTEGER, EXTERNAL :: nf_inq_varname
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          character(*):: name)

    INTEGER, EXTERNAL :: nf_inq_vartype
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     xtype)

    INTEGER, EXTERNAL :: nf_inq_varndims
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     ndims)

    INTEGER, EXTERNAL :: nf_inq_vardimid
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     dimids(*))

    INTEGER, EXTERNAL :: nf_inq_varnatts
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     natts)

    INTEGER, EXTERNAL :: nf_rename_var
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          character(*):: name)

    INTEGER, EXTERNAL :: nf_copy_var
    !!........                         (INTEGER ::     ncid_in,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     ncid_out)


    !!........ entire variable put/get routines:


    INTEGER, EXTERNAL :: nf_put_var_text
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          character(*):: text)

    INTEGER, EXTERNAL :: nf_get_var_text
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          character(*):: text)

    INTEGER, EXTERNAL :: nf_put_var_int1
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          nf_int1_t ::   i1vals(*))

    INTEGER, EXTERNAL :: nf_get_var_int1
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          nf_int1_t ::   i1vals(*))

    INTEGER, EXTERNAL :: nf_put_var_int2
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          nf_int2_t ::   i2vals(*))

    INTEGER, EXTERNAL :: nf_get_var_int2
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          nf_int2_t ::   i2vals(*))

    INTEGER, EXTERNAL :: nf_put_var_int
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     ivals(*))

    INTEGER, EXTERNAL :: nf_get_var_int
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     ivals(*))

    INTEGER, EXTERNAL :: nf_put_var_real
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          real                rvals(*))

    INTEGER, EXTERNAL :: nf_get_var_real
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          real                rvals(*))

    INTEGER, EXTERNAL :: nf_put_var_double
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          doubleprecision     dvals(*))

    INTEGER, EXTERNAL :: nf_get_var_double
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          doubleprecision     dvals(*))

    INTEGER, EXTERNAL :: nf_put_var_int64
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          INTEGER*8 ::     ivals(*))

    INTEGER, EXTERNAL :: nf_get_var_int64
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                          INTEGER*8 ::     ivals(*))


    !!........ single variable put/get routines:


    INTEGER, EXTERNAL :: nf_put_var1_text
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     index(*),
    !!........                          character*1 :: text)

    INTEGER, EXTERNAL :: nf_get_var1_text
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     index(*),
    !!........                          character*1 :: text)

    INTEGER, EXTERNAL :: nf_put_var1_int1
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     index(*),
    !!........                          nf_int1_t ::   i1val)

    INTEGER, EXTERNAL :: nf_get_var1_int1
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     index(*),
    !!........                          nf_int1_t ::   i1val)

    INTEGER, EXTERNAL :: nf_put_var1_int2
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     index(*),
    !!........                          nf_int2_t ::   i2val)

    INTEGER, EXTERNAL :: nf_get_var1_int2
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     index(*),
    !!........                          nf_int2_t ::   i2val)

    INTEGER, EXTERNAL :: nf_put_var1_int
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     index(*),
    !!........                         (INTEGER ::     ival)

    INTEGER, EXTERNAL :: nf_get_var1_int
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     index(*),
    !!........                         (INTEGER ::     ival)

    INTEGER, EXTERNAL :: nf_put_var1_real
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     index(*),
    !!........                          real                rval)

    INTEGER, EXTERNAL :: nf_get_var1_real
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     index(*),
    !!........                          real                rval)

    INTEGER, EXTERNAL :: nf_put_var1_double
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     index(*),
    !!........                          doubleprecision     dval)

    INTEGER, EXTERNAL :: nf_get_var1_double
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     index(*),
    !!........                          doubleprecision     dval)

    INTEGER, EXTERNAL :: nf_put_var1_int64
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     index(*),
    !!........                          INTEGER*8, EXTERNAL ::     ival)

    INTEGER, EXTERNAL :: nf_get_var1_int64
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     index(*),
    !!........                          INTEGER*8, EXTERNAL ::     ival)

    !!........ variable array put/get routines:


    INTEGER, EXTERNAL :: nf_put_vara_text
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                          character(*):: text)

    INTEGER, EXTERNAL :: nf_get_vara_text
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                          character(*):: text)

    INTEGER, EXTERNAL :: nf_put_vara_int1
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                          nf_int1_t ::   i1vals(*))

    INTEGER, EXTERNAL :: nf_get_vara_int1
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                          nf_int1_t ::   i1vals(*))

    INTEGER, EXTERNAL :: nf_put_vara_int2
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                          nf_int2_t ::   i2vals(*))

    INTEGER, EXTERNAL :: nf_get_vara_int2
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                          nf_int2_t ::   i2vals(*))

    INTEGER, EXTERNAL :: nf_put_vara_int
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                         (INTEGER ::     ivals(*))

    INTEGER, EXTERNAL :: nf_get_vara_int
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                         (INTEGER ::     ivals(*))

    INTEGER, EXTERNAL :: nf_put_vara_real
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                          real                rvals(*))

    INTEGER, EXTERNAL :: nf_get_vara_real
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                          real                rvals(*))

    INTEGER, EXTERNAL :: nf_put_vara_double
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                          doubleprecision     dvals(*))

    INTEGER, EXTERNAL :: nf_get_vara_double
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                          doubleprecision :: dvals(*))

    INTEGER, EXTERNAL :: nf_put_vara_int64
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                          INTEGER*8 ::     ivals(*))

    INTEGER, EXTERNAL :: nf_get_vara_int64
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                          INTEGER*8 ::     ivals(*))


    !!........ strided variable put/get routines:


    INTEGER, EXTERNAL :: nf_put_vars_text
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                         (INTEGER ::     stride(*),
    !!........                          character(*) :: text)

    INTEGER, EXTERNAL :: nf_get_vars_text
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                         (INTEGER ::     stride(*),
    !!........                          character(*) :: text)

    INTEGER, EXTERNAL :: nf_put_vars_int1
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                         (INTEGER ::     stride(*),
    !!........                          nf_int1_t ::   i1vals(*))

    INTEGER, EXTERNAL :: nf_get_vars_int1
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                         (INTEGER ::     stride(*),
    !!........                          nf_int1_t ::   i1vals(*))

    INTEGER, EXTERNAL :: nf_put_vars_int2
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                         (INTEGER ::     stride(*),
    !!........                          nf_int2_t ::   i2vals(*))

    INTEGER, EXTERNAL :: nf_get_vars_int2
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                         (INTEGER ::     stride(*),
    !!........                          nf_int2_t ::   i2vals(*))

    INTEGER, EXTERNAL :: nf_put_vars_int
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                         (INTEGER ::     stride(*),
    !!........                         (INTEGER ::     ivals(*))

    INTEGER, EXTERNAL :: nf_get_vars_int
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                         (INTEGER ::     stride(*),
    !!........                         (INTEGER ::     ivals(*))

    INTEGER, EXTERNAL :: nf_put_vars_real
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                         (INTEGER ::     stride(*),
    !!........                          real                rvals(*))

    INTEGER, EXTERNAL :: nf_get_vars_real
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                         (INTEGER ::     stride(*),
    !!........                          real                rvals(*))

    INTEGER, EXTERNAL :: nf_put_vars_double
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                         (INTEGER ::     stride(*),
    !!........                          doubleprecision     dvals(*))

    INTEGER, EXTERNAL :: nf_get_vars_double
    !!........                         (INTEGER ::     ncid,
    !!........                          integer             varid,
    !!........                          integer             start(*),
    !!........                          integer             count(*),
    !!........                          integer             stride(*),
    !!........                          doubleprecision     dvals(*))

    INTEGER, EXTERNAL :: nf_put_vars_int64
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                         (INTEGER ::     stride(*),
    !!........                          INTEGER*8::     ivals(*))

    INTEGER, EXTERNAL :: nf_get_vars_int64
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                         (INTEGER ::     stride(*),
    !!........                          INTEGER*8::     ivals(*))


    !!........ mapped variable put/get routines:


    INTEGER, EXTERNAL :: nf_put_varm_text
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                         (INTEGER ::     stride(*),
    !!........                         (INTEGER ::     imap(*),
    !!........                          character(*):: text)

    INTEGER, EXTERNAL :: nf_get_varm_text
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                         (INTEGER ::     stride(*),
    !!........                         (INTEGER ::     imap(*),
    !!........                          character(*):: text)

    INTEGER, EXTERNAL :: nf_put_varm_int1
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                         (INTEGER ::     stride(*),
    !!........                         (INTEGER ::     imap(*),
    !!........                          nf_int1_t ::   i1vals(*))

    INTEGER, EXTERNAL :: nf_get_varm_int1
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                         (INTEGER ::     stride(*),
    !!........                         (INTEGER ::     imap(*),
    !!........                          nf_int1_t ::   i1vals(*))

    INTEGER, EXTERNAL :: nf_put_varm_int2
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                         (INTEGER ::     stride(*),
    !!........                         (INTEGER ::     imap(*),
    !!........                          nf_int2_t ::   i2vals(*))

    INTEGER, EXTERNAL :: nf_get_varm_int2
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                         (INTEGER ::     stride(*),
    !!........                         (INTEGER ::     imap(*),
    !!........                          nf_int2_t ::   i2vals(*))

    INTEGER, EXTERNAL :: nf_put_varm_int
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                         (INTEGER ::     stride(*),
    !!........                         (INTEGER ::     imap(*),
    !!........                         (INTEGER ::     ivals(*))

    INTEGER, EXTERNAL :: nf_get_varm_int
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                         (INTEGER ::     stride(*),
    !!........                         (INTEGER ::     imap(*),
    !!........                         (INTEGER ::     ivals(*))

    INTEGER, EXTERNAL :: nf_put_varm_real
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                         (INTEGER ::     stride(*),
    !!........                         (INTEGER ::     imap(*),
    !!........                          real                rvals(*))

    INTEGER, EXTERNAL :: nf_get_varm_real
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                         (INTEGER ::     stride(*),
    !!........                         (INTEGER ::     imap(*),
    !!........                          real                rvals(*))

    INTEGER, EXTERNAL :: nf_put_varm_double
    !!........                         (integer             ncid,
    !!........                          integer             varid,
    !!........                          integer             start(*),
    !!........                          integer             count(*),
    !!........                          integer             stride(*),
    !!........                          integer             imap(*),
    !!........                          doubleprecision     dvals(*))

    INTEGER, EXTERNAL :: nf_get_varm_double
    !!........                         (INTEGER ::     ncid,
    !!........                          integer             varid,
    !!........                          integer             start(*),
    !!........                          integer             count(*),
    !!........                          integer             stride(*),
    !!........                          integer             imap(*),
    !!........                          doubleprecision     dvals(*))

    INTEGER, EXTERNAL :: nf_put_varm_int64
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                         (INTEGER ::     stride(*),
    !!........                         (INTEGER ::     imap(*),
    !!........                          INTEGER*8, EXTERNAL ::     ivals(*))

    INTEGER, EXTERNAL :: nf_get_varm_int64
    !!........                         (INTEGER ::     ncid,
    !!........                         (INTEGER ::     varid,
    !!........                         (INTEGER ::     start(*),
    !!........                         (INTEGER ::     count(*),
    !!........                         (INTEGER ::     stride(*),
    !!........                         (INTEGER ::     imap(*),
    !!........                          INTEGER*8, EXTERNAL ::     ivals(*))

    !!........ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    !!........ begin netcdf 2.4 backward compatibility:

    !!........ functions in the fortran interface

!!  INTEGER, EXTERNAL :: nccre
!!  INTEGER, EXTERNAL :: ncopn
!!  INTEGER, EXTERNAL :: ncddef
!!  INTEGER, EXTERNAL :: ncdid
!!  INTEGER, EXTERNAL :: ncvdef
!!  INTEGER, EXTERNAL :: ncvid
!!  INTEGER, EXTERNAL :: nctlen
!!  INTEGER, EXTERNAL :: ncsfil

    !!........ netcdf data types:

    INTEGER, PARAMETER :: ncbyte   = nf_byte      !! = 1
    INTEGER, PARAMETER :: ncchar   = nf_char      !! = 2
    INTEGER, PARAMETER :: ncshort  = nf_short     !! = 3
    INTEGER, PARAMETER :: nclong   = nf_int       !! =  4
    INTEGER, PARAMETER :: ncfloat  = nf_float     !! =  5
    INTEGER, PARAMETER :: ncdouble = nf_double    !! =  6


    !!........     masks for the struct nc flag field; passed in as 'mode' arg to
    !!........     nccreate and ncopen.
    !!........     read/write, 0 => readonly
    !!........     in create phase, cleared by ncendef
    !!........     on create destroy existing file
    !!........     in define mode, cleared by ncendef
    !!........     synchronise numrecs on change (x'10')
    !!........     synchronise whole header on change (x'20')
    !!........     numrecs has changed (x'40')
    !!........     header info has changed (x'80')
    !!........     prefill vars on endef and increase of record, the default behavior
    !!........     do not fill vars on endef and increase of record (x'100')
    !!........     isa link (x'8000')

    INTEGER, PARAMETER :: ncrdwr   = nf_write       !! = 1
    INTEGER, PARAMETER :: nccreat  =     2
    INTEGER, PARAMETER :: ncexcl   = nf_noclobber   !! = 4
    INTEGER, PARAMETER :: ncindef  =     8
    INTEGER, PARAMETER :: ncnsync  =    16
    INTEGER, PARAMETER :: nchsync  =    32
    INTEGER, PARAMETER :: ncndirty =    64
    INTEGER, PARAMETER :: nchdirty =   128
    INTEGER, PARAMETER :: ncfill   = nf_fill        !! =    0
    INTEGER, PARAMETER :: ncnofill = nf_nofill      !! =  256
    INTEGER, PARAMETER :: nclink   = 32768


    !!........     'mode' arguments for nccreate and ncopen

    INTEGER, PARAMETER :: ncnowrit = nf_nowrite     !! = 0
    INTEGER, PARAMETER :: ncwrite  = ncrdwr
    INTEGER, PARAMETER :: ncclob   = nf_clobber
    INTEGER, PARAMETER :: ncnoclob = nf_noclobber


    !!........     'size' argument to ncdimdef for an unlimited dimension

    INTEGER, PARAMETER :: ncunlim = nf_unlimited    !! = 0


    !!........     attribute id to put/get a global attribute

    INTEGER, PARAMETER :: ncglobal  = 0


    !!........     advisory maximums:

    INTEGER, PARAMETER :: maxncop  = 64
    INTEGER, PARAMETER :: maxncdim = nf_max_dims      !! = 1024
    INTEGER, PARAMETER :: maxncatt = nf_max_attrs     !! = 8192
    INTEGER, PARAMETER :: maxncvar = nf_max_vars      !! = 8192
    !!........     not enforced
    INTEGER, PARAMETER :: maxncnam = nf_max_name      !! =  256
    INTEGER, PARAMETER :: maxvdims = maxncdim


    !!........     global netcdf error status variable
    !!........     initialized in error.c

    INTEGER, PARAMETER :: ncnoerr  = nf_noerr
    INTEGER, PARAMETER :: ncebadid = nf_ebadid
    INTEGER, PARAMETER :: ncenfile = -31   ! nc_syserr
    INTEGER, PARAMETER :: nceexist = nf_eexist
    INTEGER, PARAMETER :: nceinval = nf_einval
    INTEGER, PARAMETER :: nceperm  = nf_eperm
    INTEGER, PARAMETER :: ncenotin = nf_enotindefine
    INTEGER, PARAMETER :: nceindef = nf_eindefine
    INTEGER, PARAMETER :: ncecoord = nf_einvalcoords
    INTEGER, PARAMETER :: ncemaxds = nf_emaxdims
    INTEGER, PARAMETER :: ncename  = nf_enameinuse
    INTEGER, PARAMETER :: ncenoatt = nf_enotatt
    INTEGER, PARAMETER :: ncemaxat = nf_emaxatts
    INTEGER, PARAMETER :: ncebadty = nf_ebadtype
    INTEGER, PARAMETER :: ncebadd  = nf_ebaddim
    INTEGER, PARAMETER :: nceunlim = nf_eunlimpos
    INTEGER, PARAMETER :: ncemaxvs = nf_emaxvars
    INTEGER, PARAMETER :: ncenotvr = nf_enotvar
    INTEGER, PARAMETER :: nceglob  = nf_eglobal
    INTEGER, PARAMETER :: ncenotnc = nf_enotnc
    INTEGER, PARAMETER :: ncests   = nf_ests
    INTEGER, PARAMETER :: ncentool = nf_emaxname
    INTEGER, PARAMETER :: ncfoobar =  32
    INTEGER, PARAMETER :: ncsyserr = -31

    !!........     global options variable. used to determine behavior of error handler.
    !!........     initialized in lerror.c

    INTEGER, PARAMETER :: ncfatal  = nf_fatal   !! = 1
    INTEGER, PARAMETER :: ncverbos = nf_verbose !! = 2

    !!........     default fill values.  these must be the same as in the c interface.

    INTEGER, PARAMETER :: filbyte  = nf_fill_byte       !! = -127
    INTEGER, PARAMETER :: filchar  = nf_fill_char       !! = 0
    INTEGER, PARAMETER :: filshort = nf_fill_short      !! = -32767
    INTEGER, PARAMETER :: fillong  = nf_fill_int        !! = -2147483647
    REAL   , PARAMETER :: filfloat = nf_fill_float      !! = 9.9692099683868690e+36
    REAL*8 , PARAMETER :: fildoub  = nf_fill_double     !! = 9.9692099683868690e+36


    !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !!--------  Contents of "pnetcdf.inc"    ---------------------------------

#ifdef IOAPI_PNCF

    !!......   pnetcdf fortran defines
    !!......   PnetCDF library version numbers

    integer, PARAMETER :: PNETCDF_VERSION_MAJOR = 1
    integer, PARAMETER :: PNETCDF_VERSION_MINOR = 6
    integer, PARAMETER :: PNETCDF_VERSION_SUB   = 1

    !!......   size argument for defining an unlimited dimension:

    integer*8, parameter :: nfmpi_unlimited = 0

    !!......   PnetCDF APIs
    !!......   miscellaneous routines:

    CHARACTER*80, EXTERNAL :: NFMPI_INQ_LIBVERS
    CHARACTER*80, EXTERNAL :: NFMPI_STRERROR
    LOGICAL     , EXTERNAL :: NFMPI_ISSYSERR

    !!......   control routines:

    INTEGER, EXTERNAL :: nfmpi_create
    INTEGER, EXTERNAL :: nfmpi_open
    INTEGER, EXTERNAL :: nfmpi_inq_format
    INTEGER, EXTERNAL :: nfmpi_inq_file_format
    INTEGER, EXTERNAL :: nfmpi_inq_file_info
    INTEGER, EXTERNAL :: nfmpi_get_file_info
    INTEGER, EXTERNAL :: nfmpi_delete
    INTEGER, EXTERNAL :: nfmpi_enddef
    INTEGER, EXTERNAL :: nfmpi__enddef
    INTEGER, EXTERNAL :: nfmpi_redef
    INTEGER, EXTERNAL :: nfmpi_set_default_format
    INTEGER, EXTERNAL :: nfmpi_inq_default_format
    INTEGER, EXTERNAL :: nfmpi_sync
    INTEGER, EXTERNAL :: nfmpi_abort
    INTEGER, EXTERNAL :: nfmpi_close
    INTEGER, EXTERNAL :: nfmpi_set_fill
    INTEGER, EXTERNAL :: nfmpi_def_var_fill
    INTEGER, EXTERNAL :: nfmpi_inq_var_fill
    INTEGER, EXTERNAL :: nfmpi_fill_var_rec

    !!......   general inquiry routines:

    INTEGER, EXTERNAL :: nfmpi_inq
    INTEGER, EXTERNAL :: nfmpi_inq_ndims
    INTEGER, EXTERNAL :: nfmpi_inq_nvars
    INTEGER, EXTERNAL :: nfmpi_inq_num_rec_vars
    INTEGER, EXTERNAL :: nfmpi_inq_num_fix_vars
    INTEGER, EXTERNAL :: nfmpi_inq_natts
    INTEGER, EXTERNAL :: nfmpi_inq_unlimdim
    INTEGER, EXTERNAL :: nfmpi_inq_striping
    INTEGER, EXTERNAL :: nfmpi_inq_malloc_size
    INTEGER, EXTERNAL :: nfmpi_inq_malloc_max_size
    INTEGER, EXTERNAL :: nfmpi_inq_malloc_list
    INTEGER, EXTERNAL :: nfmpi_inq_files_opened
    INTEGER, EXTERNAL :: nfmpi_inq_recsize

    !!......   dimension routines:EXTERNAL :: nfmpi_inq_recsize

    INTEGER, EXTERNAL :: nfmpi_def_dim
    INTEGER, EXTERNAL :: nfmpi_inq_dimid
    INTEGER, EXTERNAL :: nfmpi_inq_dim
    INTEGER, EXTERNAL :: nfmpi_inq_dimname
    INTEGER, EXTERNAL :: nfmpi_inq_dimlen
    INTEGER, EXTERNAL :: nfmpi_rename_dim

    !!......   general attribute routines:

    INTEGER, EXTERNAL :: nfmpi_inq_att
    INTEGER, EXTERNAL :: nfmpi_inq_attid
    INTEGER, EXTERNAL :: nfmpi_inq_atttype
    INTEGER, EXTERNAL :: nfmpi_inq_attlen
    INTEGER, EXTERNAL :: nfmpi_inq_attname
    INTEGER, EXTERNAL :: nfmpi_copy_att
    INTEGER, EXTERNAL :: nfmpi_rename_att
    INTEGER, EXTERNAL :: nfmpi_del_att

    !!......   attribute put/get routines:

    INTEGER, EXTERNAL :: nfmpi_put_att,        nfmpi_get_att
    INTEGER, EXTERNAL :: nfmpi_put_att_text,   nfmpi_get_att_text
    INTEGER, EXTERNAL :: nfmpi_put_att_int1,   nfmpi_get_att_int1
    INTEGER, EXTERNAL :: nfmpi_put_att_int2,   nfmpi_get_att_int2
    INTEGER, EXTERNAL :: nfmpi_put_att_int,    nfmpi_get_att_int
    INTEGER, EXTERNAL :: nfmpi_put_att_real,   nfmpi_get_att_real
    INTEGER, EXTERNAL :: nfmpi_put_att_double, nfmpi_get_att_double
    INTEGER, EXTERNAL :: nfmpi_put_att_int8,   nfmpi_get_att_int8

    !!......   independent data mode routines:

    INTEGER, EXTERNAL :: nfmpi_begin_indep_data
    INTEGER, EXTERNAL :: nfmpi_end_indep_data

    !!......   general variable routines:

    INTEGER, EXTERNAL :: nfmpi_def_var
    INTEGER, EXTERNAL :: nfmpi_inq_var
    INTEGER, EXTERNAL :: nfmpi_inq_varid
    INTEGER, EXTERNAL :: nfmpi_inq_varname
    INTEGER, EXTERNAL :: nfmpi_inq_vartype
    INTEGER, EXTERNAL :: nfmpi_inq_varndims
    INTEGER, EXTERNAL :: nfmpi_inq_vardimid
    INTEGER, EXTERNAL :: nfmpi_inq_varnatts
    INTEGER, EXTERNAL :: nfmpi_rename_var

    !!......   entire variable put/get routines:

    INTEGER, EXTERNAL :: nfmpi_put_var
    INTEGER, EXTERNAL :: nfmpi_put_var_text
    INTEGER, EXTERNAL :: nfmpi_put_var_int1
    INTEGER, EXTERNAL :: nfmpi_put_var_int2
    INTEGER, EXTERNAL :: nfmpi_put_var_int
    INTEGER, EXTERNAL :: nfmpi_put_var_real
    INTEGER, EXTERNAL :: nfmpi_put_var_double
    INTEGER, EXTERNAL :: nfmpi_put_var_int8

    INTEGER, EXTERNAL :: nfmpi_get_var
    INTEGER, EXTERNAL :: nfmpi_get_var_text
    INTEGER, EXTERNAL :: nfmpi_get_var_int1
    INTEGER, EXTERNAL :: nfmpi_get_var_int2
    INTEGER, EXTERNAL :: nfmpi_get_var_int
    INTEGER, EXTERNAL :: nfmpi_get_var_real
    INTEGER, EXTERNAL :: nfmpi_get_var_double
    INTEGER, EXTERNAL :: nfmpi_get_var_int8

    INTEGER, EXTERNAL :: nfmpi_get_var_all
    INTEGER, EXTERNAL :: nfmpi_get_var_text_all
    INTEGER, EXTERNAL :: nfmpi_get_var_int1_all
    INTEGER, EXTERNAL :: nfmpi_get_var_int2_all
    INTEGER, EXTERNAL :: nfmpi_get_var_int_all
    INTEGER, EXTERNAL :: nfmpi_get_var_real_all
    INTEGER, EXTERNAL :: nfmpi_get_var_double_all
    INTEGER, EXTERNAL :: nfmpi_get_var_int8_all

    !!......   single element variable put/get routines:

    INTEGER, EXTERNAL :: nfmpi_put_var1
    INTEGER, EXTERNAL :: nfmpi_put_var1_text
    INTEGER, EXTERNAL :: nfmpi_put_var1_int1
    INTEGER, EXTERNAL :: nfmpi_put_var1_int2
    INTEGER, EXTERNAL :: nfmpi_put_var1_int
    INTEGER, EXTERNAL :: nfmpi_put_var1_real
    INTEGER, EXTERNAL :: nfmpi_put_var1_double
    INTEGER, EXTERNAL :: nfmpi_put_var1_int8

    INTEGER, EXTERNAL :: nfmpi_put_var1_all
    INTEGER, EXTERNAL :: nfmpi_put_var1_text_all
    INTEGER, EXTERNAL :: nfmpi_put_var1_int1_all
    INTEGER, EXTERNAL :: nfmpi_put_var1_int2_all
    INTEGER, EXTERNAL :: nfmpi_put_var1_int_all
    INTEGER, EXTERNAL :: nfmpi_put_var1_real_all
    INTEGER, EXTERNAL :: nfmpi_put_var1_double_all
    INTEGER, EXTERNAL :: nfmpi_put_var1_int8_all

    INTEGER, EXTERNAL :: nfmpi_get_var1
    INTEGER, EXTERNAL :: nfmpi_get_var1_text
    INTEGER, EXTERNAL :: nfmpi_get_var1_int1
    INTEGER, EXTERNAL :: nfmpi_get_var1_int2
    INTEGER, EXTERNAL :: nfmpi_get_var1_int
    INTEGER, EXTERNAL :: nfmpi_get_var1_real
    INTEGER, EXTERNAL :: nfmpi_get_var1_double
    INTEGER, EXTERNAL :: nfmpi_get_var1_int8

    INTEGER, EXTERNAL :: nfmpi_get_var1_all
    INTEGER, EXTERNAL :: nfmpi_get_var1_text_all
    INTEGER, EXTERNAL :: nfmpi_get_var1_int1_all
    INTEGER, EXTERNAL :: nfmpi_get_var1_int2_all
    INTEGER, EXTERNAL :: nfmpi_get_var1_int_all
    INTEGER, EXTERNAL :: nfmpi_get_var1_real_all
    INTEGER, EXTERNAL :: nfmpi_get_var1_double_all
    INTEGER, EXTERNAL :: nfmpi_get_var1_int8_all

    !!......   variable sub-array put/get routines:

    INTEGER, EXTERNAL :: nfmpi_put_vara
    INTEGER, EXTERNAL :: nfmpi_put_vara_text
    INTEGER, EXTERNAL :: nfmpi_put_vara_int1
    INTEGER, EXTERNAL :: nfmpi_put_vara_int2
    INTEGER, EXTERNAL :: nfmpi_put_vara_int
    INTEGER, EXTERNAL :: nfmpi_put_vara_real
    INTEGER, EXTERNAL :: nfmpi_put_vara_double
    INTEGER, EXTERNAL :: nfmpi_put_vara_int8

    INTEGER, EXTERNAL :: nfmpi_put_vara_all
    INTEGER, EXTERNAL :: nfmpi_put_vara_text_all
    INTEGER, EXTERNAL :: nfmpi_put_vara_int1_all
    INTEGER, EXTERNAL :: nfmpi_put_vara_int2_all
    INTEGER, EXTERNAL :: nfmpi_put_vara_int_all
    INTEGER, EXTERNAL :: nfmpi_put_vara_real_all
    INTEGER, EXTERNAL :: nfmpi_put_vara_double_all
    INTEGER, EXTERNAL :: nfmpi_put_vara_int8_all

    INTEGER, EXTERNAL :: nfmpi_get_vara
    INTEGER, EXTERNAL :: nfmpi_get_vara_text
    INTEGER, EXTERNAL :: nfmpi_get_vara_int1
    INTEGER, EXTERNAL :: nfmpi_get_vara_int2
    INTEGER, EXTERNAL :: nfmpi_get_vara_int
    INTEGER, EXTERNAL :: nfmpi_get_vara_real
    INTEGER, EXTERNAL :: nfmpi_get_vara_double
    INTEGER, EXTERNAL :: nfmpi_get_vara_int8

    INTEGER, EXTERNAL :: nfmpi_get_vara_all
    INTEGER, EXTERNAL :: nfmpi_get_vara_text_all
    INTEGER, EXTERNAL :: nfmpi_get_vara_int1_all
    INTEGER, EXTERNAL :: nfmpi_get_vara_int2_all
    INTEGER, EXTERNAL :: nfmpi_get_vara_int_all
    INTEGER, EXTERNAL :: nfmpi_get_vara_real_all
    INTEGER, EXTERNAL :: nfmpi_get_vara_double_all
    INTEGER, EXTERNAL :: nfmpi_get_vara_int8_all

    !!......   strided variable put/get routines:

    INTEGER, EXTERNAL :: nfmpi_put_vars
    INTEGER, EXTERNAL :: nfmpi_put_vars_text
    INTEGER, EXTERNAL :: nfmpi_put_vars_int1
    INTEGER, EXTERNAL :: nfmpi_put_vars_int2
    INTEGER, EXTERNAL :: nfmpi_put_vars_int
    INTEGER, EXTERNAL :: nfmpi_put_vars_real
    INTEGER, EXTERNAL :: nfmpi_put_vars_double
    INTEGER, EXTERNAL :: nfmpi_put_vars_int8

    INTEGER, EXTERNAL :: nfmpi_put_vars_all
    INTEGER, EXTERNAL :: nfmpi_put_vars_text_all
    INTEGER, EXTERNAL :: nfmpi_put_vars_int1_all
    INTEGER, EXTERNAL :: nfmpi_put_vars_int2_all
    INTEGER, EXTERNAL :: nfmpi_put_vars_int_all
    INTEGER, EXTERNAL :: nfmpi_put_vars_real_all
    INTEGER, EXTERNAL :: nfmpi_put_vars_double_all
    INTEGER, EXTERNAL :: nfmpi_put_vars_int8_all

    INTEGER, EXTERNAL :: nfmpi_get_vars
    INTEGER, EXTERNAL :: nfmpi_get_vars_text
    INTEGER, EXTERNAL :: nfmpi_get_vars_int1
    INTEGER, EXTERNAL :: nfmpi_get_vars_int2
    INTEGER, EXTERNAL :: nfmpi_get_vars_int
    INTEGER, EXTERNAL :: nfmpi_get_vars_real
    INTEGER, EXTERNAL :: nfmpi_get_vars_double
    INTEGER, EXTERNAL :: nfmpi_get_vars_int8

    INTEGER, EXTERNAL :: nfmpi_get_vars_all
    INTEGER, EXTERNAL :: nfmpi_get_vars_text_all
    INTEGER, EXTERNAL :: nfmpi_get_vars_int1_all
    INTEGER, EXTERNAL :: nfmpi_get_vars_int2_all
    INTEGER, EXTERNAL :: nfmpi_get_vars_int_all
    INTEGER, EXTERNAL :: nfmpi_get_vars_real_all
    INTEGER, EXTERNAL :: nfmpi_get_vars_double_all
    INTEGER, EXTERNAL :: nfmpi_get_vars_int8_all

    !!......   mapped variable put/get routines:

    INTEGER, EXTERNAL :: nfmpi_put_varm
    INTEGER, EXTERNAL :: nfmpi_put_varm_text
    INTEGER, EXTERNAL :: nfmpi_put_varm_int1
    INTEGER, EXTERNAL :: nfmpi_put_varm_int2
    INTEGER, EXTERNAL :: nfmpi_put_varm_int
    INTEGER, EXTERNAL :: nfmpi_put_varm_real
    INTEGER, EXTERNAL :: nfmpi_put_varm_double
    INTEGER, EXTERNAL :: nfmpi_put_varm_int8

    INTEGER, EXTERNAL :: nfmpi_put_varm_all
    INTEGER, EXTERNAL :: nfmpi_put_varm_text_all
    INTEGER, EXTERNAL :: nfmpi_put_varm_int1_all
    INTEGER, EXTERNAL :: nfmpi_put_varm_int2_all
    INTEGER, EXTERNAL :: nfmpi_put_varm_int_all
    INTEGER, EXTERNAL :: nfmpi_put_varm_real_all
    INTEGER, EXTERNAL :: nfmpi_put_varm_double_all
    INTEGER, EXTERNAL :: nfmpi_put_varm_int8_all

    INTEGER, EXTERNAL :: nfmpi_get_varm
    INTEGER, EXTERNAL :: nfmpi_get_varm_text
    INTEGER, EXTERNAL :: nfmpi_get_varm_int1
    INTEGER, EXTERNAL :: nfmpi_get_varm_int2
    INTEGER, EXTERNAL :: nfmpi_get_varm_int
    INTEGER, EXTERNAL :: nfmpi_get_varm_real
    INTEGER, EXTERNAL :: nfmpi_get_varm_double
    INTEGER, EXTERNAL :: nfmpi_get_varm_int8

    INTEGER, EXTERNAL :: nfmpi_get_varm_all
    INTEGER, EXTERNAL :: nfmpi_get_varm_text_all
    INTEGER, EXTERNAL :: nfmpi_get_varm_int1_all
    INTEGER, EXTERNAL :: nfmpi_get_varm_int2_all
    INTEGER, EXTERNAL :: nfmpi_get_varm_int_all
    INTEGER, EXTERNAL :: nfmpi_get_varm_real_all
    INTEGER, EXTERNAL :: nfmpi_get_varm_double_all
    INTEGER, EXTERNAL :: nfmpi_get_varm_int8_all

    !!......   Non-blocking APIs
    !!......
    !!......   entire variable iput/iget routines:

    INTEGER, EXTERNAL :: nfmpi_iput_var
    INTEGER, EXTERNAL :: nfmpi_iput_var_text
    INTEGER, EXTERNAL :: nfmpi_iput_var_int1
    INTEGER, EXTERNAL :: nfmpi_iput_var_int2
    INTEGER, EXTERNAL :: nfmpi_iput_var_int
    INTEGER, EXTERNAL :: nfmpi_iput_var_real
    INTEGER, EXTERNAL :: nfmpi_iput_var_double
    INTEGER, EXTERNAL :: nfmpi_iput_var_int8

    INTEGER, EXTERNAL :: nfmpi_iget_var
    INTEGER, EXTERNAL :: nfmpi_iget_var_text
    INTEGER, EXTERNAL :: nfmpi_iget_var_int1
    INTEGER, EXTERNAL :: nfmpi_iget_var_int2
    INTEGER, EXTERNAL :: nfmpi_iget_var_int
    INTEGER, EXTERNAL :: nfmpi_iget_var_real
    INTEGER, EXTERNAL :: nfmpi_iget_var_double
    INTEGER, EXTERNAL :: nfmpi_iget_var_int8

    !!......   Nonblocking single-element variable iput/iget routines:

    INTEGER, EXTERNAL :: nfmpi_iput_var1
    INTEGER, EXTERNAL :: nfmpi_iput_var1_text
    INTEGER, EXTERNAL :: nfmpi_iput_var1_int1
    INTEGER, EXTERNAL :: nfmpi_iput_var1_int2
    INTEGER, EXTERNAL :: nfmpi_iput_var1_int
    INTEGER, EXTERNAL :: nfmpi_iput_var1_real
    INTEGER, EXTERNAL :: nfmpi_iput_var1_double
    INTEGER, EXTERNAL :: nfmpi_iput_var1_int8

    INTEGER, EXTERNAL :: nfmpi_iget_var1
    INTEGER, EXTERNAL :: nfmpi_iget_var1_text
    INTEGER, EXTERNAL :: nfmpi_iget_var1_int1
    INTEGER, EXTERNAL :: nfmpi_iget_var1_int2
    INTEGER, EXTERNAL :: nfmpi_iget_var1_int
    INTEGER, EXTERNAL :: nfmpi_iget_var1_real
    INTEGER, EXTERNAL :: nfmpi_iget_var1_double
    INTEGER, EXTERNAL :: nfmpi_iget_var1_int8

    !!......   Nonblocking subarray variable iput/iget routines:

    INTEGER, EXTERNAL :: nfmpi_iput_vara
    INTEGER, EXTERNAL :: nfmpi_iput_vara_text
    INTEGER, EXTERNAL :: nfmpi_iput_vara_int1
    INTEGER, EXTERNAL :: nfmpi_iput_vara_int2
    INTEGER, EXTERNAL :: nfmpi_iput_vara_int
    INTEGER, EXTERNAL :: nfmpi_iput_vara_real
    INTEGER, EXTERNAL :: nfmpi_iput_vara_double
    INTEGER, EXTERNAL :: nfmpi_iput_vara_int8

    INTEGER, EXTERNAL :: nfmpi_iget_vara
    INTEGER, EXTERNAL :: nfmpi_iget_vara_text
    INTEGER, EXTERNAL :: nfmpi_iget_vara_int1
    INTEGER, EXTERNAL :: nfmpi_iget_vara_int2
    INTEGER, EXTERNAL :: nfmpi_iget_vara_int
    INTEGER, EXTERNAL :: nfmpi_iget_vara_real
    INTEGER, EXTERNAL :: nfmpi_iget_vara_double
    INTEGER, EXTERNAL :: nfmpi_iget_vara_int8

    !!......   Nonblocking strided variable iput/iget routines:

    INTEGER, EXTERNAL :: nfmpi_iput_vars
    INTEGER, EXTERNAL :: nfmpi_iput_vars_text
    INTEGER, EXTERNAL :: nfmpi_iput_vars_int1
    INTEGER, EXTERNAL :: nfmpi_iput_vars_int2
    INTEGER, EXTERNAL :: nfmpi_iput_vars_int
    INTEGER, EXTERNAL :: nfmpi_iput_vars_real
    INTEGER, EXTERNAL :: nfmpi_iput_vars_double
    INTEGER, EXTERNAL :: nfmpi_iput_vars_int8

    INTEGER, EXTERNAL :: nfmpi_iget_vars
    INTEGER, EXTERNAL :: nfmpi_iget_vars_text
    INTEGER, EXTERNAL :: nfmpi_iget_vars_int1
    INTEGER, EXTERNAL :: nfmpi_iget_vars_int2
    INTEGER, EXTERNAL :: nfmpi_iget_vars_int
    INTEGER, EXTERNAL :: nfmpi_iget_vars_real
    INTEGER, EXTERNAL :: nfmpi_iget_vars_double
    INTEGER, EXTERNAL :: nfmpi_iget_vars_int8

    !!......   Nonblocking mapped variable iput/iget routines:

    INTEGER, EXTERNAL :: nfmpi_iput_varm
    INTEGER, EXTERNAL :: nfmpi_iput_varm_text
    INTEGER, EXTERNAL :: nfmpi_iput_varm_int1
    INTEGER, EXTERNAL :: nfmpi_iput_varm_int2
    INTEGER, EXTERNAL :: nfmpi_iput_varm_int
    INTEGER, EXTERNAL :: nfmpi_iput_varm_real
    INTEGER, EXTERNAL :: nfmpi_iput_varm_double
    INTEGER, EXTERNAL :: nfmpi_iput_varm_int8

    INTEGER, EXTERNAL :: nfmpi_iget_varm
    INTEGER, EXTERNAL :: nfmpi_iget_varm_text
    INTEGER, EXTERNAL :: nfmpi_iget_varm_int1
    INTEGER, EXTERNAL :: nfmpi_iget_varm_int2
    INTEGER, EXTERNAL :: nfmpi_iget_varm_int
    INTEGER, EXTERNAL :: nfmpi_iget_varm_real
    INTEGER, EXTERNAL :: nfmpi_iget_varm_double
    INTEGER, EXTERNAL :: nfmpi_iget_varm_int8

    !!......   Nonblocking entire variable bput routines:

    INTEGER, EXTERNAL :: nfmpi_bput_var
    INTEGER, EXTERNAL :: nfmpi_bput_var_text
    INTEGER, EXTERNAL :: nfmpi_bput_var_int1
    INTEGER, EXTERNAL :: nfmpi_bput_var_int2
    INTEGER, EXTERNAL :: nfmpi_bput_var_int
    INTEGER, EXTERNAL :: nfmpi_bput_var_real
    INTEGER, EXTERNAL :: nfmpi_bput_var_double
    INTEGER, EXTERNAL :: nfmpi_bput_var_int8

    !!......   Nonblocking single element variable bput routines:

    INTEGER, EXTERNAL :: nfmpi_bput_var1
    INTEGER, EXTERNAL :: nfmpi_bput_var1_text
    INTEGER, EXTERNAL :: nfmpi_bput_var1_int1
    INTEGER, EXTERNAL :: nfmpi_bput_var1_int2
    INTEGER, EXTERNAL :: nfmpi_bput_var1_int
    INTEGER, EXTERNAL :: nfmpi_bput_var1_real
    INTEGER, EXTERNAL :: nfmpi_bput_var1_double
    INTEGER, EXTERNAL :: nfmpi_bput_var1_int8

    !!......   Nonblocking subarray variable bput routines:

    INTEGER, EXTERNAL :: nfmpi_bput_vara
    INTEGER, EXTERNAL :: nfmpi_bput_vara_text
    INTEGER, EXTERNAL :: nfmpi_bput_vara_int1
    INTEGER, EXTERNAL :: nfmpi_bput_vara_int2
    INTEGER, EXTERNAL :: nfmpi_bput_vara_int
    INTEGER, EXTERNAL :: nfmpi_bput_vara_real
    INTEGER, EXTERNAL :: nfmpi_bput_vara_double
    INTEGER, EXTERNAL :: nfmpi_bput_vara_int8

    !!......   Nonblocking strided variable bput routines:

    INTEGER, EXTERNAL :: nfmpi_bput_vars
    INTEGER, EXTERNAL :: nfmpi_bput_vars_text
    INTEGER, EXTERNAL :: nfmpi_bput_vars_int1
    INTEGER, EXTERNAL :: nfmpi_bput_vars_int2
    INTEGER, EXTERNAL :: nfmpi_bput_vars_int
    INTEGER, EXTERNAL :: nfmpi_bput_vars_real
    INTEGER, EXTERNAL :: nfmpi_bput_vars_double
    INTEGER, EXTERNAL :: nfmpi_bput_vars_int8

    !!......   Nonblocking mapped variable bput routines:

    INTEGER, EXTERNAL :: nfmpi_bput_varm
    INTEGER, EXTERNAL :: nfmpi_bput_varm_text
    INTEGER, EXTERNAL :: nfmpi_bput_varm_int1
    INTEGER, EXTERNAL :: nfmpi_bput_varm_int2
    INTEGER, EXTERNAL :: nfmpi_bput_varm_int
    INTEGER, EXTERNAL :: nfmpi_bput_varm_real
    INTEGER, EXTERNAL :: nfmpi_bput_varm_double
    INTEGER, EXTERNAL :: nfmpi_bput_varm_int8

    !!......   Nonblocking control APIs

    INTEGER, EXTERNAL :: nfmpi_wait
    INTEGER, EXTERNAL :: nfmpi_wait_all
    INTEGER, EXTERNAL :: nfmpi_cancel

    INTEGER, EXTERNAL :: nfmpi_buffer_attach
    INTEGER, EXTERNAL :: nfmpi_buffer_detach
    INTEGER, EXTERNAL :: nfmpi_inq_buffer_usage
    INTEGER, EXTERNAL :: nfmpi_inq_buffer_size
    INTEGER, EXTERNAL :: nfmpi_inq_put_size
    INTEGER, EXTERNAL :: nfmpi_inq_get_size
    INTEGER, EXTERNAL :: nfmpi_inq_header_size
    INTEGER, EXTERNAL :: nfmpi_inq_header_extent
    INTEGER, EXTERNAL :: nfmpi_inq_varoffset
    INTEGER, EXTERNAL :: nfmpi_inq_nreqs

    !!......   varn routines:

    INTEGER, EXTERNAL :: nfmpi_put_varn
    INTEGER, EXTERNAL :: nfmpi_put_varn_text
    INTEGER, EXTERNAL :: nfmpi_put_varn_int1
    INTEGER, EXTERNAL :: nfmpi_put_varn_int2
    INTEGER, EXTERNAL :: nfmpi_put_varn_int
    INTEGER, EXTERNAL :: nfmpi_put_varn_real
    INTEGER, EXTERNAL :: nfmpi_put_varn_double
    INTEGER, EXTERNAL :: nfmpi_put_varn_int8

    INTEGER, EXTERNAL :: nfmpi_put_varn_all
    INTEGER, EXTERNAL :: nfmpi_put_varn_text_all
    INTEGER, EXTERNAL :: nfmpi_put_varn_int1_all
    INTEGER, EXTERNAL :: nfmpi_put_varn_int2_all
    INTEGER, EXTERNAL :: nfmpi_put_varn_int_all
    INTEGER, EXTERNAL :: nfmpi_put_varn_real_all
    INTEGER, EXTERNAL :: nfmpi_put_varn_double_all
    INTEGER, EXTERNAL :: nfmpi_put_varn_int8_all

    !!......   Nonblocking varn routines

    INTEGER, EXTERNAL :: nfmpi_iput_varn
    INTEGER, EXTERNAL :: nfmpi_iput_varn_text
    INTEGER, EXTERNAL :: nfmpi_iput_varn_int1
    INTEGER, EXTERNAL :: nfmpi_iput_varn_int2
    INTEGER, EXTERNAL :: nfmpi_iput_varn_int
    INTEGER, EXTERNAL :: nfmpi_iput_varn_real
    INTEGER, EXTERNAL :: nfmpi_iput_varn_double
    INTEGER, EXTERNAL :: nfmpi_iput_varn_int8

    INTEGER, EXTERNAL :: nfmpi_iget_varn
    INTEGER, EXTERNAL :: nfmpi_iget_varn_text
    INTEGER, EXTERNAL :: nfmpi_iget_varn_int1
    INTEGER, EXTERNAL :: nfmpi_iget_varn_int2
    INTEGER, EXTERNAL :: nfmpi_iget_varn_int
    INTEGER, EXTERNAL :: nfmpi_iget_varn_real
    INTEGER, EXTERNAL :: nfmpi_iget_varn_double
    INTEGER, EXTERNAL :: nfmpi_iget_varn_int8

    INTEGER, EXTERNAL :: nfmpi_bput_varn
    INTEGER, EXTERNAL :: nfmpi_bput_varn_text
    INTEGER, EXTERNAL :: nfmpi_bput_varn_int1
    INTEGER, EXTERNAL :: nfmpi_bput_varn_int2
    INTEGER, EXTERNAL :: nfmpi_bput_varn_int
    INTEGER, EXTERNAL :: nfmpi_bput_varn_real
    INTEGER, EXTERNAL :: nfmpi_bput_varn_double
    INTEGER, EXTERNAL :: nfmpi_bput_varn_int8

    !!......   vard routines:

    INTEGER, EXTERNAL :: nfmpi_put_vard
    INTEGER, EXTERNAL :: nfmpi_get_vard

    INTEGER, EXTERNAL :: nfmpi_put_vard_all
    INTEGER, EXTERNAL :: nfmpi_get_vard_all


#endif


CONTAINS    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-



    LOGICAL FUNCTION  CREATENC( FNAME,  FHIST,  FDESC,                      &
                                NDIMS,  DNAMES, DSIZES,                     &
                                NVARS,  VNAMES, VTYPES, VNDIMS, VDNAME,     &
                                VUNITS, VTITLE, VDESCS )

        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                   !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: FHIST                   !!  attribute  "history"
        CHARACTER*(*), INTENT(IN   ) :: FDESC                   !!  attribute  "description"
        INTEGER      , INTENT(IN   ) :: NDIMS                   !!  number of dimensions used
        CHARACTER*(*), INTENT(IN   ) :: DNAMES( NDIMS )         !!  dimension-names
        INTEGER      , INTENT(IN   ) :: DSIZES( NDIMS )         !!  dimension-values
        INTEGER      , INTENT(IN   ) :: NVARS                   !!  number of (extra) output variables
        CHARACTER*(*), INTENT(IN   ) :: VNAMES(   NVARS )       !!  variable-names
        INTEGER      , INTENT(IN   ) :: VTYPES(   NVARS )       !!  variable-type M3REAL, etc...)
        INTEGER      , INTENT(IN   ) :: VNDIMS(   NVARS )       !!  rank (number of dimensions)
        CHARACTER*(*), INTENT(IN   ) :: VDNAME( 7,NVARS )       !!  names for dimensions used for the variables
        CHARACTER*(*), INTENT(IN   ) :: VUNITS(   NVARS )       !!  attribute  "units"
        CHARACTER*(*), INTENT(IN   ) :: VTITLE(   NVARS )       !!  attribute  "long_name"
        CHARACTER*(*), INTENT(IN   ) :: VDESCS(   NVARS )       !!  attribute  "description"

        CHARACTER*1,  PARAMETER :: BLANK = ' '
        CHARACTER*24, PARAMETER :: PNAME = 'MODNCFIO/CREATENC'

        CHARACTER(LEN=256)          MESG
        CHARACTER(LEN=512)          EQNAME

        INTEGER         FID, DID, VID, F, FMODE
        INTEGER         IERR, ID, I, K, M, N, V, VV
        INTEGER         NAMELEN, UNITLEN, DESCLEN
        
        LOGICAL         EFLAG, AFLAG
        
        INTEGER         DIMIDS( NDIMS )

        INTEGER         DIMS( 7 )
        INTEGER         DIDS( 7 )

        FMODE = IOR( IOR( NF_NOCLOBBER, NF_SHARE ), NF_64BIT_OFFSET )

        CALL NAMEVAL( FNAME, EQNAME )

        IERR = NF_CREATE( EQNAME, FMODE, FID )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( NF_STRERROR( IERR ) )
            CALL M3MESG( PNAME // ' Error opening "' // TRIM( FNAME ) // '"' )
            CREATENC = .FALSE.
            RETURN
        END IF              !!  ierr nonzero:  NF_CREATE() failed

        EFLAG = .FALSE.     !!  no errors yet

        IF ( FHIST .NE. BLANK ) THEN
            IERR = NF_PUT_ATT_TEXT( FID, NF_GLOBAL, 'history', LEN( FHIST ), FHIST )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) ) 
                CALL M3MESG( PNAME // ' Error creating att "history" for  ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed
        END IF

        IF ( FDESC .NE. BLANK ) THEN
            IERR = NF_PUT_ATT_TEXT( FID, NF_GLOBAL, 'description', LEN( FDESC ), FDESC )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) ) 
                CALL M3MESG( PNAME // ' Error creating att "description" for  ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed
        END IF


        !!........  Dimensions:

        DO N = 1, NDIMS

            IERR = NF_DEF_DIM( FID, DNAMES( N ), DSIZES( N ), DIMIDS( N ) )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating netCDF dimension "' // TRIM( DNAMES( N ) ) // '" for ' // FNAME )
                EFLAG = .TRUE.
            END IF              !  ierr nonzero:  operation failed

        END DO      !! end loop creating dims

        IF ( EFLAG ) THEN
            IERR     = NF_ABORT( FID )
            CREATENC = .FALSE.
            RETURN
        END IF


        !!........  Variables:

        NAMELEN = LEN( VTITLE( 1 ) )
        UNITLEN = LEN( VUNITS( 1 ) )
        DESCLEN = LEN( VDESCS( 1 ) )
        
        DO V = 1, NVARS

            IF ( VNDIMS( V ) .GT. 7 ) THEN
                EFLAG = .TRUE.
                MESG  = 'Too many dimensions for vble "' // TRIM( VNAMES( V ) ) // '" in "' // FNAME
                CALL M3MESG( MESG )
                CYCLE
            END IF

            AFLAG = .FALSE.

            DO N = 1, VNDIMS( V )

                I = INDEX1( VDNAME( N,V ), NDIMS, DNAMES )
                IF ( I .GT. 0 ) THEN
                    DIMS( N ) = DSIZES( I )
                    DIDS( N ) = DIMIDS( I )
                ELSE
                    AFLAG = .TRUE.
                    MESG  =  PNAME//' Invalid dim "'//TRIM( VDNAME(N,V) )//'" for vble "'//TRIM( VNAMES(V) )//'" in ' // FNAME
                    CALL M3MESG( MESG )
                END IF

            END DO

            IF ( AFLAG ) THEN
                EFLAG = .TRUE.
                CYCLE
            END IF

            IERR = NF_DEF_VAR( FID, VNAMES( V ), VTYPES( V ), VNDIMS( V ), DIDS, VID )
            IF ( IERR .NE. 0 ) THEN
                CALL M3MESG( NF_STRERROR( IERR ) )
                CALL M3MESG( PNAME // ' Error creating variable "' // TRIM( VNAMES(V) ) // '" for ' // FNAME )
                EFLAG = .TRUE.
                CYCLE
            END IF              !  ierr nonzero:  operation failed

            IF ( VTITLE( V ) .NE. BLANK ) THEN
                IERR = NF_PUT_ATT_TEXT( FID, VID, 'long_name', NAMELEN, VTITLE( V ) )
                IF ( IERR .NE. 0 ) THEN
                    CALL M3MESG( NF_STRERROR( IERR ) ) 
                    CALL M3MESG( PNAME // ' Error creating att "long_name" for "' // TRIM( VNAMES(V) ) // '" in ' // FNAME )
                    EFLAG = .TRUE.
                END IF              !  ierr nonzero:  operation failed
            END IF

            IF ( VUNITS( V ) .NE. BLANK ) THEN
                IERR = NF_PUT_ATT_TEXT( FID, VID, 'units', UNITLEN, VUNITS( V ) )
                IF ( IERR .NE. 0 ) THEN
                    CALL M3MESG( NF_STRERROR( IERR ) ) 
                    CALL M3MESG( PNAME // ' Error creating att "units" for "' // TRIM( VNAMES(V) ) // '" in ' // FNAME )
                    EFLAG = .TRUE.
                END IF              !  ierr nonzero:  operation failed
            END IF

            IF ( VDESCS( V ) .NE. BLANK ) THEN
                IERR = NF_PUT_ATT_TEXT( FID, VID, 'description', DESCLEN, VDESCS( V ) )
                IF ( IERR .NE. 0 ) THEN
                    CALL M3MESG( NF_STRERROR( IERR ) ) 
                    CALL M3MESG( PNAME // ' Error creating att "description" for "' // TRIM( VNAMES(V) ) // '" in ' // FNAME )
                    EFLAG = .TRUE.
                END IF              !  ierr nonzero:  operation failed
            END IF

        END DO      !! end loop creating variables


        !!........  Put FNAME back into data mode:  attributes and variables now defined.

        IERR = NF_ENDDEF( FID )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( NF_STRERROR( IERR ) )
            CALL M3MESG( PNAME // ' Error putting "' // FNAME // '" into data mode' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_CLOSE( FID )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( NF_STRERROR( IERR ) )
            CALL M3MESG( PNAME // ' Error closing "' // TRIM( FNAME ) // '"' )
            EFLAG = .TRUE.
        END IF          !!  istat nonzero:  NF_OPEN() failed

        IF ( EFLAG ) THEN
            IERR     = NF_ABORT( FID )
        END IF

        CREATENC = ( .NOT.EFLAG )
        RETURN


    END FUNCTION CREATENC



    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-



    LOGICAL FUNCTION DESCNCVAR( FNAME, MXVAR, NVARS, VNAMES, VUNITS, VTYPES, VNDIMS, VDIMS )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        INTEGER      , INTENT(IN   ) :: MXVAR           !!  max # of vars returned
        INTEGER      , INTENT(  OUT) :: NVARS           !!  max( MXVAR, actual # of vbles )
        CHARACTER*(*), INTENT(  OUT) :: VNAMES( MXVAR ) !!  variable names
        CHARACTER*(*), INTENT(  OUT) :: VUNITS( MXVAR ) !!  variable units
        INTEGER      , INTENT(  OUT) :: VTYPES( MXVAR ) !!  types (M3REAL, M3INT, etc.)
        INTEGER      , INTENT(  OUT) :: VNDIMS( MXVAR ) !!  ranks (number of dimensions)
        INTEGER      , INTENT(  OUT) :: VDIMS(7,MXVAR ) !!  dimensions

        INTEGER         FID, VID, V, N, ATYP, ALEN
        INTEGER         ISTAT, DIMIDS( 7 ), VCOUNT, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            DESCNCVAR = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_NVARS( FID, VCOUNT )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error getting NVARS for "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO  999
        ELSE IF ( VCOUNT .GT. MXVAR ) THEN
            WRITE( MESG, '( 2( A, I3, 2X ), 3A )' )     &
                'Actual NVARS=', VCOUNT, 'exceeds MXVAR=', MXVAR, 'in "', TRIM( FNAME ), '"'
            CALL M3WARN( 'READNCVAR/DESCNCVAR', 0,0, MESG )
        END IF

        NVARS = MIN( MXVAR, VCOUNT )

        DO V = 1, NVARS

            ISTAT = NF_INQ_VAR( FID, V, VNAMES(V), VTYPES(V), VNDIMS(V), DIMIDS, NATTS  )
            IF ( ISTAT .NE. 0 ) THEN
                MESG = 'Error reading NDIMS for  "' // TRIM( VNAMES(V) ) // '" in "' // TRIM( FNAME ) // '"'
                CALL M3MESG( MESG )
                MESG = NF_STRERROR( ISTAT )
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
                CYCLE
            ELSE IF ( VNDIMS(V) .GT. 7 ) THEN
                MESG = 'Number of dimensions > 7 for  "' // TRIM( VNAMES(V) ) // '" in "' // TRIM( FNAME ) // '"'
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
                CYCLE
            END IF          !!  ierr nonzero:  NF_INQ_VAR() failed

            ISTAT = NF_INQ_ATT( FID, V, 'units', ATYP, ALEN )
            IF ( ISTAT .EQ. NF_EEXIST .OR. ISTAT .EQ. NF_ENOTATT ) THEN
                VUNITS(V) = CMISS3
            ELSE IF ( ISTAT .NE. 0 ) THEN
                MESG = 'Error reading "units" for  "' // TRIM( VNAMES(V) ) // '" in "' // TRIM( FNAME ) // '"'
                CALL M3MESG( MESG )
                MESG = NF_STRERROR( ISTAT )
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            ELSE IF ( ATYP .NE. NF_CHAR ) THEN
                MESG = 'non-CHARACTER "units" for  "' // TRIM( VNAMES(V) ) // '" in "' // TRIM( FNAME ) // '"'
                CALL M3MESG( MESG )
                MESG = NF_STRERROR( ISTAT )
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
            ELSE
                ISTAT = NF_GET_ATT_TEXT( FID, V, 'units', VUNITS(V) )
                IF ( ISTAT .NE. 0 ) THEN
                    MESG = 'Error reading "units" for  "' // TRIM( VNAMES(V) ) // '" in "' // TRIM( FNAME ) // '"'
                    CALL M3MESG( MESG )
                    MESG = NF_STRERROR( ISTAT )
                    CALL M3MESG( MESG )
                    EFLAG = .TRUE.
                    VUNITS(V) = CMISS3
                END IF
            END IF

            DO N = 1, VNDIMS(V)

                ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(N), VDIMS(N,V ) )
                IF ( ISTAT .NE. 0 ) THEN
                    MESG = 'Error reading dimension for  "' // TRIM( VNAMES(V) ) // '" in "' // TRIM( FNAME ) // '"'
                    CALL M3MESG( MESG )
                    MESG = NF_STRERROR( ISTAT )
                    CALL M3MESG( MESG )
                    EFLAG = .TRUE.
                    CYCLE
                END IF          !!  ierr nonzero:  NF_INQ_DIMLEN() failed

            END DO      !!  end loop on dimensions for this variable

        END DO          !!  end loop on variables for this file


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !!  istat nonzero:  NF_OPEN() failed

        DESCNCVAR = ( .NOT. EFLAG )
        RETURN


    END FUNCTION DESCNCVAR


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNCVAR0DR( FNAME, VNAME, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        REAL         , INTENT(  OUT) :: GRID

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNCVAR0DR = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 0 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3REAL ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        DIMS(:) = 1
        DELS(1) = 1
        ISTAT   = NF_GET_VARA_REAL( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_REAL() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNCVAR0DR = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNCVAR0DR


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNCVAR0DI( FNAME, VNAME, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(  OUT) :: GRID

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNCVAR0DI = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 0 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3INT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        DIMS(:) = 1
        DELS(1) = 1
        ISTAT   = NF_GET_VARA_INT( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNCVAR0DI = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNCVAR0DI


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNCVAR0DS( FNAME, VNAME, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER(2)   , INTENT(  OUT) :: GRID

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNCVAR0DS = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 0 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_SHORT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        DIMS(:) = 1
        DELS(1) = 1
        ISTAT   = NF_GET_VARA_INT2( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNCVAR0DS = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNCVAR0DS


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNCVAR0DB( FNAME, VNAME, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER(1)   , INTENT(  OUT) :: GRID

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNCVAR0DB = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 0 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_BYTE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        DIMS(:) = 1
        DELS(1) = 1
        ISTAT   = NF_GET_VARA_INT1( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNCVAR0DB = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNCVAR0DB


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNCVAR0DD( FNAME, VNAME, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        REAL*8       , INTENT(  OUT) :: GRID

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNCVAR0DD = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 0 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3DBLE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        DIMS(:) = 1
        DELS(1) = 1
        ISTAT   = NF_GET_VARA_DOUBLE( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_DOUBLE() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNCVAR0DD = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNCVAR0DD


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNCVAR1DR( FNAME, VNAME, NCOLS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS           !!  dimensions
        REAL         , INTENT(  OUT) :: GRID( NCOLS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNCVAR1DR = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 1 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3REAL ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        ISTAT   = NF_GET_VARA_REAL( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_REAL() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNCVAR1DR = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNCVAR1DR


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNCVAR1DI( FNAME, VNAME, NCOLS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS           !!  dimensions
        INTEGER      , INTENT(  OUT) :: GRID( NCOLS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNCVAR1DI = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 1 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3INT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        ISTAT   = NF_GET_VARA_INT( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNCVAR1DI = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNCVAR1DI


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNCVAR1DS( FNAME, VNAME, NCOLS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS           !!  dimensions
        INTEGER(2)   , INTENT(  OUT) :: GRID( NCOLS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNCVAR1DS = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 1 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_SHORT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        ISTAT   = NF_GET_VARA_INT2( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNCVAR1DS = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNCVAR1DS


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNCVAR1DB( FNAME, VNAME, NCOLS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS           !!  dimensions
        INTEGER(1)   , INTENT(  OUT) :: GRID( NCOLS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNCVAR1DB = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 1 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_BYTE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        ISTAT   = NF_GET_VARA_INT1( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNCVAR1DB = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNCVAR1DB


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNCVAR1DD( FNAME, VNAME, NCOLS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS           !!  dimensions
        REAL*8       , INTENT(  OUT) :: GRID( NCOLS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNCVAR1DD = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 1 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3DBLE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        ISTAT   = NF_GET_VARA_DOUBLE( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_DOUBLE() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNCVAR1DD = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNCVAR1DD


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-

    LOGICAL FUNCTION READNCVAR2DR( FNAME, VNAME, NCOLS, NROWS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS    !!  dimensions
        REAL         , INTENT(  OUT) :: GRID( NCOLS, NROWS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNCVAR2DR = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 2 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3REAL ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        ISTAT   = NF_GET_VARA_REAL( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_REAL() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNCVAR2DR = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNCVAR2DR


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNCVAR2DI( FNAME, VNAME, NCOLS, NROWS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS    !!  dimensions
        INTEGER      , INTENT(  OUT) :: GRID( NCOLS, NROWS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNCVAR2DI = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 2 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3INT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        ISTAT   = NF_GET_VARA_INT( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNCVAR2DI = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNCVAR2DI


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNCVAR2DS( FNAME, VNAME, NCOLS, NROWS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS    !!  dimensions
        INTEGER(2)   , INTENT(  OUT) :: GRID( NCOLS, NROWS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNCVAR2DS = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 2 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_SHORT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        ISTAT   = NF_GET_VARA_INT2( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNCVAR2DS = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNCVAR2DS


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNCVAR2DB( FNAME, VNAME, NCOLS, NROWS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS    !!  dimensions
        INTEGER(1)   , INTENT(  OUT) :: GRID( NCOLS, NROWS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNCVAR2DB = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 2 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_BYTE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        ISTAT   = NF_GET_VARA_INT1( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNCVAR2DB = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNCVAR2DB


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNCVAR2DD( FNAME, VNAME, NCOLS, NROWS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS    !!  dimensions
        REAL*8       , INTENT(  OUT) :: GRID( NCOLS, NROWS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNCVAR2DD = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 2 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3DBLE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        ISTAT   = NF_GET_VARA_DOUBLE( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_DOUBLE() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNCVAR2DD = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNCVAR2DD


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNCVAR3DR( FNAME, VNAME, NCOLS, NROWS, NLAYS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS, NLAYS    !!  dimensions
        REAL         , INTENT(  OUT) :: GRID( NCOLS, NROWS, NLAYS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNCVAR3DR = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 3 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3REAL ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        ISTAT   = NF_GET_VARA_REAL( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_REAL() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNCVAR3DR = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNCVAR3DR


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNCVAR3DI( FNAME, VNAME, NCOLS, NROWS, NLAYS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS, NLAYS    !!  dimensions
        INTEGER      , INTENT(  OUT) :: GRID( NCOLS, NROWS, NLAYS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNCVAR3DI = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 3 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3INT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        ISTAT   = NF_GET_VARA_INT( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNCVAR3DI = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNCVAR3DI


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNCVAR3DS( FNAME, VNAME, NCOLS, NROWS, NLAYS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS, NLAYS    !!  dimensions
        INTEGER(2)   , INTENT(  OUT) :: GRID( NCOLS, NROWS, NLAYS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNCVAR3DS = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 3 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_SHORT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        ISTAT   = NF_GET_VARA_INT2( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNCVAR3DS = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNCVAR3DS


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNCVAR3DB( FNAME, VNAME, NCOLS, NROWS, NLAYS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS, NLAYS    !!  dimensions
        INTEGER(1)   , INTENT(  OUT) :: GRID( NCOLS, NROWS, NLAYS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNCVAR3DB = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 3 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_BYTE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        ISTAT   = NF_GET_VARA_INT1( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNCVAR3DB = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNCVAR3DB


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNCVAR3DD( FNAME, VNAME, NCOLS, NROWS, NLAYS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS, NLAYS    !!  dimensions
        REAL*8       , INTENT(  OUT) :: GRID( NCOLS, NROWS, NLAYS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNCVAR3DD = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 3 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3DBLE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        ISTAT   = NF_GET_VARA_DOUBLE( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_DOUBLE() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNCVAR3DD = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNCVAR3DD


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNCVAR4DR( FNAME, VNAME, NCOLS, NROWS, NLAYS, NSPCS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS, NLAYS, NSPCS    !!  dimensions
        REAL         , INTENT(  OUT) :: GRID( NCOLS, NROWS, NLAYS, NSPCS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNCVAR4DR = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 4 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3REAL ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(4), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NSPCS ) THEN
            MESG = 'Bad SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = NSPCS
        ISTAT   = NF_GET_VARA_REAL( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_REAL() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNCVAR4DR = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNCVAR4DR


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNCVAR4DI( FNAME, VNAME, NCOLS, NROWS, NLAYS, NSPCS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS, NLAYS, NSPCS      !!  dimensions
        INTEGER      , INTENT(  OUT) :: GRID( NCOLS, NROWS, NLAYS, NSPCS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNCVAR4DI = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 4 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3INT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(4), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NSPCS ) THEN
            MESG = 'Bad SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = NSPCS
        ISTAT   = NF_GET_VARA_INT( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNCVAR4DI = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNCVAR4DI


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNCVAR4DS( FNAME, VNAME, NCOLS, NROWS, NLAYS, NSPCS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS, NLAYS, NSPCS      !!  dimensions
        INTEGER(2)   , INTENT(  OUT) :: GRID( NCOLS, NROWS, NLAYS, NSPCS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNCVAR4DS = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 4 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_SHORT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(4), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NSPCS ) THEN
            MESG = 'Bad SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = NSPCS
        ISTAT   = NF_GET_VARA_INT2( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNCVAR4DS = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNCVAR4DS


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNCVAR4DB( FNAME, VNAME, NCOLS, NROWS, NLAYS, NSPCS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS, NLAYS, NSPCS      !!  dimensions
        INTEGER(1)   , INTENT(  OUT) :: GRID( NCOLS, NROWS, NLAYS, NSPCS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNCVAR4DB = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 4 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_BYTE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(4), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NSPCS ) THEN
            MESG = 'Bad SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = NSPCS
        ISTAT   = NF_GET_VARA_INT1( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNCVAR4DB = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNCVAR4DB


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNCVAR4DD( FNAME, VNAME, NCOLS, NROWS, NLAYS, NSPCS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS, NLAYS, NSPCS      !!  dimensions
        REAL*8       , INTENT(  OUT) :: GRID( NCOLS, NROWS, NLAYS, NSPCS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNCVAR4DD = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 4 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3DBLE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(4), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NSPCS ) THEN
            MESG = 'Bad SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = NSPCS
        ISTAT   = NF_GET_VARA_DOUBLE( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_DOUBLE() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNCVAR4DD = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNCVAR4DD


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-
    !  SINGLE-INDEXED ("VECTOR") FORMS
    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNCVEC2DR( FNAME, VNAME, NCOLS, NROWS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS    !!  dimensions
        REAL         , INTENT(  OUT) :: GRID( NCOLS*NROWS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNCVEC2DR = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 2 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3REAL ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        ISTAT   = NF_GET_VARA_REAL( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_REAL() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNCVEC2DR = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNCVEC2DR


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNCVEC2DI( FNAME, VNAME, NCOLS, NROWS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS    !!  dimensions
        INTEGER      , INTENT(  OUT) :: GRID( NCOLS*NROWS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNCVEC2DI = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 2 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3INT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        ISTAT   = NF_GET_VARA_INT( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNCVEC2DI = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNCVEC2DI


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNCVEC2DS( FNAME, VNAME, NCOLS, NROWS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS    !!  dimensions
        INTEGER(2)   , INTENT(  OUT) :: GRID( NCOLS*NROWS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNCVEC2DS = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 2 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_SHORT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        ISTAT   = NF_GET_VARA_INT2( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNCVEC2DS = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNCVEC2DS


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNCVEC2DB( FNAME, VNAME, NCOLS, NROWS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS    !!  dimensions
        INTEGER(1)   , INTENT(  OUT) :: GRID( NCOLS*NROWS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNCVEC2DB = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 2 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_BYTE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        ISTAT   = NF_GET_VARA_INT1( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNCVEC2DB = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNCVEC2DB


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNCVEC2DD( FNAME, VNAME, NCOLS, NROWS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS    !!  dimensions
        REAL*8       , INTENT(  OUT) :: GRID( NCOLS*NROWS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNCVEC2DD = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 2 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3DBLE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        ISTAT   = NF_GET_VARA_DOUBLE( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_DOUBLE() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNCVEC2DD = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNCVEC2DD


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNCVEC3DR( FNAME, VNAME, NCOLS, NROWS, NLAYS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS, NLAYS    !!  dimensions
        REAL         , INTENT(  OUT) :: GRID( NCOLS*NROWS*NLAYS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNCVEC3DR = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 3 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3REAL ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        ISTAT   = NF_GET_VARA_REAL( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_REAL() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNCVEC3DR = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNCVEC3DR


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNCVEC3DI( FNAME, VNAME, NCOLS, NROWS, NLAYS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS, NLAYS    !!  dimensions
        INTEGER      , INTENT(  OUT) :: GRID( NCOLS*NROWS*NLAYS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNCVEC3DI = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 3 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3INT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        ISTAT   = NF_GET_VARA_INT( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNCVEC3DI = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNCVEC3DI


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNCVEC3DS( FNAME, VNAME, NCOLS, NROWS, NLAYS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS, NLAYS    !!  dimensions
        INTEGER(2)   , INTENT(  OUT) :: GRID( NCOLS*NROWS*NLAYS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNCVEC3DS = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 3 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_SHORT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        ISTAT   = NF_GET_VARA_INT2( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNCVEC3DS = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNCVEC3DS


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNCVEC3DB( FNAME, VNAME, NCOLS, NROWS, NLAYS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS, NLAYS    !!  dimensions
        INTEGER(1)   , INTENT(  OUT) :: GRID( NCOLS*NROWS*NLAYS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNCVEC3DB = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 3 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_BYTE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        ISTAT   = NF_GET_VARA_INT1( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNCVEC3DB = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNCVEC3DB


    LOGICAL FUNCTION READNCVEC3DD( FNAME, VNAME, NCOLS, NROWS, NLAYS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS, NLAYS    !!  dimensions
        REAL*8       , INTENT(  OUT) :: GRID( NCOLS*NROWS*NLAYS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNCVEC3DD = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 3 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3DBLE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        ISTAT   = NF_GET_VARA_DOUBLE( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_DOUBLE() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNCVEC3DD = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNCVEC3DD


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNCVEC4DR( FNAME, VNAME, NCOLS, NROWS, NLAYS, NSPCS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS, NLAYS, NSPCS    !!  dimensions
        REAL         , INTENT(  OUT) :: GRID( NCOLS*NROWS*NLAYS*NSPCS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNCVEC4DR = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 4 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3REAL ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(4), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NSPCS ) THEN
            MESG = 'Bad SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = NSPCS
        ISTAT   = NF_GET_VARA_REAL( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_REAL() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNCVEC4DR = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNCVEC4DR


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNCVEC4DI( FNAME, VNAME, NCOLS, NROWS, NLAYS, NSPCS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS, NLAYS, NSPCS    !!  dimensions
        INTEGER      , INTENT(  OUT) :: GRID( NCOLS*NROWS*NLAYS*NSPCS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNCVEC4DI = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 4 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3INT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(4), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NSPCS ) THEN
            MESG = 'Bad SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = NSPCS
        ISTAT   = NF_GET_VARA_INT( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNCVEC4DI = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNCVEC4DI


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNCVEC4DS( FNAME, VNAME, NCOLS, NROWS, NLAYS, NSPCS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS, NLAYS, NSPCS    !!  dimensions
        INTEGER(2)   , INTENT(  OUT) :: GRID( NCOLS*NROWS*NLAYS*NSPCS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNCVEC4DS = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 4 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_SHORT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(4), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NSPCS ) THEN
            MESG = 'Bad SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = NSPCS
        ISTAT   = NF_GET_VARA_INT2( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNCVEC4DS = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNCVEC4DS


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNCVEC4DB( FNAME, VNAME, NCOLS, NROWS, NLAYS, NSPCS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS, NLAYS, NSPCS    !!  dimensions
        INTEGER(1)   , INTENT(  OUT) :: GRID( NCOLS*NROWS*NLAYS*NSPCS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNCVEC4DB = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 4 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_BYTE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(4), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NSPCS ) THEN
            MESG = 'Bad SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = NSPCS
        ISTAT   = NF_GET_VARA_INT1( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNCVEC4DB = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNCVEC4DB


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNCVEC4DD( FNAME, VNAME, NCOLS, NROWS, NLAYS, NSPCS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS, NLAYS, NSPCS    !!  dimensions
        REAL*8       , INTENT(  OUT) :: GRID( NCOLS*NROWS*NLAYS*NSPCS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNCVEC4DD = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 4 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3DBLE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(4), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NSPCS ) THEN
            MESG = 'Bad SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = NSPCS
        ISTAT   = NF_GET_VARA_DOUBLE( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_DOUBLE() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNCVEC4DD = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNCVEC4DD


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-
    !!      TIME STEPPED FORMS READNVSTEP*()
    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNVSTEP0DR( FNAME, ISTEP, VNAME, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: ISTEP
        REAL         , INTENT(  OUT) :: GRID

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNVSTEP0DR = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 1 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(1) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3REAL ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        DIMS(1) = ISTEP
        DELS(1) = 1
        ISTAT   = NF_GET_VARA_REAL( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_REAL() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNVSTEP0DR = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNVSTEP0DR


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNVSTEP0DI( FNAME, ISTEP, VNAME, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: ISTEP
        INTEGER      , INTENT(  OUT) :: GRID

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNVSTEP0DI = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 1 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(1) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3INT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        DIMS(1) = ISTEP
        DELS(1) = 1
        ISTAT   = NF_GET_VARA_INT( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNVSTEP0DI = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNVSTEP0DI


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNVSTEP0DS( FNAME, ISTEP, VNAME, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: ISTEP
        INTEGER(2)   , INTENT(  OUT) :: GRID

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNVSTEP0DS = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 1 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(1) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_SHORT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        DIMS(1) = ISTEP
        DELS(1) = 1
        ISTAT   = NF_GET_VARA_INT2( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNVSTEP0DS = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNVSTEP0DS


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNVSTEP0DB( FNAME, ISTEP, VNAME, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: ISTEP
        INTEGER(1)   , INTENT(  OUT) :: GRID

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNVSTEP0DB = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 1 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(1) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_BYTE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        DIMS(1) = ISTEP
        DELS(1) = 1
        ISTAT   = NF_GET_VARA_INT1( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNVSTEP0DB = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNVSTEP0DB


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNVSTEP0DD( FNAME, ISTEP, VNAME, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: ISTEP
        REAL*8       , INTENT(  OUT) :: GRID

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNVSTEP0DD = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 1 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(1) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3DBLE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        DIMS(1) = ISTEP
        DELS(1) = 1
        ISTAT   = NF_GET_VARA_DOUBLE( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_DOUBLE() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNVSTEP0DD = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNVSTEP0DD


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNVSTEP1DR( FNAME, ISTEP, VNAME, NCOLS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: ISTEP, NCOLS    !!  dimensions
        REAL         , INTENT(  OUT) :: GRID( NCOLS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNVSTEP1DR = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 2 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(2) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3REAL ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        DIMS(1) = 1
        DIMS(2) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = 1
        ISTAT   = NF_GET_VARA_REAL( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_REAL() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNVSTEP1DR = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNVSTEP1DR


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNVSTEP1DI( FNAME, ISTEP, VNAME, NCOLS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: ISTEP, NCOLS      !!  dimensions
        INTEGER      , INTENT(  OUT) :: GRID( NCOLS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNVSTEP1DI = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 2 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(2) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3INT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = 1
        ISTAT   = NF_GET_VARA_INT( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNVSTEP1DI = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNVSTEP1DI


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNVSTEP1DS( FNAME, ISTEP, VNAME, NCOLS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                   !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                   !!  variable name
        INTEGER      , INTENT(IN   ) :: ISTEP, NCOLS            !!  dimensions
        INTEGER(2)   , INTENT(  OUT) :: GRID( NCOLS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNVSTEP1DS = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 2 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(2) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_SHORT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = 1
        ISTAT   = NF_GET_VARA_INT2( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNVSTEP1DS = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNVSTEP1DS


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNVSTEP1DB( FNAME, ISTEP, VNAME, NCOLS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: ISTEP, NCOLS      !!  dimensions
        INTEGER(1)   , INTENT(  OUT) :: GRID( NCOLS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNVSTEP1DB = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 2 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(2) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_BYTE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = 1
        ISTAT   = NF_GET_VARA_INT1( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNVSTEP1DB = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNVSTEP1DB


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNVSTEP1DD( FNAME, ISTEP, VNAME, NCOLS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: ISTEP, NCOLS      !!  dimensions
        REAL*8       , INTENT(  OUT) :: GRID( NCOLS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNVSTEP1DD = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 2 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(2) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3DBLE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = 1
        ISTAT   = NF_GET_VARA_DOUBLE( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_DOUBLE() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNVSTEP1DD = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNVSTEP1DD


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNVSTEP2DR( FNAME, ISTEP, VNAME, NCOLS, NROWS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: ISTEP, NCOLS, NROWS    !!  dimensions
        REAL         , INTENT(  OUT) :: GRID( NCOLS, NROWS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNVSTEP2DR = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 3 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(3) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3REAL ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = 1
        ISTAT   = NF_GET_VARA_REAL( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_REAL() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNVSTEP2DR = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNVSTEP2DR


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNVSTEP2DI( FNAME, ISTEP, VNAME, NCOLS, NROWS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: ISTEP, NCOLS, NROWS      !!  dimensions
        INTEGER      , INTENT(  OUT) :: GRID( NCOLS, NROWS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNVSTEP2DI = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 3 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(3) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3INT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = 1
        ISTAT   = NF_GET_VARA_INT( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNVSTEP2DI = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNVSTEP2DI


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNVSTEP2DS( FNAME, ISTEP, VNAME, NCOLS, NROWS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: ISTEP, NCOLS, NROWS      !!  dimensions
        INTEGER(2)   , INTENT(  OUT) :: GRID( NCOLS, NROWS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNVSTEP2DS = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 3 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(3) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_SHORT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = 1
        ISTAT   = NF_GET_VARA_INT2( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNVSTEP2DS = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNVSTEP2DS


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNVSTEP2DB( FNAME, ISTEP, VNAME, NCOLS, NROWS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: ISTEP, NCOLS, NROWS      !!  dimensions
        INTEGER(1)   , INTENT(  OUT) :: GRID( NCOLS, NROWS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNVSTEP2DB = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 3 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(3) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_BYTE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = 1
        ISTAT   = NF_GET_VARA_INT1( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNVSTEP2DB = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNVSTEP2DB


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNVSTEP2DD( FNAME, ISTEP, VNAME, NCOLS, NROWS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: ISTEP, NCOLS, NROWS      !!  dimensions
        REAL*8       , INTENT(  OUT) :: GRID( NCOLS, NROWS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNVSTEP2DD = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 3 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(3) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3DBLE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = 1
        ISTAT   = NF_GET_VARA_DOUBLE( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_DOUBLE() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNVSTEP2DD = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNVSTEP2DD


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-



    LOGICAL FUNCTION READNVSTEP3DR( FNAME, ISTEP, VNAME, NCOLS, NROWS, NLAYS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: ISTEP, NCOLS, NROWS, NLAYS    !!  dimensions
        REAL         , INTENT(  OUT) :: GRID( NCOLS, NROWS, NLAYS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNVSTEP3DR = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 4 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(4) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3REAL ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = 1
        DIMS(4) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = 1
        ISTAT   = NF_GET_VARA_REAL( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_REAL() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNVSTEP3DR = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNVSTEP3DR


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNVSTEP3DI( FNAME, ISTEP, VNAME, NCOLS, NROWS, NLAYS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: ISTEP, NCOLS, NROWS, NLAYS      !!  dimensions
        INTEGER      , INTENT(  OUT) :: GRID( NCOLS, NROWS, NLAYS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNVSTEP3DI = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 4 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(4) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3INT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = 1
        DIMS(4) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = 1
        ISTAT   = NF_GET_VARA_INT( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNVSTEP3DI = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNVSTEP3DI


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNVSTEP3DS( FNAME, ISTEP, VNAME, NCOLS, NROWS, NLAYS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: ISTEP, NCOLS, NROWS, NLAYS      !!  dimensions
        INTEGER(2)   , INTENT(  OUT) :: GRID( NCOLS, NROWS, NLAYS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNVSTEP3DS = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 4 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(4) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_SHORT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = 1
        DIMS(4) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = 1
        ISTAT   = NF_GET_VARA_INT2( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNVSTEP3DS = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNVSTEP3DS


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNVSTEP3DB( FNAME, ISTEP, VNAME, NCOLS, NROWS, NLAYS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: ISTEP, NCOLS, NROWS, NLAYS      !!  dimensions
        INTEGER(1)   , INTENT(  OUT) :: GRID( NCOLS, NROWS, NLAYS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNVSTEP3DB = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 4 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(4) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_BYTE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = 1
        DIMS(4) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = 1
        ISTAT   = NF_GET_VARA_INT1( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNVSTEP3DB = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNVSTEP3DB


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNVSTEP3DD( FNAME, ISTEP, VNAME, NCOLS, NROWS, NLAYS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: ISTEP, NCOLS, NROWS, NLAYS      !!  dimensions
        REAL*8       , INTENT(  OUT) :: GRID( NCOLS, NROWS, NLAYS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNVSTEP3DD = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 4 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(4) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3DBLE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = 1
        DIMS(4) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = 1
        ISTAT   = NF_GET_VARA_DOUBLE( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_DOUBLE() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNVSTEP3DD = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNVSTEP3DD


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNVSTEP4DR( FNAME, ISTEP, VNAME, NCOLS, NROWS, NLAYS, NSPCS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: ISTEP, NCOLS, NROWS, NLAYS, NSPCS    !!  dimensions
        REAL         , INTENT(  OUT) :: GRID( NCOLS, NROWS, NLAYS, NSPCS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNVSTEP4DR = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 5 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(5) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3REAL ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(4), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NSPCS ) THEN
            MESG = 'Bad SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = 1
        DIMS(4) = 1
        DIMS(5) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = NSPCS
        DELS(5) = 1
        ISTAT   = NF_GET_VARA_REAL( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_REAL() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNVSTEP4DR = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNVSTEP4DR


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNVSTEP4DI( FNAME, ISTEP, VNAME, NCOLS, NROWS, NLAYS, NSPCS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: ISTEP, NCOLS, NROWS, NLAYS, NSPCS      !!  dimensions
        INTEGER      , INTENT(  OUT) :: GRID( NCOLS, NROWS, NLAYS, NSPCS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNVSTEP4DI = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 5 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(5) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3INT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(4), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NSPCS ) THEN
            MESG = 'Bad SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = 1
        DIMS(4) = 1
        DIMS(5) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = NSPCS
        DELS(5) = 1
        ISTAT   = NF_GET_VARA_INT( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNVSTEP4DI = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNVSTEP4DI


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNVSTEP4DS( FNAME, ISTEP, VNAME, NCOLS, NROWS, NLAYS, NSPCS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: ISTEP, NCOLS, NROWS, NLAYS, NSPCS      !!  dimensions
        INTEGER(2)   , INTENT(  OUT) :: GRID( NCOLS, NROWS, NLAYS, NSPCS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNVSTEP4DS = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 5 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(5) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_SHORT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(4), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NSPCS ) THEN
            MESG = 'Bad SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = 1
        DIMS(4) = 1
        DIMS(5) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = NSPCS
        DELS(5) = 1
        ISTAT   = NF_GET_VARA_INT2( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNVSTEP4DS = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNVSTEP4DS


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNVSTEP4DB( FNAME, ISTEP, VNAME, NCOLS, NROWS, NLAYS, NSPCS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: ISTEP, NCOLS, NROWS, NLAYS, NSPCS      !!  dimensions
        INTEGER(1)   , INTENT(  OUT) :: GRID( NCOLS, NROWS, NLAYS, NSPCS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNVSTEP4DB = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 5 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(5) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_BYTE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(4), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NSPCS ) THEN
            MESG = 'Bad SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = 1
        DIMS(4) = 1
        DIMS(5) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = NSPCS
        DELS(5) = 1
        ISTAT   = NF_GET_VARA_INT1( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNVSTEP4DB = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNVSTEP4DB


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READNVSTEP4DD( FNAME, ISTEP, VNAME, NCOLS, NROWS, NLAYS, NSPCS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: ISTEP, NCOLS, NROWS, NLAYS, NSPCS      !!  dimensions
        REAL*8       , INTENT(  OUT) :: GRID( NCOLS, NROWS, NLAYS, NSPCS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_NOWRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            READNVSTEP4DD = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 5 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(5) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3DBLE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(4), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NSPCS ) THEN
            MESG = 'Bad SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = 1
        DIMS(4) = 1
        DIMS(5) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = NSPCS
        DELS(5) = 1
        ISTAT   = NF_GET_VARA_DOUBLE( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_DOUBLE() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        READNVSTEP4DD = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READNVSTEP4DD


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENCVAR0DR( FNAME, VNAME, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        REAL         , INTENT(IN   ) :: GRID

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENCVAR0DR = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 0 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3REAL ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        DIMS(:) = 1
        DELS(1) = 1
        ISTAT   = NF_PUT_VARA_REAL( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_REAL() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENCVAR0DR = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENCVAR0DR


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENCVAR0DI( FNAME, VNAME, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: GRID

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENCVAR0DI = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 0 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3INT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        DIMS(:) = 1
        DELS(1) = 1
        ISTAT   = NF_PUT_VARA_INT( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENCVAR0DI = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENCVAR0DI


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENCVAR0DS( FNAME, VNAME, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER(2)   , INTENT(IN   ) :: GRID

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENCVAR0DS = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 0 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_SHORT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        DIMS(:) = 1
        DELS(1) = 1
        ISTAT   = NF_PUT_VARA_INT2( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENCVAR0DS = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENCVAR0DS


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENCVAR0DB( FNAME, VNAME, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER(1)   , INTENT(IN   ) :: GRID

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENCVAR0DB = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 0 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_BYTE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        DIMS(:) = 1
        DELS(1) = 1
        ISTAT   = NF_PUT_VARA_INT1( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENCVAR0DB = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENCVAR0DB


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENCVAR0DD( FNAME, VNAME, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        REAL*8       , INTENT(IN   ) :: GRID

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENCVAR0DD = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 0 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3DBLE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        DIMS(:) = 1
        DELS(1) = 1
        ISTAT   = NF_PUT_VARA_DOUBLE( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_DOUBLE() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENCVAR0DD = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENCVAR0DD


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENCVAR1DR( FNAME, VNAME, NCOLS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS           !!  dimensions
        REAL         , INTENT(IN   ) :: GRID( NCOLS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENCVAR1DR = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 1 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3REAL ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        ISTAT   = NF_PUT_VARA_REAL( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_REAL() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENCVAR1DR = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENCVAR1DR


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENCVAR1DI( FNAME, VNAME, NCOLS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS           !!  dimensions
        INTEGER      , INTENT(IN   ) :: GRID( NCOLS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENCVAR1DI = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 1 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3INT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        ISTAT   = NF_PUT_VARA_INT( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENCVAR1DI = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENCVAR1DI


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENCVAR1DS( FNAME, VNAME, NCOLS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS           !!  dimensions
        INTEGER(2)   , INTENT(IN   ) :: GRID( NCOLS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENCVAR1DS = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 1 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_SHORT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        ISTAT   = NF_PUT_VARA_INT2( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENCVAR1DS = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENCVAR1DS


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENCVAR1DB( FNAME, VNAME, NCOLS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS           !!  dimensions
        INTEGER(1)   , INTENT(IN   ) :: GRID( NCOLS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENCVAR1DB = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 1 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_BYTE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        ISTAT   = NF_PUT_VARA_INT1( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENCVAR1DB = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENCVAR1DB


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENCVAR1DD( FNAME, VNAME, NCOLS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS           !!  dimensions
        REAL*8       , INTENT(IN   ) :: GRID( NCOLS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENCVAR1DD = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 1 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3DBLE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        ISTAT   = NF_PUT_VARA_DOUBLE( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_DOUBLE() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENCVAR1DD = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENCVAR1DD


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-

    LOGICAL FUNCTION WRITENCVAR2DR( FNAME, VNAME, NCOLS, NROWS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS    !!  dimensions
        REAL         , INTENT(IN   ) :: GRID( NCOLS, NROWS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENCVAR2DR = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 2 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3REAL ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        ISTAT   = NF_PUT_VARA_REAL( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_REAL() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENCVAR2DR = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENCVAR2DR


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENCVAR2DI( FNAME, VNAME, NCOLS, NROWS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS    !!  dimensions
        INTEGER      , INTENT(IN   ) :: GRID( NCOLS, NROWS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENCVAR2DI = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 2 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3INT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        ISTAT   = NF_PUT_VARA_INT( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENCVAR2DI = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENCVAR2DI


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENCVAR2DS( FNAME, VNAME, NCOLS, NROWS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS    !!  dimensions
        INTEGER(2)   , INTENT(IN   ) :: GRID( NCOLS, NROWS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENCVAR2DS = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 2 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_SHORT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        ISTAT   = NF_PUT_VARA_INT2( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENCVAR2DS = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENCVAR2DS


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENCVAR2DB( FNAME, VNAME, NCOLS, NROWS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS    !!  dimensions
        INTEGER(1)   , INTENT(IN   ) :: GRID( NCOLS, NROWS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENCVAR2DB = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 2 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_BYTE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        ISTAT   = NF_PUT_VARA_INT1( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENCVAR2DB = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENCVAR2DB


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENCVAR2DD( FNAME, VNAME, NCOLS, NROWS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS    !!  dimensions
        REAL*8       , INTENT(IN   ) :: GRID( NCOLS, NROWS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENCVAR2DD = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 2 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3DBLE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        ISTAT   = NF_PUT_VARA_DOUBLE( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_DOUBLE() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENCVAR2DD = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENCVAR2DD


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENCVAR3DR( FNAME, VNAME, NCOLS, NROWS, NLAYS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS, NLAYS    !!  dimensions
        REAL         , INTENT(IN   ) :: GRID( NCOLS, NROWS, NLAYS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENCVAR3DR = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 3 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3REAL ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        ISTAT   = NF_PUT_VARA_REAL( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_REAL() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENCVAR3DR = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENCVAR3DR


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENCVAR3DI( FNAME, VNAME, NCOLS, NROWS, NLAYS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS, NLAYS    !!  dimensions
        INTEGER      , INTENT(IN   ) :: GRID( NCOLS, NROWS, NLAYS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENCVAR3DI = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 3 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3INT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        ISTAT   = NF_PUT_VARA_INT( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENCVAR3DI = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENCVAR3DI


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENCVAR3DS( FNAME, VNAME, NCOLS, NROWS, NLAYS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS, NLAYS    !!  dimensions
        INTEGER(2)   , INTENT(IN   ) :: GRID( NCOLS, NROWS, NLAYS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENCVAR3DS = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 3 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_SHORT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        ISTAT   = NF_PUT_VARA_INT2( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENCVAR3DS = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENCVAR3DS


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENCVAR3DB( FNAME, VNAME, NCOLS, NROWS, NLAYS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS, NLAYS    !!  dimensions
        INTEGER(1)   , INTENT(IN   ) :: GRID( NCOLS, NROWS, NLAYS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENCVAR3DB = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 3 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_BYTE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        ISTAT   = NF_PUT_VARA_INT1( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENCVAR3DB = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENCVAR3DB


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENCVAR3DD( FNAME, VNAME, NCOLS, NROWS, NLAYS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS, NLAYS    !!  dimensions
        REAL*8       , INTENT(IN   ) :: GRID( NCOLS, NROWS, NLAYS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENCVAR3DD = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 3 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3DBLE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        ISTAT   = NF_PUT_VARA_DOUBLE( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_DOUBLE() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENCVAR3DD = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENCVAR3DD


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENCVAR4DR( FNAME, VNAME, NCOLS, NROWS, NLAYS, NSPCS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS, NLAYS, NSPCS    !!  dimensions
        REAL         , INTENT(IN   ) :: GRID( NCOLS, NROWS, NLAYS, NSPCS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENCVAR4DR = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 4 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3REAL ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(4), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NSPCS ) THEN
            MESG = 'Bad SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = NSPCS
        ISTAT   = NF_PUT_VARA_REAL( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_REAL() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENCVAR4DR = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENCVAR4DR


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENCVAR4DI( FNAME, VNAME, NCOLS, NROWS, NLAYS, NSPCS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS, NLAYS, NSPCS      !!  dimensions
        INTEGER      , INTENT(IN   ) :: GRID( NCOLS, NROWS, NLAYS, NSPCS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENCVAR4DI = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 4 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3INT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(4), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NSPCS ) THEN
            MESG = 'Bad SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = NSPCS
        ISTAT   = NF_PUT_VARA_INT( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENCVAR4DI = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENCVAR4DI


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENCVAR4DS( FNAME, VNAME, NCOLS, NROWS, NLAYS, NSPCS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS, NLAYS, NSPCS      !!  dimensions
        INTEGER(2)   , INTENT(IN   ) :: GRID( NCOLS, NROWS, NLAYS, NSPCS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENCVAR4DS = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 4 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_SHORT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(4), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NSPCS ) THEN
            MESG = 'Bad SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = NSPCS
        ISTAT   = NF_PUT_VARA_INT2( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENCVAR4DS = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENCVAR4DS


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENCVAR4DB( FNAME, VNAME, NCOLS, NROWS, NLAYS, NSPCS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS, NLAYS, NSPCS      !!  dimensions
        INTEGER(1)   , INTENT(IN   ) :: GRID( NCOLS, NROWS, NLAYS, NSPCS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENCVAR4DB = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 4 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_BYTE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(4), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NSPCS ) THEN
            MESG = 'Bad SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = NSPCS
        ISTAT   = NF_PUT_VARA_INT1( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENCVAR4DB = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENCVAR4DB


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENCVAR4DD( FNAME, VNAME, NCOLS, NROWS, NLAYS, NSPCS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS, NLAYS, NSPCS      !!  dimensions
        REAL*8       , INTENT(IN   ) :: GRID( NCOLS, NROWS, NLAYS, NSPCS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENCVAR4DD = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 4 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3DBLE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(4), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NSPCS ) THEN
            MESG = 'Bad SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = NSPCS
        ISTAT   = NF_PUT_VARA_DOUBLE( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_DOUBLE() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENCVAR4DD = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENCVAR4DD


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-
    !  SINGLE-INDEXED ("VECTOR") FORMS
    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENCVEC2DR( FNAME, VNAME, NCOLS, NROWS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS    !!  dimensions
        REAL         , INTENT(IN   ) :: GRID( NCOLS*NROWS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENCVEC2DR = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 2 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3REAL ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        ISTAT   = NF_PUT_VARA_REAL( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_REAL() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENCVEC2DR = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENCVEC2DR


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENCVEC2DI( FNAME, VNAME, NCOLS, NROWS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS    !!  dimensions
        INTEGER      , INTENT(IN   ) :: GRID( NCOLS*NROWS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENCVEC2DI = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 2 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3INT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        ISTAT   = NF_PUT_VARA_INT( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENCVEC2DI = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENCVEC2DI


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENCVEC2DS( FNAME, VNAME, NCOLS, NROWS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS    !!  dimensions
        INTEGER(2)   , INTENT(IN   ) :: GRID( NCOLS*NROWS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENCVEC2DS = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 2 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_SHORT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        ISTAT   = NF_PUT_VARA_INT2( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENCVEC2DS = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENCVEC2DS


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENCVEC2DB( FNAME, VNAME, NCOLS, NROWS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS    !!  dimensions
        INTEGER(1)   , INTENT(IN   ) :: GRID( NCOLS*NROWS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENCVEC2DB = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 2 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_BYTE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        ISTAT   = NF_PUT_VARA_INT1( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENCVEC2DB = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENCVEC2DB


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENCVEC2DD( FNAME, VNAME, NCOLS, NROWS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS    !!  dimensions
        REAL*8       , INTENT(IN   ) :: GRID( NCOLS*NROWS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENCVEC2DD = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 2 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3DBLE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        ISTAT   = NF_PUT_VARA_DOUBLE( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_DOUBLE() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENCVEC2DD = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENCVEC2DD


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENCVEC3DR( FNAME, VNAME, NCOLS, NROWS, NLAYS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS, NLAYS    !!  dimensions
        REAL         , INTENT(IN   ) :: GRID( NCOLS*NROWS*NLAYS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENCVEC3DR = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 3 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3REAL ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        ISTAT   = NF_PUT_VARA_REAL( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_REAL() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENCVEC3DR = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENCVEC3DR


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENCVEC3DI( FNAME, VNAME, NCOLS, NROWS, NLAYS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS, NLAYS    !!  dimensions
        INTEGER      , INTENT(IN   ) :: GRID( NCOLS*NROWS*NLAYS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENCVEC3DI = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 3 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3INT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        ISTAT   = NF_PUT_VARA_INT( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENCVEC3DI = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENCVEC3DI


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENCVEC3DS( FNAME, VNAME, NCOLS, NROWS, NLAYS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS, NLAYS    !!  dimensions
        INTEGER(2)   , INTENT(IN   ) :: GRID( NCOLS*NROWS*NLAYS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENCVEC3DS = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 3 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_SHORT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        ISTAT   = NF_PUT_VARA_INT2( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENCVEC3DS = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENCVEC3DS


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENCVEC3DB( FNAME, VNAME, NCOLS, NROWS, NLAYS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS, NLAYS    !!  dimensions
        INTEGER(1)   , INTENT(IN   ) :: GRID( NCOLS*NROWS*NLAYS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENCVEC3DB = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 3 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_BYTE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        ISTAT   = NF_PUT_VARA_INT1( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENCVEC3DB = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENCVEC3DB


    LOGICAL FUNCTION WRITENCVEC3DD( FNAME, VNAME, NCOLS, NROWS, NLAYS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS, NLAYS    !!  dimensions
        REAL*8       , INTENT(IN   ) :: GRID( NCOLS*NROWS*NLAYS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENCVEC3DD = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 3 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3DBLE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        ISTAT   = NF_PUT_VARA_DOUBLE( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_DOUBLE() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENCVEC3DD = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENCVEC3DD


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENCVEC4DR( FNAME, VNAME, NCOLS, NROWS, NLAYS, NSPCS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS, NLAYS, NSPCS    !!  dimensions
        REAL         , INTENT(IN   ) :: GRID( NCOLS*NROWS*NLAYS*NSPCS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENCVEC4DR = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 4 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3REAL ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(4), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NSPCS ) THEN
            MESG = 'Bad SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = NSPCS
        ISTAT   = NF_PUT_VARA_REAL( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_REAL() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENCVEC4DR = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENCVEC4DR


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENCVEC4DI( FNAME, VNAME, NCOLS, NROWS, NLAYS, NSPCS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS, NLAYS, NSPCS    !!  dimensions
        INTEGER      , INTENT(IN   ) :: GRID( NCOLS*NROWS*NLAYS*NSPCS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENCVEC4DI = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 4 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3INT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(4), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NSPCS ) THEN
            MESG = 'Bad SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = NSPCS
        ISTAT   = NF_PUT_VARA_INT( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENCVEC4DI = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENCVEC4DI


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENCVEC4DS( FNAME, VNAME, NCOLS, NROWS, NLAYS, NSPCS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS, NLAYS, NSPCS    !!  dimensions
        INTEGER(2)   , INTENT(IN   ) :: GRID( NCOLS*NROWS*NLAYS*NSPCS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENCVEC4DS = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 4 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_SHORT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(4), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NSPCS ) THEN
            MESG = 'Bad SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = NSPCS
        ISTAT   = NF_PUT_VARA_INT2( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENCVEC4DS = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENCVEC4DS


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENCVEC4DB( FNAME, VNAME, NCOLS, NROWS, NLAYS, NSPCS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS, NLAYS, NSPCS    !!  dimensions
        INTEGER(1)   , INTENT(IN   ) :: GRID( NCOLS*NROWS*NLAYS*NSPCS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENCVEC4DB = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 4 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_BYTE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(4), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NSPCS ) THEN
            MESG = 'Bad SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = NSPCS
        ISTAT   = NF_PUT_VARA_INT1( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENCVEC4DB = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENCVEC4DB


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENCVEC4DD( FNAME, VNAME, NCOLS, NROWS, NLAYS, NSPCS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: NCOLS, NROWS, NLAYS, NSPCS    !!  dimensions
        REAL*8       , INTENT(IN   ) :: GRID( NCOLS*NROWS*NLAYS*NSPCS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENCVEC4DD = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 4 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3DBLE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(4), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NSPCS ) THEN
            MESG = 'Bad SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(:) = 1
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = NSPCS
        ISTAT   = NF_PUT_VARA_DOUBLE( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_DOUBLE() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENCVEC4DD = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENCVEC4DD


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-
    !!      TIME STEPPED FORMS WRITENVSTEP*()
    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENVSTEP0DR( FNAME, ISTEP, VNAME, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: ISTEP
        REAL         , INTENT(IN   ) :: GRID

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENVSTEP0DR = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 1 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(1) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3REAL ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        DIMS(1) = ISTEP
        DELS(1) = 1
        ISTAT   = NF_PUT_VARA_REAL( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_REAL() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENVSTEP0DR = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENVSTEP0DR


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENVSTEP0DI( FNAME, ISTEP, VNAME, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: ISTEP
        INTEGER      , INTENT(IN   ) :: GRID

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENVSTEP0DI = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 1 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(1) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3INT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        DIMS(1) = ISTEP
        DELS(1) = 1
        ISTAT   = NF_PUT_VARA_INT( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENVSTEP0DI = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENVSTEP0DI


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENVSTEP0DS( FNAME, ISTEP, VNAME, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: ISTEP
        INTEGER(2)   , INTENT(IN   ) :: GRID

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENVSTEP0DS = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 1 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(1) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_SHORT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        DIMS(1) = ISTEP
        DELS(1) = 1
        ISTAT   = NF_PUT_VARA_INT2( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENVSTEP0DS = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENVSTEP0DS


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENVSTEP0DB( FNAME, ISTEP, VNAME, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: ISTEP
        INTEGER(1)   , INTENT(IN   ) :: GRID

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENVSTEP0DB = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 1 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(1) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_BYTE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        DIMS(1) = ISTEP
        DELS(1) = 1
        ISTAT   = NF_PUT_VARA_INT1( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENVSTEP0DB = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENVSTEP0DB


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENVSTEP0DD( FNAME, ISTEP, VNAME, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: ISTEP
        REAL*8       , INTENT(IN   ) :: GRID

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENVSTEP0DD = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 1 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(1) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3DBLE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        DIMS(1) = ISTEP
        DELS(1) = 1
        ISTAT   = NF_PUT_VARA_DOUBLE( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_DOUBLE() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENVSTEP0DD = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENVSTEP0DD


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENVSTEP1DR( FNAME, ISTEP, VNAME, NCOLS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: ISTEP, NCOLS    !!  dimensions
        REAL         , INTENT(IN   ) :: GRID( NCOLS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENVSTEP1DR = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 2 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(2) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3REAL ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        DIMS(1) = 1
        DIMS(2) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = 1
        ISTAT   = NF_PUT_VARA_REAL( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_REAL() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENVSTEP1DR = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENVSTEP1DR


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENVSTEP1DI( FNAME, ISTEP, VNAME, NCOLS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: ISTEP, NCOLS      !!  dimensions
        INTEGER      , INTENT(IN   ) :: GRID( NCOLS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENVSTEP1DI = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 2 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(2) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3INT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = 1
        ISTAT   = NF_PUT_VARA_INT( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENVSTEP1DI = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENVSTEP1DI


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENVSTEP1DS( FNAME, ISTEP, VNAME, NCOLS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                   !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                   !!  variable name
        INTEGER      , INTENT(IN   ) :: ISTEP, NCOLS            !!  dimensions
        INTEGER(2)   , INTENT(IN   ) :: GRID( NCOLS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENVSTEP1DS = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 2 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(2) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_SHORT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = 1
        ISTAT   = NF_PUT_VARA_INT2( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENVSTEP1DS = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENVSTEP1DS


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENVSTEP1DB( FNAME, ISTEP, VNAME, NCOLS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: ISTEP, NCOLS      !!  dimensions
        INTEGER(1)   , INTENT(IN   ) :: GRID( NCOLS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENVSTEP1DB = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 2 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(2) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_BYTE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = 1
        ISTAT   = NF_PUT_VARA_INT1( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENVSTEP1DB = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENVSTEP1DB


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENVSTEP1DD( FNAME, ISTEP, VNAME, NCOLS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: ISTEP, NCOLS      !!  dimensions
        REAL*8       , INTENT(IN   ) :: GRID( NCOLS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENVSTEP1DD = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 2 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(2) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3DBLE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = 1
        ISTAT   = NF_PUT_VARA_DOUBLE( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_DOUBLE() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENVSTEP1DD = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENVSTEP1DD


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENVSTEP2DR( FNAME, ISTEP, VNAME, NCOLS, NROWS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: ISTEP, NCOLS, NROWS    !!  dimensions
        REAL         , INTENT(IN   ) :: GRID( NCOLS, NROWS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENVSTEP2DR = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 3 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(3) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3REAL ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = 1
        ISTAT   = NF_PUT_VARA_REAL( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_REAL() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENVSTEP2DR = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENVSTEP2DR


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENVSTEP2DI( FNAME, ISTEP, VNAME, NCOLS, NROWS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: ISTEP, NCOLS, NROWS      !!  dimensions
        INTEGER      , INTENT(IN   ) :: GRID( NCOLS, NROWS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENVSTEP2DI = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 3 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(3) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3INT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = 1
        ISTAT   = NF_PUT_VARA_INT( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENVSTEP2DI = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENVSTEP2DI


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENVSTEP2DS( FNAME, ISTEP, VNAME, NCOLS, NROWS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: ISTEP, NCOLS, NROWS      !!  dimensions
        INTEGER(2)   , INTENT(IN   ) :: GRID( NCOLS, NROWS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENVSTEP2DS = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 3 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(3) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_SHORT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = 1
        ISTAT   = NF_PUT_VARA_INT2( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENVSTEP2DS = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENVSTEP2DS


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENVSTEP2DB( FNAME, ISTEP, VNAME, NCOLS, NROWS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: ISTEP, NCOLS, NROWS      !!  dimensions
        INTEGER(1)   , INTENT(IN   ) :: GRID( NCOLS, NROWS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENVSTEP2DB = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 3 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(3) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_BYTE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = 1
        ISTAT   = NF_PUT_VARA_INT1( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENVSTEP2DB = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENVSTEP2DB


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENVSTEP2DD( FNAME, ISTEP, VNAME, NCOLS, NROWS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: ISTEP, NCOLS, NROWS      !!  dimensions
        REAL*8       , INTENT(IN   ) :: GRID( NCOLS, NROWS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENVSTEP2DD = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 3 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(3) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3DBLE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = 1
        ISTAT   = NF_PUT_VARA_DOUBLE( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_DOUBLE() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENVSTEP2DD = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENVSTEP2DD


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-



    LOGICAL FUNCTION WRITENVSTEP3DR( FNAME, ISTEP, VNAME, NCOLS, NROWS, NLAYS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: ISTEP, NCOLS, NROWS, NLAYS    !!  dimensions
        REAL         , INTENT(IN   ) :: GRID( NCOLS, NROWS, NLAYS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENVSTEP3DR = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 4 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(4) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3REAL ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = 1
        DIMS(4) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = 1
        ISTAT   = NF_PUT_VARA_REAL( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_REAL() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENVSTEP3DR = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENVSTEP3DR


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENVSTEP3DI( FNAME, ISTEP, VNAME, NCOLS, NROWS, NLAYS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: ISTEP, NCOLS, NROWS, NLAYS      !!  dimensions
        INTEGER      , INTENT(IN   ) :: GRID( NCOLS, NROWS, NLAYS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENVSTEP3DI = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 4 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(4) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3INT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = 1
        DIMS(4) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = 1
        ISTAT   = NF_PUT_VARA_INT( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENVSTEP3DI = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENVSTEP3DI


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENVSTEP3DS( FNAME, ISTEP, VNAME, NCOLS, NROWS, NLAYS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: ISTEP, NCOLS, NROWS, NLAYS      !!  dimensions
        INTEGER(2)   , INTENT(IN   ) :: GRID( NCOLS, NROWS, NLAYS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENVSTEP3DS = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 4 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(4) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_SHORT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = 1
        DIMS(4) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = 1
        ISTAT   = NF_PUT_VARA_INT2( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENVSTEP3DS = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENVSTEP3DS


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENVSTEP3DB( FNAME, ISTEP, VNAME, NCOLS, NROWS, NLAYS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: ISTEP, NCOLS, NROWS, NLAYS      !!  dimensions
        INTEGER(1)   , INTENT(IN   ) :: GRID( NCOLS, NROWS, NLAYS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENVSTEP3DB = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 4 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(4) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_BYTE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = 1
        DIMS(4) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = 1
        ISTAT   = NF_PUT_VARA_INT1( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENVSTEP3DB = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENVSTEP3DB


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENVSTEP3DD( FNAME, ISTEP, VNAME, NCOLS, NROWS, NLAYS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: ISTEP, NCOLS, NROWS, NLAYS      !!  dimensions
        REAL*8       , INTENT(IN   ) :: GRID( NCOLS, NROWS, NLAYS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENVSTEP3DD = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 4 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(4) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3DBLE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = 1
        DIMS(4) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = 1
        ISTAT   = NF_PUT_VARA_DOUBLE( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_DOUBLE() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENVSTEP3DD = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENVSTEP3DD


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENVSTEP4DR( FNAME, ISTEP, VNAME, NCOLS, NROWS, NLAYS, NSPCS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: ISTEP, NCOLS, NROWS, NLAYS, NSPCS    !!  dimensions
        REAL         , INTENT(IN   ) :: GRID( NCOLS, NROWS, NLAYS, NSPCS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENVSTEP4DR = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 5 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(5) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3REAL ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(4), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NSPCS ) THEN
            MESG = 'Bad SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = 1
        DIMS(4) = 1
        DIMS(5) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = NSPCS
        DELS(5) = 1
        ISTAT   = NF_PUT_VARA_REAL( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_REAL() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENVSTEP4DR = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENVSTEP4DR


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENVSTEP4DI( FNAME, ISTEP, VNAME, NCOLS, NROWS, NLAYS, NSPCS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: ISTEP, NCOLS, NROWS, NLAYS, NSPCS      !!  dimensions
        INTEGER      , INTENT(IN   ) :: GRID( NCOLS, NROWS, NLAYS, NSPCS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENVSTEP4DI = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 5 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(5) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3INT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(4), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NSPCS ) THEN
            MESG = 'Bad SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = 1
        DIMS(4) = 1
        DIMS(5) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = NSPCS
        DELS(5) = 1
        ISTAT   = NF_PUT_VARA_INT( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENVSTEP4DI = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENVSTEP4DI


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENVSTEP4DS( FNAME, ISTEP, VNAME, NCOLS, NROWS, NLAYS, NSPCS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: ISTEP, NCOLS, NROWS, NLAYS, NSPCS      !!  dimensions
        INTEGER(2)   , INTENT(IN   ) :: GRID( NCOLS, NROWS, NLAYS, NSPCS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENVSTEP4DS = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 5 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(5) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_SHORT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(4), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NSPCS ) THEN
            MESG = 'Bad SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = 1
        DIMS(4) = 1
        DIMS(5) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = NSPCS
        DELS(5) = 1
        ISTAT   = NF_PUT_VARA_INT2( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENVSTEP4DS = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENVSTEP4DS


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENVSTEP4DB( FNAME, ISTEP, VNAME, NCOLS, NROWS, NLAYS, NSPCS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: ISTEP, NCOLS, NROWS, NLAYS, NSPCS      !!  dimensions
        INTEGER(1)   , INTENT(IN   ) :: GRID( NCOLS, NROWS, NLAYS, NSPCS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENVSTEP4DB = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 5 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(5) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_BYTE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(4), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NSPCS ) THEN
            MESG = 'Bad SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = 1
        DIMS(4) = 1
        DIMS(5) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = NSPCS
        DELS(5) = 1
        ISTAT   = NF_PUT_VARA_INT1( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENVSTEP4DB = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENVSTEP4DB


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITENVSTEP4DD( FNAME, ISTEP, VNAME, NCOLS, NROWS, NLAYS, NSPCS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: ISTEP, NCOLS, NROWS, NLAYS, NSPCS      !!  dimensions
        REAL*8       , INTENT(IN   ) :: GRID( NCOLS, NROWS, NLAYS, NSPCS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, EQNAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.

        CALL NAMEVAL( FNAME, EQNAME )

        ISTAT = NF_OPEN( EQNAME, NF_WRITE, FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error opening "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            WRITENVSTEP4DD = .FALSE.
            RETURN
        END IF          !  istat nonzero:  NF_OPEN() failed

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 5 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(5) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3DBLE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(4), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NSPCS ) THEN
            MESG = 'Bad SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = 1
        DIMS(4) = 1
        DIMS(5) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = NSPCS
        DELS(5) = 1
        ISTAT   = NF_PUT_VARA_DOUBLE( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_DOUBLE() failed


999     CONTINUE        !!  close FNAME and return

        ISTAT = NF_CLOSE( FID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error closing "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  istat nonzero:  NF_OPEN() failed

        WRITENVSTEP4DD = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITENVSTEP4DD



END MODULE MODNCFIO    !! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-



!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
!!      Support-functions needed for linking in non-netCDF4 mode:  return FALSE
!!      since netCDF-Fortran-4 not available
!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

#ifndef IOAPI_NCF4

#define NF_FATAL (1)

    INTEGER FUNCTION nf_put_var_int64( NCID, VARID, ivals )
        USE M3UTILIO
        INTEGER   ,INTENT(IN   )  ::     ncid, varid
        INTEGER(8),INTENT(IN   ) ::     ivals(*)
        nf_put_var_int64 = nf_fatal
        CALL M3MESG( 'NF_PUT_VAR_INT64:  netCDF-4 not enabled' )
        RETURN
    END FUNCTION nf_put_var_int64

    INTEGER FUNCTION nf_get_var_int64( NCID, VARID, ivals )
        USE M3UTILIO
        INTEGER   ,INTENT(IN   ) ::     ncid, varid
        INTEGER(8),INTENT(  OUT) ::     ivals(*)
        nf_get_var_int64 = nf_fatal
        CALL M3MESG( 'NF_GET_VAR_INT64:  netCDF-4 not enabled' )
        RETURN
    END FUNCTION nf_get_var_int64

    INTEGER FUNCTION nf_put_var1_int64( NCID, VARID, INDEX, ivals )
        USE M3UTILIO
        INTEGER   ,INTENT(IN   ) ::     ncid, varid, index(*)
        INTEGER(8),INTENT(IN   ) ::     ivals(*)
        nf_put_var1_int64 = nf_fatal
        CALL M3MESG( 'NF_PUT_VAR1_INT64:  netCDF-4 not enabled' )
        RETURN
    END FUNCTION nf_put_var1_int64

    INTEGER FUNCTION nf_get_var1_int64( NCID, VARID, INDEX, ivals )
        USE M3UTILIO
        INTEGER   ,INTENT(IN   ) ::     ncid, varid, index(*)
        INTEGER(8),INTENT(  OUT) ::     ivals(*)
        nf_get_var1_int64 = nf_fatal
        CALL M3MESG( 'NF_GET_VAR1_INT64:  netCDF-4 not enabled' )
        RETURN
    END FUNCTION nf_get_var1_int64

    INTEGER FUNCTION nf_put_vara_int64( NCID, VARID, START, COUNT, ivals )
        USE M3UTILIO
        INTEGER   ,INTENT(IN   ) ::     ncid, varid, start(*), count(*)
        INTEGER(8),INTENT(IN   ) ::     ivals(*)
        nf_put_vara_int64 = nf_fatal
        CALL M3MESG( 'NF_PUT_VARA_INT64:  netCDF-4 not enabled' )
        RETURN
    END FUNCTION nf_put_vara_int64

    INTEGER FUNCTION nf_get_vara_int64( NCID, VARID, START, COUNT, ivals )
        USE M3UTILIO
        INTEGER   ,INTENT(IN   ) ::     ncid, varid, start(*), count(*)
        INTEGER(8),INTENT(  OUT) ::     ivals(*)
        nf_get_vara_int64 = nf_fatal
        CALL M3MESG( 'NF_GET_VARA_INT64:  netCDF-4 not enabled' )
        RETURN
    END FUNCTION nf_get_vara_int64

    INTEGER FUNCTION nf_put_vars_int64( NCID, VARID, START, COUNT, STRIDE, ivals )
        USE M3UTILIO
        INTEGER   ,INTENT(IN   ) ::     ncid, varid, start(*), count(*), stride(*)
        INTEGER(8),INTENT(IN   ) ::     ivals(*)
        nf_put_vars_int64 = nf_fatal
        CALL M3MESG( 'NF_PUT_VARS_INT64:  netCDF-4 not enabled' )
        RETURN
    END FUNCTION nf_put_vars_int64

    INTEGER FUNCTION nf_get_vars_int64( NCID, VARID, START, COUNT, STRIDE, ivals )
        USE M3UTILIO
        INTEGER   ,INTENT(IN   ) ::     ncid, varid, start(*), count(*), stride(*)
        INTEGER(8),INTENT(  OUT) ::     ivals(*)
        nf_get_vars_int64 = nf_fatal
        CALL M3MESG( 'NF_GET_VARS_INT64:  netCDF-4 not enabled' )
        RETURN
    END FUNCTION nf_get_vars_int64

    INTEGER FUNCTION nf_put_varm_int64( NCID, VARID, START, COUNT, STRIDE, IMAP, ivals )
        USE M3UTILIO
        INTEGER   ,INTENT(IN   ) ::     ncid, varid, start(*), count(*), stride(*), imap(*)
        INTEGER(8),INTENT(IN   ) ::     ivals(*)
        nf_put_varm_int64 = nf_fatal
        CALL M3MESG( 'NF_PUT_VARM_INT64:  netCDF-4 not enabled' )
        RETURN
    END FUNCTION nf_put_varm_int64

    INTEGER FUNCTION nf_get_varm_int64( NCID, VARID, START, COUNT, STRIDE, IMAP, ivals )
        INTEGER   ,INTENT(IN   ) ::     ncid, varid, start(*), count(*), stride(*), imap(*)
        INTEGER(8),INTENT(  OUT) ::     ivals(*)
        nf_get_varm_int64 = nf_fatal
        CALL M3MESG( 'NF_GET_VARM_INT64:  netCDF-4 not enabled' )
        RETURN
    END FUNCTION nf_get_varm_int64


#endif
