
LOGICAL FUNCTION  OPNFIL3 ( EQNAME, FID, FSTATUS, PGNAME )

    !!***********************************************************************
    !! Version "$Id: opnfil3.F90 1 2017-06-10 18:05:20Z coats $"
    !! EDSS/Models-3 I/O API.
    !! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    !! (C) 2003-2010 Baron Advanced Meteorological Systems
    !! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    !! See file "LGPL.txt" for conditions of use.
    !!.........................................................................
    !!  function body starts at line  107
    !!
    !!  FUNCTION:
    !!       open pre-existing Models-3 file with logical name FLIST3( FID ),
    !!       with readonly/readwrite status FSTATUS.  If opened for write,
    !!       copy scenario description from I/O STATE3.EXT to file's
    !!       history, and name PGNAME of caller to file's updater-name.
    !!       Returns TRUE if the file is already open.
    !!
    !!  RETURN VALUE:
    !!       TRUE iff it succeeds in opening the file, reading its attributes,
    !!       and storing the relevant ones in STATE3.EXT
    !!
    !!  PRECONDITIONS REQUIRED:
    !!       File FLIST3( FID ) already exists.
    !!
    !!  SUBROUTINES AND FUNCTIONS CALLED:
    !!
    !!  REVISION  HISTORY:
    !!       prototype 9/1994 by CJC to go with new version of OPEN3()
    !!
    !!	    Modified  2/1995 by CJC to handle file type SMATRX3.
    !!
    !!	    Modified 10/1995 by CJC to handle file type TSRIES3.
    !!
    !!	    Modified  1/1997 by CJC to handle file type PTRFLY3.
    !!
    !!      revised   6/1999 by CJC:  OpenMP thread-safety
    !!
    !!      revised   3/2002 by CJC:  error message for bad ncclos() status,
    !!      for volatile, IERR = NF_sync() before return; no NF_SHARED  for CraysC
    !!
    !!      revised   3/2002 by CJC:  bug fix with OR-operation on FSTATUS
    !!
    !!      revised   3/2002 by CJC:  set NF_SHARE for volatile files
    !!
    !!      revised   6/2002 by CJC:  Don't set NF_SHARE for Cray
    !!
    !!      Modified 10/2003 by CJC for I/O API version 3:  support for
    !!      native-binary BINFIL3 file type
    !!
    !!      Modified 12/2004 by CJC:  MXVARS3 check
    !!
    !!      Modified 10/2009 by CJC:  flags-change for netCDF4  from
    !!      Martin Otte, US EPA -- remove NF_NOCLOBBER
    !!
    !!      Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !!
    !!      Modified 08/2015 by CJC for I/OL API-3.2: USE MODNCFIO
    !!
    !!      Modified 10/2015: use NF_*() instead of NC*(), for
    !!      netCDF-Fortran 4.x compatibility;  F90 "free" source format
    !!***********************************************************************

    USE MODNCFIO

    IMPLICIT NONE

    !!...........   INCLUDES:

    INCLUDE 'PARMS3.EXT'
    INCLUDE 'STATE3.EXT'


    !!...........   ARGUMENTS and their descriptions:

    CHARACTER*(*), INTENT(IN   ) :: EQNAME  !  physical name of file to be opened
    INTEGER      , INTENT(IN   ) :: FID     !  index into STATE3 tables
    INTEGER      , INTENT(IN   ) :: FSTATUS !  read-only/read-write status for NF_OPEN()
    CHARACTER*(*), INTENT(IN   ) :: PGNAME  !  name of calling program


    !!...........   EXTERNAL FUNCTIONS and their descriptions:

    LOGICAL, EXTERNAL :: CKFILE3 !  header-consistency check
    INTEGER, EXTERNAL :: OPNBIN3 !  open native-binary-mode files


    !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER              FNUM       !  netCDF file ID from NF_CREATE()
    INTEGER              FMODE      !  netCDF file-opening mode
    INTEGER              TDIM       !  netCDF dimension ID from NF_DIMID()
    INTEGER              IERR       !  netCDF error status return
    INTEGER              VAR        !  loop counter for file variables
    INTEGER              ND, NA
    INTEGER              D( MAXVDIMS )
    LOGICAL              EFLAG
    CHARACTER*(MAXNCNAM) TNAME      !  dummy arg for NF_INQ_DIM
    CHARACTER*512        EQ512      !  fixed-length buffer
    CHARACTER*256        MESG       !  fixed-length buffer

    !!.............................................................................
    !!   begin body of subroutine  OPNFIL3

    IF ( CDFID3( FID ) .EQ. BUFFIL3 ) THEN
        MESG = 'Error opening "BUFFERED" file ' // FLIST3( FID )
        CALL M3MSG2( MESG )
        CALL M3MSG2( 'File has not yet been created.' )
        OPNFIL3 = .FALSE.
        RETURN
    END IF

    IF ( FSTATUS .NE. NF_WRITE ) THEN
        FMODE = FSTATUS
    ELSE
#if _CRAY
        FMODE = NF_WRITE
#endif
#if ! ( _CRAY )
       FMODE = IOR( NF_SHARE, NF_WRITE )
#endif
    END IF
#if ! ( _CRAY )
    IF ( VOLAT3( FID ) ) THEN
        FMODE = IOR( FMODE , NF_SHARE )
    END IF
#endif

    EFLAG = .FALSE.

!$OMP CRITICAL( S_NC )

    !!...........  If BINIO3-mode file, do CRTBIN3 and return

    IF ( CDFID3( FID ) .EQ. BINFIL3 ) THEN
        EFLAG = ( 0 .EQ. OPNBIN3( EQNAME, FID, FSTATUS, PGNAME ) )
        GO TO 999
    END IF

    !!...........  Else open the netCDF file with that EQNAME....

    EQ512= EQNAME
    IERR = NF_OPEN( EQNAME, FMODE, FNUM )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error opening file at path-name:' )
        CALL M3MSG2( EQ512 )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  NF_CREATEMPI() failed
    CDFID3( FID ) = FNUM


    !!...........   Get attributes valid for all file types:
    !!...........   FTYPE:  file type ID

    IERR = NF_GET_ATT_INT( FNUM, NF_GLOBAL, 'FTYPE', FTYPE3( FID ) )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading netCDF file attribute FTYPE.' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed

    !!.......   NVARS:  number of variables

    IERR = NF_GET_ATT_INT( FNUM, NF_GLOBAL, 'NVARS', NVARS3( FID ) )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading netCDF file attribute NVARS' )
        EFLAG = .TRUE.
        GO TO 999
    ELSE IF ( NVARS3( FID ) .GT. MXVARS3 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Max value of attribute NVARS exceeded for this build' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed

    IF ( FTYPE3( FID ) .EQ. DCTNRY3 ) THEN

        !!...........   Set up variable FLAG:  "record-written" indicator, valid
        !!...........   for dictionary files:

        IERR = NF_INQ_VARID( FNUM, 'FLAG', TINDX3( FID ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading file attribute FLAG' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

    ELSE    ! Set up attributes and variables valid for nondictionary files:

        !!.......   TSTEP:  time step

        IF ( FTYPE3( FID ) .NE. KFEVNT3 ) THEN
            IERR = NF_GET_ATT_INT( FNUM, NF_GLOBAL, 'TSTEP', TSTEP3( FID ) )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading file attribute TSTEP' )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !  ierr nonzero:  operation failed
        END IF          !  if not kfevnt3

        !!.......   SDATE:  starting date (Julian date YYYYDDD)

        IERR = NF_GET_ATT_INT( FNUM, NF_GLOBAL, 'SDATE', SDATE3( FID )  )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading file attribute SDATE' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

        !!.......   STIME:  starting time (HHMMSS)

        IERR = NF_GET_ATT_INT( FNUM, NF_GLOBAL, 'STIME', STIME3( FID )  )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading file attribute STIME' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

        !!.......   NTHIK:  boundary file perimeter thickness (cells)

        IERR = NF_GET_ATT_INT( FNUM, NF_GLOBAL, 'NTHIK', NTHIK3( FID ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading file attribute NTHIK' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

        !!.......   NCOLS:  number of grid columns/profile levels

        IERR = NF_GET_ATT_INT( FNUM, NF_GLOBAL, 'NCOLS', NCOLS3( FID ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading file attribute NCOLS' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

        !!.......   NROWS:  number of grid rows/data sites

        IERR = NF_GET_ATT_INT( FNUM, NF_GLOBAL, 'NROWS', NROWS3( FID ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading file attribute NROWS' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

        !!.......   NLAYS:  number of layers

        IERR = NF_GET_ATT_INT( FNUM, NF_GLOBAL, 'NLAYS', NLAYS3( FID ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading file attribute NLAYS' )
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

        !!.......   GDTYP:  grid type ID (lat-lon, UTM, RADM, etc...)

        IERR = NF_GET_ATT_INT( FNUM, NF_GLOBAL, 'GDTYP', GDTYP3( FID ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading file attribute GDTYP' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed


        !!.......   P_ALP:  first map-projection-description angle

        IERR = NF_GET_ATT_DOUBLE( FNUM, NF_GLOBAL, 'P_ALP', P_ALP3( FID ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading file attribute P_ALP' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

        !!.......   P_BET:  second map-projection-description angle

        IERR = NF_GET_ATT_DOUBLE( FNUM, NF_GLOBAL, 'P_BET', P_BET3( FID ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading file attribute P_BET' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

        !!.......   P_GAM:  third map-projection-description angle

        IERR = NF_GET_ATT_DOUBLE( FNUM, NF_GLOBAL, 'P_GAM', P_GAM3( FID ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading file attribute P_GAM' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

        !!.......   XCENT:  lon of coordinate-system (0,0) origin

        IERR = NF_GET_ATT_DOUBLE( FNUM, NF_GLOBAL, 'XCENT', XCENT3( FID ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading file attribute XCENT' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

        !!.......   YCENT:  lat of coordinate-system (0,0) origin

        IERR = NF_GET_ATT_DOUBLE( FNUM, NF_GLOBAL, 'YCENT', YCENT3( FID ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading file attribute YCENT' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

        !!.......   XORIG:  X-coord of grid origin
        !!.......   (in map units; see FDESC3.EXT for description)

        IERR = NF_GET_ATT_DOUBLE( FNUM, NF_GLOBAL, 'XORIG', XORIG3( FID ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading file attribute XORIG' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

        !!.......   YORIG:  Y-coord of grid origin
        !!.......   (in map units; see FDESC3.EXT for description)

        IERR = NF_GET_ATT_DOUBLE( FNUM, NF_GLOBAL, 'YORIG', YORIG3( FID ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading file attribute YORIG' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

        !!.......   XCELL:  cell width (X direction)
        !!.......   (in map units; see FDESC3.EXT for description)

        IERR = NF_GET_ATT_DOUBLE( FNUM, NF_GLOBAL, 'XCELL', XCELL3( FID ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading file attribute XCELL' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

        !!.......   YCELL:  cell width (Y direction)
        !!.......   (in map units; see FDESC3.EXT for description)

        IERR = NF_GET_ATT_DOUBLE( FNUM, NF_GLOBAL, 'YCELL', YCELL3( FID ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading file attribute YCELL' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

        !!.......   VGTYP:  vertical coordinate type

        IERR = NF_GET_ATT_INT( FNUM, NF_GLOBAL, 'VGTYP', VGTYP3( FID ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading file attribute VGTYP.' )
            CALL M3MSG2( MESG )
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

        !!.......   VGTOP:  vertical  coordinate sigma-top

        IERR = NF_GET_ATT_REAL( FNUM, NF_GLOBAL, 'VGTOP', VGTOP3( FID ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading file attribute VGTOP.' )
            CALL M3MSG2( MESG )
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

        !!.......   VGLVS:  vertical coordinate values

        IERR = NF_GET_ATT_REAL( FNUM, NF_GLOBAL, 'VGLVLS', VGLVS3(1,FID) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading file attribute VGLVS.' )
            CALL M3MSG2( MESG )
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

        !!.......   GDNAM:  grid name

        IERR = NF_GET_ATT_TEXT( FNUM, NF_GLOBAL, 'GDNAM', GDNAM3( FID ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading file attribute GDNAM' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed


        !!...........   Get ID for timestep-available-flag variable:

        IERR = NF_INQ_VARID( FNUM, 'TFLAG', TINDX3( FID ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading file attribute TFLAG' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

        !!...........   Get extent of the time dimension:

        IERR = NF_INQ_DIMID( FNUM, 'TSTEP', TDIM )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading time-dimension ID' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_INQ_DIMLEN( FNUM, TDIM, MXREC3( FID ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading maximum timestep number' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

        !!...........   Now, get netCDF variable ID's

        IF ( FTYPE3( FID ) .EQ. CUSTOM3 )  THEN

            BSIZE3( FID ) = NCOLS3( FID )

        ELSE IF ( FTYPE3( FID ) .EQ. GRDDED3 )  THEN

            BSIZE3( FID ) = NCOLS3( FID ) * NROWS3( FID )

        ELSE IF ( FTYPE3( FID ) .EQ. BNDARY3 ) THEN

            BSIZE3( FID ) = 2 * ABS( NTHIK3( FID ) ) * ( NCOLS3( FID ) + NROWS3( FID ) + 2 * NTHIK3( FID ) )

        ELSE IF ( FTYPE3( FID ) .EQ. IDDATA3 ) THEN

            BSIZE3( FID ) = NROWS3( FID )

            IERR = NF_INQ_VARID( FNUM, 'ID-COUNT', NINDX3( FID ) )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading netcdf ID for variable ID-COUNT' )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !  ierr nonzero:  operation failed

            IERR = NF_INQ_VARID( FNUM, 'ID', SINDX3( FID ) )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading netcdf ID for variable ID' )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !  ierr nonzero:  operation failed

        ELSE IF ( FTYPE3( FID ) .EQ. PROFIL3 ) THEN

            BSIZE3( FID ) = NCOLS3( FID ) * NROWS3( FID )

            IERR = NF_INQ_VARID( FNUM, 'ID-COUNT', NINDX3( FID ) )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading netcdf ID for variable ID-COUNT' )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !  ierr nonzero:  operation failed

            IERR = NF_INQ_VARID( FNUM, 'ID', SINDX3( FID ) )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading netcdf ID for variable ID' )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !  ierr nonzero:  operation failed

            IERR = NF_INQ_VARID( FNUM, 'LEVEL-COUNT', LINDX3( FID ) )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading ID for variable LEVEL-COUNT' )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !  ierr nonzero:  operation failed

            IERR = NF_INQ_VARID( FNUM, 'X', XINDX3( FID ) )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading ID for variable X' )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !  ierr nonzero:  operation failed

            IERR = NF_INQ_VARID( FNUM, 'Y',YINDX3( FID ) )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading ID for variable Y' )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !  ierr nonzero:  operation failed

            IERR = NF_INQ_VARID( FNUM, 'Z', ZINDX3( FID ) )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading ID for variable Z' )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !  ierr nonzero:  operation failed

        ELSE IF ( FTYPE3( FID ) .EQ. GRNEST3 ) THEN

            BSIZE3( FID ) = NCOLS3( FID ) * NROWS3( FID )

            IERR = NF_INQ_VARID( FNUM, 'ID-COUNT', NINDX3( FID ) )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading ID for variable ID-COUNT' )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !  ierr nonzero:  operation failed

            IERR = NF_INQ_VARID( FNUM, 'ID', SINDX3( FID ) )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading netcdf ID for variable "ID"' )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !  ierr nonzero:  operation failed

            IERR = NF_INQ_VARID( FNUM, 'WIN-COLS', WCNDX3( FID ) )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading ID for variable WIN-COLS' )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !  ierr nonzero:  operation failed

            IERR = NF_INQ_VARID( FNUM, 'WIN-ROWS', WRNDX3( FID ) )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading ID for variable WIN-ROWST' )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !  ierr nonzero:  operation failed

            IERR = NF_INQ_VARID( FNUM, 'WIN-LEVS', LINDX3( FID ) )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading ID for variable WIN-LEVS' )
                MESG = 'File name:  ' // FLIST3( FID )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !  ierr nonzero:  operation failed

            IERR = NF_INQ_VARID( FNUM, 'X', XINDX3( FID ) )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading ID for variable X' )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !  ierr nonzero:  operation failed

            IERR = NF_INQ_VARID( FNUM, 'Y', YINDX3( FID ) )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading ID for variable Y' )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !  ierr nonzero:  operation failed

            IERR = NF_INQ_VARID( FNUM, 'DX', DXNDX3( FID ) )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading ID for variable DX' )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !  ierr nonzero:  operation failed

            IERR = NF_INQ_VARID( FNUM, 'DY', DYNDX3( FID ) )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading ID for variable DY' )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !  ierr nonzero:  operation failed

        ELSE IF ( FTYPE3( FID ) .EQ. SMATRX3 ) THEN

            BSIZE3( FID ) = NCOLS3( FID )

            IERR = NF_INQ_VARID( FNUM, 'COL-COUNT', SINDX3( FID ) )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading ID for variable COL-COUNT' )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !  ierr nonzero:  operation failed

            IERR = NF_INQ_VARID( FNUM, 'COEFF-INDEX', LINDX3( FID ) )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading ID for variable COEFF-INDEX' )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !  ierr nonzero:  operation failed

        ELSE IF ( FTYPE3( FID ) .EQ. TSRIES3 )  THEN

            BSIZE3( FID ) = NCOLS3( FID ) * NROWS3( FID )

        ELSE IF ( FTYPE3( FID ) .EQ. PTRFLY3 )  THEN

            BSIZE3( FID ) = NCOLS3( FID ) * NROWS3( FID )

        ELSE IF ( FTYPE3( FID ) .EQ. KFEVNT3 ) THEN

            BSIZE3( FID ) = NROWS3( FID )

            IERR = NF_INQ_VARID( FNUM, 'KFCOUNT', NINDX3( FID ) )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading netCDF ID for variable ID-COUNT' )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  NF_INQ_VARID() failed

            IERR = NF_INQ_VARID( FNUM, 'KFEVENT', SINDX3( FID ) )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading netCDF variable ID' )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  NF_INQ_VARID() failed

            IERR = NF_INQ_VARID( FNUM, 'KFSDATE', LINDX3( FID ) )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading netCDF ID for variable LEVEL-COUNT' )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  NF_INQ_VARID() failed

            IERR = NF_INQ_VARID( FNUM, 'KFSTIME', XINDX3( FID ) )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading netCDF ID for variable KFSDATE' )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  NF_INQ_VARID() failed

            IERR = NF_INQ_VARID( FNUM, 'KFLNGTH', YINDX3( FID ) )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading netCDF ID for variable KFLNGTH' )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !  ierr nonzero:  NF_INQ_VARID() failed

        ELSE    !  illegal file type

            MESG = 'ILLEGAL FILE TYPE for file ' // FLIST3( FID )
            CALL M3MSG2( MESG )
            WRITE( MESG,'(A,I5)' ) 'Unknown file type ', FTYPE3(FID)
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
            GO TO 999

        END IF      !  if file type custom, gridded, idlist, profile,
                    !  gridnest, smatrix, or none of the above.

    END IF          !  file type not dictionary


    !!.......   Variables-list for the file:

    IF ( NVARS3( FID ) .GT. 0 ) THEN
        IERR = NF_GET_ATT_TEXT( FNUM, NF_GLOBAL, 'VAR-LIST', VLIST3( 1,FID ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error reading attribute VAR-LIST' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed
    END IF              !  if nvars > 0

    DO  VAR = 1 , NVARS3( FID )

        IERR = NF_INQ_VARID( FNUM, VLIST3( VAR,FID ), VINDX3( VAR,FID ) )
        IF ( IERR .NE. 0 ) THEN
            MESG = 'Error reading IDs for  variable ' // VLIST3( VAR,FID )
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_INQ_VAR( FNUM, VINDX3( VAR,FID ), TNAME, VTYPE3( VAR,FID ), ND, D, NA )
        IF ( IERR .NE. 0 ) THEN
            MESG = 'Error reading type for  variable ' // VLIST3( VAR,FID )
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_GET_ATT_TEXT( FNUM, VINDX3( VAR,FID ), 'units', UNITS3( VAR,FID ) )
        IF ( IERR .NE. 0 ) THEN
            MESG = 'Error reading units for  variable ' // VLIST3( VAR,FID )
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

    END DO          !  end loop on variables


    !!.......   If opened for WRITE:  put attribute HISTORY:  update description

    IF ( IAND( FSTATUS, NF_WRITE ) .NE. 0 ) THEN

        IERR = NF_REDEF( FNUM )
        IF ( IERR .NE. 0 ) THEN
           CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error opening history for update' )
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_PUT_ATT_TEXT( FNUM, NF_GLOBAL, 'UPNAM', NAMLEN3, PGNAME )
        IF ( IERR .NE. 0 ) THEN
           CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error writing file attribute UPNAM' )
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_PUT_ATT_TEXT( FNUM, NF_GLOBAL, 'HISTORY', MXDLEN3 * MXDESC3, SCNDSC )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error writing file attribute HISTORY' )
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_ENDDEF( FNUM )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error flushing HISTORY to disk' )
            GO TO 999
        END IF          !  ierr nonzero:  operation failed

    END IF

    IF ( VOLAT3( FID ) ) THEN     !  volatile file:  synch with disk
        IERR =  NF_SYNC( CDFID3( FID ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error with disk synchronization' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !  if synch failed
    END IF          !  if file is volatile

    IF ( .NOT. CKFILE3( FID ) ) THEN
        IERR =  NF_CLOSE( CDFID3( FID ) )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error NF_CLOS-ing file' )
        END IF          !  ierr nonzero:  operation failed
        EFLAG = .TRUE.
    END IF

999     CONTINUE

!$OMP END CRITICAL( S_NC )

    IF ( EFLAG ) THEN
        OPNFIL3 = .FALSE.
!$OMP       CRITICAL( S_NC )
        IERR =  NF_CLOSE( CDFID3( FID ) )
!$OMP       END CRITICAL( S_NC )
        CDFID3( FID ) = IMISS3
        FLIST3( FID ) = CMISS3
    ELSE
        OPNFIL3 = .TRUE.
    END IF

    RETURN

END FUNCTION  OPNFIL3

