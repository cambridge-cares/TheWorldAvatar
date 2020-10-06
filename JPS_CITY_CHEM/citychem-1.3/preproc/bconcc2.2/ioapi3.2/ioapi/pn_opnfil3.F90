
LOGICAL FUNCTION PN_OPNFIL3( EQNAME, FID, FSTATUS, PGNAME )

    !!***********************************************************************
    !! Version "$Id: pn_opnfil3.F90 1 2017-06-10 18:05:20Z coats $"
    !! EDSS/Models-3 I/O API.
    !! Copyright (C) 2014-2015 UNC Institute for the Environment.
    !! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    !! See file "LGPL.txt" for conditions of use.
    !!.........................................................................
    !!  function body starts at line  98
    !!
    !!  FUNCTION:
    !!      open pre-existing Models-3 file with logical name FLIST3( FID ),
    !!      with readonly/readwrite status FSTATUS.  If opened for write,
    !!      copy scenario description from I/O STATE3.EXT to file's
    !!      history, and name PGNAME of caller to file's updater-name.
    !!      Returns TRUE if the file is already open.
    !!
    !!  RETURN VALUE:
    !!      TRUE iff it succeeds in opening the file, reading its attributes,
    !!      and storing the relevant ones in STATE3.EXT
    !!
    !!  PRECONDITIONS REQUIRED:
    !!      File FLIST3( FID ) already exists.
    !!
    !!  SUBROUTINES AND FUNCTIONS CALLED:
    !!
    !!  REVISION  HISTORY:
    !!      Adapted 7/2015 by David Wong from "ioapi/opnfil3.f" to handle
    !!      distributed PnetCDF gridded file access.
    !!
    !!	    Major re-write  8/2015 by CJC: major cleanup and bugfixes.
    !!      USE M3UTILIO, MODNCFIO, MODPDATA
    !!***********************************************************************

#ifdef IOAPI_PNCF

    USE M3UTILIO
    USE MODNCFIO
    USE MODPDATA

    IMPLICIT NONE

     !!...........   INCLUDES:

    INCLUDE 'STATE3.EXT'
    INCLUDE 'mpif.h'

     !!...........   ARGUMENTS and their descriptions:

    CHARACTER*(*), INTENT(IN   ) :: EQNAME  !  physical name of file to be opened
    INTEGER      , INTENT(IN   ) :: FID     !  index into STATE3 tables
    INTEGER      , INTENT(IN   ) :: FSTATUS !  read-only/read-write status for NF_OPEN()
    CHARACTER*(*), INTENT(IN   ) :: PGNAME  !  name of calling program


     !!...........   EXTERNAL FUNCTIONS and their descriptions:

    LOGICAL, EXTERNAL :: CKFILE3 !  header-consistency check

     !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         FNUM       !  netCDF file ID from NFMPI_CREATE()
    INTEGER         FMODE      !  netCDF file-opening mode
    INTEGER         TDIM       !  netCDF dimension ID from NFMPI_INQ_DIMID
    INTEGER         IERR       !  netCDF error status return
    INTEGER         VAR        !  loop counter for file variables
    INTEGER         ND, NA
    INTEGER         D( MAXNCDIM )
    LOGICAL         EFLAG, AFLAG

    CHARACTER*512   TNAME      !  dummy arg for NFMPI_INQ_DIM
    CHARACTER*512   EQ512      !  fixed-length buffer
    CHARACTER*256   MESG       !  fixed-length buffer

    INTEGER :: LOC_WORLD_COMM

     !!.............................................................................
        !!   begin body of subroutine  PN_OPNFIL3

    IF ( .NOT. PN_IO_PE ) THEN
        FMODE = NF_NOWRITE
        LOC_WORLD_COMM = PN_NON_IO_COMM
    ELSE IF ( FSTATUS .NE. NF_WRITE ) THEN
        FMODE = FSTATUS
        LOC_WORLD_COMM = PN_WORLD_COMM
    ELSE
        FMODE = IOR( NF_SHARE, NF_WRITE )
        LOC_WORLD_COMM = PN_COL_IO_COMM
    END IF

    IF ( VOLAT3( FID ) ) THEN
        FMODE = IOR( FMODE , NF_SHARE )
    END IF

    EFLAG = .FALSE.

    EQ512 = EQNAME

     !!...........  Open the netCDF file with that EQNAME....

!$OMP CRITICAL( S_NC )

    IERR  = NFMPI_OPEN( LOC_WORLD_COMM, EQNAME, FMODE, MPI_INFO_NULL, FNUM )

    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_OPNFIL3:  Error opening PnetCDF file at path-name:' )
        CALL M3MSG2( EQNAME )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed
    CDFID3( FID ) = FNUM

     !!...........   Get attributes valid for all file types:
     !!...........   FTYPE:  file type ID

     IERR = NFMPI_GET_ATT_INT( FNUM, NF_GLOBAL, 'FTYPE', FTYPE3( FID ) )
     IF ( IERR .NE. 0 ) THEN
         CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_OPNFIL3:  Error reading PnetCDF file att FTYPE.' )
         EFLAG = .TRUE.
         GO TO 999
     ELSE IF ( FTYPE3( FID ) .NE. GRDDED3 ) THEN
         MESG  = 'PN_OPNFIL3:  Non-GRIDDED files not supported'
         EFLAG = .TRUE.
         CALL M3MESG( MESG )
         GO TO 999
     ELSE
         FTYPE3( FID ) = MPIGRD3
     END IF          !  ierr nonzero:  operation failed

   !!...........   NVARS:  number of variables

   IERR = NFMPI_GET_ATT_INT( FNUM, NF_GLOBAL, 'NVARS', NVARS3( FID ) )
   IF ( IERR .NE. 0 ) THEN
       CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_OPNFIL3:  Error reading netCDF file att NVARS' )
       EFLAG = .TRUE.
       GO TO 999
   ELSE IF ( NVARS3( FID ) .GT. MXVARS3 ) THEN
       CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_OPNFIL3:  Max value of attribute NVARS exceeded' )
       EFLAG = .TRUE.
       GO TO 999
   END IF          !  ierr nonzero:  operation failed

    IERR = NFMPI_GET_ATT_INT( FNUM, NF_GLOBAL, 'TSTEP', TSTEP3( FID ) )
    IF ( IERR .NE. 0 ) THEN
        CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_OPNFIL3:  Error reading file att TSTEP' )
        EFLAG = .TRUE.
        GO TO 999
    END IF          !  ierr nonzero:  operation failed

    !!...........   SDATE:  starting date (Julian date YYYYDDD)

   IERR = NFMPI_GET_ATT_INT( FNUM, NF_GLOBAL, 'SDATE', SDATE3( FID ) )
   IF ( IERR .NE. 0 ) THEN
       CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_OPNFIL3:  Error reading file attribute SDATE' )
       EFLAG = .TRUE.
       GO TO 999
   END IF          !  ierr nonzero:  operation failed

    !!...........   STIME:  starting time (HHMMSS)

   IERR = NFMPI_GET_ATT_INT( FNUM, NF_GLOBAL, 'STIME', STIME3( FID ) )
   IF ( IERR .NE. 0 ) THEN
       CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_OPNFIL3:  Error reading file attribute STIME' )
       EFLAG = .TRUE.
       GO TO 999
   END IF          !  ierr nonzero:  operation failed

    !!...........   NTHIK:  boundary file perimeter thickness (cells)

   IERR = NFMPI_GET_ATT_INT( FNUM, NF_GLOBAL, 'NTHIK', NTHIK3( FID ) )
   IF ( IERR .NE. 0 ) THEN
       CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_OPNFIL3:  Error reading file attribute NTHIK' )
       EFLAG = .TRUE.
       GO TO 999
   END IF          !  ierr nonzero:  nfmpi_get_att_int() failed

    !!...........   NCOLS:  number of grid columns/profile levels

   IERR = NFMPI_GET_ATT_INT( FNUM, NF_GLOBAL, 'NCOLS', NCOLS3( FID ) )
   IF ( IERR .NE. 0 ) THEN
       CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_OPNFIL3:  Error reading file attribute NCOLS' )
       EFLAG = .TRUE.
       GO TO 999
   ELSE IF ( NCOLS3( FID ) .NE. GRID_COLS ) THEN
       CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Attribute NCOLS does not match master grid' )
       EFLAG = .TRUE.
       GO TO 999
   END IF          !  ierr nonzero:  nfmpi_get_att_int() failed

    !!...........   NROWS:  number of grid rows/data sites

   IERR = NFMPI_GET_ATT_INT( FNUM, NF_GLOBAL, 'NROWS', NROWS3( FID ) )
   IF ( IERR .NE. 0 ) THEN
       CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_OPNFIL3:  Error reading file attribute NROWS' )
       EFLAG = .TRUE.
       GO TO 999
   ELSE IF ( NROWS3( FID ) .NE. GRID_ROWS ) THEN
       CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Attribute NROWS does not match master grid' )
       EFLAG = .TRUE.
       GO TO 999
   END IF          !  ierr nonzero:  nfmpi_get_att_int() failed

    !!...........   NLAYS:  number of layers

   IERR = NFMPI_GET_ATT_INT( FNUM, NF_GLOBAL, 'NLAYS', NLAYS3( FID ) )
   IF ( IERR .NE. 0 ) THEN
       CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_OPNFIL3:  Error reading file attribute NLAYS' )
       GO TO 999
   END IF          !  ierr nonzero:  nfmpi_get_att_int() failed

    !!...........   GDTYP:  grid type ID (lat-lon, UTM, RADM, etc...)

   IERR = NFMPI_GET_ATT_INT( FNUM, NF_GLOBAL, 'GDTYP', GDTYP3( FID ) )
   IF ( IERR .NE. 0 ) THEN
       CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_OPNFIL3:  Error reading file attribute GDTYP' )
       EFLAG = .TRUE.
       GO TO 999
   END IF          !  ierr nonzero:  operation failed


    !!...........   P_ALP:  first map-projection-description angle

   IERR = NFMPI_GET_ATT_DOUBLE( FNUM, NF_GLOBAL, 'P_ALP', P_ALP3( FID ) )
   IF ( IERR .NE. 0 ) THEN
       CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_OPNFIL3:  Error reading file attribute P_ALP' )
       EFLAG = .TRUE.
       GO TO 999
   END IF          !  ierr nonzero:  operation failed

    !!...........   P_BET:  second map-projection-description angle

   IERR = NFMPI_GET_ATT_DOUBLE( FNUM, NF_GLOBAL, 'P_BET', P_BET3( FID ) )
   IF ( IERR .NE. 0 ) THEN
       CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_OPNFIL3:  Error reading file attribute P_BET' )
       EFLAG = .TRUE.
       GO TO 999
   END IF          !  ierr nonzero:  operation failed

    !!...........   P_GAM:  third map-projection-description angle

   IERR = NFMPI_GET_ATT_DOUBLE( FNUM, NF_GLOBAL, 'P_GAM', P_GAM3( FID ) )
   IF ( IERR .NE. 0 ) THEN
       CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_OPNFIL3:  Error reading file attribute P_GAM' )
       EFLAG = .TRUE.
       GO TO 999
   END IF          !  ierr nonzero:  operation failed

    !!...........   XCENT:  lon of coordinate-system (0,0) origin

   IERR = NFMPI_GET_ATT_DOUBLE( FNUM, NF_GLOBAL, 'XCENT', XCENT3( FID ) )
   IF ( IERR .NE. 0 ) THEN
       CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_OPNFIL3:  Error reading file attribute XCENT' )
       EFLAG = .TRUE.
       GO TO 999
   END IF          !  ierr nonzero:  operation failed

    !!...........   YCENT:  lat of coordinate-system (0,0) origin

   IERR = NFMPI_GET_ATT_DOUBLE( FNUM, NF_GLOBAL, 'YCENT', YCENT3( FID ) )
   IF ( IERR .NE. 0 ) THEN
       CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_OPNFIL3:  Error reading file attribute YCENT' )
       EFLAG = .TRUE.
       GO TO 999
   END IF          !  ierr nonzero:  operation failed

    !!...........   XORIG:  X-coord of grid origin
    !!...........   (in map units; see FDESC3.EXT for description)

   IERR = NFMPI_GET_ATT_DOUBLE( FNUM, NF_GLOBAL, 'XORIG', XORIG3( FID ) )
   IF ( IERR .NE. 0 ) THEN
       CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_OPNFIL3:  Error reading file attribute XORIG' )
       EFLAG = .TRUE.
       GO TO 999
   END IF          !  ierr nonzero:  operation failed

    !!...........   YORIG:  Y-coord of grid origin
    !!...........   (in map units; see FDESC3.EXT for description)

   IERR = NFMPI_GET_ATT_DOUBLE( FNUM, NF_GLOBAL, 'YORIG', YORIG3( FID ) )
   IF ( IERR .NE. 0 ) THEN
       CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_OPNFIL3:  Error reading file attribute YORIG' )
       EFLAG = .TRUE.
       GO TO 999
   END IF          !  ierr nonzero:  operation failed

    !!...........   XCELL:  cell width (X direction)
    !!...........   (in map units; see FDESC3.EXT for description)

   IERR = NFMPI_GET_ATT_DOUBLE( FNUM, NF_GLOBAL, 'XCELL', XCELL3( FID ) )
   IF ( IERR .NE. 0 ) THEN
       CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_OPNFIL3:  Error reading file attribute XCELL' )
       EFLAG = .TRUE.
       GO TO 999
   END IF          !  ierr nonzero:  operation failed

    !!...........   YCELL:  cell width (Y direction)
    !!...........   (in map units; see FDESC3.EXT for description)

   IERR = NFMPI_GET_ATT_DOUBLE( FNUM, NF_GLOBAL, 'YCELL', YCELL3( FID ) )
   IF ( IERR .NE. 0 ) THEN
       CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_OPNFIL3:  Error reading file attribute YCELL' )
       EFLAG = .TRUE.
       GO TO 999
   END IF          !  ierr nonzero:  operation failed

    !!...........   VGTYP:  vertical coordinate type

   IERR = NFMPI_GET_ATT_INT( FNUM, NF_GLOBAL, 'VGTYP', VGTYP3( FID ) )
   IF ( IERR .NE. 0 ) THEN
       CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_OPNFIL3:  Error reading file att VGTYP.' )
       CALL M3MSG2( MESG )
       GO TO 999
   END IF          !  ierr nonzero:  operation failed

    !!...........   VGTOP:  vertical  coordinate sigma-top

   IERR = NFMPI_GET_ATT_REAL( FNUM, NF_GLOBAL, 'VGTOP', VGTOP3( FID ) )
   IF ( IERR .NE. 0 ) THEN
       CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_OPNFIL3:  Error reading file att VGTOP.' )
       CALL M3MSG2( MESG )
       GO TO 999
   END IF          !  ierr nonzero:  operation failed

    !!...........   VGLVS:  vertical coordinate values

   IERR = NFMPI_GET_ATT_REAL( FNUM, NF_GLOBAL, 'VGLVLS', VGLVS3(1,FID) )
   IF ( IERR .NE. 0 ) THEN
       CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_OPNFIL3:  Error reading file att VGLVS.' )
       CALL M3MSG2( MESG )
       GO TO 999
   END IF          !  ierr nonzero:  operation failed

    !!...........   GDNAM:  grid name

   IERR = NFMPI_GET_ATT_TEXT( FNUM, NF_GLOBAL, 'GDNAM', GDNAM3( FID ) )
   IF ( IERR .NE. 0 ) THEN
       CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_OPNFIL3:  Error reading file attribute GDNAM' )
       EFLAG = .TRUE.
       GO TO 999
   END IF          !  ierr nonzero:  operation failed


    !!...............   Get ID for timestep-available-flag variable:

   IERR = NFMPI_INQ_VARID( FNUM, 'TFLAG', TINDX3( FID ) )
   IF ( IERR .NE. 0 ) THEN
       CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_OPNFIL3:  Error reading file attribute TFLAG' )
       EFLAG = .TRUE.
       GO TO 999
   END IF          !  ierr nonzero:  operation failed

    !!...............   Get extent of the time dimension:

   IERR = NFMPI_INQ_DIMID( FNUM, 'TSTEP', TDIM )
   IF ( IERR .NE. 0 ) THEN
       CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_OPNFIL3:  Error reading time-dimension ID' )
       EFLAG = .TRUE.
       GO TO 999
   END IF          !  ierr nonzero:  operation failed

   IERR = NFMPI_INQ_DIM( FNUM, TDIM, TNAME, MXREC3( FID ) )

   IF ( IERR .NE. 0 ) THEN
       CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_OPNFIL3:  Error reading max timestep number' )
       EFLAG = .TRUE.
       GO TO 999
   END IF          !  ierr nonzero:  operation failed

   BSIZE3( FID ) = NCOLS3( FID ) * NROWS3( FID )


    !!.......   Variables-list for the file:

   IF ( NVARS3( FID ) .GT. 0 ) THEN
       IERR =  NFMPI_GET_ATT_TEXT( FNUM, NF_GLOBAL, 'VAR-LIST', VLIST3( 1,FID ) )
       IF ( IERR .NE. 0 ) THEN
           CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_OPNFIL3:  Error reading attribute VAR-LIST' )
           EFLAG = .TRUE.
           GO TO 999
       END IF          !  ierr nonzero:  operation failed
   END IF              !  if nvars > 0

   DO  VAR = 1 , NVARS3( FID )

       IERR = NFMPI_INQ_VARID( FNUM, VLIST3( VAR,FID ), VINDX3( VAR,FID ) )
       IF ( IERR .NE. 0 ) THEN
           MESG = 'PN_OPNFIL3:  Error reading IDs for  variable ' // VLIST3( VAR,FID )
           CALL M3ABORT( FLIST3( FID ), FNUM, IERR, MESG )
           EFLAG = .TRUE.
           GO TO 999
       END IF          !  ierr nonzero:  operation failed

       IERR = NFMPI_INQ_VAR( FNUM, VINDX3( VAR,FID ), TNAME, VTYPE3( VAR,FID ), ND, D, NA )
       IF ( IERR .NE. 0 ) THEN
           MESG = 'PN_OPNFIL3:  Error reading type for  variable ' // VLIST3( VAR,FID )
           CALL M3ABORT( FLIST3( FID ), FNUM, IERR, MESG )
           EFLAG = .TRUE.
           GO TO 999
       END IF          !  ierr nonzero:  operation failed

       IERR = NFMPI_GET_ATT_TEXT( FNUM, VINDX3( VAR,FID ), 'units', UNITS3( VAR,FID ) )
       IF ( IERR .NE. 0 ) THEN
           MESG = 'PN_OPNFIL3:  Error reading units for  variable ' // VLIST3( VAR,FID )
           CALL M3ABORT( FLIST3( FID ), FNUM, IERR, MESG )
           EFLAG = .TRUE.
           GO TO 999
       END IF          !  ierr nonzero:  operation failed

   END DO          !  end loop on variables

   !!.......   If opened for WRITE:  put attribute HISTORY:  update description

   IF ( PN_IO_PE .AND. IAND( FSTATUS, NF_WRITE ) .NE. 0 ) THEN

       IERR = NFMPI_REDEF( FNUM )
       IF ( IERR .NE. 0 ) THEN
          CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_OPNFIL3:  Error opening history for update' )
           GO TO 999
       END IF          !  ierr nonzero:  operation failed

       IERR = NFMPI_PUT_ATT_TEXT( FNUM, NF_GLOBAL, 'UPNAM', PN_NAMLEN, PGNAME )
       IF ( IERR .NE. 0 ) THEN
          CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_OPNFIL3:  Error writing file attribute UPNAM' )
           GO TO 999
       END IF          !  ierr nonzero:  operation failed

       IERR = NFMPI_PUT_ATT_TEXT( FNUM, NF_GLOBAL, 'HISTORY', PN_DSCLEN, SCNDSC )
       IF ( IERR .NE. 0 ) THEN
           CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_OPNFIL3:  Error writing file attribute HISTORY' )
           GO TO 999
       END IF          !  ierr nonzero:  operation failed

       IERR = NFMPI_ENDDEF( FNUM )
       IF ( IERR .NE. 0 ) THEN
           CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_OPNFIL3:  Error flushing HISTORY to disk' )
           GO TO 999
       END IF          !  ierr nonzero:  operation failed

       IF ( VOLAT3( FID ) ) THEN     !  volatile file:  synch with disk
           IERR = NFMPI_SYNC( FNUM )
           IF ( IERR .NE. 0 ) THEN
               CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'PN_OPNFIL3:  Error with disk synchronization' )
               EFLAG = .TRUE.
               GO TO 999
           END IF                   !  if synch failed
       END IF          !  if file is volatile

       IF ( .NOT. CKFILE3( FID ) ) THEN
           EFLAG = .TRUE.
           GO TO 999
       END IF

   END IF

999 CONTINUE

!$OMP END CRITICAL( S_NC )

    IF ( FTYPE3( FID ) .EQ. MPIGRD3 ) THEN
        AFLAG = ( .NOT.EFLAG )
        IF ( .NOT.PN_FLAG( AFLAG ) ) THEN
            CALL M3MSG2( 'WRTFLAG:  MPI_SEND(EFLAG) error' )
            EFLAG = .TRUE.          
        END IF
    END IF

    IF ( EFLAG ) THEN
        PN_OPNFIL3    = .FALSE.
!$OMP   CRITICAL( S_NC )
        IERR          = NFMPI_CLOSE( FNUM )
        CDFID3( FID ) = IMISS3
        FLIST3( FID ) = CMISS3
!$OMP   END CRITICAL( S_NC )
    END IF

    PN_OPNFIL3 = ( .NOT.EFLAG )

#endif
#ifndef IOAPI_PNCF

    CALL M3MESG( 'PN_OPNFIL3 error:  PnetCDF Mode not active.' )
    PN_OPNFIL3 = .FALSE.

#endif

    RETURN

END FUNCTION  PN_OPNFIL3

