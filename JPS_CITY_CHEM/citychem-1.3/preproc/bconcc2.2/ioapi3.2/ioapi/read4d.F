
      LOGICAL FUNCTION READ4D( FNAME, VNAME, LAYER,
     &                         SDATE, STIME, TSTEP, NRECS, BUFFER )
     &                 RESULT( RDFLAG3 )

C***********************************************************************
C Version "$Id: read4d.F 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2013 Baron Advanced Meteorological Systems,
C (C) 2007-2013 Carlie J. Coats, Jr., and
C (C) 2014-2015 UNC Institute for the Environment.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  145
C
C  FUNCTION:
C       reads all the data for the specified LAYER and the timestep
C       sequence JDATE:JTIME:TSTEP:NRECS (formatted YYYYDDD and HHMMSS)
C       from the disk-resident time-stepped I/O API data file with
C       logical name FNAME, and stores it in BUFFER.
C       FNAME must be of type GRDDED3, BNDARY3, CUSTOM3, or TSERIES3.
C       If LAYER is ALLAYS3 = -1, reads all layers.
C       Does NOT support VNAME = ALLVARS3.
C
C  RETURN VALUE:
C       TRUE iff the entire operation succeeds (and the data is available)
C
C  PRECONDITIONS REQUIRED:
C       FNAME is a disk-resident I/O API data file already opened by OPEN3()
C       FNAME has type GRDDED3, BNDARY3, CUSTOM3, or TSERIES3
C       FNAME is time-stepped, and its variable VNAME has been written
C       to FNAME for all the timesteps in the requested sequence.
C       Storage order for BUFFER is according to Fortran storage order
C       for subscript tuple  ( ..., [ LAYERS, ] TIMESTEPS )
C
C  REVISION  HISTORY:
C       Adapted   8/1999 by CJC from READ3
C
C       Modified  1/2002 by CJC:  check TRIMLEN() of FNAME
C
C       Modified  3/2002 by CJC:  STATE3V changes; uses RDTFLAG()
C
C       Modified 7/2003 by CJC:  bugfix -- clean up critical sections
C       associated with INIT3()
C
C       Modified 10/2003 by CJC for I/O API version 3:  support for
C       native-binary BINFIL3 file type; uses INTEGER NAME2FID
C
C       Modified 11/2004 by CJC:  new "verbose-flag" argument to RDTFLAG
C
C       Bug-fix 2/2005 by Alex Zubrow (U. Chicago):  need to return
C       READ4D = .TRUE. for success.
C
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C
C       Modified 02/2015 by CJC for I/O API 3.2: USE M3UTILIO
C
C       Modified 02/2015 by CJC: USE MODNCFIO, NF_*() interfaces.
C       Support for M3INT8 variables
C***********************************************************************

        USE MODNCFIO
        USE M3UTILIO

        IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'STATE3.EXT'
#ifdef IOAPICPL
        INCLUDE 'STATE3V.EXT'
#endif


C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !  variable name, or 'ALL'
        INTEGER      , INTENT(IN   ) :: LAYER           !  layer number, or 0
        INTEGER      , INTENT(IN   ) :: SDATE           !  starting date, formatted YYYYDDD
        INTEGER      , INTENT(IN   ) :: STIME           !  starting time, formatted HHMMSS
        INTEGER      , INTENT(IN   ) :: TSTEP           !  time step,     formatted HHMMSS
        INTEGER      , INTENT(IN   ) :: NRECS           !  number of time steps
        REAL         , INTENT(  OUT) :: BUFFER(*)       !  input buffer array


C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER, EXTERNAL :: NAME2FID   !  fname~~> fid lookup
        LOGICAL, EXTERNAL :: RDTFLAG    !  compute time step availability
        LOGICAL, EXTERNAL :: RDVARS     !  read time step records
        EXTERNAL          :: INITBLK3        !!  BLOCK DATA to initialize STATE3 commons


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         F, FID          !  subscript  for STATE3 arrays
        INTEGER         VID, VAR, V     !  subscripts for STATE3 arrays
        INTEGER         FLEN, VLEN      !  name lengths for file, vble
        INTEGER         STEP            !  time step record number
        INTEGER         JDATE           !  date, formatted YYYYDDD
        INTEGER         JTIME           !  time, formatted HHMMSS
        INTEGER         IDUM            !  holds return value for INIT3()
        INTEGER         ITYP            !  holds file type
        INTEGER         DELTA           !  d(INDX) / d(NF_GET_VARA_* call)
        INTEGER         DIMS ( 5 )      !  start  arg array for NF_GET_VARA_*()
        INTEGER         DELTS( 5 )      !  corner arg array for NF_GET_VARA_*()
        INTEGER         IREC            !  record number:  loop counter
        INTEGER         IT              !  subscript for STEP
        INTEGER         IS              !  subscript for BUFFER
        INTEGER         IL0, IL1        !  layer extent
        CHARACTER*256   MESG
        LOGICAL         EFLAG

C...........   PARAMETERs:

        CHARACTER*7, PARAMETER :: TYPNAMES( -3 : 7 ) =
     &     (/ 'UNKNOWN',        !  -3:  file type error
     &        'DGRAPH3',        !  -2:  known file types
     &        'CUSTOM3',        !  -1:    "
     &        'DCTNRY3',        !   0:    "
     &        'GRDDED3',        !   1:    "
     &        'BNDARY3',        !   2:    "
     &        'IDDATA3',        !   3:    "
     &        'PROFIL3',        !   4:    "
     &        'GRNEST3',        !   5:    "
     &        'SMATRX3',        !   6:    "
     &        'UNKNOWN' /)      !   7:    "

#if _CRAY || REAL8
        INTEGER, PARAMETER :: TYPSIZE( 10 ) =
     &        (/ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 /)
#endif
#if ! ( _CRAY || REAL8 )
        INTEGER, PARAMETER :: TYPSIZE( 10 ) =
     &        (/ 1, 1, 1, 1, 1, 2, 1, 1, 1, 2 /)
#endif

C***********************************************************************
C   begin body of function  READ4D
C.......   Check that Models-3 I/O has been initialized:

        FLEN  = LEN_TRIM( FNAME )
        VLEN  = LEN_TRIM( VNAME )
        FID   = NAME2FID( FNAME )

        EFLAG = ( FID .LE. 0 )

        IF ( VLEN .GT. NAMLEN3 ) THEN
            EFLAG = .TRUE.
            MESG  = 'File "'// FNAME// '" Variable "'// VNAME//'"'
            CALL M3MSG2( MESG )
            WRITE( MESG, '( A, I10 )'  )
     &          'Max vble name length 16; actual:', VLEN
            CALL M3MSG2( MESG )
        END IF          !  if len( vname ) > 16

        IF ( EFLAG ) THEN
            MESG = 'Invalid variable or file name arguments'
            CALL M3WARN( 'READ3', JDATE, JTIME, MESG )
            RDFLAG3 = .FALSE.
            RETURN
        END IF          !  if len( fname ) > 16, or if len( vname ) > 16

C.......   Check availability of requested  layer, variable:

        IF ( CDFID3( FID ) .EQ. BUFFIL3 ) THEN     !  BUFFERED "file"

            MESG = 'File:  '//FNAME// ' is BUFFERED.'
            CALL M3WARN( 'READ4D', JDATE, JTIME, MESG )
            RDFLAG3 = .FALSE.
            RETURN

        ELSE IF ( CDFID3( FID ) .EQ. VIRFIL3 ) THEN

            MESG = 'File:  '//FNAME// ' is VIRTUAL.'
            CALL M3WARN( 'READ4D', JDATE, JTIME, MESG )
            RDFLAG3 = .FALSE.
            RETURN

        ELSE IF ( TSTEP3( FID ) .EQ. 0 ) THEN

            MESG = 'File:  '//FNAME// ' is time-independent.'
            CALL M3WARN( 'READ4D', JDATE, JTIME, MESG )
            RDFLAG3 = .FALSE.
            RETURN

        ELSE IF ( TSTEP3( FID ) .LT. 0 ) THEN

            MESG = 'File:  '//FNAME// ' is RESTART, not time-stepped.'
            CALL M3WARN( 'READ4D', JDATE, JTIME, MESG )
            RDFLAG3 = .FALSE.
            RETURN

        END IF          !  if file not available, or...


C.......   Check availability of requested  layer, variable:

        VID = INDEX1 ( VNAME, NVARS3( FID ), VLIST3( 1,FID ) )

        IF ( VID .EQ. 0 ) THEN
             MESG = 'File ' // FLIST3( FID )//' contains variables'
             CALL M3MSG2( MESG )
             DO  VAR=1,NVARS3( FID ), 3
                WRITE( MESG,91040 )
     &          (VLIST3( V,FID ),V=VAR,MIN( VAR+2, NVARS3( FID ) ) )
                CALL M3MSG2( MESG )
             END DO
             MESG = 'Requested variable "' //  VNAME //
     &              '" not available'
            CALL M3WARN( 'READ4D', JDATE, JTIME, MESG )
            RDFLAG3 = .FALSE.
            RETURN
        END IF          !  end check on vid

        IF ( LAYER .EQ. ALLAYS3 ) THEN

            IL0 = 1
            IL1 = NLAYS3( FID )

        ELSE IF ( LAYER .GE. 1  .AND.  LAYER .LE. NLAYS3( FID ) ) THEN

            IL0 = LAYER
            IL1 = 1
        ELSE

            WRITE( MESG,91010 ) 'Requested layer:', LAYER
            CALL M3MSG2( MESG )
            WRITE( MESG,91010 )
     &          'Layers in the file:  1 ...', NLAYS3( FID )
            CALL M3MSG2( MESG )
            MESG = 'LAYER not available in file ' // FNAME
            CALL M3WARN( 'READ4D', JDATE, JTIME, MESG )
            RDFLAG3 = .FALSE.
            RETURN

        END IF          !  end check on layer-number

C.......   Set up DIMS, DELS for RDVARS():

        ITYP = FTYPE3( FID )
        IF ( ITYP .EQ. CUSTOM3 ) THEN

            DIMS ( 1 ) = 1
            DELTS( 1 ) = NCOLS3( FID )

            DIMS ( 2 ) = IL0
            DELTS( 2 ) = IL1

            DELTS( 3 ) = 1
            IT = 3

            DELTA = NCOLS3( FID ) * NLAYS3( FID )

        ELSE IF ( ITYP .EQ. GRDDED3 ) THEN

            DIMS ( 1 ) = 1
            DELTS( 1 ) = NCOLS3( FID )

            DIMS ( 2 ) = 1
            DELTS( 2 ) = NROWS3( FID )

            DIMS ( 3 ) = IL0
            DELTS( 3 ) = IL1

            DELTS( 4 ) = 1
            IT = 4

            DELTA = NCOLS3( FID ) * NROWS3( FID ) * NLAYS3( FID )

        ELSE IF ( ITYP .EQ. BNDARY3 ) THEN

            DELTA = 2 * NTHIK3( FID )
            DELTA = DELTA * ( NCOLS3( FID ) + NROWS3( FID ) + DELTA )

            DIMS ( 1 ) = 1
            DELTS( 1 ) = DELTA

            DIMS ( 2 ) = IL0
            DELTS( 2 ) = IL1

            DELTS( 3 ) = 1

            DELTA = DELTA * NLAYS3( FID )

        ELSE IF ( ITYP .EQ. TSRIES3 ) THEN

            DIMS ( 1 ) = 1
            DELTS( 1 ) = NCOLS3( FID )

            DIMS ( 2 ) = 1
            DELTS( 2 ) = NROWS3( FID )

            DIMS ( 3 ) = IL0
            DELTS( 3 ) = IL1

            DELTS( 4 ) = 1
            IT = 4

            DELTA = NCOLS3( FID ) * NROWS3( FID ) * NLAYS3( FID )

        ELSE    !  unsupported file type:

            IDUM = MIN( MAX( ITYP, -3 ), 7 )
            WRITE( MESG, '( A, I7, 1X, 3A )' )
     &      'File type=', ITYP, '(', TYPNAMES( IDUM ), ')'
            CALL M3MSG2( MESG )
            MESG = 'Filetype not CUSTOM3, GRDDED3, BNDARY3, or TSRIES3'
            CALL M3WARN( 'READ4D', JDATE, JTIME, MESG )

            RDFLAG3 = .FALSE.
            RETURN

        END IF          !  if file type is ...


C...........   Compute record number, and check availability:

        IS    = 1
        JDATE = SDATE
        JTIME = STIME

C...........   Process the requested time step sequence:

        DO  IREC = 1, NRECS

            F = FID     !  rdtflag() returns file ID and step-number:

            IF ( .NOT. RDTFLAG( F,VID,JDATE,JTIME,STEP, .TRUE. ) ) THEN

                WRITE( MESG, '( 5A, I7.7, A, I6.6 )' )
     &              'Variable "', VNAME,
     &              '" not available in file "', FNAME,
     &              '" for date&time ', JDATE, ':', JTIME
                CALL M3WARN( 'READ4D', JDATE, JTIME, MESG )
                RDFLAG3 = .FALSE.
                RETURN

            END IF              ! if rdvars(0 failed

            DIMS( IT ) = STEP

            IF ( .NOT. RDVARS( F, VID,
     &                         DIMS, DELTS, DELTA,
     &                         BUFFER( IS ) ) ) THEN

                WRITE( MESG, '( 5A, I7.7, A, I6.6 )' )
     &              'Error reading "', VNAME, '" from "', FNAME,
     &              '" for date&time ', JDATE, ':', JTIME
                CALL M3WARN( 'READ4D', JDATE, JTIME, MESG )
                RDFLAG3 = .FALSE.
                RETURN

            END IF              ! if rdvars(0 failed

            !!  update state variable MXREC3D, and
            !!  increment time-flag and buffer-pointer:

            IS = IS + DELTA * TYPSIZE( VTYPE3( VID,FID ) )
            CALL NEXTIME( JDATE, JTIME, TSTEP )

        END DO          !  end loop on output records.

        RDFLAG3 = .TRUE.

        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Error and warning message formats..... 91xxx

91010   FORMAT ( 3 ( A , :, I5, :, 2X ) )

91040   FORMAT ( 3 ( '"', A16, '"', :, 4X ) )

        END FUNCTION READ4D

