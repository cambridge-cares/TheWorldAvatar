
LOGICAL   FUNCTION DESC3( FNAME )

    !!***********************************************************************
    !! Version "$Id: desc3.F90 1 2017-06-10 18:05:20Z coats $"
    !! EDSS/Models-3 I/O API.
    !! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    !! (C) 2003-2011 Baron Advanced Meteorological Systems,
    !! (C) 2011 David Wong, US EPA, (C) 2007-2013 Carlie J. Coats, Jr., and 
    !! (C) 2014-2015 UNC Institute for the Environment.
    !! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    !! See file "LGPL.txt" for conditions of use.
    !!.........................................................................
    !!  function body starts at line 126
    !!
    !!  FUNCTION:
    !!       puts description of FNAME into file description commons,
    !!       found in include file FDESC3.EXT.
    !!
    !!  RETURN VALUE:
    !!       TRUE iff successful
    !!
    !!  PRECONDITIONS REQUIRED:
    !!       Models-3 file with logical name FNAME must have already
    !!       been opened by OPEN3()
    !!
    !!  SUBROUTINES AND FUNCTIONS CALLED:
    !!
    !!  REVISION  HISTORY:
    !!      prototype 3/1992 by CJC
    !!
    !!      modified  7/1994 by CJC to return new coordinate-system and
    !!      grid-description parameters
    !!
    !!      Modified 10/1996 by CJC to support file type TSERIES3
    !!
    !!      Modified  5/1998 by CJC for OpenMP thread-safety
    !!
    !!      Modified  5/1999 by ALT for coupling-mode operation
    !!
    !!      Modified  1/2002 by CJC:  check TRIMLEN() of FNAME
    !!
    !!      Modified  3/2002 by CJC:  STATE3V changes
    !!
    !!      Modified 7/2003 by CJC:  bugfix -- clean up critical sections
    !!      associated with INIT3()
    !!
    !!      Modified 10/2003 by CJC for I/O API version 3:  support for
    !!      native-binary BINFIL3 file type; uses new INTEGER NAME2FID and
    !!      LOGICAL SYNCFID; more critical sections for OpenMP thread-safety
    !!
    !!      Modified 06/2005 by CJC:  formatting
    !!
    !!      Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !!
    !!      Revised 4/2011 by David Wong, US EPA, and by CJC, to add state for
    !!      full buffered-file file descriptions.  Re-organized accordingly CJC
    !!
    !!      Modified 05/2011 by CJC:  better error-message
    !!
    !!      Modified 08/2015 by CJC for I/O API 3.2:  F90 "free" source-format;
    !!      support for MPI/PnetCDF; USE MODNCFIO, MODPDATA, NF_*() interfaces
    !!
    !!      Modified 12/2015 by CJC:  bug CDF ~~> FID
    !!***********************************************************************

    USE MODNCFIO
    USE MODPDATA

    IMPLICIT NONE

    !!...........   INCLUDES:

    INCLUDE 'PARMS3.EXT'
    INCLUDE 'FDESC3.EXT'
    INCLUDE 'STATE3.EXT'
    INCLUDE 'ATDSC3.EXT'
#ifdef IOAPICPL
    INCLUDE 'STATE3V.EXT'
#endif
#ifdef  IOAPI_PNCF
    INCLUDE 'mpif.h'
#endif

    !!...........   ARGUMENTS and their descriptions:

    CHARACTER*(*), INTENT(IN   ) :: FNAME   !  logical name of file to be described.


    !!...........   EXTERNAL FUNCTIONS & BLOCK DATA

    INTEGER, EXTERNAL :: INDEX1
    INTEGER, EXTERNAL :: DSCBIN3    !  native-binary (BINIO3) description stuff
    INTEGER, EXTERNAL :: NAME2FID   !  fname~~> fid lookup
    LOGICAL, EXTERNAL :: SYNCFID

    EXTERNAL :: INITBLK3   !  block data: initialize I/O state

#ifdef IOAPICPL
    LOGICAL, EXTERNAL :: DESC3V
#endif


    !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         FID, VID        !  subscript for STATE3 arrays
    INTEGER         CDF             !  netCDF file ID from NCCRE()
    INTEGER         IERR            !  netCDF error status return
    INTEGER         VAR, LVL, ATT   !  loop counter for file variables
    LOGICAL         EFLAG
    CHARACTER*16    VNAME
    CHARACTER*256   MESG            !  message-buffer

#ifdef  IOAPI_PNCF
    INTEGER, PARAMETER :: PTYPE( 4 ) = (/ MPI_DOUBLE_PRECISION, MPI_INTEGER, MPI_REAL,  MPI_INTEGER /)
    INTEGER, PARAMETER :: PDIMS( 4 ) = (/ 9,                    16,          MXLAYS3+2, MXVARS3     /)
    INTEGER, PARAMETER :: CSIZE      = NAMLEN3*(2*MXVARS3 + 2 ) + MXDLEN3*( 2*MXDESC3 + 1 )
    INTEGER, SAVE      :: PLOCS( 4 )
    INTEGER, SAVE      :: ISIZE, RSIZE, DSIZE
    INTEGER, SAVE      :: FDTYPE = IMISS3
#endif

    !!.............................................................................
    !!   begin body of subroutine  OPEN3

!$OMP SINGLE

    !!.......   Find STATE3 index for the file:

    EFLAG = .FALSE.
    FID   = NAME2FID( FNAME )

    IF ( FID .EQ. 0 ) THEN
        MESG = 'Invalid file name argument "' // FNAME // '"'
        CALL M3WARN( 'DESC3', 0, 0, MESG )
        EFLAG = .TRUE.
        GO TO  99
    END IF

    CDF   = CDFID3( FID )

    !!.......   Characteristics stored in STATE3.EXT:

    FTYPE3D = FTYPE3( FID )

    NVARS3D = NVARS3( FID )
    DO   VAR = 1, NVARS3D
        VNAME3D( VAR ) = VLIST3( VAR,FID )
        VTYPE3D( VAR ) = VTYPE3( VAR,FID )
        UNITS3D( VAR ) = UNITS3( VAR,FID )
    END DO

    IF ( FTYPE3D .NE. DCTNRY3 ) THEN

        SDATE3D = SDATE3( FID )
        STIME3D = STIME3( FID )
        TSTEP3D = TSTEP3( FID )
        MXREC3D = MXREC3( FID )
        NTHIK3D = NTHIK3( FID )
        NCOLS3D = NCOLS3( FID )
        NROWS3D = NROWS3( FID )
        NLAYS3D = NLAYS3( FID )
        GDTYP3D = GDTYP3( FID )
        P_ALP3D = P_ALP3( FID )
        P_BET3D = P_BET3( FID )
        P_GAM3D = P_GAM3( FID )
        XCENT3D = XCENT3( FID )
        YCENT3D = YCENT3( FID )
        XORIG3D = XORIG3( FID )
        YORIG3D = YORIG3( FID )
        XCELL3D = XCELL3( FID )
        YCELL3D = YCELL3( FID )
        GDNAM3D = GDNAM3( FID )

        VGTYP3D = VGTYP3( FID )
        VGTOP3D = VGTOP3( FID )

        DO LVL = 1, NLAYS3D+1
            VGLVS3D( LVL ) = VGLVS3( LVL,FID )
        END DO

    END IF  !  if not a dictionary file


    IF ( CDF .EQ. BUFFIL3 ) THEN

        EXECN3D = EXECN3
        CDATE3D = CURDATE
        CTIME3D = CURTIME
        WDATE3D = CURDATE
        WTIME3D = CURTIME
        UPNAM3D = CMISS3
        FDESC3D = ' '
        UPDSC3D = ' '
        GO TO  99

    END IF          !  if "buffered" file

#ifdef IOAPICPL

    IF ( CDFID3( FID ) .EQ. VIRFIL3 ) THEN     !  VIRTUAL "file"
        EFLAG  = ( .NOT.DESC3V( PLIST3( FID ) ) )
        GO TO  99
    END IF          !  if "virtual" file

#endif

    IF ( VOLAT3( FID ) ) THEN      !  volatile file:  synch with disk

        IF ( .NOT. SYNCFID( FID ) ) THEN
            MESG = 'Error with disk synch for file '//FLIST3( FID )
            CALL M3WARN( 'DESC3', 0, 0, MESG )
            EFLAG = .TRUE.
            GO TO  99
        END IF              !  if synch failed

    END IF                  !  if file is volatile

    IF ( CDF .EQ. BINFIL3 ) THEN

        IF ( DSCBIN3( FID ) .EQ. 0 ) THEN
            MESG = 'Error reading header for BINFIL3 file '// FNAME
            CALL M3WARN( 'DESC3', 0, 0, MESG )
            EFLAG = .TRUE.
        END IF

        GO TO  99

    END IF          !  if "native-binary file; else if "buffered" file


    !!.......   If not [P]netcdf file:  already processed, so return

    IF ( CDF .LT. 0 ) THEN
        EFLAG = .FALSE.
        GO TO  99
    END IF          !  if "native-binary file; else if "buffered" file


    !!.......   Else read characteristics from the file header:

!$OMP CRITICAL( S_NC )

    IF ( FTYPE3( FID ) .EQ. MPIGRD3 ) THEN      !!  MPI/PnetCDF file

        FTYPE3D = GRDDED3

#ifndef IOAPI_PNCF
        CALL M3MESG( 'PnetCDF/MPI not supported in this build.' )
        EFLAG = .TRUE.
#endif

#ifdef IOAPI_PNCF

        IERR = NFMPI_GET_ATT_TEXT( CDF, NF_GLOBAL, 'EXEC_ID', EXECN3D )
        IF ( IERR .NE. 0 ) THEN
            EXECN3D = CMISS3
            WRITE( MESG, '(A,I9)' ) 'PnetCDF error number', IERR
            CALL M3MSG2( MESG )
            WRITE( MESG, '( 3A )' ) 'File header attribute EXEC_ID not available for file "', FNAME, '"'
            CALL M3MSG2( MESG )
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_GET_ATT_INT( CDF, NF_GLOBAL, 'CDATE', CDATE3D )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '(A,I9)' ) 'PnetCDF error number', IERR
            CALL M3MSG2( MESG )
            WRITE( MESG, '( 3A )' ) 'File header attribute CDATE not available for file "', FNAME, '"'
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_GET_ATT_INT( CDF, NF_GLOBAL, 'CTIME', CTIME3D )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '(A,I9)' ) 'PnetCDF error number', IERR
            CALL M3MSG2( MESG )
            WRITE( MESG, '( 3A )' ) 'File header attribute CTIME not available for file "', FNAME, '"'
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_GET_ATT_INT( CDF, NF_GLOBAL, 'WDATE', WDATE3D )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '(A,I9)' ) 'PnetCDF error number', IERR
            CALL M3MSG2( MESG )
            WRITE( MESG, '( 3A )' ) 'File header attribute WDATE not available for file "', FNAME, '"'
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_GET_ATT_INT( CDF, NF_GLOBAL, 'WTIME', WTIME3D )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '(A,I9)' ) 'PnetCDF error number', IERR
            CALL M3MSG2( MESG )
            WRITE( MESG, '( 3A )' ) 'File header attribute WTIME not available for file "', FNAME, '"'
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_GET_ATT_TEXT( CDF, NF_GLOBAL, 'UPNAM', UPNAM3D )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '(A,I9)' ) 'PnetCDF error number', IERR
            CALL M3MSG2( MESG )
            WRITE( MESG, '( 3A )' ) 'File header attribute UPNAM not available for file "', FNAME, '"'
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_GET_ATT_TEXT( CDF, NF_GLOBAL, 'FILEDESC', FDESC3D )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '(A,I9)' ) 'PnetCDF error number', IERR
            CALL M3MSG2( MESG )
            WRITE( MESG, '( 3A )' ) 'File header attribute FDESC not available for file "', FNAME, '"'
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_GET_ATT_TEXT( CDF, NF_GLOBAL, 'HISTORY', UPDSC3D )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '(A,I9)' ) 'PnetCDF error number', IERR
            CALL M3MSG2( MESG )
            WRITE( MESG, '( 3A )' ) 'File header attribute UPDSC not available for file "', FNAME, '"'
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        DO   VAR = 1, NVARS3D

            IERR = NFMPI_GET_ATT_TEXT( CDF, VINDX3( VAR,FID ), 'var_desc', VDESC3D( VAR ) )
            IF ( IERR .NE. 0 ) THEN
                WRITE( MESG, '(A,I9)' )'PnetCDF error number', IERR
                CALL M3MSG2( MESG )
                WRITE( MESG, '( 5A )' ) 'Error reading VDESC for file "', FNAME, '", vble "', VLIST3(VAR,FID), '"'
                CALL M3MSG2( MESG )
                EFLAG = .TRUE.
            END IF          !  ierr nonzero:  operation failed

        END DO      !! end loop getting variable-descriptions

#endif

    ELSE            !!  netCDF file:

        IERR = NF_GET_ATT_TEXT( CDF, NF_GLOBAL, 'EXEC_ID', EXECN3D )
        IF ( IERR .NE. 0 ) THEN
            EXECN3D = CMISS3
            WRITE( MESG, '(A,I9)' ) 'netCDF error number', IERR
            CALL M3MSG2( MESG )
            WRITE( MESG, '( 3A )' ) 'File header attribute EXEC_ID not available for file "', FNAME, '"'
            CALL M3MSG2( MESG )
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_GET_ATT_INT( CDF, NF_GLOBAL, 'CDATE', CDATE3D )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '(A,I9)' ) 'netCDF error number', IERR
            CALL M3MSG2( MESG )
            WRITE( MESG, '( 3A )' ) 'File header attribute CDATE not available for file "', FNAME, '"'
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_GET_ATT_INT( CDF, NF_GLOBAL, 'CTIME', CTIME3D )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '(A,I9)' ) 'netCDF error number', IERR
            CALL M3MSG2( MESG )
            WRITE( MESG, '( 3A )' ) 'File header attribute CTIME not available for file "', FNAME, '"'
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_GET_ATT_INT( CDF, NF_GLOBAL, 'WDATE', WDATE3D )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '(A,I9)' ) 'netCDF error number', IERR
            CALL M3MSG2( MESG )
            WRITE( MESG, '( 3A )' ) 'File header attribute WDATE not available for file "', FNAME, '"'
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_GET_ATT_INT( CDF, NF_GLOBAL, 'WTIME', WTIME3D )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '(A,I9)' ) 'netCDF error number', IERR
            CALL M3MSG2( MESG )
            WRITE( MESG, '( 3A )' ) 'File header attribute WTIME not available for file "', FNAME, '"'
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_GET_ATT_TEXT( CDF, NF_GLOBAL, 'UPNAM', UPNAM3D )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '(A,I9)' ) 'netCDF error number', IERR
            CALL M3MSG2( MESG )
            WRITE( MESG, '( 3A )' ) 'File header attribute UPNAM not available for file "', FNAME, '"'
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_GET_ATT_TEXT( CDF, NF_GLOBAL, 'FILEDESC', FDESC3D )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '(A,I9)' ) 'netCDF error number', IERR
            CALL M3MSG2( MESG )
            WRITE( MESG, '( 3A )' ) 'File header attribute FDESC not available for file "', FNAME, '"'
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_GET_ATT_TEXT( CDF, NF_GLOBAL, 'HISTORY', UPDSC3D )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '(A,I9)' ) 'netCDF error number', IERR
            CALL M3MSG2( MESG )
            WRITE( MESG, '( 3A )' ) 'File header attribute UPDSC not available for file "', FNAME, '"'
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed


        IF ( FTYPE3D .NE. DCTNRY3 ) THEN

            DO   VAR = 1, NVARS3D

                VNAME = VLIST3(VAR,FID)
                VID   = VINDX3( VAR,FID )
                IERR  = NF_GET_ATT_TEXT( CDF, VID, 'var_desc', VDESC3D( VAR ) )
                IF ( IERR .NE. 0 ) THEN
                    WRITE( MESG, '(A,I9)' )'netCDF error number', IERR
                    CALL M3MSG2( MESG )
                    WRITE( MESG, '( 5A )' ) 'Error reading VDESC for file "', FNAME, '", vble "', VNAME, '"'
                    CALL M3MSG2( MESG )
                    EFLAG = .TRUE.
                END IF          !  ierr nonzero:  operation failed

                !!...............   TSERIES3 files:  get numbers of extra per-variable
                !!...............   attributes, lists of names and values for them:

                IF ( FTYPE3D .EQ. TSRIES3 ) THEN

                    IERR = NF_INQ_VARNATTS( CDF, VID, 'natts', NATTS3D( VAR ) )
                    IF ( IERR .NE. 0 ) THEN
                        WRITE( MESG, '(A,I9)' ) 'netCDF error number', IERR
                        CALL M3MSG2( MESG )
                        WRITE( MESG, '( 3A )' ) 'Error reading dimension "natts" for variable "',   &
                                VNAME, '" in file "', FNAME, '"'
                        EFLAG = .TRUE.
                    END IF

                    DO  ATT = 1, NATTS3D( VAR )   ! loop on each addt'l att

                        IERR = NF_INQ_ATTNAME( CDF, VID, ATT+4, ATNAM3D( ATT,VAR ) )

                        IF ( IERR .NE. 0 ) THEN
                            WRITE( MESG, '(A,I9)' ) 'netCDF error number', IERR
                            CALL M3MSG2( MESG )
                            WRITE( MESG, '( A, I9, 1X, 4A )' ) 'Error reading name for att ', ATT,     &
                                ' for variable "',  VNAME, '" in file "', FNAME, '"'
                            CALL M3MSG2( MESG )
                            EFLAG = .TRUE.
                        END IF

                        IERR = NF_GET_ATT_REAL( CDF, VID, ATNAM3D( ATT,VAR ), FATTS3D( ATT,VAR ) )
                        IF ( IERR .NE. 0 ) THEN
                            WRITE( MESG, '(A,I9)' ) 'netCDF error number', IERR
                            CALL M3MSG2( MESG )
                            WRITE( MESG, '( 7A )' ) 'Error reading attribute "', ATNAM3D( ATT,VAR ),    &
                                '" for variable "',  VNAME, '" in file "', FNAME, '"'
                            CALL M3MSG2( MESG )
                            EFLAG = .TRUE.
                        END IF

                    END DO      !  end loop on ATTs for this VAR

                END IF          !  if ftype tseries3

            END DO      !! end loop getting variable-descriptions

        END IF          !  if not a dictionary file

    END IF              !!  if MPI/PnetCDF file; else netCDF file

!$OMP END CRITICAL( S_NC )

    IF ( EFLAG ) THEN
        WRITE( MESG, '( 3A )' ) 'Error(s) in DESC3(', FNAME, ')'
        CALL M3WARN( 'DESC3', 0, 0, MESG )
    END IF

99  CONTINUE

!$OMP END SINGLE

    DESC3 = ( .NOT. EFLAG )

    RETURN

END FUNCTION DESC3

