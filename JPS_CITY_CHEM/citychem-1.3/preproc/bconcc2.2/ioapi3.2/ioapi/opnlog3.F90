
RECURSIVE SUBROUTINE OPNLOG3( FID , EQNAME, STATUS )

    !!***********************************************************************
    !! Version "$Id: opnlog3.F90 119 2019-06-20 13:37:39Z coats $"
    !! EDSS/Models-3 I/O API.
    !! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    !! (C) 2003-2013 Baron Advanced Meteorological Systems,
    !! (C) 2007-2013 Carlie J. Coats, Jr., and
    !! (C) 2014-2015 UNC Institute for the Environment.
    !! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    !! See file "LGPL.txt" for conditions of use.
    !!.........................................................................
    !!  function body starts at line  105
    !!
    !!  FUNCTION:  puts description of file at FID into the program log.
    !!
    !!  PRECONDITIONS REQUIRED:
    !!       Models-3 file with index FID must have already opened by OPEN3().
    !!       To be called by OPEN3().
    !!
    !!  SUBROUTINES AND FUNCTIONS CALLED:
    !!
    !!  REVISION  HISTORY:
    !!       Prototype 3/1992 by CJC
    !!       Modified  7/1994 by CJC to return new coordinate-system and
    !!                             grid-description parameters
    !!       Modified  5/1998 by CJC:  for OpenMP thread-safety
    !!       Modified  5/2003 by CJC:  Log volatile, read/write status
    !!       Modified  1/2004 by CJC:  use "m3flush()" from "m3msg2.F"
    !!       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !!       Modified 02/2015 by CJC for I/O API 3.2: USE M3UTILIO.
    !!
    !!      Modified 08/2015 by CJC for I/O API 3.2:  F90 free format;
    !!      support for MPI/PnetCDF; USE MODNCFIO, MODPDATA
    !!
    !!      Modified 10/2015 by CJC for I/O API 3.2: use NF_*() instead of NC*()
    !!      for netCDF-Fortran 4.x compatibility
    !!
    !!      Modified 7/2016 by CJC:  bug-fix from Edward Anderson
    !!      (Lockheed Martin, supporting the U.S. EPA:: NF_GET_ATT_TEXT(()
    !!      argument-list
    !!
    !!      Modified 6/2019 by CJC:  bug-fix for LIST-files
    !!***********************************************************************

    USE M3UTILIO
    USE MODNCFIO
    USE MODPDATA

    IMPLICIT NONE

    !!...........   INCLUDES:

    INCLUDE 'STATE3.EXT'


    !!...........   ARGUMENTS and their descriptions:

    INTEGER      , INTENT(IN   ) :: FID             !  subscript for STATE3 arrays
    CHARACTER*(*), INTENT(IN   ) :: EQNAME          !  physical file name
    INTEGER      , INTENT(IN   ) :: STATUS          !  file-opening status


    !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER        FNUM       ! netCDF file ID from NF_CREATE()
    INTEGER        IERR       ! netCDF error-result argument
    INTEGER        IS, IT
    CHARACTER*24   TVALUE     ! for DT2STR()  result
    CHARACTER*10   SVALUE     ! for HHMMSS()  result
    CHARACTER*80   EXECID     ! execution-id
    CHARACTER*20   RBUF       ! for NROWS3D
    CHARACTER*20   CBUF       ! for NROWS3D
    CHARACTER*20   LBUF       ! for NCOLS3D
    CHARACTER*20   VBUF       ! for NVARS3D
    CHARACTER*20   TBUF       ! for NTHIK3D
    CHARACTER*20   KBUF       ! for KF records
    CHARACTER*20   SBUF       ! for file status
    CHARACTER*256  MESG

    CHARACTER*16, PARAMETER :: SNAME( 0:6 ) =   &
       (/ CMISS3,                   &       !!  0:  bad status
          'OLD:READ-ONLY   ',       &       !!  1
          'OLD:READ-WRITE  ',       &       !!  2
          'NEW(READ-WRITE )',       &       !!  3
          'UNKNOWN(R-W)    ',       &       !!  4
          'NEW(CREAT/TRUNC)',       &       !!  5
          CMISS3 /)                         !!  6:  bad status

    CHARACTER*8, PARAMETER :: TYPNAME( -4 : 7 ) =   &
       (/ 'UNKNOWN',        &       !!  -4:  file type error
          'KFEVNT3',        &       !!  -3:  known file types
          'DGRAPH3',        &       !!  -2:    "
          'CUSTOM3',        &       !!  -1:    "
          'DCTNRY3',        &       !!   0:    "
          'GRDDED3',        &       !!   1:    "
          'BNDARY3',        &       !!   2:    "
          'IDDATA3',        &       !!   3:    "
          'PROFIL3',        &       !!   4:    "
          'GRNEST3',        &       !!   5:    "
          'SMATRX3',        &       !!   6:    "
          'UNKNOWN'  /)             !!   7:    "


    !!.............................................................................
    !!   begin body of subroutine  OPEN3

    !!.......   Log the file description:

    FNUM  = CDFID3( FID )

!$OMP   CRITICAL( S_LOGOUT )

    !!.......   Get execution-ID for the program which produced this file:

    IF( FNUM .EQ. BUFFIL3 ) THEN

        IERR   = 0
        EXECID = EXECN3

    ELSE IF( FNUM .EQ. LSTFIL3 ) THEN

        WRITE( LOGDEV, '( /5X, A )' ) 'Opening LIST-FILE SEQUENCE'
        DO IT = IFRST3(FID), IFRST3(FID) + NLIST3(FID) - 1
            CALL OPNLOG3( ILIST3(IT) , EQNAME, STATUS )
        END DO
        WRITE( LOGDEV, '( /5X, A, I5, / )' ) 'End of LIST-FILE SEQUENCE.  NLIST=', NLIST3( FID )
        GO TO  99

    ELSE IF ( FTYPE3( FID ) .EQ. MPIGRD3 ) THEN

        IF ( .NOT. PN_IO_PE )  GO TO  99
#ifdef  IOAPI_PNCF
!$OMP CRITICAL( S_NC )
        IERR = NFMPI_GET_ATT_TEXT( FNUM, NF_GLOBAL, 'EXEC_ID', EXECID )
!$OMP END CRITICAL( S_NC )
#endif
    ELSE IF ( FNUM .GT. 0 ) THEN
!$OMP CRITICAL( S_NC )
        IERR = NF_GET_ATT_TEXT( FNUM, NF_GLOBAL, 'EXEC_ID', EXECID )
!$OMP END CRITICAL( S_NC )

    ELSE

        CALL M3MESG( 'OPNLOG3:  unrecognized file' )
        GO TO  99

    END IF

    IS = MAX( MIN( STATUS, 6 ), 0  )
    IT = MIN( MAX( FTYPE3( FID ), -4 ), 7 )

    IF ( IERR .NE. 0 ) THEN
        EXECID = CMISS3
        WRITE( LOGDEV,91010 )                                   &
           'Warning netCDF file header attribute EXEC_ID.',     &
           'Not available for file:  ' // FLIST3( FID ) ,       &
           'netCDF error number', IERR
        WRITE( LOGDEV,93000 )  ' ',                                             &
           '"' // TRIM( FLIST3( FID ) ) // '" opened as ' // SNAME( IS ),       &
           'File name "'  // TRIM( EQNAME ) // '"',                             &
           'File type '   // TYPNAME( IT ),                                     &
           'Grid name "'  // TRIM( GDNAM3( FID ) ) // '"'
    ELSE
        IF ( EXECID( 1:1 ) .EQ. CHAR( 0 ) ) EXECID = CMISS3
        WRITE( LOGDEV,93000 ) ' ',                                              &
            '"' // TRIM( FLIST3( FID ) ) // '" opened as ' // SNAME( IS ),     &
            'File name "'  // TRIM( EQNAME ) // '"',                            &
            'File type '   // TYPNAME( IT ),                                    &
            'Execution ID "' // TRIM( EXECID ) // '"',                          &
            'Grid name "'  // TRIM( GDNAM3( FID ) ) // '"'
    END IF          !  ierr nonzero:  operation failed


    WRITE( RBUF,'(I20)' ) NROWS3( FID )
    WRITE( CBUF,'(I20)' ) NCOLS3( FID )
    WRITE( LBUF,'(I20)' ) NLAYS3( FID )
    WRITE( VBUF,'(I20)' ) NVARS3( FID )
    IF ( FTYPE3( FID ) .EQ. BNDARY3 ) THEN
        WRITE( TBUF,'(I20)' ) NTHIK3( FID )
        WRITE( LOGDEV,93000 )  'Dimensions: '                       &
                // RBUF( LBLANK( RBUF )+1 : 20 ) // ' rows, '       &
                // CBUF( LBLANK( CBUF )+1 : 20 ) // ' cols, '       &
                // LBUF( LBLANK( LBUF )+1 : 20 ) // ' lays, '       &
                // VBUF( LBLANK( VBUF )+1 : 20 ) // ' vbles, '      &
                // TBUF( LBLANK( TBUF )+1 : 20 ) // ' cells thick'
    ELSE IF ( FTYPE3( FID ) .EQ. KFEVNT3 ) THEN
        WRITE( TBUF,'(I20)' ) NTHIK3( FID )
        WRITE( KBUF,'(I20)' ) MXREC3( FID )
        WRITE( LOGDEV,93000 )  'Dimensions: '                           &
                // RBUF( LBLANK( RBUF )+1 : 20 ) // ' rows, '           &
                // CBUF( LBLANK( CBUF )+1 : 20 ) // ' cols, '           &
                // LBUF( LBLANK( LBUF )+1 : 20 ) // ' lays, '           &
                // VBUF( LBLANK( VBUF )+1 : 20 ) // ' vbles, '          &
                // TBUF( LBLANK( TBUF )+1 : 20 ) // ' max recs/cell, '  &
                // KBUF( LBLANK( KBUF )+1 : 20 ) // ' total recs'
    ELSE
        WRITE( LOGDEV,93000 )  'Dimensions: '                           &
                // RBUF( LBLANK( RBUF )+1 : 20 ) // ' rows, '           &
                // CBUF( LBLANK( CBUF )+1 : 20 ) // ' cols, '           &
                // LBUF( LBLANK( LBUF )+1 : 20 ) // ' lays, '           &
                // VBUF( LBLANK( VBUF )+1 : 20 ) // ' vbles'
    END IF

    IF ( VOLAT3( FID ) ) THEN
        IF( RONLY3( FID ) ) THEN
            SBUF = 'VOLATILE READONLY'
        ELSE
            SBUF = 'VOLATILE READWRITE'
        END IF
    ELSE
        IF( RONLY3( FID ) ) THEN
            SBUF = 'READONLY'
        ELSE
            SBUF = 'READWRITE'
        END IF
    END IF

    IF ( FNUM .GE. 0 ) THEN
        WRITE( LOGDEV,93030 )  'NetCDF ID: ', CDFID3( FID ), 'opened as ', SBUF
    ELSE IF ( FNUM .EQ. BUFFIL3 ) THEN
        WRITE( LOGDEV, '( 5X, 2A )' )  'BUFFERED "file" opened as ', SBUF
    ELSE IF ( FNUM .EQ. VIRFIL3 ) THEN
        WRITE( LOGDEV, '( 5X, 2A )' )  'VIRTUAL "file" opened as ',  SBUF
    ELSE IF ( FNUM .EQ. BINFIL3 ) THEN
        WRITE( LOGDEV, '( 5X, 2A )' )  'Native-binary file opened as ', SBUF
    END IF

    IF( FTYPE3( FID ) .EQ. KFEVNT3 ) THEN

        TVALUE = DT2STR( SDATE3( FID ), STIME3( FID ) )
        WRITE( LOGDEV,93020 )                                           &
            'Starting date and time', SDATE3( FID ),  STIME3( FID ),    &
            ' (' // TRIM( TVALUE ) //  ')',                             &
            'Maximum current record number', MXREC3( FID )

    ELSE IF( TSTEP3( FID ) .EQ. 0 ) THEN

        WRITE( LOGDEV,93000 ) 'Time-independent data.'

    ELSE

        TVALUE = DT2STR( SDATE3( FID ), STIME3( FID ) )
        SVALUE = HHMMSS( ABS( TSTEP3( FID ) ) )
        WRITE( LOGDEV,93020 )                                           &
            'Starting date and time', SDATE3( FID ),  STIME3( FID ),    &
            ' (' // TRIM( TVALUE ) //  ')',                             &
            'Timestep                     ', TSTEP3( FID ),             &
            ' (' // TRIM( SVALUE ) //  ' hh:mm:ss)',                    &
            'Maximum current record number', MXREC3( FID )

    END IF          !  if kf-file, or time step zero, or not

    CALL M3FLUSH( LOGDEV )

99  CONTINUE

!$OMP   END CRITICAL( S_LOGOUT )


    RETURN

    !!******************  FORMAT  STATEMENTS   ******************************

    !!...........   Error and warning message formats..... 91xxx

91010   FORMAT ( //5X , '>>> WARNING in subroutine OPNLOG3 <<<',    &
              3 ( /5X , A , : ) , I5, // )


    !!...........   Informational log message formats..... 93xxx

93000   FORMAT ( 5X, A )

93020   FORMAT ( 5X, A, I9, ':', I6.6, A,   &
                /5X, A, I11.6, A,           &
                /5X, A, I10 )

93030   FORMAT ( 5X, A, :, I9, 2X, A, A )


END  SUBROUTINE OPNLOG3

