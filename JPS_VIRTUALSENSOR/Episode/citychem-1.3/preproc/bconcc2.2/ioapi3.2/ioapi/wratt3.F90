
LOGICAL FUNCTION  WRATT3( FNAME, VNAME, ANAME, ATYPE, AMAX, AVAL )

    !!***********************************************************************
    !! Version "$Id: wratt3.F90 96 2018-04-04 21:17:59Z coats $"
    !! EDSS/Models-3 I/O API.
    !! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    !! (C) 2003-2010 by Baron Advanced Meteorological Systems and
    !! (C) 2014-2015 UNC Institute for the Environment
    !! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    !! See file "LGPL.txt" for conditions of use.
    !!.........................................................................
    !!  subroutine body starts at line  108
    !!   Entry  WRATTC  starts at line  129
    !!
    !!  FUNCTION:
    !!       Puts the attribute named ANAME with value AVAL( AMAX ) to the
    !!       variable VNAME in file FNAME (or to the global file attributes
    !!       if VNAME == ALLVAR3).  ATYPE should be one of M3REAL, M3INT, or
    !!       M3DBLE.
    !!       CHARACTER-string attributes use
    !!               ENTRY   WRATTC( FNAME, VNAME, ANAME, CVAL )
    !!
    !!  PRECONDITIONS REQUIRED:
    !!       File must have been previously opened for output.
    !!       There are serious performance implications if the file has had
    !!       any output written to it (netCDF will do a behind-the-scenes
    !!       file-copy of the entire current contents.)
    !!
    !!  SUBROUTINES AND FUNCTIONS CALLED:
    !!       netCDF
    !!
    !!  REVISION  HISTORY:
    !!      prototype 1/2002 by Carlie J. Coats, Jr., MCNC-EMC for I/O API v2.2
    !!
    !!      Modified 7/2003 by CJC:  bugfix -- clean up critical sections
    !!      associated with INIT3()
    !!
    !!      Modified 12/2004 by CJC:  bugfix for character attribute length;
    !!      improved error messages; restructure NF_ENDDEF call.
    !!
    !!      Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !!
    !!      Modified 12/2014 by CJC: logic cleanup; use NAME2FID()
    !!
    !!      Modified 02/2015 by CJC for I/O API 3.2:  support for M3INT8 attributes
    !!
    !!      Modified 08/2015 by CJC for I/O API 3.2:  support for MPI/PnetCDF;
    !!      USE MODNCFIO, MODPDATA.  Use "NF_" routines. F90 "free" source format.
    !!
    !!      Version  11/2015 by CJC: replace MPI_OFFSET_KIND by hard-coded INTEGER(8)
    !!      because OpenMPI-1.4.x does not follow the MPOI "standard" competently.
    !!
    !!      Version  04/2018 by CJC: FTYPE3(FID) ~~> FTYPE3(F)
    !!***********************************************************************

    USE MODNCFIO
    USE MODPDATA

    IMPLICIT NONE
    LOGICAL WRATTC

    !!...........   INCLUDES:

    INCLUDE 'PARMS3.EXT'      ! I/O API constants
    INCLUDE 'STATE3.EXT'      ! I/O API internal state


    !!...........   ARGUMENTS and their descriptions:

    CHARACTER*(*), INTENT(IN   ) :: FNAME         !  logical file name
    CHARACTER*(*), INTENT(IN   ) :: VNAME         !  variable name, or ALLVARS3
    CHARACTER*(*), INTENT(IN   ) :: ANAME         !  attribute name
    INTEGER      , INTENT(IN   ) :: ATYPE         !  attribute type (M3REAL, M3INT, M3INT8, M3DBLE)
    INTEGER      , INTENT(IN   ) :: AMAX          !  attribute dimensionality/size
    REAL         , INTENT(IN   ) :: AVAL( AMAX )  !  attribute value (numeric)
    CHARACTER*(*), INTENT(IN   ) :: CVAL          !  attribute value (character-string)


    !!...........   EXTERNAL FUNCTIONS and their descriptions:

    INTEGER, EXTERNAL :: INDEX1     !  look up names in name tables
    INTEGER, EXTERNAL :: NAME2FID   !  fname~~> fid lookup


    !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         F, V            !  subscripts for STATE3 arrays
    INTEGER         FID, VID        !  netCDF ID's
    INTEGER         FLEN, VLEN      !  name lengths for file, vble
    INTEGER         IERR            !  netCDF error status return
    INTEGER         ITYPE, ILEN
    LOGICAL         EFLAG, SFLAG
    CHARACTER*16    FIL16           !  scratch file-name buffer
    CHARACTER*16    VAR16           !  scratch vble-name buffer
    CHARACTER*256   MESG            !  message-buffer

#ifdef IOAPI_PNCF
    INCLUDE  'mpif.h'
    INTEGER( 8 ) :: SIZE
!!  INTEGER( MPI_OFFSET_KIND ) :: SIZE
#endif


    !!***********************************************************************
    !!   begin body of subroutine  WRATT3

    !!...........   Check attribute type

    IF ( ( ATYPE .NE. NF_CHAR   ) .AND.         &
         ( ATYPE .NE. NF_INT    ) .AND.         &
         ( ATYPE .NE. NF_FLOAT  ) .AND.         &
         ( ATYPE .NE. NF_INT64  ) .AND.         &
         ( ATYPE .NE. NF_DOUBLE ) ) THEN

        WRITE( MESG , '( 3 A, I10 )' ) 'WRATT3:  Attribute "'  , ANAME, '" has unsupported type', ATYPE
        CALL M3WARN( 'WRATT3', 0, 0, MESG )
        WRATT3 = .FALSE.
        RETURN

    END IF

    ITYPE = ATYPE
    GO TO 111

    !!..................................................................


    ENTRY WRATTC( FNAME, VNAME, ANAME, CVAL )
    ITYPE = NF_CHAR

    !!........   fall through to  111  .................................

111 CONTINUE

    !!.......   Preliminary checks:

    EFLAG = .FALSE.
    F     = NAME2FID( FNAME )
    VLEN  = LEN_TRIM( VNAME )

    IF ( F .LE. 0 ) THEN

        EFLAG = .TRUE.

    ELSE IF ( RONLY3( F ) ) THEN

        EFLAG = .TRUE.
        MESG = 'File:  "' // FIL16 // '" is READ-ONLY.'
        CALL M3MSG2( MESG )

    ELSE IF ( CDFID3( F ) .LT. 0 ) THEN

        EFLAG = .TRUE.
        MESG = 'File:  "' // FIL16 // '" is NOT A NetCDF file.'
        CALL M3MSG2( MESG )

    ELSE IF ( VLEN .GT. NAMLEN3 ) THEN

        EFLAG = .TRUE.
        MESG  = 'File "'// FNAME// '" Variable "'// VNAME//'"'
        CALL M3MSG2( MESG )
        WRITE( MESG, '( A, I10 )'  ) 'Max vble name length 16; actual:', VLEN
        CALL M3MSG2( MESG )

    ELSE

        FID = CDFID3( F )

    END IF          !  if len( vname ) > 16

    !!...........   Get ID for variable(s) to be written.

    VAR16 = VNAME   !  fixed-length-16 scratch copy of name

    IF ( VAR16 .EQ. ALLVAR3 ) THEN

        VID = NF_GLOBAL

    ELSE

        V = INDEX1( VAR16, NVARS3( F ) , VLIST3( 1,F ) )
        IF ( V .EQ. 0 ) THEN
            EFLAG = .TRUE.
            MESG = 'Variable "'      // TRIM(VNAME) // '" not in file "' // TRIM(FNAME) // '"'
            CALL M3MSG2( MESG )
        ELSE
            VID = VINDX3( V, F )
        END IF

    END IF          !  if VAR16 is 'ALL', or not.

    IF ( EFLAG ) THEN
        MESG = 'Invalid variable or file name arguments'
        CALL M3MESG( MESG )
        GO TO 999
    END IF


    !!...........   Put file into define mode; write the attribute; and
    !!...........   restore the file to data mode:
    !!...........   Somewhat tortured logic-structure due to the fact that
    !!...........   one can't execute a RETURN within a critical section.

!$OMP   CRITICAL( S_NC )

    IF ( FTYPE3( F ) .EQ. MPIGRD3 ) THEN  !!  pnetcdf operations:

#ifdef IOAPI_PNCF

        IF ( PN_IO_PE ) THEN

            IERR = NFMPI_REDEF( FID )

            IF ( IERR .NE. NF_NOERR ) THEN

                WRITE( MESG, '( A, I10, 2X, 3 A )' ) 'Error', IERR, 'putting PnetCDF file "', TRIM(FNAME), '" into define mode'
                CALL M3WARN( 'WRATT3', 0, 0, MESG )
                EFLAG = .TRUE.

            ELSE

                IF ( ITYPE .EQ. NF_CHAR ) THEN
                    SIZE = LEN( CVAL )
                    IERR = NFMPI_PUT_ATT_TEXT(  FID, VID, ANAME, NF_CHAR,   SIZE, CVAL )
                ELSE IF ( ITYPE .EQ. M3REAL ) THEN
                    SIZE = AMAX
                    IERR = NFMPI_PUT_ATT_REAL(  FID, VID, ANAME, NF_FLOAT,  SIZE, AVAL )
                ELSE IF ( ITYPE .EQ. M3INT ) THEN
                    SIZE = AMAX
                    IERR = NFMPI_PUT_ATT_INT(   FID, VID, ANAME, NF_INT,    SIZE, AVAL )
                ELSE IF ( ITYPE .EQ. M3DBLE ) THEN
                    SIZE = AMAX
                    IERR = NFMPI_PUT_ATT_DOUBLE(FID, VID, ANAME, NF_DOUBLE, SIZE, AVAL )
                ELSE IF ( ITYPE .EQ. M3INT8 ) THEN
                    SIZE = AMAX
                    IERR = NFMPI_PUT_ATT_INT8(  FID, VID, ANAME, NF_INT64,  SIZE, AVAL )
                END IF

                IF ( IERR .NE. NF_NOERR ) THEN
                    IF ( IERR .NE. NF_NOERR ) THEN
                        MESG = 'Error creating attribute "'// TRIM( ANAME ) //      &
                               '" for PnetCDF file "'      // TRIM( FNAME ) //      &
                               '" and vble "'              // TRIM( VNAME ) // '"'
                        CALL M3WARN( 'WRATT3', 0, 0, MESG )
                        EFLAG = .TRUE.
                    END IF
                END IF

                IERR = NF_ENDDEF( FID )
                IF ( IERR .NE. NF_NOERR ) THEN
                    WRITE( MESG, '( A, I10, 2X, 3 A )' )                &
                        'Error', IERR, 'putting PnetCDF file "', TRIM(FNAME), '" back into data mode'
                    CALL M3WARN( 'WRATT3', 0, 0, MESG )
                    EFLAG = .TRUE.
                END IF

            END IF

        END IF      !!  if pn_io_pe

#endif
#ifndef IOAPI_PNCF
        CALL M3MESG( 'WRATT3 error:  PnetCDF not active.' )
#endif

    ELSE        !!  netCDF file operations:

        IERR = NF_REDEF( FID )

        IF ( IERR .NE. NF_NOERR ) THEN

            WRITE( MESG, '( A, I10, 2X, 3 A )' ) 'Error', IERR, 'putting file "', TRIM(FNAME), '" into define mode'
            CALL M3WARN( 'WRATT3', 0, 0, MESG )
            EFLAG = .TRUE.

        ELSE

            IF ( ITYPE .EQ. NF_CHAR ) THEN
                ILEN = LEN( CVAL )
                IERR = NF_PUT_ATT_TEXT(   FID, VID, ANAME,            ILEN, CVAL )
            ELSE IF ( ITYPE .EQ. M3REAL ) THEN
                IERR = NF_PUT_ATT_REAL(   FID, VID, ANAME, NF_FLOAT,  AMAX, AVAL )
            ELSE IF ( ITYPE .EQ. M3INT ) THEN
                IERR = NF_PUT_ATT_INT(    FID, VID, ANAME, NF_INT,    AMAX, AVAL )
            ELSE IF ( ITYPE .EQ. M3DBLE ) THEN
                IERR = NF_PUT_ATT_DOUBLE( FID, VID, ANAME, NF_DOUBLE, AMAX, AVAL )
            ELSE IF ( ITYPE .EQ. M3INT8 ) THEN
                IERR = IMISS3
                CALL M3MESG( 'WRATT3: netCDF does not support INTEGER*8 attributes' )
            END IF

            IF ( IERR .NE. NF_NOERR ) THEN
                IF ( IERR .NE. NF_NOERR ) THEN
                        MESG = 'Error creating attribute "'// TRIM( ANAME ) //      &
                               '" for PnetCDF file "'      // TRIM( FNAME ) //      &
                               '" and vble "'              // TRIM( VNAME ) // '"'
                    CALL M3WARN( 'WRATT3', 0, 0, MESG )
                    EFLAG = .TRUE.
                END IF
            END IF

            IERR = NF_ENDDEF( FID )
            IF ( IERR .NE. NF_NOERR ) THEN
                WRITE( MESG, '( A, I10, 2X, 3 A )' )                &
                    'Error', IERR, 'putting file "', TRIM(FNAME), '" back into data mode'
                CALL M3WARN( 'WRATT3', 0, 0, MESG )
                EFLAG = .TRUE.
            END IF

        END IF

    END IF      !!  if pnetCDF distributed, or not

!$OMP   END CRITICAL( S_NC )

    IF ( IERR .NE. NF_NOERR ) THEN
        EFLAG = .TRUE.
    END IF

999 CONTINUE

    SFLAG = ( .NOT. EFLAG )

#ifdef IOAPI_PNCF
    IF ( FTYPE3( F ) .EQ. MPIGRD3 ) THEN
        IF ( .NOT.PN_FLAG( SFLAG ) ) THEN
            CALL M3MSG2( 'WRATT3:  MPI_SEND(EFLAG) error' )
            SFLAG = .FALSE.
        END IF
    END IF
#endif

    IF ( EFLAG ) THEN
        MESG = 'Error writing attribute "' // TRIM(ANAME) //      &
               '" for file "' // TRIM(FNAME) //                   &
               '" and vble "' // TRIM(VNAME) // '"'
        CALL M3WARN( 'WRATT3', 0, 0, MESG )
    END IF          !  ierr nonzero:  operation failed
    IF ( ITYPE .EQ. NF_CHAR ) THEN
        WRATTC = SFLAG
    ELSE
        WRATT3 = SFLAG
    END IF

    RETURN

END FUNCTION  WRATT3

