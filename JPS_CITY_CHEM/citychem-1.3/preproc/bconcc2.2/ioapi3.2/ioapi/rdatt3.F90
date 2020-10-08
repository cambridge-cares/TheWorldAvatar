
LOGICAL FUNCTION   RDATT3( FNAME, VNAME, ANAME, ATYPE, AMAX, ASIZE, AVAL )

    !!***********************************************************************
    !! Version "$Id: rdatt3.F90 96 2018-04-04 21:17:59Z coats $"
    !! BAMS/MCNC/EDSS/Models-3 I/O API.
    !! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    !! (C) 2003-2011 Baron Advanced Meteorological Systems, and
    !! (C) 2014 UNC Institute for the Environment
    !! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    !! See file "LGPL.txt" for conditions of use.
    !!.........................................................................
    !!  subroutine body starts at line  101
    !!   Entry  RDATTC  starts at line  119
    !!
    !!  FUNCTION:
    !!       Reads the attribute named ANAME for the variable VNAME in the
    !!       file FNAME into AVAL( AMAX ).  If VNAME == ALLVAR3, reads
    !!       global attribute ANAME.
    !!       AVAL must have type ATYPE, which should be one of M3REAL, M3INT,
    !!       M3DBLE, or M3INT8.
    !!
    !!       CHARACTER-string attributes use
    !!               ENTRY   RDATTC( FNAME, VNAME, ANAME, CVAL )
    !!
    !!  PRECONDITIONS REQUIRED:
    !!       File must have been previously opened.
    !!
    !!  SUBROUTINES AND FUNCTIONS CALLED:
    !!       netCDF
    !!
    !!  REVISION  HISTORY:
    !!      prototype 1/2002 by Carlie J. Coats, Jr., MCNC-EMC
    !!
    !!      Modified 7/2003 by CJC:  bugfix -- clean up critical sections
    !!      associated with INIT3()
    !!
    !!      Modified 9/2013 by CJC:  Fortran-90 stuff; use NAME2FID()
    !!
    !!      Modified 12/2014 by CJC: logic cleanup
    !!
    !!      Modified 02/2015 by CJC for I/O API 3.2: Support for M3INT8 atts
    !!
    !!      Modified 08/2015 by CJC for I/O API 3.2:  support for MPI/PnetCDF;
    !!      F90 free source format; USE MODNCFIO
    !!
    !!      Version  11/2015 by CJC: replace MPI_OFFSET_KIND by hard-coded INTEGER(8)
    !!      because OpenMPI-1.4.x does not follow the MPOI "standard" competently.
    !!
    !!      Version  04/2018 by CJC: bug-fix at line 198, 250
    !!***********************************************************************

    USE MODNCFIO

    IMPLICIT NONE
    LOGICAL            RDATTC

    !!...........   INCLUDES:

    INCLUDE 'PARMS3.EXT'      ! I/O API constants
    INCLUDE 'STATE3.EXT'      ! I/O API internal state


    !!...........   ARGUMENTS and their descriptions:

    CHARACTER*(*), INTENT(IN   ) :: FNAME         !  logical file name
    CHARACTER*(*), INTENT(IN   ) :: VNAME         !  variable name, or ALLVARS3
    CHARACTER*(*), INTENT(IN   ) :: ANAME         !  attribute name
    INTEGER      , INTENT(IN   ) :: ATYPE         !  attribute type (M3REAL, M3INT, M3DBLE)
    INTEGER      , INTENT(IN   ) :: AMAX          !  attribute dimensionality
    INTEGER      , INTENT(  OUT) :: ASIZE         !  attribute actual size
    REAL         , INTENT(  OUT) :: AVAL( AMAX )  !  attribute value (numeric)
    CHARACTER*(*), INTENT(  OUT) :: CVAL          !  attribute value (character-string)


    !!...........   EXTERNAL FUNCTIONS and their descriptions:

    INTEGER, EXTERNAL :: INDEX1     !  look up names in name tables
    INTEGER, EXTERNAL :: NAME2FID   !  fname~~> fid lookup


    !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         F, V            !  subscripts for STATE3 arrays
    INTEGER         FID, VID        !  netCDF ID's
    INTEGER         VLEN            !  name length for vble
    INTEGER         IERR            !  netCDF error status return
    INTEGER         ATYP, ITYP, ALEN
    LOGICAL         EFLAG
    CHARACTER*16    PNAME
    CHARACTER*16    FIL16           !  scratch file-name buffer
    CHARACTER*16    VAR16           !  scratch vble-name buffer
    CHARACTER*16    ATT16           !  scratch  att-name buffer
    CHARACTER*256   MESG            !  message-buffer

#ifdef IOAPI_PNCF
    INCLUDE  'mpif.h'
    INTEGER( 8 ) :: SIZE
!!  INTEGER( MPI_OFFSET_KIND ) :: SIZE
#endif


    !!***********************************************************************
    !!   begin body of subroutine  RDATT3

    !!...........   Check attribute type

    IF ( ATYPE .NE. NF_INT     .AND.        &
         ATYPE .NE. NF_FLOAT   .AND.        &
         ATYPE .NE. NF_INT64   .AND.        &
         ATYPE .NE. NF_DOUBLE  ) THEN

        WRITE( MESG , '( 3 A, I10 )' ) 'RDATT3:  Attribute "'  , ANAME, '" has unsupported type', ATYPE
        CALL M3WARN( 'RDATT3', 0, 0, MESG )
        RDATT3 = .FALSE.
        RETURN

    END IF

    ITYP  = ATYPE
    PNAME = 'RDATT3'
    GO TO 111

    !!..................................................................

    ENTRY RDATTC( FNAME, VNAME, ANAME, CVAL )
    ITYP   = NF_CHAR
    PNAME = 'RDATT3C'

    !!........   fall through to  111  .................................

111 CONTINUE

    !!.........   Preliminary checks:

    EFLAG = .FALSE.
    F     = NAME2FID( FNAME )
    VLEN  = LEN_TRIM( VNAME )

    IF ( F .LE. 0 ) THEN
        EFLAG = .TRUE.
    ELSE IF ( CDFID3( F ) .LT. 0 ) THEN
        EFLAG = .TRUE.
        MESG  = 'File:  "' // FIL16 // '" is NOT A NetCDF file.'
        CALL M3MSG2( MESG )
    ELSE IF ( VLEN .GT. NAMLEN3 ) THEN
        EFLAG = .TRUE.
        WRITE( MESG, '( A, I10 )'  ) 'Max vble name length 16; actual:', VLEN
        CALL M3MSG2( MESG )
    END IF          !  if len( vname ) > 16

    IF ( EFLAG ) THEN
        MESG = 'Invalid variable or file name arguments'
        CALL M3WARN( PNAME, 0, 0, MESG )
        RDATT3 = .FALSE.
        RETURN
    END IF

    VAR16 = VNAME   !  fixed-length-16 scratch copy of name
    FIL16 = FNAME   !  fixed-length-16 scratch copy of name
    ATT16 = ANAME   !  fixed-length-16 scratch copy of name
    FID   = CDFID3( F )

    !!...........   Get ID for variable(s) to be read.

    IF ( VAR16 .EQ. ALLVAR3 ) THEN

        VID = NF_GLOBAL

    ELSE

        V = INDEX1( VAR16, NVARS3( F ) , VLIST3( 1,F ) )
        IF ( V .EQ. 0 ) THEN
            EFLAG = .TRUE.
            MESG = 'Variable "'      // VAR16 // '" not in file "' // FIL16 // '"'
            CALL M3MSG2( MESG )
        ELSE
            VID = VINDX3( V, F )
        END IF

    END IF          !  if VAR16 is 'ALL', or not.

    IF ( EFLAG ) THEN
        MESG = 'Invalid variable or file name arguments'
        CALL M3MESG( MESG )
        RDATT3 = .FALSE.
        RETURN
    END IF


    !!...........   Check attribute:  supported type; actual type and size;
    !!...........   value.
    !!...........   Somewhat tortured logic-structure due to the fact that
    !!...........   one can't execute a RETURN within a critical section.

!$OMP   CRITICAL( S_NC )

    IF ( FTYPE3( F ) .EQ. MPIGRD3 ) THEN
#ifdef IOAPI_PNCF
        IERR = NFMPI_INQ_ATT( FID, VID, ANAME, ATYP, SIZE )
        ALEN = SIZE
#endif
#ifndef IOAPI_PNCF
        CALL M3WARN( 'RDATT3', 0,0, 'PnetCDF/MPI not supported in this build.' )
        EFLAG = .TRUE.
        GO TO 999
#endif
    ELSE
        IERR = NF_INQ_ATT( FID, VID, ANAME, ATYP, ALEN )
    END IF

    IF ( IERR .NE. NF_NOERR ) THEN

        MESG = 'Error inquiring type&size for attribute "' // ANAME //  &
               '" for file "' // FNAME //'" and vble "' // VNAME // '"'
        CALL M3WARN( PNAME, 0, 0, MESG )
        EFLAG = .TRUE.

    ELSE IF ( ITYP .NE. ATYP ) THEN

        MESG = 'Bad type for attribute "' // ANAME //                   &
               '" for file "' // FNAME //'" and vble "' // VNAME // '"'
        CALL M3WARN( PNAME, 0, 0, MESG )
        EFLAG = .TRUE.

    ELSE IF ( ITYP .EQ. NF_CHAR ) THEN

        IF ( ALEN .GT. LEN( CVAL ) ) THEN

            MESG = 'Bad size for CHAR attribute "' // ANAME //          &
                   '" for file "' // FNAME //'" and vble "' // VNAME // '"'
            CALL M3WARN( PNAME, 0, 0, MESG )
            EFLAG = .TRUE.

        ELSE

            IERR = NF_GET_ATT_TEXT( FID, VID, ANAME, CVAL )

        END IF

    ELSE    !  if ( ityp  .ne. nf_char ) then...

        IF ( ALEN .GT. AMAX ) THEN

            MESG = 'Bad size for nonCHAR attribute "' // ANAME //       &
                   '" for file "' // FNAME //'" and vble "' // VNAME // '"'
            CALL M3WARN( PNAME, 0, 0, MESG )
            EFLAG = .TRUE.

        ELSE IF ( FTYPE3(F) .EQ. MPIGRD3 ) THEN

#ifdef IOAPI_PNCF
            IF ( ITYP .EQ. NF_INT    ) THEN
                IERR = NFMPI_GET_ATT_INT(   FID, VID, ANAME, AVAL )
            ELSE IF ( ITYP .EQ. NF_INT64  ) THEN
                IERR = NFMPI_GET_ATT_INT8(  FID, VID, ANAME, AVAL )
            ELSE IF ( ITYP .EQ. NF_FLOAT  ) THEN
                IERR = NFMPI_GET_ATT_REAL(  FID, VID, ANAME, AVAL )
            ELSE IF ( ITYP .EQ. NF_DOUBLE ) THEN
                IERR = NFMPI_GET_ATT_DOUBLE(FID, VID, ANAME, AVAL )
            END IF
#endif
#ifndef IOAPI_PNCF
            CALL M3MESG( 'PnetCDF not supported in this build' )
            IERR = IMISS3
#endif

        ELSE

            IF ( ITYP .EQ. NF_INT    ) THEN
                IERR = NF_GET_ATT_INT(   FID, VID, ANAME, AVAL )
            ELSE IF ( ITYP .EQ. NF_FLOAT  ) THEN
                IERR = NF_GET_ATT_REAL(  FID, VID, ANAME, AVAL )
            ELSE IF ( ITYP .EQ. NF_DOUBLE ) THEN
                IERR = NF_GET_ATT_DOUBLE(FID, VID, ANAME, AVAL )
            ELSE
                CALL M3MESG( 'NetCDF INTEGER*8 attributes not supported' )
                IERR = IMISS3
            END IF

        END IF

        ASIZE = ALEN

    END IF  !  if ierr; else if type-mismatch; else char; else...

999 CONTINUE

!$OMP   END CRITICAL( S_NC )

    IF ( IERR .NE. NF_NOERR ) THEN
        EFLAG = .TRUE.
    END IF

    IF ( EFLAG ) THEN
        MESG = 'Error reading attribute "' // ANAME //      &
               '" for file "' // FNAME // '" and vble "' // VNAME // '"'
        CALL M3WARN( PNAME, 0, 0, MESG )
    END IF          !  ierr nonzero:  operation failed
    IF ( ITYP .EQ. NF_CHAR ) THEN
        RDATTC = ( .NOT. EFLAG )
    ELSE
        RDATT3 = ( .NOT. EFLAG )
    END IF

    RETURN

END FUNCTION RDATT3

