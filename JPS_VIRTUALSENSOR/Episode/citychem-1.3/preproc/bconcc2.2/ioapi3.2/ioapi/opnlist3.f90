
LOGICAL FUNCTION OPNLIST3( FID, PGNAME ) RESULT( OFLAG )

    !!***********************************************************************
    !! Version "$Id: opnlist3.f90 118 2019-06-15 20:50:32Z coats $"
    !! EDSS/Models-3 I/O API.
    !! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    !! (C) 2003-2013 Baron Advanced Meteorological Systems,
    !! (C) 2007-2013 Carlie J. Coats, Jr., and
    !! (C) 2014 UNC Institute for the Environment.
    !! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    !! See file "LGPL.txt" for conditions of use.
    !!.........................................................................
    !!  function body starts at line  83
    !!
    !!  FUNCTION:
    !!       Open a FILE-LIST multi-file data set for status FSREAD3
    !!       with logical name FLIST3( FID ).
    !!
    !!  RETURN VALUE:
    !!       TRUE iff it succeeds in opening the file, reading its
    !!        attributes, and storing the relevant ones in STATE3.EXT
    !!
    !!  PRECONDITIONS REQUIRED:
    !!       All files in the logical-name list already exist.
    !!
    !!  REVISION  HISTORY:
    !!       Prototype  03/2002 by CJC for I/O API V2.2
    !!       Modified   02/2015 by CJC for I/O API 3.2: USE M3UTILIO
    !!
    !!      Modified 10/2015 by CJC for I/O API 3.2: use NF_*() instead of NC*()
    !!      for netCDF-Fortran 4.x compatibility
    !!***********************************************************************

    USE M3UTILIO
    USE MODNCFIO

    IMPLICIT NONE

    !!...........   INCLUDES:

    INCLUDE 'STATE3.EXT'


    !!...........   ARGUMENTS and their descriptions:

    INTEGER      , INTENT(IN   ) :: FID     !  index into STATE3 tables
    CHARACTER*(*), INTENT(IN   ) :: PGNAME  !  name of calling program


    !!...........   EXTERNAL FUNCTIONS and their descriptions:

    LOGICAL, EXTERNAL :: OPNFIL3  !  does work of opening "old" files


    !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

    CHARACTER*512   EQNAME  !  environment value of FLIST3( FID )
    CHARACTER*16    FIL16   !  scratch file-name buffer

    CHARACTER*256   MESG    !  for m3msg2, m3warn
    INTEGER         IDUM, IERR
    INTEGER         F, I, J, L, V

    !!  Support for FILE-LIST multi-file input data sets:
    !!  LISTFLAG = true iff FLIST3( FID ) is a FILE-LIST
    !!  LISTCNT  = # of elements in the list
    !!  LISTNAME( 1:LISTCNT ) = list of the logical names

    LOGICAL         AFLAG, EFLAG
    INTEGER         LISTCNT
    CHARACTER*16    LISTNAME( MXFILE3 )


    !!...........   STATEMENT FUNCTION:  REAL, REAL*8 "definitely unequal"

    LOGICAL         DBLERR
    REAL*8          P, Q

    DBLERR( P, Q ) = ( (P - Q)**2  .GT.  1.0E-10*( P*P + Q*Q + 1.0E-5 ) )

    !!.............................................................................
    !!   begin body of subroutine  OPNFIL3

    IF ( .NOT. STRLIST( FLIST3( FID ), 'Logical name list',  MXFILE3, LISTCNT, LISTNAME ) ) THEN

         MESG = FIL16 // ':'//TRIM( EQNAME )
        CALL M3MSG2( MESG )
        CALL M3WARN( 'OPEN3', 0, 0, 'Bad FILE_LIST.' )
        FLIST3( FID ) = CMISS3
        OFLAG         = .FALSE.
        RETURN

    END IF

    EFLAG = .FALSE.

    L     = ILCNT3
    IFRST3( FID ) = L + 1
    NLIST3( FID ) = LISTCNT
    CDFID3( FID ) = LSTFIL3
    SDATE3( FID ) = 99999999
    STIME3( FID ) = 0

    DO  F = 1, LISTCNT

        I = INDEX1( CMISS3, MXFILE3, FLIST3 )
        FIL16 = LISTNAME( F )
        CALL NAMEVAL( FIL16, EQNAME )
        INQUIRE ( FILE = EQNAME, EXIST = AFLAG )

        IF ( I .LE. 0 ) THEN

            MESG = 'Could not open ' // FIL16 // ' Max # of files already opened.'
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.

        ELSE IF ( AFLAG ) THEN

            IDUM = MAX ( INDEX( EQNAME, '-v' ) , INDEX( EQNAME, '-V' ) )
            IF ( IDUM .GT. 0 ) THEN
                EQNAME( IDUM:IDUM+1 ) = '  ' !  fix the '-v' (etc.)
                VOLAT3( I ) = .TRUE.         !  volatile file
            ELSE
                VOLAT3( I ) = .FALSE.
            END IF

            FLIST3( I ) = FIL16

            IF ( OPNFIL3( EQNAME, I, FSREAD3, PGNAME ) ) THEN

                L = L + 1
                ILIST3( L ) = I

                IF ( F .EQ. 1 ) THEN

                    SDATE3( FID ) = SDATE3( I )
                    STIME3( FID ) = STIME3( I )
                    TSTEP3( FID ) = TSTEP3( I )
                    FTYPE3( FID ) = FTYPE3( I )
                    GDTYP3( FID ) = GDTYP3( I )
                    NCOLS3( FID ) = NCOLS3( I )
                    NROWS3( FID ) = NROWS3( I )
                    NLAYS3( FID ) = NLAYS3( I )
                    NVARS3( FID ) = NVARS3( I )
                    NTHIK3( FID ) = NTHIK3( I )
                    P_ALP3( FID ) = P_ALP3( I )
                    P_BET3( FID ) = P_BET3( I )
                    P_GAM3( FID ) = P_GAM3( I )
                    XCENT3( FID ) = XCENT3( I )
                    YCENT3( FID ) = YCENT3( I )
                    XORIG3( FID ) = XORIG3( I )
                    YORIG3( FID ) = YORIG3( I )
                    XCELL3( FID ) = XCELL3( I )
                    YCELL3( FID ) = YCELL3( I )

                    DO  V = 1, NVARS3( FID )
                        VLIST3( V,FID ) = VLIST3( V,I )
                        VTYPE3( V,FID ) = VTYPE3( V,I )
                    END DO

                ELSE IF ( .NOT. EFLAG ) THEN

                    IF ( TSTEP3( I ) .NE. TSTEP3( FID ) ) THEN
                        EFLAG = .TRUE.
                        MESG = 'Bad TSTEP for ' //FIL16// ' in file list ' // FLIST3( FID )
                        CALL M3WARN( 'OPEN3', 0, 0, MESG )
                    END IF

                    IF ( FTYPE3( I ) .NE. FTYPE3( FID ) ) THEN
                        EFLAG = .TRUE.
                        MESG = 'Bad FTYPE for ' //FIL16// ' in file list ' // FLIST3( FID )
                        CALL M3WARN( 'OPEN3', 0, 0, MESG )
                    END IF

                    IF ( GDTYP3( I ) .NE. GDTYP3( FID ) ) THEN
                        EFLAG = .TRUE.
                        MESG = 'Bad GDTYP for ' //FIL16// ' in file list ' // FLIST3( FID )
                        CALL M3WARN( 'OPEN3', 0, 0, MESG )
                    END IF

                    IF ( NCOLS3( I ) .NE. NCOLS3( FID ) ) THEN
                        EFLAG = .TRUE.
                        MESG = 'Bad NCOLS for ' //FIL16// ' in file list ' // FLIST3( FID )
                        CALL M3WARN( 'OPEN3', 0, 0, MESG )
                    END IF

                    IF ( NROWS3( I ) .NE. NROWS3( FID ) ) THEN
                        EFLAG = .TRUE.
                        MESG = 'Bad NROWS for ' //FIL16// ' in file list ' // FLIST3( FID )
                        CALL M3WARN( 'OPEN3', 0, 0, MESG )
                    END IF

                    IF ( NLAYS3( I ) .NE. NLAYS3( FID ) ) THEN
                        EFLAG = .TRUE.
                        MESG = 'Bad NLAYS for ' //FIL16// ' in file list ' // FLIST3( FID )
                        CALL M3WARN( 'OPEN3', 0, 0, MESG )
                    END IF

                    IF ( NTHIK3( I ) .NE. NTHIK3( FID ) ) THEN
                        EFLAG = .TRUE.
                        MESG = 'Bad NTHIK for ' //FIL16// ' in file list ' // FLIST3( FID )
                        CALL M3WARN( 'OPEN3', 0, 0, MESG )
                    END IF

                    IF ( NVARS3( I ) .NE. NVARS3( FID ) ) THEN
                        EFLAG = .TRUE.
                        MESG = 'Bad NVARS for ' //FIL16// ' in file list ' // FLIST3( FID )
                        CALL M3WARN( 'OPEN3', 0, 0, MESG )
                    END IF

                    IF ( DBLERR( P_ALP3( I ), P_ALP3(FID) ) ) THEN
                        EFLAG = .TRUE.
                        MESG = 'Bad P_ALP for ' //FIL16// ' in file list ' // FLIST3( FID )
                        CALL M3WARN( 'OPEN3', 0, 0, MESG )
                    END IF

                    IF ( DBLERR( P_BET3( I ), P_BET3(FID) ) ) THEN
                        EFLAG = .TRUE.
                        MESG = 'Bad P_BET for ' //FIL16// ' in file list ' // FLIST3( FID )
                        CALL M3WARN( 'OPEN3', 0, 0, MESG )
                    END IF

                    IF ( DBLERR( P_GAM3( I ), P_GAM3(FID) ) ) THEN
                        EFLAG = .TRUE.
                        MESG = 'Bad P_GAM for ' //FIL16// ' in file list ' // FLIST3( FID )
                        CALL M3WARN( 'OPEN3', 0, 0, MESG )
                    END IF

                    IF ( DBLERR( XORIG3( I ), XORIG3(FID) ) ) THEN
                        EFLAG = .TRUE.
                        MESG = 'Bad XORIG for ' //FIL16// ' in file list ' // FLIST3( FID )
                        CALL M3WARN( 'OPEN3', 0, 0, MESG )
                    END IF

                    IF ( DBLERR( YORIG3( I ), YORIG3(FID) ) ) THEN
                        EFLAG = .TRUE.
                        MESG = 'Bad YORIG for ' //FIL16// ' in file list ' // FLIST3( FID )
                        CALL M3WARN( 'OPEN3', 0, 0, MESG )
                    END IF

                    IF ( DBLERR( XCENT3( I ), XCENT3(FID) ) ) THEN
                        EFLAG = .TRUE.
                        MESG = 'Bad XCENT for ' //FIL16// ' in file list ' // FLIST3( FID )
                        CALL M3WARN( 'OPEN3', 0, 0, MESG )
                    END IF

                    IF ( DBLERR( YCENT3( I ), YCENT3(FID) ) ) THEN
                        EFLAG = .TRUE.
                        MESG = 'Bad YCENT for ' //FIL16// ' in file list ' // FLIST3( FID )
                        CALL M3WARN( 'OPEN3', 0, 0, MESG )
                    END IF

                    IF ( DBLERR( XCELL3( I ), XCELL3(FID) ) ) THEN
                        EFLAG = .TRUE.
                        MESG = 'Bad XCELL for ' //FIL16// ' in file list ' // FLIST3( FID )
                        CALL M3WARN( 'OPEN3', 0, 0, MESG )
                    END IF

                    IF ( DBLERR( YCELL3( I ), YCELL3(FID) ) ) THEN
                        EFLAG = .TRUE.
                        MESG = 'Bad YCELL for ' //FIL16// ' in file list ' // FLIST3( FID )
                        CALL M3WARN( 'OPEN3', 0, 0, MESG )
                    END IF

                    DO  V = 1, NVARS3( FID )

                        IF ( VLIST3(V,FID) .NE. VLIST3(V,I) ) THEN
                            EFLAG = .TRUE.
                            MESG = 'Bad VNAME for ' //FIL16// ' in file list ' // FLIST3( FID )
                            CALL M3WARN( 'OPEN3', 0, 0, MESG )
                        END IF

                        IF ( VTYPE3(V,FID) .NE. VTYPE3(V,I) ) THEN
                            EFLAG = .TRUE.
                            MESG = 'Bad VNAME for ' //FIL16// ' in file list ' // FLIST3( FID )
                            CALL M3WARN( 'OPEN3', 0, 0, MESG )
                        END IF

                    END DO

                    IF ( EFLAG ) THEN
                        FLIST3( I ) = CMISS3
                    ELSE IF ( SECSDIFF( SDATE3( FID ),STIME3( FID ),     &
                                        SDATE3(   I ),STIME3(   I ) ) .LT. 0 ) THEN
                        SDATE3( FID ) = SDATE3( I )
                        STIME3( FID ) = STIME3( I )
                    END IF

                END IF              !  if F = 1 or not

            ELSE

                EFLAG = .TRUE.
                FLIST3( I ) = CMISS3

            END IF          !  if opnfil3() or not

        ELSE                !  i > 0 but not aflag:

            EFLAG = .TRUE.
            MESG  = FIL16//':'//TRIM(EQNAME)
            CALL M3MSG2( MESG )
            CALL M3WARN( 'OPNLIST3', 0, 0, 'File not available.' )
            FLIST3( I ) = CMISS3

        END IF      ! if i le 0; else if AFLAG; or not

    END DO                  !  end loop on names in this list

    IF ( EFLAG ) THEN       !  process error...

        L = ILCNT3
        DO  F = ILCNT3 + 1, L+1
            I = CDFID3( F )
            IF ( I .GT. 0 ) THEN
                IERR = NF_CLOSE( I )
                IF ( IERR .NE. 0 ) THEN
                    CALL M3ABORT( FLIST3( F ), I, IERR, 'Error NF_CLOSE-ing file' )
                END IF
            END IF
            FLIST3( I ) = CMISS3
            CDFID3( I ) = IMISS3
        END DO

        FLIST3( FID ) = CMISS3
        CDFID3( FID ) = IMISS3
        OFLAG         = .FALSE.
        RETURN

    ELSE                    !  process success

        DO  I = IFRST3(FID), IFRST3(FID) + NLIST3(FID) - 1
            F = ILIST3( I )
            J = JSTEP3( SDATE3(   F ), STIME3(   F ),       &
                        SDATE3( FID ), STIME3( FID ), TSTEP3( FID ) )
            BEGRC3( F ) = J
            ENDRC3( F ) = J + MXREC3( F ) - 1
            COUNT3      = MAX( COUNT3, ILIST3( F ) )
            MXREC3( FID ) = MAX( MXREC3( FID ), ENDRC3( F ) )
        END DO
        NLIST3( FID ) = LISTCNT
        IFRST3( FID ) = ILCNT3 + 1
        ILCNT3        = ILCNT3 + LISTCNT
        OFLAG         = .TRUE.

    END IF                  !  if error, or not

    RETURN

END FUNCTION OPNLIST3
