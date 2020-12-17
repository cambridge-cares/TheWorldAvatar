
      LOGICAL FUNCTION FILCHK3( FNAME, 
     &                          FTYPE, NCOLS, NROWS, NLAYS, NTHIK )

C***********************************************************************
C Version "$Id: filchk3.f 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems,
C (C) 2007-2013 Carlie J. Coats, Jr., and 
C (C) 2014 UNC Institute for the Environment.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  77
C
C  FUNCTION:
C       Checks to see whether file FNAME has the indicated type FTYPE
C       and appropriate dimensions NCOLS, NROWS, NLAYS, NTHIK (with
C       checking of just those that are appropriate for each FTYPE).
C       Layer-checking may be suppressed by setting NLAYS=ALLAYS3
C
C  RETURN VALUE:
C       TRUE iff the file has the user-supplied indicated file type
C       and grid/array dimensions
C
C  PRECONDITIONS REQUIRED:
C       FNAME is the logical name of an I/O API file already
C       opened by OPEN3()
C
C  REVISION  HISTORY: 
C       prototype 10/2000 by CJC
C
C       Modified 12/2003 by CJC for I/O API version 3:  uses new
C       INTEGER NAME2FID for file-lookup
C
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'STATE3.EXT'


C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*)   FNAME           !  logical file name
        INTEGER         FTYPE           !  user's queried file type
        INTEGER         NCOLS           !  user's queried col-dimension
        INTEGER         NROWS           !  user's queried row-dimension
        INTEGER         NLAYS           !  user's queried lay-dimension
        INTEGER         NTHIK           !  user's queried bdy-dimension


C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER, EXTERNAL :: INDEX1     !  look up names in name tables
        INTEGER, EXTERNAL :: NAME2FID   !  fname~~> fid lookup

        EXTERNAL :: INITBLK3            !  block data: initialize I/O state


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         FID             !  subscript  for STATE3 arrays
        LOGICAL         EFLAG
        CHARACTER*256   MESG


C***********************************************************************
C   begin body of function  FILCHK3
C.......   Find STATE3 index for the file:
 
        EFLAG = .FALSE.


C.......   Find STATE3 index for the file:

        FID   = NAME2FID( FNAME )
        IF ( FID .EQ. 0 ) THEN  !  file not available

            MESG = 'File  "'// FNAME // '" not yet opened.'
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
       
C.......   else perform checking according to file type:

        ELSE IF ( FTYPE .NE. FTYPE3( FID ) ) THEN  !  file has wrong type

            EFLAG = .TRUE.
            WRITE( MESG, '( A, I10 )' )  
     &          'File type from caller:', FTYPE
            CALL M3MSG2( MESG )
            WRITE( MESG, '( A, I10 )' )  
     &          'File type from file:  ', FTYPE3( FID )
            CALL M3MSG2( MESG )

        ELSE IF ( FTYPE .EQ. CUSTOM3 ) THEN

            IF ( NCOLS .NE. NCOLS3( FID ) ) THEN
                EFLAG = .TRUE.
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NCOLS from caller:', NCOLS
                CALL M3MSG2( MESG )
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NCOLS from file:  ', NCOLS3( FID )
                CALL M3MSG2( MESG )
            END IF
            IF ( NLAYS .NE. NLAYS3( FID ) .AND.
     &           NLAYS .NE. ALLAYS3 ) THEN
                EFLAG = .TRUE.
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NLAYS from caller:', NLAYS
                CALL M3MSG2( MESG )
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NLAYS from file:  ', NLAYS3( FID )
                CALL M3MSG2( MESG )
            END IF

        ELSE IF ( FTYPE .EQ. GRDDED3 ) THEN

            IF ( NCOLS .NE. NCOLS3( FID ) ) THEN
                EFLAG = .TRUE.
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NCOLS from caller:', NCOLS
                CALL M3MSG2( MESG )
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NCOLS from file:  ', NCOLS3( FID )
                CALL M3MSG2( MESG )
            END IF
            IF ( NROWS .NE. NROWS3( FID ) ) THEN
                EFLAG = .TRUE.
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NROWS from caller:', NROWS
                CALL M3MSG2( MESG )
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NROWS from file:  ', NROWS3( FID )
                CALL M3MSG2( MESG )
            END IF
            IF ( NLAYS .NE. NLAYS3( FID ) .AND.
     &           NLAYS .NE. ALLAYS3 ) THEN
                EFLAG = .TRUE.
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NLAYS from caller:', NLAYS
                CALL M3MSG2( MESG )
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NLAYS from file:  ', NLAYS3( FID )
                CALL M3MSG2( MESG )
            END IF

        ELSE IF ( FTYPE .EQ. BNDARY3 ) THEN

            IF ( NCOLS .NE. NCOLS3( FID ) ) THEN
                EFLAG = .TRUE.
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NCOLS from caller:', NCOLS
                CALL M3MSG2( MESG )
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NCOLS from file:  ', NCOLS3( FID )
                CALL M3MSG2( MESG )
            END IF
            IF ( NROWS .NE. NROWS3( FID ) ) THEN
                EFLAG = .TRUE.
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NROWS from caller:', NROWS
                CALL M3MSG2( MESG )
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NROWS from file:  ', NROWS3( FID )
                CALL M3MSG2( MESG )
            END IF
            IF ( NLAYS .NE. NLAYS3( FID ) .AND.
     &           NLAYS .NE. ALLAYS3 ) THEN
                EFLAG = .TRUE.
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NLAYS from caller:', NLAYS
                CALL M3MSG2( MESG )
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NLAYS from file:  ', NLAYS3( FID )
                CALL M3MSG2( MESG )
            END IF
            IF ( NTHIK .NE. NTHIK3( FID ) ) THEN
                EFLAG = .TRUE.
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NTHIK from caller:', NTHIK
                CALL M3MSG2( MESG )
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NTHIK from file:  ', NTHIK3( FID )
                CALL M3MSG2( MESG )
            END IF

        ELSE IF ( FTYPE .EQ. IDDATA3 ) THEN

            IF ( NROWS .NE. NROWS3( FID ) ) THEN
                EFLAG = .TRUE.
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NROWS from caller:', NROWS
                CALL M3MSG2( MESG )
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NROWS from file:  ', NROWS3( FID )
                CALL M3MSG2( MESG )
            END IF
            IF ( NLAYS .NE. NLAYS3( FID ) .AND.
     &           NLAYS .NE. ALLAYS3 ) THEN
                EFLAG = .TRUE.
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NLAYS from caller:', NLAYS
                CALL M3MSG2( MESG )
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NLAYS from file:  ', NLAYS3( FID )
                CALL M3MSG2( MESG )
            END IF

        ELSE IF ( FTYPE .EQ. PROFIL3 ) THEN

            IF ( NCOLS .NE. NCOLS3( FID ) ) THEN
                EFLAG = .TRUE.
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NCOLS from caller:', NCOLS
                CALL M3MSG2( MESG )
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NCOLS from file:  ', NCOLS3( FID )
                CALL M3MSG2( MESG )
            END IF
            IF ( NROWS .NE. NROWS3( FID ) ) THEN
                EFLAG = .TRUE.
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NROWS from caller:', NROWS
                CALL M3MSG2( MESG )
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NROWS from file:  ', NROWS3( FID )
                CALL M3MSG2( MESG )
            END IF
            IF ( NLAYS .NE. NLAYS3( FID ) .AND.
     &           NLAYS .NE. ALLAYS3 ) THEN
                EFLAG = .TRUE.
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NLAYS from caller:', NLAYS
                CALL M3MSG2( MESG )
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NLAYS from file:  ', NLAYS3( FID )
                CALL M3MSG2( MESG )
            END IF

        ELSE IF ( FTYPE .EQ. GRNEST3 ) THEN

            IF ( NCOLS .NE. NCOLS3( FID ) ) THEN
                EFLAG = .TRUE.
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NCOLS from caller:', NCOLS
                CALL M3MSG2( MESG )
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NCOLS from file:  ', NCOLS3( FID )
                CALL M3MSG2( MESG )
            END IF
            IF ( NROWS .NE. NROWS3( FID ) ) THEN
                EFLAG = .TRUE.
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NROWS from caller:', NROWS
                CALL M3MSG2( MESG )
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NROWS from file:  ', NROWS3( FID )
                CALL M3MSG2( MESG )
            END IF
            IF ( NLAYS .NE. NLAYS3( FID ) .AND.
     &           NLAYS .NE. ALLAYS3 ) THEN
                EFLAG = .TRUE.
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NLAYS from caller:', NLAYS
                CALL M3MSG2( MESG )
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NLAYS from file:  ', NLAYS3( FID )
                CALL M3MSG2( MESG )
            END IF

        ELSE IF ( FTYPE .EQ. PROFIL3 ) THEN

            IF ( NCOLS .NE. NCOLS3( FID ) ) THEN
                EFLAG = .TRUE.
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NCOLS from caller:', NCOLS
                CALL M3MSG2( MESG )
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NCOLS from file:  ', NCOLS3( FID )
                CALL M3MSG2( MESG )
            END IF
            IF ( NROWS .NE. NROWS3( FID ) ) THEN
                EFLAG = .TRUE.
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NROWS from caller:', NROWS
                CALL M3MSG2( MESG )
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NROWS from file:  ', NROWS3( FID )
                CALL M3MSG2( MESG )
            END IF
            IF ( NLAYS .NE. NLAYS3( FID ) .AND.
     &           NLAYS .NE. ALLAYS3 ) THEN
                EFLAG = .TRUE.
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NLAYS from caller:', NLAYS
                CALL M3MSG2( MESG )
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NLAYS from file:  ', NLAYS3( FID )
                CALL M3MSG2( MESG )
            END IF

        ELSE IF ( FTYPE .EQ. SMATRX3 ) THEN

            IF ( NCOLS .NE. NCOLS3( FID ) ) THEN
                EFLAG = .TRUE.
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NCOLS from caller:', NCOLS
                CALL M3MSG2( MESG )
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NCOLS from file:  ', NCOLS3( FID )
                CALL M3MSG2( MESG )
            END IF
            IF ( NROWS .NE. NROWS3( FID ) ) THEN
                EFLAG = .TRUE.
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NROWS from caller:', NROWS
                CALL M3MSG2( MESG )
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NROWS from file:  ', NROWS3( FID )
                CALL M3MSG2( MESG )
            END IF

        ELSE IF ( FTYPE .EQ. TSRIES3 ) THEN

            IF ( NCOLS .NE. NCOLS3( FID ) ) THEN
                EFLAG = .TRUE.
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NCOLS from caller:', NCOLS
                CALL M3MSG2( MESG )
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NCOLS from file:  ', NCOLS3( FID )
                CALL M3MSG2( MESG )
            END IF
            IF ( NROWS .NE. NROWS3( FID ) ) THEN
                EFLAG = .TRUE.
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NROWS from caller:', NROWS
                CALL M3MSG2( MESG )
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NROWS from file:  ', NROWS3( FID )
                CALL M3MSG2( MESG )
            END IF
            IF ( NLAYS .NE. NLAYS3( FID ) .AND.
     &           NLAYS .NE. ALLAYS3 ) THEN
                EFLAG = .TRUE.
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NLAYS from caller:', NLAYS
                CALL M3MSG2( MESG )
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NLAYS from file:  ', NLAYS3( FID )
                CALL M3MSG2( MESG )
            END IF

        ELSE IF ( FTYPE .EQ. PTRFLY3 ) THEN

            IF ( NCOLS .NE. NCOLS3( FID ) ) THEN
                EFLAG = .TRUE.
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NCOLS from caller:', NCOLS
                CALL M3MSG2( MESG )
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NCOLS from file:  ', NCOLS3( FID )
                CALL M3MSG2( MESG )
            END IF
            IF ( NROWS .NE. NROWS3( FID ) ) THEN
                EFLAG = .TRUE.
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NROWS from caller:', NROWS
                CALL M3MSG2( MESG )
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NROWS from file:  ', NROWS3( FID )
                CALL M3MSG2( MESG )
            END IF
            IF ( NLAYS .NE. NLAYS3( FID ) .AND.
     &           NLAYS .NE. ALLAYS3 ) THEN
                EFLAG = .TRUE.
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NLAYS from caller:', NLAYS
                CALL M3MSG2( MESG )
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NLAYS from file:  ', NLAYS3( FID )
                CALL M3MSG2( MESG )
            END IF

        ELSE IF ( FTYPE .EQ. KFEVNT3 ) THEN

            IF ( NCOLS .NE. NCOLS3( FID ) ) THEN
                EFLAG = .TRUE.
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NCOLS from caller:', NCOLS
                CALL M3MSG2( MESG )
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NCOLS from file:  ', NCOLS3( FID )
                CALL M3MSG2( MESG )
            END IF
            IF ( NROWS .NE. NROWS3( FID ) ) THEN
                EFLAG = .TRUE.
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NROWS from caller:', NROWS
                CALL M3MSG2( MESG )
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NROWS from file:  ', NROWS3( FID )
                CALL M3MSG2( MESG )
            END IF
            IF ( NLAYS .NE. NLAYS3( FID ) .AND.
     &           NLAYS .NE. ALLAYS3 ) THEN
                EFLAG = .TRUE.
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NLAYS from caller:', NLAYS
                CALL M3MSG2( MESG )
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NLAYS from file:  ', NLAYS3( FID )
                CALL M3MSG2( MESG )
            END IF
            IF ( NTHIK .LT. NTHIK3( FID ) ) THEN
                EFLAG = .TRUE.
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NTHIK from caller:', NTHIK
                CALL M3MSG2( MESG )
                WRITE( MESG, '( A, I10 )' )  
     &          'Dimension NTHIK from file:  ', NTHIK3( FID )
                CALL M3MSG2( MESG )
            END IF

        ELSE

            WRITE( MESG, '( 3A, I10 )' ) 
     &          'File "', FNAME, 
     &          '" has type invalid for FILCHK3:', FTYPE
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.

        END IF          !  if file type is ...

        FILCHK3 = ( .NOT. EFLAG )
        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Error and warning message formats..... 91xxx

91010   FORMAT ( 3 ( A , :, I5, :, 2X ) )

        END FUNCTION FILCHK3

